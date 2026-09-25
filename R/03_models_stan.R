# ==============================================================================
# Module: R/03_models_stan.R
# Purpose: Bayesian MCMC compilation, fitting, automated warm-start updates, and draws extraction
#          across the 4 pipelines (WLD, PD, XWLD, XPD)
# ==============================================================================

library(posterior)

#' Compile Stan model using cmdstanr or rstan fallback
#' @param stan_file Path to .stan file
#' @return Compiled model object
compile_stan_model <- function(stan_file) {
  if (!file.exists(stan_file)) {
    stop(sprintf("Stan file does not exist: %s", stan_file))
  }
  
  if (requireNamespace("cmdstanr", quietly = TRUE) && cmdstanr::cmdstan_version() != "") {
    message(sprintf("[Stan] Compiling %s via cmdstanr...", basename(stan_file)))
    mod <- cmdstanr::cmdstan_model(stan_file, compile = TRUE)
    return(mod)
  } else if (requireNamespace("rstan", quietly = TRUE)) {
    message(sprintf("[Stan] Compiling %s via rstan fallback...", basename(stan_file)))
    mod <- rstan::stan_model(file = stan_file)
    return(mod)
  } else {
    stop("Neither cmdstanr (with valid CmdStan installation) nor rstan is available.")
  }
}

#' Generic helper to fit a Stan model with CmdStan or RStan, supporting automated warm-start
fit_stan_engine <- function(stan_data,
                            stan_file,
                            engine_label,
                            chains = 4,
                            parallel_chains = min(chains, parallel::detectCores()),
                            iter_warmup = 1000,
                            iter_sampling = 1000,
                            adapt_delta = 0.95,
                            init = NULL,
                            warm_start = TRUE,
                            warm_start_warmup = 300,
                            inits_dir = "data/inits",
                            fast_prototype = FALSE) {
  mod <- compile_stan_model(stan_file)
  
  # Scope inits by season if season metadata is provided
  season_prefix <- if (!is.null(stan_data$season)) paste0(stan_data$season, "_") else ""
  init_file <- file.path(inits_dir, paste0(season_prefix, engine_label, "_inits.rds"))
  effective_init <- init
  effective_warmup <- iter_warmup
  
  if (is.null(effective_init) && warm_start && file.exists(init_file)) {
    cached_inits <- tryCatch(readRDS(init_file), error = function(e) NULL)
    if (!is.null(cached_inits) && is.list(cached_inits)) {
      # Verify vector parameter dimensions match N_teams if present
      raw_param <- grep("_raw$", names(cached_inits), value = TRUE)
      dim_match <- TRUE
      if (length(raw_param) > 0 && !is.null(stan_data$N_teams)) {
        if (length(cached_inits[[raw_param[1]]]) != stan_data$N_teams) {
          dim_match <- FALSE
        }
      }
      
      if (dim_match) {
        # Metric/step-size adaptation still needs a real warmup even from good inits; below
        # ~500 iterations the adapted metric is poor and ESS (especially lp__) collapses
        effective_warmup <- max(warm_start_warmup, 500L)
        message(sprintf("[%s] Prior warm-start inits loaded from %s. Reducing warmup to %d iterations (from %d).",
                        engine_label, init_file, effective_warmup, iter_warmup))
        # Jitter per chain: identical starting points for every chain make R-hat meaningless
        effective_init <- function(chain_id = 1) jitter_inits(cached_inits)
      } else {
        message(sprintf("[%s] Cached inits team dimension mismatch (teams changed). Proceeding with cold start.", engine_label))
      }
    }
  }
  
  # Strip non-Stan metadata from stan_data before passing to sampler
  declared_data <- stan_data[names(stan_data) != "season"]
  
  if (inherits(mod, "CmdStanModel")) {
    if (fast_prototype) {
      message(sprintf("[%s] Fast prototype mode enabled: running variational inference (meanfield)...", engine_label))
      fit <- mod$variational(
        data = declared_data,
        seed = 42,
        algorithm = "meanfield",
        iter = 5000
      )
      return(fit)
    }
    
    is_warm <- !is.null(effective_init)
    message(sprintf("[%s] Running NUTS MCMC (%d chains, %d warmup, %d sampling, warm_start = %s)...",
                    engine_label, chains, effective_warmup, iter_sampling, is_warm))
    
    fit <- mod$sample(
      data = declared_data,
      chains = chains,
      parallel_chains = parallel_chains,
      iter_warmup = effective_warmup,
      iter_sampling = iter_sampling,
      adapt_delta = adapt_delta,
      init = effective_init,
      seed = 42,
      show_messages = FALSE
    )
    
    # Save newly extracted posterior medians as warm-start inits for subsequent runs
    tryCatch({
      dir.create(inits_dir, recursive = TRUE, showWarnings = FALSE)
      new_inits_fn <- extract_posterior_inits(fit)
      saveRDS(new_inits_fn(), init_file)
      message(sprintf("[%s] Updated posterior medians saved to %s for subsequent warm-start.", engine_label, init_file))
    }, error = function(e) {
      warning(sprintf("[%s] Could not save warm-start inits: %s", engine_label, e$message))
    })
    
    return(fit)
  } else {
    # rstan fallback
    fit <- rstan::sampling(
      mod,
      data = declared_data,
      chains = chains,
      cores = parallel_chains,
      warmup = effective_warmup,
      iter = effective_warmup + iter_sampling,
      control = list(adapt_delta = adapt_delta),
      init = if (is.null(effective_init)) "random" else effective_init,
      seed = 42
    )
    
    tryCatch({
      dir.create(inits_dir, recursive = TRUE, showWarnings = FALSE)
      new_inits_fn <- extract_posterior_inits(fit)
      saveRDS(new_inits_fn(), init_file)
    }, error = function(e) NULL)
    
    return(fit)
  }
}

#' Fit Pipeline 1: WLD Ordered Logit Model (Actual Discrete)
#' @param stan_data List formatted for wld_ordered_logit.stan
#' @param stan_file Path to Stan file
#' @param chains Number of MCMC chains (default: 4)
#' @param parallel_chains Number of parallel chains (default: 4)
#' @param iter_warmup Warmup iterations (default: 1000, or 300 with warm_start)
#' @param iter_sampling Sampling iterations (default: 1000)
#' @param adapt_delta Target acceptance rate (default: 0.95)
#' @param init Initial values or function (optional manual init)
#' @param warm_start Logical: if TRUE, attempts to load cached posterior inits from previous runs
#' @param warm_start_warmup Warmup iterations when warm_start inits are loaded (default: 300)
#' @param fast_prototype If TRUE, uses variational inference
#' @return Fitted model object with posterior draws
fit_wld_model <- function(stan_data, 
                          stan_file = "stan/wld_ordered_logit.stan",
                          chains = 4, 
                          parallel_chains = min(chains, parallel::detectCores()),
                          iter_warmup = 1000, 
                          iter_sampling = 1000,
                          adapt_delta = 0.95,
                          init = NULL,
                          warm_start = TRUE,
                          warm_start_warmup = 300,
                          fast_prototype = FALSE) {
  fit_stan_engine(
    stan_data = stan_data,
    stan_file = stan_file,
    engine_label = "fit_wld",
    chains = chains,
    parallel_chains = parallel_chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    adapt_delta = adapt_delta,
    init = init,
    warm_start = warm_start,
    warm_start_warmup = warm_start_warmup,
    fast_prototype = fast_prototype
  )
}

#' Fit Pipeline 2: PD Robust Student-t Model (Actual Point Differential)
#' @param stan_data List formatted for pd_student_t.stan
#' @param stan_file Path to Stan file
#' @param chains Number of MCMC chains (default: 4)
#' @param parallel_chains Number of parallel chains (default: 4)
#' @param iter_warmup Warmup iterations (default: 1000, or 300 with warm_start)
#' @param iter_sampling Sampling iterations (default: 1000)
#' @param adapt_delta Target acceptance rate (default: 0.95)
#' @param init Initial values (optional manual init)
#' @param warm_start Logical: if TRUE, attempts to load cached posterior inits
#' @param warm_start_warmup Warmup iterations when warm_start inits are loaded (default: 300)
#' @param fast_prototype If TRUE, uses variational inference
#' @return Fitted model object with posterior draws
fit_pd_model <- function(stan_data, 
                         stan_file = "stan/pd_student_t.stan",
                         chains = 4, 
                         parallel_chains = min(chains, parallel::detectCores()),
                         iter_warmup = 1000, 
                         iter_sampling = 1000,
                         adapt_delta = 0.95,
                         init = NULL,
                         warm_start = TRUE,
                         warm_start_warmup = 300,
                         fast_prototype = FALSE) {
  fit_stan_engine(
    stan_data = stan_data,
    stan_file = stan_file,
    engine_label = "fit_pd",
    chains = chains,
    parallel_chains = parallel_chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    adapt_delta = adapt_delta,
    init = init,
    warm_start = warm_start,
    warm_start_warmup = warm_start_warmup,
    fast_prototype = fast_prototype
  )
}

#' Fit Pipeline 3: XWLD Ordered Logit Model (Expected Discrete Outcomes)
#' @param stan_data List formatted for xwld_ordered_logit.stan
#' @param stan_file Path to Stan file
#' @param chains Number of MCMC chains (default: 4)
#' @param parallel_chains Number of parallel chains (default: 4)
#' @param iter_warmup Warmup iterations (default: 1000, or 300 with warm_start)
#' @param iter_sampling Sampling iterations (default: 1000)
#' @param adapt_delta Target acceptance rate (default: 0.95)
#' @param init Initial values (optional manual init)
#' @param warm_start Logical: if TRUE, attempts to load cached posterior inits
#' @param warm_start_warmup Warmup iterations when warm_start inits are loaded (default: 300)
#' @param fast_prototype If TRUE, uses variational inference
#' @return Fitted model object with posterior draws
fit_xwld_model <- function(stan_data, 
                           stan_file = "stan/xwld_ordered_logit.stan",
                           chains = 4, 
                           parallel_chains = min(chains, parallel::detectCores()),
                           iter_warmup = 1000, 
                           iter_sampling = 1000,
                           adapt_delta = 0.95,
                           init = NULL,
                           warm_start = TRUE,
                           warm_start_warmup = 300,
                           fast_prototype = FALSE) {
  fit_stan_engine(
    stan_data = stan_data,
    stan_file = stan_file,
    engine_label = "fit_xwld",
    chains = chains,
    parallel_chains = parallel_chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    adapt_delta = adapt_delta,
    init = init,
    warm_start = warm_start,
    warm_start_warmup = warm_start_warmup,
    fast_prototype = fast_prototype
  )
}

#' Fit Pipeline 4: XPD Robust Student-t Model (Expected Continuous Net EPA)
#' @param stan_data List formatted for xpd_student_t.stan
#' @param stan_file Path to Stan file
#' @param chains Number of MCMC chains (default: 4)
#' @param parallel_chains Number of parallel chains (default: 4)
#' @param iter_warmup Warmup iterations (default: 1000, or 300 with warm_start)
#' @param iter_sampling Sampling iterations (default: 1000)
#' @param adapt_delta Target acceptance rate (default: 0.95)
#' @param init Initial values (optional manual init)
#' @param warm_start Logical: if TRUE, attempts to load cached posterior inits
#' @param warm_start_warmup Warmup iterations when warm_start inits are loaded (default: 300)
#' @param fast_prototype If TRUE, uses variational inference
#' @return Fitted model object with posterior draws
fit_xpd_model <- function(stan_data, 
                          stan_file = "stan/xpd_student_t.stan",
                          chains = 4, 
                          parallel_chains = min(chains, parallel::detectCores()),
                          iter_warmup = 1000, 
                          iter_sampling = 1000,
                          adapt_delta = 0.95,
                          init = NULL,
                          warm_start = TRUE,
                          warm_start_warmup = 300,
                          fast_prototype = FALSE) {
  fit_stan_engine(
    stan_data = stan_data,
    stan_file = stan_file,
    engine_label = "fit_xpd",
    chains = chains,
    parallel_chains = parallel_chains,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    adapt_delta = adapt_delta,
    init = init,
    warm_start = warm_start,
    warm_start_warmup = warm_start_warmup,
    fast_prototype = fast_prototype
  )
}

#' Perturb cached warm-start inits so each chain starts from a distinct, valid point
#' @param inits Named list from extract_posterior_inits()
#' @return Jittered init list respecting each parameter's Stan constraint
jitter_inits <- function(inits) {
  positive <- c("sigma_fbs", "sigma_fcs", "omega_fbs", "omega_fcs", "tau")
  out <- inits
  for (p in names(inits)) {
    x <- inits[[p]]
    n <- length(x)
    out[[p]] <- if (grepl("_raw$", p)) {
      x + stats::rnorm(n, 0, 0.3)
    } else if (p %in% positive) {
      x * exp(stats::rnorm(n, 0, 0.2))
    } else if (p == "nu") {
      2 + (x - 2) * exp(stats::rnorm(n, 0, 0.2))           # <lower=2>
    } else if (p == "c") {
      sort(x + stats::rnorm(n, 0, 0.1))                     # ordered[3]
    } else {
      x + stats::rnorm(n, 0, 0.1 * pmax(1, abs(x)))
    }
  }
  out
}

#' Extract parameter medians to serve as warm-start initial values for subsequent weeks
#' @param fit Fitted CmdStanMCMC or stanfit object
#' @param params Vector of parameter names to extract
#' @return Function returning list of initial values suitable for Stan init argument
extract_posterior_inits <- function(fit, params = c("theta_raw", "delta_raw", "omega_raw", "alpha_raw", 
                                                    "mu_fcs", "xi_fcs", "gamma_home", "beta_home",
                                                    "sigma_fbs", "sigma_fcs", "omega_fbs", "omega_fcs",
                                                    "c", "nu", "tau")) {
  draws <- posterior::as_draws_df(fit)
  inits <- list()
  
  for (p in params) {
    matching_cols <- grep(paste0("^", p, "(\\[.*|\\])?$"), names(draws), value = TRUE)
    if (length(matching_cols) == 1) {
      inits[[p]] <- median(draws[[matching_cols]], na.rm = TRUE)
    } else if (length(matching_cols) > 1) {
      inits[[p]] <- sapply(matching_cols, function(col) median(draws[[col]], na.rm = TRUE))
      names(inits[[p]]) <- NULL
    }
  }
  
  # For ordered cutpoints, strictly enforce ascending order c[1] < c[2] < c[3]
  if (!is.null(inits$c)) {
    inits$c <- sort(inits$c)
  }
  
  # Function returning init list for each chain
  init_fn <- function() inits
  return(init_fn)
}
