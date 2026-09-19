# ==============================================================================
# Module: R/03_models_stan.R
# Purpose: Bayesian MCMC compilation, fitting, warm-start initialization, and draws extraction
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

#' Fit Pipeline 1: WLD Ordered Logit Model
#' @param stan_data List formatted for wld_ordered_logit.stan
#' @param stan_file Path to Stan file
#' @param chains Number of MCMC chains (default: 4)
#' @param parallel_chains Number of parallel chains (default: 4)
#' @param iter_warmup Warmup iterations (default: 1000, or 300 with warm start)
#' @param iter_sampling Sampling iterations (default: 1000)
#' @param adapt_delta Target acceptance rate (default: 0.95)
#' @param init Initial values or function (optional warm-start)
#' @param fast_prototype If TRUE, uses variational inference or short chain for prototyping
#' @return Fitted model object with posterior draws
fit_wld_model <- function(stan_data, 
                          stan_file = "stan/wld_ordered_logit.stan",
                          chains = 4, 
                          parallel_chains = min(chains, parallel::detectCores()),
                          iter_warmup = 1000, 
                          iter_sampling = 1000,
                          adapt_delta = 0.95,
                          init = NULL,
                          fast_prototype = FALSE) {
  mod <- compile_stan_model(stan_file)
  
  if (inherits(mod, "CmdStanModel")) {
    if (fast_prototype) {
      message("[fit_wld] Fast prototype mode enabled: running variational inference (meanfield)...")
      fit <- mod$variational(
        data = stan_data,
        seed = 42,
        algorithm = "meanfield",
        iter = 5000
      )
      return(fit)
    }
    
    message(sprintf("[fit_wld] Running NUTS MCMC (%d chains, %d warmup, %d sampling)...",
                    chains, iter_warmup, iter_sampling))
    
    fit <- mod$sample(
      data = stan_data,
      chains = chains,
      parallel_chains = parallel_chains,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      adapt_delta = adapt_delta,
      init = init,
      seed = 42,
      show_messages = FALSE
    )
    return(fit)
  } else {
    # rstan fallback
    fit <- rstan::sampling(
      mod,
      data = stan_data,
      chains = chains,
      cores = parallel_chains,
      warmup = iter_warmup,
      iter = iter_warmup + iter_sampling,
      control = list(adapt_delta = adapt_delta),
      init = if (is.null(init)) "random" else init,
      seed = 42
    )
    return(fit)
  }
}

#' Fit Pipeline 2: XPD Robust Student-t Net EPA Model
#' @param stan_data List formatted for xpd_student_t.stan
#' @param stan_file Path to Stan file
#' @param chains Number of MCMC chains (default: 4)
#' @param parallel_chains Number of parallel chains (default: 4)
#' @param iter_warmup Warmup iterations (default: 1000)
#' @param iter_sampling Sampling iterations (default: 1000)
#' @param adapt_delta Target acceptance rate (default: 0.95)
#' @param init Initial values (optional warm start)
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
                          fast_prototype = FALSE) {
  mod <- compile_stan_model(stan_file)
  
  if (inherits(mod, "CmdStanModel")) {
    if (fast_prototype) {
      message("[fit_xpd] Fast prototype mode enabled: running variational inference (meanfield)...")
      fit <- mod$variational(
        data = stan_data,
        seed = 42,
        algorithm = "meanfield",
        iter = 5000
      )
      return(fit)
    }
    
    message(sprintf("[fit_xpd] Running NUTS MCMC (%d chains, %d warmup, %d sampling)...",
                    chains, iter_warmup, iter_sampling))
    
    fit <- mod$sample(
      data = stan_data,
      chains = chains,
      parallel_chains = parallel_chains,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      adapt_delta = adapt_delta,
      init = init,
      seed = 42,
      show_messages = FALSE
    )
    return(fit)
  } else {
    # rstan fallback
    fit <- rstan::sampling(
      mod,
      data = stan_data,
      chains = chains,
      cores = parallel_chains,
      warmup = iter_warmup,
      iter = iter_warmup + iter_sampling,
      control = list(adapt_delta = adapt_delta),
      init = if (is.null(init)) "random" else init,
      seed = 42
    )
    return(fit)
  }
}

#' Extract parameter medians to serve as warm-start initial values for subsequent weeks
#' @param fit Fitted CmdStanMCMC or stanfit object
#' @param params Vector of parameter names to extract
#' @return List of initial values suitable for Stan init argument
extract_posterior_inits <- function(fit, params = c("theta_raw", "alpha_raw", "mu_fcs", "xi_fcs", "gamma_home", "beta_home")) {
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
  
  # Function returning init list for each chain
  init_fn <- function() inits
  return(init_fn)
}
