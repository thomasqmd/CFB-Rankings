# ==============================================================================
# Module: R/04_diagnostics.R
# Purpose: MCMC convergence diagnostics, diagnostic gating assertions, and prior sensitivity sweep
#          across all 4 pipelines (WLD, PD, XWLD, XPD)
# ==============================================================================

library(posterior)
library(dplyr)
library(tidyr)
library(ggplot2)

#' Evaluate MCMC convergence health and enforce diagnostic gates
#' @param fit Fitted CmdStanMCMC or stanfit object
#' @param model_name Character name of the model for logging
#' @param strict_halt Logical: if TRUE, stops execution if diagnostic gates are breached
#' @return List containing pass status, diagnostic metrics, and parameter summary
evaluate_mcmc_health <- function(fit, model_name = "Model", strict_halt = FALSE) {
  message(sprintf("[Diagnostics] Evaluating MCMC convergence for %s...", model_name))
  
  # 1. Divergent transitions and E-BFMI (CmdStanMCMC)
  num_divergences <- 0
  ebfmi_values <- numeric(0)
  
  if (inherits(fit, "CmdStanMCMC")) {
    diag_summary <- tryCatch(fit$diagnostic_summary(quiet = TRUE), error = function(e) NULL)
    if (!is.null(diag_summary)) {
      num_divergences <- sum(diag_summary$num_divergent, na.rm = TRUE)
      ebfmi_values <- diag_summary$ebfmi
    } else {
      sampler_diag <- tryCatch(fit$sampler_diagnostics(), error = function(e) NULL)
      if (!is.null(sampler_diag) && "divergent__" %in% dimnames(sampler_diag)[[3]]) {
        num_divergences <- sum(sampler_diag[, , "divergent__"], na.rm = TRUE)
      }
      ebfmi_values <- numeric(0)
    }
  } else if (inherits(fit, "stanfit")) {
    sampler_params <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
    num_divergences <- sum(sapply(sampler_params, function(x) sum(x[, "divergent__"])))
    # E-BFMI
    ebfmi_values <- sapply(sampler_params, function(x) {
      energy <- x[, "energy__"]
      numer <- sum(diff(energy)^2) / length(energy)
      denom <- var(energy)
      numer / denom
    })
  }
  
  # 2. Summarize posterior draws for R-hat and ESS
  draws <- tryCatch(fit$draws(), error = function(e) posterior::as_draws(fit))
  
  # Exclude log_lik and replicate draws to focus diagnostics on core structural parameters
  param_summary <- posterior::summarise_draws(
    draws,
    "mean", "median", "sd", "rhat", "ess_bulk", "ess_tail"
  ) |>
    dplyr::filter(!stringr::str_detect(variable, "^log_lik|^y_rep|^pd_rep|^xwld_rep|^xpd_rep"))
  
  max_rhat <- max(param_summary$rhat, na.rm = TRUE)
  min_ess_bulk <- min(param_summary$ess_bulk, na.rm = TRUE)
  min_ess_tail <- min(param_summary$ess_tail, na.rm = TRUE)
  min_ebfmi <- if (length(ebfmi_values) > 0) min(ebfmi_values, na.rm = TRUE) else 1.0
  
  # 3. Diagnostic assertions
  issues <- character(0)
  if (max_rhat > 1.01) {
    bad_params <- param_summary |> dplyr::filter(rhat > 1.01) |> dplyr::pull(variable)
    issues <- c(issues, sprintf("R-hat gate failed: max R-hat = %.4f (> 1.01) in %d parameter(s) [%s]",
                                max_rhat, length(bad_params), paste(head(bad_params, 3), collapse = ", ")))
  }
  if (min_ess_bulk < 400) {
    issues <- c(issues, sprintf("ESS bulk gate failed: min ESS = %.1f (< 400)", min_ess_bulk))
  }
  if (min_ess_tail < 400) {
    issues <- c(issues, sprintf("ESS tail gate failed: min ESS tail = %.1f (< 400)", min_ess_tail))
  }
  if (num_divergences > 0) {
    issues <- c(issues, sprintf("Divergences gate failed: detected %d divergent transitions post-warmup", num_divergences))
  }
  if (min_ebfmi < 0.2) {
    issues <- c(issues, sprintf("E-BFMI gate failed: min E-BFMI = %.3f (< 0.2)", min_ebfmi))
  }
  
  passed <- length(issues) == 0
  
  gate_report <- tibble::tibble(
    Metric = c("Max R-hat", "Min ESS Bulk", "Min ESS Tail", "Post-warmup Divergences", "Min E-BFMI"),
    Value = c(max_rhat, min_ess_bulk, min_ess_tail, num_divergences, min_ebfmi),
    Threshold = c("<= 1.01", ">= 400", ">= 400", "= 0", "> 0.20"),
    Status = c(
      ifelse(max_rhat <= 1.01, "PASS", "FAIL"),
      ifelse(min_ess_bulk >= 400, "PASS", "FAIL"),
      ifelse(min_ess_tail >= 400, "PASS", "FAIL"),
      ifelse(num_divergences == 0, "PASS", "FAIL"),
      ifelse(min_ebfmi > 0.2, "PASS", "FAIL")
    )
  )
  
  if (!passed) {
    msg <- paste0("[DIAGNOSTIC FAILURE] ", model_name, " breached MCMC gates:\n", paste0("  - ", issues, collapse = "\n"))
    if (strict_halt) stop(msg) else warning(msg)
  } else {
    message(sprintf("[Diagnostics] %s passed all convergence checks successfully.", model_name))
  }
  
  return(list(
    model_name = model_name,
    passed = passed,
    gate_report = gate_report,
    param_summary = param_summary,
    issues = issues
  ))
}

#' Run prior sensitivity sweep across FCS tier hyperparameters
#' @param stan_data Baseline Stan data list
#' @param stan_file Path to Stan file
#' @param team_lookup Master team lookup table
#' @param model_type One of "wld", "pd", "xwld", "xpd"
#' @return Tibble summarizing top-25 Jaccard similarity and Spearman rho across perturbations
run_prior_sensitivity_sweep <- function(stan_data, 
                                       stan_file = "stan/wld_ordered_logit.stan",
                                       team_lookup,
                                       model_type = c("wld", "pd", "xwld", "xpd")) {
  model_type <- match.arg(model_type)
  message(sprintf("[Prior Sensitivity] Running hyperparameter sweep for %s pipeline...", toupper(model_type)))
  
  # Grid: mu_FCS in {-1.0, -1.5, -2.0}, sigma_FCS in {0.3, 0.5, 0.8}
  mu_grid <- c(-1.0, -1.5, -2.0)
  sigma_grid <- c(0.3, 0.5, 0.8)
  grid <- expand.grid(mu_fcs = mu_grid, sigma_fcs = sigma_grid)
  
  # Baseline fit
  mod <- compile_stan_model(stan_file)
  
  # Identify target parameter based on model type
  param_name <- switch(
    model_type,
    "wld" = "theta",
    "pd" = "delta",
    "xwld" = "omega",
    "xpd" = "alpha"
  )
  
  # Extract baseline ratings (using variational / fast sampling for sweep efficiency)
  get_team_ratings <- function(data_mod) {
    clean_data <- data_mod[names(data_mod) != "season"]
    fit <- mod$variational(data = clean_data, seed = 123, iter = 4000, algorithm = "meanfield", show_messages = FALSE)
    draws <- posterior::as_draws_df(fit)
    param_cols <- grep(paste0("^", param_name, "\\["), names(draws), value = TRUE)
    medians <- sapply(param_cols, function(col) median(draws[[col]], na.rm = TRUE))
    return(medians)
  }
  
  baseline_ratings <- get_team_ratings(stan_data)
  fbs_indices <- which(team_lookup$subdivision_id == 1L)
  baseline_fbs <- baseline_ratings[fbs_indices]
  baseline_top25 <- names(sort(baseline_fbs, decreasing = TRUE)[1:25])
  
  sweep_results <- list()
  
  for (i in seq_len(nrow(grid))) {
    p_mu <- grid$mu_fcs[i]
    p_sigma <- grid$sigma_fcs[i]
    
    # Clone data list and override hyperparameters
    perturbed_data <- stan_data
    if (model_type %in% c("wld", "xwld")) {
      perturbed_data$prior_mu_fcs_mean <- p_mu
      perturbed_data$prior_mu_fcs_sd <- p_sigma
    } else if (model_type == "pd") {
      perturbed_data$prior_xi_fcs_mean <- p_mu * 16.0
      perturbed_data$prior_xi_fcs_sd <- p_sigma * 6.0
    } else if (model_type == "xpd") {
      perturbed_data$prior_xi_fcs_mean <- p_mu * 14.0
      perturbed_data$prior_xi_fcs_sd <- p_sigma * 6.0
    }
    
    pert_ratings <- get_team_ratings(perturbed_data)
    pert_fbs <- pert_ratings[fbs_indices]
    pert_top25 <- names(sort(pert_fbs, decreasing = TRUE)[1:25])
    
    # Compute Spearman rank correlation and top-25 Jaccard overlap
    spearman_rho <- cor(baseline_fbs, pert_fbs, method = "spearman")
    jaccard_top25 <- length(intersect(baseline_top25, pert_top25)) / length(union(baseline_top25, pert_top25))
    
    alert_flag <- spearman_rho < 0.985
    
    sweep_results[[i]] <- tibble::tibble(
      pipeline = toupper(model_type),
      mu_fcs = p_mu,
      sigma_fcs = p_sigma,
      spearman_rho = spearman_rho,
      jaccard_top25 = jaccard_top25,
      alert_flag = alert_flag
    )
  }
  
  results_df <- dplyr::bind_rows(sweep_results)
  message(sprintf("[Prior Sensitivity] %s sweep completed. Min Spearman rho = %.4f, Min Top-25 Jaccard = %.3f",
                  toupper(model_type), min(results_df$spearman_rho), min(results_df$jaccard_top25)))
  
  return(results_df)
}
