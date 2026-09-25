// ==============================================================================
// Stan Model: Pipeline 2 - Expected Point Differential (XPD) Robust Student-t
// CFB Retrospective Bayesian Ranking Engine
// ==============================================================================

data {
  int<lower=1> N_games;                            // Number of games with PBP EPA
  int<lower=1> N_teams;                            // Total number of FBS + FCS teams
  array[N_games] int<lower=1, upper=N_teams> home_team; // Home team index
  array[N_games] int<lower=1, upper=N_teams> away_team; // Away team index
  array[N_games] int<lower=0, upper=1> neutral_site;    // 1 if neutral site, 0 if home venue
  vector[N_games] xpd;                             // Net EPA differential (home - away)
  array[N_teams] int<lower=1, upper=2> team_subdivision;// 1=FBS, 2=FCS

  // Hyperparameters for prior sensitivity analysis
  real prior_xi_fcs_mean;                          // Default: -21.0 points
  real<lower=0> prior_xi_fcs_sd;                   // Default: 3.0
  real<lower=0> prior_omega_rate;                  // Default: 0.1
  real prior_beta_home_mean;                       // Default: 2.5
  real<lower=0> prior_beta_home_sd;                // Default: 1.0
}

parameters {
  real xi_fcs;                                     // Latent tier differential for FCS in net EPA
  real<lower=0> omega_fbs;                         // Variance component for FBS teams
  real<lower=0> omega_fcs;                         // Variance component for FCS teams
  real beta_home;                                  // Expected point differential boost for home field
  real<lower=2> nu;                                // Student-t degrees of freedom (nu > 2 for finite variance)
  real<lower=0> tau;                               // Residual scale
  vector[N_teams] alpha_raw;                       // Standardized efficiency (non-centered)
}

transformed parameters {
  vector[N_teams] alpha;                           // Latent net efficiency rating
  vector[2] xi_sub;
  vector[2] omega_sub;

  xi_sub[1] = 0.0;                                 // FBS anchored at 0
  xi_sub[2] = xi_fcs;
  omega_sub[1] = omega_fbs;
  omega_sub[2] = omega_fcs;

  // Non-centered hierarchical parameterization
  for (t in 1:N_teams) {
    alpha[t] = xi_sub[team_subdivision[t]] + omega_sub[team_subdivision[t]] * alpha_raw[t];
  }
}

model {
  // Priors
  xi_fcs ~ normal(prior_xi_fcs_mean, prior_xi_fcs_sd);
  omega_fbs ~ exponential(prior_omega_rate);
  omega_fcs ~ exponential(prior_omega_rate);
  beta_home ~ normal(prior_beta_home_mean, prior_beta_home_sd);
  nu ~ gamma(2.0, 0.1);                            // Juarez-Steel default: mode 10, mean 20 (lets data choose tail weight)
  tau ~ exponential(0.1);
  alpha_raw ~ std_normal();

  // Robust Student-t Likelihood
  {
    vector[N_games] mu;
    for (k in 1:N_games) {
      mu[k] = alpha[home_team[k]] - alpha[away_team[k]] + beta_home * (1 - neutral_site[k]);
    }
    xpd ~ student_t(nu, mu, tau);
  }
}

generated quantities {
  vector[N_games] log_lik;                         // Pointwise log-likelihood
  vector[N_games] xpd_rep;                         // Posterior predictive replicates

  for (k in 1:N_games) {
    real mu_k = alpha[home_team[k]] - alpha[away_team[k]] + beta_home * (1 - neutral_site[k]);
    log_lik[k] = student_t_lpdf(xpd[k] | nu, mu_k, tau);
    xpd_rep[k] = student_t_rng(nu, mu_k, tau);
  }
}
