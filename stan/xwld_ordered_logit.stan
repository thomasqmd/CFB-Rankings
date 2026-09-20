// ==============================================================================
// Stan Model: Pipeline 3 - Expected Win/Loss/Overtime (XWLD) Ordered Logit
// CFB Retrospective Bayesian Ranking Engine
// ==============================================================================

data {
  int<lower=1> N_games;                            // Number of completed games
  int<lower=1> N_teams;                            // Total number of FBS + FCS teams
  array[N_games] int<lower=1, upper=N_teams> home_team; // Index of home team
  array[N_games] int<lower=1, upper=N_teams> away_team; // Index of away team
  array[N_games] int<lower=0, upper=1> neutral_site;    // 1 if neutral site, 0 if home venue
  array[N_games] int<lower=1, upper=4> outcome;         // 1=ExpLoss, 2=ExpOTLoss/TossupLoss, 3=ExpOTWin/TossupWin, 4=ExpWin
  array[N_teams] int<lower=1, upper=2> team_subdivision;// 1=FBS, 2=FCS

  // Hyperparameters for prior sensitivity analysis
  real prior_mu_fcs_mean;                          // Default: -1.5
  real<lower=0> prior_mu_fcs_sd;                   // Default: 0.5
  real<lower=0> prior_sigma_rate;                  // Default: 1.0
  real prior_gamma_mean;                           // Default: 0.2
  real<lower=0> prior_gamma_sd;                    // Default: 0.1
}

parameters {
  real mu_fcs;                                     // Latent tier differential for FCS in expected outcomes
  real<lower=0> sigma_fbs;                         // Variance component for FBS teams
  real<lower=0> sigma_fcs;                         // Variance component for FCS teams
  real gamma_home;                                 // Structural home-field advantage (log-odds)
  ordered[3] c;                                    // Cutpoints: c[1] < c[2] < c[3]
  vector[N_teams] omega_raw;                       // Standardized ability (non-centered)
}

transformed parameters {
  vector[N_teams] omega;                           // Latent expected resume strength / ability (omega_i)
  vector[2] mu_sub;
  vector[2] sigma_sub;

  mu_sub[1] = 0.0;                                 // Identifiability constraint: FBS anchored at 0
  mu_sub[2] = mu_fcs;
  sigma_sub[1] = sigma_fbs;
  sigma_sub[2] = sigma_fcs;

  // Non-centered hierarchical parameterization
  for (t in 1:N_teams) {
    omega[t] = mu_sub[team_subdivision[t]] + sigma_sub[team_subdivision[t]] * omega_raw[t];
  }
}

model {
  // Priors
  mu_fcs ~ normal(prior_mu_fcs_mean, prior_mu_fcs_sd);
  sigma_fbs ~ exponential(prior_sigma_rate);
  sigma_fcs ~ exponential(prior_sigma_rate);
  gamma_home ~ normal(prior_gamma_mean, prior_gamma_sd);
  omega_raw ~ std_normal();
  
  // Weakly informative prior on ordered cutpoints
  c ~ normal(0, 2.0);

  // Cumulative Ordered Logit Likelihood
  {
    vector[N_games] eta;
    for (k in 1:N_games) {
      eta[k] = omega[home_team[k]] - omega[away_team[k]] + gamma_home * (1 - neutral_site[k]);
    }
    outcome ~ ordered_logistic(eta, c);
  }
}

generated quantities {
  vector[N_games] log_lik;                         // Pointwise log-likelihood for LOO-CV
  array[N_games] int<lower=1, upper=4> y_rep;      // Posterior predictive replicates

  for (k in 1:N_games) {
    real eta_k = omega[home_team[k]] - omega[away_team[k]] + gamma_home * (1 - neutral_site[k]);
    log_lik[k] = ordered_logistic_lpmf(outcome[k] | eta_k, c);
    y_rep[k] = ordered_logistic_rng(eta_k, c);
  }
}
