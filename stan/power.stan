// =============================================================================
// Power model: one latent team strength r (points, neutral field) per team,
// observed through two per-game signals that share it:
//   eff    : net garbage-time-filtered PPA (home offence total - away offence total)
//            eff ~ Normal(lambda * mu, sigma_eff)
//   margin : regulation margin (OT games -> 0), conditioned on the eff residual so the
//            two signals are not double-counted as independent evidence
//            margin ~ Student-t(nu, mu + kappa * (eff - lambda * mu), sigma_m)
// where mu = r[home] - r[away] + hfa * home_adv.
// Games without eff use margin's marginal: Student-t(nu, mu, sqrt(sigma_m^2 + kappa^2 sigma_eff^2)).
//
// Hierarchy: r = mu_conf[conf] + b_prev * prev_dev + s_team * z,
//            mu_conf ~ Normal(mu_tier[tier], sigma_conf), tiers FBS (anchored at 0) / FCS / non-D1.
// prev_dev is last season's final rating relative to its tier's mean (0 when unknown), so
// b_prev is the year-to-year carry-over (team and conference strength), estimated from the data.
// prev_sd is the posterior sd of that rating: treating last season's true rating as
// Normal(prev, prev_sd^2) and integrating it out gives s_team = sqrt(sigma_team^2 + (b_prev prev_sd)^2).
//
// Switches for the ablations (the published model has both on): student_t = 0 uses a normal
// margin; conf_level = 0 drops the conference effects (teams pool to their tier only).
// =============================================================================

functions {
  // Margin density: Student-t, or normal when student_t = 0
  real margin_lpdf(real y, int student_t, real nu, real loc, real scale) {
    return student_t ? student_t_lpdf(y | nu, loc, scale) : normal_lpdf(y | loc, scale);
  }
}

data {
  int<lower=1> N_teams;
  int<lower=1> N_conf;
  array[N_teams] int<lower=1, upper=N_conf> team_conf;
  array[N_conf] int<lower=1, upper=3> conf_tier;
  array[N_teams] int<lower=1, upper=3> team_tier;
  vector[N_teams] prev_dev;
  vector<lower=0>[N_teams] prev_sd;

  int<lower=0> N_games;
  array[N_games] int<lower=1, upper=N_teams> home;
  array[N_games] int<lower=1, upper=N_teams> away;
  vector<lower=0, upper=1>[N_games] home_adv;   // 1 = true home game, 0 = neutral site
  vector[N_games] margin;                       // regulation margin, home - away
  array[N_games] int<lower=0, upper=1> has_eff;
  vector[N_games] eff;                          // 0 where has_eff == 0
  vector<lower=0>[N_games] weight;              // recency weights (1 = full weight)

  real hfa_mean;
  real<lower=0> hfa_sd;
  vector[2] tier_mean;                          // prior means for FCS, non-D1 tier offsets
  vector<lower=0>[2] tier_sd;
  real b_prev_mean;
  real<lower=0> b_prev_sd;
  // Informative priors (mean, sd) for the structural noise/scale parameters: they barely move
  // between seasons, and without them week-1 fits (~1 game per team) funnel
  vector[3] sigma_team_mean;
  vector<lower=0>[3] sigma_team_sd;
  vector[2] sigma_conf_prior;
  vector[2] lambda_prior;
  vector[2] kappa_prior;
  vector[2] sigma_eff_prior;
  vector[2] sigma_m_prior;
  vector<lower=0>[2] nu_prior;                  // Gamma(shape, rate), truncated to nu > nu_lower
  real<lower=0> nu_lower;

  int<lower=0, upper=1> student_t;
  int<lower=0, upper=1> conf_level;
  int<lower=0, upper=1> compute_ll;
}

transformed data {
  int N_eff = sum(has_eff);
  array[N_eff] int idx_eff;
  array[N_games - N_eff] int idx_noeff;
  {
    int i = 1;
    int j = 1;
    for (k in 1:N_games) {
      if (has_eff[k] == 1) { idx_eff[i] = k; i += 1; }
      else { idx_noeff[j] = k; j += 1; }
    }
  }
}

parameters {
  vector[2] mu_tier_raw;
  real<lower=0> sigma_conf;
  vector[N_conf] conf_raw;
  vector<lower=0>[3] sigma_team;
  real<lower=0, upper=1> b_prev;
  vector[N_teams] z;

  real hfa;
  real<lower=0> lambda;
  real<lower=0> sigma_eff;
  real kappa;
  real<lower=0> sigma_m;
  real<lower=nu_lower> nu;
}

transformed parameters {
  vector[3] mu_tier = append_row(0, mu_tier_raw);
  vector[N_conf] mu_conf = mu_tier[conf_tier] + conf_level * sigma_conf * conf_raw;
  vector[N_teams] r = mu_conf[team_conf] + b_prev * prev_dev
                      + sqrt(square(sigma_team[team_tier]) + square(b_prev * prev_sd)) .* z;
  real sigma_marg = sqrt(square(sigma_m) + square(kappa * sigma_eff));

  // Log prior of the population-level parameters (used by priorsense power-scaling)
  real lprior = normal_lpdf(mu_tier_raw | tier_mean, tier_sd)
              + normal_lpdf(sigma_conf | sigma_conf_prior[1], sigma_conf_prior[2])
              + normal_lpdf(sigma_team | sigma_team_mean, sigma_team_sd)
              + normal_lpdf(b_prev | b_prev_mean, b_prev_sd)
              + normal_lpdf(hfa | hfa_mean, hfa_sd)
              + normal_lpdf(lambda | lambda_prior[1], lambda_prior[2])
              + normal_lpdf(sigma_eff | sigma_eff_prior[1], sigma_eff_prior[2])
              + normal_lpdf(kappa | kappa_prior[1], kappa_prior[2])
              + normal_lpdf(sigma_m | sigma_m_prior[1], sigma_m_prior[2])
              + gamma_lpdf(nu | nu_prior[1], nu_prior[2]) - gamma_lccdf(nu_lower | nu_prior[1], nu_prior[2]);
}

model {
  target += lprior;
  conf_raw ~ std_normal();
  z ~ std_normal();

  if (N_games > 0) {
    vector[N_games] mu = r[home] - r[away] + hfa * home_adv;
    for (i in 1:N_eff) {
      int k = idx_eff[i];
      real res = eff[k] - lambda * mu[k];
      target += weight[k] * (normal_lpdf(eff[k] | lambda * mu[k], sigma_eff)
                             + margin_lpdf(margin[k] | student_t, nu, mu[k] + kappa * res, sigma_m));
    }
    for (j in 1:(N_games - N_eff)) {
      int k = idx_noeff[j];
      target += weight[k] * margin_lpdf(margin[k] | student_t, nu, mu[k], sigma_marg);
    }
  }
}

generated quantities {
  // log_lik: joint p(e, y) where e is observed, p(y) otherwise (the fitted likelihood).
  // log_lik_margin: p(y) alone for every game, the marginal law of the margin; comparable
  // across models whether or not they use the efficiency margin.
  vector[compute_ll ? N_games : 0] log_lik;
  vector[compute_ll ? N_games : 0] log_lik_margin;
  if (compute_ll) {
    vector[N_games] mu = r[home] - r[away] + hfa * home_adv;
    for (k in 1:N_games) {
      log_lik_margin[k] = margin_lpdf(margin[k] | student_t, nu, mu[k], sigma_marg);
      if (has_eff[k] == 1) {
        real res = eff[k] - lambda * mu[k];
        log_lik[k] = normal_lpdf(eff[k] | lambda * mu[k], sigma_eff)
                   + margin_lpdf(margin[k] | student_t, nu, mu[k] + kappa * res, sigma_m);
      } else {
        log_lik[k] = log_lik_margin[k];
      }
    }
  }
}
