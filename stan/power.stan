// =============================================================================
// Power model: each team has a net strength r (points, neutral field) and a total tendency
// t (how many points its games produce), both moving week to week as random walks.
// Offence and defence follow: off = (r + t) / 2, def = (r - t) / 2, so a game's expected
// home score is base + (mu + T) / 2 and away score base + (T - mu) / 2, with
//   mu = r[home] - r[away] + hfa * home_adv      (expected margin)
//   T  = t[home] + t[away]                       (expected total, relative to 2 * base)
//
// Margin, observed through two per-game signals that share r:
//   eff    : net garbage-time-filtered PPA (home offence total - away offence total)
//            eff ~ Normal(lambda * mu, sigma_eff)
//   margin : regulation margin (OT games -> 0), conditioned on the eff residual so the
//            two signals are not double-counted as independent evidence
//            margin ~ F(mu + kappa * (eff - lambda * mu); sigma_m, ...)
// Games without eff use the margin's marginal law: the same family with the scales widened by
// kappa^2 sigma_eff^2 (exact for the normal and the mixture, an approximation for the t).
// F is one of four families (margin_family):
//   1 normal(loc, sigma)
//   2 Student-t(nu, loc, sigma)
//   3 two-normal scale mixture: (1 - p_wide) normal(loc, sigma) + p_wide normal(loc, sigma_w),
//     sigma_w = sigma + sigma_gap > sigma (ordered, so the labels are identified)
//   4 Student-t whose scale grows with the expected margin: sigma_g = sqrt(sigma^2 + gamma_m^2 mu^2)
//
// Total (final score, games without overtime), observed the same way through the summed
// offensive PPA of both teams:
//   eff_sum ~ Normal(alpha_es + lambda_t * T, sigma_es)
//   total   ~ Normal(2 base + T + kappa_t * (eff_sum - alpha_es - lambda_t * T), sigma_tot)
//
// Preseason level (week 1): r0 = mu_conf[conf] + b_prev * prev_dev + s_team * z,
//            mu_conf ~ Normal(mu_tier[tier], sigma_conf), tiers FBS (anchored at 0) / FCS / non-D1;
//            t0 has the same structure with its own conference spread, tier offsets and carry-over.
// prev_dev is last season's final rating relative to its tier's mean (0 when unknown); prev_sd
// is its posterior sd, integrated out: s_team = sqrt(sigma_team^2 + (b_prev prev_sd)^2).
// Optional preseason terms (each behind a switch, 0 for teams without the data):
//   b_prev_tier  FCS and non-D1 teams get their own carry-over b_prev_low
//   use_ret      carry-over b_prev + b_ret * ret and a level shift c_ret * ret, where ret is the
//                share of last season's production that returns (centred within FBS)
//   use_tal      level shift b_tal * tal (roster talent composite, standardised within FBS)
//   use_prev2    level shift b_prev2 * prev2_dev (the rating two seasons back vs. its tier mean)
//   use_coach    sigma_team scaled by exp(delta_coach) for teams with a new head coach
//
// Dynamics: a team's state at each week it plays ("states", in week order per team) is its
// previous state plus tau * sqrt(weeks elapsed) * N(0, 1); the first state steps from r0 (the
// week-1 level). r in generated quantities is the rating at the snapshot week: the last state
// plus the drift since. dynamic = 0 freezes ratings within the season (tau = 0).
//
// Switches for the ablations: conf_level = 0 drops the conference effects; use_total = 0 drops
// the total model (its parameters are then absent).
// =============================================================================

functions {
  // Margin density for family fam; m is the expected margin from the ratings (family 4 only)
  real margin_lpdf(real y, int fam, real loc, real s, real s_w, real p_w, real nu, real gam, real m) {
    if (fam == 1) return normal_lpdf(y | loc, s);
    if (fam == 2) return student_t_lpdf(y | nu, loc, s);
    if (fam == 3) return log_mix(p_w, normal_lpdf(y | loc, s_w), normal_lpdf(y | loc, s));
    return student_t_lpdf(y | nu, loc, sqrt(square(s) + square(gam * m)));
  }
  // The same, summed over a vector of games (vectorised except for the mixture)
  real margins_lpdf(vector y, int fam, vector loc, real s, real s_w, real p_w, real nu, real gam, vector m) {
    if (fam == 1) return normal_lpdf(y | loc, s);
    if (fam == 2) return student_t_lpdf(y | nu, loc, s);
    if (fam == 3) {
      real lp = 0;
      for (i in 1:rows(y)) lp += log_mix(p_w, normal_lpdf(y[i] | loc[i], s_w), normal_lpdf(y[i] | loc[i], s));
      return lp;
    }
    return student_t_lpdf(y | nu, loc, sqrt(square(s) + square(gam * m)));
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
  vector[N_teams] prev_dev_t;                   // last season's total tendency vs. its tier mean
  vector<lower=0>[N_teams] prev_sd_t;

  // Weekly states: one per (team, week with a game), grouped by team and in week order within
  // a team, so a team's walk is a cumulative sum over its block
  int<lower=0> N_states;
  array[N_states] int<lower=1, upper=N_teams> state_team;
  array[N_states] int<lower=1, upper=N_states> state_first;  // first state of the same team
  vector<lower=0>[N_states] state_gap;                       // weeks since the previous state (r0 for the first)
  array[N_teams] int<lower=0, upper=N_states> now_state;     // latest state (0 = no game yet)
  vector<lower=0>[N_teams] now_gap;                          // weeks from it to the snapshot

  int<lower=0> N_games;
  array[N_games] int<lower=1, upper=N_states> home_state;
  array[N_games] int<lower=1, upper=N_states> away_state;
  vector<lower=0, upper=1>[N_games] home_adv;   // 1 = true home game, 0 = neutral site
  vector[N_games] margin;                       // regulation margin, home - away
  array[N_games] int<lower=0, upper=1> has_eff;
  vector[N_games] eff;                          // 0 where has_eff == 0
  vector[N_games] eff_sum;                      // home + away offensive PPA totals; 0 where has_eff == 0
  array[N_games] int<lower=0, upper=1> has_total;
  vector[N_games] total;                        // final total points; 0 where has_total == 0

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
  vector[2] tau_r_prior;                        // weekly step sd of r (normal, truncated at 0)
  vector[2] sigma_gap_prior;                    // mixture: sigma_w - sigma_m
  vector<lower=0>[2] p_wide_prior;              // mixture: Beta(a, b) weight of the wide component
  vector[2] gamma_prior;                        // scale-t: growth of the scale with |mu|
  // Total model
  vector[2] base_prior;                         // points per team, average FBS matchup
  vector[2] t_tier_mean;
  vector<lower=0>[2] t_tier_sd;
  vector[2] sigma_t_team_prior;
  vector[2] sigma_conf_t_prior;
  vector[2] b_prev_t_prior;
  vector[2] tau_t_prior;
  vector[2] lambda_t_prior;
  vector[2] alpha_es_prior;
  vector[2] kappa_t_prior;
  vector[2] sigma_es_prior;
  vector[2] sigma_tot_prior;

  // Preseason covariates (0 where unknown) and their priors
  vector[N_teams] ret;
  vector[N_teams] tal;
  vector[N_teams] prev2_dev;
  vector<lower=0, upper=1>[N_teams] coach_new;
  int<lower=0, upper=1> b_prev_tier;
  int<lower=0, upper=1> use_ret;
  int<lower=0, upper=1> use_tal;
  int<lower=0, upper=1> use_prev2;
  int<lower=0, upper=1> use_coach;
  vector[2] b_prev_low_prior;
  vector[2] b_ret_prior;
  vector[2] c_ret_prior;
  vector[2] b_tal_prior;
  vector[2] b_prev2_prior;
  vector[2] delta_coach_prior;

  int<lower=1, upper=4> margin_family;
  int<lower=0, upper=1> dynamic;
  int<lower=0, upper=1> use_total;
  int<lower=0, upper=1> conf_level;
  int<lower=0, upper=1> compute_ll;
}

transformed data {
  int N_eff = sum(has_eff);
  array[N_eff] int idx_eff;
  array[N_games - N_eff] int idx_noeff;
  int is_mix = margin_family == 3;
  int is_sct = margin_family == 4;
  int dyn_t = dynamic * use_total;
  vector[N_states] sqrt_gap = sqrt(state_gap);
  // Totals: games with eff_sum; with eff_sum and a total; with a total but no eff_sum
  int N_es_tot = 0;
  int N_tot_only = 0;
  for (k in 1:N_games) {
    if (has_eff[k] == 1 && has_total[k] == 1) N_es_tot += 1;
    if (has_eff[k] == 0 && has_total[k] == 1) N_tot_only += 1;
  }
  array[N_es_tot] int idx_es_tot;
  array[N_tot_only] int idx_tot_only;
  {
    int i = 1;
    int j = 1;
    for (k in 1:N_games) {
      if (has_eff[k] == 1 && has_total[k] == 1) { idx_es_tot[i] = k; i += 1; }
      if (has_eff[k] == 0 && has_total[k] == 1) { idx_tot_only[j] = k; j += 1; }
    }
  }
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
  vector<lower=0>[dynamic] tau_r_raw;
  vector[dynamic ? N_states : 0] eps_r;

  real hfa;
  real<lower=0> lambda;
  real<lower=0> sigma_eff;
  real kappa;
  real<lower=0> sigma_m;
  real<lower=nu_lower> nu;                      // used by families 2 and 4 only
  vector<lower=0>[is_mix] sigma_gap_raw;
  vector<lower=0, upper=1>[is_mix] p_wide_raw;
  vector<lower=0>[is_sct] gamma_raw;

  // Preseason terms
  vector<lower=0, upper=1>[b_prev_tier] b_prev_low_raw;
  vector[use_ret] b_ret_raw;
  vector[use_ret] c_ret_raw;
  vector[use_tal] b_tal_raw;
  vector[use_prev2] b_prev2_raw;
  vector[use_coach] delta_coach_raw;

  // Total model
  vector[use_total] base_raw;
  vector[use_total ? 2 : 0] t_tier_raw;
  vector<lower=0>[use_total] sigma_conf_t_raw;
  vector[use_total ? N_conf : 0] conf_t_raw;
  vector<lower=0>[use_total] sigma_t_team_raw;
  vector<lower=0, upper=1>[use_total] b_prev_t_raw;
  vector[use_total ? N_teams : 0] z_t;
  vector<lower=0>[dyn_t] tau_t_raw;
  vector[dyn_t ? N_states : 0] eps_t;
  vector<lower=0>[use_total] lambda_t_raw;
  vector[use_total] alpha_es_raw;
  vector<lower=0>[use_total] sigma_es_raw;
  vector[use_total] kappa_t_raw;
  vector<lower=0>[use_total] sigma_tot_raw;
}

transformed parameters {
  vector[3] mu_tier = append_row(0, mu_tier_raw);
  vector[N_conf] mu_conf = mu_tier[conf_tier] + conf_level * sigma_conf * conf_raw;
  vector[N_teams] r0;
  {
    vector[N_teams] carry = rep_vector(b_prev, N_teams);
    vector[N_teams] s_team = sigma_team[team_tier];
    if (b_prev_tier) for (i in 1:N_teams) if (team_tier[i] > 1) carry[i] = b_prev_low_raw[1];
    if (use_ret) carry += b_ret_raw[1] * ret;
    if (use_coach) s_team = s_team .* exp(delta_coach_raw[1] * coach_new);
    r0 = mu_conf[team_conf] + carry .* prev_dev + sqrt(square(s_team) + square(carry .* prev_sd)) .* z;
    if (use_ret) r0 += c_ret_raw[1] * ret;
    if (use_tal) r0 += b_tal_raw[1] * tal;
    if (use_prev2) r0 += b_prev2_raw[1] * prev2_dev;
  }
  real sigma_marg = sqrt(square(sigma_m) + square(kappa * sigma_eff));
  // Family parameters as scalars (defaults where the family does not use them)
  real tau_r = 0;
  real sigma_w = sigma_m;
  real p_wide = 0;
  real gamma_m = 0;
  if (dynamic) tau_r = tau_r_raw[1];
  if (is_mix) { sigma_w = sigma_m + sigma_gap_raw[1]; p_wide = p_wide_raw[1]; }
  if (is_sct) gamma_m = gamma_raw[1];
  real sigma_marg_w = sqrt(square(sigma_w) + square(kappa * sigma_eff));

  // Random walk as a cumulative sum of steps within each team's block of states
  vector[N_states] r_state = r0[state_team];
  if (dynamic && N_states > 0) {
    vector[N_states] step = sqrt_gap .* eps_r;
    vector[N_states] cs = cumulative_sum(step);
    r_state += tau_r * (cs - cs[state_first] + step[state_first]);
  }

  // Total model (empty when use_total = 0)
  vector[use_total ? N_teams : 0] t0;
  vector[use_total ? N_states : 0] t_state;
  if (use_total) {
    vector[3] t_tier = append_row(0, t_tier_raw);
    vector[N_conf] t_conf = t_tier[conf_tier] + conf_level * sigma_conf_t_raw[1] * conf_t_raw;
    t0 = t_conf[team_conf] + b_prev_t_raw[1] * prev_dev_t
         + sqrt(square(sigma_t_team_raw[1]) + square(b_prev_t_raw[1] * prev_sd_t)) .* z_t;
    t_state = t0[state_team];
    if (dyn_t && N_states > 0) {
      vector[N_states] step = sqrt_gap .* eps_t;
      vector[N_states] cs = cumulative_sum(step);
      t_state += tau_t_raw[1] * (cs - cs[state_first] + step[state_first]);
    }
  }

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
  if (dynamic) lprior += normal_lpdf(tau_r_raw | tau_r_prior[1], tau_r_prior[2]);
  if (is_mix) lprior += normal_lpdf(sigma_gap_raw | sigma_gap_prior[1], sigma_gap_prior[2])
                        + beta_lpdf(p_wide_raw | p_wide_prior[1], p_wide_prior[2]);
  if (is_sct) lprior += normal_lpdf(gamma_raw | gamma_prior[1], gamma_prior[2]);
  if (use_total) {
    lprior += normal_lpdf(base_raw | base_prior[1], base_prior[2])
            + normal_lpdf(t_tier_raw | t_tier_mean, t_tier_sd)
            + normal_lpdf(sigma_conf_t_raw | sigma_conf_t_prior[1], sigma_conf_t_prior[2])
            + normal_lpdf(sigma_t_team_raw | sigma_t_team_prior[1], sigma_t_team_prior[2])
            + normal_lpdf(b_prev_t_raw | b_prev_t_prior[1], b_prev_t_prior[2])
            + normal_lpdf(lambda_t_raw | lambda_t_prior[1], lambda_t_prior[2])
            + normal_lpdf(alpha_es_raw | alpha_es_prior[1], alpha_es_prior[2])
            + normal_lpdf(kappa_t_raw | kappa_t_prior[1], kappa_t_prior[2])
            + normal_lpdf(sigma_es_raw | sigma_es_prior[1], sigma_es_prior[2])
            + normal_lpdf(sigma_tot_raw | sigma_tot_prior[1], sigma_tot_prior[2]);
    if (dyn_t) lprior += normal_lpdf(tau_t_raw | tau_t_prior[1], tau_t_prior[2]);
  }
  if (b_prev_tier) lprior += normal_lpdf(b_prev_low_raw | b_prev_low_prior[1], b_prev_low_prior[2]);
  if (use_ret) lprior += normal_lpdf(b_ret_raw | b_ret_prior[1], b_ret_prior[2])
                         + normal_lpdf(c_ret_raw | c_ret_prior[1], c_ret_prior[2]);
  if (use_tal) lprior += normal_lpdf(b_tal_raw | b_tal_prior[1], b_tal_prior[2]);
  if (use_prev2) lprior += normal_lpdf(b_prev2_raw | b_prev2_prior[1], b_prev2_prior[2]);
  if (use_coach) lprior += normal_lpdf(delta_coach_raw | delta_coach_prior[1], delta_coach_prior[2]);
}

model {
  target += lprior;
  conf_raw ~ std_normal();
  z ~ std_normal();
  eps_r ~ std_normal();
  conf_t_raw ~ std_normal();
  z_t ~ std_normal();
  eps_t ~ std_normal();

  if (N_games > 0) {
    vector[N_games] mu = r_state[home_state] - r_state[away_state] + hfa * home_adv;
    vector[N_eff] mu_e = mu[idx_eff];
    vector[N_games - N_eff] mu_n = mu[idx_noeff];
    vector[N_eff] res = eff[idx_eff] - lambda * mu_e;
    target += normal_lpdf(eff[idx_eff] | lambda * mu_e, sigma_eff);
    target += margins_lpdf(margin[idx_eff] | margin_family, mu_e + kappa * res, sigma_m, sigma_w, p_wide, nu, gamma_m, mu_e);
    target += margins_lpdf(margin[idx_noeff] | margin_family, mu_n, sigma_marg, sigma_marg_w, p_wide, nu, gamma_m, mu_n);

    if (use_total) {
      vector[N_games] tt = t_state[home_state] + t_state[away_state];
      vector[N_games] es_mu = alpha_es_raw[1] + lambda_t_raw[1] * tt;
      real tot_marg = sqrt(square(sigma_tot_raw[1]) + square(kappa_t_raw[1] * sigma_es_raw[1]));
      target += normal_lpdf(eff_sum[idx_eff] | es_mu[idx_eff], sigma_es_raw[1]);
      target += normal_lpdf(total[idx_es_tot] | 2 * base_raw[1] + tt[idx_es_tot]
                            + kappa_t_raw[1] * (eff_sum[idx_es_tot] - es_mu[idx_es_tot]), sigma_tot_raw[1]);
      target += normal_lpdf(total[idx_tot_only] | 2 * base_raw[1] + tt[idx_tot_only], tot_marg);
    }
  }
}

generated quantities {
  // Ratings at the snapshot week: the latest state plus the random-walk drift since
  vector[N_teams] r;
  vector[use_total ? N_teams : 0] t;
  for (i in 1:N_teams) {
    if (now_state[i] == 0) r[i] = r0[i];
    else r[i] = r_state[now_state[i]];
    if (dynamic) r[i] += tau_r * sqrt(now_gap[i]) * normal_rng(0, 1);
  }
  if (use_total) {
    for (i in 1:N_teams) {
      if (now_state[i] == 0) t[i] = t0[i];
      else t[i] = t_state[now_state[i]];
      if (dyn_t) t[i] += tau_t_raw[1] * sqrt(now_gap[i]) * normal_rng(0, 1);
    }
  }

  // log_lik: joint p(e, y) where e is observed, p(y) otherwise (the fitted margin likelihood).
  // log_lik_margin: p(y) alone for every game, the marginal law of the margin; comparable
  // across models whether or not they use the efficiency margin.
  // log_lik_total: the total's contribution (joint with eff_sum where observed).
  vector[compute_ll ? N_games : 0] log_lik;
  vector[compute_ll ? N_games : 0] log_lik_margin;
  vector[compute_ll * use_total ? N_games : 0] log_lik_total;
  if (compute_ll) {
    vector[N_games] mu = r_state[home_state] - r_state[away_state] + hfa * home_adv;
    for (k in 1:N_games) {
      log_lik_margin[k] = margin_lpdf(margin[k] | margin_family, mu[k], sigma_marg, sigma_marg_w, p_wide, nu, gamma_m, mu[k]);
      if (has_eff[k] == 1) {
        real res = eff[k] - lambda * mu[k];
        log_lik[k] = normal_lpdf(eff[k] | lambda * mu[k], sigma_eff)
                   + margin_lpdf(margin[k] | margin_family, mu[k] + kappa * res, sigma_m, sigma_w, p_wide, nu, gamma_m, mu[k]);
      } else {
        log_lik[k] = log_lik_margin[k];
      }
    }
    if (use_total) {
      vector[N_games] tt = t_state[home_state] + t_state[away_state];
      real tot_marg = sqrt(square(sigma_tot_raw[1]) + square(kappa_t_raw[1] * sigma_es_raw[1]));
      for (k in 1:N_games) {
        log_lik_total[k] = 0;
        if (has_eff[k] == 1) {
          real es_mu = alpha_es_raw[1] + lambda_t_raw[1] * tt[k];
          log_lik_total[k] = normal_lpdf(eff_sum[k] | es_mu, sigma_es_raw[1]);
          if (has_total[k] == 1)
            log_lik_total[k] += normal_lpdf(total[k] | 2 * base_raw[1] + tt[k] + kappa_t_raw[1] * (eff_sum[k] - es_mu), sigma_tot_raw[1]);
        } else if (has_total[k] == 1) {
          log_lik_total[k] = normal_lpdf(total[k] | 2 * base_raw[1] + tt[k], tot_marg);
        }
      }
    }
  }
}
