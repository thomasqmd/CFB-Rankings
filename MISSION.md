# Mission: Scoring the CFB power model's predictive distributions

## Why
The power model forecasts a full distribution for every game's margin, but it gets judged mostly by point errors (MAE, RMSE) and a Brier score that is hard to interpret. The goal is to score the distributions themselves, so you can pick between model variants with confidence, write and defend the paper's backtest section yourself, and show dashboard readers one honest accuracy number.

## Success looks like
- Choose and defend a credible-set method (equal-tailed, HDI, highest-density set) for ratings, ranks and probabilities, and know when they disagree.
- Explain the Brier score, CRPS and log score in your own words, compute each by hand for one game, and say what each rewards and punishes.
- Pick between two model variants (ablations, priors, margin families) from per-game score differences with honest uncertainty, and know what it means when the CRPS and log score disagree.
- Tell calibration apart from sharpness using the backtest's coverage and PIT plots.
- Write the paper's backtest paragraph without leaning on an agent.
- Choose and justify one headline accuracy number for the dashboard.

## Constraints
- Comfortable with CDFs, quantiles and likelihood; thorough explanations are welcome.
- Lessons use this project's own backtest data (`targets::tar_read(backtest)`), not toy examples.
- All lessons live in one file (`lessons/scoring-forecasts-course.html`), with plenty of graphics.

## Out of scope
- Changing the model itself. This is about judging it.
- MCMC convergence diagnostics.
