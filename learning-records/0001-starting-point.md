# Starting point: knows point errors and CDFs; the Brier score is unclear

The user self-reports solid comfort with CDFs, quantiles and log-likelihood, and already uses MAE, RMSE and median error. They do not yet understand the Brier score, and they find point-error scores unsatisfying for a model that predicts distributions. Teaching therefore starts at the Brier → CRPS link, not at probability basics, but each step should still be spelled out, since the user said "over-explaining won't hurt".

**Evidence**: the user's own request ("I don't understand the brier score…") and their answers to the background questions on 2026-10-19.

**Implications**: CDF notation can be used freely. The pipeline already computes the CRPS, coverage and LOO-PIT, so later lessons should explain existing outputs rather than introduce new code.
