# Teaching notes

## Learner preferences
- Self-rated "solid" on CDFs, quantiles and log-likelihood, but "over-explaining won't hurt". Spell out each step and keep derivations visible.
- Dislikes point-error metrics (MAE, RMSE, median error) as the main yardstick and wants scores that use the full predictive distribution.
- Mission covers all four uses: picking between model variants, the paper, the dashboard, and general understanding.
- **Wants all lessons in a single file** (2026-10-19): `lessons/scoring-forecasts-course.html`. Add new lessons as sections there, and add new widgets to `assets/`.
- **Graphics and visuals always help** (2026-10-19). Every lesson gets at least one real-data figure and one interactive.
- Asked explicitly that credible sets, and the different ways to get them, be covered.

## Working conventions
- Ground every lesson in the project's own backtest: `targets::tar_read(backtest)`. Per-game forecasts as 199 quantiles are in `targets::tar_read(snapshots)$predictions$pred_q`, at levels `pred_probs` (`R/03_rankings.R:78`). A game is predicted from the snapshot where `week_seq == week + 1`.
- Backtest headline numbers (all games, as of 2026-10-19): n = 19,177, MAE 12.9, RMSE 16.3, Brier 0.166, log loss 0.498, CRPS 9.20, coverage 50/80/95 = 49.8/79.2/94.5%.
- Example games already used: The Game 2024 (Brier 0.675, CRPS 10.4, log score 4.22) and the 2023 Orange Bowl (Brier 0.095, CRPS 44.3, log score about 11). Credible-set examples come from the 2025 postseason posterior: Indiana's rating, Notre Dame's rank, and Indiana's win probability against Notre Dame.
- The workspace lives in the repo root (MISSION.md, RESOURCES.md, NOTES.md, lessons/, reference/, assets/, learning-records/). None of it is committed.

## Glossary candidates (promote only once the user uses them correctly)
- Brier score, CRPS, proper scoring rule, threshold, predictive distribution, point forecast.

## Course status (single file)
Lessons 1–7 written 2026-10-19: credible sets; Brier → CRPS; log score; interval score and coverage; PIT and calibration vs. sharpness; paired comparisons with clustered SE and skill scores; which number for which job, plus a mixed review quiz.
- Data: `assets/course-data.js`, built by `Rscript assets/prep-course-data.R` from the targets store. Rebuild after new weeks.
- Widgets: `assets/charts.js` (SVG primitives), `assets/scoring-widgets.js`, `assets/forecast-explorer.js`, `assets/quiz.js`.

## Possible next lessons
- Threshold-weighted CRPS, focusing the score on the region around the line (ATS).
- Decomposing the CRPS into miscalibration, discrimination and uncertainty (Arnold et al. 2023).
- Reliability diagrams for win probabilities, using the pipeline's `calib()` bins.
- Spaced review: re-run the Lesson 7 quiz a few days after the user first reads the course.
