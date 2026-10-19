# Scoring Distributional Forecasts Resources

## Knowledge

- [Book: _Forecasting: Principles and Practice_ (3rd ed.), §5.9 "Evaluating distributional forecast accuracy", Hyndman & Athanasopoulos](https://otexts.com/fpp3/distaccuracy.html)
  Free and readable. Covers quantile (pinball) scores, the Winkler interval score, the CRPS as an average of quantile scores, and skill scores. Use for: first exposure to any of these, and skill scores against a baseline (the line, Elo).
- [Paper: Gneiting & Raftery (2007), "Strictly proper scoring rules, prediction, and estimation", _JASA_ 102(477)](https://doi.org/10.1198/016214506000001437)
  The standard reference on properness. Covers the CRPS, its kernel form E|X−y| − ½E|X−X′|, and the log score. Use for: any claim about properness or the CRPS's forms. The publisher's version is paywalled, and the free DTIC tech-report copies returned 403 here.
- [Paper: Matheson & Winkler (1976), "Scoring rules for continuous probability distributions", _Management Science_ 22(10)](https://doi.org/10.1287/mnsc.22.10.1087)
  Origin of the CRPS. Use for: citing its definition.
- [Paper: Brier (1950), "Verification of forecasts expressed in terms of probability", _Monthly Weather Review_ 78(1)](https://doi.org/10.1175/1520-0493(1950)078%3C0001:VOFEIT%3E2.0.CO;2)
  Origin of the Brier score. Use for: citing its definition.
- [Paper: Arnold, Walz, Ziegel & Gneiting (2023), "Decompositions of the mean continuous ranked probability score", arXiv:2311.14122](https://arxiv.org/abs/2311.14122)
  States that the mean CRPS is the integral over thresholds of the mean Brier score. It uses that to split the CRPS into miscalibration, discrimination and uncertainty. Use for: the CRPS–Brier link, and the future lesson on decomposing the CRPS.
- [Paper: Jordan, Krüger & Lerch (2019), "Evaluating probabilistic forecasts with scoringRules", _JSS_ 90(12)](https://www.jstatsoft.org/article/view/v090i12)
  R package for the CRPS and log score from draws or closed forms. Use for: computing scores from Stan draws. scoringRules is not installed in this project yet.
- [Paper: Vehtari, Gelman & Gabry (2017), "Practical Bayesian model evaluation using leave-one-out cross-validation and WAIC", _Statistics and Computing_](https://doi.org/10.1007/s11222-016-9696-4)
  elpd/LOO is the log score estimated out of sample. It is already cited in the paper. Use for: the log score lesson.
- [Paper: Säilynoja, Bürkner & Vehtari (2022), graphical test for discrete uniformity (PIT-ECDF), _Statistics and Computing_](https://doi.org/10.1007/s11222-022-10090-6)
  Already cited in the paper. Use for: the calibration and PIT lesson.
- [Wikipedia: "Scoring rule"](https://en.wikipedia.org/wiki/Scoring_rule)
  Quick definitions of properness, locality, the CRPS integral, and the CRPS as a generalisation of MAE. Use for: fast lookups. Cite the primary sources rather than this page.
- [Paper: Gneiting, Balabdaoui & Raftery (2007), "Probabilistic forecasts, calibration and sharpness", _JRSS-B_ 69(2)](https://sites.stat.washington.edu/people/raftery/Research/PDF/Gneiting2007jrssb.pdf)
  Free PDF. Source of "maximise sharpness subject to calibration", PIT shapes (hump = too wide, U = too narrow, triangle = biased), the climatological forecaster, and the Brier score plot whose area is the CRPS. Use for: the calibration lesson.
- [Docs: bayestestR, "Credible Intervals (CI)" (Makowski, Ben-Shachar & Lüdecke)](https://easystats.github.io/bayestestR/articles/credible_interval.html)
  HDI vs. ETI, including the transformation argument (the ETI is invariant, the HDI is not), and credible vs. confidence intervals. Use for: credible sets.
- [Docs: ggdist `point_interval()`](https://mjskay.github.io/ggdist/reference/point_interval.html)
  `qi` (equal-tailed), `hdi` (may return several intervals), `hdci` (shortest single interval). Use for: computing credible sets in R.
- [Docs: posterior `mcse_quantile()`](https://mc-stan.org/posterior/reference/mcse_quantile.html)
  Monte Carlo SE of quantile estimates. Use for: checking interval endpoints.
- [Wikipedia: "Credible interval"](https://en.wikipedia.org/wiki/Credible_interval)
  Definitions of the equal-tailed, smallest (HDI) and mean-centred intervals, and highest-density regions that can be disconnected.
- [Paper: Diebold & Mariano (1995), "Comparing predictive accuracy", _JBES_ 13(3)](https://doi.org/10.1080/07350015.1995.10524599)
  Already cited in the paper. Use for: comparing two forecasters' mean scores.

## Wisdom (Communities)

- [Stan Forums](https://discourse.mc-stan.org)
  Moderated, and the LOO/PIT authors answer questions there. Use for: elpd, LOO-PIT and model comparison questions about the Stan model.
- [r/CFBAnalysis](https://www.reddit.com/r/CFBAnalysis/)
  College football modelling community (the CFBD maintainer posts there). Use for: sharing the backtest and getting feedback from other raters on how they score predictions.
- [Cross Validated, `scoring-rules` tag](https://stats.stackexchange.com/questions/tagged/scoring-rules)
  Use for: precise technical questions about properness and comparing scores. The page could not be fetched to verify activity.

## Gaps

- A readable teaching source on cluster-robust standard errors for time-clustered forecast comparisons. The course currently explains this from first principles and from the pipeline's `dm()`.
