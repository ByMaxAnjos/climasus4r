# climasus4r — Validation Detail (companion to paper.md)

This document expands the "Validation against the published literature" section of `paper.md`. It is not part of the JOSS submission itself; JOSS papers are kept short and implementation-focused, so the full crosswalk, both figures, and the caveats below live here instead of in the manuscript.

## What was checked, and what was not

Two separate questions are addressed:

1. **Specification equivalence.** Does `climasus4r` implement the same statistical design that published, peer-reviewed Brazilian temperature-mortality studies use? This is checked by reading the source of `sus_mod_dlnm()`, `sus_mod_pool()`, `sus_mod_af()`, `sus_climate_compute_heatwaves()` / `sus_climate_compute_coldwaves()`, and `sus_climate_aggregate()`'s `degree_days` strategy against the methods sections of the studies below.
2. **End-to-end execution.** Does that specification run without error on data shaped like a real DATASUS + INMET join, converge, and produce internally coherent diagnostics?

The bundled tutorial case study (`scripts/tutorial_01`-`tutorial_09`, `scripts/modelagem_01`-`08`) addresses (2) only. Its exposure series is a **seeded synthetic INMET-like series**, not a live download: `scripts/tutorial_07_clima_estacoes.R` and `scripts/modelagem_01_dlnm.R` fall back to `set.seed(2014); 19 + 4.5*cos(2*pi*(doy-15)/365) + rnorm(n, 0, 1.6)` when `vignettes-pt/dados/caso_clima_estacao.rds` is absent, and the package's own metadata flags this run with `exemplo_minimo = TRUE`. We confirmed this by reproducing the generator and matching it against the saved fit (`dados/mod_dlnm_fit.rds`): all 2170 exposure values match exactly. The daily mortality series (`caso_serie.rds`) is a small synthetic Poisson series for the same reason (mean 1.48 deaths/day, `exemplo_minimo = TRUE`). **No epidemiological claim is made from this bundle** — it demonstrates that the pipeline wiring is correct, not that a heat effect was detected in real São Paulo data. A user with network access and the appropriate INMET/DATASUS credentials can rerun `tutorial_01` through `tutorial_09` to replace this synthetic bundle with a live-downloaded series; the code path is identical, only the two `.rds` fallbacks are bypassed.

## Bundled tutorial results (synthetic exposure, real specification)

- Single-station DLNM fit (`dados/mod_dlnm_fit.rds`): quasi-Poisson, `ns(4,3)` crossbasis, lag 0-21, n = 2170 days, reference = 18.8°C (median). RR at P75 (22.0°C) = 0.735 (95% CI 0.486-1.114); dispersion ratio 1.13 ("adequate"); Ljung-Box p = 0.014 (residual autocorrelation present at this sample size).
- Six-city pooled fit (`vignettes-pt/dados/mod_pool.rds`, via `sus_mod_pool()` + `mvmeta`, REML): heterogeneity Q = 52.8, df = 60, p = 0.735, I² = 0%. Pooled RR at P75 (22.1°C, reference 19.1°C) = 1.01 (0.90-1.14); at P99 (26.3°C) = 1.01 (0.77-1.34) — a flat central estimate with widening uncertainty at the tail, as expected from a synthetic, noise-dominated exposure series with a shared generating process across all six "cities".
- These two numbers (0.735 vs. 1.05 for São Paulo specifically in the pooled BLUP table) are not in tension once their source is clear: one is a single-station fit referenced to its own median (18.8°C), the other is a BLUP shrunk toward the six-city pooled mean and referenced to the pooled median (19.1°C). Both are reported here for transparency; neither should be read as a real effect size.

## Peer-reviewed comparators

| Study | Location / period | Method | Lag structure | Key estimate | DOI |
|---|---|---|---|---|---|
| Gasparrini et al. (2015), *Lancet* | 384 locations, 13 countries incl. São Paulo (MCC Network), 1985-2012 | Two-stage DLNM, quasi-Poisson, ns(8/yr) time spline | 0-21 days | Attributable fraction 7.71% overall (cold 7.29%, heat 0.42%), driven mostly by moderate, not extreme, temperatures | 10.1016/S0140-6736(14)62114-0 |
| Zhao et al. (2021), *Lancet Planet. Health* | Global / Latin America & Caribbean region incl. Brazil, 2000-2019 | Three-stage DLNM-based model | Not reported at regional granularity | Global AF 9.43%; LAC region: cold 4.71%, heat 1.06% | 10.1016/S2542-5196(21)00081-4 |
| Bell et al. (2008), *Int J Epidemiol* | São Paulo, Santiago, Mexico City; São Paulo 1998-2002 | Case-crossover, 754,291 deaths | Same-day apparent temperature, P95 vs. P75 | São Paulo, age 65+: +6.51% mortality (95% CI 3.57-9.52%) — largest of the three cities | 10.1093/ije/dyn094 |
| Son et al. (2016), *Int J Biometeorol* | São Paulo, 1996-2010 (14.5 yr) | GLM + Bayesian hierarchical | P99 vs. P90 | +6.1% total mortality (95% CI 4.7-7.6%), RR ≈ 1.061; heat concentrated in respiratory deaths, cold in cardiovascular | 10.1007/s00484-015-1009-7 |
| Geirinhas et al. (2021), *Environ. Res. Lett.* | Southeast Brazil | Percentile climatology (not an epidemiological model) | 15-day moving window; P80/P90/P95 thresholds, ≥3 consecutive days | Heatwave definition validated against observed compound drought-heat events; no mortality RR reported in this source | 10.1088/1748-9326/abe0eb |
| Lee et al. (2021), *PLOS NTD* | Brazil, national | Climate-suitability index for dengue transmission | Monthly suitability envelope | *Ae. aegypti* suitable range 17.8-34.5°C; not viable below ~10°C or above 40°C (bounds drawn from Mordecai et al., cited therein) | 10.1371/journal.pntd.0009773 |

**Caveats carried over from the literature search:** a commonly cited Brazil-specific attributable-fraction figure from Gasparrini et al. (2015) (~3.5%) appears in secondary sources but could not be confirmed against the primary results table (paywalled); it is deliberately omitted above rather than reported unverified. No paper was found stating an explicit "10-11°C base temperature" degree-day accumulation model for *Aedes aegypti* in Brazil specifically; the ~10°C figure above is the lower non-viability bound from Lee et al. (2021), used here as the closest sourced approximation, not an exact match.

## Figures

- `figures/validation_pooled_cities.png` — six-city BLUP estimates and pooled exposure-response curve from the bundled tutorial (synthetic exposure; demonstrates pipeline wiring).
- `figures/pipeline_overview.png`, `figures/temporal_strategies.png` — architecture diagrams, also used in `paper.md`.
