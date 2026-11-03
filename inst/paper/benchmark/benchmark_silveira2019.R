# ---------------------------------------------------------------------------
# Benchmark: reproduce Silveira et al. (2019), Sci Total Environ 691:996-1004,
# doi:10.1016/j.scitotenv.2019.06.493 -- São Paulo city, cardiovascular
# mortality (ICD-10 chapter IX, I00-I99), all ages.
#
# Published São Paulo estimate (Table 2, 2000-2015):
#   MMT 25.0 °C (MMP 0.84); RR P1 1.60 (1.51-1.71); RR P99 1.15 (1.09-1.22)
#
# Model (Section 2.2), reproduced exactly:
#   quasi-Poisson; crossbasis ns(temp, knots = P10/P75/P90) x
#   ns(lag, 3 log-spaced knots), lag 0-21; ns(RH, 3); ns(time, 4 df/year);
#   day of week.
#
# Difference: INMET automatic station A701 (Mirante) only starts 2006-07-25,
# so the main run uses 2007-2015 station data (the paper filled 2000-2006
# with ERA-Interim).
# ---------------------------------------------------------------------------
library(climasus4r)
library(dplyr)

years   <- 2007:2015
lag_max <- 21L
out_dir <- "inst/paper/benchmark"

# 1. Health: daily CVD deaths of São Paulo residents ------------------------
cvd_daily <- sus_data_import(uf = "SP", year = years, system = "SIM-DO") |>
  sus_data_clean_encoding(lang = "en") |>
  sus_data_standardize(lang = "en") |>
  sus_data_filter_cid(icd_codes = "I00-I99", lang = "en") |>
  sus_data_filter_demographics(city = "3550308", lang = "en") |>
  sus_data_aggregate(
    time_unit = "day",
    group_by  = "residence_municipality_code",
    lang      = "en"
  ) |>
  sus_spatial_join(level = "munic", lang = "en")

# 2. Climate: daily mean temperature and RH, station A701 -------------------
clima <- sus_climate_inmet(years = years, station_code = "A701", lang = "en")

exposure <- sus_climate_aggregate(
  health_data       = cvd_daily,
  climate_data      = clima,
  climate_var       = c("tair_dry_bulb_c", "rh_mean_porc"),
  temporal_strategy = "distributed_lag",
  lag_days          = lag_max,
  lang              = "en"
)

# 3. Confounders as numeric columns (sus_mod_dlnm takes plain covariate names)
temp0 <- exposure$tair_dry_bulb_c_lag0
rh_ns <- splines::ns(exposure$rh_mean_porc_lag0, df = 3)
dow   <- stats::model.matrix(~ factor(weekdays(exposure$date)))[, -1]
colnames(rh_ns) <- paste0("rh_ns", seq_len(ncol(rh_ns)))
colnames(dow)   <- paste0("dow", seq_len(ncol(dow)))
exposure <- dplyr::bind_cols(exposure, as.data.frame(rh_ns), as.data.frame(dow))

# 4. DLNM with the published specification ----------------------------------
fit <- sus_mod_dlnm(
  df           = exposure,
  outcome_col  = "n_deaths",
  climate_col  = "tair_dry_bulb_c",
  lag_max      = lag_max,
  argvar       = list(fun = "ns",
                      knots = stats::quantile(temp0, c(.10, .75, .90), na.rm = TRUE)),
  arglag       = list(fun = "ns", knots = dlnm::logknots(lag_max, 3)),
  covariates   = c(colnames(rh_ns), colnames(dow)),
  dof_per_year = 4L,
  family       = "quasipoisson",
  lang         = "en"
)

# 5. Re-centre at the MMT and predict P1 / P99, as in the paper -------------
cb    <- fit$crossbasis
t_obs <- fit$data_daily$tair_dry_bulb_c_lag0
pct   <- stats::quantile(t_obs, c(.01, .99), na.rm = TRUE)
grid  <- stats::quantile(t_obs, seq(.01, .99, by = .01), na.rm = TRUE)
mmt   <- grid[[which.min(dlnm::crosspred(cb, fit$model, at = grid,
                                         cen = median(t_obs))$allRRfit)]]
# crosspred() sorts `at`, so predict only P1/P99 to keep index 1 = P1, 2 = P99
pr    <- dlnm::crosspred(cb, fit$model, at = unname(pct), cen = mmt)

bench <- tibble::tibble(
  source = c("Silveira et al. 2019 (2000-2015)", "climasus4r (2007-2015)"),
  mmt_c  = c(25.0, round(mmt, 1)),
  mmp    = c(0.84, round(mean(t_obs <= mmt, na.rm = TRUE), 2)),
  rr_p1  = c("1.60 (1.51-1.71)",
             sprintf("%.2f (%.2f-%.2f)", pr$allRRfit[1], pr$allRRlow[1], pr$allRRhigh[1])),
  rr_p99 = c("1.15 (1.09-1.22)",
             sprintf("%.2f (%.2f-%.2f)", pr$allRRfit[2], pr$allRRlow[2], pr$allRRhigh[2])),
  mean_daily_deaths = c(59.0, round(mean(fit$data_daily$y), 1)),
  mean_temp_c       = c(21.4, round(mean(t_obs, na.rm = TRUE), 1))
)
print(bench)
utils::write.csv(bench, file.path(out_dir, "benchmark_silveira2019.csv"), row.names = FALSE)

# 6. Figure: cumulative exposure-response centred at MMT ---------------------
full <- dlnm::crosspred(cb, fit$model, at = grid, cen = mmt)
png(file.path(out_dir, "benchmark_silveira2019.png"), width = 1600, height = 1200, res = 220)
plot(full, "overall", col = "black", lwd = 2, ylim = c(0.8, 2),
     xlab = "Daily mean temperature (°C)", ylab = "Cumulative RR (lag 0-21)",
     main = "São Paulo, cardiovascular mortality: climasus4r vs Silveira et al. (2019)")
abline(v = pct, lty = 2); abline(v = mmt, lty = 3)
abline(v = 25.0, lty = 3, col = "red")
legend("top", bty = "n", lty = c(1, 3, 3), col = c("black", "black", "red"),
       legend = c("climasus4r curve", "climasus4r MMT", "Published MMT (25.0 °C)"))
dev.off()

writeLines(fit$meta$input_meta$history)
