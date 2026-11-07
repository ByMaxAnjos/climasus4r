# Contract between sus_mod_spacetime_bayes() output and predict/exceedance.
# A fake fit mimics the bayes return structure (INLA not required).

fake_bayes <- function(unit = "year", keys = 2019:2021) {
  T_ <- length(keys)
  structure(list(
    fixed = data.frame(term = c("(Intercept)", "temp"), mean = c(0, 0.1),
                       sd = c(0.01, 0.01)),
    rr = data.frame(code_muni = rep(c("A", "B"), each = T_),
                    time_idx = rep(seq_len(T_), 2),
                    rr_mean = 1.2, rr_lower95 = 1, rr_upper95 = 1.5,
                    p_exceed = 0.7),
    spatial_re = data.frame(code_muni = c("A", "B"), phi_mean = 0, phi_sd = 0),
    temporal_re = data.frame(time_idx = seq_len(T_), psi_mean = c(0, 1, 2)[seq_len(T_)],
                             psi_sd = 0.1),
    model_spec = list(family = "poisson", temporal_model = "rw1"),
    family = "poisson", temporal_model = "rw1",
    data_last_obs = data.frame(code_muni = c("A", "B"), temp = c(10, 20)),
    time_keys = keys, time_unit = unit, n_areas = 2L, n_times = T_
  ), class = c("climasus_spacetime_bayes", "list"))
}

test_that("predict applies psi_mean temporal effect", {
  fit <- fake_bayes()
  nd <- data.frame(code_muni = "A", time_idx = 2L, temp = 0)
  p <- sus_mod_spacetime_predict(fit, newdata = nd, verbose = FALSE)
  expect_equal(p$predictions$pred_mean, exp(1), tolerance = 1e-6)
})

test_that("predict horizon>0 uses stored data_last_obs", {
  fit <- fake_bayes()
  p <- sus_mod_spacetime_predict(fit, horizon = 1L, verbose = FALSE)
  expect_equal(nrow(p$predictions), 2L)
  expect_false(anyNA(p$predictions$pred_mean))
})

test_that("return_samples gives actionable error", {
  expect_error(
    sus_mod_spacetime_predict(fake_bayes(), horizon = 1L, return_samples = TRUE,
                              verbose = FALSE, lang = "en"),
    "does not store"
  )
})

test_that("exceedance maps time_idx to calendar years", {
  fit <- fake_bayes("year", c(2019L, 2020L, 2021L))
  ex <- sus_mod_spacetime_exceedance(fit, aggregate_time = "year", verbose = FALSE)
  tp <- unique(as.character(ex$exceedance$time_period))
  expect_setequal(tp, c("2019", "2020", "2021"))
})

test_that("exceedance maps monthly keys and errors without calendar info", {
  fit <- fake_bayes("month", c(202001L, 202002L, 202003L))
  ex <- sus_mod_spacetime_exceedance(fit, aggregate_time = "month", verbose = FALSE)
  expect_setequal(unique(as.character(ex$exceedance$time_period)),
                  c("2020-01", "2020-02", "2020-03"))
  fit$time_keys <- NULL; fit$time_unit <- NULL
  expect_error(sus_mod_spacetime_exceedance(fit, aggregate_time = "year",
                                            verbose = FALSE))
})
