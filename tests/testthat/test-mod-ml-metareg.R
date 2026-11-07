test_that("sus_mod_ml: fewer groups than nfold does not go out of bounds", {
  skip_if_not_installed("xgboost")
  skip_on_cran()
  set.seed(1)
  df <- data.frame(y = rpois(60, 3), x = rnorm(60), x2 = rnorm(60), g = rep(c("a", "b", "c"), 20))
  expect_warning(
    res <- suppressMessages(sus_mod_ml(df, "y", c("x", "x2"), id_col = "g", nfold = 5L,
                                       nrounds = 20L, early_stopping = 10L,
                                       verbose = FALSE, lang = "en")),
    NA  # cli alerts are messages, not warnings
  )
  expect_equal(res$meta$nfold, 3L)
})

.fake_dlnm_fit <- function(seed, lag_max) {
  set.seed(seed)
  n <- 400
  t <- rnorm(n, 25, 5)
  cb <- dlnm::crossbasis(t, lag = lag_max, argvar = list(fun = "lin"),
                         arglag = list(fun = "poly", degree = 2))
  y <- rpois(n, 5)
  m <- stats::glm(y ~ cb, family = poisson())
  names(m$coefficients) <- sub("^cb", "cb", names(m$coefficients))
  structure(list(
    model = m, crossbasis = cb, data_daily = data.frame(tmean = t),
    meta = list(lag_max = lag_max, climate_col = "tmean",
                argvar = list(fun = "lin"), arglag = list(fun = "poly", degree = 2))
  ), class = "climasus_dlnm")
}

test_that("sus_mod_metaregression: double lag_max works and alpha sets CI level", {
  skip_if_not_installed("dlnm")
  skip_if_not_installed("mvmeta")
  skip_on_cran()
  fits <- list(A = .fake_dlnm_fit(1, 21), B = .fake_dlnm_fit(2, 21), C = .fake_dlnm_fit(3, 21),
               D = .fake_dlnm_fit(4, 21))
  cov <- data.frame(c1 = c(1, 2, 3, 5), row.names = names(fits))
  r95 <- suppressMessages(sus_mod_metaregression(fits, cov, blup = FALSE, verbose = FALSE, lang = "en"))
  r80 <- suppressMessages(sus_mod_metaregression(fits, cov, blup = FALSE, alpha = 0.2, verbose = FALSE, lang = "en"))
  expect_equal(r80$pooled_pred$ci.level, 0.8)
  expect_equal(r95$pooled_pred$ci.level, 0.95)
})
