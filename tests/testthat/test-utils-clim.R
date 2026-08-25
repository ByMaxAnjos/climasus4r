# Tests for pure helper functions in R/utils-clim.R

test_that(".expand_year_range handles NULL, single year, and vectors", {
  expect_null(.expand_year_range(NULL))
  expect_identical(.expand_year_range(2020), 2020L)
  expect_identical(.expand_year_range(c(2022, 2020, 2021)), c(2020L, 2021L, 2022L))
  expect_identical(.expand_year_range(c(2020, 2020, 2021)), c(2020L, 2021L))
})

test_that(".calculate_dew_point matches known Magnus-formula values", {
  # At 100% RH, dew point equals air temperature
  expect_equal(.calculate_dew_point(20, 100), 20, tolerance = 1e-6)

  # Lower humidity gives a lower dew point than air temperature
  dp <- .calculate_dew_point(25, 50)
  expect_lt(dp, 25)
  expect_gt(dp, 0)
})

test_that(".calculate_dew_point is vectorised", {
  result <- .calculate_dew_point(c(20, 25, 30), c(100, 80, 60))
  expect_length(result, 3)
  expect_true(all(is.finite(result)))
})

# --- Exact-date climate join --------------------------------------------------

test_that(".join_exact merges climate variables on date and code_muni", {
  health <- tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-01-02")),
    code_muni = c(1, 1)
  )
  climate <- tibble::tibble(
    date = as.Date(c("2020-01-01", "2020-01-02")),
    code_muni = c(1, 1),
    rainfall_mm = c(5, 0),
    patm_mb = c(1000, 1005),
    patm_max_mb = c(1001, 1006),
    patm_min_mb = c(999, 1004),
    sr_kj_m2 = c(10, 12),
    tair_dry_bulb_c = c(25, 26),
    dew_tmean_c = c(20, 21),
    tair_max_c = c(30, 31),
    tair_min_c = c(20, 21),
    dew_tmax_c = c(22, 23),
    dew_tmin_c = c(18, 19),
    rh_max_porc = c(90, 85),
    rh_min_porc = c(60, 55),
    rh_mean_porc = c(75, 70),
    wd_degrees = c(180, 190),
    ws_gust_m_s = c(5, 6),
    ws_2_m_s = c(2, 3)
  )

  result <- .join_exact(health, climate)

  expect_identical(nrow(result), 2L)
  expect_true("rainfall_mm" %in% names(result))
  expect_identical(result$rainfall_mm, c(5, 0))
})

test_that(".join_exact respects an explicit target_vars subset", {
  health <- tibble::tibble(date = as.Date("2020-01-01"), code_muni = 1)
  climate <- tibble::tibble(
    date = as.Date("2020-01-01"), code_muni = 1,
    rainfall_mm = 5, tair_dry_bulb_c = 25
  )

  result <- .join_exact(health, climate, target_vars = "rainfall_mm")

  expect_true("rainfall_mm" %in% names(result))
  expect_false("tair_dry_bulb_c" %in% names(result))
})
