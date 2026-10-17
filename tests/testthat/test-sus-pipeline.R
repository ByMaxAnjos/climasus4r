# Tests for core pipeline functions (backend = "tibble", fully offline)

make_stand <- function(stage = "stand") {
  df <- data.frame(
    underlying_cause = c("I219", "J189", "I10", "A090", "I219", "C509"),
    date_death = as.Date(c("2020-01-01", "2020-01-01", "2020-01-02",
                           "2020-01-03", "2020-01-03", "2020-01-03")),
    sex = c("Male", "Female", "Male", "Female", "Male", "Female"),
    age_years = c(70, 3, 55, 1, 80, 60),
    code_muni = "2304400",
    stringsAsFactors = FALSE
  )
  new_climasus_df(df, meta = list(
    system = "SIM", stage = stage, type = stage, backend = "tibble"
  ))
}

test_that("sus_data_filter_cid keeps matching ICD codes and advances stage", {
  out <- sus_data_filter_cid(make_stand(), icd_codes = "I",
                             backend = "tibble", lang = "en", verbose = FALSE)

  expect_s3_class(out, "climasus_df")
  expect_equal(nrow(out), 3)
  expect_true(all(startsWith(out$underlying_cause, "I")))
  expect_identical(sus_meta(out, "stage"), "filter_cid")
})

test_that("sus_data_filter_cid rejects data that is not standardized yet", {
  expect_error(
    sus_data_filter_cid(make_stand("import"), icd_codes = "I",
                        backend = "tibble", lang = "en", verbose = FALSE),
    "standardized"
  )
})

test_that("sus_data_filter_demographics filters by sex and age range", {
  by_sex <- sus_data_filter_demographics(make_stand(), sex = "Male",
                                         backend = "tibble", lang = "en",
                                         verbose = FALSE)
  expect_equal(nrow(by_sex), 3)
  expect_true(all(by_sex$sex == "Male"))
  expect_identical(sus_meta(by_sex, "stage"), "filter_demo")

  by_age <- sus_data_filter_demographics(make_stand(), age_range = c(50, 100),
                                         backend = "tibble", lang = "en",
                                         verbose = FALSE)
  expect_true(all(by_age$age_years >= 50))
})

test_that("sus_data_aggregate counts records per day", {
  out <- sus_data_aggregate(make_stand(), time_unit = "day",
                            backend = "tibble", lang = "en", verbose = FALSE)

  expect_equal(nrow(out), 3)
  expect_equal(out$n_deaths, c(2L, 1L, 3L))
  expect_equal(sum(out$n_deaths), 6L)
  expect_identical(sus_meta(out, "stage"), "aggregate")
})

test_that("sus_data_aggregate validates fun and value_col", {
  expect_error(
    sus_data_aggregate(make_stand(), fun = "bogus", backend = "tibble",
                       lang = "en", verbose = FALSE),
    "fun must be one of"
  )
  expect_error(
    sus_data_aggregate(make_stand(), fun = "mean", backend = "tibble",
                       lang = "en", verbose = FALSE),
    "value_col must be specified"
  )
})

test_that("sus_data_aggregate computes value_col statistics", {
  out <- sus_data_aggregate(make_stand(), time_unit = "day", fun = "mean",
                            value_col = "age_years", backend = "tibble",
                            lang = "en", verbose = FALSE)
  expect_equal(nrow(out), 3)
  expect_equal(out[[2]][1], mean(c(70, 3)))
})
