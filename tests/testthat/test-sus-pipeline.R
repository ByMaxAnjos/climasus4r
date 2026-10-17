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

# --- sus_data_clean_encoding ---------------------------------------------

test_that("sus_data_clean_encoding repairs latin1 bytes and sets stage", {
  bad <- "S\xe3o Paulo"
  Encoding(bad) <- "bytes"
  df <- data.frame(city = c(bad, "Natal"), n = 1:2, stringsAsFactors = FALSE)

  out <- sus_data_clean_encoding(df, backend = "tibble", lang = "en",
                                 verbose = FALSE)

  expect_s3_class(out, "climasus_df")
  expect_identical(out$city, c("São Paulo", "Natal"))
  expect_true(all(stringi::stri_enc_isutf8(out$city)))
  expect_identical(out$n, 1:2)
  expect_identical(sus_meta(out, "stage"), "clean")
})

test_that("sus_data_clean_encoding leaves valid UTF-8 untouched", {
  df <- data.frame(city = c("São Paulo", "Belém"),
                   stringsAsFactors = FALSE)
  out <- sus_data_clean_encoding(df, backend = "tibble", lang = "en",
                                 verbose = FALSE)
  expect_identical(out$city, df$city)
})

# --- sus_data_standardize ------------------------------------------------

make_raw_sim <- function() {
  raw <- data.frame(
    CAUSABAS  = c("I219", "J189"),
    DTOBITO   = c("01012020", "02012020"),
    SEXO      = c("1", "2"),
    CODMUNRES = "230440",
    stringsAsFactors = FALSE
  )
  new_climasus_df(raw, meta = list(
    system = "SIM-DO", stage = "clean", type = "clean", backend = "tibble"
  ))
}

test_that("sus_data_standardize translates columns, parses dates and codes", {
  out <- sus_data_standardize(make_raw_sim(), lang = "en", backend = "tibble",
                              verbose = FALSE)

  expect_s3_class(out, "climasus_df")
  expect_true(all(c("underlying_cause", "death_date", "sex") %in% names(out)))
  expect_s3_class(out$death_date, "Date")
  expect_identical(out$death_date, as.Date(c("2020-01-01", "2020-01-02")))
  expect_identical(as.character(out$sex), c("Male", "Female"))
  expect_identical(sus_meta(out, "stage"), "stand")
})

test_that("sus_data_standardize output feeds sus_data_filter_cid", {
  out <- make_raw_sim() |>
    sus_data_standardize(lang = "en", backend = "tibble", verbose = FALSE) |>
    sus_data_filter_cid(icd_codes = "I", backend = "tibble", lang = "en",
                        verbose = FALSE)

  expect_equal(nrow(out), 1)
  expect_identical(out$underlying_cause, "I219")
})

# --- sus_data_create_variables -------------------------------------------

make_demo <- function() {
  df <- data.frame(
    date_death = as.Date(c("2020-01-04", "2020-07-15")),
    age_years  = c(3, 70),
    sex        = c("Male", "Female"),
    stringsAsFactors = FALSE
  )
  new_climasus_df(df, meta = list(
    system = "SIM", stage = "filter_demo", type = "filter_demo",
    backend = "tibble"
  ))
}

test_that("sus_data_create_variables derives age groups", {
  out <- sus_data_create_variables(make_demo(), create_climate_vars = FALSE,
                                   backend = "tibble", lang = "en",
                                   verbose = FALSE)

  expect_s3_class(out$age_group, "factor")
  expect_identical(as.character(out$age_group), c("0-4", "60+"))
  expect_identical(sus_meta(out, "stage"), "derive")
})

test_that("sus_data_create_variables derives calendar variables", {
  out <- sus_data_create_variables(make_demo(), create_climate_vars = FALSE,
                                   backend = "tibble", lang = "en",
                                   verbose = FALSE)

  # 2020-01-04 is a Saturday; 2020-07-15 is a Wednesday
  expect_equal(out$year, c(2020, 2020))
  expect_equal(out$month, c(1, 7))
  expect_equal(out$quarter, c(1L, 3L))
  expect_equal(out$semester, c(1, 2))
  expect_equal(out$day_of_year, c(4, 197))
  expect_identical(out$is_weekend, c(TRUE, FALSE))
  expect_identical(out$day_of_week_name, c("Saturday", "Wednesday"))
})

test_that("sus_data_create_variables honours custom age breaks", {
  out <- sus_data_create_variables(make_demo(), age_breaks = c(0, 65, Inf),
                                   age_labels = c("<65", "65+"),
                                   create_calendar_vars = FALSE,
                                   create_climate_vars = FALSE,
                                   backend = "tibble", lang = "en",
                                   verbose = FALSE)

  expect_identical(as.character(out$age_group), c("<65", "65+"))
})
