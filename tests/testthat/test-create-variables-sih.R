# Regression: SIH age must come from dates when valid, and from `idade` +
# `codigo_unidade_idade` when birth dates are missing/invalid (used to abort).
mk_sih <- function() {
  df <- tibble::tibble(
    data_nascimento = as.Date(c("1990-05-01", "2020-01-10", NA, "1950-12-31")),
    data_internacao = as.Date(c("2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01")),
    idade = c(30L, 5L, 11L, 69L),
    codigo_unidade_idade = c("4", "3", "2", "4")
  )
  new_climasus_df(df, list(system = "SIH-RD", stage = "stand", type = "stand", backend = "tibble"))
}

test_that("SIH age uses dates, falling back per row to idade + unit", {
  r <- sus_data_create_variables(mk_sih(), backend = "tibble", verbose = FALSE)
  expect_equal(r$age_years, c(30, 0, 0, 69))   # row 3: 11 days -> 0 years
})

test_that("SIH without birth-date column decodes idade + unit instead of failing", {
  d <- mk_sih(); d$data_nascimento <- NULL
  r <- sus_data_create_variables(d, backend = "tibble", verbose = FALSE)
  expect_equal(r$age_years, c(30, 0, 0, 69))
})

test_that("age_group is created once with readable labels", {
  r <- sus_data_create_variables(mk_sih(), backend = "tibble", verbose = FALSE)
  expect_equal(sum(names(r) == "age_group"), 1L)
  expect_false(any(grepl("^\\[", levels(r$age_group))))
})
