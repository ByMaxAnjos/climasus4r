test_that("sus_data_export infers parquet for non-spatial .parquet", {
  skip_if_not_installed("arrow")
  f <- tempfile(fileext = ".parquet")
  expect_no_error(sus_data_export(data.frame(a = 1:3), f, verbose = FALSE))
  expect_true(file.exists(f))
})

test_that("sus_data_read errors clearly for directory without format", {
  d <- tempfile(); dir.create(d)
  expect_error(sus_data_read(d, verbose = FALSE), "format")
})

test_that("sus_cache_info/clear are recursive and stay inside cache_dir", {
  d <- tempfile(); dir.create(file.path(d, "data", "raw"), recursive = TRUE)
  f <- file.path(d, "data", "raw", "x.parquet"); file.create(f)
  outside <- tempfile(fileext = ".csv"); file.create(outside)
  expect_equal(sus_cache_info(d, verbose = FALSE)$file_count, 1)
  expect_equal(sus_cache_clear(d, verbose = FALSE), 1)
  expect_false(file.exists(f))
  expect_true(file.exists(outside))
})

test_that(".onAttach is defined once", {
  expect_false(any(grepl("enable_arrow", deparse(climasus4r:::.onAttach))))
})
