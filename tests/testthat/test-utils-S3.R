# Tests for the climasus_df S3 class system (R/utils-S3.R)

make_df <- function() {
  data.frame(code_muni = c(1, 2), value = c(10, 20))
}

test_that("new_climasus_df sets class and default metadata", {
  obj <- new_climasus_df(make_df())

  expect_s3_class(obj, "climasus_df")
  expect_s3_class(obj, "tbl_df")

  meta <- attr(obj, "sus_meta")
  expect_type(meta, "list")
  expect_null(meta$system)
  expect_null(meta$stage)
  expect_identical(meta$backend, "tibble")
  expect_false(meta$spatial)
  expect_identical(meta$history, character(0))
})

test_that("new_climasus_df detects spatial from sf class", {
  df <- make_df()
  class(df) <- c("sf", class(df))
  obj <- new_climasus_df(df)

  expect_true(attr(obj, "sus_meta")$spatial)
})

test_that("new_climasus_df rejects non-data.frame input", {
  expect_error(new_climasus_df(1:10), "is.data.frame")
})

test_that("validate_climasus_df accepts valid metadata", {
  obj <- new_climasus_df(make_df(), meta = list(
    system = "SIM", stage = "import", type = "raw", backend = "tibble"
  ))
  expect_identical(validate_climasus_df(obj), obj)
})

test_that("validate_climasus_df accepts SIH sub-system prefixes", {
  obj <- new_climasus_df(make_df(), meta = list(system = "SIH-RD"))
  expect_silent(validate_climasus_df(obj))
})

test_that("validate_climasus_df rejects invalid system/stage/type/backend", {
  bad_system <- new_climasus_df(make_df(), meta = list(system = "NOTREAL"))
  expect_error(validate_climasus_df(bad_system), "Invalid system")

  bad_stage <- new_climasus_df(make_df(), meta = list(stage = "not_a_stage"))
  expect_error(validate_climasus_df(bad_stage), "Invalid stage")

  bad_type <- new_climasus_df(make_df(), meta = list(type = "not_a_type"))
  expect_error(validate_climasus_df(bad_type), "Invalid type")

  bad_backend <- new_climasus_df(make_df(), meta = list(backend = "not_a_backend"))
  expect_error(validate_climasus_df(bad_backend), "Invalid backend")
})

test_that("validate_climasus_df warns on spatial flag / geometry mismatch", {
  obj <- new_climasus_df(make_df(), meta = list(spatial = TRUE))
  expect_warning(validate_climasus_df(obj), "no geometry column")
})

test_that("is_climasus_df distinguishes climasus_df from plain data.frame", {
  expect_true(is_climasus_df(new_climasus_df(make_df())))
  expect_false(is_climasus_df(make_df()))
})

test_that("is_stage_at_least respects stage ordering", {
  expect_true(is_stage_at_least("aggregate", "clean"))
  expect_false(is_stage_at_least("clean", "aggregate"))
  expect_false(is_stage_at_least(NULL, "clean"))
})

test_that("detect_backend_type identifies dataframe and unsupported objects", {
  expect_identical(detect_backend_type(make_df()), "dataframe")
  expect_identical(detect_backend_type(new_climasus_df(make_df())), "dataframe")
  expect_identical(detect_backend_type(1:5), "unsupported")
})

test_that("ensure_climasus_df wraps plain data.frame and passes through climasus_df", {
  wrapped <- ensure_climasus_df(make_df(), system = "SIM", stage = "import", type = "raw")
  expect_true(is_climasus_df(wrapped))
  expect_identical(sus_meta(wrapped, "system"), "SIM")

  already <- ensure_climasus_df(wrapped, system = "SINAN")
  expect_identical(sus_meta(already, "system"), "SIM")  # unchanged, passthrough
})

test_that("ensure_climasus_df rejects non data.frame input", {
  expect_error(ensure_climasus_df(1:5), "must be a data.frame")
})

# --- sus_meta() unified interface -------------------------------------------

test_that("sus_meta reads a single field and full metadata list", {
  obj <- ensure_climasus_df(make_df(), system = "SIM", stage = "import", type = "raw")

  expect_identical(sus_meta(obj, "system"), "SIM")
  expect_identical(sus_meta(obj, "stage"), "import")

  full <- sus_meta(obj)
  expect_type(full, "list")
  expect_identical(full$system, "SIM")
})

test_that("sus_meta updates fields and records history", {
  obj <- ensure_climasus_df(make_df(), system = "SIM", stage = "import", type = "raw")

  updated <- sus_meta(obj, stage = "clean", type = "clean", add_history = "cleaned encoding")

  expect_identical(sus_meta(updated, "stage"), "clean")
  expect_identical(sus_meta(updated, "type"), "clean")
  expect_length(sus_meta(updated, "history"), 1)
  expect_match(sus_meta(updated, "history"), "cleaned encoding")
})

test_that("sus_meta rejects invalid field values on update", {
  obj <- ensure_climasus_df(make_df(), system = "SIM")
  expect_error(sus_meta(obj, stage = "not_a_stage"), "Invalid stage")
})

test_that("sus_meta(valid_values = ...) returns controlled vocabularies", {
  expect_identical(sus_meta(valid_values = "system"), .climasus_systems)
  expect_identical(sus_meta(valid_values = "stage"), .climasus_stages)
  expect_identical(sus_meta(valid_values = "backend"), .climasus_backends)
})

test_that("sus_meta invalid field name errors", {
  obj <- ensure_climasus_df(make_df())
  expect_error(sus_meta(obj, "not_a_field"), "Invalid field")
})

# --- S3 methods ---------------------------------------------------------------

test_that("subsetting a climasus_df preserves class and metadata", {
  obj <- ensure_climasus_df(make_df(), system = "SIM", stage = "import")
  sub <- obj[1, ]

  expect_s3_class(sub, "climasus_df")
  expect_identical(sus_meta(sub, "system"), "SIM")
})

test_that("$<- assignment preserves climasus_df class", {
  obj <- ensure_climasus_df(make_df(), system = "SIM")
  obj$new_col <- 1:2

  expect_s3_class(obj, "climasus_df")
  expect_true("new_col" %in% names(obj))
})

test_that("rbind.climasus_df combines rows and preserves metadata", {
  a <- ensure_climasus_df(make_df(), system = "SIM", stage = "import")
  b <- ensure_climasus_df(make_df(), system = "SIM", stage = "import")

  combined <- rbind(a, b)

  expect_s3_class(combined, "climasus_df")
  expect_identical(nrow(combined), 4L)
  expect_identical(sus_meta(combined, "system"), "SIM")
})

test_that("rbind.climasus_df warns on metadata conflicts between inputs", {
  a <- ensure_climasus_df(make_df(), system = "SIM")
  b <- ensure_climasus_df(make_df(), system = "SINAN")

  expect_warning(rbind(a, b), "Metadata conflicts")
})

test_that("as.data.frame.climasus_df strips class and sus_meta attribute", {
  obj <- ensure_climasus_df(make_df(), system = "SIM")
  plain <- as.data.frame(obj)

  expect_false(is_climasus_df(plain))
  expect_null(attr(plain, "sus_meta"))
})

# --- metadata JSON round trip (backend-agnostic serialisation) --------------

test_that("meta_to_json / meta_from_json round-trip scalar fields", {
  meta <- list(system = "SIM", stage = "import", type = "raw", spatial = FALSE)
  json <- .meta_to_json(meta)
  back <- .meta_from_json(json)

  expect_identical(back$system, "SIM")
  expect_identical(back$stage, "import")
  expect_identical(back$type, "raw")
})

test_that("meta_from_json returns empty list for NULL/empty input", {
  expect_identical(.meta_from_json(NULL), list())
  expect_identical(.meta_from_json(""), list())
})
