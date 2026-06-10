# === GRUPO B: sus_climate_compute_indicators ===

run_test("compute_indicators | all", {
  r <- sus_climate_compute_indicators(
    filled_df %||% climate_df,
    indicators = "all",
    verbose    = FALSE
  )
  stopifnot(
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "indicators",
    any(c("wbgt_c", "hi_c", "utci_c") %in% names(r)),
    nrow(r) > 0
  )
  r
})

run_test("compute_indicators | HI only", {
  r <- sus_climate_compute_indicators(
    climate_df,
    indicators = c("HI"),
    verbose    = FALSE
  )
  stopifnot(
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "indicators",
    "hi_c" %in% names(r),
    nrow(r) == nrow(climate_df)
  )
  r
})

run_test("compute_indicators | WBGT+HI", {
  r <- sus_climate_compute_indicators(
    climate_df,
    indicators = c("WBGT", "HI"),
    verbose    = FALSE
  )
  stopifnot(
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "indicators",
    all(c("wbgt_c", "hi_c") %in% names(r))
  )
  r
})
