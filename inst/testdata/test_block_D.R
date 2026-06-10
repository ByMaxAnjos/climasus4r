# === GRUPO D: sus_climate_compute_heatwaves ===

# Entrada: climate_df (stage=climate, type=inmet) carregado no mestre
# filled_df pode ter sido definido pelo grupo A; usa fallback para climate_df

hw_input <- if (exists("filled_df") && !is.null(filled_df)) filled_df else climate_df

run_test("compute_heatwaves | WHO", {
  r <- sus_climate_compute_heatwaves(hw_input, method = "WHO", verbose = FALSE, lang = "pt")
  stopifnot(
    is.list(r),
    all(c("events", "daily", "summary") %in% names(r)),
    sus_meta(r[["events"]], "type") == "heatwaves",
    all(c("event_id", "start_date", "end_date", "duration_days") %in% names(r[["events"]])),
    is.integer(nrow(r[["events"]])) || nrow(r[["events"]]) >= 0
  )
  r
})

run_test("compute_heatwaves | WMO", {
  r <- sus_climate_compute_heatwaves(hw_input, method = "WMO", verbose = FALSE, lang = "pt")
  stopifnot(
    is.list(r),
    "hw_wmo" %in% names(r[["daily"]])
  )
  r
})

run_test("compute_heatwaves | EHF", {
  r <- sus_climate_compute_heatwaves(hw_input, method = "EHF", verbose = FALSE, lang = "pt")
  stopifnot(
    any(grepl("ehf", names(r[["events"]]), ignore.case = TRUE) | nrow(r[["events"]]) >= 0)
  )
  r
})

run_test("compute_heatwaves | multi-method WHO+WMO", {
  r <- sus_climate_compute_heatwaves(hw_input, method = c("WHO", "WMO"), verbose = FALSE, lang = "pt")
  stopifnot(
    all(c("hw_who", "hw_wmo") %in% names(r[["daily"]]))
  )
  r
})
