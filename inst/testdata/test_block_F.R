# === GRUPO F: sus_climate_compute_spi + sus_climate_compute_spei ===

monthly_path <- file.path(BASE, "climate_ro_2022_monthly.parquet")
if (file.exists(monthly_path)) {
  monthly_df <- climasus4r:::from_arrow_climasus(monthly_path)

  run_test("compute_spi | 1-3-6mo", {
    r <- sus_climate_compute_spi(
      monthly_df,
      var     = "rainfall_mm",
      scales  = c(1L, 3L, 6L),
      lang    = "pt",
      verbose = FALSE
    )
    stopifnot(
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "spi",
      all(c("spi_1mo", "spi_3mo", "spi_6mo") %in% names(r)),
      nrow(r) > 0
    )
    r
  })

  run_test("compute_spi | 12mo single scale", {
    r <- sus_climate_compute_spi(
      monthly_df,
      var     = "rainfall_mm",
      scales  = 12L,
      lang    = "pt",
      verbose = FALSE
    )
    stopifnot(
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "spi",
      "spi_12mo" %in% names(r),
      !"spi_1mo" %in% names(r),
      nrow(r) > 0
    )
    r
  })

  run_test("compute_spei | thornthwaite 1-3mo", {
    r <- sus_climate_compute_spei(
      monthly_df,
      rain_var   = "rainfall_mm",
      pet_method = "thornthwaite",
      scales     = c(1L, 3L),
      verbose    = FALSE
    )
    stopifnot(
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "spei",
      all(c("spei_1mo", "spei_3mo") %in% names(r)),
      nrow(r) > 0
    )
    r
  })

} else {
  cli::cli_alert_warning("GRUPO F: fixture mensal ausente — skip")
}
