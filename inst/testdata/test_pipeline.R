# test_pipeline.R
# Tests the full climasus4r pipeline with DATASUS sample data.
# Usage: Rscript inst/testdata/test_pipeline.R

devtools::load_all(".", quiet = TRUE)
BASE <- file.path(getwd(), "inst", "testdata")

ok <- 0L; fail <- 0L
run_test <- function(name, code) {
  tryCatch({
    r <- code
    n <- if (inherits(r, "ggplot")) "ggplot" else nrow(r)
    cli::cli_alert_success("{name}: OK ({n} rows)")
    ok <<- ok + 1L
  }, error = function(e) {
    cli::cli_alert_danger("{name}: FAILED — {conditionMessage(e)}")
    fail <<- fail + 1L
  })
}

# ── SIM-DO (mortalidade) ─────────────────────────────────────────────────────
cli::cli_h2("SIM-DO")
f <- file.path(BASE, "sim", "SIM_DO_RO_2022.parquet")
if (file.exists(f)) {
  df <- .read_parquet_smart(f) |> new_climasus_df(list(stage="import",system="SIM"))
  run_test("SIM-DO: clean",     sus_data_clean_encoding(df, verbose=FALSE))
  run_test("SIM-DO: stand",     df |> sus_data_clean_encoding(verbose=FALSE) |> sus_data_standardize(verbose=FALSE))
  run_test("SIM-DO: aggregate", df |> sus_data_clean_encoding(verbose=FALSE) |> sus_data_standardize(verbose=FALSE) |> (\(d) sus_meta(d, backend="tibble"))() |> sus_data_aggregate(time_unit="month", backend="tibble", verbose=FALSE))
}

# ── SINAN-DENGUE ─────────────────────────────────────────────────────────────
cli::cli_h2("SINAN-DENGUE")
f <- file.path(BASE, "sinan", "SINAN_DENGUE_RO_2022.parquet")
if (file.exists(f)) {
  df <- .read_parquet_smart(f) |> new_climasus_df(list(stage="import",system="SINAN"))
  run_test("DENGUE: clean",     sus_data_clean_encoding(df, verbose=FALSE))
  run_test("DENGUE: stand",     df |> sus_data_clean_encoding(verbose=FALSE) |> sus_data_standardize(verbose=FALSE))
  run_test("DENGUE: aggregate", df |> sus_data_clean_encoding(verbose=FALSE) |> sus_data_standardize(verbose=FALSE) |> (\(d) sus_meta(d, backend="tibble"))() |> sus_data_aggregate(time_unit="week", backend="tibble", verbose=FALSE))
}

# ── SIH-RD (internacoes) ─────────────────────────────────────────────────────
cli::cli_h2("SIH-RD")
f <- file.path(BASE, "sih", "SIH_RD_RO_2022_01.parquet")
if (file.exists(f)) {
  df <- .read_parquet_smart(f) |> new_climasus_df(list(stage="import",system="SIH"))
  run_test("SIH-RD: clean",     sus_data_clean_encoding(df, verbose=FALSE))
  run_test("SIH-RD: aggregate", df |> sus_data_clean_encoding(verbose=FALSE) |> sus_data_standardize(verbose=FALSE) |> (\(d) sus_meta(d, backend="tibble"))() |> sus_data_aggregate(time_unit="month", backend="tibble", verbose=FALSE))
}

# ── SINASC (nascidos vivos) ───────────────────────────────────────────────────
cli::cli_h2("SINASC")
f <- file.path(BASE, "sinasc", "SINASC_RO_2022.parquet")
if (file.exists(f)) {
  df <- .read_parquet_smart(f) |> new_climasus_df(list(stage="import",system="SINASC"))
  run_test("SINASC: clean",     sus_data_clean_encoding(df, verbose=FALSE))
  run_test("SINASC: aggregate", df |> sus_data_clean_encoding(verbose=FALSE) |> sus_data_standardize(verbose=FALSE) |> (\(d) sus_meta(d, backend="tibble"))() |> sus_data_aggregate(time_unit="month", backend="tibble", verbose=FALSE))
}

# ── SIA-PA (procedimentos ambulatoriais) ─────────────────────────────────────
cli::cli_h2("SIA-PA")
f <- file.path(BASE, "sia", "SIA_PA_RO_2022_01.parquet")
if (file.exists(f)) {
  df <- .read_parquet_smart(f) |> new_climasus_df(list(stage="import",system="SIA"))
  run_test("SIA-PA: clean",     sus_data_clean_encoding(df, verbose=FALSE))
  run_test("SIA-PA: aggregate", df |> sus_data_clean_encoding(verbose=FALSE) |> sus_data_standardize(verbose=FALSE) |> (\(d) sus_meta(d, backend="tibble"))() |> sus_data_aggregate(time_unit="month", backend="tibble", verbose=FALSE))
}

# ── CNES-ST (profissionais) ───────────────────────────────────────────────────
cli::cli_h2("CNES-ST")
f <- file.path(BASE, "cnes", "CNES_ST_RO_2022_01.parquet")
if (file.exists(f)) {
  df <- .read_parquet_smart(f) |> new_climasus_df(list(stage="import",system="CNES"))
  run_test("CNES-ST: clean",    sus_data_clean_encoding(df, verbose=FALSE))
  run_test("CNES-ST: aggregate",df |> sus_data_clean_encoding(verbose=FALSE) |> sus_data_standardize(verbose=FALSE) |> (\(d) sus_meta(d, backend="tibble"))() |> sus_data_aggregate(time_unit="month", backend="tibble", verbose=FALSE))
}

cli::cli_h1("Results: {ok} passed, {fail} failed out of {ok+fail} tests")

# ── SINAN-MALARIA (national database) ────────────────────────────────────────
cli::cli_h2("SINAN-MALARIA (nacional)")
f <- file.path(BASE, "sinan", "SINAN_MALARIA_BR_2022.parquet")
if (file.exists(f)) {
  df <- .read_parquet_smart(f) |> new_climasus_df(list(stage="import",system="SINAN"))
  run_test("MALARIA: clean", sus_data_clean_encoding(df, verbose=FALSE))
}
