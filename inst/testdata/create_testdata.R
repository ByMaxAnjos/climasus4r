# ============================================================
# create_testdata.R
# Generates sample Parquet files for all DATASUS systems
# in climasus4r.
#
# UF: Rondônia (RO) — small state, has all disease types
# Year: 2022 | Month: January (for monthly systems)
#
# Usage: source("inst/testdata/create_testdata.R")
#        Rscript inst/testdata/create_testdata.R
# ============================================================

if (!requireNamespace("climasus4r", quietly = TRUE)) {
  devtools::load_all(".")
} else {
  library(climasus4r)
}

BASE_DIR    <- file.path(getwd(), "inst", "testdata")
UF_DEFAULT  <- "RO"
YEAR        <- 2022L
MONTH       <- 1L

# Helper function
download_and_export <- function(system, uf, year, month = NULL,
                                subdir, filename, verbose = TRUE) {
  out_path <- file.path(BASE_DIR, subdir, filename)

  if (file.exists(out_path) && file.size(out_path) > 0) {
    if (verbose) cli::cli_alert_success("Cached: {filename}")
    return(invisible(out_path))
  }

  dir.create(file.path(BASE_DIR, subdir), recursive = TRUE, showWarnings = FALSE)

  period_str <- if (!is.null(month)) paste0(year, "/m", sprintf("%02d", month)) else as.character(year)
  if (verbose) cli::cli_alert_info("Downloading {system} ({uf}, {period_str})...")

  df <- tryCatch({
    raw <- sus_data_import(
      system    = system,
      uf        = uf,
      year      = year,
      month     = month,
      use_cache = TRUE,
      backend   = "tibble",   # collect immediately to data.frame
      verbose   = FALSE
    )
    # Ensure we have a plain data.frame (not Arrow lazy)
    if (inherits(raw, c("arrow_dplyr_query","Dataset","ArrowTabular","Table"))) {
      raw <- dplyr::collect(raw)
    }
    raw
  }, error = function(e) {
    cli::cli_alert_warning("FAILED {system} ({uf}): {conditionMessage(e)}")
    NULL
  })

  if (is.null(df) || nrow(df) == 0L) {
    cli::cli_alert_warning("Empty result: {system} ({uf} {year}) — skipping.")
    return(invisible(NULL))
  }

  # Export as plain data.frame (drop climasus_df S3 if needed)
  df_plain <- as.data.frame(df)
  .write_parquet_smart(df_plain, out_path)

  sz <- round(file.size(out_path) / 1024, 1)
  cli::cli_alert_success("{filename} — {nrow(df)} rows | {sz} KB")
  invisible(out_path)
}

# Run all systems via CLI argument or default to full run
args <- commandArgs(trailingOnly = TRUE)
group <- if (length(args) > 0) args[1] else "all"

# ── SIM (Mortality — annual) ─────────────────────────────────────────────────
if (group %in% c("all","sim")) {
  cli::cli_h2("SIM — Sistema de Informações sobre Mortalidade")
  for (s in list(
    list("SIM-DO",    "sim", "SIM_DO_RO_2022.parquet"),
    list("SIM-DOFET", "sim", "SIM_DOFET_RO_2022.parquet"),
    list("SIM-DOEXT", "sim", "SIM_DOEXT_RO_2022.parquet"),
    list("SIM-DOINF", "sim", "SIM_DOINF_RO_2022.parquet"),
    list("SIM-DOMAT", "sim", "SIM_DOMAT_RO_2022.parquet")
  )) download_and_export(s[[1]], UF_DEFAULT, YEAR, NULL, s[[2]], s[[3]])
}

# ── SINAN (Notifiable diseases — annual) ────────────────────────────────────
if (group %in% c("all","sinan")) {
  cli::cli_h2("SINAN — Sistema de Informação de Agravos de Notificação")
  for (s in list(
    list("SINAN-DENGUE",                  "sinan", "SINAN_DENGUE_RO_2022.parquet"),
    list("SINAN-CHIKUNGUNYA",             "sinan", "SINAN_CHIKUNGUNYA_RO_2022.parquet"),
    list("SINAN-ZIKA",                    "sinan", "SINAN_ZIKA_RO_2022.parquet"),
    list("SINAN-MALARIA",                 "sinan", "SINAN_MALARIA_RO_2022.parquet"),
    list("SINAN-CHAGAS",                  "sinan", "SINAN_CHAGAS_RO_2022.parquet"),
    list("SINAN-LEISHMANIOSE-VISCERAL",   "sinan", "SINAN_LV_RO_2022.parquet"),
    list("SINAN-LEISHMANIOSE-TEGUMENTAR", "sinan", "SINAN_LT_RO_2022.parquet"),
    list("SINAN-LEPTOSPIROSE",            "sinan", "SINAN_LEPTOSPIROSE_RO_2022.parquet")
  )) download_and_export(s[[1]], UF_DEFAULT, YEAR, NULL, s[[2]], s[[3]])
}

# ── SINASC (Live births — annual) ────────────────────────────────────────────
if (group %in% c("all","sinasc")) {
  cli::cli_h2("SINASC — Sistema de Informações sobre Nascidos Vivos")
  download_and_export("SINASC", UF_DEFAULT, YEAR, NULL, "sinasc", "SINASC_RO_2022.parquet")
}

# ── SIH (Hospitalizations — monthly) ─────────────────────────────────────────
if (group %in% c("all","sih")) {
  cli::cli_h2("SIH — Sistema de Informações Hospitalares")
  for (s in list(
    list("SIH-RD", UF_DEFAULT, "SIH_RD_RO_2022_01.parquet"),
    list("SIH-RJ", "RJ",       "SIH_RJ_RJ_2022_01.parquet"),
    list("SIH-SP", "SP",       "SIH_SP_SP_2022_01.parquet"),
    list("SIH-ER", UF_DEFAULT, "SIH_ER_RO_2022_01.parquet")
  )) download_and_export(s[[1]], s[[2]], YEAR, MONTH, "sih", s[[3]])
}

# ── SIA (Outpatient — monthly) ────────────────────────────────────────────────
if (group %in% c("all","sia")) {
  cli::cli_h2("SIA — Sistema de Informações Ambulatoriais")
  for (sys in c("SIA-AB","SIA-ABO","SIA-ACF","SIA-AD","SIA-AN",
                "SIA-AM","SIA-AQ","SIA-AR","SIA-ATD","SIA-PA","SIA-PS","SIA-SAD")) {
    fn <- paste0(gsub("-","_",sys), "_RO_2022_01.parquet")
    download_and_export(sys, UF_DEFAULT, YEAR, MONTH, "sia", fn)
  }
}

# ── CNES (Health establishments — monthly) ────────────────────────────────────
if (group %in% c("all","cnes")) {
  cli::cli_h2("CNES — Cadastro Nacional de Estabelecimentos de Saúde")
  for (sys in c("CNES-LT","CNES-ST","CNES-DC","CNES-EQ","CNES-SR",
                "CNES-HB","CNES-PF","CNES-EP","CNES-RC","CNES-IN",
                "CNES-EE","CNES-EF","CNES-GM")) {
    fn <- paste0(gsub("-","_",sys), "_RO_2022_01.parquet")
    download_and_export(sys, UF_DEFAULT, YEAR, MONTH, "cnes", fn)
  }
}

cli::cli_h1("Test database summary")
files <- list.files(BASE_DIR, recursive = TRUE, pattern = "\\.parquet$", full.names = TRUE)
cli::cli_alert_success("Total Parquet files: {length(files)}")
for (f in sort(files)) {
  sz <- round(file.size(f) / 1024, 1)
  cli::cli_alert_info("{basename(dirname(f))}/{basename(f)} — {sz} KB")
}
