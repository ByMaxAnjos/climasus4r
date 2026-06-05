#!/usr/bin/env Rscript
# ============================================================
# test_all_pipeline.R
# Comprehensive pipeline test: all sus_data_* functions
# against every DATASUS system in inst/testdata/
#
# Usage:
#   Rscript inst/testdata/test_all_pipeline.R
#   Rscript inst/testdata/test_all_pipeline.R SIM
#   Rscript inst/testdata/test_all_pipeline.R SIH
#   source("inst/testdata/test_all_pipeline.R")
#
# Error classification:
#   Type A — expected incompatibility (system does not have
#             the required columns for that function); skipped.
#   Type B — unexpected error (bug in the package code); recorded as FAIL.
# ============================================================

suppressPackageStartupMessages({
  if (!requireNamespace("climasus4r", quietly = TRUE)) {
    devtools::load_all(".", quiet = TRUE)
  } else {
    library(climasus4r)
  }
})

BASE  <- file.path(getwd(), "inst", "testdata")
ARGS  <- commandArgs(trailingOnly = TRUE)
GROUP <- if (length(ARGS) > 0) toupper(ARGS[1]) else "ALL"

# ── Test scaffolding ──────────────────────────────────────────────────────────
pass <- 0L; fail <- 0L; skip_n <- 0L
results <- list()

rt <- function(name, sys, stage_label, expr) {
  r <- tryCatch(expr, error = function(e) {
    msg <- conditionMessage(e)
    cli::cli_alert_danger("{name}: FAIL -- {msg}")
    results[[length(results) + 1L]] <<- list(
      name = name, sys = sys, stage = stage_label,
      result = "FAIL", rows = NA_integer_, error = msg
    )
    fail <<- fail + 1L
    invisible(NULL)
  })
  if (!is.null(r)) {
    n <- tryCatch({
      nr <- nrow(r)
      if (is.null(nr)) NA_integer_ else as.integer(nr)
    }, error = function(e) NA_integer_)
    cli::cli_alert_success("{name}: OK ({n} rows)")
    results[[length(results) + 1L]] <<- list(
      name = name, sys = sys, stage = stage_label,
      result = "PASS", rows = n, error = NA_character_
    )
    pass <<- pass + 1L
  }
  invisible(r)
}

rs <- function(name, sys, reason) {
  cli::cli_alert_info("{name}: SKIP (Type A -- {reason})")
  results[[length(results) + 1L]] <<- list(
    name = name, sys = sys, stage = "skip",
    result = "SKIP", rows = NA_integer_, error = reason
  )
  skip_n <<- skip_n + 1L
}

lp <- function(path) {
  .read_parquet_smart(file.path(BASE, path))
}

mk <- function(df, system) {
  new_climasus_df(df, list(
    stage   = "import",
    system  = system,
    type    = "individual",
    backend = "tibble"
  ))
}

# ── SIM ───────────────────────────────────────────────────────────────────────
if (GROUP %in% c("ALL", "SIM")) {
  cli::cli_h1("SIM -- Sistema de Informacoes sobre Mortalidade")

  df0  <- mk(lp("sim/SIM_DO_RO_2022.parquet"), "SIM")
  df1  <- rt("SIM clean",       "SIM", "clean",
             sus_data_clean_encoding(df0, backend = "tibble", verbose = FALSE))
  df2  <- rt("SIM stand",       "SIM", "stand",
             sus_data_standardize(df1, backend = "tibble", verbose = FALSE))
  df3  <- rt("SIM filter_cid",  "SIM", "filter_cid",
             sus_data_filter_cid(df2, disease_group = "cardiovascular",
                                 verbose = FALSE))
  df4  <- rt("SIM create_vars", "SIM", "derive",
             sus_data_create_variables(df2, verbose = FALSE))
  df4b <- if (!is.null(df4)) sus_meta(df4, backend = "tibble") else NULL
  df5  <- rt("SIM filter_demo", "SIM", "filter_demo",
             sus_data_filter_demographics(
               if (!is.null(df4)) df4 else df2, sex = "M", verbose = FALSE))
  df6  <- rt("SIM aggregate",   "SIM", "aggregate",
             sus_data_aggregate(
               if (!is.null(df4b)) df4b else sus_meta(df2, backend = "tibble"),
               time_unit = "month", backend = "tibble", verbose = FALSE))
  rt("SIM quality",  "SIM", "quality",
     sus_data_quality_report(df2, output_format = "console", verbose = FALSE))
  if (!is.null(df6))
    rt("SIM export", "SIM", "export",
       sus_data_export(df6,
                       file.path(tempdir(), "sim_test.parquet"),
                       format = "parquet", overwrite = TRUE, verbose = FALSE))
}

# ── SIH-RD ───────────────────────────────────────────────────────────────────
if (GROUP %in% c("ALL", "SIH")) {
  cli::cli_h1("SIH-RD -- Internacoes (Rondonia, jan/2022)")

  df0  <- mk(lp("sih/SIH_RD_RO_2022_01.parquet"), "SIH")
  df1  <- rt("SIH-RD clean",       "SIH-RD", "clean",
             sus_data_clean_encoding(df0, backend = "tibble", verbose = FALSE))
  df2  <- rt("SIH-RD stand",       "SIH-RD", "stand",
             sus_data_standardize(df1, backend = "tibble", verbose = FALSE))
  df3  <- rt("SIH-RD filter_cid",  "SIH-RD", "filter_cid",
             sus_data_filter_cid(df2, disease_group = "respiratory",
                                 verbose = FALSE))
  df4  <- rt("SIH-RD create_vars", "SIH-RD", "derive",
             sus_data_create_variables(df2, verbose = FALSE))
  df4b <- if (!is.null(df4)) sus_meta(df4, backend = "tibble") else NULL
  df5  <- rt("SIH-RD filter_demo", "SIH-RD", "filter_demo",
             sus_data_filter_demographics(
               if (!is.null(df4)) df4 else df2, sex = "F", verbose = FALSE))
  df6  <- rt("SIH-RD aggregate",   "SIH-RD", "aggregate",
             sus_data_aggregate(
               if (!is.null(df4b)) df4b else sus_meta(df2, backend = "tibble"),
               time_unit = "month", backend = "tibble", verbose = FALSE))
  rt("SIH-RD quality", "SIH-RD", "quality",
     sus_data_quality_report(df2, output_format = "console", verbose = FALSE))

  # SIH-SP (schema reduzido: 36 colunas, sem demographics pessoais)
  cli::cli_h2("SIH-SP (schema reduzido)")
  raw_sp <- lp("sih/SIH_SP_SP_2022_01.parquet")
  df0sp  <- mk(head(as.data.frame(raw_sp), 5000L), "SIH")
  df1sp  <- rt("SIH-SP clean",      "SIH-SP", "clean",
               sus_data_clean_encoding(df0sp, backend = "tibble", verbose = FALSE))
  df2sp  <- rt("SIH-SP stand",      "SIH-SP", "stand",
               sus_data_standardize(df1sp, verbose = FALSE))
  rt("SIH-SP filter_cid", "SIH-SP", "filter_cid",
     sus_data_filter_cid(df2sp, icd_codes = "I", verbose = FALSE))
  rs("SIH-SP create_vars", "SIH-SP",
     "Esquema SP nao tem NASC/DTNASC/IDADE pessoal (36 colunas SP_*)")
  rs("SIH-SP filter_demo", "SIH-SP",
     "Esquema SP nao tem SEXO/RACACOR/IDADE pessoal")
  rt("SIH-SP aggregate", "SIH-SP", "aggregate",
     sus_data_aggregate(sus_meta(df2sp, backend = "tibble"),
                        time_unit = "month", backend = "tibble",
                        verbose = FALSE))

  # SIH-ER (esquema minimo: 13 colunas)
  cli::cli_h2("SIH-ER (esquema minimo, 13 colunas)")
  df0er <- mk(lp("sih/SIH_ER_RO_2022_01.parquet"), "SIH")
  df1er <- rt("SIH-ER clean", "SIH-ER", "clean",
              sus_data_clean_encoding(df0er, backend = "tibble", verbose = FALSE))
  df2er <- rt("SIH-ER stand", "SIH-ER", "stand",
              sus_data_standardize(df1er, verbose = FALSE))
  rs("SIH-ER filter_cid",  "SIH-ER", "Sem coluna ICD no esquema ER")
  rs("SIH-ER create_vars", "SIH-ER", "Sem colunas de idade/nascimento no esquema ER")
  rs("SIH-ER filter_demo", "SIH-ER", "Sem demographics no esquema ER")
  rt("SIH-ER aggregate", "SIH-ER", "aggregate",
     sus_data_aggregate(sus_meta(df2er, backend = "tibble"),
                        time_unit = "month", backend = "tibble",
                        verbose = FALSE))
}

# ── SINAN ─────────────────────────────────────────────────────────────────────
if (GROUP %in% c("ALL", "SINAN")) {
  cli::cli_h1("SINAN -- Dengue (Rondonia, 2022)")

  df0  <- mk(lp("sinan/SINAN_DENGUE_RO_2022.parquet"), "SINAN")
  df1  <- rt("SINAN clean",       "SINAN", "clean",
             sus_data_clean_encoding(df0, backend = "tibble", verbose = FALSE))
  df2  <- rt("SINAN stand",       "SINAN", "stand",
             sus_data_standardize(df1, backend = "tibble", verbose = FALSE))
  rs("SINAN filter_cid", "SINAN",
     "SINAN usa codigos de agravo (ID_AGRAVO), nao CID-10")
  df4  <- rt("SINAN create_vars", "SINAN", "derive",
             sus_data_create_variables(df2, verbose = FALSE))
  df4b <- if (!is.null(df4)) sus_meta(df4, backend = "tibble") else NULL
  df5  <- rt("SINAN filter_demo", "SINAN", "filter_demo",
             sus_data_filter_demographics(
               if (!is.null(df4)) df4 else df2, sex = "M", verbose = FALSE))
  df6  <- rt("SINAN aggregate",   "SINAN", "aggregate",
             sus_data_aggregate(
               if (!is.null(df4b)) df4b else sus_meta(df2, backend = "tibble"),
               time_unit = "week", backend = "tibble", verbose = FALSE))
  rt("SINAN quality", "SINAN", "quality",
     sus_data_quality_report(df2, output_format = "console", verbose = FALSE))
}

# ── SIA ───────────────────────────────────────────────────────────────────────
if (GROUP %in% c("ALL", "SIA")) {
  cli::cli_h1("SIA-PA -- Producao Ambulatorial (Rondonia, jan/2022)")

  df0  <- mk(lp("sia/SIA_PA_RO_2022_01.parquet"), "SIA")
  df1  <- rt("SIA-PA clean",       "SIA-PA", "clean",
             sus_data_clean_encoding(df0, backend = "tibble", verbose = FALSE))
  df2  <- rt("SIA-PA stand",       "SIA-PA", "stand",
             sus_data_standardize(df1, backend = "tibble", verbose = FALSE))
  df3  <- rt("SIA-PA filter_cid",  "SIA-PA", "filter_cid",
             sus_data_filter_cid(df2, icd_codes = "J", verbose = FALSE))
  df4  <- rt("SIA-PA create_vars", "SIA-PA", "derive",
             sus_data_create_variables(df2, verbose = FALSE))
  df4b <- if (!is.null(df4)) sus_meta(df4, backend = "tibble") else NULL
  df5  <- rt("SIA-PA filter_demo", "SIA-PA", "filter_demo",
             sus_data_filter_demographics(
               if (!is.null(df4)) df4 else df2, sex = "F", verbose = FALSE))
  df6  <- rt("SIA-PA aggregate",   "SIA-PA", "aggregate",
             sus_data_aggregate(
               if (!is.null(df4b)) df4b else sus_meta(df2, backend = "tibble"),
               time_unit = "month", backend = "tibble", verbose = FALSE))
  rt("SIA-PA quality", "SIA-PA", "quality",
     sus_data_quality_report(df2, output_format = "console", verbose = FALSE))
}

# ── CNES ──────────────────────────────────────────────────────────────────────
if (GROUP %in% c("ALL", "CNES")) {
  cli::cli_h1("CNES-ST -- Estabelecimentos de Saude (Rondonia, jan/2022)")

  df0  <- mk(lp("cnes/CNES_ST_RO_2022_01.parquet"), "CNES")
  df1  <- rt("CNES-ST clean", "CNES-ST", "clean",
             sus_data_clean_encoding(df0, backend = "tibble", verbose = FALSE))
  df2  <- rt("CNES-ST stand", "CNES-ST", "stand",
             sus_data_standardize(df1, backend = "tibble", verbose = FALSE))
  rs("CNES-ST filter_cid",   "CNES-ST",
     "Registro de estabelecimentos -- sem CID-10")
  rs("CNES-ST create_vars",  "CNES-ST",
     "Dados de instalacoes -- sem informacao de idade/nascimento")
  rs("CNES-ST filter_demo",  "CNES-ST",
     "Dados de estabelecimentos -- sem demografia pessoal (sexo/raca/idade)")
  rt("CNES-ST aggregate", "CNES-ST", "aggregate",
     sus_data_aggregate(sus_meta(df2, backend = "tibble"),
                        time_unit = "month", backend = "tibble",
                        verbose = FALSE))
  rt("CNES-ST quality", "CNES-ST", "quality",
     sus_data_quality_report(df2, output_format = "console", verbose = FALSE))
}

# ── SINASC ────────────────────────────────────────────────────────────────────
if (GROUP %in% c("ALL", "SINASC")) {
  cli::cli_h1("SINASC -- Nascidos Vivos (Rondonia, 2022)")

  df0  <- mk(lp("sinasc/SINASC_RO_2022.parquet"), "SINASC")
  df1  <- rt("SINASC clean",       "SINASC", "clean",
             sus_data_clean_encoding(df0, backend = "tibble", verbose = FALSE))
  df2  <- rt("SINASC stand",       "SINASC", "stand",
             sus_data_standardize(df1, backend = "tibble", verbose = FALSE))
  df3  <- rt("SINASC filter_cid",  "SINASC", "filter_cid",
             sus_data_filter_cid(df2, icd_codes = "Q", verbose = FALSE))
  df4  <- rt("SINASC create_vars", "SINASC", "derive",
             sus_data_create_variables(df2, verbose = FALSE))
  df4b <- if (!is.null(df4)) sus_meta(df4, backend = "tibble") else NULL
  df5  <- rt("SINASC filter_demo", "SINASC", "filter_demo",
             sus_data_filter_demographics(
               if (!is.null(df4)) df4 else df2, sex = "M", verbose = FALSE))
  df6  <- rt("SINASC aggregate",   "SINASC", "aggregate",
             sus_data_aggregate(
               if (!is.null(df4b)) df4b else sus_meta(df2, backend = "tibble"),
               time_unit = "month", backend = "tibble", verbose = FALSE))
  rt("SINASC quality", "SINASC", "quality",
     sus_data_quality_report(df2, output_format = "console", verbose = FALSE))
}

# ── Final Report ──────────────────────────────────────────────────────────────
cli::cli_h1("RELATORIO FINAL")
cli::cli_alert_success("Passou  (PASS): {pass}")
cli::cli_alert_info(   "Pulado  (SKIP): {skip_n}")
cli::cli_alert_danger( "Falhou  (FAIL): {fail}")
cli::cli_text("Total: {pass + skip_n + fail}")

report_df <- do.call(rbind, lapply(results, function(x) {
  as.data.frame(x[c("sys","stage","result","name","rows","error")],
                stringsAsFactors = FALSE)
}))
print(report_df[, c("result", "sys", "stage", "name")])

# Write CSV summary
out_csv <- file.path(BASE, "test_results.csv")
utils::write.csv(report_df, out_csv, row.names = FALSE, quote = TRUE)
cli::cli_alert_success("Results saved: {out_csv}")
invisible(report_df)
