# test_spatial_join.R
# Teste de sus_spatial_join com todos os sistemas do testdata (36 arquivos)
#
# Executar com:
#   devtools::load_all(); source("inst/testdata/test_spatial_join.R")
#
# O script testa nivel "munic" para todos os sistemas e "health_facilities"
# para todos os sub-tipos CNES. Sistemas que possuem sus_data_create_variables
# bloqueado (SIH-SP, SIH-RJ, SIH-ER, CNES-*) pulam essa etapa.

devtools::load_all(quiet = TRUE)

BASE <- "inst/testdata"
ok   <- 0L
fail <- 0L

run_test <- function(name, expr) {
  tryCatch({
    r    <- force(expr)
    rows <- if (inherits(r, "data.frame")) nrow(r) else "?"
    cli::cli_alert_success("{name}: OK ({rows} linhas)")
    ok <<- ok + 1L
  }, error = function(e) {
    cli::cli_alert_danger("{name}: FALHOU — {conditionMessage(e)}")
    fail <<- fail + 1L
  })
}

# Helper: carrega parquet e aplica pipeline completo ate aggregate
pipeline <- function(path, system, skip_create_vars = FALSE) {
  df <- climasus4r:::.read_parquet_smart(path) |>
    climasus4r:::new_climasus_df(list(
      stage   = "import",
      system  = system,
      type    = "raw",
      backend = "tibble"
    )) |>
    sus_data_clean_encoding() |>
    sus_data_standardize()

  # df <- tryCatch(sus_data_filter_cid(df), error = function(e) df)

  # if (!skip_create_vars) {
  #   df <- tryCatch(sus_data_create_variables(df), error = function(e) df)
  # }

  # df <- tryCatch(sus_data_filter_demographics(df), error = function(e) df)
  df <- sus_data_aggregate(df)
  df
}

# ============================================================
# SIM — 5 sub-tipos
# ============================================================
cli::cli_h2("SIM")

sim_types <- c("DO", "DOFET", "DOEXT", "DOINF", "DOMAT")
for (sub in sim_types) {
  f <- file.path(BASE, "sim", sprintf("SIM_%s_RO_2022.parquet", sub))
  if (file.exists(f)) {
    run_test(paste0("SIM-", sub, " | munic"), {
      pipeline(f, paste0("SIM-", sub)) |>
        sus_spatial_join(level = "munic", verbose = FALSE)
    })
  }
}

# ============================================================
# SIH — 4 sub-tipos
# ============================================================
cli::cli_h2("SIH")

# SIH-RD: fluxo completo
run_test("SIH-RD | munic", {
  pipeline(file.path(BASE, "sih", "SIH_RD_RO_2022_01.parquet"), "SIH-RD") |>
    sus_spatial_join(level = "munic", verbose = FALSE)
})

# SIH-SP, SIH-RJ, SIH-ER: sus_data_create_variables bloqueado
sih_skip <- list(
  "SP" = "SIH_SP_SP_2022_01.parquet",
  "RJ" = "SIH_RJ_RJ_2022_01.parquet",
  "ER" = "SIH_ER_RO_2022_01.parquet"
)
for (sub in names(sih_skip)) {
  f <- file.path(BASE, "sih", sih_skip[[sub]])
  if (file.exists(f)) {
    run_test(paste0("SIH-", sub, " | munic (skip create_vars)"), {
      pipeline(f, paste0("SIH-", sub), skip_create_vars = TRUE) |>
        sus_spatial_join(level = "munic", verbose = FALSE)
    })
  }
}

# ============================================================
# SIA — 6 sub-tipos
# ============================================================
cli::cli_h2("SIA")

sia_types <- c("PA", "AM", "AQ", "AR", "AD", "PS")
for (sub in sia_types) {
  f <- file.path(BASE, "sia", sprintf("SIA_%s_RO_2022_01.parquet", sub))
  if (file.exists(f)) {
    run_test(paste0("SIA-", sub, " | munic"), {
      pipeline(f, paste0("SIA-", sub)) |>
        sus_spatial_join(level = "munic", verbose = FALSE)
    })
  }
}

# ============================================================
# SINAN — 8 sub-tipos
# ============================================================
cli::cli_h2("SINAN")

sinan_files <- list(
  "DENGUE"       = "SINAN_DENGUE_RO_2022.parquet",
  "CHIKUNGUNYA"  = "SINAN_CHIKUNGUNYA_RO_2022.parquet",
  "ZIKA"         = "SINAN_ZIKA_RO_2022.parquet",
  "MALARIA"      = "SINAN_MALARIA_BR_2022.parquet",
  "CHAGAS"       = "SINAN_CHAGAS_RO_2022.parquet",
  "LV"           = "SINAN_LV_RO_2022.parquet",
  "LT"           = "SINAN_LT_RO_2022.parquet",
  "LEPTOSPIROSE" = "SINAN_LEPTOSPIROSE_RO_2022.parquet"
)
for (nm in names(sinan_files)) {
  f <- file.path(BASE, "sinan", sinan_files[[nm]])
  if (file.exists(f)) {
    run_test(paste0("SINAN-", nm, " | munic"), {
      pipeline(f, paste0("SINAN-", nm)) |>
        sus_spatial_join(level = "munic", verbose = FALSE)
    })
  }
}

# ============================================================
# SINASC
# ============================================================
cli::cli_h2("SINASC")

run_test("SINASC | munic", {
  pipeline(file.path(BASE, "sinasc", "SINASC_RO_2022.parquet"), "SINASC") |>
    sus_spatial_join(level = "munic", verbose = FALSE)
})

# ============================================================
# CNES — 12 sub-tipos | munic + health_facilities
# ============================================================
cli::cli_h2("CNES")

cnes_types <- c("LT","ST","DC","EQ","SR","HB","PF","EP","RC","IN","EF","GM")
for (sub in cnes_types) {
  f <- file.path(BASE, "cnes", sprintf("CNES_%s_RO_2022_01.parquet", sub))
  if (file.exists(f)) {
    run_test(paste0("CNES-", sub, " | munic"), {
      pipeline(f, paste0("CNES-", sub), skip_create_vars = TRUE) |>
        sus_spatial_join(level = "munic", verbose = FALSE)
    })
    run_test(paste0("CNES-", sub, " | health_facilities"), {
      pipeline(f, paste0("CNES-", sub), skip_create_vars = TRUE) |>
        sus_spatial_join(level = "health_facilities", verbose = FALSE)
    })
  }
}

# ============================================================
# Sumario
# ============================================================
cli::cli_h1("Resultados sus_spatial_join")
cli::cli_alert_success("Passou : {ok}")
cli::cli_alert_danger( "Falhou : {fail}")
cli::cli_alert_info(   "Total  : {ok + fail}")
