# create_climate_testdata.R
# Gera fixtures de dados climaticos para os testes de sus_climate_aggregate().
#
# Executar com:
#   devtools::load_all()
#   source("inst/testdata/create_climate_testdata.R")
#
# Primeira execucao requer internet (INMET + geobr). Execucoes subsequentes
# leem os parquets ja salvos em inst/testdata/climate/.

devtools::load_all(quiet = TRUE)

CLIMATE_DIR <- "inst/testdata/climate"
if (!dir.exists(CLIMATE_DIR)) dir.create(CLIMATE_DIR, recursive = TRUE)

f_clim   <- file.path(CLIMATE_DIR, "climate_ro_2022.parquet")
f_health <- file.path(CLIMATE_DIR, "health_ro_2022_spatial.rds")

# ── 1. Dados climaticos INMET: RO 2022 ───────────────────────────────────────
if (!file.exists(f_clim)) {
  cli::cli_h2("Baixando dados INMET para RO (2022)...")

  df_inmet <- sus_climate_inmet(
    years     = 2022,
    uf        = "RO",
    use_cache = TRUE,
    verbose   = TRUE
  )

  cli::cli_h2("Aplicando gap-filling (XGBoost)...")
  df_filled <- sus_climate_fill_inmet(
    df_inmet,
    target_var = c("tair_dry_bulb_c", "tair_max_c", "tair_min_c", "rainfall_mm",
                   "rh_mean_porc"),
    run_evaluation = FALSE,
    verbose        = FALSE
  )

  write_parquet_climasus(df_filled, f_clim)
  cli::cli_alert_success("Clima salvo: {f_clim}")
} else {
  cli::cli_alert_info("Clima ja existe: {f_clim}")
}

# ── 2. Dados de saude: SIM-DO RO 2022 ate estagio spatial ────────────────────
if (!file.exists(f_health)) {
  cli::cli_h2("Construindo health fixture (SIM-DO RO 2022 -> spatial)...")

  BASE <- "inst/testdata"
  f_sim <- file.path(BASE, "sim", "SIM_DO_RO_2022.parquet")
  if (!file.exists(f_sim)) {
    cli::cli_abort("Arquivo nao encontrado: {f_sim}. Execute create_testdata.R primeiro.")
  }

  pipeline_to_aggregate <- function(path, system) {
    climasus4r:::.read_parquet_smart(path) |>
      climasus4r:::new_climasus_df(list(
        stage   = "import",
        system  = system,
        type    = "raw",
        backend = "tibble"
      )) |>
      sus_data_clean_encoding() |>
      sus_data_standardize() |>
      sus_data_aggregate()
  }

  df_health_spatial <- pipeline_to_aggregate(f_sim, "SIM-DO") |>
    sus_spatial_join(level = "munic", verbose = FALSE)

  # The output of sus_spatial_join() is a climasus_df (not sf subclass) but
  # contains an sfc geometry column. Arrow cannot serialize sfc columns, so we
  # save as RDS which preserves all R classes and attributes exactly.
  saveRDS(df_health_spatial, f_health)
  cli::cli_alert_success("Saude salvo: {f_health}")
} else {
  cli::cli_alert_info("Saude ja existe: {f_health}")
}

cli::cli_alert_success("Fixtures prontos em '{CLIMATE_DIR}'.")
