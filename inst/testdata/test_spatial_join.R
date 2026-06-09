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
# Sumario sus_spatial_join
# ============================================================
cli::cli_h1("Resultados sus_spatial_join")
cli::cli_alert_success("Passou : {ok}")
cli::cli_alert_danger( "Falhou : {fail}")
cli::cli_alert_info(   "Total  : {ok + fail}")

# ============================================================
# sus_census_join
# ============================================================
# Nota: os testes "censobr" fazem download pela internet (~100 MB na 1a vez).
# Os testes "mock" sao offline e testam apenas a logica de juncao/metadados.
# ============================================================
ok_c  <- 0L
fail_c <- 0L

run_test_c <- function(name, expr) {
  tryCatch({
    r    <- force(expr)
    rows <- if (inherits(r, "data.frame")) nrow(r) else "?"
    cli::cli_alert_success("{name}: OK ({rows} linhas)")
    ok_c <<- ok_c + 1L
  }, error = function(e) {
    cli::cli_alert_danger("{name}: FALHOU — {conditionMessage(e)}")
    fail_c <<- fail_c + 1L
  })
}

# Dado base: SIM-DO Rondonia ate o estagio spatial
df_sim_spatial <- tryCatch(
  pipeline(file.path(BASE, "sim", "SIM_DO_RO_2022.parquet"), "SIM-DO") |>
    sus_spatial_join(level = "munic", verbose = FALSE),
  error = function(e) NULL
)

cli::cli_h2("sus_census_join — parametros e validacao (offline)")

# Validacao: dataset invalido deve abortar
run_test_c("sus_census_join | erro: dataset invalido", {
  err <- tryCatch(
    sus_census_join(df_sim_spatial, dataset = "inexistente", verbose = FALSE),
    error = function(e) conditionMessage(e)
  )
  stopifnot(is.character(err))
  err
})

# Validacao: tracts_dataset invalido deve abortar
run_test_c("sus_census_join | erro: tracts_dataset invalido", {
  err <- tryCatch(
    sus_census_join(df_sim_spatial, dataset = "tracts",
                    tracts_dataset = "INVALIDO", verbose = FALSE),
    error = function(e) conditionMessage(e)
  )
  stopifnot(is.character(err))
  err
})

# Validacao: input nao-climasus_df deve abortar
run_test_c("sus_census_join | erro: input nao climasus_df", {
  err <- tryCatch(
    sus_census_join(data.frame(x = 1), verbose = FALSE),
    error = function(e) conditionMessage(e)
  )
  stopifnot(is.character(err))
  err
})

cli::cli_h2("sus_census_join — censobr (requer internet)")

if (!is.null(df_sim_spatial)) {

  run_test_c("sus_census_join | SIM-DO | population (sum)", {
    r <- sus_census_join(
      df_sim_spatial,
      dataset         = "population",
      year            = 2010,
      aggregation_fun = "sum",
      translate_columns = TRUE,
      verbose         = FALSE
    )
    stopifnot(sus_meta(r, "stage") == "census")
    stopifnot(sus_meta(r, "type")  == "population")
    r
  })

  run_test_c("sus_census_join | SIM-DO | population (mean)", {
    sus_census_join(
      df_sim_spatial,
      dataset         = "population",
      year            = 2010,
      aggregation_fun = "mean",
      translate_columns = FALSE,
      verbose         = FALSE
    )
  })

  run_test_c("sus_census_join | SIM-DO | households (sum)", {
    sus_census_join(
      df_sim_spatial,
      dataset         = "households",
      year            = 2010,
      aggregation_fun = "sum",
      translate_columns = TRUE,
      verbose         = FALSE
    )
  })

  run_test_c("sus_census_join | SIM-DO | tracts Basico (sum)", {
    r <- sus_census_join(
      df_sim_spatial,
      dataset         = "tracts",
      tracts_dataset  = "Basico",
      year            = 2010,
      aggregation_fun = "sum",
      translate_columns = TRUE,
      verbose         = FALSE
    )
    stopifnot(sus_meta(r, "stage") == "census")
    stopifnot(sus_meta(r, "type")  == "tracts")
    # colunas de setor (code_tract) nao devem vazar para o resultado
    stopifnot(!"code_tract" %in% names(r))
    r
  })

  run_test_c("sus_census_join | SIM-DO | tracts DomicilioRenda (mean)", {
    sus_census_join(
      df_sim_spatial,
      dataset         = "tracts",
      tracts_dataset  = "DomicilioRenda",
      year            = 2010,
      aggregation_fun = "mean",
      translate_columns = FALSE,
      verbose         = FALSE
    )
  })

} else {
  cli::cli_alert_warning("df_sim_spatial nao disponivel; testes censobr pulados.")
}

cli::cli_h1("Resultados sus_census_join")
cli::cli_alert_success("Passou : {ok_c}")
cli::cli_alert_danger( "Falhou : {fail_c}")
cli::cli_alert_info(   "Total  : {ok_c + fail_c}")

# ============================================================
# sus_socio_compute_indicators
# ============================================================
# Todos os testes usam dataframes mock (offline).
# ============================================================
ok_i  <- 0L
fail_i <- 0L

run_test_i <- function(name, expr) {
  tryCatch({
    r    <- force(expr)
    rows <- if (inherits(r, "data.frame")) nrow(r) else "?"
    cli::cli_alert_success("{name}: OK ({rows} linhas)")
    ok_i <<- ok_i + 1L
  }, error = function(e) {
    cli::cli_alert_danger("{name}: FALHOU — {conditionMessage(e)}")
    fail_i <<- fail_i + 1L
  })
}

# Helper: cria climasus_df minimo no estagio "census"
mock_census_df <- function(...) {
  df <- tibble::tibble(code_muni_7 = "1100015", ...)
  climasus4r:::new_climasus_df(df, list(
    stage = "census", system = "SIM-DO", type = "tracts", backend = "tibble"
  ))
}

cli::cli_h2("sus_socio_list_indicators")

run_test_i("sus_socio_list_indicators | lang=pt", {
  r <- sus_socio_list_indicators(lang = "pt")
  stopifnot(inherits(r, "tbl_df"))
  stopifnot(nrow(r) >= 20L)
  stopifnot(all(c("id", "name", "category", "required_cols") %in% names(r)))
  r
})

run_test_i("sus_socio_list_indicators | lang=en", {
  r <- sus_socio_list_indicators(lang = "en")
  stopifnot(nrow(r) >= 20L)
  r
})

run_test_i("sus_socio_list_indicators | filter mortality", {
  r <- sus_socio_list_indicators(lang = "pt", category = "mortality")
  stopifnot(nrow(r) > 0L)
  stopifnot(all(r$category == "mortality"))
  r
})

run_test_i("sus_socio_list_indicators | filter socioeconomic", {
  r <- sus_socio_list_indicators(lang = "en", category = "socioeconomic")
  stopifnot(nrow(r) > 0L)
  r
})

cli::cli_h2("sus_socio_compute_indicators — coluna unica, sem col_mapping")

run_test_i("compute_indicators | dependency_ratio (colunas diretas)", {
  df <- mock_census_df(pop_young = 1000L, pop_elderly = 300L, pop_working = 2500L)
  r  <- sus_socio_compute_indicators(df, indicators = "dependency_ratio",
                                     add_ci = FALSE, verbose = FALSE)
  stopifnot("ind_dependency_ratio" %in% names(r))
  expected <- (1000 + 300) / 2500 * 100
  stopifnot(abs(r$ind_dependency_ratio - expected) < 1e-9)
  r
})

run_test_i("compute_indicators | aging_index (colunas diretas)", {
  df <- mock_census_df(pop_elderly = 300L, pop_young = 1000L)
  r  <- sus_socio_compute_indicators(df, indicators = "aging_index",
                                     add_ci = FALSE, verbose = FALSE)
  stopifnot("ind_aging_index" %in% names(r))
  r
})

run_test_i("compute_indicators | infant_mortality_rate + CI Poisson", {
  df <- mock_census_df(deaths_infant = 10L, live_births = 500L)
  r  <- sus_socio_compute_indicators(df, indicators = "infant_mortality_rate",
                                     add_ci = TRUE, confidence_level = 0.95,
                                     verbose = FALSE)
  stopifnot(all(c("ind_infant_mortality_rate",
                  "ind_infant_mortality_rate_low",
                  "ind_infant_mortality_rate_high") %in% names(r)))
  stopifnot(r$ind_infant_mortality_rate_low  <= r$ind_infant_mortality_rate)
  stopifnot(r$ind_infant_mortality_rate_high >= r$ind_infant_mortality_rate)
  r
})

run_test_i("compute_indicators | water_connection_rate + CI Binomial", {
  df <- mock_census_df(hh_water = 800L, total_hh = 1000L)
  r  <- sus_socio_compute_indicators(df, indicators = "water_connection_rate",
                                     add_ci = TRUE, verbose = FALSE)
  stopifnot(all(c("ind_water_connection_rate",
                  "ind_water_connection_rate_low",
                  "ind_water_connection_rate_high") %in% names(r)))
  stopifnot(r$ind_water_connection_rate_low  >= 0)
  stopifnot(r$ind_water_connection_rate_high <= 100)
  r
})

run_test_i("compute_indicators | gini_index (sem IC)", {
  df <- mock_census_df(gini_value = 0.52)
  r  <- sus_socio_compute_indicators(df, indicators = "gini_index",
                                     add_ci = TRUE, verbose = FALSE)
  stopifnot("ind_gini_index" %in% names(r))
  # gini uncertainty_method = "none" -> sem colunas _low/_high
  stopifnot(!"ind_gini_index_low" %in% names(r))
  r
})

cli::cli_h2("sus_socio_compute_indicators — col_mapping")

run_test_i("compute_indicators | col_mapping simples", {
  df <- mock_census_df(pop_0_14 = 1000L, pop_65_mais = 300L, pop_15_64 = 2500L)
  r  <- sus_socio_compute_indicators(
    df,
    indicators  = "dependency_ratio",
    col_mapping = list(pop_young = "pop_0_14",
                       pop_elderly = "pop_65_mais",
                       pop_working = "pop_15_64"),
    add_ci  = FALSE,
    verbose = FALSE
  )
  stopifnot("ind_dependency_ratio" %in% names(r))
  r
})

run_test_i("compute_indicators | col_mapping multiplos indicadores", {
  df <- mock_census_df(
    p_jovem  = 800L,  p_idoso = 200L,  p_ativo = 2000L,
    dom_agua = 700L,  total_dom = 900L
  )
  r <- sus_socio_compute_indicators(
    df,
    indicators  = c("dependency_ratio", "water_connection_rate"),
    col_mapping = list(
      pop_young   = "p_jovem",
      pop_elderly = "p_idoso",
      pop_working = "p_ativo",
      hh_water    = "dom_agua",
      total_hh    = "total_dom"
    ),
    verbose = FALSE
  )
  stopifnot(all(c("ind_dependency_ratio",
                  "ind_water_connection_rate") %in% names(r)))
  r
})

cli::cli_h2("sus_socio_compute_indicators — auto-deteccao (indicators = NULL)")

run_test_i("compute_indicators | indicators=NULL auto-detecta", {
  df <- mock_census_df(pop_elderly = 200L, pop_young = 800L)
  r  <- sus_socio_compute_indicators(df, indicators = NULL,
                                     add_ci = FALSE, verbose = FALSE)
  # aging_index deve ter sido calculado (unico com essas colunas)
  stopifnot("ind_aging_index" %in% names(r))
  r
})

run_test_i("compute_indicators | indicators=NULL multiplos detectados", {
  df <- mock_census_df(
    pop_elderly  = 200L, pop_young    = 800L,
    hh_water     = 700L, total_hh     = 900L,
    hh_sewage    = 600L
  )
  r <- sus_socio_compute_indicators(df, indicators = NULL,
                                    add_ci = FALSE, verbose = FALSE)
  stopifnot("ind_aging_index" %in% names(r))
  stopifnot("ind_water_connection_rate" %in% names(r))
  stopifnot("ind_sewage_connection_rate" %in% names(r))
  r
})

cli::cli_h2("sus_socio_compute_indicators — degradacao gracosa")

run_test_i("compute_indicators | coluna ausente: pula indicador, computa os outros", {
  # pop_working esta ausente: dependency_ratio deve ser pulado
  # deaths_infant + live_births presentes: infant_mortality_rate deve ser calculado
  df <- mock_census_df(
    pop_young   = 1000L, pop_elderly = 300L,
    # pop_working AUSENTE
    deaths_infant = 5L, live_births = 200L
  )
  r <- sus_socio_compute_indicators(
    df,
    indicators = c("dependency_ratio", "infant_mortality_rate"),
    add_ci     = FALSE,
    verbose    = FALSE
  )
  stopifnot(!"ind_dependency_ratio" %in% names(r))
  stopifnot("ind_infant_mortality_rate" %in% names(r))
  r
})

run_test_i("compute_indicators | ID desconhecido: aviso, nao aborta", {
  df <- mock_census_df(pop_elderly = 200L, pop_young = 800L)
  r  <- withCallingHandlers(
    sus_socio_compute_indicators(
      df,
      indicators = c("aging_index", "indicador_que_nao_existe"),
      add_ci     = FALSE,
      verbose    = FALSE
    ),
    warning = function(w) invokeRestart("muffleWarning")
  )
  stopifnot("ind_aging_index" %in% names(r))
  r
})

cli::cli_h2("sus_socio_compute_indicators — metadados sus_meta")

run_test_i("compute_indicators | sus_meta stage=census type=indicators", {
  df <- mock_census_df(pop_elderly = 200L, pop_young = 800L)
  r  <- sus_socio_compute_indicators(df, indicators = "aging_index",
                                     add_ci = FALSE, verbose = FALSE)
  stopifnot(sus_meta(r, "stage") == "census")
  stopifnot(sus_meta(r, "type")  == "indicators")
  r
})

run_test_i("compute_indicators | history registra indicador calculado", {
  df <- mock_census_df(pop_elderly = 200L, pop_young = 800L)
  r  <- sus_socio_compute_indicators(df, indicators = "aging_index",
                                     add_ci = FALSE, verbose = FALSE)
  h  <- sus_meta(r, "history")
  stopifnot(any(grepl("aging_index", h)))
  r
})

cli::cli_h2("sus_socio_compute_indicators — multiplas linhas")

run_test_i("compute_indicators | 52 municipios, Poisson CI coerente", {
  n  <- 52L
  df <- tibble::tibble(
    code_muni_7   = sprintf("%07d", seq_len(n)),
    deaths_infant = sample(0L:30L, n, replace = TRUE),
    live_births   = sample(100L:2000L, n, replace = TRUE)
  )
  df <- climasus4r:::new_climasus_df(df, list(
    stage = "census", system = "SIM-DO", type = "tracts", backend = "tibble"
  ))
  r <- sus_socio_compute_indicators(df, indicators = "infant_mortality_rate",
                                    add_ci = TRUE, verbose = FALSE)
  stopifnot(nrow(r) == n)
  stopifnot(all(r$ind_infant_mortality_rate_low  <= r$ind_infant_mortality_rate |
                  is.na(r$ind_infant_mortality_rate_low)))
  stopifnot(all(r$ind_infant_mortality_rate_high >= r$ind_infant_mortality_rate |
                  is.na(r$ind_infant_mortality_rate_high)))
  r
})

cli::cli_h1("Resultados sus_socio_compute_indicators")
cli::cli_alert_success("Passou : {ok_i}")
cli::cli_alert_danger( "Falhou : {fail_i}")
cli::cli_alert_info(   "Total  : {ok_i + fail_i}")

# ============================================================
# Sumario global
# ============================================================
cli::cli_h1("Sumario Global")
cli::cli_alert_success("sus_spatial_join         : {ok} / {ok + fail}")
cli::cli_alert_success("sus_census_join          : {ok_c} / {ok_c + fail_c}")
cli::cli_alert_success("sus_socio_compute_indicators: {ok_i} / {ok_i + fail_i}")
total_ok   <- ok + ok_c + ok_i
total_fail <- fail + fail_c + fail_i
cli::cli_alert_info("Total geral: {total_ok} passou, {total_fail} falhou ({total_ok + total_fail} testes)")
