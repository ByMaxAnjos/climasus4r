#!/usr/bin/env Rscript
# ============================================================
# test_all_pipeline_v2.R
# Pipeline completo — perspectiva de usuário normal
#
# Cobre todos os sistemas DATASUS disponíveis no testdata com
# o pipeline completo de 6 etapas (backend = "arrow" padrão):
#
#   sus_data_clean_encoding()  |>
#   sus_data_standardize()     |>
#   sus_data_filter_cid()      |>   (quando ICD-10 disponível)
#   sus_data_create_variables()|>   (quando dados demográficos disponíveis)
#   sus_data_filter_demographics()|> (quando sexo/raça disponíveis)
#   sus_data_aggregate()
#
# Uso:
#   Rscript inst/testdata/test_all_pipeline_v2.R
#   Rscript inst/testdata/test_all_pipeline_v2.R SIM
#   source("inst/testdata/test_all_pipeline_v2.R")
#
# Resultados gravados em: inst/testdata/test_results_v2.csv
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

# ── Scaffolding de teste ───────────────────────────────────────────────────────
pass <- 0L; fail <- 0L; skip_n <- 0L
results <- list()

rt <- function(label, sys, stage, expr) {
  r <- tryCatch(expr, error = function(e) {
    cli::cli_alert_danger("{label}: FAIL -- {conditionMessage(e)}")
    results[[length(results) + 1L]] <<- list(
      label = label, sys = sys, stage = stage,
      result = "FAIL", rows = NA_integer_, error = conditionMessage(e)
    )
    fail <<- fail + 1L
    invisible(NULL)
  })
  if (!is.null(r)) {
    n <- tryCatch(as.integer(nrow(r)), error = function(e) NA_integer_)
    cli::cli_alert_success("{label}: OK ({n %||% '?'} linhas)")
    results[[length(results) + 1L]] <<- list(
      label = label, sys = sys, stage = stage,
      result = "PASS", rows = n, error = NA_character_
    )
    pass <<- pass + 1L
  }
  invisible(r)
}

rs <- function(label, sys, motivo) {
  cli::cli_alert_info("{label}: SKIP -- {motivo}")
  results[[length(results) + 1L]] <<- list(
    label = label, sys = sys, stage = "skip",
    result = "SKIP", rows = NA_integer_, error = motivo
  )
  skip_n <<- skip_n + 1L
}

# Carrega Parquet do testdata e cria climasus_df no estágio "import"
carregar <- function(caminho_relativo, sistema) {
  df <- climasus4r:::.read_parquet_smart(file.path(BASE, caminho_relativo))
  climasus4r:::new_climasus_df(
    as.data.frame(df),
    list(stage = "import", system = sistema, type = "raw", backend = "tibble")
  )
}

# Coleta resultado de um pipeline lazy para verificar linhas
coletar <- function(df) if (!is.null(df)) dplyr::collect(df) else NULL

`%||%` <- function(x, y) if (is.null(x)) y else x


# ==============================================================================
# SIM — Sistema de Informações sobre Mortalidade
# ==============================================================================
if (GROUP %in% c("ALL", "SIM")) {

  # ── SIM-DO: Declarações de Óbito ──────────────────────────────────────────
  cli::cli_h1("SIM-DO | Declarações de Óbito — Rondônia 2022")
  # Cenário: analista quer calcular mortalidade respiratória mensal por sexo

  df_do_raw <- carregar("sim/SIM_DO_RO_2022.parquet", "SIM-DO")
  df_do_clean <- rt("SIM-DO | clean",       "SIM-DO", "clean",
    df_do_raw |> sus_data_clean_encoding(lang = "pt", verbose = FALSE))

  df_do_std <- rt("SIM-DO | standardize",   "SIM-DO", "stand",
    df_do_clean |> sus_data_standardize(lang = "pt", verbose = FALSE))

  df_do_cid <- rt("SIM-DO | filter_cid",    "SIM-DO", "filter_cid",
    df_do_std |> sus_data_filter_cid(
      disease_group = "respiratory", lang = "pt", verbose = FALSE))

  df_do_vars <- rt("SIM-DO | create_vars",  "SIM-DO", "derive",
    df_do_std |> sus_data_create_variables(
      create_age_groups    = TRUE,
      age_breaks           = c(0, 5, 15, 60, Inf),
      create_calendar_vars = TRUE,
      lang = "pt", verbose = FALSE))

  df_do_demo <- rt("SIM-DO | filter_demo",  "SIM-DO", "filter_demo",
    (df_do_vars %||% df_do_std) |>
      sus_data_filter_demographics(sex = "F", lang = "pt", verbose = FALSE))

  rt("SIM-DO | aggregate (mês)",             "SIM-DO", "aggregate",
    (df_do_vars %||% df_do_std) |>
      sus_data_aggregate(time_unit = "month", lang = "pt", verbose = FALSE))

  # ── SIM-DOEXT: Causas Externas ────────────────────────────────────────────
  cli::cli_h2("SIM-DOEXT | Causas Externas (nacional)")

  df_doext_raw <- carregar("sim/SIM_DOEXT_RO_2022.parquet", "SIM-DOEXT")
  df_doext_std <- rt("SIM-DOEXT | clean+stand", "SIM-DOEXT", "stand",
    df_doext_raw |>
      sus_data_clean_encoding(lang = "pt", verbose = FALSE) |>
      sus_data_standardize(lang = "pt", verbose = FALSE))

  rt("SIM-DOEXT | filter_cid (causas externas)", "SIM-DOEXT", "filter_cid",
    df_doext_std |> sus_data_filter_cid(
      icd_codes = "V", lang = "pt", verbose = FALSE))

  df_doext_vars <- rt("SIM-DOEXT | create_vars", "SIM-DOEXT", "derive",
    df_doext_std |> sus_data_create_variables(
      lang = "pt", verbose = FALSE))

  rt("SIM-DOEXT | filter_demo (masculino)", "SIM-DOEXT", "filter_demo",
    (df_doext_vars %||% df_doext_std) |>
      sus_data_filter_demographics(sex = "M", lang = "pt", verbose = FALSE))

  rt("SIM-DOEXT | aggregate (mês)", "SIM-DOEXT", "aggregate",
    (df_doext_vars %||% df_doext_std) |>
      sus_data_aggregate(time_unit = "month", lang = "pt", verbose = FALSE))

  # ── SIM-DOFET: Óbitos Fetais ───────────────────────────────────────────────
  cli::cli_h2("SIM-DOFET | Óbitos Fetais (nacional)")

  df_dofet_raw <- carregar("sim/SIM_DOFET_RO_2022.parquet", "SIM-DOFET")
  df_dofet_std <- rt("SIM-DOFET | clean+stand", "SIM-DOFET", "stand",
    df_dofet_raw |>
      sus_data_clean_encoding(lang = "pt", verbose = FALSE) |>
      sus_data_standardize(lang = "pt", verbose = FALSE))

  rt("SIM-DOFET | filter_cid (perinatais)", "SIM-DOFET", "filter_cid",
    df_dofet_std |> sus_data_filter_cid(
      icd_codes = "P", lang = "pt", verbose = FALSE))

  df_dofet_vars <- rt("SIM-DOFET | create_vars", "SIM-DOFET", "derive",
    df_dofet_std |> sus_data_create_variables(
      lang = "pt", verbose = FALSE))

  rt("SIM-DOFET | filter_demo", "SIM-DOFET", "filter_demo",
    (df_dofet_vars %||% df_dofet_std) |>
      sus_data_filter_demographics(sex = "M", lang = "pt", verbose = FALSE))

  rt("SIM-DOFET | aggregate (mês)", "SIM-DOFET", "aggregate",
    (df_dofet_vars %||% df_dofet_std) |>
      sus_data_aggregate(time_unit = "month", lang = "pt", verbose = FALSE))

  # ── SIM-DOINF: Óbitos Infantis ─────────────────────────────────────────────
  cli::cli_h2("SIM-DOINF | Óbitos Infantis (nacional)")

  df_doinf_raw <- carregar("sim/SIM_DOINF_RO_2022.parquet", "SIM-DOINF")
  df_doinf_std <- rt("SIM-DOINF | clean+stand", "SIM-DOINF", "stand",
    df_doinf_raw |>
      sus_data_clean_encoding(lang = "pt", verbose = FALSE) |>
      sus_data_standardize(lang = "pt", verbose = FALSE))

  rt("SIM-DOINF | filter_cid (infecciosas)", "SIM-DOINF", "filter_cid",
    df_doinf_std |> sus_data_filter_cid(
      icd_codes = "A", lang = "pt", verbose = FALSE))

  df_doinf_vars <- rt("SIM-DOINF | create_vars", "SIM-DOINF", "derive",
    df_doinf_std |> sus_data_create_variables(
      age_breaks = c(0, 1, 5, Inf), lang = "pt", verbose = FALSE))

  rt("SIM-DOINF | filter_demo", "SIM-DOINF", "filter_demo",
    (df_doinf_vars %||% df_doinf_std) |>
      sus_data_filter_demographics(sex = "F", lang = "pt", verbose = FALSE))

  rt("SIM-DOINF | aggregate (mês)", "SIM-DOINF", "aggregate",
    (df_doinf_vars %||% df_doinf_std) |>
      sus_data_aggregate(time_unit = "month", lang = "pt", verbose = FALSE))

  # ── SIM-DOMAT: Óbitos Maternos ─────────────────────────────────────────────
  cli::cli_h2("SIM-DOMAT | Óbitos Maternos (nacional)")

  df_domat_raw <- carregar("sim/SIM_DOMAT_RO_2022.parquet", "SIM-DOMAT")
  df_domat_std <- rt("SIM-DOMAT | clean+stand", "SIM-DOMAT", "stand",
    df_domat_raw |>
      sus_data_clean_encoding(lang = "pt", verbose = FALSE) |>
      sus_data_standardize(lang = "pt", verbose = FALSE))

  rt("SIM-DOMAT | filter_cid (complicações obstétricas)", "SIM-DOMAT", "filter_cid",
    df_domat_std |> sus_data_filter_cid(
      icd_codes = "O", lang = "pt", verbose = FALSE))

  df_domat_vars <- rt("SIM-DOMAT | create_vars", "SIM-DOMAT", "derive",
    df_domat_std |> sus_data_create_variables(
      age_breaks = c(10, 20, 35, 50, Inf), lang = "pt", verbose = FALSE))

  rt("SIM-DOMAT | filter_demo (idade reprodutiva 15–49)", "SIM-DOMAT", "filter_demo",
    (df_domat_vars %||% df_domat_std) |>
      sus_data_filter_demographics(
        sex = "F", age_range = c(15, 49), lang = "pt", verbose = FALSE))

  rt("SIM-DOMAT | aggregate (mês)", "SIM-DOMAT", "aggregate",
    (df_domat_vars %||% df_domat_std) |>
      sus_data_aggregate(time_unit = "month", lang = "pt", verbose = FALSE))
}


# ==============================================================================
# SIH — Sistema de Informações Hospitalares
# ==============================================================================
if (GROUP %in% c("ALL", "SIH")) {

  # ── SIH-RD: Registro de Dados (esquema completo) ──────────────────────────
  cli::cli_h1("SIH-RD | Internações — Rondônia jan/2022")
  # Cenário: analista quer mapear internações cardiovasculares por faixa etária

  df_rd_raw <- carregar("sih/SIH_RD_RO_2022_01.parquet", "SIH-RD")
  df_rd_clean <- rt("SIH-RD | clean",      "SIH-RD", "clean",
    df_rd_raw |> sus_data_clean_encoding(lang = "pt", verbose = FALSE))

  df_rd_std <- rt("SIH-RD | standardize",  "SIH-RD", "stand",
    df_rd_clean |> sus_data_standardize(lang = "pt", verbose = FALSE))

  rt("SIH-RD | filter_cid (cardiovascular)", "SIH-RD", "filter_cid",
    df_rd_std |> sus_data_filter_cid(
      disease_group = "cardiovascular", lang = "pt", verbose = FALSE))

  df_rd_vars <- rt("SIH-RD | create_vars", "SIH-RD", "derive",
    df_rd_std |> sus_data_create_variables(
      create_age_groups    = TRUE,
      age_breaks           = c(0, 5, 15, 60, Inf),
      create_calendar_vars = TRUE,
      lang = "pt", verbose = FALSE))

  rt("SIH-RD | filter_demo (feminino)", "SIH-RD", "filter_demo",
    (df_rd_vars %||% df_rd_std) |>
      sus_data_filter_demographics(sex = "F", lang = "pt", verbose = FALSE))

  rt("SIH-RD | aggregate (mês)", "SIH-RD", "aggregate",
    (df_rd_vars %||% df_rd_std) |>
      sus_data_aggregate(time_unit = "month", lang = "pt", verbose = FALSE))

  # ── SIH-RJ: Registro de Dados (Rio de Janeiro) ────────────────────────────
  cli::cli_h2("SIH-RJ | Regime Jurídico — RJ jan/2022")

  df_rj_raw <- carregar("sih/SIH_RJ_RJ_2022_01.parquet", "SIH-RJ")
  df_rj_std <- rt("SIH-RJ | clean+stand", "SIH-RJ", "stand",
    df_rj_raw |>
      sus_data_clean_encoding(lang = "pt", verbose = FALSE) |>
      sus_data_standardize(lang = "pt", verbose = FALSE))

  rt("SIH-RJ | filter_cid (neoplasias)", "SIH-RJ", "filter_cid",
    df_rj_std |> sus_data_filter_cid(
      icd_codes = "C", lang = "pt", verbose = FALSE))

  df_rj_vars <- rt("SIH-RJ | create_vars", "SIH-RJ", "derive",
    df_rj_std |> sus_data_create_variables(
      lang = "pt", verbose = FALSE))

  rt("SIH-RJ | filter_demo", "SIH-RJ", "filter_demo",
    (df_rj_vars %||% df_rj_std) |>
      sus_data_filter_demographics(sex = "M", lang = "pt", verbose = FALSE))

  rt("SIH-RJ | aggregate (mês)", "SIH-RJ", "aggregate",
    (df_rj_vars %||% df_rj_std) |>
      sus_data_aggregate(time_unit = "month", lang = "pt", verbose = FALSE))

  # ── SIH-SP: Serviços Profissionais (esquema reduzido, sem dados pessoais) ──
  cli::cli_h2("SIH-SP | Serviços Profissionais — SP jan/2022 (esquema reduzido)")
  # SIH-SP não contém NASC, IDADE, SEXO — apenas dados de procedimentos

  df_sp_raw  <- carregar("sih/SIH_SP_SP_2022_01.parquet", "SIH-SP")
  df_sp_raw  <- climasus4r:::new_climasus_df(
    head(as.data.frame(df_sp_raw), 10000L),
    list(stage = "import", system = "SIH-SP", type = "raw", backend = "tibble")
  )
  df_sp_std <- rt("SIH-SP | clean+stand", "SIH-SP", "stand",
    df_sp_raw |>
      sus_data_clean_encoding(lang = "pt", verbose = FALSE) |>
      sus_data_standardize(lang = "pt", verbose = FALSE))

  rt("SIH-SP | filter_cid (código explícito)", "SIH-SP", "filter_cid",
    df_sp_std |> sus_data_filter_cid(
      icd_codes = "I", lang = "pt", verbose = FALSE))

  rs("SIH-SP | create_vars", "SIH-SP",
     "Esquema SP (36 colunas SP_*): sem NASC/DTNASC/IDADE pessoal")
  rs("SIH-SP | filter_demo", "SIH-SP",
     "Esquema SP: sem SEXO/RACACOR/IDADE pessoal")

  rt("SIH-SP | aggregate (mês)", "SIH-SP", "aggregate",
    df_sp_std |>
      sus_data_aggregate(time_unit = "month", lang = "pt", verbose = FALSE))

  # ── SIH-ER: Emergência (esquema mínimo, 13 colunas) ───────────────────────
  cli::cli_h2("SIH-ER | Emergência — Rondônia jan/2022 (esquema mínimo)")

  df_er_raw <- carregar("sih/SIH_ER_RO_2022_01.parquet", "SIH-ER")
  df_er_std <- rt("SIH-ER | clean+stand", "SIH-ER", "stand",
    df_er_raw |>
      sus_data_clean_encoding(lang = "pt", verbose = FALSE) |>
      sus_data_standardize(lang = "pt", verbose = FALSE))

  rs("SIH-ER | filter_cid",  "SIH-ER",
     "Esquema ER (13 colunas): sem coluna de diagnóstico ICD-10")
  rs("SIH-ER | create_vars", "SIH-ER",
     "Esquema ER: sem colunas de idade/nascimento")
  rs("SIH-ER | filter_demo", "SIH-ER",
     "Esquema ER: sem demographics pessoais")

  rt("SIH-ER | aggregate (mês)", "SIH-ER", "aggregate",
    df_er_std |>
      sus_data_aggregate(time_unit = "month", lang = "pt", verbose = FALSE))
}


# ==============================================================================
# SINAN — Sistema de Informação de Agravos de Notificação
# NOTA: SINAN usa código de agravo (ID_AGRAVO), não CID-10.
#       sus_data_filter_cid() não se aplica a este sistema.
# ==============================================================================
if (GROUP %in% c("ALL", "SINAN")) {

  sinan_casos <- list(
    list(label = "Dengue",        arq = "sinan/SINAN_DENGUE_RO_2022.parquet",        sys = "SINAN-DENGUE"),
    list(label = "Chikungunya",   arq = "sinan/SINAN_CHIKUNGUNYA_RO_2022.parquet",   sys = "SINAN-CHIKUNGUNYA"),
    list(label = "Zika",          arq = "sinan/SINAN_ZIKA_RO_2022.parquet",          sys = "SINAN-ZIKA"),
    list(label = "Chagas",        arq = "sinan/SINAN_CHAGAS_RO_2022.parquet",        sys = "SINAN-CHAGAS"),
    list(label = "Leish.Visceral",arq = "sinan/SINAN_LV_RO_2022.parquet",            sys = "SINAN-LEISHMANIOSE-VISCERAL"),
    list(label = "Leish.Tegum.",  arq = "sinan/SINAN_LT_RO_2022.parquet",            sys = "SINAN-LEISHMANIOSE-TEGUMENTAR"),
    list(label = "Leptospirose",  arq = "sinan/SINAN_LEPTOSPIROSE_RO_2022.parquet",  sys = "SINAN-LEPTOSPIROSE"),
    list(label = "Malária (BR)",  arq = "sinan/SINAN_MALARIA_BR_2022.parquet",       sys = "SINAN-MALARIA")
  )

  for (caso in sinan_casos) {
    cli::cli_h1("SINAN-{caso$label} | Rondônia 2022")

    df_raw <- carregar(caso$arq, caso$sys)
    df_std <- rt(
      sprintf("SINAN-%s | clean+stand", caso$label), caso$sys, "stand",
      df_raw |>
        sus_data_clean_encoding(lang = "pt", verbose = FALSE) |>
        sus_data_standardize(lang = "pt", verbose = FALSE)
    )

    rs(sprintf("SINAN-%s | filter_cid", caso$label), caso$sys,
       "SINAN usa ID_AGRAVO (código de agravo), não CID-10")

    df_vars <- rt(
      sprintf("SINAN-%s | create_vars", caso$label), caso$sys, "derive",
      df_std |> sus_data_create_variables(
        create_age_groups    = TRUE,
        create_calendar_vars = TRUE,
        lang = "pt", verbose = FALSE)
    )

    rt(sprintf("SINAN-%s | filter_demo (feminino)", caso$label), caso$sys, "filter_demo",
      (df_vars %||% df_std) |>
        sus_data_filter_demographics(sex = "F", lang = "pt", verbose = FALSE))

    rt(sprintf("SINAN-%s | aggregate (semana)", caso$label), caso$sys, "aggregate",
      (df_vars %||% df_std) |>
        sus_data_aggregate(time_unit = "week", lang = "pt", verbose = FALSE))
  }
}


# ==============================================================================
# SIA — Sistema de Informações Ambulatoriais
# ==============================================================================
if (GROUP %in% c("ALL", "SIA")) {

  sia_casos <- list(
    list(label = "PA",  arq = "sia/SIA_PA_RO_2022_01.parquet",  sys = "SIA-PA",
         cid = "J", grupo = "respiratório"),
    list(label = "AM",  arq = "sia/SIA_AM_RO_2022_01.parquet",  sys = "SIA-AM",
         cid = "F", grupo = "transtornos mentais"),
    list(label = "AQ",  arq = "sia/SIA_AQ_RO_2022_01.parquet",  sys = "SIA-AQ",
         cid = "K", grupo = "digestivo"),
    list(label = "PS",  arq = "sia/SIA_PS_RO_2022_01.parquet",  sys = "SIA-PS",
         cid = "F", grupo = "saúde mental"),
    list(label = "AD",  arq = "sia/SIA_AD_RO_2022_01.parquet",  sys = "SIA-AD",
         cid = "C", grupo = "neoplasias"),
    list(label = "AR",  arq = "sia/SIA_AR_RO_2022_01.parquet",  sys = "SIA-AR",
         cid = "I", grupo = "cardiovascular")
  )

  for (caso in sia_casos) {
    cli::cli_h1("SIA-{caso$label} | Rondônia jan/2022")

    df_raw <- carregar(caso$arq, caso$sys)
    df_std <- rt(
      sprintf("SIA-%s | clean+stand", caso$label), caso$sys, "stand",
      df_raw |>
        sus_data_clean_encoding(lang = "pt", verbose = FALSE) |>
        sus_data_standardize(lang = "pt", verbose = FALSE)
    )

    rt(sprintf("SIA-%s | filter_cid (%s)", caso$label, caso$grupo),
       caso$sys, "filter_cid",
       df_std |> sus_data_filter_cid(
         icd_codes = caso$cid, lang = "pt", verbose = FALSE))

    df_vars <- rt(
      sprintf("SIA-%s | create_vars", caso$label), caso$sys, "derive",
      df_std |> sus_data_create_variables(
        create_age_groups    = TRUE,
        create_calendar_vars = TRUE,
        lang = "pt", verbose = FALSE)
    )

    rt(sprintf("SIA-%s | filter_demo (masculino)", caso$label),
       caso$sys, "filter_demo",
       (df_vars %||% df_std) |>
         sus_data_filter_demographics(sex = "M", lang = "pt", verbose = FALSE))

    rt(sprintf("SIA-%s | aggregate (mês)", caso$label), caso$sys, "aggregate",
      (df_vars %||% df_std) |>
        sus_data_aggregate(time_unit = "month", lang = "pt", verbose = FALSE))
  }
}


# ==============================================================================
# CNES — Cadastro Nacional de Estabelecimentos de Saúde
# NOTA: CNES é um cadastro de instalações, não registros de pacientes.
#       filter_cid, create_vars e filter_demographics não se aplicam.
# ==============================================================================
if (GROUP %in% c("ALL", "CNES")) {

  cnes_casos <- list(
    list(label = "ST",  arq = "cnes/CNES_ST_RO_2022_01.parquet",  sys = "CNES-ST",
         descricao = "Estabelecimentos de Saúde"),
    list(label = "PF",  arq = "cnes/CNES_PF_RO_2022_01.parquet",  sys = "CNES-PF",
         descricao = "Profissionais de Saúde"),
    list(label = "EQ",  arq = "cnes/CNES_EQ_RO_2022_01.parquet",  sys = "CNES-EQ",
         descricao = "Equipamentos"),
    list(label = "SR",  arq = "cnes/CNES_SR_RO_2022_01.parquet",  sys = "CNES-SR",
         descricao = "Serviços Especializados"),
    list(label = "LT",  arq = "cnes/CNES_LT_RO_2022_01.parquet",  sys = "CNES-LT",
         descricao = "Leitos"),
    list(label = "EP",  arq = "cnes/CNES_EP_RO_2022_01.parquet",  sys = "CNES-EP",
         descricao = "Equipes"),
    list(label = "HB",  arq = "cnes/CNES_HB_RO_2022_01.parquet",  sys = "CNES-HB",
         descricao = "Habilitações"),
    list(label = "DC",  arq = "cnes/CNES_DC_RO_2022_01.parquet",  sys = "CNES-DC",
         descricao = "Dados Complementares"),
    list(label = "IN",  arq = "cnes/CNES_IN_RO_2022_01.parquet",  sys = "CNES-IN",
         descricao = "Incentivos"),
    list(label = "RC",  arq = "cnes/CNES_RC_RO_2022_01.parquet",  sys = "CNES-RC",
         descricao = "Regras Contratuais"),
    list(label = "EF",  arq = "cnes/CNES_EF_RO_2022_01.parquet",  sys = "CNES-EF",
         descricao = "Estrutura Física"),
    list(label = "GM",  arq = "cnes/CNES_GM_RO_2022_01.parquet",  sys = "CNES-GM",
         descricao = "Gestão e Metas")
  )

  motivo_skip_cnes <- "Cadastro de instalações — sem ICD-10 / sem dados de pacientes"

  for (caso in cnes_casos) {
    cli::cli_h1("CNES-{caso$label} | {caso$descricao} — Rondônia jan/2022")

    df_raw <- carregar(caso$arq, caso$sys)
    df_std <- rt(
      sprintf("CNES-%s | clean+stand", caso$label), caso$sys, "stand",
      df_raw |>
        sus_data_clean_encoding(lang = "pt", verbose = FALSE) |>
        sus_data_standardize(lang = "pt", verbose = FALSE)
    )

    rs(sprintf("CNES-%s | filter_cid",  caso$label), caso$sys, motivo_skip_cnes)
    rs(sprintf("CNES-%s | create_vars", caso$label), caso$sys, motivo_skip_cnes)
    rs(sprintf("CNES-%s | filter_demo", caso$label), caso$sys, motivo_skip_cnes)

    rt(sprintf("CNES-%s | aggregate (mês)", caso$label), caso$sys, "aggregate",
      df_std |>
        sus_data_aggregate(time_unit = "month", lang = "pt", verbose = FALSE))
  }
}


# ==============================================================================
# SINASC — Sistema de Informações sobre Nascidos Vivos
# ==============================================================================
if (GROUP %in% c("ALL", "SINASC")) {
  cli::cli_h1("SINASC | Nascidos Vivos — Rondônia 2022")
  # Cenário: analista quer avaliar anomalias congênitas por sexo do recém-nascido

  df_nasc_raw <- carregar("sinasc/SINASC_RO_2022.parquet", "SINASC")
  df_nasc_clean <- rt("SINASC | clean",      "SINASC", "clean",
    df_nasc_raw |> sus_data_clean_encoding(lang = "pt", verbose = FALSE))

  df_nasc_std <- rt("SINASC | standardize",  "SINASC", "stand",
    df_nasc_clean |> sus_data_standardize(lang = "pt", verbose = FALSE))

  rt("SINASC | filter_cid (malformações Q)", "SINASC", "filter_cid",
    df_nasc_std |> sus_data_filter_cid(
      icd_codes = "Q", lang = "pt", verbose = FALSE))

  df_nasc_vars <- rt("SINASC | create_vars", "SINASC", "derive",
    df_nasc_std |> sus_data_create_variables(
      create_age_groups    = TRUE,
      age_breaks           = c(0, 20, 25, 30, 35, Inf),
      create_calendar_vars = TRUE,
      lang = "pt", verbose = FALSE))

  rt("SINASC | filter_demo (sexo feminino)", "SINASC", "filter_demo",
    (df_nasc_vars %||% df_nasc_std) |>
      sus_data_filter_demographics(sex = "F", lang = "pt", verbose = FALSE))

  rt("SINASC | aggregate (mês)", "SINASC", "aggregate",
    (df_nasc_vars %||% df_nasc_std) |>
      sus_data_aggregate(time_unit = "month", lang = "pt", verbose = FALSE))
}


# ==============================================================================
# Relatório Final
# ==============================================================================
cli::cli_rule()
cli::cli_h1("RELATÓRIO FINAL")
cli::cli_alert_success("Passou  (PASS): {pass}")
cli::cli_alert_info(   "Pulado  (SKIP): {skip_n}")
cli::cli_alert_danger( "Falhou  (FAIL): {fail}")
cli::cli_text("Total de testes: {pass + skip_n + fail}")
cli::cli_text("Taxa de sucesso: {round(pass / max(pass + fail, 1) * 100)}% (excluindo SKIPs)")
cli::cli_rule()

report_df <- do.call(rbind, lapply(results, function(x) {
  as.data.frame(x[c("sys", "stage", "result", "label", "rows", "error")],
                stringsAsFactors = FALSE)
}))

# Tabela resumo por sistema
resumo <- tapply(report_df$result, report_df$sys, function(x) {
  paste0(
    sum(x == "PASS"), "P/",
    sum(x == "FAIL"), "F/",
    sum(x == "SKIP"), "S"
  )
})
cli::cli_h2("Por sistema")
for (s in sort(names(resumo))) {
  cli::cli_text("  {s}: {resumo[[s]]}")
}

# Exibe apenas falhas e SKIPs
falhas <- report_df[report_df$result == "FAIL", ]
if (nrow(falhas) > 0) {
  cli::cli_h2("Falhas (FAIL)")
  print(falhas[, c("result", "sys", "stage", "label", "error")])
}

# Salva CSV
out_csv <- file.path(BASE, "test_results_v2.csv")
utils::write.csv(report_df, out_csv, row.names = FALSE, quote = TRUE)
cli::cli_alert_success("Resultados salvos: {out_csv}")

invisible(report_df)
