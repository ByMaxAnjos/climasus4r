# =============================================================================
# climasusDB - Gold tier, item 1: contagens diarias municipais de OBITOS
# (SIM-DO) e INTERNACOES (SIH-RD) por grupo de doenca sensivel ao clima
# =============================================================================
# O QUE FAZ
#   Para cada UF x ano roda o pipeline climasus4r
#     sus_data_import -> sus_data_clean_encoding -> sus_data_standardize
#     -> sus_data_create_variables -> sus_data_filter_cid (por grupo)
#     -> sus_data_aggregate (dia x municipio x faixa etaria x sexo)
#   e grava Parquet particionado (hive) pela UF de RESIDENCIA (codigo IBGE de
#   2 digitos): sistema=/uf=/ano=/src-<UF do arquivo>.parquet, com a
#   proveniencia (sus_meta) embutida no schema Arrow. Arquivos SIH sao por UF
#   do hospital: um residente de MG internado em SP vem do arquivo de SP e cai
#   em uf=31. Ao ler uma particao, some n pelas chaves (varios src-*).
#
# CONTRATO DE DADOS (uma linha por celula com n > 0)
#   code_muni   chr  IBGE 6 digitos, municipio de RESIDENCIA
#   date        Date data do obito (SIM) ou da internacao (SIH)
#   grupo       chr  grupo de doenca (ver GRUPOS); grupos SE SOBREPOEM
#                    (capitulo contem subgrupos) -> NAO somar entre grupos
#   age_group, sexo  estratos; ausencia de linha = 0 eventos
#   n           int  contagem
#
# COMO RODAR (da raiz do pacote)
#   Rscript scripts/climasusDB_gold_saude.R
# =============================================================================

suppressMessages(devtools::load_all(quiet = TRUE))

UFS     <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG",
             "PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")
ANOS    <- 2000:2025
WORKERS <- 1L               # serial: com 3 workers o FTP do DATASUS falhou ~1/3 das vezes
OUT_DIR <- path.expand("~/climasusDB/gold/saude")   # fora do pacote (data/ e reservado em pacotes R)
LANG    <- "pt"

# Capitulos (nivel 1) + subgrupos (nivel 2) ja definidos em .icd_disease_groups
GRUPOS <- c(
  todas_causas = NA,                       # denominador (sem filtro CID)
  cardiovascular = "cardiovascular",       # I00-I99
  ischemic_heart = "ischemic_heart",       # I20-I25
  cerebrovascular = "cerebrovascular",     # I60-I69
  respiratory = "respiratory",             # J00-J99
  influenza_pneumonia = "influenza_pneumonia",
  copd = "copd", asthma = "asthma",
  neurological_disorders = "neurological_disorders",  # G00-G99
  renal = "renal",                         # N00-N29
  diabetes = "diabetes",
  heat_related = "heat_related",           # E86, T67, X30
  diarrheal = "diarrheal",                 # A00-A09
  vector_borne = "vector_borne",
  leptospirosis = "leptospirosis",
  mental_disorders = "mental_disorders"
)

# Faixas voltadas a calor-saude (65+ e 75+ separados)
FAIXAS <- c(0, 5, 15, 45, 65, 75, Inf)
ROTULOS <- c("0-4", "5-14", "15-44", "45-64", "65-74", "75+")

# --- SIH: arquivos sao por COMPETENCIA (mes de processamento), nao por data de
# internacao. Para cobrir as internacoes do ano Y lemos Y e jan-jun de Y+1 e
# filtramos pela data de internacao. Mantem so AIH principal (IDENT == 1) e
# remove N_AIH duplicado (reapresentacoes).
importar <- function(uf, ano, sistema) {
  if (sistema == "SIM-DO") {
    return(sus_data_import(uf = uf, year = ano, system = sistema,
                           backend = "tibble", filter_residence_uf = FALSE, parallel = FALSE,
                           workers = WORKERS, lang = LANG, verbose = FALSE))
  }
  partes <- list(
    sus_data_import(uf = uf, year = ano, month = 1:12, system = sistema,
                    backend = "tibble", filter_residence_uf = FALSE, parallel = FALSE, workers = WORKERS,
                    lang = LANG, verbose = FALSE),
    tryCatch(sus_data_import(uf = uf, year = ano + 1, month = 1:6, system = sistema,
                             backend = "tibble", filter_residence_uf = FALSE, parallel = FALSE, workers = WORKERS,
                    lang = LANG, verbose = FALSE),
             error = function(e) {
               message("   ", uf, " ", ano + 1, ": competencias seguintes indisponiveis; ",
                       "ultimos meses de ", ano, " ficam sub-registrados")
               NULL
             })
  )
  meta <- attr(partes[[1]], "sus_meta")
  d <- data.table::rbindlist(Filter(Negate(is.null), partes), fill = TRUE)
  d <- d[as.character(IDENT) == "1" &
           substr(as.character(DT_INTER), 1, 4) == as.character(ano)]
  d <- unique(d, by = "N_AIH")
  new_climasus_df(d, meta)
}

processar <- function(uf, ano, sistema) {
  base <- importar(uf, ano, sistema) |>
    sus_data_clean_encoding(backend = "tibble", lang = LANG, verbose = FALSE) |>
    sus_data_standardize(backend = "tibble", lang = LANG, verbose = FALSE) |>
    sus_data_create_variables(age_breaks = FAIXAS, age_labels = ROTULOS,
                              create_calendar_vars = FALSE,
                              create_climate_vars = FALSE,
                              backend = "tibble", lang = LANG, verbose = FALSE)

  por_grupo <- lapply(names(GRUPOS), function(g) {
    d <- if (is.na(GRUPOS[[g]])) base else
      sus_data_filter_cid(base, disease_group = GRUPOS[[g]],
                          backend = "tibble", lang = LANG, verbose = FALSE)
    if (nrow(d) == 0) return(NULL)
    agg <- sus_data_aggregate(d, time_unit = "day", fun = "count",
                              group_by = c("age_group", "sexo"),
                              complete_dates = FALSE,
                              backend = "tibble", lang = LANG, verbose = FALSE)
    agg <- dplyr::as_tibble(agg)
    tibble::tibble(code_muni = substr(as.character(agg$codigo_municipio_residencia), 1, 6),
                   date = agg$date, grupo = g,
                   age_group = as.character(agg$age_group),
                   sexo = as.character(agg$sexo),
                   n = as.integer(agg[[grep("^n_", names(agg))[1]]]))
  })
  regras_sih <- if (sistema == "SIH-RD")
    "; SIH: competencias ano..ano+1/06 filtradas por DT_INTER, IDENT == 1, N_AIH unico" else ""
  sus_meta(new_climasus_df(dplyr::bind_rows(por_grupo), attr(base, "sus_meta")),
           stage = "aggregate", type = "agg",
           add_history = paste0("climasusDB Gold: contagem dia x municipio x faixa x sexo por grupo (",
                                paste(names(GRUPOS), collapse = ","), ")", regras_sih))
}

# Retomavel: um marcador por sistema x UF x ano; falha numa celula nao para o resto.
dir.create(file.path(OUT_DIR, "_done"), recursive = TRUE, showWarnings = FALSE)
falhas <- character(0)
for (sistema in c("SIM-DO", "SIH-RD")) for (uf in UFS) for (ano in ANOS) {
  marca <- file.path(OUT_DIR, "_done", paste(sistema, uf, ano, sep = "_"))
  if (file.exists(marca)) next
  message(format(Sys.time(), "%H:%M:%S"), " >> ", sistema, " ", uf, " ", ano)
  ok <- tryCatch({
    # FTP do DATASUS falha de forma intermitente: ate 3 tentativas
    out <- NULL
    for (tentativa in 1:3) {
      out <- tryCatch(processar(uf, ano, sistema), error = function(e) {
        message("   tentativa ", tentativa, " falhou: ", conditionMessage(e)); NULL
      })
      if (!is.null(out)) break
      Sys.sleep(30 * tentativa)
    }
    if (is.null(out)) stop("sem dados apos 3 tentativas")
    message("   ", sum(out$n[out$grupo == "todas_causas"]), " eventos (todas as causas)")
    for (uf_res in unique(substr(out$code_muni, 1, 2))) {
      destino <- file.path(OUT_DIR, paste0("sistema=", sub("-.*", "", sistema)),
                           paste0("uf=", uf_res), paste0("ano=", ano),
                           paste0("src-", uf, ".parquet"))
      dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
      write_parquet_climasus(out[substr(out$code_muni, 1, 2) == uf_res, ], destino)
    }
    file.create(marca)
    TRUE
  }, error = function(e) {
    message("   FALHOU: ", conditionMessage(e)); FALSE
  })
  if (!ok) falhas <- c(falhas, paste(sistema, uf, ano))
}
message("FIM. Falhas: ", if (length(falhas)) paste(falhas, collapse = "; ") else "nenhuma")
