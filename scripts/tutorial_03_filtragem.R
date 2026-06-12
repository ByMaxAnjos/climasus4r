# =============================================================================
# tutorial_03_filtragem.R
# -----------------------------------------------------------------------------
# PROPOSITO
#   Gerar UMA VEZ, localmente, todas as figuras e tabelas canonicas usadas pelo
#   Tutorial 3 (Filtragem) da vignette climasus4r. A etapa de FILTRAGEM recorta
#   os microdados ja padronizados por DOENCA (CID-10) e por DEMOGRAFIA (sexo,
#   raca/cor, faixa etaria, regiao, municipio). A analise pesada roda AQUI; o
#   arquivo .Rmd apenas carrega os artefatos prontos (PNG via include_graphics,
#   RDS via readRDS).
#
# COMO RODAR
#   Rode a partir da RAIZ do pacote (pasta climasus4r/):
#       Rscript scripts/tutorial_03_filtragem.R
#   ou, em sessao interativa com o working directory na raiz:
#       source("scripts/tutorial_03_filtragem.R")
#
# ARTEFATOS GERADOS  (prefixo "filt_")
#   Figuras (vignettes-pt/figuras/):
#     - filt_funil_cid.png        funil: registros antes/depois do filtro CID-10
#     - filt_resp_vs_cardio.png   obitos respiratorios (J) vs cardiovasculares (I)
#     - filt_piramide_idade.png   piramide etaria antes/depois do recorte 60+
#     - filt_serie_idosos.png     serie mensal de obitos respiratorios em idosos
#   Tabelas (vignettes-pt/dados/):
#     - filt_resumo.rds           resumo de registros retidos por etapa de filtro
#     - filt_grupos_doenca.rds    grupos de doenca usados (CID-10) e n. de codigos
#   Encadeamento (caso condutor):
#     - caso_filtrado.rds         objeto recortado, entrada do Tutorial 4
#
# ENTRADA  : vignettes-pt/dados/caso_padronizado.rds   (gerado pelo Tutorial 2)
# SAIDA    : vignettes-pt/dados/caso_filtrado.rds       (entrada do Tutorial 4)
#
# CASO CONDUTOR (atravessa os 9 tutoriais)
#   "Temperatura e desfechos de saude em idosos (60+) na Regiao Metropolitana de
#   Sao Paulo, 2014-2019". Neste tutorial RECORTAMOS as causas respiratorias
#   (CID-10 J00-J99) e cardiovasculares (I00-I99) e restringimos a populacao
#   idosa (60 anos ou mais). O objeto filtrado alimenta o Tutorial 4 (Criacao de
#   Variaveis e Agregacao).
#
# NOTA SOBRE PRE-RENDERIZACAO
#   Este script e ILUSTRATIVO. Toda chamada a pacotes Suggests ou a rede e
#   protegida com requireNamespace()/tryCatch e degrada graciosamente (mensagem)
#   se o recurso estiver indisponivel. Caso o .rds de entrada nao exista (T2
#   ainda nao rodado), seguimos com um exemplo minimo sintetico apenas para
#   produzir os artefatos didaticos.
# =============================================================================

# --- Infraestrutura compartilhada -------------------------------------------
source("scripts/_setup_tutoriais.R")

COR_PRIMARIA   <- "#2E7D32"
COR_SECUNDARIA <- "#558B2F"
COR_ACENTO     <- "#EF6C00"

ANOS <- 2014:2019


# =============================================================================
# BLOCO 0 - Carregar a entrada encadeada (saida do Tutorial 2)
# =============================================================================
# O Tutorial 2 (Encoding & Padronizacao) salva 'caso_padronizado.rds'. Aqui o
# carregamos com guarda file.exists(). Se ausente, avisamos e seguimos com um
# exemplo minimo sintetico, de modo que os artefatos didaticos ainda sejam
# gerados (o caso real e reproduzido quando o T2 tiver rodado).
message(">> Bloco 0: carregando a base padronizada (saida do Tutorial 2)...")

ENTRADA <- file.path("vignettes-pt", "dados", "caso_padronizado.rds")

# Constroi um SIM-DO padronizado SINTETICO minimo, ja no estagio "stand",
# para que o script rode mesmo sem o artefato do T2. Usa as colunas canonicas
# que sus_data_filter_cid()/sus_data_filter_demographics() detectam:
#   causa_basica (CID-10), age_years (idade), sex, residence_municipality_code.
.exemplo_minimo_sim <- function() {
  set.seed(123)
  n <- 6000L
  # Mistura de capitulos CID-10 para tornar o filtro ilustrativo:
  #   J (respiratorio), I (cardiovascular) e outros capitulos (C, K, A...).
  cap <- sample(
    c("J", "I", "C", "K", "A"),
    size = n, replace = TRUE,
    prob = c(0.22, 0.30, 0.18, 0.18, 0.12)
  )
  num <- sprintf("%02d", sample(0:99, n, replace = TRUE))
  causa <- paste0(cap, num)

  idade <- pmax(0, round(stats::rnorm(n, mean = 62, sd = 22)))
  sexo  <- sample(c("Masculino", "Feminino"), n, replace = TRUE)

  # Municipios da Regiao Metropolitana de Sao Paulo (codigos IBGE 7 digitos).
  munis_rmsp <- c("3550308", "3548708", "3509502", "3548500",
                  "3547809", "3530607", "3534401", "3505708")
  muni <- sample(munis_rmsp, n, replace = TRUE)

  datas <- as.Date("2014-01-01") + sample(0:(6 * 365), n, replace = TRUE)

  df <- dplyr::tibble(
    causa_basica                = causa,
    age_years                   = idade,
    sex                         = sexo,
    residence_municipality_code = muni,
    date_death                  = datas
  )

  # Promove a climasus_df no estagio "stand" (pre-requisito dos filtros).
  if (exists("new_climasus_df", mode = "function")) {
    df <- new_climasus_df(
      df,
      list(system = "SIM", stage = "stand", type = "stand",
           backend = "tibble", history = "[exemplo minimo sintetico]",
           user = list())
    )
  } else {
    # Fallback bruto se o construtor interno nao estiver exportado.
    attr(df, "sus_meta") <- list(system = "SIM", stage = "stand",
                                 type = "stand", backend = "tibble")
    class(df) <- c("climasus_df", class(df))
  }
  df
}

usando_exemplo <- FALSE
if (file.exists(ENTRADA)) {
  caso_padronizado <- readRDS(ENTRADA)
  # O T2 salva uma lista com (ao menos) o componente 'obitos' (SIM-DO).
  obitos_std <- if (is.list(caso_padronizado) && !is.data.frame(caso_padronizado)) {
    caso_padronizado$obitos
  } else {
    caso_padronizado
  }
  if (is.null(obitos_std)) {
    message("   'obitos' ausente no .rds de entrada; usando exemplo minimo.")
    obitos_std <- .exemplo_minimo_sim()
    usando_exemplo <- TRUE
  }
} else {
  message("   AVISO: ", ENTRADA, " nao encontrado (Tutorial 2 ainda nao rodado).")
  message("   Seguindo com um exemplo minimo sintetico para gerar os artefatos.")
  obitos_std <- .exemplo_minimo_sim()
  usando_exemplo <- TRUE
}

n_base <- nrow(dplyr::as_tibble(obitos_std))
message("   Base padronizada de entrada: ", format(n_base, big.mark = "."),
        " registros", if (usando_exemplo) " (EXEMPLO SINTETICO)." else ".")


# =============================================================================
# BLOCO 1 - sus_data_cid_select(): explorar grupos de doenca (sem efeito sobre
#           os dados; apenas descobre nomes de grupos para o filtro)
# =============================================================================
# sus_data_cid_select() abre um explorador interativo (output = "browser") ou
# devolve, de forma invisivel, um data.frame com os grupos disponiveis. Em modo
# de geracao usamos output = "console" para extrair a tabela de grupos sem abrir
# o navegador. Daqui retiramos os nomes "respiratory" e "cardiovascular".
message(">> Bloco 1: explorando grupos de doenca com sus_data_cid_select()...")

grupos_doenca <- tryCatch(
  {
    g <- sus_data_cid_select(lang = "pt", output = "console", verbose = FALSE)
    dplyr::as_tibble(g)
  },
  error = function(e) {
    message("   sus_data_cid_select() indisponivel: ", conditionMessage(e)); NULL
  }
)

# Tabela enxuta dos grupos de interesse do caso condutor (respiratorio e
# cardiovascular), com seus codigos CID-10 e sensibilidade climatica.
filt_grupos_doenca <- if (!is.null(grupos_doenca)) {
  grupos_doenca |>
    dplyr::filter(.data$name %in% c("respiratory", "cardiovascular",
                                    "pneumonia", "ischemic_heart")) |>
    dplyr::transmute(
      Grupo            = .data$name,
      `Codigos CID-10` = .data$icd_codes,
      `Sensivel ao clima` = ifelse(.data$climate_sensitive, "Sim", "Nao"),
      Descricao        = .data$description
    ) |>
    as.data.frame()
} else {
  data.frame(
    Grupo = c("respiratory", "cardiovascular"),
    `Codigos CID-10` = c("J00-J99", "I00-I99"),
    `Sensivel ao clima` = c("Sim", "Sim"),
    Descricao = c("Doencas do sistema respiratorio",
                  "Doencas do coracao e sistema circulatorio"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}
save_tbl(filt_grupos_doenca, "filt_grupos_doenca")


# =============================================================================
# BLOCO 2 - sus_data_filter_cid(): recortar causas respiratorias e cardiovasculares
# =============================================================================
# Aplicamos o filtro por CID-10 de duas formas equivalentes para fins didaticos:
#   (a) por grupo de doenca (disease_group) -> mais legivel;
#   (b) por intervalo de codigos (icd_codes = "J00-J99") -> mais explicito.
# Usamos backend = "tibble" para manipulacao direta e contagem imediata.
message(">> Bloco 2: filtrando por CID-10 (respiratorio e cardiovascular)...")

# (a) Respiratorio (J00-J99) via grupo de doenca.
resp <- tryCatch(
  sus_data_filter_cid(
    df            = obitos_std,
    disease_group = "respiratory",   # equivale a icd_codes = "J00-J99"
    match_type    = "starts_with",
    backend       = "tibble",
    lang          = "pt",
    verbose       = TRUE
  ),
  error = function(e) {
    message("   filtro respiratorio falhou: ", conditionMessage(e)); NULL
  }
)

# (b) Cardiovascular (I00-I99) via intervalo de codigos CID-10.
cardio <- tryCatch(
  sus_data_filter_cid(
    df         = obitos_std,
    icd_codes  = "I00-I99",          # equivale a disease_group = "cardiovascular"
    match_type = "starts_with",
    backend    = "tibble",
    lang       = "pt",
    verbose    = TRUE
  ),
  error = function(e) {
    message("   filtro cardiovascular falhou: ", conditionMessage(e)); NULL
  }
)

n_resp   <- if (!is.null(resp))   nrow(dplyr::as_tibble(resp))   else NA_integer_
n_cardio <- if (!is.null(cardio)) nrow(dplyr::as_tibble(cardio)) else NA_integer_


# =============================================================================
# BLOCO 3 - sus_data_filter_demographics(): restringir a idosos (60+) na RMSP
# =============================================================================
# Encadeamos o recorte demografico sobre o subconjunto respiratorio: faixa
# etaria 60+ (age_range = c(60, Inf)). O argumento age_range exige a coluna
# age_years, criada na padronizacao/criacao de variaveis. Tambem ilustramos o
# recorte espacial por municipio (RMSP) e por sexo.
message(">> Bloco 3: filtrando demografia (idosos 60+, RMSP)...")

# Codigos IBGE (7 digitos) de municipios da Regiao Metropolitana de Sao Paulo.
RMSP <- c("3550308", "3548708", "3509502", "3548500",
          "3547809", "3530607", "3534401", "3505708")

resp_idosos <- tryCatch(
  sus_data_filter_demographics(
    df                = resp,
    age_range         = c(60, Inf),   # idosos: 60 anos ou mais
    municipality_code = RMSP,         # recorte espacial: RMSP (codigos IBGE)
    drop_ignored      = FALSE,        # TRUE removeria sexo/raca ignorados
    backend           = "tibble",
    use_cache         = TRUE,
    lang              = "pt",
    verbose           = TRUE
  ),
  error = function(e) {
    message("   filtro demografico falhou: ", conditionMessage(e)); NULL
  }
)

cardio_idosos <- tryCatch(
  sus_data_filter_demographics(
    df                = cardio,
    age_range         = c(60, Inf),
    municipality_code = RMSP,
    backend           = "tibble",
    use_cache         = TRUE,
    lang              = "pt",
    verbose           = TRUE
  ),
  error = function(e) {
    message("   filtro demografico (cardio) falhou: ", conditionMessage(e)); NULL
  }
)

n_resp_idosos   <- if (!is.null(resp_idosos))   nrow(dplyr::as_tibble(resp_idosos))   else NA_integer_
n_cardio_idosos <- if (!is.null(cardio_idosos)) nrow(dplyr::as_tibble(cardio_idosos)) else NA_integer_


# =============================================================================
# BLOCO 4 - Tabela-resumo: registros retidos a cada etapa de filtro (funil)
# =============================================================================
message(">> Bloco 4: tabela-resumo do funil de filtragem...")

pct <- function(n, base) if (is.na(n) || base == 0) NA_real_ else round(100 * n / base, 1)

filt_resumo <- data.frame(
  Etapa = c(
    "Base padronizada (entrada T2)",
    "Filtro CID-10: respiratorio (J00-J99)",
    "Filtro CID-10: cardiovascular (I00-I99)",
    "+ Demografia: respiratorio 60+ (RMSP)",
    "+ Demografia: cardiovascular 60+ (RMSP)"
  ),
  Registros = c(n_base, n_resp, n_cardio, n_resp_idosos, n_cardio_idosos),
  `% da base` = c(100,
                  pct(n_resp, n_base),
                  pct(n_cardio, n_base),
                  pct(n_resp_idosos, n_base),
                  pct(n_cardio_idosos, n_base)),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
save_tbl(filt_resumo, "filt_resumo")


# =============================================================================
# BLOCO 5 - Figura: funil de filtragem (CID-10)
# =============================================================================
message(">> Bloco 5: figura do funil de filtragem...")

funil <- dplyr::tibble(
  etapa = factor(
    c("Base\npadronizada", "Respiratorio\n(J00-J99)", "Resp. 60+\n(RMSP)"),
    levels = c("Base\npadronizada", "Respiratorio\n(J00-J99)", "Resp. 60+\n(RMSP)")
  ),
  n = c(n_base, n_resp, n_resp_idosos)
) |>
  dplyr::filter(!is.na(.data$n))

p_funil <- ggplot2::ggplot(funil, ggplot2::aes(x = .data$etapa, y = .data$n, fill = .data$etapa)) +
  ggplot2::geom_col(width = 0.65, show.legend = FALSE) +
  ggplot2::geom_text(ggplot2::aes(label = format(.data$n, big.mark = ".")),
                     vjust = -0.4, color = COR_PRIMARIA, fontface = "bold") +
  ggplot2::scale_fill_manual(values = c(COR_SECUNDARIA, COR_PRIMARIA, COR_ACENTO)) +
  ggplot2::labs(
    title    = "Funil de filtragem: do bruto ao recorte de interesse",
    subtitle = "SIM-DO, RMSP, 2014-2019 - causas respiratorias em idosos (60+)",
    x = NULL, y = "Numero de registros"
  ) +
  tema_climasus()

save_fig(p_funil, "filt_funil_cid", width = 8, height = 5)


# =============================================================================
# BLOCO 6 - Figura: respiratorio vs cardiovascular (idosos 60+)
# =============================================================================
message(">> Bloco 6: figura respiratorio vs cardiovascular...")

comparacao <- dplyr::tibble(
  causa = factor(c("Respiratorio\n(J00-J99)", "Cardiovascular\n(I00-I99)"),
                 levels = c("Respiratorio\n(J00-J99)", "Cardiovascular\n(I00-I99)")),
  n = c(n_resp_idosos, n_cardio_idosos)
) |>
  dplyr::filter(!is.na(.data$n))

p_comp <- ggplot2::ggplot(comparacao, ggplot2::aes(x = .data$causa, y = .data$n, fill = .data$causa)) +
  ggplot2::geom_col(width = 0.6, show.legend = FALSE) +
  ggplot2::geom_text(ggplot2::aes(label = format(.data$n, big.mark = ".")),
                     vjust = -0.4, color = COR_PRIMARIA, fontface = "bold") +
  ggplot2::scale_fill_manual(values = c(COR_PRIMARIA, COR_ACENTO)) +
  ggplot2::labs(
    title    = "Obitos em idosos (60+) por grande grupo de causa",
    subtitle = "SIM-DO, RMSP, 2014-2019 - apos filtro CID-10 + demografico",
    x = NULL, y = "Numero de obitos"
  ) +
  tema_climasus()

save_fig(p_comp, "filt_resp_vs_cardio", width = 8, height = 5)


# =============================================================================
# BLOCO 7 - Figura: piramide etaria antes vs depois do recorte 60+
# =============================================================================
# Ilustra o efeito do filtro age_range = c(60, Inf): a distribuicao etaria do
# subconjunto respiratorio (antes) versus o recorte de idosos (depois).
message(">> Bloco 7: figura da distribuicao etaria antes/depois do recorte 60+...")

p_idade <- tryCatch({
  resp_df <- dplyr::as_tibble(resp)
  if (!"age_years" %in% names(resp_df)) stop("coluna age_years ausente")

  base_idade <- resp_df |>
    dplyr::filter(!is.na(.data$age_years)) |>
    dplyr::mutate(
      faixa = cut(.data$age_years,
                  breaks = c(0, 20, 40, 60, 80, Inf),
                  labels = c("0-19", "20-39", "40-59", "60-79", "80+"),
                  right = FALSE),
      grupo = ifelse(.data$age_years >= 60, "Idosos (60+)", "Menores de 60")
    ) |>
    dplyr::count(.data$faixa, .data$grupo, name = "n")

  ggplot2::ggplot(base_idade,
                  ggplot2::aes(x = .data$faixa, y = .data$n, fill = .data$grupo)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(values = c("Idosos (60+)" = COR_PRIMARIA,
                                          "Menores de 60" = "#A5D6A7"),
                               name = NULL) +
    ggplot2::labs(
      title    = "Distribuicao etaria - obitos respiratorios",
      subtitle = "O filtro age_range = c(60, Inf) retem apenas as faixas 60-79 e 80+",
      x = "Faixa etaria (anos)", y = "Numero de obitos"
    ) +
    tema_climasus()
}, error = function(e) {
  message("   piramide etaria pulada: ", conditionMessage(e)); NULL
})

if (!is.null(p_idade)) {
  save_fig(p_idade, "filt_piramide_idade", width = 8, height = 5)
} else {
  message("   AVISO: filt_piramide_idade.png NAO foi gerado.")
}


# =============================================================================
# BLOCO 8 - Figura: serie mensal de obitos respiratorios em idosos (60+)
# =============================================================================
message(">> Bloco 8: figura da serie mensal de obitos respiratorios em idosos...")

p_serie <- tryCatch({
  df <- dplyr::as_tibble(resp_idosos)
  # Detecta uma coluna de data plausivel (padronizada ou bruta).
  col_data <- intersect(c("date_death", "data_obito", "DTOBITO"), names(df))[1]
  if (is.na(col_data)) stop("coluna de data nao encontrada")

  serie <- df
  if (col_data == "DTOBITO") {
    serie <- dplyr::mutate(serie, .data_ev = lubridate::dmy(.data$DTOBITO))
  } else {
    serie <- dplyr::mutate(serie, .data_ev = as.Date(.data[[col_data]]))
  }
  serie <- serie |>
    dplyr::filter(!is.na(.data$.data_ev)) |>
    dplyr::mutate(mes = lubridate::floor_date(.data$.data_ev, "month")) |>
    dplyr::count(.data$mes, name = "obitos")

  ggplot2::ggplot(serie, ggplot2::aes(x = .data$mes, y = .data$obitos)) +
    ggplot2::geom_line(color = COR_PRIMARIA, linewidth = 0.8) +
    ggplot2::geom_point(color = COR_PRIMARIA, size = 1.1) +
    ggplot2::labs(
      title    = "Obitos respiratorios mensais em idosos (60+)",
      subtitle = "SIM-DO, RMSP, 2014-2019 - apos filtragem CID-10 + demografica",
      x = "Mes", y = "Numero de obitos"
    ) +
    tema_climasus()
}, error = function(e) {
  message("   serie mensal pulada: ", conditionMessage(e)); NULL
})

if (!is.null(p_serie)) {
  save_fig(p_serie, "filt_serie_idosos", width = 8, height = 5)
} else {
  message("   AVISO: filt_serie_idosos.png NAO foi gerado.")
}


# =============================================================================
# BLOCO 9 - Encadeamento: salvar a base filtrada para o Tutorial 4
# =============================================================================
# Salvamos os dois recortes de interesse (respiratorio e cardiovascular em
# idosos 60+ na RMSP) para que o Tutorial 4 (Criacao de Variaveis e Agregacao)
# os carregue como ponto de partida.
message(">> Bloco 9: salvando a base filtrada para o proximo tutorial...")

caso_filtrado <- list(
  respiratorio   = if (!is.null(resp_idosos))   dplyr::as_tibble(resp_idosos)   else NULL,
  cardiovascular = if (!is.null(cardio_idosos)) dplyr::as_tibble(cardio_idosos) else NULL,
  meta = list(
    uf            = "SP",
    recorte       = "RMSP (codigos IBGE 7 digitos)",
    anos          = ANOS,
    faixa_etaria  = "60+",
    grupos_cid    = c(respiratorio = "J00-J99", cardiovascular = "I00-I99"),
    exemplo       = usando_exemplo
  )
)
save_tbl(caso_filtrado, "caso_filtrado")

message("== Concluido. Artefatos em vignettes-pt/figuras/ e vignettes-pt/dados/ ==")
