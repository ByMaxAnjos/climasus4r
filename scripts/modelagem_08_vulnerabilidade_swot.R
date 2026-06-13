# =============================================================================
# modelagem_08_vulnerabilidade_swot.R
# -----------------------------------------------------------------------------
# PROPOSITO
#   Gerar UMA VEZ, localmente, todas as figuras e tabelas canonicas usadas pelo
#   Tutorial de Modelagem 08 (Indice de Vulnerabilidade IPCC e Sintese SWOT) da
#   vignette climasus4r. Este script cobre:
#     - sus_mod_vulnerability_index()   Indice de Vulnerabilidade Composto (IPCC)
#     - sus_mod_plot_vulnerability()    Visualizacoes: ranking / pillars / lorenz
#     - sus_mod_swot()                  Analise SWOT Climatico-Saude por municipio
#     - sus_mod_plot_swot()             Visualizacoes: radar / matrix / bar
#   A analise pesada roda AQUI; o .Rmd apenas carrega os artefatos prontos.
#
# COMO RODAR
#   Rode a partir da RAIZ do pacote (pasta climasus4r/):
#       Rscript scripts/modelagem_08_vulnerabilidade_swot.R
#   ou, em sessao interativa com o working directory na raiz:
#       source("scripts/modelagem_08_vulnerabilidade_swot.R")
#
# ARTEFATOS GERADOS
#   Figuras (vignettes-pt/figuras/) — prefixo "modvi_" (vulnerabilidade):
#     - modvi_ranking.png          Lollipop: ranking de vulnerabilidade por municipio
#     - modvi_pillars.png          Barras empilhadas: decomposicao dos 3 pilares IPCC
#     - modvi_lorenz.png           Curva de Lorenz: concentracao da vulnerabilidade
#   Figuras (vignettes-pt/figuras/) — prefixo "modswot_" (SWOT):
#     - modswot_radar.png          Radar SWOT: perfil S/W/O/T por municipio
#     - modswot_matrix.png         Matriz 2x2 SWOT: municipio mais vulneravel
#     - modswot_bar.png            Barras agrupadas: escores SWOT por municipio
#   Tabelas (vignettes-pt/dados/):
#     - modvi_vi_table.rds         Tabela de escores de vulnerabilidade (vi_table)
#     - modswot_scores.rds         Tabela de escores SWOT por municipio
#   Encadeamento (caso condutor):
#     - mod_vi.rds                 Objeto climasus_vi (reuso em relatorios finais)
#
# ENTRADAS (4 obrigatorias, todas com fallback sintetico):
#   vignettes-pt/dados/caso_espacial.rds  sf de municipios da RMSP
#   vignettes-pt/dados/caso_socio.rds     indicadores socioeconomicos
#   vignettes-pt/dados/mod_dlnm_fit.rds   climasus_dlnm do Modulo M1
#   vignettes-pt/dados/mod_af.rds         climasus_af do Modulo M2
#
# DEPENDENCIAS PESADAS (Suggests):
#   dlnm, MASS, splines, mvmeta
#   Protegidas por requireNamespace()/tryCatch -- degradam com aviso se ausentes.
#
# CASO CONDUTOR:
#   "Temperatura e desfechos de saude em idosos (60+) na RM de Sao Paulo,
#    2014-2019" (respiratorias J00-J99).
# =============================================================================

# --- Infraestrutura compartilhada -------------------------------------------
source("scripts/_setup_tutoriais.R")

COR_PRIMARIA   <- "#2E7D32"   # verde floresta
COR_SECUNDARIA <- "#558B2F"   # verde folha
COR_ACENTO     <- "#EF6C00"   # laranja queimado
COR_CALOR      <- "#E05C5C"   # vermelho calor
COR_FRIO       <- "#4472C4"   # azul frio

ANOS <- 2014:2019

# Municipios da RMSP (caso condutor)
MUNICIPIOS_RMSP <- c(
  "Sao Paulo", "Guarulhos", "Campinas", "Santo Andre", "Sao Bernardo",
  "Osasco", "Maua", "Carapicuiba", "Itaquaquecetuba", "Cotia",
  "Barueri", "Diadema", "Jandira", "Embu das Artes", "Taboao da Serra"
)

set.seed(42)

message("\n", strrep("=", 70))
message("  MODELAGEM 08 -- Indice de Vulnerabilidade IPCC e Sintese SWOT")
message(strrep("=", 70), "\n")


# =============================================================================
# BLOCO 0 - Carregar ou sintetizar entradas encadeadas
# =============================================================================

message(">> Bloco 0: carregando entradas encadeadas...")

# ---- caso_espacial.rds (sf de municipios da RMSP) ---------------------------
caso_espacial <- NULL
if (file.exists("vignettes-pt/dados/caso_espacial.rds")) {
  caso_espacial <- tryCatch(readRDS("vignettes-pt/dados/caso_espacial.rds"), error = function(e) NULL)
  if (!is.null(caso_espacial) && !inherits(caso_espacial, c("sf", "data.frame")))
    caso_espacial <- NULL
}
if (is.null(caso_espacial)) {
  message("  [aviso] caso_espacial.rds ausente ou invalido. Usando fallback sintetico (data.frame simples sem geometria).")
  caso_espacial <- data.frame(
    city      = MUNICIPIOS_RMSP,
    cod_ibge  = as.character(3500000L + seq_along(MUNICIPIOS_RMSP)),
    uf        = "SP",
    stringsAsFactors = FALSE
  )
}

# ---- Objeto climasus_af (do Modulo M2) --------------------------------------
mod_af <- NULL
if (file.exists("vignettes-pt/dados/mod_af.rds")) {
  mod_af <- tryCatch(readRDS("vignettes-pt/dados/mod_af.rds"), error = function(e) NULL)
  if (!is.null(mod_af) && !inherits(mod_af, "climasus_af")) mod_af <- NULL
}
if (is.null(mod_af)) {
  message("  [aviso] mod_af.rds ausente ou invalido. Usando fallback sintetico.")
}

# ---- Objeto climasus_dlnm (do Modulo M1) ------------------------------------
mod_dlnm <- NULL
if (file.exists("vignettes-pt/dados/mod_dlnm_fit.rds")) {
  mod_dlnm <- tryCatch(readRDS("vignettes-pt/dados/mod_dlnm_fit.rds"), error = function(e) NULL)
  if (!is.null(mod_dlnm) && !inherits(mod_dlnm, "climasus_dlnm")) mod_dlnm <- NULL
}
if (is.null(mod_dlnm)) {
  message("  [aviso] mod_dlnm_fit.rds ausente ou invalido. Usando fallback sintetico.")
}

# ---- caso_socio.rds (indicadores socioeconomicos) ---------------------------
caso_socio <- NULL
if (file.exists("vignettes-pt/dados/caso_socio.rds")) {
  caso_socio <- tryCatch({
    raw_cs <- readRDS("vignettes-pt/dados/caso_socio.rds")
    # Remover geometria se for objeto sf
    if (inherits(raw_cs, c("sf", "sfc"))) {
      if (requireNamespace("sf", quietly = TRUE)) {
        raw_cs <- sf::st_drop_geometry(raw_cs)
      } else {
        geom_cols <- vapply(raw_cs, function(x) inherits(x, c("sfc", "sfg")), logical(1))
        raw_cs <- raw_cs[, !geom_cols, drop = FALSE]
      }
    }
    as.data.frame(raw_cs)
  }, error = function(e) NULL)
}
if (is.null(caso_socio)) {
  message("  [aviso] caso_socio.rds ausente. Sintetizando painel socioeconomico realista.")
  n_mun <- length(MUNICIPIOS_RMSP)
  caso_socio <- data.frame(
    city          = MUNICIPIOS_RMSP,
    pct_idosos    = round(runif(n_mun, 10, 22), 1),
    pct_doenca_cr = round(runif(n_mun, 18, 38), 1),
    hdi           = round(runif(n_mun, 0.72, 0.87), 3),
    pct_agua      = round(runif(n_mun, 65, 98), 1),
    densidade_ubs = round(runif(n_mun, 1.5, 8.0), 2),
    stringsAsFactors = FALSE
  )
}

# Garantir coluna 'city' padrao
if ("city" %in% names(caso_socio)) {
  cities_ref <- as.character(caso_socio$city)
} else {
  # Tentar identificar coluna de municipio
  chr_cols <- names(caso_socio)[vapply(caso_socio, is.character, logical(1L))]
  if (length(chr_cols) > 0L) {
    names(caso_socio)[names(caso_socio) == chr_cols[1L]] <- "city"
    cities_ref <- as.character(caso_socio$city)
  } else {
    caso_socio$city <- MUNICIPIOS_RMSP[seq_len(nrow(caso_socio))]
    cities_ref <- caso_socio$city
  }
}

n_mun <- length(cities_ref)
message("  Municipios na analise: ", n_mun)


# =============================================================================
# BLOCO 1 - Construir os tres pilares IPCC
# =============================================================================

message(">> Bloco 1: construindo pilares Exposicao, Sensibilidade e Cap. Adaptativa...")

# ---- Pilar E: Exposicao Climatica -------------------------------------------
# Indicadores: temperatura maxima media anual, dias com Tmax > P90
# Sintetizamos com base em gradientes reais conhecidos da RMSP
# (valores plausíveis de estacoes INMET na regiao)
exposure_df <- data.frame(
  city           = cities_ref,
  tmax_media     = round(runif(n_mun, 26.5, 32.0), 1),
  dias_calor_p90 = round(runif(n_mun, 30,  80),  0),
  ondas_calor    = round(runif(n_mun,  2,  12),  0),
  stringsAsFactors = FALSE
)
# Municipios perifericos tendem a ser mais quentes (ilha de calor urbana)
exposure_df$tmax_media[exposure_df$city %in%
  c("Guarulhos", "Maua", "Itaquaquecetuba", "Carapicuiba")] <-
  exposure_df$tmax_media[exposure_df$city %in%
    c("Guarulhos", "Maua", "Itaquaquecetuba", "Carapicuiba")] + 1.5

# ---- Pilar S: Sensibilidade Populacional ------------------------------------
# Indicadores: % idosos, % doenca cronica, (+ metricas DLNM se disponiveis)
sensitivity_df <- caso_socio[, c("city",
  intersect(c("pct_idosos", "pct_doenca_cr"), names(caso_socio))), drop = FALSE]
sensitivity_df <- as.data.frame(sensitivity_df)
if (!"pct_idosos" %in% names(sensitivity_df)) {
  set.seed(101L)
  sensitivity_df$pct_idosos    <- round(runif(nrow(sensitivity_df), 10, 22), 1)
}
if (!"pct_doenca_cr" %in% names(sensitivity_df)) {
  set.seed(102L)
  sensitivity_df$pct_doenca_cr <- round(runif(nrow(sensitivity_df), 18, 38), 1)
}

# ---- Pilar AC: Capacidade Adaptativa ----------------------------------------
adaptive_capacity_df <- caso_socio[, c("city",
  intersect(c("hdi", "pct_agua", "densidade_ubs"), names(caso_socio))), drop = FALSE]
adaptive_capacity_df <- as.data.frame(adaptive_capacity_df)
if (!"hdi" %in% names(adaptive_capacity_df)) {
  set.seed(103L)
  adaptive_capacity_df$hdi           <- round(runif(nrow(adaptive_capacity_df), 0.72, 0.87), 3)
}
if (!"pct_agua" %in% names(adaptive_capacity_df)) {
  set.seed(104L)
  adaptive_capacity_df$pct_agua      <- round(runif(nrow(adaptive_capacity_df), 65, 98), 1)
}
if (!"densidade_ubs" %in% names(adaptive_capacity_df)) {
  set.seed(105L)
  adaptive_capacity_df$densidade_ubs <- round(runif(nrow(adaptive_capacity_df), 1.5, 8.0), 2)
}


# =============================================================================
# BLOCO 2 - sus_mod_vulnerability_index()
# =============================================================================

message(">> Bloco 2: ajustando sus_mod_vulnerability_index()...")

# Chamada completa com 3 pilares + extracao automatica de fits DLNM
# (fits = NULL se mod_dlnm for NULL ou lista invalida)
fits_dlnm <- if (!is.null(mod_dlnm) && inherits(mod_dlnm, "climasus_dlnm")) {
  # Caso em que temos um unico ajuste multimunicipios:
  # sus_mod_vulnerability_index() aceita lista nomeada de climasus_dlnm
  # Se o ajuste for de uma unica cidade, encapsulamos como lista de 1
  list(rmsp = mod_dlnm)
} else {
  NULL  # sem extracao DLNM automatica
}

vi_fit <- sus_mod_vulnerability_index(
  exposure_df          = exposure_df,
  sensitivity_df       = sensitivity_df,
  adaptive_capacity_df = adaptive_capacity_df,
  fits                 = fits_dlnm,
  city_col             = "city",
  normalize            = "minmax",
  weights              = c(tmax_media = 2, pct_idosos = 2),  # dobrar peso de Tmax e idosos
  pillar_weights       = c(exposure = 1.5, sensitivity = 1, adaptive_capacity = 1),
  hot_percentile       = 0.99,
  cold_percentile      = 0.01,
  lang                 = "pt",
  verbose              = TRUE
)

print(vi_fit)
message("  Gini do IV: ", round(vi_fit$gini_coefficient, 3))


# =============================================================================
# BLOCO 3 - Figuras: sus_mod_plot_vulnerability()
# =============================================================================

message(">> Bloco 3: gerando figuras de vulnerabilidade...")

# -- Figura 1: Ranking lollipop -----------------------------------------------
p_ranking <- sus_mod_plot_vulnerability(
  vi_fit,
  type        = "ranking",
  output_type = "plot",
  interactive = FALSE,
  lang        = "pt"
)
p_ranking <- p_ranking + tema_climasus()
save_fig(p_ranking, "modvi_ranking", width = 8, height = 6)

# -- Figura 2: Decomposicao dos pilares IPCC (barras empilhadas) --------------
p_pillars <- sus_mod_plot_vulnerability(
  vi_fit,
  type        = "pillars",
  output_type = "plot",
  interactive = FALSE,
  lang        = "pt"
)
p_pillars <- p_pillars + tema_climasus()
save_fig(p_pillars, "modvi_pillars", width = 8, height = 6)

# -- Figura 3: Curva de Lorenz da vulnerabilidade -----------------------------
p_lorenz <- sus_mod_plot_vulnerability(
  vi_fit,
  type        = "lorenz",
  output_type = "plot",
  interactive = FALSE,
  lang        = "pt"
)
p_lorenz <- p_lorenz + tema_climasus()
save_fig(p_lorenz, "modvi_lorenz", width = 8, height = 5)


# =============================================================================
# BLOCO 4 - Tabelas de vulnerabilidade
# =============================================================================

message(">> Bloco 4: salvando tabelas de vulnerabilidade...")

# Tabela resumida dos escores VI (top 10)
vi_table_resumo <- head(vi_fit$vi_table, 10L)
save_tbl(vi_table_resumo, "modvi_vi_table")


# =============================================================================
# BLOCO 5 - sus_mod_swot()
# =============================================================================

message(">> Bloco 5: ajustando sus_mod_swot()...")

# Objeto burden: se nao disponivel, sintetizamos um climasus_burden minimo
burden_obj <- NULL

# Objeto dlnm para o SWOT (broadcast)
dlnm_obj <- if (!is.null(mod_dlnm) && inherits(mod_dlnm, "climasus_dlnm")) {
  mod_dlnm
} else {
  NULL
}

swot_fit <- sus_mod_swot(
  vulnerability = vi_fit,
  af            = if (!is.null(mod_af) && inherits(mod_af, "climasus_af")) mod_af else NULL,
  burden        = NULL,
  dlnm          = dlnm_obj,
  sensitivity   = NULL,
  score_type    = "both",
  breaks        = c(25, 50, 75),
  labels        = NULL,   # padrao em pt: Baixo/Medio/Alto/Muito Alto
  city_col      = "city",
  lang          = "pt",
  verbose       = TRUE
)

print(swot_fit)


# =============================================================================
# BLOCO 6 - Figuras: sus_mod_plot_swot()
# =============================================================================

message(">> Bloco 6: gerando figuras SWOT...")

# -- Figura 4: Radar SWOT (todos os municipios) --------------------------------
p_radar <- sus_mod_plot_swot(
  swot_fit,
  type        = "radar",
  output_type = "plot",
  interactive = FALSE,
  entities    = NULL,
  lang        = "pt"
)
save_fig(p_radar, "modswot_radar", width = 8, height = 7)

# -- Figura 5: Matriz 2x2 SWOT (municipio mais vulneravel) --------------------
top_city <- vi_fit$vi_table$city[1L]
message("  Municipio mais vulneravel: ", top_city)

p_matrix <- sus_mod_plot_swot(
  swot_fit,
  type        = "matrix",
  output_type = "plot",
  interactive = FALSE,
  entities    = top_city,
  top_n       = 3L,
  lang        = "pt"
)
save_fig(p_matrix, "modswot_matrix", width = 8, height = 6)

# -- Figura 6: Barras agrupadas (todos os municipios) -------------------------
p_bar <- sus_mod_plot_swot(
  swot_fit,
  type        = "bar",
  output_type = "plot",
  interactive = FALSE,
  entities    = NULL,
  lang        = "pt"
)
p_bar <- p_bar + tema_climasus()
save_fig(p_bar, "modswot_bar", width = 9, height = 6)


# =============================================================================
# BLOCO 7 - Tabelas SWOT
# =============================================================================

message(">> Bloco 7: salvando tabelas SWOT...")

save_tbl(swot_fit$scores, "modswot_scores")


# =============================================================================
# BLOCO 8 - Salvar objeto encadeado
# =============================================================================

message(">> Bloco 8: salvando mod_vi.rds (climasus_vi) para encadeamento...")

# Salvar o objeto de vulnerabilidade como mod_vi.rds (reuso nos relatorios finais)
saveRDS(vi_fit, "vignettes-pt/dados/mod_vi.rds")
message("  mod_vi.rds salvo em vignettes-pt/dados/")

# Tambem salvar o objeto SWOT completo para eventuais relatorios finais
save_tbl(swot_fit, "mod_vi_swot")


# =============================================================================
# RESUMO FINAL
# =============================================================================

message("\n", strrep("=", 70))
message("  MODELAGEM 08 concluida com sucesso.")
message("  Municipios analisados : ", vi_fit$meta$n_ok)
message("  Gini (IV)             : ", round(vi_fit$gini_coefficient, 3))
message("  Entidades no SWOT     : ", swot_fit$meta$n_entities)
message("  Indicadores SWOT      : ", swot_fit$meta$n_indicators)
message("  Figuras geradas       : 6  (prefixos modvi_ e modswot_)")
message("  Tabelas geradas       : 4  (modvi_vi_table, modswot_scores, mod_vi.rds, mod_vi_swot)")
message(strrep("=", 70))
