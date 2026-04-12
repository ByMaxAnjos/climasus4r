#' Visualização e Diagnóstico de Análises Clima-Saúde
#'
#' @description
#' `plot_climate_health()` produz gráficos e tabelas para exploração,
#' diagnóstico e comunicação de resultados de análises de séries temporais
#' clima-saúde. A função aceita diretamente a saída de
#' `sus_climate_aggregate()` e detecta automaticamente a estratégia temporal
#' utilizada a partir dos nomes das colunas agregadas.
#'
#' @param data Um objeto `climasus_df` produzido por `sus_climate_aggregate()`.
#'   Deve conter as colunas `date`, `n_obitos` (ou `outcome_var`) e ao menos
#'   uma variável climática agregada.
#' @param climate_col Character. Nome da coluna climática agregada a usar como
#'   exposição principal (eixo x / variável explicativa). Se `NULL` (padrão),
#'   detecta automaticamente a primeira coluna climática agregada presente.
#' @param outcome_col Character. Nome da coluna de desfecho em saúde.
#'   Padrão: `"n_obitos"`.
#' @param plot_type Character ou vetor. Tipo(s) de gráfico a gerar. Opções:
#'   \describe{
#'     \item{`"timeseries"`}{Série temporal bivariada (exposição + desfecho).}
#'     \item{`"ccf"`}{Lag Cross-Correlation Function (CCF).}
#'     \item{`"heatmap_corr"`}{Heatmap de correlação entre múltiplas defasagens.}
#'     \item{`"distribution"`}{Distribuição das variáveis agregadas (histograma + Q-Q).}
#'     \item{`"scatter"`}{Scatter plot com curva LOESS/GAM.}
#'     \item{`"dlnm_surface"`}{Superfície dose-resposta DLNM (requer `dlnm`).}
#'     \item{`"dlnm_overall"`}{Efeito cumulativo geral DLNM.}
#'     \item{`"dlnm_lag"`}{Efeito por defasagem específica DLNM.}
#'     \item{`"residuals"`}{Gráficos de diagnóstico de resíduos do modelo.}
#'     \item{`"bland_altman"`}{Bland-Altman entre duas estratégias (requer `data2`).}
#'     \item{`"coef_compare"`}{Coeficientes de regressão por estratégia.}
#'     \item{`"corr_matrix"`}{Matriz de correlação entre estratégias.}
#'     \item{`"descriptive_table"`}{Tabela descritiva das variáveis.}
#'     \item{`"correlation_table"`}{Tabela de correlações com o desfecho.}
#'     \item{`"model_comparison"`}{Tabela AIC/BIC de modelos.}
#'     \item{`"rr_table"`}{Tabela de Risco Relativo com IC 95%.}
#'     \item{`"sensitivity_table"`}{Tabela de sensibilidade por `min_obs`.}
#'     \item{`"spatial_map"`}{Mapa de efeitos por município (requer geometria).}
#'     \item{`"cascade"`}{Gráfico de cascata de efeito acumulado por defasagem.}
#'     \item{`"heat_calendar"`}{Calendário de calor — padrões sazonais.}
#'     \item{`"all_exploratory"`}{Conjunto exploratório: timeseries, ccf, scatter, distribution.}
#'     \item{`"all_diagnostic"`}{Conjunto diagnóstico: residuals, distribution, ccf.}
#'   }
#' @param data2 Um segundo `climasus_df` (opcional) para gráficos comparativos
#'   (`bland_altman`, `coef_compare`, `corr_matrix`).
#' @param model Um objeto de modelo ajustado (opcional, classe `lm`, `glm`,
#'   `negbin`, `gam`, ou `crossbasis` do `dlnm`). Necessário para
#'   `dlnm_surface`, `dlnm_overall`, `dlnm_lag`, `residuals`, `rr_table`.
#' @param crossbasis Um objeto `crossbasis` do pacote `dlnm` (opcional).
#'   Necessário para `dlnm_surface`, `dlnm_overall`, `dlnm_lag`.
#' @param group_col Character. Coluna para agrupamento/cor nos gráficos
#'   (e.g., `"abbrev_state"`, `"name_muni"`). Padrão: `NULL`.
#' @param max_lag Integer. Defasagem máxima para CCF e heatmap. Padrão: `21`.
#' @param smooth_method Character. Método de suavização no scatter:
#'   `"loess"` (padrão) ou `"gam"`.
#' @param alpha Numérico (0–1). Nível de significância para intervalos de
#'   confiança. Padrão: `0.05`.
#' @param min_obs_range Vetor numérico. Valores de `min_obs` para análise de
#'   sensibilidade (`sensitivity_table`). Padrão: `c(0.5, 0.6, 0.7, 0.8, 0.9)`.
#' @param palette Character. Paleta de cores para os gráficos.
#'   Padrão: `"climasus"` (azul-laranja-vermelho epidemiológico).
#' @param lang Character. Idioma dos rótulos: `"pt"`, `"en"`, `"es"`.
#'   Padrão: `"pt"`.
#' @param interactive Lógico. Se `TRUE`, usa `plotly` para gráficos
#'   interativos quando disponível. Padrão: `FALSE`.
#' @param save_path Character. Diretório para salvar os gráficos como PNG/PDF.
#'   Se `NULL` (padrão), não salva.
#' @param width Numérico. Largura dos gráficos em polegadas. Padrão: `10`.
#' @param height Numérico. Altura dos gráficos em polegadas. Padrão: `6`.
#' @param dpi Inteiro. Resolução para exportação. Padrão: `300`.
#' @param verbose Lógico. Exibe mensagens de progresso. Padrão: `TRUE`.
#'
#' @return Uma lista nomeada de objetos `ggplot2` (ou `gt` para tabelas).
#'   Cada elemento corresponde a um `plot_type` solicitado. Use `$` para
#'   acessar individualmente: `result$timeseries`, `result$ccf`, etc.
#'
#' @details
#' ## Detecção automática da estratégia temporal
#'
#' A função detecta a estratégia usada pelos prefixos das colunas:
#' - `lag{N}_` → `discrete_lag`
#' - `mvwin{N}_` → `moving_window`
#' - `off{N}to{M}_` → `offset_window`
#' - `{var}_lag{N}` → `distributed_lag`
#' - `gdd{N}_tbase{T}` → `degree_days`
#' - `nexc{N}_` / `pexc{N}_` → `threshold_exceedance`
#' - `ncold{N}_` / `pcold{N}_` → `cold_wave_exceedance`
#' - `wwin{N}_` → `weighted_window`
#' - `season_` → `seasonal`
#' - Coluna original sem prefixo → `exact`
#'
#' ## Fundamentos metodológicos
#'
#' Os gráficos implementados seguem:
#' - **DLNM**: Gasparrini (2011) — superfícies dose-resposta, efeito
#'   cumulativo e por defasagem.
#' - **CCF**: Box & Jenkins (1976) — identificação de defasagens críticas.
#' - **Bland-Altman**: Bland & Altman (1986) — concordância entre métodos.
#' - **Heat Calendar**: Carslaw & Ropkins (2012) — padrões sazonais.
#' - **Cascade Plot**: Gasparrini et al. (2015) — efeito acumulado sequencial.
#'
#' @references
#' Gasparrini, A. (2011). Distributed lag linear and non-linear models in R:
#' the package dlnm. *Journal of Statistical Software*, 43(8), 1-20.
#'
#' Bhaskaran, K., et al. (2013). Time series regression studies in
#' environmental epidemiology. *International Journal of Epidemiology*, 42(4).
#'
#' Lowe, R., et al. (2021). Combined effects of hydrometeorological hazards
#' and urbanisation on dengue risk in Brazil. *Nature Communications*, 12, 337.
#'
#' Box, G.E.P. & Jenkins, G.M. (1976). *Time Series Analysis: Forecasting
#' and Control*. Holden-Day.
#'
#' Bland, J.M. & Altman, D.G. (1986). Statistical methods for assessing
#' agreement between two methods of clinical measurement. *Lancet*, 327(8476).
#'
#' @examples
#' \dontrun{
#' # Saída do sus_climate_aggregate (exact)
#' df_exact <- sus_climate_aggregate(
#'   health_data = df_health, climate_data = df_climate,
#'   climate_var = "tair_dry_bulb_c", temporal_strategy = "exact"
#' )
#'
#' # Conjunto exploratório completo
#' plots <- plot_climate_health(
#'   data        = df_exact,
#'   plot_type   = "all_exploratory",
#'   lang        = "pt"
#' )
#' plots$timeseries
#' plots$ccf
#'
#' # Gráficos específicos com modelo
#' df_dlnm <- sus_climate_aggregate(
#'   health_data = df_health, climate_data = df_climate,
#'   temporal_strategy = "distributed_lag", lag_days = 21
#' )
#' cb <- dlnm::crossbasis(df_dlnm$tair_dry_bulb_c_lag0, lag = 21,
#'                        argvar = list("ns", df = 4),
#'                        arglag = list("ns", df = 3))
#' modelo <- glm(n_obitos ~ cb, data = df_dlnm, family = quasipoisson())
#'
#' plots_dlnm <- plot_climate_health(
#'   data        = df_dlnm,
#'   model       = modelo,
#'   crossbasis  = cb,
#'   plot_type   = c("dlnm_surface", "dlnm_overall", "dlnm_lag"),
#'   lang        = "pt"
#' )
#'
#' # Salvar em disco
#' plot_climate_health(
#'   data       = df_exact,
#'   plot_type  = "all_exploratory",
#'   save_path  = "~/resultados/",
#'   dpi        = 300
#' )
#' }
#'
#' @export
plot_climate_health <- function(
    data,
    climate_col     = NULL,
    outcome_col     = "n_obitos",
    plot_type       = "all_exploratory",
    data2           = NULL,
    model           = NULL,
    crossbasis      = NULL,
    group_col       = NULL,
    max_lag         = 21L,
    smooth_method   = "loess",
    alpha           = 0.05,
    min_obs_range   = c(0.5, 0.6, 0.7, 0.8, 0.9),
    palette         = "climasus",
    lang            = "pt",
    interactive     = FALSE,
    save_path       = NULL,
    width           = 10,
    height          = 6,
    dpi             = 300,
    verbose         = TRUE
) {

  # ---------------------------------------------------------------------------
  # 0. VERIFICAÇÃO DE DEPENDÊNCIAS
  # ---------------------------------------------------------------------------
  .pch_check_deps(c("ggplot2", "dplyr", "tidyr", "scales", "rlang"))

  lbl <- .pch_labels(lang)

  # ---------------------------------------------------------------------------
  # 1. VALIDAÇÃO DO INPUT
  # ---------------------------------------------------------------------------
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} deve ser um data frame ou tibble.")
  }
  if (!outcome_col %in% names(data)) {
    cli::cli_abort(
      "Coluna de desfecho '{outcome_col}' nao encontrada em data. ",
      "Colunas disponiveis: {paste(names(data), collapse = ', ')}."
    )
  }
  if (!"date" %in% names(data)) {
    cli::cli_abort("data deve conter a coluna 'date'.")
  }

  # ---------------------------------------------------------------------------
  # 2. DETECÇÃO AUTOMÁTICA DE COLUNAS CLIMÁTICAS E ESTRATÉGIA
  # ---------------------------------------------------------------------------
  detected       <- .pch_detect_strategy(data)
  strategy       <- detected$strategy
  climate_cols   <- detected$climate_cols
  lag_cols       <- detected$lag_cols      # para distributed_lag

  if (is.null(climate_col)) {
    climate_col <- climate_cols[1]
  }
  if (!climate_col %in% names(data)) {
    cli::cli_abort("Coluna climatica '{climate_col}' nao encontrada em data.")
  }

  if (verbose) {
    cli::cli_alert_info(
      paste0("Estrategia detectada: ", strategy,
             " | Coluna principal: ", climate_col,
             " | Desfecho: ", outcome_col)
    )
  }

  # ---------------------------------------------------------------------------
  # 3. PALETA DE CORES
  # ---------------------------------------------------------------------------
  pal <- .pch_palette(palette)

  # ---------------------------------------------------------------------------
  # 4. EXPANSÃO DE ALIASES
  # ---------------------------------------------------------------------------
  plot_type <- .pch_expand_aliases(plot_type, strategy)

  # ---------------------------------------------------------------------------
  # 5. GERAÇÃO DOS GRÁFICOS
  # ---------------------------------------------------------------------------
  result <- list()

  # ── PARTE 1: EXPLORATÓRIO ──────────────────────────────────────────────────

  # 1.1 Série temporal bivariada
  if ("timeseries" %in% plot_type) {
    if (verbose) cli::cli_alert("Gerando serie temporal bivariada...")
    result$timeseries <- .pch_timeseries(
      data, climate_col, outcome_col, group_col, pal, lbl
    )
  }

  # 1.2 CCF
  if ("ccf" %in% plot_type) {
    if (verbose) cli::cli_alert("Calculando CCF...")
    result$ccf <- .pch_ccf(data, climate_col, outcome_col, max_lag, alpha, pal, lbl)
  }

  # 1.3 Heatmap de correlação por defasagens
  if ("heatmap_corr" %in% plot_type) {
    if (verbose) cli::cli_alert("Gerando heatmap de correlacao...")
    result$heatmap_corr <- .pch_heatmap_corr(
      data, climate_cols, outcome_col, max_lag, pal, lbl
    )
  }

  # 1.4 Distribuição
  if ("distribution" %in% plot_type) {
    if (verbose) cli::cli_alert("Gerando graficos de distribuicao...")
    result$distribution <- .pch_distribution(data, climate_col, outcome_col, pal, lbl)
  }

  # 1.5 Scatter + suavização
  if ("scatter" %in% plot_type) {
    if (verbose) cli::cli_alert("Gerando scatter plot...")
    result$scatter <- .pch_scatter(
      data, climate_col, outcome_col, group_col, smooth_method, alpha, pal, lbl
    )
  }

  # ── PARTE 2: MODELAGEM (DLNM) ─────────────────────────────────────────────

  # 2.1 Superfície DLNM
  if ("dlnm_surface" %in% plot_type) {
    .pch_check_deps("dlnm")
    if (is.null(model) || is.null(crossbasis)) {
      cli::cli_warn("dlnm_surface requer 'model' e 'crossbasis'. Pulando.")
    } else {
      if (verbose) cli::cli_alert("Gerando superficie DLNM...")
      result$dlnm_surface <- .pch_dlnm_surface(
        model, crossbasis, outcome_col, alpha, pal, lbl
      )
    }
  }

  # 2.2 Efeito cumulativo geral (Overall)
  if ("dlnm_overall" %in% plot_type) {
    .pch_check_deps("dlnm")
    if (is.null(model) || is.null(crossbasis)) {
      cli::cli_warn("dlnm_overall requer 'model' e 'crossbasis'. Pulando.")
    } else {
      if (verbose) cli::cli_alert("Gerando curva cumulativa DLNM...")
      result$dlnm_overall <- .pch_dlnm_overall(
        model, crossbasis, alpha, pal, lbl
      )
    }
  }

  # 2.3 Efeito por defasagem específica
  if ("dlnm_lag" %in% plot_type) {
    .pch_check_deps("dlnm")
    if (is.null(model) || is.null(crossbasis)) {
      cli::cli_warn("dlnm_lag requer 'model' e 'crossbasis'. Pulando.")
    } else {
      if (verbose) cli::cli_alert("Gerando curva por defasagem DLNM...")
      result$dlnm_lag <- .pch_dlnm_lag(model, crossbasis, alpha, pal, lbl)
    }
  }

  # 2.4 Diagnóstico de resíduos
  if ("residuals" %in% plot_type) {
    if (is.null(model)) {
      cli::cli_warn("residuals requer 'model'. Pulando.")
    } else {
      if (verbose) cli::cli_alert("Gerando diagnostico de residuos...")
      result$residuals <- .pch_residuals(model, pal, lbl)
    }
  }

  # ── PARTE 3: COMPARATIVO ──────────────────────────────────────────────────

  # 3.1 Bland-Altman
  if ("bland_altman" %in% plot_type) {
    if (is.null(data2)) {
      cli::cli_warn("bland_altman requer 'data2'. Pulando.")
    } else {
      if (verbose) cli::cli_alert("Gerando Bland-Altman...")
      result$bland_altman <- .pch_bland_altman(
        data, data2, climate_col, outcome_col, pal, lbl
      )
    }
  }

  # 3.2 Coeficientes por estratégia
  if ("coef_compare" %in% plot_type) {
    if (verbose) cli::cli_alert("Gerando comparacao de coeficientes...")
    result$coef_compare <- .pch_coef_compare(data, climate_cols, outcome_col, alpha, pal, lbl)
  }

  # 3.3 Matriz de correlação entre estratégias
  if ("corr_matrix" %in% plot_type) {
    if (verbose) cli::cli_alert("Gerando matriz de correlacao entre estrategias...")
    result$corr_matrix <- .pch_corr_matrix(data, climate_cols, pal, lbl)
  }

  # ── PARTE 4: TABELAS ──────────────────────────────────────────────────────

  if ("descriptive_table" %in% plot_type) {
    if (verbose) cli::cli_alert("Gerando tabela descritiva...")
    result$descriptive_table <- .pch_descriptive_table(
      data, climate_cols, outcome_col, lbl
    )
  }

  if ("correlation_table" %in% plot_type) {
    if (verbose) cli::cli_alert("Gerando tabela de correlacoes...")
    result$correlation_table <- .pch_correlation_table(
      data, climate_cols, outcome_col, alpha, lbl
    )
  }

  if ("model_comparison" %in% plot_type) {
    if (verbose) cli::cli_alert("Gerando tabela de comparacao de modelos...")
    result$model_comparison <- .pch_model_comparison(
      data, climate_cols, outcome_col, alpha, lbl
    )
  }

  if ("rr_table" %in% plot_type) {
    if (is.null(model)) {
      cli::cli_warn("rr_table requer 'model'. Pulando.")
    } else {
      if (verbose) cli::cli_alert("Gerando tabela de RR...")
      result$rr_table <- .pch_rr_table(model, alpha, lbl)
    }
  }

  if ("sensitivity_table" %in% plot_type) {
    if (verbose) cli::cli_alert("Gerando tabela de sensibilidade...")
    result$sensitivity_table <- .pch_sensitivity_table(
      data, climate_col, outcome_col, min_obs_range, lbl
    )
  }

  # ── PARTE 5: ESPECIALIZADOS ───────────────────────────────────────────────

  if ("spatial_map" %in% plot_type) {
    if (!inherits(data, "sf") && !"geom" %in% names(data)) {
      cli::cli_warn("spatial_map requer geometria (sf). Pulando.")
    } else {
      .pch_check_deps("sf")
      if (verbose) cli::cli_alert("Gerando mapa espacial...")
      result$spatial_map <- .pch_spatial_map(
        data, climate_col, outcome_col, group_col, model, alpha, pal, lbl
      )
    }
  }

  if ("cascade" %in% plot_type) {
    if (verbose) cli::cli_alert("Gerando cascade plot...")
    result$cascade <- .pch_cascade(
      data, climate_cols, outcome_col, lag_cols, alpha, pal, lbl
    )
  }

  if ("heat_calendar" %in% plot_type) {
    if (verbose) cli::cli_alert("Gerando heat calendar...")
    result$heat_calendar <- .pch_heat_calendar(data, climate_col, outcome_col, pal, lbl)
  }

  # ---------------------------------------------------------------------------
  # 6. EXPORT (OPCIONAL)
  # ---------------------------------------------------------------------------
  if (!is.null(save_path)) {
    .pch_save_plots(result, save_path, width, height, dpi, verbose)
  }

  # ---------------------------------------------------------------------------
  # 7. INTERATIVIDADE (OPCIONAL)
  # ---------------------------------------------------------------------------
  if (interactive && requireNamespace("plotly", quietly = TRUE)) {
    result <- lapply(result, function(p) {
      if (inherits(p, "ggplot")) plotly::ggplotly(p) else p
    })
  }

  if (verbose) {
    cli::cli_alert_success(
      paste0(length(result), " elemento(s) gerado(s): ",
             paste(names(result), collapse = ", "), ".")
    )
  }

  invisible(result)
}


# =============================================================================
# HELPERS INTERNOS
# =============================================================================

# ── Verificação de dependências ───────────────────────────────────────────────
.pch_check_deps <- function(pkgs) {
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    cli::cli_abort(
      paste0("Pacote(s) necessario(s) nao instalado(s): ",
             paste(missing_pkgs, collapse = ", "),
             ".\nInstale com: install.packages(c('",
             paste(missing_pkgs, collapse = "','"), "'))")
    )
  }
}

# ── Labels multilíngues ───────────────────────────────────────────────────────
.pch_labels <- function(lang = "pt") {
  lbl <- list(
    pt = list(
      date = "Data", outcome = "N. de Óbitos", exposure = "Exposição Climática",
      correlation = "Correlação", lag = "Defasagem (dias)", rr = "Risco Relativo (RR)",
      ci95 = "IC 95%", p_value = "Valor p", n = "N", mean = "Média",
      sd = "DP", median = "Mediana", q1 = "Q1", q3 = "Q3",
      min = "Mín.", max = "Máx.", aic = "AIC", bic = "BIC",
      residuals = "Resíduos", fitted = "Valores Ajustados",
      theoretical = "Quantis Teóricos", sample = "Quantis da Amostra",
      difference = "Diferença (A − B)", mean_diff = "Média das Médias",
      strategy = "Estratégia", coef = "Coeficiente (β)", variable = "Variável",
      value = "Valor", proportion = "Proporção", count = "Contagem",
      month = "Mês", year = "Ano", day_of_week = "Dia da Semana",
      sensitivity = "Sensibilidade (min_obs)", season = "Estação"
    ),
    en = list(
      date = "Date", outcome = "Deaths", exposure = "Climate Exposure",
      correlation = "Correlation", lag = "Lag (days)", rr = "Relative Risk (RR)",
      ci95 = "95% CI", p_value = "p-value", n = "N", mean = "Mean",
      sd = "SD", median = "Median", q1 = "Q1", q3 = "Q3",
      min = "Min.", max = "Max.", aic = "AIC", bic = "BIC",
      residuals = "Residuals", fitted = "Fitted Values",
      theoretical = "Theoretical Quantiles", sample = "Sample Quantiles",
      difference = "Difference (A − B)", mean_diff = "Mean of Means",
      strategy = "Strategy", coef = "Coefficient (β)", variable = "Variable",
      value = "Value", proportion = "Proportion", count = "Count",
      month = "Month", year = "Year", day_of_week = "Day of Week",
      sensitivity = "Sensitivity (min_obs)", season = "Season"
    ),
    es = list(
      date = "Fecha", outcome = "Defunciones", exposure = "Exposición Climática",
      correlation = "Correlación", lag = "Rezago (días)", rr = "Riesgo Relativo (RR)",
      ci95 = "IC 95%", p_value = "Valor p", n = "N", mean = "Media",
      sd = "DE", median = "Mediana", q1 = "Q1", q3 = "Q3",
      min = "Mín.", max = "Máx.", aic = "AIC", bic = "BIC",
      residuals = "Residuos", fitted = "Valores Ajustados",
      theoretical = "Cuantiles Teóricos", sample = "Cuantiles Muestra",
      difference = "Diferencia (A − B)", mean_diff = "Media de Medias",
      strategy = "Estrategia", coef = "Coeficiente (β)", variable = "Variable",
      value = "Valor", proportion = "Proporción", count = "Conteo",
      month = "Mes", year = "Año", day_of_week = "Día de Semana",
      sensitivity = "Sensibilidad (min_obs)", season = "Estación"
    )
  )
  lbl[[lang]] %||% lbl[["pt"]]
}

# ── Paletas ───────────────────────────────────────────────────────────────────
.pch_palette <- function(palette = "climasus") {
  pals <- list(
    climasus  = c(primary = "#185FA5", secondary = "#D85A30", tertiary = "#1D9E75",
                  neutral  = "#888780", danger = "#A32D2D", warning = "#854F0B",
                  light    = "#E6F1FB", light2 = "#FAEEDA", surface = "#F1EFE8"),
    viridis   = c(primary = "#440154", secondary = "#31688E", tertiary = "#35B779",
                  neutral  = "#888780", danger = "#A32D2D", warning = "#FDE725",
                  light    = "#F0F0F0", light2 = "#FFFDE7", surface = "#FAFAFA"),
    colorblind = c(primary = "#0072B2", secondary = "#E69F00", tertiary = "#009E73",
                   neutral  = "#999999", danger = "#D55E00", warning = "#F0E442",
                   light    = "#D6EAF8", light2 = "#FEF9E7", surface = "#F5F5F5")
  )
  pals[[palette]] %||% pals[["climasus"]]
}

# ── Detecção de estratégia pelos prefixos das colunas ────────────────────────
.pch_detect_strategy <- function(data) {
  nms <- names(data)

  # Colunas com prefixos reconhecidos
  known_base_vars <- c(
    "patm_mb", "patm_max_mb", "patm_min_mb",
    "tair_dry_bulb_c", "tair_max_c", "tair_min_c",
    "dew_tmean_c", "dew_tmax_c", "dew_tmin_c",
    "rh_max_porc", "rh_min_porc", "rh_mean_porc",
    "rainfall_mm", "ws_gust_m_s", "ws_2_m_s", "wd_degrees", "sr_kj_m2"
  )

  # distributed_lag: var_lagN (coluna = base_var + "_lag" + N)
  dlnm_pattern  <- grep("_lag\\d+$", nms, value = TRUE)
  lag_pattern   <- grep("^lag\\d+_", nms, value = TRUE)
  mvwin_pattern <- grep("^mvwin\\d+_", nms, value = TRUE)
  off_pattern   <- grep("^off\\d+to\\d+_", nms, value = TRUE)
  gdd_pattern   <- grep("^gdd\\d+_tbase", nms, value = TRUE)
  nexc_pattern  <- grep("^nexc\\d+_", nms, value = TRUE)
  pexc_pattern  <- grep("^pexc\\d+_", nms, value = TRUE)
  ncold_pattern <- grep("^ncold\\d+_", nms, value = TRUE)
  wwin_pattern  <- grep("^wwin\\d+_", nms, value = TRUE)
  season_pattern <- grep("^season_", nms, value = TRUE)
  base_present  <- intersect(known_base_vars, nms)

  if (length(dlnm_pattern) > 0) {
    strategy    <- "distributed_lag"
    climate_cols <- unique(sub("_lag\\d+$", "", dlnm_pattern))
    climate_cols <- climate_cols[climate_cols %in% known_base_vars]
    lag_cols     <- dlnm_pattern
  } else if (length(lag_pattern) > 0) {
    strategy     <- "discrete_lag"
    climate_cols <- lag_pattern
    lag_cols     <- lag_pattern
  } else if (length(mvwin_pattern) > 0) {
    strategy     <- "moving_window"
    climate_cols <- mvwin_pattern
    lag_cols     <- character(0)
  } else if (length(off_pattern) > 0) {
    strategy     <- "offset_window"
    climate_cols <- off_pattern
    lag_cols     <- character(0)
  } else if (length(gdd_pattern) > 0) {
    strategy     <- "degree_days"
    climate_cols <- gdd_pattern
    lag_cols     <- character(0)
  } else if (length(nexc_pattern) > 0 || length(pexc_pattern) > 0) {
    strategy     <- "threshold_exceedance"
    climate_cols <- c(nexc_pattern, pexc_pattern)
    lag_cols     <- character(0)
  } else if (length(ncold_pattern) > 0) {
    strategy     <- "cold_wave_exceedance"
    climate_cols <- ncold_pattern
    lag_cols     <- character(0)
  } else if (length(wwin_pattern) > 0) {
    strategy     <- "weighted_window"
    climate_cols <- wwin_pattern
    lag_cols     <- character(0)
  } else if (length(season_pattern) > 0) {
    strategy     <- "seasonal"
    climate_cols <- season_pattern
    lag_cols     <- character(0)
  } else {
    strategy     <- "exact"
    climate_cols <- base_present
    lag_cols     <- character(0)
  }

  list(strategy = strategy, climate_cols = climate_cols, lag_cols = lag_cols)
}

# ── Expansão de aliases ───────────────────────────────────────────────────────
.pch_expand_aliases <- function(plot_type, strategy) {
  all_exploratory <- c("timeseries", "ccf", "scatter", "distribution")
  all_diagnostic  <- c("residuals", "distribution", "ccf")
  all_modeling    <- c("dlnm_surface", "dlnm_overall", "dlnm_lag")
  all_comparative <- c("bland_altman", "coef_compare", "corr_matrix")
  all_tables      <- c("descriptive_table", "correlation_table",
                       "model_comparison", "rr_table")
  all_specialized <- c("spatial_map", "cascade", "heat_calendar")

  expanded <- plot_type
  if ("all_exploratory" %in% plot_type)  expanded <- c(expanded, all_exploratory)
  if ("all_diagnostic"  %in% plot_type)  expanded <- c(expanded, all_diagnostic)
  if ("all_modeling"    %in% plot_type)  expanded <- c(expanded, all_modeling)
  if ("all_comparative" %in% plot_type)  expanded <- c(expanded, all_comparative)
  if ("all_tables"      %in% plot_type)  expanded <- c(expanded, all_tables)
  if ("all_specialized" %in% plot_type)  expanded <- c(expanded, all_specialized)

  # Para discrete_lag/distributed_lag, cascade sempre disponível
  if (strategy %in% c("discrete_lag", "distributed_lag") &&
      "heatmap_corr" %in% expanded) {
    expanded <- c(expanded, "cascade")
  }

  unique(setdiff(expanded, c("all_exploratory", "all_diagnostic",
                              "all_modeling", "all_comparative",
                              "all_tables", "all_specialized")))
}

# ── Tema base CLIMASUS ────────────────────────────────────────────────────────
.pch_theme <- function(pal) {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major   = ggplot2::element_line(color = "#E8E8E4", linewidth = 0.4),
      plot.title         = ggplot2::element_text(face = "bold", size = 13, hjust = 0),
      plot.subtitle      = ggplot2::element_text(color = "#5F5E5A", size = 10, hjust = 0),
      plot.caption       = ggplot2::element_text(color = "#888780", size = 9, hjust = 1),
      axis.title         = ggplot2::element_text(size = 10, color = "#444441"),
      axis.text          = ggplot2::element_text(size = 9,  color = "#5F5E5A"),
      legend.position    = "bottom",
      legend.text        = ggplot2::element_text(size = 9),
      legend.title       = ggplot2::element_text(size = 9, face = "bold"),
      strip.text         = ggplot2::element_text(face = "bold", size = 10),
      plot.margin        = ggplot2::margin(10, 14, 8, 10)
    )
}


# =============================================================================
# PARTE 1 — EXPLORATÓRIO
# =============================================================================

# 1.1 Série Temporal Bivariada ─────────────────────────────────────────────────
.pch_timeseries <- function(data, climate_col, outcome_col, group_col, pal, lbl) {

  df <- data |>
    dplyr::select(
      date,
      outcome   = dplyr::all_of(outcome_col),
      exposure  = dplyr::all_of(climate_col),
      dplyr::any_of(group_col %||% character(0))
    ) |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      outcome  = sum(outcome, na.rm = TRUE),
      exposure = mean(exposure, na.rm = TRUE),
      .groups  = "drop"
    )

  # Normalização para eixo duplo (0-1 para cada série)
  rng_out <- range(df$outcome,  na.rm = TRUE)
  rng_exp <- range(df$exposure, na.rm = TRUE)

  # Fator de escala: mapeia exposure ao intervalo de outcome
  scale_factor <- diff(rng_out) / diff(rng_exp)
  offset_val   <- rng_out[1] - rng_exp[1] * scale_factor

  df <- dplyr::mutate(df, exposure_scaled = exposure * scale_factor + offset_val)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = date)) +
    ggplot2::geom_area(
      ggplot2::aes(y = exposure_scaled),
      fill  = pal["light"], color = pal["primary"],
      linewidth = 0.6, alpha = 0.5
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = outcome),
      color = pal["secondary"], linewidth = 0.9
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = outcome),
      color = pal["secondary"], size = 1.4, alpha = 0.7
    ) +
    ggplot2::scale_x_date(date_breaks = "2 months", date_labels = "%b/%y") +
    ggplot2::scale_y_continuous(
      name     = lbl$outcome,
      sec.axis = ggplot2::sec_axis(
        transform = ~ (. - offset_val) / scale_factor,
        name      = lbl$exposure
      )
    ) +
    .pch_theme(pal) +
    ggplot2::labs(
      title    = paste0(lbl$exposure, " \u00d7 ", lbl$outcome),
      subtitle = paste0(lbl$date, ": ", min(df$date), " — ", max(df$date)),
      x        = NULL,
      caption  = paste0(
        "Linha laranja: ", lbl$outcome,
        " | \u00c1rea azul: ", lbl$exposure,
        " (eixo direito)"
      )
    ) +
    ggplot2::theme(axis.title.y.right = ggplot2::element_text(color = pal["primary"]),
                   axis.title.y.left  = ggplot2::element_text(color = pal["secondary"]))

  p
}


# 1.2 Lag Cross-Correlation Function ──────────────────────────────────────────
.pch_ccf <- function(data, climate_col, outcome_col, max_lag, alpha, pal, lbl) {

  df <- data |>
    dplyr::select(date,
                  outcome  = dplyr::all_of(outcome_col),
                  exposure = dplyr::all_of(climate_col)) |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      outcome  = sum(outcome, na.rm = TRUE),
      exposure = mean(exposure, na.rm = TRUE),
      .groups  = "drop"
    ) |>
    dplyr::arrange(date)

  # Calcular CCF
  ccf_obj  <- stats::ccf(df$exposure, df$outcome,
                         lag.max = max_lag, plot = FALSE, na.action = na.pass)
  ci_bound <- stats::qnorm((1 - alpha / 2)) / sqrt(ccf_obj$n.used)

  ccf_df <- data.frame(
    lag  = as.integer(ccf_obj$lag),
    acf  = as.numeric(ccf_obj$acf),
    sig  = abs(as.numeric(ccf_obj$acf)) > ci_bound
  )

  p <- ggplot2::ggplot(ccf_df, ggplot2::aes(x = lag, y = acf, fill = sig)) +
    ggplot2::geom_col(width = 0.75, color = "white", linewidth = 0.2) +
    ggplot2::geom_hline(yintercept =  ci_bound, linetype = "dashed",
                        color = pal["danger"], linewidth = 0.7) +
    ggplot2::geom_hline(yintercept = -ci_bound, linetype = "dashed",
                        color = pal["danger"], linewidth = 0.7) +
    ggplot2::geom_hline(yintercept = 0, color = pal["neutral"], linewidth = 0.4) +
    ggplot2::scale_fill_manual(
      values = c("FALSE" = pal["light"], "TRUE" = pal["primary"]),
      labels = c("FALSE" = "Não significativo", "TRUE" = paste0("p < ", alpha)),
      name   = NULL
    ) +
    ggplot2::scale_x_continuous(breaks = seq(-max_lag, max_lag, by = 7)) +
    .pch_theme(pal) +
    ggplot2::labs(
      title    = paste0("CCF: ", lbl$exposure, " (t) \u00d7 ", lbl$outcome, " (t + lag)"),
      subtitle = paste0(
        "Lags negativos: clima antecede desfecho | Lags positivos: desfecho antecede clima\n",
        "IC ", round((1 - alpha) * 100), "%: \u00b1", round(ci_bound, 3)
      ),
      x        = lbl$lag,
      y        = lbl$correlation,
      caption  = "Box & Jenkins (1976). Barras azuis: correlações significativas."
    )

  p
}


# 1.3 Heatmap de correlação por lag ───────────────────────────────────────────
.pch_heatmap_corr <- function(data, climate_cols, outcome_col, max_lag, pal, lbl) {

  # Para cada variável climática e cada lag, calcular correlação de Spearman
  df_base <- data |>
    dplyr::select(date,
                  outcome = dplyr::all_of(outcome_col)) |>
    dplyr::group_by(date) |>
    dplyr::summarise(outcome = sum(outcome, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(date)

  lags_seq   <- seq(0, max_lag)
  n_vars     <- min(length(climate_cols), 8)  # máx 8 variáveis
  vars_use   <- climate_cols[seq_len(n_vars)]

  corr_rows <- lapply(vars_use, function(var) {
    if (!var %in% names(data)) return(NULL)
    df_var <- data |>
      dplyr::select(date, exposure = dplyr::all_of(var)) |>
      dplyr::group_by(date) |>
      dplyr::summarise(exposure = mean(exposure, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(date) |>
      dplyr::inner_join(df_base, by = "date")

    lapply(lags_seq, function(l) {
      shifted <- dplyr::lag(df_var$exposure, n = l)
      r       <- suppressWarnings(
        stats::cor(shifted, df_var$outcome, use = "pairwise.complete.obs",
                   method = "spearman")
      )
      data.frame(variable = var, lag = l, rho = r)
    }) |> dplyr::bind_rows()
  }) |> dplyr::bind_rows()

  if (nrow(corr_rows) == 0) {
    cli::cli_warn("heatmap_corr: nenhuma correlacao calculada. Verifique as colunas.")
    return(ggplot2::ggplot() + ggplot2::labs(title = "Sem dados suficientes"))
  }

  # Nome curto da variável (remove prefixo longo)
  corr_rows$var_label <- sub("^(mvwin|lag|off|wwin|nexc|pexc|wwin)\\d+[^_]*_", "",
                              corr_rows$variable)

  p <- ggplot2::ggplot(corr_rows, ggplot2::aes(x = lag, y = var_label, fill = rho)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.3) +
    ggplot2::geom_text(ggplot2::aes(label = round(rho, 2)),
                       size = 2.8, color = "white") +
    ggplot2::scale_fill_gradient2(
      low    = pal["secondary"],
      mid    = "white",
      high   = pal["primary"],
      midpoint = 0,
      limits = c(-1, 1),
      name   = lbl$correlation
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, max_lag, by = 7)) +
    .pch_theme(pal) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 8),
      legend.key.height = ggplot2::unit(0.9, "cm")
    ) +
    ggplot2::labs(
      title    = paste0("Heatmap de Correlação (Spearman): Exposição \u00d7 ", lbl$outcome),
      subtitle = paste0("Por variável e defasagem (0–", max_lag, " dias)"),
      x        = lbl$lag,
      y        = NULL,
      caption  = "Correlação de Spearman. Lags: clima em t-L com desfecho em t."
    )

  p
}


# 1.4 Distribuição ────────────────────────────────────────────────────────────
.pch_distribution <- function(data, climate_col, outcome_col, pal, lbl) {

  df <- data |>
    dplyr::select(
      exposure = dplyr::all_of(climate_col),
      outcome  = dplyr::all_of(outcome_col)
    ) |>
    tidyr::drop_na()

  # Histograma da exposição
  p_hist <- ggplot2::ggplot(df, ggplot2::aes(x = exposure)) +
    ggplot2::geom_histogram(
      bins  = 30, fill = pal["primary"],
      color = "white", linewidth = 0.2, alpha = 0.85
    ) +
    ggplot2::geom_vline(
      xintercept = mean(df$exposure, na.rm = TRUE),
      linetype = "dashed", color = pal["danger"], linewidth = 0.8
    ) +
    .pch_theme(pal) +
    ggplot2::labs(
      title = paste0("Distribuição: ", climate_col),
      x     = lbl$exposure,
      y     = lbl$count,
      caption = paste0(
        "Média: ", round(mean(df$exposure, na.rm = TRUE), 2),
        " | DP: ", round(stats::sd(df$exposure, na.rm = TRUE), 2),
        " | Assimetria: ",
        round(mean((df$exposure - mean(df$exposure, na.rm = TRUE))^3,
                   na.rm = TRUE) /
                stats::sd(df$exposure, na.rm = TRUE)^3, 2)
      )
    )

  # Q-Q plot
  p_qq <- ggplot2::ggplot(df, ggplot2::aes(sample = exposure)) +
    ggplot2::stat_qq(color = pal["primary"], alpha = 0.6, size = 1.5) +
    ggplot2::stat_qq_line(color = pal["danger"], linewidth = 0.8, linetype = "dashed") +
    .pch_theme(pal) +
    ggplot2::labs(
      title   = "Q-Q Plot (Normalidade)",
      x       = lbl$theoretical,
      y       = lbl$sample,
      caption = "Linha tracejada: distribuição normal teórica."
    )

  # Boxplot do desfecho por quartil de exposição
  df_box <- df |>
    dplyr::mutate(
      quartil = cut(exposure,
                    breaks = stats::quantile(exposure, probs = seq(0, 1, 0.25),
                                             na.rm = TRUE),
                    labels = c("Q1", "Q2", "Q3", "Q4"),
                    include.lowest = TRUE)
    )

  p_box <- ggplot2::ggplot(df_box, ggplot2::aes(x = quartil, y = outcome, fill = quartil)) +
    ggplot2::geom_boxplot(alpha = 0.8, outlier.alpha = 0.4, outlier.size = 1) +
    ggplot2::scale_fill_manual(
      values = c("Q1" = pal["light"], "Q2" = "#B5D4F4",
                 "Q3" = pal["primary"], "Q4" = pal["secondary"]),
      guide  = "none"
    ) +
    .pch_theme(pal) +
    ggplot2::labs(
      title   = paste0(lbl$outcome, " por quartil de ", lbl$exposure),
      x       = paste0("Quartil de ", lbl$exposure),
      y       = lbl$outcome,
      caption = "Cada quartil representa 25% da distribuição da exposição."
    )

  # Combina os três com patchwork (se disponível) ou retorna lista
  if (requireNamespace("patchwork", quietly = TRUE)) {
    p <- (p_hist | p_qq) / p_box +
      patchwork::plot_annotation(
        title   = "Diagnóstico de Distribuição",
        caption = "Ref: Bhaskaran et al. (2013) Int. J. Epidemiology."
      )
  } else {
    p <- p_hist  # fallback
    attr(p, "qq_plot")  <- p_qq
    attr(p, "box_plot") <- p_box
  }

  p
}


# 1.5 Scatter + LOESS/GAM ─────────────────────────────────────────────────────
.pch_scatter <- function(data, climate_col, outcome_col, group_col,
                          smooth_method, alpha, pal, lbl) 
                          {

  df <- data |>
    dplyr::select(
      exposure = dplyr::all_of(climate_col),
      outcome  = dplyr::all_of(outcome_col),
      dplyr::any_of(group_col %||% character(0))
    ) |>
    tidyr::drop_na(exposure, outcome)

  # Agrega por município/data se houver duplicatas
  if (!is.null(group_col) && group_col %in% names(df)) {
    grp_sym <- rlang::sym(group_col)
    p <- ggplot2::ggplot(df,
           ggplot2::aes(x = exposure, y = outcome,
                        color = !!grp_sym, group = !!grp_sym))
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = exposure, y = outcome))
  }

  p <- p +
    ggplot2::geom_point(alpha = 0.35, size = 1.6, color = pal["primary"]) +
    ggplot2::geom_smooth(
      method  = if (smooth_method == "gam") "gam" else "loess",
      formula = if (smooth_method == "gam") y ~ s(x, bs = "cs") else y ~ x,
      se      = TRUE,
      color   = pal["secondary"],
      fill    = pal["light2"],
      linewidth = 1.1,
      level   = 1 - alpha
    ) +
    ggplot2::scale_x_continuous(labels = scales::number_format(accuracy = 0.1)) +
    .pch_theme(pal) +
    ggplot2::labs(
      title    = paste0("Relação ", lbl$exposure, " \u00d7 ", lbl$outcome),
      subtitle = paste0("Suavização: ", toupper(smooth_method),
                        " | IC ", round((1 - alpha) * 100), "%"),
      x        = lbl$exposure,
      y        = lbl$outcome,
      caption  = "Curva de suavização não-paramétrica. Área sombreada: IC da predição."
    )

  p
}


# =============================================================================
# PARTE 2 — MODELAGEM (DLNM)
# =============================================================================

# 2.1 Superfície DLNM ─────────────────────────────────────────────────────────
.pch_dlnm_surface <- function(model, crossbasis, outcome_col, alpha, pal, lbl) {
  pred <- dlnm::crosspred(crossbasis, model, by = 1, bylag = 1,
                           cumul = TRUE, ci.level = 1 - alpha)

  # Constrói grid para a superfície
  lag_seq    <- pred$lag
  var_seq    <- as.numeric(rownames(pred$matRRfit))
  mat_rr     <- pred$matRRfit

  df_surf <- expand.grid(variable = var_seq, lag = lag_seq) |>
    dplyr::mutate(
      rr  = as.vector(mat_rr),
      log_rr = log(rr)
    )

  p <- ggplot2::ggplot(df_surf, ggplot2::aes(x = lag, y = variable, fill = log_rr)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      low      = pal["primary"],
      mid      = "white",
      high     = pal["danger"],
      midpoint = 0,
      name     = "log(RR)"
    ) +
    ggplot2::geom_contour(ggplot2::aes(z = log_rr),
                          color = "grey30", linewidth = 0.3, alpha = 0.6) +
    .pch_theme(pal) +
    ggplot2::labs(
      title    = "Superfície Dose-Resposta DLNM",
      subtitle = "Risco Relativo (log) por nível de exposição e defasagem",
      x        = lbl$lag,
      y        = lbl$exposure,
      caption  = "Gasparrini (2011) J. Stat. Softw. Contornos: isovalores de log(RR)."
    )

  p
}


# 2.2 Efeito cumulativo geral ─────────────────────────────────────────────────
.pch_dlnm_overall <- function(model, crossbasis, alpha, pal, lbl) {
  pred <- dlnm::crosspred(crossbasis, model, by = 1,
                           cumul = TRUE, ci.level = 1 - alpha)

  df_ov <- data.frame(
    x    = as.numeric(pred$predvar),
    rr   = pred$allRRfit,
    lo   = pred$allRRlow,
    hi   = pred$allRRhigh
  )
  ref_val <- df_ov$x[which.min(abs(df_ov$rr - 1))]

  p <- ggplot2::ggplot(df_ov, ggplot2::aes(x = x)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi),
                         fill = pal["light"], alpha = 0.8) +
    ggplot2::geom_line(ggplot2::aes(y = rr),
                       color = pal["primary"], linewidth = 1.1) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed",
                        color = pal["neutral"], linewidth = 0.6) +
    ggplot2::geom_vline(xintercept = ref_val, linetype = "dotted",
                        color = pal["secondary"], linewidth = 0.7) +
    ggplot2::scale_y_log10() +
    .pch_theme(pal) +
    ggplot2::labs(
      title    = "Efeito Cumulativo Geral — DLNM",
      subtitle = paste0("Soma dos efeitos em todas as defasagens | IC ",
                        round((1 - alpha) * 100), "%"),
      x        = lbl$exposure,
      y        = paste0(lbl$rr, " (escala log)"),
      caption  = "Zanobetti et al. (2000). Linha tracejada: RR = 1 (sem efeito)."
    )

  p
}


# 2.3 Efeito por defasagem específica ─────────────────────────────────────────
.pch_dlnm_lag <- function(model, crossbasis, alpha, pal, lbl) {
  pred <- dlnm::crosspred(crossbasis, model, by = 1, bylag = 1,
                           ci.level = 1 - alpha)

  # Exposição de referência: mediana ou percentil 75
  ref_pct75 <- stats::quantile(as.numeric(pred$predvar), 0.75, na.rm = TRUE)
  ref_med   <- stats::median(as.numeric(pred$predvar), na.rm = TRUE)

  # Lag-response na exposição no p75 vs mediana
  lag_seq <- pred$lag
  rr_p75  <- pred$matRRfit[which.min(abs(as.numeric(rownames(pred$matRRfit)) - ref_pct75)), ]
  lo_p75  <- pred$matRRlow[which.min(abs(as.numeric(rownames(pred$matRRfit)) - ref_pct75)), ]
  hi_p75  <- pred$matRRhigh[which.min(abs(as.numeric(rownames(pred$matRRfit)) - ref_pct75)), ]

  df_lag <- data.frame(
    lag = lag_seq, rr = rr_p75, lo = lo_p75, hi = hi_p75
  )

  p <- ggplot2::ggplot(df_lag, ggplot2::aes(x = lag, y = rr)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi),
                         fill = pal["light"], alpha = 0.8) +
    ggplot2::geom_line(color = pal["primary"], linewidth = 1.1) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed",
                        color = pal["neutral"], linewidth = 0.6) +
    ggplot2::geom_point(color = pal["primary"], size = 2, alpha = 0.8) +
    ggplot2::scale_y_log10() +
    .pch_theme(pal) +
    ggplot2::labs(
      title    = "Curva Lag-Resposta — DLNM",
      subtitle = paste0("Exposição = p75 (", round(ref_pct75, 1),
                        ") vs. mediana | IC ", round((1 - alpha) * 100), "%"),
      x        = lbl$lag,
      y        = paste0(lbl$rr, " (escala log)"),
      caption  = "Armstrong (2006). Cada ponto: efeito isolado na defasagem específica."
    )

  p
}


# 2.4 Diagnóstico de resíduos ─────────────────────────────────────────────────
.pch_residuals <- function(model, pal, lbl) {

  df_res <- data.frame(
    fitted    = stats::fitted(model),
    residuals = stats::residuals(model, type = "deviance")
  )
  df_res$.index <- seq_len(nrow(df_res))

  # Resíduos vs. ajustados
  p_rv_fit <- ggplot2::ggplot(df_res, ggplot2::aes(x = fitted, y = residuals)) +
    ggplot2::geom_point(alpha = 0.4, size = 1.5, color = pal["primary"]) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        color = pal["danger"], linewidth = 0.7) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                         se = FALSE, color = pal["secondary"], linewidth = 0.9) +
    .pch_theme(pal) +
    ggplot2::labs(title = "Resíduos vs. Ajustados",
                  x = lbl$fitted, y = lbl$residuals)

  # Q-Q dos resíduos
  p_qq <- ggplot2::ggplot(df_res, ggplot2::aes(sample = residuals)) +
    ggplot2::stat_qq(color = pal["primary"], alpha = 0.5, size = 1.5) +
    ggplot2::stat_qq_line(color = pal["danger"], linetype = "dashed", linewidth = 0.8) +
    .pch_theme(pal) +
    ggplot2::labs(title = "Q-Q dos Resíduos de Desvio",
                  x = lbl$theoretical, y = lbl$sample)

  # ACF dos resíduos (autocorrelação residual)
  acf_obj <- stats::acf(df_res$residuals, lag.max = 30, plot = FALSE,
                         na.action = na.pass)
  ci_acf  <- stats::qnorm(0.975) / sqrt(length(df_res$residuals))
  df_acf  <- data.frame(lag = as.integer(acf_obj$lag[-1]),
                         acf = as.numeric(acf_obj$acf[-1]))

  p_acf <- ggplot2::ggplot(df_acf, ggplot2::aes(x = lag, y = acf)) +
    ggplot2::geom_col(fill = pal["primary"], alpha = 0.75, width = 0.7) +
    ggplot2::geom_hline(yintercept =  ci_acf, linetype = "dashed",
                        color = pal["danger"], linewidth = 0.7) +
    ggplot2::geom_hline(yintercept = -ci_acf, linetype = "dashed",
                        color = pal["danger"], linewidth = 0.7) +
    ggplot2::geom_hline(yintercept = 0, color = pal["neutral"], linewidth = 0.3) +
    .pch_theme(pal) +
    ggplot2::labs(title = "ACF dos Resíduos",
                  subtitle = "Autocorrelação residual — verificar independência",
                  x = lbl$lag, y = "ACF")

  # Scale-Location
  df_res$sqrt_abs_res <- sqrt(abs(df_res$residuals))
  p_scale <- ggplot2::ggplot(df_res, ggplot2::aes(x = fitted, y = sqrt_abs_res)) +
    ggplot2::geom_point(alpha = 0.4, size = 1.5, color = pal["primary"]) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                         se = FALSE, color = pal["secondary"], linewidth = 0.9) +
    .pch_theme(pal) +
    ggplot2::labs(title = "Scale-Location (Homocedasticidade)",
                  x = lbl$fitted,
                  y = expression(sqrt("|Resíduos Padronizados|")))

  if (requireNamespace("patchwork", quietly = TRUE)) {
    p <- (p_rv_fit | p_qq) / (p_acf | p_scale) +
      patchwork::plot_annotation(
        title   = "Diagnóstico de Resíduos do Modelo",
        caption = "Bhaskaran et al. (2013). Verificar normalidade, homocedasticidade e independência."
      )
  } else {
    p <- p_rv_fit
    attr(p, "qq")     <- p_qq
    attr(p, "acf")    <- p_acf
    attr(p, "scale")  <- p_scale
  }

  p
}


# =============================================================================
# PARTE 3 — COMPARATIVO
# =============================================================================

# 3.1 Bland-Altman ────────────────────────────────────────────────────────────
.pch_bland_altman <- function(data, data2, climate_col, outcome_col, pal, lbl) {

  # Alinha os dois datasets pela data
  df1 <- data  |> dplyr::select(date, A = dplyr::all_of(climate_col)) |>
    dplyr::group_by(date) |> dplyr::summarise(A = mean(A, na.rm = TRUE), .groups = "drop")
  df2 <- data2 |> dplyr::select(date, B = dplyr::all_of(climate_col)) |>
    dplyr::group_by(date) |> dplyr::summarise(B = mean(B, na.rm = TRUE), .groups = "drop")

  df_ba <- dplyr::inner_join(df1, df2, by = "date") |>
    dplyr::mutate(
      mean_ab = (A + B) / 2,
      diff_ab = A - B
    ) |>
    tidyr::drop_na()

  bias  <- mean(df_ba$diff_ab)
  sd_d  <- stats::sd(df_ba$diff_ab)
  loa_u <- bias + 1.96 * sd_d
  loa_l <- bias - 1.96 * sd_d

  p <- ggplot2::ggplot(df_ba, ggplot2::aes(x = mean_ab, y = diff_ab)) +
    ggplot2::geom_point(alpha = 0.45, size = 2, color = pal["primary"]) +
    ggplot2::geom_hline(yintercept = bias,  color = pal["secondary"],
                        linewidth = 1.0, linetype = "solid") +
    ggplot2::geom_hline(yintercept = loa_u, color = pal["danger"],
                        linewidth = 0.8, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = loa_l, color = pal["danger"],
                        linewidth = 0.8, linetype = "dashed") +
    ggplot2::annotate("text", x = max(df_ba$mean_ab, na.rm = TRUE),
                      y = bias  + 0.1 * sd_d,
                      label = paste0("Viés: ", round(bias, 3)),
                      hjust = 1, size = 3.2, color = pal["secondary"]) +
    ggplot2::annotate("text", x = max(df_ba$mean_ab, na.rm = TRUE),
                      y = loa_u + 0.1 * sd_d,
                      label = paste0("+1.96 DP: ", round(loa_u, 3)),
                      hjust = 1, size = 3.2, color = pal["danger"]) +
    ggplot2::annotate("text", x = max(df_ba$mean_ab, na.rm = TRUE),
                      y = loa_l - 0.1 * sd_d,
                      label = paste0("−1.96 DP: ", round(loa_l, 3)),
                      hjust = 1, size = 3.2, color = pal["danger"]) +
    .pch_theme(pal) +
    ggplot2::labs(
      title    = "Bland-Altman: Concordância entre Estratégias",
      subtitle = paste0(
        "Viés = ", round(bias, 3),
        " | Limites de concordância: [", round(loa_l, 3), ", ", round(loa_u, 3), "]"
      ),
      x        = lbl$mean_diff,
      y        = lbl$difference,
      caption  = "Bland & Altman (1986). Linhas tracejadas: limites de concordância (±1.96 DP)."
    )

  p
}


# 3.2 Coeficientes de regressão por estratégia ────────────────────────────────
.pch_coef_compare <- function(data, climate_cols, outcome_col, alpha, pal, lbl) {

  results <- lapply(climate_cols, function(col) {
    df_m <- data |>
      dplyr::select(y = dplyr::all_of(outcome_col),
                    x = dplyr::all_of(col)) |>
      tidyr::drop_na()
    if (nrow(df_m) < 10 || stats::var(df_m$x, na.rm = TRUE) == 0) return(NULL)
    mod  <- stats::glm(y ~ x, data = df_m, family = stats::quasipoisson())
    coef_val <- stats::coef(summary(mod))["x", ]
    ci   <- stats::confint.default(mod)["x", ]
    data.frame(
      variable = col,
      estimate = coef_val["Estimate"],
      se       = coef_val["Std. Error"],
      lo       = ci[1],
      hi       = ci[2],
      p_value  = coef_val["Pr(>|t|)"]
    )
  })

  df_coef <- dplyr::bind_rows(results)
  if (nrow(df_coef) == 0) {
    cli::cli_warn("coef_compare: nenhum modelo ajustado.")
    return(ggplot2::ggplot() + ggplot2::labs(title = "Sem dados suficientes"))
  }

  df_coef$sig       <- df_coef$p_value < alpha
  df_coef$var_label <- sub("^(mvwin|lag|off|wwin|nexc|pexc|ncold)\\d+[^_]*_", "",
                            df_coef$variable)

  p <- ggplot2::ggplot(df_coef,
         ggplot2::aes(x = estimate, y = stats::reorder(var_label, estimate),
                      color = sig)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                        color = pal["neutral"], linewidth = 0.6) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lo, xmax = hi),
                            height = 0.25, linewidth = 0.7) +
    ggplot2::geom_point(size = 3.5) +
    ggplot2::scale_color_manual(
      values = c("FALSE" = pal["neutral"], "TRUE" = pal["primary"]),
      labels = c("FALSE" = paste0("p \u2265 ", alpha),
                 "TRUE"  = paste0("p < ", alpha)),
      name   = NULL
    ) +
    .pch_theme(pal) +
    ggplot2::labs(
      title    = paste0(lbl$coef, " (Quasi-Poisson) por Variável Climática"),
      subtitle = paste0("Modelo univariado | IC ", round((1 - alpha) * 100), "%"),
      x        = lbl$coef,
      y        = NULL,
      caption  = paste0("Pontos azuis: p < ", alpha, ". Barras: IC ", round((1 - alpha) * 100), "%.")
    )

  p
}


# 3.3 Matriz de correlação entre estratégias ──────────────────────────────────
.pch_corr_matrix <- function(data, climate_cols, pal, lbl) {

  df_mat <- data |>
    dplyr::select(dplyr::any_of(climate_cols)) |>
    dplyr::group_by() |>
    as.data.frame()

  df_mat <- df_mat[, colSums(is.na(df_mat)) < nrow(df_mat) * 0.9, drop = FALSE]
  if (ncol(df_mat) < 2) {
    cli::cli_warn("corr_matrix: menos de 2 colunas validas.")
    return(ggplot2::ggplot() + ggplot2::labs(title = "Dados insuficientes"))
  }

  corr_m <- stats::cor(df_mat, use = "pairwise.complete.obs", method = "spearman")

  # Converter para formato longo
  df_long <- as.data.frame(as.table(corr_m)) |>
    dplyr::rename(var1 = Var1, var2 = Var2, rho = Freq) |>
    dplyr::mutate(
      label1 = sub("^(mvwin|lag|off|wwin|nexc|pexc|ncold)\\d+[^_]*_", "", as.character(var1)),
      label2 = sub("^(mvwin|lag|off|wwin|nexc|pexc|ncold)\\d+[^_]*_", "", as.character(var2))
    )

  p <- ggplot2::ggplot(df_long, ggplot2::aes(x = label1, y = label2, fill = rho)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.4) +
    ggplot2::geom_text(ggplot2::aes(label = round(rho, 2)),
                       size = 3, color = "white") +
    ggplot2::scale_fill_gradient2(
      low = pal["secondary"], mid = "white", high = pal["primary"],
      midpoint = 0, limits = c(-1, 1), name = lbl$correlation
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = ggplot2::element_text(size = 8)
    ) +
    .pch_theme(pal) +
    ggplot2::labs(
      title    = "Matriz de Correlação (Spearman) entre Estratégias",
      subtitle = "Multicolinearidade: |ρ| > 0.8 indica redundância entre variáveis",
      x        = NULL, y = NULL,
      caption  = "VIF > 5 em modelos múltiplos indica colinearidade problemática (car::vif())."
    )

  p
}


# =============================================================================
# PARTE 4 — TABELAS
# =============================================================================

# 4.1 Tabela descritiva ───────────────────────────────────────────────────────
.pch_descriptive_table <- function(data, climate_cols, outcome_col, lbl) {

  all_cols <- c(outcome_col, climate_cols)
  all_cols <- intersect(all_cols, names(data))

  df_desc <- lapply(all_cols, function(col) {
    x <- data[[col]]
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NULL)
    data.frame(
      Variavel  = col,
      N         = length(x),
      Media     = round(mean(x), 3),
      DP        = round(stats::sd(x), 3),
      Mediana   = round(stats::median(x), 3),
      Q1        = round(stats::quantile(x, 0.25), 3),
      Q3        = round(stats::quantile(x, 0.75), 3),
      Min       = round(min(x), 3),
      Max       = round(max(x), 3),
      N_missing = sum(is.na(data[[col]]))
    )
  }) |> dplyr::bind_rows()

  # Tenta formatar com gt se disponível, senão retorna tibble
  if (requireNamespace("gt", quietly = TRUE)) {
    tb <- gt::gt(df_desc) |>
      gt::tab_header(
        title    = "Tabela 1 — Estatísticas Descritivas",
        subtitle = "Variáveis climáticas agregadas e desfecho de saúde"
      ) |>
      gt::cols_label(
        Variavel = "Variável", N = lbl$n, Media = lbl$mean, DP = lbl$sd,
        Mediana = lbl$median, Q1 = lbl$q1, Q3 = lbl$q3,
        Min = lbl$min, Max = lbl$max, N_missing = "NA"
      ) |>
      gt::fmt_number(columns = c(Media, DP, Mediana, Q1, Q3, Min, Max),
                     decimals = 3) |>
      gt::tab_source_note("Bhaskaran et al. (2013) Int. J. Epidemiology 42(4).")
    return(tb)
  }

  tibble::as_tibble(df_desc)
}


# 4.2 Tabela de correlações com o desfecho ────────────────────────────────────
.pch_correlation_table <- function(data, climate_cols, outcome_col, alpha, lbl) {

  outcome_vec <- data[[outcome_col]]

  df_corr <- lapply(climate_cols, function(col) {
    if (!col %in% names(data)) return(NULL)
    x <- data[[col]]
    ok <- !is.na(x) & !is.na(outcome_vec)
    if (sum(ok) < 5) return(NULL)
    sp <- stats::cor.test(x[ok], outcome_vec[ok], method = "spearman",
                          exact = FALSE)
    pr <- stats::cor.test(x[ok], outcome_vec[ok], method = "pearson")
    data.frame(
      Variavel          = col,
      Rho_Spearman      = round(sp$estimate, 3),
      p_Spearman        = round(sp$p.value, 4),
      r_Pearson         = round(pr$estimate, 3),
      p_Pearson         = round(pr$p.value, 4),
      Significativo     = sp$p.value < alpha
    )
  }) |> dplyr::bind_rows()

  df_corr <- dplyr::arrange(df_corr, Rho_Spearman)

  if (requireNamespace("gt", quietly = TRUE)) {
    tb <- gt::gt(df_corr) |>
      gt::tab_header(
        title    = "Tabela 2 — Correlações com o Desfecho",
        subtitle = paste0("Spearman e Pearson | Desfecho: ", outcome_col)
      ) |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(
          columns  = Variavel,
          rows     = Significativo == TRUE
        )
      ) |>
      gt::tab_source_note(paste0(
        "Células em negrito: p < ", alpha, ". Box & Jenkins (1976)."
      ))
    return(tb)
  }

  tibble::as_tibble(df_corr)
}


# 4.3 Tabela de comparação de modelos (AIC/BIC) ───────────────────────────────
.pch_model_comparison <- function(data, climate_cols, outcome_col, alpha, lbl) {

  df_models <- lapply(climate_cols, function(col) {
    df_m <- data |>
      dplyr::select(y = dplyr::all_of(outcome_col),
                    x = dplyr::all_of(col)) |>
      tidyr::drop_na()
    if (nrow(df_m) < 10 || stats::var(df_m$x, na.rm = TRUE) < 1e-10) return(NULL)
    tryCatch({
      mod <- stats::glm(y ~ x, data = df_m, family = stats::quasipoisson())
      # Quasi-Poisson nao tem AIC definido; usar Poisson para comparação
      mod_p <- stats::glm(y ~ x, data = df_m, family = stats::poisson())
      data.frame(
        Variavel    = col,
        N           = nrow(df_m),
        AIC         = round(stats::AIC(mod_p), 1),
        BIC         = round(stats::BIC(mod_p), 1),
        DesvioRes   = round(mod$deviance, 1),
        GrauLib     = mod$df.residual,
        DispRatio   = round(mod$deviance / mod$df.residual, 3)
      )
    }, error = function(e) NULL)
  }) |> dplyr::bind_rows()

  df_models <- dplyr::arrange(df_models, AIC)

  if (requireNamespace("gt", quietly = TRUE)) {
    tb <- gt::gt(df_models) |>
      gt::tab_header(
        title    = "Tabela 3 — Comparação de Modelos (AIC/BIC)",
        subtitle = "Modelos Poisson univariados | Ordenados por AIC crescente"
      ) |>
      gt::cols_label(
        Variavel = "Variável", N = lbl$n,
        AIC = lbl$aic, BIC = lbl$bic,
        DesvioRes = "Desvio Residual", GrauLib = "GL",
        DispRatio = "Razão de Dispersão"
      ) |>
      gt::tab_style(
        style = gt::cell_fill(color = "#EAF3DE"),
        locations = gt::cells_body(rows = 1)
      ) |>
      gt::tab_source_note(
        "Razão de Dispersão > 1.5 indica superdispersão. Usar Quasi-Poisson ou NegBin."
      )
    return(tb)
  }

  tibble::as_tibble(df_models)
}


# 4.4 Tabela de Risco Relativo (RR com IC 95%) ────────────────────────────────
.pch_rr_table <- function(model, alpha, lbl) {

  coef_m <- stats::coef(summary(model))
  ci_m   <- tryCatch(stats::confint(model), error = function(e) {
    stats::confint.default(model)
  })

  df_rr <- data.frame(
    Parametro = rownames(coef_m),
    RR        = round(exp(coef_m[, "Estimate"]), 4),
    IC_low    = round(exp(ci_m[, 1]), 4),
    IC_high   = round(exp(ci_m[, 2]), 4),
    p_value   = round(coef_m[, ncol(coef_m)], 4)
  ) |>
    dplyr::filter(Parametro != "(Intercept)") |>
    dplyr::mutate(
      Sig = dplyr::case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01  ~ "**",
        p_value < 0.05  ~ "*",
        TRUE            ~ ""
      ),
      RR_IC = paste0(RR, " [", IC_low, ", ", IC_high, "]")
    )

  if (requireNamespace("gt", quietly = TRUE)) {
    tb <- gt::gt(df_rr) |>
      gt::tab_header(
        title    = "Tabela 4 — Risco Relativo com IC 95%",
        subtitle = paste0("Baseado no modelo ajustado | IC ", round((1 - alpha) * 100), "%")
      ) |>
      gt::cols_label(
        Parametro = "Parâmetro", RR = lbl$rr,
        IC_low = "IC inf.", IC_high = "IC sup.",
        p_value = lbl$p_value, Sig = "", RR_IC = paste0(lbl$rr, " [IC]")
      ) |>
      gt::tab_source_note(
        "*** p<0.001  ** p<0.01  * p<0.05. Lowe et al. (2021) Nat. Comm."
      )
    return(tb)
  }

  tibble::as_tibble(df_rr)
}


# 4.5 Análise de sensibilidade (min_obs) ──────────────────────────────────────
.pch_sensitivity_table <- function(data, climate_col, outcome_col, min_obs_range, lbl) {

  df_sens <- lapply(min_obs_range, function(mo) {
    x   <- data[[climate_col]]
    y   <- data[[outcome_col]]
    ok  <- !is.na(x) & !is.na(y)
    n_valid  <- sum(ok)
    n_na     <- sum(!ok)
    pct_na   <- round(100 * n_na / length(x), 1)

    if (n_valid < 5) {
      return(data.frame(min_obs = mo, N_valido = n_valid,
                        N_NA = n_na, Pct_NA = pct_na,
                        RR = NA, IC_low = NA, IC_high = NA, p = NA))
    }
    tryCatch({
      mod  <- stats::glm(y[ok] ~ x[ok], family = stats::quasipoisson())
      coef_v <- stats::coef(summary(mod))["x[ok]", ]
      ci_v   <- stats::confint.default(mod)["x[ok]", ]
      data.frame(
        min_obs  = mo,
        N_valido = n_valid,
        N_NA     = n_na,
        Pct_NA   = pct_na,
        RR       = round(exp(coef_v["Estimate"]), 4),
        IC_low   = round(exp(ci_v[1]), 4),
        IC_high  = round(exp(ci_v[2]), 4),
        p        = round(coef_v["Pr(>|t|)"], 4)
      )
    }, error = function(e) {
      data.frame(min_obs = mo, N_valido = n_valid, N_NA = n_na,
                 Pct_NA = pct_na, RR = NA, IC_low = NA, IC_high = NA, p = NA)
    })
  }) |> dplyr::bind_rows()

  if (requireNamespace("gt", quietly = TRUE)) {
    tb <- gt::gt(df_sens) |>
      gt::tab_header(
        title    = "Tabela 5 — Análise de Sensibilidade: min_obs",
        subtitle = paste0("Variação de min_obs em sus_climate_aggregate() | Variável: ", climate_col)
      ) |>
      gt::cols_label(
        min_obs = lbl$sensitivity, N_valido = "N válido",
        N_NA = "N NA", Pct_NA = "% NA",
        RR = lbl$rr, IC_low = "IC inf.", IC_high = "IC sup.", p = lbl$p_value
      ) |>
      gt::tab_source_note(
        "RR estavel entre valores de min_obs indica robustez da analise."
      )
    return(tb)
  }

  tibble::as_tibble(df_sens)
}


# =============================================================================
# PARTE 5 — ESPECIALIZADOS
# =============================================================================

# 5.1 Mapa espacial de efeitos ────────────────────────────────────────────────
.pch_spatial_map <- function(data, climate_col, outcome_col,
                               group_col, model, alpha, pal, lbl) 
                               {

  # Agrega por município: RR do modelo se disponível, caso contrário média
  if (!is.null(model) && "code_muni" %in% names(data)) {
    # Estimativa do efeito por município via modelo simples
    df_muni <- data |>
      dplyr::group_by(.data$code_muni) |>
      dplyr::summarise(
        rr_est = tryCatch({
          x <- .data[[climate_col]]
          y <- .data[[outcome_col]]
          ok <- !is.na(x) & !is.na(y)
          if (sum(ok) < 5) return(NA_real_)
          mod_m <- stats::glm(y[ok] ~ x[ok], family = stats::quasipoisson())
          exp(stats::coef(mod_m)["x[ok]"])
        }, error = function(e) NA_real_),
        exposure_mean = mean(.data[[climate_col]], na.rm = TRUE),
        .groups = "drop"
      )
    fill_col <- "rr_est"
    fill_lab <- lbl$rr
  } else {
    df_muni <- data |>
      dplyr::group_by(.data$code_muni) |>
      dplyr::summarise(
        exposure_mean = mean(.data[[climate_col]], na.rm = TRUE),
        .groups = "drop"
      )
    fill_col <- "exposure_mean"
    fill_lab <- lbl$exposure
  }

  # Join com geometria
  df_sf <- data |>
    dplyr::select("code_muni", geom) |>
    dplyr::distinct(.data$code_muni, .keep_all = TRUE) |>
    dplyr::left_join(df_muni, by = "code_muni") |>
    sf::st_as_sf()

  p <- ggplot2::ggplot(df_sf) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[fill_col]]),
                     color = "white", linewidth = 0.15) +
    ggplot2::scale_fill_gradient2(
      low      = pal["primary"],
      mid      = "white",
      high     = pal["danger"],
      midpoint = if (fill_col == "rr_est") 1 else median(df_sf[[fill_col]], na.rm = TRUE),
      name     = fill_lab,
      na.value = "#CCCCCC"
    ) +
    ggplot2::coord_sf() +
    .pch_theme(pal) +
    ggplot2::theme(
      axis.text   = ggplot2::element_blank(),
      axis.ticks  = ggplot2::element_blank(),
      panel.grid  = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title    = paste0("Mapa Espacial: ", fill_lab, " por Município"),
      subtitle = paste0("Variável: ", climate_col),
      x        = NULL, y = NULL,
      caption  = "Lowe et al. (2021) Nat. Comm. Municípios sem dados: cinza."
    )

  p
}


# 5.2 Cascade Plot ────────────────────────────────────────────────────────────
.pch_cascade <- function(data, climate_cols, outcome_col, lag_cols, alpha, pal, lbl) {

  # Para distributed_lag ou discrete_lag, mostra coeficientes acumulados por lag
  cols_use <- if (length(lag_cols) > 0) lag_cols else climate_cols
  cols_use <- intersect(cols_use, names(data))

  if (length(cols_use) == 0) {
    cli::cli_warn("cascade: nenhuma coluna de defasagem encontrada.")
    return(ggplot2::ggplot() + ggplot2::labs(title = "Sem dados de defasagem"))
  }

  y <- data[[outcome_col]]

  df_cas <- lapply(seq_along(cols_use), function(i) {
    col <- cols_use[i]
    x   <- data[[col]]
    ok  <- !is.na(x) & !is.na(y)
    if (sum(ok) < 5 || stats::var(x[ok]) < 1e-10) return(NULL)
    tryCatch({
      mod <- stats::glm(y[ok] ~ x[ok], family = stats::quasipoisson())
      coef_v <- stats::coef(summary(mod))["x[ok]", ]
      ci_v   <- stats::confint.default(mod)["x[ok]", ]
      # Extrai número do lag do nome da coluna
      lag_num <- as.integer(regmatches(col, regexpr("\\d+", col)))
      data.frame(
        lag      = lag_num %||% i,
        col_name = col,
        rr       = exp(coef_v["Estimate"]),
        lo       = exp(ci_v[1]),
        hi       = exp(ci_v[2]),
        rr_cum   = NA_real_   # preenchido abaixo
      )
    }, error = function(e) NULL)
  }) |> dplyr::bind_rows()

  if (nrow(df_cas) == 0) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Sem dados suficientes"))
  }

  df_cas <- dplyr::arrange(df_cas, lag)
  df_cas$rr_cum <- cumprod(df_cas$rr)

  # Plot com barras (RR individual) e linha (RR cumulativo)
  scale_f <- max(df_cas$rr_cum, na.rm = TRUE) / max(df_cas$rr, na.rm = TRUE)

  p <- ggplot2::ggplot(df_cas, ggplot2::aes(x = lag)) +
    ggplot2::geom_col(ggplot2::aes(y = rr), fill = pal["light"],
                      color = pal["primary"], width = 0.65, alpha = 0.8) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi),
                           width = 0.2, color = pal["primary"], linewidth = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = rr_cum / scale_f),
                       color = pal["secondary"], linewidth = 1.2) +
    ggplot2::geom_point(ggplot2::aes(y = rr_cum / scale_f),
                        color = pal["secondary"], size = 2.5) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed",
                        color = pal["neutral"], linewidth = 0.6) +
    ggplot2::scale_y_continuous(
      name     = paste0(lbl$rr, " (por defasagem)"),
      sec.axis = ggplot2::sec_axis(~ . * scale_f,
                                   name = paste0(lbl$rr, " cumulativo"))
    ) +
    ggplot2::scale_x_continuous(breaks = df_cas$lag) +
    .pch_theme(pal) +
    ggplot2::labs(
      title    = "Cascade Plot — Efeito por Defasagem (Acumulado Sequencial)",
      subtitle = "Barras: RR individual | Linha laranja: RR cumulativo (produto)",
      x        = lbl$lag,
      caption  = "Gasparrini et al. (2015). Barras de erro: IC 95%."
    )

  p
}


# 5.3 Heat Calendar ───────────────────────────────────────────────────────────
.pch_heat_calendar <- function(data, climate_col, outcome_col, pal, lbl) {

  df <- data |>
    dplyr::select(date,
                  outcome  = dplyr::all_of(outcome_col),
                  exposure = dplyr::all_of(climate_col)) |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      outcome  = sum(outcome,  na.rm = TRUE),
      exposure = mean(exposure, na.rm = TRUE),
      .groups  = "drop"
    ) |>
    dplyr::mutate(
      year     = lubridate::year(date),
      month    = lubridate::month(date, label = TRUE, abbr = TRUE,
                                  locale = "pt_BR.UTF-8"),
      week_day = lubridate::wday(date, label = TRUE, abbr = TRUE,
                                  locale = "pt_BR.UTF-8"),
      week_num = as.integer(format(date, "%U"))
    )

  # Calendário de calor da EXPOSIÇÃO
  p_exp <- ggplot2::ggplot(df, ggplot2::aes(x = week_num, y = week_day, fill = exposure)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.3) +
    ggplot2::scale_fill_gradient2(
      low      = pal["primary"],
      mid      = "#FAEEDA",
      high     = pal["secondary"],
      midpoint = stats::median(df$exposure, na.rm = TRUE),
      name     = lbl$exposure,
      na.value = pal["neutral"]
    ) +
    ggplot2::facet_wrap(~year, ncol = 1) +
    .pch_theme(pal) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks  = ggplot2::element_blank(),
      panel.grid  = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title    = "Calendário de Calor — Exposição Climática",
      subtitle = climate_col,
      x        = lbl$month,
      y        = lbl$day_of_week
    )

  # Calendário do DESFECHO
  p_out <- ggplot2::ggplot(df, ggplot2::aes(x = week_num, y = week_day, fill = outcome)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.3) +
    ggplot2::scale_fill_gradient(
      low      = pal["light"],
      high     = pal["danger"],
      name     = lbl$outcome,
      na.value = pal["neutral"]
    ) +
    ggplot2::facet_wrap(~year, ncol = 1) +
    .pch_theme(pal) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks  = ggplot2::element_blank(),
      panel.grid  = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title    = "Calendário de Calor — Desfecho de Saúde",
      subtitle = outcome_col,
      x        = lbl$month,
      y        = lbl$day_of_week,
      caption  = "Carslaw & Ropkins (2012). Colunas: semanas do ano."
    )

  if (requireNamespace("patchwork", quietly = TRUE)) {
    p <- p_exp / p_out +
      patchwork::plot_annotation(
        title = "Heat Calendar: Padrões Sazonais de Exposição e Desfecho"
      )
  } else {
    p <- p_exp
    attr(p, "outcome_calendar") <- p_out
  }

  p
}


# =============================================================================
# EXPORT
# =============================================================================

.pch_save_plots <- function(result, save_path, width, height, dpi, verbose) {
  if (!dir.exists(save_path)) dir.create(save_path, recursive = TRUE)

  for (nm in names(result)) {
    obj <- result[[nm]]
    if (inherits(obj, "ggplot")) {
      file_path <- file.path(save_path, paste0(nm, ".png"))
      ggplot2::ggsave(file_path, plot = obj, width = width, height = height,
                      dpi = dpi, bg = "white")
      if (verbose) cli::cli_alert_success("Salvo: {file_path}")
    } else if (inherits(obj, "gt_tbl") && requireNamespace("gt", quietly = TRUE)) {
      file_path <- file.path(save_path, paste0(nm, ".html"))
      gt::gtsave(obj, filename = file_path)
      if (verbose) cli::cli_alert_success("Salvo: {file_path}")
    }
  }
  invisible(NULL)
}
