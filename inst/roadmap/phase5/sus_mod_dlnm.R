#' Modelo Distributed Lag Non-linear (DLNM) para Análises Clima-Saúde
#'
#' @description
#' `sus_mod_dlnm()` constrói e ajusta um modelo DLNM completo a partir de um
#' `climasus_df` com estratégia `"distributed_lag"`, encapsulando em um único
#' passo a construção da `crossbasis`, o ajuste do GLM e o cálculo do
#' `crosspred`. O objeto retornado (`climasus_dlnm`) é aceito diretamente
#' por `plot_climate_health(fit = ...)`.
#'
#' @param data Um `climasus_df` produzido por `sus_climate_aggregate()` com
#'   `temporal_strategy = "distributed_lag"`. Deve conter colunas no formato
#'   `{var}_lag0`, `{var}_lag1`, …, `{var}_lag{L}`.
#' @param outcome_col Character. Coluna de desfecho. Padrão: `"n_obitos"`.
#' @param climate_col Character. Variável climática base (sem sufixo `_lagN`).
#'   Se `NULL`, detecta automaticamente.
#' @param lag_max Integer. Defasagem máxima. Se `NULL`, detecta pelo maior N
#'   presente nas colunas `{var}_lagN`.
#' @param argvar Lista de argumentos para a dimensão de exposição da
#'   `crossbasis`. Padrão: `list(fun = "ns", df = 4)`.
#' @param arglag Lista de argumentos para a dimensão de lag da `crossbasis`.
#'   Padrão: `list(fun = "ns", df = 3)`.
#' @param family Character. Família GLM: `"quasipoisson"` (padrão),
#'   `"poisson"` ou `"negbin"`.
#' @param ns_df Integer ou `NULL`. Graus de liberdade para spline temporal
#'   (`ns(date, df = ns_df)`). Recomendado: 4–8.
#' @param ref_value Numérico ou `NULL`. Valor de referência para o `crosspred`
#'   (ponto onde RR = 1). Se `NULL`, usa a mediana da exposição.
#' @param alpha Numérico (0–1). Nível de significância para IC. Padrão: `0.05`.
#' @param lang Character. Idioma das mensagens. Padrão: `"pt"`.
#' @param verbose Lógico. Exibe progresso. Padrão: `TRUE`.
#'
#' @return Um objeto `climasus_dlnm` (lista) contendo:
#'   \describe{
#'     \item{`$model`}{Objeto `glm` ajustado com a `crossbasis`.}
#'     \item{`$crossbasis`}{Objeto `crossbasis` do pacote `dlnm`.}
#'     \item{`$pred`}{Objeto `crosspred` pré-calculado — reutilizado por
#'       todos os plots DLNM sem recalcular.}
#'     \item{`$models`}{Tibble com RR cumulativo no percentil 75 vs mediana,
#'       para integração com `plot_climate_health()`.}
#'     \item{`$diagnostics`}{Lista com dispersão e autocorrelação residual.}
#'     \item{`$meta`}{Metadados: variável, lag_max, argvar, arglag, n, etc.}
#'   }
#'
#' @examples
#' \dontrun{
#' df_dlnm <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial,
#'   climate_data = df_inmet,
#'   climate_var = "tair_dry_bulb_c",
#'   temporal_strategy = "distributed_lag",
#'   lag_days = 21
#' )
#'
#' fit_dlnm <- sus_mod_dlnm(
#'   data        = df_dlnm,
#'   outcome_col = "n_obitos",
#'   lag_max     = 21,
#'   ns_df       = 4,
#'   verbose     = TRUE
#' )
#'
#' print(fit_dlnm)
#'
#' plots <- plot_climate_health(
#'   data = df_dlnm,
#'   fit  = fit_dlnm,
#'   plot_type = c("dlnm_surface", "dlnm_overall", "dlnm_lag", "residuals")
#' )
#' }
#'
#' @references
#' Gasparrini, A. (2011). Distributed lag linear and non-linear models in R:
#' the package dlnm. *Journal of Statistical Software*, 43(8), 1-20.
#'
#' @seealso [sus_mod_glm()], [sus_mod_compare()], [plot_climate_health()]
#' @export
sus_mod_dlnm <- function(
    data,
    outcome_col = "n_obitos",
    climate_col = NULL,
    lag_max     = NULL,
    argvar      = list(fun = "ns", df = 4),
    arglag      = list(fun = "ns", df = 3),
    family      = "quasipoisson",
    ns_df       = NULL,
    ref_value   = NULL,
    alpha       = 0.05,
    lang        = "pt",
    verbose     = TRUE
) {

  # ── Verificação de dependências ───────────────────────────────────────────
  if (!requireNamespace("dlnm", quietly = TRUE))
    cli::cli_abort("Pacote 'dlnm' necessario. Instale com install.packages('dlnm').")
  if (!requireNamespace("splines", quietly = TRUE))
    cli::cli_abort("Pacote 'splines' necessario. Instale com install.packages('splines').")

  # ── Validação de inputs ───────────────────────────────────────────────────
  if (!is.data.frame(data))
    cli::cli_abort("{.arg data} deve ser um data frame ou tibble.")
  if (!outcome_col %in% names(data))
    cli::cli_abort("Coluna de desfecho '{outcome_col}' nao encontrada.")
  family <- match.arg(family, c("quasipoisson", "poisson", "negbin"))

  # ── Detecta variável climática base e lag_max ─────────────────────────────
  if (is.null(climate_col)) {
    climate_col <- .sdl_detect_base_var(data)
    if (verbose) cli::cli_alert_info("Variavel base detectada: {climate_col}")
  }

  lag_cols <- grep(paste0("^", climate_col, "_lag\\d+$"), names(data), value = TRUE)
  if (length(lag_cols) == 0)
    cli::cli_abort(
      "Nenhuma coluna '{climate_col}_lagN' encontrada. ",
      "Execute sus_climate_aggregate() com temporal_strategy = 'distributed_lag'."
    )

  if (is.null(lag_max)) {
    lag_nums <- as.integer(gsub(paste0("^", climate_col, "_lag"), "", lag_cols))
    lag_max  <- max(lag_nums, na.rm = TRUE)
    if (verbose) cli::cli_alert_info("lag_max detectado: {lag_max}")
  }

  # ── Agrega por data ───────────────────────────────────────────────────────
  lag_col_names <- paste0(climate_col, "_lag", seq(0, lag_max))
  missing_lags  <- setdiff(lag_col_names, names(data))
  if (length(missing_lags) > 0)
    cli::cli_abort("Colunas ausentes: {paste(missing_lags, collapse=', ')}")

  if (verbose) cli::cli_progress_step("Preparando matriz de exposicao...")

  df_agg <- data |>
    sf::st_drop_geometry() |>
    dplyr::select(
      date,
      y = dplyr::all_of(outcome_col),
      dplyr::all_of(lag_col_names)
    ) |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      y = sum(y, na.rm = TRUE),
      dplyr::across(dplyr::all_of(lag_col_names), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::arrange(date) |>
    tidyr::drop_na()

  if (nrow(df_agg) < lag_max + 10)
    cli::cli_abort("Dados insuficientes: {nrow(df_agg)} obs apos drop_na (minimo: {lag_max + 10}).")

  if (verbose) cli::cli_progress_done()

  # ── Matriz de exposição ───────────────────────────────────────────────────
  expo_matrix <- as.matrix(dplyr::select(df_agg, dplyr::all_of(lag_col_names)))
  colnames(expo_matrix) <- seq(0, lag_max)  # dlnm espera lags como nomes

  # ── Crossbasis ────────────────────────────────────────────────────────────
  if (verbose) cli::cli_progress_step("Construindo crossbasis...")

  cb <- dlnm::crossbasis(
    expo_matrix,
    lag    = lag_max,
    argvar = argvar,
    arglag = arglag
  )

  if (verbose) cli::cli_progress_done()

  # ── Fórmula e modelo ──────────────────────────────────────────────────────
  if (!is.null(ns_df)) {
    formula_obj <- stats::as.formula(
      paste("y ~ cb + splines::ns(as.integer(date), df =", ns_df, ")")
    )
  } else {
    formula_obj <- y ~ cb
  }

  if (verbose) cli::cli_progress_step("Ajustando GLM {toupper(family)}...")

  fam_obj <- switch(family,
    poisson      = stats::poisson(),
    quasipoisson = stats::quasipoisson(),
    negbin       = {
      if (!requireNamespace("MASS", quietly = TRUE))
        cli::cli_abort("Pacote 'MASS' necessario para negbin.")
      # Usa Poisson como proxy para negbin em DLNM (MASS::glm.nb nao suporta crossbasis)
      cli::cli_alert_warning("negbin nao suportado com crossbasis. Usando quasipoisson.")
      stats::quasipoisson()
    }
  )

  model <- stats::glm(formula_obj, data = df_agg, family = fam_obj)

  if (verbose) cli::cli_progress_done()

  # ── Crosspred ─────────────────────────────────────────────────────────────
  if (verbose) cli::cli_progress_step("Calculando crosspred...")

  if (is.null(ref_value))
    ref_value <- stats::median(expo_matrix[, 1], na.rm = TRUE)

  pred <- dlnm::crosspred(
    cb,
    model,
    by        = 1,
    bylag     = 1,
    cumul     = TRUE,
    cen       = ref_value,
    ci.level  = 1 - alpha
  )

  if (verbose) cli::cli_progress_done()

  # ── Diagnósticos ──────────────────────────────────────────────────────────
  resid_dev  <- stats::residuals(model, type = "deviance")
  disp_ratio <- model$deviance / model$df.residual
  bt         <- tryCatch(stats::Box.test(resid_dev, lag = 10, type = "Ljung-Box"),
                         error = function(e) NULL)

  diagnostics <- list(
    disp_ratio    = disp_ratio,
    autocorr_test = bt,
    has_autocorr  = if (!is.null(bt)) bt$p.value < 0.05 else NA
  )

  if (verbose) {
    if (isTRUE(diagnostics$has_autocorr))
      cli::cli_alert_warning(
        "Autocorrelacao residual (Ljung-Box p = {round(bt$p.value, 4)}). ",
        "Aumente ns_df= para melhor controle da tendencia temporal."
      )
    if (disp_ratio > 1.5)
      cli::cli_alert_info("Razao de dispersao = {round(disp_ratio, 2)}.")
  }

  # ── Tabela de RR cumulativo para plot_climate_health ─────────────────────
  # Extrai RR cumulativo no percentil 75 da exposição como resumo do efeito
  p75    <- stats::quantile(expo_matrix[, 1], 0.75, na.rm = TRUE)
  p75_idx <- which.min(abs(as.numeric(pred$predvar) - p75))

  models_tbl <- tibble::tibble(
    variable   = climate_col,
    n          = nrow(df_agg),
    family     = family,
    lag_max    = lag_max,
    ref_value  = ref_value,
    exposure_p75 = round(p75, 3),
    rr         = pred$allRRfit[p75_idx],
    lo         = pred$allRRlow[p75_idx],
    hi         = pred$allRRhigh[p75_idx],
    # Lag de efeito máximo
    lag_peak   = pred$lag[which.max(abs(log(
      pred$matRRfit[p75_idx, ])))],
    disp_ratio = round(disp_ratio, 3)
  )

  if (verbose) {
    rr_r  <- round(models_tbl$rr, 4)
    lo_r  <- round(models_tbl$lo, 4)
    hi_r  <- round(models_tbl$hi, 4)
    lag_p <- models_tbl$lag_peak
    cli::cli_alert_success(
      "RR cumulativo (p75 vs mediana): {rr_r} [{lo_r}, {hi_r}] | Lag pico: {lag_p}"
    )
  }

  # ── lag_rr para cascade plot ──────────────────────────────────────────────
  lag_rr_tbl <- tibble::tibble(
    lag    = as.integer(pred$lag),
    rr     = as.numeric(pred$matRRfit[p75_idx, ]),
    lo     = as.numeric(pred$matRRlow[p75_idx,  ]),
    hi     = as.numeric(pred$matRRhigh[p75_idx, ]),
    rr_cum = NA_real_
  )
  lag_rr_tbl$rr_cum <- cumprod(lag_rr_tbl$rr)

  # ── Monta objeto climasus_dlnm ────────────────────────────────────────────
  structure(
    list(
      model       = model,
      crossbasis  = cb,
      pred        = pred,
      models      = models_tbl,
      lag_rr      = lag_rr_tbl,
      diagnostics = diagnostics,
      meta        = list(
        climate_col = climate_col,
        outcome_col = outcome_col,
        lag_max     = lag_max,
        ref_value   = ref_value,
        argvar      = argvar,
        arglag      = arglag,
        family      = family,
        ns_df       = ns_df,
        n           = nrow(df_agg),
        call_time   = Sys.time()
      )
    ),
    class = "climasus_dlnm"
  )
}


# =============================================================================
# MÉTODOS S3
# =============================================================================

#' @export
print.climasus_dlnm <- function(x, ...) {
  cli::cli_h2("Modelo climasus_dlnm")
  cli::cli_text("Exposicao : {x$meta$climate_col}  (lag 0-{x$meta$lag_max})")
  cli::cli_text("Desfecho  : {x$meta$outcome_col}")
  cli::cli_text("Familia   : {x$meta$family}")
  cli::cli_text("N obs     : {x$meta$n}")
  cli::cli_text("Ref value : {round(x$meta$ref_value, 2)}")
  cli::cli_rule()

  m   <- x$models
  rr  <- round(m$rr, 4)
  lo  <- round(m$lo, 4)
  hi  <- round(m$hi, 4)
  cli::cli_text(
    "RR cumulativo (p75 vs mediana): {rr} [{lo}, {hi}]"
  )
  cli::cli_text("Lag de efeito pico: {m$lag_peak}")
  cli::cli_text("Dispersao: {round(x$diagnostics$disp_ratio, 2)}")

  if (isTRUE(x$diagnostics$has_autocorr))
    cli::cli_alert_warning("Autocorrelacao residual detectada.")

  invisible(x)
}

#' @export
summary.climasus_dlnm <- function(object, ...) {
  print(object)
  cat("\n--- crossbasis ---\n")
  print(summary(object$crossbasis))
  cat("\n--- GLM ---\n")
  print(summary(object$model))
  invisible(object)
}

#' @export
coef.climasus_dlnm <- function(object, ...) {
  stats::coef(object$model, ...)
}


# =============================================================================
# HELPERS INTERNOS
# =============================================================================

#' @keywords internal
.sdl_detect_base_var <- function(data) {
  lag_cols   <- grep("_lag\\d+$", names(data), value = TRUE)
  if (length(lag_cols) == 0)
    cli::cli_abort("Nenhuma coluna _lagN encontrada. Use temporal_strategy = 'distributed_lag'.")
  base_vars  <- unique(sub("_lag\\d+$", "", lag_cols))
  known_vars <- c("tair_dry_bulb_c","tair_max_c","tair_min_c","rainfall_mm",
                  "rh_mean_porc","patm_mb","sr_kj_m2","wd_degrees","ws_2_m_s")
  # Prioriza variáveis conhecidas
  preferred  <- intersect(known_vars, base_vars)
  if (length(preferred) > 0) return(preferred[1])
  base_vars[1]
}
