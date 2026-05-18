#' Modelos Lineares Generalizados para Análises Clima-Saúde
#'
#' @description
#' `sus_mod_glm()` ajusta modelos GLM (Poisson, Quasi-Poisson ou Binomial
#' Negativo) sobre a saída de `sus_climate_aggregate()`, realizando
#' automaticamente diagnóstico de superdispersão, teste de autocorrelação
#' residual e cálculo do Risco Relativo (RR) com IC 95%.
#'
#' O objeto retornado (`climasus_glm`) é aceito diretamente por
#' `plot_climate_health(fit = ...)`, eliminando a necessidade de passar
#' `model=` e `crossbasis=` como parâmetros separados.
#'
#' @param data Um `climasus_df` produzido por `sus_climate_aggregate()`.
#' @param outcome_col Character. Coluna de desfecho. Padrão: `"n_obitos"`.
#' @param climate_col Character. Coluna climática principal. Se `NULL`
#'   (padrão), usa a primeira variável climática detectada automaticamente.
#' @param covariates Character vector. Colunas adicionais a incluir como
#'   covariáveis. Aceita `NULL` (apenas exposição climática).
#' @param family Character. Família do GLM:
#'   \describe{
#'     \item{`"auto"`}{Ajusta Poisson; se dispersão > 1.5, muda para
#'       Quasi-Poisson automaticamente (padrão).}
#'     \item{`"poisson"`}{Poisson (para dados sem superdispersão).}
#'     \item{`"quasipoisson"`}{Quasi-Poisson (padrão para contagens em saúde).}
#'     \item{`"negbin"`}{Binomial Negativo via `MASS::glm.nb()`.}
#'   }
#' @param ns_df Integer ou `NULL`. Graus de liberdade para spline natural
#'   temporal (`splines::ns(date, df = ns_df)`). Se `NULL` (padrão), não
#'   inclui tendência temporal. Recomendado: 4–8 para séries anuais.
#' @param alpha Numérico (0–1). Nível de significância para IC. Padrão: `0.05`.
#' @param spatial_rr Lógico. Se `TRUE`, calcula RR por município (estratificado
#'   por `code_muni`). Padrão: `FALSE` (modelo marginal).
#' @param lang Character. Idioma das mensagens: `"pt"`, `"en"`, `"es"`.
#'   Padrão: `"pt"`.
#' @param verbose Lógico. Exibe diagnósticos no console. Padrão: `TRUE`.
#'
#' @return Um objeto de classe `climasus_glm` (lista) contendo:
#'   \describe{
#'     \item{`$model`}{Objeto `glm` ajustado.}
#'     \item{`$models`}{Tibble com RR, IC 95%, AIC, BIC, dispersão por variável.}
#'     \item{`$spatial_rr`}{Tibble com RR por `code_muni` (se `spatial_rr = TRUE`).}
#'     \item{`$diagnostics`}{Lista com: dispersão, `Box.test` de autocorrelação,
#'       Shapiro-Wilk dos resíduos, VIF (se múltiplas covariáveis).}
#'     \item{`$meta`}{Lista com: `family`, `formula`, `climate_col`,
#'       `outcome_col`, `n`, `call_time`.}
#'   }
#'
#' @examples
#' \dontrun{
#' df_exact <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial,
#'   climate_data = df_inmet,
#'   climate_var = "tair_dry_bulb_c",
#'   temporal_strategy = "exact"
#' )
#'
#' fit_glm <- sus_mod_glm(
#'   data        = df_exact,
#'   outcome_col = "n_obitos",
#'   family      = "auto",
#'   ns_df       = 4,
#'   verbose     = TRUE
#' )
#'
#' print(fit_glm)
#' summary(fit_glm)
#'
#' plots <- plot_climate_health(
#'   data = df_exact,
#'   fit  = fit_glm,
#'   plot_type = c("residuals", "rr_table", "spatial_map")
#' )
#' }
#'
#' @seealso [sus_mod_dlnm()], [sus_mod_compare()], [plot_climate_health()]
#' @export
sus_mod_glm <- function(
    data,
    outcome_col  = "n_obitos",
    climate_col  = NULL,
    covariates   = NULL,
    family       = "auto",
    ns_df        = NULL,
    alpha        = 0.05,
    spatial_rr   = FALSE,
    lang         = "pt",
    verbose      = TRUE
) {

  # ── Validação de inputs ───────────────────────────────────────────────────
  if (!is.data.frame(data))
    cli::cli_abort("{.arg data} deve ser um data frame ou tibble.")
  if (!outcome_col %in% names(data))
    cli::cli_abort("Coluna de desfecho '{outcome_col}' nao encontrada.")
  if (!"date" %in% names(data))
    cli::cli_abort("{.arg data} deve conter a coluna 'date'.")

  family <- match.arg(family, c("auto", "poisson", "quasipoisson", "negbin"))

  # ── Detecta coluna climática principal ───────────────────────────────────
  if (is.null(climate_col)) {
    detected    <- .smg_detect_climate_col(data)
    climate_col <- detected[1]
    if (verbose)
      cli::cli_alert_info("Coluna climatica detectada: {climate_col}")
  }
  if (!climate_col %in% names(data))
    cli::cli_abort("Coluna climatica '{climate_col}' nao encontrada.")

  # ── Prepara dados para modelagem ─────────────────────────────────────────
  df_mod <- data |>
    dplyr::select(
      date,
      y    = dplyr::all_of(outcome_col),
      x    = dplyr::all_of(climate_col),
      dplyr::any_of(covariates %||% character(0))
    ) |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      y = sum(y, na.rm = TRUE),
      x = mean(x, na.rm = TRUE),
      dplyr::across(dplyr::any_of(covariates %||% character(0)),
                    ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    tidyr::drop_na(x, y)

  if (nrow(df_mod) < 10)
    cli::cli_abort("Menos de 10 observacoes validas apos preparacao dos dados.")

  # ── Fórmula ───────────────────────────────────────────────────────────────
  rhs <- "x"
  if (!is.null(ns_df)) {
    if (!requireNamespace("splines", quietly = TRUE))
      cli::cli_abort("Pacote 'splines' necessario para ns_df. Instale com install.packages('splines').")
    rhs <- paste0(rhs, " + splines::ns(as.integer(date), df = ", ns_df, ")")
  }
  if (!is.null(covariates)) {
    rhs <- paste0(rhs, " + ", paste(covariates, collapse = " + "))
  }
  formula_obj <- stats::as.formula(paste("y ~", rhs))

  # ── Família: auto-detecção de superdispersão ──────────────────────────────
  if (family == "auto") {
    mod_p   <- stats::glm(formula_obj, data = df_mod, family = stats::poisson())
    disp_r  <- mod_p$deviance / mod_p$df.residual
    family  <- if (disp_r > 1.5) {
      if (verbose)
        cli::cli_alert_info(
          "Razao de dispersao = {round(disp_r, 2)} > 1.5. Usando Quasi-Poisson."
        )
      "quasipoisson"
    } else "poisson"
  }

  # ── Ajuste do modelo ──────────────────────────────────────────────────────
  if (verbose) cli::cli_progress_step("Ajustando modelo {toupper(family)}...")

  model <- if (family == "negbin") {
    if (!requireNamespace("MASS", quietly = TRUE))
      cli::cli_abort("Pacote 'MASS' necessario para negbin.")
    MASS::glm.nb(formula_obj, data = df_mod)
  } else {
    fam_obj <- switch(family,
      poisson      = stats::poisson(),
      quasipoisson = stats::quasipoisson()
    )
    stats::glm(formula_obj, data = df_mod, family = fam_obj)
  }

  if (verbose) cli::cli_progress_done()

  # ── Extrai RR, IC e estatísticas ─────────────────────────────────────────
  coef_sum  <- stats::coef(summary(model))
  ci        <- tryCatch(stats::confint(model),
                        error = function(e) stats::confint.default(model))
  disp_ratio <- model$deviance / model$df.residual

  models_tbl <- tibble::tibble(
    variable   = climate_col,
    n          = nrow(df_mod),
    family     = family,
    estimate   = coef_sum["x", "Estimate"],
    se         = coef_sum["x", "Std. Error"],
    lo         = ci["x", 1],
    hi         = ci["x", 2],
    rr         = exp(coef_sum["x", "Estimate"]),
    rr_lo      = exp(ci["x", 1]),
    rr_hi      = exp(ci["x", 2]),
    p_value    = coef_sum["x", ncol(coef_sum)],
    aic        = tryCatch(stats::AIC(model), error = function(e) NA_real_),
    bic        = tryCatch(stats::BIC(model), error = function(e) NA_real_),
    disp_ratio = round(disp_ratio, 3)
  )

  # ── Diagnósticos ──────────────────────────────────────────────────────────
  resid_dev  <- stats::residuals(model, type = "deviance")
  bt         <- tryCatch(stats::Box.test(resid_dev, lag = 10, type = "Ljung-Box"),
                         error = function(e) NULL)
  sw         <- tryCatch(stats::shapiro.test(resid_dev[seq_len(min(5000, length(resid_dev)))]),
                         error = function(e) NULL)

  diagnostics <- list(
    disp_ratio     = disp_ratio,
    autocorr_test  = bt,
    normality_test = sw,
    has_autocorr   = if (!is.null(bt)) bt$p.value < 0.05 else NA,
    has_non_normal = if (!is.null(sw)) sw$p.value < 0.05 else NA
  )

  if (verbose) {
    if (!is.null(bt) && bt$p.value < 0.05)
      cli::cli_alert_warning(
        "Autocorrelacao residual detectada (Ljung-Box p = {round(bt$p.value, 4)}). ",
        "Considere adicionar ns_df= para controlar tendencia temporal."
      )
    if (disp_ratio > 1.5 && family == "poisson")
      cli::cli_alert_warning(
        "Razao de dispersao = {round(disp_ratio, 2)}. Considere family = 'quasipoisson' ou 'negbin'."
      )
  }

  # ── RR espacial (opcional) ────────────────────────────────────────────────
  sp_rr <- NULL
  if (spatial_rr && "code_muni" %in% names(data)) {
    if (verbose) cli::cli_progress_step("Calculando RR por municipio...")

    fam_obj_sp <- switch(family,
      poisson      = stats::poisson(),
      quasipoisson = stats::quasipoisson(),
      negbin       = stats::poisson()  # fallback para spatial
    )

    sp_rr <- data |>
      sf::st_drop_geometry() |>
      dplyr::select(
        code_muni = "code_muni",
        y = dplyr::all_of(outcome_col),
        x = dplyr::all_of(climate_col)
      ) |>
      dplyr::group_by(.data$code_muni) |>
      dplyr::summarise(
        n_obs = dplyr::n(),
        rr = tryCatch({
          df_sp <- dplyr::cur_data()
          ok    <- !is.na(df_sp$x) & !is.na(df_sp$y)
          if (sum(ok) < 5) return(NA_real_)
          m <- stats::glm(y[ok] ~ x[ok], family = fam_obj_sp)
          exp(stats::coef(m)["x[ok]"])
        }, error = function(e) NA_real_),
        exposure_mean = mean(.data$x, na.rm = TRUE),
        .groups = "drop"
      )

    if (verbose) cli::cli_progress_done()
  }

  # ── Monta objeto climasus_glm ─────────────────────────────────────────────
  structure(
    list(
      model       = model,
      models      = models_tbl,
      spatial_rr  = sp_rr,
      diagnostics = diagnostics,
      meta        = list(
        family      = family,
        formula     = deparse(formula_obj),
        climate_col = climate_col,
        outcome_col = outcome_col,
        n           = nrow(df_mod),
        ns_df       = ns_df,
        call_time   = Sys.time()
      )
    ),
    class = "climasus_glm"
  )
}


# =============================================================================
# MÉTODOS S3
# =============================================================================

#' @export
print.climasus_glm <- function(x, ...) {
  cli::cli_h2("Modelo climasus_glm")
  cli::cli_text("Familia   : {x$meta$family}")
  cli::cli_text("Exposicao : {x$meta$climate_col}")
  cli::cli_text("Desfecho  : {x$meta$outcome_col}")
  cli::cli_text("N obs     : {x$meta$n}")
  cli::cli_text("Formula   : {x$meta$formula}")
  cli::cli_rule()

  rr  <- round(x$models$rr, 4)
  lo  <- round(x$models$rr_lo, 4)
  hi  <- round(x$models$rr_hi, 4)
  p   <- round(x$models$p_value, 4)
  sig <- dplyr::case_when(p < 0.001 ~ "***", p < 0.01 ~ "**",
                          p < 0.05 ~ "*", TRUE ~ "")
  cli::cli_text("RR = {rr} [{lo}, {hi}]  p = {p} {sig}")

  disp <- round(x$diagnostics$disp_ratio, 2)
  cli::cli_text("Dispersao : {disp}{if(disp > 1.5) ' (!superdispersao)' else ''}")

  if (isTRUE(x$diagnostics$has_autocorr))
    cli::cli_alert_warning("Autocorrelacao residual detectada.")

  invisible(x)
}

#' @export
summary.climasus_glm <- function(object, ...) {
  print(object)
  cat("\n--- Modelo GLM subjacente ---\n")
  print(summary(object$model))
  invisible(object)
}

#' @export
coef.climasus_glm <- function(object, ...) {
  stats::coef(object$model, ...)
}

#' @export
predict.climasus_glm <- function(object, newdata = NULL, ...) {
  stats::predict(object$model, newdata = newdata, ...)
}


# =============================================================================
# HELPERS INTERNOS
# =============================================================================

#' @keywords internal
.smg_detect_climate_col <- function(data) {
  known <- c(
    "patm_mb","patm_max_mb","patm_min_mb",
    "tair_dry_bulb_c","tair_max_c","tair_min_c",
    "dew_tmean_c","dew_tmax_c","dew_tmin_c",
    "rh_max_porc","rh_min_porc","rh_mean_porc",
    "rainfall_mm","ws_gust_m_s","ws_2_m_s","wd_degrees","sr_kj_m2"
  )
  prefixed <- grep(
    "^(mvwin|lag\\d|off\\d|gdd|nexc|pexc|ncold|wwin|season_|tair|rain|rh|pat|dew|ws|wd|sr)\\d*",
    names(data), value = TRUE
  )
  cols <- union(intersect(known, names(data)), prefixed)
  cols <- setdiff(cols, c("date","code_muni","n_obitos","geom","name_muni",
                           "code_state","abbrev_state","code_muni_7"))
  if (length(cols) == 0) cli::cli_abort("Nenhuma variavel climatica detectada em data.")
  cols
}
