#' Comparação de Estratégias Temporais e Modelos Clima-Saúde
#'
#' @description
#' `sus_mod_compare()` ajusta modelos GLM univariados para cada coluna
#' climática presente em um `climasus_df`, permitindo comparar o efeito
#' estimado (RR com IC 95%) entre diferentes estratégias temporais de
#' `sus_climate_aggregate()`. Opcionalmente realiza análise de sensibilidade
#' variando `min_obs`.
#'
#' O objeto retornado (`climasus_compare`) é aceito por
#' `plot_climate_health(fit = ...)`, alimentando automaticamente os plots
#' `coef_compare`, `model_comparison`, `rr_table`, `sensitivity_table` e
#' `cascade` sem re-ajustar modelos.
#'
#' @param data Um `climasus_df` produzido por `sus_climate_aggregate()`.
#'   Pode conter colunas de múltiplas estratégias (e.g., `lag7_tair`,
#'   `mvwin14_tair`, `nexc7_gt35_tair`) para comparação cruzada.
#' @param outcome_col Character. Coluna de desfecho. Padrão: `"n_obitos"`.
#' @param climate_cols Character vector ou `NULL`. Colunas climáticas a
#'   comparar. Se `NULL` (padrão), detecta todas automaticamente.
#' @param family Character. Família GLM para todos os modelos:
#'   `"auto"` (padrão), `"quasipoisson"`, `"poisson"`, `"negbin"`.
#' @param ns_df Integer ou `NULL`. Graus de liberdade para spline temporal.
#'   Padrão: `NULL`.
#' @param alpha Numérico (0–1). Nível de significância. Padrão: `0.05`.
#' @param min_obs_range Vetor numérico ou `NULL`. Se fornecido, calcula
#'   análise de sensibilidade repetindo os modelos para cada valor de
#'   `min_obs`. Padrão: `NULL` (sem análise de sensibilidade).
#'   Exemplo: `c(0.5, 0.6, 0.7, 0.8, 0.9)`.
#' @param spatial_rr Lógico. Se `TRUE`, calcula RR por `code_muni` para
#'   a variável com menor AIC. Padrão: `FALSE`.
#' @param lang Character. Idioma das mensagens. Padrão: `"pt"`.
#' @param verbose Lógico. Exibe progresso. Padrão: `TRUE`.
#'
#' @return Um objeto `climasus_compare` (lista) contendo:
#'   \describe{
#'     \item{`$models`}{Tibble com uma linha por variável climática:
#'       `variable`, `strategy`, `n`, `family`, `estimate`, `se`,
#'       `lo`, `hi`, `rr`, `rr_lo`, `rr_hi`, `p_value`, `aic`, `bic`,
#'       `disp_ratio`.}
#'     \item{`$best`}{Nome da variável com menor AIC.}
#'     \item{`$sensitivity`}{Tibble de sensibilidade por `min_obs`
#'       (apenas se `min_obs_range` fornecido).}
#'     \item{`$spatial_rr`}{Tibble com RR por município para a melhor
#'       variável (se `spatial_rr = TRUE`).}
#'     \item{`$lag_rr`}{Tibble com RR por defasagem para plots cascade
#'       (apenas quando colunas `_lagN` presentes).}
#'     \item{`$meta`}{Metadados: `n_models`, `outcome_col`, `call_time`.}
#'   }
#'
#' @examples
#' \dontrun{
#' # Compara exact, moving_window e discrete_lag na mesma chamada
#' df_multi <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial,
#'   climate_data = df_inmet,
#'   climate_var = "tair_dry_bulb_c",
#'   temporal_strategy = "exact"
#' ) |>
#'   dplyr::left_join(
#'     sus_climate_aggregate(..., temporal_strategy = "moving_window", window_days = 14),
#'     by = c("date", "code_muni")
#'   )
#'
#' fit_cmp <- sus_mod_compare(
#'   data          = df_multi,
#'   outcome_col   = "n_obitos",
#'   min_obs_range = c(0.5, 0.7, 0.9),
#'   spatial_rr    = TRUE
#' )
#'
#' print(fit_cmp)
#'
#' plots <- plot_climate_health(
#'   data = df_multi,
#'   fit  = fit_cmp,
#'   plot_type = c("coef_compare", "model_comparison",
#'                 "sensitivity_table", "cascade", "spatial_map")
#' )
#' }
#'
#' @seealso [sus_mod_glm()], [sus_mod_dlnm()], [plot_climate_health()]
#' @export
sus_mod_compare <- function(
    data,
    outcome_col   = "n_obitos",
    climate_cols  = NULL,
    family        = "auto",
    ns_df         = NULL,
    alpha         = 0.05,
    min_obs_range = NULL,
    spatial_rr    = FALSE,
    lang          = "pt",
    verbose       = TRUE
) {

  # ── Validação ─────────────────────────────────────────────────────────────
  if (!is.data.frame(data))
    cli::cli_abort("{.arg data} deve ser um data frame ou tibble.")
  if (!outcome_col %in% names(data))
    cli::cli_abort("Coluna de desfecho '{outcome_col}' nao encontrada.")
  family <- match.arg(family, c("auto", "quasipoisson", "poisson", "negbin"))

  # ── Detecta colunas climáticas ────────────────────────────────────────────
  if (is.null(climate_cols)) {
    climate_cols <- .smc_detect_all_climate_cols(data, outcome_col)
    if (verbose)
      cli::cli_alert_info("{length(climate_cols)} coluna(s) climatica(s) detectada(s).")
  }

  invalid <- setdiff(climate_cols, names(data))
  if (length(invalid) > 0)
    cli::cli_abort("Colunas nao encontradas: {paste(invalid, collapse=', ')}")

  # ── Agrega por data (remove estratificação por município) ─────────────────
  all_cols <- c("date", outcome_col, climate_cols)
  df_agg   <- data |>
    sf::st_drop_geometry() |>
    dplyr::select(dplyr::any_of(all_cols)) |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(outcome_col), ~ sum(.x, na.rm = TRUE)),
      dplyr::across(dplyr::all_of(climate_cols), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::rename(y = dplyr::all_of(outcome_col))

  # ── Fórmula base ─────────────────────────────────────────────────────────
  rhs_suffix <- if (!is.null(ns_df)) {
    if (!requireNamespace("splines", quietly = TRUE))
      cli::cli_abort("Pacote 'splines' necessario para ns_df.")
    paste0(" + splines::ns(as.integer(date), df = ", ns_df, ")")
  } else ""

  # ── Ajusta um modelo por variável ─────────────────────────────────────────
  if (verbose) cli::cli_progress_step("Ajustando {length(climate_cols)} modelo(s)...")

  models_list <- lapply(climate_cols, function(col) {
    df_m <- df_agg |>
      dplyr::select(date, y, x = dplyr::all_of(col)) |>
      tidyr::drop_na(x, y)

    if (nrow(df_m) < 10 || stats::var(df_m$x, na.rm = TRUE) < 1e-10)
      return(NULL)

    # Auto-detect family
    fam_used <- family
    if (family == "auto") {
      mod_p  <- tryCatch(
        stats::glm(y ~ x, data = df_m, family = stats::poisson()),
        error = function(e) NULL
      )
      if (!is.null(mod_p)) {
        dr <- mod_p$deviance / mod_p$df.residual
        fam_used <- if (dr > 1.5) "quasipoisson" else "poisson"
      } else {
        fam_used <- "quasipoisson"
      }
    }

    formula_obj <- stats::as.formula(paste0("y ~ x", rhs_suffix))

    mod <- tryCatch({
      if (fam_used == "negbin") {
        if (!requireNamespace("MASS", quietly = TRUE))
          cli::cli_abort("Pacote 'MASS' necessario para negbin.")
        MASS::glm.nb(formula_obj, data = df_m)
      } else {
        fam_obj <- if (fam_used == "poisson") stats::poisson()
                   else stats::quasipoisson()
        stats::glm(formula_obj, data = df_m, family = fam_obj)
      }
    }, error = function(e) NULL)

    if (is.null(mod)) return(NULL)

    coef_s <- stats::coef(summary(mod))
    if (!"x" %in% rownames(coef_s)) return(NULL)

    ci <- tryCatch(stats::confint(mod)["x", ],
                   error = function(e) stats::confint.default(mod)["x", ])

    disp_r  <- mod$deviance / mod$df.residual
    aic_val <- tryCatch(stats::AIC(mod),  error = function(e) NA_real_)
    bic_val <- tryCatch(stats::BIC(mod),  error = function(e) NA_real_)

    tibble::tibble(
      variable   = col,
      strategy   = .smc_strategy_from_col(col),
      n          = nrow(df_m),
      family     = fam_used,
      estimate   = coef_s["x", "Estimate"],
      se         = coef_s["x", "Std. Error"],
      lo         = ci[1],
      hi         = ci[2],
      rr         = exp(coef_s["x", "Estimate"]),
      rr_lo      = exp(ci[1]),
      rr_hi      = exp(ci[2]),
      p_value    = coef_s["x", ncol(coef_s)],
      aic        = aic_val,
      bic        = bic_val,
      disp_ratio = round(disp_r, 3)
    )
  })

  models_tbl <- dplyr::bind_rows(models_list)
  if (verbose) cli::cli_progress_done()

  if (nrow(models_tbl) == 0)
    cli::cli_abort("Nenhum modelo ajustado com sucesso.")

  # ── Melhor modelo (menor AIC; fallback: menor p_value) ───────────────────
  best_col <- if (!all(is.na(models_tbl$aic))) {
    models_tbl$variable[which.min(models_tbl$aic)]
  } else {
    models_tbl$variable[which.min(models_tbl$p_value)]
  }

  if (verbose)
    cli::cli_alert_success(
      "Melhor variavel por AIC: {best_col} ",
      "(RR = {round(models_tbl$rr[models_tbl$variable == best_col], 4)})"
    )

  # ── Análise de sensibilidade (min_obs) ─────────────────────────────────────
  sensitivity_tbl <- NULL
  if (!is.null(min_obs_range) && best_col %in% names(data)) {
    if (verbose) cli::cli_progress_step("Analise de sensibilidade...")

    sensitivity_tbl <- dplyr::bind_rows(lapply(min_obs_range, function(mo) {
      x_all  <- data[[best_col]]
      y_all  <- data[[outcome_col]]
      ok     <- !is.na(x_all) & !is.na(y_all)
      n_na   <- sum(is.na(x_all))
      n_ok   <- sum(ok)

      row_base <- tibble::tibble(
        min_obs  = mo,
        variable = best_col,
        n_valido = n_ok,
        n_na     = n_na,
        pct_na   = round(100 * n_na / length(x_all), 1)
      )

      if (n_ok < 5) {
        return(dplyr::mutate(row_base, rr = NA_real_, lo = NA_real_,
                             hi = NA_real_, p_value = NA_real_))
      }

      fam_s <- if (family %in% c("auto", "quasipoisson")) stats::quasipoisson()
               else stats::poisson()

      tryCatch({
        df_s   <- data.frame(y = y_all[ok], x = x_all[ok])
        mod_s  <- stats::glm(y ~ x, data = df_s, family = fam_s)
        coef_s <- stats::coef(summary(mod_s))["x", ]
        ci_s   <- stats::confint.default(mod_s)["x", ]
        dplyr::mutate(row_base,
          rr      = round(exp(coef_s["Estimate"]), 4),
          lo      = round(exp(ci_s[1]), 4),
          hi      = round(exp(ci_s[2]), 4),
          p_value = round(coef_s[length(coef_s)], 4)
        )
      }, error = function(e) {
        dplyr::mutate(row_base, rr = NA_real_, lo = NA_real_,
                     hi = NA_real_, p_value = NA_real_)
      })
    }))

    if (verbose) cli::cli_progress_done()
  }

  # ── RR por lag (cascade) para colunas _lagN ───────────────────────────────
  lag_rr_tbl <- NULL
  lag_cols   <- dplyr::filter(models_tbl, grepl("_lag\\d+$", .data$variable))
  if (nrow(lag_cols) > 0) {
    lag_rr_tbl <- lag_cols |>
      dplyr::mutate(
        lag    = as.integer(gsub(".*_lag(\\d+)$", "\\1", .data$variable)),
        rr_cum = NA_real_
      ) |>
      dplyr::arrange(.data$lag) |>
      dplyr::select("lag", col_name = "variable", "rr", lo = "rr_lo",
                    hi = "rr_hi", "rr_cum")
    lag_rr_tbl$rr_cum <- cumprod(lag_rr_tbl$rr)
  }

  # ── RR espacial (opcional) ────────────────────────────────────────────────
  sp_rr <- NULL
  if (spatial_rr && "code_muni" %in% names(data) && best_col %in% names(data)) {
    if (verbose) cli::cli_progress_step("Calculando RR por municipio...")

    fam_sp <- if (family %in% c("auto", "quasipoisson")) stats::quasipoisson()
              else stats::poisson()

    sp_rr <- data |>
      sf::st_drop_geometry() |>
      dplyr::select("code_muni",
                    y = dplyr::all_of(outcome_col),
                    x = dplyr::all_of(best_col)) |>
      dplyr::group_by(.data$code_muni) |>
      dplyr::summarise(
        n_obs = dplyr::n(),
        rr    = tryCatch({
          df_sp <- dplyr::cur_data()
          ok    <- !is.na(df_sp$x) & !is.na(df_sp$y)
          if (sum(ok) < 5) return(NA_real_)
          m <- stats::glm(y[ok] ~ x[ok], family = fam_sp)
          exp(stats::coef(m)["x[ok]"])
        }, error = function(e) NA_real_),
        exposure_mean = mean(.data$x, na.rm = TRUE),
        .groups = "drop"
      )

    if (verbose) cli::cli_progress_done()
  }

  # ── Monta objeto climasus_compare ─────────────────────────────────────────
  structure(
    list(
      models      = models_tbl,
      best        = best_col,
      sensitivity = sensitivity_tbl,
      spatial_rr  = sp_rr,
      lag_rr      = lag_rr_tbl,
      meta        = list(
        n_models    = nrow(models_tbl),
        outcome_col = outcome_col,
        family      = family,
        ns_df       = ns_df,
        call_time   = Sys.time()
      )
    ),
    class = "climasus_compare"
  )
}


# =============================================================================
# MÉTODOS S3
# =============================================================================

#' @export
print.climasus_compare <- function(x, ...) {
  cli::cli_h2("Comparacao climasus_compare")
  cli::cli_text("Modelos ajustados : {x$meta$n_models}")
  cli::cli_text("Desfecho          : {x$meta$outcome_col}")
  cli::cli_text("Melhor variavel   : {x$best}")
  cli::cli_rule()

  df_print <- x$models |>
    dplyr::mutate(
      RR_IC = paste0(round(rr, 4), " [", round(rr_lo, 4), ", ", round(rr_hi, 4), "]"),
      sig   = dplyr::case_when(
        p_value < 0.001 ~ "***", p_value < 0.01 ~ "**",
        p_value < 0.05  ~ "*",   TRUE ~ ""
      )
    ) |>
    dplyr::select(variable, strategy, n, RR_IC, p_value, sig, aic, disp_ratio) |>
    dplyr::arrange(dplyr::pick(dplyr::any_of("aic")))

  print(df_print, n = 20)

  if (!is.null(x$sensitivity)) {
    cli::cli_rule()
    cli::cli_text("Sensibilidade (min_obs) para '{x$best}':")
    print(dplyr::select(x$sensitivity, min_obs, n_valido, rr, lo, hi, p_value))
  }

  invisible(x)
}

#' @export
summary.climasus_compare <- function(object, ...) {
  print(object)
  invisible(object)
}


# =============================================================================
# HELPERS INTERNOS
# =============================================================================

#' @keywords internal
.smc_detect_all_climate_cols <- function(data, outcome_col) {
  known <- c(
    "patm_mb","patm_max_mb","patm_min_mb",
    "tair_dry_bulb_c","tair_max_c","tair_min_c",
    "dew_tmean_c","dew_tmax_c","dew_tmin_c",
    "rh_max_porc","rh_min_porc","rh_mean_porc",
    "rainfall_mm","ws_gust_m_s","ws_2_m_s","wd_degrees","sr_kj_m2"
  )
  exclude <- c("date","code_muni","code_muni_7","name_muni","code_state",
               "abbrev_state","geom","geometry", outcome_col)
  prefixed <- grep(
    "^(mvwin|lag\\d|off\\d+to|gdd|nexc|pexc|ncold|wwin|season_|tair|rain|rh|pat|dew|ws|wd|sr)\\d*",
    names(data), value = TRUE
  )
  candidate <- union(intersect(known, names(data)), prefixed)
  setdiff(candidate, exclude)
}

#' @keywords internal
.smc_strategy_from_col <- function(col) {
  dplyr::case_when(
    grepl("^mvwin\\d+_",      col) ~ "moving_window",
    grepl("^lag\\d+_",        col) ~ "discrete_lag",
    grepl("_lag\\d+$",        col) ~ "distributed_lag",
    grepl("^off\\d+to\\d+_",  col) ~ "offset_window",
    grepl("^gdd\\d+_",        col) ~ "degree_days",
    grepl("^(nexc|pexc)\\d+_",col) ~ "threshold_exceedance",
    grepl("^ncold\\d+_",      col) ~ "cold_wave_exceedance",
    grepl("^wwin\\d+_",       col) ~ "weighted_window",
    grepl("^season_",         col) ~ "seasonal",
    TRUE                           ~ "exact"
  )
}
