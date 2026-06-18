# =============================================================================
# sus_mod_plot_ml.R
# Visualisations from a climasus_ml object (XGBoost ML model)
#
# Types:
#   "importance" -- horizontal bar chart of feature importance (Gain)
#   "fit"        -- observed vs. CV-predicted scatter with R² / RMSE annotation
#   "cv_log"     -- train/test loss over boosting rounds from xgb.cv()
# =============================================================================

utils::globalVariables(c(
  "Feature", "Gain", "Cover", "Frequency",
  "observed", "fitted", "cv_predicted", "residual",
  "iter", "metric_val", "split_type"
))

# -- Local i18n ---------------------------------------------------------------
.ml_plot_labels <- list(

  importance_title = list(
    pt = "Import\u00e2ncia das Vari\u00e1veis (Gain)",
    en = "Feature Importance (Gain)",
    es = "Importancia de las Variables (Gain)"
  ),
  fit_title = list(
    pt = "Observado vs. Predito (CV fora da amostra)",
    en = "Observed vs. CV Predicted (Out-of-Fold)",
    es = "Observado vs. Predicho (CV fuera de muestra)"
  ),
  cv_log_title = list(
    pt = "Converg\u00eancia do Modelo por Rodada",
    en = "Model Loss by Boosting Round",
    es = "Convergencia del Modelo por Ronda"
  ),

  x_gain     = list(pt = "Ganho (Gain)",   en = "Gain",       es = "Ganancia"),
  y_feature  = list(pt = "Vari\u00e1vel",        en = "Feature",    es = "Variable"),
  x_observed = list(pt = "Observado",       en = "Observed",   es = "Observado"),
  y_cv_pred  = list(pt = "Predito (CV)",    en = "CV Predicted", es = "Predicho (CV)"),
  x_round    = list(pt = "Rodada",          en = "Round",      es = "Ronda"),
  lbl_train  = list(pt = "treino",          en = "train",      es = "entrenamiento"),
  lbl_test   = list(pt = "valida\u00e7\u00e3o (CV)",  en = "CV test",    es = "validaci\u00f3n (CV)"),
  lbl_best   = list(pt = "melhor rodada",   en = "best round", es = "mejor ronda"),

  err_not_ml = list(
    pt = "{.arg x} deve ser um {.cls climasus_ml} de {.fn sus_mod_ml}.",
    en = "{.arg x} must be a {.cls climasus_ml} from {.fn sus_mod_ml}.",
    es = "{.arg x} debe ser un {.cls climasus_ml} de {.fn sus_mod_ml}."
  ),
  warn_no_importance = list(
    pt = "Tabela de import\u00e2ncia vazia. Verifique o modelo.",
    en = "Importance table is empty. Check the model.",
    es = "La tabla de importancia est\u00e1 vac\u00eda. Verifique el modelo."
  ),
  warn_no_cv_log = list(
    pt = "Sem colunas de m\u00e9trica em {.code x$cv_log}. Exibindo apenas iter.",
    en = "No metric columns found in {.code x$cv_log}. Showing iter only.",
    es = "Sin columnas de m\u00e9trica en {.code x$cv_log}. Solo se muestra iter."
  ),
  warn_lang = list(
    pt = "Idioma '{lang}' n\u00e3o suportado. Usando 'pt'.",
    en = "Language '{lang}' not supported. Using 'pt'.",
    es = "Idioma '{lang}' no admitido. Usando 'pt'."
  )
)

.mlpl <- function(key, lang) {
  entry <- .ml_plot_labels[[key]]
  if (is.null(entry)) return(key)
  entry[[lang]] %||% entry[["pt"]]
}


# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' Plots and Tables from an XGBoost Machine Learning Model
#'
#' Produces feature importance charts, observed-vs-predicted scatter plots,
#' and cross-validation loss curves from a `climasus_ml` object returned by
#' [sus_mod_ml()].
#'
#' @section Plot types (`type`):
#' | `type` | Description |
#' |--------|-------------|
#' | `"importance"` | Horizontal bar chart of XGBoost feature Gain (top `n_top` features) |
#' | `"fit"` | Observed vs. out-of-fold CV-predicted scatter with R² and RMSE annotation |
#' | `"cv_log"` | Train/test loss per boosting round from `xgb.cv()`, with best-round marker |
#'
#' @param x A `climasus_ml` object produced by [sus_mod_ml()].
#' @param type Character. Plot type: `"importance"` (default), `"fit"`, or
#'   `"cv_log"`.
#' @param output_type Character. `"plot"` (default), `"table"`, or `"all"`
#'   (named list `$plot`, `$table`, `$data`).
#' @param n_top Integer. Maximum number of features to show in the importance
#'   plot. Default `20L`.
#' @param interactive Logical. `TRUE` converts the ggplot2 output to an
#'   interactive \pkg{plotly} widget. Default `FALSE`.
#' @param base_size Numeric. ggplot2 base font size. Default `12`.
#' @param save_plot Character or `NULL`. File path to save the plot.
#' @param lang Character. Language for labels: `"pt"` (default), `"en"`, `"es"`.
#' @param verbose Logical. Print progress messages. Default `FALSE`.
#'
#' @return Depending on `output_type`:
#'   - `"plot"` -> a `ggplot` or `plotly` object.
#'   - `"table"` -> a `tibble` of the plotted data.
#'   - `"all"` -> a named list: `$plot`, `$table`, `$data`.
#'
#' @examples
#' \dontrun{
#' ml <- sus_mod_ml(df, outcome_col = "n_obitos", feature_cols = c("tmax", "pop"))
#'
#' sus_mod_plot_ml(ml, type = "importance", lang = "pt")
#' sus_mod_plot_ml(ml, type = "fit",        lang = "en")
#' sus_mod_plot_ml(ml, type = "cv_log",     lang = "es")
#' out <- sus_mod_plot_ml(ml, output_type = "all")
#' out$table
#' }
#'
#' @seealso [sus_mod_ml()], [sus_mod_plot_dlnm()]
#'
#' @export
#' @importFrom cli cli_h1 cli_alert_success cli_alert_warning cli_abort
#' @importFrom rlang check_installed
#' @importFrom glue glue
sus_mod_plot_ml <- function(
    x,
    type        = c("importance", "fit", "cv_log"),
    output_type = c("plot", "table", "all"),
    n_top       = 20L,
    interactive = FALSE,
    base_size   = 12L,
    save_plot   = NULL,
    lang        = c("pt", "en", "es"),
    verbose     = FALSE
) {
  type        <- match.arg(type)
  output_type <- match.arg(output_type)
  n_top       <- max(1L, as.integer(n_top))

  lang <- if (is.character(lang) && length(lang) == 1L) {
    if (!lang %in% c("pt", "en", "es")) {
      cli::cli_alert_warning(glue::glue(.mlpl("warn_lang", "pt")))
      "pt"
    } else {
      lang
    }
  } else {
    match.arg(lang)
  }

  if (!inherits(x, "climasus_ml"))
    cli::cli_abort(.mlpl("err_not_ml", lang))

  rlang::check_installed("ggplot2", reason = "to create plots with sus_mod_plot_ml()")
  if (verbose) cli::cli_h1("climasus4r \u2014 ML Model Plot")

  # -- Build plot and table ---------------------------------------------------
  if (type == "importance") {
    p   <- .ml_plot_importance(x, n_top, lang, base_size)
    tbl <- if (nrow(x$importance) > 0L) x$importance[seq_len(min(n_top, nrow(x$importance))), ] else x$importance

  } else if (type == "fit") {
    p   <- .ml_plot_fit(x, lang, base_size)
    tbl <- x$predictions

  } else {  # cv_log
    p   <- .ml_plot_cv_log(x, lang, base_size)
    tbl <- x$cv_log
  }

  # -- Interactive / save -----------------------------------------------------
  if (interactive) {
    rlang::check_installed("plotly", reason = "for interactive ML plots")
    p <- plotly::ggplotly(p)
  }
  if (!is.null(save_plot)) {
    if (interactive && inherits(p, "plotly")) {
      rlang::check_installed("htmlwidgets", reason = "to save interactive plots as HTML")
      htmlwidgets::saveWidget(p, save_plot, selfcontained = TRUE)
    } else {
      ggplot2::ggsave(save_plot, plot = p, width = 9, height = 5)
    }
    if (verbose) cli::cli_alert_success("Plot salvo em {.file {save_plot}}")
  }

  if (output_type == "plot")  return(p)
  if (output_type == "table") return(tbl)
  list(plot = p, table = tbl, data = tbl)
}


# =============================================================================
# INTERNAL BUILDERS
# =============================================================================

#' @keywords internal
#' @noRd
.ml_plot_importance <- function(x, n_top, lang, base_size) {
  imp  <- x$importance
  meta <- x$meta

  if (nrow(imp) == 0L) {
    cli::cli_alert_warning(.mlpl("warn_no_importance", lang))
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No importance data") +
        ggplot2::theme_bw(base_size = base_size)
    )
  }

  imp_top <- imp[seq_len(min(n_top, nrow(imp))), ]

  # Factor Feature so highest Gain appears at the top of the horizontal plot
  imp_top$Feature <- factor(imp_top$Feature, levels = rev(imp_top$Feature))

  ggplot2::ggplot(imp_top, ggplot2::aes(y = Feature, x = Gain)) +
    ggplot2::geom_col(fill = "#4472C4", alpha = 0.85) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(Gain, 3)),
      hjust = -0.15, size = base_size * 0.22, color = "gray20"
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
    ggplot2::labs(
      title    = .mlpl("importance_title", lang),
      subtitle = glue::glue(
        "{meta$outcome_col} | top {nrow(imp_top)} / {nrow(imp)} {.mlpl('y_feature', lang)}"
      ),
      x = .mlpl("x_gain",    lang),
      y = .mlpl("y_feature", lang)
    ) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold"),
      plot.subtitle      = ggplot2::element_text(color = "gray40"),
      panel.grid.major.y = ggplot2::element_blank()
    )
}

#' @keywords internal
#' @noRd
.ml_plot_fit <- function(x, lang, base_size) {
  pred <- x$predictions
  perf <- x$performance
  meta <- x$meta

  r2_txt   <- if (!is.na(perf$R2_cv))
    glue::glue("R\u00b2 (CV) = {round(perf$R2_cv, 3)}") else "R\u00b2 (CV) = NA"
  rmse_txt <- glue::glue("RMSE (CV) = {round(perf$RMSE_cv, 3)}")
  annot    <- paste(r2_txt, rmse_txt, sep = "\n")

  # Axis range for reference line (observed range covers both axes)
  all_vals <- c(pred$observed, pred$cv_predicted)
  ax_min   <- min(all_vals, na.rm = TRUE)
  ax_max   <- max(all_vals, na.rm = TRUE)

  ggplot2::ggplot(pred, ggplot2::aes(x = observed, y = cv_predicted)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
    ggplot2::geom_point(alpha = 0.45, color = "#4472C4", size = 1.8) +
    ggplot2::annotate(
      "text",
      x     = ax_min + 0.05 * (ax_max - ax_min),
      y     = ax_max - 0.05 * (ax_max - ax_min),
      label = annot,
      hjust = 0, vjust = 1,
      size  = base_size * 0.28,
      color = "#4472C4"
    ) +
    ggplot2::labs(
      title    = .mlpl("fit_title", lang),
      subtitle = glue::glue(
        "{meta$outcome_col} | {nrow(pred)} obs | nrounds = {perf$best_nrounds}"
      ),
      x = .mlpl("x_observed", lang),
      y = .mlpl("y_cv_pred",  lang)
    ) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "gray40")
    )
}

#' @keywords internal
#' @noRd
.ml_plot_cv_log <- function(x, lang, base_size) {
  cv_df <- x$cv_log
  meta  <- x$meta

  # Detect train and test *_mean columns (eval_metric is dynamic)
  mean_cols  <- names(cv_df)[grepl("_mean$", names(cv_df))]
  train_col  <- mean_cols[grepl("^train_", mean_cols)][1L]
  test_col   <- mean_cols[grepl("^test_",  mean_cols)][1L]

  if (is.na(train_col) || is.na(test_col)) {
    cli::cli_alert_warning(.mlpl("warn_no_cv_log", lang))
    return(
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No CV metric columns found") +
        ggplot2::theme_bw(base_size = base_size)
    )
  }

  # Derive metric label from column name (e.g. "train_rmse_mean" -> "rmse")
  metric_nm <- sub("^train_", "", sub("_mean$", "", train_col))

  # Localized split labels
  lbl_train <- .mlpl("lbl_train", lang)
  lbl_test  <- .mlpl("lbl_test",  lang)

  cv_long <- data.frame(
    iter       = rep(cv_df[["iter"]], 2L),
    metric_val = c(cv_df[[train_col]], cv_df[[test_col]]),
    split_type = factor(
      rep(c(lbl_train, lbl_test), each = nrow(cv_df)),
      levels = c(lbl_train, lbl_test)
    )
  )

  fill_vals <- stats::setNames(c("gray60", "#4472C4"), c(lbl_train, lbl_test))
  best_nr   <- meta$best_nrounds

  ggplot2::ggplot(cv_long, ggplot2::aes(x = iter, y = metric_val, color = split_type)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_vline(
      xintercept = best_nr,
      linetype   = "dashed",
      color      = "gray35",
      linewidth  = 0.7
    ) +
    ggplot2::annotate(
      "text",
      x = best_nr, y = Inf,
      label  = glue::glue("{.mlpl('lbl_best', lang)}: {best_nr}"),
      hjust  = 1.05, vjust = 1.4,
      size   = base_size * 0.26,
      color  = "gray35"
    ) +
    ggplot2::scale_color_manual(values = fill_vals, name = NULL) +
    ggplot2::labs(
      title    = .mlpl("cv_log_title", lang),
      subtitle = glue::glue(
        "{meta$outcome_col} | {meta$objective} | eta = {meta$eta}"
      ),
      x = .mlpl("x_round", lang),
      y = toupper(metric_nm)
    ) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold"),
      plot.subtitle   = ggplot2::element_text(color = "gray40"),
      legend.position = "top"
    )
}
