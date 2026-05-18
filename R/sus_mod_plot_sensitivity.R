# =============================================================================
# sus_mod_plot_sensitivity.R
# Visualisations from a climasus_sensitivity object (multi-stratum DLNM)
#
# Types:
#   "curves"  -- overlay of all strata ERC curves from $stratum_curves
#   "scatter" -- hot_rr vs cold_rr per stratum from $comparison (size = SI)
#   "bar"     -- horizontal forest of hot/cold RR per stratum from $rr_table
# =============================================================================

utils::globalVariables(c(
  "stratum", "label", "exposure", "rr", "rr_lo", "rr_hi",
  "hot_rr", "cold_rr", "sensitivity_index",
  "component", "component_label"
))

# -- Local i18n ---------------------------------------------------------------
.sns_plot_labels <- list(

  curves_title = list(
    pt = "Curvas Exposi\u00e7\u00e3o-Resposta por Estrato",
    en = "Exposure-Response Curves by Stratum",
    es = "Curvas Exposici\u00f3n-Respuesta por Estrato"
  ),
  scatter_title = list(
    pt = "RR Calor vs. RR Frio por Estrato",
    en = "Hot RR vs. Cold RR by Stratum",
    es = "RR Calor vs. RR Fr\u00edo por Estrato"
  ),
  bar_title = list(
    pt = "Estimativas de RR por Estrato (Calor e Frio)",
    en = "RR Estimates by Stratum (Hot and Cold)",
    es = "Estimaciones de RR por Estrato (Calor y Fr\u00edo)"
  ),

  x_exposure = list(
    pt = "Exposi\u00e7\u00e3o",
    en = "Exposure",
    es = "Exposici\u00f3n"
  ),
  y_rr      = list(pt = "RR (IC 95%)",  en = "RR (95% CI)",  es = "RR (IC 95%)"),
  x_cold_rr = list(pt = "RR Frio",      en = "Cold RR",      es = "RR Fr\u00edo"),
  y_hot_rr  = list(pt = "RR Calor",     en = "Hot RR",       es = "RR Calor"),
  x_rr      = list(pt = "RR (IC 95%)",  en = "RR (95% CI)",  es = "RR (IC 95%)"),
  y_stratum = list(pt = "Estrato",       en = "Stratum",      es = "Estrato"),
  strata    = list(pt = "estratos",      en = "strata",       es = "estratos"),
  comp_hot  = list(pt = "Calor",         en = "Hot",          es = "Calor"),
  comp_cold = list(pt = "Frio",          en = "Cold",         es = "Fr\u00edo"),
  size_si   = list(
    pt = "\u00cdndice de Sensibilidade",
    en = "Sensitivity Index",
    es = "\u00cdndice de Sensibilidad"
  ),

  err_not_sens = list(
    pt = "{.arg x} deve ser um {.cls climasus_sensitivity} de {.fn sus_mod_sensitivity}.",
    en = "{.arg x} must be a {.cls climasus_sensitivity} from {.fn sus_mod_sensitivity}.",
    es = "{.arg x} debe ser un {.cls climasus_sensitivity} de {.fn sus_mod_sensitivity}."
  ),
  warn_lang = list(
    pt = "Idioma '{lang}' n\u00e3o suportado. Usando 'pt'.",
    en = "Language '{lang}' not supported. Using 'pt'.",
    es = "Idioma '{lang}' no admitido. Usando 'pt'."
  )
)

.snpl <- function(key, lang) {
  entry <- .sns_plot_labels[[key]]
  if (is.null(entry)) return(key)
  entry[[lang]] %||% entry[["pt"]]
}


# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' Plots and Tables from a Multi-Stratum Sensitivity Analysis
#'
#' Produces exposure-response curve overlays, hot-vs-cold RR scatter plots,
#' and grouped forest plots from a `climasus_sensitivity` object returned by
#' [sus_mod_sensitivity()].
#'
#' @section Plot types (`type`):
#' | `type` | Description |
#' |--------|-------------|
#' | `"curves"` | Overlay of each stratum's exposure-response curve with CI ribbon |
#' | `"scatter"` | Hot RR vs Cold RR per stratum; point size encodes sensitivity index |
#' | `"bar"` | Horizontal forest plot of hot and cold RR per stratum ordered by SI |
#'
#' @param x A `climasus_sensitivity` object produced by [sus_mod_sensitivity()].
#' @param type Character. Plot type: `"curves"` (default), `"scatter"`, or
#'   `"bar"`.
#' @param output_type Character. `"plot"` (default), `"table"`, or `"all"`
#'   (named list `$plot`, `$table`, `$data`).
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
#' sens <- sus_mod_sensitivity(fits, lang = "pt")
#'
#' sus_mod_plot_sensitivity(sens, type = "curves",  lang = "pt")
#' sus_mod_plot_sensitivity(sens, type = "scatter", lang = "en")
#' sus_mod_plot_sensitivity(sens, type = "bar",     lang = "es")
#' out <- sus_mod_plot_sensitivity(sens, output_type = "all")
#' out$table
#' }
#'
#' @seealso [sus_mod_sensitivity()], [sus_mod_plot_dlnm()]
#'
#' @export
#' @importFrom cli cli_h1 cli_alert_success cli_alert_warning cli_abort
#' @importFrom rlang check_installed
#' @importFrom glue glue
sus_mod_plot_sensitivity <- function(
    x,
    type        = c("curves", "scatter", "bar"),
    output_type = c("plot", "table", "all"),
    interactive = FALSE,
    base_size   = 12L,
    save_plot   = NULL,
    lang        = c("pt", "en", "es"),
    verbose     = FALSE
) {
  type        <- match.arg(type)
  output_type <- match.arg(output_type)
  lang <- if (is.character(lang) && length(lang) == 1L) {
    if (!lang %in% c("pt", "en", "es")) {
      cli::cli_alert_warning(glue::glue(.snpl("warn_lang", "pt")))
      "pt"
    } else {
      lang
    }
  } else {
    match.arg(lang)
  }

  if (!inherits(x, "climasus_sensitivity"))
    cli::cli_abort(.snpl("err_not_sens", lang))

  rlang::check_installed("ggplot2", reason = "to create plots with sus_mod_plot_sensitivity()")
  if (verbose) cli::cli_h1("climasus4r \u2014 Sensitivity Plot")

  # -- Build plot and table ---------------------------------------------------
  if (type == "curves") {
    p   <- .sns_plot_curves(x, lang, base_size)
    tbl <- x$stratum_curves

  } else if (type == "scatter") {
    p   <- .sns_plot_scatter(x, lang, base_size)
    tbl <- x$comparison

  } else {  # bar
    p   <- .sns_plot_bar(x, lang, base_size)
    tbl <- x$rr_table
  }

  # -- Interactive / save -----------------------------------------------------
  if (interactive) {
    rlang::check_installed("plotly", reason = "for interactive sensitivity plots")
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
.sns_plot_curves <- function(x, lang, base_size) {
  sc   <- x$stratum_curves
  meta <- x$meta

  # Order legend by SI descending (highest sensitivity stratum listed first)
  si_order <- x$comparison$label
  sc$label <- factor(sc$label, levels = si_order)

  ggplot2::ggplot(
    sc,
    ggplot2::aes(
      x     = exposure,
      y     = rr,
      color = label,
      fill  = label,
      group = stratum
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = rr_lo, ymax = rr_hi),
      alpha = 0.08, color = NA
    ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "gray30") +
    ggplot2::labs(
      title    = .snpl("curves_title", lang),
      subtitle = glue::glue(
        "{meta$climate_col} | {meta$n_strata} {.snpl('strata', lang)}"
      ),
      x     = .snpl("x_exposure", lang),
      y     = .snpl("y_rr",       lang),
      color = NULL,
      fill  = NULL
    ) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold"),
      plot.subtitle   = ggplot2::element_text(color = "gray40"),
      legend.position = "bottom"
    )
}

#' @keywords internal
#' @noRd
.sns_plot_scatter <- function(x, lang, base_size) {
  comp <- x$comparison
  meta <- x$meta

  ggplot2::ggplot(comp, ggplot2::aes(x = cold_rr, y = hot_rr)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
    ggplot2::geom_abline(
      slope = 1, intercept = 0,
      linetype = "dotted", color = "gray60"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(size = sensitivity_index),
      color = "#4472C4", alpha = 0.85
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      size = base_size * 0.25,
      vjust = -1.2, color = "gray20"
    ) +
    ggplot2::scale_size_continuous(
      name  = .snpl("size_si", lang),
      range = c(3, 9)
    ) +
    ggplot2::labs(
      title    = .snpl("scatter_title", lang),
      subtitle = glue::glue(
        "{meta$climate_col} | {meta$n_strata} {.snpl('strata', lang)}"
      ),
      x = .snpl("x_cold_rr", lang),
      y = .snpl("y_hot_rr",  lang)
    ) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "gray40")
    )
}

#' @keywords internal
#' @noRd
.sns_plot_bar <- function(x, lang, base_size) {
  rr_tbl <- x$rr_table
  meta   <- x$meta

  # Strata ordered by SI descending; highest SI at top of horizontal plot
  si_order <- x$comparison$label

  lbl_hot  <- .snpl("comp_hot",  lang)
  lbl_cold <- .snpl("comp_cold", lang)
  comp_map <- c(hot = lbl_hot, cold = lbl_cold)

  rr_tbl$component_label <- factor(
    comp_map[rr_tbl$component],
    levels = c(lbl_hot, lbl_cold)
  )
  rr_tbl$label <- factor(rr_tbl$label, levels = rev(si_order))

  fill_vals <- stats::setNames(c("#E05C5C", "#4472C4"), c(lbl_hot, lbl_cold))

  ggplot2::ggplot(rr_tbl, ggplot2::aes(y = label, x = rr, color = component_label)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = rr_lo, xmax = rr_hi),
      width       = 0.25,
      linewidth   = 0.8,
      na.rm       = TRUE,
      orientation = "y",
      position    = ggplot2::position_dodge(width = 0.6)
    ) +
    ggplot2::geom_point(
      size     = 3.5,
      position = ggplot2::position_dodge(width = 0.6)
    ) +
    ggplot2::scale_color_manual(values = fill_vals, name = NULL) +
    ggplot2::labs(
      title    = .snpl("bar_title",  lang),
      subtitle = glue::glue(
        "{meta$climate_col} | {meta$n_strata} {.snpl('strata', lang)}"
      ),
      x = .snpl("x_rr",      lang),
      y = .snpl("y_stratum",  lang)
    ) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold"),
      plot.subtitle      = ggplot2::element_text(color = "gray40"),
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position    = "top"
    )
}
