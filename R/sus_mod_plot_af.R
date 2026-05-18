# =============================================================================
# sus_mod_plot_af.R
# Visualisations from a climasus_af object (Attributable Fraction / Number)
#
# Types:
#   "bar"     -- grouped bar chart of AF% by component (heat / cold / total)
#   "forest"  -- horizontal forest plot of AN +/- CI by component
#   "quantile"-- AF% by exposure quantile band ($by_quantile)
# =============================================================================

utils::globalVariables(c(
  "component", "component_label", "comp_label",
  "af_pct", "af_pct_lo", "af_pct_hi",
  "an", "an_lo", "an_hi",
  "x_val"
))

# -- Local i18n ---------------------------------------------------------------
.af_plot_labels <- list(

  bar_title = list(
    pt = "Fra\u00E7\u00E3o Atribu\u00EDvel por Componente",
    en = "Attributable Fraction by Component",
    es = "Fracci\u00F3n Atribuible por Componente"
  ),
  forest_title = list(
    pt = "N\u00FAmero Atribu\u00EDvel (\u00B1 IC) por Componente",
    en = "Attributable Number (\u00B1 CI) by Component",
    es = "N\u00FAmero Atribuible (\u00B1 IC) por Componente"
  ),
  quantile_title = list(
    pt = "FA por Faixa de Percentil de Exposi\u00E7\u00E3o",
    en = "AF by Exposure Quantile Range",
    es = "FA por Rango de Percentil de Exposici\u00F3n"
  ),

  x_component = list(pt = "Componente",        en = "Component",     es = "Componente"),
  y_af_pct    = list(pt = "FA (%)",             en = "AF (%)",        es = "FA (%)"),
  y_an        = list(pt = "NA (casos)",          en = "AN (cases)",    es = "NA (casos)"),
  x_quantile  = list(
    pt = "Faixa de Percentil",
    en = "Quantile Range",
    es = "Rango de Percentil"
  ),

  comp_total  = list(pt = "Total", en = "Total", es = "Total"),
  comp_heat   = list(pt = "Calor", en = "Heat",  es = "Calor"),
  comp_cold   = list(pt = "Frio",  en = "Cold",  es = "Fr\u00EDo"),

  err_not_af = list(
    pt = "{.arg fit} deve ser um {.cls climasus_af} de {.fn sus_mod_af}.",
    en = "{.arg fit} must be a {.cls climasus_af} from {.fn sus_mod_af}.",
    es = "{.arg fit} debe ser un {.cls climasus_af} de {.fn sus_mod_af}."
  ),
  err_no_quantile = list(
    pt = "Sem dados em {.code fit$by_quantile}. Execute {.fn sus_mod_af} com {.arg pred_at} padr\u00E3o.",
    en = "No data in {.code fit$by_quantile}. Run {.fn sus_mod_af} with default {.arg pred_at}.",
    es = "Sin datos en {.code fit$by_quantile}. Ejecute {.fn sus_mod_af} con {.arg pred_at} predeterminado."
  ),
  warn_lang = list(
    pt = "Idioma '{lang}' n\u00E3o suportado. Usando 'pt'.",
    en = "Language '{lang}' not supported. Using 'pt'.",
    es = "Idioma '{lang}' no admitido. Usando 'pt'."
  )
)

.afpl <- function(key, lang) {
  entry <- .af_plot_labels[[key]]
  if (is.null(entry)) return(key)
  entry[[lang]] %||% entry[["pt"]]
}


# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' Plots and Tables from an Attributable Fraction Analysis
#'
#' Produces bar charts, forest plots, and quantile summaries from a
#' `climasus_af` object returned by [sus_mod_af()]. All visualisation types
#' follow standard environmental epidemiology burden-of-disease reporting.
#'
#' @section Plot types (`type`):
#' | `type` | Description |
#' |--------|-------------|
#' | `"bar"` | Grouped bar chart of AF% with CI by component (heat/cold/total) |
#' | `"forest"` | Horizontal forest plot of AN +/- CI by component |
#' | `"quantile"` | Bar chart of AF% across exposure quantile bands (requires `$by_quantile`) |
#'
#' @param fit A `climasus_af` object produced by [sus_mod_af()].
#' @param type Character. Plot type: `"bar"` (default), `"forest"`, or
#'   `"quantile"`.
#' @param output_type Character. `"plot"` (default), `"table"`, or `"all"`
#'   (named list `$plot`, `$table`, `$data`).
#' @param interactive Logical. `TRUE` converts the ggplot2 output to an
#'   interactive \pkg{plotly} widget. Default `FALSE`.
#' @param base_size Numeric. ggplot2 base font size. Default `12`.
#' @param save_plot Character or `NULL`. File path to save the plot (PNG/PDF
#'   for static; HTML for interactive).
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
#' fit <- sus_mod_dlnm(df_dl, outcome_col = "n_obitos")
#' af  <- sus_mod_af(fit)
#'
#' sus_mod_plot_af(af, type = "bar",      lang = "pt")
#' sus_mod_plot_af(af, type = "forest",   lang = "en")
#' sus_mod_plot_af(af, type = "quantile", lang = "es")
#' out <- sus_mod_plot_af(af, output_type = "all")
#' out$table
#' }
#'
#' @seealso [sus_mod_af()], [sus_mod_plot_dlnm()]
#'
#' @export
#' @importFrom cli cli_h1 cli_alert_success cli_alert_warning cli_abort
#' @importFrom rlang check_installed
#' @importFrom glue glue
sus_mod_plot_af <- function(
    fit,
    type        = c("bar", "forest", "quantile"),
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
      cli::cli_alert_warning(glue::glue(.afpl("warn_lang", "pt")))
      "pt"
    } else {
      lang
    }
  } else {
    match.arg(lang)
  }

  if (!inherits(fit, "climasus_af"))
    cli::cli_abort(.afpl("err_not_af", lang))

  rlang::check_installed("ggplot2", reason = "to create plots with sus_mod_plot_af()")
  if (verbose) cli::cli_h1("climasus4r \u2014 Attributable Fraction Plot")

  # -- Component labels and color palette -------------------------------------
  lbl_total <- .afpl("comp_total", lang)
  lbl_heat  <- .afpl("comp_heat",  lang)
  lbl_cold  <- .afpl("comp_cold",  lang)
  comp_map  <- c(total = lbl_total, heat = lbl_heat, cold = lbl_cold)
  fill_cols <- stats::setNames(
    c("#808080", "#E05C5C", "#4472C4"),
    c(lbl_total, lbl_heat, lbl_cold)
  )

  # -- Build plot and table ---------------------------------------------------
  if (type == "quantile") {
    qtl_data <- fit$by_quantile
    if (is.null(qtl_data) || nrow(qtl_data) == 0L)
      cli::cli_abort(.afpl("err_no_quantile", lang))
    p   <- .afplot_quantile(qtl_data, lbl_heat, lbl_cold, fit$meta, lang, base_size)
    tbl <- qtl_data
  } else {
    dat <- fit$total
    dat$component_label <- factor(
      comp_map[dat$component],
      levels = c(lbl_total, lbl_heat, lbl_cold)
    )
    p   <- if (type == "bar") {
      .afplot_bar(dat, fill_cols, fit$meta, lang, base_size)
    } else {
      .afplot_forest(dat, fill_cols, fit$meta, lang, base_size)
    }
    tbl <- fit$total
  }

  # -- Interactive / save -----------------------------------------------------
  if (interactive) {
    rlang::check_installed("plotly", reason = "for interactive AF plots")
    p <- plotly::ggplotly(p)
  }
  if (!is.null(save_plot)) {
    if (interactive && inherits(p, "plotly")) {
      rlang::check_installed("htmlwidgets", reason = "to save interactive plots as HTML")
      htmlwidgets::saveWidget(p, save_plot, selfcontained = TRUE)
    } else {
      ggplot2::ggsave(save_plot, plot = p, width = 8, height = 5)
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
.afplot_bar <- function(dat, fill_cols, meta, lang, base_size) {
  ggplot2::ggplot(
    dat,
    ggplot2::aes(x = component_label, y = af_pct, fill = component_label)
  ) +
    ggplot2::geom_col(width = 0.6, alpha = 0.85) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = af_pct_lo, ymax = af_pct_hi),
      width = 0.2, linewidth = 0.8, na.rm = TRUE
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    ggplot2::scale_fill_manual(values = fill_cols, guide = "none") +
    ggplot2::labs(
      title    = .afpl("bar_title", lang),
      subtitle = glue::glue("{meta$outcome_col} \u2014 {meta$climate_col}"),
      x        = .afpl("x_component", lang),
      y        = .afpl("y_af_pct", lang)
    ) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold"),
      plot.subtitle      = ggplot2::element_text(color = "gray40"),
      panel.grid.major.x = ggplot2::element_blank()
    )
}

#' @keywords internal
#' @noRd
.afplot_forest <- function(dat, fill_cols, meta, lang, base_size) {
  ggplot2::ggplot(dat, ggplot2::aes(y = component_label, color = component_label)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = an_lo, xmax = an_hi),
      width = 0.25, linewidth = 0.9, na.rm = TRUE,
      orientation = "y"
    ) +
    ggplot2::geom_point(ggplot2::aes(x = an), size = 3.5) +
    ggplot2::scale_color_manual(values = fill_cols, guide = "none") +
    ggplot2::labs(
      title    = .afpl("forest_title", lang),
      subtitle = glue::glue("{meta$outcome_col} \u2014 {meta$climate_col}"),
      x        = .afpl("y_an", lang),
      y        = .afpl("x_component", lang)
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
.afplot_quantile <- function(qtl, lbl_heat, lbl_cold, meta, lang, base_size) {
  if ("quantile_label" %in% names(qtl)) {
    qtl$x_val <- factor(qtl$quantile_label, levels = unique(qtl$quantile_label))
  } else {
    qtl$x_val <- factor(as.character(qtl$quantile_prob))
  }

  fill_vals <- stats::setNames(c("#E05C5C", "#4472C4"), c(lbl_heat, lbl_cold))

  if ("component" %in% names(qtl)) {
    comp_map_q     <- c(hot = lbl_heat, cold = lbl_cold)
    qtl$comp_label <- factor(comp_map_q[qtl$component], levels = c(lbl_heat, lbl_cold))
    p <- ggplot2::ggplot(qtl, ggplot2::aes(x = x_val, y = af_pct, fill = comp_label)) +
      ggplot2::geom_col(alpha = 0.85) +
      ggplot2::scale_fill_manual(
        values = fill_vals,
        name   = .afpl("x_component", lang)
      )
  } else {
    p <- ggplot2::ggplot(qtl, ggplot2::aes(x = x_val, y = af_pct)) +
      ggplot2::geom_col(fill = "#4472C4", alpha = 0.85)
  }

  p +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    ggplot2::labs(
      title    = .afpl("quantile_title", lang),
      subtitle = glue::glue("{meta$outcome_col} \u2014 {meta$climate_col}"),
      x        = .afpl("x_quantile", lang),
      y        = .afpl("y_af_pct", lang)
    ) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold"),
      plot.subtitle      = ggplot2::element_text(color = "gray40"),
      axis.text.x        = ggplot2::element_text(angle = 30, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank()
    )
}
