# =============================================================================
# sus_mod_plot_burden.R
# Visualisations from a climasus_burden object (city-level disease burden)
#
# Types:
#   "lollipop" -- ranked lollipop chart of city burden (AN or excess)
#   "lorenz"   -- Lorenz concentration curve from $concentration
#   "stacked"  -- stacked bar of heat/cold breakdown by city (AF input only)
# =============================================================================

utils::globalVariables(c(
  "city", "rank", "an", "af_pct", "excess",
  "pct_of_total", "cumulative_pct",
  "component", "component_label",
  "rank_pct", "metric_val", "ci_lo", "ci_hi"
))

# -- Local i18n ---------------------------------------------------------------
.burd_plot_labels <- list(

  lollipop_title = list(
    pt = "Carga de Doen\u00E7a por Cidade",
    en = "Disease Burden by City",
    es = "Carga de Enfermedad por Ciudad"
  ),
  lorenz_title = list(
    pt = "Curva de Concentra\u00E7\u00E3o de Lorenz",
    en = "Lorenz Concentration Curve",
    es = "Curva de Concentraci\u00F3n de Lorenz"
  ),
  lorenz_equality = list(
    pt = "Igualdade perfeita",
    en = "Perfect equality",
    es = "Igualdad perfecta"
  ),
  stacked_title = list(
    pt = "Decomposi\u00E7\u00E3o Calor/Frio por Cidade",
    en = "Heat/Cold Decomposition by City",
    es = "Descomposici\u00F3n Calor/Fr\u00EDo por Ciudad"
  ),

  x_city       = list(pt = "Cidade",               en = "City",           es = "Ciudad"),
  y_an         = list(pt = "NA (casos)",            en = "AN (cases)",     es = "NA (casos)"),
  y_excess     = list(pt = "Excesso (casos)",        en = "Excess (cases)", es = "Exceso (casos)"),
  x_cities_pct = list(
    pt = "Cidades acumuladas (%)",
    en = "Cumulative cities (%)",
    es = "Ciudades acumuladas (%)"
  ),
  y_burden_pct = list(
    pt = "Carga acumulada (%)",
    en = "Cumulative burden (%)",
    es = "Carga acumulada (%)"
  ),

  comp_heat = list(pt = "Calor", en = "Heat", es = "Calor"),
  comp_cold = list(pt = "Frio",  en = "Cold", es = "Fr\u00EDo"),

  err_not_burden = list(
    pt = "{.arg x} deve ser um {.cls climasus_burden} de {.fn sus_mod_burden}.",
    en = "{.arg x} must be a {.cls climasus_burden} from {.fn sus_mod_burden}.",
    es = "{.arg x} debe ser un {.cls climasus_burden} de {.fn sus_mod_burden}."
  ),
  err_stacked_excess = list(
    pt = "O tipo 'stacked' requer entrada {.cls climasus_af} com componente 'all'.",
    en = "The 'stacked' type requires {.cls climasus_af} input with component 'all'.",
    es = "El tipo 'stacked' requiere entrada {.cls climasus_af} con componente 'all'."
  ),
  warn_lang = list(
    pt = "Idioma '{lang}' n\u00E3o suportado. Usando 'pt'.",
    en = "Language '{lang}' not supported. Using 'pt'.",
    es = "Idioma '{lang}' no admitido. Usando 'pt'."
  )
)

.bpl <- function(key, lang) {
  entry <- .burd_plot_labels[[key]]
  if (is.null(entry)) return(key)
  entry[[lang]] %||% entry[["pt"]]
}


# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' Plots and Tables from a City-Level Disease Burden Analysis
#'
#' Produces lollipop charts, Lorenz concentration curves, and stacked bar
#' charts from a `climasus_burden` object returned by [sus_mod_burden()].
#'
#' @section Plot types (`type`):
#' | `type` | Description |
#' |--------|-------------|
#' | `"lollipop"` | Ranked lollipop chart of city AN or excess burden (default) |
#' | `"lorenz"` | Lorenz concentration curve showing burden inequality across cities |
#' | `"stacked"` | Stacked heat/cold bar by city (only for AF input with `component = "all"`) |
#'
#' @param x A `climasus_burden` object produced by [sus_mod_burden()].
#' @param type Character. Plot type: `"lollipop"` (default), `"lorenz"`, or
#'   `"stacked"`.
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
#' burden <- sus_mod_burden(af_list, lang = "pt")
#'
#' sus_mod_plot_burden(burden, type = "lollipop", lang = "pt")
#' sus_mod_plot_burden(burden, type = "lorenz",   lang = "en")
#' sus_mod_plot_burden(burden, type = "stacked",  lang = "es")
#' out <- sus_mod_plot_burden(burden, output_type = "all")
#' out$table
#' }
#'
#' @seealso [sus_mod_burden()], [sus_mod_af()], [sus_mod_excess()]
#'
#' @export
#' @importFrom cli cli_h1 cli_alert_success cli_alert_warning cli_abort
#' @importFrom rlang check_installed
#' @importFrom glue glue
sus_mod_plot_burden <- function(
    x,
    type        = c("lollipop", "lorenz", "stacked"),
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
      cli::cli_alert_warning(glue::glue(.bpl("warn_lang", "pt")))
      "pt"
    } else {
      lang
    }
  } else {
    match.arg(lang)
  }

  if (!inherits(x, "climasus_burden"))
    cli::cli_abort(.bpl("err_not_burden", lang))

  rlang::check_installed("ggplot2", reason = "to create plots with sus_mod_plot_burden()")
  if (verbose) cli::cli_h1("climasus4r \u2014 Disease Burden Plot")

  is_af    <- isTRUE(x$meta$input_type == "climasus_af")
  comp_all <- isTRUE(x$meta$component  == "all")

  # -- Build plot and table ---------------------------------------------------
  if (type == "lollipop") {
    p   <- .bplot_lollipop(x, is_af, lang, base_size)
    tbl <- if (is_af && "component" %in% names(x$burden_table)) {
      x$burden_table[x$burden_table$component == "total", ]
    } else {
      x$burden_table
    }

  } else if (type == "lorenz") {
    p   <- .bplot_lorenz(x, lang, base_size)
    tbl <- x$concentration

  } else {  # stacked
    if (!is_af || !comp_all)
      cli::cli_abort(.bpl("err_stacked_excess", lang))
    p   <- .bplot_stacked(x, lang, base_size)
    tbl <- x$burden_table[x$burden_table$component %in% c("heat", "cold"), ]
  }

  # -- Interactive / save -----------------------------------------------------
  if (interactive) {
    rlang::check_installed("plotly", reason = "for interactive burden plots")
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
.bplot_lollipop <- function(x, is_af, lang, base_size) {
  bt   <- x$burden_table
  meta <- x$meta

  # For "all" component AF data, isolate the "total" rows
  if (is_af && "component" %in% names(bt)) {
    bt <- bt[bt$component == "total", ]
  }

  metric_col <- if ("an" %in% names(bt)) "an" else "excess"
  y_label    <- if (metric_col == "an") .bpl("y_an", lang) else .bpl("y_excess", lang)
  lo_col     <- if (metric_col == "an") "an_lo" else "excess_lo"
  hi_col     <- if (metric_col == "an") "an_hi" else "excess_hi"
  has_ci     <- lo_col %in% names(bt) && !all(is.na(bt[[lo_col]]))

  # Sort so highest-burden city is at the top in a horizontal plot
  bt            <- bt[order(bt$rank, decreasing = TRUE), ]
  bt$city       <- factor(bt$city, levels = bt$city)
  bt$metric_val <- bt[[metric_col]]
  # Add CI columns now so they're in the data when ggplot() stores a snapshot
  if (has_ci) {
    bt$ci_lo <- bt[[lo_col]]
    bt$ci_hi <- bt[[hi_col]]
  } else {
    bt$ci_lo <- NA_real_
    bt$ci_hi <- NA_real_
  }

  p <- ggplot2::ggplot(bt, ggplot2::aes(y = city, x = metric_val)) +
    ggplot2::geom_segment(
      ggplot2::aes(y = city, yend = city, x = 0, xend = metric_val),
      color = "#4472C4", linewidth = 0.8
    ) +
    ggplot2::geom_point(color = "#4472C4", size = 3.2) +
    ggplot2::geom_vline(xintercept = 0, color = "gray30", linewidth = 0.5)

  if (has_ci) {
    p <- p + ggplot2::geom_errorbar(
      ggplot2::aes(xmin = ci_lo, xmax = ci_hi),
      width = 0.2, color = "#4472C4", na.rm = TRUE,
      orientation = "y"
    )
  }

  p +
    ggplot2::labs(
      title    = .bpl("lollipop_title", lang),
      subtitle = glue::glue("Top {nrow(bt)} \u2014 {meta$rank_by}"),
      x        = y_label,
      y        = .bpl("x_city", lang)
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
.bplot_lorenz <- function(x, lang, base_size) {
  conc <- x$concentration
  n    <- nrow(conc)
  meta <- x$meta

  conc$rank_pct <- conc$rank / n * 100

  eq_df <- data.frame(rank_pct = c(0, 100), cumulative_pct = c(0, 100))

  # Trapezoidal Gini from the concentration data
  gini <- if (n >= 2L) {
    area_curve <- sum(
      diff(conc$rank_pct) *
        (conc$cumulative_pct[-1L] + conc$cumulative_pct[-n]) / 2
    ) / 100
    round((50 - area_curve) / 50, 3L)
  } else {
    NA_real_
  }
  gini_txt <- if (!is.na(gini)) glue::glue("Gini = {gini}") else ""

  ggplot2::ggplot(conc, ggplot2::aes(x = rank_pct, y = cumulative_pct)) +
    ggplot2::geom_line(
      data = eq_df,
      ggplot2::aes(x = rank_pct, y = cumulative_pct),
      linetype = "dashed", color = "gray50", linewidth = 0.8
    ) +
    ggplot2::geom_area(fill = "#4472C4", alpha = 0.12) +
    ggplot2::geom_line(color = "#4472C4", linewidth = 1.1) +
    ggplot2::geom_point(color = "#4472C4", size = 2.5) +
    ggplot2::annotate(
      "text", x = 65, y = 12,
      label = gini_txt,
      color = "#4472C4", size = base_size * 0.3, hjust = 0
    ) +
    ggplot2::labs(
      title    = .bpl("lorenz_title", lang),
      subtitle = glue::glue(
        "{meta$n_cities} {.bpl('x_city', lang)} \u2014 {meta$rank_by}"
      ),
      caption  = glue::glue("-- {.bpl('lorenz_equality', lang)}"),
      x        = .bpl("x_cities_pct", lang),
      y        = .bpl("y_burden_pct", lang)
    ) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "gray40")
    )
}

#' @keywords internal
#' @noRd
.bplot_stacked <- function(x, lang, base_size) {
  bt   <- x$burden_table
  meta <- x$meta

  # Derive city display order from total-component rank
  tot_order <- bt[bt$component == "total", c("city", "rank")]
  tot_order <- tot_order[order(tot_order$rank, decreasing = TRUE), ]

  dat <- bt[bt$component %in% c("heat", "cold"), ]

  lbl_heat   <- .bpl("comp_heat", lang)
  lbl_cold   <- .bpl("comp_cold", lang)
  comp_map   <- c(heat = lbl_heat, cold = lbl_cold)
  dat$component_label <- factor(
    comp_map[dat$component],
    levels = c(lbl_heat, lbl_cold)
  )
  dat$city <- factor(dat$city, levels = tot_order$city)

  fill_vals <- stats::setNames(c("#E05C5C", "#4472C4"), c(lbl_heat, lbl_cold))

  ggplot2::ggplot(dat, ggplot2::aes(x = city, y = an, fill = component_label)) +
    ggplot2::geom_col(alpha = 0.85) +
    ggplot2::scale_fill_manual(values = fill_vals, name = NULL) +
    ggplot2::labs(
      title    = .bpl("stacked_title", lang),
      subtitle = glue::glue("{meta$n_cities} {.bpl('x_city', lang)}"),
      x        = .bpl("x_city", lang),
      y        = .bpl("y_an", lang)
    ) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold"),
      plot.subtitle      = ggplot2::element_text(color = "gray40"),
      axis.text.x        = ggplot2::element_text(angle = 30, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position    = "top"
    )
}
