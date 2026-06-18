# =============================================================================
# sus_mod_plot_pool.R
# Visualisations from a climasus_pool object (pooled DLNM meta-analysis)
#
# Types:
#   "overall"   -- pooled exposure-response curve with CI ribbon
#   "forest"    -- city-specific RR forest plot (original + BLUP)
#   "spaghetti" -- city BLUP curves overlaid on the pooled curve
# =============================================================================

utils::globalVariables(c(
  "exposure", "rr", "lo", "hi",
  "city", "n", "blup_rr", "blup_lo", "blup_hi",
  "lag", "rr_lag"
))

# -- Local i18n ---------------------------------------------------------------
.pool_plot_labels <- list(

  overall_title = list(
    pt = "Curva Exposi\u00E7\u00E3o-Resposta Agrupada",
    en = "Pooled Exposure-Response Curve",
    es = "Curva Exposici\u00F3n-Respuesta Agrupada"
  ),
  forest_title = list(
    pt = "Estimativas por Cidade (\u00B1 IC) \u2014 p75 da Exposi\u00E7\u00E3o",
    en = "City-Specific Estimates (\u00B1 CI) \u2014 Exposure p75",
    es = "Estimativas por Ciudad (\u00B1 IC) \u2014 p75 de Exposici\u00F3n"
  ),
  spaghetti_title = list(
    pt = "Curvas BLUP por Cidade + Estimativa Agrupada",
    en = "City BLUP Curves + Pooled Estimate",
    es = "Curvas BLUP por Ciudad + Estimativa Agrupada"
  ),

  x_exposure = list(
    pt = "Exposi\u00E7\u00E3o",
    en = "Exposure",
    es = "Exposici\u00F3n"
  ),
  y_rr   = list(pt = "RR (IC 95%)",  en = "RR (95% CI)", es = "RR (IC 95%)"),
  y_city = list(pt = "Cidade",        en = "City",        es = "Ciudad"),

  forest_cap_blup = list(
    pt = "\u25C6 estimativa original  \u25CF BLUP",
    en = "\u25C6 original estimate  \u25CF BLUP",
    es = "\u25C6 estimativa original  \u25CF BLUP"
  ),
  cities = list(pt = "cidades", en = "cities", es = "ciudades"),

  err_not_pool = list(
    pt = "{.arg x} deve ser um {.cls climasus_pool} de {.fn sus_mod_pool}.",
    en = "{.arg x} must be a {.cls climasus_pool} from {.fn sus_mod_pool}.",
    es = "{.arg x} debe ser un {.cls climasus_pool} de {.fn sus_mod_pool}."
  ),
  err_no_curve = list(
    pt = "Sem curva agrupada em {.code x$exposure_curve}. O agrupamento falhou.",
    en = "No pooled curve in {.code x$exposure_curve}. Pooling failed.",
    es = "Sin curva agrupada en {.code x$exposure_curve}. El agrupamiento fall\u00F3."
  ),
  warn_no_blup = list(
    pt = "Sem BLUPs em {.code x$blup_preds}. Mostrando apenas a curva agrupada.",
    en = "No BLUPs in {.code x$blup_preds}. Showing pooled curve only.",
    es = "Sin BLUPs en {.code x$blup_preds}. Mostrando solo la curva agrupada."
  ),
  warn_lang = list(
    pt = "Idioma '{lang}' n\u00E3o suportado. Usando 'pt'.",
    en = "Language '{lang}' not supported. Using 'pt'.",
    es = "Idioma '{lang}' no admitido. Usando 'pt'."
  )
)

.poolpl <- function(key, lang) {
  entry <- .pool_plot_labels[[key]]
  if (is.null(entry)) return(key)
  entry[[lang]] %||% entry[["pt"]]
}


# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' Plots and Tables from a Pooled DLNM Meta-Analysis
#'
#' Produces exposure-response curves, city-specific forest plots, and BLUP
#' spaghetti plots from a `climasus_pool` object returned by [sus_mod_pool()].
#'
#' @section Plot types (`type`):
#' | `type` | Description |
#' |--------|-------------|
#' | `"overall"` | Pooled exposure-response curve with CI ribbon |
#' | `"forest"` | City-specific RR at exposure p75 with CI (original and BLUP) |
#' | `"spaghetti"` | Per-city BLUP curves overlaid on the pooled curve |
#'
#' @param x A `climasus_pool` object produced by [sus_mod_pool()].
#' @param type Character. Plot type: `"overall"` (default), `"forest"`, or
#'   `"spaghetti"`.
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
#' pool <- sus_mod_pool(fits, lang = "pt")
#'
#' sus_mod_plot_pool(pool, type = "overall",   lang = "pt")
#' sus_mod_plot_pool(pool, type = "forest",    lang = "en")
#' sus_mod_plot_pool(pool, type = "spaghetti", lang = "en")
#' out <- sus_mod_plot_pool(pool, output_type = "all")
#' out$table
#' }
#'
#' @seealso [sus_mod_pool()], [sus_mod_plot_dlnm()]
#'
#' @export
#' @importFrom cli cli_h1 cli_alert_success cli_alert_warning cli_abort
#' @importFrom rlang check_installed
#' @importFrom glue glue
#' @importFrom purrr imap compact list_rbind
#' @importFrom tibble tibble
sus_mod_plot_pool <- function(
    x,
    type        = c("overall", "forest", "spaghetti"),
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
      cli::cli_alert_warning(glue::glue(.poolpl("warn_lang", "pt")))
      "pt"
    } else {
      lang
    }
  } else {
    match.arg(lang)
  }

  if (!inherits(x, "climasus_pool"))
    cli::cli_abort(.poolpl("err_not_pool", lang))

  rlang::check_installed("ggplot2", reason = "to create plots with sus_mod_plot_pool()")
  if (verbose) cli::cli_h1("climasus4r \u2014 Pooled DLNM Plot")

  # -- Build plot and table ---------------------------------------------------
  if (type == "overall") {
    if (is.null(x$exposure_curve) || nrow(x$exposure_curve) == 0L)
      cli::cli_abort(.poolpl("err_no_curve", lang))
    p   <- .poolplot_overall(x, lang, base_size)
    tbl <- x$exposure_curve

  } else if (type == "forest") {
    if (is.null(x$city_table) || nrow(x$city_table) == 0L)
      cli::cli_abort(.poolpl("err_no_curve", lang))
    p   <- .poolplot_forest(x, lang, base_size)
    tbl <- x$city_table

  } else {  # spaghetti
    blup <- x$blup_preds
    if (is.null(blup) || all(vapply(blup, is.null, logical(1L)))) {
      cli::cli_alert_warning(.poolpl("warn_no_blup", lang))
      if (is.null(x$exposure_curve) || nrow(x$exposure_curve) == 0L)
        cli::cli_abort(.poolpl("err_no_curve", lang))
      p <- .poolplot_overall(x, lang, base_size)
    } else {
      p <- .poolplot_spaghetti(x, lang, base_size)
    }
    tbl <- x$city_table
  }

  # -- Interactive / save -----------------------------------------------------
  if (interactive) {
    rlang::check_installed("plotly", reason = "for interactive pool plots")
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
.poolplot_overall <- function(x, lang, base_size) {
  ec   <- x$exposure_curve
  meta <- x$meta

  ggplot2::ggplot(ec, ggplot2::aes(x = exposure, y = rr)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi),
                         fill = "#4472C4", alpha = 0.15) +
    ggplot2::geom_line(color = "#4472C4", linewidth = 1.1) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "gray30") +
    ggplot2::geom_vline(xintercept = meta$ref_value,
                        linetype = "dotted", color = "gray50") +
    ggplot2::labs(
      title    = .poolpl("overall_title", lang),
      subtitle = glue::glue(
        "{meta$outcome_col} \u2014 {meta$climate_col} | {meta$n_cities} {.poolpl('cities', lang)}"
      ),
      x = .poolpl("x_exposure", lang),
      y = .poolpl("y_rr", lang)
    ) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "gray40")
    )
}

#' @keywords internal
#' @noRd
.poolplot_forest <- function(x, lang, base_size) {
  ct   <- x$city_table
  meta <- x$meta

  # Order by ascending RR so highest city is at the top of a horizontal plot
  ct      <- ct[order(ct$rr), ]
  ct$city <- factor(ct$city, levels = ct$city)

  has_blup    <- !all(is.na(ct$blup_rr))
  caption_txt <- if (has_blup) .poolpl("forest_cap_blup", lang) else NULL

  p <- ggplot2::ggplot(ct, ggplot2::aes(y = city)) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = lo, xmax = hi),
                           width = 0.2, linewidth = 0.8, color = "gray55",
                           orientation = "y") +
    ggplot2::geom_point(ggplot2::aes(x = rr), shape = 18L, size = 3.5, color = "gray45")

  if (has_blup) {
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(xmin = blup_lo, xmax = blup_hi),
        width = 0.2, linewidth = 0.8, color = "#4472C4",
        orientation = "y",
        position = ggplot2::position_nudge(y = -0.3),
        na.rm = TRUE
      ) +
      ggplot2::geom_point(
        ggplot2::aes(x = blup_rr),
        shape = 16L, size = 3.0, color = "#4472C4",
        position = ggplot2::position_nudge(y = -0.3),
        na.rm = TRUE
      )
  }

  p +
    ggplot2::labs(
      title    = .poolpl("forest_title", lang),
      subtitle = glue::glue(
        "{meta$outcome_col} \u2014 {meta$climate_col} | {meta$n_cities} {.poolpl('cities', lang)}"
      ),
      caption  = caption_txt,
      x        = .poolpl("y_rr", lang),
      y        = .poolpl("y_city", lang)
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
.poolplot_spaghetti <- function(x, lang, base_size) {
  meta   <- x$meta
  pooled <- x$exposure_curve

  city_curves <- purrr::imap(x$blup_preds, function(pred, nm) {
    if (is.null(pred)) return(NULL)
    tibble::tibble(
      city     = nm,
      exposure = as.numeric(pred$predvar),
      rr       = as.numeric(pred$allRRfit)
    )
  }) |> purrr::compact() |> purrr::list_rbind()

  ggplot2::ggplot() +
    ggplot2::geom_line(
      data = city_curves,
      ggplot2::aes(x = exposure, y = rr, group = city),
      color = "gray70", linewidth = 0.5, alpha = 0.8
    ) +
    ggplot2::geom_ribbon(
      data = pooled,
      ggplot2::aes(x = exposure, ymin = lo, ymax = hi),
      fill = "#4472C4", alpha = 0.15
    ) +
    ggplot2::geom_line(
      data = pooled,
      ggplot2::aes(x = exposure, y = rr),
      color = "#4472C4", linewidth = 1.3
    ) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "gray30") +
    ggplot2::geom_vline(xintercept = meta$ref_value,
                        linetype = "dotted", color = "gray50") +
    ggplot2::labs(
      title    = .poolpl("spaghetti_title", lang),
      subtitle = glue::glue(
        "{meta$outcome_col} \u2014 {meta$climate_col} | {meta$n_cities} {.poolpl('cities', lang)}"
      ),
      x = .poolpl("x_exposure", lang),
      y = .poolpl("y_rr", lang)
    ) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "gray40")
    )
}
