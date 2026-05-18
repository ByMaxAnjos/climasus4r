# =============================================================================
# sus_mod_plot_dlnm.R
# Scientific visualisation and statistical tables for climasus_dlnm models
#
# Theory: Gasparrini et al. (2010, 2011, 2014); Armstrong (2006);
#         Bhaskaran et al. (2013)
# Input : climasus_dlnm object from sus_mod_dlnm()
# Output: ggplot2 / plotly charts + gt statistical tables
# =============================================================================

# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  # from fit$exposure_response
  "exposure", "rr", "lo", "hi", "pct",
  # from fit$lag_response
  "lag", "rr_cum",
  # from fit$data_daily
  "y", "date",
  # reshaped long-form data frames used in internal helpers
  "exp_val", "lag_val", "rr_mat", "lag_label", "exposure_lag0",
  # ggplot computed aesthetics (after_stat)
  "count",
  # columns in er_tbl used with := in dplyr::select
  "percentile", "rr_95ci"
))

# ── Local i18n ────────────────────────────────────────────────────────────────
.dlnm_labels <- list(

  # plot: overall ---------------------------------------------------------------
  overall_title = list(
    pt = "Curva Exposi\u00E7\u00E3o-Resposta Cumulativa",
    en = "Cumulative Exposure-Response Curve",
    es = "Curva Acumulada Exposici\u00F3n-Respuesta"
  ),
  overall_sub = list(
    pt = "Efeito cumulativo da exposi\u00E7\u00E3o clim\u00E1tica sobre o desfecho di\u00E1rio",
    en = "Cumulative effect of climate exposure on daily health outcome",
    es = "Efecto acumulado de la exposici\u00F3n clim\u00E1tica sobre el resultado diario"
  ),
  x_exposure = list(
    pt = "Temperatura (\u00B0C)",
    en = "Temperature (\u00B0C)",
    es = "Temperatura (\u00B0C)"
  ),
  y_rr = list(
    pt = "Risco Relativo (IC 95%)",
    en = "Relative Risk (95% CI)",
    es = "Riesgo Relativo (IC 95%)"
  ),
  y_count = list(
    pt = "Contagem",
    en = "Count",
    es = "Recuento"
  ),
  ref_line = list(
    pt = "Refer\u00EAncia (RR=1)",
    en = "Reference (RR=1)",
    es = "Referencia (RR=1)"
  ),
  ref_value_label = list(
    pt = "Mediana (refer\u00EAncia)",
    en = "Median (reference)",
    es = "Mediana (referencia)"
  ),
  ci_band = list(
    pt = "IC 95%",
    en = "95% CI",
    es = "IC 95%"
  ),
  distribution_sub = list(
    pt = "Distribui\u00E7\u00E3o da exposi\u00E7\u00E3o (dados di\u00E1rios)",
    en = "Exposure distribution (daily data)",
    es = "Distribuci\u00F3n de la exposici\u00F3n (datos diarios)"
  ),

  # plot: lag -------------------------------------------------------------------
  lag_title = list(
    pt = "Curva Lag-Resposta",
    en = "Lag-Response Curve",
    es = "Curva Lag-Respuesta"
  ),
  lag_sub = list(
    pt = "Efeito por tempo de lag (exposi\u00E7\u00E3o no percentil 75)",
    en = "Effect by lag time (exposure at 75th percentile)",
    es = "Efecto por tiempo de lag (exposici\u00F3n en percentil 75)"
  ),
  x_lag = list(
    pt = "Dias de Atraso (lag)",
    en = "Lag (days)",
    es = "Das de Retraso (lag)"
  ),
  rr_specific = list(
    pt = "RR Espec\u00EDfico por Lag",
    en = "Lag-Specific RR",
    es = "RR Espec\u00EDfico por Lag"
  ),
  rr_cumulative = list(
    pt = "RR Cumulativo",
    en = "Cumulative RR",
    es = "RR Acumulado"
  ),

  # plot: surface / contour -----------------------------------------------------
  surface_title = list(
    pt = "Superf\u00EDcie de Resposta DLNM",
    en = "DLNM Response Surface",
    es = "Superficie de Respuesta DLNM"
  ),
  surface_sub = list(
    pt = "Efeito bidimensional exposi\u00E7\u00E3o \u00D7 lag",
    en = "Bidimensional exposure \u00D7 lag effect",
    es = "Efecto bidimensional exposici\u00F3n \u00D7 lag"
  ),
  contour_title = list(
    pt = "Mapa de Contorno DLNM",
    en = "DLNM Contour Map",
    es = "Mapa de Contorno DLNM"
  ),
  z_rr_log = list(
    pt = "log(RR)",
    en = "log(RR)",
    es = "log(RR)"
  ),

  # plot: slice -----------------------------------------------------------------
  slice_title = list(
    pt = "Curvas de Resposta por Lag Espec\u00EDfico",
    en = "Lag-Specific Exposure-Response Curves",
    es = "Curvas de Respuesta por Lag Espec\u00EDfico"
  ),
  slice_sub = list(
    pt = "Perfil dose-resposta em cada tempo de lag",
    en = "Dose-response profile at each lag time",
    es = "Perfil dosis-respuesta en cada tiempo de lag"
  ),
  lag_group = list(
    pt = "Lag (dias)",
    en = "Lag (days)",
    es = "Lag (d\u00EDas)"
  ),

  # plot: distribution ----------------------------------------------------------
  dist_title = list(
    pt = "Distribui\u00E7\u00E3o da Exposi\u00E7\u00E3o Clim\u00E1tica",
    en = "Climate Exposure Distribution",
    es = "Distribuci\u00F3n de la Exposici\u00F3n Clim\u00E1tica"
  ),
  dist_sub = list(
    pt = "Histograma e densidade kernel com percentis-chave",
    en = "Histogram and kernel density with key percentiles",
    es = "Histograma y densidad kernel con percentiles clave"
  ),
  percentile_label = list(
    pt = "Percentil",
    en = "Percentile",
    es = "Percentil"
  ),

  # plot: series ----------------------------------------------------------------
  series_title = list(
    pt = "S\u00E9rie Temporal: Desfecho e Exposi\u00E7\u00E3o",
    en = "Time Series: Outcome and Exposure",
    es = "Serie Temporal: Resultado y Exposici\u00F3n"
  ),
  series_sub = list(
    pt = "Contagem di\u00E1ria de eventos e exposi\u00E7\u00E3o clim\u00E1tica",
    en = "Daily event count and climate exposure",
    es = "Recuento diario de eventos y exposici\u00F3n clim\u00E1tica"
  ),
  outcome_label = list(
    pt = "Desfecho (contagem di\u00E1ria)",
    en = "Outcome (daily count)",
    es = "Resultado (recuento diario)"
  ),
  exposure_label = list(
    pt = "Exposi\u00E7\u00E3o (lag 0)",
    en = "Exposure (lag 0)",
    es = "Exposici\u00F3n (lag 0)"
  ),

  # errors
  err_not_dlnm = list(
    pt = "{.arg fit} deve ser um {.cls climasus_dlnm} de {.fn sus_mod_dlnm}.",
    en = "{.arg fit} must be a {.cls climasus_dlnm} from {.fn sus_mod_dlnm}.",
    es = "{.arg fit} debe ser un {.cls climasus_dlnm} de {.fn sus_mod_dlnm}."
  ),
  err_no_pred = list(
    pt = "O objeto {.arg fit} n\u00E3o cont\u00E9m {.val $pred}. Reajuste com {.fn sus_mod_dlnm}.",
    en = "Object {.arg fit} does not contain {.val $pred}. Re-fit with {.fn sus_mod_dlnm}.",
    es = "El objeto {.arg fit} no contiene {.val $pred}. Re-ajuste con {.fn sus_mod_dlnm}."
  )
)

.dlnml <- function(key, lang) {
  entry <- .dlnm_labels[[key]]
  if (is.null(entry)) return(key)
  entry[[lang]] %||% entry[["pt"]]
}


# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' Scientific Plots and Tables from a DLNM Fit
#'
#' Produces publication-quality visualisations and statistical summary tables
#' from a `climasus_dlnm` object returned by [sus_mod_dlnm()]. Implements all
#' visualisation types standard in distributed lag non-linear model (DLNM)
#' epidemiology: overall exposure-response, lag-response, bidimensional surface,
#' contour map, lag-stratified slices, exposure distribution, and time series.
#'
#' @section Plot types (`type`):
#' | `type` | Description | Key reference |
#' |--------|-------------|---------------|
#' | `"overall"` | Cumulative exposure-response curve with CI + exposure histogram | Gasparrini et al. (2010) |
#' | `"lag"` | Lag-specific and cumulative lag-response at a given exposure | Gasparrini et al. (2010) |
#' | `"surface"` | 3-D bidimensional exposure × lag response surface | Gasparrini (2011) |
#' | `"contour"` | 2-D contour heat map of exposure × lag (publication-friendly) | Gasparrini et al. (2014) |
#' | `"slice"` | Exposure-response curves at specific lag times (multilag) | Armstrong (2006) |
#' | `"distribution"` | Exposure variable histogram/density with quartile markers | Bhaskaran et al. (2013) |
#' | `"series"` | Daily outcome count + exposure time series | Bhaskaran et al. (2013) |
#'
#' @section Statistical table (`output_type = "table"` or `"all"`):
#' When a table is requested, returns a \pkg{gt} object (or tibble fallback)
#' with four sections: (1) model specification, (2) cumulative RR at exposure
#' percentiles, (3) lag peak summary, and (4) model diagnostics following
#' reporting standards in Gasparrini et al. (2014) and Bhaskaran et al. (2013).
#'
#' @param fit A `climasus_dlnm` object produced by [sus_mod_dlnm()].
#' @param type Character. Plot type (see **Plot types** section). Default
#'   `"overall"`.
#' @param output_type Character. What to return: `"plot"` (default), `"table"`,
#'   or `"all"` (named list `$plot`, `$table`, `$data`).
#' @param exposure_at Numeric or `NULL`. Exposure value used as the focal point
#'   for the `"lag"` plot. `NULL` (default) uses the 75th percentile of lag-0
#'   exposure.
#' @param lags_at Integer vector. Lag times shown in the `"slice"` plot.
#'   Default `c(0L, 3L, 7L, 14L, 21L)` — automatically clipped to `lag_max`.
#' @param pred_at Numeric vector (0–1). Quantile probabilities at which to
#'   report cumulative RR in the statistical table. Default:
#'   `c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99)`.
#' @param interactive Logical. `TRUE` → [plotly::ggplotly()] / plotly 3-D for
#'   `"surface"`; `FALSE` (default) → static ggplot2.
#' @param color_palette Character. Name of a \pkg{ggsci} palette. Default
#'   `"npg"`. Other useful options: `"lancet"`, `"jama"`, `"nejm"`, `"aaas"`.
#' @param base_size Numeric. ggplot2 base font size. Default `12`.
#' @param save_plot Character or `NULL`. File path to save the output. PNG/PDF
#'   for static; HTML for interactive.
#' @param lang Character. Language for labels: `"pt"` (default), `"en"`, `"es"`.
#' @param verbose Logical. Print progress messages. Default `FALSE`.
#'
#' @return Depending on `output_type`:
#'   - `"plot"` → a `ggplot` or `plotly` object.
#'   - `"table"` → a `gt_tbl` (or `tibble` when \pkg{gt} is unavailable).
#'   - `"all"` → a named list: `$plot`, `$table`, `$data`.
#'
#' @references
#' Gasparrini, A., Armstrong, B., & Kenward, M.G. (2010). Distributed lag
#' non-linear models. *Statistics in Medicine*, 29(21), 2224–2234.
#' \doi{10.1002/sim.3940}
#'
#' Gasparrini, A. (2011). Distributed lag linear and non-linear models in R:
#' the package dlnm. *Journal of Statistical Software*, 43(8), 1–20.
#' \doi{10.18637/jss.v043.i08}
#'
#' Gasparrini, A., Armstrong, B., & Kenward, M.G. (2014). Reducing and
#' meta-analysing estimates from distributed lag non-linear models.
#' *BMC Medical Research Methodology*, 14, 70. \doi{10.1186/1471-2288-14-70}
#'
#' Armstrong, B. (2006). Models for the relationship between ambient
#' temperature and daily mortality. *Epidemiology*, 17(6), 624–631.
#'
#' Bhaskaran, K., Gasparrini, A., Hajat, S., Smeeth, L., & Armstrong, B.
#' (2013). Time series regression studies in environmental epidemiology.
#' *International Journal of Epidemiology*, 42(4), 1187–1195.
#' \doi{10.1093/ije/dyt092}
#'
#' @examples
#' \dontrun{
#' fit <- sus_mod_dlnm(df_dl, outcome_col = "n_obitos", lang = "pt")
#'
#' # Overall exposure-response curve (static)
#' sus_mod_plot_dlnm(fit, type = "overall", lang = "pt")
#'
#' # Interactive lag-response
#' sus_mod_plot_dlnm(fit, type = "lag", interactive = TRUE, lang = "en")
#'
#' # 3-D surface (interactive only)
#' sus_mod_plot_dlnm(fit, type = "surface", interactive = TRUE)
#'
#' # Full output with table
#' out <- sus_mod_plot_dlnm(fit, type = "overall", output_type = "all", lang = "pt")
#' out$plot
#' out$table
#' }
#'
#' @seealso [sus_mod_dlnm()], [sus_climate_plot_heatwaves()]
#'
#' @export
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom rlang check_installed
#' @importFrom purrr map list_rbind
#' @importFrom tibble tibble
#' @importFrom dplyr select mutate arrange any_of
sus_mod_plot_dlnm <- function(
    fit,
    type          = c("overall", "lag", "surface", "contour", "slice",
                      "distribution", "series"),
    output_type   = c("plot", "table", "all"),
    exposure_at   = NULL,
    lags_at       = c(0L, 3L, 7L, 14L, 21L),
    pred_at       = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99),
    interactive   = FALSE,
    color_palette = "npg",
    base_size     = 12L,
    save_plot     = NULL,
    lang          = c("pt", "en", "es"),
    verbose       = FALSE
) {

  # ── Package checks ──────────────────────────────────────────────────────────
  rlang::check_installed(
    c("ggplot2", "ggsci", "patchwork"),
    reason = "to run sus_mod_plot_dlnm()"
  )
  if (interactive) {
    rlang::check_installed("plotly", reason = "for interactive plots")
  }

  # ── Argument matching ───────────────────────────────────────────────────────
  type        <- match.arg(type)
  output_type <- match.arg(output_type)
  lang        <- match.arg(lang)

  # ── Input validation ────────────────────────────────────────────────────────
  if (!inherits(fit, "climasus_dlnm")) {
    cli::cli_abort(.dlnml("err_not_dlnm", lang))
  }
  if (is.null(fit$pred) || !inherits(fit$pred, "crosspred")) {
    cli::cli_abort(.dlnml("err_no_pred", lang))
  }

  if (verbose) {
    cli::cli_h1(
      if (lang == "pt") "climasus4r \u2014 Visualiza\u00E7\u00E3o DLNM"
      else if (lang == "es") "climasus4r \u2014 Visualizaci\u00F3n DLNM"
      else "climasus4r \u2014 DLNM Visualisation"
    )
    cli::cli_alert_info(
      if (lang == "pt") "Gerando tipo: {.val {type}}"
      else if (lang == "es") "Generando tipo: {.val {type}}"
      else "Generating type: {.val {type}}"
    )
  }

  # ── Build palette ────────────────────────────────────────────────────────────
  pal <- .dlnm_palette(color_palette)

  # ── Generate plot ────────────────────────────────────────────────────────────
  p <- NULL
  if (output_type %in% c("plot", "all")) {
    p <- switch(
      type,
      overall      = .plot_dlnm_overall(fit, pal, base_size, interactive, lang),
      lag          = .plot_dlnm_lag(fit, exposure_at, pal, base_size, interactive, lang),
      surface      = .plot_dlnm_surface(fit, pal, interactive, lang),
      contour      = .plot_dlnm_contour(fit, pal, base_size, interactive, lang),
      slice        = .plot_dlnm_slice(fit, lags_at, pal, base_size, interactive, lang),
      distribution = .plot_dlnm_distribution(fit, pal, base_size, interactive, lang),
      series       = .plot_dlnm_series(fit, pal, base_size, interactive, lang)
    )
  }

  # ── Generate table ───────────────────────────────────────────────────────────
  tbl <- NULL
  if (output_type %in% c("table", "all")) {
    tbl <- .table_dlnm_summary(fit, pred_at, lang)
  }

  # ── Save ─────────────────────────────────────────────────────────────────────
  if (!is.null(save_plot) && !is.null(p)) {
    ext <- tolower(tools::file_ext(save_plot))
    if (interactive && ext == "html") {
      rlang::check_installed("htmlwidgets", reason = "to save HTML plots")
      htmlwidgets::saveWidget(p, file = save_plot, selfcontained = TRUE)
    } else {
      ggplot2::ggsave(filename = save_plot, plot = p,
                      width = 10, height = 7, dpi = 300)
    }
    if (verbose) cli::cli_alert_success("Plot saved to {.path {save_plot}}")
  }

  # ── Return ────────────────────────────────────────────────────────────────────
  if (output_type == "plot")  return(p)
  if (output_type == "table") return(tbl)

  list(
    plot  = p,
    table = tbl,
    data  = list(
      exposure_response = fit$exposure_response,
      lag_response      = fit$lag_response,
      data_daily        = fit$data_daily,
      diagnostics       = fit$diagnostics
    )
  )
}


# =============================================================================
# THEME AND PALETTE HELPERS
# =============================================================================

#' @keywords internal
#' @noRd
.dlnm_theme <- function(base_size = 12) {
  ggplot2::theme_minimal(base_family = "sans", base_size = base_size) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(
        face = "bold", size = base_size + 3, color = "#2C3E50",
        margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size, color = "#555555",
        margin = ggplot2::margin(b = 12)
      ),
      plot.caption  = ggplot2::element_text(
        size = base_size - 2, color = "#888888", hjust = 0,
        margin = ggplot2::margin(t = 8)
      ),
      axis.title    = ggplot2::element_text(
        face = "bold", size = base_size, color = "#34495E"
      ),
      axis.text     = ggplot2::element_text(size = base_size - 1, color = "#666666"),
      legend.title  = ggplot2::element_text(face = "bold", size = base_size - 1),
      legend.text   = ggplot2::element_text(size = base_size - 2),
      legend.position = "bottom",
      panel.grid.minor  = ggplot2::element_blank(),
      panel.grid.major  = ggplot2::element_line(color = "#ECEFF1", linewidth = 0.4),
      panel.border      = ggplot2::element_blank(),
      plot.background   = ggplot2::element_rect(fill = "white", color = NA),
      panel.background  = ggplot2::element_rect(fill = "white", color = NA),
      strip.text        = ggplot2::element_text(
        face = "bold", size = base_size, color = "#2C3E50"
      ),
      strip.background  = ggplot2::element_rect(fill = "#F0F4F8", color = NA)
    )
}

#' @keywords internal
#' @noRd
.dlnm_palette <- function(palette_name) {
  pal_fn <- tryCatch(
    utils::getFromNamespace(paste0("pal_", palette_name), "ggsci"),
    error = function(e) {
      cli::cli_alert_warning("Palette {.val {palette_name}} not found. Using {.val npg}.")
      ggsci::pal_npg
    }
  )
  cols <- pal_fn()(10)
  list(
    main    = cols[[1]],
    ci      = scales::alpha(cols[[1]], 0.18),
    second  = cols[[2]],
    ci2     = scales::alpha(cols[[2]], 0.18),
    neutral = "#CCCCCC",
    ref     = "#E74C3C",
    hot     = cols[[1]],
    cold    = cols[[4]],
    mid     = "white",
    all     = cols
  )
}


# =============================================================================
# PLOT 1: OVERALL EXPOSURE-RESPONSE
# =============================================================================

#' @keywords internal
#' @noRd
.plot_dlnm_overall <- function(fit, pal, base_size, interactive, lang) {

  # Exposure-response tibble (from sus_mod_dlnm pre-computed output)
  er <- fit$exposure_response

  # Extend with a full 100-point smooth curve from pred
  df_curve <- tibble::tibble(
    exposure = as.numeric(fit$pred$predvar),
    rr       = as.numeric(fit$pred$allRRfit),
    lo       = as.numeric(fit$pred$allRRlow),
    hi       = as.numeric(fit$pred$allRRhigh)
  )

  ref_val <- fit$meta$ref_value
  var_lbl <- fit$meta$climate_col

  # Caption with references
  caption_txt <- switch(lang,
    pt = "Fonte: DLNM (Gasparrini et al., 2010, Stat Med). IC 95% pelo m\u00E9todo delta.",
    en = "Source: DLNM (Gasparrini et al., 2010, Stat Med). 95% CI via delta method.",
    es = "Fuente: DLNM (Gasparrini et al., 2010, Stat Med). IC 95% por m\u00E9todo delta."
  )

  # Main panel: exposure-response curve
  p_main <- ggplot2::ggplot(df_curve, ggplot2::aes(x = exposure)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lo, ymax = hi),
      fill = pal$ci, color = NA
    ) +
    ggplot2::geom_line(ggplot2::aes(y = rr), color = pal$main, linewidth = 1) +
    ggplot2::geom_hline(
      yintercept = 1, linetype = "dashed", color = pal$ref, linewidth = 0.7
    ) +
    ggplot2::geom_vline(
      xintercept = ref_val, linetype = "dotted", color = "#555555", linewidth = 0.6
    ) +
    # Mark key percentiles from exposure_response table
    ggplot2::geom_point(
      data = er,
      ggplot2::aes(x = exposure, y = rr),
      color = pal$second, size = 2.5, alpha = 0.9
    ) +
    ggplot2::scale_x_continuous(expand = c(0.01, 0)) +
    ggplot2::scale_y_continuous(expand = c(0.02, 0)) +
    ggplot2::labs(
      title    = .dlnml("overall_title", lang),
      subtitle = .dlnml("overall_sub", lang),
      x        = var_lbl,
      y        = .dlnml("y_rr", lang),
      caption  = caption_txt
    ) +
    .dlnm_theme(base_size)

  # Bottom panel: exposure histogram
  lag0_col <- paste0(fit$meta$climate_col, "_lag0")
  df_hist  <- tibble::tibble(exposure = fit$data_daily[[lag0_col]])

  p_hist <- ggplot2::ggplot(df_hist, ggplot2::aes(x = exposure)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(count)),
      bins = 30L, fill = pal$main, alpha = 0.4, color = "white"
    ) +
    ggplot2::geom_vline(
      xintercept = ref_val, linetype = "dotted", color = "#555555", linewidth = 0.6
    ) +
    ggplot2::scale_x_continuous(expand = c(0.01, 0)) +
    ggplot2::labs(
      x = var_lbl, y = .dlnml("y_count", lang),
      subtitle = .dlnml("distribution_sub", lang)
    ) +
    .dlnm_theme(base_size - 1) +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(size = base_size - 2),
      plot.margin   = ggplot2::margin(t = 4, b = 4, l = 0, r = 0)
    )

  # Stack panels with patchwork
  p <- patchwork::wrap_plots(p_main, p_hist, ncol = 1L, heights = c(3, 1))

  if (interactive) {
    p_int <- plotly::ggplotly(p_main, tooltip = c("x", "y")) |>
      plotly::layout(
        hoverlabel = list(bgcolor = "white"),
        margin     = list(t = 50, b = 50, l = 50, r = 30)
      ) |>
      plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
    return(p_int)
  }
  p
}


# =============================================================================
# PLOT 2: LAG-RESPONSE
# =============================================================================

#' @keywords internal
#' @noRd
.plot_dlnm_lag <- function(fit, exposure_at, pal, base_size, interactive, lang) {

  lag_seq <- as.integer(seq(fit$pred$lag[1L], fit$pred$lag[2L]))

  # Determine exposure value for the lag curve
  lag0_col <- paste0(fit$meta$climate_col, "_lag0")
  p75_val  <- stats::quantile(fit$data_daily[[lag0_col]], 0.75, na.rm = TRUE)
  focal_exp <- if (is.null(exposure_at)) p75_val else exposure_at
  focal_idx <- which.min(abs(as.numeric(fit$pred$predvar) - focal_exp))

  # Lag-specific RR at the focal exposure
  df_lag <- tibble::tibble(
    lag    = lag_seq,
    rr     = as.numeric(fit$pred$matRRfit[focal_idx, ]),
    lo     = as.numeric(fit$pred$matRRlow[focal_idx,  ]),
    hi     = as.numeric(fit$pred$matRRhigh[focal_idx, ])
  )

  # Cumulative lag-response (already in fit$lag_response)
  df_cum <- fit$lag_response

  focal_round <- round(focal_exp, 1L)
  subtitle_txt <- switch(lang,
    pt = paste0(.dlnml("lag_sub", lang), " = ", focal_round, "\u00B0C"),
    en = paste0(.dlnml("lag_sub", lang), " = ", focal_round, "\u00B0C"),
    es = paste0(.dlnml("lag_sub", lang), " = ", focal_round, "\u00B0C")
  )

  p <- ggplot2::ggplot(df_lag, ggplot2::aes(x = lag)) +
    # CI band for lag-specific
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lo, ymax = hi),
      fill = pal$ci, color = NA
    ) +
    # Lag-specific RR
    ggplot2::geom_line(
      ggplot2::aes(y = rr, color = .dlnml("rr_specific", lang)),
      linewidth = 1
    ) +
    # Cumulative RR (secondary line)
    ggplot2::geom_line(
      data = df_cum,
      ggplot2::aes(x = lag, y = rr_cum, color = .dlnml("rr_cumulative", lang)),
      linewidth = 1, linetype = "dashed"
    ) +
    ggplot2::geom_hline(
      yintercept = 1, linetype = "dotted", color = pal$ref, linewidth = 0.6
    ) +
    ggplot2::scale_color_manual(
      values = stats::setNames(c(pal$main, pal$second),
                               c(.dlnml("rr_specific", lang), .dlnml("rr_cumulative", lang))),
      name = NULL
    ) +
    ggplot2::scale_x_continuous(
      breaks = lag_seq[seq(1L, length(lag_seq), by = max(1L, length(lag_seq) %/% 8L))]
    ) +
    ggplot2::labs(
      title    = .dlnml("lag_title", lang),
      subtitle = subtitle_txt,
      x        = .dlnml("x_lag", lang),
      y        = .dlnml("y_rr", lang),
      caption  = "Gasparrini et al. (2010); Bhaskaran et al. (2013)"
    ) +
    .dlnm_theme(base_size)

  if (interactive) {
    return(
      plotly::ggplotly(p, tooltip = c("x", "y", "colour")) |>
        plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
    )
  }
  p
}


# =============================================================================
# PLOT 3: 3-D SURFACE
# =============================================================================

#' @keywords internal
#' @noRd
.plot_dlnm_surface <- function(fit, pal, interactive, lang) {

  lag_seq  <- as.integer(seq(fit$pred$lag[1L], fit$pred$lag[2L]))
  exp_vec  <- as.numeric(fit$pred$predvar)
  rr_mat   <- fit$pred$matRRfit           # rows = exposure, cols = lag
  log_mat  <- log(rr_mat)

  title_txt <- .dlnml("surface_title", lang)
  sub_txt   <- .dlnml("surface_sub", lang)

  if (interactive) {
    rlang::check_installed("plotly", reason = "for 3-D surface plots")
    # plotly surface: x=exposure, y=lag, z=matrix (rows=exposure, cols=lag)
    return(
      plotly::plot_ly(
        x = exp_vec,
        y = lag_seq,
        z = rr_mat,
        type       = "surface",
        colorscale = list(
          c(0, pal$cold),
          c(0.5, pal$mid),
          c(1, pal$hot)
        ),
        showscale  = TRUE
      ) |>
        plotly::layout(
          title  = list(text = title_txt, font = list(size = 16)),
          scene  = list(
            xaxis = list(title = fit$meta$climate_col),
            yaxis = list(title = if (lang == "pt") "Lag (dias)" else "Lag (days)"),
            zaxis = list(title = "RR")
          ),
          margin = list(t = 60, b = 30, l = 30, r = 30)
        ) |>
        plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
    )
  }

  # Static fallback: contour (identical to .plot_dlnm_contour)
  .plot_dlnm_contour(fit, pal, 12L, FALSE, lang)
}


# =============================================================================
# PLOT 4: CONTOUR MAP
# =============================================================================

#' @keywords internal
#' @noRd
.plot_dlnm_contour <- function(fit, pal, base_size, interactive, lang) {

  lag_seq  <- as.integer(seq(fit$pred$lag[1L], fit$pred$lag[2L]))
  exp_vec  <- as.numeric(fit$pred$predvar)
  rr_mat   <- fit$pred$matRRfit
  n_exp    <- length(exp_vec)
  n_lag    <- length(lag_seq)

  # Reshape to long format
  df_cont <- tibble::tibble(
    exp_val = rep(exp_vec, times  = n_lag),
    lag_val = rep(lag_seq, each   = n_exp),
    rr_mat  = as.vector(rr_mat)
  ) |>
    dplyr::mutate(rr_mat = log(rr_mat))   # log(RR) for diverging scale

  p <- ggplot2::ggplot(
    df_cont,
    ggplot2::aes(x = lag_val, y = exp_val, fill = rr_mat)
  ) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      low      = pal$cold,
      mid      = pal$mid,
      high     = pal$hot,
      midpoint = 0,
      name     = .dlnml("z_rr_log", lang)
    ) +
    ggplot2::geom_contour(
      ggplot2::aes(z = rr_mat),
      breaks  = c(-0.2, -0.1, 0, 0.1, 0.2),
      color   = "white",
      linewidth = 0.3,
      alpha   = 0.6
    ) +
    ggplot2::scale_x_continuous(
      breaks = lag_seq[seq(1L, n_lag, by = max(1L, n_lag %/% 8L))],
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::labs(
      title    = .dlnml("contour_title", lang),
      subtitle = .dlnml("surface_sub", lang),
      x        = .dlnml("x_lag", lang),
      y        = fit$meta$climate_col,
      caption  = "Gasparrini (2011); Gasparrini et al. (2014)"
    ) +
    .dlnm_theme(base_size)

  if (interactive) {
    return(
      plotly::ggplotly(p, tooltip = c("x", "y", "fill")) |>
        plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
    )
  }
  p
}


# =============================================================================
# PLOT 5: LAG-STRATIFIED SLICES
# =============================================================================

#' @keywords internal
#' @noRd
.plot_dlnm_slice <- function(fit, lags_at, pal, base_size, interactive, lang) {

  lag_seq      <- as.integer(seq(fit$pred$lag[1L], fit$pred$lag[2L]))
  selected_lags <- intersect(as.integer(lags_at), lag_seq)
  if (length(selected_lags) == 0L) selected_lags <- lag_seq[seq(1L, length(lag_seq), length.out = 5L)]

  exp_vec  <- as.numeric(fit$pred$predvar)

  df_slice <- purrr::map(selected_lags, function(l) {
    lag_idx  <- which(lag_seq == l)
    tibble::tibble(
      exposure  = exp_vec,
      lag_label = paste("Lag", l),
      rr        = as.numeric(fit$pred$matRRfit[, lag_idx]),
      lo        = as.numeric(fit$pred$matRRlow[,  lag_idx]),
      hi        = as.numeric(fit$pred$matRRhigh[, lag_idx])
    )
  }) |> list_rbind()

  n_lags  <- length(selected_lags)
  pal_vec <- if (n_lags <= length(pal$all)) pal$all[seq_len(n_lags)]
             else grDevices::colorRampPalette(pal$all)(n_lags)

  p <- ggplot2::ggplot(
    df_slice,
    ggplot2::aes(x = exposure, color = lag_label, fill = lag_label)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lo, ymax = hi),
      alpha = 0.10, color = NA
    ) +
    ggplot2::geom_line(ggplot2::aes(y = rr), linewidth = 0.85) +
    ggplot2::geom_hline(
      yintercept = 1, linetype = "dashed", color = pal$ref, linewidth = 0.5
    ) +
    ggplot2::geom_vline(
      xintercept = fit$meta$ref_value,
      linetype = "dotted", color = "#555555", linewidth = 0.5
    ) +
    ggplot2::scale_color_manual(
      values = stats::setNames(pal_vec, paste("Lag", selected_lags)),
      name   = .dlnml("lag_group", lang)
    ) +
    ggplot2::scale_fill_manual(
      values = stats::setNames(pal_vec, paste("Lag", selected_lags)),
      name   = .dlnml("lag_group", lang)
    ) +
    ggplot2::scale_x_continuous(expand = c(0.01, 0)) +
    ggplot2::labs(
      title    = .dlnml("slice_title", lang),
      subtitle = .dlnml("slice_sub", lang),
      x        = fit$meta$climate_col,
      y        = .dlnml("y_rr", lang),
      caption  = "Armstrong (2006); Gasparrini et al. (2014)"
    ) +
    .dlnm_theme(base_size)

  if (interactive) {
    return(
      plotly::ggplotly(p, tooltip = c("x", "y", "colour")) |>
        plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
    )
  }
  p
}


# =============================================================================
# PLOT 6: EXPOSURE DISTRIBUTION
# =============================================================================

#' @keywords internal
#' @noRd
.plot_dlnm_distribution <- function(fit, pal, base_size, interactive, lang) {

  lag0_col <- paste0(fit$meta$climate_col, "_lag0")
  df_dist  <- tibble::tibble(exposure = fit$data_daily[[lag0_col]])

  # Quartile markers
  qtls <- stats::quantile(df_dist$exposure, c(0.10, 0.25, 0.50, 0.75, 0.90, 0.99), na.rm = TRUE)
  pct_names <- c("P10", "P25", "P50\n(ref)", "P75", "P90", "P99")
  df_qtl <- tibble::tibble(
    exposure = as.numeric(qtls),
    pct      = pct_names,
    y        = max(graphics::hist(df_dist$exposure, plot = FALSE)$counts) * 1.05
  )

  p <- ggplot2::ggplot(df_dist, ggplot2::aes(x = exposure)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(count)),
      bins  = 35L,
      fill  = pal$main,
      alpha = 0.45,
      color = "white",
      linewidth = 0.2
    ) +
    ggplot2::geom_density(
      ggplot2::aes(y = ggplot2::after_stat(count)),
      color     = pal$second,
      linewidth = 0.8,
      adjust    = 1.2
    ) +
    ggplot2::geom_vline(
      data        = tibble::tibble(exposure = as.numeric(qtls)),
      ggplot2::aes(xintercept = exposure),
      linetype    = "dashed",
      color       = pal$ref,
      linewidth   = 0.45,
      alpha       = 0.7
    ) +
    ggplot2::geom_text(
      data = df_qtl,
      ggplot2::aes(x = exposure, y = y, label = pct),
      size = base_size * 0.28,
      color = "#333333",
      vjust = 0,
      hjust = 0.5,
      check_overlap = TRUE
    ) +
    ggplot2::scale_x_continuous(expand = c(0.01, 0)) +
    ggplot2::labs(
      title    = .dlnml("dist_title", lang),
      subtitle = .dlnml("dist_sub", lang),
      x        = fit$meta$climate_col,
      y        = .dlnml("y_count", lang),
      caption  = "Bhaskaran et al. (2013)"
    ) +
    .dlnm_theme(base_size)

  if (interactive) {
    return(
      plotly::ggplotly(p, tooltip = c("x", "y")) |>
        plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
    )
  }
  p
}


# =============================================================================
# PLOT 7: TIME SERIES
# =============================================================================

#' @keywords internal
#' @noRd
.plot_dlnm_series <- function(fit, pal, base_size, interactive, lang) {

  lag0_col <- paste0(fit$meta$climate_col, "_lag0")
  df_ts <- fit$data_daily |>
    dplyr::select(date, y, dplyr::any_of(lag0_col)) |>
    dplyr::arrange(date)

  # Normalise exposure to outcome range for dual-axis overlay
  y_range   <- range(df_ts$y, na.rm = TRUE)
  exp_range <- range(df_ts[[lag0_col]], na.rm = TRUE)

  df_ts$exposure_lag0 <- scales::rescale(
    df_ts[[lag0_col]],
    to   = y_range,
    from = exp_range
  )

  # Secondary-axis transform functions
  sec_breaks <- pretty(exp_range, n = 5L)
  sec_breaks_scaled <- scales::rescale(sec_breaks, to = y_range, from = exp_range)

  p <- ggplot2::ggplot(df_ts, ggplot2::aes(x = date)) +
    # Exposure as area
    ggplot2::geom_area(
      ggplot2::aes(y = exposure_lag0),
      fill  = pal$ci,
      color = "transparent"
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = exposure_lag0),
      color     = pal$second,
      linewidth = 0.5,
      alpha     = 0.6
    ) +
    # Outcome as bar + line
    ggplot2::geom_col(
      ggplot2::aes(y = y),
      fill  = pal$main,
      alpha = 0.55,
      width = 1
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = y),
      color     = pal$main,
      linewidth = 0.4,
      alpha     = 0.5
    ) +
    ggplot2::scale_y_continuous(
      name     = .dlnml("outcome_label", lang),
      sec.axis = ggplot2::sec_axis(
        ~ scales::rescale(., to = exp_range, from = y_range),
        name   = .dlnml("exposure_label", lang),
        breaks = sec_breaks
      )
    ) +
    ggplot2::scale_x_date(
      date_labels = "%b\n%Y",
      date_breaks = "3 months",
      expand      = c(0.01, 0)
    ) +
    ggplot2::labs(
      title    = .dlnml("series_title", lang),
      subtitle = .dlnml("series_sub", lang),
      x        = NULL,
      caption  = "Bhaskaran et al. (2013)"
    ) +
    .dlnm_theme(base_size) +
    ggplot2::theme(
      axis.title.y.right = ggplot2::element_text(color = pal$second),
      axis.text.y.right  = ggplot2::element_text(color = pal$second)
    )

  if (interactive) {
    return(
      plotly::ggplotly(p, tooltip = c("x", "y")) |>
        plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
    )
  }
  p
}


# =============================================================================
# TABLE: STATISTICAL SUMMARY
# =============================================================================

#' @keywords internal
#' @noRd
.table_dlnm_summary <- function(fit, pred_at, lang) {

  m    <- fit$models
  diag <- fit$diagnostics
  meta <- fit$meta
  pred <- fit$pred

  # Full exposure-response at requested quantiles
  lag0_col  <- paste0(meta$climate_col, "_lag0")
  expo_vals <- stats::quantile(fit$data_daily[[lag0_col]], pred_at, na.rm = TRUE)

  er_tbl <- purrr::map(seq_along(pred_at), function(i) {
    val <- expo_vals[[i]]
    idx <- which.min(abs(as.numeric(pred$predvar) - val))
    tibble::tibble(
      percentile   = paste0("P", round(pred_at[[i]] * 100)),
      exposure     = round(val, 2L),
      rr           = round(pred$allRRfit[[idx]], 4L),
      lo           = round(pred$allRRlow[[idx]],  4L),
      hi           = round(pred$allRRhigh[[idx]], 4L),
      rr_95ci      = sprintf("%.3f (%.3f\u2013%.3f)",
                             pred$allRRfit[[idx]],
                             pred$allRRlow[[idx]],
                             pred$allRRhigh[[idx]])
    )
  }) |> list_rbind()

  # Peak lag information
  lag_seq  <- as.integer(seq(pred$lag[1L], pred$lag[2L]))
  p75_expo <- stats::quantile(fit$data_daily[[lag0_col]], 0.75, na.rm = TRUE)
  p75_idx  <- which.min(abs(as.numeric(pred$predvar) - p75_expo))
  lag_rr   <- abs(log(as.numeric(pred$matRRfit[p75_idx, ])))
  lag_peak <- lag_seq[which.max(lag_rr)]

  # Date range info
  date_range <- range(fit$data_daily$date, na.rm = TRUE)
  n_years    <- round(as.numeric(diff(date_range)) / 365.25, 1L)

  # ── Build gt table if available ──────────────────────────────────────────────
  if (requireNamespace("gt", quietly = TRUE)) {

    # Section 1 labels
    spec_lbl <- switch(lang,
      pt = c("Par\u00E2metro", "Valor"),
      en = c("Parameter", "Value"),
      es = c("Par\u00E1metro", "Valor")
    )
    spec_rows <- tibble::tibble(
      Parameter = c(
        switch(lang, pt="Vari\u00E1vel clim\u00E1tica", en="Climate variable", es="Variable clim\u00E1tica"),
        switch(lang, pt="Vari\u00E1vel desfecho",       en="Outcome variable",  es="Variable resultado"),
        switch(lang, pt="Fam\u00EDlia GLM",             en="GLM family",        es="Familia GLM"),
        switch(lang, pt="Lag m\u00E1ximo",              en="Maximum lag",       es="Lag m\u00E1ximo"),
        switch(lang, pt="Valor de refer\u00EAncia",     en="Reference value",   es="Valor de referencia"),
        switch(lang, pt="ns_df (controle temporal)",    en="ns_df (time trend)","ns_df (tendencia temporal)"),
        switch(lang, pt="N observa\u00E7\u00F5es",      en="N observations",    es="N observaciones"),
        switch(lang, pt="Per\u00EDodo de estudo",       en="Study period",      es="Per\u00EDodo de estudio"),
        switch(lang, pt="Anos de dados",                en="Years of data",     es="A\u00F1os de datos")
      ),
      Value = c(
        meta$climate_col,
        meta$outcome_col,
        meta$family,
        as.character(meta$lag_max),
        sprintf("%.3f", meta$ref_value),
        as.character(meta$ns_df %||% "auto"),
        formatC(meta$n, format = "d", big.mark = ","),
        sprintf("%s \u2013 %s", format(date_range[1]), format(date_range[2])),
        as.character(n_years)
      )
    )

    # Section 2 labels
    er_section <- er_tbl |>
      dplyr::select(
        !!switch(lang, pt="Percentil", en="Percentile", es="Percentil") := percentile,
        !!switch(lang, pt="Exposi\u00E7\u00E3o", en="Exposure", es="Exposici\u00F3n") := exposure,
        !!switch(lang, pt="RR (IC 95%)", en="RR (95% CI)", es="RR (IC 95%)") := rr_95ci
      )

    # Section 3 diagnostics
    diag_lbl <- switch(lang,
      pt = c("Raz\u00E3o de Dispers\u00E3o", "Categoria dispers\u00E3o",
             "Ljung-Box p-valor", "Autocorr. residual",
             "AIC (Poisson re-fit)", "Desvio residual", "Lag pico (p75)"),
      en = c("Dispersion ratio", "Dispersion category",
             "Ljung-Box p-value", "Residual autocorr.",
             "AIC (Poisson re-fit)", "Residual deviance", "Peak lag (p75)"),
      es = c("Raz\u00F3n de dispersi\u00F3n", "Categor\u00EDa dispersi\u00F3n",
             "Ljung-Box valor p", "Autocorr. residual",
             "AIC (Poisson re-fit)", "Desvio residual", "Lag pico (p75)")
    )
    diag_rows <- tibble::tibble(
      Parameter = diag_lbl,
      Value = c(
        sprintf("%.3f", diag$disp_ratio),
        diag$disp_category,
        sprintf("%.4f", diag$autocorr_pval),
        if (isTRUE(diag$has_autocorr))
          switch(lang, pt="Sim", en="Yes", es="S\u00ED")
        else
          switch(lang, pt="N\u00E3o", en="No", es="No"),
        if (!is.na(diag$aic_poisson)) sprintf("%.1f", diag$aic_poisson) else "NA",
        sprintf("%.1f", diag$deviance),
        as.character(lag_peak)
      )
    )

    # Build gt table
    title_str <- switch(lang,
      pt = "Resumo do Modelo DLNM",
      en = "DLNM Model Summary",
      es = "Resumen del Modelo DLNM"
    )
    subtitle_str <- sprintf(
      "%s | %s | lag 0\u2013%d",
      meta$climate_col, meta$family, meta$lag_max
    )

    tbl <- gt::gt(spec_rows) |>
      gt::tab_header(
        title    = gt::md(paste0("**", title_str, "**")),
        subtitle = subtitle_str
      ) |>
      gt::tab_spanner(
        label   = switch(lang,
          pt="1. Especifica\u00E7\u00E3o do Modelo",
          en="1. Model Specification",
          es="1. Especificaci\u00F3n del Modelo"),
        columns = c("Parameter", "Value")
      ) |>
      gt::cols_label(Parameter = spec_lbl[1], Value = spec_lbl[2]) |>
      gt::tab_source_note(
        gt::md(
          "Gasparrini, A. *et al.* (2010) *Stat Med*; Bhaskaran, K. *et al.* (2013) *Int J Epidemiol*"
        )
      )

    # Add exposure-response section
    tbl2 <- gt::gt(er_section) |>
      gt::tab_header(
        title    = switch(lang,
          pt="2. Curva Exposi\u00E7\u00E3o-Resposta (RR Cumulativo)",
          en="2. Exposure-Response Curve (Cumulative RR)",
          es="2. Curva Exposici\u00F3n-Respuesta (RR Acumulado)"),
        subtitle = sprintf(
          switch(lang,
            pt="Referencia: %s = %.3f",
            en="Reference: %s = %.3f",
            es="Referencia: %s = %.3f"),
          meta$climate_col, meta$ref_value)
      ) |>
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(
          rows = er_section[[1]] == "P75"
        )
      ) |>
      gt::tab_source_note(
        gt::md("RR > 1 indicates increased risk; < 1 protective. Reference = median exposure.")
      )

    # Add diagnostics section
    tbl3 <- gt::gt(diag_rows) |>
      gt::tab_header(
        title = switch(lang,
          pt="3. Diagn\u00F3sticos do Modelo",
          en="3. Model Diagnostics",
          es="3. Diagn\u00F3sticos del Modelo")
      ) |>
      gt::tab_style(
        style = gt::cell_fill(color = "#FFF3CD"),
        locations = gt::cells_body(
          rows = diag_rows$Parameter %in% c(diag_lbl[3], diag_lbl[4]) &
                 diag_rows$Value %in% c("Sim", "Yes", "S\u00ED")
        )
      ) |>
      gt::tab_source_note(
        gt::md(
          paste0(
            switch(lang,
              pt="Dispers\u00E3o adequada: \u03C6 < 1.5. ",
              en="Adequate dispersion: \u03C6 < 1.5. ",
              es="Dispersi\u00F3n adecuada: \u03C6 < 1.5. "
            ),
            switch(lang,
              pt="AIC calculado via Poisson (quasi n\u00E3o tem log-verossimilhan\u00E7a; Winkelmann, 2008).",
              en="AIC computed via Poisson re-fit (quasi has no log-likelihood; Winkelmann, 2008).",
              es="AIC calculado v\u00EDa Poisson (quasi no tiene log-verosimilitud; Winkelmann, 2008)."
            )
          )
        )
      )

    # Return a list of three gt tables (section 1, 2, 3)
    return(list(
      model_spec        = tbl,
      exposure_response = tbl2,
      diagnostics       = tbl3
    ))
  }

  # ── Fallback: plain tibble list (no gt) ──────────────────────────────────────
  list(
    exposure_response = er_tbl,
    diagnostics = tibble::tibble(
      indicator = c("disp_ratio", "disp_category", "autocorr_pval",
                    "has_autocorr", "aic_poisson", "lag_peak"),
      value     = c(
        as.character(round(diag$disp_ratio, 3L)),
        diag$disp_category,
        as.character(round(diag$autocorr_pval, 4L)),
        as.character(isTRUE(diag$has_autocorr)),
        if (!is.na(diag$aic_poisson)) as.character(round(diag$aic_poisson, 1L)) else "NA",
        as.character(lag_peak)
      )
    ),
    model_spec = tibble::tibble(
      parameter = c("climate_col", "outcome_col", "family", "lag_max",
                    "ref_value", "ns_df", "n", "n_years"),
      value     = c(
        meta$climate_col, meta$outcome_col, meta$family,
        as.character(meta$lag_max),
        as.character(round(meta$ref_value, 3L)),
        as.character(meta$ns_df %||% "auto"),
        as.character(meta$n),
        as.character(n_years)
      )
    )
  )
}
