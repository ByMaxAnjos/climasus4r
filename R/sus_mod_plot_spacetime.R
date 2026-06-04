# =============================================================================
# sus_mod_plot_spacetime.R
# Visualizations for climasus_spacetime_bayes and climasus_spacetime_exceedance
#
# Types:
#   "rr_map"       -- choropleth of RR by municipality (optionally at one time)
#   "temporal"     -- line chart of posterior mean RR +/- 95% CI over time
#   "interaction"  -- heatmap of space-time interaction (municipality x time)
#   "exceedance"   -- P(RR > threshold) choropleth map
#   "coef"         -- forest plot of fixed effects
#
# Input : climasus_spacetime_bayes  OR  climasus_spacetime_exceedance
# Output: ggplot2 object (or plotly when interactive = TRUE)
#
# References:
#   Knorr-Held, L. (2000). Bayesian modelling of inseparable space-time
#     variation in disease risk. Statistics in Medicine, 19(17-18), 2555-2567.
#   Blangiardo, M. & Cameletti, M. (2015). Spatial and Spatio-temporal
#     Bayesian Models with R-INLA. Wiley.
# =============================================================================

# -- NSE variable declarations -------------------------------------------------
utils::globalVariables(c(
  # rr / exceedance data frames
  "rr_mean", "rr_lower95", "rr_upper95",
  "code_muni", "time_idx",
  "exceedance_prob",
  # interaction / gamma
  "gamma_mean",
  # fixed-effects data frame
  "term", "mean", "lower95", "upper95",
  # dplyr helpers
  "geometry", "reorder",
  # internal temporary columns
  "ci_width", "sig_elevated"
))

# -- Local i18n ----------------------------------------------------------------
.st_plot_labels <- list(

  # rr_map -----------------------------------------------------------------------
  rr_map_title = list(
    pt = "Risco Relativo Espa\u00e7o-Temporal (m\u00e9dia posterior)",
    en = "Space-Time Relative Risk (posterior mean)",
    es = "Riesgo Relativo Espacio-Temporal (media posterior)"
  ),
  rr_map_title_t = list(
    pt = "Risco Relativo Espa\u00e7o-Temporal -- per\u00edodo {t}",
    en = "Space-Time Relative Risk -- period {t}",
    es = "Riesgo Relativo Espacio-Temporal -- per\u00edodo {t}"
  ),
  rr_fill = list(
    pt = "RR",
    en = "RR",
    es = "RR"
  ),
  rr_sig_note = list(
    pt = "Contorno preto: IC95% inferior > 1 (risco significativamente elevado)",
    en = "Black outline: lower 95% CI > 1 (significantly elevated risk)",
    es = "Contorno negro: IC95% inferior > 1 (riesgo significativamente elevado)"
  ),
  facet_time_label = list(
    pt = "Per\u00edodo",
    en = "Period",
    es = "Per\u00edodo"
  ),

  # temporal ---------------------------------------------------------------------
  temporal_title = list(
    pt = "Evolu\u00e7\u00e3o Temporal do Risco Relativo",
    en = "Temporal Trend of Relative Risk",
    es = "Evoluci\u00f3n Temporal del Riesgo Relativo"
  ),
  temporal_sub = list(
    pt = "M\u00e9dia posterior com intervalo de credibilidade de 95%",
    en = "Posterior mean with 95% credible interval",
    es = "Media posterior con intervalo de credibilidad del 95%"
  ),
  x_time = list(
    pt = "\u00cdndice de Tempo",
    en = "Time Index",
    es = "\u00cdndice de Tiempo"
  ),
  y_rr = list(
    pt = "Risco Relativo (IC 95%)",
    en = "Relative Risk (95% CI)",
    es = "Riesgo Relativo (IC 95%)"
  ),
  ref_line = list(
    pt = "Refer\u00eancia (RR=1)",
    en = "Reference (RR=1)",
    es = "Referencia (RR=1)"
  ),

  # interaction ------------------------------------------------------------------
  interaction_title = list(
    pt = "Intera\u00e7\u00e3o Espa\u00e7o-Tempo (efeito diferencial)",
    en = "Space-Time Interaction (differential effect)",
    es = "Interacci\u00f3n Espacio-Tiempo (efecto diferencial)"
  ),
  interaction_sub = list(
    pt = "M\u00e9dia posterior do componente de intera\u00e7\u00e3o (\u03b3)",
    en = "Posterior mean of the interaction component (\u03b3)",
    es = "Media posterior del componente de interacci\u00f3n (\u03b3)"
  ),
  x_time_idx = list(
    pt = "Per\u00edodo",
    en = "Period",
    es = "Per\u00edodo"
  ),
  y_muni = list(
    pt = "Munic\u00edpio",
    en = "Municipality",
    es = "Municipio"
  ),
  gamma_fill = list(
    pt = "\u03b3 (intera\u00e7\u00e3o)",
    en = "\u03b3 (interaction)",
    es = "\u03b3 (interacci\u00f3n)"
  ),

  # exceedance -------------------------------------------------------------------
  exc_title = list(
    pt = "Probabilidade de Exceda\u00eancia P(RR > {thr})",
    en = "Exceedance Probability P(RR > {thr})",
    es = "Probabilidad de Excedencia P(RR > {thr})"
  ),
  exc_fill = list(
    pt = "P(RR > {thr})",
    en = "P(RR > {thr})",
    es = "P(RR > {thr})"
  ),

  # coef -------------------------------------------------------------------------
  coef_title = list(
    pt = "Efeitos Fixos: M\u00e9dia Posterior (IC 95%)",
    en = "Fixed Effects: Posterior Mean (95% CI)",
    es = "Efectos Fijos: Media Posterior (IC 95%)"
  ),
  coef_x = list(
    pt = "M\u00e9dia Posterior (IC 95%)",
    en = "Posterior Mean (95% CI)",
    es = "Media Posterior (IC 95%)"
  ),
  coef_y = list(
    pt = "Covari\u00e1vel",
    en = "Covariate",
    es = "Covariable"
  ),

  # errors / warnings -----------------------------------------------------------
  err_not_spacetime = list(
    pt = paste0(
      "{.arg x} deve ser um objeto {.cls climasus_spacetime_bayes} ou ",
      "{.cls climasus_spacetime_exceedance} produzido por ",
      "{.fn sus_mod_spacetime_bayes} ou {.fn sus_mod_spacetime_exceedance}."
    ),
    en = paste0(
      "{.arg x} must be a {.cls climasus_spacetime_bayes} or ",
      "{.cls climasus_spacetime_exceedance} object from ",
      "{.fn sus_mod_spacetime_bayes} or {.fn sus_mod_spacetime_exceedance}."
    ),
    es = paste0(
      "{.arg x} debe ser un objeto {.cls climasus_spacetime_bayes} o ",
      "{.cls climasus_spacetime_exceedance} producido por ",
      "{.fn sus_mod_spacetime_bayes} o {.fn sus_mod_spacetime_exceedance}."
    )
  ),
  err_exceedance_type = list(
    pt = paste0(
      "type={.val exceedance} requer um objeto {.cls climasus_spacetime_exceedance}. ",
      "Use {.fn sus_mod_spacetime_exceedance} para gerar esse objeto."
    ),
    en = paste0(
      "type={.val exceedance} requires a {.cls climasus_spacetime_exceedance} object. ",
      "Use {.fn sus_mod_spacetime_exceedance} to generate it."
    ),
    es = paste0(
      "type={.val exceedance} requiere un objeto {.cls climasus_spacetime_exceedance}. ",
      "Use {.fn sus_mod_spacetime_exceedance} para generarlo."
    )
  ),
  err_not_sf = list(
    pt = "{.arg municipalities} deve ser um objeto {.cls sf} com a coluna {.val code_muni}.",
    en = "{.arg municipalities} must be an {.cls sf} object with a {.val code_muni} column.",
    es = "{.arg municipalities} debe ser un objeto {.cls sf} con la columna {.val code_muni}."
  ),
  err_type_needs_sf = list(
    pt = "type={.val {type}} requer o argumento {.arg municipalities} (objeto sf).",
    en = "type={.val {type}} requires the {.arg municipalities} argument (sf object).",
    es = "type={.val {type}} requiere el argumento {.arg municipalities} (objeto sf)."
  ),
  err_no_fixed = list(
    pt = "Nenhum efeito fixo encontrado em {.code x$fixed}. Ajuste o modelo com covari\u00e1veis.",
    en = "No fixed effects found in {.code x$fixed}. Fit the model with covariates.",
    es = "No se encontraron efectos fijos en {.code x$fixed}. Ajuste el modelo con covariables."
  ),
  err_no_rr = list(
    pt = "O objeto {.arg x} n\u00e3o cont\u00e9m {.code $rr}. Verifique o objeto de entrada.",
    en = "Object {.arg x} does not contain {.code $rr}. Check the input object.",
    es = "El objeto {.arg x} no contiene {.code $rr}. Compruebe el objeto de entrada."
  ),
  err_no_interaction = list(
    pt = paste0(
      "O objeto {.arg x} n\u00e3o cont\u00e9m {.code $interaction}. ",
      "Ajuste o modelo com intera\u00e7\u00e3o espa\u00e7o-tempo."
    ),
    en = paste0(
      "Object {.arg x} does not contain {.code $interaction}. ",
      "Fit the model with space-time interaction."
    ),
    es = paste0(
      "El objeto {.arg x} no contiene {.code $interaction}. ",
      "Ajuste el modelo con interacci\u00f3n espacio-tiempo."
    )
  ),
  err_no_exceedance = list(
    pt = "O objeto {.arg x} n\u00e3o cont\u00e9m {.code $exceedance}. Verifique o objeto de entrada.",
    en = "Object {.arg x} does not contain {.code $exceedance}. Check the input object.",
    es = "El objeto {.arg x} no contiene {.code $exceedance}. Compruebe el objeto de entrada."
  ),
  err_threshold_not_found = list(
    pt = paste0(
      "Limiar {.val {thr}} n\u00e3o encontrado em {.code x$exceedance}. ",
      "Limiares dispon\u00edveis: {.val {avail}}."
    ),
    en = paste0(
      "Threshold {.val {thr}} not found in {.code x$exceedance}. ",
      "Available thresholds: {.val {avail}}."
    ),
    es = paste0(
      "Umbral {.val {thr}} no encontrado en {.code x$exceedance}. ",
      "Umbrales disponibles: {.val {avail}}."
    )
  ),
  warn_lang = list(
    pt = "Idioma '{lang}' n\u00e3o suportado. Usando 'pt'.",
    en = "Language '{lang}' not supported. Using 'pt'.",
    es = "Idioma '{lang}' no admitido. Usando 'pt'."
  ),
  warn_time_range = list(
    pt = paste0(
      "{.arg time_range} ignorado para type={.val {type}}. ",
      "Somente {.val temporal} e {.val interaction} suportam filtragem por tempo."
    ),
    en = paste0(
      "{.arg time_range} ignored for type={.val {type}}. ",
      "Only {.val temporal} and {.val interaction} support time filtering."
    ),
    es = paste0(
      "{.arg time_range} ignorado para type={.val {type}}. ",
      "Solo {.val temporal} e {.val interaction} admiten filtrado por tiempo."
    )
  ),
  warn_plotly = list(
    pt = "Pacote {.pkg plotly} n\u00e3o dispon\u00edvel. Retornando objeto ggplot2 est\u00e1tico.",
    en = "Package {.pkg plotly} not available. Returning static ggplot2 object.",
    es = "Paquete {.pkg plotly} no disponible. Retornando objeto ggplot2 est\u00e1tico."
  )
)

# -- Internal i18n helper ------------------------------------------------------
#' @keywords internal
#' @noRd
.stl <- function(key, lang, ...) {
  entry <- .st_plot_labels[[key]]
  if (is.null(entry)) return(key)
  msg <- entry[[lang]] %||% entry[["pt"]]
  if (...length() > 0L) msg <- glue::glue(msg, .envir = rlang::env(...))
  msg
}

# -- Internal theme helpers ----------------------------------------------------
#' @keywords internal
#' @noRd
.st_map_theme <- function(base_size = 11) {
  ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(
      legend.position   = "right",
      legend.key.height = ggplot2::unit(1.2, "cm"),
      plot.title        = ggplot2::element_text(
        hjust = 0.5, face = "bold", size = base_size + 1
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5, size = base_size - 1, color = "grey40"
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0, size = base_size - 2, color = "grey50"
      ),
      strip.text = ggplot2::element_text(size = base_size - 1, face = "bold")
    )
}

# -- Internal plot builders ----------------------------------------------------

#' @keywords internal
#' @noRd
.st_plot_rr_map <- function(x, municipalities, time_point, time_range,
                             facet_time, palette, title, base_size, lang) {
  if (is.null(x$rr)) {
    cli::cli_abort(.stl("err_no_rr", lang))
  }

  rr_df <- x$rr
  rr_df$code_muni <- as.character(rr_df$code_muni)
  municipalities$code_muni <- as.character(municipalities$code_muni)

  has_time <- "time_idx" %in% names(rr_df)

  if (has_time && !is.null(time_point) && !facet_time) {
    rr_df <- rr_df[rr_df$time_idx == time_point, ]
  }

  if (has_time && !is.null(time_range) && !facet_time) {
    rr_df <- rr_df[rr_df$time_idx >= time_range[1] &
                     rr_df$time_idx <= time_range[2], ]
  }

  # Aggregate over time when not faceting and data is multi-temporal
  if (has_time && is.null(time_point) && !facet_time) {
    rr_df <- stats::aggregate(
      cbind(rr_mean, rr_lower95, rr_upper95) ~ code_muni,
      data = rr_df,
      FUN  = mean,
      na.rm = TRUE
    )
  }

  rr_sf <- dplyr::left_join(municipalities, rr_df, by = "code_muni")
  rr_sf$sig_elevated <- !is.na(rr_sf$rr_lower95) & rr_sf$rr_lower95 > 1

  default_title <- if (!is.null(time_point) && !facet_time) {
    .stl("rr_map_title_t", lang, t = time_point)
  } else {
    .stl("rr_map_title", lang)
  }

  p <- ggplot2::ggplot(rr_sf) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = rr_mean),
      color    = "grey80",
      linewidth = 0.15
    ) +
    ggplot2::geom_sf(
      data      = rr_sf[!is.na(rr_sf$sig_elevated) & rr_sf$sig_elevated, ],
      fill      = NA,
      color     = "black",
      linewidth  = 0.55
    ) +
    ggplot2::scale_fill_gradient2(
      low      = RColorBrewer::brewer.pal(11, palette)[1],
      mid      = "white",
      high     = RColorBrewer::brewer.pal(11, palette)[11],
      midpoint = 1.0,
      na.value = "grey90",
      name     = .stl("rr_fill", lang)
    ) +
    ggplot2::labs(
      title   = title %||% default_title,
      caption = .stl("rr_sig_note", lang)
    ) +
    .st_map_theme(base_size)

  if (has_time && facet_time) {
    p <- p +
      ggplot2::facet_wrap(
        ~time_idx,
        labeller = ggplot2::labeller(
          time_idx = function(v) paste(.stl("facet_time_label", lang), v)
        )
      )
  }

  p
}

#' @keywords internal
#' @noRd
.st_plot_temporal <- function(x, time_range, title, base_size, lang) {
  if (is.null(x$rr)) {
    cli::cli_abort(.stl("err_no_rr", lang))
  }

  rr_df <- x$rr

  if (!"time_idx" %in% names(rr_df)) {
    # Fall back: treat single-time data as one point
    rr_df$time_idx <- 1L
  }

  if (!is.null(time_range)) {
    rr_df <- rr_df[rr_df$time_idx >= time_range[1] &
                     rr_df$time_idx <= time_range[2], ]
  }

  # Aggregate over municipalities for overall temporal trend
  trend_df <- stats::aggregate(
    cbind(rr_mean, rr_lower95, rr_upper95) ~ time_idx,
    data  = rr_df,
    FUN   = mean,
    na.rm = TRUE
  )

  p <- ggplot2::ggplot(
    trend_df,
    ggplot2::aes(x = time_idx, y = rr_mean)
  ) +
    ggplot2::geom_hline(
      yintercept = 1.0,
      linetype   = 2,
      color      = "grey60",
      linewidth  = 0.5
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = rr_lower95, ymax = rr_upper95),
      fill  = "#2166ac",
      alpha = 0.20
    ) +
    ggplot2::geom_line(color = "#2166ac", linewidth = 0.8) +
    ggplot2::geom_point(color = "#2166ac", size = 1.8) +
    ggplot2::labs(
      title    = title %||% .stl("temporal_title", lang),
      subtitle = .stl("temporal_sub", lang),
      x        = .stl("x_time", lang),
      y        = .stl("y_rr", lang)
    ) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "grey40")
    )

  p
}

#' @keywords internal
#' @noRd
.st_plot_interaction <- function(x, time_range, title, base_size, lang) {
  if (is.null(x$interaction)) {
    cli::cli_abort(.stl("err_no_interaction", lang))
  }

  ia_df <- x$interaction
  ia_df$code_muni <- as.character(ia_df$code_muni)

  if (!is.null(time_range) && "time_idx" %in% names(ia_df)) {
    ia_df <- ia_df[ia_df$time_idx >= time_range[1] &
                     ia_df$time_idx <= time_range[2], ]
  }

  # Sort municipalities by mean gamma for a readable y-axis
  muni_order <- stats::aggregate(
    gamma_mean ~ code_muni,
    data = ia_df,
    FUN  = mean,
    na.rm = TRUE
  )
  muni_order <- muni_order[order(muni_order$gamma_mean), "code_muni"]
  ia_df$code_muni <- factor(ia_df$code_muni, levels = muni_order)

  p <- ggplot2::ggplot(
    ia_df,
    ggplot2::aes(x = time_idx, y = code_muni, fill = gamma_mean)
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.05) +
    ggplot2::scale_fill_gradient2(
      low      = "#313695",
      mid      = "white",
      high     = "#a50026",
      midpoint = 0,
      na.value = "grey90",
      name     = .stl("gamma_fill", lang)
    ) +
    ggplot2::labs(
      title    = title %||% .stl("interaction_title", lang),
      subtitle = .stl("interaction_sub", lang),
      x        = .stl("x_time_idx", lang),
      y        = .stl("y_muni", lang)
    ) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      axis.text.y   = ggplot2::element_text(size = max(base_size - 4, 5)),
      axis.ticks.y  = ggplot2::element_blank(),
      plot.title    = ggplot2::element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "grey40"),
      panel.grid    = ggplot2::element_blank()
    )

  p
}

#' @keywords internal
#' @noRd
.st_plot_exceedance <- function(x, municipalities, threshold, title,
                                base_size, lang) {
  if (is.null(x$exceedance)) {
    cli::cli_abort(.stl("err_no_exceedance", lang))
  }

  exc_list <- x$exceedance

  # exceedance may be a list keyed by threshold or a single data.frame
  if (is.data.frame(exc_list)) {
    exc_df <- exc_list
  } else {
    thr_key <- as.character(threshold)
    avail   <- paste(names(exc_list), collapse = ", ")
    if (!thr_key %in% names(exc_list)) {
      cli::cli_abort(.stl("err_threshold_not_found", lang,
                          thr = thr_key, avail = avail))
    }
    exc_df <- exc_list[[thr_key]]
  }

  exc_df$code_muni    <- as.character(exc_df$code_muni)
  municipalities$code_muni <- as.character(municipalities$code_muni)

  # Aggregate over time if needed
  if ("time_idx" %in% names(exc_df)) {
    exc_df <- stats::aggregate(
      exceedance_prob ~ code_muni,
      data  = exc_df,
      FUN   = mean,
      na.rm = TRUE
    )
  }

  exc_sf <- dplyr::left_join(municipalities, exc_df, by = "code_muni")

  p <- ggplot2::ggplot(exc_sf) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = exceedance_prob),
      color    = "grey80",
      linewidth = 0.15
    ) +
    ggplot2::scale_fill_gradientn(
      colors   = c("white", "#fee08b", "#d73027", "#a50026"),
      limits   = c(0, 1),
      na.value = "grey90",
      name     = .stl("exc_fill", lang, thr = threshold)
    ) +
    ggplot2::labs(
      title = title %||% .stl("exc_title", lang, thr = threshold)
    ) +
    .st_map_theme(base_size)

  p
}

#' @keywords internal
#' @noRd
.st_plot_coef <- function(x, title, base_size, lang) {
  fixed_df <- x$fixed
  if (is.null(fixed_df) || nrow(fixed_df) == 0L) {
    cli::cli_abort(.stl("err_no_fixed", lang))
  }

  p <- ggplot2::ggplot(
    fixed_df,
    ggplot2::aes(
      x    = mean,
      y    = reorder(term, mean),
      xmin = lower95,
      xmax = upper95
    )
  ) +
    ggplot2::geom_vline(
      xintercept = 0,
      linetype   = 2,
      color      = "grey60"
    ) +
    ggplot2::geom_pointrange(
      color     = "#2166ac",
      linewidth  = 0.7,
      size      = 0.55
    ) +
    ggplot2::labs(
      title = title %||% .stl("coef_title", lang),
      x     = .stl("coef_x", lang),
      y     = .stl("coef_y", lang)
    ) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(hjust = 0.5, face = "bold"),
      panel.grid.major.y = ggplot2::element_blank()
    )

  p
}

# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' Visualizations for Space-Time Bayesian Disease Mapping Results
#'
#' Produces five types of visualization for `climasus_spacetime_bayes` and
#' `climasus_spacetime_exceedance` objects returned by
#' [sus_mod_spacetime_bayes()] and [sus_mod_spacetime_exceedance()]:
#' choropleth maps of relative risk, temporal trend charts, space-time
#' interaction heatmaps, exceedance probability maps, and fixed-effect forest
#' plots. Static ggplot2 output is returned by default; set
#' `interactive = TRUE` for [plotly::ggplotly()] output.
#'
#' @section Plot types (`type`):
#' \describe{
#'   \item{`"rr_map"`}{Choropleth map of the posterior mean relative risk
#'     (`rr_mean`) merged with the `municipalities` sf polygon layer.
#'     Uses a diverging colour scale centred at 1.0. Municipalities where
#'     the entire 95\% credible interval lies above 1 receive a black border.
#'     Supply `time_point` to show a single period; set `facet_time = TRUE`
#'     to produce one facet per period; leave both as defaults to display the
#'     mean across all periods. Requires `municipalities`.}
#'   \item{`"temporal"`}{Line chart of the population-averaged posterior mean
#'     RR (aggregated across municipalities) with a shaded 95\% credible
#'     interval ribbon over time. Supply `time_range` to restrict the
#'     displayed periods.}
#'   \item{`"interaction"`}{Tile heatmap of the space-time interaction term
#'     (`gamma_mean`) with municipalities on the y-axis and time periods on
#'     the x-axis. Requires `x$interaction` in the input object. Supply
#'     `time_range` to restrict columns.}
#'   \item{`"exceedance"`}{Choropleth map of the exceedance probability
#'     P(RR > `threshold`). Requires a `climasus_spacetime_exceedance`
#'     object (or a `climasus_spacetime_bayes` that has an `$exceedance`
#'     slot). Requires `municipalities`.}
#'   \item{`"coef"`}{Horizontal forest plot of the fixed-effect posterior
#'     summaries (`x$fixed`). A dashed vertical reference line marks zero.}
#' }
#'
#' @param x A `climasus_spacetime_bayes` object from [sus_mod_spacetime_bayes()]
#'   or a `climasus_spacetime_exceedance` object from
#'   [sus_mod_spacetime_exceedance()].
#' @param type Character. One of `"rr_map"` (default), `"temporal"`,
#'   `"interaction"`, `"exceedance"`, `"coef"`. See **Plot types**.
#' @param municipalities An `sf` polygon object with a `code_muni` column
#'   (7-digit IBGE code). Required for `type = "rr_map"` and
#'   `type = "exceedance"`. Typically from [geobr::read_municipality()].
#' @param time_point Integer or `NULL`. Single time index to display in
#'   `type = "rr_map"`. Ignored when `facet_time = TRUE` or for other types.
#'   Default `NULL` (aggregate mean across all periods).
#' @param time_range Integer vector of length 2 (`c(t_start, t_end)`) to
#'   restrict the time axis in `type = "temporal"` and `type = "interaction"`.
#'   Default `NULL` (all periods).
#' @param threshold Numeric. RR threshold used to select the exceedance layer
#'   from `x$exceedance` when `type = "exceedance"`. Default `1.0`.
#' @param facet_time Logical. When `TRUE` and `type = "rr_map"`, produces a
#'   faceted map with one panel per time period. Default `FALSE`.
#' @param palette Character. Name of a diverging RColorBrewer palette for RR
#'   maps (e.g. `"RdYlBu"`, `"RdBu"`, `"PuOr"`). Default `"RdYlBu"`.
#' @param title Character or `NULL`. Custom plot title. `NULL` uses a
#'   multilingual default.
#' @param base_size Numeric. Base font size for ggplot2 themes. Default `11`.
#' @param interactive Logical. `TRUE` wraps the ggplot object with
#'   [plotly::ggplotly()] (requires \pkg{plotly}). Default `FALSE`.
#' @param lang Character. Language for axis labels and titles: `"pt"`
#'   (default), `"en"`, `"es"`.
#' @param ... Currently unused; reserved for future arguments.
#'
#' @return A `ggplot` object (or `plotly` object when `interactive = TRUE`).
#'
#' @examples
#' \dontrun{
#' library(climasus4r)
#' library(geobr)
#'
#' shp <- geobr::read_municipality(code_muni = "all", year = 2022)
#'
#' fit <- sus_mod_spacetime_bayes(
#'   df         = agg,
#'   outcome    = "deaths",
#'   covariates = c("temp_mean", "prec_total"),
#'   offset     = "expected"
#' )
#'
#' # Choropleth map: mean RR across all periods
#' sus_mod_plot_spacetime(fit, municipalities = shp, type = "rr_map")
#'
#' # Choropleth for a single period (t = 3)
#' sus_mod_plot_spacetime(fit, municipalities = shp, type = "rr_map",
#'                        time_point = 3L)
#'
#' # Faceted map
#' sus_mod_plot_spacetime(fit, municipalities = shp, type = "rr_map",
#'                        facet_time = TRUE)
#'
#' # Temporal trend
#' sus_mod_plot_spacetime(fit, type = "temporal", lang = "en")
#'
#' # Space-time interaction heatmap
#' sus_mod_plot_spacetime(fit, type = "interaction")
#'
#' # Exceedance probability (requires climasus_spacetime_exceedance input)
#' exc <- sus_mod_spacetime_exceedance(fit, thresholds = c(1.0, 1.5, 2.0))
#' sus_mod_plot_spacetime(exc, municipalities = shp, type = "exceedance",
#'                        threshold = 1.5)
#'
#' # Fixed-effect forest plot
#' sus_mod_plot_spacetime(fit, type = "coef", lang = "en")
#' }
#'
#' @seealso [sus_mod_spacetime_bayes()], [sus_mod_spacetime_exceedance()],
#'   [sus_mod_plot_spatial_bayes()]
#'
#' @references
#' Knorr-Held, L. (2000). Bayesian modelling of inseparable space-time
#' variation in disease risk. *Statistics in Medicine*, 19(17-18), 2555-2567.
#' \doi{10.1002/sim.1030}
#'
#' Blangiardo, M. & Cameletti, M. (2015). *Spatial and Spatio-temporal
#' Bayesian Models with R-INLA*. Wiley. \doi{10.1002/9781118950203}
#'
#' @export
#' @importFrom cli cli_abort cli_alert_warning
#' @importFrom rlang check_installed
#' @importFrom dplyr left_join
#' @importFrom stats aggregate reorder
sus_mod_plot_spacetime <- function(
    x,
    type           = c("rr_map", "temporal", "interaction", "exceedance", "coef"),
    municipalities = NULL,
    time_point     = NULL,
    time_range     = NULL,
    threshold      = 1.0,
    facet_time     = FALSE,
    palette        = "RdYlBu",
    title          = NULL,
    base_size      = 11,
    interactive    = FALSE,
    lang           = "pt",
    ...
) {

  # -- validate lang -------------------------------------------------------------
  if (!is.character(lang) || length(lang) != 1L ||
      !lang %in% c("pt", "en", "es")) {
    cli::cli_alert_warning(
      glue::glue(.stl("warn_lang", "pt"), lang = lang)
    )
    lang <- "pt"
  }

  type <- match.arg(type)

  # -- validate x ----------------------------------------------------------------
  valid_classes <- c("climasus_spacetime_bayes", "climasus_spacetime_exceedance")
  if (!any(vapply(valid_classes, function(cl) inherits(x, cl), logical(1L)))) {
    cli::cli_abort(.stl("err_not_spacetime", lang))
  }

  # -- exceedance type requires exceedance object --------------------------------
  if (type == "exceedance" &&
      !inherits(x, "climasus_spacetime_exceedance") &&
      is.null(x$exceedance)) {
    cli::cli_abort(.stl("err_exceedance_type", lang))
  }

  # -- check required packages ---------------------------------------------------
  rlang::check_installed(
    "ggplot2",
    reason = "required for all plot types in sus_mod_plot_spacetime()"
  )

  if (type %in% c("rr_map", "exceedance")) {
    rlang::check_installed(
      "sf",
      reason = paste0(
        "required for choropleth maps in sus_mod_plot_spacetime() ",
        "(type='", type, "')"
      )
    )
    rlang::check_installed(
      "RColorBrewer",
      reason = "required for diverging colour palettes in sus_mod_plot_spacetime()"
    )
  }

  # -- validate municipalities for map types ------------------------------------
  if (type %in% c("rr_map", "exceedance")) {
    if (is.null(municipalities)) {
      cli::cli_abort(.stl("err_type_needs_sf", lang, type = type))
    }
    if (!inherits(municipalities, "sf") ||
        !"code_muni" %in% names(municipalities)) {
      cli::cli_abort(.stl("err_not_sf", lang))
    }
  }

  # -- warn about ignored time_range for irrelevant types -----------------------
  if (!is.null(time_range) && !type %in% c("temporal", "interaction")) {
    cli::cli_alert_warning(.stl("warn_time_range", lang, type = type))
  }

  # -- dispatch ------------------------------------------------------------------
  p <- switch(
    type,
    rr_map = .st_plot_rr_map(
      x, municipalities, time_point, time_range,
      facet_time, palette, title, base_size, lang
    ),
    temporal = .st_plot_temporal(
      x, time_range, title, base_size, lang
    ),
    interaction = .st_plot_interaction(
      x, time_range, title, base_size, lang
    ),
    exceedance = .st_plot_exceedance(
      x, municipalities, threshold, title, base_size, lang
    ),
    coef = .st_plot_coef(
      x, title, base_size, lang
    )
  )

  # -- optional interactive output -----------------------------------------------
  if (interactive) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      cli::cli_alert_warning(.stl("warn_plotly", lang))
      return(p)
    }
    return(plotly::ggplotly(p))
  }

  p
}
