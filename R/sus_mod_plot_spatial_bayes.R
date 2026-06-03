# =============================================================================
# sus_mod_plot_spatial_bayes.R
# Visualizations for climasus_spatial_bayes objects
#
# Types:
#   "rr"          — Relative Risk choropleth map
#   "uncertainty" — CI width choropleth map
#   "coef"        — Forest plot of fixed-effect coefficients
#   "both"        — RR + uncertainty side-by-side (patchwork or list)
# =============================================================================

# -- NSE variable declarations -------------------------------------------------
utils::globalVariables(c(
  "rr_mean", "rr_lower95", "rr_upper95", "ci_width",
  "sig_elevated", "code_muni", "term", "mean", "lower95", "upper95",
  "geometry", "reorder"
))

# -- Local i18n ----------------------------------------------------------------
.plot_bayes_labels <- list(

  rr_title = list(
    pt = "Risco Relativo Suavizado (BYM/CAR)",
    en = "Smoothed Relative Risk (BYM/CAR)",
    es = "Riesgo Relativo Suavizado (BYM/CAR)"
  ),
  rr_fill = list(
    pt = "RR",
    en = "RR",
    es = "RR"
  ),
  rr_sig_label = list(
    pt = "RR > 1 (IC95% elevado)",
    en = "RR > 1 (95% CI elevated)",
    es = "RR > 1 (IC95% elevado)"
  ),

  unc_title = list(
    pt = "Incerteza: Largura do Intervalo de Credibilidade (95%)",
    en = "Uncertainty: 95% Credible Interval Width",
    es = "Incertidumbre: Amplitud del Intervalo de Credibilidad (95%)"
  ),
  unc_fill = list(
    pt = "Largura IC",
    en = "CI Width",
    es = "Amplitud IC"
  ),

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
  coef_ref_label = list(
    pt = "Refer\u00eancia (zero)",
    en = "Reference (zero)",
    es = "Referencia (cero)"
  ),

  both_title = list(
    pt = "Mapeamento Bayesiano Espacial de Doen\u00e7as",
    en = "Bayesian Spatial Disease Mapping",
    es = "Mapeo Bayesiano Espacial de Enfermedades"
  ),

  sig_note = list(
    pt = "Contorno preto: IC95% inferior > 1 (risco significativamente elevado)",
    en = "Black outline: lower 95% CI > 1 (significantly elevated risk)",
    es = "Contorno negro: IC95% inferior > 1 (riesgo significativamente elevado)"
  ),

  err_not_bayes = list(
    pt = "{.arg x} deve ser um objeto {.cls climasus_spatial_bayes} produzido por {.fn sus_mod_spatial_bayes}.",
    en = "{.arg x} must be a {.cls climasus_spatial_bayes} object from {.fn sus_mod_spatial_bayes}.",
    es = "{.arg x} debe ser un objeto {.cls climasus_spatial_bayes} de {.fn sus_mod_spatial_bayes}."
  ),
  err_not_sf = list(
    pt = "{.arg municipalities} deve ser um objeto {.cls sf} com a coluna {.val code_muni}.",
    en = "{.arg municipalities} must be an {.cls sf} object with a {.val code_muni} column.",
    es = "{.arg municipalities} debe ser un objeto {.cls sf} con la columna {.val code_muni}."
  ),
  err_no_fixed = list(
    pt = "Nenhum efeito fixo encontrado em {.code x$fixed}. Ajuste o modelo com covari\u00e1veis.",
    en = "No fixed effects found in {.code x$fixed}. Fit the model with covariates.",
    es = "No se encontraron efectos fijos en {.code x$fixed}. Ajuste el modelo con covariables."
  ),
  err_type_needs_sf = list(
    pt = "type={.val {type}} requer o argumento {.arg municipalities} (objeto sf).",
    en = "type={.val {type}} requires the {.arg municipalities} argument (sf object).",
    es = "type={.val {type}} requiere el argumento {.arg municipalities} (objeto sf)."
  ),
  warn_lang = list(
    pt = "Idioma '{lang}' n\u00e3o suportado. Usando 'pt'.",
    en = "Language '{lang}' not supported. Using 'pt'.",
    es = "Idioma '{lang}' no admitido. Usando 'pt'."
  ),
  warn_patchwork = list(
    pt = "Pacote {.pkg patchwork} n\u00e3o dispon\u00edvel. Retornando lista com $rr e $uncertainty.",
    en = "Package {.pkg patchwork} not available. Returning list with $rr and $uncertainty.",
    es = "Paquete {.pkg patchwork} no disponible. Retornando lista con $rr y $uncertainty."
  )
)

# -- Internal helpers ----------------------------------------------------------
#' @keywords internal
#' @noRd
.pbl <- function(key, lang, ...) {
  entry <- .plot_bayes_labels[[key]]
  if (is.null(entry)) return(key)
  msg <- entry[[lang]] %||% entry[["pt"]]
  if (...length() > 0L) msg <- glue::glue(msg, .envir = rlang::env(...))
  msg
}

#' @keywords internal
#' @noRd
.bayes_map_theme <- function(base_size = 12) {
  ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(
      legend.position   = "right",
      legend.key.height = ggplot2::unit(1.2, "cm"),
      plot.title        = ggplot2::element_text(
        hjust = 0.5, face = "bold", size = base_size + 1
      ),
      plot.caption = ggplot2::element_text(
        hjust = 0, size = base_size - 2, color = "grey50"
      )
    )
}

#' @keywords internal
#' @noRd
.plot_rr <- function(x, municipalities, title, lang, base_size) {
  rr_sf <- dplyr::left_join(
    municipalities,
    x$rr,
    by = "code_muni"
  )

  rr_sf$sig_elevated <- rr_sf$rr_lower95 > 1

  p <- ggplot2::ggplot(rr_sf) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = rr_mean),
      color = "grey80",
      linewidth = 0.15
    ) +
    ggplot2::geom_sf(
      data = rr_sf[!is.na(rr_sf$sig_elevated) & rr_sf$sig_elevated, ],
      fill = NA,
      color = "black",
      linewidth = 0.6
    ) +
    ggplot2::scale_fill_gradient2(
      low      = "navy",
      mid      = "white",
      high     = "red3",
      midpoint = 1.0,
      na.value = "grey90",
      name     = .pbl("rr_fill", lang)
    ) +
    ggplot2::labs(
      title   = title %||% .pbl("rr_title", lang),
      caption = .pbl("sig_note", lang)
    ) +
    .bayes_map_theme(base_size)

  p
}

#' @keywords internal
#' @noRd
.plot_uncertainty <- function(x, municipalities, title, lang, base_size) {
  unc_data <- x$rr
  unc_data$ci_width <- unc_data$rr_upper95 - unc_data$rr_lower95

  unc_sf <- dplyr::left_join(
    municipalities,
    unc_data[, c("code_muni", "ci_width")],
    by = "code_muni"
  )

  p <- ggplot2::ggplot(unc_sf) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = ci_width),
      color = "grey80",
      linewidth = 0.15
    ) +
    ggplot2::scale_fill_viridis_c(
      option   = "plasma",
      na.value = "grey90",
      name     = .pbl("unc_fill", lang)
    ) +
    ggplot2::labs(
      title = title %||% .pbl("unc_title", lang)
    ) +
    .bayes_map_theme(base_size)

  p
}

#' @keywords internal
#' @noRd
.plot_coef <- function(x, title, lang, base_size) {
  fixed_df <- x$fixed
  if (is.null(fixed_df) || nrow(fixed_df) == 0L) {
    cli::cli_abort(.pbl("err_no_fixed", lang))
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
      color    = "#2166ac",
      linewidth = 0.7,
      size     = 0.55
    ) +
    ggplot2::labs(
      title = title %||% .pbl("coef_title", lang),
      x     = .pbl("coef_x", lang),
      y     = .pbl("coef_y", lang)
    ) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
      panel.grid.major.y = ggplot2::element_blank()
    )

  p
}

# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' Visualizations for Bayesian Spatial Disease Mapping Results
#'
#' Produces three types of visualization for a `climasus_spatial_bayes` object
#' returned by [sus_mod_spatial_bayes()]: a relative risk choropleth map,
#' a credible interval width (uncertainty) choropleth, a forest plot of fixed
#' effects, or a combined panel of both maps.
#'
#' @section Plot types (`type`):
#' \describe{
#'   \item{`"rr"`}{Choropleth map of the smoothed posterior mean relative risk
#'     (`rr_mean`). Uses a diverging colour scale centred at 1.0 (navy = below,
#'     white = 1.0, red = above). Municipalities where the entire 95\% credible
#'     interval lies above 1 (`rr_lower95 > 1`) receive a black border to flag
#'     significantly elevated risk.}
#'   \item{`"uncertainty"`}{Choropleth of the 95\% credible interval width
#'     (`rr_upper95 - rr_lower95`), rendered with the plasma palette from
#'     [ggplot2::scale_fill_viridis_c()]. Highlights areas with high posterior
#'     uncertainty, often due to small populations or sparse data.}
#'   \item{`"coef"`}{Horizontal forest plot of the fixed-effect posterior
#'     summaries stored in `x$fixed`. A dashed vertical reference line marks
#'     zero. Points show the posterior mean; horizontal bars show the 95\%
#'     credible interval.}
#'   \item{`"both"`}{Side-by-side panel combining the `"rr"` and
#'     `"uncertainty"` maps. Uses \pkg{patchwork} when available; falls back to
#'     a named list with `$rr` and `$uncertainty` otherwise.}
#' }
#'
#' @param x A `climasus_spatial_bayes` object from [sus_mod_spatial_bayes()].
#' @param municipalities An `sf` object containing municipality polygons with a
#'   `code_muni` column (7-digit IBGE code). Required for `type = "rr"`,
#'   `"uncertainty"`, and `"both"`. Typically obtained via
#'   [geobr::read_municipality()].
#' @param type Character. One of `"rr"` (default), `"uncertainty"`, `"coef"`,
#'   or `"both"`. See **Plot types**.
#' @param title Character or `NULL`. Custom plot title. When `NULL` (default),
#'   a multilingual default is used.
#' @param base_size Numeric. Base font size passed to ggplot2 themes. Default
#'   `12`.
#' @param lang Character. Language for axis labels and titles: `"pt"` (default),
#'   `"en"`, or `"es"`.
#' @param ... Currently unused; reserved for future arguments.
#'
#' @return
#' - For `type = "rr"`, `"uncertainty"`, `"coef"`: a `ggplot` object.
#' - For `type = "both"`: a `patchwork` object when \pkg{patchwork} is
#'   installed, otherwise a named list with elements `$rr` and `$uncertainty`.
#'
#' @examples
#' \dontrun{
#' library(climasus4r)
#' library(geobr)
#'
#' shp <- geobr::read_municipality(code_muni = "all", year = 2022)
#' W   <- sus_mod_spatial_weights(shp, return_matrix = TRUE)
#'
#' result <- sus_mod_spatial_bayes(
#'   df         = agg,
#'   outcome    = "deaths",
#'   W          = W,
#'   covariates = c("temp_mean", "prec_total"),
#'   offset     = "expected_deaths"
#' )
#'
#' # Relative risk map
#' sus_mod_plot_spatial_bayes(result, municipalities = shp, type = "rr")
#'
#' # Uncertainty map
#' sus_mod_plot_spatial_bayes(result, municipalities = shp, type = "uncertainty")
#'
#' # Forest plot of fixed effects
#' sus_mod_plot_spatial_bayes(result, type = "coef", lang = "en")
#'
#' # Combined panel (requires patchwork)
#' sus_mod_plot_spatial_bayes(result, municipalities = shp, type = "both")
#' }
#'
#' @seealso [sus_mod_spatial_bayes()], [sus_mod_spatial_weights()]
#'
#' @export
#' @importFrom cli cli_abort cli_alert_warning
#' @importFrom rlang check_installed
#' @importFrom dplyr left_join
#' @importFrom stats reorder
sus_mod_plot_spatial_bayes <- function(
    x,
    municipalities = NULL,
    type           = c("rr", "uncertainty", "coef", "both"),
    title          = NULL,
    base_size      = 12,
    lang           = "pt",
    ...
) {

  # -- validate lang -----------------------------------------------------------
  if (!is.character(lang) || length(lang) != 1L || !lang %in% c("pt", "en", "es")) {
    cli::cli_alert_warning(
      glue::glue(.pbl("warn_lang", "pt"), lang = lang)
    )
    lang <- "pt"
  }

  type <- match.arg(type)

  # -- validate x --------------------------------------------------------------
  if (!inherits(x, "climasus_spatial_bayes")) {
    cli::cli_abort(.pbl("err_not_bayes", lang))
  }

  # -- check packages ----------------------------------------------------------
  rlang::check_installed("ggplot2",
    reason = "required for all plot types in sus_mod_plot_spatial_bayes()")

  if (type %in% c("rr", "uncertainty", "both")) {
    rlang::check_installed("sf",
      reason = "required for choropleth maps in sus_mod_plot_spatial_bayes()")
  }

  # -- validate municipalities when needed -------------------------------------
  if (type %in% c("rr", "uncertainty", "both")) {
    if (is.null(municipalities)) {
      cli::cli_abort(.pbl("err_type_needs_sf", lang, type = type))
    }
    if (!inherits(municipalities, "sf") || !"code_muni" %in% names(municipalities)) {
      cli::cli_abort(.pbl("err_not_sf", lang))
    }
    municipalities$code_muni <- as.character(municipalities$code_muni)
    x$rr$code_muni           <- as.character(x$rr$code_muni)
  }

  # -- dispatch ----------------------------------------------------------------
  if (type == "rr") {
    return(.plot_rr(x, municipalities, title, lang, base_size))
  }

  if (type == "uncertainty") {
    return(.plot_uncertainty(x, municipalities, title, lang, base_size))
  }

  if (type == "coef") {
    return(.plot_coef(x, title, lang, base_size))
  }

  # type == "both"
  p_rr  <- .plot_rr(x, municipalities, NULL, lang, base_size)
  p_unc <- .plot_uncertainty(x, municipalities, NULL, lang, base_size)

  if (requireNamespace("patchwork", quietly = TRUE)) {
    combined <- p_rr + p_unc +
      patchwork::plot_annotation(
        title = title %||% .pbl("both_title", lang),
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
        )
      )
    return(combined)
  }

  cli::cli_alert_warning(.pbl("warn_patchwork", lang))
  list(rr = p_rr, uncertainty = p_unc)
}
