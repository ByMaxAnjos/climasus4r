# =============================================================================
# sus_mod_plot_spatial_moran.R
# Two-panel visualisation for LISA / Moran results
#
# Panels:
#   "map"     -- LISA cluster choropleth (HH/LL/HL/LH/NS)
#   "scatter" -- Moran scatter plot (standardised value vs. spatial lag)
#   "both"    -- patchwork side-by-side (falls back to named list)
#
# Input : climasus_spatial_moran from sus_mod_spatial_moran()
#         municipalities: sf object with code_muni geometry column
# =============================================================================

# -- NSE variable declarations ------------------------------------------------
utils::globalVariables(c(
  "quadrant", "z_std", "lag_z", "code_muni"
))

# -- Local i18n ---------------------------------------------------------------
.plot_moran_labels <- list(

  title_map = list(
    pt = "Mapa de Clusters LISA",
    en = "LISA Cluster Map",
    es = "Mapa de Cl\u00fasteres LISA"
  ),

  subtitle_map = list(
    pt = "Classifica\u00e7\u00e3o por quadrante de autocorrela\u00e7\u00e3o local",
    en = "Local spatial autocorrelation quadrant classification",
    es = "Clasificaci\u00f3n por cuadrante de autocorrelaci\u00f3n local"
  ),

  title_scatter = list(
    pt = "Diagrama de Dispers\u00e3o de Moran",
    en = "Moran Scatter Plot",
    es = "Diagrama de Dispersi\u00f3n de Moran"
  ),

  x_scatter = list(
    pt = "Valor padronizado (z)",
    en = "Standardised value (z)",
    es = "Valor estandarizado (z)"
  ),

  y_scatter = list(
    pt = "Defasagem espacial (Wz)",
    en = "Spatial lag (Wz)",
    es = "Rezago espacial (Wz)"
  ),

  legend = list(
    pt = "Quadrante LISA",
    en = "LISA Quadrant",
    es = "Cuadrante LISA"
  ),

  quad_HH = list(
    pt = "HH (ponto quente)",
    en = "HH (hotspot)",
    es = "HH (punto caliente)"
  ),
  quad_LL = list(
    pt = "LL (ponto frio)",
    en = "LL (coldspot)",
    es = "LL (punto fr\u00edo)"
  ),
  quad_HL = list(
    pt = "HL (outlier espacial)",
    en = "HL (spatial outlier)",
    es = "HL (valor at\u00edpico espacial)"
  ),
  quad_LH = list(
    pt = "LH (outlier espacial)",
    en = "LH (spatial outlier)",
    es = "LH (valor at\u00edpico espacial)"
  ),
  quad_NS = list(
    pt = "N\u00e3o significativo",
    en = "Not significant",
    es = "No significativo"
  ),

  err_not_moran = list(
    pt = "{.arg x} deve ser um {.cls climasus_spatial_moran} de {.fn sus_mod_spatial_moran}.",
    en = "{.arg x} must be a {.cls climasus_spatial_moran} from {.fn sus_mod_spatial_moran}.",
    es = "{.arg x} debe ser un {.cls climasus_spatial_moran} de {.fn sus_mod_spatial_moran}."
  ),

  err_no_code_muni = list(
    pt = "{.arg municipalities} deve conter a coluna {.val code_muni} para o merge espacial.",
    en = "{.arg municipalities} must contain column {.val code_muni} for the spatial merge.",
    es = "{.arg municipalities} debe contener la columna {.val code_muni} para el merge espacial."
  ),

  warn_unmatched = list(
    pt = "{n_unmatched} munic\u00edpio(s) em {.arg x$local} sem correspond\u00eancia em {.arg municipalities}. Ser\u00e3o omitidos do mapa.",
    en = "{n_unmatched} municipality/ies in {.arg x$local} with no match in {.arg municipalities}. They will be omitted from the map.",
    es = "{n_unmatched} municipio(s) en {.arg x$local} sin coincidencia en {.arg municipalities}. Se omitir\u00e1n del mapa."
  ),

  warn_lang = list(
    pt = "Idioma '{lang}' n\u00e3o suportado. Usando 'pt'.",
    en = "Language '{lang}' not supported. Using 'pt'.",
    es = "Idioma '{lang}' no admitido. Usando 'pt'."
  )
)

# -- Internal label dispatcher -------------------------------------------------
#' @keywords internal
#' @noRd
.pml <- function(key, lang) {
  entry <- .plot_moran_labels[[key]]
  if (is.null(entry)) return(key)
  entry[[lang]] %||% entry[["pt"]]
}

# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' Two-Panel LISA Visualisation: Cluster Map and Moran Scatter Plot
#'
#' Produces a choropleth LISA cluster map and/or a Moran scatter plot from a
#' `climasus_spatial_moran` object returned by [sus_mod_spatial_moran()].
#'
#' @section Panel types (`type`):
#' | `type` | Description |
#' |--------|-------------|
#' | `"map"` | Choropleth map coloured by LISA quadrant (HH/LL/HL/LH/NS). |
#' | `"scatter"` | Moran scatter plot of standardised values vs. spatial lag. |
#' | `"both"` | Side-by-side panels via \pkg{patchwork} (named list fallback if not installed). |
#'
#' @section Colour scheme:
#' Quadrant colours follow the conventional LISA palette: HH = `"red3"`,
#' LL = `"blue3"`, HL = `"darkorange"`, LH = `"steelblue"`, NS = `"grey80"`.
#'
#' @param x A `climasus_spatial_moran` object from [sus_mod_spatial_moran()].
#' @param municipalities An `sf` object with at least the column `code_muni`
#'   (7-digit IBGE code) and polygon geometry. Required when `type` is `"map"`
#'   or `"both"`. Ignored for `type = "scatter"`.
#' @param type Character. Which panel(s) to produce. One of `"map"` (default),
#'   `"scatter"`, or `"both"`.
#' @param alpha Numeric in (0, 1). Significance level used for the subtitle
#'   annotation. Default `0.05`.
#' @param title Character or `NULL`. Custom title overriding the default
#'   translated title. Applies to the first / only panel. Default `NULL`.
#' @param lang Character. Output language: `"pt"` (default), `"en"`, or `"es"`.
#' @param ... Additional arguments passed to [ggplot2::ggsave()] when saving,
#'   or ignored.
#'
#' @return
#' - `type = "map"` -> a `ggplot` object.
#' - `type = "scatter"` -> a `ggplot` object.
#' - `type = "both"` -> a `patchwork` / `ggplot` combined object, or a named
#'   list `list(map = <ggplot>, scatter = <ggplot>)` if \pkg{patchwork} is not
#'   installed.
#'
#' @examples
#' \dontrun{
#' # Requires spdep, sf, ggplot2 (and optionally patchwork)
#' library(climasus4r)
#'
#' # moran_res <- sus_mod_spatial_moran(df, "deaths", W)
#' # shp <- geobr::read_municipality(code_muni = "all", year = 2020)
#'
#' sus_mod_plot_spatial_moran(moran_res, municipalities = shp, type = "both")
#' sus_mod_plot_spatial_moran(moran_res, municipalities = shp, type = "map", lang = "en")
#' sus_mod_plot_spatial_moran(moran_res, type = "scatter", lang = "es")
#' }
#'
#' @seealso [sus_mod_spatial_moran()], [sus_mod_spatial_weights()]
#'
#' @export
#' @importFrom cli cli_abort cli_alert_warning
#' @importFrom rlang check_installed `%||%`
sus_mod_plot_spatial_moran <- function(
    x,
    municipalities = NULL,
    type           = c("map", "scatter", "both"),
    alpha          = 0.05,
    title          = NULL,
    lang           = "pt",
    ...
) {
  type <- match.arg(type)

  # Language validation
  lang <- if (!lang %in% c("pt", "en", "es")) {
    cli::cli_alert_warning(
      glue::glue(.pml("warn_lang", "pt"), lang = lang)
    )
    "pt"
  } else {
    lang
  }

  # Input class check
  if (!inherits(x, "climasus_spatial_moran")) {
    cli::cli_abort(.pml("err_not_moran", lang))
  }

  # ggplot2 required for all types
  rlang::check_installed("ggplot2", reason = "to create Moran plots")

  # sf required for map
  if (type %in% c("map", "both")) {
    rlang::check_installed("sf", reason = "to merge LISA results with municipality geometry")
    if (is.null(municipalities)) {
      cli::cli_abort(
        c("!" = "{.arg municipalities} must be provided when {.arg type} is {.val map} or {.val both}.")
      )
    }
    if (!("code_muni" %in% names(municipalities))) {
      cli::cli_abort(.pml("err_no_code_muni", lang))
    }
  }

  # -- Colour palette (fixed conventional LISA palette) -----------------------
  quad_colors <- c(
    HH = "red3",
    LL = "blue3",
    HL = "darkorange",
    LH = "steelblue",
    NS = "grey80"
  )

  # Build panels based on requested type
  if (type == "map") {
    return(.moran_map_panel(x, municipalities, quad_colors, alpha, title, lang))
  }

  if (type == "scatter") {
    return(.moran_scatter_panel(x, quad_colors, alpha, title, lang))
  }

  # type == "both"
  p_map     <- .moran_map_panel(x, municipalities, quad_colors, alpha, title, lang)
  p_scatter <- .moran_scatter_panel(x, quad_colors, alpha, NULL, lang)

  if (requireNamespace("patchwork", quietly = TRUE)) {
    return(patchwork::wrap_plots(p_map, p_scatter))
  }

  list(map = p_map, scatter = p_scatter)
}


# =============================================================================
# INTERNAL PANEL BUILDERS
# =============================================================================

#' @keywords internal
#' @noRd
.moran_map_panel <- function(x, municipalities, quad_colors, alpha, title, lang) {

  local_df <- x$local

  # Ensure code_muni types align for merge
  municipalities[["code_muni"]] <- as.character(municipalities[["code_muni"]])
  local_df[["code_muni"]]       <- as.character(local_df[["code_muni"]])

  n_unmatched <- sum(!local_df[["code_muni"]] %in% municipalities[["code_muni"]])
  if (n_unmatched > 0L) {
    cli::cli_alert_warning(
      glue::glue(.pml("warn_unmatched", lang), n_unmatched = n_unmatched)
    )
  }

  merged_sf <- merge(
    municipalities,
    local_df[, c("code_muni", "quadrant", "Ii")],
    by     = "code_muni",
    all.x  = TRUE
  )

  # Ensure factor levels are complete (keeps legend consistent)
  quad_levels <- c("HH", "LL", "HL", "LH", "NS")
  merged_sf[["quadrant"]] <- factor(
    as.character(merged_sf[["quadrant"]]),
    levels = quad_levels
  )

  # Localised legend labels
  quad_labels <- stats::setNames(
    vapply(
      paste0("quad_", quad_levels),
      function(k) .pml(k, lang),
      character(1L)
    ),
    quad_levels
  )

  # Only keep colors / labels for levels that actually exist in the data
  present_levels <- levels(droplevels(merged_sf[["quadrant"]]))
  quad_colors_use <- quad_colors[present_levels]
  quad_labels_use <- quad_labels[present_levels]

  p_map <- ggplot2::ggplot(merged_sf) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = quadrant),
      color     = "white",
      linewidth = 0.1
    ) +
    ggplot2::scale_fill_manual(
      values   = quad_colors_use,
      labels   = quad_labels_use,
      name     = .pml("legend", lang),
      na.value = "grey90",
      drop     = FALSE
    ) +
    ggplot2::theme_void() +
    ggplot2::labs(
      title    = title %||% .pml("title_map", lang),
      subtitle = .pml("subtitle_map", lang)
    ) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "gray40"),
      legend.position = "bottom"
    )

  p_map
}

#' @keywords internal
#' @noRd
.moran_scatter_panel <- function(x, quad_colors, alpha, title, lang) {

  local_df <- x$local

  # Attach z_std and lag_z if not already present
  # sus_mod_spatial_moran does not store z_std/lag_z in $local by default,
  # so we derive them from Ii and the quadrant as visual approximations.
  # If the columns are already present (e.g., from extended output) use them;
  # otherwise derive z_std from Ii (Ii = z * Wz so sign can be inferred from quadrant).
  if (!("z_std" %in% names(local_df))) {
    # Derive z_std as sign-correct proxy: sign from quadrant, magnitude from |Ii|^0.5
    sign_z <- ifelse(
      local_df[["quadrant"]] %in% c("HH", "HL"), 1L,
      ifelse(local_df[["quadrant"]] %in% c("LL", "LH"), -1L, sign(local_df[["Ii"]]))
    )
    local_df[["z_std"]] <- sign_z * sqrt(abs(local_df[["Ii"]]))
  }

  if (!("lag_z" %in% names(local_df))) {
    # Ii = z_std * lag_z  =>  lag_z = Ii / z_std
    safe_z <- ifelse(abs(local_df[["z_std"]]) < 1e-10, NA_real_, local_df[["z_std"]])
    sign_wz <- ifelse(
      local_df[["quadrant"]] %in% c("HH", "LH"), 1L,
      ifelse(local_df[["quadrant"]] %in% c("LL", "HL"), -1L, sign(local_df[["Ii"]]))
    )
    raw_lag <- local_df[["Ii"]] / safe_z
    local_df[["lag_z"]] <- ifelse(is.na(raw_lag), sign_wz * sqrt(abs(local_df[["Ii"]])), raw_lag)
  }

  # Ensure quadrant is a factor with full levels
  quad_levels <- c("HH", "LL", "HL", "LH", "NS")
  local_df[["quadrant"]] <- factor(
    as.character(local_df[["quadrant"]]),
    levels = quad_levels
  )

  # Build subtitle from global stats
  g <- x$global
  subtitle_txt <- paste0(
    "I = ", round(g$I, 3),
    "  p = ", round(g$p_simulated, 3)
  )

  present_levels <- levels(droplevels(local_df[["quadrant"]]))
  quad_colors_use <- quad_colors[present_levels]

  p_scatter <- ggplot2::ggplot(
    local_df,
    ggplot2::aes(x = z_std, y = lag_z)
  ) +
    ggplot2::geom_point(
      ggplot2::aes(color = quadrant),
      alpha = 0.7,
      size  = 1.8
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, color = "grey50") +
    ggplot2::geom_smooth(
      method  = "lm",
      se      = FALSE,
      color   = "grey40",
      linewidth = 0.8,
      formula = y ~ x
    ) +
    ggplot2::scale_color_manual(
      values = quad_colors_use,
      name   = .pml("legend", lang),
      drop   = FALSE
    ) +
    ggplot2::labs(
      title    = title %||% .pml("title_scatter", lang),
      subtitle = subtitle_txt,
      x        = .pml("x_scatter", lang),
      y        = .pml("y_scatter", lang)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold"),
      plot.subtitle   = ggplot2::element_text(color = "gray40"),
      legend.position = "bottom"
    )

  p_scatter
}
