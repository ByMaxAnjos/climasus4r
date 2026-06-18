# =============================================================================
# sus_mod_plot_spatial_scan.R
# Choropleth map of Kulldorff spatial scan cluster results
#
# Renders a choropleth in which municipalities belonging to significant
# clusters are highlighted.  When show_rr = TRUE the fill encodes the
# relative-risk (RR) value of each cluster on a diverging colour scale;
# when show_rr = FALSE municipalities are coloured by a categorical cluster
# label (most-likely cluster, secondary clusters, background).
# =============================================================================

# -- NSE variable declarations ------------------------------------------------
utils::globalVariables(c(
  "code_muni", "cluster_id", "cluster_label", "RR",
  "fill_var", "is_significant"
))

# -- Local i18n ---------------------------------------------------------------
.plot_scan_labels <- list(

  title = list(
    pt = "Aglomerados Espaciais de Kulldorff",
    en = "Kulldorff Spatial Clusters",
    es = "Aglomerados Espaciales de Kulldorff"
  ),

  caption_rr = list(
    pt = "Preenchimento: Risco Relativo (RR). Contorno espesso = aglomerado significativo.",
    en = "Fill: Relative Risk (RR). Thick border = significant cluster.",
    es = "Relleno: Riesgo Relativo (RR). Borde grueso = aglomerado significativo."
  ),

  caption_cat = list(
    pt = "Preenchimento: categoria de aglomerado.",
    en = "Fill: cluster category.",
    es = "Relleno: categor\u00eda de aglomerado."
  ),

  legend_rr = list(
    pt = "Risco Relativo",
    en = "Relative Risk",
    es = "Riesgo Relativo"
  ),

  legend_cluster = list(
    pt = "Aglomerado",
    en = "Cluster",
    es = "Aglomerado"
  ),

  label_mlc = list(
    pt = "Mais prov\u00e1vel",
    en = "Most likely",
    es = "M\u00e1s probable"
  ),

  label_secondary = list(
    pt = "Aglomerado {i}",
    en = "Cluster {i}",
    es = "Aglomerado {i}"
  ),

  label_background = list(
    pt = "Plano de fundo",
    en = "Background",
    es = "Segundo plano"
  ),

  err_not_scan = list(
    pt = "{.arg x} deve ser um {.cls climasus_spatial_scan} de {.fn sus_mod_spatial_scan}.",
    en = "{.arg x} must be a {.cls climasus_spatial_scan} from {.fn sus_mod_spatial_scan}.",
    es = "{.arg x} debe ser un {.cls climasus_spatial_scan} de {.fn sus_mod_spatial_scan}."
  ),

  err_not_sf = list(
    pt = "{.arg municipalities} deve ser um objeto {.cls sf}.",
    en = "{.arg municipalities} must be an {.cls sf} object.",
    es = "{.arg municipalities} debe ser un objeto {.cls sf}."
  ),

  err_no_code_muni = list(
    pt = "{.arg municipalities} deve conter a coluna {.val code_muni}.",
    en = "{.arg municipalities} must contain the column {.val code_muni}.",
    es = "{.arg municipalities} debe contener la columna {.val code_muni}."
  ),

  warn_no_sig = list(
    pt = "Nenhum aglomerado significativo (alpha = {alpha}); o mapa mostra apenas o mais prov\u00e1vel.",
    en = "No significant clusters (alpha = {alpha}); map shows only the most-likely cluster.",
    es = "No hay aglomerados significativos (alpha = {alpha}); el mapa muestra solo el m\u00e1s probable."
  ),

  warn_lang = list(
    pt = "Idioma '{lang}' n\u00e3o suportado. Usando 'pt'.",
    en = "Language '{lang}' not supported. Using 'pt'.",
    es = "Idioma '{lang}' no admitido. Usando 'pt'."
  )
)

#' @keywords internal
#' @noRd
.pscan_lbl <- function(key, lang, ...) {
  entry <- .plot_scan_labels[[key]]
  if (is.null(entry)) return(key)
  txt  <- entry[[lang]] %||% entry[["pt"]]
  dots <- list(...)
  if (length(dots) > 0L) glue::glue(txt, .envir = list2env(dots)) else txt
}


# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' Choropleth Map of Kulldorff Spatial Scan Cluster Results
#'
#' Renders a choropleth map from a `climasus_spatial_scan` object produced by
#' [sus_mod_spatial_scan()].  Each municipality polygon is coloured either by
#' its cluster's Relative Risk (RR, continuous diverging scale) or by a
#' categorical cluster label, making it straightforward to communicate the
#' location, extent, and intensity of spatial health clusters.
#'
#' @section Colour encoding:
#' When `show_rr = TRUE` (default), the fill variable is the numeric RR of the
#' cluster that each municipality belongs to.  Background municipalities
#' (`cluster_id == 0`) receive `NA` and are rendered in `"grey80"`.  A
#' diverging gradient (navy to white to red) centred on RR = 1 is used.
#' Municipalities belonging to a significant cluster (p-value < `alpha`)
#' additionally receive a thicker border (`linewidth = 1.5`) to distinguish
#' them from the most-likely cluster when it is not itself significant.
#'
#' When `show_rr = FALSE`, municipalities are filled with categorical colours:
#' the most-likely cluster, up to nine secondary clusters, and the background
#' each receive a distinct colour via [ggplot2::scale_fill_manual()].
#'
#' @param x A `climasus_spatial_scan` object returned by
#'   [sus_mod_spatial_scan()].
#' @param municipalities An `sf` object with POLYGON or MULTIPOLYGON geometry
#'   containing a `code_muni` column that matches the identifiers stored in
#'   `x$most_likely_cluster$location_ids` and `x$secondary_clusters`.
#' @param show_rr Logical.  `TRUE` (default) colours municipalities by
#'   Relative Risk using a continuous diverging scale.  `FALSE` uses
#'   categorical cluster-membership colours.
#' @param alpha Numeric in (0, 1).  Significance threshold used to decide
#'   which cluster borders to emphasise.  Must match the value used when
#'   running [sus_mod_spatial_scan()].  Default `0.05`.
#' @param title Character or `NULL`.  Plot title.  If `NULL` (default) a
#'   localised default title is used.
#' @param lang Character.  Output language: `"pt"` (default), `"en"`, `"es"`.
#' @param ... Currently unused; reserved for future extensions.
#'
#' @return A `ggplot` object.  Assign to a variable to modify further or pass
#'   to [ggplot2::ggsave()].
#'
#' @examples
#' \dontrun{
#' library(geobr)
#' library(dplyr)
#'
#' muni_sf <- geobr::read_municipality(code_muni = 31, year = 2020)
#' muni_sf <- dplyr::rename(muni_sf, code_muni = code_muni)
#'
#' set.seed(1)
#' df_cases <- tibble::tibble(
#'   code_muni  = as.character(muni_sf$code_muni),
#'   cases      = rpois(nrow(muni_sf), 5),
#'   population = sample(5000:200000, nrow(muni_sf), replace = TRUE)
#' )
#'
#' result <- sus_mod_spatial_scan(
#'   df             = df_cases,
#'   cases          = "cases",
#'   population     = "population",
#'   municipalities = muni_sf,
#'   n_simulations  = 199L,
#'   lang           = "pt"
#' )
#'
#' # Continuous RR map (default)
#' sus_mod_plot_spatial_scan(result, municipalities = muni_sf, lang = "pt")
#'
#' # Categorical cluster-membership map
#' sus_mod_plot_spatial_scan(result, municipalities = muni_sf,
#'                           show_rr = FALSE, lang = "en")
#' }
#'
#' @seealso [sus_mod_spatial_scan()], [sus_spatial_join()],
#'   [sus_mod_plot_vulnerability()]
#'
#' @export
#' @importFrom cli cli_abort cli_alert_warning
#' @importFrom rlang check_installed `%||%`
#' @importFrom dplyr left_join mutate case_when
#' @importFrom tibble tibble
#' @importFrom glue glue
sus_mod_plot_spatial_scan <- function(
    x,
    municipalities,
    show_rr = TRUE,
    alpha   = 0.05,
    title   = NULL,
    lang    = "pt",
    ...
) {
  # -- Validate lang -----------------------------------------------------------
  if (!lang %in% c("pt", "en", "es")) {
    cli::cli_alert_warning(glue::glue(.pscan_lbl("warn_lang", "pt")))
    lang <- "pt"
  }

  # -- Package checks ----------------------------------------------------------
  rlang::check_installed("ggplot2", reason = "to render choropleth maps")
  rlang::check_installed("sf",      reason = "to handle municipality geometry")

  # -- Input validation --------------------------------------------------------
  if (!inherits(x, "climasus_spatial_scan"))
    cli::cli_abort(.pscan_lbl("err_not_scan", lang))

  if (!inherits(municipalities, "sf"))
    cli::cli_abort(.pscan_lbl("err_not_sf", lang))

  if (!"code_muni" %in% names(municipalities))
    cli::cli_abort(.pscan_lbl("err_no_code_muni", lang))

  # -- Coerce code_muni to character ------------------------------------------
  municipalities$code_muni <- as.character(municipalities$code_muni)

  # -- Build lookup table: code_muni -> cluster_id, RR -----------------------
  # cluster_id: 0 = background, 1 = most-likely cluster, 2+ = secondary
  mlc <- x$most_likely_cluster
  sec <- x$secondary_clusters

  # Significance flags
  mlc_sig <- !is.na(mlc$p_value) && mlc$p_value < alpha
  sec_sig  <- vapply(
    sec,
    function(cl) !is.na(cl$p_value) && cl$p_value < alpha,
    logical(1L)
  )

  # Warn when no significant cluster exists at all
  if (!mlc_sig && (length(sec_sig) == 0L || !any(sec_sig))) {
    cli::cli_alert_warning(
      glue::glue(.pscan_lbl("warn_no_sig", lang))
    )
  }

  # All municipalities present in the sf object
  all_codes <- as.character(municipalities$code_muni)

  # Start all as background (cluster_id = 0, RR = NA, not significant)
  lookup <- tibble::tibble(
    code_muni      = all_codes,
    cluster_id     = 0L,
    RR             = NA_real_,
    is_significant = FALSE
  )

  # Helper: stamp a set of codes with cluster attributes
  .stamp <- function(lkp, ids, cid, rr, sig) {
    ids <- as.character(ids)
    hit <- lkp$code_muni %in% ids
    lkp$cluster_id[hit]     <- cid
    lkp$RR[hit]             <- rr
    lkp$is_significant[hit] <- sig
    lkp
  }

  # Most-likely cluster (id = 1)
  lookup <- .stamp(lookup, mlc$location_ids, cid = 1L, rr = mlc$RR,
                   sig = mlc_sig)

  # Secondary clusters (id = 2, 3, ...)
  if (length(sec) > 0L) {
    for (i in seq_along(sec)) {
      lookup <- .stamp(lookup, sec[[i]]$location_ids, cid = i + 1L,
                       rr = sec[[i]]$RR, sig = sec_sig[i])
    }
  }

  # -- Cluster labels ----------------------------------------------------------
  lbl_bg  <- .pscan_lbl("label_background", lang)
  lbl_mlc <- .pscan_lbl("label_mlc", lang)

  cluster_levels <- c(
    lbl_mlc,
    if (length(sec) > 0L)
      vapply(seq_along(sec),
             function(i) .pscan_lbl("label_secondary", lang, i = i + 1L),
             character(1L))
    else
      character(0L),
    lbl_bg
  )

  lookup$cluster_label <- dplyr::case_when(
    lookup$cluster_id == 0L ~ lbl_bg,
    lookup$cluster_id == 1L ~ lbl_mlc,
    TRUE ~ vapply(
      lookup$cluster_id,
      function(cid) .pscan_lbl("label_secondary", lang, i = cid),
      character(1L)
    )
  )
  lookup$cluster_label <- factor(lookup$cluster_label, levels = cluster_levels)

  # -- Merge with municipalities sf -------------------------------------------
  merged_sf <- dplyr::left_join(municipalities, lookup, by = "code_muni")

  # -- Title and caption -------------------------------------------------------
  plot_title   <- title %||% .pscan_lbl("title", lang)
  plot_caption <- if (show_rr) .pscan_lbl("caption_rr", lang) else
    .pscan_lbl("caption_cat", lang)

  # -- Assemble ggplot --------------------------------------------------------
  if (show_rr) {

    # Subset to significant municipalities for the thick-border overlay layer
    sig_sf <- merged_sf[
      !is.na(merged_sf$is_significant) & merged_sf$is_significant, ,
      drop = FALSE
    ]

    p <- ggplot2::ggplot(merged_sf) +
      ggplot2::geom_sf(
        ggplot2::aes(fill = RR),
        color     = "white",
        linewidth = 0.1
      ) +
      ggplot2::geom_sf(
        data      = sig_sf,
        fill      = NA,
        color     = "black",
        linewidth = 1.5
      ) +
      ggplot2::scale_fill_gradient2(
        low      = "navy",
        mid      = "white",
        high     = "red",
        midpoint = 1,
        na.value = "grey80",
        name     = .pscan_lbl("legend_rr", lang)
      ) +
      ggplot2::theme_void() +
      ggplot2::labs(
        title   = plot_title,
        caption = plot_caption
      ) +
      ggplot2::theme(
        plot.title   = ggplot2::element_text(face = "bold", hjust = 0.5),
        plot.caption = ggplot2::element_text(color = "gray40", hjust = 0.5,
                                             size = 8)
      )

  } else {

    # Categorical colour palette
    n_sec <- length(sec)
    sec_colors <- if (n_sec > 0L) {
      palette_pool <- c(
        "#E69F00", "#56B4E9", "#009E73", "#F0E442",
        "#0072B2", "#D55E00", "#CC79A7", "#44AA99", "#882255"
      )
      palette_pool[seq_len(min(n_sec, length(palette_pool)))]
    } else {
      character(0L)
    }

    fill_values <- stats::setNames(
      c("firebrick", sec_colors, "grey80"),
      cluster_levels
    )

    p <- ggplot2::ggplot(merged_sf) +
      ggplot2::geom_sf(
        ggplot2::aes(fill = cluster_label),
        color     = "white",
        linewidth = 0.1
      ) +
      ggplot2::scale_fill_manual(
        values   = fill_values,
        na.value = "grey80",
        name     = .pscan_lbl("legend_cluster", lang),
        drop     = FALSE
      ) +
      ggplot2::theme_void() +
      ggplot2::labs(
        title   = plot_title,
        caption = plot_caption
      ) +
      ggplot2::theme(
        plot.title      = ggplot2::element_text(face = "bold", hjust = 0.5),
        plot.caption    = ggplot2::element_text(color = "gray40", hjust = 0.5,
                                                size = 8),
        legend.position = "bottom"
      )
  }

  p
}
