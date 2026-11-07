# \u2500\u2500 NSE variable declarations \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500
utils::globalVariables(c(
  "x", "y", "value", "layer_lbl"
))

# \u2500\u2500 Exported function \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500

#' Plot a Raster Grid (GeoTIFF/NetCDF) as a Publication-Ready Map
#'
#' @description
#' `sus_grid_plot()` renders a raster surface -- a `SpatRaster`, a single
#' GeoTIFF/NetCDF file path, or the named vector of cached file paths
#' returned by the `sus_grid_*()` family (e.g. [sus_grid_chirps()],
#' [sus_grid_era5()], [sus_grid_pdsi()], [sus_grid_pollution_ghap()],
#' [sus_grid_pollution_merra2()]) when called with `municipalities = NULL`
#' or `raster_area != FALSE` -- as a `ggplot2` map, following the same
#' publication style used across the other `sus_*_plot_*()` functions.
#'
#' Use it after any `sus_grid_*()` call that returns rasters/paths (stage 8,
#' gridded climate/environment) to inspect a surface before aggregating it.
#' Large rasters are automatically downsampled for plotting (see
#' `max_cells`).
#'
#' @param x A `terra::SpatRaster`; a single file path (`"file.tif"`,
#'   `"file.tif.gz"`, `"file.nc"`); or a (named) character vector of file paths,
#'   as returned by `sus_grid_*()` with `municipalities = NULL` (layers are
#'   stacked and named by the vector names). Any other class errors; an empty
#'   vector errors; missing files error.
#' @param layer Single layer selector, or `"all"`. Forms: integer index (`1`,
#'   `3`), layer name (`"2022-01-01"`; must match `names(x)`), or `"all"` to facet
#'   every layer (capped at `max_layers`). Default `1`. A single value only
#'   (vectors such as `1:3` are not supported); index outside `1:nlyr` or unknown
#'   name errors.
#' @param max_layers Positive integer. Maximum layers drawn when `layer = "all"`; the
#'   first `max_layers` are kept and a warning is printed. Default `12`.
#' @param municipalities An `sf` polygon object (e.g. rows from [geobr::read_municipality()]
#'   or a state polygon) or `NULL` (default). When supplied, the raster is
#'   reprojected-to-match, cropped and masked to it (cells outside become `NA` and
#'   are not drawn) and its boundary is overlaid. `NULL` = full raster extent.
#' @param state_borders Logical scalar. Overlay Brazilian state boundaries (needs `geobr`
#'   and `sf`, cached on disk; if unavailable a warning is issued and borders are
#'   omitted). Default `TRUE`.
#' @param diverging Logical scalar. If `TRUE`, use a blue-white-red scale centered on
#'   0 with symmetric limits (for anomalies/indices such as PDSI) and ignore
#'   `palette`. Default `FALSE`.
#' @param palette Character scalar. Sequential palette used when `diverging = FALSE`:
#'   `"viridis"` (default), `"plasma"`, `"magma"`, `"inferno"`, `"cividis"`, or any
#'   [RColorBrewer::brewer.pal()] palette name (e.g. `"YlOrRd"`, `"Blues"`).
#'   Unknown names fall back to `"YlOrRd"` with a warning.
#' @param name Character scalar or `NULL` (default). Legend title; `NULL` uses
#'   the (first) layer name.
#' @param max_cells Numeric scalar. If the number of cells x layers exceeds this, the
#'   raster is regularly subsampled before plotting. Default `500000`.
#' @param title Character scalar or `NULL` (default, built-in title in `lang`).
#' @param subtitle Character scalar or `NULL` (default, auto-generated from the layer
#'   name / number of layers).
#' @param caption Character scalar or `NULL` (default, a generic `climasus4r` line).
#' @param base_size Numeric scalar. Base font size for `ggplot2::theme_void()`.
#'   Default `11`.
#' @param interactive Logical scalar. If `TRUE`, return `plotly::ggplotly()` of the map
#'   (requires `plotly`). Default `FALSE`.
#' @param use_cache Logical scalar. Cache the downloaded state boundaries on disk.
#'   Default `TRUE`.
#' @param cache_dir Character scalar. Directory of the boundary cache. Default
#'   `"~/.climasus4r_cache/spatial"`.
#' @param lang Character scalar. Message and label language: `"pt"` (default),
#'   `"en"` or `"es"` (validated with `match.arg()`; other values error).
#' @param verbose Logical scalar. Print progress messages. Default `TRUE`.
#'
#' @return A `ggplot` object (a raster layer in lon/lat with optional state and
#'   municipality outlines; faceted by layer when `layer = "all"`), or a
#'   `plotly` object when `interactive = TRUE`. Not a `climasus_df`; `sus_meta`
#'   is not involved.
#'
#' @seealso [sus_grid_chirps()], [sus_grid_era5()], [sus_grid_pdsi()],
#'   [sus_grid_pollution_ghap()], [sus_grid_pollution_merra2()],
#'   [sus_grid_join()], [sus_data_plot_aggregate_map()]
#'
#' @examples
#' \dontrun{
#' library(climasus4r)
#'
#' # From a raster returned in memory
#' r <- sus_grid_chirps(resolution = "annual", years = 2022,
#'                       raster_area = TRUE)
#' sus_grid_plot(r, lang = "pt")
#'
#' # From cached GeoTIFF paths, faceting every month
#' paths <- sus_grid_chirps(resolution = "monthly", years = 2022)
#' sus_grid_plot(paths, layer = "all", lang = "en")
#'
#' # Anomaly-style raster with a diverging palette
#' sus_grid_plot(r, diverging = TRUE, name = "Anomaly (mm)")
#' }
#'
#' @export
#' @importFrom rlang %||% .data
sus_grid_plot <- function(
    x,
    layer          = 1,
    max_layers     = 12,
    municipalities = NULL,
    state_borders  = TRUE,
    diverging      = FALSE,
    palette        = "viridis",
    name           = NULL,
    max_cells      = 500000,
    title          = NULL,
    subtitle       = NULL,
    caption        = NULL,
    base_size      = 11,
    interactive    = FALSE,
    use_cache      = TRUE,
    cache_dir      = "~/.climasus4r_cache/spatial",
    lang           = "pt",
    verbose        = TRUE
) {

  # ---------------------------------------------------------------------------
  # 0.  Package checks
  # ---------------------------------------------------------------------------
  rlang::check_installed("ggplot2", reason = "required to build the map plot.")
  rlang::check_installed("terra",   reason = "required to read raster grids.")

  # ---------------------------------------------------------------------------
  # 1.  Validate lang
  # ---------------------------------------------------------------------------
  lang <- match.arg(lang, c("pt", "en", "es"))
  msg  <- .gridplot_msgs[[lang]]

  # ---------------------------------------------------------------------------
  # 2.  Resolve input to a SpatRaster
  # ---------------------------------------------------------------------------
  r <- .gridplot_as_raster(x, msg)

  n_lyr <- terra::nlyr(r)

  # ---------------------------------------------------------------------------
  # 3.  Select layer(s)
  # ---------------------------------------------------------------------------
  facet <- FALSE
  if (identical(layer, "all")) {
    facet <- TRUE
    if (n_lyr > max_layers) {
      if (verbose) cli::cli_alert_warning(
        glue::glue(msg$too_many_layers, n = n_lyr, max = max_layers))
      r <- r[[seq_len(max_layers)]]
    }
  } else {
    idx <- if (is.character(layer)) {
      m <- match(layer, names(r))
      if (is.na(m)) cli::cli_abort(glue::glue(msg$layer_not_found, layer = layer))
      m
    } else {
      layer <- as.integer(layer)
      if (layer < 1L || layer > n_lyr) {
        cli::cli_abort(glue::glue(msg$layer_out_of_range, layer = layer, n = n_lyr))
      }
      layer
    }
    r <- r[[idx]]
  }

  # ---------------------------------------------------------------------------
  # 3b.  Crop + mask to municipalities, if supplied
  # ---------------------------------------------------------------------------
  if (!is.null(municipalities)) {
    rlang::check_installed("sf", reason = "required to crop/mask by municipalities.")
    muni_vect <- terra::vect(sf::st_transform(municipalities, terra::crs(r)))
    r <- terra::crop(r, muni_vect)
    r <- terra::mask(r, muni_vect)
    if (verbose) cli::cli_alert_info(glue::glue(msg$cropped, n = nrow(municipalities)))
  }

  # ---------------------------------------------------------------------------
  # 4.  Downsample large rasters
  # ---------------------------------------------------------------------------
  n_cells <- terra::ncell(r) * terra::nlyr(r)
  if (n_cells > max_cells) {
    if (verbose) cli::cli_alert_info(glue::glue(msg$downsampling, n = n_cells))
    r <- terra::spatSample(
      r, size = max_cells, method = "regular", as.raster = TRUE
    )
  }

  # ---------------------------------------------------------------------------
  # 5.  Long-format data.frame for ggplot2
  # ---------------------------------------------------------------------------
  df_r <- terra::as.data.frame(r, xy = TRUE, na.rm = TRUE)
  if (nrow(df_r) == 0L) cli::cli_abort(msg$no_data)

  value_cols <- setdiff(names(df_r), c("x", "y"))
  df_long <- tidyr::pivot_longer(
    df_r, cols = dplyr::all_of(value_cols),
    names_to = "layer_lbl", values_to = "value"
  )
  df_long$layer_lbl <- factor(df_long$layer_lbl, levels = value_cols)

  legend_lbl <- name %||% value_cols[1]

  # ---------------------------------------------------------------------------
  # 6.  Optional state / municipality boundary overlays
  # ---------------------------------------------------------------------------
  state_sf <- NULL
  if (state_borders) {
    has_geobr <- requireNamespace("geobr", quietly = TRUE)
    has_sf    <- requireNamespace("sf",    quietly = TRUE)
    if (!has_geobr || !has_sf) {
      cli::cli_warn(msg$geobr_missing)
    } else {
      state_sf <- tryCatch(
        sf::st_transform(
          get_spatial_data_with_cache(
            level = "state", year = 2020, cache_dir = cache_dir,
            use_cache = use_cache, lang = lang, verbose = verbose
          ),
          crs = 4326
        ),
        error = function(e) {
          cli::cli_warn(c("!" = "geobr::read_state() failed: {e$message}"))
          NULL
        }
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 7.  Colour scale
  # ---------------------------------------------------------------------------
  rng <- range(df_long$value, na.rm = TRUE)

  fill_scale <- if (diverging) {
    lim <- max(abs(rng))
    ggplot2::scale_fill_gradient2(
      low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
      limits = c(-lim, lim), name = legend_lbl,
      guide = ggplot2::guide_colorbar(
        barwidth = ggplot2::unit(9, "lines"), barheight = ggplot2::unit(0.6, "lines"),
        title.position = "top", title.hjust = 0.5, ticks = FALSE
      )
    )
  } else if (palette %in% c("viridis", "plasma", "magma", "inferno", "cividis")) {
    ggplot2::scale_fill_viridis_c(
      option = palette, name = legend_lbl,
      guide = ggplot2::guide_colorbar(
        barwidth = ggplot2::unit(9, "lines"), barheight = ggplot2::unit(0.6, "lines"),
        title.position = "top", title.hjust = 0.5, ticks = FALSE
      )
    )
  } else {
    pal_cols <- tryCatch(
      RColorBrewer::brewer.pal(9, palette),
      error = function(e) {
        cli::cli_warn(glue::glue(msg$bad_palette, palette = palette))
        RColorBrewer::brewer.pal(9, "YlOrRd")
      }
    )
    ggplot2::scale_fill_gradientn(
      colours = pal_cols, name = legend_lbl,
      guide = ggplot2::guide_colorbar(
        barwidth = ggplot2::unit(9, "lines"), barheight = ggplot2::unit(0.6, "lines"),
        title.position = "top", title.hjust = 0.5, ticks = FALSE
      )
    )
  }

  # ---------------------------------------------------------------------------
  # 8.  Titles
  # ---------------------------------------------------------------------------
  map_title    <- title    %||% msg$default_title
  map_subtitle <- subtitle %||% if (facet) {
    glue::glue(msg$default_subtitle_all, n = length(value_cols))
  } else {
    glue::glue(msg$default_subtitle, layer = value_cols[1], n = n_lyr)
  }
  cap_lbl <- caption %||% msg$default_caption

  # ---------------------------------------------------------------------------
  # 9.  Build plot
  # ---------------------------------------------------------------------------
  p <- ggplot2::ggplot()

  if (!is.null(state_sf)) {
    p <- p + ggplot2::geom_sf(
      data = state_sf, fill = NA, color = "#4A4A4A",
      linewidth = 0.30, inherit.aes = FALSE
    )
  }

  p <- p + ggplot2::geom_raster(
    data = df_long,
    ggplot2::aes(x = .data$x, y = .data$y, fill = .data$value)
  )

  if (!is.null(municipalities)) {
    p <- p + ggplot2::geom_sf(
      data = municipalities, fill = NA, color = "#666666",
      linewidth = 0.15, inherit.aes = FALSE
    )
  }

  if (!is.null(state_sf)) {
    p <- p + ggplot2::geom_sf(
      data = state_sf, fill = NA, color = "#333333",
      linewidth = 0.35, inherit.aes = FALSE
    )
  }

  p <- p +
    fill_scale +
    ggplot2::coord_sf(xlim = range(df_long$x), ylim = range(df_long$y),
                       expand = FALSE) +
    ggplot2::labs(
      title = map_title, subtitle = map_subtitle, caption = cap_lbl
    ) +
    ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title    = ggplot2::element_text(face = "bold", size = base_size - 1),
      legend.text     = ggplot2::element_text(size = base_size - 2),
      plot.title      = ggplot2::element_text(face = "bold", size = base_size + 1,
                                              hjust = 0),
      plot.subtitle   = ggplot2::element_text(colour = "grey45",
                                              size = base_size * 0.88, hjust = 0,
                                              margin = ggplot2::margin(b = 4)),
      plot.caption    = ggplot2::element_text(size = base_size - 2,
                                              color = "#888888", hjust = 1),
      strip.text      = ggplot2::element_text(face = "bold", size = base_size)
    )

  if (facet) {
    p <- p + ggplot2::facet_wrap(~layer_lbl)
  }

  # ---------------------------------------------------------------------------
  # 10.  Verbose summary
  # ---------------------------------------------------------------------------
  if (verbose) {
    cli::cli_alert_success(glue::glue(msg$done, n = terra::nlyr(r)))
  }

  # ---------------------------------------------------------------------------
  # 11.  Interactive wrapper
  # ---------------------------------------------------------------------------
  if (interactive) {
    rlang::check_installed("plotly", reason = "required when interactive = TRUE.")
    return(plotly::ggplotly(p))
  }

  p
}


# =============================================================================
#  INTERNAL HELPERS
# =============================================================================

#' Resolve `sus_grid_plot()` input into a `SpatRaster`
#' @keywords internal
#' @noRd
.gridplot_as_raster <- function(x, msg) {
  if (inherits(x, "SpatRaster")) return(x)

  if (is.character(x)) {
    if (length(x) == 0L) cli::cli_abort(msg$empty_input)

    read_one <- function(path) {
      if (!file.exists(path)) {
        cli::cli_abort(glue::glue(msg$file_not_found, path = path))
      }
      gdal_path <- if (endsWith(path, ".gz")) paste0("/vsigzip/", path) else path
      terra::rast(gdal_path)
    }

    rasters <- lapply(x, read_one)
    r <- if (length(rasters) == 1L) rasters[[1]] else terra::rast(rasters)

    lyr_names <- names(x)
    if (!is.null(lyr_names) && length(lyr_names) == terra::nlyr(r)) {
      names(r) <- lyr_names
    }
    return(r)
  }

  cli::cli_abort(msg$bad_input_class)
}

#' Multilingual messages for sus_grid_plot()
#' @keywords internal
#' @noRd
.gridplot_msgs <- list(
  pt = list(
    default_title    = "Mapa de Grade Climatica/Ambiental",
    default_subtitle  = "Camada: {layer} | {n} camada(s) disponivel(is)",
    default_subtitle_all = "{n} camada(s) exibida(s)",
    default_caption   = "climasus4r \u2022 sus_grid_plot()",
    too_many_layers   = "{n} camadas encontradas; exibindo apenas as primeiras {max}.",
    layer_not_found   = "Camada {.val {layer}} nao encontrada no raster.",
    layer_out_of_range = "{.arg layer} = {layer} fora do intervalo (1-{n}).",
    downsampling      = "Raster com {n} celulas; reamostrando para plotagem mais rapida...",
    cropped           = "Raster recortado e mascarado para {n} poligono(s).",
    no_data           = "Nenhum valor valido (nao-NA) encontrado na camada selecionada.",
    geobr_missing     = "geobr nao disponivel: fronteiras estaduais omitidas.",
    bad_palette       = "Paleta {.val {palette}} desconhecida; usando 'YlOrRd'.",
    empty_input       = "{.arg x} nao pode ser um vetor de caminhos vazio.",
    file_not_found    = "Arquivo nao encontrado: {path}",
    bad_input_class   = "{.arg x} deve ser um SpatRaster, um caminho de arquivo, ou um vetor nomeado de caminhos (ex.: saida de sus_grid_chirps(municipalities = NULL)).",
    done              = "Mapa gerado ({n} camada(s))."
  ),
  en = list(
    default_title     = "Climate/Environmental Grid Map",
    default_subtitle   = "Layer: {layer} | {n} layer(s) available",
    default_subtitle_all = "{n} layer(s) shown",
    default_caption    = "climasus4r \u2022 sus_grid_plot()",
    too_many_layers    = "{n} layers found; showing only the first {max}.",
    layer_not_found    = "Layer {.val {layer}} not found in the raster.",
    layer_out_of_range = "{.arg layer} = {layer} out of range (1-{n}).",
    downsampling       = "Raster has {n} cells; resampling for faster plotting...",
    cropped            = "Raster cropped and masked to {n} polygon(s).",
    no_data            = "No valid (non-NA) values found in the selected layer.",
    geobr_missing      = "geobr unavailable: state borders omitted.",
    bad_palette        = "Unknown palette {.val {palette}}; using 'YlOrRd'.",
    empty_input        = "{.arg x} cannot be an empty path vector.",
    file_not_found     = "File not found: {path}",
    bad_input_class    = "{.arg x} must be a SpatRaster, a file path, or a named vector of paths (e.g. output of sus_grid_chirps(municipalities = NULL)).",
    done               = "Map generated ({n} layer(s))."
  ),
  es = list(
    default_title      = "Mapa de Grilla Climatica/Ambiental",
    default_subtitle    = "Capa: {layer} | {n} capa(s) disponible(s)",
    default_subtitle_all = "{n} capa(s) mostrada(s)",
    default_caption     = "climasus4r \u2022 sus_grid_plot()",
    too_many_layers     = "{n} capas encontradas; mostrando solo las primeras {max}.",
    layer_not_found     = "Capa {.val {layer}} no encontrada en el raster.",
    layer_out_of_range  = "{.arg layer} = {layer} fuera de rango (1-{n}).",
    downsampling        = "Raster con {n} celdas; remuestreando para graficar mas rapido...",
    cropped             = "Raster recortado y enmascarado a {n} poligono(s).",
    no_data             = "No se encontraron valores validos (no-NA) en la capa seleccionada.",
    geobr_missing       = "geobr no disponible: limites estatales omitidos.",
    bad_palette         = "Paleta {.val {palette}} desconocida; usando 'YlOrRd'.",
    empty_input         = "{.arg x} no puede ser un vector de rutas vacio.",
    file_not_found      = "Archivo no encontrado: {path}",
    bad_input_class     = "{.arg x} debe ser un SpatRaster, una ruta de archivo, o un vector nombrado de rutas (ej.: salida de sus_grid_chirps(municipalities = NULL)).",
    done                = "Mapa generado ({n} capa(s))."
  )
)
