# Shared internal helpers for the `raster_area` argument used across all
# sus_grid_*() functions (sus_grid_chirps, sus_grid_era5, sus_grid_pdsi,
# sus_grid_pollution_ghap, sus_grid_pollution_merra2).
#
# `raster_area` accepts:
#   FALSE            -- return cached file paths (unchanged legacy behavior)
#   TRUE             -- return an in-memory SpatRaster, full extent
#   sf object        -- return an in-memory SpatRaster cropped + masked to it
#   character (2-letter UF, e.g. "MT") -- auto-download the state boundary
#                        via geobr (cached) and crop + mask to it

#' Resolve `raster_area` into a `terra::SpatVector` (or `NULL`)
#'
#' @param raster_area FALSE, TRUE, an `sf` object, or a 2-letter UF code.
#' @param raster_crs Target CRS (from the raster being cropped) to
#'   reproject the resolved area into.
#' @return `NULL` when no area-specific crop should be applied (i.e.
#'   `raster_area = TRUE`), otherwise a `terra::SpatVector` ready to pass
#'   to [terra::crop()] / [terra::mask()].
#' @keywords internal
#' @noRd
.sus_grid_resolve_area <- function(raster_area, raster_crs, cache_dir,
                                    use_cache, lang, verbose) {
  if (isTRUE(raster_area)) return(NULL)

  rlang::check_installed("sf", reason = "required to resolve raster_area.")

  if (inherits(raster_area, "sf")) {
    return(terra::vect(sf::st_transform(raster_area, raster_crs)))
  }

  if (is.character(raster_area) && length(raster_area) == 1L) {
    rlang::check_installed("geobr", reason = "required to auto-download state boundaries.")
    uf <- toupper(raster_area)
    state_sf <- get_spatial_data_with_cache(
      level = "state", year = 2020, cache_dir = cache_dir,
      use_cache = use_cache, lang = lang, verbose = verbose
    )
    match_col <- intersect(c("abbrev_state", "code_state"), names(state_sf))
    hit <- state_sf[toupper(as.character(state_sf[[match_col[1]]])) == uf, ]
    if (nrow(hit) == 0L) {
      cli::cli_abort(c(
        "{.arg raster_area} = {.val {raster_area}} did not match any Brazilian state.",
        "i" = "Use a 2-letter UF code (e.g. {.val MT}), an {.cls sf} object, or {.code TRUE}/{.code FALSE}."
      ))
    }
    return(terra::vect(sf::st_transform(hit, raster_crs)))
  }

  cli::cli_abort(c(
    "{.arg raster_area} must be {.code TRUE}, {.code FALSE}, an {.cls sf} object, or a 2-letter UF code.",
    "x" = "Got class {.cls {class(raster_area)}}."
  ))
}

#' Crop + mask a raster to a resolved area, if any
#' @param r A `terra::SpatRaster`.
#' @param area_vect `NULL` (no-op) or a `terra::SpatVector` from
#'   `.sus_grid_resolve_area()`.
#' @return The (possibly cropped + masked) `SpatRaster`.
#' @keywords internal
#' @noRd
.sus_grid_crop_mask <- function(r, area_vect) {
  if (is.null(area_vect)) return(r)
  r <- terra::crop(r, area_vect)
  terra::mask(r, area_vect)
}
