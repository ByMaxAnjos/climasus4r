# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "code_muni", "date", "value", "n_files", "n_rows",
  "filename", "bad", "valid", "err", "year_val"
))

# ── Exported function ─────────────────────────────────────────────────────────

#' Import GHAP High-Resolution Pollution Data for Brazilian Municipalities
#'
#' @description
#' `sus_grid_pollution_ghap()` downloads, crops, spatially aggregates, and
#' caches GHAP (GlobalHighAirPollutants) raster data to Brazilian
#' municipalities. No API key is required.
#'
#' GHAP provides AI-generated, seamless ground-level air pollutant fields at
#' 1 km (PM2.5, CO) and 10 km (O3) resolution. Files are downloaded as NetCDF
#' from Zenodo, cropped to Brazil, aggregated with area-weighted extraction,
#' and the result is stored as a Parquet cache for fast subsequent access.
#'
#' Available pollutants and temporal coverage:
#' \itemize{
#'   \item **PM2.5** (`"pm25"`) — daily, monthly & annual, 2017–2022, 1 km, \eqn{\mu g/m^3}
#'   \item **O3** (`"o3"`) — annual only, 2000–2020, 10 km, ppb
#'   \item **CO** (`"co"`) — annual only, 2019–2022, 1 km, mg/m\eqn{^3}
#'   \item **NO2** (`"no2"`) — data not yet publicly released
#' }
#'
#' When `municipalities` is provided the function returns a `climasus_df` at
#' `stage = "climate"`, `type = "pollution_ghap"`, compatible with
#' [sus_climate_anomaly()], [sus_climate_aggregate()], and [sus_mod_dlnm()].
#'
#' @param pollutants Character vector. Pollutants to download.
#'   Accepted: `"pm25"`, `"o3"`, `"co"`. Default: `"pm25"`.
#'   Note: `"no2"` is not yet publicly available.
#'
#' @param resolution Character. Temporal aggregation of source files.
#'   One of `"daily"`, `"monthly"`, or `"annual"`. Default: `"monthly"`.
#'   \itemize{
#'     \item `"daily"` — available for PM2.5 only (2017–2022). Each monthly
#'       ZIP (~3 GB) is downloaded, extracted to a temp directory, each daily
#'       NetCDF is aggregated, and the result is cached as Parquet. Subsequent
#'       calls read from Parquet without re-extracting.
#'     \item `"monthly"` — PM2.5 only (2017–2022).
#'     \item `"annual"` — PM2.5 (2017–2022), O3 (2000–2020), CO (2019–2022).
#'   }
#'   O3 and CO automatically fall back to `"annual"` regardless of this
#'   parameter.
#'
#' @param years Integer vector. Years to download. Availability depends on
#'   pollutant and resolution:
#'   \itemize{
#'     \item PM2.5 daily, monthly & annual: 2017–2022
#'     \item O3 annual: 2000–2020
#'     \item CO annual: 2019–2022
#'   }
#'   `NULL` (default) returns all available years for the selected pollutant.
#'
#' @param months Integer vector (1–12). Months to include when
#'   `resolution = "monthly"`. Ignored for annual data. Default: `1:12`.
#'
#' @param municipalities An `sf` object with polygon geometries
#'   (e.g., from [geobr::read_municipality()]). If provided, raster data are
#'   aggregated to these polygons and a `climasus_df` is returned.
#'   If `NULL`, a named character vector of cached NetCDF file paths is
#'   returned instead.
#'
#' @param agg_fun Character. Spatial aggregation function applied by
#'   `exactextractr::exact_extract()`. Default: `"mean"` (area-weighted).
#'   Other options: `"sum"`, `"median"`, `"min"`, `"max"`.
#'
#' @param crop_brazil Logical. Crop global rasters to Brazil's bounding box
#'   before extraction, significantly reducing memory use. Default `TRUE`.
#'
#' @param use_cache Logical. If `TRUE` (default), reuse previously downloaded
#'   NetCDF files and previously computed municipality-level Parquet caches.
#'
#' @param cache_dir Character. Root directory for all cached files.
#'   Default `"~/.climasus4r_cache/ghap"`. Sub-directories are created per
#'   pollutant.
#'
#' @param lang Character. Message language: `"pt"` (default), `"en"`, `"es"`.
#'
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return
#' \itemize{
#'   \item If `municipalities` is provided: a `climasus_df` tibble with
#'     columns `code_muni`, `date`, and one column per pollutant (e.g.,
#'     `pm25_mean`, `o3_mean`). Metadata: `stage = "climate"`,
#'     `type = "pollution_ghap"`.
#'   \item If `municipalities = NULL`: a named character vector of paths to
#'     the cached NetCDF files.
#' }
#'
#' @section Data source:
#' Wei, J. et al. (2023). Estimating 1-km-resolution PM2.5 concentrations
#' across China using the space-time random forest approach.
#' *Remote Sensing of Environment*, 231, 111221.
#' DOI: \doi{10.5281/zenodo.10800980}\cr
#' Wei, J. et al. GlobalHighAirPollutants (GHAP) v2. Zenodo. CC-BY 4.0.\cr
#' PM2.5: \doi{10.5281/zenodo.10800980} |
#' O3: \doi{10.5281/zenodo.10208188} |
#' CO: \doi{10.5281/zenodo.14207363}
#'
#' @section Packages required:
#' Spatial aggregation requires `terra` and `exactextractr` (both in
#' `Suggests`). The Parquet cache requires `arrow` (in `Imports`).
#' Install with `install.packages(c("terra", "exactextractr"))`.
#'
#' @examples
#' \dontrun{
#' library(geobr)
#' mt_mun <- read_municipality(code_muni = "MT", year = 2020)
#'
#' # PM2.5 monthly for Mato Grosso, Jan-Mar 2020
#' ghap <- sus_grid_pollution_ghap(
#'   pollutants    = "pm25",
#'   resolution    = "monthly",
#'   years         = 2020,
#'   months        = 1:3,
#'   municipalities = mt_mun,
#'   lang          = "pt"
#' )
#'
#' sus_meta(ghap, "stage")  # "climate"
#' sus_meta(ghap, "type")   # "pollution_ghap"
#'
#' # O3 annual, all available years
#' ghap_o3 <- sus_grid_pollution_ghap(
#'   pollutants    = "o3",
#'   resolution    = "annual",
#'   municipalities = mt_mun,
#'   lang          = "en"
#' )
#' }
#'
#' @seealso [sus_climate_anomaly()], [sus_climate_aggregate()],
#'   [sus_grid_era5()], [sus_grid_pollution_cams()], [sus_mod_dlnm()]
#'
#' @export
#' @importFrom rlang .data is_installed
#' @importFrom dplyr filter select rename mutate bind_rows full_join join_by
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom lubridate year
#' @importFrom tibble as_tibble
sus_grid_pollution_ghap <- function(
    pollutants    = "pm25",
    resolution    = "monthly",
    years         = NULL,
    months        = 1:12,
    municipalities = NULL,
    agg_fun       = "mean",
    crop_brazil   = TRUE,
    use_cache     = TRUE,
    cache_dir     = "~/.climasus4r_cache/ghap",
    lang          = "pt",
    verbose       = TRUE) {

  # ── 1. Validation ───────────────────────────────────────────────────────────

  if (!is.character(lang) || length(lang) != 1 || !lang %in% c("pt", "en", "es")) {
    cli::cli_abort("{.arg lang} must be 'pt', 'en', or 'es'.")
  }
  msg <- .ghap_msgs[[lang]]

  # Pollutants
  valid_pollutants <- c("pm25", "o3", "co")
  if (identical(pollutants, "all")) {
    pollutants <- valid_pollutants
  } else {
    if (!is.character(pollutants) || length(pollutants) == 0) {
      cli::cli_abort(msg$invalid_pollutants_type)
    }
    # Flag NO2 specifically
    if ("no2" %in% tolower(pollutants)) {
      cli::cli_alert_warning(msg$no2_unavailable)
      pollutants <- setdiff(pollutants, c("no2", "NO2"))
    }
    bad <- paste(setdiff(pollutants, valid_pollutants), collapse = ", ")
    if (nchar(bad) > 0) {
      valid <- paste(valid_pollutants, collapse = ", ")
      cli::cli_abort(msg$invalid_pollutants)
    }
    if (length(pollutants) == 0) {
      cli::cli_abort(msg$no_pollutants)
    }
  }

  # Resolution
  valid_res <- c("daily", "monthly", "annual")
  if (length(resolution) != 1 || !resolution %in% valid_res) {
    bad   <- paste(setdiff(resolution, valid_res), collapse = ", ")
    valid <- paste(valid_res, collapse = ", ")
    cli::cli_abort(msg$invalid_resolution)
  }

  # Months
  if (!is.numeric(months) || any(is.na(months)) ||
      any(months < 1 | months > 12)) {
    cli::cli_abort(msg$invalid_months)
  }
  months <- sort(as.integer(unique(months)))

  # Municipalities
  if (!is.null(municipalities)) {
    if (!requireNamespace("sf", quietly = TRUE)) cli::cli_abort(msg$need_sf)
    if (!inherits(municipalities, "sf"))        cli::cli_abort(msg$muni_not_sf)
    rlang::check_installed("terra",
      reason = "to read GHAP NetCDF raster files")
    rlang::check_installed("exactextractr",
      reason = "to aggregate rasters to municipality polygons")
  }

  valid_agg <- c("mean", "sum", "median", "min", "max")
  if (!is.character(agg_fun) || length(agg_fun) != 1 ||
      !agg_fun %in% valid_agg) {
    valid <- paste(valid_agg, collapse = ", ")
    cli::cli_abort(msg$invalid_agg)
  }

  if (!is.logical(use_cache) || length(use_cache) != 1) {
    cli::cli_abort(msg$invalid_use_cache)
  }
  if (!is.character(cache_dir) || nchar(trimws(cache_dir)) == 0) {
    cli::cli_abort(msg$invalid_cache_dir)
  }
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)

  # ── 2. Build download manifest ───────────────────────────────────────────────
  # Auto-adjust resolution for pollutants that only have annual data
  annual_only <- c("o3", "co")
  manifest_rows <- list()

  for (p in pollutants) {
    p_res <- resolution
    if (p %in% annual_only && p_res %in% c("monthly", "daily")) {
      cli::cli_alert_warning(
        glue::glue(msg$fallback_annual, pollutant = toupper(p)))
      p_res <- "annual"
    }
    # daily resolution only available for PM2.5
    if (p_res == "daily" && p != "pm25") {
      cli::cli_alert_warning(
        glue::glue(msg$fallback_annual, pollutant = toupper(p)))
      p_res <- "annual"
    }

    # Determine available years for this pollutant × resolution
    avail_years <- .ghap_avail_years[[p]][[p_res]]
    if (is.null(years)) {
      req_years <- avail_years
    } else {
      req_years <- as.integer(years)
      bad_y <- req_years[!req_years %in% avail_years]
      if (length(bad_y) > 0) {
        cli::cli_alert_warning(
          glue::glue(msg$years_unavail,
                     pollutant = toupper(p),
                     bad_years = paste(bad_y, collapse = ", "),
                     avail     = paste(range(avail_years), collapse = "-")))
        req_years <- intersect(req_years, avail_years)
        if (length(req_years) == 0) next
      }
    }

    for (yr in req_years) {
      if (p_res == "daily") {
        for (mo in months) {
          info    <- .ghap_file_info(p, "daily", yr, mo)
          out_col <- paste0(p, "_", agg_fun)
          manifest_rows[[length(manifest_rows) + 1]] <- c(
            pollutant  = p,
            resolution = "daily",
            year       = as.character(yr),
            month      = sprintf("%02d", mo),
            filename   = info$filename,
            record_id  = info$record_id,
            url        = info$url,
            out_col    = out_col,
            # cache_nc holds the ZIP; Parquet cache is per-month (all days in month)
            cache_nc   = file.path(cache_dir, p, "daily", info$filename),
            cache_pq   = file.path(cache_dir, p, "parquet",
                                   paste0(p, "_daily_", yr,
                                          sprintf("%02d", mo), ".parquet"))
          )
        }
      } else if (p_res == "monthly") {
        for (mo in months) {
          info   <- .ghap_file_info(p, "monthly", yr, mo)
          out_col <- paste0(p, "_", agg_fun)
          manifest_rows[[length(manifest_rows) + 1]] <- c(
            pollutant  = p,
            resolution = "monthly",
            year       = as.character(yr),
            month      = sprintf("%02d", mo),
            filename   = info$filename,
            record_id  = info$record_id,
            url        = info$url,
            out_col    = out_col,
            cache_nc   = file.path(cache_dir, p, info$filename),
            cache_pq   = file.path(cache_dir, p, "parquet",
                                   paste0(p, "_", yr, sprintf("%02d", mo), ".parquet"))
          )
        }
      } else {
        info    <- .ghap_file_info(p, "annual", yr, NA_integer_)
        out_col <- paste0(p, "_", agg_fun)
        manifest_rows[[length(manifest_rows) + 1]] <- c(
          pollutant  = p,
          resolution = "annual",
          year       = as.character(yr),
          month      = NA_character_,
          filename   = info$filename,
          record_id  = info$record_id,
          url        = info$url,
          out_col    = out_col,
          cache_nc   = file.path(cache_dir, p, info$filename),
          cache_pq   = file.path(cache_dir, p, "parquet",
                                 paste0(p, "_", yr, "_annual.parquet"))
        )
      }
    }
  }

  if (length(manifest_rows) == 0) {
    cli::cli_abort(msg$no_data_to_download)
  }

  manifest <- do.call(rbind, lapply(manifest_rows, as.data.frame,
                                     stringsAsFactors = FALSE))

  n_files <- nrow(manifest)
  if (verbose) {
    cli::cli_h1(msg$title)
    cli::cli_alert_info(glue::glue(msg$download_start, n_files = n_files))
  }

  # ── 3. If municipalities provided: check Parquet cache first ─────────────────
  # Returns early from Parquet if all requested files are already cached
  if (!is.null(municipalities) && use_cache) {
    all_cached <- all(file.exists(manifest$cache_pq))
    if (all_cached) {
      if (verbose) {
        cli::cli_alert_success(msg$parquet_cache_hit)
      }
      return(.ghap_build_result_from_parquet(manifest, municipalities, verbose, msg))
    }
  }

  # ── 4. Download NetCDF files (with cache) ─────────────────────────────────
  for (i in seq_len(n_files)) {
    .ghap_download_file(
      url        = manifest$url[i],
      cache_path = manifest$cache_nc[i],
      use_cache  = use_cache,
      verbose    = verbose,
      msg        = msg
    )
  }

  # ── 5. If no municipalities: return file paths ────────────────────────────
  if (is.null(municipalities)) {
    paths <- stats::setNames(
      manifest$cache_nc,
      paste(manifest$pollutant, manifest$year,
            ifelse(is.na(manifest$month), "annual", manifest$month), sep = "_")
    )
    if (verbose) {
      cli::cli_alert_success(glue::glue(msg$done_paths, n = n_files))
    }
    return(paths)
  }

  # ── 6. Prepare municipalities ─────────────────────────────────────────────
  muni_id_col <- .ghap_detect_muni_col(municipalities)
  if (muni_id_col != "code_muni") {
    municipalities$code_muni <- as.character(municipalities[[muni_id_col]])
  } else {
    municipalities$code_muni <- as.character(municipalities$code_muni)
  }
  municipalities$code_muni <- substr(municipalities$code_muni, 1L, 7L)
  municipalities <- sf::st_transform(municipalities, crs = 4326)

  # Brazil bounding box used to crop before aggregation (pixel → WGS84 via .ghap_read_and_fix)
  brazil_bbox <- if (crop_brazil) terra::ext(-75, -28, -35, 6) else NULL

  n_mun <- nrow(municipalities)
  if (verbose) {
    cli::cli_alert_info(glue::glue(msg$agg_start, n_mun = n_mun))
  }

  # ── 7. Extract raster → polygons for each file ───────────────────────────
  result_list <- vector("list", n_files)

  for (i in seq_len(n_files)) {
    nc_path    <- manifest$cache_nc[i]
    pq_path    <- manifest$cache_pq[i]
    out_col    <- manifest$out_col[i]
    pollutant  <- manifest$pollutant[i]
    yr         <- as.integer(manifest$year[i])
    mo_str     <- manifest$month[i]

    if (is.na(nc_path) || !file.exists(nc_path)) {
      cli::cli_alert_warning(
        glue::glue(msg$skip_missing, filename = manifest$filename[i]))
      next
    }

    # Check Parquet cache for this specific file
    if (use_cache && file.exists(pq_path) && file.size(pq_path) > 0) {
      if (verbose) {
        cli::cli_alert_success(
          glue::glue(msg$parquet_hit, filename = basename(pq_path)))
      }
      result_list[[i]] <- arrow::read_parquet(pq_path)
      next
    }

    df_i <- tryCatch({
      bb <- if (!is.null(brazil_bbox)) as.vector(brazil_bbox) else
              c(-180, 180, -90, 90)

      # ── Daily ZIP branch ──────────────────────────────────────────────────
      if (endsWith(nc_path, ".zip")) {
        df_i <- .ghap_extract_daily_zip(
          zip_path   = nc_path,
          municipalities = municipalities,
          bb         = bb,
          agg_fun    = agg_fun,
          out_col    = out_col,
          yr         = yr,
          mo_str     = mo_str,
          verbose    = verbose,
          msg        = msg
        )
        df_i
      } else {
      # ── Monthly / annual NetCDF branch ───────────────────────────────────
      r  <- .ghap_read_and_fix(nc_path,
                                xmin = bb[1], xmax = bb[2],
                                ymin = bb[3], ymax = bb[4])

      # Detect variable layer (first non-auxiliary layer)
      nlyr_r <- terra::nlyr(r)
      if (nlyr_r > 1) {
        # Use all layers (one per day for monthly files)
        layer_dates <- tryCatch(as.Date(terra::time(r)), error = function(e) NULL)
      } else {
        layer_dates <- NULL
      }

      # Area-weighted extraction
      agg_result <- exactextractr::exact_extract(
        x        = r,
        y        = municipalities,
        fun      = agg_fun,
        progress = FALSE
      )

      # agg_result: nrow(municipalities) rows × nlyr(r) columns
      # named "{agg_fun}" (single layer) or "{agg_fun}.{layer_name}" (multi)
      agg_df <- as.data.frame(agg_result)
      agg_df$code_muni <- municipalities$code_muni

      # Build output — handle single vs multi-layer
      if (nlyr_r == 1) {
        val_col <- names(agg_df)[1]
        out_df  <- data.frame(
          code_muni = agg_df$code_muni,
          date      = if (!is.null(layer_dates)) layer_dates[1]
                      else as.Date(paste0(yr, "-",
                                          if (!is.na(mo_str)) mo_str else "01",
                                          "-01")),
          stringsAsFactors = FALSE
        )
        out_df[[out_col]] <- agg_df[[val_col]]
        out_df
      } else {
        # Multi-layer (daily within a monthly file or multiple bands)
        date_cols <- names(agg_df)[names(agg_df) != "code_muni"]
        out_long  <- tidyr::pivot_longer(
          agg_df,
          cols      = dplyr::all_of(date_cols),
          names_to  = "layer_name",
          values_to = out_col
        )
        if (!is.null(layer_dates) && length(layer_dates) == nlyr_r) {
          # Map layer index to date
          layer_idx     <- match(out_long$layer_name,
                                 paste0(agg_fun, ".", names(r)))
          out_long$date <- layer_dates[layer_idx]
          out_long$date[is.na(out_long$date)] <- as.Date(
            paste0(yr, "-", if (!is.na(mo_str)) mo_str else "01", "-01"))
        } else {
          out_long$date <- as.Date(
            paste0(yr, "-", if (!is.na(mo_str)) mo_str else "01", "-01"))
        }
        out_long$layer_name <- NULL
        as.data.frame(out_long[, c("code_muni", "date", out_col)])
      }
      }  # end else (monthly/annual branch)
    }, error = function(e) {
      cli::cli_warn(c(
        glue::glue(msg$extract_warn, filename = manifest$filename[i]),
        "i" = conditionMessage(e)
      ))
      NULL
    })

    if (!is.null(df_i)) {
      # Save to Parquet cache
      pq_dir <- dirname(pq_path)
      if (!dir.exists(pq_dir)) {
        dir.create(pq_dir, recursive = TRUE, showWarnings = FALSE)
      }
      tryCatch(
        arrow::write_parquet(df_i, pq_path),
        error = function(e) {
          cli::cli_alert_warning(
            glue::glue(msg$parquet_write_warn, filename = basename(pq_path)))
        }
      )
      result_list[[i]] <- df_i
    }
  }

  # ── 8. Merge all results ──────────────────────────────────────────────────
  result_list <- result_list[!vapply(result_list, is.null, logical(1))]
  if (length(result_list) == 0) {
    cli::cli_abort(msg$no_data)
  }

  result <- do.call(rbind, result_list)
  result <- result[order(result$code_muni, result$date), ]
  rownames(result) <- NULL
  result <- tibble::as_tibble(result)

  n_rows <- nrow(result)
  if (verbose) {
    cli::cli_alert_success(
      glue::glue(msg$agg_done, n_rows = n_rows, n_mun = n_mun))
  }

  # ── 9. Build climasus_df ──────────────────────────────────────────────────
  meta <- list(
    system  = NULL,
    stage   = "climate",
    type    = "pollution_ghap",
    spatial = FALSE,
    temporal = list(
      start  = min(result$date, na.rm = TRUE),
      end    = max(result$date, na.rm = TRUE),
      unit   = if (resolution == "monthly") "day" else "year",
      source = "zenodo_ghap"
    ),
    created          = Sys.time(),
    modified         = Sys.time(),
    pollutants       = pollutants,
    resolution       = resolution,
    n_municipalities = n_mun,
    n_observations   = n_rows,
    agg_fun          = agg_fun,
    history          = sprintf(
      "[%s] sus_grid_pollution_ghap(): %d pollutant(s), res=%s, %d obs",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      length(pollutants), resolution, n_rows
    ),
    user = list()
  )

  base_classes <- setdiff(class(result), "climasus_df")
  structure(result, sus_meta = meta, class = c("climasus_df", base_classes))
}


# ── Internal constants ────────────────────────────────────────────────────────

#' Convert WGS84 bounding box to GHAP pixel coordinate space
#'
#' GHAP NetCDF files store data in a custom pixel grid:
#'   x (longitude): 0–36000 (mapping -180 to +180 deg, 100 pixels/deg)
#'   y (latitude):  0–18000 (mapping +90 to -90 deg, INVERTED, 100 pixels/deg)
#' After cropping, the raster must be flipped vertically and re-assigned
#' EPSG:4326 to be usable with sf/exactextractr.
#' @keywords internal
#' @noRd
.ghap_to_pixel_bbox <- function(xmin, xmax, ymin, ymax) {
  # Pixel x: lon + 180, scaled by 100 pixels/degree
  px_xmin <- round((xmin + 180) * 100)
  px_xmax <- round((xmax + 180) * 100)
  # Pixel y is INVERTED: y_pixel = (90 - lat) * 100
  # so geographic ymax (north) → smallest pixel y value
  px_ymin <- round((90 - ymax) * 100)
  px_ymax <- round((90 - ymin) * 100)
  list(xmin = px_xmin, xmax = px_xmax, ymin = px_ymin, ymax = px_ymax)
}

#' Read GHAP NetCDF, crop to bbox, flip, assign CRS, return WGS84 SpatRaster
#' @keywords internal
#' @noRd
.ghap_read_and_fix <- function(nc_path, xmin = -75, xmax = -28,
                                ymin = -35, ymax = 6) {
  r <- terra::rast(nc_path)

  # Crop in pixel space
  px <- .ghap_to_pixel_bbox(xmin, xmax, ymin, ymax)
  r  <- terra::crop(r, terra::ext(px$xmin, px$xmax, px$ymin, px$ymax))

  # Flip vertically (latitude is stored north→south, pixel index 0=north)
  r <- terra::flip(r, direction = "vertical")

  # Re-assign geographic extent and CRS
  terra::ext(r) <- terra::ext(xmin, xmax, ymin, ymax)
  terra::crs(r) <- "EPSG:4326"
  r
}

#' Extract and aggregate all daily NC files from a monthly GHAP ZIP
#'
#' Downloads are ~3 GB per ZIP. After aggregation the daily NC files are
#' deleted from the temp directory to free disk space; the ZIP is kept in
#' cache for future calls so the full extraction is only done once.
#' @keywords internal
#' @noRd
.ghap_extract_daily_zip <- function(zip_path, municipalities, bb,
                                     agg_fun, out_col, yr, mo_str,
                                     verbose, msg) {
  tmp_dir <- file.path(tempdir(), paste0("ghap_", basename(zip_path)))
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  if (verbose) {
    cli::cli_alert_info(
      glue::glue(msg$zip_extract, filename = basename(zip_path)))
  }

  # Extract ZIP
  tryCatch(
    utils::unzip(zip_path, exdir = tmp_dir, overwrite = TRUE),
    error = function(e) cli::cli_abort(
      c(glue::glue(msg$zip_error, filename = basename(zip_path)),
        "i" = conditionMessage(e)))
  )

  # Find daily NC files inside the extracted dir
  nc_files <- list.files(tmp_dir, pattern = "\\.nc$",
                          full.names = TRUE, recursive = TRUE)
  # Sort by filename (date embedded in name: YYYYMMDD)
  nc_files <- sort(nc_files)

  if (length(nc_files) == 0) {
    cli::cli_warn(glue::glue(msg$zip_no_nc, filename = basename(zip_path)))
    return(NULL)
  }

  if (verbose) {
    cli::cli_alert_info(
      glue::glue(msg$daily_processing,
                 n_days = length(nc_files),
                 month  = paste0(yr, "-", mo_str)))
  }

  # Process each daily NC: fix CRS, extract, collect
  day_dfs <- lapply(nc_files, function(nc_path) {
    tryCatch({
      r <- .ghap_read_and_fix(nc_path,
                               xmin = bb[1], xmax = bb[2],
                               ymin = bb[3], ymax = bb[4])

      # Parse date from filename: GHAP_PM2.5_D1K_YYYYMMDD_V1.nc
      bn    <- tools::file_path_sans_ext(basename(nc_path))
      parts <- strsplit(bn, "_")[[1]]
      # Date part is 8-digit YYYYMMDD in position 4 (0-indexed: parts[4])
      date_str <- parts[grepl("^\\d{8}$", parts)]
      day_date <- if (length(date_str) == 1) {
        as.Date(date_str, format = "%Y%m%d")
      } else {
        as.Date(paste0(yr, "-", mo_str, "-01"))  # fallback
      }

      agg_result <- exactextractr::exact_extract(
        x = r, y = municipalities, fun = agg_fun, progress = FALSE)

      agg_df           <- as.data.frame(agg_result)
      agg_df$code_muni <- municipalities$code_muni

      val_col          <- names(agg_df)[names(agg_df) != "code_muni"][1]
      out_df           <- data.frame(
        code_muni = agg_df$code_muni,
        date      = day_date,
        stringsAsFactors = FALSE
      )
      out_df[[out_col]] <- agg_df[[val_col]]
      out_df
    }, error = function(e) {
      cli::cli_warn(c(
        glue::glue(msg$extract_warn, filename = basename(nc_path)),
        "i" = conditionMessage(e)))
      NULL
    })
  })

  day_dfs <- day_dfs[!vapply(day_dfs, is.null, logical(1))]
  if (length(day_dfs) == 0) return(NULL)
  do.call(rbind, day_dfs)
}

#' Zenodo record IDs for GHAP, by pollutant and resolution
#' @keywords internal
#' @noRd
.ghap_records <- list(
  pm25 = list(
    # Main record has ALL monthly + annual for 2017-2022
    monthly = "10800980",
    annual  = "10800980",
    # Daily data: one separate Zenodo record per year (one ZIP per month, ~3 GB each)
    daily   = c(
      "2017" = "10801181",
      "2018" = "10795801",
      "2019" = "10799037",
      "2020" = "10800555",
      "2021" = "10799203",
      "2022" = "10795662"
    )
  ),
  o3 = list(
    annual  = "10208188"
  ),
  co = list(
    annual  = "14207363"
  )
)

#' Available years per pollutant per resolution
#' @keywords internal
#' @noRd
.ghap_avail_years <- list(
  pm25 = list(
    daily   = 2017:2022,
    monthly = 2017:2022,
    annual  = 2017:2022
  ),
  o3 = list(
    annual  = 2000:2020
  ),
  co = list(
    annual  = 2019:2022
  )
)

#' Resolution code in filenames per pollutant
#' @keywords internal
#' @noRd
.ghap_res_code <- c(pm25 = "1K", o3 = "1K", co = "1K")  # O3 actually 10km but named 1K in file

#' Build filename and download URL for one GHAP file
#' @keywords internal
#' @noRd
.ghap_file_info <- function(pollutant, resolution, year, month = NA_integer_) {
  p_fname  <- switch(pollutant,
    pm25 = "PM2.5", o3 = "O3", co = "CO", toupper(pollutant))
  res_code <- .ghap_res_code[[pollutant]]

  if (resolution == "daily" && !is.na(month)) {
    # Daily: one ZIP per month; record_id is year-specific
    ym        <- sprintf("%04d%02d", as.integer(year), as.integer(month))
    filename  <- sprintf("GHAP_%s_D%s_%s_V1.zip", p_fname, res_code, ym)
    record_id <- .ghap_records[[pollutant]][["daily"]][[as.character(year)]]
    if (is.null(record_id) || is.na(record_id)) {
      cli::cli_abort(
        "No daily Zenodo record found for {pollutant} year {year}.")
    }
  } else if (resolution == "monthly" && !is.na(month)) {
    ym        <- sprintf("%04d%02d", as.integer(year), as.integer(month))
    filename  <- sprintf("GHAP_%s_M%s_%s_V1.nc",  p_fname, res_code, ym)
    record_id <- .ghap_records[[pollutant]][["monthly"]]
  } else {
    filename  <- sprintf("GHAP_%s_Y%s_%04d_V1.nc", p_fname, res_code,
                         as.integer(year))
    record_id <- .ghap_records[[pollutant]][["annual"]]
  }

  url <- sprintf("https://zenodo.org/records/%s/files/%s?download=1",
                 record_id, filename)
  list(filename = filename, record_id = record_id, url = url)
}

#' Read all cached Parquet files and assemble climasus_df
#' @keywords internal
#' @noRd
.ghap_build_result_from_parquet <- function(manifest, municipalities, verbose, msg) {
  parts <- lapply(manifest$cache_pq, function(pq) {
    tryCatch(arrow::read_parquet(pq), error = function(e) NULL)
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) cli::cli_abort(msg$no_data)

  result <- do.call(rbind, parts)
  result <- result[order(result$code_muni, result$date), ]
  rownames(result) <- NULL
  result <- tibble::as_tibble(result)

  n_rows <- nrow(result)
  if (verbose) cli::cli_alert_success(
    glue::glue(msg$agg_done, n_rows = n_rows,
               n_mun = dplyr::n_distinct(result$code_muni)))

  meta <- list(
    system = NULL, stage = "climate", type = "pollution_ghap",
    spatial = FALSE,
    temporal = list(start = min(result$date, na.rm = TRUE),
                    end   = max(result$date, na.rm = TRUE),
                    source = "zenodo_ghap_cache"),
    created = Sys.time(), modified = Sys.time(), history =
      sprintf("[%s] sus_grid_pollution_ghap(): from Parquet cache, %d obs",
              format(Sys.time(), "%Y-%m-%d %H:%M:%S"), n_rows),
    user = list()
  )
  base_classes <- setdiff(class(result), "climasus_df")
  structure(result, sus_meta = meta, class = c("climasus_df", base_classes))
}

#' Download one GHAP file with cache
#' @keywords internal
#' @noRd
.ghap_download_file <- function(url, cache_path, use_cache, verbose, msg) {
  filename <- basename(cache_path)
  if (use_cache && file.exists(cache_path) && file.size(cache_path) > 0) {
    if (verbose) cli::cli_alert_success(
      glue::glue(msg$cache_hit, filename = filename))
    return(invisible(cache_path))
  }
  dir_path <- dirname(cache_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  if (verbose) cli::cli_alert_info(
    glue::glue(msg$download_file, filename = filename))
  tryCatch({
    utils::download.file(url = url, destfile = cache_path,
                         mode = "wb", quiet = !verbose, method = "auto")
    if (verbose) cli::cli_alert_success(
      glue::glue(msg$download_done, filename = filename))
    invisible(cache_path)
  }, error = function(e) {
    if (file.exists(cache_path)) unlink(cache_path)
    cli::cli_warn(glue::glue(msg$download_error, filename = filename,
                             err = conditionMessage(e)))
    invisible(NA_character_)
  })
}

#' Auto-detect municipality identifier column in an sf object
#' @keywords internal
#' @noRd
.ghap_detect_muni_col <- function(municipalities) {
  candidates <- c("code_muni", "CD_MUN", "CD_GEOCMU", "code_municipality")
  found <- intersect(candidates, names(municipalities))
  if (length(found) > 0) return(found[1])
  for (col in names(municipalities)) {
    vals <- as.character(municipalities[[col]])[!is.na(municipalities[[col]])]
    if (length(vals) > 0 &&
        all(grepl("^\\d{6,7}$", vals[seq_len(min(5, length(vals)))]))) {
      return(col)
    }
  }
  cli::cli_abort(c(
    "Could not detect a municipality identifier column.",
    "i" = "Expected one of: {.val {paste(candidates, collapse = ', ')}}."
  ))
}

#' Multilingual messages for sus_grid_pollution_ghap()
#' @keywords internal
#' @noRd
.ghap_msgs <- list(
  pt = list(
    title                  = "Dados GHAP de Polui\u00e7\u00e3o Atmosf\u00e9rica",
    invalid_pollutants_type = "{.arg pollutants} deve ser um vetor de caracteres.",
    invalid_pollutants     = "{.arg pollutants} inv\u00e1lido(s): {bad}. Use: {valid}.",
    no2_unavailable        = "NO2 ainda n\u00e3o est\u00e1 dispon\u00edvel no GHAP. Ser\u00e1 ignorado.",
    no_pollutants          = "Nenhum poluente v\u00e1lido selecionado.",
    invalid_resolution     = "{.arg resolution} inv\u00e1lido: {bad}. Use: {valid}.",
    invalid_months         = "{.arg months} deve ser um vetor inteiro entre 1 e 12.",
    invalid_agg            = "{.arg agg_fun} inv\u00e1lido. Op\u00e7\u00f5es: {valid}.",
    invalid_use_cache      = "{.arg use_cache} deve ser TRUE ou FALSE.",
    invalid_cache_dir      = "{.arg cache_dir} deve ser uma string n\u00e3o vazia.",
    need_sf                = "O pacote {.pkg sf} \u00e9 necess\u00e1rio para agregar por munic\u00edpios.",
    muni_not_sf            = "{.arg municipalities} deve ser um objeto {.cls sf}.",
    fallback_annual        = "{pollutant}: apenas dados anuais dispon\u00edveis no GHAP. Usando resolution='annual'.",
    years_unavail          = "{pollutant}: anos {bad_years} fora do intervalo dispon\u00edvel ({avail}). Ignorando.",
    no_data_to_download    = "Nenhum dado dispon\u00edvel para os par\u00e2metros fornecidos.",
    download_start         = "Baixando {n_files} arquivo(s) GHAP do Zenodo...",
    cache_hit              = "Cache encontrado: {filename}",
    download_file          = "Baixando: {filename}",
    download_done          = "Conclu\u00eddo: {filename}",
    download_error         = "Falha ao baixar {filename}: {err}",
    parquet_cache_hit      = "Todos os arquivos encontrados no cache Parquet. Carregando...",
    parquet_hit            = "Cache Parquet: {filename}",
    parquet_write_warn     = "N\u00e3o foi poss\u00edvel salvar cache Parquet: {filename}",
    skip_missing           = "Arquivo n\u00e3o dispon\u00edvel no cache: {filename}",
    extract_warn           = "N\u00e3o foi poss\u00edvel processar {filename}.",
    no_data                = "Nenhum dado foi extra\u00eddo com sucesso.",
    agg_start              = "Agregando para {n_mun} munic\u00edpio(s)...",
    agg_done               = "Conclu\u00eddo: {n_rows} observa\u00e7\u00f5es ({n_mun} munic\u00edpios).",
    zip_extract            = "Extraindo ZIP: {filename} (pode levar alguns minutos)...",
    zip_error              = "Falha ao extrair ZIP: {filename}.",
    zip_no_nc              = "Nenhum arquivo .nc encontrado dentro de {filename}.",
    daily_processing       = "Processando {n_days} dia(s) de {month}...",
    done_paths             = "{n} arquivo(s) dispon\u00edvel(is) no cache."
  ),
  en = list(
    title                  = "GHAP Atmospheric Pollution Data",
    invalid_pollutants_type = "{.arg pollutants} must be a character vector.",
    invalid_pollutants     = "Invalid {.arg pollutants}: {bad}. Use: {valid}.",
    no2_unavailable        = "NO2 is not yet publicly available in GHAP and will be skipped.",
    no_pollutants          = "No valid pollutants selected.",
    invalid_resolution     = "Invalid {.arg resolution}: {bad}. Use: {valid}.",
    invalid_months         = "{.arg months} must be an integer vector between 1 and 12.",
    invalid_agg            = "Invalid {.arg agg_fun}. Options: {valid}.",
    invalid_use_cache      = "{.arg use_cache} must be TRUE or FALSE.",
    invalid_cache_dir      = "{.arg cache_dir} must be a non-empty string.",
    need_sf                = "Package {.pkg sf} is required to aggregate by municipality.",
    muni_not_sf            = "{.arg municipalities} must be an {.cls sf} object.",
    fallback_annual        = "{pollutant}: only annual data available in GHAP. Using resolution='annual'.",
    years_unavail          = "{pollutant}: years {bad_years} outside available range ({avail}). Skipping.",
    no_data_to_download    = "No data available for the provided parameters.",
    download_start         = "Downloading {n_files} GHAP file(s) from Zenodo...",
    cache_hit              = "Cache found: {filename}",
    download_file          = "Downloading: {filename}",
    download_done          = "Done: {filename}",
    download_error         = "Failed to download {filename}: {err}",
    parquet_cache_hit      = "All files found in Parquet cache. Loading...",
    parquet_hit            = "Parquet cache: {filename}",
    parquet_write_warn     = "Could not write Parquet cache: {filename}",
    skip_missing           = "File not available in cache: {filename}",
    extract_warn           = "Could not process {filename}.",
    no_data                = "No data was successfully extracted.",
    agg_start              = "Aggregating to {n_mun} municipality/ies...",
    agg_done               = "Complete: {n_rows} observations ({n_mun} municipalities).",
    zip_extract            = "Extracting ZIP: {filename} (may take a few minutes)...",
    zip_error              = "Failed to extract ZIP: {filename}.",
    zip_no_nc              = "No .nc files found inside {filename}.",
    daily_processing       = "Processing {n_days} day(s) for {month}...",
    done_paths             = "{n} file(s) available in cache."
  ),
  es = list(
    title                  = "Datos GHAP de Contaminaci\u00f3n Atmosf\u00e9rica",
    invalid_pollutants_type = "{.arg pollutants} debe ser un vector de caracteres.",
    invalid_pollutants     = "{.arg pollutants} inv\u00e1lido(s): {bad}. Use: {valid}.",
    no2_unavailable        = "NO2 a\u00fan no est\u00e1 disponible en GHAP y ser\u00e1 omitido.",
    no_pollutants          = "Ning\u00fan contaminante v\u00e1lido seleccionado.",
    invalid_resolution     = "{.arg resolution} inv\u00e1lido: {bad}. Use: {valid}.",
    invalid_months         = "{.arg months} debe ser un vector entero entre 1 y 12.",
    invalid_agg            = "{.arg agg_fun} inv\u00e1lido. Opciones: {valid}.",
    invalid_use_cache      = "{.arg use_cache} debe ser TRUE o FALSE.",
    invalid_cache_dir      = "{.arg cache_dir} debe ser una cadena no vac\u00eda.",
    need_sf                = "El paquete {.pkg sf} es necesario para agregar por municipios.",
    muni_not_sf            = "{.arg municipalities} debe ser un objeto {.cls sf}.",
    fallback_annual        = "{pollutant}: solo datos anuales disponibles en GHAP. Usando resolution='annual'.",
    years_unavail          = "{pollutant}: a\u00f1os {bad_years} fuera del rango disponible ({avail}). Omitiendo.",
    no_data_to_download    = "No hay datos disponibles para los par\u00e1metros indicados.",
    download_start         = "Descargando {n_files} archivo(s) GHAP de Zenodo...",
    cache_hit              = "Cach\u00e9 encontrado: {filename}",
    download_file          = "Descargando: {filename}",
    download_done          = "Completado: {filename}",
    download_error         = "Error al descargar {filename}: {err}",
    parquet_cache_hit      = "Todos los archivos encontrados en cach\u00e9 Parquet. Cargando...",
    parquet_hit            = "Cach\u00e9 Parquet: {filename}",
    parquet_write_warn     = "No se pudo guardar cach\u00e9 Parquet: {filename}",
    skip_missing           = "Archivo no disponible en cach\u00e9: {filename}",
    extract_warn           = "No se pudo procesar {filename}.",
    no_data                = "No se extrajo ning\u00fan dato correctamente.",
    agg_start              = "Agregando a {n_mun} municipio(s)...",
    agg_done               = "Completo: {n_rows} observaciones ({n_mun} municipios).",
    zip_extract            = "Extrayendo ZIP: {filename} (puede tardar varios minutos)...",
    zip_error              = "Error al extraer ZIP: {filename}.",
    zip_no_nc              = "No se encontraron archivos .nc en {filename}.",
    daily_processing       = "Procesando {n_days} d\u00eda(s) de {month}...",
    done_paths             = "{n} archivo(s) disponible(s) en cach\u00e9."
  )
)
