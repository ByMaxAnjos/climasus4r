# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "code_muni", "date", "rainfall_chirps_mm", "layer_name",
  "bad", "valid", "err", "n_rows", "n_mun", "n_files", "filename"
))

# ── Exported function ─────────────────────────────────────────────────────────

#' Import CHIRPS Rainfall Data for Brazilian Municipalities
#'
#' @description
#' `sus_grid_chirps()` downloads CHIRPS v2.0 precipitation data, crops it to
#' Brazil, spatially aggregates to municipalities, and returns a `climasus_df`
#' compatible with [sus_mod_dlnm()] and [sus_climate_anomaly()].
#'
#' CHIRPS (Climate Hazards Group InfraRed Precipitation with Station data) is a
#' quasi-global, 0.05\eqn{^\circ} (~5 km) daily rainfall dataset from UCSB CHC
#' covering 1981 to present. It combines satellite imagery with station data and
#' is considered the best freely available high-resolution rainfall product for
#' Brazil — particularly for leptospirosis, diarrheal disease, and dengue
#' analyses where precipitation is the key exposure.
#'
#' No authentication is required.
#'
#' @param resolution Character. Temporal resolution of source files:
#'   \itemize{
#'     \item `"monthly"` (default) — one file per month (~5–20 MB each);
#'       returns mm/month.
#'     \item `"daily"` — one file per day (~3 MB each); returns mm/day.
#'       Recommended only for short periods; a full year = 365 downloads.
#'     \item `"annual"` — one file per year; returns mm/year.
#'   }
#'
#' @param years Integer vector. Years to download. Coverage: 1981 to present
#'   for daily/monthly; 1981–2024 for annual. `NULL` defaults to the last
#'   two complete years.
#'
#' @param months Integer vector (1–12). Months to include for daily and monthly
#'   resolutions. Ignored for annual. Default `1:12`.
#'
#' @param municipalities An `sf` POLYGON object (e.g., from
#'   [geobr::read_municipality()]). When provided, rasters are aggregated and a
#'   `climasus_df` is returned. If `NULL`, a named character vector of cached
#'   file paths is returned instead.
#'
#' @param agg_fun Character. Spatial aggregation function for
#'   `exactextractr::exact_extract()`. Default `"mean"` (area-weighted mean).
#'   For rainfall totals over a municipality use `"sum"`.
#'
#' @param crop_brazil Logical. Crop global rasters to Brazil's bounding box
#'   before aggregation. Reduces memory usage significantly. Default `TRUE`.
#'
#' @param use_cache Logical. Reuse previously downloaded raster files and
#'   aggregated Parquet caches. Default `TRUE`.
#'
#' @param cache_dir Character. Root cache directory.
#'   Default `"~/.climasus4r_cache/chirps"`.
#'
#' @param lang Character. Message language: `"pt"` (default), `"en"`, `"es"`.
#'
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return
#' \itemize{
#'   \item If `municipalities` is provided: a `climasus_df` with columns
#'     `code_muni` (character), `date` (Date), and `rainfall_chirps_mm`
#'     (numeric). Metadata: `stage = "climate"`, `type = "chirps"`.
#'   \item If `municipalities = NULL`: a named character vector of paths to
#'     the cached GeoTIFF/gz files.
#' }
#'
#' @section Units:
#' \itemize{
#'   \item Daily: mm/day
#'   \item Monthly: mm/month (cumulative)
#'   \item Annual: mm/year (cumulative)
#' }
#' CHIRPS missing/no-data value (-9999) is automatically converted to `NA`.
#'
#' @section Data source:
#' Funk, C. et al. (2015). The climate hazards infrared precipitation with
#' stations — a new environmental record for monitoring extremes.
#' *Scientific Data*, 2, 150066. \doi{10.1038/sdata.2015.66}\cr
#' Data: \url{https://data.chc.ucsb.edu/products/CHIRPS-2.0/}
#'
#' @section Packages required:
#' Spatial aggregation requires `terra` and `exactextractr` (both in
#' `Suggests`). Install with `install.packages(c("terra", "exactextractr"))`.
#'
#' @examples
#' \dontrun{
#' library(geobr)
#' mt_mun <- read_municipality(code_muni = "MT", year = 2020)
#'
#' # Monthly rainfall for Mato Grosso, 2020
#' chirps <- sus_grid_chirps(
#'   resolution    = "monthly",
#'   years         = 2020,
#'   municipalities = mt_mun,
#'   lang          = "pt"
#' )
#' sus_meta(chirps, "stage")  # "climate"
#' sus_meta(chirps, "type")   # "chirps"
#'
#' # Daily rainfall, fire season
#' chirps_daily <- sus_grid_chirps(
#'   resolution    = "daily",
#'   years         = 2020,
#'   months        = 7:9,
#'   municipalities = mt_mun,
#'   agg_fun       = "mean",
#'   lang          = "en"
#' )
#'
#' # Annual totals for all Brazil (no spatial aggregation)
#' paths <- sus_grid_chirps(
#'   resolution = "annual",
#'   years      = 2018:2022
#' )
#' }
#'
#' @seealso [sus_grid_era5()], [sus_climate_anomaly()], [sus_mod_dlnm()]
#'
#' @export
#' @importFrom rlang .data is_installed
#' @importFrom dplyr filter select rename mutate full_join join_by
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom lubridate year month days_in_month make_date
#' @importFrom tibble as_tibble
sus_grid_chirps <- function(
    resolution    = "monthly",
    years         = NULL,
    months        = 1:12,
    municipalities = NULL,
    agg_fun       = "mean",
    crop_brazil   = TRUE,
    use_cache     = TRUE,
    cache_dir     = "~/.climasus4r_cache/chirps",
    lang          = "pt",
    verbose       = TRUE) {

  # ── 1. Validation ───────────────────────────────────────────────────────────

  if (!is.character(lang) || length(lang) != 1 || !lang %in% c("pt", "en", "es")) {
    cli::cli_abort("{.arg lang} must be 'pt', 'en', or 'es'.")
  }
  msg <- .chirps_msgs[[lang]]

  # Resolution
  valid_res <- c("monthly", "daily", "annual")
  if (length(resolution) != 1 || !resolution %in% valid_res) {
    bad   <- paste(setdiff(resolution, valid_res), collapse = ", ")
    valid <- paste(valid_res, collapse = ", ")
    cli::cli_abort(msg$invalid_resolution)
  }

  # Years
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  max_year_annual <- 2024L   # CHIRPS annual stops at 2024 currently
  max_year <- if (resolution == "annual") max_year_annual else current_year

  if (is.null(years)) {
    years <- (current_year - 2L):(current_year - 1L)
    if (verbose) cli::cli_alert_info(
      glue::glue(msg$default_years, years = paste(range(years), collapse = "-")))
  } else {
    if (!is.numeric(years) || any(is.na(years))) cli::cli_abort(msg$invalid_years_type)
    years <- sort(as.integer(unique(years)))
    bad_y <- years[years < 1981L | years > max_year]
    if (length(bad_y) > 0) {
      bad <- paste(bad_y, collapse = ", ")
      cli::cli_abort(c(msg$invalid_years_range, "x" = msg$invalid_years_x))
    }
  }

  # Months
  if (!is.numeric(months) || any(is.na(months)) ||
      any(months < 1 | months > 12)) cli::cli_abort(msg$invalid_months)
  months <- sort(as.integer(unique(months)))

  # Municipalities
  if (!is.null(municipalities)) {
    if (!requireNamespace("sf", quietly = TRUE)) cli::cli_abort(msg$need_sf)
    if (!inherits(municipalities, "sf"))        cli::cli_abort(msg$muni_not_sf)
    rlang::check_installed("terra",
      reason = "to read CHIRPS GeoTIFF rasters")
    rlang::check_installed("exactextractr",
      reason = "to aggregate rasters to municipality polygons")
  }

  valid_agg <- c("mean", "sum", "median", "min", "max")
  if (!is.character(agg_fun) || length(agg_fun) != 1 || !agg_fun %in% valid_agg) {
    valid <- paste(valid_agg, collapse = ", ")
    cli::cli_abort(msg$invalid_agg)
  }

  if (!is.logical(use_cache) || length(use_cache) != 1) cli::cli_abort(msg$invalid_use_cache)
  if (!is.character(cache_dir) || nchar(trimws(cache_dir)) == 0) cli::cli_abort(msg$invalid_cache_dir)
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)

  # Cache Parquet fica no cache_dir/parquet, uma unidade (mês/ano) por
  # arquivo — precisa de um sufixo por conjunto de municípios, senão a
  # 1a chamada (ex. município do AC) grava o cache e qualquer chamada
  # seguinte com OUTRO conjunto de municípios (ex. RO) bate no mesmo
  # arquivo e devolve os dados do AC de volta, sem erro nenhum.
  muni_hash <- ""
  if (!is.null(municipalities)) {
    muni_col_probe <- .chirps_detect_muni_col(municipalities)
    muni_hash <- paste0("_", substr(digest::digest(
      sort(as.character(municipalities[[muni_col_probe]]))), 1, 10))
  }

  # ── 2. Build download manifest ───────────────────────────────────────────────
  manifest_rows <- list()

  for (yr in years) {
    if (resolution == "annual") {
      info <- .chirps_file_info("annual", yr, NA_integer_, NA_integer_)
      pq_path <- file.path(cache_dir, "parquet",
                           sprintf("chirps_annual_%04d%s.parquet", yr, muni_hash))
      manifest_rows[[length(manifest_rows) + 1]] <- list(
        resolution = "annual",
        year       = yr,
        month      = NA_integer_,
        day        = NA_integer_,
        filename   = info$filename,
        url        = info$url,
        cache_tif  = file.path(cache_dir, resolution, info$filename),
        cache_pq   = pq_path,
        date_val   = as.Date(sprintf("%04d-01-01", yr))
      )
    } else if (resolution == "monthly") {
      for (mo in months) {
        info    <- .chirps_file_info("monthly", yr, mo, NA_integer_)
        pq_path <- file.path(cache_dir, "parquet",
                             sprintf("chirps_monthly_%04d%02d%s.parquet", yr, mo, muni_hash))
        manifest_rows[[length(manifest_rows) + 1]] <- list(
          resolution = "monthly",
          year       = yr,
          month      = mo,
          day        = NA_integer_,
          filename   = info$filename,
          url        = info$url,
          cache_tif  = file.path(cache_dir, resolution, info$filename),
          cache_pq   = pq_path,
          date_val   = as.Date(sprintf("%04d-%02d-01", yr, mo))
        )
      }
    } else {
      # daily: one file per day
      for (mo in months) {
        n_days <- as.integer(lubridate::days_in_month(
          lubridate::make_date(yr, mo, 1L)))
        # One Parquet per month covers all days
        pq_path <- file.path(cache_dir, "parquet",
                             sprintf("chirps_daily_%04d%02d%s.parquet", yr, mo, muni_hash))
        for (dy in seq_len(n_days)) {
          info <- .chirps_file_info("daily", yr, mo, dy)
          manifest_rows[[length(manifest_rows) + 1]] <- list(
            resolution = "daily",
            year       = yr,
            month      = mo,
            day        = dy,
            filename   = info$filename,
            url        = info$url,
            cache_tif  = file.path(cache_dir, resolution, info$filename),
            cache_pq   = pq_path,   # shared per month
            date_val   = as.Date(sprintf("%04d-%02d-%02d", yr, mo, dy))
          )
        }
      }
    }
  }

  if (length(manifest_rows) == 0) cli::cli_abort(msg$no_data_params)

  manifest <- do.call(rbind, lapply(manifest_rows, as.data.frame,
                                     stringsAsFactors = FALSE))
  manifest$year  <- as.integer(manifest$year)
  manifest$month <- as.integer(manifest$month)
  manifest$day   <- as.integer(manifest$day)

  n_files <- nrow(manifest)
  if (verbose) {
    cli::cli_h1(msg$title)
    cli::cli_alert_info(glue::glue(msg$download_start, n_files = n_files))
  }

  # ── 3. Parquet early-return ──────────────────────────────────────────────────
  unique_pq <- unique(manifest$cache_pq)
  if (!is.null(municipalities) && use_cache && all(file.exists(unique_pq))) {
    if (verbose) cli::cli_alert_success(msg$parquet_cache_hit)
    return(.chirps_build_from_parquet(unique_pq, verbose, msg))
  }

  # ── 4. Download rasters ───────────────────────────────────────────────────────
  unique_files <- unique(manifest[, c("filename", "url", "cache_tif")])
  for (i in seq_len(nrow(unique_files))) {
    .chirps_download_file(
      url        = unique_files$url[i],
      cache_path = unique_files$cache_tif[i],
      use_cache  = use_cache,
      verbose    = verbose,
      msg        = msg
    )
  }

  # ── 5. Return file paths if no municipalities ────────────────────────────────
  if (is.null(municipalities)) {
    paths <- stats::setNames(unique_files$cache_tif, unique_files$filename)
    if (verbose) cli::cli_alert_success(
      glue::glue(msg$done_paths, n = length(paths)))
    return(paths)
  }

  # ── 6. Prepare municipalities ────────────────────────────────────────────────
  muni_id_col <- .chirps_detect_muni_col(municipalities)
  if (muni_id_col != "code_muni") {
    municipalities$code_muni <- as.character(municipalities[[muni_id_col]])
  } else {
    municipalities$code_muni <- as.character(municipalities$code_muni)
  }
  municipalities$code_muni <- substr(municipalities$code_muni, 1L, 7L)
  municipalities <- sf::st_transform(municipalities, crs = 4326)

  brazil_bbox <- if (crop_brazil) terra::ext(-75, -28, -35, 6) else NULL
  n_mun       <- nrow(municipalities)

  if (verbose) cli::cli_alert_info(
    glue::glue(msg$agg_start, n_mun = n_mun))

  # ── 7. Aggregate: group by Parquet unit (month for daily, 1 file per month/annual) ──

  # For daily: process all days in a month together to avoid repeated muni-join overhead
  pq_groups <- split(seq_len(nrow(manifest)), manifest$cache_pq)

  result_list <- lapply(names(pq_groups), function(pq_path) {
    idx_rows <- pq_groups[[pq_path]]

    # Check Parquet cache for this group
    if (use_cache && file.exists(pq_path) && file.size(pq_path) > 0) {
      if (verbose) cli::cli_alert_success(
        glue::glue(msg$parquet_hit, filename = basename(pq_path)))
      return(.read_parquet_smart(pq_path))
    }

    # Process each row in this group
    day_dfs <- lapply(idx_rows, function(i) {
      tif_path <- manifest$cache_tif[i]
      date_val <- as.Date(manifest$date_val[i])

      if (!file.exists(tif_path) || file.size(tif_path) == 0) {
        cli::cli_alert_warning(
          glue::glue(msg$skip_missing, filename = manifest$filename[i]))
        return(NULL)
      }

      tryCatch({
        # Read: .tif.gz via GDAL /vsigzip/, .tif directly
        raster_path <- if (endsWith(tif_path, ".gz")) {
          paste0("/vsigzip/", tif_path)
        } else {
          tif_path
        }
        r <- terra::rast(raster_path)

        # Replace CHIRPS no-data (-9999) with NA
        r[r < -999] <- NA

        # Crop to Brazil bbox (CHIRPS is standard WGS84, no flip needed)
        if (!is.null(brazil_bbox)) {
          r <- terra::crop(r, brazil_bbox)
        }

        # Ensure CRS is set
        if (is.na(terra::crs(r, describe = TRUE)$authority)) {
          terra::crs(r) <- "EPSG:4326"
        }

        # Area-weighted extraction
        agg_result <- exactextractr::exact_extract(
          x        = r,
          y        = municipalities,
          fun      = agg_fun,
          progress = FALSE
        )

        out_df <- data.frame(
          code_muni          = municipalities$code_muni,
          date               = date_val,
          rainfall_chirps_mm = as.numeric(agg_result),
          stringsAsFactors   = FALSE
        )
        out_df
      }, error = function(e) {
        cli::cli_warn(c(
          glue::glue(msg$extract_warn, filename = manifest$filename[i]),
          "i" = conditionMessage(e)
        ))
        NULL
      })
    })

    day_dfs <- day_dfs[!vapply(day_dfs, is.null, logical(1))]
    if (length(day_dfs) == 0) return(NULL)

    group_df <- do.call(rbind, day_dfs)

    # Save Parquet cache for this group
    pq_dir <- dirname(pq_path)
    if (!dir.exists(pq_dir)) {
      dir.create(pq_dir, recursive = TRUE, showWarnings = FALSE)
    }
    tryCatch(
      .write_parquet_smart(group_df, pq_path),
      error = function(e) cli::cli_alert_warning(
        glue::glue(msg$parquet_write_warn, filename = basename(pq_path)))
    )
    group_df
  })

  result_list <- result_list[!vapply(result_list, is.null, logical(1))]
  if (length(result_list) == 0) cli::cli_abort(msg$no_data)

  result <- do.call(rbind, result_list)
  result <- result[order(result$code_muni, result$date), ]
  rownames(result) <- NULL
  result <- tibble::as_tibble(result)

  n_rows <- nrow(result)
  if (verbose) cli::cli_alert_success(
    glue::glue(msg$agg_done, n_rows = n_rows, n_mun = n_mun))

  # ── 8. Build climasus_df ──────────────────────────────────────────────────────
  meta <- list(
    system   = NULL,
    stage    = "climate",
    type     = "chirps",
    spatial  = FALSE,
    temporal = list(
      start  = min(result$date, na.rm = TRUE),
      end    = max(result$date, na.rm = TRUE),
      unit   = switch(resolution, daily = "day", monthly = "month", "year"),
      source = "ucsb_chirps_v2"
    ),
    created          = Sys.time(),
    modified         = Sys.time(),
    resolution       = resolution,
    years            = years,
    months           = if (resolution != "annual") months else NULL,
    agg_fun          = agg_fun,
    n_municipalities = n_mun,
    n_observations   = n_rows,
    history          = sprintf(
      "[%s] sus_grid_chirps(): res=%s, %d obs",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"), resolution, n_rows
    ),
    user = list()
  )

  base_classes <- setdiff(class(result), "climasus_df")
  structure(result, sus_meta = meta, class = c("climasus_df", base_classes))
}


# ── Internal constants and helpers ────────────────────────────────────────────

#' Build CHIRPS filename and CHC URL
#' @keywords internal
#' @noRd
.chirps_file_info <- function(resolution, year, month, day) {
  base <- "https://data.chc.ucsb.edu/products/CHIRPS-2.0"

  if (resolution == "annual") {
    filename <- sprintf("chirps-v2.0.%04d.tif", year)
    url      <- sprintf("%s/global_annual/tifs/%s", base, filename)
  } else if (resolution == "monthly") {
    filename <- sprintf("chirps-v2.0.%04d.%02d.tif.gz", year, month)
    url      <- sprintf("%s/global_monthly/tifs/%s", base, filename)
  } else {
    filename <- sprintf("chirps-v2.0.%04d.%02d.%02d.tif.gz",
                        year, month, day)
    url      <- sprintf("%s/global_daily/tifs/p05/%04d/%s",
                        base, year, filename)
  }
  list(filename = filename, url = url)
}

#' Assemble climasus_df from pre-existing Parquet caches (early-return path)
#' @keywords internal
#' @noRd
.chirps_build_from_parquet <- function(pq_paths, verbose, msg) {
  parts <- lapply(pq_paths, function(p) {
    tryCatch(.read_parquet_smart(p), error = function(e) NULL)
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
    system = NULL, stage = "climate", type = "chirps", spatial = FALSE,
    temporal = list(start = min(result$date, na.rm = TRUE),
                    end   = max(result$date, na.rm = TRUE),
                    source = "ucsb_chirps_v2_cache"),
    created = Sys.time(), modified = Sys.time(),
    history = sprintf("[%s] sus_grid_chirps(): from Parquet cache, %d obs",
                      format(Sys.time(), "%Y-%m-%d %H:%M:%S"), n_rows),
    user = list()
  )
  base_classes <- setdiff(class(result), "climasus_df")
  structure(result, sus_meta = meta, class = c("climasus_df", base_classes))
}

#' Download one CHIRPS GeoTIFF file with cache
#' @keywords internal
#' @noRd
.chirps_download_file <- function(url, cache_path, use_cache, verbose, msg) {
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
    cli::cli_warn(glue::glue(msg$download_error,
                             filename = filename, err = conditionMessage(e)))
    invisible(NA_character_)
  })
}

#' Auto-detect municipality identifier column in sf object
#' @keywords internal
#' @noRd
.chirps_detect_muni_col <- function(municipalities) {
  candidates <- c("code_muni", "CD_MUN", "CD_GEOCMU", "code_municipality")
  found <- intersect(candidates, names(municipalities))
  if (length(found) > 0) return(found[1])
  for (col in names(municipalities)) {
    vals <- as.character(municipalities[[col]])[!is.na(municipalities[[col]])]
    if (length(vals) > 0 &&
        all(grepl("^\\d{6,7}$", vals[seq_len(min(5L, length(vals)))])))
      return(col)
  }
  cli::cli_abort(c(
    "Could not detect a municipality identifier column.",
    "i" = "Expected one of: {.val {paste(candidates, collapse = ', ')}}."
  ))
}

#' Multilingual messages for sus_grid_chirps()
#' @keywords internal
#' @noRd
.chirps_msgs <- list(
  pt = list(
    title               = "Dados CHIRPS de Precipita\u00e7\u00e3o",
    invalid_resolution  = "{.arg resolution} inv\u00e1lido: {bad}. Use: {valid}.",
    invalid_years_type  = "{.arg years} deve ser num\u00e9rico sem NA.",
    invalid_years_range = "{.arg years} deve estar entre 1981 e {max_year}.",
    invalid_years_x     = "Ano(s) inv\u00e1lido(s): {bad}.",
    default_years       = "Usando anos {years} (padr\u00e3o: \u00faltimos 2 anos completos).",
    invalid_months      = "{.arg months} deve ser inteiro entre 1 e 12.",
    need_sf             = "O pacote {.pkg sf} \u00e9 necess\u00e1rio para agregar por munic\u00edpios.",
    muni_not_sf         = "{.arg municipalities} deve ser um objeto {.cls sf}.",
    invalid_agg         = "{.arg agg_fun} inv\u00e1lido. Op\u00e7\u00f5es: {valid}.",
    invalid_use_cache   = "{.arg use_cache} deve ser TRUE ou FALSE.",
    invalid_cache_dir   = "{.arg cache_dir} deve ser uma string n\u00e3o vazia.",
    no_data_params      = "Nenhum dado dispon\u00edvel para os par\u00e2metros fornecidos.",
    download_start      = "Baixando {n_files} arquivo(s) CHIRPS do CHC/UCSB...",
    cache_hit           = "Cache encontrado: {filename}",
    download_file       = "Baixando: {filename}",
    download_done       = "Conclu\u00eddo: {filename}",
    download_error      = "Falha ao baixar {filename}: {err}",
    parquet_cache_hit   = "Todos os arquivos no cache Parquet. Carregando...",
    parquet_hit         = "Cache Parquet: {filename}",
    parquet_write_warn  = "N\u00e3o foi poss\u00edvel salvar cache Parquet: {filename}",
    skip_missing        = "Arquivo n\u00e3o dispon\u00edvel: {filename}",
    extract_warn        = "N\u00e3o foi poss\u00edvel processar {filename}.",
    no_data             = "Nenhum dado foi extra\u00eddo com sucesso.",
    agg_start           = "Agregando para {n_mun} munic\u00edpio(s)...",
    agg_done            = "Conclu\u00eddo: {n_rows} observa\u00e7\u00f5es ({n_mun} munic\u00edpios).",
    done_paths          = "{n} arquivo(s) GeoTIFF dispon\u00edvel(is) no cache."
  ),
  en = list(
    title               = "CHIRPS Precipitation Data",
    invalid_resolution  = "Invalid {.arg resolution}: {bad}. Use: {valid}.",
    invalid_years_type  = "{.arg years} must be numeric without NA.",
    invalid_years_range = "{.arg years} must be between 1981 and {max_year}.",
    invalid_years_x     = "Invalid year(s): {bad}.",
    default_years       = "Using years {years} (default: last 2 complete years).",
    invalid_months      = "{.arg months} must be integer between 1 and 12.",
    need_sf             = "Package {.pkg sf} is required to aggregate by municipality.",
    muni_not_sf         = "{.arg municipalities} must be an {.cls sf} object.",
    invalid_agg         = "Invalid {.arg agg_fun}. Options: {valid}.",
    invalid_use_cache   = "{.arg use_cache} must be TRUE or FALSE.",
    invalid_cache_dir   = "{.arg cache_dir} must be a non-empty string.",
    no_data_params      = "No data available for the provided parameters.",
    download_start      = "Downloading {n_files} CHIRPS file(s) from CHC/UCSB...",
    cache_hit           = "Cache found: {filename}",
    download_file       = "Downloading: {filename}",
    download_done       = "Done: {filename}",
    download_error      = "Failed to download {filename}: {err}",
    parquet_cache_hit   = "All files found in Parquet cache. Loading...",
    parquet_hit         = "Parquet cache: {filename}",
    parquet_write_warn  = "Could not write Parquet cache: {filename}",
    skip_missing        = "File not available: {filename}",
    extract_warn        = "Could not process {filename}.",
    no_data             = "No data was successfully extracted.",
    agg_start           = "Aggregating to {n_mun} municipality/ies...",
    agg_done            = "Complete: {n_rows} observations ({n_mun} municipalities).",
    done_paths          = "{n} GeoTIFF file(s) available in cache."
  ),
  es = list(
    title               = "Datos CHIRPS de Precipitaci\u00f3n",
    invalid_resolution  = "{.arg resolution} inv\u00e1lido: {bad}. Use: {valid}.",
    invalid_years_type  = "{.arg years} debe ser num\u00e9rico sin NA.",
    invalid_years_range = "{.arg years} debe estar entre 1981 y {max_year}.",
    invalid_years_x     = "A\u00f1o(s) inv\u00e1lido(s): {bad}.",
    default_years       = "Usando a\u00f1os {years} (por defecto: \u00faltimos 2 a\u00f1os completos).",
    invalid_months      = "{.arg months} debe ser entero entre 1 y 12.",
    need_sf             = "El paquete {.pkg sf} es necesario para agregar por municipios.",
    muni_not_sf         = "{.arg municipalities} debe ser un objeto {.cls sf}.",
    invalid_agg         = "{.arg agg_fun} inv\u00e1lido. Opciones: {valid}.",
    invalid_use_cache   = "{.arg use_cache} debe ser TRUE o FALSE.",
    invalid_cache_dir   = "{.arg cache_dir} debe ser una cadena no vac\u00eda.",
    no_data_params      = "No hay datos disponibles para los par\u00e1metros indicados.",
    download_start      = "Descargando {n_files} archivo(s) CHIRPS de CHC/UCSB...",
    cache_hit           = "Cach\u00e9 encontrado: {filename}",
    download_file       = "Descargando: {filename}",
    download_done       = "Completado: {filename}",
    download_error      = "Error al descargar {filename}: {err}",
    parquet_cache_hit   = "Todos los archivos en cach\u00e9 Parquet. Cargando...",
    parquet_hit         = "Cach\u00e9 Parquet: {filename}",
    parquet_write_warn  = "No se pudo guardar cach\u00e9 Parquet: {filename}",
    skip_missing        = "Archivo no disponible: {filename}",
    extract_warn        = "No se pudo procesar {filename}.",
    no_data             = "No se extrajo ning\u00fan dato correctamente.",
    agg_start           = "Agregando a {n_mun} municipio(s)...",
    agg_done            = "Completo: {n_rows} observaciones ({n_mun} municipios).",
    done_paths          = "{n} archivo(s) GeoTIFF disponible(s) en cach\u00e9."
  )
)
