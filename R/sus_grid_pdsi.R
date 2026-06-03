# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "code_muni", "date", "pdsi",
  "bad", "valid", "err", "n_rows", "n_mun", "n_files", "filename"
))

# ── Exported function ─────────────────────────────────────────────────────────

#' Import Palmer Drought Severity Index (PDSI) for Brazilian Municipalities
#'
#' @description
#' `sus_grid_pdsi()` downloads Palmer Drought Severity Index (PDSI) data,
#' crops it to Brazil, spatially aggregates to municipalities, and returns a
#' `climasus_df` compatible with [sus_mod_dlnm()] and [sus_climate_anomaly()].
#'
#' PDSI (Palmer, 1965) quantifies cumulative moisture departures relative to
#' local climate normals using a two-layer soil water balance model. Values
#' range from approximately -10 (extreme drought) to +10 (extreme wet), with
#' operational categories at -2 (moderate drought) to -4 (extreme drought).
#' Health applications in Brazil:
#' \itemize{
#'   \item Drought (PDSI < -2): malnutrition, diarrheal disease, mental health
#'     stress, vector-borne disease in semi-arid Northeast and Amazônia
#'   \item Wet periods (PDSI > 2): leptospirosis, hepatitis A, flooding
#' }
#'
#' **Available sources:**
#' \itemize{
#'   \item `"terraclimate"` (default): TerraClimate monthly PDSI (Abatzoglou
#'     et al., 2018; University of Idaho). Resolution 1/24° (~4 km), 1950–2025,
#'     WGS84, no authentication. One NetCDF file (~165 MB) per year.
#'   \item `"noaa_psl"`: Dai (2011) self-calibrated PDSI from NOAA Physical
#'     Sciences Laboratory. Resolution 2.5°, 1850–2018, single global file
#'     (~40 MB). Coarser but longer record.
#' }
#'
#' @param years Integer vector. Years to download.
#'   TerraClimate: 1950–2025. NOAA PSL: 1850–2018.
#'   `NULL` (default) = last two complete years.
#'
#' @param months Integer vector (1–12). Months to include. Default `1:12`.
#'
#' @param source Character. Data source: `"terraclimate"` (default) or
#'   `"noaa_psl"`.
#'
#' @param municipalities An `sf` POLYGON object (e.g., from
#'   [geobr::read_municipality()]). When provided, rasters are aggregated and a
#'   `climasus_df` is returned. If `NULL`, returns cached NetCDF file paths.
#'
#' @param agg_fun Character. Spatial aggregation for
#'   `exactextractr::exact_extract()`. Default `"mean"` (area-weighted).
#'
#' @param crop_brazil Logical. Crop global rasters to Brazil's bounding box.
#'   Default `TRUE`.
#'
#' @param use_cache Logical. Reuse cached NetCDF files and Parquet results.
#'   Default `TRUE`.
#'
#' @param cache_dir Character. Root cache directory.
#'   Default `"~/.climasus4r_cache/pdsi"`.
#'
#' @param lang Character. Message language: `"pt"` (default), `"en"`, `"es"`.
#'
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return
#' \itemize{
#'   \item If `municipalities` is provided: a `climasus_df` with columns
#'     `code_muni`, `date` (Date, first day of month), and `pdsi` (numeric,
#'     unitless). Metadata: `stage = "climate"`, `type = "pdsi"`.
#'   \item If `municipalities = NULL`: a named character vector of cached
#'     NetCDF file paths.
#' }
#'
#' @section PDSI classification:
#' \preformatted{
#'  PDSI >= +4.0 : Extremely wet
#'  PDSI  +3.0 to +3.99 : Very wet
#'  PDSI  +2.0 to +2.99 : Moderately wet
#'  PDSI  -1.99 to +1.99 : Near normal
#'  PDSI  -2.0 to -2.99 : Moderate drought  (D1)
#'  PDSI  -3.0 to -3.99 : Severe drought    (D2)
#'  PDSI <= -4.0 : Extreme drought           (D3-D4)
#' }
#'
#' @section Data sources:
#' \itemize{
#'   \item TerraClimate: Abatzoglou, J.T. et al. (2018). TerraClimate, a high-
#'     resolution global dataset of monthly climate and climatic water balance
#'     from 1958–2015. *Scientific Data*, 5, 170191.
#'     \doi{10.1038/sdata.2017.191}.
#'     URL: \url{https://climate.northwestknowledge.net/TERRACLIMATE-DATA/}
#'   \item NOAA PSL: Dai, A. (2011). Characteristics and trends in various
#'     forms of the PDSI during 1900–2008. *J. Geophys. Res.*, 116, D12115.
#'     \doi{10.1029/2010JD015541}.
#'     URL: \url{https://downloads.psl.noaa.gov/Datasets/dai_pdsi/}
#' }
#'
#' @section Packages required:
#' `terra` and `exactextractr` (both in `Suggests`) are required when
#' `municipalities` is provided.
#'
#' @examples
#' \dontrun{
#' library(geobr)
#' mt_mun <- read_municipality(code_muni = "MT", year = 2020)
#'
#' # TerraClimate PDSI for Mato Grosso, 2015-2022
#' pdsi_mt <- sus_grid_pdsi(
#'   years          = 2015:2022,
#'   months         = 1:12,
#'   municipalities = mt_mun,
#'   lang           = "pt"
#' )
#' sus_meta(pdsi_mt, "stage")  # "climate"
#' sus_meta(pdsi_mt, "type")   # "pdsi"
#'
#' # Longer history with NOAA PSL (2.5 deg)
#' pdsi_hist <- sus_grid_pdsi(
#'   years          = 1960:2018,
#'   source         = "noaa_psl",
#'   municipalities = mt_mun,
#'   lang           = "en"
#' )
#'
#' # Join to health data for DLNM
#' combined <- sus_grid_join(sih_mt, pdsi_mt)
#' }
#'
#' @seealso [sus_climate_compute_spi()], [sus_climate_compute_spei()],
#'   [sus_grid_chirps()], [sus_grid_join()], [sus_mod_dlnm()]
#'
#' @export
#' @importFrom rlang .data is_installed
#' @importFrom dplyr filter select rename full_join join_by
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom lubridate year month make_date
#' @importFrom tibble as_tibble
sus_grid_pdsi <- function(
    years         = NULL,
    months        = 1:12,
    source        = c("terraclimate", "noaa_psl"),
    municipalities = NULL,
    agg_fun       = "mean",
    crop_brazil   = TRUE,
    use_cache     = TRUE,
    cache_dir     = "~/.climasus4r_cache/pdsi",
    lang          = "pt",
    verbose       = TRUE) {

  # ── 1. Validation ───────────────────────────────────────────────────────────

  if (!is.character(lang) || length(lang) != 1 || !lang %in% c("pt", "en", "es")) {
    cli::cli_abort("{.arg lang} must be 'pt', 'en', or 'es'.")
  }
  msg <- .pdsi_msgs[[lang]]

  source <- match.arg(source)

  current_year <- as.integer(format(Sys.Date(), "%Y"))
  max_year <- switch(source, terraclimate = 2025L, noaa_psl = 2018L)
  min_year <- switch(source, terraclimate = 1950L, noaa_psl = 1850L)

  if (is.null(years)) {
    years <- (current_year - 2L):(current_year - 1L)
    if (years[2] > max_year) years <- years - (years[2] - max_year)
    if (verbose) cli::cli_alert_info(
      glue::glue(msg$default_years, years = paste(range(years), collapse = "-")))
  } else {
    if (!is.numeric(years) || any(is.na(years))) cli::cli_abort(msg$invalid_years_type)
    years <- sort(as.integer(unique(years)))
    bad_y <- years[years < min_year | years > max_year]
    if (length(bad_y) > 0) {
      bad <- paste(bad_y, collapse = ", ")
      cli::cli_abort(c(msg$invalid_years_range, "x" = msg$invalid_years_x))
    }
  }

  if (!is.numeric(months) || any(is.na(months)) || any(months < 1 | months > 12)) {
    cli::cli_abort(msg$invalid_months)
  }
  months <- sort(as.integer(unique(months)))

  if (!is.null(municipalities)) {
    if (!requireNamespace("sf", quietly = TRUE)) cli::cli_abort(msg$need_sf)
    if (!inherits(municipalities, "sf")) cli::cli_abort(msg$muni_not_sf)
    rlang::check_installed("terra",         reason = "to read PDSI NetCDF rasters")
    rlang::check_installed("exactextractr", reason = "to aggregate rasters to municipality polygons")
  }

  valid_agg <- c("mean", "sum", "median", "min", "max")
  if (!is.character(agg_fun) || length(agg_fun) != 1 || !agg_fun %in% valid_agg) {
    valid <- paste(valid_agg, collapse = ", ")
    cli::cli_abort(msg$invalid_agg)
  }

  if (!is.logical(use_cache) || length(use_cache) != 1) cli::cli_abort(msg$invalid_use_cache)
  if (!is.character(cache_dir) || nchar(trimws(cache_dir)) == 0) cli::cli_abort(msg$invalid_cache_dir)
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)

  # ── 2. Build manifest (one entry per year for TerraClimate; single file for NOAA PSL) ──

  if (source == "terraclimate") {
    manifest <- data.frame(
      year       = years,
      filename   = sprintf("TerraClimate_PDSI_%04d.nc", years),
      url        = sprintf(
        "https://climate.northwestknowledge.net/TERRACLIMATE-DATA/TerraClimate_PDSI_%04d.nc",
        years),
      cache_path = file.path(cache_dir, source,
                             sprintf("TerraClimate_PDSI_%04d.nc", years)),
      cache_pq   = file.path(cache_dir, "parquet",
                             sprintf("pdsi_terraclimate_%04d.parquet", years)),
      stringsAsFactors = FALSE
    )
  } else {
    # NOAA PSL: single file for all years
    manifest <- data.frame(
      year       = years[1],   # representative (single file)
      filename   = "pdsi.mon.mean.selfcalibrated.nc",
      url        = "https://downloads.psl.noaa.gov/Datasets/dai_pdsi/pdsi.mon.mean.selfcalibrated.nc",
      cache_path = file.path(cache_dir, source, "pdsi.mon.mean.selfcalibrated.nc"),
      cache_pq   = file.path(cache_dir, "parquet",
                             sprintf("pdsi_noaa_psl_%d_%d.parquet", min(years), max(years))),
      stringsAsFactors = FALSE
    )
  }

  n_files <- if (source == "terraclimate") nrow(manifest) else 1L
  if (verbose) {
    cli::cli_h1(msg$title)
    cli::cli_alert_info(glue::glue(msg$download_start,
                                    source = source, n_files = n_files))
  }

  # ── 3. Parquet early-return ──────────────────────────────────────────────────
  pq_paths <- unique(manifest$cache_pq)
  if (!is.null(municipalities) && use_cache && all(file.exists(pq_paths))) {
    if (verbose) cli::cli_alert_success(msg$parquet_cache_hit)
    return(.pdsi_build_from_parquet(pq_paths, verbose, msg))
  }

  # ── 4. Download rasters ───────────────────────────────────────────────────────
  unique_nc <- unique(manifest[, c("filename", "url", "cache_path")])
  for (i in seq_len(nrow(unique_nc))) {
    .pdsi_download_file(
      url        = unique_nc$url[i],
      cache_path = unique_nc$cache_path[i],
      use_cache  = use_cache,
      verbose    = verbose,
      msg        = msg
    )
  }

  # ── 5. Return file paths if no municipalities ────────────────────────────────
  if (is.null(municipalities)) {
    paths <- stats::setNames(unique_nc$cache_path, unique_nc$filename)
    if (verbose) cli::cli_alert_success(
      glue::glue(msg$done_paths, n = length(paths)))
    return(paths)
  }

  # ── 6. Prepare municipalities ────────────────────────────────────────────────
  muni_id_col <- .pdsi_detect_muni_col(municipalities)
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

  # ── 7. Extract by year ────────────────────────────────────────────────────────
  result_list <- lapply(seq_len(nrow(manifest)), function(i) {
    yr       <- manifest$year[i]
    nc_path  <- manifest$cache_path[i]
    pq_path  <- manifest$cache_pq[i]

    if (!file.exists(nc_path) || file.size(nc_path) == 0) {
      cli::cli_alert_warning(
        glue::glue(msg$skip_missing, filename = manifest$filename[i]))
      return(NULL)
    }

    if (use_cache && file.exists(pq_path) && file.size(pq_path) > 0) {
      if (verbose) cli::cli_alert_success(
        glue::glue(msg$parquet_hit, filename = basename(pq_path)))
      return(arrow::read_parquet(pq_path))
    }

    tryCatch({
      df_yr <- .pdsi_extract_year(
        nc_path   = nc_path,
        source    = source,
        year      = yr,
        months    = months,
        municipalities = municipalities,
        bbox      = brazil_bbox,
        agg_fun   = agg_fun
      )

      if (!is.null(df_yr) && nrow(df_yr) > 0) {
        pq_dir <- dirname(pq_path)
        if (!dir.exists(pq_dir)) dir.create(pq_dir, recursive = TRUE, showWarnings = FALSE)
        tryCatch(arrow::write_parquet(df_yr, pq_path),
                 error = function(e) cli::cli_alert_warning(
                   glue::glue(msg$parquet_write_warn, filename = basename(pq_path))))
      }
      df_yr
    }, error = function(e) {
      cli::cli_warn(c(
        glue::glue(msg$extract_warn, filename = manifest$filename[i]),
        "i" = conditionMessage(e)
      ))
      NULL
    })
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
    type     = "pdsi",
    spatial  = FALSE,
    temporal = list(
      start  = min(result$date, na.rm = TRUE),
      end    = max(result$date, na.rm = TRUE),
      unit   = "month",
      source = source
    ),
    created          = Sys.time(),
    modified         = Sys.time(),
    years            = years,
    months           = months,
    source           = source,
    n_municipalities = n_mun,
    n_observations   = n_rows,
    agg_fun          = agg_fun,
    history          = sprintf(
      "[%s] sus_grid_pdsi(): source=%s, %d obs",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"), source, n_rows
    ),
    user = list()
  )
  base_classes <- setdiff(class(result), "climasus_df")
  structure(result, sus_meta = meta, class = c("climasus_df", base_classes))
}


# ── Internal helpers ──────────────────────────────────────────────────────────

#' Extract PDSI from one annual NetCDF and aggregate to municipalities
#' @keywords internal
#' @noRd
.pdsi_extract_year <- function(nc_path, source, year, months,
                                municipalities, bbox, agg_fun) {
  var_name <- if (source == "terraclimate") "PDSI" else "pdsi"
  r <- terra::rast(nc_path, subds = var_name)

  # Extract dates from raster time metadata
  dates <- tryCatch(as.Date(terra::time(r)), error = function(e) NULL)
  if (is.null(dates) || length(dates) != terra::nlyr(r)) {
    # Fallback: assign months 1-12 for the given year
    n_layers <- terra::nlyr(r)
    dates    <- seq(lubridate::make_date(year, 1, 1),
                    by = "month", length.out = n_layers)
  }

  # Filter to requested months
  keep <- lubridate::month(dates) %in% months
  if (source == "noaa_psl") {
    # Also filter to requested years
    keep <- keep & lubridate::year(dates) %in% year
  }
  if (!any(keep)) return(NULL)

  r     <- r[[which(keep)]]
  dates <- dates[keep]

  # Crop to Brazil bbox
  if (!is.null(bbox)) r <- terra::crop(r, bbox)

  # Ensure standard CRS (TerraClimate and Dai PDSI are both WGS84)
  if (is.na(terra::crs(r)) || nchar(terra::crs(r)) == 0) {
    terra::crs(r) <- "EPSG:4326"
  }

  # Name layers by date for clean extraction
  names(r) <- format(dates, "d%Y%m%d")

  # Area-weighted extraction
  agg_result <- exactextractr::exact_extract(
    x = r, y = municipalities, fun = agg_fun, progress = FALSE)

  agg_df           <- as.data.frame(agg_result)
  agg_df$code_muni <- municipalities$code_muni

  # Pivot to long format
  date_cols  <- names(agg_df)[names(agg_df) != "code_muni"]
  prefix     <- paste0(agg_fun, ".")

  out_long <- tidyr::pivot_longer(
    agg_df,
    cols      = dplyr::all_of(date_cols),
    names_to  = "date_str",
    values_to = "pdsi"
  )
  out_long$date     <- as.Date(
    sub(paste0("^", prefix, "d"), "", out_long$date_str), format = "%Y%m%d")
  out_long$date_str <- NULL

  as.data.frame(out_long[, c("code_muni", "date", "pdsi")])
}

#' Download one PDSI NetCDF file with cache
#' @keywords internal
#' @noRd
.pdsi_download_file <- function(url, cache_path, use_cache, verbose, msg) {
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

#' Build climasus_df from Parquet cache
#' @keywords internal
#' @noRd
.pdsi_build_from_parquet <- function(pq_paths, verbose, msg) {
  parts <- lapply(pq_paths, function(p) {
    tryCatch(arrow::read_parquet(p), error = function(e) NULL)
  })
  parts  <- parts[!vapply(parts, is.null, logical(1))]
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
    system = NULL, stage = "climate", type = "pdsi", spatial = FALSE,
    temporal = list(start = min(result$date, na.rm = TRUE),
                    end   = max(result$date, na.rm = TRUE),
                    source = "cache"),
    created = Sys.time(), modified = Sys.time(),
    history = sprintf("[%s] sus_grid_pdsi(): from Parquet cache, %d obs",
                      format(Sys.time(), "%Y-%m-%d %H:%M:%S"), n_rows),
    user = list()
  )
  base_classes <- setdiff(class(result), "climasus_df")
  structure(result, sus_meta = meta, class = c("climasus_df", base_classes))
}

#' Auto-detect municipality identifier column
#' @keywords internal
#' @noRd
.pdsi_detect_muni_col <- function(municipalities) {
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

#' Multilingual messages for sus_grid_pdsi()
#' @keywords internal
#' @noRd
.pdsi_msgs <- list(
  pt = list(
    title               = "Dados PDSI (Palmer Drought Severity Index)",
    invalid_years_type  = "{.arg years} deve ser num\u00e9rico sem NA.",
    invalid_years_range = "{.arg years} deve estar entre 1850/1950 e 2018/2025 para esta fonte.",
    invalid_years_x     = "Ano(s) inv\u00e1lido(s): {bad}.",
    default_years       = "Usando anos {years} (padr\u00e3o: \u00faltimos 2 anos completos).",
    invalid_months      = "{.arg months} deve ser inteiro entre 1 e 12.",
    need_sf             = "O pacote {.pkg sf} \u00e9 necess\u00e1rio para agregar por munic\u00edpios.",
    muni_not_sf         = "{.arg municipalities} deve ser um objeto {.cls sf}.",
    invalid_agg         = "{.arg agg_fun} inv\u00e1lido. Op\u00e7\u00f5es: {valid}.",
    invalid_use_cache   = "{.arg use_cache} deve ser TRUE ou FALSE.",
    invalid_cache_dir   = "{.arg cache_dir} deve ser uma string n\u00e3o vazia.",
    download_start      = "Baixando {n_files} arquivo(s) PDSI ({source}) sem autentica\u00e7\u00e3o...",
    cache_hit           = "Cache encontrado: {filename}",
    download_file       = "Baixando: {filename}",
    download_done       = "Conclu\u00eddo: {filename}",
    download_error      = "Falha ao baixar {filename}: {err}",
    parquet_cache_hit   = "Todos os dados no cache Parquet. Carregando...",
    parquet_hit         = "Cache Parquet: {filename}",
    parquet_write_warn  = "N\u00e3o foi poss\u00edvel salvar cache Parquet: {filename}",
    skip_missing        = "Arquivo n\u00e3o dispon\u00edvel: {filename}",
    extract_warn        = "N\u00e3o foi poss\u00edvel processar {filename}.",
    no_data             = "Nenhum dado foi extra\u00eddo com sucesso.",
    agg_start           = "Agregando para {n_mun} munic\u00edpio(s)...",
    agg_done            = "Conclu\u00eddo: {n_rows} observa\u00e7\u00f5es ({n_mun} munic\u00edpios).",
    done_paths          = "{n} arquivo(s) NetCDF dispon\u00edvel(is) no cache."
  ),
  en = list(
    title               = "PDSI (Palmer Drought Severity Index) Data",
    invalid_years_type  = "{.arg years} must be numeric without NA.",
    invalid_years_range = "{.arg years} must be between 1850/1950 and 2018/2025 for this source.",
    invalid_years_x     = "Invalid year(s): {bad}.",
    default_years       = "Using years {years} (default: last 2 complete years).",
    invalid_months      = "{.arg months} must be integer between 1 and 12.",
    need_sf             = "Package {.pkg sf} is required to aggregate by municipality.",
    muni_not_sf         = "{.arg municipalities} must be an {.cls sf} object.",
    invalid_agg         = "Invalid {.arg agg_fun}. Options: {valid}.",
    invalid_use_cache   = "{.arg use_cache} must be TRUE or FALSE.",
    invalid_cache_dir   = "{.arg cache_dir} must be a non-empty string.",
    download_start      = "Downloading {n_files} PDSI file(s) ({source}), no authentication...",
    cache_hit           = "Cache found: {filename}",
    download_file       = "Downloading: {filename}",
    download_done       = "Done: {filename}",
    download_error      = "Failed to download {filename}: {err}",
    parquet_cache_hit   = "All data found in Parquet cache. Loading...",
    parquet_hit         = "Parquet cache: {filename}",
    parquet_write_warn  = "Could not write Parquet cache: {filename}",
    skip_missing        = "File not available: {filename}",
    extract_warn        = "Could not process {filename}.",
    no_data             = "No data was successfully extracted.",
    agg_start           = "Aggregating to {n_mun} municipality/ies...",
    agg_done            = "Complete: {n_rows} observations ({n_mun} municipalities).",
    done_paths          = "{n} NetCDF file(s) available in cache."
  ),
  es = list(
    title               = "Datos PDSI (\u00cdndice de Severidad de Sequ\u00eda de Palmer)",
    invalid_years_type  = "{.arg years} debe ser num\u00e9rico sin NA.",
    invalid_years_range = "{.arg years} debe estar entre 1850/1950 y 2018/2025 para esta fuente.",
    invalid_years_x     = "A\u00f1o(s) inv\u00e1lido(s): {bad}.",
    default_years       = "Usando a\u00f1os {years} (por defecto: \u00faltimos 2 a\u00f1os completos).",
    invalid_months      = "{.arg months} debe ser entero entre 1 y 12.",
    need_sf             = "El paquete {.pkg sf} es necesario para agregar por municipios.",
    muni_not_sf         = "{.arg municipalities} debe ser un objeto {.cls sf}.",
    invalid_agg         = "{.arg agg_fun} inv\u00e1lido. Opciones: {valid}.",
    invalid_use_cache   = "{.arg use_cache} debe ser TRUE o FALSE.",
    invalid_cache_dir   = "{.arg cache_dir} debe ser una cadena no vac\u00eda.",
    download_start      = "Descargando {n_files} archivo(s) PDSI ({source}), sin autenticaci\u00f3n...",
    cache_hit           = "Cach\u00e9 encontrado: {filename}",
    download_file       = "Descargando: {filename}",
    download_done       = "Completado: {filename}",
    download_error      = "Error al descargar {filename}: {err}",
    parquet_cache_hit   = "Todos los datos en cach\u00e9 Parquet. Cargando...",
    parquet_hit         = "Cach\u00e9 Parquet: {filename}",
    parquet_write_warn  = "No se pudo guardar cach\u00e9 Parquet: {filename}",
    skip_missing        = "Archivo no disponible: {filename}",
    extract_warn        = "No se pudo procesar {filename}.",
    no_data             = "No se extrajo ning\u00fan dato correctamente.",
    agg_start           = "Agregando a {n_mun} municipio(s)...",
    agg_done            = "Completo: {n_rows} observaciones ({n_mun} municipios).",
    done_paths          = "{n} archivo(s) NetCDF disponible(s) en cach\u00e9."
  )
)
