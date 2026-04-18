#' Import and Process INMET Meteorological Data
#'
#' @description
#' `sus_climate_inmet()` downloads, imports, standardizes, and quality-controls
#' Brazilian meteorological data from the National Institute of Meteorology (INMET).
#'
#' The function implements a **comprehensive processing pipeline**:
#' \enumerate{
#'   \item **Download**: Multi-year data with automatic retry and backoff
#'   \item **Parsing**: Handles INMET's CSV format with metadata extraction
#'   \item **Standardization**: Renames columns to canonical names (see **Variables**)
#'   \item **Quality Control**: Physical consistency checks (see **QC Details**)
#'   \item **Caching**: Two-level (memory + disk) with Parquet format
#'   \item **Parallel Processing**: Both between and within years
#' }
#'
#' @param years Numeric vector of year(s) to import.
#'   Examples: `2020`, `2020:2024`, `c(2019, 2021, 2023)`.
#'   Must be between 2000 and current year. If `NULL`, imports last 2 years.
#'
#' @param uf Character vector of Brazilian state codes (e.g., `"AM"`,
#'   `c("RJ", "MG")`). Case insensitive. If `NULL` (default), imports
#'   **all 27 states** (may be slow - see **Performance**).
#'
#' @param station_code Character vector of INMET station codes to filter
#'   (e.g., `c("A101", "A122")`). Optional. If provided, the function downloads
#'   data for the requested `years`/`uf` and then filters to include only the
#'   specified stations. If `NULL` (default), all stations in the selected
#'   states are returned. Station codes are matched case-insensitively.
#'
#' @param use_cache Logical. If `TRUE` (default), implements two-level caching:
#'   \itemize{
#'     \item **Session cache**: In-memory cache (MD5 hash of parameters)
#'     \item **Disk cache**: Parquet files with Zstandard compression
#'   }
#'   Use `unlink(cache_dir, recursive = TRUE)` to clear all caches.
#'
#' @param cache_dir Character. Directory path for disk cache.
#'   Default: `"~/.climasus4r_cache/climate"`. Created automatically.
#'
#' @param parallel Logical. If `TRUE` (default), enables **two levels** of parallelism:
#'   \itemize{
#'     \item **Between years**: Multiple years downloaded simultaneously
#'     \item **Within year**: CSV files for each year parsed in parallel
#'   }
#'   For single-year imports, only within-year parallelization applies.
#'
#' @param workers Integer. Number of parallel workers. Default: `4`.
#'   Ignored if `parallel = FALSE`. Uses `future::multisession` backend.
#'   Automatically capped at `availableCores() - 1`.
#'
#' @param lang Character. Message language. One of:
#'   \itemize{
#'     \item `"pt"`: Portuguese (default)
#'     \item `"en"`: English
#'     \item `"es"`: Spanish
#'   }
#'
#' @param verbose Logical. If `TRUE` (default), prints detailed progress including:
#'   \itemize{
#'     \item Cache hits/misses
#'     \item Download progress with retry attempts
#'     \item QC modifications (rows corrected/removed)
#'     \item Final statistics
#'   }
#'
#' @return
#' A `climasus_df` object (subclass of `tibble`) with class hierarchy:
#' `climasus_df` > `tbl_df` > `tbl` > `data.frame`
#'
#' **Data Columns:**
#' \describe{
#'   \item{`station_code`}{Character. INMET 8-digit station identifier (e.g., "A001")}
#'   \item{`station_name`}{Character. Full station name}
#'   \item{`region`}{Character. Brazilian region (Norte, Nordeste, etc.)}
#'   \item{`UF`}{Character. State abbreviation}
#'   \item{`latitude`, `longitude`, `altitude`}{Numeric. Station coordinates (WGS84)}
#'   \item{`date`}{POSIXct. Observation timestamp **UTC** (always hourly)}
#'   \item{`year`}{Integer. Year extracted from date}
#'   \item{**Climate variables**}{Numeric. Standardized names (see **Variables**)}
#' }
#'
#' **Metadata** (accessible via `attr(x, "sus_meta")`):
#' \describe{
#'   \item{`version`}{Package version used}
#'   \item{`timestamp`}{Import date/time}
#'   \item{`source`}{"INMET"}
#'   \item{`years`}{Years imported}
#'   \item{`ufs`}{States imported}
#'   \item{`station_codes`}{Station codes filtered (or NULL if all)}
#'   \item{`cache_used`}{Whether cache was used}
#'   \item{`qc_stats`}{List with quality control statistics}
#'   \item{`n_stations`}{Number of unique stations}
#'   \item{`n_observations`}{Total rows}
#'   \item{`temporal_coverage`}{List with `start` and `end` dates}
#'   \item{`history`}{Character vector of processing steps}
#' }
#'
#' @section Standardized Meteorological Variables:
#'
#' INMET raw column names vary by year. This function automatically detects and
#' renames them to the following canonical names:
#'
#' | **Canonical Name** | **Description** | **Unit** | **Physical Range** |
#' |--------------------|-----------------|----------|-------------------|
#' | `rainfall_mm` | Precipitation total | mm | 0 - 500 |
#' | `patm_mb` | Mean atmospheric pressure | mb | 700 - 1100 |
#' | `patm_max_mb` | Max atmospheric pressure | mb | 700 - 1100 |
#' | `patm_min_mb` | Min atmospheric pressure | mb | 700 - 1100 |
#' | `sr_kj_m2` | Solar radiation | kJ/m² | 0 - 40000 |
#' | `tair_dry_bulb_c` | Mean air temperature | °C | -90 - 60 |
#' | `tair_max_c` | Max air temperature | °C | -90 - 60 |
#' | `tair_min_c` | Min air temperature | °C | -90 - 60 |
#' | `dew_tmean_c` | Mean dew point | °C | -90 - 60 |
#' | `dew_tmax_c` | Max dew point | °C | -90 - 60 |
#' | `dew_tmin_c` | Min dew point | °C | -90 - 60 |
#' | `rh_mean_porc` | Mean relative humidity | % | 0 - 100 |
#' | `rh_max_porc` | Max relative humidity | % | 0 - 100 |
#' | `rh_min_porc` | Min relative humidity | % | 0 - 100 |
#' | `ws_2_m_s` | Wind speed at 2m | m/s | 0 - 100 |
#' | `ws_gust_m_s` | Wind gust | m/s | 0 - 100 |
#' | `wd_degrees` | Wind direction | degrees | 0 - 360 |
#'
#' @section Quality Control Details:
#'
#' The function applies **automatic physical consistency checks**:
#'
#' **1. Physical Range Validation:**
#' Values outside physically possible ranges are set to `NA`:
#' \itemize{
#'   \item **Temperature**: -90°C to 60°C
#'   \item **Pressure**: 700 mb to 1100 mb
#'   \item **Humidity**: 0% to 100%
#'   \item **Precipitation**: 0 mm to 500 mm
#'   \item **Solar radiation**: 0 to 40000 kJ/m²
#'   \item **Wind speed**: 0 to 100 m/s
#'   \item **Wind direction**: 0° to 360°
#' }
#'
#' **2. Dew Point Consistency:**
#' Calculates theoretical dew point from T and RH using Magnus formula.
#' If |observed - calculated| > 3°C, observed is set to NA.
#'
#' **3. Solar Radiation:**
#' Nighttime values (18h-6h) are set to 0 for physical consistency.
#'
#' @section Caching System Details:
#'
#' **Disk Cache:**
#' \itemize{
#'   \item **Format**: Apache Parquet with Zstandard compression (level 6)
#'   \item **Partitioning**: By `year` and `UF` for fast filtering
#'   \item **Backup**: Compressed CSV as fallback if Parquet corrupted
#'   \item **Location**: `~/.climasus4r_cache/climate/inmet_parquet/`
#' }
#'
#' @note
#' \itemize{
#'   \item **Data frequency**: Always **hourly**. Use `sus_climate_aggregate()` for daily/weekly.
#'   \item **Timezone**: All timestamps are **UTC**. Convert if needed.
#'   \item **Missing data**: Represented as `NA`. Use `sus_climate_fill_inmet()` for imputation.
#'   \item **Encoding**: All strings converted to UTF-8.
#'   \item **Decimals**: Converted from comma (`,`) to point (`.`).
#' }
#'
#' @seealso
#' * [sus_climate_fill_inmet()] for ML-based gap filling
#' * [sus_spatial_join()] for preparing municipality data
#'
#' @examples
#' \dontrun{
#' # Basic import - single state, single year
#' climate_am <- sus_climate_inmet(
#'   years = 2023,
#'   uf = "AM"
#' )
#'
#' # Filter by specific station codes
#' climate_sp <- sus_climate_inmet(
#'   years = 2023,
#'   uf = "SP",
#'   station_code = c("A701", "A711")
#' )
#'
#' # Multi-year import with parallel processing
#' climate_sp <- sus_climate_inmet(
#'   years = 2020:2024,
#'   uf = "SP",
#'   parallel = TRUE,
#'   workers = 4
#' )
#'
#' # Import all Southeast states
#' climate_se <- sus_climate_inmet(
#'   years = 2023,
#'   uf = c("SP", "RJ", "MG", "ES"),
#'   verbose = TRUE
#' )
#'
#' # Inspect available stations
#' climate_df |>
#'   dplyr::distinct(station_code, station_name, latitude, longitude)
#' }
#'
#' @export
#' @importFrom rlang .data
#' @importFrom data.table :=
sus_climate_inmet <- function(
    years        = NULL,
    uf           = NULL,
    station_code = NULL,
    use_cache    = TRUE,
    cache_dir    = "~/.climasus4r_cache/climate",
    parallel     = TRUE,
    workers      = 4,
    lang         = "pt",
    verbose      = TRUE) {

  # ============================================================================
  # VALIDATION AND SETUP
  # ============================================================================

  # --- years ------------------------------------------------------------------
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  if (!is.null(years)) {
    if (!is.numeric(years) || any(is.na(years))) {
      cli::cli_abort(
        "{.arg years} must be a numeric vector without NA values."
      )
    }
    years <- as.integer(years)
    # Allow current year: INMET publishes partial data throughout the year.
    # Reject only clearly future years (> current_year).
    invalid_years <- years[years < 2000 | years > current_year]
    if (length(invalid_years) > 0) {
      cli::cli_abort(
        c(
          "Invalid values in {.arg years}.",
          "x" = "Years must be between 2000 and {current_year}.",
          "i" = "Invalid: {paste(invalid_years, collapse = ', ')}",
          "i" = "Note: partial data for {current_year} may be available."
        )
      )
    }
  } else {
    years <- (current_year - 1):current_year
    if (verbose)
      cli::cli_alert_info(
        "{.arg years} not supplied - defaulting to {paste(years, collapse = '-')}."
      )
  }

  # --- lang -------------------------------------------------------------------
  if (!is.character(lang) || length(lang) != 1 || !lang %in% c("pt", "en", "es")) {
    cli::cli_abort("{.arg lang} must be one of 'pt', 'en', or 'es'.")
  }

  # --- uf ---------------------------------------------------------------------
  valid_ufs <- c(
    "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
    "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
    "RS", "RO", "RR", "SC", "SP", "SE", "TO"
  )
  if (!is.null(uf)) {
    if (!is.character(uf) || any(is.na(uf)) || any(nchar(trimws(uf)) == 0)) {
      cli::cli_abort(
        "{.arg uf} must be a character vector of non-empty, non-NA state codes."
      )
    }
    uf <- toupper(trimws(uf))
    invalid_ufs <- setdiff(uf, valid_ufs)
    if (length(invalid_ufs) > 0) {
      cli::cli_abort(
        c(
          "Invalid values in {.arg uf}.",
          "x" = "Unknown UF code(s): {paste(invalid_ufs, collapse = ', ')}.",
          "i" = "Valid codes: {paste(valid_ufs, collapse = ', ')}"
        )
      )
    }
  }

  # --- station_code -----------------------------------------------------------
  if (!is.null(station_code)) {
    if (!is.character(station_code) ||
        any(is.na(station_code)) ||
        any(nchar(trimws(station_code)) == 0)) {
      cli::cli_abort(
        "{.arg station_code} must be a character vector of non-empty, non-NA station codes."
      )
    }
    station_code <- toupper(trimws(station_code))
  }

  # --- use_cache / cache_dir --------------------------------------------------
  if (!is.logical(use_cache) || length(use_cache) != 1 || is.na(use_cache)) {
    cli::cli_abort("{.arg use_cache} must be a single logical value (TRUE or FALSE).")
  }

  if (!is.character(cache_dir) || length(cache_dir) != 1 || nchar(trimws(cache_dir)) == 0) {
    cli::cli_abort("{.arg cache_dir} must be a single non-empty character string.")
  }

  # --- parallel / workers -----------------------------------------------------
  if (!is.logical(parallel) || length(parallel) != 1 || is.na(parallel)) {
    cli::cli_abort("{.arg parallel} must be a single logical value (TRUE or FALSE).")
  }

  if (!is.numeric(workers) || length(workers) != 1 ||
      is.na(workers) || workers < 1) {
    cli::cli_abort("{.arg workers} must be a single positive integer.")
  }
  workers <- as.integer(workers)

  # ============================================================================
  # MESSAGES
  # ============================================================================
  msg <- list(
    pt = list(
      cache_config  = "Usando cache em: {dir}",
      import_start  = "Importando dados INMET para {n_years} ano{?s}...",
      import_done   = "Importa\u00E7\u00E3o conclu\u00EDda: {n_rows} observa\u00E7\u00F5es de {n_stations} esta\u00E7\u00F5es",
      filter_code   = "Filtrando por {length(station_code)} c\u00F3digo{?s} de esta\u00E7\u00E3o...",
      no_rows_code  = "Nenhuma observa\u00E7\u00E3o encontrada para os c\u00F3digos fornecidos em {.arg station_code}: {paste(station_code, collapse = ', ')}"
    ),
    en = list(
      cache_config  = "Using cache directory: {dir}",
      import_start  = "Importing INMET data for {n_years} year{?s}...",
      import_done   = "Import complete: {n_rows} observations from {n_stations} stations",
      filter_code   = "Filtering by {length(station_code)} station code{?s}...",
      no_rows_code  = "No observations found for the provided {.arg station_code} value(s): {paste(station_code, collapse = ', ')}"
    ),
    es = list(
      cache_config  = "Usando cache en: {dir}",
      import_start  = "Importando datos INMET para {n_years} a\u00F1o{?s}...",
      import_done   = "Importaci\u00F3n completada: {n_rows} observaciones de {n_stations} estaciones",
      filter_code   = "Filtrando por {length(station_code)} c\u00F3digo{?s} de estaci\u00F3n...",
      no_rows_code  = "No se encontraron observaciones para los c\u00F3digos de {.arg station_code}: {paste(station_code, collapse = ', ')}"
    )
  )[[lang]]

  # ============================================================================
  # CACHE CONFIGURATION
  # ============================================================================
  if (use_cache) {
    cache_dir <- path.expand(cache_dir)
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }
    if (verbose) {
      cli::cli_alert_info(glue::glue(msg$cache_config, dir = cache_dir))
    }
  }

  # ============================================================================
  # IMPORT DATA
  # ============================================================================
  if (verbose) {
    cli::cli_alert_info(glue::glue(msg$import_start, n_years = length(years)))
  }

  climate_data <- download_inmet(
    years     = years,
    uf        = uf,
    cache_dir = cache_dir,
    use_cache = use_cache,
    parallel  = parallel,
    workers   = workers,
    verbose   = verbose,
    lang      = lang
  )

  # ============================================================================
  # FILTER BY station_code (NEW ARGUMENT)
  # ============================================================================
  if (!is.null(station_code)) {

    if (!"station_code" %in% names(climate_data)) {
      cli::cli_abort(
        c(
          "Cannot filter by {.arg station_code}.",
          "x" = "Column {.field station_code} not found in the downloaded data.",
          "i" = "Check that the INMET data for the selected years/states contains station identifiers."
        )
      )
    }

    if (verbose) {
      cli::cli_alert_info(glue::glue(msg$filter_code))
    }

    climate_data <- dplyr::filter(
      climate_data,
      toupper(.data$station_code) %in% station_code
    )

    if (nrow(climate_data) == 0) {
      cli::cli_abort(glue::glue(msg$no_rows_code))
    }
  }

  # ============================================================================
  # S3 CLASS + METADATA
  # ============================================================================
  n_stations_final <- if ("station_code" %in% names(climate_data))
    dplyr::n_distinct(climate_data$station_code) else NA_integer_

  if (!inherits(climate_data, "climasus_df")) {
    meta <- list(
      system   = NULL,
      stage    = "climate",
      type     = "inmet",
      spatial  = inherits(climate_data, "sf"),
      temporal = list(
        start = min(climate_data$date, na.rm = TRUE),
        end   = max(climate_data$date, na.rm = TRUE)
      ),
      created          = Sys.time(),
      modified         = Sys.time(),
      years            = years,
      ufs              = uf,
      station_codes    = station_code,
      n_stations       = n_stations_final,
      n_observations   = nrow(climate_data),
      history          = sprintf(
        "[%s] INMET data imported",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ),
      user = list()
    )

    base_classes <- setdiff(class(climate_data), "climasus_df")
    climate_data <- structure(
      climate_data,
      sus_meta = meta,
      class    = c("climasus_df", base_classes)
    )
  } else {
    climate_data <- sus_meta(
      climate_data,
      system  = NULL,
      stage   = "climate",
      type    = "inmet",
      temporal = list(
        start = min(climate_data$date, na.rm = TRUE),
        end   = max(climate_data$date, na.rm = TRUE)
      )
    )
    climate_data <- sus_meta(
      climate_data,
      add_history = "INMET data imported"
    )
  }

  if (verbose) {
    cli::cli_alert_success(
      glue::glue(
        msg$import_done,
        n_rows     = nrow(climate_data),
        n_stations = n_stations_final
      )
    )
  }

  return(climate_data)
}


# ==============================================================================
# INTERNAL: download_inmet()
# ==============================================================================

#' Download and Cache INMET Meteorological Data
#'
#' @description
#' Downloads, processes, and caches INMET meteorological data for one or
#' multiple years. Features intelligent caching, parallel processing, UF
#' filtering, and robust download with retries.
#'
#' @param years Numeric vector. Year(s) to download.
#' @param uf Character vector. State codes to filter (optional).
#' @param cache_dir Character. Directory for caching.
#' @param use_cache Logical. Whether to use cached data.
#' @param parallel Logical. Whether to use parallel processing.
#' @param workers Integer. Number of workers for parallel processing.
#' @param verbose Logical. Whether to print progress messages.
#' @param lang Character. Language for messages ("en", "pt", "es").
#'
#' @return A tibble with standardized meteorological data.
#'
#' @details
#' ## Data Source
#' Data is downloaded from INMET's historical portal:
#' \url{https://portal.inmet.gov.br/dadoshistoricos}
#'
#' ## Download Robustness
#' The function uses a multi-method download strategy with up to
#' `max_retries` attempts, exponential back-off, and automatic fallback
#' between `libcurl`, `curl`, and `wget` methods. A temporary file is used
#' during download and only promoted to the final path on success, preventing
#' partial/corrupt ZIPs from poisoning the cache.
#'
#' ## Parallel Processing Modes
#' - **Multiple years**: Parallelizes across years (each year processed independently)
#' - **Single year**: Parallelizes across CSV files within that year
#'
#' @keywords internal
#' @noRd
download_inmet <- function(
    years,
    uf        = NULL,
    cache_dir = "~/.climasus4r_cache/climate",
    use_cache = TRUE,
    parallel  = FALSE,
    workers   = NULL,
    verbose   = FALSE,
    lang      = "en"
) {

  # --------------------------------------------------------------------------
  # Setup
  # --------------------------------------------------------------------------
  old_timeout <- getOption("timeout")
  options(timeout = max(3600, old_timeout))
  on.exit(options(timeout = old_timeout), add = TRUE)

  # Cap workers to what parallelly/OS policy actually allows.
  # parallelly::availableWorkers() respects mc.cores, cgroups, and the
  # parallelly.maxWorkers.localhost hard limit (300% CPU).
  # future::availableCores() alone does NOT enforce that policy, which
  # causes the checkNumberOfLocalWorkers() error when workers > cores.
  max_allowed <- tryCatch(
    if (requireNamespace("parallelly", quietly = TRUE))
      max(1L, length(parallelly::availableWorkers()))
    else
      max(1L, future::availableCores() - 1L),
    error = function(e) 1L
  )

  if (is.null(workers)) {
    workers <- max_allowed
  } else {
    workers_req <- as.integer(workers)
    workers     <- min(workers_req, max_allowed)
    if (workers < workers_req && verbose)
      cli::cli_alert_warning(
        "Requested {workers_req} workers capped to {workers} (machine limit)."
      )
  }

  # With only 1 effective worker, parallel gains nothing.
  use_parallel <- parallel && workers > 1L

  year_range  <- .expand_year_range(years)
  cache_dir   <- path.expand(cache_dir)
  dataset_dir <- file.path(cache_dir, "inmet_parquet")

  if (!dir.exists(cache_dir))
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  if (verbose) {
    cli::cli_h1("INMET Data Download")
    cli::cli_alert_info("Years: {paste(year_range, collapse = ', ')}")
    if (!is.null(uf))
      cli::cli_alert_info("States: {paste(uf, collapse = ', ')}")
    cli::cli_alert_info("Cache: {ifelse(use_cache, 'ENABLED', 'DISABLED')}")
    cli::cli_alert_info("Cache dir: {cache_dir}")
  }

  # --------------------------------------------------------------------------
  # Internal helper: robust single-file download with retry + back-off
  #
  # Root cause of "Connection reset by peer" / LibreSSL errors on macOS:
  #   The INMET server rejects connections that lack a browser-like User-Agent
  #   header, or that use the default LibreSSL cipher negotiation used by R's
  #   libcurl on macOS. Setting a realistic User-Agent and using httr2 (which
  #   wraps libcurl with better defaults) resolves the issue.
  #
  # Strategy (in order):
  #   1. httr2  — preferred: full header control, streaming write, retries
  #   2. httr   — fallback if httr2 is unavailable
  #   3. curl   — system curl binary with explicit --user-agent flag
  #   4. wget   — system wget binary with explicit --user-agent flag
  # --------------------------------------------------------------------------
  # Returns a list: list(ok = TRUE/FALSE, reason = "human-readable string")
  # Callers must check $ok and surface $reason on failure.
  .download_robust <- function(url, dest, max_retries = 3L, verbose = FALSE) {

    ua <- paste0(
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
      "AppleWebKit/537.36 (KHTML, like Gecko) ",
      "Chrome/124.0.0.0 Safari/537.36"
    )

    tmp        <- paste0(dest, ".tmp")
    last_reason <- "unknown error"
    on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)

    # TRUE  = success (file moved to dest)
    # FALSE = retriable failure
    # character string = permanent failure (HTTP 4xx) — do NOT retry
    try_httr2 <- function() {
      if (!requireNamespace("httr2", quietly = TRUE))
        return(FALSE)
      tryCatch({
        req <- httr2::request(url) |>
          httr2::req_headers(
            "User-Agent" = ua,
            "Accept"     = "application/zip, application/octet-stream, */*",
            "Referer"    = "https://portal.inmet.gov.br/dadoshistoricos"
          ) |>
          httr2::req_timeout(3600) |>
          # Only retry transient server errors, never 4xx
          httr2::req_retry(
            max_tries    = max_retries,
            backoff      = ~ 2 ^ .x,
            is_transient = \(resp) httr2::resp_status(resp) %in%
                             c(429L, 500L, 502L, 503L, 504L)
          )
        resp   <- httr2::req_perform(req, path = tmp)
        status <- httr2::resp_status(resp)
        if (status == 200L && file.exists(tmp) && file.size(tmp) > 0) {
          file.rename(tmp, dest)
          return(TRUE)
        }
        if (file.exists(tmp)) unlink(tmp)
        # 4xx = permanent; surface it immediately
        if (status >= 400L && status < 500L)
          return(sprintf("HTTP %d from server (permanent error)", status))
        FALSE
      }, error = function(e) {
        if (file.exists(tmp)) unlink(tmp)
        msg <- conditionMessage(e)
        # httr2 wraps HTTP errors; extract status if present
        if (grepl("HTTP 40[0-9]", msg))
          return(sub(".*(HTTP 40[0-9]).*", "\\1 (permanent error)", msg))
        if (verbose) cli::cli_alert_warning("httr2: {msg}")
        FALSE
      })
    }

    try_httr <- function() {
      if (!requireNamespace("httr", quietly = TRUE)) return(FALSE)
      tryCatch({
        resp <- httr::GET(
          url,
          httr::add_headers(
            "User-Agent" = ua,
            "Accept"     = "application/zip, application/octet-stream, */*",
            "Referer"    = "https://portal.inmet.gov.br/dadoshistoricos"
          ),
          httr::timeout(3600),
          httr::write_disk(tmp, overwrite = TRUE)
        )
        sc <- httr::status_code(resp)
        if (sc == 200L && file.exists(tmp) && file.size(tmp) > 0) {
          file.rename(tmp, dest)
          return(TRUE)
        }
        if (file.exists(tmp)) unlink(tmp)
        if (sc >= 400L && sc < 500L)
          return(sprintf("HTTP %d from server (permanent error)", sc))
        FALSE
      }, error = function(e) {
        if (file.exists(tmp)) unlink(tmp)
        if (verbose) cli::cli_alert_warning("httr: {conditionMessage(e)}")
        FALSE
      })
    }

    try_curl_bin <- function() {
      curl_bin <- Sys.which("curl")
      if (nchar(curl_bin) == 0) return(FALSE)
      # Write HTTP status code to a sidecar file so we can inspect it
      status_file <- paste0(tmp, ".status")
      on.exit(if (file.exists(status_file)) unlink(status_file), add = TRUE)
      tryCatch({
        ret <- system2(
          curl_bin,
          args   = c(
            "--silent", "--show-error", "--location",
            "--max-time", "3600",
            "--write-out", shQuote("%{http_code}"),
            "--user-agent", shQuote(ua),
            "--header", shQuote("Referer: https://portal.inmet.gov.br/dadoshistoricos"),
            "--output", shQuote(tmp),
            shQuote(url)
          ),
          stdout = status_file,
          stderr = if (verbose) "" else FALSE
        )
        http_code <- tryCatch(
          as.integer(trimws(readLines(status_file, warn = FALSE))),
          error = function(e) 0L
        )
        if (ret == 0L && http_code == 200L &&
            file.exists(tmp) && file.size(tmp) > 0) {
          file.rename(tmp, dest)
          return(TRUE)
        }
        if (file.exists(tmp)) unlink(tmp)
        if (http_code >= 400L && http_code < 500L)
          return(sprintf("HTTP %d from server (permanent error)", http_code))
        FALSE
      }, error = function(e) {
        if (file.exists(tmp)) unlink(tmp)
        if (verbose) cli::cli_alert_warning("curl binary: {conditionMessage(e)}")
        FALSE
      })
    }

    try_wget_bin <- function() {
      wget_bin <- Sys.which("wget")
      if (nchar(wget_bin) == 0) return(FALSE)
      tryCatch({
        stderr_out <- character(0)
        ret <- system2(
          wget_bin,
          args   = c(
            "--server-response", "--timeout=3600", "--tries=1",
            paste0("--user-agent=", shQuote(ua)),
            "--header", shQuote("Referer: https://portal.inmet.gov.br/dadoshistoricos"),
            "--output-document", shQuote(tmp),
            shQuote(url)
          ),
          stdout = FALSE,
          stderr = TRUE
        )
        # wget prints HTTP status to stderr
        http_line <- grep("HTTP/", ret, value = TRUE)
        http_code <- tryCatch(
          as.integer(sub(".*HTTP/[^ ]+ ([0-9]+).*", "\\1",
                         utils::tail(http_line, 1))),
          error = function(e) 0L
        )
        if (ret == 0L && file.exists(tmp) && file.size(tmp) > 0) {
          file.rename(tmp, dest)
          return(TRUE)
        }
        if (file.exists(tmp)) unlink(tmp)
        if (!is.na(http_code) && http_code >= 400L && http_code < 500L)
          return(sprintf("HTTP %d from server (permanent error)", http_code))
        FALSE
      }, error = function(e) {
        if (file.exists(tmp)) unlink(tmp)
        if (verbose) cli::cli_alert_warning("wget binary: {conditionMessage(e)}")
        FALSE
      })
    }

    for (attempt in seq_len(max_retries)) {

      if (verbose && attempt > 1L)
        cli::cli_alert_info("Download attempt {attempt}/{max_retries}...")

      for (try_fn in list(try_httr2, try_httr, try_curl_bin, try_wget_bin)) {
        result <- try_fn()
        if (isTRUE(result))
          return(list(ok = TRUE, reason = NULL))
        if (is.character(result)) {
          # Permanent HTTP 4xx — no point retrying with any method
          return(list(ok = FALSE, reason = result))
        }
        # FALSE = retriable; record and continue
        if (is.character(result)) last_reason <- result
      }

      if (attempt < max_retries) {
        wait_secs <- 2L ^ attempt
        last_reason <- sprintf(
          "all methods failed on attempt %d/%d", attempt, max_retries
        )
        if (verbose)
          cli::cli_alert_info(
            "All download methods failed (attempt {attempt}/{max_retries}). Retrying in {wait_secs}s..."
          )
        Sys.sleep(wait_secs)
      }
    }

    return(list(ok = FALSE, reason = last_reason))
  }

  # --------------------------------------------------------------------------
  # Internal: process one year
  # --------------------------------------------------------------------------
  process_year <- function(year) {

    year_int       <- as.integer(year)
    zip_file       <- file.path(cache_dir, paste0("inmet_", year_int, ".zip"))
    year_cache_path <- file.path(dataset_dir, paste0("year=", year_int))

    # ------------------------------------------------------------------
    # 1. Try disk cache (Parquet)
    # ------------------------------------------------------------------
    if (use_cache && requireNamespace("arrow", quietly = TRUE)) {

      if (dir.exists(year_cache_path)) {

        if (verbose)
          cli::cli_alert_info("Year {year_int}: Loading from Parquet cache")

        data_cached <- tryCatch(
          arrow::open_dataset(year_cache_path) |> dplyr::collect(),
          error = function(e) {
            if (verbose)
              cli::cli_alert_warning(
                "Year {year_int}: Cache read failed ({conditionMessage(e)}). Re-downloading."
              )
            NULL
          }
        )

        if (!is.null(data_cached) && nrow(data_cached) > 0) {
          if (!is.null(uf) && "UF" %in% names(data_cached)) {
            data_cached <- dplyr::filter(data_cached, .data$UF %in% toupper(uf))
          }
          return(dplyr::as_tibble(data_cached))
        }

        # Cache was empty or corrupt - remove and re-download
        if (verbose)
          cli::cli_alert_warning(
            "Year {year_int}: Cache empty or corrupt. Re-downloading."
          )
        unlink(year_cache_path, recursive = TRUE)
      }
    }

    # ------------------------------------------------------------------
    # 2. Download ZIP (if not already cached on disk)
    # ------------------------------------------------------------------

    # Remove zero-byte / corrupt ZIPs left by previous failed downloads
    if (file.exists(zip_file) && file.size(zip_file) == 0) {
      unlink(zip_file)
      if (verbose)
        cli::cli_alert_warning(
          "Year {year_int}: Removed empty ZIP from previous attempt."
        )
    }

    if (!file.exists(zip_file)) {

      # INMET URL pattern: https://portal.inmet.gov.br/uploads/dadoshistoricos/<year>.zip
      url <- paste0(
        "https://portal.inmet.gov.br/uploads/dadoshistoricos/",
        year_int,
        ".zip"
      )

      if (verbose)
        cli::cli_alert_info("Year {year_int}: Downloading from {url}")

      dl <- .download_robust(url, zip_file, max_retries = 3L, verbose = verbose)

      if (!isTRUE(dl$ok)) {
        reason <- if (!is.null(dl$reason)) dl$reason else "unknown error"

        # Distinguish permanent HTTP errors (e.g. 404 = year not yet published,
        # 403 = access denied) from transient network failures so the user
        # gets an actionable message instead of a generic one.
        if (grepl("HTTP 40[34]", reason)) {
          cli::cli_alert_warning(c(
            "Year {year_int}: {reason}.",
            "i" = if (grepl("404", reason))
              "The file for {year_int} may not be published yet at https://portal.inmet.gov.br/dadoshistoricos"
            else
              "Access denied (HTTP 403). Check firewall / proxy settings."
          ))
        } else {
          cli::cli_alert_warning(
            "Year {year_int}: Download failed ({reason}). Skipping."
          )
        }

        if (file.exists(zip_file)) unlink(zip_file)
        return(dplyr::tibble())
      }
    } else {
      if (verbose)
        cli::cli_alert_info("Year {year_int}: Using cached ZIP")
    }

    # ------------------------------------------------------------------
    # 3. Unzip
    # ------------------------------------------------------------------
    temp_dir <- file.path(tempdir(), paste0("inmet_", year_int, "_", Sys.getpid()))
    unlink(temp_dir, recursive = TRUE)
    dir.create(temp_dir, recursive = TRUE)
    on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

    unzip_result <- tryCatch({
      utils::unzip(zip_file, exdir = temp_dir)
      TRUE
    }, error = function(e) {
      cli::cli_alert_warning(
        "Year {year_int}: Failed to unzip ({conditionMessage(e)}). Removing corrupt ZIP."
      )
      unlink(zip_file)
      FALSE
    })

    if (!isTRUE(unzip_result)) return(dplyr::tibble())

    files <- list.files(
      temp_dir,
      pattern   = "\\.[Cc][Ss][Vv]$",
      full.names = TRUE,
      recursive  = TRUE
    )

    if (length(files) == 0) {
      cli::cli_alert_warning("Year {year_int}: No CSV files found inside ZIP.")
      return(dplyr::tibble())
    }

    # ------------------------------------------------------------------
    # 4. Filename-based UF pre-filter
    #
    # IMPORTANT: we keep TWO file lists:
    #   - `files_to_parse`: ALL CSVs in the ZIP (used for cache write)
    #   - `files_for_uf`:   only CSVs matching the requested UF(s)
    #
    # The cache must always store the complete national dataset so that
    # subsequent calls with a different `uf` can be served from cache
    # without re-downloading the ZIP.
    # ------------------------------------------------------------------
    files_all <- files  # full national set — used for cache write

    if (!is.null(uf)) {
      uf_pattern  <- paste(toupper(uf), collapse = "|")
      files_for_uf <- files[grepl(uf_pattern, basename(files), ignore.case = TRUE)]

      if (length(files_for_uf) == 0) {
        if (verbose)
          cli::cli_alert_warning(
            "Year {year_int}: No CSV files matched UF filter ({paste(toupper(uf), collapse = ', ')})."
          )
        return(dplyr::tibble())
      }
    } else {
      files_for_uf <- files_all
    }

    # ------------------------------------------------------------------
    # 5. Parse CSV files
    #
    # When a UF filter is active AND a valid cache does not yet exist,
    # we must parse all national files so the cache is complete.
    # If the cache already exists (hit at step 1), we never reach here.
    #
    # Within-year parallelism guards:
    #  a) use_parallel = TRUE and workers > 1
    #  b) not already inside a multisession worker (no nested futures)
    # ------------------------------------------------------------------
    cache_exists <- use_cache &&
                    requireNamespace("arrow", quietly = TRUE) &&
                    dir.exists(year_cache_path)

    # Files to actually parse: all files when we will write cache,
    # or only UF-filtered files when cache already exists (should not
    # reach here in that case, but defensive).
    files_to_parse <- if (!cache_exists) files_all else files_for_uf

    already_parallel <- !inherits(future::plan(), "SequentialFuture") &&
                        !inherits(future::plan(), "sequential")

    if (use_parallel && !already_parallel &&
        length(files_to_parse) > 1 &&
        requireNamespace("furrr", quietly = TRUE)) {

      old_plan <- future::plan()
      on.exit(future::plan(old_plan), add = TRUE)
      future::plan(future::multisession, workers = workers)

      year_data <- furrr::future_map_dfr(
        files_to_parse,
        .parse_inmet_csv,
        .progress = verbose
      )

    } else {
      year_data <- purrr::map_dfr(files_to_parse, .parse_inmet_csv)
    }

    if (nrow(year_data) == 0) {
      if (verbose)
        cli::cli_alert_warning("Year {year_int}: Parsing produced 0 rows.")
      return(dplyr::tibble())
    }

    year_data$year <- year_int

    # ------------------------------------------------------------------
    # 6. Write Parquet cache — always the full national dataset
    # ------------------------------------------------------------------
    if (!cache_exists && use_cache && requireNamespace("arrow", quietly = TRUE)) {

      dir.create(year_cache_path, recursive = TRUE, showWarnings = FALSE)

      tryCatch(
        arrow::write_dataset(
          year_data,
          path   = year_cache_path,
          format = "parquet"
        ),
        error = function(e) {
          if (verbose)
            cli::cli_alert_warning(
              "Year {year_int}: Failed to write Parquet cache ({conditionMessage(e)})."
            )
        }
      )
      if (verbose)
        cli::cli_alert_success("Year {year_int}: Full national dataset cached.")
    }

    # ------------------------------------------------------------------
    # 7. Apply UF column filter before returning (post-cache)
    # ------------------------------------------------------------------
    if (!is.null(uf) && "UF" %in% names(year_data)) {
      year_data <- dplyr::filter(year_data, .data$UF %in% toupper(uf))
    }

    return(dplyr::as_tibble(year_data))
  }

  # --------------------------------------------------------------------------
  # Process all years (parallel or sequential)
  # --------------------------------------------------------------------------
  if (use_parallel && length(year_range) > 1 &&
      requireNamespace("furrr", quietly = TRUE)) {

    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = workers)

    results <- furrr::future_map(year_range, process_year, .progress = verbose)

  } else {
    results <- purrr::map(year_range, process_year)
  }

  results <- purrr::keep(results, ~ nrow(.x) > 0)

  if (length(results) == 0) {
    # Build the most informative possible abort message.
    # If all failures were HTTP 404, the year(s) simply aren't published yet.
    cli::cli_abort(
      c(
        "No data could be downloaded for year(s): {paste(year_range, collapse = ', ')}.",
        "i" = "Check that the year(s) are published at https://portal.inmet.gov.br/dadoshistoricos",
        "i" = "If the year is current, data may not have been released yet.",
        "i" = "Verify your internet connection and any proxy / firewall settings.",
        "i" = "Run with {.code verbose = TRUE} for per-year failure details."
      )
    )
  }

  combined <- dplyr::bind_rows(results)

  if ("date" %in% names(combined))
    combined <- dplyr::arrange(combined, .data$date)

  if (verbose)
    cli::cli_alert_success("Loaded {nrow(combined)} total rows.")

  return(dplyr::as_tibble(combined))
}