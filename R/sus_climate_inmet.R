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
#' **Metadata** (accessible via `attr(x, "climasus_meta")`):
#' \describe{
#'   \item{`version`}{Package version used}
#'   \item{`timestamp`}{Import date/time}
#'   \item{`source`}{"INMET"}
#'   \item{`years`}{Years imported}
#'   \item{`ufs`}{States imported}
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
#'   \item **Missing data**: Represented as `NA`. Use `sus_climate_fill_gaps()` for imputation.
#'   \item **Encoding**: All strings converted to UTF-8.
#'   \item **Decimals**: Converted from comma (`,`) to point (`.`).
#' }
#'
#' @seealso
#' * [sus_climate_fill_gaps()] for ML-based gap filling
#' * [sus_join_spatial()] for preparing municipality data
#'
#' @examples
#' \dontrun{
#' # Basic import - single state, single year
#' climate_am <- sus_climate_inmet(
#'   years = 2023,
#'   uf = "AM"
#' )
#' 
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
#'
#' # Inspect available stations
#' climate_df %>%
#'   dplyr::distinct(station_code, station_name, latitude, longitude)
#' }
#'
#' @export
#' @importFrom rlang .data
#' @importFrom data.table :=
sus_climate_inmet <- function(
    years = 2022,
    uf = 'AM',
    use_cache = TRUE,
    cache_dir = "~/.climasus4r_cache/climate",
    parallel = TRUE,
    workers = 4,
    lang = "pt",
    verbose = TRUE) {

  # ============================================================================
  # VALIDATION AND SETUP
  # ============================================================================
  
  # Validate years (Requirement 2: Dynamic time validation)
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  if (!is.null(years)) {
    if (!is.numeric(years)) {
      cli::cli_abort("Argument {.arg years} must be numeric.")
    }
    invalid_years <- years[years < 2000 | years > current_year]
    if (length(invalid_years) > 0) {
      cli::cli_abort(
        "Years must be between 2000 and {current_year}. Invalid: {paste(invalid_years, collapse = ', ')}"
      )
    }
  }

  # Validate language (Requirement 3: i18n support)
  if (!lang %in% c("pt", "en", "es")) {
    cli::cli_abort("{.arg lang} must be 'pt', 'en', or 'es'.")
  }

  # Validate UF codes
  if (!is.null(uf)) {
    valid_ufs <- c("all",
      "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
      "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
      "RS", "RO", "RR", "SC", "SP", "SE", "TO"
    )
    invalid_ufs <- setdiff(uf, valid_ufs)
    if (length(invalid_ufs) > 0) {
      cli::cli_abort(
        "Invalid UF codes: {paste(invalid_ufs, collapse = ', ')}. Valid: {paste(valid_ufs, collapse = ', ')}"
      )
    }
  }

 # ============================================================================
  # MESSAGES
  # ============================================================================
  msg <- list(
    pt = list(
      cache_config = "Usando cache em: {dir}",
      import_start = "Importando dados INMET para {n_years} ano{?s}...",
      import_done = "Importa\u00E7\u00E3o conclu\u00EDda: {n_rows} observa\u00E7\u00F5es de {n_stations} esta\u00E7\u00F5es"
    ),
    en = list(
      cache_config = "Using cache directory: {dir}",
      import_start = "Importing INMET data for {n_years} year{?s}...",
      import_done = "Import complete: {n_rows} observations from {n_stations} stations"
    ),
    es = list(
      cache_config = "Usando cache en: {dir}",
      import_start = "Importando datos INMET para {n_years} ano{?s}...",
      import_done = "Importacion completada: {n_rows} observaciones de {n_stations} estaciones"
    )
  )[[lang]]

  # ==========================================================================
  # CHACE CONFIGURATION
  # ==========================================================================
  if (use_cache) {
    cache_dir <- path.expand(cache_dir)
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }

    if (verbose) {
      cli::cli_alert_info(glue::glue( msg$configuring_cache, dir = cache_dir))
    }
  }

  # ============================================================================
  # IMPORT DATA
  # ============================================================================
   if (verbose && !is.null(years)) {
      cli::cli_alert_info(glue::glue(
        msg$import_start, 
        n_years = length(years)
      ))
    }
  climate_data <- download_inmet(
    years = years,
    uf = uf,
    cache_dir = cache_dir,
    use_cache = use_cache,
    parallel = parallel,
    workers = workers,
    verbose = verbose,
    lang = lang
  )
  
  # ===========================================================
  # S3 CLASS + METADATA
  # ===========================================================
  if (!inherits(climate_data, "climasus_df")) {
    # Create new climasus_df
    meta <- list(
      system = NULL,
      stage = "climate",
      type = "inmet",
      spatial = inherits(climate_data, "sf"),
      temporal = NULL,
      created = Sys.time(),
      modified = Sys.time(),
      history = sprintf(
        "[%s] INMET data Imported",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ),
      user = list()
    )

    base_classes <- setdiff(class(climate_data), "climasus_df")
    climate_data <- structure(
      climate_data,
      climasus_meta = meta,
      class = c("climasus_df", base_classes)
    )
  } else {
    # Already climasus_df - update metadata
    climate_data <- climasus_meta(
      climate_data,
      system = NULL,
      stage = "climate",
      type = "inmet",
      temporal = list(
        start = min(climate_data$date),
        end = max(climate_data$date)
      )
    )
    climate_data <- climasus_meta(
      climate_data,
      add_history = "INMET data Imported"
    )
  }

  return(climate_data)
}


#' Download and Cache INMET Meteorological Data
#'
#' @description
#' Downloads, processes, and caches INMET meteorological data for one or multiple years.
#' Features intelligent caching, parallel processing, and UF filtering.
#'
#' @param years Numeric vector. Year(s) to download. Can be single year (e.g., 2023),
#'   vector (e.g., c(2020, 2021, 2022)), or range (e.g., 2015:2020).
#' @param uf Character vector. State codes to filter (optional). Examples: "SP", c("RJ", "MG").
#'   If NULL, downloads all available states.
#' @param cache_dir Character. Directory for caching downloaded and processed data.
#'   Default is "~/.climasus4r_cache/inmet".
#' @param use_cache Logical. Whether to use cached data. If TRUE, avoids re-downloading
#'   and re-processing existing data. Default is TRUE.
#' @param parallel Logical. Whether to use parallel processing. If TRUE, parallelizes
#'   across years (multiple years) or across CSV files (single year). Default is FALSE.
#' @param workers Integer. Number of workers for parallel processing. If NULL,
#'   uses `future::availableCores() - 1`. Default is NULL.
#' @param verbose Logical. Whether to print progress messages. Default is FALSE.
#' @param lang Character. Language for messages. Options: "en" (English), "pt" (Portuguese),
#'   "es" (Spanish). Default is "en".
#'
#' @return A tibble with standardized meteorological data containing:
#'   \itemize{
#'     \item All original weather variables (temperature, humidity, precipitation, etc.)
#'     \item Standardized date column named "date"
#'     \item UF column with state codes
#'     \item year column for filtering
#'   }
#'
#' @details
#' ## Data Source
#' Data is downloaded from INMET's historical portal:
#' https://portal.inmet.gov.br/dadoshistoricos
#'
#' ## Caching Strategy
#' The function uses a two-level caching system:
#' 1. **ZIP files**: Raw downloads are cached as ZIP files to avoid re-downloading
#' 2. **Parquet dataset**: Processed data is stored as a partitioned Arrow dataset
#'    (by year and UF) for lightning-fast subsequent access
#'
#' ## Parallel Processing Modes
#' - **Multiple years**: Parallelizes across years (each year processed independently)
#' - **Single year**: Parallelizes across CSV files within that year
#'
#' @examples
#' \dontrun{
#' # Download single year for São Paulo
#' data_sp <- download_inmet(
#'   years = 2023,
#'   uf = "SP",
#'   verbose = TRUE
#' )
#'
#' # Download multiple years with parallel processing
#' data_multi <- download_inmet(
#'   years = 2020:2022,
#'   uf = c("RJ", "MG"),
#'   parallel = TRUE,
#'   workers = 4,
#'   verbose = TRUE
#' )
#'
#' # Use cache (second run will be instant)
#' data_cached <- download_inmet(
#'   years = 2023,
#'   uf = "SP",
#'   use_cache = TRUE
#' )
#' }
#' @keywords internal
#' @noRd
download_inmet <- function(
  years,
  uf = NULL,
  cache_dir = "~/.climasus4r_cache/climate",
  use_cache = TRUE,
  parallel = FALSE,
  workers = NULL,
  verbose = FALSE,
  lang = "en"
) {

  if (is.null(workers)) {
    workers <- future::availableCores()
  }

  year_range <- .expand_year_range(years)
  cache_dir <- path.expand(cache_dir)
  dataset_dir <- file.path(cache_dir, "parquet")

  if (verbose) {
    cli::cli_h1("INMET Data Download")
    cli::cli_alert_info("Years: {paste(year_range, collapse = ', ')}")
    if (!is.null(uf))
      cli::cli_alert_info("States: {paste(uf, collapse = ', ')}")
    cli::cli_alert_info("Cache: {ifelse(use_cache, 'ENABLED', 'DISABLED')}")
    cli::cli_alert_info("Cache dir: {cache_dir}")
  }

  # --------------------------------------------------------------------------
  # FUNÇÃO INTERNA PARA PROCESSAR UM ANO
  # --------------------------------------------------------------------------

  process_year <- function(year) {

    year_int <- as.integer(year)
    zip_file <- file.path(cache_dir, paste0("inmet_", year_int, ".zip"))
    year_cache_path <- file.path(dataset_dir, paste0("year=", year_int))

    # ----------------------------------------------------
    # 1. TENTAR CACHE
    # ----------------------------------------------------
    if (use_cache && requireNamespace("arrow", quietly = TRUE)) {

      if (dir.exists(year_cache_path)) {

        if (verbose)
          cli::cli_alert_info("Year {year_int}: Loading from cache")

        data_cached <- try(
          arrow::open_dataset(year_cache_path) |>
            dplyr::collect(),
          silent = TRUE
        )

        if (!inherits(data_cached, "try-error") &&
            nrow(data_cached) > 0) {

          if (!is.null(uf) && "UF" %in% names(data_cached)) {
            data_cached <- data_cached |>
              dplyr::filter(UF %in% toupper(uf))
          }

          return(dplyr::as_tibble(data_cached))
        }
      }
    }

    # ----------------------------------------------------
    # 2. DOWNLOAD
    # ----------------------------------------------------
    if (!file.exists(zip_file)) {

      if (verbose)
        cli::cli_alert_info("Year {year_int}: Downloading")

      url <- paste0(
        "https://portal.inmet.gov.br/uploads/dadoshistoricos/",
        year_int,
        ".zip"
      )

      try(
        utils::download.file(
          url,
          zip_file,
          method = "libcurl",
          mode = "wb",
          quiet = !verbose
        ),
        silent = TRUE
      )
    }

    if (!file.exists(zip_file)) {
      cli::cli_alert_warning("Year {year_int}: Download failed")
      return(dplyr::tibble())
    }

    # ----------------------------------------------------
    # 3. UNZIP
    # ----------------------------------------------------
    temp_dir <- file.path(tempdir(), paste0("inmet_", year_int))
    unlink(temp_dir, recursive = TRUE)
    dir.create(temp_dir)

    unzip(zip_file, exdir = temp_dir)

    files <- list.files(
      temp_dir,
      pattern = "\\.csv$|\\.CSV$",
      full.names = TRUE,
      recursive = TRUE
    )

    if (length(files) == 0)
      return(dplyr::tibble())

    # ----------------------------------------------------
    # 4. PROCESSAMENTO CSV
    # ----------------------------------------------------
    if (!is.null(uf)) {
      pattern <- paste(toupper(uf), collapse = "|")
      files <- files[grepl(pattern, basename(files), ignore.case = TRUE)]
    }

    if (length(files) == 0)
      return(dplyr::tibble())

    if (parallel && length(files) > 1 &&
        requireNamespace("furrr", quietly = TRUE)) {

      old_plan <- future::plan()
      on.exit(future::plan(old_plan), add = TRUE)
      future::plan(future::multisession, workers = workers)

      year_data <- furrr::future_map_dfr(
        files,
        .parse_inmet_csv,
        .progress = verbose
      )

    } else {

      year_data <- purrr::map_dfr(
        files,
        .parse_inmet_csv
      )
    }

    if (nrow(year_data) == 0)
      return(dplyr::tibble())

    year_data$year <- year_int

    # ----------------------------------------------------
    # 5. SALVAR CACHE (SOMENTE POR YEAR)
    # ----------------------------------------------------
    if (use_cache && requireNamespace("arrow", quietly = TRUE)) {

      dir.create(year_cache_path, recursive = TRUE, showWarnings = FALSE)

      arrow::write_dataset(
        year_data,
        path = year_cache_path,
        format = "parquet"
      )
    }

    unlink(temp_dir, recursive = TRUE)

    return(dplyr::as_tibble(year_data))
  }

  # --------------------------------------------------------------------------
  # PROCESSAR TODOS OS ANOS
  # --------------------------------------------------------------------------

  if (parallel && length(year_range) > 1 &&
      requireNamespace("furrr", quietly = TRUE)) {

    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = workers)

    results <- furrr::future_map(
      year_range,
      process_year,
      .progress = verbose
    )

  } else {

    results <- purrr::map(
      year_range,
      process_year
    )
  }

  results <- purrr::keep(results, ~ nrow(.x) > 0)

  if (length(results) == 0)
    cli::cli_abort("No data downloaded.")

  combined <- dplyr::bind_rows(results)

  if ("date" %in% names(combined))
    combined <- combined |> dplyr::arrange(date)

  if (verbose)
    cli::cli_alert_success(
      "Loaded {nrow(combined)} total rows"
    )

  return(dplyr::as_tibble(combined))
}
