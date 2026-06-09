# -- NSE variable declarations (suppresses R CMD CHECK warnings) --------------
utils::globalVariables(c(
  # source columns from table_info.parquet
  "gauge_code", "city", "state", "lat", "long", "elevation",
  "time_step", "network", "responsible", "utc",
  # source columns from table_data.parquet
  "datetime", "rain_mm",
  # output columns after rename/join
  "station_code", "station_name", "uf", "latitude", "longitude",
  "altitude", "time_step_min", "utc_offset", "date", "rainfall_mm",
  # aggregation helpers
  "year", "month"
))

# -- Exported function --------------------------------------------------------

#' Import UNIPLU-BR: Unified Brazilian Rainfall Dataset
#'
#' @description
#' `sus_climate_uniplu()` downloads, caches, and imports data from the
#' **Unified Brazilian Rainfall Dataset (UNIPLU-BR)** — the most comprehensive
#' national rainfall database for Brazil, covering **21,000+ gauges** from
#' five monitoring networks over **140 years (1885–2025)**.
#'
#' The function implements a full processing pipeline:
#' \enumerate{
#'   \item **Download**: Single 1.6 GB ZIP from Zenodo (first call only)
#'   \item **Extraction**: Two Parquet files cached locally for instant re-use
#'   \item **Standardization**: Column names aligned with climasus4r conventions
#'   \item **Filtering**: By year, state (UF), and/or monitoring network
#'   \item **Aggregation**: Sub-daily data optionally aggregated to daily/monthly/yearly totals
#' }
#'
#' @param years Integer vector of year(s) to import.
#'   Examples: `2020`, `2020:2024`, `c(2015, 2020, 2024)`.
#'   Must be between 1885 and the current year. If `NULL` (default), imports
#'   the last 2 years.
#'
#' @param uf Character vector of Brazilian state codes
#'   (e.g., `"RN"`, `c("SP", "RJ", "MG")`). Case insensitive. If `NULL`
#'   (default), all 27 states are returned.
#'
#' @param network Character vector of monitoring network(s) to include.
#'   One or more of: `"Hidroweb"`, `"INMET"`, `"ICEA"`, `"CEMADEN"`,
#'   `"Telemetria"`. Case insensitive. If `NULL` (default), all networks
#'   are returned.
#'
#' @param aggregate_to Character. Temporal resolution for aggregating sub-daily
#'   observations into totals. One of:
#'   \itemize{
#'     \item `"day"` (default): Daily rainfall totals (`sum(rain_mm)`)
#'     \item `"month"`: Monthly rainfall totals
#'     \item `"year"`: Annual rainfall totals
#'     \item `"none"`: Return raw observations at original resolution
#'   }
#'   When `"none"`, `rainfall_mm` reflects the raw measurement interval
#'   (10 min to 1440 min depending on network). Use `"day"` for compatibility
#'   with `sus_climate_aggregate()`.
#'
#' @param use_cache Logical. If `TRUE` (default), skips download when the two
#'   Parquet files already exist in `cache_dir/uniplu/`. The full ZIP (~1.6 GB)
#'   is only downloaded once. Use `use_cache = FALSE` to force a re-download
#'   (e.g., after a new dataset version is published on Zenodo).
#'
#' @param cache_dir Character. Directory path for the disk cache.
#'   Default: `"~/.climasus4r_cache/climate"`. Created automatically if absent.
#'   Use `unlink(file.path(cache_dir, "uniplu"), recursive = TRUE)` to clear
#'   the UNIPLU cache.
#'
#' @param lang Character. Message language. One of:
#'   \itemize{
#'     \item `"pt"`: Portuguese (default)
#'     \item `"en"`: English
#'     \item `"es"`: Spanish
#'   }
#'
#' @param verbose Logical. If `TRUE` (default), prints progress messages
#'   including cache status, download progress, filtering counts, and
#'   aggregation summary.
#'
#' @return
#' A `climasus_df` object (subclass of `tibble`) at `stage = "climate"`,
#' `type = "uniplu"`. Columns:
#'
#' \describe{
#'   \item{`station_code`}{Character. Unique gauge identifier (from `gauge_code`)}
#'   \item{`station_name`}{Character. City name of the gauge (from `city`)}
#'   \item{`uf`}{Character. State abbreviation (from `state`)}
#'   \item{`latitude`}{Numeric. Decimal latitude, WGS84 (from `lat`)}
#'   \item{`longitude`}{Numeric. Decimal longitude, WGS84 (from `long`)}
#'   \item{`altitude`}{Numeric. Elevation in metres above sea level (from `elevation`)}
#'   \item{`network`}{Character. Source monitoring network}
#'   \item{`time_step_min`}{Integer. Original temporal resolution in minutes
#'     (e.g., 10, 60, 1440). Only present when `aggregate_to = "none"`.}
#'   \item{`utc_offset`}{Numeric. Timezone offset from UTC (from `utc`).
#'     Only present when `aggregate_to = "none"`.}
#'   \item{`date`}{POSIXct or Date. Observation timestamp / aggregation period}
#'   \item{`rainfall_mm`}{Numeric. Precipitation in millimetres
#'     (sum over aggregation period, or raw value when `aggregate_to = "none"`)}
#' }
#'
#' **Metadata** (accessible via `sus_meta()`):
#' \describe{
#'   \item{`stage`}{"climate"}
#'   \item{`type`}{"uniplu"}
#'   \item{`source`}{"UNIPLU-BR"}
#'   \item{`doi`}{"10.5281/zenodo.18883358"}
#'   \item{`years`}{Years retained after filtering}
#'   \item{`ufs`}{States retained after filtering}
#'   \item{`networks`}{Networks retained after filtering}
#'   \item{`n_stations`}{Number of unique gauges}
#'   \item{`n_observations`}{Total rows}
#'   \item{`temporal`}{List with `start`, `end`, `unit` (aggregate_to)}
#' }
#'
#' @section Monitoring Networks:
#'
#' | **Network** | **Operator** | **Period** | **Resolution** |
#' |-------------|-------------|----------|--------------|
#' | Hidroweb | ANA | 1885–2025 | Daily (1440 min) |
#' | INMET daily | INMET | 1889–2025 | Daily (1440 min) |
#' | ICEA | ICEA | 1951–2025 | Various |
#' | CEMADEN | CEMADEN | 2014–2025 | 10–15 min |
#' | Telemetria | Various | 2014–2025 | 10–60 min |
#' | INMET sub-daily | INMET | 2000–2025 | 60 min |
#'
#' @note
#' \itemize{
#'   \item **Data quality**: The dataset is strictly structurally standardized.
#'     **No outlier removal or physical consistency checks were applied** by the
#'     data producers. Extreme or implausible values from source agencies remain.
#'     Consider applying your own quality filters for scientific analyses.
#'   \item **Download size**: ~1.6 GB compressed. Only performed once; subsequent
#'     calls read from cached Parquet files (typically a few seconds).
#'   \item **Rainfall only**: UNIPLU-BR contains precipitation exclusively.
#'     For multi-variable climate data (temperature, humidity, radiation), use
#'     [sus_climate_inmet()].
#'   \item **License**: CC-BY-4.0. Cite the dataset and paper when publishing.
#' }
#'
#' @references
#' **Dataset**:
#' Das Neves Almeida, C., Bertrand, G. F., Lemos, F. C., et al. (2026).
#' *Unified Brazilian Rainfall Dataset (UNIPLU-BR): A Standardized National
#' Database of Point Precipitation from Major Brazilian Monitoring Networks
#' (1885–2025)*. Zenodo. \doi{10.5281/zenodo.18883358}
#'
#' **Related paper**:
#' Das Neves Almeida, C., Bertrand, G. F., Lemos, F. C., et al. (2025).
#' The design of the Brazilian Sub-Daily Rainfall dataset (BR-SDR): two decades
#' of high-time-resolution data in Brazil.
#' *Hydrological Sciences Journal*, 70(11), 1850–1862.
#' \doi{10.1080/02626667.2025.2506193}
#'
#' **GitHub repository**: \url{https://github.com/LARHENA/UNIPLU-BR}
#'
#' @seealso
#' * [sus_climate_inmet()] for multi-variable meteorological data (INMET)
#' * [sus_climate_aggregate()] for health-climate temporal integration
#' * [sus_climate_fill_inmet()] for ML-based gap filling
#'
#' @examples
#' \dontrun{
#' # Daily rainfall for Rio Grande do Norte, last 2 years (default)
#' rain_rn <- sus_climate_uniplu(uf = "RN")
#'
#' # Multiple states, specific years, daily totals
#' rain_ne <- sus_climate_uniplu(
#'   years = 2015:2024,
#'   uf    = c("RN", "CE", "PB", "PE"),
#'   aggregate_to = "day"
#' )
#'
#' # Only ANA/Hidroweb network, monthly totals
#' rain_hidroweb <- sus_climate_uniplu(
#'   years   = 2000:2020,
#'   network = "Hidroweb",
#'   aggregate_to = "month"
#' )
#'
#' # Raw sub-daily data (no aggregation)
#' rain_raw <- sus_climate_uniplu(
#'   years        = 2023,
#'   uf           = "SP",
#'   network      = "CEMADEN",
#'   aggregate_to = "none"
#' )
#'
#' # Check metadata
#' sus_meta(rain_rn)
#'
#' # Inspect stations
#' dplyr::distinct(rain_rn, station_code, station_name, latitude, longitude)
#'
#' # Integrate with health data (requires prior sus_spatial_join())
#' health_climate <- sus_climate_aggregate(
#'   health_data  = health_spatial,
#'   climate_data = rain_rn,
#'   climate_var  = "rainfall_mm",
#'   temporal_strategy = "moving_window",
#'   window_days  = 14
#' )
#' }
#'
#' @export
#' @importFrom dplyr filter mutate select rename left_join summarise n_distinct
#' @importFrom dplyr collect as_tibble
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom glue glue
#' @importFrom rlang .data
sus_climate_uniplu <- function(
    years        = NULL,
    uf           = NULL,
    network      = NULL,
    aggregate_to = "day",
    use_cache    = TRUE,
    cache_dir    = "~/.climasus4r_cache/climate",
    lang         = "pt",
    verbose      = TRUE) {

  # ============================================================================
  # VALIDATION
  # ============================================================================

  # --- years ------------------------------------------------------------------
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  if (!is.null(years)) {
    if (!is.numeric(years) || any(is.na(years))) {
      cli::cli_abort("{.arg years} must be a numeric vector without NA values.")
    }
    years <- as.integer(years)
    invalid_years <- years[years < 1885L | years > current_year]
    if (length(invalid_years) > 0) {
      cli::cli_abort(
        c(
          "Invalid values in {.arg years}.",
          "x" = "Years must be between 1885 and {current_year}.",
          "i" = "Invalid: {paste(invalid_years, collapse = ', ')}"
        )
      )
    }
  } else {
    years <- (current_year - 1L):current_year
    if (verbose)
      cli::cli_alert_info(
        "{.arg years} not supplied \u2013 defaulting to {paste(years, collapse = '-')}."
      )
  }

  # --- lang -------------------------------------------------------------------
  if (!is.character(lang) || length(lang) != 1L || !lang %in% c("pt", "en", "es")) {
    cli::cli_abort("{.arg lang} must be one of 'pt', 'en', or 'es'.")
  }

  # --- uf ---------------------------------------------------------------------
  valid_ufs <- c(
    "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
    "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
    "RS", "RO", "RR", "SC", "SP", "SE", "TO"
  )
  if (!is.null(uf)) {
    if (!is.character(uf) || any(is.na(uf)) || any(nchar(trimws(uf)) == 0L)) {
      cli::cli_abort("{.arg uf} must be a character vector of non-empty, non-NA state codes.")
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

  # --- network ----------------------------------------------------------------
  valid_networks <- c("Hidroweb", "INMET", "ICEA", "CEMADEN", "Telemetria")
  if (!is.null(network)) {
    if (!is.character(network) || any(is.na(network))) {
      cli::cli_abort("{.arg network} must be a character vector.")
    }
    network_canon <- valid_networks[
      match(tolower(trimws(network)), tolower(valid_networks))
    ]
    na_nets <- network[is.na(network_canon)]
    if (length(na_nets) > 0) {
      cli::cli_abort(
        c(
          "Invalid {.arg network} value(s): {paste(na_nets, collapse = ', ')}.",
          "i" = "Valid options: {paste(valid_networks, collapse = ', ')}"
        )
      )
    }
    network <- network_canon
  }

  # --- aggregate_to -----------------------------------------------------------
  valid_agg <- c("none", "day", "month", "year")
  if (!is.character(aggregate_to) || length(aggregate_to) != 1L ||
      !aggregate_to %in% valid_agg) {
    cli::cli_abort(
      "{.arg aggregate_to} must be one of: {paste(valid_agg, collapse = ', ')}."
    )
  }

  # --- use_cache / cache_dir --------------------------------------------------
  if (!is.logical(use_cache) || length(use_cache) != 1L || is.na(use_cache)) {
    cli::cli_abort("{.arg use_cache} must be a single logical value (TRUE or FALSE).")
  }
  if (!is.character(cache_dir) || length(cache_dir) != 1L ||
      nchar(trimws(cache_dir)) == 0L) {
    cli::cli_abort("{.arg cache_dir} must be a single non-empty character string.")
  }

  # ============================================================================
  # MESSAGES
  # ============================================================================
  msg <- list(
    pt = list(
      cache_config   = "Cache em: {dir}",
      cache_hit      = "Dados UNIPLU-BR encontrados no cache. Lendo arquivos Parquet...",
      cache_miss     = "Cache n\u00E3o encontrado. Baixando UNIPLU-BR do Zenodo (~1,6 GB)...",
      download_start = "Download iniciado. Isso pode levar v\u00E1rios minutos.",
      download_done  = "Download conclu\u00EDdo. Extraindo arquivos Parquet...",
      extract_done   = "Arquivos extra\u00EDdos e salvos em cache: {dir}",
      filter_year    = "Filtrando por {length(years)} ano{?s}: {paste(years, collapse = ', ')}...",
      filter_uf      = "Filtrando por UF: {paste(uf, collapse = ', ')}...",
      filter_net     = "Filtrando por rede: {paste(network, collapse = ', ')}...",
      agg_start      = "Agregando para resolu\u00E7\u00E3o: {agg}...",
      import_done    = "Conclu\u00EDdo: {n_rows} observa\u00E7\u00F5es de {n_stations} esta\u00E7\u00F5es"
    ),
    en = list(
      cache_config   = "Cache directory: {dir}",
      cache_hit      = "UNIPLU-BR found in cache. Reading Parquet files...",
      cache_miss     = "Cache not found. Downloading UNIPLU-BR from Zenodo (~1.6 GB)...",
      download_start = "Download started. This may take several minutes.",
      download_done  = "Download complete. Extracting Parquet files...",
      extract_done   = "Files extracted and cached at: {dir}",
      filter_year    = "Filtering by {length(years)} year{?s}: {paste(years, collapse = ', ')}...",
      filter_uf      = "Filtering by UF: {paste(uf, collapse = ', ')}...",
      filter_net     = "Filtering by network: {paste(network, collapse = ', ')}...",
      agg_start      = "Aggregating to resolution: {agg}...",
      import_done    = "Done: {n_rows} observations from {n_stations} stations"
    ),
    es = list(
      cache_config   = "Directorio de cach\u00E9: {dir}",
      cache_hit      = "UNIPLU-BR encontrado en cach\u00E9. Leyendo archivos Parquet...",
      cache_miss     = "Cach\u00E9 no encontrado. Descargando UNIPLU-BR de Zenodo (~1,6 GB)...",
      download_start = "Descarga iniciada. Esto puede tardar varios minutos.",
      download_done  = "Descarga completada. Extrayendo archivos Parquet...",
      extract_done   = "Archivos extra\u00EDdos y guardados en cach\u00E9: {dir}",
      filter_year    = "Filtrando por {length(years)} a\u00F1o{?s}: {paste(years, collapse = ', ')}...",
      filter_uf      = "Filtrando por UF: {paste(uf, collapse = ', ')}...",
      filter_net     = "Filtrando por red: {paste(network, collapse = ', ')}...",
      agg_start      = "Agregando a la resoluci\u00F3n: {agg}...",
      import_done    = "Completado: {n_rows} observaciones de {n_stations} estaciones"
    )
  )[[lang]]

  # ============================================================================
  # CACHE SETUP
  # ============================================================================
  cache_dir    <- path.expand(cache_dir)
  uniplu_dir   <- file.path(cache_dir, "uniplu")
  info_parquet <- file.path(uniplu_dir, "table_info.parquet")
  data_parquet <- file.path(uniplu_dir, "table_data.parquet")

  if (!dir.exists(uniplu_dir))
    dir.create(uniplu_dir, recursive = TRUE, showWarnings = FALSE)

  if (verbose)
    cli::cli_alert_info(glue::glue(msg$cache_config, dir = uniplu_dir))

  # ============================================================================
  # DOWNLOAD + EXTRACT (if cache missing or bypassed)
  # ============================================================================
  cache_valid <- use_cache &&
    file.exists(info_parquet) && file.size(info_parquet) > 0L &&
    file.exists(data_parquet) && file.size(data_parquet) > 0L

  if (!cache_valid) {
    if (verbose) {
      cli::cli_alert_info(msg$cache_miss)
      cli::cli_alert_info(msg$download_start)
    }

    zip_url  <- paste0(
      "https://zenodo.org/records/18883358/files/",
      "Brazilian%20Unified%20Rainfall%20Dataset%20(1885%20-%202025).zip",
      "?download=1"
    )
    zip_dest <- tempfile(fileext = ".zip")
    on.exit(if (file.exists(zip_dest)) unlink(zip_dest), add = TRUE)

    old_timeout <- getOption("timeout")
    options(timeout = max(7200L, old_timeout))
    on.exit(options(timeout = old_timeout), add = TRUE)

    dl_ok <- .uniplu_download_robust(zip_url, zip_dest, verbose = verbose)
    if (!isTRUE(dl_ok)) {
      cli::cli_abort(
        c(
          "Failed to download UNIPLU-BR from Zenodo.",
          "x" = if (is.character(dl_ok)) dl_ok else "All download methods failed.",
          "i" = "Check your internet connection and try again.",
          "i" = "URL: {zip_url}"
        )
      )
    }

    if (verbose) cli::cli_alert_info(msg$download_done)

    # Extract the two parquet files
    tmp_dir <- tempfile()
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    tryCatch(
      utils::unzip(zip_dest, exdir = tmp_dir),
      error = function(e) {
        cli::cli_abort(
          c(
            "Failed to extract ZIP archive.",
            "x" = conditionMessage(e),
            "i" = "The downloaded file may be corrupt. Try again with {.code use_cache = FALSE}."
          )
        )
      }
    )

    # Find parquet files (may be nested in subdirectory)
    found_info <- list.files(tmp_dir, pattern = "table_info\\.parquet$",
                             recursive = TRUE, full.names = TRUE)
    found_data <- list.files(tmp_dir, pattern = "table_data\\.parquet$",
                             recursive = TRUE, full.names = TRUE)

    if (length(found_info) == 0L || length(found_data) == 0L) {
      cli::cli_abort(
        c(
          "Expected Parquet files not found in the ZIP archive.",
          "i" = "Files found: {paste(list.files(tmp_dir, recursive = TRUE), collapse = ', ')}",
          "i" = "The Zenodo archive may have changed structure. Please report this at",
          "i" = "https://github.com/ByMaxAnjos/climasus4r/issues"
        )
      )
    }

    file.copy(found_info[1L], info_parquet, overwrite = TRUE)
    file.copy(found_data[1L], data_parquet, overwrite = TRUE)

    if (verbose)
      cli::cli_alert_success(glue::glue(msg$extract_done, dir = uniplu_dir))

  } else {
    if (verbose) cli::cli_alert_info(msg$cache_hit)
  }

  # ============================================================================
  # READ PARQUET FILES
  # ============================================================================
  rlang::check_installed("arrow", reason = "to read UNIPLU-BR Parquet files")

  tbl_info <- arrow::read_parquet(info_parquet) |> dplyr::collect()
  tbl_data <- arrow::read_parquet(data_parquet) |> dplyr::collect()

  # ============================================================================
  # JOIN + STANDARDIZE COLUMNS
  # ============================================================================
  result <- tbl_data |>
    dplyr::left_join(tbl_info, by = "gauge_code") |>
    dplyr::rename(
      station_code  = "gauge_code",
      station_name  = "city",
      uf            = "state",
      latitude      = "lat",
      longitude     = "long",
      altitude      = "elevation",
      time_step_min = "time_step",
      utc_offset    = "utc",
      date          = "datetime",
      rainfall_mm   = "rain_mm"
    ) |>
    dplyr::mutate(
      date        = as.POSIXct(.data$date),
      station_code = as.character(.data$station_code),
      uf           = toupper(trimws(.data$uf))
    )

  # ============================================================================
  # FILTER: year
  # ============================================================================
  if (verbose)
    cli::cli_alert_info(glue::glue(msg$filter_year))

  result <- result |>
    dplyr::filter(as.integer(format(.data$date, "%Y")) %in% years)

  if (nrow(result) == 0L) {
    cli::cli_abort(
      c(
        "No observations found for years: {paste(years, collapse = ', ')}.",
        "i" = "The UNIPLU-BR dataset covers 1885\u20132025."
      )
    )
  }

  # ============================================================================
  # FILTER: UF
  # ============================================================================
  if (!is.null(uf)) {
    if (verbose)
      cli::cli_alert_info(glue::glue(msg$filter_uf))
    result <- result |>
      dplyr::filter(.data$uf %in% uf)
    if (nrow(result) == 0L) {
      cli::cli_abort(
        "No observations found for UF: {paste(uf, collapse = ', ')}."
      )
    }
  }

  # ============================================================================
  # FILTER: network
  # ============================================================================
  if (!is.null(network)) {
    if (verbose)
      cli::cli_alert_info(glue::glue(msg$filter_net))
    result <- result |>
      dplyr::filter(.data$network %in% network)
    if (nrow(result) == 0L) {
      cli::cli_abort(
        "No observations found for network: {paste(network, collapse = ', ')}."
      )
    }
  }

  # ============================================================================
  # TEMPORAL AGGREGATION
  # ============================================================================
  if (aggregate_to != "none") {
    if (verbose)
      cli::cli_alert_info(glue::glue(msg$agg_start, agg = aggregate_to))

    # Keep only columns meaningful after aggregation (drop time_step_min, utc_offset)
    group_cols <- c("station_code", "station_name", "uf",
                    "latitude", "longitude", "altitude", "network")

    result <- switch(
      aggregate_to,
      "day" = {
        result |>
          dplyr::mutate(date = as.Date(.data$date)) |>
          dplyr::summarise(
            rainfall_mm = sum(.data$rainfall_mm, na.rm = TRUE),
            .by         = c(group_cols, "date")
          ) |>
          dplyr::mutate(date = as.POSIXct(.data$date))
      },
      "month" = {
        result |>
          dplyr::mutate(
            year  = as.integer(format(.data$date, "%Y")),
            month = as.integer(format(.data$date, "%m")),
            date  = as.Date(paste(.data$year, .data$month, "01", sep = "-"))
          ) |>
          dplyr::summarise(
            rainfall_mm = sum(.data$rainfall_mm, na.rm = TRUE),
            .by         = c(group_cols, "date")
          ) |>
          dplyr::mutate(date = as.POSIXct(.data$date))
      },
      "year" = {
        result |>
          dplyr::mutate(
            date = as.Date(paste(format(.data$date, "%Y"), "01", "01", sep = "-"))
          ) |>
          dplyr::summarise(
            rainfall_mm = sum(.data$rainfall_mm, na.rm = TRUE),
            .by         = c(group_cols, "date")
          ) |>
          dplyr::mutate(date = as.POSIXct(.data$date))
      }
    )
  }

  result <- dplyr::arrange(result, .data$station_code, .data$date)

  # ============================================================================
  # S3 CLASS + METADATA
  # ============================================================================
  n_stations_final <- dplyr::n_distinct(result$station_code)
  date_range       <- range(result$date, na.rm = TRUE)

  meta <- list(
    system         = NULL,
    stage          = "climate",
    type           = "uniplu",
    spatial        = FALSE,
    temporal       = list(
      start = date_range[1L],
      end   = date_range[2L],
      unit  = aggregate_to
    ),
    created        = Sys.time(),
    modified       = Sys.time(),
    source         = "UNIPLU-BR",
    doi            = "10.5281/zenodo.18883358",
    years          = sort(unique(as.integer(format(result$date, "%Y")))),
    ufs            = uf,
    networks       = network,
    n_stations     = n_stations_final,
    n_observations = nrow(result),
    aggregate_to   = aggregate_to,
    history        = sprintf(
      "[%s] UNIPLU-BR rainfall data imported (aggregate_to = '%s')",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      aggregate_to
    ),
    user = list()
  )

  base_classes <- setdiff(class(result), "climasus_df")
  result <- structure(
    result,
    sus_meta = meta,
    class    = c("climasus_df", base_classes)
  )

  if (verbose) {
    cli::cli_alert_success(
      glue::glue(
        msg$import_done,
        n_rows     = nrow(result),
        n_stations = n_stations_final
      )
    )
  }

  result
}

# ==============================================================================
# INTERNAL: .uniplu_download_robust()
# ==============================================================================

#' Robust download helper for UNIPLU-BR ZIP
#'
#' Tries httr2, httr, system curl, and system wget in order.
#' Returns TRUE on success, a character reason string on permanent HTTP error,
#' FALSE if all methods fail.
#'
#' @keywords internal
#' @noRd
.uniplu_download_robust <- function(url, dest, max_retries = 3L, verbose = FALSE) {

  ua <- paste0(
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
    "AppleWebKit/537.36 (KHTML, like Gecko) ",
    "Chrome/124.0.0.0 Safari/537.36"
  )

  tmp        <- paste0(dest, ".tmp")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)

  try_httr2 <- function() {
    if (!requireNamespace("httr2", quietly = TRUE)) return(FALSE)
    tryCatch({
      req <- httr2::request(url) |>
        httr2::req_headers(
          "User-Agent" = ua,
          "Accept"     = "application/zip, application/octet-stream, */*"
        ) |>
        httr2::req_timeout(7200) |>
        httr2::req_retry(
          max_tries    = max_retries,
          backoff      = ~ 2 ^ .x,
          is_transient = \(resp) httr2::resp_status(resp) %in%
            c(429L, 500L, 502L, 503L, 504L)
        )
      resp   <- httr2::req_perform(req, path = tmp)
      status <- httr2::resp_status(resp)
      if (status == 200L && file.exists(tmp) && file.size(tmp) > 0L) {
        file.rename(tmp, dest)
        return(TRUE)
      }
      if (file.exists(tmp)) unlink(tmp)
      if (status >= 400L && status < 500L)
        return(sprintf("HTTP %d from server (permanent error)", status))
      FALSE
    }, error = function(e) {
      if (file.exists(tmp)) unlink(tmp)
      if (verbose) cli::cli_alert_warning("httr2: {conditionMessage(e)}")
      FALSE
    })
  }

  try_httr <- function() {
    if (!requireNamespace("httr", quietly = TRUE)) return(FALSE)
    tryCatch({
      resp <- httr::GET(
        url,
        httr::add_headers("User-Agent" = ua,
                          "Accept"     = "application/zip, application/octet-stream, */*"),
        httr::timeout(7200),
        httr::write_disk(tmp, overwrite = TRUE)
      )
      sc <- httr::status_code(resp)
      if (sc == 200L && file.exists(tmp) && file.size(tmp) > 0L) {
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
    if (nchar(curl_bin) == 0L) return(FALSE)
    status_file <- paste0(tmp, ".status")
    on.exit(if (file.exists(status_file)) unlink(status_file), add = TRUE)
    tryCatch({
      ret <- system2(
        curl_bin,
        args   = c(
          "--silent", "--show-error", "--location",
          "--max-time", "7200",
          "--write-out", shQuote("%{http_code}"),
          "--user-agent", shQuote(ua),
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
          file.exists(tmp) && file.size(tmp) > 0L) {
        file.rename(tmp, dest)
        return(TRUE)
      }
      if (file.exists(tmp)) unlink(tmp)
      if (!is.na(http_code) && http_code >= 400L && http_code < 500L)
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
    if (nchar(wget_bin) == 0L) return(FALSE)
    tryCatch({
      ret <- system2(
        wget_bin,
        args   = c(
          "--server-response", "--timeout=7200", "--tries=1",
          paste0("--user-agent=", shQuote(ua)),
          "--output-document", shQuote(tmp),
          shQuote(url)
        ),
        stdout = FALSE,
        stderr = if (verbose) TRUE else FALSE
      )
      if (ret == 0L && file.exists(tmp) && file.size(tmp) > 0L) {
        file.rename(tmp, dest)
        return(TRUE)
      }
      if (file.exists(tmp)) unlink(tmp)
      FALSE
    }, error = function(e) {
      if (file.exists(tmp)) unlink(tmp)
      if (verbose) cli::cli_alert_warning("wget binary: {conditionMessage(e)}")
      FALSE
    })
  }

  for (attempt in seq_len(max_retries)) {
    result <- try_httr2()
    if (isTRUE(result)) return(TRUE)
    if (is.character(result)) return(result)

    result <- try_httr()
    if (isTRUE(result)) return(TRUE)
    if (is.character(result)) return(result)

    result <- try_curl_bin()
    if (isTRUE(result)) return(TRUE)
    if (is.character(result)) return(result)

    result <- try_wget_bin()
    if (isTRUE(result)) return(TRUE)
    if (is.character(result)) return(result)

    if (attempt < max_retries) {
      wait <- 2 ^ attempt
      if (verbose)
        cli::cli_alert_warning(
          "All download methods failed (attempt {attempt}/{max_retries}). Retrying in {wait}s..."
        )
      Sys.sleep(wait)
    }
  }

  FALSE
}
