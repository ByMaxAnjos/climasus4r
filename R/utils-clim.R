#' Utilities functions for climasus4r - climate integration
#'
#' @keywords internal
#' @name utils-clim
#' @noRd
NULL

# Global variable declarations for R CMD check
utils::globalVariables(c(
  ".",                                    # dplyr pronoun
  ".N", ".win_start", ".win_end",        # data.table
  "rle_id",                              # internal variable
  "rainfall_mm", "ws_2_m_s",             # meteorological
  "code_muni",                           # municipality
  "station_code", "station_id",          # station identifiers
  "latitude", "longitude", "altitude",   # spatial
  "UF", "station_name", "year",          # metadata
  "region", "datetime",                  # temporal/spatial
  "tair_min_c", "tair_dry_bulb_c", "tair_max_c",  # temperature
  "dew_tmean_c", "dew_tmax_c", "dew_tmin_c",      # dew point
  "rh_min_porc", "rh_mean_porc", "rh_max_porc",   # humidity
  "patm_min_mb", "patm_mb", "patm_max_mb",        # pressure
  "sr_kj_m2", "wd_degrees", "ws_gust_m_s",        # radiation/wind
  "dew_calc", "dew_diff",                         # derived
  "time_diff", "climate_var",                     # temporal
  "n_obs", "prop_obs", "value",                   # aggregation
  "value_window", "n_valid", "n_days",            # window stats
  "prop_valid", "missing_pct",                    # quality
  "passes_quality", "station_index",              # filtering
  "distance_m", "station_unique_name",            # spatial matching
  "target_vars",
   "dayofyear", "hour", "quarter", "season", "wday_tmp", "weekday", "weekofyear",
   "month", ".join_key", ".hour", ".is_night"                                # target variables
))
# ============================================================================
# INMET HELPER FUNCTIONS
# ============================================================================

#' Helper Function: Expand Year Ranges
#'
#' @description
#' Converts year specifications (single year, vector, or range) to a numeric vector.
#' Examples: 2022 -> c(2022), c(2022, 2024) -> c(2022, 2024), 2022:2025 -> c(2022, 2023, 2024, 2025)
#'
#' @param years Numeric. Year(s) to process.
#'
#' @return Numeric vector of years.
#'
#' @keywords internal
#' @noRd
.expand_year_range <- function(years) {
  if (is.null(years)) {
    return(NULL)
  }

  # Handle single year
  if (length(years) == 1) {
    return(as.integer(years))
  }

  # Handle vector or range
  years_expanded <- as.integer(years)

  # Check if it's a continuous range (e.g., 2022:2025)
  if (length(years_expanded) > 1) {
    # If all consecutive, it's already expanded
    return(sort(unique(years_expanded)))
  }

  return(years_expanded)
}

#' Process INMET Data for a Single Year
#'
#' @description
#' Downloads, parses, and caches INMET meteorological data for a specific year.
#' Handles internal parallelization of CSV file parsing.
#'
#' @param year Integer. Single year to process.
#' @param uf Character vector. State codes to filter (optional).
#' @param cache_dir Character. Directory for caching.
#' @param use_cache Logical. Whether to use cached data.
#' @param parallel Logical. Whether to use parallel processing for CSV files.
#' @param workers Integer. Number of workers for parallel processing.
#' @param verbose Logical. Whether to print progress messages.
#'
#' @return Data frame with standardized meteorological data.
#'
#' @keywords internal
#' @noRd
.process_inmet_year_with_cache <- function(
    year,
    uf = NULL,
    cache_dir,
    use_cache = use_cache,
    parallel = parallel,
    workers = workers,
    verbose = verbose)
{

  dataset_dir <- file.path(cache_dir, "inmet_parquet")

  if (use_cache && dir.exists(dataset_dir) && requireNamespace("arrow", quietly = TRUE)) {
    if (verbose) {
      cli::cli_alert_info("Using cached Arrow dataset for year {year}")
    }
    tryCatch(
      {
        year <- as.integer(year)
        ds <- arrow::open_dataset(dataset_dir) %>%
          dplyr::filter(.data$year == as.integer(year))

        if (!is.null(uf) && !is.na(uf) && uf != "") {
          uf <- base::toupper(uf)
          ds <- ds %>% dplyr::filter(.data$UF %in% uf)
        }

        result <- dplyr::collect(ds)

        if (nrow(result) > 0) {
          if (verbose) {
            cli::cli_alert_success("Loaded {nrow(result)} rows from cache")
          }
          return(result)
        } else {
          if (verbose) {
            cli::cli_alert_warning(
              "Cache is empty for year {year}, will re-download"
            )
          }
        }
      },
      error = function(e) {
        if (verbose) cli::cli_alert_warning("Error reading cache: {e$message}")
      }
    )
  } else {
    # ---- 2. Download / unzip ----
    zip_file <- file.path(cache_dir, paste0("inmet_", year, ".zip"))

    if (!file.exists(zip_file)) {
      if (verbose) {
        cli::cli_alert_info("Downloading INMET data for {year}")
      }
      options(timeout = 600)
      url <- paste0(
        "https://portal.inmet.gov.br/uploads/dadoshistoricos/",
        year,
        ".zip"
      )
      try(
        utils::download.file(
          url,
          zip_file,
          method = "libcurl",
          mode = "wb",
          cacheOK = FALSE,
          headers = c(
            "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
          ),
          quiet = TRUE
        ),
        silent = TRUE
      )
    }

    if (!file.exists(zip_file)) {
      if (verbose) {
        cli::cli_alert_warning("Failed to download data for year {year}")
      }
      return(data.frame())
    }

    temp_dir <- file.path(tempdir(), paste0("inmet_", year))
    unlink(temp_dir, recursive = TRUE)
    utils::unzip(zip_file, exdir = temp_dir)

    files <- list.files(
      temp_dir,
      full.names = TRUE,
      pattern = "\\.CSV$|\\.csv$"
    )

    if (length(files) == 0) {
      if (verbose) {
        cli::cli_alert_warning("No CSV files found in {year} archive")
      }
      return(data.frame())
    }

    if (verbose) {
      cli::cli_alert_info("Parsing {length(files)} CSV files")
    }

    # ---- 3. Parse files (with optional internal parallelization) ----
    if (
      parallel && length(files) > 1 && requireNamespace("furrr", quietly = TRUE)
    ) {
      if (verbose) {
        cli::cli_alert_info("Using parallel processing ({workers} workers)")
      }

      # Setup parallel backend
      old_plan <- future::plan()
      on.exit(future::plan(old_plan), add = TRUE)
      future::plan(future::multisession, workers = workers)

      year_data <- furrr::future_map_dfr(
        files,
        .parse_inmet_csv,
        .progress = verbose
      )
    } else {
      # Sequential parsing
      year_data <- purrr::map_dfr(
        files,
        .parse_inmet_csv
      )
    }

    # ---- 4. Save partitioned dataset ----
    if (nrow(year_data) > 0 && use_cache && requireNamespace("arrow", quietly = TRUE)) {
      if (verbose) {
        cli::cli_alert_info("Caching data to Arrow Parquet format")
      }

      # Ensure parquet directory exists
      if (!dir.exists(dataset_dir)) {
        dir.create(dataset_dir, recursive = TRUE, showWarnings = FALSE)
      }

      tryCatch(
        {
          arrow::write_dataset(
            year_data,
            path = dataset_dir,
            format = "parquet",
            partitioning = c("year", "UF")
          )
          if (verbose) cli::cli_alert_success("Cached successfully")
        },
        error = function(e) {
          if (verbose) cli::cli_alert_warning("Failed to cache: {e$message}")
        }
      )
    }
    unlink(temp_dir, recursive = TRUE)
    return(dplyr::as_tibble(year_data))
  }
}


#' Download and Cache INMET Data for Multiple Years
#'
#' @description
#' Main download function that handles multiple years, UF filtering, and parallelization.
#' Supports year ranges (e.g., 2022:2025) and individual years.
#'
#' @param years Numeric. Year(s) to download. Can be single year, vector, or range.
#' @param uf Character vector. State codes to filter (optional).
#' @param cache_dir Character. Directory for caching.
#' @param use_cache Logical. Whether to use cached data.
#' @param parallel Logical. Whether to use parallel processing.
#' @param workers Integer. Number of workers for parallel processing.
#' @param verbose Logical. Whether to print progress messages.
#' @param lang Character. Language for messages ("pt", "en", or "es").
#'
#' @return Data frame with combined meteorological data from all years.
#'
#' @keywords internal
#' @noRd
.download_and_cache_inmet <- function(
    years,
    uf = uf,
    cache_dir,
    use_cache = use_cache,
    parallel = parallel,
    workers = workers,
    verbose = verbose,
    lang = "en") 
    {
  
  # ---- 1. Validate and expand years ----
  years_expanded <- .expand_year_range(years)

  if (parallel && length(years_expanded) > 1) {
    # Parallelize between years
    if (!requireNamespace("future", quietly = TRUE)) {
      cli::cli_alert_warning("Package 'future' not installed. Falling back to sequential processing.")
      parallel <- FALSE
    } else {
      if (workers > parallel::detectCores()) {
        cli::cli_alert_warning(
          "{workers} workers requested but only {parallel::detectCores()} cores available. Using {parallel::detectCores() - 1}."
        )
        workers <- parallel::detectCores() - 1
      }

      if (verbose) cli::cli_alert_info("Parallel processing: {length(years_expanded)} years with {workers} workers")
    }
  } else if (parallel && length(years_expanded) == 1) {
    # For single year, parallelization happens internally in .process_inmet_year_with_cache()
    if (verbose) cli::cli_alert_info("Single year: internal parallelization will be used for CSV files")
  }

  # ---- 3. Create parameter grid
  if (!is.null(uf)) {
    params <- expand.grid(
      year = years_expanded,
      uf = uf,
      stringsAsFactors = FALSE
    )
  } else {
    params <- data.frame(
      year = years_expanded,
      uf = rep(NA_character_, length(years_expanded)),
      stringsAsFactors = FALSE
    )
  }

  if (verbose) {
    cli::cli_alert_info("Processing {nrow(params)} parameter combination(s)")
  }

  # ---- 4. Download data (parallel or sequential) ----

  if (parallel && length(years_expanded) > 1) {
    # Parallel between years
    if (verbose) cli::cli_alert_info("Starting parallel downloads...")

    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = workers)

    list_of_dfs <- future.apply::future_mapply(
      FUN = .process_inmet_year_with_cache,
      year = params$year,
      uf = params$uf,
      MoreArgs = list(
        cache_dir = cache_dir,
        use_cache = use_cache,
        parallel = FALSE,  # No internal parallelization when external parallelization is active
        workers = 1,
        verbose = verbose
      ),
      SIMPLIFY = FALSE,
      future.seed = TRUE
    )
  } else {
    # Sequential processing
    if (verbose) cli::cli_alert_info("Starting sequential downloads...")

    list_of_dfs <- mapply(
      FUN = .process_inmet_year_with_cache,
      year = params$year,
      uf = params$uf,
      MoreArgs = list(
        cache_dir = cache_dir,
        use_cache = use_cache,
        parallel = parallel,  # Allow internal parallelization for single year
        workers = workers,
        verbose = verbose  # FIX #7: Propagate verbose
      ),
      SIMPLIFY = FALSE
    )
  }

  # ---- 5. Clean results ----
  # FIX #8: Better error handling
  # Remove NULL and empty data frames
  list_of_dfs <- list_of_dfs[!sapply(list_of_dfs, function(x) {
    is.null(x) || (is.data.frame(x) && nrow(x) == 0)
  })]

  if (length(list_of_dfs) == 0) {
    cli::cli_abort("No data was successfully downloaded.")  # FIX #9: Use direct string
  }

  # ---- 6. Combine all dataframes ----
  if (verbose) {
    cli::cli_alert_info("Combining {length(list_of_dfs)} datasets...")
  }

  climate_data <- dplyr::bind_rows(list_of_dfs)

  if (nrow(climate_data) == 0) {
    cli::cli_abort("No data rows in combined dataset.")  # FIX #9: Use direct string
  }
  detected_col <- names(climate_data)[sapply(climate_data, function(x) inherits(x, c("Date", "POSIXt")))][1]
  
  if (!is.na(detected_col) && detected_col != "date") {
    climate_data <- climate_data %>% dplyr::rename(date = !!rlang::sym(detected_col))
  }
  if (verbose) {
    cli::cli_alert_success("Successfully loaded {nrow(climate_data)} rows of climate data")
  }

  return(dplyr::as_tibble(climate_data))
}

#' Parse Individual INMET CSV File
#' @keywords internal
#' @noRd
.parse_inmet_csv <- function(file_path) {
  tryCatch(
    {
      # --- OMM ---
      
      omm <- data.table::fread(
        file_path,
        header = FALSE,
        nrows = 8,
        sep = ";",
        encoding = "Latin-1",
        showProgress = FALSE
      )
      
      # Extract metadata values
      region        <- omm[1, 2]$V2
      UF            <- omm[2, 2]$V2
      station_name  <- omm[3, 2]$V2
      station_code  <- omm[4, 2]$V2
      latitude      <- omm[5, 2]$V2
      longitude     <- omm[6, 2]$V2
      altitude      <- omm[7, 2]$V2
      
      latitude <- as.numeric(stringr::str_replace(latitude, ",", "."))
      longitude <- as.numeric(stringr::str_replace(longitude, ",", "."))
      altitude <- as.numeric(stringr::str_replace(altitude, ",", "."))
      
      # --- Dados ---
      dt <- data.table::fread(
        file_path,
        skip = 8,
        sep = ";",
        dec = ",",
        na.strings = c("-9999", ""),
        encoding = "Latin-1",
        fill = TRUE,
        showProgress = FALSE
      )
      
      names(dt) <- c("date", "hour", "rainfall_mm", 
                     "patm_mb", "patm_max_mb", "patm_min_mb", "sr_kj_m2", 
                     "tair_dry_bulb_c", "dew_tmean_c", "tair_max_c", 
                     "tair_min_c", "dew_tmax_c", "dew_tmin_c", 
                     "rh_max_porc", "rh_min_porc", "rh_mean_porc", 
                     "wd_degrees", "ws_gust_m_s", "ws_2_m_s", "v")
      
      dt <- dt %>% 
        dplyr::mutate(date= lubridate::ymd_hm(paste(.data$date, .data$hour))) %>% 
        dplyr::select(-dplyr::starts_with("v"), -.data$hour)
      
      dt_result <- .apply_quality_control(dt, verbose = FALSE)
      
      
      # --- Metadados ---
      dt_result <- dt_result %>%
        dplyr::mutate(
          region = region,
          UF = UF,
          station_code = station_code,
          station_name = station_name,
          year = lubridate::year(.data$date),
          latitude = latitude,
          longitude = longitude,
          altitude = altitude
        )
      
      dt_result[]
    },
    error = function(e) data.table::data.table()
  )
}

#' Apply quality control checks to meteorological data
#'
#' @param dt A data.table with meteorological data
#' @param verbose Logical, whether to print QC messages
#' @return The data.table with quality-controlled data
#'
#' @keywords internal
#' @noRd
.apply_quality_control <- function(dt, verbose = FALSE) {
  
  qc_stats <- list(
    total_rows = nrow(dt),
    removed = 0,
    modified = 0
  )
  
  if (verbose) {
    cli::cli_h2("Applying Quality Control")
    cli::cli_alert_info("Total rows: {qc_stats$total_rows}")
  }
    
  # Tmin <= Tmean <= Tmax
  if (all(c("tair_dry_bulb_c", "tair_min_c", "tair_max_c") %in% names(dt))) {
    invalid_rows <- dt[tair_min_c > tair_dry_bulb_c | tair_dry_bulb_c > tair_max_c, .N]
    if (invalid_rows > 0 && verbose) {
      cli::cli_alert_warning("Tmin > Tmean or Tmean > Tmax in {invalid_rows} rows")
    }
    
    dt[tair_min_c > tair_dry_bulb_c, tair_min_c := NA_real_]
    dt[tair_dry_bulb_c > tair_max_c, tair_max_c := NA_real_]
  }
  
  # Td <= T (temperatura de orvalho <= temperatura do ar)
  if (all(c("tair_dry_bulb_c", "dew_tmean_c") %in% names(dt))) {
    invalid_rows <- dt[dew_tmean_c > tair_dry_bulb_c, .N]
    if (invalid_rows > 0 && verbose) {
      cli::cli_alert_warning("Dew point > Air temperature in {invalid_rows} rows")
    }
    dt[dew_tmean_c > tair_dry_bulb_c, dew_tmean_c := NA_real_]
  }
  
  # RHmin <= RHmean <= RHmax
  if (all(c("rh_min_porc", "rh_mean_porc", "rh_max_porc") %in% names(dt))) {
    invalid_rows <- dt[rh_min_porc > rh_mean_porc | rh_mean_porc > rh_max_porc, .N]
    if (invalid_rows > 0 && verbose) {
      cli::cli_alert_warning("RHmin > RHmean or RHmean > RHmax in {invalid_rows} rows")
    }
    
    dt[rh_min_porc > rh_mean_porc, rh_min_porc := NA_real_]
    dt[rh_mean_porc > rh_max_porc, rh_max_porc := NA_real_]
  }
  
  # Pmin <= Pmean <= Pmax
  if (all(c("patm_min_mb", "patm_mb", "patm_max_mb") %in% names(dt))) {
    invalid_rows <- dt[patm_min_mb > patm_mb | patm_mb > patm_max_mb, .N]
    if (invalid_rows > 0 && verbose) {
      cli::cli_alert_warning("Pmin > Pmean or Pmean > Pmax in {invalid_rows} rows")
    }
    
    dt[patm_min_mb > patm_mb, patm_min_mb := NA_real_]
    dt[patm_mb > patm_max_mb, patm_max_mb := NA_real_]
  }
    
  temp_cols <- c("tair_dry_bulb_c", "tair_min_c", "tair_max_c",
                 "dew_tmean_c", "dew_tmax_c", "dew_tmin_c")
  
  for (col in intersect(temp_cols, names(dt))) {
    n_before <- dt[!is.na(get(col)), .N]
    dt[get(col) < -90 | get(col) > 60, (col) := NA_real_]
    n_after <- dt[!is.na(get(col)), .N]
    removed <- n_before - n_after
    if (removed > 0 && verbose) {
      cli::cli_alert_info("Removed {removed} invalid {col} values")
      qc_stats$modified <- qc_stats$modified + removed
    }
  }
  
  rh_cols <- c("rh_min_porc", "rh_max_porc", "rh_mean_porc")
  for (col in intersect(rh_cols, names(dt))) {
    n_before <- dt[!is.na(get(col)), .N]
    dt[get(col) < 0 | get(col) > 100, (col) := NA_real_]
    n_after <- dt[!is.na(get(col)), .N]
    removed <- n_before - n_after
    if (removed > 0 && verbose) {
      cli::cli_alert_info("Removed {removed} invalid {col} values")
      qc_stats$modified <- qc_stats$modified + removed
    }
  }
  
  press_cols <- c("patm_mb", "patm_min_mb", "patm_max_mb")
  for (col in intersect(press_cols, names(dt))) {
    n_before <- dt[!is.na(get(col)), .N]
    dt[get(col) < 700 | get(col) > 1100, (col) := NA_real_]
    n_after <- dt[!is.na(get(col)), .N]
    removed <- n_before - n_after
    if (removed > 0 && verbose) {
      cli::cli_alert_info("Removed {removed} invalid {col} values")
      qc_stats$modified <- qc_stats$modified + removed
    }
  }
  
  # Precipitation (0 a 500 mm )
  if ("rainfall_mm" %in% names(dt)) {
    n_before <- dt[!is.na(rainfall_mm), .N]
    dt[rainfall_mm < 0, rainfall_mm := 0]  
    dt[rainfall_mm > 500, rainfall_mm := NA_real_]  
    n_after <- dt[!is.na(rainfall_mm), .N]
    removed <- n_before - n_after
    if (removed > 0 && verbose) {
      cli::cli_alert_info("Corrected {n_before - n_after} rainfall values")
      qc_stats$modified <- qc_stats$modified + removed
    }
  }
  
  # solar radiation (0 a 40000 kJ/m2)
  if ("sr_kj_m2" %in% names(dt)) {
    n_before <- dt[!is.na(sr_kj_m2), .N]
    dt[sr_kj_m2 < 0, sr_kj_m2 := 0]
    dt[sr_kj_m2 > 40000, sr_kj_m2 := NA_real_]
    n_after <- dt[!is.na(sr_kj_m2), .N]
    removed <- n_before - n_after
    if (removed > 0 && verbose) {
      cli::cli_alert_info("Corrected {removed} solar radiation values")
      qc_stats$modified <- qc_stats$modified + removed
    }
  }
  
  # Velocidade do Vento (0 a 100 m/s)
  wind_speed_cols <- c("ws_2_m_s", "ws_gust_m_s")
  for (col in intersect(wind_speed_cols, names(dt))) {
    n_before <- dt[!is.na(get(col)), .N]
    dt[get(col) < 0, (col) := 0]
    dt[get(col) > 100, (col) := NA_real_]
    n_after <- dt[!is.na(get(col)), .N]
    removed <- n_before - n_after
    if (removed > 0 && verbose) {
      cli::cli_alert_info("Corrected {removed} {col} values")
      qc_stats$modified <- qc_stats$modified + removed
    }
  }
  
  # wind direction (0 a 360 graus)
  if ("wd_degrees" %in% names(dt)) {
    n_before <- dt[!is.na(wd_degrees), .N]
    dt[wd_degrees < 0 | wd_degrees > 360, wd_degrees := NA_real_]
    n_after <- dt[!is.na(wd_degrees), .N]
    removed <- n_before - n_after
    if (removed > 0 && verbose) {
      cli::cli_alert_info("Removed {removed} invalid wind direction values")
      qc_stats$modified <- qc_stats$modified + removed
    }
  }
  
  # ---- 3.Temporal ----
  if (all(c("tair_dry_bulb_c", "rh_mean_porc", "dew_tmean_c") %in% names(dt))) {

    dt[, dew_calc := .calculate_dew_point(tair_dry_bulb_c, rh_mean_porc)]
    
    dt[, dew_diff := abs(dew_tmean_c - dew_calc)]
    large_diff <- dt[!is.na(dew_diff) & dew_diff > 3, .N]
    
    if (large_diff > 0 && verbose) {
      cli::cli_alert_warning("{large_diff} rows with dew point inconsistency > 3\u00b0CC")

      dt[!is.na(dew_diff) & dew_diff > 3, dew_tmean_c := NA_real_]
    }
    
    dt[, c("dew_calc", "dew_diff") := NULL]
  }
  
  if ("datetime" %in% names(dt)) {
    data.table::setorder(dt, datetime)
    
    check_static <- function(col, max_static = 24) {
      if (col %in% names(dt)) {
        dt[, rle_id := data.table::rleid(get(col))]
        dt[, seq_len := .N, by = rle_id]
        
        static_rows <- dt[!is.na(get(col)) & seq_len > max_static, .N]
        if (static_rows > 0 && verbose) {
          cli::cli_alert_warning("Possible sensor failure: {static_rows} rows with static {col} (> {max_static} consecutive identical values)")
          dt[!is.na(get(col)) & seq_len > max_static, (col) := NA_real_]
        }
        
        dt[, c("rle_id", "seq_len") := NULL]
      }
    }
    
    check_static("tair_dry_bulb_c", 24)  # 24 horas
    check_static("rh_mean_porc", 48)     # 48 horas
    check_static("patm_mb", 72)         # 72 horas
  }
  
  if ("datetime" %in% names(dt) && nrow(dt) > 1) {

    dt[, time_diff := as.numeric(difftime(datetime, data.table::shift(datetime, type = "lag"), units = "hours"))]
    large_gaps <- dt[!is.na(time_diff) & time_diff > 6, .N]  
    
    if (large_gaps > 0 && verbose) {
      cli::cli_alert_info("{large_gaps} time gaps > 6 hours detected")
    }
    
    dt[, time_diff := NULL]
  }
  
  if (verbose) {
    cli::cli_h3("QC Summary")
    cli::cli_alert_success("Total modifications: {qc_stats$modified}")
    
    missing_counts <- sapply(dt, function(x) sum(is.na(x)))
    missing_vars <- names(missing_counts[missing_counts > 0])
    
    if (length(missing_vars) > 0) {
      cli::cli_alert_info("Missing values by variable:")
      for (var in missing_vars) {
        pct <- round(missing_counts[var] / nrow(dt) * 100, 1)
        cli::cli_li("{var}: {missing_counts[var]} ({pct}%)")
      }
    }
  }
  
  return(dt)
}

#' Calculate Dew Point from Temperature and Relative Humidity (FIXED VERSION)
#'
#' @description
#' Uses Magnus formula approximation.
#'
#' @param temp_c Temperature in Celsius.
#' @param rh_pct Relative humidity in percentage.
#'
#' @return Dew point temperature in Celsius.
#'
#' @keywords internal
#' @noRd
.calculate_dew_point <- function(temp_c, rh_pct) {
  # Constants for Magnus formula
  a <- 17.27
  b <- 237.7

  # Convert RH to fraction
  rh <- rh_pct / 100

  # Calculate dew point
  # FIX #7: Use base::log() explicitly
  alpha <- (a * temp_c) / (b + temp_c) + base::log(rh)
  dew_c <- (b * alpha) / (a - alpha)

  return(dew_c)
}

# ============================================================================
# TEMPORAL AGGREGATION
# ============================================================================

# --- Helper function imputation ---

#' Aggregate Climate Data Temporally (FIXED VERSION)
#'
#' @description
#' Aggregates meteorological data to specified temporal resolution.
#' Properly handles standardized column names and returns clean output.
#'
#' @param data Data frame with meteorological data.
#' @param time_unit Character. Temporal aggregation unit.
#' @param datetime_col Character. Name of datetime column (default: "data").
#' @param na.rm Logical. Whether to remove NAs in aggregation.
#'
#' @return Aggregated data frame with standardized column names.
#'
#' @keywords internal
#' @noRd
.aggregate_meteo_data <- function(
    data,
    time_unit = "day",
    datetime_col = "date",
    na.rm = TRUE) 
    {

  # FIX #10: Add input validation
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  # Ensure datetime column is in correct format
  if (!inherits(data[[datetime_col]], "POSIXct")) {
    data[[datetime_col]] <- lubridate::as_datetime(data[[datetime_col]])
  }

  # FIX #2: Use standardized column names consistently
  # FIX #6: Remove duplicate aggregation rules
  aggregation_rules <- list(
    # Precipitation - sum
    c("rainfall_mm", "sum"),

    # Atmospheric pressure - mean
    c("patm_mb", "mean"),
    c("patm_max_mb", "mean"),
    c("patm_min_mb", "mean"),

    # Solar radiation - sum (daily accumulated)
    c("sr_kj_m2", "sum"),

    # Temperatures - appropriate statistics
    c("tair_dry_bulb_c", "mean"),
    c("tair_max_c", "max"),
    c("tair_min_c", "min"),

    # Dew point temperatures
    c("dew_tmean_c", "mean"),
    c("dew_tmax_c", "max"),
    c("dew_tmin_c", "min"),

    # Relative humidity
    c("rh_max_porc", "max"),
    c("rh_min_porc", "min"),
    c("rh_mean_porc", "mean"),

    # Wind
    c("ws_gust_m_s", "max"),     # Maximum gust
    c("wd_degrees", "mean"),     # Mean wind direction
    c("ws_2_m_s", "mean"),       # Mean wind speed

    # Geographic/metadata - keep first observation
    c("region", "first"),
    c("station_name", "first"),
    c("station_code", "first"),
    c("latitude", "first"),
    c("longitude", "first"),
    c("altitude", "first"),
    c("UF", "first"),
    c("year", "first")
  )

  # Create safe aggregation functions
  safe_sum <- function(x, na.rm = TRUE) {
    if (all(is.na(x))) {
      return(NA_real_)
    }
    sum(x, na.rm = na.rm)
  }
  
  safe_mean <- function(x, na.rm = TRUE) {
    if (all(is.na(x))) {
      return(NA_real_)
    }
    mean(x, na.rm = na.rm)
  }
  
  safe_max <- function(x, na.rm = TRUE) {
    if (all(is.na(x))) {
      return(NA_real_)
    }
    result <- max(x, na.rm = na.rm)
    if (is.infinite(result) && result < 0) {
      return(NA_real_)
    }
    result
  }
  
  safe_min <- function(x, na.rm = TRUE) {
    if (all(is.na(x))) {
      return(NA_real_)
    }
    result <- min(x, na.rm = na.rm)
    # min() retorna Inf quando todos os valores sao NA com na.rm = TRUE
    if (is.infinite(result) && result > 0) {
      return(NA_real_)
    }
    result
  }
  
  safe_first <- function(x) {
    if (length(x) == 0 || all(is.na(x))) {
      return(NA)
    }
    x[1]
  }

  # Create aggregation functions
  agg_funs <- list()
  # Lista para armazenar apenas as variaveis meteorologicas (excluindo metadados de agrupamento)
  meteo_vars <- character()
  
  for (rule in aggregation_rules) {
    var_name <- rule[1]
    agg_type <- rule[2]

    if (var_name %in% names(data)) {
      if (agg_type == "sum") {
        agg_funs[[var_name]] <- function(x) safe_sum(x, na.rm = na.rm)
        meteo_vars <- c(meteo_vars, var_name)
      } else if (agg_type == "mean") {
        agg_funs[[var_name]] <- function(x) safe_mean(x, na.rm = na.rm)
        meteo_vars <- c(meteo_vars, var_name)
      } else if (agg_type == "max") {
        agg_funs[[var_name]] <- function(x) safe_max(x, na.rm = na.rm)
        meteo_vars <- c(meteo_vars, var_name)
      } else if (agg_type == "min") {
        agg_funs[[var_name]] <- function(x) safe_min(x, na.rm = na.rm)
        meteo_vars <- c(meteo_vars, var_name)
      } else if (agg_type == "first") {
        agg_funs[[var_name]] <- function(x) safe_first(x)
        if (!var_name %in% c("region", "station_name", "station_code", 
                            "latitude", "longitude", "altitude", "UF", "year")) {
          meteo_vars <- c(meteo_vars, var_name)
        }
      }
    }
  }

  # Prepare data for aggregation
  data_copy <- data

  # Create temporal aggregation column
  if (time_unit == "day") {
    data_copy$time_group <- as.Date(data_copy[[datetime_col]])
  } else if (grepl("^\\d+ days?$", time_unit, ignore.case = TRUE)) {
    n_days <- as.numeric(gsub("\\D", "", time_unit))
    data_copy$time_group <- lubridate::floor_date(
      data_copy[[datetime_col]],
      unit = paste(n_days, "days")
    )
  } else if (time_unit == "week") {
    data_copy$time_group <- lubridate::floor_date(
      data_copy[[datetime_col]],
      unit = "week"
    )
  } else if (grepl("^\\d+ weeks?$", time_unit, ignore.case = TRUE)) {
    n_weeks <- as.numeric(gsub("\\D", "", time_unit))
    data_copy$time_group <- lubridate::floor_date(
      data_copy[[datetime_col]],
      unit = paste(n_weeks, "weeks")
    )
  } else if (time_unit == "month") {
    data_copy$time_group <- lubridate::floor_date(
      data_copy[[datetime_col]],
      unit = "month"
    )
  } else if (grepl("^\\d+ months?$", time_unit, ignore.case = TRUE)) {
    n_months <- as.numeric(gsub("\\D", "", time_unit))
    data_copy$time_group <- lubridate::floor_date(
      data_copy[[datetime_col]],
      unit = paste(n_months, "months")
    )
  } else if (time_unit == "quarter") {
    data_copy$time_group <- lubridate::floor_date(
      data_copy[[datetime_col]],
      unit = "quarter"
    )
  } else if (time_unit == "year") {
    data_copy$time_group <- lubridate::floor_date(
      data_copy[[datetime_col]],
      unit = "year"
    )
  } else if (time_unit == "season") {
    # Define Brazilian seasons (southern hemisphere)
    get_brazilian_season <- function(date) {
      month <- lubridate::month(date)
      season <- dplyr::case_when(
        month %in% c(12, 1, 2) ~ "DJF",   # Summer
        month %in% c(3, 4, 5) ~ "MAM",    # Autumn
        month %in% c(6, 7, 8) ~ "JJA",    # Winter
        month %in% c(9, 10, 11) ~ "SON"   # Spring
      )
      return(season)
    }

    data_copy$time_group <- get_brazilian_season(data_copy[[datetime_col]])
  } else {
    cli::cli_abort("Unknown time_unit: '{time_unit}'")
  }

  # Aggregate data
  if (time_unit == "season") {
    # For seasons, we need year as well
    data_copy$year <- lubridate::year(data_copy[[datetime_col]])
    
    # Verificar se station_name existe nos dados
    if ("station_name" %in% names(data_copy)) {
      aggregated_data <- data_copy %>%
        dplyr::group_by(.data$station_name, .data$year, .data$time_group) %>%
        dplyr::summarise(
          dplyr::across(
            dplyr::any_of(meteo_vars),
            ~ agg_funs[[dplyr::cur_column()]](.)
          ),
          # Adicionar metadados explicitamente
          region = agg_funs[["region"]](region),
          station_code = agg_funs[["station_code"]](station_code),
          latitude = agg_funs[["latitude"]](latitude),
          longitude = agg_funs[["longitude"]](longitude),
          altitude = agg_funs[["altitude"]](altitude),
          UF = agg_funs[["UF"]](UF),
          .groups = "drop"
        ) %>%
        dplyr::rename(date = .data$time_group)
    } else {
      aggregated_data <- data_copy %>%
        dplyr::group_by(.data$year, .data$time_group) %>%
        dplyr::summarise(
          dplyr::across(
            dplyr::any_of(meteo_vars),
            ~ agg_funs[[dplyr::cur_column()]](.)
          ),
          region = agg_funs[["region"]](region),
          station_name = if ("station_name" %in% names(data_copy)) agg_funs[["station_name"]](station_name) else NA,
          station_code = agg_funs[["station_code"]](station_code),
          latitude = agg_funs[["latitude"]](latitude),
          longitude = agg_funs[["longitude"]](longitude),
          altitude = agg_funs[["altitude"]](altitude),
          UF = agg_funs[["UF"]](UF),
          .groups = "drop"
        ) %>%
        dplyr::rename(date = .data$time_group)
    }
  } else {
    if ("station_name" %in% names(data_copy)) {
      aggregated_data <- data_copy %>%
        dplyr::group_by(.data$station_name, .data$time_group) %>%
        dplyr::summarise(
          dplyr::across(
            dplyr::any_of(meteo_vars),
            ~ agg_funs[[dplyr::cur_column()]](.)
          ),
          region = agg_funs[["region"]](region),
          station_code = agg_funs[["station_code"]](station_code),
          latitude = agg_funs[["latitude"]](latitude),
          longitude = agg_funs[["longitude"]](longitude),
          altitude = agg_funs[["altitude"]](altitude),
          UF = agg_funs[["UF"]](UF),
          year = agg_funs[["year"]](year),
          .groups = "drop"
        ) %>%
        dplyr::rename(date = .data$time_group)
    } else {
      aggregated_data <- data_copy %>%
        dplyr::group_by(.data$time_group) %>%
        dplyr::summarise(
          dplyr::across(
            dplyr::any_of(meteo_vars),
            ~ agg_funs[[dplyr::cur_column()]](.)
          ),
          region = agg_funs[["region"]](region),
          station_name = if ("station_name" %in% names(data_copy)) agg_funs[["station_name"]](station_name) else NA,
          station_code = agg_funs[["station_code"]](station_code),
          latitude = agg_funs[["latitude"]](latitude),
          longitude = agg_funs[["longitude"]](longitude),
          altitude = agg_funs[["altitude"]](altitude),
          UF = agg_funs[["UF"]](UF),
          year = agg_funs[["year"]](year),
          .groups = "drop"
        ) %>%
        dplyr::rename(date = .data$time_group)
    }
  }

  # FIX #8: Remove unnecessary attributes
  # (They can cause issues with dplyr/tidyverse operations)

  return(dplyr::as_tibble(aggregated_data))
}

# ============================================================================
# STPATIAL MACHTING
# ============================================================================

#' Spatial Matching: Assign Climate Data to Geographic Units
#' @keywords internal
#' 
#' @param df Data frame with climate station data
#' @param spatial_obj Spatial object with municipality data
#' @param quality_threshold Maximum allowed missing data percentage (0 to 1)
#' @param msg Message templates for warnings/info
#' @param verbose Logical, whether to display messages
#' @param quality_vars Character vector of variable names to use for quality assessment
#' @param quality_method Method for combining multiple quality variables: "any", "all", or "mean"
#' 
#' @details This function matches climate stations to municipalities based on:
#' - Spatial proximity (nearest station using sf::st_nearest_feature)
#' - Data quality (completeness of specified variables)
#' - Allows fallback to nearest station if quality criteria not met
#' 
#' @return Data frame with matched climate data and municipality information
#' @noRd
.match_spatial <- function(df, spatial_obj, quality_threshold, msg, verbose,
                          quality_vars = "tair_dry_bulb_c",
                          quality_method = c("any", "all", "mean"))
                          {
  
  # Validar quality_method
  quality_method <- match.arg(quality_method)
  
  required_cols <- c("station_name", "latitude", "longitude")
  if (!all(required_cols %in% names(df))) {
    cli::cli_abort("Dados de estacoes devem conter colunas: {paste(required_cols, collapse = ', ')}")
  }
  
  if (!"code_muni" %in% names(spatial_obj)) {
    cli::cli_abort("spatial_obj deve conter coluna 'code_muni'. Please, use sus_join_spatial() for getting it")
  }
  
  missing_vars <- setdiff(quality_vars, names(df))
  if (length(missing_vars) > 0) {
    cli::cli_warn("Algumas variaveis de qualidade nao encontradas: {missing_vars}")
    cli::cli_inform("Usando apenas variaveis disponiveis para calculo de qualidade")
    quality_vars <- intersect(quality_vars, names(df))
  }
  
  if (length(quality_vars) == 0) {
    cli::cli_abort("Nenhuma variavel de qualidade valida encontrada no conjunto de dados")
  }
  
  if (verbose) {
    cli::cli_inform("Calculando qualidade com {length(quality_vars)} variavel(s): {paste(quality_vars, collapse = ', ')}")
    cli::cli_inform("Metodo de combinacao: {quality_method}")
  }
  
  stations <- df %>%
    dplyr::distinct(.data$station_name, .data$latitude, .data$longitude) %>%
    sf::st_as_sf(
      coords = c("longitude", "latitude"),
      crs = 4674,
      remove = FALSE
    )
  
  station_quality <- .calculate_station_quality(
    df = df,
    quality_vars = quality_vars,
    quality_method = quality_method,
    verbose = verbose
  )
  
  if (sf::st_crs(spatial_obj) != sf::st_crs(stations)) {
    spatial_obj <- sf::st_transform(spatial_obj, sf::st_crs(stations))
  }
  
  spatial_centroids <- suppressWarnings(
    spatial_obj %>% sf::st_centroid()
  )
  
  nearest_indices <- sf::st_nearest_feature(spatial_centroids, stations)
  
  mun_station_map <- data.frame(
    code_muni = spatial_centroids$code_muni,
    station_index = nearest_indices,
    row.names = NULL
  )
  
  distances_list <- list()
  
  for (i in seq_len(nrow(spatial_centroids))) {
    mun_center <- spatial_centroids[i, ]
    station_idx <- nearest_indices[i]
    
    distance_m <- as.numeric(sf::st_distance(mun_center, stations[station_idx, ]))
    distances_list[[i]] <- distance_m
  }
  
  mun_station_map <- mun_station_map %>%
    dplyr::mutate(
      station_name = stations$station_name[station_index],
      distance_m = unlist(distances_list)
    )
  
  mun_station_map <- mun_station_map %>%
    dplyr::left_join(station_quality, by = "station_name") %>%
    dplyr::mutate(
      passes_quality = missing_pct <= quality_threshold
    )
  
  problematic_munis <- mun_station_map %>%
    dplyr::filter(!passes_quality) %>%
    dplyr::pull(code_muni)
  
  if (length(problematic_munis) > 0) {
    if (verbose) {
      cli::cli_inform("Encontradas {length(problematic_munis)} municipios com qualidade abaixo do limiar")
    }
    
    for (mun_code in problematic_munis) {
      mun_idx <- which(spatial_centroids$code_muni == mun_code)
      mun_center <- spatial_centroids[mun_idx, ]
      
      distances <- sf::st_distance(mun_center, stations) %>%
        as.numeric()
      
      station_order <- order(distances)
      
      found <- FALSE
      for (idx in station_order) {
        station_name <- stations$station_name[idx]
        quality_info <- station_quality %>%
          dplyr::filter(station_name == !!station_name)
        
        if (nrow(quality_info) > 0 && quality_info$missing_pct <= quality_threshold) {
          # Atualizar mapeamento
          mun_station_map$station_name[mun_station_map$code_muni == mun_code] <- station_name
          mun_station_map$station_index[mun_station_map$code_muni == mun_code] <- idx
          mun_station_map$distance_m[mun_station_map$code_muni == mun_code] <- distances[idx]
          mun_station_map$missing_pct[mun_station_map$code_muni == mun_code] <- quality_info$missing_pct
          mun_station_map$passes_quality[mun_station_map$code_muni == mun_code] <- TRUE
          found <- TRUE
          break
        }
      }
      
      if (!found && verbose) {
        best_idx <- station_order[1]
        station_name <- stations$station_name[best_idx]
        quality_info <- station_quality %>%
          dplyr::filter(station_name == !!station_name)
        
        if (nrow(quality_info) > 0) {
          quality_pct <- quality_info$missing_pct * 100
          
          if ("missing_details" %in% names(quality_info)) {
            details <- quality_info$missing_details[[1]]
            problem_vars <- names(details)[details > quality_threshold]
            if (length(problem_vars) > 0) {
              cli::cli_alert_warning(glue::glue(
                msg$quality_warning_detail,
                name = station_name,
                pct = round(quality_pct, 1),
                vars = paste(problem_vars, collapse = ", ")
              ))
            } else {
              cli::cli_alert_warning(glue::glue(
                msg$quality_warning,
                name = station_name,
                pct = round(quality_pct, 1)
              ))
            }
          } else {
            cli::cli_alert_warning(glue::glue(
              msg$quality_warning,
              name = station_name,
              pct = round(quality_pct, 1)
            ))
          }
        }
      }
    }
  }
  
  unique_munis <- unique(spatial_obj$code_muni)
  
  matched_data_list <- list()
  
  for (i in seq_along(unique_munis)) {
    mun_code <- unique_munis[i]
    
    station_info <- mun_station_map %>%
      dplyr::filter(code_muni == mun_code) %>%
      dplyr::slice(1)  # Pegar primeiro se houver duplicatas
    
    if (nrow(station_info) > 0) {
      mun_info <- spatial_obj %>%
        dplyr::filter(code_muni == mun_code) %>%
        dplyr::slice(1) %>%
        sf::st_drop_geometry()
      
      station_data <- df %>%
        dplyr::filter(station_name == station_info$station_name) %>%
        dplyr::mutate(
          code_muni = mun_code,
          name_muni = ifelse("name_muni" %in% names(mun_info), 
                           mun_info$name_muni[1], NA_character_),
          distance_km = station_info$distance_m / 1000,
          quality_missing_pct = ifelse(!is.na(station_info$missing_pct), 
                                      station_info$missing_pct * 100, NA_real_)
        )
      
      matched_data_list[[i]] <- station_data
    }
  }
  
  # Combinar todos os dados
  matched_data <- dplyr::bind_rows(matched_data_list)
  
  return(matched_data)
}

#' Helper function: Calculate station quality based on multiple variables
#' @keywords internal
#' @noRd
.calculate_station_quality <- function(df, quality_vars, quality_method, verbose = FALSE) {
  
  if (length(quality_vars) == 1) {
    station_quality <- df %>%
      dplyr::group_by(station_name) %>%
      dplyr::summarise(
        missing_pct = sum(is.na(.data[[quality_vars]])) / dplyr::n(),
        .groups = "drop"
      )
    
  } else {
    station_quality <- df %>%
      dplyr::group_by(station_name) %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(quality_vars),
          ~ sum(is.na(.)) / dplyr::n(),
          .names = "missing_{.col}"
        ),
        .groups = "drop"
      )
    
    missing_cols <- paste0("missing_", quality_vars)
    
    if (quality_method == "any") {
      station_quality <- station_quality %>%
        dplyr::mutate(
          missing_pct = pmax(!!!rlang::syms(missing_cols), na.rm = TRUE)
        )
      
    } else if (quality_method == "all") {
      station_quality <- station_quality %>%
        dplyr::mutate(
          missing_pct = pmin(!!!rlang::syms(missing_cols), na.rm = TRUE)
        )
      
    } else if (quality_method == "mean") {
      station_quality <- station_quality %>%
        dplyr::mutate(
          missing_pct = rowMeans(
            dplyr::across(dplyr::all_of(missing_cols)),
            na.rm = TRUE
          )
        )
    }
    
    # Adicionar detalhes para debugging
    if (verbose) {
      station_quality <- station_quality %>%
        dplyr::mutate(
          missing_details = purrr::map(1:dplyr::n(), function(i) {
            row_data <- station_quality[i, ]
            details <- row_data %>%
              dplyr::select(dplyr::all_of(missing_cols)) %>%
              as.list()
            names(details) <- quality_vars
            details
          })
        )
    }
  }
  
  return(station_quality)
}

#' Helper function: Calculate station quality based on multiple variables (parallel version)
#' @keywords internal
#' @noRd
.generate_quality_messages <- function() {
  list(
    quality_warning = paste(
      "Estacao {name} usada apesar de {pct}% de dados faltantes.",
      "Considere aumentar quality_threshold ou revisar os dados."
    ),
    quality_warning_detail = paste(
      "Estacao {name} usada apesar de {pct}% de dados faltantes.",
      "Variaveis problematicas: {vars}.",
      "Considere ajustar quality_vars ou quality_threshold."
    ),
    quality_info = paste(
      "Matching espacial concluido.",
      "Variaveis de qualidade: {vars}",
      "Metodo: {method}",
      "Limiar: {threshold}%"
    )
  )
}


#' Spatial Matching: Assign Climate Data to Geographic Units (Parallel Version)
#' 
#' @param df Data frame with climate station data
#' @param spatial_obj Spatial object with municipality data
#' @param quality_threshold Maximum allowed missing data percentage (0 to 1)
#' @param msg Message templates for warnings/info
#' @param verbose Logical, whether to display messages
#' @param parallel Logical, whether to use parallel processing
#' @param workers Number of parallel workers to use
#' @param quality_vars Character vector of variable names to use for quality assessment
#' @param quality_method Method for combining multiple quality variables: "any", "all", or "mean"
#' 
#' @details This function matches climate stations to municipalities using parallel processing
#' @keywords  internal
#' @noRd
.match_spatial_with_parallel <- function(df, spatial_obj, quality_threshold, msg, verbose,
                          parallel = FALSE, workers = future::availableCores() - 1,
                          quality_vars = "tair_dry_bulb_c",
                          quality_method = c("any", "all", "mean")) 
                          {
  
  quality_method <- match.arg(quality_method)
  
  required_cols <- c("station_name", "latitude", "longitude")
  if (!all(required_cols %in% names(df))) {
    cli::cli_abort("Dados de estacoes devem conter colunas: {paste(required_cols, collapse = ', ')}")
  }
  
  if (!"code_muni" %in% names(spatial_obj)) {
    cli::cli_abort("spatial_obj deve conter coluna 'code_muni'")
  }
  
  missing_vars <- setdiff(quality_vars, names(df))
  if (length(missing_vars) > 0) {
    cli::cli_warn("Algumas variaveis de qualidade nao encontradas: {missing_vars}")
    cli::cli_inform("Usando apenas variaveis disponiveis para calculo de qualidade")
    quality_vars <- intersect(quality_vars, names(df))
  }
  
  if (length(quality_vars) == 0) {
    cli::cli_abort("Nenhuma variavel de qualidade valida encontrada no conjunto de dados")
  }
  
  if (verbose) {
    cli::cli_inform("Calculando qualidade com {length(quality_vars)} variavel(s): {paste(quality_vars, collapse = ', ')}")
    cli::cli_inform("Metodo de combinacao: {quality_method}")
    if (parallel) {
      cli::cli_inform("Processamento paralelo ativado com {workers} workers")
    }
  }
  stations <- df %>%
    dplyr::distinct(.data$station_name, .data$latitude, .data$longitude) %>%
  
    dplyr::mutate(
      station_id = dplyr::row_number(),
      station_unique_name = paste0(station_name, "_", station_id)
    ) %>%
    sf::st_as_sf(
      coords = c("longitude", "latitude"),
      crs = 4674,
      remove = FALSE
    )

  df_with_id <- df %>%
    dplyr::left_join(
      stations %>% 
        sf::st_drop_geometry() %>%
        dplyr::select(station_name, latitude, longitude, station_unique_name),
      by = c("station_name", "latitude", "longitude")
    )
  
  station_quality <- .calculate_station_quality_parallel(
    df = df_with_id,
    quality_vars = quality_vars,
    quality_method = quality_method,
    verbose = verbose
  )

  if (sf::st_crs(spatial_obj) != sf::st_crs(stations)) {
    spatial_obj <- sf::st_transform(spatial_obj, sf::st_crs(stations))
  }
  
  spatial_centroids <- suppressWarnings(
    spatial_obj %>% sf::st_centroid()
  )
  
  if (parallel && workers > 1) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = workers)
    
    if (verbose) {
      cli::cli_inform("Iniciando processamento paralelo...")
      cli::cli_progress_bar("Processando municipios", total = length(unique(spatial_obj$code_muni)))
    }

    unique_munis <- unique(spatial_obj$code_muni)
    
    matched_data_list <- future.apply::future_lapply(
      seq_along(unique_munis),
      function(i) {
        result <- .process_municipality_optimized_parallel(
          mun_code = unique_munis[i],
          spatial_obj = spatial_obj,
          spatial_centroids = spatial_centroids,
          df = df_with_id,
          stations = stations,
          station_quality = station_quality,
          quality_threshold = quality_threshold,
          quality_vars = quality_vars,
          quality_method = quality_method,
          msg = msg,
          verbose = verbose && (i %% 10 == 0)  
        )
        
        if (verbose && i %% 10 == 0) {
          cli::cli_progress_update()
        }
        
        return(result)
      },
      future.seed = TRUE,
      future.chunk.size = ceiling(length(unique_munis) / workers)
    )
    
    if (verbose) {
      cli::cli_progress_done()
      cli::cli_inform("Combinando resultados...")
    }
    
    matched_data <- dplyr::bind_rows(matched_data_list)
    
  } else {

    if (verbose) cli::cli_inform("Processamento sequencial...")
    
    unique_munis <- unique(spatial_obj$code_muni)
    matched_data_list <- list()
    
    if (verbose) {
      cli::cli_progress_bar("Processando municipios", total = length(unique_munis))
    }
    
    for (i in seq_along(unique_munis)) {
      matched_data_list[[i]] <- .process_municipality_optimized_parallel(
        mun_code = unique_munis[i],
        spatial_obj = spatial_obj,
        spatial_centroids = spatial_centroids,
        df = df_with_id,
        stations = stations,
        station_quality = station_quality,
        quality_threshold = quality_threshold,
        quality_vars = quality_vars,
        quality_method = quality_method,
        msg = msg,
        verbose = verbose
      )
      
      if (verbose) cli::cli_progress_update()
    }
    
    if (verbose) cli::cli_progress_done()
    
    matched_data <- dplyr::bind_rows(matched_data_list)
  }
  
  matched_data <- matched_data %>%
    dplyr::distinct(
      station_unique_name, code_muni, date,
      .keep_all = TRUE
    )
  
  if ("distance_km" %in% names(matched_data)) {
  } else {
    
    distance_map <- data.frame(
      code_muni = spatial_centroids$code_muni,
      station_index = sf::st_nearest_feature(spatial_centroids, stations)
    ) %>%
      dplyr::mutate(
        station_name = stations$station_name[station_index],
        distance_m = purrr::map2_dbl(
          seq_len(nrow(spatial_centroids)),
          station_index,
          ~ as.numeric(sf::st_distance(
            spatial_centroids[.x, ],
            stations[.y, ]
          ))
        )
        #distance_km = distance_m / 1000
      ) %>%
      dplyr::select(.data$code_muni, .data$station_name, .data$distance_m)
    distance_map <- purrr::map2_df(
      seq_len(nrow(spatial_centroids)),
      sf::st_nearest_feature(spatial_centroids, stations),
      function(i, station_idx) {
        dist_m <- as.numeric(sf::st_distance(
          spatial_centroids[i, ],
          stations[station_idx, ]
        ))
        
        data.frame(
          code_muni = spatial_centroids$code_muni[i],
          station_index = station_idx,
          station_name = stations$station_name[station_idx],
          distance_m = dist_m,
          stringsAsFactors = FALSE
        )
      }
    ) %>%
      dplyr::mutate(
        distance_km = distance_m / 1000
      ) %>%
      dplyr::select(.data$code_muni, .data$station_name, .data$distance_km)
        matched_data <- matched_data %>%
          dplyr::left_join(
            distance_map,
            by = c("code_muni", "station_name")
          )
      }
  
  if (verbose) {
    cli::cli_alert_success("Matching espacial concluido!")
    cli::cli_inform("Municipios processados: {length(unique(matched_data$code_muni))}")
    cli::cli_inform("Estacoes utilizadas: {length(unique(matched_data$station_name))}")
    cli::cli_inform("Total de registros: {nrow(matched_data)}")
  }
  
  return(matched_data)
}

#' Helper function: Calculate station quality based on multiple variables (parallel version)
#' @keywords internal
#' @noRd
.calculate_station_quality_parallel <- function(df, quality_vars, quality_method, verbose = FALSE) {
  
  if (length(quality_vars) == 1) {
    station_quality <- df %>%
      dplyr::group_by(station_unique_name) %>%
      dplyr::summarise(
        station_name = dplyr::first(station_name),
        missing_pct = sum(is.na(.data[[quality_vars]])) / dplyr::n(),
        .groups = "drop"
      )
    
  } else {
    station_quality <- df %>%
      dplyr::group_by(station_unique_name) %>%
      dplyr::summarise(
        station_name = dplyr::first(station_name),
        dplyr::across(
          dplyr::all_of(quality_vars),
          ~ sum(is.na(.)) / dplyr::n(),
          .names = "missing_{.col}"
        ),
        .groups = "drop"
      )
    
    missing_cols <- paste0("missing_", quality_vars)
    
    if (quality_method == "any") {
      station_quality <- station_quality %>%
        dplyr::mutate(
          missing_pct = do.call(pmax, c(.[, missing_cols], list(na.rm = TRUE)))
        )
      
    } else if (quality_method == "all") {
      station_quality <- station_quality %>%
        dplyr::mutate(
          missing_pct = do.call(pmin, c(.[, missing_cols], list(na.rm = TRUE)))
        )
      
    } else if (quality_method == "mean") {
      station_quality <- station_quality %>%
        dplyr::mutate(
          missing_pct = rowMeans(
            dplyr::select(., dplyr::all_of(missing_cols)),
            na.rm = TRUE
          )
        )
    }
  }
  
  return(station_quality)
}

#' Helper function otimizada para processar um unico municipio (parallel version)
#' @keywords internal
#' @noRd
.process_municipality_optimized_parallel <- function(mun_code, spatial_obj, spatial_centroids, 
                                           df, stations, station_quality, 
                                           quality_threshold, quality_vars, quality_method,
                                           msg, verbose) 
                                           {
  
  mun_idx <- which(spatial_centroids$code_muni == mun_code)
  
  if (length(mun_idx) == 0) {
    return(NULL)
  }
  
  mun_center <- spatial_centroids[mun_idx, ]
  
  nearest_idx <- sf::st_nearest_feature(mun_center, stations)
  
  if (length(nearest_idx) > 1) {
    nearest_idx <- nearest_idx[1]
  }
  
  station_unique_name <- stations$station_unique_name[nearest_idx]
  
  distance_m <- as.numeric(sf::st_distance(mun_center, stations[nearest_idx, ]))
  
  quality_info <- station_quality %>%
    dplyr::filter(station_unique_name == !!station_unique_name)
  
  if (nrow(quality_info) == 0 || quality_info$missing_pct > quality_threshold) {
    distances <- sf::st_distance(mun_center, stations) %>%
      as.numeric()
    
    station_order <- order(distances)
    
    found <- FALSE
    for (idx in station_order) {
      candidate_station <- stations$station_unique_name[idx]
      candidate_quality <- station_quality %>%
        dplyr::filter(station_unique_name == !!candidate_station)
      
      if (nrow(candidate_quality) > 0 && candidate_quality$missing_pct <= quality_threshold) {
        station_unique_name <- candidate_station
        distance_m <- distances[idx]
        quality_info <- candidate_quality
        found <- TRUE
        break
      }
    }
    
    if (!found) {
      best_idx <- station_order[1]
      station_unique_name <- stations$station_unique_name[best_idx]
      distance_m <- distances[best_idx]
      quality_info <- station_quality %>%
        dplyr::filter(station_unique_name == !!station_unique_name)
      
      if (verbose && nrow(quality_info) > 0) {
        quality_pct <- quality_info$missing_pct * 100
        
        problem_details <- ""
        if (length(quality_vars) > 1) {
          missing_details <- quality_info %>%
            dplyr::select(dplyr::starts_with("missing_"))
          
          if (ncol(missing_details) > 0) {
            problem_vars <- names(missing_details)[as.numeric(missing_details[1, ]) > quality_threshold]
            if (length(problem_vars) > 0) {
              problem_vars_clean <- gsub("missing_", "", problem_vars)
              problem_details <- paste("Variaveis problematicas:", paste(problem_vars_clean, collapse = ", "))
            }
          }
        }
        
        warning_msg <- if (nchar(problem_details) > 0) {
          glue::glue(msg$quality_warning_detail,
                     name = quality_info$station_name[1],
                     pct = round(quality_pct, 1),
                     vars = problem_details)
        } else {
          glue::glue(msg$quality_warning,
                     name = quality_info$station_name[1],
                     pct = round(quality_pct, 1))
        }
        
        message(warning_msg)
      }
    }
  }
  
  mun_info <- spatial_obj %>%
    dplyr::filter(code_muni == mun_code) %>%
    dplyr::slice(1) %>%
    sf::st_drop_geometry()
  
  if (station_unique_name %in% df$station_unique_name) {
    station_data <- df %>%
      dplyr::filter(station_unique_name == !!station_unique_name) %>%
      dplyr::mutate(
        code_muni = mun_code,
        name_muni = ifelse("name_muni" %in% names(mun_info), 
                          mun_info$name_muni[1], NA_character_),
        #distance_km = distance_m / 1000,
        distance_km = if (length(distance_m) == 1) distance_m / 1000 else NA_real_,
        quality_missing_pct = ifelse(nrow(quality_info) > 0, 
                                    quality_info$missing_pct[1] * 100, 
                                    NA_real_)
      )
    return(station_data)
  }
  
  return(NULL)
}

# ============================================================================
# TEMPORAL MACHTING
# ============================================================================

#' Integracao Temporal: Matching por Combinacao Exata
#' @keywords internal
#' @noRd
.join_exact <- function(health_data, climate_data, target_vars = NULL) {
  
   if (is.null(target_vars)) { 
   target_vars <- climate_data %>%
      dplyr::select(rainfall_mm:ws_2_m_s) %>% 
      names()
  } else {
    target_vars
  }

  result <- health_data %>%
    dplyr::left_join(
      climate_data %>% dplyr::select(.data$date, .data$code_muni, dplyr::all_of(target_vars)),
      by = c("date", "code_muni")
      #suffix = c("_health", "_climate")
    )
  
  result <- result %>%
    dplyr::relocate(
      date, code_muni,
      .after = date
    )
  
  return(result)
}

#' Integracao Temporal: Matching por Janela Temporal
#' @keywords internal
#' @noRd
.join_window <- function(
  health_data,
  climate_data,
  window_days = 0,
  target_vars = NULL, 
  min_obs = 0.7
) {
  window_size <- 2 * window_days + 1

 if (is.null(target_vars)) { 
   target_vars <- climate_data %>%
      dplyr::select(rainfall_mm:ws_2_m_s) %>% 
      names()
  } else {
    target_vars
  }
    
  health_win <- health_data %>% 
    dplyr::mutate(
      .win_start = .data$date - window_days,
      .win_end = .data$date + window_days
    )

  climate_sel <- climate_data %>%
    dplyr::select(date, code_muni, dplyr::all_of(target_vars))

  joined <- health_win %>%
    dplyr::left_join(
      climate_sel %>% dplyr::rename(date_climate = date),
      by = c("code_muni"),
      relationship = "many-to-many"
    ) %>%
    dplyr::filter(
      .data$date_climate >= .win_start & .data$date_climate <= .win_end
    )

  agg_fun_var <- function(var) {
    sum_vars <- c(
      "rainfall_mm", #
      "sr_kj_m2" # 
    )

    mean_vars <- c(
      "patm_mb",
      "patm_max_mb",
      "patm_min_mb",
      "tair_dry_bulb_c",
      "tair_max_c",
      "tair_min_c",
      "dew_tmean_c",
      "dew_tmax_c",
      "dew_tmin_c",
      "rh_max_porc",
      "rh_min_porc",
      "rh_mean_porc",
      "ws_gust_m_s",
      "ws_2_m_s"
    )
    if (var == "wd_degrees") {
      return("mean_circular")
    }

    if (var %in% sum_vars) {
      return("sum")
    }

    if (var %in% mean_vars) {
      return("mean")
    }

    return("mean")
  }

  .make_agg_rule <- function(vars) {
    dplyr::as_tibble(
      climate_var = vars,
      agg_type = dplyr::case_when(
        climate_var == "rainfall_mm" ~ "sum",
        climate_var == "wd_degrees" ~ "mean_circular",
        TRUE ~ "mean"
      )
    )
  }
  agg_rule <- .make_agg_rule(target_vars)

  health_id_vars <- setdiff( names(health_win), c(".win_start", ".win_end"))

  out <- joined %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(target_vars),
      names_to = "climate_var",
      values_to = "value"
    ) %>%
    dplyr::left_join(agg_rule, by = "climate_var") %>%
    
    dplyr::group_by(
      dplyr::across(dplyr::all_of(health_id_vars)),
      climate_var
    ) %>%
    dplyr::summarise(
      n_obs = dplyr::n(),
      prop_obs = n_obs / window_size,
      
      value_window = dplyr::case_when(
        dplyr::first(agg_type) == "sum" ~ sum(value, na.rm = TRUE),
        dplyr::first(agg_type) == "mean" ~ mean(value, na.rm = TRUE),
        dplyr::first(agg_type) == "mean_circular" ~
          atan2(
            mean(sin(value * pi / 180), na.rm = TRUE),
            mean(cos(value * pi / 180), na.rm = TRUE)
          ) * 180 / pi,
        TRUE ~ mean(value, na.rm = TRUE)
      ),
      .groups = 'drop'
    ) %>%
    
    dplyr::filter(prop_obs >= min_obs) %>%
    
    dplyr::select(
      dplyr::all_of(health_id_vars),
      climate_var,
      value_window
    ) %>%
    tidyr::pivot_wider(
      names_from = climate_var,
      values_from = value_window,
      names_prefix = paste0("win", window_days, "_")
    )

  return(out)
}

#' Integracao Temporal com Janela Movel
#' @keywords internal
#' @noRd
.join_lag <- function(health_data, climate_data,
                         lag_days = c(0, 7, 14),
                         target_vars = NULL,
                         agg_funs = list(mean = mean, sum = sum),
                         min_obs = 1) 
                         {
  
   window_size = length(lag_days)
  
  if (is.null(target_vars)) { 
   target_vars <- climate_data %>%
      dplyr::select(rainfall_mm:ws_2_m_s) %>% 
      names()
  } else {
    target_vars
  }

  .make_agg_rule <- function(vars) {
    dplyr::as_tibble(
      climate_var = vars,
      agg_type = dplyr::case_when(
        stringr::str_detect(climate_var, "rainfall_mm") ~ "sum",
        stringr::str_detect(climate_var, "sr_kj_m2") ~ "sum",
        stringr::str_detect(climate_var, "wd_degrees") ~ "mean_circular",
        TRUE ~ "mean"
      )
    )
  }
  
  agg_rule <- .make_agg_rule(target_vars)
  
  result <- health_data
  
  for (lag in lag_days) {
    health_windows <- health_data %>%
      dplyr::mutate(
        .lag_date = .data$date - lubridate::days(lag),
        .win_start = .data$.lag_date - lubridate::days(window_size - 1),
        .win_end = .data$.lag_date
      ) %>%
      dplyr::select(
        dplyr::all_of(names(health_data)),
        .data$.win_start, .data$.win_end
      )
    
    joined <- health_windows %>%
      tidyr::expand_grid(
        date_climate = seq(
          from = min(health_windows$.win_start),
          to = max(health_windows$.win_end),
          by = "day"
        )
      ) %>%
      dplyr::filter(
        .data$date_climate >= .data$.win_start & .data$date_climate <= .data$.win_end
      ) %>%
      dplyr::left_join(
        climate_data %>%
          dplyr::select(.data$code_muni, .data$date, dplyr::all_of(target_vars)) %>%
          dplyr::rename(date_climate = date),
        by = c("code_muni", "date_climate")
      )
    
    for (var in target_vars) {
      agg_type <- agg_rule %>% 
        dplyr::filter(climate_var == var) %>%
        dplyr::pull(agg_type)
      
      aggregated <- joined %>%
        dplyr::group_by(
          dplyr::across(dplyr::all_of(setdiff(names(health_data), "geom"))),
          .win_start, .win_end
        ) %>%
        dplyr::summarise(
          n_obs = sum(!is.na(.data[[var]])),
          prop_obs = n_obs / window_size,
          
          value = dplyr::case_when(
            agg_type == "sum" ~ sum(.data[[var]], na.rm = TRUE),
            agg_type == "mean" ~ mean(.data[[var]], na.rm = TRUE),
            agg_type == "mean_circular" ~
              atan2(
                mean(sin(.data[[var]] * pi / 180), na.rm = TRUE),
                mean(cos(.data[[var]] * pi / 180), na.rm = TRUE)
              ) * 180 / pi,
            TRUE ~ mean(.data[[var]], na.rm = TRUE)
          ),
          
          value_final = ifelse(
            prop_obs >= min_obs, 
            value,
            NA_real_
          ),
          .groups = 'drop'
        ) %>%
        dplyr::select(-.data$value, -.data$n_obs, -.data$prop_obs, -.data$.win_start, -.data$.win_end) %>%
        dplyr::rename_with(~ paste0("win", window_size, "_", var, "_lag", lag), .cols = "value_final")
      
      result <- result %>%
        dplyr::left_join(
          aggregated,
          by = setdiff(names(health_data), "geom")
        )
    }
  }
  
  return(result)
}

#' Integracao Temporal: Matching Sazonal
#' @keywords internal
#' @noRd
.join_seasonal <- function(health_data, climate_data) {

  get_brazilian_season <- function(date) {
    month <- lubridate::month(date)
    dplyr::case_when(
      month %in% c(12, 1, 2) ~ "Summer",
      month %in% c(3, 4, 5) ~ "Autumn", 
      month %in% c(6, 7, 8) ~ "Winter",
      month %in% c(9, 10, 11) ~ "Spring"
    )
  }
  climate_seasonal <- climate_data %>%
    dplyr::mutate(
      year = lubridate::year(date),
      season = get_brazilian_season(date)
    ) %>%
    dplyr::group_by(.data$code_muni, .data$station_name, .data$year, .data$season) %>%
    dplyr::summarise(
      tair_season_mean = mean(.data$tair_dry_bulb_c, na.rm = TRUE),
      tair_season_max = max(.data$tair_max_c, na.rm = TRUE),
      tair_season_min = min(.data$tair_min_c, na.rm = TRUE),
      rainfall_season_total = sum(.data$rainfall_mm, na.rm = TRUE),
      rainfall_season_days = sum(.data$rainfall_mm > 0, na.rm = TRUE),
      rh_season_mean = mean(.data$rh_mean_porc, na.rm = TRUE),
      n_valid_days = sum(!is.na(.data$tair_dry_bulb_c)),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_valid_days >= 60) 
  
  health_data %>%
    dplyr::mutate(
      year = lubridate::year(date),
      season = get_brazilian_season(date)
    ) %>%
    dplyr::left_join(
      climate_seasonal,
      by = c("code_muni", "station_name", "year", "season")
    )
}


#' Integracao Temporal: Matching Sazonal
#' @description
#' Agrega dados climaticos por estacao do ano e associa a dados de saude.
#' 
#' @param health_data Dados de saude (data.frame ou tibble)
#' @param climate_data Dados climaticos (data.frame ou tibble)
#' @param target_vars Variaveis climaticas para incluir (opcional, se NULL usa todas)
#' @param agg_funs Funcoes de agregacao por variavel (lista nomeada)
#' @param season_def Definicao de estacoes: "brazil" (padrao), "official", "custom"
#' @param custom_seasons Lista customizada de estacaes (se season_def = "custom")
#' @param min_season_days Dias minimos por estacao para considerar valida
#' @param na_prop_max Proporcao maxima de NAs permitida por estacao
#' 
#' @return health_data com variaveis climaticas agregadas por estacao
#' 
#' @keywords internal
#' @noRd
.join_seasonal <- function(health_data, 
                           climate_data,
                           target_vars = NULL,
                           agg_funs = NULL,
                           season_def = c("brazil", "official", "custom"),
                           custom_seasons = NULL,
                           min_season_days = 60,
                           na_prop_max = 0.3) 
                           {
  
  # Validar inputs
  season_def <- match.arg(season_def)
  
  if (is.null(target_vars)) { 
   target_vars <- climate_data %>%
      dplyr::select(rainfall_mm:ws_2_m_s) %>%
      names()
  } else {
    target_vars
  }

  safe_max <- function(x, ...) if(all(is.na(x))) NA_real_ else max(x, ..., na.rm = TRUE)
  safe_min <- function(x, ...) if(all(is.na(x))) NA_real_ else min(x, ..., na.rm = TRUE)
  
  required_climate <- c("code_muni", "date", target_vars)
  missing_cols <- setdiff(required_climate, names(climate_data))
  if (length(missing_cols) > 0) {
    warning("Colunas ausentes em climate_data: ", paste(missing_cols, collapse = ", "))
    target_vars <- setdiff(target_vars, missing_cols)
  }
  
  if (is.null(agg_funs)) {
      agg_funs <- list(
        rainfall_mm = list(
          total = ~sum(.x, na.rm = TRUE),
          days_above = ~sum(.x > 0, na.rm = TRUE),
          max = ~safe_max(.x), # Ajustado
          mean = ~mean(.x, na.rm = TRUE)
        ),
        tair = list(
          mean = ~mean(.x, na.rm = TRUE),
          max = ~safe_max(.x), # Ajustado
          min = ~safe_min(.x), # Ajustado
          range = ~safe_max(.x) - safe_min(.x) # Ajustado
        ),
        rh = list(
          mean = ~mean(.x, na.rm = TRUE),
          min = ~safe_min(.x), # Ajustado
          max = ~safe_max(.x)  # Ajustado
        ),
        sr = list(
          total = ~sum(.x, na.rm = TRUE),
          mean = ~mean(.x, na.rm = TRUE)
        ),
        default = list(
          mean = ~mean(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE),
          min = ~safe_min(.x), # Ajustado
          max = ~safe_max(.x)  # Ajustado
        )
      )
    }
  
  get_season <- function(date, definition = season_def, custom = custom_seasons) {
    month_day <- format(date, "%m-%d")
    month <- lubridate::month(date)
    
    if (definition == "brazil") {
      dplyr::case_when(
        month %in% c(12, 1, 2) ~ "Summer",
        month %in% c(3, 4, 5) ~ "Autumn",
        month %in% c(6, 7, 8) ~ "Winter",
        month %in% c(9, 10, 11) ~ "Spring",
        TRUE ~ NA_character_
      )
    } else if (definition == "official") {
      day_of_year <- lubridate::yday(date)
      dplyr::case_when(
        # Primavera: 22/09 a 21/12
        (day_of_year >= 265 & day_of_year <= 355) ~ "Spring",
        # Verao: 22/12 a 21/03
        day_of_year >= 356 | day_of_year <= 80 ~ "Summer",
        # Outono: 22/03 a 21/06
        day_of_year >= 81 & day_of_year <= 172 ~ "Autumn",
        # Inverno: 22/06 a 21/09
        day_of_year >= 173 & day_of_year <= 264 ~ "Winter",
        TRUE ~ NA_character_
      )
    } else if (definition == "custom" && !is.null(custom)) {
      season <- NA_character_
      for (season_name in names(custom)) {
        ranges <- custom[[season_name]]
        for (range in ranges) {
          if (month_day >= range[1] & month_day <= range[2]) {
            season <- season_name
            break
          }
        }
      }
      season
    } else {
      # Por trimestre como fallback
      dplyr::case_when(
        month %in% 1:3 ~ "Q1",
        month %in% 4:6 ~ "Q2",
        month %in% 7:9 ~ "Q3",
        month %in% 10:12 ~ "Q4",
        TRUE ~ NA_character_
      )
    }
  }
  
  get_var_agg_funs <- function(var_name) {
    for (pattern in names(agg_funs)) {
      if (grepl(pattern, var_name, ignore.case = TRUE) && pattern != "default") {
        return(agg_funs[[pattern]])
      }
    }

    return(agg_funs[["default"]])
  }
  
  climate_seasonal <- climate_data %>%
    dplyr::mutate(
      year = lubridate::year(.data$date),
      season = get_season(.data$date),
      season_year = paste(.data$year, .data$season, sep = "_")
    ) %>%
    dplyr::filter(!is.na(.data$season)) 
  

  agg_results <- list()
  
  for (var in target_vars) {
    if (!var %in% names(climate_seasonal)) next
    
    var_agg_funs <- get_var_agg_funs(var)
    
    var_agg <- climate_seasonal %>%
      dplyr::group_by(.data$code_muni, .data$year, .data$season, .data$season_year) %>%
      dplyr::summarise(
        n_days = dplyr::n(),
        n_valid = sum(!is.na(.data[[var]])),
        prop_valid = n_valid / n_days,
        
        dplyr::across(
          .cols = .data[[var]],
          .fns = var_agg_funs,
          .names = "{.fn}"
        ),
        .groups = 'drop'
      ) %>%
      
      dplyr::filter(
        n_days >= min_season_days,
        prop_valid >= (1 - na_prop_max)
      ) %>%
      
      dplyr::rename_with(
        ~ paste(var, ., sep = "_"),
        .cols = c(names(var_agg_funs), "n_days", "n_valid", "prop_valid")
      ) %>%
      
      dplyr::select(-dplyr::any_of(var))
    
    agg_results[[var]] <- var_agg
  }
  
  if (length(agg_results) > 0) {
    climate_agg <- agg_results[[1]]
    
    if (length(agg_results) > 1) {
      for (i in 2:length(agg_results)) {
        climate_agg <- climate_agg %>%
          dplyr::full_join(
            agg_results[[i]],
            by = c("code_muni", "year", "season", "season_year")
          )
      }
    }
  } else {
    stop("Nenhuma variavel pode ser agregada")
  }
  
  # Preparar dados de saude
  health_prepared <- health_data %>%
    dplyr::mutate(
      year = lubridate::year(.data$date),
      season = get_season(.data$date),
      season_year = paste(.data$year, .data$season, sep = "_")
    )
  
  # Juntar com dados climaticos agregados
  result <- health_prepared %>%
    dplyr::left_join(
      climate_agg,
      by = c("code_muni", "year", "season", "season_year")
    ) %>%
    
    # Remover colunas auxiliares se desejado
    dplyr::select(-.data$year, -.data$season, -.data$season_year)
    
  return(result)
}