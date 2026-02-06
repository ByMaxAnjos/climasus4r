#' Import, Harmonize, and Process INMET Meteorological Data with Epidemiological Intelligence
#'
#' @description
#' `sus_climate_inmet()` imports, standardizes, and processes Brazilian
#' meteorological data from INMET for environmental and epidemiological analysis.
#' 
#' The function implements:
#' - **Flexible temporal aggregation** (hour → season, including multi-period windows),
#' - **Optional spatial matching** to municipalities or other sf objects,
#' - **Optional gap-filling via machine learning (`sus_climate_fill_gaps()`)**, and
#' - **Parallel, cache-aware processing** for large multi-year datasets.
#'
#' This function is designed for:
#' - Climate–health studies (e.g., dengue, malaria, heat stress),
#' - Environmental exposure assessment,
#' - Time-series gap filling and preprocessing,
#' - Spatiotemporal integration of climate and health data.
#'
#' @param spatial_obj An `sf` object (e.g., output from `sus_join_spatial()`) for
#'   spatial matching. If NULL (default), returns processed meteorological data only.
#'
#' @param years Numeric vector of year(s) to filter (e.g., `c(2020, 2023)` or `2020:2025`).
#'   Must be between 2000 and the current year.
#'
#' @param uf Character vector of Brazilian state codes (e.g., `c("SP","RJ")`).
#'   If NULL, all states are included.
#'
#' @param time_unit Character specifying temporal aggregation.
#'   Options include:
#'   `"hour"`, `"day"` (default), `"week"`, `"month"`, `"quarter"`,
#'   `"year"`, `"season"` (DJF, MAM, JJA, SON), or multi-period formats such as
#'   `"2 days"`, `"5 days"`, `"14 days"`, `"3 months"`, `"6 months"`.
#' 
#' @param temporal_strategy Character. Strategy for temporal matching when
#'   `spatial_obj` is provided. Must be one of:
#'   \describe{
#'     \item{"exact"}{Exact date matching between health and climate data.}
#'     \item{"window"}{Aggregates climate variables within a symmetric window
#'       around each health date (± `window_days`).}
#'     \item{"lag"}{Creates lagged climate predictors based on `lag_days`.}
#'     \item{"seasonal"}{Matches climate data by climatological season (DJF, MAM,
#'       JJA, SON).}
#'   }
#'   Default: `"exact"`.
#'
#' @param window_days Integer vector or single integer. Only used when
#'   `temporal_strategy = "window"`. Defines the number of days before and after
#'   each health date to aggregate climate variables (e.g., `7` = ±7 days).
#'   If NULL and `temporal_strategy = "window"`, the function will error.
#'
#' @param lag_days Integer vector. Only used when `temporal_strategy = "lag"`.
#'   Defines temporal lags (in days) to create lagged climate variables
#'   (e.g., `c(0, 7, 14)`). If NULL and `temporal_strategy = "lag"`,
#'   the function will error.
#'
#' @param impute_missing Logical. If TRUE, fills missing values using
#'   `sus_climate_fill_gaps()`, which trains station-specific XGBoost models with
#'   temporal + meteorological predictors. Parallel processing is recommended. Imputation is performed on raw (high-frequency) data before aggregation.
#'
#' @param target_var Character vector of variable (only one at once) to impute when
#'   `impute_missing = TRUE`. Default: `"tair_dry_bulb_c"`. See variable list below.
#'
#' @param quality_threshold Numeric. Maximum allowed proportion of missing values
#'   per station/variable before skipping (default: 0.4). Stations are excluded if more than quality_threshold proportion of missing values exist for any target variable before imputation.
#'
#' @param use_cache Logical. If TRUE, uses Arrow/Parquet cache to speed up repeated
#'   processing of the same years/UFs.
#'
#' @param cache_dir Character. Directory for cached files.
#'   Default: `"~/.climasus4r_cache/climate"`.
#'
#' @param parallel Logical. If TRUE, processes year/UF combinations in parallel.
#'
#' @param workers Integer. Number of parallel workers (default = 4).
#'
#' @param lang Character. Message language: `"pt"`, `"en"`, or `"es"`.
#'
#' @param verbose Logical. If TRUE, prints progress messages.
#'
#' ## **Accepted & Standardized Meteorological Variables**
#'
#' The function **accepts** the following INMET variables
#' (suggested canonical names in parentheses):
#'
#' - `rainfall_mm` (precipitation, mm)
#' - `patm_mb` (mean atmospheric pressure, mb)
#' - `patm_max_mb`, `patm_min_mb` (pressure extremes)
#' - `sr_kj_m2` (solar radiation, kJ/m²)
#' - `tair_dry_bulb_c` (mean air temperature, °C)
#' - `tair_max_c`, `tair_min_c` (temperature extremes, °C)
#' - `dew_tmean_c`, `dew_tmax_c`, `dew_tmin_c` (dew point, °C)
#' - `rh_mean_porc`, `rh_max_porc`, `rh_min_porc` (relative humidity, %)
#' - `ws_2_m_s` (wind speed at 2m, m/s)
#' - `ws_gust_m_s` (wind gust, m/s)
#' - `wd_degrees` (wind direction, degrees)
#' 
#' ## **Missing Data Handling**
#' When `impute_missing = TRUE`, the function:
#' 1. Splits data by station
#' 2. Trains two XGBoost models per station and variable:
#'    - Model A: Temporal + meteorological predictors
#'    - Model B: Temporal predictors only (fallback)
#' 3. Uses Model A where meteorological covariates are available
#' 4. Falls back to Model B when they are missing
#' 5. Marks imputed values in an `is_imputed` column
#' 
#' ## **Spatial Matching** (if `spatial_obj` provided)
#' The function:
#' 1. Computes distances between municipality centroids and stations,
#' 2. Assigns the nearest valid station,
#' 3. Falls back to nearest station with a warning if no station meets quality criteria.
#'
#' ## **Temporal Aggregation**
#' Aggregation supports:
#' - Mean, sum, min, max depending on variable type,
#' - Smart handling of radiation and wind,
#' - Consistent alignment with epidemiological weeks when relevant.
#'
#' @return
#' A tibble with processed meteorological data containing:
#' - `date`: aggregated datetime,
#' - `station_code`, `station_name`, `latitude`, `longitude`,
#' - Standardized climate variables (list above),
#' - If `spatial_obj` provided: `code_mun`, `name_mun`, `distance_km`.
#'
#' @examples
#' \dontrun{
#' # Basic processing
#' climate_processed <- sus_climate_inmet(
#'   years = 2020:2021,
#'   time_unit = "day"
#' )
#'
#' # With spatial matching + gap filling
#' mun_sf <- sus_join_spatial(df_health)
#'
#' climate_with_spatial <- sus_climate_inmet(
#'   spatial_obj = mun_sf,
#'   years = 2020:2021,
#'   uf = c("SP","RJ"),
#'   time_unit = "week",
#'   target_vars = c("tair_dry_bulb_c", "rh_mean_porc", "ws_2_m_s"),
#'   impute_missing = TRUE,
#'   parallel = TRUE,
#'   workers = 4
#' )
#' }
#'
#' @export
#' @importFrom rlang .data
#' @importFrom data.table :=
#' 
sus_climate_inmet <- function(
    spatial_obj = NULL,
    years = 2022,
    uf = NULL,
    time_unit = "day",
    temporal_strategy = "exact",  
    window_days = NULL,            
    lag_days = NULL,               
    impute_missing = FALSE,
    target_var = "tair_dry_bulb_c",
    quality_threshold = 0.4,
    use_cache = TRUE,
    cache_dir = "~/.climasus4r_cache/climate",
    parallel = TRUE,
    workers = 4,
    lang = "pt",
    verbose = TRUE) {

  # ============================================================================
  # VALIDATION AND SETUP
  # ============================================================================
  rlang::check_installed(c("purrr"), 
                       reason = "to run sus_climate_fill_gaps()")  
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

  # Validate spatial_obj (Requirement 1: Optional spatial matching)
  if (!is.null(spatial_obj) && !inherits(spatial_obj, "sf")) {
    cli::cli_abort("{.arg spatial_obj} must be an sf object or NULL.")
  }

  if (!is.null(spatial_obj) && nrow(spatial_obj) == 0) {
    cli::cli_abort("{.arg spatial_obj} is empty (0 rows).")
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

  if (!is.numeric(quality_threshold) || length(quality_threshold) != 1) {
    cli::cli_abort("{.arg quality_threshold} must be a single numeric value between 0 and 1.")
  }

  if (is.na(quality_threshold) || quality_threshold < 0 || quality_threshold > 1) {
    cli::cli_abort(
      "{.arg quality_threshold} must be between 0 and 1. Received: {quality_threshold}"
    )
  }

  temporal_strategy <- match.arg(
    temporal_strategy,
    choices = c("exact", "window", "lag", "seasonal")
  )
  if (temporal_strategy == "window") {

  if (is.null(window_days)) {
      cli::cli_abort(
        "{.arg window_days} must be provided when {.arg temporal_strategy = 'window'}."
      )
    }

    if (!is.numeric(window_days) || anyNA(window_days)) {
      cli::cli_abort("{.arg window_days} must be numeric.")
    }

    window_days <- as.integer(window_days)

    if (any(window_days < 0)) {
      cli::cli_abort("{.arg window_days} must be non-negative integers.")
    }

    if (length(window_days) > 1) {
      cli::cli_alert_info(
        "Using the maximum value of {.arg window_days}: {max(window_days)}"
      )
      window_days <- max(window_days)
    }

  } else {
    # If not window strategy, silently ignore user input (best practice)
    window_days <- NULL
  }

  if (temporal_strategy == "lag") {
      if (is.null(lag_days)) {
        cli::cli_abort(
          "{.arg lag_days} must be provided when {.arg temporal_strategy = 'lag'}."
        )
      }

      if (!is.numeric(lag_days) || anyNA(lag_days)) {
        cli::cli_abort("{.arg lag_days} must be numeric.")
      }

      lag_days <- as.integer(lag_days)

      if (any(lag_days < 0)) {
        cli::cli_abort("{.arg lag_days} must be non-negative integers.")
      }

      # Sort for consistency in output and modeling
      lag_days <- sort(unique(lag_days))

    } else {
      lag_days <- NULL
    }
  # Initialize messages (Requirement 3: i18n)
  msg <- .get_messages(lang)

  # ==========================================================================
  # CONFIGURE CENSOBR CACHE
  # ==========================================================================
  if (use_cache) {
    cache_dir <- path.expand(cache_dir)
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    }

    if (verbose) {
      cli::cli_alert_info(glue::glue(
        msg$configuring_cache,
        dir = cache_dir
      ))
    }
  }

  # ============================================================================
  # DOWNLAOD AND CHACE
  # ============================================================================
  
  climate_data <- .download_and_cache_inmet(
    years = years,
    uf = uf,
    cache_dir = cache_dir,
    use_cache = TRUE,
    parallel = parallel,
    workers = workers,
    verbose = verbose,
    lang = lang
  )
  # ============================================================================
  # MISSING DATA IMPUTATION (Parallel processing)
  # ============================================================================

  if (impute_missing) {
    if (verbose) cli::cli_progress_step(msg$imputing)

    # Apply imputation
    climate_data <- sus_climate_fill_gaps(
      climate_data,
      target_var = target_var,
      quality_threshold = quality_threshold,
      run_evaluation = TRUE
    )

  }

  # ============================================================================
  # TEMPORAL AGGREGATION
  # ============================================================================

  if (verbose) {
    cli::cli_progress_step(msg$aggregating)
  }

  tryCatch(
    {
      climate_data_agg <- .aggregate_meteo_data(
        climate_data,
        time_unit = time_unit
      )
    },
    error = function(e) {
      cli::cli_abort(paste0(
        "Invalid time_unit: '",
        time_unit,
        "'. ",
        "Use formats like 'day', 'week', 'month', 'quarter', 'year', 'season', ",
        "or multi-period like '2 days', '5 days', '14 days', '3 months', etc. The '2 weeks' period does not work in this version "
      ))
    }
  )


  # ============================================================================
  # SPATIAL and TEMPORAL MATCHING 
  # ============================================================================

  if (!is.null(spatial_obj)) {
    
    #SPATIAL
    if (verbose) cli::cli_progress_step(msg$spatial_match)
    
    if (parallel) {  
      climate_data_agg <- .match_spatial_with_parallel(
      df = climate_data_agg,
      spatial_obj = spatial_obj,
      msg = msg,
      verbose = verbose,
      quality_vars = target_vars,
      quality_method = "any", #c("any", "all", "mean")
      quality_threshold = quality_threshold
    )
    } else { 
      climate_data_agg <- .match_spatial(
      df = climate_data_agg,
      spatial_obj = spatial_obj,
      msg = msg,
      verbose = verbose,
      quality_vars = target_vars,
      quality_method = "any",
      quality_threshold = quality_threshold
    )
    }
    
    # FASE: Matching Temporal
    if (verbose) cli::cli_h2("Fase 2: Matching Temporal")
    
    climate_macth_temporal <- switch(
      temporal_strategy,
      exact = .join_exact(
        health_data = spatial_obj,
        climate_data = climate_data_agg,
        target_vars = target_vars
      ),
      window = .join_window(
        health_data = spatial_obj,
        climate_data = climate_data_agg,
        target_vars = target_vars,
        window_days = window_days
      ),
      lag = .join_lag(
        health_data = spatial_obj,
        climate_data = climate_data_agg,
        target_vars = target_vars,
        lag_days = lag_days
      ),
      seasonal = .join_seasonal(
        health_data = spatial_obj,
        climate_data = climate_data_agg,
        target_vars = target_vars
      )
    )
  }

  if (verbose) cli::cli_progress_done()

  # ===========================================================
  # S3 CLASS + METADATA
  # ===========================================================
  if (!inherits(climate_data_agg, "climasus_df")) {
    # Create new climasus_df
    meta <- list(
      system = NULL,
      stage = "climate",
      type = "inmet",
      spatial = inherits(climate_data_agg, "sf"),
      temporal = NULL,
      created = Sys.time(),
      modified = Sys.time(),
      history = sprintf(
        "[%s] INMET data Imported",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ),
      user = list()
    )

    base_classes <- setdiff(class(climate_data_agg), "climasus_df")
    climate_data_agg <- structure(
      climate_data_agg,
      climasus_meta = meta,
      class = c("climasus_df", base_classes)
    )
  } else {
    # Already climasus_df - update metadata
    climate_data_agg <- climasus_meta(
      climate_data_agg,
      system = NULL,
      stage = "climate",
      type = "inmet",
      temporal = list(
        start = min(climate_data_agg$date),
        end = max(climate_data_agg$date),
        imputed = impute_missing,
        source = "inmet"
      )
    )
    climate_data_agg <- climasus_meta(
      climate_data_agg,
      add_history = "INMET data Imported"
    )
  }

  return(dplyr::as_tibble(climate_data_agg))
}