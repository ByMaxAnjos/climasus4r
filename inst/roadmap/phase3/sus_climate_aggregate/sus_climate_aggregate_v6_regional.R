#' Integration of Climate and Health Data 
#'
#' @description
#' `sus_climate_aggregate()` aggregates meteorological data to DATASUS health data
#' using epidemiologically temporal strategies **adapted to Brazil's diverse
#' regional climates**. The function automatically detects or accepts regional climate
#' classification (tropical, subtropical, temperate) and adjusts parameters accordingly.
#'
#' @param health_data A `climasus_df` object produced by `sus_join_spatial()`.
#' @param climate_data A `climasus_df` object produced by  `sus_climate_inmet()` and `sus_climate_fill_gaps()`.
#' @param climate_var Character vector with climate columns to aggregate.
#'   Use `"all"` (default) to include all available variables.
#' @param time_unit Temporal aggregation unit. Options: `"day"` (default), `"week"`,
#'   `"month"`, `"quarter"`, `"year"`, `"season"`. It includes options suchb as `" 5 days"`, `" 14 days"`, `"2 months"`.
#' @param temporal_strategy Temporal matching strategy. Options:
#'   \describe{
#'     \item{`"exact"`}{Exact date match.}
#'     \item{`"discrete_lag"`}{Climate value exactly \eqn{t - L} days before event.}
#'     \item{`"moving_window"`}{Mean/sum of sliding window \eqn{[t - W, t]}.
#'       **RECOMMENDED** for all regions.}
#'     \item{`"offset_window"`}{Historical interval \eqn{[t - W_2, t - W_1]}.}
#'     \item{`"distributed_lag"`}{Lag matrix 0 to L for DLNM modeling.}
#'     \item{`"degree_days"`}{Growing Degree Days (GDD) for thermal stress.
#'       **Note:** Uses region-specific `temp_base` by default.}
#'     \item{`"seasonal"`}{Match by climate season (DJF, MAM, JJA, SON).
#'       **Note:** Adapted to regional seasonality.}
#'     \item{`"threshold_exceedance"`}{Counts days exceeding threshold.
#'       **Note:** Uses region-specific heatwave and rainfull thresholds}
#'     \item{`"cold_wave_exceedance"`}{**Note:** Counts days below threshold.
#'       Ideal for cold extremes in southern regions. Produces columns
#'       `ncold{W}_lt{threshold}_`.}
#'     \item{`"weighted_window"`}{Weighted mean with decay function.}
#'   }
#' @param climate_region Character. Climate classification for parameter adaptation.
#'   Options:
#'   \describe{
#'     \item{`"auto"`}{Automatically detects based on latitude (default).}
#'     \item{`"tropical"`}{North, Northeast (Amazônia, Nordeste). Tbase=20°C.}
#'     \item{`"subtropical"`}{Center-West, Southeast (Centro-Oeste, Sudeste). Tbase=18°C.}
#'     \item{`"temperate"`}{South (Sul). Tbase=15°C. Includes cold wave analysis.}
#'   }
#'   When `"auto"`, detection uses: latitude > -5° → tropical; -5° to -20° → subtropical;
#'   < -20° → temperate.
#' @param window_days Integer. Number of days in window for applicable strategies.
#' @param lag_days Integer vector. Specific lags for `discrete_lag` or maximum lag
#'   for `distributed_lag`.
#' @param offset_days Integer vector of length 2 for `offset_window`.
#' @param temp_base Numeric. Temperature base for `degree_days`. If `NULL` (default),
#'   uses region-specific default: 20°C (tropical), 18°C (subtropical), 15°C (temperate).
#'   Override to use custom value (e.g., 11°C for *Aedes aegypti*).
#' @param gdd_temp_var Character. Temperature column for `degree_days`.
#'   Default: `"tair_dry_bulb_c"`.
#' @param min_obs Numeric (0–1). Minimum proportion of valid observations required.
#'   Default: 0.7 (70%).
#' @param threshold_value Numeric. Threshold for `threshold_exceedance` or
#'   `cold_wave_exceedance`. If `NULL` for `threshold_exceedance`, uses region-specific
#'   default: 35°C (tropical), 32°C (subtropical), 30°C (temperate).
#' @param threshold_direction Character: `"above"` (default) or `"below"`.
#'   For `cold_wave_exceedance`, automatically set to `"below"`.
#' @param weights Numeric vector (optional) for `weighted_window`.
#' @param use_cache Logical. If `TRUE`, uses Arrow/Parquet cache. Default: `TRUE`.
#' @param cache_dir Character. Directory for cache files.
#' @param lang Language for messages: `"pt"` (Portuguese), `"en"` (English), or
#'   `"es"` (Spanish). Default: `"pt"`.
#' @param verbose Logical. If `TRUE`, displays progress messages. Default: `TRUE`.
#' @param warn_extreme_year Logical. If `TRUE`, warns about extreme climate years.
#'   Default: `TRUE`.
#' @param warn_regional_mismatch Logical. If `TRUE`, warns when parameters may be
#'   inappropriate for detected region. Default: `TRUE`.
#'
#' @return A `climasus_df` (tibble) with original health data and integrated climate
#'   variables as new columns. Geometry is preserved if input is `sf`.
#'
#' @section Regional Climate Considerations:
#' 
#' The function automatically adapts to Brazil's diverse climates:
#' 
#' **Tropical (North, Northeast):** Amazônia, Nordeste
#' - Mean temperature: 24-28°C
#' - Recommended temp_base: 20°C (human health), 11°C (vector biology)
#' - Heatwave threshold: Tmax > 35°C
#' - Seasonal pattern: Dry season (May-Sep) vs. wet season (Nov-Mar)
#' - High autocorrelation: Use DLNM or limit lags
#' - Recommended strategies: `moving_window`, `seasonal`, `threshold_exceedance`
#'
#' **Subtropical (Center-West, Southeast):** Centro-Oeste, Sudeste
#' - Mean temperature: 18-26°C
#' - Recommended temp_base: 18°C (human health), 11°C (vector biology)
#' - Heatwave threshold: Tmax > 32°C
#' - Seasonal pattern: Moderate variation
#' - Moderate autocorrelation: Standard lag analysis acceptable
#' - Recommended strategies: `moving_window`, `threshold_exceedance`
#'
#' **Temperate (South):** Sul
#' - Mean temperature: 12-24°C
#' - Recommended temp_base: 15°C (human health), 11°C (vector biology)
#' - Heatwave threshold: Tmax > 30°C
#' - **Coldwave threshold: Tmin < 5°C** (CRITICAL for health)
#' - Seasonal pattern: Strong winter/summer contrast
#' - Low autocorrelation: Lag analysis robust
#' - Recommended strategies: `moving_window`, `cold_wave_exceedance`
#'
#' @section New Strategy: cold_wave_exceedance
#'
#' Counts days within window \eqn{[t - W, t]} when climate variable falls below
#' threshold. Ideal for cold extremes in southern regions.
#'
#' **Example:**
#' ```
#' df_coldwave <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial,
#'   climate_data = df_inmet,
#'   climate_region = "temperate",
#'   temporal_strategy = "cold_wave_exceedance",
#'   window_days = 7,
#'   threshold_value = 5  # Tmin < 5°C
#' )
#' ```
#'
#' Produces columns: `ncold7_lt5_tair_min_c`, `pcold7_lt5_tair_min_c`
#' (count and proportion of days with Tmin < 5°C in last 7 days).
#'
#' @examples
#' \dontrun{
#' # Example 1: Automatic regional detection (Amazônia)
#' df_amazon <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial,
#'   climate_data = df_inmet,
#'   climate_region = "auto",  # Detects as "tropical"
#'   temporal_strategy = "moving_window",
#'   window_days = 14
#' )
#'
#' # Example 2: Explicit subtropical region (São Paulo)
#' df_southeast <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial_sp,
#'   climate_data = df_inmet_sp,
#'   climate_region = "subtropical",
#'   temporal_strategy = "threshold_exceedance",
#'   window_days = 7,
#'   threshold_value = 32  # Heatwave for Southeast
#' )
#'
#' # Example 3: Cold waves in southern region
#' df_coldwave <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial_south,
#'   climate_data = df_inmet_south,
#'   climate_region = "temperate",
#'   temporal_strategy = "cold_wave_exceedance",
#'   window_days = 7,
#'   threshold_value = 5
#' )
#'
#' # Example 4: Degree-days with region-specific temp_base
#' df_gdd <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial,
#'   climate_data = df_inmet,
#'   climate_region = "subtropical",  # Uses temp_base = 18°C automatically
#'   temporal_strategy = "degree_days",
#'   window_days = 21
#' )
#' }
#'
#' @export
sus_climate_aggregate <- function(
    health_data,
    climate_data,
    climate_var       = "all",
    time_unit         = "day",
    temporal_strategy = "exact",
    climate_region    = "auto",  # NEW: regional adaptation
    window_days       = NULL,
    lag_days          = NULL,
    offset_days       = NULL,
    temp_base            = NULL,  # CHANGED: NULL uses region-specific default
    gdd_temp_var      = "tair_dry_bulb_c",
    min_obs           = 0.7,
    threshold_value     = NULL,  # CHANGED: NULL uses region-specific default
    threshold_direction = "above",
    weights             = NULL,
    use_cache           = TRUE,
    cache_dir         = "~/.climasus4r_cache/climate",
    lang              = "pt",
    verbose           = TRUE,
    warn_extreme_year = TRUE,
    warn_regional_mismatch = TRUE  # NEW: warn about regional mismatches
) {

  msg <- .get_messages(lang)

  # ---------------------------------------------------------------------------
  # 1. VALIDATION OF HEALTH DATA
  # ---------------------------------------------------------------------------
  .validate_health_input(health_data, lang, verbose)
  system <- climasus_meta(health_data, "system")

  # ---------------------------------------------------------------------------
  # 2. VALIDATION OF CLIMATE DATA
  # ---------------------------------------------------------------------------
  .validate_climate_input(climate_data, lang, verbose)

  # ---------------------------------------------------------------------------
  # 3. NEW: DETECT OR VALIDATE CLIMATE REGION
  # ---------------------------------------------------------------------------
  climate_region <- .detect_climate_region(climate_data, climate_region, verbose, lang)
  
  if (verbose) {
    cli::cli_alert_info("Climate region: {climate_region}")
  }

  # ---------------------------------------------------------------------------
  # 4. NEW: GET REGION-SPECIFIC DEFAULTS
  # ---------------------------------------------------------------------------
  region_defaults <- .get_region_defaults(climate_region)
  
  # Apply region-specific defaults if user didn't specify
  if (is.null(temp_base)) {
    temp_base <- region_defaults$temp_base_health
    if (verbose) {
      cli::cli_alert_info("Using region-specific temp_base = {temp_base}°C for human health")
    }
  }
  
  if (is.null(threshold_value) && temporal_strategy %in% c("threshold_exceedance", "cold_wave_exceedance")) {
    threshold_value <- region_defaults$heatwave_threshold
    if (verbose && temporal_strategy == "threshold_exceedance") {
      cli::cli_alert_info("Using region-specific heatwave threshold = {threshold_value}°C")
    }
  }

  # ---------------------------------------------------------------------------
  # 5. VALIDATION OF TEMPORAL OVERLAP
  # ---------------------------------------------------------------------------
  .validate_date_overlap(health_data, climate_data, lang)

  # ---------------------------------------------------------------------------
  # 6. CHECK FOR EXTREME CLIMATE YEARS
  # ---------------------------------------------------------------------------
  if (warn_extreme_year) {
    .warn_extreme_climate_year(climate_data, lang, verbose)
  }

  # ---------------------------------------------------------------------------
  # 7. VALIDATION OF CLIMATE VARIABLES
  # ---------------------------------------------------------------------------
  climate_var <- .validate_climate_var(climate_data, climate_var, lang)

  # ---------------------------------------------------------------------------
  # 8. VALIDATION OF STRATEGY AND PARAMETERS
  # ---------------------------------------------------------------------------
  temporal_strategy <- match.arg(
    temporal_strategy,
    choices = c(
      "exact", "discrete_lag", "moving_window",
      "offset_window", "distributed_lag", "degree_days", "seasonal",
      "threshold_exceedance", "cold_wave_exceedance",  # NEW
      "weighted_window"
    )
  )

  window_days  <- .validate_window_days(temporal_strategy, window_days, lang)
  lag_days     <- .validate_lag_days(temporal_strategy, lag_days, lang)
  offset_days  <- .validate_offset_days(temporal_strategy, offset_days, lang)
  .validate_degree_days_params(temporal_strategy, climate_var, gdd_temp_var, temp_base, lang)
  .validate_threshold_params(temporal_strategy, threshold_value, threshold_direction, lang)
  weights <- .validate_weights(temporal_strategy, window_days, weights, lang)

  # ---------------------------------------------------------------------------
  # 9. NEW: VALIDATE REGIONAL APPROPRIATENESS
  # ---------------------------------------------------------------------------
  if (warn_regional_mismatch) {
    .validate_regional_appropriateness(
      temporal_strategy, climate_region, temp_base, threshold_value, 
      region_defaults, verbose, lang
    )
  }

  # ---------------------------------------------------------------------------
  # 10. PRE-AGGREGATION (hour -> day, if necessary)
  # ---------------------------------------------------------------------------
  if (verbose) cli::cli_progress_step(msg$aggregating)

  climate_temporal_meta <- climasus_meta(climate_data, "temporal")
  is_hourly_meta <- !is.null(climate_temporal_meta$unit) &&
                    climate_temporal_meta$unit == "hour"

  is_hourly_structural <- FALSE
  if (!is_hourly_meta) {
    n_rows                <- nrow(climate_data)
    n_unique_dates        <- dplyr::n_distinct(as.Date(climate_data$date))
    n_stations            <- dplyr::n_distinct(climate_data$station_code)
    rows_per_station_day  <- n_rows / max(n_unique_dates * n_stations, 1)
    is_hourly_structural  <- rows_per_station_day > 2
    if (is_hourly_structural && verbose) {
      cli::cli_alert_info(paste0(
        "Temporal metadata absent. Sub-daily resolution detected. ",
        "Aggregating to daily resolution before join."
      ))
    }
  }

  if (is_hourly_meta || is_hourly_structural) {
    climate_data <- .aggregate_meteo_data(climate_data, time_unit = "day")
  }
  if (!identical(time_unit, "day")) {
    climate_data <- .aggregate_meteo_data(climate_data, time_unit = time_unit)
  }

  climate_data <- dplyr::mutate(climate_data, date = as.Date(.data$date))

  if (verbose) cli::cli_progress_done()

  # ---------------------------------------------------------------------------
  # 11. SPATIAL METEO STATION MATCHING
  # ---------------------------------------------------------------------------
  
  if (verbose) cli::cli_progress_step(msg$spatial_match)

  climate_matched <- .match_spatial(
    df          = climate_data,
    spatial_obj = health_data,
    verbose     = verbose
  )

  if (verbose) cli::cli_progress_done()

  # ---------------------------------------------------------------------------
  # 12. TEMPORAL JOIN
  # ---------------------------------------------------------------------------
  if (verbose) cli::cli_progress_step(msg$temporal_join)

  df <- switch(
    temporal_strategy,
    "exact"          = .join_exact(health_data, climate_matched, climate_var),
    "discrete_lag"   = .join_discrete_lag(health_data, climate_matched, climate_var, lag_days),
    "moving_window"  = .join_moving_window(health_data, climate_matched, climate_var, window_days, min_obs),
    "offset_window"  = .join_offset_window(health_data, climate_matched, climate_var, offset_days, min_obs),
    "distributed_lag"= .join_distributed_lag(health_data, climate_matched, climate_var, lag_days),
    "degree_days"    = .join_degree_days(health_data, climate_matched, window_days, gdd_temp_var, temp_base, min_obs),
    "seasonal"       = .join_seasonal(health_data, climate_matched, climate_var),
    "threshold_exceedance" = .join_threshold_exceedance(
                               health_data, climate_matched, climate_var,
                               window_days, threshold_value, threshold_direction, min_obs),
    "cold_wave_exceedance" = .join_cold_wave_exceedance(  # NEW
                               health_data, climate_matched, climate_var,
                               window_days, threshold_value, min_obs),
    "weighted_window"      = .join_weighted_window(
                               health_data, climate_matched, climate_var,
                               window_days, weights, min_obs)
  )

  if (verbose) cli::cli_progress_done()

  # ---------------------------------------------------------------------------
  # 13. METADATA UPDATE
  # ---------------------------------------------------------------------------
  df <- .set_climate_agg_meta(df, system)

  return(df)
}


# =============================================================================
# NEW HELPER FUNCTIONS FOR REGIONAL ADAPTATION
# =============================================================================

#' NEW: Detect climate region based on latitude
#' @keywords internal
#' @noRd
.detect_climate_region <- function(climate_data, climate_region, verbose, lang) {
  if (climate_region != "auto") {
    # Validate specified region
    if (!climate_region %in% c("tropical", "subtropical", "temperate")) {
      cli::cli_abort(
        "{.arg climate_region} must be 'auto', 'tropical', 'subtropical', or 'temperate'."
      )
    }
    return(climate_region)
  }
  
  # Auto-detect based on latitude
  mean_lat <- mean(climate_data$latitude, na.rm = TRUE)
  
  if (mean_lat > -5) {
    detected <- "tropical"
  } else if (mean_lat > -20) {
    detected <- "subtropical"
  } else {
    detected <- "temperate"
  }
  
  if (verbose) {
    cli::cli_alert_info(
      "Auto-detecting climate region based on latitude ({round(mean_lat, 1)}°): {detected}"
    )
  }
  
  detected
}


#' NEW: Get region-specific defaults
#' @keywords internal
#' @noRd
.get_region_defaults <- function(climate_region) {
  defaults <- list(
    tropical = list(
      temp_base_health = 20,
      temp_base_vector = 11,
      heatwave_threshold = 35,
      coldwave_threshold = NA,
      description = "Tropical (North, Northeast)"
    ),
    subtropical = list(
      temp_base_health = 18,
      temp_base_vector = 11,
      heatwave_threshold = 32,
      coldwave_threshold = 10,
      description = "Subtropical (Center-West, Southeast)"
    ),
    temperate = list(
      temp_base_health = 15,
      temp_base_vector = 11,
      heatwave_threshold = 30,
      coldwave_threshold = 5,
      description = "Temperate (South)"
    )
  )
  
  defaults[[climate_region]]
}


#' NEW: Validate regional appropriateness of parameters
#' @keywords internal
#' @noRd
.validate_regional_appropriateness <- function(
    temporal_strategy, climate_region, temp_base, threshold_value,
    region_defaults, verbose, lang
) {
  if (temporal_strategy == "degree_days") {
    recommended_base <- region_defaults$temp_base_health
    
    if (abs(temp_base - recommended_base) > 3) {
      cli::cli_alert_warning(
        "temp_base = {temp_base}°C may be inappropriate for {climate_region} region. ",
        "Recommended: {recommended_base}°C. ",
        "Proceed with caution or override with explicit justification."
      )
    }
  }
  
  if (temporal_strategy == "threshold_exceedance") {
    recommended_threshold <- region_defaults$heatwave_threshold
    
    if (abs(threshold_value - recommended_threshold) > 3) {
      cli::cli_alert_warning(
        "Heatwave threshold = {threshold_value}°C may be inappropriate for {climate_region} region. ",
        "Recommended: {recommended_threshold}°C."
      )
    }
  }
  
  if (temporal_strategy == "cold_wave_exceedance" && is.na(region_defaults$coldwave_threshold)) {
    cli::cli_alert_warning(
      "cold_wave_exceedance strategy not recommended for {climate_region} region. ",
      "Cold extremes are not a major health concern in this region."
    )
  }
}


#' NEW: Join for cold wave exceedance (counts days below threshold)
#'
#' @description
#' Similar to `threshold_exceedance`, but counts days when climate variable
#' falls BELOW threshold. Ideal for cold extremes in southern regions.
#'
#' @keywords internal
#' @noRd
.join_cold_wave_exceedance <- function(
    health_data, climate_data, climate_var,
    window_days, threshold_value, min_obs
) {
  window_size    <- window_days + 1L
  health_id_vars <- setdiff(names(health_data), "geom")

  # For each climate variable, count days below threshold
  result_list <- list()

  for (var in climate_var) {
    if (!var %in% names(climate_data)) {
      cli::cli_warn("Variable '{var}' not found in climate data. Skipping.")
      next
    }

    climate_dt <- data.table::as.data.table(
      dplyr::select(climate_data, "code_muni", "date", value = dplyr::all_of(var))
    )
    climate_dt[, date_int  := as.integer(date)]
    climate_dt[, date_int2 := date_int]
    climate_dt[, date := NULL]

    health_dt <- data.table::as.data.table(
      dplyr::select(sf::st_drop_geometry(health_data),
                    dplyr::all_of(health_id_vars))
    )
    health_dt[, .row_id        := .I]
    health_dt[, .win_start_int := as.integer(date) - window_days]
    health_dt[, .win_end_int   := as.integer(date)]

    data.table::setkeyv(climate_dt, c("code_muni", "date_int", "date_int2"))
    data.table::setkeyv(health_dt,  c("code_muni", ".win_start_int", ".win_end_int"))

    joined <- data.table::foverlaps(
      health_dt, climate_dt,
      by.x    = c("code_muni", ".win_start_int", ".win_end_int"),
      by.y    = c("code_muni", "date_int",        "date_int2"),
      type    = "any",
      mult    = "all",
      nomatch = NA
    )

    id_plus_row <- c(health_id_vars, ".row_id")
    col_n_name  <- paste0("ncold", window_days, "_lt", threshold_value, "_", var)
    col_p_name  <- paste0("pcold", window_days, "_lt", threshold_value, "_", var)

    result <- joined[
      ,
      .(
        n_obs    = sum(!is.na(value)),
        prop_obs = sum(!is.na(value)) / window_size,
        n_below  = sum(value < threshold_value, na.rm = TRUE),
        p_below  = sum(value < threshold_value, na.rm = TRUE) / sum(!is.na(value))
      ),
      by = id_plus_row
    ]

    result[prop_obs < min_obs, c(col_n_name, col_p_name) := NA_real_]
    result[prop_obs >= min_obs, c(col_n_name, col_p_name) := .(n_below, p_below)]
    result[, c("n_obs", "prop_obs", "n_below", "p_below", ".row_id") := NULL]

    result_list[[var]] <- result
  }

  # Combine all results
  if (length(result_list) == 0) {
    cli::cli_abort("No valid climate variables for cold_wave_exceedance.")
  }

  result_combined <- Reduce(function(x, y) merge(x, y, by = health_id_vars, all = TRUE),
                            result_list)

  tibble::as_tibble(result_combined)
}

