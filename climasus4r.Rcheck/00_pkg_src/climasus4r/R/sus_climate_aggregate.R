#' Integration of Climate and Health Data
#'
#' @description
#' `sus_climate_aggregate()` aggregates meteorological data to DATASUS health data
#' using epidemiologically rigorous temporal strategies. The function links each
#' health record to the nearest climate station (by Euclidean distance) and applies
#' the requested temporal window.
#'
#' @param health_data A `climasus_df` object produced by `sus_spatial_join()`.
#'   Must contain columns `date` (Date), `code_muni` (character), and geometry
#'   column `geom`.
#' @param climate_data A `climasus_df` object produced by `sus_climate_fill_inmet()`.
#'   Must contain `date` (Date or POSIXct), `station_code`, `latitude`, `longitude`,
#'   and climate variables.
#' @param climate_var Character vector with climate variables to aggregate.
#'   Use `"all"` (default) to include all available variables.
#'
#'   Available variables are grouped as follows:
#'
#'   **Atmospheric pressure:**
#'   - `"patm_mb"`, `"patm_max_mb"`, `"patm_min_mb"`
#'
#'   **Air temperature:**
#'   - `"tair_dry_bulb_c"`, `"tair_max_c"`, `"tair_min_c"`
#'
#'   **Dew point temperature:**
#'   - `"dew_tmean_c"`, `"dew_tmax_c"`, `"dew_tmin_c"`
#'
#'   **Relative humidity:**
#'   - `"rh_mean_porc"`, `"rh_max_porc"`, `"rh_min_porc"`
#'
#'   **Precipitation:**
#'   - `"rainfall_mm"`
#'
#'   **Wind:**
#'   - `"ws_2_m_s"` (mean wind speed),
#'   - `"ws_gust_m_s"` (wind gust),
#'   - `"wd_degrees"` (wind direction)
#'
#'   **Solar radiation:**
#'   - `"sr_kj_m2"`
#'
#'   **Biometeorological indices:**
#'   - `"wbgt_c"` (Wet Bulb Globe Temperature)
#'   - `"hi_c"` (Heat Index)
#'   - `"thi_c"` (Temperature-Humidity Index)
#'   - `"wcet_c"` (Wind Chill Equivalent Temperature)
#'   - `"wct_c"` (Wind Chill Temperature)
#'   - `"et_c"` (Effective Temperature)
#'   - `"utci_c"` (Universal Thermal Climate Index)
#'   - `"pet_c"` (Physiological Equivalent Temperature)
#'
#'   **Thermal indices (degree-days):**
#'   - `"cdd_c"` (Cooling Degree Days)
#'   - `"hdd_c"` (Heating Degree Days)
#'   - `"gdd_c"` (Growing Degree Days)
#'
#'   **Derived variables:**
#'   - `"diurnal_range_c"` (daily temperature range)
#'   - `"vapor_pressure_kpa"` (saturation vapor pressure)
#'
#'   @section Notes:
#'   - Not all variables may be available in `climate_data`. Use `names(climate_data)`
#'     to inspect available columns.
#'   - When `"all"` is used, only existing variables in the dataset are selected.
#'   - Derived and index variables depend on prior processing with
#'     `sus_climate_fill_inmet()` or feature engineering steps.
#' @param time_unit Temporal aggregation unit for raw climate data before join.
#'   Options: `"day"` (default), `"week"`, `"month"`, `"quarter"`, `"year"`,
#'   `"season"`. Relevant only when input data are hourly resolution.
#' @param temporal_strategy Temporal matching strategy. Options:
#'   \describe{
#'     \item{`"exact"`}{Exact date match (same-day temperature for acute heat-related mortality).}
#'     \item{`"discrete_lag"`}{Climate value exactly L days before event.}
#'     \item{`"moving_window"`}{Mean/sum of sliding window (t-W, t). **RECOMMENDED** for cumulative exposure.}
#'     \item{`"offset_window"`}{Aggregates historical interval (t-W2, t-W1), ignoring recent days.}
#'     \item{`"distributed_lag"`}{Creates lag matrix 0 to L for DLNM modeling.}
#'     \item{`"degree_days"`}{Calculates Growing Degree Days (GDD) for thermal stress.}
#'     \item{`"seasonal"`}{Aggregates by climate season (DJF, MAM, JJA, SON).}
#'     \item{`"threshold_exceedance"`}{Counts days exceeding threshold (e.g., heatwaves).}
#'     \item{`"cold_wave_exceedance"`}{Counts days below threshold (e.g., cold extremes in southern regions).}
#'     \item{`"weighted_window"`}{Weighted mean with decay function.}
#'   }
#' @param climate_region Character. Climate classification for parameter adaptation.
#'   Options: `"auto"` (default, auto-detect by latitude), `"tropical"` (North, Northeast),
#'   `"subtropical"` (Center-West, Southeast), `"temperate"` (South).
#' @param window_days Integer. Number of days in window for `moving_window`,
#'   `offset_window`, `degree_days`, `threshold_exceedance`, `cold_wave_exceedance`,
#'   or `weighted_window`. Required for these strategies.
#' @param lag_days Integer vector. Specific lags for `discrete_lag` or maximum lag
#'   for `distributed_lag`. Required for these strategies.
#' @param offset_days Integer vector of length 2 for `offset_window`.
#'   Defines historical interval: `c(W1, W2)` aggregates from t-W2 to t-W1.
#' @param temp_base Numeric. Temperature base for `degree_days` calculation.
#'   If `NULL` (default), uses region-specific default: 20degC (tropical), 18degC (subtropical),
#'   15degC (temperate). Use 11 degC for *Aedes aegypti* development, 10degC for *Plasmodium*.
#' @param gdd_temp_var Character. Temperature column for `degree_days`.
#'   Default: `"tair_dry_bulb_c"`.
#' @param min_obs Numeric (0 to 1). Minimum proportion of valid observations required
#'   within window. Default: 0.7 (70%).
#' @param threshold_value Numeric. Threshold for `threshold_exceedance` or
#'   `cold_wave_exceedance`. If `NULL`, uses region-specific default.
#' @param threshold_direction Character: `"above"` (default, > threshold) or
#'   `"below"` (< threshold). For `cold_wave_exceedance`, automatically set to `"below"`.
#' @param weights Numeric vector (optional) for `weighted_window`. Defines weight
#'   for each day within window, from most recent (position 1) to oldest (position W+1).
#'   If `NULL`, linear decreasing weights are generated automatically.
#' @param lang Language for messages: `"pt"` (Portuguese), `"en"` (English), or
#'   `"es"` (Spanish). Default: `"pt"`.
#' @param verbose Logical. If `TRUE`, displays progress messages. Default: `TRUE`.
#'
#' @return A `climasus_df` (tibble) with original health data and integrated climate
#'   variables as new columns. Geometry is preserved if input is `sf`.
#'
#' @details
#' ## Temporal Strategies Epidemiological Foundations
#'
#' The choice of strategy should reflect the hypothesized biological mechanism:
#'
#' - **exact**: Immediate effects (heat stroke, hemorrhagic stroke)
#' - **discrete_lag**: Known delayed effect (e.g., temperature 7 days before influences dengue)
#' - **moving_window**: Cumulative exposure without specific lag.
#' - **offset_window**: Defined incubation period (e.g., temperature 14-7 days before death)
#' - **distributed_lag**: Distributed lag analysis (DLNM); generates exposure matrix for `dlnm::crossbasis()`
#' - **degree_days**: Thermal threshold for vector development. GDD above temperature base accumulates over window_days.
#' - **seasonal**: Seasonal climate patterns for ecological or long-term studies
#' - **threshold_exceedance**: Counts days exceeding threshold (ideal for heatwaves)
#' - **cold_wave_exceedance**: Counts days below threshold (ideal for cold extremes in southern regions)
#' - **weighted_window**: Weighted mean modeling biological decay of exposure
#'
#' ## Regional Climate Considerations
#'
#' The function automatically adapts to Brazil's diverse climates:
#'
#' **Tropical (North, Northeast):** Mean temperature 24-28degC, recommended temp_base 20degC (human health),
#' heatwave threshold Tmax > 35degC, high autocorrelation.
#'
#' **Subtropical (Center-West, Southeast):** Mean temperature 18-26degC, recommended temp_base 18degC,
#' heatwave threshold Tmax > 32degC, moderate autocorrelation.
#'
#' **Temperate (South):** Mean temperature 12-24degC, recommended temp_base 15degC,
#' heatwave threshold Tmax > 30degC, **coldwave threshold Tmin < 5degC** (critical),
#' low autocorrelation.
#'
#' ## Causal Inference & Look-Ahead Bias
#'
#' This function uses **retroactive windows** (t-W, t), never symmetric windows (t-W, t+W).
#' The climate of day t+7 cannot cause a health event on day t. This design prevents
#' look-ahead bias, a common methodological error in environmental epidemiology.
#'
#'
#' @references
#' Gasparrini, A. (2011). Distributed lag linear and non-linear models in R: the package dlnm.
#' *Journal of Statistical Software*, 43(8), 1-20.
#'
#' Silveira, I. H., et al. (2023). Heat waves and mortality in the Brazilian Amazon.
#' *International Journal of Hygiene and Environmental Health*, 248, 114109.
#'
#' Marengo, J. A., et al. (2023). A cold wave of winter 2021 in central South America.
#' *Climate Dynamics*, 61, 3-4.
#'
#' @examples
#' \dontrun{
#' # Prepare data
#' df_health <- sus_data_import() |>
#'   sus_data_clean_encoding() |>
#'   sus_data_standardize() |>
#'   sus_spatial_join(level = "munic")
#'
#' df_climate <- sus_climate_inmet(years = 2023, uf = "AM") |>
#'   sus_climate_fill_inmet(target_var = "all", parallel = TRUE)
#'
#' # Example 1: Exact match (same-day temperature)
#' df_exact <- sus_climate_aggregate(
#'   health_data = df_health,
#'   climate_data = df_climate,
#'   climate_var = "tair_dry_bulb_c",
#'   temporal_strategy = "exact"
#' )
#'
#' # Example 2: Moving window (cumulative exposure, RECOMMENDED)
#' df_moving <- sus_climate_aggregate(
#'   health_data = df_health,
#'   climate_data = df_climate,
#'   temporal_strategy = "moving_window",
#'   window_days = 14
#' )
#'
#' # Example 3: Regional adaptation (auto-detect)
#' df_regional <- sus_climate_aggregate(
#'   health_data = df_health,
#'   climate_data = df_climate,
#'   climate_region = "auto",
#'   temporal_strategy = "threshold_exceedance",
#'   window_days = 7
#' )
#'
#' # Example 4: Cold waves (southern regions)
#' df_coldwave <- sus_climate_aggregate(
#'   health_data = df_health_south,
#'   climate_data = df_climate_south,
#'   climate_region = "temperate",
#'   temporal_strategy = "cold_wave_exceedance",
#'   window_days = 7,
#'   threshold_value = 5
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
    climate_region    = "auto",
    window_days       = NULL,
    lag_days          = NULL,
    offset_days       = NULL,
    temp_base         = NULL,
    gdd_temp_var      = "tair_dry_bulb_c",
    min_obs           = 0.7,
    threshold_value   = NULL,
    threshold_direction = "above",
    weights           = NULL,
    lang              = "pt",
    verbose           = TRUE
) {

  if (verbose) {
    title_msg <- switch(lang,
      "en" = "climasus4r - Integration of Climate and Health Data",
      "pt" = "climasus4r - Integra\u00e7\u00e3o de Dados Clim\u00e1ticos e de Sa\u00fade",
      "es" = "climasus4r - Integraci\u00f3n de Datos Clim\u00e1ticos y de Salud"
    )
    cli::cli_h1(title_msg)
  }
  
  msg <- .get_messages(lang)

  # ===========================================================================
  # 1. HEALTH DATA VALIDATION
  # ===========================================================================
  system <- sus_meta(health_data, "system")
  .validate_health_input(health_data, lang, verbose)

  # ---------------------------------------------------------------------------
  # 2. CLIMATE DATA VALIDATION
  # ---------------------------------------------------------------------------
  .validate_climate_input(climate_data, lang, verbose)

  # ---------------------------------------------------------------------------
  # 3. TEMPORAL OVERLAP VALIDATION
  # ---------------------------------------------------------------------------
  .validate_date_overlap(health_data, climate_data, lang)

  # ---------------------------------------------------------------------------
  # 4. TARGET CLIMATE VARIABLES VALIDATION
  # ---------------------------------------------------------------------------
  climate_var <- .validate_climate_var(climate_data, climate_var, lang)

  # ---------------------------------------------------------------------------
  # DETECT OR VALIDATE CLIMATE REGION
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
      cli::cli_alert_info("Using region-specific temp_base = {temp_base}degC for human health")
    }
  }
  
  if (is.null(threshold_value) && temporal_strategy %in% c("threshold_exceedance", "cold_wave_exceedance")) {
    threshold_value <- region_defaults$heatwave_threshold
    if (verbose && temporal_strategy == "threshold_exceedance") {
      cli::cli_alert_info("Using region-specific heatwave threshold = {threshold_value}degC")
    }
  }

   # ---------------------------------------------------------------------------
  # 9. NEW: VALIDATE REGIONAL APPROPRIATENESS
  # ---------------------------------------------------------------------------
    .validate_regional_appropriateness(
      temporal_strategy, climate_region, temp_base, threshold_value, 
      region_defaults, verbose, lang
    )
  # ---------------------------------------------------------------------------
  # 5. STRATEGY AND PARAMETER VALIDATION
  # ---------------------------------------------------------------------------
  temporal_strategy <- match.arg(
    temporal_strategy,
    choices = c(
      "exact", "discrete_lag", "moving_window",
      "offset_window", "distributed_lag", "degree_days", "seasonal",
      "threshold_exceedance", "weighted_window", "cold_wave_exceedance"
    )
  )

  window_days  <- .validate_window_days(temporal_strategy, window_days, lang)
  lag_days     <- .validate_lag_days(temporal_strategy, lag_days, lang)
  offset_days  <- .validate_offset_days(temporal_strategy, offset_days, lang)
  .validate_degree_days_params(temporal_strategy, climate_var, gdd_temp_var, temp_base)
  .validate_threshold_params(temporal_strategy, threshold_value, threshold_direction, lang)
  weights <- .validate_weights(temporal_strategy, window_days, weights, lang)


  # ---------------------------------------------------------------------------
  # 6. TEMPORAL PRE-AGGREGATION (hour -> day, if needed)
  # ---------------------------------------------------------------------------
  # Hourly resolution detection occurs at two levels:
  #   (a) temporal$unit == "hour" metadata  primary source.
  #   (b) structural inspection  fallback when metadata is missing.
  #       Criteria: if (n_rows / n_unique_dates / n_stations) > 2, the data
  #       is sub-daily. Without this fallback, hourly data would pass directly
  #       to the join and produce ~24x inflated values.
  if (verbose) cli::cli_progress_step(msg$aggregating)

  climate_temporal_meta <- sus_meta(climate_data, "temporal")
  is_hourly_meta <- !is.null(climate_temporal_meta$unit) && climate_temporal_meta$unit == "hour"

  is_hourly_structural <- FALSE
  if (!is_hourly_meta) {
    n_rows                <- nrow(climate_data)
    n_unique_dates        <- dplyr::n_distinct(as.Date(climate_data$date))
    n_stations            <- dplyr::n_distinct(climate_data$station_code)
    rows_per_station_day  <- n_rows / max(n_unique_dates * n_stations, 1)
    is_hourly_structural  <- rows_per_station_day > 2
    if (is_hourly_structural && verbose) {
      cli::cli_alert_info(paste0(
        "Temporal metadata missing. Sub-daily resolution detected ",
        "(~", round(rows_per_station_day, 1), " obs/station/day). ",
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
  
  if (verbose) cli::cli_progress_done()

  # ---------------------------------------------------------------------------
  # 7. SPATIAL MATCHING (stations -> municipalities)
  # ---------------------------------------------------------------------------
  if (verbose) cli::cli_progress_step(msg$spatial_match)

  climate_matched <- .match_spatial(
    df          = climate_data,
    spatial_obj = health_data,
    verbose     = verbose
  )
  if (verbose) cli::cli_progress_done()

  # ---------------------------------------------------------------------------
  # 8. TEMPORAL JOIN
  # ---------------------------------------------------------------------------
  if (verbose) cli::cli_progress_step(msg$temporal_join)

  df_agg <- switch(
    temporal_strategy,
    "exact"          = .join_exact(health_data, climate_matched, climate_var),
    "discrete_lag"   = .join_discrete_lag(health_data, climate_matched, climate_var, lag_days),
    "moving_window"  = .join_moving_window(health_data, climate_matched, climate_var, window_days, min_obs),
    "offset_window"  = .join_offset_window(health_data, climate_matched, climate_var, offset_days, min_obs),
    "distributed_lag"= .join_distributed_lag(health_data, climate_matched, climate_var, lag_days),
    "degree_days"    = .join_degree_days(health_data, climate_matched, window_days, gdd_temp_var, temp_base, min_obs),
    "seasonal"            = .join_seasonal(health_data, climate_matched, climate_var),
    "threshold_exceedance" = .join_threshold_exceedance(
                               health_data, climate_matched, climate_var,
                               window_days, threshold_value, threshold_direction, min_obs),
    "cold_wave_exceedance" = .join_cold_wave_exceedance(
                               health_data, climate_matched, climate_var,
                               window_days, threshold_value, min_obs),
    "weighted_window"      = .join_weighted_window(
                               health_data, climate_matched, climate_var,
                               window_days, weights, min_obs)
  )

  if(temporal_strategy != "exact"){
     df_agg <- suppressMessages(.join_exact(df_agg, climate_matched, climate_var)) 
  }

  if (verbose) cli::cli_progress_done()
  
  # ---------------------------------------------------------------------------
  # 9. METADATA HISTORY CONSTRUCTION
  # ---------------------------------------------------------------------------
  agg_details <- c()
  
  # Temporal strategy (main)
  agg_details <- c(agg_details, sprintf("Strategy: %s", temporal_strategy))
  
  # Climate variables
  if (length(climate_var) <= 3) {
    agg_details <- c(
      agg_details,
      sprintf("Variables: %s", paste(climate_var, collapse = ", "))
    )
  } else {
    agg_details <- c(
      agg_details,
      sprintf(
        "Variables: %s and %d more",
        paste(utils::head(climate_var, 3), collapse = ", "),
        length(climate_var) - 3
      )
    )
  }
  
  # Climate region
  agg_details <- c(
    agg_details,
    sprintf("Climate region: %s (%s)", 
            climate_region,
            region_defaults$description)
  )
  
  # Time unit
  if (!is.null(time_unit) && time_unit != "day") {
    agg_details <- c(agg_details, sprintf("Time unit: %s", time_unit))
  }
  
  # Strategy-specific parameters
  if (temporal_strategy %in% c("moving_window", "degree_days", 
                                "threshold_exceedance", "weighted_window", 
                                "cold_wave_exceedance") && !is.null(window_days)) {
    agg_details <- c(agg_details, sprintf("Window: %d day(s)", window_days))
  }
  
  if (temporal_strategy == "discrete_lag" && !is.null(lag_days)) {
    if (length(lag_days) <= 3) {
      agg_details <- c(
        agg_details,
        sprintf("Lags: %s", paste(lag_days, collapse = ", "))
      )
    } else {
      agg_details <- c(
        agg_details,
        sprintf("Lags: %s and %d more", 
                paste(utils::head(lag_days, 3), collapse = ", "),
                length(lag_days) - 3)
      )
    }
  }
  
  if (temporal_strategy == "distributed_lag" && !is.null(lag_days)) {
    agg_details <- c(agg_details, sprintf("Max lag: %d day(s)", max(lag_days)))
  }
  
  if (temporal_strategy == "offset_window" && !is.null(offset_days)) {
    agg_details <- c(
      agg_details,
      sprintf("Offset window: [t-%d, t-%d]", offset_days[2], offset_days[1])
    )
  }
  
  if (temporal_strategy == "degree_days") {
    agg_details <- c(
      agg_details,
      sprintf("GDD temp base: %.1fdegC (var: %s)", temp_base, gdd_temp_var)
    )
  }
  
  if (temporal_strategy == "threshold_exceedance" && !is.null(threshold_value)) {
    dir_symbol <- if (threshold_direction == "above") ">" else "<"
    agg_details <- c(
      agg_details,
      sprintf("Threshold: %s %.1f", dir_symbol, threshold_value)
    )
  }
  
  if (temporal_strategy == "cold_wave_exceedance" && !is.null(threshold_value)) {
    agg_details <- c(
      agg_details,
      sprintf("Cold wave threshold: < %.1fdegC", threshold_value)
    )
  }
  
  if (temporal_strategy == "weighted_window" && !is.null(weights)) {
    agg_details <- c(
      agg_details,
      sprintf("Weighted window: %d weights provided", length(weights))
    )
  }
  
  # Minimum observations
  if (min_obs != 0.7) {
    agg_details <- c(agg_details, sprintf("Min obs: %.0f%%", min_obs * 100))
  }
  
  # Hourly aggregation note
  if (is_hourly_meta || is_hourly_structural) {
    agg_details <- c(agg_details, "Hourly data aggregated to daily")
  }
  
  # Create history message
  history_msg <- sprintf(
    "Climate aggregation [%s]",
    paste(agg_details, collapse = " | ")
  )
  

  # ---------------------------------------------------------------------------
  # 9. METADATA UPDATE
  # ---------------------------------------------------------------------------
  df_agg <- .set_climate_agg_meta(df_agg, system, type=temporal_strategy, history_msg = history_msg)
  
  return(df_agg)
}


# =============================================================================
# VALIDATION HELPERS
# =============================================================================

#' Returns list of messages in selected language
#' @keywords internal
#' @noRd
.get_messages <- function(lang = "pt") {
  messages <- list(
    pt = list(
      aggregating   = "Aggregating temporal data...",
      spatial_match = "Performing spatial matching...",
      temporal_join = "Integrating temporal data..."
    ),
    en = list(
      aggregating   = "Aggregating temporal data...",
      spatial_match = "Performing spatial matching...",
      temporal_join = "Integrating temporal data..."
    ),
    es = list(
      aggregating   = "Agregando datos temporales...",
      spatial_match = "Realizando coincidencia espacial...",
      temporal_join = "Integrando datos temporales..."
    )
  )
  messages[[lang]] %||% messages[["en"]]
}



#' Validates health data object
#' @keywords internal
#' @noRd
.validate_health_input <- function(health_data, lang, verbose) {
  if (!inherits(health_data, "climasus_df")) {
    cli::cli_abort(.msg_not_climasus_df(lang, "health"))
  }
  current_stage  <- sus_meta(health_data, "stage")
  required_stage <- "spatial"
  if (is.null(current_stage) || current_stage != required_stage) {
    cli::cli_abort(.msg_wrong_stage(
      lang, "health", current_stage, required_stage, "sus_spatial_join(...)"
    ))
  }
  if (verbose) cli::cli_alert_success(.msg_stage_validated(lang))
}


#' Validates climate data object
#' @keywords internal
#' @noRd
.validate_climate_input <- function(climate_data, lang, verbose) {
  if (!inherits(climate_data, "climasus_df")) {
    cli::cli_abort(.msg_not_climasus_df(lang, "climate"))
  }
  current_stage  <- sus_meta(climate_data, "stage")
  required_stage <- "climate"
  if (is.null(current_stage) || current_stage != required_stage) {
    cli::cli_abort(.msg_wrong_stage(
      lang, "climate", current_stage, required_stage,
      "sus_climate_inmet(...) followed by sus_climate_fill_inmet(..., evaluation = FALSE) and sus_climate_compute_indicators(...)"
    ))
  }
  # Aceita dados provenientes de sus_climate_fill() (source = "INMET") ou
  # sus_climate_compute_indicators() (source = "compute_indicators" ou NULL).
  # Quando os metadados temporais estão ausentes ou o source é desconhecido,
  # emite aviso em vez de abortar — compatibilidade com pipelines externos.
  temporal_meta <- sus_meta(climate_data, "temporal")
  valid_sources <- c("INMET", "compute_indicators", "indicators")
  if (!is.null(temporal_meta) &&
      !is.null(temporal_meta$source) &&
      !temporal_meta$source %in% valid_sources) {
    cli::cli_warn(paste0(
      "climate_data source '", temporal_meta$source, "' is not in the ",
      "recognized list (", paste(valid_sources, collapse = ", "), "). ",
      "Proceeding with caution."
    ))
  }
  if (verbose) cli::cli_alert_success(.msg_stage_validated(lang))
}


#' Validates temporal overlap between health and climate data
#' @keywords internal
#' @noRd
.validate_date_overlap <- function(health_data, climate_data, lang) {
  health_dates  <- sus_meta(health_data, "temporal")
  climate_dates <- sus_meta(climate_data, "temporal")

  if (is.null(health_dates) || is.null(climate_dates) ||
      is.null(health_dates$start) || is.null(climate_dates$start)) {
    cli::cli_warn(.msg_cannot_validate_dates(lang))
    return(invisible(NULL))
  }

  h_start <- as.Date(health_dates$start);  h_end <- as.Date(health_dates$end)
  c_start <- as.Date(climate_dates$start); c_end <- as.Date(climate_dates$end)

  if (h_end < c_start || h_start > c_end) {
    cli::cli_abort(.msg_no_date_overlap(lang, h_start, h_end, c_start, c_end))
  }
  if (h_start < c_start || h_end > c_end) {
    cli::cli_warn(.msg_partial_date_overlap(
      lang, c_start, c_end,
      max(h_start, c_start), min(h_end, c_end)
    ))
  }
  invisible(NULL)
}


#' Validates and resolves target climate variables
#' @keywords internal
#' @noRd
.validate_climate_var <- function(climate_data, climate_var, lang) {
  known_climate_cols <- c(
    #Variavies INMET
    "patm_mb", "patm_max_mb", "patm_min_mb",
    "tair_dry_bulb_c", "tair_max_c", "tair_min_c",
    "dew_tmean_c", "dew_tmax_c", "dew_tmin_c",
    "rh_max_porc", "rh_min_porc", "rh_mean_porc",
    "rainfall_mm", "ws_gust_m_s", "ws_2_m_s", "wd_degrees", "sr_kj_m2",
    #indices biometeorologicos
    "wbgt_c", "hi_c", "thi_c", "wcet_c", "wct_c",
    "et_c", "utci_c", "pet_c",
    # indices termicos acumulados
    "cdd_c", "hdd_c", "gdd_c",
    # Derivadas
    "diurnal_range_c",
    #Umidade derivada
    "vapor_pressure_kpa" 
  )
  available <- intersect(known_climate_cols, colnames(climate_data))
  if (length(available) == 0) cli::cli_abort(.msg_no_climate_vars(lang))
  if (identical(climate_var, "all") || is.null(climate_var)) return(available)

  missing_vars <- setdiff(climate_var, available)
  if (length(missing_vars) > 0) {
    cli::cli_warn(.msg_missing_climate_vars(lang, missing_vars, available))
    climate_var <- intersect(climate_var, available)
  }
  if (length(climate_var) == 0) cli::cli_abort(.msg_no_valid_climate_vars(lang))
  return(climate_var)
}

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
      "Auto-detecting climate region based on latitude ({round(mean_lat, 1)}deg): {detected}"
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
        "temp_base = {temp_base}degC may be inappropriate for {climate_region} region. ",
        "Recommended: {recommended_base}degC. ",
        "Proceed with caution or override with explicit justification."
      )
    }
  }
  
  if (temporal_strategy == "threshold_exceedance") {
    recommended_threshold <- region_defaults$heatwave_threshold
    
    if (abs(threshold_value - recommended_threshold) > 3) {
      cli::cli_alert_warning(
        "Heatwave threshold = {threshold_value}degC may be inappropriate for {climate_region} region. ",
        "Recommended: {recommended_threshold}degC."
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

#' Validates window_days parameter
#' @keywords internal
#' @noRd
.validate_window_days <- function(temporal_strategy, window_days, lang) {
  if (!temporal_strategy %in% c("moving_window", "degree_days",
                               "threshold_exceedance", "weighted_window")) return(NULL)
  if (is.null(window_days)) {
    cli::cli_abort("{.arg window_days} must be provided for strategy '{temporal_strategy}'.")
  }
  if (!is.numeric(window_days) || length(window_days) != 1 || window_days < 1) {
    cli::cli_abort("{.arg window_days} must be a positive integer.")
  }
  as.integer(window_days)
}


#' Validates lag_days parameter
#' @keywords internal
#' @noRd
.validate_lag_days <- function(temporal_strategy, lag_days, lang) {
  if (!temporal_strategy %in% c("discrete_lag", "distributed_lag")) return(NULL)
  if (is.null(lag_days)) {
    cli::cli_abort("{.arg lag_days} must be provided for strategy '{temporal_strategy}'.")
  }
  if (!is.numeric(lag_days) || any(lag_days < 0)) {
    cli::cli_abort("{.arg lag_days} must contain non-negative integers.")
  }
  if (temporal_strategy == "distributed_lag") {
    if (length(lag_days) > 1) {
      cli::cli_alert_info("Distributed lag uses sequence 0:max. Using max value: {max(lag_days)}")
      return(as.integer(max(lag_days)))
    }
    return(as.integer(lag_days))
  }
  as.integer(sort(unique(lag_days)))
}


#' Validates offset_days parameter
#' @keywords internal
#' @noRd
.validate_offset_days <- function(temporal_strategy, offset_days, lang) {
  if (temporal_strategy != "offset_window") return(invisible(NULL))
  if (is.null(offset_days) || length(offset_days) != 2) {
    cli::cli_abort("{.arg offset_days} must be a numeric vector of length 2 (e.g., c(7, 14)).")
  }
  if (!is.numeric(offset_days) || any(offset_days < 0)) {
    cli::cli_abort("{.arg offset_days} must contain non-negative integers.")
  }
  as.integer(sort(offset_days))
}


#' Validates degree_days parameters
#' @keywords internal
#' @noRd
.validate_degree_days_params <- function(temporal_strategy, climate_var, gdd_temp_var, temp_base) {
  if (temporal_strategy != "degree_days") return(invisible(NULL))
  if (!gdd_temp_var %in% climate_var && !identical(climate_var, "all")) {
    cli::cli_alert_info("GDD temperature variable '{gdd_temp_var}' not in climate_var. Attempting to use it anyway.")
  }
  if (!is.numeric(temp_base) || length(temp_base) != 1 || temp_base < 0) {
    cli::cli_abort("{.arg temp_base} must be a positive numeric value (e.g., 11 for Aedes aegypti).")
  }
  invisible(NULL)
}

#' Validates threshold_exceedance parameters
#' @keywords internal
#' @noRd
.validate_threshold_params <- function(temporal_strategy, threshold_value, threshold_direction, lang) {
  if (temporal_strategy != "threshold_exceedance") return(invisible(NULL))
  if (is.null(threshold_value) || !is.numeric(threshold_value) || length(threshold_value) != 1) {
    cli::cli_abort("{.arg threshold_value} must be a scalar number for strategy 'threshold_exceedance'.")
  }
  if (!threshold_direction %in% c("above", "below")) {
    cli::cli_abort("{.arg threshold_direction} must be 'above' or 'below'.")
  }
  invisible(NULL)
}


#' Validates and builds weight vector for weighted_window
#' @keywords internal
#' @noRd
.validate_weights <- function(temporal_strategy, window_days, weights, lang) {
  if (temporal_strategy != "weighted_window") return(NULL)
  n_expected <- window_days + 1L
  if (is.null(weights)) {
    # Automatic linear weights: 1.0 (event day) down to ~0.1 (oldest day)
    weights <- seq(1.0, 0.1, length.out = n_expected)
    cli::cli_alert_info(
      paste0("Weights not provided. Using automatic linear decay: ",
             "1.0 (event day) to ", round(min(weights), 2), " (oldest day).")
    )
    return(weights)
  }
  if (!is.numeric(weights) || any(weights < 0)) {
    cli::cli_abort("{.arg weights} must be a non-negative numeric vector.")
  }
  if (length(weights) != n_expected) {
    cli::cli_abort(paste0(
      "{.arg weights} must have length window_days + 1 = ", n_expected,
      " (event day + ", window_days, " previous days). ",
      "Provided length: ", length(weights), "."
    ))
  }
  weights
}

# --- Helper function imputation ---

#' Aggregate Meteorological Data Temporally (FIXED VERSION)
#'
#' @importFrom dplyr mutate group_by summarise across any_of case_when rename as_tibble .data
#' @importFrom lubridate as_datetime floor_date month year
#' @importFrom cli cli_abort
#' @noRd
.aggregate_meteo_data <- function(
    data,
    time_unit = "day",
    datetime_col = "date",
    na_rm = TRUE) 
    {

  # 1. Input validation
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  # 2. Normalize datetime column
  data_proc <- data %>%
    dplyr::mutate(!!datetime_col := lubridate::as_datetime(.data[[datetime_col]]))

  # 3. Create time grouping column
  data_proc <- data_proc %>%
    dplyr::mutate(time_group = dplyr::case_when(
      time_unit == "day"   ~ as.Date(.data[[datetime_col]]),
      
      time_unit == "week"  ~ as.Date(lubridate::floor_date(.data[[datetime_col]], unit = "week")),
      
      time_unit == "month" ~ as.Date(lubridate::floor_date(.data[[datetime_col]], unit = "month")),
      
      time_unit == "year"  ~ as.Date(lubridate::floor_date(.data[[datetime_col]], unit = "year")),
      
      time_unit == "season" ~ as.Date(lubridate::floor_date(.data[[datetime_col]], unit = "month")),
      
      # Handle formats like "15 days" or "2 months"
      grepl("^\\d+", time_unit) ~ as.Date(lubridate::floor_date(.data[[datetime_col]], unit = time_unit)),
      
      TRUE ~ as.Date(lubridate::floor_date(.data[[datetime_col]], unit = "day"))
    ))

  # Special logic for seasons (Southern Hemisphere - Brazil)
  if (time_unit == "season") {
    data_proc <- data_proc %>%
      dplyr::mutate(
        season_label = dplyr::case_when(
          lubridate::month(.data[[datetime_col]]) %in% c(12, 1, 2) ~ "DJF",
          lubridate::month(.data[[datetime_col]]) %in% c(3, 4, 5)  ~ "MAM",
          lubridate::month(.data[[datetime_col]]) %in% c(6, 7, 8)  ~ "JJA",
          lubridate::month(.data[[datetime_col]]) %in% c(9, 10, 11) ~ "SON",
          TRUE ~ NA_character_
        ),
        year = lubridate::year(.data[[datetime_col]])
      )
    
    group_vars <- c("season_label", "year")
  } else {
    group_vars <- "time_group"
  }

  # 4. Aggregation rules by variable type
  # INMET base variables
  cols_sum  <- c("rainfall_mm", "sr_kj_m2",
                 # Degree-day indices (sus_climate_compute_indicators)
                 "cdd_c", "hdd_c", "gdd_c")

  cols_mean <- c(
    "patm_mb", "patm_max_mb", "patm_min_mb",
    "tair_dry_bulb_c", "dew_tmean_c",
    "rh_mean_porc", "wd_degrees", "ws_2_m_s",
    # Derived variables (sus_climate_compute_indicators)
    "vapor_pressure_kpa", "diurnal_range_c"
  )

  # Heat-stress indices -> daily MAXIMUM (sus_climate_compute_indicators)
  # wbgt_c: Wet Bulb Globe Temperature-represents peak heat stress
  # hi_c:   Heat Index-peak perceived temperature
  # thi_c:  Temperature-Humidity Index-peak discomfort
  # et_c:   Effective Temperature-peak thermal sensation
  # utci_c: Universal Thermal Climate Index-peak physiological stress
  # pet_c:  Physiological Equivalent Temperature-peak thermal load
  cols_max  <- c(
    "tair_max_c", "dew_tmax_c", "rh_max_porc", "ws_gust_m_s",
    "wbgt_c", "hi_c", "thi_c", "et_c", "utci_c", "pet_c"
  )

  # Cold-stress indices → daily MINIMUM (sus_climate_compute_indicators)
  # wcet_c: Wind Chill Equivalent Temperature-minimum thermal comfort
  # wct_c:  Wind Chill Temperature-minimum perceived temperature
  cols_min  <- c(
    "tair_min_c", "dew_tmin_c", "rh_min_porc",
    "wcet_c", "wct_c"
  )

  # Any numeric column from sus_climate_compute_indicators not covered above
  # is aggregated by mean. This prevents silent column loss when new
  # indicators are added to the pipeline.
  all_known <- c(cols_sum, cols_mean, cols_max, cols_min,
                 "date", "station_code", "station_name",
                 "latitude", "longitude", "altitude", "uf", "region",
                 "year", "time_group", "season_label")
  numeric_cols <- names(data_proc)[vapply(data_proc, is.numeric, logical(1))]
  cols_mean_extra <- setdiff(numeric_cols, all_known)
  if (length(cols_mean_extra) > 0) {
    cols_mean <- c(cols_mean, cols_mean_extra)
  }
  
  # Metadata to preserve
  meta_vars <- c(
    "region", "station_name", "station_code",
    "latitude", "longitude", "altitude", "uf"
  )
  
  # Identify existing grouping columns
  existing_groups <- intersect(c(meta_vars, group_vars), names(data_proc))

  # 5. Safe aggregation functions
  s_sum  <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = na_rm)
  s_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = na_rm)
  s_max  <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = na_rm)
  s_min  <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = na_rm)

  # 6. Final aggregation
  aggregated_data <- data_proc %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(existing_groups))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::any_of(cols_sum),  s_sum),
      dplyr::across(dplyr::any_of(cols_mean), s_mean),
      dplyr::across(dplyr::any_of(cols_max),  s_max),
      dplyr::across(dplyr::any_of(cols_min),  s_min),
      .groups = "drop"
    )

  # 7. Final name cleanup
  if ("time_group" %in% names(aggregated_data)) {
    aggregated_data <- aggregated_data %>%
      dplyr::rename(date = time_group)
  } else if ("season_label" %in% names(aggregated_data)) {
    aggregated_data <- aggregated_data %>%
      dplyr::rename(season = .data$season_label)
  }

  return(dplyr::as_tibble(aggregated_data))
}


# =============================================================================
# SPATIAL MATCHING
# =============================================================================

#' Spatial Matching: Assigns climate data to geographic units
#'
#' @description
#' For each municipality in `spatial_obj`, identifies the nearest climate station
#' (shortest distance between municipal centroid and station coordinates) and
#' replicates climate data from that station to the municipality. The result is
#' a climate data frame where each row already has the corresponding `code_muni`
#' column, ready for temporal join.
#'
#' @keywords internal
#' @noRd
.match_spatial <- function(df, spatial_obj, verbose = FALSE) {
  # Colunas obrigatorias para o matching espacial.
  # sus_climate_compute_indicators() pode nao incluir latitude/longitude
  # diretamente nesse caso tentamos recupera-las de um atributo salvo
  # ou de uma tabela de estacoes armazenada no objeto.
  required_core   <- c("station_code")
  required_coords <- c("latitude", "longitude")
  required_name   <- c("station_name")

  if (!all(required_core %in% names(df))) {
    cli::cli_abort(
      "climate_data must contain 'station_code'. ",
      "Received columns: {paste(names(df), collapse = ', ')}"
    )
  }

  # Se latitude/longitude ou station_name estao ausentes, tenta recuperar
  # de atributos do objeto (padrao de sus_climate_compute_indicators)
  if (!all(c(required_coords, required_name) %in% names(df))) {
    station_meta <- attr(df, "station_metadata")
    if (!is.null(station_meta) &&
        all(c("station_code", "latitude", "longitude") %in% names(station_meta))) {
      df <- dplyr::left_join(
        df,
        dplyr::select(station_meta, dplyr::any_of(
          c("station_code", "station_name", "latitude", "longitude", "altitude")
        )),
        by = "station_code"
      )
      if (verbose)
        cli::cli_alert_info(
          "Recovered station coordinates from 'station_metadata' attribute."
        )
    }
  }

  # Se ainda faltam lat/lon, nao e possível fazer o matching — aborta com
  # mensagem informativa sobre como fornecer os metadados de estacao.
  missing_cols <- setdiff(c("station_code", "latitude", "longitude"), names(df))
  if (length(missing_cols) > 0) {
    cli::cli_abort(paste0(
      "climate_data is missing required columns for spatial matching: ",
      paste(missing_cols, collapse = ", "), ". \n",
      "If using sus_climate_compute_indicators(), ensure the output retains \n",
      "latitude, longitude, and station_name columns from the original INMET data, \n",
      "or attach them via: \n",
      "  attr(indicators_data, 'station_metadata') <- station_coords_df"
    ))
  }

  # station_name pode ser missing-usa station_code como fallback
  if (!"station_name" %in% names(df)) {
    df <- dplyr::mutate(df, station_name = .data$station_code)
    if (verbose)
      cli::cli_alert_info(
        "station_name not found. Using station_code as station name."
      )
  }
  if (!"code_muni" %in% names(spatial_obj)) {
    cli::cli_abort("spatial_obj deve conter a coluna 'code_muni'. Execute sus_spatial_join() antes.")
  }
  if (verbose) cli::cli_inform("Performing spatial matching of climate stations to municipalities")
 
  # ------------------------------------------------------------------
  # Estacoes unicas como sf
  # ------------------------------------------------------------------
  stations <- df %>%
    dplyr::distinct(.data$station_code, .data$station_name,
                    .data$latitude, .data$longitude) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4674, remove = FALSE)
 
  # ------------------------------------------------------------------
  # MUNICIPIOS UNICOS
  # ------------------------------------------------------------------
  # health_data tem UMA LINHA POR EVENTO (obito/caso), nao por municipio.
  # Porto Velho com 120 obitos em 2023 gera 120 linhas com o mesmo
  # code_muni e a mesma geometria.
  #
  # Se usarmos spatial_obj diretamente no st_nearest_feature, o
  # mun_station_map tera 2 024 linhas (= nrow(health_data)) em vez de
  # N_municipios_unicos linhas.  O left_join subsequente
  #   climate_data (365 dias x 14 estacoes = 5 110 linhas)
  #   x mun_station_map (2 024 linhas, com code_muni repetidos)
  # explode para 365 x 2 024 x (eventos_por_municipio) linhas - o
  # exato bug observado (2 024 -> 212 298 no exact).
  #
  # Solucao: deduplica por code_muni ANTES do calculo espacial.
  # A geometria de um municipio e identica em todas as linhas (e o
  # poligono do municipio, nao do evento), entao st_union ou first
  # produzem o mesmo resultado.  Usamos dplyr::slice(1) apos
  # distinct(code_muni) para preservar o sfc sem acionar
  # summarise() que exigiria sf::st_union (mais lento).
  spatial_sf <- sf::st_as_sf(spatial_obj)
  if (!identical(sf::st_crs(spatial_sf), sf::st_crs(stations))) {
    spatial_sf <- sf::st_transform(spatial_sf, sf::st_crs(stations))
  }
 
  # Um poligono por code_muni - descarta linhas de eventos duplicados
  geom_col   <- attr(spatial_sf, "sf_column") %||% "geom"
  munic_unique <- spatial_sf |>
    dplyr::group_by(.data$code_muni) |>
    dplyr::slice(1L) |>
    dplyr::ungroup() |>
    dplyr::select(
      "code_muni",
      dplyr::any_of("name_muni"),
      dplyr::all_of(geom_col)
    )
 
  # ------------------------------------------------------------------
  # Centroide de cada municipio unico
  # ------------------------------------------------------------------
  mun_points <- tryCatch(
    suppressWarnings(sf::st_point_on_surface(munic_unique)),
    error = function(e) suppressWarnings(sf::st_centroid(munic_unique))
  )
 
  # Indice da estacao mais proxima para cada municipio unico
  nearest_idx <- sf::st_nearest_feature(mun_points, stations)
  distances   <- sf::st_distance(mun_points, stations[nearest_idx, ], by_element = TRUE)
 
  # ------------------------------------------------------------------
  # Mapa 1-para-1: code_muni -> station_code  (N_municipios linhas)
  # ------------------------------------------------------------------
  mun_station_map <- dplyr::tibble(
    code_muni    = munic_unique$code_muni,
    station_code = stations$station_code[nearest_idx],
    distance_km  = as.numeric(distances) / 1000
  )
  if ("name_muni" %in% names(munic_unique)) {
    mun_station_map$name_muni <- munic_unique$name_muni
  }
 
  if (verbose) {
    d <- mun_station_map$distance_km
    n_munic   <- nrow(mun_station_map)
    n_station <- dplyr::n_distinct(stations$station_code)
    cli::cli_inform(c(
      "v" = "Spatial matching completed:",
      "i" = "{n_munic} municipio(s) unico(s) vinculados a {n_station} estacao(oes).",
      " " = "Distancia (km): Min={round(min(d),1)} | Mediana={round(stats::median(d),1)} | Max={round(max(d),1)}"
    ))
  }
 
  # ------------------------------------------------------------------
  # Expande o clima: cada dia de cada estacao recebe todos os municipios
  # que apontam para ela.
  #
  # Cardinalidade esperada:
  #   climate_matched = N_dias x N_municipios_unicos
  #   (ex.: 365 dias x 2 024 municipios unicos -> 5 110 linhas se 14
  #   estacoes cobrindo 2 024 municipios diferentes)
  #
  # A relacao e many-to-many CONTROLADA:
  #   - climate_data: muitos dias por station_code  (m linhas/estacao)
  #   - mun_station_map: muitos municipios por station_code  (k mun/estacao)
  # Resultado: m x k linhas por estacao - correto e esperado.
  # ------------------------------------------------------------------
  matched <- df %>%
    dplyr::left_join(mun_station_map, by = "station_code",
                     relationship = "many-to-many")
 
  # Reordena colunas de identificacao para facilitar inspecao
  id_cols <- intersect(
    c("code_muni", "name_muni", "station_code", "station_name", "distance_km"),
    names(matched)
  )
  matched <- dplyr::select(matched, dplyr::all_of(id_cols), dplyr::everything())
 
  return(matched)
}

# =============================================================================
# TEMPORAL STRATEGY IMPLEMENTATIONS
# =============================================================================

#' Exact: exact date matching
#' @keywords internal
#' @noRd
.join_exact <- function(health_data, climate_data, target_vars) {
  agg_rule <- .build_agg_rules(target_vars)
  # Agrega climate_matched para 1 linha por (date, code_muni),
  # aplicando a regra correta por tipo de variavel.
  climate_sel <- climate_data |>
    dplyr::select("date", "code_muni", dplyr::all_of(target_vars)) |>
    dplyr::group_by(.data$date, .data$code_muni) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(target_vars),
        .fns = function(x) {
          # Identifica a regra para esta variavel pelo nome da coluna
          # (dplyr::across passa o nome via cur_column())
          rule <- agg_rule$agg_type[agg_rule$climate_var == dplyr::cur_column()]
          if (length(rule) == 0L) rule <- "mean"
          switch(
            rule,
            sum           = sum(x, na.rm = TRUE),
            mean_circular = .circular_mean(x),
            mean(x, na.rm = TRUE)   # "mean" e qualquer outro
          )
        }
      ),
      .groups = "drop"
    )
  health_data |>
    dplyr::left_join(climate_sel, by = c("date", "code_muni")) |>
    dplyr::relocate(dplyr::any_of(c("date", "code_muni")))
}


#' Discrete Lag: climate values exactly L days before event
#'
#' @description
#' For each `lag` in `lag_days`, shifts the event date `t` to `t - lag` and
#' looks up the corresponding climate value. This is equivalent to
#' "what was the climate L days before this death?" There is no look-ahead bias
#' because only past dates are queried.
#'
#' Critical fix from previous version: the final join was done by
#' `health_id_vars` (all columns of health_data), which created an explosive
#' cartesian product. Now the join is done via surrogate key `.row_id` injected
#' and removed after each iteration.
#'
#' @keywords internal
#' @noRd
.join_discrete_lag <- function(health_data, climate_data, target_vars, lag_days) {
  # Chave surrogate para identificar cada linha do health_data univocamente
  health_keyed <- dplyr::mutate(health_data, .row_id = dplyr::row_number())
  result <- health_keyed
 
  # Agrega para 1 linha por (date, code_muni) usando as regras de tipo de
  # variavel - mesma logica de .join_exact() para evitar selecao arbitraria
  # de estacap quando um municipio esta equidistante de duas estacoes.
  agg_rule_lag <- .build_agg_rules(target_vars)
  climate_sel <- climate_data |>
    dplyr::select("date", "code_muni", dplyr::all_of(target_vars)) |>
    dplyr::group_by(.data$date, .data$code_muni) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(target_vars),
        .fns = function(x) {
          rule <- agg_rule_lag$agg_type[agg_rule_lag$climate_var == dplyr::cur_column()]
          if (length(rule) == 0L) rule <- "mean"
          switch(rule,
            sum           = sum(x, na.rm = TRUE),
            mean_circular = .circular_mean(x),
            mean(x, na.rm = TRUE)
          )
        }
      ),
      .groups = "drop"
    )
 
  for (lag in lag_days) {
    # Cria tabela de lookup: .row_id + date historica + code_muni
    lookup <- health_keyed %>%
      dplyr::select(".row_id", "date", "code_muni") %>%
      dplyr::mutate(.hist_date = .data$date - lag)
 
    # Join pelo par (code_muni, .hist_date)
    joined <- lookup %>%
      dplyr::left_join(
        dplyr::rename(climate_sel, .hist_date = "date"),
        by = c("code_muni", ".hist_date")
      ) %>%
      dplyr::select(".row_id", dplyr::all_of(target_vars))
 
    # Renomeia colunas com prefixo de lag
    new_names <- stats::setNames(target_vars, paste0("lag", lag, "_", target_vars))
    joined <- dplyr::rename(joined, !!!new_names)
 
    # Join por chave surrogate (1-para-1)
    result <- dplyr::left_join(result, joined, by = ".row_id")
  }
 
  # Remove chave surrogate
  dplyr::select(result, -".row_id")
}


#' Distributed Lag: lag matrix 0 ... L for DLNM
#'
#' @description
#' Generates columns `{var}_lag0`, `{var}_lag1`, `{var}_lag{L}` for direct
#' use in `dlnm::crossbasis()`. The logic is: for lag `l`, look up the
#' climate value at `t - l` (i.e., *l* days before the event). This is
#' implemented by adding `l` days to the climate date column, so that
#' the climate date `t_clim` aligns with the event date `t` when
#' `t_clim = t - l <-> t_clim + l = t`.
#'
#' Fix from previous version: the sign was correct (+ days(l)),
#' but `bind_cols` accumulated duplicate columns. Now uses `.row_id`.
#'
#' @keywords internal
#' @noRd
.join_distributed_lag <- function(health_data, climate_data, target_vars, max_lag) {
  health_keyed <- dplyr::mutate(health_data, .row_id = dplyr::row_number())
 
  # Agrega para 1 linha por (date, code_muni)-mesma regra usada em
  # .join_exact() e .join_discrete_lag() para consistencia de pipeline.
  agg_rule_dl <- .build_agg_rules(target_vars)
  climate_sel <- climate_data |>
    dplyr::select("date", "code_muni", dplyr::all_of(target_vars)) |>
    dplyr::group_by(.data$date, .data$code_muni) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(target_vars),
        .fns = function(x) {
          rule <- agg_rule_dl$agg_type[agg_rule_dl$climate_var == dplyr::cur_column()]
          if (length(rule) == 0L) rule <- "mean"
          switch(rule,
            sum           = sum(x, na.rm = TRUE),
            mean_circular = .circular_mean(x),
            mean(x, na.rm = TRUE)
          )
        }
      ),
      .groups = "drop"
    )
 
  result <- health_keyed
 
  for (l in seq(0L, as.integer(max_lag))) {
    # Desloca o calendario climatico +l dias -> alinha com o evento
    climate_shifted <- climate_sel %>%
      dplyr::mutate(date = .data$date + l)
 
    joined <- health_keyed %>%
      dplyr::select(".row_id", "date", "code_muni") %>%
      dplyr::left_join(climate_shifted, by = c("date", "code_muni")) %>%
      dplyr::select(".row_id", dplyr::all_of(target_vars))
 
    new_names <- stats::setNames(target_vars, paste0(target_vars, "_lag", l))
    joined <- dplyr::rename(joined, !!!new_names)
    result <- dplyr::left_join(result, joined, by = ".row_id")
  }
 
  dplyr::select(result, -".row_id")
}
 

#' Seasonal: matching by climatological season (DJF/MAM/JJA/SON)
#'
#' @description
#' Associates each health record with seasonal climatological means/sums of the
#' corresponding season. Seasons follow Southern Hemisphere meteorological
#' convention: DJF (summer), MAM (autumn), JJA (winter), SON (spring).
#' Requires at least 60 valid daily observations per season-year-variable to
#' include the value.
#'
#' @keywords internal
#' @noRd
.join_seasonal <- function(health_data, climate_data, target_vars) {
  get_season <- function(date) {
    m <- lubridate::month(date)
    dplyr::case_when(
      m %in% c(12L, 1L, 2L) ~ "DJF",
      m %in% c(3L, 4L, 5L)  ~ "MAM",
      m %in% c(6L, 7L, 8L)  ~ "JJA",
      TRUE                   ~ "SON"
    )
  }

  agg_rule <- .build_agg_rules(target_vars)

  climate_seasonal <- climate_data %>%
    dplyr::mutate(
      year   = lubridate::year(.data$date),
      season = get_season(.data$date)
    ) %>%
    tidyr::pivot_longer(
      cols      = dplyr::all_of(target_vars),
      names_to  = "climate_var",
      values_to = "value"
    ) %>%
    dplyr::left_join(agg_rule, by = "climate_var") %>%
    dplyr::group_by(code_muni, .data$station_name,
                    .data$year, .data$season,
                    .data$climate_var, .data$agg_type) %>%
    dplyr::summarise(
      n_valid = sum(!is.na(.data$value)),
      value   = dplyr::case_when(
        dplyr::first(.data$agg_type) == "sum"           ~ sum(.data$value, na.rm = TRUE),
        dplyr::first(.data$agg_type) == "mean_circular" ~ .circular_mean(.data$value),
        TRUE                                             ~ mean(.data$value, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$n_valid >= 60L) %>%
    dplyr::select(-"n_valid", -"agg_type") %>%
    tidyr::pivot_wider(
      names_from   = "climate_var",
      values_from  = "value",
      names_prefix = "season_"
    )

  health_data %>%
    dplyr::mutate(
      year   = lubridate::year(.data$date),
      season = get_season(.data$date)
    ) %>%
    dplyr::left_join(
      climate_seasonal,
      by = c("code_muni",  "year", "season")
    ) %>%
    dplyr::select(-"year", -"season")
}


#' Overlap join via data.table::foverlaps (internal use)
#'
#' @description
#' Performance core shared by `moving_window`, `offset_window` and `degree_days`.
#' Uses `data.table::foverlaps` to perform conditional interval join **without
#' materializing the cartesian product** between municipalities and climate dates.
#'
#' Standard dplyr (`left_join` by `code_muni` + subsequent filter) would need to
#' allocate the full table before filtering:
#'   2024 municipalities x 122,640 climate rows aprox. 248 M rows -> OOM crash.
#'
#' `foverlaps` uses a binary index on climate intervals, running in O(n log n)
#' and allocating only rows that satisfy the temporal filter.
#'
#' @param health_dt   `data.table` with columns `.row_id`, `code_muni`,
#'   `.win_start` (Date/integer), `.win_end` (Date/integer).
#' @param climate_dt  `data.table` with columns `code_muni`, `date_int`
#'   (integer), and climate variables.
#' @param target_vars Vector of climate variable names to include.
#'
#' @return `data.table` with rows satisfying
#'   `.win_start <= date_int <= .win_end`, by `code_muni`.
#'
#' @keywords internal
#' @noRd
.foverlaps_window <- function(health_dt, climate_dt, target_vars) {
  # foverlaps requires both tables to have the SAME interval key.
  # We represent dates as integers (days since origin) to avoid
  # Date vs integer type issues in data.table.
  # health_dt must have: code_muni, .win_start_int, .win_end_int
  # climate_dt must have: code_muni, date_int, date_int (duplicated column
  #   = degenerate interval [date, date])

  data.table::setkeyv(climate_dt, c("code_muni", "date_int", "date_int2"))
  data.table::setkeyv(health_dt,  c("code_muni", ".win_start_int", ".win_end_int"))

  joined <- data.table::foverlaps(
    health_dt,
    climate_dt,
    by.x = c("code_muni", ".win_start_int", ".win_end_int"),
    by.y = c("code_muni", "date_int",        "date_int2"),
    type = "any",
    mult = "all",
    nomatch = NA   # preserves health rows without match (left join)
  )
  joined
}


#' Moving Window: right sliding window (t - W, t)
#'
#' @description
#' Aggregates the `W + 1` immediately preceding days (inclusive) to the event,
#' using `data.table::foverlaps` for interval overlap join.
#' This avoids materializing the cartesian product that caused memory overflow
#' in the original dplyr `many-to-many` implementation.
#'
#' Complexity: O(n log n) instead of O(n x m).
#'
#' @keywords internal
#' @noRd
.join_moving_window <- function(health_data, climate_data, climate_var, window_days, min_obs) {
  window_size <- window_days + 1L
  health_work <- health_data %>% dplyr::mutate(.row_id = dplyr::row_number())
  health_valid <- health_work[!is.na(health_work$date), ]
  
  result_list <- list()
  for (var in climate_var) {
    climate_dt <- data.table::as.data.table(climate_data[, c("code_muni", "date", var)])
    data.table::setnames(climate_dt, var, "value")
    climate_dt[, `:=`(d1 = as.integer(date), d2 = as.integer(date))]
    
    health_dt <- data.table::as.data.table(sf::st_drop_geometry(health_valid[, c("code_muni", "date", ".row_id")]))
    health_dt[, `:=`(start = as.integer(date) - window_days, end = as.integer(date))]
    
    # Prevent NA error in foverlaps
    health_dt <- health_dt[!is.na(start)]
    
    data.table::setkey(climate_dt, code_muni, d1, d2)
    joined <- data.table::foverlaps(health_dt, climate_dt, by.x=c("code_muni", "start", "end"), by.y=c("code_muni", "d1", "d2"))
    
    res <- joined[, .(
      avg = if(sum(!is.na(value))/window_size >= min_obs) mean(value, na.rm=TRUE) else NA_real_
    ), by = .row_id]
    
    data.table::setnames(res, "avg", paste0(var, "_mean_w", window_days))
    result_list[[var]] <- res
  }
  
  all_res <- Reduce(function(x,y) merge(x,y, by=".row_id", all=TRUE), result_list)
  final <- health_work %>% dplyr::left_join(all_res, by=".row_id") %>% dplyr::select(-.row_id)
  return(dplyr::as_tibble(final))
}

#' Offset Window: aggregates historical interval (t - W2, t - W1)
#'
#' @description
#' Ignores the most recent `W1` days and aggregates the `(W2 - W1 + 1)` days
#' preceding them. Useful for modeling incubation periods or biological latency.
#' Uses `data.table::foverlaps` for memory efficiency.
#'
#' Example: `offset_days = c(7, 14)` captures exposure from `t-14` to `t-7`,
#' ignoring the week immediately before the event.
#'
#' @keywords internal
#' @noRd
.join_offset_window <- function(health_data, climate_data, target_vars, offset_days, min_obs) {
  # 1. Window parameters
  w1          <- offset_days[1]         # closest to event (e.g., 2)
  w2          <- offset_days[2]         # furthest (e.g., 5)
  window_size <- (w2 - w1) + 1L
  agg_rule    <- .build_agg_rules(target_vars)
  
  # 2. Unique Identification and Backup
  # Create .row_id to guarantee original order even with NAs or duplicates
  health_work <- health_data %>% 
    dplyr::mutate(.row_id = dplyr::row_number())
  
  # 3. Filter Valid Dates (Essential for foverlaps)
  health_valid <- health_work[!is.na(health_work$date), ]
  
  if (nrow(health_valid) == 0) {
    # Return original dataframe with result columns filled with NA
    new_cols <- paste0("off", w1, "to", w2, "_", target_vars)
    for(nc in new_cols) health_work[[nc]] <- NA_real_
    return(dplyr::as_tibble(health_work) %>% dplyr::select(-.row_id))
  }

  # --- Prepare climate data.table -------------------------------------------
  climate_dt <- data.table::as.data.table(
    dplyr::select(climate_data, "code_muni", "date", dplyr::all_of(target_vars))
  )
  climate_dt[, `:=`(
    date_int  = as.integer(date),
    date_int2 = as.integer(date),
    date = NULL
  )]
  data.table::setkey(climate_dt, code_muni, date_int, date_int2)

  # Prepare health data.table (only valid rows)
  health_dt <- data.table::as.data.table(
    sf::st_drop_geometry(health_valid)
  )
  
  # Calculate intervals (t-w2 to t-w1)
  health_dt[, `:=`(
    .win_start_int = as.integer(date) - w2,
    .win_end_int   = as.integer(date) - w1
  )]
  
  # Final guarantee against NAs in foverlaps search columns
  health_dt <- health_dt[!is.na(.win_start_int) & !is.na(.win_end_int)]

  # Overlap join 
  # Using internal helper if it exists, or foverlaps directly
  joined <- data.table::foverlaps(
    health_dt, climate_dt,
    by.x = c("code_muni", ".win_start_int", ".win_end_int"),
    by.y = c("code_muni", "date_int", "date_int2"),
    type = "any", mult = "all", nomatch = NA
  )

  # Aggregate by .row_id 
  result_dt <- joined[, lapply(
    stats::setNames(target_vars, target_vars),
    function(v) {
      vals    <- get(v)
      n_valid <- sum(!is.na(vals))
      prop    <- n_valid / window_size
      
      if (prop < min_obs) return(NA_real_)
      
      rule <- agg_rule$agg_type[agg_rule$climate_var == v]
      if (length(rule) > 0 && identical(rule, "sum")) return(sum(vals, na.rm = TRUE))
      if (length(rule) > 0 && identical(rule, "mean_circular")) return(.circular_mean(vals))
      
      mean(vals, na.rm = TRUE)
    }
  ), by = .(.row_id)]

  #  Final Formatting 
  new_nms <- paste0("off", w1, "to", w2, "_", target_vars)
  data.table::setnames(result_dt, target_vars, new_nms)

  # Final join with health_work to reintegrate everything (including geometry and NAs)
  final_df <- health_work %>%
    dplyr::left_join(as.data.frame(result_dt), by = ".row_id") %>%
    dplyr::select(-.row_id)

  # Set metadata (if function exists in environment)
  if(exists(".set_climate_agg_meta")) {
    final_df <- .set_climate_agg_meta(final_df, type=NULL, system = NULL, history_msg = NULL)
  }

  return(dplyr::as_tibble(final_df))
}

#' Degree Days: Accumulated Growing Degree Days
#'
#' @description
#' Calculates \eqn{GDD = \sum_{d=t-W}^{t} \max(0, T_d - T_{base})} for each
#' health record, using `data.table::foverlaps` for memory efficiency.
#' Used to model thermal development of arthropod vectors (e.g., *Aedes aegypti*):
#' GDD above `temp_base` accumulates over `window_days` days before the event.
#'
#' @keywords internal
#' @noRd
.join_degree_days <- function(health_data, climate_data, window_days, gdd_temp_var, temp_base, min_obs) {
  window_size <- window_days + 1L
  health_work <- health_data %>% dplyr::mutate(.row_id = dplyr::row_number())
  health_valid <- health_work[!is.na(health_work$date), ]
  
  climate_dt <- data.table::as.data.table(climate_data[, c("code_muni", "date", gdd_temp_var)])
  data.table::setnames(climate_dt, gdd_temp_var, "temp")
  climate_dt[, `:=`(d1 = as.integer(date), d2 = as.integer(date))]
  
  health_dt <- data.table::as.data.table(sf::st_drop_geometry(health_valid[, c("code_muni", "date", ".row_id")]))
  health_dt[, `:=`(start = as.integer(date) - window_days, end = as.integer(date))]
  health_dt <- health_dt[!is.na(start)]
  
  data.table::setkey(climate_dt, code_muni, d1, d2)
  joined <- data.table::foverlaps(health_dt, climate_dt, by.x=c("code_muni", "start", "end"), by.y=c("code_muni", "d1", "d2"))
  
  res <- joined[, .(
    gdd = if(sum(!is.na(temp))/window_size >= min_obs) sum(pmax(temp - temp_base, 0), na.rm=TRUE) else NA_real_
  ), by = .row_id]
  
  data.table::setnames(res, "gdd", paste0("gdd_w", window_days, "tbase", temp_base))
  
  final <- health_work %>% dplyr::left_join(res, by=".row_id") %>% dplyr::select(-.row_id)
  return(dplyr::as_tibble(final))
}

#' Threshold Exceedance: counts days of extreme events in window (t - W, t)
#'
#' @description
#' For each health record, counts the number of days within the window
#' \eqn{(t - W, t)} in which the climate variable exceeded the threshold
#' \code{threshold_value} in the specified direction.
#'
#' This metric captures the frequency of extreme exposure, which often
#' predicts health outcomes better than the window average:
#' \itemize{
#'   \item Heat waves: the human body fails after X consecutive days of
#'     extreme heat, not by the weekly average temperature.
#'   \item Floods/Leptospirosis: risk increases with number of days of
#'     intense precipitation above 50mm, not with the average.
#' }
#'
#' Uses `data.table::foverlaps` for memory efficiency.
#' Output columns: `nexc{W}_{dir}{thr}_{var}` (number of exceedance days) and
#' `prop{W}_{dir}{thr}_{var}` (proportion within window).
#'
#' @keywords internal
#' @noRd
.join_threshold_exceedance <- function(health_data, climate_data, target_vars,
                                       window_days, threshold_value,
                                       threshold_direction, min_obs) 
                                       {
  window_size    <- window_days + 1L
  health_id_vars <- setdiff(names(health_data), "geom")
  dir_label      <- if (threshold_direction == "above") "gt" else "lt"
  thr_label      <- gsub("\\.", "p", as.character(threshold_value))
 
  # Prepara data.table do clima 
  climate_dt <- data.table::as.data.table(
    dplyr::select(climate_data, "code_muni", "date", dplyr::all_of(target_vars))
  )
  climate_dt[, date_int  := as.integer(date)]
  climate_dt[, date_int2 := date_int]
  climate_dt[, date := NULL]
 
  # Prepara data.table de saude com janela [t-W, t] 
  health_dt <- data.table::as.data.table(
    dplyr::select(sf::st_drop_geometry(health_data), dplyr::all_of(health_id_vars))
  )
  health_dt[, .row_id        := .I]
  health_dt[, .win_start_int := as.integer(date) - window_days]
  health_dt[, .win_end_int   := as.integer(date)]
 
  #  Overlap join 
  joined <- .foverlaps_window(health_dt, climate_dt, target_vars)
 
  # Conta excedencias por variavel
  id_plus_row <- c(health_id_vars, ".row_id")
 
  result <- joined[
    ,
    {
      out_list <- list()
      for (v in target_vars) {
        vals         <- get(v)
        n_obs        <- sum(!is.na(vals))
        prop_obs_val <- n_obs / window_size
 
        # All output values are forced to double to guarantee type consistency
        # across groups in data.table. sum(logical) returns integer in R, which
        # conflicts with NA_real_ used for the below-min_obs branch, causing:
        #   "Column N of result for group K is type double but expecting integer"
        # Casting explicitly to double eliminates all type ambiguity.
        if (prop_obs_val < min_obs) {
          n_exc <- NA_real_
          p_exc <- NA_real_
        } else {
          exc   <- if (threshold_direction == "above") vals > threshold_value
                   else vals < threshold_value
          n_exc <- as.double(sum(exc, na.rm = TRUE))
          p_exc <- n_exc / n_obs
        }
 
        exc_col  <- paste0("nexc",  window_days, "_", dir_label, thr_label, "_", v)
        prop_col <- paste0("pexc",  window_days, "_", dir_label, thr_label, "_", v)
        out_list[[exc_col]]  <- n_exc
        out_list[[prop_col]] <- round(p_exc, 4L)
      }
      out_list
    },
    by = id_plus_row
  ]
 
  result[, .row_id := NULL]
  .set_climate_agg_meta(result, type = NULL, system = NULL, history_msg = NULL)
}
 
#' Weighted Window: weighted mean with temporal decay
#'
#' @description
#' Applies a weight vector \code{weights} to the days of the window
#' \eqn{[t - W, t]}, giving more importance to recent days (biologically more
#' relevant) and less to more distant days. The weighted mean is:
#'
#' \deqn{\bar{X}_w = \frac{\sum_{d=0}^{W} w_d \cdot X_{t-d}}{\sum_{d=0}^{W} w_d \cdot \mathbb{1}[X_{t-d} \text{ is not NA}]}}
#'
#' Where \eqn{w_0} is the weight of the event day (position 1 in vector
#' \code{weights}) and \eqn{w_W} is the weight of the oldest day (position W+1).
#'
#' Clinical applications:
#' \itemize{
#'   \item PM2.5/asthma: strong impact in first 48h, then decays.
#'   \item Infectious diseases: model incubation with more weight on period
#'     of highest infectious contact probability.
#' }
#'
#' For sum variables (rainfall_mm, sr_kj_m2), the function calculates
#' \emph{weighted sum} instead of weighted mean , epidemiologically correct
#' behavior (recent precipitation weighs more on runoff).
#' For \code{wd_degrees}, uses weighted circular mean (von Mises).
#'
#' Uses `data.table::foverlaps` for memory efficiency.
#' Output columns: `wwin{W}_{var}`.
#'
#' @keywords internal
#' @noRd
.join_weighted_window <- function(health_data, climate_data, target_vars,
                                   window_days, weights, min_obs) 
{
  # 1. Parameters and Rules
  window_size    <- window_days + 1L
  agg_rule       <- .build_agg_rules(target_vars)
  
  # 2. Backup and Unique ID (Essential for integrity)
  health_work <- health_data %>% 
    dplyr::mutate(.row_id = dplyr::row_number())
  
  # 3. Filter Valid Dates (Prevents foverlaps error)
  health_valid <- health_work[!is.na(health_work$date), ]
  
  # If no valid data, return original object with result columns filled with NA
  if (nrow(health_valid) == 0) {
    new_cols <- paste0("wwin", window_days, "_", target_vars)
    for(nc in new_cols) health_work[[nc]] <- NA_real_
    return(dplyr::as_tibble(health_work) %>% dplyr::select(-.row_id))
  }

  # Prepare climate data.table 
  climate_dt <- data.table::as.data.table(
    dplyr::select(climate_data, "code_muni", "date", dplyr::all_of(target_vars))
  )
  climate_dt[, `:=`(
    date_int  = as.integer(date),
    date_int2 = as.integer(date),
    date = NULL
  )]
  data.table::setkey(climate_dt, code_muni, date_int, date_int2)

  #  Prepare health data.table 
  health_dt <- data.table::as.data.table(
    sf::st_drop_geometry(health_valid)
  )
  
  health_dt[, `:=`(
    .win_start_int = as.integer(date) - window_days,
    .win_end_int   = as.integer(date)
  )]
  
  # Additional guarantee against NAs in interval columns
  health_dt <- health_dt[!is.na(.win_start_int) & !is.na(.win_end_int)]

  #  Overlap join 
  # If .foverlaps_window is your function that handles NAs, you can use it.
  # Otherwise, direct foverlaps is safer:
  joined <- data.table::foverlaps(
    health_dt, climate_dt,
    by.x = c("code_muni", ".win_start_int", ".win_end_int"),
    by.y = c("code_muni", "date_int", "date_int2"),
    type = "any", mult = "all", nomatch = NA
  )

  # Calculate lag of each climate observation relative to event day
  joined[, .lag := .win_end_int - date_int]

  # Weighted mean/sum by variable using .row_id 
  result_dt <- joined[
    ,
    {
      out_list <- list()
      for (v in target_vars) {
        vals     <- get(v)
        lags     <- .lag
        # Filter NAs before indexing weights to avoid index error
        not_na   <- !is.na(vals) & !is.na(lags)
        
        n_obs    <- sum(not_na)
        prop_val <- n_obs / window_size
        
        if (prop_val < min_obs) {
          out_list[[v]] <- NA_real_
          next
        }
        
        v_ok  <- vals[not_na]
        l_ok  <- lags[not_na]
        w_ok  <- weights[l_ok + 1L] # lag 0 -> weights[1]
        
        # If after filtering no valid weights remain
        if(length(w_ok) == 0 || sum(w_ok, na.rm=TRUE) == 0) {
          out_list[[v]] <- NA_real_
          next
        }
        
        rule <- agg_rule$agg_type[agg_rule$climate_var == v]
        
        val_w <- if (length(rule) > 0 && identical(rule, "sum")) {
          as.double(sum(v_ok * w_ok))
        } else if (length(rule) > 0 && identical(rule, "mean_circular")) {
          rad <- v_ok * pi / 180
          result <- atan2(sum(w_ok * sin(rad)), sum(w_ok * cos(rad))) * 180 / pi
          as.double(result) 
        } else {
           as.double(sum(v_ok * w_ok) / sum(w_ok))
        }
        out_list[[v]] <- val_w
      }
      out_list
    },
    by = .(.row_id)
  ]

  for (v in target_vars) {
    if (v %in% names(result_dt)) {
      data.table::set(result_dt, j = v, value = as.double(result_dt[[v]]))
    }
  }

  # Formatting and Reintegration 
  new_nms <- paste0("wwin", window_days, "_", target_vars)
  data.table::setnames(result_dt, target_vars, new_nms)

  # Join with original to preserve geometry and all rows
  final_df <- health_work %>%
    dplyr::left_join(as.data.frame(result_dt), by = ".row_id") %>%
    dplyr::select(-.row_id)

  if(exists(".set_climate_agg_meta")) {
    final_df <- .set_climate_agg_meta(final_df, type = NULL, system = NULL, history_msg = NULL)
  }

  return(dplyr::as_tibble(final_df))
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
  # 1. CONSTANT PREPARATION
  window_size <- window_days + 1L
  
  # 2. CREATE UNIQUE KEY AND INITIAL CLEANING
  # Use .row_id to ensure data returns to exact original row
  health_work <- health_data %>%
    dplyr::mutate(.row_id = dplyr::row_number()) %>%
    sf::st_drop_geometry() # Remove geometry for join performance
  
  # 3. SEPARATE AND VALIDATE DATES (foverlaps requires 'start' and 'end' without NA)
  health_valid <- health_work[!is.na(health_work$date), ]
  
  # If no valid data, return original structure with NAs
  if (nrow(health_valid) == 0) {
    for (var in climate_var) {
      health_work[[paste0("ncold", window_days, "_lt", threshold_value, "_", var)]] <- NA_real_
      health_work[[paste0("pcold", window_days, "_lt", threshold_value, "_", var)]] <- NA_real_
    }
    return(dplyr::as_tibble(health_work) %>% dplyr::select(-.row_id))
  }
  
  result_list <- list()
  
  for (var in climate_var) {
    # Climate Preparation (data.table)
    climate_dt <- data.table::as.data.table(
      dplyr::select(climate_data, "code_muni", "date", value = dplyr::all_of(var))
    )
    # Filter NAs in climate to avoid counting errors
    climate_dt <- climate_dt[!is.na(value) & !is.na(date)]
    climate_dt[, `:=`(date_int = as.integer(date), 
                     date_int2 = as.integer(date), 
                     date = NULL)]
    
    # Health Preparation (only rows with date)
    health_dt <- data.table::as.data.table(
      dplyr::select(health_valid, "code_muni", "date", ".row_id")
    )
    health_dt[, `:=`(.win_start_int = as.integer(date) - window_days,
                     .win_end_int = as.integer(date))]
    
    # FINAL GUARANTEE: foverlaps fails if there are NAs in interval columns
    health_dt <- health_dt[!is.na(.win_start_int) & !is.na(.win_end_int)]
    
    # Keys for foverlaps
    data.table::setkeyv(climate_dt, c("code_muni", "date_int", "date_int2"))
    data.table::setkeyv(health_dt, c("code_muni", ".win_start_int", ".win_end_int"))
    
    # Overlap join
    joined <- data.table::foverlaps(
      health_dt, climate_dt,
      by.x = c("code_muni", ".win_start_int", ".win_end_int"),
      by.y = c("code_muni", "date_int", "date_int2"),
      type = "any", mult = "all", nomatch = NA
    )
    
    col_n <- paste0("ncold", window_days, "_lt", threshold_value, "_", var)
    col_p <- paste0("pcold", window_days, "_lt", threshold_value, "_", var)
    
    # Aggregation by original row ID
    # res <- joined[, .(
    #   n_below = sum(value < threshold_value, na.rm = TRUE),
    #   n_obs   = sum(!is.na(value))
    # ), by = .(.row_id)]
    res <- joined[, .(
      n_below = as.double(sum(value < threshold_value, na.rm = TRUE)),  # Forcar double
      n_obs   = as.double(sum(!is.na(value)))                           # Forcar double
    ), by = .(.row_id)]
    
    # Apply quality criteria (min_obs)
    res[, prop := n_obs / window_size]
    res[prop >= min_obs, (col_n) := as.numeric(n_below)]
    res[prop >= min_obs, (col_p) := n_below / n_obs]
    
    # Para grupos que nao atendem min_obs, definir como NA_real_
    res[prop < min_obs, (col_n) := NA_real_]
    res[prop < min_obs, (col_p) := NA_real_]

    # Keep only columns of interest for merging
    result_list[[var]] <- res[, .SD, .SDcols = c(".row_id", col_n, col_p)]
  }
  
  # 4. CONSOLIDATION
  # Merge results from all climate variables
  all_results <- Reduce(function(x, y) merge(x, y, by = ".row_id", all = TRUE), result_list)
  
  if (!is.null(all_results) && nrow(all_results) > 0) {
    for (col in setdiff(names(all_results), ".row_id")) {
      if (is.integer(all_results[[col]])) {
        all_results[[col]] <- as.double(all_results[[col]])
      }
    }
  }

  # 5. REINSERTION INTO ORIGINAL OBJECT (Preserving geometry and rows without date)
  final_df <- health_data %>%
    dplyr::mutate(.row_id = dplyr::row_number()) %>%
    dplyr::left_join(all_results, by = ".row_id") %>%
    dplyr::select(-.row_id)
  
  return(dplyr::as_tibble(final_df))
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Aggregation rules by climate variable type
#'
#' @description
#' Defines whether each variable should be summed (`sum`), calculated as
#' arithmetic mean (`mean`) or circular mean (`mean_circular`):
#' - `rainfall_mm`, `sr_kj_m2`: additive quantities -> sum.
#' - `wd_degrees`: circular angle -> vector mean (von Mises).
#' - Other variables: arithmetic means.
#'
#' @keywords internal
#' @noRd
.build_agg_rules <- function(vars) {
  dplyr::tibble(
    climate_var = vars,
    agg_type    = dplyr::case_when(
      vars %in% c("rainfall_mm", "sr_kj_m2") ~ "sum",
      vars == "wd_degrees"                    ~ "mean_circular",
      TRUE                                    ~ "mean"
    )
  )
}


#' Circular mean (wind direction)
#'
#' @description
#' Calculates vector mean of angles in degrees using sine/cosine decomposition.
#' Avoids the arithmetic mean artifact of angles (e.g., mean of 1 deg and 359 deg should be 0 degC, not 180 deg).
#'
#' @keywords internal
#' @noRd
.circular_mean <- function(x, na_rm = TRUE) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(NA_real_)
  rad <- x * pi / 180
  atan2(mean(sin(rad)), mean(cos(rad))) * 180 / pi
}


# #' Updates climasus_df metadata after aggregation
# #' @keywords internal
# #' @noRd
# .set_climate_agg_meta <- function(df, system) {
#   if (!inherits(df, "climasus_df")) {
#     meta <- list(
#       system   = system,
#       stage    = "climate",
#       type     = "agg",
#       spatial  = inherits(df, "sf"),
#       temporal = NULL,
#       created  = Sys.time(),
#       modified = Sys.time(),
#       history  = sprintf("[%s] Climate aggregation", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
#       user     = list()
#     )
#     base_classes <- setdiff(class(df), "climasus_df")
#     structure(df, sus_meta = meta, class = c("climasus_df", base_classes))
#   } else {
#     sus_meta(df, stage = "climate", type = "agg")
#   }
# }

.set_climate_agg_meta <- function(df, system, type, history_msg = NULL) {
  if (!inherits(df, "climasus_df")) {
    meta <- list(
      system   = system,
      stage    = "climate",
      type     = type,
      spatial  = inherits(df, "sf"),
      temporal = NULL,
      created  = Sys.time(),
      modified = Sys.time(),
      history  = sprintf("[%s] Climate aggregation", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      user     = list()
    )
    
    # Add detailed history if provided
    if (!is.null(history_msg)) {
      meta$history <- c(meta$history, history_msg)
    }
    
    base_classes <- setdiff(class(df), "climasus_df")
    df <- structure(df, sus_meta = meta, class = c("climasus_df", base_classes))
  } else {
    # Update existing climasus_df
    if (!is.null(history_msg)) {
      df <- sus_meta(df, add_history = history_msg)
    }
    df <- sus_meta(df, stage = "climate", type = type)
  }
  
  return(df)
}

# =============================================================================
# ERROR MESSAGE HELPERS
# =============================================================================

.msg_not_climasus_df <- function(lang, data_type) {
  pipeline_steps <- list(
    health = list(
      pt = c(
        "  1. sus_data_import() or sus_data_read()",
        "  2. sus_data_clean_encoding()",
        "  3. sus_data_standardize()",
        "  4. sus_data_filter_cid()",
        "  5. sus_data_filter_demographics()",
        "  6. sus_data_aggregate()",
        "  7. sus_spatial_join()"
      ),
      en = c(
        "  1. sus_data_import() or sus_data_read()",
        "  2. sus_data_clean_encoding()",
        "  3. sus_data_standardize()",
        "  4. sus_data_filter_cid()",
        "  5. sus_data_filter_demographics()",
        "  6. sus_data_aggregate()",
        "  7. sus_spatial_join()"
      )
    ),
    climate = list(
      pt = c(
        "  1. sus_climate_inmet()",
        "  2. sus_climate_fill_inmet(evaluation = FALSE)"
      ),
      en = c(
        "  1. sus_climate_inmet()",
        "  2. sus_climate_fill_inmet(evaluation = FALSE)"
      )
    )
  )
  steps <- pipeline_steps[[data_type]][[lang]] %||% pipeline_steps[[data_type]][["en"]]
  base  <- list(
    pt = "Input is not a climasus_df object.",
    en = "Input is not a climasus_df object.",
    es = "La entrada no es un objeto climasus_df."
  )
  paste0(
    base[[lang]] %||% base[["en"]],
    "\nThis function requires data from the CLIMASUS4r pipeline.\n\nPrepare your data:\n",
    paste(steps, collapse = "\n")
  )
}

.msg_wrong_stage <- function(lang, data_type, current, required, hint) {
  labels <- list(
    health  = list(pt = "health",  en = "health",  es = "salud"),
    climate = list(pt = "climate",  en = "climate", es = "clima")
  )
  dl <- labels[[data_type]][[lang]] %||% labels[[data_type]][["en"]]
  base <- list(
    pt = paste0("{dl} data has invalid stage.\n",
                "Current: {current} | Required: {required}\n",
                "Run: {dl}_data <- {hint}"),
    en = paste0("{dl} data has invalid stage.\n",
                "Current: {current} | Required: {required}\n",
                "Please run: {dl}_data <- {hint}")
  )
  glue::glue(base[[lang]] %||% base[["en"]],
             dl = dl, current = current %||% "unknown",
             required = required, hint = hint)
}

.msg_wrong_source <- function(lang) {
  msgs <- list(
    pt = "Climate data must be from INMET source.",
    en = "Climate data must be from INMET source.",
    es = "Los datos climaticos deben ser de fuente INMET."
  )
  msgs[[lang]] %||% msgs[["en"]]
}

.msg_stage_validated <- function(lang) {
  msgs <- list(
    pt = "Data stage validated: aggregation allowed",
    en = "Data stage validated: aggregation allowed",
    es = "Etapa de datos validada: agregacion permitida"
  )
  msgs[[lang]] %||% msgs[["en"]]
}

.msg_cannot_validate_dates <- function(lang) {
  msgs <- list(
    pt = "Cannot validate date overlap: temporal metadata missing.",
    en = "Cannot validate date overlap: temporal metadata missing.",
    es = "No se puede validar superposicion de fechas: metadatos temporales faltantes."
  )
  msgs[[lang]] %||% msgs[["en"]]
}

.msg_no_date_overlap <- function(lang, h_start, h_end, c_start, c_end) {
  msgs <- list(
    pt = paste0("No date overlap between health and climate data.\n",
                "Health: ", h_start, " to ", h_end, "\n",
                "Climate: ", c_start, " to ", c_end),
    en = paste0("No date overlap between health and climate data.\n",
                "Health: ", h_start, " to ", h_end, "\n",
                "Climate: ", c_start, " to ", c_end)
  )
  msgs[[lang]] %||% msgs[["en"]]
}

.msg_partial_date_overlap <- function(lang, c_start, c_end, eff_start, eff_end) {
  msgs <- list(
    pt = paste0("Health data extends beyond climate range.\n",
                "Climate available: ", c_start, " to ", c_end, "\n",
                "Effective period: ", eff_start, " to ", eff_end),
    en = paste0("Health data extends beyond climate range.\n",
                "Climate available: ", c_start, " to ", c_end, "\n",
                "Effective period: ", eff_start, " to ", eff_end)
  )
  msgs[[lang]] %||% msgs[["en"]]
}

.msg_no_climate_vars <- function(lang) {
  msgs <- list(
    pt = "No recognized climate variables found in data.",
    en = "No recognized climate variables found in data.",
    es = "No se encontraron variables climaticas reconocidas."
  )
  msgs[[lang]] %||% msgs[["en"]]
}

.msg_missing_climate_vars <- function(lang, missing, available) {
  msgs <- list(
    pt = paste0("Variables not found: ", paste(missing, collapse = ", "),
                ". Using: ", paste(available, collapse = ", ")),
    en = paste0("Variables not found: ", paste(missing, collapse = ", "),
                ". Using: ", paste(available, collapse = ", "))
  )
  msgs[[lang]] %||% msgs[["en"]]
}

.msg_no_valid_climate_vars <- function(lang) {
  msgs <- list(
    pt = "No valid climate variables remaining after validation.",
    en = "No valid climate variables remaining after validation.",
    es = "No quedan variables climaticas validas."
  )
  msgs[[lang]] %||% msgs[["en"]]
}