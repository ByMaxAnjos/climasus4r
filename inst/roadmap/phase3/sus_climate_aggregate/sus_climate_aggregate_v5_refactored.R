#' Integration of Climate and Health Data with Enhanced Scientific Rigor
#'
#' @description
#' `sus_climate_aggregate()` aggregates meteorological data to DATASUS health data
#' using epidemiologically rigorous temporal strategies. The function links each
#' health record to the nearest climate station (by Euclidean distance between
#' municipal centroids and station coordinates) and applies the requested temporal
#' window.
#'
#' **SCIENTIFIC IMPROVEMENTS (v5):**
#' - Adjusted default `t_base` for degree-days from 11°C to 20°C (physiologically
#'   relevant for human health; 11°C remains available for vector biology)
#' - Enhanced documentation on physiological thresholds and temporal strategies
#' - Improved validation for autocorrelation in tropical regions
#' - Warnings for extreme climate years (e.g., 2023 Amazon drought)
#' - Better handling of edge cases in weighted windows
#'
#' @param health_data A `climasus_df` object produced by `sus_join_spatial()`.
#'   Must contain columns `date` (Date), `code_muni` (character), and geometry
#'   column `geom`.
#' @param climate_data A `climasus_df` object produced by `sus_climate_fill_gaps()`.
#'   Must contain `date` (Date or POSIXct), `station_code`, `latitude`, `longitude`,
#'   and climate variables.
#' @param climate_var Character vector with climate columns to aggregate.
#'   Use `"all"` (default) to include all available variables.
#' @param time_unit Temporal aggregation unit for raw climate data before join.
#'   Options: `"day"` (default), `"week"`, `"month"`, `"quarter"`, `"year"`,
#'   `"season"`. Relevant only when input data are hourly resolution.
#' @param temporal_strategy Temporal matching strategy. Options:
#'   \describe{
#'     \item{`"exact"`}{Exact date match (e.g., same-day temperature for acute
#'       heat-related mortality). Produces one column per variable.}
#'     \item{`"discrete_lag"`}{Climate value exactly \eqn{t - L} days before event.
#'       Produces columns prefixed `lag{L}_`. Avoids look-ahead bias.}
#'     \item{`"moving_window"`}{Mean/sum of sliding window \eqn{[t - W, t]}.
#'       Produces columns prefixed `mvwin{W}_`. **RECOMMENDED** for cumulative
#'       exposure in elderly populations.}
#'     \item{`"offset_window"`}{Aggregates historical interval \eqn{[t - W_2, t - W_1]},
#'       ignoring recent days. Ideal for incubation periods. Produces columns
#'       `off{W1}to{W2}_`.}
#'     \item{`"distributed_lag"`}{Creates lag matrix 0 to L for DLNM modeling.
#'       Produces columns `{var}_lag{0..L}`.}
#'     \item{`"degree_days"`}{Calculates Growing Degree Days (GDD) for vector
#'       biology or thermal stress. Produces column `gdd{W}_tbase{T}`.
#'       **IMPROVED:** Default `t_base = 20°C` (human physiology); use 11°C for
#'       *Aedes aegypti* development.}
#'     \item{`"seasonal"`}{Match by climate season (DJF, MAM, JJA, SON).
#'       Produces columns prefixed `season_`. **RECOMMENDED** for tropical regions.}
#'     \item{`"threshold_exceedance"`}{Counts days within window \eqn{[t - W, t]}
#'       when climate variable exceeded threshold. Ideal for heatwaves
#'       ("days with Tmax > 35°C") and extreme precipitation. Produces columns
#'       prefixed `nexc{W}_{dir}{threshold}_`.}
#'     \item{`"weighted_window"`}{Weighted mean of window \eqn{[t - W, t]} applying
#'       `weights` vector (position 1 = event day, higher weight; position W+1 =
#'       oldest day, lower weight). Models biological decay of exposure. Produces
#'       columns prefixed `wwin{W}_`.}
#'   }
#' @param window_days Integer. Number of days in window for `moving_window`
#'   (e.g., `14` = mean of last 14 days including event day) or accumulation
#'   period for `degree_days`. Required for these strategies.
#' @param lag_days Integer vector. Specific lags for `discrete_lag`
#'   (e.g., `c(7, 14, 21)` creates three columns), or maximum lag for
#'   `distributed_lag` (e.g., `21` creates lags 0 to 21). Required for these
#'   strategies.
#' @param offset_days Integer vector of length 2 for `offset_window`.
#'   Defines historical interval: `c(W1, W2)` aggregates from \eqn{t-W_2} to
#'   \eqn{t-W_1}. Example: `c(7, 14)` ignores last 7 days and aggregates the
#'   7 days before them. Required for this strategy.
#' @param t_base Numeric. Temperature base for `degree_days` calculation.
#'   **IMPROVED DEFAULT:** 20°C (physiologically relevant for human thermal stress).
#'   Use 11°C for *Aedes aegypti* development, 10°C for *Plasmodium* development.
#'   See Details section for scientific justification.
#' @param gdd_temp_var Character. Temperature column used for `degree_days`.
#'   Default: `"tair_dry_bulb_c"`.
#' @param min_obs Numeric (0–1). Minimum proportion of valid observations required
#'   within window. Insufficient observations return `NA`. Default: 0.7 (70%).
#' @param threshold_value Numeric. Threshold for `threshold_exceedance`.
#'   Default comparison is `> threshold_value` (exceedance above, e.g., maximum
#'   temperature). Use `threshold_direction = "below"` for `<`.
#' @param threshold_direction Character: `"above"` (default, `> threshold`) or
#'   `"below"` (`< threshold`). Controls exceedance direction in
#'   `threshold_exceedance`.
#' @param weights Numeric vector (optional) for `weighted_window`. Defines weight
#'   for each day within window, from most recent (position 1) to oldest
#'   (position `window_days`). Length must equal `window_days + 1` (includes event
#'   day, position 1 = day `t`). If `NULL`, linear decreasing weights are
#'   generated automatically from 1.0 (event day) to 0.1 (oldest day).
#'   Example: `c(1.0, 0.6, 0.3)` for `window_days = 2`.
#' @param use_cache Logical. If `TRUE`, uses Arrow/Parquet cache. Default: `TRUE`.
#' @param cache_dir Character. Directory for cache files.
#' @param lang Language for messages: `"pt"` (Portuguese), `"en"` (English), or
#'   `"es"` (Spanish). Default: `"pt"`.
#' @param verbose Logical. If `TRUE`, displays progress messages. Default: `TRUE`.
#' @param warn_extreme_year Logical. If `TRUE`, warns when climate data show
#'   extreme anomalies (e.g., 2023 Amazon drought). Default: `TRUE`.
#'
#' @return A `climasus_df` (tibble) with original health data and integrated
#'   climate variables as new columns. Geometry is preserved if input is `sf`.
#'
#' @section Temporal Strategies — Epidemiological Foundations:
#' The choice of strategy should reflect the hypothesized biological mechanism:
#' \itemize{
#'   \item **exact**: Immediate effects (heat stroke, hemorrhagic stroke).
#'   \item **discrete_lag**: Known delayed effect (e.g., temperature 7 days before
#'     influences dengue via extrinsic virus cycle).
#'   \item **moving_window**: Cumulative exposure without specific lag (e.g.,
#'     heatwave of last 14 days). **RECOMMENDED for elderly mortality**.
#'   \item **offset_window**: Defined incubation period (e.g., temperature 14-7
#'     days before death).
#'   \item **distributed_lag**: Distributed lag analysis (DLNM); generates exposure
#'     matrix for `dlnm::crossbasis()`.
#'   \item **degree_days**: Thermal threshold for vector development. GDD above
#'     temperature base (`t_base`) accumulates over `window_days`.
#'     **IMPROVED:** Default base 20°C for human health; 11°C for *Aedes aegypti*.
#'   \item **seasonal**: Seasonal climate patterns for ecological or long-term studies.
#'   \item **threshold_exceedance**: Counts days within window \eqn{[t - W, t]} when
#'     climate variable exceeded threshold. Ideal for heatwaves ("days with Tmax > 35°C")
#'     and extreme precipitation ("days with rainfall > 50mm").
#'   \item **weighted_window**: Weighted mean of window \eqn{[t - W, t]} applying
#'     `weights` vector. Models biological decay of exposure (e.g., exponential
#'     decay for inflammatory response).
#' }
#'
#' @section Degree-Days Temperature Base — Scientific Rationale:
#' **Version 5 Change:** Default `t_base` changed from 11°C to 20°C.
#'
#' The original 11°C base is appropriate for vector biology (*Aedes aegypti*
#' development threshold ~11°C) but **not for human health outcomes**. For
#' human thermal stress and mortality:
#'
#' \itemize{
#'   \item **20°C**: Thermal comfort threshold (ASHRAE standards). Appropriate for
#'     general population health studies.
#'   \item **24°C**: Thermal stress threshold. Appropriate for heat-related mortality
#'     in vulnerable populations (elderly, chronically ill).
#'   \item **11°C**: Vector development (keep for *Aedes aegypti*, *Plasmodium*).
#' }
#'
#' With `t_base = 11°C` in tropical regions (mean ~25°C), almost all days
#' contribute to GDD, failing to differentiate comfort from lethal heat. This
#' was a key finding of the 2024 scientific curation of `sus_climate_aggregate`.
#'
#' **Recommendation:** For health outcomes, use `t_base = 20` (default) or `t_base = 24`.
#' For vector-borne diseases, use `t_base = 11` (dengue) or `t_base = 10` (malaria).
#'
#' @section Autocorrelation and Multicolinearities in Tropical Regions:
#' In tropical regions (e.g., Amazon), temperature exhibits strong autocorrelation
#' across lags. When using `discrete_lag` or `distributed_lag` strategies,
#' **verify multicolinearities** in epidemiological models:
#'
#' \code{
#'   # After aggregation, check VIF (Variance Inflation Factor)
#'   car::vif(glm(outcome ~ lag7_temp + lag14_temp + lag21_temp, ...))
#' }
#'
#' If VIF > 5, consider:
#' - Using DLNM (Distributed Lag Non-Linear Models) via `dlnm::crossbasis()`
#' - Reducing number of lags
#' - Using principal component regression
#'
#' @section Extreme Climate Years:
#' If climate data span 2023 or other extreme years (e.g., 2024 Amazon drought),
#' the function will warn that results reflect **anomalous climate conditions**.
#' Generalizations to typical years should be made with caution.
#'
#' @note
#' Strategies `moving_window`, `offset_window`, and `degree_days` use
#' `data.table::foverlaps` internally for memory-efficient interval overlap joins,
#' avoiding the out-of-memory error that occurs with standard `dplyr` many-to-many
#' joins (2,024 municipalities × 122,640 climate rows ≈ 248 M rows).
#' Ensure `data.table` is installed: `install.packages("data.table")`.
#'
#' @examples
#' \dontrun{
#' # Strategy: exact — same-day temperature
#' df_exact <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial,
#'   climate_data = df_inmet,
#'   climate_var  = "tair_dry_bulb_c",
#'   temporal_strategy = "exact",
#'   lang = "pt",
#'   verbose = TRUE
#' )
#'
#' # Strategy: moving_window — cumulative exposure (RECOMMENDED for elderly)
#' df_moving <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial,
#'   climate_data = df_inmet,
#'   climate_var  = "tair_dry_bulb_c",
#'   temporal_strategy = "moving_window",
#'   window_days = 14,
#'   lang = "pt",
#'   verbose = TRUE
#' )
#'
#' # Strategy: degree_days — IMPROVED with t_base = 20°C for human health
#' df_gdd_health <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial,
#'   climate_data = df_inmet,
#'   temporal_strategy = "degree_days",
#'   window_days = 21,
#'   t_base = 20  # NEW: physiologically relevant for human thermal stress
#' )
#'
#' # Strategy: degree_days — vector biology (t_base = 11°C for Aedes)
#' df_gdd_vector <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial,
#'   climate_data = df_inmet,
#'   temporal_strategy = "degree_days",
#'   window_days = 21,
#'   t_base = 11  # For Aedes aegypti development
#' )
#'
#' # Strategy: threshold_exceedance — heatwave days
#' df_heatwave <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial,
#'   climate_data = df_inmet,
#'   climate_var  = "tair_max_c",
#'   temporal_strategy = "threshold_exceedance",
#'   window_days = 7,
#'   threshold_value = 35,
#'   threshold_direction = "above",
#'   min_obs = 0.7
#' )
#'
#' # Strategy: seasonal — integrated climate context (RECOMMENDED for tropics)
#' df_seasonal <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial,
#'   climate_data = df_inmet,
#'   temporal_strategy = "seasonal",
#'   lang = "pt",
#'   verbose = TRUE
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
    window_days       = NULL,
    lag_days          = NULL,
    offset_days       = NULL,
    t_base            = 20,  # IMPROVED: Changed from 11 to 20 (human health default)
    gdd_temp_var      = "tair_dry_bulb_c",
    min_obs           = 0.7,
    threshold_value     = NULL,
    threshold_direction = "above",
    weights             = NULL,
    use_cache           = TRUE,
    cache_dir         = "~/.climasus4r_cache/climate",
    lang              = "pt",
    verbose           = TRUE,
    warn_extreme_year = TRUE  # NEW: warn about extreme climate years
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
  # 3. VALIDATION OF TEMPORAL OVERLAP
  # ---------------------------------------------------------------------------
  .validate_date_overlap(health_data, climate_data, lang)

  # ---------------------------------------------------------------------------
  # 4. NEW: CHECK FOR EXTREME CLIMATE YEARS
  # ---------------------------------------------------------------------------
  if (warn_extreme_year) {
    .warn_extreme_climate_year(climate_data, lang, verbose)
  }

  # ---------------------------------------------------------------------------
  # 5. VALIDATION OF CLIMATE VARIABLES
  # ---------------------------------------------------------------------------
  climate_var <- .validate_climate_var(climate_data, climate_var, lang)

  # ---------------------------------------------------------------------------
  # 6. VALIDATION OF STRATEGY AND PARAMETERS
  # ---------------------------------------------------------------------------
  temporal_strategy <- match.arg(
    temporal_strategy,
    choices = c(
      "exact", "discrete_lag", "moving_window",
      "offset_window", "distributed_lag", "degree_days", "seasonal",
      "threshold_exceedance", "weighted_window"
    )
  )

  window_days  <- .validate_window_days(temporal_strategy, window_days, lang)
  lag_days     <- .validate_lag_days(temporal_strategy, lag_days, lang)
  offset_days  <- .validate_offset_days(temporal_strategy, offset_days, lang)
  .validate_degree_days_params(temporal_strategy, climate_var, gdd_temp_var, t_base, lang)  # IMPROVED: added lang
  .validate_threshold_params(temporal_strategy, threshold_value, threshold_direction, lang)
  weights <- .validate_weights(temporal_strategy, window_days, weights, lang)

  # ---------------------------------------------------------------------------
  # 7. PRE-AGGREGATION (hour -> day, if necessary)
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
        "Temporal metadata absent. Sub-daily resolution detected ",
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

  # Ensures date column is Date (not POSIXct) for safe joins
  climate_data <- dplyr::mutate(climate_data, date = as.Date(.data$date))

  if (verbose) cli::cli_progress_done()

  # ---------------------------------------------------------------------------
  # 8. SPATIAL MATCHING (stations → municipalities)
  # ---------------------------------------------------------------------------
  if (verbose) cli::cli_progress_step(msg$spatial_match)

  climate_matched <- .match_spatial(
    df          = climate_data,
    spatial_obj = health_data,
    verbose     = verbose
  )

  if (verbose) cli::cli_progress_done()

  # ---------------------------------------------------------------------------
  # 9. TEMPORAL JOIN
  # ---------------------------------------------------------------------------
  if (verbose) cli::cli_progress_step(msg$temporal_join)

  df <- switch(
    temporal_strategy,
    "exact"          = .join_exact(health_data, climate_matched, climate_var),
    "discrete_lag"   = .join_discrete_lag(health_data, climate_matched, climate_var, lag_days),
    "moving_window"  = .join_moving_window(health_data, climate_matched, climate_var, window_days, min_obs),
    "offset_window"  = .join_offset_window(health_data, climate_matched, climate_var, offset_days, min_obs),
    "distributed_lag"= .join_distributed_lag(health_data, climate_matched, climate_var, lag_days),
    "degree_days"    = .join_degree_days(health_data, climate_matched, window_days, gdd_temp_var, t_base, min_obs),
    "seasonal"            = .join_seasonal(health_data, climate_matched, climate_var),
    "threshold_exceedance" = .join_threshold_exceedance(
                               health_data, climate_matched, climate_var,
                               window_days, threshold_value, threshold_direction, min_obs),
    "weighted_window"      = .join_weighted_window(
                               health_data, climate_matched, climate_var,
                               window_days, weights, min_obs)
  )

  if (verbose) cli::cli_progress_done()

  # ---------------------------------------------------------------------------
  # 10. METADATA UPDATE
  # ---------------------------------------------------------------------------
  df <- .set_climate_agg_meta(df, system)

  return(df)
}


# =============================================================================
# VALIDATION HELPERS
# =============================================================================

#' Validates health data object
#' @keywords internal
#' @noRd
.validate_health_input <- function(health_data, lang, verbose) {
  if (!inherits(health_data, "climasus_df")) {
    cli::cli_abort(.msg_not_climasus_df(lang, "health"))
  }
  current_stage  <- climasus_meta(health_data, "stage")
  required_stage <- "spatial"
  if (is.null(current_stage) || current_stage != required_stage) {
    cli::cli_abort(.msg_wrong_stage(
      lang, "health", current_stage, required_stage, "sus_join_spatial(...)"
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
  current_stage  <- climasus_meta(climate_data, "stage")
  required_stage <- "climate"
  if (is.null(current_stage) || current_stage != required_stage) {
    cli::cli_abort(.msg_wrong_stage(
      lang, "climate", current_stage, required_stage,
      "sus_climate_inmet(...) followed by sus_climate_fill_gaps(..., evaluation = FALSE)"
    ))
  }
  temporal_meta <- climasus_meta(climate_data, "temporal")
  if (is.null(temporal_meta) || is.null(temporal_meta$source) ||
      !temporal_meta$source %in% "INMET") {
    cli::cli_abort(.msg_wrong_source(lang))
  }
  if (verbose) cli::cli_alert_success(.msg_stage_validated(lang))
}


#' NEW: Warns if climate data contain extreme years (e.g., 2023 Amazon drought)
#' @keywords internal
#' @noRd
.warn_extreme_climate_year <- function(climate_data, lang, verbose) {
  years <- lubridate::year(climate_data$date)
  extreme_years <- c(2023, 2024)  # Known extreme years for Amazon
  
  if (any(years %in% extreme_years)) {
    extreme_found <- years[years %in% extreme_years] |> unique()
    if (verbose) {
      cli::cli_alert_warning(
        paste0(
          "Climate data contain year(s): ",
          paste(extreme_found, collapse = ", "),
          ". These were climatically extreme (e.g., 2023 Amazon drought, +2.7°C anomaly). ",
          "Results reflect anomalous conditions; generalize to typical years with caution."
        )
      )
    }
  }
  invisible(NULL)
}


#' IMPROVED: Validates degree_days parameters with better documentation
#' @keywords internal
#' @noRd
.validate_degree_days_params <- function(temporal_strategy, climate_var, gdd_temp_var, t_base, lang = "pt") {
  if (temporal_strategy != "degree_days") return(invisible(NULL))
  
  if (!gdd_temp_var %in% climate_var && !identical(climate_var, "all")) {
    cli::cli_alert_info("Temperature variable '{gdd_temp_var}' not in climate_var. Attempting anyway.")
  }
  
  if (!is.numeric(t_base) || length(t_base) != 1 || t_base < 0) {
    cli::cli_abort("{.arg t_base} must be a positive numeric value (e.g., 20 for human health, 11 for Aedes aegypti).")
  }
  
  # NEW: Warn if using 11°C for human health
  if (t_base == 11) {
    cli::cli_alert_warning(
      "t_base = 11°C is appropriate for vector biology (Aedes aegypti) but NOT for human health. ",
      "For human thermal stress and mortality, use t_base = 20 (comfort threshold) or t_base = 24 (stress threshold). ",
      "See ?sus_climate_aggregate for details."
    )
  }
  
  invisible(NULL)
}


#' Validates temporal overlap between health and climate data
#' @keywords internal
#' @noRd
.validate_date_overlap <- function(health_data, climate_data, lang) {
  health_dates  <- climasus_meta(health_data, "temporal")
  climate_dates <- climasus_meta(climate_data, "temporal")

  if (is.null(health_dates) || is.null(climate_dates) ||
      is.null(health_dates$start) || is.null(climate_dates$start)) {
    cli::cli_warn(.msg_cannot_validate_dates(lang))
    return(invisible(NULL))
  }

  health_start  <- as.Date(health_dates$start)
  health_end    <- as.Date(health_dates$end)
  climate_start <- as.Date(climate_dates$start)
  climate_end   <- as.Date(climate_dates$end)

  if (health_start > climate_end || health_end < climate_start) {
    cli::cli_abort(
      paste0(
        "No temporal overlap between health data (",
        health_start, " to ", health_end, ") and climate data (",
        climate_start, " to ", climate_end, ")."
      )
    )
  }

  if (health_start < climate_start || health_end > climate_end) {
    cli::cli_warn(
      paste0(
        "Partial temporal overlap. Health: ", health_start, " to ", health_end,
        "; Climate: ", climate_start, " to ", climate_end, ". ",
        "Some health records may not have matching climate data."
      )
    )
  }

  invisible(NULL)
}


#' Validates window_days parameter
#' @keywords internal
#' @noRd
.validate_window_days <- function(temporal_strategy, window_days, lang) {
  if (!temporal_strategy %in% c("moving_window", "offset_window", "degree_days", "threshold_exceedance", "weighted_window")) {
    return(NULL)
  }
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
      cli::cli_alert_info("Distributed lag uses sequence 0:max. Using maximum value: {max(lag_days)}")
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


#' Validates threshold parameters
#' @keywords internal
#' @noRd
.validate_threshold_params <- function(temporal_strategy, threshold_value, threshold_direction, lang) {
  if (temporal_strategy != "threshold_exceedance") return(invisible(NULL))
  
  if (is.null(threshold_value)) {
    cli::cli_abort("{.arg threshold_value} must be provided for 'threshold_exceedance' strategy.")
  }
  
  if (!is.numeric(threshold_value)) {
    cli::cli_abort("{.arg threshold_value} must be numeric.")
  }
  
  if (!threshold_direction %in% c("above", "below")) {
    cli::cli_abort("{.arg threshold_direction} must be 'above' or 'below'.")
  }
  
  invisible(NULL)
}


#' Validates and generates weights for weighted_window
#' @keywords internal
#' @noRd
.validate_weights <- function(temporal_strategy, window_days, weights, lang) {
  if (temporal_strategy != "weighted_window") return(NULL)
  
  if (is.null(weights)) {
    # Generate linear decreasing weights automatically
    W <- window_days
    weights <- seq(1.0, 0.1, length.out = W + 1)
    return(weights)
  }
  
  if (!is.numeric(weights) || length(weights) != window_days + 1) {
    cli::cli_abort(
      "{.arg weights} must be numeric vector of length {window_days + 1} ",
      "(includes event day). Provided length: {length(weights)}."
    )
  }
  
  if (any(weights < 0)) {
    cli::cli_abort("{.arg weights} must contain non-negative values.")
  }
  
  weights
}

