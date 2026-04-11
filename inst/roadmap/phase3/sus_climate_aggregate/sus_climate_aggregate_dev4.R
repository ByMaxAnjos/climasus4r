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
#' @param climate_data A `climasus_df` object produced by `sus_climate_fill()`.
#'   Must contain `date` (Date or POSIXct), `station_code`, `latitude`, `longitude`,
#'   and climate variables.
#' @param climate_var Character vector with climate columns to aggregate.
#'   Use `"all"` (default) to include all available variables.
#' @param time_unit Temporal aggregation unit for raw climate data before join.
#'   Options: `"day"` (default), `"week"`, `"month"`, `"quarter"`, `"year"`,
#'   `"season"`. Relevant only when input data are hourly resolution.
#' @param temporal_strategy Temporal matching strategy. Options:
#'   \describe{
#'     \item{`"exact"`}{Exact date match (same-day temperature for acute heat-related mortality).}
#'     \item{`"discrete_lag"`}{Climate value exactly L days before event.}
#'     \item{`"moving_window"`}{Mean/sum of sliding window [t-W, t]. **RECOMMENDED** for cumulative exposure.}
#'     \item{`"offset_window"`}{Aggregates historical interval [t-W2, t-W1], ignoring recent days.}
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
#'   If `NULL` (default), uses region-specific default: 20°C (tropical), 18°C (subtropical),
#'   15°C (temperate). Use 11°C for *Aedes aegypti* development, 10°C for *Plasmodium*.
#' @param gdd_temp_var Character. Temperature column for `degree_days`.
#'   Default: `"tair_dry_bulb_c"`.
#' @param min_obs Numeric (0–1). Minimum proportion of valid observations required
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
#' ## Temporal Strategies — Epidemiological Foundations
#'
#' The choice of strategy should reflect the hypothesized biological mechanism:
#'
#' - **exact**: Immediate effects (heat stroke, hemorrhagic stroke)
#' - **discrete_lag**: Known delayed effect (e.g., temperature 7 days before influences dengue)
#' - **moving_window**: Cumulative exposure without specific lag. **RECOMMENDED for elderly mortality**.
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
#' **Tropical (North, Northeast):** Mean temperature 24-28°C, recommended temp_base 20°C (human health),
#' heatwave threshold Tmax > 35°C, high autocorrelation.
#'
#' **Subtropical (Center-West, Southeast):** Mean temperature 18-26°C, recommended temp_base 18°C,
#' heatwave threshold Tmax > 32°C, moderate autocorrelation.
#'
#' **Temperate (South):** Mean temperature 12-24°C, recommended temp_base 15°C,
#' heatwave threshold Tmax > 30°C, **coldwave threshold Tmin < 5°C** (critical),
#' low autocorrelation.
#'
#' ## Causal Inference & Look-Ahead Bias
#'
#' This function uses **retroactive windows** [t-W, t], never symmetric windows [t-W, t+W].
#' The climate of day t+7 cannot cause a health event on day t. This design prevents
#' look-ahead bias, a common methodological error in environmental epidemiology.
#'
#' ## Autocorrelation in Tropical Regions
#'
#' Temperature exhibits strong autocorrelation in tropical regions (e.g., Amazon).
#' When using `discrete_lag` or `distributed_lag` strategies, verify multicolinearities
#' in epidemiological models using `car::vif()`. If VIF > 5, consider using DLNM
#' or reducing the number of lags.
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
#'   sus_climate_fill()
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
    climate_var       = "all",  # FIXED: Changed from "tair_dry_bulb_c"
    time_unit         = "day",
    temporal_strategy = "exact",
    climate_region    = "auto",
    window_days       = NULL,
    lag_days          = NULL,
    offset_days       = NULL,
    temp_base         = NULL,  # FIXED: Changed from 24
    gdd_temp_var      = "tair_dry_bulb_c",
    min_obs           = 0.7,
    threshold_value   = NULL,
    threshold_direction = "above",
    weights           = NULL,
    lang              = "pt",
    verbose           = TRUE
) {

  msg <- .get_messages(lang)

  # ---------------------------------------------------------------------------
  # 1. VALIDAÇÃO DOS DADOS DE SAÚDE
  # ---------------------------------------------------------------------------
  .validate_health_input(health_data, lang, verbose)
  system <- sus_meta(health_data, "system")

  # ---------------------------------------------------------------------------
  # 2. VALIDAÇÃO DOS DADOS CLIMÁTICOS
  # ---------------------------------------------------------------------------
  .validate_climate_input(climate_data, lang, verbose)

  # ---------------------------------------------------------------------------
  # 3. VALIDAÇÃO DA SOBREPOSIÇÃO TEMPORAL
  # ---------------------------------------------------------------------------
  .validate_date_overlap(health_data, climate_data, lang)

  # ---------------------------------------------------------------------------
  # 4. VALIDAÇÃO DAS VARIÁVEIS CLIMÁTICAS ALVO
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
  # 9. NEW: VALIDATE REGIONAL APPROPRIATENESS
  # ---------------------------------------------------------------------------
    .validate_regional_appropriateness(
      temporal_strategy, climate_region, temp_base, threshold_value, 
      region_defaults, verbose, lang
    )
  # ---------------------------------------------------------------------------
  # 5. VALIDAÇÃO DA ESTRATÉGIA E SEUS PARÂMETROS
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
  # 6. PRE-AGREGACAO TEMPORAL (hora -> dia, se necessario)
  # ---------------------------------------------------------------------------
  # A deteccao de resolucao horaria ocorre em dois niveis:
  #   (a) metadado temporal$unit == "hour"  — fonte primaria.
  #   (b) inspecao estrutural — fallback quando metadados estao ausentes.
  #       Criterio: se (n_linhas / n_datas_unicas / n_estacoes) > 2, os dados
  #       sao sub-diarios. Sem este fallback, dados horarios passam direto para
  #       o join e produzem valores ~24x inflados (ex.: GDD 168 000 em vez
  #       de ~357 para 21 dias com base 11 C, temperatura ~28 C).
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
        "Metadado temporal ausente. Resolucao sub-diaria detectada ",
        "(~", round(rows_per_station_day, 1), " obs/estacao/dia). ",
        "Agregando para resolucao diaria antes do join."
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
  # 7. MATCHING ESPACIAL (estações → municípios)
  # ---------------------------------------------------------------------------
  if (verbose) cli::cli_progress_step(msg$spatial_match)

  climate_matched <- .match_spatial(
    df          = climate_data,
    spatial_obj = health_data,
    verbose     = verbose
  )

  if (verbose) cli::cli_progress_done()

  # ---------------------------------------------------------------------------
  # 8. JOIN TEMPORAL
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

  if (verbose) cli::cli_progress_done()
  
  # ---------------------------------------------------------------------------
  # 9. CONSTRUÇÃO DO HISTÓRICO DE METADADOS
  # ---------------------------------------------------------------------------
  agg_details <- c()
  
  # Temporal strategy (principal)
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
      sprintf("GDD temp base: %.1f°C (var: %s)", temp_base, gdd_temp_var)
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
      sprintf("Cold wave threshold: < %.1f°C", threshold_value)
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
  # 9. ATUALIZAÇÃO DE METADADOS
  # ---------------------------------------------------------------------------
  df_agg <- .set_climate_agg_meta(df_agg, system, history_msg = history_msg)

  return(df_agg)
}


# =============================================================================
# HELPERS DE VALIDAÇÃO
# =============================================================================

#' Retorna lista de mensagens no idioma selecionado
#' @keywords internal
#' @noRd
.get_messages <- function(lang = "pt") {
  messages <- list(
    pt = list(
      aggregating   = "Agregando dados temporais...",
      spatial_match = "Realizando matching espacial...",
      temporal_join = "Integrando dados temporais..."
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



#' Valida o objeto de dados de saúde
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


#' Valida o objeto de dados climáticos
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
      "sus_climate_inmet(...) seguido de sus_climate_fill_inmet(..., evaluation = FALSE)"
    ))
  }
  temporal_meta <- sus_meta(climate_data, "temporal")
  if (is.null(temporal_meta) || is.null(temporal_meta$source) ||
      !temporal_meta$source %in% "INMET") {
    cli::cli_abort(.msg_wrong_source(lang))
  }
  if (verbose) cli::cli_alert_success(.msg_stage_validated(lang))
}


#' Valida a sobreposição temporal entre dados de saúde e climáticos
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


#' Valida e resolve a seleção de variáveis climáticas
#' @keywords internal
#' @noRd
.validate_climate_var <- function(climate_data, climate_var, lang) {
  known_climate_cols <- c(
    "patm_mb", "patm_max_mb", "patm_min_mb",
    "tair_dry_bulb_c", "tair_max_c", "tair_min_c",
    "dew_tmean_c", "dew_tmax_c", "dew_tmin_c",
    "rh_max_porc", "rh_min_porc", "rh_mean_porc",
    "rainfall_mm", "ws_gust_m_s", "ws_2_m_s", "wd_degrees", "sr_kj_m2"
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


#' Valida o parâmetro window_days
#' @keywords internal
#' @noRd
.validate_window_days <- function(temporal_strategy, window_days, lang) {
  if (!temporal_strategy %in% c("moving_window", "degree_days",
                               "threshold_exceedance", "weighted_window")) return(NULL)
  if (is.null(window_days)) {
    cli::cli_abort("{.arg window_days} deve ser fornecido para a estrategia '{temporal_strategy}'.")
  }
  if (!is.numeric(window_days) || length(window_days) != 1 || window_days < 1) {
    cli::cli_abort("{.arg window_days} deve ser um inteiro positivo.")
  }
  as.integer(window_days)
}


#' Valida o parâmetro lag_days
#' @keywords internal
#' @noRd
.validate_lag_days <- function(temporal_strategy, lag_days, lang) {
  if (!temporal_strategy %in% c("discrete_lag", "distributed_lag")) return(NULL)
  if (is.null(lag_days)) {
    cli::cli_abort("{.arg lag_days} deve ser fornecido para a estrategia '{temporal_strategy}'.")
  }
  if (!is.numeric(lag_days) || any(lag_days < 0)) {
    cli::cli_abort("{.arg lag_days} deve conter inteiros nao-negativos.")
  }
  if (temporal_strategy == "distributed_lag") {
    if (length(lag_days) > 1) {
      cli::cli_alert_info("Distributed lag usa sequencia 0:max. Usando valor maximo: {max(lag_days)}")
      return(as.integer(max(lag_days)))
    }
    return(as.integer(lag_days))
  }
  as.integer(sort(unique(lag_days)))
}


#' Valida o parâmetro offset_days
#' @keywords internal
#' @noRd
.validate_offset_days <- function(temporal_strategy, offset_days, lang) {
  if (temporal_strategy != "offset_window") return(invisible(NULL))
  if (is.null(offset_days) || length(offset_days) != 2) {
    cli::cli_abort("{.arg offset_days} deve ser um vetor numerico de comprimento 2 (e.g., c(7, 14)).")
  }
  if (!is.numeric(offset_days) || any(offset_days < 0)) {
    cli::cli_abort("{.arg offset_days} deve conter inteiros nao-negativos.")
  }
  as.integer(sort(offset_days))
}


#' Valida parâmetros de degree_days
#' @keywords internal
#' @noRd
.validate_degree_days_params <- function(temporal_strategy, climate_var, gdd_temp_var, temp_base) {
  if (temporal_strategy != "degree_days") return(invisible(NULL))
  if (!gdd_temp_var %in% climate_var && !identical(climate_var, "all")) {
    cli::cli_alert_info("Variavel de temperatura GDD '{gdd_temp_var}' nao esta em climate_var. Tentando usar mesmo assim.")
  }
  if (!is.numeric(temp_base) || length(temp_base) != 1 || temp_base < 0) {
    cli::cli_abort("{.arg temp_base} deve ser um valor numerico positivo (e.g., 11 para Aedes aegypti).")
  }
  invisible(NULL)
}


# =============================================================================
# MATCHING ESPACIAL
# =============================================================================

#' Matching Espacial: Atribui dados climáticos às unidades geográficas
#'
#' @description
#' Para cada município no `spatial_obj`, identifica a estação climatológica
#' mais próxima (menor distância geográfica entre o centroide municipal e as
#' coordenadas da estação) e replica os dados climáticos dessa estação para
#' o município. O resultado é um data frame climático em que cada linha já
#' possui a coluna `code_muni` correspondente, pronta para o join temporal.
#'
#' @keywords internal
#' @noRd
.match_spatial <- function(df, spatial_obj, verbose = FALSE) {
  required_cols <- c("station_name", "station_code", "latitude", "longitude")
  if (!all(required_cols %in% names(df))) {
    cli::cli_abort("Dados de estacao devem conter: {paste(required_cols, collapse = ', ')}")
  }
  if (!"code_muni" %in% names(spatial_obj)) {
    cli::cli_abort("spatial_obj deve conter a coluna 'code_muni'. Execute sus_spatial_join() antes.")
  }
  if (verbose) cli::cli_inform("Performing spatial matching of climate stations to municipalities")

  # Estações únicas como sf
  stations <- df %>%
    dplyr::distinct(.data$station_code, .data$station_name,
                    .data$latitude, .data$longitude) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4674, remove = FALSE)

  # Garante CRS compatível
  spatial_sf <- sf::st_as_sf(spatial_obj)
  if (!identical(sf::st_crs(spatial_sf), sf::st_crs(stations))) {
    spatial_sf <- sf::st_transform(spatial_sf, sf::st_crs(stations))
  }

  # Centroide de cada município (ponto dentro do polígono preferido)
  mun_points <- tryCatch(
    suppressWarnings(sf::st_point_on_surface(spatial_sf)),
    error = function(e) suppressWarnings(sf::st_centroid(spatial_sf))
  )

  # Índice da estação mais próxima para cada município
  nearest_idx <- sf::st_nearest_feature(mun_points, stations)
  distances   <- sf::st_distance(mun_points, stations[nearest_idx, ], by_element = TRUE)

  # Mapa único município → estação (sem duplicatas de municípios)
  mun_station_map <- dplyr::tibble(
    code_muni    = spatial_sf$code_muni,
    station_code = stations$station_code[nearest_idx],
    distance_km  = as.numeric(distances) / 1000
  )
  if ("name_muni" %in% names(spatial_sf)) {
    mun_station_map$name_muni <- spatial_sf$name_muni
  }

  if (verbose) {
    d <- mun_station_map$distance_km
    cli::cli_inform(c(
      "v" = "Spatial matching completed:",
      "i" = "{nrow(mun_station_map)} municipios vinculados a {dplyr::n_distinct(stations$station_code)} estacoes.",
      " " = "Distancia (km): Min={round(min(d),1)} | Mediana={round(stats::median(d),1)} | Max={round(max(d),1)}"
    ))
  }

  # Replica dados climáticos por município:
  # cada linha do clima ganha N colunas de município (via left_join no mapa).
  # Relação é many-to-many (muitos dias × muitos municípios por estação),
  # mas é esperada e controlada porque o mapa tem exatamente 1 linha por
  # município.
  matched <- df %>%
    dplyr::left_join(mun_station_map, by = "station_code",
                     relationship = "many-to-many")

  # Reordena colunas de identificação para facilitar inspeção
  id_cols <- intersect(
    c("code_muni", "name_muni", "station_code", "station_name", "distance_km"),
    names(matched)
  )
  matched <- dplyr::select(matched, dplyr::all_of(id_cols), dplyr::everything())

  return(matched)
}


# =============================================================================
# IMPLEMENTAÇÕES DAS ESTRATÉGIAS TEMPORAIS
# =============================================================================

#' Exact: correspondência exata de data
#' @keywords internal
#' @noRd
.join_exact <- function(health_data, climate_data, target_vars) {
  # health_data$date é Date; climate_data$date foi convertido para Date acima
  climate_sel <- climate_data %>%
    dplyr::select("date", "code_muni", dplyr::all_of(target_vars)) %>%
    # Deduplicação: se houver múltiplas linhas para mesma data+município,
    # retém a primeira (não deve ocorrer após agregação diária)
    dplyr::distinct(.data$date, code_muni, .keep_all = TRUE)

  health_data %>%
    dplyr::left_join(climate_sel, by = c("date", "code_muni")) %>%
    dplyr::relocate(dplyr::any_of(c("date", "code_muni")))
}


#' Discrete Lag: valores climáticos exatamente L dias antes do evento
#'
#' @description
#' Para cada defasagem `lag` em `lag_days`, desloca a data do evento `t` para
#' `t - lag` e busca o valor climático correspondente. Isso é equivalente
#' a "qual era o clima L dias antes desta morte?". Não há look-ahead bias
#' porque apenas datas passadas são consultadas.
#'
#' Correção crítica em relação à versão anterior: o join final era feito por
#' `health_id_vars` (todas as colunas do health_data), o que criava um
#' produto cartesiano explosivo. Agora o join é feito por uma chave surrogate
#' `.row_id` injetada e removida após cada iteração.
#'
#' @keywords internal
#' @noRd
.join_discrete_lag <- function(health_data, climate_data, target_vars, lag_days) {
  # Chave surrogate para identificar cada linha do health_data univocamente
  health_keyed <- dplyr::mutate(health_data, .row_id = dplyr::row_number())
  result <- health_keyed

  climate_sel <- climate_data %>%
    dplyr::select("date", "code_muni", dplyr::all_of(target_vars)) %>%
    dplyr::distinct(date, code_muni, .keep_all = TRUE)

  for (lag in lag_days) {
    # Cria tabela de lookup: .row_id + date histórica + code_muni
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


#' Overlap join via data.table::foverlaps (uso interno)
#'
#' @description
#' Núcleo de desempenho compartilhado por `moving_window`, `offset_window` e
#' `degree_days`. Usa `data.table::foverlaps` para fazer o join condicional
#' de intervalo **sem materializar o produto cartesiano** entre municípios e
#' datas climáticas.
#'
#' O dplyr padrão (`left_join` por `code_muni` + filtro posterior) precisa
#' alocar a tabela completa antes de filtrar:
#'   2024 municípios × 122 640 linhas climáticas ≈ 248 M linhas → OOM crash.
#'
#' `foverlaps` utiliza um índice binário sobre os intervalos do clima,
#' resolvendo em O(n log n) e alocando apenas as linhas que satisfazem
#' o filtro temporal.
#'
#' @param health_dt   `data.table` com colunas `.row_id`, `code_muni`,
#'   `.win_start` (Date/integer), `.win_end` (Date/integer).
#' @param climate_dt  `data.table` com colunas `code_muni`, `date_int`
#'   (integer), e as variáveis climáticas.
#' @param target_vars Vetor de nomes de variáveis climáticas a incluir.
#'
#' @return `data.table` com linhas que satisfazem
#'   `.win_start <= date_int <= .win_end`, por `code_muni`.
#'
#' @keywords internal
#' @noRd
.foverlaps_window <- function(health_dt, climate_dt, target_vars) {
  # foverlaps exige que ambas as tabelas tenham a MESMA chave de intervalo.
  # Representamos datas como inteiros (dias desde origem) para evitar
  # problemas de tipo com Date vs integer no data.table.
  # health_dt  deve ter: code_muni, .win_start_int, .win_end_int
  # climate_dt deve ter: code_muni, date_int, date_int (coluna duplicada
  #   = intervalo degenerado [date, date])

  data.table::setkeyv(climate_dt, c("code_muni", "date_int", "date_int2"))
  data.table::setkeyv(health_dt,  c("code_muni", ".win_start_int", ".win_end_int"))

  joined <- data.table::foverlaps(
    health_dt,
    climate_dt,
    by.x = c("code_muni", ".win_start_int", ".win_end_int"),
    by.y = c("code_muni", "date_int",        "date_int2"),
    type = "any",
    mult = "all",
    nomatch = NA   # preserva linhas de saúde sem correspondência (left join)
  )
  joined
}


#' Moving Window: janela deslizante direita [t - W, t]
#'
#' @description
#' Agrega os `W + 1` dias imediatamente anteriores (inclusive) ao evento,
#' usando `data.table::foverlaps` para o overlap join por intervalo de data.
#' Isso evita a materialização do produto cartesiano que causava estouro de
#' memória na implementação original com dplyr `many-to-many`.
#'
#' Complexidade: O(n log n) em vez de O(n × m).
#'
#' @keywords internal
#' @noRd
.join_moving_window <- function(health_data, climate_data, target_vars, window_days, min_obs) {
  window_size    <- window_days + 1L
  agg_rule       <- .build_agg_rules(target_vars)
  health_id_vars <- setdiff(names(health_data), "geom")

  # --- Prepara data.table do clima (intervalo degenerado: [date, date]) ------
  climate_dt <- data.table::as.data.table(
    dplyr::select(climate_data, "code_muni", "date", dplyr::all_of(target_vars))
  )
  climate_dt[, date_int  := as.integer(date)]
  climate_dt[, date_int2 := date_int]          # intervalo [d, d]
  climate_dt[, date := NULL]

  # --- Prepara data.table de saúde com janela [t-W, t] ----------------------
  health_dt <- data.table::as.data.table(
    dplyr::select(sf::st_drop_geometry(health_data),
                  dplyr::all_of(health_id_vars))
  )
  health_dt[, .row_id       := .I]
  health_dt[, .win_start_int := as.integer(date) - window_days]
  health_dt[, .win_end_int   := as.integer(date)]

  # --- Overlap join ----------------------------------------------------------
  joined <- .foverlaps_window(health_dt, climate_dt, target_vars)

  # --- Agrega por linha de saúde × variável ----------------------------------
  id_plus_row <- c(health_id_vars, ".row_id")

  result <- joined[
    ,
    lapply(
      stats::setNames(target_vars, target_vars),
      function(v) {
        vals     <- get(v)
        n_valid  <- sum(!is.na(vals))
        prop     <- n_valid / window_size
        if (prop < min_obs) return(NA_real_)
        rule <- agg_rule$agg_type[agg_rule$climate_var == v]
        if (identical(rule, "sum"))           return(sum(vals, na.rm = TRUE))
        if (identical(rule, "mean_circular")) return(.circular_mean(vals))
        mean(vals, na.rm = TRUE)
      }
    ),
    by = id_plus_row
  ]
  # Renomeia com prefixo e remove .row_id
  old_nms <- target_vars
  new_nms <- paste0("mvwin", window_days, "_", target_vars)
  data.table::setnames(result, old_nms, new_nms)
  result[, .row_id := NULL]
  .set_climate_agg_meta(result, system = NULL, history_msg = NULL)
  #dplyr::as_tibble(result)
}


#' Offset Window: agrega o intervalo histórico [t - W2, t - W1]
#'
#' @description
#' Ignora os `W1` dias mais recentes e agrega os `(W2 - W1 + 1)` dias
#' anteriores a eles. Útil para modelar períodos de incubação ou latência
#' biológica. Usa `data.table::foverlaps` para eficiência de memória.
#'
#' Exemplo: `offset_days = c(7, 14)` captura exposição de `t-14` a `t-7`,
#' ignorando a semana imediatamente anterior ao evento.
#'
#' @keywords internal
#' @noRd
.join_offset_window <- function(health_data, climate_data, target_vars, offset_days, min_obs) {
  w1          <- offset_days[1]         # mais próximo ao evento
  w2          <- offset_days[2]         # mais distante
  window_size <- (w2 - w1) + 1L
  agg_rule    <- .build_agg_rules(target_vars)
  health_id_vars <- setdiff(names(health_data), "geom")

  # --- Prepara data.table do clima -------------------------------------------
  climate_dt <- data.table::as.data.table(
    dplyr::select(climate_data, "code_muni", "date", dplyr::all_of(target_vars))
  )
  climate_dt[, date_int  := as.integer(date)]
  climate_dt[, date_int2 := date_int]
  climate_dt[, date := NULL]

  # --- Prepara data.table de saúde com janela [t-W2, t-W1] ------------------
  health_dt <- data.table::as.data.table(
    dplyr::select(sf::st_drop_geometry(health_data),
                  dplyr::all_of(health_id_vars))
  )
  health_dt[, .row_id        := .I]
  health_dt[, .win_start_int := as.integer(date) - w2]
  health_dt[, .win_end_int   := as.integer(date) - w1]

  # --- Overlap join ----------------------------------------------------------
  joined <- .foverlaps_window(health_dt, climate_dt, target_vars)

  # --- Agrega ----------------------------------------------------------------
  id_plus_row <- c(health_id_vars, ".row_id")

  result <- joined[
    ,
    lapply(
      stats::setNames(target_vars, target_vars),
      function(v) {
        vals    <- get(v)
        n_valid <- sum(!is.na(vals))
        prop    <- n_valid / window_size
        if (prop < min_obs) return(NA_real_)
        rule <- agg_rule$agg_type[agg_rule$climate_var == v]
        if (identical(rule, "sum"))           return(sum(vals, na.rm = TRUE))
        if (identical(rule, "mean_circular")) return(.circular_mean(vals))
        mean(vals, na.rm = TRUE)
      }
    ),
    by = id_plus_row
  ]

  old_nms <- target_vars
  new_nms <- paste0("off", w1, "to", w2, "_", target_vars)
  data.table::setnames(result, old_nms, new_nms)
  result[, .row_id := NULL]

  .set_climate_agg_meta(result, system = NULL, history_msg = NULL)
}


#' Distributed Lag: matriz de defasagens 0…L para DLNM
#'
#' @description
#' Gera colunas `{var}_lag0`, `{var}_lag1`, …, `{var}_lag{L}` para uso
#' direto em `dlnm::crossbasis()`. A lógica é: para o lag `l`, busca o
#' valor climático em `t - l` (i.e., *l* dias antes do evento). Isso é
#' implementado adicionando `l` dias à coluna de data do clima, de modo que
#' a data climática `t_clim` coincida com a data do evento `t` quando
#' `t_clim = t - l ⟺ t_clim + l = t`.
#'
#' Correção em relação à versão anterior: o sinal estava correto (+ days(l)),
#' mas o `bind_cols` acumulava colunas duplicadas. Agora usa `.row_id`.
#'
#' @keywords internal
#' @noRd
.join_distributed_lag <- function(health_data, climate_data, target_vars, max_lag) {
  health_keyed <- dplyr::mutate(health_data, .row_id = dplyr::row_number())

  climate_sel <- climate_data %>%
    dplyr::select("date", "code_muni", dplyr::all_of(target_vars)) %>%
    dplyr::distinct(.data$date, code_muni, .keep_all = TRUE)

  result <- health_keyed

  for (l in seq(0L, as.integer(max_lag))) {
    # Desloca o calendário climático +l dias → alinha com o evento
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


#' Degree Days: Graus-Dia de Desenvolvimento acumulados
#'
#' @description
#' Calcula \eqn{GDD = \sum_{d=t-W}^{t} \max(0, T_d - T_{base})} para cada
#' registro de saúde, usando `data.table::foverlaps` para eficiência de memória.
#' Utilizado para modelar o desenvolvimento térmico de vetores artrópodes
#' (e.g., *Aedes aegypti*): GDD acima de `temp_base` acumula-se ao longo de
#' `window_days` dias antes do evento.
#'
#' @keywords internal
#' @noRd
.join_degree_days <- function(health_data, climate_data, window_days, temp_var, temp_base, min_obs) {
  window_size    <- window_days + 1L
  health_id_vars <- setdiff(names(health_data), "geom")

  if (!temp_var %in% names(climate_data)) {
    cli::cli_abort("Variavel de temperatura '{temp_var}' nao encontrada para calculo de graus-dia.")
  }

  # --- Prepara data.table do clima -------------------------------------------
  climate_dt <- data.table::as.data.table(
    dplyr::select(climate_data, "code_muni", "date", temp = dplyr::all_of(temp_var))
  )
  climate_dt[, date_int  := as.integer(date)]
  climate_dt[, date_int2 := date_int]
  climate_dt[, date := NULL]

  # --- Prepara data.table de saúde com janela [t-W, t] ----------------------
  health_dt <- data.table::as.data.table(
    dplyr::select(sf::st_drop_geometry(health_data),
                  dplyr::all_of(health_id_vars))
  )
  health_dt[, .row_id        := .I]
  health_dt[, .win_start_int := as.integer(date) - window_days]
  health_dt[, .win_end_int   := as.integer(date)]

  # --- Overlap join ----------------------------------------------------------
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

  # --- Calcula GDD por linha de saúde ----------------------------------------
  id_plus_row  <- c(health_id_vars, ".row_id")
  col_out_name <- paste0("gdd", window_days, "_tbase", temp_base)

  result <- joined[
    ,
    .(
      n_obs    = sum(!is.na(temp)),
      prop_obs = sum(!is.na(temp)) / window_size,
      gdd      = sum(pmax(0, temp - temp_base), na.rm = TRUE)
    ),
    by = id_plus_row
  ]

  result[prop_obs < min_obs, gdd := NA_real_]
  result[, prop_obs := NULL]
  result[, n_obs    := NULL]
  result[, .row_id  := NULL]
  data.table::setnames(result, "gdd", col_out_name)

  .set_climate_agg_meta(result, system = NULL, history_msg = NULL)
}


#' Seasonal: correspondência por estação climatológica (DJF/MAM/JJA/SON)
#'
#' @description
#' Associa cada registro de saúde às médias/somas climatológicas sazonais da
#' estação correspondente. As estações seguem a convenção meteorológica do
#' Hemisfério Sul: DJF (verão), MAM (outono), JJA (inverno), SON (primavera).
#' Requer ao menos 60 observações diárias válidas por estação-ano-variável
#' para incluir o valor.
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


#' Valida parametros de threshold_exceedance
#' @keywords internal
#' @noRd
.validate_threshold_params <- function(temporal_strategy, threshold_value, threshold_direction, lang) {
  if (temporal_strategy != "threshold_exceedance") return(invisible(NULL))
  if (is.null(threshold_value) || !is.numeric(threshold_value) || length(threshold_value) != 1) {
    cli::cli_abort("{.arg threshold_value} deve ser um numero escalar para a estrategia \'threshold_exceedance\'.")
  }
  if (!threshold_direction %in% c("above", "below")) {
    cli::cli_abort("{.arg threshold_direction} deve ser \'above\' ou \'below\'.")
  }
  invisible(NULL)
}


#' Valida e constroi vetor de pesos para weighted_window
#' @keywords internal
#' @noRd
.validate_weights <- function(temporal_strategy, window_days, weights, lang) {
  if (temporal_strategy != "weighted_window") return(NULL)
  n_expected <- window_days + 1L
  if (is.null(weights)) {
    # Pesos lineares automaticos: 1.0 (dia do evento) ate ~0.1 (dia mais antigo)
    weights <- seq(1.0, 0.1, length.out = n_expected)
    cli::cli_alert_info(
      paste0("Pesos nao fornecidos. Usando decaimento linear automatico: ",
             "1.0 (dia do evento) a ", round(min(weights), 2), " (dia mais antigo).")
    )
    return(weights)
  }
  if (!is.numeric(weights) || any(weights < 0)) {
    cli::cli_abort("{.arg weights} deve ser um vetor numerico nao-negativo.")
  }
  if (length(weights) != n_expected) {
    cli::cli_abort(paste0(
      "{.arg weights} deve ter comprimento window_days + 1 = ", n_expected,
      " (dia do evento + ", window_days, " dias anteriores). ",
      "Comprimento fornecido: ", length(weights), "."
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
  cols_sum  <- c("rainfall_mm", "sr_kj_m2")
  
  cols_mean <- c(
    "patm_mb", "patm_max_mb", "patm_min_mb",
    "tair_dry_bulb_c", "dew_tmean_c",
    "rh_mean_porc", "wd_degrees", "ws_2_m_s"
  )
  
  cols_max  <- c("tair_max_c", "dew_tmax_c", "rh_max_porc", "ws_gust_m_s")
  
  cols_min  <- c("tair_min_c", "dew_tmin_c", "rh_min_porc")
  
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
# THRESHOLD EXCEEDANCE
# =============================================================================

#' Threshold Exceedance: conta dias de eventos extremos na janela [t - W, t]
#'
#' @description
#' Para cada registro de saude, conta o numero de dias dentro da janela
#' \eqn{[t - W, t]} em que a variavel climatica ultrapassou o limiar
#' \code{threshold_value} na direcao especificada.
#'
#' Esta metrica captura a frequencia de exposicao extrema, que muitas vezes
#' prediz desfechos em saude melhor do que a media da janela:
#' \itemize{
#'   \item Ondas de calor: o corpo humano falha apos X dias consecutivos de
#'     calor extremo, nao pela temperatura media da semana.
#'   \item Inundacoes/Leptospirose: o risco sobe com o numero de dias de
#'     precipitacao intensa acima de 50mm, nao com a media.
#' }
#'
#' Usa \code{data.table::foverlaps} para eficiencia de memoria.
#' Colunas de saida: \code{nexc{W}_{dir}{thr}_{var}} (numero de dias de
#' excedencia) e \code{prop{W}_{dir}{thr}_{var}} (proporcao sobre a janela).
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

  # --- Prepara data.table do clima ------------------------------------------
  climate_dt <- data.table::as.data.table(
    dplyr::select(climate_data, "code_muni", "date", dplyr::all_of(target_vars))
  )
  climate_dt[, date_int  := as.integer(date)]
  climate_dt[, date_int2 := date_int]
  climate_dt[, date := NULL]

  # --- Prepara data.table de saude com janela [t-W, t] ----------------------
  health_dt <- data.table::as.data.table(
    dplyr::select(sf::st_drop_geometry(health_data), dplyr::all_of(health_id_vars))
  )
  health_dt[, .row_id        := .I]
  health_dt[, .win_start_int := as.integer(date) - window_days]
  health_dt[, .win_end_int   := as.integer(date)]

  # --- Overlap join ----------------------------------------------------------
  joined <- .foverlaps_window(health_dt, climate_dt, target_vars)

  # --- Conta excedencias por variavel ---------------------------------------
  id_plus_row <- c(health_id_vars, ".row_id")

  result <- joined[
    ,
    {
      out_list <- list()
      for (v in target_vars) {
        vals    <- get(v)
        n_obs   <- sum(!is.na(vals))
        prop_obs_val <- n_obs / window_size
        if (prop_obs_val < min_obs) {
          n_exc  <- NA_integer_
          p_exc  <- NA_real_
        } else {
          exc    <- if (threshold_direction == "above") vals > threshold_value
                    else vals < threshold_value
          n_exc  <- sum(exc, na.rm = TRUE)
          p_exc  <- n_exc / n_obs
        }
        exc_col  <- paste0("nexc",  window_days, "_", dir_label, thr_label, "_", v)
        prop_col <- paste0("pexc",  window_days, "_", dir_label, thr_label, "_", v)
        out_list[[exc_col]]  <- n_exc
        out_list[[prop_col]] <- round(p_exc, 4)
      }
      out_list
    },
    by = id_plus_row
  ]

  result[, .row_id := NULL]
  .set_climate_agg_meta(result, system = NULL, history_msg = NULL)
}


# =============================================================================
# WEIGHTED WINDOW
# =============================================================================

#' Weighted Window: media ponderada com decaimento temporal
#'
#' @description
#' Aplica um vetor de pesos \code{weights} aos dias da janela
#' \eqn{[t - W, t]}, dando mais importancia aos dias recentes (biologicamente
#' mais relevantes) e menos aos dias mais distantes. A media ponderada e:
#'
#' \deqn{\bar{X}_w = \frac{\sum_{d=0}^{W} w_d \cdot X_{t-d}}{\sum_{d=0}^{W} w_d \cdot \mathbb{1}[X_{t-d} \text{ nao e NA}]}}
#'
#' Onde \eqn{w_0} e o peso do dia do evento (posicao 1 no vetor
#' \code{weights}) e \eqn{w_W} e o peso do dia mais antigo (posicao W+1).
#'
#' Aplicacoes clinicas:
#' \itemize{
#'   \item PM2.5/asma: impacto forte nas primeiras 48h, dilui depois.
#'   \item Doencas infecciosas: modelar incubacao com mais peso no periodo
#'     de maior probabilidade de contato infeccioso.
#' }
#'
#' Para variaveis de soma (rainfall_mm, sr_kj_m2), a funcao calcula a
#' \emph{soma ponderada} em vez de media ponderada — comportamento
#' epidemiologicamente correto (precipitacao recente pesa mais no escoamento).
#' Para \code{wd_degrees}, usa media circular ponderada (von Mises).
#'
#' Usa \code{data.table::foverlaps} para eficiencia de memoria.
#' Colunas de saida: \code{wwin{W}_{var}}.
#'
#' @keywords internal
#' @noRd
.join_weighted_window <- function(health_data, climate_data, target_vars,
                                   window_days, weights, min_obs) 
                                   {
  window_size    <- window_days + 1L
  agg_rule       <- .build_agg_rules(target_vars)
  health_id_vars <- setdiff(names(health_data), "geom")

  # weights[1] = dia do evento (lag 0), weights[W+1] = dia mais antigo (lag W)
  # Revertemos para alinhar com a ordenacao do join (date_int decrescente nao
  # e garantida), entao usamos o deslocamento: peso = weights[lag + 1]
  # onde lag = win_end_int - date_int  (0 = dia do evento, W = mais antigo).

  # --- Prepara data.table do clima ------------------------------------------
  climate_dt <- data.table::as.data.table(
    dplyr::select(climate_data, "code_muni", "date", dplyr::all_of(target_vars))
  )
  climate_dt[, date_int  := as.integer(date)]
  climate_dt[, date_int2 := date_int]
  climate_dt[, date := NULL]

  # --- Prepara data.table de saude ------------------------------------------
  health_dt <- data.table::as.data.table(
    dplyr::select(sf::st_drop_geometry(health_data), dplyr::all_of(health_id_vars))
  )
  health_dt[, .row_id        := .I]
  health_dt[, .win_start_int := as.integer(date) - window_days]
  health_dt[, .win_end_int   := as.integer(date)]

  # --- Overlap join ----------------------------------------------------------
  joined <- .foverlaps_window(health_dt, climate_dt, target_vars)

  # Calcula o lag de cada observacao climatica relativa ao dia do evento
  # lag = 0 para dia do evento, lag = W para dia mais antigo
  joined[, .lag := .win_end_int - date_int]

  # --- Media/soma ponderada por variavel ------------------------------------
  id_plus_row <- c(health_id_vars, ".row_id")

  result <- joined[
    ,
    {
      out_list <- list()
      for (v in target_vars) {
        vals     <- get(v)
        lags     <- .lag
        w_vec    <- weights[lags + 1L]   # indexacao: lag 0 -> weights[1]
        not_na   <- !is.na(vals)
        n_obs    <- sum(not_na)
        prop_val <- n_obs / window_size
        if (prop_val < min_obs) {
          out_list[[v]] <- NA_real_
          next
        }
        rule <- agg_rule$agg_type[agg_rule$climate_var == v]
        val_w <- if (identical(rule, "sum")) {
          # Soma ponderada (grandezas acumulativas)
          sum(vals[not_na] * w_vec[not_na])
        } else if (identical(rule, "mean_circular")) {
          # Media circular ponderada via decomposicao vetorial
          rad   <- vals[not_na] * pi / 180
          w_ok  <- w_vec[not_na]
          atan2(sum(w_ok * sin(rad)), sum(w_ok * cos(rad))) * 180 / pi
        } else {
          # Media ponderada padrao
          sum(vals[not_na] * w_vec[not_na]) / sum(w_vec[not_na])
        }
        out_list[[v]] <- val_w
      }
      out_list
    },
    by = id_plus_row
  ]

  old_nms <- target_vars
  new_nms <- paste0("wwin", window_days, "_", target_vars)
  data.table::setnames(result, old_nms, new_nms)
  result[, .row_id := NULL]

  .set_climate_agg_meta(result, system = NULL, history_msg = NULL)
}

# =============================================================================
# FUNÇÕES UTILITÁRIAS
# =============================================================================

#' Regras de agregação por tipo de variável climática
#'
#' @description
#' Define se cada variável deve ser somada (`sum`), calculada como média
#' aritmética (`mean`) ou como média circular (`mean_circular`):
#' - `rainfall_mm`, `sr_kj_m2`: grandezas aditivas → soma.
#' - `wd_degrees`: ângulo circular → média vetorial (von Mises).
#' - Demais variáveis: médias aritméticas.
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


#' Média circular (direção do vento)
#'
#' @description
#' Calcula a média vetorial de ângulos em graus usando decomposição em
#' componentes seno/cosseno. Evita o artefato da média aritmética de ângulos
#' (e.g., média de 1° e 359° deve ser 0°, não 180°).
#'
#' @keywords internal
#' @noRd
.circular_mean <- function(x, na_rm = TRUE) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(NA_real_)
  rad <- x * pi / 180
  atan2(mean(sin(rad)), mean(cos(rad))) * 180 / pi
}


# #' Atualiza metadados do objeto climasus_df após agregação
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

.set_climate_agg_meta <- function(df, system, history_msg = NULL) {
  if (!inherits(df, "climasus_df")) {
    meta <- list(
      system   = system,
      stage    = "climate",
      type     = "agg",
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
    df <- sus_meta(df, stage = "climate", type = "agg")
  }
  
  return(df)
}

# =============================================================================
# HELPERS DE MENSAGENS DE ERRO
# =============================================================================

.msg_not_climasus_df <- function(lang, data_type) {
  pipeline_steps <- list(
    health = list(
      pt = c(
        "  1. sus_data_import() ou sus_data_read()",
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
    pt = "Entrada nao e um objeto climasus_df.",
    en = "Input is not a climasus_df object.",
    es = "La entrada no es un objeto climasus_df."
  )
  paste0(
    base[[lang]] %||% base[["en"]],
    "\nEsta funcao requer dados do pipeline CLIMASUS4r.\n\nPrepare seus dados:\n",
    paste(steps, collapse = "\n")
  )
}

.msg_wrong_stage <- function(lang, data_type, current, required, hint) {
  labels <- list(
    health  = list(pt = "saude",  en = "health",  es = "salud"),
    climate = list(pt = "clima",  en = "climate", es = "clima")
  )
  dl <- labels[[data_type]][[lang]] %||% labels[[data_type]][["en"]]
  base <- list(
    pt = paste0("Dados de {dl} possuem estagio invalido.\n",
                "Atual: {current} | Requerido: {required}\n",
                "Execute: {dl}_data <- {hint}"),
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
    pt = "Dados climaticos devem ser da fonte INMET.",
    en = "Climate data must be from INMET source.",
    es = "Los datos climaticos deben ser de fuente INMET."
  )
  msgs[[lang]] %||% msgs[["en"]]
}

.msg_stage_validated <- function(lang) {
  msgs <- list(
    pt = "Estagio de dados validado: agregacao permitida",
    en = "Data stage validated: aggregation allowed",
    es = "Etapa de datos validada: agregacion permitida"
  )
  msgs[[lang]] %||% msgs[["en"]]
}

.msg_cannot_validate_dates <- function(lang) {
  msgs <- list(
    pt = "Nao foi possivel validar sobreposicao de datas: metadados temporais ausentes.",
    en = "Cannot validate date overlap: temporal metadata missing.",
    es = "No se puede validar superposicion de fechas: metadatos temporales faltantes."
  )
  msgs[[lang]] %||% msgs[["en"]]
}

.msg_no_date_overlap <- function(lang, h_start, h_end, c_start, c_end) {
  msgs <- list(
    pt = paste0("Sem sobreposicao de datas entre saude e clima.\n",
                "Saude: ", h_start, " a ", h_end, "\n",
                "Clima: ", c_start, " a ", c_end),
    en = paste0("No date overlap between health and climate data.\n",
                "Health: ", h_start, " to ", h_end, "\n",
                "Climate: ", c_start, " to ", c_end)
  )
  msgs[[lang]] %||% msgs[["en"]]
}

.msg_partial_date_overlap <- function(lang, c_start, c_end, eff_start, eff_end) {
  msgs <- list(
    pt = paste0("Dados de saude ultrapassam o periodo climatico.\n",
                "Clima disponivel: ", c_start, " a ", c_end, "\n",
                "Periodo efetivo: ", eff_start, " a ", eff_end),
    en = paste0("Health data extends beyond climate range.\n",
                "Climate available: ", c_start, " to ", c_end, "\n",
                "Effective period: ", eff_start, " to ", eff_end)
  )
  msgs[[lang]] %||% msgs[["en"]]
}

.msg_no_climate_vars <- function(lang) {
  msgs <- list(
    pt = "Nenhuma variavel climatica reconhecida nos dados.",
    en = "No recognized climate variables found in data.",
    es = "No se encontraron variables climaticas reconocidas."
  )
  msgs[[lang]] %||% msgs[["en"]]
}

.msg_missing_climate_vars <- function(lang, missing, available) {
  msgs <- list(
    pt = paste0("Variaveis nao encontradas: ", paste(missing, collapse = ", "),
                ". Usando: ", paste(available, collapse = ", ")),
    en = paste0("Variables not found: ", paste(missing, collapse = ", "),
                ". Using: ", paste(available, collapse = ", "))
  )
  msgs[[lang]] %||% msgs[["en"]]
}

.msg_no_valid_climate_vars <- function(lang) {
  msgs <- list(
    pt = "Nenhuma variavel climatica valida apos validacao.",
    en = "No valid climate variables remaining after validation.",
    es = "No quedan variables climaticas validas."
  )
  msgs[[lang]] %||% msgs[["en"]]
}
