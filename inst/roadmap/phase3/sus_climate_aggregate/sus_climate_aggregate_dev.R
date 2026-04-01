#' Integration of climate and health data
#'
#' @description
#' `sus_climate_aggregate()` aggregates meteorological data to DATASUS health data
#' using epidemiologically rigorous temporal strategies.
#'
#' @param health_data A `climasus_df` object from `sus_join_spatial()`.
#' @param climate_data A `climasus_df` object from `sus_climate_fill_gaps()`.
#' @param climate_var Character vector specifying climate columns to aggregate.
#'   Use `"all"` (default) to include all available climate variables.
#' @param time_unit Character specifying temporal aggregation unit.
#'   Options: `"day"`, `"week"`, `"month"`, `"quarter"`, `"year"`, `"season"`.
#' @param temporal_strategy Character. Strategy for temporal matching:
#'   \describe{
#'     \item{"exact"}{Exact date matching (e.g., same-day heatstroke).}
#'     \item{"discrete_lag"}{Fetches climate at exactly $t - L$ days ago.}
#'     \item{"moving_window"}{Moving/Rolling window strictly from $[t - w, t]$. Avoids look-ahead bias.}
#'     \item{"offset_window"}{Aggregates a specific historical gap $[t - W_2, t - W_1]$, ignoring recent days. Ideal for incubation periods.}
#'     \item{"distributed_lag"}{Creates a matrix of lags (0 to L) for DLNM modeling.}
#'     \item{"degree_days"}{Calculates Growing Degree Days (GDD) for vector biology.}
#'     \item{"seasonal"}{Matches by climatological season (DJF, MAM, JJA, SON).}
#'   }
#' @param window_days Integer. Days for `moving_window` or accumulation period for `degree_days`.
#' @param lag_days Integer vector. Specific lags for `discrete_lag` (e.g., `c(7, 14, 21)`),
#'   or max lag for `distributed_lag` (e.g., `21` creates lags 0 to 21).
#' @param offset_days Integer vector of length 2 for `offset_window`. 
#'   Defines the historical gap to aggregate. e.g., `c(7, 14)` aggregates from $t-14$ to $t-7$.
#' @param t_base Numeric. Base temperature for `degree_days` calculation. Default 11 (suitable for Aedes aegypti).
#' @param gdd_temp_var Character. Temperature column to use for `degree_days`. Default `"tair_dry_bulb_c"`.
#' @param min_obs Numeric (0-1). Minimum proportion of observations required in window. Default: 0.7.
#' @param use_cache Logical. If TRUE, uses Arrow/Parquet cache. Default: TRUE.
#' @param cache_dir Character. Directory for cached files.
#' @param lang Character. Message language: `"pt"`, `"en"`, or `"es"`.
#' @param verbose Logical. If TRUE, prints progress messages.
#'
#' @return A `climasus_df` tibble with health data and integrated climate variables.
#'
#' @examples
#' \dontrun{
#' # Moving Window (Rolling average of previous 14 days)
#' result <- sus_climate_aggregate(
#'   health, climate,
#'   temporal_strategy = "moving_window",
#'   window_days = 14
#' )
#'
#' # Offset Window (Ignore last 7 days, aggregate from 14 to 7 days ago)
#' result <- sus_climate_aggregate(
#'   health, climate,
#'   temporal_strategy = "offset_window",
#'   offset_days = c(7, 14)
#' )
#' }
#'
#' @export
sus_climate_aggregate <- function(
    health_data,
    climate_data,
    climate_var = "all",
    time_unit = "day",
    temporal_strategy = "exact",
    window_days = NULL,
    lag_days = NULL,
    offset_days = NULL,
    t_base = 11,
    gdd_temp_var = "tair_dry_bulb_c",
    min_obs = 0.7,
    use_cache = TRUE,
    cache_dir = "~/.climasus4r_cache/climate",
    lang = "pt",
    verbose = TRUE
) {

  msg <- .get_messages(lang)

  # ===========================================================================
  # VALIDATE HEALTH DATA
  # ===========================================================================
  .validate_health_input(health_data, lang, verbose)
  system <- climasus_meta(health_data, "system")

  # ===========================================================================
  # VALIDATE CLIMATE DATA
  # ===========================================================================
  .validate_climate_input(climate_data, lang, verbose)

  # ===========================================================================
  # VALIDATE DATE RANGE OVERLAP
  # ===========================================================================
  .validate_date_overlap(health_data, climate_data, lang)

  # ===========================================================================
  # VALIDATE CLIMATE TARGET VARIABLE
  # ===========================================================================
  climate_var <- .validate_climate_var(climate_data, climate_var, lang)

  # ===========================================================================
  # STRATEGY & PARAMETER VALIDATION
  # ===========================================================================
  temporal_strategy <- match.arg(
    temporal_strategy,
    choices = c("exact", "discrete_lag", "moving_window", "offset_window", "distributed_lag", "degree_days", "seasonal")
  )

  window_days <- .validate_window_days(temporal_strategy, window_days, lang)
  lag_days <- .validate_lag_days(temporal_strategy, lag_days, lang)
  offset_days <- .validate_offset_days(temporal_strategy, offset_days, lang)
  .validate_degree_days_params(temporal_strategy, climate_var, gdd_temp_var, t_base)

  # ===========================================================================
  # PRE-AGGREGATION (e.g., hour to day)
  # ===========================================================================
  if (verbose) cli::cli_progress_step(msg$aggregating)

  climate_temporal_meta <- climasus_meta(climate_data, "temporal")
  if (!is.null(climate_temporal_meta$unit) && climate_temporal_meta$unit == "hour") {
    climate_data <- .aggregate_meteo_data(climate_data, time_unit = "day")
  }

  if (!identical(time_unit, "day")) {
    climate_data <- .aggregate_meteo_data(climate_data, time_unit = time_unit)
  }

  if (verbose) cli::cli_progress_done()

  # ===========================================================================
  # SPATIAL MATCHING
  # ===========================================================================
  if (verbose) cli::cli_progress_step(msg$spatial_match)

  climate_data_matched <- .match_spatial(
    df = climate_data,
    spatial_obj = health_data,
    verbose = verbose
  )

  if (verbose) cli::cli_progress_done()

  # ===========================================================================
  # TEMPORAL JOIN
  # ===========================================================================
  if (verbose) cli::cli_progress_step(msg$temporal_join)

  # Handle geometry carefully
  geom <- NULL
  if (sf::st_is_sfc(health_data$geom)) {
    geom <- health_data$geom
    health_data <- sf::st_drop_geometry(health_data)
  }

  df <- switch(
    temporal_strategy,
    "exact" = .join_exact(health_data, climate_data_matched, climate_var),
    "discrete_lag" = .join_discrete_lag(health_data, climate_data_matched, climate_var, lag_days),
    "moving_window" = .join_moving_window(health_data, climate_data_matched, climate_var, window_days, min_obs),
    "offset_window" = .join_offset_window(health_data, climate_data_matched, climate_var, offset_days, min_obs),
    "distributed_lag" = .join_distributed_lag(health_data, climate_data_matched, climate_var, lag_days),
    "degree_days" = .join_degree_days(health_data, climate_data_matched, window_days, gdd_temp_var, t_base, min_obs),
    "seasonal" = .join_seasonal(health_data, climate_data_matched, climate_var)
  )

  if (!is.null(geom)) {
    df <- sf::st_as_sf(df, sf_column_name = "geom", crs = sf::st_crs(geom))
    df$geom <- geom
  }
  if (verbose) cli::cli_progress_done()

  # ===========================================================================
  # UPDATE METADATA
  # ===========================================================================
  df <- .set_climate_agg_meta(df, system)

  return(df)
}

# ============================================================================
# VALIDATION HELPERS
# ============================================================================

#' Validate health data input
#' @keywords internal
#' @noRd
.validate_health_input <- function(health_data, lang, verbose) {
  if (!inherits(health_data, "climasus_df")) {
    cli::cli_abort(.msg_not_climasus_df(lang, "health"))
  }

  current_stage <- climasus_meta(health_data, "stage")
  required_stage <- "spatial"

  if (is.null(current_stage) || current_stage != required_stage) {
    cli::cli_abort(.msg_wrong_stage(
      lang = lang, data_type = "health", current = current_stage,
      required = required_stage, hint = "sus_join_spatial(...)"
    ))
  }

  if (verbose) cli::cli_alert_success(.msg_stage_validated(lang))
}


#' Validate climate data input
#' @keywords internal
#' @noRd
.validate_climate_input <- function(climate_data, lang, verbose) {
  if (!inherits(climate_data, "climasus_df")) {
    cli::cli_abort(.msg_not_climasus_df(lang, "climate"))
  }

  current_stage <- climasus_meta(climate_data, "stage")
  required_stage <- "climate"

  if (is.null(current_stage) || current_stage != required_stage) {
    cli::cli_abort(.msg_wrong_stage(
      lang = lang, data_type = "climate", current = current_stage,
      required = required_stage, hint = "sus_climate_inmet(...) followed by sus_climate_fill_gaps(..., evaluation = FALSE)"
    ))
  }

  temporal_meta <- climasus_meta(climate_data, "temporal")
  if (is.null(temporal_meta) || is.null(temporal_meta$source) || !temporal_meta$source %in% "INMET") {
    cli::cli_abort(.msg_wrong_source(lang))
  }

  if (is.null(temporal_meta$evaluation) || temporal_meta$evaluation) {
    cli::cli_abort(.msg_wrong_evaluation(lang))
  }

  if (verbose) cli::cli_alert_success(.msg_stage_validated(lang))
}


#' Validate date overlap between health and climate data
#' @keywords internal
#' @noRd
.validate_date_overlap <- function(health_data, climate_data, lang) {
  health_dates <- climasus_meta(health_data, "temporal")
  climate_dates <- climasus_meta(climate_data, "temporal")

  if (is.null(health_dates) || is.null(climate_dates)) {
    cli::cli_warn(.msg_cannot_validate_dates(lang))
    return(invisible(NULL))
  }

  health_start <- as.Date(health_dates$start); health_end <- as.Date(health_dates$end)
  climate_start <- as.Date(climate_dates$start); climate_end <- as.Date(climate_dates$end)

  if (health_end < climate_start || health_start > climate_end) {
    cli::cli_abort(.msg_no_date_overlap(lang, health_start, health_end, climate_start, climate_end))
  }

  if (health_start < climate_start || health_end > climate_end) {
    cli::cli_warn(.msg_partial_date_overlap(
      lang, climate_start, climate_end, max(health_start, climate_start), min(health_end, climate_end)
    ))
  }

  invisible(NULL)
}


#' Validate and resolve climate variable selection
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

  available_climate_cols <- intersect(known_climate_cols, colnames(climate_data))

  if (length(available_climate_cols) == 0) cli::cli_abort(.msg_no_climate_vars(lang))
  if (identical(climate_var, "all") || is.null(climate_var)) return(available_climate_cols)

  missing_vars <- setdiff(climate_var, available_climate_cols)
  if (length(missing_vars) > 0) {
    cli::cli_warn(.msg_missing_climate_vars(lang, missing_vars, available_climate_cols))
    climate_var <- intersect(climate_var, available_climate_cols)
  }

  if (length(climate_var) == 0) cli::cli_abort(.msg_no_valid_climate_vars(lang))
  return(climate_var)
}


#' Validate window_days parameter
#' @keywords internal
#' @noRd
.validate_window_days <- function(temporal_strategy, window_days, lang) {
  if (!temporal_strategy %in% c("moving_window", "degree_days")) return(NULL)
  
  if (is.null(window_days)) {
    cli::cli_abort("{.arg window_days} must be provided for '{temporal_strategy}' strategy.")
  }
  if (!is.numeric(window_days) || window_days < 0) {
    cli::cli_abort("{.arg window_days} must be a non-negative integer.")
  }
  as.integer(window_days)
}


#' Validate lag_days parameter
#' @keywords internal
#' @noRd
.validate_lag_days <- function(temporal_strategy, lag_days, lang) {
  if (!temporal_strategy %in% c("discrete_lag", "distributed_lag")) return(NULL)
  
  if (is.null(lag_days)) {
    cli::cli_abort("{.arg lag_days} must be provided for '{temporal_strategy}' strategy.")
  }
  if (!is.numeric(lag_days) || any(lag_days < 0)) {
    cli::cli_abort("{.arg lag_days} must be non-negative integers.")
  }
  
  if (temporal_strategy == "distributed_lag") {
    if (length(lag_days) > 1) {
      cli::cli_alert_info("Distributed lag uses sequence 0:max. Using max value: {max(lag_days)}")
      as.integer(max(lag_days))
    } else {
      as.integer(lag_days)
    }
  } else {
    as.integer(sort(unique(lag_days)))
  }
}


#' Validate offset_days parameter
#' @keywords internal
#' @noRd
.validate_offset_days <- function(temporal_strategy, offset_days, lang) {
  if (temporal_strategy != "offset_window") return(invisible(NULL))
  
  if (is.null(offset_days) || length(offset_days) != 2) {
    cli::cli_abort("{.arg offset_days} must be a numeric vector of length 2 (e.g., c(7, 14)) for '{temporal_strategy}'.")
  }
  if (!is.numeric(offset_days) || any(offset_days < 0)) {
    cli::cli_abort("{.arg offset_days} must be non-negative integers.")
  }
  # Sort to ensure W1 (closest to t) is first, W2 (furthest) is second
  as.integer(sort(offset_days))
}


#' Validate degree days parameters
#' @keywords internal
#' @noRd
.validate_degree_days_params <- function(temporal_strategy, climate_var, gdd_temp_var, t_base) {
  if (temporal_strategy != "degree_days") return(invisible(NULL))
  
  if (!gdd_temp_var %in% climate_var && !identical(climate_var, "all")) {
    cli::cli_alert_info("GDD temperature variable '{gdd_temp_var}' not explicitly in climate_var. Attempting to use it anyway.")
  }
  if (!is.numeric(t_base) || t_base < 0) {
    cli::cli_abort("{.arg t_base} must be a positive numeric value (e.g., 11 for Aedes aegypti).")
  }
  invisible(NULL)
}


# ============================================================================
# ERROR MESSAGE HELPERS
# ============================================================================

#' @keywords internal
#' @noRd
.msg_not_climasus_df <- function(lang, data_type) {
  pipeline_steps <- list(
    health = list(
      en = c("  1. Import: health_data <- sus_data_import() or sus_data_read()", "  2. Clean: health_data <- sus_data_clean_encoding()", "  3. Standardize: health_data <- sus_data_standardize()", "  4. Filter CID: health_data <- sus_data_filter_cid()", "  5. Filter demographics: health_data <- sus_data_filter_demographics()", "  6. Aggregate: health_data <- sus_data_aggregate()", "  7. Spatial join: health_data <- sus_join_spatial()"),
      pt = c("  1. Importar: health_data <- sus_data_import() ou sus_data_read()", "  2. Limpar: health_data <- sus_data_clean_encoding()", "  3. Padronizar: health_data <- sus_data_standardize()", "  4. Filtrar CID: health_data <- sus_data_filter_cid()", "  5. Filtrar demografia: health_data <- sus_data_filter_demographics()", "  6. Agregar: health_data <- sus_data_aggregate()", "  7. Juncao espacial: health_data <- sus_join_spatial()"),
      es = c("  1. Importar: health_data <- sus_data_import() o sus_data_read()", "  2. Limpiar: health_data <- sus_data_clean_encoding()", "  3. Estandarizar: health_data <- sus_data_standardize()", "  4. Filtrar CID: health_data <- sus_data_filter_cid()", "  5. Filtrar demografia: health_data <- sus_data_filter_demographics()", "  6. Agregar: health_data <- sus_data_aggregate()", "  7. Union espacial: health_data <- sus_join_spatial()")
    ),
    climate = list(
      en = c("  1. Import: climate_data <- sus_climate_inmet()", "  2. Fill gaps: climate_data <- sus_climate_fill_gaps(evaluation = FALSE)"),
      pt = c("  1. Importar: climate_data <- sus_climate_inmet()", "  2. Preencher falhas: climate_data <- sus_climate_fill_gaps(evaluation = FALSE)"),
      es = c("  1. Importar: climate_data <- sus_climate_inmet()", "  2. Rellenar fallos: climate_data <- sus_climate_fill_gaps(evaluation = FALSE)")
    )
  )
  steps <- pipeline_steps[[data_type]][[lang]] %||% pipeline_steps[[data_type]][["en"]]
  base_msg <- list(en = "Input is not a climasus_df object.", pt = "Entrada nao e um objeto climasus_df.", es = "La entrada no es un objeto climasus_df.")
  paste0(base_msg[[lang]] %||% base_msg[["en"]], "\n", "This function requires data from the CLIMASUS4r pipeline.\n\n", "Please prepare your data first:\n", paste(steps, collapse = "\n"))
}

.msg_wrong_stage <- function(lang, data_type, current, required, hint) {
  labels <- list(health = list(en = "health", pt = "saude", es = "salud"), climate = list(en = "climate", pt = "clima", es = "clima"))
  data_label <- labels[[data_type]][[lang]] %||% labels[[data_type]][["en"]]
  base_msg <- list(
    en = paste0("{data_type} data has invalid stage for aggregation.\n", "Current stage: {current}\n", "Required stage: {required}\n\n", "Please run:\n  {data_type}_data <- {hint}"),
    pt = paste0("Dados de {data_type} possuem estagio invalido para agregacao.\n", "Estagio atual: {current}\n", "Estagio requerido: {required}\n\n", "Por favor, execute:\n  {data_type}_data <- {hint}"),
    es = paste0("Los datos de {data_type} tienen etapa invalida para agregacion.\n", "Etapa actual: {current}\n", "Etapa requerida: {required}\n\n", "Por favor, ejecute:\n  {data_type}_data <- {hint}")
  )
  cli::cli_format_glue(base_msg[[lang]] %||% base_msg[["en"]], data_type = data_label, current = current %||% "unknown", required = required, hint = hint)
}

.msg_wrong_source <- function(lang) { msgs <- list(en = "Climate data must be from INMET source.", pt = "Dados climaticos devem ser da fonte INMET.", es = "Los datos climaticos deben ser de fuente INMET."); msgs[[lang]] %||% msgs[["en"]] }
.msg_wrong_evaluation <- function(lang) { msgs <- list(en = "Climate data must have evaluation = FALSE. Run sus_climate_fill_gaps(..., evaluation = FALSE).", pt = "Dados climaticos devem ter evaluation = FALSE. Execute sus_climate_fill_gaps(..., evaluation = FALSE).", es = "Los datos climaticos deben tener evaluation = FALSE. Ejecute sus_climate_fill_gaps(..., evaluation = FALSE)."); msgs[[lang]] %||% msgs[["en"]] }
.msg_stage_validated <- function(lang) { msgs = list(en = "Data stage validated: aggregation allowed", pt = "Estagio de dados validado: agregacao permitida", es = "Etapa de datos validada: agregacion permitida"); msgs[[lang]] %||% msgs[["en"]] }
.msg_cannot_validate_dates <- function(lang) { msgs = list(en = "Cannot validate date overlap: temporal metadata missing.", pt = "Nao foi possivel validar sobreposicao de datas: metadados temporais ausentes.", es = "No se puede validar superposicion de fechas: metadatos temporales faltantes."); msgs[[lang]] %||% msgs[["en"]] }

.msg_no_date_overlap <- function(lang, h_start, h_end, c_start, c_end) {
  msgs <- list(en = paste0("No date overlap between health and climate data.\n", "Health data: {h_start} to {h_end}\n", "Climate data: {c_start} to {c_end}"),
              pt = paste0("Sem sobreposicao de datas entre dados de saude e clima.\n", "Dados de saude: {h_start} a {h_end}\n", "Dados climaticos: {c_start} a {c_end}"),
              es = paste0("Sin superposicion de fechas entre datos de salud y clima.\n", "Datos de salud: {h_start} a {h_end}\n", "Datos climaticos: {c_start} a {c_end}"))
  cli::cli_format_glue(msgs[[lang]] %||% msgs[["en"]], h_start = h_start, h_end = h_end, c_start = c_start, c_end = c_end)
}

.msg_partial_date_overlap <- function(lang, c_start, c_end, effective_start, effective_end) {
  msgs <- list(en = paste0("Health data extends beyond climate data range.\n", "Climate data available: {c_start} to {c_end}\n", "Effective analysis period: {effective_start} to {effective_end}"),
              pt = paste0("Dados de saude ultrapassam o periodo dos dados climaticos.\n", "Dados climaticos disponiveis: {c_start} a {c_end}\n", "Periodo efetivo de analise: {effective_start} a {effective_end}"),
              es = paste0("Los datos de salud exceden el rango de datos climaticos.\n", "Datos climaticos disponibles: {c_start} a {c_end}\n", "Periodo efectivo de analisis: {effective_start} a {effective_end}"))
  cli::cli_format_glue(msgs[[lang]] %||% msgs[["en"]], c_start = c_start, c_end = c_end, effective_start = effective_start, effective_end = effective_end)
}

.msg_no_climate_vars <- function(lang) { msgs = list(en = "No recognized climate variables found in data.", pt = "Nenhuma variavel climatica reconhecida encontrada nos dados.", es = "No se encontraron variables climaticas reconocidas en los datos."); msgs[[lang]] %||% msgs[["en"]] }

.msg_missing_climate_vars <- function(lang, missing, available) {
  msgs <- list(en = "Climate variables not found: {missing}. Using available: {available}", pt = "Variaveis climaticas nao encontradas: {missing}. Usando disponiveis: {available}", es = "Variables climaticas no encontradas: {missing}. Usando disponibles: {available}")
  cli::cli_format_glue(msgs[[lang]] %||% msgs[["en"]], missing = paste(missing, collapse = ", "), available = paste(available, collapse = ", "))
}

.msg_no_valid_climate_vars <- function(lang) { msgs = list(en = "No valid climate variables remaining after validation.", pt = "Nenhuma variavel climatica valida apos validacao.", es = "No quedan variables climaticas validas despues de la validacion."); msgs[[lang]] %||% msgs[["en"]] }


# ============================================================================
# SPATIAL MATCHING
# ============================================================================

#' Spatial Matching: Assign Climate Data to Geographic Units
#' @keywords internal
#' @noRd
.match_spatial <- function(df, spatial_obj, verbose = FALSE) {
  required_cols <- c("station_name", "station_code", "latitude", "longitude")
  if (!all(required_cols %in% names(df))) cli::cli_abort("Station data must contain columns: {paste(required_cols, collapse = ', ')}")
  if (!"code_muni" %in% names(spatial_obj)) cli::cli_abort("spatial_obj must contain 'code_muni' column. Please use sus_join_spatial() first")
  if (verbose) cli::cli_inform("Performing spatial matching of climate stations to municipalities")

  stations <- df %>%
    dplyr::distinct(.data$station_code, .data$station_name, .data$latitude, .data$longitude) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4674, remove = FALSE)

  if (!identical(sf::st_crs(spatial_obj), sf::st_crs(stations))) {
    spatial_obj <- sf::st_transform(spatial_obj, sf::st_crs(stations))
  }

  spatial_points <- tryCatch({
    suppressWarnings(sf::st_point_on_surface(spatial_obj))
  }, error = function(e) {
    if (verbose) cli::cli_warn("Point on surface failed, falling back to centroid")
    suppressWarnings(sf::st_centroid(spatial_obj))
  })

  nearest_indices <- sf::st_nearest_feature(spatial_points, stations)
  distances <- sf::st_distance(spatial_points, stations[nearest_indices, ], by_element = TRUE)

  mun_station_map <- data.frame(
    code_muni = spatial_obj$code_muni,
    station_code = stations$station_code[nearest_indices],
    distance_km = as.numeric(distances) / 1000,
    stringsAsFactors = FALSE
  )
  if ("name_muni" %in% names(spatial_obj)) mun_station_map$name_muni <- spatial_obj$name_muni

  matched_data <- df %>% dplyr::inner_join(mun_station_map, by = "station_code", relationship = "many-to-many")
  
  id_cols <- intersect(c("code_muni", "name_muni", "station_code", "station_name", "distance_km"), names(matched_data))
  matched_data <- matched_data %>% dplyr::select(dplyr::all_of(id_cols), dplyr::everything())

  if (verbose) {
    dist_stats <- summary(mun_station_map$distance_km)
    cli::cli_inform(c("v" = "Spatial matching completed:", "i" = "{nrow(mun_station_map)} municipalities matched to {length(unique(stations$station_code))} stations.", " " = "Distance (km): Min={round(dist_stats[1],1)} | Median={round(dist_stats[3],1)} | Max={round(dist_stats[6],1)}"))
  }
  return(matched_data)
}


# ============================================================================
# TEMPORAL STRATEGY IMPLEMENTATIONS
# ============================================================================

#' Exact date matching
#' @keywords internal
#' @noRd
.join_exact <- function(health_data, climate_data, target_vars) {
  health_data %>%
    dplyr::left_join(
      climate_data %>% dplyr::select(.data$date, .data$code_muni, dplyr::all_of(target_vars)),
      by = c("date", "code_muni")
    ) %>%
    dplyr::relocate(dplyr::any_of(c("date", "code_muni")))
}

#' Discrete Lag: Fetch values at specific past days
#' @keywords internal
#' @noRd
.join_discrete_lag <- function(health_data, climate_data, target_vars, lag_days) {
  health_id_vars <- setdiff(names(health_data), "geom")
  result <- health_data

  for (lag in lag_days) {
    lagged_health <- health_data %>%
      dplyr::mutate(.hist_date = .data$date - lubridate::days(lag)) %>%
      dplyr::select(dplyr::all_of(health_id_vars), .data$.hist_date)

    joined <- lagged_health %>%
      dplyr::left_join(
        climate_data %>% dplyr::select(.data$date, .data$code_muni, dplyr::all_of(target_vars)) %>% dplyr::rename(.hist_date = .data$date),
        by = c("code_muni", ".hist_date")
      ) %>% dplyr::select(-.data$.hist_date)

    rename_cols <- stats::setNames(target_vars, paste0("lag", lag, "_", target_vars))
    joined <- dplyr::rename(joined, !!!rename_cols)
    result <- result %>% dplyr::left_join(joined, by = health_id_vars)
  }
  return(result)
}

#' Moving Window: Standard right-aligned rolling window [t - W, t]
#' @keywords internal
#' @noRd
.join_moving_window <- function(health_data, climate_data, target_vars, window_days, min_obs) {
  window_size <- window_days + 1
  agg_rule <- .build_agg_rules(target_vars)
  health_id_vars <- setdiff(names(health_data), "geom")

  health_win <- health_data %>% dplyr::mutate(.win_start = .data$date - window_days)
  climate_sel <- climate_data %>% dplyr::select(date, code_muni, dplyr::all_of(target_vars)) %>% dplyr::rename(date_climate = date)

  joined <- health_win %>%
    dplyr::left_join(climate_sel, by = "code_muni", relationship = "many-to-many") %>%
    dplyr::filter(.data$date_climate >= .win_start & .data$date_climate <= .data$date)

  out <- joined %>%
    tidyr::pivot_longer(cols = dplyr::all_of(target_vars), names_to = "climate_var", values_to = "value") %>%
    dplyr::left_join(agg_rule, by = "climate_var") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(health_id_vars)), climate_var) %>%
    dplyr::summarise(
      n_obs = dplyr::n(),
      prop_obs = n_obs / window_size,
      value_window = dplyr::case_when(
        dplyr::first(agg_type) == "sum" ~ sum(value, na.rm = TRUE),
        dplyr::first(agg_type) == "mean" ~ mean(value, na.rm = TRUE),
        dplyr::first(agg_type) == "mean_circular" ~ .circular_mean(value),
        TRUE ~ mean(value, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(value_window = dplyr::if_else(prop_obs < min_obs, NA_real_, value_window)) %>%
    dplyr::select(dplyr::all_of(health_id_vars), climate_var, value_window) %>%
    tidyr::pivot_wider(names_from = climate_var, values_from = value_window, names_prefix = paste0("mvwin", window_days, "_"))

  return(out)
}

#' Offset Window: Aggregates a specific historical gap [t - W2, t - W1]
#' @keywords internal
#' @noRd
.join_offset_window <- function(health_data, climate_data, target_vars, offset_days, min_obs) {
  # offset_days is guaranteed sorted c(W1, W2) by validation. W1 < W2
  w1 <- offset_days[1] # closest to event (e.g., 7)
  w2 <- offset_days[2] # furthest from event (e.g., 14)
  
  window_size <- (w2 - w1) + 1
  agg_rule <- .build_agg_rules(target_vars)
  health_id_vars <- setdiff(names(health_data), "geom")

  health_win <- health_data %>%
    dplyr::mutate(
      .win_start = .data$date - w2, # furthest back
      .win_end   = .data$date - w1  # closest back
    )
    
  climate_sel <- climate_data %>% dplyr::select(date, code_muni, dplyr::all_of(target_vars)) %>% dplyr::rename(date_climate = date)

  joined <- health_win %>%
    dplyr::left_join(climate_sel, by = "code_muni", relationship = "many-to-many") %>%
    dplyr::filter(.data$date_climate >= .win_start & .data$date_climate <= .win_end)

  out <- joined %>%
    tidyr::pivot_longer(cols = dplyr::all_of(target_vars), names_to = "climate_var", values_to = "value") %>%
    dplyr::left_join(agg_rule, by = "climate_var") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(health_id_vars)), climate_var) %>%
    dplyr::summarise(
      n_obs = dplyr::n(),
      prop_obs = n_obs / window_size,
      value_window = dplyr::case_when(
        dplyr::first(agg_type) == "sum" ~ sum(value, na.rm = TRUE),
        dplyr::first(agg_type) == "mean" ~ mean(value, na.rm = TRUE),
        dplyr::first(agg_type) == "mean_circular" ~ .circular_mean(value),
        TRUE ~ mean(value, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(value_window = dplyr::if_else(prop_obs < min_obs, NA_real_, value_window)) %>%
    dplyr::select(dplyr::all_of(health_id_vars), climate_var, value_window) %>%
    # Creates column name like: off7to14_rainfall_mm
    tidyr::pivot_wider(names_from = climate_var, values_from = value_window, names_prefix = paste0("off", w1, "to", w2, "_"))

  return(out)
}

#' Distributed Lag: Matrix 0 to L via forward-shift trick
#' @keywords internal
#' @noRd
.join_distributed_lag <- function(health_data, climate_data, target_vars, max_lag) {
  health_id_vars <- setdiff(names(health_data), "geom")
  result <- health_data
  lags_seq <- seq(0, max_lag)

  for (l in lags_seq) {
    climate_shifted <- climate_data %>%
      dplyr::mutate(date = .data$date + lubridate::days(l)) %>%
      dplyr::select(.data$date, .data$code_muni, dplyr::all_of(target_vars))

    joined <- health_data %>% dplyr::left_join(climate_shifted, by = c("date", "code_muni"))
    rename_cols <- stats::setNames(target_vars, paste0(target_vars, "_lag", l))
    lagged_cols <- dplyr::rename(joined %>% dplyr::select(dplyr::all_of(target_vars)), !!!rename_cols)
    result <- dplyr::bind_cols(result, lagged_cols)
  }
  return(result)
}

#' Degree Days: Growing Degree Days (GDD)
#' @keywords internal
#' @noRd
.join_degree_days <- function(health_data, climate_data, window_days, temp_var, t_base, min_obs) {
  window_size <- window_days + 1
  health_id_vars <- setdiff(names(health_data), "geom")

  if (!temp_var %in% names(climate_data)) cli::cli_abort("Temperature variable '{temp_var}' not found for degree days calculation.")

  health_win <- health_data %>% dplyr::mutate(.win_start = .data$date - window_days)
  climate_sel <- climate_data %>% dplyr::select(date, code_muni, !!temp_var) %>% dplyr::rename(date_climate = date, temp = !!temp_var)

  joined <- health_win %>%
    dplyr::left_join(climate_sel, by = "code_muni", relationship = "many-to-many") %>%
    dplyr::filter(.data$date_climate >= .win_start & .data$date_climate <= .data$date)

  out <- joined %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(health_id_vars))) %>%
    dplyr::summarise(
      n_obs = sum(!is.na(temp)),
      prop_obs = n_obs / window_size,
      gdd = sum(pmax(0, .data$temp - t_base), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(gdd = dplyr::if_else(prop_obs < min_obs, NA_real_, gdd)) %>%
    dplyr::select(dplyr::all_of(health_id_vars), gdd) %>%
    dplyr::rename(!!paste0("gdd", window_days, "_tbase", t_base) := gdd)

  return(out)
}

#' Seasonal matching
#' @keywords internal
#' @noRd
.join_seasonal <- function(health_data, climate_data, target_vars) {
  get_season <- function(date) {
    month <- lubridate::month(date)
    dplyr::case_when(month %in% c(12, 1, 2) ~ "DJF", month %in% c(3, 4, 5) ~ "MAM", month %in% c(6, 7, 8) ~ "JJA", TRUE ~ "SON")
  }

  agg_rule <- .build_agg_rules(target_vars)

  climate_seasonal <- climate_data %>%
    dplyr::mutate(year = lubridate::year(date), season = get_season(date)) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(target_vars), names_to = "climate_var", values_to = "value") %>%
    dplyr::left_join(agg_rule, by = "climate_var") %>%
    dplyr::group_by(code_muni, station_name, year, season, climate_var, agg_type) %>%
    dplyr::summarise(
      n_valid = sum(!is.na(value)),
      value = dplyr::case_when(
        dplyr::first(agg_type) == "sum" ~ sum(value, na.rm = TRUE),
        dplyr::first(agg_type) == "mean" ~ mean(value, na.rm = TRUE),
        dplyr::first(agg_type) == "mean_circular" ~ .circular_mean(value),
        TRUE ~ mean(value, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    dplyr::filter(n_valid >= 60) %>%
    dplyr::select(-n_valid, -agg_type) %>%
    tidyr::pivot_wider(names_from = climate_var, values_from = value, names_prefix = "season_")

  health_data %>%
    dplyr::mutate(year = lubridate::year(date), season = get_season(date)) %>%
    dplyr::left_join(climate_seasonal, by = c("code_muni", "station_name", "year", "season")) %>%
    dplyr::select(-year, -season)
}


# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' @keywords internal
#' @noRd
.build_agg_rules <- function(vars) {
  dplyr::tibble(
    climate_var = vars,
    agg_type = dplyr::case_when(
      climate_var %in% c("rainfall_mm", "sr_kj_m2") ~ "sum",
      climate_var == "wd_degrees" ~ "mean_circular",
      TRUE ~ "mean"
    )
  )
}

#' @keywords internal
#' @noRd
.circular_mean <- function(x, na_rm = TRUE) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  rad <- x * pi / 180
  atan2(mean(sin(rad)), mean(cos(rad))) * 180 / pi
}

#' @keywords internal
#' @noRd
.set_climate_agg_meta <- function(df, system) {
  if (!inherits(df, "climasus_df")) {
    meta <- list(system = system, stage = "climate", type = "agg", spatial = inherits(df, "sf"), temporal = NULL, created = Sys.time(), modified = Sys.time(), history = sprintf("[%s] Climate aggregation", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), user = list())
    base_classes <- setdiff(class(df), "climasus_df")
    structure(df, climasus_meta = meta, class = c("climasus_df", base_classes))
  } else {
    climasus_meta(df, stage = "climate", type = "agg")
  }
}

#' @keywords internal
#' @noRd
.get_messages <- function(lang = "pt") {
  messages <- list(
    pt = list(aggregating = "Agregando dados temporais...", spatial_match = "Realizando matching espacial...", temporal_join = "Integrando dados temporais..."),
    en = list(aggregating = "Aggregating temporal data...", spatial_match = "Performing spatial matching...", temporal_join = "Integrating temporal data..."),
    es = list(aggregating = "Agregando datos temporales...", spatial_match = "Realizando coincidencia espacial...", temporal_join = "Integrando datos temporales...")
  )
  messages[[lang]] %||% messages[["en"]]
}