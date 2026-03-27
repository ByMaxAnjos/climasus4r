#' Integration of climate and health data
#'
#' @description
#' `sus_climate_aggregate()` aggregates meteorological data to datasus for environmental and epidemiological analysis.
#' 
#' The function implements:
#' - **Flexible temporal aggregation** (hour → season, including multi-period windows),
#' - **Optional spatial matching** to municipalities or other sf objects,
#' - **Parallel, cache-aware processing** for large multi-year datasets.
#'
#' This function is designed for:
#' - Climate–health studies (e.g., dengue, malaria, heat stress),
#' - Environmental exposure assessment,
#' - Time-series gap filling and preprocessing,
#' - Spatiotemporal integration of climate and health data.
#'
#' @param climate_data An `sf` object (e.g., output from `sus_climate_inmet()` with `sus_climate_fill_gaps()` for
#'   spatial matching. 
#' 
#' @param climate_var Character string specifying the `climate data`column to aggregate.
#'   Example: `climate_var = "tair_dry_bulb_c"`. Default is NULL. 
#'
#' @param time_unit Character specifying temporal aggregation.
#'   Options include:
#'   `"day"` (default), `"week"`, `"month"`, `"quarter"`,
#'   `"year"`, `"season"` (DJF, MAM, JJA, SON), or multi-period formats such as
#'   `"2 days"`, `"5 days"`, `"14 days"`, `"3 months"`, `"6 months"`. 
#' 
#' @param temporal_strategy Character. Strategy for temporal matching. Must be one of:
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
#' @param use_cache Logical. If TRUE, uses Arrow/Parquet cache to speed up repeated
#'   processing.
#'
#' @param cache_dir Character. Directory for cached files.
#'   Default: `"~/.climasus4r_cache/climate"`.
#'
#' @param lang Character. Message language: `"pt"`, `"en"`, or `"es"`.
#'
#' @param verbose Logical. If TRUE, prints progress messages.
#' 
#' ## **Spatial Matching** 
#' The function:
#' 1. Computes distances between municipality centroids and stations,
#' 2. Assigns the nearest valid station,
#' 3. Falls back to nearest station with a warning if no station meets quality criteria.
#'
#' ## **Temporal Aggregation**
#' Aggregation supports:
#' - Consistent alignment with epidemiological weeks when relevant.
#'
#' @return
#' A tibble with processed meteorological data containing:
#' 
sus_climate_aggregate <- function(
    health_data, 
    climate_data,
    climate_var = "all",
    time_unit = "day",
    temporal_strategy = "exact",  
    window_days = NULL,            
    lag_days = NULL,
    use_cache = TRUE,
    cache_dir = "~/.climasus4r_cache/climate",
    lang = "pt",
    verbose = TRUE             
) {
  
  # ============================================================================
  # VALIDATE HEATLH DATA
  # ============================================================================
  # Check if data is climasus_df
  if (inherits(health_data, "climasus_df")) {
    
    # Required stages for aggregation
    required_stages <- c("spatial")
    allowed_stages  <- c("spatial")
    
    current_stage <- climasus_meta(health_data, "stage")
    
    # Must have passed through at least one filter stage
    if (is.null(current_stage) || !current_stage %in% allowed_stages) {
      
      msg_error <- list(
        en = paste0(
          "Data must be filtered before aggregation.\n",
          "Current stage: ", current_stage %||% "unknown", "\n",
          "Required stage: filter_cid or filter_demo\n\n",
          "Please run:\n",
          "  health_data <- sus_join_spatial(...)"
        ),
        pt = paste0(
          "Dados devem ser filtrados antes da agregacao.\n",
          "Estagio atual: ", current_stage %||% "desconhecido", "\n",
          "Estagio requerido: filter_cid ou filter_demo\n\n",
          "Por favor, execute:\n",
          "  health_data <- sus_join_spatial(...)"
        ),
        es = paste0(
          "Los datos deben ser filtrados antes de la agregacion.\n",
          "Etapa actual: ", current_stage %||% "desconocida", "\n",
          "Etapa requerida: filter_cid o filter_demo\n\n",
          "Por favor, ejecute:\n",
          "  health_data <- sus_join_spatial(...)"
        )
      )
      
      cli::cli_abort(msg_error[[lang]] %||% msg_error[["en"]])
    }
    
    # Stage validated
    if (verbose) {
      msg_stage_ok <- list(
        en = "Data stage validated: aggregation allowed",
        pt = "Estagio de dados validado: agregacao permitida",
        es = "Etapa de datos validada: agregacion permitida"
      )
      
      cli::cli_alert_success(msg_stage_ok[[lang]] %||% msg_stage_ok[["en"]])
    }
    
    # Update metadata
    health_data <- climasus_meta(health_data, stage = "climate", type  = "agg")
    
  } else {
    
    # NOT climasus_df - ABORT execution
    msg_error <- list(
      en = paste0(
        "Input is not a climasus_df object.\n",
        "This function requires data from the CLIMASUS4r pipeline.\n\n",
        "Please prepare your data first:\n",
        "  1. Import: health_data <- sus_data_import(...) or sus_data_read(...)\n",
        "  2. Clean: health_data <- sus_data_clean_encoding(...)\n",
        "  3. Standardize: health_data <- sus_data_standardize(...)\n",
        "  4. Filter cid: health_data <- sus_data_filter_cid(...)\n",
        "  5. Filter demo: health_data <- sus_data_filter_demographics(...)\n",
        "  6. Aggregate: health_data <- sus_data_aggregate(...)\n",
        "  7. Spatial: health_data <- sus_join_spatial(...)"
      ),
      pt = paste0(
        "Entrada nao e um objeto climasus_df.\n",
        "Esta funcao requer dados do pipeline CLIMASUS4r.\n\n",
        "Por favor, prepare seus dados primeiro:\n",
        "  1. Import: health_data <- sus_data_import(...) or sus_data_read(...)\n",
        "  2. Clean: health_data <- sus_data_clean_encoding(...)\n",
        "  3. Standardize: health_data <- sus_data_standardize(...)\n",
        "  4. Filter cid: health_data <- sus_data_filter_cid(...)\n",
        "  5. Filter demo: health_data <- sus_data_filter_demographics(...)\n",
        "  6. Aggregate: health_data <- sus_data_aggregate(...)\n",
        "  7. Espacial: health_data <- sus_join_spatial(...)"
      ),
      es = paste0(
        "La entrada no es un objeto climasus_df.\n",
        "Esta funcion requiere datos del pipeline CLIMASUS4r.\n\n",
        "Por favor, prepare sus datos primero:\n",
        "  1. Import: health_data <- sus_data_import(...) or sus_data_read(...)\n",
        "  2. Clean: health_data <- sus_data_clean_encoding(...)\n",
        "  3. Standardize: health_data <- sus_data_standardize(...)\n",
        "  4. Filter cid: health_data <- sus_data_filter_cid(...)\n",
        "  5. Filter demo: health_data <- sus_data_filter_demographics(...)\n",
        "  6. Aggregate: health_data <- sus_data_aggregate(...)\n",
        "  7. Espacial: health_data <- sus_join_spatial(...)"
      )
    )
    
    cli::cli_abort(msg_error[[lang]])
  }
  system <- climasus_meta(health_data, "system")
  
  # ============================================================================
  # VALIDATE CLIMATE DATA
  # ============================================================================
  if (inherits(climate_data, "climasus_df")) {
    
    # Required stages for aggregation
    required_stages <- c("climate")
    allowed_stages  <- c("climate")
    
    current_stage <- climasus_meta(climate_data, "stage")
    
    # Must have passed through at least one filter stage
    if (is.null(current_stage) || !current_stage %in% allowed_stages) {
      
      msg_error <- list(
        en = paste0(
          "Data must be filtered before aggregation.\n",
          "Current stage: ", current_stage %||% "unknown", "\n",
          "Required stage: filter_cid or filter_demo\n\n",
          "Please run at least:\n",
          "  climate_data <- sus_climate_inmet(...)\n",
          "  climate_data <- sus_climate_fill_gaps(...)"
        ),
        pt = paste0(
          "Dados devem ser filtrados antes da agregacao.\n",
          "Estagio atual: ", current_stage %||% "desconhecido", "\n",
          "Estagio requerido: filter_cid ou filter_demo\n\n",
          "Por favor, execute ao menos:\n",
          "  climate_data <- sus_climate_inmet(...)\n",
          "  climate_data <- sus_climate_fill_gaps(...)"
        ),
        es = paste0(
          "Los datos deben ser filtrados antes de la agregacion.\n",
          "Etapa actual: ", current_stage %||% "desconocida", "\n",
          "Etapa requerida: filter_cid o filter_demo\n\n",
          "Por favor, ejecute al menos:\n",
          "  climate_data <- sus_climate_inmet(...)\n",
          "  climate_data <- sus_climate_fill_gaps(...)"
        )
      )
      
      cli::cli_abort(msg_error[[lang]] %||% msg_error[["en"]])
    }
    
    # Stage validated
    if (verbose) {
      msg_stage_ok <- list(
        en = "Data stage validated: aggregation allowed",
        pt = "Estagio de dados validado: agregacao permitida",
        es = "Etapa de datos validada: agregacion permitida"
      )
      
      cli::cli_alert_success(msg_stage_ok[[lang]] %||% msg_stage_ok[["en"]])
    }
    
    # Update metadata
    climate_data <- climasus_meta(climate_data, stage = "climate", type = "agg")
    
  } else {
    
    # NOT climasus_df - ABORT execution
    msg_error <- list(
      en = paste0(
        "Input is not a climasus_df object.\n",
        "This function requires data from the CLIMASUS4r pipeline.\n\n",
        "Please run at least:\n",
        "  climate_data <- sus_climate_inmet(...)\n",
        "  climate_data <- sus_climate_fill_gaps(...)"
      ),
      pt = paste0(
        "Entrada nao e um objeto climasus_df.\n",
        "Esta funcao requer dados do pipeline CLIMASUS4r.\n\n",
        "Por favor, execute ao menos:\n",
        "  climate_data <- sus_climate_inmet(...)\n",
        "  climate_data <- sus_climate_fill_gaps(...)"
      ),
      es = paste0(
        "La entrada no es un objeto climasus_df.\n",
        "Esta funcion requiere datos del pipeline CLIMASUS4r.\n\n",
        "Por favor, ejecute al menos:\n",
        "  climate_data <- sus_climate_inmet(...)\n",
        "  climate_data <- sus_climate_fill_gaps(...)"
      )
    )
    
    cli::cli_abort(msg_error[[lang]])
  }
  
  # ============================================================================
  # VALIDATE CLIMATE SOURCE AND EVALUALTION
  # ============================================================================
  
  source <- climasus_meta(climate_data, "temporal")$source
  
  if(!source %in% "INMET") {
    msg_error <- list(
      en = paste0(
        "This function requires meteorological data from the INMET.\n\n",
        "Please run at least:\n",
        "  climate_data <- sus_climate_inmet(...)\n",
        "  climate_data <- sus_climate_fill_gaps(...)"
      ),
      pt = paste0(
        "Esta funcao requer dados meteorologicos do INMET.\n\n",
        "Por favor, execute ao menos:\n",
        "  climate_data <- sus_climate_inmet(...)\n",
        "  climate_data <- sus_climate_fill_gaps(...)"
      ),
      es = paste0(
        "Esta funcion requiere datos meteorologicos del INMET.\n\n",
        "Por favor, ejecute al menos:\n",
        "  climate_data <- sus_climate_inmet(...)\n",
        "  climate_data <- sus_climate_fill_gaps(...)"
      )
    )
    
    cli::cli_abort(msg_error[[lang]])
  }
  evaluation <- climasus_meta(climate_data, "temporal")$evaluation
  
  if(!evaluation) {
    msg_error <- list(
      en = paste0(
        "This function requires filled data without evaluation.\n\n",
        "Please run:\n",
        "  climate_data <- sus_climate_fill_gaps(..., evaluation = FALSE)"
      ),
      pt = paste0(
        "Esta funcao requer dados preenchidos sem evaluacao.\n\n",
        "Por favor, execute:\n",
        "  climate_data <- sus_climate_fill_gaps(..., evaluation = FALSE)"
      ),
      es = paste0(
        "Esta funcion requiere datos preenchidos no incluso evaluacion.\n\n",
        "Por favor, ejecute al menos:\n",
        "  climate_data <- sus_climate_fill_gaps(...)"
      )
    )
    
    cli::cli_abort(msg_error[[lang]])
  }
  
  
  # ============================================================================
  # VALIDATE RANGE DATES 
  # ============================================================================
  
  date_start <- climasus_meta(climate_data, "temporal")$start
  date_end <- climasus_meta(climate_data, "temporal")$end
  
  climasus_meta(health_data, "temporal")
  if(!source %in% "INMET") {
    msg_error <- list(
      en = paste0(
        "This function requires meteorological data from the INMET.\n\n",
        "Please run at least:\n",
        "  climate_data <- sus_climate_inmet(...)\n",
        "  climate_data <- sus_climate_fill_gaps(...)"
      ),
      pt = paste0(
        "Esta funcao requer dados meteorologicos do INMET.\n\n",
        "Por favor, execute ao menos:\n",
        "  climate_data <- sus_climate_inmet(...)\n",
        "  climate_data <- sus_climate_fill_gaps(...)"
      ),
      es = paste0(
        "Esta funcion requiere datos meteorologicos del INMET.\n\n",
        "Por favor, ejecute al menos:\n",
        "  climate_data <- sus_climate_inmet(...)\n",
        "  climate_data <- sus_climate_fill_gaps(...)"
      )
    )
    
    cli::cli_abort(msg_error[[lang]])
  }
  
  # ============================================================================
  # VALIDATE CLIMATE TARGET VARIABLE
  # ============================================================================
  if(!is.null(climate_var) && climate_var != "all") { 
    missing_targets <- setdiff(climate_var, colnames(climate_data))
    if (length(missing_targets) > 0) {
      cli::cli_warming(sprintf("Target variables not found in data: %s", paste(missing_targets, collapse = ", ")))
      climate_var <- "all"
    }
  }
  # ============================================================================
  # VALIDATE TEMPORAL STRATEGY WITH WINDOWS AND LAG
  # ============================================================================
  temporal_strategy <- match.arg(temporal_strategy,
                                 choices = c("exact", "window", "lag", "seasonal"))
  
  if (temporal_strategy == "window") {
    
    if (is.null(window_days)) {cli::cli_abort("{.arg window_days} must be provided when {.arg temporal_strategy = 'window'}.")}
    
    if (!is.numeric(window_days) || anyNA(window_days)) {cli::cli_abort("{.arg window_days} must be numeric.")}
    
    window_days <- as.integer(window_days)
    
    if (any(window_days < 0)) {cli::cli_abort("{.arg window_days} must be non-negative integers.")}
    
    if (length(window_days) > 1) {cli::cli_alert_info("Using the maximum value of {.arg window_days}: {max(window_days)}")
      window_days <- max(window_days)
    }
    
  } else {
    window_days <- NULL
  }
  
  if (temporal_strategy == "lag") {
    if (is.null(lag_days)) {cli::cli_abort("{.arg lag_days} must be provided when {.arg temporal_strategy = 'lag'}.")}
    
    if (!is.numeric(lag_days) || anyNA(lag_days)) {cli::cli_abort("{.arg lag_days} must be numeric.")}
    
    lag_days <- as.integer(lag_days)
    
    if (any(lag_days < 0)) {cli::cli_abort("{.arg lag_days} must be non-negative integers.")}
    
    lag_days <- sort(unique(lag_days))
    
  } else {
    lag_days <- NULL
  }
  # ============================================================================
  # TEMPORAL CLIMATE AGGREGATION
  # ============================================================================
  if (verbose) {cli::cli_progress_step(msg$aggregating)}
  
  if (is.null(time_unit) || identical(time_unit, "hour")) { 
    climate_data_agg <- .aggregate_meteo_data(climate_data, time_unit = "day")
  }
  climate_data_agg <- .aggregate_meteo_data(climate_data, time_unit = time_unit)
  
  if (verbose) cli::cli_progress_done()
  
  # ============================================================================
  # SPATIAL MATCHING 
  # ============================================================================
  if (verbose) {cli::cli_progress_step(msg$spatial_match)}
  
  climate_data_mat <- .match_spatial(
    df = climate_data_agg,
    spatial_obj = health_data,
    verbose = verbose)
  
  df <- switch(
    temporal_strategy,
    "exact" = .join_exact(
      health_data = spatial_obj,
      climate_data = climate_data_mat,
      target_vars = climate_var
    ),
    "window" = .join_window(
      health_data = spatial_obj,
      climate_data = climate_data_mat,
      target_vars = climate_var,
      window_days = window_days
    ),
    "lag" = .join_lag(
      health_data = spatial_obj,
      climate_data = climate_data_mat,
      target_vars = climate_var,
      lag_days = lag_days
    ),
    "seasonal" = .join_seasonal(
      health_data = spatial_obj,
      climate_data = climate_data_mat,
      target_vars = climate_var
    )
  )
  
  # Update stage and type
  if (!inherits(df, "climasus_df")) {
    # Create new climasus_df
    meta <- list(
      system = system,
      stage = "climate",
      type = "agg",
      spatial = inherits(df, "sf"),
      temporal = NULL,
      created = Sys.time(),
      modified = Sys.time(),
      history = sprintf(
        "[%s] Climate aggregation",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ),
      user = list()
    )
    
    base_classes <- setdiff(class(df), "climasus_df")
    df <- structure(
      df,
      climasus_meta = meta,
      class = c("climasus_df", base_classes)
    )
  } else { 
    df <- climasus_meta(
      df,
      system = climasus_meta(df, "system"), 
      stage = "climate",
      type = "agg"
    )     
  }
  if (verbose) cli::cli_progress_done()
  
  return(df)
  
}

# ============================================================================
# Helper functions
# ============================================================================











.get_messages <- function(lang = "pt") {
  messages <- list(
    pt = list(
      setup = "Configurando ambiente...",
      aggregating = "Agregando dados temporais...",
      calc_lags = "Calculando defasagens temporais...",
      spatial_match = "Realizando matching espacial...",
      no_data = "Nenhum dado foi encontrado para os criterios especificados.",
      quality_warning = "Estacao {code} com {pct}% de dados faltantes. Usando mesmo assim.",
      lag_info = "Criando lags para: {paste(lags_weeks, collapse = ', ')} semanas"
    ),
    en = list(
      setup = "Setting up environment...",
      aggregating = "Aggregating temporal data...",
      calc_lags = "Calculating temporal lags...",
      spatial_match = "Performing spatial matching...",
      no_data = "No data found for the specified criteria.",
      quality_warning = "Station {code} with {pct}% missing data. Using anyway.",
      lag_info = "Creating lags for: {paste(lags_weeks, collapse = ', ')} weeks"
    ),
    es = list(
      setup = "Configurando el entorno...",
      aggregating = "Agregando datos temporales...",
      calc_lags = "Calculando rezagos temporales...",
      spatial_match = "Realizando coincidencia espacial...",
      no_data = "No se encontraron datos para los criterios especificados.",
      quality_warning = "Estacion {code} con {pct}% de datos faltantes. Usando de todos modos.",
      lag_info = "Creando rezagos para: {paste(lags_weeks, collapse = ', ')} semanas"
    )
  )
  
  return(messages[[lang]])
}

