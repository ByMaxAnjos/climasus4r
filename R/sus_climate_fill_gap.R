#' Fill gaps in climate and air-quality time series using XGBoost
#'
#' @title Gap-filling of climate data with station-wise machine learning
#'
#' @description
#' This function performs data-driven gap-filling (statistical imputation) in
#' climate and air-quality time series using gradient boosting (XGBoost).
#' 
#' The method:
#' \itemize{
#'   \item Trains **one separate model per station** and per target variable;
#'   \item Uses automatic temporal feature engineering (lags and rolling windows);
#'   \item Applies station-level quality filtering before modeling;
#'   \item Supports multiple data sources (e.g., INMET, NOAA, AirQuality);
#'   \item Can run in parallel across stations;
#'   \item Returns both filled data and processing metadata.
#' }
#'
#' **Important methodological notes:**
#' \itemize{
#'   \item This is a **gap-filling (imputation) method, NOT a forecasting model**.
#'         Predictions are only made for timestamps where data are missing.
#'   \item Models are trained **only on observed (non-missing) data** within each station.
#'   \item No information from the future is used to impute past values
#'         (only lagged and rolling features are used).
#'   \item If a station exceeds the allowed missingness threshold for a variable,
#'         that variable is not imputed for that station.
#' }
#'
#' @param df 
#'   A data.frame (or tibble) containing climate or air-quality data.
#'   Must include:
#'   \itemize{
#'     \item a datetime column (POSIXct, POSIXlt, or convertible to datetime),
#'     \item a station identifier column,
#'     \item at least one numeric target variable to be filled.
#'   }
#'
#' @param target_var 
#'   Character vector. One or more variable names to be gap-filled.
#'   Each variable is modeled and imputed **independently**.
#'   Example: `c("tair_dry_bulb_c", "rh_mean_porc", "ws_2_m_s")`.
#'
#' @param datetime_col 
#'   Character. Name of the datetime column in `df`.  
#'   If `NULL` (default), the function attempts automatic detection.
#'
#' @param station_col 
#'   Character. Name of the station identifier column.  
#'   If `NULL`, it is auto-detected based on the declared `source`.
#'
#' @param source 
#'   Character. Data source identifier:
#'   \itemize{
#'     \item `"INMET"` (Brazilian weather stations),
#'     \item `"NOAA"` (global weather stations),
#'     \item `"AirQuality"` (air pollution data),
#'     \item `"auto"` (default): automatically detected from column patterns.
#'   }
#'
#' @param quality_threshold 
#'   Numeric. Maximum allowed proportion of missing values **per station
#'   and per variable**.  
#'   Stations exceeding this threshold are excluded for that variable.
#'   Default: `0.4` (40% missing).
#'
#' @param lag_periods 
#'   Numeric vector. Time lags (in hours) used to construct predictors.
#'   Default: `c(3, 6, 12, 24)`.
#'
#' @param rolling_windows 
#'   Numeric vector. Window sizes (in hours) for rolling statistics
#'   (mean and standard deviation).  
#'   Default: `c(3, 6, 12)`.
#'
#' @param run_evaluation 
#'   Logical. If `TRUE`, performs an internal missing-data simulation to assess
#'   imputation performance (RMSE / MAE). Default: `FALSE`.
#'
#' @param gap_percentage 
#'   Numeric. If `run_evaluation = TRUE`, proportion of observed data
#'   artificially removed to test performance. Default: `0.2` (20%).
#'
#' @param gap_mechanism 
#'   Character. Missing-data mechanism used in evaluation when
#'   `run_evaluation = TRUE`:
#'   \itemize{
#'     \item `"MCAR"`: Missing Completely At Random (default),
#'     \item `"MAR"`: Missing At Random,
#'     \item `"MNAR"`: Missing Not At Random.
#'   }
#'
#' @param keep_features 
#'   Logical. If `TRUE`, retains engineered features (lags, rolling statistics,
#'   and temporal variables).  
#'   If `FALSE` (default), only original columns + `is_imputed_*` are returned.
#'
#' @param model_params 
#'   List of additional XGBoost parameters to override defaults.
#'   Example:
#'   \code{
#'   list(max_depth = 4, eta = 0.05, nrounds = 200)
#'   }
#'
#' @param n_workers 
#'   Integer. Number of parallel workers.  
#'   If `NULL` (default), uses `availableCores() - 1`.
#'
#' @param verbose 
#'   Logical. If `TRUE`, prints progress messages and warnings.
#'
#' @param lang 
#'   Character. Language for messages:
#'   `"pt"`, `"en"`, or `"es"`. Default: `"pt"`.
#'
#' @return 
#' An object of class `"climasus_df"` with updated metadata, containing:
#' \itemize{
#'   \item **data**: original data with filled values,
#'   \item **is_imputed_<var>**: logical flag per variable indicating which values were imputed,
#'   \item **climasus_meta**: metadata including:
#'     \itemize{
#'       \item data source,
#'       \item number of imputed values,
#'       \item imputation rate,
#'       \item processing time,
#'       \item quality threshold used.
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage (single variable)
#' filled_data <- sus_climate_fill_gaps(
#'   df = inmet_climate,
#'   target_var = "tair_dry_bulb_c"
#' )
#'
#' # Multiple variables in one call
#' filled_data <- sus_climate_fill_gaps(
#'   df = climate_data,
#'   target_var = c("tair_dry_bulb_c", "rh_mean_porc", "ws_2_m_s"),
#'   quality_threshold = 0.4
#' )
#'
#' # With performance evaluation
#' filled_data <- sus_climate_fill_gaps(
#'   df = climate_data,
#'   target_var = "tair_dry_bulb_c",
#'   run_evaluation = TRUE,
#'   gap_percentage = 0.2
#' )
#' }
#'
#' @export
#' @importFrom rlang .data
#' @importFrom data.table :=
sus_climate_fill_gaps <- function(
  df,
  target_var,
  datetime_col = NULL,
  station_col = NULL,
  source = "auto",
  quality_threshold = 0.4,
  lag_periods = c(3, 6, 12, 24),
  rolling_windows = c(3, 6, 12),
  run_evaluation = FALSE,
  gap_percentage = 0.2,
  gap_mechanism = "MCAR", 
  keep_features = FALSE,
  model_params = list(),
  n_workers = NULL,
  verbose = TRUE,
  lang = "pt"
) {
  
  # Check pak
  rlang::check_installed(c("furrr", "xgboost", "zoo"), 
                       reason = "to run sus_climate_fill_gaps()")
  
  # Set seed for reproducibility
  set.seed(42)

  # ---- Initialize Messages ----
  messages <- .get_fill_gaps_messages(lang)

  # ---- Detect Data Source ----
  if (source == "auto") {
    source <- .detect_data_source(df)
    if (verbose) cli::cli_alert_info(messages$source_detected(source))
  }

  # ---- Auto-detect Columns ----
  datetime_col <- .detect_datetime_column(df, datetime_col, verbose, messages)
  station_col <- .detect_station_column(
    df,
    station_col,
    source,
    verbose,
    messages
  )
 
  # ---- Validate all targets exist ----
  missing_targets <- setdiff(target_var, colnames(df))
  if (length(missing_targets) > 0) {
    cli::cli_abort(
      sprintf(
        "Target variables not found in data: %s",
        paste(missing_targets, collapse = ", ")
      )
    )
  }

  if (verbose) {
    cli::cli_alert_info(messages$starting_process)
  }

    # ---- Rename datetime column for consistency ----
  if (datetime_col != "date") {
    df <- df %>%
      dplyr::rename(date = !!rlang::sym(datetime_col))
  }

  # ===========================================================
  # QUALITY FILTER: NOW BY STATION VARIABLE
  # ===========================================================
  if (verbose) {
    cli::cli_alert_info(messages$quality_filter)
  }

  station_stats <- df %>%
    dplyr::group_by(!!rlang::sym(station_col)) %>%
    dplyr::summarise(
      total_rows = dplyr::n(),
      missing_rows = sum(is.na(!!rlang::sym(target_var))),
      missing_pct = .data$missing_rows / .data$total_rows,
      .groups = "drop"
    )
  
  bad_stations <- station_stats %>%
    dplyr::filter(.data$missing_pct > quality_threshold) %>%
    dplyr::pull(!!rlang::sym(station_col))

  if (length(bad_stations) > 0 && verbose) {
    cli::cli_alert_warning(
      sprintf(
        messages$skipping_stations,
        length(bad_stations),
        round(quality_threshold * 100),
        paste(bad_stations, collapse = ", ")
      )
    )
  }

  valid_stations <- station_stats %>%
    dplyr::filter(.data$missing_pct <= quality_threshold) %>%
    dplyr::pull(!!rlang::sym(station_col))


  if (length(valid_stations) == 0) {
    cli::cli_abort(
      sprintf(
        "No stations passed the quality threshold of %.0f%%. Increase quality_threshold.",
        quality_threshold * 100
      )
    )
  }
  
 if (verbose) {
    skipped <- setdiff(unique(df[[station_col]]), valid_stations)
    if (length(skipped) > 0) {
      cli::cli_alert_warning(
        sprintf(
          "Skipping %d stations for ALL variables: %s",
          length(skipped),
          paste(utils::head(skipped, 10), collapse = ", ")
        )
      )
    }
  }

  df_filtered <- df %>%
    dplyr::filter(!!rlang::sym(station_col) %in% valid_stations)

  # ===========================================================
  # PARALLEL SETUP
  # ===========================================================
  if (is.null(n_workers)) {
    n_workers <- max(1, future::availableCores() - 1)
  }

  if (verbose) {
    cli::cli_alert_info(sprintf(messages$parallel_setup, n_workers))
  }

  old_plan <- future::plan()
  future::plan(future::multisession, workers = n_workers)
  on.exit(future::plan(old_plan), add = TRUE)

  # ===========================================================
  # PROCESS STATIONS IN PARALLEL
  # ===========================================================
  if (verbose) {cli::cli_alert_info(messages$processing_stations)}

  start_time <- Sys.time()

  # Process stations in parallel
  stations_list <- df_filtered %>%
    dplyr::group_split(!!rlang::sym(station_col), .keep = TRUE)

  results_list <- furrr::future_map(
    stations_list,
    function(station_df) {
      tryCatch(
        .process_single_station_xgboost(
          station_df = station_df,
          target_var = target_var,
          station_col = station_col,
          lag_periods = lag_periods,
          rolling_windows = rolling_windows,
          verbose = FALSE
        ),
        error = function(e) {
          cli::cli_alert_warning(
            sprintf(
              "Station %s failed: %s",
              unique(station_df[[station_col]]),
              e$message
            )
          )
          return(station_df) 
        }
      )
    },
    .options = furrr::furrr_options(seed = TRUE),
    .progress = verbose
  )

  df_filled <- dplyr::bind_rows(results_list)
  
  # Restore original datetime column name if needed
  if (datetime_col != "date") {
    df_filled <- df_filled %>%
      dplyr::rename(!!rlang::sym(datetime_col) := date)
  }

  # ===========================================================
  # EVALUATION (MUST HAPPEN BEFORE IMPUTATION!)
  # ===========================================================
    if (run_evaluation) {
      if (verbose) {
        cli::cli_alert_info("Running evaluation with artificial gaps...")
      }
      
      # Run evaluation on a sample of stations to save time
      eval_stations <- sample(valid_stations, min(3, length(valid_stations)))
      df_eval <- df_filtered %>%
        dplyr::filter(!!rlang::sym(station_col) %in% eval_stations)
      
      eval_metrics <- .evaluate_single_station(
        station_df = df_eval,
        target_var = target_var,
        station_col = station_col,
        gap_percentage = gap_percentage,
        gap_mechanism = gap_mechanism,
        lag_periods = lag_periods,
        rolling_windows = rolling_windows,
        model_params = model_params,
        verbose = verbose
      )

      if (verbose) {
        cli::cli_alert_success(sprintf(
          "Evaluation completed: RMSE = %.3f, MAE = %.3f, R2 = %.3f",
          eval_metrics$rmse,
          eval_metrics$mae,
          eval_metrics$r_squared
        ))
        print(eval_metrics)
      }
    }
  
  # ===========================================================
  # STATISTICS PER VARIABLE
  # ===========================================================
  n_imputed <- sum(df_filled$is_imputed, na.rm = TRUE)
  n_total <- nrow(df_filled)
  imputation_rate <- n_imputed / n_total

  end_time <- Sys.time()
  processing_time <- difftime(end_time, start_time, units = "secs")

  if (verbose) {
    cli::cli_alert_success(
      sprintf(
        messages$process_complete,
        n_imputed,
        n_total,
        round(imputation_rate * 100, 2),
        round(as.numeric(processing_time), 2)
      )
    )
  }

  # ===========================================================
  # S3 CLASS + METADATA
  # ===========================================================
  if (!inherits(df_filled, "climasus_df")) {
    # Create new climasus_df
    meta <- list(
      system = NULL,
      stage = "climate",
      type = "filled",
      spatial = inherits(df_filled, "sf"),
      temporal = NULL,
      created = Sys.time(),
      modified = Sys.time(),
      history = sprintf(
        "[%s] Gap-filling with dual ML model (multitarget)",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ),
      user = list()
    )

    base_classes <- setdiff(class(df_filled), "climasus_df")
    df_filled <- structure(
      df_filled,
      climasus_meta = meta,
      class = c("climasus_df", base_classes)
    )
  } else {
    # Already climasus_df - update metadata
    df_filled <- climasus_meta(
      df_filled,
      system = NULL,
      stage = "climate",
      type = "filled",
      temporal = list(
        start = min(df_filled$date),
        end = max(df_filled$date),
        source = source,
        imputed = TRUE,
        target_var = target_var,
        imputation_rate = n_imputed / nrow(df_filled),
        quality_threshold = quality_threshold
      )
    )
    df_filled <- climasus_meta(
      df_filled,
      add_history = "Gap-filling with dual ML model (multitarget)"
    )
  }
  # ===========================================================
  # DROP ENGINEERED FEATURES IF REQUESTED
  # ===========================================================
 if (!keep_features) {
    original_cols <- colnames(df)
    impute_cols <- paste0("is_imputed_", target_var)
    cols_to_keep <- unique(c(original_cols, impute_cols))
    cols_to_keep <- cols_to_keep[cols_to_keep %in% colnames(df_filled)]
    df_filled <- df_filled[, cols_to_keep, drop = FALSE]
  }

  return(df_filled)
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Detect Data Source from Column Names
#' @keywords internal
#' @noRd
.detect_data_source <- function(df) {
  col_names <- tolower(colnames(df))

  if (any(grepl("station_code|estacao_codigo", col_names))) {
    return("INMET")
  } else if (any(grepl("station_id|wban", col_names))) {
    return("NOAA")
  } else if (any(grepl("sensor_id|air_quality|pm25", col_names))) {
    return("AirQuality")
  } else {
    return("Unknown")
  }
}

#' Detect Datetime Column
#' @keywords internal
#' @noRd
.detect_datetime_column <- function(df, datetime_col, verbose, messages) {
  if (!is.null(datetime_col)) {
    return(datetime_col)
  }

  date_cols <- colnames(df)[sapply(df, function(x) {
    inherits(x, c("Date", "POSIXct", "POSIXlt"))
  })]

  if (length(date_cols) == 0) {
    cli::cli_abort(messages$no_datetime_found)
  }

  datetime_col <- date_cols[1]

  # if (verbose) {
  #   cli::cli_alert_info(sprintf(messages$datetime_detected, datetime_col))
  # }

  return(datetime_col)
}

#' Detect Station Column
#' @keywords internal
#' @noRd
.detect_station_column <- function(df, station_col, source, verbose, messages) {
  if (!is.null(station_col)) {
    if (station_col %in% colnames(df)) {
      return(station_col)
    } else {
      cli::cli_warn(sprintf(messages$station_col_not_found, station_col))
    }
  }

  # Source-specific detection
  station_patterns <- list(
    INMET = c("station_code", "estacao_codigo", "codigo_estacao"),
    NOAA = c("station_id", "station_identifier", "ws", "wd"),
    AirQuality = c("sensor_id", "site_id", "location_id"),
    Unknown = c("station", "site", "location", "id")
  )

  patterns <- station_patterns[[source]] %||% station_patterns$Unknown

  for (pattern in patterns) {
    matching_cols <- colnames(df)[grepl(pattern, tolower(colnames(df)))]
    if (length(matching_cols) > 0) {
      station_col <- matching_cols[1]
      # if (verbose) {
      #   cli::cli_alert_info(sprintf(messages$station_detected, station_col))
      # }
      return(station_col)
    }
  }

  # Fallback: create synthetic station column
  if (verbose) {
    cli::cli_alert_warning(messages$no_station_found)
  }

  return(".station_all")
}

#' Get Multilingual Messages
#' @keywords internal
#' @noRd
.get_fill_gaps_messages <- function(lang = "pt") {
  messages <- list(
    pt = list(
      starting_process = "Iniciando preenchimento de lacunas...",
      quality_filter = "Aplicando filtro de qualidade por estacao...",
      source_detected = function(s) sprintf("Fonte de dados detectada: %s", s),
      datetime_detected = function(c) sprintf("Coluna de data/hora detectada: %s", c),
      station_detected = function(c) sprintf("Coluna de estacao detectada: %s", c),
      no_datetime_found = "Nenhuma coluna de data/hora encontrada.",
      no_station_found = "Nenhuma coluna de estacao encontrada. Processando todos os dados juntos.",
      station_col_not_found = "Coluna de estacao '%s' nao encontrada.",
      skipping_stations = "Pulando %d estacao(oes) com >%d%% de dados faltantes: %s",
      parallel_setup = "Configurando processamento paralelo com %d nucleos",
      processing_stations = "Processando estacoes em paralelo..."
    ),
    en = list(
      starting_process = "Starting gap filling process...",
      quality_filter = "Applying quality filter by station...",
      source_detected = function(s) sprintf("Data source detected: %s", s),
      datetime_detected = function(c) sprintf("Datetime column detected: %s", c),
      station_detected = function(c) sprintf("Station column detected: %s", c),
      no_datetime_found = "No datetime column found.",
      no_station_found = "No station column found. Processing all data together.",
      station_col_not_found = "Station column '%s' not found.",
      skipping_stations = "Skipping %d station(s) with >%d%% missing values: %s",
      parallel_setup = "Setting up parallel processing with %d workers",
      processing_stations = "Processing stations in parallel..."
    ),
    es = list(
      starting_process = "Iniciando proceso de relleno de brechas...",
      quality_filter = "Aplicando filtro de calidad por estacion...",
      source_detected = function(s) sprintf("Fuente de datos detectada: %s", s),
      datetime_detected = function(c) sprintf("Columna de fecha/hora detectada: %s", c),
      station_detected = function(c) sprintf("Columna de estacion detectada: %s", c),
      no_datetime_found = "No se encontro columna de fecha/hora.",
      no_station_found = "No se encontro columna de estacion. Procesando todos los datos juntos.",
      station_col_not_found = "Columna de estacion '%s' no encontrada.",
      skipping_stations = "Omitiendo %d estacion(es) con >%d%% de valores faltantes: %s",
      parallel_setup = "Configurando procesamiento paralelo con %d trabajadores",
      processing_stations = "Procesando estaciones en paralelo..."
    )
  )
  lang <- match.arg(lang, names(messages))
  return(messages[[lang]])
}

#' Process Single Station with Dual-Model XGBoost Strategy (PYTHON STRATEGY)
#'
#' COMPLETE REWRITE following the Python approach:
#' 1. Train on COMPLETE data only (dropna)
#' 2. Make predictions in BATCH (not per-row loop)
#' 3. Use simple, robust features (no lag complexity)
#' 4. Fast vectorized operations
#'
#' This approach is proven to work and runs in minutes, not hours.
#'
#' @param station_df Data frame with station data
#' @param target_var Name of target variable to impute
#' @param station_col Name of station column
#' @param lag_periods Vector of lag periods (OPTIONAL, for advanced features)
#' @param rolling_windows Vector of rolling window sizes (OPTIONAL)
#' @param model_params List of XGBoost parameters
#' @param verbose Logical, print progress messages
#' @param system_name Optional system identifier
#'
#' @return Data frame with imputed values and is_imputed column
#'
#' @keywords internal
#' @noRd
.process_single_station_xgboost <- function(
  station_df,
  target_var,
  station_col,
  lag_periods = c(1, 24),
  rolling_windows = c(6, 24),
  model_params = list(),
  verbose = TRUE
) {
  # ---- Step 0: Sort and Initialize ----
  if ("date" %in% colnames(station_df)) {
    station_df <- station_df %>% dplyr::arrange(.data$date)
  }

  if (!"is_imputed" %in% colnames(station_df)) {
    station_df$is_imputed <- FALSE
  }
    # Handle solar radiation
  if ("sr_kj_m2" %in% colnames(station_df)) {
    station_df <- .handle_solar_radiation(station_df, "sr_kj_m2", "date")
  }
  # ---- Step 1: Identify Missing and Training Data ----
  original_nas_idx <- which(is.na(station_df[[target_var]]))
  train_idx <- which(!is.na(station_df[[target_var]]))
  base_train_idx <- train_idx

  # ---- Step 2: Create SIMPLE Temporal Features (no lags yet) ----
  station_df <- create_time_features(station_df, "date")
  station_df <- create_lag_features(
    station_df,
    target_cols = target_var,
    lag_periods = lag_periods, # ex: c(1, 3, 6, 12, 24)
    datetime_col = "date",
    verbose = FALSE
  )
  station_df <- create_rolling_features(
    station_df,
    target_vars = target_var,
    windows = rolling_windows, # ex: c(3, 6, 12)
    stats = c("mean", "sd") # Python usa "std", R usa "sd"
  )

  # ---- Step 3: Identify Feature Sets ----
  all_numeric <- colnames(station_df)[sapply(station_df, is.numeric)]

  exclude_cols <- c(
    target_var, "year", "latitude", "longitude", "altitude",
    #target_var,
    station_col,
    "is_imputed"
  )

  # Temporal features (from create_time_features)
  temporal_features <- all_numeric[grepl(
    "hour|minute|day|month|dayofyear|weekday|quarter|weekofyear|season|is_|solar_|heating_|cooling_|lunar_|synoptic_|weekly_|fourier_|time_index|days_since|months_since|years_since|potential_|evapotranspiration_|phase|rain_season|zone",
    all_numeric,
    ignore.case = TRUE
  )]

  lag_rolling_features <- all_numeric[grepl(
    "lag_|lead_|rolling_",
    all_numeric,
    ignore.case = TRUE
  )]

  temporal_features <- c(temporal_features, lag_rolling_features)

  # Everything else numeric is meteorological/covariate
  available_meteo <- setdiff(
    all_numeric,
    c(temporal_features, lag_rolling_features, exclude_cols)
  )

  # ---- Step 5: Train Model A (All Features) ----
  model_a <- NULL
  feature_cols_a <- c(temporal_features, available_meteo)

  if (length(feature_cols_a) > 0) {
    model_a <- tryCatch(
      {
        X_train <- station_df[train_idx, feature_cols_a, drop = FALSE]
        y_train <- station_df[[target_var]][train_idx]

        params <- list(
          booster = "gbtree",
          objective = "reg:squarederror",
          eta = 0.1,
          max_depth = 3,
          subsample = 0.8,
          colsample_bytree = 0.8,
          min_child_weight = 5,
          alpha = 0.0, 
          lambda = 1.5, 
          nthread = parallel::detectCores() - 1,
          tree_method = "hist" 
        )

        dtrain <- xgboost::xgb.DMatrix(
          data = as.matrix(X_train),
          label = y_train
        )
        nrounds <- params$nrounds %||% 150
        model <- xgboost::xgb.train(
          params = params,
          data = dtrain,
          nrounds = nrounds,
          verbose = 0
        )

        if (verbose) {
          cli::cli_alert_success("Model A trained successfully")
        }
        model
      },
      error = function(e) {
        if (verbose) {
          cli::cli_alert_warning(sprintf(
            "Model A training failed: %s",
            e$message
          ))
        }
        NULL
      }
    )
  }

  # ---- Step 6: Train Model B (Temporal Features Only) ----
  model_b <- NULL

  if (length(temporal_features) > 0) {
    model_b <- tryCatch(
      {
        X_train <- station_df[train_idx, temporal_features, drop = FALSE]
        y_train <- station_df[[target_var]][train_idx]

        params <- list(
          booster = "gbtree",
          objective = "reg:squarederror",
          eta = 0.1,
          max_depth = 3,
          subsample = 0.8,
          colsample_bytree = 0.8,
          min_child_weight = 5,
          alpha = 0.0, 
          lambda = 1.5, 
          nthread = parallel::detectCores() - 1,
          tree_method = "hist" 
        )
        dtrain <- xgboost::xgb.DMatrix(
          data = as.matrix(X_train),
          label = y_train
        )
        nrounds <- params$nrounds %||% 150
        model <- xgboost::xgb.train(
          params = params,
          data = dtrain,
          nrounds = nrounds,
          verbose = 0
        )


        if (verbose) {
          cli::cli_alert_success("Model B trained successfully")
        }
        model
      },
      error = function(e) {
        if (verbose) {
          cli::cli_alert_warning(sprintf(
            "Model B training failed: %s",
            e$message
          ))
        }
        NULL
      }
    )
  }

  if (is.null(model_a) && is.null(model_b)) {
    if (verbose) {
      cli::cli_alert_warning("No models trained successfully")
    }
    return(station_df)
  }

  # ---- Step 7: BATCH PREDICTIONS (not per-row loop!) ----
  if (verbose) {
    cli::cli_alert_info("Making predictions...")
  }

  missing_data <- station_df[original_nas_idx, , drop = FALSE]
  predictions <- numeric(length(original_nas_idx))
  model_used <- character(length(original_nas_idx))

  # ---- Predict with Model A (if meteorological data available) ----
  if (!is.null(model_a)) {
    has_meteo <- stats::complete.cases(missing_data[, available_meteo, drop = FALSE])

    if (any(has_meteo)) {
      X_pred <- missing_data[has_meteo, feature_cols_a, drop = FALSE]

      # Impute any remaining NAs with column means from training data
      for (col in colnames(X_pred)) {
        col_mean <- mean(
          station_df[train_idx, col, drop = TRUE],
          na.rm = TRUE
        )
        X_pred[[col]][is.na(X_pred[[col]])] <- col_mean
      }

      # BATCH prediction (single call, not 113k calls!)
      y_train_mean <- mean(y_train, na.rm = TRUE)
      preds <- stats::predict(model_a, as.matrix(X_pred))
      preds <- ifelse(is.na(preds), y_train_mean, preds)

      predictions[has_meteo] <- preds
      model_used[has_meteo] <- "A"

      if (verbose) {
        cli::cli_alert_info(sprintf("Model A: %d predictions", sum(has_meteo)))
      }
    }
  }
  # ---- Predict with Model B (fallback for missing meteorological data) ----
  needs_b <- is.na(model_used) | model_used == ""

  if (!is.null(model_b) && any(needs_b)) {
    X_pred <- missing_data[needs_b, temporal_features, drop = FALSE]

    # Impute any remaining NAs
    for (col in colnames(X_pred)) {
      col_mean <- mean(
        station_df[train_idx, col, drop = TRUE],
        na.rm = TRUE
      )
      X_pred[[col]][is.na(X_pred[[col]])] <- col_mean
    }

    # BATCH prediction
    y_train_mean <- mean(y_train, na.rm = TRUE)
    preds <- stats::predict(model_b, as.matrix(X_pred))
    preds <- ifelse(is.na(preds), y_train_mean, preds)

    predictions[needs_b] <- preds
    model_used[needs_b] <- "B"

    if (verbose) {
      cli::cli_alert_info(sprintf("Model B: %d predictions", sum(needs_b)))
    }
  }
  # ---- Step 8: Update DataFrame ----
  station_df[[target_var]][original_nas_idx] <- predictions
  station_df$is_imputed[original_nas_idx] <- TRUE

  return(station_df)
}

#' Create Time-Based Features
#'
#' @description Adds comprehensive time-related features including cyclical
#'   transformations and categorical indicators for temporal patterns.
#'
#' @keywords internal
#' @noRd
create_time_features <- function(data, datetime_col = 'date') {
  `.` <- NULL
  original_cols <- colnames(data)

  if (!inherits(data[[datetime_col]], "POSIXct")) {
    data[[datetime_col]] <- as.POSIXct(data[[datetime_col]])
  }

  df <- data

  df <- df %>%
    dplyr::mutate(
      hour = lubridate::hour(!!rlang::sym(datetime_col)),
      minute = lubridate::minute(!!rlang::sym(datetime_col)),
      day = lubridate::day(!!rlang::sym(datetime_col)),
      month = lubridate::month(!!rlang::sym(datetime_col)),
      year = lubridate::year(!!rlang::sym(datetime_col)),
      dayofyear = lubridate::yday(!!rlang::sym(datetime_col)),
      weekday = lubridate::wday(!!rlang::sym(datetime_col), week_start = 1) - 1,
      quarter = lubridate::quarter(!!rlang::sym(datetime_col)),
      weekofyear = lubridate::week(!!rlang::sym(datetime_col)),

      is_weekend = as.integer(.data$weekday %in% c(5, 6)),

      hour_sin = sin(2 * pi * .data$hour / 24),
      hour_cos = cos(2 * pi * .data$hour / 24),
      month_sin = sin(2 * pi * .data$month / 12),
      month_cos = cos(2 * pi * .data$month / 12),
      dayofyear_sin = sin(2 * pi * .data$dayofyear / 365.25),
      dayofyear_cos = cos(2 * pi * .data$dayofyear / 365.25),
      weekday_sin = sin(2 * pi * .data$weekday / 7),
      weekday_cos = cos(2 * pi * .data$weekday / 7),
      quarter_sin = sin(2 * pi * .data$quarter / 4),
      quarter_cos = cos(2 * pi * .data$quarter / 4),
      weekofyear_sin = sin(2 * pi * .data$weekofyear / 52),
      weekofyear_cos = cos(2 * pi * .data$weekofyear / 52),

      hour_decimal = .data$hour + .data$minute / 60.0,
      minute_decimal = .data$minute / 60.0,

      season = dplyr::case_when(
        .data$month %in% c(12, 1, 2) ~ 0, # summer
        .data$month %in% c(3, 4, 5) ~ 1, # autumn
        .data$month %in% c(6, 7, 8) ~ 2, # winter
        .data$month %in% c(9, 10, 11) ~ 3, # spring
        TRUE ~ NA_real_
      ),

      is_morning = as.integer(.data$hour >= 6 & .data$hour < 12),
      is_afternoon = as.integer(.data$hour >= 12 & .data$hour < 18),
      is_evening = as.integer(.data$hour >= 18 & .data$hour < 22),
      is_night = as.integer(.data$hour >= 22 | .data$hour < 6),

      solar_noon_proximity = abs(.data$hour - 12) / 12,
      is_sunrise = as.integer(.data$hour >= 5 & .data$hour <= 7),
      is_sunset = as.integer(.data$hour >= 17 & .data$hour <= 19),

      is_dry_season = as.integer(.data$month %in% c(5:10)),
      is_rainy_season = as.integer(.data$month %in% c(11:4)),

      day_length = dplyr::case_when(
        .data$month %in% c(12) ~ 13.5,
        .data$month %in% c(6) ~ 10.5,
        TRUE ~ 12 + 2 * sin(2 * pi * (.data$dayofyear - 80) / 365.25)
      ),

      is_holiday = 0,
      is_working_hour = as.integer(
        .data$hour >= 8 & .data$hour < 18 & !(.data$weekday %in% c(5, 6))
      ),

      diurnal_phase = dplyr::case_when(
        .data$hour >= 5 & .data$hour < 11 ~ "morning_rise",
        .data$hour >= 11 & .data$hour < 14 ~ "midday",
        .data$hour >= 14 & .data$hour < 18 ~ "afternoon_decline",
        .data$hour >= 18 & .data$hour < 22 ~ "evening",
        TRUE ~ "night"
      )
    )

  season_dummies <- stats::model.matrix(
    ~ factor(.data$season, levels = 0:3) - 1,
    data = df
  )
  colnames(season_dummies) <- c("season_0", "season_1", "season_2", "season_3")
  df <- cbind(df, season_dummies)

  df <- df %>%
    dplyr::mutate(
      hour_month_interaction = .data$hour * .data$month / 288,
      season_hour_interaction = .data$season * .data$hour / 72
    )

  df <- df %>%
    dplyr::mutate(
      fourier_year_sin1 = sin(2 * pi * .data$dayofyear / 365.25),
      fourier_year_cos1 = cos(2 * pi * .data$dayofyear / 365.25),
      fourier_year_sin2 = sin(4 * pi * .data$dayofyear / 365.25),
      fourier_year_cos2 = cos(4 * pi * .data$dayofyear / 365.25),

      fourier_day_sin1 = sin(2 * pi * .data$hour_decimal / 24),
      fourier_day_cos1 = cos(2 * pi * .data$hour_decimal / 24),
      fourier_day_sin2 = sin(4 * pi * .data$hour_decimal / 24),
      fourier_day_cos2 = cos(4 * pi * .data$hour_decimal / 24)
    )

  df <- df %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(
      "station_name",
      "station_code"
    )))) %>%
    dplyr::arrange(!!rlang::sym(datetime_col)) %>%
    dplyr::mutate(
      time_index = dplyr::row_number(),
      days_since_start = as.numeric(difftime(
        !!rlang::sym(datetime_col),
        min(!!rlang::sym(datetime_col)),
        units = "days"
      )),
      months_since_start = .data$days_since_start / 30.44,
      years_since_start = .data$days_since_start / 365.25
    ) %>%
    dplyr::ungroup()

  df <- df %>%
    dplyr::mutate(
      lunar_phase = (.data$day %% 29.53) / 29.53,
      synoptic_period = .data$dayofyear %% 5,
      weekly_cycle = .data$weekday / 6
    )

  df <- df %>%
    dplyr::mutate(
      heating_degree_hours = pmax(18 - .data$hour_decimal / 24 * 10, 0),
      cooling_degree_hours = pmax(.data$hour_decimal / 24 * 10 - 24, 0),

      solar_angle = 90 - abs(12 - .data$hour) * 15,
      potential_solar_radiation = pmax(sin(pi * .data$solar_angle / 180), 0),

      evapotranspiration_potential = dplyr::case_when(
        .data$season == 0 ~ 1.2,
        .data$season == 1 ~ 0.9,
        .data$season == 2 ~ 0.6,
        .data$season == 3 ~ 1.0,
        TRUE ~ 0.8
      ) *
        (1 - .data$is_night)
    )

  if ("diurnal_phase" %in% colnames(df)) {
    phase_dummies <- stats::model.matrix(~ diurnal_phase - 1, data = df)
    phase_dummy_cols <- as.data.frame(phase_dummies) %>%
      stats::setNames(gsub("diurnal_phase", "phase", colnames(.)))
    df <- cbind(df, phase_dummy_cols)
    df <- df %>% dplyr::select(-.data$diurnal_phase)
  }

  df <- df %>%
    dplyr::mutate(
      climate_zone = dplyr::case_when(
        .data$month %in% c(1:3, 10:12) & .data$hour >= 12 ~ "hot_humid",
        .data$month %in% c(4:9) & .data$hour >= 12 ~ "warm_dry",
        TRUE ~ "moderate"
      ),

      is_transition_season = as.integer(.data$month %in% c(3, 4, 9, 10)),

      rain_season_intensity = dplyr::case_when(
        .data$month == 1 ~ 0.9,
        .data$month == 2 ~ 0.8,
        .data$month == 3 ~ 0.7,
        .data$month == 12 ~ 0.8,
        TRUE ~ 0.3
      )
    )
  if ("climate_zone" %in% colnames(df)) {
    zone_dummies <- stats::model.matrix(~ climate_zone - 1, data = df)
    zone_dummy_cols <- as.data.frame(zone_dummies) %>%
      stats::setNames(gsub("climate_zone", "zone", colnames(.)))
    df <- cbind(df, zone_dummy_cols)
    df <- df %>% dplyr::select(-.data$climate_zone)
  }

  return(dplyr::as_tibble(df))
}

#' Create Lagged Features with Robust Imputation
#'
#' @description Adds lagged features and fills resulting NAs to allow immediate model training.
#' @keywords internal
#' @noRd
create_lag_features <- function(
  data,
  target_cols,
  lag_periods = c(1, 2, 3, 6, 12),
  group_cols = NULL,
  datetime_col = "date",
  include_forward_lags = TRUE,
  include_seasonal_lags = TRUE,
  include_diffs = TRUE,
  include_ratios = FALSE,
  na_action = TRUE, #
  verbose = TRUE
) {
  # suppressPackageStartupMessages({
  #   library(dplyr)
  #   library(zoo)
  # })

  available_cols <- intersect(target_cols, colnames(data))
  original_all_cols <- colnames(data)

  if (!is.null(group_cols) && datetime_col %in% colnames(data)) {
    data <- data %>% dplyr::arrange(!!!rlang::syms(c(group_cols, datetime_col)))
  } else if (datetime_col %in% colnames(data)) {
    data <- data %>% dplyr::arrange(!!rlang::sym(datetime_col))
  }
  create_lags_for_col <- function(df, col_name, periods, grp_cols = NULL) {
    for (lag_val in periods) {
      if (lag_val > 0) {
        lag_name <- paste0(col_name, "_lag_", lag_val)
        if (!is.null(grp_cols)) {
          df <- df %>%
            dplyr::group_by(!!!rlang::syms(grp_cols)) %>%
            dplyr::mutate(
              !!lag_name := dplyr::lag(!!rlang::sym(col_name), lag_val)
            ) %>%
            dplyr::ungroup()
        } else {
          df <- df %>%
            dplyr::mutate(
              !!lag_name := dplyr::lag(!!rlang::sym(col_name), lag_val)
            )
        }
      }
    }

    if (include_forward_lags) {
      for (lead_val in periods) {
        if (lead_val > 0) {
          lead_name <- paste0(col_name, "_lead_", lead_val)
          if (!is.null(grp_cols)) {
            df <- df %>%
              dplyr::group_by(!!!rlang::syms(grp_cols)) %>%
              dplyr::mutate(
                !!lead_name := dplyr::lead(!!rlang::sym(col_name), lead_val)
              ) %>%
              dplyr::ungroup()
          } else {
            df <- df %>%
              dplyr::mutate(
                !!lead_name := dplyr::lead(!!rlang::sym(col_name), lead_val)
              )
          }
        }
      }
    }
    return(df)
  }

  result_data <- data
  for (col in available_cols) {
    result_data <- create_lags_for_col(
      result_data,
      col,
      lag_periods,
      group_cols
    )
  }

  # --- Lags Sazonais (24h e 168h) ---
  if (include_seasonal_lags && datetime_col %in% colnames(result_data)) {
    for (col in available_cols) {
      lag24_name <- paste0(col, "_same_hour_yesterday")
      lag168_name <- paste0(col, "_same_hour_last_week")

      if (!is.null(group_cols)) {
        result_data <- result_data %>%
          dplyr::group_by(!!!rlang::syms(group_cols)) %>%
          dplyr::mutate(
            !!lag24_name := dplyr::lag(!!rlang::sym(col), 24),
            !!lag168_name := dplyr::lag(!!rlang::sym(col), 168)
          ) %>%
          dplyr::ungroup()
      } else {
        result_data <- result_data %>%
          dplyr::mutate(
            !!lag24_name := dplyr::lag(!!rlang::sym(col), 24),
            !!lag168_name := dplyr::lag(!!rlang::sym(col), 168)
          )
      }
    }
  }

  # --- Diffs e Ratios ---
  if (include_diffs && length(lag_periods) >= 2) {
    for (col in available_cols) {
      lag_cols <- grep(
        paste0("^", col, "_lag_"),
        colnames(result_data),
        value = TRUE
      )
      if (length(lag_cols) >= 2) {
        lag_nums <- as.numeric(gsub(paste0(col, "_lag_"), "", lag_cols))
        ord <- order(lag_nums)
        lag_cols <- lag_cols[ord]
        lag_nums <- lag_nums[ord]

        # Diff Current vs Lag 1
        diff_curr <- paste0(col, "_diff_current_lag_", lag_nums[1])
        result_data <- result_data %>%
          dplyr::mutate(
            !!diff_curr := !!rlang::sym(col) - !!rlang::sym(lag_cols[1])
          )
      }
    }
  }

  # ==============================================================================
  # MECANISMO DE PREENCHIMENTO DE FALHAS (GAP-FILLING)
  # ==============================================================================
  if (na_action == TRUE) {
    created_cols <- setdiff(colnames(result_data), original_all_cols)
    created_numeric <- created_cols[sapply(created_cols, function(x) {
      is.numeric(result_data[[x]])
    })]

    if (length(created_numeric) > 0) {
      impute_vector <- function(x) {
        if (all(is.na(x))) {
          return(x)
        }

        x <- zoo::na.approx(x, na.rm = FALSE, rule = 2)

        x <- zoo::na.locf(x, na.rm = FALSE)

        x <- zoo::na.locf(x, fromLast = TRUE, na.rm = FALSE)

        if (any(is.na(x))) {
          x[is.na(x)] <- mean(x, na.rm = TRUE)
        }
        return(x)
      }

      if (!is.null(group_cols)) {
        result_data <- result_data %>%
          dplyr::group_by(!!!rlang::syms(group_cols)) %>%
          dplyr::mutate(dplyr::across(
            dplyr::all_of(created_numeric),
            impute_vector
          )) %>%
          dplyr::ungroup()
      } else {
        result_data <- result_data %>%
          dplyr::mutate(dplyr::across(
            dplyr::all_of(created_numeric),
            impute_vector
          ))
      }
    }
  }

  return(dplyr::as_tibble(result_data))
}

#' Create Rolling Window Features with Imputation
#'
#' @description Adds rolling stats and fills initial NAs caused by window width.
#' @keywords internal
#' @noRd
create_rolling_features <- function(
  data,
  target_vars,
  windows = c(3, 6, 12, 24),
  stats = c("mean", "sd", "min", "max"),
  impute_nans = TRUE
) {
  # require(zoo)
  # require(dplyr)

  calculate_rolling_stats <- function(x, window, stat, impute = TRUE) {
    if (length(x) < window) {
      return(rep(mean(x, na.rm = TRUE), length(x)))
    }

    res <- rep(NA_real_, length(x))

    if (stat == "mean") {
      res <- zoo::rollmean(x, window, fill = NA, align = "right", na.rm = TRUE)
    } else if (stat == "sd") {
      res <- zoo::rollapply(
        x,
        window,
        sd,
        fill = NA,
        align = "right",
        na.rm = TRUE
      )
    } else if (stat == "min") {
      res <- zoo::rollapply(
        x,
        window,
        min,
        fill = NA,
        align = "right",
        na.rm = TRUE
      )
    } else if (stat == "max") {
      res <- zoo::rollapply(
        x,
        window,
        max,
        fill = NA,
        align = "right",
        na.rm = TRUE
      )
    }

    if (impute) {
      res <- zoo::na.approx(res, na.rm = FALSE, rule = 2)
      res <- zoo::na.locf(res, na.rm = FALSE)
      res <- zoo::na.locf(res, fromLast = TRUE, na.rm = FALSE)
      if (any(is.na(res))) {
        res[is.na(res)] <- mean(res, na.rm = TRUE)
      }
    }

    return(res)
  }

  for (col in target_vars) {
    if (!col %in% colnames(data)) {
      next
    }

    for (window in windows) {
      for (stat in stats) {
        new_col_name <- paste0(col, "_rolling_", stat, "_", window)
        data[[new_col_name]] <- calculate_rolling_stats(
          data[[col]],
          window,
          stat,
          impute = impute_nans
        )
      }
    }
  }

  return(dplyr::as_tibble(data))
}

# ---- SOLAR RADIATION HANDLING ----

#' Handle Nocturnal Solar Radiation Values
#'
#' @description Sets nocturnal solar radiation values to 0 for physical consistency
#'
#' @param data Data frame with solar radiation column
#' @param radiation_col Character. Name of solar radiation column
#' @param datetime_col Character. Name of datetime column
#' @param night_start Numeric. Start of night period (hour)
#' @param night_end Numeric. End of night period (hour)
#'
#' @return Data frame with corrected radiation values
#'
#' @keywords internal
#' @noRd
.handle_solar_radiation <- function(
    data,
    radiation_col = "sr_kj_m2",
    datetime_col = "date",
    night_start = 18,
    night_end = 6) {
  
  if (!radiation_col %in% colnames(data)) {
    return(data)
  }
  
  # Create copy to avoid modifying original
  df <- data
  
  # Extract hour from datetime
  df <- df %>%
    dplyr::mutate(
      .hour = lubridate::hour(!!rlang::sym(datetime_col)),
      .is_night = .data$.hour >= night_start | .data$.hour < night_end,
      !!radiation_col := dplyr::if_else(
        .data$.is_night & is.na(!!rlang::sym(radiation_col)),
        0,
        !!rlang::sym(radiation_col)
      )
    ) %>%
    dplyr::select(-.data$.hour, -.data$.is_night)
  
  return(df)
}


# ---- EVALUATION MODULE ----

#' Introduce Artificial Gaps
#'
#' @description Generate artificial missing values for evaluation
#'
#' @param data Data frame
#' @param target_var Character. Target variable
#' @param gap_percentage Numeric. Percentage of data to remove (0-1)
#' @param gap_mechanism Character. 'MCAR', 'MAR', or 'MNAR'
#'
#' @return List with gap_indices and data_with_gaps
#'
#' @keywords internal
#' @noRd
.introduce_nan <- function(
    data,
    target_var,
    gap_percentage = 0.2,
    gap_mechanism = "MCAR") {
  
  n <- nrow(data)
  n_gaps <- ceiling(n * gap_percentage)
  
  if (gap_mechanism == "MCAR") {
    # Missing Completely At Random
    gap_indices <- sample(seq_len(n), n_gaps, replace = FALSE)
  } else if (gap_mechanism == "MAR") {
    # Missing At Random (depends on other variables)
    gap_indices <- which(rank(data[[target_var]], na.last = "keep") <= n_gaps)
  } else if (gap_mechanism == "MNAR") {
    # Missing Not At Random (depends on target itself)
    gap_indices <- which(data[[target_var]] >stats::quantile(data[[target_var]], 0.9, na.rm = TRUE))
    gap_indices <- gap_indices[seq_len(min(n_gaps, length(gap_indices)))]
  }
  
  data_with_gaps <- data
  data_with_gaps[[target_var]][gap_indices] <- NA
  
  return(list(
    gap_indices = gap_indices,
    data_with_gaps = data_with_gaps
  ))
}


#' Introduce Artificial Gaps
#'
#' @description Generate artificial missing values for evaluation
#' @return List with gap_indices and data_with_gaps
#'
#' @keywords internal
#' @noRd
.evaluate_single_station <- function(
  station_df,
  target_var,
  station_col,
  gap_percentage = 0.2,
  gap_mechanism = "MCAR",
  ...
) {

  original_data <- station_df %>%
    dplyr::filter(!is.na(.data[[target_var]]))

  gap_info <- .introduce_nan(
    data = original_data,
    target_var = target_var,
    gap_percentage = gap_percentage,
    gap_mechanism = gap_mechanism
  )

  data_with_gaps <- gap_info$data_with_gaps
  gap_indices <- gap_info$gap_indices

  filled_data <- .process_single_station_xgboost(
    station_df = data_with_gaps,
    target_var = target_var,
    station_col = station_col,
    ...
  )

  true_values <- original_data[[target_var]][gap_indices]
  pred_values <- filled_data[[target_var]][gap_indices]

  rmse <- sqrt(mean((true_values - pred_values)^2, na.rm = TRUE))
  mae <- mean(abs(true_values - pred_values), na.rm = TRUE)
  r_squared <- 1 - (
    sum((true_values - pred_values)^2, na.rm = TRUE) /
      sum((true_values - mean(true_values, na.rm = TRUE))^2, na.rm = TRUE)
  )

  return(list(
    rmse = rmse,
    mae = mae,
    r_squared = r_squared,
    n_gaps = length(gap_indices),
    gap_percentage = gap_percentage,
    gap_mechanism = gap_mechanism
  ))
}
