#' @title Fill gaps in climate and air-quality time series using XGBoost
#'
#' @description
#' `sus_climate_fill_gaps()` imputes missing values in climate time series using
#' station-wise XGBoost models with automated feature engineering.
#' 
#' **Key features:**
#' \itemize{
#'   \item **Single-target focus**: Each call imputes ONE variable
#'   \item **Station-wise modeling**: Separate model per station
#'   \item **Temporal features**: Automatic creation of lags and rolling statistics
#'   \item **Quality control**: Stations with >`quality_threshold` missing are excluded
#'   \item **Parallel processing**: Stations processed in parallel for speed
#'   \item **Evaluation mode**: Assess accuracy by creating artificial gaps
#' }
#'
#' @param df 
#'   A data frame (or tibble) containing climate data, typically from `sus_climate_inmet()`.
#'   Must contain:
#'   \itemize{
#'     \item A datetime column (POSIXct or convertible)
#'     \item A station identifier column
#'     \item The target numeric column to be imputed
#'   }
#'
#' @param target_var 
#'   **Single** character string specifying the column to impute.
#'   Example: `target_var = "tair_dry_bulb_c"`.
#'
#' @param datetime_col 
#'   Character. Name of the datetime column. If `NULL` (default), auto-detected.
#'
#' @param station_col 
#'   Character. Name of the station identifier column. If `NULL`, auto-detected.
#'
#' @param quality_threshold 
#'   Numeric (0-1). Maximum allowed missing proportion per station.
#'   Stations exceeding this are excluded. Default: `0.4` (40%).
#'
#' @param run_evaluation 
#'   Logical. If `TRUE`, runs in evaluation mode:
#'   \itemize{
#'     \item Creates artificial MCAR gaps in observed data
#'     \item Imputes and compares predictions with true values
#'     \item Returns metrics by station
#'   }
#'   Default: `FALSE` (production mode).
#'
#' @param gap_percentage 
#'   Numeric (0-1). Proportion of data to set as missing in evaluation mode. The `"MCAR"` Missing Completely At Random is used as default.
#'   Default: `0.2` (20%).
#'
#' @param keep_features 
#'   Logical. If `TRUE`, retains engineered features (lags, rolling stats).
#'   Default: `FALSE` (returns only original columns + `is_imputed`).
#'
#' @param parallel 
#'   Logical. If `TRUE` (default), processes stations in parallel using `furrr`.
#'
#' @param workers 
#'   Integer. Number of parallel workers. If `NULL`, uses `availableCores() - 1`.
#'
#' @param verbose 
#'   Logical. If `TRUE` (default), prints progress messages.
#'
#' @param lang 
#'   Character. Message language: `"pt"` (Portuguese), `"en"` (English), or `"es"` (Spanish).
#'   Default: `"pt"`.
#'
#' @return
#' **Production mode** (`run_evaluation = FALSE`):
#' Returns a `tibble` (same class as input) with:
#' \itemize{
#'   \item Original columns plus imputed values in `target_var`
#'   \item `is_imputed`: Logical flag (TRUE for filled values)
#'   \item `climasus_meta` attribute with imputation metadata
#' }
#'
#' **Evaluation mode** (`run_evaluation = TRUE`):
#' Returns a list of class `climasus_eval` containing:
#' \itemize{
#'   \item `$data`: Data frame with artificial gaps and predictions
#'   \item `$metrics`: A `tibble` with per-station performance metrics:
#'     \itemize{
#'       \item `station`: Station identifier
#'       \item `rmse`: Root Mean Squared Error
#'       \item `mae`: Mean Absolute Error
#'       \item `r_squared`: R-squared (lower than 1, higher is better)
#'       \item `smape`: Symmetric MAPE (0-200%, lower is better)
#'       \item `slope_bias`: Should be close to 1.0, indicating underestimate and overestimate
#'       \item `n_gaps`: Number of artificial gaps
#'     }
#' }
#'
#' @section Methodological Notes:
#' 
#' **Important limitations:**
#' \itemize{
#'   \item **Not forecasting**: Predicts only where data are missing
#'   \item **No future data**: Uses only past information (lags)
#'   \item **Station independence**: Models don't share information
#'   \item **Quality filter**: Stations with >`quality_threshold` missing are skipped
#' }
#'
#' **Feature engineering:**
#' Automatically creates:
#' \itemize{
#'   \item Time features: hour, day, month, year, cyclic transforms
#'   \item Lag features: 1,2,3,6,12,24,48,72,168 periods
#'   \item Rolling statistics: mean and sd over windows 3,6,12,24,48,72
#' }
#'
#' @section Evaluation Mode Details:
#' 
#' When `run_evaluation = TRUE`, the function:
#' \enumerate{
#'   \item Creates artificial gaps (MCAR by default)
#'   \item Runs imputation on the data with gaps
#'   \item Compares predictions with true values
#'   \item Returns per-station performance
#' }
#' 
#' This helps assess model accuracy before production use.
#'
#' @examples
#' \dontrun{
#' # ===== PRODUCTION MODE =====
#' # Impute missing temperature data
#' filled_temp <- sus_climate_fill_gaps(
#'   df = climate_data,
#'   target_var = "tair_dry_bulb_c",
#'   quality_threshold = 0.3,
#'   parallel = TRUE
#' )
#'
#'
#' # ===== EVALUATION MODE =====
#' # Assess model performance on a subset
#' eval_results <- sus_climate_fill_gaps(
#'   df = climate_data,
#'   target_var = "ws_2_m_s",
#'   run_evaluation = TRUE,
#'   gap_percentage = 0.2,
#'   workers = 4
#' )
#'
#' # View performance metrics
#' eval_results$metrics
#' 
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
  quality_threshold = 0.4,
  run_evaluation = FALSE,
  gap_percentage = 0.2,
  keep_features = FALSE,
  parallel = TRUE,
  workers = NULL,
  verbose = TRUE,
  lang = "pt"
) {
  
  # Check pak
  rlang::check_installed(c("furrr", "xgboost", "zoo", "MASS"), 
                       reason = "to run sus_climate_fill_gaps()")
  
  # Set seed for reproducibility
  set.seed(42)

  # ---- Initialize Messages ----
  messages <- .get_fill_gaps_messages(lang)

  # ---- Detect Data Source ----
  source <- .detect_data_source(df)

  # ---- Auto-detect Columns ----
  
  datetime_col <- .detect_datetime_column(df, datetime_col, verbose, messages)
  station_col <- .detect_station_column(df, station_col, source, verbose, messages)

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

 if (length(target_var) > 1) {
    msg <- switch(lang,
      "pt" = "Apenas uma variavel alvo e permitida. Por favor, selecione apenas {.strong one target_var}.",
      "es" = "Solo se permite una variable objetivo. Por favor, seleccione solo {.strong una target_var}.",
      "en" = "Just one target variable is possible. Please, provide {.strong one target_var} at a time.",
      "Just one target variable is possible."
    )
    cli::cli_abort(msg)
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
  
  df_filtered <- df %>%
    dplyr::filter(!!rlang::sym(station_col) %in% valid_stations)

  # ===========================================================
  # BRANCH PRINCIPAL
  # ===========================================================
  if (run_evaluation) {
      
     if (verbose) {
      msg <- switch(lang,
        "en" = "{.info Evaluating model performance for {.field {target_var}}}",
        "pt" = "{.info Avaliando performance do modelo para {.field {target_var}}}",
        "es" = "{.info Evaluando desempeño del modelo para {.field {target_var}}}",
        "{.info Evaluating model performance: {.field {target_var}}}"
      )
      
      cli::cli_h3(msg)
    }
      
      df_filled <- .evaluate_single_station(
        df = df_filtered, 
        target_var = target_var,
        station_col = station_col,
        gap_percentage = gap_percentage,
        parallel = parallel,
        workers = future::availableCores() - 1,
        lang = lang,
        verbose = verbose
      ) 
      if (verbose) cli::cli_alert_success("Evaluation complete")
      
      return(df_filled) 
      
  } else { 

    # ===========================================================
  # PARALLEL SETUP
  # ===========================================================
  if(parallel && length(valid_stations) > 1) { 

    if (verbose) {cli::cli_alert_info(messages$processing_stations)}

    if (is.null(workers)) { workers <- max(1, future::availableCores() - 1)}

    if (verbose) { cli::cli_alert_info(sprintf(messages$parallel_setup, workers))}

    old_plan <- future::plan()
    future::plan(future::multisession, workers = workers)
    on.exit(future::plan(old_plan), add = TRUE)

  }
 
  # ===========================================================
  # PROCESS STATIONS IN PARALLEL
  # ===========================================================

  # Process stations in parallel
  stations_list <- df_filtered %>%
    dplyr::group_split(!!rlang::sym(station_col), .keep = TRUE)

  results_list <- furrr::future_map(
    stations_list,
    function(station_df) {
      tryCatch(
        .process_single_station_xgboost2(
          station_df = station_df,
          target_var = target_var,
          station_col = station_col,
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

  #Restore original datetime column name if needed
  if (datetime_col != "date") {
    df_filled <- df_filled %>%
      dplyr::rename(!!rlang::sym(datetime_col) := date)
  }
   # ===========================================================
  # STATISTICS PER VARIABLE
  # ===========================================================
  n_imputed <- sum(df_filled$is_imputed, na.rm = TRUE)
  n_total <- nrow(df_filled)
  imputation_rate <- n_imputed / n_total * 100

  if (verbose) {
    cli::cli_alert_success(
      "Imputation completed for {.strong {target_var}}: {.val {n_imputed}} row{?s} imputed out of {.val {n_total}} 
      ({.strong {round(imputation_rate, 2)}%})."
    )
  }
  
  # ===========================================================
  # DROP ENGINEERED FEATURES IF REQUESTED
  # ===========================================================
  if (!keep_features) {
      original_cols <- colnames(df)
      impute_cols <- paste0(grep("^is_imputed", colnames(df_filled), value = TRUE),"_", target_var)
      cols_to_keep <- c(original_cols, impute_cols)
      cols_to_keep <- cols_to_keep[cols_to_keep %in% colnames(df_filled)]
      df_filled <- df_filled[, cols_to_keep, drop = FALSE]
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
        "[%s] Gap-filling with dual ML model",
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
        evaluation = FALSE,
        target_var = target_var,
        imputation_rate = n_imputed / n_total,
        quality_threshold = quality_threshold
      )
    )
    df_filled <- climasus_meta(
      df_filled,
      add_history = "Gap-filling with ML model"
    )
  }

    return(df_filled)

  }
  
 
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

#' Process Single Station with XGBoost (Non-Negative Support)
#'
#' @return Data frame with imputed values and is_imputed column
#' @keywords internal
#' @noRd
.process_single_station_xgboost2 <- function(
    station_df,
    target_var,
    station_col,
    model_params = list(),
    verbose = TRUE
) {
  
  get_transform_rule <- function(target_var) {
    dplyr::case_when(
      grepl("rainfall", target_var) ~ "log1p",
      grepl("ws_|gust", target_var) ~ "sqrt",
      grepl("sr_kj", target_var)    ~ "sqrt",
      grepl("rh_|patm", target_var) ~ "none", # Adaptativo para estabilidade
      TRUE                          ~ "none"
    )
  }
  transform_type <- get_transform_rule(target_var)

if (!target_var %in% colnames(station_df)) {
  cli::cli_abort("target_var '{target_var}' not found in data")
}

if ("date" %in% colnames(station_df)) {
  station_df <- station_df %>% dplyr::arrange(.data$date)
}
if (!"is_imputed" %in% colnames(station_df)) {
  station_df$is_imputed <- FALSE
}

if (target_var == "sr_kj_m2" && "date" %in% colnames(station_df)) {
  station_df <- .handle_solar_radiation(station_df, target_var, "date")
}

  original_nas_idx <- which(is.na(station_df[[target_var]]))
train_idx <- which(!is.na(station_df[[target_var]]))

if (length(train_idx) < 30) {
  if (verbose) cli::cli_alert_warning("Insufficient training data ({length(train_idx)} obs), skipping")
  return(station_df)
}

y_train_original <- station_df[[target_var]][train_idx]
y_train <- y_train_original
transform_params <- list(shift = 0, lambda = 1, type = transform_type)

if (transform_type != "none") {
  
  if (transform_type %in% c("log", "boxcox")) {
    min_val <- min(y_train, na.rm = TRUE)
    shift <- if (min_val <= 0) abs(min_val) + 0.001 else 0
    y_train_shifted <- y_train + shift
    transform_params$shift <- shift
    
    if (transform_type == "log") {
      y_train <- log(y_train_shifted)
    } else {
      # Box-Cox
      lambda <- tryCatch({
        bc <- MASS::boxcox(y_train_shifted ~ 1, plotit = FALSE)
        bc$x[which.max(bc$y)]
      }, error = function(e) 1)
      
      y_train <- if (abs(lambda) < 1e-6) log(y_train_shifted) else (y_train_shifted^lambda - 1) / lambda
      transform_params$lambda <- lambda
    }
  } 
  
  else if (transform_type == "log1p") {
    y_train <- log1p(y_train)
  } else if (transform_type == "sqrt") {
    y_train <- sqrt(y_train)
  }
    
    if (verbose) {
      cli::cli_alert_success("Transformation applied: {.field {transform_type}} | Shift: {round(transform_params$shift, 4)}")
    }
  }
  
  # ============================================================================
  # CRIAR FEATURES
  # ============================================================================
  
  station_df <- create_time_features(station_df, "date")
  
  station_df <- create_lag_features(
    station_df,
    target_cols = target_var,
    verbose = FALSE
  )
  station_df <- create_rolling_features(
    station_df,
    target_vars = target_var
  )
  # ============================================================================
  # IDENTIFICAR FEATURES
  # ============================================================================
  
  all_numeric <- colnames(station_df)[sapply(station_df, is.numeric)]
  
  exclude_cols <- c(
    target_var, 
    station_col, "is_imputed"
  )
  
  # Features temporais
  temporal_features <- all_numeric[grepl(
    "hour|minute|day|month|dayofyear|weekday|quarter|weekofyear|season|is_|hour_|sin|cos",
    all_numeric,
    ignore.case = TRUE
  )]
  
  # Features de lag/rolling
  lag_rolling_features <- all_numeric[grepl(
    paste0(target_var, "_(lag|lead|rolling|same_hour|diff)"),
    all_numeric,
    ignore.case = TRUE
  )]
  
  temporal_features <- unique(c(temporal_features, lag_rolling_features))
  temporal_features <- setdiff(temporal_features, exclude_cols)
  
  available_meteo <- setdiff(all_numeric, c(temporal_features, lag_rolling_features, exclude_cols))
  
  # ============================================================================
  # TREINAR MODELOS (APENAS MODELO B - TEMPORAL)
  # ============================================================================
  
  X_train <- station_df[train_idx, temporal_features, drop = FALSE]
    
  model <- tryCatch({
    
    params <- list(
      booster = "gbtree",
      eval_metric = "rmse",
      objective = "reg:squarederror",
      eta = 0.05, 
      max_depth = 4,
      subsample = 0.7,
      colsample_bytree = 0.7,
      min_child_weight = 3,
      alpha = 0.5, 
      lambda = 2.0, 
      gamma = 0.1, 
      nthread = 1,
      tree_method = "hist"
    )   

    params <- utils::modifyList(params, model_params)
    nrounds <- params$nrounds %||% 200  
    
    dtrain <- xgboost::xgb.DMatrix(
      data = as.matrix(X_train),
      label = y_train
    )
    
    xgboost::xgb.train(
      params = params,
      data = dtrain,
      nrounds = nrounds,
      verbose = 0,
      early_stopping_rounds = 20,
      evals = list(train = dtrain)
    )
    
  }, error = function(e) {
    if (verbose) {
      cli::cli_alert_warning(sprintf("Model training failed: %s", e$message))
    }
    NULL
  })
  
  if (is.null(model)) {return(station_df)}
  
  missing_data <- station_df[original_nas_idx, , drop = FALSE]
  
  X_pred <- missing_data[, temporal_features, drop = FALSE]
  
  predictions <- tryCatch({
    preds <- stats::predict(model, as.matrix(X_pred))

    if (transform_type != "none") {
      
      if (transform_type == "log") {
        preds <- exp(preds) - transform_params$shift
      } else if (transform_type == "log1p") {
        preds <- expm1(preds)
      } else if (transform_type == "sqrt") {
        preds <- preds^2
      } else if (transform_type == "boxcox") {
        if (abs(transform_params$lambda) < 1e-6) {
          preds <- exp(preds) - transform_params$shift
        } else {
          preds <- (preds * transform_params$lambda + 1)^(1/transform_params$lambda) - 
                   transform_params$shift
        }
      }
    }
    
    # # 2. Garantir nao-negatividade
    # preds <- pmax(preds, 0)
    
    # 3. Aplicar limites fisicos (se fornecidos)
    if (grepl("rh|umid|humidity", target_var, ignore.case = TRUE)) {
        preds <- pmin(pmax(preds, 0), 100)
      } else if (grepl("t.*c|temp", target_var, ignore.case = TRUE)) {
        preds <- pmin(pmax(preds, -90), 60)
      } else if (grepl("pres|patm", target_var, ignore.case = TRUE)) {
        preds <- pmin(pmax(preds, 700), 1100)
      } else if (grepl("rain|precip", target_var, ignore.case = TRUE)) {
        preds <- pmax(preds, 0)
      } else if (grepl("ws|wind", target_var, ignore.case = TRUE)) {
        preds <- pmax(preds, 0)
      }
    
    preds <- .smooth_extreme_predictions(preds, y_train_original)
    
    preds
    
  }, error = function(e) {
    if (verbose) {
      cli::cli_alert_warning(sprintf("Prediction failed: %s", e$message))
    }
    rep(mean(y_train_original, na.rm = TRUE), length(original_nas_idx))
  })
  
  station_df[[target_var]][original_nas_idx] <- predictions
  station_df$is_imputed[original_nas_idx] <- TRUE
  return(station_df)
}


#' Smooth Extreme Predictions
#'
#' @param preds Numeric vector of predictions
#' @param train_vals Training values for reference
#' @param iqr_multiplier IQR multiplier for outlier detection
#'
#' @return Smoothed predictions
#' @keywords internal
#' @noRd
.smooth_extreme_predictions <- function(preds, train_vals, iqr_multiplier = 5) {
  
  # Calcular limites baseados nos dados de treino
  q1 <- stats::quantile(train_vals, 0.25, na.rm = TRUE)
  q3 <- stats::quantile(train_vals, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  lower_bound <- q1 - iqr_multiplier * iqr
  upper_bound <- q3 + iqr_multiplier * iqr
  
  train_mean <- mean(train_vals, na.rm = TRUE)
  
  # Identificar predicoes extremas
  extreme_idx <- which(preds < lower_bound | preds > upper_bound)
  
  if (length(extreme_idx) > 0) {
    # Suavizar: misturar predicao extrema com media
    alpha <- 0.3  # Peso da media
    preds[extreme_idx] <- alpha * train_mean + (1 - alpha) * preds[extreme_idx]
  }
  
  return(preds)
}


#' Create Time-Based Features
#'
#' @description Generates cyclical and categorical time-based features from a datetime column.
#' @param data A data.frame or tibble.
#' @param datetime_col Character string with the name of the datetime column. Default is "datetime".
#' @keywords internal
#' @noRd
create_time_features <- function(data, datetime_col = "date") {
  
  # Ensure input is a tibble and handle datetime conversion
  df <- dplyr::as_tibble(data)
  df[[datetime_col]] <- lubridate::as_datetime(df[[datetime_col]])
  
  # 1. Basic Temporal Components
  df <- df %>%
    dplyr::mutate(
      hour       = as.integer(lubridate::hour(.data[[datetime_col]])),
      day        = as.integer(lubridate::day(.data[[datetime_col]])),
      month      = as.integer(lubridate::month(.data[[datetime_col]])),
      year       = as.integer(lubridate::year(.data[[datetime_col]])),
      dayofyear  = as.integer(lubridate::yday(.data[[datetime_col]])),
      quarter    = as.integer(lubridate::quarter(.data[[datetime_col]])),
      weekofyear = as.integer(lubridate::isoweek(.data[[datetime_col]])),
      # Internal helper for weekday (1=Mon, 7=Sun)
      wday_tmp   = lubridate::wday(.data[[datetime_col]], week_start = 1)
    )
  
  # 2. Boolean and Categorical Indicators
  df <- df %>%
    dplyr::mutate(
      weekday    = as.integer(wday_tmp - 1), # Base 0-6 (0=Mon, 6=Sun)
      is_weekend = dplyr::if_else(wday_tmp %in% c(6, 7), 1L, 0L),
      
      # Season mapping (0: Winter/Summer context dependent, 1: Autumn/Spring, etc)
      season = dplyr::case_when(
        month %in% c(12, 1, 2) ~ 0L,
        month %in% c(3, 4, 5)  ~ 1L,
        month %in% c(6, 7, 8)  ~ 2L,
        month %in% c(9, 10, 11) ~ 3L,
        TRUE                   ~ NA_integer_
      )
    )
  
  # 3. Cyclical Features (Sine/Cosine transformations)
  df <- df %>%
    dplyr::mutate(
      hour_sin      = sin(2 * pi * hour / 24),
      hour_cos      = cos(2 * pi * hour / 24),
      month_sin     = sin(2 * pi * month / 12),
      month_cos     = cos(2 * pi * month / 12),
      dayofyear_sin = sin(2 * pi * dayofyear / 365.25),
      dayofyear_cos = cos(2 * pi * dayofyear / 365.25),
      weekday_sin   = sin(2 * pi * weekday / 7),
      weekday_cos   = cos(2 * pi * weekday / 7),
      quarter_sin   = sin(2 * pi * quarter / 4),
      quarter_cos   = cos(2 * pi * quarter / 4),
      weekofyear_sin = sin(2 * pi * weekofyear / 52),
      weekofyear_cos = cos(2 * pi * weekofyear / 52)
    )
  
  # 4. Feature Engineering: Dummies and Buckets
  df <- df %>%
    dplyr::mutate(
      # Manual One-Hot Encoding for seasons
      season_0 = dplyr::if_else(season == 0, 1L, 0L),
      season_1 = dplyr::if_else(season == 1, 1L, 0L),
      season_2 = dplyr::if_else(season == 2, 1L, 0L),
      season_3 = dplyr::if_else(season == 3, 1L, 0L),
      
      # Day period indicators
      is_morning   = dplyr::if_else(hour >= 6 & hour < 12, 1L, 0L),
      is_afternoon = dplyr::if_else(hour >= 12 & hour < 18, 1L, 0L),
      is_evening   = dplyr::if_else(hour >= 18 & hour < 22, 1L, 0L),
      is_night     = dplyr::if_else(hour >= 22 | hour < 6, 1L, 0L),
      
      # Additional continuous features
      hour_decimal = hour + (lubridate::minute(.data[[datetime_col]]) / 60.0)
    ) %>%
    dplyr::select(-wday_tmp) # Clean up helper column
  
  return(df)
}

#' Create Lagged Features with Robust Imputation
#'
#' @description Adds lagged features and fills resulting NAs to allow immediate model training.
#' @keywords internal
#' @noRd
create_lag_features <- function(
  data,
  target_cols,
  lag_periods = c(1, 2, 3, 12, 24, 168),
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
    # created_numeric <- created_cols[sapply(created_cols, function(x) {
    #   is.numeric(result_data[[x]])
    # })]
    is_num <- vapply(created_cols, function(x) is.numeric(result_data[[x]]), FUN.VALUE = logical(1))
    created_numeric <- created_cols[is_num]

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
  stats = c("mean", "sd"),
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
    night_end = 6) 
    {
  
  if (!radiation_col %in% colnames(data)) {
    return(data)
  }
  
  # Create copy to avoid modifying original
  df <- data
  
  # Extract hour from datetime
  df <- df %>%
    dplyr::mutate(
      .hour = lubridate::hour(!!rlang::sym(datetime_col)),
      .is_night = .hour >= night_start | .hour < night_end,
      !!radiation_col := dplyr::if_else(
        .is_night & is.na(!!rlang::sym(radiation_col)),
        0,
        !!rlang::sym(radiation_col)
      )
    ) %>%
    dplyr::select(-.hour, -.is_night)
  
  return(df)
}


# ---- EVALUATION MODULE ----


#' Calculate ML Performance Metrics for Imputation Evaluation
#'
#' @description
#' Computa m\u00e9tricas de performance para avaliar imputa\u00e7\u00e3o de dados clim\u00e1ticos.
#' Inclui remo\u00e7\u00e3o opcional de outliers via Z-score e m\u00e9tricas robustas.
#'
#' @param true_values Numeric vector. Valores reais (observados).
#' @param pred_values Numeric vector. Valores preditos (imputados).
#' @param na.rm Logical. Remove NAs antes do c\u00e1lculo? (default: TRUE)
#' @param remove_outliers Logical. Remove outliers via Z-score? (default: FALSE)
#' @param z_threshold Numeric. Limiar Z-score para outlier (default: 3)
#' @param digits Integer. N\u00famero de casas decimais para exibi\u00e7\u00e3o (default: 2)
#' @param lang Character. Idioma: "pt", "en", "es" (default: "pt")
#'
#' @return Invis\u00edvel: lista com todas as m\u00e9tricas calculadas
#'
#' @examples
#' \dontrun{
#' true <- rnorm(100, mean = 25, sd = 5)
#' pred <- true + rnorm(100, mean = 0, sd = 2)
#' 
#' # B\u00e1sico
#' calc_ml_metrics(true, pred)
#' 
#' # Com remo\u00e7\u00e3o de outliers (recomendado para dados clim\u00e1ticos)
#' calc_ml_metrics(true, pred, remove_outliers = TRUE, z_threshold = 3)
#' }
#' @keywords internal
#' @noRd
calc_ml_metrics <- function(
    true_values, 
    pred_values, 
    na.rm = TRUE,
    remove_outliers = TRUE,
    z_threshold = 3,
    digits = 2,
    lang = "pt",
    verbose = verbose
) {
  
  # ============================================================================
  # VALIDA\u00c7\u00c3O INICIAL
  # ============================================================================
  
  # Verificar se s\u00e3o vetores num\u00e9ricos
  if (!is.numeric(true_values) || !is.numeric(pred_values)) {
    cli::cli_abort("true_values and pred_values must be numeric vectors")
  }
  
  # Verificar mesmo comprimento
  if (length(true_values) != length(pred_values)) {
    cli::cli_abort("true_values and pred_values must have the same length")
  }
  
  # ============================================================================
  # REMO\u00c7\u00c3O DE OUTLIERS (OPCIONAL)
  # ============================================================================
  
  if (remove_outliers) {
    # Calcular Z-scores para valores reais
    true_mean <- mean(true_values, na.rm = na.rm)
    true_sd <- sd(true_values, na.rm = na.rm)
    
    if (true_sd > 0 && !is.na(true_sd)) {
      z_scores <- abs((true_values - true_mean) / true_sd)
      outlier_idx <- which(z_scores > z_threshold)
      
      if (length(outlier_idx) > 0) {
        # Remover outliers de AMBOS os vetores
        true_values <- true_values[-outlier_idx]
        pred_values <- pred_values[-outlier_idx]
      }
    }
  }
  
  # ============================================================================
  # REMO\u00c7\u00c3O DE NAs (SE SOLICITADO)
  # ============================================================================
  
  if (na.rm) {
    valid_idx <- !is.na(true_values) & !is.na(pred_values)
    true_values <- true_values[valid_idx]
    pred_values <- pred_values[valid_idx] 
    if (length(true_values) == 0) {
      cli::cli_abort("No valid observations after NA removal")
    }
  }
  
  # ============================================================================
  # C\u00c1LCULO DAS M\u00c9TRICAS BASE
  # ============================================================================
  
  n <- length(true_values)
  errors <- true_values - pred_values
  abs_errors <- abs(errors)
  squared_errors <- errors^2
  
  # RMSE - Root Mean Squared Error
  rmse <- sqrt(mean(squared_errors))
  
  # MAE - Mean Absolute Error
  mae <- mean(abs_errors)
  
  # R\u00b2 - Coeficiente de Determina\u00e7\u00e3o
  ss_res <- sum(squared_errors)
  ss_tot <- sum((true_values - mean(true_values))^2)
  
  # R\u00b2 pode ser negativo se o modelo for pior que a m\u00e9dia
  r_squared <- 1 - (ss_res / ss_tot)
  
  # ============================================================================
  # M\u00c9TRICAS DE ERRO PERCENTUAL
  # ============================================================================
  
  # sMAPE - Symmetric Mean Absolute Percentage Error
  denominator <- abs(true_values) + abs(pred_values)
  
  # Evitar divis\u00e3o por zero
  denominator[denominator < 1e-8] <- 1e-8
  
  smape <- 200 * mean(abs_errors / denominator)
  
  # # MPE - Mean Percentage Error (vi\u00e9s percentual)
  # mpe <- 100 * mean(errors / true_values, na.rm = TRUE)
  
  # ============================================================================
  # M\u00c9TRICAS DE VI\u00c9S E CORRELA\u00c7\u00c3O
  # ============================================================================
  
  # Slope Bias (coeficiente angular da regress\u00e3o pred ~ true)
  if (stats::var(true_values) > 1e-10 && n >= 3) {
    slope_bias <- tryCatch({
      fit <- stats::lm(pred_values ~ true_values)
      unname(stats::coef(fit)[2])
    }, error = function(e) NA_real_)
  } else {
    slope_bias <- NA_real_
  }

  # # Intercept Bias (coeficiente linear)
  # if (var(true_values) > 1e-10 && n >= 3 && !is.na(slope_bias)) {
  #   intercept_bias <- tryCatch({
  #     fit <- lm(pred_values ~ true_values)
  #     unname(coef(fit)[1])
  #   }, error = function(e) NA_real_)
  # } else {
  #   intercept_bias <- NA_real_
  # }
  
  # Correla\u00e7\u00e3o de Pearson
  # correlation <- cor(true_values, pred_values, use = "complete.obs")
  
  # ============================================================================
  # M\u00c9TRICAS DE ERRO ESCALONADO
  # ============================================================================
  
  # # nRMSE - RMSE normalizado pela m\u00e9dia
  # mean_true <- mean(true_values)
  # if (abs(mean_true) > 1e-10) {
  #   nrmse_mean <- rmse / abs(mean_true)
  # } else {
  #   nrmse_mean <- NA_real_
  # }
  
  # # nRMSE - RMSE normalizado pelo desvio padr\u00e3o
  # sd_true <- sd(true_values)
  # if (sd_true > 1e-10) {
  #   nrmse_sd <- rmse / sd_true
  # } else {
  #   nrmse_sd <- NA_real_
  # }
  
  # ============================================================================
  # M\u00c9TRICAS DE VI\u00c9S ABSOLUTO
  # ============================================================================
  
  # # Mean Bias Error (vi\u00e9s m\u00e9dio)
  # mbe <- mean(errors)
  
  # # Mean Absolute Scaled Error (MASE) - simplificado
  # naive_errors <- abs(diff(true_values))
  # if (length(naive_errors) > 0 && mean(naive_errors) > 0) {
  #   mase <- mae / mean(naive_errors)
  # } else {
  #   mase <- NA_real_
  # }
  
  # ============================================================================
  # MENSAGENS INTERNACIONALIZADAS COM UNICODE ESCAPES
  # ============================================================================
  
  labels <- list(
    en = list(
      title = "ML Imputation Performance Metrics",
      subtitle = "{n} observations | {if(remove_outliers) paste('Outliers removed (|z| >', z_threshold, ')') else 'No outlier removal'}",
      rmse = "Root Mean Squared Error",
      mae  = "Mean Absolute Error",
      r2   = "R-Squared",
      smape = "sMAPE",
      mpe = "MPE",
      slope = "Slope Bias",
      intercept = "Intercept Bias",
      cor = "Pearson Correlation",
      nrmse_mean = "nRMSE (by mean)",
      nrmse_sd = "nRMSE (by SD)",
      mbe = "Mean Bias Error",
      mase = "MASE",
      rmse_desc = "Same unit as target. Lower is better.",
      mae_desc  = "Same unit as target. Lower is better.",
      r2_desc   = "\u2264 1.0. 1.0 is perfect, 0 is baseline, <0 is worse than baseline.",
      smape_desc = "0-200%. 0% is perfect. More robust than MAPE.",
      mpe_desc = "Positive = overestimation, negative = underestimation.",
      slope_desc = "Should be close to 1.0.",
      intercept_desc = "Should be close to 0.",
      cor_desc = "Strength of linear relationship (0-1).",
      nrmse_desc = "Unitless. <0.1 = excellent, <0.5 = good.",
      mbe_desc = "Positive = overestimation, negative = underestimation.",
      mase_desc = "<1 = better than naive forecast."
    ),
    pt = list(
      title = "M\u00e9tricas de Performance de Imputa\u00e7\u00e3o ML",
      subtitle = "{n} observa\u00e7\u00f5es | {if(remove_outliers) paste('Outliers removidos (|z| >', z_threshold, ')') else 'Sem remo\u00e7\u00e3o de outliers'}",
      rmse = "Raiz do Erro Quadr\u00e1tico M\u00e9dio (RMSE)",
      mae  = "Erro Absoluto M\u00e9dio (MAE)",
      r2   = "R-Quadrado",
      smape = "sMAPE",
      mpe = "EPM",
      slope = "Vi\u00e9s de Inclina\u00e7\u00e3o",
      intercept = "Vi\u00e9s de Intercepto",
      cor = "Correla\u00e7\u00e3o de Pearson",
      nrmse_mean = "nRMSE (pela m\u00e9dia)",
      nrmse_sd = "nRMSE (pelo DP)",
      mbe = "Erro M\u00e9dio de Vi\u00e9s",
      mase = "MASE",
      rmse_desc = "Mesma unidade do alvo. Menor \u00e9 melhor.",
      mae_desc  = "Mesma unidade do alvo. Menor \u00e9 melhor.",
      r2_desc   = "\u2264 1,0. 1,0 \u00e9 perfeito, 0 \u00e9 linha de base, <0 \u00e9 pior que a linha de base.",
      smape_desc = "0-200%. 0% \u00e9 perfeito. Mais robusto que MAPE.",
      mpe_desc = "Positivo = superestimativa, negativo = subestimativa.",
      slope_desc = "Deve ser pr\u00f3ximo de 1,0.",
      intercept_desc = "Deve ser pr\u00f3ximo de 0.",
      cor_desc = "For\u00e7a da rela\u00e7\u00e3o linear (0-1).",
      nrmse_desc = "Sem unidade. <0,1 = excelente, <0,5 = bom.",
      mbe_desc = "Positivo = superestimativa, negativo = subestimativa.",
      mase_desc = "<1 = melhor que previs\u00e3o ing\u00eanua."
    ),
    es = list(
      title = "M\u00e9tricas de Rendimiento de Imputaci\u00f3n ML",
      subtitle = "{n} observaciones | {if(remove_outliers) paste('Outliers removidos (|z| >', z_threshold, ')') else 'Sin remoci\u00f3n de outliers'}",
      rmse = "Ra\u00edz del Error Cuadr\u00e1tico Medio (RMSE)",
      mae  = "Error Absoluto Medio (MAE)",
      r2   = "R-Cuadrado",
      smape = "sMAPE",
      mpe = "EPM",
      slope = "Sesgo de Pendiente",
      intercept = "Sesgo de Intercepto",
      cor = "Correlaci\u00f3n de Pearson",
      nrmse_mean = "nRMSE (por media)",
      nrmse_sd = "nRMSE (por DE)",
      mbe = "Error Medio de Sesgo",
      mase = "MASE",
      rmse_desc = "Misma unidad del objetivo. Menor es mejor.",
      mae_desc  = "Misma unidad del objetivo. Menor es mejor.",
      r2_desc   = "\u2264 1,0. 1,0 es perfecto, 0 es l\u00ednea base, <0 es peor que l\u00ednea base.",
      smape_desc = "0-200%. 0% es perfecto. M\u00e1s robusto que MAPE.",
      mpe_desc = "Positivo = sobreestimaci\u00f3n, negativo = subestimaci\u00f3n.",
      slope_desc = "Debe estar cerca de 1,0.",
      intercept_desc = "Debe estar cerca de 0.",
      cor_desc = "Fuerza de la relaci\u00f3n lineal (0-1).",
      nrmse_desc = "Sin unidad. <0,1 = excelente, <0,5 = bueno.",
      mbe_desc = "Positivo = sobreestimaci\u00f3n, negativo = subestimaci\u00f3n.",
      mase_desc = "<1 = mejor que predicci\u00f3n ingenua."
    )
  )
  
  l <- if (lang %in% names(labels)) labels[[lang]] else labels[["en"]]
  
  # ============================================================================
  # EXIBI\u00c7\u00c3O DOS RESULTADOS
  # ============================================================================
  if(verbose) { 
    cli::cli_h1(l$title)
    cli::cli_text(glue::glue(l$subtitle))
    cli::cli_text("")
    
    # Grupo 1: M\u00e9tricas de Erro (mesma unidade)
    cli::cli_h3("\ud83d\udccf Error Metrics (same unit)")
    cli::cli_bullets(c(
      " " = " ",
      "\u2022" = "{.strong {l$rmse}}: {round(rmse, digits)} - {.grey {l$rmse_desc}}",
      "\u2022" = "{.strong {l$mae}}: {round(mae, digits)} - {.grey {l$mae_desc}}"
    ))
    
    # Grupo 2: M\u00e9tricas de Qualidade do Ajuste
    cli::cli_h3("\ud83d\udcca Goodness of Fit")
    cli::cli_bullets(c(
      " " = " ",
      "\u2022" = "{.strong {l$r2}}: {round(r_squared, digits)} - {.grey {l$r2_desc}}"
    ))
    
    # Grupo 3: M\u00e9tricas Percentuais
    cli::cli_h3("\ud83d\udcc8 Percentage Metrics")
    cli::cli_bullets(c(
      " " = " ",
      "\u2022" = "{.strong {l$smape}}: {round(smape, 2)}% - {.grey {l$smape_desc}}"
    ))
    
    # Grupo 4: M\u00e9tricas de Vi\u00e9s
    cli::cli_h3("\u2696\ufe0f Bias Metrics")
    cli::cli_bullets(c(
      " " = " ",
      "\u2022" = "{.strong {l$slope}}: {round(slope_bias, digits)} - {.grey {l$slope_desc}}"
    ))
  } 
  # ============================================================================
  # AVALIA\u00c7\u00c3O QUALITATIVA
  # ============================================================================
  if(verbose) {
    if (lang == "pt") {
    if (abs(r_squared) < 0.3) {
      cli::cli_alert_danger("R\u00b2 muito baixo. O modelo n\u00e3o est\u00e1 explicando a variabilidade dos dados.")
    } else if (r_squared < 0.6) {
      cli::cli_alert_warning("R\u00b2 moderado. O modelo explica parcialmente os dados.")
    } else if (r_squared > 0.9) {
      cli::cli_alert_success("R\u00b2 excelente! O modelo explica muito bem os dados.")
    }
    
    if (abs(slope_bias - 1) > 0.2 && !is.na(slope_bias)) {
      cli::cli_alert_warning("Vi\u00e9s de inclina\u00e7\u00e3o detectado. Slope = {round(slope_bias, 3)} (ideal = 1.0)")
    }
    
    if (smape < 10) {
      cli::cli_alert_success("sMAPE excelente: {round(smape, 1)}%")
    } else if (smape < 30) {
      cli::cli_alert_info("sMAPE aceit\u00e1vel: {round(smape, 1)}%")
    } else {
      cli::cli_alert_danger("sMAPE alto: {round(smape, 1)}%. Erro percentual significativo.")
    }
    
  } else if (lang == "en") {
    if (abs(r_squared) < 0.3) {
      cli::cli_alert_danger("Very low R\u00b2. Model is not explaining data variability.")
    } else if (r_squared < 0.6) {
      cli::cli_alert_warning("Moderate R\u00b2. Model partially explains the data.")
    } else if (r_squared > 0.9) {
      cli::cli_alert_success("Excellent R\u00b2! Model explains data very well.")
    }
    
    if (abs(slope_bias - 1) > 0.2 && !is.na(slope_bias)) {
      cli::cli_alert_warning("Slope bias detected. Slope = {round(slope_bias, 3)} (ideal = 1.0)")
    }
    
    if (smape < 10) {
      cli::cli_alert_success("Excellent sMAPE: {round(smape, 1)}%")
    } else if (smape < 30) {
      cli::cli_alert_info("Acceptable sMAPE: {round(smape, 1)}%")
    } else {
      cli::cli_alert_danger("High sMAPE: {round(smape, 1)}%. Significant percentage error.")
    }
  }

  }
  
  # ===========================================================================
  # RETORNO INVIS\u00cdVEL
  # ============================================================================
  
metrics_table <- dplyr::tibble(
  rmse = rmse,
  mae = mae,
  r_squared = r_squared,
  smape = smape,
  slope_bias = slope_bias,
  n_obs = n
)
  return(metrics_table)
}


#' Introduce Artificial Gaps in Time Series
#'
#' @description Generate artificial missing values for evaluation with proper mechanisms
#'
#' @param data Data frame with time series data
#' @param target_var Character. Name of target variable
#' @param gap_percentage Numeric. Proportion of data to set as missing (0-1)
#' @param gap_mechanism Character. One of "MCAR", "MAR", "MNAR"
#' @param mar_depends_on Character. Variable(s) to use for MAR (required if MAR)
#' @param seed Integer. Random seed for reproducibility
#'
#' @return List with gap_indices and data_with_gaps
#' @keywords internal
#' @noRd
.introduce_nan <- function(
    data,
    target_var,
    gap_percentage = 0.2,
    gap_mechanism = "MCAR",
    mar_depends_on = NULL,
    seed = 42
) {
  
  set.seed(seed)
  n <- nrow(data)
  n_gaps <- ceiling(n * gap_percentage)
  
  if (gap_mechanism == "MCAR") {
    # Missing Completely At Random
    gap_indices <- sample(seq_len(n), n_gaps, replace = FALSE)
    
  } else if (gap_mechanism == "MAR") {
    # Missing At Random (depends on OTHER variables)
    if (is.null(mar_depends_on)) {
      cli::cli_abort("mar_depends_on must be provided for MAR mechanism")
    }
    
    missing_vars <- setdiff(mar_depends_on, colnames(data))
    if (length(missing_vars) > 0) {
      cli::cli_abort("mar_depends_on variables not found: {paste(missing_vars, collapse = ', ')}")
    }
    
    dep_vars_scaled <- scale(data[, mar_depends_on, drop = FALSE])
    
    mar_score <- rowMeans(dep_vars_scaled, na.rm = TRUE)
    
    # Probabilidade de missing proporcional ao escore
    prob <- (mar_score - min(mar_score, na.rm = TRUE)) / 
            diff(range(mar_score, na.rm = TRUE))
    prob[is.na(prob)] <- 0.5  # Handle NAs
    
    # Amostrar com probabilidades
    gap_indices <- sample(seq_len(n), n_gaps, replace = FALSE, prob = prob)
    
  } else if (gap_mechanism == "MNAR") {
    # Missing Not At Random (depends on target value)
    target_vals <- data[[target_var]]
    
    valid_idx <- which(!is.na(target_vals))
    valid_vals <- target_vals[valid_idx]
    
    val_std <- scale(valid_vals)
    
    prob_extreme <- abs(val_std) / sum(abs(val_std), na.rm = TRUE)
    
    selected_valid <- sample(seq_along(valid_idx), 
                            min(n_gaps, length(valid_idx)), 
                            replace = FALSE, 
                            prob = prob_extreme)
    
    gap_indices <- valid_idx[selected_valid]
    
  } else {
    cli::cli_abort("gap_mechanism must be one of: 'MCAR', 'MAR', 'MNAR'")
  }
  
  gap_indices <- unique(gap_indices)
  if (length(gap_indices) > n_gaps) {
    gap_indices <- gap_indices[1:n_gaps]
  }

  data_with_gaps <- data
  data_with_gaps[[target_var]][gap_indices] <- NA
  
  return(list(
    gap_indices = gap_indices,
    data_with_gaps = data_with_gaps,
    mechanism = gap_mechanism,
    n_gaps = length(gap_indices)
  ))
}


#' Evaluate Single Station Imputation Performance
#'
#' @description Evaluate imputation performance by introducing artificial gaps
#'
#' @return List with evaluation metrics
#' @keywords internal
#' @noRd
.evaluate_single_station <- function(
    df,
    target_var,
    station_col,
    gap_percentage = 0.2,
    gap_mechanism = "MCAR",
    mar_depends_on = NULL,
    remove_outliers = TRUE,
    parallel = TRUE,
    workers = future::availableCores() - 1,
    lang = "pt",
    seed = 42,
    verbose = verbose
) {
  
  # ============================================================================
  # VALIDATION
  # ============================================================================

  original_data <- df %>%
    dplyr::filter(!is.na(.data[[target_var]])) %>%
    dplyr::arrange(.data$date, .data[[station_col]]) %>%
    dplyr::mutate(
      .row_id = dplyr::row_number(), 
      .station_id = .data[[station_col]]
    )
  
  # ============================================================================
  # INTRODUZIR GAPS ARTIFICIAIS
  # ============================================================================
  
  gap_info <- .introduce_nan(
    data = original_data,
    target_var = target_var,
    gap_percentage = gap_percentage,
    gap_mechanism = gap_mechanism,
    mar_depends_on = mar_depends_on,
    seed = 42
  )
  
  gap_indices <- gap_info$gap_indices  
  data_with_gaps <- gap_info$data_with_gaps
  n_gaps <- length(gap_indices)
  
  # ============================================================================
  # APLICAR
  # ============================================================================
  if(parallel) { 
    old_plan <- future::plan()
    future::plan(future::multisession, workers = workers)
    on.exit(future::plan(old_plan), add = TRUE)

    # Process stations in parallel
    stations_list <- data_with_gaps %>%
      dplyr::group_split(!!rlang::sym(station_col), .keep = TRUE)

    filled_list <- furrr::future_map(
      stations_list,
      function(station_df) {
        tryCatch(
          .process_single_station_xgboost2(
            station_df = station_df,
            target_var = target_var,
            station_col = station_col,
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
  } else {
    stations_list <- data_with_gaps %>%
      dplyr::group_split(!!rlang::sym(station_col), .keep = TRUE)
    
    filled_list <- list()
    for (i in seq_along(stations_list)) {
      station_id <- unique(stations_list[[i]][[station_col]]) 
      filled_list[[i]] <- .process_single_station_xgboost2(
        station_df = stations_list[[i]],
        target_var = target_var,
        station_col = station_col,
        verbose = FALSE
      )
    }  
  }
  filled_data <- dplyr::bind_rows(filled_list)

  # ============================================================================
  # ALINHAR DADOS PARA COMPARACAO
  # ============================================================================
  
  # Criar chave
  original_data <- original_data %>%
    dplyr::mutate(
      .join_key = paste(.data[[station_col]], .data$date, sep = "_")
    )
  
  filled_data <- filled_data %>%
    dplyr::mutate(
      .join_key = paste(.data[[station_col]], .data$date, sep = "_")
    )
  
  comparison_data <- original_data[gap_indices, ] %>%
    dplyr::select(.data$date, dplyr::all_of(station_col), .join_key, true_value = dplyr::all_of(target_var)) %>%
    dplyr::left_join(
      filled_data %>% dplyr::select(.join_key, pred_value = dplyr::all_of(target_var)),
      by = ".join_key"
    )

  my_stations <- unique(comparison_data %>% dplyr::select(dplyr::all_of(station_col)))
  
  # ============================================================================
  # CALCULAR METRICAS
  # ============================================================================
  metrics_list <- lapply(1:nrow(my_stations), function(i) { 
    
    current_station <- my_stations[i, 1, drop = TRUE]
    comparison_df <- comparison_data %>% 
      dplyr::filter(.data[[station_col]] == current_station)
  
    res <- calc_ml_metrics(
      true_values = comparison_df$true_value,
      pred_values = comparison_df$pred_value,
      remove_outliers = TRUE,
      lang = lang,
      verbose = FALSE)
  
    res$station_id <-  as.character(current_station)

    return(res)

  })

  metrics <- dplyr::bind_rows(metrics_list) %>%
    dplyr::relocate(station_id, .before = 1) %>%
    dplyr::mutate(
      gap_percentage = gap_percentage,
      gap_mechanism = gap_mechanism
    )
  
  db_final <- comparison_data %>%
    dplyr::select(-.join_key) |> 
    dplyr::arrange(.data$date)
  
  return(list(
    data = db_final,
    metrics = metrics
  ))
}