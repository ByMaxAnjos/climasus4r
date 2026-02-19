#' Aggregate Health Data into Time Series
#'
#' Aggregates individual-level health data into time series counts by specified
#' time units and grouping variables. This function is essential for preparing
#' data for time series analysis, DLNM models, and other temporal epidemiological methods.
#'
#' @param df A data frame containing health data (output from `sus_data_standardize()`, or `sus_data_filter*()`).
#' @param time_unit Character string specifying the temporal aggregation unit.
#'   **Standard units**: `"day"`, `"week"`, `"month"`, `"quarter"`, `"year"`
#'   **Multi-day/week/month**: `"2 days"`, `"5 days"` (pentads), `"14 days"` (fortnightly),
#'    `"3 months"` (trimester), `"6 months"` (semester).
#'   **Special**: `"season"` (Brazilian seasons: DJF, MAM, JJA, SON).
#'   Default is `"day"`.
#' @param fun Character string or list of functions specifying the aggregation function(s).
#'   Options: `"count"` (default), `"sum"`, `"mean"`, `"median"`, `"min"`, `"max"`,
#'   `"sd"`, `"q25"` (25th percentile), `"q75"`, `"q95"`, and `"q99"`.
#'   Can also be a named list for multiple aggregations, e.g.,
#'   `list(mean_temp = "mean", max_temp = "max")`.
#' @param value_col Character string with the name of the column to aggregate when
#'   using functions other than `"count"`. Required for `"sum"`, `"mean"`, etc.
#'   For example, `"temperature"`, `"precipitation"`, `"pm25"`.
#' @param date_col Character string with the name of the date column to use for
#'   aggregation. If `NULL` (default), the function will attempt to auto-detect
#'   the date column based on common patterns.
#' @param group_by Character vector with names of columns to group by (e.g.,
#'   `c("sex", "age_group", "municipality_code", "race")`). If `NULL` (default), aggregates
#'   across all records.
#' @param complete_dates Logical. If `TRUE` (default), fills in missing time periods
#'   with zero counts to create a complete time series without gaps.
#' @param lang Character string specifying the language for messages. Options:
#'   `"en"` (English, default), `"pt"` (Portuguese, default), `"es"` (Spanish).
#' @param verbose Logical. If `TRUE` (default), prints progress messages.
#'
#' @return A tibble with aggregated data containing:
#'   \itemize{
#'     \item `date`: The aggregated date (start of period)
#'     \item Grouping columns (if `group_by` was specified)
#'     \item Aggregated value column(s) with smart names based on system and function
#'   }
#'
#' @details
#' **New Features**:
#' \itemize{
#'   \item **Multiple aggregation functions**: Beyond counting, you can now calculate
#'     mean, sum, median, percentiles, etc., useful for climate and environmental data.
#'   \item **Smart column naming**: The aggregated column is automatically named
#'     based on the health system (e.g., `n_deaths` for SIM-DO, `n_hospitalizations`
#'     for SIH-RD, `n_births` for SINASC).
#' }
#' **Epidemiological Use Cases**:
#' \itemize{
#'   \item **Daily/Weekly**: Standard time series analysis, DLNM for short-term effects
#'   \item **Pentads (5 days)**: Heat wave analysis, smoothing daily noise
#'   \item **Fortnightly (14 days)**: Diseases with longer incubation periods
#'   \item **Monthly**: Seasonal patterns, long-term trends
#'   \item **Quarterly**: SUS management reports, policy evaluation
#'   \item **Seasonal**: Dengue, Influenza, respiratory diseases aligned with Brazilian climate
#'   \item **Yearly**: Long-term trend analysis, climate change impacts
#' }
#'
#' **Brazilian Seasons** (when `time_unit = "season"`):
#' \itemize{
#'   \item **Summer (Verao)**: December-January-February (DJF)
#'   \item **Autumn (Outono)**: March-April-May (MAM)
#'   \item **Winter (Inverno)**: June-July-August (JJA)
#'   \item **Spring (Primavera)**: September-October-November (SON)
#' }
#'
#' @examples
#' \dontrun{
#' library(climasus4r)
#'
#' # Basic daily aggregation
#' df_daily <- sus_data_import(uf = "SP", year = 2023, system = "SIM-DO") |>
#'   sus_data_standardize() |>
#'   sus_data_filter_cid(disease_group = "respiratory") |>
#'   sus_data_aggregate(time_unit = "day")
#'
#' # Pentad aggregation (5-day periods) for heat wave analysis
#' df_pentad <- sus_data_aggregate(df, time_unit = "5 days")
#'
#' # Fortnightly aggregation for diseases with longer incubation
#' df_fortnightly <- sus_data_aggregate(df, time_unit = "14 days")
#'
#' # Monthly aggregation by municipality
#' df_monthly <- sus_data_aggregate(
#'   df,
#'   time_unit = "month",
#'   group_by = c("race", "sex"),
#'   lang = "pt"
#' )
#'
#' # Quarterly aggregation for SUS reports
#' df_quarterly <- sus_data_aggregate(df, time_unit = "quarter")
#'
#' # Seasonal aggregation for dengue analysis (Brazilian seasons)
#' df_seasonal <- sus_data_aggregate(
#'   df,
#'   time_unit = "season",
#'   group_by = "state"
#' )
#'
#' # Weekly aggregation by age group and sex
#' df_weekly <- sus_data_aggregate(
#'   df,
#'   time_unit = "week",
#'   group_by = c("age_group", "sex")
#' )
#' }
#' @importFrom rlang .data
#'  
#' @export
sus_data_aggregate <- function(df,
                               time_unit = "day",
                               fun = "count",
                               value_col = NULL,
                               group_by = NULL,
                               complete_dates = TRUE,
                               date_col = NULL,
                               lang = "pt",
                               verbose = TRUE) {
  
  # Validate inputs
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }
  
  if (nrow(df) == 0) {
    stop("df is empty (0 rows)")
  }
  
  if (!lang %in% c("en", "pt", "es")) {
    stop("lang must be one of: 'en', 'pt', 'es'")
  }
  
  # Validate fun parameter
  valid_funs <- c("count", "sum", "mean", "median", "min", "max", "sd", 
                  "q25", "q75", "q95", "q99")
  
  if (is.character(fun)) {
    if (!fun %in% valid_funs) {
      cli::cli_abort(paste0("fun must be one of: ", paste(valid_funs, collapse = ", ")))
    }
    if (fun != "count" && is.null(value_col)) {
      cli::cli_abort("value_col must be specified when fun is not 'count'")
    }
  } else if (is.list(fun)) {
    if (!all(unlist(fun) %in% valid_funs)) {
      cli::cli_abort(paste0("All functions in list must be one of: ", paste(valid_funs, collapse = ", ")))
    }
    if (is.null(value_col)) {
      cli::cli_abort("value_col must be specified when using multiple aggregation functions")
    }
  } else {
    cli::cli_abort("fun must be a character string or a named list")
  }

   # Check if data is climasus_df
  if (inherits(df, "climasus_df")) {
    
    current_stage <- climasus_meta(df, "stage")
    required_stage <- "stand"

    if (!is_stage_at_least(current_stage, required_stage)) {

      msg_error <- list(
        en = paste0(
          "Data must be filtered before aggregation.\n",
          "Current stage: ", current_stage %||% "unknown", "\n",
          "Required stage: ", required_stage, "\n\n",
          "Please run:\n",
          "  df <- sus_data_standardize(df)"
        ),
        pt = paste0(
          "Dados devem ser filtrados antes da agregacao.\n",
          "Estagio atual: ", current_stage %||% "desconhecido", "\n",
          "Estagio requerido: ", required_stage, "\n\n",
          "Por favor, execute:\n",
          "  df <- sus_data_standardize(df)"
        ),
        es = paste0(
          "Los datos deben ser filtrados antes de la agregacion.\n",
          "Etapa actual: ", current_stage %||% "desconocida", "\n",
          "Etapa requerida: ", required_stage, "\n\n",
          "Por favor, ejecute:\n",
          "  df <- sus_data_standardize(df)"
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
    df <- climasus_meta(df, stage = "aggregate", type = "agg")

  } else {
      
      # NOT climasus_df - ABORT execution
      msg_error <- list(
      en = paste0(
        "Input is not a climasus_df object.\n",
        "This function requires data from the CLIMASUS4r pipeline.\n\n",
        "Please prepare your data first:\n",
        "  1. Import: df <- sus_data_import(...) or sus_data_read(...)\n",
        "  2. Clean: df <- sus_data_clean_encoding(df)\n",
        "  3. Standardize: df <- sus_data_standardize(df)\n",
        "  4. Aggregate: df <- sus_data_aggregate(df, ...)\n\n",
        "If using external data, run sus_data_standardize() first to prepare it."
      ),
      pt = paste0(
        "Entrada nao e um objeto climasus_df.\n",
        "Esta funcao requer dados do pipeline CLIMASUS4r.\n\n",
        "Por favor, prepare seus dados primeiro:\n",
        "  1. Importar: df <- sus_data_import(...) ou sus_data_read(...)\n",
        "  2. Limpar: df <- sus_data_clean_encoding(df)\n",
        "  3. Padronizar: df <- sus_data_aggregate(df)\n",
        "  4. Agregar: df <- sus_data_aggregate(df, ...)\n\n",
        "Se usar dados externos, execute sus_data_standardize() primeiro para prepara-los."
      ),
      es = paste0(
        "La entrada no es un objeto climasus_df.\n",
        "Esta funcion requiere datos del pipeline CLIMASUS4r.\n\n",
        "Por favor, prepare sus datos primero:\n",
        "  1. Importar: df <- sus_data_import(...) o sus_data_read(...)\n",
        "  2. Limpiar: df <- sus_data_clean_encoding(df)\n",
        "  3. Estandarizar: df <- sus_data_standardize(df)\n",
        "  4. Agregar: df <- sus_data_aggregate(df, ...)\n\n",
        "Si usa datos externos, ejecute sus_data_standardize() primero para prepararlos."
      )
    )
    
      
      cli::cli_abort(msg_error[[lang]])
  }
  
  #Detect system if not specified
  system <- climasus_meta(df, "system")

  # Detect duplicate column names
  data.table::setDT(df)
  cols_to_keep <- !data.table::duplicated(names(df))
  if (any(!cols_to_keep)) {df <- df[, cols_to_keep, with = FALSE]} 
  
  # Auto-detect date column if not specified
  if (is.null(date_col)) {
    date_col <- detect_date_column(df, system)
  
    if (verbose) {
      msg <- switch(lang,
        "en" = paste0("Auto-detected date column: ", date_col),
        "pt" = paste0("Coluna de data auto-detectada: ", date_col),
        "es" = paste0("Columna de fecha auto-detectada: ", date_col)
      )
      cli::cli_alert_info(msg)
    }
  }
  
  # Validate date column exists
  if (!date_col %in% names(df)) {
    stop(paste0("Date column '", date_col, "' not found in data frame"))
  }
  
  # Convert date column to Date if not already
  if (!inherits(df[[date_col]], "Date")) {
    if (verbose) {
      msg <- switch(lang,
        "en" = "Converting date column to Date format...",
        "pt" = "Convertendo coluna de data para formato Date...",
        "es" = "Convirtiendo columna de fecha a formato Date..."
      )
      cli::cli_alert_info(msg)
    }
    df[[date_col]] <- as.Date(df[[date_col]])
  }
  
  # Remove rows with missing dates
  n_missing <- sum(is.na(df[[date_col]]))

  if (n_missing > 0) {
    if (verbose) {
      msg <- switch(lang,
        "en" = paste0("Removing ", n_missing, " rows with missing dates"),
        "pt" = paste0("Removendo ", n_missing, " linhas com datas faltantes"),
        "es" = paste0("Eliminando ", n_missing, " filas con fechas faltantes")
      )
      cli::cli_alert_warning(msg)
    }
    df <- df[!is.na(df[[date_col]]), ]
  }
  
  # Create aggregation period column
  if (time_unit == "season") {
    # Custom Brazilian Season Logic
    df$agg_date <- get_brazilian_season_start(df[[date_col]])
    time_unit_label <- switch(lang,
      "en" = "seasonal",
      "pt" = "sazonais",
      "es" = "estacionales"
    )
  } else {
    # Flexible Lubridate Logic (covers "2 days", "week", "month", "quarter", etc.)
    tryCatch({
      df$agg_date <- lubridate::floor_date(df[[date_col]], unit = time_unit)
      
      # Generate appropriate label for time_unit
      time_unit_label <- get_time_unit_label(time_unit, lang)
      
    }, error = function(e) {
      cli::cli_abort(paste0(
        "Invalid time_unit: '", time_unit, "'. ",
        "Use formats like 'day', 'week', 'month', 'quarter', 'year', 'season', ",
        "or multi-period like '2 days', '5 days', '14 days', '3 months', etc. The '2 weeks' period does not work in this version "
      ))
    })
  }
  

  # Prepare grouping variables
  if (is.null(group_by)) {
    group_vars <- "agg_date"
  } else {
    # Validate group_by columns exist
    missing_cols <- setdiff(group_by, names(df))
    if (length(missing_cols) > 0) {
      cli::cli_abort(paste0("Grouping columns not found: ", paste(missing_cols, collapse = ", ")))
    }
    group_vars <- c("agg_date", group_by)
  }
  
  # Aggregate counts
  if (verbose) {
    msg <- switch(lang,
      "en" = paste0("Aggregating to ", time_unit_label, " counts..."),
      "pt" = paste0("Agregando para contagens ", time_unit_label, "..."),
      "es" = paste0("Agregando a conteos ", time_unit_label, "...")
    )
    cli::cli_alert_info(msg)
  }
  
  # Store original row count
  n_original <- nrow(df)
  
  #Get geographical columns

 # Get geographical columns (auto-detect municipality codes)
  get_geo_col <- c(
    "residence_municipality_code", "municipality_code", "residence_municipality",
    "codigo_municipio_nascimento", "codigo_municipio_ocurrencia", "codigo_municipio",
    "codigo_municipio_paciente", "uf_municipio_estabelecimento", "facility_uf_municipality",
    "patient_municipality_code", "uf_municipio_establecimiento",
    "cep_paciente", "codigo_postal_paciente", "codigo_postal", "patient_zip_code", "zip_code",
    "codigo_municipio_residencia", "CODMUNRES"
  )
  geo_col <- names(df)[grepl(paste(get_geo_col, collapse = "|"), names(df), ignore.case = TRUE)]
  
  # Add geo columns to group_vars if not already included
  if (length(geo_col) > 0) {
    group_vars <- unique(c(group_vars, geo_col))
  }
  # Perform aggregation based on fun type
  if (is.character(fun) && fun == "count") {
    # Simple count aggregation
    df_agg <- df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarise(agg_value = dplyr::n(), .groups = "drop")
    
    # Intelligent column naming for count
    agg_col_name <- get_smart_column_name(system, "count", lang)
    names(df_agg)[names(df_agg) == "agg_value"] <- agg_col_name
    
  } else if (is.character(fun)) {
    # Single function aggregation (mean, sum, etc.)
    df_agg <- df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarise(
        agg_value = apply_aggregation_function(fun, .data[[value_col]]),
        .groups = "drop"
      )
    
    # Intelligent column naming
    agg_col_name <- paste0(fun, "_", value_col)
    names(df_agg)[names(df_agg) == "agg_value"] <- agg_col_name
    
  } else if (is.list(fun)) {
    # Multiple function aggregation
    agg_formulas <- lapply(names(fun), function(name) {
      fun_type <- fun[[name]]
      rlang::quo(apply_aggregation_function(!!fun_type, !!rlang::sym(value_col)))
    })
    names(agg_formulas) <- names(fun)
    
    df_agg <- df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
      dplyr::summarise(!!!agg_formulas, .groups = "drop")
  }
  
  # Rename agg_date to date
  df_agg <- df_agg %>%
    dplyr::rename(date = .data$agg_date)
  
  # Complete dates if requested
  if (complete_dates) {
    fill_value <- if (is.character(fun) && fun == "count") 0 else NA
    df_agg <- complete_time_series(df_agg, time_unit, group_by, fill_value, lang, verbose)
  }
  
  # Sort by date and grouping variables
  sort_vars <- c("date", group_by)
  sort_vars <- sort_vars[sort_vars %in% names(df_agg)]
  df_agg <- df_agg %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(sort_vars)))
  
  # Print summary
  if (verbose) {
    n_periods <- length(unique(df_agg$date))
    n_groups <- if (is.null(group_by)) 1 else length(unique(interaction(df_agg[group_by])))
    
    msg <- switch(lang,
      "en" = paste0("Aggregated ", format(n_original, big.mark = ","), " records into ",
                    format(nrow(df_agg), big.mark = ","), " ", time_unit_label, " periods"),
      "pt" = paste0("Agregados ", format(n_original, big.mark = ","), " registros em ",
                    format(nrow(df_agg), big.mark = ","), " periodos ", time_unit_label),
      "es" = paste0("Agregados ", format(n_original, big.mark = ","), " registros en ",
                    format(nrow(df_agg), big.mark = ","), " periodos ", time_unit_label)
    )
    cli::cli_alert_success(msg)
    
    # Print date range
    date_range_msg <- switch(lang,
      "en" = paste0("Date range: ", min(df_agg$date), " to ", max(df_agg$date)),
      "pt" = paste0("Intervalo de datas: ", min(df_agg$date), " a ", max(df_agg$date)),
      "es" = paste0("Rango de fechas: ", min(df_agg$date), " a ", max(df_agg$date))
    )
    cli::cli_alert_info(date_range_msg)
    
    # Print number of time periods and groups
    if (!is.null(group_by)) {
      group_msg <- switch(lang,
        "en" = paste0(n_periods, " time periods x ", n_groups, " groups"),
        "pt" = paste0(n_periods, " periodos temporais x ", n_groups, " grupos"),
        "es" = paste0(n_periods, " periodos temporales x ", n_groups, " grupos")
      )
      cli::cli_alert_info(group_msg)
    }
  }

  if (!inherits(df_agg, "climasus_df")) {
    # Create new climasus_df with metadata from original df
    original_system <- climasus_meta(df, "system")
    
    meta <- list(
      system = original_system,
      stage = "aggregate",
      type = "agg",
      spatial = inherits(df_agg, "sf"),
      temporal = list(
        start = min(df_agg$date, na.rm = TRUE),
        end = max(df_agg$date, na.rm = TRUE),
        resolution = time_unit
      ),
      created = Sys.time(),
      modified = Sys.time(),
      history = climasus_meta(df, "history") %||% character(0),  # Preserve history
      user = list()
    )
    
    base_classes <- setdiff(class(df_agg), "climasus_df")
    df_agg <- structure(
      df_agg,
      climasus_meta = meta,
      class = c("climasus_df", base_classes)
    )
  }

  # Update stage and type
  df_agg <- climasus_meta(
    df_agg,
    system = climasus_meta(df_agg, "system"),  # Preserve original system
    stage = "aggregate",
    type = "agg",
    temporal = list(
      start = min(df_agg$date),
      end = max(df_agg$date),
      resolution = time_unit
    )
  )
  # Build detailed aggregation history message
  agg_details <- c()

  # Time unit
  if (!is.null(time_unit)) {
    agg_details <- c( agg_details, sprintf("Time unit: %s", time_unit))
  }

  # Aggregation function
  if (!is.null(fun)) {
    if (is.character(fun)) {
      agg_details <- c(
        agg_details,
        sprintf("Aggregation: %s", fun)
      )
    } else {
      agg_details <- c(
        agg_details,
        "Aggregation: custom function"
      )
    }
  }

  # Value column
  if (!is.null(value_col)) {
    agg_details <- c(
      agg_details,
      sprintf("Value column: %s", value_col)
    )
  } else {
    agg_details <- c(
      agg_details,
      "Value column: record count"
    )
  }

  # Grouping variables
  if (!is.null(group_by)) {
    if (length(group_by) <= 3) {
      agg_details <- c(
        agg_details,
        sprintf(
          "Grouped by: %s",
          paste(group_by, collapse = ", ")
        )
      )
    } else {
      agg_details <- c(
        agg_details,
        sprintf(
          "Grouped by: %s and %d more",
          paste(utils::head(group_by, 3), collapse = ", "),
          length(group_by) - 3
        )
      )
    }
  }

  # Date completion
  if (isTRUE(complete_dates)) {
    agg_details <- c(
      agg_details,
      "Completed missing dates"
    )
  }

  # Date column used
  if (!is.null(date_col)) {
    agg_details <- c(
      agg_details,
      sprintf("Date column: %s", date_col)
    )
  }

  # Create history message
  history_msg <- sprintf("Data aggregated [%s]", paste(agg_details, collapse = " | "))

  # Register metadata
  df_agg <- climasus_meta(df_agg, add_history = history_msg)

  return(df_agg)
}


# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Get Intelligent Column Name for Aggregated Data
#' 
#' Returns an appropriate column name based on the health system and aggregation type.
#' 
#' @param system Character string with system name
#' @param fun_type Character string with function type
#' @param lang Language code
#' @return Character string with column name
#' @keywords internal
#' @noRd
get_smart_column_name <- function(system, fun_type, lang) {
    
  # Generate intelligent name based on system
  if (fun_type == "count") {
    if (is.null(system)) {
      return("n")  # Generic fallback
    }
    
    # Language-specific naming
    if (lang == "en") {
      col_names <- list(
        "SIM" = "n_deaths",
        "SIH" = "n_hospitalizations",
        "SINASC" = "n_births",
        "SINAN" = "n_cases",
        "SIA" = "n_procedures",
        "CNES" = "n_establishments"
      )
    } else if (lang == "pt") {
      col_names <- list(
        "SIM" = "n_obitos",
        "SIH" = "n_internacoes",
        "SINASC" = "n_nascimentos",
        "SINAN" = "n_casos",
        "SIA" = "n_procedimentos",
        "CNES" = "n_estabelecimentos"
      )
    } else if (lang == "es") {
      col_names <- list(
        "SIM" = "n_muertes",
        "SIH" = "n_hospitalizaciones",
        "SINASC" = "n_nacimientos",
        "SINAN" = "n_casos",
        "SIA" = "n_procedimientos",
        "CNES" = "n_establecimientos"
      )
    }
    
    return(col_names[[system]])
  }
  
  # For other functions, return generic name
  return(paste0(fun_type, "_value"))
}

#' Apply Aggregation Function
#' 
#' Applies the specified aggregation function to a vector of values.
#' 
#' @param fun_type Character string with function type
#' @param values Numeric vector to aggregate
#' @return Aggregated value
#' @keywords internal
#' @noRd
apply_aggregation_function <- function(fun_type, values) {
  switch(fun_type,
    "sum" = sum(values, na.rm = TRUE),
    "mean" = mean(values, na.rm = TRUE),
    "median" = median(values, na.rm = TRUE),
    "min" = min(values, na.rm = TRUE),
    "max" = max(values, na.rm = TRUE),
    "sd" = sd(values, na.rm = TRUE),
    "q25" = quantile(values, 0.25, na.rm = TRUE),
    "q75" = quantile(values, 0.75, na.rm = TRUE),
    "q95" = quantile(values, 0.95, na.rm = TRUE),
    "q99" = quantile(values, 0.99, na.rm = TRUE),
    stop(paste0("Unknown function type: ", fun_type))
  )
}

#' Detect Date Column
#' 
#' Automatically detects the date column in a data frame based on common patterns
#' and column types.
#' 
#' @param df A data frame
#' @param system Character string indicating the SUS system.
#'   One of: "SIM", "SIH", "SINAN", "CNES", "SINASC"
#' @return Character string with the name of the detected date column
#' @keywords internal
#' @noRd
detect_date_column <- function(df, system) {
  # Common date column patterns (in order of priority)
 system_date_map <- list(

    SIM = c(
      "data_obito", "death_date", "fecha_muerte",
      "DTOBITO"
    ),

    SINASC = c(
      "data_nascimento", "birth_date", "fecha_nacimiento",
      "DTNASC"
    ),

    SIH = c(
      "data_internacao", "admission_date", "fecha_ingreso",
      "DT_INTER"
    ),

    SINAN = c(
      "data_notificacao", "notification_date", "fecha_notificacion",
      "DT_NOTIFIC"
    ),

    CNES = c(
      "data_atualizacao", "update_date", "fecha_actualizacion",
      "DT_COMPET"
    )
  )

  date_patterns <- system_date_map[[system]]
  
  # Find first matching column
  for (pattern in date_patterns) {
    if (pattern %in% names(df)) {
      return(pattern)
    }
  }
  
  # If no match, look for Date class columns
  date_cols <- names(df)[sapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXct"))]
  if (length(date_cols) > 0) {
    return(date_cols[1])
  }
  
  # If still no match, error
  cli::cli_abort("Could not auto-detect date column. Please specify 'date_col' parameter.")
}


#' Get Brazilian Season Start Date
#' 
#' Returns the first day of the Brazilian season for a given date.
#' Seasons are defined as:
#' - Summer (Verao): Dec-Jan-Feb (starts Dec 1st)
#' - Autumn (Outono): Mar-Apr-May (starts Mar 1st)
#' - Winter (Inverno): Jun-Jul-Aug (starts Jun 1st)
#' - Spring (Primavera): Sep-Oct-Nov (starts Sep 1st)
#' 
#' @param dates A vector of Date objects
#' @return A vector of Date objects representing the start of each season
#' @keywords internal
#' @noRd
#' 
#' @examples
#' \dontrun{
#' dates <- as.Date(c("2023-01-15", "2023-06-20", "2023-12-25"))
#' get_brazilian_season_start(dates)
#' # Returns: "2022-12-01" "2023-06-01" "2023-12-01"
#' }
get_brazilian_season_start <- function(dates) {
  months <- as.numeric(format(dates, "%m"))
  years <- as.numeric(format(dates, "%Y"))
  
  # Adjust years for Jan/Feb (they belong to previous year's summer start)
  adj_years <- years
  adj_years[months %in% c(1, 2)] <- years[months %in% c(1, 2)] - 1
  
  # Map months to season start month
  start_months <- character(length(dates))
  start_months[months %in% c(12, 1, 2)] <- "12"   # Summer starts Dec 1st
  start_months[months %in% c(3, 4, 5)]   <- "03"  # Autumn starts Mar 1st
  start_months[months %in% c(6, 7, 8)]   <- "06"  # Winter starts Jun 1st
  start_months[months %in% c(9, 10, 11)] <- "09"  # Spring starts Sep 1st
  
  # Create dates
  date_strings <- paste0(adj_years, "-", start_months, "-01")
  return(as.Date(date_strings))
}


#' Get Time Unit Label for Messages
#' 
#' Generates appropriate multilingual labels for different time units.
#' 
#' @param time_unit Character string with the time unit
#' @param lang Language code ("en", "pt", "es")
#' @return Character string with the appropriate label
#' @keywords internal
#' @noRd
get_time_unit_label <- function(time_unit, lang) {
  # Normalize time_unit to lowercase for matching
  tu_lower <- tolower(time_unit)
  
  # Check for standard units
  if (grepl("^day$|^1 day$", tu_lower)) {
    return(switch(lang, "en" = "daily", "pt" = "diarias", "es" = "diarios"))
  }
  
  if (grepl("^week$|^1 week$", tu_lower)) {
    return(switch(lang, "en" = "weekly", "pt" = "semanais", "es" = "semanales"))
  }
  
  if (grepl("^month$|^1 month$", tu_lower)) {
    return(switch(lang, "en" = "monthly", "pt" = "mensais", "es" = "mensuales"))
  }
  
  if (grepl("^quarter$|^3 month", tu_lower)) {
    return(switch(lang, "en" = "quarterly", "pt" = "trimestrais", "es" = "trimestrales"))
  }
  
  if (grepl("^year$|^1 year$", tu_lower)) {
    return(switch(lang, "en" = "yearly", "pt" = "anuais", "es" = "anuales"))
  }
  
  # Check for multi-day periods
  if (grepl("^[0-9]+ day", tu_lower)) {
    n_days <- as.numeric(gsub(" day.*", "", tu_lower))
    if (n_days == 5) {
      return(switch(lang, "en" = "pentad (5-day)", "pt" = "pentadais (5 dias)", "es" = "pentadales (5 dias)"))
    }
    return(switch(lang,
      "en" = paste0(n_days, "-day"),
      "pt" = paste0("de ", n_days, " dias"),
      "es" = paste0("de ", n_days, " dias")
    ))
  }
  
  # Check for multi-week periods
  if (grepl("^[0-9]+ week", tu_lower)) {
    n_weeks <- as.numeric(gsub(" week.*", "", tu_lower))
    if (n_weeks == 2) {
      return(switch(lang, "en" = "fortnightly (2-week)", "pt" = "quinzenais (2 semanas)", "es" = "quincenales (2 semanas)"))
    }
    return(switch(lang,
      "en" = paste0(n_weeks, "-week"),
      "pt" = paste0("de ", n_weeks, " semanas"),
      "es" = paste0("de ", n_weeks, " semanas")
    ))
  }
  
  # Check for multi-month periods
  if (grepl("^[0-9]+ month", tu_lower)) {
    n_months <- as.numeric(gsub(" month.*", "", tu_lower))
    if (n_months == 6) {
      return(switch(lang, "en" = "semester (6-month)", "pt" = "semestrais (6 meses)", "es" = "semestrales (6 meses)"))
    }
    return(switch(lang,
      "en" = paste0(n_months, "-month"),
      "pt" = paste0("de ", n_months, " meses"),
      "es" = paste0("de ", n_months, " meses")
    ))
  }
  
  # Default: return as-is
  return(time_unit)
}


#' Complete Time Series with Missing Periods
#' 
#' Fills in missing time periods with specified fill values to create a complete time series.
#' Handles different time units including multi-period aggregations.
#' 
#' @param df_agg Aggregated data frame
#' @param time_unit Time unit used for aggregation
#' @param group_by Grouping variables
#' @param fill_value Value to use for missing periods (0 for counts, NA for others)
#' @param lang Language for messages
#' @param verbose Whether to print messages
#' @return Data frame with complete time series
#' @keywords internal
#' @noRd
complete_time_series <- function(df_agg, time_unit, group_by, fill_value = 0, lang, verbose) {
  
  if (verbose) {
    msg <- switch(lang,
      "en" = "Filling missing time periods...",
      "pt" = "Preenchendo periodos faltantes...",
      "es" = "Rellenando periodos faltantes..."
    )
    cli::cli_alert_info(msg)
  }
  
  # Determine the appropriate sequence increment based on time_unit
  min_date <- min(df_agg$date)
  max_date <- max(df_agg$date)
  
  # Generate complete date sequence based on time_unit
  if (time_unit == "season") {
    # For seasons, we need to generate all season starts in the range
    # Get all unique dates that already exist
    date_seq <- unique(df_agg$date)
    date_seq <- sort(date_seq)
    
  } else {
    # For other time units, use lubridate to generate sequence
    # Start from min_date and generate sequence
    date_seq <- seq(min_date, max_date, by = "day")
    
    # Floor all dates to the appropriate time_unit
    tryCatch({
      date_seq <- unique(lubridate::floor_date(date_seq, unit = time_unit))
    }, error = function(e) {
      # If floor_date fails, just use existing dates
      date_seq <- unique(df_agg$date)
    })
  }
  
  if (is.null(group_by)) {
    # No grouping: simple completion
    complete_df <- data.frame(date = date_seq)
    df_agg <- dplyr::left_join(complete_df, df_agg, by = "date")
  } else {
    # With grouping: complete for each group
    all_groups <- df_agg |>
      dplyr::select(dplyr::all_of(group_by)) |>
      dplyr::distinct()
    
    complete_df <- tidyr::expand_grid(
      date = date_seq,
      all_groups
    )
    
    df_agg <- dplyr::left_join(complete_df, df_agg, 
                               by = c("date", group_by))
  }
  
  # Replace NA values with fill_value
  value_cols <- setdiff(names(df_agg), c("date", group_by))
  for (col in value_cols) {
    df_agg[[col]][is.na(df_agg[[col]])] <- fill_value
  }
  
  # Report how many periods were filled
  if (verbose) {
    n_filled <- sum(df_agg[[value_cols[1]]] == fill_value)
    if (n_filled > 0) {
      msg <- switch(lang,
        "en" = paste0("Filled ", n_filled, " missing periods"),
        "pt" = paste0("Preenchidos ", n_filled, " periodos faltantes"),
        "es" = paste0("Rellenados ", n_filled, " periodos faltantes")
      )
      cli::cli_alert_info(msg)
    }
  }
  
  return(df_agg)
}
