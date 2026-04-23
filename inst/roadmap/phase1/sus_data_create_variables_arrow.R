#' Create Derived Variables for Epidemiological Analysis (Arrow/DuckDB Compatible)
#'
#' Versão otimizada para pipelines lazy com Arrow e DuckDB. Mantém as operações
#' no formato lazy sempre que possível, coletando apenas quando necessário para
#' operações complexas (como decodificação de idade DATASUS).
#'
#' @inheritParams sus_data_create_variables
#' @param lazy Logical. If `TRUE`, returns a lazy Arrow Dataset or DuckDB connection.
#'   If `FALSE`, materializes as tibble. Default `TRUE`.
#' @param duckdb_conn Optional DuckDB connection for advanced SQL operations.
#'
#' @return If `lazy = TRUE`, returns an Arrow Dataset or DuckDB connection with
#'   derived variables. If `lazy = FALSE`, returns a `climasus_df` tibble.
#'
#' @export
sus_data_create_variables_arrow <- function(
    df,
    create_age_groups = TRUE,
    age_breaks = c(0, 5, 15, 60, Inf),
    age_labels = NULL,
    create_calendar_vars = TRUE,
    create_climate_vars = TRUE,
    climate_region = NULL,
    date_col = NULL,
    age_col = NULL,
    hemisphere = "south",
    lang = "pt",
    verbose = TRUE,
    lazy = TRUE,
    duckdb_conn = NULL
) {
  
  cli::cli_h1("climasus4r - Create Derived Variables (Arrow/DuckDB)")
  
  # ========================================================================
  # VALIDAÇÕES INICIAIS
  # ========================================================================
  
  is_arrow_dataset <- inherits(df, "ArrowObject") || 
                      inherits(df, "FileSystemDataset") ||
                      inherits(df, "arrow_dplyr_query")
  is_duckdb_conn <- inherits(df, "duckdb_connection") ||
                    inherits(df, "tbl_duckdb_connection")
  
  if (!lang %in% c("en", "pt", "es")) {
    stop("lang must be one of: 'en', 'pt', 'es'")
  }
  
  if (!hemisphere %in% c("south", "north")) {
    stop("hemisphere must be 'south' or 'north'")
  }
  
  created_vars <- character(0)
  
  # Extract system from metadata if available
  system <- NULL
  if (inherits(df, "climasus_df")) {
    system <- sus_meta(df, "system")
  }
  
  # Get column names
  if (is_arrow_dataset || is_duckdb_conn) {
    col_names <- names(df)
  } else {
    col_names <- names(df)
  }
  
  # ========================================================================
  # AGE COLUMN IDENTIFICATION (LAZY COMPATIBLE)
  # ========================================================================
  
  age_col_to_use <- age_col
  
  # Check if user-specified age column exists
  if (!is.null(age_col)) {
    if (age_col %in% col_names) {
      if (verbose) {
        msg <- switch(lang,
          "en" = paste0("Using existing age column: ", age_col),
          "pt" = paste0("Usando coluna de idade existente: ", age_col),
          "es" = paste0("Usando columna de edad existente: ", age_col)
        )
        cli::cli_alert_info(msg)
      }
    } else {
      age_col_to_use <- NULL
    }
  }
  
  # For lazy data, build age calculation expressions
  if ((is_arrow_dataset || is_duckdb_conn) && is.null(age_col_to_use)) {
    
    if (verbose) {
      cli::cli_alert_info("Attempting lazy age calculation from dates...")
    }
    
    # Detect birth date column (padronizada)
    birth_col <- NULL
    birth_patterns <- c("data_nascimento", "birth_date", "fecha_nacimiento", "DTNASC")
    for (pattern in birth_patterns) {
      if (pattern %in% col_names) {
        birth_col <- pattern
        break
      }
    }
    
    # Detect event date column (padronizada para SIH)
    event_col <- NULL
    event_patterns <- c("data_internacao", "admission_date", "data_evento", 
                        "DT_INTER", "SP_DTINTER", "data_internacao_sp")
    for (pattern in event_patterns) {
      if (pattern %in% col_names) {
        event_col <- pattern
        break
      }
    }
    
    if (!is.null(birth_col) && !is.null(event_col)) {
      
      if (is_arrow_dataset) {
        # Arrow stores Date as int32 (days since 1970-01-01).
        # as.integer(Date) casts to int32 — fully supported kernel.
        # Subtracting two int32 values gives days between dates (no duration cast).
        df <- df |>
          dplyr::mutate(
            .data_nasc_tmp = as.integer(as.Date(.data[[birth_col]])),
            .data_evnt_tmp = as.integer(as.Date(.data[[event_col]])),
            age_years      = floor((.data_evnt_tmp - .data_nasc_tmp) / 365.25)
          ) |>
          dplyr::select(-".data_nasc_tmp", -".data_evnt_tmp")
          
      } else if (is_duckdb_conn) {
        # DuckDB SQL expression
        df <- df |>
          dplyr::mutate(
            age_years = sql(
              paste0(
                "FLOOR(DATEDIFF('day', ", birth_col, ", ", event_col, ") / 365.25)"
              )
            )
          )
      }
      
      age_col_to_use <- "age_years"
      
      if (verbose) {
        cli::cli_alert_success("Lazy age calculation from dates applied")
      }
      
    } else {
      # Try age code column (DATASUS format)
      age_code_col <- NULL
      age_code_patterns <- c("codigo_idade", "age_code", "IDADE", "NU_IDADE_N")
      for (pattern in age_code_patterns) {
        if (pattern %in% col_names) {
          age_code_col <- pattern
          break
        }
      }
      
      if (!is.null(age_code_col)) {
        if (verbose) {
          cli::cli_alert_info("Decoding DATASUS age codes...")
        }
        
        if (is_arrow_dataset) {
          # For Arrow, we must materialise temporarily for DATASUS code decoding
          # because the branching logic isn't expressible as a pure Arrow expression.
          
          if (verbose) {
            cli::cli_alert_warning("Age code decoding requires materialization...")
          }
          
          # Collect only the age code column for decoding
          age_codes_df <- df |>
            dplyr::select(dplyr::all_of(age_code_col)) |>
            dplyr::collect()
          
          # Decode
          age_years_vec <- decode_datasus_age_vectorized(age_codes_df[[age_code_col]])
          
          # Re-attach to the full collected data frame
          df_collected <- df |> dplyr::collect()
          df_collected$age_years <- age_years_vec
          
          if (lazy) {
            df <- arrow::as_arrow_table(df_collected)
          } else {
            df <- df_collected
          }
          
          age_col_to_use <- "age_years"
          
          if (verbose) {
            cli::cli_alert_success("Age decoded from DATASUS codes")
          }
        } else {
          # DuckDB: can use CASE WHEN inline
          df <- df |>
            dplyr::mutate(
              age_years = sql(
                paste0("
                  CASE 
                    WHEN SUBSTR(CAST(", age_code_col, " AS VARCHAR), 1, 1) = '1' THEN 0
                    WHEN SUBSTR(CAST(", age_code_col, " AS VARCHAR), 1, 1) = '2' THEN 0
                    WHEN SUBSTR(CAST(", age_code_col, " AS VARCHAR), 1, 1) = '3' THEN 
                      FLOOR(CAST(SUBSTR(CAST(", age_code_col, " AS VARCHAR), 2) AS INTEGER) / 12)
                    WHEN SUBSTR(CAST(", age_code_col, " AS VARCHAR), 1, 1) = '4' THEN 
                      CAST(SUBSTR(CAST(", age_code_col, " AS VARCHAR), 2) AS INTEGER)
                    WHEN SUBSTR(CAST(", age_code_col, " AS VARCHAR), 1, 1) = '5' THEN 
                      CAST(SUBSTR(CAST(", age_code_col, " AS VARCHAR), 2) AS INTEGER) + 100
                    ELSE NULL
                  END
                ")
              )
            )
          age_col_to_use <- "age_years"
        }
      } else {
        # Fall back to any existing age-like column
        age_patterns <- c("idade", "age_years", "age", "edad")
        for (pattern in age_patterns) {
          if (pattern %in% col_names) {
            age_col_to_use <- pattern
            if (verbose) {
              cli::cli_alert_info(paste0("Using existing age column: ", pattern))
            }
            break
          }
        }
      }
    }
  }
  
  # Ensure age column exists
  if (is.null(age_col_to_use) || !(age_col_to_use %in% names(df))) {
    cli::cli_abort(c(
      "Could not determine age column or calculate age from available data",
      "i" = "Available columns with potential age information:",
      "*" = paste(grep("idade|nasc|data|age|birth", col_names, 
                       ignore.case = TRUE, value = TRUE), collapse = "\n  "),
      ">" = "Please specify age column manually with {.code age_col} parameter"
    ))
  }
  
  # ========================================================================
  # CREATE AGE GROUPS (LAZY COMPATIBLE)
  # ========================================================================
  
  if (create_age_groups && !is.null(age_col_to_use)) {
    
    if (verbose) {
      msg <- switch(lang,
        "en" = "Creating age groups...",
        "pt" = "Criando faixas etarias...",
        "es" = "Creando grupos de edad..."
      )
      cli::cli_alert_info(msg)
    }
    
    # Generate labels if not provided
    if (is.null(age_labels)) {
      age_labels <- generate_age_labels(age_breaks, lang)
    }
    
    if (is_arrow_dataset || is_duckdb_conn) {
      # Build two-sided formulas (condition ~ label) required by case_when().
      # The previous approach used bare expressions, which caused:
      # "Each argument to case_when() must be a two-sided formula".
      formulas <- vector("list", length(age_labels))
      for (i in seq_along(age_labels)) {
        lower <- age_breaks[i]
        upper <- age_breaks[i + 1]
        label <- age_labels[i]
        lhs <- if (is.infinite(upper)) {
          rlang::expr(.data[[!!age_col_to_use]] >= !!lower)
        } else {
          rlang::expr(.data[[!!age_col_to_use]] >= !!lower &
                      .data[[!!age_col_to_use]] <  !!upper)
        }
        formulas[[i]] <- rlang::new_formula(lhs, label)
      }
      case_expr <- rlang::call2("case_when", !!!formulas, .default = NA_character_)
      df <- df |> dplyr::mutate(age_group = !!case_expr)
      
    } else {
      # Regular data frame
      df$age_group <- cut(
        df[[age_col_to_use]],
        breaks = age_breaks,
        labels = age_labels,
        right = FALSE,
        include.lowest = TRUE
      )
    }
    
    created_vars <- c(created_vars, "age_group")
    
    # Climate risk group
    risk_labels <- switch(lang,
      "en" = c("High Risk (0-4)", "Standard Risk (5-64)", "High Risk (65+)"),
      "pt" = c("Alto Risco (0-4)", "Risco Padrao (5-64)", "Alto Risco (65+)"),
      "es" = c("Alto Riesgo (0-4)", "Riesgo Estandar (5-64)", "Alto Riesgo (65+)")
    )
    
    risk_varname <- switch(lang,
      "en" = "climate_risk_group",
      "pt" = "grupo_risco_climatico",
      "es" = "grupo_riesgo_climatico"
    )
    
    if (is_arrow_dataset || is_duckdb_conn) {
      df <- df |>
        dplyr::mutate(
          !!risk_varname := dplyr::case_when(
            .data[[age_col_to_use]] <= 4 ~ !!risk_labels[1],
            .data[[age_col_to_use]] <= 64 ~ !!risk_labels[2],
            .data[[age_col_to_use]] >= 65 ~ !!risk_labels[3],
            .default = NA_character_
          )
        )
    } else {
      df[[risk_varname]] <- cut(
        df[[age_col_to_use]],
        breaks = c(-1, 4, 64, Inf),
        labels = risk_labels,
        right = TRUE,
        include.lowest = TRUE
      )
    }
    
    created_vars <- c(created_vars, risk_varname)
    
    # IBGE age groups
    ibge_labels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                     "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", 
                     "60-64", "65-69", "70-74", "75-79", "80+")
    
    ibge_varname <- switch(lang,
      "pt" = "faixa_etaria_ibge",
      "en" = "ibge_age_group",
      "es" = "grupo_edad_ibge"
    )
    
    if (is_arrow_dataset || is_duckdb_conn) {
      ibge_breaks <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 
                       65, 70, 75, 80, Inf)
      
      formulas_ibge <- vector("list", length(ibge_labels))
      for (i in seq_along(ibge_labels)) {
        lower <- ibge_breaks[i]
        upper <- ibge_breaks[i + 1]
        label <- ibge_labels[i]
        lhs <- if (is.infinite(upper)) {
          rlang::expr(.data[[!!age_col_to_use]] >= !!lower)
        } else {
          rlang::expr(.data[[!!age_col_to_use]] >= !!lower &
                      .data[[!!age_col_to_use]] <  !!upper)
        }
        formulas_ibge[[i]] <- rlang::new_formula(lhs, label)
      }
      case_expr_ibge <- rlang::call2("case_when", !!!formulas_ibge, .default = NA_character_)
      df <- df |> dplyr::mutate(!!ibge_varname := !!case_expr_ibge)
    } else {
      df[[ibge_varname]] <- cut(
        df[[age_col_to_use]],
        breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 
                   65, 70, 75, 80, Inf),
        labels = ibge_labels,
        right = FALSE,
        include.lowest = TRUE
      )
    }
    
    created_vars <- c(created_vars, ibge_varname)
  }
  
  # ========================================================================
  # CALENDAR VARIABLES (simplified for Arrow compatibility)
  # ========================================================================
  
  if (create_calendar_vars) {
    
    # Auto-detect date column
    if (is.null(date_col)) {
      date_patterns <- c("data_internacao", "data_internacao_sp", "DT_INTER", 
                         "SP_DTINTER", "admission_date")
      for (pattern in date_patterns) {
        if (pattern %in% col_names) {
          date_col <- pattern
          break
        }
      }
      
      if (verbose && !is.null(date_col)) {
        msg <- switch(lang,
          "en" = paste0("Auto-detected date column: ", date_col),
          "pt" = paste0("Coluna de data auto-detectada: ", date_col),
          "es" = paste0("Columna de fecha auto-detectada: ", date_col)
        )
        cli::cli_alert_info(msg)
      }
    }
    
    if (!is.null(date_col) && date_col %in% col_names) {
      
      if (verbose) {
        msg <- switch(lang,
          "en" = "Creating calendar variables...",
          "pt" = "Criando variaveis de calendario...",
          "es" = "Creando variables de calendario..."
        )
        cli::cli_alert_info(msg)
      }
      
      if (is_arrow_dataset || is_duckdb_conn) {
        df <- df |>
          dplyr::mutate(
            data_ref  = as.Date(.data[[date_col]]),
            ano       = lubridate::year(.data$data_ref),
            mes       = lubridate::month(.data$data_ref),
            trimestre = lubridate::quarter(.data$data_ref)
          ) |>
          dplyr::select(-data_ref)
        
        created_vars <- c(created_vars, "ano", "mes", "trimestre")
        
      } else {
        if (!inherits(df[[date_col]], "Date")) {
          df[[date_col]] <- as.Date(df[[date_col]])
        }
        
        df$ano       <- lubridate::year(df[[date_col]])
        df$mes       <- lubridate::month(df[[date_col]])
        df$trimestre <- lubridate::quarter(df[[date_col]])
        
        created_vars <- c(created_vars, "ano", "mes", "trimestre")
      }
    } else {
      cli::cli_alert_warning("No date column found. Calendar variables not created.")
    }
  }
  
  # ========================================================================
  # SUMMARY
  # ========================================================================
  
  if (verbose && length(created_vars) > 0) {
    msg <- switch(lang,
      "en" = paste0("Created ", length(created_vars), " new variables: ",
                    paste(created_vars, collapse = ", ")),
      "pt" = paste0("Criadas ", length(created_vars), " novas variaveis: ",
                    paste(created_vars, collapse = ", ")),
      "es" = paste0("Creadas ", length(created_vars), " nuevas variables: ",
                    paste(created_vars, collapse = ", "))
    )
    cli::cli_alert_success(msg)
  }
  
  # Add climasus_df metadata if not lazy
  if (!lazy && !(is_arrow_dataset || is_duckdb_conn)) {
    if (!inherits(df, "climasus_df")) {
      meta <- list(
        system   = system,
        stage    = "derive",
        type     = "derive",
        spatial  = inherits(df, "sf"),
        temporal = NULL,
        created  = Sys.time(),
        modified = Sys.time(),
        history  = sprintf("[%s] Create variables (Arrow optimized)", 
                           format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        user     = list()
      )
      
      base_classes <- setdiff(class(df), "climasus_df")
      df <- structure(
        df,
        sus_meta = meta,
        class    = c("climasus_df", base_classes)
      )
    } else {
      df <- sus_meta(df, stage = "derive", type = "derive",
                    add_history = "Derived variables created (Arrow pipeline)")
    }
  }
  
  return(df)
}

# ============================================================================
# LAZY COLUMN DETECTION HELPERS
# ============================================================================

#' Detect birth date column in lazy dataset
#' @keywords internal
#' @noRd
detect_birth_date_column_lazy <- function(col_names) {
  patterns <- c("birth_date", "data_nascimento", "fecha_nacimiento", "DTNASC")
  for (pattern in patterns) {
    if (pattern %in% col_names) return(pattern)
  }
  return(NULL)
}

#' Detect event date column in lazy dataset
#' @keywords internal
#' @noRd
detect_event_date_column_lazy <- function(col_names) {
  patterns <- c("death_date", "notification_date", "admission_date", "event_date",
                "data_obito", "data_notificacao", "data_internacao", "data_evento",
                "fecha_muerte", "fecha_notificacion", "fecha_evento",
                "DTOBITO", "DT_NOTIFIC", "DT_INTER", "DT_SAIDA", "DT_EVENT")
  for (pattern in patterns) {
    if (pattern %in% col_names) return(pattern)
  }
  return(NULL)
}

#' Detect age code column in lazy dataset
#' @keywords internal
#' @noRd
detect_age_code_column_lazy <- function(col_names) {
  patterns <- c("age_code", "codigo_idade", "codigo_edad", "NU_IDADE_N", "IDADE")
  for (pattern in patterns) {
    if (pattern %in% col_names) return(pattern)
  }
  return(NULL)
}

#' Detect date column in lazy dataset
#' @keywords internal
#' @noRd
detect_date_column_lazy <- function(col_names) {
  date_patterns <- c("date", "data", "fecha", "DT_NOTIFIC", "DTOBITO", "DT_INTER",
                     "DTNASC", "DT_SAIDA", "DT_EVENT", "notification_date",
                     "death_date", "admission_date", "event_date")
  for (pattern in date_patterns) {
    if (pattern %in% col_names) return(pattern)
  }
  return(NULL)
}