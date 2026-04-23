#' Filter SUS health data by ICD-10 codes or disease groups with multilingual support
#'
#' Filters Brazilian Unified Health System (SUS) data based on ICD-10 codes.
#' Supports lazy evaluation with Arrow and DuckDB backends for out-of-memory processing.
#'
#' @param df A `data.frame`, `tibble`, Arrow Dataset, Arrow query, or DuckDB table
#'   containing SUS health data with ICD-10 codes.
#' @param icd_codes A character vector of ICD-10 codes, ranges, or categories.
#' @param disease_group Character. Name of predefined disease group.
#' @param icd_column Character. Name of the column containing ICD-10 codes.
#' @param match_type Character. Type of matching algorithm.
#' @param lang Character. Language for user interface messages.
#' @param verbose Logical. If TRUE, prints detailed filtering information.
#'
#' @return A filtered object of the same class as input (preserves backend type).
#' @export
#'
#' @examples
#' \dontrun{
#' # With Arrow Dataset (lazy)
#' arrow_ds <- arrow::open_dataset("data/sim/")
#' arrow_filtered <- sus_data_filter_cid(arrow_ds, disease_group = "dengue")
#' 
#' # With DuckDB
#' con <- DBI::dbConnect(duckdb::duckdb())
#' duck_tbl <- dplyr::tbl(con, "sim_data")
#' duck_filtered <- sus_data_filter_cid(duck_tbl, disease_group = "cardiovascular")
#' }
sus_data_filter_cid_arrow <- function(df,
                                icd_codes = NULL,
                                disease_group = NULL,
                                icd_column = NULL,
                                match_type = "starts_with",
                                lang = "pt",
                                verbose = TRUE) {
  
  # ============================================================================
  # Input Validation
  # ============================================================================
  cli::cli_h1("climasus4r - Filter Data ICD-10")
  
  # Detect backend type
  backend_type <- detect_backend_type(df)
  
  if (backend_type == "unsupported") {
    cli::cli_alert_danger("Input must be a data.frame, Arrow Dataset, or DuckDB table.")
    stop("Unsupported input type.")
  }
  
  # Check if data is empty (only for data frames)
  if (backend_type == "dataframe" && nrow(df) == 0) {
    cli::cli_alert_warning("Input data frame is empty. Returning as-is.")
    return(df)
  }
  
  # Validate language
  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_alert_warning("Language '{lang}' not supported. Using Portuguese (pt).")
    lang <- "pt"
  }
  
  # ============================================================================
  # Handle climasus metadata (if available)
  # ============================================================================
  
  # Try to get metadata for stage validation
  has_metadata <- FALSE
  current_stage <- NULL
  detected_system <- NULL
  
  if (inherits(df, "climasus_df") || inherits(df, "climasus_dataset")) {
    has_metadata <- TRUE
    current_stage <- tryCatch(sus_meta(df, "stage"), error = function(e) NULL)
    detected_system <- tryCatch(sus_meta(df, "system"), error = function(e) NULL)
    
    required_stage <- "stand"
    
    if (!is.null(current_stage) && !is_stage_at_least(current_stage, required_stage)) {
      msg_error <- list(
        en = paste0("Data must be standardized before disease filtering (CID-10). Current stage: ", current_stage),
        pt = paste0("Dados devem ser padronizados antes da filtragem por doenca (CID-10). Estagio atual: ", current_stage),
        es = paste0("Los datos deben estar estandarizados antes del filtrado por enfermedad (CIE-10). Etapa actual: ", current_stage)
      )
      cli::cli_abort(msg_error[[lang]])
    }
    
    if (verbose && !is.null(current_stage)) {
      msg_stage_ok <- list(
        en = "Data stage validated: disease filtering (CID-10)",
        pt = "Estagio de dados validado: filtragem por doenca (CID-10)",
        es = "Etapa de datos validada: filtrado por enfermedad (CIE-10)"
      )
      cli::cli_alert_success(msg_stage_ok[[lang]])
    }
  }
  
  # ============================================================================
  # Handle icd_codes vs disease_group
  # ============================================================================
  
  if (is.null(icd_codes) && is.null(disease_group)) {
    msg <- list(
      en = "Either 'icd_codes' or 'disease_group' must be provided.",
      pt = "E necessario fornecer 'icd_codes' ou 'disease_group'.",
      es = "Debe proporcionar 'icd_codes' o 'disease_group'."
    )
    cli::cli_alert_danger(msg[[lang]])
    stop("Missing required parameter.")
  }
  
  if (!is.null(icd_codes) && !is.null(disease_group)) {
    msg <- list(
      en = "Both provided. Using 'disease_group' and ignoring 'icd_codes'.",
      pt = "Ambos fornecidos. Usando 'disease_group' e ignorando 'icd_codes'.",
      es = "Ambos proporcionados. Usando 'disease_group' e ignorando 'icd_codes'."
    )
    cli::cli_alert_warning(msg[[lang]])
    icd_codes <- NULL
  }
  
  # ============================================================================
  # Process disease_group if provided
  # ============================================================================
  
  if (!is.null(disease_group)) {
    # Handle single or multiple disease groups
    disease_groups_vec <- if (length(disease_group) == 1) disease_group else disease_group
    
    # Validate ALL disease groups exist
    invalid_groups <- disease_groups_vec[!disease_groups_vec %in% names(.icd_disease_groups)]
    
    if (length(invalid_groups) > 0) {
      available_groups <- paste(names(.icd_disease_groups), collapse = ", ")
      msg <- list(
        en = paste0("Invalid disease group(s): '", paste(invalid_groups, collapse = "', '"), "'. Available: ", available_groups),
        pt = paste0("Grupo(s) de doenca invalido(s): '", paste(invalid_groups, collapse = "', '"), "'. Disponiveis: ", available_groups),
        es = paste0("Grupo(s) de enfermedad invalido(s): '", paste(invalid_groups, collapse = "', '"), "'. Disponibles: ", available_groups)
      )
      cli::cli_abort(msg[[lang]])
    }
    
    # Extract ICD codes from ALL groups
    icd_codes <- character(0)
    for (group in disease_groups_vec) {
      group_codes <- .icd_disease_groups[[group]]$codes
      icd_codes <- c(icd_codes, group_codes)
    }
    icd_codes <- unique(icd_codes)
    
    if (verbose) {
      msg <- list(
        en = paste0("Using disease group(s): ", paste(disease_groups_vec, collapse = ", "), "\nTotal ICD codes: ", length(icd_codes)),
        pt = paste0("Usando grupo(s) de doenca: ", paste(disease_groups_vec, collapse = ", "), "\nTotal de codigos CID: ", length(icd_codes)),
        es = paste0("Usando grupo(s) de enfermedad: ", paste(disease_groups_vec, collapse = ", "), "\nTotal de codigos CIE: ", length(icd_codes))
      )
      cli::cli_alert_info(msg[[lang]])
    }
  }
  
  # ============================================================================
  # Auto-detect ICD column if not specified
  # ============================================================================
  
  if (is.null(icd_column)) {
    # Get column names (works for lazy objects too)
    col_names <- get_column_names(df, backend_type)
    
    # Detect health system from metadata or auto-detect
    if (is.null(detected_system)) {
      detected_system <- detect_health_system_lazy(df, col_names, backend_type)
    }
    
    # Priority column names by language
    if (lang == "en") {
      icd_column_priority <- list(
        SIM = c("underlying_cause", "underlying_cause_original", "cause_line_a", "cause_line_b", "cause_line_c", "cause_line_d"),
        SIH = c("primary_diagnosis", "secondary_diagnosis", "death_cause", "notified_cause", "sp_primary_icd", "sp_secondary_icd", paste0("secondary_diagnosis_", 1:9)),
        SIA = c("primary_icd", "secondary_icd", "associated_icd"),
        SINASC = c("underlying_cause", "maternal_cause", "fetal_cause"),
        CNES = character(0),
        SINAN = character(0)
      )
    } else if (lang == "pt") {
      icd_column_priority <- list(
        SIM = c("causa_basica", "causa_basica_original", "linha_causa_a", "linha_causa_b", "linha_causa_c", "linha_causa_d"),
        SIH = c("diagnostico_principal", "diagnostico_secundario", "causa_morte", "causa_notificada", "cid_principal_sp", "sp_secundario_icd", paste0("diagnostico_secundario_", 1:9)),
        SIA = c("cid_principal", "cid_secundario", "cid_associado"),
        SINASC = c("causa_basica", "causa_materna", "causa_fetal"),
        CNES = character(0),
        SINAN = character(0)
      )
    } else {
      icd_column_priority <- list(
        SIM = c("causa_basica", "causa_basica_original", "linea_causa_a", "linea_causa_b", "linea_causa_c", "linea_causa_d"),
        SIH = c("diagnostico_principal", "diagnostico_secundario", "causa_muerte", "causa_notificada", "sp_cie_principal", "sp_cie_secundario", paste0("diagnostico_secundario_", 1:9)),
        SIA = c("cie_principal", "cie_secundario", "cie_asociado"),
        SINASC = c("causa_basica", "causa_materna", "causa_fetal"),
        CNES = character(0),
        SINAN = character(0)
      )
    }
    
    if (detected_system %in% names(icd_column_priority)) {
      possible_cols <- icd_column_priority[[detected_system]]
      icd_column <- possible_cols[possible_cols %in% col_names][1]
    }
    
    # If still NULL, try common patterns
    if (is.null(icd_column) || is.na(icd_column)) {
      if (lang == "en") {
        pattern <- "underlying_cause|primary_diagnosis|primary_icd|maternal_cause"
      } else if (lang == "pt") {
        pattern <- "causa_basica|diagnostico_principal|cid_principal|causa_materna"
      } else {
        pattern <- "causa_basica|diagnostico_principal|cie_principal|causa_materna"
      }
      matches <- grep(pattern, col_names, value = TRUE, ignore.case = TRUE)
      if (length(matches) > 0) {
        icd_column <- matches[1]
      }
    }
    
    # If still NULL, error
    if (is.null(icd_column) || is.na(icd_column)) {
      msg <- list(
        en = "No ICD column found. Please specify 'icd_column' manually.",
        pt = "Nenhuma coluna CID encontrada. Por favor, especifique 'icd_column' manualmente.",
        es = "No se encontro columna CIE. Por favor, especifique 'icd_column' manualmente."
      )
      cli::cli_alert_danger(msg[[lang]])
      stop("ICD column not found.")
    }
    
    if (verbose) {
      msg_detected <- list(
        en = paste0("Auto-detected ICD column: ", icd_column),
        pt = paste0("Coluna CID detectada automaticamente: ", icd_column),
        es = paste0("Columna CIE detectada automaticamente: ", icd_column)
      )
      cli::cli_alert_info(msg_detected[[lang]])
    }
  }
  
  # ============================================================================
  # Process ICD codes and create filter expression
  # ============================================================================
  
  # Expand ICD code ranges
  expanded_codes <- process_icd_codes(icd_codes)
  
  # Create filter pattern
  if (match_type == "exact") {
    # Exact match
    filter_expr <- list(icd_column = icd_column, codes = expanded_codes, type = "exact")
  } else if (match_type == "starts_with") {
    # Starts with pattern
    pattern <- paste0("^(", paste(expanded_codes, collapse = "|"), ")")
    filter_expr <- list(icd_column = icd_column, pattern = pattern, type = "starts_with")
  } else if (match_type == "range") {
    # Range pattern
    pattern <- paste(expanded_codes, collapse = "|")
    filter_expr <- list(icd_column = icd_column, pattern = pattern, type = "range")
  } else {
    cli::cli_alert_warning("Invalid match_type. Using 'starts_with'.")
    pattern <- paste0("^(", paste(expanded_codes, collapse = "|"), ")")
    filter_expr <- list(icd_column = icd_column, pattern = pattern, type = "starts_with")
  }
  
  # ============================================================================
  # Apply filter based on backend type
  # ============================================================================
  
  filtered_df <- apply_filter_by_backend(df, filter_expr, backend_type)
  
  # ============================================================================
  # Report results (only for data frames - for lazy backends, show estimate)
  # ============================================================================
  
  if (verbose) {
    b_mark <- if(lang == "en") "," else "."
    d_mark <- if(lang == "en") "." else ","
    
    if (backend_type == "dataframe") {
      n_orig <- nrow(df)
      n_filt <- nrow(filtered_df)
      pct <- round(100 * n_filt / n_orig, 1)
      
      n_orig_txt <- format(n_orig, big.mark = b_mark, decimal.mark = d_mark)
      n_filt_txt <- format(n_filt, big.mark = b_mark, decimal.mark = d_mark)
      pct_txt <- gsub("\\.", d_mark, as.character(pct))
      
      if (lang == "pt") {
        cli::cli_alert_info("Registros originais: {n_orig_txt}")
        cli::cli_alert_success("Registros filtrados: {n_filt_txt}")
        cli::cli_alert_info("Percentual retido: {pct_txt}%")
      } else if (lang == "es") {
        cli::cli_alert_info("Registros originales: {n_orig_txt}")
        cli::cli_alert_success("Registros filtrados: {n_filt_txt}")
        cli::cli_alert_info("Porcentaje retenido: {pct_txt}%")
      } else {
        cli::cli_alert_info("Original records: {n_orig_txt}")
        cli::cli_alert_success("Filtered records: {n_filt_txt}")
        cli::cli_alert_info("Percentage retained: {pct_txt}%")
      }
    } else {
      # For lazy backends, show that filter is applied lazily
      msg_lazy <- list(
        en = "Filter applied lazily (out-of-memory). Use `collect()` to materialize.",
        pt = "Filtro aplicado lazy (fora da memoria). Use `collect()` para materializar.",
        es = "Filtro aplicado lazy (fuera de memoria). Use `collect()` para materializar."
      )
      cli::cli_alert_info(msg_lazy[[lang]])
    }
  }
  
  # ============================================================================
  # Update metadata if climasus object
  # ============================================================================
  
  # if (has_metadata || inherits(filtered_df, "climasus_dataset") || inherits(filtered_df, "arrow_dplyr_query")) {
  #   # Try to add climasus class and metadata
  #   if (!inherits(filtered_df, "climasus_df") && !inherits(filtered_df, "climasus_dataset")) {
  #     # Add climasus_dataset class for Arrow objects
  #     if (inherits(filtered_df, c("arrow_dplyr_query", "Dataset", "ArrowTabular"))) {
  #       class(filtered_df) <- unique(c("climasus_dataset", class(filtered_df)))
  #     }
  #   }
    
  #   # Update metadata
  #   filtered_df <- tryCatch({
  #     sus_meta(
  #       filtered_df,
  #       system = detected_system,
  #       stage = "filter_cid",
  #       type = "filter_cid"
  #     )
  #   }, error = function(e) {
  #     # If sus_meta fails, return as-is
  #     if (verbose) {
  #       cli::cli_alert_warning("Could not update metadata: {e$message}")
  #     }
  #     filtered_df
  #   })
    
  #   # Add to processing history
  #   if (!is.null(disease_group)) {
  #     if (length(disease_group) == 1) {
  #       history_msg <- sprintf("Filtered by disease group: %s", disease_group)
  #     } else {
  #       history_msg <- sprintf("Filtered by disease groups: %s", paste(disease_group, collapse = ", "))
  #     }
  #   } else {
  #     history_msg <- sprintf("Filtered by ICD codes: %s", paste(utils::head(icd_codes, 3), collapse = ", "))
  #     if (length(icd_codes) > 3) {
  #       history_msg <- paste0(history_msg, sprintf(" (and %d more)", length(icd_codes) - 3))
  #     }
  #   }
    
  #   filtered_df <- tryCatch({
  #     sus_meta(filtered_df, add_history = history_msg)
  #   }, error = function(e) {
  #     filtered_df
  #   })
  # }
  
  return(filtered_df)
}

# ============================================================================
# Helper functions for backend support
# ============================================================================

#' Detect backend type of input object
#' @keywords internal
#' @noRd
detect_backend_type <- function(df) {
  if (inherits(df, "data.frame")) {
    return("dataframe")
  } else if (inherits(df, c("arrow_dplyr_query", "Dataset", "ArrowTabular", "RecordBatchReader"))) {
    return("arrow")
  } else if (inherits(df, c("tbl_duckdb_connection", "duckdb_connection"))) {
    return("duckdb")
  } else {
    return("unsupported")
  }
}

#' Get column names from lazy objects
#' @keywords internal
#' @noRd
get_column_names <- function(df, backend_type) {
  if (backend_type == "dataframe") {
    return(names(df))
  } else if (backend_type == "arrow") {
    # For Arrow objects
    if (inherits(df, "arrow_dplyr_query")) {
      return(names(df$selected_columns %||% df$.data$schema$names))
    } else if (inherits(df, "Dataset")) {
      return(df$schema$names)
    } else {
      return(tryCatch(names(df), error = function(e) character(0)))
    }
  } else if (backend_type == "duckdb") {
    # For DuckDB tbl objects
    return(tryCatch(colnames(df), error = function(e) character(0)))
  }
  return(character(0))
}

#' Detect health system from lazy object
#' @keywords internal
#' @noRd
detect_health_system_lazy <- function(df, col_names, backend_type) {
  # Common patterns for each system
  sim_patterns <- c("causa_basica", "underlying_cause", "linha_causa", "cause_line", "causabas", "CAUSABAS")
  sih_patterns <- c("diagnostico_principal", "primary_diagnosis", "diag_princ", "DIAG_PRINC")
  sinan_patterns <- c("agravado", "disease", "doenca", "notific")
  sinasc_patterns <- c("nascimento", "birth", "mae", "mother")
  
  col_names_lower <- tolower(col_names)
  
  if (any(sapply(sim_patterns, function(p) any(grepl(p, col_names_lower))))) {
    return("SIM")
  } else if (any(sapply(sih_patterns, function(p) any(grepl(p, col_names_lower))))) {
    return("SIH")
  } else if (any(sapply(sinan_patterns, function(p) any(grepl(p, col_names_lower))))) {
    return("SINAN")
  } else if (any(sapply(sinasc_patterns, function(p) any(grepl(p, col_names_lower))))) {
    return("SINASC")
  }
  
  return("UNKNOWN")
}

#' Apply filter based on backend type
#' @keywords internal
#' @noRd
apply_filter_by_backend <- function(df, filter_expr, backend_type) {
  
  icd_col <- filter_expr$icd_column
  
  if (backend_type == "dataframe") {
    # Standard data frame filtering
    if (filter_expr$type == "exact") {
      result <- df[df[[icd_col]] %in% filter_expr$codes, ]
    } else {
      result <- df[grepl(filter_expr$pattern, df[[icd_col]], ignore.case = FALSE), ]
    }
    
  } else if (backend_type == "arrow") {
    # Arrow lazy filtering using dplyr verbs
    if (filter_expr$type == "exact") {
      result <- df %>%
        dplyr::filter(!!rlang::sym(icd_col) %in% filter_expr$codes)
    } else {
      result <- df %>%
        dplyr::filter(grepl(filter_expr$pattern, !!rlang::sym(icd_col)))
    }
    
  } else if (backend_type == "duckdb") {
    # DuckDB lazy filtering
    if (filter_expr$type == "exact") {
      result <- df %>%
        dplyr::filter(!!rlang::sym(icd_col) %in% filter_expr$codes)
    } else {
      result <- df %>%
        dplyr::filter(grepl(filter_expr$pattern, !!rlang::sym(icd_col)))
    }
    
  } else {
    stop("Unsupported backend type")
  }
  
  return(result)
}