#' Filter SUS health data by ICD-10 codes or disease groups with multilingual support
#'
#' Filters Brazilian Unified Health System (SUS) data based on ICD-10 codes.
#' (International Classification of Diseases, 10th Revision) or predefined epidemiological disease groups. This function 
#' supports complex filtering scenarios including specific codes, code ranges, 
#' chapters, and 50+ disease groups relevant to epidemiological research in Brazil.
#' Includes specialized support for SUS-specific coding practices and 
#' multilingual interface (English, Portuguese, Spanish).
#'
#' @param df A `data.frame` or `tibble` containing SUS health data with ICD-10 codes.
#'   Typically obtained from DATASUS systems (SIM, SIH, SINAN). The data should
#'   contain at least one column with ICD-10 codes in standard format (e.g., "A00.0",
#'   "I10", "C50.9").
#' @param icd_codes A character vector of ICD-10 codes, ranges, or categories.
#'   Multiple syntaxes are supported:
#'   
#'   **Basic filtering:**
#'   - Single code: `"J18.9"` (Pneumonia, unspecified)
#'   - Multiple codes: `c("I10", "I11.0", "I11.9")` (Hypertensive diseases)
#'   
#'   **Range filtering:**
#'   - Complete range: `"J00-J99"` (All diseases of respiratory system)
#'   - Partial range: `"J09-J18"` (Influenza and pneumonia only)
#'   
#'   **Chapter filtering:**
#'   - Full chapter: `"I"` (All diseases of circulatory system, codes I00-I99)
#'   - Chapter group: `"C00-D48"` (All neoplasms)
#'   
#'   **Special SUS categories:**
#'   - `"causas_externas"` or `"external_causes"`: V01-Y98 (External causes)
#'   - `"causas_maternas"` or `"maternal_causes"`: O00-O99 (Pregnancy/childbirth)
#'   - `"causas_infantis"` or `"infant_causes"`: P00-P96 (Perinatal conditions)
#'   - `"doencas_infecciosas"` or `"infectious_diseases"`: A00-B99 (Infectious)
#'   - `"doencas_respiratorias"` or `"respiratory_diseases"`: J00-J99 (Respiratory)
#'   - `"doencas_cardiovasculares"` or `"cardiovascular_diseases"`: I00-I99 (Cardio)
#'   - `"neoplasias"` or `"neoplasms"`: C00-D48 (Neoplasms)
#'   
#'   **Brazilian epidemiological priorities:**
#'   - `"dengue_like"`: A90-A91 (Dengue) + A92.0-A92.9 (Other viral fevers)
#'   - `"zika_chik"`: A92.8 (Zika) + A92.0 (Chikungunya)
#'   - `"tb_respiratoria"`: A15-A16 (Respiratory tuberculosis)
#'   - `"covid19"`: U07.1 (COVID-19) + U07.2 (Suspected COVID-19)
#'   - `"violencia"`: X85-Y09 (Assault) + Y35-Y36 (Legal intervention)
#' 
#'   **Note**: Either `icd_codes` OR `disease_group` must be provided, not both.
#' 
#' @param disease_group Character. Name of predefined disease group (e.g., "dengue", 
#'   "cardiovascular", "respiratory"). Use `list_disease_groups()` to see all available groups.
#'   Mutually exclusive with `icd_codes`.
#'   
#' @param icd_column Character. Name of the column containing ICD-10 codes.
#'   If NULL (default), the function attempts auto-detection from common SUS
#'   column names in this priority order:
#'   1. `"CAUSABAS"` - Underlying cause (primary cause of death in SIM)
#'   2. `"DIAG_PRINC"` - Main diagnosis (SIH hospitalizations)
#'   3. `"DIAG_SECUN"` - Secondary diagnosis
#'   4. `"CAUSAOBITO"` - Cause of death (alternative SIM field)
#'   5. `"DIAGNOSTIC"` - Diagnosis (SINAN notifiable diseases)
#'   6. `"linha_a"` through `"linha_f"` - Multiple cause lines (SIM)
#'   
#' @param match_type Character. Type of matching algorithm:
#'   - `"exact"`: Exact code match (e.g., "I10" matches only "I10")
#'   - `"starts_with"`: Match codes starting with pattern (default, e.g., "I10" matches "I10", "I10.0", "I10.9")
#'   - `"range"`: Match codes within specified ranges (e.g., "I10-I15" matches I10-I15.9)
#'   - `"chapter"`: Match entire ICD-10 chapter (e.g., "I" matches I00-I99.9)
#'   - `"fuzzy"`: Allow for common SUS coding variations (e.g., "I10" matches "I10", "I10 ", "I10X")
#'   
#' @param lang Character. Language for user interface messages, warnings, and 
#'   documentation. Options:
#'   - `"en"`: English 
#'   - `"pt"`: Portuguese (default, recommended for Brazilian users)
#'   - `"es"`: Spanish
#'   Affects all console output and documentation of matched codes.
#'   
#' @param verbose Logical. If TRUE (default), prints detailed filtering
#'   information including: records processed, match statistics, common
#'   coding issues detected, and time elapsed.
#'
#' @return A filtered `data.frame` or `tibble` containing only records matching
#'   the specified ICD-10 codes or disease group. The output preserves all original columns
#'
#' @details
#' ## Automatic ICD Column Detection
#' 
#' The function automatically identifies the appropriate ICD column based on the
#' health system
#' 
#' ## Disease Groups
#' 
#' The function includes 50+ predefined epidemiological groups organized by:
#' 
#' - **ICD Chapters**: All major disease categories (A00-Y98)
#' - **Climate-Sensitive Diseases**: Vector-borne, waterborne, heat-related, etc.
#' - **Specific Conditions**: Dengue, malaria, cardiovascular, respiratory, etc.
#' - **Syndromic Groups**: Fever, respiratory, diarrheal syndromes
#' - **Age-Specific Groups**: Pediatric, elderly populations
#' 
#' Each group includes:
#' - ICD code ranges
#' - Multilingual labels and descriptions
#' - Climate sensitivity flag
#' - Associated climate factors
#' 
#' Use `list_disease_groups()` to see all available groups and their details.
#' 
#'
#' @importFrom stringr str_detect str_trim str_to_upper str_remove_all
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_alert_danger
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 1: Filter by explicit ICD codes
#' df_cardio <- sus_data_filter_cid(
#'   sim_data,
#'   icd_codes = "I00-I99",
#'   lang = "en"
#' )
#' 
#' # Example 2: Filter by disease group (easier!)
#' df_dengue <- sus_data_filter_cid(
#'   sinan_data,
#'   disease_group = "dengue",
#'   lang = "pt"
#' )
#' 
#' # Example 3: Climate-sensitive diseases
#' df_climate <- sus_data_filter_cid(
#'   sim_data,
#'   disease_group = "climate_sensitive_all",
#'   lang = "en"
#' )
#' 
#' # Example 4: Multiple specific codes
#' df_ami_stroke <- sus_data_filter_cid(
#'   sih_data,
#'   icd_codes = c("I21", "I22", "I63", "I64"),
#'   lang = "es"
#' )
#' 
#' # Example 5: Respiratory diseases in children
#' df_pediatric <- sus_data_filter_cid(
#'   sih_data,
#'   disease_group = "pediatric_respiratory",
#'   lang = "pt"
#' )
#' 
#' # List all available disease groups
#' list_disease_groups(lang = "pt")
#' 
#' # List only climate-sensitive groups
#' list_disease_groups(climate_sensitive_only = TRUE, lang = "en")
#' 
#' # Get details about a specific group
#' get_disease_group_details("dengue", lang = "pt")
#' }
#'
#' @references
#' 1. World Health Organization. (2016). ICD-10 International Statistical 
#'    Classification of Diseases and Related Health Problems. 10th Revision.
#'    
#' 2. Brazilian Ministry of Health. (2023). Classificacao Estatistica 
#'    Internacional de Doencas e Problemas Relacionados a Saude - CID-10.
#'    DATASUS. \url{http://datasus.saude.gov.br/cid10}
#' 
sus_data_filter_cid <- function(df,
                                icd_codes = NULL,
                                disease_group = NULL,
                                icd_column = NULL,
                                match_type = "starts_with",
                                lang = "pt",
                                verbose = TRUE) {
  
  # ============================================================================
  # Input Validation
  # ============================================================================
  
  if (!is.data.frame(df)) {
    cli::cli_alert_danger("Input 'df' must be a data.frame.")
    stop("Invalid input type.")
  }
  
  if (nrow(df) == 0) {
    cli::cli_alert_warning("Input data frame is empty. Returning as-is.")
    return(df)
  }
  
  # Validate language
  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_alert_warning(
      "Language '{lang}' not supported. Using Portuguese (pt)."
    )
    lang <- "pt"
  }

    # Check if data is climasus_df
  # Check if data is climasus_df
  if (inherits(df, "climasus_df")) {
    current_stage <- climasus_meta(df, "stage")
    required_stage <- "stand"

    if (!is_stage_at_least(current_stage, required_stage)) {

      msg_error <- list(
        en = paste0(
          "Data must be standardized before disease filtering (CID-10).\n",
          "Current stage: ", current_stage %||% "unknown", "\n",
          "Required stage: ", required_stage, "\n\n",
          "Please run:\n",
          "  df <- sus_data_standardize(df)"
        ),
        pt = paste0(
          "Dados devem ser padronizados antes da filtragem por doenca (CID-10).\n",
          "Estagio atual: ", current_stage %||% "desconhecido", "\n",
          "Estagio requerido: ", required_stage, "\n\n",
          "Por favor, execute:\n",
          "  df <- sus_data_standardize(df)"
        ),
        es = paste0(
          "Los datos deben estar estandarizados antes del filtrado por enfermedad (CID-10).\n",
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
        en = "Data stage validated: disease filtering (CID-10)",
        pt = "Estagio de dados validado: filtragem por doenca (CID-10)",
        es = "Etapa de datos validada: filtrado por enfermedad (CID-10)"
      )

      cli::cli_alert_success(msg_stage_ok[[lang]] %||% msg_stage_ok[["en"]])
    }

    # Update metadata
    df <- climasus_meta(df, stage = "filter_cid", type = "filter_cid")

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
          "  4. Filter disease: df <- sus_data_filter_cid(df, disease_group = 'dengue')\n\n",
          "If using external data, run sus_data_standardize() first to prepare it."
        ),
        pt = paste0(
          "Entrada nao e um objeto climasus_df.\n",
          "Esta funcao requer dados do pipeline CLIMASUS4r.\n\n",
          "Por favor, prepare seus dados primeiro:\n",
          "  1. Importar: df <- sus_data_import(...) ou sus_data_read(...)\n",
          "  2. Limpar: df <- sus_data_clean_encoding(df)\n",
          "  3. Padronizar: df <- sus_data_standardize(df)\n",
          "  4. Filtrar doencas: df <- sus_data_filter_cid(df, disease_group = 'dengue')\n\n",
          "Se usar dados externos, execute sus_data_standardize() primeiro para prepara-los."
        ),
        es = paste0(
          "La entrada no es un objeto climasus_df.\n",
          "Esta funcion requiere datos del pipeline CLIMASUS4r.\n\n",
          "Por favor, prepare sus datos primero:\n",
          "  1. Importar: df <- sus_data_import(...) o sus_data_read(...)\n",
          "  2. Limpiar: df <- sus_data_clean_encoding(df)\n",
          "  3. Estandarizar: df <- sus_data_standardize(df)\n",
          "  4. Filtrar: df <- sus_data_filter_cid(df, disease_group = 'dengue')\n\n",
          "Si usa datos externos, ejecute sus_data_standardize() primero para prepararlos."
        )
      )
      
      cli::cli_abort(msg_error[[lang]])
  }

  # Get UI messages
  ui_msg <- get_ui_messages(lang)
  
  # ============================================================================
  # Handle icd_codes vs disease_group
  # ============================================================================
  
  # Check that at least one is provided
  if (is.null(icd_codes) && is.null(disease_group)) {
    msg <- list(
      en = "Either 'icd_codes' or 'disease_group' must be provided.",
      pt = "E necessario fornecer 'icd_codes' ou 'disease_group'.",
      es = "Debe proporcionar 'icd_codes' o 'disease_group'."
    )
    cli::cli_alert_danger(msg[[lang]])
    stop("Missing required parameter.")
  }
  
  # Check that both are not provided
  if (!is.null(icd_codes) && !is.null(disease_group)) {
    msg <- list(
      en = "Both 'icd_codes' and 'disease_group' provided. Using 'disease_group' and ignoring 'icd_codes'.",
      pt = "Ambos 'icd_codes' e 'disease_group' fornecidos. Usando 'disease_group' e ignorando 'icd_codes'.",
      es = "Se proporcionaron 'icd_codes' y 'disease_group'. Usando 'disease_group' e ignorando 'icd_codes'."
    )
    cli::cli_alert_warning(msg[[lang]])
    icd_codes <- NULL  # Prioritize disease_group
  }
  
  # ============================================================================
  # Process disease_group if provided
  # ============================================================================
  
  if (!is.null(disease_group)) {
  
  # Validate ALL disease groups exist
  invalid_groups <- disease_group[!disease_group %in% names(.icd_disease_groups)]
  
  if (length(invalid_groups) > 0) {
    # Found invalid groups - show error
    available_groups <- paste(names(.icd_disease_groups), collapse = ", ")
    
    msg <- list(
      en = paste0(
        "Invalid disease group(s): '", paste(invalid_groups, collapse = "', '"), "'.\n",
        "Available groups: ", available_groups
      ),
      pt = paste0(
        "Grupo(s) de doenca invalido(s): '", paste(invalid_groups, collapse = "', '"), "'.\n",
        "Grupos disponiveis: ", available_groups
      ),
      es = paste0(
        "Grupo(s) de enfermedad invalido(s): '", paste(invalid_groups, collapse = "', '"), "'.\n",
        "Grupos disponibles: ", available_groups
      )
    )
    if (length(invalid_groups) > 0) {
      available_groups <- names(.icd_disease_groups)
      
      # Find similar group names for each invalid group
      suggestions <- character(0)
      for (inv_group in invalid_groups) {
        # Calculate string distance
        distances <- utils::adist(inv_group, available_groups)
        closest_idx <- which.min(distances)
        closest_match <- available_groups[closest_idx]
        min_distance <- distances[closest_idx]
        
        # If distance is small (likely a typo), suggest the closest match
        if (min_distance <= 3) {
          suggestions <- c(suggestions, paste0("'", inv_group, "' -> Did you mean '", closest_match, "'?"))
        }
      }
      
      # Build error message with suggestions
      available_list <- paste(available_groups, collapse = ", ")
      
      if (length(suggestions) > 0) {
        suggestion_text <- paste0("\n\nSuggestions:\n  ", paste(suggestions, collapse = "\n  "))
      } else {
        suggestion_text <- ""
      }
      
      msg <- list(
        en = paste0(
          "Invalid disease group(s): '", paste(invalid_groups, collapse = "', '"), "'.",
          suggestion_text, "\n\n",
          "Available groups: ", available_list
        ),
        pt = paste0(
          "Grupo(s) de doenca invalido(s): '", paste(invalid_groups, collapse = "', '"), "'.",
          suggestion_text, "\n\n",
          "Grupos disponiveis: ", available_list
        ),
        es = paste0(
          "Grupo(s) de enfermedad invalido(s): '", paste(invalid_groups, collapse = "', '"), "'.",
          suggestion_text, "\n\n",
          "Grupos disponibles: ", available_list
        )
      )
      
      cli::cli_abort(msg[[lang]])
    }
    
  }
  
  # All groups are valid - extract ICD codes from ALL groups
  icd_codes <- character(0)
  
  for (group in disease_group) {
    group_codes <- .icd_disease_groups[[group]]$codes
    icd_codes <- c(icd_codes, group_codes)
  }
  
  # Remove duplicates (in case groups overlap)
  icd_codes <- unique(icd_codes)
  
  if (verbose) {
    msg <- list(
      en = paste0(
        "Using disease group(s): ", paste(disease_group, collapse = ", "), "\n",
        "Total ICD codes: ", length(icd_codes)
      ),
      pt = paste0(
        "Usando grupo(s) de doenca: ", paste(disease_group, collapse = ", "), "\n",
        "Total de codigos CID: ", length(icd_codes)
      ),
      es = paste0(
        "Usando grupo(s) de enfermedad: ", paste(disease_group, collapse = ", "), "\n",
        "Total de codigos CIE: ", length(icd_codes)
      )
    )
    cli::cli_alert_info(msg[[lang]])
  }
}
  # ============================================================================
  # Auto-detect ICD column if not specified
  # ============================================================================
  
  if (is.null(icd_column)) {
    
    # Detect health system
    detected_system <- climasus_meta(df, "system")
    if(is.null(detected_system)) {detected_system <- detect_health_system(df)}

    if(lang == "en"){ 
    # Priority order by system (all 6 major SUS systems)
    icd_column_priority <- list(
      SIM = c("underlying_cause", "underlying_cause_original", "cause_line_a", "cause_line_b", "cause_line_c", "cause_line_d"),
      SIH = c("primary_diagnosis", "secondary_diagnosis", "death_cause", "notified_cause", paste0("secondary_diagnosis_", 1:9)),
      SIA = c("primary_icd", "secondary_icd", "associated_icd"),
      SINASC = c("underlying_cause", "maternal_cause", "fetal_cause"),  # Birth system - uses ICD for maternal/fetal deaths
      CNES = character(0),  # CNES does not use ICD-10 codes (establishment registry)
      SINAN = character(0)  # SINAN uses disease-specific codes, not ICD-10
    )
    } else if (lang == "pt") {
      icd_column_priority <- list(
      SIM = c("causa_basica", "causa_basica_original", "linha_causa_a", "linha_causa_b", "linha_causa_c", "linha_causa_d"),
      SIH = c("diagnostico_principal", "diagnostico_secundario", "causa_morte", "causa_notificada", paste0("diagnostico_secundario_", 1:9)),
      SIA = c("cid_principal", "cid_secundario", "cid_associado"),
      SINASC = c("causa_basica", "causa_materna", "causa_fetal"),
      CNES = character(0),  # CNES nao usa codigos CID-10 (cadastro de estabelecimentos)
      SINAN = character(0)  # SINAN usa codigos especificos de agravos, nao CID-10
    )
    } else { 
      icd_column_priority <- list(
      SIM = c("causa_basica", "causa_basica_original", "linea_causa_a", "linea_causa_b", "linea_causa_c", "linea_causa_d"),
      SIH = c("diagnostico_principal", "diagnostico_secundario", "causa_muerte", "causa_notificada", paste0("diagnostico_secundario_", 1:9)),
      SIA = c("cie_principal", "cie_secundario", "cie_asociado"),
      SINASC = c("causa_basica", "causa_materna", "causa_fetal"),
      CNES = character(0),  # CNES no usa codigos CIE-10 (registro de establecimientos)
      SINAN = character(0)  # SINAN usa codigos especificos de enfermedades, no CIE-10
    )
    }
    
    if (detected_system %in% names(icd_column_priority)) {
      possible_cols <- icd_column_priority[[detected_system]]
      icd_column <- possible_cols[possible_cols %in% names(df)][1]
    }
    
    # If still NULL, try common patterns
    if (is.null(icd_column) || is.na(icd_column)) {
      if (lang == "en") { 
        icd_pattern_cols <- grep("underlying_cause|underlying_cause_original|primary_diagnosis|secondary_diagnosis|primary_icd|secondary_icd|maternal_cause", names(df), value = TRUE, ignore.case = TRUE)
      } else if ( lang == "pt") { 
        icd_pattern_cols <- grep("causa_basica|causa_basica_original|diagnostico_principal|diagnostico_secundario|cid_principal|cid_secundario|causa_materna", names(df), value = TRUE, ignore.case = TRUE)
      } else {
        icd_pattern_cols <- grep("causa_basica|causa_basica_original|diagnostico_principal|diagnostico_secundario|cie_principal|cie_secundario|causa_materna", names(df), value = TRUE, ignore.case = TRUE)
       }
      if (length(icd_pattern_cols) > 0) {
        icd_column <- icd_pattern_cols[1]
      }
    }
    
    # If still NULL, error
    if (is.null(icd_column) || is.na(icd_column)) {
      msg <- list(
        en = "No ICD column found. This system may not use ICD-10 codes (e.g., SINAN, SINASC, CNES).",
        pt = "Nenhuma coluna CID encontrada. Este sistema pode nao usar codigos CID-10 (ex: SINAN, SINASC, CNES).",
        es = "No se encontro columna CIE. Este sistema puede no usar codigos CIE-10 (ej: SINAN, SINASC, CNES)."
      )
      cli::cli_alert_danger(msg[[lang]])
      stop("No ICD column available for filtering.")
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
  
  # Validate column exists
  if (!icd_column %in% names(df)) {
    msg <- list(
      en = paste0("Column '", icd_column, "' not found in data."),
      pt = paste0("Coluna '", icd_column, "' nao encontrada nos dados."),
      es = paste0("Columna '", icd_column, "' no encontrada en los datos.")
    )
    cli::cli_alert_danger(msg[[lang]])
    stop("ICD column not found.")
  }
  
  # ============================================================================
  # Process ICD codes and filter
  # ============================================================================
  
  # Expand ICD code ranges
  expanded_codes <- process_icd_codes(icd_codes)
  
  # Create filter condition based on match_type
  if (match_type == "exact") {
    filtered_df <- df[df[[icd_column]] %in% expanded_codes, ]
  } else if (match_type == "starts_with") {
    pattern <- paste0("^(", paste(expanded_codes, collapse = "|"), ")")
    filtered_df <- df[grepl(pattern, df[[icd_column]], ignore.case = FALSE), ]
  } else if (match_type == "range") {
    pattern <- paste(expanded_codes, collapse = "|")
    filtered_df <- df[grepl(pattern, df[[icd_column]], ignore.case = FALSE), ]
  } else {
    cli::cli_alert_warning("Invalid match_type. Using 'starts_with'.")
    pattern <- paste0("^(", paste(expanded_codes, collapse = "|"), ")")
    filtered_df <- df[grepl(pattern, df[[icd_column]], ignore.case = FALSE), ]
  }
  
  # ============================================================================
  # Report results
  # ============================================================================
  
  if (verbose) {
  b_mark <- if(lang == "en") "," else "."
  d_mark <- if(lang == "en") "." else ","
  
  
  n_orig_txt <- format(nrow(df), big.mark = b_mark, decimal.mark = d_mark)
  n_filt_txt <- format(nrow(filtered_df), big.mark = b_mark, decimal.mark = d_mark)
  pct <- round(100 * nrow(filtered_df) / nrow(df), 1)
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
}
  filtered_df <- climasus_meta(
    filtered_df,
    system = detected_system,
    stage = "filter_cid",
    type = "filter_cid"
  )

  # Add to processing history
  if (!is.null(disease_group)) {
    # Handle single or multiple disease groups
    if (length(disease_group) == 1) {
      history_msg <- sprintf("Filtered by disease group: %s", disease_group)
    } else {
      history_msg <- sprintf(
        "Filtered by disease groups: %s",
        paste(disease_group, collapse = ", ")
      )
    }
  } else {
    # Handle ICD codes
    history_msg <- sprintf(
      "Filtered by ICD codes: %s",
      paste(utils::head(icd_codes, 3), collapse = ", ")
    )
    if (length(icd_codes) > 3) {
      history_msg <- paste0(
        history_msg,
        sprintf(" (and %d more)", length(icd_codes) - 3)
      )
    }
  }

  # Add history to metadata
  filtered_df <- climasus_meta(filtered_df, add_history = history_msg)
  
  return(filtered_df)
}


#' List available disease groups
#'
#' Returns a data frame with all available disease groups, their labels,
#' and climate sensitivity.
#'
#' @param climate_sensitive_only Logical. If TRUE, returns only climate-sensitive groups.
#' @param lang Character. Language for labels ("en", "pt", "es"). Default "en".
#' @param verbose Logical. If TRUE, prints summary information. Default TRUE.
#'
#' @return A data.frame with columns: group_name, label, climate_sensitive, n_codes.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # List all groups in Portuguese
#' list_disease_groups(lang = "pt")
#' 
#' # List only climate-sensitive groups in English
#' list_disease_groups(climate_sensitive_only = TRUE, lang = "en")
#' }
list_disease_groups <- function(climate_sensitive_only = FALSE, lang = "en", verbose = TRUE) {
  
  # Validate language
  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_alert_warning("Language '{lang}' not supported. Using English (en).")
    lang <- "en"
  }
  
  # Get all group names
  all_groups <- names(.icd_disease_groups)
  
  # Filter by climate sensitivity if requested
  if (climate_sensitive_only) {
    all_groups <- all_groups[sapply(.icd_disease_groups, function(x) x$climate_sensitive)]
  }
  
  # Build data frame
  result <- data.frame(
    group_name = all_groups,
    label = sapply(all_groups, function(g) .icd_disease_groups[[g]]$label[[lang]]),
    climate_sensitive = sapply(all_groups, function(g) .icd_disease_groups[[g]]$climate_sensitive),
    n_codes = sapply(all_groups, function(g) length(.icd_disease_groups[[g]]$codes)),
    stringsAsFactors = FALSE
  )
  
  rownames(result) <- NULL
  
  # Print summary if verbose
  if (verbose) {
    msg_header <- list(
      en = "Available Disease Groups",
      pt = "Grupos de Doencas Disponiveis",
      es = "Grupos de Enfermedades Disponibles"
    )
    cli::cli_h1(msg_header[[lang]])
    
    msg_total <- list(
      en = paste0("Total groups: ", nrow(result)),
      pt = paste0("Total de grupos: ", nrow(result)),
      es = paste0("Total de grupos: ", nrow(result))
    )
    cli::cli_alert_info(msg_total[[lang]])
    
    n_climate <- sum(result$climate_sensitive)
    msg_climate <- list(
      en = paste0("Climate-sensitive groups: ", n_climate),
      pt = paste0("Grupos sensiveis ao clima: ", n_climate),
      es = paste0("Grupos sensibles al clima: ", n_climate)
    )
    cli::cli_alert_info(msg_climate[[lang]])
  }
  
  return(result)
}


#' Get disease group details
#'
#' Returns detailed information about a specific disease group including
#' ICD codes, description, and climate factors.
#'
#' @param group_name Character (e.g, "dengue", "cardiovascular"). Name of the disease group.
#' @param lang Character. Language for output ("en", "pt", "es"). Default "en".
#'
#' @return A list with detailed group information.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get details about dengue in Portuguese
#' get_disease_group_details("dengue", lang = "pt")
#' 
#' # Get details about cardiovascular diseases in English
#' get_disease_group_details("cardiovascular", lang = "en")
#' }
get_disease_group_details <- function(group_name, lang = "en") {
  
  # Validate language
  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_alert_warning("Language '{lang}' not supported. Using English (en).")
    lang <- "en"
  }
  
  # Validate group exists
  if (!group_name %in% names(.icd_disease_groups)) {
    available <- paste(names(.icd_disease_groups), collapse = ", ")
    msg <- list(
      en = paste0("Invalid disease group '", group_name, "'. Available: ", available),
      pt = paste0("Grupo invalido '", group_name, "'. Disponiveis: ", available),
      es = paste0("Grupo invalido '", group_name, "'. Disponibles: ", available)
    )
    cli::cli_alert_danger(msg[[lang]])
    stop("Invalid disease_group.")
  }
  
  # Get group info
  info <- get_disease_group_info(group_name, lang)
  
  # Print formatted output
  msg_header <- list(
    en = paste0("Disease Group: ", info$label),
    pt = paste0("Grupo de Doencas: ", info$label),
    es = paste0("Grupo de Enfermedades: ", info$label)
  )
  cli::cli_h1(msg_header[[lang]])
  
  msg_name <- list(
    en = paste0("Internal name: ", info$name),
    pt = paste0("Nome interno: ", info$name),
    es = paste0("Nombre interno: ", info$name)
  )
  cli::cli_alert_info(msg_name[[lang]])
  
  if (!is.null(info$description)) {
    msg_desc <- list(
      en = paste0("Description: ", info$description),
      pt = paste0("Descricao: ", info$description),
      es = paste0("Descripcion: ", info$description)
    )
    cli::cli_alert_info(msg_desc[[lang]])
  }
  
  msg_codes <- list(
    en = paste0("ICD codes: ", paste(info$codes, collapse = ", ")),
    pt = paste0("Codigos CID: ", paste(info$codes, collapse = ", ")),
    es = paste0("Codigos CIE: ", paste(info$codes, collapse = ", "))
  )
  cli::cli_alert_info(msg_codes[[lang]])
  
  msg_climate <- list(
    en = paste0("Climate-sensitive: ", ifelse(info$climate_sensitive, "Yes", "No")),
    pt = paste0("Sensivel ao clima: ", ifelse(info$climate_sensitive, "Sim", "Nao")),
    es = paste0("Sensible al clima: ", ifelse(info$climate_sensitive, "Si", "No"))
  )
  cli::cli_alert_info(msg_climate[[lang]])
  
  if (info$climate_sensitive && !is.null(info$climate_factors)) {
    msg_factors <- list(
      en = paste0("Climate factors: ", paste(info$climate_factors, collapse = ", ")),
      pt = paste0("Fatores climaticos: ", paste(info$climate_factors, collapse = ", ")),
      es = paste0("Factores climaticos: ", paste(info$climate_factors, collapse = ", "))
    )
    cli::cli_alert_info(msg_factors[[lang]])
  }
  
  return(invisible(info))
}


#' Helper function to process ICD code ranges
#'
#' @keywords internal
#' @noRd
process_icd_codes <- function(codes) {
  expanded <- c()
  
  for (code in codes) {
    if (grepl("-", code)) {
      parts <- strsplit(code, "-")[[1]]
      start_code <- trimws(parts[1])
      end_code <- trimws(parts[2])
      
      start_letter <- substr(start_code, 1, 1)
      start_num_str <- substr(start_code, 2, nchar(start_code))
      start_num <- if(nchar(start_num_str) > 0) as.numeric(start_num_str) else 0
      
      end_letter <- substr(end_code, 1, 1)
      end_num_str <- substr(end_code, 2, nchar(end_code))
      end_num <- if(nchar(end_num_str) > 0) as.numeric(end_num_str) else 99
      
      if (start_letter != end_letter) {
        warning(paste0("ICD range '", code, "' spans different chapters. Using start chapter only."))
        end_letter <- start_letter
      }
      
      for (num in start_num:end_num) {
        expanded <- c(expanded, paste0(start_letter, sprintf("%02d", num)))
      }
    } else {
      expanded <- c(expanded, code)
    }
  }
  
  return(unique(expanded))
}