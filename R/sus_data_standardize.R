#' Standardize SUS data column names and values
#'
#' This function standardizes column names and categorical values in SUS datasets,
#' ensuring consistency across different years and versions. It supports three languages:
#' English (en), Portuguese (pt), and Spanish (es).
#'
#' The function builds upon the preprocessing done by `microdatasus`, adding an
#' additional layer of standardization specifically designed for climate-health
#' research workflows.
#'
#' @param df A `data.frame` or `tibble` to be standardized (typically output from `sus_data_import()`).
#' @param lang Character. Output language for column names and values. Options: "en" (English), 
#'   "pt" (Portuguese, Default), "es" (Spanish).
#' @param translate_columns Logical. If TRUE, translates column names. Default is TRUE.
#' @param standardize_values Logical. If TRUE, standardizes categorical values. Default is TRUE.
#' @param keep_original Logical. If TRUE, keeps original columns alongside standardized ones. Default is FALSE.
#' @param verbose Logical. If TRUE, prints a report of standardization actions. Default is TRUE.
#'
#' @return A `data.frame` with standardized column names and values in the specified language.
#'
#' @importFrom dplyr rename mutate case_when
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Standardize to English (default)
#' df_en <- sus_data_standardize(df_raw, lang = "en")
#' 
#' # Standardize to Portuguese
#' df_pt <- sus_data_standardize(df_raw, lang = "pt")
#' 
#' # Standardize to Spanish
#' df_es <- sus_data_standardize(df_raw, lang = "es")
#' 
#' # Keep original columns for comparison
#' df_both <- sus_data_standardize(
#'   df_raw,
#'   lang = "pt",
#'   keep_original = TRUE
#' )
#'
#' # Only translate column names (not values)
#' df_cols_only <- sus_data_standardize(
#'   df_raw,
#'   lang = "en",
#'   translate_columns = TRUE,
#'   standardize_values = FALSE
#' )
#'
#' # Complete pipeline
#' df_analysis_ready <- sus_data_import(uf = "SP", year = 2023, system = "SIM-DO") |>
#'   sus_data_clean_encoding() |>
#'   sus_data_standardize(lang = "pt")
#' }
#' @references
#' Brazilian Ministry of Health. DATASUS. \url{http://datasus.saude.gov.br}
#' 
#' SALDANHA, Raphael de Freitas; BASTOS, Ronaldo Rocha; BARCELLOS, Christovam. Microdatasus: pacote para download e pre-processamento de microdados do Departamento de Informatica do SUS (DATASUS). Cad. Saude Publica, Rio de Janeiro , v. 35, n. 9, e00032419, 2019. Available from https://doi.org/10.1590/0102-311x00032419.

sus_data_standardize <- function(df,
                                   lang = "pt",
                                   translate_columns = TRUE,
                                   standardize_values = TRUE,
                                   keep_original = FALSE,
                                   verbose = TRUE) {
  
  # Validate inputs
  if (!is.data.frame(df)) {
    cli::cli_abort("Input must be a data frame")
  }
  
  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_abort("Language must be 'en', 'pt', or 'es'")
  }
  
  if (nrow(df) == 0) {
    if (verbose) cli::cli_alert_warning("Empty data frame - returning as is")
    return(df)
  }
  
  # Get UI messages
  messages <- get_ui_messages_standardize(lang)
  
  # Detect health system
  system <- detect_health_system(df)
  
  if (verbose) {
    cli::cli_h2(messages$standardize_title)
    cli::cli_alert_info(paste(messages$detected_system, get_system_description(system, lang)))
    cli::cli_alert_info(paste(messages$language, toupper(lang)))
  }
  
  # Get translation dictionaries based on system
  
  if (system == "SIM") {
    switch(lang,
      "en" = translations <- get_translation_dict_en(),
      "pt" = translations <- get_translation_dict_pt(),
      # default case (any other language)
      translations <- get_translation_dict_es()
    )
  }
  if (system == "SINAN") {
    switch(lang,
      "en" = translations <- get_translation_dict_en_sinan(),
      "pt" = translations <- get_translation_dict_pt_sinan(),
      # default case (any other language)
      translations <- get_translation_dict_es_sinan()
    )
  }

  if (system == "SIH") {
    switch(lang,
      "en" = translations <- get_translation_dict_en_sih(),
      "pt" = translations <- get_translation_dict_pt_sih(),
      # default case (any other language)
      translations <- get_translation_dict_es_sih()
    )
  }

  if (system == "SIA") {
    switch(lang,
      "en" = translations <- get_translation_dict_en_sia(),
      "pt" = translations <- get_translation_dict_pt_sia(),
      # default case (any other language)
      translations <- get_translation_dict_es_sia()
    )
  }
 

  
  
  # ============================================================================
  # STEP 1: Translate Column Names
  # ============================================================================
  
  if (translate_columns) {
    if (verbose) cli::cli_alert_info(messages$translating_columns)
    
    original_names <- names(df)
    new_names <- original_names
    
    # Translate column names
    for (i in seq_along(original_names)) {
      orig_name <- original_names[i]
      if (orig_name %in% names(translations$columns)) {
        new_names[i] <- translations$columns[[orig_name]]
      }
    }
    
    # Count translations
    n_translated <- sum(original_names != new_names)
    
    if (keep_original) {
      # Create new columns with translated names
      for (i in seq_along(original_names)) {
        if (original_names[i] != new_names[i]) {
          df[[new_names[i]]] <- df[[original_names[i]]]
        }
      }
    } else {
      # Rename columns in place
      names(df) <- new_names
    }
    
    if (verbose) {
      cli::cli_alert_success(paste(
        messages$columns_translated,
        n_translated,
        "of",
        length(original_names)
      ))
    }
  }
  
  # ============================================================================
  # STEP 2: Translate Categorical Values
  # ============================================================================
  
  if (standardize_values) {
    if (verbose) cli::cli_alert_info(messages$translating_values)
    
    current_names <- names(df)
    n_variables_translated <- 0
    n_values_translated <- 0
    
    # Translate categorical values
    for (var_name in names(translations$values)) {
      # Check if variable exists in data (original or translated name)
      col_name <- NULL
      
      # Try original name
      if (var_name %in% current_names) {
        col_name <- var_name
      }
      # Try translated name
      else if (var_name %in% names(translations$columns)) {
        translated_name <- translations$columns[[var_name]]
        if (translated_name %in% current_names) {
          col_name <- translated_name
        }
      }
      
      if (!is.null(col_name)) {
        # Get value mappings
        value_map <- translations$values[[var_name]]
        
        # Convert column to character for mapping
        original_values <- as.character(df[[col_name]])
        new_values <- original_values
        
        # Apply mappings
        for (code in names(value_map)) {
          new_values[original_values == code] <- value_map[[code]]
        }
        
        # Count translations
        n_changed <- sum(original_values != new_values, na.rm = TRUE)
        
        if (n_changed > 0) {
          if (keep_original) {
            # Create new column with "_translated" suffix
            new_col_name <- paste0(col_name, "_translated")
            df[[new_col_name]] <- as.factor(new_values)
          } else {
            # Replace original column
            df[[col_name]] <- as.factor(new_values)
          }
          
          n_variables_translated <- n_variables_translated + 1
          n_values_translated <- n_values_translated + n_changed
        }
      }
    }
    
    if (verbose) {
      cli::cli_alert_success(paste(
        messages$values_translated,
        n_variables_translated,
        "variables,",
        n_values_translated,
        "values"
      ))
    }
  }
  # ============================================================================
  # STEP 3: Datetime formating 
  # ============================================================================
  if(lang == "en"){
    date_patterns <- c("_date")
    } else if (lang == "pt") {
    date_patterns <- c("data_")
    } else {
    date_patterns <- c("fecha_")
    }
  
  cols_to_fix <- names(df)[grepl(paste(date_patterns, collapse = "|"), names(df), ignore.case = TRUE)]

  if(length(cols_to_fix) > 0) {
    
    for (col in cols_to_fix) {
      vals <- as.character(df[[col]])
      suppressWarnings({
        parsed_dates <- lubridate::parse_date_time(vals, 
          orders = c("dmy", "dmY", "ymd", "Ymd", "dmy HMS", "ymd HMS", "ym", "my"),
          truncated = 3
        )
      })
      if (!all(is.na(parsed_dates)) || all(is.na(vals))) {
         df[[col]] <- as.Date(parsed_dates) 
      }
      }
  }

  # ============================================================================
  # STEP 4: Final Summary
  # ============================================================================
  
  if (verbose) {
    cli::cli_alert_success(paste(
      messages$standardization_complete,
      nrow(df),
      "rows,",
      ncol(df),
      "columns"
    ))
  }
  
  return(df)
}


# ============================================================================
# Helper function: Get UI messages in different languages
# ============================================================================

#' Get UI Messages
#'
#' Returns user interface messages in the specified language
#'
#' @param lang Character. Language code: "en", "pt", "es"
#'
#' @return List of translated UI messages
#'
#' @keywords internal
get_ui_messages_standardize <- function(lang = "en") {
  
  messages <- list(
    en = list(
      standardize_title = "Standardizing SUS Data",
      detected_system = "Detected system:",
      language = "Language:",
      translating_columns = "Translating column names...",
      columns_translated = "Translated",
      translating_values = "Translating categorical values...",
      values_translated = "Translated",
      standardization_complete = "Standardization complete:",
      no_translations = "No translations available for this system"
    ),
    pt = list(
      standardize_title = "Padronizando Dados SUS",
      detected_system = "Sistema detectado:",
      language = "Idioma:",
      translating_columns = "Traduzindo nomes de colunas...",
      columns_translated = "Traduzidas",
      translating_values = "Traduzindo valores categoricos...",
      values_translated = "Traduzidas",
      standardization_complete = "Padronizacao completa:",
      no_translations = "Nenhuma traducao disponivel para este sistema"
    ),
    es = list(
      standardize_title = "Estandarizando Datos SUS",
      detected_system = "Sistema detectado:",
      language = "Idioma:",
      translating_columns = "Traduciendo nombres de columnas...",
      columns_translated = "Traducidas",
      translating_values = "Traduciendo valores categoricos...",
      values_translated = "Traducidas",
      standardization_complete = "Estandarizacion completa:",
      no_translations = "No hay traducciones disponibles para este sistema"
    )
  )
  
  return(messages[[lang]])
}