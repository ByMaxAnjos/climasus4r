#' Standardize SUS data column names and values in Arrow Dataset (lazy)
#'
#' This function applies standardization to column names and categorical values
#' in an Arrow Dataset without materializing the data. Transformations are stored
#' as Arrow compute expressions and applied during the final collect().
#'
#' @param dataset An Arrow Dataset (climasus_dataset)
#' @param lang Character. Output language for column names and values. 
#' @param translate_columns Logical. If TRUE, translates column names. Default is TRUE.
#' @param standardize_values Logical. If TRUE, standardizes categorical values. Default is TRUE.
#' @param keep_original Logical. If TRUE, keeps original columns. Default is FALSE.
#' @param verbose Logical. If TRUE, prints a report of standardization actions.
#'
#' @return An Arrow Dataset with standardization applied lazily
#' @export
sus_data_standardize_arrow <- function(dataset,
                                        lang = "pt",
                                        translate_columns = TRUE,
                                        standardize_values = TRUE,
                                        keep_original = FALSE,
                                        verbose = TRUE) {
  
  # Input validation
  if (!inherits(dataset, "Dataset") && !inherits(dataset, "climasus_dataset")) {
    cli::cli_abort("Input must be an Arrow Dataset")
  }
  
  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_abort("Language must be 'en', 'pt', or 'es'")
  }
  
  # Detect health system from metadata or schema
  system <- NULL
  if (inherits(dataset, "climasus_dataset")) {
    meta <- attr(dataset, "sus_meta")
    system <- meta$system
  }
  
  if (is.null(system)) {
    # Try to detect from column names
    schema <- arrow::schema(dataset)
    cols <- names(schema)
    system <- detect_health_system_from_colnames(cols)
  }
  
  if (verbose) {
    messages <- get_ui_messages_standardize(lang)
    cli::cli_h1(paste(messages$standardize_title, "(Arrow Lazy)"))
    cli::cli_alert_info(paste(messages$detected_system, 
                               get_system_description(system, lang)))
    cli::cli_alert_info(paste(messages$language, toupper(lang)))
  }
  
  # Get translation dictionaries
  translations <- get_translations_for_system(system, lang)
  
  # Store standardization plan as attributes
  standardization_plan <- list(
    system = system,
    lang = lang,
    translate_columns = translate_columns,
    standardize_values = standardize_values,
    keep_original = keep_original,
    column_mapping = NULL,
    value_mappings = NULL,
    applied = Sys.time()
  )
  
  # Create column renaming plan
  if (translate_columns && !is.null(translations$columns)) {
    current_cols <- names(arrow::schema(dataset))
    col_mapping <- list()
    
    for (orig in names(translations$columns)) {
      if (orig %in% current_cols) {
        new_name <- translations$columns[[orig]]
        col_mapping[[orig]] <- new_name
      }
    }
    
    standardization_plan$column_mapping <- col_mapping
    
    if (verbose && length(col_mapping) > 0) {
      cli::cli_alert_success(
        paste(messages$columns_translated, 
              length(col_mapping), 
              "columns will be renamed")
      )
    }
  }
  
  # Create value mapping plan
  if (standardize_values && !is.null(translations$values)) {
    value_mappings <- list()
    
    for (var_name in names(translations$values)) {
      # Find the column name (original or translated)
      col_name <- NULL
      if (var_name %in% names(standardization_plan$column_mapping)) {
        col_name <- standardization_plan$column_mapping[[var_name]]
      } else if (var_name %in% names(arrow::schema(dataset))) {
        col_name <- var_name
      }
      
      if (!is.null(col_name)) {
        value_mappings[[col_name]] <- translations$values[[var_name]]
      }
    }
    
    standardization_plan$value_mappings <- value_mappings
    
    if (verbose && length(value_mappings) > 0) {
      cli::cli_alert_success(
        paste(messages$values_translated,
              length(value_mappings),
              "variables with value mappings")
      )
    }
  }
  
  # Store standardization plan as attribute
  dataset <- structure(
    dataset,
    standardization_plan = standardization_plan,
    needs_standardization = TRUE,
    class = c("climasus_dataset_standardize", class(dataset))
  )
  
  # Update metadata
  if (inherits(dataset, "climasus_dataset")) {
    meta <- attr(dataset, "sus_meta")
    meta$stage <- "standardize"
    meta$system <- system
    meta$modified <- Sys.time()
    meta$history <- c(meta$history,
                      sprintf("[%s] Standardization plan prepared (%s)",
                              format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                              toupper(lang)))
    attr(dataset, "sus_meta") <- meta
  }
  
  if (verbose) {
    cli::cli_alert_success("Standardization will be applied lazily during collect()")
  }
  
  return(dataset)
}

#' Detect health system from column names
#' @noRd
detect_health_system_from_colnames <- function(colnames) {
  # SIM indicators
  if (any(grepl("DTOBITO|CAUSABAS|SEXO|IDADE", colnames))) {
    if (any(grepl("CODMUNRES|CODMUNOCOR", colnames))) {
      return("SIM")
    }
  }
  
  # SIH indicators
  if (any(grepl("DIAG_PRINC|VAL_TOT|UF_ZI|ANO_CMPT", colnames))) {
    return("SIH")
  }
  
  # SINAN indicators
  if (any(grepl("SG_UF_NOT|DT_NOTIFIC|CLASSI_FIN", colnames))) {
    return("SINAN")
  }
  
  # SINASC indicators
  if (any(grepl("IDADEMAE|PESO|APGAR1|CONSULTAS", colnames))) {
    return("SINASC")
  }
  
  # CNES indicators
  if (any(grepl("CNES|CODUFMUN|ESTAB|ATIVIDAD", colnames))) {
    return("CNES")
  }
  
  # SIA indicators
  if (any(grepl("PA|PROC_ID|CNS_PAC|MUN_PAC", colnames))) {
    return("SIA")
  }
  
  return("UNKNOWN")
}

#' Get translations for specific system and language
#' @noRd
get_translations_for_system <- function(system, lang) {
  # This function should call the appropriate translation dictionaries
  # from your existing code
  
  switch(system,
    "SIM" = {
      switch(lang,
        "en" = get_translation_dict_en(),
        "pt" = get_translation_dict_pt(),
        get_translation_dict_es()
      )
    },
    "SINAN" = {
      switch(lang,
        "en" = get_translation_dict_en_sinan(),
        "pt" = get_translation_dict_pt_sinan(),
        get_translation_dict_es_sinan()
      )
    },
    "SIH" = {
      switch(lang,
        "en" = get_translation_dict_en_sih(),
        "pt" = get_translation_dict_pt_sih(),
        get_translation_dict_es_sih()
      )
    },
    "SIA" = {
      switch(lang,
        "en" = get_translation_dict_en_sia(),
        "pt" = get_translation_dict_pt_sia(),
        get_translation_dict_es_sia()
      )
    },
    "CNES" = {
      switch(lang,
        "en" = get_translation_dict_en_cnes(),
        "pt" = get_translation_dict_pt_cnes(),
        get_translation_dict_es_cnes()
      )
    },
    "SINASC" = {
      switch(lang,
        "en" = get_translation_dict_en_sinasc(),
        "pt" = get_translation_dict_pt_sinasc(),
        get_translation_dict_es_sinasc()
      )
    },
    # Default empty translations
    list(columns = list(), values = list())
  )
}