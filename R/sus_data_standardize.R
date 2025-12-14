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
#' @param lang Character. Output language for column names and values. Options: "en" (English, default), 
#'   "pt" (Portuguese), "es" (Spanish).
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
#' df_compare <- sus_data_standardize(df_raw, lang = "pt", keep_original = TRUE)
#' }
sus_data_standardize <- function(df, 
                                lang = "en",
                                translate_columns = TRUE,
                                standardize_values = TRUE,
                                keep_original = FALSE,
                                verbose = TRUE) {
  
  # Input validation
  if (!is.data.frame(df)) {
    cli::cli_alert_danger("Input 'df' must be a data.frame.")
    stop("Invalid input type.")
  }
  
  # Validate and get translation dictionary
  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_alert_warning("Language '{lang}' not supported. Using English (en).")
    lang <- "en"
  }
  
  trans_dict <- get_translation_dict(lang)
  ui_msg <- get_ui_messages(lang)
  
  if (verbose) {
    cli::cli_h1(ui_msg$standardization_header)
    cli::cli_alert_info("{ui_msg$original_columns}: {ncol(df)}")
    cli::cli_alert_info("{ui_msg$original_records}: {format(nrow(df), big.mark = ',')}")
  }
  
  # Store original column names for reporting
  original_cols <- names(df)
  
  # ============================================================================
  # STEP 1: Column Name Translation
  # ============================================================================
  
  if (translate_columns) {
    
    renamed_cols <- 0
    
    for (old_name in names(trans_dict$columns)) {
      if (old_name %in% names(df)) {
        new_name <- trans_dict$columns[[old_name]]
        
        if (keep_original) {
          # Keep original and create new column
          df[[new_name]] <- df[[old_name]]
        } else {
          # Rename in place
          names(df)[names(df) == old_name] <- new_name
        }
        renamed_cols <- renamed_cols + 1
      }
    }
    
    if (verbose && renamed_cols > 0) {
      lang_names <- list(en = "English", pt = "Portugues", es = "Espanol")
      cli::cli_alert_success("{ui_msg$translated_columns} {lang_names[[lang]]}: {renamed_cols} {if (lang == 'en') 'columns' else if (lang == 'pt') 'colunas' else 'columnas'}")
    }
  }
  
  # ============================================================================
  # STEP 2: Value Standardization
  # ============================================================================
  
  if (standardize_values) {
    
    standardized_vars <- 0
    
    # Helper function to translate values
    translate_values <- function(values, var_type) {
      if (is.null(trans_dict$values[[var_type]][[lang]])) {
        return(values)
      }
      
      translation_map <- trans_dict$values[[var_type]][[lang]]
      
      # Convert to character for matching
      values_char <- as.character(values)
      
      # Apply translations
      translated <- sapply(values_char, function(v) {
        if (is.na(v)) return(NA_character_)
        if (v %in% names(translation_map)) {
          return(translation_map[[v]])
        }
        return(v)
      }, USE.NAMES = FALSE)
      
      return(translated)
    }
    
    # Sex standardization
    sex_cols <- c("sex", "sexo", "SEXO")
    sex_col <- sex_cols[sex_cols %in% names(df)][1]
    
    if (!is.na(sex_col)) {
      df[[sex_col]] <- translate_values(df[[sex_col]], "sex")
      standardized_vars <- standardized_vars + 1
    }
    
    # Race/color standardization
    race_cols <- c("race", "raca_cor", "raza_color", "RACACOR")
    race_col <- race_cols[race_cols %in% names(df)][1]
    
    if (!is.na(race_col)) {
      df[[race_col]] <- translate_values(df[[race_col]], "race")
      standardized_vars <- standardized_vars + 1
    }
    
    # Death type standardization
    death_type_cols <- c("death_type", "tipo_obito", "tipo_muerte", "TIPOBITO")
    death_type_col <- death_type_cols[death_type_cols %in% names(df)][1]
    
    if (!is.na(death_type_col)) {
      df[[death_type_col]] <- translate_values(df[[death_type_col]], "death_type")
      standardized_vars <- standardized_vars + 1
    }
    
    # Marital status standardization
    marital_cols <- c("marital_status", "estado_civil", "ESTCIV")
    marital_col <- marital_cols[marital_cols %in% names(df)][1]
    
    if (!is.na(marital_col)) {
      df[[marital_col]] <- translate_values(df[[marital_col]], "marital_status")
      standardized_vars <- standardized_vars + 1
    }
    
    if (verbose && standardized_vars > 0) {
      cli::cli_alert_success("{ui_msg$standardized_values}: {standardized_vars}")
    }
  }
  
  # ============================================================================
  # STEP 3: Final Report
  # ============================================================================
  
  if (verbose) {
    cli::cli_alert_success("{ui_msg$standardization_completed}")
    cli::cli_alert_info("{ui_msg$final_columns}: {ncol(df)}")
  }
  
  return(df)
}
