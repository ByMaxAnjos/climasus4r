#' @title Calculate Standardized Health and Socio-demographic Indicators (v3.0)
#'
#' @description
#' Calculates standardized health and socio-demographic indicators from a single,
#' enriched dataframe. This function assumes the input dataframe has already been
#' enriched with Census data (e.g., via `sus_socio_add_census`).
#'
#' @param df A dataframe (or sf object) already enriched with all necessary
#'   columns (health aggregates, population, census variables).
#' @param indicators A character vector of indicator names to calculate.
#' @param lang Language for the output and messages: "pt" (default), "en", or "es".
#' @param col_mapping A named list to override default column names required by the
#'   indicators. E.g., `list(pop_total = "my_custom_pop_column")`.
#'
#' @return A tibble (or sf object) with the calculated indicators appended as new columns.
#'
#' @details
#' The function operates on the principle of "Variable Mapping," where it checks for the
#' existence of required columns (numerator, denominator, or formula components)
#' in the input dataframe before calculation.
#'
#' @export
sus_socio_indicators <- function(df,
                                 indicators,
                                 lang = "pt",
                                 col_mapping = NULL) {

  # --- 1. Setup and Validation ---
  lang <- match.arg(lang, c("pt", "en", "es"))
  
  # Load internal indicators definition (INDICATORS_LIST)
  source("/home/ubuntu/climasus4r/R/indicators_list_v3.R")

  # Check if all requested indicators exist in the list
  if (!all(indicators %in% names(INDICATORS_LIST))) {
    stop(sprintf("One or more requested indicators are not recognized: %s",
                 paste(indicators[!indicators %in% names(INDICATORS_LIST)], collapse = ", ")))
  }

  # --- 2. Apply Column Mapping Overrides ---
  # This is CRITICAL for user flexibility
  indicator_defs <- INDICATORS_LIST[indicators]
  
  if (!is.null(col_mapping)) {
    for (i in seq_along(indicator_defs)) {
      ind_name <- names(indicator_defs)[i]
      
      # Iterate over required_vars (numerator, denominator, pop_young, etc.)
      for (var_name in names(indicator_defs[[i]]$required_vars)) {
        default_col <- indicator_defs[[i]]$required_vars[[var_name]]
        
        # Check if the user provided an override for this default column name
        if (default_col %in% names(col_mapping)) {
          indicator_defs[[i]]$required_vars[[var_name]] <- col_mapping[[default_col]]
        }
      }
    }
  }

  # --- 3. Indicator Calculation Loop ---
  df_result <- df # Start with the input dataframe

  for (ind_name in names(indicator_defs)) {
    ind_def <- indicator_defs[[ind_name]]
    
    # Check for required columns
    required_cols <- unlist(ind_def$required_vars)
    missing_cols <- required_cols[!required_cols %in% names(df_result)]
    
    if (length(missing_cols) > 0) {
      warning(sprintf("Skipping indicator '%s'. Missing required columns: %s",
                      ind_name, paste(missing_cols, collapse = ", ")))
      next
    }
    
    # Determine the name of the new column
    new_col_name <- paste0("ind_", ind_name)
    
    # --- 4. Apply Formula ---
    if (!is.null(ind_def$formula)) {
      # Formula-based calculation (e.g., Dependency Ratio)
      
      # Replace conceptual names in formula with actual column names
      formula_str <- ind_def$formula
      for (var_name in names(ind_def$required_vars)) {
        actual_col <- ind_def$required_vars[[var_name]]
        # Use string replacement to substitute the conceptual name with the actual column name
        formula_str <- gsub(var_name, actual_col, formula_str, fixed = TRUE)
      }
      
      # Evaluate the formula within the dataframe context
      df_result <- df_result %>%
        dplyr::mutate(
          !!new_col_name := eval(parse(text = formula_str))
        )
      
    } else {
      # Numerator/Denominator based calculation (e.g., Mortality Rate)
      
      # Get actual column names
      num_col <- ind_def$required_vars$numerator
      den_col <- ind_def$required_vars$denominator
      multiplier <- ind_def$multiplier
      
      # Calculate the rate
      df_result <- df_result %>%
        dplyr::mutate(
          !!new_col_name := (.data[[num_col]] / .data[[den_col]]) * multiplier
        )
    }
    
    # Add metadata to the new column (optional, for future use)
    attr(df_result[[new_col_name]], "indicator_name") <- ind_def$name_pt
    attr(df_result[[new_col_name]], "unit") <- ind_def$unit
  }

  # --- 5. Final Cleanup and Return ---
  # Add language-specific messages here if needed
  
  return(df_result)
}
