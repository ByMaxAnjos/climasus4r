#' @title Calculate Socio-demographic and Health Indicators (Lazy & Robust)
#'
#' @description
#' Calculates indicators based on the `INDICATORS_LIST` metadata.
#' This function is "Lazy" because it only calculates what is requested and
#' "Robust" because it validates columns and handles statistical uncertainty.
#'
#' @param df A data frame (or sf object) already enriched with census and health data.
#' @param indicators Character vector of indicator IDs to calculate.
#' @param col_mapping Optional list to map concept names to actual column names.
#' @param confidence_level Numeric. Level for confidence intervals (default 0.95).
#' @param lang Language for messages: "pt" (default), "en", or "es".
#'
#' @return The input data frame with new columns for the calculated indicators
#'   and their confidence intervals (if applicable).
#'
#' @importFrom dplyr mutate across all_of
#' @importFrom rlang parse_expr eval_tidy
#' @export
sus_socio_indicators <- function(df,
                                 indicators = NULL,
                                 col_mapping = NULL,
                                 confidence_level = 0.95,
                                 lang = "pt") {
  
  # 1. Load Metadata
  source("/home/ubuntu/climasus4r/R/indicators_list_v4.R")
  
  # 2. Multilingual Messages
  msgs <- list(
    pt = list(start = "Iniciando calculo de indicadores...", missing = "Colunas ausentes para: ", success = "Indicadores calculados com sucesso."),
    en = list(start = "Starting indicator calculation...", missing = "Missing columns for: ", success = "Indicators calculated successfully."),
    es = list(start = "Iniciando calculo de indicadores...", missing = "Columnas ausentes para: ", success = "Indicadores calculados con exito.")
  )[[lang]]
  
  message(msgs$start)
  
  # 3. Default Indicators
  if (is.null(indicators)) indicators <- names(INDICATORS_LIST)
  
  # 4. Calculation Loop
  for (id in indicators) {
    meta <- INDICATORS_LIST[[id]]
    if (is.null(meta)) next
    
    # Check required columns (with mapping)
    req_cols <- meta$required_cols
    actual_cols <- req_cols
    if (!is.null(col_mapping)) {
      for (i in seq_along(req_cols)) {
        if (req_cols[i] %in% names(col_mapping)) {
          actual_cols[i] <- col_mapping[[req_cols[i]]]
        }
      }
    }
    
    if (!all(actual_cols %in% names(df))) {
      warning(paste0(msgs$missing, id))
      next
    }
    
    # Lazy Evaluation of Formula
    formula_str <- meta$formula
    # Replace concept names with actual column names in the formula string
    for (i in seq_along(req_cols)) {
      formula_str <- gsub(req_cols[i], actual_cols[i], formula_str)
    }
    
    # Calculate Indicator
    expr <- rlang::parse_expr(formula_str)
    df[[paste0("ind_", id)]] <- rlang::eval_tidy(expr, data = df) * meta$multiplier
    
    # 5. Robust Uncertainty Calculation
    if (meta$uncertainty_method != "None") {
      # Simple Poisson/Binomial implementation for CIs
      # (In a real package, we would use more robust methods like epitools or survey)
      z <- qnorm(1 - (1 - confidence_level) / 2)
      
      if (meta$uncertainty_method == "Poisson") {
        # Numerator is count, Denominator is population/live births
        num_col <- actual_cols[1]
        den_col <- actual_cols[2]
        
        # Standard Error of the Rate
        se <- (sqrt(df[[num_col]]) / df[[den_col]]) * meta$multiplier
        df[[paste0("ind_", id, "_low")]] <- df[[paste0("ind_", id)]] - z * se
        df[[paste0("ind_", id, "_high")]] <- df[[paste0("ind_", id)]] + z * se
      }
      
      if (meta$uncertainty_method == "Binomial") {
        # Proportion p = n/N
        p <- df[[paste0("ind_", id)]] / meta$multiplier
        n <- df[[actual_cols[2]]] # Total population/households
        se <- sqrt((p * (1 - p)) / n) * meta$multiplier
        df[[paste0("ind_", id, "_low")]] <- df[[paste0("ind_", id)]] - z * se
        df[[paste0("ind_", id, "_high")]] <- df[[paste0("ind_", id)]] + z * se
      }
      
      # Ensure CI bounds are logical (e.g., not negative)
      df[[paste0("ind_", id, "_low")]] <- pmax(0, df[[paste0("ind_", id, "_low")]])
    }
  }
  
  message(msgs$success)
  return(df)
}
