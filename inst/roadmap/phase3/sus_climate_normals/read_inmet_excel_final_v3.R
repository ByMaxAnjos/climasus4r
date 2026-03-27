# Read INMET Excel file with proper header handling, type coercion, and pivot_longer transformation
.read_inmet_excel <- function(file_path, var_code) {
  
  tryCatch({
    # Step 1: Read rows 3 and 4 to extract month names and decendial indicators
    header_months <- readxl::read_excel(
      file_path, 
      sheet = 1, 
      skip = 2, 
      n_max = 1, 
      col_names = FALSE
    )
    
    header_decades <- readxl::read_excel(
      file_path, 
      sheet = 1, 
      skip = 3, 
      n_max = 1, 
      col_names = FALSE
    )
    
    # Step 2: Read the actual data (starting from row 5)
    data <- readxl::read_excel(
      file_path, 
      sheet = 1, 
      skip = 4,
      col_names = FALSE
    )
    
    # Step 3: Build proper column names
    # First three columns are: Código, Nome da Estação, UF
    col_names <- c("codigo", "nome_estacao", "uf")
    
    # For the remaining columns, combine month name with decendial indicator
    for (i in 4:ncol(data)) {
      month_name <- as.character(header_months[[1, i]])
      decade_indicator <- as.character(header_decades[[1, i]])
      
      # Handle NA or empty values - propagate month name forward
      if (is.na(month_name) || month_name == "" || month_name == "NA") {
        # Look back to find the last non-empty month name
        for (j in (i-1):4) {
          prev_month <- as.character(header_months[[1, j]])
          if (!is.na(prev_month) && prev_month != "" && prev_month != "NA") {
            month_name <- prev_month
            break
          }
        }
      }
      
      # Clean month name
      month_clean <- month_name %>%
        stringi::stri_trans_general("Latin-ASCII") %>%
        stringi::stri_replace_all_regex("[^[:alnum:]]+", "_") %>%
        stringi::stri_trans_tolower() %>%
        stringi::stri_replace_all_regex("^_|_$", "")
      
      # Clean decade indicator - keep only the numeric part
      decade_clean <- decade_indicator %>%
        stringi::stri_trans_general("Latin-ASCII") %>%
        stringi::stri_replace_all_regex("[^0-9]", "") %>%
        stringi::stri_trans_tolower()
      
      # Combine month and decade
      col_name <- paste0(month_clean, "_", decade_clean)
      col_names <- c(col_names, col_name)
    }
    
    # Step 4: Assign column names
    colnames(data) <- col_names
    
    # Step 5: Ensure codigo is character
    data$codigo <- as.character(data$codigo)
    
    # Step 6: Convert all value columns to character first to handle mixed types
    # This ensures consistent handling of numeric and non-numeric values (like "-")
    for (i in 4:ncol(data)) {
      data[[i]] <- as.character(data[[i]])
    }
    
    # Step 7: Convert to tibble
    data <- dplyr::as_tibble(data)
    
    # Step 8: Pivot longer to transform from wide to long format
    data_long <- data %>%
      tidyr::pivot_longer(
        cols = -c(codigo, nome_estacao, uf),
        names_to = "mes_decada",
        values_to = "valor"
      )
    
    # Step 9: Extract month and decade from the combined column
    # Use a more robust regex that captures the month name and the trailing numbers
    data_long <- data_long %>%
      tidyr::separate(
        mes_decada,
        into = c("mes", "decada"),
        sep = "_(?=[0-9]+$)",  # Split before the LAST sequence of digits
        remove = TRUE
      ) %>%
      dplyr::mutate(
        # Convert valor to numeric, treating "-" as NA
        valor = suppressWarnings(as.numeric(
          stringi::stri_replace_all_regex(valor, "^-$", NA_character_)
        ))
      )
    
    # Step 10: Add variable code
    data_long$var_code <- var_code
    
    # Step 11: Reorder columns for better readability
    data_long <- data_long %>%
      dplyr::select(codigo, nome_estacao, uf, mes, decada, valor, var_code)
    
    return(data_long)
    
  }, error = function(e) {
    warning(paste("Error reading Excel file:", e$message))
    return(NULL)
  })
}
