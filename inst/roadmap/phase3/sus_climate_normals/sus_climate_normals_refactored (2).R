#' Download and Process Climate Normals Data from INMET
#'
#' @description
#' This function downloads climate normals data from the Brazilian National Institute
#' of Meteorology (INMET) for specified periods and variables.
#'
#' @param period A character string specifying the climate normals period.
#'   Valid options are "1961-1990", "1981-2010", or "1991-2020".
#'   Defaults to "1991-2020".
#'
#' @param target_var A character vector of variable codes to download.
#'   If NULL (default), all available variables for the period are downloaded.
#'
#' @param lang A character string specifying the language for messages.
#'   Supported options are "pt" (Portuguese), "en" (English), and "es" (Spanish).
#'   Defaults to "en".
#'
#' @param use_cache A logical value. If TRUE (default), uses cached data.
#'
#' @param verbose A logical value. If TRUE (default), displays progress messages.
#'
#' @return A tibble containing the downloaded climate normals data.
#'
#' @importFrom dplyr tibble bind_rows filter select
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer
#' @importFrom cli cli_alert_info cli_alert_success
#' @importFrom janitor clean_names
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   normals_data <- sus_climate_normals(period = "1991-2020", lang = "en")
#' }
sus_climate_normals <- function(period = "1991-2020",
                                target_var = NULL,
                                lang = "en",
                                cache_dir = "~/.climasus4r_cache/normals",
                                use_cache = TRUE,
                                verbose = TRUE) {

  lang <- tolower(lang)
  if (!lang %in% c("pt", "en", "es")) {lang <- "pt" }

  if (!period %in% c("1961-1990", "1981-2010", "1991-2020")) {
    cli::cli_abort("Invalid period. Must be one of: '1961-1990', '1981-2010', '1991-2020'")
  }

  messages <- .get_messages(lang)

  if (verbose) {cli::cli_alert_info(messages$loading_dictionary)}

  dictionary <- dplyr::as_tibble(normas_df)

  period_vars <- dictionary %>% dplyr::filter(.data$period == !!period)

  if (nrow(period_vars) == 0) {cli::cli_abort(paste("No variables found for period:", period))}

  if (is.null(target_var)) {
    vars_to_download <- period_vars
  } else {
    vars_to_download <- period_vars %>%
      dplyr::filter(.data$var_code %in% target_var)

    if (nrow(vars_to_download) == 0) {
      stop("No matching variables found.")
    }
  }

  if (verbose) {cli::cli_alert_info(paste(messages$found_variables, nrow(vars_to_download)))}

  all_data <- list()

  for (i in seq_len(nrow(vars_to_download))) {
    var_row <- vars_to_download[i, ]
    var_code <- var_row$var_code
    code_link <- var_row$code_link

    if (verbose) {
      cli::cli_alert_info(
        paste(messages$downloading, var_row[[paste0("variable_", lang)]])
      )
    }

    var_data <- .download_and_cache_normals(
      code_link = code_link,
      var_code = var_code,
      period = period,
      use_cache = use_cache,
      cache_dir = cache_dir,
      verbose = verbose,
      lang = lang
    )

    if (!is.null(var_data) && nrow(var_data) > 0) {
      all_data[[var_code]] <- var_data
    }
  }

  if (length(all_data) == 0) {
    stop(messages$no_data_downloaded)
  }

  result <- dplyr::bind_rows(all_data, .id = "variable")

  if (verbose) {
    cli::cli_alert_success(messages$download_complete)
  }

  return(dplyr::as_tibble(result))
}



# Internal utility functions for sus_climate_normals

# Get multilingual messages
.get_messages <- function(lang = "en") {
  messages <- list(
    pt = list(
      loading_dictionary = "Carregando dicion\u00e1rio de vari\u00e1veis...",
      found_variables = "Encontradas",
      downloading = "Baixando:",
      no_data_downloaded = "Nenhum dado foi baixado.",
      download_complete = "Download conclu\u00eddo com sucesso!",
      processing = "Processando dados...",
      cache_found = "Dados em cache encontrados.",
      cache_saved = "Dados salvos em cache."
    ),
    en = list(
      loading_dictionary = "Loading variables dictionary...",
      found_variables = "Found",
      downloading = "Downloading:",
      no_data_downloaded = "No data was downloaded.",
      download_complete = "Download completed successfully!",
      processing = "Processing data...",
      cache_found = "Cached data found.",
      cache_saved = "Data saved to cache."
    ),
    es = list(
      loading_dictionary = "Cargando diccionario de variables...",
      found_variables = "Encontrados",
      downloading = "Descargando:",
      no_data_downloaded = "No se descargaron datos.",
      download_complete = "\u00a1Descarga completada con \u00e9xito!",
      processing = "Procesando datos...",
      cache_found = "Datos en cach\u00e9 encontrados.",
      cache_saved = "Datos guardados en cach\u00e9."
    )
  )

  lang <- tolower(lang)
  if (!lang %in% names(messages)) {
    lang <- "en"
  }

  return(messages[[lang]])
}

# # Load the normals dictionary
# .get_normals_dictionary <- function() {
#   dict_path <- system.file("extdata", "climate_normals_dictionary_final.csv", 
#                            package = "climasus4r")

#   if (!file.exists(dict_path)) {
#     stop("Climate normals dictionary not found in package.")
#   }

#   dictionary <- read.csv(dict_path, stringsAsFactors = FALSE)
#   return(dplyr::as_tibble(dictionary))
# }

# Download and cache normals data
.download_and_cache_normals <- function(code_link, var_code, period, use_cache = TRUE, cache_dir, 
                                        verbose = TRUE, lang = "en") {
  
  messages <- .get_messages(lang)
  
  if (!dir.exists(cache_dir)) {dir.create(cache_dir, recursive = TRUE)}

  period_dir <- file.path(path.expand(cache_dir), period)

  if (!dir.exists(period_dir)) {dir.create(period_dir, recursive = TRUE, showWarnings = FALSE)}
  
  cache_file <- file.path(period_dir, paste0(var_code, ".rds"))

  # Check cache
  if (use_cache && file.exists(cache_file)) {
    if (verbose) {cli::cli_alert_info(messages$cache_found)}
    return(readRDS(cache_file))
  }
  # Build URL
  base_url <- "https://portal.inmet.gov.br/uploads/normais/"
  url <- paste0(base_url, code_link, ".xlsx")

  # Download file
  temp_file <- tempfile(fileext = ".xlsx")
  options(HTTPUserAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/119.0.0.0 Safari/537.36")
  tryCatch({
    options(timeout = 300)
    download.file(url, temp_file, mode = "wb", quiet = !verbose)
  }, error = function(e) {
    warning(paste("Failed to download", var_code, ":", e$message))
    return(NULL)
  })

  if (!file.exists(temp_file)) {
    return(NULL)
  }

  # Read and process Excel file
  data <- .read_inmet_excel(temp_file, var_code)

  # Save to cache
  if (use_cache && !is.null(data)) {
    saveRDS(data, cache_file)
    if (verbose) {
      cli::cli_alert_info(messages$cache_saved)
    }
  }

  unlink(temp_file)
  return(data)
}
# Read INMET Excel file with proper handling of merged cells in headers
.read_inmet_excel <- function(file_path, var_code) {
  
  tryCatch({
    # Step 1: Read the raw Excel data without col_names to see the structure
    raw_data <- readxl::read_excel(
      file_path, 
      sheet = 1, 
      skip = 1,
      col_names = TRUE,
      col_types = "text"
    )

    raw_data <- readxl::read_xlsx(file_path, skip = 2, col_names = TRUE) |>
    dplyr::rename_with(~ stringi::stri_trans_general(., "Latin-ASCII")) |>
    dplyr::mutate(dplyr::across(c(
      "Janeiro",
      "Fevereiro",
      "Marco",
      "Abril",
      "Maio",
      "Junho",
      "Julho",
      "Agosto",
      "Setembro",
      "Outubro",
      "Novembro",
      "Dezembro",
      "Ano"
    ), as.numeric)) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 4))) |>
    suppressWarnings()
    
    # Step 2: Extract the header rows (rows 1 and 2 of the raw data, which are Excel rows 3 and 4)
    header_row1 <- as.character(raw_data[1, ])  # Months (with merged cells)
    header_row2 <- as.character(raw_data[2, ])  # Decade indicators (1º DEC, 2º DEC, 3º DEC)
    
    # Step 3: Read the actual data (starting from row 5 in Excel, which is row 3 in raw_data)
    data <- raw_data[3:nrow(raw_data), ]
    
    # Step 4: Build proper column names by combining months and decades
    col_names <- c("codigo", "nome_estacao", "uf")
    
    # Process remaining columns
    current_month <- ""
    for (i in 4:length(header_row1)) {
      month_val <- header_row1[i]
      decade_val <- header_row2[i]
      
      # If month cell is not NA/empty, update current_month
      if (!is.na(month_val) && month_val != "" && month_val != "NA") {
        current_month <- month_val
      }
      
      # Clean month name
      month_clean <- current_month %>%
        stringi::stri_trans_general("Latin-ASCII") %>%
        stringi::stri_replace_all_regex("[^[:alnum:]]+", "_") %>%
        stringi::stri_trans_tolower() %>%
        stringi::stri_replace_all_regex("^_|_$", "")
      
      # Extract decade number from decade indicator (1º DEC -> 1, 2º DEC -> 2, etc.)
      decade_num <- decade_val %>%
        stringi::stri_trans_general("Latin-ASCII") %>%
        stringi::stri_replace_all_regex("[^0-9]", "") %>%
        stringi::stri_sub(1, 1)  # Get only the first digit
      
      # Create column name
      col_name <- paste0(month_clean, "_", decade_num)
      col_names <- c(col_names, col_name)
    }
    
    # Step 5: Assign column names to data
    colnames(data) <- col_names
    
    # Step 6: Ensure codigo is character
    data$codigo <- as.character(data$codigo)
    
    # Step 7: Convert all value columns to character to handle mixed types
    for (i in 4:ncol(data)) {
      data[[i]] <- as.character(data[[i]])
    }
    
    # Step 8: Convert to tibble
    data <- dplyr::as_tibble(data)
    
    # Step 9: Pivot longer to transform from wide to long format
    data_long <- data %>%
      tidyr::pivot_longer(
        cols = -c(codigo, nome_estacao, uf),
        names_to = "mes_decada",
        values_to = "valor"
      )
    
    # Step 10: Extract month and decade from the combined column
    data_long <- data_long %>%
      tidyr::separate(
        mes_decada,
        into = c("mes", "decada"),
        sep = "_(?=[0-9]$)",  # Split before the last single digit
        remove = TRUE
      ) %>%
      dplyr::mutate(
        # Convert valor to numeric, treating "-" as NA
        valor = suppressWarnings(as.numeric(
          stringi::stri_replace_all_regex(valor, "^-$", NA_character_)
        ))
      )
    
    # Step 11: Add variable code
    data_long$var_code <- var_code
    
    # Step 12: Reorder columns for better readability
    data_long <- data_long %>%
      dplyr::select(codigo, nome_estacao, uf, mes, decada, valor, var_code)
    
    return(data_long)
    
  }, error = function(e) {
    warning(paste("Error reading Excel file:", e$message))
    return(NULL)
  })
}

