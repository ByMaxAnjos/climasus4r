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

# Load the normals dictionary
.get_normals_dictionary <- function() {
  dict_path <- system.file("extdata", "climate_normals_dictionary_final.csv", 
                           package = "climasus4r")

  if (!file.exists(dict_path)) {
    stop("Climate normals dictionary not found in package.")
  }

  dictionary <- read.csv(dict_path, stringsAsFactors = FALSE)
  return(dplyr::as_tibble(dictionary))
}

# Download and cache normals data
.download_and_cache_normals <- function(code_link, var_code, use_cache = TRUE, 
                                        verbose = TRUE, lang = "en") {
  
  messages <- .get_messages(lang)
  cache_dir <- file.path(path.expand("~"), ".climasus4r_cache", "normals")
  
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  cache_file <- file.path(cache_dir, paste0(var_code, ".rds"))

  # Check cache
  if (use_cache && file.exists(cache_file)) {
    if (verbose) {
      cli::cli_alert_info(messages$cache_found)
    }
    return(readRDS(cache_file))
  }

  # Build URL
  base_url <- "https://www.gov.br/inmet/pt-br/centrais-de-conteudo/downloads/normais-climatologicas/"
  url <- paste0(base_url, code_link, ".xlsx")

  # Download file
  temp_file <- tempfile(fileext = ".xlsx")
  
  tryCatch({
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

# Read INMET Excel file with proper header handling
.read_inmet_excel <- function(file_path, var_code) {
  
  tryCatch({
    # Read the first few rows to detect header structure
    raw_data <- readxl::read_excel(file_path, sheet = 1, col_names = FALSE)
    
    # Typically headers are in rows 1-3
    # Row 1: Title
    # Row 2: Variable name
    # Row 3: Column headers
    
    # Read with proper skip
    data <- readxl::read_excel(file_path, sheet = 1, skip = 3)
    
    # Clean column names
    data <- janitor::clean_names(data)
    
    # Convert to tibble and add variable code
    data <- dplyr::as_tibble(data)
    data$var_code <- var_code
    
    return(data)
    
  }, error = function(e) {
    warning(paste("Error reading Excel file:", e$message))
    return(NULL)
  })
}
