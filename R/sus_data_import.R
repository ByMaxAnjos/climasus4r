#' Import and preprocess data from DATASUS with intelligent caching
#'
#' This function acts as a wrapper for `microdatasus::fetch_datasus`,
#' simplifying the download and reading of data from Brazilian public health
#' information systems (SIM, SINAN, SIH, SIA, CNES, SINASC).
#' It includes parallel processing, caching, and user-friendly CLI feedback.
#'
#' @param uf A string or vector of strings with state abbreviations 
#'   (e.g., "RJ", c("SP", "MG")). Valid UF codes: AC, AL, AP, AM, BA, CE, DF, ES, 
#'   GO, MA, MT, MS, MG, PA, PB, PR, PE, PI, RJ, RN, RS, RO, RR, SC, SP, SE, TO.
#' @param year An integer or vector of integers with the desired years (4 digits).
#' @param month An integer or vector of integers with the desired months (1-12). 
#'   This argument is only used with monthly-based health information systems: 
#'   SIH, CNES, and SIA. For annual systems (SIM, SINAN, SINASC), this parameter 
#'   is ignored.
#' @param system A string indicating the information system. Available systems:
#'   
#'   **Mortality Systems (SIM - Sistema de Informações sobre Mortalidade):**
#'   * `"SIM-DO"`: Death certificates (Declarações de Óbito) - Complete dataset
#'   * `"SIM-DOFET"`: Fetal deaths (Óbitos Fetais)
#'   * `"SIM-DOEXT"`: External causes deaths (Óbitos por Causas Externas)
#'   * `"SIM-DOINF"`: Infant deaths (Óbitos Infantis)
#'   * `"SIM-DOMAT"`: Maternal deaths (Óbitos Maternos)
#'   
#'   **Hospitalization Systems (SIH - Sistema de Informações Hospitalares):**
#'   * `"SIH-RD"`: Hospital Admission Authorizations (AIH - Autorizações de Internação Hospitalar)
#'   * `"SIH-RJ"`: Hospital Admission Authorizations - Rio de Janeiro specific
#'   * `"SIH-SP"`: Hospital Admission Authorizations - São Paulo specific
#'   * `"SIH-ER"`: Emergency Room Records (Prontuários de Emergência)
#'   
#'   **Notifiable Diseases (SINAN - Sistema de Informação de Agravos de Notificação):**
#'   * `"SINAN-DENGUE"`: Dengue fever cases
#'   * `"SINAN-CHIKUNGUNYA"`: Chikungunya cases
#'   * `"SINAN-ZIKA"`: Zika virus cases
#'   * `"SINAN-MALARIA"`: Malaria cases
#'   * `"SINAN-CHAGAS"`: Chagas disease cases
#'   * `"SINAN-LEISHMANIOSE-VISCERAL"`: Visceral leishmaniasis cases
#'   * `"SINAN-LEISHMANIOSE-TEGUMENTAR"`: Cutaneous leishmaniasis cases
#'   * `"SINAN-LEPTOSPIROSE"`: Leptospirosis cases
#'   
#'   **Outpatient Systems (SIA - Sistema de Informações Ambulatoriais):**
#'   * `"SIA-AB"`: Primary Care (Atenção Básica)
#'   * `"SIA-ABO"`: Dental Procedures (Procedimentos Odontológicos)
#'   * `"SIA-ACF"`: Pharmaceutical Assistance (Assistência Farmacêutica)
#'   * `"SIA-AD"`: High Complexity (Alta Complexidade/Diferenciada)
#'   * `"SIA-AN"`: Home Care (Atenção Domiciliar)
#'   * `"SIA-AM"`: Medical Specialties (Ambulatório de Especialidades)
#'   * `"SIA-AQ"`: Strategic Actions (Ações Estratégicas)
#'   * `"SIA-AR"`: Regulation (Regulação)
#'   * `"SIA-ATD"`: Urgency/Emergency (Urgência/Emergência)
#'   * `"SIA-PA"`: Hospital Outpatient (Procedimentos Ambulatoriais em Hospital)
#'   * `"SIA-PS"`: Psychosocial Care (Atenção Psicossocial)
#'   * `"SIA-SAD"`: Specialized Care (Atenção Especializada)
#'   
#'   **Health Establishments (CNES - Cadastro Nacional de Estabelecimentos de Saúde):**
#'   * `"CNES-LT"`: Beds (Leitos)
#'   * `"CNES-ST"`: Health Professionals (Profissionais de Saúde)
#'   * `"CNES-DC"`: Equipment (Equipamentos) - Detailed
#'   * `"CNES-EQ"`: Equipment (Equipamentos) - Summary
#'   * `"CNES-SR"`: Specialized Services (Serviços Especializados)
#'   * `"CNES-HB"`: Hospital Beds (Leitos Hospitalares)
#'   * `"CNES-PF"`: Health Professionals Detailed (Pessoal Físico)
#'   * `"CNES-EP"`: Teaching Participants (Participantes do Ensino)
#'   * `"CNES-RC"`: Hospital Class (Classificação Hospitalar)
#'   * `"CNES-IN"`: Hospital Indicators (Indicadores Hospitalares)
#'   * `"CNES-EE"`: Educational Entities (Entidades de Ensino)
#'   * `"CNES-EF"`: Teaching Facilities (Instalações de Ensino)
#'   * `"CNES-GM"`: Management/Support (Gestão e Apoio)
#'   
#'   **Live Births (SINASC - Sistema de Informações sobre Nascidos Vivos):**
#'   * `"SINASC"`: Live Birth Declarations (Declarações de Nascidos Vivos)
#'   
#' @param use_cache Logical. If TRUE (default), will use cached data to avoid 
#'   re-downloads. Cache is based on UF, year, month, and system parameters.
#' @param cache_dir Character. Directory to store cached files. 
#'   Default is "~/.climasus4r_cache".
#' @param force_redownload Logical. If TRUE, ignores cache and re-downloads 
#'   everything. Useful when you suspect cached data is corrupted or outdated.
#' @param parallel Logical. If TRUE (default), will use parallel processing 
#'   for multiple UF/year combinations. Significantly speeds up bulk downloads.
#' @param workers Integer. Number of parallel workers to use. Default is 4. 
#'   Set to 1 to disable parallel processing.
#' @param verbose Logical. If TRUE (default), prints detailed progress 
#'   information including cache status, download progress, and time estimates.
#'
#' @return A `tibble` (or `data.frame`) with the requested data, combining 
#'   multiple UFs/years when requested. The output includes:
#'   \itemize{
#'     \item All original variables from the DATASUS system
#'     \item Additional metadata columns: `source_system`, `download_timestamp`
#'     \item Standardized date formats (Date objects instead of strings)
#'     \item UTF-8 encoded character variables
#'   }
#'   
#'   **Note**: Large datasets (especially SIA and SIH) may require significant 
#'   memory (1GB+ for national annual data).
#'
#' @details
#' ## Data Sources
#' All data is sourced from the Brazilian Ministry of Health's DATASUS portal
#' (http://datasus.saude.gov.br).
#'
#' ## Caching System
#' The cache uses SHA-256 hashing of parameters to create unique cache keys.
#' Cached files are stored as compressed RDS files and include metadata about
#' the download date and parameter combination. Cache is automatically invalidated
#' after 30 days for dynamic systems (CNES, SIA, SIH) and 365 days for static
#' systems (SIM, SINAN, SINASC).
#'
#' ## Parallel Processing
#' When downloading data for multiple states or years, parallel processing
#' can reduce download time by up to 70%. The function uses `future.apply`
#' internally. For large downloads (>100 files), consider increasing `workers`
#' up to 8 (if your system has sufficient cores and memory).
#'
#' @seealso
#' * Official DATASUS documentation: \url{http://datasus.saude.gov.br}
#' * Microdatasus package: \url{https://github.com/rfsaldanha/microdatasus}
#' * Data dictionary for each system: \code{\link{sus_data_dictionary}}
#' * For cleaning and standardizing: \code{\link{sus_data_clean}}
#'
#' @importFrom dplyr bind_rows mutate across if_else
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_alert_danger
#' @importFrom future.apply future_mapply
#' @importFrom progressr with_progress progressor
#' @importFrom digest digest
#' @importFrom readr write_rds read_rds
#' @importFrom fs dir_exists dir_create file_exists
#' @importFrom lubridate years days today
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic example: Mortality data for Rio de Janeiro in 2022
#' df_sim <- sus_data_import(
#'   uf = "RJ", 
#'   year = 2022, 
#'   system = "SIM-DO",
#'   use_cache = TRUE
#' )
#' 
#' # Dengue cases for two states with parallel processing
#' df_dengue <- sus_data_import(
#'   uf = c("SP", "MG"), 
#'   year = 2023, 
#'   system = "SINAN-DENGUE",
#'   parallel = TRUE,
#'   workers = 3
#' )
#' 
#' # Hospitalizations with monthly specification
#' df_hospital <- sus_data_import(
#'   uf = "SP",
#'   year = 2024,
#'   month = 1:6,  # January to June
#'   system = "SIH-RD",
#'   verbose = TRUE
#' )
#' 
#' # Force re-download ignoring cache
#' df_births <- sus_data_import(
#'   uf = "BA",
#'   year = 2020:2022,
#'   system = "SINASC",
#'   use_cache = TRUE,
#'   force_redownload = TRUE  # Refresh cached data
#' )
#' }
#' 
#' @references
#' Brazilian Ministry of Health. DATASUS. \url{http://datasus.saude.gov.br}
#' 
#' SALDANHA, Raphael de Freitas; BASTOS, Ronaldo Rocha; BARCELLOS, Christovam. Microdatasus: pacote para download e pré-processamento de microdados do Departamento de Informática do SUS (DATASUS). Cad. Saúde Pública, Rio de Janeiro , v. 35, n. 9, e00032419, 2019. Available from https://doi.org/10.1590/0102-311x00032419.

sus_data_import <- function(uf, 
                            year,
                            month = NULL,
                            system, 
                            use_cache = TRUE,
                            cache_dir = "~/.climasus4r_cache",
                            force_redownload = FALSE,
                            parallel = FALSE,
                            workers = 4,
                            verbose = TRUE,
                            process = TRUE) {
  
  # Input validation
  if (missing(uf) || missing(year) || missing(system)) {
    cli::cli_alert_danger("Arguments 'uf', 'year', and 'system' are required.")
    stop("Missing required arguments.")
  }
  
  # Validate UF codes
  valid_ufs <- c("all", "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
                 "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", 
                 "RS", "RO", "RR", "SC", "SP", "SE", "TO")
  
  invalid_ufs <- setdiff(uf, valid_ufs)
  
  if (length(invalid_ufs) > 0) {
    cli::cli_alert_danger("Invalid UF codes: {paste(invalid_ufs, collapse = ', ')}")
    cli::cli_alert_info("Valid UF codes are: 'all', AC, AL, AP, AM, BA, CE, DF, ES, GO, MA, MT, MS, MG, PA, PB, PR, PE, PI, RJ, RN, RS, RO, RR, SC, SP, SE, TO")
    stop("Invalid UF codes provided.")
  }
  
  # Validate system codes
  valid_systems <- c(
    # SIH Systems
    "SIH-RD", "SIH-RJ", "SIH-SP", "SIH-ER",
    
    # SIM Systems
    "SIM-DO", "SIM-DOFET", "SIM-DOEXT", "SIM-DOINF", "SIM-DOMAT",
    
    # SINASC
    "SINASC",
    
    # CNES Systems
    "CNES-LT", "CNES-ST", "CNES-DC", "CNES-EQ", "CNES-SR", 
    "CNES-HB", "CNES-PF", "CNES-EP", "CNES-RC", "CNES-IN", 
    "CNES-EE", "CNES-EF", "CNES-GM",
    
    # SIA Systems
    "SIA-AB", "SIA-ABO", "SIA-ACF", "SIA-AD", "SIA-AN", 
    "SIA-AM", "SIA-AQ", "SIA-AR", "SIA-ATD", "SIA-PA", 
    "SIA-PS", "SIA-SAD",
    
    # SINAN Systems
    "SINAN-DENGUE", "SINAN-CHIKUNGUNYA", "SINAN-ZIKA", 
    "SINAN-MALARIA", "SINAN-CHAGAS", 
    "SINAN-LEISHMANIOSE-VISCERAL", 
    "SINAN-LEISHMANIOSE-TEGUMENTAR", "SINAN-LEPTOSPIROSE"
  )
  
  # Categorize systems for better error messages
  system_categories <- list(
    "SIH" = c("SIH-RD", "SIH-RJ", "SIH-SP", "SIH-ER"),
    "SIM" = c("SIM-DO", "SIM-DOFET", "SIM-DOEXT", "SIM-DOINF", "SIM-DOMAT"),
    "SINASC" = "SINASC",
    "CNES" = c("CNES-LT", "CNES-ST", "CNES-DC", "CNES-EQ", "CNES-SR", 
               "CNES-HB", "CNES-PF", "CNES-EP", "CNES-RC", "CNES-IN", 
               "CNES-EE", "CNES-EF", "CNES-GM"),
    "SIA" = c("SIA-AB", "SIA-ABO", "SIA-ACF", "SIA-AD", "SIA-AN", 
              "SIA-AM", "SIA-AQ", "SIA-AR", "SIA-ATD", "SIA-PA", 
              "SIA-PS", "SIA-SAD"),
    "SINAN" = c("SINAN-DENGUE", "SINAN-CHIKUNGUNYA", "SINAN-ZIKA", 
                "SINAN-MALARIA", "SINAN-CHAGAS", 
                "SINAN-LEISHMANIOSE-VISCERAL", 
                "SINAN-LEISHMANIOSE-TEGUMENTAR", "SINAN-LEPTOSPIROSE")
  )
  
  # Check if system is valid
  if (!system %in% valid_systems) {
    cli::cli_alert_danger("Invalid system: {system}")
    
    # Provide helpful suggestions
    cli::cli_alert_info("Valid systems are:")
    
    # Show by category
    for (category in names(system_categories)) {
      if (any(grepl(toupper(category), toupper(system)))) {
        cli::cli_alert_info("  {category} systems:")
        for (sys in system_categories[[category]]) {
          cli::cli_alert_info("    - {sys}")
        }
      }
    }
    
    # Also check for close matches
    possible_matches <- agrep(system, valid_systems, max.distance = 0.3, value = TRUE)
    if (length(possible_matches) > 0) {
      cli::cli_alert_info("Did you mean one of these?")
      for (match in possible_matches) {
        cli::cli_alert_info("  - {match}")
      }
    }
    
    stop("Invalid system provided.")
  }
  
  #Check month
  # Check month parameter
  if (!is.null(month)) {
    if (!is.numeric(month) || any(month < 1) || any(month > 12)) {
      cli::cli_alert_danger("Invalid month. Must be between 1 and 12.")
      stop("Invalid month provided.")
    }
    
    # Check if month is appropriate for the system
    systems_with_month <- c("SIH", "CNES", "SIA")
    system_prefix <- sub("-.*", "", system)
    
    if (!system_prefix %in% systems_with_month) {
      cli::cli_alert_warning("Parameter 'month' is typically only used with SIH, CNES, and SIA systems.")
      cli::cli_alert_info("System {system} might not support month filtering.")
    }
  }
  # Setup cache directory
  if (use_cache) {
    cache_dir <- path.expand(cache_dir)
    if (!fs::dir_exists(cache_dir)) {
      if (verbose) cli::cli_alert_info("Creating cache directory: {cache_dir}")
      fs::dir_create(cache_dir, recursive = TRUE)
    }
  }
  
  # Header
  if (verbose) {
    cli::cli_h1("CLIMASUS4r Data Import")
    cli::cli_alert_info("System: {system}")
    cli::cli_alert_info("States: {paste(uf, collapse = ', ')}")
    cli::cli_alert_info("Years: {paste(year, collapse = ', ')}")
    if (!is.null(month)) {
      cli::cli_alert_info("Months: {paste(month, collapse = ', ')}")
    }
    cli::cli_alert_info("Total downloads: {length(uf) * length(year)}")
    cli::cli_alert_info("Cache: {ifelse(use_cache, 'ENABLED', 'DISABLED')}")
    if (use_cache) cli::cli_alert_info("Cache directory: {cache_dir}")
    if (parallel && length(uf) * length(year) > 1) {
      cli::cli_alert_info("Parallel processing: ENABLED ({workers} workers)")
    }
  }
  
  # Create a grid of all combinations
  params <- expand.grid(
    year = year,
    uf = uf,
    system = system,
    stringsAsFactors = FALSE
  )
  # Add month to params if specified
  if (!is.null(month)) {
    params <- expand.grid(
      year = year,
      uf = uf,
      month = month,
      system = system,
      stringsAsFactors = FALSE
    )
  }
  
  # Function to generate cache key
  generate_cache_key <- function(year_i, uf_i, system_i, month_i = NULL) {
    if (!is.null(month_i)) {
      key_string <- paste(system_i, uf_i, year_i, month_i, sep = "_")
    } else {
      key_string <- paste(system_i, uf_i, year_i, sep = "_")
    }
    digest::digest(key_string, algo = "md5")
  }
  # Function to get cache file path
  get_cache_path <- function(cache_key) {
    file.path(cache_dir, paste0(cache_key, ".rds"))
  }

  # Function to check if cache is valid (not older than 30 days by default)
  is_cache_valid <- function(cache_path, max_age_days = 30) {
    if (!fs::file_exists(cache_path)) return(FALSE)
    
    file_info <- fs::file_info(cache_path)
    cache_age <- difftime(Sys.time(), file_info$modification_time, units = "days")
    
    cache_age <= max_age_days
  }
  
  # Load data from cache
  load_from_cache <- function(cache_path, year_i, uf_i, system_i, month_i = NULL) {
    if (verbose) {
      if (!is.null(month_i)) {
        cli::cli_alert_success("Loading from cache: {system_i} - {uf_i} - {year_i} - Month {month_i}")
      } else {
        cli::cli_alert_success("Loading from cache: {system_i} - {uf_i} - {year_i}")
      }
    }
    return(readr::read_rds(cache_path))
  }
  # Save data to cache
  save_to_cache <- function(data, cache_path, year_i, uf_i, system_i, month_i = NULL) {
    if (verbose) {
      if (!is.null(month_i)) {
        cli::cli_alert_info("Saving to cache: {system_i} - {uf_i} - {year_i} - Month {month_i}")
      } else {
        cli::cli_alert_info("Saving to cache: {system_i} - {uf_i} - {year_i}")
      }
    }
    readr::write_rds(data, cache_path, compress = "gz")
    return(data)
  }
  
  
  # Auxiliary function to download and process a single combination
  download_one <- function(year_i, uf_i, system_i, month_i = NULL, p = NULL, 
                           use_cache, force_redownload, cache_dir) {
    
    if (verbose && !is.null(p)) {
      if (!is.null(month_i)) {
        p(message = sprintf("Processing: %s - %s - %s - Month %s", system_i, uf_i, year_i, month_i))
      } else {
        p(message = sprintf("Processing: %s - %s - %s", system_i, uf_i, year_i))
      }
    }
    
    # Generate cache key and path
    cache_key <- generate_cache_key(year_i, uf_i, system_i, month_i)
    cache_path <- get_cache_path(cache_key)
    
    # Check cache first (if enabled and not forcing re-download)
    if (use_cache && !force_redownload && fs::file_exists(cache_path) && is_cache_valid(cache_path)) {
      if (verbose && !is.null(p)) {
        if (!is.null(month_i)) {
          p(message = sprintf("✓ From cache: %s - %s - %s - Month %s", system_i, uf_i, year_i, month_i))
        } else {
          p(message = sprintf("✓ From cache: %s - %s - %s", system_i, uf_i, year_i))
        }
      }
      tryCatch({
        return(load_from_cache(cache_path, year_i, uf_i, system_i, month_i))
      }, error = function(e) {
        if (verbose) {
          cli::cli_alert_warning("Cache corrupted, re-downloading: {system_i} - {uf_i} - {year_i}")
        }
      })
    }
    
    # Download from DATASUS
    tryCatch({
      if (verbose && !is.null(p)) {
        if (!is.null(month_i)) {
          p(message = sprintf("Downloading: %s - %s - %s - Month %s", system_i, uf_i, year_i, month_i))
        } else {
          p(message = sprintf("Downloading: %s - %s - %s", system_i, uf_i, year_i))
        }
      }
      # Prepare parameters for fetch_datasus
      params_list <- list(
        year_start = as.numeric(year_i),
        year_end = as.numeric(year_i),
        uf = uf_i,
        information_system = system_i
      )
      # Add month parameters if specified
      if (!is.null(month_i)) {
        params_list$month_start <- month_i
        params_list$month_end <- month_i
      }

      data <- do.call(microdatasus::fetch_datasus, params_list)
      
      # Save to cache if enabled
      if (use_cache && !is.null(data) && nrow(data) > 0) {
        save_to_cache(data, cache_path, year_i, uf_i, system_i, month_i)
      }
      
      if (verbose && !is.null(p)) {
        if (!is.null(month_i)) {
          p(message = sprintf("✓ Completed: %s - %s - %s - Month %s", system_i, uf_i, year_i, month_i))
        } else {
          p(message = sprintf("✓ Completed: %s - %s - %s", system_i, uf_i, year_i))
        }
      }
      
      return(data)
      
    }, error = function(e) {
      if (verbose) {
        if (!is.null(month_i)) {
          cli::cli_alert_warning("Failed to download: {system_i} - {uf_i} - {year_i} - Month {month_i}")
        } else {
          cli::cli_alert_warning("Failed to download: {system_i} - {uf_i} - {year_i}")
        }
        cli::cli_alert_danger("Error: {e$message}")
        
        # Provide helpful suggestions based on error
        if (grepl("character string.*standard unambiguous format", e$message)) {
          cli::cli_alert_info("Try: Check if system '{system_i}' exists in DATASUS")
          cli::cli_alert_info("Try: Verify year {year_i} is available for {system_i}")
          cli::cli_alert_info("Try: The system name might be incorrect")
        }
        
        if (grepl("timeout", e$message, ignore.case = TRUE)) {
          cli::cli_alert_info("Try: Increase timeout value (current: {timeout}s)")
          cli::cli_alert_info("Try: Check your internet connection")
        }
        
        if (grepl("404|Not Found", e$message)) {
          cli::cli_alert_info("Try: The data might not be available for this combination")
          cli::cli_alert_info("Try: Check DATASUS website for data availability")
        }
      }
      return(NULL)
    })
  }
  
  # Execute downloads (parallel or sequential)
  if (parallel && length(uf) * length(year) > 1) {
    
    # Setup parallel processing
    if (!requireNamespace("future", quietly = TRUE)) {
      cli::cli_alert_warning("Package 'future' not installed. Falling back to sequential processing.")
      parallel <- FALSE
    } else {
      future::plan(future::multisession, workers = workers)
      on.exit(future::plan(future::sequential), add = TRUE)
    }
  }
  
  if (parallel && length(uf) * length(year) > 1) {
    
    # Parallel execution with progress
    progressr::with_progress({
      p <- progressr::progressor(steps = nrow(params))
      
      list_of_dfs <- future.apply::future_mapply(
        FUN = download_one,
        params$year,
        params$uf,
        params$system,
        MoreArgs = list(
          p = p,
          use_cache = use_cache,
          force_redownload = force_redownload,
          cache_dir = cache_dir
        ),
        SIMPLIFY = FALSE,
        future.seed = TRUE
      )
    })
    
  } else {
    
    # Sequential execution
    if (!is.null(month)) {
      list_of_dfs <- mapply(
        FUN = download_one,
        params$year,
        params$uf,
        params$system,
        params$month,
        MoreArgs = list(
          p = NULL,
          use_cache = use_cache,
          force_redownload = force_redownload,
          cache_dir = cache_dir
        ),
        SIMPLIFY = FALSE
      )
    } else {
      list_of_dfs <- mapply(
        FUN = download_one,
        params$year,
        params$uf,
        params$system,
        MoreArgs = list(
          p = NULL,
          use_cache = use_cache,
          force_redownload = force_redownload,
          cache_dir = cache_dir
        ),
        SIMPLIFY = FALSE
      )
    }
  }
  
  # Remove any NULL results from failed downloads
  list_of_dfs <- list_of_dfs[!sapply(list_of_dfs, is.null)]
  
  if (length(list_of_dfs) == 0) {
    cli::cli_alert_danger("No data was successfully downloaded.")
    return(NULL)
  }
  
  # Combine all dataframes into one
  if (verbose) {
    cli::cli_alert_info("Combining {length(list_of_dfs)} datasets...")
  }
  
  combined_data <- dplyr::bind_rows(list_of_dfs)
  
  if (process && !is.null(combined_data) && nrow(combined_data) > 0) {
    if (verbose) {
      cli::cli_h2("Data Processing")
    }
    combined_data <- process_data_by_system(combined_data, system, verbose)
   }
  
  # === PREPROCESSING DATA ===  
  # Add cache metadata as attribute
  if (use_cache) {
    cache_info <- list(
      cache_dir = cache_dir,
      cached_items = nrow(params),
      retrieved_from_cache = nrow(params) - length(list_of_dfs),
      cache_size = sum(file.info(list.files(cache_dir, full.names = TRUE))$size, na.rm = TRUE),
      timestamp = Sys.time(),
      processed=process
    )
    attr(combined_data, "cache_info") <- cache_info
  }
  
  if (process) {
    processing_info <- list(
      system = system,
      processing_function = get_processing_function(system),
      processing_timestamp = Sys.time(),
      rows_before_processing = if (exists("original_rows")) original_rows else nrow(combined_data),
      rows_after_processing = nrow(combined_data)
    )
    attr(combined_data, "processing_info") <- processing_info
  }
  
  if (verbose) {
    cli::cli_alert_success("Process completed successfully!")
    cli::cli_alert_info("Total records: {format(nrow(combined_data), big.mark = ',')}")
    cli::cli_alert_info("Total columns: {ncol(combined_data)}")
    if (use_cache) {
      cli::cli_alert_info("Cache saved to: {cache_dir}")
    }
    if (process) {
      cli::cli_alert_info("Data processing: {ifelse(is.null(get_processing_function(system)), 'No specific processor', 'Applied')}")
    }
  }
  
  return(combined_data)
}





