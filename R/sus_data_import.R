#' Import and preprocess data from DATASUS with intelligent caching
#'
#' This function acts as a wrapper for `microdatasus::fetch_datasus`,
#' simplifying the download and reading of data from Brazilian public health
#' information systems (SIM, SINAN, SIH, SIA, CNES, SINASC).
#' It includes parallel processing, caching, and user-friendly CLI feedback.
#'
#' @param uf A string or vector of strings with state abbreviations (**igonered if 'region' is provided**) 
#'   (e.g., "AM", c("SP", "RJ")). Valid UF codes: AC, AL, AP, AM, BA, CE, DF, ES, 
#'   GO, MA, MT, MS, MG, PA, PB, PR, PE, PI, RJ, RN, RS, RO, RR, SC, SP, SE, TO.
#' @param region A string indicating a predefined group of states (supports multilingual names PT, EN, ES). Available regions:
#'   
#'   **IBGE Macro-regions:**
#'   * `"norte"`: c("AC", "AP", "AM", "PA", "RO", "RR", "TO")
#'   * `"nordeste"`: c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE")
#'   * `"centro_oeste"`: c("DF", "GO", "MT", "MS")
#'   * `"sudeste"`: c("ES", "MG", "RJ", "SP")
#'   * `"sul"`: c("PR", "RS", "SC")
#'   
#'   **Biomes (Ecological Borders):**
#'   * `"amazonia_legal"`: c("AC", "AP", "AM", "PA", "RO", "RR", "MT", "MA", "TO")
#'   * `"mata_atlantica"`: c("AL", "BA", "CE", "ES", "GO", "MA", "MG", "MS", "PB", "PE", "PI", "PR", "RJ", "RN", "RS", "SC", "SE", "SP")
#'   * `"caatinga"`: c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "MG")
#'   * `"cerrado"`: c("BA", "DF", "GO", "MA", "MG", "MS", "MT", "PA", "PI", "PR", "RO", "SP", "TO")
#'   * `"pantanal"`: c("MT", "MS")
#'   * `"pampa"`: c("RS")
#' 
#'   **Hydrography & Climate:**
#'   * `"bacia_amazonia"`: c("AC", "AM", "AP", "MT", "PA", "RO", "RR")
#'   * `"bacia_sao_francisco"`: c("AL", "BA", "DF", "GO", "MG", "PE", "SE")
#'   * `"bacia_parana"`: c("GO", "MG", "MS", "PR", "SP")
#'   * `"bacia_tocantins"`: c("GO", "MA", "PA", "TO")
#'   * `"semi_arido"`: c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "MG")
#'   
#'   **Health, Agriculture & Geopolitics:**
#'   * `"matopiba"`: c("MA", "TO", "PI", "BA")
#'   * `"arco_desmatamento"`: c("RO", "AC", "AM", "PA", "MT", "MA")
#'   * `"dengue_hyperendemic"`: c("GO", "MS", "MT", "PR", "RJ", "SP")
#'   * `"sudene"`: c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "MG", "ES")
#'   * `"fronteira_brasil"`: c("AC", "AM", "AP", "MT", "MS", "PA", "PR", "RO", "RR", "RS", "SC")
#' 
#' @param year An integer or vector of integers with the desired years (4 digits).
#' @param month An integer or vector of integers with the desired months (1-12). 
#'   This argument is only used with monthly-based health information systems: 
#'   SIH, CNES, and SIA. For annual systems (SIM, SINAN, SINASC), this parameter 
#'   is ignored.
#' @param system A string indicating the information system. Available systems:
#'   
#'   **Mortality Systems (SIM - Mortality Information System):**
#'   * `"SIM-DO"`: Death certificates (Declaracoes de Obito) - Complete dataset
#'   * `"SIM-DOFET"`: Fetal deaths (Obitos Fetais)
#'   * `"SIM-DOEXT"`: External causes deaths (Obitos por Causas Externas)
#'   * `"SIM-DOINF"`: Infant deaths (Obitos Infantis)
#'   * `"SIM-DOMAT"`: Maternal deaths (Obitos Maternos)
#'   
#'   **Hospitalization Systems (SIH - Hospital Information System):**
#'   * `"SIH-RD"`: Hospital Admission Authorizations (AIH - Autorizacoes de Internacao Hospitalar)
#'   * `"SIH-RJ"`: Hospital Admission Authorizations - Rio de Janeiro specific
#'   * `"SIH-SP"`: Hospital Admission Authorizations - Sao Paulo specific
#'   * `"SIH-ER"`: Emergency Room Records (Prontuarios de Emergencia)
#'   
#'   **Notifiable Diseases (SINAN - Notifiable Diseases Information System):**
#'   * `"SINAN-DENGUE"`: Dengue fever cases
#'   * `"SINAN-CHIKUNGUNYA"`: Chikungunya cases
#'   * `"SINAN-ZIKA"`: Zika virus cases
#'   * `"SINAN-MALARIA"`: Malaria cases
#'   * `"SINAN-CHAGAS"`: Chagas disease cases
#'   * `"SINAN-LEISHMANIOSE-VISCERAL"`: Visceral leishmaniasis cases
#'   * `"SINAN-LEISHMANIOSE-TEGUMENTAR"`: Cutaneous leishmaniasis cases
#'   * `"SINAN-LEPTOSPIROSE"`: Leptospirosis cases
#'   
#'   **Outpatient Systems (SIA - Outpatient Information System):**
#'   * `"SIA-AB"`: Primary Care (Atencao Basica)
#'   * `"SIA-ABO"`: Dental Procedures (Procedimentos Odontologicos)
#'   * `"SIA-ACF"`: Pharmaceutical Assistance (Assistencia Farmaceutica)
#'   * `"SIA-AD"`: High Complexity (Alta Complexidade/Diferenciada)
#'   * `"SIA-AN"`: Home Care (Atencao Domiciliar)
#'   * `"SIA-AM"`: Medical Specialties (Ambulatorio de Especialidades)
#'   * `"SIA-AQ"`: Strategic Actions (Acoes Estrategicas)
#'   * `"SIA-AR"`: Regulation (Regulacao)
#'   * `"SIA-ATD"`: Urgency/Emergency (Urgencia/Emergencia)
#'   * `"SIA-PA"`: Hospital Outpatient (Procedimentos Ambulatoriais em Hospital)
#'   * `"SIA-PS"`: Psychosocial Care (Atencao Psicossocial)
#'   * `"SIA-SAD"`: Specialized Care (Atencao Especializada)
#'   
#'   **Health Establishments (CNES - National Health Establishment Registry):**
#'   * `"CNES-LT"`: Beds (Leitos)
#'   * `"CNES-ST"`: Health Professionals (Profissionais de Saude)
#'   * `"CNES-DC"`: Equipment (Equipamentos) - Detailed
#'   * `"CNES-EQ"`: Equipment (Equipamentos) - Summary
#'   * `"CNES-SR"`: Specialized Services (Servicos Especializados)
#'   * `"CNES-HB"`: Hospital Beds (Leitos Hospitalares)
#'   * `"CNES-PF"`: Health Professionals Detailed (Pessoal Fisico)
#'   * `"CNES-EP"`: Teaching Participants (Participantes do Ensino)
#'   * `"CNES-RC"`: Hospital Class (Classificacao Hospitalar)
#'   * `"CNES-IN"`: Hospital Indicators (Indicadores Hospitalares)
#'   * `"CNES-EE"`: Educational Entities (Entidades de Ensino)
#'   * `"CNES-EF"`: Teaching Facilities (Instalacoes de Ensino)
#'   * `"CNES-GM"`: Management/Support (Gestao e Apoio)
#'   
#'   **Live Births (SINASC - Live Birth Information System):**
#'   * `"SINASC"`: Live Birth Declarations (Declaracoes de Nascidos Vivos)
#'   
#' @param use_cache Logical. If TRUE (default), will use cached data to avoid 
#'   re-downloads. Cache is based on UF, year, month, and system parameters.
#' @param cache_dir Character. Directory to store cached files. 
#'   Default is "~/.climasus4r_cache/data".
#' @param force_redownload Logical. If TRUE, ignores cache and re-downloads 
#'   everything. Useful when you suspect cached data is corrupted or outdated.
#' @param parallel Logical. If TRUE (default), will use parallel processing 
#'   for multiple UF/year combinations. Significantly speeds up bulk downloads.
#' @param workers Integer. Number of parallel workers to use. Default is 4. 
#'   Set to 1 to disable parallel processing.
#' @param lang Character string specifying the language for variable labels and
#'   messages. Options: `"en"` (English), `"pt"` (Portuguese, default), `"es"` (Spanish).
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
#'
#' @importFrom glue glue
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
#' SALDANHA, Raphael de Freitas; BASTOS, Ronaldo Rocha; BARCELLOS, Christovam. Microdatasus: pacote para download e pre-processamento de microdados do Departamento de Informatica do SUS (DATASUS). Cad. Saude Publica, Rio de Janeiro , v. 35, n. 9, e00032419, 2019. Available from https://doi.org/10.1590/0102-311x00032419.

sus_data_import <- function(uf = NULL, 
                            region = NULL,
                            year,
                            month = NULL,
                            system, 
                            use_cache = TRUE,
                            cache_dir = "~/.climasus4r_cache/data",
                            force_redownload = FALSE,
                            parallel = FALSE,
                            workers = 4,
                            lang = "pt",
                            verbose = TRUE) {
  # --- 1. Region Handling (Precedence over UF) ---
  # Validate language
  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_abort("lang must be one of: 'en', 'pt', 'es'")
  }
  #Check if arrow pak installed
  check_arrow(lang=lang)

  if (!is.null(region)) {
    reg_clean <- tolower(region)
    target_key <- if (reg_clean %in% names(.region_aliases)) .region_aliases[[reg_clean]] else reg_clean
    if (target_key %in% names(.br_regions)) {
      if (!is.null(uf) && verbose) {
      cli::cli_alert_warning("Both {.arg uf} and {.arg region} provided. Using region mapping for: {.val {region}}.")
      }
     uf <- .br_regions[[target_key]]
      if (verbose) cli::cli_alert_info("Region {.val {region}} expanded to: {paste(uf, collapse = ', ')}")
    } else {
      cli::cli_abort("Region {.val {region}} not recognized. Check documentation for valid regions.")
    }
  }
  
  # Input validation
  if (is.null(uf) || missing(year) || missing(system)) {
    cli::cli_alert_danger("Arguments {.arg uf} (or {.arg region}), {.arg year}, and {.arg system} are required.")
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
  
   # Valid month
  system_prefix_month <- sub("-.*", "", system)
  systems_requiring_month <- c("SIH", "CNES", "SIA")
  
  if (system_prefix_month %in% systems_requiring_month && is.null(month)) {
    cli::cli_alert_danger("System {system} requires the 'month' argument.")
    cli::cli_alert_info("Please provide months as a vector (e.g., month = 1:12 or month = 1).")
    stop("Missing required argument: month. Please use month for 'SIH', 'CNES' and 'SIA' systems.")
  }
  
  if (!is.null(month)) {
    if (!is.numeric(month) || any(month < 1) || any(month > 12)) {
      cli::cli_alert_danger("Invalid month. Must be a numeric vector between 1 and 12.")
      stop("Invalid month provided.")
    }
    
    # 3. Aviso se o usuario fornecer mes para sistemas que geralmente são anuais (SIM, SINASC, SINAN)
    if (!system_prefix_month %in% systems_requiring_month) {
      cli::cli_alert_warning("Parameter 'month' is provided but typically not used for {system_prefix} systems.")
      cli::cli_alert_info("These systems usually aggregate data by year. The 'month' filter might be ignored by the server.")
    }
  }
  #Check month
  if (parallel && workers > parallel::detectCores()) {
    cli::cli_alert_warning(
      "{workers} workers requested but only {parallel::detectCores()} cores available"
    )
  }

  #National system for efficient download
  national_systems <- c(
    "SINAN-DENGUE", "SINAN-CHIKUNGUNYA", "SINAN-ZIKA",
    "SINAN-MALARIA", "SINAN-CHAGAS", "SINAN-LEISHMANIOSE-VISCERAL",
    "SINAN-LEISHMANIOSE-TEGUMENTAR", "SINAN-LEPTOSPIROSE"
  )

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
    cli::cli_h1("Climasus4r Data Import")
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

  #New: Is national sys
  is_national <- system %in% national_systems
  
  if (is_national) {    
    # Criamos params com apenas UMA combinacao (primeira UF)
    # Mas mantemos todas as UFs para filtragem posterior
    params <- expand.grid(
      year = year,
      uf = uf[1],  # Usa apenas a primeira UF para download
      system = system,
      stringsAsFactors = FALSE
    )
    
    # Guarda todas as UFs para filtragem
    all_ufs <- uf
    
  } else {
    # Para sistemas normais, criamos grid completo
    params <- expand.grid(
      year = year,
      uf = uf,
      system = system,
      stringsAsFactors = FALSE
    )
  }
  # Create a grid of all combinations
  # params <- expand.grid(
  #   year = year,
  #   uf = uf,
  #   system = system,
  #   stringsAsFactors = FALSE
  # )
  # Add month to params if specified
  # if (!is.null(month)) {
  #   params <- expand.grid(
  #     year = year,
  #     #uf = uf,
  #     uf = if (is_national) uf[1] else uf,
  #     month = month,
  #     system = system,
  #     stringsAsFactors = FALSE
  #   )
  #   if (is_national) all_ufs <- uf
  # }
  if (!is.null(month)) {
    if (is_national) {
      # Para sistemas nacionais com mês, criar grid com apenas uma UF
      params <- expand.grid(
        year = year,
        uf = uf[1],  # Apenas a primeira UF
        month = month,
        system = system,
        stringsAsFactors = FALSE
      )
      all_ufs <- uf  # Guardar todas para filtragem
    } else {
      # Para sistemas normais, criar grid completo
      params <- expand.grid(
        year = year,
        uf = uf,
        month = month,
        system = system,
        stringsAsFactors = FALSE
      )
    }
  }
  # Function to generate cache key
  # generate_cache_key <- function(year_i, uf_i, system_i, month_i = NULL) {
  #   if (!is.null(month_i)) {
  #     key_string <- paste(system_i, uf_i, year_i, month_i, sep = "_")
  #   } else {
  #     key_string <- paste(system_i, uf_i, year_i, sep = "_")
  #   }
  #   digest::digest(key_string, algo = "md5")
  # }
  generate_cache_key <- function(year_i, uf_i, system_i, month_i = NULL, is_national = FALSE) {
  if (is_national) {
    # Para sistemas nacionais, ignoramos UF na chave
    if (!is.null(month_i)) {
      key_string <- paste(system_i, year_i, month_i, sep = "_")
    } else {
      key_string <- paste(system_i, year_i, sep = "_")
    }
  } else {
    if (!is.null(month_i)) {
      key_string <- paste(system_i, uf_i, year_i, month_i, sep = "_")
    } else {
      key_string <- paste(system_i, uf_i, year_i, sep = "_")
    }
  }
  digest::digest(key_string, algo = "md5")
}
  # Function to get cache file path
  get_cache_path <- function(cache_key) {
    if (requireNamespace("arrow", quietly = TRUE)) { 
      file.path(cache_dir, paste0(cache_key, ".parquet"))
    } else { 
      file.path(cache_dir, paste0(cache_key, ".rds"))
    }    
  }

  # Function to check if cache is valid (not older than 30 days by default)
  is_cache_valid <- function(cache_path, max_age_days = 30) {
    if (!fs::file_exists(cache_path)) return(FALSE)
    
    file_info <- fs::file_info(cache_path)
    cache_age <- difftime(Sys.time(), file_info$modification_time, units = "days")
    
    cache_age <= max_age_days
  }
  
  # Load data from cache
  # Load data from cache
load_from_cache <- function(cache_path, year_i, uf_i, system_i, month_i = NULL, is_national = FALSE) {
  if (verbose) {
    if (is_national) {
      if (!is.null(month_i)) {
        cli::cli_alert_success("Loading from cache (national): {system_i} - {year_i} - Month {month_i}")
      } else {
        cli::cli_alert_success("Loading from cache (national): {system_i} - {year_i}")
      }
    } else {
      if (!is.null(month_i)) {
        cli::cli_alert_success("Loading from cache: {system_i} - {uf_i} - {year_i} - Month {month_i}")
      } else {
        cli::cli_alert_success("Loading from cache: {system_i} - {uf_i} - {year_i}")
      }
    }
  }
  
  if (requireNamespace("arrow", quietly = TRUE)) {
    return(arrow::read_parquet(cache_path)) 
  } else {
    return(readRDS(cache_path))
  }
}

# Save data to cache
save_to_cache <- function(data, cache_path, year_i, uf_i, system_i, month_i = NULL, is_national = FALSE) {
  if (verbose) {
    if (is_national) {
      if (!is.null(month_i)) {
        cli::cli_alert_info("Saving to cache (national): {system_i} - {year_i} - Month {month_i}")
      } else {
        cli::cli_alert_info("Saving to cache (national): {system_i} - {year_i}")
      }
    } else {
      if (!is.null(month_i)) {
        cli::cli_alert_info("Saving to cache: {system_i} - {uf_i} - {year_i} - Month {month_i}")
      } else {
        cli::cli_alert_info("Saving to cache: {system_i} - {uf_i} - {year_i}")
      }
    }
  }
  
  if (requireNamespace("arrow", quietly = TRUE)) {
    arrow::write_parquet(data, cache_path)
  } else { 
    saveRDS(data, cache_path)
  }
  
  return(data)
}

  
  # Auxiliary function to download and process a single combination
  download_one <- function(year_i, uf_i, system_i, month_i = NULL, p = NULL, 
                           use_cache, force_redownload, cache_dir, verbose, is_national=FALSE) {
    
    if (verbose && !is.null(p)) {
        if (is_national) {
        p(message = sprintf("Processing (National-SINAN): %s - %s", system_i, year_i))
      } else { 
          if (!is.null(month_i)) {
          p(message = sprintf("Processing: %s - %s - %s - Month %s", system_i, uf_i, year_i, month_i))
        } else {
          p(message = sprintf("Processing: %s - %s - %s", system_i, uf_i, year_i))
        }
      }
      
    }
    
    # Generate cache key and path
    cache_key <- generate_cache_key(year_i, uf_i, system_i, month_i, is_national)
    cache_path <- get_cache_path(cache_key)
    
    # Check cache first (if enabled and not forcing re-download)
    if (use_cache && !force_redownload && fs::file_exists(cache_path) && is_cache_valid(cache_path)) {
      if (verbose && !is.null(p)) {
        if (!is.null(month_i)) {
          p(message = sprintf("From cache: %s - %s - %s - Month %s", system_i, uf_i, year_i, month_i))
        } else {
          p(message = sprintf("From cache: %s - %s - %s", system_i, uf_i, year_i))
        }
      }
      tryCatch({
        return(load_from_cache(cache_path, year_i, uf_i, system_i, month_i, is_national))
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
        save_to_cache(data, cache_path, year_i, uf_i, system_i, month_i, is_national)
      }
      
      if (verbose && !is.null(p)) {
        if (!is.null(month_i)) {
          p(message = sprintf("Completed: %s - %s - %s - Month %s", system_i, uf_i, year_i, month_i))
        } else {
          p(message = sprintf("Completed: %s - %s - %s", system_i, uf_i, year_i))
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
  n_months <- if (is.null(month)) 1 else length(month)
  total_tasks <- length(uf) * length(year) * n_months

  # Execute downloads (parallel or sequential)
  if (parallel && total_tasks > 1) {

    if (verbose) {cli::cli_alert_info("Parallel processing enabled ({workers} workers)")}

    list_of_dfs <- furrr::future_map(
      seq_len(nrow(params)),
      function(i) {

        if (!is.null(month)) {
          download_one(
            year_i = params$year[i],
            uf_i = params$uf[i],
            system_i = params$system[i],
            month_i = params$month[i],
            use_cache = use_cache,
            force_redownload = force_redownload,
            cache_dir = cache_dir,
            verbose = FALSE,
            is_national = is_national
          )
        } else {
          download_one(
            year_i = params$year[i],
            uf_i = params$uf[i],
            system_i = params$system[i],
            month_i = NULL,
            use_cache = use_cache,
            force_redownload = force_redownload,
            cache_dir = cache_dir,
            verbose = FALSE,
            is_national = is_national
          )
        }

      },
      .options = furrr::furrr_options(
        seed = TRUE,
        packages = c(
          "cli",
          "fs",
          "digest",
          "microdatasus",
          "dplyr",
          "arrow"
        )
      )
    )

  } else {
    list_of_dfs <- purrr::map(
      seq_len(nrow(params)),
      function(i) {
        download_one(
          year_i = params$year[i],
          uf_i = params$uf[i],
          system_i = params$system[i],
          month_i = if (!is.null(month)) params$month[i] else NULL,
          use_cache = use_cache,
          force_redownload = force_redownload,
          cache_dir = cache_dir,
          verbose = verbose,
          is_national = is_national
        )
      }
    )
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

  #NEW national system
  # Se for sistema nacional, filtrar pelos UFs solicitados
  if (is_national && exists("all_ufs")) {
    
    # Identificar coluna de UF (diferentes sistemas podem ter nomes diferentes)
    uf_cols <- c("SG_UF", "SG_UF_NOT")
    uf_col <- uf_cols[uf_cols %in% names(combined_data)]
    data.table::setDT(combined_data)
    combined_data <- combined_data[get(uf_col) %in% all_ufs]
  }

  # Add cache metadata as attribute
  if (use_cache) {
    cache_info <- list(
      cache_dir = cache_dir,
      cached_items = nrow(params),
      retrieved_from_cache = nrow(params) - length(list_of_dfs),
      cache_size = sum(file.info(list.files(cache_dir, full.names = TRUE))$size, na.rm = TRUE),
      timestamp = Sys.time()
    )
    attr(combined_data, "cache_info") <- cache_info
  }
    
  if (verbose) {
    cli::cli_alert_success("Process completed successfully!")
    cli::cli_alert_info("Total records: {format(nrow(combined_data), big.mark = ',')}")
    cli::cli_alert_info("Total columns: {ncol(combined_data)}")
    if (use_cache) {
      cli::cli_alert_info("Cache saved to: {cache_dir}")
    }
  }
  
  if (!inherits(combined_data, "climasus_df")) {
      # Create new climasus_df
      meta <- list(
        system = NULL,
        stage = "import",
        type = "raw",
        spatial = inherits(combined_data, "sf"),
        temporal = NULL,
        created = Sys.time(),
        modified = Sys.time(),
        history = sprintf(
          "[%s] Imported datasus with climasus4r",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        ),
        user = list()
      )

      base_classes <- setdiff(class(combined_data), "climasus_df")
      combined_data <- structure(
        combined_data,
        climasus_meta = meta,
        class = c("climasus_df", base_classes)
      )
    } else {
      # Already climasus_df - update metadata
      combined_data <- climasus_meta(combined_data, system = system, stage = "import", type = "raw")
      combined_data <- climasus_meta(combined_data, add_history = "Imported datasus with climasus4r")
    }

  return(combined_data)
}



#Helper functions
#' @title Regional Definitions for Brazilian States
#' @description Internal dataset with state to region mappings including Biomes, 
#' Hydrography, and Epidemiological clusters.
#' @noRd
.br_regions <- list(
  # --- IBGE Macro-regions ---
  norte = c("AC", "AP", "AM", "PA", "RO", "RR", "TO"),
  nordeste = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE"),
  centro_oeste = c("DF", "GO", "MT", "MS"),
  sudeste = c("ES", "MG", "RJ", "SP"),
  sul = c("PR", "RS", "SC"),
  
  # --- Biomes (Ecological Borders) ---
  amazonia_legal = c("AC", "AP", "AM", "PA", "RO", "RR", "MT", "MA", "TO"),
  mata_atlantica = c("AL", "BA", "CE", "ES", "GO", "MA", "MG", "MS", "PB", 
                     "PE", "PI", "PR", "RJ", "RN", "RS", "SC", "SE", "SP"),
  caatinga = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "MG"),
  cerrado = c("BA", "DF", "GO", "MA", "MG", "MS", "MT", "PA", "PI", "PR", "RO", "SP", "TO"),
  pantanal = c("MT", "MS"),
  pampa = c("RS"),
  
  # --- Hydrography & Climate ---
  bacia_amazonica = c("AC", "AM", "AP", "MT", "PA", "RO", "RR"),
  bacia_sao_francisco = c("AL", "BA", "DF", "GO", "MG", "PE", "SE"),
  bacia_parana = c("GO", "MG", "MS", "PR", "SP"),
  bacia_tocantins = c("GO", "MA", "PA", "TO"),
  semi_arido = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "MG"),
  
  # --- Health, Agriculture & Geopolitics ---
  matopiba = c("MA", "TO", "PI", "BA"),
  arco_desmatamento = c("RO", "AC", "AM", "PA", "MT", "MA"),
  dengue_hyperendemic = c("GO", "MS", "MT", "PR", "RJ", "SP"),
  sudene = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "MG", "ES"),
  fronteira_brasil = c("AC", "AM", "AP", "MT", "MS", "PA", "PR", "RO", "RR", "RS", "SC")
)

#' @description Multilingual aliases for the region argument (EN, ES, PT).
#' @noRd
.region_aliases <- list(
  # English
  north = "norte", northeast = "nordeste", central_west = "centro_oeste", 
  southeast = "sudeste", south = "sul", amazon = "amazonia_legal", 
  legal_amazon = "amazonia_legal", atlantic_forest = "mata_atlantica",
  semi_arid = "semi_arido", border = "fronteira_brasil",
  # Spanish
  noreste = "nordeste", sudeste = "sudeste", sur = "sul", 
  amazonia = "amazonia_legal", bosque_atlantico = "mata_atlantica",
  semiarido = "semi_arido", frontera = "fronteira_brasil"
)