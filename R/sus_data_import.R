#' Import and preprocess data from DATASUS with intelligent caching
#'
#' This function acts as a wrapper for `microdatasus::fetch_datasus`,
#' simplifying the download and reading of data from systems like SIM, SINAN, and SIH.
#' It includes parallel processing, caching, and user-friendly CLI feedback.
#'
#' @param uf A string or vector of strings with state abbreviations (e.g., "RJ", c("SP", "MG")).
#' @param year An integer or vector of integers with the desired years.
#' @param month An integer or vector of integers with the desired months. This argument is only used with the healh information systems SIH, CNES and SIA
#' @param system A string indicating the information system ("SIM", "SINAN", "SIH", etc.).
#' @param use_cache Logical. If TRUE, will use cached data to avoid re-downloads. Default is TRUE.
#' @param cache_dir Character. Directory to store cached files. Default is "~/.climasus4r_cache".
#' @param force_redownload Logical. If TRUE, ignores cache and re-downloads everything.
#' @param parallel Logical. If TRUE, will use parallel processing for multiple downloads. Default is TRUE.
#' @param workers Integer. Number of parallel workers to use. Default is 4.
#' @param verbose Logical. If TRUE, prints detailed progress information. Default is TRUE.
#'
#' @return A `data.frame` (or `tibble`) with the requested data, combined if multiple UFs/years are requested.
#'
#' @importFrom dplyr bind_rows
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_alert_danger
#' @importFrom future.apply future_mapply
#' @importFrom progressr with_progress progressor
#' @importFrom digest digest
#' @importFrom readr write_rds read_rds
#' @importFrom fs dir_exists dir_create file_exists file_info
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Use cache for faster repeated downloads
#' df_sim <- sus_data_import(
#'   uf = "RJ", 
#'   year = 2022, 
#'   system = "SIM",
#'   use_cache = TRUE
#' )
#'
#' # Force re-download even if cached
#' df_dengue <- sus_data_import(
#'   uf = c("SP", "MG"), 
#'   year = 2023, 
#'   system = "SINAN-DENGUE",
#'   use_cache = TRUE,
#'   force_redownload = TRUE
#' )
#' }

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





