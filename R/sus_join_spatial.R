#' @title Spatially Link SUS Data to Brazilian Geographic Units
#' @description Performs a spatial join between a data.frame containing SUS data 
#' and official Brazilian geographic boundaries (state, municipality, census tract) 
#' or geocodes postal codes (CEP). This function is the core spatial engine for the 
#' entire climasus4r package and serves as the foundation for all spatial analyses.
#'
#' @param df A `data.frame` containing health data with a geographic identifier column.
#'   The data frame should have a `system` column created by `detect_health_system()`.
#' @param level Character string specifying the geographic aggregation level.
#'   Options: `"state"`, `"munic"` (municipality), `"census"` (census tract), `"cep"` (postal code).
#'   Default is `"munic"`.
#'   **Note**: `"cep"` level is only available for SIH and CNES systems.
#' @param join_col Character string with the name of the column in `df` containing 
#'   the geographic identifier. If `NULL` (default), the function will automatically 
#'   detect the appropriate column based on the `level` and common SUS column patterns.
#' @param year Numeric. Year of the geographic boundaries to use. Default is 2020.
#'   Only applicable for `"state"`, `"munic"`, and `"census"` levels.
#' @param simplified Logical. If `TRUE` (default), uses simplified geometries for 
#'   faster processing and visualization. Only applicable for polygon-based levels.
#' @param lang Character string specifying the language for messages.
#'   Options: `"pt"` (Portuguese, default), `"en"` (English), `"es"` (Spanish).
#' @param use_cache Logical. If `TRUE` (default), uses cached spatial data to avoid 
#'   re-downloads and improve performance.
#' @param cache_dir Character string specifying the directory to store cached files.
#'   Default is `"~/.climasus4r_cache"`.
#' @param verbose Logical. If `TRUE` (default), prints detailed progress information 
#'   including cache status, download progress, and join statistics.
#'
#' @return An `sf` object (Simple Features data.frame) with all original columns from 
#'   `df` plus a `geometry` column containing the spatial geometries:
#'   \itemize{
#'     \item **state/munic/census**: POLYGON or MULTIPOLYGON geometries
#'     \item **cep**: POINT geometries (geocoded locations)
#'   }
#'
#' @details
#' **Geographic Levels**:
#' \itemize{
#'   \item **state**: Brazilian state (UF) boundaries from IBGE via `geobr::read_state()`
#'   \item **munic**: Municipality boundaries from IBGE via `geobr::read_municipality()`
#'   \item **census**: Census tract boundaries from IBGE via `geobr::read_census_tract()`
#'   \item **cep**: Postal code point geocoding via `geocodebr::busca_por_cep()` 
#'     (only for SIH and CNES systems)
#' }
#'
#' **System-Aware Logic**:
#' The function expects a `system` column in `df` (created by `detect_health_system()`).
#' The `"cep"` level is restricted to SIH and CNES systems because only these systems 
#' reliably contain postal code information.
#'
#' **Automatic Column Detection**:
#' If `join_col = NULL`, the function automatically detects the appropriate geographic 
#' column based on common SUS patterns:
#' \itemize{
#'   \item **Municipality**: `residence_municipality_code`, `municipality_code`, 
#'     `codigo_municipio`, `CODMUNRES`, etc.
#'   \item **State**: `SG_UF`, `UF`, `UF_ZI`
#'   \item **CEP**: `cep`, `cep_paciente`, `codigo_postal`, `zip_code`, etc.
#'   \item **Census tract**: `COD_SETOR`, `code_census_tract`
#' }
#'
#' **Caching Strategy**:
#' Spatial data is cached as Parquet files in `~/.climasus4r_cache/` to:
#' \itemize{
#'   \item Avoid repeated downloads
#'   \item Improve performance (10-100x faster)
#'   \item Enable offline reuse
#' }
#' Cache files are named: `state_shp.parquet`, `munic_shp.parquet`, `census_shp.parquet`.
#' CEP geocoding is never cached as it uses an API.
#'
#' **Code Normalization**:
#' \itemize{
#'   \item **Municipality codes**: Accepts 6 or 7 digits; 7-digit codes have the 
#'     verification digit removed automatically
#'   \item **CEP**: Always zero-padded to 8 characters
#'   \item All join columns are coerced to character type
#' }
#' @references 
#' Pereira, R.H.M.; Goncalves, C.N.; et. all (2019) geobr: Loads Shapefiles of Official Spatial Data Sets of Brazil. GitHub repository - https://github.com/ipeaGIT/geobr.
#' Pereira, Rafael H. M.; Barbosa, Rogerio J. (2023) censobr: Download Data from Brazil’s Population Census. R package version v0.4.0, https://CRAN.R-project.org/package=censobr. DOI: 10.32614/CRAN.package.censobr.
#' 
#' @examples
#' \dontrun{
#' library(climasus4r)
#'
#' # Example 1: Link mortality data to municipalities
#' df_sim <- sus_data_import(uf = "SP", year = 2023, system = "SIM-DO") %>%
#'   sus_data_standardize(lang = "pt")
#'
#' sf_sim <- sus_join_spatial(
#'   df = df_sim,
#'   level = "munic",
#'   lang = "pt"
#' )
#'
#' # Plot the results
#' library(ggplot2)
#' ggplot(sf_sim) +
#'   geom_sf(aes(fill = n_deaths)) +
#'   theme_minimal()
#'
#' # Example 2: Link to states for regional analysis
#' sf_states <- sus_join_spatial(
#'   df = df_sim,
#'   level = "state",
#'   lang = "pt"
#' )
#'
#' # Example 3: Geocode CEP for hospitalization data (SIH only)
#' df_sih <- sus_data_import(uf = "RJ", year = 2023, system = "SIH-RD") %>%
#'   sus_data_standardize(lang = "pt")
#'
#' sf_cep <- sus_join_spatial(
#'   df = df_sih,
#'   level = "cep",
#'   lang = "pt"
#' )
#'
#' # Example 4: Census tract level analysis
#' sf_census <- sus_join_spatial(
#'   df = df_sim,
#'   level = "census",
#'   year = 2010,
#'   lang = "pt"
#' )
#' }
#' @export
#' @importFrom geobr read_state read_municipality read_census_tract
#' @importFrom geocodebr busca_por_cep
#' @importFrom dplyr left_join filter mutate
#' @importFrom sf st_as_sf st_read st_write
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_danger cli_alert_warning cli_abort cli_progress_step
#' @importFrom arrow write_parquet read_parquet
#' @importFrom rlang .data
sus_join_spatial <- function(df,
                             level = "munic",
                             join_col = NULL,
                             year = 2020,
                             simplified = TRUE,
                             lang = "pt",
                             use_cache = TRUE,
                             cache_dir = "~/.climasus4r_cache",
                             verbose = TRUE) {
  
  # ============================================================================
  # 1. INPUT VALIDATION
  # ============================================================================
  
  # Validate data frame
  if (!is.data.frame(df)) {
    cli::cli_abort("Input 'df' must be a data.frame.")
  }
  
  if (nrow(df) == 0) {
    cli::cli_abort("Input 'df' is empty (0 rows).")
  }
  
  # Validate language
  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_abort("lang must be one of: 'en', 'pt', 'es'")
  }
  
  # Get multilingual messages
  msg <- get_spatial_messages(lang)
  
  # Validate level
  valid_levels <- c("state", "munic", "census", "cep")
  if (!level %in% valid_levels) {
    cli::cli_alert_danger(msg$invalid_level)
    cli::cli_alert_info(msg$valid_levels)
    cli::cli_abort(paste0("Invalid level: '", level, "'"))
  }
  
  # ============================================================================
  # 2. SYSTEM-AWARE VALIDATION (CEP restriction)
  # ============================================================================
  
  if (level == "cep") {
    # Check if system column exists
    if (!"system" %in% names(df)) {
      cli::cli_abort(msg$system_column_missing)
    }
    
    # Get unique systems in the data
    systems_in_data <- unique(df$system)
    allowed_systems <- c("SIH", "CNES")
    
    # Check if any system is allowed
    if (!any(systems_in_data %in% allowed_systems)) {
      cli::cli_abort(msg$cep_restricted)
    }
    
    if (verbose) {
      cli::cli_alert_info(msg$cep_allowed)
    }
  }
  
  # ============================================================================
  # 3. AUTOMATIC JOIN COLUMN DETECTION
  # ============================================================================
  
  if (is.null(join_col)) {
    if (verbose) {
      cli::cli_alert_info(msg$detecting_column)
    }
    
    # Define column candidates based on level
    common_cols <- switch(level,
      "munic" = c(
        "residence_municipality_code",
        "municipality_code",
        "residence_municipality",
        "codigo_municipio",
        "codigo_municipio_residencia",
        #"codigo_municipio_nascimento",
        "codigo_municipio_ocorrencia",
        "codigo_municipio_paciente",
        "patient_municipality_code",
        "uf_municipio_estabelecimento",
        "facility_uf_municipality",
        "uf_municipio_establecimiento",
        "CODMUNRES"
      ),
      "state" = c("SG_UF", "UF", "UF_ZI", "state_code", "codigo_uf"),
      "cep" = c(
        "cep",
        "cep_paciente",
        "codigo_postal",
        "codigo_postal_paciente",
        "zip_code",
        "patient_zip_code"
      ),
      "census" = c("COD_SETOR", "code_census_tract", "codigo_setor_censitario")
    )
    
    # Find first matching column
    join_col <- names(df)[names(df) %in% common_cols][1]
    
    if (is.na(join_col) || length(join_col) == 0) {
      cli::cli_alert_danger(msg$no_column_found)
      cli::cli_alert_info(paste0(msg$expected_columns, paste(common_cols, collapse = ", ")))
      cli::cli_abort(paste0("No valid geographic column found for level: ", level))
    }
    
    if (verbose) {
      cli::cli_alert_success(paste0(msg$column_detected, join_col))
    }
  } else {
    # Validate user-provided join_col
    if (!join_col %in% names(df)) {
      cli::cli_abort(paste0(msg$column_not_found, join_col))
    }
  }
  
  # ============================================================================
  # 4. CODE NORMALIZATION
  # ============================================================================
  
  # Convert join column to character
  df[[join_col]] <- as.character(df[[join_col]])
  
  # Normalize municipality codes (remove verification digit if 7 digits)
  if (level == "munic") {
  df <- df %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(join_col),
        ~ ifelse(nchar(.x) == 7, substr(.x, 1, 6), .x)
      )
    )
  }
  
  # Normalize CEP (zero-pad to 8 characters)
  if (level == "cep") {
    df[[join_col]] <- stringr::str_pad(df[[join_col]], 8, pad = "0")
  }
  
  # ============================================================================
  # 5. CACHE SETUP
  # ============================================================================
  
  if (use_cache && level != "cep") {
    cache_dir <- path.expand(cache_dir)
    if (!fs::dir_exists(cache_dir)) {
      if (verbose) {
        cli::cli_alert_info(paste0(msg$creating_cache, cache_dir))
      }
      fs::dir_create(cache_dir, recursive = TRUE)
    }
    
    if (verbose) {
      cli::cli_alert_info(msg$cache_enabled)
    }
  }
  
  # ============================================================================
  # 6. SPATIAL DATA RETRIEVAL
  # ============================================================================
  
  if (level %in% c("state", "munic", "census")) {
    # Use cache-enabled retrieval for polygon-based levels
    spatial_df <- get_spatial_data_with_cache(
      level = level,
      year = year,
      simplified = simplified,
      cache_dir = cache_dir,
      use_cache = use_cache,
      lang = lang,
      verbose = verbose
    )
    
    # Determine the code column name in spatial data
    code_col_spatial <- switch(level,
      "state" = "code_state",
      "munic" = "code_muni",
      "census" = "code_tract"
    )
    
    # Ensure code column is character
    spatial_df[[code_col_spatial]] <- as.character(spatial_df[[code_col_spatial]])
    
    # ============================================================================
    # 7. PERFORM SPATIAL JOIN
    # ============================================================================
    
    if (verbose) {
      cli::cli_alert_info(msg$joining_data)
    }
    
    if (nchar(df[[join_col]][1]) == 6) {
      cli::cli_alert_info(
        switch(
          lang,
          "pt" = "Convertendo codigos de municipio (6 to 7 digitos - IBGE)",
          "en" = "Converting municipality codes (6 to 7 digits - IBGE)",
          "es" = "Convirtiendo codigos municipales (6 to 7 digitos - IBGE)"
        )
      )

      df[[join_col]] <- .convert_muni_6_to_7(
        muni_code_6 = df[[join_col]],
        spatial_df = spatial_df
      )
    }
  
    df <- df %>% dplyr::arrange(.data[[join_col]])
    spatial_df <- spatial_df %>% dplyr::arrange(.data[[code_col_spatial]])
    
    # Perform left join
    result_sf <- dplyr::inner_join(
      df,
      spatial_df,
      by = stats::setNames(code_col_spatial, join_col)
    )
    
    # Convert to sf object
    result_sf <- result_sf %>% sf::st_as_sf() 
    
    # Remove rows with missing geometries
    n_before <- nrow(result_sf)
    result_sf <- result_sf %>% dplyr::filter(!is.na(sf::st_dimension(.data$geom)))
    n_after <- nrow(result_sf)
    
    if (verbose && n_before > n_after) {
      n_removed <- n_before - n_after
      cli::cli_alert_warning(paste0(msg$rows_removed, n_removed))
    }
    
  } else if (level == "cep") {
    # ============================================================================
    # 8. CEP GEOCODING (API-based, no cache)
    # ============================================================================
    
    if (verbose) {
      cli::cli_alert_info(msg$geocoding_cep)
    }
    
    # Get unique CEPs
    unique_ceps <- unique(df[[join_col]])
    
    if (verbose) {
      cli::cli_alert_info(paste0(msg$geocoding_count, length(unique_ceps)))
    }
    
    # Geocode using geocodebr
    tryCatch({
      geo_points <- geocodebr::busca_por_cep(
        cep = unique_ceps,
        resultado_sf = TRUE,
        verboso = FALSE,
        cache = FALSE
      )
      geo_points$cep <- stringr::str_replace_all(geo_points$cep, "\\D", "")

      df <- df %>% dplyr::arrange(.data[[join_col]])
      spatial_df <- spatial_df %>% dplyr::arrange(.data[[code_col_spatial]])
      # Join geocoded points back to original data
      result_sf <- dplyr::inner_join(
        df,
        geo_points,
        by = stats::setNames("cep", join_col),
        relationship = "many-to-many"
      )
      
      # Convert to sf object
      result_sf <- sf::st_as_sf(result_sf)
      
    }, error = function(e) {
      cli::cli_abort(paste0(msg$geocoding_error, e$message))
    })
  }
  
  # ============================================================================
  # 9. FINAL VALIDATION AND SUCCESS MESSAGE
  # ============================================================================
  
  if (verbose) {
    cli::cli_alert_success(msg$join_success)
    cli::cli_alert_info(paste0(msg$final_rows, nrow(result_sf)))
  }
  
  return(result_sf)
}


# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Get Spatial Data with Caching
#'
#' Retrieves spatial data from IBGE with intelligent caching to improve performance.
#' Spatial data is cached as Parquet files for fast reloading.
#'
#' @param level Geographic level ("state", "munic", "census")
#' @param year Year of the boundaries
#' @param simplified Use simplified geometries
#' @param cache_dir Cache directory path
#' @param use_cache Whether to use cache
#' @param lang Language for messages
#' @param verbose Print messages
#'
#' @return sf object with spatial data
#' @keywords internal
#' @noRd
get_spatial_data_with_cache <- function(level, year, simplified, cache_dir, 
                                        use_cache, lang, verbose) {
  
  msg <- get_spatial_messages(lang)
  
  # Define cache file name
  cache_file <- file.path(
    cache_dir,
    paste0(level, "_", year, "_", ifelse(simplified, "simp", "full"), ".gpkg")
  )
  
  # Try to load from cache
  if (use_cache && file.exists(cache_file)) {
    if (verbose) {
      cli::cli_alert_success(paste0(msg$loading_cache, basename(cache_file)))
    }
    
    tryCatch({
      #spatial_df <- arrow::read_parquet(cache_file)
      spatial_df <- sf::st_read(cache_file)
      spatial_df <- sf::st_as_sf(spatial_df)
      return(spatial_df)
    }, error = function(e) {
      if (verbose) {
        cli::cli_alert_warning(msg$cache_error)
      }
    })
  }
  
  # Download from IBGE
  if (verbose) {
    cli::cli_alert_info(msg$downloading_data)
  }
  
  spatial_df <- switch(level,
    "state" = geobr::read_state(
      code_state = "all",
      year = year,
      simplified = simplified,
      showProgress = verbose,
      cache = FALSE
    ),
    "munic" = geobr::read_municipality(
      code_muni = "all",
      year = year,
      simplified = simplified,
      showProgress = verbose
    ),
    "census" = geobr::read_census_tract(
      code_tract = "all",
      year = year,
      simplified = simplified,
      showProgress = verbose,
      cache = FALSE
    )
  )
  
  # Save to cache
  if (use_cache) {
    if (verbose) {
      cli::cli_alert_info(msg$saving_cache)
    }
    
    tryCatch({
      #arrow::write_parquet(spatial_df, cache_file)
      sf::st_write(spatial_df, cache_file, driver = "GPKG", quiet = TRUE, delete_dsn = TRUE, append = TRUE)
      if (verbose) {
        cli::cli_alert_success(msg$cache_saved)
      }
    }, error = function(e) {
      if (verbose) {
        cli::cli_alert_warning(paste0(msg$cache_save_error, e$message))
      }
    })
  }
  
  return(spatial_df)
}


#' Get Multilingual Messages for Spatial Operations
#'
#' Returns user interface messages in the specified language for spatial join operations.
#'
#' @param lang Character. Language code: "en", "pt", "es"
#'
#' @return List of translated UI messages
#' @keywords internal
#' @noRd
get_spatial_messages <- function(lang) {
  messages <- list(
    en = list(
      invalid_level = "Invalid geographic level specified.",
      valid_levels = "Valid levels are: 'state', 'munic', 'census', 'cep'.",
      system_column_missing = "Column 'system' not found in data. Please run detect_health_system() first.",
      cep_restricted = "Level 'cep' is only available for SIH and CNES systems.",
      cep_allowed = "CEP geocoding is available for this system.",
      detecting_column = "Auto-detecting geographic column...",
      no_column_found = "No valid geographic column found for the selected level.",
      expected_columns = "Expected columns: ",
      column_detected = "Geographic column detected: ",
      column_not_found = "Column not found in data frame: ",
      creating_cache = "Creating cache directory: ",
      cache_enabled = "Cache: ENABLED",
      loading_cache = "Loading from cache: ",
      cache_error = "Cache loading failed. Downloading fresh data...",
      downloading_data = "Downloading spatial data from IBGE...",
      saving_cache = "Saving to cache...",
      cache_saved = "Spatial data cached successfully.",
      cache_save_error = "Failed to save cache: ",
      joining_data = "Performing spatial join...",
      rows_removed = "Removed rows with missing geometries: ",
      geocoding_cep = "Geocoding postal codes (CEP)...",
      geocoding_count = "Geocoding unique CEPs: ",
      geocoding_error = "Geocoding failed: ",
      join_success = "Spatial join completed successfully!",
      final_rows = "Final dataset rows: "
    ),
    pt = list(
      invalid_level = "Nivel geografico invalido especificado.",
      valid_levels = "Niveis validos sao: 'state', 'munic', 'census', 'cep'.",
      system_column_missing = "Coluna 'system' nao encontrada nos dados. Execute detect_health_system() primeiro.",
      cep_restricted = "Nivel 'cep' disponivel apenas para os sistemas SIH e CNES.",
      cep_allowed = "Geocodificacao de CEP disponivel para este sistema.",
      detecting_column = "Auto-detectando coluna geografica...",
      no_column_found = "Nenhuma coluna geografica valida encontrada para o nivel selecionado.",
      expected_columns = "Colunas esperadas: ",
      column_detected = "Coluna geografica detectada: ",
      column_not_found = "Coluna nao encontrada no data frame: ",
      creating_cache = "Criando diretorio de cache: ",
      cache_enabled = "Cache: ATIVADO",
      loading_cache = "Carregando do cache: ",
      cache_error = "Falha ao carregar cache. Baixando dados novos...",
      downloading_data = "Baixando dados espaciais do IBGE...",
      saving_cache = "Salvando no cache...",
      cache_saved = "Dados espaciais armazenados em cache com sucesso.",
      cache_save_error = "Falha ao salvar cache: ",
      joining_data = "Realizando juncao espacial...",
      rows_removed = "Linhas removidas com geometrias faltantes: ",
      geocoding_cep = "Geocodificando codigos postais (CEP)...",
      geocoding_count = "Geocodificando CEPs unicos: ",
      geocoding_error = "Geocodificacao falhou: ",
      join_success = "Juncao espacial concluida com sucesso!",
      final_rows = "Linhas no dataset final: "
    ),
    es = list(
      invalid_level = "Nivel geografico invalido especificado.",
      valid_levels = "Niveles validos son: 'state', 'munic', 'census', 'cep'.",
      system_column_missing = "Columna 'system' no encontrada en los datos. Ejecute detect_health_system() primero.",
      cep_restricted = "Nivel 'cep' disponible solo para los sistemas SIH y CNES.",
      cep_allowed = "Geocodificacion de CEP disponible para este sistema.",
      detecting_column = "Auto-detectando columna geografica...",
      no_column_found = "Ninguna columna geografica valida encontrada para el nivel seleccionado.",
      expected_columns = "Columnas esperadas: ",
      column_detected = "Columna geografica detectada: ",
      column_not_found = "Columna no encontrada en el data frame: ",
      creating_cache = "Creando directorio de cache: ",
      cache_enabled = "Cache: ACTIVADO",
      loading_cache = "Cargando desde cache: ",
      cache_error = "Fallo al cargar cache. Descargando datos nuevos...",
      downloading_data = "Descargando datos espaciales del IBGE...",
      saving_cache = "Guardando en cache...",
      cache_saved = "Datos espaciales almacenados en cache con exito.",
      cache_save_error = "Fallo al guardar cache: ",
      joining_data = "Realizando union espacial...",
      rows_removed = "Filas eliminadas con geometrias faltantes: ",
      geocoding_cep = "Geocodificando codigos postales (CEP)...",
      geocoding_count = "Geocodificando CEPs unicos: ",
      geocoding_error = "Geocodificacion fallo: ",
      join_success = "Union espacial completada con exito!",
      final_rows = "Filas en el dataset final: "
    )
  )
  
  return(messages[[lang]])
}

#' Get Multilingual Messages for Spatial Operations
#'
#' Returns user interface messages in the specified language for spatial join operations.
#'
#' @param lang Character. Language code: "en", "pt", "es"
#'
#' @return List of translated UI messages
#' @keywords internal
#' @noRd
# .convert_muni_6_to_7 <- function(muni_code_6, spatial_df) {
#   muni_code_6 <- as.character(muni_code_6)

#   spatial_df %>%
#     dplyr::mutate(
#       code_muni_6 = substr(.data$code_muni, 1, 6)
#     ) %>%
#     dplyr::select(.data$code_muni_6, .data$code_muni) %>%
#     dplyr::distinct() %>%
#     dplyr::right_join(
#       data.frame(code_muni_6 = muni_code_6, stringsAsFactors = FALSE),
#       by = "code_muni_6"
#     ) %>%
#     dplyr::pull(.data$code_muni)
# }

.convert_muni_6_to_7 <- function(muni_code_6, spatial_df) {

  muni_code_6 <- as.character(muni_code_6)

  lookup <- spatial_df %>%
    dplyr::transmute(
      code_muni_6 = substr(.data$code_muni, 1, 6),
      code_muni_7 = .data$code_muni
    ) %>%
    dplyr::distinct()

  lookup$code_muni_7[match(muni_code_6, lookup$code_muni_6)]
}