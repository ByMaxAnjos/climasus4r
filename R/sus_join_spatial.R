#' @title Spatially Link SUS Data to Brazilian Geographic Units
#' @description Performs a spatial join between a data.frame containing SUS data
#' and official Brazilian geographic boundaries (state, municipality, census tract)
#' or geocodes postal codes (CEP). This function is the core spatial engine for the
#' entire climasus4r package and serves as the foundation for all spatial analyses.
#'
#' @param df A `data.frame` containing health data with a geographic identifier column.
#'   The data frame should have a `system` column created by `detect_health_system()`.
#' @param level Character string specifying the geographic aggregation level. Default is `"munic"`.
#'   Options: 
#'   \itemize{
#'     \item \strong{Administrative:} "munic" (municipality). Default. 
#'     \item \strong{Health:} "health_region", "health_facilities", "schools"
#'     \item \strong{Environmental:} "amazon", "biomes", "conservation_units", 
#'           "disaster_risk_area", "semiarid", "indigenous_land"
#'     \item \strong{Urban:} "neighborhood", "urban_area", "metro_area", 
#'           "urban_concentrations", "pop_arrangements",
#'     \item \strong{Miscellaneous:} "cep" (postal code)
#'   }
#'   **Note**: `"cep"` level is only available for SIH and CNES systems.
#' @param join_col Character string with the name of the column in `df` containing
#'   the geographic identifier. If `NULL` (default), the function will automatically
#'   detect the appropriate column based on the `level` and common SUS column patterns.
#' @param lang Character string specifying the language for messages.
#'   Options: `"pt"` (Portuguese, default), `"en"` (English), `"es"` (Spanish).
#' @param use_cache Logical. If `TRUE` (default), uses cached spatial data to avoid
#'   re-downloads and improve performance.
#' @param cache_dir Character string specifying the directory to store cached files.
#'   Default is `"~/.climasus4r_cache/spatial"`.
#' @param verbose Logical. If `TRUE` (default), prints detailed progress information
#'   including cache status, download progress, and join statistics.
#'
#' @return An `sf` object (Simple Features data.frame) with all original columns from
#'   `df` plus a `geometry` column containing the spatial geometries:
#'   \itemize{
#'     \item **/munic/neighborhood**: POLYGON or MULTIPOLYGON geometries
#'     \item **/cnes/cep**: POINT geometries (geocoded locations)
#'   }
#'
#' @details
#' **Caching Strategy**:
#' Spatial data is cached as Parquet files in `~/.climasus4r_cache/spatial` to:
#' \itemize{
#'   \item Avoid repeated downloads
#'   \item Improve performance (10-100x faster)
#'   \item Enable offline reuse
#' }
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
#'   level = "school",
#'   lang = "pt"
#' )
#' }
#' @export
#' @importFrom glue glue
#' @importFrom rlang .data
sus_join_spatial <- function(
  df,
  level = "munic",
  join_col = NULL,
  lang = "pt",
  use_cache = TRUE,
  cache_dir = "~/.climasus4r_cache/spatial",
  verbose = TRUE
) {
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
  check_spatial(lang)
  # Get multilingual messages
  msg <- get_spatial_messages(lang)

  # Validate level
  valid_levels <- c("munic", "cep", "school","health_region","amazon",
    "semiarid", "biomes", "conservation_units", "disaster_risk_area", 
    "indigenous_land", "urban_area", "metro_area", "urban_concentrations",
    "pop_arrangements", "health_facilities", "neighborhood" 
    )
  if (!level %in% valid_levels) {
    cli::cli_alert_danger(msg$invalid_level)
    cli::cli_alert_info(msg$valid_levels)
    cli::cli_abort(paste0("Invalid level: '", level, "'"))
  }

  # Check if data is climasus_df
  if (inherits(df, "climasus_df")) {

    # Minimum required stage
    required_stage <- "stand"
    current_stage  <- climasus_meta(df, "stage")

    if (!is_stage_at_least(current_stage, required_stage)) {

      msg_error <- list(
        en = paste0(
          "Data must be standardized before spatial aggregation\n",
          "Current stage: ", current_stage %||% "unknown", "\n",
          "Required stage: ", required_stage, "\n\n",
          "Please run:\n",
          "  df <- sus_data_standardize(df)"
        ),
        pt = paste0(
          "Dados devem ser padronizados antes de agregar espacialmente.\n",
          "Estagio atual: ", current_stage %||% "desconhecido", "\n",
          "Estagio requerido: ", required_stage, "\n\n",
          "Por favor, execute:\n",
          "  df <- sus_data_standardize(df)"
        ),
        es = paste0(
          "Los datos deben estar estandarizados antes de agregar espacialmente.\n",
          "Etapa actual: ", current_stage %||% "desconocida", "\n",
          "Etapa requerida: ", required_stage, "\n\n",
          "Por favor, ejecute:\n",
          "  df <- sus_data_standardize(df)"
        )
      )

      cli::cli_abort(msg_error[[lang]] %||% msg_error[["en"]])
    }

    # Stage validated
    if (verbose) {
      msg_stage_ok <- list(
        en = "Data stage validated",
        pt = "Estagio de dados validado",
        es = "Etapa de datos validada"
      )

      cli::cli_alert_success(msg_stage_ok[[lang]] %||% msg_stage_ok[["en"]])
    }

    # Update metadata
    df <- climasus_meta(df, stage = "spatial", type = level)
  } else {
    
    # NOT climasus_df - ABORT execution
    msg_error <- list(
      en = paste0(
        "Input is not a climasus_df object.\n",
        "This function requires data from the CLIMASUS4r pipeline.\n\n",
        "Please prepare your data first:\n",
        "  1. Import: df <- sus_data_import(...) or sus_data_read(...)\n",
        "  2. Clean: df <- sus_data_clean_encoding(df)\n",
        "  3. Standardize: df <- sus_data_standardize(df)\n",
        "If using external data, run sus_data_standardize() first to prepare it."
      ),
      pt = paste0(
        "Entrada nao e um objeto climasus_df.\n",
        "Esta funcao requer dados do pipeline CLIMASUS4r.\n\n",
        "Por favor, prepare seus dados primeiro:\n",
        "  1. Importar: df <- sus_data_import(...) ou sus_data_read(...)\n",
        "  2. Limpar: df <- sus_data_clean_encoding(df)\n",
        "  3. Padronizar: df <- sus_data_standardize(df)\n",
        "Se usar dados externos, execute sus_data_standardize() primeiro para prepara-los."
      ),
      es = paste0(
        "La entrada no es un objeto climasus_df.\n",
        "Esta funcion requiere datos del pipeline CLIMASUS4r.\n\n",
        "Por favor, prepare sus datos primero:\n",
        "  1. Importar: df <- sus_data_import(...) o sus_data_read(...)\n",
        "  2. Limpiar: df <- sus_data_clean_encoding(df)\n",
        "  3. Estandarizar: df <- sus_data_standardize(df)\n",
        "Si usa datos externos, ejecute sus_data_standardize() primero para prepararlos."
      )
    )
    
    cli::cli_abort(msg_error[[lang]])
  }

  system <- climasus_meta(df, "system")
  # ============================================================================
  # 2. SYSTEM-AWARE VALIDATION (CEP restriction)
  # ============================================================================

  if (level == "cep") {

    # Get unique systems in the data
    systems_in_data <- system
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
    
    if (verbose) {cli::cli_alert_info(msg$detecting_column)}

    # Define column candidates based on level
    common_cols <- switch(
      level,
      "munic" = c(
        "codigo_municipio_residencia",
        "residence_municipality_code",

        "codigo_municipio_ocorrencia",
        "codigo_municipio_ocurrencia",
        "occurrence_municipality_code",

        "codigo_municipio",
        "municipality_code",

        "codigo_municipio_nascimento",
        "codigo_municipio_nacimiento",
        "birth_municipality_code",

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
      )
    )

    if (level == "munic") {
      #Priority
      existent_cols <- names(df)[names(df) %in% common_cols]

      priority_order <- c(
        "codigo_municipio_residencia",
        "residence_municipality_code",

        "codigo_municipio_ocorrencia",
        "codigo_municipio_ocurrencia",
        "occurrence_municipality_code",

        "codigo_municipio",
        "municipality_code",

        "codigo_municipio_nascimento",
        "codigo_municipio_nacimiento",
        "birth_municipality_code",

        "codigo_municipio_paciente",
        "patient_municipality_code",

        "uf_municipio_estabelecimento",
        "facility_uf_municipality",
        "uf_municipio_establecimiento",

        "CODMUNRES"
      )
      join_col <- priority_order[priority_order %in% existent_cols][1]
    } else {
      # Find first matching column
      join_col <- names(df)[names(df) %in% common_cols][1]
    }

    if (is.na(join_col) || length(join_col) == 0) {
      cli::cli_alert_danger(msg$no_column_found)
      cli::cli_alert_info(paste0(
        msg$expected_columns,
        paste(common_cols, collapse = ", ")
      ))
      cli::cli_abort(paste0(
        "No valid geographic column found for level: ",
        level
      ))
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

  if (use_cache) {
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

  if (level %in% c("munic")) {
    # Use cache-enabled retrieval for polygon-based levels
    spatial_df <- get_spatial_munic_cache(
      level = "munic",
      cache_dir = cache_dir,
      use_cache = use_cache,
      lang = lang,
      verbose = verbose
    )

    # Determine the code column name in spatial data
    code_col_spatial <- switch(
      "munic",
      "munic" = "code_muni"
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

    if (join_col != code_col_spatial) {
      names(result_sf)[names(result_sf) == join_col] <- code_col_spatial
    }

    # Convert to sf object
    result_sf <- result_sf %>% sf::st_as_sf()

    # Remove rows with missing geometries
    result_sf <- result_sf %>%
      dplyr::filter(!is.na(sf::st_dimension(.data$geom)))
  }

  if (level %in% c("cep", "health_facilities", "neighborhood")) {
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
    tryCatch(
      {
        geo_points <- geocodebr::busca_por_cep(
          cep = unique_ceps,
          resultado_sf = TRUE,
          verboso = FALSE,
          cache = FALSE
        )
        geo_points$cep <- stringr::str_replace_all(geo_points$cep, "\\D", "")

        df <- df %>% dplyr::arrange(.data[[join_col]])


        # Join geocoded points back to original data
        result_sf <- dplyr::left_join(
          df,
          geo_points,
          by = stats::setNames("cep", join_col),
          relationship = "many-to-many", multiple = "first"
        )

        if (join_col != "cep") {
          names(result_sf)[names(result_sf) == join_col] <- "cep"
        }
        result_sf <- sf::st_as_sf(result_sf)
      },
      error = function(e) {
        cli::cli_abort(paste0(msg$geocoding_error, e$message))
      }
    )
  }

  # ============================================================================
  # 9. CHECK OTHER LEVEL 
  # ============================================================================
  
  if (!level %in% any("muni", "cep")) {
  # Use cache-enabled retrieval for polygon-based levels
    spatial_df <- get_spatial_munic_cache(
      level = "munic",
      cache_dir = cache_dir,
      use_cache = use_cache,
      lang = lang,
      verbose = verbose
    )

    # Determine the code column name in spatial data
    code_col_spatial <- switch(
      "munic",
      "munic" = "code_muni"
    )

    # Ensure code column is character
    spatial_df[[code_col_spatial]] <- as.character(spatial_df[[
      code_col_spatial
    ]])

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

    if (join_col != code_col_spatial) {
      names(result_sf)[names(result_sf) == join_col] <- code_col_spatial
    }

  }
  
  if (level %in% c("school")) { 

    spatial_var <- get_spatial_data_with_cache(
      level = "school",
      cache_dir = cache_dir,
      use_cache = use_cache,
      lang = lang,
      verbose = verbose
    )
    spatial_var <- sf::st_make_valid(spatial_var)
    
    result_sf <- dplyr::inner_join(
      result_sf,
      spatial_var,
      by = c("name_muni", "abbrev_state"),
      relationship = "many-to-many"
     )
  }

  if (level == "health_region") {
     
    spatial_var <- get_spatial_data_with_cache(
      level = "health_region",
      cache_dir = cache_dir,
      use_cache = use_cache,
      lang = lang,
      verbose = verbose
    )
    spatial_var <- sf::st_make_valid(spatial_var)
    result_sf$geom <- NULL

    result_sf <- dplyr::inner_join(
      result_sf,
      spatial_var,
      by = c("abbrev_state", "code_state", "name_state"),
      relationship = "many-to-many"
     )
  }

  if (level == "amazon") {
     
    spatial_var <- get_spatial_data_with_cache(
      level = "amazon",
      cache_dir = cache_dir,
      use_cache = use_cache,
      lang = lang,
      verbose = verbose
    )
    spatial_var <- sf::st_make_valid(spatial_var)

    result_sf <- result_sf %>% sf::st_as_sf()

    result_sf <- sf::st_join(result_sf, spatial_var)
  }

  if (level == "semiarid") {
     
    spatial_var <- get_spatial_data_with_cache(
      level = "semiarid",
      cache_dir = cache_dir,
      use_cache = use_cache,
      lang = lang,
      verbose = verbose
    )
    spatial_var <- sf::st_make_valid(spatial_var)
    spatial_var$name_muni <- NULL
    spatial_var$code_state <- NULL

    result_sf <- dplyr::inner_join(
      result_sf,
      spatial_var,
      by = c("code_muni", "abbrev_state"),
      relationship = "many-to-many"
     )
  }

  if (level == "biomes") { 

    spatial_var <- get_spatial_data_with_cache(
      level = "biomes",
      cache_dir = cache_dir,
      use_cache = use_cache,
      lang = lang,
      verbose = verbose
    )
    spatial_var <- sf::st_make_valid(spatial_var)

    result_sf <- result_sf %>% sf::st_as_sf()

    result_sf <- sf::st_join(result_sf, spatial_var)
  }

  if (level == "conservation_units") { 

    spatial_var <- get_spatial_data_with_cache(
      level = "conservation_units",
      cache_dir = cache_dir,
      use_cache = use_cache,
      lang = lang,
      verbose = verbose
    )
    result_sf <- result_sf %>% sf::st_as_sf()
    spatial_var$date <- NULL
    spatial_var <- sf::st_make_valid(spatial_var)
    result_sf <- sf::st_make_valid(result_sf)
    #Join
    result_sf <- sf::st_join(result_sf, spatial_var)
  }

  if (level == "disaster_risk_area") { 

    spatial_var <- get_spatial_data_with_cache(
      level = "disaster_risk_area",
      cache_dir = cache_dir,
      use_cache = use_cache,
      lang = lang,
      verbose = verbose
    )

    spatial_var <- sf::st_make_valid(spatial_var)

    result_sf$geom <- NULL
    result_sf$code_state <- NULL
    spatial_var$name_muni <- NULL
    #Join
    result_sf <- dplyr::inner_join(
      result_sf,
      spatial_var,
      by = c("code_muni", "abbrev_state"),
      relationship = "many-to-many"
     )
    
  }

  if (level == "indigenous_land") { 

    spatial_var <- get_spatial_data_with_cache(
      level = "indigenous_land",
      cache_dir = cache_dir,
      use_cache = use_cache,
      lang = lang,
      verbose = verbose
    )
    spatial_var <- sf::st_make_valid(spatial_var)

    result_sf$code_state <- NULL
    spatial_var$date <- NULL
    #Join
    result_sf <- dplyr::inner_join(
      result_sf,
      spatial_var,
      by = c("name_muni", "abbrev_state"),
      relationship = "many-to-many"
     )
    
  }

  if (level == "urban_area") {
     
    spatial_var <- get_spatial_data_with_cache(
      level = "urban_area",
      cache_dir = cache_dir,
      use_cache = use_cache,
      lang = lang,
      verbose = verbose
    )
    spatial_var <- sf::st_make_valid(spatial_var)
    spatial_var$name_muni <- NULL
    spatial_var$code_state <- NULL
    spatial_var$name_state <- NULL
   
    result_sf <- dplyr::inner_join(
      result_sf,
      spatial_var,
      by = c("code_muni", "abbrev_state"),
      relationship = "many-to-many"
     )
  }

  if (level == "metro_area") {
     
    spatial_var <- get_spatial_data_with_cache(
      level = "metro_area",
      cache_dir = cache_dir,
      use_cache = use_cache,
      lang = lang,
      verbose = verbose
    )
    spatial_var <- sf::st_make_valid(spatial_var)
    spatial_var$name_muni <- NULL
    spatial_var$code_state <- NULL
   
    result_sf <- dplyr::inner_join(
      result_sf,
      spatial_var,
      by = c("code_muni", "abbrev_state"),
      relationship = "many-to-many"
     )
  }

  if (level == "urban_concentrations") {
     
    spatial_var <- get_spatial_data_with_cache(
      level = "urban_concentrations",
      cache_dir = cache_dir,
      use_cache = use_cache,
      lang = lang,
      verbose = verbose
    )
    spatial_var <- sf::st_make_valid(spatial_var)
    spatial_var$name_muni <- NULL
    spatial_var$code_state <- NULL
    spatial_var$name_state <- NULL
   
    result_sf <- dplyr::inner_join(
      result_sf,
      spatial_var,
      by = c("code_muni", "abbrev_state"),
      relationship = "many-to-many"
     )
  }

   if (level == "pop_arrangements") {
     
    spatial_var <- get_spatial_data_with_cache(
      level = "pop_arrangements",
      cache_dir = cache_dir,
      use_cache = use_cache,
      lang = lang,
      verbose = verbose
    )
    spatial_var <- sf::st_make_valid(spatial_var)
    spatial_var$name_muni <- NULL
    spatial_var$code_state <- NULL
    spatial_var$name_state <- NULL
   
    result_sf <- dplyr::inner_join(
      result_sf,
      spatial_var,
      by = c("code_muni", "abbrev_state"),
      relationship = "many-to-many"
     )
  }
  
  
  if (level == "health_facilities" && !system == "CNES") {
    if (verbose) {cli::cli_abort("The level {.val health_facilities} is only available for the {.code CNES} system.")}
  }
  if (level == "health_facilities") {
    spatial_var <- get_spatial_data_with_cache(
      level = "health_facilities",
      cache_dir = cache_dir,
      use_cache = use_cache,
      lang = lang,
      verbose = verbose
    )
    # Join geocoded points back to original data
    result_sf <- dplyr::inner_join(
      df,
      spatial_var,
      by = stats::setNames("code_cnes", "cnes"),
      relationship = "many-to-many"
    )
  }

  if (level == "neighborhood" && !system %in% c("CNES", "SIH")) {
    if (verbose) {cli::cli_abort("The level {.val neighborhood} is only available for the {.code CNES} system.")}
  }

  if (level == "neighborhood") { 

    spatial_var <- get_spatial_data_with_cache(
      level = "neighborhood",
      cache_dir = cache_dir,
      use_cache = use_cache,
      lang = lang,
      verbose = verbose
    )
    spatial_var <- sf::st_make_valid(spatial_var)

    # Join geocoded points back to original data
    result_sf <- sf::st_join(result_sf, spatial_var)
    
  }
    
  # ============================================================================
  # 10. FINAL VALIDATION AND SUCCESS MESSAGE
  # ============================================================================

  if (!inherits(result_sf, "sf")) {
    result_sf <- result_sf %>% sf::st_as_sf()
  }
  # Remove rows with missing geometries
  result_sf <- result_sf %>% dplyr::filter(!is.na(sf::st_dimension(.data$geom)))

  # Update stage and type
  result_sf <- climasus_meta(
    result_sf,
    system = climasus_meta(result_sf, "system"),  # Preserve original system
    stage = "spatial",
    type = level,
    temporal = NULL,
    add_history = "Spatial join"
  )

  if (verbose) {
    cli::cli_alert_success(msg$join_success)
    cli::cli_alert_info(paste0(msg$final_rows, nrow(result_sf)))
  }
  
  return(result_sf)
}


# ============================================================================
# HELPER FUNCTIONS
# ============================================================================


#' Get Spatial Municipality Data with Caching
#'
#' Retrieves spatial data from IBGE with intelligent caching to improve performance.
#' Spatial data is cached as Parquet files for fast reloading.
#'
#' @param level Geographic level ("munic")
#' @param cache_dir Cache directory path
#' @param use_cache Whether to use cache
#' @param lang Language for messages
#' @param verbose Print messages
#'
#' @return sf object with spatial data
#' @keywords internal
#' @noRd
get_spatial_munic_cache <- function(
  level = "munic",
  cache_dir,
  use_cache,
  lang,
  verbose
) {
  msg <- get_spatial_messages(lang)

  if ((requireNamespace("sfarrow", quietly = TRUE))) { 
  # Define cache file name
  cache_file <- file.path(
    cache_dir,
    paste0(level, "_", ".parquet")
  )
  } else { 
  # Define cache file name
  cache_file <- file.path(
    cache_dir,
    paste0(level, "_", ".gpkg")
  )
  }

  # Try to load from cache
  if (use_cache && file.exists(cache_file)) {
    if (verbose) {
      cli::cli_alert_success(paste0(msg$loading_cache, basename(cache_file)))
    }

    tryCatch(
      {
        if ((requireNamespace("sfarrow", quietly = TRUE))) { 
          spatial_df <- sfarrow::st_read_parquet(cache_file)
        } else { 
          spatial_df <- sf::st_read(cache_file)
          spatial_df <- sf::st_as_sf(spatial_df)
        }
        return(spatial_df)
      },
      error = function(e) {
        if (verbose) {
          cli::cli_alert_warning(msg$cache_error)
        }
      }
    )
  }

  # Download from IBGE
  if (verbose) {
    cli::cli_alert_info(msg$downloading_data)
  }

  spatial_df <- switch(
    level,
    "munic" = geobr::read_municipality(
      code_muni = "all",
      simplified = TRUE,
      showProgress = verbose
    )
  )

  # Save to cache
  if (use_cache) {
    if (verbose) {
      cli::cli_alert_info(msg$saving_cache)
    }

    tryCatch(
      { 
        if ((requireNamespace("sfarrow", quietly = TRUE))) { 
          sfarrow::st_write_parquet(obj = spatial_df, dsn = cache_file)
        } else {
          sf::st_write(spatial_df, cache_file, driver = "GPKG", quiet = TRUE, delete_dsn = TRUE, append = TRUE)
         }
        if (verbose) {
          cli::cli_alert_success(msg$cache_saved)
        }
      },
      error = function(e) {
        if (verbose) {
          cli::cli_alert_warning(paste0(msg$cache_save_error, e$message))
        }
      }
    )
  }

  return(spatial_df)
}



#' Get Spatial Data with Caching
#'
#' Retrieves spatial data from IBGE with intelligent caching to improve performance.
#' Spatial data is cached as Parquet files for fast reloading.
#'
#' @param level Geographic level ("munic")
#' @param cache_dir Cache directory path
#' @param use_cache Whether to use cache
#' @param lang Language for messages
#' @param verbose Print messages
#'
#' @return sf object with spatial data
#' @keywords internal
#' @noRd
get_spatial_data_with_cache <- function(
  level,
  year,
  cache_dir,
  use_cache,
  lang,
  verbose
) {
  msg <- get_spatial_messages(lang)

  if ((requireNamespace("sfarrow", quietly = TRUE))) { 
  # Define cache file name
  cache_file <- file.path(
    cache_dir,
    paste0(level, "_", ".parquet")
  )
  } else { 
  # Define cache file name
  cache_file <- file.path(
    cache_dir,
    paste0(level, "_", ".gpkg")
  )
  }

  # Try to load from cache
  if (use_cache && file.exists(cache_file)) {
    if (verbose) {
      cli::cli_alert_success(paste0(msg$loading_cache, basename(cache_file)))
    }

    tryCatch(
      {
        if ((requireNamespace("sfarrow", quietly = TRUE))) { 
          spatial_df <- sfarrow::st_read_parquet(cache_file)
        } else { 
          spatial_df <- sf::st_read(cache_file)
          spatial_df <- sf::st_as_sf(spatial_df)
        }
        return(spatial_df)
      },
      error = function(e) {
        if (verbose) {
          cli::cli_alert_warning(msg$cache_error)
        }
      }
    )
  }

  # Download from IBGE
  if (verbose) {
    cli::cli_alert_info(msg$downloading_data)
  }

  spatial_df <- switch(
    level,
    "school" = geobr::read_schools(
      cache = FALSE,
      showProgress = verbose
    ),
    "health_facilities" = geobr::read_health_facilities(
      date = 202303,
      showProgress = verbose,
      cache = FALSE
    ),
    "health_region" = geobr::read_health_region(
      showProgress = verbose,
      cache = FALSE
    ),
    "amazon" = geobr::read_amazon(
      showProgress = verbose,
      cache = FALSE
    ),
    "semiarid" = geobr::read_semiarid(
      showProgress = verbose,
      cache = FALSE
    ),
    "biomes" = geobr::read_biomes(
      showProgress = verbose,
      cache = FALSE
    ),
    "conservation_units" = geobr::read_conservation_units(
      date = 201909,
      showProgress = verbose,
      cache = FALSE
    ),
    "disaster_risk_area" = geobr::read_disaster_risk_area(
      showProgress = verbose,
      cache = FALSE
    ),
    "indigenous_land" = geobr::read_indigenous_land(
      date= 201907,
      showProgress = verbose,
      cache = FALSE
    ),
    "neighborhood" = geobr::read_neighborhood(
      showProgress = verbose,
      cache = FALSE
    ),
    "urban_area" = geobr::read_urban_area(
      showProgress = verbose,
      cache = FALSE
    ),
    "metro_area" = geobr::read_metro_area(
      showProgress = verbose,
      cache = FALSE
    ),
    "urban_concentrations" = geobr::read_urban_concentrations(
      showProgress = verbose,
      cache = FALSE
    ),
    "pop_arrangements" = geobr::read_pop_arrangements(
      showProgress = verbose,
      cache = FALSE
    )
  )

  # Save to cache
  if (use_cache) {
    if (verbose) {
      cli::cli_alert_info(msg$saving_cache)
    }

    tryCatch(
      { 
        if ((requireNamespace("sfarrow", quietly = TRUE))) { 
         suppressWarnings(sfarrow::st_write_parquet(obj = spatial_df, dsn = cache_file))
        } else {
          sf::st_write(spatial_df, cache_file, driver = "GPKG", quiet = TRUE, delete_dsn = TRUE, append = TRUE)
         }
        if (verbose) {
          cli::cli_alert_success(msg$cache_saved)
        }
      },
      error = function(e) {
        if (verbose) {
          cli::cli_alert_warning(paste0(msg$cache_save_error, e$message))
        }
      }
    )
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