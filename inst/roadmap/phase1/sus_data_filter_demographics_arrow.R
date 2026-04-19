#' Filter Health Data by Demographic Variables
#'
#' Filters health data based on demographic characteristics such as sex, race,
#' age range, education level, region, and municipality. Supports lazy evaluation
#' with Arrow and DuckDB backends for out-of-memory processing.
#'
#' @param df A data frame, Arrow Dataset, Arrow query, or DuckDB table containing health data.
#' @param sex Character vector specifying sex categories to include.
#' @param race Character vector specifying race/color categories to include.
#' @param age_range Numeric vector of length 2 specifying the age range `c(min_age, max_age)`.
#' @param education Character vector specifying education levels to include.
#' @param region A string indicating a predefined group of states or regions.
#' @param city Character or numeric vector specifying municipality names or codes.
#' @param drop_ignored Logical. If `TRUE`, removes rows with missing values or ignored codes.
#' @param lang Character string specifying the language for messages.
#' @param verbose Logical. If `TRUE`, prints filtering summary.
#' @param use_cache Logical. If `TRUE` (default), uses cached spatial data to avoid
#'   re-downloads and improve performance.
#' @param cache_dir Character string specifying the directory to store cached files.
#'   Default is `"~/.climasus4r_cache/spatial"`.
#'
#' @return A filtered object of the same class as input (preserves backend type).
#' @export
#'
#' @examples
#' \dontrun{
#' # With Arrow Dataset
#' arrow_ds <- arrow::open_dataset("data/sim/")
#' arrow_filtered <- sus_data_filter_demographics(
#'   arrow_ds,
#'   sex = "Feminino",
#'   age_range = c(65, Inf),
#'   lang = "pt"
#' )
#' }
sus_data_filter_demographics_arrow <- function(df,
                                          sex = NULL,
                                          race = NULL,
                                          age_range = NULL,
                                          education = NULL,
                                          region = NULL,
                                          city = NULL,
                                          drop_ignored = FALSE,
                                          lang = "pt",
                                          verbose = TRUE,
                                          use_cache = TRUE,
                                          cache_dir = "~/.climasus4r_cache/spatial"
                                        ) {
  
  cli::cli_h1("climasus4r - Filter Data Demographics")
  
  # ============================================================================
  # Input Validation and Backend Detection
  # ============================================================================
  
  # Detect backend type
  backend_type <- detect_backend_type(df)
  
  if (backend_type == "unsupported") {
    cli::cli_abort("Input must be a data.frame, Arrow Dataset, or DuckDB table.")
  }
  
  # Validate language
  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_abort("lang must be one of: 'en', 'pt', 'es'")
  }
  
  # Validate sex values if provided
  if (!is.null(sex)) {
    valid_sex <- c("Male", "Female", "Unknown",
                   "Masculino", "Feminino", "Ignorado",
                   "Femenino", "Desconocido")
    unrecognized <- setdiff(tools::toTitleCase(sex), valid_sex)
    if (length(unrecognized) > 0) {
      cli::cli_warn(c(
        "Unrecognized sex values: {.val {unrecognized}}",
        "i" = "Valid values: {.val {valid_sex}}"
      ))
    }
  }
  
  # ============================================================================
  # Handle climasus metadata (only for data frames)
  # ============================================================================
  
  system <- NULL
  
  # Only check stage and metadata for climasus_df (data frame objects)
  if (inherits(df, "climasus_df")) {
    required_stage <- "stand"
    current_stage <- attr(df, "sus_meta")$stage
    
    if (!is.null(current_stage) && !is_stage_at_least(current_stage, required_stage)) {
      msg_error <- list(
        en = paste0("Data must be standardized before demographic filtering. Current stage: ", current_stage),
        pt = paste0("Dados devem ser padronizados antes da filtragem demografica. Estagio atual: ", current_stage),
        es = paste0("Los datos deben estar estandarizados antes del filtrado demografico. Etapa actual: ", current_stage)
      )
      cli::cli_abort(msg_error[[lang]] %||% msg_error[["en"]])
    }
    
    # Get system from metadata
    system <- attr(df, "sus_meta")$system
    
    if (verbose && !is.null(current_stage)) {
      msg_stage_ok <- list(
        en = "Data stage validated: demographic filtering",
        pt = "Estagio de dados validado: filtragem demografica",
        es = "Etapa de datos validada: filtrado demografico"
      )
      cli::cli_alert_success(msg_stage_ok[[lang]] %||% msg_stage_ok[["en"]])
    }
  } else if (backend_type == "dataframe" && !inherits(df, "climasus_df")) {
    # NOT climasus_df - abort for data frames
    msg_error <- list(
      en = "Input is not a climasus_df object. Please run sus_data_standardize() first.",
      pt = "A entrada nao e um objeto climasus_df. Execute sus_data_standardize() primeiro.",
      es = "La entrada no es un objeto climasus_df. Ejecute sus_data_standardize() primero."
    )
    cli::cli_abort(msg_error[[lang]])
  }
  
  # ============================================================================
  # Get column names (works for lazy objects)
  # ============================================================================
  
  col_names <- get_column_names(df, backend_type)
  
  # Track filters applied for verbose output
  filters_applied <- character()
  n_original <- NULL
  
  # Get original row count only for data frames
  if (backend_type == "dataframe") {
    n_original <- nrow(df)
  }
  
  ignored_codes <- c("ignorado", "ignorada", "unknown", "desconocido", "i", 
                     "9", "99", "999", "9999", "000000", "-", "", " ")
  
  # ============================================================================
  # Helper function to apply filters based on backend
  # ============================================================================
  
  apply_filter <- function(data, filter_expr) {
    apply_filter_by_backend(data, filter_expr, backend_type)
  }
  
  # ============================================================================
  # FILTER BY SEX
  # ============================================================================
  
  if (!is.null(sex)) {
    sex_col <- find_column_from_names(col_names, c("sex", "sexo", "SEXO"))
    
    if (is.null(sex_col)) {
      cli::cli_alert_warning("Sex column not found. Skipping sex filter.")
    } else {
      # For lazy backends, we can't easily drop ignored codes without collecting
      # So we only do drop_ignored for data frames
      if (drop_ignored && backend_type == "dataframe") {
        # This would need to be implemented for data frames only
        # For Arrow/DuckDB, we skip drop_ignored to maintain lazy evaluation
        if (verbose) {
          cli::cli_alert_info("drop_ignored=TRUE only supported for data frames. Skipping for lazy backend.")
        }
      }
      
      # Normalize sex values
      sex_targets <- tools::toTitleCase(sex)
      
      # Create filter expression
      filter_expr <- list(
        type = "in",
        column = sex_col,
        values = sex_targets
      )
      
      df <- apply_filter(df, filter_expr)
      filters_applied <- c(filters_applied, paste0("sex: ", paste(sex_targets, collapse = ", ")))
    }
  }
  
  # ============================================================================
  # FILTER BY RACE
  # ============================================================================
  
  if (!is.null(race)) {
    race_col <- find_column_from_names(col_names, c("race", "raca", "raza", "RACACOR", "RACA_COR"))
    
    if (is.null(race_col)) {
      cli::cli_alert_warning("Race column not found. Skipping race filter.")
    } else {
      race_targets <- tools::toTitleCase(race)
      
      filter_expr <- list(
        type = "in",
        column = race_col,
        values = race_targets
      )
      
      df <- apply_filter(df, filter_expr)
      filters_applied <- c(filters_applied, paste0("race: ", paste(race_targets, collapse = ", ")))
    }
  }
  
  # ============================================================================
  # FILTER BY AGE RANGE
  # ============================================================================
  
  if (!is.null(age_range)) {
    if (length(age_range) != 2) {
      cli::cli_abort("age_range must be a numeric vector of length 2: c(min_age, max_age)")
    }
    
    age_col <- find_column_from_names(col_names, c("age_years"))
    
    if (is.null(age_col)) {
      cli::cli_alert_warning("Age column not found. Use sus_data_create_variables to create age_years column.")
    } else {
      min_age <- age_range[1]
      max_age <- age_range[2]
      
      filter_expr <- list(
        type = "between",
        column = age_col,
        min = min_age,
        max = max_age
      )
      
      df <- apply_filter(df, filter_expr)
      filters_applied <- c(filters_applied, paste0("age: ", min_age, "-", ifelse(is.infinite(max_age), "+", max_age)))
    }
  }
  
  # ============================================================================
  # FILTER BY EDUCATION
  # ============================================================================
  
  if (!is.null(education)) {
    edu_col <- find_column_from_names(col_names, c("education", "escolaridade", "escolaridad", "ESC", "ESC2010"))
    
    if (is.null(edu_col)) {
      cli::cli_alert_warning("Education column not found. Skipping education filter.")
    } else {
      education_targets <- tools::toTitleCase(education)
      
      filter_expr <- list(
        type = "in",
        column = edu_col,
        values = education_targets
      )
      
      df <- apply_filter(df, filter_expr)
      filters_applied <- c(filters_applied, paste0("education: ", paste(education_targets, collapse = ", ")))
    }
  }
  
  # ============================================================================
  # FILTER BY REGION OR STATES
  # ============================================================================
  
  if (!is.null(region)) {
    # Get state codes for the region
    all_target_siglas <- unique(unlist(lapply(region, translate_input)))
    
    # Get UF column
    uf_col <- find_column_from_names(col_names, c("manager_uf", "UF_ZI", "uf_gestor", "notification_uf", "UF"))
    
    if (is.null(uf_col)) {
      cli::cli_alert_warning("UF column not found. Skipping region filter.")
    } else if (length(all_target_siglas) > 0) {
      filter_expr <- list(
        type = "in",
        column = uf_col,
        values = all_target_siglas
      )
      
      df <- apply_filter(df, filter_expr)
      
      # Get state names for message (only for data frames or if we have the data)
      if (verbose) {
        cli::cli_alert_success("Filtered by {length(all_target_siglas)} states based on: {paste(region, collapse = ', ')}")
      }
      filters_applied <- c(filters_applied, paste0("region: ", paste(region, collapse = ", ")))
    } else {
      cli::cli_abort("No valid states found for: {.val {region}}")
    }
  }
  
  # ============================================================================
  # FILTER BY MUNICIPALITY
  # ============================================================================
  
  if (!is.null(city)) {
    
  municipio_meta <- get_spatial_municipio_cache(
      cache_dir = cache_dir,
      use_cache = use_cache,
      lang = lang,
      verbose = verbose)
      
    muni_col <- find_column_from_names(col_names, c("residence_city", 
                                                    "city", 
                                                    "residence_municipality",
                                                    "municipio_residencia",
                                                    "codigo_municipio",
                                                    "codigo_municipio_residencia",
                                                    "CODMUNRES"))
    
    if (is.null(muni_col)) {
      cli::cli_alert_warning("Municipality column not found. Skipping municipality filter.")
    } else {
      city <- as.character(city)
      
      filter_expr <- list(
        type = "in",
        column = muni_col,
        values = city
      )
      
      df <- apply_filter(df, filter_expr)
      
      if (length(city) == 1) {
        filters_applied <- c(filters_applied, paste0("municipality: ", city))
      } else if (length(city) <= 3) {
        filters_applied <- c(filters_applied, paste0("municipalities: ", paste(city, collapse = ", ")))
      } else {
        filters_applied <- c(filters_applied, paste0("municipalities: ", length(city), " codes"))
      }
    }
  }
  
  # ============================================================================
  # SUMMARY MESSAGE
  # ============================================================================
  
  if (verbose) {
    if (length(filters_applied) == 0) {
      msg <- switch(lang,
                    "en" = "No demographic filters applied",
                    "pt" = "Nenhum filtro demografico aplicado",
                    "es" = "Ningun filtro demografico aplicado")
      cli::cli_alert_warning(msg)
    } else {
      filter_msg <- switch(lang,
                           "en" = "Applied demographic filters:",
                           "pt" = "Filtros demograficos aplicados:",
                           "es" = "Filtros demograficos aplicados:")
      cli::cli_alert_info(filter_msg)
      
      for (filter in filters_applied) {
        cli::cli_li(filter)
      }
      
      # For data frames, show row counts
      if (backend_type == "dataframe" && !is.null(n_original)) {
        n_filtered <- nrow(df)
        n_removed <- n_original - n_filtered
        pct_retained <- round(100 * n_filtered / n_original, 2)
        
        summary_msg <- switch(lang,
                              "en" = paste0("Retained ", format(n_filtered, big.mark = ","), 
                                           " of ", format(n_original, big.mark = ","), 
                                           " rows (", pct_retained, "%)"),
                              "pt" = paste0("Retidos ", format(n_filtered, big.mark = ","),
                                           " de ", format(n_original, big.mark = ","),
                                           " registros (", pct_retained, "%)"),
                              "es" = paste0("Retenidos ", format(n_filtered, big.mark = ","),
                                           " de ", format(n_original, big.mark = ","),
                                           " registros (", pct_retained, "%)"))
        cli::cli_alert_success(summary_msg)
        
        removed_msg <- switch(lang,
                              "en" = paste0("Removed ", format(n_removed, big.mark = ","), " rows"),
                              "pt" = paste0("Removidos ", format(n_removed, big.mark = ","), " registros"),
                              "es" = paste0("Eliminados ", format(n_removed, big.mark = ","), " registros"))
        cli::cli_alert_info(removed_msg)
      } else {
        # For lazy backends
        msg_lazy <- list(
          en = "Filters applied lazily (out-of-memory). Use `collect()` to materialize.",
          pt = "Filtros aplicados lazy (fora da memoria). Use `collect()` para materializar.",
          es = "Filtros aplicados lazy (fuera de memoria). Use `collect()` para materializar."
        )
        cli::cli_alert_info(msg_lazy[[lang]])
      }
    }
  }
  
  # ============================================================================
  # Preserve climasus class if it existed
  # ============================================================================
  
  # If input had climasus_dataset class, preserve it
  if (inherits(df, "climasus_dataset") && backend_type == "arrow") {
    # Class already preserved by apply_filter_by_backend
    if (!inherits(df, "climasus_dataset")) {
      class(df) <- unique(c("climasus_dataset", class(df)))
    }
  }
  
  # For data frames that were climasus_df, add metadata
  if (backend_type == "dataframe" && !is.null(system)) {
    if (!inherits(df, "climasus_df")) {
      # Create metadata
      meta <- list(
        system = system,
        stage = "filter_demo",
        type = "filter_demo",
        spatial = FALSE,
        temporal = NULL,
        created = Sys.time(),
        modified = Sys.time(),
        history = sprintf("[%s] Demographic filters applied: %s",
                         format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                         paste(filters_applied, collapse = " | ")),
        user = list()
      )
      
      class(df) <- unique(c("climasus_df", class(df)))
      attr(df, "sus_meta") <- meta
    }
  }
  
  return(df)
}


# ============================================================================
# HELPER FUNCTIONS FOR LAZY BACKENDS
# ============================================================================

#' Find column from names (works with lazy objects)
#' @keywords internal
#' @noRd
find_column_from_names <- function(col_names, patterns) {
  for (pattern in patterns) {
    if (pattern %in% col_names) {
      return(pattern)
    }
  }
  return(NULL)
}

#' Apply filter based on backend type
#' @keywords internal
#' @noRd
apply_filter_by_backend <- function(df, filter_expr, backend_type) {
  
  col <- filter_expr$column
  
  if (backend_type == "dataframe") {
    # Standard data frame filtering
    if (filter_expr$type == "in") {
      result <- df[df[[col]] %in% filter_expr$values, ]
    } else if (filter_expr$type == "between") {
      result <- df[df[[col]] >= filter_expr$min & df[[col]] <= filter_expr$max, ]
    } else {
      result <- df
    }
    
  } else if (backend_type == "arrow") {
    # Arrow lazy filtering
    if (filter_expr$type == "in") {
      result <- df %>%
        dplyr::filter(!!rlang::sym(col) %in% filter_expr$values)
    } else if (filter_expr$type == "between") {
      result <- df %>%
        dplyr::filter(!!rlang::sym(col) >= filter_expr$min & 
                      !!rlang::sym(col) <= filter_expr$max)
    } else {
      result <- df
    }
    
  } else if (backend_type == "duckdb") {
    # DuckDB lazy filtering
    if (filter_expr$type == "in") {
      result <- df %>%
        dplyr::filter(!!rlang::sym(col) %in% filter_expr$values)
    } else if (filter_expr$type == "between") {
      result <- df %>%
        dplyr::filter(!!rlang::sym(col) >= filter_expr$min & 
                      !!rlang::sym(col) <= filter_expr$max)
    } else {
      result <- df
    }
    
  } else {
    stop("Unsupported backend type")
  }
  
  return(result)
}

#' Translate input for region filtering
#' @keywords internal
#' @noRd
translate_input <- function(x) {
  if (is.numeric(x) || !is.na(suppressWarnings(as.numeric(x)))) {
    return(.br_regions[[tolower(as.character(x))]] %||% toupper(as.character(x)))
  }
  
  reg_clean <- tolower(x)
  target_key <- if (reg_clean %in% names(.region_aliases)) .region_aliases[[reg_clean]] else reg_clean
  
  if (target_key %in% names(.br_regions)) {
    return(.br_regions[[target_key]]) 
  } else {
    return(toupper(x)) 
  }
}

# ============================================================================
# DATA DEFINITIONS (keep existing ones)
# ============================================================================

#' Regional Definitions for Brazilian States
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

#' Multilingual aliases for region argument
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

# ===========================================================================
# Get spatial municipio meta
# ===========================================================================

#' Get Spatial municipio Data with Caching
#'
#' Retrieves spatial municipio data with intelligent caching to improve performance.
#' Spatial data is cached as Parquet files for fast reloading.
#' @param cache_dir Cache directory path
#' @param use_cache Whether to use cache
#'
#' @return sf object with spatial data
#' @keywords internal
#' @noRd
get_spatial_municipio_cache <- function(
  cache_dir,
  use_cache,
  lang,
  verbose
) {
  msg <- get_spatial_municipio_messages(lang)

  if ((requireNamespace("arrow", quietly = TRUE))) { 
  # Define cache file name
  cache_file <- file.path(
    cache_dir,
    paste0("municipio_meta.parquet")
  )
  } else { 
  # Define cache file name
  cache_file <- file.path(
    cache_dir,
    paste0("municipio_meta.rds")
  )
  }

  # Try to load from cache
  if (use_cache && file.exists(cache_file)) {
    if (verbose) {
      cli::cli_alert_success(paste0(msg$loading_cache, basename(cache_file)))
    }

    tryCatch(
      {
        if ((requireNamespace("arrow", quietly = TRUE))) { 
          spatial_df <- arrow::read_parquet(cache_file)
        } else { 
          spatial_df <- readRDS(cache_file)
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

  # Download from climasus Data Center
  if (verbose) {
    cli::cli_alert_info(msg$downloading_data)
  }
  if ((requireNamespace("arrow", quietly = TRUE))) { 
    spatial_df <- suppressMessages(arrow::read_parquet("https://github.com/ByMaxAnjos/climasus4r/raw/refs/heads/master/inst/data_4r/municipio_meta.parquet"))
    } else {
      spatial_df <- suppressMessages(readRDS("https://github.com/ByMaxAnjos/climasus4r/raw/refs/heads/master/inst/data_4r/municipio_meta.rds"))
      }


  # Save to cache
  if (use_cache) {
    if (verbose) {
      cli::cli_alert_info(msg$saving_cache)
    }
    tryCatch(
      { 
        if ((requireNamespace("arrow", quietly = TRUE))) { 
          arrow::write_parquet(obj = spatial_df, dsn = cache_file)
        } else {
          readRDS(spatial_df, cache_file)
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
get_spatial_municipio_messages <- function(lang) {
  messages <- list(
    en = list(
      creating_cache = "Creating cache directory: ",
      cache_enabled = "Cache: ENABLED",
      loading_cache = "Loading from cache: ",
      cache_error = "Cache loading failed. Downloading fresh data...",
      downloading_data = "Downloading spatial data from climasus Data Center...",
      saving_cache = "Saving to cache...",
      cache_saved = "Spatial data cached successfully.",
      cache_save_error = "Failed to save cache: "
    ),
    pt = list(
      creating_cache = "Criando diretorio de cache: ",
      cache_enabled = "Cache: ATIVADO",
      loading_cache = "Carregando do cache: ",
      cache_error = "Falha ao carregar cache. Baixando dados novos...",
      downloading_data = "Baixando dados espaciais do climasus Data Center ...",
      saving_cache = "Salvando no cache...",
      cache_saved = "Dados espaciais armazenados em cache com sucesso.",
      cache_save_error = "Falha ao salvar cache: "
    ),
    es = list(
      creating_cache = "Creando directorio de cache: ",
      cache_enabled = "Cache: ACTIVADO",
      loading_cache = "Cargando desde cache: ",
      cache_error = "Fallo al cargar cache. Descargando datos nuevos...",
      downloading_data = "Descargando datos espaciales del climasus Data Center...",
      saving_cache = "Guardando en cache...",
      cache_saved = "Datos espaciales almacenados en cache con exito.",
      cache_save_error = "Fallo al guardar cache: "
    )
  )
  
  return(messages[[lang]])
}
