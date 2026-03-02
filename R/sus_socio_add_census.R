#' Add Census Socioeconomic variables to Health Data
#'
#' Enriches health data with socioeconomic indicators from Brazilian Census using
#' the `censobr` package. Provides aggregation of microdata to municipality level. Supports multiple census datasets (population, households,
#' families, mortality, emigration) with intelligent caching and optimized performance for large datasets using Arrow/Parquet format.
#'
#' @param df A `data.frame` or `sf` object containing health data. Typically the output from `sus_join_spatial()` function.
#' @param dataset Character string specifying the census dataset to use. Options:
#'   \itemize{
#'     \item `"population"` - Population microdata (default)
#'     \item `"households"` - Household microdata
#'     \item `"families"` - Family microdata
#'     \item `"mortality"` - Mortality microdata
#'     \item `"emigration"` - Emigration microdata
#'     \item `"tracts"` - Census tract aggregate data (coming soon...)
#'   }
#' @param year Integer specifying census year. Options: `2010` (default) or `2000`.
#'   Note: Dataset availability varies by year.
#' @param census_vars Character vector specifying census variables to add. Use `sus_census_explore()` to select available variables.
#'   If `NULL`, returns all available variables (not recommended for large datasets).
#' @param aggregation_fun Character. Method to aggregate microdata to municipality level:
#'   \itemize{
#'     \item `"sum"` - Sums the selected variables by municipality (e.g., for total population).
#'     \item `"mean"` - Averages the selected variables (e.g., for income).
#'     \item `"median"`, `"min"`, `"max"`, `"sd"`, `"q25"`, `"q75"`, `"q95"`, and `"q99"`.
#'   }
#' @param join_muni_col Character string specifying the column in df containing the **6 or 7-digit IBGE municipality code**. If `NULL`, detects common SUS patterns (e.g., `code_muni`).
#' @param use_cache Logical. If `TRUE` (default), uses censobr's caching system to store
#'   downloaded data locally for faster subsequent access.
#' @param cache_dir Character string specifying cache directory path. Defaults to
#'   `"~/.climasus4r_cache/census"`.The function automatically calls `censobr::set_censobr_cache_dir(cache_dir)` to ensure consistency across the ecosystem.
#' @param lang Character string specifying language for messages. Options: `"pt"` (Portuguese, default), `"en"` (English), `"es"` (Spanish).
#' @param translate_columns Logical. If TRUE, translates column names. Default is TRUE.
#' @param standardize_values Logical. If TRUE, standardizes categorical values. Default is TRUE.
#' @param verbose Logical. If `TRUE` (default), prints progress messages and download progress bar.
#'
#' @return Returns the input `data.frame` or `sf` object with additional columns. If the input is an sf object, the spatial geometry and CRS are strictly preserved through a join.
#'
#' @details
#' **Integration with censobr package**:
#' This function is a wrapper around censobr's dataset-specific functions
#' (`read_population()`, `read_households()`, etc.), providing seamless integration
#' with the climasus4r ecosystem.
#'
#' **Geographic Columns**:
#' The function automatically inherits and matches geographic columns following
#' the geobr/censobr standard:
#' \itemize{
#'   \item `code_muni` - 7-digit municipality code
#'   \item `code_state` - 2-digit state code
#'   \item `abbrev_state` - State abbreviation (e.g., "AM")
#'   \item `name_state` - State name
#'   \item `code_region` - Region code
#'   \item `name_region` - Region name
#'   \item `code_weighting` - Weighting area code
#' }
#'
#' **Automatic Column Detection**:
#' If `join_muni_col = NULL`, the function automatically detects the appropriate code municipality
#' column based on common SUS patterns:
#' \itemize{
#'   \item **Municipality**: `residence_municipality_code`, `municipality_code`,
#'     `codigo_municipio`, `CODMUNRES`, etc.
#' }
#' **Performance Optimization**:
#' The function uses Arrow/Parquet format for efficient larger-than-memory dataset handling:
#' \itemize{
#'   \item Downloads data only once (cached locally)
#'   \item Filters municipalities BEFORE loading to RAM
#'   \item Uses `dplyr::collect()` only after filtering
#'   \item Uses `sfarrow` pak for spatial filtering 
#' }
#' **Spatial Data Support**:
#' If `df` is an `sf` object from `sus_join_spatial()`, geometries are preserved in the output.
#'
#' @references
#' Pereira, Rafael H. M.; Barbosa, Rogerio J. (2023) censobr: Download Data from Brazil’s Population Census. R package version v0.4.0, https://CRAN.R-project.org/package=censobr. DOI: 10.32614/CRAN.package.censobr.
#'
#' @examples
#' \dontrun{
#' library(climasus4r)
#'
#' # Prepare spatial health data
#' sf_sim <- sus_data_import(uf = "SP", year = 2023, system = "SIM-DO") %>%
#'   sus_data_standardize(lang = "pt") %>%
#'   sus_join_spatial(level = "munic", lang = "pt")
#'
#' # Add census population data
#' sf_enriched <- sus_socio_add_census(
#'   df = sf_sim,
#'   dataset = "population",
#'   census_vars = c("V0001", "V0002"),
#'   year = 2010,
#'   lang = "pt"
#' )
#' }
#' @export
#' @importFrom glue glue
#' @importFrom rlang .data
sus_socio_add_census <- function(
  df,
  dataset = "population",
  year = 2010,
  census_vars = NULL,
  aggregation_fun = "sum",
  join_muni_col = NULL,
  use_cache = TRUE,
  cache_dir = "~/.climasus4r_cache/census",
  lang = "pt",
  translate_columns = TRUE,
  standardize_values = TRUE,
  verbose = TRUE
) {
  # ==========================================================================
  # 1. MULTILINGUAL MESSAGES & VALIDATION
  # ==========================================================================
  cli::cli_h1("climasus4r - Add Census Socioeconomic Variables")

  if (!lang %in% c("pt", "en", "es")) {
  cli::cli_alert_warning("lang must be one of: 'pt', 'en', 'es'")
    lang <- "pt"
  }
  check_spatial(lang)
  
  msg <- get_census_messages(lang)

  # Input validation
  if (!is.data.frame(df)) {cli::cli_abort(msg$error_df_type)}

  valid_datasets <- c(
    "population",
    "households",
    "families",
    "mortality",
    "emigration",
    "tracts"
  )
  if (!dataset %in% valid_datasets) {
    cli::cli_abort(glue::glue(
      msg$error_invalid_dataset,
      valid = paste(valid_datasets, collapse = ", ")
    ))
  }

  valid_funs <- c(
    "sum",
    "mean",
    "median",
    "min",
    "max",
    "sd",
    "q25",
    "q75",
    "q95",
    "q99"
  )
  if (!aggregation_fun %in% valid_funs) {
    cli::cli_abort(glue::glue(
      msg$error_invalid_fun,
      valid = paste(valid_funs, collapse = ", ")
    ))
  }
  # Check if data is climasus_df
  if (inherits(df, "climasus_df")) {

    # Minimum required stage
    required_stage <- "spatial"
    current_stage  <- climasus_meta(df, "stage")

    if (!is_stage_at_least(current_stage, required_stage)) {

      msg_error <- list(
        en = paste0(
          "Data must be spatialized before spatial aggregation\n",
          "Current stage: ", current_stage %||% "unknown", "\n",
          "Required stage: ", required_stage, "\n\n",
          "Please run:\n",
          "  df <- sus_join_spatial(df)"
        ),
        pt = paste0(
          "Dados devem ser espacializados antes de agregar espacialmente.\n",
          "Estagio atual: ", current_stage %||% "desconhecido", "\n",
          "Estagio requerido: ", required_stage, "\n\n",
          "Por favor, execute:\n",
          "  df <- sus_join_spatial(df)"
        ),
        es = paste0(
          "Los datos deben estar espacializado antes de agregar espacialmente.\n",
          "Etapa actual: ", current_stage %||% "desconocida", "\n",
          "Etapa requerida: ", required_stage, "\n\n",
          "Por favor, ejecute:\n",
          "  df <- sus_join_spatial(df)"
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
    df <- climasus_meta(df, stage = "spatial", type = dataset)
  } else {
    
    # NOT climasus_df - ABORT execution
    msg_error <- list(
        en = c(
          "{.red {cli::symbol$cross} Input is not a {.cls climasus_df} object.}",
          "i" = "This function requires data formatted by the {.pkg climasus4r} pipeline.",
          " " = "",
          "Please prepare your data first:",
          "*" = "{.strong 1. Import:} {.code df <- sus_data_import(...)} or {.code sus_data_read(...)}",
          "*" = "{.strong 2. Clean:} {.code df <- sus_data_clean_encoding(df)}",
          "*" = "{.strong 3. Standardize:} {.code df <- sus_data_standardize(df)}",
          "*" = "{.strong 4. Aggregate:} {.code df <- sus_data_aggregate(...)}",
          "*" = "{.strong 5. spatial:} {.code df <- sus_join_spatial(...)}",
          " " = "",
          "v" = "Tip: If using external data, run {.fn sus_data_standardize} first."
        ),
        pt = c(
          "{.red {cli::symbol$cross} A entrada como nao objeto {.cls climasus_df}.}",
          "i" = "Esta funcao requer dados processados pelo pipeline {.pkg climasus4r}.",
          " " = "",
          "Por favor, prepare seus dados primeiro:",
          "*" = "{.strong 1. Importar:} {.code df <- sus_data_import(...)} ou {.code sus_data_read(...)}",
          "*" = "{.strong 2. Limpar:} {.code df <- sus_data_clean_encoding(df)}",
          "*" = "{.strong 3. Padronizar:} {.code df <- sus_data_standardize(df)}",
          "*" = "{.strong 4. Agregar:} {.code df <- sus_data_aggregate(...)}",
          "*" = "{.strong 5. spatial:} {.code df <- sus_join_spatial(...)}",
          " " = "",
          "v" = "Dica: Se usar dados externos, execute {.fn sus_data_standardize} primeiro."
        ),
        es = c(
          "{.red {cli::symbol$cross} La entrada no es un objeto {.cls climasus_df}.}",
          "i" = "Esta funcion requiere datos procesados por el pipeline {.pkg climasus4r}.",
          " " = "",
          "Por favor, prepare sus datos primero:",
          "*" = "{.strong 1. Importar:} {.code df <- sus_data_import(...)} o {.code sus_data_read(...)}",
          "*" = "{.strong 2. Limpiar:} {.code df <- sus_data_clean_encoding(df)}",
          "*" = "{.strong 3. Estandarizar:} {.code df <- sus_data_standardize(df)}",
          "*" = "{.strong 4. Agregar:} {.code df <- sus_data_aggregate(...)}",
          "*" = "{.strong 5. spatial:} {.code df <- sus_join_spatial(...)}",
          " " = "",
          "v" = "Consejo: Si usa datos externos, ejecute {.fn sus_data_standardize} primero."
        )
      )
    
    cli::cli_abort(msg_error[[lang]])
  }

  system <- climasus_meta(df, "system")

  # Check if df is spatial
  is_spatial <- inherits(df, "sf")
  rows_before <- nrow(df)

  # ==========================================================================
  # 2. CONFIGURE CENSOBR CACHE
  # ==========================================================================
  if (use_cache) {
    cache_dir_expanded <- path.expand(cache_dir)
    if (!dir.exists(cache_dir_expanded)) {
      dir.create(cache_dir_expanded, recursive = TRUE, showWarnings = FALSE)
    }
    censobr::set_censobr_cache_dir(cache_dir_expanded, verbose = FALSE)
    if (verbose) {
      cli::cli_alert_info(glue::glue(
        msg$configuring_cache,
        dir = cache_dir_expanded
      ))
    }
  }

  # ==========================================================================
  # 3. IDENTIFY GEOGRAPHIC COLUMN
  # ==========================================================================

  # if (is.null(join_muni_col)) {
  #   # Standard geographic columns from geobr/censobr
  #   geo_cols_standard <- c(
  #     "code_muni",
  #     "code_state",
  #     "abbrev_state",
  #     "name_state",
  #     "code_region",
  #     "name_region",
  #     "code_weighting"
  #   )

  #   # Find which geo columns exist in df
  #   geo_cols_in_df <- intersect(names(df), geo_cols_standard)

  #   # If no standard columns found, try to detect municipality column
  #   if (length(geo_cols_in_df) == 0) {
  #     if (verbose) {
  #       cli::cli_alert_info(msg$detecting_geo_col)
  #     }

  #     # Priority order for municipality columns
  #     priority_order <- c(
  #       "code_muni_7",
  #       "codigo_municipio_residencia",
  #       "residence_municipality_code",
  #       "codigo_municipio_ocorrencia",
  #       "codigo_municipio_ocurrencia",
  #       "occurrence_municipality_code",
  #       "codigo_municipio",
  #       "municipality_code",
  #       "CODMUNRES"
  #     )

  #     join_col <- priority_order[priority_order %in% names(df)][1]

  #     if (is.na(join_col)) {
  #       cli::cli_abort(msg$no_geo_cols)
  #     }
  #     # Rename to code_muni
  #     if (join_col != "code_muni") {
  #       df <- df %>% dplyr::rename(code_muni = dplyr::any_of(join_col))
  #     }
  #     geo_cols_in_df <- "code_muni"
  #   }

  #   if (verbose) {
  #     cli::cli_alert_success(glue::glue(
  #       msg$geo_cols_found,
  #       cols = paste(geo_cols_in_df, collapse = ", ")
  #     ))
  #   }
  # } else {
  #   # Validate user-provided join_col
  #   if (!join_muni_col %in% names(df)) {
  #     cli::cli_abort(paste0(msg$column_not_found, join_muni_col))
  #   }
  #   # Normalize to code_muni
  #   if (join_muni_col != "code_muni") {
  #     df <- df %>% dplyr::rename(code_muni = dplyr::any_of(join_muni_col))
  #   }
  # }
  # Ensure 7 digits (convert 6 to 7 if needed)

  # df$code_muni <- as.character(df$code_muni)
  # if (any(nchar(df$code_muni) == 6)) {
  #   if (verbose) {
  #     cli::cli_alert_info(msg$converting_6_to_7)
  #   }
  #   df$code_muni <- .convert_muni_6_to_7_fast(df$code_muni, cache_dir)
  # }

  # ==========================================================================
  # 4. FETCH & FILTER CENSUS DATA
  # ==========================================================================

  read_fn <- switch(
    dataset,
    "population" = censobr::read_population,
    "households" = censobr::read_households,
    "families" = censobr::read_families,
    "mortality" = censobr::read_mortality,
    "emigration" = censobr::read_emigration,
    "tracts" = censobr::read_tracts
  )

  if (verbose) {
    cli::cli_alert_info(glue::glue(
      msg$fetching_census,
      dataset = dataset,
      year = year
    ))
  }

  census_data <- read_fn(
    year = year,
    showProgress = verbose,
    cache = TRUE,
    verbose = FALSE
  )

#  # Filter municipalities BEFORE loading to RAM (Performance optimization)
  unique_munis <- unique(as.integer(df$code_muni_7))
  unique_munis <- unique_munis[!is.na(unique_munis)]

  if (verbose) {
    cli::cli_alert_info(glue::glue(
      msg$filtering_munis,
      n = format(length(unique_munis), big.mark = ",")
    ))
  }
  
  # ==========================================================================
  # 5. AGGREGATION
  # ==========================================================================
  if (verbose) {
    cli::cli_alert_info(glue::glue(msg$aggregating, mode = aggregation_fun))
  }
  census_data_filtered <- census_data %>%
  dplyr::filter(.data$code_muni %in% !!unique_munis)

  available_cols <- names(census_data)

  if (!is.null(census_vars)) {
    cols_to_select <- unique(c("code_muni", census_vars))
    cols_to_select <- intersect(cols_to_select, available_cols)
  } else {
    cols_to_select <- setdiff(available_cols, c("code_state", "abbrev_state", "name_state", "code_region", "name_region", "code_weighting"))
  }

  census_data_filtered <- census_data %>% dplyr::select(dplyr::all_of(cols_to_select))

# Grouping variables
  group_vars <- census_data_filtered %>%
    dplyr::select(dplyr::where(is.character), dplyr::where(is.factor)) %>%
    names()
  group_vars <- unique(c("code_muni", group_vars))

  # Map aggregation function to native R functions for speed
  agg_fn_internal <- function(fun_type, values) {
    switch(
      fun_type,
      "sum" = sum(values, na.rm = TRUE),
      "mean" = mean(values, na.rm = TRUE),
      "median" = median(values, na.rm = TRUE),
      "min" = min(values, na.rm = TRUE),
      "max" = max(values, na.rm = TRUE),
      "sd" = sd(values, na.rm = TRUE),
      "q25" = quantile(values, 0.25, na.rm = TRUE),
      "q75" = quantile(values, 0.75, na.rm = TRUE),
      "q95" = quantile(values, 0.95, na.rm = TRUE),
      "q99" = quantile(values, 0.99, na.rm = TRUE),
      stop(paste0("Unknown function type: ", fun_type))
    )
  }


  census_agg <- census_data_filtered %>%
    dplyr::filter(.data$code_muni %in% !!unique_munis) %>%
    dplyr::group_by(.data$code_muni) %>%
    dplyr::summarise(
      dplyr::across(
        .cols = dplyr::where(is.numeric),
        .fns = ~ agg_fn_internal(fun_type = aggregation_fun, values = .x)
      ),
      .groups = "drop"
    ) %>%
    dplyr::collect() 
    # dplyr::mutate(dplyr::across(dplyr::where(is.list), ~ as.numeric(unlist(.x)))) %>%
    # dplyr::mutate(code_muni = as.character(.data$code_muni))

  
  # ==========================================================================
  # 6. STANDARDIZATION & JOIN
  # ==========================================================================

  if (translate_columns || standardize_values) {
    census_agg <- .census_data_standardize_optimized(
      census_agg, dataset, lang, translate_columns, standardize_values
    )
    # Add suffix to numeric columns to indicate aggregation
   census_agg$code_muni <- as.character(census_agg$code_muni)
    census_agg <- census_agg %>%
      dplyr::rename_with(
        ~ paste0(.x, "_", aggregation_fun),
        #.cols = dplyr::where(~ {is.numeric(.x)})
        .cols = dplyr::where(is.numeric)
      )
  }
  
  #New = convert IBGE code_muni
  census_agg <- census_agg %>%
    dplyr::mutate(code_muni_7 = as.character(code_muni)) %>%
    dplyr::select(-code_muni)

  if (verbose) {cli::cli_alert_info(msg$joining)}

  df_enriched <- dplyr::left_join(
    df, census_agg, 
    by = "code_muni_7")

  # ==========================================================================
  # 7. FINALIZATION
  # ==========================================================================

  if (is_spatial) {df_enriched <- sf::st_as_sf(df_enriched)}
  
  # Preserve climasus metadata
  if (!inherits(df_enriched, "climasus_df")) {
    # Create new climasus_df
    meta <- list(
      system = system,
      stage = "census",
      type = dataset,
      spatial = inherits(df_enriched, "sf"),
      temporal = NULL,
      created = Sys.time(),
      modified = Sys.time(),
      history = sprintf(
        "[%s] Added census data",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ),
      user = list()
    )

    base_classes <- setdiff(class(df_enriched), "climasus_df")
    df_enriched <- structure(
      df_enriched,
      climasus_meta = meta,
      class = c("climasus_df", base_classes)
    )
  } else { 
    df_enriched <- climasus_meta(
    df_enriched,
    system = system,  # Preserve original system
    stage = "census",
    type = dataset,
    add_history = "[%s] Added census data"
  )     
  }
  
  if (verbose) {
    cli::cli_alert_success(msg$success)
    cli::cli_alert_info(glue::glue(
      msg$rows_summary,
      before = rows_before,
      after = nrow(df_enriched)
    ))
    cli::cli_alert_info(glue::glue(
      msg$vars_added,
      n = ncol(df_enriched) - ncol(df)
    ))
  }

  return(df_enriched)
}

# ==========================================================================
# INTERNAL HELPER FUNCTIONS
# ==========================================================================
#' Optimized Census Standardization
#' @noRd
.census_data_standardize_optimized <- function(df, dataset, lang, translate_cols, std_values) {
  # Get translations from utils-i18n
  translations <- switch(dataset,
    "population" = switch(lang, "en" = get_census_dictionary_en_population(), "es" = get_census_dictionary_es_population(), get_census_dictionary_pt_population()),
    "households" = switch(lang, "en" = get_translate_dictionary_en_households(), "es" = get_translate_dictionary_es_households(), get_translate_dictionary_pt_households()),
    "families"   = switch(lang, "en" = get_translate_dictionary_en_families(), "es" = get_translate_dictionary_es_families(), get_translate_dictionary_pt_families()),
    "mortality"  = switch(lang, "en" = get_translate_dictionary_en_mortality(), "es" = get_translate_dictionary_es_mortality(), get_translate_dictionary_pt_mortality()),
    "emigration" = switch(lang, "en" = get_translate_dictionary_en_emigration(), "es" = get_translate_dictionary_es_emigration(), get_translate_dictionary_pt_emigration())
  )

  # 1. Translate Columns
  if (translate_cols && !is.null(translations$columns)) {
    df <- df %>% 
      dplyr::rename_with(~ as.character(translations$columns[.x]), 
    .cols = dplyr::any_of(names(translations$columns)))
    
    # cols_to_rename <- intersect(names(df), names(translations$columns))
    # if (length(cols_to_rename) > 0) {
    #   df <- df %>% dplyr::rename_with(
    #     ~ unname(translations$columns[.x]),
    #     .cols = dplyr::all_of(cols_to_rename)
    #   )
    # }
  }

  # 2. Standardize Values
  if (std_values && !is.null(translations$values)) {
    for (var in intersect(names(df), names(translations$values))) {
      val_map <- translations$values[[var]]
      df[[var]] <- factor(as.character(df[[var]]), levels = names(val_map), labels = as.character(val_map))
    }
  }

  return(df)
}

#' Get Multilingual Messages for Census Operations
#' @noRd
get_census_messages <- function(lang) {
  list(
    en = list(
      error_df_type = "Input 'df' must be a data.frame or sf object",
      error_invalid_dataset = "Invalid dataset. Must be one of: {valid}",
      error_invalid_fun = "Invalid aggregation function. Must be one of: {valid}",
      detecting_geo_col = "Detecting municipality column...",
      no_geo_cols = "No municipality code column found. Please standardize your data first.",
      converting_6_to_7 = "Converting 6-digit codes to 7-digit IBGE standard...",
      geo_cols_found = "Geographic columns identified: {cols}",
      column_not_found = "Column not found in data frame: ",
      configuring_cache = "Configuring censobr cache in: {dir}",
      fetching_census = "Fetching {dataset} data for {year}...",
      filtering_munis = "Filtering data for {n} municipalities...",
      vars_not_found = "Variables not found in dataset: {vars}",
      aggregating = "Aggregating microdata (Mode: {mode})...",
      joining = "Joining census data to health data...",
      preserving_geom = "Preserving spatial geometries...",
      success = "Census data added successfully!",
      rows_summary = "Rows: {before} (before) -> {after} (after)",
      vars_added = "{n} variables added."
    ),
    pt = list(
      error_df_type = "O input 'df' deve ser um data.frame ou objeto sf",
      error_invalid_dataset = "Dataset invalido. Deve ser um de: {valid}",
      error_invalid_fun = "Funcao de agregacao invalida. Deve ser uma de: {valid}",
      detecting_geo_col = "Detectando coluna de municipio...",
      no_geo_cols = "Nenhuma coluna de codigo de municipio encontrada. Padronize seus dados primeiro.",
      converting_6_to_7 = "Convertendo codigos de 6 digitos para o padrao de 7 digitos do IBGE...",
      geo_cols_found = "Colunas geograficas identificadas: {cols}",
      column_not_found = "Coluna nao encontrada no data frame: ",
      configuring_cache = "Configurando cache do censobr em: {dir}",
      fetching_census = "Buscando dados de {dataset} para {year}...",
      filtering_munis = "Filtrando dados para {n} municipios...",
      vars_not_found = "Variaveis nao encontradas no dataset: {vars}",
      aggregating = "Agregando microdados (Modo: {mode})...",
      joining = "Unindo dados do censo aos dados de saude...",
      preserving_geom = "Preservando geometrias espaciais...",
      success = "Dados do censo adicionados com sucesso!",
      rows_summary = "Linhas: {before} (antes) -> {after} (depois)",
      vars_added = "{n} variaveis adicionadas."
    ),
    es = list(
      error_df_type = "El input 'df' debe ser un data.frame o objeto sf",
      error_invalid_dataset = "Dataset invalido. Debe ser uno de: {valid}",
      error_invalid_fun = "Funcion de agregacion invalida. Debe ser una de: {valid}",
      detecting_geo_col = "Detectando columna de municipio...",
      no_geo_cols = "No se encontro ninguna columna de codigo de municipio. Estandarice sus datos primero.",
      converting_6_to_7 = "Convirtiendo codigos de 6 digitos al estandar de 7 digitos del IBGE...",
      geo_cols_found = "Columnas geograficas identificadas: {cols}",
      column_not_found = "Columna no encontrada en el data frame: ",
      configuring_cache = "Configurando cache de censobr en: {dir}",
      fetching_census = "Buscando datos de {dataset} para {year}...",
      filtering_munis = "Filtrando datos para {n} municipios...",
      vars_not_found = "Variables no encontradas en el dataset: {vars}",
      aggregating = "Agregando microdatos (Modo: {mode})...",
      joining = "Uniendo datos del censo a los datos de salud...",
      preserving_geom = "Preservando geometrias espaciales...",
      success = "Datos del censo agregados con exito!",
      rows_summary = "Filas: {before} (antes) -> {after} (despues)",
      vars_added = "{n} variables agregadas."
    )
  )[[lang]]
}
