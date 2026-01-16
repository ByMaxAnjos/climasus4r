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
#' @param vars Character vector specifying census variables to add. Use `sus_census_explore()` to select available variables.
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
#'   vars = c("V0001", "V0002"),
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
  vars = NULL,
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
  if (!lang %in% c("pt", "en", "es")) {
  cli::cli_abort("lang must be one of: 'pt', 'en', 'es'")
  }
  check_spatial(lang)
  
  msg <- get_census_messages(lang)

  # Input validation
  if (!is.data.frame(df)) {
    cli::cli_abort(msg$error_df_type)
  }

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

  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_abort("Argument 'lang' must be 'en', 'pt', or 'es'")
  }

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

  if (is.null(join_muni_col)) {
    # Standard geographic columns from geobr/censobr
    geo_cols_standard <- c(
      "code_muni",
      "code_state",
      "abbrev_state",
      "name_state",
      "code_region",
      "name_region",
      "code_weighting"
    )

    # Find which geo columns exist in df
    geo_cols_in_df <- intersect(names(df), geo_cols_standard)

    # If no standard columns found, try to detect municipality column
    if (length(geo_cols_in_df) == 0) {
      if (verbose) {
        cli::cli_alert_info(msg$detecting_geo_col)
      }

      # Priority order for municipality columns
      priority_order <- c(
        "code_muni",
        "codigo_municipio_residencia",
        "residence_municipality_code",
        "codigo_municipio_ocorrencia",
        "codigo_municipio_ocurrencia",
        "occurrence_municipality_code",
        "codigo_municipio",
        "municipality_code",
        "CODMUNRES"
      )

      join_col <- priority_order[priority_order %in% names(df)][1]

      if (is.na(join_col)) {
        cli::cli_abort(msg$no_geo_cols)
      }
      # Rename to code_muni
      if (join_col != "code_muni") {
        df <- df %>% dplyr::rename(code_muni = dplyr::any_of(join_col))
      }
      geo_cols_in_df <- "code_muni"
    }

    if (verbose) {
      cli::cli_alert_success(glue::glue(
        msg$geo_cols_found,
        cols = paste(geo_cols_in_df, collapse = ", ")
      ))
    }
  } else {
    # Validate user-provided join_col
    if (!join_muni_col %in% names(df)) {
      cli::cli_abort(paste0(msg$column_not_found, join_muni_col))
    }
    # Normalize to code_muni
    if (join_muni_col != "code_muni") {
      df <- df %>% dplyr::rename(code_muni = dplyr::any_of(join_muni_col))
    }
  }
  # Ensure 7 digits (convert 6 to 7 if needed)
  df$code_muni <- as.character(df$code_muni)
  if (any(nchar(df$code_muni) == 6)) {
    if (verbose) {
      cli::cli_alert_info(msg$converting_6_to_7)
    }
    df$code_muni <- .convert_muni_6_to_7_fast(df$code_muni, cache_dir)
  }

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

  # Filter municipalities BEFORE loading to RAM (Performance optimization)
  unique_munis <- unique(as.character(df$code_muni))
  unique_munis <- unique_munis[!is.na(unique_munis)]

  if (verbose) {
    cli::cli_alert_info(glue::glue(
      msg$filtering_munis,
      n = format(length(unique_munis), big.mark = ",")
    ))
  }

  census_data_filtered <- census_data %>%
    dplyr::filter(.data$code_muni %in% !!as.integer(unique_munis))

  cols_to_select <- setdiff(
    names(census_data_filtered),
    c(
      "code_state",
      "abbrev_state",
      "name_state",
      "code_region",
      "name_region",
      "code_weighting"
    )
  )

  # Select variables
  if (!is.null(vars)) {
    # Ensure code_muni is always selected
    cols_to_select <- unique(c("code_muni", vars))
    available_cols <- names(census_data_filtered)
    cols_to_select <- intersect(cols_to_select, available_cols)

    missing_vars <- setdiff(vars, available_cols)
    if (length(missing_vars) > 0 && verbose) {
      cli::cli_alert_warning(glue::glue(
        msg$vars_not_found,
        vars = paste(missing_vars, collapse = ", ")
      ))
    }

    census_data_filtered <- census_data_filtered %>%
      dplyr::select(dplyr::all_of(cols_to_select))
  }

  # ==========================================================================
  # 5. STRATIFIED AGGREGATION
  # ==========================================================================
  if (verbose) {
    cli::cli_alert_info(glue::glue(msg$aggregating, mode = aggregation_fun))
  }

  # Identify grouping variables (code_muni + any categorical variable in vars)
  # We use a character-based approach for grouping to preserve stratifications
  group_vars <- census_data_filtered %>%
    dplyr::select(dplyr::all_of(cols_to_select)) %>%
    dplyr::select(dplyr::where(is.character), dplyr::where(is.factor)) %>%
    names()
  # Ensure code_muni is always in group_vars
  group_vars <- unique(c("code_muni", group_vars))

  # Map aggregation function to native R functions for speed
  agg_fn <- function(fun_type, values) {
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
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      dplyr::across(
        .cols = dplyr::where(is.numeric),
        .fns = ~ agg_fn(fun_type = aggregation_fun, values = .x)
      ),
      .groups = "drop"
    ) %>%
    dplyr::collect() %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.list), ~ as.numeric(unlist(.x)))) %>%
    dplyr::mutate(code_muni = as.character(.data$code_muni))

  # ==========================================================================
  # 6. STANDARDIZATION & JOIN
  # ==========================================================================

  if (translate_columns || standardize_values) {
    census_agg <- .census_data_standardize_optimized(
      census_agg,
      dataset,
      lang,
      translate_columns,
      standardize_values
    )
    # Add suffix to numeric columns to indicate aggregation
    census_agg <- census_agg %>%
      dplyr::rename_with(
        ~ paste0(.x, "_", aggregation_fun),
        .cols = dplyr::where(is.numeric)
      )
  }

  if (verbose) {
    cli::cli_alert_info(msg$joining)
  }

  df_enriched <- dplyr::left_join(
    df,
    census_agg,
    by = "code_muni",
    relationship = "many-to-many"
  )

  # ==========================================================================
  # 7. FINALIZATION
  # ==========================================================================

  if (is_spatial) {
    df_enriched <- sf::st_as_sf(df_enriched)
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

  return(if (is_spatial) df_enriched else dplyr::as_tibble(df_enriched))
}

# ==========================================================================
# INTERNAL HELPER FUNCTIONS
# ==========================================================================

#' Fast 6 to 7 digit municipality code conversion
#' @noRd
.convert_muni_6_to_7_fast <- function(muni_code_6, cache_dir) {
  # Use a static lookup table or a very fast join
  # For now, we'll use a simplified version of the original logic but optimized
  muni_code_6 <- as.character(muni_code_6)

  lookup <- get_spatial_muni_with_cache(spatial_cache_dir = cache_dir)

  if (is.null(lookup)) {
    lookup <- geobr::read_municipality(
    code_muni = "all",
    as_sf = FALSE,
    showProgress = FALSE) 
  }
  # Load only the codes from geobr (much faster than full shapes)
   lookup <- lookup %>%
    sf::st_drop_geometry() %>%
    dplyr::transmute(
      code_muni_6 = substr(as.character(.data$code_muni), 1, 6),
      code_muni_7 = as.character(.data$code_muni)
    ) %>%
    dplyr::distinct()
    
  lookup$code_muni_7[match(muni_code_6, lookup$code_muni_6)]
}

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
    df <- df %>% dplyr::rename_with(~ as.character(translations$columns[.x]), .cols = dplyr::any_of(names(translations$columns)))
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

#' Get Spatial Data with Caching
#'
#' Retrieves spatial data from IBGE with intelligent caching to improve performance.
#' Spatial data is cached as Parquet files for fast reloading.
#' @param spatial_cache_dir Cache directory path
#' @return sf object with spatial data
#' @keywords internal
#' @noRd
get_spatial_muni_with_cache <- function(spatial_cache_dir) {
  # Ensure the directory exists
  if (!dir.exists(spatial_cache_dir)) dir.create(spatial_cache_dir, recursive = TRUE)

  cache_file <- file.path(spatial_cache_dir, "sf_municipality_2010.parquet")

  if (file.exists(cache_file)) {
    return(sfarrow::st_read_parquet(cache_file))
  } else {
    spatial_df <- geobr::read_municipality(code_muni = "all", simplified = TRUE, showProgress = FALSE)
    
    # Corrected: Write to the full file path
    sfarrow::st_write_parquet(obj = spatial_df, dsn = cache_file)
    
    return(spatial_df)
  }
}