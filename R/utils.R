#' Multilingual Translation System for CLIMASUS4r
#'
#' Internal functions to support Portuguese, Spanish, and English translations
#' throughout the package.
#'
#' @keywords internal
#' @name utils
NULL

#' Get translation dictionary
#'
#' Returns a comprehensive translation dictionary for column names and values
#' across Portuguese (pt), Spanish (es), and English (en).
#'
#' @param lang Character. Language code: "pt" (Portuguese), "es" (Spanish), or "en" (English).
#'
#' @return A list containing translation mappings.
#' @keywords internal
get_translation_dict <- function(lang = "en") {
  
  # Validate language
  if (!lang %in% c("en", "pt", "es")) {
    warning(sprintf("Language '%s' not supported. Defaulting to English.", lang))
    lang <- "en"
  }
  
  # Column name translations
  column_translations <- list(
    en = list(
      "ORIGEM" = "origin",
      "TIPOBITO" = "death_type",
      "DTOBITO" = "death_date",
      "HORAOBITO" = "death_time",
      "CODMUNNATU" = "birth_municipality_code",
      "DTNASC" = "birth_date",
      "IDADE" = "age_code",
      "IDADEanos" = "age_years",
      "IDADEmeses" = "age_months",
      "IDADEdias" = "age_days",
      "IDADEhoras" = "age_hours",
      "IDADEminutos" = "age_minutes",
      "SEXO" = "sex",
      "RACACOR" = "race",
      "ESTCIV" = "marital_status",
      "ESC" = "education",
      "ESC2010" = "education_2010",
      "OCUP" = "occupation",
      "CODMUNRES" = "residence_municipality_code",
      "LOCOCOR" = "occurrence_place",
      "CODESTAB" = "establishment_code",
      "munResNome" = "residence_municipality_name",
      "munResUf" = "residence_state",
      "munResLat" = "residence_latitude",
      "munResLon" = "residence_longitude",
      "CAUSABAS" = "underlying_cause",
      "LINHAA" = "cause_line_a"
    ),
    pt = list(
      "ORIGEM" = "origem",
      "TIPOBITO" = "tipo_obito",
      "DTOBITO" = "data_obito",
      "HORAOBITO" = "hora_obito",
      "CODMUNNATU" = "codigo_municipio_nascimento",
      "DTNASC" = "data_nascimento",
      "IDADE" = "codigo_idade",
      "IDADEanos" = "idade_anos",
      "IDADEmeses" = "idade_meses",
      "IDADEdias" = "idade_dias",
      "IDADEhoras" = "idade_horas",
      "IDADEminutos" = "idade_minutos",
      "SEXO" = "sexo",
      "RACACOR" = "raca_cor",
      "ESTCIV" = "estado_civil",
      "ESC" = "escolaridade",
      "ESC2010" = "escolaridade_2010",
      "OCUP" = "ocupacao",
      "CODMUNRES" = "codigo_municipio_residencia",
      "LOCOCOR" = "local_ocorrencia",
      "CODESTAB" = "codigo_estabelecimento",
      "munResNome" = "nome_municipio_residencia",
      "munResUf" = "uf_residencia",
      "munResLat" = "latitude_residencia",
      "munResLon" = "longitude_residencia",
      "CAUSABAS" = "causa_basica",
      "LINHAA" = "linha_causa_a"
    ),
    es = list(
      "ORIGEM" = "origen",
      "TIPOBITO" = "tipo_muerte",
      "DTOBITO" = "fecha_muerte",
      "HORAOBITO" = "hora_muerte",
      "CODMUNNATU" = "codigo_municipio_nacimiento",
      "DTNASC" = "fecha_nacimiento",
      "IDADE" = "codigo_edad",
      "IDADEanos" = "edad_anos",
      "IDADEmeses" = "edad_meses",
      "IDADEdias" = "edad_dias",
      "IDADEhoras" = "edad_horas",
      "IDADEminutos" = "edad_minutos",
      "SEXO" = "sexo",
      "RACACOR" = "raza_color",
      "ESTCIV" = "estado_civil",
      "ESC" = "escolaridad",
      "ESC2010" = "escolaridad_2010",
      "OCUP" = "ocupacion",
      "CODMUNRES" = "codigo_municipio_residencia",
      "LOCOCOR" = "lugar_ocurrencia",
      "CODESTAB" = "codigo_establecimiento",
      "munResNome" = "nombre_municipio_residencia",
      "munResUf" = "uf_residencia",
      "munResLat" = "latitud_residencia",
      "munResLon" = "longitud_residencia",
      "CAUSABAS" = "causa_basica",
      "LINHAA" = "linea_causa_a"
    )
  )
  
  # Categorical value translations
  value_translations <- list(
    sex = list(
      en = list("1" = "Male", "2" = "Female", "0" = "Ignored",
                "Masculino" = "Male", "Feminino" = "Female", "Ignorado" = "Ignored"),
      pt = list("1" = "Masculino", "2" = "Feminino", "0" = "Ignorado",
                "Male" = "Masculino", "Female" = "Feminino", "Ignored" = "Ignorado"),
      es = list("1" = "Masculino", "2" = "Femenino", "0" = "Ignorado",
                "Male" = "Masculino", "Female" = "Femenino", "Ignored" = "Ignorado")
    ),
    race = list(
      en = list("1" = "White", "2" = "Black", "3" = "Yellow", "4" = "Brown", "5" = "Indigenous", "0" = "Ignored",
                "Branca" = "White", "Preta" = "Black", "Amarela" = "Yellow", "Parda" = "Brown", 
                "Ind\u00edgena" = "Indigenous", "Ignorado" = "Ignored"),
      pt = list("1" = "Branca", "2" = "Preta", "3" = "Amarela", "4" = "Parda", "5" = "Ind\u00edgena", "0" = "Ignorado",
                "White" = "Branca", "Black" = "Preta", "Yellow" = "Amarela", "Brown" = "Parda",
                "Indigenous" = "Ind\u00edgena", "Ignored" = "Ignorado"),
      es = list("1" = "Blanca", "2" = "Negra", "3" = "Amarilla", "4" = "Parda", "5" = "Ind\u00edgena", "0" = "Ignorado",
                "White" = "Blanca", "Black" = "Negra", "Yellow" = "Amarilla", "Brown" = "Parda",
                "Indigenous" = "Ind\u00edgena", "Ignored" = "Ignorado")
    ),
    death_type = list(
      en = list("1" = "Fetal", "2" = "Non-fetal",
                "Fetal" = "Fetal", "N\u00e3o Fetal" = "Non-fetal"),
      pt = list("1" = "Fetal", "2" = "N\u00e3o Fetal",
                "Fetal" = "Fetal", "Non-fetal" = "N\u00e3o Fetal"),
      es = list("1" = "Fetal", "2" = "No Fetal",
                "Fetal" = "Fetal", "Non-fetal" = "No Fetal")
    ),
    marital_status = list(
      en = list("1" = "Single", "2" = "Married", "3" = "Widowed", "4" = "Legally separated", 
                "5" = "Stable union", "9" = "Ignored",
                "Solteiro" = "Single", "Casado" = "Married", "Vi\u00favo" = "Widowed"),
      pt = list("1" = "Solteiro", "2" = "Casado", "3" = "Vi\u00favo", "4" = "Separado judicialmente",
                "5" = "Uni\u00e3o est\u00e1vel", "9" = "Ignorado",
                "Single" = "Solteiro", "Married" = "Casado", "Widowed" = "Vi\u00favo"),
      es = list("1" = "Soltero", "2" = "Casado", "3" = "Viudo", "4" = "Separado judicialmente",
                "5" = "Uni\u00f3n estable", "9" = "Ignorado",
                "Single" = "Soltero", "Married" = "Casado", "Widowed" = "Viudo")
    )
  )
  
  return(list(
    columns = column_translations[[lang]],
    values = value_translations,
    lang = lang
  ))
}

#' Get UI messages in specified language
#'
#' Returns user interface messages in the specified language.
#'
#' @param lang Character. Language code: "pt", "es", or "en".
#'
#' @return A list of translated UI messages.
#' @keywords internal
get_ui_messages <- function(lang = "en") {
  
  messages <- list(
    en = list(
      data_import_header = "Climasus4r Data Import",
      system = "System",
      states = "States",
      years = "Years",
      total_downloads = "Total downloads",
      parallel_enabled = "Parallel processing: ENABLED",
      workers = "workers",
      download_completed = "Download and combination completed successfully!",
      total_records = "Total records",
      total_columns = "Total columns",
      encoding_header = "Climasus4r Encoding Cleanup",
      checking_columns = "Checking text columns for encoding issues...",
      corrected_columns = "Corrected column(s)",
      affected_columns = "Affected columns",
      no_correction_needed = "All text columns have correct encoding. No corrections needed.",
      standardization_header = "Climasus4r Data Standardization",
      original_columns = "Original columns",
      original_records = "Original records",
      translated_columns = "Translated column names to",
      standardized_values = "Standardized values in categorical variable(s)",
      standardization_completed = "Standardization completed!",
      final_columns = "Final columns",
      filtering_header = "Climasus4r ICD-10 Filtering",
      icd_column = "ICD column",
      filter_codes = "Filter codes",
      match_type = "Match type",
      filtering_completed = "Filtering completed!",
      records_kept = "Records kept",
      records_removed = "Records removed"
    ),
    pt = list(
      data_import_header = "climasus4r Importa\u00e7\u00e3o de Dados",
      system = "Sistema",
      states = "Estados",
      years = "Anos",
      total_downloads = "Total de downloads",
      parallel_enabled = "Processamento paralelo: ATIVADO",
      workers = "trabalhadores",
      download_completed = "Download e combina\u00e7\u00e3o conclu\u00eddos com sucesso!",
      total_records = "Total de registros",
      total_columns = "Total de colunas",
      encoding_header = "climasus4r Limpeza de Codifica\u00e7\u00e3o",
      checking_columns = "Verificando colunas de texto para problemas de codifica\u00e7\u00e3o...",
      corrected_columns = "Coluna(s) corrigida(s)",
      affected_columns = "Colunas afetadas",
      no_correction_needed = "Todas as colunas de texto t\u00eam codifica\u00e7\u00e3o correta. Nenhuma corre\u00e7\u00e3o necess\u00e1ria.",
      standardization_header = "climasus4r Padroniza\u00e7\u00e3o de Dados",
      original_columns = "Colunas originais",
      original_records = "Registros originais",
      translated_columns = "Nomes de colunas traduzidos para",
      standardized_values = "Valores padronizados em vari\u00e1vel(is) categ\u00f3rica(s)",
      standardization_completed = "Padroniza\u00e7\u00e3o conclu\u00edda!",
      final_columns = "Colunas finais",
      filtering_header = "climasus4r Filtragem por CID-10",
      icd_column = "Coluna CID",
      filter_codes = "C\u00f3digos de filtro",
      match_type = "Tipo de correspond\u00eancia",
      filtering_completed = "Filtragem conclu\u00edda!",
      records_kept = "Registros mantidos",
      records_removed = "Registros removidos"
    ),
    es = list(
      data_import_header = "climasus4r Importaci\u00f3n de Datos",
      system = "Sistema",
      states = "Estados",
      years = "A\u00f1os",
      total_downloads = "Total de descargas",
      parallel_enabled = "Procesamiento paralelo: ACTIVADO",
      workers = "trabajadores",
      download_completed = "\u00a1Descarga y combinaci\u00f3n completadas con \u00e9xito!",
      total_records = "Total de registros",
      total_columns = "Total de columnas",
      encoding_header = "climasus4r Limpieza de Codificaci\u00f3n",
      checking_columns = "Verificando columnas de texto para problemas de codificaci\u00f3n...",
      corrected_columns = "Columna(s) corregida(s)",
      affected_columns = "Columnas afectadas",
      no_correction_needed = "Todas las columnas de texto tienen codificaci\u00f3n correcta. No se necesitan correcciones.",
      standardization_header = "climasus4r Estandarizaci\u00f3n de Datos",
      original_columns = "Columnas originales",
      original_records = "Registros originales",
      translated_columns = "Nombres de columnas traducidos a",
      standardized_values = "Valores estandarizados en variable(s) categ\u00f3rica(s)",
      standardization_completed = "\u00a1Estandarizaci\u00f3n completada!",
      final_columns = "Columnas finales",
      filtering_header = "climasus4r Filtrado por CIE-10",
      icd_column = "Columna CIE",
      filter_codes = "C\u00f3digos de filtro",
      match_type = "Tipo de coincidencia",
      filtering_completed = "\u00a1Filtrado completado!",
      records_kept = "Registros mantenidos",
      records_removed = "Registros eliminados"
    )
  )
  
  if (!lang %in% names(messages)) {
    warning(sprintf("Language '%s' not supported for UI messages. Defaulting to English.", lang))
    lang <- "en"
  }
  
  return(messages[[lang]])
}


# Additional helper functions for cache management

#' Clear the CLIMASUS4r cache
#'
#' @param cache_dir Character. Cache directory to clear. Default is "~/.climasus4r_cache".
#' @param older_than_days Numeric. Only clear files older than this many days. NULL clears all.
#' @param verbose Logical. If TRUE, prints information about what was cleared.
#'
#' @importFrom stats median sd quantile
#' @export
#'
#' @examples
#' \dontrun{
#' # Clear all cache
#' clear_climasus_cache()
#'
#' # Clear only files older than 90 days
#' clear_climasus_cache(older_than_days = 90)
#' }
clear_climasus_cache <- function(cache_dir = "~/.climasus4r_cache", 
                                 older_than_days = NULL,
                                 verbose = TRUE) {
  
  cache_dir <- path.expand(cache_dir)
  
  if (!fs::dir_exists(cache_dir)) {
    if (verbose) cli::cli_alert_info("Cache directory does not exist: {cache_dir}")
    return(invisible(NULL))
  }
  
  cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  
  if (length(cache_files) == 0) {
    if (verbose) cli::cli_alert_info("No cache files found in {cache_dir}")
    return(invisible(NULL))
  }
  
  if (!is.null(older_than_days)) {
    file_info <- fs::file_info(cache_files)
    file_age <- difftime(Sys.time(), file_info$modification_time, units = "days")
    files_to_delete <- cache_files[file_age > older_than_days]
  } else {
    files_to_delete <- cache_files
  }
  
  if (length(files_to_delete) == 0) {
    if (verbose) cli::cli_alert_info("No files match the criteria for deletion.")
    return(invisible(NULL))
  }
  
  total_size <- sum(file.info(files_to_delete)$size, na.rm = TRUE)
  
  if (verbose) {
    cli::cli_alert_info("Clearing cache: {cache_dir}")
    cli::cli_alert_info("Files to delete: {length(files_to_delete)}")
    cli::cli_alert_info("Total size: {format(total_size, big.mark = ',')} bytes")
    
    if (interactive()) {
      proceed <- utils::menu(
        c("Yes", "No"),
        title = paste("Are you sure you want to delete", length(files_to_delete), "files?")
      )
      if (proceed != 1) {
        cli::cli_alert_info("Cache clearing cancelled.")
        return(invisible(NULL))
      }
    }
  }
  
  unlink(files_to_delete)
  
  if (verbose) {
    cli::cli_alert_success("Successfully cleared {length(files_to_delete)} cache files.")
  }
  
  return(invisible(length(files_to_delete)))
}

#' Get cache information
#'
#' @param cache_dir Character. Cache directory to inspect. Default is "~/.climasus4r_cache".
#' @param verbose Logical. If TRUE, prints detailed information.
#'
#' @return A list with cache statistics.
#' @export
#'
#' @examples
#' \dontrun{
#' cache_info <- get_climasus_cache_info()
#' }
get_climasus_cache_info <- function(cache_dir = "~/.climasus4r_cache", 
                                    verbose = TRUE) {
  
  cache_dir <- path.expand(cache_dir)
  
  if (!fs::dir_exists(cache_dir)) {
    if (verbose) cli::cli_alert_info("Cache directory does not exist: {cache_dir}")
    return(list(
      exists = FALSE,
      cache_dir = cache_dir,
      file_count = 0,
      total_size = 0
    ))
  }
  
  cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  
  if (length(cache_files) == 0) {
    cache_stats <- list(
      exists = TRUE,
      cache_dir = cache_dir,
      file_count = 0,
      total_size = 0,
      newest_file = NA,
      oldest_file = NA
    )
  } else {
    file_info <- fs::file_info(cache_files)
    
    cache_stats <- list(
      exists = TRUE,
      cache_dir = cache_dir,
      file_count = length(cache_files),
      total_size = sum(file_info$size, na.rm = TRUE),
      newest_file = max(file_info$modification_time, na.rm = TRUE),
      oldest_file = min(file_info$modification_time, na.rm = TRUE),
      avg_file_size = mean(file_info$size, na.rm = TRUE),
      median_file_size = median(file_info$size, na.rm = TRUE)
    )
    
    # Parse file names to get system distribution
    file_names <- basename(cache_files)
    if (verbose) {
      cli::cli_h2("Cache Information")
      cli::cli_alert_info("Cache directory: {cache_dir}")
      cli::cli_alert_info("Total files: {cache_stats$file_count}")
      cli::cli_alert_info("Total size: {format(cache_stats$total_size, big.mark = ',')} bytes")
      cli::cli_alert_info("Oldest file: {cache_stats$oldest_file}")
      cli::cli_alert_info("Newest file: {cache_stats$newest_file}")
    }
  }
  
  return(cache_stats)
}

#' Map system codes to their respective processing functions
#'
#' @param system The system code (e.g., "SIM-DO", "SINAN-DENGUE")
#' @return The name of the processing function to use
#' @noRd
get_processing_function <- function(system) {
  system_mappings <- list(
    # SIM systems
    "SIM-DO" = "process_sim",
    "SIM-DOFET" = "process_sim",
    "SIM-DOEXT" = "process_sim",
    "SIM-DOINF" = "process_sim",
    "SIM-DOMAT" = "process_sim",
    
    # SINASC
    "SINASC" = "process_sinasc",
    
    # SIH systems
    "SIH-RD" = "process_sih",
    "SIH-RJ" = "process_sih",
    "SIH-SP" = "process_sih",
    "SIH-ER" = "process_sih",
    
    # CNES systems
    "CNES-LT" = "process_cnes",
    "CNES-ST" = "process_cnes",
    "CNES-DC" = "process_cnes",
    "CNES-EQ" = "process_cnes",
    "CNES-SR" = "process_cnes",
    "CNES-HB" = "process_cnes",
    "CNES-PF" = "process_cnes",
    "CNES-EP" = "process_cnes",
    "CNES-RC" = "process_cnes",
    "CNES-IN" = "process_cnes",
    "CNES-EE" = "process_cnes",
    "CNES-EF" = "process_cnes",
    "CNES-GM" = "process_cnes",
    
    # SIA systems
    "SIA-AB" = "process_sia",
    "SIA-ABO" = "process_sia",
    "SIA-ACF" = "process_sia",
    "SIA-AD" = "process_sia",
    "SIA-AN" = "process_sia",
    "SIA-AM" = "process_sia",
    "SIA-AQ" = "process_sia",
    "SIA-AR" = "process_sia",
    "SIA-ATD" = "process_sia",
    "SIA-PA" = "process_sia",
    "SIA-PS" = "process_sia",
    "SIA-SAD" = "process_sia",
    
    # SINAN systems
    "SINAN-DENGUE" = "process_sinan_dengue",
    "SINAN-CHIKUNGUNYA" = "process_sinan_chikungunya",
    "SINAN-ZIKA" = "process_sinan_zika",
    "SINAN-MALARIA" = "process_sinan_malaria",
    "SINAN-CHAGAS" = "process_sinan_chagas",
    "SINAN-LEISHMANIOSE-TEGUMENTAR" = "process_sinan_leishmaniose_tegumentar",
    "SINAN-LEISHMANIOSE-VISCERAL" = "process_sinan_leishmaniose_visceral",
    "SINAN-LEPTOSPIROSE" = "process_sinan_malaria"  # Usa process_sinan_malaria como fallback
  )
  
  return(system_mappings[[system]])
}

#' Check if a processing function exists and is available
#'
#' @param func_name Name of the processing function
#' @return Logical indicating if the function exists
#' @noRd
check_processing_function <- function(func_name) {
  exists(func_name, mode = "function")
}

#' Process data according to system type
#'
#' @param data Raw data from DATASUS
#' @param system System code
#' @param verbose Whether to print messages
#' @return Processed data
#' @noRd
process_data_by_system <- function(data, system, verbose = TRUE) {
  if (is.null(data) || nrow(data) == 0) {
    return(data)
  }
  
  # Get the appropriate processing function
  process_func_name <- get_processing_function(system)
  
  if (is.null(process_func_name)) {
    if (verbose) {
      cli::cli_alert_warning("No processing function defined for system: {system}")
      cli::cli_alert_info("Returning raw data without processing")
    }
    return(data)
  }
  
  # Check if the function exists
  if (!check_processing_function(process_func_name)) {
    if (verbose) {
      cli::cli_alert_warning("Processing function '{process_func_name}' not found")
      cli::cli_alert_info("Make sure the function is defined or the package is loaded")
      cli::cli_alert_info("Returning raw data without processing")
    }
    return(data)
  }
  
  # Apply the processing function
  tryCatch({
    if (verbose) {
      cli::cli_alert_info("Processing {system} data with {process_func_name}()...")
    }
    
    # Get the function from the global environment
    process_func <- get(process_func_name, mode = "function")
    
    # Apply processing
    processed_data <- process_func(data)
    
    if (verbose) {
      cli::cli_alert_success("Data processing completed for {system}")
      cli::cli_alert_info("Before processing: {nrow(data)} rows, {ncol(data)} columns")
      cli::cli_alert_info("After processing: {nrow(processed_data)} rows, {ncol(processed_data)} columns")
    }
    
    return(processed_data)
    
  }, error = function(e) {
    if (verbose) {
      cli::cli_alert_warning("Failed to process {system} data: {e$message}")
      cli::cli_alert_info("Returning raw data")
    }
    return(data)
  })
}