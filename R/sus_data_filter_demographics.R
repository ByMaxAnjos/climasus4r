#' Filter Health Data by Demographic Variables
#'
#' Filters health data based on demographic characteristics such as sex, race,
#' age range, education level, and marital status. This function complements
#' `sus_data_filter_cid()` by enabling stratified analyses by population subgroups.
#'
#' @param df A data frame containing health data.
#' @param sex Character vector specifying sex categories to include. Accepts
#'   values in English, Portuguese, or Spanish (e.g., `"Male"`, `"Masculino"`,
#'   `"Masculino"`). If `NULL` (default), includes all sexes.
#' @param race Character vector specifying race/color categories to include.
#'   Accepts IBGE standard categories in multiple languages. If `NULL` (default),
#'   includes all races.
#' @param age_range Numeric vector of length 2 specifying the age range
#'   `c(min_age, max_age)`. Use `Inf` for no upper limit. If `NULL` (default),
#'   includes all ages.
#' @param education Character vector specifying education levels to include.
#'   If `NULL` (default), includes all education levels.
#' @param marital_status Character vector specifying marital status categories
#'   to include. If `NULL` (default), includes all marital statuses.
#' @param region A string indicating a predefined group of states or regions (supports multilingual names PT, EN, ES). See below in details.
#' @param municipality_code Character or numeric vector specifying municipality
#'   codes (IBGE 6 or 7-digit codes) to include. If `NULL` (default), includes
#'   all municipalities.
#' @param lang Character string specifying the language for messages. Options:
#'   `"en"` (English), `"pt"` (Portuguese, default), `"es"` (Spanish).
#' @param verbose Logical. If `TRUE` (default), prints filtering summary.
#'
#' @return A filtered data frame containing only rows that match all specified
#'   demographic criteria.
#'
#' @details
#' The function automatically detects column names in different languages and
#' standardizations. It handles both original DATASUS column names and standardized
#' names from `sus_data_standardize()`.
#'
#' **Sex categories** (case-insensitive):
#' \itemize{
#'   \item English: `"Male"`, `"Female"`, `"Unknown"`
#'   \item Portuguese: `"Masculino"`, `"Feminino"`, `"Ignorado"`
#'   \item Spanish: `"Masculino"`, `"Femenino"`, `"Desconocido"`
#' }
#'
#' **Race/Color categories** (IBGE standard):
#' \itemize{
#'   \item English: `"White"`, `"Black"`, `"Yellow"`, `"Brown"`, `"Indigenous"`
#'   \item Portuguese: `"Branca"`, `"Preta"`, `"Amarela"`, `"Parda"`, `"Indigena"`
#'   \item Spanish: `"Blanca"`, `"Negra"`, `"Amarilla"`, `"Parda"`, `"Indigena"`
#' }
#' **IBGE Macro-regions:**
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
#' @examples
#' \dontrun{
#' library(climasus4r)
#'
#' # Filter by sex only
#' df_women <- sus_data_filter_demographics(df, sex = "Female")
#'
#' # Filter by age range (elderly, 65+)
#' df_elderly <- sus_data_filter_demographics(df, age_range = c(65, Inf))
#'
#' # Filter by multiple criteria (elderly women)
#' df_elderly_women <- sus_data_filter_demographics(
#'   df,
#'   sex = c("Feminino"),
#'   age_range = c(65, Inf),
#'   lang = "pt"
#' )
#'
#' # Filter by race and municipality
#' df_subset <- sus_data_filter_demographics(
#'   df,
#'   race = c("Branca", "Parda"),
#'   municipality_code = c("3550308", "3304557"),  # Sao Paulo, Rio de Janeiro
#'   lang = "pt"
#' )
#'
#' # Complex filtering (children under 5, both sexes, specific municipality)
#' df_children <- sus_data_filter_demographics(
#'   df,
#'   age_range = c(0, 5),
#'   region = "Norte",
#'   lang = "en"
#' )
#' }
#'
#' @export
sus_data_filter_demographics <- function(df,
                                          sex = NULL,
                                          race = NULL,
                                          age_range = NULL,
                                          education = NULL,
                                          marital_status = NULL,
                                          municipality_code = NULL,
                                          lang = "pt",
                                          verbose = TRUE) {
  
  # Validate inputs
  if (!is.data.frame(df)) {
    cli::cli_abort("df must be a data frame")
  }

  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_abort("lang must be one of: 'en', 'pt', 'es'")
  }

  # Check if data is climasus_df
  if (inherits(df, "climasus_df")) {

    # Minimum required stage
    required_stage <- "stand"
    current_stage  <- climasus_meta(df, "stage")

    if (!is_stage_at_least(current_stage, required_stage)) {

      msg_error <- list(
        en = paste0(
          "Data must be standardized before demographic filtering.\n",
          "Current stage: ", current_stage %||% "unknown", "\n",
          "Required stage: ", required_stage, "\n\n",
          "Please run:\n",
          "  df <- sus_data_standardize(df)"
        ),
        pt = paste0(
          "Dados devem ser padronizados antes da filtragem demografica.\n",
          "Estagio atual: ", current_stage %||% "desconhecido", "\n",
          "Estagio requerido: ", required_stage, "\n\n",
          "Por favor, execute:\n",
          "  df <- sus_data_standardize(df)"
        ),
        es = paste0(
          "Los datos deben estar estandarizados antes del filtrado demografico.\n",
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
        en = "Data stage validated: demographic filtering",
        pt = "Estagio de dados validado: filtragem demografica",
        es = "Etapa de datos validada: filtrado demografico"
      )

      cli::cli_alert_success(msg_stage_ok[[lang]] %||% msg_stage_ok[["en"]])
    }

    # Update metadata
    df <- climasus_meta(df, stage = "filter_demo", type  = "filter_demo")
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
        "  4. Filter demographics: df <- sus_filter_demographics(df, ...)\n\n",
        "If using external data, run sus_data_standardize() first to prepare it."
      ),
      pt = paste0(
        "Entrada nao e um objeto climasus_df.\n",
        "Esta funcao requer dados do pipeline CLIMASUS4r.\n\n",
        "Por favor, prepare seus dados primeiro:\n",
        "  1. Importar: df <- sus_data_import(...) ou sus_data_read(...)\n",
        "  2. Limpar: df <- sus_data_clean_encoding(df)\n",
        "  3. Padronizar: df <- sus_data_standardize(df)\n",
        "  4. Filtrar demografia: df <- sus_filter_demographics(df, ...)\n\n",
        "Se usar dados externos, execute sus_data_standardize() primeiro para prepara-los."
      ),
      es = paste0(
        "La entrada no es un objeto climasus_df.\n",
        "Esta funcion requiere datos del pipeline CLIMASUS4r.\n\n",
        "Por favor, prepare sus datos primero:\n",
        "  1. Importar: df <- sus_data_import(...) o sus_data_read(...)\n",
        "  2. Limpiar: df <- sus_data_clean_encoding(df)\n",
        "  3. Estandarizar: df <- sus_data_standardize(df)\n",
        "  4. Filtrar demografia: df <- sus_filter_demographics(df, ...)\n\n",
        "Si usa datos externos, ejecute sus_data_standardize() primero para prepararlos."
      )
    )
    
    cli::cli_abort(msg_error[[lang]])
  }
  
  # Store original row count
  df <- data.table::as.data.table(df)

  n_original <- nrow(df)
  
  # Track filters applied
  filters_applied <- character()
  
  # ========================================================================
  # FILTER BY SEX
  # ========================================================================
  
  if (!is.null(sex)) {
    sex_col <- find_column(df, c("sex", "sexo", "SEXO"))
    sex_col <- tolower(sex_col)
    if (is.null(sex_col)) {
      cli::cli_alert_warning("Sex column not found. Skipping sex filter.")
    } else {
      # Normalize sex values (case-insensitive matching)
      sex_targets <- tools::toTitleCase(sex)
      df <- df[get(sex_col) %in% sex_targets]
      filters_applied <- c(filters_applied, paste0("sex: ", paste(sex_targets, collapse = ", ")))
    }
  }
  
  # ========================================================================
  # FILTER BY RACE
  # ========================================================================
  
  if (!is.null(race)) {
    race_col <- find_column(df, c("race", "raca", "raza", "RACACOR", "RACA_COR"))
    race_col <- tolower(race_col)
    if (is.null(race_col)) {
      cli::cli_alert_warning("Race column not found. Skipping race filter.")
    } else {
      race_targets <- tools::toTitleCase(race)
      df <- df[get(race_col) %in% race_targets]
      filters_applied <- c(filters_applied, paste0("race: ", paste(race_targets, collapse = ", ")))
    }
  }
  
  # ========================================================================
  # FILTER BY AGE RANGE
  # ========================================================================
  
  if (!is.null(age_range)) {
    if (length(age_range) != 2) {
      cli::cli_abort("age_range must be a numeric vector of length 2: c(min_age, max_age)")
    }
    
    age_col <- find_column(df, c("age_years"))
    
    if (is.null(age_col)) {
      cli::cli_alert_warning("Age column not found. Skipping age filter or use sus_create_variables to create age_years e age_group columns.")
    } else {
      min_age <- age_range[1]
      max_age <- age_range[2]
      
      df <- df[data.table::between(get(age_col)), min_age, max_age]
      filters_applied <- c(filters_applied, paste0("age: ", min_age, "-", ifelse(is.infinite(max_age), "+", max_age)))
    }
  }
  
  # ========================================================================
  # FILTER BY EDUCATION
  # ========================================================================
  
  if (!is.null(education)) {
    edu_col <- find_column(df, c("education", "escolaridade", "escolaridad", 
                                  "ESC", "ESC2010"))
    edu_col <- tolower(edu_col)
    if (is.null(edu_col)) {
      cli::cli_alert_warning("Education column not found. Skipping education filter.")
    } else {
      education_targets <- tools::toTitleCase(education)
      df <- df[get(edu_col) %in% education_targets]
      filters_applied <- c(filters_applied, paste0("education: ", paste(education_targets, collapse = ", ")))
    }
  }
    
  # ========================================================================
  # FILTER BY MARITAL STATUS
  # ========================================================================
  
  if (!is.null(marital_status)) {
    marital_col <- find_column(df, c("marital_status", "estado_civil", 
                                      "estado_civil", "ESTCIV"))
    marital_col <- tolower(marital_col)
    if (is.null(marital_col)) {
      warning("Marital status column not found. Skipping marital status filter.")
    } else {
      marital_targets <- tools::toTitleCase(marital_status)
      df <- df[get(marital_col) %in% marital_targets]
      filters_applied <- c(filters_applied, paste0("marital_status: ", paste(marital_targets, collapse = ", ")))
    }
  }
  
  # ========================================================================
  # FILTER BY REGION OR STATES
  # ========================================================================
  if (!is.null(region)) {
   df_ufs_brasil <- data.table::data.table(
    sigla  = c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA", "MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF"),
    codigo = c(11, 12, 13, 14, 15, 16, 17, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31, 32, 33, 35, 41, 42, 43, 50, 51, 52, 53),
    estado_pt = c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins", "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia", "Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo", "Paraná", "Santa Catarina", "Rio Grande do Sul", "Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal"),
    
    # english
    state_en = c("Rondonia", "Acre", "Amazonas", "Roraima", "Para", "Amapa", "Tocantins", "Maranhao", "Piaui", "Ceara", "Rio Grande do Norte", "Paraiba", "Pernambuco", "Alagoas", "Sergipe", "Bahia", "Minas Gerais", "Espirito Santo", "Rio de Janeiro", "Sao Paulo", "Parana", "Santa Catarina", "Rio Grande do Sul", "Mato Grosso do Sul", "Mato Grosso", "Goias", "Federal District"),
    
    # Espanhol
    estado_es = c("Rondonia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins", "Maranhão", "Piauí", "Ceará", "Rio Grande del Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahía", "Minas Gerais", "Espírito Santo", "Río de Janeiro", "São Paulo", "Paraná", "Santa Catarina", "Rio Grande del Sur", "Mato Grosso del Sur", "Mato Grosso", "Goiás", "Distrito Federal")
  )
  data.table::setDT(df)
  translate_input <- function(x) {
    if (is.numeric(x) || !is.na(suppressWarnings(as.numeric(x)))) {
      return(df_ufs_brasil[codigo == as.numeric(x), sigla])
    }
    
    reg_clean <- tolower(x)
    target_key <- if (reg_clean %in% names(.region_aliases)) .region_aliases[[reg_clean]] else reg_clean
    
    if (target_key %in% names(.br_regions)) {
      return(.br_regions[[target_key]]) 
    } else {
      return(toupper(x)) 
    }
  }
  
  all_target_siglas <- unique(unlist(lapply(region, translate_input)))
  
  target_codes <- df_ufs_brasil[sigla %in% all_target_siglas, codigo]
  
  if (length(target_codes) > 0) {
    uf_col <- find_column(df, c("manager_uf", "UF_ZI", "uf_gestor", "notification_uf"))
    df <- df[as.numeric(get(uf_col)) %in% target_codes]
    col_name <- base::switch(lang,
                             "pt" = "estado_pt",
                             "en" = "state_en",
                             "es" = "estado_es",
                             "estado_pt")
    
    names_log <- df_ufs_brasil[codigo %in% target_codes, get(col_name)]
    filters_applied <- c(filters_applied, paste0("States: ", paste(names_log, collapse = ", ")))
    
    cli::cli_alert_success("Filtered by {length(target_codes)} states based on: {paste(region, collapse = ', ')}")
  } else {
    cli::cli_abort("No valid states found for: {.val {region}}")
  }
}


  # ========================================================================
  # FILTER BY MUNICIPALITY
  # ========================================================================
  if (!is.null(municipality_code)) {
    muni_col <- find_column(df, c("residence_municipality_code", 
                                  "municipality_code", 
                                  "residence_municipality",
                                   "municipio_residencia",
                                   "codigo_municipio",
                                   "codigo_municipio_residencia",
                                   "CODMUNRES"))
    
    if (is.null(muni_col)) {
      cli::cli_alert_warning("Municipality column not found. Skipping municipality filter.")
    } else {
      # Convert to character for comparison
      municipality_code <- as.character(municipality_code)
      df[[muni_col]] <- as.character(df[[muni_col]])
      
      df <- df[get(muni_col) %in% municipality_code]
      filters_applied <- c(filters_applied, 
                          paste0("municipality: ", length(municipality_code), " codes"))
    }
  }
  
  # ========================================================================
  # SUMMARY MESSAGE
  # ========================================================================
  
  n_filtered <- nrow(df)
  n_removed <- n_original - n_filtered
  pct_retained <- round(100 * n_filtered / n_original, 2)
  
  if (verbose) {
    if (length(filters_applied) == 0) {
      msg <- switch(lang,
        "en" = "No demographic filters applied",
        "pt" = "Nenhum filtro demografico aplicado",
        "es" = "Ningun filtro demografico aplicado"
      )
      cli::cli_alert_warning(msg)
    } else {
      # Print filters applied
      filter_msg <- switch(lang,
        "en" = "Applied demographic filters:",
        "pt" = "Filtros demograficos aplicados:",
        "es" = "Filtros demograficos aplicados:"
      )
      cli::cli_alert_info(filter_msg)
      
      for (filter in filters_applied) {
        cli::cli_li(filter)
      }
      
      # Print summary
      summary_msg <- switch(lang,
        "en" = paste0("Retained ", format(n_filtered, big.mark = ","), 
                     " of ", format(n_original, big.mark = ","), 
                     " rows (", pct_retained, "%)"),
        "pt" = paste0("Retidos ", format(n_filtered, big.mark = ","),
                     " de ", format(n_original, big.mark = ","),
                     " registros (", pct_retained, "%)"),
        "es" = paste0("Retenidos ", format(n_filtered, big.mark = ","),
                     " de ", format(n_original, big.mark = ","),
                     " registros (", pct_retained, "%)")
      )
      cli::cli_alert_success(summary_msg)
      
      removed_msg <- switch(lang,
        "en" = paste0("Removed ", format(n_removed, big.mark = ","), " rows"),
        "pt" = paste0("Removidos ", format(n_removed, big.mark = ","), " registros"),
        "es" = paste0("Eliminados ", format(n_removed, big.mark = ","), " registros")
      )
      cli::cli_alert_info(removed_msg)
    }
  }
  # ============================================================================
  # Update climasus_df metadata (DETAILED VERSION)
  # ============================================================================

  # Update stage and type
   if (!inherits(df, "climasus_df")) {
    # Create new climasus_df
    meta <- list(
      system = system,
      stage = "filter_demo",
      type = "filter_demo",
      spatial = inherits(df, "sf"),
      temporal = NULL,
      created = Sys.time(),
      modified = Sys.time(),
      history = sprintf(
        "[%s] Standardized column names and types",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ),
      user = list()
    )

    base_classes <- setdiff(class(df), "climasus_df")
    df <- structure(
      df,
      climasus_meta = meta,
      class = c("climasus_df", base_classes)
    )
  } else { 
    df <- climasus_meta(
    df,
    system = climasus_meta(df, "system"),  # Preserve original system
    stage = "filter_demo",
    type = "filter_demo"
  )     
  }


  # Build detailed processing history message
  filter_details <- c()

  if (!is.null(sex)) {
     filter_details <- c( filter_details, sprintf("Sex: %s", paste(sex, collapse = ", "))
  )
  }

  if (!is.null(race)) {
    filter_details <- c(filter_details, sprintf("Race: %s", paste(race, collapse=", ")))
  }

  if (!is.null(age_range)) {

  if (length(age_range) == 1) {

    filter_details <- c(
      filter_details,
      sprintf("Age: %g years", age_range[1])
    )

  } else if (length(age_range) == 2) {

    min_age <- age_range[1]
    max_age <- age_range[2]

    if (is.infinite(max_age)) {
      filter_details <- c(
        filter_details,
        sprintf("Age: >= %g years", min_age)
      )
    } else {
      filter_details <- c(
        filter_details,
        sprintf("Age: %g-%g years", min_age, max_age)
      )
    }

  } else {

    filter_details <- c(filter_details, "Age: selected age groups")

  }
}

  if (!is.null(education)) {
    filter_details <- c(filter_details, sprintf("Education: %s", paste(education, collapse=", ")))
  }

  if (!is.null(marital_status)) {
    marital_names <- sapply(marital_status, function(m) marital_status[as.character(m)] %||% m)
    filter_details <- c(filter_details, sprintf("Marital: %s", paste(marital_names, collapse=", ")))
  }

  if (!is.null(municipality_code)) {
    if (length(municipality_code) == 1) {
      filter_details <- c(filter_details, sprintf("Municipality: %s", municipality_code))
    } else if (length(municipality_code) <= 3) {
      filter_details <- c(filter_details, sprintf("Municipalities: %s", paste(municipality_code, collapse=", ")))
    } else {
      filter_details <- c(filter_details, sprintf("Municipalities: %s and %d more", 
                                                  paste(utils::head(municipality_code, 3), collapse=", "),
                                                  length(municipality_code) - 3))
    }
  }

  # Create history message
  if (length(filter_details) > 0) {
    history_msg <- sprintf("Filtered by demographics [%s]", paste(filter_details, collapse=" | "))
  } else {
    history_msg <- "Filtered by demographics [no filters applied]"
  }

  df <- climasus_meta(df, add_history = history_msg)

  return(df)
}


# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Find column by patterns (same as in other functions)
find_column <- function(df, patterns) {
  for (pattern in patterns) {
    if (pattern %in% names(df)) {
      return(pattern)
    }
  }
  return(NULL)
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