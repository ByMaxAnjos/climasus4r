#' Create Derived Variables for Epidemiological Analysis
#'
#' Creates commonly used derived variables from health data across ALL SUS systems
#' (SIM, SINAN, SIH, SIA, SINASC, CNES), including age groups, calendar variables,
#' and other epidemiologically relevant categorizations. Features age
#' calculation that handles different data formats across systems.
#'
#' @param df A data frame containing health data from any SUS system.
#' @param create_age_groups Logical. If `TRUE`, creates age group categories.
#'   Default is `FALSE`.
#' @param age_col Character string with the name of the age column (in years).
#'   If `NULL` (default), auto-detects and calculates age using hierarchical logic:
#'   1. Direct age column (if exists)
#'   2. Calculate from dates (event_date - birth_date)
#'   3. Decode DATASUS age codes (NU_IDADE_N, IDADE)
#' @param age_breaks Numeric vector specifying the breakpoints for age groups.
#'   Default is `c(0, 5, 15, 65, Inf)` for standard epidemiological categories.
#' @param age_labels Character vector with labels for age groups. If `NULL`
#'   (default), generates labels automatically from breaks.
#' @param create_calendar_vars Logical. If `TRUE`, creates calendar variables
#'   (day of week, month, season, etc.). Default is `FALSE`.
#' @param date_col Character string with the name of the date column. If `NULL`
#'   (default), auto-detects the date column.
#' @param create_climate_vars Logical. Se `TRUE`, cria variaveis climaticas e sazonais.
#' @param climate_region Character. Regiao climatica para calculos sazonais.
#'   Opcoes: "norte", "nordeste", "centro-oeste", "sudeste", "sul".
#' @param hemisphere Character string specifying the hemisphere for season
#'   calculation. Options: `"south"` (default, for Brazil), `"north"`.
#' @param lang Character string specifying the language for variable labels and
#'   messages. Options: `"en"` (English), `"pt"` (Portuguese, default), `"es"` (Spanish).
#' @param verbose Logical. If `TRUE` (default), prints progress messages.
#'
#' @return The input data frame with additional columns for the created variables.
#'
#' @details
#' **Age Calculation** (Hierarchical Logic):
#' 
#' The function uses a 3-tier hierarchy to ensure age is calculated correctly
#' across all SUS systems:
#' 
#' 1. **Direct Age Column** (Fastest): If a column with age in years already exists
#'    (common in SIM after microdatasus processing), uses it directly.
#'    
#' 2. **Date Calculation** (Gold Standard): If birth date and event date are available,
#'    calculates exact age as: `interval(birth_date, event_date) / years(1)`.
#'    This is the most accurate method and works for:
#'    - SINAN: Has `DTNASC` and `DT_NOTIFIC`
#'    - SIH: Has `NASC` and `DT_INTER`
#'    - SINASC: Has `DTNASC` (mother) and `DTNASC` (newborn)
#'    
#' 3. **DATASUS Code Decoder** (Fallback): If dates are missing (common in anonymized
#'    data), decodes the composite age code used by DATASUS:
#'    - Codes starting with `1`: Hours (converted to 0 years)
#'    - Codes starting with `2`: Days (converted to 0 years)
#'    - Codes starting with `3`: Months (converted to 0 years for <12 months)
#'    - Codes starting with `4`: Years (e.g., 4035 = 35 years)
#'    - Codes starting with `5`: 100+ years (e.g., 5105 = 105 years)
#'
#' **Age Groups**: Creates a factor variable `age_group` based on the specified
#' breaks and labels. Common epidemiological categories:
#' \itemize{
#'   \item Pediatric: 0-4, 5-14, 15-19
#'   \item Adult: 20-39, 40-59, 60+
#'   \item Elderly: 65-74, 75-84, 85+
#'   \item Climate-Health: 0-4, 5-64, 65+ (vulnerable populations)
#' }
#'
#' **Calendar Variables** (when `create_calendar_vars = TRUE`):
#' \itemize{
#'   \item `day_of_week`: Day of the week (1 = Monday, 7 = Sunday)
#'   \item `day_of_week_name`: Day name (e.g., "Monday", "Segunda-feira")
#'   \item `month`: Month number (1-12)
#'   \item `month_name`: Month name (e.g., "January", "Janeiro")
#'   \item `year`: Year
#'   \item `quarter`: Quarter (1-4)
#'   \item `season`: Season (Summer, Autumn, Winter, Spring)
#'   \item `is_weekend`: Logical indicating if date is weekend
#'   \item `day_of_year`: Day of year (1-365/366)
#'   \item `semester`: Semester (1 or 2)
#' }
#'
#' **Seasons** are calculated based on hemisphere:
#' \itemize{
#'   \item **Southern Hemisphere** (Brazil): Summer (Dec-Feb), Autumn (Mar-May),
#'     Winter (Jun-Aug), Spring (Sep-Nov)
#'   \item **Northern Hemisphere**: Summer (Jun-Aug), Autumn (Sep-Nov),
#'     Winter (Dec-Feb), Spring (Mar-May)
#' }
#'
#' @examples
#' \dontrun{
#' library(climasus4r)
#'
#' # ===== EXAMPLE 1: SIM (Mortality) - Age already calculated =====
#' df_sim <- sus_data_import(uf = "SP", year = 2023, system = "SIM-DO") |>
#'   sus_data_standardize(lang = "en") |>
#'   sus_create_variables(
#'     create_age_groups = TRUE,
#'     age_breaks = c(0, 5, 65, Inf),
#'     age_labels = c("0-4", "5-64", "65+"),
#'     create_calendar_vars = TRUE,
#'     lang = "en"
#'   )
#' # Uses direct age column (fastest)
#'
#' # ===== EXAMPLE 2: SINAN (Dengue) - Calculate from dates =====
#' df_sinan <- sus_data_import(uf = "RJ", year = 2023, system = "SINAN-DENGUE") |>
#'   sus_data_standardize(lang = "pt") |>
#'   sus_create_variables(
#'     create_age_groups = TRUE,
#'     age_breaks = c(0, 15, 60, Inf),
#'     create_calendar_vars = TRUE,
#'     lang = "pt"
#'   )
#' # Calculates age from DTNASC and DT_NOTIFIC (gold standard)
#'
#' # ===== EXAMPLE 3: SIH (Hospitalizations) - Decode age codes =====
#' df_sih <- sus_data_import(uf = "MG", year = 2023, system = "SIH-RD") |>
#'   sus_data_standardize(lang = "es") |>
#'   sus_create_variables(
#'     create_age_groups = TRUE,
#'     age_breaks = c(0, 18, 60, Inf),
#'     age_labels = c("0-17", "18-59", "60+"),
#'     create_calendar_vars = TRUE,
#'     lang = "es"
#'   )
#' # Decodes DATASUS age codes if dates are missing (fallback)
#'
#' # ===== EXAMPLE 4: Custom age groups for elderly analysis =====
#' df_elderly <- sus_create_variables(
#'   df,
#'   create_age_groups = TRUE,
#'   age_breaks = c(60, 70, 80, 90, Inf),
#'   age_labels = c("60-69", "70-79", "80-89", "90+"),
#'   lang = "pt"
#' )
#'
#' # ===== EXAMPLE 5: Calendar variables and climate variables =====
#' df_calendar_climate <- sus_create_variables(
#'   df,
#'   create_calendar_vars = TRUE,
#'   create_semester = TRUE,
#'   create_climate_vars = TRUE,
#'   climate_region = "Norte"
#' )
#' }
#'
#' @export
sus_create_variables <- function(df,
                                  create_age_groups = TRUE,
                                  age_breaks = c(0, 5, 15, 65, Inf),
                                  age_labels = NULL,
                                  create_calendar_vars = TRUE,
                                  create_climate_vars = TRUE,
                                  climate_region = NULL,
                                  date_col = NULL,
                                  age_col = NULL,
                                  hemisphere = "south",
                                  lang = "pt",
                                  verbose = TRUE) {
  
  # Validate inputs
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }
  
  if (nrow(df) == 0) {
     stop("df is empty (0 rows)")
  }
  
  if (!lang %in% c("en", "pt", "es")) {
     stop("lang must be one of: 'en', 'pt', 'es'")
  }
  
  if (!hemisphere %in% c("south", "north")) {
     stop("hemisphere must be 'south' or 'north'")
  }
  
  # Track which variables were created
  created_vars <- character(0)
  
  # ========================================================================
  # CREATE AGE GROUPS (WITH AGE CALCULATION)
  # ========================================================================
  
  if (create_age_groups) {
    
    # STEP 1: Determine age column
    age_col_to_use <- age_col
    
    # If user specified age_col, check if it exists
    if (!is.null(age_col)) {
      if (!age_col %in% names(df)) {
        if (verbose) {
          cli::cli_alert_warning(paste0("Age column '", age_col, "' not found in data frame. Will calculate age from dates or age codes."))
        }
        age_col_to_use <- NULL
      } else {
        if (verbose) {
          msg <- switch(lang,
            "en" = paste0("Using existing age column: ", age_col),
            "pt" = paste0("Usando coluna de idade existente: ", age_col),
            "es" = paste0("Usando columna de edad existente: ", age_col)
          )
          cli::cli_alert_info(msg)
        }
      }
    }
    
    # If no valid age column, try to calculate
    if (is.null(age_col_to_use)) {
      # Try to calculate from dates first (gold standard)
      age_from_dates <- try_calculate_age_from_dates(df, lang, verbose)
      
      if (!is.null(age_from_dates)) {
        df$age_years <- age_from_dates
        age_col_to_use <- "age_years"
        if (verbose) {
          msg <- switch(lang,
            "en" = "Calculated age from birth date and event date (gold standard)",
            "pt" = "Idade calculada a partir da data de nascimento e data do evento (padrao ouro)",
            "es" = "Edad calculada a partir de la fecha de nacimiento y fecha del evento (estandar de oro)"
          )
          cli::cli_alert_success(msg)
        }
      } else {
        # Fallback to decoding DATASUS age codes
        age_from_code <- try_decode_datasus_age(df, lang, verbose)
        
        if (!is.null(age_from_code)) {
          df$age_years <- age_from_code
          age_col_to_use <- "age_years"
          if (verbose) {
            msg <- switch(lang,
              "en" = "Decoded age from DATASUS age code (fallback method)",
              "pt" = "Idade decodificada do codigo de idade DATASUS (metodo alternativo)",
              "es" = "Edad decodificada del codigo de edad DATASUS (metodo alternativo)"
            )
            cli::cli_alert_warning(msg)
          }
        } else {
          stop("Could not determine age. Please ensure data has one of:\n  1. An age column\n  2. Birth date and event date columns\n  3. DATASUS age code column")
        }
      }
    }
    
    # Generate labels if not provided
    if (is.null(age_labels)) {
      age_labels <- generate_age_labels(age_breaks, lang)
    }
    
    # Validate breaks and labels
    if (length(age_labels) != (length(age_breaks) - 1)) {
      stop("Length of age_labels must be one less than length of age_breaks")
    }
    
    # Create age groups
    if (verbose) {
      msg <- switch(lang,
        "en" = "Creating age groups...",
        "pt" = "Criando faixas etarias...",
        "es" = "Creando grupos de edad..."
      )
      cli::cli_alert_info(msg)
    }
    
    # Ensure age column is numeric
    df[[age_col_to_use]] <- as.numeric(df[[age_col_to_use]])

    df$age_group <- cut(
      df[[age_col_to_use]],
      breaks = age_breaks,
      labels = age_labels,
      right = FALSE,
      include.lowest = TRUE
    )  
    
    created_vars <- c(created_vars, "age_group")
    
    # Report age distribution
    if (verbose) {
      age_summary <- table(df$age_group, useNA = "ifany")
      msg <- switch(lang,
        "en" = paste0("Age distribution: ", 
                      paste(names(age_summary), "=", age_summary, collapse = ", ")),
        "pt" = paste0("Distribuicao etaria: ",
                      paste(names(age_summary), "=", age_summary, collapse = ", ")),
        "es" = paste0("Distribucion etaria: ",
                      paste(names(age_summary), "=", age_summary, collapse = ", "))
      )
      cli::cli_alert_info(msg)
    }

    risk_labels <- switch(
    lang,
    "en" = c("High Risk (0-4)", "Standard Risk (5-64)", "High Risk (65+)"),
    "pt" = c("Alto Risco (0-4)", "Risco Padrao (5-64)", "Alto Risco (65+)"),
    "es" = c("Alto Riesgo (0-4)", "Riesgo Estandar (5-64)", "Alto Riesgo (65+)")
    )

    risk_varname <- switch(
      lang,
      "en" = "climate_risk_group",
      "pt" = "grupo_risco_climatico",
      "es" = "grupo_riesgo_climatico"
    )

    df[[risk_varname]] <- cut(
      df$age_years,
      breaks = c(-1, 4, 64, Inf),
      labels = risk_labels,
      right = TRUE,
      include.lowest = TRUE
    )

    created_vars <- c(created_vars, risk_varname)  
    }
  
  # ========================================================================
  # CREATE CALENDAR VARIABLES
  # ========================================================================
  
  if (create_calendar_vars) {
    
    # Auto-detect date column if not specified
    if (is.null(date_col)) {
      date_col <- detect_date_column(df)
      if (verbose) {
        msg <- switch(lang,
          "en" = paste0("Auto-detected date column: ", date_col),
          "pt" = paste0("Coluna de data auto-detectada: ", date_col),
          "es" = paste0("Columna de fecha auto-detectada: ", date_col)
        )
        cli::cli_alert_info(msg)
      }
    }
    
    # Validate date column exists
    if (!date_col %in% names(df)) {
      stop(paste0("Date column '", date_col, "' not found in data frame"))
    }
    
    # Convert to Date if not already
    if (!inherits(df[[date_col]], "Date")) {
      df[[date_col]] <- as.Date(df[[date_col]])
    }
    
    if (verbose) {
      msg <- switch(lang,
        "en" = "Creating calendar variables...",
        "pt" = "Criando variaveis de calendario...",
        "es" = "Creando variables de calendario..."
      )
      cli::cli_alert_info(msg)
    }
    
    if (lang == "en") { 
      # Extract date components
    df$year <- lubridate::year(df[[date_col]])
    df$month <- lubridate::month(df[[date_col]])
    df$day_of_week <- lubridate::wday(df[[date_col]], week_start = 1)  # Monday = 1
    df$day_of_year <- lubridate::yday(df[[date_col]])
    df$quarter <- lubridate::quarter(df[[date_col]])
    df$epidemiological_week <- lubridate::epiweek(df[[date_col]])
    
    # Create month names
    df$month_name <- get_month_names(df$month, lang)
    
    # Create day of week names
    df$day_of_week_name <- get_day_names(df$day_of_week, lang)
    
    # Create weekend indicator
    df$is_weekend <- df$day_of_week %in% c(6, 7)
      
    # Create semester if requested
    df$semester <- ifelse(df$month <= 6, 1, 2)
    
     created_vars <- c(
      created_vars,
      "year", "month", "month_name",
      "day_of_week", "day_of_week_name",
      "day_of_year", "quarter",
      "epidemiological_week",
      "is_weekend", "semester"
    )

    } else if (lang  == "pt") {
      # Extrair componentes da data
      df$ano <- lubridate::year(df[[date_col]])
      df$mes <- lubridate::month(df[[date_col]])
      df$dia_semana <- lubridate::wday(df[[date_col]], week_start = 1)
      df$dia_ano <- lubridate::yday(df[[date_col]])
      df$trimestre <- lubridate::quarter(df[[date_col]])
      df$semana_epidemiologica <- lubridate::epiweek(df[[date_col]])

      # Criar nomes
      df$nome_mes <- get_month_names(df$mes, lang)
      df$nome_dia_semana <- get_day_names(df$dia_semana, lang)

      # Indicador de fim de semana
      df$fim_semana <- df$dia_semana %in% c(6, 7)

      # Semestre
      df$semestre <- ifelse(df$mes <= 6, 1, 2)

      created_vars <- c(
        created_vars,
        "ano", "mes", "nome_mes",
        "dia_semana", "nome_dia_semana",
        "dia_ano", "trimestre",
        "semana_epidemiologica",
        "fim_semana", "semestre"
      )

    } else {
      # Extraer componentes de la fecha
      df$anio <- lubridate::year(df[[date_col]])
      df$mes <- lubridate::month(df[[date_col]])
      df$dia_semana <- lubridate::wday(df[[date_col]], week_start = 1)
      df$dia_anio <- lubridate::yday(df[[date_col]])
      df$trimestre <- lubridate::quarter(df[[date_col]])
      df$semana_epidemiologica <- lubridate::epiweek(df[[date_col]])

      # Crear nombres
      df$nombre_mes <- get_month_names(df$mes, lang)
      df$nombre_dia_semana <- get_day_names(df$dia_semana, lang)

      # Indicador de fin de semana
      df$fin_semana <- df$dia_semana %in% c(6, 7)

      # Semestre
      df$semestre <- ifelse(df$mes <= 6, 1, 2)

      created_vars <- c(
        created_vars,
        "anio", "mes", "nombre_mes",
        "dia_semana", "nombre_dia_semana",
        "dia_anio", "trimestre",
        "semana_epidemiologica",
        "fin_semana", "semestre"
      )
     }
    
  }

  # ========================================================================
  # SAZONALITY CLIMATE VARIABLES
  # ========================================================================
   if (create_climate_vars) {
    if (!create_calendar_vars) {
      cli::cli_abort(
        "{.arg create_calendar_vars} is required but was set to {.val FALSE}.",
        bullet = "info",
        bullet_vct = c(
          ">" = "Set {.code create_calendar_vars = TRUE} to enable calendar features."
        )
      )
    }
    month_var <- if (lang == "en") "month" else "mes"

    if (is.null(climate_region)) {

      cli::cli_alert_info(
        switch(
          lang,
          "en" = "Climate region not provided. Astronomical season was created.",
          "pt" = "Regiao climatica nao informada. Estacao astronomica criada.",
          "es" = "Region climatica no informada. Estacion astronomica creada."
        )
      )
      season_name <- switch(
        lang,
        "en" = "astronomical_season",
        "pt" = "estacao_astronomica",
        "es" = "estacion_astronomica"
      )
      df[[season_name]] <- get_season(df[[month_var]], hemisphere, lang)
      created_vars <- c(created_vars, season_name)
    } else {
      season_climate_name <- switch(
        lang,
        "en" = "climatic_season",
        "pt" = "estacao_climatica",
        "es" = "estacion_climatica"
      )
      df[[season_climate_name]] <- get_climate_season(df[[month_var]], hemisphere, lang)
      created_vars <- c(created_vars, season_climate_name)

      dry_rainny_name <- switch(
        lang,
        "en" = "dry_rainy_season",
        "pt" = "estacao_seca_chuvosa",
        "es" = "estacion_seca_lluviosa"
      )
      df[[dry_rainny_name]] <- get_dry_rainy_season(df[[month_var]], hemisphere, lang)
      created_vars <- c(created_vars, dry_rainny_name)
    
    }
   }
  
  # ========================================================================
  # SUMMARY MESSAGE
  # ========================================================================
  
  if (verbose && length(created_vars) > 0) {
    msg <- switch(lang,
      "en" = paste0("Created ", length(created_vars), " new variables: ",
                    paste(created_vars, collapse = ", ")),
      "pt" = paste0("Criadas ", length(created_vars), " novas variaveis: ",
                    paste(created_vars, collapse = ", ")),
      "es" = paste0("Creadas ", length(created_vars), " nuevas variables: ",
                    paste(created_vars, collapse = ", "))
    )
    cli::cli_alert_success(msg)
  }
  
  return(df)
}

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Try to Detect Existing Age Column
#' 
#' @param df Data frame
#' @return Name of age column or NULL if not found
#' @keywords internal
#' @noRd
try_detect_age_column <- function(df) {
  # Patterns for age in years (after standardization or processing)
  age_patterns <- c(
    "age_years",      # English standardized
    "age_code",            # Generic
    "codigo_idade",
    "codigo_edad",
    "IDADE", "idade", "age", "edad"
  )
  
  for (pattern in age_patterns) {
    if (pattern %in% names(df)) {
      # Validate it's numeric
      if (is.numeric(df[[pattern]])) {
        return(pattern)
      }
    }
  }
  
  return(NULL)
}


#' Try to Calculate Age from Dates
#' 
#' Calculates exact age in years from birth date and event date.
#' This is the GOLD STANDARD method.
#' 
#' @param df Data frame
#' @param lang Language for messages
#' @param verbose Whether to print messages
#' @return Vector of ages in years or NULL if dates not found
#' @keywords internal
#' @noRd
try_calculate_age_from_dates <- function(df, lang, verbose) {
  
  # Find birth date column
  birth_date_patterns <- c(
    "birth_date", "data_nascimento", "fecha_nacimiento",  # Standardized
    "DTNASC"                          # Original DATASUS
  )
  
  birth_col <- NULL
  for (pattern in birth_date_patterns) {
    if (pattern %in% names(df)) {
      birth_col <- pattern
      break
    }
  }
  
  if (is.null(birth_col)) {
    return(NULL)  # No birth date found
  }
  
  # Find event date column (notification, death, admission, etc.)
  event_date_patterns <- c(
    "death_date", "notification_date", "admission_date", "event_date",  # Standardized
    "data_obito", "data_notificacao", "data_internacao", "data_evento", # Portuguese
    "fecha_muerte", "fecha_notificacion", "fecha_evento",                # Spanish
    "DTOBITO", "DT_NOTIFIC", "DT_INTER", "DT_SAIDA", "DT_EVENT"         # Original DATASUS
  )
  
  event_col <- NULL
  for (pattern in event_date_patterns) {
    if (pattern %in% names(df)) {
      event_col <- pattern
      break
    }
  }
  
  if (is.null(event_col)) {
    return(NULL)  # No event date found
  }
  
  # Convert to Date if needed
  birth_date <- as.Date(df[[birth_col]])
  event_date <- as.Date(df[[event_col]])
  
  # Calculate age in years
  age_years <- as.numeric(lubridate::interval(birth_date, event_date) / lubridate::years(1))
  
  # Floor to integer (standard practice)
  age_years <- floor(age_years)
  
  # Handle negative ages (data quality issue)
  n_negative <- sum(age_years < 0, na.rm = TRUE)
  if (n_negative > 0 && verbose) {
    msg <- switch(lang,
      "en" = paste0("Warning: ", n_negative, " records with negative age (birth date after event date). Setting to NA."),
      "pt" = paste0("Aviso: ", n_negative, " registros com idade negativa (data de nascimento apos data do evento). Definindo como NA."),
      "es" = paste0("Advertencia: ", n_negative, " registros con edad negativa (fecha de nacimiento despues de fecha del evento). Estableciendo como NA.")
    )
    cli::cli_alert_warning(msg)
  }
  age_years[age_years < 0] <- NA
  
  # Handle ages > 120 (data quality issue)
  n_old <- sum(age_years > 120, na.rm = TRUE)
  if (n_old > 0 && verbose) {
    msg <- switch(lang,
      "en" = paste0("Warning: ", n_old, " records with age > 120 years. Setting to NA."),
      "pt" = paste0("Aviso: ", n_old, " registros com idade > 120 anos. Definindo como NA."),
      "es" = paste0("Advertencia: ", n_old, " registros con edad > 120 anos. Estableciendo como NA.")
    )
    cli::cli_alert_warning(msg)
  }
  age_years[age_years > 120] <- NA
  
  return(age_years)
}


#' Try to Decode DATASUS Age Codes
#' 
#' Decodes the composite age code used by DATASUS systems (SINAN, SIH).
#' 
#' DATASUS Age Code Format:
#' - First digit = Unit code (1=hours, 2=days, 3=months, 4=years, 5=100+ years)
#' - Remaining digits = Value
#' 
#' Examples:
#' - 1024 = 24 hours -> 0 years
#' - 2015 = 15 days -> 0 years
#' - 3011 = 11 months -> 0 years
#' - 4035 = 35 years -> 35 years
#' - 5105 = 105 years -> 105 years
#' 
#' @param df Data frame
#' @param lang Language for messages
#' @param verbose Whether to print messages
#' @return Vector of ages in years or NULL if code column not found
#' @keywords internal
#' @noRd
try_decode_datasus_age <- function(df, lang, verbose) {
  
  # Find age code column
  age_code_patterns <- c(
    "age_code",  # ADD THIS - matches your column name
    "codigo_idade", "codigo_edad",
    "NU_IDADE_N",  # SINAN standard
    "IDADE"        # SIH standard
  )
  
  age_code_col <- NULL
  for (pattern in age_code_patterns) {
    if (pattern %in% names(df)) {
      age_code_col <- pattern
      break
    }
  }
  
  if (is.null(age_code_col)) {
    return(NULL)  # No age code column found
  }
  
  # Get age codes
  age_codes <- df[[age_code_col]]
  
  # Ensure numeric
  if (!is.numeric(age_codes)) {
    age_codes <- as.numeric(age_codes)
  }
  
  # Decode using vectorized approach
  age_years <- decode_datasus_age_vectorized(age_codes)
  
  return(age_years)
}


#' Decode DATASUS Age Codes (Vectorized)
#' 
#' Vectorized implementation of DATASUS age code decoder for performance.
#' 
#' @param age_codes Numeric vector of DATASUS age codes
#' @return Numeric vector of ages in years
#' @keywords internal
#' @noRd
decode_datasus_age_vectorized <- function(age_codes) {
  
  # Initialize result vector
  age_years <- rep(NA_real_, length(age_codes))
  age_codes_char <- as.character(age_codes)
  unit_code <- as.numeric(substr(age_codes_char, 1, 1))
  value <- as.numeric(substr(age_codes_char, 2, nchar(age_codes_char)))
 # Handle cases where value extraction results in empty string
  value[is.na(value)] <- 0
  
  # Decode based on unit code
  # Unit 1: Hours -> 0 years
  age_years[unit_code == 1] <- 0
  
  # Unit 2: Days -> 0 years
  age_years[unit_code == 2] <- 0
  
  # Unit 3: Months -> years = floor(months / 12)
  age_years[unit_code == 3] <- floor(value[unit_code == 3] / 12)
  
  # Unit 4: Years -> value
  age_years[unit_code == 4] <- value[unit_code == 4]
  
  # Unit 5: 100+ years -> value + 100
  age_years[unit_code == 5] <- value[unit_code == 5]

  
  return(age_years)
}


#' Detect Date Column
#' 
#' Auto-detects the date column in a data frame based on common patterns.
#' 
#' @param df Data frame
#' @return Name of date column
#' @keywords internal
#' @noRd
detect_date_column <- function(df) {
  # Common date column patterns (in order of priority)
  date_patterns <- c(
    # English
    "death_date", "notification_date", "birth_date", "admission_date",
    "discharge_date", "date", "event_date",
    # Portuguese
    "data_obito", "data_notificacao", "data_nascimento", "data_internacao",
    "data_alta", "data_evento", "data",
    # Spanish
    "fecha_muerte", "fecha_notificacion", "fecha_nacimiento", "fecha",
    # Original DATASUS
    "DTOBITO", "DT_NOTIFIC", "DTNASC", "DT_INTER", "DT_SAIDA", "DT_EVENT"
  )
  
  # Find first matching column
  for (pattern in date_patterns) {
    if (pattern %in% names(df)) {
      return(pattern)
    }
  }
  
  # If no match, look for Date class columns
  date_cols <- names(df)[sapply(df, function(x) inherits(x, "Date") || inherits(x, "POSIXct"))]
  if (length(date_cols) > 0) {
    return(date_cols[1])
  }
  
  # If still no match, error
  stop("Could not auto-detect date column. Please specify 'date_col' parameter.")
}


#' Generate Age Labels
#' 
#' Generates age group labels from breaks.
#' 
#' @param breaks Numeric vector of age breaks
#' @param lang Language for labels
#' @return Character vector of labels
#' @keywords internal
#' @noRd
generate_age_labels <- function(breaks, lang) {
  n <- length(breaks) - 1
  labels <- character(n)
  
  for (i in 1:n) {
    lower <- breaks[i]
    upper <- breaks[i + 1]
    
    if (is.infinite(upper)) {
      labels[i] <- paste0(lower, "+")
    } else {
      labels[i] <- paste0(lower, "-", upper - 1)
    }
  }
  
  return(labels)
}


#' Get Month Names
#' 
#' Returns month names in the specified language.
#' 
#' @param month_num Numeric vector of month numbers (1-12)
#' @param lang Language code
#' @return Character vector of month names
#' @keywords internal
#' @noRd
get_month_names <- function(month_num, lang) {
  month_names <- switch(lang,
    "en" = c("January", "February", "March", "April", "May", "June",
             "July", "August", "September", "October", "November", "December"),
    "pt" = c("Janeiro", "Fevereiro", "Marco", "Abril", "Maio", "Junho",
             "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"),
    "es" = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
             "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  )
  
  return(month_names[month_num])
}


#' Get Day Names
#' 
#' Returns day of week names in the specified language.
#' 
#' @param day_num Numeric vector of day numbers (1-7, Monday=1)
#' @param lang Language code
#' @return Character vector of day names
#' @keywords internal
#' @noRd
get_day_names <- function(day_num, lang) {
  day_names <- switch(lang,
    "en" = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", 
             "Saturday", "Sunday"),
    "pt" = c("Segunda-feira", "Terca-feira", "Quarta-feira", "Quinta-feira",
             "Sexta-feira", "Sabado", "Domingo"),
    "es" = c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes",
             "Sabado", "Domingo")
  )
  
  return(day_names[day_num])
}


#' Get Season
#' 
#' Returns season based on month and hemisphere.
#' 
#' @param month_num Numeric vector of month numbers (1-12)
#' @param hemisphere Hemisphere ("south" or "north")
#' @param lang Language code
#' @return Character vector of season names
#' @keywords internal
#' @noRd
get_season <- function(month_num, hemisphere, lang) {
  
  if (hemisphere == "south") {
    # Southern Hemisphere (Brazil)
    season_num <- ifelse(month_num %in% c(12, 1, 2), 1,  # Summer
                  ifelse(month_num %in% c(3, 4, 5), 2,    # Autumn
                  ifelse(month_num %in% c(6, 7, 8), 3,    # Winter
                         4)))                              # Spring
  } else {
    # Northern Hemisphere
    season_num <- ifelse(month_num %in% c(6, 7, 8), 1,    # Summer
                  ifelse(month_num %in% c(9, 10, 11), 2,  # Autumn
                  ifelse(month_num %in% c(12, 1, 2), 3,   # Winter
                         4)))                              # Spring
  }
  
  season_names <- switch(lang,
    "en" = c("Summer", "Autumn", "Winter", "Spring"),
    "pt" = c("Verao", "Outono", "Inverno", "Primavera"),
    "es" = c("Verano", "Otono", "Invierno", "Primavera")
  )
  
  return(season_names[season_num])
}

#' Obtem a Estacao Climatica Brasileira
#' @keywords internal
#' @noRd
get_climate_season <- function(months, region, lang = "pt") {

  # Labels sem acentos
  labels <- switch(
    lang,
    "pt" = c("Chuvosa", "Transicao", "Seca"),
    "en" = c("Rainy", "Transition", "Dry"),
    "es" = c("Lluviosa", "Transicion", "Seca"),
    c("Rainy", "Transition", "Dry")
  )

  no_dry_label <- switch(
    lang,
    "pt" = "Sem estacao seca definida",
    "en" = "No defined dry season",
    "es" = "Sin estacion seca definida",
    "No defined dry season"
  )

  season <- switch(
    region,

    "norte" = ifelse(
      months %in% 1:5,
      labels[1],                       # Chuvosa
      ifelse(months %in% 6:11,
             labels[3],                # Seca
             labels[2])                # Transicao (dezembro)
    ),

    "nordeste" = ifelse(
      months %in% 2:7,
      labels[1],                       # Chuvosa
      ifelse(months %in% c(8,9,10,11,12,1),
             labels[3],                # Seca
             labels[2])
    ),

    "centro-oeste" = ifelse(
      months %in% c(10,11,12,1,2,3),
      labels[1],                       # Chuvosa
      labels[3]                        # Seca
    ),

    "sudeste" = ifelse(
      months %in% c(10,11,12,1,2,3),
      labels[1],                       # Chuvosa
      labels[3]                        # Seca
    ),

    "sul" = rep(no_dry_label, length(months)),

    rep(NA_character_, length(months))
  )

  return(season)
}


#' Obtem a Estacao Seca ou Chuvosa
#' @keywords internal
#' @noRd
get_dry_rainy_season <- function(months, region, lang = "pt") {

  season <- rep(NA_character_, length(months))

  labels <- switch(
    lang,
    "pt" = c("Chuvosa", "Seca"),
    "en" = c("Rainy", "Dry"),
    "es" = c("Lluviosa", "Seca"),
    c("Rainy", "Dry")
  )

  if (region == "norte") {

    season[months %in% 1:5]  <- labels[1]
    season[months %in% 6:12] <- labels[2]

  } else if (region == "nordeste") {

    season[months %in% 2:7]              <- labels[1]
    season[months %in% c(8,9,10,11,12,1)] <- labels[2]

  } else if (region %in% c("centro-oeste", "sudeste")) {

    season[months %in% c(10,11,12,1,2,3)] <- labels[1]
    season[months %in% 4:9]               <- labels[2]

  } else if (region == "sul") {

    season[] <- switch(
      lang,
      "pt" = "Sem estacao seca definida",
      "en" = "No defined dry season",
      "es" = "Sin estacion seca definida"
    )

  }

  return(season)
}

