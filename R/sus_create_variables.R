#' Create Derived Variables for Epidemiological Analysis
#'
#' Creates commonly used derived variables from health data across ALL SUS systems
#' (SIM, SINAN, SIH, SIA, SINASC, CNES), including age groups, calendar variables,
#' and other epidemiologically relevant categorizations. Features age
#' calculation that handles different data formats across systems.
#'
#' @param df A data frame containing health data from any SUS system.
#' @param create_age_groups Logical. If `TRUE`, derives age-based variables.
#' When enabled, the function will attempt to determine age using the following
#' hierarchy (in order of preference):
#' \enumerate{
#'   \item An existing age column supplied via \code{age_col}.
#'   \item Calculation from birth date and event date columns (gold standard).
#'   \item Decoding of DATASUS age code variables (fallback method).
#' }
#'
#' If age is successfully determined, the following variables may be created:
#'
#' \itemize{
#'   \item \strong{User-defined age groups}:
#'     A categorical variable based on \code{age_breaks} and \code{age_labels},
#'     created using \code{cut()}. This variable is always created when
#'     \code{create_age_groups = TRUE}.
#'
#'   \item \strong{Climate risk age group}:
#'     A coarse age classification designed for climate–health analyses,
#'     with three categories:
#'     \describe{
#'       \item{0–4}{High risk}
#'       \item{5–64}{Standard risk}
#'       \item{65+}{High risk}
#'     }
#'     The variable name depends on the selected language:
#'     \itemize{
#'       \item English: \code{climate_risk_group}
#'       \item Portuguese: \code{grupo_risco_climatico}
#'       \item Spanish: \code{grupo_riesgo_climatico}
#'     }
#'
#'   \item \strong{IBGE quinquennial age groups}:
#'     A standardized 17-group age classification following the Brazilian
#'     Institute of Geography and Statistics (IBGE) quinquennial structure:
#'     \code{0–4, 5–9, ..., 75–79, 80+}.
#'     This variable ensures national and international comparability and is
#'     particularly useful for demographic and epidemiological analyses.
#'     Variable names by language:
#'     \itemize{
#'       \item English: \code{ibge_age_group}
#'       \item Portuguese: \code{faixa_etaria_ibge}
#'       \item Spanish: \code{grupo_edad_ibge}
#'     }
#' }
#'
#' All age group variables are created only if age can be reliably determined.
#' If age cannot be inferred, the function will stop with an informative error.
#' @param age_breaks Numeric vector specifying the breakpoints for age groups.
#'   Default is `c(0, 5, 15, 65, Inf)` for standard epidemiological categories.
#' @param age_labels Character vector with labels for age groups. If `NULL`
#'   (default), generates labels automatically from breaks.
#' @param create_calendar_vars Logical. If `TRUE`, creates calendar variables
#'   (day of week, month, season, etc.). Default is `FALSE`.
#' @param age_col Character string with the name of the age column (in years).
#'   If `NULL` (default), auto-detects and calculates age using hierarchical logic:
#'   1. Direct age column (if exists)
#'   2. Calculate from dates (event_date - birth_date)
#'   3. Decode DATASUS age codes (NU_IDADE_N, IDADE)
#' @param date_col Character string with the name of the date column. If `NULL`
#'   (default), auto-detects the date column. 
#' @param create_climate_vars Logical. Se `TRUE`, cria variaveis climaticas e sazonais.
#' @param climate_region Character. Regiao climatica para calculos sazonais.
#'   Opcoes: "norte", "nordeste", "centro-oeste", "sudeste", "sul".
#' @param hemisphere Character string specifying the hemisphere for season
#'   calculation. Options: `"south"` (default, for Brazil), `"north"`.
#' @param backend Character string specifying the data processing backend.
#'   Use `"arrow"` for out-of-memory, lazy processing (recommended for large datasets),
#'   or `"tibble"` for in-memory processing (recommended for small to medium datasets).
#'
#'   - `"arrow"`: operations are performed lazily using the Apache Arrow engine,
#'     avoiding loading the full dataset into memory. Ideal for large files
#'     (e.g., Parquet, Feather) and high-performance workflows.
#'
#'   - `"tibble"`: data is fully loaded into memory as a tibble and processed eagerly
#'     using dplyr. Simpler and more predictable, but may be slow or fail for large datasets.
#'
#'   If not specified, the function may automatically choose the backend based on
#'   the input data type.
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
#'   sus_data_create_variables(
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
#'   sus_data_create_variables(
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
#'   sus_data_create_variables(
#'     create_age_groups = TRUE,
#'     age_breaks = c(0, 18, 60, Inf),
#'     age_labels = c("0-17", "18-59", "60+"),
#'     create_calendar_vars = TRUE,
#'     lang = "es"
#'   )
#' # Decodes DATASUS age codes if dates are missing (fallback)
#'
#' # ===== EXAMPLE 4: Custom age groups for elderly analysis =====
#' df_elderly <- sus_data_create_variables(
#'   df,
#'   create_age_groups = TRUE,
#'   age_breaks = c(60, 70, 80, 90, Inf),
#'   age_labels = c("60-69", "70-79", "80-89", "90+"),
#'   lang = "pt"
#' )
#'
#' # ===== EXAMPLE 5: Calendar variables and climate variables =====
#' df_calendar_climate <- sus_data_create_variables(
#'   df,
#'   create_calendar_vars = TRUE,
#'   create_semester = TRUE,
#'   create_climate_vars = TRUE,
#'   climate_region = "Norte"
#' )
#' }
#'
#' @export
sus_data_create_variables <- function(
  df,
  create_age_groups = TRUE,
  age_breaks = c(0, 5, 15, 60, Inf),
  age_labels = NULL,
  create_calendar_vars = TRUE,
  create_climate_vars = TRUE,
  climate_region = NULL,
  date_col = NULL,
  age_col = NULL,
  hemisphere = "south",
  backend = "arrow",
  lang = "pt",
  verbose = TRUE
) {
  validate_system_pipeline_support(df, "sus_data_create_variables", lang = lang)

  if (backend == "arrow") {
    result <- tryCatch({
      .variable_arrow_internal(
        df = df,
        create_age_groups = create_age_groups,
        age_breaks = age_breaks,
        age_labels = age_labels,
        create_calendar_vars = create_calendar_vars,
        create_climate_vars = create_climate_vars,
        climate_region = climate_region,
        date_col = date_col,
        age_col = age_col,
        hemisphere = hemisphere,
        lang = lang, 
        verbose = verbose  
      )
    }, error = function(e) {
      .warn_arrow_fallback(conditionMessage(e), lang = lang)
      NULL
    })
    
    if (!is.null(result)) {
      return(result)
    }
  }
  # Fallback: tibble
  .variable_tibble_internal(
       df = df,
        create_age_groups = create_age_groups,
        age_breaks = age_breaks,
        age_labels = age_labels,
        create_calendar_vars = create_calendar_vars,
        create_climate_vars = create_climate_vars,
        climate_region = climate_region,
        date_col = date_col,
        age_col = age_col,
        hemisphere = hemisphere,
        lang = lang, 
        verbose = verbose  
  )
}

# ===========================================================================
# INTERNAL FUNCTIONS
# ===========================================================================
.variable_tibble_internal <- function(
  df,
  create_age_groups,
  age_breaks,
  age_labels,
  create_calendar_vars,
  create_climate_vars,
  climate_region,
  date_col,
  age_col,
  hemisphere,
  lang,
  verbose
) {
  cli::cli_h1("climasus4r - Create Derived Variables")
  # Validate inputs
  if (!is.data.frame(df)) {
    df <- new_climasus_df(                                      
      dplyr::collect(df),                                                
      sus_meta(df)  # extrai metadata do Arrow                           
    ) 
    if (!is.data.frame(df)) {
      cli::cli_abort("Input {.arg df} must be a data.frame or a collectable dplyr object.")
    }
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

  # Check if data is climasus_df
  if (inherits(df, "climasus_df")) {
    # Minimum required stage
    required_stage <- "stand"
    current_stage <- sus_meta(df, "stage")

    if (!is_stage_at_least(current_stage, required_stage)) {
      msg_error <- list(
        en = paste0(
          "Data must be standardized before creating variables.\n",
          "Current stage: ",
          current_stage %||% "unknown",
          "\n",
          "Required stage: ",
          required_stage,
          "\n\n",
          "Please run:\n",
          "  df <- sus_data_standardize(df)"
        ),
        pt = paste0(
          "Dados devem ser padronizados antes de criar variaveis.\n",
          "Estagio atual: ",
          current_stage %||% "desconhecido",
          "\n",
          "Estagio requerido: ",
          required_stage,
          "\n\n",
          "Por favor, execute:\n",
          "  df <- sus_data_standardize(df)"
        ),
        es = paste0(
          "Los datos deben estar estandarizados antes de crear variables.\n",
          "Etapa actual: ",
          current_stage %||% "desconocida",
          "\n",
          "Etapa requerida: ",
          required_stage,
          "\n\n",
          "Por favor, ejecute:\n",
          "  df <- sus_data_standardize(df)"
        )
      )

      cli::cli_abort(msg_error[[lang]] %||% msg_error[["en"]])
    }

    # Stage validated
    if (verbose) {
      msg_stage_ok <- list(
        en = "Data stage validated: variables derivation",
        pt = "Estagio de dados validado: derivacao de variaveis",
        es = "Etapa de datos validada: derivacion de variables"
      )

      cli::cli_alert_success(msg_stage_ok[[lang]] %||% msg_stage_ok[["en"]])
    }

    # Update metadata
    df <- sus_meta(df, stage = "derive", type = "derive")
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
        "*" = "{.strong 4. Create:} {.code df <- sus_data_aggregate(...)}",
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
        "*" = "{.strong 4. Criar:} {.code df <- sus_data_aggregate(...)}",
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
        "*" = "{.strong 4. Crirar:} {.code df <- sus_data_aggregate(...)}",
        " " = "",
        "v" = "Consejo: Si usa datos externos, ejecute {.fn sus_data_standardize} primero."
      )
    )

    cli::cli_abort(msg_error[[lang]])
  }

  # Track which variables were created
  created_vars <- character(0)

  system <- sus_meta(df, "system")

  # ========================================================================
  # AGE COLUMN IDENTIFICATION AND CALCULATION
  # ========================================================================

  # STEP 1: Determine age column
  age_col_to_use <- age_col

  # If user specified age_col, check if it exists
  if (!is.null(age_col)) {
    if (!age_col %in% names(df)) {
      if (verbose) {
        cli::cli_alert_warning(paste0(
          "Age column '",
          age_col,
          "' not found in data frame. Will calculate age from dates or age codes."
        ))
      }
      age_col_to_use <- NULL
    } else {
      if (verbose) {
        msg <- switch(
          lang,
          "en" = paste0("Using existing age column: ", age_col),
          "pt" = paste0("Usando coluna de idade existente: ", age_col),
          "es" = paste0("Usando columna de edad existente: ", age_col)
        )
        cli::cli_alert_info(msg)
      }
    }
  }

  # If no valid age column, check for DIRECT age columns (not DATASUS codes)
  # Note: DATASUS age-code columns (codigo_idade, IDADE, etc.) need special
  # decoding via try_decode_datasus_age — they are NOT handled here.
  if (is.null(age_col_to_use)) {
    direct_age_patterns <- c(
      "age_years",          # already decoded age in years
      "idade_mae",          # SINASC maternal age (direct value, e.g. 28)
      "mother_age", "edad_madre"
    )
    for (.pat in direct_age_patterns) {
      if (.pat %in% names(df)) {
        col_vals <- df[[.pat]]
        is_numeric_compat <- is.numeric(col_vals) ||
          (is.character(col_vals) &&
             suppressWarnings(mean(is.na(as.numeric(col_vals)))) < 0.5)
        if (is_numeric_compat) {
          age_col_to_use <- .pat
          if (verbose) {
            age_msg <- .pat
            cli::cli_alert_info("Using existing age column: {age_msg}")
          }
          break
        }
      }
    }
  }

  # If still no age column, try to calculate
  if (is.null(age_col_to_use)) {
    # Try to calculate from dates first (gold standard)
    age_from_dates <- try_calculate_age_from_dates(df, lang, verbose)

    if (!is.null(age_from_dates)) {
      df$age_years <- age_from_dates
      age_col_to_use <- "age_years"
      if (verbose) {
        msg <- switch(
          lang,
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
          msg <- switch(
            lang,
            "en" = "Decoded age from DATASUS age code (fallback method)",
            "pt" = "Idade decodificada do codigo de idade DATASUS (metodo alternativo)",
            "es" = "Edad decodificada del codigo de edad DATASUS (metodo alternativo)"
          )
          cli::cli_alert_warning(msg)
        }
      } else {
        stop(
          "Could not determine age. Please ensure data has one of:\n  1. An age column\n  2. Birth date and event date columns\n  3. DATASUS age code column"
        )
      }
    }
  }

  # Ensure age column is numeric
  df[[age_col_to_use]] <- as.numeric(df[[age_col_to_use]])

  # Downstream code references df$age_years — ensure it exists
  if (age_col_to_use != "age_years") {
    df$age_years <- df[[age_col_to_use]]
  }

  # ========================================================================
  # CREATE AGE GROUPS  Climate risk group (fixed definition)
  # ========================================================================

  if (create_age_groups) {
    # Create age groups
    if (verbose) {
      msg <- switch(
        lang,
        "en" = "Creating age groups...",
        "pt" = "Criando faixas etarias...",
        "es" = "Creando grupos de edad..."
      )
      cli::cli_alert_info(msg)
    }

    df$age_group <- cut(
      df[[age_col_to_use]],
      breaks = age_breaks,
      labels = age_labels,
      right = FALSE,
      include.lowest = TRUE
    )

    created_vars <- c(created_vars, "age_group")

    # --------------------------------------------------
    # STEP 5: Climate risk group (fixed definition)
    # --------------------------------------------------
    # Report age distribution
    if (verbose) {
      age_summary <- table(df$age_group, useNA = "ifany")
      msg <- switch(
        lang,
        "en" = paste0(
          "Age distribution: ",
          paste(names(age_summary), "=", age_summary, collapse = ", ")
        ),
        "pt" = paste0(
          "Distribuicao etaria: ",
          paste(names(age_summary), "=", age_summary, collapse = ", ")
        ),
        "es" = paste0(
          "Distribucion etaria: ",
          paste(names(age_summary), "=", age_summary, collapse = ", ")
        )
      )
      cli::cli_alert_info(msg)
    }

    risk_labels <- switch(
      lang,
      "en" = c("High Risk (0-4)", "Standard Risk (5-64)", "High Risk (65+)"),
      "pt" = c("Alto Risco (0-4)", "Risco Padrao (5-64)", "Alto Risco (65+)"),
      "es" = c(
        "Alto Riesgo (0-4)",
        "Riesgo Estandar (5-64)",
        "Alto Riesgo (65+)"
      )
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

    # --------------------------------------------------
    # STEP : IBGE group (fixed definition)
    # --------------------------------------------------
    ibge_labels <- c(
      "0-4",
      "5-9",
      "10-14",
      "15-19",
      "20-24",
      "25-29",
      "30-34",
      "35-39",
      "40-44",
      "45-49",
      "50-54",
      "55-59",
      "60-64",
      "65-69",
      "70-74",
      "75-79",
      "80+"
    )
    ibge_varname <- switch(
      lang,
      "pt" = "faixa_etaria_ibge",
      "en" = "ibge_age_group",
      "es" = "grupo_edad_ibge"
    )
    df[[ibge_varname]] <- cut(
      df[[age_col_to_use]],
      breaks = c(
        0,
        5,
        10,
        15,
        20,
        25,
        30,
        35,
        40,
        45,
        50,
        55,
        60,
        65,
        70,
        75,
        80,
        Inf
      ),
      labels = ibge_labels,
      right = FALSE,
      include.lowest = TRUE
    )

    created_vars <- c(created_vars, ibge_varname)
  }

  # ========================================================================
  # CREATE AGE GROUPS with age_breaks from user
  # ========================================================================
  if (!is.null(age_breaks)) {
    if (!is.numeric(age_breaks) || length(age_breaks) < 2) {
      cli::cli_abort(
        c(
          "{.arg age_breaks} must be numeric or coercible to numeric.",
          i = "You can define custom age group structures.",
          ">" = "Example (standard 5-year age groups):",
          " " = "{.code c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf)}"
        )
      )
    }

    # Generate labels if not provided
    if (is.null(age_labels)) {
      age_labels <- generate_age_labels(age_breaks, lang)
    }

    # Validate breaks and labels
    if (length(age_labels) != (length(age_breaks) - 1)) {
      cli::cli_abort(
        "Length of age_labels must be one less than length of age_breaks"
      )
    }

    if (verbose) {
      cli::cli_alert_info(
        switch(
          lang,
          "en" = "Creating age groups...",
          "pt" = "Criando faixas etarias...",
          "es" = "Creando grupos de edad..."
        )
      )
    }

    df$age_group <- cut(
      df[[age_col_to_use]],
      breaks = age_breaks,
      labels = age_labels,
      right = FALSE,
      include.lowest = TRUE
    )

    created_vars <- c(created_vars, "age_group")
  }

  # ========================================================================
  # CREATE CALENDAR VARIABLES USER.
  # ========================================================================

  if (create_calendar_vars) {
    # Auto-detect date column if not specified
    if (is.null(date_col)) {
      date_col <- detect_date_column(df, system)
      if (verbose) {
        msg <- switch(
          lang,
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
      msg <- switch(
        lang,
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
      df$day_of_week <- lubridate::wday(df[[date_col]], week_start = 1) # Monday = 1
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
        "year",
        "month",
        "month_name",
        "day_of_week",
        "day_of_week_name",
        "day_of_year",
        "quarter",
        "epidemiological_week",
        "is_weekend",
        "semester"
      )
    } else if (lang == "pt") {
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
        "ano",
        "mes",
        "nome_mes",
        "dia_semana",
        "nome_dia_semana",
        "dia_ano",
        "trimestre",
        "semana_epidemiologica",
        "fim_semana",
        "semestre"
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
        "anio",
        "mes",
        "nombre_mes",
        "dia_semana",
        "nombre_dia_semana",
        "dia_anio",
        "trimestre",
        "semana_epidemiologica",
        "fin_semana",
        "semestre"
      )
    }
  }

  # ========================================================================
  # SAZONALITY CLIMATE VARIABLES
  # ========================================================================

  if (create_climate_vars && !create_calendar_vars) {
    cli::cli_abort(
      c(
        "{.arg create_calendar_vars} is required but was set to {.val FALSE}.",
        ">" = "First set {.code create_calendar_vars = TRUE} to enable climate features."
      ),
      call = NULL
    )
  }

  if (create_climate_vars) {
    month_var <- if (lang == "en") "month" else "mes"

    if (is.null(climate_region)) {
      cli::cli_alert_info(
        switch(
          lang,
          "en" = "{.arg climate_region} not provided. Astronomical season was created.",
          "pt" = "{.arg climate_region} nao informada. Estacao astronomica criada.",
          "es" = "{.arg climate_region}  no informada. Estacion astronomica creada."
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
      df[[season_climate_name]] <- get_climate_season(
        df[[month_var]],
        hemisphere,
        lang
      )
      created_vars <- c(created_vars, season_climate_name)

      dry_rainny_name <- switch(
        lang,
        "en" = "dry_rainy_season",
        "pt" = "estacao_seca_chuvosa",
        "es" = "estacion_seca_lluviosa"
      )
      df[[dry_rainny_name]] <- get_dry_rainy_season(
        df[[month_var]],
        hemisphere,
        lang
      )
      created_vars <- c(created_vars, dry_rainny_name)
    }
  }

  # ========================================================================
  # SUMMARY MESSAGE
  # ========================================================================

  if (verbose && length(created_vars) > 0) {
    msg <- switch(
      lang,
      "en" = paste0(
        "Created ",
        length(created_vars),
        " new variables: ",
        paste(created_vars, collapse = ", ")
      ),
      "pt" = paste0(
        "Criadas ",
        length(created_vars),
        " novas variaveis: ",
        paste(created_vars, collapse = ", ")
      ),
      "es" = paste0(
        "Creadas ",
        length(created_vars),
        " nuevas variables: ",
        paste(created_vars, collapse = ", ")
      )
    )
    cli::cli_alert_success(msg)
  }

  # Update stage and type
  if (!inherits(df, "climasus_df")) {
    # Create new climasus_df
    meta <- list(
      system = system,
      stage = "derive",
      type = "derive",
      spatial = inherits(df, "sf"),
      temporal = NULL,
      created = Sys.time(),
      modified = Sys.time(),
      history = sprintf(
        "[%s] Create variables",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ),
      user = list()
    )

    base_classes <- setdiff(class(df), "climasus_df")
    df <- structure(
      df,
      sus_meta = meta,
      class = c("climasus_df", base_classes)
    )
  } else {
    df <- sus_meta(
      df,
      system = sus_meta(df, "system"), # Preserve original system
      stage = "derive",
      type = "derive"
    )
  }

  # Build detailed processing history message
  var_details <- c()

  # Age groups
  if (isTRUE(create_age_groups)) {
    if (!is.null(age_breaks) && length(age_breaks) <= 3) {
      age_desc <- sprintf(
        "%d-%d",
        min(age_breaks, na.rm = TRUE),
        max(age_breaks[is.finite(age_breaks)], na.rm = TRUE)
      )
    } else {
      age_desc <- "custom age group structure"
    }

    var_details <- c(
      var_details,
      sprintf("Age groups (%s)", age_desc)
    )
  }

  # Calendar variables
  if (isTRUE(create_calendar_vars)) {
    var_details <- c(
      var_details,
      "Calendar variables"
    )
  }

  # Climate variables
  if (isTRUE(create_climate_vars)) {
    climate_label <- if (!is.null(climate_region)) {
      climate_region
    } else {
      "automatic region"
    }

    var_details <- c(
      var_details,
      sprintf("Climate variables (%s)", climate_label)
    )
  }

  # Create history message
  history_msg <- if (length(var_details) > 0) {
    sprintf(
      "Derived variables created [%s]",
      paste(var_details, collapse = " | ")
    )
  } else {
    "No derived variables were created"
  }
  # Register metadata
  df <- sus_meta(df, add_history = history_msg)

  return(df)
}

.variable_arrow_internal <- function(
  df,
  create_age_groups,
  age_breaks,
  age_labels,
  create_calendar_vars,
  create_climate_vars,
  climate_region,
  date_col,
  age_col,
  hemisphere,
  lang,
  verbose
) {
  lazy = TRUE
  duckdb_conn = NULL
  is_arrow_dataset <- inherits(df, "ArrowObject") || 
                      inherits(df, "FileSystemDataset") ||
                      inherits(df, "arrow_dplyr_query")
  is_duckdb_conn <- inherits(df, "duckdb_connection") ||
                    inherits(df, "tbl_duckdb_connection")
  
  if (!lang %in% c("en", "pt", "es")) {
    stop("lang must be one of: 'en', 'pt', 'es'")
  }
  
  if (!hemisphere %in% c("south", "north")) {
    stop("hemisphere must be 'south' or 'north'")
  }
  
  created_vars <- character(0)
  
  # Extract system from metadata if available
  system <- sus_meta(df, "system")
  
  # Get column names
  if (is_arrow_dataset || is_duckdb_conn) {
    col_names <- names(df)
  } else {
    col_names <- names(df)
  }
  
  # ========================================================================
  # AGE COLUMN IDENTIFICATION (LAZY COMPATIBLE)
  # ========================================================================
  
  age_col_to_use <- age_col
  
  # Check if user-specified age column exists
  if (!is.null(age_col)) {
    if (age_col %in% col_names) {
      if (verbose) {
        msg <- switch(lang,
          "en" = paste0("Using existing age column: ", age_col),
          "pt" = paste0("Usando coluna de idade existente: ", age_col),
          "es" = paste0("Usando columna de edad existente: ", age_col)
        )
        cli::cli_alert_info(msg)
      }
    } else {
      age_col_to_use <- NULL
    }
  }
  
  # For lazy data, build age calculation expressions
  if ((is_arrow_dataset || is_duckdb_conn) && is.null(age_col_to_use)) {
    
    if (verbose) {
      cli::cli_alert_info("Attempting lazy age calculation from dates...")
    }
    
    # Detect birth date column (padronizada)
    birth_col <- NULL
    birth_patterns <- c("data_nascimento", "birth_date", "fecha_nacimiento", "DTNASC")
    for (pattern in birth_patterns) {
      if (pattern %in% col_names) {
        birth_col <- pattern
        break
      }
    }
    
    # Detect event date column (padronizada para SIH)
    event_col <- NULL
    event_patterns <- c("data_internacao", "admission_date", "data_evento", 
                        "DT_INTER", "SP_DTINTER", "data_internacao_sp")
    for (pattern in event_patterns) {
      if (pattern %in% col_names) {
        event_col <- pattern
        break
      }
    }
    
    if (!is.null(birth_col) && !is.null(event_col)) {
      
      if (is_arrow_dataset) {
        # Arrow stores Date as int32 (days since 1970-01-01).
        # as.integer(Date) casts to int32 — fully supported kernel.
        # Subtracting two int32 values gives days between dates (no duration cast).
        df <- df |>
          dplyr::mutate(
            .data_nasc_tmp = as.integer(as.Date(.data[[birth_col]])),
            .data_evnt_tmp = as.integer(as.Date(.data[[event_col]])),
            age_years      = floor((.data_evnt_tmp - .data_nasc_tmp) / 365.25)
          ) |>
          dplyr::select(-".data_nasc_tmp", -".data_evnt_tmp")
          
      } else if (is_duckdb_conn) {
        # DuckDB SQL expression
        df <- df |>
          dplyr::mutate(
            age_years = sql(
              paste0(
                "FLOOR(DATEDIFF('day', ", birth_col, ", ", event_col, ") / 365.25)"
              )
            )
          )
      }
      
      age_col_to_use <- "age_years"
      
      if (verbose) {
        cli::cli_alert_success("Lazy age calculation from dates applied")
      }
      
    } else {
      # Try age code column (DATASUS format)
      age_code_col <- NULL
      age_code_patterns <- c("codigo_idade", "age_code", "IDADE", "NU_IDADE_N",
                             "idade_paciente", "patient_age_code", "edad_paciente")
      for (pattern in age_code_patterns) {
        if (pattern %in% col_names) {
          age_code_col <- pattern
          break
        }
      }
      
      if (!is.null(age_code_col)) {
        if (verbose) {
          cli::cli_alert_info("Decoding DATASUS age codes...")
        }
        
        if (is_arrow_dataset) {
          # For Arrow, we must materialise temporarily for DATASUS code decoding
          # because the branching logic isn't expressible as a pure Arrow expression.
          
          if (verbose) {
            cli::cli_alert_warning("Age code decoding requires materialization...")
          }
          
          # Collect only the age code column for decoding
          age_codes_df <- df |>
            dplyr::select(dplyr::all_of(age_code_col)) |>
            dplyr::collect()
          
          # Decode
          age_years_vec <- decode_datasus_age_vectorized(age_codes_df[[age_code_col]])
          
          # Re-attach to the full collected data frame
          df_collected <- df |> dplyr::collect()
          df_collected$age_years <- age_years_vec
          
          if (lazy) {
            df <- arrow::as_arrow_table(df_collected)
          } else {
            df <- df_collected
          }
          
          age_col_to_use <- "age_years"
          
          if (verbose) {
            cli::cli_alert_success("Age decoded from DATASUS codes")
          }
        } else {
          # DuckDB: can use CASE WHEN inline
          df <- df |>
            dplyr::mutate(
              age_years = sql(
                paste0("
                  CASE 
                    WHEN SUBSTR(CAST(", age_code_col, " AS VARCHAR), 1, 1) = '1' THEN 0
                    WHEN SUBSTR(CAST(", age_code_col, " AS VARCHAR), 1, 1) = '2' THEN 0
                    WHEN SUBSTR(CAST(", age_code_col, " AS VARCHAR), 1, 1) = '3' THEN 
                      FLOOR(CAST(SUBSTR(CAST(", age_code_col, " AS VARCHAR), 2) AS INTEGER) / 12)
                    WHEN SUBSTR(CAST(", age_code_col, " AS VARCHAR), 1, 1) = '4' THEN 
                      CAST(SUBSTR(CAST(", age_code_col, " AS VARCHAR), 2) AS INTEGER)
                    WHEN SUBSTR(CAST(", age_code_col, " AS VARCHAR), 1, 1) = '5' THEN 
                      CAST(SUBSTR(CAST(", age_code_col, " AS VARCHAR), 2) AS INTEGER) + 100
                    ELSE NULL
                  END
                ")
              )
            )
          age_col_to_use <- "age_years"
        }
      } else {
        # Fall back to any existing age-like column
        age_patterns <- c("idade", "age_years", "age", "edad")
        for (pattern in age_patterns) {
          if (pattern %in% col_names) {
            age_col_to_use <- pattern
            if (verbose) {
              cli::cli_alert_info(paste0("Using existing age column: ", pattern))
            }
            break
          }
        }
      }
    }
  }
  
  # Ensure age column exists
  if (is.null(age_col_to_use) || !(age_col_to_use %in% names(df))) {
    cli::cli_abort(c(
      "Could not determine age column or calculate age from available data",
      "i" = "Available columns with potential age information:",
      "*" = paste(grep("idade|nasc|data|age|birth", col_names, 
                       ignore.case = TRUE, value = TRUE), collapse = "\n  "),
      ">" = "Please specify age column manually with {.code age_col} parameter"
    ))
  }
  
  # ========================================================================
  # CREATE AGE GROUPS (LAZY COMPATIBLE)
  # ========================================================================
  
  if (create_age_groups && !is.null(age_col_to_use)) {
    
    if (verbose) {
      msg <- switch(lang,
        "en" = "Creating age groups...",
        "pt" = "Criando faixas etarias...",
        "es" = "Creando grupos de edad..."
      )
      cli::cli_alert_info(msg)
    }
    
    # Generate labels if not provided
    if (is.null(age_labels)) {
      age_labels <- generate_age_labels(age_breaks, lang)
    }
    
    if (is_arrow_dataset || is_duckdb_conn) {
      # Build two-sided formulas (condition ~ label) required by case_when().
      # The previous approach used bare expressions, which caused:
      # "Each argument to case_when() must be a two-sided formula".
      formulas <- vector("list", length(age_labels))
      for (i in seq_along(age_labels)) {
        lower <- age_breaks[i]
        upper <- age_breaks[i + 1]
        label <- age_labels[i]
        lhs <- if (is.infinite(upper)) {
          rlang::expr(.data[[!!age_col_to_use]] >= !!lower)
        } else {
          rlang::expr(.data[[!!age_col_to_use]] >= !!lower &
                      .data[[!!age_col_to_use]] <  !!upper)
        }
        formulas[[i]] <- rlang::new_formula(lhs, label)
      }
      case_expr <- rlang::call2("case_when", !!!formulas, .default = NA_character_)
      df <- df |> dplyr::mutate(age_group = !!case_expr)
      
    } else {
      # Regular data frame
      df$age_group <- cut(
        df[[age_col_to_use]],
        breaks = age_breaks,
        labels = age_labels,
        right = FALSE,
        include.lowest = TRUE
      )
    }
    
    created_vars <- c(created_vars, "age_group")
    
    # Climate risk group
    risk_labels <- switch(lang,
      "en" = c("High Risk (0-4)", "Standard Risk (5-64)", "High Risk (65+)"),
      "pt" = c("Alto Risco (0-4)", "Risco Padrao (5-64)", "Alto Risco (65+)"),
      "es" = c("Alto Riesgo (0-4)", "Riesgo Estandar (5-64)", "Alto Riesgo (65+)")
    )
    
    risk_varname <- switch(lang,
      "en" = "climate_risk_group",
      "pt" = "grupo_risco_climatico",
      "es" = "grupo_riesgo_climatico"
    )
    
    if (is_arrow_dataset || is_duckdb_conn) {
      df <- df |>
        dplyr::mutate(
          !!risk_varname := dplyr::case_when(
            .data[[age_col_to_use]] <= 4 ~ !!risk_labels[1],
            .data[[age_col_to_use]] <= 64 ~ !!risk_labels[2],
            .data[[age_col_to_use]] >= 65 ~ !!risk_labels[3],
            .default = NA_character_
          )
        )
    } else {
      df[[risk_varname]] <- cut(
        df[[age_col_to_use]],
        breaks = c(-1, 4, 64, Inf),
        labels = risk_labels,
        right = TRUE,
        include.lowest = TRUE
      )
    }
    
    created_vars <- c(created_vars, risk_varname)
    
    # IBGE age groups
    ibge_labels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                     "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", 
                     "60-64", "65-69", "70-74", "75-79", "80+")
    
    ibge_varname <- switch(lang,
      "pt" = "faixa_etaria_ibge",
      "en" = "ibge_age_group",
      "es" = "grupo_edad_ibge"
    )
    
    if (is_arrow_dataset || is_duckdb_conn) {
      ibge_breaks <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 
                       65, 70, 75, 80, Inf)
      
      formulas_ibge <- vector("list", length(ibge_labels))
      for (i in seq_along(ibge_labels)) {
        lower <- ibge_breaks[i]
        upper <- ibge_breaks[i + 1]
        label <- ibge_labels[i]
        lhs <- if (is.infinite(upper)) {
          rlang::expr(.data[[!!age_col_to_use]] >= !!lower)
        } else {
          rlang::expr(.data[[!!age_col_to_use]] >= !!lower &
                      .data[[!!age_col_to_use]] <  !!upper)
        }
        formulas_ibge[[i]] <- rlang::new_formula(lhs, label)
      }
      case_expr_ibge <- rlang::call2("case_when", !!!formulas_ibge, .default = NA_character_)
      df <- df |> dplyr::mutate(!!ibge_varname := !!case_expr_ibge)
    } else {
      df[[ibge_varname]] <- cut(
        df[[age_col_to_use]],
        breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 
                   65, 70, 75, 80, Inf),
        labels = ibge_labels,
        right = FALSE,
        include.lowest = TRUE
      )
    }
    
    created_vars <- c(created_vars, ibge_varname)
  }
  
  # ========================================================================
  # CALENDAR VARIABLES (simplified for Arrow compatibility)
  # ========================================================================
  
  if (create_calendar_vars) {
    
    # Auto-detect date column
    if (is.null(date_col)) {
      date_patterns <- c("data_internacao", "data_internacao_sp", "DT_INTER", 
                         "SP_DTINTER", "admission_date")
      for (pattern in date_patterns) {
        if (pattern %in% col_names) {
          date_col <- pattern
          break
        }
      }
      
      if (verbose && !is.null(date_col)) {
        msg <- switch(lang,
          "en" = paste0("Auto-detected date column: ", date_col),
          "pt" = paste0("Coluna de data auto-detectada: ", date_col),
          "es" = paste0("Columna de fecha auto-detectada: ", date_col)
        )
        cli::cli_alert_info(msg)
      }
    }
    
    if (!is.null(date_col) && date_col %in% col_names) {
      
      if (verbose) {
        msg <- switch(lang,
          "en" = "Creating calendar variables...",
          "pt" = "Criando variaveis de calendario...",
          "es" = "Creando variables de calendario..."
        )
        cli::cli_alert_info(msg)
      }
      
      if (is_arrow_dataset || is_duckdb_conn) {
        df <- df |>
          dplyr::mutate(
            data_ref  = as.Date(.data[[date_col]]),
            ano       = lubridate::year(.data$data_ref),
            mes       = lubridate::month(.data$data_ref),
            trimestre = lubridate::quarter(.data$data_ref)
          ) |>
          dplyr::select(-data_ref)
        
        created_vars <- c(created_vars, "ano", "mes", "trimestre")
        
      } else {
        if (!inherits(df[[date_col]], "Date")) {
          df[[date_col]] <- as.Date(df[[date_col]])
        }
        
        df$ano       <- lubridate::year(df[[date_col]])
        df$mes       <- lubridate::month(df[[date_col]])
        df$trimestre <- lubridate::quarter(df[[date_col]])
        
        created_vars <- c(created_vars, "ano", "mes", "trimestre")
      }
    } else {
      cli::cli_alert_warning("No date column found. Calendar variables not created.")
    }
  }
  
  # ========================================================================
  # SUMMARY
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
  # Add climasus_df metadata if not lazy 
  df <- sus_meta(
    df, 
    stage = "derive", 
    type = "derive",
    add_history = sprintf("[%s] Create variables (Arrow optimized)", 
                   ))
  
  
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
    "age_years",
    "age_code",
    "codigo_idade",
    "codigo_edad",
    "IDADE", "idade", "age", "edad",
    "idade_mae",   # SINASC maternal age
    "mother_age", "edad_madre"
  )
  
  for (pattern in age_patterns) {
    if (pattern %in% names(df)) {
      col <- df[[pattern]]
      if (is.numeric(col)) return(pattern)
      # Accept character columns that parse as numeric (e.g. "40", "032")
      if (is.character(col)) {
        suppressWarnings(vals <- as.numeric(col))
        if (mean(is.na(vals)) < 0.5) return(pattern)
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
    "age_code",
    "codigo_idade", "codigo_edad",
    "NU_IDADE_N",       # SINAN
    "IDADE",            # SIH raw
    "idade_paciente",   # SIA-PA standardized
    "patient_age_code", "edad_paciente"
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

#' Detect birth date column in lazy dataset
#' @keywords internal
#' @noRd
detect_birth_date_column_lazy <- function(col_names) {
  patterns <- c("birth_date", "data_nascimento", "fecha_nacimiento", "DTNASC")
  for (pattern in patterns) {
    if (pattern %in% col_names) return(pattern)
  }
  return(NULL)
}

#' Detect event date column in lazy dataset
#' @keywords internal
#' @noRd
detect_event_date_column_lazy <- function(col_names) {
  patterns <- c("death_date", "notification_date", "admission_date", "event_date",
                "data_obito", "data_notificacao", "data_internacao", "data_evento",
                "fecha_muerte", "fecha_notificacion", "fecha_evento",
                "DTOBITO", "DT_NOTIFIC", "DT_INTER", "DT_SAIDA", "DT_EVENT")
  for (pattern in patterns) {
    if (pattern %in% col_names) return(pattern)
  }
  return(NULL)
}

#' Detect age code column in lazy dataset
#' @keywords internal
#' @noRd
detect_age_code_column_lazy <- function(col_names) {
  patterns <- c("age_code", "codigo_idade", "codigo_edad", "NU_IDADE_N", "IDADE")
  for (pattern in patterns) {
    if (pattern %in% col_names) return(pattern)
  }
  return(NULL)
}

#' Detect date column in lazy dataset
#' @keywords internal
#' @noRd
detect_date_column_lazy <- function(col_names) {
  date_patterns <- c("date", "data", "fecha", "DT_NOTIFIC", "DTOBITO", "DT_INTER",
                     "DTNASC", "DT_SAIDA", "DT_EVENT", "notification_date",
                     "death_date", "admission_date", "event_date")
  for (pattern in date_patterns) {
    if (pattern %in% col_names) return(pattern)
  }
  return(NULL)
}