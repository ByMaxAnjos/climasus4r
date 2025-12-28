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
#' @param municipality_code Character or numeric vector specifying municipality
#'   codes (IBGE 6 or 7-digit codes) to include. If `NULL` (default), includes
#'   all municipalities.
#' @param lang Character string specifying the language for messages. Options:
#'   `"en"` (English, default), `"pt"` (Portuguese), `"es"` (Spanish).
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
#'   sex = "Feminino",
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
#'   municipality_code = "3550308",
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
                                          lang = "en",
                                          verbose = TRUE) {
  
  # Validate inputs
  if (!is.data.frame(df)) {
    cli::cli_abort("df must be a data frame")
  }
  
  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_abort("lang must be one of: 'en', 'pt', 'es'")
  }
  
  # Store original row count
  n_original <- nrow(df)
  
  # Track filters applied
  filters_applied <- character()
  
  # ========================================================================
  # FILTER BY SEX
  # ========================================================================
  
  if (!is.null(sex)) {
    sex_col <- find_column(df, c("sex", "sexo", "SEXO"))
    
    if (is.null(sex_col)) {
      cli::cli_alert_warning("Sex column not found. Skipping sex filter.")
    } else {
      # Normalize sex values (case-insensitive matching)
      df <- df[tolower(df[[sex_col]]) %in% tolower(sex), ]
      filters_applied <- c(filters_applied, paste0("sex: ", paste(sex, collapse = ", ")))
    }
  }
  
  # ========================================================================
  # FILTER BY RACE
  # ========================================================================
  
  if (!is.null(race)) {
    race_col <- find_column(df, c("race", "raca", "raza", "RACACOR"))
    
    if (is.null(race_col)) {
      cli::cli_alert_warning("Race column not found. Skipping race filter.")
    } else {
      df <- df[tolower(df[[race_col]]) %in% tolower(race), ]
      filters_applied <- c(filters_applied, paste0("race: ", paste(race, collapse = ", ")))
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
      
      df <- df[df[[age_col]] >= min_age & df[[age_col]] <= max_age, ]
      filters_applied <- c(filters_applied, 
                          paste0("age: ", min_age, "-", 
                                 ifelse(is.infinite(max_age), "+", max_age)))
    }
  }
  
  # ========================================================================
  # FILTER BY EDUCATION
  # ========================================================================
  
  if (!is.null(education)) {
    edu_col <- find_column(df, c("education", "escolaridade", "escolaridad", 
                                  "ESC", "ESC2010"))
    
    if (is.null(edu_col)) {
      cli::cli_alert_warning("Education column not found. Skipping education filter.")
    } else {
      df <- df[tolower(df[[edu_col]]) %in% tolower(education), ]
      filters_applied <- c(filters_applied, 
                          paste0("education: ", paste(education, collapse = ", ")))
    }
  }
  
  # ========================================================================
  # FILTER BY MARITAL STATUS
  # ========================================================================
  
  if (!is.null(marital_status)) {
    marital_col <- find_column(df, c("marital_status", "estado_civil", 
                                      "estado_civil", "ESTCIV"))
    
    if (is.null(marital_col)) {
      warning("Marital status column not found. Skipping marital status filter.")
    } else {
      df <- df[tolower(df[[marital_col]]) %in% tolower(marital_status), ]
      filters_applied <- c(filters_applied, 
                          paste0("marital_status: ", paste(marital_status, collapse = ", ")))
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
      
      df <- df[df[[muni_col]] %in% municipality_code, ]
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
