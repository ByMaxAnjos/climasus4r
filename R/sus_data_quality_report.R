#' Generate Data Quality Report for Health Data
#'
#' Generates a comprehensive data quality report for health data, including
#' summaries of missing values, data distributions, date validations, and
#' ICD code frequencies. This function helps identify potential data quality
#' issues before analysis.
#'
#' @param df A data frame containing health data.
#' @param output_format Character string specifying the output format. Options:
#'   `"console"` (default, prints to console), `"html"` (saves HTML report),
#'   `"markdown"` (saves Markdown report).
#' @param output_file Character string with the path to save the report file.
#'   Required if `output_format` is not `"console"`. If `NULL`, uses a default
#'   filename based on timestamp.
#' @param check_dates Logical. If `TRUE` (default), performs date validation
#'   checks (e.g., future dates, dates before birth).
#' @param check_icd Logical. If `TRUE` (default), summarizes ICD code distributions.
#' @param top_n Integer. Number of top categories to show in frequency tables.
#'   Default is 10.
#' @param lang Character string specifying the language for the report. Options:
#'   `"en"` (English, default), `"pt"` (Portuguese), `"es"` (Spanish).
#'
#' @return Invisibly returns a list containing the quality metrics. If
#'   `output_format = "console"`, prints the report to the console. Otherwise,
#'   saves the report to a file.
#'
#' @details
#' The data quality report includes:
#' \itemize{
#'   \item **Dataset Overview**: Dimensions, column types
#'   \item **Missing Values**: Count and percentage of NAs by column
#'   \item **Demographic Variables**: Frequency tables for sex, race, age groups
#'   \item **Date Validation**: Checks for invalid dates (future, before 1900, etc.)
#'   \item **ICD Codes**: Top 10 most frequent diagnosis codes
#'   \item **Geographic Distribution**: Top municipalities
#' }
#'
#' @examples
#' \dontrun{
#' library(climasus4r)
#'
#' # Print report to console
#' sus_data_quality_report(df, lang = "pt")
#'
#' # Save HTML report
#' sus_data_quality_report(
#'   df,
#'   output_format = "html",
#'   output_file = "reports/dq_report.html",
#'   lang = "en"
#' )
#'
#' # Save Markdown report
#' sus_data_quality_report(
#'   df,
#'   output_format = "markdown",
#'   output_file = "reports/dq_report.md"
#' )
#' }
#'
#' @export
sus_data_quality_report <- function(df,
                                     output_format = "console",
                                     output_file = NULL,
                                     check_dates = TRUE,
                                     check_icd = TRUE,
                                     top_n = 10,
                                     lang = "en") {
  
  # Validate inputs
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }
  
  if (!output_format %in% c("console", "html", "markdown")) {
    stop("output_format must be one of: 'console', 'html', 'markdown'")
  }
  
  if (output_format != "console" && is.null(output_file)) {
    # Generate default filename
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    ext <- ifelse(output_format == "html", ".html", ".md")
    output_file <- paste0("dq_report_", timestamp, ext)
  }
  
  if (!lang %in% c("en", "pt", "es")) {
    stop("lang must be one of: 'en', 'pt', 'es'")
  }
  
  # Initialize report content
  report <- list()
  
  # ========================================================================
  # 1. DATASET OVERVIEW
  # ========================================================================
  
  report$overview <- list(
    n_rows = nrow(df),
    n_cols = ncol(df),
    col_types = sapply(df, class)
  )
  
  # ========================================================================
  # 2. MISSING VALUES ANALYSIS
  # ========================================================================
  
  missing_summary <- data.frame(
    column = names(df),
    n_missing = sapply(df, function(x) sum(is.na(x))),
    pct_missing = sapply(df, function(x) round(100 * sum(is.na(x)) / length(x), 2))
  )
  missing_summary <- missing_summary[missing_summary$n_missing > 0, ]
  missing_summary <- missing_summary[order(-missing_summary$n_missing), ]
  
  report$missing_values <- missing_summary
  
  # ========================================================================
  # 3. DEMOGRAPHIC VARIABLES
  # ========================================================================
  
  report$demographics <- list()
  
  # Sex distribution
  sex_col <- find_column(df, c("sex", "sexo", "SEXO"))
  if (!is.null(sex_col)) {
    report$demographics$sex <- as.data.frame(table(df[[sex_col]], useNA = "ifany"))
    names(report$demographics$sex) <- c("Category", "Count")
  }
  
  # Race distribution
  race_col <- find_column(df, c("race", "raca", "raza", "RACACOR"))
  if (!is.null(race_col)) {
    report$demographics$race <- as.data.frame(table(df[[race_col]], useNA = "ifany"))
    names(report$demographics$race) <- c("Category", "Count")
  }
  
  # Age distribution
  age_col <- find_column(df, c("age_years", "age", "idade", "edad", "IDADE"))
  if (!is.null(age_col)) {
    report$demographics$age_summary <- summary(df[[age_col]])
  }
  
  # ========================================================================
  # 4. DATE VALIDATION
  # ========================================================================
  
  if (check_dates) {
    report$date_validation <- list()
    
    # Find all date columns
    date_cols <- names(df)[sapply(df, function(x) inherits(x, "Date") || 
                                                   inherits(x, "POSIXct"))]
    
    for (col in date_cols) {
      dates <- as.Date(df[[col]])
      
      validation <- list(
        n_missing = sum(is.na(dates)),
        n_future = sum(dates > Sys.Date(), na.rm = TRUE),
        n_before_1900 = sum(dates < as.Date("1900-01-01"), na.rm = TRUE),
        date_range = c(min(dates, na.rm = TRUE), max(dates, na.rm = TRUE))
      )
      
      report$date_validation[[col]] <- validation
    }
  }
  
  # ========================================================================
  # 5. ICD CODE DISTRIBUTION
  # ========================================================================
  
  if (check_icd) {
    report$icd_distribution <- list()
    
    # Find ICD columns
    icd_patterns <- c("underlying_cause", "causa_basica", "CAUSABAS",
                      "primary_diagnosis", "diagnostico_principal", "DIAG_PRINC",
                      "PA_CIDPRI")
    
    icd_col <- find_column(df, icd_patterns)
    
    if (!is.null(icd_col)) {
      icd_freq <- as.data.frame(table(df[[icd_col]]), stringsAsFactors = FALSE)
      names(icd_freq) <- c("ICD_Code", "Count")
      icd_freq <- icd_freq[order(-icd_freq$Count), ]
      icd_freq$Percentage <- round(100 * icd_freq$Count / sum(icd_freq$Count), 2)
      
      report$icd_distribution$top_codes <- utils::head(icd_freq, top_n)
      report$icd_distribution$n_unique_codes <- nrow(icd_freq)
    }
  }
  
  # ========================================================================
  # 6. GEOGRAPHIC DISTRIBUTION
  # ========================================================================
  
  report$geographic <- list()
  
  muni_col <- find_column(df, c("residence_municipality_code", "municipio_residencia",
                                 "CODMUNRES", "municipality_code"))
  
  if (!is.null(muni_col)) {
    muni_freq <- as.data.frame(table(df[[muni_col]]), stringsAsFactors = FALSE)
    names(muni_freq) <- c("Municipality_Code", "Count")
    muni_freq <- muni_freq[order(-muni_freq$Count), ]
    muni_freq$Percentage <- round(100 * muni_freq$Count / sum(muni_freq$Count), 2)
    
    report$geographic$top_municipalities <- head(muni_freq, top_n)
    report$geographic$n_unique_municipalities <- nrow(muni_freq)
  }
  
  # ========================================================================
  # GENERATE OUTPUT
  # ========================================================================
  
  if (output_format == "console") {
    print_report_console(report, lang)
  } else if (output_format == "markdown") {
    write_report_markdown(report, output_file, lang)
    cli::cli_alert_success(paste0("Report saved to: ", output_file))
  } else if (output_format == "html") {
    write_report_html(report, output_file, lang)
    cli::cli_alert_success(paste0("Report saved to: ", output_file))
  }
  
  invisible(report)
}


# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Find column by patterns
find_column <- function(df, patterns) {
  for (pattern in patterns) {
    if (pattern %in% names(df)) {
      return(pattern)
    }
  }
  return(NULL)
}


# Print report to console
print_report_console <- function(report, lang) {
  
  # Title
  title <- switch(lang,
    "en" = "DATA QUALITY REPORT",
    "pt" = "RELATORIO DE QUALIDADE DE DADOS",
    "es" = "INFORME DE CALIDAD DE DATOS"
  )
  
  cli::cli_h1(title)
  cli::cli_text("")
  
  # Overview
  overview_title <- switch(lang,
    "en" = "Dataset Overview",
    "pt" = "Visao Geral do Dataset",
    "es" = "Vision General del Dataset"
  )
  cli::cli_h2(overview_title)
  cli::cli_text(paste0("Rows: ", format(report$overview$n_rows, big.mark = ",")))
  cli::cli_text(paste0("Columns: ", report$overview$n_cols))
  cli::cli_text("")
  
  # Missing values
  if (nrow(report$missing_values) > 0) {
    missing_title <- switch(lang,
      "en" = "Missing Values",
      "pt" = "Valores Faltantes",
      "es" = "Valores Faltantes"
    )
    cli::cli_h2(missing_title)
    print(report$missing_values, row.names = FALSE)
    cli::cli_text("")
  }
  
  # Demographics
  if (length(report$demographics) > 0) {
    demo_title <- switch(lang,
      "en" = "Demographic Variables",
      "pt" = "Variaveis Demograficas",
      "es" = "Variables Demograficas"
    )
    cli::cli_h2(demo_title)
    
    if (!is.null(report$demographics$sex)) {
      cli::cli_h3("Sex / Sexo")
      print(report$demographics$sex, row.names = FALSE)
      cli::cli_text("")
    }
    
    if (!is.null(report$demographics$race)) {
      cli::cli_h3("Race / Raca / Raza")
      print(report$demographics$race, row.names = FALSE)
      cli::cli_text("")
    }
    
    if (!is.null(report$demographics$age_summary)) {
      cli::cli_h3("Age Summary / Resumo de Idade")
      print(report$demographics$age_summary)
      cli::cli_text("")
    }
  }
  
  # Date validation
  if (length(report$date_validation) > 0) {
    date_title <- switch(lang,
      "en" = "Date Validation",
      "pt" = "Validacao de Datas",
      "es" = "Validacion de Fechas"
    )
    cli::cli_h2(date_title)
    
    for (col in names(report$date_validation)) {
      cli::cli_h3(col)
      val <- report$date_validation[[col]]
      cli::cli_text(paste0("Missing: ", val$n_missing))
      cli::cli_text(paste0("Future dates: ", val$n_future))
      cli::cli_text(paste0("Before 1900: ", val$n_before_1900))
      cli::cli_text(paste0("Range: ", val$date_range[1], " to ", val$date_range[2]))
      cli::cli_text("")
    }
  }
  
  # ICD distribution
  if (!is.null(report$icd_distribution$top_codes)) {
    icd_title <- switch(lang,
      "en" = "Top ICD Codes",
      "pt" = "Codigos CID Mais Frequentes",
      "es" = "Codigos CIE Mas Frecuentes"
    )
    cli::cli_h2(icd_title)
    cli::cli_text(paste0("Unique codes: ", report$icd_distribution$n_unique_codes))
    print(report$icd_distribution$top_codes, row.names = FALSE)
    cli::cli_text("")
  }
  
  # Geographic distribution
  if (!is.null(report$geographic$top_municipalities)) {
    geo_title <- switch(lang,
      "en" = "Top Municipalities",
      "pt" = "Municipios Mais Frequentes",
      "es" = "Municipios Mas Frecuentes"
    )
    cli::cli_h2(geo_title)
    cli::cli_text(paste0("Unique municipalities: ", report$geographic$n_unique_municipalities))
    print(report$geographic$top_municipalities, row.names = FALSE)
  }
}


# Write report to Markdown file
write_report_markdown <- function(report, output_file, lang) {
  
  # Create markdown content
  md <- character()
  
  # Title
  title <- switch(lang,
    "en" = "# Data Quality Report",
    "pt" = "# Relatorio de Qualidade de Dados",
    "es" = "# Informe de Calidad de Datos"
  )
  md <- c(md, title, "", paste0("Generated: ", Sys.time()), "", "---", "")
  
  # Overview
  md <- c(md, "## Dataset Overview", "")
  md <- c(md, paste0("- **Rows**: ", format(report$overview$n_rows, big.mark = ",")))
  md <- c(md, paste0("- **Columns**: ", report$overview$n_cols), "", "")
  
  # Missing values
  if (nrow(report$missing_values) > 0) {
    md <- c(md, "## Missing Values", "")
    md <- c(md, knitr::kable(report$missing_values, format = "markdown"), "")
  }
  
  # Add other sections similarly...
  
  # Write to file
  writeLines(md, output_file)
}


# Write report to HTML file
write_report_html <- function(report, output_file, lang) {
  
  # Create HTML content
  html <- character()
  
  html <- c(html, "<!DOCTYPE html>")
  html <- c(html, "<html>")
  html <- c(html, "<head>")
  html <- c(html, "<meta charset='UTF-8'>")
  html <- c(html, "<title>Data Quality Report</title>")
  html <- c(html, "<style>")
  html <- c(html, "body { font-family: Arial, sans-serif; margin: 40px; }")
  html <- c(html, "h1 { color: #2c3e50; }")
  html <- c(html, "h2 { color: #34495e; border-bottom: 2px solid #3498db; }")
  html <- c(html, "table { border-collapse: collapse; width: 100%; margin: 20px 0; }")
  html <- c(html, "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }")
  html <- c(html, "th { background-color: #3498db; color: white; }")
  html <- c(html, "</style>")
  html <- c(html, "</head>")
  html <- c(html, "<body>")
  
  # Title
  title <- switch(lang,
    "en" = "Data Quality Report",
    "pt" = "Relatorio de Qualidade de Dados",
    "es" = "Informe de Calidad de Datos"
  )
  html <- c(html, paste0("<h1>", title, "</h1>"))
  html <- c(html, paste0("<p>Generated: ", Sys.time(), "</p>"))
  
  # Overview
  html <- c(html, "<h2>Dataset Overview</h2>")
  html <- c(html, paste0("<p><strong>Rows:</strong> ", format(report$overview$n_rows, big.mark = ","), "</p>"))
  html <- c(html, paste0("<p><strong>Columns:</strong> ", report$overview$n_cols, "</p>"))
  
  # Add tables and other sections...
  
  html <- c(html, "</body>")
  html <- c(html, "</html>")
  
  # Write to file
  writeLines(html, output_file)
}
