#' Explore Climate Normals Variables
#'
#' @description
#' This function opens an interactive HTML viewer in the browser to explore the available
#' climate normals variables from the INMET normals dictionary. It allows users to search,
#' filter, and view details about the variables available for download via the
#' `sus_climate_normals` function.
#'
#' The function uses the `climate_normals_dictionary_final.csv` file, which must be
#' located in the `inst/extdata` directory of the `{climasus4r}` package.
#'
#' @param lang A character string specifying the language for the interactive viewer.
#'   Supported languages are "pt" (Portuguese), "en" (English), and "es" (Spanish).
#'   Defaults to "en".
#'
#' @return
#' This function does not return a value. It is called for its side effect of
#' launching an interactive data explorer in a web browser.
#'
#' @importFrom DT datatable formatStyle styleEqual
#' @importFrom htmltools browsable tagList tags
#' @importFrom utils browseURL
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   sus_normals_explore(lang = "pt")
#' }
sus_normals_explore <- function(lang = "en") {
  
  # Validate language
  lang <- tolower(lang)
  if (!lang %in% c("pt", "en", "es")) {
    lang <- "en" # Default to English if lang is invalid
  }
  
  # --- Load Internal Dictionary ---
  dictionary_path <- system.file("extdata", "climate_normals_dictionary_final.csv", package = "climasus4r")
  
  if (!file.exists(dictionary_path)) {
    stop("Climate normals dictionary not found. Please ensure 'climate_normals_dictionary_final.csv' is in 'inst/extdata'.")
  }
  
  # Use a tryCatch for robustness in reading the file
  tryCatch({
    variables_df <- read.csv(dictionary_path, stringsAsFactors = FALSE)
  }, error = function(e) {
    stop(paste("Failed to read the climate normals dictionary:", e$message))
  })
  
  # --- Select Columns based on Language ---
  # Define which columns to display and their names based on the selected language
  lang_cols <- switch(lang,
                    pt = list(cols = c("variable_pt", "period", "var_code"), 
                              names = c("Vari\u00e1vel", "Per\u00edodo", "C\u00f3digo")), # Variável, Período, Código
                    es = list(cols = c("variable_es", "period", "var_code"), 
                              names = c("Variable", "Per\u00edodo", "C\u00f3digo")),      # Variable, Período, Código
                    en = list(cols = c("variable_en", "period", "var_code"), 
                              names = c("Variable", "Period", "Code")))
  
  # Subset the dataframe to only the required columns
  display_df <- variables_df[, lang_cols$cols]
  colnames(display_df) <- lang_cols$names
  
  # --- Create Interactive DataTable ---
  # Using the same styling and options as sus_census_select for consistency
  dt_obj <- DT::datatable(
    display_df,
    filter = 'top',
    rownames = FALSE,
    class = 'cell-border stripe',
    options = list(
      pageLength = 15,
      lengthMenu = c(15, 30, 50, 100),
      searchHighlight = TRUE,
      language = list(
        url = switch(lang,
                     pt = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json',
                     es = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Spanish.json',
                     en = '//cdn.datatables.net/plug-ins/1.10.25/i18n/English.json')
      )
    )
  ) %>%
    DT::formatStyle(
      columns = 2, # The 'Period' column
      backgroundColor = DT::styleEqual(
        levels = c("1961-1990", "1981-2010", "1991-2020"),
        values = c('#ccebc5', '#ffffcc', '#a8ddb5') # Pastel colors
      )
    )
  
  # --- Create HTML Wrapper ---
  # This follows the 'climate forest' theme from your preference
  title_text <- switch(lang,
                       pt = "Explorador de Vari\u00e1veis das Normais Climatol\u00f3gicas", # Explorador de Variáveis das Normais Climatológicas
                       es = "Explorador de Variables de las Normales Climatol\u00f3gicas", # Explorador de Variables de las Normales Climatológicas
                       en = "Climate Normals Variable Explorer")
  
  # HTML and CSS for the wrapper page
  html_wrapper <- htmltools::browsable(
    htmltools::tagList(
      htmltools::tags$head(
        htmltools::tags$style(HTML("
          body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; background-color: #f4f4f4; }
          .container { margin: 20px; }
          .title {
            font-size: 24px;
            color: #2c3e50; /* Dark blue-gray */
            margin-bottom: 20px;
            border-bottom: 2px solid #16a085; /* Greenish accent */
            padding-bottom: 10px;
          }
        "))
      ),
      htmltools::tags$div(class = "container",
                          htmltools::tags$h1(class = "title", title_text),
                          dt_obj
      )
    )
  )
  
  # --- Launch in Browser ---
  # Create a temporary file to host the HTML content
  temp_file <- tempfile(fileext = ".html")
  htmltools::save_html(html_wrapper, file = temp_file)
  
  # Open the file in the default browser
  utils::browseURL(temp_file)
  
  # The function returns nothing, as its purpose is the side effect
  invisible(NULL)
}
