#' @title Explore the Catalog of Socio-demographic Indicators
#'
#' @description
#' Converts the internal `INDICATORS_LIST` into a flat table for easy consultation.
#' It can render an interactive table in the RStudio Viewer or return a tibble.
#'
#' @param viewer Logical. If `TRUE` (default), attempts to render an interactive
#'   table using the `DT` package. If `FALSE` or if `DT` is not installed,
#'   returns a static tibble.
#' @param lang Language for the catalog: "pt" (default), "en", or "es".
#'
#' @return A tibble containing the indicator IDs, names, units, and formulas.
#'
#' @importFrom dplyr bind_rows mutate select
#' @importFrom tibble as_tibble
#' @export
#'
#' @examples
#' \dontrun{
#' # View the catalog interactively
#' sus_socio_indicator_explore()
#'
#' # Get the catalog as a tibble for programming
#' catalog <- sus_socio_indicator_explore(viewer = FALSE)
#' }
sus_socio_indicator_explore <- function(viewer = TRUE, lang = "pt") {
  
  # 1. Load the internal list (Assuming it's available in the package environment)
  # For this implementation, we source the file created in the previous task
  source("/home/ubuntu/climasus4r/R/indicators_list_v3.R")
  
  # 2. Transform nested list to flat tibble
  catalog <- lapply(names(INDICATORS_LIST), function(id) {
    item <- INDICATORS_LIST[[id]]
    
    # Select name based on language
    name_col <- switch(lang,
                       "pt" = item$name_pt,
                       "en" = item$name_en,
                       "es" = item$name_es)
    
    tibble::tibble(
      id = id,
      name = name_col,
      unit = item$unit,
      multiplier = ifelse(is.null(item$multiplier), NA, item$multiplier),
      formula = ifelse(is.null(item$formula), "Numerator/Denominator", item$formula)
    )
  }) %>% 
    dplyr::bind_rows()
  
  # 3. Handle Interactivity
  if (viewer) {
    if (requireNamespace("DT", quietly = TRUE)) {
      return(DT::datatable(
        catalog,
        options = list(pageLength = 10, scrollX = TRUE),
        caption = "climasus4r - Indicators Catalog",
        rownames = FALSE,
        filter = "top"
      ))
    } else {
      message("Package 'DT' not found. Returning static table. Install 'DT' for interactivity.")
      if (interactive()) {
        View(catalog)
      }
    }
  }
  
  return(catalog)
}
