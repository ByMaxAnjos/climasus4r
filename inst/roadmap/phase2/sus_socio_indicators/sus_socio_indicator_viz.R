#' @title Intelligent Visualization of Health Indicators
#'
#' @description
#' Generates opinionated and aesthetically pleasing visualizations based on
#' indicator metadata and data structure (spatial, temporal, or ranking).
#'
#' @param data The object returned by `sus_socio_indicators`.
#' @param indicator String with the ID of the indicator (e.g., "infant_mortality_rate").
#' @param type Visualization type: "auto" (default), "map", "line", "bar".
#' @param top_n Integer (default 20) for ranking charts.
#' @param interactive Logical. If `TRUE`, converts the ggplot to a plotly object.
#' @param lang Language for labels: "pt" (default), "en", or "es".
#'
#' @return A ggplot2 object or a plotly object.
#'
#' @import ggplot2
#' @importFrom dplyr filter slice_max n_distinct
#' @importFrom scales comma
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming df_final is an sf object with indicator calculated
#' # df_final <- sus_socio_indicators(df_enriched, "infant_mortality_rate")
#'
#' # 1. Automatic map for spatial data with one year
#' # sus_socio_indicator_viz(df_final, "infant_mortality_rate")
#'
#' # 2. Line chart for temporal trends (assuming multiple years)
#' # sus_socio_indicator_viz(df_final_temporal, "dependency_ratio", type = "line")
#'
#' # 3. Bar chart for ranking
#' # sus_socio_indicator_viz(df_final, "infant_mortality_rate", type = "bar")
#' }
sus_socio_indicator_viz <- function(data,
                                    indicator,
                                    type = "auto",
                                    top_n = 20,
                                    interactive = FALSE,
                                    lang = "pt") {
  
  # 1. Metadata Retrieval
  source("/home/ubuntu/climasus4r/R/indicators_list_v3.R")
  ind_meta <- INDICATORS_LIST[[indicator]]
  
  if (is.null(ind_meta)) {
    stop(sprintf("Indicator '%s' not found in the catalog.", indicator))
  }
  
  # Select label based on language
  label <- switch(lang,
                  "pt" = ind_meta$name_pt,
                  "en" = ind_meta$name_en,
                  "es" = ind_meta$name_es)
  
  unit <- ind_meta$unit
  col_name <- paste0("ind_", indicator)
  
  if (!col_name %in% names(data)) {
    stop(sprintf("Column '%s' not found in the data. Did you run sus_socio_indicators()?", col_name))
  }
  
  # 2. Intelligence Logic ("Auto")
  is_spatial <- inherits(data, "sf")
  n_years <- if ("year" %in% names(data)) dplyr::n_distinct(data$year) else 1
  
  if (type == "auto") {
    if (is_spatial && n_years == 1) {
      type <- "map"
    } else if (n_years > 1) {
      type <- "line"
    } else {
      type <- "bar"
    }
  }
  
  # 3. Base Plotting
  p <- NULL
  
  # --- MAP ---
  if (type == "map") {
    if (!is_spatial) stop("Data must be an 'sf' object to plot a map.")
    
    p <- ggplot(data) +
      geom_sf(aes(fill = .data[[col_name]]), color = "white", size = 0.1) +
      scale_fill_viridis_c(option = "magma", labels = scales::comma) +
      theme_void() +
      labs(
        title = label,
        subtitle = sprintf("Unidade: %s", unit),
        fill = "Valor"
      )
  }
  
  # --- LINE ---
  if (type == "line") {
    if (!"year" %in% names(data)) stop("Data must have a 'year' column for line charts.")
    
    # Aggregation for line chart if needed (e.g., if data is at municipality level)
    # For simplicity, we assume the data is already aggregated for the line plot
    
    p <- ggplot(data, aes(x = year, y = .data[[col_name]])) +
      geom_line(color = "#2c3e50", size = 1) +
      geom_point(color = "#e74c3c", size = 2) +
      theme_minimal() +
      labs(
        title = sprintf("%s - Tendencia Temporal", label),
        subtitle = sprintf("Unidade: %s", unit),
        x = "Ano",
        y = label
      )
  }
  
  # --- BAR / RANKING ---
  if (type == "bar") {
    plot_data <- data %>%
      dplyr::slice_max(.data[[col_name]], n = top_n)
    
    # Try to find a name column for the Y axis
    name_col <- intersect(names(data), c("name_muni", "municipality", "code_muni"))[1]
    
    p <- ggplot(plot_data, aes(x = reorder(.data[[name_col]], .data[[col_name]]), y = .data[[col_name]])) +
      geom_col(fill = "#3498db") +
      coord_flip() +
      theme_minimal() +
      labs(
        title = sprintf("Top %s - %s", top_n, label),
        subtitle = sprintf("Unidade: %s", unit),
        x = "Localizacao",
        y = label
      )
  }
  
  # 4. Interactivity
  if (interactive) {
    if (requireNamespace("plotly", quietly = TRUE)) {
      return(plotly::ggplotly(p))
    } else {
      message("Package 'plotly' not found. Returning static ggplot. Install 'plotly' for interactivity.")
    }
  }
  
  return(p)
}
