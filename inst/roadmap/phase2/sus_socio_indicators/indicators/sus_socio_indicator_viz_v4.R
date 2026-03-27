#' @title Intelligent Visualization and Summary of Health Indicators
#'
#' @description
#' Generates professional visualizations (maps, lines, bars) or epidemiological
#' summary tables using `gtsummary`.
#'
#' @param data The object returned by `sus_socio_indicators`.
#' @param indicator String with the ID of the indicator (e.g., "infant_mortality_rate").
#' @param type Visualization type: "auto" (default), "map", "line", "bar", "table".
#' @param top_n Integer (default 20) for ranking charts.
#' @param interactive Logical. If `TRUE`, converts the ggplot to a plotly object.
#' @param lang Language for labels: "pt" (default), "en", or "es".
#'
#' @return A ggplot2 object, a plotly object, or a gtsummary table.
#'
#' @import ggplot2
#' @importFrom dplyr filter slice_max n_distinct
#' @importFrom scales comma
#' @export
sus_socio_indicator_viz <- function(data,
                                    indicator,
                                    type = "auto",
                                    top_n = 20,
                                    interactive = FALSE,
                                    lang = "pt") {
  
  # 1. Metadata Retrieval
  source("/home/ubuntu/climasus4r/R/indicators_list_v4.R")
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
  
  # 3. Route to Table (gtsummary)
  if (type == "table") {
    if (requireNamespace("gtsummary", quietly = TRUE)) {
      # Create a clean summary table
      table <- data %>%
        dplyr::select(dplyr::any_of(c("year", "name_muni", "abbrev_state", col_name))) %>%
        gtsummary::tbl_summary(
          by = if ("year" %in% names(.)) "year" else NULL,
          label = list(setNames(label, col_name)),
          statistic = list(all_continuous() ~ "{mean} ({sd})")
        ) %>%
        gtsummary::add_n() %>%
        gtsummary::bold_labels()
      
      return(table)
    } else {
      message("Package 'gtsummary' not found. Install it for professional tables.")
      return(head(data))
    }
  }
  
  # 4. Base Plotting (ggplot2)
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
    
    p <- ggplot(data, aes(x = year, y = .data[[col_name]])) +
      geom_line(color = "#1b4332", size = 1) +
      geom_point(color = "#52b788", size = 2) +
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
      geom_col(fill = "#40916c") +
      coord_flip() +
      theme_minimal() +
      labs(
        title = sprintf("Top %s - %s", top_n, label),
        subtitle = sprintf("Unidade: %s", unit),
        x = "Localizacao",
        y = label
      )
  }
  
  # 5. Interactivity
  if (interactive) {
    if (requireNamespace("plotly", quietly = TRUE)) {
      return(plotly::ggplotly(p))
    } else {
      message("Package 'plotly' not found. Returning static ggplot. Install 'plotly' for interactivity.")
    }
  }
  
  return(p)
}
