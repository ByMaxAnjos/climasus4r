#' Plot and Analyze Heatwave Events
#'
#' @description
#' Visualizes and analyzes the results from `sus_climate_compute_heatwaves()`.
#' It provides multiple visualization types including time series of events,
#' calendar heatmaps, intensity distributions, and annual summaries.
#'
#' @param hw_result List. The output from `sus_climate_compute_heatwaves()`.
#' @param type Character. Type of plot to generate:
#'   * `"timeline"`: Gantt-style timeline of heatwave events.
#'   * `"calendar"`: Calendar heatmap showing days with heatwaves.
#'   * `"intensity"`: Scatter plot of duration vs. intensity (EHF peak).
#'   * `"trend"`: Bar chart of number of events per year.
#' @param station_code Character. Optional. Filter by a specific station code.
#' @param method Character. Optional. Filter by a specific method (e.g., "EHF", "INMET").
#' @param year Numeric. Optional. Filter by a specific year (useful for calendar plots).
#' @param interactive Logical. If `TRUE`, returns an interactive Plotly chart. If `FALSE`, returns a static ggplot2 chart. Default is `TRUE`.
#' @param color_palette Character. Name of the ggsci color palette to use. Default is `"npg"`.
#' @param lang Character. Language for labels and titles: `"en"` (English), `"pt"` (Portuguese), or `"es"` (Spanish). Default is `"en"`.
#' @param save_plot Character. Optional file path to save the plot (e.g., "plot.html" or "plot.png").
#'
#' @return A ggplot or plotly object.
#' @export
sus_climate_plot_heatwaves <- function(
  hw_result,
  type = c("timeline", "calendar", "intensity", "trend"),
  station_code = NULL,
  method = NULL,
  year = NULL,
  interactive = TRUE,
  color_palette = "npg",
  lang = "en",
  save_plot = NULL
) {

  # Check required dependencies
  rlang::check_installed(
    c("ggplot2", "ggsci", "lubridate", "dplyr"),
    reason = "to run sus_climate_plot_heatwaves"
  )
  if (interactive) {
    rlang::check_installed("plotly", reason = "for interactive plots")
  }

  type <- match.arg(type)
  lang <- match.arg(lang, c("en", "pt", "es"))
  
  # Extract data from result list
  if (!is.list(hw_result) || !all(c("events", "daily", "summary") %in% names(hw_result))) {
    cli::cli_abort("hw_result must be the output list from sus_climate_compute_heatwaves().")
  }
  
  events_df <- hw_result$events
  daily_df  <- hw_result$daily
  summary_df <- hw_result$summary
  
  # Apply filters
  if (!is.null(station_code)) {
    events_df <- dplyr::filter(events_df, station_code %in% !!station_code)
    daily_df  <- dplyr::filter(daily_df, station_code %in% !!station_code)
    summary_df <- dplyr::filter(summary_df, station_code %in% !!station_code)
  }
  
  if (!is.null(method)) {
    events_df <- dplyr::filter(events_df, method %in% !!method)
    summary_df <- dplyr::filter(summary_df, method %in% !!method)
  }
  
  if (!is.null(year)) {
    events_df <- dplyr::filter(events_df, lubridate::year(start_date) %in% !!year)
    daily_df  <- dplyr::filter(daily_df, lubridate::year(date_day) %in% !!year)
    summary_df <- dplyr::filter(summary_df, year %in% !!year)
  }
  
  if (nrow(events_df) == 0) {
    cli::cli_warn("No heatwave events found after applying filters.")
    return(NULL)
  }
  
  # Get colors
  colors <- .get_hw_palette(color_palette)
  
  # Generate plot based on type
  p <- switch(type,
    "timeline"  = .plot_hw_timeline(events_df, colors, lang),
    "calendar"  = .plot_hw_calendar(daily_df, method, year, colors, lang),
    "intensity" = .plot_hw_intensity(events_df, colors, lang),
    "trend"     = .plot_hw_trend(summary_df, colors, lang)
  )

  # Convert to plotly if requested
  if (interactive) {
    p <- plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(
        hoverlabel = list(bgcolor = "white", font = list(family = "Arial", size = 12)),
        margin = list(t = 50, b = 50, l = 50, r = 50)
      ) |>
      plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
  }

  # Save plot if requested
  if (!is.null(save_plot)) {
    if (interactive) {
      rlang::check_installed("htmlwidgets", reason = "to save interactive plots")
      htmlwidgets::saveWidget(p, file = save_plot, selfcontained = TRUE)
    } else {
      ggplot2::ggsave(filename = save_plot, plot = p, width = 10, height = 6, dpi = 300)
    }
  }
  
  return(p)
}

# =============================================================================
# HELPER FUNCTIONS FOR PLOTTING
# =============================================================================

.get_hw_palette <- function(palette_name) {
  # Use utils::getFromNamespace for safety
  pal_func <- tryCatch(
    utils::getFromNamespace(paste0("pal_", palette_name), "ggsci"),
    error = function(e) {
      cli::cli_warn("Palette '{palette_name}' not found. Using 'npg' instead.")
      ggsci::pal_npg
    }
  )

  cols <- pal_func()(10)

  list(
    low     = cols[4], # Blue
    severe  = cols[2], # Orange/Yellow
    extreme = cols[1], # Red
    neutral = "#E0E0E0", # Light Gray
    text    = "#333333",
    bg      = "#FFFFFF"
  )
}

.translate_intensity_class <- function(events, labels) {
  events$intensity_class <- base::gsub("Low Intensity \\(LIHW\\)", labels$low, events$intensity_class)
  events$intensity_class <- base::gsub("Severe \\(SHW\\)", labels$severe, events$intensity_class)
  events$intensity_class <- base::gsub("Extreme \\(EHW\\)", labels$extreme, events$intensity_class)
  events$intensity_class <- base::gsub("Unknown", labels$unknown, events$intensity_class)
  events
}

.hw_theme <- function() {
  ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 16, color = "#2C3E50", margin = ggplot2::margin(b = 10)),
      plot.subtitle = ggplot2::element_text(size = 12, color = "#7F8C8D", margin = ggplot2::margin(b = 15)),
      axis.title = ggplot2::element_text(face = "bold", size = 12, color = "#34495E"),
      axis.text = ggplot2::element_text(size = 10, color = "#7F8C8D"),
      legend.title = ggplot2::element_text(face = "bold", size = 11, color = "#2C3E50"),
      legend.text = ggplot2::element_text(size = 10, color = "#34495E"),
      legend.position = "bottom",
      legend.box.background = ggplot2::element_rect(color = "#BDC3C7", linewidth = 0.5, fill = "white"),
      legend.margin = ggplot2::margin(t = 5, r = 10, b = 5, l = 10),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "#ECF0F1", linewidth = 0.5),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      strip.text = ggplot2::element_text(face = "bold", size = 11, color = "#2C3E50"),
      strip.background = ggplot2::element_rect(fill = "#ECF0F1", color = NA)
    )
}

.plot_hw_timeline <- function(events, colors, lang) {

  # Prepare labels
  labels <- switch(lang,
    en = list(title = "Heatwave Events Timeline", subtitle = "Duration and intensity of events across stations", x = "Date", y = "Station", dur = "Duration (days)", int = "Intensity", unknown = "Unknown", low = "Low Intensity (LIHW)", severe = "Severe (SHW)", extreme = "Extreme (EHW)"),
    pt = list(title = "Linha do Tempo de Ondas de Calor", subtitle = "Duracao e intensidade dos eventos por estacao", x = "Data", y = "Estacao", dur = "Duracao (dias)", int = "Intensidade", unknown = "Desconhecida", low = "Baixa Intensidade (LIHW)", severe = "Severa (SHW)", extreme = "Extrema (EHW)"),
    es = list(title = "Linea de Tiempo de Olas de Calor", subtitle = "Duracion e intensidad de los eventos por estacion", x = "Fecha", y = "Estacion", dur = "Duracion (dias)", int = "Intensidad", unknown = "Desconocida", low = "Baja Intensidad (LIHW)", severe = "Severa (SHW)", extreme = "Extrema (EHW)")
  )

  # Ensure intensity_class exists
  if (!"intensity_class" %in% names(events)) {
    events$intensity_class <- "Unknown"
  }

  # Translate intensity classes if needed
  if (lang %in% c("pt", "es")) {
    events <- .translate_intensity_class(events, labels)
  }

  # Create tooltip text
  tooltip_template <- switch(lang,
    en = "<b>%s</b><br>Method: %s<br>Start: %s<br>End: %s<br>Duration: %d days<br>Peak Temp: %.1fdegC<br>Intensity: %s",
    pt = "<b>%s</b><br>Metodo: %s<br>Inicio: %s<br>Fim: %s<br>Duracao: %d dias<br>Temp. Max: %.1fdegC<br>Intensidade: %s",
    es = "<b>%s</b><br>Metodo: %s<br>Inicio: %s<br>Fin: %s<br>Duracion: %d dias<br>Temp. Max: %.1fdegC<br>Intensidad: %s"
  )
  
  events$tooltip <- sprintf(
    tooltip_template,
    events$station_code, events$method, events$start_date, events$end_date, 
    events$duration_days, events$temp_peak, events$intensity_class
  )
  
  p <- ggplot2::ggplot(events) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = start_date, xend = end_date, 
        y = station_code, yend = station_code,
        color = intensity_class,
        linewidth = duration_days,
        text = tooltip
      ),
      alpha = 0.8
    ) +
    ggplot2::scale_color_manual(
      values = setNames(
        c(colors$low, colors$severe, colors$extreme, colors$severe),
        c(labels$low, labels$severe, labels$extreme, labels$unknown)
      ),
      name = labels$int
    ) +
    ggplot2::scale_linewidth_continuous(range = c(2, 8), name = labels$dur) +
    ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
    ggplot2::labs(title = labels$title, subtitle = labels$subtitle, x = labels$x, y = labels$y) +
    .hw_theme()

  return(p)
}

.plot_hw_calendar <- function(daily, method, year, colors, lang) {
  
  # Prepare labels
  labels <- switch(lang,
    en = list(title = "Heatwave Calendar", subtitle = "Daily occurrence of heatwaves", x = "Month", y = "Day", fill = "Heatwave", yes = "Yes", no = "No", date = "Date", tmax = "Tmax"),
    pt = list(title = "Calendario de Ondas de Calor", subtitle = "Ocorrencia diaria de ondas de calor", x = "Mes", y = "Dia", fill = "Onda de Calor", yes = "Sim", no = "Nao", date = "Data", tmax = "Tmax"),
    es = list(title = "Calendario de Olas de Calor", subtitle = "Ocurrencia diaria de olas de calor", x = "Mes", y = "Dia", fill = "Ola de Calor", yes = "Si", no = "No", date = "Fecha", tmax = "Tmax")
  )
  
  # If method is NULL, use hw_any, otherwise use specific method
  hw_col <- if (is.null(method)) "hw_any" else paste0("hw_", tolower(method[1]))
  
  if (!hw_col %in% names(daily)) {
    cli::cli_abort(sprintf("Column %s not found in daily data.", hw_col))
  }
  
  # Prepare data for calendar
  cal_data <- daily |>
    dplyr::mutate(
      year = lubridate::year(date_day),
      month = lubridate::month(date_day, label = TRUE, abbr = TRUE),
      day = lubridate::day(date_day),
      is_hw = !!rlang::sym(hw_col)
    )
    
  # If multiple years, facet by year. If specific year requested, it's already filtered.
  
  tooltip_template <- sprintf("%s: %%s<br>%s: %%.1fdegC<br>%s: %%s", labels$date, labels$tmax, labels$fill)
  
  cal_data$tooltip <- sprintf(
    tooltip_template,
    cal_data$date_day, cal_data$tmax, ifelse(cal_data$is_hw, labels$yes, labels$no)
  )
  
  p <- ggplot2::ggplot(cal_data, ggplot2::aes(x = month, y = day, fill = is_hw, text = tooltip)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = colors$extreme, "FALSE" = colors$neutral),
      labels = c("TRUE" = labels$yes, "FALSE" = labels$no),
      name = labels$fill
    ) +
    ggplot2::scale_y_reverse(breaks = 1:31) +
    ggplot2::labs(title = labels$title, subtitle = labels$subtitle, x = labels$x, y = labels$y) +
    .hw_theme() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 7)
    )
    
  if (length(unique(cal_data$year)) > 1) {
    p <- p + ggplot2::facet_wrap(~year, ncol = 3)
  }
  
  return(p)
}

.plot_hw_intensity <- function(events, colors, lang) {
  
  # Prepare labels
  labels <- switch(lang,
    en = list(title = "Heatwave Intensity vs Duration", subtitle = "Relationship between event length and peak temperature", x = "Duration (days)", y = "Peak Temperature (degC)", color = "Intensity", unknown = "Unknown", low = "Low Intensity (LIHW)", severe = "Severe (SHW)", extreme = "Extreme (EHW)"),
    pt = list(title = "Intensidade vs Duracao da Onda de Calor", subtitle = "Relacao entre a duracao do evento e a temperatura maxima", x = "Duracao (dias)", y = "Temperatura Maxima (degC)", color = "Intensidade", unknown = "Desconhecida", low = "Baixa Intensidade (LIHW)", severe = "Severa (SHW)", extreme = "Extrema (EHW)"),
    es = list(title = "Intensidad vs Duracion de la Ola de Calor", subtitle = "Relacion entre la duracion del evento y la temperatura maxima", x = "Duracion (dias)", y = "Temperatura Maxima (degC)", color = "Intensidad", unknown = "Desconocida", low = "Baja Intensidad (LIHW)", severe = "Severa (SHW)", extreme = "Extrema (EHW)")
  )
  
  if (!"intensity_class" %in% names(events)) {
    events$intensity_class <- "Unknown"
  }

  # Translate intensity classes if needed
  if (lang %in% c("pt", "es")) {
    events <- .translate_intensity_class(events, labels)
  }

  tooltip_template <- switch(lang,
    en = "<b>%s</b><br>Date: %s<br>Duration: %d days<br>Peak Temp: %.1fdegC<br>Intensity: %s",
    pt = "<b>%s</b><br>Data: %s<br>Duracao: %d dias<br>Temp. Max: %.1fdegC<br>Intensidade: %s",
    es = "<b>%s</b><br>Fecha: %s<br>Duracion: %d dias<br>Temp. Max: %.1fdegC<br>Intensidad: %s"
  )
  
  events$tooltip <- sprintf(
    tooltip_template,
    events$station_code, events$start_date, events$duration_days, events$temp_peak, events$intensity_class
  )
  
  p <- ggplot2::ggplot(events, ggplot2::aes(x = duration_days, y = temp_peak, color = intensity_class, size = duration_days, text = tooltip)) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::scale_color_manual(
      values = setNames(
        c(colors$low, colors$severe, colors$extreme, colors$severe),
        c(labels$low, labels$severe, labels$extreme, labels$unknown)
      ),
      name = labels$color
    ) +
    ggplot2::labs(title = labels$title, subtitle = labels$subtitle, x = labels$x, y = labels$y) +
    .hw_theme() +
    ggplot2::guides(size = "none")
    
  return(p)
}

.plot_hw_trend <- function(summary_df, colors, lang) {
  
  # Prepare labels
  labels <- switch(lang,
    en = list(title = "Annual Heatwave Trend", subtitle = "Number of heatwave events per year by method", x = "Year", y = "Number of Events", fill = "Method", year = "Year", events = "Events"),
    pt = list(title = "Tendencia Anual de Ondas de Calor", subtitle = "Numero de eventos de onda de calor por ano e metodo", x = "Ano", y = "Numero de Eventos", fill = "Metodo", year = "Ano", events = "Eventos"),
    es = list(title = "Tendencia Anual de Olas de Calor", subtitle = "Numero de eventos de ola de calor por ano y metodo", x = "Ano", y = "Numero de Eventos", fill = "Metodo", year = "Ano", events = "Eventos")
  )
  
  # Aggregate by year and method
  trend_data <- summary_df |>
    dplyr::group_by(year, method) |>
    dplyr::summarise(total_events = sum(n_events, na.rm = TRUE), .groups = "drop")
    
  tooltip_template <- sprintf("%s: %%d<br>%s: %%s<br>%s: %%d", labels$year, labels$fill, labels$events)
  
  trend_data$tooltip <- sprintf(
    tooltip_template,
    trend_data$year, trend_data$method, trend_data$total_events
  )
  
  p <- ggplot2::ggplot(trend_data, ggplot2::aes(x = year, y = total_events, fill = method, text = tooltip)) +
    ggplot2::geom_col(position = "dodge", alpha = 0.9) +
    ggplot2::scale_fill_manual(
      values = c(
        "EHF" = colors$extreme,
        "INMET" = colors$severe,
        "WHO" = colors$low,
        "WMO" = "#4DBBD5FF",
        "UTCI" = "#00A087FF",
        "WBGT" = "#3C5488FF",
        "HI" = "#F39B7FFF"
      ),
      name = labels$fill
    ) +
    ggplot2::labs(title = labels$title, subtitle = labels$subtitle, x = labels$x, y = labels$y) +
    .hw_theme()

  return(p)
}

# Declare NSE variables for R CMD check
utils::globalVariables(c("day", "end_date", "intensity_class", "is_hw", "month",
                         "n_events", "setNames", "tooltip", "total_events", "year"))
