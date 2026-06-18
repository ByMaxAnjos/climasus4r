#' Plot and Analyze Coldwave Events
#'
#' @description
#' Visualizes and analyzes the results from `sus_climate_compute_coldwaves()`.
#' Provides multiple visualization types including a Gantt-style timeline of
#' events, a calendar heatmap, an intensity-vs-duration scatter plot, and an
#' annual trend bar chart.
#'
#' @param cw_result List. The output from `sus_climate_compute_coldwaves()`.
#' @param type Character. Type of plot to generate:
#'   * `"timeline"`: Gantt-style timeline of coldwave events.
#'   * `"calendar"`: Calendar heatmap showing days with coldwaves.
#'   * `"intensity"`: Scatter plot of duration vs. coldest temperature.
#'   * `"trend"`: Bar chart of number of events per year.
#' @param station_code Character. Optional. Filter by a specific station code.
#' @param method Character. Optional. Filter by a specific method (e.g.,
#'   `"EHF"`, `"INMET"`).
#' @param year Numeric. Optional. Filter by a specific year (useful for
#'   calendar plots).
#' @param interactive Logical. If `TRUE`, returns an interactive Plotly chart.
#'   If `FALSE`, returns a static ggplot2 chart. Default is `TRUE`.
#' @param color_palette Character. Name of the ggsci palette to use. Default
#'   is `"npg"`. Blue tones are extracted for cold-event emphasis.
#' @param lang Character. Language for labels: `"pt"` (default), `"en"`,
#'   `"es"`.
#' @param save_plot Character. Optional file path to save the plot
#'   (e.g., `"plot.html"` or `"plot.png"`).
#'
#' @return A ggplot or plotly object.
#'
#' @examples
#' \dontrun{
#' cw <- df_indicators |> sus_climate_compute_coldwaves(method = c("WHO", "EHF"))
#' sus_climate_plot_coldwaves(cw, type = "timeline", lang = "pt")
#' sus_climate_plot_coldwaves(cw, type = "trend", interactive = FALSE)
#' }
#'
#' @seealso [sus_climate_compute_coldwaves()]
#'
#' @export
#' @importFrom dplyr filter mutate group_by summarise select
#' @importFrom lubridate year month day
#' @importFrom cli cli_abort cli_warn
#' @importFrom rlang sym
sus_climate_plot_coldwaves <- function(
    cw_result,
    type          = c("timeline", "calendar", "intensity", "trend"),
    station_code  = NULL,
    method        = NULL,
    year          = NULL,
    interactive   = TRUE,
    color_palette = "npg",
    lang          = "pt",
    save_plot     = NULL
) {
  rlang::check_installed(
    c("ggplot2", "ggsci", "lubridate", "dplyr"),
    reason = "to run sus_climate_plot_coldwaves()"
  )
  if (interactive) {
    rlang::check_installed("plotly", reason = "for interactive plots")
  }

  type <- match.arg(type)
  lang <- match.arg(lang, c("pt", "en", "es"))

  if (!is.list(cw_result) || !all(c("events", "daily", "summary") %in% names(cw_result))) {
    cli::cli_abort("cw_result must be the output list from sus_climate_compute_coldwaves().")
  }

  events_df  <- cw_result$events
  daily_df   <- cw_result$daily
  summary_df <- cw_result$summary

  # Apply filters
  if (!is.null(station_code)) {
    events_df  <- dplyr::filter(events_df,  station_code %in% !!station_code)
    daily_df   <- dplyr::filter(daily_df,   station_code %in% !!station_code)
    summary_df <- dplyr::filter(summary_df, station_code %in% !!station_code)
  }
  if (!is.null(method)) {
    events_df  <- dplyr::filter(events_df,  method %in% !!method)
    summary_df <- dplyr::filter(summary_df, method %in% !!method)
  }
  if (!is.null(year)) {
    events_df  <- dplyr::filter(events_df,  lubridate::year(start_date) %in% !!year)
    daily_df   <- dplyr::filter(daily_df,   lubridate::year(date_day)   %in% !!year)
    summary_df <- dplyr::filter(summary_df, year %in% !!year)
  }

  if (nrow(events_df) == 0) {
    cli::cli_warn("No coldwave events found after applying filters.")
    return(NULL)
  }

  colors <- .get_cw_palette(color_palette)

  p <- switch(type,
    "timeline"  = .plot_cw_timeline(events_df, colors, lang),
    "calendar"  = .plot_cw_calendar(daily_df, method, year, colors, lang),
    "intensity" = .plot_cw_intensity(events_df, colors, lang),
    "trend"     = .plot_cw_trend(summary_df, colors, lang)
  )

  if (interactive) {
    p <- plotly::ggplotly(p, tooltip = "text") |>
      plotly::layout(
        hoverlabel = list(bgcolor = "white", font = list(family = "Arial", size = 12)),
        margin     = list(t = 50, b = 50, l = 50, r = 50)
      ) |>
      plotly::config(displayModeBar = TRUE, displaylogo = FALSE)
  }

  if (!is.null(save_plot)) {
    if (interactive) {
      rlang::check_installed("htmlwidgets", reason = "to save interactive plots")
      htmlwidgets::saveWidget(p, file = save_plot, selfcontained = TRUE)
    } else {
      ggplot2::ggsave(filename = save_plot, plot = p, width = 10, height = 6, dpi = 300)
    }
  }

  p
}


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

.get_cw_palette <- function(palette_name) {
  pal_func <- tryCatch(
    utils::getFromNamespace(paste0("pal_", palette_name), "ggsci"),
    error = function(e) {
      cli::cli_warn("Palette '{palette_name}' not found. Using 'npg' instead.")
      ggsci::pal_npg
    }
  )
  cols <- pal_func()(10)
  list(
    low     = "#4DBBD5FF",  # light blue
    severe  = "#3C5488FF",  # dark blue
    extreme = "#8B1A4AFF",  # deep violet / purple
    neutral = "#E0E0E0",
    text    = "#333333",
    bg      = "#FFFFFF"
  )
}

.translate_cw_intensity_class <- function(events, labels) {
  events$intensity_class <- base::gsub("Low Intensity \\(LICW\\)", labels$low,     events$intensity_class)
  events$intensity_class <- base::gsub("Severe \\(SCW\\)",         labels$severe,  events$intensity_class)
  events$intensity_class <- base::gsub("Extreme \\(ECW\\)",        labels$extreme, events$intensity_class)
  events$intensity_class <- base::gsub("Unknown",                  labels$unknown, events$intensity_class)
  events
}

.cw_theme <- function() {
  ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 16, color = "#2C3E50",
                                            margin = ggplot2::margin(b = 10)),
      plot.subtitle = ggplot2::element_text(size = 12, color = "#7F8C8D",
                                            margin = ggplot2::margin(b = 15)),
      axis.title    = ggplot2::element_text(face = "bold", size = 12, color = "#34495E"),
      axis.text     = ggplot2::element_text(size = 10, color = "#7F8C8D"),
      legend.title  = ggplot2::element_text(face = "bold", size = 11, color = "#2C3E50"),
      legend.text   = ggplot2::element_text(size = 10, color = "#34495E"),
      legend.position      = "bottom",
      legend.box.background = ggplot2::element_rect(color = "#BDC3C7", linewidth = 0.5,
                                                     fill = "white"),
      legend.margin        = ggplot2::margin(t = 5, r = 10, b = 5, l = 10),
      panel.grid.minor     = ggplot2::element_blank(),
      panel.grid.major     = ggplot2::element_line(color = "#ECF0F1", linewidth = 0.5),
      plot.background      = ggplot2::element_rect(fill = "white", color = NA),
      panel.background     = ggplot2::element_rect(fill = "white", color = NA),
      strip.text           = ggplot2::element_text(face = "bold", size = 11, color = "#2C3E50"),
      strip.background     = ggplot2::element_rect(fill = "#ECF0F1", color = NA)
    )
}

.plot_cw_timeline <- function(events, colors, lang) {
  labels <- switch(lang,
    en = list(
      title    = "Coldwave Events Timeline",
      subtitle = "Duration and intensity of events across stations",
      x = "Date", y = "Station", dur = "Duration (days)", int = "Intensity",
      unknown = "Unknown",
      low     = "Low Intensity (LICW)",
      severe  = "Severe (SCW)",
      extreme = "Extreme (ECW)"
    ),
    pt = list(
      title    = "Linha do Tempo de Ondas de Frio",
      subtitle = "Duracao e intensidade dos eventos por estacao",
      x = "Data", y = "Estacao", dur = "Duracao (dias)", int = "Intensidade",
      unknown = "Desconhecida",
      low     = "Baixa Intensidade (LICW)",
      severe  = "Severa (SCW)",
      extreme = "Extrema (ECW)"
    ),
    es = list(
      title    = "Linea de Tiempo de Olas de Frio",
      subtitle = "Duracion e intensidad de los eventos por estacion",
      x = "Fecha", y = "Estacion", dur = "Duracion (dias)", int = "Intensidad",
      unknown = "Desconocida",
      low     = "Baja Intensidad (LICW)",
      severe  = "Severa (SCW)",
      extreme = "Extrema (ECW)"
    )
  )

  if (!"intensity_class" %in% names(events)) events$intensity_class <- "Unknown"
  if (lang %in% c("pt", "es")) events <- .translate_cw_intensity_class(events, labels)

  tooltip_template <- switch(lang,
    en = "<b>%s</b><br>Method: %s<br>Start: %s<br>End: %s<br>Duration: %d days<br>Coldest Temp: %.1f\u00b0C<br>Intensity: %s",
    pt = "<b>%s</b><br>Metodo: %s<br>Inicio: %s<br>Fim: %s<br>Duracao: %d dias<br>Temp. Min: %.1f\u00b0C<br>Intensidade: %s",
    es = "<b>%s</b><br>Metodo: %s<br>Inicio: %s<br>Fin: %s<br>Duracion: %d dias<br>Temp. Min: %.1f\u00b0C<br>Intensidad: %s"
  )
  events$tooltip <- sprintf(
    tooltip_template,
    events$station_code, events$method,
    events$start_date, events$end_date,
    events$duration_days, events$temp_peak,
    events$intensity_class
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
    ggplot2::labs(title = labels$title, subtitle = labels$subtitle,
                  x = labels$x, y = labels$y) +
    .cw_theme()

  p
}

.plot_cw_calendar <- function(daily, method, year, colors, lang) {
  labels <- switch(lang,
    en = list(
      title    = "Coldwave Calendar",
      subtitle = "Daily occurrence of coldwaves",
      x = "Month", y = "Day", fill = "Coldwave",
      yes = "Yes", no = "No", date = "Date", tmin = "Tmin"
    ),
    pt = list(
      title    = "Calendario de Ondas de Frio",
      subtitle = "Ocorrencia diaria de ondas de frio",
      x = "Mes", y = "Dia", fill = "Onda de Frio",
      yes = "Sim", no = "Nao", date = "Data", tmin = "Tmin"
    ),
    es = list(
      title    = "Calendario de Olas de Frio",
      subtitle = "Ocurrencia diaria de olas de frio",
      x = "Mes", y = "Dia", fill = "Ola de Frio",
      yes = "Si", no = "No", date = "Fecha", tmin = "Tmin"
    )
  )

  cw_col <- if (is.null(method)) "cw_any" else paste0("cw_", tolower(method[1]))
  if (!cw_col %in% names(daily)) {
    cli::cli_abort(sprintf("Column %s not found in daily data.", cw_col))
  }

  cal_data <- daily |>
    dplyr::mutate(
      year  = lubridate::year(date_day),
      month = lubridate::month(date_day, label = TRUE, abbr = TRUE),
      day   = lubridate::day(date_day),
      is_cw = !!rlang::sym(cw_col)
    )

  tooltip_template <- sprintf(
    "%s: %%s<br>%s: %%.1f\u00b0C<br>%s: %%s",
    labels$date, labels$tmin, labels$fill
  )
  cal_data$tooltip <- sprintf(
    tooltip_template,
    cal_data$date_day,
    cal_data$tmin,
    ifelse(cal_data$is_cw, labels$yes, labels$no)
  )

  p <- ggplot2::ggplot(
      cal_data,
      ggplot2::aes(x = month, y = day, fill = is_cw, text = tooltip)
    ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::scale_fill_manual(
      values = c("TRUE" = colors$extreme, "FALSE" = colors$neutral),
      labels = c("TRUE" = labels$yes, "FALSE" = labels$no),
      name   = labels$fill
    ) +
    ggplot2::scale_y_reverse(breaks = 1:31) +
    ggplot2::labs(title = labels$title, subtitle = labels$subtitle,
                  x = labels$x, y = labels$y) +
    .cw_theme() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      axis.text.y      = ggplot2::element_text(size = 7)
    )

  if (length(unique(cal_data$year)) > 1) {
    p <- p + ggplot2::facet_wrap(~year, ncol = 3)
  }

  p
}

.plot_cw_intensity <- function(events, colors, lang) {
  labels <- switch(lang,
    en = list(
      title    = "Coldwave Intensity vs Duration",
      subtitle = "Relationship between event length and coldest temperature",
      x = "Duration (days)", y = "Coldest Temperature (\u00b0C)",
      color = "Intensity",
      unknown = "Unknown",
      low     = "Low Intensity (LICW)",
      severe  = "Severe (SCW)",
      extreme = "Extreme (ECW)"
    ),
    pt = list(
      title    = "Intensidade vs Duracao da Onda de Frio",
      subtitle = "Relacao entre a duracao do evento e a temperatura minima",
      x = "Duracao (dias)", y = "Temperatura Minima (\u00b0C)",
      color = "Intensidade",
      unknown = "Desconhecida",
      low     = "Baixa Intensidade (LICW)",
      severe  = "Severa (SCW)",
      extreme = "Extrema (ECW)"
    ),
    es = list(
      title    = "Intensidad vs Duracion de la Ola de Frio",
      subtitle = "Relacion entre la duracion del evento y la temperatura minima",
      x = "Duracion (dias)", y = "Temperatura Minima (\u00b0C)",
      color = "Intensidad",
      unknown = "Desconocida",
      low     = "Baja Intensidad (LICW)",
      severe  = "Severa (SCW)",
      extreme = "Extrema (ECW)"
    )
  )

  if (!"intensity_class" %in% names(events)) events$intensity_class <- "Unknown"
  if (lang %in% c("pt", "es")) events <- .translate_cw_intensity_class(events, labels)

  tooltip_template <- switch(lang,
    en = "<b>%s</b><br>Date: %s<br>Duration: %d days<br>Coldest Temp: %.1f\u00b0C<br>Intensity: %s",
    pt = "<b>%s</b><br>Data: %s<br>Duracao: %d dias<br>Temp. Min: %.1f\u00b0C<br>Intensidade: %s",
    es = "<b>%s</b><br>Fecha: %s<br>Duracion: %d dias<br>Temp. Min: %.1f\u00b0C<br>Intensidad: %s"
  )
  events$tooltip <- sprintf(
    tooltip_template,
    events$station_code, events$start_date,
    events$duration_days, events$temp_peak,
    events$intensity_class
  )

  p <- ggplot2::ggplot(
      events,
      ggplot2::aes(
        x = duration_days, y = temp_peak,
        color = intensity_class, size = duration_days,
        text = tooltip
      )
    ) +
    ggplot2::geom_point(alpha = 0.7) +
    ggplot2::scale_color_manual(
      values = setNames(
        c(colors$low, colors$severe, colors$extreme, colors$severe),
        c(labels$low, labels$severe, labels$extreme, labels$unknown)
      ),
      name = labels$color
    ) +
    # Lower temp_peak = more extreme cold; reverse so visually higher = colder
    ggplot2::scale_y_reverse() +
    ggplot2::labs(title = labels$title, subtitle = labels$subtitle,
                  x = labels$x, y = labels$y) +
    .cw_theme() +
    ggplot2::guides(size = "none")

  p
}

.plot_cw_trend <- function(summary_df, colors, lang) {
  labels <- switch(lang,
    en = list(
      title    = "Annual Coldwave Trend",
      subtitle = "Number of coldwave events per year by method",
      x = "Year", y = "Number of Events", fill = "Method",
      year = "Year", events = "Events"
    ),
    pt = list(
      title    = "Tendencia Anual de Ondas de Frio",
      subtitle = "Numero de eventos de onda de frio por ano e metodo",
      x = "Ano", y = "Numero de Eventos", fill = "Metodo",
      year = "Ano", events = "Eventos"
    ),
    es = list(
      title    = "Tendencia Anual de Olas de Frio",
      subtitle = "Numero de eventos de ola de frio por ano y metodo",
      x = "Ano", y = "Numero de Eventos", fill = "Metodo",
      year = "Ano", events = "Eventos"
    )
  )

  trend_data <- summary_df |>
    dplyr::group_by(year, method) |>
    dplyr::summarise(total_events = sum(n_events, na.rm = TRUE), .groups = "drop")

  tooltip_template <- sprintf(
    "%s: %%d<br>%s: %%s<br>%s: %%d",
    labels$year, labels$fill, labels$events
  )
  trend_data$tooltip <- sprintf(
    tooltip_template,
    trend_data$year, trend_data$method, trend_data$total_events
  )

  p <- ggplot2::ggplot(
      trend_data,
      ggplot2::aes(x = year, y = total_events, fill = method, text = tooltip)
    ) +
    ggplot2::geom_col(position = "dodge", alpha = 0.9) +
    ggplot2::scale_fill_manual(
      values = c(
        "EHF"   = colors$extreme,
        "INMET" = colors$severe,
        "WHO"   = colors$low,
        "WMO"   = "#00A087FF",
        "UTCI"  = "#4DBBD5FF",
        "WBGT"  = "#8B1A4AFF",
        "HI"    = "#F39B7FFF"
      ),
      name = labels$fill
    ) +
    ggplot2::labs(title = labels$title, subtitle = labels$subtitle,
                  x = labels$x, y = labels$y) +
    .cw_theme()

  p
}

# Declare NSE variables for R CMD check
utils::globalVariables(c(
  "day", "end_date", "intensity_class", "is_cw", "month",
  "n_events", "total_days_cw", "tooltip", "total_events", "year",
  "start_date", "duration_days", "temp_peak", "station_code",
  "date_day", "tmin", "method"
))
