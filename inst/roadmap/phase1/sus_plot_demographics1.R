#' Visualize Demographic Profile of Filtered SUS/SINAN Data
#'
#' Produces tables, maps, or charts summarizing the demographic composition of a
#' filtered climasus_df dataset. Designed to work directly after
#' `sus_data_filter_demographics()` using SINAN dengue notification data.
#'
#' @param df A climasus_df object (output from `sus_data_filter_demographics()`).
#' @param type Character string. One or more of:
#'   `"table"`, `"map"`, `"bar"`, `"pyramid"`, `"temporal"`, `"risk"`,
#'   `"dashboard"`. Default is `"table"`.
#'   - `"table"`: Frequency table of one or all demographic variables.
#'   - `"map"`: Choropleth map by state or municipality.
#'   - `"bar"`: Horizontal bar chart for a single variable.
#'   - `"pyramid"`: Classic population pyramid by age and sex.
#'   - `"temporal"`: Case counts over time (week, month, year).
#'   - `"risk"`: Distribution by `climate_risk_group`.
#'   - `"dashboard"`: Composite of all available outputs (saved as HTML/PNG).
#' @param var Character string specifying the demographic variable to visualize
#'   (used for `"table"` and `"bar"`).
#'   Options: `"sex"`, `"race"`, `"age_group"`, `"ibge_age_group"`,
#'   `"education"`, `"gestation_status"`, `"climate_risk_group"`,
#'   `"astronomical_season"`, `"notification_uf"`, `"quarter"`, `"semester"`.
#'   If `NULL` (default), shows all available variables (for `"table"`) or
#'   picks the most informative variable automatically.
#' @param time_var Character string for temporal axis in `"temporal"` type.
#'   Options: `"notification_date"`, `"epidemiological_week"`, `"month"`,
#'   `"quarter"`, `"year"`. Default: `"notification_date"`.
#' @param color_by Character string. Secondary grouping variable for color
#'   in temporal and bar charts. Example: `"sex"`, `"race"`, `"age_group"`.
#' @param map_level Character string. `"state"` (default) or `"municipality"`.
#' @param palette Character string. `"sus"` (default package palette),
#'   or any `RColorBrewer` palette name, or `"viridis"`.
#' @param interactive Logical. If `TRUE`, returns interactive `plotly`/`leaflet`
#'   outputs. If `FALSE` (default), returns `ggplot2`/`gt` static outputs.
#' @param lang Character. `"en"` (default), `"pt"`, or `"es"`.
#' @param top_n Integer. For bar charts, limits to top N categories. Default: 15.
#' @param save_path Character string. Path to save output. For `"dashboard"`,
#'   saves HTML via `htmltools`. For individual plots, saves PNG via `ggplot2::ggsave()`.
#'   If `NULL` (default), does not save.
#' @param width,height Numeric. Plot dimensions in inches for saving. Default: 10 x 7.
#' @param ... Additional arguments passed to underlying plot functions.
#'
#' @return Invisibly returns the plot, table, or list of outputs (for dashboard).
#'   Renders output as a side effect.
#'
#' @examples
#' \dontrun{
#' df_filtered <- sus_data_filter_demographics(df, sex = "Female", age_range = c(15, 49))
#'
#' # Frequency table of all demographics
#' sus_view_demographics(df_filtered, type = "table")
#'
#' # Bar chart: race distribution
#' sus_view_demographics(df_filtered, type = "bar", var = "race")
#'
#' # Population pyramid
#' sus_view_demographics(df_filtered, type = "pyramid")
#'
#' # Choropleth map by state
#' sus_view_demographics(df_filtered, type = "map", map_level = "state", interactive = TRUE)
#'
#' # Epidemic curve colored by sex
#' sus_view_demographics(df_filtered, type = "temporal",
#'                        time_var = "epidemiological_week", color_by = "sex")
#'
#' # Climate risk group distribution
#' sus_view_demographics(df_filtered, type = "risk")
#'
#' # Full dashboard saved as HTML
#' sus_view_demographics(df_filtered, type = "dashboard",
#'                        interactive = TRUE, save_path = "dengue_profile.html")
#' }
#'
#' @export
sus_view_demographics <- function(df,
                                   type        = "table",
                                   var         = NULL,
                                   time_var    = "notification_date",
                                   color_by    = NULL,
                                   map_level   = "state",
                                   palette     = "sus",
                                   interactive = FALSE,
                                   lang        = "pt",
                                   top_n       = 15L,
                                   save_path   = NULL,
                                   width       = 10,
                                   height      = 7,
                                   ...) {

  # ── 0. Validation ────────────────────────────────────────────────────────
  if (!is.data.frame(df)) cli::cli_abort("df must be a data frame.")
  if (!lang %in% c("en", "pt", "es")) cli::cli_abort("lang must be 'en', 'pt', or 'es'.")

  valid_types <- c("table","map","bar","pyramid","temporal","risk","dashboard")
  bad <- setdiff(type, valid_types)
  if (length(bad) > 0) {
    cli::cli_abort(c("Unknown type(s): {.val {bad}}",
                     "i" = "Valid: {.val {valid_types}}"))
  }

  data.table::setDT(df)

  # Extract filter summary if available
  fsummary <- attr(df, "filter_summary")

  # ── 1. Detect available columns ──────────────────────────────────────────
  .avail <- .detect_sinan_cols(df)

  # ── 2. Dispatch ──────────────────────────────────────────────────────────
  if ("dashboard" %in% type) {
    out <- .view_dashboard_sinan(df, .avail, palette, interactive,
                                  lang, fsummary, save_path, width, height, ...)
    return(invisible(out))
  }

  out_list <- lapply(type, function(tp) {
    switch(tp,
      "table"    = .view_table_sinan(df, var, .avail, lang, interactive, top_n),
      "map"      = .view_map_sinan(df, .avail, map_level, palette, lang, interactive),
      "bar"      = .view_bar_sinan(df, var, .avail, color_by, palette, lang, interactive, top_n),
      "pyramid"  = .view_pyramid_sinan(df, .avail, palette, lang, interactive),
      "temporal" = .view_temporal_sinan(df, time_var, color_by, .avail, palette, lang, interactive),
      "risk"     = .view_risk_sinan(df, .avail, palette, lang, interactive)
    )
  })

  if (!is.null(save_path)) {
    .save_views(out_list, type, save_path, width, height, lang)
  }

  if (length(out_list) == 1) {
    print(out_list[[1]])
    return(invisible(out_list[[1]]))
  }

  invisible(out_list)
}


# ════════════════════════════════════════════════════════════════════════════
# TABLE
# ════════════════════════════════════════════════════════════════════════════
.view_table_sinan <- function(df, var, avail, lang, interactive, top_n) {

  # Columns to summarise (all if var = NULL)
  candidate_vars <- list(
    sex               = avail$sex,
    race              = avail$race,
    age_group         = avail$age_group,
    ibge_age_group    = avail$ibge_age_group,
    education         = avail$education,
    gestation_status  = avail$gestation,
    climate_risk_group = avail$climate_risk,
    astronomical_season = avail$season,
    notification_uf   = avail$notif_uf
  )

  if (!is.null(var)) {
    candidate_vars <- candidate_vars[intersect(var, names(candidate_vars))]
    if (length(candidate_vars) == 0) {
      cli::cli_abort("var '{var}' not recognized or column not found.")
    }
  }

  # Build summary table
  rows <- data.table::rbindlist(lapply(names(candidate_vars), function(dim) {
    col <- candidate_vars[[dim]]
    if (is.null(col)) return(NULL)
    dt <- df[!is.na(get(col)), .N, by = .(category = get(col))]
    dt[, `:=`(variable = dim, pct = round(100 * N / nrow(df), 1))]
    dt[order(-N)]
  }), fill = TRUE)

  if (nrow(rows) == 0) {
    cli::cli_alert_warning("No data available for table.")
    return(NULL)
  }

  data.table::setnames(rows, "N", .lbl("count", lang))
  data.table::setnames(rows, "pct", .lbl("percent", lang))

  if (interactive) {
    return(DT::datatable(
      rows,
      filter  = "top",
      options = list(pageLength = 20, scrollX = TRUE),
      caption = htmltools::tags$caption(
        style = "font-weight:bold; font-size:1.1em;",
        .lbl("demographic_summary", lang)
      )
    ))
  }

  # gt static table
  rows |>
    as.data.frame() |>
    gt::gt(groupname_col = "variable") |>
    gt::tab_header(
      title    = gt::md(paste0("**", .lbl("demographic_summary", lang), "**")),
      subtitle = gt::md(paste0("n = ", scales::comma(nrow(df)), " ", .lbl("records", lang)))
    ) |>
    gt::fmt_number(columns = .lbl("count", lang), decimals = 0, use_seps = TRUE) |>
    gt::fmt_number(columns = .lbl("percent", lang), decimals = 1, suffix = "%") |>
    gt::data_color(columns = .lbl("percent", lang),
                   palette = c("#f7fbff", "#08519c")) |>
    gt::cols_width(category ~ gt::px(180)) |>
    gt::tab_source_note(.lbl("source", lang)) |>
    gt::opt_row_striping() |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_row_groups()
    )
}


# ════════════════════════════════════════════════════════════════════════════
# MAP
# ════════════════════════════════════════════════════════════════════════════
.view_map_sinan <- function(df, avail, map_level, palette, lang, interactive) {

  pal_colors <- .get_palette(palette, type = "sequential")

  if (map_level == "municipality") {
    geo_col <- avail$muni_code
    if (is.null(geo_col)) {
      cli::cli_alert_warning("Municipality code not found. Falling back to state map.")
      map_level <- "state"
    }
  }

  if (map_level == "state") {
    geo_col <- avail$notif_uf %||% avail$res_uf
    if (is.null(geo_col)) cli::cli_abort("No UF column found for map.")

    count_dt <- df[!is.na(get(geo_col)), .N, by = .(uf_code = as.integer(get(geo_col)))]

    if (!requireNamespace("geobr", quietly = TRUE)) {
      cli::cli_abort("Package 'geobr' required for maps. Install with: install.packages('geobr')")
    }

    shape <- geobr::read_state(year = 2020, showProgress = FALSE)
    map_data <- dplyr::left_join(shape, count_dt,
                                  by = c("code_state" = "uf_code"))
    fill_col  <- "N"
    label_col <- "abbrev_state"

  } else {
    count_dt <- df[!is.na(get(geo_col)), .N,
                   by = .(muni_code = as.integer(substr(as.character(get(geo_col)), 1, 6)))]

    shape <- geobr::read_municipality(year = 2020, showProgress = FALSE)
    map_data <- dplyr::left_join(shape, count_dt,
                                  by = c("code_muni" = "muni_code"))
    fill_col  <- "N"
    label_col <- "name_muni"
  }

  if (interactive) {
    if (!requireNamespace("leaflet", quietly = TRUE)) {
      cli::cli_abort("Package 'leaflet' required for interactive maps.")
    }
    pal_fn <- leaflet::colorNumeric(pal_colors, domain = map_data[[fill_col]],
                                     na.color = "#e0e0e0")
    out <- leaflet::leaflet(map_data) |>
      leaflet::addProviderTiles("CartoDB.Positron") |>
      leaflet::addPolygons(
        fillColor   = ~pal_fn(get(fill_col)),
        fillOpacity = 0.75,
        color       = "white",
        weight      = 0.8,
        label       = ~paste0(get(label_col), ": ", scales::comma(get(fill_col) %||% 0)),
        highlightOptions = leaflet::highlightOptions(
          weight = 2, color = "#444", bringToFront = TRUE
        )
      ) |>
      leaflet::addLegend(
        pal    = pal_fn,
        values = ~get(fill_col),
        title  = .lbl("n_cases", lang),
        labFormat = leaflet::labelFormat(big.mark = ",")
      )
    return(out)
  }

  # ggplot2 static
  ggplot2::ggplot(map_data) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[fill_col]]),
                     color = "white", linewidth = 0.25) +
    ggplot2::scale_fill_gradientn(
      colors   = pal_colors,
      na.value = "#e0e0e0",
      name     = .lbl("n_cases", lang),
      labels   = scales::comma
    ) +
    ggplot2::labs(
      title   = .lbl("map_title", lang),
      caption = .lbl("source", lang)
    ) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold", size = 14, hjust = 0),
      legend.position    = "right",
      plot.caption       = ggplot2::element_text(color = "gray50", size = 8)
    )
}


# ════════════════════════════════════════════════════════════════════════════
# BAR CHART
# ════════════════════════════════════════════════════════════════════════════
.view_bar_sinan <- function(df, var, avail, color_by, palette, lang, interactive, top_n) {

  # Auto-select variable if not specified
  if (is.null(var)) {
    var <- "race"
    cli::cli_alert_info("var not specified. Defaulting to 'race'.")
  }

  col_map <- list(
    sex = avail$sex, race = avail$race, age_group = avail$age_group,
    ibge_age_group = avail$ibge_age_group, education = avail$education,
    gestation_status = avail$gestation, climate_risk_group = avail$climate_risk,
    astronomical_season = avail$season, notification_uf = avail$notif_uf,
    quarter = avail$quarter, semester = avail$semester
  )

  col <- col_map[[var]]
  if (is.null(col)) cli::cli_abort("Column for {.val {var}} not found in data.")

  # Build bar data
  if (!is.null(color_by)) {
    cb_col <- col_map[[color_by]]
    if (is.null(cb_col)) {
      cli::cli_alert_warning("color_by column '{color_by}' not found. Ignoring.")
      color_by <- NULL
    }
  }

  if (is.null(color_by)) {
    bar_dt <- df[!is.na(get(col)), .N, by = .(category = as.character(get(col)))]
    bar_dt[, pct := round(100 * N / sum(N), 1)]
    bar_dt <- utils::head(bar_dt[order(-N)], top_n)

    pal <- .get_palette(palette, n = nrow(bar_dt), type = "qualitative")

    p <- ggplot2::ggplot(bar_dt,
        ggplot2::aes(x = stats::reorder(category, N), y = N, fill = category)) +
      ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(pct, "%")),
                         hjust = -0.15, size = 3.4, color = "gray30") +
      ggplot2::scale_fill_manual(values = pal) +
      ggplot2::scale_y_continuous(
        labels = scales::comma,
        expand = ggplot2::expansion(mult = c(0, 0.18))
      ) +
      ggplot2::coord_flip()

  } else {
    cb_col_name <- col_map[[color_by]]
    bar_dt <- df[!is.na(get(col)) & !is.na(get(cb_col_name)),
                 .N,
                 by = .(category = as.character(get(col)),
                        color_var = as.character(get(cb_col_name)))]
    bar_dt[, pct := round(100 * N / sum(N), 1), by = color_var]

    pal <- .get_palette(palette, n = length(unique(bar_dt$color_var)), type = "qualitative")

    p <- ggplot2::ggplot(bar_dt,
        ggplot2::aes(x = stats::reorder(category, N), y = N, fill = color_var)) +
      ggplot2::geom_col(position = "dodge", width = 0.72) +
      ggplot2::scale_fill_manual(values = pal, name = color_by) +
      ggplot2::scale_y_continuous(labels = scales::comma,
                                   expand = ggplot2::expansion(mult = c(0, 0.1))) +
      ggplot2::coord_flip()
  }

  p <- p +
    ggplot2::labs(
      title   = paste0(.lbl("bar_title", lang), ": ", var),
      x       = NULL,
      y       = .lbl("count", lang),
      caption = .lbl("source", lang)
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      plot.title         = ggplot2::element_text(face = "bold", size = 14),
      axis.text.y        = ggplot2::element_text(size = 11),
      plot.caption       = ggplot2::element_text(color = "gray50", size = 8)
    )

  if (interactive) return(plotly::ggplotly(p, tooltip = c("x", "y")))
  p
}


# ════════════════════════════════════════════════════════════════════════════
# POPULATION PYRAMID
# ════════════════════════════════════════════════════════════════════════════
.view_pyramid_sinan <- function(df, avail, palette, lang, interactive) {

  age_col <- avail$ibge_age_group %||% avail$age_group
  sex_col <- avail$sex

  if (is.null(age_col) || is.null(sex_col)) {
    cli::cli_abort(c(
      "Pyramid requires age group and sex columns.",
      "i" = "Run sus_create_variables() to create 'age_group' and 'ibge_age_group'."
    ))
  }

  pyr_dt <- df[!is.na(get(age_col)) & !is.na(get(sex_col)),
               .N,
               by = .(age_group = as.character(get(age_col)),
                      sex       = as.character(get(sex_col)))]

  # Normalize sex labels
  male_lbl   <- .lbl("male", lang)
  female_lbl <- .lbl("female", lang)

  pyr_dt[sex %in% c("Male","Masculino","Masculino"),   sex_norm := male_lbl]
  pyr_dt[sex %in% c("Female","Feminino","Femenino"),   sex_norm := female_lbl]
  pyr_dt <- pyr_dt[!is.na(sex_norm)]

  pyr_dt[sex_norm == male_lbl,   n_plot := -N]
  pyr_dt[sex_norm == female_lbl, n_plot :=  N]

  max_n  <- max(abs(pyr_dt$n_plot), na.rm = TRUE)
  breaks <- pretty(c(-max_n, max_n), n = 6)

  # Ordered age groups — respect factor levels if present
  if (is.factor(df[[age_col]])) {
    lvls <- levels(df[[age_col]])
  } else {
    lvls <- sort(unique(pyr_dt$age_group))
  }
  pyr_dt[, age_group := factor(age_group, levels = lvls)]

  colors <- c(male_lbl = "#1B6CA8", female_lbl = "#D94F3D")
  names(colors) <- c(male_lbl, female_lbl)

  p <- ggplot2::ggplot(pyr_dt,
      ggplot2::aes(x = age_group, y = n_plot, fill = sex_norm)) +
    ggplot2::geom_col(width = 0.88) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      breaks = breaks,
      labels = scales::comma(abs(breaks))
    ) +
    ggplot2::scale_fill_manual(values = colors, name = NULL) +
    ggplot2::geom_hline(yintercept = 0, color = "white", linewidth = 0.5) +
    ggplot2::annotate("text", x = Inf, y = -max_n * 0.5, label = male_lbl,
                      hjust = 0.5, vjust = -0.5, fontface = "bold",
                      color = "#1B6CA8", size = 4) +
    ggplot2::annotate("text", x = Inf, y =  max_n * 0.5, label = female_lbl,
                      hjust = 0.5, vjust = -0.5, fontface = "bold",
                      color = "#D94F3D", size = 4) +
    ggplot2::labs(
      title   = .lbl("pyramid_title", lang),
      x       = .lbl("age_group_lbl", lang),
      y       = .lbl("count", lang),
      caption = .lbl("source", lang)
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      plot.title      = ggplot2::element_text(face = "bold", size = 14),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.caption    = ggplot2::element_text(color = "gray50", size = 8)
    )

  if (interactive) return(plotly::ggplotly(p))
  p
}


# ════════════════════════════════════════════════════════════════════════════
# TEMPORAL / EPIDEMIC CURVE
# ════════════════════════════════════════════════════════════════════════════
.view_temporal_sinan <- function(df, time_var, color_by, avail, palette, lang, interactive) {

  # Validate / auto-select time variable
  time_candidates <- c("notification_date", "epidemiological_week",
                        "month", "quarter", "year",
                        "first_symptoms_date", "investigation_date")

  if (!time_var %in% names(df)) {
    found <- Find(function(x) x %in% names(df), time_candidates)
    if (is.null(found)) cli::cli_abort("No temporal column found.")
    time_var <- found
    cli::cli_alert_info("time_var auto-detected: {.field {time_var}}")
  }

  col_map <- list(
    sex = avail$sex, race = avail$race, age_group = avail$age_group,
    ibge_age_group = avail$ibge_age_group, climate_risk_group = avail$climate_risk,
    astronomical_season = avail$season
  )

  # Aggregate
  if (!is.null(color_by) && color_by %in% names(col_map) && !is.null(col_map[[color_by]])) {
    cb_col <- col_map[[color_by]]
    ts_dt  <- df[!is.na(get(time_var)) & !is.na(get(cb_col)),
                 .N,
                 by = .(time_val  = get(time_var),
                        color_val = as.character(get(cb_col)))]
    pal <- .get_palette(palette, n = length(unique(ts_dt$color_val)), type = "qualitative")

    p <- ggplot2::ggplot(ts_dt,
        ggplot2::aes(x = time_val, y = N, color = color_val, group = color_val)) +
      ggplot2::geom_line(linewidth = 0.8, alpha = 0.85) +
      ggplot2::scale_color_manual(values = pal, name = color_by)

  } else {
    ts_dt <- df[!is.na(get(time_var)), .N, by = .(time_val = get(time_var))]

    p <- ggplot2::ggplot(ts_dt, ggplot2::aes(x = time_val, y = N)) +
      ggplot2::geom_col(fill = "#1B6CA8", alpha = 0.8, width = 0.85) +
      ggplot2::geom_smooth(method = "loess", se = TRUE, color = "#D94F3D",
                           fill = "#D94F3D", alpha = 0.15, linewidth = 0.9,
                           formula = y ~ x)
  }

  # Format x-axis based on column type
  if (inherits(df[[time_var]], "Date")) {
    p <- p + ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "3 months")
  } else {
    p <- p + ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
  }

  p <- p +
    ggplot2::scale_y_continuous(labels = scales::comma,
                                 expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(
      title   = .lbl("temporal_title", lang),
      subtitle = paste0(.lbl("time_var_lbl", lang), ": ", time_var),
      x       = NULL,
      y       = .lbl("n_cases", lang),
      caption = .lbl("source", lang)
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", size = 14),
      axis.text.x   = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.minor = ggplot2::element_blank(),
      plot.caption  = ggplot2::element_text(color = "gray50", size = 8)
    )

  if (interactive) return(plotly::ggplotly(p))
  p
}


# ════════════════════════════════════════════════════════════════════════════
# CLIMATE RISK GROUP
# ════════════════════════════════════════════════════════════════════════════
.view_risk_sinan <- function(df, avail, palette, lang, interactive) {

  risk_col <- avail$climate_risk
  if (is.null(risk_col)) {
    cli::cli_abort("'climate_risk_group' column not found in data.")
  }

  # Cross risk group with age group and sex
  age_col <- avail$age_group
  sex_col <- avail$sex

  risk_dt <- df[!is.na(get(risk_col)), .N, by = .(
    risk  = as.character(get(risk_col)),
    age   = if (!is.null(age_col)) as.character(get(age_col)) else "All",
    sex   = if (!is.null(sex_col)) as.character(get(sex_col)) else "All"
  )]
  risk_dt[, pct := round(100 * N / sum(N), 1)]

  pal <- .get_palette(palette, n = length(unique(risk_dt$sex)), type = "qualitative")

  p <- ggplot2::ggplot(risk_dt,
      ggplot2::aes(x = stats::reorder(risk, N), y = N, fill = sex)) +
    ggplot2::geom_col(position = "stack", width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~age, scales = "free_x", ncol = 2) +
    ggplot2::scale_fill_manual(values = pal, name = .lbl("sex_lbl", lang)) +
    ggplot2::scale_y_continuous(labels = scales::comma,
                                 expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::labs(
      title   = .lbl("risk_title", lang),
      x       = NULL,
      y       = .lbl("count", lang),
      caption = .lbl("source", lang)
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      strip.text      = ggplot2::element_text(face = "bold"),
      plot.title      = ggplot2::element_text(face = "bold", size = 14),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.caption    = ggplot2::element_text(color = "gray50", size = 8)
    )

  if (interactive) return(plotly::ggplotly(p))
  p
}


# ════════════════════════════════════════════════════════════════════════════
# DASHBOARD
# ════════════════════════════════════════════════════════════════════════════
.view_dashboard_sinan <- function(df, avail, palette, interactive, lang,
                                   fsummary, save_path, width, height, ...) {
  cli::cli_alert_info("Building demographic dashboard...")

  parts <- list(
    table    = tryCatch(.view_table_sinan(df, NULL, avail, lang, FALSE, 20), error = function(e) { cli::cli_alert_warning("table: {e$message}"); NULL }),
    map      = tryCatch(.view_map_sinan(df, avail, "state", palette, lang, FALSE), error = function(e) { cli::cli_alert_warning("map: {e$message}"); NULL }),
    pyramid  = tryCatch(.view_pyramid_sinan(df, avail, palette, lang, FALSE), error = function(e) { cli::cli_alert_warning("pyramid: {e$message}"); NULL }),
    temporal = tryCatch(.view_temporal_sinan(df, "notification_date", "sex", avail, palette, lang, FALSE), error = function(e) { cli::cli_alert_warning("temporal: {e$message}"); NULL }),
    bar_race = tryCatch(.view_bar_sinan(df, "race", avail, NULL, palette, lang, FALSE, 10), error = function(e) { cli::cli_alert_warning("bar_race: {e$message}"); NULL }),
    risk     = tryCatch(.view_risk_sinan(df, avail, palette, lang, FALSE), error = function(e) { cli::cli_alert_warning("risk: {e$message}"); NULL })
  )

  gg_parts <- Filter(function(x) inherits(x, "ggplot"), parts)

  if (length(gg_parts) >= 2) {
    combined <- patchwork::wrap_plots(gg_parts, ncol = 2) +
      patchwork::plot_annotation(
        title    = .lbl("dashboard_title", lang),
        subtitle = if (!is.null(fsummary)) {
          paste0(
            scales::comma(fsummary$n_retained), " / ",
            scales::comma(fsummary$n_original), " ", .lbl("records", lang),
            " (", fsummary$pct, "%)"
          )
        } else NULL,
        caption = .lbl("source", lang),
        theme   = ggplot2::theme(
          plot.title    = ggplot2::element_text(face = "bold", size = 18),
          plot.subtitle = ggplot2::element_text(color = "gray40", size = 12),
          plot.caption  = ggplot2::element_text(color = "gray50", size = 9)
        )
      )
    print(combined)

    if (!is.null(save_path) && grepl("\\.png$", save_path, ignore.case = TRUE)) {
      ggplot2::ggsave(save_path, combined, width = width * 2, height = height * 2, dpi = 150)
      cli::cli_alert_success("Dashboard saved: {.file {save_path}}")
    }
  }

  if (!is.null(parts$table)) print(parts$table)

  if (!is.null(save_path) && grepl("\\.html$", save_path, ignore.case = TRUE)) {
    .save_dashboard_html(parts, fsummary, lang, save_path)
  }

  invisible(parts)
}


# ════════════════════════════════════════════════════════════════════════════
# HELPERS
# ════════════════════════════════════════════════════════════════════════════

#' @keywords internal
.detect_sinan_cols <- function(df) {
  fc <- function(...) find_column(df, c(...))
  list(
    sex          = fc("sex", "sexo", "SEXO"),
    race         = fc("race", "raca", "RACACOR"),
    age_years    = fc("age_years", "idade", "IDADE"),
    age_group    = fc("age_group", "faixa_etaria"),
    ibge_age_group = fc("ibge_age_group", "faixa_etaria_ibge"),
    climate_risk = fc("climate_risk_group", "grupo_risco_climatico"),
    education    = fc("education_level", "education", "escolaridade", "ESC"),
    gestation    = fc("gestation_status", "gestante", "CS_GESTANT"),
    season       = fc("astronomical_season", "estacao_astronomica"),
    notif_uf     = fc("notification_uf", "UF_ZI"),
    res_uf       = fc("residence_uf", "uf_residencia"),
    muni_code    = fc("residence_municipality_code", "notification_municipality_code",
                      "CODMUNRES", "municipality_code"),
    notif_date   = fc("notification_date", "data_notificacao", "DT_NOTIFIC"),
    epi_week     = fc("epidemiological_week", "semana_epidemiologica"),
    year         = fc("year", "ano"),
    month        = fc("month", "mes"),
    quarter      = fc("quarter", "trimestre"),
    semester     = fc("semester", "semestre")
  )
}

#' @keywords internal
.get_palette <- function(palette, n = 8, type = "qualitative") {
  sus_qual <- c("#1B6CA8","#D94F3D","#3BB273","#F4A261",
                "#7B2D8B","#2EC4B6","#FF9F1C","#555F61",
                "#E76F51","#264653","#A8DADC","#457B9D")
  sus_seq  <- c("#f7fbff","#deebf7","#c6dbef","#9ecae1",
                "#6baed6","#4292c6","#2171b5","#084594")

  if (palette == "sus") {
    return(if (type == "sequential") sus_seq else sus_qual[seq_len(min(n, length(sus_qual)))])
  }
  if (palette == "viridis") {
    return(viridisLite::viridis(n))
  }
  # RColorBrewer
  if (requireNamespace("RColorBrewer", quietly = TRUE)) {
    max_n <- RColorBrewer::brewer.pal.info[palette, "maxcolors"]
    return(RColorBrewer::brewer.pal(min(n, max_n), palette))
  }
  sus_qual[seq_len(min(n, length(sus_qual)))]
}

#' @keywords internal
.save_views <- function(out_list, types, save_path, width, height, lang) {
  for (i in seq_along(out_list)) {
    obj <- out_list[[i]]
    tp  <- types[i]
    fp  <- sub("(\\.\\w+)$", paste0("_", tp, "\\1"), save_path)
    if (inherits(obj, "ggplot")) {
      ggplot2::ggsave(fp, obj, width = width, height = height, dpi = 150)
      cli::cli_alert_success("Saved {tp}: {.file {fp}}")
    }
  }
}

#' @keywords internal
.save_dashboard_html <- function(parts, fsummary, lang, save_path) {
  if (!requireNamespace("htmltools", quietly = TRUE)) {
    cli::cli_alert_warning("'htmltools' required for HTML export. Skipping.")
    return(invisible(NULL))
  }
  # Convert gt table to HTML if present
  tbl_html <- if (!is.null(parts$table) && inherits(parts$table, "gt_tbl")) {
    gt::as_raw_html(parts$table)
  } else ""

  header <- if (!is.null(fsummary)) {
    paste0("<p style='color:gray'>n = ", scales::comma(fsummary$n_retained),
           " (", fsummary$pct, "%)</p>")
  } else ""

  html_out <- htmltools::tagList(
    htmltools::tags$h1(.lbl("dashboard_title", lang)),
    htmltools::HTML(header),
    htmltools::HTML(tbl_html)
  )
  htmltools::save_html(html_out, file = save_path)
  cli::cli_alert_success("HTML dashboard saved: {.file {save_path}}")
}


# ── i18n labels ───────────────────────────────────────────────────────────────
.view_labels <- list(
  demographic_summary = c(en = "Demographic Summary",             pt = "Resumo Demografico",              es = "Resumen Demografico"),
  dashboard_title     = c(en = "Dengue Notification Profile",     pt = "Perfil de Notificacoes de Dengue",es = "Perfil de Notificaciones de Dengue"),
  map_title           = c(en = "Geographic Distribution",         pt = "Distribuicao Geografica",         es = "Distribucion Geografica"),
  bar_title           = c(en = "Distribution by",                 pt = "Distribuicao por",                es = "Distribucion por"),
  pyramid_title       = c(en = "Age-Sex Pyramid",                 pt = "Piramide Etaria por Sexo",        es = "Piramide de Edad y Sexo"),
  temporal_title      = c(en = "Epidemic Curve",                  pt = "Curva Epidemica",                 es = "Curva Epidemica"),
  risk_title          = c(en = "Climate Risk Group Distribution",  pt = "Distribuicao por Grupo de Risco", es = "Distribucion por Grupo de Riesgo"),
  count               = c(en = "Count",                           pt = "Contagem",                        es = "Conteo"),
  percent             = c(en = "Percent (%)",                     pt = "Percentual (%)",                  es = "Porcentaje (%)"),
  n_cases             = c(en = "Cases",                           pt = "Casos",                           es = "Casos"),
  records             = c(en = "records",                         pt = "registros",                       es = "registros"),
  male                = c(en = "Male",                            pt = "Masculino",                       es = "Masculino"),
  female              = c(en = "Female",                          pt = "Feminino",                        es = "Femenino"),
  age_group_lbl       = c(en = "Age Group",                       pt = "Faixa Etaria",                    es = "Grupo de Edad"),
  time_var_lbl        = c(en = "Time variable",                   pt = "Variavel temporal",               es = "Variable temporal"),
  sex_lbl             = c(en = "Sex",                             pt = "Sexo",                            es = "Sexo"),
  source              = c(en = "Source: DATASUS/Ministry of Health (SINAN)", pt = "Fonte: DATASUS/MS (SINAN)", es = "Fuente: DATASUS/MS (SINAN)")
)

.lbl <- function(key, lang) {
  row <- .view_labels[[key]]
  if (is.null(row)) return(key)
  row[[lang]] %||% row[["pt"]]
}
