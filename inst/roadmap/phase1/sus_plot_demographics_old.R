
#' Visualize Demographic Filtering Results
#'
#' Produces tables, maps, or charts summarizing the demographic composition
#' of a filtered climasus_df dataset. Designed to complement
#' `sus_data_filter_demographics()`.
#'
#' @param df A climasus_df object (output from `sus_data_filter_demographics()`).
#' @param type Character string. One of `"table"`, `"map"`, `"bar"`, `"pyramid"`,
#'   or `"dashboard"` (combines all outputs). Default is `"table"`.
#' @param var Character string specifying which demographic variable to visualize.
#'   Options: `"sex"`, `"race"`, `"age"`, `"education"`, `"region"`.
#'   Ignored when `type = "dashboard"`.
#' @param palette Character string. Color palette name from `RColorBrewer` or
#'   `"sus"` for the package default palette. Default is `"sus"`.
#' @param interactive Logical. If `TRUE`, produces interactive plots via `plotly`
#'   and `leaflet`. If `FALSE` (default), uses `ggplot2` and `gt`.
#' @param lang Character string. `"en"`, `"pt"` (default), or `"es"`.
#' @param save_path Character string. If provided, saves output to this path.
#'   For `"dashboard"`, saves an HTML file. For individual plots, saves PNG.
#' @param ... Additional arguments passed to underlying plotting functions.
#'
#' @return Invisibly returns the plot or table object. Side effect: renders output.
#'
#' @export
sus_view_demographics <- function(df,
                                  type     = "table",
                                  var      = NULL,
                                  palette  = "sus",
                                  interactive = FALSE,
                                  lang     = "pt",
                                  save_path = NULL,
                                  ...) {
  
  # --- Input validation ---
  if (!inherits(df, "climasus_df")) {
    cli::cli_abort("df must be a climasus_df object from the CLIMASUS4r pipeline.")
  }
  
  valid_types <- c("table", "map", "bar", "pyramid", "dashboard")
  if (!type %in% valid_types) {
    cli::cli_abort("type must be one of: {.val {valid_types}}")
  }
  
  # --- Dispatch ---
  out <- switch(type,
                "table"     = .view_table(df, var, lang, interactive, ...),
                "map"       = .view_map(df, lang, interactive, palette, ...),
                "bar"       = .view_bar(df, var, lang, interactive, palette, ...),
                "pyramid"   = .view_pyramid(df, lang, interactive, palette, ...),
                "dashboard" = .view_dashboard(df, lang, interactive, palette, ...)
  )
  
  # --- Save if requested ---
  if (!is.null(save_path)) {
    .save_view(out, type, save_path, lang)
  }
  
  invisible(out)
}


# HELPER FUNCTIONS --------------------------------------------------------

.view_table <- function(df, var, lang, interactive, ...) {
  
  # Detect available demographic columns
  demo_cols <- .detect_demo_cols(df)
  
  if (is.null(var)) {
    # Summary table: one row per demographic dimension
    summary_list <- lapply(names(demo_cols), function(dim) {
      col <- demo_cols[[dim]]
      if (!col %in% names(df)) return(NULL)
      
      counts <- df[, .N, by = get(col)]
      data.table::setnames(counts, c("category", "n"))
      counts[, `:=`(
        dimension = dim,
        pct = round(100 * n / sum(n), 1)
      )]
      counts
    })
    tbl_data <- data.table::rbindlist(summary_list, fill = TRUE)
    
  } else {
    col <- demo_cols[[var]]
    tbl_data <- df[, .N, by = get(col)]
    data.table::setnames(tbl_data, c("category", "n"))
    tbl_data[, pct := round(100 * n / sum(n), 1)]
  }
  
  if (interactive) {
    # DT::datatable for interactive HTML table
    out <- DT::datatable(
      tbl_data,
      options = list(pageLength = 20, scrollX = TRUE),
      caption = .label("demographic_summary", lang)
    )
  } else {
    # gt for static publication-quality table
    out <- tbl_data |>
      gt::gt() |>
      gt::tab_header(
        title    = .label("demographic_summary", lang),
        subtitle = .label("data_subtitle", lang)
      ) |>
      gt::cols_label(
        category  = .label("category", lang),
        n         = .label("count", lang),
        pct       = .label("percent", lang)
      ) |>
      gt::fmt_number(columns = "n", decimals = 0, use_seps = TRUE) |>
      gt::fmt_number(columns = "pct", decimals = 1, suffix = "%") |>
      gt::data_color(
        columns = "pct",
        palette = c("#f7fbff", "#08519c")
      ) |>
      gt::tab_source_note(.label("source_datasus", lang))
  }
  
  out
}

.view_map <- function(df, lang, interactive, palette, ...) {
  
  # Requires: sf geometry OR municipality/state codes to join with shapefile
  uf_col   <- find_column(df, c("manager_uf", "UF_ZI", "notification_uf", "uf"))
  muni_col <- find_column(df, c("residence_municipality_code", "CODMUNRES",
                                "municipality_code"))
  
  # Decide aggregation level (municipality > state)
  if (!is.null(muni_col)) {
    level <- "municipality"
    geo_col <- muni_col
  } else if (!is.null(uf_col)) {
    level <- "state"
    geo_col <- uf_col
  } else {
    cli::cli_abort(c(
      "Cannot produce map: no geographic column found.",
      "i" = "Expected one of: {.val {c('manager_uf', 'municipality_code', 'CODMUNRES')}}"
    ))
  }
  
  # Count records per geographic unit
  count_col <- .get_count_colname(df, lang)
  map_data  <- df[, .N, by = get(geo_col)]
  data.table::setnames(map_data, c("geo_id", count_col))
  
  # Load built-in Brazil shapefile (package data)
  # geobr is the natural dependency here
  if (level == "state") {
    shape <- geobr::read_state(year = 2020, showProgress = FALSE)
    map_data <- dplyr::left_join(
      shape, map_data,
      by = c("code_state" = "geo_id")
    )
  } else {
    shape <- geobr::read_municipality(year = 2020, showProgress = FALSE)
    map_data <- dplyr::left_join(
      shape, map_data,
      by = c("code_muni" = "geo_id")
    )
  }
  
  if (interactive) {
    # leaflet choropleth
    pal <- leaflet::colorNumeric(palette = "YlOrRd",
                                 domain = map_data[[count_col]],
                                 na.color = "#cccccc")
    out <- leaflet::leaflet(map_data) |>
      leaflet::addProviderTiles("CartoDB.Positron") |>
      leaflet::addPolygons(
        fillColor   = ~pal(get(count_col)),
        fillOpacity = 0.8,
        color       = "white",
        weight      = 1,
        label       = ~paste0(name_muni %||% abbrev_state, ": ", get(count_col)),
        highlightOptions = leaflet::highlightOptions(
          weight = 2, color = "#333", bringToFront = TRUE
        )
      ) |>
      leaflet::addLegend(
        pal    = pal,
        values = ~get(count_col),
        title  = .label("legend_count", lang)
      )
  } else {
    # ggplot2 + sf static map
    out <- ggplot2::ggplot(map_data) +
      ggplot2::geom_sf(ggplot2::aes(fill = .data[[count_col]]),
                       color = "white", linewidth = 0.2) +
      ggplot2::scale_fill_viridis_c(
        option    = "plasma",
        name      = .label("legend_count", lang),
        na.value  = "#cccccc",
        labels    = scales::comma
      ) +
      ggplot2::labs(
        title    = .label("map_title", lang),
        subtitle = .label("map_subtitle", lang),
        caption  = .label("source_datasus", lang)
      ) +
      ggplot2::theme_void(base_size = 12) +
      ggplot2::theme(
        legend.position = "right",
        plot.title      = ggplot2::element_text(face = "bold")
      )
  }
  
  out
}

.view_bar <- function(df, var, lang, interactive, palette, ...) {
  
  if (is.null(var)) {
    cli::cli_abort("var must be specified for type = 'bar'. Options: 'sex', 'race', 'age', 'education'")
  }
  
  demo_cols <- .detect_demo_cols(df)
  col       <- demo_cols[[var]]
  
  if (is.null(col) || !col %in% names(df)) {
    cli::cli_abort("Column for {.val {var}} not found in data.")
  }
  
  bar_data <- df[, .N, by = get(col)]
  data.table::setnames(bar_data, c("category", "n"))
  bar_data[, pct := round(100 * n / sum(n), 1)]
  bar_data <- bar_data[order(-n)]
  
  .sus_palette <- c("#1B6CA8", "#E84855", "#3BB273", "#F4A261",
                    "#7B2D8B", "#2EC4B6", "#FF9F1C", "#CBCBCB")
  
  p <- ggplot2::ggplot(
    bar_data,
    ggplot2::aes(
      x    = stats::reorder(category, n),
      y    = n,
      fill = category
    )
  ) +
    ggplot2::geom_col(show.legend = FALSE, width = 0.7) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(pct, "%")),
      hjust = -0.2, size = 3.5, color = "gray30"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = .sus_palette) +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    ggplot2::labs(
      title    = .label(paste0("bar_title_", var), lang),
      x        = NULL,
      y        = .label("count", lang),
      caption  = .label("source_datasus", lang)
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      plot.title         = ggplot2::element_text(face = "bold", size = 14)
    )
  
  if (interactive) {
    return(plotly::ggplotly(p, tooltip = c("x", "y", "label")))
  }
  
  p
}

.view_pyramid <- function(df, lang, interactive, palette, ...) {
  
  age_col <- find_column(df, c("age_group", "grupo_etario", "faixa_etaria", "age_group_5yr"))
  sex_col <- find_column(df, c("sex", "sexo", "SEXO"))
  
  if (is.null(age_col) || is.null(sex_col)) {
    cli::cli_abort(c(
      "Population pyramid requires both age group and sex columns.",
      "i" = "Run {.fn sus_create_variables} to create 'age_group' before visualizing."
    ))
  }
  
  # Build pyramid data
  pyr_data <- df[, .N, by = c(age_col, sex_col)]
  data.table::setnames(pyr_data, c("age_group", "sex", "n"))
  
  # Labels dinâmicos baseados no idioma
  lab_male   <- .label("male", lang)
  lab_female <- .label("female", lang)
  
  # Normalize sex labels
  male_vals   <- c("Male", "Masculino", "M")
  female_vals <- c("Female", "Feminino", "F")
  
  pyr_data[sex %in% male_vals,   sex_plot := lab_male]
  pyr_data[sex %in% female_vals, sex_plot := lab_female]
  pyr_data <- pyr_data[!is.na(sex_plot)]
  
  # Mirroring
  pyr_data[, n_plot := ifelse(sex_plot == lab_male, -n, n)]
  
  max_n  <- max(abs(pyr_data$n_plot))
  breaks <- pretty(c(-max_n, max_n), n = 6)
  
  # DEFINIÇÃO DAS CORES (Correção do Erro de Sintaxe)
  fill_colors <- stats::setNames(c("#1B6CA8", "#E84855"), c(lab_male, lab_female))
  
  p <- ggplot2::ggplot(
    pyr_data,
    ggplot2::aes(
      x    = age_group,
      y    = n_plot,
      fill = sex_plot
    )
  ) +
    ggplot2::geom_col(width = 0.85) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      breaks = breaks,
      labels = function(x) scales::comma(abs(x)) # Simplificado
    ) +
    ggplot2::scale_fill_manual(
      values = fill_colors,
      name   = NULL
    ) +
    ggplot2::labs(
      title   = .label("pyramid_title", lang),
      x       = .label("age_group", lang),
      y       = .label("count", lang),
      caption = .label("source_datasus", lang)
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title      = ggplot2::element_text(face = "bold")
    )
  
  if (interactive) {
    return(plotly::ggplotly(p))
  }
  
  return(p)
}



.view_dashboard <- function(df, lang, interactive, palette, ...) {
  
  # Collect all available outputs
  parts <- list()
  
  parts$table   <- tryCatch(.view_table(df, NULL, lang, FALSE), error = function(e) NULL)
  parts$map     <- tryCatch(.view_map(df, lang, FALSE, palette), error = function(e) NULL)
  parts$bar_sex <- tryCatch(.view_bar(df, "sex", lang, FALSE, palette), error = function(e) NULL)
  parts$pyramid <- tryCatch(.view_pyramid(df, lang, FALSE, palette), error = function(e) NULL)
  
  # Compose with patchwork
  valid_plots <- Filter(
    function(x) inherits(x, "ggplot"),
    list(parts$map, parts$bar_sex, parts$pyramid)
  )
  
  if (length(valid_plots) > 0) {
    combined <- patchwork::wrap_plots(valid_plots, ncol = 2) +
      patchwork::plot_annotation(
        title    = .label("dashboard_title", lang),
        subtitle = .label("dashboard_subtitle", lang),
        caption  = .label("source_datasus", lang),
        theme    = ggplot2::theme(
          plot.title    = ggplot2::element_text(face = "bold", size = 16),
          plot.subtitle = ggplot2::element_text(color = "gray40")
        )
      )
    print(combined)
  }
  
  # Print gt table separately (can't embed in patchwork)
  if (!is.null(parts$table)) print(parts$table)
  
  invisible(parts)
}

# Supporting Infrastructure -----------------------------------------------

.view_labels <- list(
  demographic_summary = c(en = "Demographic Summary",      pt = "Resumo Demografico",       es = "Resumen Demografico"),
  category            = c(en = "Category",                 pt = "Categoria",                 es = "Categoria"),
  count               = c(en = "Count",                    pt = "Contagem",                  es = "Conteo"),
  percent             = c(en = "Percent",                  pt = "Percentual",                es = "Porcentaje"),
  male                = c(en = "Male",                     pt = "Masculino",                 es = "Masculino"),
  female              = c(en = "Female",                   pt = "Feminino",                  es = "Femenino"),
  pyramid_title       = c(en = "Age-Sex Pyramid",          pt = "Piramide Etaria por Sexo",  es = "Piramide de Edad y Sexo"),
  map_title           = c(en = "Geographic Distribution",  pt = "Distribuicao Geografica",   es = "Distribucion Geografica"),
  dashboard_title     = c(en = "Demographic Profile",      pt = "Perfil Demografico",        es = "Perfil Demografico"),
  source_datasus      = c(en = "Source: DATASUS/Ministry of Health", pt = "Fonte: DATASUS/Ministerio da Saude", es = "Fuente: DATASUS/Ministerio de Salud"),
  age_group           = c(en = "Age Group",                pt = "Faixa Etaria",              es = "Grupo de Edad"),
  legend_count        = c(en = "Records",                  pt = "Registros",                 es = "Registros")
)

.label <- function(key, lang) {
  row <- .view_labels[[key]]
  if (is.null(row)) return(key)
  row[[lang]] %||% row[["pt"]]
}

.detect_demo_cols <- function(df) {
  list(
    sex       = find_column(df, c("sex", "sexo", "SEXO")),
    race      = find_column(df, c("race", "raca", "raza", "RACACOR")),
    age       = find_column(df, c("age_years", "idade", "edad")),
    age_group = find_column(df, c("age_group", "faixa_etaria", "grupo_etario")),
    education = find_column(df, c("education", "escolaridade", "ESC"))
  )
}

find_column <- function(df, patterns) {
  for (pattern in patterns) {
    if (pattern %in% names(df)) {
      return(pattern)
    }
  }
  return(NULL)
}


---
  
