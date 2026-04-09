# =============================================================================
# sus_view_demographics.R
# Robust, publication-quality demographic visualization for climasus4r
# Compatible with: SIM, SIH, CNES, SINAN, SIA, SIAB, and all DATASUS systems
# Style: The Lancet / Nature Medicine · EN / PT / ES
# Author: climasus4r package
# =============================================================================

#' Visualize Demographic Profiles from DATASUS Systems
#'
#' Produces publication-quality tables, maps, and charts summarising the
#' demographic and climate-risk composition of any standardised
#' \code{climasus_df} dataset (SIM, SIH, CNES, SINAN, SIA, SIAB, etc.).
#' Visual style follows The Lancet / Nature Medicine guidelines.
#'
#' @param df A \code{climasus_df} object produced by the \pkg{climasus4r}
#'   pipeline (after \code{sus_data_filter_demographics()}).
#' @param type Character. Visualization type. One of:
#'   \itemize{
#'     \item \code{"table"}      — Summary frequency table (gt / DT).
#'     \item \code{"bar"}        — Horizontal bar chart for one variable.
#'     \item \code{"pyramid"}    — Age-sex population pyramid.
#'     \item \code{"map"}        — Choropleth map (state or municipality level).
#'     \item \code{"temporal"}   — Time-series line chart (month/year/epi-week).
#'     \item \code{"climate"}    — Climate-risk group distribution (bar + heatmap).
#'     \item \code{"race_equity"}— Race/color equity plot (diverging from national benchmark).
#'     \item \code{"dashboard"}  — Composite panel combining key plots (Lancet layout).
#'   }
#' @param var Character. Demographic variable to visualise (required for
#'   \code{"bar"} and \code{"table"} with a single variable). One of:
#'   \code{"sex"}, \code{"race"}, \code{"age"}, \code{"age_group"},
#'   \code{"education"}, \code{"climate_risk"}, \code{"region"},
#'   \code{"epidemiological_week"}, \code{"month"}, \code{"year"}.
#' @param time_unit Character. Temporal resolution for \code{type = "temporal"}.
#'   One of \code{"month"} (default), \code{"epi_week"}, \code{"year"},
#'   \code{"quarter"}, \code{"semester"}.
#' @param fill_var Character. Optional grouping variable for stratified
#'   temporal plots (e.g. \code{"sex"}, \code{"age_group"},
#'   \code{"climate_risk_group"}).
#' @param palette Character. Colour scheme. One of:
#'   \code{"lancet"} (default), \code{"sunset"}, \code{"nature"}, \code{"nejm"},
#'   \code{"sus"}, \code{"viridis"}, \code{"colorblind"}.
#' @param map_var Character. Variable to map in \code{type = "map"}.
#'   Default \code{"count"} (record density). Accepts any numeric column name.
#' @param show_ci Logical. Add 95% Poisson confidence intervals in temporal
#'   plots. Default \code{FALSE}.
#' @param benchmark Numeric vector of length equal to race/age categories.
#'   National or regional reference proportions for equity plots.
#'   If \code{NULL} (default), IBGE 2022 Census proportions are used
#'   where available.
#' @param interactive Logical. If \code{TRUE}, returns interactive
#'   \pkg{plotly} / \pkg{leaflet} widgets. Default \code{FALSE}.
#' @param base_size Numeric. Base font size for ggplot2 theme. Default 11
#'   (Lancet column width).
#' @param lang Character. Language for labels and messages.
#'   \code{"en"} (default), \code{"pt"}, \code{"es"}.
#' @param caption_suffix Character. Additional text appended to figure
#'   caption (e.g., study period, DOI). Default \code{NULL}.
#' @param save_path Character. File path for saving output. For
#'   \code{"dashboard"} an HTML file is saved; otherwise PNG/PDF/SVG
#'   depending on the extension. Default \code{NULL}.
#' @param width,height Numeric. Output dimensions in inches. Defaults:
#'   \code{width = 7}, \code{height = 5} (single); \code{12 x 9}
#'   (dashboard).
#' @param dpi Numeric. Resolution for raster output. Default \code{300}.
#' @param verbose Logical. Print progress messages. Default \code{TRUE}.
#' @param ... Additional arguments passed to underlying functions.
#'
#' @return Invisibly returns the primary plot or list of plots. Side effect:
#'   renders output in the active graphics device.
#'
#' @section Systems supported:
#' \code{sus_view_demographics()} automatically adapts column detection to all
#' major DATASUS information systems:
#' \itemize{
#'   \item \strong{SINAN} (notifiable diseases): dengue, chikungunya, COVID-19, etc.
#'   \item \strong{SIM} (mortality): ICD-10, cause-of-death, comorbidities.
#'   \item \strong{SIH} (hospitalisation): procedure codes, length of stay, AIH.
#'   \item \strong{CNES} (health facilities): capacity, equipment, workforce.
#'   \item \strong{SIA} (outpatient): production, procedures.
#'   \item \strong{SIAB/e-SUS} (primary care): coverage, visits.
#' }
#'
#' @section Climate & Environment Integration:
#' When the variable \code{climate_risk_group} is present (created by
#' \code{sus_data_create_variables()}), the \code{"climate"} plot type produces
#' a combined bar + season heatmap showing disease burden by ecological
#' risk category, astronomical season, and biome region. This is designed
#' for climate-health submissions to The Lancet Planetary Health.
#'
#' @examples
#' \dontrun{
#' library(climasus4r)
#'
#' # Basic age-sex pyramid (Lancet style, English)
#' sus_view_demographics(df, type = "pyramid", lang = "en")
#'
#' # Publication-quality dashboard, save to PDF
#' sus_view_demographics(
#'   df,
#'   type      = "dashboard",
#'   palette   = "lancet",
#'   lang      = "en",
#'   save_path = "figures/fig1_demographics.pdf",
#'   width = 12, height = 9
#' )
#'
#' # Temporal epidemic curve, stratified by climate risk group
#' sus_view_demographics(
#'   df,
#'   type      = "temporal",
#'   time_unit = "epi_week",
#'   fill_var  = "climate_risk_group",
#'   show_ci   = TRUE,
#'   lang      = "en"
#' )
#'
#' # Race equity diverging plot (SIM data)
#' sus_view_demographics(df, type = "race_equity", lang = "pt")
#'
#' # Interactive choropleth map (SINAN-dengue, municipality level)
#' sus_view_demographics(df, type = "map", interactive = TRUE, lang = "pt")
#' }
#'
#' @export
sus_view_demographics <- function(df,
                                  type          = "table",
                                  var           = NULL,
                                  time_unit     = "month",
                                  fill_var      = NULL,
                                  palette       = "lancet",
                                  map_var       = "count",
                                  show_ci       = FALSE,
                                  benchmark     = NULL,
                                  interactive   = FALSE,
                                  base_size     = 11,
                                  lang          = "en",
                                  caption_suffix = NULL,
                                  save_path     = NULL,
                                  width         = NULL,
                                  height        = NULL,
                                  dpi           = 300,
                                  verbose       = TRUE,
                                  ...) {
  
  # ── 1. Input Validation ────────────────────────────────────────────────────
  
  if (!inherits(df, "climasus_df")) {
    cli::cli_abort(c(
      "{.cls climasus_df} object required.",
      "i" = "Run the full climasus4r pipeline first.",
      "*" = "{.code df <- sus_data_import(...)}",
      "*" = "{.code df <- sus_data_standardize(df)}",
      "*" = "{.code df <- sus_data_create_variables(df)}",
      "*" = "{.code df <- sus_data_filter_demographics(df)}"
    ))
  }
  
  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_abort("{.arg lang} must be one of: {.val {c('en', 'pt', 'es')}}")
  }
  
  valid_types <- c("table", "bar", "pyramid", "map",
                   "temporal", "climate", "race_equity", "dashboard")
  if (!type %in% valid_types) {
    cli::cli_abort("{.arg type} must be one of: {.val {valid_types}}")
  }
  
  valid_time <- c("month", "epi_week", "year", "quarter", "semester")
  if (!time_unit %in% valid_time) {
    cli::cli_abort("{.arg time_unit} must be one of: {.val {valid_time}}")
  }
  
  # ── 2. Detect DATASUS system ───────────────────────────────────────────────
  system_id <- tryCatch(
    sus_meta(df, "system"),
    error = function(e) "unknown"
  )
  
  if (verbose) {
    cli::cli_alert_info(
      .vl("system_detected", lang, system_id %||% "unknown")
    )
  }
  
  # ── 3. Build caption ───────────────────────────────────────────────────────
  cap_base <- .vl("source_datasus", lang)
  caption  <- if (!is.null(caption_suffix)) {
    paste0(cap_base, " | ", caption_suffix)
  } else {
    cap_base
  }
  
  # ── 4. Dispatch ───────────────────────────────────────────────────────────
  out <- switch(type,
                "table"      = .vd_table(df, var, lang, interactive, palette, caption, base_size, ...),
                "bar"        = .vd_bar(df, var, lang, interactive, palette, caption, base_size, ...),
                "pyramid"    = .vd_pyramid(df, lang, interactive, palette, caption, base_size, ...),
                "map"        = .vd_map(df, map_var, lang, interactive, palette, caption, base_size, ...),
                "temporal"   = .vd_temporal(df, time_unit, fill_var, show_ci, lang, interactive,
                                            palette, caption, base_size, ...),
                "climate"    = .vd_climate(df, lang, interactive, palette, caption, base_size, ...),
                "race_equity"= .vd_race_equity(df, benchmark, lang, interactive, palette,
                                               caption, base_size, ...),
                "dashboard"  = .vd_dashboard(df, lang, interactive, palette, caption,
                                             base_size, show_ci, ...)
  )
  
  # ── 5. Save ───────────────────────────────────────────────────────────────
  if (!is.null(save_path)) {
    .vd_save(out, type, save_path,
             width  = width  %||% if (type == "dashboard") 12 else 7,
             height = height %||% if (type == "dashboard") 9  else 5,
             dpi    = dpi, lang = lang, verbose = verbose)
  }
  
  invisible(out)
}


# =============================================================================
# ── INTERNAL PLOT FUNCTIONS ──────────────────────────────────────────────────
# =============================================================================

# ── Table ─────────────────────────────────────────────────────────────────────
.vd_table <- function(df, var, lang, interactive, palette, caption, base_size, ...) {
  
  demo_cols <- .vd_detect_cols(df)
  pal        <- .vd_palette(palette)
  
  if (is.null(var)) {
    # Multi-dimension frequency table
    rows <- lapply(names(demo_cols), function(dim) {
      col <- demo_cols[[dim]]
      if (is.null(col) || !col %in% names(df)) return(NULL)
      tab <- as.data.frame(table(df[[col]], useNA = "ifany"), stringsAsFactors = FALSE)
      names(tab) <- c("category", "n")
      tab$dimension <- .vl(dim, lang)
      tab$pct <- round(100 * tab$n / sum(tab$n, na.rm = TRUE), 1)
      tab[order(-tab$n), ]
    })
    tbl_data <- do.call(rbind, Filter(Negate(is.null), rows))
    tbl_data <- tbl_data[, c("dimension", "category", "n", "pct")]
  } else {
    col <- demo_cols[[var]]
    if (is.null(col) || !col %in% names(df)) {
      cli::cli_abort("Column for {.val {var}} not found in dataset.")
    }
    tbl_data <- as.data.frame(table(df[[col]], useNA = "ifany"), stringsAsFactors = FALSE)
    names(tbl_data) <- c("category", "n")
    tbl_data$pct <- round(100 * tbl_data$n / sum(tbl_data$n, na.rm = TRUE), 1)
    tbl_data <- tbl_data[order(-tbl_data$n), ]
  }
  
  if (interactive) {
    if (!requireNamespace("DT", quietly = TRUE)) {
      cli::cli_abort("Package {.pkg DT} required for interactive tables.")
    }
    return(DT::datatable(
      tbl_data,
      caption  = .vl("demographic_summary", lang),
      options  = list(pageLength = 25, scrollX = TRUE,
                      dom = "Bfrtip", buttons = c("csv", "excel")),
      rownames = FALSE
    ))
  }
  
  if (!requireNamespace("gt", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg gt} required for static tables.")
  }
  
  col_labels <- list(
    dimension = .vl("dimension", lang),
    category  = .vl("category", lang),
    n         = .vl("count", lang),
    pct       = .vl("percent", lang)
  )
  
  gt_obj <- gt::gt(tbl_data) |>
    gt::tab_header(
      title    = gt::md(paste0("**", .vl("demographic_summary", lang), "**")),
      subtitle = gt::md(paste0("*n* = ", format(nrow(df), big.mark = ",")))
    ) |>
    gt::cols_label(.list = col_labels[names(col_labels) %in% names(tbl_data)]) |>
    gt::fmt_integer(columns = "n", use_seps = TRUE) |>
    gt::fmt_number(columns = "pct", decimals = 1, suffix = "%") |>
    gt::data_color(
      columns = "pct",
      palette = c("#f7f7f7", pal[1])
    ) |>
    gt::tab_source_note(caption) |>
    gt::tab_options(
      table.font.size      = gt::px(base_size),
      heading.title.font.size = gt::px(base_size + 2),
      column_labels.font.weight = "bold",
      table.border.top.color  = "#000000",
      table.border.top.width  = gt::px(2),
      column_labels.border.bottom.color = "#000000",
      column_labels.border.bottom.width = gt::px(1),
      table_body.border.bottom.color    = "#000000",
      table_body.border.bottom.width    = gt::px(1.5)
    )
  
  # Group rows by dimension if multi-dim
  if (!is.null(var) == FALSE && "dimension" %in% names(tbl_data)) {
    gt_obj <- gt_obj |> gt::tab_row_group(
      label = "",
      rows  = everything()
    )
  }
  
  gt_obj
}

# ── Bar chart ─────────────────────────────────────────────────────────────────
.vd_bar <- function(df, var, lang, interactive, palette, caption, base_size, ...) {
  
  if (is.null(var)) {
    cli::cli_abort(c(
      "{.arg var} required for {.code type = 'bar'}.",
      "i" = "Options: {.val {c('sex','race','age_group','education','climate_risk','region')}}"
    ))
  }
  
  demo_cols <- .vd_detect_cols(df)
  col       <- demo_cols[[var]]
  pal        <- .vd_palette(palette)
  
  if (is.null(col) || !col %in% names(df)) {
    cli::cli_abort("Column for {.val {var}} not found. Run {.fn sus_data_create_variables} to generate it.")
  }
  
  bar_data <- as.data.frame(table(df[[col]], useNA = "no"), stringsAsFactors = FALSE)
  names(bar_data) <- c("category", "n")
  bar_data$pct <- round(100 * bar_data$n / sum(bar_data$n, na.rm = TRUE), 1)
  bar_data <- bar_data[order(bar_data$n), ]
  bar_data$category <- factor(bar_data$category, levels = bar_data$category)
  
  p <- ggplot2::ggplot(bar_data, ggplot2::aes(x = category, y = n, fill = category)) +
    ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(pct, "%")),
      hjust  = -0.15,
      size   = base_size * 0.27,
      colour = "grey30",
      fontface = "plain"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = rep(pal, length.out = nrow(bar_data))) +
    ggplot2::scale_y_continuous(
      labels = scales::label_comma(),
      expand = ggplot2::expansion(mult = c(0, 0.18))
    ) +
    ggplot2::labs(
      title   = .vl(paste0("bar_title_", var), lang),
      x       = NULL,
      y       = .vl("count", lang),
      caption = caption
    ) +
    .lancet_theme(base_size) +
    ggplot2::theme(
      axis.line.y          = ggplot2::element_blank(),
      panel.grid.major.y   = ggplot2::element_blank(),
      panel.grid.minor.y   = ggplot2::element_blank()
    )
  
  if (interactive) {
    .require_pkg("plotly")
    return(plotly::ggplotly(p, tooltip = c("x", "y", "text")))
  }
  p
}

# ── Population pyramid ────────────────────────────────────────────────────────
# .vd_pyramid <- function(df, lang, interactive, palette, caption, base_size, ...) {
#   
#   #age_col <- .find_col(df, c("age_group", "ibge_age_group"))
#   age_col <- .find_col(df, c("ibge_age_group"))
#   sex_col <- .find_col(df, c("sex", "sexo", "SEXO"))
#   
#   if (is.null(age_col) || is.null(sex_col)) {
#     cli::cli_abort(c(
#       "Population pyramid requires {.strong age group} and {.strong sex} columns.",
#       "i" = "Run {.fn sus_data_create_variables} to generate {.val age_group}."
#     ))
#   }
#   
#   pal       <- .vd_palette(palette)
#   lab_male  <- .vl("male", lang)
#   lab_female <- .vl("female", lang)
#   
#   male_vals   <- c("Male", "Masculino", "M", "1", "male", "masculino")
#   female_vals <- c("Female", "Feminino", "F", "2", "female", "feminino")
#   
#   pyr_raw <- as.data.frame(
#     table(df[[age_col]], df[[sex_col]], useNA = "no"),
#     stringsAsFactors = FALSE
#   )
#   names(pyr_raw) <- c("age_group", "sex_raw", "n")
#   pyr_raw$n <- as.numeric(pyr_raw$n)
#   
#   pyr_raw$sex_label <- dplyr::case_when(
#     pyr_raw$sex_raw %in% male_vals   ~ lab_male,
#     pyr_raw$sex_raw %in% female_vals ~ lab_female,
#     TRUE ~ NA_character_
#   )
#   pyr_raw <- pyr_raw[!is.na(pyr_raw$sex_label), ]
#   
#   # Total per sex for % annotation
#   total_male   <- sum(pyr_raw$n[pyr_raw$sex_label == lab_male],   na.rm = TRUE)
#   total_female <- sum(pyr_raw$n[pyr_raw$sex_label == lab_female], na.rm = TRUE)
#   
#   pyr_raw$n_plot <- ifelse(pyr_raw$sex_label == lab_male, -pyr_raw$n, pyr_raw$n)
#   pyr_raw$pct    <- ifelse(
#     pyr_raw$sex_label == lab_male,
#     round(100 * pyr_raw$n / total_male,   1),
#     round(100 * pyr_raw$n / total_female, 1)
#   )
#   
#   max_n  <- max(abs(pyr_raw$n_plot), na.rm = TRUE)
#   brks   <- pretty(c(-max_n, max_n), n = 6)
#   col_map <- stats::setNames(c(pal[2], pal[1]), c(lab_male, lab_female))
#   
#   # Sex ratio subtitle
#   sex_ratio <- if (total_female > 0) round(total_male / total_female, 2) else NA
#   sub_txt   <- if (!is.na(sex_ratio)) {
#     paste0(.vl("sex_ratio", lang), ": ", sex_ratio)
#   } else ""
#   
#   p <- ggplot2::ggplot(
#     pyr_raw,
#     ggplot2::aes(x = age_group, y = n_plot, fill = sex_label,
#                  text = paste0(age_group, " | ", sex_label,
#                                "\n n = ", scales::comma(abs(n_plot)),
#                                " (", pct, "%)"))
#   ) +
#     ggplot2::geom_col(width = 0.82) +
#     ggplot2::geom_hline(yintercept = 0, colour = "white", linewidth = 0.4) +
#     ggplot2::coord_flip() +
#     ggplot2::scale_y_continuous(
#       breaks = brks,
#       labels = function(x) scales::comma(abs(x)),
#       expand = ggplot2::expansion(mult = 0.04)
#     ) +
#     ggplot2::scale_fill_manual(values = col_map, name = NULL) +
#     ggplot2::labs(
#       title    = .vl("pyramid_title", lang),
#       subtitle = sub_txt,
#       x        = .vl("age_group", lang),
#       y        = .vl("count", lang),
#       caption  = caption
#     ) +
#     .lancet_theme(base_size) +
#     ggplot2::theme(
#       legend.position = "top",
#       legend.key.size = ggplot2::unit(0.5, "lines")
#     )
#   
#   if (interactive) {
#     .require_pkg("plotly")
#     return(plotly::ggplotly(p, tooltip = "text"))
#   }
#   p
# }

.vd_pyramid <- function(df, lang, interactive, palette, caption, base_size, ...) {
  
  age_col <- .find_col(df, c("ibge_age_group"))
  sex_col <- .find_col(df, c("sex", "sexo", "SEXO"))
  
  if (is.null(age_col) || is.null(sex_col)) {
    cli::cli_abort(c(
      "Population pyramid requires {.strong age group} and {.strong sex} columns.",
      "i" = "Run {.fn sus_data_create_variables} to generate {.val age_group}."
    ))
  }
  
  pal       <- .vd_palette(palette)
  lab_male  <- .vl("male", lang)
  lab_female <- .vl("female", lang)
  
  male_vals   <- c("Male", "Masculino", "M", "1", "male", "masculino")
  female_vals <- c("Female", "Feminino", "F", "2", "female", "feminino")
  
  pyr_raw <- as.data.frame(
    table(df[[age_col]], df[[sex_col]], useNA = "no"),
    stringsAsFactors = FALSE
  )
  names(pyr_raw) <- c("age_group", "sex_raw", "n")
  
  # --- INÍCIO DA CORREÇÃO DE ORDENAÇÃO ---
  # Extraímos os números iniciais de cada faixa para ordenar numericamente
  # Ex: "5-9" vira 5, "50-54" vira 50. Assim o 5 vem antes do 50.
  unique_ages <- unique(pyr_raw$age_group)
  ordered_ages <- unique_ages[order(as.numeric(gsub("[^0-9].*", "", unique_ages)))]
  
  pyr_raw$age_group <- factor(pyr_raw$age_group, levels = ordered_ages)
  # --- FIM DA CORREÇÃO ---
  
  pyr_raw$n <- as.numeric(pyr_raw$n)
  
  pyr_raw$sex_label <- dplyr::case_when(
    pyr_raw$sex_raw %in% male_vals   ~ lab_male,
    pyr_raw$sex_raw %in% female_vals ~ lab_female,
    TRUE ~ NA_character_
  )
  pyr_raw <- pyr_raw[!is.na(pyr_raw$sex_label), ]
  
  total_male   <- sum(pyr_raw$n[pyr_raw$sex_label == lab_male],   na.rm = TRUE)
  total_female <- sum(pyr_raw$n[pyr_raw$sex_label == lab_female], na.rm = TRUE)
  
  pyr_raw$n_plot <- ifelse(pyr_raw$sex_label == lab_male, -pyr_raw$n, pyr_raw$n)
  pyr_raw$pct    <- ifelse(
    pyr_raw$sex_label == lab_male,
    round(100 * pyr_raw$n / total_male,   1),
    round(100 * pyr_raw$n / total_female, 1)
  )
  
  max_n  <- max(abs(pyr_raw$n_plot), na.rm = TRUE)
  brks   <- pretty(c(-max_n, max_n), n = 6)
  col_map <- stats::setNames(c(pal[2], pal[1]), c(lab_male, lab_female))
  
  sex_ratio <- if (total_female > 0) round(total_male / total_female, 2) else NA
  sub_txt   <- if (!is.na(sex_ratio)) {
    paste0(.vl("sex_ratio", lang), ": ", sex_ratio)
  } else ""
  
  p <- ggplot2::ggplot(
    pyr_raw,
    ggplot2::aes(x = age_group, y = n_plot, fill = sex_label,
                 text = paste0(age_group, " | ", sex_label,
                               "\n n = ", scales::comma(abs(n_plot)),
                               " (", pct, "%)"))
  ) +
    ggplot2::geom_col(width = 0.82) +
    ggplot2::geom_hline(yintercept = 0, colour = "white", linewidth = 0.4) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      breaks = brks,
      labels = function(x) scales::comma(abs(x)),
      expand = ggplot2::expansion(mult = 0.04)
    ) +
    ggplot2::scale_fill_manual(values = col_map, name = NULL) +
    ggplot2::labs(
      title    = .vl("pyramid_title", lang),
      subtitle = sub_txt,
      x        = .vl("age_group", lang),
      y        = .vl("count", lang),
      caption  = caption
    ) +
    .lancet_theme(base_size) +
    ggplot2::theme(
      legend.position = "top",
      legend.key.size = ggplot2::unit(0.5, "lines")
    )
  
  if (interactive) {
    .require_pkg("plotly")
    return(plotly::ggplotly(p, tooltip = "text"))
  }
  p
}


# ── Map ───────────────────────────────────────────────────────────────────────
.vd_map <- function(df, map_var, lang, interactive, palette, caption, base_size, ...) {
  
  .require_pkg("geobr")
  
  uf_col   <- .find_col(df, c("manager_uf", "uf_gestor", "UF_ZI", "notification_uf",
                              "residence_uf", "uf", "SG_UF_NOT", "CS_FLXRET"))
  muni_col <- .find_col(df, c("residence_municipality_code", "municipality_code",
                              "CODMUNRES", "CO_MUNICIPIO_GESTOR", "municipio_residencia"))
  
  level  <- if (!is.null(muni_col)) "municipality" else if (!is.null(uf_col)) "state" else NULL
  
  if (is.null(level)) {
    cli::cli_abort(c(
      "No geographic column found for mapping.",
      "i" = "Expected: {.val {c('manager_uf','residence_municipality_code','CODMUNRES')}}"
    ))
  }
  
  geo_col <- if (level == "municipality") muni_col else uf_col
  
  # Aggregate
  if (map_var == "count") {
    agg <- as.data.frame(table(df[[geo_col]], useNA = "no"), stringsAsFactors = FALSE)
    names(agg) <- c("geo_id", "value")
    agg$value <- as.numeric(agg$value)
    value_label <- .vl("legend_count", lang)
  } else {
    if (!map_var %in% names(df)) {
      cli::cli_abort("Column {.val {map_var}} not found in dataset.")
    }
    agg <- stats::aggregate(df[[map_var]], list(geo_id = df[[geo_col]]), mean, na.rm = TRUE)
    names(agg)[2] <- "value"
    value_label <- map_var
  }
  
  if (level == "state") {
    # Parse numeric UF code
    agg$geo_id <- suppressWarnings(as.integer(agg$geo_id))
    shape <- get_spatial_munic_cache(
      level = "states",
      lang = "pt",
      verbose = TRUE
    )
    #shape <- geobr::read_state(year = 2010, showProgress = FALSE)
    map_sf <- merge(shape, agg, by.x = "code_state", by.y = "geo_id", all.x = TRUE)
    label_col <- "abbrev_state"
  } else {
    agg$geo_id <- suppressWarnings(as.integer(
      substr(as.character(agg$geo_id), 1, 6)
    ))
  
    shape <- get_spatial_munic_cache(
      level = "munic",
      lang = "pt",
      verbose = TRUE
    )
    #shape <- geobr::read_municipality(year = 2022, showProgress = FALSE)
    shape$code_muni6 <- as.integer(substr(as.character(shape$code_muni), 1, 6))
    map_sf <- merge(shape, agg, by.x = "code_muni6", by.y = "geo_id", all.x = TRUE)
    label_col <- "name_muni"
  }
  
  pal <- .vd_palette(palette)
  
  if (interactive) {
    .require_pkg("leaflet")
    pal_fn <- leaflet::colorNumeric(
      palette  = pal[1:5],
      domain   = map_sf$value,
      na.color = "#d0d0d0"
    )
    lbl <- paste0("<b>", map_sf[[label_col]], "</b><br>",
                  value_label, ": ", scales::comma(map_sf$value, accuracy = 1))
    out <- leaflet::leaflet(map_sf) |>
      leaflet::addProviderTiles("CartoDB.Positron") |>
      leaflet::addPolygons(
        fillColor       = ~pal_fn(value),
        fillOpacity     = 0.82,
        color           = "white",
        weight          = 0.8,
        label           = lapply(lbl, htmltools::HTML),
        highlightOptions = leaflet::highlightOptions(
          weight = 2, color = "#444", bringToFront = TRUE
        )
      ) |>
      leaflet::addLegend(
        pal    = pal_fn,
        values = ~value,
        title  = value_label,
        labFormat = leaflet::labelFormat(big.mark = ",")
      )
    return(out)
  }
  
  ggplot2::ggplot(map_sf) +
    ggplot2::geom_sf(ggplot2::aes(fill = value), colour = "white", linewidth = 0.15) +
    ggplot2::scale_fill_gradientn(
      colours  = c("#f7f7f7", pal[1]),
      name     = value_label,
      na.value = "#d0d0d0",
      labels   = scales::label_comma()
    ) +
    ggplot2::labs(
      title   = .vl("map_title", lang),
      caption = caption
    ) +
    ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(
      legend.position   = "right",
      plot.title        = ggplot2::element_text(face = "bold", size = base_size + 1,
                                                hjust = 0, margin = ggplot2::margin(b = 4)),
      plot.caption      = ggplot2::element_text(colour = "grey50", size = base_size - 2,
                                                hjust = 0),
      plot.background   = ggplot2::element_rect(fill = "white", colour = NA),
      legend.key.height = ggplot2::unit(2, "lines")
    )
}

# ── Temporal (epidemic curve / time-series) ───────────────────────────────────
.vd_temporal <- function(df, time_unit, fill_var, show_ci, lang,
                         interactive, palette, caption, base_size, ...) {
  
  time_col_map <- list(
    month    = c("month", "mes"),
    epi_week = c("epidemiological_week", "semana_epidemiologica", "SEM_NOT"),
    year     = c("year", "ano", "ANO_NOT"),
    quarter  = c("quarter", "trimestre"),
    semester = c("semester", "semestre")
  )
  
  time_col <- .find_col(df, time_col_map[[time_unit]])
  
  if (is.null(time_col)) {
    cli::cli_abort(c(
      "No column found for {.val {time_unit}}.",
      "i" = "Run {.fn sus_data_create_variables} to generate temporal variables."
    ))
  }
  
  pal <- .vd_palette(palette)
  
  if (!is.null(fill_var)) {
    fill_col <- .find_col(df, c(fill_var,
                                paste0(fill_var, "_group"),
                                gsub("_", ".", fill_var)))
    if (is.null(fill_col) || !fill_col %in% names(df)) {
      cli::cli_warn("Column {.val {fill_var}} not found; ignoring stratification.")
      fill_col <- NULL
    }
  } else {
    fill_col <- NULL
  }
  
  # Aggregate
  if (!is.null(fill_col)) {
    agg <- as.data.frame(
      table(df[[time_col]], df[[fill_col]], useNA = "no"),
      stringsAsFactors = FALSE
    )
    names(agg) <- c("time_x", "group", "n")
    agg$n <- as.numeric(agg$n)
  } else {
    agg <- as.data.frame(table(df[[time_col]], useNA = "no"), stringsAsFactors = FALSE)
    names(agg) <- c("time_x", "n")
    agg$n <- as.numeric(agg$n)
    agg$group <- .vl("count", lang)
  }
  
  agg$time_x <- type.convert(agg$time_x, as.is = TRUE)
  
  # Optional Poisson CI
  if (show_ci) {
    agg$ci_lo <- stats::qpois(0.025, agg$n)
    agg$ci_hi <- stats::qpois(0.975, agg$n)
  }
  
  x_label <- switch(time_unit,
                    month    = .vl("month",   lang),
                    epi_week = .vl("epi_week", lang),
                    year     = .vl("year",     lang),
                    quarter  = .vl("quarter",  lang),
                    semester = .vl("semester", lang)
  )
  
  n_groups <- length(unique(agg$group))
  fill_vals <- rep(pal, length.out = n_groups)
  names(fill_vals) <- unique(agg$group)
  
  p <- ggplot2::ggplot(agg, ggplot2::aes(x = time_x, y = n,
                                         colour = group, group = group,
                                         text   = paste0(x_label, ": ", time_x,
                                                         "\n", .vl("count", lang),
                                                         ": ", scales::comma(n)))) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 1.8, shape = 16)
  
  if (show_ci) {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = ci_lo, ymax = ci_hi, fill = group),
        alpha = 0.12, colour = NA, show.legend = FALSE
      ) +
      ggplot2::scale_fill_manual(values = fill_vals)
  }
  
  p <- p +
    ggplot2::scale_colour_manual(values = fill_vals, name = NULL) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                expand = ggplot2::expansion(mult = c(0, 0.08))) +
    ggplot2::labs(
      title   = .vl("temporal_title", lang),
      x       = x_label,
      y       = .vl("count", lang),
      caption = caption
    ) +
    .lancet_theme(base_size) +
    ggplot2::theme(legend.position = "top")
  
  if (interactive) {
    .require_pkg("plotly")
    return(plotly::ggplotly(p, tooltip = "text"))
  }
  p
}

# ── Climate-risk distribution ─────────────────────────────────────────────────
.vd_climate <- function(df, lang, interactive, palette, caption, base_size, ...) {
  
  clim_col   <- .find_col(df, c("climate_risk_group", "grupo_risco_climatico"))
  season_col <- .find_col(df, c("astronomical_season", "estacao_astronomica", "season"))
  biome_col  <- .find_col(df, c("biome", "bioma"))
  month_col  <- .find_col(df, c("month", "mes"))
  
  pal <- .vd_palette(palette)
  
  # Panel A: bar chart of climate_risk_group
  if (is.null(clim_col)) {
    cli::cli_abort(c(
      "Column {.val climate_risk_group} not found.",
      "i" = "Run {.fn sus_data_create_variables} to generate climate risk variables."
    ))
  }
  
  bar_d <- as.data.frame(table(df[[clim_col]], useNA = "no"), stringsAsFactors = FALSE)
  names(bar_d) <- c("group", "n")
  bar_d$pct <- round(100 * bar_d$n / sum(bar_d$n), 1)
  bar_d <- bar_d[order(bar_d$n), ]
  bar_d$group <- factor(bar_d$group, levels = bar_d$group)
  
  p_bar <- ggplot2::ggplot(bar_d, ggplot2::aes(x = group, y = n, fill = group)) +
    ggplot2::geom_col(width = 0.7, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(pct, "%")),
                       hjust = -0.15, size = base_size * 0.27, colour = "grey30") +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = grDevices::colorRampPalette(c("#f7f7f7", pal[1]))(nrow(bar_d))
    ) +
    ggplot2::scale_y_continuous(labels = scales::label_comma(),
                                expand = ggplot2::expansion(mult = c(0, 0.2))) +
    ggplot2::labs(
      title = .vl("climate_bar_title", lang),
      x = NULL, y = .vl("count", lang)
    ) +
    .lancet_theme(base_size)
  
  # Panel B: season × month heatmap (if available)
  if (!is.null(season_col) && !is.null(month_col)) {
    heat_d <- as.data.frame(
      table(df[[month_col]], df[[season_col]], useNA = "no"),
      stringsAsFactors = FALSE
    )
    names(heat_d) <- c("month", "season", "n")
    heat_d$n      <- as.numeric(heat_d$n)
    heat_d$month  <- factor(heat_d$month, levels = as.character(1:12))
    
    month_labs <- switch(lang,
                         pt = c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"),
                         es = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"),
                         c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    )
    
    p_heat <- ggplot2::ggplot(heat_d,
                              ggplot2::aes(x = month, y = season, fill = n)) +
      ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
      ggplot2::geom_text(ggplot2::aes(label = scales::comma(n)),
                         size = base_size * 0.23, colour = "grey20") +
      ggplot2::scale_x_discrete(labels = month_labs) +
      ggplot2::scale_fill_gradientn(
        colours = c("#f7f7f7", pal[2]),
        name = .vl("count", lang),
        labels = scales::label_comma()
      ) +
      ggplot2::labs(
        title = .vl("climate_heat_title", lang),
        x = .vl("month", lang), y = .vl("season", lang)
      ) +
      .lancet_theme(base_size) +
      ggplot2::theme(
        axis.text.x  = ggplot2::element_text(size = base_size - 1),
        legend.position = "right"
      )
    
    .require_pkg("patchwork")
    out <- (p_bar / p_heat) +
      patchwork::plot_annotation(caption = caption)
    return(out)
  }
  
  p_bar + ggplot2::labs(caption = caption)
}

# ── Race/colour equity diverging plot ─────────────────────────────────────────
.vd_race_equity <- function(df, benchmark, lang, interactive,
                            palette, caption, base_size, ...) {
  
  race_col <- .find_col(df, c("race", "raca", "raza", "RACACOR", "RACA_COR"))
  
  if (is.null(race_col)) {
    cli::cli_abort(c(
      "Race/colour column not found.",
      "i" = "Expected: {.val {c('race','raca','RACACOR')}}"
    ))
  }
  
  pal <- .vd_palette(palette)
  
  # Observed proportions
  obs_tab <- as.data.frame(table(df[[race_col]], useNA = "no"), stringsAsFactors = FALSE)
  names(obs_tab) <- c("race", "n")
  obs_tab$obs_pct <- 100 * obs_tab$n / sum(obs_tab$n)
  
  # IBGE 2022 Census benchmark (Censo 2022)
  ibge_2022 <- c(
    Branca    = 43.5, Parda = 45.3, Preta = 10.2,
    Amarela   = 0.5,  Indigena = 0.5,
    White     = 43.5, Brown = 45.3, Black = 10.2,
    Yellow    = 0.5,  Indigenous = 0.5,
    Blanca    = 43.5, Negra = 10.2
  )
  
  if (!is.null(benchmark)) {
    ref <- benchmark
  } else {
    matched <- ibge_2022[obs_tab$race]
    ref     <- ifelse(is.na(matched), NA_real_, matched)
  }
  
  obs_tab$ref_pct <- ref
  obs_tab <- obs_tab[!is.na(obs_tab$ref_pct), ]
  obs_tab$diff    <- round(obs_tab$obs_pct - obs_tab$ref_pct, 2)
  obs_tab$dir     <- ifelse(obs_tab$diff >= 0, .vl("overrep", lang), .vl("underrep", lang))
  obs_tab         <- obs_tab[order(obs_tab$diff), ]
  obs_tab$race    <- factor(obs_tab$race, levels = obs_tab$race)
  
  col_over  <- pal[1]
  col_under <- pal[3] %||% "#d73027"
  
  p <- ggplot2::ggplot(obs_tab,
                       ggplot2::aes(x = race, y = diff, fill = dir,
                                    text = paste0(race,
                                                  "\nObserved: ", round(obs_pct, 1), "%",
                                                  "\nReference: ", round(ref_pct, 1), "%",
                                                  "\nDiff: ", diff, " pp"))) +
    ggplot2::geom_col(width = 0.68) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.6, colour = "grey30") +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c(stats::setNames(col_over,  .vl("overrep",  lang)),
                 stats::setNames(col_under, .vl("underrep", lang))),
      name = NULL
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(ifelse(x > 0, "+", ""), x, " pp"),
      expand = ggplot2::expansion(mult = 0.12)
    ) +
    ggplot2::labs(
      title    = .vl("equity_title", lang),
      subtitle = .vl("equity_subtitle", lang),
      x        = .vl("race", lang),
      y        = .vl("equity_axis", lang),
      caption  = caption
    ) +
    .lancet_theme(base_size) +
    ggplot2::theme(legend.position = "top")
  
  if (interactive) {
    .require_pkg("plotly")
    return(plotly::ggplotly(p, tooltip = "text"))
  }
  p
}

#── Dashboard: composite Lancet panel ────────────────────────────────────────
# .vd_dashboard <- function(df, lang, interactive, palette, caption,
#                           base_size, show_ci, ...) {
# 
#   .require_pkg("patchwork")
#   pal <- .vd_palette(palette)
# 
#   collect <- function(expr) tryCatch(expr, error = function(e) {
#     cli::cli_warn("Panel skipped: {conditionMessage(e)}")
#     NULL
#   })
# 
#   p_pyr  <- collect(.vd_pyramid(df, lang, FALSE, palette, caption, base_size))
#   p_sex  <- collect(.vd_bar(df, "sex",   lang, FALSE, palette, caption, base_size))
#   p_race <- collect(.vd_bar(df, "race",  lang, FALSE, palette, caption, base_size))
#   p_age  <- collect(.vd_bar(df, "age_group", lang, FALSE, palette, caption, base_size))
#   p_time <- collect(.vd_temporal(df, "month", NULL, show_ci, lang, FALSE,
#                                  palette, caption, base_size))
#   p_map  <- collect(.vd_map(df, "count", lang, FALSE, palette, caption, base_size))
# 
#   plots  <- Filter(function(x) inherits(x, "ggplot"),
#                    list(p_pyr, p_time, p_sex, p_race, p_age, p_map))
# 
#   if (length(plots) == 0) {
#     cli::cli_abort("No valid panels could be generated for dashboard.")
#   }
# 
#   n <- length(plots)
#   ncols <- if (n <= 2) 1 else 2
# 
#   combined <- patchwork::wrap_plots(plots, ncol = ncols) +
#     patchwork::plot_annotation(
#       title    = .vl("dashboard_title", lang),
#       subtitle = paste0(
#         "N = ", format(nrow(df), big.mark = ","), " | ",
#         tryCatch(sus_meta(df, "system"), error = function(e) "DATASUS")
#       ),
#       caption  = caption,
#       tag_levels = "A",
#       theme = ggplot2::theme(
#         plot.title    = ggplot2::element_text(
#           face = "bold", size = base_size + 3, hjust = 0
#         ),
#         plot.subtitle = ggplot2::element_text(
#           colour = "grey40", size = base_size - 1, hjust = 0,
#           margin = ggplot2::margin(b = 8)
#         ),
#         plot.caption  = ggplot2::element_text(
#           colour = "grey50", size = base_size - 2, hjust = 0
#         ),
#         plot.background = ggplot2::element_rect(fill = "white", colour = NA)
#       )
#     )
# 
#   print(combined)
#   invisible(combined)
# }

.vd_dashboard <- function(df, lang, interactive, palette, caption,
                          base_size, show_ci, ...) {
  
  .require_pkg("patchwork")
  pal <- .vd_palette(palette)
  
  collect <- function(expr) {
    tryCatch(expr, error = function(e) {
      cli::cli_warn("Panel skipped: {conditionMessage(e)}")
      NULL
    })
  }
  
  # ------------------------------------------------------------
  # Generate panels safely
  # ------------------------------------------------------------
  
  p_pyr  <- collect(.vd_pyramid(df, lang, FALSE, palette, caption, base_size))
  p_time <- collect(.vd_temporal(df, "month", NULL, show_ci, lang, FALSE,
                                 palette, caption, base_size))
  p_sex  <- collect(.vd_bar(df, "sex", lang, FALSE, palette, caption, base_size))
  p_race <- collect(.vd_bar(df, "race", lang, FALSE, palette, caption, base_size))
  p_age  <- collect(.vd_bar(df, "age_group", lang, FALSE, palette, caption, base_size))
  p_map  <- collect(.vd_map(df, "count", lang, FALSE, palette, caption, base_size))
  
  # ------------------------------------------------------------
  # Validate panels
  # ------------------------------------------------------------
  
  required_left  <- list(p_pyr, p_time)
  required_right <- list(p_sex, p_race, p_age, p_map)
  
  if (any(!sapply(c(required_left, required_right), inherits, "ggplot"))) {
    cli::cli_abort("Some required panels could not be generated.")
  }
  
  # ------------------------------------------------------------
  # Left Column (60%)
  # Pyramid larger than temporal
  # ------------------------------------------------------------
  
  left_column <- p_pyr / p_time +
    patchwork::plot_layout(heights = c(3, 2))
  
  # ------------------------------------------------------------
  # Right Column (40%)
  # 2x2 balanced grid
  # ------------------------------------------------------------
  
  right_top    <- p_sex  | p_race
  right_bottom <- p_age  | p_map
  
  right_column <- right_top / right_bottom +
    patchwork::plot_layout(heights = c(1, 1))
  
  # ------------------------------------------------------------
  # Combine Columns
  # ------------------------------------------------------------
  
  combined <- left_column | right_column +
    patchwork::plot_layout(widths = c(3, 2))  # 60% / 40%
  
  # ------------------------------------------------------------
  # Global Annotation
  # ------------------------------------------------------------
  
  combined <- combined +
    patchwork::plot_annotation(
      title = .vl("dashboard_title", lang),
      subtitle = paste0(
        "N = ", format(nrow(df), big.mark = ","), " | ",
        tryCatch(sus_meta(df, "system"),
                 error = function(e) "DATASUS")
      ),
      caption = caption,
      tag_levels = "A",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(
          face = "bold",
          size = base_size + 3,
          hjust = 0
        ),
        plot.subtitle = ggplot2::element_text(
          colour = "grey40",
          size = base_size - 1,
          hjust = 0,
          margin = ggplot2::margin(b = 8)
        ),
        plot.caption = ggplot2::element_text(
          colour = "grey50",
          size = base_size - 2,
          hjust = 0
        ),
        plot.background = ggplot2::element_rect(
          fill = "white",
          colour = NA
        )
      )
    )
  
  print(combined)
  invisible(combined)
}

# =============================================================================
# ── SAVE HELPER ──────────────────────────────────────────────────────────────
# =============================================================================
.vd_save <- function(out, type, save_path, width, height, dpi, lang, verbose) {
  
  ext <- tolower(tools::file_ext(save_path))
  
  if (type == "dashboard" && ext == "html") {
    .require_pkg("htmltools")
    if (inherits(out, "ggplot")) {
      .require_pkg("svglite")
      tmp_svg <- tempfile(fileext = ".svg")
      ggplot2::ggsave(tmp_svg, out, width = width, height = height, device = "svg")
      htmltools::save_html(htmltools::tags$img(src = tmp_svg), file = save_path)
    }
  } else if (inherits(out, "ggplot")) {
    ggplot2::ggsave(save_path, plot = out,
                    width = width, height = height,
                    dpi   = dpi, bg = "white")
    if (verbose) cli::cli_alert_success(.vl("saved_to", lang, save_path))
  } else if (inherits(out, c("gt_tbl"))) {
    if (ext %in% c("html", "htm")) {
      gt::gtsave(out, save_path)
    } else if (ext == "pdf") {
      gt::gtsave(out, save_path)
    }
    if (verbose) cli::cli_alert_success(.vl("saved_to", lang, save_path))
  } else if (inherits(out, "htmlwidget")) {
    .require_pkg("htmlwidgets")
    htmlwidgets::saveWidget(out, save_path, selfcontained = TRUE)
    if (verbose) cli::cli_alert_success(.vl("saved_to", lang, save_path))
  } else {
    cli::cli_warn("Cannot auto-save object of class {.cls {class(out)}}.")
  }
}


# =============================================================================
# ── THEME & PALETTE ──────────────────────────────────────────────────────────
# =============================================================================

#' @title Lancet-style ggplot2 theme
#' @noRd
.lancet_theme <- function(base_size = 11) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      # Text
      plot.title        = ggplot2::element_text(face  = "bold",
                                                size  = base_size + 1,
                                                hjust = 0,
                                                margin = ggplot2::margin(b = 4)),
      plot.subtitle     = ggplot2::element_text(colour = "grey40",
                                                size  = base_size - 1,
                                                hjust = 0,
                                                margin = ggplot2::margin(b = 6)),
      plot.caption      = ggplot2::element_text(colour = "grey50",
                                                size  = base_size - 2,
                                                hjust = 0,
                                                margin = ggplot2::margin(t = 6)),
      axis.title        = ggplot2::element_text(size  = base_size - 0.5, colour = "grey20"),
      axis.text         = ggplot2::element_text(size  = base_size - 1,   colour = "grey20"),
      axis.line         = ggplot2::element_line(colour = "grey30", linewidth = 0.4),
      axis.ticks        = ggplot2::element_line(colour = "grey60", linewidth = 0.3),
      # Grid
      panel.grid.major.x = ggplot2::element_line(colour = "grey92", linewidth = 0.35),
      panel.grid.major.y = ggplot2::element_line(colour = "grey92", linewidth = 0.35),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.background   = ggplot2::element_rect(fill = "white", colour = NA),
      # Legend
      legend.background  = ggplot2::element_blank(),
      legend.key         = ggplot2::element_blank(),
      legend.text        = ggplot2::element_text(size = base_size - 1),
      legend.key.size    = ggplot2::unit(0.75, "lines"),
      # Strip (facets)
      strip.background   = ggplot2::element_rect(fill = "grey95", colour = NA),
      strip.text         = ggplot2::element_text(face = "bold", size = base_size - 1),
      # Margins
      plot.background    = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin        = ggplot2::margin(8, 10, 6, 8)
    )
}

#' @title Colour palettes
#' @noRd
.vd_palette <- function(name = "lancet") {
  palettes <- list(
    # The Lancet – NEJM – Nature core colour palettes (adapted from ggsci)
    sunset <- c(
      "#2F4F8F",  # azul profundo
      "#5D739F",  # azul medio
      "#F4D19B",  # dourado claro
      "#F9AC4A",  # laranja vibrante
      "#E65100"   # laranja intenso
    ),
    lancet     = c("#00468B", "#ED0000", "#42B540", "#0099B4",
                   "#925E9F", "#FDAF91", "#AD002A", "#ADB6B6"),
    nature     = c("#4DBBD5", "#E64B35", "#00A087", "#3C5488",
                   "#F39B7F", "#8491B4", "#91D1C2", "#DC0000"),
    nejm       = c("#BC3C29", "#0072B5", "#E18727", "#20854E",
                   "#7876B1", "#6F99AD", "#FFDC91", "#EE4C97"),
    sus        = c("#1B6CA8", "#E84855", "#3BB273", "#F4A261",
                   "#7B2D8B", "#2EC4B6", "#FF9F1C", "#CBCBCB"),
    colorblind = c("#0072B2", "#D55E00", "#009E73", "#CC79A7",
                   "#56B4E9", "#E69F00", "#F0E442", "#999999"),
    viridis    = scales::viridis_pal()(8)
  )
  palettes[[name]] %||% palettes[["lancet"]]
}


# =============================================================================
# ── COLUMN DETECTION ─────────────────────────────────────────────────────────
# =============================================================================

#' @noRd
.vd_detect_cols <- function(df) {
  list(
    sex          = .find_col(df, c("sex", "sexo", "SEXO", "CS_SEXO")),
    race         = .find_col(df, c("race", "raca", "raza", "RACACOR", "RACA_COR", "CS_RACA")),
    age          = .find_col(df, c("age_years", "idade", "edad", "NU_IDADE_N")),
    age_group    = .find_col(df, c("age_group", "ibge_age_group", "faixa_etaria",
                                   "grupo_etario", "age_group_5yr")),
    ibge_age_group    = .find_col(df, c("ibge_age_group")),
    education    = .find_col(df, c("education_level", "education", "escolaridade", "escolaridad",
                                   "ESC", "ESC2010", "CS_ESCOL_N")),
    climate_risk = .find_col(df, c("climate_risk_group", "grupo_risco_climatico")),
    region       = .find_col(df, c("manager_uf", "uf_gestor", "UF_ZI",
                                   "SG_UF_NOT", "notification_uf")),
    municipality = .find_col(df, c("residence_municipality_code", "municipality_code",
                                   "CODMUNRES", "municipio_residencia"))
  )
}

#' @noRd
.find_col <- function(df, patterns) {
  for (p in patterns) {
    if (p %in% names(df)) return(p)
  }
  NULL
}


# =============================================================================
# ── MULTILINGUAL LABELS ──────────────────────────────────────────────────────
# =============================================================================

.vd_labels <- list(
  
  # Meta
  system_detected    = list(en = "System detected: %s",
                            pt = "Sistema detectado: %s",
                            es = "Sistema detectado: %s"),
  saved_to           = list(en = "Output saved: %s",
                            pt = "Saida salva: %s",
                            es = "Salida guardada: %s"),
  
  # Dimensions
  sex          = list(en = "Sex",          pt = "Sexo",         es = "Sexo"),
  race         = list(en = "Race/Colour",  pt = "Raca/Cor",     es = "Raza/Color"),
  age          = list(en = "Age (years)",  pt = "Idade (anos)", es = "Edad (anos)"),
  age_group    = list(en = "Age Group",    pt = "Faixa Etaria", es = "Grupo de Edad"),
  education    = list(en = "Education",    pt = "Escolaridade", es = "Escolaridad"),
  climate_risk = list(en = "Climate Risk", pt = "Risco Climatico", es = "Riesgo Climatico"),
  region       = list(en = "State/Region", pt = "Estado/Regiao", es = "Estado/Region"),
  municipality = list(en = "Municipality", pt = "Municipio",    es = "Municipio"),
  dimension    = list(en = "Dimension",    pt = "Dimensao",     es = "Dimension"),
  
  # Table labels
  demographic_summary = list(
    en = "Demographic Summary",
    pt = "Resumo Demografico",
    es = "Resumen Demografico"
  ),
  category    = list(en = "Category",  pt = "Categoria", es = "Categoria"),
  count       = list(en = "Count",     pt = "Contagem",  es = "Conteo"),
  percent     = list(en = "Percent",   pt = "Percentual", es = "Porcentaje"),
  
  # Sex labels
  male   = list(en = "Male",   pt = "Masculino", es = "Masculino"),
  female = list(en = "Female", pt = "Feminino",  es = "Femenino"),
  sex_ratio = list(en = "Sex ratio (M:F)", pt = "Razao de sexo (M:F)", es = "Razon de sexo (M:F)"),
  
  # Bar titles
  bar_title_sex       = list(en = "Distribution by Sex",
                             pt = "Distribuicao por Sexo",
                             es = "Distribucion por Sexo"),
  bar_title_race      = list(en = "Distribution by Race/Colour",
                             pt = "Distribuicao por Raca/Cor",
                             es = "Distribucion por Raza/Color"),
  bar_title_age_group = list(en = "Distribution by Age Group",
                             pt = "Distribuicao por Faixa Etaria",
                             es = "Distribucion por Grupo de Edad"),
  bar_title_education = list(en = "Distribution by Education Level",
                             pt = "Distribuicao por Escolaridade",
                             es = "Distribucion por Nivel Educativo"),
  bar_title_climate_risk = list(
    en = "Distribution by Climate Risk Group",
    pt = "Distribuicao por Grupo de Risco Climatico",
    es = "Distribucion por Grupo de Riesgo Climatico"
  ),
  bar_title_region    = list(en = "Distribution by State/Region",
                             pt = "Distribuicao por Estado/Regiao",
                             es = "Distribucion por Estado/Region"),
  
  # Pyramid
  pyramid_title = list(
    en = "Age–Sex Population Pyramid",
    pt = "Piramide Etaria por Sexo",
    es = "Piramide de Edad y Sexo"
  ),
  
  # Map
  map_title    = list(en = "Geographic Distribution of Cases",
                      pt = "Distribuicao Geografica dos Casos",
                      es = "Distribucion Geografica de los Casos"),
  legend_count = list(en = "Records",   pt = "Registros",  es = "Registros"),
  
  # Temporal
  temporal_title = list(en = "Temporal Distribution of Cases",
                        pt = "Distribuicao Temporal dos Casos",
                        es = "Distribucion Temporal de los Casos"),
  month     = list(en = "Month",             pt = "Mes",                  es = "Mes"),
  epi_week  = list(en = "Epidemiological Week", pt = "Semana Epidemiologica", es = "Semana Epidemiologica"),
  year      = list(en = "Year",              pt = "Ano",                  es = "Ano"),
  quarter   = list(en = "Quarter",           pt = "Trimestre",            es = "Trimestre"),
  semester  = list(en = "Semester",          pt = "Semestre",             es = "Semestre"),
  
  # Climate panels
  climate_bar_title = list(
    en = "Cases by Climate Risk Group",
    pt = "Casos por Grupo de Risco Climatico",
    es = "Casos por Grupo de Riesgo Climatico"
  ),
  climate_heat_title = list(
    en = "Seasonal Distribution (Month × Season)",
    pt = "Distribuicao Sazonal (Mes × Estacao)",
    es = "Distribucion Estacional (Mes × Estacion)"
  ),
  season = list(en = "Season", pt = "Estacao", es = "Estacion"),
  
  # Equity
  equity_title    = list(
    en = "Race/Colour Equity Analysis vs. National Census Benchmark",
    pt = "Analise de Equidade Racial vs. Censo Nacional (IBGE 2022)",
    es = "Analisis de Equidad Racial vs. Censo Nacional (IBGE 2022)"
  ),
  equity_subtitle = list(
    en = "Percentage-point difference: observed vs. IBGE 2022 Census proportions",
    pt = "Diferenca em pontos percentuais: observado vs. Censo IBGE 2022",
    es = "Diferencia en puntos porcentuales: observado vs. Censo IBGE 2022"
  ),
  equity_axis = list(
    en = "Difference from national proportion (pp)",
    pt = "Diferenca da proporcao nacional (pp)",
    es = "Diferencia de la proporcion nacional (pp)"
  ),
  overrep  = list(en = "Over-represented",  pt = "Sobre-representado",  es = "Sobrerrepresentado"),
  underrep = list(en = "Under-represented", pt = "Sub-representado",    es = "Subrepresentado"),
  
  # Dashboard
  dashboard_title = list(
    en = "Demographic Profile",
    pt = "Perfil Demografico",
    es = "Perfil Demografico"
  ),
  
  # Source
  source_datasus = list(
    en = "Source: DATASUS / Brazilian Ministry of Health",
    pt = "Fonte: DATASUS / Ministerio da Saude",
    es = "Fuente: DATASUS / Ministerio de Salud de Brasil"
  )
)

#' @noRd
.vl <- function(key, lang, ...) {
  row <- .vd_labels[[key]]
  if (is.null(row)) return(key)
  txt <- row[[lang]] %||% row[["en"]] %||% key
  if (...length() > 0) {
    return(sprintf(txt, ...))
  }
  txt
}


# =============================================================================
# ── UTILITY ──────────────────────────────────────────────────────────────────
# =============================================================================

#' @noRd
.require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg {pkg}} is required but not installed.
      Install with {.code install.packages('{pkg}')}."
    )
  }
}

#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b



# ============================================================================
# HELPER FUNCTIONS
# ============================================================================


#' Get Spatial Municipality Data with Caching
#'
#' Retrieves spatial data from IBGE with intelligent caching to improve performance.
#' Spatial data is cached as Parquet files for fast reloading.
#'
#' @param level Geographic level ("munic")
#' @param cache_dir Cache directory path
#' @param use_cache Whether to use cache
#' @param lang Language for messages
#' @param verbose Print messages
#'
#' @return sf object with spatial data
#' @keywords internal
#' @noRd
get_spatial_munic_cache <- function(
    level = "munic",
    cache_dir = "/Users/maxanjos/.climasus4r_cache/spatial",
    use_cache = TRUE, 
    lang,
    verbose
) {
  msg <- get_spatial_messages(lang)
  
  if ((requireNamespace("sfarrow", quietly = TRUE))) { 
    # Define cache file name
    cache_file <- file.path(
      cache_dir,
      paste0(level, "_", ".parquet")
    )
  } else { 
    # Define cache file name
    cache_file <- file.path(
      cache_dir,
      paste0(level, "_", ".gpkg")
    )
  }
  
  # Try to load from cache
  if (use_cache && file.exists(cache_file)) {
    if (verbose) {
      cli::cli_alert_success(paste0(msg$loading_cache, basename(cache_file)))
    }
    
    tryCatch(
      {
        if ((requireNamespace("sfarrow", quietly = TRUE))) { 
          spatial_df <- sfarrow::st_read_parquet(cache_file)
        } else { 
          spatial_df <- sf::st_read(cache_file)
          spatial_df <- sf::st_as_sf(spatial_df)
        }
        return(spatial_df)
      },
      error = function(e) {
        if (verbose) {
          cli::cli_alert_warning(msg$cache_error)
        }
      }
    )
  }
  
  # Download from IBGE
  if (verbose) {
    cli::cli_alert_info(msg$downloading_data)
  }
  
  options(timeout = 600)
  spatial_df <- switch(
    level,
    "munic" = geobr::read_municipality(
      code_muni = "all",
      simplified = TRUE,
      showProgress = verbose,
      cache = FALSE,
    ),
    "states" = geobr::read_state(
      code_state = "all",
      simplified = TRUE,
      showProgress = verbose,
      cache = FALSE,
    )
  )
  
  # Save to cache
  if (use_cache) {
    if (verbose) {
      cli::cli_alert_info(msg$saving_cache)
    }
    
    tryCatch(
      { 
        if ((requireNamespace("sfarrow", quietly = TRUE))) { 
          sfarrow::st_write_parquet(obj = spatial_df, dsn = cache_file)
        } else {
          sf::st_write(spatial_df, cache_file, driver = "GPKG", quiet = TRUE, delete_dsn = TRUE, append = TRUE)
        }
        if (verbose) {
          cli::cli_alert_success(msg$cache_saved)
        }
      },
      error = function(e) {
        if (verbose) {
          cli::cli_alert_warning(paste0(msg$cache_save_error, e$message))
        }
      }
    )
  }
  
  return(spatial_df)
}


#' Get Multilingual Messages for Spatial Operations
#'
#' Returns user interface messages in the specified language for spatial join operations.
#'
#' @param lang Character. Language code: "en", "pt", "es"
#'
#' @return List of translated UI messages
#' @keywords internal
#' @noRd
get_spatial_messages <- function(lang) {
  messages <- list(
    en = list(
      invalid_level = "Invalid geographic level specified.",
      valid_levels = "Valid levels are: 'state', 'munic', 'census', 'cep'.",
      system_column_missing = "Column 'system' not found in data. Please run detect_health_system() first.",
      cep_restricted = "Level 'cep' is only available for SIH and CNES systems.",
      cep_allowed = "CEP geocoding is available for this system.",
      detecting_column = "Auto-detecting geographic column...",
      no_column_found = "No valid geographic column found for the selected level.",
      expected_columns = "Expected columns: ",
      column_detected = "Geographic column detected: ",
      column_not_found = "Column not found in data frame: ",
      creating_cache = "Creating cache directory: ",
      cache_enabled = "Cache: ENABLED",
      loading_cache = "Loading from cache: ",
      cache_error = "Cache loading failed. Downloading fresh data...",
      downloading_data = "Downloading spatial data from IBGE...",
      saving_cache = "Saving to cache...",
      cache_saved = "Spatial data cached successfully.",
      cache_save_error = "Failed to save cache: ",
      joining_data = "Performing spatial join...",
      rows_removed = "Removed rows with missing geometries: ",
      geocoding_cep = "Geocoding postal codes (CEP)...",
      geocoding_count = "Geocoding unique CEPs: ",
      geocoding_error = "Geocoding failed: ",
      join_success = "Spatial join completed successfully!",
      final_rows = "Final dataset rows: "
    ),
    pt = list(
      invalid_level = "Nivel geografico invalido especificado.",
      valid_levels = "Niveis validos sao: 'state', 'munic', 'census', 'cep'.",
      system_column_missing = "Coluna 'system' nao encontrada nos dados. Execute detect_health_system() primeiro.",
      cep_restricted = "Nivel 'cep' disponivel apenas para os sistemas SIH e CNES.",
      cep_allowed = "Geocodificacao de CEP disponivel para este sistema.",
      detecting_column = "Auto-detectando coluna geografica...",
      no_column_found = "Nenhuma coluna geografica valida encontrada para o nivel selecionado.",
      expected_columns = "Colunas esperadas: ",
      column_detected = "Coluna geografica detectada: ",
      column_not_found = "Coluna nao encontrada no data frame: ",
      creating_cache = "Criando diretorio de cache: ",
      cache_enabled = "Cache: ATIVADO",
      loading_cache = "Carregando do cache: ",
      cache_error = "Falha ao carregar cache. Baixando dados novos...",
      downloading_data = "Baixando dados espaciais do IBGE...",
      saving_cache = "Salvando no cache...",
      cache_saved = "Dados espaciais armazenados em cache com sucesso.",
      cache_save_error = "Falha ao salvar cache: ",
      joining_data = "Realizando juncao espacial...",
      rows_removed = "Linhas removidas com geometrias faltantes: ",
      geocoding_cep = "Geocodificando codigos postais (CEP)...",
      geocoding_count = "Geocodificando CEPs unicos: ",
      geocoding_error = "Geocodificacao falhou: ",
      join_success = "Juncao espacial concluida com sucesso!",
      final_rows = "Linhas no dataset final: "
    ),
    es = list(
      invalid_level = "Nivel geografico invalido especificado.",
      valid_levels = "Niveles validos son: 'state', 'munic', 'census', 'cep'.",
      system_column_missing = "Columna 'system' no encontrada en los datos. Ejecute detect_health_system() primero.",
      cep_restricted = "Nivel 'cep' disponible solo para los sistemas SIH y CNES.",
      cep_allowed = "Geocodificacion de CEP disponible para este sistema.",
      detecting_column = "Auto-detectando columna geografica...",
      no_column_found = "Ninguna columna geografica valida encontrada para el nivel seleccionado.",
      expected_columns = "Columnas esperadas: ",
      column_detected = "Columna geografica detectada: ",
      column_not_found = "Columna no encontrada en el data frame: ",
      creating_cache = "Creando directorio de cache: ",
      cache_enabled = "Cache: ACTIVADO",
      loading_cache = "Cargando desde cache: ",
      cache_error = "Fallo al cargar cache. Descargando datos nuevos...",
      downloading_data = "Descargando datos espaciales del IBGE...",
      saving_cache = "Guardando en cache...",
      cache_saved = "Datos espaciales almacenados en cache con exito.",
      cache_save_error = "Fallo al guardar cache: ",
      joining_data = "Realizando union espacial...",
      rows_removed = "Filas eliminadas con geometrias faltantes: ",
      geocoding_cep = "Geocodificando codigos postales (CEP)...",
      geocoding_count = "Geocodificando CEPs unicos: ",
      geocoding_error = "Geocodificacion fallo: ",
      join_success = "Union espacial completada con exito!",
      final_rows = "Filas en el dataset final: "
    )
  )
  
  return(messages[[lang]])
}
