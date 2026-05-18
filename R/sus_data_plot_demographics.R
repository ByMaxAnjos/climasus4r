# =============================================================================
# sus_data_plot_demographics.R
# Publication-quality demographic visualisation for climasus4r
# Palettes: ggsci (Lancet, NPG, NEJM, JCO, AAAS) + viridis + colorblind + SUS
# Style: The Lancet / Nature Medicine . PT / EN / ES
# =============================================================================

# -- NSE variable declarations (suppresses R CMD CHECK warnings) ---------------
utils::globalVariables(c(
  # bar / table
  "category", "n", "pct", "dimension",
  # pyramid
  "age_group", "sex_raw", "sex_label", "n_plot",
  # heatmap
  "row_val", "col_val", "fill_val",
  # temporal
  "time_x", "group", "ci_lo", "ci_hi",
  # climate heatmap
  "month", "season",
  # race-equity
  "obs_pct", "ref_pct", "diff", "dir", "race"
))

# =============================================================================
# -- EXPORTED FUNCTION ---------------------------------------------------------
# =============================================================================

#' Visualise Demographic Profiles from DATASUS Systems
#'
#' Produces publication-quality tables, charts, and composite dashboards
#' summarising the demographic and climate-risk composition of any
#' standardised `climasus_df` dataset (SIM, SIH, SINAN, CNES, SIA, SINASC).
#' Visual style follows The Lancet / Nature Medicine guidelines.
#' Colour palettes are powered by [ggsci].
#'
#' @param df A `climasus_df` object at stage `"filter_demo"` or later (output
#'   of [sus_data_filter_demographics()]).
#' @param type Character. Visualisation type. One of:
#'   - `"table"`      -- Frequency table (gt / DT).
#'   - `"bar"`        -- Horizontal bar chart for one variable.
#'   - `"pyramid"`    -- Age-sex population pyramid.
#'   - `"heatmap"`    -- Cross-demographic tile matrix (two variables).
#'   - `"temporal"`   -- Epidemic curve / time-series.
#'   - `"climate"`    -- Climate-risk group distribution (bar + season heatmap).
#'   - `"race_equity"`-- Race/colour equity diverging plot vs. IBGE 2022 Census.
#'   - `"dashboard"`  -- Composite Lancet-layout panel.
#' @param var Character. Demographic variable (required for `"bar"` and
#'   single-variable `"table"`). One of `"sex"`, `"race"`, `"age_group"`,
#'   `"education"`, `"climate_risk"`, `"region"`.
#' @param time_unit Character. Temporal resolution for `type = "temporal"`.
#'   One of `"month"` (default), `"epi_week"`, `"year"`, `"quarter"`,
#'   `"semester"`.
#' @param fill_var Character. Optional stratification variable for temporal
#'   plots (e.g., `"sex"`, `"age_group"`, `"climate_risk_group"`).
#' @param palette Character. Colour palette powered by [ggsci]. One of
#'   `"lancet"` (default), `"nature"`, `"nejm"`, `"jco"`, `"aaas"`,
#'   `"sus"`, `"viridis"`, `"colorblind"`.
#' @param heatmap_row Character. Row variable for `type = "heatmap"`. One of
#'   `"age_group"`, `"sex"`, `"race"`, `"education"`, `"climate_risk"`,
#'   `"region"`. Defaults to `"age_group"` (auto-detected).
#' @param heatmap_col Character. Column variable for `type = "heatmap"`.
#'   Defaults to `"race"` (auto-detected). Must differ from `heatmap_row`.
#' @param fill_metric Character. What to show in each tile for
#'   `type = "heatmap"`. One of `"pct_row"` (default -- % within each row,
#'   e.g. % of each age group that is Parda), `"pct_col"` (% within each
#'   column), `"pct_total"` (% of all records), `"count"` (raw count).
#' @param show_ci Logical. Add 95% Poisson confidence intervals in temporal
#'   plots. Default `FALSE`.
#' @param benchmark Numeric named vector. Reference proportions for the
#'   race-equity plot. If `NULL` (default), IBGE 2022 Census proportions
#'   are used where available.
#' @param interactive Logical. Return interactive [plotly::ggplotly()] widget
#'   instead of static ggplot2/gt objects. Default `FALSE`.
#' @param base_size Numeric. Base font size for ggplot2 theme. Default `11`.
#' @param lang Character. Language for labels and messages. One of `"pt"`
#'   (default), `"en"`, `"es"`.
#' @param caption_suffix Character. Additional text appended to the figure
#'   caption (e.g., study period, DOI). Default `NULL`.
#' @param save_path Character. File path to save output (PNG, PDF, SVG, or
#'   HTML). Default `NULL` (no file saved).
#' @param width,height Numeric. Output dimensions in inches. Defaults: `7 x 5`
#'   for single plots; `12 x 9` for dashboards.
#' @param dpi Numeric. Resolution for raster output. Default `300`.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#' @param ... Additional arguments passed to underlying plot helpers.
#'
#' @return Invisibly returns the primary plot or list of plots. Side effect:
#'   renders in the active graphics device (or interactive widget).
#'
#' @section Systems supported:
#' Automatically adapts column detection to SINAN, SIM, SIH, CNES, SIA, and
#' SINASC. Run `sus_data_create_variables()` before this function to generate
#' `age_group`, `year`, `month`, `epidemiological_week`, and
#' `climate_risk_group`.
#'
#' @section Climate integration:
#' When `climate_risk_group` is present (created by
#' `sus_data_create_variables()`), `type = "climate"` produces a combined
#' bar + seasonal heatmap designed for climate-health submissions to
#' *The Lancet Planetary Health*.
#'
#' @examples
#' \dontrun{
#' # Age-sex pyramid (Lancet palette, Portuguese)
#' sus_data_plot_demographics(df, type = "pyramid", lang = "pt")
#'
#' # Composite dashboard saved as PDF
#' sus_data_plot_demographics(
#'   df,
#'   type      = "dashboard",
#'   palette   = "lancet",
#'   lang      = "en",
#'   save_path = "figures/fig1_demographics.pdf",
#'   width = 12, height = 9
#' )
#'
#' # Epidemic curve stratified by climate risk group (with CI)
#' sus_data_plot_demographics(
#'   df,
#'   type      = "temporal",
#'   time_unit = "epi_week",
#'   fill_var  = "climate_risk_group",
#'   show_ci   = TRUE,
#'   lang      = "pt"
#' )
#'
#' # Race equity diverging plot
#' sus_data_plot_demographics(df, type = "race_equity", lang = "pt")
#'
#' # Cross-demographic heatmap: age group x race, % within each age group
#' sus_data_plot_demographics(
#'   df,
#'   type        = "heatmap",
#'   heatmap_row = "age_group",
#'   heatmap_col = "race",
#'   fill_metric = "pct_row",
#'   lang        = "pt"
#' )
#'
#' # Cross-demographic heatmap: education x climate risk (absolute counts)
#' sus_data_plot_demographics(
#'   df,
#'   type        = "heatmap",
#'   heatmap_row = "education",
#'   heatmap_col = "climate_risk",
#'   fill_metric = "count",
#'   palette     = "nejm"
#' )
#' }
#'
#' @export
#' @importFrom dplyr filter mutate select arrange count rename left_join
#'   case_when n desc
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning
#'   cli_abort cli_warn
#' @importFrom rlang check_installed .data
#' @importFrom stats setNames qpois
sus_data_plot_demographics <- function(
    df,
    type           = "table",
    var            = NULL,
    time_unit      = "month",
    fill_var       = NULL,
    palette        = "lancet",
    heatmap_row    = NULL,
    heatmap_col    = NULL,
    fill_metric    = "pct_row",
    show_ci        = FALSE,
    benchmark      = NULL,
    interactive    = FALSE,
    base_size      = 11,
    lang           = "pt",
    caption_suffix = NULL,
    save_path      = NULL,
    width          = NULL,
    height         = NULL,
    dpi            = 300,
    verbose        = TRUE,
    ...
) {
  # -- 1. Dependency checks ----------------------------------------------------
  rlang::check_installed(
    c("ggplot2", "ggsci", "patchwork", "scales"),
    reason = "to run {.fn sus_data_plot_demographics}"
  )

  # -- 2. Materialise Arrow / lazy inputs --------------------------------------
  if (inherits(df, c("arrow_dplyr_query", "Dataset", "ArrowTabular", "Table"))) {
    if (verbose) cli::cli_alert_info("Materialising Arrow dataset...")
    meta_backup <- tryCatch(sus_meta(df), error = function(e) list())
    df <- dplyr::collect(df)
    if (length(meta_backup) > 0L) df <- new_climasus_df(df, meta_backup)
  }

  # -- 3. Validate input type --------------------------------------------------
  if (!is.data.frame(df)) {
    cli::cli_abort(
      c("Input {.arg df} must be a {.cls data.frame} or collectable Arrow object.",
        "i" = "Use {.fn sus_data_import} to create a {.cls climasus_df}.")
    )
  }

  # -- 4. Stage validation -----------------------------------------------------
  if (inherits(df, "climasus_df")) {
    current_stage <- sus_meta(df, "stage")
    if (!is_stage_at_least(current_stage, "filter_demo")) {
      cli::cli_abort(c(
        "Stage {.val filter_demo} required; current stage: {.val {current_stage %||% 'unknown'}}.",
        "i" = "Run the full pipeline first:",
        "*" = "{.code df <- sus_data_import(...)}",
        "*" = "{.code df <- sus_data_standardize(df)}",
        "*" = "{.code df <- sus_data_create_variables(df)}",
        "*" = "{.code df <- sus_data_filter_demographics(df)}"
      ))
    }
  }

  # -- 5. Validate arguments ---------------------------------------------------
  if (!lang %in% c("pt", "en", "es")) {
    cli::cli_alert_warning(
      "{.arg lang} {.val {lang}} not supported. Using {.val pt}."
    )
    lang <- "pt"
  }

  valid_types <- c("table", "bar", "pyramid", "heatmap",
                   "temporal", "climate", "race_equity", "dashboard")
  if (!type %in% valid_types) {
    cli::cli_abort("{.arg type} must be one of: {.val {valid_types}}")
  }

  valid_time <- c("month", "epi_week", "year", "quarter", "semester")
  if (!time_unit %in% valid_time) {
    cli::cli_abort("{.arg time_unit} must be one of: {.val {valid_time}}")
  }

  # -- 6. Detect DATASUS system ------------------------------------------------
  system_id <- tryCatch(sus_meta(df, "system"), error = function(e) "unknown")
  if (verbose) {
    cli::cli_alert_info(.vl("system_detected", lang, system_id %||% "unknown"))
  }

  # -- 7. Build caption --------------------------------------------------------
  cap_base <- .vl("source_datasus", lang)
  caption  <- if (!is.null(caption_suffix)) {
    paste0(cap_base, " | ", caption_suffix)
  } else {
    cap_base
  }

  # -- 8. Dispatch -------------------------------------------------------------
  out <- switch(
    type,
    "table"       = .vd_table(df, var, lang, interactive, palette, caption, base_size, ...),
    "bar"         = .vd_bar(df, var, lang, interactive, palette, caption, base_size, ...),
    "pyramid"     = .vd_pyramid(df, lang, interactive, palette, caption, base_size, ...),
    "heatmap"     = .vd_heatmap(df, heatmap_row, heatmap_col, fill_metric, lang,
                                interactive, palette, caption, base_size, ...),
    "temporal"    = .vd_temporal(df, time_unit, fill_var, show_ci, lang,
                                 interactive, palette, caption, base_size, ...),
    "climate"     = .vd_climate(df, lang, interactive, palette, caption, base_size, ...),
    "race_equity" = .vd_race_equity(df, benchmark, lang, interactive, palette,
                                    caption, base_size, ...),
    "dashboard"   = .vd_dashboard(df, lang, interactive, palette, caption,
                                  base_size, show_ci, ...)
  )

  # -- 9. Save -----------------------------------------------------------------
  if (!is.null(save_path)) {
    .vd_save(
      out, type, save_path,
      width  = width  %||% if (type == "dashboard") 12 else 7,
      height = height %||% if (type == "dashboard") 9  else 5,
      dpi    = dpi,
      lang   = lang,
      verbose = verbose
    )
  }

  invisible(out)
}


# =============================================================================
# -- INTERNAL PLOT FUNCTIONS ---------------------------------------------------
# =============================================================================

# -- Table ---------------------------------------------------------------------
#' @noRd
.vd_table <- function(df, var, lang, interactive, palette, caption, base_size, ...) {

  demo_cols <- .vd_detect_cols(df)
  pal        <- .vd_palette(palette)

  if (is.null(var)) {
    rows <- lapply(names(demo_cols), function(dim) {
      col <- demo_cols[[dim]]
      if (is.null(col) || !col %in% names(df)) return(NULL)
      tab <- df |>
        dplyr::count(.data[[col]], name = "n") |>
        dplyr::mutate(
          category  = as.character(.data[[col]]),
          dimension = .vl(dim, lang),
          pct       = round(100 * n / sum(n, na.rm = TRUE), 1)
        ) |>
        dplyr::select(dimension, category, n, pct) |>
        dplyr::arrange(dplyr::desc(n))
      as.data.frame(tab)
    })
    tbl_data <- do.call(rbind, Filter(Negate(is.null), rows))
  } else {
    col <- demo_cols[[var]]
    if (is.null(col) || !col %in% names(df)) {
      cli::cli_abort("Column for {.val {var}} not found in dataset.")
    }
    tbl_data <- df |>
      dplyr::count(.data[[col]], name = "n") |>
      dplyr::mutate(
        category = as.character(.data[[col]]),
        pct      = round(100 * n / sum(n, na.rm = TRUE), 1)
      ) |>
      dplyr::select(category, n, pct) |>
      dplyr::arrange(dplyr::desc(n)) |>
      as.data.frame()
  }

  if (interactive) {
    rlang::check_installed("DT", reason = "for interactive tables")
    return(DT::datatable(
      tbl_data,
      caption  = .vl("demographic_summary", lang),
      options  = list(pageLength = 25, scrollX = TRUE,
                      dom = "Bfrtip", buttons = c("csv", "excel")),
      rownames = FALSE
    ))
  }

  rlang::check_installed("gt", reason = "for static demographic tables")

  col_labels <- list(
    dimension = .vl("dimension", lang),
    category  = .vl("category", lang),
    n         = .vl("count", lang),
    pct       = .vl("percent", lang)
  )

  gt::gt(tbl_data) |>
    gt::tab_header(
      title    = gt::md(paste0("**", .vl("demographic_summary", lang), "**")),
      subtitle = gt::md(paste0("*n* = ", format(nrow(df), big.mark = ",")))
    ) |>
    gt::cols_label(.list = col_labels[names(col_labels) %in% names(tbl_data)]) |>
    gt::fmt_integer(columns = "n", use_seps = TRUE) |>
    gt::fmt_number(columns = "pct", decimals = 1, suffix = "%") |>
    gt::data_color(columns = "pct", palette = c("#f7f7f7", pal[1])) |>
    gt::tab_source_note(caption) |>
    gt::tab_options(
      table.font.size                   = gt::px(base_size),
      heading.title.font.size           = gt::px(base_size + 2),
      column_labels.font.weight         = "bold",
      table.border.top.color            = "#000000",
      table.border.top.width            = gt::px(2),
      column_labels.border.bottom.color = "#000000",
      column_labels.border.bottom.width = gt::px(1),
      table_body.border.bottom.color    = "#000000",
      table_body.border.bottom.width    = gt::px(1.5)
    )
}


# -- Bar chart -----------------------------------------------------------------
#' @noRd
.vd_bar <- function(df, var, lang, interactive, palette, caption, base_size, ...) {

  if (is.null(var)) {
    cli::cli_abort(c(
      "{.arg var} is required for {.code type = 'bar'}.",
      "i" = "Options: {.val {c('sex','race','age_group','education','climate_risk','region')}}"
    ))
  }

  demo_cols <- .vd_detect_cols(df)
  col       <- demo_cols[[var]]
  pal       <- .vd_palette(palette)

  if (is.null(col) || !col %in% names(df)) {
    cli::cli_abort(
      "Column for {.val {var}} not found. Run {.fn sus_data_create_variables} first."
    )
  }

  bar_data <- df |>
    dplyr::filter(!is.na(.data[[col]])) |>
    dplyr::count(.data[[col]], name = "n") |>
    dplyr::rename(category = 1) |>
    dplyr::mutate(
      category = as.character(category),
      pct      = round(100 * n / sum(n, na.rm = TRUE), 1)
    ) |>
    dplyr::arrange(n) |>
    dplyr::mutate(category = factor(category, levels = category)) |>
    as.data.frame()

  p <- ggplot2::ggplot(bar_data, ggplot2::aes(x = category, y = n, fill = category)) +
    ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(pct, "%")),
      hjust    = -0.15,
      size     = base_size * 0.27,
      colour   = "grey30",
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
      axis.line.y        = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )

  if (interactive) {
    rlang::check_installed("plotly", reason = "for interactive bar charts")
    return(plotly::ggplotly(p, tooltip = c("x", "y", "text")))
  }
  p
}


# -- Population pyramid --------------------------------------------------------
#' @noRd
.vd_pyramid <- function(df, lang, interactive, palette, caption, base_size, ...) {

  age_col <- .find_col(df, c("ibge_age_group", "faixa_etaria_ibge",
                              "grupo_edad_ibge", "age_group", "faixa_etaria"))
  sex_col <- .find_col(df, c("sex", "sexo", "SEXO"))

  if (is.null(age_col) || is.null(sex_col)) {
    cli::cli_abort(c(
      "Population pyramid requires {.strong age group} and {.strong sex} columns.",
      "i" = "Run {.fn sus_data_create_variables} to generate {.val age_group}."
    ))
  }

  pal        <- .vd_palette(palette)
  lab_male   <- .vl("male",   lang)
  lab_female <- .vl("female", lang)

  male_vals   <- c("Male", "Masculino", "M", "1", "male", "masculino")
  female_vals <- c("Female", "Feminino", "F", "2", "female", "feminino")

  pyr_raw <- df |>
    dplyr::filter(!is.na(.data[[age_col]]), !is.na(.data[[sex_col]])) |>
    dplyr::count(.data[[age_col]], .data[[sex_col]], name = "n") |>
    dplyr::rename(age_group = 1, sex_raw = 2) |>
    dplyr::mutate(
      n         = as.numeric(n),
      sex_label = dplyr::case_when(
        sex_raw %in% male_vals   ~ lab_male,
        sex_raw %in% female_vals ~ lab_female,
        .default = NA_character_
      )
    ) |>
    dplyr::filter(!is.na(sex_label)) |>
    as.data.frame()

  # Robust numeric ordering of age-group strings (e.g. "5-9" < "10-14" < "50-54")
  unique_ages  <- unique(as.character(pyr_raw$age_group))
  lead_digits  <- suppressWarnings(as.numeric(stringr::str_extract(unique_ages, "^\\d+")))
  ordered_ages <- unique_ages[order(lead_digits)]
  pyr_raw$age_group <- factor(pyr_raw$age_group, levels = ordered_ages)

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
  } else {
    ""
  }

  p <- ggplot2::ggplot(
    pyr_raw,
    ggplot2::aes(
      x    = age_group,
      y    = n_plot,
      fill = sex_label,
      text = paste0(
        age_group, " | ", sex_label,
        "\n n = ", scales::comma(abs(n_plot)),
        " (", pct, "%)"
      )
    )
  ) +
    ggplot2::geom_col(width = 0.82) +
    ggplot2::geom_hline(yintercept = 0, colour = "white", linewidth = 0.4) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      breaks = brks,
      labels = \(x) scales::comma(abs(x)),
      expand = ggplot2::expansion(mult = 0.04)
    ) +
    ggplot2::scale_fill_manual(values = col_map, name = NULL) +
    ggplot2::labs(
      title    = .vl("pyramid_title", lang),
      subtitle = sub_txt,
      x        = .vl("age_group",    lang),
      y        = .vl("count",        lang),
      caption  = caption
    ) +
    .lancet_theme(base_size) +
    ggplot2::theme(
      legend.position = "top",
      legend.key.size = ggplot2::unit(0.5, "lines")
    )

  if (interactive) {
    rlang::check_installed("plotly", reason = "for interactive pyramid")
    return(plotly::ggplotly(p, tooltip = "text"))
  }
  p
}


# -- Cross-demographic tile heatmap -------------------------------------------
#' @noRd
.vd_heatmap <- function(df, row_var, col_var, fill_metric, lang, interactive,
                        palette, caption, base_size, ...) {

  demo_cols <- .vd_detect_cols(df)
  pal       <- .vd_palette(palette)

  # 1. Auto-select variables ------------------------------------------------
  avail <- names(Filter(Negate(is.null), demo_cols))

  if (is.null(row_var)) {
    # Prefer age_group as row (natural ordering reads well top-to-bottom)
    row_var <- if ("ibge_age_group" %in% avail) "ibge_age_group"
               else if ("age_group"  %in% avail) "age_group"
               else avail[1]
  }
  if (is.null(col_var)) {
    # Pick a different dimension from row; prefer race then sex
    col_var <- if (row_var != "race"  && "race"  %in% avail) "race"
               else if (row_var != "sex"   && "sex"   %in% avail) "sex"
               else avail[avail != row_var][1]
  }

  if (is.null(col_var) || row_var == col_var)
    cli::cli_abort(c(
      "{.arg heatmap_row} and {.arg heatmap_col} must be different variables.",
      "i" = "Available: {.val {avail}}"
    ))

  # 2. Resolve actual column names ------------------------------------------
  row_col <- demo_cols[[row_var]] %||% .find_col(df, row_var)
  col_col <- demo_cols[[col_var]] %||% .find_col(df, col_var)

  if (is.null(row_col) || !row_col %in% names(df))
    cli::cli_abort("Row variable {.val {row_var}} not found. Available: {.val {avail}}")
  if (is.null(col_col) || !col_col %in% names(df))
    cli::cli_abort("Column variable {.val {col_var}} not found. Available: {.val {avail}}")

  # 3. Validate fill_metric -------------------------------------------------
  valid_metrics <- c("pct_row", "pct_col", "pct_total", "count")
  if (!fill_metric %in% valid_metrics) {
    cli::cli_warn("Unknown {.arg fill_metric} {.val {fill_metric}}; using {.val pct_row}.")
    fill_metric <- "pct_row"
  }

  # 4. Cross-tabulate -------------------------------------------------------
  heat <- df |>
    dplyr::filter(!is.na(.data[[row_col]]), !is.na(.data[[col_col]])) |>
    dplyr::count(.data[[row_col]], .data[[col_col]], name = "n") |>
    dplyr::rename(row_val = 1, col_val = 2) |>
    dplyr::mutate(row_val = as.character(row_val),
                  col_val = as.character(col_val),
                  n       = as.numeric(n))

  if (nrow(heat) == 0)
    cli::cli_abort("No data after filtering NA in {.val {row_col}} and {.val {col_col}}.")

  # 5. Compute fill value ---------------------------------------------------
  heat <- switch(fill_metric,
    pct_row   = heat |>
      dplyr::mutate(fill_val = round(100 * n / sum(n), 1), .by = row_val),
    pct_col   = heat |>
      dplyr::mutate(fill_val = round(100 * n / sum(n), 1), .by = col_val),
    pct_total = heat |>
      dplyr::mutate(fill_val = round(100 * n / sum(n, na.rm = TRUE), 1)),
    count     = dplyr::mutate(heat, fill_val = n)
  )
  heat <- as.data.frame(heat)

  # 6. Meaningful row ordering (age groups numeric, others alphabetic) ------
  uniq_rows <- unique(heat$row_val)
  lead_nums <- suppressWarnings(as.numeric(stringr::str_extract(uniq_rows, "^\\d+")))
  row_levels <- if (all(is.na(lead_nums))) sort(uniq_rows)
                else                        uniq_rows[order(lead_nums)]
  heat$row_val <- factor(heat$row_val, levels = row_levels)
  heat$col_val <- factor(heat$col_val,
                         levels = sort(unique(as.character(heat$col_val))))

  # 7. Labels ---------------------------------------------------------------
  fill_label <- switch(fill_metric,
    pct_row   = paste0("% ", .vl("within_row",   lang)),
    pct_col   = paste0("% ", .vl("within_col",   lang)),
    pct_total = paste0("% ", .vl("of_total",     lang)),
    count     = .vl("legend_count", lang)
  )
  row_label <- .vl(row_var, lang) %||% row_col
  col_label <- .vl(col_var, lang) %||% col_col

  cell_fmt <- if (fill_metric == "count") {
    function(x) scales::comma(x, accuracy = 1)
  } else {
    function(x) paste0(round(x, 1), "%")
  }

  # 8. Build plot -----------------------------------------------------------
  p <- ggplot2::ggplot(
    heat,
    ggplot2::aes(
      x    = col_val,
      y    = row_val,
      fill = fill_val,
      text = paste0(
        col_label, ": ", col_val, "\n",
        row_label, ": ", row_val, "\n",
        fill_label, ": ", cell_fmt(fill_val), "\n",
        .vl("legend_count", lang), ": ", scales::comma(n, accuracy = 1)
      )
    )
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.55) +
    ggplot2::geom_text(
      ggplot2::aes(label = cell_fmt(fill_val)),
      size      = base_size * 0.27,
      colour    = "grey15",
      fontface  = "plain"
    ) +
    ggplot2::scale_fill_gradientn(
      colours  = c("#f5f5f5", pal[2], pal[1]),
      name     = fill_label,
      na.value = "#ebebeb"
    ) +
    ggplot2::scale_x_discrete(position = "bottom") +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::labs(
      title    = paste0(.vl("heatmap_title", lang), ": ",
                        row_label, " x ", col_label),
      subtitle = paste0(.vl("fill_metric_label", lang), ": ", fill_label),
      x        = col_label,
      y        = row_label,
      caption  = caption
    ) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      axis.text.x       = ggplot2::element_text(angle = 30, hjust = 1,
                                                 size = base_size * 0.85),
      axis.text.y       = ggplot2::element_text(size  = base_size * 0.85),
      axis.ticks        = ggplot2::element_blank(),
      axis.line         = ggplot2::element_blank(),
      panel.grid        = ggplot2::element_blank(),
      legend.position   = "right",
      legend.key.height = ggplot2::unit(2, "lines"),
      plot.title        = ggplot2::element_text(face = "bold",
                                                size = base_size + 1),
      plot.subtitle     = ggplot2::element_text(colour = "grey50",
                                                size   = base_size - 1),
      plot.background   = ggplot2::element_rect(fill = "white", colour = NA)
    )

  if (interactive) {
    rlang::check_installed("plotly", reason = "for interactive heatmaps")
    return(plotly::ggplotly(p, tooltip = "text"))
  }

  p
}


# -- Temporal epidemic curve ---------------------------------------------------
#' @noRd
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

  pal      <- .vd_palette(palette)
  fill_col <- NULL

  if (!is.null(fill_var)) {
    fill_col <- .find_col(df, c(fill_var, paste0(fill_var, "_group"),
                                 gsub("_", ".", fill_var)))
    if (is.null(fill_col) || !fill_col %in% names(df)) {
      cli::cli_warn("Column {.val {fill_var}} not found; ignoring stratification.")
      fill_col <- NULL
    }
  }

  if (!is.null(fill_col)) {
    agg <- df |>
      dplyr::filter(!is.na(.data[[time_col]]), !is.na(.data[[fill_col]])) |>
      dplyr::count(.data[[time_col]], .data[[fill_col]], name = "n") |>
      dplyr::rename(time_x = 1, group = 2) |>
      dplyr::mutate(n = as.numeric(n)) |>
      as.data.frame()
  } else {
    agg <- df |>
      dplyr::filter(!is.na(.data[[time_col]])) |>
      dplyr::count(.data[[time_col]], name = "n") |>
      dplyr::rename(time_x = 1) |>
      dplyr::mutate(n = as.numeric(n), group = .vl("count", lang)) |>
      as.data.frame()
  }

  agg$time_x <- utils::type.convert(agg$time_x, as.is = TRUE)

  if (show_ci) {
    agg$ci_lo <- stats::qpois(0.025, agg$n)
    agg$ci_hi <- stats::qpois(0.975, agg$n)
  }

  x_label <- switch(time_unit,
    month    = .vl("month",    lang),
    epi_week = .vl("epi_week", lang),
    year     = .vl("year",     lang),
    quarter  = .vl("quarter",  lang),
    semester = .vl("semester", lang)
  )

  n_groups  <- length(unique(agg$group))
  fill_vals <- stats::setNames(
    rep(pal, length.out = n_groups),
    unique(agg$group)
  )

  p <- ggplot2::ggplot(
    agg,
    ggplot2::aes(
      x      = time_x,
      y      = n,
      colour = group,
      group  = group,
      text   = paste0(x_label, ": ", time_x,
                      "\n", .vl("count", lang), ": ", scales::comma(n))
    )
  ) +
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
    ggplot2::scale_y_continuous(
      labels = scales::label_comma(),
      expand = ggplot2::expansion(mult = c(0, 0.08))
    ) +
    ggplot2::labs(
      title   = .vl("temporal_title", lang),
      x       = x_label,
      y       = .vl("count",          lang),
      caption = caption
    ) +
    .lancet_theme(base_size) +
    ggplot2::theme(legend.position = "top")

  if (interactive) {
    rlang::check_installed("plotly", reason = "for interactive temporal plots")
    return(plotly::ggplotly(p, tooltip = "text"))
  }
  p
}


# -- Climate-risk distribution -------------------------------------------------
#' @noRd
.vd_climate <- function(df, lang, interactive, palette, caption, base_size, ...) {

  clim_col   <- .find_col(df, c("climate_risk_group", "grupo_risco_climatico"))
  season_col <- .find_col(df, c("astronomical_season", "estacao_astronomica", "season"))
  month_col  <- .find_col(df, c("month", "mes"))
  pal        <- .vd_palette(palette)

  if (is.null(clim_col)) {
    cli::cli_abort(c(
      "Column {.val climate_risk_group} not found.",
      "i" = "Run {.fn sus_data_create_variables} to generate climate risk variables."
    ))
  }

  # Panel A: bar of climate_risk_group
  bar_d <- df |>
    dplyr::filter(!is.na(.data[[clim_col]])) |>
    dplyr::count(.data[[clim_col]], name = "n") |>
    dplyr::rename(group = 1) |>
    dplyr::mutate(
      group = as.character(group),
      pct   = round(100 * n / sum(n), 1)
    ) |>
    dplyr::arrange(n) |>
    dplyr::mutate(group = factor(group, levels = group)) |>
    as.data.frame()

  clim_pal <- grDevices::colorRampPalette(c("#f7f7f7", pal[1]))(nrow(bar_d))

  p_bar <- ggplot2::ggplot(bar_d, ggplot2::aes(x = group, y = n, fill = group)) +
    ggplot2::geom_col(width = 0.7, show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(label = paste0(pct, "%")),
      hjust = -0.15, size = base_size * 0.27, colour = "grey30"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = clim_pal) +
    ggplot2::scale_y_continuous(
      labels = scales::label_comma(),
      expand = ggplot2::expansion(mult = c(0, 0.2))
    ) +
    ggplot2::labs(
      title = .vl("climate_bar_title", lang),
      x     = NULL,
      y     = .vl("count", lang)
    ) +
    .lancet_theme(base_size)

  # Panel B: season x month heatmap (if available)
  if (!is.null(season_col) && !is.null(month_col)) {
    month_labs <- switch(lang,
      pt = c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"),
      es = c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic"),
         c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    )

    heat_d <- df |>
      dplyr::filter(!is.na(.data[[month_col]]), !is.na(.data[[season_col]])) |>
      dplyr::count(.data[[month_col]], .data[[season_col]], name = "n") |>
      dplyr::rename(month = 1, season = 2) |>
      dplyr::mutate(month = factor(month, levels = as.character(1:12))) |>
      as.data.frame()

    p_heat <- ggplot2::ggplot(heat_d, ggplot2::aes(x = month, y = season, fill = n)) +
      ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
      ggplot2::geom_text(
        ggplot2::aes(label = scales::comma(n)),
        size = base_size * 0.23, colour = "grey20"
      ) +
      ggplot2::scale_x_discrete(labels = month_labs) +
      ggplot2::scale_fill_gradientn(
        colours = c("#f7f7f7", pal[2]),
        name    = .vl("count", lang),
        labels  = scales::label_comma()
      ) +
      ggplot2::labs(
        title = .vl("climate_heat_title", lang),
        x     = .vl("month",  lang),
        y     = .vl("season", lang)
      ) +
      .lancet_theme(base_size) +
      ggplot2::theme(
        axis.text.x     = ggplot2::element_text(size = base_size - 1),
        legend.position = "right"
      )

    rlang::check_installed("patchwork", reason = "for combined climate panels")
    out <- (p_bar / p_heat) + patchwork::plot_annotation(caption = caption)
    return(out)
  }

  p_bar + ggplot2::labs(caption = caption)
}


# -- Race/colour equity diverging plot -----------------------------------------
#' @noRd
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

  obs_tab <- df |>
    dplyr::filter(!is.na(.data[[race_col]])) |>
    dplyr::count(.data[[race_col]], name = "n") |>
    dplyr::rename(race = 1) |>
    dplyr::mutate(
      race    = as.character(race),
      obs_pct = 100 * n / sum(n)
    ) |>
    as.data.frame()

  # IBGE Census 2022 national proportions
  ibge_2022 <- c(
    Branca = 43.5, Parda = 45.3, Preta = 10.2, Amarela = 0.5, Indigena = 0.5,
    White  = 43.5, Brown  = 45.3, Black = 10.2, Yellow  = 0.5, Indigenous = 0.5,
    Blanca = 43.5, Negra  = 10.2
  )

  obs_tab$ref_pct <- if (!is.null(benchmark)) {
    benchmark[obs_tab$race]
  } else {
    ibge_2022[obs_tab$race]
  }

  obs_tab <- obs_tab |>
    dplyr::filter(!is.na(ref_pct)) |>
    dplyr::mutate(
      diff = round(obs_pct - ref_pct, 2),
      dir  = ifelse(diff >= 0, .vl("overrep", lang), .vl("underrep", lang))
    ) |>
    dplyr::arrange(diff) |>
    dplyr::mutate(race = factor(race, levels = race))

  col_over  <- pal[1]
  col_under <- pal[3] %||% "#d73027"

  p <- ggplot2::ggplot(
    obs_tab,
    ggplot2::aes(
      x    = race,
      y    = diff,
      fill = dir,
      text = paste0(
        race,
        "\nObserved: ",  round(obs_pct, 1), "%",
        "\nReference: ", round(ref_pct, 1), "%",
        "\nDiff: ",      diff, " pp"
      )
    )
  ) +
    ggplot2::geom_col(width = 0.68) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.6, colour = "grey30") +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c(
        stats::setNames(col_over,  .vl("overrep",  lang)),
        stats::setNames(col_under, .vl("underrep", lang))
      ),
      name = NULL
    ) +
    ggplot2::scale_y_continuous(
      labels = \(x) paste0(ifelse(x > 0, "+", ""), x, " pp"),
      expand = ggplot2::expansion(mult = 0.12)
    ) +
    ggplot2::labs(
      title    = .vl("equity_title",    lang),
      subtitle = .vl("equity_subtitle", lang),
      x        = .vl("race",            lang),
      y        = .vl("equity_axis",     lang),
      caption  = caption
    ) +
    .lancet_theme(base_size) +
    ggplot2::theme(legend.position = "top")

  if (interactive) {
    rlang::check_installed("plotly", reason = "for interactive equity plots")
    return(plotly::ggplotly(p, tooltip = "text"))
  }
  p
}


# -- Dashboard: composite Lancet panel ----------------------------------------
#' @noRd
.vd_dashboard <- function(df, lang, interactive, palette, caption,
                           base_size, show_ci, ...) {

  rlang::check_installed("patchwork", reason = "for composite dashboard")

  # Helper: silently skip panels that fail (not every dataset has all columns)
  .try_panel <- function(expr) {
    tryCatch(expr, error = function(e) {
      cli::cli_warn("Panel skipped: {conditionMessage(e)}")
      NULL
    })
  }

  p_pyr   <- .try_panel(.vd_pyramid(df, lang, FALSE, palette, caption, base_size))
  p_time  <- .try_panel(.vd_temporal(df, "month", NULL, show_ci, lang, FALSE,
                                     palette, caption, base_size))
  p_sex   <- .try_panel(.vd_bar(df, "sex",       lang, FALSE, palette, caption, base_size))
  p_race  <- .try_panel(.vd_bar(df, "race",      lang, FALSE, palette, caption, base_size))
  p_age   <- .try_panel(.vd_bar(df, "age_group", lang, FALSE, palette, caption, base_size))
  p_heat  <- .try_panel(.vd_heatmap(df, NULL, NULL, "pct_row", lang, FALSE,
                                    palette, caption, base_size))

  # Filter to valid ggplot panels
  left_plots  <- Filter(\(x) inherits(x, "ggplot"), list(p_pyr,  p_time))
  right_plots <- Filter(\(x) inherits(x, "ggplot"), list(p_sex,  p_race, p_age, p_heat))
  all_plots   <- c(left_plots, right_plots)

  if (length(all_plots) == 0L) {
    cli::cli_abort("No valid panels could be generated for the dashboard.")
  }

  # Build layout: left column (pyramid + temporal) | right column (2x2 grid)
  # Equal column widths and equal heights within the left column so that
  # panels A and B match the visual footprint of C-F.
  if (length(left_plots) >= 2L && length(right_plots) >= 2L) {
    left_col  <- patchwork::wrap_plots(left_plots,  ncol = 1)
    right_col <- patchwork::wrap_plots(right_plots, ncol = 2)
    combined  <- (left_col | right_col) + patchwork::plot_layout(widths = c(1, 1))
  } else {
    ncols    <- if (length(all_plots) <= 2L) 1L else 2L
    combined <- patchwork::wrap_plots(all_plots, ncol = ncols)
  }

  combined <- combined +
    patchwork::plot_annotation(
      title    = .vl("dashboard_title", lang),
      subtitle = paste0(
        "N = ", format(nrow(df), big.mark = ","), " | ",
        tryCatch(sus_meta(df, "system"), error = function(e) "DATASUS")
      ),
      caption    = caption,
      tag_levels = "A",
      theme = ggplot2::theme(
        plot.title    = ggplot2::element_text(
          face = "bold", size = base_size + 3, hjust = 0
        ),
        plot.subtitle = ggplot2::element_text(
          colour = "grey40", size = base_size - 1, hjust = 0,
          margin = ggplot2::margin(b = 8)
        ),
        plot.caption  = ggplot2::element_text(
          colour = "grey50", size = base_size - 2, hjust = 0
        ),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA)
      )
    )

  print(combined)
  invisible(combined)
}


# =============================================================================
# -- SAVE HELPER ---------------------------------------------------------------
# =============================================================================
#' @noRd
.vd_save <- function(out, type, save_path, width, height, dpi, lang, verbose) {

  ext <- tolower(tools::file_ext(save_path))

  if (inherits(out, "ggplot")) {
    ggplot2::ggsave(save_path, plot = out,
                    width = width, height = height, dpi = dpi, bg = "white")
    if (verbose) cli::cli_alert_success(.vl("saved_to", lang, save_path))

  } else if (inherits(out, "gt_tbl")) {
    rlang::check_installed("gt", reason = "to save gt table")
    gt::gtsave(out, save_path)
    if (verbose) cli::cli_alert_success(.vl("saved_to", lang, save_path))

  } else if (inherits(out, "htmlwidget")) {
    rlang::check_installed("htmlwidgets", reason = "to save HTML widget")
    htmlwidgets::saveWidget(out, save_path, selfcontained = TRUE)
    if (verbose) cli::cli_alert_success(.vl("saved_to", lang, save_path))

  } else {
    cli::cli_warn("Cannot auto-save object of class {.cls {class(out)[1]}}.")
  }
}


# =============================================================================
# -- THEME ---------------------------------------------------------------------
# =============================================================================

#' Lancet-style ggplot2 theme
#' @noRd
.lancet_theme <- function(base_size = 11) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold", size = base_size + 1,
                                                 hjust = 0, margin = ggplot2::margin(b = 4)),
      plot.subtitle      = ggplot2::element_text(colour = "grey40", size = base_size - 1,
                                                 hjust = 0, margin = ggplot2::margin(b = 6)),
      plot.caption       = ggplot2::element_text(colour = "grey50", size = base_size - 2,
                                                 hjust = 0, margin = ggplot2::margin(t = 6)),
      axis.title         = ggplot2::element_text(size = base_size - 0.5, colour = "grey20"),
      axis.text          = ggplot2::element_text(size = base_size - 1,   colour = "grey20"),
      axis.line          = ggplot2::element_line(colour = "grey30", linewidth = 0.4),
      axis.ticks         = ggplot2::element_line(colour = "grey60", linewidth = 0.3),
      panel.grid.major.x = ggplot2::element_line(colour = "grey92", linewidth = 0.35),
      panel.grid.major.y = ggplot2::element_line(colour = "grey92", linewidth = 0.35),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.background   = ggplot2::element_rect(fill = "white", colour = NA),
      legend.background  = ggplot2::element_blank(),
      legend.key         = ggplot2::element_blank(),
      legend.text        = ggplot2::element_text(size = base_size - 1),
      legend.key.size    = ggplot2::unit(0.75, "lines"),
      strip.background   = ggplot2::element_rect(fill = "grey95", colour = NA),
      strip.text         = ggplot2::element_text(face = "bold", size = base_size - 1),
      plot.background    = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin        = ggplot2::margin(8, 10, 6, 8)
    )
}


# =============================================================================
# -- PALETTE (powered by ggsci) ------------------------------------------------
# =============================================================================

#' Colour palettes via ggsci
#'
#' Uses the ggsci package to return journal-quality colour vectors.
#' Falls back to the Lancet palette if the requested name is not recognised.
#'
#' @param name One of `"lancet"`, `"nature"`, `"nejm"`, `"jco"`, `"aaas"`,
#'   `"sus"`, `"viridis"`, `"colorblind"`.
#' @noRd
.vd_palette <- function(name = "lancet") {
  rlang::check_installed("ggsci", reason = "for colour palettes")

  switch(name,
    lancet     = ggsci::pal_lancet("lanonc")(9),
    nature     = ggsci::pal_npg("nrc")(10),
    nejm       = ggsci::pal_nejm("default")(8),
    jco        = ggsci::pal_jco("default")(10),
    aaas       = ggsci::pal_aaas("default")(10),
    sus        = c("#1B6CA8", "#E84855", "#3BB273", "#F4A261",
                   "#7B2D8B", "#2EC4B6", "#FF9F1C", "#CBCBCB"),
    colorblind = c("#0072B2", "#D55E00", "#009E73", "#CC79A7",
                   "#56B4E9", "#E69F00", "#F0E442", "#999999"),
    viridis    = scales::viridis_pal()(8),
    {
      cli::cli_warn(
        "Palette {.val {name}} not recognised. Using {.val lancet}."
      )
      ggsci::pal_lancet("lanonc")(9)
    }
  )
}


# =============================================================================
# -- COLUMN DETECTION ----------------------------------------------------------
# =============================================================================

#' @noRd
.vd_detect_cols <- function(df) {
  list(
    sex          = .find_col(df, c("sex", "sexo", "SEXO", "CS_SEXO")),
    race         = .find_col(df, c("race", "raca", "raza", "RACACOR", "RACA_COR", "CS_RACA")),
    age          = .find_col(df, c("age_years", "idade", "edad", "NU_IDADE_N")),
    age_group    = .find_col(df, c("ibge_age_group", "age_group", "faixa_etaria",
                                   "grupo_etario", "age_group_5yr")),
    ibge_age_group = .find_col(df, c("ibge_age_group")),
    education    = .find_col(df, c("education_level", "education", "escolaridade",
                                   "escolaridad", "ESC", "ESC2010", "CS_ESCOL_N")),
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
# -- MULTILINGUAL LABELS -------------------------------------------------------
# =============================================================================

.vd_labels <- list(
  system_detected     = list(en = "System detected: %s",
                             pt = "Sistema detectado: %s",
                             es = "Sistema detectado: %s"),
  saved_to            = list(en = "Output saved: %s",
                             pt = "Saida salva: %s",
                             es = "Salida guardada: %s"),
  sex                 = list(en = "Sex",            pt = "Sexo",            es = "Sexo"),
  race                = list(en = "Race/Colour",    pt = "Raca/Cor",        es = "Raza/Color"),
  age                 = list(en = "Age (years)",    pt = "Idade (anos)",    es = "Edad (anos)"),
  age_group           = list(en = "Age Group",      pt = "Faixa Etaria",    es = "Grupo de Edad"),
  ibge_age_group      = list(en = "Age Group",      pt = "Faixa Etaria",    es = "Grupo de Edad"),
  education           = list(en = "Education",      pt = "Escolaridade",    es = "Escolaridad"),
  climate_risk        = list(en = "Climate Risk",   pt = "Risco Climatico", es = "Riesgo Climatico"),
  region              = list(en = "State/Region",   pt = "Estado/Regiao",   es = "Estado/Region"),
  municipality        = list(en = "Municipality",   pt = "Municipio",       es = "Municipio"),
  dimension           = list(en = "Dimension",      pt = "Dimensao",        es = "Dimension"),
  demographic_summary = list(en = "Demographic Summary",
                             pt = "Resumo Demografico",
                             es = "Resumen Demografico"),
  category            = list(en = "Category",  pt = "Categoria",  es = "Categoria"),
  count               = list(en = "Count",     pt = "Contagem",   es = "Conteo"),
  percent             = list(en = "Percent",   pt = "Percentual", es = "Porcentaje"),
  male                = list(en = "Male",      pt = "Masculino",  es = "Masculino"),
  female              = list(en = "Female",    pt = "Feminino",   es = "Femenino"),
  sex_ratio           = list(en = "Sex ratio (M:F)", pt = "Razao de sexo (M:F)",
                             es = "Razon de sexo (M:F)"),
  bar_title_sex       = list(en = "Distribution by Sex",
                             pt = "Distribuicao por Sexo",
                             es = "Distribucion por Sexo"),
  bar_title_race      = list(en = "Distribution by Race/Colour",
                             pt = "Distribuicao por Raca/Cor",
                             es = "Distribucion por Raza/Color"),
  bar_title_age_group = list(en = "Distribution by Age Group",
                             pt = "Distribuicao por Faixa Etaria",
                             es = "Distribucion por Grupo de Edad"),
  bar_title_ibge_age_group = list(en = "Distribution by Age Group",
                             pt = "Distribuicao por Faixa Etaria",
                             es = "Distribucion por Grupo de Edad"),
  bar_title_education = list(en = "Distribution by Education Level",
                             pt = "Distribuicao por Escolaridade",
                             es = "Distribucion por Nivel Educativo"),
  bar_title_climate_risk = list(en = "Distribution by Climate Risk Group",
                                pt = "Distribuicao por Grupo de Risco Climatico",
                                es = "Distribucion por Grupo de Riesgo Climatico"),
  bar_title_region    = list(en = "Distribution by State/Region",
                             pt = "Distribuicao por Estado/Regiao",
                             es = "Distribucion por Estado/Region"),
  pyramid_title       = list(en = "Age-Sex Population Pyramid",
                             pt = "Piramide Etaria por Sexo",
                             es = "Piramide de Edad y Sexo"),
  map_title           = list(en = "Geographic Distribution of Cases",
                             pt = "Distribuicao Geografica dos Casos",
                             es = "Distribucion Geografica de los Casos"),
  legend_count        = list(en = "Records",    pt = "Registros",   es = "Registros"),
  temporal_title      = list(en = "Temporal Distribution of Cases",
                             pt = "Distribuicao Temporal dos Casos",
                             es = "Distribucion Temporal de los Casos"),
  month    = list(en = "Month",              pt = "Mes",                   es = "Mes"),
  epi_week = list(en = "Epidemiological Week", pt = "Semana Epidemiologica",
                  es = "Semana Epidemiologica"),
  year     = list(en = "Year",              pt = "Ano",                   es = "Ano"),
  quarter  = list(en = "Quarter",           pt = "Trimestre",             es = "Trimestre"),
  semester = list(en = "Semester",          pt = "Semestre",              es = "Semestre"),
  climate_bar_title  = list(en = "Cases by Climate Risk Group",
                            pt = "Casos por Grupo de Risco Climatico",
                            es = "Casos por Grupo de Riesgo Climatico"),
  climate_heat_title = list(en = "Seasonal Distribution (Month x Season)",
                            pt = "Distribuicao Sazonal (Mes x Estacao)",
                            es = "Distribucion Estacional (Mes x Estacion)"),
  season  = list(en = "Season",   pt = "Estacao",   es = "Estacion"),
  equity_title    = list(en = "Race/Colour Equity vs. National Census Benchmark",
                         pt = "Equidade Racial vs. Censo Nacional (IBGE 2022)",
                         es = "Equidad Racial vs. Censo Nacional (IBGE 2022)"),
  equity_subtitle = list(
    en = "Percentage-point difference: observed vs. IBGE 2022 Census proportions",
    pt = "Diferenca em pontos percentuais: observado vs. Censo IBGE 2022",
    es = "Diferencia en puntos porcentuales: observado vs. Censo IBGE 2022"
  ),
  equity_axis     = list(en = "Difference from national proportion (pp)",
                         pt = "Diferenca da proporcao nacional (pp)",
                         es = "Diferencia de la proporcion nacional (pp)"),
  overrep  = list(en = "Over-represented",  pt = "Sobre-representado",  es = "Sobrerrepresentado"),
  underrep = list(en = "Under-represented", pt = "Sub-representado",    es = "Subrepresentado"),
  dashboard_title = list(en = "Demographic Profile",
                         pt = "Perfil Demografico",
                         es = "Perfil Demografico"),
  source_datasus  = list(en = "Source: DATASUS / Brazilian Ministry of Health",
                         pt = "Fonte: DATASUS / Ministerio da Saude",
                         es = "Fuente: DATASUS / Ministerio de Salud de Brasil"),
  heatmap_title    = list(en = "Demographic Cross-tabulation",
                          pt = "Tabulacao Cruzada Demografica",
                          es = "Tabulacion Cruzada Demografica"),
  fill_metric_label = list(en = "Metric",   pt = "Metrica",  es = "Metrica"),
  within_row       = list(en = "within row",    pt = "dentro da linha",
                           es = "dentro de fila"),
  within_col       = list(en = "within column", pt = "dentro da coluna",
                           es = "dentro de columna"),
  of_total         = list(en = "of total",      pt = "do total",
                           es = "del total")
)

#' @noRd
.vl <- function(key, lang, ...) {
  row <- .vd_labels[[key]]
  if (is.null(row)) return(key)
  txt <- row[[lang]] %||% row[["pt"]] %||% key
  if (...length() > 0L) sprintf(txt, ...) else txt
}


# =============================================================================
# -- (spatial shape cache removed -- type = "map" replaced by type = "heatmap")
# -- MULTILINGUAL LABELS end --------------------------------------------------
# =============================================================================

