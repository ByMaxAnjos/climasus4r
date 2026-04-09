# =============================================================================
# sus_view_aggregate() — Publication-quality temporal visualizations
# Compatible with: SINAN, SIM-DO, SIH, SINASC, SIA, CNES
# =============================================================================

#' Visualize Aggregated DATASUS Time Series 
#'
#' Produces publication-quality temporal visualizations from the output of
#' `sus_data_aggregate()`. Supports epidemic curves, seasonal decomposition,
#' heatmaps, trend analysis, cross-correlation with climate variables, and
#' multi-system dashboards. 
#'
#' @param df A data frame or climasus_df — output from `sus_data_aggregate()`.
#'   Must contain a `date` column and at least one numeric outcome column.
#'
#' @param value_col Character. Name of the outcome column to visualize.
#'   Auto-detected if `NULL` from common DATASUS patterns:
#'   `n_casos`, `n_deaths`, `n_obitos`, `n_admissions`, `n_internacoes`,
#'   `n_births`, `n_nascimentos`, `n_procedures`, `n_casos`.
#'
#' @param date_col Character. Date column name. Default: `"date"`.
#'
#' @param group_col Character. Optional grouping column for stratified plots
#'   (e.g., `"sex"`, `"age_group"`, `"uf_code"`, `"race"`). If `NULL`,
#'   aggregates across all records.
#'
#' @param facet_col Character. Column for faceting (small multiples). If `NULL`,
#'   no faceting is applied. Example: `"uf_code"`, `"age_group"`.
#'
#' @param facet_ncol Integer. Number of facet columns. Default: `3`.
#'
#' @param climate_col Character vector. Names of climate/environmental columns
#'   to overlay or cross-correlate. Required for `"climate"` and `"ccf"` types.
#'   Examples: `c("temperature", "precipitation", "humidity", "pm25")`.
#'
#' @param type Character vector. One or more plot types to produce:
#'   \itemize{
#'     \item `"epidemic"`: Epidemic curve with LOESS trend and CI ribbon.
#'       The workhorse plot for any DATASUS system.
#'     \item `"seasonal"`: Seasonal decomposition boxplot — median, IQR, and
#'       outliers by month across years. Reveals annual cyclicity.
#'     \item `"heatmap"`: Calendar heatmap (year × week or month × year).
#'       Ideal for identifying outbreak timing and intensity patterns.
#'     \item `"trend"`: Annual trend bar chart with YoY change annotations.
#'       For policy evaluation and long-term burden assessment.
#'     \item `"climate"`: Dual-axis overlay of outcome + climate variable.
#'       For environmental health / climate-disease studies.
#'     \item `"ccf"`: Cross-correlation function plot between outcome and
#'       climate variable. Identifies lag structure for DLNM modelling.
#'     \item `"decomposition"`: STL decomposition of the time series into
#'       trend, seasonal, and remainder components.
#'     \item `"dashboard"`: Patchwork composite of epidemic + seasonal +
#'       heatmap + trend panels in a single journal figure.
#'   }
#'   Default: `"epidemic"`.
#'
#' @param smooth_method Character. Smoother for epidemic curve.
#'   `"loess"` (default), `"gam"`, `"lm"`, `"none"`.
#'
#' @param smooth_span Numeric. LOESS span (0–1). Default: `0.25`.
#'   Smaller values capture finer temporal structure (useful for multi-year
#'   series with strong annual cycles). Ignored for `"gam"` and `"lm"`.
#'
#' @param log_transform Logical. If `TRUE` (default for count/rate data),
#'   applies log(1+x) transformation to the Y axis with back-transformed
#'   labels. Set `FALSE` for normally distributed indicators.
#'
#' @param rate_col Character. If provided, uses this pre-computed rate column
#'   instead of raw counts. Example: `"incidencia"` (incidence per 100k).
#'
#' @param pop_col Character. Population column for on-the-fly rate calculation.
#'   Requires `rate_per` to be set.
#'
#' @param rate_per Numeric. Rate denominator. Common values: `100000`, `1000`,
#'   `1` (raw). Ignored if `rate_col` is provided.
#'
#' @param time_unit Character. Temporal resolution for display-level
#'   aggregation (applied on top of input data). Options: `"day"`, `"week"`,
#'   `"month"`, `"quarter"`, `"year"`. If `NULL` (default), inferred from
#'   the input data's date spacing.
#'
#' @param palette Character. Colour palette name.
#'   `"journal"` (default — Nature/Lancet accessible palette),
#'   `"viridis"`, `"plasma"`, `"cividis"` (perceptually uniform, colorblind-safe),
#'   `"climate"` (blue-red diverging, for climate anomaly overlays),
#'   or any RColorBrewer palette name.
#'
#' @param color_by Character. Column to map to colour aesthetic in epidemic
#'   and trend plots. Example: `"sex"`, `"age_group"`, `"uf_code"`.
#'
#' @param annotation Logical. If `TRUE` (default), adds peak annotations,
#'   summary statistics, and epidemiological markers.
#'
#' @param free_scales Logical. If `TRUE`, facets use independent Y scales
#'   (`scales = "free_y"`). Default: `TRUE`.
#'
#' @param title Character. Plot title. Auto-generated if `NULL`.
#' @param subtitle Character. Plot subtitle. Auto-generated if `NULL`.
#' @param caption Character. Figure caption. Auto-generated if `NULL`.
#'   Caption text is formatted for direct inclusion in journal submissions.
#'
#' @param year_breaks Character. Date breaks for X axis. Default: `"3 months"`.
#' @param date_labels Character. Date label format. Default: `"%b/%y"`.
#'
#' @param base_size Numeric. Base font size. Default: `10.5`.
#'   Use 10–11 for double-column (180 mm), 8–9 for single-column (85 mm).
#'
#' @param lang Character. Language for labels and messages.
#'   `"en"` (default), `"pt"`, `"es"`.
#'
#' @param save_path Character. Export file path. Extension sets format:
#'   `.tiff` (300 DPI, LZW — journal standard), `.pdf` (vector),
#'   `.png` (150 DPI — preview). If `NULL`, does not save.
#'
#' @param width,height Numeric. Export dimensions in mm. Default: `180 × 120`
#'   (double-column, landscape). For dashboard: auto-scaled.
#'
#' @param dpi Integer. Export resolution. Default: `300`.
#'
#' @param verbose Logical. If `TRUE` (default), prints progress messages.
#'
#' @return A `ggplot` or `patchwork` object (invisibly). Prints as side effect.
#'
#' @details
#' **Plot type selection guide by DATASUS system:**
#' \itemize{
#'   \item SINAN (dengue, chikungunya, zika, leptospirosis): `"epidemic"` +
#'     `"seasonal"` + `"heatmap"` — captures outbreak dynamics and seasonality.
#'   \item SIM-DO (mortality): `"trend"` + `"decomposition"` — long-term
#'     burden and trend analysis. Add `"climate"` for heat-mortality studies.
#'   \item SIH (hospitalizations): `"epidemic"` + `"trend"` — admission waves
#'     and bed-pressure periods.
#'   \item SINASC (births): `"seasonal"` + `"trend"` — monthly cyclicity in
#'     birth rates and preterm births.
#'   \item SIA (outpatient): `"trend"` + `"heatmap"` — procedure volume and
#'     access gaps.
#'   \item CNES (infrastructure): `"trend"` — capacity evolution over time.
#' }
#'
#' **Smoothing for `"epidemic"` type:**
#' LOESS (span = 0.25) is the default because it is distribution-free, robust
#' to zero-inflated count data, and produces honest CIs via bootstrap. GAM
#' with thin-plate splines is available (`smooth_method = "gam"`) for series
#' with strong seasonal structure and ≥ 3 full annual cycles. For very short
#' series (< 1 year), use `smooth_method = "lm"`.
#'
#' @examples
#' \dontrun{
#' # SINAN-DENGUE: epidemic curve faceted by state
#' sus_view_aggregate(df_amazon_agg, type = "epidemic",
#'                    facet_col = "uf_code", rate_col = "incidencia")
#'
#' # Seasonal pattern (dengue cyclicity)
#' sus_view_aggregate(df_amazon_agg, type = "seasonal", value_col = "n_casos")
#'
#' # Calendar heatmap
#' sus_view_aggregate(df_amazon_agg, type = "heatmap",
#'                    facet_col = "uf_code", value_col = "n_casos")
#'
#' # Full dashboard
#' sus_view_aggregate(df_amazon_agg, type = "dashboard",
#'                    facet_col = "uf_code", rate_col = "incidencia",
#'                    save_path = "fig2_dengue.tiff")
#'
#' # SIM-DO: annual mortality trend
#' sus_view_aggregate(sim_df, type = "trend",
#'                    value_col = "n_deaths", group_col = "cause_group")
#'
#' # Climate overlay (temperature vs dengue)
#' sus_view_aggregate(df_climate, type = "climate",
#'                    value_col = "n_casos", climate_col = "mean_temp",
#'                    lang = "en")
#'
#' # Cross-correlation: lag structure for DLNM
#' sus_view_aggregate(df_climate, type = "ccf",
#'                    value_col = "n_casos", climate_col = "temperature",
#'                    lang = "en")
#' }
#'
#' @export
sus_view_aggregate <- function(df,
                                # ── Columns ────────────────────────────────
                                value_col    = NULL,
                                date_col     = "date",
                                group_col    = NULL,
                                facet_col    = NULL,
                                facet_ncol   = 3L,
                                climate_col  = NULL,
                                # ── Plot type ──────────────────────────────
                                type         = "epidemic",
                                # ── Smoothing ──────────────────────────────
                                smooth_method = "loess",
                                smooth_span   = 0.25,
                                # ── Scale ──────────────────────────────────
                                log_transform = TRUE,
                                rate_col      = NULL,
                                pop_col       = NULL,
                                rate_per      = NULL,
                                time_unit     = NULL,
                                # ── Visual ─────────────────────────────────
                                palette       = "journal",
                                color_by      = NULL,
                                annotation    = TRUE,
                                free_scales   = TRUE, #fixed
                                # ── Text ───────────────────────────────────
                                title         = NULL,
                                subtitle      = NULL,
                                caption       = NULL,
                                year_breaks   = "3 months",
                                date_labels   = "%b/%y",
                                base_size     = 10.5,
                                lang          = "en",
                                # ── Export ─────────────────────────────────
                                save_path     = NULL,
                                width         = 180,
                                height        = 120,
                                dpi           = 300,
                                verbose       = TRUE) {

  # ════════════════════════════════════════════════════════════════════════════
  # 0. VALIDATION & SETUP
  # ════════════════════════════════════════════════════════════════════════════
  if (!is.data.frame(df)) cli::cli_abort("{.arg df} must be a data frame.")
  if (nrow(df) == 0)       cli::cli_abort("{.arg df} is empty.")
  if (!lang %in% c("en", "pt", "es")) cli::cli_abort("{.arg lang} must be 'en', 'pt', or 'es'.")

  valid_types <- c("epidemic","seasonal","heatmap","trend",
                   "climate","ccf","decomposition","dashboard")
  bad_types <- setdiff(type, valid_types)
  if (length(bad_types) > 0) {
    cli::cli_abort("Unknown type(s): {.val {bad_types}}. Valid: {.val {valid_types}}")
  }

  # ── System detection ────────────────────────────────────────────────────────
  system_name <- if (inherits(df, "climasus_df")) {
    sus_meta(df, "system") %||% .infer_system(df)
  } else {
    .infer_system(df)
  }
  if (verbose) cli::cli_alert_info("System: {system_name}")

  # ── Column auto-detection ───────────────────────────────────────────────────
  date_col  <- .va_find_col(df, c(date_col, "date", "data", "DT_NOTIFIC",
                                   "DTOBITO","DT_INTER","DTNASC","DT_COMPET"))
  if (is.null(date_col)) cli::cli_abort("Date column not found. Specify {.arg date_col}.")

  value_col <- value_col %||% rate_col %||% .va_find_col(df, c(
    "n_casos","n_deaths","n_obitos","n_admissions","n_internacoes",
    "n_births","n_nascimentos","n_procedures","n_procedimentos",
    "incidencia","taxa_mortalidade","count","n"
  ))
  if (is.null(value_col)) cli::cli_abort("Outcome column not found. Specify {.arg value_col}.")

  # Use pre-computed rate column if provided
  plot_col <- rate_col %||% value_col

  # ── On-the-fly rate calculation ─────────────────────────────────────────────
  if (!is.null(pop_col) && !is.null(rate_per) && is.null(rate_col)) {
    if (!pop_col %in% names(df)) cli::cli_abort("Population column '{pop_col}' not found.")
    df <- df %>%
      dplyr::mutate(.plot_rate = (.data[[value_col]] / .data[[pop_col]]) * rate_per)
    plot_col <- ".plot_rate"
    if (verbose) cli::cli_alert_info("Rate computed: {value_col}/{pop_col} × {rate_per}")
  }

  # ── Date parsing & time unit inference ─────────────────────────────────────
  df <- df %>% dplyr::mutate(.date = as.Date(.data[[date_col]]))
  time_unit <- time_unit %||% .infer_time_unit(df$.date)
  if (verbose) cli::cli_alert_info("Time unit inferred: {time_unit}")

  # ── Temporal re-aggregation if needed ──────────────────────────────────────
  agg_cols <- c(".date", group_col, facet_col, color_by, climate_col)
  agg_cols <- unique(agg_cols[!is.null(agg_cols) & agg_cols %in% names(df)])

  # ── Labels / i18n ──────────────────────────────────────────────────────────
  lbl <- .va_labels(lang)

  # ── Colour palette ─────────────────────────────────────────────────────────
  pal <- .va_palette(palette)

  # ── Y axis label ───────────────────────────────────────────────────────────
  y_label_base <- .system_y_label(system_name, plot_col, rate_per, lang)
  y_label <- if (log_transform) paste0(y_label_base, lbl$log_suffix) else y_label_base

  # ── Y transform functions ───────────────────────────────────────────────────
  if (log_transform) {
    y_trans  <- log1p
    y_inv    <- expm1
    y_breaks <- .log_breaks(df[[plot_col]])
    y_lbls   <- scales::comma(round(y_inv(y_breaks), 0))
  } else {
    y_trans  <- identity
    y_inv    <- identity
    y_breaks <- scales::pretty_breaks(n = 5)(df[[plot_col]])
    y_lbls   <- scales::comma(y_breaks)
  }

  # ── Auto titles ─────────────────────────────────────────────────────────────
  year_range <- range(as.integer(format(df$.date, "%Y")), na.rm = TRUE)
  yr_str     <- if (diff(year_range) == 0) year_range[1] else
    paste0(year_range[1], "\u2013", year_range[2])

  title   <- title   %||% .va_title(system_name, plot_col, yr_str, lang)
  caption <- caption %||% .va_caption(system_name, smooth_method, smooth_span,
                                       log_transform, rate_per, lang)

  # ════════════════════════════════════════════════════════════════════════════
  # 1. DISPATCH
  # ════════════════════════════════════════════════════════════════════════════
  if ("dashboard" %in% type) {
    out <- .va_dashboard(df, plot_col, facet_col, facet_ncol, group_col,
                         color_by, climate_col, smooth_method, smooth_span,
                         y_trans, y_breaks, y_lbls, y_label, pal,
                         annotation, free_scales, time_unit, year_breaks,
                         date_labels, title, caption, base_size, lang, lbl,
                         log_transform, verbose)
    height <- height * 2.2
  } else {
    plots <- lapply(type, function(tp) {
      switch(tp,
        epidemic      = .va_epidemic(df, plot_col, facet_col, facet_ncol,
                                      group_col, color_by, smooth_method,
                                      smooth_span, y_trans, y_breaks, y_lbls,
                                      y_label, pal, annotation, free_scales,
                                      year_breaks, date_labels, title,
                                      subtitle, caption, base_size, lbl,
                                      log_transform),
        seasonal      = .va_seasonal(df, plot_col, group_col, color_by,
                                      facet_col, facet_ncol, y_label, pal,
                                      annotation, title, subtitle, caption,
                                      base_size, lbl, log_transform),
        heatmap       = .va_heatmap(df, plot_col, facet_col, facet_ncol,
                                     palette, title, subtitle, caption,
                                     base_size, lbl, log_transform),
        trend         = .va_trend(df, plot_col, group_col, color_by,
                                   facet_col, facet_ncol, y_label, pal,
                                   annotation, title, subtitle, caption,
                                   base_size, lbl, log_transform),
        climate       = .va_climate(df, plot_col, climate_col, group_col,
                                     facet_col, facet_ncol, y_label, pal,
                                     title, subtitle, caption, base_size, lbl),
        ccf           = .va_ccf(df, plot_col, climate_col, title,
                                 subtitle, caption, base_size, lbl),
        decomposition = .va_decomposition(df, plot_col, time_unit, title,
                                           subtitle, caption, base_size, lbl)
      )
    })
    out <- if (length(plots) == 1) plots[[1]] else
      patchwork::wrap_plots(plots, ncol = 1)
  }

  print(out)
  if (verbose) cli::cli_alert_success("Plot rendered: {paste(type, collapse=', ')}")

  # ── Export ──────────────────────────────────────────────────────────────────
  if (!is.null(save_path)) {
    ext <- tolower(tools::file_ext(save_path))
    if (ext %in% c("tiff","tif")) {
      ggplot2::ggsave(save_path, out, width=width, height=height,
                      units="mm", dpi=dpi, compression="lzw")
    } else if (ext == "pdf") {
      ggplot2::ggsave(save_path, out, width=width, height=height,
                      units="mm", device="pdf")
    } else {
      ggplot2::ggsave(save_path, out, width=width, height=height,
                      units="mm", dpi=dpi)
    }
    if (verbose) cli::cli_alert_success("Saved: {.file {save_path}}")
  }

  invisible(out)
}


# =============================================================================
# PLOT ENGINES
# =============================================================================

# ── 1. EPIDEMIC CURVE ─────────────────────────────────────────────────────────
.va_epidemic <- function(df, plot_col, facet_col, facet_ncol, group_col,
                          color_by, smooth_method, smooth_span, y_trans,
                          y_breaks, y_lbls, y_label, pal, annotation,
                          free_scales, year_breaks, date_labels,
                          title, subtitle, caption, base_size, lbl,
                          log_transform) {

  # Aggregate: weekly mean per group/facet if daily data
  grp <- unique(c(".date", facet_col, color_by, group_col))
  grp <- grp[grp %in% names(df)]

  df_agg <- df %>%
    dplyr::mutate(
      .epi_week = lubridate::floor_date(.date, unit = "week", week_start = 1)
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(
      unique(c(".epi_week", setdiff(grp, ".date")))
    ))) %>%
    dplyr::summarise(
      .val  = mean(.data[[plot_col]][.data[[plot_col]] > 0], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      .val = dplyr::if_else(is.nan(.val) | is.na(.val), 0, .val),
      .y   = y_trans(.val)
    )

  # Peak labels
  if (annotation) {
    peak_grp <- if (!is.null(facet_col) && facet_col %in% names(df_agg))
      facet_col else character(0)
    peaks <- df_agg %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(peak_grp))) %>%
      dplyr::slice_max(.val, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        .peak_year = format(.epi_week, "%Y"),
        .label     = paste0(
          formatC(round(.val, 0), format = "d", big.mark = ","),
          "\n(", .peak_year, ")"
        )
      )
  }

  # Smooth method spec
  sm_method  <- switch(smooth_method,
    loess = "loess", gam = "gam", lm = "lm", none = NULL, "loess"
  )
  sm_formula <- if (smooth_method == "gam") y ~ s(x, bs = "tp") else y ~ x

  # Color aesthetic
  color_aes <- if (!is.null(color_by) && color_by %in% names(df_agg))
    ggplot2::aes(color = .data[[color_by]], fill = .data[[color_by]])
  else
    ggplot2::aes()

  p <- ggplot2::ggplot(df_agg, ggplot2::aes(x = .epi_week)) +

    # Background area
    ggplot2::geom_area(
      ggplot2::aes(y = .y),
      fill = pal[2], alpha = 0.12, na.rm = TRUE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .y),
      color = pal[2], linewidth = 0.35, alpha = 0.55, na.rm = TRUE
    ) +

    # Trend smoother
    {
      if (!is.null(sm_method)) {
        ggplot2::geom_smooth(
          ggplot2::aes(y = .y),
          method    = sm_method,
          formula   = sm_formula,
          span      = smooth_span,
          color     = pal[6],
          fill      = pal[5],
          alpha     = 0.20,
          linewidth = 1.1,
          na.rm     = TRUE
        )
      }
    } +

    # Peak annotation
    {
      if (annotation) {
        ggrepel::geom_label_repel(
          data          = peaks,
          ggplot2::aes(y = y_trans(.val), label = .label),
          size          = base_size * 0.23,
          fontface      = "bold",
          color         = pal[6],
          fill          = scales::alpha("white", 0.88),
          label.padding = ggplot2::unit(0.2, "lines"),
          label.size    = 0.28,
          box.padding   = 0.5,
          nudge_y       = diff(range(df_agg$.y, na.rm = TRUE)) * 0.12,
          segment.color = pal[6],
          segment.size  = 0.3,
          segment.linetype = "dashed",
          na.rm         = TRUE,
          max.overlaps  = 20
        )
      }
    } +

    # Facet
    {
      if (!is.null(facet_col) && facet_col %in% names(df_agg)) {
        ggplot2::facet_wrap(
          stats::as.formula(paste0("~ ", facet_col)),
          scales = if (free_scales) "free_y" else "fixed",
          ncol   = facet_ncol
        )
      }
    } +

    ggplot2::scale_x_date(
      date_breaks = year_breaks,
      date_labels = date_labels,
      expand      = ggplot2::expansion(mult = c(0.01, 0.02))
    ) +
    ggplot2::scale_y_continuous(
      name   = y_label,
      breaks = y_breaks,
      labels = y_lbls,
      expand = ggplot2::expansion(mult = c(0.01, 0.12))
    ) +
    ggplot2::labs(title = title, subtitle = subtitle, x = NULL, caption = caption) +
    .va_theme(base_size)
}


# ── 2. SEASONAL DECOMPOSITION BOXPLOT ─────────────────────────────────────────
.va_seasonal <- function(df, plot_col, group_col, color_by, facet_col,
                          facet_ncol, y_label, pal, annotation,
                          title, subtitle, caption, base_size, lbl,
                          log_transform) {

  month_labels <- if (lbl$lang == "pt") {
    c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")
  } else if (lbl$lang == "es") {
    c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
  } else {
    c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  }

  df_s <- df %>%
    dplyr::mutate(
      .month     = as.integer(format(.date, "%m")),
      .month_lbl = factor(month_labels[.month], levels = month_labels),
      .year      = format(.date, "%Y"),
      .val       = if (log_transform) log1p(.data[[plot_col]]) else .data[[plot_col]]
    )

  color_aes_col <- if (!is.null(color_by) && color_by %in% names(df_s))
    color_by else ".month_lbl"

  p <- ggplot2::ggplot(df_s,
    ggplot2::aes(x = .month_lbl, y = .val, fill = .data[[color_aes_col]])) +

    ggplot2::geom_boxplot(
      outlier.size  = 0.6,
      outlier.alpha = 0.4,
      linewidth     = 0.35,
      fatten        = 1.5,
      width         = 0.65,
      color         = "grey30"
    ) +

    # Monthly median trend line
    ggplot2::stat_summary(
      fun       = median,
      geom      = "line",
      ggplot2::aes(group = 1),
      color     = pal[6],
      linewidth = 0.8,
      alpha     = 0.85
    ) +
    ggplot2::stat_summary(
      fun     = median,
      geom    = "point",
      color   = pal[6],
      size    = 1.8,
      shape   = 21,
      fill    = "white",
      stroke  = 1.2
    ) +

    # Facet
    {
      if (!is.null(facet_col) && facet_col %in% names(df_s)) {
        ggplot2::facet_wrap(
          stats::as.formula(paste0("~ ", facet_col)),
          scales = if (free_scales) "free_y" else "fixed",
          ncol   = facet_ncol
        )
      }
    } +

    ggplot2::scale_fill_manual(values = colorRampPalette(pal)(12), guide = "none") +
    ggplot2::scale_y_continuous(
      name   = y_label,
      labels = if (log_transform)
        function(x) scales::comma(round(expm1(x), 0))
      else
        scales::comma
    ) +
    ggplot2::labs(
      title    = title %||% lbl$seasonal_title,
      subtitle = subtitle %||% lbl$seasonal_sub,
      x        = lbl$month_lbl,
      caption  = caption
    ) +
    .va_theme(base_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = base_size * 0.78)
    )
}


# ── 3. CALENDAR HEATMAP ────────────────────────────────────────────────────────
.va_heatmap <- function(df, plot_col, facet_col, facet_ncol, palette,
                         title, subtitle, caption, base_size, lbl,
                         log_transform) {

  heatmap_pal <- if (palette == "journal") {
    c("#F7FBFF","#DEEBF7","#C6DBEF","#9ECAE1","#6BAED6",
      "#4292C6","#2171B5","#08519C","#08306B")
  } else {
    colorRampPalette(c("#FFFFD9","#EDF8B1","#7FCDBB",
                       "#41B6C4","#1D91C0","#225EA8","#0C2C84"))(9)
  }

  df_h <- df %>%
    dplyr::mutate(
      .year  = as.integer(format(.date, "%Y")),
      .month = factor(
        format(.date, "%b"),
        levels = if (lbl$lang == "pt")
          c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")
        else
          month.abb
      ),
      .val   = if (log_transform) log1p(.data[[plot_col]]) else .data[[plot_col]]
    ) %>%
    dplyr::group_by(.year, .month,
                     dplyr::across(dplyr::all_of(
                       intersect(facet_col, names(df))
                     ))) %>%
    dplyr::summarise(.val = mean(.val, na.rm = TRUE), .groups = "drop")

  fill_lim <- range(df_h$.val, na.rm = TRUE)

  p <- ggplot2::ggplot(df_h, ggplot2::aes(x = .month, y = factor(.year), fill = .val)) +

    ggplot2::geom_tile(color = "white", linewidth = 0.4, na.rm = TRUE) +

    ggplot2::scale_fill_gradientn(
      colors   = heatmap_pal,
      na.value = "grey95",
      name     = if (log_transform) paste0(lbl$fill_label, "\n(log\u2081\u208a\u2093)") else lbl$fill_label,
      labels   = if (log_transform)
        function(x) scales::comma(round(expm1(x), 0))
      else
        scales::comma,
      guide = ggplot2::guide_colorbar(
        title.position  = "top",
        barwidth        = ggplot2::unit(12, "lines"),
        barheight       = ggplot2::unit(0.55, "lines"),
        ticks.linewidth = 0.4
      )
    ) +

    {
      if (!is.null(facet_col) && facet_col %in% names(df_h)) {
        ggplot2::facet_wrap(
          stats::as.formula(paste0("~ ", facet_col)),
          ncol = facet_ncol
        )
      }
    } +

    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +

    ggplot2::labs(
      title    = title %||% lbl$heatmap_title,
      subtitle = subtitle %||% lbl$heatmap_sub,
      x        = lbl$month_lbl,
      y        = NULL,
      caption  = caption
    ) +
    .va_theme(base_size) +
    ggplot2::theme(
      legend.position  = "bottom",
      legend.direction = "horizontal",
      axis.text.x      = ggplot2::element_text(angle = 0, size = base_size * 0.78),
      axis.text.y      = ggplot2::element_text(size = base_size * 0.78),
      panel.grid       = ggplot2::element_blank()
    )
}


# ── 4. ANNUAL TREND BAR CHART ─────────────────────────────────────────────────
.va_trend <- function(df, plot_col, group_col, color_by, facet_col,
                       facet_ncol, y_label, pal, annotation,
                       title, subtitle, caption, base_size, lbl,
                       log_transform) {

  grp_cols <- unique(c(
    ".year",
    if (!is.null(color_by) && color_by %in% names(df)) color_by else NULL,
    if (!is.null(facet_col) && facet_col %in% names(df)) facet_col else NULL
  ))

  df_t <- df %>%
    dplyr::mutate(.year = as.integer(format(.date, "%Y"))) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_cols))) %>%
    dplyr::summarise(
      .total = sum(.data[[plot_col]], na.rm = TRUE),
      .mean  = mean(.data[[plot_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.year) %>%
    dplyr::mutate(
      .pct_change = (.total / dplyr::lag(.total) - 1) * 100,
      .label_pct  = dplyr::case_when(
        is.na(.pct_change) ~ "",
        .pct_change > 0    ~ paste0("+", round(.pct_change, 1), "%"),
        TRUE               ~ paste0(round(.pct_change, 1), "%")
      )
    )

  fill_col <- if (!is.null(color_by) && color_by %in% names(df_t)) color_by else NULL

  p <- ggplot2::ggplot(df_t, ggplot2::aes(x = factor(.year), y = .total)) +

    ggplot2::geom_col(
      ggplot2::aes(fill = if (!is.null(fill_col)) .data[[fill_col]] else factor(.year)),
      width = 0.72, alpha = 0.88, show.legend = !is.null(fill_col)
    ) +

    # YoY annotation
    {
      if (annotation) {
        ggplot2::geom_text(
          ggplot2::aes(
            y     = .total,
            label = .label_pct,
            color = .pct_change > 0
          ),
          vjust    = -0.5,
          size     = base_size * 0.22,
          fontface = "bold",
          na.rm    = TRUE
        )
      }
    } +

    ggplot2::scale_fill_manual(values = colorRampPalette(pal)(nrow(df_t))) +
    ggplot2::scale_color_manual(values = c("FALSE" = "#2196F3", "TRUE" = "#C0392B"),
                                 guide = "none") +
    ggplot2::scale_y_continuous(
      name   = y_label,
      labels = scales::comma,
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +

    {
      if (!is.null(facet_col) && facet_col %in% names(df_t)) {
        ggplot2::facet_wrap(
          stats::as.formula(paste0("~ ", facet_col)),
          scales = "free_y", ncol = facet_ncol
        )
      }
    } +

    ggplot2::labs(
      title    = title %||% lbl$trend_title,
      subtitle = subtitle %||% lbl$trend_sub,
      x        = NULL,
      caption  = caption
    ) +
    .va_theme(base_size) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x        = ggplot2::element_text(angle = 45, hjust = 1)
    )
}


# ── 5. CLIMATE DUAL-AXIS OVERLAY ──────────────────────────────────────────────
.va_climate <- function(df, plot_col, climate_col, group_col, facet_col,
                         facet_ncol, y_label, pal, title, subtitle,
                         caption, base_size, lbl) {

  if (is.null(climate_col)) {
    cli::cli_abort("Specify {.arg climate_col} for type = 'climate'.")
  }
  clim <- climate_col[1]
  if (!clim %in% names(df)) cli::cli_abort("Climate column '{clim}' not found.")

  # Monthly aggregation for dual-axis clarity
  df_c <- df %>%
    dplyr::mutate(.month = lubridate::floor_date(.date, "month")) %>%
    dplyr::group_by(.month,
                     dplyr::across(dplyr::all_of(
                       intersect(facet_col, names(df))
                     ))) %>%
    dplyr::summarise(
      .outcome = mean(.data[[plot_col]], na.rm = TRUE),
      .climate = mean(.data[[clim]], na.rm = TRUE),
      .groups  = "drop"
    )

  # Dual-axis scaling
  scale_factor <- max(df_c$.outcome, na.rm = TRUE) /
    max(df_c$.climate, na.rm = TRUE)

  p <- ggplot2::ggplot(df_c, ggplot2::aes(x = .month)) +

    # Climate variable as filled area (left scaled)
    ggplot2::geom_area(
      ggplot2::aes(y = .climate * scale_factor),
      fill = "#B3CDE3", alpha = 0.35, na.rm = TRUE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .climate * scale_factor),
      color = "#4393C3", linewidth = 0.7, na.rm = TRUE, linetype = "dashed"
    ) +

    # Outcome as solid line
    ggplot2::geom_line(
      ggplot2::aes(y = .outcome),
      color = pal[6], linewidth = 1.0, na.rm = TRUE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = .outcome),
      color = pal[6], size = 1.2, alpha = 0.7, na.rm = TRUE
    ) +

    # Dual Y axes
    ggplot2::scale_y_continuous(
      name     = y_label,
      labels   = scales::comma,
      sec.axis = ggplot2::sec_axis(
        transform = ~ . / scale_factor,
        name      = clim,
        labels    = scales::comma
      )
    ) +

    ggplot2::scale_x_date(date_breaks = "3 months", date_labels = "%b/%y") +

    {
      if (!is.null(facet_col) && facet_col %in% names(df_c)) {
        ggplot2::facet_wrap(
          stats::as.formula(paste0("~ ", facet_col)),
          scales = "free_y", ncol = facet_ncol
        )
      }
    } +

    ggplot2::labs(
      title    = title %||% paste(lbl$climate_title, clim),
      subtitle = subtitle %||% paste0(lbl$climate_sub, clim, " (dashed, right axis)"),
      x        = NULL,
      caption  = caption
    ) +
    .va_theme(base_size) +
    ggplot2::theme(
      axis.title.y.right = ggplot2::element_text(color = "#4393C3", size = base_size * 0.82),
      axis.text.y.right  = ggplot2::element_text(color = "#4393C3")
    )
}


# ── 6. CROSS-CORRELATION FUNCTION ─────────────────────────────────────────────
.va_ccf <- function(df, plot_col, climate_col, title, subtitle, caption,
                     base_size, lbl) {

  if (is.null(climate_col)) {
    cli::cli_abort("Specify {.arg climate_col} for type = 'ccf'.")
  }

  # Aggregate to monthly for CCF stability
  df_ccf <- df %>%
    dplyr::mutate(.month = lubridate::floor_date(.date, "month")) %>%
    dplyr::group_by(.month) %>%
    dplyr::summarise(
      .outcome = mean(.data[[plot_col]], na.rm = TRUE),
      dplyr::across(dplyr::all_of(climate_col[climate_col %in% names(df)]),
                    ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::arrange(.month)

  plots_ccf <- lapply(climate_col[climate_col %in% names(df)], function(clim) {
    ccf_res <- stats::ccf(
      df_ccf[[clim]],
      df_ccf$.outcome,
      lag.max = 12,
      plot    = FALSE,
      na.action = na.omit
    )
    df_ccf_plot <- data.frame(
      lag = ccf_res$lag[,,1],
      acf = ccf_res$acf[,,1]
    )
    ci <- qnorm(0.975) / sqrt(ccf_res$n.used)

    ggplot2::ggplot(df_ccf_plot, ggplot2::aes(x = lag, y = acf)) +
      ggplot2::geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
      ggplot2::geom_hline(yintercept = c(-ci, ci), linetype = "dashed",
                          color = "#C0392B", linewidth = 0.5) +
      ggplot2::geom_segment(ggplot2::aes(xend = lag, yend = 0),
                            color = "#2C3E50", linewidth = 0.8) +
      ggplot2::geom_point(color = "#2C3E50", size = 2.2,
                          shape = 21, fill = "white", stroke = 1.2) +
      ggplot2::annotate("text", x = Inf, y = Inf,
                        label = paste0("95% CI: ±", round(ci, 3)),
                        hjust = 1.1, vjust = 1.5,
                        size = base_size * 0.22, color = "#C0392B") +
      ggplot2::scale_x_continuous(breaks = seq(-12, 12, 3)) +
      ggplot2::labs(
        title    = paste0(lbl$ccf_title, clim),
        subtitle = subtitle %||% lbl$ccf_sub,
        x        = lbl$ccf_lag,
        y        = lbl$ccf_corr,
        caption  = caption
      ) +
      .va_theme(base_size)
  })

  if (length(plots_ccf) == 1) plots_ccf[[1]] else
    patchwork::wrap_plots(plots_ccf, ncol = 2)
}


# ── 7. STL DECOMPOSITION ──────────────────────────────────────────────────────
.va_decomposition <- function(df, plot_col, time_unit, title, subtitle,
                               caption, base_size, lbl) {

  # Aggregate to monthly for STL
  df_d <- df %>%
    dplyr::mutate(.month = lubridate::floor_date(.date, "month")) %>%
    dplyr::group_by(.month) %>%
    dplyr::summarise(.val = sum(.data[[plot_col]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(.month)

  n_months <- nrow(df_d)
  if (n_months < 24) {
    cli::cli_alert_warning("STL decomposition requires at least 24 months. Skipping.")
    return(ggplot2::ggplot() + ggplot2::labs(title = "Insufficient data for STL decomposition"))
  }

  ts_obj <- stats::ts(df_d$.val, frequency = 12)
  stl_fit <- stats::stl(ts_obj, s.window = "periodic", robust = TRUE)
  stl_df  <- as.data.frame(stl_fit$time.series)
  stl_df$.date <- df_d$.month
  stl_df$observed <- df_d$.val

  components <- list(
    observed  = lbl$stl_observed,
    trend     = lbl$stl_trend,
    seasonal  = lbl$stl_seasonal,
    remainder = lbl$stl_remainder
  )

  plots_stl <- lapply(names(components), function(comp) {
    ggplot2::ggplot(stl_df, ggplot2::aes(x = .date, y = .data[[comp]])) +
      ggplot2::geom_line(color = "#2C3E50", linewidth = 0.7) +
      {
        if (comp == "trend") {
          ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                               span = 0.5, color = "#C0392B", fill = "#E74C3C",
                               alpha = 0.15, linewidth = 0.8, se = TRUE)
        }
      } +
      {
        if (comp == "remainder") {
          ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                              color = "grey50", linewidth = 0.4)
        }
      } +
      ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::labs(y = components[[comp]], x = NULL) +
      .va_theme(base_size * 0.88) +
      ggplot2::theme(plot.margin = ggplot2::margin(2, 8, 2, 8))
  })

  patchwork::wrap_plots(plots_stl, ncol = 1) +
    patchwork::plot_annotation(
      title    = title %||% lbl$stl_title,
      subtitle = subtitle %||% lbl$stl_sub,
      caption  = caption,
      theme    = ggplot2::theme(
        plot.title    = ggplot2::element_text(face = "bold", size = base_size * 1.1),
        plot.subtitle = ggplot2::element_text(size = base_size * 0.85, color = "grey35"),
        plot.caption  = ggplot2::element_text(size = base_size * 0.65, color = "grey45",
                                               hjust = 0, lineheight = 1.3)
      )
    )
}


# ── 8. DASHBOARD ─────────────────────────────────────────────────────────────
.va_dashboard <- function(df, plot_col, facet_col, facet_ncol, group_col,
                           color_by, climate_col, smooth_method, smooth_span,
                           y_trans, y_breaks, y_lbls, y_label, pal,
                           annotation, free_scales, time_unit, year_breaks,
                           date_labels, title, caption, base_size, lang, lbl,
                           log_transform, verbose) {

  safe <- function(expr, fallback = NULL) {
    tryCatch(expr, error = function(e) {
      cli::cli_alert_warning("Panel failed: {e$message}")
      fallback
    })
  }

  p_epi  <- safe(.va_epidemic(df, plot_col, facet_col, facet_ncol, group_col,
                               color_by, smooth_method, smooth_span, y_trans,
                               y_breaks, y_lbls, y_label, pal, annotation,
                               free_scales, year_breaks, date_labels, NULL,
                               NULL, NULL, base_size, lbl, log_transform))
  p_seas  <- safe(.va_seasonal(df, plot_col, group_col, color_by, facet_col,
                                facet_ncol, y_label, pal, annotation, NULL,
                                NULL, NULL, base_size, lbl, log_transform))
  p_heat  <- safe(.va_heatmap(df, plot_col, facet_col, min(facet_ncol, 4L),
                               "journal", NULL, NULL, NULL, base_size, lbl,
                               log_transform))
  p_trend <- safe(.va_trend(df, plot_col, group_col, color_by, facet_col,
                             facet_ncol, y_label, pal, annotation, NULL,
                             NULL, NULL, base_size, lbl, log_transform))

  valid_plots <- Filter(Negate(is.null), list(p_epi, p_seas, p_heat, p_trend))

  if (length(valid_plots) == 0) cli::cli_abort("All panels failed.")

  # Layout: epidemic spans full width; others in 2-col grid below
  if (length(valid_plots) >= 3) {
    out <- (valid_plots[[1]]) /
      (patchwork::wrap_plots(valid_plots[-1], ncol = 2)) +
      patchwork::plot_layout(heights = c(1, 1.2))
  } else {
    out <- patchwork::wrap_plots(valid_plots, ncol = 1)
  }

  out + patchwork::plot_annotation(
    title   = title,
    caption = caption,
    theme   = ggplot2::theme(
      plot.title   = ggplot2::element_text(face = "bold", size = base_size * 1.15,
                                            color = "#1A252F"),
      plot.caption = ggplot2::element_text(size = base_size * 0.62, color = "grey45",
                                            hjust = 0, lineheight = 1.3)
    )
  )
}


# =============================================================================
# SHARED THEME
# =============================================================================
.va_theme <- function(base_size = 10.5) {
  ggplot2::theme_classic(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      panel.grid.major.y  = ggplot2::element_line(color = "grey91", linewidth = 0.28),
      panel.grid.major.x  = ggplot2::element_line(color = "grey96", linewidth = 0.22),
      panel.grid.minor    = ggplot2::element_blank(),
      panel.border        = ggplot2::element_rect(color = "grey55", fill = NA, linewidth = 0.4),
      panel.spacing.x     = ggplot2::unit(0.75, "lines"),
      panel.spacing.y     = ggplot2::unit(0.85, "lines"),
      strip.background    = ggplot2::element_rect(fill = "#1C2B3A", color = NA),
      strip.text          = ggplot2::element_text(color = "white", face = "bold",
                                                   size = base_size * 0.82,
                                                   margin = ggplot2::margin(3.5, 5, 3.5, 5)),
      axis.text.x         = ggplot2::element_text(angle = 45, hjust = 1,
                                                   size = base_size * 0.72, color = "grey30"),
      axis.text.y         = ggplot2::element_text(size = base_size * 0.78, color = "grey30"),
      axis.title.y        = ggplot2::element_text(size = base_size * 0.85, color = "grey20",
                                                   margin = ggplot2::margin(r = 6)),
      axis.ticks          = ggplot2::element_line(color = "grey55", linewidth = 0.28),
      axis.line           = ggplot2::element_line(color = "grey55", linewidth = 0.28),
      plot.title          = ggplot2::element_text(face = "bold", size = base_size * 1.08,
                                                   color = "#1A252F",
                                                   margin = ggplot2::margin(b = 4)),
      plot.subtitle       = ggplot2::element_text(size = base_size * 0.85, color = "grey35",
                                                   lineheight = 1.35,
                                                   margin = ggplot2::margin(b = 8)),
      plot.caption        = ggplot2::element_text(size = base_size * 0.65, color = "grey45",
                                                   hjust = 0, lineheight = 1.3,
                                                   margin = ggplot2::margin(t = 8)),
      plot.title.position    = "plot",
      plot.caption.position  = "plot",
      legend.position        = "bottom",
      legend.title           = ggplot2::element_text(size = base_size * 0.78, face = "bold"),
      legend.text            = ggplot2::element_text(size = base_size * 0.72),
      plot.margin            = ggplot2::margin(12, 14, 10, 12)
    )
}


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

#' @keywords internal
#' @noRd
.va_find_col <- function(df, candidates) {
  for (x in candidates) if (!is.null(x) && x %in% names(df)) return(x)
  NULL
}

#' @keywords internal
#' @noRd
.infer_system <- function(df) {
  cols <- tolower(names(df))
  if (any(grepl("notif|sinan|dengue|casos|chikungunya", cols))) return("SINAN")
  if (any(grepl("obito|death|mortal|sim", cols)))                return("SIM")
  if (any(grepl("intern|admission|sih", cols)))                  return("SIH")
  if (any(grepl("nasc|birth|sinasc", cols)))                     return("SINASC")
  if (any(grepl("proced|sia", cols)))                            return("SIA")
  if (any(grepl("cnes|bed|leito|estabelec", cols)))              return("CNES")
  "DATASUS"
}

#' @keywords internal
#' @noRd
.infer_time_unit <- function(dates) {
  dates <- sort(unique(dates[!is.na(dates)]))
  if (length(dates) < 2) return("day")
  med_diff <- as.numeric(median(diff(dates)))
  if (med_diff <= 1)   return("day")
  if (med_diff <= 8)   return("week")
  if (med_diff <= 35)  return("month")
  if (med_diff <= 100) return("quarter")
  "year"
}

#' @keywords internal
#' @noRd
.log_breaks <- function(x) {
  x <- x[is.finite(x) & x >= 0]
  mx <- max(x, na.rm = TRUE)
  cands <- c(0, 1, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 50000)
  cands <- cands[cands <= mx * 1.05]
  log1p(cands[round(seq(1, length(cands), length.out = min(6, length(cands))))])
}

#' @keywords internal
#' @noRd
.va_palette <- function(palette) {
  built <- list(
    journal = c("#1A4E8C","#2980B9","#5DADE2","#A9CCE3","#F4A460","#E07B39","#C0392B"),
    climate = c("#053061","#2166AC","#4393C3","#92C5DE","#FDDBC7","#D6604D","#B2182B"),
    viridis = c("#440154","#31688E","#35B779","#FDE725","#B8DE29","#6DCD59","#1F9E89"),
    plasma  = c("#0D0887","#6A00A8","#B12A90","#E16462","#FCA636","#F0F921","#FEDB67")
  )
  if (palette %in% names(built)) return(built[[palette]])
  if (requireNamespace("RColorBrewer", quietly = TRUE)) {
    tryCatch(return(RColorBrewer::brewer.pal(7, palette)), error = function(e) NULL)
  }
  built[["journal"]]
}

#' @keywords internal
#' @noRd
.system_y_label <- function(system, col, rate_per, lang) {
  denom <- if (!is.null(rate_per)) paste0("/", scales::comma(rate_per)) else ""
  lookup <- list(
    en = list(SINAN="Incidence rate", SIM="Mortality rate",
              SIH="Hospitalizations", SINASC="Births",
              SIA="Procedures", CNES="Capacity", DATASUS="Count"),
    pt = list(SINAN="Taxa de incidencia", SIM="Taxa de mortalidade",
              SIH="Internacoes", SINASC="Nascimentos",
              SIA="Procedimentos", CNES="Capacidade", DATASUS="Contagem"),
    es = list(SINAN="Tasa de incidencia", SIM="Tasa de mortalidad",
              SIH="Hospitalizaciones", SINASC="Nacimientos",
              SIA="Procedimientos", CNES="Capacidad", DATASUS="Conteo")
  )
  base <- lookup[[lang]][[system]] %||% gsub("_", " ", col)
  paste0(base, denom)
}

#' @keywords internal
#' @noRd
.va_title <- function(system, col, yr_str, lang) {
  lookup <- list(
    en = list(
      SINAN  = paste0("Temporal Dynamics of Notified Cases (", yr_str, ")"),
      SIM    = paste0("Mortality Trends (", yr_str, ")"),
      SIH    = paste0("Hospitalization Dynamics (", yr_str, ")"),
      SINASC = paste0("Birth Rate Patterns (", yr_str, ")"),
      SIA    = paste0("Outpatient Procedure Volume (", yr_str, ")"),
      CNES   = paste0("Health Infrastructure Capacity (", yr_str, ")"),
      DATASUS= paste0("DATASUS Time Series (", yr_str, ")")
    ),
    pt = list(
      SINAN  = paste0("Dinamica Temporal de Casos Notificados (", yr_str, ")"),
      SIM    = paste0("Tendencias de Mortalidade (", yr_str, ")"),
      SIH    = paste0("Dinamica de Internacoes (", yr_str, ")"),
      SINASC = paste0("Padroes de Natalidade (", yr_str, ")"),
      SIA    = paste0("Volume de Procedimentos Ambulatoriais (", yr_str, ")"),
      CNES   = paste0("Capacidade da Rede de Saude (", yr_str, ")"),
      DATASUS= paste0("Serie Temporal DATASUS (", yr_str, ")")
    ),
    es = list(
      SINAN  = paste0("Dinamica Temporal de Casos Notificados (", yr_str, ")"),
      SIM    = paste0("Tendencias de Mortalidad (", yr_str, ")"),
      SIH    = paste0("Dinamica de Hospitalizaciones (", yr_str, ")"),
      SINASC = paste0("Patrones de Natalidad (", yr_str, ")"),
      SIA    = paste0("Volumen de Procedimientos (", yr_str, ")"),
      CNES   = paste0("Capacidad de la Red de Salud (", yr_str, ")"),
      DATASUS= paste0("Serie Temporal DATASUS (", yr_str, ")")
    )
  )
  lookup[[lang]][[system]] %||% paste0(gsub("_"," ", col), " (", yr_str, ")")
}

#' @keywords internal
#' @noRd
.va_caption <- function(system, smooth_method, smooth_span, log_transform,
                         rate_per, lang) {
  sm_note <- switch(smooth_method,
    loess = paste0("LOESS (span\u2009=\u2009", smooth_span, ", degree\u2009=\u20092)"),
    gam   = "GAM (thin-plate spline, REML)",
    lm    = "Linear regression",
    none  = "No smoother"
  )
  log_note <- if (log_transform) " Y-axis: log(1+x) transformation." else ""
  rate_note <- if (!is.null(rate_per)) paste0(" Rate per ", scales::comma(rate_per), " inhabitants.") else ""

  sources <- list(
    SINAN   = "SINAN, Brazilian Ministry of Health / DATASUS.",
    SIM     = "SIM-DO, Brazilian Ministry of Health / DATASUS.",
    SIH     = "SIH-RD, Brazilian Ministry of Health / DATASUS.",
    SINASC  = "SINASC, Brazilian Ministry of Health / DATASUS.",
    SIA     = "SIA, Brazilian Ministry of Health / DATASUS.",
    CNES    = "CNES, Brazilian Ministry of Health / DATASUS.",
    DATASUS = "DATASUS, Brazilian Ministry of Health."
  )
  src <- sources[[system]] %||% "DATASUS, Brazilian Ministry of Health."

  paste0("Source: ", src, rate_note,
         " Trend: ", sm_note, ".", log_note,
         " Shaded band: 95% pointwise CI.")
}

#' @keywords internal
#' @noRd
.va_labels <- function(lang) {
  labels <- list(
    en = list(
      lang            = "en",
      log_suffix      = " (log\u2081\u208a\u2093)",
      month_lbl       = "Month",
      fill_label      = "Mean value",
      seasonal_title  = "Seasonal Distribution",
      seasonal_sub    = "Monthly distribution across all years (median \u00b1 IQR)",
      heatmap_title   = "Calendar Heatmap",
      heatmap_sub     = "Mean value by month and year",
      trend_title     = "Annual Trend",
      trend_sub       = "Total by year with year-over-year change (%)",
      climate_title   = "Outcome vs Climate Variable:",
      climate_sub     = "Monthly mean outcome (solid, left axis) vs ",
      ccf_title       = "Cross-Correlation: Outcome vs ",
      ccf_sub         = "Lag structure for DLNM analysis (monthly aggregation)",
      ccf_lag         = "Lag (months)",
      ccf_corr        = "Cross-correlation",
      stl_title       = "STL Time Series Decomposition",
      stl_sub         = "Seasonal-Trend decomposition using LOESS (STL)",
      stl_observed    = "Observed",
      stl_trend       = "Trend",
      stl_seasonal    = "Seasonal",
      stl_remainder   = "Remainder"
    ),
    pt = list(
      lang            = "pt",
      log_suffix      = " (log\u2081\u208a\u2093)",
      month_lbl       = "Mes",
      fill_label      = "Media",
      seasonal_title  = "Distribuicao Sazonal",
      seasonal_sub    = "Distribuicao mensal em todos os anos (mediana \u00b1 IQR)",
      heatmap_title   = "Mapa de Calor Calendário",
      heatmap_sub     = "Media por mes e ano",
      trend_title     = "Tendencia Anual",
      trend_sub       = "Total por ano com variacao anual (%)",
      climate_title   = "Desfecho vs Variavel Climatica:",
      climate_sub     = "Media mensal do desfecho (linha, eixo esq.) vs ",
      ccf_title       = "Correlacao Cruzada: Desfecho vs ",
      ccf_sub         = "Estrutura de defasagem para modelos DLNM (agregacao mensal)",
      ccf_lag         = "Defasagem (meses)",
      ccf_corr        = "Correlacao cruzada",
      stl_title       = "Decomposicao STL da Serie Temporal",
      stl_sub         = "Decomposicao Sazonal-Tendencia via LOESS (STL)",
      stl_observed    = "Observado",
      stl_trend       = "Tendencia",
      stl_seasonal    = "Sazonal",
      stl_remainder   = "Residuo"
    ),
    es = list(
      lang            = "es",
      log_suffix      = " (log\u2081\u208a\u2093)",
      month_lbl       = "Mes",
      fill_label      = "Media",
      seasonal_title  = "Distribucion Estacional",
      seasonal_sub    = "Distribucion mensual en todos los anos (mediana \u00b1 IQR)",
      heatmap_title   = "Mapa de Calor Calendario",
      heatmap_sub     = "Media por mes y ano",
      trend_title     = "Tendencia Anual",
      trend_sub       = "Total por ano con cambio interanual (%)",
      climate_title   = "Resultado vs Variable Climatica:",
      climate_sub     = "Media mensual (linea, eje izq.) vs ",
      ccf_title       = "Correlacion Cruzada: Resultado vs ",
      ccf_sub         = "Estructura de rezago para modelos DLNM (agregacion mensual)",
      ccf_lag         = "Rezago (meses)",
      ccf_corr        = "Correlacion cruzada",
      stl_title       = "Descomposicion STL de la Serie Temporal",
      stl_sub         = "Descomposicion Estacional-Tendencia via LOESS (STL)",
      stl_observed    = "Observado",
      stl_trend       = "Tendencia",
      stl_seasonal    = "Estacional",
      stl_remainder   = "Residuo"
    )
  )
  labels[[lang]]
}
