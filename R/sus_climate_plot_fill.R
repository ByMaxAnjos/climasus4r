# =============================================================================
# sus_climate_plot_fill.R
# Visualisation helpers for gap-filling results -- climasus4r package
#
# Exported:
#   sus_climate_plot_fill()         -- main dispatcher
#
# Internal helpers (@noRd):
#   .plot_fill_series()             -- production-mode time-series orchestrator
#   .plot_fill_eval()               -- evaluation-mode dashboard orchestrator
#   .label_gap_segments()           -- NEW: consecutive-gap block identifier
#   .build_series_plotly()          -- plotly time-series builder
#   .build_series_ggplot()          -- ggplot2 time-series builder
#   .build_eval_plotly()            -- plotly eval-dashboard builder
#   .build_eval_ggplot()            -- ggplot2 eval-dashboard builder
#   .dt_metrics_table()             -- DT table of per-station eval metrics
#   .dt_production_table()          -- DT table of per-station imputation summary
#   .maybe_downsample()             -- smart downsampling for large series
#   .validate_min_points()          -- guard for sparse-station plots
#   .get_scientific_palette()       -- ggsci colour mapper
#   .i18n() / .i18n_strings         -- multilingual string lookup
#   .log_verbose()                  -- verbose logging wrapper
# =============================================================================


# =============================================================================
#  MAIN EXPORTED FUNCTION
# =============================================================================

#' Visualise time-series gap-filling from sus_climate_fill()
#'
#' @description
#' Unified visualisation for gap-filling outputs of the `climasus4r` pipeline.
#' Automatically detects **production mode** (filled tibble) vs **evaluation
#' mode** (list with `data` and `metrics` components) and renders appropriate
#' interactive (plotly) or static (ggplot2) plots plus DT metric tables.
#'
#' **Production mode** displays:
#' \itemize{
#'   \item Time-series of observed (with gaps) vs imputed series.
#'   \item Imputed segments as distinct markers.  Consecutive imputed blocks are
#'         connected by a dotted line; isolated single points appear as dots only.
#'   \item Interactive range-slider with zoom presets (plotly only).
#' }
#'
#' **Evaluation mode** displays a 2×2 diagnostic dashboard:
#' \itemize{
#'   \item Observed vs Predicted scatter with 1:1 reference line.
#'   \item Residual distribution histogram.
#'   \item Residuals over time (temporal-autocorrelation diagnostic).
#'   \item Residuals vs observed (heteroscedasticity diagnostic).
#' }
#'
#' @param df_filled Output of `sus_climate_fill()`. In production mode this is a
#'   `<climasus_df>` tibble; in evaluation mode it is the named list returned by
#'   `sus_climate_fill(evaluation = TRUE)`.
#' @param df_original Raw data before gap filling, e.g. from `sus_climate_inmet()`.
#'   Required in production mode; ignored in evaluation mode.
#' @param target_var Character vector of variable(s) to visualise.
#' @param interactive Logical. `TRUE` → plotly; `FALSE` → ggplot2 (default: `TRUE`).
#' @param output_type What to return: `"plot"`, `"table"`, `"metrics"`, or `"all"`
#'   (default: `"all"`).
#' @param save_plot Optional file path to save the plot (e.g. `"plot.png"` or
#'   `"plot.html"`).
#' @param lang Language: `"en"`, `"pt"`, or `"es"` (default: `"en"`).
#' @param color_palette ggsci palette name (default: `"npg"`).
#' @param verbose Print diagnostic information (default: `FALSE`).
#'
#' @return Depending on `output_type`, returns the plot, table, metrics, or a
#'   named list containing all four components: `plot`, `table`, `metrics`, `data`.
#'
#' @export
sus_climate_plot_fill <- function(
    df_filled,
    df_original   = NULL,
    target_var,
    interactive   = FALSE,
    output_type   = c("plot", "table", "metrics", "all"),
    save_plot     = NULL,
    lang          = c("en", "pt", "es"),
    color_palette = c(
      "npg", "aaas", "nejm", "lancet", "jama", "bmj", "jco", "frontiers",
      "gsea", "uchicago", "primer", "atlassian", "observable", "d3", "igv",
      "cosmic", "locuszoom", "ucscgb", "startrek", "tron", "futurama",
      "rickandmorty", "simpsons", "flatui", "bs5", "material", "tw3"
    ),
    verbose       = FALSE
) {

  # ---------------------------------------------------------------------------
  # 0. Package availability
  # ---------------------------------------------------------------------------
  if (verbose) {
      title_msg <- switch(lang,
        "en" = "climasus4r - Visualise time-series gap-filling",
        "pt" = "climasus4r - Visualizar o preenchimento de falhas em s\u00e9ries temporais",
        "es" = "climasus4r - Visualizar el completado de brechas en series temporales"
      )
      cli::cli_h1(title_msg)
  }
  
  rlang::check_installed(
    c("ggplot2", "ggsci", "DT", "patchwork", "htmltools"),
    reason = "to run sus_climate_plot_fill"
  )
  if (interactive) {
    rlang::check_installed("plotly", reason = "to enable interactive mode")
  }

  # ---------------------------------------------------------------------------
  # 1. Argument validation
  # ---------------------------------------------------------------------------
  output_type   <- match.arg(output_type)
  lang          <- match.arg(lang)
  color_palette <- match.arg(color_palette)

  if (!is.character(target_var) || length(target_var) == 0L) {
    cli::cli_abort("{.arg target_var} must be a non-empty character vector.")
  }
  if (!is.logical(interactive) || length(interactive) != 1L) {
    cli::cli_abort("{.arg interactive} must be a single logical value.")
  }

  # Internal layout constants (not exposed to the public API)
  date_col      <- "date"
  station_col   <- "station_code"
  station_code  <- NULL   # plot all stations unless set downstream
  start_date    <- NULL
  end_date      <- NULL
  show_table    <- TRUE
  height        <- 800L
  n_threshold   <- 5000L
  min_points    <- 10L

  output_format <- if (interactive) "plotly" else "ggplot"

  # ---------------------------------------------------------------------------
  # 2. Detect mode: evaluation vs production
  # ---------------------------------------------------------------------------
  is_eval <-
    is.list(df_filled)                              &&
    !is.data.frame(df_filled)                       &&
    length(df_filled) > 0L                          &&
    !is.null(df_filled[[target_var[1]]])            &&
    all(c("data", "metrics") %in% names(df_filled[[target_var[1]]]))

  # ---------------------------------------------------------------------------
  # 3. Validate df_filled (production mode only)
  # ---------------------------------------------------------------------------
  if (!is_eval) {
    if (!inherits(df_filled, "climasus_df")) {
      cli::cli_abort(
        c(
          .i18n_strings[[lang]][["err_not_climasus"]],
          "i" = .i18n_strings[[lang]][["err_pipeline_hint"]]
        )
      )
    }

    current_stage <- sus_meta(df_filled, "stage")
    type          <- sus_meta(df_filled, "type")

    if (current_stage != "climate" && !type %in% c("filled", "inmet")) {
      cli::cli_abort(
        .i18n("err_wrong_stage", lang,
              current = current_stage %||% "unknown",
              required = "climate")
      )
    }

    .log_verbose(verbose, .i18n("stage_ok", lang))

    # Validate df_original
    if (is.null(df_original)) {
      cli::cli_abort(
        "{.arg df_original} must be supplied in production mode."
      )
    }
    if (!inherits(df_original, "climasus_df")) {
      cli::cli_abort(
        c(
          .i18n_strings[[lang]][["err_orig_not_climasus"]],
          "i" = .i18n_strings[[lang]][["err_pipeline_hint"]]
        )
      )
    }

    orig_stage <- sus_meta(df_original, "stage")
    orig_type  <- sus_meta(df_original, "type")

    if (orig_stage != "climate" || orig_type != "inmet") {
      cli::cli_abort(
        .i18n("err_wrong_stage", lang,
              current = orig_stage %||% "unknown",
              required = "climate / inmet")
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 4. Multi-variable loop
  # ---------------------------------------------------------------------------
  if (length(target_var) > 1L) {

    results <- lapply(target_var, function(tv) {
      sus_climate_plot_fill(
        df_filled     = df_filled,
        df_original   = df_original,
        target_var    = tv,
        interactive   = interactive,
        output_type   = "all",
        save_plot     = NULL,
        lang          = lang,
        color_palette = color_palette,
        verbose       = verbose
      )
    })
    names(results) <- target_var

    combined_plot <- if (output_format == "plotly") {
      figs <- lapply(results, `[[`, "plot")
      do.call(
        plotly::subplot,
        c(figs, list(nrows = length(figs), shareX = TRUE,
                     titleY = TRUE, margin = 0.04))
      )
    } else {
      figs <- Filter(Negate(is.null), lapply(results, `[[`, "plot"))
      patchwork::wrap_plots(figs, ncol = 1) +
        patchwork::plot_annotation(
          title = if (!is.null(station_code))
            paste(.i18n("station_label", lang), station_code)
        )
    }

    all_metrics <- dplyr::bind_rows(
      lapply(target_var, function(tv) {
        m <- results[[tv]]$metrics
        if (!is.null(m) && nrow(m) > 0L) dplyr::mutate(m, variable = tv)
      })
    )

    dt_tbl <- if (show_table && nrow(all_metrics) > 0L) {
      if (is_eval)
        .dt_metrics_table(all_metrics,
                          target_var = paste(target_var, collapse = ", "),
                          lang = lang)
      else
        .dt_production_table(all_metrics, lang = lang)
    } else {
      NULL
    }

    .save_output(combined_plot, save_plot, interactive, verbose)

    res_list <- list(
      plot    = combined_plot,
      table   = dt_tbl,
      metrics = all_metrics,
      data    = lapply(results, `[[`, "data")
    )

    return(invisible(switch(
      output_type,
      "plot"    = res_list$plot,
      "table"   = res_list$table,
      "metrics" = res_list$metrics,
      res_list
    )))
  }

  # ---------------------------------------------------------------------------
  # 5. Single-variable route
  # ---------------------------------------------------------------------------
  common_args <- list(
    target_var    = target_var,
    station_code  = station_code,
    date_col      = date_col,
    station_col   = station_col,
    start_date    = start_date,
    end_date      = end_date,
    show_table    = show_table,
    output_format = output_format,
    height        = height,
    n_threshold   = n_threshold,
    min_points    = min_points,
    lang          = lang,
    color_palette = color_palette,
    verbose       = verbose
  )

  res_list <- if (is_eval) {
    if (!target_var %in% names(df_filled)) {
      cli::cli_abort(c(
        "Variable {.val {target_var}} not found in evaluation object.",
        "i" = "Available: {.val {paste(names(df_filled), collapse = ', ')}}"
      ))
    }
    do.call(.plot_fill_eval,
            c(list(eval_var = df_filled[[target_var]]), common_args))
  } else {
    do.call(.plot_fill_series,
            c(list(df_filled = df_filled, df_original = df_original),
              common_args))
  }

  .save_output(res_list$plot, save_plot, interactive, verbose)

  return(invisible(switch(
    output_type,
    "plot"    = res_list$plot,
    "table"   = res_list$table,
    "metrics" = res_list$metrics,
    res_list
  )))
}


# =============================================================================
#  INTERNAL HELPER: save plot to disk
# =============================================================================

#' @keywords internal
#' @noRd
.save_output <- function(plot, save_plot, interactive, verbose) {
  if (is.null(save_plot) || is.null(plot)) return(invisible(NULL))

  if (interactive) {
    if (requireNamespace("htmlwidgets", quietly = TRUE)) {
      htmlwidgets::saveWidget(plot, file = save_plot, selfcontained = TRUE)
      if (verbose) cli::cli_alert_success("Plot saved to {.path {save_plot}}")
    } else {
      cli::cli_alert_warning(
        "Package {.pkg htmlwidgets} required to save plotly plots."
      )
    }
  } else {
    ggplot2::ggsave(filename = save_plot, plot = plot,
                    width = 12, height = 5, dpi = 150)
    if (verbose) cli::cli_alert_success("Plot saved to {.path {save_plot}}")
  }
  invisible(NULL)
}


# =============================================================================
#  PRODUCTION-MODE ORCHESTRATOR
# =============================================================================

#' @keywords internal
#' @noRd
.plot_fill_series <- function(
    df_filled, df_original, target_var, station_code,
    date_col, station_col, start_date, end_date,
    show_table, output_format, height, n_threshold,
    min_points, lang, verbose, color_palette
) {

  # -- Column-level validation ------------------------------------------------
  flag_col <- paste0("is_imputed_", target_var)

  for (col in c(target_var, date_col)) {
    if (!col %in% names(df_filled))
      cli::cli_abort(
        "Column {.val {col}} not found in {.arg df_filled}."
      )
    if (!col %in% names(df_original))
      cli::cli_abort(
        "Column {.val {col}} not found in {.arg df_original}."
      )
  }

  if (!inherits(df_filled[[date_col]], c("POSIXct", "POSIXt", "Date")))
    cli::cli_abort(
      "Column {.val {date_col}} in {.arg df_filled} must be a date-time type, not {.cls {class(df_filled[[date_col]])}}."
    )
  if (!inherits(df_original[[date_col]], c("POSIXct", "POSIXt", "Date")))
    cli::cli_abort(
      "Column {.val {date_col}} in {.arg df_original} must be a date-time type, not {.cls {class(df_original[[date_col]])}}."
    )

  if (!flag_col %in% names(df_filled)) {
    cli::cli_alert_warning(.i18n("warn_no_flag", lang, fc = flag_col))
    flag_col <- NULL
  }

  # -- Station filter ---------------------------------------------------------
  if (!is.null(station_code)) {
    if (!station_col %in% names(df_filled))
      cli::cli_abort(
        "Station column {.val {station_col}} not found in {.arg df_filled}."
      )
    df_filled   <- dplyr::filter(df_filled,   .data[[station_col]] == station_code)
    df_original <- dplyr::filter(df_original, .data[[station_col]] == station_code)
    if (nrow(df_filled) == 0L)
      cli::cli_abort("No rows for station_code = {.val {station_code}}.")
  }

  # -- Duplicate-date guard ---------------------------------------------------
  join_by <- if (station_col %in% names(df_filled) &&
                 station_col %in% names(df_original))
    c(date_col, station_col) else date_col

  n_dup_filled <- sum(duplicated(df_filled[, join_by]))
  n_dup_orig   <- sum(duplicated(df_original[, join_by]))
  if (n_dup_filled > 0L)
    cli::cli_warn(
      "{n_dup_filled} duplicate date(s) found in {.arg df_filled} -- keeping first occurrence."
    )
  if (n_dup_orig > 0L)
    cli::cli_warn(
      "{n_dup_orig} duplicate date(s) found in {.arg df_original} -- keeping first occurrence."
    )

  df_filled   <- df_filled[!duplicated(df_filled[, join_by]), ]
  df_original <- df_original[!duplicated(df_original[, join_by]), ]

  # -- Date-window clip -------------------------------------------------------
  if (!is.null(start_date) || !is.null(end_date)) {
    sd <- if (!is.null(start_date))
      as.POSIXct(start_date, tz = "UTC")
    else
      as.POSIXct(-Inf, origin = "1970-01-01", tz = "UTC")
    ed <- if (!is.null(end_date))
      as.POSIXct(end_date, tz = "UTC")
    else
      as.POSIXct(Inf,  origin = "1970-01-01", tz = "UTC")
    df_filled   <- dplyr::filter(df_filled,
                                  .data[[date_col]] >= sd & .data[[date_col]] <= ed)
    df_original <- dplyr::filter(df_original,
                                  .data[[date_col]] >= sd & .data[[date_col]] <= ed)
  }

  # -- Join -------------------------------------------------------------------
  keep_filled <- unique(c(join_by, target_var, flag_col))
  keep_orig   <- c(join_by, target_var)

  df_joined <- dplyr::full_join(
    df_original[, intersect(keep_orig,   names(df_original))],
    df_filled[,  intersect(keep_filled,  names(df_filled))],
    by     = join_by,
    suffix = c("_original", "_filled")
  ) |>
    dplyr::arrange(.data[[date_col]])

  orig_col   <- paste0(target_var, "_original")
  filled_col <- paste0(target_var, "_filled")

  # -- Imputed flag -----------------------------------------------------------
  if (!is.null(flag_col) && flag_col %in% names(df_joined)) {
    df_joined$is_imputed <- as.logical(df_joined[[flag_col]])
    df_joined$is_imputed[is.na(df_joined$is_imputed)] <- FALSE
  } else {
    df_joined$is_imputed <-
      is.na(df_joined[[orig_col]]) & !is.na(df_joined[[filled_col]])
  }

  # -- Minimum-points guard ---------------------------------------------------
  n_valid_orig <- sum(!is.na(df_joined[[orig_col]]))
  if (!.validate_min_points(n_valid_orig, station_code, min_points, lang)) {
    return(invisible(list(plot = NULL, table = NULL, metrics = NULL,
                          data = df_joined)))
  }

  # -- Verbose diagnostics ----------------------------------------------------
  if (verbose) {
    na_before <- sum(is.na(df_joined[[orig_col]]))
    na_after  <- sum(is.na(df_joined[[filled_col]]))
    cli::cli_h1(paste0("Variable: ", target_var))
    cli::cli_text(.i18n("verbose_period", lang,
                        from = format(min(df_joined[[date_col]], na.rm = TRUE)),
                        to   = format(max(df_joined[[date_col]], na.rm = TRUE))))
    cli::cli_text(.i18n("verbose_nas", lang,
                        na_before = na_before, na_after = na_after))
    if (station_col %in% names(df_joined)) {
      cli::cli_text(.i18n("verbose_pts", lang))
      pts <- table(df_joined[[station_col]])
      for (nm in names(pts))
        cli::cli_text("    {nm}: {as.integer(pts[[nm]])}")
    }
  }

  # -- Summary metrics --------------------------------------------------------
  total_obs    <- nrow(df_joined)
  total_filled <- sum(df_joined$is_imputed, na.rm = TRUE)
  pct_filled   <- if (total_obs > 0L)
    round((total_filled / total_obs) * 100, 1) else 0

  cli::cli_alert_info(
    .i18n("info_summary", lang, v = target_var,
          nf = total_filled, pct = pct_filled)
  )

  prod_metrics <- if (station_col %in% names(df_joined)) {
    df_joined |>
      dplyr::group_by(.data[[station_col]]) |>
      dplyr::summarise(
        n_obs       = dplyr::n(),
        n_imputed   = sum(.data$is_imputed, na.rm = TRUE),
        pct_imputed = round(mean(.data$is_imputed, na.rm = TRUE) * 100, 2),
        .groups     = "drop"
      )
  } else {
    dplyr::tibble(n_obs = total_obs, n_imputed = total_filled,
                   pct_imputed = pct_filled)
  }

  # -- Build plot -------------------------------------------------------------
  fig <- if (output_format == "plotly") {
    .build_series_plotly(df_joined, orig_col, filled_col, date_col,
                         station_col, target_var, station_code,
                         total_filled, pct_filled,
                         height, n_threshold, lang, verbose, color_palette)
  } else {
    .build_series_ggplot(df_joined, orig_col, filled_col, date_col,
                         station_col, target_var, station_code,
                         total_filled, pct_filled,
                         lang, color_palette)
  }

  dt_tbl <- if (show_table)
    .dt_production_table(prod_metrics, lang = lang) else NULL

  invisible(list(plot = fig, table = dt_tbl, metrics = prod_metrics,
                 data = df_joined))
}


# =============================================================================
#  KEY HELPER: label consecutive gap segments
# =============================================================================

#' Assign a unique integer segment ID to each run of consecutive imputed rows.
#'
#' Rows that are **not** imputed receive `NA`.  Consecutive imputed rows
#' belonging to the same unbroken block share the same positive integer.
#' The function operates per `station_col` so that station boundaries are
#' never bridged.
#'
#' @param df Data frame sorted by `date_col`, already containing `is_imputed`.
#' @param date_col Name of the date-time column.
#' @param station_col Name of the station-code column, or `NULL`.
#' @return `df` with an additional integer column `gap_segment_id`
#'   (`NA` for observed rows, positive integer for each imputed block).
#'
#' @keywords internal
#' @noRd
.label_gap_segments <- function(df, date_col, station_col = NULL) {

  # Label one station's data frame with compact local segment IDs.
  # is_imputed NAs are coerced to FALSE so rle() never sees NA values.
  .label_one <- function(d) {
    imp       <- as.logical(d$is_imputed)
    imp[is.na(imp)] <- FALSE       # NA -> not imputed; safe for rle()

    if (!any(imp)) {
      d$gap_segment_id <- NA_integer_
      return(d)
    }

    rle_obj <- rle(imp)
    run_ids <- rep(seq_along(rle_obj$lengths), rle_obj$lengths)
    seg_id  <- ifelse(imp, run_ids, NA_integer_)

    # Re-map to compact 1..k, keeping only imputed run IDs
    imputed_run_ids        <- unique(run_ids[imp])
    lookup                 <- seq_along(imputed_run_ids)
    names(lookup)          <- as.character(imputed_run_ids)
    seg_id[!is.na(seg_id)] <- lookup[as.character(seg_id[!is.na(seg_id)])]

    d$gap_segment_id <- as.integer(seg_id)
    d
  }

  # Ensure chronological order before segmenting
  df <- df[order(df[[date_col]]), ]

  if (!is.null(station_col) && station_col %in% names(df)) {
    # Single pass: label each station and accumulate a global offset so
    # segment IDs are globally unique across all stations.
    # Avoids the previous double split+bind_rows and the -Inf warning from
    # max(..., na.rm = TRUE) on all-NA gap_segment_id columns.
    offset   <- 0L
    stations <- split(df, df[[station_col]])
    stations <- lapply(stations, function(d) {
      d         <- .label_one(d)
      local_max <- suppressWarnings(max(d$gap_segment_id, na.rm = TRUE))
      if (is.finite(local_max)) {
        d$gap_segment_id <- ifelse(
          is.na(d$gap_segment_id), NA_integer_,
          d$gap_segment_id + offset
        )
        offset <<- offset + local_max
      }
      d
    })
    dplyr::bind_rows(stations)
  } else {
    .label_one(df)
  }
}


# =============================================================================
#  PLOTLY — time-series builder
# =============================================================================

#' @keywords internal
#' @noRd
.build_series_plotly <- function(
    df_joined, orig_col, filled_col, date_col, station_col,
    target_var, station_code, total_filled, pct_filled,
    height, n_threshold, lang, verbose, color_palette
) {

  pal <- .get_scientific_palette(color_palette)  # cached after first call

  # Ensure POSIXct dates
  if (!inherits(df_joined[[date_col]], "POSIXct"))
    df_joined[[date_col]] <- as.POSIXct(df_joined[[date_col]], tz = "UTC")

  # Determine whether to facet (same logic as the ggplot2 builder)
  has_station <- station_col %in% names(df_joined)
  n_stations  <- if (has_station) length(unique(df_joined[[station_col]])) else 1L
  use_facet   <- has_station && is.null(station_code) && n_stations > 1L

  # Label consecutive gap segments on the full data
  df_segmented <- .label_gap_segments(df_joined, date_col, station_col)

  title_str <- sprintf(
    "<b>%s \u2014 %s</b>%s<br><sup>%d %s (%.1f%%)</sup>",
    .i18n("title_series", lang), target_var,
    if (!is.null(station_code))
      paste0("  |  ", .i18n("station_label", lang), ": ", station_code)
    else "",
    total_filled, .i18n("gaps_filled", lang), pct_filled
  )

  # ---------------------------------------------------------------------------
  # Helper: build one self-contained plotly panel for a single station's data.
  # `show_legend` controls whether this panel contributes legend entries —
  # only the first panel does, to avoid duplicates in subplot mode.
  # `show_rangeslider` is TRUE only for the last panel (bottom row).
  # ---------------------------------------------------------------------------
  .build_one_panel <- function(df_st, show_legend, show_rangeslider,
                                panel_title, tt) {

    ds      <- .maybe_downsample(df_st, n_threshold = n_threshold,
                                  verbose = FALSE, lang = lang)
    line_df <- ds$line_df
    if (!inherits(line_df[[date_col]], "POSIXct"))
      line_df[[date_col]] <- as.POSIXct(line_df[[date_col]], tz = "UTC")

    imputed_df <- df_st[
      !is.na(df_st$gap_segment_id) & !is.na(df_st[[filled_col]]), ]

    p <- plotly::plot_ly()

    # Observed line
    p <- plotly::add_trace(
      p,
      x             = line_df[[date_col]],
      y             = line_df[[orig_col]],
      type          = tt, mode = "lines",
      name          = .i18n("original", lang),
      showlegend    = show_legend,
      legendgroup   = "observed",
      line          = list(color = pal$original_observed, width = 1.8),
      connectgaps   = FALSE,
      hovertemplate = paste0(
        "<b>%{x|%Y-%m-%d %H:%M}</b><br>",
        target_var, ": %{y:.2f}<extra></extra>"
      )
    )

    # Imputed segments — one trace per consecutive block
    if (nrow(imputed_df) > 0L) {
      seg_ids    <- sort(unique(imputed_df$gap_segment_id))
      first_seg  <- TRUE
      for (sid in seg_ids) {
        seg       <- imputed_df[imputed_df$gap_segment_id == sid, ]
        seg       <- seg[order(seg[[date_col]]), ]
        is_single <- nrow(seg) == 1L
        seg_mode  <- if (is_single) "markers" else "markers+lines"

        trace_args <- list(
          p             = p,
          x             = seg[[date_col]],
          y             = seg[[filled_col]],
          type          = "scatter",
          mode          = seg_mode,
          name          = .i18n("filled_series", lang),
          # Show legend entry only on the very first segment of the first panel
          showlegend    = show_legend && first_seg,
          legendgroup   = "imputed",
          marker        = list(
            size   = 5,
            color  = pal$imputed_marker,
            symbol = "circle",
            line   = list(color = "white", width = 1)
          ),
          hovertemplate = paste0(
            "<b>%{x|%Y-%m-%d %H:%M}</b><br>",
            .i18n("imputed_pts", lang), ": %{y:.2f}<extra></extra>"
          )
        )
        if (!is_single)
          trace_args$line <- list(color = pal$filled_imputed, width = 1.5,
                                   dash = "dot")

        p         <- do.call(plotly::add_trace, trace_args)
        first_seg <- FALSE
      }
    }

    min_date <- min(line_df[[date_col]], na.rm = TRUE)
    max_date <- max(line_df[[date_col]], na.rm = TRUE)

    # x-axis: rangeslider only on the bottom panel to save vertical space
    xaxis_cfg <- list(
      title       = if (show_rangeslider) .i18n("date_time", lang) else "",
      type        = "date",
      range       = c(min_date, max_date),
      tickformat  = "%b %Y",
      rangeslider = list(visible = show_rangeslider, thickness = 0.04),
      rangeselector = if (show_rangeslider) list(
        buttons = list(
          list(count = 7,  label = "7d",  step = "day",   stepmode = "backward"),
          list(count = 1,  label = "1m",  step = "month", stepmode = "backward"),
          list(count = 3,  label = "3m",  step = "month", stepmode = "backward"),
          list(count = 6,  label = "6m",  step = "month", stepmode = "backward"),
          list(count = 1,  label = "1y",  step = "year",  stepmode = "backward"),
          list(step = "all", label = "All")
        )
      ) else list()
    )

    plotly::layout(
      p,
      annotations = list(list(
        text      = panel_title,
        x         = 0, xref = "paper",
        y         = 1.02, yref = "paper",
        xanchor   = "left", yanchor   = "bottom",
        showarrow = FALSE,
        font      = list(size = 11, color = "#333333")
      )),
      xaxis     = xaxis_cfg,
      yaxis     = list(title = target_var, tickformat = ".2f", zeroline = FALSE),
      hovermode = "x unified",
      template  = "plotly_white"
    )
  }

  # ---------------------------------------------------------------------------
  # Single-station path: keep the original single-panel layout with full
  # range-selector controls.
  # ---------------------------------------------------------------------------
  if (!use_facet) {
    ds      <- .maybe_downsample(df_joined, n_threshold = n_threshold,
                                  verbose = verbose, lang = lang)
    line_df <- ds$line_df
    if (!inherits(line_df[[date_col]], "POSIXct"))
      line_df[[date_col]] <- as.POSIXct(line_df[[date_col]], tz = "UTC")
    tt <- if (ds$use_gl) "scattergl" else "scatter"

    imputed_df <- df_segmented[
      !is.na(df_segmented$gap_segment_id) &
        !is.na(df_segmented[[filled_col]]), ]

    fig <- plotly::plot_ly(height = height, width = NULL)

    fig <- plotly::add_trace(
      fig,
      x             = line_df[[date_col]],
      y             = line_df[[orig_col]],
      type          = tt, mode = "lines",
      name          = .i18n("original", lang),
      line          = list(color = pal$original_observed, width = 2),
      connectgaps   = FALSE,
      hovertemplate = paste0(
        "<b>%{x|%Y-%m-%d %H:%M}</b><br>",
        target_var, ": %{y:.2f}<extra></extra>"
      )
    )

    if (nrow(imputed_df) > 0L) {
      seg_ids <- sort(unique(imputed_df$gap_segment_id))
      first   <- TRUE
      for (sid in seg_ids) {
        seg       <- imputed_df[imputed_df$gap_segment_id == sid, ]
        seg       <- seg[order(seg[[date_col]]), ]
        is_single <- nrow(seg) == 1L
        seg_mode  <- if (is_single) "markers" else "markers+lines"

        trace_args <- list(
          p             = fig,
          x             = seg[[date_col]],
          y             = seg[[filled_col]],
          type          = "scatter",
          mode          = seg_mode,
          name          = .i18n("filled_series", lang),
          showlegend    = first,
          legendgroup   = "imputed",
          marker        = list(size = 6, color = pal$imputed_marker,
                               symbol = "circle",
                               line = list(color = "white", width = 1)),
          hovertemplate = paste0(
            "<b>%{x|%Y-%m-%d %H:%M}</b><br>",
            .i18n("imputed_pts", lang), ": %{y:.2f}<extra></extra>"
          )
        )
        if (!is_single)
          trace_args$line <- list(color = pal$filled_imputed, width = 1.5,
                                   dash = "dot")

        fig   <- do.call(plotly::add_trace, trace_args)
        first <- FALSE
      }
    }

    min_date <- min(line_df[[date_col]], na.rm = TRUE)
    max_date <- max(line_df[[date_col]], na.rm = TRUE)

    return(plotly::layout(
      fig,
      title     = list(text = title_str, font = list(size = 14)),
      xaxis     = list(
        title         = .i18n("date_time", lang),
        type          = "date",
        range         = c(min_date, max_date),
        tickformat    = "%b %Y",
        rangeslider   = list(visible = TRUE, thickness = 0.05),
        rangeselector = list(
          buttons = list(
            list(count = 7,  label = "7d",  step = "day",   stepmode = "backward"),
            list(count = 1,  label = "1m",  step = "month", stepmode = "backward"),
            list(count = 3,  label = "3m",  step = "month", stepmode = "backward"),
            list(count = 6,  label = "6m",  step = "month", stepmode = "backward"),
            list(count = 1,  label = "1y",  step = "year",  stepmode = "backward"),
            list(step = "all", label = "All")
          )
        )
      ),
      yaxis     = list(title = target_var, tickformat = ".2f", zeroline = FALSE),
      legend    = list(orientation = "h", x = 0, y = -0.22,
                       bgcolor = "rgba(255,255,255,0.8)",
                       bordercolor = "#DDDDDD", borderwidth = 1),
      hovermode = "x unified",
      template  = "plotly_white",
      margin    = list(t = 80, b = 50, l = 60, r = 30)
    ))
  }

  # ---------------------------------------------------------------------------
  # Multi-station faceted path: one subplot per station, 2 columns.
  # The rangeslider is placed only on the last row to avoid visual clutter.
  # shareX = TRUE synchronises pan/zoom across all panels.
  # ---------------------------------------------------------------------------
  stations    <- sort(unique(df_joined[[station_col]]))
  ncol_facet  <- min(2L, length(stations))
  nrow_facet  <- ceiling(length(stations) / ncol_facet)
  # Height: allocate per-panel height + extra for title and legend
  panel_h     <- max(200L, height %/% max(nrow_facet, 1L))
  total_h     <- panel_h * nrow_facet + 120L

  # Determine which stations sit on the last row (get the rangeslider)
  last_row_stations <- stations[
    seq(from = (nrow_facet - 1L) * ncol_facet + 1L,
        to   = length(stations))
  ]

  # Use scattergl if ANY station exceeds the threshold
  use_gl <- any(
    vapply(stations, function(st)
      sum(df_joined[[station_col]] == st) > n_threshold, logical(1))
  )
  tt <- if (use_gl) "scattergl" else "scatter"

  panel_list <- lapply(seq_along(stations), function(i) {
    st     <- stations[[i]]
    df_st  <- df_segmented[df_segmented[[station_col]] == st, ]

    .build_one_panel(
      df_st          = df_st,
      show_legend    = (i == 1L),
      show_rangeslider = st %in% last_row_stations,
      panel_title    = paste0("<b>", .i18n("station_label", lang),
                               ": ", st, "</b>"),
      tt             = tt
    )
  })

  fig_combined <- do.call(
    plotly::subplot,
    c(
      panel_list,
      list(
        nrows      = nrow_facet,
        shareX     = FALSE,   # each panel has its own date range slider
        shareY     = TRUE,    # same y-axis scale — honest visual comparison
        titleX     = TRUE,
        titleY     = TRUE,
        margin     = 0.04
      )
    )
  )

  plotly::layout(
    fig_combined,
    height    = total_h,
    title     = list(text = title_str, font = list(size = 13)),
    legend    = list(orientation = "h", x = 0, y = -0.06,
                     bgcolor = "rgba(255,255,255,0.8)",
                     bordercolor = "#DDDDDD", borderwidth = 1),
    hovermode = "x",
    template  = "plotly_white",
    margin    = list(t = 80, b = 60, l = 60, r = 30)
  )
}


# =============================================================================
#  GGPLOT2 — time-series builder
# =============================================================================

#' @keywords internal
#' @noRd
.build_series_ggplot <- function(
    df_joined, orig_col, filled_col, date_col, station_col,
    target_var, station_code, total_filled, pct_filled,
    lang, color_palette
) {

  pal <- .get_scientific_palette(color_palette)  # cached after first call

  # Ensure is_imputed exists
  if (!"is_imputed" %in% names(df_joined)) {
    df_joined$is_imputed <-
      is.na(df_joined[[orig_col]]) & !is.na(df_joined[[filled_col]])
  }

  # Label consecutive gap segments (correct line-grouping fix)
  df_seg <- .label_gap_segments(df_joined, date_col, station_col)

  # Determine whether station column is present and has multiple levels.
  # Faceting is applied when:
  #   (a) station_col exists in the data, AND
  #   (b) no specific station_code was requested (i.e., we are showing all), AND
  #   (c) there is more than one station in the data.
  has_station  <- station_col %in% names(df_seg)
  n_stations   <- if (has_station) length(unique(df_seg[[station_col]])) else 1L
  use_facet    <- has_station && is.null(station_code) && n_stations > 1L

  # Canonical column selection — include station_col when present
  keep_cols <- c(date_col, orig_col, filled_col, "is_imputed", "gap_segment_id")
  if (has_station) keep_cols <- c(keep_cols, station_col)

  df_plot <- df_seg |>
    dplyr::select(dplyr::all_of(keep_cols)) |>
    dplyr::mutate(
      original_val = .data[[orig_col]],
      imputed_val  = dplyr::if_else(
        .data$is_imputed, .data[[filled_col]], NA_real_
      )
    )

  imp_df <- df_plot[!is.na(df_plot$gap_segment_id), ]

  # Title: when faceting, station info is carried by the strip labels so we
  # omit the "| Station: X" fragment from the main title.
  title_str <- paste0(
    .i18n("title_series", lang), " \u2014 ", target_var,
    if (!is.null(station_code))
      paste0("  |  ", .i18n("station_label", lang), ": ", station_code),
    sprintf("\n%d %s (%.1f%%)", total_filled, .i18n("gaps_filled", lang), pct_filled)
  )

  colour_map <- c(
    "observed" = pal$original_observed,
    "imputed"  = pal$filled_imputed
  )
  label_map <- c(
    "observed" = .i18n("original", lang),
    "imputed"  = .i18n("filled_series", lang)
  )

  # Base theme shared by single-station and faceted layouts
  base_theme <- ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      legend.position      = "bottom",
      legend.key.width     = ggplot2::unit(1.8, "cm"),
      legend.text          = ggplot2::element_text(size = 9),
      plot.title           = ggplot2::element_text(size = 11, face = "bold",
                                                    margin = ggplot2::margin(b = 6)),
      plot.title.position  = "plot",
      axis.text.x          = ggplot2::element_text(angle = 30, hjust = 1, size = 8),
      panel.grid.minor     = ggplot2::element_blank(),
      panel.grid.major.x   = ggplot2::element_line(colour = pal$grid_line,
                                                     linewidth = 0.3),
      panel.grid.major.y   = ggplot2::element_line(colour = pal$grid_line,
                                                     linewidth = 0.3),
      panel.border         = ggplot2::element_rect(colour = "#CCCCCC"),
      strip.background     = ggplot2::element_rect(fill = "#F5F5F5"),
      strip.text           = ggplot2::element_text(size = 9, face = "bold")
    )

  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[[date_col]])) +

    ggplot2::geom_line(
      ggplot2::aes(y = .data$original_val, colour = "observed"),
      linewidth = 0.7, na.rm = TRUE
    ) +

    # Each consecutive imputed block is a separate polyline — no cross-gap lines.
    ggplot2::geom_line(
      data = imp_df,
      ggplot2::aes(y     = .data$imputed_val,
                   colour = "imputed",
                   group  = .data$gap_segment_id),
      linewidth = 0.7, linetype = "dotted", na.rm = TRUE
    ) +

    ggplot2::geom_point(
      data = imp_df,
      ggplot2::aes(y = .data$imputed_val, colour = "imputed"),
      size = 1.6, shape = 16, na.rm = TRUE
    ) +

    ggplot2::scale_colour_manual(values = colour_map, labels = label_map) +
    ggplot2::scale_x_datetime(
      date_labels = "%b %Y",
      date_breaks = "2 months",
      expand      = c(0.01, 0)
    ) +
    ggplot2::labs(
      title  = title_str,
      x      = .i18n("date_time", lang),
      y      = target_var,
      colour = NULL
    ) +
    base_theme

  # ---------------------------------------------------------------------------
  # Faceting: one panel per station when multiple stations are present.
  # ncol is capped at 2 for readability; scales = "fixed" keeps the y-axis
  # comparable across panels; axis.text.x is set inside each strip.
  # ---------------------------------------------------------------------------
  if (use_facet) {
    ncol_facet <- min(2L, n_stations)
    p <- p +
      ggplot2::facet_wrap(
        ggplot2::vars(.data[[station_col]]),
        ncol   = ncol_facet,
        scales = "fixed",
        labeller = ggplot2::labeller(
          .default = ggplot2::label_both
        )
      ) +
      ggplot2::theme(
        # Slightly smaller x-axis text in faceted layout to avoid crowding
        axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, size = 7),
        # More breathing room between panels
        panel.spacing = ggplot2::unit(0.8, "lines")
      )
  }

  p
}


# =============================================================================
#  EVALUATION-MODE ORCHESTRATOR
# =============================================================================

#' @keywords internal
#' @noRd
.plot_fill_eval <- function(
    eval_var, target_var, station_code, station_col, date_col,
    start_date, end_date, show_table, output_format,
    height, n_threshold, min_points, lang, verbose, color_palette
) {

  df      <- eval_var$data
  metrics <- eval_var$metrics

  # -- Structural validation --------------------------------------------------
  if (!is.data.frame(df))
    cli::cli_abort("Evaluation data component must be a data frame.")

  missing_cols <- setdiff(c("true_value", "pred_value", date_col), names(df))
  if (length(missing_cols) > 0L)
    cli::cli_abort(
      "Evaluation data is missing column(s): {.val {missing_cols}}"
    )

  if (!is.numeric(df$true_value))
    cli::cli_abort("{.val true_value} must be numeric, not {.cls {class(df$true_value)}}.")
  if (!is.numeric(df$pred_value))
    cli::cli_abort("{.val pred_value} must be numeric, not {.cls {class(df$pred_value)}}.")

  # -- Station filter ---------------------------------------------------------
  if (!is.null(station_code)) {
    if (!station_col %in% names(df))
      cli::cli_abort(
        "Station column {.val {station_col}} not found in evaluation data."
      )
    df <- dplyr::filter(df, .data[[station_col]] == station_code)
    if (!is.null(metrics) && "station_id" %in% names(metrics))
      metrics <- dplyr::filter(metrics, .data$station_id == station_code)
    if (nrow(df) == 0L)
      cli::cli_abort("No evaluation rows for station {.val {station_code}}.")
  }

  # -- Validate metric consistency with stations present ----------------------
  if (!is.null(metrics) && station_col %in% names(df)) {
    eval_stations   <- unique(df[[station_col]])
    metric_stations <- if ("station_id" %in% names(metrics))
      unique(metrics$station_id) else character(0)
    missing_metrics <- setdiff(eval_stations, metric_stations)
    if (length(missing_metrics) > 0L)
      cli::cli_alert_warning(
        "Metrics missing for station(s): {.val {missing_metrics}}. Table may be incomplete."
      )
  }

  # -- Date clip --------------------------------------------------------------
  if (date_col %in% names(df) &&
      (!is.null(start_date) || !is.null(end_date))) {
    sd <- if (!is.null(start_date))
      as.POSIXct(start_date, tz = "UTC")
    else
      as.POSIXct(-Inf, origin = "1970-01-01", tz = "UTC")
    ed <- if (!is.null(end_date))
      as.POSIXct(end_date, tz = "UTC")
    else
      as.POSIXct(Inf,  origin = "1970-01-01", tz = "UTC")
    df <- dplyr::filter(df, .data[[date_col]] >= sd & .data[[date_col]] <= ed)
  }

  # -- Valid pairs only -------------------------------------------------------
  df_valid <- df[!is.na(df$true_value) & !is.na(df$pred_value), ]
  if (nrow(df_valid) == 0L)
    cli::cli_abort("No valid true/pred pairs found after filtering.")

  if (!.validate_min_points(nrow(df_valid), station_code, min_points, lang))
    return(invisible(list(plot = NULL, table = NULL, metrics = metrics,
                          data = df_valid)))

  # -- Verbose diagnostics ----------------------------------------------------
  if (verbose) {
    cli::cli_h1(paste0("Evaluation: ", target_var))
    if (date_col %in% names(df_valid))
      cli::cli_text(.i18n("verbose_period", lang,
                          from = format(min(df_valid[[date_col]], na.rm = TRUE)),
                          to   = format(max(df_valid[[date_col]], na.rm = TRUE))))
    if (station_col %in% names(df_valid)) {
      cli::cli_text(.i18n("verbose_pts", lang))
      pts <- table(df_valid[[station_col]])
      for (nm in names(pts))
        cli::cli_text("    {nm}: {as.integer(pts[[nm]])}")
    }
  }

  # -- Pooled metrics ---------------------------------------------------------
  y_true    <- df_valid$true_value
  y_pred    <- df_valid$pred_value
  residuals <- y_true - y_pred
  ss_res    <- sum(residuals^2)
  ss_tot    <- sum((y_true - mean(y_true))^2)
  r2_pool   <- if (ss_tot > 0) 1 - ss_res / ss_tot else NA_real_
  rmse_p    <- sqrt(mean(residuals^2))
  mae_p     <- mean(abs(residuals))
  n_pool    <- length(y_true)

  cli::cli_alert_info(
    .i18n("info_eval", lang, v = target_var, n = n_pool,
          rmse = round(rmse_p, 3), mae = round(mae_p, 3),
          r2   = round(r2_pool, 3))
  )

  # -- Build plot -------------------------------------------------------------
  fig <- if (output_format == "plotly") {
    .build_eval_plotly(df_valid, y_true, y_pred, residuals,
                       date_col, target_var, station_code,
                       r2_pool, rmse_p, mae_p, n_pool,
                       height, n_threshold, lang, color_palette, verbose)
  } else {
    .build_eval_ggplot(df_valid, y_true, y_pred, residuals,
                       date_col, target_var, station_code,
                       r2_pool, rmse_p, mae_p, n_pool,
                       lang, color_palette)
  }

  dt_tbl <- if (show_table && !is.null(metrics) && nrow(metrics) > 0L)
    .dt_metrics_table(metrics, target_var = target_var, lang = lang)
  else NULL

  invisible(list(plot = fig, table = dt_tbl, metrics = metrics,
                 data = df_valid))
}


# =============================================================================
#  PLOTLY — evaluation dashboard builder
# =============================================================================

#' @keywords internal
#' @noRd
.build_eval_plotly <- function(
    df_valid, y_true, y_pred, residuals,
    date_col, target_var, station_code,
    r2_pool, rmse_p, mae_p, n_pool,
    height, n_threshold, lang, color_palette, verbose
) {

  pal     <- .get_scientific_palette(color_palette)
  has_date <- date_col %in% names(df_valid)

  if (has_date && !inherits(df_valid[[date_col]], "POSIXct"))
    df_valid[[date_col]] <- as.POSIXct(df_valid[[date_col]], tz = "UTC")

  lim_min <- min(c(y_true, y_pred), na.rm = TRUE)
  lim_max <- max(c(y_true, y_pred), na.rm = TRUE)
  ds      <- .maybe_downsample(df_valid, n_threshold = n_threshold,
                                verbose = verbose, lang = lang)
  tt      <- if (ds$use_gl) "scattergl" else "scatter"

  # Panel 1: Observed vs Predicted
  fig_scat <- plotly::plot_ly() |>
    plotly::add_trace(
      x = y_true, y = y_pred,
      type = tt, mode = "markers",
      name = .i18n("station_obs", lang),
      marker = list(color = pal$scatter, opacity = 0.4, size = 5),
      hovertemplate = paste0(
        .i18n("observed", lang), ": %{x:.3f}<br>",
        .i18n("predicted", lang), ": %{y:.3f}<extra></extra>"
      )
    ) |>
    plotly::add_segments(
      x = lim_min, y = lim_min, xend = lim_max, yend = lim_max,
      line = list(color = pal$reference_line, dash = "dash", width = 1.5),
      name = .i18n("one_to_one", lang), showlegend = TRUE
    ) |>
    plotly::layout(
      title  = list(
        text = sprintf(
          "<b>%s</b><br><sup>R\u00b2 = %.3f | RMSE = %.3f | MAE = %.3f | n = %d</sup>",
          .i18n("obs_vs_pred", lang), r2_pool, rmse_p, mae_p, n_pool
        ),
        font = list(size = 11)
      ),
      xaxis  = list(title = .i18n("observed", lang),  tickformat = ".2f"),
      yaxis  = list(title = .i18n("predicted", lang), tickformat = ".2f"),
      height = height %/% 2L,
      template = "plotly_white"
    )

  # Panel 2: Residual histogram
  fig_hist <- plotly::plot_ly(
    x = residuals, type = "histogram", nbinsx = 40, opacity = 0.75,
    marker = list(color = pal$residual_hist,
                  line  = list(color = "white", width = 0.5)),
    name = .i18n("residuals", lang)
  ) |>
    plotly::layout(
      title    = list(text = paste0("<b>", .i18n("res_dist", lang), "</b>"),
                      font = list(size = 11)),
      xaxis    = list(title = .i18n("residual_axis", lang), tickformat = ".2f"),
      yaxis    = list(title = .i18n("count", lang)),
      bargap   = 0.05, height = height %/% 2L, template = "plotly_white"
    )

  # Panel 3: Residuals vs Observed
  fig_res_vs_obs <- plotly::plot_ly(
    x = y_true, y = residuals,
    type = tt, mode = "markers",
    marker = list(color = pal$scatter, opacity = 0.4, size = 4),
    name = .i18n("res_vs_obs", lang),
    hovertemplate = paste0(
      .i18n("observed", lang), ": %{x:.3f}<br>",
      .i18n("residuals", lang), ": %{y:.3f}<extra></extra>"
    )
  ) |>
    plotly::add_segments(
      x = min(y_true, na.rm = TRUE), y = 0,
      xend = max(y_true, na.rm = TRUE), yend = 0,
      line = list(color = pal$zero_line, dash = "dash", width = 1),
      showlegend = FALSE
    ) |>
    plotly::layout(
      title    = list(text = paste0("<b>", .i18n("res_vs_obs", lang), "</b>"),
                      font = list(size = 11)),
      xaxis    = list(title = .i18n("observed", lang),  tickformat = ".2f"),
      yaxis    = list(title = .i18n("residuals", lang), tickformat = ".2f"),
      height   = height %/% 2L, template = "plotly_white"
    )

  # Panel 4: Residuals over time (optional — only when date column is present)
  fig_combined <- if (has_date) {
    fig_ts_res <- plotly::plot_ly(
      x = df_valid[[date_col]], y = residuals,
      type = tt, mode = "markers",
      marker = list(color = pal$residual_point, opacity = 0.5, size = 3),
      name   = .i18n("res_over_t", lang),
      hovertemplate = paste0(
        "<b>%{x|%Y-%m-%d %H:%M}</b><br>",
        .i18n("residuals", lang), ": %{y:.3f}<extra></extra>"
      )
    ) |>
      plotly::add_segments(
        x    = min(df_valid[[date_col]], na.rm = TRUE), y = 0,
        xend = max(df_valid[[date_col]], na.rm = TRUE), yend = 0,
        line = list(color = pal$zero_line, dash = "dash", width = 1),
        showlegend = FALSE
      ) |>
      plotly::layout(
        title    = list(text = paste0("<b>", .i18n("res_over_time", lang), "</b>"),
                        font = list(size = 11)),
        xaxis    = list(title = .i18n("date_time", lang),
                        type = "date", tickformat = "%Y-%m-%d", nticks = 8),
        yaxis    = list(title = .i18n("residuals", lang), tickformat = ".2f"),
        height   = height %/% 2L, template = "plotly_white"
      )

    plotly::subplot(fig_scat, fig_hist, fig_res_vs_obs, fig_ts_res,
                    nrows = 2, shareX = FALSE,
                    titleX = TRUE, titleY = TRUE, margin = 0.07)
  } else {
    plotly::subplot(fig_scat, fig_hist, fig_res_vs_obs,
                    nrows = 1, shareX = FALSE,
                    titleX = TRUE, titleY = TRUE, margin = 0.07)
  }

  plotly::layout(
    fig_combined,
    title = list(
      text = paste0(
        "<b>", .i18n("eval_dashboard", lang), " \u2014 ", target_var, "</b>",
        if (!is.null(station_code))
          paste0("  |  ", .i18n("station_label", lang), ": ", station_code)
      ),
      font = list(size = 13)
    ),
    showlegend = TRUE
  )
}


# =============================================================================
#  GGPLOT2 — evaluation dashboard builder
# =============================================================================

#' @keywords internal
#' @noRd
.build_eval_ggplot <- function(
    df_valid, y_true, y_pred, residuals,
    date_col, target_var, station_code,
    r2_pool, rmse_p, mae_p, n_pool,
    lang, color_palette
) {

  pal    <- .get_scientific_palette(color_palette)
  theme_eval <- ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(size = 9, face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = pal$grid_line,
                                                linewidth = 0.3)
    )

  df_scat <- data.frame(observed = y_true, predicted = y_pred,
                         residual = residuals)

  # Panel 1: Observed vs Predicted
  p_scat <- ggplot2::ggplot(df_scat,
                             ggplot2::aes(x = observed, y = predicted)) +
    ggplot2::geom_point(colour = pal$scatter, alpha = 0.35, size = 1.2) +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                          colour = pal$reference_line, linetype = "dashed",
                          linewidth = 0.8) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, colour = "grey40",
                          fill = "grey85", linewidth = 0.5, alpha = 0.4,
                          formula = y ~ x) +
    ggplot2::labs(
      title = sprintf(
        "%s\nR\u00b2 = %.3f  |  RMSE = %.3f  |  MAE = %.3f  |  n = %d",
        .i18n("obs_vs_pred", lang), r2_pool, rmse_p, mae_p, n_pool
      ),
      x = .i18n("observed", lang),
      y = .i18n("predicted", lang)
    ) +
    theme_eval

  # Panel 2: Residual histogram
  p_hist <- ggplot2::ggplot(df_scat, ggplot2::aes(x = residual)) +
    ggplot2::geom_histogram(bins = 40, fill = pal$residual_hist,
                             colour = "white", alpha = 0.85) +
    ggplot2::geom_vline(xintercept = 0,
                         colour = pal$reference_line, linetype = "dashed",
                         linewidth = 0.8) +
    ggplot2::labs(
      title = .i18n("res_dist", lang),
      x     = .i18n("residual_axis", lang),
      y     = .i18n("count", lang)
    ) +
    theme_eval

  # Panel 3: Residuals vs Observed
  p_res_vs_obs <- ggplot2::ggplot(df_scat,
                                   ggplot2::aes(x = observed, y = residual)) +
    ggplot2::geom_point(colour = pal$scatter, alpha = 0.35, size = 0.9) +
    ggplot2::geom_hline(yintercept = 0,
                         colour = pal$zero_line, linetype = "dashed",
                         linewidth = 0.8) +
    ggplot2::labs(
      title = .i18n("res_vs_obs", lang),
      x     = .i18n("observed", lang),
      y     = .i18n("residuals", lang)
    ) +
    theme_eval

  has_date <- date_col %in% names(df_valid)

  if (has_date) {
    df_scat[[date_col]] <- df_valid[[date_col]]
    p_ts <- ggplot2::ggplot(df_scat,
                             ggplot2::aes(x = .data[[date_col]], y = residual)) +
      ggplot2::geom_point(colour = pal$residual_point, alpha = 0.35, size = 0.8) +
      ggplot2::geom_hline(yintercept = 0,
                           colour = pal$zero_line, linetype = "dashed",
                           linewidth = 0.8) +
      ggplot2::labs(
        title = .i18n("res_over_time", lang),
        x     = .i18n("date_time", lang),
        y     = .i18n("residuals", lang)
      ) +
      ggplot2::scale_x_datetime(date_labels = "%b %Y") +
      theme_eval +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))

    combined <- patchwork::wrap_plots(p_scat, p_hist, p_res_vs_obs, p_ts,
                                       ncol = 2, nrow = 2)
  } else {
    combined <- patchwork::wrap_plots(p_scat, p_hist, p_res_vs_obs,
                                       ncol = 2, nrow = 2)
  }

  combined +
    patchwork::plot_annotation(
      title = paste0(
        .i18n("eval_dashboard", lang), " \u2014 ", target_var,
        if (!is.null(station_code))
          paste0(" | ", .i18n("station_label", lang), ": ", station_code)
      ),
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 11, face = "bold")
      )
    )
}


# =============================================================================
#  DT TABLE HELPERS
# =============================================================================

#' Styled DT table — per-station evaluation metrics
#' @keywords internal
#' @noRd
.dt_metrics_table <- function(metrics, target_var = "", lang = "en") {

  col_labels <- c(
    station_id     = .i18n("station_col",   lang),
    variable       = "Variable",
    rmse           = "RMSE",
    mae            = "MAE",
    r_squared      = "R\u00b2",
    smape          = "sMAPE (%)",
    slope_bias     = "Slope bias",
    n_obs          = .i18n("n_obs_col",     lang),
    gap_percentage = .i18n("gap_pct_col",   lang),
    gap_mechanism  = .i18n("mechanism_col", lang)
  )

  keep_cols    <- intersect(names(col_labels), names(metrics))
  display_data <- as.data.frame(metrics[, keep_cols])
  names(display_data) <- col_labels[keep_cols]

  rmse_idx  <- which(names(display_data) == "RMSE") - 1L
  order_idx <- if (length(rmse_idx) == 1L) list(list(rmse_idx, "asc")) else list()

  DT::datatable(
    display_data,
    caption    = htmltools::tags$caption(
      style = "caption-side:top;text-align:left;font-weight:bold;font-size:13px;",
      paste0(.i18n("perf_metrics", lang), " \u2014 ", target_var)
    ),
    extensions = "Buttons",
    options    = list(
      dom        = "Bfrtip",
      buttons    = list("csv", "excel", "copy"),
      pageLength = 20,
      order      = order_idx,
      scrollX    = TRUE
    ),
    rownames = FALSE,
    class    = "stripe hover compact"
  ) |>
    DT::formatRound(
      columns = intersect(
        c("RMSE", "MAE", "R\u00b2", "sMAPE (%)", "Slope bias"),
        names(display_data)
      ),
      digits = 3
    ) |>
    DT::formatStyle(
      columns    = intersect(c("RMSE", "MAE"), names(display_data)),
      background = DT::styleInterval(
        cuts   = c(1, 5, 15),
        values = c("#d4edda", "#fff3cd", "#fde2e0", "#f5c6cb")
      )
    ) |>
    DT::formatStyle(
      columns    = intersect("R\u00b2", names(display_data)),
      background = DT::styleInterval(
        cuts   = c(0, 0.5, 0.8),
        values = c("#f5c6cb", "#fde2e0", "#fff3cd", "#d4edda")
      )
    ) |>
    DT::formatStyle(
      columns    = intersect("sMAPE (%)", names(display_data)),
      background = DT::styleInterval(
        cuts   = c(10, 30, 60),
        values = c("#d4edda", "#fff3cd", "#fde2e0", "#f5c6cb")
      )
    )
}


#' Styled DT table — per-station production imputation summary
#' @keywords internal
#' @noRd
.dt_production_table <- function(prod_metrics, lang = "en") {

  col_labels <- c(
    station_code = .i18n("station_col",     lang),
    variable     = "Variable",
    n_obs        = .i18n("total_obs",       lang),
    n_imputed    = .i18n("gaps_filled_col", lang),
    pct_imputed  = .i18n("imputed_pct",     lang)
  )

  keep_cols    <- intersect(names(col_labels), names(prod_metrics))
  display_data <- as.data.frame(prod_metrics[, keep_cols])
  names(display_data) <- col_labels[keep_cols]

  pct_col   <- .i18n("imputed_pct", lang)
  pct_idx   <- which(names(display_data) == pct_col) - 1L
  order_idx <- if (length(pct_idx) == 1L) list(list(pct_idx, "desc")) else list()

  DT::datatable(
    display_data,
    caption    = htmltools::tags$caption(
      style = "caption-side:top;text-align:left;font-weight:bold;font-size:13px;",
      .i18n("imputation_summ", lang)
    ),
    extensions = "Buttons",
    options    = list(
      dom        = "Bfrtip",
      buttons    = list("csv", "excel", "copy"),
      pageLength = 20,
      order      = order_idx,
      scrollX    = TRUE
    ),
    rownames = FALSE,
    class    = "stripe hover compact"
  ) |>
    DT::formatRound(intersect(pct_col, names(display_data)), digits = 1) |>
    DT::formatStyle(
      intersect(pct_col, names(display_data)),
      background = DT::styleInterval(
        cuts   = c(10, 30, 50),
        values = c("#d4edda", "#fff3cd", "#fde2e0", "#f5c6cb")
      )
    )
}


# =============================================================================
#  DOWNSAMPLING HELPER
# =============================================================================

#' Smart downsampling — keeps every nth row for line traces only.
#'
#' Imputed marker points are **never** downsampled.
#' @keywords internal
#' @noRd
.maybe_downsample <- function(df, n_threshold = 5000L, target_n = 3000L,
                               verbose = FALSE, lang = "en") {
  n      <- nrow(df)
  use_gl <- n > n_threshold

  if (use_gl) {
    step    <- max(1L, floor(n / target_n))
    line_df <- df[seq(1L, n, by = step), ]
    if (verbose)
      cli::cli_alert_warning(
        .i18n("downsample_warn", lang, n = n, s = nrow(line_df))
      )
  } else {
    line_df <- df
  }

  list(line_df = line_df, use_gl = use_gl)
}


# =============================================================================
#  MINIMUM-POINTS GUARD
# =============================================================================

#' Return FALSE (with a warning) when a station has too few valid observations.
#' @keywords internal
#' @noRd
.validate_min_points <- function(n_valid, station_code, min_points, lang = "en") {
  if (!is.null(station_code) && !is.null(min_points) && n_valid < min_points) {
    cli::cli_alert_warning(
      .i18n("warn_few_pts", lang,
            st = station_code, n = n_valid, min_pts = min_points)
    )
    return(FALSE)
  }
  TRUE
}


# =============================================================================
#  SCIENTIFIC COLOR PALETTE  (with session-level cache)
# =============================================================================

# In-memory cache: avoids re-invoking pal_*() on every builder call.
# The cache lives for the duration of the R session and is keyed by
# palette_name, so switching palettes mid-session works correctly.
.palette_cache <- new.env(parent = emptyenv())

#' @keywords internal
#' @noRd
.get_scientific_palette <- function(palette_name = "npg") {

  valid_palettes <- c(
    "npg", "aaas", "nejm", "lancet", "jama", "bmj", "jco", "frontiers", "gsea",
    "uchicago", "primer", "atlassian", "observable",
    "d3", "igv", "cosmic", "locuszoom", "ucscgb",
    "startrek", "tron", "futurama", "rickandmorty", "simpsons", "flatui",
    "bs5", "material", "tw3"
  )

  if (!palette_name %in% valid_palettes) {
    cli::cli_warn("Palette {.val {palette_name}} not recognised. Using {.val npg}.")
    palette_name <- "npg"
  }

  # Return cached result immediately if available
  if (exists(palette_name, envir = .palette_cache, inherits = FALSE))
    return(get(palette_name, envir = .palette_cache, inherits = FALSE))

  # First call for this palette_name: build and store
  pal_func <- switch(
    palette_name,
    "npg"          = ggsci::pal_npg(),
    "aaas"         = ggsci::pal_aaas(),
    "nejm"         = ggsci::pal_nejm(),
    "lancet"       = ggsci::pal_lancet(),
    "jama"         = ggsci::pal_jama(),
    "bmj"          = ggsci::pal_bmj(),
    "jco"          = ggsci::pal_jco(),
    "frontiers"    = ggsci::pal_frontiers(),
    "gsea"         = ggsci::pal_gsea(),
    "uchicago"     = ggsci::pal_uchicago(),
    "primer"       = ggsci::pal_primer(),
    "atlassian"    = ggsci::pal_atlassian(),
    "observable"   = ggsci::pal_observable(),
    "d3"           = ggsci::pal_d3(),
    "igv"          = ggsci::pal_igv(),
    "cosmic"       = ggsci::pal_cosmic(),
    "locuszoom"    = ggsci::pal_locuszoom(),
    "ucscgb"       = ggsci::pal_ucscgb(),
    "startrek"     = ggsci::pal_startrek(),
    "tron"         = ggsci::pal_tron(),
    "futurama"     = ggsci::pal_futurama(),
    "rickandmorty" = ggsci::pal_rickandmorty(),
    "simpsons"     = ggsci::pal_simpsons(),
    "flatui"       = ggsci::pal_flatui(),
    "bs5"          = ggsci::pal_bs5(),
    "material"     = ggsci::pal_material(),
    "tw3"          = ggsci::pal_tw3(),
    ggsci::pal_npg()
  )

  colors <- pal_func(10)

  result <- list(
    original_observed = colors[2],
    filled_imputed    = colors[1],
    imputed_marker    = colors[1],
    imputed_line      = colors[1],
    scatter           = colors[3],
    reference_line    = if (palette_name %in% c("startrek", "tron", "futurama",
                                                  "rickandmorty", "simpsons"))
                          "#333333" else colors[8],
    residual_hist     = colors[4],
    residual_point    = colors[5],
    zero_line         = "#999999",
    grid_line         = "#EEEEEE"
  )

  assign(palette_name, result, envir = .palette_cache)
  result
}


# =============================================================================
#  i18n STRINGS
# =============================================================================

#' @keywords internal
#' @noRd
.i18n_strings <- list(

  en = list(
    # Error messages
    err_not_climasus      = "{.red {cli::symbol$cross} Input is not a {.cls climasus_df} object.}",
    err_orig_not_climasus = "{.red {cli::symbol$cross} df_original is not a {.cls climasus_df} object.}",
    err_pipeline_hint     = "This function requires data processed by the {.pkg climasus4r} pipeline.\n  1. {.code sus_climate_inmet(...)}\n  2. {.code sus_climate_fill(...)}",
    err_wrong_stage       = "Wrong data stage. Current: {current}. Required: {required}.",
    stage_ok              = "Data stage validated.",

    # Time-series
    title_series    = "Gap-Filling",
    station_label   = "Station",
    gaps_filled     = "gaps filled",
    original        = "Observed (with gaps)",
    filled_series   = "Imputed series",
    imputed_pts     = "Imputed values",
    date_time       = "Date / Time",

    # Evaluation
    obs_vs_pred     = "Observed vs Predicted",
    observed        = "Observed value",
    predicted       = "Predicted value",
    residuals       = "Residuals",
    res_dist        = "Residual distribution",
    res_over_time   = "Residuals over time",
    res_vs_obs      = "Residuals vs observed",
    residual_axis   = "Residual (observed \u2212 predicted)",
    count           = "Count",
    eval_dashboard  = "Model evaluation dashboard",
    station_obs     = "Station observations",
    one_to_one      = "1:1 reference line",
    res_over_t      = "Residual over time",

    # Tables
    imputation_summ = "Imputation summary by station",
    perf_metrics    = "Performance metrics by station",
    total_obs       = "Total observations",
    gaps_filled_col = "Gaps filled",
    imputed_pct     = "Imputed (%)",
    station_col     = "Station",
    n_obs_col       = "N observations",
    gap_pct_col     = "Gap proportion (%)",
    mechanism_col   = "Gap mechanism",

    # Warnings & Info
    warn_few_pts    = "Station '{st}' has only {n} valid points (< {min_pts} minimum). Skipping.",
    warn_no_flag    = "Flag column '{fc}' not found; imputed points derived from NAs in original data.",
    info_summary    = "Gap-fill summary for '{v}': {nf} gaps filled ({pct}%)",
    info_eval       = "Evaluation summary for '{v}': n = {n} | RMSE = {rmse} | MAE = {mae} | R\u00b2 = {r2}",

    # Verbose
    verbose_period  = "  Period  : {from} to {to}",
    verbose_nas     = "  NAs before / after : {na_before} / {na_after}",
    verbose_pts     = "  Points by station :",
    downsample_warn = "Large dataset ({n} rows): line traces downsampled to {s} rows (markers unchanged)."
  ),

  pt = list(
    # Error messages
    err_not_climasus      = "{.red {cli::symbol$cross} A entrada nao e um objeto {.cls climasus_df}.}",
    err_orig_not_climasus = "{.red {cli::symbol$cross} df_original nao e um objeto {.cls climasus_df}.}",
    err_pipeline_hint     = "Esta funcao requer dados processados pelo pipeline {.pkg climasus4r}.\n  1. {.code sus_climate_inmet(...)}\n  2. {.code sus_climate_fill(...)}",
    err_wrong_stage       = "Estagio incorreto. Atual: {current}. Requerido: {required}.",
    stage_ok              = "Estagio de dados validado.",

    # Time-series
    title_series    = "Preenchimento de Falhas",
    station_label   = "Estacao",
    gaps_filled     = "falhas preenchidas",
    original        = "Observado (com falhas)",
    filled_series   = "Serie imputada",
    imputed_pts     = "Valores imputados",
    date_time       = "Data / Hora",

    # Evaluation
    obs_vs_pred     = "Observado vs Predito",
    observed        = "Valor observado",
    predicted       = "Valor predito",
    residuals       = "Residuos",
    res_dist        = "Distribuicao dos residuos",
    res_over_time   = "Residuos ao longo do tempo",
    res_vs_obs      = "Residuos vs observado",
    residual_axis   = "Residuo (observado \u2212 predito)",
    count           = "Contagem",
    eval_dashboard  = "Painel de avaliacao do modelo",
    station_obs     = "Observacoes por estacao",
    one_to_one      = "Linha de referencia 1:1",
    res_over_t      = "Residuo ao longo do tempo",

    # Tables
    imputation_summ = "Resumo de imputacao por estacao",
    perf_metrics    = "Metricas de desempenho por estacao",
    total_obs       = "Total de observacoes",
    gaps_filled_col = "Falhas preenchidas",
    imputed_pct     = "Imputado (%)",
    station_col     = "Estacao",
    n_obs_col       = "N observacoes",
    gap_pct_col     = "Proporcao de falhas (%)",
    mechanism_col   = "Mecanismo da falha",

    # Warnings & Info
    warn_few_pts    = "Estacao '{st}' tem apenas {n} pontos validos (< {min_pts} minimo). Ignorando.",
    warn_no_flag    = "Coluna de flag '{fc}' nao encontrada; imputados derivados dos NAs originais.",
    info_summary    = "Resumo para '{v}': {nf} falhas preenchidas ({pct}%)",
    info_eval       = "Avaliacao para '{v}': n = {n} | RMSE = {rmse} | MAE = {mae} | R\u00b2 = {r2}",

    # Verbose
    verbose_period  = "  Periodo : {from} ate {to}",
    verbose_nas     = "  NAs antes / depois : {na_before} / {na_after}",
    verbose_pts     = "  Pontos por estacao :",
    downsample_warn = "Dataset grande ({n} linhas): linhas amostradas para {s} (marcadores inalterados)."
  ),

  es = list(
    # Error messages
    err_not_climasus      = "{.red {cli::symbol$cross} La entrada no es un objeto {.cls climasus_df}.}",
    err_orig_not_climasus = "{.red {cli::symbol$cross} df_original no es un objeto {.cls climasus_df}.}",
    err_pipeline_hint     = "Esta funcion requiere datos procesados por el pipeline {.pkg climasus4r}.\n  1. {.code sus_climate_inmet(...)}\n  2. {.code sus_climate_fill(...)}",
    err_wrong_stage       = "Etapa incorrecta. Actual: {current}. Requerida: {required}.",
    stage_ok              = "Etapa de datos validada.",

    # Time-series
    title_series    = "Relleno de Brechas",
    station_label   = "Estacion",
    gaps_filled     = "brechas completadas",
    original        = "Observado (con brechas)",
    filled_series   = "Serie imputada",
    imputed_pts     = "Valores imputados",
    date_time       = "Fecha / Hora",

    # Evaluation
    obs_vs_pred     = "Observado vs Predicho",
    observed        = "Valor observado",
    predicted       = "Valor predicho",
    residuals       = "Residuos",
    res_dist        = "Distribucion de residuos",
    res_over_time   = "Residuos en el tiempo",
    res_vs_obs      = "Residuos vs observado",
    residual_axis   = "Residuo (observado \u2212 predicho)",
    count           = "Conteo",
    eval_dashboard  = "Panel de evaluacion del modelo",
    station_obs     = "Observaciones por estacion",
    one_to_one      = "Linea de referencia 1:1",
    res_over_t      = "Residuo en el tiempo",

    # Tables
    imputation_summ = "Resumen de imputacion por estacion",
    perf_metrics    = "Metricas de rendimiento por estacion",
    total_obs       = "Total observaciones",
    gaps_filled_col = "Brechas completadas",
    imputed_pct     = "Imputado (%)",
    station_col     = "Estacion",
    n_obs_col       = "N observaciones",
    gap_pct_col     = "Proporcion de brechas (%)",
    mechanism_col   = "Mecanismo de brecha",

    # Warnings & Info
    warn_few_pts    = "Estacion '{st}' tiene solo {n} puntos validos (< {min_pts} minimo). Omitiendo.",
    warn_no_flag    = "Columna flag '{fc}' no encontrada; imputados derivados de NAs originales.",
    info_summary    = "Resumen para '{v}': {nf} brechas completadas ({pct}%)",
    info_eval       = "Evaluacion para '{v}': n = {n} | RMSE = {rmse} | MAE = {mae} | R\u00b2 = {r2}",

    # Verbose
    verbose_period  = "  Periodo : {from} hasta {to}",
    verbose_nas     = "  NAs antes / despues : {na_before} / {na_after}",
    verbose_pts     = "  Puntos por estacion :",
    downsample_warn = "Dataset grande ({n} filas): lineas muestreadas a {s} filas (marcadores sin cambio)."
  )
)


#' Retrieve an i18n string with simple placeholder substitution
#' @keywords internal
#' @noRd
.i18n <- function(key, lang = "en", ...) {
  lang <- match.arg(lang, c("en", "pt", "es"))
  str  <- .i18n_strings[[lang]][[key]]
  if (is.null(str)) str <- .i18n_strings[["en"]][[key]]
  if (is.null(str)) return(key)
  dots <- list(...)
  for (nm in names(dots))
    str <- gsub(paste0("\\{", nm, "\\}"), as.character(dots[[nm]]), str,
                fixed = FALSE)
  str
}


# =============================================================================
#  VERBOSE LOGGING
# =============================================================================

#' @keywords internal
#' @noRd
.log_verbose <- function(verbose, ...) {
  if (isTRUE(verbose)) cli::cli_text(...)
}