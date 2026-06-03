# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "date", "value", "variable", "outcome_val", "climate_val",
  "month_num", "month_lbl", "lag_val", "acf_val", "sig",
  "x_val", "y_val", "col_name", ".data", "density",
  "var1", "var2", "corr"
))

# ── Exported function ─────────────────────────────────────────────────────────

#' Visualise Climate-Health Aggregate Data
#'
#' @description
#' `sus_climate_plot_aggregate()` produces exploratory visualisations for the
#' output of [sus_climate_aggregate()]. Six complementary plot types cover the
#' full exploratory workflow before modelling: time-series overlay, scatter with
#' smooth, cross-correlation (CCF), distribution, correlation matrix, and
#' seasonal patterns. Modelling-specific plots (DLNM surfaces, residual
#' diagnostics, RR tables) are deliberately excluded to avoid duplication with
#' the dedicated `sus_mod_plot_*` family.
#'
#' @param df A `climasus_df` at `stage = "climate"` produced by
#'   [sus_climate_aggregate()]. Must contain at least `date`, `code_muni`, a
#'   health outcome column, and one or more climate exposure columns.
#'
#' @param outcome_col Character. Name of the health-outcome column
#'   (e.g. `"n_obitos"`, `"n_internacoes"`). If `NULL` (default) the first
#'   integer column that is not a climate variable is used.
#'
#' @param climate_cols Character vector of climate column names to visualise.
#'   If `NULL` (default) all columns that match the naming conventions of
#'   [sus_climate_aggregate()] are detected automatically (`lag*`, `mvwin*`,
#'   `off*to*`, `gdd*`, `_lag*`, or known INMET variable names).
#'
#' @param plot_type Character. One of:
#'   \describe{
#'     \item{`"timeseries"`}{Dual-axis time-series: rescaled climate exposure
#'       (area) overlaid on health outcome (line + points). One panel per
#'       climate column if multiple columns are present.}
#'     \item{`"scatter"`}{Scatter plot with loess/GAM smooth: climate exposure
#'       on x-axis, health outcome on y-axis. Shows the marginal association
#'       ignoring temporal structure.}
#'     \item{`"ccf"`}{Cross-Correlation Function bars from \eqn{-30} to
#'       `+max_lag` days. Highlights lags with significant correlation
#'       (|r| > 2/sqrt(n)) in a contrasting colour. Useful for identifying
#'       the optimal lag before modelling.}
#'     \item{`"distribution"`}{Histogram + density overlay of each climate
#'       column. For `discrete_lag` outputs the multiple lag columns are
#'       overlaid for comparison.}
#'     \item{`"corr_matrix"`}{Heatmap of Spearman correlations between all
#'       climate columns and the outcome. Especially informative when the
#'       input was produced with `temporal_strategy = "discrete_lag"`.}
#'     \item{`"seasonal"`}{Monthly boxplots of the climate exposure and the
#'       outcome incidence, side-by-side, to reveal seasonal co-variation.}
#'   }
#'   Default `"timeseries"`.
#'
#' @param smooth_method Character. Smoothing method for the scatter plot:
#'   `"loess"` (default) or `"gam"`.
#'
#' @param max_lag Integer. Maximum lag (days) shown in the CCF plot.
#'   Default `30L`.
#'
#' @param alpha Numeric. Confidence level for CCF significance bounds
#'   (default `0.05`; bounds drawn at \eqn{\pm 2/\sqrt{n}}).
#'
#' @param interactive Logical. If `TRUE`, returns a `plotly` interactive
#'   version. Requires `plotly` to be installed. Default `FALSE`.
#'
#' @param lang Character. Language for axis labels and titles:
#'   `"pt"` (default), `"en"`, `"es"`.
#'
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return A `ggplot2` object (or a `plotly` object when
#'   `interactive = TRUE`). If `climate_cols` contains more than one column
#'   and `plot_type %in% c("timeseries", "distribution")`, a list of
#'   `ggplot2` objects is returned (one per column) wrapped in an invisible
#'   `patchwork` when `patchwork` is installed.
#'
#' @seealso [sus_climate_aggregate()], [sus_climate_plot_fill()],
#'   [sus_climate_plot_heatwaves()], [sus_mod_plot_dlnm()]
#'
#' @examples
#' \dontrun{
#' # Build aggregate data (exact strategy)
#' df_agg <- sus_climate_aggregate(
#'   health_data       = sf_sim_spatial,
#'   climate_data      = df_inmet,
#'   climate_var       = "tair_dry_bulb_c",
#'   temporal_strategy = "exact"
#' )
#'
#' # Time-series overlay
#' sus_climate_plot_aggregate(df_agg, plot_type = "timeseries", lang = "pt")
#'
#' # Discrete lag: correlation matrix across all lag columns
#' df_lag <- sus_climate_aggregate(
#'   health_data       = sf_sim_spatial,
#'   climate_data      = df_inmet,
#'   climate_var       = "tair_dry_bulb_c",
#'   temporal_strategy = "discrete_lag",
#'   lag_days          = c(7, 14, 21)
#' )
#' sus_climate_plot_aggregate(df_lag, plot_type = "corr_matrix", lang = "en")
#'
#' # Cross-correlation
#' sus_climate_plot_aggregate(df_agg, plot_type = "ccf", max_lag = 21L)
#' }
#'
#' @export
#' @importFrom dplyr select mutate filter summarise n_distinct all_of
#'   rename arrange n everything
#' @importFrom tidyr pivot_longer
#' @importFrom stats cor ccf sd median
#' @importFrom rlang .data check_installed
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
sus_climate_plot_aggregate <- function(
    df,
    outcome_col   = NULL,
    climate_cols  = NULL,
    plot_type     = "timeseries",
    smooth_method = "loess",
    max_lag       = 30L,
    alpha         = 0.05,
    interactive   = FALSE,
    lang          = "pt",
    verbose       = TRUE) {

  # ── 1. Validate language ───────────────────────────────────────────────────
  if (!is.character(lang) || length(lang) != 1 || !lang %in% c("pt", "en", "es")) {
    cli::cli_abort("{.arg lang} must be 'pt', 'en', or 'es'.")
  }
  lbl <- .cpa_labels[[lang]]

  valid_types <- c("timeseries", "scatter", "ccf",
                   "distribution", "corr_matrix", "seasonal")
  if (length(plot_type) != 1 || !plot_type %in% valid_types) {
    cli::cli_abort(c(
      "{.arg plot_type} must be one of: {.val {valid_types}}.",
      "i" = "Modelling plots (DLNM, residuals, RR) are in {.fn sus_mod_plot_dlnm}."
    ))
  }

  rlang::check_installed("ggplot2", reason = "to produce aggregate climate plots")

  # ── 2. Validate input df ───────────────────────────────────────────────────
  if (!inherits(df, "data.frame")) {
    cli::cli_abort("{.arg df} must be a data.frame or climasus_df.")
  }
  # Drop sf geometry for plotting
  if (inherits(df, "sf")) {
    df <- sf::st_drop_geometry(df)
  }

  if (!"date" %in% names(df)) {
    cli::cli_abort("{.arg df} must contain a {.val date} column.")
  }
  df$date <- as.Date(df$date)

  # ── 3. Auto-detect columns ─────────────────────────────────────────────────
  if (is.null(climate_cols)) {
    climate_cols <- .cpa_detect_climate_cols(df)
    if (length(climate_cols) == 0) {
      cli::cli_abort(c(
        "No climate columns detected in {.arg df}.",
        "i" = "Pass column names explicitly via {.arg climate_cols}."
      ))
    }
  } else {
    missing_c <- setdiff(climate_cols, names(df))
    if (length(missing_c) > 0) {
      cli::cli_abort("Climate column(s) not found: {.val {missing_c}}.")
    }
  }

  if (is.null(outcome_col)) {
    outcome_col <- .cpa_detect_outcome_col(df, climate_cols)
    if (is.null(outcome_col)) {
      cli::cli_abort(c(
        "No health-outcome column detected in {.arg df}.",
        "i" = "Pass the column name via {.arg outcome_col}."
      ))
    }
  } else {
    if (!outcome_col %in% names(df)) {
      cli::cli_abort("Outcome column {.val {outcome_col}} not found in {.arg df}.")
    }
  }

  if (verbose) {
    cli::cli_h1(paste(lbl$title, "-", .cpa_type_label(plot_type, lang)))
    cli::cli_alert_info(paste0(
      lbl$n_rows, ": ", nrow(df), " | ",
      lbl$outcome_lbl, ": ", outcome_col, " | ",
      lbl$climate_lbl, ": ", paste(climate_cols, collapse = ", ")))
  }

  # ── 4. Dispatch to plot function ───────────────────────────────────────────
  p <- switch(plot_type,
    timeseries  = .cpa_timeseries(df, climate_cols, outcome_col, lbl),
    scatter     = .cpa_scatter(df, climate_cols, outcome_col,
                                smooth_method, alpha, lbl),
    ccf         = .cpa_ccf(df, climate_cols, outcome_col,
                            max_lag, alpha, lbl),
    distribution = .cpa_distribution(df, climate_cols, lbl),
    corr_matrix = .cpa_corr_matrix(df, climate_cols, outcome_col, lbl),
    seasonal    = .cpa_seasonal(df, climate_cols, outcome_col, lbl)
  )

  # ── 5. Optionally convert to plotly ────────────────────────────────────────
  if (interactive) {
    rlang::check_installed("plotly", reason = "for interactive climate aggregate plots")
    p <- plotly::ggplotly(p)
  }

  if (verbose) {
    cli::cli_alert_success(lbl$done)
  }
  p
}


# ── Internal: column detection ───────────────────────────────────────────────

#' @keywords internal
#' @noRd
.cpa_detect_climate_cols <- function(df) {
  meta_cols <- c("date", "code_muni", "code_muni_7", "name_muni",
                 "code_state", "abbrev_state", "geom", "geometry",
                 "region", "UF", "latitude", "longitude", "altitude")
  candidate <- setdiff(names(df), meta_cols)

  climate_rx <- c(
    "^lag\\d+_",                             # discrete_lag: lag7_tair_dry_bulb_c
    "^mvwin\\d+_",                           # moving_window: mvwin14_tair_dry_bulb_c
    "^off\\d+to\\d+_",                       # offset_window: off7to14_tair_dry_bulb_c
    "^gdd\\d+",                              # degree_days: gdd21_tbase11
    "_lag\\d+$",                             # distributed_lag: tair_dry_bulb_c_lag0
    "^(tair|patm|rh_|dew_|rainfall|ws_|wd_|sr_|cdd|hdd|wbgt|hi_|utci|pet|thi|diurnal|vapor)",
    "^(spi_|spei_|pdsi|smvi|chirps|era5|cams|ghap|merra2|fires|prodes)"
  )

  is_climate <- vapply(candidate, function(col) {
    any(vapply(climate_rx, function(rx) grepl(rx, col), logical(1)))
  }, logical(1))

  candidate[is_climate]
}

#' @keywords internal
#' @noRd
.cpa_detect_outcome_col <- function(df, climate_cols) {
  meta_cols  <- c("date", "code_muni", "code_muni_7", "name_muni",
                  "code_state", "abbrev_state", "geom", "geometry")
  candidate  <- setdiff(names(df), c(meta_cols, climate_cols))
  int_cols   <- candidate[vapply(candidate, function(col) {
    is.integer(df[[col]]) || is.numeric(df[[col]])
  }, logical(1))]
  # Prefer columns with typical health-outcome name prefixes
  preferred_rx <- c("^n_", "^count", "obito", "internac", "caso", "morte",
                    "hospitaliz", "death", "case", "admission")
  named <- int_cols[vapply(int_cols, function(col) {
    any(vapply(preferred_rx, function(rx) grepl(rx, col, ignore.case = TRUE),
               logical(1)))
  }, logical(1))]
  if (length(named) > 0) return(named[1])
  if (length(int_cols) > 0) return(int_cols[1])
  NULL
}

#' @keywords internal
#' @noRd
.cpa_type_label <- function(plot_type, lang) {
  labels <- list(
    pt = c(timeseries = "S\u00e9rie Temporal", scatter = "Dispers\u00e3o",
           ccf = "Correla\u00e7\u00e3o Cruzada", distribution = "Distribui\u00e7\u00e3o",
           corr_matrix = "Matriz de Correla\u00e7\u00e3o", seasonal = "Padr\u00e3o Sazonal"),
    en = c(timeseries = "Time Series", scatter = "Scatter",
           ccf = "Cross-Correlation", distribution = "Distribution",
           corr_matrix = "Correlation Matrix", seasonal = "Seasonal Pattern"),
    es = c(timeseries = "Serie Temporal", scatter = "Dispersi\u00f3n",
           ccf = "Correlaci\u00f3n Cruzada", distribution = "Distribuci\u00f3n",
           corr_matrix = "Matriz de Correlaci\u00f3n", seasonal = "Patr\u00f3n Estacional")
  )
  labels[[lang]][[plot_type]] %||% plot_type
}


# ── Internal: shared theme & palette ─────────────────────────────────────────

#' @keywords internal
#' @noRd
.cpa_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor  = ggplot2::element_blank(),
      panel.grid.major  = ggplot2::element_line(color = "#E8E8E4", linewidth = 0.4),
      plot.title        = ggplot2::element_text(face = "bold", size = 13, hjust = 0),
      plot.subtitle     = ggplot2::element_text(color = "#5F5E5A", size = 10, hjust = 0),
      plot.caption      = ggplot2::element_text(color = "#888780", size = 9, hjust = 1),
      axis.title        = ggplot2::element_text(size = 10, color = "#444441"),
      axis.text         = ggplot2::element_text(size = 9, color = "#5F5E5A"),
      legend.position   = "bottom",
      strip.text        = ggplot2::element_text(face = "bold", size = 10),
      plot.margin       = ggplot2::margin(10, 14, 8, 10)
    )
}

.CPA_PAL <- c(
  primary   = "#185FA5",
  secondary = "#D85A30",
  tertiary  = "#1D9E75",
  light     = "#C8D9EF",
  neutral   = "#888780"
)


# ── Internal: timeseries ──────────────────────────────────────────────────────

#' @keywords internal
#' @noRd
.cpa_timeseries <- function(df, climate_cols, outcome_col, lbl) {
  # Use only first climate col if multiple; facet if needed
  # For simplicity, handle up to 4 climate cols via facet
  col <- climate_cols[1]
  x   <- df[[col]]
  y   <- as.numeric(df[[outcome_col]])

  # Rescale climate to outcome range for dual-axis display
  x_range <- range(x, na.rm = TRUE)
  y_range <- range(y, na.rm = TRUE)
  scale_f  <- diff(y_range) / max(diff(x_range), 1e-9)
  shift_f  <- y_range[1] - x_range[1] * scale_f

  df_plot <- data.frame(
    date          = df$date,
    outcome_val   = y,
    climate_scaled = x * scale_f + shift_f
  )

  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data$date)) +
    ggplot2::geom_area(
      ggplot2::aes(y = .data$climate_scaled),
      fill = .CPA_PAL["light"], color = .CPA_PAL["primary"],
      alpha = 0.5, linewidth = 0.6) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$outcome_val),
      color = .CPA_PAL["secondary"], linewidth = 0.9) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$outcome_val),
      color = .CPA_PAL["secondary"], size = 1.2, alpha = 0.7) +
    ggplot2::scale_x_date(
      date_breaks = "2 months", date_labels = "%b/%y",
      expand = ggplot2::expansion(mult = 0.01)) +
    ggplot2::scale_y_continuous(
      name     = lbl$outcome_lbl,
      sec.axis = ggplot2::sec_axis(
        transform = ~ (. - shift_f) / scale_f,
        name      = col
      )
    ) +
    ggplot2::labs(
      title    = lbl$ts_title,
      subtitle = paste0(lbl$outcome_lbl, ": ", outcome_col,
                        "   |   ", lbl$climate_lbl, ": ", col),
      x        = lbl$date_lbl,
      caption  = paste0("climasus4r \u2022 sus_climate_plot_aggregate()")
    ) +
    .cpa_theme()

  # Facet if multiple climate columns
  if (length(climate_cols) > 1) {
    long <- do.call(rbind, lapply(climate_cols, function(cc) {
      xc   <- df[[cc]]
      sc_f <- diff(y_range) / max(diff(range(xc, na.rm = TRUE)), 1e-9)
      sh_f <- y_range[1] - min(xc, na.rm = TRUE) * sc_f
      data.frame(date = df$date, outcome_val = y,
                 climate_scaled = xc * sc_f + sh_f,
                 col_name = cc)
    }))
    p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$date)) +
      ggplot2::geom_area(
        ggplot2::aes(y = .data$climate_scaled),
        fill = .CPA_PAL["light"], color = .CPA_PAL["primary"], alpha = 0.5) +
      ggplot2::geom_line(
        ggplot2::aes(y = .data$outcome_val),
        color = .CPA_PAL["secondary"], linewidth = 0.8) +
      ggplot2::scale_x_date(date_breaks = "3 months", date_labels = "%b/%y") +
      ggplot2::labs(title = lbl$ts_title, x = lbl$date_lbl,
                    y = lbl$outcome_lbl, caption = "climasus4r") +
      ggplot2::facet_wrap(ggplot2::vars(.data$col_name), ncol = 1, scales = "free_y") +
      .cpa_theme()
  }
  p
}


# ── Internal: scatter ─────────────────────────────────────────────────────────

#' @keywords internal
#' @noRd
.cpa_scatter <- function(df, climate_cols, outcome_col, smooth_method, alpha, lbl) {
  col    <- climate_cols[1]
  df_plot <- data.frame(
    x_val = df[[col]],
    y_val = as.numeric(df[[outcome_col]])
  )
  df_plot <- df_plot[!is.na(df_plot$x_val) & !is.na(df_plot$y_val), ]

  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data$x_val, y = .data$y_val)) +
    ggplot2::geom_point(
      color = .CPA_PAL["primary"], alpha = 0.35, size = 1.6) +
    ggplot2::geom_smooth(
      method  = smooth_method,
      formula = y ~ x,
      se      = TRUE,
      color   = .CPA_PAL["secondary"],
      fill    = .CPA_PAL["light"],
      linewidth = 1.1,
      level   = 1 - alpha) +
    ggplot2::labs(
      title    = lbl$sc_title,
      subtitle = paste0(lbl$climate_lbl, ": ", col,
                        "  \u2192  ", lbl$outcome_lbl, ": ", outcome_col),
      x        = col,
      y        = outcome_col,
      caption  = paste0("smooth = ", smooth_method, " | climasus4r")
    ) +
    .cpa_theme()

  if (length(climate_cols) > 1) {
    long <- do.call(rbind, lapply(climate_cols, function(cc) {
      data.frame(x_val = df[[cc]], y_val = as.numeric(df[[outcome_col]]),
                 col_name = cc)
    }))
    long <- long[!is.na(long$x_val) & !is.na(long$y_val), ]
    p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$x_val, y = .data$y_val)) +
      ggplot2::geom_point(color = .CPA_PAL["primary"], alpha = 0.3, size = 1.2) +
      ggplot2::geom_smooth(method = smooth_method, formula = y ~ x, se = TRUE,
                           color = .CPA_PAL["secondary"], fill = .CPA_PAL["light"],
                           linewidth = 0.9, level = 1 - alpha) +
      ggplot2::labs(title = lbl$sc_title, x = lbl$climate_lbl,
                    y = outcome_col, caption = "climasus4r") +
      ggplot2::facet_wrap(ggplot2::vars(.data$col_name), scales = "free_x") +
      .cpa_theme()
  }
  p
}


# ── Internal: ccf ─────────────────────────────────────────────────────────────

#' @keywords internal
#' @noRd
.cpa_ccf <- function(df, climate_cols, outcome_col, max_lag, alpha, lbl) {
  col     <- climate_cols[1]
  x_ts    <- df[[col]]
  y_ts    <- as.numeric(df[[outcome_col]])

  # Remove rows with NA
  ok   <- !is.na(x_ts) & !is.na(y_ts)
  x_ts <- x_ts[ok]; y_ts <- y_ts[ok]
  n    <- length(x_ts)

  res  <- stats::ccf(x_ts, y_ts, lag.max = max_lag, plot = FALSE)
  ci   <- 2 / sqrt(n)

  df_ccf <- data.frame(
    lag_val = as.integer(res$lag),
    acf_val = as.numeric(res$acf),
    sig     = abs(as.numeric(res$acf)) > ci
  )

  p <- ggplot2::ggplot(df_ccf, ggplot2::aes(x = .data$lag_val, y = .data$acf_val,
                                              fill = .data$sig)) +
    ggplot2::geom_col(width = 0.75, color = "white", linewidth = 0.2) +
    ggplot2::geom_hline(yintercept =  ci, linetype = "dashed",
                        color = .CPA_PAL["secondary"], linewidth = 0.7) +
    ggplot2::geom_hline(yintercept = -ci, linetype = "dashed",
                        color = .CPA_PAL["secondary"], linewidth = 0.7) +
    ggplot2::geom_hline(yintercept = 0, color = .CPA_PAL["neutral"],
                        linewidth = 0.4) +
    ggplot2::scale_fill_manual(
      values = c("FALSE" = .CPA_PAL["light"], "TRUE" = .CPA_PAL["primary"]),
      guide  = ggplot2::guide_legend(title = lbl$sig_lbl)) +
    ggplot2::scale_x_continuous(
      breaks = seq(-max_lag, max_lag, by = 7)) +
    ggplot2::labs(
      title    = lbl$ccf_title,
      subtitle = paste0(col, "  \u2192  ", outcome_col,
                        "  |  n = ", n),
      x        = lbl$lag_lbl,
      y        = lbl$corr_lbl,
      caption  = paste0("\u00b12/\u221an = \u00b1", round(ci, 3),
                        " | climasus4r")
    ) +
    .cpa_theme()
  p
}


# ── Internal: distribution ────────────────────────────────────────────────────

#' @keywords internal
#' @noRd
.cpa_distribution <- function(df, climate_cols, lbl) {
  # Long-format for all climate columns
  long <- do.call(rbind, lapply(climate_cols, function(cc) {
    data.frame(value = df[[cc]], variable = cc,
               stringsAsFactors = FALSE)
  }))
  long <- long[!is.na(long$value), ]

  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$value,
                                           fill = .data$variable,
                                           color = .data$variable)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      alpha = 0.35, bins = 40, position = "identity", linewidth = 0.3) +
    ggplot2::geom_density(alpha = 0, linewidth = 0.8) +
    ggplot2::scale_fill_manual(
      values = stats::setNames(
        grDevices::colorRampPalette(
          c(.CPA_PAL["primary"], .CPA_PAL["tertiary"],
            .CPA_PAL["secondary"]))(length(climate_cols)),
        climate_cols),
      guide = ggplot2::guide_legend(title = lbl$variable_lbl)) +
    ggplot2::scale_color_manual(
      values = stats::setNames(
        grDevices::colorRampPalette(
          c(.CPA_PAL["primary"], .CPA_PAL["tertiary"],
            .CPA_PAL["secondary"]))(length(climate_cols)),
        climate_cols),
      guide = "none") +
    ggplot2::labs(
      title   = lbl$dist_title,
      x       = lbl$climate_lbl,
      y       = lbl$density_lbl,
      caption = "climasus4r"
    ) +
    .cpa_theme()

  if (length(climate_cols) > 3) {
    p <- p + ggplot2::facet_wrap(ggplot2::vars(.data$variable),
                                  scales = "free", ncol = 2)
  }
  p
}


# ── Internal: corr_matrix ─────────────────────────────────────────────────────

#' @keywords internal
#' @noRd
.cpa_corr_matrix <- function(df, climate_cols, outcome_col, lbl) {
  cols_all <- c(climate_cols, outcome_col)
  mat_data <- df[, cols_all, drop = FALSE]
  mat_data <- mat_data[stats::complete.cases(mat_data), ]

  cor_mat  <- stats::cor(mat_data, method = "spearman")

  # Lower triangle only
  cor_df <- as.data.frame(as.table(cor_mat))
  names(cor_df) <- c("var1", "var2", "corr")
  cor_df <- cor_df[as.integer(cor_df$var1) >= as.integer(cor_df$var2), ]
  cor_df$var1 <- factor(cor_df$var1, levels = cols_all)
  cor_df$var2 <- factor(cor_df$var2, levels = rev(cols_all))

  p <- ggplot2::ggplot(cor_df, ggplot2::aes(x = .data$var1, y = .data$var2,
                                              fill = .data$corr)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = round(.data$corr, 2)),
                       size = 3, color = "white", fontface = "bold") +
    ggplot2::scale_fill_gradient2(
      low      = .CPA_PAL["secondary"],
      mid      = "white",
      high     = .CPA_PAL["primary"],
      midpoint = 0,
      limits   = c(-1, 1),
      name     = lbl$spearman_lbl) +
    ggplot2::labs(
      title    = lbl$corr_title,
      subtitle = lbl$corr_subtitle,
      x        = NULL,
      y        = NULL,
      caption  = "climasus4r"
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, size = 8),
      axis.text.y = ggplot2::element_text(size = 8)
    ) +
    .cpa_theme()
  p
}


# ── Internal: seasonal ────────────────────────────────────────────────────────

#' @keywords internal
#' @noRd
.cpa_seasonal <- function(df, climate_cols, outcome_col, lbl) {
  col  <- climate_cols[1]
  mo_lbl <- switch(lbl$lang %||% "pt",
    pt = c("Jan","Fev","Mar","Abr","Mai","Jun",
           "Jul","Ago","Set","Out","Nov","Dez"),
    es = c("Ene","Feb","Mar","Abr","May","Jun",
           "Jul","Ago","Sep","Oct","Nov","Dic"),
    c("Jan","Feb","Mar","Apr","May","Jun",
      "Jul","Aug","Sep","Oct","Nov","Dec")
  )

  df_s <- data.frame(
    month_num = lubridate::month(df$date),
    climate_val = df[[col]],
    outcome_val = as.numeric(df[[outcome_col]])
  )
  df_s <- df_s[!is.na(df_s$climate_val) & !is.na(df_s$outcome_val), ]
  df_s$month_lbl <- factor(mo_lbl[df_s$month_num],
                            levels = mo_lbl)

  # Two-panel: climate top, outcome bottom
  p_clim <- ggplot2::ggplot(df_s,
    ggplot2::aes(x = .data$month_lbl, y = .data$climate_val)) +
    ggplot2::geom_boxplot(fill = .CPA_PAL["light"],
                          color = .CPA_PAL["primary"],
                          outlier.size = 1, outlier.alpha = 0.5) +
    ggplot2::labs(title = lbl$seas_title,
                  subtitle = col,
                  x = NULL, y = col) +
    .cpa_theme()

  p_out <- ggplot2::ggplot(df_s,
    ggplot2::aes(x = .data$month_lbl, y = .data$outcome_val)) +
    ggplot2::geom_boxplot(fill = "#F5D5C8",
                          color = .CPA_PAL["secondary"],
                          outlier.size = 1, outlier.alpha = 0.5) +
    ggplot2::labs(x = lbl$month_lbl, y = outcome_col,
                  caption = "climasus4r") +
    .cpa_theme()

  if (requireNamespace("patchwork", quietly = TRUE)) {
    p_clim / p_out
  } else {
    p_clim
  }
}


# ── Multilingual labels ───────────────────────────────────────────────────────

#' @keywords internal
#' @noRd
.cpa_labels <- list(
  pt = list(
    lang        = "pt",
    title       = "climasus4r \u2014 Visualiza\u00e7\u00e3o do Agregado Climato-Sa\u00fade",
    n_rows      = "Linhas",
    outcome_lbl = "Desfecho",
    climate_lbl = "Clima",
    date_lbl    = "Data",
    lag_lbl     = "Defasagem (dias)",
    corr_lbl    = "Correla\u00e7\u00e3o",
    sig_lbl     = "Significativo",
    density_lbl = "Densidade",
    variable_lbl = "Vari\u00e1vel",
    month_lbl   = "M\u00eas",
    spearman_lbl = "Spearman",
    ts_title    = "S\u00e9rie Temporal: Clima e Sa\u00fade",
    sc_title    = "Dispers\u00e3o: Clima vs Desfecho",
    ccf_title   = "Fun\u00e7\u00e3o de Correla\u00e7\u00e3o Cruzada",
    dist_title  = "Distribui\u00e7\u00e3o das Vari\u00e1veis Clim\u00e1ticas",
    corr_title  = "Matriz de Correla\u00e7\u00e3o de Spearman",
    corr_subtitle = "Correla\u00e7\u00e3o entre defasagens clim\u00e1ticas e desfecho",
    seas_title  = "Padr\u00e3o Sazonal",
    done        = "Plot gerado com sucesso."
  ),
  en = list(
    lang        = "en",
    title       = "climasus4r \u2014 Climate-Health Aggregate Visualisation",
    n_rows      = "Rows",
    outcome_lbl = "Outcome",
    climate_lbl = "Climate",
    date_lbl    = "Date",
    lag_lbl     = "Lag (days)",
    corr_lbl    = "Correlation",
    sig_lbl     = "Significant",
    density_lbl = "Density",
    variable_lbl = "Variable",
    month_lbl   = "Month",
    spearman_lbl = "Spearman",
    ts_title    = "Time Series: Climate and Health",
    sc_title    = "Scatter: Climate vs Outcome",
    ccf_title   = "Cross-Correlation Function",
    dist_title  = "Distribution of Climate Variables",
    corr_title  = "Spearman Correlation Matrix",
    corr_subtitle = "Correlations between climate lags and outcome",
    seas_title  = "Seasonal Pattern",
    done        = "Plot generated successfully."
  ),
  es = list(
    lang        = "es",
    title       = "climasus4r \u2014 Visualizaci\u00f3n del Agregado Clima-Salud",
    n_rows      = "Filas",
    outcome_lbl = "Desenlace",
    climate_lbl = "Clima",
    date_lbl    = "Fecha",
    lag_lbl     = "Rezago (d\u00edas)",
    corr_lbl    = "Correlaci\u00f3n",
    sig_lbl     = "Significativo",
    density_lbl = "Densidad",
    variable_lbl = "Variable",
    month_lbl   = "Mes",
    spearman_lbl = "Spearman",
    ts_title    = "Serie Temporal: Clima y Salud",
    sc_title    = "Dispersi\u00f3n: Clima vs Desenlace",
    ccf_title   = "Funci\u00f3n de Correlaci\u00f3n Cruzada",
    dist_title  = "Distribuci\u00f3n de Variables Clim\u00e1ticas",
    corr_title  = "Matriz de Correlaci\u00f3n de Spearman",
    corr_subtitle = "Correlaciones entre rezagos clim\u00e1ticos y desenlace",
    seas_title  = "Patr\u00f3n Estacional",
    done        = "Gr\u00e1fico generado correctamente."
  )
)
