# =============================================================================
# sus_data_plot_aggregate_ts.R
# Publication-quality time-series visualisation of aggregated DATASUS outcomes
#
# Types:
#   "epidemic"  -- Area + smooth curve over time
#   "seasonal"  -- Monthly boxplot distribution
#   "heatmap"   -- Calendar tile (year x month)
#   "trend"     -- Annual bar chart with YoY % change
# Palettes: ggsci (lancet, nejm, jco, uchicago) + viridis fallback
# Style: The Lancet / Nature Medicine  PT / EN / ES
# =============================================================================

# -- NSE variable declarations (suppresses R CMD CHECK warnings) --------------
utils::globalVariables(c(
  # epidemic
  ".date", ".val", ".y", ".label",
  # seasonal
  ".month_num", ".month_lbl", ".val_s",
  # heatmap
  "year_val", "month_num", ".fill_val",
  # trend
  ".year_val", ".annual_total", ".pct_change", ".label_pct",
  # shared
  "date"
))

# -- Local i18n ---------------------------------------------------------------
.ts_msgs <- list(

  title = list(
    pt = "S\u00E9rie Temporal: Desfechos de Sa\u00FAde",
    en = "Health Outcomes Time Series",
    es = "Serie Temporal: Resultados de Salud"
  ),
  detecting_col = list(
    pt = "Detectando coluna de desfecho...",
    en = "Detecting outcome column...",
    es = "Detectando columna de desenlace..."
  ),
  col_detected = list(
    pt = "Coluna detectada: {col}",
    en = "Column detected: {col}",
    es = "Columna detectada: {col}"
  ),
  no_col = list(
    pt = "Nenhuma coluna de desfecho encontrada. Use {.arg value_col}.",
    en = "No outcome column found. Use {.arg value_col}.",
    es = "No se encontr\u00F3 columna de desenlace. Use {.arg value_col}."
  ),
  wrong_stage = list(
    pt = "Dados devem estar no stage 'aggregate' (ou posterior). Stage atual: {stage}.",
    en = "Data must be at stage 'aggregate' (or later). Current stage: {stage}.",
    es = "Los datos deben estar en stage 'aggregate' (o posterior). Stage actual: {stage}."
  ),
  no_date = list(
    pt = "Coluna de data n\u00E3o encontrada. Esperada: 'date' ou 'data'.",
    en = "Date column not found. Expected: 'date' or 'data'.",
    es = "Columna de fecha no encontrada. Esperada: 'date' o 'data'."
  ),
  warn_lang = list(
    pt = "Idioma '{lang}' n\u00E3o suportado. Usando 'pt'.",
    en = "Language '{lang}' not supported. Using 'pt'.",
    es = "Idioma '{lang}' no admitido. Usando 'pt'."
  ),
  done = list(
    pt = "Gr\u00E1fico gerado: {n} linhas, tipo '{type}'.",
    en = "Plot generated: {n} rows, type '{type}'.",
    es = "Gr\u00E1fico generado: {n} filas, tipo '{type}'."
  ),
  materialise = list(
    pt = "Materializando dataset Arrow...",
    en = "Materialising Arrow dataset...",
    es = "Materializando dataset Arrow..."
  ),
  # axis / label helpers
  y_label = list(
    pt = "Contagem",
    en = "Count",
    es = "Conteo"
  ),
  x_year   = list(pt = "Ano",   en = "Year",  es = "A\u00F1o"),
  x_month  = list(pt = "M\u00EAs", en = "Month", es = "Mes"),
  log_note = list(
    pt = " (escala log\u2081\u208A\u2093)",
    en = " (log\u2081\u208A\u2093 scale)",
    es = " (escala log\u2081\u208A\u2093)"
  ),
  month_abbr = list(
    pt = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
           "Jul", "Ago", "Set", "Out", "Nov", "Dez"),
    en = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
    es = c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
           "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  ),
  fill_label = list(
    pt = "Valor m\u00E9dio",
    en = "Mean value",
    es = "Valor medio"
  )
)

# helper to retrieve a message key for a given language
.tsm <- function(key, lang = "pt", ...) {
  entry <- .ts_msgs[[key]]
  if (is.null(entry)) return(key)
  txt <- entry[[lang]] %||% entry[["pt"]]
  if (...length() > 0L) glue::glue(txt, .envir = rlang::env(...)) else txt
}

# =============================================================================
# -- PALETTE HELPER ------------------------------------------------------------
# =============================================================================

#' @keywords internal
#' @noRd
.ts_palette <- function(palette, n = 8L) {
  if (requireNamespace("ggsci", quietly = TRUE)) {
    cols <- switch(
      palette,
      lancet   = ggsci::pal_lancet()(min(n, 9L)),
      nejm     = ggsci::pal_nejm()(min(n, 8L)),
      jco      = ggsci::pal_jco()(min(n, 10L)),
      uchicago = ggsci::pal_uchicago()(min(n, 9L)),
      grDevices::colorRampPalette(c("#185FA5", "#D85A30", "#1D9E75"))(n)
    )
  } else {
    cols <- grDevices::colorRampPalette(
      c("#185FA5", "#D85A30", "#1D9E75", "#9467BD", "#8C564B")
    )(n)
  }
  if (length(cols) < n) {
    cols <- grDevices::colorRampPalette(cols)(n)
  }
  cols
}

# =============================================================================
# -- THEME HELPER --------------------------------------------------------------
# =============================================================================

#' @keywords internal
#' @noRd
.ts_theme <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      panel.grid.minor       = ggplot2::element_blank(),
      panel.grid.major.x     = ggplot2::element_line(colour = "grey92", linewidth = 0.25),
      panel.grid.major.y     = ggplot2::element_line(colour = "grey88", linewidth = 0.30),
      panel.border           = ggplot2::element_rect(colour = "grey60", fill = NA,
                                                     linewidth = 0.35),
      strip.background       = ggplot2::element_rect(fill = "#1C2B3A"),
      strip.text             = ggplot2::element_text(colour = "white", face = "bold",
                                                     size = base_size * 0.85),
      axis.text.x            = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position        = "bottom",
      plot.title             = ggplot2::element_text(face = "bold",
                                                     size = base_size * 1.1),
      plot.subtitle          = ggplot2::element_text(colour = "grey35",
                                                     size = base_size * 0.90),
      plot.caption           = ggplot2::element_text(colour = "grey50",
                                                     size = base_size * 0.72,
                                                     hjust = 0),
      plot.margin            = ggplot2::margin(12, 14, 10, 12)
    )
}

# =============================================================================
# -- STAGE ORDER HELPER --------------------------------------------------------
# =============================================================================

#' @keywords internal
#' @noRd
.ts_stage_ok <- function(stage) {
  valid_later <- c("aggregate", "spatial", "climate", "census")
  isTRUE(stage %in% valid_later)
}

# =============================================================================
# -- PLOT ENGINE: EPIDEMIC -----------------------------------------------------
# =============================================================================

#' @keywords internal
#' @noRd
.ts_epidemic <- function(df, value_col, group_col, facet_col, facet_ncol,
                         smooth_method, smooth_span, log_transform,
                         free_scales, pal, title, date_labels, year_breaks,
                         base_size, lang) {

  # Build working data
  df_e <- df |>
    dplyr::mutate(.date = as.Date(date), .val = .data[[value_col]])

  if (log_transform) {
    df_e <- df_e |> dplyr::mutate(.y = log1p(.val))
    y_lab <- paste0(.tsm("y_label", lang), .tsm("log_note", lang))
    y_trans_fn  <- log1p
    y_inv_fn    <- expm1
    y_scale <- ggplot2::scale_y_continuous(
      name   = y_lab,
      labels = function(x) scales::comma(round(expm1(x), 0))
    )
  } else {
    df_e <- df_e |> dplyr::mutate(.y = .val)
    y_lab  <- .tsm("y_label", lang)
    y_scale <- ggplot2::scale_y_continuous(
      name   = y_lab,
      labels = scales::comma,
      expand = ggplot2::expansion(mult = c(0.01, 0.12))
    )
  }

  # Smooth method
  sm_method  <- switch(smooth_method,
    loess = "loess",
    gam   = "gam",
    lm    = "lm",
    "loess"
  )
  sm_formula <- if (smooth_method == "gam") {
    rlang::check_installed("mgcv", reason = "to use smooth_method = 'gam'")
    y ~ s(x, bs = "tp")
  } else {
    y ~ x
  }

  primary_col   <- pal[[1L]]
  secondary_col <- pal[[6L]] %||% pal[[length(pal)]]

  # Base mapping
  if (!is.null(group_col) && group_col %in% names(df_e)) {
    p <- ggplot2::ggplot(df_e, ggplot2::aes(
      x     = .date,
      y     = .y,
      color = .data[[group_col]],
      fill  = .data[[group_col]]
    ))
  } else {
    p <- ggplot2::ggplot(df_e, ggplot2::aes(x = .date, y = .y))
  }

  p <- p +
    ggplot2::geom_area(alpha = 0.25, fill = primary_col, na.rm = TRUE) +
    ggplot2::geom_line(linewidth = 0.8, color = primary_col, na.rm = TRUE) +
    ggplot2::geom_smooth(
      method  = sm_method,
      formula = sm_formula,
      span    = smooth_span,
      se      = TRUE,
      color   = secondary_col,
      fill    = secondary_col,
      alpha   = 0.18,
      linewidth = 1.0,
      na.rm   = TRUE
    ) +
    ggplot2::scale_x_date(
      date_breaks = year_breaks,
      date_labels = date_labels,
      expand      = ggplot2::expansion(mult = c(0.01, 0.02))
    ) +
    y_scale +
    ggplot2::labs(
      title    = title %||% .tsm("title", lang),
      x        = NULL,
      color    = group_col,
      fill     = group_col
    ) +
    .ts_theme(base_size)

  if (!is.null(group_col) && group_col %in% names(df_e)) {
    n_grps <- dplyr::n_distinct(df_e[[group_col]])
    p <- p +
      ggplot2::scale_color_manual(values = .ts_palette("lancet", n_grps)) +
      ggplot2::scale_fill_manual(values = .ts_palette("lancet", n_grps))
  }

  if (!is.null(facet_col) && facet_col %in% names(df_e)) {
    p <- p + ggplot2::facet_wrap(
      stats::as.formula(paste0("~ ", facet_col)),
      ncol   = facet_ncol,
      scales = if (free_scales) "free_y" else "fixed"
    )
  }

  p
}

# =============================================================================
# -- PLOT ENGINE: SEASONAL -----------------------------------------------------
# =============================================================================

#' @keywords internal
#' @noRd
.ts_seasonal <- function(df, value_col, group_col, facet_col, facet_ncol,
                         log_transform, free_scales, pal, title, base_size, lang) {

  month_abbr <- .ts_msgs$month_abbr[[lang]] %||% .ts_msgs$month_abbr[["pt"]]

  df_s <- df |>
    dplyr::mutate(
      .date      = as.Date(date),
      .month_num = lubridate::month(.date),
      .month_lbl = factor(month_abbr[.month_num], levels = month_abbr),
      .val_s     = if (log_transform) log1p(.data[[value_col]]) else .data[[value_col]]
    )

  y_lab <- if (log_transform) {
    paste0(.tsm("y_label", lang), .tsm("log_note", lang))
  } else {
    .tsm("y_label", lang)
  }

  primary_col   <- pal[[1L]]
  secondary_col <- pal[[6L]] %||% pal[[length(pal)]]

  fill_aes <- if (!is.null(group_col) && group_col %in% names(df_s)) {
    ggplot2::aes(x = .month_lbl, y = .val_s,
                 fill = .data[[group_col]])
  } else {
    ggplot2::aes(x = .month_lbl, y = .val_s,
                 fill = .month_lbl)
  }

  p <- ggplot2::ggplot(df_s, fill_aes) +
    ggplot2::geom_boxplot(
      alpha         = 0.60,
      outlier.size  = 1.0,
      outlier.alpha = 0.50,
      linewidth     = 0.35,
      fatten        = 1.5,
      width         = 0.65,
      color         = "grey30"
    ) +
    ggplot2::stat_summary(
      fun     = stats::median,
      geom    = "line",
      mapping = ggplot2::aes(group = 1),
      color   = secondary_col,
      linewidth = 0.90,
      alpha   = 0.85
    ) +
    ggplot2::stat_summary(
      fun     = stats::median,
      geom    = "point",
      color   = secondary_col,
      size    = 1.8,
      shape   = 21,
      fill    = "white",
      stroke  = 1.2
    ) +
    ggplot2::scale_y_continuous(
      name   = y_lab,
      labels = if (log_transform) {
        function(x) scales::comma(round(expm1(x), 0))
      } else {
        scales::comma
      }
    ) +
    ggplot2::scale_x_discrete(
      name = .tsm("x_month", lang)
    ) +
    ggplot2::labs(
      title = title %||% .tsm("title", lang),
      x     = .tsm("x_month", lang),
      fill  = group_col %||% NULL
    ) +
    .ts_theme(base_size) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5))

  if (is.null(group_col) || !group_col %in% names(df_s)) {
    p <- p +
      ggplot2::scale_fill_manual(
        values = grDevices::colorRampPalette(pal)(12L),
        guide  = "none"
      )
  } else {
    n_grps <- dplyr::n_distinct(df_s[[group_col]])
    p <- p +
      ggplot2::scale_fill_manual(values = .ts_palette("lancet", n_grps))
  }

  if (!is.null(facet_col) && facet_col %in% names(df_s)) {
    p <- p + ggplot2::facet_wrap(
      stats::as.formula(paste0("~ ", facet_col)),
      ncol   = facet_ncol,
      scales = if (free_scales) "free_y" else "fixed"
    )
  }

  p
}

# =============================================================================
# -- PLOT ENGINE: HEATMAP ------------------------------------------------------
# =============================================================================

#' @keywords internal
#' @noRd
.ts_heatmap <- function(df, value_col, facet_col, facet_ncol,
                        log_transform, free_scales, pal, title, base_size, lang) {

  month_abbr <- .ts_msgs$month_abbr[[lang]] %||% .ts_msgs$month_abbr[["pt"]]

  df_h <- df |>
    dplyr::mutate(
      .date   = as.Date(date),
      year_val  = lubridate::year(.date),
      month_num = lubridate::month(.date)
    ) |>
    dplyr::group_by(year_val, month_num) |>
    dplyr::summarise(
      .fill_val = mean(.data[[value_col]], na.rm = TRUE),
      .groups   = "drop"
    )

  if (log_transform) {
    df_h <- df_h |> dplyr::mutate(.fill_val = log1p(.fill_val))
  }

  # Build heatmap palette (blue-gradient style)
  hm_pal <- grDevices::colorRampPalette(
    c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B")
  )(9L)

  fill_lab <- if (log_transform) {
    paste0(.tsm("fill_label", lang), "\n(log\u2081\u208A\u2093)")
  } else {
    .tsm("fill_label", lang)
  }

  p <- ggplot2::ggplot(
    df_h,
    ggplot2::aes(
      x    = month_num,
      y    = factor(year_val),
      fill = .fill_val
    )
  ) +
    ggplot2::geom_tile(color = "white", linewidth = 0.4, na.rm = TRUE) +
    ggplot2::scale_fill_gradientn(
      colors   = hm_pal,
      na.value = "grey95",
      name     = fill_lab,
      labels   = if (log_transform) {
        function(x) scales::comma(round(expm1(x), 0))
      } else {
        scales::comma
      },
      guide = ggplot2::guide_colorbar(
        title.position = "top",
        barwidth       = ggplot2::unit(12, "lines"),
        barheight      = ggplot2::unit(0.55, "lines"),
        ticks.linewidth = 0.4
      )
    ) +
    ggplot2::scale_x_continuous(
      breaks = 1:12,
      labels = month_abbr,
      expand = c(0, 0),
      name   = .tsm("x_month", lang)
    ) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::labs(
      title = title %||% .tsm("title", lang),
      y     = .tsm("x_year", lang)
    ) +
    .ts_theme(base_size) +
    ggplot2::theme(
      legend.position   = "bottom",
      legend.direction  = "horizontal",
      axis.text.x       = ggplot2::element_text(angle = 0, hjust = 0.5,
                                                 size = base_size * 0.78),
      axis.text.y       = ggplot2::element_text(size = base_size * 0.78),
      panel.grid        = ggplot2::element_blank()
    )

  if (!is.null(facet_col) && facet_col %in% names(df)) {
    # re-build with facet grouping
    df_h2 <- df |>
      dplyr::mutate(
        .date     = as.Date(date),
        year_val  = lubridate::year(.date),
        month_num = lubridate::month(.date)
      ) |>
      dplyr::group_by(year_val, month_num, .data[[facet_col]]) |>
      dplyr::summarise(
        .fill_val = mean(.data[[value_col]], na.rm = TRUE),
        .groups   = "drop"
      )

    if (log_transform) df_h2 <- df_h2 |> dplyr::mutate(.fill_val = log1p(.fill_val))

    p <- ggplot2::ggplot(
      df_h2,
      ggplot2::aes(x = month_num, y = factor(year_val), fill = .fill_val)
    ) +
      ggplot2::geom_tile(color = "white", linewidth = 0.4, na.rm = TRUE) +
      ggplot2::scale_fill_gradientn(
        colors   = hm_pal,
        na.value = "grey95",
        name     = fill_lab,
        labels   = if (log_transform) {
          function(x) scales::comma(round(expm1(x), 0))
        } else {
          scales::comma
        },
        guide = ggplot2::guide_colorbar(
          title.position = "top",
          barwidth       = ggplot2::unit(12, "lines"),
          barheight      = ggplot2::unit(0.55, "lines"),
          ticks.linewidth = 0.4
        )
      ) +
      ggplot2::scale_x_continuous(
        breaks = 1:12, labels = month_abbr, expand = c(0, 0),
        name   = .tsm("x_month", lang)
      ) +
      ggplot2::scale_y_discrete(expand = c(0, 0)) +
      ggplot2::labs(title = title %||% .tsm("title", lang), y = .tsm("x_year", lang)) +
      ggplot2::facet_wrap(
        stats::as.formula(paste0("~ ", facet_col)),
        ncol   = facet_ncol,
        scales = if (free_scales) "free_y" else "fixed"
      ) +
      .ts_theme(base_size) +
      ggplot2::theme(
        legend.position  = "bottom",
        legend.direction = "horizontal",
        axis.text.x      = ggplot2::element_text(angle = 0, hjust = 0.5,
                                                  size = base_size * 0.78),
        panel.grid       = ggplot2::element_blank()
      )
  }

  p
}

# =============================================================================
# -- PLOT ENGINE: TREND --------------------------------------------------------
# =============================================================================

#' @keywords internal
#' @noRd
.ts_trend <- function(df, value_col, facet_col, facet_ncol,
                      free_scales, pal, title, base_size, lang) {

  df_t <- df |>
    dplyr::mutate(
      .date     = as.Date(date),
      .year_val = lubridate::year(.date)
    ) |>
    dplyr::group_by(.year_val) |>
    dplyr::summarise(
      .annual_total = sum(.data[[value_col]], na.rm = TRUE),
      .groups       = "drop"
    ) |>
    dplyr::arrange(.year_val) |>
    dplyr::mutate(
      .pct_change = (.annual_total / dplyr::lag(.annual_total) - 1L) * 100,
      .label_pct  = dplyr::case_when(
        is.na(.pct_change) ~ "",
        .pct_change >  0   ~ paste0("+", round(.pct_change, 1L), "%"),
        TRUE               ~ paste0(round(.pct_change, 1L), "%")
      )
    )

  n_years     <- nrow(df_t)
  bar_colors  <- grDevices::colorRampPalette(pal)(n_years)
  primary_col <- pal[[1L]]

  p <- ggplot2::ggplot(
    df_t,
    ggplot2::aes(
      x    = factor(.year_val),
      y    = .annual_total,
      fill = factor(.year_val)
    )
  ) +
    ggplot2::geom_col(width = 0.72, alpha = 0.88, show.legend = FALSE) +
    ggplot2::geom_text(
      ggplot2::aes(
        y     = .annual_total,
        label = .label_pct,
        color = .pct_change > 0
      ),
      vjust    = -0.5,
      size     = base_size * 0.22,
      fontface = "bold",
      na.rm    = TRUE
    ) +
    ggplot2::scale_fill_manual(values = bar_colors, guide = "none") +
    ggplot2::scale_color_manual(
      values = c("FALSE" = "#2196F3", "TRUE" = "#C0392B"),
      guide  = "none"
    ) +
    ggplot2::scale_y_continuous(
      name   = .tsm("y_label", lang),
      labels = scales::comma,
      expand = ggplot2::expansion(mult = c(0, 0.15))
    ) +
    ggplot2::labs(
      title = title %||% .tsm("title", lang),
      x     = .tsm("x_year", lang)
    ) +
    .ts_theme(base_size) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x        = ggplot2::element_text(angle = 45, hjust = 1)
    )

  if (!is.null(facet_col) && facet_col %in% names(df)) {
    df_tf <- df |>
      dplyr::mutate(
        .date     = as.Date(date),
        .year_val = lubridate::year(.date)
      ) |>
      dplyr::group_by(.year_val, .data[[facet_col]]) |>
      dplyr::summarise(
        .annual_total = sum(.data[[value_col]], na.rm = TRUE),
        .groups       = "drop"
      ) |>
      dplyr::arrange(.data[[facet_col]], .year_val) |>
      dplyr::group_by(.data[[facet_col]]) |>
      dplyr::mutate(
        .pct_change = (.annual_total / dplyr::lag(.annual_total) - 1L) * 100,
        .label_pct  = dplyr::case_when(
          is.na(.pct_change) ~ "",
          .pct_change >  0   ~ paste0("+", round(.pct_change, 1L), "%"),
          TRUE               ~ paste0(round(.pct_change, 1L), "%")
        )
      ) |>
      dplyr::ungroup()

    p <- ggplot2::ggplot(
      df_tf,
      ggplot2::aes(
        x    = factor(.year_val),
        y    = .annual_total,
        fill = factor(.year_val)
      )
    ) +
      ggplot2::geom_col(width = 0.72, alpha = 0.88, show.legend = FALSE) +
      ggplot2::geom_text(
        ggplot2::aes(y = .annual_total, label = .label_pct, color = .pct_change > 0),
        vjust = -0.5, size = base_size * 0.22, fontface = "bold", na.rm = TRUE
      ) +
      ggplot2::scale_fill_manual(values = bar_colors, guide = "none") +
      ggplot2::scale_color_manual(
        values = c("FALSE" = "#2196F3", "TRUE" = "#C0392B"),
        guide  = "none"
      ) +
      ggplot2::scale_y_continuous(
        name   = .tsm("y_label", lang),
        labels = scales::comma,
        expand = ggplot2::expansion(mult = c(0, 0.15))
      ) +
      ggplot2::labs(
        title = title %||% .tsm("title", lang),
        x     = .tsm("x_year", lang)
      ) +
      ggplot2::facet_wrap(
        stats::as.formula(paste0("~ ", facet_col)),
        ncol   = facet_ncol,
        scales = if (free_scales) "free_y" else "fixed"
      ) +
      .ts_theme(base_size) +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        axis.text.x        = ggplot2::element_text(angle = 45, hjust = 1)
      )
  }

  p
}

# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' Time-Series Plot of Aggregated DATASUS Health Outcomes
#'
#' Produces publication-quality time-series visualisations (epidemic curve,
#' seasonal boxplot, calendar heatmap, or annual trend) from a `climasus_df`
#' dataset at stage `"aggregate"` or later. Colour palettes are powered by
#' [ggsci] (Lancet, NEJM, JCO, UChicago). An interactive [plotly] widget can
#' be returned with `interactive = TRUE`.
#'
#' @param df A `climasus_df` at stage `"aggregate"` or later (output of
#'   [sus_data_aggregate()]).
#' @param value_col Character. Name of the outcome column to plot. If `NULL`
#'   (default), the function auto-detects the first matching column from:
#'   `n_obitos`, `n_internacoes`, `n_nascimentos`, `n_casos`,
#'   `n_procedimentos`, `n_estabelecimentos`, and their English / Spanish
#'   equivalents.
#' @param group_col Character. Optional column used for colour-grouping in
#'   `"epidemic"` and `"seasonal"` plots.
#' @param facet_col Character. Optional column for `facet_wrap()` panels.
#' @param facet_ncol Integer. Number of facet columns. Default `3L`.
#' @param plot_type Character. One or more of `"epidemic"`, `"seasonal"`,
#'   `"heatmap"`, `"trend"`. When multiple types are supplied, plots are
#'   assembled with [patchwork] (one per row). Default: `"epidemic"`.
#' @param smooth_method Character. Smoothing method for the `"epidemic"` plot:
#'   `"loess"` (default), `"gam"`, or `"lm"`.
#' @param smooth_span Numeric. Span for LOESS smoothing. Default `0.25`.
#' @param log_transform Logical. Apply `log1p` transformation on the y-axis
#'   for `"epidemic"` and `"seasonal"` plots, and to the fill scale in
#'   `"heatmap"`. Default `FALSE`.
#' @param free_scales Logical. Allow free y-axis scales in faceted plots.
#'   Default `TRUE`.
#' @param palette Character. Colour palette. One of `"lancet"` (default),
#'   `"nejm"`, `"jco"`, `"uchicago"`, or `"viridis"`.
#' @param title Character. Plot title. Auto-generated if `NULL`.
#' @param date_labels Character. `strftime` format string for x-axis date
#'   labels. Default `"%b/%y"`.
#' @param year_breaks Character. `date_breaks` string for the x-axis. Default
#'   `"3 months"`.
#' @param base_size Numeric. Base font size for the ggplot2 theme. Default `11`.
#' @param interactive Logical. Return a [plotly::ggplotly()] widget instead of
#'   a static ggplot2 object. Default `FALSE`.
#' @param lang Character. Language for labels and messages: `"pt"` (default),
#'   `"en"`, or `"es"`.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return Invisibly returns the ggplot2 object (or plotly widget when
#'   `interactive = TRUE`). Side effect: prints the plot in the active graphics
#'   device.
#'
#' @section Supported systems:
#' Automatically adapts y-axis labels and titles to SINAN, SIM, SIH, SINASC,
#' SIA, and CNES via outcome column auto-detection.
#'
#' @examples
#' \dontrun{
#' # Epidemic curve, Portuguese labels
#' sus_data_plot_aggregate_ts(df_agg, lang = "pt")
#'
#' # Seasonal boxplot, log-transformed
#' sus_data_plot_aggregate_ts(
#'   df_agg,
#'   plot_type     = "seasonal",
#'   log_transform = TRUE,
#'   lang          = "en"
#' )
#'
#' # Calendar heatmap by group
#' sus_data_plot_aggregate_ts(
#'   df_agg,
#'   plot_type  = "heatmap",
#'   facet_col  = "municipio",
#'   facet_ncol = 2L,
#'   lang       = "pt"
#' )
#'
#' # Multi-panel: epidemic + trend
#' sus_data_plot_aggregate_ts(
#'   df_agg,
#'   plot_type = c("epidemic", "trend"),
#'   lang      = "en"
#' )
#'
#' # Interactive plotly widget
#' sus_data_plot_aggregate_ts(df_agg, interactive = TRUE)
#' }
#'
#' @export
#' @importFrom dplyr mutate group_by summarise arrange ungroup n_distinct
#'   case_when lag
#' @importFrom lubridate year month floor_date
#' @importFrom stats median as.formula
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning
#'   cli_abort
#' @importFrom rlang check_installed .data
#' @importFrom scales comma
sus_data_plot_aggregate_ts <- function(
    df,
    value_col     = NULL,
    group_col     = NULL,
    facet_col     = NULL,
    facet_ncol    = 3L,
    plot_type     = c("epidemic", "seasonal", "heatmap", "trend"),
    smooth_method = "loess",
    smooth_span   = 0.25,
    log_transform = FALSE,
    free_scales   = TRUE,
    palette       = "lancet",
    title         = NULL,
    date_labels   = "%b/%y",
    year_breaks   = "3 months",
    base_size     = 11,
    interactive   = FALSE,
    lang          = "pt",
    verbose       = TRUE
) {

  # -- 1. Dependency checks -----------------------------------------------------
  rlang::check_installed(
    c("ggplot2", "scales"),
    reason = "to run {.fn sus_data_plot_aggregate_ts}"
  )

  # -- 2. Validate lang ---------------------------------------------------------
  if (!lang %in% c("pt", "en", "es")) {
    cli::cli_alert_warning(
      .tsm("warn_lang", "pt", lang = lang)
    )
    lang <- "pt"
  }

  # -- 3. Materialise Arrow / lazy inputs ---------------------------------------
  if (inherits(df, c("arrow_dplyr_query", "Dataset", "ArrowTabular", "Table"))) {
    if (verbose) cli::cli_alert_info(.tsm("materialise", lang))
    meta_backup <- tryCatch(sus_meta(df), error = function(e) list())
    df <- dplyr::collect(df)
    if (length(meta_backup) > 0L) df <- new_climasus_df(df, meta_backup)
  }

  # -- 4. Validate data.frame ---------------------------------------------------
  if (!is.data.frame(df)) {
    cli::cli_abort(c(
      "Input {.arg df} must be a {.cls data.frame} or collectable Arrow object.",
      "i" = "Use {.fn sus_data_import} to create a {.cls climasus_df}."
    ))
  }
  if (nrow(df) == 0L) {
    cli::cli_abort("{.arg df} has 0 rows. Nothing to plot.")
  }

  # -- 5. Stage validation ------------------------------------------------------
  if (inherits(df, "climasus_df")) {
    current_stage <- sus_meta(df, "stage")
    if (!.ts_stage_ok(current_stage)) {
      stage <- current_stage %||% "unknown"
      cli::cli_alert_warning(.tsm("wrong_stage", lang, stage = stage))
    }
  }

  # -- 6. Validate plot_type ----------------------------------------------------
  plot_type    <- match.arg(plot_type, several.ok = TRUE)
  valid_types  <- c("epidemic", "seasonal", "heatmap", "trend")
  bad_types    <- setdiff(plot_type, valid_types)
  if (length(bad_types) > 0L) {
    cli::cli_abort(
      "{.arg plot_type} must be one or more of: {.val {valid_types}}. Unknown: {.val {bad_types}}"
    )
  }

  # -- 7. Validate smooth_method ------------------------------------------------
  valid_smooth <- c("loess", "gam", "lm", "none")
  if (!smooth_method %in% valid_smooth) {
    cli::cli_alert_warning(
      "{.arg smooth_method} '{smooth_method}' unknown. Using 'loess'."
    )
    smooth_method <- "loess"
  }

  # -- 8. Date column detection -------------------------------------------------
  date_candidates <- c("date", "data", "DT_NOTIFIC", "DTOBITO", "DT_INTER",
                       "DTNASC", "DT_COMPET", "dt_obito", "dt_inter",
                       "dt_notific", "dt_nasc", "dt_compet")
  date_col_found <- NULL
  for (cand in date_candidates) {
    if (cand %in% names(df)) {
      date_col_found <- cand
      break
    }
  }
  if (is.null(date_col_found)) {
    cli::cli_abort(.tsm("no_date", lang))
  }
  # normalise to a column named "date" for downstream engines
  if (date_col_found != "date") {
    df <- df |> dplyr::mutate(date = as.Date(.data[[date_col_found]]))
  } else {
    df <- df |> dplyr::mutate(date = as.Date(.data[["date"]]))
  }

  # -- 9. Outcome column auto-detection -----------------------------------------
  outcome_candidates <- c(
    "n_obitos", "n_internacoes", "n_nascimentos", "n_casos",
    "n_procedimentos", "n_estabelecimentos",
    "n_deaths", "n_hospitalizations", "n_births", "n_procedures",
    "n_establishments",
    "n_muertes", "n_hospitalizaciones", "n_nacimientos",
    "n_procedimientos", "n_establecimientos",
    "count", "n", "total"
  )

  if (!is.null(value_col)) {
    if (!value_col %in% names(df)) {
      cli::cli_abort(
        "{.arg value_col} '{value_col}' not found in {.arg df}."
      )
    }
  } else {
    if (verbose) cli::cli_alert_info(.tsm("detecting_col", lang))
    for (cand in outcome_candidates) {
      if (cand %in% names(df)) {
        value_col <- cand
        break
      }
    }
    if (is.null(value_col)) {
      cli::cli_abort(.tsm("no_col", lang))
    }
    if (verbose) {
      col <- value_col
      cli::cli_alert_success(.tsm("col_detected", lang, col = col))
    }
  }

  # -- 10. Validate group_col / facet_col ---------------------------------------
  if (!is.null(group_col) && !group_col %in% names(df)) {
    cli::cli_alert_warning(
      "{.arg group_col} '{group_col}' not found in {.arg df}. Ignoring."
    )
    group_col <- NULL
  }
  if (!is.null(facet_col) && !facet_col %in% names(df)) {
    cli::cli_alert_warning(
      "{.arg facet_col} '{facet_col}' not found in {.arg df}. Ignoring."
    )
    facet_col <- NULL
  }

  # -- 11. Build palette --------------------------------------------------------
  pal <- .ts_palette(palette, n = 8L)

  if (verbose) cli::cli_h1(.tsm("title", lang))

  # -- 12. Dispatch to plot engines ---------------------------------------------
  plots <- lapply(plot_type, function(tp) {
    switch(
      tp,
      epidemic = .ts_epidemic(
        df            = df,
        value_col     = value_col,
        group_col     = group_col,
        facet_col     = facet_col,
        facet_ncol    = facet_ncol,
        smooth_method = smooth_method,
        smooth_span   = smooth_span,
        log_transform = log_transform,
        free_scales   = free_scales,
        pal           = pal,
        title         = title,
        date_labels   = date_labels,
        year_breaks   = year_breaks,
        base_size     = base_size,
        lang          = lang
      ),
      seasonal = .ts_seasonal(
        df            = df,
        value_col     = value_col,
        group_col     = group_col,
        facet_col     = facet_col,
        facet_ncol    = facet_ncol,
        log_transform = log_transform,
        free_scales   = free_scales,
        pal           = pal,
        title         = title,
        base_size     = base_size,
        lang          = lang
      ),
      heatmap = .ts_heatmap(
        df            = df,
        value_col     = value_col,
        facet_col     = facet_col,
        facet_ncol    = facet_ncol,
        log_transform = log_transform,
        free_scales   = free_scales,
        pal           = pal,
        title         = title,
        base_size     = base_size,
        lang          = lang
      ),
      trend = .ts_trend(
        df         = df,
        value_col  = value_col,
        facet_col  = facet_col,
        facet_ncol = facet_ncol,
        free_scales = free_scales,
        pal        = pal,
        title      = title,
        base_size  = base_size,
        lang       = lang
      )
    )
  })

  # -- 13. Assemble output ------------------------------------------------------
  if (length(plots) == 1L) {
    out <- plots[[1L]]
  } else {
    rlang::check_installed("patchwork", reason = "to combine multiple plot types")
    out <- patchwork::wrap_plots(plots, ncol = 1L)
  }

  # -- 14. Interactive conversion -----------------------------------------------
  if (interactive) {
    rlang::check_installed("plotly", reason = "to use {.arg interactive = TRUE}")
    out <- plotly::ggplotly(out)
  }

  # -- 15. Done message ---------------------------------------------------------
  if (verbose) {
    n    <- nrow(df)
    type <- paste(plot_type, collapse = "+")
    cli::cli_alert_success(.tsm("done", lang, n = n, type = type))
  }

  print(out)
  invisible(out)
}
