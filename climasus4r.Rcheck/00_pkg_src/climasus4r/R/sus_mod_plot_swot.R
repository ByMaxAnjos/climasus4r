# =============================================================================
# sus_mod_plot_swot.R
# Plots for climasus_swot objects
#
# Types:
#   "radar"  — Spider/radar chart of S/W/O/T scores per entity
#   "matrix" — Classic 2×2 SWOT board with top indicators per quadrant
#   "bar"    — Grouped horizontal bars of quadrant scores per entity
# =============================================================================

utils::globalVariables(c(
  "entity", "quadrant", "score", "quadrant_label",
  "x", "y", "r", "group", "xmin", "xmax", "ymin", "ymax"
))

# ── Local i18n ────────────────────────────────────────────────────────────────
.swot_plot_labels <- list(

  radar_title = list(
    pt = "Perfil SWOT Clim\u00e1tico-Sa\u00fade",
    en = "Climate-Health SWOT Profile",
    es = "Perfil SWOT Clima-Salud"
  ),
  matrix_title = list(
    pt = "An\u00e1lise SWOT Clim\u00e1tico-Sa\u00fade",
    en = "Climate-Health SWOT Analysis",
    es = "An\u00e1lisis SWOT Clima-Salud"
  ),
  bar_title = list(
    pt = "Pontua\u00e7\u00f5es SWOT por Entidade",
    en = "SWOT Scores by Entity",
    es = "Puntuaciones SWOT por Entidad"
  ),
  quadrant_S = list(pt = "For\u00e7as",       en = "Strengths",     es = "Fortalezas"),
  quadrant_W = list(pt = "Fraquezas",    en = "Weaknesses",    es = "Debilidades"),
  quadrant_O = list(pt = "Oportunidades",en = "Opportunities", es = "Oportunidades"),
  quadrant_T = list(pt = "Amea\u00e7as",      en = "Threats",       es = "Amenazas"),

  x_score   = list(pt = "Pontua\u00e7\u00e3o (0\u2013100)", en = "Score (0\u2013100)", es = "Puntuaci\u00f3n (0\u2013100)"),
  y_entity  = list(pt = "Entidade",          en = "Entity",        es = "Entidad"),

  top_inds = list(
    pt = "Principais indicadores:",
    en = "Top indicators:",
    es = "Principales indicadores:"
  ),
  n_inds = list(
    pt = "({n} indicador(es))",
    en = "({n} indicator(s))",
    es = "({n} indicador(es))"
  ),
  no_data = list(
    pt = "Sem dados",
    en = "No data",
    es = "Sin datos"
  ),

  err_not_swot = list(
    pt = "{.arg x} deve ser um {.cls climasus_swot} de {.fn sus_mod_swot}.",
    en = "{.arg x} must be a {.cls climasus_swot} from {.fn sus_mod_swot}.",
    es = "{.arg x} debe ser un {.cls climasus_swot} de {.fn sus_mod_swot}."
  ),
  warn_matrix_multi = list(
    pt = "type='matrix' mostra apenas uma entidade. Exibindo: {ent}. Use type='bar' para m\u00faltiplas.",
    en = "type='matrix' shows one entity only. Showing: {ent}. Use type='bar' for multiple.",
    es = "type='matrix' muestra solo una entidad. Mostrando: {ent}. Use type='bar' para m\u00faltiples."
  ),
  warn_lang = list(
    pt = "Idioma '{lang}' n\u00e3o suportado. Usando 'pt'.",
    en = "Language '{lang}' not supported. Using 'pt'.",
    es = "Idioma '{lang}' no admitido. Usando 'pt'."
  )
)

#' @keywords internal
#' @noRd
.swpl <- function(key, lang, ...) {
  entry <- .swot_plot_labels[[key]]
  if (is.null(entry)) return(key)
  msg <- entry[[lang]] %||% entry[["pt"]]
  if (...length() > 0L) msg <- glue::glue(msg, .envir = rlang::env(...))
  msg
}


# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' Plots and Tables from a Climate-Health SWOT Analysis
#'
#' Produces radar (spider), matrix (2×2 SWOT board), and bar charts from a
#' `climasus_swot` object returned by [sus_mod_swot()].
#'
#' @section Plot types (`type`):
#' | `type` | Description |
#' |--------|-------------|
#' | `"radar"` | Spider/radar chart of the four quadrant scores (0–100) per entity |
#' | `"matrix"` | Classic 2×2 SWOT board: score, category label, and top indicators for each quadrant. Best for a single entity. |
#' | `"bar"` | Grouped horizontal bars comparing all four quadrant scores per entity |
#'
#' @param x A `climasus_swot` object from [sus_mod_swot()].
#' @param type Character. Plot type: `"radar"` (default), `"matrix"`, or
#'   `"bar"`.
#' @param output_type Character. `"plot"` (default), `"table"`, or `"all"`
#'   (named list `$plot`, `$table`).
#' @param interactive Logical. Convert the ggplot2 output to an interactive
#'   \pkg{plotly} widget. Default `FALSE`.
#' @param entities Character vector or `NULL`. Subset of entity names to plot.
#'   `NULL` (default) plots all entities. For `type = "matrix"`, only the first
#'   entity in the selection is shown.
#' @param top_n Integer. For `type = "matrix"`, maximum number of indicator
#'   bullets to display per quadrant. Default `3L`.
#' @param base_size Numeric. ggplot2 base font size. Default `12`.
#' @param save_plot Character or `NULL`. File path to save the plot (PNG/PDF).
#' @param lang Character. Language for labels: `"pt"` (default), `"en"`, `"es"`.
#' @param verbose Logical. Print progress messages. Default `FALSE`.
#'
#' @return Depending on `output_type`:
#'   - `"plot"` → a `ggplot` or `plotly` object.
#'   - `"table"` → a `tibble` of the plotted data.
#'   - `"all"` → a named list `$plot`, `$table`.
#'
#' @examples
#' \dontrun{
#' swot <- sus_mod_swot(vulnerability = vi, af = af_res, lang = "pt")
#'
#' sus_mod_plot_swot(swot, type = "matrix", lang = "pt")
#' sus_mod_plot_swot(swot, type = "radar",  lang = "en")
#' sus_mod_plot_swot(swot, type = "bar",    interactive = TRUE)
#' out <- sus_mod_plot_swot(swot, output_type = "all")
#' }
#'
#' @seealso [sus_mod_swot()], [sus_mod_plot_vulnerability()]
#'
#' @export
#' @importFrom cli cli_h1 cli_alert_success cli_alert_warning cli_abort
#' @importFrom rlang check_installed
#' @importFrom glue glue
sus_mod_plot_swot <- function(
    x,
    type        = c("radar", "matrix", "bar"),
    output_type = c("plot", "table", "all"),
    interactive = FALSE,
    entities    = NULL,
    top_n       = 3L,
    base_size   = 12L,
    save_plot   = NULL,
    lang        = c("pt", "en", "es"),
    verbose     = FALSE
) {
  type        <- match.arg(type)
  output_type <- match.arg(output_type)
  lang <- if (is.character(lang) && length(lang) == 1L) {
    if (!lang %in% c("pt", "en", "es")) {
      cli::cli_alert_warning(glue::glue(.swpl("warn_lang", "pt")))
      "pt"
    } else lang
  } else match.arg(lang)

  if (!inherits(x, "climasus_swot"))
    cli::cli_abort(.swpl("err_not_swot", lang))

  rlang::check_installed("ggplot2",
                         reason = "to create plots with sus_mod_plot_swot()")
  if (verbose) cli::cli_h1("climasus4r \u2014 SWOT Plot")

  # -- Build plot and table ---------------------------------------------------
  if (type == "radar") {
    p   <- .swot_plot_radar(x, lang, base_size, entities)
    tbl <- .swot_long_scores(x, lang, entities)
  } else if (type == "matrix") {
    p   <- .swot_plot_matrix(x, lang, base_size, entities, top_n)
    tbl <- x$scores
    if (!is.null(entities)) tbl <- tbl[tbl$entity %in% entities, ]
  } else {
    p   <- .swot_plot_bar(x, lang, base_size, entities)
    tbl <- .swot_long_scores(x, lang, entities)
  }

  # -- Interactive / save -----------------------------------------------------
  if (interactive) {
    rlang::check_installed("plotly", reason = "for interactive SWOT plots")
    p <- plotly::ggplotly(p)
  }
  if (!is.null(save_plot)) {
    if (interactive && inherits(p, "plotly")) {
      rlang::check_installed("htmlwidgets",
                             reason = "to save interactive plots as HTML")
      htmlwidgets::saveWidget(p, save_plot, selfcontained = TRUE)
    } else {
      ggplot2::ggsave(save_plot, plot = p, width = 9, height = 6)
    }
    if (verbose) cli::cli_alert_success("Plot salvo em {.file {save_plot}}")
  }

  if (output_type == "plot")  return(p)
  if (output_type == "table") return(tbl)
  list(plot = p, table = tbl)
}


# =============================================================================
# INTERNAL BUILDERS
# =============================================================================

# Quadrant colours used across all plot types
.swot_colors <- c(S = "#22c55e", W = "#f97316", O = "#3b82f6", T = "#ef4444")


#' Reshape scores to long format for radar and bar charts
#' @keywords internal
#' @noRd
.swot_long_scores <- function(x, lang, entities_sel) {
  scores <- x$scores
  if (!is.null(entities_sel))
    scores <- scores[scores$entity %in% entities_sel, ]

  quad_labels <- c(
    S = .swpl("quadrant_S", lang),
    W = .swpl("quadrant_W", lang),
    O = .swpl("quadrant_O", lang),
    T = .swpl("quadrant_T", lang)
  )

  rows <- lapply(c("S", "W", "O", "T"), function(q) {
    score_col <- paste0(q, "_score")
    dplyr::tibble(
      entity         = scores$entity,
      quadrant       = q,
      quadrant_label = unname(quad_labels[q]),
      score          = scores[[score_col]]
    )
  })
  dplyr::bind_rows(rows)
}


# ── Radar chart ───────────────────────────────────────────────────────────────

#' @keywords internal
#' @noRd
.swot_plot_radar <- function(x, lang, base_size, entities_sel) {
  long <- .swot_long_scores(x, lang, entities_sel)
  long$score[is.na(long$score)] <- 0

  # Four axes at 90°, 0°, 270°, 180° (S top, O right, T bottom, W left)
  quad_order  <- c("S", "O", "T", "W")
  quad_angles <- c(S = 90, O = 0, T = 270, W = 180) * pi / 180

  quad_labels_vec <- c(
    S = .swpl("quadrant_S", lang),
    O = .swpl("quadrant_O", lang),
    T = .swpl("quadrant_T", lang),
    W = .swpl("quadrant_W", lang)
  )

  all_entities <- unique(long$entity)

  # Build polygon data (score polygon per entity — closed)
  poly_rows <- lapply(all_entities, function(ent) {
    ent_data <- long[long$entity == ent, ]
    pts <- lapply(quad_order, function(q) {
      r     <- (ent_data$score[ent_data$quadrant == q] %||% 0) / 100
      theta <- quad_angles[[q]]
      data.frame(entity = ent, quadrant = q,
                 r = r, x = r * cos(theta), y = r * sin(theta),
                 stringsAsFactors = FALSE)
    })
    pts_df <- do.call(rbind, pts)
    # Close polygon
    rbind(pts_df, pts_df[1L, ])
  })
  poly_df <- do.call(rbind, poly_rows)

  # Grid rings at 33 / 66 / 100 (fractional radius)
  ring_vals <- c(0.33, 0.66, 1.0)
  theta_seq <- seq(0, 2 * pi, length.out = 200)
  grid_rows <- lapply(ring_vals, function(rv) {
    data.frame(r = rv, x = rv * cos(theta_seq),
               y = rv * sin(theta_seq), stringsAsFactors = FALSE)
  })
  grid_df <- do.call(rbind, grid_rows)

  # Axis lines
  axis_rows <- lapply(quad_order, function(q) {
    theta <- quad_angles[[q]]
    data.frame(quadrant = q, x = c(0, cos(theta)), y = c(0, sin(theta)),
               stringsAsFactors = FALSE)
  })
  axis_df <- do.call(rbind, axis_rows)

  # Axis label positions (just beyond ring = 1)
  label_scale <- 1.22
  axis_labels <- data.frame(
    label = unname(quad_labels_vec),
    x     = label_scale * cos(unname(quad_angles)),
    y     = label_scale * sin(unname(quad_angles)),
    stringsAsFactors = FALSE
  )

  n_ents <- length(all_entities)
  fill_colors <- if (n_ents == 1L) {
    stats::setNames(.swot_colors["S"], all_entities)
  } else {
    pal <- grDevices::colorRampPalette(c("#10b981", "#3b82f6", "#f97316",
                                          "#8b5cf6", "#ef4444"))(n_ents)
    stats::setNames(pal, all_entities)
  }

  ggplot2::ggplot() +
    # Grid rings
    ggplot2::geom_path(data = grid_df,
                       ggplot2::aes(x = x, y = y, group = r),
                       color = "gray80", linetype = "dashed",
                       linewidth = 0.35) +
    # Axis spokes
    ggplot2::geom_line(data = axis_df,
                       ggplot2::aes(x = x, y = y, group = quadrant),
                       color = "gray60", linewidth = 0.5) +
    # Score polygons (filled)
    ggplot2::geom_polygon(data = poly_df,
                          ggplot2::aes(x = x, y = y,
                                       group = entity, fill = entity),
                          alpha = 0.20) +
    ggplot2::geom_path(data = poly_df,
                       ggplot2::aes(x = x, y = y,
                                    group = entity, color = entity),
                       linewidth = 0.9) +
    ggplot2::geom_point(data = poly_df[!duplicated(paste(poly_df$entity,
                                                          poly_df$quadrant)), ],
                        ggplot2::aes(x = x, y = y, color = entity),
                        size = 2.5) +
    # Axis labels
    ggplot2::geom_text(data = axis_labels,
                       ggplot2::aes(x = x, y = y, label = label),
                       size = base_size * 0.30, fontface = "bold",
                       hjust = 0.5, vjust = 0.5) +
    # Ring tick labels (only for first entity to avoid clutter)
    ggplot2::annotate("text",
                      x = c(0.33, 0.66, 1.0) * cos(0) + 0.02,
                      y = c(0.33, 0.66, 1.0) * sin(0),
                      label = c("33", "66", "100"),
                      size = base_size * 0.22, color = "gray50", hjust = 0) +
    ggplot2::scale_fill_manual(values = fill_colors, name = NULL) +
    ggplot2::scale_color_manual(values = fill_colors, name = NULL) +
    ggplot2::coord_fixed(xlim = c(-1.35, 1.35), ylim = c(-1.35, 1.35)) +
    ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", hjust = 0.5),
      legend.position = if (n_ents > 1L) "bottom" else "none"
    ) +
    ggplot2::labs(title = .swpl("radar_title", lang))
}


# ── 2×2 Matrix board ─────────────────────────────────────────────────────────

#' @keywords internal
#' @noRd
.swot_plot_matrix <- function(x, lang, base_size, entities_sel, top_n) {
  scores <- x$scores
  if (!is.null(entities_sel))
    scores <- scores[scores$entity %in% entities_sel, ]

  if (nrow(scores) > 1L) {
    ent_show <- scores$entity[[1L]]
    cli::cli_alert_warning(
      glue::glue(.swpl("warn_matrix_multi", "pt"), ent = ent_show)
    )
    scores <- scores[1L, ]
  }

  ent <- scores$entity[[1L]]

  # Quadrant layout (row, col):  S top-left, W top-right, O bottom-left, T bot-right
  quads <- list(
    S = list(q = "S", xmn = 0,   xmx = 0.5, ymn = 0.5, ymx = 1,   color = .swot_colors["S"]),
    W = list(q = "W", xmn = 0.5, xmx = 1,   ymn = 0.5, ymx = 1,   color = .swot_colors["W"]),
    O = list(q = "O", xmn = 0,   xmx = 0.5, ymn = 0,   ymx = 0.5, color = .swot_colors["O"]),
    T = list(q = "T", xmn = 0.5, xmx = 1,   ymn = 0,   ymx = 0.5, color = .swot_colors["T"])
  )

  p <- ggplot2::ggplot() +
    ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) +
    ggplot2::theme_void(base_size = base_size) +
    ggplot2::labs(
      title    = .swpl("matrix_title", lang),
      subtitle = ent
    ) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", hjust = 0.5,
                                             size = base_size * 1.1),
      plot.subtitle = ggplot2::element_text(color = "gray40", hjust = 0.5)
    )

  # Cross lines
  p <- p +
    ggplot2::annotate("segment", x = 0, xend = 1, y = 0.5, yend = 0.5,
                      color = "gray30", linewidth = 1.2) +
    ggplot2::annotate("segment", x = 0.5, xend = 0.5, y = 0, yend = 1,
                      color = "gray30", linewidth = 1.2)

  for (qd in quads) {
    q_lbl   <- .swpl(paste0("quadrant_", qd$q), lang)
    sc_val  <- scores[[paste0(qd$q, "_score")]][[1L]]
    sc_cat  <- if (paste0(qd$q, "_cat") %in% names(scores))
      scores[[paste0(qd$q, "_cat")]][[1L]] else NA_character_

    score_txt <- if (!is.na(sc_val)) sprintf("%.0f", sc_val) else .swpl("no_data", lang)
    cat_txt   <- if (!is.na(sc_cat)) paste0(" (", sc_cat, ")") else ""

    pad   <- 0.025
    x_mid <- (qd$xmn + qd$xmx) / 2
    y_top <- qd$ymx - pad

    # Background rectangle
    rect_df <- data.frame(
      xmin = qd$xmn + pad, xmax = qd$xmx - pad,
      ymin = qd$ymn + pad, ymax = qd$ymx - pad,
      stringsAsFactors = FALSE
    )
    p <- p +
      ggplot2::geom_rect(
        data = rect_df,
        ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
        fill  = qd$color, alpha = 0.10,
        color = qd$color, linewidth = 0.8
      )

    # Header: quadrant name + score
    header_lbl <- paste0(q_lbl, "  ", score_txt, cat_txt)
    p <- p +
      ggplot2::annotate("text",
                        x = x_mid, y = y_top - 0.01,
                        label = header_lbl,
                        hjust = 0.5, vjust = 1,
                        size  = base_size * 0.34,
                        fontface = "bold",
                        color = qd$color)

    # Top indicator bullets
    ent_inds <- x$indicators[
      x$indicators$entity   == ent &
        x$indicators$quadrant == qd$q, ]
    ent_inds <- ent_inds[order(-ent_inds$norm_score), ]
    ent_inds <- head(ent_inds, top_n)

    n_ind   <- nrow(ent_inds)
    y_start <- y_top - 0.09
    line_h  <- (qd$ymx - qd$ymn - 0.15) / pmax(1L, top_n)

    if (n_ind > 0L) {
      for (k in seq_len(n_ind)) {
        ind_row <- ent_inds[k, ]
        lbl     <- paste0("\u2022 ", ind_row$indicator,
                          " \u2013 ", round(ind_row$norm_score))
        p <- p +
          ggplot2::annotate("text",
                            x = qd$xmn + pad * 1.5,
                            y = y_start - (k - 1L) * line_h,
                            label = lbl,
                            hjust = 0, vjust = 1,
                            size  = base_size * 0.24,
                            color = "gray25")
      }
    } else {
      p <- p +
        ggplot2::annotate("text",
                          x = x_mid, y = y_start,
                          label = .swpl("no_data", lang),
                          hjust = 0.5, vjust = 1,
                          size  = base_size * 0.28, color = "gray60",
                          fontface = "italic")
    }
  }
  p
}


# ── Bar chart ─────────────────────────────────────────────────────────────────

#' @keywords internal
#' @noRd
.swot_plot_bar <- function(x, lang, base_size, entities_sel) {
  long <- .swot_long_scores(x, lang, entities_sel)

  quad_labels <- c(
    .swpl("quadrant_S", lang),
    .swpl("quadrant_W", lang),
    .swpl("quadrant_O", lang),
    .swpl("quadrant_T", lang)
  )
  names(quad_labels) <- c("S", "W", "O", "T")

  fill_vec <- stats::setNames(.swot_colors, unname(quad_labels))

  long$quadrant_label <- factor(long$quadrant_label,
                                 levels = unname(quad_labels))

  ggplot2::ggplot(long,
                  ggplot2::aes(x = score, y = entity, fill = quadrant_label)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75),
                      width = 0.65, alpha = 0.88, na.rm = TRUE) +
    ggplot2::scale_fill_manual(values = fill_vec, name = NULL) +
    ggplot2::scale_x_continuous(limits = c(0, 100),
                                 expand = ggplot2::expansion(mult = c(0, 0.04))) +
    ggplot2::labs(
      title = .swpl("bar_title", lang),
      x     = .swpl("x_score",  lang),
      y     = .swpl("y_entity", lang)
    ) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold"),
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position    = "top"
    )
}
