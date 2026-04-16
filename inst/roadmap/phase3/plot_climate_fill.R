#' plot_climate_fill
#'
#' @description
#' Unified visualization function for gap-filling outputs. Automatically detects
#' **production mode** (filled tibble) vs **evaluation mode** (list with
#' `data` and `metrics` components) and renders appropriate interactive (plotly)
#' or static (ggplot2) plots plus DT metric tables.
#'
#' **Production mode** displays:
#' \itemize{
#'   \item Time-series of observed (with gaps) vs imputed series
#'   \item Imputed points as distinct markers connected by dotted line
#'   \item Interactive range slider with zoom presets
#' }
#'
#' **Evaluation mode** displays a 2×2 dashboard:
#' \itemize{
#'   \item Observed vs Predicted scatter with 1:1 reference line
#'   \item Residual distribution histogram
#'   \item Residuals over time (diagnostic for temporal autocorrelation)
#'   \item Residuals vs observed (diagnostic for heteroscedasticity)
#' }
#'
#' @param df_filled Output of `sus_climate_fill_inmet()`
#' @param df_original Raw data before gap filling (`sus_climate_inmet()`)
#' @param target_var Character vector of variable(s) to visualise
#' @param interactive Logical. If TRUE, uses plotly; if FALSE, uses ggplot2 (default: TRUE)
#' @param output_type What to return: "plot", "table", "metrics", or "all" (default: "all")
#' @param save_plot Optional file path to save the plot (e.g., "plot.png" or "plot.html")
#' @param lang Language: "en", "pt", or "es" (default: "en")
#' @param color_palette ggsci palette name. Options: "npg" (default), "aaas", "nejm", "lancet", "jama", "bmj", "jco", "frontiers", "gsea", "uchicago", "primer", "atlassian", "observable", "d3", "gephi", "igv", "cosmic", "locuszoom", "ucscgb", "startrek", "tron", "futurama", "rickandmorty", "simpsons", "flatui", "bs5", "material", "tw3"
#' @param verbose Print diagnostic information
#'
#' @return Depending on `output_type`, returns the plot, table, metrics, or a list of all.
#'
#' @export
plot_climate_fill <- function(
    df_filled,
    df_original   = NULL,
    target_var,
    interactive    = FALSE,
    output_type   = c("plot", "table", "metrics", "all"),
    save_plot     = NULL,
    lang          = c("en", "pt", "es"),
    color_palette = c("npg", "aaas", "nejm", "lancet", "jama", "bmj", "jco", "frontiers", "gsea",
                       "uchicago", "primer", "atlassian", "observable",
                       "d3", "gephi", "igv", "cosmic", "locuszoom", "ucscgb",
                       "startrek", "tron", "futurama", "rickandmorty", "simpsons", "flatui",
                       "bs5", "material", "tw3"),
    verbose       = FALSE
) {

  # ===========================================================================
  # CHECK PAK
  # ===========================================================================
  
   rlang::check_installed(
      c("ggplot2", "ggsci", "DT", "patchwork"),
      reason = "to run plot_climate_fill"
    )
  
  if(interactive) { 
    rlang::check_installed(
      c("plotly"),
      reason = "to active the interactive mode"
    )
  }
 
  # ===========================================================================
  # VALIDATION ARGS
  # ===========================================================================

  output_type   <- match.arg(output_type)
  lang          <- match.arg(lang)
  color_palette <- match.arg(color_palette)
  
  # Internal defaults (removed from main signature as requested)
  station_code  <- NULL
  date_col      <- "date"
  station_col   <- "station_code"
  start_date    <- NULL
  end_date      <- NULL
  show_table    <- TRUE
  height        <- 800
  n_threshold   <- 5000
  min_points    <- 10
  
  output_format <- if (interactive) "plotly" else "ggplot"

  if (!is.character(target_var) || length(target_var) == 0L)
    cli::cli_abort("target_var must be a non-empty character vector.")

  # ── Detect mode ──────────────────────────────────────────────────────────────
  is_eval <- 
    (is.list(df_filled) &&
       !is.data.frame(df_filled) &&
       length(df_filled) > 0L &&
       !is.null(df_filled[[target_var[1]]]) &&
       all(c("data", "metrics") %in% names(df_filled[[target_var[1]]])))

  # ===========================================================================
  # VALID DF_FILLED
  # ===========================================================================
  if(!is_eval) {
    if (inherits(df_filled, "climasus_df")) {
    
    current_stage <- sus_meta(df_filled, "stage")
    type <- sus_meta(df_filled, "type")
    required_stage <- "climate"

    if (current_stage != "climate" && !type %in% c("filled","inmet")) {

      msg_error <- list(
        en = paste0(
          "Data must be filtered before aggregation.\n",
          "Current stage: ", current_stage %||% "unknown", "\n",
          "Required stage: ", required_stage, "\n\n",
          "Please run:\n",
          "  1. sus_climate_inmet(...)",
          "  2. sus_climate_fill_inmet(evaluation = FALSE)"
        ),
        pt = paste0(
          "Dados devem ser filtrados antes da agregacao.\n",
          "Estagio atual: ", current_stage %||% "desconhecido", "\n",
          "Estagio requerido: ", required_stage, "\n\n",
          "Por favor, execute:\n",
          "  1. sus_climate_inmet(...)",
          "  2. sus_climate_fill_inmet(evaluation = FALSE)"
        ),
        es = paste0(
          "Los datos deben ser filtrados antes de la agregacion.\n",
          "Etapa actual: ", current_stage %||% "desconocida", "\n",
          "Etapa requerida: ", required_stage, "\n\n",
          "Por favor, ejecute:\n",
          "  1. sus_climate_inmet(...)",
          "  2. sus_climate_fill_inmet(evaluation = FALSE)"
        )
      )

      cli::cli_abort(msg_error[[lang]] %||% msg_error[["en"]])
    }
    # Stage validated
    if (verbose) {
      msg_stage_ok <- list(
         en = "Data stage validated: aggregation allowed",
        pt = "Estagio de dados validado: agregacao permitida",
        es = "Etapa de datos validada: agregacion permitida"
      )

      cli::cli_alert_success(msg_stage_ok[[lang]] %||% msg_stage_ok[["en"]])
    }

  } else {
      # NOT climasus_df - ABORT execution
      msg_error <- list(
        en = c(
          "{.red {cli::symbol$cross} Input is not a {.cls climasus_df} object.}",
          "i" = "This function requires data formatted by the {.pkg climasus4r} pipeline.",
          " " = "",
          "Please prepare your data first:",
          "*" = "{.strong 1. Import INMET Data:} {.code df <- sus_climate_inmet(...)}",
          "*" = "{.strong 2. Filling gaps:} {.code df <- sus_climate_fill(...)}"
        ),
        pt = c(
          "{.red {cli::symbol$cross} A entrada como nao objeto {.cls climasus_df}.}",
          "i" = "Esta funcao requer dados processados pelo pipeline {.pkg climasus4r}.",
          " " = "",
          "Por favor, prepare seus dados primeiro:",
          "*" = "{.strong 1. Importar Dados do INMET:} {.code df <- sus_climate_inmet(...)}",
          "*" = "{.strong 2. Preencher falhas:} {.code df <- sus_climate_fill(...)}"
        ),
        es = c(
          "{.red {cli::symbol$cross} La entrada no es un objeto {.cls climasus_df}.}",
          "i" = "Esta funcion requiere datos procesados por el pipeline {.pkg climasus4r}.",
          " " = "",
          "Por favor, prepare sus datos primero:",
          "*" = "{.strong 1. Importar Dados del INMET:} {.code df <- sus_climate_inmet(...)}",
          "*" = "{.strong 2. Preencher falhas:} {.code df <- sus_climate_fill(...)}"
        )
      )
      cli::cli_abort(msg_error[[lang]])
  }
  }
  
  
  # ===========================================================================
  # VALIDA DF_ORIGINAL
  # ===========================================================================
  if(!is_eval) {
     if (inherits(df_original, "climasus_df")) {
    
    current_stage <- sus_meta(df_original, "stage")
    type <- sus_meta(df_original, "type")
    required_stage <- "climate"

    if (current_stage != "climate" && type != "inmet") {

      msg_error <- list(
        en = paste0(
          "Data must be filtered before aggregation.\n",
          "Current stage: ", current_stage %||% "unknown", "\n",
          "Required stage: ", required_stage, "\n\n",
          "Please run:\n",
          "  1. sus_climate_inmet(...)",
          "  2. sus_climate_fill_inmet(evaluation = FALSE)"
        ),
        pt = paste0(
          "Dados devem ser filtrados antes da agregacao.\n",
          "Estagio atual: ", current_stage %||% "desconhecido", "\n",
          "Estagio requerido: ", required_stage, "\n\n",
          "Por favor, execute:\n",
          "  1. sus_climate_inmet(...)",
          "  2. sus_climate_fill_inmet(evaluation = FALSE)"
        ),
        es = paste0(
          "Los datos deben ser filtrados antes de la agregacion.\n",
          "Etapa actual: ", current_stage %||% "desconocida", "\n",
          "Etapa requerida: ", required_stage, "\n\n",
          "Por favor, ejecute:\n",
          "  1. sus_climate_inmet(...)",
          "  2. sus_climate_fill_inmet(evaluation = FALSE)"
        )
      )

      cli::cli_abort(msg_error[[lang]] %||% msg_error[["en"]])
    }
    # Stage validated
    if (verbose) {
      msg_stage_ok <- list(
         en = "Data stage validated: aggregation allowed",
        pt = "Estagio de dados validado: agregacao permitida",
        es = "Etapa de datos validada: agregacion permitida"
      )

      cli::cli_alert_success(msg_stage_ok[[lang]] %||% msg_stage_ok[["en"]])
    }

  } else {
      # NOT climasus_df - ABORT execution
      msg_error <- list(
        en = c(
          "{.red {cli::symbol$cross} Input is not a {.cls climasus_df} object.}",
          "i" = "This function requires data formatted by the {.pkg climasus4r} pipeline.",
          " " = "",
          "Please prepare your data first:",
          "*" = "{.strong 1. Import INMET Data:} {.code df <- sus_climate_inmet(...)}",
          "*" = "{.strong 2. Filling gaps:} {.code df <- sus_climate_fill(...)}"
        ),
        pt = c(
          "{.red {cli::symbol$cross} A entrada como nao objeto {.cls climasus_df}.}",
          "i" = "Esta funcao requer dados processados pelo pipeline {.pkg climasus4r}.",
          " " = "",
          "Por favor, prepare seus dados primeiro:",
          "*" = "{.strong 1. Importar Dados do INMET:} {.code df <- sus_climate_inmet(...)}",
          "*" = "{.strong 2. Preencher falhas:} {.code df <- sus_climate_fill(...)}"
        ),
        es = c(
          "{.red {cli::symbol$cross} La entrada no es un objeto {.cls climasus_df}.}",
          "i" = "Esta funcion requiere datos procesados por el pipeline {.pkg climasus4r}.",
          " " = "",
          "Por favor, prepare sus datos primero:",
          "*" = "{.strong 1. Importar Dados del INMET:} {.code df <- sus_climate_inmet(...)}",
          "*" = "{.strong 2. Preencher falhas:} {.code df <- sus_climate_fill(...)}"
        )
      )
      cli::cli_abort(msg_error[[lang]])
  }
  }
 
  
  # ── Multi-variable loop ───────────────────────────────────────────────────────
  if (length(target_var) > 1L) {

    results <- lapply(target_var, function(tv) {
      plot_climate_fill(
        df_filled = df_filled,
        df_original   = df_original,
        target_var    = tv,
        interactive    = interactive,
        output_type   = "all",
        save_plot     = NULL,
        lang          = lang,
        color_palette = color_palette,
        verbose       = verbose
      )
    })
    names(results) <- target_var

    # Combine plots
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

    # Combined metrics
    all_metrics <- dplyr::bind_rows(
      lapply(target_var, function(tv) {
        m <- results[[tv]]$metrics
        if (!is.null(m) && nrow(m) > 0L)
          dplyr::mutate(m, variable = tv)
      })
    )

    dt_tbl <- if (show_table && nrow(all_metrics) > 0L) {
      if (is_eval)
        .dt_metrics_table(all_metrics,
                          target_var = paste(target_var, collapse = ", "),
                          lang = lang)
      else
        .dt_production_table(all_metrics, lang = lang)
    } else NULL

    # Save plot if requested
    if (!is.null(save_plot)) {
      if (interactive) {
        if (requireNamespace("htmlwidgets", quietly = TRUE)) {
          htmlwidgets::saveWidget(combined_plot, file = save_plot)
          if (verbose) cli::cli_alert_success("Plot saved to {save_plot}")
        } else {
          cli::cli_alert_warning("Package 'htmlwidgets' required to save plotly plots.")
        }
      } else {
        ggplot2::ggsave(filename = save_plot, plot = combined_plot, width = 10, height = 8)
        if (verbose) cli::cli_alert_success("Plot saved to {save_plot}")
      }
    }

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

  # ── Single-variable route ─────────────────────────────────────────────────────
  args <- list(
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
    if (!target_var %in% names(df_filled))
      cli::cli_abort(
        "target_var '{target_var}' not found. Available: {paste(names(df_filled), collapse = ', ')}"
      )
    do.call(.plot_fill_eval,
            c(list(eval_var = df_filled[[target_var]]), args))
  } else {
    if (is.null(df_original))
      cli::cli_abort("df_original must be supplied in production mode.")
    do.call(.plot_fill_series,
            c(list(df_filled = df_filled, df_original = df_original), args))
  }
  
  # Save plot if requested
  if (!is.null(save_plot) && !is.null(res_list$plot)) {
    if (interactive) {
      if (requireNamespace("htmlwidgets", quietly = TRUE)) {
        htmlwidgets::saveWidget(res_list$plot, file = save_plot)
        if (verbose) cli::cli_alert_success("Plot saved to {save_plot}")
      } else {
        cli::cli_alert_warning("Package 'htmlwidgets' required to save plotly plots.")
      }
    } else {
      ggplot2::ggsave(filename = save_plot, plot = res_list$plot, width = 10, height = 6)
      if (verbose) cli::cli_alert_success("Plot saved to {save_plot}")
    }
  }
  
  return(invisible(switch(
    output_type,
    "plot"    = res_list$plot,
    "table"   = res_list$table,
    "metrics" = res_list$metrics,
    res_list
  )))
}

# =============================================================================
#  PRODUCTION-MODE HELPER — time-series
# =============================================================================

#' @keywords internal
#' @noRd
.plot_fill_series <- function(
    df_filled, df_original, target_var, station_code,
    date_col, station_col, start_date, end_date,
    show_table, output_format, height, n_threshold,
    min_points, lang, verbose, color_palette
) {

  # ── Column validation ────────────────────────────────────────────────────────
  flag_col <- paste0("is_imputed_", target_var)

  for (col in c(target_var, date_col)) {
    if (!col %in% names(df_filled))
      cli::cli_abort("Column '{col}' not found in df_filled.")
    if (!col %in% names(df_original))
      cli::cli_abort("Column '{col}' not found in df_original.")
  }

  if (!flag_col %in% names(df_filled)) {
    cli::cli_alert_warning(.i18n("warn_no_flag", lang, fc = flag_col))
    flag_col <- NULL
  }

  # ── Station filter ───────────────────────────────────────────────────────────
  if (!is.null(station_code)) {
    if (!station_col %in% names(df_filled))
      cli::cli_abort("station_col '{station_col}' not found in df_filled.")
    df_filled   <- dplyr::filter(df_filled,   .data[[station_col]] == station_code)
    df_original <- dplyr::filter(df_original, .data[[station_col]] == station_code)
    if (nrow(df_filled) == 0L)
      cli::cli_abort("No rows for station_code = '{station_code}'.")
  }

  # ── Date-window clip ─────────────────────────────────────────────────────────
  if (!is.null(start_date) || !is.null(end_date)) {
    sd <- if (!is.null(start_date)) as.POSIXct(start_date, tz = "UTC") else
      as.POSIXct(-Inf, origin = "1970-01-01", tz = "UTC")
    ed <- if (!is.null(end_date)) as.POSIXct(end_date, tz = "UTC") else
      as.POSIXct(Inf,  origin = "1970-01-01", tz = "UTC")
    df_filled   <- dplyr::filter(df_filled,   .data[[date_col]] >= sd & .data[[date_col]] <= ed)
    df_original <- dplyr::filter(df_original, .data[[date_col]] >= sd & .data[[date_col]] <= ed)
  }

  # ── Join ─────────────────────────────────────────────────────────────────────
  join_by <- if (station_col %in% names(df_filled) &&
                 station_col %in% names(df_original))
    c(date_col, station_col) else date_col

  keep_filled <- unique(c(join_by, target_var, flag_col))
  keep_orig   <- c(join_by, target_var)

  df_joined <- dplyr::full_join(
    df_original[, intersect(keep_orig,   names(df_original))],
    df_filled[,  intersect(keep_filled,  names(df_filled))],
    by     = join_by,
    suffix = c("_original", "_filled")
  ) |> dplyr::arrange(.data[[date_col]])

  orig_col   <- paste0(target_var, "_original")
  filled_col <- paste0(target_var, "_filled")

  # ── Imputed flag ─────────────────────────────────────────────────────────────
  if (!is.null(flag_col) && flag_col %in% names(df_joined)) {
    df_joined$is_imputed <- as.logical(df_joined[[flag_col]])
    df_joined$is_imputed[is.na(df_joined$is_imputed)] <- FALSE
  } else {
    df_joined$is_imputed <-
      is.na(df_joined[[orig_col]]) & !is.na(df_joined[[filled_col]])
  }

  # ── Minimum-points guard ─────────────────────────────────────────────────────
  n_valid_orig <- sum(!is.na(df_joined[[orig_col]]))
  if (!.validate_min_points(n_valid_orig, station_code, min_points, lang))
    return(invisible(list(plot = NULL, table = NULL, metrics = NULL,
                          data = df_joined)))

  # ── Verbose diagnostics ──────────────────────────────────────────────────────
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

  # ── Summary metrics ──────────────────────────────────────────────────────────
  total_obs    <- nrow(df_joined)
  total_filled <- sum(df_joined$is_imputed, na.rm = TRUE)
  pct_filled   <- if (total_obs > 0L) round((total_filled / total_obs) * 100, 1) else 0

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

  # ── Build plot ───────────────────────────────────────────────────────────────
  fig <- if (output_format == "plotly") {
    .build_series_plotly(df_joined, orig_col, filled_col, date_col,
                         target_var, station_code, total_filled, pct_filled,
                         height, n_threshold, lang, verbose, color_palette)
  } else {
    .build_series_ggplot(df_joined, orig_col, filled_col, date_col,
                         target_var, station_code, total_filled, pct_filled,
                         lang, color_palette)
  }

  dt_tbl <- if (show_table)
    .dt_production_table(prod_metrics, lang = lang) else NULL

  invisible(list(plot = fig, table = dt_tbl, metrics = prod_metrics,
                 data = df_joined))
}


# =============================================================================
#  PLOTLY — time-series builder (CORRIGIDO - DATAS)
# =============================================================================

#' @keywords internal
#' @noRd
.build_series_plotly <- function(
    df_joined, orig_col, filled_col, date_col,
    target_var, station_code, total_filled, pct_filled,
    height, n_threshold, lang, verbose, color_palette
) {

  pal <- .get_scientific_palette(color_palette)
  
  # Garantir que as datas sejam POSIXct
  if (!inherits(df_joined[[date_col]], "POSIXct")) {
    df_joined[[date_col]] <- as.POSIXct(df_joined[[date_col]], tz = "UTC")
  }
  
  ds      <- .maybe_downsample(df_joined, n_threshold = n_threshold,
                                verbose = verbose, lang = lang)
  line_df <- ds$line_df
  
  if (!inherits(line_df[[date_col]], "POSIXct")) {
    line_df[[date_col]] <- as.POSIXct(line_df[[date_col]], tz = "UTC")
  }
  
  tt      <- if (ds$use_gl) "scattergl" else "scatter"
  
  # DEBUG: Verificar a coluna is_imputed
  if (verbose) {
    cli::cli_text("Columns in df_joined: {paste(names(df_joined), collapse = ', ')}")
    cli::cli_text("is_imputed exists: {'is_imputed' %in% names(df_joined)}")
    if ('is_imputed' %in% names(df_joined)) {
      cli::cli_text("is_imputed sum: {sum(df_joined$is_imputed, na.rm = TRUE)}")
    }
  }
  
  # Garantir que is_imputed existe e é lógico
  if (!"is_imputed" %in% names(df_joined)) {
    df_joined$is_imputed <- is.na(df_joined[[orig_col]]) & !is.na(df_joined[[filled_col]])
  }
  
  # Filtrar linhas imputadas
  imputed_rows <- df_joined[df_joined$is_imputed & !is.na(df_joined[[filled_col]]), ]
  
  if (nrow(imputed_rows) > 0) {
    if (!inherits(imputed_rows[[date_col]], "POSIXct")) {
      imputed_rows[[date_col]] <- as.POSIXct(imputed_rows[[date_col]], tz = "UTC")
    }
    
    # Criar grupos para linhas consecutivas
    idx <- which(df_joined$is_imputed)
    if (length(idx) > 0) {
      imputed_rows$imputed_group <- cumsum(c(1, diff(idx) != 1))
    }
  }
  
  title_str <- sprintf(
    "<b>%s — %s</b>%s<br><sup>%d %s (%.1f%%)</sup>",
    .i18n("title_series", lang), target_var,
    if (!is.null(station_code))
      paste0("  |  ", .i18n("station_label", lang), ": ", station_code) else "",
    total_filled, .i18n("gaps_filled", lang), pct_filled
  )

  fig <- plotly::plot_ly(
    height = height,
    width = NULL
  )
  
  # 1. Série original com falhas (linha com gaps)
  fig <- fig |> plotly::add_trace(
    x           = line_df[[date_col]],
    y           = line_df[[orig_col]],
    type        = tt, 
    mode        = "lines",
    name        = .i18n("original", lang),
    line        = list(color = pal$original_observed, width = 2),
    connectgaps = FALSE,
    hovertemplate = paste0(
      "<b>%{x|%Y-%m-%d %H:%M}</b><br>",
      target_var, ": %{y:.2f}<extra></extra>"
    )
  )
  
  # 2. SÉRIE COMPLETA PREENCHIDA (linha contínua) - ESTAVA FALTANDO!
  fig <- fig |> plotly::add_trace(
    x           = line_df[[date_col]],
    y           = line_df[[filled_col]],
    type        = tt, 
    mode        = "lines",
    name        = .i18n("filled_series", lang),
    line        = list(color = pal$filled_imputed, width = 1.5),
    connectgaps = TRUE,  # Conecta todos os pontos, incluindo os imputados
    hovertemplate = paste0(
      "<b>%{x|%Y-%m-%d %H:%M}</b><br>",
      target_var, ": %{y:.2f}<extra></extra>"
    )
  )
  
  # 3. Adicionar pontos imputados como marcadores destacados
  if (nrow(imputed_rows) > 0) {
    fig <- fig |> plotly::add_trace(
      x    = imputed_rows[[date_col]],
      y    = imputed_rows[[filled_col]],
      type = "scatter", 
      mode = "markers",
      name = .i18n("imputed_pts", lang),
      marker = list(
        size   = 8,
        color  = pal$imputed_marker,
        symbol = "circle",
        line   = list(color = "white", width = 1.5)
      ),
      hovertemplate = paste0(
        "<b>%{x|%Y-%m-%d %H:%M}</b><br>",
        .i18n("imputed_pts", lang), ": %{y:.2f}<extra></extra>"
      )
    )
  }

  # Calcular range de datas
  min_date <- min(line_df[[date_col]], na.rm = TRUE)
  max_date <- max(line_df[[date_col]], na.rm = TRUE)
  
  fig |> plotly::layout(
    title     = list(text = title_str, font = list(size = 14)),
    xaxis     = list(
      title         = .i18n("date_time", lang),
      type          = "date",
      range         = c(min_date, max_date),
      tickformat    = "%Y-%m-%d",
      rangeslider   = list(visible = TRUE),
      rangeselector = list(
        buttons = list(
          list(count = 7,  label = "7d", step = "day", stepmode = "backward"),
          list(count = 1,  label = "1m", step = "month", stepmode = "backward"),
          list(count = 3,  label = "3m", step = "month", stepmode = "backward"),
          list(count = 6,  label = "6m", step = "month", stepmode = "backward"),
          list(count = 1,  label = "1y", step = "year", stepmode = "backward"),
          list(step = "all")
        )
      )
    ),
    yaxis     = list(
      title      = target_var,
      tickformat = ".2f"
    ),
    legend    = list(
      orientation = "h", 
      x = 0, 
      y = -0.25,
      traceorder = "normal"
    ),
    hovermode = "x unified",
    template  = "plotly_white"
  )
}
# .build_series_plotly <- function(
#     df_joined, orig_col, filled_col, date_col,
#     target_var, station_code, total_filled, pct_filled,
#     height, n_threshold, lang, verbose, color_palette
# ) {

#   pal <- .get_scientific_palette(color_palette)
  
#   ds      <- .maybe_downsample(df_joined, n_threshold = n_threshold,
#                                 verbose = verbose, lang = lang)
#   line_df <- ds$line_df
  
#   # Garantir que line_df também tenha datas POSIXct
#   if (!inherits(line_df[[date_col]], "POSIXct")) {
#     line_df[[date_col]] <- as.POSIXct(line_df[[date_col]], tz = "UTC")
#   }
  
#   tt      <- if (ds$use_gl) "scattergl" else "scatter"
  
#   # Get imputed rows (non-NA and flagged as imputed)
#   imputed_rows <- df_joined[df_joined$is_imputed & 
#                            !is.na(df_joined[[filled_col]]), ]
  
#   # Garantir que imputed_rows também tenha datas POSIXct
#   if (nrow(imputed_rows) > 0 && !inherits(imputed_rows[[date_col]], "POSIXct")) {
#     imputed_rows[[date_col]] <- as.POSIXct(imputed_rows[[date_col]], tz = "UTC")
#   }
  
#   # Get contiguous blocks of imputed values for line connection
#   if (nrow(imputed_rows) > 0) {
#     idx <- which(df_joined$is_imputed)
#     imputed_rows$imputed_group <- cumsum(c(1, diff(idx) != 1))
#   }
  
#   title_str <- sprintf(
#     "<b>%s — %s</b>%s<br><sup>%d %s (%.1f%%)</sup>",
#     .i18n("title_series", lang), target_var,
#     if (!is.null(station_code))
#       paste0("  |  ", .i18n("station_label", lang), ": ", station_code) else "",
#     total_filled, .i18n("gaps_filled", lang), pct_filled
#   )

#   fig <- plotly::plot_ly(
#     height = height,
#     width = NULL
#   ) |>
#     # 1. Original series with gaps (no line connection across NAs)
#     plotly::add_trace(
#       x           = line_df[[date_col]],
#       y           = line_df[[orig_col]],
#       type        = tt, 
#       mode        = "lines",
#       name        = .i18n("original", lang),
#       line        = list(color = pal$original_observed, width = 1.8),
#       connectgaps = FALSE,
#       hovertemplate = paste0(
#         "<b>%{x|%Y-%m-%d %H:%M}</b><br>",
#         target_var, ": %{y:.2f}<extra></extra>"
#       )
#     ) |>
#     # 2. Imputed values as markers
#     plotly::add_trace(
#       x    = imputed_rows[[date_col]],
#       y    = imputed_rows[[filled_col]],
#       type = "scatter", 
#       mode = "markers",
#       name = .i18n("imputed_pts", lang),
#       marker = list(
#         size   = 6,
#         color  = pal$imputed_marker,
#         symbol = "circle",
#         line   = list(color = pal$filled_imputed, width = 1.5)
#       ),
#       hovertemplate = paste0(
#         "<b>%{x|%Y-%m-%d %H:%M}</b><br>",
#         .i18n("imputed_pts", lang), ": %{y:.2f}<extra></extra>"
#       )
#     )

#   # 3. Add line segments connecting consecutive imputed points
#   if (nrow(imputed_rows) > 1) {
#     line_x <- c()
#     line_y <- c()
    
#     for (grp in unique(imputed_rows$imputed_group)) {
#       grp_rows <- imputed_rows[imputed_rows$imputed_group == grp, ]
#       if (nrow(grp_rows) > 1) {
#         line_x <- c(line_x, grp_rows[[date_col]], NA)
#         line_y <- c(line_y, grp_rows[[filled_col]], NA)
#       }
#     }
    
#     if (length(line_x) > 0) {
#       fig <- fig |> plotly::add_trace(
#         x = line_x,
#         y = line_y,
#         type = "scatter", 
#         mode = "lines",
#         name = .i18n("filled_series", lang),
#         line = list(color = pal$imputed_line, width = 1.5, dash = "dot"),
#         showlegend = FALSE,
#         hoverinfo = "none",
#         legendgroup = "imputed"
#       )
#     }
#   }

#   # CORREÇÃO: Configuração explícita do eixo x para datas
#   # Calculate min and max dates to ensure the range is correct
#   min_date <- min(line_df[[date_col]], na.rm = TRUE)
#   max_date <- max(line_df[[date_col]], na.rm = TRUE)
  
#   fig |> plotly::layout(
#     title     = list(text = title_str, font = list(size = 14)),
#     xaxis     = list(
#       title         = .i18n("date_time", lang),
#       type          = "date",           # Força tipo data
#       range         = c(min_date, max_date), # Force exact date range
#       tickformat    = "%Y-%m-%d %H:%M", # Formato ano-mês-dia hora:minuto
#       tickmode      = "auto",           # Modo automático de ticks
#       nticks        = 10,               # Número aproximado de ticks
#       rangeslider   = list(visible = TRUE),
#       rangeselector = list(
#         buttons = list(
#           list(count = 1, label = "1w", step = "week", stepmode = "backward"),
#           list(count = 1, label = "1m", step = "month", stepmode = "backward"),
#           list(count = 6, label = "6m", step = "month", stepmode = "backward"),
#           list(count = 1, label = "1y", step = "year",  stepmode = "backward"),
#           list(step = "all")
#         )
#       )
#     ),yaxis     = list(
#       title      = target_var,
#       tickformat = ".2f"
#     ),
#     legend    = list(orientation = "h", x = 0, y = -0.25),
#     hovermode = "x unified",
#     template  = "plotly_white"
#   )
# }


# =============================================================================
#  PLOTLY — evaluation dashboard builder (CORRIGIDO - DATAS)
# =============================================================================

#' @keywords internal
#' @noRd
.build_eval_plotly <- function(
    df_valid, y_true, y_pred, residuals,
    date_col, target_var, station_code,
    r2_pool, rmse_p, mae_p, n_pool,
    height, n_threshold, lang, color_palette, verbose
) {

  pal <- .get_scientific_palette(color_palette)
  
  # CORREÇÃO: Garantir que a coluna de data seja POSIXct
  has_date <- date_col %in% names(df_valid)
  if (has_date && !inherits(df_valid[[date_col]], "POSIXct")) {
    df_valid[[date_col]] <- as.POSIXct(df_valid[[date_col]], tz = "UTC")
  }
  
  lim_min <- min(c(y_true, y_pred))
  lim_max <- max(c(y_true, y_pred))
  ds      <- .maybe_downsample(df_valid, n_threshold = n_threshold,
                                verbose = verbose, lang = lang)
  tt      <- if (ds$use_gl) "scattergl" else "scatter"

  # Panel 1: Observed vs Predicted
  fig_scat <- plotly::plot_ly(
    height = height,
    width = NULL
  ) |>
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
      title    = list(
        text = sprintf(
          "<b>%s</b><br><sup>R² = %.3f | RMSE = %.3f | MAE = %.3f | n = %d</sup>",
          .i18n("obs_vs_pred", lang), r2_pool, rmse_p, mae_p, n_pool
        ),
        font = list(size = 11)
      ),
      xaxis    = list(
        title = .i18n("observed", lang), 
        tickformat = ".2f"
      ),
      yaxis    = list(
        title = .i18n("predicted", lang),
        tickformat = ".2f"
      ),
      template = "plotly_white"
    )

  # Panel 2: Residual histogram
  fig_hist <- plotly::plot_ly(
    height = height,
    width = NULL
  ) |>
    plotly::add_trace(
      x = residuals, 
      type = "histogram", 
      nbinsx = 40, 
      opacity = 0.75,
      marker = list(
        color = pal$residual_hist,
        line  = list(color = "white", width = 0.5)
      ),
      name = .i18n("residuals", lang)
    ) |>
    plotly::layout(
      title    = list(
        text = paste0("<b>", .i18n("res_dist", lang), "</b>"),
        font = list(size = 11)
      ),
      xaxis    = list(
        title = .i18n("residual_axis", lang),
        tickformat = ".2f"
      ),
      yaxis    = list(title = .i18n("count", lang)),
      bargap   = 0.05,
      template = "plotly_white"
    )

  # Panel 3: Residuals vs Observed
  fig_res_vs_obs <- plotly::plot_ly(
    height = height,
    width = NULL
  ) |>
    plotly::add_trace(
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
      x = min(y_true), y = 0, xend = max(y_true), yend = 0,
      line = list(color = pal$zero_line, dash = "dash", width = 1),
      showlegend = FALSE
    ) |>
    plotly::layout(
      title    = list(
        text = paste0("<b>", .i18n("res_vs_obs", lang), "</b>"),
        font = list(size = 11)
      ),
      xaxis    = list(
        title = .i18n("observed", lang),
        tickformat = ".2f"
      ),
      yaxis    = list(
        title = .i18n("residuals", lang),
        tickformat = ".2f"
      ),
      template = "plotly_white"
    )

  # Panel 4: Residuals over time (if date available)
  fig_combined <- if (has_date) {
    fig_ts_res <- plotly::plot_ly(
      height = height,
      width = NULL
    ) |>
      plotly::add_trace(
        x = df_valid[[date_col]], 
        y = residuals,
        type = tt, 
        mode = "markers",
        marker = list(color = pal$residual_point, opacity = 0.5, size = 3),
        name = .i18n("res_over_t", lang),
        hovertemplate = paste0(
          "<b>%{x|%Y-%m-%d %H:%M}</b><br>",
          .i18n("residuals", lang), ": %{y:.3f}<extra></extra>"
        )
      ) |>
      plotly::add_segments(
        x = min(df_valid[[date_col]]), y = 0,
        xend = max(df_valid[[date_col]]), yend = 0,
        line = list(color = pal$zero_line, dash = "dash", width = 1),
        showlegend = FALSE
      ) |>
      plotly::layout(
        title    = list(
          text = paste0("<b>", .i18n("res_over_time", lang), "</b>"),
          font = list(size = 11)
        ),
        xaxis    = list(
          title = .i18n("date_time", lang),
          type = "date",
          tickformat = "%Y-%m-%d",
          tickmode = "auto",
          nticks = 10
        ),
        yaxis    = list(
          title = .i18n("residuals", lang),
          tickformat = ".2f"
        ),
        template = "plotly_white"
      )

    plotly::subplot(
      fig_scat, fig_hist, fig_res_vs_obs, fig_ts_res,
      nrows = 2, 
      shareX = FALSE,
      titleX = TRUE, 
      titleY = TRUE, 
      margin = 0.06,
      heights = c(height, height)
    )
  } else {
    plotly::subplot(
      fig_scat, fig_hist, fig_res_vs_obs,
      nrows = 1, 
      shareX = FALSE,
      titleX = TRUE, 
      titleY = TRUE, 
      margin = 0.06,
      heights = c(height, height, height)
    )
  }

  fig_combined |>
    plotly::layout(
      title = list(
        text = paste0(
          "<b>", .i18n("eval_dashboard", lang), " — ", target_var, "</b>",
          if (!is.null(station_code))
            paste0("  |  ", .i18n("station_label", lang), ": ", station_code)
        ),
        font = list(size = 13)
      ),
      showlegend = TRUE
    )
}
# =============================================================================
#  GGPLOT2 — time-series builder (CORRECTED)
# =============================================================================

#' @keywords internal
#' @noRd
.build_series_ggplot <- function(
    df_joined, orig_col, filled_col, date_col,
    target_var, station_code, total_filled, pct_filled,
    lang, color_palette
) {

  pal <- .get_scientific_palette(color_palette)
  
  # Garantir que is_imputed existe
  if (!"is_imputed" %in% names(df_joined)) {
    df_joined$is_imputed <- is.na(df_joined[[orig_col]]) & !is.na(df_joined[[filled_col]])
  }
  
  # Preparar dados
  df_plot <- df_joined |>
    dplyr::select(dplyr::all_of(c(date_col, orig_col, filled_col, "is_imputed"))) |>
    dplyr::mutate(
      original_val = .data[[orig_col]],
      filled_val = .data[[filled_col]],
      imputed_val = ifelse(.data$is_imputed, .data[[filled_col]], NA_real_)
    )
  
  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[[date_col]])) +
    # Série original (com gaps)
    ggplot2::geom_line(
      ggplot2::aes(y = .data$original_val, colour = "original"),
      linewidth = 1,
      na.rm = TRUE
    ) +
    # SÉRIE COMPLETA PREENCHIDA (linha contínua) - ESTAVA FALTANDO!
    ggplot2::geom_line(
      ggplot2::aes(y = .data$filled_val, colour = "filled"),
      linewidth = 1,
      alpha = 0.8,
      na.rm = TRUE
    ) +
    # Pontos imputados destacados
    ggplot2::geom_point(
      ggplot2::aes(y = .data$imputed_val, colour = "imputed"),
      shape = 16,
      size = 2,
      alpha = 0.9,
      na.rm = TRUE
    ) +
    ggplot2::scale_x_datetime(date_labels = "%Y-%m-%d") +
    ggplot2::labs(
      title   = paste0(
        .i18n("title_series", lang), " — ", target_var,
        if (!is.null(station_code))
          paste0(" | ", .i18n("station_label", lang), ": ", station_code),
        sprintf("\n%d %s (%.1f%%)",
                total_filled, .i18n("gaps_filled", lang), pct_filled)
      ),
      x       = .i18n("date_time", lang),
      y       = target_var,
      colour  = NULL
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        "original" = pal$original_observed,
        "filled"   = pal$filled_imputed,
        "imputed"  = pal$imputed_marker
      ),
      labels = c(
        "original" = .i18n("original", lang),
        "filled"   = .i18n("filled_series", lang),
        "imputed"  = .i18n("imputed_pts", lang)
      )
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title      = ggplot2::element_text(size = 11, face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = pal$grid_line)
    )
  
  return(p)
}
# .build_series_ggplot <- function(
#     df_joined, orig_col, filled_col, date_col,
#     target_var, station_code, total_filled, pct_filled,
#     lang, color_palette
# ) {

#   pal <- .get_scientific_palette(color_palette)
  
#   # Prepare data for original series only (for line)
#   df_original_long <- df_joined |>
#     dplyr::select(dplyr::all_of(c(date_col, orig_col))) |>
#     dplyr::rename(value = all_of(orig_col)) |>
#     dplyr::mutate(series = .i18n("original", lang))
  
#   # Imputed points data
#   imputed_df <- df_joined[df_joined$is_imputed & 
#                           !is.na(df_joined[[filled_col]]), 
#                           c(date_col, filled_col)]
#   if (nrow(imputed_df) > 0) {
#     imputed_df <- imputed_df |> dplyr::rename(value = all_of(filled_col))
#     imputed_df$series <- .i18n("imputed_pts", lang)
#   }

#   # Create a combined dataframe for ggplot to handle legends properly
#   df_plot <- df_joined |>
#     dplyr::select(dplyr::all_of(c(date_col, orig_col, filled_col, "is_imputed"))) |>
#     dplyr::mutate(
#       original_val = .data[[orig_col]],
#       imputed_val = ifelse(.data$is_imputed, .data[[filled_col]], NA_real_)
#     )
    
#   # Identify consecutive imputed points for grouping lines
#   df_plot$imputed_group <- NA_integer_
#   if (any(df_plot$is_imputed, na.rm = TRUE)) {
#     idx <- which(df_plot$is_imputed)
#     df_plot$imputed_group[idx] <- cumsum(c(1, diff(idx) != 1))
#   }

#   p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[[date_col]])) +
#     # Original series line (shows gaps)
#     ggplot2::geom_line(
#       ggplot2::aes(y = .data$original_val, colour = "original"),
#       linewidth = 0.8,
#       na.rm = TRUE
#     ) +
#     # Imputed lines (connecting consecutive imputed points)
#     ggplot2::geom_line(
#       ggplot2::aes(y = .data$imputed_val, group = .data$imputed_group, colour = "imputed"),
#       linewidth = 0.8,
#       linetype = "dashed",
#       na.rm = TRUE
#     ) +
#     # Imputed points
#     ggplot2::geom_point(
#       ggplot2::aes(y = .data$imputed_val, colour = "imputed"),
#       shape = 16,
#       size = 1.5,
#       alpha = 0.8,
#       na.rm = TRUE
#     ) +
#     # Formatting x-axis for dates
#     ggplot2::scale_x_datetime(date_labels = "%Y-%m-%d\n%H:%M") +
#     ggplot2::labs(
#       title   = paste0(
#         .i18n("title_series", lang), " \u2014 ", target_var,
#         if (!is.null(station_code))
#           paste0(" | ", .i18n("station_label", lang), ": ", station_code),
#         sprintf("\n%d %s (%.1f%%)",
#                 total_filled, .i18n("gaps_filled", lang), pct_filled)
#       ),
#       x       = .i18n("date_time", lang),
#       y       = target_var,
#       colour  = NULL
#     ) +
#     ggplot2::scale_colour_manual(
#       values = stats::setNames(
#         c(pal$original_observed, pal$imputed_marker),
#         c("original", "imputed")
#       ),
#       labels = stats::setNames(
#         c(.i18n("original", lang), .i18n("imputed_pts", lang)),
#         c("original", "imputed")
#       )
#     ) +
#     ggplot2::theme_bw() +
#     ggplot2::theme(
#       legend.position = "bottom",
#       plot.title      = ggplot2::element_text(size = 11, face = "bold"),
#       panel.grid.minor = ggplot2::element_blank(),
#       panel.grid.major = ggplot2::element_line(colour = pal$grid_line)
#     )
# }


# =============================================================================
#  EVALUATION-MODE HELPER
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

  # ── Station filter ───────────────────────────────────────────────────────────
  if (!is.null(station_code)) {
    if (!station_col %in% names(df))
      cli::cli_abort("station_col '{station_col}' not found in evaluation data.")
    df      <- dplyr::filter(df,      .data[[station_col]] == station_code)
    metrics <- dplyr::filter(metrics, .data$station_id     == station_code)
    if (nrow(df) == 0L)
      cli::cli_abort("No evaluation rows for station '{station_code}'.")
  }

  # ── Date-window clip ─────────────────────────────────────────────────────────
  if (date_col %in% names(df) && (!is.null(start_date) || !is.null(end_date))) {
    sd <- if (!is.null(start_date)) as.POSIXct(start_date, tz = "UTC") else
      as.POSIXct(-Inf, origin = "1970-01-01", tz = "UTC")
    ed <- if (!is.null(end_date)) as.POSIXct(end_date, tz = "UTC") else
      as.POSIXct(Inf,  origin = "1970-01-01", tz = "UTC")
    df <- dplyr::filter(df, .data[[date_col]] >= sd & .data[[date_col]] <= ed)
  }

  # ── Validate ─────────────────────────────────────────────────────────────────
  missing_cols <- setdiff(c("true_value", "pred_value"), names(df))
  if (length(missing_cols) > 0L)
    cli::cli_abort("Evaluation data missing columns: {paste(missing_cols, collapse = ', ')}")

  df_valid <- df[!is.na(df$true_value) & !is.na(df$pred_value), ]
  if (nrow(df_valid) == 0L)
    cli::cli_abort("No valid true/pred pairs found.")

  if (!.validate_min_points(nrow(df_valid), station_code, min_points, lang))
    return(invisible(list(plot = NULL, table = NULL, metrics = metrics,
                          data = df_valid)))

  # ── Verbose diagnostics ──────────────────────────────────────────────────────
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

  # ── Pooled metrics ───────────────────────────────────────────────────────────
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
          r2 = round(r2_pool, 3))
  )

  # ── Build plot ───────────────────────────────────────────────────────────────
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
    .dt_metrics_table(metrics, target_var = target_var, lang = lang) else NULL

  invisible(list(plot = fig, table = dt_tbl, metrics = metrics, data = df_valid))
}


# =============================================================================
#  PLOTLY — evaluation dashboard builder (COMPLETE with i18n)
# =============================================================================

#' @keywords internal
#' @noRd
.build_eval_plotly <- function(
    df_valid, y_true, y_pred, residuals,
    date_col, target_var, station_code,
    r2_pool, rmse_p, mae_p, n_pool,
    height, n_threshold, lang, color_palette, verbose
) {

  pal <- .get_scientific_palette(color_palette)
  
  lim_min <- min(c(y_true, y_pred))
  lim_max <- max(c(y_true, y_pred))
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
      title    = list(
        text = sprintf(
          "<b>%s</b><br><sup>R² = %.3f | RMSE = %.3f | MAE = %.3f | n = %d</sup>",
          .i18n("obs_vs_pred", lang), r2_pool, rmse_p, mae_p, n_pool
        ),
        font = list(size = 11)
      ),
      xaxis    = list(title = .i18n("observed", lang), 
                     tickformat = ".2f"),
      yaxis    = list(title = .i18n("predicted", lang),
                     tickformat = ".2f"),
      height   = height, template = "plotly_white"
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
      xaxis    = list(title = .i18n("residual_axis", lang),
                     tickformat = ".2f"),
      yaxis    = list(title = .i18n("count", lang)),
      bargap   = 0.05, height = height, template = "plotly_white"
    )

  # Panel 3: Residuals vs Observed (heteroscedasticity diagnostic)
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
      x = min(y_true), y = 0, xend = max(y_true), yend = 0,
      line = list(color = pal$zero_line, dash = "dash", width = 1),
      showlegend = FALSE
    ) |>
    plotly::layout(
      title    = list(text = paste0("<b>", .i18n("res_vs_obs", lang), "</b>"),
                     font = list(size = 11)),
      xaxis    = list(title = .i18n("observed", lang),
                     tickformat = ".2f"),
      yaxis    = list(title = .i18n("residuals", lang),
                     tickformat = ".2f"),
      height   = height, template = "plotly_white"
    )

  # Panel 4: Residuals over time (if date available)
  has_date <- date_col %in% names(df_valid)

  fig_combined <- if (has_date) {
    fig_ts_res <- plotly::plot_ly(
      x = df_valid[[date_col]], y = residuals,
      type = tt, mode = "markers",
      marker = list(color = pal$residual_point, opacity = 0.5, size = 3),
      name = .i18n("res_over_t", lang),
      hovertemplate = paste0(
        "<b>%{x|%Y-%m-%d %H:%M}</b><br>",
        .i18n("residuals", lang), ": %{y:.3f}<extra></extra>"
      )
    ) |>
      plotly::add_segments(
        x = min(df_valid[[date_col]]), y = 0,
        xend = max(df_valid[[date_col]]), yend = 0,
        line = list(color = pal$zero_line, dash = "dash", width = 1),
        showlegend = FALSE
      ) |>
      plotly::layout(
        title    = list(text = paste0("<b>", .i18n("res_over_time", lang), "</b>"),
                       font = list(size = 11)),
        xaxis    = list(title = .i18n("date_time", lang)),
        yaxis    = list(title = .i18n("residuals", lang),
                       tickformat = ".2f"),
        height   = height, template = "plotly_white"
      )

    plotly::subplot(fig_scat, fig_hist, fig_res_vs_obs, fig_ts_res,
                    nrows = 2, shareX = FALSE,
                    titleX = TRUE, titleY = TRUE, margin = 0.06)
  } else {
    plotly::subplot(fig_scat, fig_hist, fig_res_vs_obs,
                    nrows = 1, shareX = FALSE,
                    titleX = TRUE, titleY = TRUE, margin = 0.06)
  }

  fig_combined |>
    plotly::layout(
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
#  GGPLOT2 — evaluation dashboard builder (COMPLETE with i18n)
# =============================================================================

#' @keywords internal
#' @noRd
.build_eval_ggplot <- function(
    df_valid, y_true, y_pred, residuals,
    date_col, target_var, station_code,
    r2_pool, rmse_p, mae_p, n_pool,
    lang, color_palette
) {

  pal <- .get_scientific_palette(color_palette)
  
  df_scat <- data.frame(
    observed = y_true, 
    predicted = y_pred,
    residual = residuals
  )

  # Panel 1: Observed vs Predicted
  p_scat <- ggplot2::ggplot(df_scat,
                             ggplot2::aes(x = observed, y = predicted)) +
    ggplot2::geom_point(colour = pal$scatter, alpha = 0.4, size = 1.5) +
    ggplot2::geom_abline(slope = 1, intercept = 0,
                          colour = pal$reference_line, linetype = "dashed",
                          linewidth = 0.8) +
    ggplot2::geom_smooth(method = "lm", se = FALSE,
                          colour = "grey40", linewidth = 0.5) +
    ggplot2::labs(
      title = sprintf(
        "%s\nR² = %.3f | RMSE = %.3f | MAE = %.3f | n = %d",
        .i18n("obs_vs_pred", lang), r2_pool, rmse_p, mae_p, n_pool
      ),
      x = .i18n("observed", lang),
      y = .i18n("predicted", lang)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 9, face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )

  # Panel 2: Residual histogram
  p_hist <- ggplot2::ggplot(df_scat, ggplot2::aes(x = residual)) +
    ggplot2::geom_histogram(bins = 40, fill = pal$residual_hist,
                             colour = "white", alpha = 0.8) +
    ggplot2::geom_vline(xintercept = 0,
                         colour = pal$reference_line, linetype = "dashed",
                         linewidth = 0.8) +
    ggplot2::labs(
      title = .i18n("res_dist", lang),
      x     = .i18n("residual_axis", lang),
      y     = .i18n("count", lang)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 9, face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )

  # Panel 3: Residuals vs Observed
  p_res_vs_obs <- ggplot2::ggplot(df_scat, 
                                   ggplot2::aes(x = observed, y = residual)) +
    ggplot2::geom_point(colour = pal$scatter, alpha = 0.4, size = 1) +
    ggplot2::geom_hline(yintercept = 0, 
                         colour = pal$zero_line, linetype = "dashed",
                         linewidth = 0.8) +
    ggplot2::labs(
      title = .i18n("res_vs_obs", lang),
      x = .i18n("observed", lang),
      y = .i18n("residuals", lang)
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 9, face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )

  has_date <- date_col %in% names(df_valid)
  
  if (has_date) {
    df_scat[[date_col]] <- df_valid[[date_col]]
    # Panel 4: Residuals over time
    p_ts <- ggplot2::ggplot(df_scat,
                             ggplot2::aes(x = .data[[date_col]], y = residual)) +
      ggplot2::geom_point(colour = pal$residual_point, alpha = 0.4, size = 1) +
      ggplot2::geom_hline(yintercept = 0,
                           colour = pal$zero_line, linetype = "dashed",
                           linewidth = 0.8) +
      ggplot2::labs(
        title = .i18n("res_over_time", lang),
        x     = .i18n("date_time", lang),
        y     = .i18n("residuals", lang)
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 9, face = "bold"),
        panel.grid.minor = ggplot2::element_blank()
      )

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
#'
#' @param metrics Tibble from `sus_climate_fill_inmet()` evaluation mode.
#' @param target_var Character. Used in the table caption.
#' @param lang Language code (`"en"`, `"pt"`, `"es"`).
#' @keywords internal
#' @noRd
.dt_metrics_table <- function(metrics, target_var = "", lang = "en") {

  col_labels <- c(
    station_id     = .i18n("station_col",   lang),
    variable       = "Variable",
    rmse           = "RMSE",
    mae            = "MAE",
    r_squared      = "R²",
    smape          = "sMAPE (%)",
    slope_bias     = "Slope bias",
    n_obs          = .i18n("n_obs_col",     lang),
    gap_percentage = .i18n("gap_pct_col",   lang),
    gap_mechanism  = .i18n("mechanism_col", lang)
  )

  keep_cols    <- intersect(names(col_labels), names(metrics))
  display_data <- as.data.frame(metrics[, keep_cols])
  names(display_data) <- col_labels[keep_cols]

  rmse_col <- "RMSE"
  rmse_idx <- which(names(display_data) == rmse_col) - 1L
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
        c("RMSE", "MAE", "R²", "sMAPE (%)", "Slope bias"),
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
      columns    = intersect("R²", names(display_data)),
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
#'
#' @param prod_metrics Tibble with imputation counts.
#' @param lang Language code.
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

  pct_col <- .i18n("imputed_pct", lang)
  pct_idx <- which(names(display_data) == pct_col) - 1L
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
# plot_climate_fill.R
# Visualisation helpers for gap-filling results — climasus4r package
#
# Exported functions
#   plot_climate_fill()          — main dispatcher
#
# Internal helpers (@noRd)
#   .plot_fill_series()          — time-series panel (production output)
#   .plot_fill_eval()            — evaluation dashboard
#   .build_series_plotly()       — plotly time-series builder
#   .build_series_ggplot()       — ggplot2 time-series builder
#   .build_eval_plotly()         — plotly eval dashboard builder
#   .build_eval_ggplot()         — ggplot2 eval dashboard builder
#   .dt_metrics_table()          — DT table of per-station eval metrics
#   .dt_production_table()       — DT table of per-station imputation summary
#   .maybe_downsample()          — smart downsampling for large series
#   .i18n()                      — i18n string lookup
#   .validate_min_points()       — guard for sparse-station plots
# =============================================================================

# -----------------------------------------------------------------------------
# IMPORTS
# -----------------------------------------------------------------------------
#' @importFrom dplyr filter select mutate arrange left_join full_join
#'   bind_rows group_by summarise n all_of relocate between
#' @importFrom tidyr pivot_longer
#' @importFrom plotly plot_ly add_trace add_segments layout subplot
#' @importFrom ggplot2 ggplot aes geom_line geom_point scale_colour_manual
#'   scale_x_datetime labs theme_bw theme facet_wrap geom_histogram
#'   geom_vline geom_abline geom_smooth element_text
#' @importFrom ggsci pal_npg pal_aaas pal_nejm pal_lancet pal_jama pal_bmj pal_jco
#'   pal_frontiers pal_gsea pal_uchicago pal_primer pal_atlassian pal_observable
#'   pal_d3 pal_gephi pal_igv pal_cosmic pal_locuszoom pal_ucscgb
#'   pal_startrek pal_tron pal_futurama pal_rickandmorty pal_simpsons pal_flatui
#'   pal_bs5 pal_material pal_tw3
#' @importFrom patchwork wrap_plots plot_annotation
#' @importFrom DT datatable formatRound formatStyle styleInterval
#' @importFrom rlang .data sym
#' @importFrom cli cli_abort cli_alert_info cli_alert_warning cli_h1 cli_text
#' @importFrom htmltools tags
NULL

# =============================================================================
#  SCIENTIFIC COLOR PALETTE (ggsci)
# =============================================================================

#' Scientific color palette for gap-filling visualizations
#'
#' @param palette_name Name of the ggsci palette
#' @keywords internal
#' @noRd
.get_scientific_palette <- function(palette_name = "npg") {
  
  # Validate palette name
  valid_palettes <- c(
    # Scientific journals
    "npg", "aaas", "nejm", "lancet", "jama", "bmj", "jco", "frontiers", "gsea",
    # Professional/Academic
    "uchicago", "primer", "atlassian", "observable",
    # Specialized
    "d3", "gephi", "igv", "cosmic", "locuszoom", "ucscgb",
    # Creative
    "startrek", "tron", "futurama", "rickandmorty", "simpsons", "flatui",
    # Design systems
    "bs5", "material", "tw3"
  )
  
  if (!palette_name %in% valid_palettes) {
    cli::cli_warn("Palette '{palette_name}' not recognized. Using 'npg' as default.")
    palette_name <- "npg"
  }
  
  # Get the base palette function from ggsci
  pal_func <- switch(
    palette_name,
    # Scientific journals
    "npg"        = ggsci::pal_npg(),
    "aaas"       = ggsci::pal_aaas(),
    "nejm"       = ggsci::pal_nejm(),
    "lancet"     = ggsci::pal_lancet(),
    "jama"       = ggsci::pal_jama(),
    "bmj"        = ggsci::pal_bmj(),
    "jco"        = ggsci::pal_jco(),
    "frontiers"  = ggsci::pal_frontiers(),
    "gsea"       = ggsci::pal_gsea(),
    # Professional/Academic
    "uchicago"   = ggsci::pal_uchicago(),
    "primer"     = ggsci::pal_primer(),
    "atlassian"  = ggsci::pal_atlassian(),
    "observable" = ggsci::pal_observable(),
    # Specialized
    "d3"         = ggsci::pal_d3(),
    "gephi"      = ggsci::pal_gephi(),
    "igv"        = ggsci::pal_igv(),
    "cosmic"     = ggsci::pal_cosmic(),
    "locuszoom"  = ggsci::pal_locuszoom(),
    "ucscgb"     = ggsci::pal_ucscgb(),
    # Creative
    "startrek"   = ggsci::pal_startrek(),
    "tron"       = ggsci::pal_tron(),
    "futurama"   = ggsci::pal_futurama(),
    "rickandmorty" = ggsci::pal_rickandmorty(),
    "simpsons"   = ggsci::pal_simpsons(),
    "flatui"     = ggsci::pal_flatui(),
    # Design systems
    "bs5"        = ggsci::pal_bs5(),
    "material"   = ggsci::pal_material(),
    "tw3"        = ggsci::pal_tw3(),
    ggsci::pal_npg() # default fallback
  )
  
  # Extract colors (safely handling different palette sizes)
  colors <- pal_func(10)
  
  # Map colors to semantic roles based on the chosen palette
  # We try to maintain consistent semantic meaning (e.g., blue for observed, red/orange for imputed)
  # This generic mapping works well for most palettes
  list(
    original_observed = colors[2],  # Usually a cool color (blue/teal)
    filled_imputed    = colors[1],  # Usually a warm color (red/orange)
    imputed_marker    = colors[1],  # Same as imputed line
    imputed_line      = colors[1],  # Warm color for filled data
    scatter           = colors[3],  # Neutral/secondary color
    reference_line    = if (palette_name %in% c("startrek", "tron", "futurama", "rickandmorty", "simpsons")) 
                          "#333333" else colors[8],  # Dark color for reference
    residual_hist     = colors[4],  # Tertiary color
    residual_point    = colors[5],  # Quaternary color
    zero_line         = "#999999",  # Grey (zero reference)
    grid_line         = "#EEEEEE"   # Light grey (panel background)
  )
}

# =============================================================================
#  i18n STRINGS (complete with all keys)
# =============================================================================

#' @keywords internal
#' @noRd
.i18n_strings <- list(

  en = list(
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
    residual_axis   = "Residual (observed − predicted)",
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
    info_eval       = "Evaluation summary for '{v}': n = {n} | RMSE = {rmse} | MAE = {mae} | R² = {r2}",
    
    # Verbose
    verbose_period  = "  Period  : {from} to {to}",
    verbose_nas     = "  NAs before / after : {na_before} / {na_after}",
    verbose_pts     = "  Points by station :",
    downsample_warn = "Large dataset ({n} rows): line traces downsampled to {s} rows (markers unchanged)."
  ),

  pt = list(
    # Time-series
    title_series    = "Preenchimento de Falhas",
    station_label   = "Estação",
    gaps_filled     = "falhas preenchidas",
    original        = "Observado (com falhas)",
    filled_series   = "Série imputada",
    imputed_pts     = "Valores imputados",
    date_time       = "Data / Hora",
    
    # Evaluation
    obs_vs_pred     = "Observado vs Predito",
    observed        = "Valor observado",
    predicted       = "Valor predito",
    residuals       = "Resíduos",
    res_dist        = "Distribuição dos resíduos",
    res_over_time   = "Resíduos ao longo do tempo",
    res_vs_obs      = "Resíduos vs observado",
    residual_axis   = "Resíduo (observado − predito)",
    count           = "Contagem",
    eval_dashboard  = "Painel de avaliação do modelo",
    station_obs     = "Observações por estação",
    one_to_one      = "Linha de referência 1:1",
    res_over_t      = "Resíduo ao longo do tempo",
    
    # Tables
    imputation_summ = "Resumo de imputação por estação",
    perf_metrics    = "Métricas de desempenho por estação",
    total_obs       = "Total de observações",
    gaps_filled_col = "Falhas preenchidas",
    imputed_pct     = "Imputado (%)",
    station_col     = "Estação",
    n_obs_col       = "N observações",
    gap_pct_col     = "Proporção de falhas (%)",
    mechanism_col   = "Mecanismo da falha",
    
    # Warnings & Info
    warn_few_pts    = "Estação '{st}' tem apenas {n} pontos válidos (< {min_pts} mínimo). Ignorando.",
    warn_no_flag    = "Coluna de flag '{fc}' não encontrada; imputados derivados dos NAs originais.",
    info_summary    = "Resumo para '{v}': {nf} falhas preenchidas ({pct}%)",
    info_eval       = "Avaliação para '{v}': n = {n} | RMSE = {rmse} | MAE = {mae} | R² = {r2}",
    
    # Verbose
    verbose_period  = "  Período : {from} até {to}",
    verbose_nas     = "  NAs antes / depois : {na_before} / {na_after}",
    verbose_pts     = "  Pontos por estação :",
    downsample_warn = "Dataset grande ({n} linhas): linhas amostradas para {s} (marcadores inalterados)."
  ),

  es = list(
    # Time-series
    title_series    = "Relleno de Brechas",
    station_label   = "Estación",
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
    res_dist        = "Distribución de residuos",
    res_over_time   = "Residuos en el tiempo",
    res_vs_obs      = "Residuos vs observado",
    residual_axis   = "Residuo (observado − predicho)",
    count           = "Conteo",
    eval_dashboard  = "Panel de evaluación del modelo",
    station_obs     = "Observaciones por estación",
    one_to_one      = "Línea de referencia 1:1",
    res_over_t      = "Residuo en el tiempo",
    
    # Tables
    imputation_summ = "Resumen de imputación por estación",
    perf_metrics    = "Métricas de rendimiento por estación",
    total_obs       = "Total observaciones",
    gaps_filled_col = "Brechas completadas",
    imputed_pct     = "Imputado (%)",
    station_col     = "Estación",
    n_obs_col       = "N observaciones",
    gap_pct_col     = "Proporción de brechas (%)",
    mechanism_col   = "Mecanismo de brecha",
    
    # Warnings & Info
    warn_few_pts    = "Estación '{st}' tiene solo {n} puntos válidos (< {min_pts} mínimo). Omitiendo.",
    warn_no_flag    = "Columna flag '{fc}' no encontrada; imputados derivados de NAs originales.",
    info_summary    = "Resumen para '{v}': {nf} brechas completadas ({pct}%)",
    info_eval       = "Evaluación para '{v}': n = {n} | RMSE = {rmse} | MAE = {mae} | R² = {r2}",
    
    # Verbose
    verbose_period  = "  Período : {from} hasta {to}",
    verbose_nas     = "  NAs antes / después : {na_before} / {na_after}",
    verbose_pts     = "  Puntos por estación :",
    downsample_warn = "Dataset grande ({n} filas): líneas muestreadas a {s} filas (marcadores sin cambio)."
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
  for (nm in names(dots)) {
    str <- gsub(paste0("\\{", nm, "\\}"), as.character(dots[[nm]]), str)
  }
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

# =============================================================================
#  DOWNSAMPLING FOR LARGE SERIES
# =============================================================================

#' Smart downsampling — keeps every nth row for line traces only.
#'
#' Imputed marker points are **never** downsampled so users can always inspect
#' every filled gap precisely.
#'
#' @param df Data frame sorted by date.
#' @param n_threshold Integer.  Row count above which downsampling triggers.
#' @param target_n Integer.  Approximate row count after thinning.
#' @param verbose Logical.
#' @param lang Language code.
#' @return List: `$line_df` (thinned or original) and `$use_gl` logical flag
#'   indicating whether WebGL (`scattergl`) should be used.
#' @keywords internal
#' @noRd
.maybe_downsample <- function(df, n_threshold = 5000, target_n = 3000,
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