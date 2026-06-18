# =============================================================================
# sus_data_quality_report.R
# Pipeline-aware data quality report for climasus_df objects.
# =============================================================================

# -- NSE variable declarations ------------------------------------------------
utils::globalVariables(c(
  "column", "n_missing", "pct_missing", "quality_flag",
  "section", "metric", "value", "status"
))


# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' Pipeline-aware data quality report for health data
#'
#' Reads `sus_meta` processing history to identify which climasus4r pipeline
#' functions were applied, then produces a section-by-section quality
#' assessment covering completeness, demographic distributions, date
#' validity, ICD-10 codes, geographic coverage, and derived variables.
#' Returns an overall quality score (0-100) and supports four output formats.
#'
#' @param df A `climasus_df` or plain `data.frame`. Arrow objects are
#'   materialised automatically.
#' @param output_format Character. One of `"console"` (default), `"gt"`,
#'   `"markdown"`, or `"html"`. The `"gt"` format returns a `gt_tbl` object
#'   suitable for Quarto / R Markdown documents.
#' @param output_file Character. File path for `"markdown"` and `"html"`
#'   formats. Defaults to a timestamped filename in the working directory.
#' @param check_icd Logical. Include ICD-10 quality section. Default `TRUE`.
#' @param check_dates Logical. Include date validation section. Default `TRUE`.
#' @param top_n Integer. Rows shown in frequency tables. Default `10`.
#' @param lang Character. Output language: `"pt"` (default), `"en"`, `"es"`.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return Invisibly returns a named list of quality metrics (always
#'   available regardless of `output_format`):
#'   \describe{
#'     \item{`$meta`}{Pipeline metadata from `sus_meta`}
#'     \item{`$pipeline`}{Functions detected in processing history}
#'     \item{`$overview`}{Row/column counts, duplicates, column types}
#'     \item{`$missing`}{Per-column missing values with quality flags}
#'     \item{`$demographics`}{Frequency tables for sex, race, age, education}
#'     \item{`$dates`}{Date range and validity per date column}
#'     \item{`$icd`}{ICD-10 frequency and validity}
#'     \item{`$geographic`}{Municipality and state coverage}
#'     \item{`$derived`}{Presence of expected derived variables}
#'     \item{`$score`}{Overall quality score 0-100}
#'   }
#'
#' @examples
#' \dontrun{
#' # Console report (default)
#' sus_data_quality_report(df, lang = "pt")
#'
#' # gt table (for Quarto / R Markdown)
#' sus_data_quality_report(df, output_format = "gt", lang = "en")
#'
#' # Save complete Markdown report
#' sus_data_quality_report(df, output_format = "markdown",
#'                         output_file = "reports/quality.md", lang = "pt")
#'
#' # Save HTML report
#' sus_data_quality_report(df, output_format = "html",
#'                         output_file = "reports/quality.html")
#' }
#'
#' @export
#' @importFrom dplyr filter mutate select arrange desc
#' @importFrom cli cli_h1 cli_h2 cli_h3 cli_rule cli_text cli_alert_info
#'   cli_alert_success cli_alert_warning cli_alert_danger cli_abort cli_bullets
#' @importFrom rlang check_installed .data
#' @importFrom stats median
#' @importFrom utils head
sus_data_quality_report <- function(
    df,
    output_format = "console",
    output_file   = NULL,
    check_icd     = TRUE,
    check_dates   = TRUE,
    top_n         = 10,
    lang          = "pt",
    verbose       = TRUE
) {

  # -- 1. Materialise Arrow lazy inputs ---------------------------------------
  if (inherits(df, c("arrow_dplyr_query", "Dataset", "ArrowTabular", "Table"))) {
    if (verbose) cli::cli_alert_info(.qrl("materialising", lang))
    meta_backup <- tryCatch(sus_meta(df), error = function(e) list())
    df <- dplyr::collect(df)
    if (length(meta_backup) > 0L) df <- new_climasus_df(df, meta_backup)
  }

  # -- 2. Input validation ---------------------------------------------------
  if (!is.data.frame(df)) {
    cli::cli_abort(c(
      "Input {.arg df} must be a {.cls data.frame} or collectable Arrow object.",
      "i" = "Use {.fn sus_data_import} to begin the pipeline."
    ))
  }

  if (!output_format %in% c("console", "gt", "markdown", "html")) {
    cli::cli_abort(
      "{.arg output_format} must be one of: {.val console}, {.val gt},
       {.val markdown}, {.val html}."
    )
  }

  if (!lang %in% c("pt", "en", "es")) {
    cli::cli_alert_warning(
      "Language {.val {lang}} not supported. Using {.val pt}."
    )
    lang <- "pt"
  }

  if (output_format %in% c("markdown", "html") && is.null(output_file)) {
    ts  <- format(Sys.time(), "%Y%m%d_%H%M%S")
    ext <- if (output_format == "html") ".html" else ".md"
    output_file <- paste0("dq_report_", ts, ext)
  }

  # -- 3. Extract sus_meta ---------------------------------------------------
  is_climasus <- inherits(df, "climasus_df")
  meta <- if (is_climasus) {
    tryCatch(sus_meta(df), error = function(e) list())
  } else {
    list()
  }

  `%||%` <- function(x, y) if (is.null(x)) y else x

  stage   <- meta$stage   %||% "unknown"
  system  <- meta$system  %||% "unknown"
  type    <- meta$type    %||% "unknown"
  backend <- meta$backend %||% "tibble"
  history <- meta$history %||% character(0)

  # -- 4. Compute all quality metrics ----------------------------------------
  if (verbose) cli::cli_alert_info(.qrl("computing", lang))

  report <- list(
    meta         = list(stage = stage, system = system, type = type,
                        backend = backend, n_history = length(history),
                        generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    pipeline     = .qr_parse_history(history, stage),
    overview     = .qr_overview(df),
    missing      = .qr_missing(df),
    demographics = .qr_demographics(df),
    dates        = if (check_dates) .qr_dates(df) else list(),
    icd          = if (check_icd)   .qr_icd(df, top_n) else list(),
    geographic   = .qr_geographic(df, top_n),
    derived      = .qr_derived(df, stage),
    score        = NULL
  )
  report$score <- .qr_score(report)

  # -- 5. Render output ------------------------------------------------------
  switch(
    output_format,
    console  = .qr_print_console(report, history, lang),
    gt       = {
      rlang::check_installed("gt", reason = "for gt quality report output")
      return(.qr_render_gt(report, history, lang))
    },
    markdown = {
      .qr_write_markdown(report, history, output_file, lang)
      if (verbose) cli::cli_alert_success(.qrl("saved_to", lang, output_file))
    },
    html = {
      .qr_write_html(report, history, output_file, lang)
      if (verbose) cli::cli_alert_success(.qrl("saved_to", lang, output_file))
    }
  )

  invisible(report)
}


# =============================================================================
# INTERNAL: COLUMN DETECTOR
# =============================================================================

#' @noRd
.qr_col <- function(df, patterns) {
  for (p in patterns) {
    if (p %in% names(df)) return(p)
  }
  NULL
}


# =============================================================================
# INTERNAL: PIPELINE HISTORY PARSER
# =============================================================================

# Map stage names to the function that advances TO that stage
.qr_stage_fn_map <- c(
  import     = "sus_data_import()",
  read       = "sus_data_read()",
  clean      = "sus_data_clean_encoding()",
  stand      = "sus_data_standardize()",
  filter_cid = "sus_data_filter_cid()",
  filter_demo = "sus_data_filter_demographics()",
  derive     = "sus_data_create_variables()",
  aggregate  = "sus_data_aggregate()",
  spatial    = "sus_join_spatial()",
  census     = "sus_socio_add_census()",
  climate    = "sus_climate_*()"
)

#' @noRd
.qr_parse_history <- function(history, current_stage) {

  stage_order <- c("import", "clean", "stand", "filter_cid", "filter_demo",
                   "derive", "aggregate", "spatial", "census", "climate")

  # Stages reached = all stages up to and including current_stage
  stage_idx <- match(current_stage, stage_order)
  if (is.na(stage_idx)) stage_idx <- 0L

  stages_reached <- if (stage_idx > 0L) stage_order[seq_len(stage_idx)] else character(0)

  # Map each reached stage to its function name
  functions_applied <- unname(.qr_stage_fn_map[stages_reached])
  functions_applied <- functions_applied[!is.na(functions_applied)]

  history_functions <- unique(unlist(lapply(history, .qr_detect_history_functions)))
  functions_applied <- unique(c(functions_applied, history_functions))

  # Parse history entries: "[YYYY-MM-DD HH:MM:SS] message"
  parsed_steps <- lapply(history, function(entry) {
    ts  <- sub("^\\[(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\\].*", "\\1", entry)
    msg <- sub("^\\[\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\]\\s*", "", entry)
    list(timestamp = ts, message = msg)
  })

  list(
    stages_reached    = stages_reached,
    current_stage     = current_stage,
    functions_applied = functions_applied,
    n_history_entries = length(history),
    parsed_steps      = parsed_steps
  )
}

#' @noRd
.qr_detect_history_functions <- function(entry) {
  msg <- sub("^\\[\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\]\\s*", "", entry)
  patterns <- list(
    "sus_data_import()" = "Imported datasus|Imported DATASUS",
    "sus_data_read()" = "Read .*files|Collected Arrow Dataset",
    "sus_data_clean_encoding()" = "Cleaned character encoding",
    "sus_data_standardize()" = "Standardized column names and types",
    "sus_data_filter_cid()" = "Filtered by disease group|CID|ICD",
    "sus_data_filter_demographics()" = "Demographic filters|Filtered demographics|City:",
    "sus_data_create_variables()" = "Derived variables created|Create variables",
    "sus_data_aggregate()" = "Temporal Data aggregated|Data aggregated",
    "sus_join_spatial()" = "Spatial Data aggregated|Spatial join",
    "sus_socio_add_census()" = "Added census data",
    "sus_climate_aggregate()" = "Climate aggregation",
    "sus_climate_inmet()" = "INMET data imported"
  )

  hits <- names(patterns)[vapply(patterns, function(rx) {
    grepl(rx, msg, ignore.case = TRUE)
  }, logical(1L))]
  unname(hits)
}


# =============================================================================
# INTERNAL: METRIC COMPUTERS
# =============================================================================

#' @noRd
.qr_overview <- function(df) {
  col_types   <- sapply(df, function(x) class(x)[1L])
  type_counts <- as.list(table(col_types))
  n_dup       <- sum(duplicated(df))
  list(
    n_rows    = nrow(df),
    n_cols    = ncol(df),
    n_dup     = n_dup,
    pct_dup   = round(100 * n_dup / max(nrow(df), 1L), 2),
    col_types = type_counts
  )
}

#' @noRd
.qr_missing <- function(df) {
  n_total <- nrow(df)

  miss <- data.frame(
    column      = names(df),
    n_missing   = vapply(df, function(x) sum(is.na(x)), integer(1L)),
    pct_missing = vapply(df, function(x) {
      round(100 * sum(is.na(x)) / max(n_total, 1L), 1)
    }, numeric(1L)),
    row.names   = NULL,
    stringsAsFactors = FALSE
  )

  miss$quality_flag <- ifelse(miss$pct_missing < 5,  "ok",
                       ifelse(miss$pct_missing < 20, "warn", "critical"))

  miss_only <- miss[miss$n_missing > 0, ]

  list(
    by_column           = miss_only[order(-miss_only$n_missing), ],
    n_complete_cols     = sum(miss$n_missing == 0L),
    n_warn_cols         = sum(miss$quality_flag == "warn"),
    n_critical_cols     = sum(miss$quality_flag == "critical"),
    completeness_score  = {
      nok  <- sum(miss$quality_flag == "ok")
      nwrn <- sum(miss$quality_flag == "warn")
      nc   <- nrow(miss)
      if (nc == 0L) 100 else round(100 * (nok + 0.5 * nwrn) / nc, 1)
    },
    overall_pct_missing = round(
      100 * sum(miss$n_missing) / max(n_total * ncol(df), 1L), 1
    )
  )
}

#' @noRd
.qr_demographics <- function(df) {
  out <- list()
  n   <- nrow(df)

  .freq_tbl <- function(col) {
    tbl         <- as.data.frame(table(df[[col]], useNA = "always"),
                                 stringsAsFactors = FALSE)
    names(tbl)  <- c("category", "count")
    tbl$pct     <- round(100 * tbl$count / n, 1)
    tbl
  }

  sex_col <- .qr_col(df, c("sex", "sexo", "SEXO", "CS_SEXO"))
  if (!is.null(sex_col)) {
    out$sex <- list(
      col         = sex_col,
      freq        = .freq_tbl(sex_col),
      pct_missing = round(100 * sum(is.na(df[[sex_col]])) / n, 1)
    )
  }

  race_col <- .qr_col(df, c("race", "raca", "raza", "RACACOR", "RACA_COR", "CS_RACA"))
  if (!is.null(race_col)) {
    out$race <- list(
      col         = race_col,
      freq        = .freq_tbl(race_col),
      pct_missing = round(100 * sum(is.na(df[[race_col]])) / n, 1)
    )
  }

  age_col <- .qr_col(df, c("age_years", "idade", "edad", "NU_IDADE_N", "IDADE"))
  if (!is.null(age_col)) {
    vals <- df[[age_col]][!is.na(df[[age_col]])]
    out$age <- list(
      col           = age_col,
      n_missing     = sum(is.na(df[[age_col]])),
      pct_missing   = round(100 * sum(is.na(df[[age_col]])) / n, 1),
      min           = if (length(vals) > 0L) min(vals) else NA_real_,
      max           = if (length(vals) > 0L) max(vals) else NA_real_,
      mean          = if (length(vals) > 0L) round(mean(vals), 1) else NA_real_,
      median        = if (length(vals) > 0L) stats::median(vals) else NA_real_,
      n_implausible = sum(vals < 0 | vals > 130, na.rm = TRUE)
    )
  }

  edu_col <- .qr_col(df, c("education_level", "education", "escolaridade",
                             "escolaridad", "ESC", "ESC2010", "CS_ESCOL_N"))
  if (!is.null(edu_col)) {
    out$education <- list(
      col         = edu_col,
      freq        = .freq_tbl(edu_col),
      pct_missing = round(100 * sum(is.na(df[[edu_col]])) / n, 1)
    )
  }

  crisk_col <- .qr_col(df, c("climate_risk_group", "grupo_risco_climatico"))
  if (!is.null(crisk_col)) {
    out$climate_risk <- list(
      col         = crisk_col,
      freq        = .freq_tbl(crisk_col),
      pct_missing = round(100 * sum(is.na(df[[crisk_col]])) / n, 1)
    )
  }

  out
}

#' @noRd
.qr_dates <- function(df) {
  date_cols <- names(df)[vapply(df, function(x) {
    inherits(x, c("Date", "POSIXct", "POSIXlt"))
  }, logical(1L))]

  if (length(date_cols) == 0L) return(list())
  today <- Sys.Date()
  out   <- list()

  for (col in date_cols) {
    dates <- tryCatch(as.Date(df[[col]]), error = function(e) NULL)
    if (is.null(dates)) next
    valid <- dates[!is.na(dates)]
    out[[col]] <- list(
      n_total    = length(dates),
      n_missing  = sum(is.na(dates)),
      n_future   = sum(dates > today,                na.rm = TRUE),
      n_pre1900  = sum(dates < as.Date("1900-01-01"), na.rm = TRUE),
      date_min   = if (length(valid) > 0L) min(valid) else NA,
      date_max   = if (length(valid) > 0L) max(valid) else NA
    )
  }
  out
}

#' @noRd
.qr_icd <- function(df, top_n) {
  icd_col <- .qr_col(df, c(
    "underlying_cause", "causa_basica", "CAUSABAS",
    "primary_diagnosis", "diagnostico_principal", "DIAG_PRINC",
    "PA_CIDPRI", "notification_icd", "cid_principal", "cid10"
  ))
  if (is.null(icd_col)) return(list())

  codes   <- df[[icd_col]]
  n_total <- length(codes)

  is_valid <- grepl("^[A-Za-z][0-9]{2}", codes) & !is.na(codes)

  freq        <- as.data.frame(table(codes, useNA = "always"), stringsAsFactors = FALSE)
  names(freq) <- c("code", "count")
  freq        <- freq[order(-freq$count), ]
  freq$pct    <- round(100 * freq$count / n_total, 2)

  valid_codes <- codes[is_valid]
  chap_freq   <- if (length(valid_codes) > 0L) {
    chapters <- substr(valid_codes, 1L, 1L)
    cf       <- as.data.frame(table(chapters), stringsAsFactors = FALSE)
    cf[order(-cf$Freq), ]
  } else {
    data.frame()
  }

  list(
    col         = icd_col,
    n_unique    = length(unique(codes[!is.na(codes)])),
    n_missing   = sum(is.na(codes)),
    pct_missing = round(100 * sum(is.na(codes)) / n_total, 1),
    pct_valid   = round(100 * sum(is_valid) / n_total, 1),
    top_codes   = utils::head(freq, top_n),
    chapters    = chap_freq
  )
}

#' @noRd
.qr_geographic <- function(df, top_n) {
  out <- list()

  muni_col <- .qr_col(df, c(
    "codigo_municipio_residencia", "residence_municipality_code",
    "codigo_municipio_ocorrencia", "occurrence_municipality_code",
    "codigo_municipio_notificacao", "notification_municipality_code",
    "codigo_municipio_nascimento", "birth_municipality_code",
    "codigo_municipio_paciente", "patient_municipality_code",
    "codigo_municipio", "municipality_code", "code_muni", "code_muni_7",
    "CODMUNRES", "MUNI_RES", "municipio_residencia", "cod_municipio"
  ))
  if (!is.null(muni_col)) {
    freq        <- as.data.frame(table(df[[muni_col]], useNA = "ifany"),
                                 stringsAsFactors = FALSE)
    names(freq) <- c("code", "count")
    freq        <- freq[order(-freq$count), ]
    freq$pct    <- round(100 * freq$count / nrow(df), 2)
    out$municipalities <- list(
      col       = muni_col,
      n_unique  = length(unique(df[[muni_col]][!is.na(df[[muni_col]])])),
      n_missing = sum(is.na(df[[muni_col]])),
      top       = utils::head(freq, top_n)
    )
  }

  uf_col <- .qr_col(df, c(
    "uf_residencia", "residence_uf", "uf_ocorrencia", "occurrence_uf",
    "uf_notificacao", "notification_uf", "manager_uf", "uf_gestor",
    "UF_ZI", "SG_UF_NOT", "CODUFRES", "notification_uf"
  ))
  if (!is.null(uf_col)) {
    freq_uf        <- as.data.frame(table(df[[uf_col]], useNA = "ifany"),
                                    stringsAsFactors = FALSE)
    names(freq_uf) <- c("uf", "count")
    freq_uf        <- freq_uf[order(-freq_uf$count), ]
    freq_uf$pct    <- round(100 * freq_uf$count / nrow(df), 2)
    out$states <- list(
      col      = uf_col,
      n_unique = length(unique(df[[uf_col]][!is.na(df[[uf_col]])])),
      top      = utils::head(freq_uf, top_n)
    )
  }

  out
}

#' @noRd
.qr_derived <- function(df, stage) {
  expected <- list(
    age_group        = c("age_group", "grupo_idade", "grupo_edad"),
    ibge_age_group   = c("ibge_age_group", "faixa_etaria_ibge", "grupo_edad_ibge"),
    sex_label        = c("sex", "sexo"),
    climate_risk_grp = c("climate_risk_group", "grupo_risco_climatico",
                         "grupo_riesgo_climatico"),
    month            = c("month", "mes"),
    year             = c("year", "ano"),
    quarter          = c("quarter", "trimestre"),
    epi_week         = c("epidemiological_week", "semana_epidemiologica"),
    season           = c("astronomical_season", "estacao_astronomica",
                         "estacion_astronomica", "climatic_season",
                         "estacao_climatica", "estacion_climatica"),
    dry_rainy        = c("dry_rainy_season", "estacao_seca_chuvosa",
                         "estacion_seca_lluviosa")
  )

  presence <- lapply(expected, function(patterns) {
    found <- .qr_col(df, patterns)
    list(present = !is.null(found), col = found)
  })

  list(
    stage_at_least_derive = tryCatch(
      is_stage_at_least(stage, "derive"), error = function(e) FALSE
    ),
    variables = presence
  )
}

#' @noRd
.qr_score <- function(report) {
  scores  <- numeric(0)
  weights <- numeric(0)

  cs <- report$missing$completeness_score
  if (!is.null(cs) && !is.na(cs)) {
    scores  <- c(scores,  cs)
    weights <- c(weights, 0.40)
  }

  demo_miss <- vapply(report$demographics, function(d) {
    d$pct_missing %||% 0
  }, numeric(1L))
  if (length(demo_miss) > 0L) {
    scores  <- c(scores,  max(100 - mean(demo_miss), 0))
    weights <- c(weights, 0.20)
  }

  pv <- report$icd$pct_valid
  if (!is.null(pv) && !is.na(pv)) {
    scores  <- c(scores,  pv)
    weights <- c(weights, 0.20)
  }

  if (length(report$dates) > 0L) {
    n_issues <- sum(vapply(report$dates, function(d) d$n_future + d$n_pre1900, integer(1L)))
    n_tot    <- sum(vapply(report$dates, function(d) d$n_total,                integer(1L)))
    if (n_tot > 0L) {
      scores  <- c(scores,  max(100 * (1 - n_issues / n_tot), 0))
      weights <- c(weights, 0.20)
    }
  }

  if (length(scores) == 0L) return(NA_real_)
  round(sum(scores * weights) / sum(weights), 1)
}

`%||%` <- function(x, y) if (is.null(x)) y else x


# =============================================================================
# INTERNAL: CONSOLE OUTPUT
# =============================================================================

#' @noRd
.qr_flag_icon <- function(flag) {
  switch(flag,
    ok       = cli::col_green("[OK]"),
    warn     = cli::col_yellow("[WARN]"),
    critical = cli::col_red("[CRIT]"),
    cli::col_cyan("[INFO]")
  )
}

#' @noRd
.qr_score_icon <- function(score) {
  if (is.na(score))  return(cli::col_cyan("  ?  "))
  if (score >= 80)   return(cli::col_green(sprintf(" %4.1f", score)))
  if (score >= 60)   return(cli::col_yellow(sprintf(" %4.1f", score)))
  cli::col_red(sprintf(" %4.1f", score))
}

#' @noRd
.qr_print_console <- function(report, history, lang) {

  meta <- report$meta
  pipe <- report$pipeline
  ov   <- report$overview
  miss <- report$missing
  sc   <- report$score

  # Header
  cli::cli_rule(left = .qrl("report_title", lang))
  cli::cli_text(.qrl("generated_at", lang,
                     format(Sys.time(), "%Y-%m-%d %H:%M")))
  cli::cli_text("")

  # Overall score
  score_str <- if (is.na(sc)) "N/A" else paste0(round(sc, 1), " / 100")
  flag      <- if (is.na(sc)) "info"
               else if (sc >= 80) "ok"
               else if (sc >= 60) "warn"
               else "critical"
  cli::cli_text(paste0(.qr_flag_icon(flag), "  ",
    .qrl("quality_score", lang), ": ", cli::style_bold(score_str)))
  cli::cli_text("")

  # ---- Section 0: Pipeline audit --------------------------------------------------------------------------------------------
  cli::cli_h2(.qrl("sec_pipeline", lang))
  cli::cli_text(paste0(.qrl("current_stage", lang), ": ",
    cli::style_bold(meta$stage), "  |  ",
    .qrl("system_label",  lang), ": ", cli::style_bold(meta$system), "  |  ",
    .qrl("type_label",    lang), ": ", cli::style_bold(meta$type)))
  cli::cli_text("")

  if (length(pipe$functions_applied) > 0L) {
    cli::cli_text(.qrl("fns_applied", lang))
    for (fn in pipe$functions_applied) cli::cli_text(paste0("  ", cli::col_green("v"), "  ", fn))
  } else {
    cli::cli_alert_warning(.qrl("no_history", lang))
  }

  if (length(pipe$stages_reached) > 0L) {
    all_stages <- c("import", "clean", "stand", "filter_cid", "filter_demo",
                    "derive", "aggregate", "spatial", "census", "climate")
    remaining  <- setdiff(all_stages, pipe$stages_reached)
    if (length(remaining) > 0L) {
      cli::cli_text("")
      cli::cli_text(.qrl("stages_pending", lang))
      for (s in remaining) cli::cli_text("  {.dim -}  {s}")
    }
  }

  if (length(history) > 0L) {
    cli::cli_text("")
    cli::cli_text(paste0(.qrl("history_entries", lang), ": ",
                         cli::style_bold(length(history))))
    for (h in history) {
      ts  <- sub("^\\[(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\\].*", "\\1", h)
      msg <- sub("^\\[\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\\]\\s*", "", h)
      cat(sprintf("  [%s]  %s\n", ts, msg))
    }
  }

  # ---- Section 1: Overview ------------------------------------------------------------------------------------------------------
  cli::cli_text("")
  cli::cli_h2(.qrl("sec_overview", lang))
  cli::cli_text(paste0(.qrl("ov_rows",    lang), ": ",
    cli::style_bold(format(ov$n_rows, big.mark = ","))))
  cli::cli_text(paste0(.qrl("ov_cols",    lang), ": ",
    cli::style_bold(ov$n_cols)))
  dup_flag <- if (ov$n_dup == 0L) "ok" else if (ov$pct_dup < 1) "warn" else "critical"
  cli::cli_text(paste0(.qr_flag_icon(dup_flag), "  ",
    .qrl("ov_duplicates", lang), ": ", ov$n_dup,
    " (", ov$pct_dup, "%)"))

  # ---- Section 2: Missing values ------------------------------------------------------------------------------------------
  cli::cli_text("")
  cli::cli_h2(.qrl("sec_missing", lang))
  cli::cli_text(paste0(
    .qrl("miss_complete_cols", lang), ": ",
    cli::style_bold(miss$n_complete_cols), " / ", ov$n_cols, "  |  ",
    .qrl("miss_warn", lang), ": ", miss$n_warn_cols, "  |  ",
    .qrl("miss_critical", lang), ": ", miss$n_critical_cols))
  cli::cli_text(paste0(.qrl("completeness_score", lang), ": ",
    .qr_score_icon(miss$completeness_score), " / 100"))

  if (nrow(miss$by_column) > 0L) {
    cli::cli_text("")
    top_miss <- utils::head(miss$by_column, 10L)
    for (i in seq_len(nrow(top_miss))) {
      row <- top_miss[i, ]
      cli::cli_text(paste0(
        "  ", .qr_flag_icon(row$quality_flag), "  ",
        cli::style_bold(row$column), ": ",
        row$n_missing, " (", row$pct_missing, "%)"))
    }
    if (nrow(miss$by_column) > 10L)
      cli::cli_text(paste0("  ... (", nrow(miss$by_column) - 10L,
                           " ", .qrl("more_cols", lang), ")"))
  } else {
    cli::cli_alert_success(.qrl("no_missing", lang))
  }

  # ---- Section 3: Demographics ----------------------------------------------------------------------------------------------
  cli::cli_text("")
  cli::cli_h2(.qrl("sec_demographics", lang))

  demo_items <- list(
    sex         = .qrl("sex",         lang),
    race        = .qrl("race",        lang),
    age         = .qrl("age",         lang),
    education   = .qrl("education",   lang),
    climate_risk = .qrl("climate_risk", lang)
  )

  if (length(report$demographics) == 0L) {
    cli::cli_alert_warning(.qrl("no_demo_cols", lang))
  }

  for (key in names(demo_items)) {
    d <- report$demographics[[key]]
    if (is.null(d)) next
    label <- demo_items[[key]]

    if (key == "age") {
      imp_flag <- if (d$n_implausible == 0L) "ok" else "warn"
      cli::cli_text(paste0(
        "  ", .qrl("age", lang), " [", d$col, "]: ",
        .qrl("age_range", lang), " ", d$min, "-", d$max,
        "  |  ", .qrl("mean", lang), " ", d$mean,
        "  |  ", .qrl("missing", lang), " ", d$pct_missing, "%",
        "  |  ", .qr_flag_icon(imp_flag), " ",
        .qrl("implausible", lang), " ", d$n_implausible))
    } else {
      miss_flag <- if (d$pct_missing < 5) "ok"
                   else if (d$pct_missing < 20) "warn" else "critical"
      cli::cli_text(paste0(
        "  ", label, " [", d$col, "]: ",
        .qrl("missing", lang), " ", d$pct_missing, "%  ",
        .qr_flag_icon(miss_flag)))
    }
  }

  # ---- Section 4: Dates ------------------------------------------------------------------------------------------------------------
  if (length(report$dates) > 0L) {
    cli::cli_text("")
    cli::cli_h2(.qrl("sec_dates", lang))
    for (col in names(report$dates)) {
      d <- report$dates[[col]]
      issues_flag <- if ((d$n_future + d$n_pre1900) == 0L) "ok" else "warn"
      cli::cli_text(paste0(
        "  ", .qr_flag_icon(issues_flag), "  ", cli::style_bold(col), ": ",
        if (!is.na(d$date_min)) paste0(d$date_min, " - ", d$date_max) else "NA",
        "  |  ", .qrl("future", lang), " ", d$n_future,
        "  |  ", .qrl("pre1900", lang), " ", d$n_pre1900,
        "  |  ", .qrl("missing", lang), " ", d$n_missing))
    }
  }

  # ---- Section 5: ICD codes ----------------------------------------------------------------------------------------------------
  if (length(report$icd) > 0L) {
    cli::cli_text("")
    cli::cli_h2(.qrl("sec_icd", lang))
    icd <- report$icd
    validity_flag <- if (icd$pct_valid >= 95) "ok"
                     else if (icd$pct_valid >= 80) "warn" else "critical"
    cli::cli_text(paste0(
      "  ", .qr_flag_icon(validity_flag), "  [", icd$col, "]  ",
      .qrl("icd_unique", lang), " ", icd$n_unique, "  |  ",
      .qrl("icd_valid", lang),  " ", icd$pct_valid, "%  |  ",
      .qrl("missing", lang),    " ", icd$pct_missing, "%"))
    if (nrow(icd$top_codes) > 0L) {
      cli::cli_text(paste0("  ", .qrl("icd_top", lang), ":"))
      top <- utils::head(icd$top_codes[!is.na(icd$top_codes$code), ], 5L)
      for (i in seq_len(nrow(top)))
        cli::cli_text(paste0("    ", top$code[i], ": ",
                             top$count[i], " (", top$pct[i], "%)"))
    }
  }

  # ---- Section 6: Geographic --------------------------------------------------------------------------------------------------
  geo <- report$geographic
  if (length(geo) > 0L) {
    cli::cli_text("")
    cli::cli_h2(.qrl("sec_geographic", lang))
    if (!is.null(geo$municipalities))
      cli::cli_text(paste0("  ", .qrl("municipalities", lang), ": ",
        geo$municipalities$n_unique, "  |  ",
        .qrl("missing", lang), " ", geo$municipalities$n_missing))
    if (!is.null(geo$states))
      cli::cli_text(paste0("  ", .qrl("states", lang), ": ",
        geo$states$n_unique))
  }

  # ---- Section 7: Derived variables ----------------------------------------------------------------------------------
  drv <- report$derived
  cli::cli_text("")
  cli::cli_h2(.qrl("sec_derived", lang))
  for (key in names(drv$variables)) {
    v    <- drv$variables[[key]]
    icon <- if (v$present) cli::col_green("v") else cli::col_yellow("?")
    lbl  <- if (v$present) paste0(key, " [", v$col, "]") else key
    cli::cli_text(paste0("  ", icon, "  ", lbl))
  }

  cli::cli_rule()
}


# =============================================================================
# INTERNAL: GT OUTPUT
# =============================================================================

#' @noRd
.qr_render_gt <- function(report, history, lang) {

  rlang::check_installed("gt", reason = "for quality report gt output")

  meta <- report$meta
  pipe <- report$pipeline
  ov   <- report$overview
  miss <- report$missing
  icd  <- report$icd
  geo  <- report$geographic
  drv  <- report$derived
  sc   <- report$score

  # Build a flat metrics table
  rows <- list()

  .row <- function(section, metric, value, status = "info") {
    rows[[length(rows) + 1L]] <<- data.frame(
      section = section, metric = metric,
      value   = as.character(value), status = status,
      stringsAsFactors = FALSE
    )
  }

  # Pipeline
  sec_pipe <- .qrl("sec_pipeline", lang)
  .row(sec_pipe, .qrl("current_stage", lang), meta$stage,  "info")
  .row(sec_pipe, .qrl("system_label",  lang), meta$system, "info")
  .row(sec_pipe, .qrl("type_label",    lang), meta$type,   "info")
  .row(sec_pipe, .qrl("history_entries", lang), meta$n_history, "info")
  for (fn in pipe$functions_applied) .row(sec_pipe, fn, "applied", "ok")

  # Overview
  sec_ov <- .qrl("sec_overview", lang)
  .row(sec_ov, .qrl("ov_rows",       lang), format(ov$n_rows, big.mark = ","), "info")
  .row(sec_ov, .qrl("ov_cols",       lang), ov$n_cols, "info")
  dup_st <- if (ov$n_dup == 0L) "ok" else if (ov$pct_dup < 1) "warn" else "critical"
  .row(sec_ov, .qrl("ov_duplicates", lang),
       paste0(ov$n_dup, " (", ov$pct_dup, "%)"), dup_st)

  # Completeness
  sec_miss <- .qrl("sec_missing", lang)
  .row(sec_miss, .qrl("completeness_score",  lang),
       paste0(miss$completeness_score, " / 100"),
       if (miss$completeness_score >= 80) "ok"
       else if (miss$completeness_score >= 60) "warn" else "critical")
  .row(sec_miss, .qrl("miss_complete_cols", lang),
       paste0(miss$n_complete_cols, " / ", ov$n_cols), "info")
  .row(sec_miss, .qrl("miss_warn",     lang), miss$n_warn_cols,     "warn")
  .row(sec_miss, .qrl("miss_critical", lang), miss$n_critical_cols, "critical")

  # Demographics
  sec_demo <- .qrl("sec_demographics", lang)
  demo_keys <- list(sex = "sex", race = "race", age = "age",
                    education = "education", climate_risk = "climate_risk")
  for (key in names(demo_keys)) {
    d <- report$demographics[[key]]
    if (is.null(d)) next
    lbl  <- .qrl(key, lang)
    miss_pct <- if (key == "age") d$pct_missing else d$pct_missing
    st   <- if (miss_pct < 5) "ok" else if (miss_pct < 20) "warn" else "critical"
    .row(sec_demo, paste0(lbl, " [", d$col, "]"),
         paste0(.qrl("missing", lang), " ", miss_pct, "%"), st)
    if (key == "age" && !is.null(d$n_implausible) && d$n_implausible > 0L)
      .row(sec_demo, paste0(lbl, " - ", .qrl("implausible", lang)),
           d$n_implausible, "warn")
  }

  # ICD
  if (length(icd) > 0L) {
    sec_icd <- .qrl("sec_icd", lang)
    vst <- if (icd$pct_valid >= 95) "ok" else if (icd$pct_valid >= 80) "warn" else "critical"
    .row(sec_icd, paste0(.qrl("icd_valid",  lang), " [", icd$col, "]"),
         paste0(icd$pct_valid, "%"), vst)
    .row(sec_icd, .qrl("icd_unique",  lang), icd$n_unique,    "info")
    .row(sec_icd, .qrl("missing",     lang), paste0(icd$pct_missing, "%"),
         if (icd$pct_missing < 5) "ok" else "warn")
  }

  # Geographic
  if (!is.null(geo$municipalities))
    .row(.qrl("sec_geographic", lang), .qrl("municipalities", lang),
         geo$municipalities$n_unique, "info")
  if (!is.null(geo$states))
    .row(.qrl("sec_geographic", lang), .qrl("states", lang),
         geo$states$n_unique, "info")

  # Derived
  sec_drv <- .qrl("sec_derived", lang)
  for (key in names(drv$variables)) {
    v  <- drv$variables[[key]]
    st <- if (v$present) "ok" else "warn"
    .row(sec_drv, key,
         if (v$present) paste0("present [", v$col, "]") else "absent", st)
  }

  # Overall score
  sc_st <- if (is.na(sc)) "info"
           else if (sc >= 80) "ok" else if (sc >= 60) "warn" else "critical"
  .row(.qrl("quality_score", lang), .qrl("quality_score", lang),
       if (is.na(sc)) "N/A" else paste0(sc, " / 100"), sc_st)

  tbl <- do.call(rbind, rows)

  # Status palette
  status_colors <- c(
    ok       = "#27ae60",
    warn     = "#e67e22",
    critical = "#e74c3c",
    info     = "#2980b9"
  )

  gt::gt(tbl, groupname_col = "section", rowname_col = "metric") |>
    gt::tab_header(
      title    = gt::md(paste0("**", .qrl("report_title", lang), "**")),
      subtitle = gt::md(paste0(
        "*", .qrl("current_stage", lang), "*: **", meta$stage, "**  |  ",
        "*", .qrl("system_label",  lang), "*: **", meta$system, "**  |  ",
        format(Sys.time(), "%Y-%m-%d %H:%M")
      ))
    ) |>
    gt::cols_label(metric = .qrl("metric", lang),
                   value  = .qrl("value",  lang),
                   status = .qrl("status", lang)) |>
    gt::data_color(
      columns = "status",
      target_columns = "value",
      method = "factor",
      palette = unname(status_colors[unique(tbl$status)])
    ) |>
    gt::cols_hide("status") |>
    gt::tab_options(
      table.font.size                   = gt::px(12),
      row_group.font.weight             = "bold",
      column_labels.font.weight         = "bold",
      table.border.top.color            = "#000000",
      table.border.top.width            = gt::px(2),
      column_labels.border.bottom.color = "#000000",
      column_labels.border.bottom.width = gt::px(1)
    ) |>
    gt::tab_source_note(
      gt::md(paste0("climasus4r | ", .qrl("source_note", lang)))
    )
}


# =============================================================================
# INTERNAL: MARKDOWN OUTPUT
# =============================================================================

#' @noRd
.qr_write_markdown <- function(report, history, output_file, lang) {

  meta <- report$meta
  pipe <- report$pipeline
  ov   <- report$overview
  miss <- report$missing
  icd  <- report$icd
  geo  <- report$geographic
  drv  <- report$derived
  sc   <- report$score

  md <- character(0)
  .h1  <- function(x) c(paste0("# ", x),  "")
  .h2  <- function(x) c(paste0("## ", x), "")
  .h3  <- function(x) c(paste0("### ", x), "")
  .p   <- function(...) c(paste0(...), "")
  .sep <- function()   c("---", "")
  .kbl <- function(df) {
    if (nrow(df) == 0L) return("")
    rlang::check_installed("knitr", reason = "for markdown tables in quality report")
    c(knitr::kable(df, format = "markdown"), "")
  }
  .flag_md <- function(flag) {
    switch(flag, ok = ":white_check_mark:", warn = ":warning:",
           critical = ":red_circle:", ":information_source:")
  }

  # Title + metadata
  md <- c(md, .h1(.qrl("report_title", lang)))
  md <- c(md, .p("> ", .qrl("generated_at", lang, meta$generated)))
  md <- c(md, "")

  score_str  <- if (is.na(sc)) "N/A" else paste0(round(sc, 1), " / 100")
  score_flag <- if (is.na(sc)) "info" else if (sc >= 80) "ok" else if (sc >= 60) "warn" else "critical"
  md <- c(md, .p("**", .qrl("quality_score", lang), "**: ",
                 .flag_md(score_flag), " ", score_str))
  md <- c(md, .sep())

  # Section 0: Pipeline
  md <- c(md, .h2(.qrl("sec_pipeline", lang)))
  md <- c(md, .p("| ", .qrl("metric", lang), " | ", .qrl("value", lang), " |"))
  md <- c(md, .p("|---|---|"))
  md <- c(md, .p("| ", .qrl("current_stage", lang), " | `", meta$stage,   "` |"))
  md <- c(md, .p("| ", .qrl("system_label",  lang), " | `", meta$system,  "` |"))
  md <- c(md, .p("| ", .qrl("type_label",    lang), " | `", meta$type,    "` |"))
  md <- c(md, .p("| ", .qrl("history_entries", lang), " | ",meta$n_history, " |"))
  md <- c(md, "")
  md <- c(md, .h3(.qrl("fns_applied", lang)))
  for (fn in pipe$functions_applied) md <- c(md, .p("- `", fn, "`"))
  md <- c(md, "")
  if (length(history) > 0L) {
    md <- c(md, .h3(.qrl("history_log", lang)))
    for (h in history) md <- c(md, .p("- ", h))
    md <- c(md, "")
  }
  md <- c(md, .sep())

  # Section 1: Overview
  md <- c(md, .h2(.qrl("sec_overview", lang)))
  md <- c(md, .p("- **", .qrl("ov_rows",       lang), "**: ",
                 format(ov$n_rows, big.mark = ",")))
  md <- c(md, .p("- **", .qrl("ov_cols",       lang), "**: ", ov$n_cols))
  dup_flag <- if (ov$n_dup == 0L) "ok" else if (ov$pct_dup < 1) "warn" else "critical"
  md <- c(md, .p("- ", .flag_md(dup_flag), " **", .qrl("ov_duplicates", lang),
                 "**: ", ov$n_dup, " (", ov$pct_dup, "%)"))
  md <- c(md, .sep())

  # Section 2: Completeness
  md <- c(md, .h2(.qrl("sec_missing", lang)))
  sc_miss <- miss$completeness_score
  sc_flag <- if (sc_miss >= 80) "ok" else if (sc_miss >= 60) "warn" else "critical"
  md <- c(md, .p("**", .qrl("completeness_score", lang), "**: ",
                 .flag_md(sc_flag), " ", sc_miss, " / 100"))
  md <- c(md, "")
  if (nrow(miss$by_column) > 0L) {
    md <- c(md, .kbl(miss$by_column[, c("column", "n_missing", "pct_missing", "quality_flag")]))
  } else {
    md <- c(md, .p("> ", .qrl("no_missing", lang)))
  }
  md <- c(md, .sep())

  # Section 3: Demographics
  md <- c(md, .h2(.qrl("sec_demographics", lang)))
  for (key in c("sex", "race", "education", "climate_risk")) {
    d <- report$demographics[[key]]
    if (is.null(d)) next
    md <- c(md, .h3(paste0(.qrl(key, lang), " [", d$col, "]")))
    md <- c(md, .p(.qrl("missing", lang), ": ", d$pct_missing, "%"))
    md <- c(md, .kbl(d$freq))
  }
  d_age <- report$demographics$age
  if (!is.null(d_age)) {
    md <- c(md, .h3(paste0(.qrl("age", lang), " [", d_age$col, "]")))
    md <- c(md, .p("- ", .qrl("age_range", lang), ": ", d_age$min, " - ", d_age$max))
    md <- c(md, .p("- ", .qrl("mean", lang), ": ", d_age$mean))
    md <- c(md, .p("- ", .qrl("missing", lang), ": ", d_age$pct_missing, "%"))
    if (d_age$n_implausible > 0L)
      md <- c(md, .p("- :warning: ", .qrl("implausible", lang), ": ", d_age$n_implausible))
    md <- c(md, "")
  }
  md <- c(md, .sep())

  # Section 4: Dates
  if (length(report$dates) > 0L) {
    md <- c(md, .h2(.qrl("sec_dates", lang)))
    for (col in names(report$dates)) {
      d <- report$dates[[col]]
      issues_flag <- if ((d$n_future + d$n_pre1900) == 0L) "ok" else "warn"
      md <- c(md, .h3(col))
      md <- c(md, .p("- ", .flag_md(issues_flag), " ",
                     .qrl("date_range", lang), ": ",
                     if (!is.na(d$date_min)) paste0(d$date_min, " - ", d$date_max) else "N/A"))
      md <- c(md, .p("- ", .qrl("future",  lang), ": ", d$n_future))
      md <- c(md, .p("- ", .qrl("pre1900", lang), ": ", d$n_pre1900))
      md <- c(md, .p("- ", .qrl("missing", lang), ": ", d$n_missing))
      md <- c(md, "")
    }
    md <- c(md, .sep())
  }

  # Section 5: ICD
  if (length(icd) > 0L) {
    md <- c(md, .h2(.qrl("sec_icd", lang)))
    vst <- if (icd$pct_valid >= 95) "ok" else if (icd$pct_valid >= 80) "warn" else "critical"
    md <- c(md, .p("- **", .qrl("icd_unique", lang), "**: ", icd$n_unique))
    md <- c(md, .p("- ", .flag_md(vst), " **", .qrl("icd_valid",   lang), "**: ", icd$pct_valid, "%"))
    md <- c(md, .p("- **", .qrl("missing",    lang), "**: ", icd$pct_missing, "%"))
    md <- c(md, "")
    md <- c(md, .h3(.qrl("icd_top", lang)))
    md <- c(md, .kbl(icd$top_codes[!is.na(icd$top_codes$code), ]))
    if (nrow(icd$chapters) > 0L) {
      md <- c(md, .h3(.qrl("icd_chapters", lang)))
      md <- c(md, .kbl(icd$chapters))
    }
    md <- c(md, .sep())
  }

  # Section 6: Geographic
  if (length(geo) > 0L) {
    md <- c(md, .h2(.qrl("sec_geographic", lang)))
    if (!is.null(geo$municipalities)) {
      md <- c(md, .h3(.qrl("municipalities", lang)))
      md <- c(md, .p("- ", .qrl("municipalities", lang), ": ", geo$municipalities$n_unique))
      md <- c(md, .kbl(geo$municipalities$top))
    }
    if (!is.null(geo$states)) {
      md <- c(md, .h3(.qrl("states", lang)))
      md <- c(md, .kbl(geo$states$top))
    }
    md <- c(md, .sep())
  }

  # Section 7: Derived variables
  md <- c(md, .h2(.qrl("sec_derived", lang)))
  for (key in names(drv$variables)) {
    v   <- drv$variables[[key]]
    flg <- if (v$present) "ok" else "warn"
    lbl <- if (v$present) paste0(key, " [", v$col, "]") else key
    md  <- c(md, .p("- ", .flag_md(flg), " ", lbl))
  }
  md <- c(md, "")

  writeLines(md, output_file)
}


# =============================================================================
# INTERNAL: HTML OUTPUT
# =============================================================================

#' @noRd
.qr_write_html <- function(report, history, output_file, lang) {

  meta <- report$meta
  pipe <- report$pipeline
  ov   <- report$overview
  miss <- report$missing
  icd  <- report$icd
  geo  <- report$geographic
  drv  <- report$derived
  sc   <- report$score

  score_color <- if (is.na(sc)) "#2980b9"
                 else if (sc >= 80) "#27ae60"
                 else if (sc >= 60) "#e67e22"
                 else "#e74c3c"
  score_str   <- if (is.na(sc)) "N/A" else paste0(round(sc, 1), " / 100")

  .badge <- function(status, text) {
    col <- switch(status,
      ok       = "#27ae60",
      warn     = "#e67e22",
      critical = "#e74c3c",
      "#2980b9"
    )
    sprintf('<span style="background:%s;color:#fff;border-radius:4px;padding:2px 7px;font-size:11px">%s</span>',
            col, text)
  }

  .df_to_html <- function(df, id = "") {
    if (nrow(df) == 0L) return("<p><em>--</em></p>")
    header <- paste0("<th>", names(df), "</th>", collapse = "")
    rows <- apply(df, 1, function(r) {
      paste0("<tr>", paste0("<td>", r, "</td>", collapse = ""), "</tr>")
    })
    paste0('<table id="', id, '"><thead><tr>', header, "</tr></thead><tbody>",
           paste(rows, collapse = ""), "</tbody></table>")
  }

  css <- '
body{font-family:Arial,sans-serif;margin:40px;color:#2c3e50;max-width:1100px}
h1{color:#1a252f;border-bottom:3px solid #1B6CA8;padding-bottom:8px}
h2{color:#1B6CA8;border-bottom:1px solid #dde;margin-top:30px}
h3{color:#34495e;margin-top:15px}
table{border-collapse:collapse;width:100%;margin:12px 0;font-size:13px}
th,td{border:1px solid #dde;padding:6px 10px;text-align:left}
th{background:#1B6CA8;color:#fff}
tr:nth-child(even){background:#f8f9fa}
.score-box{display:inline-block;padding:10px 20px;border-radius:6px;
  font-size:22px;font-weight:bold;color:#fff;margin:8px 0}
.meta-bar{background:#f0f4f8;border-radius:6px;padding:12px 18px;
  margin-bottom:20px;font-size:13px;display:flex;gap:24px;flex-wrap:wrap}
.meta-item span{font-weight:bold;color:#1B6CA8}
.fn-list{list-style:none;padding:0}
.fn-list li::before{content:"v  ";color:#27ae60;font-weight:bold}
.pending-list{list-style:none;padding:0;color:#aaa}
.pending-list li::before{content:"-  "}
.history-entry{font-size:12px;color:#555;margin:2px 0}
footer{margin-top:40px;font-size:12px;color:#aaa;border-top:1px solid #eee;padding-top:10px}
'

  html <- c(
    "<!DOCTYPE html>",
    "<html lang='", lang, "'>",
    "<head><meta charset='UTF-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    paste0("<title>", .qrl("report_title", lang), "</title>"),
    "<style>", css, "</style>",
    "</head><body>",
    paste0("<h1>", .qrl("report_title", lang), "</h1>"),
    paste0('<div class="meta-bar">'),
    paste0('<div class="meta-item">', .qrl("generated_at", lang, meta$generated), "</div>"),
    paste0('<div class="meta-item">', .qrl("current_stage", lang),
           ': <span>', meta$stage, "</span></div>"),
    paste0('<div class="meta-item">', .qrl("system_label",  lang),
           ': <span>', meta$system, "</span></div>"),
    paste0('<div class="meta-item">', .qrl("type_label",    lang),
           ': <span>', meta$type, "</span></div>"),
    "</div>",
    paste0('<div class="score-box" style="background:', score_color, '">',
           .qrl("quality_score", lang), ": ", score_str, "</div>"),

    # Pipeline
    paste0("<h2>", .qrl("sec_pipeline", lang), "</h2>"),
    paste0("<p>", .qrl("fns_applied", lang), ":</p>"),
    "<ul class='fn-list'>",
    paste0("<li>", pipe$functions_applied, "</li>", collapse = ""),
    "</ul>",

    if (length(history) > 0L) c(
      paste0("<h3>", .qrl("history_log", lang), "</h3>"),
      paste0('<div class="history-entry">', history, "</div>", collapse = "")
    ) else "",

    # Overview
    paste0("<h2>", .qrl("sec_overview", lang), "</h2>"),
    "<table><tr>",
    paste0("<th>", .qrl("metric", lang), "</th><th>", .qrl("value", lang), "</th></tr>"),
    paste0("<tr><td>", .qrl("ov_rows",       lang), "</td><td>",
           format(ov$n_rows, big.mark = ","), "</td></tr>"),
    paste0("<tr><td>", .qrl("ov_cols",       lang), "</td><td>", ov$n_cols, "</td></tr>"),
    paste0("<tr><td>", .qrl("ov_duplicates", lang), "</td><td>",
           ov$n_dup, " (", ov$pct_dup, "%)", "</td></tr>"),
    "</table>",

    # Completeness
    paste0("<h2>", .qrl("sec_missing", lang), "</h2>"),
    paste0("<p><strong>", .qrl("completeness_score", lang), ":</strong> ",
           .badge(if (miss$completeness_score >= 80) "ok"
                  else if (miss$completeness_score >= 60) "warn" else "critical",
                  paste0(miss$completeness_score, " / 100")), "</p>"),
    if (nrow(miss$by_column) > 0L) {
      rows_html <- apply(miss$by_column, 1, function(r) {
        bg <- switch(r["quality_flag"],
          ok = "#eafaf1", warn = "#fef9e7", critical = "#fdf0ed", "#fff")
        paste0('<tr style="background:', bg, '"><td>', r["column"], "</td><td>",
               r["n_missing"], "</td><td>", r["pct_missing"], "%</td><td>",
               .badge(r["quality_flag"], r["quality_flag"]), "</td></tr>")
      })
      c("<table><thead><tr>",
        paste0("<th>", c(.qrl("column", lang), "N missing", "%", .qrl("status", lang)),
               "</th>", collapse = ""),
        "</tr></thead><tbody>", paste(rows_html, collapse = ""), "</tbody></table>")
    } else paste0("<p><em>", .qrl("no_missing", lang), "</em></p>"),

    # Demographics
    paste0("<h2>", .qrl("sec_demographics", lang), "</h2>"),
    do.call(paste0, lapply(c("sex", "race", "education", "climate_risk"), function(key) {
      d <- report$demographics[[key]]
      if (is.null(d)) return("")
      paste0("<h3>", .qrl(key, lang), " [", d$col, "]</h3>",
             "<p>", .qrl("missing", lang), ": ", d$pct_missing, "%</p>",
             .df_to_html(d$freq))
    })),

    do.call(paste0, lapply(list(report$demographics$age), function(d) {
      if (is.null(d)) return("")
      paste0("<h3>", .qrl("age", lang), " [", d$col, "]</h3>",
             "<p>", .qrl("age_range", lang), ": ", d$min, " - ", d$max,
             " | ", .qrl("mean", lang), ": ", d$mean,
             " | ", .qrl("missing", lang), ": ", d$pct_missing, "%",
             if (d$n_implausible > 0L) paste0(
               " | ", .badge("warn", paste0(.qrl("implausible", lang), ": ", d$n_implausible))
             ) else "", "</p>")
    })),

    # Dates
    if (length(report$dates) > 0L) c(
      paste0("<h2>", .qrl("sec_dates", lang), "</h2>"),
      "<table><thead><tr>",
      paste0("<th>", c(.qrl("column", lang), .qrl("date_range", lang),
                       .qrl("future", lang), .qrl("pre1900", lang),
                       .qrl("missing", lang)), "</th>", collapse = ""),
      "</tr></thead><tbody>",
      do.call(paste0, lapply(names(report$dates), function(col) {
        d <- report$dates[[col]]
        rng <- if (!is.na(d$date_min)) paste0(d$date_min, " - ", d$date_max) else "N/A"
        bg <- if ((d$n_future + d$n_pre1900) == 0L) "#eafaf1" else "#fef9e7"
        paste0('<tr style="background:', bg, '"><td>', col,
               "</td><td>", rng,
               "</td><td>", d$n_future,
               "</td><td>", d$n_pre1900,
               "</td><td>", d$n_missing, "</td></tr>")
      })),
      "</tbody></table>"
    ) else "",

    # ICD
    if (length(icd) > 0L) c(
      paste0("<h2>", .qrl("sec_icd", lang), "</h2>"),
      paste0("<p>",
             .badge(if (icd$pct_valid >= 95) "ok" else if (icd$pct_valid >= 80) "warn" else "critical",
                    paste0(.qrl("icd_valid", lang), ": ", icd$pct_valid, "%")),
             " | ", .qrl("icd_unique", lang), ": ", icd$n_unique,
             " | ", .qrl("missing", lang), ": ", icd$pct_missing, "%</p>"),
      paste0("<h3>", .qrl("icd_top", lang), "</h3>"),
      .df_to_html(icd$top_codes[!is.na(icd$top_codes$code), ]),
      if (nrow(icd$chapters) > 0L) c(
        paste0("<h3>", .qrl("icd_chapters", lang), "</h3>"),
        .df_to_html(icd$chapters)
      ) else ""
    ) else "",

    # Geographic
    if (length(geo) > 0L) c(
      paste0("<h2>", .qrl("sec_geographic", lang), "</h2>"),
      if (!is.null(geo$municipalities)) c(
        paste0("<h3>", .qrl("municipalities", lang), " (n=",
               geo$municipalities$n_unique, ")</h3>"),
        .df_to_html(geo$municipalities$top)
      ) else "",
      if (!is.null(geo$states)) c(
        paste0("<h3>", .qrl("states", lang), " (n=", geo$states$n_unique, ")</h3>"),
        .df_to_html(geo$states$top)
      ) else ""
    ) else "",

    # Derived
    paste0("<h2>", .qrl("sec_derived", lang), "</h2>"),
    "<table><thead><tr>",
    paste0("<th>", c(.qrl("variable", lang), .qrl("status", lang), .qrl("column", lang)),
           "</th>", collapse = ""),
    "</tr></thead><tbody>",
    do.call(paste0, lapply(names(drv$variables), function(key) {
      v <- drv$variables[[key]]
      paste0("<tr><td>", key, "</td><td>",
             .badge(if (v$present) "ok" else "warn",
                    if (v$present) .qrl("present", lang) else .qrl("absent", lang)),
             "</td><td>", v$col %||% "-", "</td></tr>")
    })),
    "</tbody></table>",

    # Footer
    paste0('<footer>climasus4r | ', .qrl("source_note", lang), '</footer>'),
    "</body></html>"
  )

  writeLines(html, output_file)
}


# =============================================================================
# MULTILINGUAL LABELS
# =============================================================================

.qr_labels <- list(
  report_title        = list(pt = "Relatorio de Qualidade de Dados",
                             en = "Data Quality Report",
                             es = "Informe de Calidad de Datos"),
  generated_at        = list(pt = "Gerado em: %s",
                             en = "Generated: %s",
                             es = "Generado: %s"),
  materialising       = list(pt = "Materializando dataset Arrow...",
                             en = "Materialising Arrow dataset...",
                             es = "Materializando dataset Arrow..."),
  computing           = list(pt = "Calculando metricas de qualidade...",
                             en = "Computing quality metrics...",
                             es = "Calculando metricas de calidad..."),
  saved_to            = list(pt = "Relatorio salvo em: %s",
                             en = "Report saved to: %s",
                             es = "Informe guardado en: %s"),
  quality_score       = list(pt = "Pontuacao de Qualidade",
                             en = "Quality Score",
                             es = "Puntuacion de Calidad"),
  # Pipeline section
  sec_pipeline        = list(pt = "0. Auditoria do Pipeline",
                             en = "0. Pipeline Audit",
                             es = "0. Auditoria del Pipeline"),
  current_stage       = list(pt = "Etapa atual",
                             en = "Current stage",
                             es = "Etapa actual"),
  system_label        = list(pt = "Sistema",
                             en = "System",
                             es = "Sistema"),
  type_label          = list(pt = "Tipo",
                             en = "Type",
                             es = "Tipo"),
  fns_applied         = list(pt = "Funcoes aplicadas ao dataset",
                             en = "Functions applied to dataset",
                             es = "Funciones aplicadas al dataset"),
  no_history          = list(pt = "Historico de processamento vazio ou dado nao e climasus_df",
                             en = "Processing history empty or data is not climasus_df",
                             es = "Historial de procesamiento vacio o dato no es climasus_df"),
  stages_pending      = list(pt = "Etapas ainda nao aplicadas",
                             en = "Stages not yet applied",
                             es = "Etapas aun no aplicadas"),
  history_entries     = list(pt = "Entradas no historico",
                             en = "History entries",
                             es = "Entradas en el historial"),
  history_log         = list(pt = "Log de Processamento",
                             en = "Processing Log",
                             es = "Registro de Procesamiento"),
  more_entries        = list(pt = "entradas adicionais",
                             en = "more entries",
                             es = "entradas adicionales"),
  # Overview
  sec_overview        = list(pt = "1. Visao Geral",
                             en = "1. Overview",
                             es = "1. Vista General"),
  ov_rows             = list(pt = "Linhas",
                             en = "Rows",
                             es = "Filas"),
  ov_cols             = list(pt = "Colunas",
                             en = "Columns",
                             es = "Columnas"),
  ov_duplicates       = list(pt = "Duplicatas",
                             en = "Duplicates",
                             es = "Duplicados"),
  # Missing
  sec_missing         = list(pt = "2. Completude (Valores Ausentes)",
                             en = "2. Completeness (Missing Values)",
                             es = "2. Completitud (Valores Faltantes)"),
  completeness_score  = list(pt = "Pontuacao de Completude",
                             en = "Completeness Score",
                             es = "Puntuacion de Completitud"),
  miss_complete_cols  = list(pt = "Colunas completas",
                             en = "Complete columns",
                             es = "Columnas completas"),
  miss_warn           = list(pt = "Colunas com aviso (5-20%)",
                             en = "Warning columns (5-20%)",
                             es = "Columnas con aviso (5-20%)"),
  miss_critical       = list(pt = "Colunas criticas (>20%)",
                             en = "Critical columns (>20%)",
                             es = "Columnas criticas (>20%)"),
  no_missing          = list(pt = "Nenhum valor ausente detectado",
                             en = "No missing values detected",
                             es = "No se detectaron valores faltantes"),
  more_cols           = list(pt = "colunas adicionais",
                             en = "more columns",
                             es = "columnas adicionales"),
  # Demographics
  sec_demographics    = list(pt = "3. Variaveis Demograficas",
                             en = "3. Demographic Variables",
                             es = "3. Variables Demograficas"),
  no_demo_cols        = list(pt = "Nenhuma coluna demografica padr\u00e3o detectada",
                             en = "No standard demographic columns detected",
                             es = "No se detectaron columnas demograficas estandar"),
  sex                 = list(pt = "Sexo",          en = "Sex",          es = "Sexo"),
  race                = list(pt = "Raca/Cor",      en = "Race/Colour",  es = "Raza/Color"),
  age                 = list(pt = "Idade",          en = "Age",          es = "Edad"),
  age_range           = list(pt = "Intervalo",      en = "Range",        es = "Rango"),
  mean                = list(pt = "Media",          en = "Mean",         es = "Media"),
  median              = list(pt = "Mediana",        en = "Median",       es = "Mediana"),
  implausible         = list(pt = "Implausivel (<0 ou >130)",
                             en = "Implausible (<0 or >130)",
                             es = "Implausible (<0 o >130)"),
  education           = list(pt = "Escolaridade",  en = "Education",    es = "Escolaridad"),
  climate_risk        = list(pt = "Risco Climatico", en = "Climate Risk", es = "Riesgo Climatico"),
  missing             = list(pt = "Ausente",        en = "Missing",      es = "Faltante"),
  # Dates
  sec_dates           = list(pt = "4. Validacao de Datas",
                             en = "4. Date Validation",
                             es = "4. Validacion de Fechas"),
  date_range          = list(pt = "Intervalo",      en = "Range",        es = "Rango"),
  future              = list(pt = "Datas futuras",  en = "Future dates", es = "Fechas futuras"),
  pre1900             = list(pt = "Antes de 1900",  en = "Before 1900",  es = "Antes de 1900"),
  # ICD
  sec_icd             = list(pt = "5. Qualidade dos Codigos CID-10",
                             en = "5. ICD-10 Code Quality",
                             es = "5. Calidad de Codigos CIE-10"),
  icd_unique          = list(pt = "Codigos unicos",
                             en = "Unique codes",
                             es = "Codigos unicos"),
  icd_valid           = list(pt = "Codigos validos",
                             en = "Valid codes",
                             es = "Codigos validos"),
  icd_top             = list(pt = "Codigos mais frequentes",
                             en = "Top ICD codes",
                             es = "Codigos mas frecuentes"),
  icd_chapters        = list(pt = "Distribuicao por capitulo",
                             en = "Chapter distribution",
                             es = "Distribucion por capitulo"),
  # Geographic
  sec_geographic      = list(pt = "6. Cobertura Geografica",
                             en = "6. Geographic Coverage",
                             es = "6. Cobertura Geografica"),
  municipalities      = list(pt = "Municipios",    en = "Municipalities", es = "Municipios"),
  states              = list(pt = "Estados (UF)",  en = "States (UF)",    es = "Estados (UF)"),
  # Derived
  sec_derived         = list(pt = "7. Variaveis Derivadas",
                             en = "7. Derived Variables",
                             es = "7. Variables Derivadas"),
  variable            = list(pt = "Variavel",      en = "Variable",     es = "Variable"),
  present             = list(pt = "Presente",      en = "Present",      es = "Presente"),
  absent              = list(pt = "Ausente",       en = "Absent",       es = "Ausente"),
  # Table labels
  metric              = list(pt = "Metrica",       en = "Metric",       es = "Metrica"),
  value               = list(pt = "Valor",         en = "Value",        es = "Valor"),
  status              = list(pt = "Status",        en = "Status",       es = "Estado"),
  column              = list(pt = "Coluna",        en = "Column",       es = "Columna"),
  # Source note
  source_note         = list(pt = "Dados de saude brasileiros (DATASUS)",
                             en = "Brazilian health data (DATASUS)",
                             es = "Datos de salud brasile\u00f1os (DATASUS)")
)

#' @noRd
.qrl <- function(key, lang, ...) {
  row <- .qr_labels[[key]]
  if (is.null(row)) return(key)
  txt <- row[[lang]] %||% row[["pt"]] %||% key
  if (...length() > 0L) sprintf(txt, ...) else txt
}
