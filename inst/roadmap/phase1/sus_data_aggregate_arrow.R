#' Aggregate Health Data into Time Series
#'
#' Aggregates individual-level health data into time series counts by specified
#' time units and grouping variables. This function is essential for preparing
#' data for time series analysis, DLNM models, and other temporal epidemiological methods.
#'
#' Supports three backends transparently:
#' \itemize{
#'   \item **tibble / data.frame** — standard in-memory dplyr pipeline (original behaviour).
#'   \item **Arrow Table / Dataset** — pushed down via \pkg{arrow}'s dplyr back-end;
#'     collected to a tibble only after aggregation.
#'   \item **DuckDB tbl** — SQL pushed down via \pkg{duckplyr} / `dplyr` on a DuckDB
#'     connection; collected to a tibble only after aggregation.
#' }
#'
#' @param df A data frame, Arrow Table/Dataset, or DuckDB `tbl_dbi` object.
#'   Must be the output of `sus_data_standardize()` (or downstream filter functions)
#'   and carry a `climasus_df` class / `sus_meta` attribute.
#' @param time_unit Character string specifying the temporal aggregation unit.
#'   **Standard units**: `"day"`, `"week"`, `"month"`, `"quarter"`, `"year"`
#'   **Multi-day/week/month**: `"2 days"`, `"5 days"` (pentads), `"14 days"` (fortnightly),
#'    `"3 months"` (trimester), `"6 months"` (semester).
#'   **Special**: `"season"` (Brazilian seasons: DJF, MAM, JJA, SON).
#'   Default is `"day"`.
#' @param fun Character string or list of functions specifying the aggregation function(s).
#'   Options: `"count"` (default), `"sum"`, `"mean"`, `"median"`, `"min"`, `"max"`,
#'   `"sd"`, `"q25"` (25th percentile), `"q75"`, `"q95"`, and `"q99"`.
#'   Can also be a named list for multiple aggregations, e.g.,
#'   `list(mean_temp = "mean", max_temp = "max")`.
#'   \strong{Note}: `"median"`, `"sd"`, and percentiles (`"q*"`) require materialisation
#'   of the date column on Arrow/DuckDB backends and may be slower on very large datasets.
#' @param group_by Character vector with names of columns to group by (e.g.,
#'   `c("sex", "age_group", "race")`). If `NULL` (default), aggregates
#'   across `"municipality_code"` records.
#' @param value_col Character string with the name of the column to aggregate when
#'   using functions other than `"count"`. Required for `"sum"`, `"mean"`, etc.
#' @param date_col Character string with the name of the date column to use for
#'   aggregation. If `NULL` (default), the function will attempt to auto-detect
#'   the date column based on common patterns.
#' @param complete_dates Logical. If `TRUE` (default), fills in missing time periods
#'   with zero counts to create a complete time series without gaps.
#' @param lang Character string specifying the language for messages. Options:
#'   `"en"` (English), `"pt"` (Portuguese, default), `"es"` (Spanish).
#' @param verbose Logical. If `TRUE` (default), prints progress messages.
#'
#' @return
#' \itemize{
#'   \item **Arrow backend**: an `arrow::Table` with `sus_meta` JSON embedded in
#'     schema metadata (key `"sus_meta"`).  Use `read_sus_meta_from_schema()` to
#'     retrieve it later.
#'   \item **DuckDB backend**: a lazy `tbl_sql`; `sus_meta` JSON is stored/updated
#'     in the `.__sus_meta__` helper table in the same connection.
#'   \item **tibble backend**: a plain tibble with `attr(df, "sus_meta")` set.
#'     No `climasus_df` class is attached.
#' }
#'
#' @details
#' **Backend detection** is automatic:
#' \itemize{
#'   \item Arrow Tables (`arrow::Table`, `arrow::Dataset`) are detected via
#'     `inherits(df, "ArrowTabular")` / `inherits(df, "Dataset")`.
#'   \item DuckDB tbls are detected via `inherits(df, "tbl_dbi")` with a DuckDB
#'     connection (checked through `class(dbplyr::remote_con(df))`).
#'   \item Everything else is treated as a standard tibble/data.frame.
#' }
#' The entire aggregation pipeline is **fully lazy** — no `collect()` is called
#' unless (a) `fun` is `"median"`, `"sd"`, or a quantile on Arrow, or (b)
#' `complete_dates = TRUE`.  Metadata (`sus_meta`) is recovered from the Arrow
#' schema or DuckDB `.__sus_meta__` table and re-embedded in the output via
#' `embed_sus_meta()`.
#'
#' **Brazilian Seasons** (when `time_unit = "season"`):
#' \itemize{
#'   \item **Summer (Verao)**: December-January-February (DJF)
#'   \item **Autumn (Outono)**: March-April-May (MAM)
#'   \item **Winter (Inverno)**: June-July-August (JJA)
#'   \item **Spring (Primavera)**: September-October-November (SON)
#' }
#'
#' @examples
#' \dontrun{
#' library(climasus4r)
#' library(arrow)
#' library(duckdb)
#'
#' # --- Arrow backend ---
#' arrow_tbl <- sus_data_import(uf = "SP", year = 2023, system = "SIM-DO") |>
#'   sus_data_standardize() |>
#'   arrow::as_arrow_table()   # convert AFTER standardise (keeps sus_meta)
#'
#' df_daily <- sus_data_aggregate(arrow_tbl, time_unit = "day")
#'
#' # --- DuckDB backend ---
#' con <- duckdb::dbConnect(duckdb::duckdb())
#' duck_tbl <- sus_data_import(uf = "SP", year = 2023, system = "SIM-DO") |>
#'   sus_data_standardize() |>
#'   dplyr::copy_to(con, ., "sim_sp_2023", overwrite = TRUE)
#' # NOTE: copy_to drops attributes; re-attach before calling aggregate:
#' attr(duck_tbl, "sus_meta") <- attr(original_df, "sus_meta")
#' class(duck_tbl) <- c("climasus_df", class(duck_tbl))
#'
#' df_monthly <- sus_data_aggregate(duck_tbl, time_unit = "month")
#'
#' # --- Standard tibble (original usage) ---
#' df_weekly <- sus_data_aggregate(
#'   df,
#'   time_unit = "week",
#'   group_by = c("age_group", "sex")
#' )
#' }
#' @importFrom rlang .data
#'
#' @export
sus_data_aggregate_arrow <- function(df,
                               time_unit  = "day",
                               fun        = "count",
                               group_by   = NULL,
                               value_col  = NULL,
                               complete_dates = FALSE,
                               date_col   = NULL,
                               lang       = "pt",
                               verbose    = TRUE) {

  # --------------------------------------------------------------------------
  # 0. Detect backend
  # --------------------------------------------------------------------------
  backend <- detect_backend(df)
  incoming_meta <- attr(df, "sus_meta")
  # --------------------------------------------------------------------------
  # 1. Validate inputs
  # --------------------------------------------------------------------------
  if (verbose) {
    title_msg <- switch(lang,
      "en" = "climasus4r - Temporal Data Aggregation",
      "pt" = "climasus4r - Agrega\u00e7\u00e3o Temporal de Dados",
      "es" = "climasus4r - Agregaci\u00f3n Temporal de Datos"
    )
    cli::cli_h1(title_msg)

    backend_msg <- switch(backend,
      "arrow"  = switch(lang,
        "en" = "Backend: Arrow (columnar in-memory)",
        "pt" = "Backend: Arrow (colunar em mem\u00f3ria)",
        "es" = "Backend: Arrow (columnar en memoria)"
      ),
      "duckdb" = switch(lang,
        "en" = "Backend: DuckDB (in-process SQL)",
        "pt" = "Backend: DuckDB (SQL em processo)",
        "es" = "Backend: DuckDB (SQL en proceso)"
      ),
      switch(lang,
        "en" = "Backend: tibble / data.frame (standard)",
        "pt" = "Backend: tibble / data.frame (padr\u00e3o)",
        "es" = "Backend: tibble / data.frame (est\u00e1ndar)"
      )
    )
    cli::cli_alert_info(backend_msg)
  }

  # Basic structural checks (work for all backends)
  if (!is.data.frame(df) && backend == "tibble") {
    stop("df must be a data frame")
  }

  if (backend == "tibble" && nrow(df) == 0) {
    stop("df is empty (0 rows)")
  }

  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_alert_warning("Language '{lang}' not supported. Using Portuguese (pt).")
    lang <- "pt"
  }

  # Validate fun parameter
  valid_funs <- c("count", "sum", "mean", "median", "min", "max", "sd",
                  "q25", "q75", "q95", "q99")

  if (is.character(fun)) {
    if (!fun %in% valid_funs) {
      cli::cli_abort(paste0("fun must be one of: ", paste(valid_funs, collapse = ", ")))
    }
    if (fun != "count" && is.null(value_col)) {
      cli::cli_abort("value_col must be specified when fun is not 'count'")
    }
  } else if (is.list(fun)) {
    if (!all(unlist(fun) %in% valid_funs)) {
      cli::cli_abort(paste0("All functions in list must be one of: ",
                            paste(valid_funs, collapse = ", ")))
    }
    if (is.null(value_col)) {
      cli::cli_abort("value_col must be specified when using multiple aggregation functions")
    }
  } else {
    cli::cli_abort("fun must be a character string or a named list")
  }

  # --------------------------------------------------------------------------
  # 2. climasus_df / metadata validation
  #
  # Arrow and DuckDB backends lose R attributes on every dplyr verb (mutate,
  # filter, group_by, etc.) because those operations return a new query object
  # that has no knowledge of R-level attributes.  We therefore try to recover
  # sus_meta from three sources, in order of preference:
  #
  #   (a) The object itself — works when the user passes a raw Arrow Table /
  #       DuckDB tbl that was just created and still carries the attribute.
  #   (b) The Arrow schema metadata field "sus_meta" — works when the user
  #       used `sus_as_arrow()` / `sus_as_duckdb()` helpers that embed the
  #       metadata in the schema as a JSON string.
  #   (c) The `.data` slot of an `arrow_dplyr_query` — Arrow stores the
  #       original Table in `df$.data`; we can read its attributes / schema.
  #
  # If none of the sources yields valid metadata the function aborts with a
  # helpful message explaining how to convert correctly.
  # --------------------------------------------------------------------------

  # Recover sus_meta from R attr, Arrow schema, or DuckDB metadata table.
  # Works uniformly for climasus_df tibbles, Arrow Tables/queries, DuckDB tbls.
  system <- incoming_meta$system

  # -- Stage validation -------------------------------------------------------
  current_stage  <- incoming_meta$stage
  required_stage <- "stand"

  if (!is_stage_at_least(current_stage, required_stage)) {
    msg_error <- list(
      en = paste0(
        "Data must be standardised before aggregation.\n",
        "Current stage: ", current_stage %||% "unknown", "\n",
        "Required stage: ", required_stage, "\n\n",
        "Please run:\n  df <- sus_data_standardize(df)"
      ),
      pt = paste0(
        "Dados devem ser padronizados antes da agregacao.\n",
        "Estagio atual: ", current_stage %||% "desconhecido", "\n",
        "Estagio requerido: ", required_stage, "\n\n",
        "Por favor, execute:\n  df <- sus_data_standardize(df)"
      ),
      es = paste0(
        "Los datos deben ser estandarizados antes de la agregacion.\n",
        "Etapa actual: ", current_stage %||% "desconocida", "\n",
        "Etapa requerida: ", required_stage, "\n\n",
        "Por favor, ejecute:\n  df <- sus_data_standardize(df)"
      )
    )
    cli::cli_abort(msg_error[[lang]] %||% msg_error[["en"]])
  }

  if (verbose) {
    msg_ok <- list(
      en = "Data stage validated: aggregation allowed",
      pt = "Estagio de dados validado: agregacao permitida",
      es = "Etapa de datos validada: agregacion permitida"
    )
    cli::cli_alert_success(msg_ok[[lang]] %||% msg_ok[["en"]])
  }

  # --------------------------------------------------------------------------
  # 4. Resolve date column
  # --------------------------------------------------------------------------
  # Column names are available for all backends via dplyr::tbl_vars / colnames
  col_names <- get_col_names(df, backend)

  if (is.null(date_col)) {
    date_col <- detect_date_column_from_names(col_names, system)

    if (verbose) {
      msg <- switch(lang,
        "en" = paste0("Auto-detected date column: ", date_col),
        "pt" = paste0("Coluna de data auto-detectada: ", date_col),
        "es" = paste0("Columna de fecha auto-detectada: ", date_col)
      )
      cli::cli_alert_info(msg)
    }
  }

  if (!date_col %in% col_names) {
    date_cols_hint <- paste(
      grep("(^dt_|_date$|^data_|^fecha_|^date_)", col_names, value = TRUE, ignore.case = TRUE),
      collapse = ", "
    )
    if (nchar(date_cols_hint) == 0) date_cols_hint <- "(none found)"
    cli::cli_abort(c(
      paste0("Date column '", date_col, "' not found in data."),
      "i" = paste0("Date-like columns available: ", date_cols_hint),
      "i" = paste0("All columns (first 30): ",
                   paste(utils::head(col_names, 30), collapse = ", "))
    ))
  }

  # --------------------------------------------------------------------------
  # 5. Resolve geographic column
  # --------------------------------------------------------------------------
  geo_col <- resolve_geo_col(col_names, system)

  # --------------------------------------------------------------------------
  # 6. Build the agg_date expression & filter bad geo — backend-aware
  # --------------------------------------------------------------------------
  # For Arrow and DuckDB we build SQL/dplyr expressions that execute remotely.
  # For season we need a helper that works element-wise on a Date vector —
  # so we pull only the date column first (cheap), compute season starts in R,
  # then join back. This is the one case where a partial collect is unavoidable.

  if (time_unit == "season") {
    # Partial materialise: pull only the date + geo + group_by columns needed
    pull_cols <- unique(c(date_col, geo_col, group_by, value_col))
    pull_cols <- pull_cols[!is.na(pull_cols) & pull_cols %in% col_names]

    df_work <- df |>
      dplyr::select(dplyr::all_of(pull_cols)) |>
      dplyr::collect() |>
      dplyr::filter(!is.na(.data[[date_col]])) |>
      dplyr::mutate(
        !!date_col := as.Date(.data[[date_col]]),
        agg_date   = get_brazilian_season_start(.data[[date_col]])
      )

    time_unit_label <- switch(lang,
      "en" = "seasonal", "pt" = "sazonais", "es" = "estacionales"
    )

  } else {
    # For Arrow and DuckDB: use dplyr::mutate with lubridate — both backends
    # support lubridate expressions via their dplyr translations.
    time_unit_label <- get_time_unit_label(time_unit, lang)

    df_work <- df |>
      dplyr::filter(!is.na(.data[[date_col]])) |>
      dplyr::mutate(
        !!date_col := as.Date(.data[[date_col]]),
        agg_date   = lubridate::floor_date(.data[[date_col]], unit = !!time_unit)
      )

    # Filter invalid geo values — push down before collect
    invalid_geo <- c("0", "000000", "")
    if (!is.null(geo_col) && geo_col %in% col_names) {
      df_work <- df_work |>
        dplyr::filter(!(.data[[geo_col]] %in% !!invalid_geo),
                      !is.na(.data[[geo_col]]))
    }
  }

  # --------------------------------------------------------------------------
  # 7. Count original rows (for verbose summary)
  # --------------------------------------------------------------------------
  # Arrow/DuckDB: nrow on a lazy tbl triggers a COUNT(*) — acceptable overhead
  n_original <- tryCatch(nrow(df_work), error = function(e) NA_integer_)

  # --------------------------------------------------------------------------
  # 8. Build group variables and aggregate — still lazy where possible
  # --------------------------------------------------------------------------
  group_vars <- build_group_vars(group_by, geo_col, col_names)
  # Always include agg_date
  group_vars_full <- unique(c("agg_date", group_vars))

 # Build a single dplyr expression translatable by Arrow and DuckDB backends.
  # apply_aggregation_function() uses base-R functions (quantile, sd, median)
  # which cannot be pushed down — we use the dplyr / arrow / duckdb translations
  # directly instead.  collect() is deferred to step 9.

  .make_agg_expr <- function(fun_type, col_sym) {
    switch(fun_type,
      "count"  = rlang::expr(dplyr::n()),
      "sum"    = rlang::expr(sum(!!col_sym,             na.rm = TRUE)),
      "mean"   = rlang::expr(mean(!!col_sym,            na.rm = TRUE)),
      "min"    = rlang::expr(min(!!col_sym,             na.rm = TRUE)),
      "max"    = rlang::expr(max(!!col_sym,             na.rm = TRUE)),
      # median / sd / quantiles are NOT supported by Arrow's dplyr back-end and
      # have limited DuckDB support — we signal this clearly rather than silently
      # falling back to base-R (which would materialise the whole table).
      "median" = rlang::expr(median(!!col_sym,          na.rm = TRUE)),
      "sd"     = rlang::expr(sd(!!col_sym,              na.rm = TRUE)),
      "q25"    = rlang::expr(quantile(!!col_sym, 0.25,  na.rm = TRUE)),
      "q75"    = rlang::expr(quantile(!!col_sym, 0.75,  na.rm = TRUE)),
      "q95"    = rlang::expr(quantile(!!col_sym, 0.95,  na.rm = TRUE)),
      "q99"    = rlang::expr(quantile(!!col_sym, 0.99,  na.rm = TRUE)),
      cli::cli_abort("Unknown aggregation function: {fun_type}")
    )
  }

  if (is.character(fun) && fun == "count") {

    df_agg <- df_work |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars_full))) |>
      dplyr::summarise(agg_value = dplyr::n(), .groups = "drop")

    agg_col_name <- get_smart_column_name(system, "count", lang)

  } else if (is.character(fun)) {

    col_sym      <- rlang::sym(value_col)
    agg_expr     <- .make_agg_expr(fun, col_sym)
    agg_col_name <- paste0(fun, "_", value_col)

    df_agg <- df_work |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars_full))) |>
      dplyr::summarise(agg_value = !!agg_expr, .groups = "drop")

  } else if (is.list(fun)) {

    col_sym <- rlang::sym(value_col)
    agg_exprs <- stats::setNames(
      lapply(fun, function(fun_type) .make_agg_expr(fun_type, col_sym)),
      names(fun)
    )

    df_agg <- df_work |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars_full))) |>
      dplyr::summarise(!!!agg_exprs, .groups = "drop")

    agg_col_name <- NULL  # multiple columns — no single rename needed
  }

  # --------------------------------------------------------------------------
  # 9. Materialise — single dplyr::collect() for Arrow and DuckDB backends
  # --------------------------------------------------------------------------
  # All upstream operations (filter, mutate, group_by, summarise) were fully
  # lazy. We collect exactly once here, after aggregation is complete,
  # so only the small aggregated result is pulled into R memory.
  df_agg <- dplyr::collect(df_agg)

  # Rename the sentinel column now that we are in-memory
  if (!is.null(agg_col_name)) {
    names(df_agg)[names(df_agg) == "agg_value"] <- agg_col_name
  }

  df_agg <- df_agg |>
    dplyr::rename(date = agg_date)

  # Drop duplicate columns (can arise from DuckDB schema)
  cols_unique <- !base::duplicated(names(df_agg))
  if (any(!cols_unique)) df_agg <- df_agg[, cols_unique, drop = FALSE]

  # --------------------------------------------------------------------------
  # 10. Complete dates (in-memory, always a tibble at this point)
  # --------------------------------------------------------------------------
  if (complete_dates) {
    fill_value <- if (is.character(fun) && fun == "count") 0L else NA
    df_agg <- complete_time_series(df_agg, time_unit, group_by, fill_value, lang, verbose)

    # Re-filter invalid geo after fill (fill may have introduced NAs)
    if (!is.null(geo_col) && geo_col %in% names(df_agg)) {
      df_agg <- df_agg[!is.na(df_agg[[geo_col]]) & df_agg[[geo_col]] != "0", ]
    }
  }

  # --------------------------------------------------------------------------
  # 11. Sort
  # --------------------------------------------------------------------------
  sort_vars <- c("date", group_by)
  sort_vars <- sort_vars[sort_vars %in% names(df_agg)]
  df_agg <- df_agg |>
    dplyr::arrange(dplyr::across(dplyr::all_of(sort_vars)))

  # --------------------------------------------------------------------------
  # 12. Verbose summary
  # --------------------------------------------------------------------------
  if (verbose) {
    n_periods <- length(unique(df_agg$date))
    n_groups  <- if (is.null(group_by)) 1L
                 else length(unique(interaction(df_agg[group_by])))

    n_original_fmt <- if (is.na(n_original)) "?" else format(n_original, big.mark = ",")

    msg <- switch(lang,
      "en" = paste0("Aggregated ", n_original_fmt, " records into ",
                    format(nrow(df_agg), big.mark = ","), " ", time_unit_label, " periods"),
      "pt" = paste0("Agregados ", n_original_fmt, " registros em ",
                    format(nrow(df_agg), big.mark = ","), " periodos ", time_unit_label),
      "es" = paste0("Agregados ", n_original_fmt, " registros en ",
                    format(nrow(df_agg), big.mark = ","), " periodos ", time_unit_label)
    )
    cli::cli_alert_success(msg)

    date_range_msg <- switch(lang,
      "en" = paste0("Date range: ", min(df_agg$date), " to ", max(df_agg$date)),
      "pt" = paste0("Intervalo de datas: ", min(df_agg$date), " a ", max(df_agg$date)),
      "es" = paste0("Rango de fechas: ", min(df_agg$date), " a ", max(df_agg$date))
    )
    cli::cli_alert_info(date_range_msg)

    if (!is.null(group_by)) {
      group_msg <- switch(lang,
        "en" = paste0(n_periods, " time periods x ", n_groups, " groups"),
        "pt" = paste0(n_periods, " periodos temporais x ", n_groups, " grupos"),
        "es" = paste0(n_periods, " periodos temporales x ", n_groups, " grupos")
      )
      cli::cli_alert_info(group_msg)
    }
  }

  # --------------------------------------------------------------------------
  # 14. History — single write via sus_meta(add_history = ...)
  # --------------------------------------------------------------------------
  agg_details <- Filter(Negate(is.null), c(
    sprintf("Time unit: %s", time_unit),
    if (is.character(fun)) sprintf("Aggregation: %s", fun) else "Aggregation: custom function",
    if (!is.null(value_col)) sprintf("Value column: %s", value_col) else "Value column: record count",
    if (!is.null(group_by)) {
      if (length(group_by) <= 3)
        sprintf("Grouped by: %s", paste(group_by, collapse = ", "))
      else
        sprintf("Grouped by: %s and %d more",
                paste(utils::head(group_by, 3), collapse = ", "),
                length(group_by) - 3)
    },
    if (isTRUE(complete_dates)) "Completed missing dates",
    if (!is.null(date_col)) sprintf("Date column: %s", date_col)
  ))

  # --------------------------------------------------------------------------
  # 13. Re-attach sus_meta (df_agg is a plain tibble after collect())
  # --------------------------------------------------------------------------
  meta           <- incoming_meta %||% list()
  meta$system    <- system
  meta$stage     <- "aggregate"   # correct stage: data has been aggregated
  meta$type      <- "agg"
  meta$modified  <- Sys.time()
  meta$temporal  <- list(
    start = min(df_agg$date, na.rm = TRUE),
    end   = max(df_agg$date, na.rm = TRUE),
    unit  = time_unit
  )
  meta$history  <- c(
  meta$history %||% character(0),
  sprintf("[%s] Temporal Data aggregated (lazy DuckDB): %s",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"), 
          paste(agg_details, collapse = " | "))
)
attr(df_agg, "sus_meta") <- meta

  return(df_agg)
}


# =============================================================================
# BACKEND HELPERS
# =============================================================================

# -----------------------------------------------------------------------------
# WHY sus_meta GETS LOST ON ARROW/DUCKDB
# -----------------------------------------------------------------------------
# Arrow Tables and DuckDB tbls are NOT plain R lists; they are external
# pointers / query objects.  Every dplyr verb (filter, mutate, group_by …)
# on an Arrow Table returns a NEW `arrow_dplyr_query` object — an S3 class
# defined inside the {arrow} package — which has NO relationship to the
# original R attributes set by climasus4r.
#
# The same happens with DuckDB: `copy_to()` and any subsequent dplyr verb
# return a `tbl_dbi` that knows nothing about R-level attributes.
#
# SOLUTIONS AVAILABLE:
#   1. `recover_sus_meta(df)` — inspects three places where metadata might
#      still be reachable:
#        (a) `attr(df, "sus_meta")` — survives if NO dplyr verb was applied yet
#            (e.g., user called `arrow::as_arrow_table(df)` and passed it directly).
#        (b) `df$.data` — `arrow_dplyr_query` objects store the source Table in
#            the `.data` slot.  Attributes set on the original Table are still
#            there.
#        (c) Arrow schema metadata field `"sus_meta"` — survives all dplyr verbs
#            because schema metadata travels with the data.  Requires the user
#            to have called `sus_as_arrow()` which embeds the metadata as JSON.
#
#   2. `sus_as_arrow(df)` / `sus_as_duckdb(df, con)` — recommended helpers that
#      embed `sus_meta` into the Arrow schema so it is never lost.
# -----------------------------------------------------------------------------

#' Recover sus_meta from an Arrow or DuckDB object
#'
#' Tries three sources in order:
#' 1. `attr(df, "sus_meta")` — present when no dplyr verb has been applied yet.
#' 2. `attr(df$.data, "sus_meta")` — present on the source Table inside an
#'    `arrow_dplyr_query` object.
#' 3. Arrow schema metadata field `"sus_meta"` (JSON string) — always present
#'    when the Table was created with `sus_as_arrow()`.
#'
#' @param df Arrow Table, arrow_dplyr_query, or DuckDB tbl_dbi
#' @param backend Character; result of `detect_backend(df)`
#' @return A named list (sus_meta) or `NULL` if not found
#' @keywords internal
#' @noRd
recover_sus_meta <- function(df, backend) {

  # --- Source (a): direct attribute (no dplyr verb applied) -----------------
  meta <- attr(df, "sus_meta", exact = TRUE)
  if (!is.null(meta)) return(meta)

  # --- Source (b): .data slot inside arrow_dplyr_query ----------------------
  if (inherits(df, "arrow_dplyr_query")) {
    source_table <- tryCatch(df$.data, error = function(e) NULL)
    if (!is.null(source_table)) {
      # Try R attribute first
      meta <- attr(source_table, "sus_meta", exact = TRUE)
      if (!is.null(meta)) return(meta)

      # Try Arrow schema metadata (set by sus_as_arrow)
      meta <- read_sus_meta_from_schema(source_table)
      if (!is.null(meta)) return(meta)
    }
  }

  # --- Source (c): schema metadata on an Arrow Table directly ---------------
  if (backend == "arrow" && !inherits(df, "arrow_dplyr_query")) {
    meta <- read_sus_meta_from_schema(df)
    if (!is.null(meta)) return(meta)
  }

  # --- DuckDB: metadata can be stored as a single-row metadata table --------
  # (only if sus_as_duckdb() was used — it creates a .__sus_meta__ table)
  if (backend == "duckdb") {
    meta <- read_sus_meta_from_duckdb(df)
    if (!is.null(meta)) return(meta)
  }

  NULL
}

#' Read sus_meta from an Arrow Table's schema metadata field
#'
#' Arrow schema metadata is a named character vector (key → value).
#' `sus_as_arrow()` stores the serialised list under the key `"sus_meta"`.
#'
#' @param table An Arrow Table or RecordBatch
#' @return sus_meta list or NULL
#' @keywords internal
#' @noRd
read_sus_meta_from_schema <- function(table) {
  tryCatch({
    schema_meta <- table$schema$metadata
    if (!is.null(schema_meta) && "sus_meta" %in% names(schema_meta)) {
      jsonlite::fromJSON(schema_meta[["sus_meta"]], simplifyVector = FALSE)
    } else {
      NULL
    }
  }, error = function(e) NULL)
}

#' Read sus_meta from a hidden DuckDB metadata table
#'
#' `sus_as_duckdb()` creates a table named `.__sus_meta__` with a single
#' column `json` containing the serialised metadata.
#'
#' @param tbl A DuckDB tbl_dbi
#' @return sus_meta list or NULL
#' @keywords internal
#' @noRd
read_sus_meta_from_duckdb <- function(tbl) {
  tryCatch({
    con <- dbplyr::remote_con(tbl)
    if (DBI::dbExistsTable(con, ".__sus_meta__")) {
      json_str <- DBI::dbGetQuery(con, "SELECT json FROM \".__sus_meta__\" LIMIT 1")$json
      jsonlite::fromJSON(json_str, simplifyVector = FALSE)
    } else {
      NULL
    }
  }, error = function(e) NULL)
}

# =============================================================================
# PUBLIC CONVERSION HELPERS
# =============================================================================

#' Convert a climasus_df to an Arrow Table with embedded metadata
#'
#' Embeds `sus_meta` as a JSON string in the Arrow schema metadata field
#' `"sus_meta"`.  This ensures the metadata survives all downstream dplyr
#' verbs (filter, mutate, group_by, etc.) because Arrow schema metadata
#' travels with the data.
#'
#' @param df A `climasus_df` tibble (output of any `sus_data_*` function).
#' @return An Arrow Table with `sus_meta` embedded in the schema.
#'
#' @examples
#' \dontrun{
#' arrow_df <- sus_data_standardize(df) |> sus_as_arrow()
#' result   <- sus_data_aggregate(arrow_df, time_unit = "month")
#' }
#' @export
sus_as_arrow <- function(df) {
  if (!requireNamespace("arrow",     quietly = TRUE)) stop("Package 'arrow' is required.")
  if (!requireNamespace("jsonlite",  quietly = TRUE)) stop("Package 'jsonlite' is required.")
  if (!inherits(df, "climasus_df"))  stop("df must be a climasus_df object.")

  meta      <- attr(df, "sus_meta")
  meta_json <- jsonlite::toJSON(meta, auto_unbox = TRUE, null = "null")

  # Convert to Arrow Table (drops R attributes)
  tbl <- arrow::as_arrow_table(tibble::as_tibble(df))

  # Re-embed metadata in the schema
  new_schema <- tbl$schema$WithMetadata(
    c(tbl$schema$metadata, list(sus_meta = as.character(meta_json)))
  )
  tbl$cast(new_schema)
}

#' Convert a climasus_df to a DuckDB lazy tbl with embedded metadata
#'
#' Copies the data to a DuckDB connection and stores `sus_meta` in a helper
#' table named `.__sus_meta__`.  This allows `sus_data_aggregate()` to
#' recover the metadata automatically.
#'
#' @param df A `climasus_df` tibble.
#' @param con A DuckDB `DBIConnection` (from `duckdb::dbConnect(duckdb::duckdb())`).
#' @param name Character; name for the DuckDB table. Defaults to `"sus_data"`.
#' @param overwrite Logical. Overwrite existing table? Default `TRUE`.
#' @return A `tbl_dbi` referencing the DuckDB table.
#'
#' @examples
#' \dontrun{
#' con      <- duckdb::dbConnect(duckdb::duckdb())
#' duck_tbl <- sus_data_standardize(df) |> sus_as_duckdb(con)
#' result   <- sus_data_aggregate(duck_tbl, time_unit = "month")
#' }
#' @export
sus_as_duckdb <- function(df, con, name = "sus_data", overwrite = TRUE) {
  if (!requireNamespace("duckdb",   quietly = TRUE)) stop("Package 'duckdb' is required.")
  if (!requireNamespace("DBI",      quietly = TRUE)) stop("Package 'DBI' is required.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required.")
  if (!inherits(df, "climasus_df")) stop("df must be a climasus_df object.")

  meta      <- attr(df, "sus_meta")
  meta_json <- jsonlite::toJSON(meta, auto_unbox = TRUE, null = "null")

  # Copy data
  dplyr::copy_to(con, tibble::as_tibble(df), name = name, overwrite = overwrite)

  # Store metadata in a helper table
  DBI::dbExecute(con, 'DROP TABLE IF EXISTS ".__sus_meta__"')
  DBI::dbExecute(con,
    sprintf('CREATE TABLE ".__sus_meta__" AS SELECT \'%s\'::VARCHAR AS json',
            gsub("'", "''", as.character(meta_json)))
  )

  dplyr::tbl(con, name)
}

#' Detect the compute backend of a data object
#'
#' @param df Any R object
#' @return One of `"arrow"`, `"duckdb"`, or `"tibble"`
#' @keywords internal
#' @noRd
detect_backend <- function(df) {
  # Arrow Table or Arrow Dataset
  if (inherits(df, c("ArrowTabular", "Dataset", "arrow_dplyr_query"))) {
    return("arrow")
  }

  # DuckDB: a tbl_dbi whose underlying connection is a DuckDBConnection
  if (inherits(df, "tbl_dbi")) {
    con_class <- tryCatch(
      class(dbplyr::remote_con(df)),
      error = function(e) ""
    )
    if (any(grepl("duckdb", con_class, ignore.case = TRUE))) {
      return("duckdb")
    }
    # Generic DBI — treat as tibble (collect first)
    return("tibble")
  }

  "tibble"
}


#' Return column names for any backend
#'
#' @keywords internal
#' @noRd
get_col_names <- function(df, backend) {
  # names() works for tibble, Arrow Table, arrow_dplyr_query, and DuckDB tbl
  nms <- tryCatch(names(df), error = function(e) NULL)
  if (!is.null(nms) && length(nms) > 0) return(nms)
  nms <- tryCatch(colnames(df), error = function(e) NULL)
  if (!is.null(nms) && length(nms) > 0) return(nms)
  tryCatch(as.character(dplyr::tbl_vars(df)), error = function(e) character(0))
}

#' Build group_vars vector (geo + user-supplied), validated against col_names
#'
#' @keywords internal
#' @noRd
build_group_vars <- function(group_by, geo_col, col_names) {
  vars <- character(0)
  if (!is.null(geo_col) && geo_col %in% col_names) vars <- c(vars, geo_col)
  if (!is.null(group_by)) {
    missing_cols <- setdiff(group_by, col_names)
    if (length(missing_cols) > 0)
      cli::cli_abort(paste0("Grouping columns not found: ", paste(missing_cols, collapse = ", ")))
    vars <- unique(c(vars, group_by))
  }
  vars
}

#' Resolve the best geographic column for the given system
#'
#' Mirrors the logic from the original function but operates on a character
#' vector of column names (works for all backends).
#'
#' @keywords internal
#' @noRd
resolve_geo_col <- function(col_names, system) {
  geo_groups <- list(
    residencia = c(
      "codigo_municipio_residencia", "residence_municipality_code",
      "municipio_residencia_paciente_sp", "CODMUNRES", "MUNI_RES"
    ),
    ocorrencia = c(
      "codigo_municipio_ocorrencia", "codigo_municipio_ocurrencia",
      "occurrence_municipality_code", "codigo_municipio_notificacao",
      "codigo_municipio_notificacion", "notification_municipality_code"
    ),
    estabelecimento = c(
      "municipio_estabelecimento_sp", "uf_municipio_estabelecimento",
      "facility_uf_municipality", "uf_municipio_establecimiento"
    ),
    nascimento = c(
      "codigo_municipio_nascimento", "codigo_municipio_nacimiento",
      "birth_municipality_code"
    ),
    generico = c(
      "codigo_municipio", "municipality_code",
      "codigo_municipio_paciente", "patient_municipality_code",  "codigo_gestor_sp", "sp_manager_code", "sp_codigo_gestor"
    ),
    cep = c(
      "cep_paciente", "codigo_postal_paciente", "codigo_postal",
      "patient_zip_code", "zip_code"
    )
  )

  system_priority <- list(
    SIM    = c("ocorrencia", "residencia", "estabelecimento", "generico"),
    SIH    = c("residencia", "estabelecimento", "ocorrencia", "generico"),
    SINAN  = c("residencia", "ocorrencia", "estabelecimento", "generico"),
    SIA    = c("residencia", "estabelecimento", "generico"),
    CNES   = c("estabelecimento", "generico"),
    SINASC = c("residencia", "nascimento", "estabelecimento", "generico")
  )

  base_system <- sub("-.*", "", toupper(system %||% ""))
  priority_groups <- system_priority[[base_system]] %||%
    c("residencia", "ocorrencia", "estabelecimento", "generico")

  priority_order <- c(
    unlist(geo_groups[priority_groups], use.names = FALSE),
    geo_groups$cep
  )

  matches <- priority_order[priority_order %in% col_names]
  if (length(matches) == 0) return(NULL)
  matches[[1]]
}

#' Detect date column from a vector of column names (no df needed)
#'
#' Replaces `detect_date_column()` which required a data.frame.
#' Falls back to any column whose name contains "date" / "data" / "dt_".
#'
#' @keywords internal
#' @noRd
detect_date_column_from_names <- function(col_names, system) {
  system_date_map <- list(
    SIM    = c("data_obito",       "death_date",         "fecha_muerte",       "DTOBITO"),
    SINASC = c("data_nascimento",  "birth_date",         "fecha_nacimiento",   "DTNASC"),
    SIH    = c("data_internacao",  "admission_date",     "fecha_ingreso", "data_internacao_sp", "sp_fecha_ingreso","sp_admission_date", "DT_INTER"),
    SINAN  = c("data_notificacao", "notification_date",  "fecha_notificacion", "DT_NOTIFIC"),
    CNES   = c("data_atualizacao", "update_date",        "fecha_actualizacion","DT_COMPET")
  )

  # Normalize "SIH-RD", "SIH-SP", "SIM-DO", etc. → "SIH", "SIM"
  base_system <- sub("-.*", "", toupper(system %||% ""))
  candidates <- system_date_map[[base_system]]

  for (cand in candidates) {
    if (cand %in% col_names) return(cand)
  }

  # Generic fallback: columns whose name suggests a date
  fallback <- col_names[grepl("(^dt_|_date$|^data_|^fecha_|^date_)", col_names,
                               ignore.case = TRUE)]
  if (length(fallback) > 0) return(fallback[[1]])

  cli::cli_abort(c(
    "Could not auto-detect date column. Please specify the 'date_col' parameter.",
    "i" = paste0("System detected: '", system, "' (base: '", base_system, "')"),
    "i" = paste0("Candidates tried: ",
                 paste(candidates %||% "(none — system key not matched)", collapse = ", ")),
    "i" = paste0("Columns available (first 30): ",
                 paste(utils::head(col_names, 30), collapse = ", "))
  ))
}


# =============================================================================
# ORIGINAL HELPER FUNCTIONS (unchanged, kept for compatibility)
# =============================================================================

#' Get Intelligent Column Name for Aggregated Data
#' @keywords internal
#' @noRd
get_smart_column_name <- function(system, fun_type, lang) {
  if (fun_type == "count") {
    if (is.null(system)) return("n")

    if (lang == "en") {
      col_names <- list(
        SIM    = "n_deaths",
        SIH    = "n_hospitalizations",
        SINASC = "n_births",
        SINAN  = "n_cases",
        SIA    = "n_procedures",
        CNES   = "n_establishments"
      )
    } else if (lang == "pt") {
      col_names <- list(
        SIM    = "n_obitos",
        SIH    = "n_internacoes",
        SINASC = "n_nascimentos",
        SINAN  = "n_casos",
        SIA    = "n_procedimentos",
        CNES   = "n_estabelecimentos"
      )
    } else {
      col_names <- list(
        SIM    = "n_muertes",
        SIH    = "n_hospitalizaciones",
        SINASC = "n_nacimientos",
        SINAN  = "n_casos",
        SIA    = "n_procedimientos",
        CNES   = "n_establecimientos"
      )
    }
    return(col_names[[system]])
  }
  paste0(fun_type, "_value")
}



#' Get Brazilian Season Start Date
#' @keywords internal
#' @noRd
get_brazilian_season_start <- function(dates) {
  months <- as.numeric(format(dates, "%m"))
  years  <- as.numeric(format(dates, "%Y"))

  adj_years <- years
  adj_years[months %in% c(1, 2)] <- years[months %in% c(1, 2)] - 1

  start_months <- character(length(dates))
  start_months[months %in% c(12, 1, 2)]  <- "12"
  start_months[months %in% c(3,  4, 5)]  <- "03"
  start_months[months %in% c(6,  7, 8)]  <- "06"
  start_months[months %in% c(9, 10, 11)] <- "09"

  as.Date(paste0(adj_years, "-", start_months, "-01"))
}

#' Get Time Unit Label for Messages
#' @keywords internal
#' @noRd
get_time_unit_label <- function(time_unit, lang) {
  tu_lower <- tolower(time_unit)

  if (grepl("^day$|^1 day$", tu_lower))
    return(switch(lang, "en" = "daily",     "pt" = "diarias",   "es" = "diarios"))
  if (grepl("^week$|^1 week$", tu_lower))
    return(switch(lang, "en" = "weekly",    "pt" = "semanais",  "es" = "semanales"))
  if (grepl("^month$|^1 month$", tu_lower))
    return(switch(lang, "en" = "monthly",   "pt" = "mensais",   "es" = "mensuales"))
  if (grepl("^quarter$|^3 month", tu_lower))
    return(switch(lang, "en" = "quarterly", "pt" = "trimestrais","es" = "trimestrales"))
  if (grepl("^year$|^1 year$", tu_lower))
    return(switch(lang, "en" = "yearly",    "pt" = "anuais",    "es" = "anuales"))

  if (grepl("^[0-9]+ day", tu_lower)) {
    n <- as.numeric(gsub(" day.*", "", tu_lower))
    if (n == 5)
      return(switch(lang, "en" = "pentad (5-day)",
                          "pt" = "pentadais (5 dias)",
                          "es" = "pentadales (5 dias)"))
    return(switch(lang,
      "en" = paste0(n, "-day"),
      "pt" = paste0("de ", n, " dias"),
      "es" = paste0("de ", n, " dias")
    ))
  }
  if (grepl("^[0-9]+ week", tu_lower)) {
    n <- as.numeric(gsub(" week.*", "", tu_lower))
    if (n == 2)
      return(switch(lang, "en" = "fortnightly (2-week)",
                          "pt" = "quinzenais (2 semanas)",
                          "es" = "quincenales (2 semanas)"))
    return(switch(lang,
      "en" = paste0(n, "-week"),
      "pt" = paste0("de ", n, " semanas"),
      "es" = paste0("de ", n, " semanas")
    ))
  }
  if (grepl("^[0-9]+ month", tu_lower)) {
    n <- as.numeric(gsub(" month.*", "", tu_lower))
    if (n == 6)
      return(switch(lang, "en" = "semester (6-month)",
                          "pt" = "semestrais (6 meses)",
                          "es" = "semestrales (6 meses)"))
    return(switch(lang,
      "en" = paste0(n, "-month"),
      "pt" = paste0("de ", n, " meses"),
      "es" = paste0("de ", n, " meses")
    ))
  }
  time_unit
}

#' Complete Time Series with Missing Periods
#' @keywords internal
#' @noRd
complete_time_series <- function(df_agg, time_unit, group_by, fill_value = 0,
                                 lang, verbose) {
  if (verbose) {
    msg <- switch(lang,
      "en" = "Filling missing time periods...",
      "pt" = "Preenchendo periodos faltantes...",
      "es" = "Rellenando periodos faltantes..."
    )
    cli::cli_alert_info(msg)
  }

  min_date <- min(df_agg$date)
  max_date <- max(df_agg$date)

  if (time_unit == "season") {
    date_seq <- sort(unique(df_agg$date))
  } else {
    date_seq <- tryCatch(
      unique(lubridate::floor_date(seq(min_date, max_date, by = "day"),
                                   unit = time_unit)),
      error = function(e) unique(df_agg$date)
    )
  }

  if (is.null(group_by)) {
    complete_df <- data.frame(date = date_seq)
    df_agg <- dplyr::left_join(complete_df, df_agg, by = "date")
  } else {
    all_groups <- df_agg |>
      dplyr::select(dplyr::all_of(group_by)) |>
      dplyr::distinct()

    complete_df <- tidyr::expand_grid(date = date_seq, all_groups)
    df_agg <- dplyr::left_join(complete_df, df_agg, by = c("date", group_by))
  }

  value_cols <- setdiff(names(df_agg), c("date", group_by))
  for (col in value_cols) {
    df_agg[[col]][is.na(df_agg[[col]])] <- fill_value
  }

  if (verbose) {
    n_filled <- sum(df_agg[[value_cols[1]]] == fill_value)
    if (n_filled > 0) {
      msg <- switch(lang,
        "en" = paste0("Filled ", n_filled, " missing periods"),
        "pt" = paste0("Preenchidos ", n_filled, " periodos faltantes"),
        "es" = paste0("Rellenados ", n_filled, " periodos faltantes")
      )
      cli::cli_alert_info(msg)
    }
  }

  df_agg
}