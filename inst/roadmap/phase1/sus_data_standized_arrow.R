#' Standardize SUS data column names and values
#'
#' Standardizes column names and categorical values in SUS datasets, ensuring
#' consistency across different years and versions. Supports three languages:
#' English (en), Portuguese (pt), and Spanish (es).
#'
#' Accepts three input types:
#' \itemize{
#'   \item \code{data.frame} / \code{tibble} / \code{climasus_df} — processed
#'         in-memory (same behaviour as \code{sus_data_standardize}).
#'   \item Arrow \code{Dataset} / \code{arrow_dplyr_query} / \code{climasus_dataset}
#'         — rename and recode applied lazily via Arrow compute; no \code{collect()}.
#'   \item DuckDB \code{tbl_dbi} / \code{tbl_lazy} — rename and recode translated
#'         to SQL by dbplyr; no \code{collect()}.
#' }
#'
#' @param df A \code{data.frame}, \code{tibble}, Arrow \code{Dataset}
#'   (\code{climasus_dataset}), or DuckDB lazy table (\code{tbl_dbi} /
#'   \code{tbl_lazy}).
#' @param lang Character. Output language for column names and values.
#'   Options: \code{"en"} (English), \code{"pt"} (Portuguese, default),
#'   \code{"es"} (Spanish).
#' @param translate_columns Logical. If \code{TRUE}, translates column names.
#'   Default \code{TRUE}.
#' @param standardize_values Logical. If \code{TRUE}, standardizes categorical
#'   values. Default \code{TRUE}.
#' @param keep_original Logical. If \code{TRUE}, keeps original columns
#'   alongside standardized ones (suffix \code{_traduzido} for Arrow/DuckDB,
#'   \code{_translated} for tibble). Default \code{FALSE}.
#' @param verbose Logical. If \code{TRUE}, prints a report of standardization
#'   actions. Default \code{TRUE}.
#'
#' @return
#' \itemize{
#'   \item For Arrow/DuckDB inputs: the same lazy object with renamed columns
#'         and recoded values applied as a lazy plan; \code{sus_meta} attribute
#'         updated (\code{stage = "stand"}).
#'   \item For tibble/data.frame inputs: a \code{climasus_df} with standardized
#'         column names and values (\code{stage = "stand"}).
#' }
#'
#' @importFrom dplyr rename mutate case_when collect
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Standard tibble pipeline
#' df_pt <- sus_data_standardize_arrow(df_raw, lang = "pt")
#'
#' # Arrow lazy pipeline — standardize without collecting
#' ds <- sus_data_import_arrow(uf = "SP", year = 2023, system = "SIH-SP")
#' ds_std <- sus_data_standardize_arrow(ds, lang = "pt")
#' df     <- dplyr::collect(ds_std)   # collect after all lazy ops
#'
#' # DuckDB lazy pipeline
#' con <- duckdb::dbConnect(duckdb::duckdb())
#' tbl <- dplyr::tbl(con, "sih_view")
#' tbl_std <- sus_data_standardize_arrow(tbl, lang = "pt")
#' df      <- dplyr::collect(tbl_std)
#' }
sus_data_standardize_arrow <- function(df,
                                       lang               = "pt",
                                       translate_columns  = TRUE,
                                       standardize_values = TRUE,
                                       keep_original      = FALSE,
                                       verbose            = TRUE) {

  if (!lang %in% c("en", "pt", "es"))
    cli::cli_abort("Language must be 'en', 'pt', or 'es'")

  messages  <- .get_ui_messages_std(lang)
  is_arrow  <- inherits(df, c("Dataset", "ArrowObject", "arrow_dplyr_query",
                               "climasus_dataset", "climasus_dataset_standardize"))
  is_duckdb <- inherits(df, c("tbl_dbi", "tbl_lazy"))

  # ── Arrow lazy path ──────────────────────────────────────────────────────────
  if (is_arrow) {
    incoming_meta <- attr(df, "sus_meta")
    schema_names  <- names(df)   # works on Dataset and arrow_dplyr_query
    system        <- .std_detect_system(incoming_meta, schema_names)
    translations  <- .std_load_translations(system, lang)

    if (verbose) {
      cli::cli_h1(messages$standardize_title)
      cli::cli_alert_info(paste(messages$detected_system,
                                get_system_description(system, lang), "(Arrow lazy)"))
      cli::cli_alert_info(paste(messages$language, toupper(lang)))
    }

    # 1a. Column renaming — lazy (Arrow C++ engine renames the projection)
    n_translated <- 0L
    if (translate_columns && length(translations$columns) > 0) {
      cols_present <- intersect(names(translations$columns), schema_names)
      if (length(cols_present) > 0) {
        n_translated <- length(cols_present)
        # dplyr::rename(new = old): setNames(old_vec, new_vec) → c(new = "old")
        rename_vec <- setNames(cols_present,
                               unlist(translations$columns[cols_present]))
        df <- dplyr::rename(df, !!!rlang::syms(rename_vec))
      }
    }
    if (verbose)
      cli::cli_alert_success(paste(messages$columns_translated, n_translated,
                                   "of", length(schema_names)))

    # 1b. Value recoding — lazy via dplyr::mutate + case_when (Arrow compute)
    #
    # Arrow is schema-strict: comparing an int32 column to a character literal
    # silently returns NULL. Cast every column to utf8 before comparison so the
    # mapping keys (always character) are type-compatible.
    n_vars_recoded <- 0L
    if (standardize_values && length(translations$values) > 0) {
      if (verbose) cli::cli_alert_info(messages$translating_values)
      current_cols <- names(df)   # updated names after rename

      for (var_name in names(translations$values)) {
        col <- .std_resolve_col(var_name, current_cols, translations$columns)
        if (is.null(col)) next

        mapping  <- translations$values[[var_name]]
        dest_col <- if (keep_original) paste0(col, "_traduzido") else col

        # Build case_when arms: cast to utf8 for type-safe comparison
        cases <- lapply(names(mapping), function(k) {
          rlang::expr(as.character(!!rlang::sym(col)) == !!k ~ !!mapping[[k]])
        })

        df <- dplyr::mutate(df,
          !!dest_col := dplyr::case_when(
            !!!cases,
            TRUE ~ as.character(!!rlang::sym(col))
          )
        )
        n_vars_recoded <- n_vars_recoded + 1L
      }

      if (verbose)
        cli::cli_alert_success(paste(messages$values_translated,
                                     n_vars_recoded, "variables"))
    }

    # 1c. Date column casting — attempt Arrow-native ymd() (YYYYMMDD → Date32)
    #     Wrapped in tryCatch: skipped gracefully if Arrow version or column
    #     type does not support the compute function.
    date_pattern <- switch(lang, "en" = "_date", "pt" = "data_", "es" = "fecha_")
    for (col in names(df)[grepl(date_pattern, names(df), ignore.case = TRUE)]) {
      df <- tryCatch(
        dplyr::mutate(df, !!col := lubridate::ymd(!!rlang::sym(col))),
        error = function(e) df
      )
    }

    # 1d. Update sus_meta on the lazy wrapper
    meta          <- incoming_meta %||% list()
    meta$system   <- system
    meta$stage    <- "stand"
    meta$type     <- "stand"
    meta$modified <- Sys.time()
    meta$history  <- c(
      meta$history %||% character(0),
      sprintf("[%s] Standardized (lazy Arrow): system=%s, lang=%s",
              format(Sys.time(), "%Y-%m-%d %H:%M:%S"), system, toupper(lang))
    )
    attr(df, "sus_meta") <- meta

    if (verbose)
      cli::cli_alert_success(paste(messages$standardization_complete,
                                   ncol(df), "columns (lazy)"))
    return(df)
  }

  # ── DuckDB lazy path ─────────────────────────────────────────────────────────
  if (is_duckdb) {
    incoming_meta <- attr(df, "sus_meta")
    schema_names  <- names(df)   # dbplyr tracks projected names
    system        <- .std_detect_system(incoming_meta, schema_names)
    translations  <- .std_load_translations(system, lang)

    if (verbose) {
      cli::cli_h1(messages$standardize_title)
      cli::cli_alert_info(paste(messages$detected_system,
                                get_system_description(system, lang), "(DuckDB lazy)"))
      cli::cli_alert_info(paste(messages$language, toupper(lang)))
    }

    # 2a. Column renaming — dbplyr translates to SQL column aliases
    n_translated <- 0L
    if (translate_columns && length(translations$columns) > 0) {
      cols_present <- intersect(names(translations$columns), schema_names)
      if (length(cols_present) > 0) {
        n_translated <- length(cols_present)
        rename_vec <- setNames(cols_present,
                               unlist(translations$columns[cols_present]))
        df <- dplyr::rename(df, !!!rlang::syms(rename_vec))
      }
    }
    if (verbose)
      cli::cli_alert_success(paste(messages$columns_translated, n_translated,
                                   "of", length(schema_names)))

    # 2b. Value recoding — dbplyr translates case_when to SQL CASE WHEN.
    #     as.character() translates to CAST(col AS TEXT) for type safety.
    n_vars_recoded <- 0L
    if (standardize_values && length(translations$values) > 0) {
      if (verbose) cli::cli_alert_info(messages$translating_values)
      current_cols <- names(df)

      for (var_name in names(translations$values)) {
        col <- .std_resolve_col(var_name, current_cols, translations$columns)
        if (is.null(col)) next

        mapping  <- translations$values[[var_name]]
        dest_col <- if (keep_original) paste0(col, "_traduzido") else col

        cases <- lapply(names(mapping), function(k) {
          rlang::expr(as.character(!!rlang::sym(col)) == !!k ~ !!mapping[[k]])
        })

        df <- dplyr::mutate(df,
          !!dest_col := dplyr::case_when(
            !!!cases,
            TRUE ~ as.character(!!rlang::sym(col))
          )
        )
        n_vars_recoded <- n_vars_recoded + 1L
      }

      if (verbose)
        cli::cli_alert_success(paste(messages$values_translated,
                                     n_vars_recoded, "variables"))
    }

    # 2c. Date column casting — as.Date() translates to SQL TRY_CAST / CAST
    date_pattern <- switch(lang, "en" = "_date", "pt" = "data_", "es" = "fecha_")
    for (col in names(df)[grepl(date_pattern, names(df), ignore.case = TRUE)]) {
      df <- tryCatch(
        dplyr::mutate(df, !!col := as.Date(!!rlang::sym(col))),
        error = function(e) df
      )
    }

    # 2d. Update sus_meta on the lazy wrapper
    meta          <- incoming_meta %||% list()
    meta$system   <- system
    meta$stage    <- "stand"
    meta$type     <- "stand"
    meta$modified <- Sys.time()
    meta$history  <- c(
      meta$history %||% character(0),
      sprintf("[%s] Standardized (lazy DuckDB): system=%s, lang=%s",
              format(Sys.time(), "%Y-%m-%d %H:%M:%S"), system, toupper(lang))
    )
    attr(df, "sus_meta") <- meta

    if (verbose)
      cli::cli_alert_success(paste(messages$standardization_complete,
                                   ncol(df), "columns (lazy)"))
    return(df)
  }

  # ── Tibble / data.frame path (identical to sus_data_standardize) ─────────────

  if (!is.data.frame(df))
    cli::cli_abort("Input must be a data frame")

  if (nrow(df) == 0) {
    if (verbose) cli::cli_alert_warning("Empty data frame - returning as is")
    return(df)
  }

  system       <- detect_health_system(df)
  translations <- .std_load_translations(system, lang)

  if (verbose) {
    cli::cli_h1(messages$standardize_title)
    cli::cli_alert_info(paste(messages$detected_system,
                              get_system_description(system, lang)))
    cli::cli_alert_info(paste(messages$language, toupper(lang)))
  }

  # Step 1: Translate column names
  if (translate_columns) {
    if (verbose) cli::cli_alert_info(messages$translating_columns)

    original_names <- names(df)
    new_names      <- original_names

    for (i in seq_along(original_names)) {
      if (original_names[i] %in% names(translations$columns))
        new_names[i] <- translations$columns[[original_names[i]]]
    }

    n_translated <- sum(original_names != new_names)

    if (keep_original) {
      for (i in seq_along(original_names)) {
        if (original_names[i] != new_names[i])
          df[[new_names[i]]] <- df[[original_names[i]]]
      }
    } else {
      names(df) <- new_names
    }

    if (verbose)
      cli::cli_alert_success(paste(messages$columns_translated, n_translated,
                                   "of", length(original_names)))
  }

  # Step 2: Translate categorical values
  if (standardize_values) {
    if (verbose) cli::cli_alert_info(messages$translating_values)

    current_names          <- names(df)
    n_variables_translated <- 0L
    n_values_translated    <- 0L

    for (var_name in names(translations$values)) {
      col_name <- .std_resolve_col(var_name, current_names, translations$columns)
      if (is.null(col_name)) next

      value_map       <- translations$values[[var_name]]
      original_values <- as.character(df[[col_name]])
      new_values      <- original_values

      for (code in names(value_map))
        new_values[original_values == code] <- value_map[[code]]

      n_changed <- sum(original_values != new_values, na.rm = TRUE)
      if (n_changed > 0) {
        dest <- if (keep_original) paste0(col_name, "_translated") else col_name
        df[[dest]] <- as.factor(new_values)
        n_variables_translated <- n_variables_translated + 1L
        n_values_translated    <- n_values_translated + n_changed
      }
    }

    if (verbose)
      cli::cli_alert_success(paste(messages$values_translated,
                                   n_variables_translated, "variables,",
                                   n_values_translated, "values"))
  }

  # Step 3: Date formatting
  date_pattern <- switch(lang, "en" = "_date", "pt" = "data_", "es" = "fecha_")
  cols_to_fix  <- names(df)[grepl(date_pattern, names(df), ignore.case = TRUE)]

  for (col in cols_to_fix) {
    vals <- as.character(df[[col]])
    suppressWarnings(
      parsed_dates <- lubridate::parse_date_time(
        vals,
        orders    = c("dmy", "dmY", "ymd", "Ymd", "dmy HMS", "ymd HMS", "ym", "my"),
        truncated = 3
      )
    )
    if (!all(is.na(parsed_dates)) || all(is.na(vals)))
      df[[col]] <- as.Date(parsed_dates)
  }

  names(df) <- make.unique(names(df), sep = "_")

  # Step 4: Final summary
  if (verbose)
    cli::cli_alert_success(paste(messages$standardization_complete,
                                 nrow(df), "rows,", ncol(df), "columns"))

  # Step 5: Attach / update sus_meta
  if (!inherits(df, "climasus_df")) {
    meta <- list(
      system   = system,
      stage    = "stand",
      type     = "stand",
      spatial  = inherits(df, "sf"),
      temporal = NULL,
      created  = Sys.time(),
      modified = Sys.time(),
      history  = sprintf("[%s] Standardized column names and types",
                         format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      user     = list()
    )
    base_classes <- setdiff(class(df), "climasus_df")
    df <- structure(df, sus_meta = meta, class = c("climasus_df", base_classes))
  } else {
    df <- sus_meta(df, system = system, stage = "stand", type = "stand")
    df <- sus_meta(df, add_history = "Standardized column names and types")
  }

  return(df)
}


# ==============================================================================
# Internal helpers
# ==============================================================================

#' Resolve which column name to use for a given value-map entry
#' @keywords internal
#' @noRd
.std_resolve_col <- function(var_name, current_cols, col_translations) {
  if (var_name %in% current_cols) return(var_name)
  if (var_name %in% names(col_translations)) {
    translated <- col_translations[[var_name]]
    if (!is.null(translated) && translated %in% current_cols) return(translated)
  }
  NULL
}

#' Detect DATASUS system from sus_meta or column names
#' @keywords internal
#' @noRd
.std_detect_system <- function(meta, col_names) {
  system <- meta$system %||% NULL
  if (!is.null(system)) return(sub("-.*", "", system))  # "SIH-SP" → "SIH"
  # Fallback: pass column names as a mock data frame to the column-heuristic
  detect_health_system(setNames(vector("list", length(col_names)), col_names))
}

#' Load translation dictionaries for a given system and language
#' @keywords internal
#' @noRd
.std_load_translations <- function(system, lang) {
  system_base <- sub("-.*", "", system %||% "UNKNOWN")
  tryCatch(
    switch(system_base,
      "SIM"    = switch(lang,
                   "en" = get_translation_dict_en(),
                   "pt" = get_translation_dict_pt(),
                          get_translation_dict_es()),
      "SIH"    = switch(lang,
                   "en" = get_translation_dict_en_sih(),
                   "pt" = get_translation_dict_pt_sih(),
                          get_translation_dict_es_sih()),
      "SINAN"  = switch(lang,
                   "en" = get_translation_dict_en_sinan(),
                   "pt" = get_translation_dict_pt_sinan(),
                          get_translation_dict_es_sinan()),
      "SIA"    = switch(lang,
                   "en" = get_translation_dict_en_sia(),
                   "pt" = get_translation_dict_pt_sia(),
                          get_translation_dict_es_sia()),
      "CNES"   = switch(lang,
                   "en" = get_translation_dict_en_cnes(),
                   "pt" = get_translation_dict_pt_cnes(),
                          get_translation_dict_es_cnes()),
      "SINASC" = switch(lang,
                   "en" = get_translation_dict_en_sinasc(),
                   "pt" = get_translation_dict_pt_sinasc(),
                          get_translation_dict_es_sinasc()),
      list(columns = list(), values = list())
    ),
    error = function(e) list(columns = list(), values = list())
  )
}

#' UI messages for sus_data_standardize_arrow
#' @keywords internal
#' @noRd
.get_ui_messages_std <- function(lang = "pt") {
  list(
    en = list(
      standardize_title        = "Standardizing SUS Data",
      detected_system          = "Detected system:",
      language                 = "Language:",
      translating_columns      = "Translating column names...",
      columns_translated       = "Translated",
      translating_values       = "Translating categorical values...",
      values_translated        = "Translated",
      standardization_complete = "Standardization complete:"
    ),
    pt = list(
      standardize_title        = "Padronizando Dados SUS",
      detected_system          = "Sistema detectado:",
      language                 = "Idioma:",
      translating_columns      = "Traduzindo nomes de colunas...",
      columns_translated       = "Traduzidas",
      translating_values       = "Traduzindo valores categoricos...",
      values_translated        = "Traduzidas",
      standardization_complete = "Padronizacao completa:"
    ),
    es = list(
      standardize_title        = "Estandarizando Datos SUS",
      detected_system          = "Sistema detectado:",
      language                 = "Idioma:",
      translating_columns      = "Traduciendo nombres de columnas...",
      columns_translated       = "Traducidas",
      translating_values       = "Traduciendo valores categoricos...",
      values_translated        = "Traducidas",
      standardization_complete = "Estandarizacion completa:"
    )
  )[[lang]]
}
