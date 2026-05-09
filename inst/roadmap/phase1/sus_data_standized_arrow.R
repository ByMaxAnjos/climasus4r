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

    
    # ===========================================================================
    # Valid
    # ===========================================================================
     is_arrow_input <- inherits(df, c(
      "climasus_dataset", "arrow_dplyr_query", "Dataset", "ArrowTabular", "Table"
    ))

    if (!is_arrow_input) {
      if (!inherits(df, "data.frame")) {
        cli::cli_abort(
          "`df` must be an Arrow object or a data.frame/tibble that can be converted to Arrow."
        )
      }

      df <- if (inherits(df, "climasus_df")) {
        as_arrow_climasus(df)
      } else {
        arrow::as_arrow_table(dplyr::as_tibble(df))
      }

      if (verbose) {
        cli::cli_alert_info(
          "Converted input data.frame to Arrow for lazy encoding checks."
        )
      }
    }
    
    if (!lang %in% c("en", "pt", "es"))
      cli::cli_abort("Language must be 'en', 'pt', or 'es'")

    messages  <- .get_ui_messages_std(lang)
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
          translated_names <- unlist(translations$columns[cols_present], use.names = FALSE)
          proposed_names <- schema_names
          names(proposed_names) <- schema_names
          proposed_names[cols_present] <- translated_names
          duplicated_targets <- unique(proposed_names[duplicated(proposed_names)])

          if (length(duplicated_targets) > 0) {
            keep_idx <- !translated_names %in% duplicated_targets
            cols_present <- cols_present[keep_idx]
            translated_names <- translated_names[keep_idx]
          }

          if (length(duplicated_targets) > 0 && verbose) {
            cli::cli_alert_warning(
              paste0(
                "Skipping duplicated translated name(s): ",
                paste(duplicated_targets, collapse = ", ")
              )
            )
          }

          n_translated <- length(cols_present)

          if (length(cols_present) > 0) {
            # dplyr::rename(new = old): setNames(old_vec, new_vec) → c(new = "old")
            rename_vec <- stats::setNames(cols_present, translated_names)
            df <- dplyr::rename(df, !!!rlang::syms(rename_vec))
          }
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

      if (verbose)
        cli::cli_alert_success(paste(messages$standardization_complete,
                                    ncol(df), "columns (lazy)"))

      df <- sus_meta(df, system = system, stage = "stand", type = "stand")
      df <- sus_meta(df, add_history = "Standardized column names and types")
     
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
    detect_health_system(stats::setNames(vector("list", length(col_names)), col_names))
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
