#' Standardize SUS data column names and values
#'
#' This function standardizes column names and categorical values in SUS datasets,
#' ensuring consistency across different years and versions. It supports three languages:
#' English (en), Portuguese (pt), and Spanish (es).
#'
#' The function builds upon the preprocessing done by `microdatasus`, adding an
#' additional layer of standardization specifically designed for climate-health
#' research workflows.
#'
#' @param df A `data.frame` or `tibble` to be standardized (typically output from `sus_data_import()`).
#' @param lang Character. Output language for column names and values. Options: "en" (English),
#'   "pt" (Portuguese, Default), "es" (Spanish).
#' @param translate_columns Logical. If TRUE, translates column names. Default is TRUE.
#' @param standardize_values Logical. If TRUE, standardizes categorical values. Default is TRUE.
#' @param keep_original Logical. If TRUE, keeps original columns alongside standardized ones. Default is FALSE.
#' @param backend Character string specifying the data processing backend.
#'   Use `"arrow"` for out-of-memory, lazy processing (recommended for large datasets),
#'   or `"tibble"` for in-memory processing (recommended for small to medium datasets).
#'
#'   - `"arrow"`: operations are performed lazily using the Apache Arrow engine,
#'     avoiding loading the full dataset into memory. Ideal for large files
#'     (e.g., Parquet, Feather) and high-performance workflows.
#'
#'   - `"tibble"`: data is fully loaded into memory as a tibble and processed eagerly
#'     using dplyr. Simpler and more predictable, but may be slow or fail for large datasets.
#'
#'   If not specified, the function may automatically choose the backend based on
#'   the input data type.
#' @param verbose Logical. If TRUE, prints a report of standardization actions. Default is TRUE.
#'
#' @return A `data.frame` with standardized column names and values in the specified language.
#'
#' @importFrom dplyr rename mutate case_when
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Standardize to English (default)
#' df_en <- sus_data_standardize(df_raw, lang = "en")
#'
#' # Standardize to Portuguese
#' df_pt <- sus_data_standardize(df_raw, lang = "pt")
#'
#' # Standardize to Spanish
#' df_es <- sus_data_standardize(df_raw, lang = "es")
#'
#' # Keep original columns for comparison
#' df_both <- sus_data_standardize(
#'   df_raw,
#'   lang = "pt",
#'   keep_original = TRUE
#' )
#'
#' # Only translate column names (not values)
#' df_cols_only <- sus_data_standardize(
#'   df_raw,
#'   lang = "en",
#'   translate_columns = TRUE,
#'   standardize_values = FALSE
#' )
#'
#' # Complete pipeline
#' df_analysis_ready <- sus_data_import(uf = "SP", year = 2023, system = "SIM-DO") |>
#'   sus_data_clean_encoding() |>
#'   sus_data_standardize(lang = "pt")
#' }
#' @references
#' Brazilian Ministry of Health. DATASUS. \url{http://datasus.saude.gov.br}
#'
#' SALDANHA, Raphael de Freitas; BASTOS, Ronaldo Rocha; BARCELLOS, Christovam. Microdatasus: pacote para download e pre-processamento de microdados do Departamento de Informatica do SUS (DATASUS). Cad. Saude Publica, Rio de Janeiro , v. 35, n. 9, e00032419, 2019. Available from https://doi.org/10.1590/0102-311x00032419.

sus_data_standardize <- function(
  df,
  lang = "pt",
  translate_columns = TRUE,
  standardize_values = TRUE,
  keep_original = FALSE,
  backend = "arrow",
  verbose = TRUE
) {
  if (backend == "arrow") {
    result <- tryCatch(
      {
        .standardize_arrow_internal(
          df = df,
          translate_columns = translate_columns,
          standardize_values = standardize_values,
          keep_original = keep_original,
          lang = lang,
          verbose = verbose
        )
      },
      error = function(e) {
        .warn_arrow_fallback(conditionMessage(e), lang = lang)
        NULL
      }
    )
    if (!is.null(result)) {
      return(result)
    }
  }
  # Fallback: tibble
  .standardize_tibble_internal(
    df = df,
    translate_columns = translate_columns,
    standardize_values = standardize_values,
    keep_original = keep_original,
    lang = lang,
    verbose = verbose
  )
}
# ===========================================================================
# INTERNAL FUNCTIONS
# ===========================================================================
.standardize_tibble_internal <- function(
  df,
  translate_columns,
  standardize_values,
  keep_original,
  lang = lang,
  verbose = verbose
) {
  # Validate inputs
  if (!is.data.frame(df)) {
    df <- new_climasus_df(                                      
      dplyr::collect(df),                                                
      sus_meta(df)  # extrai metadata do Arrow                           
    ) 
    if (!is.data.frame(df)) {
      cli::cli_abort("Input {.arg df} must be a data.frame or a collectable dplyr object.")
    }
  }

  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_abort("Language must be 'en', 'pt', or 'es'")
  }

  if (nrow(df) == 0) {
    if (verbose) {
      cli::cli_alert_warning("Empty data frame - returning as is")
    }
    return(df)
  }

  # Get UI messages
  messages <- get_ui_messages_standardize(lang)

  # Detect health system
  system <- detect_health_system(df)
  #df$system <- paste0(system)

  if (verbose) {
    cli::cli_h1(messages$standardize_title)
    cli::cli_alert_info(paste(
      messages$detected_system,
      get_system_description(system, lang)
    ))
    cli::cli_alert_info(paste(messages$language, toupper(lang)))
  }

  # Get translation dictionaries based on system

  if (system == "SIM") {
    switch(
      lang,
      "en" = translations <- get_translation_dict_en(),
      "pt" = translations <- get_translation_dict_pt(),
      # default case (any other language)
      translations <- get_translation_dict_es()
    )
  }
  if (system == "SINAN") {
    switch(
      lang,
      "en" = translations <- get_translation_dict_en_sinan(),
      "pt" = translations <- get_translation_dict_pt_sinan(),
      # default case (any other language)
      translations <- get_translation_dict_es_sinan()
    )
  }

  if (system == "SIH") {
    switch(
      lang,
      "en" = translations <- get_translation_dict_en_sih(),
      "pt" = translations <- get_translation_dict_pt_sih(),
      # default case (any other language)
      translations <- get_translation_dict_es_sih()
    )
  }

  if (system == "SIA") {
    switch(
      lang,
      "en" = translations <- get_translation_dict_en_sia(),
      "pt" = translations <- get_translation_dict_pt_sia(),
      # default case (any other language)
      translations <- get_translation_dict_es_sia()
    )
  }
  if (system == "CNES") {
    switch(
      lang,
      "en" = translations <- get_translation_dict_en_cnes(),
      "pt" = translations <- get_translation_dict_pt_cnes(),
      # default case (any other language)
      translations <- get_translation_dict_es_cnes()
    )
  }
  if (system == "SINASC") {
    switch(
      lang,
      "en" = translations <- get_translation_dict_en_sinasc(),
      "pt" = translations <- get_translation_dict_pt_sinasc(),
      # default case (any other language)
      translations <- get_translation_dict_es_sinasc()
    )
  }

  # ============================================================================
  # STEP 1: Translate Column Names
  # ============================================================================

  if (translate_columns) {
    if (verbose) {
      cli::cli_alert_info(messages$translating_columns)
    }

    original_names <- names(df)
    new_names <- original_names

    # Translate column names
    for (i in seq_along(original_names)) {
      orig_name <- original_names[i]
      if (orig_name %in% names(translations$columns)) {
        new_names[i] <- translations$columns[[orig_name]]
      }
    }

    # Count translations
    n_translated <- sum(original_names != new_names)

    if (keep_original) {
      # Create new columns with translated names
      for (i in seq_along(original_names)) {
        if (original_names[i] != new_names[i]) {
          df[[new_names[i]]] <- df[[original_names[i]]]
        }
      }
    } else {
      # Rename columns in place
      names(df) <- new_names
    }

    if (verbose) {
      cli::cli_alert_success(paste(
        messages$columns_translated,
        n_translated,
        "of",
        length(original_names)
      ))
    }
  }

  # ============================================================================
  # STEP 2: Translate Categorical Values
  # ============================================================================

  if (standardize_values) {
    if (verbose) {
      cli::cli_alert_info(messages$translating_values)
    }

    current_names <- names(df)
    n_variables_translated <- 0
    n_values_translated <- 0

    # Translate categorical values
    for (var_name in names(translations$values)) {
      # Check if variable exists in data (original or translated name)
      col_name <- NULL

      # Try original name
      if (var_name %in% current_names) {
        col_name <- var_name
      } else if (var_name %in% names(translations$columns)) {
        # Try translated name
        translated_name <- translations$columns[[var_name]]
        if (translated_name %in% current_names) {
          col_name <- translated_name
        }
      }

      if (!is.null(col_name)) {
        # Get value mappings
        value_map <- translations$values[[var_name]]

        # Convert column to character for mapping
        original_values <- as.character(df[[col_name]])
        new_values <- original_values

        # Apply mappings
        for (code in names(value_map)) {
          new_values[original_values == code] <- value_map[[code]]
        }

        # Count translations
        n_changed <- sum(original_values != new_values, na.rm = TRUE)

        if (n_changed > 0) {
          if (keep_original) {
            # Create new column with "_translated" suffix
            new_col_name <- paste0(col_name, "_translated")
            df[[new_col_name]] <- as.factor(new_values)
          } else {
            # Replace original column
            df[[col_name]] <- as.factor(new_values)
          }

          n_variables_translated <- n_variables_translated + 1
          n_values_translated <- n_values_translated + n_changed
        }
      }
    }

    if (verbose) {
      cli::cli_alert_success(paste(
        messages$values_translated,
        n_variables_translated,
        "variables,",
        n_values_translated,
        "values"
      ))
    }
  }
  # ============================================================================
  # STEP 3: Datetime formating
  # ============================================================================
 date_pattern <- switch(lang, "en" = "_date", "pt" = "data_", "es" = "fecha_")

  for (col in names(df)[grepl(date_pattern, names(df), ignore.case = TRUE)]) {
    df <- dplyr::mutate(df, !!col := lubridate::ymd(!!rlang::sym(col)))
  }

  names(df) <- make.unique(names(df), sep = "_")

  # ============================================================================
  # STEP 4: Final Summary
  # ============================================================================

  if (verbose) {
    cli::cli_alert_success(paste(
      messages$standardization_complete,
      nrow(df),
      "rows,",
      ncol(df),
      "columns"
    ))
  }
  if (!inherits(df, "climasus_df")) {
    # Create new climasus_df
    meta <- list(
      system = system,
      stage = "stand",
      type = "stand",
      spatial = inherits(df, "sf"),
      temporal = NULL,
      created = Sys.time(),
      modified = Sys.time(),
      history = sprintf(
        "[%s] Standardized column names and types",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ),
      user = list()
    )

    base_classes <- setdiff(class(df), "climasus_df")
    df <- structure(
      df,
      sus_meta = meta,
      class = c("climasus_df", base_classes)
    )
  } else {
    # Already climasus_df - update metadata
    df <- sus_meta(df, system = system, stage = "stand", type = "stand")
    df <- sus_meta(df, add_history = "Standardized column names and types")
  }
  return(df)
}

.standardize_arrow_internal <- function(
  df,
  translate_columns,
  standardize_values,
  keep_original,
  lang = lang,
  verbose = verbose
) {
  is_arrow_input <- inherits(
    df,
    c(
      "climasus_dataset",
      "arrow_dplyr_query",
      "Dataset",
      "ArrowTabular",
      "Table"
    )
  )

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

  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_abort("Language must be 'en', 'pt', or 'es'")
  }

  messages <- .get_ui_messages_std(lang)
  incoming_meta <- attr(df, "sus_meta")
  schema_names <- names(df) # works on Dataset and arrow_dplyr_query
  system <- .std_detect_system(incoming_meta, schema_names)
  translations <- .std_load_translations(system, lang)

  if (verbose) {
    cli::cli_h1(messages$standardize_title)
    cli::cli_alert_info(paste(
      messages$detected_system,
      get_system_description(system, lang),
      "(Arrow lazy)"
    ))
    cli::cli_alert_info(paste(messages$language, toupper(lang)))
  }

  # 1a. Column renaming — lazy (Arrow C++ engine renames the projection)
  n_translated <- 0L
  if (translate_columns && length(translations$columns) > 0) {
    cols_present <- intersect(names(translations$columns), schema_names)
    if (length(cols_present) > 0) {
      translated_names <- unlist(
        translations$columns[cols_present],
        use.names = FALSE
      )
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
  if (verbose) {
    cli::cli_alert_success(paste(
      messages$columns_translated,
      n_translated,
      "of",
      length(schema_names)
    ))
  }

  # 1b. Value recoding — lazy via dplyr::mutate + case_when (Arrow compute)
  #
  # Arrow is schema-strict: comparing an int32 column to a character literal
  # silently returns NULL. Cast every column to utf8 before comparison so the
  # mapping keys (always character) are type-compatible.
  n_vars_recoded <- 0L
  if (standardize_values && length(translations$values) > 0) {
    if (verbose) {
      cli::cli_alert_info(messages$translating_values)
    }
    current_cols <- names(df) # updated names after rename

    for (var_name in names(translations$values)) {
      col <- .std_resolve_col(var_name, current_cols, translations$columns)
      if (is.null(col)) {
        next
      }

      mapping <- translations$values[[var_name]]
      dest_col <- if (keep_original) paste0(col, "_traduzido") else col

      # Build case_when arms: cast to utf8 for type-safe comparison
      cases <- lapply(names(mapping), function(k) {
        rlang::expr(as.character(!!rlang::sym(col)) == !!k ~ !!mapping[[k]])
      })

      df <- dplyr::mutate(
        df,
        !!dest_col := dplyr::case_when(
          !!!cases,
          TRUE ~ as.character(!!rlang::sym(col))
        )
      )
      n_vars_recoded <- n_vars_recoded + 1L
    }

    if (verbose) {
      cli::cli_alert_success(paste(
        messages$values_translated,
        n_vars_recoded,
        "variables"
      ))
    }
  }

  # 1c. Date column casting - attempt Arrow-native ymd() (YYYYMMDD  Date32)
  #     Wrapped in tryCatch: skipped gracefully if Arrow version or column
  #     type does not support the compute function.
  date_pattern <- switch(lang, "en" = "_date", "pt" = "data_", "es" = "fecha_")
  
  if(system %in% c("SIM", "SIA", "CNES", "SINASC")) {
  cols_to_fix <- names(df)[grepl(
    paste(date_pattern, collapse = "|"),
    names(df),
    ignore.case = TRUE
  )]

  if (length(cols_to_fix) > 0) {
    # If Arrow lazy (dplyr query), collect to eager mode for date parsing
    is_lazy <- inherits(df, c("arrow_dplyr_query", "Dataset"))
    if (is_lazy) {
      df <- dplyr::collect(df)
    }

    # Parse dates in eager mode (works reliably)
    for (col in cols_to_fix) {
      df[[col]] <- tryCatch({
        col_val <- df[[col]]
        if (inherits(col_val, c("Date", "POSIXct", "POSIXlt"))) {
          # microdatasus already parsed these — just normalise to Date
          as.Date(col_val)
        } else {
          # Try YYYYMMDD / ISO first (microdatasus output format),
          # fall back to raw DDMMYYYY if majority are NA
          result <- suppressWarnings(lubridate::ymd(as.character(col_val)))
          na_frac <- sum(is.na(result), na.rm = TRUE) / max(length(result), 1)
          if (na_frac > 0.5) lubridate::dmy(as.character(col_val)) else result
        }
      }, error = function(e) {
        if (verbose) {
          cli::cli_alert_warning(
            "Could not parse {col} as date: {conditionMessage(e)}"
          )
        }
        df[[col]]
      })
    }

    # Convert back to Arrow if was originally lazy
    if (is_lazy) {
      df <- arrow::as_arrow_table(df)
    }
  }

  # Ensure unique column names
  names(df) <- make.unique(names(df), sep = "_")
  }

  for (col in names(df)[grepl(date_pattern, names(df), ignore.case = TRUE)]) {
    df <- tryCatch(
      dplyr::mutate(df, !!col := lubridate::ymd(!!rlang::sym(col))),
      error = function(e) df
    )
  }
  
   if (verbose) {
    cli::cli_alert_success(paste(
      messages$standardization_complete,
      ncol(df),
      "columns (lazy)"
    ))
  }

  df <- sus_meta(df, system = system, stage = "stand", type = "stand")
  df <- sus_meta(df, add_history = "Standardized column names and types")

  return(df)
}


# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

#' Get UI Messages
#'
#' Returns user interface messages in the specified language
#'
#' @param lang Character. Language code: "en", "pt", "es"
#'
#' @return List of translated UI messages
#'
#' @keywords internal
#' @noRd
get_ui_messages_standardize <- function(lang = "en") {
  messages <- list(
    en = list(
      standardize_title = "Standardizing SUS Data",
      detected_system = "Detected system:",
      language = "Language:",
      translating_columns = "Translating column names...",
      columns_translated = "Translated",
      translating_values = "Translating categorical values...",
      values_translated = "Translated",
      standardization_complete = "Standardization complete:",
      no_translations = "No translations available for this system"
    ),
    pt = list(
      standardize_title = "Padronizando Dados SUS",
      detected_system = "Sistema detectado:",
      language = "Idioma:",
      translating_columns = "Traduzindo nomes de colunas...",
      columns_translated = "Traduzidas",
      translating_values = "Traduzindo valores categoricos...",
      values_translated = "Traduzidas",
      standardization_complete = "Padronizacao completa:",
      no_translations = "Nenhuma traducao disponivel para este sistema"
    ),
    es = list(
      standardize_title = "Estandarizando Datos SUS",
      detected_system = "Sistema detectado:",
      language = "Idioma:",
      translating_columns = "Traduciendo nombres de columnas...",
      columns_translated = "Traducidas",
      translating_values = "Traduciendo valores categoricos...",
      values_translated = "Traducidas",
      standardization_complete = "Estandarizacion completa:",
      no_translations = "No hay traducciones disponibles para este sistema"
    )
  )

  return(messages[[lang]])
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
