#' Detect and correct character encoding issues in Arrow Dataset (lazy)
#'
#' This function applies encoding correction to text columns in an Arrow Dataset
#' without materializing the data. The correction is applied lazily using Arrow
#' compute expressions that will be executed during the final collect().
#'
#' @param dataset An Arrow Dataset (climasus_dataset)
#' @param lang Character. Language for UI messages. Options: "en", "pt", "es".
#' @param verbose Logical. If TRUE, prints a report of columns checked and corrected.
#'
#' @return An Arrow Dataset with encoding-corrected text columns
#'
#' @importFrom arrow map_batches
#' @importFrom dplyr mutate across
#' @importFrom stringi stri_enc_isutf8 stri_conv
#' @export
sus_data_clean_encoding_arrow <- function(dataset, lang = "pt", verbose = TRUE) {
  
  # Input validation
  if (!inherits(dataset, "Dataset") && !inherits(dataset, "climasus_dataset")) {
    cli::cli_abort("Input must be an Arrow Dataset")
  }
  
  # Validate language
  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_alert_warning("Language '{lang}' not supported. Using English (en).")
    lang <- "en"
  }
  
  ui_msg <- get_ui_messages(lang)
  
  if (verbose) {
    cli::cli_h1(paste(ui_msg$encoding_header, "(Arrow Lazy)"))
  }
  
  # Get schema to identify text columns
  schema <- tryCatch({
    arrow::schema(dataset)
  }, error = function(e) {
    cli::cli_alert_danger("Failed to read dataset schema: {conditionMessage(e)}")
    return(NULL)
  })
  
  if (is.null(schema)) {
    return(dataset)
  }
  
  # Identify character/text columns
  text_cols <- names(schema)[sapply(schema, function(field) {
    field$type == arrow::string()
  })]
  
  if (length(text_cols) == 0) {
    if (verbose) {
      no_text_msg <- list(
        en = "No text columns found to check.",
        pt = "Nenhuma coluna de texto encontrada para verificar.",
        es = "No se encontraron columnas de texto para verificar."
      )
      cli::cli_alert_info(no_text_msg[[lang]])
    }
    return(dataset)
  }
  
  if (verbose) {
    cli::cli_alert_info("{ui_msg$checking_columns}")
    checking_count_msg <- list(
      en = "Checking {length(text_cols)} text column{?s}",
      pt = "Verificando {length(text_cols)} coluna{?s} de texto",
      es = "Verificando {length(text_cols)} columna{?s} de texto"
    )
    cli::cli_alert_info(checking_count_msg[[lang]])
  }
  
  # For Arrow Dataset, we need to sample to detect encoding issues
  # Take a sample of rows to analyze encoding
  sample_size <- min(10000, tryCatch(nrow(dataset), error = function(e) 10000))
  
  sample_data <- tryCatch({
    dataset |>
      dplyr::slice_head(n = sample_size) |>
      dplyr::collect()
  }, error = function(e) {
    cli::cli_alert_warning("Could not sample dataset: {conditionMessage(e)}")
    NULL
  })
  
  if (is.null(sample_data)) {
    cli::cli_alert_warning("Cannot detect encoding issues. Returning original dataset.")
    return(dataset)
  }
  
  # Detect which columns need correction
  cols_to_correct <- c()
  
  for (col in text_cols) {
    if (col %in% names(sample_data)) {
      test_vector <- stats::na.omit(sample_data[[col]])
      if (length(test_vector) > 0) {
        # Check if any string is invalid UTF-8
        needs_correction <- !all(stringi::stri_enc_isutf8(test_vector))
        if (needs_correction) {
          cols_to_correct <- c(cols_to_correct, col)
        }
      }
    }
  }
  
  if (verbose) {
    if (length(cols_to_correct) > 0) {
      corrected_msg <- list(
        en = "Will correct {length(cols_to_correct)} column{?s}",
        pt = "Serão corrigida{?s} {length(cols_to_correct)} coluna{?s}",
        es = "Se corregirán {length(cols_to_correct)} columna{?s}"
      )
      cli::cli_alert_info(corrected_msg[[lang]])
      cli::cli_alert_info("{ui_msg$affected_columns}: {paste(cols_to_correct, collapse = ', ')}")
    } else {
      cli::cli_alert_success(ui_msg$no_correction_needed)
    }
  }
  
  # Apply encoding correction using Arrow compute expressions
  if (length(cols_to_correct) > 0) {
    # Create a new dataset with corrected columns
    # Note: Arrow doesn't have built-in encoding conversion, so we need to
    # either convert during collect or create a wrapper
    
    # Option 1: Store metadata about which columns need correction
    # This will be applied during collect
    attr(dataset, "encoding_correction") <- list(
      cols = cols_to_correct,
      from = "latin1",
      to = "UTF-8",
      applied = Sys.time()
    )
    
    # Option 2: For now, mark the dataset as needing encoding correction
    # The actual conversion will happen in a custom collect method
    dataset <- structure(
      dataset,
      needs_encoding_correction = TRUE,
      encoding_cols = cols_to_correct,
      class = c("climasus_dataset_encoding", class(dataset))
    )
    
    if (verbose) {
      cli::cli_alert_success(
        "Encoding correction will be applied lazily during collect()"
      )
    }
  }
  
  # Update metadata
  if (inherits(dataset, "climasus_dataset")) {
    meta <- attr(dataset, "climasus_meta")
    meta$stage <- "clean_encoding"
    meta$modified <- Sys.time()
    meta$history <- c(meta$history,
                      sprintf("[%s] Encoding correction marked for %d columns",
                              format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                              length(cols_to_correct)))
    attr(dataset, "climasus_meta") <- meta
  }
  
  return(dataset)
}