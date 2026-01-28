#' Detect and correct character encoding issues
#'
#' This function scans text columns in a data.frame and corrects common encoding
#' problems (e.g., "Sao Paulo") that occur when Latin1 data is
#' incorrectly read as UTF-8. It acts as a final auditor to ensure all text data
#' is properly encoded, complementing the preprocessing done by `microdatasus`.
#' Supports multilingual output messages (English, Portuguese, Spanish).
#'
#' @param df A `data.frame` or `tibble` to be cleaned.
#' @param lang Character. Language for UI messages. Options: "en" (English), 
#'   "pt" (Portuguese, default), "es" (Spanish).
#' @param verbose Logical. If TRUE, prints a report of columns checked and corrected. Default is TRUE.
#'
#' @return A `data.frame` with corrected text columns.
#'
#' @importFrom dplyr select_if
#' @importFrom stringi stri_enc_isutf8 stri_conv
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a sample dataset with encoding issues
#' # In real data, this might happen with Brazilian Portuguese text
#' df_problem <- data.frame(
#'   id = 1:3,
#'   city = c("Sao Paulo", "Rio de Janeiro", "Belo Horizonte"),
#'   state = c("SP", "RJ", "MG"),
#'   stringsAsFactors = FALSE
#' )
#' 
#' # Simulate encoding issue (for demonstration only)
#' # In practice, this happens when Latin1 text is read as UTF-8
#' 
#' # Correct encoding with English messages
#' df_clean_en <- sus_data_clean_encoding(df_problem, lang = "en")
#'
#' # Correct encoding with Portuguese messages
#' df_clean_pt <- sus_data_clean_encoding(df_problem, lang = "pt")
#'
#' # Correct encoding with Spanish messages
#' df_clean_es <- sus_data_clean_encoding(df_problem, lang = "es")
#'
#' # Use in a pipeline
#' df_clean <- sus_data_import(uf = "RJ", year = 2022, system = "SIM") %>%
#'   sus_data_clean_encoding(lang = "pt")
#' }
sus_data_clean_encoding <- function(df, lang = "pt", verbose = TRUE) {
  
  # Input validation
  if (!is.data.frame(df)) {
    cli::cli_alert_danger("Input 'df' must be a data.frame.")
    cli::cli_abort("Invalid input type.")
  }
  
  # Validate language and get UI messages
  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_alert_warning("Language '{lang}' not supported. Using English (en).")
    lang <- "en"
  }
  
  ui_msg <- get_ui_messages(lang)
  
  if (verbose) {
    cli::cli_h1(ui_msg$encoding_header)
  }
  
  # 1. Identify text columns (character type)
  text_cols <- names(dplyr::select_if(df, is.character))
  
  if (length(text_cols) == 0) {
    if (verbose) {
      no_text_msg <- list(
        en = "No text columns found to check.",
        pt = "Nenhuma coluna de texto encontrada para verificar.",
        es = "No se encontraron columnas de texto para verificar."
      )
      cli::cli_alert_info(no_text_msg[[lang]])
    }
    return(df)
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
  
  corrected_cols <- c()
  
  # 2. Iterate over each text column
  for (col in text_cols) {
    # Get a non-NA subset for testing
    test_vector <- stats::na.omit(df[[col]])
    if (length(test_vector) == 0) next # Skip if column only has NAs
    
    # 3. Detection Heuristic:
    # stri_enc_isutf8 returns FALSE for invalid UTF-8 byte sequences.
    # If ANY string in the column is invalid, we consider the column "sick".
    needs_correction <- !all(stringi::stri_enc_isutf8(test_vector))
    
    # 4. Apply Correction
    if (needs_correction) {
      # Record the column that will be corrected
      corrected_cols <- c(corrected_cols, col)
      
      # The magic happens here:
      # stri_conv converts from latin1 to utf8, repairing the "broken" characters
      df[[col]] <- stringi::stri_conv(df[[col]], "latin1", "utf8")
    }
  }
  
  # 5. Generate Report
  if (verbose) {
    if (length(corrected_cols) > 0) {
      corrected_msg <- list(
        en = "Corrected {length(corrected_cols)} column{?s}",
        pt = "Corrigida{?s} {length(corrected_cols)} coluna{?s}",
        es = "Corregida{?s} {length(corrected_cols)} columna{?s}"
      )
      cli::cli_alert_success(corrected_msg[[lang]])
      cli::cli_alert_info("{ui_msg$affected_columns}: {paste(corrected_cols, collapse = ', ')}")
    } else {
      cli::cli_alert_success(ui_msg$no_correction_needed)
    }
  }

  if (!inherits(df, "climasus_df")) {
    # Create new climasus_df
    meta <- list(
      system = NULL,
      stage = "clean",
      type = "clean",
      spatial = inherits(df, "sf"),
      temporal = NULL,
      created = Sys.time(),
      modified = Sys.time(),
      history = sprintf(
        "[%s] Cleaned character encoding (UTF-8)",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      ),
      user = list()
    )

    base_classes <- setdiff(class(df), "climasus_df")
    df <- structure(
      df,
      climasus_meta = meta,
      class = c("climasus_df", base_classes)
    )
  } else {
    # Already climasus_df - update metadata
    df <- climasus_meta(df, stage = "clean", type = "clean")
    df <- climasus_meta(df, add_history = "Cleaned character encoding (UTF-8)")
  }

  
  return(df)
}
