#' Export Processed Health Data with Metadata
#'
#' Exports processed health data to a file with optional metadata documentation
#' to ensure reproducibility. Supports multiple file formats optimized for
#' different use cases (RDS for R, Arrow/Parquet for interoperability, CSV for
#' universal access).
#'
#' @param df A data frame containing the processed health data to export.
#' @param file_path Character string specifying the output file path. The file
#'   extension determines the format if `format` is not explicitly specified.
#' @param format Character string specifying the output format. Options:
#'   `"rds"` (default, R binary format), `"arrow"` (Apache Arrow/Parquet), and
#'   `"csv"` (comma-separated values).
#'   If `NULL`, infers from `file_path` extension.
#' @param include_metadata Logical. If `TRUE` (default), saves a companion
#'   metadata file (`.txt` or `.json`) with processing information.
#' @param metadata Named list containing custom metadata to save. Common fields:
#'   \itemize{
#'     \item `source_system`: Health system (e.g., "SIM-DO")
#'     \item `states`: Vector of state codes
#'     \item `years`: Vector of years
#'     \item `filters_applied`: Description of filters
#'     \item `disease_groups`: Disease groups included
#'     \item `processing_date`: Date of processing
#'     \item `package_version`: climasus4r version
#'     \item `author`: Analyst name
#'     \item `notes`: Additional notes
#'   }
#'   If `NULL`, generates basic metadata automatically.
#' @param compress Logical. If `TRUE` (default for RDS and Arrow), compresses
#'   the output file. Compression level can be specified for some formats.
#' @param compression_level Integer specifying compression level (1-9). Higher
#'   values = smaller files but slower. Default is 6. Only applies to formats
#'   that support compression.
#' @param overwrite Logical. If `TRUE`, overwrites existing files. If `FALSE`
#'   (default), stops with an error if file exists.
#' @param lang Character string specifying the language for messages. Options:
#'   `"en"` (English, default), `"pt"` (Portuguese), `"es"` (Spanish).
#' @param verbose Logical. If `TRUE` (default), prints export summary.
#'
#' @return Invisibly returns the file path of the exported data. If metadata
#'   was saved, also returns the metadata file path as an attribute.
#'
#' @details
#' **File Formats**:
#' \itemize{
#'   \item **RDS** (`.rds`): Native R format. Fast, compressed, preserves all
#'     R object attributes. Best for R-only workflows.
#'   \item **Arrow/Parquet** (`.parquet`, `.arrow`): Columnar format. Excellent
#'     compression, fast reading, language-agnostic. Best for large datasets
#'     and interoperability with Python, Spark, etc.
#'   \item **CSV** (`.csv`): Universal text format. Human-readable, compatible
#'     with all software. Best for sharing with non-R users. Larger file size.
#' }
#'
#' **Metadata**: The metadata file contains information about data provenance,
#' processing steps, and analysis parameters. This ensures reproducibility and
#' helps future users understand the data.
#'
#' @examples
#' \dontrun{
#' library(climasus4r)
#'
#' # Basic export to RDS
#' sus_data_export(df_final, "output/data.rds")
#'
#' # Export to Arrow with custom metadata
#' sus_data_export(
#'   df_final,
#'   file_path = "output/respiratory_sp_2023.parquet",
#'   format = "arrow",
#'   metadata = list(
#'     source_system = "SIM-DO",
#'     states = "SP",
#'     years = 2023,
#'     disease_groups = "respiratory",
#'     filters_applied = "age >= 65, sex = Female",
#'     author = "Max Anjos",
#'     notes = "Elderly women respiratory deaths for DLNM analysis"
#'   )
#' )
#'
#' # Export to CSV without metadata
#' sus_data_export(
#'   df_final,
#'   file_path = "output/data.csv",
#'   include_metadata = FALSE
#' )
#'
#' # Export with maximum compression
#' sus_data_export(
#'   df_large,
#'   file_path = "output/large_dataset.rds",
#'   compress = TRUE,
#'   compression_level = 9
#' )
#' }
#'
#' @export
sus_data_export <- function(df,
                             file_path,
                             format = NULL,
                             include_metadata = TRUE,
                             metadata = NULL,
                             compress = TRUE,
                             compression_level = 6,
                             overwrite = FALSE,
                             lang = "en",
                             verbose = TRUE) {
  
  # Validate inputs
  if (!is.data.frame(df)) {
    stop("df must be a data frame")
  }
  
  if (missing(file_path) || is.null(file_path)) {
    stop("file_path must be specified")
  }
  
  if (!lang %in% c("en", "pt", "es")) {
    stop("lang must be one of: 'en', 'pt', 'es'")
  }
  
  # Check if file exists
  if (file.exists(file_path) && !overwrite) {
    stop(paste0("File already exists: ", file_path, ". Set overwrite = TRUE to replace."))
  }
  
  # Infer format from extension if not specified
  if (is.null(format)) {
    ext <- tools::file_ext(file_path)
    format <- switch(tolower(ext),
      "rds" = "rds",
      "parquet" = "arrow",
      "arrow" = "arrow",
      "csv" = "csv",
      "rds"  # default
    )
    
    if (verbose) {
      msg <- switch(lang,
        "en" = paste0("Inferred format from extension: ", format),
        "pt" = paste0("Formato inferido da extensao: ", format),
        "es" = paste0("Formato inferido de la extension: ", format)
      )
      cli::cli_alert_info(msg)
    }
  }
  
  # Validate format
  if (!format %in% c("rds", "arrow", "csv")) {
    cli::cli_abort("format must be one of: 'rds', 'arrow', 'csv'")
  }
  
  # Create output directory if it doesn't exist
  output_dir <- dirname(file_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    if (verbose) {
      msg <- switch(lang,
        "en" = paste0("Created output directory: ", output_dir),
        "pt" = paste0("Diretorio de saida criado: ", output_dir),
        "es" = paste0("Directorio de salida creado: ", output_dir)
      )
      cli::cli_alert_info(msg)
    }
  }
  
  # ========================================================================
  # EXPORT DATA
  # ========================================================================
  
  if (verbose) {
    msg <- switch(lang,
      "en" = paste0("Exporting data to ", format, " format..."),
      "pt" = paste0("Exportando dados para formato ", format, "..."),
      "es" = paste0("Exportando datos a formato ", format, "...")
    )
    cli::cli_alert_info(msg)
  }
  
  start_time <- Sys.time()
  
  # Export based on format
  if (format == "rds") {
    saveRDS(df, file = file_path, compress = compress, 
            version = 3, ascii = FALSE)
    
  } else if (format == "arrow") {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required for Arrow/Parquet export. Install with: install.packages('arrow')")
    }
    arrow::write_parquet(df, sink = file_path, 
                         compression = ifelse(compress, "snappy", "uncompressed"))
    
  } else if (format == "csv") {
    data.table::fwrite(df, file = file_path, sep = ",", 
                       na = "", quote = "auto")
    
  }
  
  end_time <- Sys.time()
  export_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # Get file size
  file_size_bytes <- file.info(file_path)$size
  file_size_mb <- round(file_size_bytes / 1024^2, 2)
  
  # ========================================================================
  # SAVE METADATA
  # ========================================================================
  
  metadata_file <- NULL
  
  if (include_metadata) {
    # Generate metadata if not provided
    if (is.null(metadata)) {
      metadata <- list()
    }
    
    # Add automatic metadata
    metadata$export_date <- as.character(Sys.time())
    metadata$package_version <- tryCatch(
      as.character(utils::packageVersion("climasus4r")),
      error = function(e) "unknown"
    )
    metadata$n_rows <- nrow(df)
    metadata$n_cols <- ncol(df)
    metadata$column_names <- names(df)
    metadata$file_format <- format
    metadata$file_size_mb <- file_size_mb
    metadata$compressed <- compress
    
    # Save metadata
    metadata_file <- paste0(tools::file_path_sans_ext(file_path), "_metadata.txt")
    
    # Write metadata as formatted text
    meta_lines <- c(
      "=" %s% 70,
      "CLIMASUS4R DATA EXPORT METADATA",
      "=" %s% 70,
      "",
      paste0("Export Date: ", metadata$export_date),
      paste0("Package Version: ", metadata$package_version),
      "",
      "--- DATA INFORMATION ---",
      paste0("Rows: ", format(metadata$n_rows, big.mark = ",")),
      paste0("Columns: ", metadata$n_cols),
      "",
      "--- FILE INFORMATION ---",
      paste0("Format: ", metadata$file_format),
      paste0("File Size: ", metadata$file_size_mb, " MB"),
      paste0("Compressed: ", metadata$compressed),
      paste0("File Path: ", file_path),
      ""
    )
    
    # Add custom metadata fields
    custom_fields <- setdiff(names(metadata), 
                             c("export_date", "package_version", "n_rows", 
                               "n_cols", "column_names", "file_format", 
                               "file_size_mb", "compressed"))
    
    if (length(custom_fields) > 0) {
      meta_lines <- c(meta_lines, "--- CUSTOM METADATA ---")
      for (field in custom_fields) {
        value <- metadata[[field]]
        if (length(value) > 1) {
          value <- paste(value, collapse = ", ")
        }
        meta_lines <- c(meta_lines, paste0(field, ": ", value))
      }
      meta_lines <- c(meta_lines, "")
    }
    
    # Add column names
    meta_lines <- c(meta_lines,
                   "--- COLUMN NAMES ---",
                   paste(metadata$column_names, collapse = ", "),
                   "",
                   "=" %s% 70)
    
    writeLines(meta_lines, metadata_file)
  }
  
  # ========================================================================
  # SUMMARY MESSAGE
  # ========================================================================
  
  if (verbose) {
    success_msg <- switch(lang,
      "en" = paste0("Successfully exported ", format(nrow(df), big.mark = ","),
                   " rows to ", file_path),
      "pt" = paste0("Exportados com sucesso ", format(nrow(df), big.mark = ","),
                   " registros para ", file_path),
      "es" = paste0("Exportados exitosamente ", format(nrow(df), big.mark = ","),
                   " registros a ", file_path)
    )
    cli::cli_alert_success(success_msg)
    
    size_msg <- switch(lang,
      "en" = paste0("File size: ", file_size_mb, " MB"),
      "pt" = paste0("Tamanho do arquivo: ", file_size_mb, " MB"),
      "es" = paste0("Tamano del archivo: ", file_size_mb, " MB")
    )
    cli::cli_alert_info(size_msg)
    
    time_msg <- switch(lang,
      "en" = paste0("Export time: ", round(export_time, 2), " seconds"),
      "pt" = paste0("Tempo de exportacao: ", round(export_time, 2), " segundos"),
      "es" = paste0("Tiempo de exportacion: ", round(export_time, 2), " segundos")
    )
    cli::cli_alert_info(time_msg)
    
    if (!is.null(metadata_file)) {
      meta_msg <- switch(lang,
        "en" = paste0("Metadata saved to: ", metadata_file),
        "pt" = paste0("Metadados salvos em: ", metadata_file),
        "es" = paste0("Metadatos guardados en: ", metadata_file)
      )
      cli::cli_alert_info(meta_msg)
    }
  }
  
  # Return file path invisibly
  result <- file_path
  if (!is.null(metadata_file)) {
    attr(result, "metadata_file") <- metadata_file
  }
  
  invisible(result)
}


# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# String repeat operator
`%s%` <- function(x, n) {
  paste(rep(x, n), collapse = "")
}
