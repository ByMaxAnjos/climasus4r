#' Export Processed Health Data with Metadata
#'
#' Exports processed health data to a file with optional metadata documentation
#' to ensure reproducibility. Supports multiple file formats optimized for
#' different use cases, including GeoArrow/GeoParquet for spatial data.
#'
#' @param df A data frame or sf object containing the processed health data to export.
#' @param file_path Character string specifying the output file path. The file
#'   extension determines the format if `format` is not explicitly specified.
#' @param format Character string specifying the output format. Options:
#'   `"rds"` (default, R binary format), `"arrow"` (Apache Arrow/Parquet),
#'   `"geoparquet"` (GeoArrow/GeoParquet for spatial data), `"shapefile"` (ESRI Shapefile), `"GeoPackage"`, 
#'   and `"csv"` (comma-separated values).
#'   If `NULL`, infers from `file_path` extension and data type (auto-detects sf objects).
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
#'   \item **GeoParquet** (`.geoparquet`, `.parquet` for sf objects): Optimized
#'     columnar format for spatial data. Combines benefits of Parquet with
#'     efficient geometry storage. 50-90% smaller than shapefiles, 10-100x faster.
#'   \item **Shapefile** (`.shp` or `.gpkg`): Traditional GIS format. Widely supported but
#'     inefficient for large datasets. Multiple files generated (.shp, .shx, .dbf, etc.).
#'   \item **CSV** (`.csv`): Universal text format. Human-readable, compatible
#'     with all software. Best for sharing with non-R users. Larger file size.
#'     Note: Geometries are exported as WKT (Well-Known Text) for spatial data.
#' }
#'
#' **Automatic Format Detection**:
#' If `format = NULL`, the function automatically detects the best format:
#' \itemize{
#'   \item If input is an `sf` object and extension is `.parquet` → `"geoparquet"`
#'   \item If input is an `sf` object and extension is `.shp` → `"shapefile"`
#'   \item Otherwise, infers from file extension
#' }
#'
#' **Spatial Data Export**:
#' When exporting `sf` objects (spatial data from `sus_join_spatial()`):
#' \itemize{
#'   \item **Recommended**: Use GeoParquet format for optimal performance
#'   \item GeoParquet preserves CRS, geometry types, and all attributes
#'   \item Compatible with QGIS, Python (geopandas), and other GIS software
#'   \item Significantly faster and smaller than shapefiles
#' }
#'
#' @examples
#' \dontrun{
#' library(climasus4r)
#'
#' # Export regular data frame to RDS
#' sus_data_export(df_final, "output/data.rds")
#'
#' # Export spatial data to GeoParquet (RECOMMENDED)
#' sf_result <- sus_join_spatial(df, level = "munic")
#' sus_data_export(
#'   sf_result,
#'   file_path = "output/spatial_data.geoparquet",
#'   format = "geoparquet"  # Auto-detected if extension is .parquet
#' )
#'
#' # Export spatial data to Shapefile (traditional)
#' sus_data_export(
#'   sf_result,
#'   file_path = "output/spatial_data.shp",
#'   format = "shapefile"
#' )
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
#'     author = "Max Anjos"
#'   )
#' )
#' }
#'
#' @export
#' @importFrom glue glue
sus_data_export <- function(df,
                             file_path,
                             format = NULL,
                             include_metadata = TRUE,
                             metadata = NULL,
                             compress = TRUE,
                             compression_level = 6,
                             overwrite = FALSE,
                             lang = "pt",
                             verbose = TRUE) {
  
  # ========================================================================
  # 1. INPUT VALIDATION
  # ========================================================================
  
  if (!is.data.frame(df)) {
    stop("df must be a data frame or sf object")
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
  
  # ========================================================================
  # 2. DETECT IF INPUT IS SPATIAL (sf object)
  # ========================================================================
  
  is_spatial <- inherits(df, "sf")
  
  if (is_spatial && verbose) {
    msg <- switch(lang,
      "en" = "Spatial data (sf object) detected",
      "pt" = "Dados espaciais (objeto sf) detectados",
      "es" = "Datos espaciales (objeto sf) detectados"
    )
    cli::cli_alert_info(msg)
  }
  
  # ========================================================================
  # 3. INFER FORMAT FROM EXTENSION AND DATA TYPE
  # ========================================================================
  
  if (is.null(format)) {
    ext <- tolower(tools::file_ext(file_path))
    
    # Auto-detect format based on extension and data type
    if (is_spatial) {
      format <- switch(ext,
        "shp" = "shapefile",
        "parquet" = "geoparquet",
        "geoparquet" = "geoparquet",
        "gpkg" = "gpkg",
        "geojson" = "geojson",
        "rds" = "rds",
        "csv" = "csv",
        "geoparquet"  # default for spatial data
      )
    } else {
      format <- switch(ext,
        "rds" = "rds",
        "parquet" = "arrow",
        "arrow" = "arrow",
        "csv" = "csv",
        "rds"  # default for non-spatial data
      )
    }
    
    if (verbose) {
      msg <- switch(lang,
        "en" = paste0("Inferred format from extension: ", format),
        "pt" = paste0("Formato inferido da extensao: ", format),
        "es" = paste0("Formato inferido de la extension: ", format)
      )
      cli::cli_alert_info(msg)
    }
  }
  
  # ========================================================================
  # 4. VALIDATE FORMAT
  # ========================================================================
  
  valid_formats <- c("rds", "arrow", "csv", "geoparquet", "shapefile", "gpkg", "geojson")
  
  if (!format %in% valid_formats) {
    cli::cli_abort(paste0("format must be one of: ", paste(valid_formats, collapse = ", ")))
  }
  
  # Check if spatial format is used for non-spatial data
  if (!is_spatial && format %in% c("geoparquet", "shapefile", "gpkg", "geojson")) {
    cli::cli_abort(paste0("Format '", format, "' can only be used with spatial data (sf objects)"))
  }
  
  # Check if non-spatial format is used for spatial data (warn but allow)
  if (is_spatial && format %in% c("arrow", "csv") && verbose) {
    msg <- switch(lang,
      "en" = "Warning: Exporting spatial data to non-spatial format. Geometries will be converted to WKT. Consider using 'geoparquet' format instead.",
      "pt" = "Aviso: Exportando dados espaciais para formato nao-espacial. Geometrias serao convertidas para WKT. Considere usar formato 'geoparquet'.",
      "es" = "Advertencia: Exportando datos espaciales a formato no espacial. Las geometrias se convertiran a WKT. Considere usar formato 'geoparquet'."
    )
    cli::cli_alert_warning(msg)
  }
  
  # ========================================================================
  # 5. CREATE OUTPUT DIRECTORY
  # ========================================================================
  
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
  # 6. EXPORT DATA
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
    # ======================================================================
    # RDS FORMAT (works for both spatial and non-spatial)
    # ======================================================================
    saveRDS(df, file = file_path, compress = compress, 
            version = 3, ascii = FALSE)
    
  } else if (format == "arrow") {
    # ======================================================================
    # ARROW/PARQUET FORMAT (non-spatial)
    # ======================================================================
    
    # Convert sf to regular data.frame if needed (geometries as WKT)
    if (is_spatial) {
      df_export <- df
      df_export$geometry <- sf::st_as_text(df$geometry)
      df_export <- as.data.frame(df_export)
      class(df_export) <- "data.frame"
    } else {
      df_export <- df
    }
    
    arrow::write_parquet(df_export, sink = file_path, 
                         compression = ifelse(compress, "snappy", "uncompressed"))
    
  } else if (format == "geoparquet") {
    # ======================================================================
    # GEOPARQUET FORMAT (spatial only) ⭐ RECOMMENDED FOR SPATIAL DATA
    # ======================================================================
    if (!is_spatial) {
      cli::cli_abort("GeoParquet format requires spatial data (sf object)")
    }
    sfarrow::st_write_parquet(obj = df, dsn = file_path)
  } else if (format == "shapefile") {
    # ======================================================================
    # SHAPEFILE FORMAT (spatial only, traditional)
    # ======================================================================
    if (!is_spatial) {
      cli::cli_abort("Shapefile format requires spatial data (sf object)")
    }
    
    sf::st_write(df, dsn = file_path, driver = "ESRI Shapefile", 
                 delete_dsn = overwrite, quiet = !verbose)
    
  } else if (format == "gpkg") {
    # ======================================================================
    # GEOPACKAGE FORMAT (spatial only)
    # ======================================================================
    if (!is_spatial) {
      cli::cli_abort("GeoPackage format requires spatial data (sf object)")
    }
    
    sf::st_write(df, dsn = file_path, driver = "GPKG", 
                 delete_dsn = overwrite, quiet = !verbose)
    
  } else if (format == "geojson") {
    # ======================================================================
    # GEOJSON FORMAT (spatial only)
    # ======================================================================
    if (!is_spatial) {
      cli::cli_abort("GeoJSON format requires spatial data (sf object)")
    }
    
    sf::st_write(df, dsn = file_path, driver = "GeoJSON", 
                 delete_dsn = overwrite, quiet = !verbose)
    
  } else if (format == "csv") {
    # ======================================================================
    # CSV FORMAT (works for both, geometries as WKT for spatial)
    # ======================================================================
    if (is_spatial) {
      df_export <- df
      df_export$geometry <- sf::st_as_text(df$geometry)
      df_export <- as.data.frame(df_export)
      class(df_export) <- "data.frame"
    } else {
      df_export <- df
    }
    
    data.table::fwrite(df_export, file = file_path, sep = ",", 
                       na = "", quote = "auto")
  }
  
  end_time <- Sys.time()
  export_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # ========================================================================
  # 7. GET FILE SIZE
  # ========================================================================
  
  # For shapefiles, calculate total size of all associated files
  if (format == "shapefile") {
    base_name <- tools::file_path_sans_ext(file_path)
    shp_files <- list.files(dirname(file_path), 
                            pattern = paste0("^", basename(base_name), "\\."),
                            full.names = TRUE)
    file_size_bytes <- sum(file.info(shp_files)$size, na.rm = TRUE)
  } else {
    file_size_bytes <- file.info(file_path)$size
  }
  
  file_size_mb <- round(file_size_bytes / 1024^2, 2)
  
  # ========================================================================
  # 8. SAVE METADATA
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
    metadata$is_spatial <- is_spatial
    
    # Add spatial-specific metadata
    if (is_spatial) {
      metadata$geometry_type <- as.character(unique(sf::st_geometry_type(df)))
      metadata$crs <- as.character(sf::st_crs(df)$input)
      metadata$bbox <- as.character(sf::st_bbox(df))
    }
    
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
      paste0("Spatial Data: ", metadata$is_spatial),
      ""
    )
    
    # Add spatial info
    if (is_spatial) {
      meta_lines <- c(meta_lines,
                     "--- SPATIAL INFORMATION ---",
                     paste0("Geometry Type: ", paste(metadata$geometry_type, collapse = ", ")),
                     paste0("CRS: ", metadata$crs),
                     paste0("Bounding Box: ", paste(metadata$bbox, collapse = ", ")),
                     "")
    }
    
    meta_lines <- c(meta_lines,
                   "--- FILE INFORMATION ---",
                   paste0("Format: ", metadata$file_format),
                   paste0("File Size: ", metadata$file_size_mb, " MB"),
                   paste0("Compressed: ", metadata$compressed),
                   paste0("File Path: ", file_path),
                   "")
    
    # Add custom metadata fields
    custom_fields <- setdiff(names(metadata), 
                             c("export_date", "package_version", "n_rows", 
                               "n_cols", "column_names", "file_format", 
                               "file_size_mb", "compressed", "is_spatial",
                               "geometry_type", "crs", "bbox"))
    
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
  # 9. SUMMARY MESSAGE
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
    
    # Recommendation for spatial data
    if (is_spatial && format != "geoparquet" && verbose) {
      rec_msg <- switch(lang,
        "en" = "Tip: For optimal performance with spatial data, consider using 'geoparquet' format (50-90% smaller, 10-100x faster than shapefiles)",
        "pt" = "Dica: Para melhor desempenho com dados espaciais, considere usar formato 'geoparquet' (50-90% menor, 10-100x mais rapido que shapefiles)",
        "es" = "Consejo: Para un rendimiento optimo con datos espaciales, considere usar formato 'geoparquet' (50-90% mas pequeno, 10-100x mas rapido que shapefiles)"
      )
      cli::cli_alert_info(rec_msg)
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
