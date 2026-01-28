#' Read Processed Health Data with Batch and Parallel Support
#'
#' Intelligently reads one or multiple health data files exported by `sus_data_export()`.
#' Supports automatic format detection, batch processing, parallel execution, spatial data,
#' metadata loading, and data validation.
#'
#' @param path Character vector of file paths, or a single directory path.
#'   If a directory is provided, all matching files will be read.
#' @param format Character string specifying the input format. Options: `"dbf"`,
#'   `"rds"`, `"parquet"`, `"geoparquet"`, `"shapefile"`, `"gpkg"`, `"geojson"`, `"csv"`.
#'   If `NULL` (default), automatically detects format from file extension.
#' @param read_metadata Logical. If `TRUE` (default), loads companion metadata files
#'   and attaches them as attributes.
#' @param parallel Logical. If `TRUE`, uses parallel processing for multiple files.
#'   Requires `future` and `future.apply` packages. Default: `FALSE`.
#' @param workers Integer. Number of parallel workers when `parallel = TRUE`. Default: 4.
#' @param lang Character string specifying the language for messages. Options:
#'   `"en"` (English), `"pt"` (Portuguese, default), `"es"` (Spanish).
#' @param verbose Logical. If `TRUE` (default), prints progress and summary.
#'
#' @return A data frame or sf object (for spatial data) containing the loaded data.
#'   For batch reads, all files are combined with `dplyr::bind_rows()`.
#'   Metadata is attached as attributes:
#'   \itemize{
#'     \item Single file: `attr(df, "metadata")`
#'     \item Batch: `attr(df, "batch_metadata")` (list of metadata from each file)
#'     \item Batch: `attr(df, "n_files_combined")` (number of files)
#'   }
#'
#' @details
#' **Batch Processing**:
#' Pass a vector of file paths or a directory path to read multiple files at once.
#' All files are automatically combined into a single object.
#'
#' **Parallel Processing**:
#' When `parallel = TRUE`, files are read simultaneously using `future.apply`.
#' This significantly speeds up batch reads of large files.
#'
#' **Format Detection**:
#' Automatically detects format from file extension. For `.parquet` files,
#' automatically determines if it's GeoParquet (spatial) or regular Parquet.
#'
#' **Memory Efficiency**:
#' For very large datasets (>50 GB), consider using chunked processing or
#' reading files individually instead of batch mode.
#'
#' @examples
#' \dontrun{
#' library(climasus4r)
#'
#' # Single file
#' df <- sus_data_read("output/data.parquet")
#'
#' # Multiple files (vector)
#' df <- sus_data_read(c("output/2020.parquet", "output/2021.parquet"))
#'
#' # Directory (all Parquet files)
#' df <- sus_data_read("output/", format = "parquet")
#'
#' # Parallel batch read
#' df <- sus_data_read("output/", format = "dbf", 
#'                     parallel = TRUE, workers = 6)
#'
#' # Access batch metadata
#' batch_meta <- attr(df, "batch_metadata")
#' n_files <- attr(df, "n_files_combined")
#' }
#'
#' @export
#' @importFrom glue glue
sus_data_read <- function(path,
                          format = NULL,
                          parallel = FALSE,
                          workers = 4,
                          read_metadata = FALSE,
                          lang = "pt",
                          verbose = TRUE) {
  
  # ==========================================================================
  # 1. INPUT VALIDATION
  # ==========================================================================
  
  if (missing(path) || is.null(path) || length(path) == 0) {
    cli::cli_abort(c(
      "x" = "{.arg path} must be specified",
      "i" = "Provide a valid file path or directory",
      ">" = "Example: {.code path = 'data/input.csv'}",
      ">" = "Example: {.code path = c('file1.csv', 'file2.csv')}"
    ))
  }
  
  if (!lang %in% c("en", "pt", "es")) {
    stop("lang must be one of: 'en', 'pt', 'es'")
  }
  
  if (parallel && workers > parallel::detectCores()) {
    cli::cli_alert_warning(
      "{workers} workers requested but only {parallel::detectCores()} cores available"
    )
  }
  
  # ==========================================================================
  # 2. IDENTIFY ALL FILES (BATCH LOGIC)
  # ==========================================================================
  
  all_files <- c()
  
  for (p in path) {
    if (dir.exists(p)) {
      # Directory: list all matching files
      files_in_dir <- list.files(
        path = p,
        pattern = paste0("\\.", format, "$"),
        full.names = TRUE,
        recursive = TRUE
      )
      all_files <- c(all_files, files_in_dir)
      
    } else if (file.exists(p)) {
      # Single file
      all_files <- c(all_files, p)
      
    } else {
      # File/directory not found
      msg <- switch(lang,
        "en" = paste0("Path not found: ", p),
        "pt" = paste0("Caminho nao encontrado: ", p),
        "es" = paste0("Ruta no encontrada: ", p)
      )
      if (verbose) cli::cli_alert_warning(msg)
    }
  }
  
  # Remove duplicates and metadata files
  all_files <- unique(all_files)
  all_files <- all_files[!grepl("_metadata\\.txt$", all_files)]
  
  if (length(all_files) == 0) {
    msg <- switch(lang,
      "en" = "No valid files found to read",
      "pt" = "Nenhum arquivo valido encontrado para leitura",
      "es" = "No se encontraron archivos validos para leer"
    )
    cli::cli_abort(msg)
  }
  
  # ==========================================================================
  # 3. DISPLAY BATCH INFO
  # ==========================================================================
  
  is_batch <- length(all_files) > 1
  
  if (verbose) {
    title_msg <- switch(lang,
      "en" = "climasus4r: Data Reader",
      "pt" = "climasus4r: Leitor de Dados",
      "es" = "climasus4r: Lector de Datos"
    )
    cli::cli_h1(title_msg)
    
    files_msg <- switch(lang,
      "en" = paste0("Found ", length(all_files), " file(s) to process"),
      "pt" = paste0("Encontrados ", length(all_files), " arquivo(s) para processar"),
      "es" = paste0("Encontrados ", length(all_files), " archivo(s) para procesar")
    )
    cli::cli_alert_info(files_msg)
    
    if (is_batch && parallel) {
      parallel_msg <- switch(lang,
        "en" = paste0("Parallel processing enabled with ", workers, " workers"),
        "pt" = paste0("Processamento paralelo ativado com ", workers, " workers"),
        "es" = paste0("Procesamiento paralelo activado con ", workers, " workers")
      )
      cli::cli_alert_info(parallel_msg)
    }
  }
  
  # ==========================================================================
  # 4. INTERNAL SINGLE-FILE READER
  # ==========================================================================
  
  read_single_file <- function(file_path) {
    
    # ------------------------------------------------------------------------
    # 4.1. Detect format from extension
    # ------------------------------------------------------------------------
    
    detected_format <- format
    
    if (is.null(detected_format)) {
      ext <- tolower(tools::file_ext(file_path))
      
      detected_format <- switch(ext,
        "rds" = "rds",
        "rsd" = "rds",
        "drs" = "rds",
        "parquet" = "parquet",
        "bdf" = "dbf",
        "dbf" = "dbf",
        "shp" = "shapefile",
        "gpkg" = "gpkg",
        "pgkg" = "gpkg",
        "gpgk" = "gpkg",
        "geojson" = "geojson",
        "geojsons" = "geojson",
        "json" = "geojson",
        "csv" = "csv",
        "tsv" = "csv",
        "txt" = "csv",
        "unknown"
      )
      
      if (detected_format == "unknown") {
        cli::cli_alert_warning(paste0("Unsupported file extension: .", ext, " - Skipping: ", basename(file_path)))
        return(NULL)
      }
    }
    
    # ------------------------------------------------------------------------
    # 4.2. Refine Parquet format (arrow vs geoparquet)
    # ------------------------------------------------------------------------
    
    if (detected_format == "parquet") {
      is_geoparquet <- FALSE
      
      tryCatch({
        if (requireNamespace("arrow", quietly = TRUE)) {
          parquet_file <- arrow::ParquetFileReader$create(file_path)
          schema <- parquet_file$GetSchema()
          
          field_names <- names(schema)
          has_geometry <- "geometry" %in% field_names || "geom" %in% field_names
          
          metadata_keys <- names(schema$metadata)
          has_geo_metadata <- any(grepl("geo", metadata_keys, ignore.case = TRUE))
          
          is_geoparquet <- has_geometry || has_geo_metadata
        }
      }, error = function(e) {
        is_geoparquet <<- FALSE
      })
      
      detected_format <- ifelse(is_geoparquet, "geoparquet", "parquet")
    }
    
    # ------------------------------------------------------------------------
    # 4.3. Read data based on format
    # ------------------------------------------------------------------------
    
    start_time <- Sys.time()
    
    df <- tryCatch({
      
      if (detected_format == "rds") {
        readRDS(file_path) 
        
      } else if (detected_format == "parquet") {

        arrow::read_parquet(file_path, as_data_frame = TRUE)
        
      } else if (detected_format == "geoparquet") {
        # Try sfarrow first, then sf
        result <- NULL
        
        if (requireNamespace("sfarrow", quietly = TRUE)) {
          result <- tryCatch(
            sfarrow::st_read_parquet(dsn = file_path),
            error = function(e) NULL
          )
        }
        
        if (is.null(result) && requireNamespace("sf", quietly = TRUE)) {
          result <- tryCatch(
            sf::st_read(file_path, quiet = TRUE),
            error = function(e) NULL
          )
        } 
        
        if (is.null(result)) {
          stop("Failed to read GeoParquet. Install 'sfarrow' or 'sf'")
        }
        
        result
        
      } else if (detected_format =="dbf") {

        dplyr::as_tibble(sf::st_read(file_path, quiet = TRUE))
        
      } else if (detected_format %in% c("shapefile", "gpkg", "geojson")) {
        
        sf::st_read(file_path, quiet = TRUE)
        
      } else if (detected_format == "csv") {
        if (requireNamespace("data.table", quietly = TRUE)) {
          data.table::fread(file_path, data.table = FALSE)
        } else {
          utils::read.csv(file_path, stringsAsFactors = FALSE)
        }
        
      } else {
        stop(paste0("Unsupported format: ", detected_format))
      }
      
    }, error = function(e) {
      cli::cli_alert_warning(paste0("Error reading ", basename(file_path), ": ", e$message))
      return(NULL)
    })
    
    if (is.null(df)) return(NULL)
    
    end_time <- Sys.time()
    read_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
    
    # ------------------------------------------------------------------------
    # 4.4. Read metadata (if requested)
    # ------------------------------------------------------------------------
    
    metadata <- NULL
    
    if (read_metadata) {
      metadata_file <- paste0(tools::file_path_sans_ext(file_path), "_metadata.txt")
      
      if (file.exists(metadata_file)) {
        metadata <- tryCatch({
          meta_lines <- readLines(metadata_file, warn = FALSE)
          
          meta_list <- list()
          for (line in meta_lines) {
            if (grepl("^=+$", line) || nchar(trimws(line)) == 0 || grepl("^---", line)) {
              next
            }
            
            if (grepl(":", line)) {
              parts <- strsplit(line, ":", fixed = TRUE)[[1]]
              if (length(parts) >= 2) {
                key <- trimws(parts[1])
                value <- trimws(paste(parts[-1], collapse = ":"))
                
                # Type conversion
                if (grepl("^[0-9]+$", value)) {
                  value <- as.integer(value)
                } else if (grepl("^[0-9.]+$", value)) {
                  value <- as.numeric(value)
                } else if (value %in% c("TRUE", "FALSE")) {
                  value <- as.logical(value)
                }
                
                meta_list[[key]] <- value
              }
            }
          }
          
          meta_list
        }, error = function(e) NULL)
      }
    }
    
    # ------------------------------------------------------------------------
    # 4.6. Attach metadata and file info
    # ------------------------------------------------------------------------
    
    if (!is.null(metadata)) {
      attr(df, "metadata") <- metadata
    }
    
    # Add file info for batch tracking
    attr(df, "source_file") <- basename(file_path)
    attr(df, "read_time") <- read_time
    
    return(df)
  }
  
  # ==========================================================================
  # 5. BATCH EXECUTION (PARALLEL OR SEQUENTIAL)
  # ==========================================================================
  
  # Setup parallel processing if requested
  if (parallel && is_batch) {
    if (!requireNamespace("future.apply", quietly = TRUE)) {
      msg <- switch(lang,
        "en" = "Package 'future.apply' required for parallel. Falling back to sequential.",
        "pt" = "Pacote 'future.apply' necessario para paralelo. Voltando para sequencial.",
        "es" = "Paquete 'future.apply' requerido para paralelo. Volviendo a secuencial."
      )
      if (verbose) cli::cli_alert_warning(msg)
      parallel <- FALSE
    } else {
      future::plan(future::multisession, workers = workers)
      on.exit(future::plan(future::sequential), add = TRUE)
    }
  }
  
  # Choose execution function
  read_func <- if (parallel && is_batch) future.apply::future_lapply else lapply
  
  # Read all files
  if (verbose && is_batch) {
    reading_msg <- switch(lang,
      "en" = "Reading files...",
      "pt" = "Lendo arquivos...",
      "es" = "Leyendo archivos..."
    )
    cli::cli_alert_info(reading_msg)
  }
  
  list_of_dfs <- read_func(all_files, function(f) {
    if (verbose) {
      file_msg <- switch(lang,
        "en" = paste0("Reading: ", basename(f)),
        "pt" = paste0("Lendo: ", basename(f)),
        "es" = paste0("Leyendo: ", basename(f))
      )
      cli::cli_alert_info(file_msg)
    }
    
    read_single_file(f)
  })
  
  # ==========================================================================
  # 6. CONSOLIDATION (FOR BATCH)
  # ==========================================================================
  
  # Remove failed reads (NULL)
  list_of_dfs <- list_of_dfs[!sapply(list_of_dfs, is.null)]
  
  if (length(list_of_dfs) == 0) {
    msg <- switch(lang,
      "en" = "No files successfully read",
      "pt" = "Nenhum arquivo lido com sucesso",
      "es" = "Ningun archivo leido exitosamente"
    )
    cli::cli_abort(msg)
  }
  
  # Single file: return directly
  if (length(list_of_dfs) == 1) {
    final_df <- list_of_dfs[[1]]
    
    if (verbose) {
      success_msg <- switch(lang,
        "en" = paste0("Successfully loaded ", format(nrow(final_df), big.mark = ","),
                     " rows and ", ncol(final_df), " columns"),
        "pt" = paste0("Carregados com sucesso ", format(nrow(final_df), big.mark = ","),
                     " registros e ", ncol(final_df), " colunas"),
        "es" = paste0("Cargados exitosamente ", format(nrow(final_df), big.mark = ","),
                     " registros y ", ncol(final_df), " columnas")
      )
      cli::cli_alert_success(success_msg)
    }
    
    return(final_df)
  }
  
  # Batch: collect metadata before combining
  all_metadata <- lapply(list_of_dfs, function(x) attr(x, "metadata"))
  all_source_files <- sapply(list_of_dfs, function(x) attr(x, "source_file"))
  
  if (verbose) {
    consolidating_msg <- switch(lang,
      "en" = paste0("Consolidating ", length(list_of_dfs), " datasets..."),
      "pt" = paste0("Consolidando ", length(list_of_dfs), " conjuntos de dados..."),
      "es" = paste0("Consolidando ", length(list_of_dfs), " conjuntos de datos...")
    )
    cli::cli_alert_info(consolidating_msg)
  }
  
  # Combine all data frames
  final_df <- dplyr::bind_rows(list_of_dfs)
  
  # Attach batch metadata
  attr(final_df, "batch_metadata") <- all_metadata
  attr(final_df, "source_files") <- all_source_files
  attr(final_df, "n_files_combined") <- length(list_of_dfs)
  
  # ==========================================================================
  # 7. FINAL SUMMARY
  # ==========================================================================
  
  if (verbose) {
    success_msg <- switch(lang,
      "en" = paste0("Successfully loaded ", format(nrow(final_df), big.mark = ","),
                   " total rows from ", length(list_of_dfs), " files"),
      "pt" = paste0("Carregados com sucesso ", format(nrow(final_df), big.mark = ","),
                   " registros totais de ", length(list_of_dfs), " arquivos"),
      "es" = paste0("Cargados exitosamente ", format(nrow(final_df), big.mark = ","),
                   " registros totales de ", length(list_of_dfs), " archivos")
    )
    cli::cli_alert_success(success_msg)
    
    # Spatial info
    if (inherits(final_df, "sf") && requireNamespace("sf", quietly = TRUE)) {
      geom_type <- as.character(unique(sf::st_geometry_type(final_df)))
      crs_info <- sf::st_crs(final_df)$input
      
      geom_msg <- switch(lang,
        "en" = paste0("Geometry type: ", paste(geom_type, collapse = ", ")),
        "pt" = paste0("Tipo de geometria: ", paste(geom_type, collapse = ", ")),
        "es" = paste0("Tipo de geometria: ", paste(geom_type, collapse = ", "))
      )
      cli::cli_alert_info(geom_msg)
      
      crs_msg <- switch(lang,
        "en" = paste0("CRS: ", crs_info),
        "pt" = paste0("CRS: ", crs_info),
        "es" = paste0("CRS: ", crs_info)
      )
      cli::cli_alert_info(crs_msg)
    }
  }

    # Check if loaded data is a climasus_df object
    if (inherits(final_df, "climasus_df")) {
      
      # Data was saved by climasus4r pipeline - preserve and update metadata
      if (verbose) {
        meta_msg <- switch(lang,
          "en" = "Detected climasus_df object - preserving metadata",
          "pt" = "Objeto climasus_df detectado - preservando metadados",
          "es" = "Objeto climasus_df detectado - preservando metadatos"
        )
        cli::cli_alert_info(meta_msg)
      }
      
      # Add read action to history
      if (is_batch) {
        history_msg <- sprintf("Read %d files from batch: %s", 
                              length(list_of_dfs), 
                              paste(basename(utils::head(all_source_files, 3)), collapse=", "))
        if (length(all_source_files) > 3) {
          history_msg <- paste0(history_msg, sprintf(" (and %d more)", length(all_source_files) - 3))
        }
      } else {
        source_file <- attr(final_df, "source_file")
        history_msg <- if (!is.null(source_file)) {
          sprintf("Read from file: %s", source_file)
        } else {
          "Read from file"
        }
      }
      
      final_df <- climasus_meta(final_df, add_history = history_msg)
      
      # Display metadata summary if verbose
      if (verbose) {
        current_stage <- climasus_meta(final_df, "stage")
        current_system <- climasus_meta(final_df, "system")
        
        if (!is.null(current_stage)) {
          stage_msg <- switch(lang,
            "en" = paste0("Pipeline stage: ", current_stage),
            "pt" = paste0("Estagio do pipeline: ", current_stage),
            "es" = paste0("Etapa del pipeline: ", current_stage)
          )
          cli::cli_alert_info(stage_msg)
        }
        
        if (!is.null(current_system)) {
          system_msg <- switch(lang,
            "en" = paste0("Health system: ", current_system),
            "pt" = paste0("Sistema de saude: ", current_system),
            "es" = paste0("Sistema de salud: ", current_system)
          )
          cli::cli_alert_info(system_msg)
        }
      }
      
    } else {
      
      # Data is NOT climasus_df (external file) - keep as-is
      if (verbose) {
        external_msg <- switch(lang,
          "en" = "Loaded external data (not from climasus4r pipeline)",
          "pt" = "Dados externos carregados (nao do pipeline climasus4r)",
          "es" = "Datos externos cargados (no del pipeline climasus4r)"
        )
        cli::cli_alert_info(external_msg)
      }
    
    return(final_df)
  }
  
  return(final_df)
}
