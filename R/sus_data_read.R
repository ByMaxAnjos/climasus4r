#' Read Processed Health Data with Automatic Format Detection
#'
#' Intelligently reads health data files exported by `sus_data_export()` or
#' created externally. Automatically detects file format, reads spatial data,
#' loads metadata, and validates data integrity.
#'
#' @param file_path Character string specifying the input file path.
#' @param format Character string specifying the input format. Options:
#'   `"rds"`, `"arrow"`, `"geoparquet"`, `"shapefile"`, `"gpkg"`, `"geojson"`, `"csv"`.
#'   If `NULL` (default), automatically detects format from file extension.
#' @param read_metadata Logical. If `TRUE` (default), attempts to load companion
#'   metadata file (if it exists) and attaches it as an attribute to the returned object.
#' @param validate Logical. If `TRUE` (default), validates data integrity after loading.
#'   For spatial data, checks geometry validity and CRS.
#' @param lang Character string specifying the language for messages. Options:
#'   `"en"` (English), `"pt"` (Portuguese, default), `"es"` (Spanish).
#' @param verbose Logical. If `TRUE` (default), prints loading progress and summary.
#'
#' @return A data frame or sf object (for spatial data) containing the loaded data.
#'   If metadata was found and `read_metadata = TRUE`, metadata is attached as
#'   an attribute accessible via `attr(df, "metadata")`.
#'
#' @details
#' **Automatic Format Detection**:
#' The function automatically detects the file format based on the extension:
#' \itemize{
#'   \item `.rds` → RDS format
#'   \item `.parquet`, `.arrow` → Arrow/Parquet or GeoParquet (auto-detected)
#'   \item `.geoparquet` → GeoParquet
#'   \item `.shp` → Shapefile
#'   \item `.gpkg` → GeoPackage
#'   \item `.geojson` → GeoJSON
#'   \item `.csv` → CSV
#' }
#'
#' **GeoParquet Detection**:
#' For `.parquet` files, the function automatically determines if it's a GeoParquet
#' (spatial) or regular Parquet file by checking for spatial metadata.
#'
#' **Metadata Loading**:
#' If a companion metadata file exists (e.g., `data_metadata.txt` for `data.parquet`),
#' it is automatically loaded and attached to the returned object.
#'
#' **Data Validation**:
#' When `validate = TRUE`, the function checks:
#' \itemize{
#'   \item Non-zero rows and columns
#'   \item For spatial data: valid geometries, defined CRS, valid bounding box
#' }
#'
#' @examples
#' \dontrun{
#' library(climasus4r)
#'
#' # Read with automatic format detection
#' df <- sus_data_read("output/data.parquet")
#'
#' # Read spatial data (GeoParquet)
#' sf_data <- sus_data_read("output/spatial_data.geoparquet")
#'
#' # Read shapefile
#' sf_data <- sus_data_read("output/spatial_data.shp")
#'
#' # Read without metadata
#' df <- sus_data_read("output/data.rds", read_metadata = FALSE)
#'
#' # Access metadata
#' df <- sus_data_read("output/data.parquet")
#' metadata <- attr(df, "metadata")
#' print(metadata$source_system)
#' print(metadata$years)
#'
#' # Silent reading
#' df <- sus_data_read("output/data.csv", verbose = FALSE)
#' }
#'
#' @export
#' @importFrom sf st_read st_is_valid st_crs
#' @importFrom arrow read_parquet
#' @importFrom utils read.csv
#' @importFrom sfarrow st_read_parquet
sus_data_read <- function(file_path,
                          format = NULL,
                          read_metadata = TRUE,
                          validate = TRUE,
                          lang = "pt",
                          verbose = TRUE) {
  
  # ========================================================================
  # 1. INPUT VALIDATION
  # ========================================================================
  
  if (missing(file_path) || is.null(file_path)) {
    stop("file_path must be specified")
  }
  
  if (!lang %in% c("en", "pt", "es")) {
    stop("lang must be one of: 'en', 'pt', 'es'")
  }
  
  # Check if file exists
  if (!file.exists(file_path)) {
    msg <- switch(lang,
      "en" = paste0("File not found: ", file_path),
      "pt" = paste0("Arquivo nao encontrado: ", file_path),
      "es" = paste0("Archivo no encontrado: ", file_path)
    )
    stop(msg)
  }
  
  # ========================================================================
  # 2. DETECT FORMAT FROM EXTENSION
  # ========================================================================
  
  if (is.null(format)) {
    ext <- tolower(tools::file_ext(file_path))
    
    format <- switch(ext,
      "rds" = "rds",
      "parquet" = "parquet",  # Will be refined to arrow or geoparquet
      "arrow" = "arrow",
      "geoparquet" = "geoparquet",
      "shp" = "shapefile",
      "gpkg" = "gpkg",
      "geojson" = "geojson",
      "csv" = "csv",
      stop(paste0("Unsupported file extension: .", ext))
    )
    
    if (verbose) {
      msg <- switch(lang,
        "en" = paste0("Detected format from extension: ", format),
        "pt" = paste0("Formato detectado da extensao: ", format),
        "es" = paste0("Formato detectado de la extension: ", format)
      )
      cli::cli_alert_info(msg)
    }
  }
  
  # ========================================================================
  # 3. REFINE PARQUET FORMAT (arrow vs geoparquet)
  # ========================================================================
  
  if (format == "parquet") {
    # Try to detect if it's GeoParquet by checking metadata
    is_geoparquet <- FALSE
    
    tryCatch({
      if (requireNamespace("arrow", quietly = TRUE)) {
        # Read Parquet metadata
        parquet_file <- arrow::ParquetFileReader$create(file_path)
        schema <- parquet_file$GetSchema()
        
        # Check if there's a 'geometry' field or 'geo' metadata
        field_names <- names(schema)
        has_geometry <- "geometry" %in% field_names || "geom" %in% field_names
        
        # Check for GeoParquet metadata key
        metadata_keys <- names(schema$metadata)
        has_geo_metadata <- any(grepl("geo", metadata_keys, ignore.case = TRUE))
        
        is_geoparquet <- has_geometry || has_geo_metadata
      }
    }, error = function(e) {
      # If detection fails, assume regular parquet
      is_geoparquet <<- FALSE
    })
    
    format <- ifelse(is_geoparquet, "geoparquet", "arrow")
    
    if (verbose) {
      msg <- switch(lang,
        "en" = paste0("Refined format: ", format),
        "pt" = paste0("Formato refinado: ", format),
        "es" = paste0("Formato refinado: ", format)
      )
      cli::cli_alert_info(msg)
    }
  }
  
  # ========================================================================
  # 4. READ DATA BASED ON FORMAT
  # ========================================================================
  
  if (verbose) {
    msg <- switch(lang,
      "en" = paste0("Reading file: ", basename(file_path)),
      "pt" = paste0("Lendo arquivo: ", basename(file_path)),
      "es" = paste0("Leyendo archivo: ", basename(file_path))
    )
    cli::cli_alert_info(msg)
  }
  
  start_time <- Sys.time()
  
  # Read based on format
  if (format == "rds") {
    # ======================================================================
    # RDS FORMAT
    # ======================================================================
    df <- readRDS(file_path)
    
  } else if (format == "arrow") {
    # ======================================================================
    # ARROW/PARQUET FORMAT (non-spatial)
    # ======================================================================
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package 'arrow' is required to read Parquet files. Install with: install.packages('arrow')")
    }
    
    df <- arrow::read_parquet(file_path)
    df <- as.data.frame(df)
    
  } else if (format == "geoparquet") {
    # ======================================================================
    # GEOPARQUET FORMAT (spatial)
    # ======================================================================
    
    # Try geoarrow first, then sfarrow, then sf
    df <- NULL
    
    if (requireNamespace("sfarrow", quietly = TRUE)) {
      tryCatch({
        df <- sfarrow::st_read_parquet(dsn = file_path)
      }, error = function(e) {
        df <<- NULL
      })
    }
    
    if (is.null(df) && requireNamespace("sf", quietly = TRUE)) {
      tryCatch({
        df <- sf::st_read(file_path, quiet = !verbose)
      }, error = function(e) {
        df <<- NULL
      })
    }
    
    if (is.null(df)) {
      stop("Failed to read GeoParquet file. Install 'geoarrow', 'sfarrow', or 'sf' package.")
    }
    
  } else if (format == "shapefile") {
    # ======================================================================
    # SHAPEFILE FORMAT
    # ======================================================================
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("Package 'sf' is required to read shapefiles. Install with: install.packages('sf')")
    }
    
    df <- sf::st_read(file_path, quiet = !verbose)
    
  } else if (format == "gpkg") {
    # ======================================================================
    # GEOPACKAGE FORMAT
    # ======================================================================
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("Package 'sf' is required to read GeoPackage files. Install with: install.packages('sf')")
    }
    
    df <- sf::st_read(file_path, quiet = !verbose)
    
  } else if (format == "geojson") {
    # ======================================================================
    # GEOJSON FORMAT
    # ======================================================================
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("Package 'sf' is required to read GeoJSON files. Install with: install.packages('sf')")
    }
    
    df <- sf::st_read(file_path, quiet = !verbose)
    
  } else if (format == "csv") {
    # ======================================================================
    # CSV FORMAT
    # ======================================================================
    if (requireNamespace("data.table", quietly = TRUE)) {
      df <- data.table::fread(file_path, data.table = FALSE)
    } else {
      df <- read.csv(file_path, stringsAsFactors = FALSE)
    }
    
  } else {
    stop(paste0("Unsupported format: ", format))
  }
  
  end_time <- Sys.time()
  read_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  # ========================================================================
  # 5. DETECT IF SPATIAL
  # ========================================================================
  
  is_spatial <- inherits(df, "sf")
  
  # ========================================================================
  # 6. READ METADATA (if requested and exists)
  # ========================================================================
  
  metadata <- NULL
  
  if (read_metadata) {
    metadata_file <- paste0(tools::file_path_sans_ext(file_path), "_metadata.txt")
    
    if (file.exists(metadata_file)) {
      tryCatch({
        # Read metadata file
        meta_lines <- readLines(metadata_file, warn = FALSE)
        
        # Parse metadata (simple key: value format)
        metadata <- list()
        current_section <- NULL
        
        for (line in meta_lines) {
          # Skip separator lines and empty lines
          if (grepl("^=+$", line) || nchar(trimws(line)) == 0) {
            next
          }
          
          # Detect section headers
          if (grepl("^---", line)) {
            current_section <- trimws(gsub("---", "", line))
            next
          }
          
          # Parse key: value pairs
          if (grepl(":", line)) {
            parts <- strsplit(line, ":", fixed = TRUE)[[1]]
            if (length(parts) >= 2) {
              key <- trimws(parts[1])
              value <- trimws(paste(parts[-1], collapse = ":"))
              
              # Convert to appropriate type
              if (grepl("^[0-9]+$", value)) {
                value <- as.integer(value)
              } else if (grepl("^[0-9.]+$", value)) {
                value <- as.numeric(value)
              } else if (value %in% c("TRUE", "FALSE")) {
                value <- as.logical(value)
              }
              
              metadata[[key]] <- value
            }
          }
        }
        
        if (verbose) {
          msg <- switch(lang,
            "en" = "Metadata file found and loaded",
            "pt" = "Arquivo de metadados encontrado e carregado",
            "es" = "Archivo de metadatos encontrado y cargado"
          )
          cli::cli_alert_success(msg)
        }
        
      }, error = function(e) {
        if (verbose) {
          msg <- switch(lang,
            "en" = "Warning: Failed to read metadata file",
            "pt" = "Aviso: Falha ao ler arquivo de metadados",
            "es" = "Advertencia: Fallo al leer archivo de metadatos"
          )
          cli::cli_alert_warning(msg)
        }
      })
    }
  }
  
  # ========================================================================
  # 7. VALIDATE DATA (if requested)
  # ========================================================================
  
  if (validate) {
    validation_passed <- TRUE
    validation_messages <- c()
    
    # Basic validation
    if (nrow(df) == 0) {
      validation_passed <- FALSE
      validation_messages <- c(validation_messages, "Dataset has zero rows")
    }
    
    if (ncol(df) == 0) {
      validation_passed <- FALSE
      validation_messages <- c(validation_messages, "Dataset has zero columns")
    }
    
    # Spatial validation
    if (is_spatial) {
      # Check CRS
      if (is.na(sf::st_crs(df))) {
        validation_passed <- FALSE
        validation_messages <- c(validation_messages, "CRS is not defined")
      }
      
      # Check geometry validity (sample first 100 rows for performance)
      sample_size <- min(100, nrow(df))
      sample_indices <- sample(1:nrow(df), sample_size)
      invalid_geoms <- !sf::st_is_valid(df[sample_indices, ])
      
      if (any(invalid_geoms)) {
        validation_passed <- FALSE
        n_invalid <- sum(invalid_geoms)
        validation_messages <- c(validation_messages, 
                                paste0(n_invalid, " invalid geometries detected in sample"))
      }
    }
    
    if (verbose) {
      if (validation_passed) {
        msg <- switch(lang,
          "en" = "Data validation: PASSED",
          "pt" = "Validacao de dados: APROVADA",
          "es" = "Validacion de datos: APROBADA"
        )
        cli::cli_alert_success(msg)
      } else {
        msg <- switch(lang,
          "en" = paste0("Data validation: FAILED - ", paste(validation_messages, collapse = ", ")),
          "pt" = paste0("Validacao de dados: FALHOU - ", paste(validation_messages, collapse = ", ")),
          "es" = paste0("Validacion de datos: FALLO - ", paste(validation_messages, collapse = ", "))
        )
        cli::cli_alert_warning(msg)
      }
    }
  }
  
  # ========================================================================
  # 8. DISPLAY SUMMARY
  # ========================================================================
  
  if (verbose) {
    msg <- switch(lang,
      "en" = paste0("Successfully loaded ", format(nrow(df), big.mark = ","),
                   " rows and ", ncol(df), " columns"),
      "pt" = paste0("Carregados com sucesso ", format(nrow(df), big.mark = ","),
                   " registros e ", ncol(df), " colunas"),
      "es" = paste0("Cargados exitosamente ", format(nrow(df), big.mark = ","),
                   " registros y ", ncol(df), " columnas")
    )
    cli::cli_alert_success(msg)
    
    # Spatial info
    if (is_spatial) {
      geom_type <- as.character(unique(sf::st_geometry_type(df)))
      crs_info <- sf::st_crs(df)$input
      
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
    
    time_msg <- switch(lang,
      "en" = paste0("Read time: ", round(read_time, 2), " seconds"),
      "pt" = paste0("Tempo de leitura: ", round(read_time, 2), " segundos"),
      "es" = paste0("Tiempo de lectura: ", round(read_time, 2), " segundos")
    )
    cli::cli_alert_info(time_msg)
  }
  
  # ========================================================================
  # 9. ATTACH METADATA AND RETURN
  # ========================================================================
  
  if (!is.null(metadata)) {
    attr(df, "metadata") <- metadata
  }
  
  return(df)
}
