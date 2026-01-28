# ==============================================================================
# CLIMASUS4R S3 CLASS SYSTEM - MINIMAL EXPORT VERSION
# ==============================================================================
#
# This implementation minimizes namespace pollution by exporting only:
# 1. climasus_meta() - unified metadata interface
# 2. S3 methods (required for dispatch, but invisible to users)
#
# All other functions are internal.
#
# ==============================================================================

# ==============================================================================
# INTERNAL: CONTROLLED VOCABULARIES
# ==============================================================================

#' Valid DATASUS systems
#' @keywords internal
#' @noRd
.climasus_systems <- c("SIM", "SIH", "SIA", "SINAN", "CNES", "SINASC")

#' Valid pipeline stages
#' @keywords internal
#' @noRd
.climasus_stages <- c(
  "import", "clean", "stand", "filter_cid", "filter_demo",
  "derive", "aggregate", "spatial"
)

#' Valid data types
#' @keywords internal
#' @noRd
.climasus_types <- c(
  "raw", "clean", "stand", "filter_cid", "filter_demo",
  "derive", "agg", "spatial"
)

#' Null-coalescing operator
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' hierarquia oficial de estagios
#' @keywords internal
#' @noRd
.climasus_stage_order <- c(
  "import",
  "clean",
  "stand",
  "filter_cid",
  "filter_demo",
  "derive",
  "aggregate",
  "spatial"
)
#' Funcao hierarquia oficial de estagios
#' @keywords internal
#' @noRd
is_stage_at_least <- function(current, required) {
  if (is.null(current)) return(FALSE)

  match(current, .climasus_stage_order) >=
    match(required, .climasus_stage_order)
}

# ==============================================================================
# INTERNAL: CONSTRUCTOR FUNCTIONS
# ==============================================================================

#' Low-level constructor for climasus_df objects
#' @keywords internal
#' @noRd
new_climasus_df <- function(x, meta = list()) {
  stopifnot(is.data.frame(x))
  stopifnot(is.list(meta))
  
  # Ensure meta has required structure
  meta <- utils::modifyList(
    list(
      system = NULL,
      stage = NULL,
      type = NULL,
      spatial = inherits(x, "sf"),
      temporal = NULL,
      created = Sys.time(),
      modified = Sys.time(),
      history = character(0),
      user = list()
    ),
    meta
  )
  
  # Build class vector without duplication
  base_classes <- setdiff(class(x), "climasus_df")
  
  structure(
    x,
    climasus_meta = meta,
    class = c("climasus_df", base_classes)
  )
}


#' Validate climasus_df object
#' @keywords internal
#' @noRd
validate_climasus_df <- function(x) {
  
  if (!is.data.frame(x)) {
    stop("climasus_df must inherit from data.frame", call. = FALSE)
  }
  
  meta <- attr(x, "climasus_meta")
  if (is.null(meta) || !is.list(meta)) {
    stop("climasus_meta attribute is missing or invalid", call. = FALSE)
  }
  
  # Validate system
  if (!is.null(meta$system)) {
    if (length(meta$system) != 1 || !meta$system %in% .climasus_systems) {
      stop(
        sprintf(
          "Invalid system '%s'. Must be one of: %s",
          meta$system,
          paste(.climasus_systems, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  
  # Validate stage
  if (!is.null(meta$stage)) {
    if (length(meta$stage) != 1 || !meta$stage %in% .climasus_stages) {
      stop(
        sprintf(
          "Invalid stage '%s'. Must be one of: %s",
          meta$stage,
          paste(.climasus_stages, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  
  # Validate type
  if (!is.null(meta$type)) {
    if (length(meta$type) != 1 || !meta$type %in% .climasus_types) {
      stop(
        sprintf(
          "Invalid type '%s'. Must be one of: %s",
          meta$type,
          paste(.climasus_types, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  
  # Validate spatial flag
  if (!is.logical(meta$spatial) || length(meta$spatial) != 1) {
    stop("spatial must be a single logical value", call. = FALSE)
  }
  
  # Check spatial consistency
  is_sf <- inherits(x, "sf")
  if (meta$spatial && !is_sf) {
    warning("spatial = TRUE but object does not inherit from 'sf'", call. = FALSE)
  }
  if (!meta$spatial && is_sf) {
    warning("spatial = FALSE but object inherits from 'sf'", call. = FALSE)
  }
  
  x
}


#' Test if object is a climasus_df
#' @keywords internal
#' @noRd
is_climasus_df <- function(x) {
  inherits(x, "climasus_df")
}


# ==============================================================================
# INTERNAL: METADATA ACCESSORS
# ==============================================================================

#' Get all climasus metadata
#' @keywords internal
#' @noRd
get_climasus_meta_internal <- function(x) {
  if (!is_climasus_df(x)) {
    stop(
      "`x` must be a <climasus_df> object. ",
      "Use `sus_data_import()` or `sus_data_standardize()` ",
      "to integrate your data into the climasus4r ecosystem.",
      call. = FALSE
    )
  }
  attr(x, "climasus_meta") %||% list()
}


#' Get specific metadata field
#' @keywords internal
#' @noRd
get_climasus_field_internal <- function(x, field) {
  if (!is_climasus_df(x)) {
    stop("`x` must be a climasus_df object", call. = FALSE)
  }
  
  valid_fields <- c("system", "stage", "type", "spatial", "temporal", 
                   "created", "modified", "history", "user")
  
  if (!field %in% valid_fields) {
    stop(
      sprintf(
        "Invalid field '%s'. Must be one of: %s",
        field,
        paste(valid_fields, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  meta <- get_climasus_meta_internal(x)
  meta[[field]]
}


#' Update climasus metadata
#' @keywords internal
#' @noRd
update_climasus_meta_internal <- function(x, ...) {
  if (!is_climasus_df(x)) {
    stop("`x` must be a climasus_df object", call. = FALSE)
  }
  
  meta_old <- get_climasus_meta_internal(x)
  meta_new <- utils::modifyList(meta_old, list(...))
  meta_new$modified <- Sys.time()
  
  obj <- new_climasus_df(x, meta_new)
  validate_climasus_df(obj)
}

#' Add entry to processing history
#' @keywords internal
#' @noRd
add_climasus_history_internal <- function(x, step) {
  if (!is_climasus_df(x)) {
    stop("`x` must be a climasus_df object", call. = FALSE)
  }
  
  if (!is.character(step) || length(step) != 1) {
    stop("`step` must be a single character string", call. = FALSE)
  }
  
  history <- get_climasus_field_internal(x, "history")
  
  timestamped_step <- sprintf(
    "[%s] %s",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    step
  )
  
  new_history <- c(history, timestamped_step)
  update_climasus_meta_internal(x, history = new_history)
}

#' Print processing history
#' @keywords internal
#' @noRd
print_history_internal <- function(x) {
  if (!is_climasus_df(x)) {
    stop("`x` must be a climasus_df object", call. = FALSE)
  }
  
  history <- get_climasus_field_internal(x, "history")
  
  if (length(history) == 0) {
    cat("No processing history recorded.\n")
    return(invisible(NULL))
  }
  
  cat("Processing History:\n")
  cat(paste0("  ", seq_along(history), ". ", history, "\n"), sep = "")
  
  invisible(NULL)
}


#' Get valid values for metadata fields
#' @keywords internal
#' @noRd
get_valid_values_internal <- function(field) {
  field <- match.arg(field, c("system", "stage", "type"))
  
  switch(field,
    system = .climasus_systems,
    stage = .climasus_stages,
    type = .climasus_types
  )
}

# ==============================================================================
# EXPORTED: S3 METHODS
# ==============================================================================

#' Print method for climasus_df
#' @param x A climasus_df object
#' @param n Integer. Number of rows to print
#' @param ... Additional arguments passed to print.data.frame
#' @export
print.climasus_df <- function(x, n = 10, ...) {
  
  meta <- attr(x, "climasus_meta") %||% list()
  
  # Header
  cat(sprintf("<climasus_df [%d x %d]>\n", nrow(x), ncol(x)))
  
  # Metadata summary
  cat("System:  ", meta$system %||% "unknown", "\n", sep = "")
  cat("Stage:   ", meta$stage %||% "unknown", "\n", sep = "")
  cat("Type:    ", meta$type %||% "unknown", "\n", sep = "")
  cat("Spatial: ", meta$spatial %||% FALSE, "\n", sep = "")
  
  if (!is.null(meta$created)) {
    cat("Created: ", format(meta$created, "%Y-%m-%d %H:%M:%S"), "\n", sep = "")
  }
  
  if (length(meta$history) > 0) {
    cat("History: ", length(meta$history), " steps\n", sep = "")
  }
  
  cat("\n")
  
  # Print data
  if (nrow(x) > n) {
    cat("# Showing first", n, "of", nrow(x), "rows\n")
    print(utils::head(x, n), ...)
    cat("# ... with", nrow(x) - n, "more rows\n")
  } else {
    NextMethod()
  }
  
  invisible(x)
}


#' Subsetting method for climasus_df
#' @param x A climasus_df object
#' @param i Row indices
#' @param j Column indices
#' @param drop Logical
#' @export
`[.climasus_df` <- function(x, i, j, drop = FALSE) {
  
  meta <- attr(x, "climasus_meta")
  out <- NextMethod()
  
  if (is.data.frame(out)) {
    meta$modified <- Sys.time()
    out <- new_climasus_df(out, meta)
  }
  
  out
}


#' Column extraction for climasus_df
#' @param x A climasus_df object
#' @param i Column index or name
#' @export
`[[.climasus_df` <- function(x, i) {
  NextMethod()
}


#' Column assignment for climasus_df
#' @param x A climasus_df object
#' @param name Column name
#' @param value New column values
#' @export
`$<-.climasus_df` <- function(x, name, value) {
  
  meta <- attr(x, "climasus_meta")
  x <- NextMethod()
  meta$modified <- Sys.time()
  
  new_climasus_df(x, meta)
}


#' Row binding for climasus_df objects
#' @param ... climasus_df objects to combine
#' @param deparse.level Integer
#' @export
rbind.climasus_df <- function(..., deparse.level = 1) {
  
  objects <- list(...)
  are_climasus <- sapply(objects, is_climasus_df)
  
  if (!all(are_climasus)) {
    warning(
      "Not all objects are climasus_df. Metadata from first climasus_df will be used.",
      call. = FALSE
    )
  }
  
  first_climasus <- which(are_climasus)[1]
  meta <- attr(objects[[first_climasus]], "climasus_meta")
  
  if (sum(are_climasus) > 1) {
    systems <- unique(sapply(objects[are_climasus], function(o) attr(o, "climasus_meta")$system))
    stages <- unique(sapply(objects[are_climasus], function(o) attr(o, "climasus_meta")$stage))
    
    if (length(systems) > 1 || length(stages) > 1) {
      warning(
        "Metadata conflicts detected. Using metadata from first object.",
        call. = FALSE
      )
    }
  }
  
  out <- do.call(rbind.data.frame, c(objects, list(deparse.level = deparse.level)))
  
  meta$modified <- Sys.time()
  meta$history <- c(
    meta$history,
    sprintf("[%s] rbind: combined %d objects",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            length(objects))
  )
  
  new_climasus_df(out, meta)
}


#' Coerce climasus_df to data.frame
#' @param x A climasus_df object
#' @param ... Additional arguments (ignored)
#' @export
as.data.frame.climasus_df <- function(x, ...) {
  
  class(x) <- setdiff(class(x), "climasus_df")
  attr(x, "climasus_meta") <- NULL
  
  x
}


# ==============================================================================
# INTERNAL: PIPELINE INTEGRATION HELPER
# ==============================================================================

#' Convert data.frame to climasus_df if needed
#'
#' Helper function for pipeline functions to ensure input is climasus_df.
#' If input is already climasus_df, returns as-is.
#' If input is data.frame, converts to climasus_df with provided metadata.
#'
#' @param df A data.frame or climasus_df object
#' @param system Character. DATASUS system
#' @param stage Character. Pipeline stage
#' @param type Character. Data type
#' @param ... Additional metadata
#'
#' @return A climasus_df object
#' @keywords internal
#' @noRd
ensure_climasus_df <- function(df, system = NULL, stage = NULL, type = NULL, ...) {
  
  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame", call. = FALSE)
  }
  
  if (is_climasus_df(df)) {
    return(df)
  }
  
  # Convert to climasus_df
  meta <- list(
    system = system,
    stage = stage,
    type = type,
    spatial = inherits(df, "sf"),
    ...
  )
  
  obj <- new_climasus_df(df, meta)
  validate_climasus_df(obj)
}


# ==============================================================================
# EXAMPLE PIPELINE FUNCTION PATTERN
# ==============================================================================

# This is an example of how pipeline functions should integrate with climasus_df
# 
# sus_data_filter_cid <- function(df, disease_group, lang = "en", verbose = TRUE) {
#   
#   # 1. Ensure input is climasus_df
#   df <- ensure_climasus_df(df, stage = "filter_cid", type = "filter_cid")
#   
#   # 2. Perform filtering
#   df_filtered <- filter_logic(df, disease_group)
#   
#   # 3. Update metadata
#   df_filtered <- climasus_meta(
#     df_filtered,
#     stage = "filter_cid",
#     type = "filter_cid"
#   )
#   
#   # 4. Add to history
#   df_filtered <- climasus_meta(
#     df_filtered,
#     add_history = sprintf("Filtered by disease group: %s", disease_group)
#   )
#   
#   # 5. Return
#   return(df_filtered)
# }
