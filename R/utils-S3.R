# ==============================================================================
# CLIMASUS4R S3 CLASS SYSTEM - MINIMAL EXPORT VERSION
# ==============================================================================
#
# This implementation minimizes namespace pollution by exporting only:
# 1. sus_meta()           - unified metadata interface
# 2. as_arrow_climasus()  - convert to Arrow Table (preserves metadata)
# 3. as_duckdb_climasus() - register as DuckDB view (preserves metadata)
# 4. from_arrow_climasus()  - reconstruct climasus_df from Arrow Table
# 5. from_duckdb_climasus() - reconstruct climasus_df from DuckDB view
# 6. S3 methods (required for dispatch, but invisible to users)
#
# Storage backends supported:
#   - tibble  (default, in-memory)
#   - parquet (via arrow package, columnar on-disk)
#   - duckdb  (via duckdb package, analytical SQL engine)
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
  "derive", "aggregate", "spatial", "climate", "census"
)

#' Valid data types
#' @keywords internal
#' @noRd
.climasus_types <- c(
  # import and start
  "raw", "clean", "stand",
  # Filter
  "filter_cid", "filter_demo",
  # Create variable and aggregation
  "derive", "agg",
  # climate
  "inmet", "filled",
  # spatial
  "munic", "cep", "schools", "health_region", "amazon",
  "semiarid", "biomes", "conservation_units", "disaster_risk_area",
  "indigenous_land", "urban_area", "metro_area", "urban_concentrations",
  "pop_arrangements", "health_facilities", "neighborhood",
  # census
  "population", "households", "families", "mortality", "emigration", "tracts",
  # Temporal strategy-climate aggregate
  "exact", "discrete_lag", "moving_window",
  "offset_window", "distributed_lag", "degree_days", "seasonal",
  "threshold_exceedance", "weighted_window", "cold_wave_exceedance",
  # Climate-bioindicators
  "indicators"
)

#' Valid storage backends for climasus_df
#' @keywords internal
#' @noRd
.climasus_backends <- c("tibble", "parquet", "duckdb")

#' Null-coalescing operator
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Official stage hierarchy
#' @keywords internal
#' @noRd
.climasus_stage_order <- c(
  "import", "clean", "stand", "filter_cid", "filter_demo",
  "derive", "aggregate", "spatial", "census", "climate"
)

#' Check if current stage is at least a required stage
#' @keywords internal
#' @noRd
is_stage_at_least <- function(current, required) {
  if (is.null(current)) return(FALSE)
  match(current, .climasus_stage_order) >=
    match(required, .climasus_stage_order)
}

# ==============================================================================
# INTERNAL: METADATA SERIALISATION HELPERS
# ==============================================================================

#' Serialise sus_meta list to a JSON string (for Arrow schema metadata)
#' @keywords internal
#' @noRd
.meta_to_json <- function(meta) {
  # Use jsonlite if available, otherwise a simple fallback
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::toJSON(meta, auto_unbox = TRUE, POSIXt = "string", null = "null")
  } else {
    # Minimal fallback: store key scalar fields only
    paste0(
      '{"system":"', meta$system %||% "", '",',
      '"stage":"',   meta$stage  %||% "", '",',
      '"type":"',    meta$type   %||% "", '",',
      '"spatial":',  tolower(as.character(meta$spatial %||% FALSE)), '}'
    )
  }
}

#' Deserialise JSON string back to sus_meta list
#' @keywords internal
#' @noRd
.meta_from_json <- function(json_str) {
  if (is.null(json_str) || nchar(json_str) == 0) return(list())
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    tryCatch(
      jsonlite::fromJSON(json_str, simplifyVector = TRUE),
      error = function(e) list()
    )
  } else {
    list()
  }
}

# ==============================================================================
# INTERNAL: CONSTRUCTOR FUNCTIONS
# ==============================================================================

#' Low-level constructor for climasus_df objects
#' @keywords internal
#' @noRd
  new_climasus_df <- function(x, meta = list()) {

    # Accept Arrow Tables as input (convert to tibble first)
    if (inherits(x, "ArrowTabular")) {
      if (!requireNamespace("arrow", quietly = TRUE)) {
        stop("Package 'arrow' is required to convert Arrow Tables. Install with: install.packages('arrow')", call. = FALSE)
      }
      x <- dplyr::as_tibble(x)
    }

    stopifnot(is.data.frame(x))
    stopifnot(is.list(meta))

    base <- dplyr::as_tibble(unclass(x))

    # Ensure meta has required structure
    meta <- utils::modifyList(
      list(
        system   = NULL,
        stage    = NULL,
        type     = NULL,
        spatial  = inherits(x, "sf"),
        temporal = NULL,
        backend  = "tibble",          # NEW: storage backend tracker
        created  = Sys.time(),
        modified = Sys.time(),
        history  = character(0),
        user     = list()
      ),
      meta
    )

    structure(
      base,
      sus_meta = meta,
      class = c("climasus_df", "tbl_df", "tbl", "data.frame")
    )
  }

#' Validate climasus_df object
#' @keywords internal
#' @noRd
validate_climasus_df <- function(x) {

  if (!is.data.frame(x)) {
    stop("climasus_df must inherit from data.frame", call. = FALSE)
  }

  meta <- attr(x, "sus_meta")
  if (is.null(meta) || !is.list(meta)) {
    stop("sus_meta attribute is missing or invalid", call. = FALSE)
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

  # Validate backend
  if (!is.null(meta$backend)) {
    if (!meta$backend %in% .climasus_backends) {
      stop(
        sprintf(
          "Invalid backend '%s'. Must be one of: %s",
          meta$backend,
          paste(.climasus_backends, collapse = ", ")
        ),
        call. = FALSE
      )
    }
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
get_sus_meta_internal <- function(x) {
  if (!is_climasus_df(x)) {
    stop(
      "`x` must be a <climasus_df> object. ",
      "Use `sus_data_import()` or `sus_data_standardize()` ",
      "to integrate your data into the climasus4r ecosystem.",
      call. = FALSE
    )
  }
  attr(x, "sus_meta") %||% list()
}

#' Get specific metadata field
#' @keywords internal
#' @noRd
get_climasus_field_internal <- function(x, field) {
  if (!is_climasus_df(x)) {
    stop("`x` must be a climasus_df object", call. = FALSE)
  }

  valid_fields <- c("system", "stage", "type", "spatial", "temporal",
                    "backend", "created", "modified", "history", "user")

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

  meta <- get_sus_meta_internal(x)
  meta[[field]]
}

#' Update climasus metadata
#' @keywords internal
#' @noRd
update_sus_meta_internal <- function(x, ...) {
  if (!is_climasus_df(x)) {
    stop("`x` must be a climasus_df object", call. = FALSE)
  }

  meta_old <- get_sus_meta_internal(x)
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
  update_sus_meta_internal(x, history = new_history)
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
    cli::cli_alert_info("No processing history recorded.")
    return(invisible(NULL))
  }

  cli::cli_h2("Processing History")
  cli::cli_text("{.dim {length(history)} step(s) recorded}")
  cli::cli_text("")
  cli::cli_ol()

  for (entry in history) {
    ts  <- sub("^\\[(.*?)\\].*$", "\\1", entry)
    msg <- sub("^\\[.*?\\]\\s*", "", entry)
    cli::cli_li("{.dim [{ts}]} {msg}")
  }

  invisible(NULL)
}

#' Get valid values for metadata fields
#' @keywords internal
#' @noRd
get_valid_values_internal <- function(field) {
  field <- match.arg(field, c("system", "stage", "type", "backend"))

  switch(field,
    system  = .climasus_systems,
    stage   = .climasus_stages,
    type    = .climasus_types,
    backend = .climasus_backends
  )
}

# ==============================================================================
# EXPORTED: UNIFIED METADATA INTERFACE
# ==============================================================================

#' Access and update climasus_df metadata
#'
#' A single, unified interface for reading and writing metadata on
#' \code{climasus_df} objects. Minimises namespace pollution by replacing
#' the previous family of ten accessor functions.
#'
#' @param x A \code{climasus_df} object, or \code{NULL} when using
#'   \code{valid_values}.
#' @param field Character. When provided as the only extra argument, returns
#'   the value of that metadata field (\code{"system"}, \code{"stage"},
#'   \code{"type"}, \code{"spatial"}, \code{"temporal"}, \code{"backend"},
#'   \code{"created"}, \code{"modified"}, \code{"history"}, \code{"user"}).
#' @param system Character. DATASUS system identifier (e.g. \code{"SIM"}).
#' @param stage Character. Pipeline stage (e.g. \code{"filter_cid"}).
#' @param type Character. Data type (e.g. \code{"raw"}).
#' @param backend Character. Storage backend: \code{"tibble"} (default),
#'   \code{"parquet"}, or \code{"duckdb"}.
#' @param add_history Character. A single string appended (with timestamp) to
#'   the processing history.
#' @param print_history Logical. If \code{TRUE}, prints the history and returns
#'   \code{invisible(NULL)}.
#' @param valid_values Character. When provided, returns the allowed values for
#'   that vocabulary (\code{"system"}, \code{"stage"}, \code{"type"},
#'   \code{"backend"}).
#' @param ... Additional named metadata fields to update.
#'
#' @return The updated \code{climasus_df} object, the requested field value, or
#'   \code{invisible(NULL)} when \code{print_history = TRUE}.
#'
#' @examples
#' \dontrun{
#' # Get all metadata
#' sus_meta(df)
#'
#' # Get a specific field
#' sus_meta(df, "stage")
#'
#' # Update stage and type
#' df <- sus_meta(df, stage = "filter_cid", type = "filter_cid")
#'
#' # Add to history
#' df <- sus_meta(df, add_history = "Filtered by respiratory CID codes")
#'
#' # Print history
#' sus_meta(df, print_history = TRUE)
#'
#' # Query valid values
#' sus_meta(valid_values = "backend")
#' }
#' @export
sus_meta <- function(
    x             = NULL,
    field         = NULL,
    system        = NULL,
    stage         = NULL,
    type          = NULL,
    backend       = NULL,
    add_history   = NULL,
    print_history = FALSE,
    valid_values  = NULL,
    ...
) {

  # ── 0. valid_values query (no x required) ──────────────────────────────────
  if (!is.null(valid_values)) {
    return(get_valid_values_internal(valid_values))
  }

  # ── 1. print_history ────────────────────────────────────────────────────────
  if (isTRUE(print_history)) {
    return(print_history_internal(x))
  }

  # ── 2. field getter ─────────────────────────────────────────────────────────
  if (!is.null(field) && is.null(system) && is.null(stage) &&
      is.null(type) && is.null(backend) && is.null(add_history) && ...length() == 0) {
    return(get_climasus_field_internal(x, field))
  }

  # ── 3. get all metadata (no modifiers supplied) ─────────────────────────────
  if (is.null(field) && is.null(system) && is.null(stage) &&
      is.null(type) && is.null(backend) && is.null(add_history) && ...length() == 0) {
    return(get_sus_meta_internal(x))
  }

  # ── 4. add_history ───────────────────────────────────────────────────────────
  if (!is.null(add_history)) {
    x <- add_climasus_history_internal(x, add_history)
  }

  # ── 5. update metadata fields ────────────────────────────────────────────────
  updates <- c(
    if (!is.null(system))  list(system  = system),
    if (!is.null(stage))   list(stage   = stage),
    if (!is.null(type))    list(type    = type),
    if (!is.null(backend)) list(backend = backend),
    list(...)
  )

  if (length(updates) > 0) {
    x <- do.call(update_sus_meta_internal, c(list(x), updates))
  }

  x
}

# ==============================================================================
# INTERNAL: ARROW PARQUET BACKEND
# ==============================================================================

#' Convert a climasus_df to an Arrow Table, preserving metadata
#'
#' Converts the in-memory \code{climasus_df} tibble to an
#' \code{arrow::Table}. The \code{sus_meta} metadata is serialised as JSON
#' and stored in the Arrow schema metadata under the key
#' \code{"climasus_meta"}, so it can be recovered with
#' \code{from_arrow_climasus()}.
#'
#' @param x A \code{climasus_df} object.
#' @param ... Additional arguments passed to \code{arrow::as_arrow_table()}.
#'
#' @return An \code{arrow::Table} with \code{sus_meta} embedded in schema
#'   metadata.
#'
#' @examples
#' \dontrun{
#' arrow_tbl <- as_arrow_climasus(df)
#'
#' # Write to Parquet
#' arrow::write_parquet(arrow_tbl, "data/sim_respiratory.parquet")
#'
#' # Read back and reconstruct climasus_df
#' arrow_tbl2 <- arrow::read_parquet("data/sim_respiratory.parquet",
#'                                   as_data_frame = FALSE)
#' df2 <- from_arrow_climasus(arrow_tbl2)
#' }
#' @keywords internal
#' @noRd
as_arrow_climasus <- function(x, ...) {

  if (!requireNamespace("arrow", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg arrow} is required for Parquet support.",
      "i" = "Install with: {.code install.packages('arrow')}"
    ))
  }

  if (!is_climasus_df(x)) {
    cli::cli_abort("`x` must be a {.cls climasus_df} object.")
  }

  meta <- get_sus_meta_internal(x)

  # Serialise metadata to JSON string
  meta_json <- .meta_to_json(meta)

  # Convert to Arrow Table
  tbl_plain <- as.data.frame.climasus_df(x)  # strip S3 class first
  arrow_tbl <- arrow::as_arrow_table(tbl_plain, ...)

  # Embed metadata in Arrow schema
  new_schema <- arrow_tbl$schema$WithMetadata(
    list(climasus_meta = as.character(meta_json))
  )
  arrow_tbl <- arrow_tbl$cast(new_schema)

  cli::cli_alert_success(
    "Converted to Arrow Table [{nrow(x)} x {ncol(x)}]. ",
    "Metadata embedded in schema under key {.val climasus_meta}."
  )

  arrow_tbl
}

#' Write a climasus_df directly to a Parquet file
#'
#' Convenience wrapper around \code{as_arrow_climasus()} and
#' \code{arrow::write_parquet()}. Metadata is preserved in the Parquet
#' file's schema.
#'
#' @param x A \code{climasus_df} object.
#' @param path Character. File path for the Parquet file (e.g.
#'   \code{"data/sim_2020.parquet"}).
#' @param ... Additional arguments passed to \code{arrow::write_parquet()}.
#'
#' @return \code{invisible(x)} — the original \code{climasus_df}, updated
#'   with \code{backend = "parquet"} in its metadata.
#'
#' @examples
#' \dontrun{
#' df <- write_parquet_climasus(df, "data/sim_respiratory.parquet")
#' sus_meta(df, "backend")  # "parquet"
#' }
#' @export
write_parquet_climasus <- function(x, path, ...) {

  if (!requireNamespace("arrow", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg arrow} is required.",
      "i" = "Install with: {.code install.packages('arrow')}"
    ))
  }

  if (!is_climasus_df(x)) {
    cli::cli_abort("`x` must be a {.cls climasus_df} object.")
  }

  arrow_tbl <- as_arrow_climasus(x)
  arrow::write_parquet(arrow_tbl, sink = path, ...)

  # Update backend in metadata
  x <- sus_meta(
    x,
    backend     = "parquet",
    add_history = sprintf("Written to Parquet: %s", path)
  )

  cli::cli_alert_success("Saved to {.path {path}}")
  invisible(x)
}

#' Reconstruct a climasus_df from an Arrow Table or Parquet file
#'
#' Reads back the \code{sus_meta} metadata that was embedded by
#' \code{as_arrow_climasus()} or \code{write_parquet_climasus()} and
#' reconstructs a fully-featured \code{climasus_df} object.
#'
#' @param x An \code{arrow::Table} object, or a character string giving the
#'   path to a Parquet file.
#' @param ... Additional arguments passed to \code{arrow::read_parquet()} when
#'   \code{x} is a file path.
#'
#' @return A \code{climasus_df} object with all original metadata restored.
#'
#' @examples
#' \dontrun{
#' # From an Arrow Table already in memory
#' df2 <- from_arrow_climasus(arrow_tbl)
#'
#' # Directly from a Parquet file
#' df3 <- from_arrow_climasus("data/sim_respiratory.parquet")
#' }
#' @keywords internal
#' @noRd
from_arrow_climasus <- function(x, ...) {

  if (!requireNamespace("arrow", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg arrow} is required.",
      "i" = "Install with: {.code install.packages('arrow')}"
    ))
  }

  # Accept file path
  if (is.character(x)) {
    if (!file.exists(x)) {
      cli::cli_abort("File not found: {.path {x}}")
    }
    x <- arrow::read_parquet(x, as_data_frame = FALSE, ...)
  }

  if (!inherits(x, "ArrowTabular")) {
    cli::cli_abort("`x` must be an Arrow Table or a path to a Parquet file.")
  }

  # Extract embedded metadata
  schema_meta <- x$schema$metadata
  meta <- if (!is.null(schema_meta[["climasus_meta"]])) {
    .meta_from_json(schema_meta[["climasus_meta"]])
  } else {
    list()
  }

  # Convert to tibble
  df_plain <- as.data.frame(x)

  # Reconstruct climasus_df
  meta$backend  <- "tibble"  # back in memory
  meta$modified <- Sys.time()

  obj <- new_climasus_df(df_plain, meta)

  cli::cli_alert_success(
    "Reconstructed {.cls climasus_df} [{nrow(obj)} x {ncol(obj)}] from Arrow Table."
  )

  obj
}

# ==============================================================================
# INTERNAL: DUCKDB BACKEND
# ==============================================================================

#' Register a climasus_df as a DuckDB view, preserving metadata
#'
#' Registers the \code{climasus_df} as an in-memory DuckDB view (or a
#' persistent table in a DuckDB file), enabling fast analytical SQL queries.
#' The \code{sus_meta} metadata is stored as a JSON string in a dedicated
#' single-row table called \code{<view_name>__meta} within the same
#' connection.
#'
#' @param x A \code{climasus_df} object.
#' @param con A DuckDB connection created with
#'   \code{duckdb::dbConnect(duckdb::duckdb())}.
#' @param view_name Character. Name of the DuckDB view/table (default:
#'   \code{"climasus_data"}).
#' @param overwrite Logical. Whether to overwrite an existing view with the
#'   same name (default: \code{TRUE}).
#'
#' @return \code{invisible(x)} — the original \code{climasus_df}, updated
#'   with \code{backend = "duckdb"} in its metadata.
#'
#' @examples
#' \dontrun{
#' library(duckdb)
#' con <- dbConnect(duckdb())
#'
#' df <- as_duckdb_climasus(df, con, view_name = "sim_respiratory")
#'
#' # Query with SQL
#' DBI::dbGetQuery(con, "SELECT sexo, COUNT(*) AS n FROM sim_respiratory GROUP BY sexo")
#'
#' # Or use dplyr
#' dplyr::tbl(con, "sim_respiratory") |> dplyr::count(sexo)
#'
#' # Reconstruct climasus_df
#' df2 <- from_duckdb_climasus(con, "sim_respiratory")
#' }
#' @keywords internal
#' @noRd
as_duckdb_climasus <- function(x, con, view_name = "climasus_data", overwrite = TRUE) {

  if (!requireNamespace("duckdb", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg duckdb} is required for DuckDB support.",
      "i" = "Install with: {.code install.packages('duckdb')}"
    ))
  }
  if (!requireNamespace("DBI", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg DBI} is required.",
      "i" = "Install with: {.code install.packages('DBI')}"
    ))
  }

  if (!is_climasus_df(x)) {
    cli::cli_abort("`x` must be a {.cls climasus_df} object.")
  }

  meta <- get_sus_meta_internal(x)
  meta_json <- as.character(.meta_to_json(meta))

  # Register data as a DuckDB view
  df_plain <- as.data.frame.climasus_df(x)
  DBI::dbWriteTable(con, view_name, df_plain, overwrite = overwrite)

  # Store metadata in a companion table: <view_name>__meta
  meta_table <- paste0(view_name, "__meta")
  meta_df    <- data.frame(meta_json = meta_json, stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, meta_table, meta_df, overwrite = TRUE)

  # Update backend in metadata
  x <- sus_meta(
    x,
    backend     = "duckdb",
    add_history = sprintf("Registered as DuckDB view: %s", view_name)
  )

  cli::cli_alert_success(
    "Registered {.val {view_name}} in DuckDB [{nrow(x)} x {ncol(x)}]. ",
    "Metadata stored in {.val {meta_table}}."
  )

  invisible(x)
}

#' Reconstruct a climasus_df from a DuckDB view
#'
#' Reads back the \code{sus_meta} metadata stored by
#' \code{as_duckdb_climasus()} and reconstructs a fully-featured
#' \code{climasus_df} object from a DuckDB connection.
#'
#' @param con A DuckDB connection.
#' @param view_name Character. Name of the DuckDB view/table to read (default:
#'   \code{"climasus_data"}).
#' @param query Character. Optional SQL \code{WHERE} clause or full
#'   \code{SELECT} statement. When \code{NULL} (default), the entire table is
#'   read. When a \code{WHERE} clause is provided (starting with
#'   \code{"WHERE"}), it is appended to \code{SELECT * FROM <view_name>}.
#'   A full \code{SELECT} statement is executed as-is.
#'
#' @return A \code{climasus_df} object with all original metadata restored.
#'
#' @examples
#' \dontrun{
#' # Full table
#' df2 <- from_duckdb_climasus(con, "sim_respiratory")
#'
#' # Filtered subset (still returns climasus_df)
#' df3 <- from_duckdb_climasus(con, "sim_respiratory",
#'                              query = "WHERE ano_obito = 2020")
#' }
#' @keywords internal
#' @noRd
from_duckdb_climasus <- function(con, view_name = "climasus_data", query = NULL) {

  if (!requireNamespace("DBI", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg DBI} is required.",
      "i" = "Install with: {.code install.packages('DBI')}"
    ))
  }

  # Build SQL
  sql <- if (is.null(query)) {
    sprintf("SELECT * FROM %s", view_name)
  } else if (grepl("^WHERE", trimws(query), ignore.case = TRUE)) {
    sprintf("SELECT * FROM %s %s", view_name, query)
  } else {
    query  # full SELECT provided by user
  }

  df_plain <- DBI::dbGetQuery(con, sql)

  # Recover metadata from companion table
  meta_table <- paste0(view_name, "__meta")
  meta <- tryCatch({
    meta_row  <- DBI::dbGetQuery(con, sprintf("SELECT meta_json FROM %s LIMIT 1", meta_table))
    if (nrow(meta_row) > 0) .meta_from_json(meta_row$meta_json[1]) else list()
  }, error = function(e) list())

  # Reconstruct climasus_df
  meta$backend  <- "tibble"  # back in memory
  meta$modified <- Sys.time()

  obj <- new_climasus_df(df_plain, meta)

  cli::cli_alert_success(
    "Reconstructed {.cls climasus_df} [{nrow(obj)} x {ncol(obj)}] from DuckDB view {.val {view_name}}."
  )

  obj
}

#' Write a climasus_df to a persistent DuckDB file
#'
#' Convenience wrapper that opens a persistent DuckDB connection to
#' \code{path}, registers the data and metadata, then closes the connection.
#'
#' @param x A \code{climasus_df} object.
#' @param path Character. File path for the DuckDB database (e.g.
#'   \code{"data/sim.duckdb"}).
#' @param view_name Character. Name of the table inside the DuckDB file
#'   (default: \code{"climasus_data"}).
#' @param overwrite Logical. Whether to overwrite an existing table (default:
#'   \code{TRUE}).
#'
#' @return \code{invisible(x)} — the original \code{climasus_df}, updated
#'   with \code{backend = "duckdb"} in its metadata.
#'
#' @examples
#' \dontrun{
#' df <- write_duckdb_climasus(df, "data/sim_respiratory.duckdb")
#' sus_meta(df, "backend")  # "duckdb"
#'
#' # Read back later
#' con <- duckdb::dbConnect(duckdb::duckdb(), "data/sim_respiratory.duckdb")
#' df2 <- from_duckdb_climasus(con, "climasus_data")
#' duckdb::dbDisconnect(con)
#' }
#' @export
write_duckdb_climasus <- function(x, path, view_name = "climasus_data", overwrite = TRUE) {

  if (!requireNamespace("duckdb", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg duckdb} is required.",
      "i" = "Install with: {.code install.packages('duckdb')}"
    ))
  }

  con <- duckdb::dbConnect(duckdb::duckdb(), dbdir = path)
  on.exit(duckdb::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  x <- as_duckdb_climasus(x, con = con, view_name = view_name, overwrite = overwrite)

  cli::cli_alert_success("Saved to DuckDB file: {.path {path}}")
  invisible(x)
}

# ==============================================================================
# EXPORTED: S3 METHODS
# ==============================================================================

#' Print method for climasus_df
#' @param x A climasus_df object
#' @param n Integer. Number of rows to print
#' @param ... Additional arguments passed to print
#' @export
print.climasus_df <- function(x, n = 10, ...) {

  meta <- attr(x, "sus_meta") %||% list()

  # Header
  cli::cli_h1("{.cls climasus_df} [{nrow(x)} x {ncol(x)}]")

  # Core metadata
  cli::cli_inform(c(
    "i" = "{.strong System:}  {.val {meta$system  %||% 'unknown'}}",
    " " = "{.strong Stage:}   {.emph {meta$stage   %||% 'unknown'}}",
    " " = "{.strong Type:}    {meta$type    %||% 'unknown'}",
    " " = "{.strong Backend:} {.field {meta$backend %||% 'tibble'}}",
    " " = "{.strong Spatial:} {meta$spatial %||% FALSE}"
  ))

  if (!is.null(meta$created)) {
    cli::cli_inform("{.strong Created:} {format(meta$created, '%Y-%m-%d %H:%M:%S')}")
  }

  if (length(meta$history) > 0) {
    cli::cli_inform("{.strong History:} {.info {length(meta$history)} step(s) recorded}")
  }

  cli::cli_text("")

  if (nrow(x) > n) {
    cli::cli_alert_info("Showing first {n} of {nrow(x)} rows:")
    print(utils::head(dplyr::as_tibble(x), n = n), ...)
    cli::cli_text("# ... with {nrow(x) - n} more rows.")
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

  meta <- attr(x, "sus_meta")
  out  <- NextMethod()

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

  meta <- attr(x, "sus_meta")
  x    <- NextMethod()
  meta$modified <- Sys.time()

  new_climasus_df(x, meta)
}

#' Row binding for climasus_df objects
#' @param ... climasus_df objects to combine
#' @param deparse.level Integer
#' @export
rbind.climasus_df <- function(..., deparse.level = 1) {

  objects      <- list(...)
  are_climasus <- sapply(objects, is_climasus_df)

  if (!all(are_climasus)) {
    warning(
      "Not all objects are climasus_df. Metadata from first climasus_df will be used.",
      call. = FALSE
    )
  }

  first_climasus <- which(are_climasus)[1]
  meta           <- attr(objects[[first_climasus]], "sus_meta")

  if (sum(are_climasus) > 1) {
    systems <- unique(sapply(objects[are_climasus], function(o) attr(o, "sus_meta")$system))
    stages  <- unique(sapply(objects[are_climasus], function(o) attr(o, "sus_meta")$stage))

    if (length(systems) > 1 || length(stages) > 1) {
      warning(
        "Metadata conflicts detected. Using metadata from first object.",
        call. = FALSE
      )
    }
  }

  out <- do.call(rbind.data.frame, c(objects, list(deparse.level = deparse.level)))

  meta$modified <- Sys.time()
  meta$history  <- c(
    meta$history,
    sprintf("[%s] rbind: combined %d objects",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            length(objects))
  )

  new_climasus_df(out, meta)
}

#' Coerce climasus_df to plain data.frame (strips metadata)
#' @param x A climasus_df object
#' @param ... Additional arguments (ignored)
#' @export
as.data.frame.climasus_df <- function(x, ...) {

  class(x)             <- setdiff(class(x), "climasus_df")
  attr(x, "sus_meta")  <- NULL

  x
}

# ==============================================================================
# INTERNAL: PIPELINE INTEGRATION HELPER
# ==============================================================================

#' Convert data.frame to climasus_df if needed
#'
#' Helper for pipeline functions. If input is already \code{climasus_df},
#' returns as-is. Otherwise wraps in a new \code{climasus_df} with the
#' supplied metadata.
#'
#' @param df A data.frame or climasus_df object.
#' @param system Character. DATASUS system.
#' @param stage Character. Pipeline stage.
#' @param type Character. Data type.
#' @param ... Additional metadata fields.
#'
#' @return A climasus_df object.
#' @keywords internal
#' @noRd
ensure_climasus_df <- function(df, system = NULL, stage = NULL, type = NULL, ...) {

  if (!is.data.frame(df)) {
    stop("`df` must be a data.frame", call. = FALSE)
  }

  if (is_climasus_df(df)) {
    return(df)
  }

  meta <- list(
    system  = system,
    stage   = stage,
    type    = type,
    spatial = inherits(df, "sf"),
    backend = "tibble",
    ...
  )

  obj <- new_climasus_df(df, meta)
  validate_climasus_df(obj)
}

# ==============================================================================
# EXAMPLE PIPELINE FUNCTION PATTERN
# ==============================================================================

# sus_data_filter_cid <- function(df, disease_group, lang = "en", verbose = TRUE) {
#
#   # 1. Ensure input is climasus_df
#   df <- ensure_climasus_df(df, stage = "filter_cid", type = "filter_cid")
#
#   # 2. Perform filtering
#   df_filtered <- filter_logic(df, disease_group)
#
#   # 3. Update metadata + history
#   df_filtered <- sus_meta(
#     df_filtered,
#     stage       = "filter_cid",
#     type        = "filter_cid",
#     add_history = sprintf("Filtered by disease group: %s", disease_group)
#   )
#
#   # 4. Return
#   return(df_filtered)
# }
