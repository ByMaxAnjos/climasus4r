#' Manage climasus_df S3 Class Metadata and Storage Backends
#'
#' Unified interface for getting, setting, and managing metadata in
#' \code{climasus_df} objects, with native support for three storage backends:
#' in-memory \strong{tibble} (default), columnar \strong{Parquet} files via the
#' \pkg{arrow} package, and analytical \strong{DuckDB} databases via the
#' \pkg{duckdb} package.
#'
#' @param x A \code{climasus_df} object. Required for all operations except
#'   \code{valid_values}.
#' @param field Character. Specific metadata field to retrieve. One of:
#'   \code{"system"}, \code{"stage"}, \code{"type"}, \code{"spatial"},
#'   \code{"temporal"}, \code{"backend"}, \code{"created"}, \code{"modified"},
#'   \code{"history"}, \code{"user"}.
#' @param add_history Character. A single string appended (with timestamp) to
#'   the processing history.
#' @param print_history Logical. If \code{TRUE}, prints the full processing
#'   history to the console and returns \code{invisible(NULL)}.
#' @param valid_values Character. Query the controlled vocabulary for a field.
#'   One of: \code{"system"}, \code{"stage"}, \code{"type"}, \code{"backend"}.
#'   Does not require \code{x}.
#'
#' @section Backend Operations:
#' The following named arguments trigger backend-specific operations when
#' passed via \code{...}:
#'
#' \describe{
#'   \item{\code{to_parquet = "<path>"}}{
#'     Converts the \code{climasus_df} to an Arrow Table, embeds \code{sus_meta}
#'     as JSON in the Parquet schema, and writes to \code{<path>}. Returns the
#'     updated \code{climasus_df} with \code{backend = "parquet"}.
#'   }
#'   \item{\code{from_parquet = "<path>"}}{
#'     Reads a Parquet file written by \code{to_parquet} and reconstructs a
#'     fully-featured \code{climasus_df} with all original metadata. Returns the
#'     reconstructed object. \code{x} is ignored.
#'   }
#'   \item{\code{to_duckdb = <DBI connection>}}{
#'     Registers the \code{climasus_df} as a DuckDB table (default name:
#'     \code{"climasus_data"}) and stores \code{sus_meta} in a companion table
#'     \code{<duckdb_view>__meta}. Returns the updated \code{climasus_df} with
#'     \code{backend = "duckdb"}.
#'   }
#'   \item{\code{from_duckdb = <DBI connection>}}{
#'     Reads a table from a DuckDB connection and reconstructs a
#'     \code{climasus_df}. Combine with \code{duckdb_view} and
#'     \code{duckdb_query} for fine-grained control.
#'   }
#'   \item{\code{duckdb_view = "<name>"}}{
#'     Name of the DuckDB table/view to read or write (default:
#'     \code{"climasus_data"}).
#'   }
#'   \item{\code{duckdb_query = "<SQL>"}}{
#'     Optional SQL \code{WHERE} clause or full \code{SELECT} statement applied
#'     when reading from DuckDB.
#'   }
#' }
#'
#' @param ... Named arguments for metadata updates (e.g., \code{system = "SIH"},
#'   \code{stage = "clean"}) or backend operations (see \strong{Backend
#'   Operations} section above).
#'
#' @return Depends on the operation:
#' \itemize{
#'   \item \strong{Get all metadata}: Named list with all metadata fields.
#'   \item \strong{Get specific field}: Value of the requested field.
#'   \item \strong{Update metadata}: Updated \code{climasus_df} object.
#'   \item \strong{Add history}: Updated \code{climasus_df} with new history entry.
#'   \item \strong{Print history}: \code{invisible(NULL)} after printing.
#'   \item \strong{Valid values}: Character vector of allowed values.
#'   \item \strong{to_parquet}: Updated \code{climasus_df} (\code{backend = "parquet"}).
#'   \item \strong{from_parquet}: Reconstructed \code{climasus_df} from file.
#'   \item \strong{to_duckdb}: Updated \code{climasus_df} (\code{backend = "duckdb"}).
#'   \item \strong{from_duckdb}: Reconstructed \code{climasus_df} from DuckDB.
#' }
#'
#' @details
#' \strong{Operation Dispatch Order:}
#'
#' \enumerate{
#'   \item \code{valid_values} — vocabulary query (no \code{x} required)
#'   \item \code{from_parquet} — read Parquet file and reconstruct
#'   \item \code{from_duckdb} — read DuckDB table and reconstruct
#'   \item \code{print_history} — print processing history
#'   \item \code{add_history} — append timestamped history entry
#'   \item \code{to_parquet} — write to Parquet file
#'   \item \code{to_duckdb} — register in DuckDB
#'   \item \code{field} getter — return single metadata field
#'   \item \code{...} updates — update metadata fields
#'   \item Default — return all metadata as list
#' }
#'
#' \strong{Metadata Persistence Across Backends:}
#'
#' When writing to Parquet, \code{sus_meta} is serialised as JSON and stored
#' in the Arrow schema metadata under the key \code{"climasus_meta"}, making it
#' fully recoverable after reading the file back.
#'
#' When writing to DuckDB, \code{sus_meta} is stored in a companion table named
#' \code{<view_name>__meta} within the same connection, enabling SQL-level
#' introspection of pipeline provenance.
#'
#' @examples
#' \dontrun{
#' # ── Basic metadata operations ──────────────────────────────────────────────
#'
#' # Get all metadata
#' meta <- sus_meta(df)
#'
#' # Get specific field
#' sus_meta(df, "stage")     # "filter_cid"
#' sus_meta(df, "backend")   # "tibble"
#'
#' # Update metadata
#' df <- sus_meta(df, stage = "clean", type = "clean")
#'
#' # Add to processing history
#' df <- sus_meta(df, add_history = "Removed missing values")
#'
#' # Print history
#' sus_meta(df, print_history = TRUE)
#'
#' # Query controlled vocabulary
#' sus_meta(valid_values = "backend")   # "tibble" "parquet" "duckdb"
#' sus_meta(valid_values = "system")    # "SIM" "SIH" ...
#'
#' # ── Arrow / Parquet backend ────────────────────────────────────────────────
#'
#' # Write to Parquet (metadata embedded in schema)
#' df <- sus_meta(df, to_parquet = "data/sim_respiratory.parquet")
#' sus_meta(df, "backend")  # "parquet"
#'
#' # Read back as climasus_df (metadata fully restored)
#' df2 <- sus_meta(from_parquet = "data/sim_respiratory.parquet")
#' sus_meta(df2, "stage")   # "filter_cid"
#'
#' # ── DuckDB backend ─────────────────────────────────────────────────────────
#'
#' library(duckdb)
#' con <- duckdb::dbConnect(duckdb::duckdb())
#'
#' # Register as DuckDB view
#' df <- sus_meta(df, to_duckdb = con, duckdb_view = "sim_respiratory")
#'
#' # SQL query directly on the view
#' DBI::dbGetQuery(con, "SELECT sexo, COUNT(*) AS n FROM sim_respiratory GROUP BY sexo")
#'
#' # Read back as climasus_df (with optional SQL filter)
#' df3 <- sus_meta(from_duckdb = con,
#'                 duckdb_view  = "sim_respiratory",
#'                 duckdb_query = "WHERE ano_obito = 2020")
#' sus_meta(df3, "stage")   # "filter_cid"
#'
#' duckdb::dbDisconnect(con)
#' }
#'
#' @seealso
#' \code{\link{write_parquet_climasus}}, \code{\link{write_duckdb_climasus}}
#'
#' @export
sus_meta <- function(
    x             = NULL,
    field         = NULL,
    add_history   = NULL,
    print_history = FALSE,
    valid_values  = NULL,
    ...
) {

  # Capture all named extra arguments once
  dots <- list(...)

  # Extract backend-specific arguments from dots (remove before metadata update)
  to_parquet   <- dots[["to_parquet"]]
  from_parquet <- dots[["from_parquet"]]
  to_duckdb    <- dots[["to_duckdb"]]
  from_duckdb  <- dots[["from_duckdb"]]
  duckdb_view  <- dots[["duckdb_view"]]  %||% "climasus_data"
  duckdb_query <- dots[["duckdb_query"]]

  # Remove backend keys from dots so they don't reach update_sus_meta_internal
  backend_keys <- c("to_parquet", "from_parquet", "to_duckdb", "from_duckdb",
                    "duckdb_view", "duckdb_query")
  meta_dots <- dots[!names(dots) %in% backend_keys]

  # ── 1. valid_values query (no x required) ──────────────────────────────────
  if (!is.null(valid_values)) {
    return(get_valid_values_internal(valid_values))
  }

  # ── 2. from_parquet: read Parquet → climasus_df (x not needed) ─────────────
  if (!is.null(from_parquet)) {
    return(from_arrow_climasus(from_parquet))
  }

  # ── 3. from_duckdb: read DuckDB → climasus_df (x not needed) ───────────────
  if (!is.null(from_duckdb)) {
    return(from_duckdb_climasus(
      con       = from_duckdb,
      view_name = duckdb_view,
      query     = duckdb_query
    ))
  }

  # ── Require x for all remaining operations ──────────────────────────────────
  if (is.null(x)) {
    cli::cli_abort(
      c(
        "`x` is required for this operation.",
        "i" = "Use {.code sus_meta(from_parquet = '<path>')} or ",
        " " = "{.code sus_meta(from_duckdb = con)} to read without {.arg x}."
      )
    )
  }

  # ── 4. print_history ────────────────────────────────────────────────────────
  if (isTRUE(print_history)) {
    return(print_history_internal(x))
  }

  # ── 5. add_history ───────────────────────────────────────────────────────────
  if (!is.null(add_history)) {
    x <- add_climasus_history_internal(x, add_history)
    # If no other operations requested, return early
    if (is.null(to_parquet) && is.null(to_duckdb) &&
        is.null(field) && length(meta_dots) == 0) {
      return(x)
    }
  }

  # ── 6. to_parquet: write climasus_df → Parquet ──────────────────────────────
  if (!is.null(to_parquet)) {
    if (!is.character(to_parquet) || length(to_parquet) != 1) {
      cli::cli_abort(
        "{.arg to_parquet} must be a single character string (file path)."
      )
    }
    return(write_parquet_climasus(x, path = to_parquet))
  }

  # ── 7. to_duckdb: register climasus_df → DuckDB ─────────────────────────────
  if (!is.null(to_duckdb)) {
    if (!inherits(to_duckdb, "DBIConnection")) {
      cli::cli_abort(
        c(
          "{.arg to_duckdb} must be a DBI connection object.",
          "i" = "Create one with: {.code con <- duckdb::dbConnect(duckdb::duckdb())}"
        )
      )
    }
    return(as_duckdb_climasus(x, con = to_duckdb, view_name = duckdb_view))
  }

  # ── 8. field getter ─────────────────────────────────────────────────────────
  if (!is.null(field) && length(meta_dots) == 0) {
    return(get_climasus_field_internal(x, field))
  }

  # ── 9. metadata update (named args in ...) ───────────────────────────────────
  if (length(meta_dots) > 0) {
    return(do.call(update_sus_meta_internal, c(list(x), meta_dots)))
  }

  # ── 10. get all metadata (default) ──────────────────────────────────────────
  return(get_sus_meta_internal(x))
}
