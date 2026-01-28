#' Manage climasus S3 Class System metadata 
#'
#' Unified interface for getting, setting, and managing metadata in climasus_df
#' objects. This single function replaces multiple accessor functions to keep
#' the package namespace clean.
#'
#' @param x A climasus_df object (required for get/set operations, optional for
#'   `valid_values`)
#' @param field Character. Specific metadata field to retrieve. One of: "system",
#'   "stage", "type", "spatial", "temporal", "created", "modified", "history", "user".
#' @param add_history Character. Add entry to processing history with automatic
#'   timestamp.
#' @param print_history Logical. If TRUE, prints processing history to console.
#' @param valid_values Character. Get valid values for a field. One of: "system",
#'   "stage", "type". Does not require `x`.
#' @param ... Named arguments to update metadata (e.g., `system = "SIH"`,
#'   `stage = "clean"`).
#'
#' @return Depends on the operation:
#'   \itemize{
#'     \item **Get all metadata**: Returns list with all metadata (no additional args)
#'     \item **Get specific field**: Returns value of requested field
#'     \item **Update metadata**: Returns updated climasus_df object
#'     \item **Add history**: Returns updated climasus_df object with new history entry
#'     \item **Print history**: Returns NULL invisibly after printing
#'     \item **Valid values**: Returns character vector of valid values
#'   }
#'
#' @details
#' The function behavior depends on which arguments are provided:
#'
#' **Get Operations:**
#' - `climasus_meta(x)` - Get all metadata as a list
#' - `climasus_meta(x, "system")` - Get specific field value
#'
#' **Set Operations:**
#' - `climasus_meta(x, system = "SIH", stage = "clean")` - Update metadata
#' - `climasus_meta(x, add_history = "Filtered data")` - Add history entry
#'
#' **Utility Operations:**
#' - `climasus_meta(x, print_history = TRUE)` - Print processing history
#' - `climasus_meta(valid_values = "system")` - Get valid system values
#'
#' @examples
#' \dontrun{
#' # Create climasus_df (done internally by pipeline functions)
#' df <- data.frame(x = 1:10, y = letters[1:10])
#' df <- new_climasus_df(df, list(system = "SIH", stage = "import"))
#'
#' # Get all metadata
#' meta <- climasus_meta(df)
#' str(meta)
#'
#' # Get specific field
#' system <- climasus_meta(df, "system")  # "SIH"
#' stage <- climasus_meta(df, "stage")    # "import"
#'
#' # Update metadata
#' df <- climasus_meta(df, stage = "clean", type = "clean")
#'
#' # Add to processing history
#' df <- climasus_meta(df, add_history = "Removed missing values")
#' df <- climasus_meta(df, add_history = "Standardized column names")
#'
#' # Print history
#' climasus_meta(df, print_history = TRUE)
#'
#' # Get valid values (no x needed)
#' systems <- climasus_meta(valid_values = "system")
#' stages <- climasus_meta(valid_values = "stage")
#' }
#'
#' @export
climasus_meta <- function(x = NULL,
                          field = NULL,
                          add_history = NULL,
                          print_history = FALSE,
                          valid_values = NULL,
                          ...) {
  
  # Operation 1: Get valid values (no x needed)
  if (!is.null(valid_values)) {
    return(get_valid_values_internal(valid_values))
  }
  
  # Require x for all other operations
  if (is.null(x)) {
    stop("`x` is required for this operation", call. = FALSE)
  }
  
  # Operation 2: Print history
  if (print_history) {
    return(print_history_internal(x))
  }
  
  # Operation 3: Add to history
  if (!is.null(add_history)) {
    return(add_climasus_history_internal(x, add_history))
  }
  
  # Operation 4: Get specific field
  if (!is.null(field)) {
    return(get_climasus_field_internal(x, field))
  }
  
  # Operation 5: Update metadata (if ... provided)
  dots <- list(...)
  if (length(dots) > 0) {
    return(update_climasus_meta_internal(x, ...))
  }
  
  # Operation 6: Get all metadata (default)
  return(get_climasus_meta_internal(x))
}
