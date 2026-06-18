# =============================================================================
# sus_rap_recipe.R — Compact YAML interchange format for RAP sharing.
#
# Exported: sus_rap_recipe(), sus_rap_from_recipe()
# =============================================================================

utils::globalVariables(character(0L))

# =============================================================================
# sus_rap_recipe() — Export a RAP as a compact YAML recipe
# =============================================================================

#' Export a RAP as a Compact YAML Recipe
#'
#' Serialises the essential information of a `rap_object` — parameters,
#' pipeline steps, and package versions — into a compact YAML file (typically
#' under 50 lines). The recipe can be shared with colleagues and re-imported
#' with [sus_rap_from_recipe()].
#'
#' @param rap A `rap_object` returned by [sus_rap_read()] or [sus_rap_export()].
#' @param file_path `character(1)` — Output path for the `.yaml` file. If
#'   `NULL`, a timestamped file is created in the working directory.
#' @param include_data_hash `logical(1)` — Compute and embed a hash of the
#'   input parameters for traceability. Default `TRUE`.
#' @param lang `character(1)` — Language for messages.
#' @param overwrite `logical(1)` — Overwrite existing file. Default `FALSE`.
#'
#' @return Invisibly, the path of the created `.yaml` file.
#' @export
#'
#' @importFrom cli cli_alert_success cli_abort cli_alert_info
#' @importFrom rlang %||%
#'
#' @examples
#' \dontrun{
#' rap <- sus_rap_read("pipeline_sp.R")
#' sus_rap_recipe(rap, "pipeline_sp.yaml")
#' }
sus_rap_recipe <- function(
    rap,
    file_path         = NULL,
    include_data_hash = TRUE,
    lang              = "pt",
    overwrite         = FALSE
) {
  rlang::check_installed("yaml", reason = "para exportar receitas RAP")

  if (!inherits(rap, "rap_object"))
    cli::cli_abort("Argumento `rap` deve ser um {.cls rap_object}.")

  lang <- match.arg(lang, c("pt", "en", "es"))

  if (is.null(file_path))
    file_path <- paste0("rap_recipe_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".yaml")

  if (file.exists(file_path) && !overwrite)
    cli::cli_abort("Arquivo '{file_path}' ja existe. Use {.arg overwrite = TRUE}.")

  recipe <- list(
    rap_version         = "1.0",
    created             = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    climasus4r_version  = tryCatch(
      as.character(utils::packageVersion("climasus4r")),
      error = function(e) "?"
    ),
    pipeline_type  = rap$structure$type %||% "Pipeline Generico",
    parameters     = rap$params,
    steps          = lapply(rap$steps, function(s) {
      list(
        function_name = s$function_name,
        params        = if (length(s$important_params) > 0L) s$important_params else NULL
      )
    }),
    metadata = list(
      r_version       = rap$metadata$r_version       %||% R.version.string,
      platform        = rap$metadata$platform        %||% .Platform$OS.type,
      package_version = rap$metadata$package_version %||% "?"
    )
  )

  if (include_data_hash) {
    input_str        <- paste(unlist(rap$structure$input_params), collapse = "|")
    recipe$data_hash <- digest::digest(input_str, algo = "xxhash32")
  }

  yaml::write_yaml(recipe, file_path)
  cli::cli_alert_success("Receita RAP exportada: {.path {file_path}}")
  invisible(file_path)
}


# =============================================================================
# sus_rap_from_recipe() — Import and optionally run a YAML recipe
# =============================================================================

#' Import and Optionally Run a RAP Recipe
#'
#' Reads a YAML recipe exported by [sus_rap_recipe()] and reconstructs a
#' `rap_object`. Parameter overrides can be supplied at import time. Passing
#' `execute = TRUE` immediately re-runs the pipeline using [sus_rap_run()].
#'
#' @param recipe_path `character(1)` — Path to the `.yaml` recipe file.
#' @param override_params Named list of parameters to override from the recipe
#'   (e.g. `list(uf = "RJ", years = 2022:2024)`).
#' @param execute `logical(1)` — Run the pipeline immediately after loading.
#'   Default `FALSE`.
#' @param lang `character(1)` — Language for messages.
#' @param ... Additional arguments forwarded to [sus_rap_run()] when
#'   `execute = TRUE`.
#'
#' @return A `rap_object` (or the pipeline result when `execute = TRUE`).
#' @export
#'
#' @importFrom cli cli_alert_success cli_abort cli_alert_info
#' @importFrom rlang %||%
#'
#' @examples
#' \dontrun{
#' rap <- sus_rap_from_recipe("pipeline_sp.yaml")
#' print(rap)
#'
#' # Import with parameter overrides
#' rap_rj <- sus_rap_from_recipe("pipeline_sp.yaml",
#'                               override_params = list(uf = "RJ"))
#' }
sus_rap_from_recipe <- function(
    recipe_path,
    override_params = list(),
    execute         = FALSE,
    lang            = "pt",
    ...
) {
  rlang::check_installed("yaml", reason = "para importar receitas RAP")

  if (!file.exists(recipe_path))
    cli::cli_abort("Receita nao encontrada: {.path {recipe_path}}")

  recipe <- yaml::read_yaml(recipe_path)

  cli::cli_alert_info("Importando receita: {recipe$pipeline_type %||% '?'}")

  params <- modifyList(
    recipe$parameters %||% list(),
    override_params
  )

  steps <- lapply(recipe$steps %||% list(), function(s) {
    list(
      function_name    = s$function_name %||% "unknown",
      important_params = s$params        %||% list(),
      arguments        = list(),
      line             = ""
    )
  })

  input_params <- list(
    uf     = params$uf,
    years  = params$years,
    system = params$system
  )
  output_params <- list(
    time_unit = params$time_unit,
    group_by  = params$group_by
  )

  rap_obj <- structure(
    list(
      metadata  = recipe$metadata %||% list(),
      params    = params,
      steps     = steps,
      structure = list(
        type           = recipe$pipeline_type %||% "Pipeline Generico",
        input_params   = input_params,
        output_params  = output_params,
        total_steps    = length(steps),
        functions_used = vapply(steps, function(s) s$function_name, character(1L))
      ),
      source    = normalizePath(recipe_path, mustWork = FALSE),
      format    = "recipe",
      raw       = character(0L)
    ),
    class = c("rap_object", "list")
  )

  cli::cli_alert_success("Receita carregada: {length(steps)} etapa(s).")

  if (execute) return(sus_rap_run(rap_obj, lang = lang, ...))

  rap_obj
}
