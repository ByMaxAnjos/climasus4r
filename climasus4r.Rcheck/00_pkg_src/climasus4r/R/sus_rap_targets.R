# =============================================================================
# sus_rap_targets.R — Generate targets pipelines and execute RAPs via
# targets::tar_make() with optional dynamic branching.
#
# Exported: sus_rap_targets(), sus_rap_make()
# =============================================================================

utils::globalVariables(character(0L))

# ── Local labels ──────────────────────────────────────────────────────────────
.rap_tar_labels <- list(
  generating = list(pt = "Gerando _targets.R para '{type}' (branching: {branch})...",
                    en = "Generating _targets.R for '{type}' (branching: {branch})...",
                    es = "Generando _targets.R para '{type}' (branching: {branch})..."),
  file_ok    = list(pt = "Pipeline targets criado: {path}",
                    en = "Targets pipeline created: {path}",
                    es = "Pipeline targets creado: {path}"),
  running    = list(pt = "Executando via targets::tar_make()...",
                    en = "Running via targets::tar_make()...",
                    es = "Ejecutando via targets::tar_make()..."),
  run_ok     = list(pt = "Execucao targets concluida.",
                    en = "Targets execution complete.",
                    es = "Ejecucion targets completada.")
)

#' @keywords internal
#' @noRd
.trl <- function(key, lang, ...) {
  e   <- .rap_tar_labels[[key]]
  if (is.null(e)) return(key)
  txt <- e[[lang]] %||% e[["pt"]]
  if (...length() > 0L) glue::glue(txt, .envir = rlang::env(...)) else txt
}


# =============================================================================
# sus_rap_targets() — generate a _targets.R file
# =============================================================================

#' Generate a targets Pipeline from a climasus4r RAP
#'
#' Converts a `rap_object` or a pipeline expression into a ready-to-use
#' `_targets.R` file. The generated script uses `targets::tar_target()` with
#' optional dynamic branching over UFs, years, or their Cartesian product.
#'
#' @section Branching strategies:
#' \describe{
#'   \item{`"none"`}{Single analysis, no branching.}
#'   \item{`"uf"`}{One branch per UF — ideal for multi-state comparisons.}
#'   \item{`"year"`}{One branch per year.}
#'   \item{`"cross"`}{All UF-year combinations via `tidyr::crossing()`.}
#' }
#'
#' @param pipeline A `rap_object` (from [sus_rap_read()]) or a quoted
#'   pipeline expression (from `quote()`).
#' @param file_path `character(1)` — Output path. Default `"_targets.R"`.
#' @param branching `character(1)` — Branching strategy: `"none"` (default),
#'   `"uf"`, `"year"`, or `"cross"`.
#' @param workers `integer(1)` — Number of parallel workers. Default `1`.
#' @param include_qc `logical(1)` — Add a quality-control target. Default `TRUE`.
#' @param lang `character(1)` — Language for messages.
#' @param overwrite `logical(1)` — Overwrite existing file. Default `FALSE`.
#'
#' @return Invisibly, the path of the generated `_targets.R` file.
#' @export
#'
#' @importFrom cli cli_alert_info cli_alert_success cli_abort
#' @importFrom rlang %||%
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' rap <- sus_rap_read("pipeline_sp.R")
#' sus_rap_targets(rap, "_targets.R", branching = "uf", workers = 4)
#' # then run: targets::tar_make()
#' }
sus_rap_targets <- function(
    pipeline,
    file_path  = "_targets.R",
    branching  = c("none", "uf", "year", "cross"),
    workers    = 1L,
    include_qc = TRUE,
    lang       = "pt",
    overwrite  = FALSE
) {
  rlang::check_installed("targets", reason = "para gerar pipelines targets")

  branching <- match.arg(branching)
  lang      <- match.arg(lang, c("pt", "en", "es"))

  # Resolve pipeline to a rap_object or structure
  if (inherits(pipeline, "rap_object")) {
    rap            <- pipeline
    structure_obj  <- rap$structure
  } else {
    rap           <- structure(
      list(
        metadata  = list(),
        params    = list(),
        steps     = list(),
        structure = .rap_extract_structure(pipeline),
        source    = NULL,
        format    = "expression",
        raw       = character(0L)
      ),
      class = c("rap_object", "list")
    )
    structure_obj <- rap$structure
  }

  if (file.exists(file_path) && !overwrite)
    cli::cli_abort("Arquivo '{file_path}' ja existe. Use {.arg overwrite = TRUE}.")

  cli::cli_alert_info(.trl("generating", lang,
                            type   = structure_obj$type,
                            branch = branching))

  content <- .rap_generate_targets_script(
    rap        = rap,
    structure  = structure_obj,
    branching  = branching,
    workers    = workers,
    include_qc = include_qc,
    lang       = lang
  )

  writeLines(content, file_path, useBytes = FALSE)

  data_dir <- file.path(dirname(file_path), "data")
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

  cli::cli_alert_success(.trl("file_ok", lang, path = normalizePath(file_path)))
  invisible(file_path)
}


# =============================================================================
# sus_rap_make() — execute a targets pipeline
# =============================================================================

#' Execute a targets Pipeline from a RAP
#'
#' Runs a `_targets.R` pipeline (generated by [sus_rap_targets()]) using
#' `targets::tar_make()`, with optional parallelism via `workers`.
#'
#' @param rap A `rap_object`, a `tar_rap_object`, or a `character(1)` path to
#'   a `_targets.R` file. When a `rap_object` is passed, a temporary
#'   `_targets.R` is generated automatically.
#' @param workers `integer(1)` — Parallel workers. Default `1`.
#' @param reporter `character(1)` — Progress reporter for `tar_make()`.
#'   Default `"verbose"`.
#' @param branching `character(1)` — Branching strategy when `rap` is a
#'   `rap_object` (ignored when a file path is passed). Default `"none"`.
#' @param lang `character(1)` — Language for messages.
#' @param ... Additional arguments forwarded to `targets::tar_make()`.
#'
#' @return A list with `$results`, `$rap`, and `$store`.
#' @export
#'
#' @examples
#' \dontrun{
#' rap <- sus_rap_read("pipeline_sp.R")
#' result <- sus_rap_make(rap, workers = 2, branching = "uf")
#' head(result$results)
#' }
sus_rap_make <- function(
    rap,
    workers   = 1L,
    reporter  = "verbose",
    branching = "none",
    lang      = "pt",
    ...
) {
  rlang::check_installed("targets", reason = "para executar pipelines targets")
  lang <- match.arg(lang, c("pt", "en", "es"))

  # Resolve targets script path
  if (is.character(rap)) {
    targets_file <- rap
    rap_obj      <- tryCatch(sus_rap_read(rap, lang = lang, validate = FALSE),
                              error = function(e) NULL)
  } else if (inherits(rap, "rap_object")) {
    temp_dir     <- tempfile("climasus_targets_")
    dir.create(temp_dir, recursive = TRUE)
    targets_file <- file.path(temp_dir, "_targets.R")
    sus_rap_targets(rap, file_path = targets_file, branching = branching,
                    workers = workers, overwrite = TRUE, lang = lang)
    rap_obj <- rap
  } else {
    cli::cli_abort("Argumento `rap` deve ser um {.cls rap_object} ou caminho de arquivo.")
  }

  store_dir <- file.path(dirname(targets_file), "_targets")
  old_wd    <- setwd(dirname(targets_file))
  on.exit(setwd(old_wd), add = TRUE)

  cli::cli_alert_info(.trl("running", lang))

  tryCatch({
    targets::tar_config_set(store = store_dir)
    targets::tar_make(
      script   = targets_file,
      reporter = reporter,
      workers  = workers,
      ...
    )
  }, error = function(e) cli::cli_abort("Falha na execucao targets: {e$message}"))

  # Try to read final results target
  results <- tryCatch(
    targets::tar_read("climasus_pipeline_results", store = store_dir),
    error = function(e) NULL
  )

  cli::cli_alert_success(.trl("run_ok", lang))

  structure(
    list(results = results, rap = rap_obj, store = store_dir),
    class = "sus_targets_run"
  )
}

#' @export
print.sus_targets_run <- function(x, ...) {
  cat(sprintf(
    "<sus_targets_run>\n  store   : %s\n  results : %s rows\n",
    x$store,
    if (is.data.frame(x$results)) nrow(x$results) else "?"
  ))
  invisible(x)
}


# =============================================================================
# Internal — generate _targets.R content
# =============================================================================

#' @keywords internal
#' @noRd
.rap_generate_targets_script <- function(rap, structure, branching, workers,
                                          include_qc, lang) {
  ip <- structure$input_params
  op <- structure$output_params

  scheduler <- if (workers > 1L) "\"multicore\"" else "\"sequential\""

  header <- c(
    "# Generated by climasus4r::sus_rap_targets()",
    sprintf("# Pipeline: %s", structure$type),
    sprintf("# Created : %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    "",
    "library(targets)",
    "library(tarchetypes)",
    "library(climasus4r)",
    "library(dplyr)",
    "",
    sprintf("options(clustermq.scheduler = %s)", scheduler),
    ""
  )

  params_block <- c(
    "list(",
    sprintf("  tar_target(params_uf,        %s),", deparse(ip$uf    %||% "SP")),
    sprintf("  tar_target(params_years,     %s),", deparse(ip$years %||% 2020L)),
    sprintf("  tar_target(params_system,    \"%s\"),", ip$system    %||% "SIM-DO"),
    sprintf("  tar_target(params_time_unit, \"%s\"),", op$time_unit %||% "month"),
    "  tar_target(params_lang,       \"pt\"),"
  )

  branching_targets <- switch(
    branching,
    "uf"    = c(
      "  tar_target(branch_uf,   params_uf,   pattern = map(params_uf)),"
    ),
    "year"  = c(
      "  tar_target(branch_year, params_years, pattern = map(params_years)),"
    ),
    "cross" = c(
      "  tar_target(analysis_grid, tidyr::crossing(uf = params_uf, year = params_years)),"
    ),
    character(0L)
  )

  uf_arg   <- if (branching == "uf")    "uf = branch_uf,"   else "uf = params_uf,"
  year_arg <- if (branching == "year")  "year = branch_year," else "year = params_years,"
  pattern_arg <- switch(
    branching,
    "none"  = "",
    "uf"    = ",\n    pattern = map(branch_uf)",
    "year"  = ",\n    pattern = map(branch_year)",
    "cross" = ",\n    pattern = map(analysis_grid)"
  )

  pipeline_name <- gsub("[^a-zA-Z0-9_]", "_", tolower(structure$type))
  if (nchar(pipeline_name) == 0L) pipeline_name <- "climasus_data"

  import_target <- c(
    sprintf("  tar_target(%s,", pipeline_name),
    "    climasus4r::sus_data_import(",
    sprintf("      %s", uf_arg),
    sprintf("      %s", year_arg),
    "      system   = params_system,",
    "      parallel = TRUE,",
    "      lang     = params_lang",
    sprintf("    )%s", pattern_arg),
    "  ),"
  )

  qc_target <- if (include_qc) {
    c(
      sprintf("  tar_target(%s_qc,", pipeline_name),
      sprintf("    climasus4r::sus_data_quality_report(%s)", pipeline_name),
      "  ),"
    )
  } else character(0L)

  closing <- c("  NULL", ")")

  content <- c(header,
               params_block,
               branching_targets,
               import_target,
               qc_target,
               closing)

  content
}
