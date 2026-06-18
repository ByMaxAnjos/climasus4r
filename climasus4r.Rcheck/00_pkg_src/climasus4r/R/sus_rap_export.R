# =============================================================================
# sus_rap_export.R — Export climasus4r pipelines as Reproducible Analytical
# Pipelines (RAP) in multiple formats: R script, RMarkdown, Quarto, function.
# =============================================================================

# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(character(0L))

# ── Local labels ──────────────────────────────────────────────────────────────
.rap_export_labels <- list(
  session_start  = list(pt = "Iniciando exportacao RAP | {r_ver}",
                        en = "Starting RAP export | {r_ver}",
                        es = "Iniciando exportacion RAP | {r_ver}"),
  steps_found    = list(pt = "{n} etapa(s) detectadas no pipeline.",
                        en = "{n} step(s) detected in pipeline.",
                        es = "{n} etapa(s) detectadas en el pipeline."),
  generating     = list(pt = "Gerando conteudo no formato '{fmt}'...",
                        en = "Generating content in format '{fmt}'...",
                        es = "Generando contenido en formato '{fmt}'..."),
  dir_created    = list(pt = "Diretorio criado: {path}",
                        en = "Directory created: {path}",
                        es = "Directorio creado: {path}"),
  export_ok      = list(pt = "Pipeline exportado com sucesso: {path}",
                        en = "Pipeline successfully exported: {path}",
                        es = "Pipeline exportado con exito: {path}"),
  pipeline_null  = list(pt = "Pipeline nao fornecido; tentando capturar do contexto.",
                        en = "No pipeline provided; attempting context capture.",
                        es = "Pipeline no proporcionado; intentando captura de contexto."),
  missing_params = list(pt = "Parametros de entrada (uf/years/system) nao detectados.",
                        en = "Input parameters (uf/years/system) not detected.",
                        es = "Parametros de entrada (uf/years/system) no detectados.")
)

#' @keywords internal
#' @noRd
.repl <- function(key, lang, ...) {
  e  <- .rap_export_labels[[key]]
  if (is.null(e)) return(key)
  txt <- e[[lang]] %||% e[["pt"]]
  if (...length() > 0L) glue::glue(txt, .envir = rlang::env(...)) else txt
}


# ── Exported function ─────────────────────────────────────────────────────────

#' Export a climasus4r Pipeline as a Reproducible Analytical Pipeline (RAP)
#'
#' Converts a `climasus4r` pipeline expression into a portable, self-contained
#' artefact — an R script (`.R`), RMarkdown document (`.Rmd`), Quarto document
#' (`.qmd`), or an encapsulated R function — suitable for sharing, archiving,
#' and re-execution. The exported file embeds session metadata (R version,
#' package versions, timestamp, platform), an editable parameters block, and
#' optional validation checks, following Reproducible Analytical Pipeline
#' principles.
#'
#' @section Formats:
#' \describe{
#'   \item{`"script"`}{Standalone `.R` script with header, params block,
#'     pipeline execution, validation, and `sessionInfo()`.}
#'   \item{`"rmarkdown"`}{`.Rmd` document with YAML params, setup chunk,
#'     pipeline, validation, and optional visualisation section.}
#'   \item{`"quarto"`}{`.qmd` document using Quarto YAML front matter.}
#'   \item{`"function"`}{Parametrised `.R` function with Roxygen2 header.}
#' }
#'
#' @param pipeline A pipeline expression (created with `quote()` or
#'   `rlang::quo()`) containing `sus_*` function calls connected by `|>` or
#'   `%>%`. Pass `NULL` to capture from the calling context.
#' @param file_path `character(1)` — Output file path. Extension informs format
#'   detection. Pass `NULL` to return content without writing to disk.
#' @param format `character(1)` — One of `"script"`, `"rmarkdown"`,
#'   `"quarto"`, or `"function"`. Inferred from `file_path` extension when
#'   possible.
#' @param include_metadata `logical(1)` — Embed R version, package versions,
#'   timestamp, and platform. Default `TRUE`.
#' @param include_validation `logical(1)` — Add a validation block (`stopifnot`
#'   + NA-rate check). Default `TRUE`.
#' @param include_documentation `character(1)` — Comment density in the
#'   exported code: `"minimal"`, `"standard"` (default), or `"comprehensive"`.
#' @param output_type `character(1)` — Purpose of the document:
#'   `"analysis"`, `"dashboard"`, or `"report"`. Affects visualisation
#'   sections in Rmd/Quarto outputs.
#' @param lang `character(1)` — Language for comments and messages: `"pt"`
#'   (default), `"en"`, or `"es"`.
#' @param overwrite `logical(1)` — Overwrite an existing file. Default `FALSE`.
#'
#' @return Invisibly, a `character` vector with the exported file content.
#'   Side-effect: writes to `file_path` when provided.
#'
#' @export
#'
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom rlang %||% is_quosure get_expr env
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' pipeline <- quote(
#'   sus_data_import(uf = "SP", years = 2020:2022, system = "SIM-DO") |>
#'     sus_data_clean_encoding() |>
#'     sus_data_filter_cid(cid_group = "respiratory") |>
#'     sus_data_aggregate(by = "month")
#' )
#'
#' # Export as R script
#' sus_rap_export(pipeline, "pipeline_respiratorio.R", lang = "pt")
#'
#' # Export as Quarto document
#' sus_rap_export(pipeline, "analise.qmd", format = "quarto",
#'                include_documentation = "comprehensive")
#' }
sus_rap_export <- function(
    pipeline              = NULL,
    file_path             = NULL,
    format                = c("script", "rmarkdown", "quarto", "function"),
    include_metadata      = TRUE,
    include_validation    = TRUE,
    include_documentation = c("standard", "minimal", "comprehensive"),
    output_type           = c("analysis", "dashboard", "report"),
    lang                  = "pt",
    overwrite             = FALSE
) {
  exec_meta <- .rap_collect_session_meta()

  # ── 1. Validate arguments ───────────────────────────────────────────────────
  format                <- match.arg(format)
  include_documentation <- match.arg(include_documentation)
  output_type           <- match.arg(output_type)
  lang                  <- match.arg(lang, c("pt", "en", "es"))

  if (!is.logical(include_metadata)   || length(include_metadata)   != 1L)
    cli::cli_abort("`include_metadata` deve ser TRUE ou FALSE.")
  if (!is.logical(include_validation) || length(include_validation) != 1L)
    cli::cli_abort("`include_validation` deve ser TRUE ou FALSE.")

  if (!is.null(file_path))
    .rap_validate_file_path(file_path, format, overwrite)

  cli::cli_alert_info(.repl("session_start", lang,
                             r_ver = exec_meta$r_version))

  # ── 2. Parse pipeline ───────────────────────────────────────────────────────
  if (is.null(pipeline)) {
    cli::cli_alert_warning(.repl("pipeline_null", lang))
    pipeline <- rlang::enquo(pipeline)
  }

  pipeline_structure <- tryCatch(
    .rap_extract_structure(pipeline),
    error = function(e) cli::cli_abort("Falha ao analisar o pipeline: {e$message}")
  )

  .rap_validate_structure(pipeline_structure, lang)
  cli::cli_alert_info(.repl("steps_found", lang,
                             n = pipeline_structure$total_steps))

  # ── 3. Build metadata ───────────────────────────────────────────────────────
  metadata <- if (include_metadata) {
    tryCatch(
      .rap_build_metadata(pipeline_structure, lang, exec_meta),
      error = function(e) { cli::cli_alert_warning("Metadados parciais: {e$message}"); NULL }
    )
  } else NULL

  # ── 4. Generate content ─────────────────────────────────────────────────────
  cli::cli_alert_info(.repl("generating", lang, fmt = format))

  content <- tryCatch(
    switch(
      format,
      "script"    = .rap_content_script(
                      pipeline_structure, metadata,
                      include_validation, include_documentation, lang, exec_meta),
      "rmarkdown" = .rap_content_rmarkdown(
                      pipeline_structure, metadata,
                      include_validation, include_documentation, lang,
                      output_type, exec_meta),
      "quarto"    = .rap_content_quarto(
                      pipeline_structure, metadata,
                      include_validation, include_documentation, lang,
                      output_type, exec_meta),
      "function"  = .rap_content_function(
                      pipeline_structure, metadata,
                      include_validation, include_documentation, lang)
    ),
    error = function(e) cli::cli_abort("Erro ao gerar conteudo: {e$message}")
  )

  if (!is.character(content) || length(content) == 0L)
    cli::cli_abort("O conteudo gerado esta vazio.")

  # ── 5. Write file ───────────────────────────────────────────────────────────
  if (!is.null(file_path)) {
    dir_path <- dirname(file_path)
    if (!dir.exists(dir_path)) {
      dir.create(dir_path, recursive = TRUE)
      cli::cli_alert_info(.repl("dir_created", lang, path = dir_path))
    }
    writeLines(content, file_path, useBytes = FALSE)
    cli::cli_alert_success(.repl("export_ok", lang, path = file_path))
  }

  invisible(content)
}


# =============================================================================
# Internal helpers — session & validation
# =============================================================================

#' @keywords internal
#' @noRd
.rap_collect_session_meta <- function() {
  pkgs <- c("climasus4r", "dplyr", "rlang", "glue", "ggplot2", "rmarkdown", "quarto")
  installed <- vapply(pkgs, function(p) {
    tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
  }, character(1L))

  list(
    r_version      = R.version.string,
    r_version_num  = paste(R.Version()$major, R.Version()$minor, sep = "."),
    platform       = .rap_detect_platform(),
    exec_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    exec_date      = format(Sys.Date(), "%Y-%m-%d"),
    packages       = installed[!is.na(installed)],
    tz             = Sys.timezone()
  )
}

#' @keywords internal
#' @noRd
.rap_detect_platform <- function() {
  os <- .Platform$OS.type
  if (os == "windows") return("windows")
  sys <- tryCatch(tolower(Sys.info()[["sysname"]]), error = function(e) "unix")
  if (grepl("darwin", sys)) return("macos")
  if (grepl("linux",  sys)) return("linux")
  os
}

#' @keywords internal
#' @noRd
.rap_validate_file_path <- function(file_path, format, overwrite) {
  if (!is.character(file_path) || length(file_path) != 1L || nchar(file_path) == 0L)
    cli::cli_abort("`file_path` deve ser uma string nao vazia.")

  ext_map <- c(script = ".R", rmarkdown = ".Rmd", quarto = ".qmd", "function" = ".R")
  expected <- ext_map[[format]]
  actual   <- tolower(tools::file_ext(file_path))
  if (!is.null(expected) &&
      !identical(paste0(".", actual), tolower(expected)))
    cli::cli_alert_warning("Extensao esperada '{expected}' difere de '{file_path}'.")

  if (file.exists(file_path) && !overwrite)
    cli::cli_abort("O arquivo '{file_path}' ja existe. Use {.arg overwrite = TRUE}.")
}

#' @keywords internal
#' @noRd
.rap_validate_structure <- function(structure, lang) {
  required <- c("steps", "type", "input_params", "output_params",
                "total_steps", "functions_used")
  missing_fields <- setdiff(required, names(structure))
  if (length(missing_fields) > 0L)
    cli::cli_abort("Estrutura do pipeline invalida. Campos ausentes: {missing_fields}")

  if (!is.list(structure$steps) || length(structure$steps) == 0L)
    cli::cli_abort("O pipeline nao contém etapas validas.")

  if (is.null(structure$input_params$uf)    ||
      is.null(structure$input_params$years) ||
      is.null(structure$input_params$system))
    cli::cli_alert_warning(.repl("missing_params", lang))
}


# =============================================================================
# Internal helpers — pipeline structure parsing
# =============================================================================

#' @keywords internal
#' @noRd
.rap_extract_structure <- function(pipeline_expr) {
  if (rlang::is_quosure(pipeline_expr))
    pipeline_expr <- rlang::get_expr(pipeline_expr)

  .pipe_ops <- c("%>%", "|>", "pipe")

  parse_pipe <- function(expr, acc) {
    if (!is.call(expr)) return(acc)
    op <- tryCatch(as.character(expr[[1L]]), error = function(e) "")

    if (op %in% .pipe_ops) {
      acc <- parse_pipe(expr[[2L]], acc)
      acc <- parse_pipe(expr[[3L]], acc)
    } else {
      fn   <- op
      args <- if (length(expr) > 1L) as.list(expr[-1L]) else list()
      prms <- tryCatch(.rap_extract_step_params(fn, args), error = function(e) list())
      acc  <- c(acc, list(list(
        function_name    = fn,
        arguments        = args,
        important_params = prms,
        line             = tryCatch(deparse(expr)[1L], error = function(e) "")
      )))
    }
    acc
  }

  steps <- parse_pipe(pipeline_expr, list())
  if (length(steps) == 0L) cli::cli_abort("Nenhuma etapa encontrada na expressao.")

  pipeline_type  <- tryCatch(.rap_identify_type(steps),  error = function(e) "Pipeline Generico")
  input_params   <- tryCatch(.rap_input_params(steps),   error = function(e) list())
  output_params  <- tryCatch(.rap_output_params(steps),  error = function(e) list())
  functions_used <- unique(vapply(steps, function(s) s$function_name, character(1L)))

  list(
    steps          = steps,
    type           = pipeline_type,
    input_params   = input_params,
    output_params  = output_params,
    total_steps    = length(steps),
    functions_used = functions_used
  )
}

.RAP_PARAM_MAP <- list(
  sus_data_import              = c("uf", "year", "years", "system"),
  sus_data_filter_cid          = c("cid_group", "disease_group"),
  sus_data_filter_demographics = c("age_range"),
  sus_data_aggregate           = c("time_unit", "by", "group_by"),
  sus_data_create_variables    = c("create_age_groups", "create_calendar_vars",
                                   "age_breaks", "age_labels")
)

#' @keywords internal
#' @noRd
.rap_extract_step_params <- function(fn, args) {
  keys    <- .RAP_PARAM_MAP[[fn]]
  if (is.null(keys) || length(args) == 0L) return(list())
  nms     <- names(args)
  result  <- list()
  for (k in keys) {
    idx <- which(nms == k)
    if (length(idx) == 1L)
      result[[k]] <- tryCatch(eval(args[[idx]]), error = function(e) args[[idx]])
  }
  result
}

#' @keywords internal
#' @noRd
.rap_identify_type <- function(steps) {
  if (length(steps) == 0L) return("Pipeline Generico")
  fns <- vapply(steps, function(s) s$function_name %||% "", character(1L))

  if (any(grepl("filter_cid", fns))) {
    cid_groups <- unique(vapply(steps, function(s) {
      s$important_params$cid_group %||% s$important_params$disease_group %||% ""
    }, character(1L)))
    cid_groups <- cid_groups[nchar(cid_groups) > 0L]
    if ("respiratory"    %in% cid_groups) return("Mortalidade por Doencas Respiratorias")
    if ("cardiovascular" %in% cid_groups) return("Mortalidade Cardiovascular")
    if ("heat_related"   %in% cid_groups) return("Mortalidade por Calor")
    if (length(cid_groups) > 0L) return(paste("Mortalidade por", paste(cid_groups, collapse = " e ")))
  }
  if (any(grepl("SINAN", fns, ignore.case = TRUE))) return("Morbidade (SINAN)")
  if (any(grepl("heat|calor", fns, ignore.case = TRUE))) return("Eventos Climaticos Extremos")
  "Pipeline Generico"
}

#' @keywords internal
#' @noRd
.rap_input_params <- function(steps) {
  imp <- Filter(function(s) s$function_name == "sus_data_import", steps)
  if (length(imp) == 0L) return(list(uf = NULL, years = NULL, system = NULL))
  s <- imp[[1L]]
  list(
    uf     = s$important_params$uf    %||% NULL,
    years  = s$important_params$years %||% s$important_params$year %||% NULL,
    system = s$important_params$system %||% NULL
  )
}

#' @keywords internal
#' @noRd
.rap_output_params <- function(steps) {
  agg <- Filter(function(s) s$function_name == "sus_data_aggregate", steps)
  if (length(agg) == 0L) return(list(time_unit = NULL, group_by = NULL))
  s <- agg[[1L]]
  list(
    time_unit = s$important_params$time_unit %||% s$important_params$by %||% NULL,
    group_by  = s$important_params$group_by  %||% NULL
  )
}

#' @keywords internal
#' @noRd
.rap_build_metadata <- function(structure, lang, exec_meta = list()) {
  desc <- if (lang == "pt") {
    glue::glue("Pipeline para analise de {structure$type} com dados do DATASUS")
  } else if (lang == "es") {
    glue::glue("Pipeline para analisis de {structure$type} con datos del DATASUS")
  } else {
    glue::glue("Pipeline for {structure$type} analysis using DATASUS data")
  }

  pkg_ver <- tryCatch(
    as.character(utils::packageVersion("climasus4r")),
    error = function(e) "?"
  )

  list(
    name            = paste("Pipeline", structure$type),
    description     = desc,
    version         = "1.0.0",
    created         = exec_meta$exec_timestamp %||% format(Sys.time()),
    created_date    = exec_meta$exec_date      %||% format(Sys.Date()),
    package_version = pkg_ver,
    r_version       = exec_meta$r_version      %||% R.version.string,
    platform        = exec_meta$platform       %||% .Platform$OS.type,
    packages        = exec_meta$packages       %||% list(),
    structure       = structure,
    random_seed     = 42L
  )
}

#' @keywords internal
#' @noRd
.rap_capture_pipeline_code <- function(structure) {
  ip <- structure$input_params

  code <- c(
    "df <- sus_data_import(",
    sprintf("  uf     = %s,",     deparse(ip$uf    %||% "NULL")),
    sprintf("  years  = %s,",     deparse(ip$years %||% "NULL")),
    sprintf("  system = \"%s\",", ip$system %||% "SIM-DO"),
    "  parallel = TRUE,",
    "  lang     = params$lang",
    ")"
  )

  for (step in structure$steps[-1L]) {
    fn       <- step$function_name
    arg_lines <- vapply(names(step$important_params), function(k) {
      sprintf("  %s = %s", k, deparse(step$important_params[[k]]))
    }, character(1L))

    if (length(arg_lines) > 0L) {
      code <- c(code, "|>", sprintf("  %s(", fn),
                paste(arg_lines, collapse = ",\n"), "  )")
    } else {
      code <- c(code, "|>", sprintf("  %s(lang = params$lang)", fn))
    }
  }

  c(code,
    "|>",
    "  sus_data_export(",
    "    file_path        = file.path(params$output_dir, \"resultado.csv\"),",
    "    format           = \"csv\",",
    "    include_metadata = TRUE,",
    "    lang             = params$lang",
    "  )")
}

#' @keywords internal
#' @noRd
.rap_pkg_check_block <- function(pkgs) {
  c(
    "required_pkgs <- c(",
    sprintf("  %s", paste(sprintf('"%s"', pkgs), collapse = ", ")),
    ")",
    "missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace,",
    "                                       quietly = TRUE, FUN.VALUE = logical(1L))]",
    "if (length(missing_pkgs) > 0L)",
    "  stop(paste(\"Pacotes ausentes:\", paste(missing_pkgs, collapse = \", \")), call. = FALSE)",
    ""
  )
}


# =============================================================================
# Content generators — one per format
# =============================================================================

#' @keywords internal
#' @noRd
.rap_content_script <- function(structure, metadata, include_validation,
                                 doc_level, lang, exec_meta = list()) {
  ip     <- structure$input_params
  op     <- structure$output_params
  seed   <- metadata$random_seed %||% 42L
  date_s <- exec_meta$exec_date %||% format(Sys.Date())
  r_ver  <- exec_meta$r_version %||% R.version.string
  pkgs   <- exec_meta$packages  %||% list()
  pkg_str <- if (length(pkgs) > 0L) {
    paste(sprintf("# %s: %s", names(pkgs), pkgs), collapse = "\n")
  } else "# (pacotes nao verificados)"

  header <- c(
    "#!/usr/bin/env Rscript",
    "# vim: set fileencoding=UTF-8:",
    "",
    "# ============================================================================",
    sprintf("# Pipeline Reprodutivel (RAP) - %s", metadata$name %||% "climasus4r"),
    "# Gerado por climasus4r::sus_rap_export()",
    "# ============================================================================",
    sprintf("# Versao R  : %s", r_ver),
    sprintf("# Data      : %s", date_s),
    sprintf("# Pacote    : climasus4r %s", metadata$package_version %||% "?"),
    sprintf("# Plataforma: %s", exec_meta$platform %||% "?"),
    "# Pacotes carregados:",
    pkg_str,
    "# ============================================================================",
    ""
  )

  setup <- c(
    "# -- 0. Reprodutibilidade ---------------------------------------------------",
    sprintf("set.seed(%dL)", seed),
    "",
    "# -- 1. Dependencias --------------------------------------------------------",
    .rap_pkg_check_block(c("climasus4r", "dplyr", "glue")),
    "library(climasus4r)",
    "library(dplyr)",
    "library(glue)",
    ""
  )

  params_block <- c(
    "# -- 2. PARAMS (EDITE AQUI) ------------------------------------------------",
    "# ## -- PARAMS --",
    "params <- list(",
    sprintf("  uf         = %s,",     deparse(ip$uf    %||% "NULL")),
    sprintf("  years      = %s,",     deparse(ip$years %||% "NULL")),
    sprintf("  system     = \"%s\",", ip$system %||% "SIM-DO"),
    sprintf("  time_unit  = \"%s\",", op$time_unit %||% "month"),
    "  output_dir = \"resultados\",",
    sprintf("  lang       = \"%s\",", lang),
    sprintf("  seed       = %dL", seed),
    ")",
    "# ## -- END PARAMS --",
    "",
    "if (!dir.exists(params$output_dir))",
    "  dir.create(params$output_dir, recursive = TRUE)",
    ""
  )

  pipeline_exec <- c(
    "# -- 3. Execucao do pipeline ------------------------------------------------",
    "message(\"Iniciando pipeline...\")",
    "start_time <- proc.time()",
    "",
    "df_resultado <- tryCatch({",
    "",
    paste0("  ", .rap_capture_pipeline_code(structure)),
    "",
    "}, error = function(e) {",
    "  stop(glue(\"Falha no pipeline: {conditionMessage(e)}\"), call. = FALSE)",
    "})",
    "",
    "elapsed <- round((proc.time() - start_time)[\"elapsed\"], 1)",
    "message(glue(\"Pipeline concluido em {elapsed}s\"))",
    ""
  )

  validation_block <- character(0L)
  if (include_validation) {
    validation_block <- c(
      "# -- 4. Validacao ----------------------------------------------------------",
      "stopifnot(",
      "  \"Resultado vazio\"            = nrow(df_resultado) > 0L,",
      "  \"Resultado nao e data.frame\" = is.data.frame(df_resultado)",
      ")",
      "na_pct <- round(100 * colMeans(is.na(df_resultado)), 1)",
      "if (any(na_pct > 50))",
      "  warning(\"Colunas com >50% de NAs: \",",
      "          paste(names(na_pct[na_pct > 50]), collapse = \", \"), call. = FALSE)",
      "message(glue(\"Validacao OK: {nrow(df_resultado)} linhas, {ncol(df_resultado)} colunas\"))",
      ""
    )
  }

  export_block <- c(
    "# -- 5. Exportacao ---------------------------------------------------------",
    "output_file <- file.path(",
    sprintf("  params$output_dir, paste0(\"resultado_%s.csv\")", date_s),
    ")",
    "sus_data_export(",
    "  df_resultado,",
    "  file_path        = output_file,",
    "  format           = \"csv\",",
    "  include_metadata = TRUE,",
    "  lang             = params$lang",
    ")",
    "message(glue(\"Resultados salvos em: {output_file}\"))",
    ""
  )

  session_block <- c(
    "# -- 6. Sessao -------------------------------------------------------------",
    "utils::sessionInfo()",
    ""
  )

  c(header, setup, params_block, pipeline_exec,
    validation_block, export_block, session_block)
}


#' @keywords internal
#' @noRd
.rap_content_rmarkdown <- function(structure, metadata, include_validation,
                                    doc_level, lang, output_type, exec_meta = list()) {
  ip     <- structure$input_params
  op     <- structure$output_params
  seed   <- metadata$random_seed %||% 42L
  name   <- metadata$name  %||% "Analise climasus4r"
  date_s <- exec_meta$exec_date %||% format(Sys.Date())

  yaml_hdr <- c(
    "---",
    sprintf("title: \"Pipeline Reprodutivel: %s\"", name),
    sprintf("author: \"%s\"", Sys.getenv("USER", "Analista")),
    sprintf("date: \"%s\"", date_s),
    "output:",
    "  html_document:",
    "    toc: true",
    "    toc_float: true",
    "    toc_depth: 4",
    "    number_sections: true",
    "    theme: flatly",
    "    code_folding: show",
    "params:",
    sprintf("  uf: \"%s\"",          paste(ip$uf    %||% "NULL", collapse = ", ")),
    sprintf("  years: \"%s\"",       paste(ip$years %||% "NULL", collapse = ":")),
    sprintf("  system: \"%s\"",      ip$system    %||% "SIM-DO"),
    sprintf("  time_unit: \"%s\"",   op$time_unit %||% "month"),
    "  output_dir: \"resultados\"",
    sprintf("  lang: \"%s\"",        lang),
    sprintf("  seed: %d", seed),
    "---", ""
  )

  setup_chunk <- c(
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)",
    sprintf("set.seed(%dL)", seed),
    .rap_pkg_check_block(c("climasus4r", "dplyr", "ggplot2", "glue")),
    "library(climasus4r); library(dplyr); library(ggplot2); library(glue)",
    "```", ""
  )

  intro <- c(
    "# Sobre",
    "",
    sprintf("> Gerado automaticamente em %s pelo pacote `climasus4r`.", date_s),
    sprintf("> R %s | climasus4r %s",
            exec_meta$r_version %||% R.version.string,
            metadata$package_version %||% "?"),
    "",
    metadata$description %||% "Pipeline reprodutivel do DATASUS.", ""
  )

  pipeline_section <- c(
    "# Execucao", "",
    "```{r pipeline}",
    "df_resultado <- tryCatch({",
    "",
    paste0("  ", .rap_capture_pipeline_code(structure)),
    "",
    "}, error = function(e) { knitr::knit_exit(); stop(conditionMessage(e)) })",
    "```", ""
  )

  validation_section <- character(0L)
  if (include_validation) {
    validation_section <- c(
      "# Validacao", "",
      "```{r validation}",
      "stopifnot(\"Resultado vazio\" = nrow(df_resultado) > 0L)",
      "na_pct <- round(100 * colMeans(is.na(df_resultado)), 1)",
      "knitr::kable(",
      "  data.frame(Coluna = names(na_pct), `NA (%)` = na_pct, check.names = FALSE),",
      "  caption = \"Completude das colunas\"",
      ")",
      "```", ""
    )
  }

  viz_section <- character(0L)
  if (output_type %in% c("dashboard", "report")) {
    viz_section <- c(
      "# Visualizacao", "",
      "```{r viz, fig.width=10, fig.height=5}",
      "if (all(c(\"data\", \"n\") %in% names(df_resultado))) {",
      "  ggplot(df_resultado, aes(x = data, y = n)) +",
      "    geom_line(colour = \"#2EC4B6\", linewidth = 0.9) +",
      "    geom_point(colour = \"#E63946\", size = 1.2) +",
      sprintf("    labs(title = \"Serie Temporal - %s\",", name),
      "         x = \"Data\", y = \"Contagem\") +",
      "    theme_minimal(base_size = 13)",
      "}",
      "```", ""
    )
  }

  session_section <- c(
    "# Sessao", "",
    "```{r session-info, echo=FALSE}",
    "utils::sessionInfo()",
    "```", ""
  )

  c(yaml_hdr, setup_chunk, intro, pipeline_section,
    validation_section, viz_section, session_section)
}


#' @keywords internal
#' @noRd
.rap_content_quarto <- function(structure, metadata, include_validation,
                                 doc_level, lang, output_type, exec_meta = list()) {
  ip     <- structure$input_params
  op     <- structure$output_params
  seed   <- metadata$random_seed %||% 42L
  name   <- metadata$name  %||% "Analise climasus4r"
  date_s <- exec_meta$exec_date %||% format(Sys.Date())

  yaml_hdr <- c(
    "---",
    sprintf("title: \"Pipeline Reprodutivel: %s\"", name),
    sprintf("author: \"%s\"", Sys.getenv("USER", "Analista")),
    sprintf("date: \"%s\"", date_s),
    "format:",
    "  html:",
    "    toc: true",
    "    toc-depth: 4",
    "    number-sections: true",
    "    theme: flatly",
    "    code-fold: true",
    "    code-tools: true",
    "execute:",
    "  echo: true",
    "  warning: false",
    "  message: false",
    "params:",
    sprintf("  uf: \"%s\"",          paste(ip$uf    %||% "NULL", collapse = ", ")),
    sprintf("  years: \"%s\"",       paste(ip$years %||% "NULL", collapse = ":")),
    sprintf("  system: \"%s\"",      ip$system    %||% "SIM-DO"),
    sprintf("  time_unit: \"%s\"",   op$time_unit %||% "month"),
    "  output_dir: \"resultados\"",
    sprintf("  lang: \"%s\"",        lang),
    sprintf("  seed: %d", seed),
    "---", ""
  )

  setup_chunk <- c(
    "```{r}",
    "#| label: setup",
    "#| include: false",
    sprintf("set.seed(%dL)", seed),
    .rap_pkg_check_block(c("climasus4r", "dplyr", "ggplot2", "glue")),
    "library(climasus4r); library(dplyr); library(ggplot2); library(glue)",
    "```", ""
  )

  intro <- c(
    "## Sobre", "",
    sprintf("> Gerado em **%s** | R `%s` | climasus4r `%s`",
            date_s,
            exec_meta$r_version %||% R.version.string,
            metadata$package_version %||% "?"),
    "",
    metadata$description %||% "Pipeline reprodutivel do DATASUS.", ""
  )

  pipeline_section <- c(
    "## Pipeline", "",
    "```{r}",
    "#| label: pipeline",
    "df_resultado <- tryCatch({",
    "",
    paste0("  ", .rap_capture_pipeline_code(structure)),
    "",
    "}, error = \\(e) stop(conditionMessage(e)))",
    "```", ""
  )

  session_section <- c(
    "## Sessao", "",
    "```{r}",
    "#| label: session-info",
    "#| echo: false",
    "utils::sessionInfo()",
    "```", ""
  )

  c(yaml_hdr, setup_chunk, intro, pipeline_section, session_section)
}


#' @keywords internal
#' @noRd
.rap_content_function <- function(structure, metadata, include_validation,
                                   doc_level, lang) {
  ip      <- structure$input_params
  seed    <- metadata$random_seed %||% 42L
  pkg_ver <- metadata$package_version %||% "?"
  r_ver   <- metadata$r_version       %||% R.version.string

  roxygen <- c(
    sprintf("# climasus4r %s | %s | %s",
            pkg_ver, r_ver, metadata$created_date %||% format(Sys.Date())),
    "",
    "#' @title Executar Pipeline Reprodutivel",
    sprintf("#' @description %s", metadata$description %||% "Pipeline gerado por climasus4r."),
    "#' @param uf `character` — UF(s) para analise.",
    "#' @param years `integer` — Anos a serem processados.",
    "#' @param output_dir `character` — Diretorio para salvar resultados.",
    "#' @param time_unit `character` — Unidade de agregacao temporal.",
    "#' @param lang `character` — Idioma (`\"pt\"`, `\"en\"`, ou `\"es\"`).",
    "#' @param seed `integer` — Semente aleatoria (padrao: 42).",
    "#' @param verbose `logical` — Exibir mensagens de progresso.",
    "#' @return `data.frame` com resultados processados (invisivel).",
    "#' @export",
    ""
  )

  body <- c(
    "run_pipeline <- function(",
    sprintf("    uf         = %s,", deparse(ip$uf    %||% "\"SP\"")),
    sprintf("    years      = %s,", deparse(ip$years %||% 2020L)),
    "    output_dir = \"resultados\",",
    sprintf("    time_unit  = \"%s\",", structure$output_params$time_unit %||% "month"),
    sprintf("    lang       = \"%s\",", lang),
    sprintf("    seed       = %dL,", seed),
    "    verbose    = TRUE",
    ") {",
    "",
    "  stopifnot(",
    "    \"uf deve ser character\"      = is.character(uf),",
    "    \"years deve ser numerico\"    = is.numeric(years) || is.integer(years),",
    "    \"output_dir deve ser string\" = is.character(output_dir)",
    "  )",
    "  set.seed(seed)",
    "",
    "  params <- list(",
    sprintf("    uf = uf, years = years, system = \"%s\",", ip$system %||% "SIM-DO"),
    "    output_dir = output_dir, time_unit = time_unit, lang = lang",
    "  )",
    "  if (!dir.exists(params$output_dir))",
    "    dir.create(params$output_dir, recursive = TRUE)",
    "",
    "  if (verbose) message(\"Executando pipeline...\")",
    "  start_time <- proc.time()",
    "",
    "  df_resultado <- tryCatch({",
    "",
    paste0("    ", .rap_capture_pipeline_code(structure)),
    "",
    "  }, error = function(e) {",
    "    stop(paste(\"Falha no pipeline:\", conditionMessage(e)), call. = FALSE)",
    "  })",
    "",
    if (include_validation) {
      c("  stopifnot(",
        "    \"Resultado vazio\" = is.data.frame(df_resultado) && nrow(df_resultado) > 0L",
        "  )", "")
    },
    "  elapsed <- round((proc.time() - start_time)[\"elapsed\"], 1)",
    "  if (verbose)",
    "    message(glue::glue(\"Concluido em {elapsed}s | {nrow(df_resultado)} linhas\"))",
    "",
    "  invisible(df_resultado)",
    "}"
  )

  c(roxygen, body)
}
