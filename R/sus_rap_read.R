# =============================================================================
# sus_rap_read.R  - Read, inspect, run and update exported RAP artefacts.
# Companion to sus_rap_export(). Defines the rap_object S3 class.
#
# Exported: sus_rap_read(), sus_rap_inspect(), sus_rap_run(), sus_rap_update()
# S3 methods: print.rap_object(), summary.rap_object(), [.rap_object
# =============================================================================

utils::globalVariables(character(0L))

# -- Local labels --------------------------------------------------------------
.rap_read_labels <- list(
  reading          = list(pt = "Lendo RAP: {path}",
                          en = "Reading RAP: {path}",
                          es = "Leyendo RAP: {path}"),
  format_detected  = list(pt = "   Formato detectado: {fmt}",
                          en = "   Format detected: {fmt}",
                          es = "   Formato detectado: {fmt}"),
  read_ok          = list(pt = "Leitura OK | {n} etapas | uf={uf} | years={yrs}",
                          en = "Read OK | {n} steps | uf={uf} | years={yrs}",
                          es = "Lectura OK | {n} etapas | uf={uf} | years={yrs}"),
  params_overridden = list(pt = "Parametros substituidos: {keys}",
                           en = "Params overridden: {keys}",
                           es = "Parametros reemplazados: {keys}"),
  dry_run_header   = list(pt = "-- [dry_run] Codigo que seria executado --",
                          en = "-- [dry_run] Code that would be executed --",
                          es = "-- [dry_run] Codigo que seria ejecutado --"),
  running          = list(pt = "Executando | uf={uf} | years={yrs}",
                          en = "Running | uf={uf} | years={yrs}",
                          es = "Ejecutando | uf={uf} | years={yrs}"),
  run_ok           = list(pt = "Concluido | {nrow} linhas x {ncol} colunas | {elapsed}s",
                          en = "Done | {nrow} rows x {ncol} cols | {elapsed}s",
                          es = "Completado | {nrow} filas x {ncol} columnas | {elapsed}s"),
  no_patches       = list(pt = "Nenhum parametro para atualizar.",
                          en = "No parameters to update.",
                          es = "Ningun parametro para actualizar."),
  backup_created   = list(pt = "Backup criado: {path}",
                          en = "Backup created: {path}",
                          es = "Copia de seguridad creada: {path}"),
  update_ok        = list(pt = "Arquivo atualizado: {path}",
                          en = "File updated: {path}",
                          es = "Archivo actualizado: {path}")
)

#' @keywords internal
#' @noRd
.rrpl <- function(key, lang, ...) {
  e   <- .rap_read_labels[[key]]
  if (is.null(e)) return(key)
  txt <- e[[lang]] %||% e[["pt"]]
  if (...length() > 0L) glue::glue(txt, .envir = rlang::env(...)) else txt
}


# =============================================================================
# sus_rap_read()  - Read a RAP file into an R object
# =============================================================================

#' Read an Exported RAP File
#'
#' Reads a file produced by [sus_rap_export()] and reconstructs a live
#' `rap_object` containing session metadata, configurable parameters, and the
#' pipeline step list. The result can be inspected with [sus_rap_inspect()],
#' re-executed with [sus_rap_run()], or patched with [sus_rap_update()].
#'
#' @param file_path `character(1)`  - Path to an `.R`, `.Rmd`, or `.qmd` file
#'   exported by [sus_rap_export()].
#' @param lang `character(1)`  - Language for messages: `"pt"` (default),
#'   `"en"`, or `"es"`.
#' @param validate `logical(1)`  - Check structural integrity. Default `TRUE`.
#'
#' @return A `rap_object` list with fields `$metadata`, `$params`, `$steps`,
#'   `$structure`, `$source`, `$format`, and `$raw`.
#'
#' @export
#' @importFrom cli cli_alert_info cli_alert_warning cli_alert_success cli_abort
#' @importFrom rlang %||%
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' rap <- sus_rap_read("pipeline_sp.R")
#' print(rap)
#' }
sus_rap_read <- function(file_path, lang = "pt", validate = TRUE) {
  lang <- match.arg(lang, c("pt", "en", "es"))

  if (!is.character(file_path) || length(file_path) != 1L || nchar(file_path) == 0L)
    cli::cli_abort("`file_path` deve ser uma string nao vazia.")
  if (!file.exists(file_path))
    cli::cli_abort("Arquivo nao encontrado: {.path {file_path}}")

  cli::cli_alert_info(.rrpl("reading", lang, path = file_path))

  raw <- tryCatch(
    readLines(file_path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) cli::cli_abort("Falha ao ler o arquivo: {e$message}")
  )

  fmt <- .rap_detect_format(raw, file_path)
  cli::cli_alert_info(.rrpl("format_detected", lang, fmt = fmt))

  metadata <- tryCatch(
    .rap_parse_metadata(raw, fmt),
    error = function(e) { cli::cli_alert_warning("Metadados parciais: {e$message}"); list() }
  )
  params <- tryCatch(
    .rap_parse_params(raw, fmt),
    error = function(e) { cli::cli_alert_warning("Parametros parciais: {e$message}"); list() }
  )
  steps <- tryCatch(
    .rap_parse_steps(raw),
    error = function(e) { cli::cli_alert_warning("Etapas parciais: {e$message}"); list() }
  )

  structure_obj <- list(
    steps          = steps,
    type           = metadata$pipeline_type %||% "Pipeline Generico",
    input_params   = list(uf = params$uf, years = params$years, system = params$system),
    output_params  = list(time_unit = params$time_unit, group_by = params$group_by),
    total_steps    = length(steps),
    functions_used = unique(vapply(steps, function(s) s$function_name %||% "", character(1L)))
  )

  rap_obj <- structure(
    list(
      metadata  = metadata,
      params    = params,
      steps     = steps,
      structure = structure_obj,
      source    = normalizePath(file_path, mustWork = FALSE),
      format    = fmt,
      raw       = raw
    ),
    class = c("rap_object", "list")
  )

  if (validate) {
    issues <- .rap_check_integrity(rap_obj)
    if (length(issues) > 0L)
      cli::cli_alert_warning("Problemas de validacao:\n  - {paste(issues, collapse = '\n  - ')}")
  }

  cli::cli_alert_success(.rrpl("read_ok", lang,
                                n   = length(steps),
                                uf  = paste(params$uf    %||% "?", collapse = ", "),
                                yrs = paste(params$years %||% "?", collapse = ":")))
  rap_obj
}


# =============================================================================
# sus_rap_inspect()  - Summarise or diff two RAPs
# =============================================================================

#' Inspect or Diff rap_object(s)
#'
#' Prints a structured summary of a `rap_object`. When two objects are
#' supplied, performs a side-by-side diff showing changed parameters, added or
#' removed pipeline steps, and package version drift.
#'
#' @param rap A `rap_object` returned by [sus_rap_read()].
#' @param rap2 A second `rap_object` for comparison, or `NULL` (default) for
#'   single-object summary only.
#' @param verbose `logical(1)`  - Show detailed step list. Default `TRUE`.
#' @param lang `character(1)`  - Language for messages.
#'
#' @return Invisibly, a list with `$summary` (and `$diff` when `rap2` is given).
#' @export
#'
#' @examples
#' \dontrun{
#' rap <- sus_rap_read("pipeline_sp.R")
#' sus_rap_inspect(rap)
#'
#' rap_v2 <- sus_rap_read("pipeline_sp_v2.R")
#' sus_rap_inspect(rap, rap_v2)
#' }
sus_rap_inspect <- function(rap, rap2 = NULL, verbose = TRUE, lang = "pt") {
  .rap_assert_rap_object(rap, lang)
  cat("\n")

  .rap_print_header("rap_object summary")
  .rap_kv("Source",    rap$source)
  .rap_kv("Format",    rap$format %||% "?")
  .rap_kv("Type",      rap$structure$type %||% "?")
  .rap_kv("R version", rap$metadata$r_version %||% "?")
  .rap_kv("Package",   paste0("climasus4r ", rap$metadata$package_version %||% "?"))
  .rap_kv("Created",   rap$metadata$created %||% "?")
  .rap_kv("Platform",  rap$metadata$platform %||% "?")
  .rap_kv("Seed",      rap$metadata$random_seed %||% rap$params$seed %||% "?")

  .rap_print_sub("Params")
  .rap_kv("uf",        paste(rap$params$uf    %||% "?", collapse = ", "))
  .rap_kv("years",     paste(rap$params$years %||% "?", collapse = ":"))
  .rap_kv("system",    rap$params$system    %||% "?")
  .rap_kv("time_unit", rap$params$time_unit %||% "?")
  .rap_kv("lang",      rap$params$lang      %||% "?")

  if (verbose && length(rap$steps) > 0L) {
    .rap_print_sub(sprintf("Pipeline steps (%d)", length(rap$steps)))
    for (i in seq_along(rap$steps)) {
      s    <- rap$steps[[i]]
      prms <- if (length(s$important_params) > 0L) {
        paste(vapply(names(s$important_params), function(k)
                sprintf("%s=%s", k, deparse(s$important_params[[k]])), character(1L)),
              collapse = ", ")
      } else ""
      cat(sprintf("  %2d. %-40s %s\n", i, s$function_name %||% "?", prms))
    }
  }

  result <- list(summary = .rap_to_summary_list(rap))

  if (!is.null(rap2)) {
    .rap_assert_rap_object(rap2, lang)
    .rap_print_header("Diff: RAP 1 vs RAP 2")
    diff_result <- .rap_diff(rap, rap2)
    result$diff <- diff_result

    if (length(diff_result$params_changed) > 0L) {
      .rap_print_sub("Params changed")
      for (d in diff_result$params_changed)
        cat(sprintf("  %-12s  %s  ->  %s\n", d$key,
                    .rap_fmt_val(d$before), .rap_fmt_val(d$after)))
    } else {
      cat("  Params identical\n")
    }
    if (length(diff_result$steps_added)   > 0L) {
      .rap_print_sub("Steps added in RAP 2")
      for (s in diff_result$steps_added)   cat(sprintf("  + %s\n", s))
    }
    if (length(diff_result$steps_removed) > 0L) {
      .rap_print_sub("Steps removed in RAP 2")
      for (s in diff_result$steps_removed) cat(sprintf("  - %s\n", s))
    }
    if (length(diff_result$version_drift) > 0L) {
      .rap_print_sub("Package version drift")
      for (d in diff_result$version_drift)
        cat(sprintf("  %-20s  %s  ->  %s\n", d$pkg, d$v1 %||% "?", d$v2 %||% "?"))
    }
    if (all(lengths(diff_result) == 0L)) cat("  Pipelines are identical\n")
  }

  cat("\n")
  invisible(result)
}


# =============================================================================
# sus_rap_run()  - Re-execute a RAP with optional parameter overrides
# =============================================================================

#' Re-execute an Exported RAP
#'
#' Reconstructs and executes the pipeline contained in a `rap_object`. Any
#' parameters passed via `...` override the originals before execution, making
#' it easy to replay an analysis for a different state or year range.
#'
#' @param rap A `rap_object` returned by [sus_rap_read()].
#' @param ... Name-value pairs overriding fields in `rap$params` (e.g.
#'   `uf = "RJ"`, `years = 2021:2023`).
#' @param envir `environment`  - Execution environment. Default `parent.frame()`.
#' @param dry_run `logical(1)`  - Print the reconstructed code without executing.
#'   Default `FALSE`.
#' @param lang `character(1)`  - Language for messages.
#'
#' @return Invisibly, the `data.frame` produced by the pipeline.
#' @export
#'
#' @importFrom utils modifyList
#' @importFrom cli cli_alert_info cli_alert_success cli_abort
#' @importFrom rlang %||%
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' rap <- sus_rap_read("pipeline_sp.R")
#' df  <- sus_rap_run(rap, uf = "RJ", years = 2021:2023)
#' sus_rap_run(rap, dry_run = TRUE)
#' }
sus_rap_run <- function(rap, ..., envir = parent.frame(),
                        dry_run = FALSE, lang = "pt") {
  .rap_assert_rap_object(rap, lang)
  lang <- match.arg(lang, c("pt", "en", "es"))

  overrides <- list(...)
  params    <- modifyList(rap$params, overrides)

  if (length(overrides) > 0L)
    cli::cli_alert_info(.rrpl("params_overridden", lang,
                               keys = paste(names(overrides), collapse = ", ")))

  pkgs_needed  <- c("climasus4r", "dplyr", "glue")
  missing_pkgs <- pkgs_needed[!vapply(pkgs_needed, requireNamespace,
                                      quietly = TRUE, FUN.VALUE = logical(1L))]
  if (length(missing_pkgs) > 0L)
    cli::cli_abort("Pacotes ausentes: {paste(missing_pkgs, collapse = ', ')}")

  pipeline_call <- tryCatch(
    .rap_rebuild_call(rap$steps, params),
    error = function(e) cli::cli_abort("Falha ao reconstruir pipeline: {e$message}")
  )

  if (dry_run) {
    cli::cli_alert_info(.rrpl("dry_run_header", lang))
    cat(paste(pipeline_call, collapse = "\n"), "\n")
    return(invisible(NULL))
  }

  seed <- params$seed %||% rap$metadata$random_seed %||% 42L
  set.seed(as.integer(seed))

  cli::cli_alert_info(.rrpl("running", lang,
                             uf  = paste(params$uf    %||% "?", collapse = ", "),
                             yrs = paste(params$years %||% "?", collapse = ":")))
  t0 <- proc.time()

  result <- tryCatch(
    eval(parse(text = pipeline_call), envir = envir),
    error = function(e) cli::cli_abort("Falha na execucao: {e$message}")
  )

  elapsed <- round((proc.time() - t0)[["elapsed"]], 1L)

  if (!is.data.frame(result)) {
    cli::cli_alert_warning("O resultado nao e um data.frame.")
  } else {
    cli::cli_alert_success(.rrpl("run_ok", lang,
                                  nrow = nrow(result), ncol = ncol(result),
                                  elapsed = elapsed))
  }

  invisible(result)
}


# =============================================================================
# sus_rap_update()  - Patch parameters in the exported file on disk
# =============================================================================

#' Update Parameters in an Exported RAP File
#'
#' Applies targeted patches to the editable `params <- list(...)` block of an
#' exported RAP file without re-exporting from scratch. A `.bak` backup is
#' created before any modification.
#'
#' @param rap A `rap_object` returned by [sus_rap_read()].
#' @param ... Named parameters to update (e.g. `uf = "\"RJ\""`,
#'   `years = "2021:2024"`). Values are treated as raw R code strings.
#' @param backup `logical(1)`  - Create a `.bak` backup before writing.
#'   Default `TRUE`.
#' @param lang `character(1)`  - Language for messages.
#'
#' @return Invisibly, the updated `rap_object` (re-read from the patched file).
#' @export
#'
#' @examples
#' \dontrun{
#' rap    <- sus_rap_read("pipeline_sp.R")
#' rap_rj <- sus_rap_update(rap, uf = '"RJ"', years = "2021:2024")
#' }
sus_rap_update <- function(rap, ..., backup = TRUE, lang = "pt") {
  .rap_assert_rap_object(rap, lang)
  lang    <- match.arg(lang, c("pt", "en", "es"))
  patches <- list(...)

  if (length(patches) == 0L) {
    cli::cli_alert_info(.rrpl("no_patches", lang))
    return(invisible(rap))
  }

  file_path <- rap$source
  if (!file.exists(file_path))
    cli::cli_abort("Arquivo nao encontrado: {.path {file_path}}")

  if (backup) {
    bak <- paste0(file_path, ".bak")
    file.copy(file_path, bak, overwrite = TRUE)
    cli::cli_alert_info(.rrpl("backup_created", lang, path = bak))
  }

  lines        <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  changed_keys <- character(0L)

  for (key in names(patches)) {
    new_val <- patches[[key]]
    pat     <- sprintf("^(\\s*%s\\s*=\\s*)(.+?)(,?\\s*)$", key)
    idx     <- grep(pat, lines, perl = TRUE)

    if (length(idx) > 0L) {
      lines[idx]   <- sub(pat, sprintf("\\1%s\\3", new_val), lines[idx], perl = TRUE)
      changed_keys <- c(changed_keys, key)
      cli::cli_alert_info("  Parametro atualizado: {key} = {new_val}")
    } else {
      cli::cli_alert_warning("Parametro '{key}' nao encontrado no arquivo; ignorado.")
    }
  }

  if (length(changed_keys) > 0L) {
    writeLines(lines, file_path, useBytes = FALSE)
    cli::cli_alert_success(.rrpl("update_ok", lang, path = file_path))
  }

  invisible(sus_rap_read(file_path, lang = lang, validate = FALSE))
}


# =============================================================================
# S3 methods for rap_object
# =============================================================================

#' @export
print.rap_object <- function(x, ...) {
  cat(sprintf(
    "<rap_object>\n  source : %s\n  format : %s\n  type   : %s\n  steps  : %d\n  uf     : %s\n  years  : %s\n",
    x$source %||% "?",
    x$format %||% "?",
    x$structure$type %||% "?",
    length(x$steps),
    paste(x$params$uf    %||% "?", collapse = ", "),
    paste(x$params$years %||% "?", collapse = ":")
  ))
  invisible(x)
}

#' @export
summary.rap_object <- function(object, ...) sus_rap_inspect(object, ...)

#' @export
`[.rap_object` <- function(x, i) x[[i]]


# =============================================================================
# Internal parsing helpers
# =============================================================================

#' @keywords internal
#' @noRd
.rap_detect_format <- function(raw, file_path) {
  ext <- tolower(tools::file_ext(file_path))
  if (ext %in% c("rmd", "rmarkdown")) return("rmarkdown")
  if (ext == "qmd")                    return("quarto")
  first_100 <- paste(head(raw, 100L), collapse = "\n")
  if (grepl("^---", first_100) && grepl("quarto|format:", first_100)) return("quarto")
  if (grepl("^---", first_100) && grepl("output:", first_100))         return("rmarkdown")
  "script"
}

#' @keywords internal
#' @noRd
.rap_parse_metadata <- function(raw, fmt) {
  meta <- list()

  meta_start <- grep("^#\\s*---\\s*RAP METADATA", raw)
  meta_end   <- if (length(meta_start) > 0L)
    grep("^#\\s*---\\s*$", raw[seq(meta_start[1L] + 1L, length(raw))])[1L] +
    meta_start[1L] else 0L

  if (length(meta_start) > 0L && meta_end > meta_start[1L]) {
    yaml_lines <- raw[seq(meta_start[1L] + 1L, meta_end - 1L)]
    yaml_text  <- paste(sub("^#\\s?", "", yaml_lines), collapse = "\n")
    meta <- tryCatch(
      if (requireNamespace("yaml", quietly = TRUE))
        yaml::yaml.load(yaml_text) else list(),
      error = function(e) list()
    )
  }

  .xhf <- function(pat) {
    m <- grep(pat, raw, value = TRUE)
    if (length(m) == 0L) return(NULL)
    trimws(sub(pat, "\\1", m[1L]))
  }

  meta$r_version       <- meta$r_version       %||% .xhf("^#.*Versao R\\s*:\\s*(.+)")
  meta$package_version <- meta$package_version %||% .xhf("^#.*climasus4r\\s+(\\S+)")
  meta$created         <- meta$created         %||% .xhf("^#.*Data\\s*:\\s*(.+)")
  meta$platform        <- meta$platform        %||% .xhf("^#.*Plataforma\\s*:\\s*(.+)")
  meta$random_seed     <- meta$random_seed     %||% {
    s <- .xhf("set\\.seed\\((\\d+)")
    if (!is.null(s)) as.integer(s) else 42L
  }

  meta
}

#' @keywords internal
#' @noRd
.rap_parse_params <- function(raw, fmt) {
  params <- list()
  start  <- grep("^\\s*params\\s*<-\\s*list\\s*\\(", raw)
  if (length(start) == 0L) return(params)

  end <- start[1L]; depth <- 0L
  for (i in seq(start[1L], min(start[1L] + 50L, length(raw)))) {
    depth <- depth + nchar(gsub("[^(]", "", raw[i])) -
                     nchar(gsub("[^)]", "", raw[i]))
    if (depth <= 0L && i > start[1L]) { end <- i; break }
  }

  block_text <- paste(raw[seq(start[1L], end)], collapse = "\n")

  .safe_extract <- function(key, text) {
    pat <- sprintf("%s\\s*=\\s*([^,\n]+)", key)
    m   <- regmatches(text, regexpr(pat, text, perl = TRUE))
    if (length(m) == 0L) return(NULL)
    val_str <- trimws(sub(sprintf("%s\\s*=\\s*", key), "", m))
    tryCatch(eval(parse(text = val_str)), error = function(e) val_str)
  }

  params$uf         <- .safe_extract("uf",         block_text)
  params$years      <- .safe_extract("years",      block_text)
  params$system     <- .safe_extract("system",     block_text)
  params$time_unit  <- .safe_extract("time_unit",  block_text)
  params$lang       <- .safe_extract("lang",       block_text)
  params$seed       <- .safe_extract("seed",       block_text)
  params$output_dir <- .safe_extract("output_dir", block_text)

  Filter(Negate(is.null), params)
}

#' @keywords internal
#' @noRd
.rap_parse_steps <- function(raw) {
  sus_funcs <- c(
    "sus_data_import", "sus_data_clean_encoding", "sus_data_standardize",
    "sus_data_filter_cid", "sus_data_filter_demographics",
    "sus_data_create_variables", "sus_data_aggregate", "sus_data_export",
    "sus_data_quality_report"
  )
  pattern <- paste(sus_funcs, collapse = "|")
  idx     <- grep(pattern, raw)

  lapply(idx, function(i) {
    line <- trimws(raw[i])
    fn   <- regmatches(line, regexpr(pattern, line))
    fn   <- if (length(fn) > 0L) fn[1L] else "unknown"
    params_raw <- sub(sprintf(".*%s\\s*\\(", fn), "", line)
    params_raw <- sub("\\).*$", "", params_raw)
    list(
      function_name    = fn,
      important_params = .rap_parse_inline_params(fn, params_raw),
      line             = line
    )
  })
}

#' @keywords internal
#' @noRd
.rap_parse_inline_params <- function(fn, params_str) {
  keys_map <- list(
    sus_data_import              = c("uf", "year", "years", "system"),
    sus_data_filter_cid          = c("cid_group", "disease_group"),
    sus_data_filter_demographics = c("age_range"),
    sus_data_aggregate           = c("time_unit", "by", "group_by"),
    sus_data_create_variables    = c("create_age_groups", "create_calendar_vars")
  )
  keys   <- keys_map[[fn]] %||% character(0L)
  result <- list()
  for (k in keys) {
    pat <- sprintf("%s\\s*=\\s*([\"'][^\"']+[\"']|[^,]+)", k)
    m   <- regmatches(params_str, regexpr(pat, params_str, perl = TRUE))
    if (length(m) > 0L) {
      val_str  <- trimws(sub(sprintf("%s\\s*=\\s*", k), "", m))
      result[[k]] <- tryCatch(eval(parse(text = val_str)), error = function(e) val_str)
    }
  }
  result
}

#' @keywords internal
#' @noRd
.rap_rebuild_call <- function(steps, params) {
  if (length(steps) == 0L) cli::cli_abort("Nenhuma etapa para reconstruir.")

  uf_s  <- deparse(params$uf    %||% "NULL")
  yr_s  <- deparse(params$years %||% "NULL")
  sys_s <- deparse(params$system %||% "SIM-DO")
  lg_s  <- sprintf('"%s"', params$lang %||% "pt")

  call_lines <- sprintf(
    "sus_data_import(uf = %s, year = %s, system = %s, lang = %s, parallel = TRUE)",
    uf_s, yr_s, sys_s, lg_s
  )

  for (s in steps[-1L]) {
    fn   <- s$function_name
    pars <- s$important_params
    if (length(pars) > 0L) {
      par_str <- paste(
        vapply(names(pars), function(k) sprintf("%s = %s", k, deparse(pars[[k]])),
               character(1L)),
        collapse = ", "
      )
      call_lines <- c(call_lines, sprintf('|>\n  %s(%s)', fn, par_str))
    } else {
      call_lines <- c(call_lines, sprintf('|>\n  %s(lang = "%s")',
                                          fn, params$lang %||% "pt"))
    }
  }
  paste(call_lines, collapse = " ")
}

#' @keywords internal
#' @noRd
.rap_check_integrity <- function(rap_obj) {
  issues <- character(0L)
  if (length(rap_obj$steps) == 0L)
    issues <- c(issues, "Nenhuma etapa de pipeline detectada no arquivo.")
  if (is.null(rap_obj$params$uf))
    issues <- c(issues, "Parametro 'uf' nao encontrado.")
  if (is.null(rap_obj$params$years))
    issues <- c(issues, "Parametro 'years' nao encontrado.")
  issues
}

#' @keywords internal
#' @noRd
.rap_diff <- function(r1, r2) {
  all_keys    <- union(names(r1$params), names(r2$params))
  params_diff <- Filter(Negate(is.null), lapply(all_keys, function(k) {
    v1 <- r1$params[[k]]; v2 <- r2$params[[k]]
    if (!identical(v1, v2)) list(key = k, before = v1, after = v2) else NULL
  }))
  fn1     <- vapply(r1$steps, function(s) s$function_name %||% "", character(1L))
  fn2     <- vapply(r2$steps, function(s) s$function_name %||% "", character(1L))
  p1      <- r1$metadata$packages %||% list()
  p2      <- r2$metadata$packages %||% list()
  all_pkgs <- union(names(p1), names(p2))
  version_drift <- Filter(Negate(is.null), lapply(all_pkgs, function(p) {
    v1 <- p1[[p]]; v2 <- p2[[p]]
    if (!identical(v1, v2)) list(pkg = p, v1 = v1, v2 = v2) else NULL
  }))
  list(
    params_changed = params_diff,
    steps_added    = setdiff(fn2, fn1),
    steps_removed  = setdiff(fn1, fn2),
    version_drift  = version_drift
  )
}

#' @keywords internal
#' @noRd
.rap_assert_rap_object <- function(x, lang = "pt") {
  if (!inherits(x, "rap_object"))
    cli::cli_abort("Argumento deve ser um {.cls rap_object} (use {.fn sus_rap_read}).")
}

#' @keywords internal
#' @noRd
.rap_to_summary_list <- function(rap) {
  list(source = rap$source, format = rap$format, type = rap$structure$type,
       params = rap$params, n_steps = length(rap$steps), metadata = rap$metadata)
}

# -- Print helpers -------------------------------------------------------------
.rap_print_header <- function(title)
  cat(sprintf("-- %s %s\n", title, paste(rep("-", max(0L, 50L - nchar(title))), collapse = "")))
.rap_print_sub    <- function(title) cat(sprintf("\n  %s:\n", title))
.rap_kv           <- function(k, v)  cat(sprintf("  %-14s %s\n", paste0(k, ":"), v))
.rap_fmt_val      <- function(x) if (is.null(x)) "NULL" else paste(x, collapse = ", ")
