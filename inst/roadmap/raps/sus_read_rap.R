# ==============================================================================
# sus_read_rap.R — Leitura e Gestão de RAPs Exportados
# Pacote: climasus4r
# ==============================================================================
# Autor:       climasus4r Team
# Versão:      1.0.0
# Descrição:   Família de funções para ler, inspecionar, executar e atualizar
#              pipelines exportados pelo sus_export_raps().
#
# Fluxo de uso:
#   1. sus_export_raps(pipeline, "meu_pipeline.R")   # autor
#   2. rap <- sus_read_rap("meu_pipeline.R")          # consumidor
#   3. sus_inspect_rap(rap)                           # o que é isso?
#   4. sus_run_rap(rap, uf = "RJ", years = 2023)      # executar com novos params
#   5. sus_update_rap(rap, uf = "RJ")                 # patch no arquivo
#
# Dependências: rlang, glue, yaml (sugerida)
# ==============================================================================

# ── Classe S3 para rap_object ──────────────────────────────────────────────────
# Um rap_object é uma lista S3 com os campos:
#   $metadata   — lista de metadados de sessão e versão
#   $params     — lista de parâmetros configuráveis (uf, years, system, …)
#   $steps      — lista de etapas (function_name, important_params, line)
#   $structure  — estrutura completa retornada pelo extract_pipeline_structure()
#   $source     — caminho do arquivo de origem
#   $format     — "script" | "rmarkdown" | "quarto" | "function"
#   $raw        — vetor character com o conteúdo bruto do arquivo

# ==============================================================================
#' Ler um Pipeline Analítico Reprodutível Exportado
#'
#' @title Ler RAP Exportado
#'
#' @description
#' Lê um arquivo exportado por \code{sus_export_raps()} e reconstrói um
#' objeto R vivo (\code{rap_object}) contendo os metadados de sessão,
#' parâmetros configuráveis e a estrutura completa do pipeline. O objeto
#' resultante pode ser inspecionado com \code{sus_inspect_rap()}, executado
#' com \code{sus_run_rap()}, ou atualizado com \code{sus_update_rap()}.
#'
#' @param file_path \code{character(1)} — Caminho para o arquivo \code{.R},
#'   \code{.Rmd} ou \code{.qmd} exportado por \code{sus_export_raps()}.
#' @param lang \code{character(1)} — Idioma das mensagens de log:
#'   \code{"pt"} (padrão) ou \code{"en"}.
#' @param validate \code{logical(1)} — Verificar integridade do arquivo
#'   (estrutura mínima esperada). Padrão: \code{TRUE}.
#'
#' @return Um objeto \code{rap_object} (lista S3) com campos:
#'   \itemize{
#'     \item \code{$metadata}  — versão R, pacotes, timestamp, plataforma
#'     \item \code{$params}    — uf, years, system, time_unit, lang, seed
#'     \item \code{$steps}     — lista de etapas do pipeline
#'     \item \code{$structure} — estrutura derivada completa
#'     \item \code{$source}    — caminho do arquivo lido
#'     \item \code{$format}    — formato detectado
#'     \item \code{$raw}       — conteúdo bruto do arquivo
#'   }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Ler um script exportado
#' rap <- sus_read_rap("pipeline_cardio.R")
#'
#' # Ler um documento Quarto
#' rap <- sus_read_rap("relatorio_dengue.qmd", lang = "pt")
#'
#' # Ler sem validação (mais permissivo)
#' rap <- sus_read_rap("pipeline_legado.R", validate = FALSE)
#' }
sus_read_rap <- function(file_path, lang = "pt", validate = TRUE) {

  lang <- match.arg(lang, c("pt", "en"))

  # ── 1. Verificar arquivo ────────────────────────────────────────────────────
  if (!is.character(file_path) || length(file_path) != 1L || nchar(file_path) == 0L) {
    stop(.rmsg(lang, "bad_filepath"), call. = FALSE)
  }
  if (!file.exists(file_path)) {
    stop(.rmsg(lang, "not_found", file_path), call. = FALSE)
  }

  message(.rmsg(lang, "reading", file_path))

  # ── 2. Ler arquivo bruto ────────────────────────────────────────────────────
  raw <- tryCatch(
    readLines(file_path, warn = FALSE, encoding = "UTF-8"),
    error = function(e) stop(.rmsg(lang, "read_error", conditionMessage(e)), call. = FALSE)
  )

  # ── 3. Detectar formato ─────────────────────────────────────────────────────
  fmt <- .detect_rap_format(raw, file_path)
  message(.rmsg(lang, "format_detected", fmt))

  # ── 4. Extrair bloco de metadados YAML ─────────────────────────────────────
  metadata <- tryCatch(
    .parse_rap_metadata(raw, fmt),
    error = function(e) {
      warning(.rmsg(lang, "metadata_partial", conditionMessage(e)), call. = FALSE)
      list()
    }
  )

  # ── 5. Extrair parâmetros configuráveis ─────────────────────────────────────
  params <- tryCatch(
    .parse_rap_params(raw, fmt),
    error = function(e) {
      warning(.rmsg(lang, "params_partial", conditionMessage(e)), call. = FALSE)
      list()
    }
  )

  # ── 6. Extrair etapas do pipeline ───────────────────────────────────────────
  steps <- tryCatch(
    .parse_rap_steps(raw),
    error = function(e) {
      warning(.rmsg(lang, "steps_partial", conditionMessage(e)), call. = FALSE)
      list()
    }
  )

  # ── 7. Montar estrutura derivada ────────────────────────────────────────────
  structure_obj <- list(
    steps          = steps,
    type           = metadata$pipeline_type %||% "Pipeline Genérico",
    input_params   = list(
      uf     = params$uf,
      years  = params$years,
      system = params$system
    ),
    output_params  = list(
      time_unit = params$time_unit,
      group_by  = params$group_by
    ),
    total_steps    = length(steps),
    functions_used = unique(vapply(steps, function(s) s$function_name %||% "", character(1L)))
  )

  # ── 8. Construir rap_object ─────────────────────────────────────────────────
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

  # ── 9. Validação de integridade ─────────────────────────────────────────────
  if (validate) {
    issues <- .validate_rap_object(rap_obj)
    if (length(issues) > 0L) {
      warning(
        .rmsg(lang, "validation_issues",
              paste(issues, collapse = "\n  - ")),
        call. = FALSE
      )
    }
  }

  message(.rmsg(lang, "read_ok",
                length(steps),
                params$uf    %||% "?",
                params$years %||% "?"))

  rap_obj
}


# ==============================================================================
#' Inspecionar e Comparar Objetos RAP
#'
#' @title Inspecionar RAP
#'
#' @description
#' Exibe um resumo estruturado de um \code{rap_object}. Quando dois objetos
#' são fornecidos, realiza uma comparação (\emph{diff}) mostrando parâmetros
#' alterados, etapas adicionadas/removidas e divergências de versão de pacote.
#' Útil para auditoria e revisão de pipelines.
#'
#' @param rap \code{rap_object} — Objeto lido por \code{sus_read_rap()}.
#' @param rap2 \code{rap_object} ou \code{NULL} — Segundo objeto para diff.
#'   Se \code{NULL}, apenas o resumo de \code{rap} é impresso.
#' @param verbose \code{logical(1)} — Exibir etapas detalhadas. Padrão: \code{TRUE}.
#' @param lang \code{character(1)} — Idioma das mensagens.
#'
#' @return Invisível: lista com o resumo (e diff, se aplicável).
#' @export
#'
#' @examples
#' \dontrun{
#' rap <- sus_read_rap("pipeline_sp.R")
#' sus_inspect_rap(rap)
#'
#' # Comparar duas versões
#' rap_v1 <- sus_read_rap("pipeline_v1.R")
#' rap_v2 <- sus_read_rap("pipeline_v2.R")
#' sus_inspect_rap(rap_v1, rap_v2)
#' }
sus_inspect_rap <- function(rap, rap2 = NULL, verbose = TRUE, lang = "pt") {

  .check_rap_object(rap, lang)

  # ── Resumo de um RAP ────────────────────────────────────────────────────────
  cat("\n")
  .section_header("RAP object summary")

  .kv("Source",    rap$source)
  .kv("Format",    rap$format)
  .kv("Type",      rap$structure$type %||% "?")
  .kv("R version", rap$metadata$r_version %||% "?")
  .kv("Package",   paste0("climasus4r ",
                           rap$metadata$package_version %||% "?"))
  .kv("Created",   rap$metadata$created %||% "?")
  .kv("Platform",  rap$metadata$platform %||% "?")
  .kv("Seed",      rap$metadata$random_seed %||% rap$params$seed %||% "?")

  .section_sub("Params")
  .kv("uf",        paste(rap$params$uf    %||% "?", collapse = ", "))
  .kv("years",     paste(rap$params$years %||% "?", collapse = ":"))
  .kv("system",    rap$params$system    %||% "?")
  .kv("time_unit", rap$params$time_unit %||% "?")
  .kv("lang",      rap$params$lang      %||% "?")

  if (verbose && length(rap$steps) > 0L) {
    .section_sub(sprintf("Pipeline steps (%d)", length(rap$steps)))
    for (i in seq_along(rap$steps)) {
      s <- rap$steps[[i]]
      params_str <- if (length(s$important_params) > 0L) {
        paste(
          vapply(names(s$important_params), function(k)
            sprintf("%s=%s", k, deparse(s$important_params[[k]])),
            character(1L)),
          collapse = ", "
        )
      } else ""
      cat(sprintf("  %2d. %-40s %s\n", i, s$function_name %||% "?", params_str))
    }
  }

  result <- list(summary = .rap_to_list(rap))

  # ── Diff entre dois RAPs ────────────────────────────────────────────────────
  if (!is.null(rap2)) {
    .check_rap_object(rap2, lang)
    .section_header("Diff: RAP 1 vs RAP 2")

    diff_result <- .diff_raps(rap, rap2)
    result$diff <- diff_result

    if (length(diff_result$params_changed) > 0L) {
      .section_sub("Params changed")
      for (d in diff_result$params_changed) {
        cat(sprintf("  %-12s  %s  ->  %s\n",
                    d$key,
                    .fmt_val(d$before),
                    .fmt_val(d$after)))
      }
    } else {
      cat("  Params identical\n")
    }

    if (length(diff_result$steps_added) > 0L) {
      .section_sub("Steps added in RAP 2")
      for (s in diff_result$steps_added) cat(sprintf("  + %s\n", s))
    }
    if (length(diff_result$steps_removed) > 0L) {
      .section_sub("Steps removed in RAP 2")
      for (s in diff_result$steps_removed) cat(sprintf("  - %s\n", s))
    }

    if (length(diff_result$version_drift) > 0L) {
      .section_sub("Package version drift")
      for (d in diff_result$version_drift) {
        cat(sprintf("  %-20s  %s  ->  %s\n", d$pkg, d$v1 %||% "?", d$v2 %||% "?"))
      }
    }

    if (all(lengths(diff_result) == 0L)) {
      cat("  Pipelines are identical\n")
    }
  }

  cat("\n")
  invisible(result)
}


# ==============================================================================
#' Executar um RAP com Parâmetros Opcionais
#'
#' @title Executar RAP
#'
#' @description
#' Reconstrói e executa o pipeline contido em um \code{rap_object}, com a
#' possibilidade de substituir parâmetros antes da execução. Equivale a
#' re-rodar o script exportado, mas diretamente do R sem tocar no arquivo.
#' O controle de semente e as verificações de dependências são aplicados
#' automaticamente.
#'
#' @param rap \code{rap_object} — Objeto lido por \code{sus_read_rap()}.
#' @param ... Parâmetros a sobrescrever (ex: \code{uf = "RJ"}, \code{years = 2023}).
#'   Qualquer campo de \code{rap$params} pode ser passado aqui.
#' @param envir \code{environment} — Ambiente onde executar o pipeline.
#'   Padrão: \code{parent.frame()}.
#' @param dry_run \code{logical(1)} — Se \code{TRUE}, apenas imprime o código
#'   que seria executado sem executar. Útil para auditar. Padrão: \code{FALSE}.
#' @param lang \code{character(1)} — Idioma das mensagens.
#'
#' @return O \code{data.frame} resultante do pipeline (invisível).
#' @export
#'
#' @examples
#' \dontrun{
#' rap <- sus_read_rap("pipeline_cardio.R")
#'
#' # Executar com os parâmetros originais
#' df <- sus_run_rap(rap)
#'
#' # Executar com novos estados e ano
#' df <- sus_run_rap(rap, uf = c("RJ", "ES"), years = 2021:2023)
#'
#' # Auditoria sem executar
#' sus_run_rap(rap, uf = "BA", dry_run = TRUE)
#' }
sus_run_rap <- function(rap, ..., envir = parent.frame(),
                         dry_run = FALSE, lang = "pt") {

  .check_rap_object(rap, lang)

  # ── 1. Mesclar parâmetros (originais + sobrescritas) ────────────────────────
  overrides <- list(...)
  params    <- modifyList(rap$params, overrides)

  if (length(overrides) > 0L) {
    changed <- paste(names(overrides), collapse = ", ")
    message(.rmsg(lang, "params_overridden", changed))
  }

  # ── 2. Verificar dependências ────────────────────────────────────────────────
  pkgs_needed <- c("climasus4r", "dplyr", "glue")
  missing_pkgs <- pkgs_needed[!vapply(pkgs_needed, requireNamespace,
                                       quietly = TRUE, FUN.VALUE = logical(1L))]
  if (length(missing_pkgs) > 0L) {
    stop(.rmsg(lang, "missing_pkgs", paste(missing_pkgs, collapse = ", ")),
         call. = FALSE)
  }

  # ── 3. Reconstruir chamada do pipeline ──────────────────────────────────────
  pipeline_call <- tryCatch(
    .rebuild_pipeline_call(rap$steps, params),
    error = function(e) stop(.rmsg(lang, "rebuild_error", conditionMessage(e)),
                              call. = FALSE)
  )

  if (dry_run) {
    message(.rmsg(lang, "dry_run_header"))
    cat(paste(pipeline_call, collapse = "\n"), "\n")
    return(invisible(NULL))
  }

  # ── 4. Executar ──────────────────────────────────────────────────────────────
  seed <- params$seed %||% rap$metadata$random_seed %||% 42L
  set.seed(as.integer(seed))

  message(.rmsg(lang, "running", params$uf %||% "?", params$years %||% "?"))
  start <- proc.time()

  result <- tryCatch(
    eval(parse(text = pipeline_call), envir = envir),
    error = function(e) stop(.rmsg(lang, "run_error", conditionMessage(e)),
                              call. = FALSE)
  )

  elapsed <- round((proc.time() - start)[["elapsed"]], 1L)

  if (!is.data.frame(result)) {
    warning(.rmsg(lang, "non_df_result"), call. = FALSE)
  } else {
    message(.rmsg(lang, "run_ok", nrow(result), ncol(result), elapsed))
  }

  invisible(result)
}


# ==============================================================================
#' Atualizar Parâmetros em um RAP Exportado
#'
#' @title Atualizar RAP
#'
#' @description
#' Aplica \emph{patches} em parâmetros específicos diretamente no arquivo
#' exportado, sem necessidade de re-exportar do zero. A função lê o arquivo,
#' substitui os valores no bloco \code{params <- list(...)}, e grava de volta.
#' Um backup é criado automaticamente antes de qualquer modificação.
#'
#' @param rap \code{rap_object} — Objeto lido por \code{sus_read_rap()}.
#' @param ... Parâmetros a atualizar no arquivo (ex: \code{uf = "SP"},
#'   \code{years = 2020:2024}).
#' @param backup \code{logical(1)} — Criar backup \code{.bak} antes de
#'   modificar. Padrão: \code{TRUE}.
#' @param lang \code{character(1)} — Idioma das mensagens.
#'
#' @return Invisível: o \code{rap_object} atualizado (re-lido do arquivo).
#' @export
#'
#' @examples
#' \dontrun{
#' rap <- sus_read_rap("pipeline_sp.R")
#'
#' # Atualizar apenas a UF
#' rap_rj <- sus_update_rap(rap, uf = "\"RJ\"")
#'
#' # Atualizar múltiplos parâmetros de uma vez
#' rap2 <- sus_update_rap(rap, uf = "c(\"SP\", \"RJ\")", years = "2021:2024")
#' }
sus_update_rap <- function(rap, ..., backup = TRUE, lang = "pt") {

  .check_rap_object(rap, lang)

  patches <- list(...)
  if (length(patches) == 0L) {
    message(.rmsg(lang, "no_patches"))
    return(invisible(rap))
  }

  file_path <- rap$source
  if (!file.exists(file_path)) {
    stop(.rmsg(lang, "not_found", file_path), call. = FALSE)
  }

  # ── 1. Backup ────────────────────────────────────────────────────────────────
  if (backup) {
    bak <- paste0(file_path, ".bak")
    file.copy(file_path, bak, overwrite = TRUE)
    message(.rmsg(lang, "backup_created", bak))
  }

  # ── 2. Ler e aplicar patches ─────────────────────────────────────────────────
  lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
  changed_keys <- character(0L)

  for (key in names(patches)) {
    new_val <- patches[[key]]

    # Padrão: "  key = <qualquer coisa>," dentro do bloco params
    pat <- sprintf("^(\\s*%s\\s*=\\s*)(.+?)(,?\\s*)$", key)
    idx <- grep(pat, lines, perl = TRUE)

    if (length(idx) > 0L) {
      lines[idx] <- sub(
        pat,
        sprintf("\\1%s\\3", new_val),
        lines[idx],
        perl = TRUE
      )
      changed_keys <- c(changed_keys, key)
      message(.rmsg(lang, "param_updated", key, new_val))
    } else {
      warning(.rmsg(lang, "param_not_found", key), call. = FALSE)
    }
  }

  # ── 3. Gravar ────────────────────────────────────────────────────────────────
  if (length(changed_keys) > 0L) {
    writeLines(lines, file_path, useBytes = FALSE)
    message(.rmsg(lang, "update_ok", file_path))
  }

  # ── 4. Re-ler e retornar rap atualizado ──────────────────────────────────────
  invisible(sus_read_rap(file_path, lang = lang, validate = FALSE))
}


# ==============================================================================
# Métodos S3 para rap_object
# ==============================================================================

#' @export
print.rap_object <- function(x, ...) {
  cat(sprintf(
    "<rap_object>\n  source : %s\n  format : %s\n  type   : %s\n  steps  : %d\n  uf     : %s\n  years  : %s\n",
    x$source,
    x$format %||% "?",
    x$structure$type %||% "?",
    length(x$steps),
    paste(x$params$uf %||% "?", collapse = ", "),
    paste(x$params$years %||% "?", collapse = ":")
  ))
  invisible(x)
}

#' @export
summary.rap_object <- function(object, ...) sus_inspect_rap(object, ...)

#' @export
`[.rap_object` <- function(x, i) x[[i]]


# ==============================================================================
# Funções internas de parsing
# ==============================================================================

#' @keywords internal
.detect_rap_format <- function(raw, file_path) {
  ext <- tolower(tools::file_ext(file_path))
  if (ext %in% c("rmd", "rmarkdown")) return("rmarkdown")
  if (ext == "qmd")                    return("quarto")

  # Inspecionar conteúdo para casos sem extensão clara
  first_100 <- paste(head(raw, 100L), collapse = "\n")
  if (grepl("^---", first_100) && grepl("quarto|format:", first_100)) return("quarto")
  if (grepl("^---", first_100) && grepl("output:", first_100))         return("rmarkdown")
  return("script")
}


#' @keywords internal
.parse_rap_metadata <- function(raw, fmt) {
  meta <- list()

  # Tentar parser YAML nativo (bloco entre "# --- RAP METADATA" e "# ---")
  meta_start <- grep("^#\\s*---\\s*RAP METADATA", raw)
  meta_end   <- if (length(meta_start) > 0L)
    grep("^#\\s*---\\s*$", raw[seq(meta_start[1L] + 1L, length(raw))])[1L] +
    meta_start[1L] else 0L

  if (length(meta_start) > 0L && meta_end > meta_start[1L]) {
    yaml_lines <- raw[seq(meta_start[1L] + 1L, meta_end - 1L)]
    yaml_text  <- paste(sub("^#\\s?", "", yaml_lines), collapse = "\n")
    meta <- tryCatch(
      yaml::yaml.load(yaml_text),
      error = function(e) list()
    )
  }

  # Fallback: extração via regex das linhas de cabeçalho
  .extract_header_field <- function(pattern) {
    m <- grep(pattern, raw, value = TRUE)
    if (length(m) == 0L) return(NULL)
    trimws(sub(pattern, "\\1", m[1L]))
  }

  meta$r_version       <- meta$r_version       %||% .extract_header_field("^#.*Versão R\\s*:\\s*(.+)")
  meta$package_version <- meta$package_version %||% .extract_header_field("^#.*climasus4r\\s+(\\S+)")
  meta$created         <- meta$created         %||% .extract_header_field("^#.*Data\\s*:\\s*(.+)")
  meta$platform        <- meta$platform        %||% .extract_header_field("^#.*Plataforma\\s*:\\s*(.+)")
  meta$random_seed     <- meta$random_seed     %||% {
    s <- .extract_header_field("set\\.seed\\((\\d+)")
    if (!is.null(s)) as.integer(s) else 42L
  }

  meta
}


#' @keywords internal
.parse_rap_params <- function(raw, fmt) {
  params <- list()

  # Localizar bloco params <- list(...)
  start <- grep("^\\s*params\\s*<-\\s*list\\s*\\(", raw)
  if (length(start) == 0L) return(params)

  # Encontrar fechamento correspondente do parêntese
  end <- start[1L]
  depth <- 0L
  for (i in seq(start[1L], min(start[1L] + 50L, length(raw)))) {
    depth <- depth + nchar(gsub("[^(]", "", raw[i])) -
                     nchar(gsub("[^)]", "", raw[i]))
    if (depth <= 0L && i > start[1L]) { end <- i; break }
  }

  block <- raw[seq(start[1L], end)]
  block_text <- paste(block, collapse = "\n")

  # Extrair pares chave = valor com eval seguro
  .safe_extract <- function(key, text) {
    pat <- sprintf("%s\\s*=\\s*([^,\n]+)", key)
    m   <- regmatches(text, regexpr(pat, text, perl = TRUE))
    if (length(m) == 0L) return(NULL)
    val_str <- trimws(sub(sprintf("%s\\s*=\\s*", key), "", m))
    tryCatch(eval(parse(text = val_str)), error = function(e) val_str)
  }

  params$uf        <- .safe_extract("uf",        block_text)
  params$years     <- .safe_extract("years",     block_text)
  params$system    <- .safe_extract("system",    block_text)
  params$time_unit <- .safe_extract("time_unit", block_text)
  params$lang      <- .safe_extract("lang",      block_text)
  params$seed      <- .safe_extract("seed",      block_text)
  params$output_dir <- .safe_extract("output_dir", block_text)

  Filter(Negate(is.null), params)
}


#' @keywords internal
.parse_rap_steps <- function(raw) {
  # Detectar linhas com funções sus_* conhecidas
  sus_funcs <- c(
    "sus_data_import", "sus_data_clean_encoding", "sus_data_standardize",
    "sus_data_filter_cid", "sus_data_filter_demographics",
    "sus_data_create_variables", "sus_data_aggregate", "sus_data_export",
    "sus_data_quality_report"
  )

  pattern <- paste(sus_funcs, collapse = "|")
  idx     <- grep(pattern, raw)

  steps <- lapply(idx, function(i) {
    line <- trimws(raw[i])
    fn   <- regmatches(line, regexpr(paste(sus_funcs, collapse = "|"), line))
    fn   <- if (length(fn) > 0L) fn[1L] else "unknown"

    # Extrair parâmetros inline
    params_raw <- sub(sprintf(".*%s\\s*\\(", fn), "", line)
    params_raw <- sub("\\).*$", "", params_raw)

    important <- .parse_inline_params(fn, params_raw)

    list(
      function_name    = fn,
      important_params = important,
      line             = line
    )
  })

  steps
}


#' @keywords internal
.parse_inline_params <- function(fn, params_str) {
  keys_map <- list(
    sus_data_import              = c("uf", "year", "system"),
    sus_data_filter_cid          = c("disease_group"),
    sus_data_filter_demographics = c("age_range"),
    sus_data_aggregate           = c("time_unit", "group_by"),
    sus_data_create_variables         = c("create_age_groups", "create_calendar_vars")
  )
  keys   <- keys_map[[fn]] %||% character(0L)
  result <- list()

  for (k in keys) {
    pat <- sprintf("%s\\s*=\\s*([\"'][^\"']+[\"']|[^,]+)", k)
    m   <- regmatches(params_str, regexpr(pat, params_str, perl = TRUE))
    if (length(m) > 0L) {
      val_str <- trimws(sub(sprintf("%s\\s*=\\s*", k), "", m))
      result[[k]] <- tryCatch(eval(parse(text = val_str)), error = function(e) val_str)
    }
  }
  result
}


#' @keywords internal
.rebuild_pipeline_call <- function(steps, params) {
  if (length(steps) == 0L) stop("Nenhuma etapa para reconstruir.", call. = FALSE)

  # Primeira etapa: sus_data_import com params atualizados
  first <- steps[[1L]]
  uf_s  <- deparse(params$uf    %||% "NULL")
  yr_s  <- deparse(params$years %||% "NULL")
  sys_s <- deparse(params$system %||% "SIM-DO")
  lg_s  <- sprintf("\"%s\"", params$lang %||% "pt")

  call_lines <- c(
    sprintf("sus_data_import(uf = %s, year = %s, system = %s, lang = %s, parallel = TRUE)",
            uf_s, yr_s, sys_s, lg_s)
  )

  # Etapas subsequentes
  for (s in steps[-1L]) {
    fn   <- s$function_name
    pars <- s$important_params

    if (length(pars) > 0L) {
      par_str <- paste(
        vapply(names(pars), function(k) sprintf("%s = %s", k, deparse(pars[[k]])),
               character(1L)),
        collapse = ", "
      )
      call_lines <- c(call_lines, sprintf("|>\n  %s(%s)", fn, par_str))
    } else {
      call_lines <- c(call_lines, sprintf("|>\n  %s(lang = \"%s\")",
                                           fn, params$lang %||% "pt"))
    }
  }

  paste(call_lines, collapse = " ")
}


#' @keywords internal
.validate_rap_object <- function(rap_obj) {
  issues <- character(0L)

  if (length(rap_obj$steps) == 0L)
    issues <- c(issues, "Nenhuma etapa de pipeline detectada no arquivo.")

  if (is.null(rap_obj$params$uf))
    issues <- c(issues, "Parâmetro 'uf' não encontrado.")

  if (is.null(rap_obj$params$years))
    issues <- c(issues, "Parâmetro 'years' não encontrado.")

  issues
}


#' @keywords internal
.diff_raps <- function(r1, r2) {
  # Params diff
  all_keys    <- union(names(r1$params), names(r2$params))
  params_diff <- Filter(Negate(is.null), lapply(all_keys, function(k) {
    v1 <- r1$params[[k]]
    v2 <- r2$params[[k]]
    if (!identical(v1, v2))
      list(key = k, before = v1, after = v2)
    else NULL
  }))

  # Steps diff
  fn1   <- vapply(r1$steps, function(s) s$function_name %||% "", character(1L))
  fn2   <- vapply(r2$steps, function(s) s$function_name %||% "", character(1L))
  added   <- setdiff(fn2, fn1)
  removed <- setdiff(fn1, fn2)

  # Package version drift
  p1 <- r1$metadata$packages %||% list()
  p2 <- r2$metadata$packages %||% list()
  all_pkgs <- union(names(p1), names(p2))
  version_drift <- Filter(Negate(is.null), lapply(all_pkgs, function(p) {
    v1 <- p1[[p]]
    v2 <- p2[[p]]
    if (!identical(v1, v2))
      list(pkg = p, v1 = v1, v2 = v2)
    else NULL
  }))

  list(
    params_changed = params_diff,
    steps_added    = added,
    steps_removed  = removed,
    version_drift  = version_drift
  )
}


#' @keywords internal
.check_rap_object <- function(x, lang = "pt") {
  if (!inherits(x, "rap_object")) {
    stop(.rmsg(lang, "not_rap_object"), call. = FALSE)
  }
}

#' @keywords internal
.rap_to_list <- function(rap) {
  list(
    source   = rap$source,
    format   = rap$format,
    type     = rap$structure$type,
    params   = rap$params,
    n_steps  = length(rap$steps),
    metadata = rap$metadata
  )
}


# ==============================================================================
# Utilitários de impressão
# ==============================================================================

.section_header <- function(title) {
  cat(sprintf("── %s %s\n", title,
              paste(rep("─", max(0L, 50L - nchar(title))), collapse = "")))
}
.section_sub <- function(title) cat(sprintf("\n  %s:\n", title))
.kv   <- function(k, v) cat(sprintf("  %-14s %s\n", paste0(k, ":"), v))
.fmt_val <- function(x) if (is.null(x)) "NULL" else paste(x, collapse = ", ")


# ==============================================================================
# Sistema de mensagens localizado (read side)
# ==============================================================================

.READ_MSGS <- list(
  pt = list(
    bad_filepath      = "file_path deve ser uma string não vazia.",
    not_found         = "Arquivo não encontrado: %s",
    read_error        = "Falha ao ler o arquivo: %s",
    reading           = "📖 Lendo RAP: %s",
    format_detected   = "   Formato detectado: %s",
    metadata_partial  = "⚠️  Metadados parciais: %s",
    params_partial    = "⚠️  Parâmetros parciais: %s",
    steps_partial     = "⚠️  Etapas parciais: %s",
    validation_issues = "⚠️  Problemas de validação:\n  - %s",
    read_ok           = "✅ Leitura OK | %d etapas | uf=%s | years=%s",
    no_patches        = "Nenhum parâmetro para atualizar fornecido.",
    backup_created    = "💾 Backup criado: %s",
    param_updated     = "  ✎ %s = %s",
    param_not_found   = "Parâmetro '%s' não encontrado no arquivo; ignorado.",
    update_ok         = "✅ Arquivo atualizado: %s",
    params_overridden = "  ↺ Parâmetros sobrescritos: %s",
    missing_pkgs      = "Pacotes ausentes: %s",
    rebuild_error     = "Falha ao reconstruir pipeline: %s",
    dry_run_header    = "── [dry_run] Código que seria executado ──",
    running           = "🚀 Executando | uf=%s | years=%s",
    run_error         = "Falha na execução: %s",
    run_ok            = "✅ Concluído | %d linhas × %d colunas | %.1fs",
    non_df_result     = "O resultado não é um data.frame.",
    not_rap_object    = "Argumento deve ser um rap_object (use sus_read_rap())."
  ),
  en = list(
    bad_filepath      = "file_path must be a non-empty string.",
    not_found         = "File not found: %s",
    read_error        = "Failed to read file: %s",
    reading           = "📖 Reading RAP: %s",
    format_detected   = "   Format detected: %s",
    metadata_partial  = "⚠️  Partial metadata: %s",
    params_partial    = "⚠️  Partial params: %s",
    steps_partial     = "⚠️  Partial steps: %s",
    validation_issues = "⚠️  Validation issues:\n  - %s",
    read_ok           = "✅ Read OK | %d steps | uf=%s | years=%s",
    no_patches        = "No parameters to update were provided.",
    backup_created    = "💾 Backup created: %s",
    param_updated     = "  ✎ %s = %s",
    param_not_found   = "Parameter '%s' not found in file; skipped.",
    update_ok         = "✅ File updated: %s",
    params_overridden = "  ↺ Params overridden: %s",
    missing_pkgs      = "Missing packages: %s",
    rebuild_error     = "Failed to rebuild pipeline: %s",
    dry_run_header    = "── [dry_run] Code that would be executed ──",
    running           = "🚀 Running | uf=%s | years=%s",
    run_error         = "Execution failed: %s",
    run_ok            = "✅ Done | %d rows × %d cols | %.1fs",
    non_df_result     = "Result is not a data.frame.",
    not_rap_object    = "Argument must be a rap_object (use sus_read_rap())."
  )
)

#' @keywords internal
.rmsg <- function(lang, key, ...) {
  tmpl <- .READ_MSGS[[lang]][[key]] %||% .READ_MSGS[["en"]][[key]] %||% key
  if (...length() > 0L) sprintf(tmpl, ...) else tmpl
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0L) a else b
