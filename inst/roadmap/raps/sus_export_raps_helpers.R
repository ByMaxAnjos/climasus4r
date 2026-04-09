# ==============================================================================
# sus_export_raps_helpers.R — Análise e Extração de Estrutura de Pipeline
# Pacote: climasus4r
# ==============================================================================
# Dependências internas: sus_export_raps.R (operador %||%, .log_warn)
# ==============================================================================

# ==============================================================================
#' Extrair Estrutura do Pipeline
#'
#' @description
#' Analisa recursivamente uma expressão de pipeline (`%>%` ou `|>`) e retorna
#' uma lista estruturada com as etapas, parâmetros de entrada/saída e tipo
#' de análise identificado.
#'
#' @param pipeline_expr Expressão R (objeto `quote()` ou `rlang::quo()`).
#'
#' @return Lista com campos:
#'   \itemize{
#'     \item `steps` — lista de etapas (função, argumentos, parâmetros)
#'     \item `type` — tipo de pipeline identificado (string)
#'     \item `input_params` — uf, years, system
#'     \item `output_params` — time_unit, group_by
#'     \item `total_steps` — número inteiro
#'     \item `functions_used` — vetor de nomes de funções únicas
#'   }
#'
#' @keywords internal
extract_pipeline_structure <- function(pipeline_expr) {

  # Desembrulhar quosure (rlang) se necessário
  if (rlang::is_quosure(pipeline_expr)) {
    pipeline_expr <- rlang::get_expr(pipeline_expr)
  }

  # Operadores de pipe reconhecidos
  .pipe_ops <- c("%>%", "|>", "pipe")

  # ── Parser recursivo ────────────────────────────────────────────────────────
  parse_pipe <- function(expr, steps_list) {
    if (!is.call(expr)) return(steps_list)

    op <- tryCatch(as.character(expr[[1L]]), error = function(e) "")

    if (op %in% .pipe_ops) {
      # Nó pipe: descer pelos dois lados
      steps_list <- parse_pipe(expr[[2L]], steps_list)
      steps_list <- parse_pipe(expr[[3L]], steps_list)
    } else {
      # Nó folha: chamada de função
      func_name <- op
      args      <- if (length(expr) > 1L) as.list(expr[-1L]) else list()

      params <- tryCatch(
        extract_important_params(func_name, args),
        error = function(e) list()
      )

      steps_list <- c(steps_list, list(list(
        function_name    = func_name,
        arguments        = args,
        important_params = params,
        line             = tryCatch(deparse(expr)[1L], error = function(e) "")
      )))
    }
    steps_list
  }

  steps <- parse_pipe(pipeline_expr, list())

  if (length(steps) == 0L) {
    stop("Nenhuma etapa encontrada na expressão fornecida.")
  }

  # ── Derivar campos resumidos ─────────────────────────────────────────────────
  pipeline_type  <- tryCatch(identify_pipeline_type(steps),  error = function(e) "Pipeline Genérico")
  input_params   <- tryCatch(extract_input_parameters(steps),  error = function(e) list())
  output_params  <- tryCatch(extract_output_parameters(steps), error = function(e) list())
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


# ==============================================================================
#' Identificar Tipo de Pipeline pelas Funções Usadas
#' @keywords internal
identify_pipeline_type <- function(steps) {
  if (length(steps) == 0L) return("Pipeline Genérico")

  funcs <- vapply(steps, function(s) s$function_name %||% "", character(1L))

  cond <- list(
    respiratory  = any(grepl("filter_cid",  funcs)) &&
                   any(vapply(steps, function(s) {
                     identical(s$important_params$disease_group, "respiratory")
                   }, logical(1L))),
    cardiovascular = any(grepl("filter_cid", funcs)) &&
                   any(vapply(steps, function(s) {
                     identical(s$important_params$disease_group, "cardiovascular")
                   }, logical(1L))),
    heat_related = any(grepl("filter_cid",  funcs)) &&
                   any(vapply(steps, function(s) {
                     identical(s$important_params$disease_group, "heat_related")
                   }, logical(1L))),
    sinan        = any(grepl("SINAN",       funcs, ignore.case = TRUE)),
    heat_wave    = any(grepl("heat|calor",  funcs, ignore.case = TRUE))
  )

  if (cond$respiratory)   return("Mortalidade por Doenças Respiratórias")
  if (cond$cardiovascular) return("Mortalidade Cardiovascular")
  if (cond$heat_related)  return("Mortalidade por Calor")
  if (cond$sinan)         return("Morbidade (SINAN)")
  if (cond$heat_wave)     return("Eventos Climáticos Extremos")

  # Fallback: CIDs customizados
  if (any(grepl("filter_cid", funcs))) {
    cid_groups <- .extract_cid_groups(steps)
    if (length(cid_groups) > 0L) {
      return(paste("Mortalidade por", paste(cid_groups, collapse = " e ")))
    }
  }

  "Pipeline Genérico"
}


# ── Parâmetros importantes por função ─────────────────────────────────────────

#' @keywords internal
.IMPORTANT_PARAMS_MAP <- list(
  sus_data_import              = c("uf", "year", "system"),
  sus_data_filter_cid          = c("disease_group"),
  sus_data_filter_demographics = c("age_range"),
  sus_data_aggregate           = c("time_unit", "group_by"),
  sus_data_create_variables         = c("create_age_groups", "create_calendar_vars",
                                    "age_breaks", "age_labels")
)

#' @keywords internal
extract_important_params <- function(func_name, args) {
  params     <- list()
  param_keys <- .IMPORTANT_PARAMS_MAP[[func_name]]

  if (is.null(param_keys) || length(args) == 0L) return(params)

  arg_names <- names(args)

  for (p in param_keys) {
    idx <- which(arg_names == p)
    if (length(idx) == 1L) {
      params[[p]] <- tryCatch(eval(args[[idx]]), error = function(e) args[[idx]])
    }
  }
  params
}


# ── Extração de parâmetros de entrada/saída ────────────────────────────────────

#' @keywords internal
extract_input_parameters <- function(steps) {
  import_step <- Filter(function(s) s$function_name == "sus_data_import", steps)

  if (length(import_step) == 0L) return(list(uf = NULL, years = NULL, system = NULL))

  s <- import_step[[1L]]
  list(
    uf     = s$important_params$uf    %||% NULL,
    years  = s$important_params$year  %||% NULL,
    system = s$important_params$system %||% NULL
  )
}

#' @keywords internal
extract_output_parameters <- function(steps) {
  agg_step <- Filter(function(s) s$function_name == "sus_data_aggregate", steps)

  if (length(agg_step) == 0L) return(list(time_unit = NULL, group_by = NULL))

  s <- agg_step[[1L]]
  list(
    time_unit = s$important_params$time_unit %||% NULL,
    group_by  = s$important_params$group_by  %||% NULL
  )
}

#' @keywords internal
.extract_cid_groups <- function(steps) {
  cid_steps <- Filter(function(s) grepl("filter_cid", s$function_name), steps)
  unique(vapply(cid_steps, function(s) {
    s$important_params$disease_group %||% ""
  }, character(1L)))
}


# ==============================================================================
#' Identificar Parâmetros Variáveis do Pipeline
#'
#' @description
#' Retorna os parâmetros candidatos a serem expostos como argumentos de uma
#' função exportada (uf, years, output_dir e time_unit).
#'
#' @keywords internal
identify_variable_params <- function(structure) {
  params <- character(0L)

  if (!is.null(structure$input_params$uf))    params <- c(params, "uf")
  if (!is.null(structure$input_params$years)) params <- c(params, "years")
  params <- c(params, "output_dir")
  if (!is.null(structure$output_params$time_unit)) params <- c(params, "time_unit")

  params
}


# ==============================================================================
#' Criar Metadados Completos do Pipeline
#'
#' @param structure Lista retornada por `extract_pipeline_structure()`.
#' @param lang `"pt"` ou `"en"`.
#' @param exec_meta Lista retornada por `.collect_session_metadata()`.
#'
#' @keywords internal
create_pipeline_metadata <- function(structure, lang, exec_meta = list()) {

  desc <- if (lang == "pt") {
    sprintf("Pipeline para análise de %s com dados do DATASUS", structure$type)
  } else {
    sprintf("Pipeline for %s analysis using DATASUS data", structure$type)
  }

  pkg_ver <- tryCatch(
    as.character(utils::packageVersion("climasus4r")),
    error = function(e) "desconhecida"
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
    random_seed     = .CLIMASUS_EXPORT_DEFAULTS$random_seed
  )
}


# ==============================================================================
#' Capturar Código do Pipeline como Vetor de Linhas
#' @keywords internal
capture_pipeline_code <- function(structure) {
  ip <- structure$input_params

  code <- c(
    "# Importar dados",
    "df <- sus_data_import(",
    sprintf("  uf     = %s,",    deparse(ip$uf    %||% "NULL")),
    sprintf("  year   = %s,",    deparse(ip$years %||% "NULL")),
    sprintf("  system = \"%s\",", ip$system %||% "SIM-DO"),
    "  parallel = TRUE,",
    "  lang     = params$lang",
    ")"
  )

  for (step in structure$steps[-1L]) {
    fn <- step$function_name

    # Serializar argumentos conhecidos de forma limpa
    arg_lines <- vapply(names(step$important_params), function(k) {
      v <- step$important_params[[k]]
      sprintf("  %s = %s", k, deparse(v))
    }, character(1L))

    if (length(arg_lines) > 0L) {
      code <- c(code,
        "|>",
        sprintf("  %s(", fn),
        paste(arg_lines, collapse = ",\n"),
        "  )"
      )
    } else {
      code <- c(code,
        "|>",
        sprintf("  %s(lang = params$lang)", fn)
      )
    }
  }

  c(code,
    "|>",
    "  sus_data_export(",
    "    file_path        = file.path(params$output_dir, \"resultado.csv\"),",
    "    format           = \"csv\",",
    "    include_metadata = TRUE,",
    "    lang             = params$lang",
    "  )"
  )
}
