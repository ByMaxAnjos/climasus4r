# ==============================================================================
# sus_export_raps.R — Exportação Robusta de Pipelines Reprodutíveis
# Pacote: climasus4r
# ==============================================================================
# Autor:       climasus4r Team
# Versão:      2.0.0
# Última rev.: 2025-01-01
# Descrição:   Exporta um pipeline analítico como script R autônomo,
#              RMarkdown, Quarto ou função encapsulada, com metadados,
#              validação e estrutura para fácil reutilização.
#
# Dependências: rlang, glue, fs (sugeridas: rmarkdown, quarto)
# ==============================================================================

# ------------------------------------------------------------------------------
# Parâmetros globais (ajuste antes de usar)
# ------------------------------------------------------------------------------
.CLIMASUS_EXPORT_DEFAULTS <- list(
  format               = "script",       # "script" | "rmarkdown" | "quarto" | "function"
  lang                 = "pt",           # "pt" | "en"
  output_type          = "analysis",     # "analysis" | "dashboard" | "report"
  include_metadata     = TRUE,
  include_validation   = TRUE,
  include_documentation = "standard",    # "minimal" | "standard" | "comprehensive"
  encoding             = "UTF-8",
  random_seed          = 42L             # semente para etapas com amostragem
)

# ==============================================================================
#' Exportar Pipeline Reprodutível (sus_export_raps)
#'
#' @title Exportar Pipeline Analítico Reprodutível
#'
#' @description
#' Converte um pipeline `climasus4r` em um artefato portável (script R,
#' RMarkdown, Quarto ou função encapsulada). O arquivo exportado inclui
#' metadados de sessão, validação de dados e documentação parametrizável,
#' promovendo os princípios de Reproducible Analytical Pipelines (RAPs).
#'
#' @param pipeline Expressão ou objeto de pipeline (lista de etapas ou pipe).
#'   Se `NULL`, tenta capturar o contexto de chamada.
#' @param file_path `character(1)` — Caminho de destino. Se `NULL`, retorna
#'   o conteúdo gerado como vetor de caracteres invisível.
#' @param format `character(1)` — Formato de saída. Um entre:
#'   \itemize{
#'     \item{\code{"script"}}{ Script R autônomo (`.R`)}
#'     \item{\code{"rmarkdown"}}{ Documento RMarkdown (`.Rmd`)}
#'     \item{\code{"quarto"}}{ Documento Quarto (`.qmd`)}
#'     \item{\code{"function"}}{ Função reutilizável (`.R`)}
#'   }
#' @param include_metadata `logical(1)` — Incluir metadados de sessão
#'   (versão R, pacotes, data/hora, SO). Padrão: `TRUE`.
#' @param include_validation `logical(1)` — Incluir bloco de validação de
#'   dados (completude, consistência, relatório de qualidade). Padrão: `TRUE`.
#' @param include_documentation `character(1)` — Nível de documentação:
#'   `"minimal"`, `"standard"` (padrão) ou `"comprehensive"`.
#' @param lang `character(1)` — Idioma dos comentários e mensagens:
#'   `"pt"` (português, padrão) ou `"en"` (inglês).
#' @param output_type `character(1)` — Finalidade do documento gerado:
#'   `"analysis"`, `"dashboard"` ou `"report"`.
#' @param overwrite `logical(1)` — Sobrescrever arquivo existente.
#'   Padrão: `FALSE` (lança erro se o arquivo já existe).
#'
#' @return Invisível: vetor `character` com o conteúdo exportado.
#'   Efeito colateral: grava o arquivo em `file_path` quando informado.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # ── Exemplo 1: Script R simples ────────────────────────────────────────────
#' pipeline_expr <- quote(
#'   sus_data_import(uf = "SP", year = 2020:2022, system = "SIM-DO",
#'                   lang = "pt") |>
#'     sus_data_clean_encoding(lang = "pt") |>
#'     sus_data_standardize(lang = "pt") |>
#'     sus_data_filter_cid(disease_group = "cardiovascular", lang = "pt") |>
#'     sus_data_aggregate(time_unit = "month", lang = "pt")
#' )
#'
#' sus_export_raps(
#'   pipeline   = pipeline_expr,
#'   file_path  = "pipeline_cardio.R",
#'   format     = "script",
#'   lang       = "pt"
#' )
#'
#' # ── Exemplo 2: RMarkdown com validação completa ────────────────────────────
#' sus_export_raps(
#'   pipeline              = pipeline_expr,
#'   file_path             = "relatorio_cardio.Rmd",
#'   format                = "rmarkdown",
#'   include_validation    = TRUE,
#'   include_documentation = "comprehensive",
#'   output_type           = "report",
#'   lang                  = "pt"
#' )
#'
#' # ── Exemplo 3: Função reutilizável com parâmetros configuráveis ────────────
#' sus_export_raps(
#'   pipeline  = pipeline_expr,
#'   file_path = "run_pipeline_cardio.R",
#'   format    = "function",
#'   lang      = "pt"
#' )
#' }
sus_export_raps <- function(
    pipeline              = NULL,
    file_path             = NULL,
    format                = c("script", "rmarkdown", "quarto", "function"),
    include_metadata      = .CLIMASUS_EXPORT_DEFAULTS$include_metadata,
    include_validation    = .CLIMASUS_EXPORT_DEFAULTS$include_validation,
    include_documentation = .CLIMASUS_EXPORT_DEFAULTS$include_documentation,
    lang                  = .CLIMASUS_EXPORT_DEFAULTS$lang,
    output_type           = .CLIMASUS_EXPORT_DEFAULTS$output_type,
    overwrite             = FALSE
) {

  # ── 0. Metadados de execução ────────────────────────────────────────────────
  exec_meta <- .collect_session_metadata()
  .log_info(lang, "session_start", exec_meta$r_version)

  # ── 1. Validação de argumentos ──────────────────────────────────────────────
  format                <- tryCatch(
    match.arg(format),
    error = function(e) .abort(lang, "invalid_format", paste(format, collapse = ", "))
  )
  include_documentation <- match.arg(
    include_documentation, c("minimal", "standard", "comprehensive")
  )
  lang                  <- match.arg(lang, c("pt", "en"))
  output_type           <- match.arg(output_type, c("analysis", "dashboard", "report"))

  if (!is.logical(include_metadata)   || length(include_metadata)   != 1L) {
    .abort(lang, "bad_logical", "include_metadata")
  }
  if (!is.logical(include_validation) || length(include_validation) != 1L) {
    .abort(lang, "bad_logical", "include_validation")
  }

  if (!is.null(file_path)) {
    .validate_file_path(file_path, format, overwrite, lang)
  }

  # ── 2. Capturar / inspecionar pipeline ─────────────────────────────────────
  if (is.null(pipeline)) {
    pipeline <- rlang::enquo(pipeline)
    .log_warn(lang, "pipeline_null")
  }

  pipeline_structure <- tryCatch(
    extract_pipeline_structure(pipeline),
    error = function(e) {
      .abort(lang, "parse_error", conditionMessage(e))
    }
  )

  .validate_pipeline_structure(pipeline_structure, lang)
  .log_info(lang, "steps_found", pipeline_structure$total_steps)

  # ── 3. Construir metadados completos ────────────────────────────────────────
  metadata <- if (include_metadata) {
    tryCatch(
      create_pipeline_metadata(pipeline_structure, lang, exec_meta),
      error = function(e) {
        .log_warn(lang, "metadata_failed", conditionMessage(e))
        NULL
      }
    )
  } else NULL

  # ── 4. Gerar conteúdo conforme formato ─────────────────────────────────────
  .log_info(lang, "generating", format)

  content <- tryCatch({
    switch(
      format,
      "script"    = create_script_content(
                      pipeline_structure, metadata,
                      include_validation, include_documentation, lang, exec_meta),
      "rmarkdown" = create_rmarkdown_content(
                      pipeline_structure, metadata,
                      include_validation, include_documentation, lang,
                      output_type, exec_meta),
      "quarto"    = create_quarto_content(
                      pipeline_structure, metadata,
                      include_validation, include_documentation, lang,
                      output_type, exec_meta),
      "function"  = create_function_content(
                      pipeline_structure, metadata,
                      include_validation, include_documentation, lang)
    )
  }, error = function(e) {
    .abort(lang, "content_error", conditionMessage(e))
  })

  if (!is.character(content) || length(content) == 0L) {
    .abort(lang, "empty_content")
  }

  # ── 5. Gravar arquivo ───────────────────────────────────────────────────────
  if (!is.null(file_path)) {
    tryCatch({
      dir_path <- dirname(file_path)
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
        .log_info(lang, "dir_created", dir_path)
      }
      writeLines(content, file_path, useBytes = FALSE)
      .log_success(lang, "export_ok", file_path)
    }, error = function(e) {
      .abort(lang, "write_error", conditionMessage(e))
    })
  }

  invisible(content)
}


# ==============================================================================
# Funções auxiliares internas
# ==============================================================================

# ── Metadados de sessão ─────────────────────────────────────────────────────

#' @keywords internal
.collect_session_metadata <- function() {
  pkgs_of_interest <- c("climasus4r", "dplyr", "rlang", "glue",
                         "ggplot2", "rmarkdown", "quarto")

  installed <- vapply(pkgs_of_interest, function(p) {
    tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
  }, character(1L))

  list(
    r_version       = R.version.string,
    r_version_num   = paste(R.Version()$major, R.Version()$minor, sep = "."),
    platform        = .detect_platform(),
    exec_timestamp  = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    exec_date       = format(Sys.Date(), "%Y-%m-%d"),
    packages        = installed[!is.na(installed)],
    locale          = Sys.getlocale("LC_CTYPE"),
    tz              = Sys.timezone()
  )
}

#' @keywords internal
.detect_platform <- function() {
  os <- .Platform$OS.type
  if (os == "windows") return("windows")
  sys <- tryCatch(tolower(Sys.info()[["sysname"]]), error = function(e) "unix")
  if (grepl("darwin", sys)) return("macos")
  if (grepl("linux",  sys)) return("linux")
  return(os)
}

# ── Validações ─────────────────────────────────────────────────────────────

#' @keywords internal
.validate_file_path <- function(file_path, format, overwrite, lang) {
  if (!is.character(file_path) || length(file_path) != 1L || nchar(file_path) == 0L) {
    .abort(lang, "bad_filepath")
  }

  ext_map <- c(script = ".R", rmarkdown = ".Rmd", quarto = ".qmd", "function" = ".R")
  expected_ext <- ext_map[[format]]
  actual_ext   <- tolower(tools::file_ext(file_path))

  if (!is.na(expected_ext) &&
      !identical(paste0(".", actual_ext), tolower(expected_ext))) {
    .log_warn(lang, "ext_mismatch", expected_ext, file_path)
  }

  if (file.exists(file_path) && !overwrite) {
    .abort(lang, "file_exists", file_path)
  }
}

#' @keywords internal
.validate_pipeline_structure <- function(structure, lang) {
  required <- c("steps", "type", "input_params", "output_params",
                 "total_steps", "functions_used")
  missing_fields <- setdiff(required, names(structure))

  if (length(missing_fields) > 0L) {
    .abort(lang, "bad_structure", paste(missing_fields, collapse = ", "))
  }

  if (!is.list(structure$steps) || length(structure$steps) == 0L) {
    .abort(lang, "empty_steps")
  }

  if (is.null(structure$input_params$uf) ||
      is.null(structure$input_params$years) ||
      is.null(structure$input_params$system)) {
    .log_warn(lang, "missing_input_params")
  }
}

# ── Sistema de logging ──────────────────────────────────────────────────────

.MSGS <- list(
  pt = list(
    session_start     = "🔍 Iniciando exportação | %s",
    steps_found       = "📋 Etapas detectadas no pipeline: %d",
    generating        = "⚙️  Gerando conteúdo no formato: '%s'",
    dir_created       = "📁 Diretório criado: %s",
    export_ok         = "✅ Pipeline exportado com sucesso para: %s",
    pipeline_null     = "⚠️  Pipeline não fornecido; tentando capturar do contexto.",
    metadata_failed   = "⚠️  Falha ao gerar metadados: %s",
    ext_mismatch      = "⚠️  Extensão esperada '%s' difere de '%s'.",
    invalid_format    = "Formato inválido: '%s'. Use 'script', 'rmarkdown', 'quarto' ou 'function'.",
    bad_logical       = "Argumento '%s' deve ser TRUE ou FALSE.",
    bad_filepath      = "file_path deve ser uma string não vazia.",
    file_exists       = "O arquivo '%s' já existe. Use overwrite = TRUE para sobrescrever.",
    parse_error       = "Falha ao analisar o pipeline: %s",
    bad_structure     = "Estrutura do pipeline inválida. Campos ausentes: %s",
    empty_steps       = "O pipeline não contém etapas válidas.",
    missing_input_params = "⚠️  Parâmetros de entrada (uf/years/system) não detectados.",
    content_error     = "Erro ao gerar conteúdo: %s",
    empty_content     = "O conteúdo gerado está vazio.",
    write_error       = "Não foi possível gravar o arquivo: %s"
  ),
  en = list(
    session_start     = "🔍 Starting export | %s",
    steps_found       = "📋 Pipeline steps detected: %d",
    generating        = "⚙️  Generating content in format: '%s'",
    dir_created       = "📁 Directory created: %s",
    export_ok         = "✅ Pipeline successfully exported to: %s",
    pipeline_null     = "⚠️  No pipeline provided; attempting context capture.",
    metadata_failed   = "⚠️  Metadata generation failed: %s",
    ext_mismatch      = "⚠️  Expected extension '%s' differs from '%s'.",
    invalid_format    = "Invalid format: '%s'. Use 'script', 'rmarkdown', 'quarto' or 'function'.",
    bad_logical       = "Argument '%s' must be TRUE or FALSE.",
    bad_filepath      = "file_path must be a non-empty string.",
    file_exists       = "File '%s' already exists. Use overwrite = TRUE to overwrite.",
    parse_error       = "Failed to parse pipeline: %s",
    bad_structure     = "Invalid pipeline structure. Missing fields: %s",
    empty_steps       = "Pipeline contains no valid steps.",
    missing_input_params = "⚠️  Input parameters (uf/years/system) not detected.",
    content_error     = "Error generating content: %s",
    empty_content     = "Generated content is empty.",
    write_error       = "Could not write file: %s"
  )
)

#' @keywords internal
.get_msg <- function(lang, key, ...) {
  tmpl <- .MSGS[[lang]][[key]] %||% .MSGS[["en"]][[key]] %||% key
  if (...length() > 0L) sprintf(tmpl, ...) else tmpl
}

#' @keywords internal
.log_info    <- function(lang, key, ...) message(.get_msg(lang, key, ...))
#' @keywords internal
.log_warn    <- function(lang, key, ...) warning(.get_msg(lang, key, ...), call. = FALSE)
#' @keywords internal
.log_success <- function(lang, key, ...) message(.get_msg(lang, key, ...))
#' @keywords internal
.abort       <- function(lang, key, ...) stop(.get_msg(lang, key, ...), call. = FALSE)

# Operador nulo
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0L) a else b
