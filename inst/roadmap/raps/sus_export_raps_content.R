# ==============================================================================
# sus_export_raps_content.R — Geração de Conteúdo por Formato
# Pacote: climasus4r
# ==============================================================================
# Dependências internas: sus_export_raps.R, sus_export_raps_helpers.R
# ==============================================================================

# ==============================================================================
# 1. SCRIPT R
# ==============================================================================

#' Criar Conteúdo para Script R Autônomo
#'
#' @description
#' Gera um script `.R` completo, com cabeçalho reprodutível, controle de
#' semente, verificação de dependências, validação, execução do pipeline e
#' informações da sessão.
#'
#' @keywords internal
create_script_content <- function(structure, metadata, include_validation,
                                   doc_level, lang, exec_meta = list()) {

  ip      <- structure$input_params
  op      <- structure$output_params
  seed    <- .CLIMASUS_EXPORT_DEFAULTS$random_seed
  date_s  <- exec_meta$exec_date %||% format(Sys.Date())
  r_ver   <- exec_meta$r_version %||% R.version.string
  pkgs    <- exec_meta$packages  %||% list()
  pkg_str <- if (length(pkgs) > 0L) {
    paste(sprintf("# %s: %s", names(pkgs), pkgs), collapse = "\n")
  } else {
    "# (pacotes não verificados)"
  }

  header <- c(
    "#!/usr/bin/env Rscript",
    "# vim: set fileencoding=UTF-8:",
    "",
    "# ============================================================================",
    sprintf("# Pipeline Reprodutível — %s", metadata$name %||% "climasus4r"),
    "# ============================================================================",
    sprintf("# Versão R  : %s", r_ver),
    sprintf("# Data      : %s", date_s),
    sprintf("# Pacote    : climasus4r %s", metadata$package_version %||% "?"),
    sprintf("# Plataforma: %s", exec_meta$platform %||% "?"),
    "# Pacotes carregados no momento da exportação:",
    pkg_str,
    "# ============================================================================",
    ""
  )

  setup <- c(
    "# ── 0. Reprodutibilidade ─────────────────────────────────────────────────────",
    sprintf("set.seed(%dL)  # controle de semente aleatória", seed),
    "",
    "# ── 1. Dependências ──────────────────────────────────────────────────────────",
    .pkg_check_block(c("climasus4r", "dplyr", "glue")),
    "",
    "library(climasus4r)",
    "library(dplyr)",
    "library(glue)",
    ""
  )

  params_block <- c(
    "# ── 2. Parâmetros configuráveis (EDITE AQUI) ─────────────────────────────────",
    "params <- list(",
    sprintf("  uf         = %s,",    deparse(ip$uf    %||% "NULL")),
    sprintf("  years      = %s,",    deparse(ip$years %||% "NULL")),
    sprintf("  system     = \"%s\",", ip$system %||% "SIM-DO"),
    sprintf("  time_unit  = \"%s\",", op$time_unit %||% "month"),
    "  output_dir = \"resultados\",",
    sprintf("  lang       = \"%s\",", lang),
    sprintf("  seed       = %dL", seed),
    ")",
    "",
    "# Garantir diretório de saída",
    "if (!dir.exists(params$output_dir)) {",
    "  dir.create(params$output_dir, recursive = TRUE)",
    "}",
    ""
  )

  pipeline_exec <- c(
    "# ── 3. Execução do pipeline ───────────────────────────────────────────────────",
    "message(\"\\U1F680 Iniciando pipeline...\")",
    "start_time <- proc.time()",
    "",
    "df_resultado <- tryCatch({",
    "",
    paste0("  ", capture_pipeline_code(structure)),
    "",
    "}, error = function(e) {",
    "  stop(glue(\"Falha na execução do pipeline: {conditionMessage(e)}\"), call. = FALSE)",
    "})",
    "",
    "elapsed <- round((proc.time() - start_time)[\"elapsed\"], 1)",
    "message(glue(\"\\u23F1 Pipeline concluído em {elapsed}s\"))",
    ""
  )

  validation_block <- character(0L)
  if (include_validation) {
    validation_block <- c(
      "# ── 4. Validação dos resultados ──────────────────────────────────────────────",
      "stopifnot(",
      "  \"Resultado vazio\"           = nrow(df_resultado) > 0L,",
      "  \"Resultado não é data.frame\" = is.data.frame(df_resultado)",
      ")",
      "",
      "# Verificar completude das colunas-chave",
      "na_counts <- colSums(is.na(df_resultado))",
      "na_pct    <- round(100 * na_counts / nrow(df_resultado), 1)",
      "if (any(na_pct > 50)) {",
      "  warning(\"Colunas com >50% de NAs: \",",
      "          paste(names(na_pct[na_pct > 50]), collapse = \", \"),",
      "          call. = FALSE)",
      "}",
      "message(glue(\"\\u2714 Validação OK: {nrow(df_resultado)} linhas, {ncol(df_resultado)} colunas\"))",
      ""
    )
  }

  export_block <- c(
    "# ── 5. Exportação ────────────────────────────────────────────────────────────",
    "output_file <- file.path(",
    sprintf("  params$output_dir, paste0(\"resultado_%s.csv\")", date_s),
    ")",
    "",
    "sus_data_export(",
    "  df_resultado,",
    "  file_path        = output_file,",
    "  format           = \"csv\",",
    "  include_metadata = TRUE,",
    "  lang             = params$lang",
    ")",
    "message(glue(\"\\U1F4C1 Resultados salvos em: {output_file}\"))",
    ""
  )

  session_block <- c(
    "# ── 6. Informações da sessão (reprodutibilidade) ─────────────────────────────",
    "utils::sessionInfo()",
    ""
  )

  content <- c(header, setup, params_block, pipeline_exec,
                validation_block, export_block, session_block)
  content
}


# ==============================================================================
# 2. RMARKDOWN
# ==============================================================================

#' Criar Conteúdo para Documento RMarkdown
#' @keywords internal
create_rmarkdown_content <- function(structure, metadata, include_validation,
                                      doc_level, lang, output_type, exec_meta = list()) {

  ip     <- structure$input_params
  op     <- structure$output_params
  seed   <- .CLIMASUS_EXPORT_DEFAULTS$random_seed
  name   <- metadata$name %||% "Análise climasus4r"
  date_s <- exec_meta$exec_date %||% format(Sys.Date())

  yaml <- c(
    "---",
    sprintf("title: \"Pipeline Reprodutível: %s\"", name),
    sprintf("author: \"%s\"", Sys.getenv("USER", "Analista")),
    sprintf("date: \"%s\"", date_s),
    "output:",
    "  html_document:",
    "    toc: true",
    "    toc_float: true",
    "    toc_depth: 4",
    "    number_sections: true",
    "    theme: flatly",
    "    highlight: kate",
    "    code_folding: show",
    "params:",
    sprintf("  uf: \"%s\"",         paste(ip$uf    %||% "NULL", collapse = ", ")),
    sprintf("  years: \"%s\"",       paste(ip$years %||% "NULL", collapse = ":")),
    sprintf("  system: \"%s\"",      ip$system     %||% "SIM-DO"),
    sprintf("  time_unit: \"%s\"",   op$time_unit  %||% "month"),
    "  output_dir: \"resultados\"",
    sprintf("  lang: \"%s\"",        lang),
    sprintf("  seed: %d", seed),
    "---",
    ""
  )

  setup_chunk <- c(
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(",
    "  echo    = TRUE,",
    "  warning = FALSE,",
    "  message = FALSE,",
    "  comment = \"#>\"",
    ")",
    sprintf("set.seed(%dL)", seed),
    .pkg_check_block(c("climasus4r", "dplyr", "ggplot2", "glue")),
    "library(climasus4r)",
    "library(dplyr)",
    "library(ggplot2)",
    "library(glue)",
    "```",
    ""
  )

  intro <- c(
    "# Sobre este Documento",
    "",
    sprintf("> **Gerado automaticamente em %s** pelo pacote `climasus4r`.", date_s),
    sprintf("> R %s | climasus4r %s",
            exec_meta$r_version %||% R.version.string,
            metadata$package_version %||% "?"),
    "",
    metadata$description %||% "Este documento descreve um pipeline reprodutível do DATASUS.",
    "",
    "## Parâmetros do Pipeline",
    "",
    "```{r params-table, echo=FALSE}",
    "knitr::kable(",
    "  data.frame(",
    sprintf("    Parâmetro = c(\"UF\", \"Anos\", \"Sistema\", \"Agregação\", \"Idioma\", \"Semente\"),"),
    sprintf("    Valor     = c(\"%s\", \"%s\", \"%s\", \"%s\", \"%s\", \"%d\")",
            paste(ip$uf    %||% "N/A", collapse = ", "),
            paste(ip$years %||% "N/A", collapse = ":"),
            ip$system    %||% "N/A",
            op$time_unit %||% "N/A",
            lang,
            seed),
    "  ),",
    "  caption = \"Configuração do Pipeline\"",
    ")",
    "```",
    ""
  )

  pipeline_section <- c(
    "# Execução do Pipeline",
    "",
    "```{r pipeline}",
    "df_resultado <- tryCatch({",
    "",
    paste0("  ", capture_pipeline_code(structure)),
    "",
    "}, error = function(e) {",
    "  knitr::knit_exit()",
    "  stop(conditionMessage(e))",
    "})",
    "```",
    ""
  )

  validation_section <- character(0L)
  if (include_validation) {
    validation_section <- c(
      "# Validação dos Dados",
      "",
      "```{r validation}",
      "stopifnot(",
      "  \"Resultado vazio\" = nrow(df_resultado) > 0L",
      ")",
      "",
      "na_pct <- round(100 * colMeans(is.na(df_resultado)), 1)",
      "knitr::kable(",
      "  data.frame(Coluna = names(na_pct), `NA (%)` = na_pct, check.names = FALSE),",
      "  caption = \"Completude das colunas\"",
      ")",
      "```",
      ""
    )
  }

  viz_section <- character(0L)
  if (output_type %in% c("dashboard", "report") && include_validation) {
    viz_section <- c(
      "# Visualização",
      "",
      "```{r viz, fig.width=10, fig.height=5}",
      "if (all(c(\"data\", \"n\") %in% names(df_resultado))) {",
      "  ggplot(df_resultado, aes(x = data, y = n)) +",
      "    geom_line(colour = \"#2EC4B6\", linewidth = 0.9) +",
      "    geom_point(colour = \"#E63946\", size = 1.2) +",
      "    labs(",
      sprintf("      title    = \"Série Temporal — %s\",", name),
      "      subtitle = glue(\"Período: {min(df_resultado$data, na.rm=TRUE)} a {max(df_resultado$data, na.rm=TRUE)}\"),",
      "      x = \"Data\", y = \"Contagem\"",
      "    ) +",
      "    theme_minimal(base_size = 13)",
      "}",
      "```",
      ""
    )
  }

  session_section <- c(
    "# Informações da Sessão",
    "",
    "```{r session-info, echo=FALSE}",
    "utils::sessionInfo()",
    "```",
    ""
  )

  c(yaml, setup_chunk, intro, pipeline_section,
    validation_section, viz_section, session_section)
}


# ==============================================================================
# 3. QUARTO
# ==============================================================================

#' Criar Conteúdo para Documento Quarto
#' @keywords internal
create_quarto_content <- function(structure, metadata, include_validation,
                                   doc_level, lang, output_type, exec_meta = list()) {

  ip     <- structure$input_params
  op     <- structure$output_params
  seed   <- .CLIMASUS_EXPORT_DEFAULTS$random_seed
  name   <- metadata$name %||% "Análise climasus4r"
  date_s <- exec_meta$exec_date %||% format(Sys.Date())

  yaml <- c(
    "---",
    sprintf("title: \"Pipeline Reprodutível: %s\"", name),
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
    sprintf("  uf: \"%s\"",       paste(ip$uf    %||% "NULL", collapse = ", ")),
    sprintf("  years: \"%s\"",    paste(ip$years %||% "NULL", collapse = ":")),
    sprintf("  system: \"%s\"",   ip$system    %||% "SIM-DO"),
    sprintf("  time_unit: \"%s\"", op$time_unit %||% "month"),
    "  output_dir: \"resultados\"",
    sprintf("  lang: \"%s\"", lang),
    sprintf("  seed: %d", seed),
    "---",
    ""
  )

  setup_chunk <- c(
    "```{r}",
    "#| label: setup",
    "#| include: false",
    sprintf("set.seed(%dL)", seed),
    .pkg_check_block(c("climasus4r", "dplyr", "ggplot2", "glue")),
    "library(climasus4r)",
    "library(dplyr)",
    "library(ggplot2)",
    "library(glue)",
    "```",
    ""
  )

  intro <- c(
    "## Sobre",
    "",
    sprintf("> Gerado em **%s** | R `%s` | climasus4r `%s`",
            date_s,
            exec_meta$r_version %||% R.version.string,
            metadata$package_version %||% "?"),
    "",
    metadata$description %||% "Pipeline reprodutível do DATASUS.",
    ""
  )

  pipeline_section <- c(
    "## Pipeline",
    "",
    "```{r}",
    "#| label: pipeline",
    "df_resultado <- tryCatch({",
    "",
    paste0("  ", capture_pipeline_code(structure)),
    "",
    "}, error = \\(e) stop(conditionMessage(e)))",
    "```",
    ""
  )

  session_section <- c(
    "## Sessão",
    "",
    "```{r}",
    "#| label: session-info",
    "#| echo: false",
    "utils::sessionInfo()",
    "```",
    ""
  )

  c(yaml, setup_chunk, intro, pipeline_section, session_section)
}


# ==============================================================================
# 4. FUNÇÃO ENCAPSULADA
# ==============================================================================

#' Criar Conteúdo para Função Reutilizável
#' @keywords internal
create_function_content <- function(structure, metadata, include_validation,
                                     doc_level, lang) {

  ip      <- structure$input_params
  seed    <- .CLIMASUS_EXPORT_DEFAULTS$random_seed
  pkg_ver <- metadata$package_version %||% "?"
  r_ver   <- metadata$r_version       %||% R.version.string

  roxygen <- c(
    sprintf("# climasus4r %s | %s | %s", pkg_ver, r_ver, metadata$created_date %||% format(Sys.Date())),
    "",
    "#' @title Executar Pipeline Reprodutível",
    sprintf("#' @description %s", metadata$description %||% "Pipeline gerado pelo climasus4r."),
    "#'",
    "#' @param uf `character` — UF(s) para análise.",
    "#' @param years `integer` — Anos a serem processados.",
    "#' @param output_dir `character` — Diretório para salvar resultados.",
    "#' @param time_unit `character` — Unidade de agregação temporal.",
    "#' @param lang `character` — Idioma (`\"pt\"` ou `\"en\"`).",
    "#' @param seed `integer` — Semente aleatória (padrão: 42).",
    "#' @param verbose `logical` — Exibir mensagens de progresso.",
    "#'",
    "#' @return `data.frame` com resultados processados (invisível).",
    "#' @export",
    "#'",
    "#' @examples",
    "#' \\dontrun{",
    sprintf("#' resultados <- run_pipeline(uf = %s, years = %s)",
            deparse(ip$uf    %||% "\"SP\""),
            deparse(ip$years %||% 2020L)),
    "#' }",
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
    "  # ── Validação de argumentos ─────────────────────────────────────────────",
    "  stopifnot(",
    "    \"uf deve ser character\"    = is.character(uf),",
    "    \"years deve ser numérico\"  = is.numeric(years) || is.integer(years),",
    "    \"output_dir deve ser string\" = is.character(output_dir)",
    "  )",
    "",
    "  # ── Reprodutibilidade ────────────────────────────────────────────────────",
    "  set.seed(seed)",
    "",
    "  # ── Parâmetros internos ──────────────────────────────────────────────────",
    "  params <- list(",
    sprintf("    uf = uf, years = years, system = \"%s\",", ip$system %||% "SIM-DO"),
    "    output_dir = output_dir, time_unit = time_unit, lang = lang",
    "  )",
    "",
    "  if (!dir.exists(params$output_dir))",
    "    dir.create(params$output_dir, recursive = TRUE)",
    "",
    "  if (verbose) message(\"\\U1F680 Executando pipeline...\")",
    "  start_time <- proc.time()",
    "",
    "  # ── Pipeline ─────────────────────────────────────────────────────────────",
    "  df_resultado <- tryCatch({",
    "",
    paste0("    ", capture_pipeline_code(structure)),
    "",
    "  }, error = function(e) {",
    "    stop(paste(\"Falha no pipeline:\", conditionMessage(e)), call. = FALSE)",
    "  })",
    "",
    if (include_validation) {
      c(
        "  # ── Validação ───────────────────────────────────────────────────────────",
        "  stopifnot(",
        "    \"Resultado vazio\" = is.data.frame(df_resultado) && nrow(df_resultado) > 0L",
        "  )",
        ""
      )
    },
    "  elapsed <- round((proc.time() - start_time)[\"elapsed\"], 1)",
    "  if (verbose)",
    "    message(glue::glue(\"\\u2705 Concluído em {elapsed}s | {nrow(df_resultado)} linhas\"))",
    "",
    "  invisible(df_resultado)",
    "}"
  )

  c(roxygen, body)
}


# ==============================================================================
# Utilitários de geração de código
# ==============================================================================

#' Bloco de verificação de pacotes instalados
#' @keywords internal
.pkg_check_block <- function(pkgs) {
  lines <- c(
    "# Verificar dependências",
    "required_pkgs <- c(",
    sprintf("  %s", paste(sprintf("\"%s\"", pkgs), collapse = ", ")),
    ")",
    "missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace,",
    "                                       quietly = TRUE, FUN.VALUE = logical(1L))]",
    "if (length(missing_pkgs) > 0L) {",
    "  stop(paste(\"Pacotes ausentes:\", paste(missing_pkgs, collapse = \", \")),",
    "       call. = FALSE)",
    "}",
    ""
  )
  lines
}
