#' Criar Conteúdo para Script R
create_script_content <- function(structure, metadata, include_validation, 
                                   doc_level, lang) {
  
  # Template do script
  template <- c(
    "#!/usr/bin/env Rscript",
    "",
    "# ============================================================================",
    "# Pipeline Reprodutível - {pipeline_name}",
    "# ============================================================================",
    "# Autor: {author}",
    "# Data: {date}",
    "# Versão: {version}",
    "# Descrição: {description}",
    "# ============================================================================",
    "",
    "# ----------------------------------------------------------------------------",
    "# 0. Configuração Inicial",
    "# ----------------------------------------------------------------------------",
    "",
    "# Carregar pacotes necessários",
    "library(climasus4r)",
    "library(dplyr)",
    "",
    "# Definir parâmetros do pipeline (EDITAR CONFORME NECESSÁRIO)",
    "params <- list(",
    "  uf = {uf},",
    "  years = {years},",
    "  system = \"{system}\",",
    "  output_dir = \"resultados\",",
    "  lang = \"{lang}\"",
    ")",
    "",
    "# Criar diretório de saída",
    "if (!dir.exists(params$output_dir)) {",
    "  dir.create(params$output_dir, recursive = TRUE)",
    "}",
    "",
    "# ----------------------------------------------------------------------------",
    "# 1. Execução do Pipeline",
    "# ----------------------------------------------------------------------------",
    "",
    "message(\"🚀 Iniciando pipeline...\")",
    "start_time <- Sys.time()",
    "",
    "# Pipeline principal",
    "df_resultado <- sus_data_import(",
    "  uf = params$uf,",
    "  year = params$years,",
    "  system = params$system,",
    "  parallel = TRUE,",
    "  lang = params$lang",
    ") %>%",
    ""
  )
  
  # Adicionar etapas do pipeline
  for (i in 2:length(structure$steps)) {
    step <- structure$steps[[i]]
    step_code <- deparse(step$arguments, width.cutoff = 60)
    step_code <- paste(step_code, collapse = "\n    ")
    
    template <- c(template,
      sprintf("  %s(", step$function_name),
      sprintf("    %s", step_code),
      "  ) %>%",
      ""
    )
  }
  
  # Finalizar pipeline
  template <- c(template,
    "  # Finalizar pipeline",
    "  sus_data_export(",
    "    file_path = file.path(params$output_dir, \"resultado_{date}.csv\"),",
    "    format = \"csv\",",
    "    include_metadata = TRUE,",
    "    lang = params$lang",
    "  )",
    "",
    "# ----------------------------------------------------------------------------",
    "# 2. Validação e Relatório",
    "# ----------------------------------------------------------------------------",
    ""
  )
  
  # Adicionar validação se solicitado
  if (include_validation) {
    template <- c(template,
      "# Gerar relatório de qualidade dos dados",
      "if (requireNamespace(\"ggplot2\", quietly = TRUE)) {",
      "  quality_report <- sus_data_quality_report(df_resultado)",
      "  print(quality_report)",
      "  ",
      "  # Salvar relatório",
      "  write.csv(quality_report, ",
      "            file.path(params$output_dir, \"quality_report.csv\"),",
      "            row.names = FALSE)",
      "}",
      ""
    )
  }
  
  # Finalizar script
  template <- c(template,
    "end_time <- Sys.time()",
    "message(glue::glue(\"✅ Pipeline concluído em {round(difftime(end_time, start_time, units = 'mins'), 2)} minutos\"))",
    "message(glue::glue(\"📁 Resultados salvos em: {params$output_dir}\"))",
    "",
    "# ----------------------------------------------------------------------------",
    "# Informações do Pipeline",
    "# ----------------------------------------------------------------------------",
    "sessionInfo()"
  )
  
  # Substituir placeholders
  template <- glue::glue(paste(template, collapse = "\n"))
  
  as.character(template)
}

#' Criar Conteúdo para RMarkdown/Quarto
create_rmarkdown_content <- function(structure, metadata, include_validation,
                                      doc_level, lang, output_type) {
  
  # YAML header
  yaml <- c(
    "---",
    sprintf("title: \"Pipeline Reprodutível: %s\"", 
            ifelse(!is.null(metadata$name), metadata$name, "Análise Epidemiológica")),
    sprintf("author: \"%s\"", Sys.getenv("USER", "Analista")),
    sprintf("date: \"%s\"", format(Sys.Date(), "%d de %B de %Y")),
    "output:",
    "  html_document:",
    "    toc: true",
    "    toc_float: true",
    "    theme: flatly",
    "    code_folding: show",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)",
    "library(climasus4r)",
    "library(dplyr)",
    "library(ggplot2)",
    "```",
    "",
    "# Introdução",
    "",
    "## Objetivo do Pipeline",
    "",
    metadata$description %||% "Este pipeline foi gerado automaticamente para análise de dados do DATASUS.",
    "",
    "## Parâmetros do Pipeline",
    "",
    "```{r params, echo=FALSE}",
    "params_df <- data.frame(",
    sprintf("  Parâmetro = c('UF', 'Anos', 'Sistema', 'Agregação', 'Grupo'),"),
    sprintf("  Valor = c('%s', '%s', '%s', '%s', '%s')",
      paste(structure$input_params$uf, collapse = ", "),
      paste(structure$input_params$years, collapse = "-"),
      structure$input_params$system,
      structure$output_params$time_unit %||% "N/A",
      paste(structure$output_params$group_by, collapse = ", ") %||% "N/A"
    ),
    ")",
    "knitr::kable(params_df, caption = 'Configuração do Pipeline')",
    "```",
    "",
    "# Execução do Pipeline",
    "",
    "## Código Completo",
    "",
    "```{r pipeline, eval=FALSE}"
  )
  
  # Adicionar código do pipeline
  pipeline_code <- capture_pipeline_code(structure)
  yaml <- c(yaml, pipeline_code, "```")
  
  # Adicionar visualização se for dashboard
  if (output_type == "dashboard" && include_validation) {
    yaml <- c(yaml,
      "",
      "# Resultados e Visualizações",
      "",
      "```{r viz, fig.width=10, fig.height=6}",
      "# Visualização da série temporal",
      "if (ncol(df_resultado) >= 2) {",
      "  df_resultado %>%",
      "    ggplot(aes(x = data, y = n)) +",
      "    geom_line(color = '#2EC4B6', size = 1) +",
      "    geom_point(color = '#E63946', size = 0.5) +",
      "    labs(title = 'Série Temporal',",
      "         x = 'Tempo',",
      "         y = 'Contagem') +",
      "    theme_minimal()",
      "}",
      "```"
    )
  }
  
  # Adicionar relatório de qualidade
  if (include_validation) {
    yaml <- c(yaml,
      "",
      "# Relatório de Qualidade dos Dados",
      "",
      "```{r quality, echo=FALSE}",
      "if (exists('df_resultado')) {",
      "  quality_metrics <- sus_data_quality_report(df_resultado)",
      "  knitr::kable(quality_metrics, caption = 'Métricas de Qualidade')",
      "}",
      "```"
    )
  }
  
  yaml <- c(yaml,
    "",
    "# Informações do Ambiente",
    "",
    "```{r session-info, echo=FALSE}",
    "sessionInfo()",
    "```"
  )
  
  paste(yaml, collapse = "\n")
}

#' Capturar Código do Pipeline
capture_pipeline_code <- function(structure) {
  code_lines <- c(
    "# Carregar dados",
    "df <- sus_data_import(",
    sprintf("  uf = %s,", deparse(structure$input_params$uf)),
    sprintf("  year = %s,", deparse(structure$input_params$years)),
    sprintf("  system = \"%s\",", structure$input_params$system),
    "  parallel = TRUE,",
    "  lang = \"pt\"",
    ")"
  )
  
  # Adicionar etapas subsequentes
  for (step in structure$steps[-1]) {
    code_lines <- c(code_lines,
      sprintf("%%>%%", ""),
      sprintf("  %s(", step$function_name),
      "    # Parâmetros configurados automaticamente",
      "  )"
    )
  }
  
  code_lines <- c(code_lines,
    "%>%",
    "  sus_data_export(",
    "    file_path = \"resultados/analise.csv\",",
    "    format = \"csv\",",
    "    include_metadata = TRUE",
    "  )"
  )
  
  code_lines
}

#' Criar Metadados do Pipeline
create_pipeline_metadata <- function(structure, lang) {
  list(
    name = paste("Pipeline", structure$type),
    description = if (lang == "pt") {
      sprintf("Pipeline para análise de %s com dados do DATASUS", structure$type)
    } else {
      sprintf("Pipeline for %s analysis using DATASUS data", structure$type)
    },
    version = "1.0.0",
    created = Sys.time(),
    package_version = packageVersion("climasus4r"),
    r_version = R.version.string,
    structure = structure
  )
}