#' Exportar Pipeline Reprodutível
#'
#' Exporta um pipeline analítico como um script R autônomo, com metadados,
#' documentação e estrutura para fácil reutilização.
#'
#' @param pipeline Objeto pipeline (lista com etapas) ou expressão pipe
#' @param file_path Caminho para salvar o arquivo
#' @param format Formato de saída: "script", "rmarkdown", "quarto", "function"
#' @param include_metadata Incluir metadados (TRUE/FALSE)
#' @param include_validation Incluir validação de dados (TRUE/FALSE)
#' @param include_documentation Nível de documentação: "minimal", "standard", "comprehensive"
#' @param lang Idioma: "pt" ou "en"
#' @param output_type Tipo de saída: "analysis", "dashboard", "report"
#'
#' @export
sus_export_raps <- function(
    pipeline = NULL,
    file_path = NULL,
    format = c("script", "rmarkdown", "quarto", "function"),
    include_metadata = TRUE,
    include_validation = TRUE,
    include_documentation = "standard",
    lang = "pt",
    output_type = "analysis"
) {
  
  # Validação inicial
  format <- match.arg(format)
  
  # Capturar pipeline se não fornecido explicitamente
  if (is.null(pipeline)) {
    pipeline <- rlang::enquo(pipeline)
  }
  
  # Extrair estrutura do pipeline
  pipeline_structure <- extract_pipeline_structure(pipeline)
  
  # Preparar metadados
  metadata <- if (include_metadata) {
    create_pipeline_metadata(pipeline_structure, lang)
  }
  
  # Criar conteúdo baseado no formato
  content <- switch(format,
    "script" = create_script_content(pipeline_structure, metadata, 
                                      include_validation, include_documentation, lang),
    "rmarkdown" = create_rmarkdown_content(pipeline_structure, metadata,
                                           include_validation, include_documentation, lang,
                                           output_type),
    "quarto" = create_quarto_content(pipeline_structure, metadata,
                                     include_validation, include_documentation, lang,
                                     output_type),
    "function" = create_function_content(pipeline_structure, metadata,
                                         include_validation, include_documentation, lang)
  )
  
  # Escrever arquivo
  if (!is.null(file_path)) {
    writeLines(content, file_path)
    message(glue::glue("✅ Pipeline exportado com sucesso para: {file_path}"))
    
    # Retornar invisivelmente o conteúdo
    invisible(content)
  } else {
    return(content)
  }
}