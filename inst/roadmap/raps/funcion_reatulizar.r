#' Criar Função a Partir do Pipeline
create_function_content <- function(structure, metadata, include_validation, doc_level, lang) {
  
  # Identificar parâmetros variáveis
  variable_params <- identify_variable_params(structure)
  
  # Gerar documentação roxygen2
  roxygen <- c(
    "#' @title Executar Pipeline Reprodutível",
    sprintf("#' @description %s", metadata$description),
    "#'",
    "#' @param ... Parâmetros do pipeline:",
    "#'   \\itemize{",
    "#'     \\item{uf}{Unidade federativa (código ou nome)}",
    "#'     \\item{years}{Anos a serem analisados}",
    "#'     \\item{output_dir}{Diretório para salvar resultados}",
    "#'   }",
    "#'",
    "#' @return Dataframe com resultados processados",
    "#' @export",
    "#'",
    "#' @examples",
    "#' \\dontrun{",
    "#' resultados <- run_pipeline(uf = \"SP\", years = 2020:2023)",
    "#' }",
    ""
  )
  
  # Corpo da função
  function_body <- c(
    "run_pipeline <- function(...) {",
    "  # Capturar parâmetros",
    "  params <- list(...)",
    "  ",
    "  # Parâmetros padrão",
    sprintf("  default_params <- list("),
    sprintf("    uf = %s,", deparse(structure$input_params$uf)),
    sprintf("    years = %s,", deparse(structure$input_params$years)),
    "    output_dir = \"resultados\"",
    "  )",
    "  ",
    "  # Combinar parâmetros",
    "  params <- modifyList(default_params, params)",
    "  ",
    "  # Criar diretório de saída",
    "  if (!dir.exists(params$output_dir)) {",
    "    dir.create(params$output_dir, recursive = TRUE)",
    "  }",
    "  ",
    "  # Executar pipeline",
    "  message(\"🚀 Executando pipeline...\")",
    "  ",
    "  df_resultado <- sus_data_import(",
    "    uf = params$uf,",
    "    year = params$years,",
    sprintf("    system = \"%s\",", structure$input_params$system),
    "    parallel = TRUE,",
    "    lang = \"pt\"",
    "  )"
  )
  
  # Adicionar etapas do pipeline
  for (step in structure$steps[-1]) {
    function_body <- c(function_body,
      sprintf("    %%>%% %s(", step$function_name),
      "      # Parâmetros do pipeline original",
      "    )"
    )
  }
  
  function_body <- c(function_body,
    "  ",
    "  # Salvar resultados",
    "  sus_data_export(",
    "    df_resultado,",
    sprintf("    file_path = file.path(params$output_dir, \"resultado_%s.csv\"),", 
            format(Sys.Date(), "%Y%m%d")),
    "    format = \"csv\",",
    "    include_metadata = TRUE",
    "  )",
    "  ",
    "  message(glue::glue(\"✅ Pipeline concluído! Resultados salvos em: {params$output_dir}\"))",
    "  ",
    "  # Retornar resultados",
    "  invisible(df_resultado)",
    "}"
  )
  
  c(roxygen, function_body)
}