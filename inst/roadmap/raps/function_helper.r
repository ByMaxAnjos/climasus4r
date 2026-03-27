#' Extrair Estrutura do Pipeline
#'
#' Analisa o pipeline e extrai informações sobre as funções usadas,
#' parâmetros, ordem de execução, etc.
extract_pipeline_structure <- function(pipeline_expr) {
  # Converte a expressão do pipe em uma lista de etapas
  steps <- list()
  
  # Função recursiva para desmembrar pipes
  parse_pipe <- function(expr, steps_list) {
    if (is.call(expr) && (expr[[1]] == quote(`%>%`) || expr[[1]] == quote(`|>`))) {
      # É um pipe, processa a parte esquerda e direita
      steps_list <- parse_pipe(expr[[2]], steps_list)
      steps_list <- parse_pipe(expr[[3]], steps_list)
    } else if (is.call(expr)) {
      # É uma chamada de função
      func_name <- as.character(expr[[1]])
      args <- as.list(expr[-1])
      
      # Extrai parâmetros importantes
      params <- extract_important_params(func_name, args)
      
      steps_list <- c(steps_list, list(list(
        function_name = func_name,
        arguments = args,
        important_params = params,
        line = deparse(expr)[1]
      )))
    }
    steps_list
  }
  
  steps <- parse_pipe(pipeline_expr, list())
  
  # Identificar tipo de pipeline
  pipeline_type <- identify_pipeline_type(steps)
  
  # Extrair parâmetros de entrada (UF, anos, sistema)
  input_params <- extract_input_parameters(steps)
  
  # Extrair parâmetros de saída (agregações, filtros)
  output_params <- extract_output_parameters(steps)
  
  list(
    steps = steps,
    type = pipeline_type,
    input_params = input_params,
    output_params = output_params,
    total_steps = length(steps),
    functions_used = unique(sapply(steps, function(x) x$function_name))
  )
}

#' Identificar Tipo de Pipeline Baseado nas Funções Usadas
identify_pipeline_type <- function(steps) {
  funcs <- sapply(steps, function(x) x$function_name)
  
  if (any(grepl("filter_cid", funcs))) {
    cid_groups <- extract_cid_groups(steps)
    return(paste("Mortalidade por", paste(cid_groups, collapse = " e ")))
  } else if (any(grepl("SINAN", funcs))) {
    return("Morbidade (SINAN)")
  } else if (any(grepl("heat", funcs, ignore.case = TRUE))) {
    return("Eventos Climáticos Extremos")
  } else {
    return("Pipeline Genérico")
  }
}

#' Extrair Parâmetros Importantes
extract_important_params <- function(func_name, args) {
  params <- list()
  
  # Mapeamento de funções para parâmetros importantes
  important_params_map <- list(
    "sus_data_import" = c("uf", "year", "system"),
    "sus_data_filter_cid" = c("disease_group"),
    "sus_data_filter_demographics" = c("age_range"),
    "sus_data_aggregate" = c("time_unit", "group_by"),
    "sus_create_variables" = c("create_age_groups", "create_calendar_vars")
  )
  
  if (func_name %in% names(important_params_map)) {
    for (param in important_params_map[[func_name]]) {
      if (param %in% names(args)) {
        params[[param]] <- args[[param]]
      }
    }
  }
  
  params
}

#' Extrair Parâmetros de Entrada do Pipeline
extract_input_parameters <- function(steps) {
  import_step <- NULL
  for (step in steps) {
    if (step$function_name == "sus_data_import") {
      import_step <- step
      break
    }
  }
  
  if (!is.null(import_step)) {
    list(
      uf = import_step$important_params$uf,
      years = import_step$important_params$year,
      system = import_step$important_params$system
    )
  } else {
    list()
  }
}

#' Extrair Parâmetros de Saída do Pipeline
extract_output_parameters <- function(steps) {
  aggregate_step <- NULL
  for (step in steps) {
    if (step$function_name == "sus_data_aggregate") {
      aggregate_step <- step
      break
    }
  }
  
  if (!is.null(aggregate_step)) {
    list(
      time_unit = aggregate_step$important_params$time_unit,
      group_by = aggregate_step$important_params$group_by
    )
  } else {
    list()
  }
}