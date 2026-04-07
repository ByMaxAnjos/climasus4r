
# Create the sus_targets_integration.R file content

sus_targets_integration = '''# ==============================================================================
# sus_targets_integration.R — Integração climasus4r + targets
# Pacote: climasus4r
# ==============================================================================
# Autor:       climasus4r Team
# Versão:      1.0.0
# Última rev.: 2026-04-03
# Descrição:   Sistema de integração entre pipelines climasus4r e o pacote targets.
#              Permite exportar/importar pipelines como targets dinâmicos,
#              criar fábricas de targets parametrizadas, e executar RAPs
#              dentro de workflows targets com cache e reprodutibilidade.
#
# Dependências: targets, tarchetypes, rlang, glue, yaml
# ==============================================================================

# ==============================================================================
# 1. CLASSE S3: tar_rap_object (Extensão de rap_object para targets)
# ==============================================================================

#' @title Objeto RAP para Targets
#' @description Extensão da classe rap_object com capacidades targets,
#'   permitindo integração nativa com pipelines targets.
#' @export
new_tar_rap_object <- function(rap_obj, tar_config = list()) {
  if (!inherits(rap_obj, "rap_object")) {
    stop("Input deve ser um rap_object (use sus_read_rap())", call. = FALSE)
  }
  
  structure(
    list(
      rap = rap_obj,
      config = modifyList(
        list(
          store = "_targets",
          script = "_targets.R",
          workers = 1,
          controller = NULL,
          error = "stop",
          memory = "transient"
        ),
        tar_config
      ),
      targets_meta = list()
    ),
    class = c("tar_rap_object", "rap_object", "list")
  )
}

#' @export
print.tar_rap_object <- function(x, ...) {
  cat(sprintf(
    "<tar_rap_object> [TARGETS-ENABLED]\\n  source : %s\\n  format : %s\\n  type   : %s\\n  steps  : %d\\n  store  : %s\\n  workers: %d\\n",
    x$rap$source,
    x$rap$format %||% "?",
    x$rap$structure$type %||% "?",
    length(x$rap$steps),
    x$config$store,
    x$config$workers
  ))
  invisible(x)
}

# ==============================================================================
# 2. FUNÇÕES DE EXPORTAÇÃO PARA TARGETS
# ==============================================================================

#' Exportar Pipeline como Pipeline Targets
#'
#' @title Exportar RAP como Targets Pipeline
#'
#' @description Converte um pipeline climasus4r em um arquivo `_targets.R`
#'   completo, permitindo execução via `targets::tar_make()`. Suporta
#'   branching dinâmico para múltiplas UFs, anos ou doenças.
#'
#' @param pipeline Expressão de pipeline ou rap_object.
#' @param file_path Caminho para salvar (padrão: "_targets.R").
#' @param branching Lógica de branching: "none", "uf", "year", "cross" (uf×year).
#' @param parallel_workers Número de workers para paralelização.
#' @param include_qc Incluir targets de quality control/validação.
#' @param lang Idioma ("pt" ou "en").
#' @param additional_targets Lista de targets adicionais para incluir.
#' @param overwrite Sobrescrever arquivo existente.
#'
#' @return Invisível: caminho do arquivo criado.
#' @export
#'
#' @examples
#' \\dontrun{
#' # Exportar com branching por UF (executa em paralelo para cada estado)
#' sus_export_targets(
#'   pipeline = rap,
#'   branching = "uf",
#'   parallel_workers = 4
#' )
#' 
#' # Branching cruzado: todas as combinações UF × Ano
#' sus_export_targets(
#'   pipeline = rap,
#'   branching = "cross",
#'   parallel_workers = 8
#' )
#' }
sus_export_targets <- function(
    pipeline = NULL,
    file_path = "_targets.R",
    branching = c("none", "uf", "year", "disease", "cross"),
    parallel_workers = 1,
    include_qc = TRUE,
    lang = "pt",
    additional_targets = list(),
    overwrite = FALSE
) {
  
  branching <- match.arg(branching)
  lang <- match.arg(lang, c("pt", "en"))
  
  # Obter estrutura do pipeline
  if (inherits(pipeline, "rap_object")) {
    rap <- pipeline
    structure_obj <- rap$structure
  } else {
    rap <- sus_read_rap(pipeline, lang = lang, validate = TRUE)
    structure_obj <- rap$structure
  }
  
  # Verificar arquivo
  if (file.exists(file_path) && !overwrite) {
    stop(sprintf("Arquivo '%s' já existe. Use overwrite = TRUE.", file_path))
  }
  
  message(sprintf("🎯 Exportando pipeline '%s' para targets...", structure_obj$type))
  
  # Gerar conteúdo do _targets.R
  content <- .generate_targets_script(
    rap = rap,
    structure = structure_obj,
    branching = branching,
    workers = parallel_workers,
    include_qc = include_qc,
    lang = lang,
    additional_targets = additional_targets
  )
  
  # Escrever arquivo
  writeLines(content, file_path, useBytes = FALSE)
  message(sprintf("✅ Pipeline targets criado: %s", normalizePath(file_path)))
  
  # Criar diretório de dados se necessário
  data_dir <- file.path(dirname(file_path), "data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
    message(sprintf("📁 Diretório de dados criado: %s", data_dir))
  }
  
  invisible(file_path)
}

# ==============================================================================
# 3. FUNÇÕES DE LEITURA E IMPORTAÇÃO
# ==============================================================================

#' Ler RAP como Objeto Targets
#'
#' @title Ler RAP para Targets
#'
#' @description Lê um arquivo RAP e converte em tar_rap_object,
#'   pronto para integração com targets.
#'
#' @param file_path Caminho do arquivo RAP.
#' @param tar_config Configuração do targets (store, workers, etc.).
#' @param lang Idioma.
#' @param validate Validar integridade do RAP.
#'
#' @return Objeto tar_rap_object.
#' @export
sus_read_rap_targets <- function(
    file_path,
    tar_config = list(),
    lang = "pt",
    validate = TRUE
) {
  rap <- sus_read_rap(file_path, lang = lang, validate = validate)
  new_tar_rap_object(rap, tar_config)
}

# ==============================================================================
# 4. FÁBRICAS DE TARGETS (TARGET FACTORIES)
# ==============================================================================

#' Fábrica de Targets para Pipeline climasus4r
#'
#' @title Criar Targets a partir de RAP
#'
#' @description Cria uma lista de targets (para usar em `_targets.R`)
#'   a partir de um rap_object. Permite integração manual em pipelines
#'   targets existentes.
#'
#' @param rap Objeto rap_object ou tar_rap_object.
#' @param name Nome base dos targets (padrão: "climasus_pipeline").
#' @param branching Estratégia de branching.
#' @param include_qc Incluir target de quality control.
#' @param format Formato de armazenamento ("rds", "qs", "parquet", etc.).
#'
#' @return Lista de objetos tar_target.
#' @export
#'
#' @examples
#' \\dontrun{
#' # Em _targets.R:
#' library(targets)
#' library(climasus4r)
#' 
#' rap <- sus_read_rap("meu_pipeline.R")
#' 
#' list(
#'   tar_target(raw_data, read_csv("dados.csv")),
#'   sus_tar_factory(rap, name = "sus_analysis", branching = "uf"),
#'   tar_target(report, render_report(sus_analysis_results))
#' )
#' }
sus_tar_factory <- function(
    rap,
    name = "climasus_pipeline",
    branching = c("none", "uf", "year", "cross"),
    include_qc = TRUE,
    format = "rds"
) {
  
  branching <- match.arg(branching)
  
  if (!inherits(rap, "rap_object")) {
    stop("Input deve ser um rap_object", call. = FALSE)
  }
  
  ip <- rap$structure$input_params
  op <- rap$structure$output_params
  
  targets_list <- list()
  
  # Target 1: Parâmetros de entrada
  targets_list[[paste0(name, "_params")]] <- targets::tar_target_raw(
    name = paste0(name, "_params"),
    command = quote(list(
      uf = !!ip$uf %||% "SP",
      years = !!ip$years %||% 2020L,
      system = !!ip$system %||% "SIM-DO",
      time_unit = !!op$time_unit %||% "month",
      lang = "pt"
    ))
  )
  
  # Target 2: Dados brutos (com ou sem branching)
  data_target_name <- paste0(name, "_data")
  
  if (branching == "uf") {
    # Branching dinâmico por UF
    targets_list[[paste0(name, "_uf_list")]] <- targets::tar_target_raw(
      name = paste0(name, "_uf_list"),
      command = parse(text = sprintf("%s_params$uf", name))
    )
    
    targets_list[[data_target_name]] <- targets::tar_target_raw(
      name = data_target_name,
      command = parse(text = sprintf(
        "climasus4r::sus_data_import(
        uf = %s_uf_list,
        year = %s_params$years,
        system = %s_params$system,
        parallel = TRUE,
        lang = %s_params$lang
      )",
        name, name, name, name
      )),
      pattern = parse(text = sprintf("map(%s_uf_list)", name)),
      iteration = "list"
    )
    
  } else if (branching == "year") {
    # Branching por ano
    targets_list[[paste0(name, "_year_list")]] <- targets::tar_target_raw(
      name = paste0(name, "_year_list"),
      command = parse(text = sprintf("%s_params$years", name))
    )
    
    targets_list[[data_target_name]] <- targets::tar_target_raw(
      name = data_target_name,
      command = parse(text = sprintf(
        "climasus4r::sus_data_import(
        uf = %s_params$uf,
        year = %s_year_list,
        system = %s_params$system,
        parallel = TRUE,
        lang = %s_params$lang
      )",
        name, name, name, name
      )),
      pattern = parse(text = sprintf("map(%s_year_list)", name)),
      iteration = "list"
    )
    
  } else if (branching == "cross") {
    # Branching cruzado UF × Ano
    targets_list[[paste0(name, "_grid")]] <- targets::tar_target_raw(
      name = paste0(name, "_grid"),
      command = parse(text = sprintf(
        "tidyr::crossing(
        uf = %s_params$uf,
        year = %s_params$years
      )",
        name, name
      ))
    )
    
    targets_list[[data_target_name]] <- targets::tar_target_raw(
      name = data_target_name,
      command = parse(text = sprintf(
        "climasus4r::sus_data_import(
        uf = %s_grid$uf,
        year = %s_grid$year,
        system = %s_params$system,
        parallel = FALSE,
        lang = %s_params$lang
      )",
        name, name, name, name
      )),
      pattern = parse(text = sprintf("map(%s_grid)", name)),
      iteration = "list"
    )
    
  } else {
    # Sem branching - importação única
    targets_list[[data_target_name]] <- targets::tar_target_raw(
      name = data_target_name,
      command = parse(text = sprintf(
        "climasus4r::sus_data_import(
        uf = %s_params$uf,
        year = %s_params$years,
        system = %s_params$system,
        parallel = TRUE,
        lang = %s_params$lang
      )",
        name, name, name, name
      ))
    )
  }
  
  # Targets 3+: Processamento (uma por etapa do pipeline)
  prev_target <- data_target_name
  
  for (i in seq_along(rap$steps[-1])) {
    step <- rap$steps[[i + 1]]
    step_name <- paste0(name, "_step_", i, "_", step$function_name)
    
    # Construir chamada da função
    fn <- step$function_name
    params <- step$important_params
    
    if (length(params) > 0) {
      param_str <- paste(
        vapply(names(params), function(k) {
          sprintf("%s = %s", k, deparse(params[[k]]))
        }, character(1)),
        collapse = ", "
      )
      cmd <- sprintf("%s(%s, lang = %s_params$lang)", fn, param_str, name)
    } else {
      cmd <- sprintf("%s(lang = %s_params$lang)", fn, name)
    }
    
    # Se há branching, propagar o pattern
    if (branching != "none" && i == 1) {
      targets_list[[step_name]] <- targets::tar_target_raw(
        name = step_name,
        command = parse(text = sprintf("%s |> %s", prev_target, cmd))),
      pattern = if (branching != "none") parse(text = sprintf("map(%s)", prev_target)) else NULL,
      iteration = "list"
      )
  } else if (branching != "none") {
    targets_list[[step_name]] <- targets::tar_target_raw(
      name = step_name,
      command = parse(text = sprintf("%s |> %s", prev_target, cmd))),
    pattern = parse(text = sprintf("map(%s)", prev_target)),
    iteration = "list"
    )
} else {
  targets_list[[step_name]] <- targets::tar_target_raw(
    name = step_name,
    command = parse(text = sprintf("%s |> %s", prev_target, cmd))
  )
}
    
    prev_target <- step_name
    }
  
  # Target final: Resultado agregado
  final_name <- paste0(name, "_results")
  if (branching != "none") {
    targets_list[[final_name]] <- targets::tar_target_raw(
      name = final_name,
      command = parse(text = sprintf("dplyr::bind_rows(%s)", prev_target)),
      pattern = parse(text = sprintf("map(%s)", prev_target))
    )
  } else {
    targets_list[[final_name]] <- targets::tar_target_raw(
      name = final_name,
      command = parse(text = prev_target)
    )
  }
  
  # Quality Control target (opcional)
  if (include_qc) {
    targets_list[[paste0(name, "_qc")]] <- targets::tar_target_raw(
      name = paste0(name, "_qc"),
      command = parse(text = sprintf(
        "climasus4r::sus_data_quality_report(%s)",
        final_name
      ))
    )
  }
  
  # Retornar como lista nomeada
  structure(targets_list, class = c("sus_tar_pipeline", "list"))
  }

# ==============================================================================
# 5. FUNÇÕES DE EXECUÇÃO E ORQUESTRAÇÃO
# ==============================================================================

#' Executar Pipeline RAP via Targets
#'
#' @title Executar RAP com Targets
#'
#' @description Executa um pipeline RAP usando a infraestrutura do targets,
#'   com cache automático, paralelização e reprodutibilidade.
#'
#' @param rap Objeto rap_object ou caminho para arquivo RAP.
#' @param workers Número de workers paralelos.
#' @param reporter Tipo de relatório de progresso.
#' @param envir Ambiente de execução.
#' @param ... Argumentos adicionais para tar_make().
#'
#' @return Objeto tar_rap_object com metadados de execução.
#' @export
sus_run_targets <- function(
    rap,
    workers = 1,
    reporter = "verbose",
    envir = parent.frame(),
    ...
) {
  
  # Converter para tar_rap_object se necessário
  if (is.character(rap)) {
    rap <- sus_read_rap_targets(rap)
  } else if (inherits(rap, "rap_object") && !inherits(rap, "tar_rap_object")) {
    rap <- new_tar_rap_object(rap)
  }
  
  if (!inherits(rap, "tar_rap_object")) {
    stop("Input deve ser um rap_object, tar_rap_object ou caminho de arquivo")
  }
  
  # Criar script temporário _targets.R
  temp_dir <- tempfile("climasus_targets_")
  dir.create(temp_dir, recursive = TRUE)
  
  targets_file <- file.path(temp_dir, "_targets.R")
  store_dir <- file.path(temp_dir, "_targets")
  
  # Exportar pipeline
  sus_export_targets(
    pipeline = rap$rap,
    file_path = targets_file,
    branching = if (workers > 1) "uf" else "none",
    parallel_workers = workers,
    overwrite = TRUE
  )
  
  # Configurar e executar
  old_wd <- setwd(temp_dir)
  on.exit(setwd(old_wd), add = TRUE)
  
  message("🚀 Iniciando execução targets...")
  
  tryCatch({
    targets::tar_config_set(store = store_dir)
    targets::tar_make(
      script = targets_file,
      reporter = reporter,
      workers = workers,
      ...
    )
    
    # Carregar resultados
    results <- targets::tar_read("climasus_pipeline_results", store = store_dir)
    
    # Atualizar metadados
    rap$targets_meta <- list(
      execution_time = Sys.time(),
      store_path = store_dir,
      workers_used = workers,
      status = "completed"
    )
    
    message("✅ Pipeline concluído com sucesso!")
    
    # Retornar resultados e objeto atualizado
    structure(
      list(
        results = results,
        rap = rap,
        store = store_dir
      ),
      class = "sus_targets_run"
    )
    
  }, error = function(e) {
    rap$targets_meta$status <- "failed"
    rap$targets_meta$error <- conditionMessage(e)
    stop(sprintf("Falha na execução targets: %s", conditionMessage(e)))
  })
}

# ==============================================================================
# 6. FUNÇÕES DE INTERCÂMBIO RÁPIDO (FAST INTERCHANGE)
# ==============================================================================

#' Intercâmbio Rápido de Pipeline
#'
#' @title Fast Pipeline Interchange
#'
#' @description Sistema de intercâmbio rápido que permite:
#'   1. Exportar um RAP como "receita" compacta (.yaml ou .json)
#'   2. Importar e executar imediatamente via targets
#'   3. Versionamento e compartilhamento entre equipes
#'
#' @param rap Objeto rap_object.
#' @param file_path Caminho para salvar a receita (.yaml).
#' @param include_data Incluir hash dos dados de entrada.
#' @param compress Comprimir arquivo.
#'
#' @return Caminho do arquivo criado.
#' @export
sus_export_rap_recipe <- function(
    rap,
    file_path = NULL,
    include_data = TRUE,
    compress = FALSE
) {
  
  if (!inherits(rap, "rap_object")) {
    stop("Input deve ser um rap_object", call. = FALSE)
  }
  
  if (is.null(file_path)) {
    file_path <- paste0("rap_recipe_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".yaml")
  }
  
  recipe <- list(
    version = "1.0",
    created = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    climasus4r_version = as.character(utils::packageVersion("climasus4r")),
    pipeline_type = rap$structure$type,
    parameters = rap$params,
    steps = lapply(rap$steps, function(s) {
      list(
        function_name = s$function_name,
        params = s$important_params
      )
    }),
    metadata = rap$metadata
  )
  
  if (include_data) {
    # Calcular hash dos parâmetros de entrada para rastreabilidade
    input_str <- paste(
      unlist(rap$structure$input_params),
      collapse = "|"
    )
    recipe$data_hash <- digest::digest(input_str, algo = "xxhash32")
  }
  
  yaml::write_yaml(recipe, file_path)
  
  if (compress) {
    zip_file <- paste0(file_path, ".zip")
    zip::zip(zip_file, file_path)
    file.remove(file_path)
    file_path <- zip_file
  }
  
  message(sprintf("📦 Receita exportada: %s", file_path))
  invisible(file_path)
}

#' Importar e Executar Receita RAP
#'
#' @title Importar Receita RAP
#'
#' @description Lê uma receita RAP e cria um pipeline targets pronto
#'   para execução imediata.
#'
#' @param recipe_path Caminho da receita (.yaml ou .zip).
#' @param override_params Parâmetros para sobrescrever.
#' @param execute Executar imediatamente via targets.
#' @param workers Workers para execução.
#'
#' @return Objeto tar_rap_object ou sus_targets_run (se execute=TRUE).
#' @export
sus_import_rap_recipe <- function(
    recipe_path,
    override_params = list(),
    execute = FALSE,
    workers = 1
) {
  
  # Descomprimir se necessário
  if (grepl("\\.zip$", recipe_path)) {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    zip::unzip(recipe_path, exdir = temp_dir)
    yaml_file <- list.files(temp_dir, pattern = "\\.yaml$", full.names = TRUE)[1]
  } else {
    yaml_file <- recipe_path
  }
  
  recipe <- yaml::read_yaml(yaml_file)
  
  # Reconstruir rap_object da receita
  rap <- structure(
    list(
      metadata = recipe$metadata,
      params = modifyList(recipe$parameters, override_params),
      steps = lapply(recipe$steps, function(s) {
        list(
          function_name = s$function_name,
          important_params = s$params,
          arguments = list(),
          line = ""
        )
      }),
      structure = list(
        type = recipe$pipeline_type,
        input_params = recipe$parameters[c("uf", "years", "system")],
        output_params = recipe$parameters[c("time_unit", "group_by")],
        total_steps = length(recipe$steps),
        functions_used = sapply(recipe$steps, function(s) s$function_name)
      ),
      source = recipe_path,
      format = "recipe",
      raw = list()
    ),
    class = "rap_object"
  )
  
  tar_rap <- new_tar_rap_object(rap)
  
  if (execute) {
    return(sus_run_targets(tar_rap, workers = workers))
  }
  
  tar_rap
}

# ==============================================================================
# 7. FUNÇÕES INTERNAS (Helpers)
# ==============================================================================

#' @keywords internal
.generate_targets_script <- function(
    rap, structure, branching, workers, include_qc, lang, additional_targets
) {
  
  ip <- structure$input_params
  op <- structure$output_params
  
  # Header
  header <- c(
    "# Generated by climasus4r::sus_export_targets()",
    sprintf("# Pipeline: %s", structure$type),
    sprintf("# Created: %s", format(Sys.time())),
    "",
    "library(targets)",
    "library(tarchetypes)",
    "library(climasus4r)",
    "library(dplyr)",
    "",
    "# Configure parallel processing",
    sprintf("options(clustermq.scheduler = %s)", 
            if (workers > 1) "\"multicore\"" else "\"sequential\""),
    ""
  )
  
  # Parâmetros globais
  params_block <- c(
    "# Pipeline parameters",
    "list(",
    sprintf("  tar_target(params_uf, %s),", deparse(ip$uf %||% "SP")),
    sprintf("  tar_target(params_years, %s),", deparse(ip$years %||% 2020L)),
    sprintf("  tar_target(params_system, \"%s\"),", ip$system %||% "SIM-DO"),
    sprintf("  tar_target(params_time_unit, \"%s\"),", op$time_unit %||% "month"),
    "  tar_target(params_lang, \"pt\"),",
    ")",
    ""
  )
  
  # Branching setup
  branching_setup <- switch(branching,
                            "uf" = c(
                              "# Branching by UF",
                              "tar_target(uf_branch, params_uf, pattern = map(params_uf))",
                              ""
                            ),
                            "year" = c(
                              "# Branching by Year",
                              "tar_target(year_branch, params_years, pattern = map(params_years))",
                              ""
                            ),
                            "cross" = c(
                              "# Cross branching UF × Year",
                              "tar_target(analysis_grid, tidyr::crossing(uf = params_uf, year = params_years))",
                              ""
                            ),
                            character(0)
  )
  
  # Main pipeline targets
  pipeline_targets <- c(
    "# Main pipeline",
    sprintf("tar_target(%s,", rap$structure$type %||% "climasus_data"),
    "  climasus4r::sus_data_import(",
    if (branching == "uf") "    uf = uf_branch," else "    uf = params_uf,",
    if (branching == "year") "    year = year_branch," else "    year = params_years,",
    "    system = params_system,",
    "    parallel = TRUE,",
    "    lang = params_lang",
    "  )",
    if (branching %in% c("uf", "year", "cross")) sprintf(", pattern = %s", 
                                                         switch(branching,
                                                                "uf" = "map(uf_branch)",
                                                                "year" = "map(year_branch)",
                                                                "cross" = "map(analysis_grid)"
                                                         )) else "",
    ")",
    ""
  )
  
  # QC target
  qc_target <- if (include_qc) c(
    "# Quality control",
    sprintf("tar_target(%s_qc,", rap$structure$type %||% "climasus_data"),
    "  climasus4r::sus_data_quality_report(climasus_data)",
    ")",
    ""
  ) else character(0)
  
  # Additional targets
  additional_block <- if (length(additional_targets) > 0) {
    c(
      "# Additional user-defined targets",
      paste(sapply(additional_targets, deparse), collapse = "\\n"),
      ""
    )
  } else character(0)
  
  # Combine all
  content <- c(
    header,
    params_block,
    branching_setup,
    pipeline_targets,
    qc_target,
    additional_block
  )
  
  paste(content, collapse = "\\n")
}

# ==============================================================================
# 8. MÉTODOS S3 ADICIONAIS
# ==============================================================================

#' @export
summary.tar_rap_object <- function(object, ...) {
  cat("Targets-Enabled RAP Summary\\n")
  cat("============================\\n")
  cat(sprintf("Pipeline: %s\\n", object$rap$structure$type))
  cat(sprintf("Source: %s\\n", object$rap$source))
  cat(sprintf("Steps: %d\\n", length(object$rap$steps)))
  cat(sprintf("Target Store: %s\\n", object$config$store))
  cat(sprintf("Workers: %d\\n", object$config$workers))
  if (length(object$targets_meta) > 0) {
    cat(sprintf("Last Run: %s\\n", object$targets_meta$execution_time %||% "N/A"))
    cat(sprintf("Status: %s\\n", object$targets_meta$status %||% "N/A"))
  }
  invisible(object)
}

# Operador de pipeline para tar_rap_object
#' @export
`|>.tar_rap_object` <- function(lhs, rhs) {
  # Permitir encadeamento de operações
  rhs(lhs)
}

# ==============================================================================
# 9. UTILITÁRIOS
# ==============================================================================

#' Visualizar Pipeline como Grafo
#'
#' @title Visualizar Pipeline Targets
#'
#' @description Cria visualização do pipeline targets usando tar_visnetwork().
#'
#' @param rap Objeto tar_rap_object.
#' @param ... Argumentos para tar_visnetwork().
#'
#' @return Objeto visNetwork.
#' @export
sus_vis_pipeline <- function(rap, ...) {
  if (!inherits(rap, "tar_rap_object")) {
    stop("Input deve ser um tar_rap_object", call. = FALSE)
  }
  
  # Criar script temporário e visualizar
  temp_file <- tempfile(fileext = ".R")
  sus_export_targets(rap$rap, file_path = temp_file, overwrite = TRUE)
  
  targets::tar_visnetwork(script = temp_file, ...)
}

#' Checar Status do Pipeline
#'
#' @title Status do Pipeline Targets
#'
#' @description Verifica status de execução do pipeline em um store targets.
#'
#' @param rap Objeto tar_rap_object ou caminho do store.
#' @return Data frame com status.
#' @export
sus_tar_status <- function(rap) {
  store <- if (inherits(rap, "tar_rap_object")) {
    rap$config$store
  } else {
    rap
  }
  
  if (!dir.exists(store)) {
    return(data.frame(message = "Store não encontrado"))
  }
  
  targets::tar_progress_summary(store = store)
}

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0L) a else b
'''

print("Arquivo sus_targets_integration.R criado com sucesso!")
print(f"Total de linhas: {len(sus_targets_integration.split(chr(10)))}")
