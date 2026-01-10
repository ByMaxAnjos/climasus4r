#' Interactive Census Variables Explorer
#'
#' Opens an interactive HTML interface to explore Brazilian Census variables
#' and copy codes for use with sus_socio_add_census().
#'
#' @param dataset Character string specifying the census dataset. Options:
#'   \itemize{
#'     \item `"all"` - All datasets (default)
#'     \item `"population"` - Population microdata
#'     \item `"households"` - Household microdata
#'     \item `"families"` - Family microdata
#'     \item `"mortality"` - Mortality microdata
#'     \item `"emigration"` - Emigration microdata
#'     \item `"tracts"` - Census tract aggregate data
#'   }
#' @param year Integer specifying census year. Options: `2010` (default) or `2000`.
#' @param lang Character string specifying language. Options: `"pt"` (Portuguese,
#'   default), `"en"` (English), `"es"` (Spanish).
#' @param output Character string specifying output format. Options:
#'   \itemize{
#'     \item `"browser"` - Interactive HTML in web browser (default)
#'     \item `"console"` - Simple list in R console
#'     \item `"codes"` - Returns only variable codes as character vector
#'   }
#' @param verbose Logical. If `TRUE` (default), prints informative messages.
#'
#' @return Depending on `output`:
#'   - `"browser"`: Opens HTML interface, returns invisible data.frame
#'   - `"console"`: Prints summary, returns invisible data.frame
#'   - `"codes"`: Returns character vector of variable codes
#'
#' @details
#' This function helps users discover which census variables are available.
#' The interactive interface allows easy copying of variable codes for use
#' in `sus_socio_add_census(vars = ...)`.
#'
#' @examples
#' \dontrun{
#' # Open interactive explorer for all datasets
#' sus_census_explore()
#'
#' # Explore only population variables
#' sus_census_explore(dataset = "population")
#'
#' # Get variable codes for programmatic use
#' vars <- sus_census_explore(
#'   dataset = "population",
#'   output = "codes",
#'   lang = "en"
#' )
#'
#' # Use in sus_socio_add_census
#' data <- sus_socio_add_census(
#'   df = my_data,
#'   vars = vars,
#'   dataset = "population",
#'   year = 2010
#' )
#' }
#'
#' @export
#' @importFrom cli cli_h1 cli_h2 cli_h3 cli_alert_info cli_alert_success 
#' @importFrom cli cli_alert_warning cli_rule cli_bullets
sus_census_explore <- function(dataset = "all",
                                year = 2010,
                                lang = "pt",
                                output = "browser",
                                verbose = TRUE) {
  
  # ==========================================================================
  # 1. VALIDACAO DOS PARAMETROS
  # ==========================================================================
  
  valid_datasets <- c("all", "population", "households", "families", 
                     "mortality", "emigration", "tracts")
  if (!dataset %in% valid_datasets) {
    cli::cli_abort("Invalid dataset. Options: {paste(valid_datasets, collapse = ', ')}")
  }
  
  valid_years <- c(2000, 2010)
  if (!year %in% valid_years) {
    cli::cli_abort("Invalid year. Options: {paste(valid_years, collapse = ', ')}")
  }
  
  valid_langs <- c("pt", "en", "es")
  if (!lang %in% valid_langs) {
    cli::cli_abort("Invalid language. Options: {paste(valid_langs, collapse = ', ')}")
  }
  
  valid_outputs <- c("browser", "console", "codes")
  if (!output %in% valid_outputs) {
    cli::cli_abort("Invalid output. Options: {paste(valid_outputs, collapse = ', ')}")
  }
  
  # ==========================================================================
  # 2. CARREGAR DICIONARIOS
  # ==========================================================================
  
  if (verbose) {
    cli::cli_alert_info("Loading census dictionaries...")
  }
  
  # Mapeamento das funcoes por dataset e idioma
  dict_functions <- list(
    population = list(
      pt = "get_census_dictionary_pt_population",
      en = "get_census_dictionary_en_population", 
      es = "get_census_dictionary_es_population"
    ),
    households = list(
      pt = "get_translate_dictionary_pt_households",
      en = "get_translate_dictionary_en_households",
      es = "get_translate_dictionary_es_households"
    ),
    families = list(
      pt = "get_translate_dictionary_pt_families",
      en = "get_translate_dictionary_en_families",
      es = "get_translate_dictionary_es_families"
    ),
    mortality = list(
      pt = "get_translate_dictionary_pt_mortality",
      en = "get_translate_dictionary_en_mortality",
      es = "get_translate_dictionary_es_mortality"
    ),
    emigration = list(
      pt = "get_translate_dictionary_pt_emigration",
      en = "get_translate_dictionary_en_emigration",
      es = "get_translate_dictionary_es_emigration"
    )
  )
  
  # Funcao para carregar um dicionario
  load_dict <- function(dataset_name, lang_code) {
    func_name <- dict_functions[[dataset_name]][[lang_code]]
    if (exists(func_name, mode = "function")) {
      tryCatch({
        return(do.call(func_name, list()))
      }, error = function(e) {
        cli::cli_alert_warning("Could not load {func_name}: {e$message}")
        return(NULL)
      })
    } else {
      cli::cli_alert_warning("Function {func_name} not found")
      return(NULL)
    }
  }
  
  # Determinar quais datasets carregar
  if (dataset == "all") {
    datasets_to_load <- names(dict_functions)
  } else {
    datasets_to_load <- dataset
  }
  
  # Lista para armazenar todos os dados
  all_vars <- list()
  
  for (ds in datasets_to_load) {
    dict_data <- load_dict(ds, lang)
    
    if (!is.null(dict_data) && !is.null(dict_data$columns)) {
      # Criar data.frame com informacoes basicas
      df <- data.frame(
        code = names(dict_data$columns),
        name = as.character(dict_data$columns),
        dataset = ds,
        stringsAsFactors = FALSE
      )
      
      # Adicionar tipo da variavel baseado no codigo
      df$type <- ifelse(grepl("^M", df$code), "imputation_marker", "regular_variable")
      
      # Adicionar categoria baseada no nome
      df$category <- categorize_variable(df$name, lang)
      
      # Adicionar valores possiveis se existirem
      df$has_categories <- sapply(df$code, function(c) {
        c %in% names(dict_data$values)
      })
      
      # Contar valores possiveis
      df$n_categories <- sapply(df$code, function(c) {
        if (c %in% names(dict_data$values)) {
          return(length(dict_data$values[[c]]))
        } else {
          return(0)
        }
      })
      
      all_vars[[ds]] <- df
      
      if (verbose) {
        cli::cli_alert_success("Loaded {nrow(df)} variables from {ds} dataset")
      }
    } else if (verbose) {
      cli::cli_alert_warning("No variables found for dataset: {ds}")
    }
  }
  
  # Combinar todos os datasets
  if (length(all_vars) == 0) {
    cli::cli_abort("No variables found for the specified parameters")
  }
  
  vars_df <- do.call(rbind, all_vars)
  rownames(vars_df) <- NULL
  
  # Adicionar ano
  vars_df$year <- year
  
  # Ordenar por dataset e codigo
  vars_df <- vars_df[order(vars_df$dataset, vars_df$code), ]
  
  # ==========================================================================
  # 3. GERAR SAIDA
  # ==========================================================================
  
  if (output == "codes") {
    # Retornar apenas os codigos
    codes <- unique(vars_df$code)
    
    if (verbose) {
      cli::cli_h1("Census Variables - Codes Only")
      cli::cli_rule()
      cli::cli_alert_info("Found {length(codes)} variables")
      cli::cli_alert_info("Dataset(s): {paste(unique(vars_df$dataset), collapse = ', ')}")
      cli::cli_rule()
      
      # Mostrar primeiros 10 codigos
      if (length(codes) > 0) {
        cli::cli_alert_info("First 10 codes:")
        for (i in 1:min(10, length(codes))) {
          cli::cli_bullets(c("*" = "{.code {codes[i]}}"))
        }
        if (length(codes) > 10) {
          cli::cli_alert_info("... and {length(codes) - 10} more")
        }
      }
    }
    
    return(codes)
  }
  
  if (output == "console") {
    # Saida formatada no console
    if (verbose) {
      cli::cli_h1("Brazilian Census Variables Explorer")
      cli::cli_h2("Census {year}")
      cli::cli_rule()
      
      cli::cli_alert_info("Total variables: {nrow(vars_df)}")
      cli::cli_alert_info("Datasets: {paste(unique(vars_df$dataset), collapse = ', ')}")
      cli::cli_alert_info("Language: {lang}")
      cli::cli_rule()
      
      # Resumo por dataset
      for (ds in unique(vars_df$dataset)) {
        ds_vars <- vars_df[vars_df$dataset == ds, ]
        n_total <- nrow(ds_vars)
        n_categorical <- sum(ds_vars$has_categories)
        
        cli::cli_h3("{toupper(ds)} Dataset")
        cli::cli_bullets(c(
          "*" = "Total variables: {n_total}",
          "*" = "Categorical variables: {n_categorical}",
          "*" = "Imputation markers: {sum(ds_vars$type == 'imputation_marker')}"
        ))
        
        # Mostrar categorias presentes
        cats <- table(ds_vars$category)
        if (length(cats) > 0) {
          cli::cli_alert_info("Categories:")
          for (cat in names(cats)) {
            cli::cli_bullets(c(" " = "{cat}: {cats[cat]}"))
          }
        }
        
        # Mostrar primeiras 5 variaveis como exemplo
        if (n_total > 0) {
          cli::cli_alert_info("Sample variables:")
          for (i in 1:min(5, n_total)) {
            var <- ds_vars[i, ]
            has_cat <- ifelse(var$has_categories, 
                             paste0(" (", var$n_categories, " categories)"), 
                             "")
            cli::cli_bullets(c(
              " " = "{.code {var$code}}: {var$name}{has_cat}"
            ))
          }
          if (n_total > 5) {
            cli::cli_alert_info("... and {n_total - 5} more variables")
          }
        }
        
        cli::cli_rule()
      }
      
      cli::cli_alert_info("Use {.code output = 'codes'} to get all variable codes")
      cli::cli_alert_info("Use {.code output = 'browser'} for interactive interface")
    }
    
    return(invisible(vars_df))
  }
  
  if (output == "browser") {
    # Interface HTML interativa
    html_content <- generate_forest_theme_html(vars_df, year, lang, dataset)
    
    # Salvar arquivo temporario
    html_file <- tempfile(fileext = ".html")
    writeLines(html_content, html_file, useBytes = TRUE)
    
    # Abrir no navegador
    utils::browseURL(html_file)
    
    if (verbose) {
      cli::cli_h1("Interactive Census Explorer")
      cli::cli_alert_success("Interface opened in browser!")
      cli::cli_bullets(c(
        "*" = "Total variables: {nrow(vars_df)}",
        "*" = "Temporary file: {html_file}",
        "*" = "Language: {lang}"
      ))
      cli::cli_rule()
      cli::cli_alert_info("Usage tips:")
      cli::cli_bullets(c(
        "1" = "Click on rows to select variables",
        "2" = "Use Ctrl+Click for multiple selection",
        "3" = "Click 'Copy Codes' to get R code for {.code sus_socio_add_census()}",
        "4" = "Export to CSV for external analysis"
      ))
    }
    
    return(invisible(vars_df))
  }
}

#' Categorizar variaveis baseado no nome
#' @keywords internal
#' @noRd
categorize_variable <- function(var_names, lang) {
  
  # Definir padroes de busca por categoria
  patterns <- list(
    demographics = c("idade", "age", "edad", "sexo", "sex", "raca", "race", 
                    "cor", "color", "nascimento", "birth", "nacimiento",
                    "parentesco", "kinship", "parentesco"),
    education = c("escola", "educ", "alfabetiz", "literacy", "instrucao",
                 "instruction", "instruccion", "curso", "course", "curso",
                 "serie", "grade", "grado"),
    income = c("renda", "income", "ingreso", "salario", "wage", "salario",
               "beneficio", "benefit", "beneficio", "aposentadoria", 
               "pension", "pension"),
    housing = c("domicilio", "household", "vivienda", "agua", "water", "agua",
                "esgoto", "sewer", "alcantarillado", "energia", "energy", 
                "energia", "lixo", "garbage", "basura", "banheiro", "bathroom"),
    health = c("saude", "health", "salud", "deficiencia", "disability", 
               "discapacidad", "enxergar", "see", "ver", "ouvir", "hear", 
               "oir", "caminhar", "walk", "caminar", "morte", "death", 
               "muerte", "filho", "child", "hijo"),
    migration = c("migracao", "migration", "migracion", "nasceu", "born",
                  "nacio", "residencia", "residence", "residencia", 
                  "moradia", "dwelling", "vivienda", "fixou", "settled"),
    geography = c("municipio", "municipality", "municipio", "uf", "state",
                  "estado", "regiao", "region", "region", "area", "area"),
    work = c("trabalho", "work", "trabajo", "ocupacao", "occupation",
             "ocupacion", "emprego", "employment", "empleo", "profissao",
             "profession", "profesion")
  )
  
  # Funcao para determinar categoria
  find_category <- function(name) {
    name_lower <- tolower(name)
    
    for (cat in names(patterns)) {
      for (pattern in patterns[[cat]]) {
        if (grepl(pattern, name_lower)) {
          return(cat)
        }
      }
    }
    return("other")
  }
  
  return(sapply(var_names, find_category))
}

#' Gerar interface HTML com tema Clima Forest
#' @keywords internal
#' @noRd
generate_forest_theme_html <- function(vars_df, year, lang, selected_dataset) {
  
  # Textos multilíngue (sem acentos)
  texts <- list(
    pt = list(
      title = "Explorador de Variaveis do Censo Brasileiro",
      subtitle = "Selecione variaveis para usar em sus_socio_add_census()",
      total_vars = "Total de variaveis",
      showing = "Mostrando",
      dataset_label = "Dataset",
      code_label = "Codigo",
      name_label = "Nome",
      category_label = "Categoria",
      type_label = "Tipo",
      values_label = "Valores",
      copy_btn = "\U0001F4CB  Copiar Codigos Selecionados",
      copy_all_btn = "\U0001F4CB  Copiar Todos os Codigos",
      export_btn = "\U0001F4BE Exportar CSV",
      tips_title = "\U0001F4A1 Como usar:",
      tip1 = "Selecione as variaveis clicando nas linhas (use Ctrl+Clique para multiplas)",
      tip2 = "Clique em 'Copiar Codigos' para copiar os codigos no formato correto",
      tip3 = "Use os codigos copiados no argumento 'vars' de sus_socio_add_census()",
      tip4 = paste("Censo", year, "- Interface gerada por climasus4r"),
      no_selection = "Nenhuma variavel selecionada. Clique nas linhas da tabela.",
      regular_var = "Variavel regular",
      imputation_var = "Marcador de imputacao",
      categorical_var = "Variavel categorica"
    ),
    en = list(
      title = "Brazilian Census Variables Explorer",
      subtitle = "Select variables to use in sus_socio_add_census()",
      total_vars = "Total variables",
      showing = "Showing",
      dataset_label = "Dataset",
      code_label = "Code",
      name_label = "Name",
      category_label = "Category",
      type_label = "Type",
      values_label = "Values",
      copy_btn = "\U0001F4CB  Copy Selected Codes",
      copy_all_btn = "\U0001F4CB  Copy All Codes",
      export_btn = "\U0001F4BE Export CSV",
      tips_title = "\U0001F4A1 How to use:",
      tip1 = "Select variables by clicking on rows (use Ctrl+Click for multiple)",
      tip2 = "Click 'Copy Codes' to copy codes in the correct format",
      tip3 = "Use copied codes in the 'vars' argument of sus_socio_add_census()",
      tip4 = paste("Census", year, "- Interface generated by climasus4r"),
      no_selection = "No variables selected. Click on table rows.",
      regular_var = "Regular variable",
      imputation_var = "Imputation marker",
      categorical_var = "Categorical variable"
    ),
    es = list(
      title = "Explorador de Variables del Censo Brasileno",
      subtitle = "Seleccione variables para usar en sus_socio_add_census()",
      total_vars = "Total de variables",
      showing = "Mostrando",
      dataset_label = "Dataset",
      code_label = "Codigo",
      name_label = "Nombre",
      category_label = "Categoria",
      type_label = "Tipo",
      values_label = "Valores",
      copy_btn = "\U0001F4CB  Copiar Codigos Seleccionados",
      copy_all_btn = "\U0001F4CB  Copiar Todos los Codigos",
      export_btn = "\U0001F4BE Exportar CSV",
      tips_title = "\U0001F4A1 Como usar:",
      tip1 = "Seleccione variables haciendo clic en las filas (use Ctrl+Click para multiples)",
      tip2 = "Haga clic en 'Copiar Codigos' para copiar codigos en el formato correcto",
      tip3 = "Use los codigos copiados en el argumento 'vars' de sus_socio_add_census()",
      tip4 = paste("Censo", year, "- Interfaz generada por climasus4r"),
      no_selection = "No hay variables seleccionadas. Haga clic en las filas de la tabla.",
      regular_var = "Variable regular",
      imputation_var = "Marcador de imputacion",
      categorical_var = "Variable categorica"
    )
  )
  
  text <- texts[[lang]]
  
  # Gerar linhas da tabela agrupadas por dataset
  table_rows <- ""
  current_dataset <- ""
  
  for (i in 1:nrow(vars_df)) {
    row <- vars_df[i, ]
    
    # Adicionar cabecalho do dataset se mudou
    if (row$dataset != current_dataset) {
      current_dataset <- row$dataset
      n_vars_dataset <- sum(vars_df$dataset == current_dataset)
      n_categorical <- sum(vars_df$dataset == current_dataset & vars_df$has_categories)
      
      table_rows <- paste0(table_rows, sprintf(
        '<tr class="dataset-header">
          <td colspan="6" class="dataset-title">
            <div class="dataset-header-content">
              <span class="dataset-icon">\U0001F4CA</span>
              <span class="dataset-name">%s</span>
              <span class="dataset-stats">%d variables (%d categorical)</span>
            </div>
          </td>
        </tr>',
        toupper(current_dataset),
        n_vars_dataset,
        n_categorical
      ))
    }
    
    # Determinar icone e classe do tipo
    if (row$type == "imputation_marker") {
      type_icon <- "\U0001F3F7\U0000FE0F"
      type_class <- "type-imputation"
      type_label <- text$imputation_var
    } else {
      type_icon <- "\U0001F4DD"
      type_class <- "type-regular"
      type_label <- text$regular_var
    }
    
    # Informacao sobre valores categoricos
    values_info <- ""
    if (row$has_categories) {
      values_info <- sprintf(
        '<span class="categorical-badge" title="%s: %d">
          \U0001F4CA %d
        </span>',
        text$categorical_var,
        row$n_categories,
        row$n_categories
      )
    }
    
    # Linha da variavel
    table_rows <- paste0(table_rows, sprintf(
      '<tr class="variable-row %s" data-code="%s" data-dataset="%s" data-category="%s">
        <td class="code-cell">
          <div class="code-container">
            <code class="variable-code">%s</code>
            <span class="copy-icon" onclick="copySingleCode(\'%s\')" title="Copy this code">\U0001F4CB</span>
          </div>
        </td>
        <td class="name-cell" title="%s">%s</td>
        <td class="category-cell">
          <span class="category-badge category-%s">%s</span>
        </td>
        <td class="type-cell">
          <span class="type-badge %s">%s %s</span>
        </td>
        <td class="values-cell">%s</td>
        <td class="dataset-cell">
          <span class="dataset-badge dataset-%s">%s</span>
        </td>
      </tr>',
      row$type,
      row$code,
      row$dataset,
      row$category,
      row$code,
      row$code,
      row$name,
      substr(row$name, 1, 60),
      row$category,
      row$category,
      type_class,
      type_icon,
      type_label,
      values_info,
      row$dataset,
      row$dataset
    ))
  }
  
  # Construir HTML completo com tema Clima Forest
  header_part <- sprintf('<!DOCTYPE html>
<html lang="%s">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>%s</title>', lang, text$title)
  
  # 2. CSS separado
  css_part <- '<style>
        /* Tema Clima Forest - Cores naturais verdes/terra */
        :root {
            --forest-dark: #2C5530;
            --forest-medium: #4A7C59;
            --forest-light: #8FB996;
            --earth-dark: #8B4513;
            --earth-light: #D2691E;
            --sky-light: #E8F4F8;
            --text-dark: #2C3E50;
            --text-light: #5D6D7E;
            --success: #27AE60;
            --warning: #F39C12;
            --danger: #E74C3C;
            
        }
        
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }
        
        body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, 
                         "Helvetica Neue", Arial, sans-serif;
            background: linear-gradient(135deg, var(--sky-light) 0%%, #f5f9fc 100%%);
            color: var(--text-dark);
            line-height: 1.6;
            padding: 20px;
            min-height: 100vh;
        }
        
        .container {
            max-width: 1600px;
            margin: 0 auto;
            background: white;
            border-radius: 16px;
            box-shadow: 0 10px 40px rgba(44, 83, 48, 0.15);
            overflow: hidden;
            border: 1px solid rgba(139, 69, 19, 0.1);
        }
        
        /* Header com gradiente forest */
        .header {
            background: linear-gradient(135deg, #1e3a28 0%%, #2C5530 100%%);
            color: #f0f7f0;
            padding: 40px;
            text-align: center;
            position: relative;
            overflow: hidden;
        }
        
        .header::before {
            content: "";
            position: absolute;
            top: 0;
            left: 0;
            right: 0;
            bottom: 0;
            background-image: url(\'data:image/svg+xml,<svg width="100" height="100" viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg"><path d="M20,20 Q40,5 60,20 T100,20" stroke="rgba(255,255,255,0.1)" fill="none" stroke-width="2"/></svg>\');
            opacity: 0.3;
        }
        
        .header h1 {
            font-size: 2.5em;
            margin-bottom: 10px;
            font-weight: 700;
            position: relative;
            text-shadow: 0 2px 6px rgba(0,0,0,0.4);
            color: #8FB996;
            letter-spacing: 0.5px;
        }
        
        .header p {
            font-size: 1.2em;
            opacity: 0.95;
            position: relative;
            max-width: 800px;
            margin: 0 auto;
            color: #8FB996; /* Verde claro */
            text-shadow: 0 1px 3px rgba(0, 0, 0, 0.3);
            font-weight: 400;
        }
        
        /* Stats bar */
        .stats-bar {
            background: linear-gradient(to right, var(--forest-light) 0%%, #B8D8C0 100%%);
            padding: 20px 40px;
            display: flex;
            justify-content: space-between;
            align-items: center;
            font-size: 1.1em;
            color: var(--forest-dark);
            border-bottom: 2px solid rgba(139, 69, 19, 0.1);
        }
        
        .stats-bar strong {
            color: var(--earth-dark);
            font-size: 1.4em;
            margin-left: 5px;
        }
        
        /* Controls */
        .controls {
            padding: 25px 40px;
            background: #f8faf9;
            display: flex;
            gap: 15px;
            flex-wrap: wrap;
            border-bottom: 1px solid #e8f0e8;
        }
        
        .btn {
            padding: 12px 24px;
            border: none;
            border-radius: 8px;
            font-weight: 600;
            cursor: pointer;
            transition: all 0.3s ease;
            font-size: 1em;
            display: flex;
            align-items: center;
            gap: 8px;
        }
        
        .btn-primary {
            background: var(--forest-medium);
            color: white;
            border: 1px solid rgba(44, 83, 48, 0.2);
        }
        
        .btn-primary:hover {
            background: var(--forest-medium);
            transform: translateY(-2px);
            color: var(--forest-dark);
            border: 1px solid rgba(44, 83, 48, 0.2);
        }
        
        .btn-secondary {
            background: var(--forest-light);
            color: var(--forest-dark);
            border: 1px solid rgba(44, 83, 48, 0.2);
        }
        
        .btn-secondary:hover {
            background: var(--forest-medium);
            color: white;
            border: 1px solid rgba(44, 83, 48, 0.2);
        }
        
        /* Code help box */
        .code-help {
            background: linear-gradient(to right, #F1F8E9 0%%, #E8F5E9 100%%);
            padding: 20px;
            margin: 20px 40px;
            border-radius: 10px;
            border-left: 5px solid var(--success);
            font-family: "Consolas", "Monaco", monospace;
            font-size: 0.95em;
            display: none;
            border: 1px solid #C8E6C9;
        }
        
        .code-help.show {
            display: block;
            animation: fadeIn 0.3s ease;
        }
        
        /* Table container */
        .table-container {
            max-height: 700px;
            overflow-y: auto;
            padding: 0;
            margin: 0 20px;
            border-radius: 10px;
            border: 1px solid #e8f0e8;
        }
        
        /* Table styles */
        table {
            width: 100%%;
            border-collapse: collapse;
            background: white;
        }
        
        th {
            background: linear-gradient(to bottom, var(--forest-light) 0%%, #9FC9A6 100%%);
            padding: 18px 15px;
            text-align: left;
            font-weight: 600;
            color: var(--forest-dark);
            border-bottom: 3px solid var(--forest-medium);
            position: sticky;
            top: 0;
            z-index: 10;
            font-size: 0.95em;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }
        
        td {
            padding: 15px;
            border-bottom: 1px solid #f0f5f0;
            vertical-align: middle;
        }
        
        /* Dataset headers */
        .dataset-header {
            background: linear-gradient(to right, #f8faf9 0%%, #f0f7f0 100%%) !important;
        }
        
        .dataset-title {
            padding: 15px !important;
            font-weight: 700;
            color: var(--forest-dark);
            border-bottom: 2px solid var(--forest-light);
            font-size: 1.1em;
        }
        
        .dataset-header-content {
            display: flex;
            align-items: center;
            gap: 15px;
        }
        
        .dataset-icon {
            font-size: 1.3em;
        }
        
        .dataset-name {
            font-weight: 700;
        }
        
        .dataset-stats {
            margin-left: auto;
            font-size: 0.9em;
            color: var(--earth-light);
            font-weight: 500;
        }
        
        /* Variable rows */
        .variable-row {
            cursor: pointer;
            transition: all 0.2s ease;
            border-left: 4px solid transparent;
        }
        
        .variable-row:hover {
            background: #f8faf9 !important;
            border-left-color: var(--forest-light);
        }
        
        .variable-row.selected {
            background: #E8F5E9 !important;
            border-left-color: var(--success);
            box-shadow: inset 3px 0 0 var(--success);
        }
        
        /* Code cell */
        .code-cell {
            font-family: "Consolas", "Monaco", monospace;
            min-width: 120px;
        }
        
        .code-container {
            display: flex;
            align-items: center;
            gap: 10px;
        }
        
        .variable-code {
            background: #f5f9f5;
            padding: 8px 12px;
            border-radius: 6px;
            color: var(--earth-dark);
            font-weight: 700;
            font-size: 1.1em;
            border: 1px solid #e0ede0;
            flex-grow: 1;
        }
        
        .copy-icon {
            cursor: pointer;
            padding: 5px;
            border-radius: 4px;
            background: var(--sky-light);
            color: var(--forest-medium);
            transition: all 0.2s;
            font-size: 0.9em;
        }
        
        .copy-icon:hover {
            background: var(--forest-light);
            color: white;
            transform: scale(1.1);
        }
        
        /* Category badges */
        .category-badge {
            display: inline-block;
            padding: 6px 12px;
            border-radius: 20px;
            font-size: 0.85em;
            font-weight: 600;
            text-transform: capitalize;
        }
        
        .category-demographics { background: #E3F2FD; color: #1565C0; }
        .category-education { background: #F3E5F5; color: #7B1FA2; }
        .category-income { background: #E8F5E9; color: #2E7D32; }
        .category-housing { background: #FFF3E0; color: #EF6C00; }
        .category-health { background: #FFEBEE; color: #C62828; }
        .category-migration { background: #E0F2F1; color: #00695C; }
        .category-geography { background: #E8EAF6; color: #3949AB; }
        .category-work { background: #FFF8E1; color: #FF8F00; }
        .category-other { background: #F5F5F5; color: #616161; }
        
        /* Type badges */
        .type-badge {
            display: inline-flex;
            align-items: center;
            gap: 6px;
            padding: 6px 12px;
            border-radius: 6px;
            font-size: 0.85em;
            font-weight: 500;
        }
        
        .type-regular { background: #E8F5E9; color: #2E7D32; }
        .type-imputation { background: #FFF3E0; color: #F57C00; }
        
        /* Dataset badges */
        .dataset-badge {
            display: inline-block;
            padding: 6px 12px;
            border-radius: 6px;
            font-size: 0.85em;
            font-weight: 600;
            text-transform: capitalize;
            background: #f0f5f0;
            color: var(--forest-medium);
        }
        
        /* Categorical badge */
        .categorical-badge {
            display: inline-flex;
            align-items: center;
            gap: 4px;
            padding: 4px 10px;
            background: #E3F2FD;
            color: #1565C0;
            border-radius: 12px;
            font-size: 0.85em;
            font-weight: 500;
            cursor: help;
        }
        
        /* Tips section */
        .tips {
            margin: 40px;
            padding: 30px;
            background: linear-gradient(135deg, #F1F8E9 0%%, #E8F5E9 100%%);
            border-radius: 12px;
            border-left: 6px solid var(--success);
        }
        
        .tips h3 {
            margin-bottom: 20px;
            color: var(--forest-dark);
            font-size: 1.3em;
            display: flex;
            align-items: center;
            gap: 10px;
        }
        
        .tips ul {
            margin-left: 25px;
        }
        
        .tips li {
            margin-bottom: 12px;
            line-height: 1.7;
            color: var(--text-light);
        }
        
        /* Footer */
        .footer {
            text-align: center;
            padding: 25px;
            color: var(--text-light);
            border-top: 1px solid #e8f0e8;
            background: #f8faf9;
            font-size: 0.95em;
        }
        
        /* Animations */
        @keyframes fadeIn {
            from { opacity: 0; transform: translateY(-10px); }
            to { opacity: 1; transform: translateY(0); }
        }
        
        /* Responsive */
        @media (max-width: 1200px) {
            .table-container {
                margin: 0 10px;
            }
            
            th, td {
                padding: 12px 10px;
            }
        }
        
        @media (max-width: 992px) {
            .controls {
                flex-direction: column;
            }
            
            .btn {
                width: 100%%;
                justify-content: center;
            }
            
            .header h1 {
                font-size: 2em;
            }
            
            .header p {
                font-size: 1em;
            }
        }
        
        @media (max-width: 768px) {
            .table-container {
                font-size: 0.9em;
            }
            
            .dataset-header-content {
                flex-direction: column;
                align-items: flex-start;
                gap: 5px;
            }
            
            .dataset-stats {
                margin-left: 0;
            }
            
            .code-container {
                flex-direction: column;
                align-items: flex-start;
                gap: 5px;
            }
            
            .copy-icon {
                align-self: flex-end;
            }
        }
    </style>'
  
  # 3. Corpo HTML com sprintf para variaveis
  body_template <- '
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>%s</h1>
            <p>%s</p>
        </div>
        
        <div class="stats-bar">
            <div>%s: <strong id="totalCount">%d</strong></div>
            <div>%s <strong id="selectedCount">0</strong> selected</div>
            <div>Language: <strong>%s</strong> | Year: <strong>%d</strong></div>
        </div>
        
        <div class="controls">
            <button class="btn btn-primary" onclick="copySelectedCodes()">
                <span>\U0001F4CB</span> %s
            </button>
            <button class="btn btn-secondary" onclick="copyAllCodes()">
                <span>\U0001F4CB</span> %s
            </button>
            <button class="btn btn-secondary" onclick="exportToCSV()">
                <span>\U0001F4BE</span> %s
            </button>
        </div>
        
        <div id="codeHelp" class="code-help">
            <strong>Ready to use in sus_socio_add_census():</strong><br>
            <code id="codeOutput">c()</code>
        </div>
        
        <div class="table-container">
            <table id="variablesTable">
                <thead>
                    <tr>
                        <th width="140">%s</th>
                        <th>%s</th>
                        <th width="120">%s</th>
                        <th width="140">%s</th>
                        <th width="80">%s</th>
                        <th width="100">%s</th>
                    </tr>
                </thead>
                <tbody>
                    %s
                </tbody>
            </table>
        </div>
        
        <div class="tips">
            <h3>%s</h3>
            <ul>
                <li>%s</li>
                <li>%s</li>
                <li>%s</li>
            </ul>
        </div>
        
        <div class="footer">
            %s<br>
            <small>Generated at %s</small>
        </div>
    </div>'
  
  # 4. JavaScript separado
  js_template <- '
    <script>
        // Selecao de linhas
        document.querySelectorAll(".variable-row").forEach(row => {
            row.addEventListener("click", function(e) {
                if (e.ctrlKey || e.metaKey) {
                    // Selecao multipla com Ctrl
                    this.classList.toggle("selected");
                } else if (e.shiftKey) {
                    // Selecao com Shift (range)
                    const rows = Array.from(document.querySelectorAll(".variable-row"));
                    const currentIndex = rows.indexOf(this);
                    const selectedRows = document.querySelectorAll(".variable-row.selected");
                    
                    if (selectedRows.length === 1) {
                        const firstSelected = selectedRows[0];
                        const firstIndex = rows.indexOf(firstSelected);
                        
                        const start = Math.min(firstIndex, currentIndex);
                        const end = Math.max(firstIndex, currentIndex);
                        
                        // Deselecionar tudo primeiro
                        rows.forEach(r => r.classList.remove("selected"));
                        
                        // Selecionar range
                        for (let i = start; i <= end; i++) {
                            rows[i].classList.add("selected");
                        }
                    } else {
                        this.classList.add("selected");
                    }
                } else {
                    // Se o clique foi no icone de copia, nao altera selecao
                    if (e.target.closest(".copy-icon")) {
                        return;
                    }
                    
                    // Selecao unica
                    this.classList.add("selected");
                    
                    // Remover selecao de outras
                    document.querySelectorAll(".variable-row.selected").forEach(otherRow => {
                        if (otherRow !== this) {
                            otherRow.classList.remove("selected");
                        }
                    });
                }
                
                updateSelectedCount();
                e.stopPropagation();
            });
        });
        
        // Atualizar contador
        function updateSelectedCount() {
            const selected = document.querySelectorAll(".variable-row.selected").length;
            document.getElementById("selectedCount").textContent = selected;
            
            const codeHelp = document.getElementById("codeHelp");
            
            if (selected > 0) {
                showCopyHelp();
                codeHelp.classList.add("show");
            } else {
                codeHelp.classList.remove("show");
            }
        }
        
        // Mostrar ajuda de copia
        function showCopyHelp() {
            const selected = document.querySelectorAll(".variable-row.selected");
            const codes = Array.from(selected).map(row => row.getAttribute("data-code"));
            const codeString = \'c("\' + codes.join(\'\", \"\') + \'")\';
            
            document.getElementById("codeOutput").textContent = codeString;
        }
        
        // Copiar codigo individual
        function copySingleCode(code) {
            const codeString = \'c("\' + code + \'")\';
            
            navigator.clipboard.writeText(codeString).then(() => {
                showNotification("Code copied: " + code);
            });
        }
        
        // Copiar codigos selecionados
        function copySelectedCodes() {
            const selected = document.querySelectorAll(".variable-row.selected");
            if (selected.length === 0) {
                showNotification("%s", "warning");
                return;
            }
            
            const codes = Array.from(selected).map(row => row.getAttribute("data-code"));
            const codeString = \'c("\' + codes.join(\'\", \"\') + \'")\';
            
            navigator.clipboard.writeText(codeString).then(() => {
                showNotification("\U00002705 " + selected.length + " codes copied to clipboard");
                console.log("\\\\nReady to use in sus_socio_add_census():\\\\n");
                console.log(codeString);
            });
        }
        
        // Copiar todos os codigos
        function copyAllCodes() {
            const allRows = document.querySelectorAll(".variable-row");
            if (allRows.length === 0) {
                showNotification("No variables available", "warning");
                return;
            }
            
            const codes = Array.from(allRows).map(row => row.getAttribute("data-code"));
            const codeString = \'c("\' + codes.join(\'\", \"\') + \'")\';
            
            navigator.clipboard.writeText(codeString).then(() => {
                showNotification("\U00002705 All " + codes.length + " codes copied to clipboard");
                console.log("\\\\nAll variable codes:\\\\n");
                console.log(codeString);
            });
        }
        
        // Exportar para CSV
        function exportToCSV() {
            const rows = document.querySelectorAll(".variable-row");
            let csv = "code,name,category,type,dataset,has_categories,n_categories\\\\n";
            
            rows.forEach(row => {
                const code = row.getAttribute("data-code");
                const name = row.cells[1].textContent;
                const category = row.getAttribute("data-category");
                const typeText = row.cells[3].textContent.trim();
                const dataset = row.getAttribute("data-dataset");
                const hasCategories = row.cells[4].textContent.includes("\U0001F4CA") ? "TRUE" : "FALSE";
                const nCategories = row.cells[4].textContent.match(/\\\\d+/)?.[0] || "0";
                
                csv += `"${code}","${name}","${category}","${typeText}","${dataset}","${hasCategories}","${nCategories}"\\\\n`;
            });
            
            const blob = new Blob([csv], { type: "text/csv;charset=utf-8;" });
            const link = document.createElement("a");
            const url = URL.createObjectURL(blob);
            link.setAttribute("href", url);
            link.setAttribute("download", "census_variables_%d_%s.csv");
            link.style.visibility = "hidden";
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);
            
            showNotification("\U00002705 CSV file downloaded");
        }
        
        // Notificacao
        function showNotification(message, type = "success") {
            // Remover notificacoes anteriores
            const oldNotifications = document.querySelectorAll(".notification");
            oldNotifications.forEach(n => n.remove());
            
            // Criar nova notificacao
            const notification = document.createElement("div");
            notification.className = `notification notification-${type}`;
            notification.textContent = message;
            notification.style.cssText = `
                position: fixed;
                top: 20px;
                right: 20px;
                padding: 15px 20px;
                background: ${type === "success" ? "#27AE60" : "#F39C12"};
                color: white;
                border-radius: 8px;
                box-shadow: 0 4px 12px rgba(0,0,0,0.15);
                z-index: 1000;
                animation: fadeIn 0.3s ease;
                font-weight: 500;
            `;
            
            document.body.appendChild(notification);
            
            // Remover automaticamente depois de 3 segundos
            setTimeout(() => {
                notification.style.opacity = "0";
                notification.style.transform = "translateX(100px)";
                setTimeout(() => notification.remove(), 300);
            }, 3000);
        }
        
        // Inicializar
        updateSelectedCount();
    </script>
</body>
</html>'
  
  # 5. Construir o corpo com sprintf (agora menor)
  body_content <- sprintf(
    body_template,
    text$title,              # header h1
    text$subtitle,           # header p
    text$total_vars,         # stats label
    nrow(vars_df),           # total count
    text$showing,            # showing label
    lang,                    # language
    year,                    # year
    text$copy_btn,           # copy button
    text$copy_all_btn,       # copy all button
    text$export_btn,         # export button
    text$code_label,         # code column
    text$name_label,         # name column
    text$category_label,     # category column
    text$type_label,         # type column
    text$values_label,       # values column
    text$dataset_label,      # dataset column
    table_rows,              # table rows
    text$tips_title,         # tips title
    text$tip1,               # tip 1
    text$tip2,               # tip 2
    text$tip3,               # tip 3
    text$tip4,               # tip 4/footer
    format(Sys.time(), "%Y-%m-%d %H:%M:%S")  # timestamp
  )
  
  # 6. Construir JavaScript com sprintf
  js_content <- sprintf(js_template, text$no_selection, year, lang)
  
  # 7. Juntar todas as partes
  html_content <- paste(
    header_part,
    css_part,
    body_content,
    js_content,
    sep = "\n"
  )
  
  return(html_content)
}
