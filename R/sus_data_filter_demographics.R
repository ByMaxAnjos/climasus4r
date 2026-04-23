#' Filter Health Data by Demographic Variables
#'
#' Filters health data based on demographic characteristics such as sex, race,
#' age range, education level, region, and municipality. This function complements
#' `sus_data_filter_cid()` by enabling stratified analyses by population subgroups.
#'
#' @param df A `climasus_df` object containing health data (output of the
#'   `climasus4r` pipeline).
#' @param sex Character vector specifying sex categories to include. Accepts
#'   values in English, Portuguese, or Spanish (e.g., `"Male"`, `"Masculino"`,
#'   `"Masculino"`). If `NULL` (default), includes all sexes.
#' @param race Character vector specifying race/color categories to include.
#'   Accepts IBGE standard categories in multiple languages. If `NULL` (default),
#'   includes all races.
#' @param age_range Numeric vector of length 2 specifying the age range
#'   `c(min_age, max_age)`. Use `Inf` for no upper limit. If `NULL` (default),
#'   includes all ages.
#' @param education Character vector specifying education levels to include.
#'   If `NULL` (default), includes all education levels.
#' @param region A string indicating a predefined group of states or regions
#'   (supports multilingual names PT, EN, ES). See Details.
#' @param city Character vector of municipality names (e.g., `"Sao Paulo"`,
#'   `"Natal"`) or IBGE codes (6 or 7-digit, e.g., `"3550308"`, `"2408102"`).
#'   Case-insensitive; accents are normalised for matching. Partial typos trigger
#'   fuzzy suggestions. If `NULL` (default), no additional municipality filter is
#'   applied. Results are merged (union) with any codes in `municipality_code`.
#' @param municipality_code Character or numeric vector specifying municipality
#'   codes (IBGE 6 or 7-digit codes) to include. If `NULL` (default), includes
#'   all municipalities.
#' @param drop_ignored Logical. If `TRUE`, explicitly removes rows where
#'   demographic variables (sex, race, education) contain missing values (`NA`)
#'   or DATASUS ignored codes (e.g., `"9"`, `"Ignorado"`). Default is `FALSE`.
#' @param use_cache Logical. If `TRUE` (default), uses cached spatial data to
#'   avoid re-downloads and improve performance. Only relevant when `city` is
#'   provided.
#' @param cache_dir Character string specifying the directory to store cached
#'   files. Default is `"~/.climasus4r_cache/spatial"`.
#' @param lang Character string specifying the language for messages.
#'   Options: `"en"` (English), `"pt"` (Portuguese, default), `"es"` (Spanish).
#' @param verbose Logical. If `TRUE` (default), prints filtering summary.
#'
#' @return A `climasus_df` filtered by all specified demographic criteria.
#'
#' @details
#' The function automatically detects column names in different languages and
#' standardisations. It handles both original DATASUS column names and
#' standardised names from `sus_data_standardize()`.
#'
#' **Sex categories** (case-insensitive):
#' \itemize{
#'   \item English: `"Male"`, `"Female"`
#'   \item Portuguese: `"Masculino"`, `"Feminino"`
#'   \item Spanish: `"Masculino"`, `"Femenino"`
#' }
#'
#' **Race/Color categories** (IBGE standard):
#' \itemize{
#'   \item English: `"White"`, `"Black"`, `"Yellow"`, `"Brown"`, `"Indigenous"`
#'   \item Portuguese: `"Branca"`, `"Preta"`, `"Amarela"`, `"Parda"`, `"Indigena"`
#'   \item Spanish: `"Blanca"`, `"Negra"`, `"Amarilla"`, `"Parda"`, `"Indigena"`
#' }
#'
#' **IBGE Macro-regions:**
#'   * `"norte"`: c("AC", "AP", "AM", "PA", "RO", "RR", "TO")
#'   * `"nordeste"`: c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE")
#'   * `"centro_oeste"`: c("DF", "GO", "MT", "MS")
#'   * `"sudeste"`: c("ES", "MG", "RJ", "SP")
#'   * `"sul"`: c("PR", "RS", "SC")
#'
#'   **Biomes (Ecological Borders):**
#'   * `"amazonia_legal"`: c("AC", "AP", "AM", "PA", "RO", "RR", "MT", "MA", "TO")
#'   * `"mata_atlantica"`: c("AL", "BA", "CE", "ES", "GO", "MA", "MG", "MS", "PB", "PE", "PI", "PR", "RJ", "RN", "RS", "SC", "SE", "SP")
#'   * `"caatinga"`: c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "MG")
#'   * `"cerrado"`: c("BA", "DF", "GO", "MA", "MG", "MS", "MT", "PA", "PI", "PR", "RO", "SP", "TO")
#'   * `"pantanal"`: c("MT", "MS")
#'   * `"pampa"`: c("RS")
#'
#'   **Hydrography & Climate:**
#'   * `"bacia_amazonia"`: c("AC", "AM", "AP", "MT", "PA", "RO", "RR")
#'   * `"bacia_sao_francisco"`: c("AL", "BA", "DF", "GO", "MG", "PE", "SE")
#'   * `"bacia_parana"`: c("GO", "MG", "MS", "PR", "SP")
#'   * `"bacia_tocantins"`: c("GO", "MA", "PA", "TO")
#'   * `"semi_arido"`: c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "MG")
#'
#'   **Health, Agriculture & Geopolitics:**
#'   * `"matopiba"`: c("MA", "TO", "PI", "BA")
#'   * `"arco_desmatamento"`: c("RO", "AC", "AM", "PA", "MT", "MA")
#'   * `"dengue_hyperendemic"`: c("GO", "MS", "MT", "PR", "RJ", "SP")
#'   * `"sudene"`: c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "MG", "ES")
#'   * `"fronteira_brasil"`: c("AC", "AM", "AP", "MT", "MS", "PA", "PR", "RO", "RR", "RS", "SC")
#'
#' @examples
#' \dontrun{
#' library(climasus4r)
#'
#' # Filter by sex only
#' df_women <- sus_data_filter_demographics(df, sex = "Female")
#'
#' # Filter by age range (elderly, 65+)
#' df_elderly <- sus_data_filter_demographics(df, age_range = c(65, Inf))
#'
#' # Filter by city name (with accent tolerance)
#' df_natal <- sus_data_filter_demographics(df, city = "Natal", lang = "pt")
#'
#' # Filter by multiple city names
#' df_capitals <- sus_data_filter_demographics(
#'   df,
#'   city = c("Sao Paulo", "Rio de Janeiro", "Fortaleza"), #Usar acentos se preferir
#'   lang = "pt"
#' )
#'
#' # Mix: city names + explicit codes (union)
#' df_subset <- sus_data_filter_demographics(
#'   df,
#'   city            = "Natal",
#'   municipality_code = "3550308",
#'   lang            = "pt"
#' )
#'
#' # Complex filtering
#' df_children <- sus_data_filter_demographics(
#'   df,
#'   age_range = c(0, 5),
#'   region    = "Norte",
#'   lang      = "pt"
#' )
#' }
#'
#' @export
sus_data_filter_demographics <- function(df,
                                          sex               = NULL,
                                          race              = NULL,
                                          age_range         = NULL,
                                          education         = NULL,
                                          region            = NULL,
                                          municipality_code = NULL,
                                          city              = NULL,
                                          drop_ignored      = FALSE,
                                          use_cache         = TRUE,
                                          cache_dir         = "~/.climasus4r_cache/spatial",
                                          lang              = "pt",
                                          verbose           = TRUE) {

  cli::cli_h1("climasus4r - Filter Data Demographics")

  # Input validation 
  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_abort("lang must be one of: 'en', 'pt', 'es'")
  }

  if (!is.null(sex)) {
    valid_sex <- c("Male", "Female", "Unknown",
                   "Masculino", "Feminino", "Ignorado",
                   "Femenino", "Desconocido")
    unrecognized <- setdiff(tools::toTitleCase(sex), valid_sex)
    if (length(unrecognized) > 0) {
      cli::cli_warn(c(
        "Unrecognized sex values: {.val {unrecognized}}",
        "i" = "Valid values: {.val {valid_sex}}"
      ))
    }
  }

  # climasus_df validation 
  if (inherits(df, "climasus_df")) {

    required_stage <- "stand"
    current_stage  <- sus_meta(df, "stage")

    if (!is_stage_at_least(current_stage, required_stage)) {
      msg_error <- list(
        en = paste0(
          "Data must be standardized before demographic filtering.\n",
          "Current stage: ", current_stage %||% "unknown", "\n",
          "Required stage: ", required_stage, "\n\n",
          "Please run:\n  df <- sus_data_standardize(df)"
        ),
        pt = paste0(
          "Dados devem ser padronizados antes da filtragem demografica.\n",
          "Estagio atual: ", current_stage %||% "desconhecido", "\n",
          "Estagio requerido: ", required_stage, "\n\n",
          "Por favor, execute:\n  df <- sus_data_standardize(df)"
        ),
        es = paste0(
          "Los datos deben estar estandarizados antes del filtrado demografico.\n",
          "Etapa actual: ", current_stage %||% "desconocida", "\n",
          "Etapa requerida: ", required_stage, "\n\n",
          "Por favor, ejecute:\n  df <- sus_data_standardize(df)"
        )
      )
      cli::cli_abort(msg_error[[lang]] %||% msg_error[["en"]])
    }

    if (verbose) {
      msg_stage_ok <- list(
        en = "Data stage validated: demographic filtering",
        pt = "Estagio de dados validado: filtragem demografica",
        es = "Etapa de datos validada: filtrado demografico"
      )
      cli::cli_alert_success(msg_stage_ok[[lang]] %||% msg_stage_ok[["en"]])
    }

    df <- sus_meta(df, stage = "filter_demo", type = "filter_demo")

  } else {
    msg_error <- list(
      en = c(
        "{.red {cli::symbol$cross} Input is not a {.cls climasus_df} object.}",
        "i" = "This function requires data formatted by the {.pkg climasus4r} pipeline.",
        " " = "",
        "Please prepare your data first:",
        "*" = "{.strong 1. Import:} {.code df <- sus_data_import(...)} or {.code sus_data_read(...)}",
        "*" = "{.strong 2. Clean:} {.code df <- sus_data_clean_encoding(df)}",
        "*" = "{.strong 3. Standardize:} {.code df <- sus_data_standardize(df)}",
        "*" = "{.strong 4. Create:} {.code df <- sus_data_create_variables(...)}",
        "*" = "{.strong 5. Filter demo:} {.code df <- sus_data_filter_demographics(...)}",
        "v" = "Tip: If using external data, run {.fn sus_data_standardize} first."
      ),
      pt = c(
        "{.red {cli::symbol$cross} A entrada nao e um objeto {.cls climasus_df}.}",
        "i" = "Esta funcao requer dados processados pelo pipeline {.pkg climasus4r}.",
        " " = "",
        "Por favor, prepare seus dados primeiro:",
        "*" = "{.strong 1. Importar:} {.code df <- sus_data_import(...)} ou {.code sus_data_read(...)}",
        "*" = "{.strong 2. Limpar:} {.code df <- sus_data_clean_encoding(df)}",
        "*" = "{.strong 3. Padronizar:} {.code df <- sus_data_standardize(df)}",
        "*" = "{.strong 4. Criar:} {.code df <- sus_data_create_variables(...)}",
        "*" = "{.strong 5. Filtrar demo:} {.code df <- sus_data_filter_demographics(...)}",
        "v" = "Dica: Se usar dados externos, execute {.fn sus_data_standardize} primeiro."
      ),
      es = c(
        "{.red {cli::symbol$cross} La entrada no es un objeto {.cls climasus_df}.}",
        "i" = "Esta funcion requiere datos procesados por el pipeline {.pkg climasus4r}.",
        " " = "",
        "Por favor, prepare sus datos primero:",
        "*" = "{.strong 1. Importar:} {.code df <- sus_data_import(...)} o {.code sus_data_read(...)}",
        "*" = "{.strong 2. Limpiar:} {.code df <- sus_data_clean_encoding(df)}",
        "*" = "{.strong 3. Estandarizar:} {.code df <- sus_data_standardize(df)}",
        "*" = "{.strong 4. Crear:} {.code df <- sus_data_create_variables(...)}",
        "*" = "{.strong 5. Filtrar demo:} {.code df <- sus_data_filter_demographics(...)}",
        "v" = "Consejo: Si usa datos externos, ejecute {.fn sus_data_standardize} primero."
      )
    )
    cli::cli_abort(msg_error[[lang]])
  }

  n_original    <- nrow(df)
  filters_applied <- character()
  system        <- sus_meta(df, "system")
  ignored_codes <- c("ignorado", "ignorada", "unknown", "desconocido", "i",
                     "9", "99", "999", "9999", "000000", "-", "", " ")

  # FILTER BY SEX 
  if (!is.null(sex)) {
    sex_col <- find_column(df, c("sex", "sexo", "SEXO"))
    sex_col <- tolower(sex_col)

    if (drop_ignored) {
      res <- clean_ignored_internal(df, sex_col, "Sex/Sexo", ignored_codes, lang, verbose)
      df  <- res$df
    }

    if (is.null(sex_col)) {
      cli::cli_alert_warning("Sex column not found. Skipping sex filter.")
    } else {
      sex_targets <- tools::toTitleCase(sex)
      df <- df_filter_col(df, sex_col, sex_targets)
      filters_applied <- c(filters_applied,
                           paste0("sex: ", paste(sex_targets, collapse = ", ")))
    }
  }

  # FILTER BY RACE 
  if (!is.null(race)) {
    race_col <- find_column(df, c("race", "raca", "raza", "RACACOR", "RACA_COR"))
    race_col <- tolower(race_col)

    if (drop_ignored) {
      res <- clean_ignored_internal(df, race_col, "Race/Raca", ignored_codes, lang, verbose)
      df  <- res$df
    }

    if (is.null(race_col)) {
      cli::cli_alert_warning("Race column not found. Skipping race filter.")
    } else {
      race_targets <- tools::toTitleCase(race)
      df <- df_filter_col(df, race_col, race_targets)
      filters_applied <- c(filters_applied,
                           paste0("race: ", paste(race_targets, collapse = ", ")))
    }
  }

  # FILTER BY AGE RANGE 
  if (!is.null(age_range)) {
    if (length(age_range) != 2) {
      cli::cli_abort("age_range must be a numeric vector of length 2: c(min_age, max_age)")
    }

    age_col <- find_column(df, c("age_years"))

    if (is.null(age_col)) {
      cli::cli_alert_warning(paste0(
        "Age column not found. Skipping age filter. ",
        "Run sus_data_create_variables() to create age_years."
      ))
    } else {
      min_age <- age_range[1]
      max_age <- age_range[2]
      df <- dplyr::filter(df, .data[[age_col]] >= min_age & .data[[age_col]] <= max_age)
      filters_applied <- c(filters_applied,
                           paste0("age: ", min_age, "-",
                                  ifelse(is.infinite(max_age), "+", max_age)))
    }
  }

  #  FILTER BY EDUCATION 
  if (!is.null(education)) {
    edu_col <- find_column(df, c("education", "escolaridade", "escolaridad",
                                  "ESC", "ESC2010"))
    edu_col <- tolower(edu_col)

    if (drop_ignored) {
      res <- clean_ignored_internal(df, edu_col, "Education/Escolaridade", ignored_codes, lang, verbose)
      df  <- res$df
    }

    if (is.null(edu_col)) {
      cli::cli_alert_warning("Education column not found. Skipping education filter.")
    } else {
      education_targets <- tools::toTitleCase(education)
      df <- df_filter_col(df, edu_col, education_targets)
      filters_applied <- c(filters_applied,
                           paste0("education: ", paste(education_targets, collapse = ", ")))
    }
  }

  # FILTER BY REGION OR STATES
  if (!is.null(region)) {
    df_ufs_brasil <- data.table::data.table(
      sigla  = c("RO","AC","AM","RR","PA","AP","TO","MA","PI","CE","RN","PB","PE",
                 "AL","SE","BA","MG","ES","RJ","SP","PR","SC","RS","MS","MT","GO","DF"),
      codigo = c(11,12,13,14,15,16,17,21,22,23,24,25,26,27,28,29,31,32,33,35,41,42,43,50,51,52,53),
      estado_pt = c("Rond\u00f4nia","Acre","Amazonas","Roraima","Par\u00e1","Amap\u00e1",
                    "Tocantins","Maranh\u00e3o","Piau\u00ed","Cear\u00e1",
                    "Rio Grande do Norte","Para\u00edba","Pernambuco","Alagoas",
                    "Sergipe","Bahia","Minas Gerais","Esp\u00edrito Santo",
                    "Rio de Janeiro","S\u00e3o Paulo","Paran\u00e1","Santa Catarina",
                    "Rio Grande do Sul","Mato Grosso do Sul","Mato Grosso",
                    "Goi\u00e1s","Distrito Federal"),
      state_en = c("Rondonia","Acre","Amazonas","Roraima","Para","Amapa","Tocantins",
                   "Maranhao","Piaui","Ceara","Rio Grande do Norte","Paraiba",
                   "Pernambuco","Alagoas","Sergipe","Bahia","Minas Gerais",
                   "Espirito Santo","Rio de Janeiro","Sao Paulo","Parana",
                   "Santa Catarina","Rio Grande do Sul","Mato Grosso do Sul",
                   "Mato Grosso","Goias","Federal District"),
      estado_es = c("Rondonia","Acre","Amazonas","Roraima","Par\u00e1","Amap\u00e1",
                    "Tocantins","Maranh\u00e3o","Piau\u00ed","Cear\u00e1",
                    "Rio Grande del Norte","Para\u00edba","Pernambuco","Alagoas",
                    "Sergipe","Bah\u00eda","Minas Gerais","Esp\u00edrito Santo",
                    "R\u00edo de Janeiro","S\u00e3o Paulo","Paran\u00e1",
                    "Santa Catarina","Rio Grande del Sur","Mato Grosso del Sur",
                    "Mato Grosso","Goi\u00e1s","Distrito Federal")
    )

    all_target_siglas <- unique(unlist(lapply(region, translate_input)))
    target_codes <- df_ufs_brasil[sigla %in% all_target_siglas, codigo]

    if (length(target_codes) > 0) {
      uf_col <- find_column(df, c("manager_uf", "UF_ZI", "uf_gestor", "notification_uf"))
      uf_values <- as.numeric(df[[uf_col]])
      df <- df[uf_values %in% target_codes, ]

      col_name <- base::switch(lang,
        "pt" = "estado_pt",
        "en" = "state_en",
        "es" = "estado_es",
        "estado_pt"
      )
      names_log <- df_ufs_brasil[codigo %in% target_codes, col_name, with = FALSE]
      filters_applied <- c(filters_applied,
                           paste0("States: ", paste(names_log, collapse = ", ")))
      cli::cli_alert_success(
        "Filtered by {length(target_codes)} states based on: {paste(region, collapse = ', ')}"
      )
    } else {
      cli::cli_abort("No valid states found for: {.val {region}}")
    }
  }

  #  FILTER BY MUNICIPALITY (codes and/or city names)
  #
  # city names -> resolved to 7-digit IBGE codes via municipio_meta, then merged
  # with any explicit municipality_code values before a single filter pass.

  all_muni_codes <- if (!is.null(municipality_code)) as.character(municipality_code) else character(0)
  muni_meta      <- NULL  # loaded lazily; reused for zero-row diagnostics

  if (!is.null(city) && length(city) > 0) {
    .muni_patterns <- c("residence_municipality_code", "municipality_code",
                        "residence_municipality", "municipio_residencia",
                        "codigo_municipio", "codigo_municipio_residencia", "CODMUNRES")

    
    muni_meta <- get_spatial_municipio_cache(
      cache_dir = cache_dir,
      use_cache = use_cache,
      lang      = lang,
      verbose   = verbose
    )

    resolved <- resolve_city_input_internal(city, muni_meta, lang, verbose)

    if (length(resolved$codes) > 0) {
      muni_col_check <- find_column(df, .muni_patterns)
      check_city_uf_internal(df, resolved$codes, muni_col_check, muni_meta, lang, verbose)
      all_muni_codes <- unique(c(all_muni_codes, resolved$codes))
    }
    # Snapshot of available municipality codes BEFORE filtering (for zero-row diagnostic)
    muni_col              <- find_column(df, .muni_patterns)
    available_muni_codes  <- if (!is.null(muni_col))
      unique(as.character(df[[muni_col]])) else character(0)
    muni_filter_applied   <- length(all_muni_codes) > 0L

    if (muni_filter_applied) {
      if (is.null(muni_col)) {
        cli::cli_alert_warning("Municipality column not found. Skipping municipality filter.")
      } else {
        # Always include both 6-digit (DATASUS) and 7-digit (IBGE) forms so the
        # filter works regardless of which format the data column uses.
        filter_codes <- muni_codes_both_formats(all_muni_codes)
        df <- df_filter_col(df, muni_col, filter_codes)
        filters_applied <- c(filters_applied,
                            sprintf("municipality: %d codes", length(all_muni_codes)))
      }
    }

  }

  # SUMMARY MESSAGE 
  n_filtered <- nrow(df)
  n_removed  <- n_original - n_filtered
  pct_retained <- round(100 * n_filtered / max(n_original, 1L), 2)

  if (verbose) {
    if (length(filters_applied) == 0) {
      msg <- switch(lang,
        "en" = "No demographic filters applied",
        "pt" = "Nenhum filtro demografico aplicado",
        "es" = "Ningun filtro demografico aplicado"
      )
      cli::cli_alert_warning(msg)
    } else {
      filter_msg <- switch(lang,
        "en" = "Applied demographic filters:",
        "pt" = "Filtros demograficos aplicados:",
        "es" = "Filtros demograficos aplicados:"
      )
      cli::cli_alert_info(filter_msg)
      for (f in filters_applied) cli::cli_li(f)

      summary_msg <- switch(lang,
        "en" = paste0("Retained ", format(n_filtered, big.mark = ","),
                      " of ", format(n_original, big.mark = ","),
                      " rows (", pct_retained, "%)"),
        "pt" = paste0("Retidos ", format(n_filtered, big.mark = ","),
                      " de ", format(n_original, big.mark = ","),
                      " registros (", pct_retained, "%)"),
        "es" = paste0("Retenidos ", format(n_filtered, big.mark = ","),
                      " de ", format(n_original, big.mark = ","),
                      " registros (", pct_retained, "%)")
      )
      cli::cli_alert_success(summary_msg)

      removed_msg <- switch(lang,
        "en" = paste0("Removed ", format(n_removed, big.mark = ","), " rows"),
        "pt" = paste0("Removidos ", format(n_removed, big.mark = ","), " registros"),
        "es" = paste0("Eliminados ", format(n_removed, big.mark = ","), " registros")
      )
      cli::cli_alert_info(removed_msg)

      # Zero-row diagnostic: show available municipalities when the result is empty
      if (n_filtered == 0L && muni_filter_applied && length(available_muni_codes) > 0L) {
        show_available_municipalities_internal(
          available_muni_codes = available_muni_codes,
          muni_meta            = muni_meta,
          cache_dir            = cache_dir,
          use_cache            = use_cache,
          lang                 = lang
        )
      }
    }
  }

  #  Update climasus_df metadata
  if (!inherits(df, "climasus_df")) {
    meta <- list(
      system   = system,
      stage    = "filter_demo",
      type     = "filter_demo",
      spatial  = FALSE,
      temporal = NULL,
      created  = Sys.time(),
      modified = Sys.time(),
      history  = sprintf("[%s] Demographic filters applied",
                         format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      user     = list()
    )
    base_classes <- setdiff(class(df), "climasus_df")
    df <- structure(df, sus_meta = meta, class = c("climasus_df", base_classes))
  } else {
    df <- sus_meta(df, system = system, stage = "filter_demo", type = "filter_demo")
  }

  # Build history entry
  filter_details <- character(0)

  if (!is.null(sex))
    filter_details <- c(filter_details, sprintf("Sex: %s", paste(sex, collapse = ", ")))

  if (!is.null(race))
    filter_details <- c(filter_details, sprintf("Race: %s", paste(race, collapse = ", ")))

  if (!is.null(age_range)) {
    if (length(age_range) == 2) {
      max_age <- age_range[2]
      filter_details <- c(filter_details,
        if (is.infinite(max_age)) sprintf("Age: >= %g years", age_range[1])
        else                       sprintf("Age: %g-%g years", age_range[1], max_age)
      )
    } else {
      filter_details <- c(filter_details, "Age: selected age groups")
    }
  }

  if (!is.null(education))
    filter_details <- c(filter_details, sprintf("Education: %s", paste(education, collapse = ", ")))

  if (!is.null(region))
    filter_details <- c(filter_details, sprintf("Region: %s", paste(region, collapse = ", ")))

  if (!is.null(city))
    filter_details <- c(filter_details, sprintf("City: %s", paste(city, collapse = ", ")))

  if (length(all_muni_codes) > 0 && is.null(city)) {
    if (length(all_muni_codes) <= 3)
      filter_details <- c(filter_details,
                          sprintf("Municipalities: %s", paste(all_muni_codes, collapse = ", ")))
    else
      filter_details <- c(filter_details,
                          sprintf("Municipalities: %s and %d more",
                                  paste(utils::head(all_muni_codes, 3), collapse = ", "),
                                  length(all_muni_codes) - 3))
  }

  history_msg <- if (length(filter_details) > 0)
    sprintf("Filtered by demographics [%s]", paste(filter_details, collapse = " | "))
  else
    "Filtered by demographics [no filters applied]"

  df <- sus_meta(df, add_history = history_msg)

  return(df)
}


# ==============================================================================
# INTERNAL HELPERS
# ==============================================================================

# Apply a column membership filter; works for data.frame, tibble, and Arrow table.
#' @noRd
df_filter_col <- function(df, col, values) {
  dplyr::filter(df, .data[[col]] %in% values)
}

# Expand municipality codes to cover both DATASUS 6-digit and IBGE 7-digit formats.
# DATASUS columns store 6-digit codes (no check digit); muni_meta uses 7-digit.
# Including both ensures the filter works regardless of column format.
#' @noRd
muni_codes_both_formats <- function(codes) {
  codes <- as.character(codes)
  codes_6 <- substr(codes, 1L, 6L)   # strip check digit from 7-digit → DATASUS format
  unique(c(codes, codes_6))
}

# Show which municipalities are present in the data when the filter returns 0 rows.
#' @noRd
show_available_municipalities_internal <- function(available_muni_codes,
                                                    muni_meta,
                                                    cache_dir,
                                                    use_cache,
                                                    lang) {
  header <- switch(lang,
    "en" = "Municipalities available in the data (no match found for your filter):",
    "pt" = "Municipios disponiveis nos dados (nenhum registro para o filtro informado):",
    "es" = "Municipios disponibles en los datos (sin coincidencia para el filtro indicado):"
  )
  cli::cli_alert_warning(header)

  # Load muni_meta if not already available
  if (is.null(muni_meta)) {
    muni_meta <- tryCatch(
      get_spatial_municipio_cache(
        cache_dir = cache_dir,
        use_cache = use_cache,
        lang      = lang,
        verbose   = FALSE
      ),
      error = function(e) NULL
    )
  }

  codes <- unique(available_muni_codes[!is.na(available_muni_codes) & nchar(available_muni_codes) >= 6L])

  if (!is.null(muni_meta) && length(codes) > 0L) {
    meta_6 <- substr(muni_meta$municipio, 1L, 6L)

    # Primary: 6-digit match (DATASUS format — no check digit)
    idx <- match(codes, meta_6)

    # Fallback: exact 7-digit match (IBGE format)
    need_fallback <- is.na(idx) & nchar(codes) == 7L
    if (any(need_fallback)) {
      idx[need_fallback] <- match(codes[need_fallback], muni_meta$municipio)
    }

    labels <- ifelse(
      !is.na(idx),
      sprintf("%s (%s) \u2014 %s", muni_meta$name[idx], muni_meta$uf_code[idx], codes),
      codes
    )
  } else {
    labels <- codes
  }

  labels <- sort(unique(labels))
  for (lbl in labels) cli::cli_li(lbl)
}

# Retrieve municipio_meta with disk caching (parquet or rds fallback).
# Downloads from GitHub if not cached locally.
#' @noRd
get_spatial_municipio_cache <- function(cache_dir, use_cache, lang, verbose) {
  msg <- get_spatial_municipio_messages(lang)

  use_arrow <- requireNamespace("arrow", quietly = TRUE)
  cache_file <- file.path(
    cache_dir,
    if (use_arrow) "municipio_meta.parquet" else "municipio_meta.rds"
  )

  # Ensure cache directory exists
  if (use_cache && !dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    if (verbose) cli::cli_alert_info(paste0(msg$creating_cache, cache_dir))
  }

  # Try loading from cache
  if (use_cache && file.exists(cache_file)) {
    if (verbose) cli::cli_alert_success(paste0(msg$loading_cache, basename(cache_file)))
    result <- tryCatch(
      {
        if (use_arrow) as.data.frame(arrow::read_parquet(cache_file))
        else           readRDS(cache_file)
      },
      error = function(e) {
        if (verbose) cli::cli_alert_warning(msg$cache_error)
        NULL
      }
    )
    if (!is.null(result)) return(result)
  }

  # Download from climasus Data Center
  if (verbose) cli::cli_alert_info(msg$downloading_data)

  remote_base <- "https://github.com/ByMaxAnjos/climasus4r/raw/refs/heads/master/inst/data_4r/"
  spatial_df <- if (use_arrow) {
    as.data.frame(
      suppressMessages(arrow::read_parquet(paste0(remote_base, "municipio_meta.parquet")))
    )
  } else {
    url <- paste0(remote_base, "municipio_meta.rds")
    tmp <- tempfile(fileext = ".rds")
    utils::download.file(url, tmp, quiet = TRUE, mode = "wb")
    readRDS(tmp)
  }

  # Save to cache
  if (use_cache) {
    if (verbose) cli::cli_alert_info(msg$saving_cache)
    tryCatch(
      {
        if (use_arrow) arrow::write_parquet(spatial_df, cache_file)
        else           saveRDS(spatial_df, cache_file)
        if (verbose) cli::cli_alert_success(msg$cache_saved)
      },
      error = function(e) {
        if (verbose) cli::cli_alert_warning(paste0(msg$cache_save_error, e$message))
      }
    )
  }

  spatial_df
}

# Multilingual messages for municipio cache operations.
#' @noRd
get_spatial_municipio_messages <- function(lang) {
  messages <- list(
    en = list(
      creating_cache    = "Creating cache directory: ",
      loading_cache     = "Loading from cache: ",
      cache_error       = "Cache loading failed. Downloading fresh data...",
      downloading_data  = "Downloading municipio data from climasus Data Center...",
      saving_cache      = "Saving to cache...",
      cache_saved       = "Municipio data cached successfully.",
      cache_save_error  = "Failed to save cache: "
    ),
    pt = list(
      creating_cache    = "Criando diretorio de cache: ",
      loading_cache     = "Carregando do cache: ",
      cache_error       = "Falha ao carregar cache. Baixando dados novos...",
      downloading_data  = "Baixando dados de municipios do climasus Data Center...",
      saving_cache      = "Salvando no cache...",
      cache_saved       = "Dados de municipios armazenados em cache com sucesso.",
      cache_save_error  = "Falha ao salvar cache: "
    ),
    es = list(
      creating_cache    = "Creando directorio de cache: ",
      loading_cache     = "Cargando desde cache: ",
      cache_error       = "Fallo al cargar cache. Descargando datos nuevos...",
      downloading_data  = "Descargando datos de municipios del climasus Data Center...",
      saving_cache      = "Guardando en cache...",
      cache_saved       = "Datos de municipios almacenados en cache con exito.",
      cache_save_error  = "Fallo al guardar cache: "
    )
  )
  messages[[lang]] %||% messages[["pt"]]
}

# Resolve city names / codes to 7-digit IBGE municipality codes.
# Returns list(codes = character vector of resolved codes).
#' @noRd
resolve_city_input_internal <- function(city, muni_meta, lang, verbose) {
  codes_found <- character(0)

  meta_norm <- stringi::stri_trans_general(tolower(muni_meta$no_accents), "Latin-ASCII")

  for (raw in city) {
    input <- trimws(as.character(raw))

    #  Numeric code path (6 or 7 digits) 
    if (grepl("^\\d{6,7}$", input)) {
      if (nchar(input) == 7L) {
        idx <- which(muni_meta$municipio == input)
      } else {
        idx <- which(substr(muni_meta$municipio, 1L, 6L) == input)
      }

      if (length(idx) > 0L) {
        codes_found <- c(codes_found, muni_meta$municipio[idx])
      } else {
        msgs <- list(
          pt = "Codigo de municipio {.val {input}} nao encontrado em municipio_meta.",
          en = "Municipality code {.val {input}} not found in municipio_meta.",
          es = "Codigo de municipio {.val {input}} no encontrado en municipio_meta."
        )
        cli::cli_alert_warning(msgs[[lang]] %||% msgs[["pt"]])
      }
      next
    }

    #  Name path 
    input_norm <- stringi::stri_trans_general(tolower(input), "Latin-ASCII")

    # Exact match (accent-normalised)
    idx <- which(meta_norm == input_norm)

    # Fallback: exact match on original name column
    if (length(idx) == 0L) {
      orig_norm <- stringi::stri_trans_general(tolower(muni_meta$name), "Latin-ASCII")
      idx <- which(orig_norm == input_norm)
    }

    if (length(idx) > 0L) {
      codes_found <- c(codes_found, muni_meta$municipio[idx])

      # Ambiguity warning (same name in multiple UFs)
      if (verbose && length(idx) > 1L) {
        info <- paste(
          sprintf("%s (%s - %s)",
                  muni_meta$name[idx], muni_meta$uf_code[idx], muni_meta$municipio[idx]),
          collapse = ", "
        )
        msgs <- list(
          pt = "'{input}' encontrado em {length(idx)} estados: {info}. Todos incluidos.",
          en = "'{input}' found in {length(idx)} states: {info}. All included.",
          es = "'{input}' encontrado en {length(idx)} estados: {info}. Todos incluidos."
        )
        cli::cli_alert_warning(glue::glue(msgs[[lang]] %||% msgs[["pt"]]))
      }
    } else {
      # Fuzzy suggestions
      suggest_city_matches_internal(input, input_norm, meta_norm, muni_meta, lang)
    }
  }

  list(codes = unique(codes_found))
}

# Suggest close municipality name matches.
#
# Strategy (in priority order):
#   1. Prefix match  — catches truncated input like "Nata" → "Natal"
#   2. Global edit-distance (utils::adist) — catches transpositions / typos
#      Uses full-string distance, NOT substring, preventing false positives
#      such as "nata" matching "pimenta" via a shared sub-sequence.
#' @noRd
suggest_city_matches_internal <- function(input, input_norm, meta_norm, muni_meta, lang) {
  msgs_not_found <- list(
    pt = "Municipio {.val {input}} nao encontrado.",
    en = "Municipality {.val {input}} not found.",
    es = "Municipio {.val {input}} no encontrado."
  )
  cli::cli_alert_warning(msgs_not_found[[lang]] %||% msgs_not_found[["pt"]])

  # 1. Prefix match: input is a leading fragment of the city name
  prefix_idx <- which(startsWith(meta_norm, input_norm))

  # 2. Full-string edit distance — avoids substring false positives
  #    Allow 1 edit for short inputs, up to 3 for longer ones
  max_dist  <- max(1L, min(3L, floor(nchar(input_norm) * 0.25)))
  distances <- utils::adist(input_norm, meta_norm, ignore.case = TRUE)[1L, ]
  fuzzy_idx <- which(distances <= max_dist)
  fuzzy_idx <- fuzzy_idx[order(distances[fuzzy_idx])]  # closest first

  # Merge: prefix results first (most intuitive), then edit-distance matches
  close_idx <- unique(c(prefix_idx, fuzzy_idx))
  close_idx <- utils::head(close_idx, 5L)

  if (length(close_idx) == 0L) {
    msgs_none <- list(
      pt = "Nenhuma sugestao disponivel. Verifique a grafia ou use o codigo IBGE.",
      en = "No suggestions available. Check the spelling or use the IBGE code.",
      es = "Sin sugerencias disponibles. Verifique la ortografia o use el codigo IBGE."
    )
    cli::cli_alert_info(msgs_none[[lang]] %||% msgs_none[["pt"]])
    return(invisible(NULL))
  }

  suggestions <- sprintf("%s (%s \u2014 %s)",
                         muni_meta$name[close_idx],
                         muni_meta$uf_code[close_idx],
                         muni_meta$municipio[close_idx])

  msgs_suggest <- list(
    pt = "Voce quis dizer:",
    en = "Did you mean:",
    es = "\u00bfQuiso decir:"
  )
  cli::cli_alert_info(msgs_suggest[[lang]] %||% msgs_suggest[["pt"]])
  for (s in suggestions) cli::cli_li(s)
}

# Warn when requested municipalities belong to UFs absent from the data.
# Derives data UFs from the municipality column (no separate UF column needed).
#' @noRd
check_city_uf_internal <- function(df, requested_codes, muni_col, muni_meta, lang, verbose) {
  if (!verbose || is.null(muni_col)) return(invisible(NULL))

  requested_ufs <- unique(muni_meta$uf_code[muni_meta$municipio %in% requested_codes])

  data_codes <- unique(as.character(df[[muni_col]]))
  data_codes <- data_codes[!is.na(data_codes) & nchar(data_codes) >= 6L]
  data_ufs   <- unique(muni_meta$uf_code[muni_meta$municipio %in% data_codes])

  if (length(data_ufs) == 0L) return(invisible(NULL))

  outside_ufs <- setdiff(requested_ufs, data_ufs)
  if (length(outside_ufs) == 0L) return(invisible(NULL))

  outside_cities <- muni_meta$name[
    muni_meta$municipio %in% requested_codes & muni_meta$uf_code %in% outside_ufs
  ]

  msgs <- list(
    pt = c(
      "!" = paste0("Municipios solicitados pertencem a estado(s) ausente(s) nos dados: ",
                   paste(outside_ufs, collapse = ", ")),
      "i" = paste0("Municipios afetados: ",
                   paste(utils::head(outside_cities, 5L), collapse = ", ")),
      ">" = "Verifique se os dados foram filtrados por UF em sus_data_import()."
    ),
    en = c(
      "!" = paste0("Requested municipalities belong to state(s) not in the data: ",
                   paste(outside_ufs, collapse = ", ")),
      "i" = paste0("Affected municipalities: ",
                   paste(utils::head(outside_cities, 5L), collapse = ", ")),
      ">" = "Check if the data was pre-filtered by state in sus_data_import()."
    ),
    es = c(
      "!" = paste0("Los municipios solicitados pertenecen a estado(s) no presentes en los datos: ",
                   paste(outside_ufs, collapse = ", ")),
      "i" = paste0("Municipios afectados: ",
                   paste(utils::head(outside_cities, 5L), collapse = ", ")),
      ">" = "Verifique si los datos fueron filtrados por estado en sus_data_import()."
    )
  )
  cli::cli_warn(msgs[[lang]] %||% msgs[["pt"]])
}

# Remove rows with NA or ignored-code values in a column.
#' @noRd
clean_ignored_internal <- function(df, col_name, var_label, ignored_codes, lang, verbose) {
  if (is.null(col_name) || !col_name %in% names(df)) return(list(df = df, dropped = 0L))

  n_before <- nrow(df)
  df <- df[!is.na(df[[col_name]]), ]
  df <- df[!(trimws(tolower(as.character(df[[col_name]]))) %in% ignored_codes), ]
  dropped <- n_before - nrow(df)

  if (dropped > 0L && verbose) {
    msg <- list(
      en = paste0("Dropped ", format(dropped, big.mark = ","),
                  " rows with missing/ignored '", var_label, "'."),
      pt = paste0("Removidos ", format(dropped, big.mark = ","),
                  " registros com '", var_label, "' ignorado/NA."),
      es = paste0("Eliminados ", format(dropped, big.mark = ","),
                  " registros con '", var_label, "' ignorado/NA.")
    )
    cli::cli_alert_info(msg[[lang]] %||% msg[["en"]])
  }
  list(df = df, dropped = dropped)
}

# Find the first matching column name from a priority list.
#' @noRd
find_column <- function(df, patterns) {
  for (p in patterns) if (p %in% names(df)) return(p)
  return(NULL)
}

#' @noRd
translate_input <- function(x) {
  if (is.numeric(x) || !is.na(suppressWarnings(as.numeric(x)))) {
    return(df_ufs_brasil[codigo == as.numeric(x), sigla])
  }
  reg_clean  <- tolower(x)
  target_key <- if (reg_clean %in% names(.region_aliases)) .region_aliases[[reg_clean]] else reg_clean
  if (target_key %in% names(.br_regions)) .br_regions[[target_key]] else toupper(x)
}

#' @noRd
.br_regions <- list(
  # IBGE Macro-regions
  norte         = c("AC", "AP", "AM", "PA", "RO", "RR", "TO"),
  nordeste      = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE"),
  centro_oeste  = c("DF", "GO", "MT", "MS"),
  sudeste       = c("ES", "MG", "RJ", "SP"),
  sul           = c("PR", "RS", "SC"),
  # Biomes
  amazonia_legal    = c("AC", "AP", "AM", "PA", "RO", "RR", "MT", "MA", "TO"),
  mata_atlantica    = c("AL", "BA", "CE", "ES", "GO", "MA", "MG", "MS", "PB",
                        "PE", "PI", "PR", "RJ", "RN", "RS", "SC", "SE", "SP"),
  caatinga          = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "MG"),
  cerrado           = c("BA", "DF", "GO", "MA", "MG", "MS", "MT", "PA", "PI", "PR", "RO", "SP", "TO"),
  pantanal          = c("MT", "MS"),
  pampa             = c("RS"),
  # Hydrography & Climate
  bacia_amazonica       = c("AC", "AM", "AP", "MT", "PA", "RO", "RR"),
  bacia_sao_francisco   = c("AL", "BA", "DF", "GO", "MG", "PE", "SE"),
  bacia_parana          = c("GO", "MG", "MS", "PR", "SP"),
  bacia_tocantins       = c("GO", "MA", "PA", "TO"),
  semi_arido            = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "MG"),
  # Health, Agriculture & Geopolitics
  matopiba          = c("MA", "TO", "PI", "BA"),
  arco_desmatamento = c("RO", "AC", "AM", "PA", "MT", "MA"),
  dengue_hyperendemic = c("GO", "MS", "MT", "PR", "RJ", "SP"),
  sudene            = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "MG", "ES"),
  fronteira_brasil  = c("AC", "AM", "AP", "MT", "MS", "PA", "PR", "RO", "RR", "RS", "SC")
)

#' @noRd
.region_aliases <- list(
  # English
  north = "norte", northeast = "nordeste", central_west = "centro_oeste",
  southeast = "sudeste", south = "sul", amazon = "amazonia_legal",
  legal_amazon = "amazonia_legal", atlantic_forest = "mata_atlantica",
  semi_arid = "semi_arido", border = "fronteira_brasil",
  # Spanish
  noreste = "nordeste", sudeste = "sudeste", sur = "sul",
  amazonia = "amazonia_legal", bosque_atlantico = "mata_atlantica",
  semiarido = "semi_arido", frontera = "fronteira_brasil"
)
