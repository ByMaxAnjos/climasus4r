#' Filter Health Data by Demographic Variables (Arrow / DuckDB lazy backend)
#'
#' Filters health data based on demographic characteristics such as sex, race,
#' age range, education level, region, and municipality. Supports lazy evaluation
#' with Arrow and DuckDB backends for out-of-memory processing.
#'
#' @param df A `climasus_df`, Arrow Dataset/Table, Arrow query, or DuckDB tbl.
#' @param sex Character vector of sex categories to include.
#' @param race Character vector of race/color categories to include.
#' @param age_range Numeric vector of length 2: `c(min_age, max_age)`.
#' @param education Character vector of education levels to include.
#' @param region String indicating a predefined group of states or regions
#'   (multilingual PT/EN/ES).
#' @param city Character vector of municipality names (e.g., `"Natal"`,
#'   `"Sao Paulo"`) or IBGE codes (6 or 7-digit). Case-insensitive; accents
#'   normalised. Typos trigger fuzzy suggestions. Merged (union) with
#'   `municipality_code`.
#' @param municipality_code Character or numeric vector of 6 or 7-digit IBGE
#'   municipality codes. Applied independently from `city`.
#' @param drop_ignored Logical. Remove rows with NA / ignored codes in
#'   demographic columns. For lazy backends, only applied after `collect()`.
#' @param lang Language for messages: `"pt"` (default), `"en"`, `"es"`.
#' @param verbose Logical. Print filtering summary (default `TRUE`).
#' @param use_cache Logical. Use cached `municipio_meta` (default `TRUE`).
#' @param cache_dir Directory for cached files
#'   (default `"~/.climasus4r_cache/data"`).
#'
#' @return A filtered object of the same class/backend type as input.
#'   For Arrow/DuckDB, returns a lazy query — call `dplyr::collect()` to
#'   materialise.
#'
#' @examples
#' \dontrun{
#' # Arrow Dataset (lazy — no collect until needed)
#' arrow_ds <- arrow::open_dataset("data/sim/")
#' arrow_filtered <- sus_data_filter_demographics_arrow(
#'   arrow_ds,
#'   sex       = "Feminino",
#'   age_range = c(65, Inf),
#'   city      = "Natal",
#'   lang      = "pt"
#' )
#' result <- dplyr::collect(arrow_filtered)
#'
#' # DuckDB tbl (lazy)
#' con <- DBI::dbConnect(duckdb::duckdb())
#' tbl_sim <- dplyr::tbl(con, "sim_data")
#' tbl_filtered <- sus_data_filter_demographics_arrow(
#'   tbl_sim,
#'   city = "Recife",
#'   lang = "pt"
#' )
#' result <- dplyr::collect(tbl_filtered)
#' }
sus_data_filter_demographics_arrow <- function(df,
                                               sex               = NULL,
                                               race              = NULL,
                                               age_range         = NULL,
                                               education         = NULL,
                                               region            = NULL,
                                               city              = NULL,
                                               municipality_code = NULL,
                                               drop_ignored      = FALSE,
                                               lang              = "pt",
                                               verbose           = TRUE,
                                               use_cache         = TRUE,
                                               cache_dir         = "~/.climasus4r_cache/spatial") {

  cli::cli_h1("climasus4r - Filter Data Demographics")

  #  Backend detection 
  backend_type <- detect_backend_type(df)
  if (backend_type == "unsupported") {
    cli::cli_abort(c(
      "Unsupported input type: {.cls {class(df)}}.",
      "i" = "Accepted: data.frame / climasus_df, Arrow Dataset/Table, or DuckDB tbl."
    ))
  }

  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_abort("lang must be one of: 'en', 'pt', 'es'")
  }

  #  Sex validation 
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

  #  climasus_df stage validation (data frames only) 
  system <- NULL
  if (inherits(df, "climasus_df")) {
    required_stage <- "stand"
    current_stage  <- sus_meta(df, "stage")
    if (!is.null(current_stage) && !is_stage_at_least(current_stage, required_stage)) {
      msg_error <- list(
        en = paste0("Data must be standardized before demographic filtering. Stage: ", current_stage),
        pt = paste0("Dados devem ser padronizados antes da filtragem. Estagio: ", current_stage),
        es = paste0("Datos deben ser estandarizados antes del filtrado. Etapa: ", current_stage)
      )
      cli::cli_abort(msg_error[[lang]] %||% msg_error[["en"]])
    }
    system <- sus_meta(df, "system")
    if (verbose) {
      msg_ok <- list(
        en = "Data stage validated: demographic filtering",
        pt = "Estagio de dados validado: filtragem demografica",
        es = "Etapa de datos validada: filtrado demografico"
      )
      cli::cli_alert_success(msg_ok[[lang]] %||% msg_ok[["en"]])
    }
  } else if (backend_type == "dataframe") {
    msg_error <- list(
      en = "Input is not a climasus_df. Run sus_data_standardize() first.",
      pt = "A entrada nao e um climasus_df. Execute sus_data_standardize() primeiro.",
      es = "La entrada no es un climasus_df. Ejecute sus_data_standardize() primero."
    )
    cli::cli_abort(msg_error[[lang]] %||% msg_error[["en"]])
  }

  #  Column names (lazy-safe: names() works for Arrow/DuckDB tbl) 
  col_names       <- names(df)
  filters_applied <- character()
  n_original      <- if (backend_type == "dataframe") nrow(df) else NULL

  ignored_codes <- c("ignorado", "ignorada", "unknown", "desconocido", "i",
                     "9", "99", "999", "9999", "000000", "-", "", " ")

  # Unified lazy-compatible filter dispatcher
  apply_filter <- function(data, filter_expr) {
    apply_filter_by_backend(data, filter_expr, backend_type)
  }

  #  FILTER BY SEX 
  if (!is.null(sex)) {
    sex_col <- find_column_from_names(col_names, c("sex", "sexo", "SEXO"))
    if (is.null(sex_col)) {
      cli::cli_alert_warning("Sex column not found. Skipping sex filter.")
    } else {
      sex_targets <- tools::toTitleCase(sex)
      df <- apply_filter(df, list(type = "in", column = sex_col, values = sex_targets))
      filters_applied <- c(filters_applied, paste0("sex: ", paste(sex_targets, collapse = ", ")))
    }
  }

  #  FILTER BY RACE 
  if (!is.null(race)) {
    race_col <- find_column_from_names(col_names, c("race", "raca", "raza", "RACACOR", "RACA_COR"))
    if (is.null(race_col)) {
      cli::cli_alert_warning("Race column not found. Skipping race filter.")
    } else {
      race_targets <- tools::toTitleCase(race)
      df <- apply_filter(df, list(type = "in", column = race_col, values = race_targets))
      filters_applied <- c(filters_applied, paste0("race: ", paste(race_targets, collapse = ", ")))
    }
  }

  #  FILTER BY AGE RANGE 
  if (!is.null(age_range)) {
    if (length(age_range) != 2L) cli::cli_abort("age_range must be c(min_age, max_age)")
    age_col <- find_column_from_names(col_names, c("age_years"))
    if (is.null(age_col)) {
      cli::cli_alert_warning("age_years column not found. Run sus_data_create_variables() first.")
    } else {
      df <- apply_filter(df, list(
        type = "between", column = age_col,
        min = age_range[1], max = age_range[2]
      ))
      filters_applied <- c(filters_applied, paste0(
        "age: ", age_range[1], "-", ifelse(is.infinite(age_range[2]), "+", age_range[2])
      ))
    }
  }

  #  FILTER BY EDUCATION 
  if (!is.null(education)) {
    edu_col <- find_column_from_names(
      col_names, c("education", "escolaridade", "escolaridad", "ESC", "ESC2010")
    )
    if (is.null(edu_col)) {
      cli::cli_alert_warning("Education column not found. Skipping education filter.")
    } else {
      edu_targets <- tools::toTitleCase(education)
      df <- apply_filter(df, list(type = "in", column = edu_col, values = edu_targets))
      filters_applied <- c(filters_applied, paste0("education: ", paste(edu_targets, collapse = ", ")))
    }
  }

  #  FILTER BY REGION 
  if (!is.null(region)) {
    all_target_siglas <- unique(unlist(lapply(region, translate_input_arrow)))
    uf_col <- find_column_from_names(
      col_names, c("manager_uf", "UF_ZI", "uf_gestor", "notification_uf", "UF", "uf")
    )
    if (is.null(uf_col)) {
      cli::cli_alert_warning("UF column not found. Skipping region filter.")
    } else if (length(all_target_siglas) > 0L) {
      df <- apply_filter(df, list(type = "in", column = uf_col, values = all_target_siglas))
      filters_applied <- c(filters_applied, paste0("region: ", paste(region, collapse = ", ")))
      if (verbose)
        cli::cli_alert_success(
          "Filtered by {length(all_target_siglas)} states: {paste(region, collapse = ', ')}"
        )
    } else {
      cli::cli_abort("No valid states found for: {.val {region}}")
    }
  }

  #  FILTER BY MUNICIPALITY (city names and/or explicit codes) 
  #
  # Resolution (always eager, muni_meta is small):
  #   city names  → 7-digit IBGE codes via municipio_meta
  #   both formats (6 + 7 digit) included in the filter vector
  #
  # Actual filtering is lazy for Arrow/DuckDB via apply_filter().

  .muni_patterns <- c("residence_municipality_code", "municipality_code",
                      "residence_municipality", "municipio_residencia",
                      "codigo_municipio", "codigo_municipio_residencia", "CODMUNRES")

  all_muni_codes <- if (!is.null(municipality_code)) as.character(municipality_code) else character(0)
  muni_meta      <- NULL

  if (!is.null(city) && length(city) > 0L) {
    muni_meta <- get_spatial_municipio_cache(
      cache_dir = cache_dir,
      use_cache = use_cache,
      lang      = lang,
      verbose   = verbose
    )

    resolved <- resolve_city_input_internal(city, muni_meta, lang, verbose)

    if (length(resolved$codes) > 0L) {
      # UF consistency check: requires eager column access — data frames only
      if (backend_type == "dataframe") {
        muni_col_check <- find_column_from_names(col_names, .muni_patterns)
        check_city_uf_internal(df, resolved$codes, muni_col_check, muni_meta, lang, verbose)
      }
      all_muni_codes <- unique(c(all_muni_codes, resolved$codes))
    }
  }

  muni_filter_applied <- length(all_muni_codes) > 0L
  muni_col            <- find_column_from_names(col_names, .muni_patterns)

  # Snapshot available codes BEFORE filtering — data frames only
  # (Arrow/DuckDB: skip to preserve laziness; user can inspect after collect())
  available_muni_codes <- character(0)
  if (muni_filter_applied && backend_type == "dataframe" && !is.null(muni_col)) {
    available_muni_codes <- unique(as.character(df[[muni_col]]))
    available_muni_codes <- available_muni_codes[!is.na(available_muni_codes)]
  }

  if (muni_filter_applied) {
    if (is.null(muni_col)) {
      cli::cli_alert_warning("Municipality column not found. Skipping municipality filter.")
    } else {
      # Always include both 6-digit (DATASUS) and 7-digit (IBGE) forms
      filter_codes <- muni_codes_both_formats(all_muni_codes)
      df <- apply_filter(df, list(type = "in", column = muni_col, values = filter_codes))
      filters_applied <- c(filters_applied,
                           sprintf("municipality: %d codes", length(all_muni_codes)))
    }
  }

  #  SUMMARY MESSAGE 
  if (verbose) {
    if (length(filters_applied) == 0L) {
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

      if (backend_type == "dataframe" && !is.null(n_original)) {
        n_filtered   <- nrow(df)
        n_removed    <- n_original - n_filtered
        pct_retained <- round(100 * n_filtered / max(n_original, 1L), 2)

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

        # Zero-row diagnostic
        if (n_filtered == 0L && muni_filter_applied && length(available_muni_codes) > 0L) {
          show_available_municipalities_internal(
            available_muni_codes = available_muni_codes,
            muni_meta            = muni_meta,
            cache_dir            = cache_dir,
            use_cache            = use_cache,
            lang                 = lang
          )
        }

      } else {
        # Lazy backend — row count not available until collect()
        msg_lazy <- list(
          en = "Filters applied lazily. Call {.code dplyr::collect()} to materialise.",
          pt = "Filtros aplicados de forma lazy. Chame {.code dplyr::collect()} para materializar.",
          es = "Filtros aplicados lazy. Llame {.code dplyr::collect()} para materializar."
        )
        cli::cli_alert_info(msg_lazy[[lang]] %||% msg_lazy[["pt"]])
      }
    }
  }

  #  Preserve / attach climasus metadata 
  if (backend_type == "dataframe") {
    if (inherits(df, "climasus_df")) {
      df <- sus_meta(df, system = system, stage = "filter_demo", type = "filter_demo")
      history_msg <- sprintf("Filtered by demographics [%s]", paste(filters_applied, collapse = " | "))
      df <- sus_meta(df, add_history = history_msg)
    }
  }

  return(df)
}


# ==============================================================================
# HELPERS — BACKEND DETECTION & DISPATCH
# ==============================================================================

#' Detect the storage backend of an input object.
#' Returns "dataframe", "arrow", "duckdb", or "unsupported".
#' @noRd
detect_backend_type <- function(df) {
  if (inherits(df, c("data.frame", "tbl_df", "climasus_df"))) return("dataframe")
  if (inherits(df, c("ArrowObject", "arrow_dplyr_query",
                      "Dataset", "Table", "RecordBatch",
                      "ArrowTabular", "FileSystemDataset"))) return("arrow")
  if (inherits(df, c("tbl_dbi", "tbl_sql", "tbl_lazy")))       return("duckdb")
  "unsupported"
}

#' Apply a structured filter expression to any supported backend.
#'
#' filter_expr is a list with:
#'   $type   — "in" | "between"
#'   $column — column name (character)
#'   $values — for "in": character/numeric vector
#'   $min, $max — for "between": numeric scalars
#'
#' Arrow and DuckDB both go through dplyr verbs so the query stays lazy.
#' @noRd
apply_filter_by_backend <- function(df, filter_expr, backend_type) {
  col <- filter_expr$column

  if (filter_expr$type == "in") {
    vals <- filter_expr$values
    # dplyr::filter works lazily for Arrow/DuckDB and eagerly for data frames.
    # Using it uniformly avoids the need to special-case each backend.
    return(dplyr::filter(df, .data[[col]] %in% vals))
  }

  if (filter_expr$type == "between") {
    lo <- filter_expr$min
    hi <- filter_expr$max
    return(dplyr::filter(df, .data[[col]] >= lo & .data[[col]] <= hi))
  }

  df  # pass-through for unrecognised types
}

#' Find the first matching column name from a vector of candidate names.
#' Works with any object that supports names() (data.frame, Arrow, DuckDB tbl).
#' @noRd
find_column_from_names <- function(col_names, patterns) {
  for (p in patterns) if (p %in% col_names) return(p)
  NULL
}

#' Expand municipality codes to cover both DATASUS (6-digit) and IBGE (7-digit).
#' @noRd
muni_codes_both_formats <- function(codes) {
  codes   <- as.character(codes)
  codes_6 <- substr(codes, 1L, 6L)
  unique(c(codes, codes_6))
}

# ==============================================================================
# HELPERS — REGION TRANSLATION (local copies, independent from main R/ file)
# ==============================================================================

#' @noRd
translate_input_arrow <- function(x) {
  if (is.numeric(x) || !is.na(suppressWarnings(as.numeric(x)))) {
    # numeric state code → sigla (lookup in .br_regions_arrow)
    return(toupper(as.character(x)))
  }
  reg_clean  <- tolower(x)
  target_key <- if (reg_clean %in% names(.region_aliases_arrow))
    .region_aliases_arrow[[reg_clean]] else reg_clean
  if (target_key %in% names(.br_regions_arrow)) .br_regions_arrow[[target_key]] else toupper(x)
}

#' @noRd
.br_regions_arrow <- list(
  norte           = c("AC", "AP", "AM", "PA", "RO", "RR", "TO"),
  nordeste        = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE"),
  centro_oeste    = c("DF", "GO", "MT", "MS"),
  sudeste         = c("ES", "MG", "RJ", "SP"),
  sul             = c("PR", "RS", "SC"),
  amazonia_legal  = c("AC", "AP", "AM", "PA", "RO", "RR", "MT", "MA", "TO"),
  mata_atlantica  = c("AL", "BA", "CE", "ES", "GO", "MA", "MG", "MS", "PB",
                      "PE", "PI", "PR", "RJ", "RN", "RS", "SC", "SE", "SP"),
  caatinga        = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "MG"),
  cerrado         = c("BA", "DF", "GO", "MA", "MG", "MS", "MT", "PA", "PI", "PR", "RO", "SP", "TO"),
  pantanal        = c("MT", "MS"),
  pampa           = c("RS"),
  bacia_amazonica = c("AC", "AM", "AP", "MT", "PA", "RO", "RR"),
  semi_arido      = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "MG"),
  matopiba        = c("MA", "TO", "PI", "BA"),
  dengue_hyperendemic = c("GO", "MS", "MT", "PR", "RJ", "SP"),
  sudene          = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "MG", "ES"),
  fronteira_brasil = c("AC", "AM", "AP", "MT", "MS", "PA", "PR", "RO", "RR", "RS", "SC")
)

#' @noRd
.region_aliases_arrow <- list(
  north = "norte", northeast = "nordeste", central_west = "centro_oeste",
  southeast = "sudeste", south = "sul", amazon = "amazonia_legal",
  legal_amazon = "amazonia_legal", atlantic_forest = "mata_atlantica",
  semi_arid = "semi_arido", border = "fronteira_brasil",
  noreste = "nordeste", sur = "sul", amazonia = "amazonia_legal",
  semiarido = "semi_arido", frontera = "fronteira_brasil"
)

# ==============================================================================
# HELPERS — MUNICIPALITY RESOLUTION (shared with main R/ file at runtime)
# NOTE: At package load time, these are provided by sus_data_filter_demographics.R.
#       Defined here so the roadmap script is self-contained when sourced directly.
# ==============================================================================

if (!exists("get_spatial_municipio_cache")) {
  get_spatial_municipio_cache <- function(cache_dir, use_cache, lang, verbose) {
    msg <- get_spatial_municipio_messages(lang)
    use_arrow  <- requireNamespace("arrow", quietly = TRUE)
    cache_file <- file.path(cache_dir,
                            if (use_arrow) "municipio_meta.parquet" else "municipio_meta.rds")
    if (use_cache && !dir.exists(cache_dir))
      dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    if (use_cache && file.exists(cache_file)) {
      if (verbose) cli::cli_alert_success(paste0(msg$loading_cache, basename(cache_file)))
      result <- tryCatch(
        if (use_arrow) as.data.frame(arrow::read_parquet(cache_file)) else readRDS(cache_file),
        error = function(e) NULL
      )
      if (!is.null(result)) return(result)
    }
    if (verbose) cli::cli_alert_info(msg$downloading_data)
    remote <- "https://github.com/ByMaxAnjos/climasus4r/raw/refs/heads/master/inst/data_4r/"
    spatial_df <- if (use_arrow)
      as.data.frame(suppressMessages(arrow::read_parquet(paste0(remote, "municipio_meta.parquet"))))
    else {
      tmp <- tempfile(fileext = ".rds")
      utils::download.file(paste0(remote, "municipio_meta.rds"), tmp, quiet = TRUE, mode = "wb")
      readRDS(tmp)
    }
    if (use_cache) tryCatch(
      { if (use_arrow) arrow::write_parquet(spatial_df, cache_file) else saveRDS(spatial_df, cache_file) },
      error = function(e) NULL
    )
    spatial_df
  }

  get_spatial_municipio_messages <- function(lang) {
    msgs <- list(
      en = list(loading_cache = "Loading from cache: ", downloading_data = "Downloading municipio data..."),
      pt = list(loading_cache = "Carregando do cache: ", downloading_data = "Baixando dados de municipios..."),
      es = list(loading_cache = "Cargando desde cache: ", downloading_data = "Descargando datos de municipios...")
    )
    msgs[[lang]] %||% msgs[["pt"]]
  }
}

if (!exists("resolve_city_input_internal")) {
  resolve_city_input_internal <- function(city, muni_meta, lang, verbose) {
    codes_found <- character(0)
    meta_norm <- stringi::stri_trans_general(tolower(muni_meta$no_accents), "Latin-ASCII")
    for (raw in city) {
      input <- trimws(as.character(raw))
      if (grepl("^\\d{6,7}$", input)) {
        idx <- if (nchar(input) == 7L) which(muni_meta$municipio == input)
               else which(substr(muni_meta$municipio, 1L, 6L) == input)
        if (length(idx) > 0L) codes_found <- c(codes_found, muni_meta$municipio[idx])
        else cli::cli_alert_warning("Municipality code {.val {input}} not found.")
        next
      }
      input_norm <- stringi::stri_trans_general(tolower(input), "Latin-ASCII")
      idx <- which(meta_norm == input_norm)
      if (length(idx) == 0L) {
        orig <- stringi::stri_trans_general(tolower(muni_meta$name), "Latin-ASCII")
        idx  <- which(orig == input_norm)
      }
      if (length(idx) > 0L) {
        codes_found <- c(codes_found, muni_meta$municipio[idx])
      } else {
        suggest_city_matches_internal(input, input_norm, meta_norm, muni_meta, lang)
      }
    }
    list(codes = unique(codes_found))
  }
}

if (!exists("suggest_city_matches_internal")) {
  suggest_city_matches_internal <- function(input, input_norm, meta_norm, muni_meta, lang) {
    msgs_nf <- list(pt = "Municipio {.val {input}} nao encontrado.",
                    en = "Municipality {.val {input}} not found.",
                    es = "Municipio {.val {input}} no encontrado.")
    cli::cli_alert_warning(msgs_nf[[lang]] %||% msgs_nf[["pt"]])
    prefix_idx <- which(startsWith(meta_norm, input_norm))
    max_dist   <- max(1L, min(3L, floor(nchar(input_norm) * 0.25)))
    distances  <- utils::adist(input_norm, meta_norm, ignore.case = TRUE)[1L, ]
    fuzzy_idx  <- which(distances <= max_dist)
    fuzzy_idx  <- fuzzy_idx[order(distances[fuzzy_idx])]
    close_idx  <- utils::head(unique(c(prefix_idx, fuzzy_idx)), 5L)
    if (length(close_idx) == 0L) {
      cli::cli_alert_info("No suggestions available. Check spelling or use the IBGE code.")
      return(invisible(NULL))
    }
    cli::cli_alert_info(switch(lang, pt = "Voce quis dizer:", en = "Did you mean:", es = "\u00bfQuiso decir:"))
    for (s in sprintf("%s (%s \u2014 %s)",
                      muni_meta$name[close_idx],
                      muni_meta$uf_code[close_idx],
                      muni_meta$municipio[close_idx])) cli::cli_li(s)
  }
}

if (!exists("check_city_uf_internal")) {
  check_city_uf_internal <- function(df, requested_codes, muni_col, muni_meta, lang, verbose) {
    if (!verbose || is.null(muni_col)) return(invisible(NULL))
    requested_ufs <- unique(muni_meta$uf_code[muni_meta$municipio %in% requested_codes])
    data_codes <- unique(as.character(df[[muni_col]]))
    data_codes <- data_codes[!is.na(data_codes) & nchar(data_codes) >= 6L]
    data_ufs   <- unique(muni_meta$uf_code[
      muni_meta$municipio %in% data_codes |
      substr(muni_meta$municipio, 1L, 6L) %in% data_codes
    ])
    outside_ufs <- setdiff(requested_ufs, data_ufs)
    if (length(outside_ufs) == 0L) return(invisible(NULL))
    outside_cities <- muni_meta$name[
      muni_meta$municipio %in% requested_codes & muni_meta$uf_code %in% outside_ufs
    ]
    msgs <- list(
      pt = c("!" = paste0("Municipios pertencem a estado(s) ausentes nos dados: ",
                          paste(outside_ufs, collapse = ", ")),
             "i" = paste0("Afetados: ", paste(utils::head(outside_cities, 5L), collapse = ", ")),
             ">" = "Verifique se os dados foram filtrados por UF em sus_data_import()."),
      en = c("!" = paste0("Municipalities belong to state(s) not in the data: ",
                          paste(outside_ufs, collapse = ", ")),
             "i" = paste0("Affected: ", paste(utils::head(outside_cities, 5L), collapse = ", ")),
             ">" = "Check if data was pre-filtered by state in sus_data_import()."),
      es = c("!" = paste0("Municipios pertenecen a estado(s) no presentes: ",
                          paste(outside_ufs, collapse = ", ")),
             "i" = paste0("Afectados: ", paste(utils::head(outside_cities, 5L), collapse = ", ")),
             ">" = "Verifique si los datos fueron filtrados por estado en sus_data_import().")
    )
    cli::cli_warn(msgs[[lang]] %||% msgs[["pt"]])
  }
}

if (!exists("show_available_municipalities_internal")) {
  show_available_municipalities_internal <- function(available_muni_codes, muni_meta,
                                                      cache_dir, use_cache, lang) {
    header <- switch(lang,
      "en" = "Municipalities available in the data (no match for your filter):",
      "pt" = "Municipios disponiveis nos dados (nenhum registro para o filtro informado):",
      "es" = "Municipios disponibles en los datos (sin coincidencia para el filtro):"
    )
    cli::cli_alert_warning(header)
    if (is.null(muni_meta))
      muni_meta <- tryCatch(
        get_spatial_municipio_cache(cache_dir, use_cache, lang, verbose = FALSE),
        error = function(e) NULL
      )
    codes  <- unique(available_muni_codes[!is.na(available_muni_codes) & nchar(available_muni_codes) >= 6L])
    if (!is.null(muni_meta) && length(codes) > 0L) {
      meta_6 <- substr(muni_meta$municipio, 1L, 6L)
      idx    <- match(codes, meta_6)
      need_fallback <- is.na(idx) & nchar(codes) == 7L
      if (any(need_fallback)) idx[need_fallback] <- match(codes[need_fallback], muni_meta$municipio)
      labels <- ifelse(!is.na(idx),
        sprintf("%s (%s) \u2014 %s", muni_meta$name[idx], muni_meta$uf_code[idx], codes),
        codes)
    } else { labels <- codes }
    for (lbl in sort(unique(labels))) cli::cli_li(lbl)
  }
}
