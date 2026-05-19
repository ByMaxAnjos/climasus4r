# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "variable_name", "code_link", "period", "variable_pt", "variable_en",
  "variable_es", "var_slug", "var_code", "id",
  "codigo", "nome_estacao", "uf", "mes", "decada", "valor", "mes_decada"
))

# ── Exported function ──────────────────────────────────────────────────────────

#' Download and Process INMET Climate Normals
#'
#' `sus_climate_normals()` retrieves 30-year climatological normals from the
#' Brazilian National Institute of Meteorology (INMET) for one of three
#' reference periods (1961-1990, 1981-2010, 1991-2020). It loads the variable
#' catalogue from the bundled `normal_meta.parquet` dictionary—with transparent
#' disk caching and automatic fallback download—then fetches each selected
#' variable's Excel file from the INMET portal, parses the decade-level monthly
#' structure, and returns a tidy long-format `climasus_df`.
#'
#' @section Public-health and environmental relevance:
#' Climate normals serve as the **climatological baseline** against which
#' observed exposures are compared in epidemiological studies:
#' \itemize{
#'   \item **Heat-health**: temperature normals reveal chronically hot
#'     municipalities for heat-wave attribution and heat-related mortality studies.
#'   \item **Precipitation extremes**: wet-day and dry-spell normals support
#'     flood and drought exposure assessments linked to diarrhoeal disease,
#'     leptospirosis, and waterborne outbreaks.
#'   \item **Vector-borne diseases**: humidity and temperature normals define
#'     the climatic envelope of *Aedes* mosquito habitat, informing dengue,
#'     chikungunya and Zika risk mapping.
#'   \item **Relative-risk baselines**: DLNM models built with
#'     [sus_mod_dlnm()] require a reference exposure; normals provide the
#'     station-level climatological mean for that reference.
#'   \item **Vulnerability indices**: combined with [sus_mod_vulnerability_index()],
#'     normals quantify chronic exposure as an IPCC *hazard* pillar component.
#' }
#'
#' @param period Character. Reference climatological period. One of
#'   `"1961-1990"`, `"1981-2010"`, or `"1991-2020"` (default).
#' @param target_var Character vector of `var_code` values to download
#'   (e.g. `c("t_max", "precipitation")`). `NULL` (default) downloads all
#'   variables available for the selected `period`. Run
#'   `sus_climate_normals_meta()` to inspect the catalogue.
#' @param cache_dir Character. Local directory for disk-cached files.
#'   Default: `"~/.climasus4r_cache/normals"`.
#' @param use_cache Logical. If `TRUE` (default), loads from and saves to
#'   `cache_dir`. Set to `FALSE` to force a fresh download.
#' @param lang Character. Message language: `"pt"` (default), `"en"`, `"es"`.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return A `climasus_df` object at stage `"climate"` and type `"normals"`,
#'   tidy long format with columns:
#' \describe{
#'   \item{`codigo`}{Character. INMET station identifier.}
#'   \item{`nome_estacao`}{Character. Station name.}
#'   \item{`uf`}{Character. Brazilian state abbreviation.}
#'   \item{`mes`}{Character. Month name (Portuguese, lower-case ASCII).}
#'   \item{`decada`}{Character. Decade within month (`"1"`, `"2"`, `"3"`).}
#'   \item{`valor`}{Numeric. Climatological normal value.}
#'   \item{`var_code`}{Character. Variable identifier from `normal_meta`.}
#'   \item{`variable_pt`}{Character. Variable label in Portuguese.}
#'   \item{`variable_en`}{Character. Variable label in English.}
#'   \item{`period`}{Character. Reference period (e.g. `"1991-2020"`).}
#' }
#'
#' @examples
#' \dontrun{
#' # Full 1991-2020 normals (all variables)
#' normals <- sus_climate_normals(period = "1991-2020")
#'
#' # Only temperature variables
#' temp_normals <- sus_climate_normals(
#'   period     = "1991-2020",
#'   target_var = c("t_max", "t_min", "t_mean_comp"),
#'   lang       = "pt"
#' )
#'
#' # Browse available variables first
#' meta <- sus_climate_normals_meta(period = "1991-2020")
#' }
#'
#' @seealso [sus_climate_normals_meta()], [sus_climate_inmet()],
#'   [sus_mod_dlnm()], [sus_mod_vulnerability_index()]
#'
#' @export
#' @importFrom dplyr filter select mutate across bind_rows as_tibble n_distinct
#' @importFrom tidyr pivot_longer separate
#' @importFrom stringi stri_trans_general stri_replace_all_regex stri_trans_tolower stri_sub
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom rlang .data
sus_climate_normals <- function(
    period     = "1991-2020",
    target_var = NULL,
    cache_dir  = "~/.climasus4r_cache/normals",
    use_cache  = TRUE,
    lang       = "pt",
    verbose    = TRUE
) {
  # ── Input validation ─────────────────────────────────────────────────────────
  lang <- tolower(lang)
  if (!lang %in% c("pt", "en", "es")) {
    cli::cli_alert_warning("Unsupported language {.val {lang}}. Using {.val pt}.")
    lang <- "pt"
  }

  valid_periods <- c("1961-1990", "1981-2010", "1991-2020")
  if (!period %in% valid_periods) {
    cli::cli_abort(c(
      "Invalid {.arg period}: {.val {period}}.",
      "i" = "Must be one of: {.val {valid_periods}}."
    ))
  }

  msg <- .normals_messages(lang)

  if (verbose) cli::cli_h1(msg$title)

  # ── Load metadata dictionary ──────────────────────────────────────────────────
  if (verbose) cli::cli_alert_info(msg$loading_dict)
  normal_meta <- .get_normal_meta(cache_dir, use_cache, verbose, msg)

  period_vars <- normal_meta |> dplyr::filter(.data$period == !!period)

  if (nrow(period_vars) == 0) {
    cli::cli_abort("No variables found for period {.val {period}}.")
  }

  if (!is.null(target_var)) {
    period_vars <- period_vars |> dplyr::filter(.data$var_code %in% target_var)
    if (nrow(period_vars) == 0) {
      cli::cli_abort(c(
        "None of the requested {.arg target_var} codes found for period {.val {period}}.",
        "i" = "Run {.fn sus_climate_normals_meta} to see available codes."
      ))
    }
  }

  if (verbose) {
    cli::cli_alert_info(paste(msg$found_vars, nrow(period_vars)))
  }

  # ── Download each variable ────────────────────────────────────────────────────
  all_data <- vector("list", nrow(period_vars))

  for (i in seq_len(nrow(period_vars))) {
    row      <- period_vars[i, ]
    var_label <- switch(lang, en = row$variable_en, es = row$variable_es, row$variable_pt)
    if (verbose) cli::cli_alert_info(paste(msg$downloading, var_label))

    var_data <- .download_normal_var(
      code_link = row$code_link,
      var_code  = row$var_code,
      period    = period,
      cache_dir = cache_dir,
      use_cache = use_cache,
      verbose   = verbose,
      msg       = msg
    )

    if (!is.null(var_data) && nrow(var_data) > 0) {
      var_data$variable_pt <- row$variable_pt
      var_data$variable_en <- row$variable_en
      var_data$variable_es <- row$variable_es
      var_data$period      <- period
      all_data[[i]] <- var_data
    }
  }

  all_data <- Filter(Negate(is.null), all_data)

  if (length(all_data) == 0) {
    cli::cli_abort(msg$no_data)
  }

  result <- dplyr::bind_rows(all_data)

  # ── Promote to climasus_df ────────────────────────────────────────────────────
  meta <- list(
    system         = NULL,
    stage          = "climate",
    type           = "normals",
    period         = period,
    n_stations     = dplyr::n_distinct(result$codigo, na.rm = TRUE),
    n_observations = nrow(result),
    n_variables    = dplyr::n_distinct(result$var_code),
    created        = Sys.time(),
    modified       = Sys.time(),
    history        = sprintf(
      "[%s] INMET climate normals imported — period: %s",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"), period
    ),
    user = list()
  )

  base_classes <- setdiff(class(result), "climasus_df")
  result <- structure(result, sus_meta = meta, class = c("climasus_df", base_classes))

  if (verbose) {
    cli::cli_alert_success(paste0(
      msg$done, " ",
      nrow(result), " rows | ",
      dplyr::n_distinct(result$var_code), " variables | ",
      dplyr::n_distinct(result$codigo, na.rm = TRUE), " stations."
    ))
  }

  result
}

# ── Companion catalogue function ───────────────────────────────────────────────

#' Browse the INMET Climate Normals Catalogue
#'
#' Returns the `normal_meta` dictionary as a tibble so you can inspect
#' available variables and their `var_code` values before calling
#' [sus_climate_normals()].
#'
#' @param period Character. Filter to a specific period, or `NULL` (default)
#'   to return the full catalogue.
#' @param lang Character. Language for the returned label column:
#'   `"pt"` (default), `"en"`, `"es"`.
#' @param cache_dir Character. Cache directory. Default:
#'   `"~/.climasus4r_cache/normals"`.
#' @param use_cache Logical. Use cached catalogue. Default `TRUE`.
#' @param verbose Logical. Print messages. Default `FALSE`.
#'
#' @return A tibble with columns `var_code`, `variable_label`, `period`,
#'   `var_slug`, and `code_link`.
#'
#' @examples
#' \dontrun{
#' sus_climate_normals_meta()
#' sus_climate_normals_meta(period = "1991-2020", lang = "en")
#' }
#'
#' @export
#' @importFrom dplyr filter select mutate
#' @importFrom rlang .data
sus_climate_normals_meta <- function(
    period    = NULL,
    lang      = "pt",
    cache_dir = "~/.climasus4r_cache/normals",
    use_cache = TRUE,
    verbose   = FALSE
) {
  lang <- tolower(lang)
  if (!lang %in% c("pt", "en", "es")) lang <- "pt"

  msg  <- .normals_messages(lang)
  meta <- .get_normal_meta(cache_dir, use_cache, verbose, msg)

  label_col <- switch(lang, en = "variable_en", es = "variable_es", "variable_pt")

  out <- meta |>
    dplyr::mutate(variable_label = .data[[label_col]]) |>
    dplyr::select("var_code", "variable_label", "period", "var_slug", "code_link")

  if (!is.null(period)) {
    out <- out |> dplyr::filter(.data$period == !!period)
  }

  out
}

# ── Internal: load / cache / download normal_meta ──────────────────────────────

#' @keywords internal
#' @noRd
.get_normal_meta <- function(cache_dir, use_cache, verbose, msg) {
  use_arrow  <- requireNamespace("arrow", quietly = TRUE)
  cache_dir  <- path.expand(cache_dir)
  cache_file <- file.path(
    cache_dir,
    if (use_arrow) "normal_meta.parquet" else "normal_meta.rds"
  )

  # 1. Try bundled package file first
  pkg_file <- system.file("data_4r", "normal_meta.parquet", package = "climasus4r")
  if (nzchar(pkg_file) && file.exists(pkg_file)) {
    result <- tryCatch(
      {
        if (use_arrow) as.data.frame(arrow::read_parquet(pkg_file))
        else           as.data.frame(arrow::read_parquet(pkg_file))
      },
      error = function(e) NULL
    )
    if (!is.null(result) && nrow(result) > 0) return(dplyr::as_tibble(result))
  }

  # 2. Ensure cache directory exists
  if (use_cache && !dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    if (verbose) cli::cli_alert_info(paste0(msg$creating_cache, cache_dir))
  }

  # 3. Try loading from cache
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
    if (!is.null(result)) return(dplyr::as_tibble(result))
  }

  # 4. Download from climasus Data Center
  if (verbose) cli::cli_alert_info(msg$downloading_dict)

  remote_base <- "https://github.com/ByMaxAnjos/climasus4r/raw/refs/heads/master/inst/data_4r/"
  normal_df <- if (use_arrow) {
    as.data.frame(
      suppressMessages(arrow::read_parquet(paste0(remote_base, "normal_meta.parquet")))
    )
  } else {
    url <- paste0(remote_base, "normal_meta.rds")
    tmp <- tempfile(fileext = ".rds")
    utils::download.file(url, tmp, quiet = TRUE, mode = "wb")
    readRDS(tmp)
  }

  # 5. Save to cache
  if (use_cache) {
    if (verbose) cli::cli_alert_info(msg$saving_cache)
    tryCatch(
      {
        if (use_arrow) arrow::write_parquet(normal_df, cache_file)
        else           saveRDS(normal_df, cache_file)
        if (verbose) cli::cli_alert_success(msg$cache_saved)
      },
      error = function(e) {
        if (verbose) cli::cli_alert_warning(paste0(msg$cache_save_error, e$message))
      }
    )
  }

  dplyr::as_tibble(normal_df)
}

# ── Internal: download and cache one variable's Excel file ─────────────────────

#' @keywords internal
#' @noRd
.download_normal_var <- function(
    code_link, var_code, period,
    cache_dir, use_cache, verbose, msg
) {
  cache_dir  <- path.expand(cache_dir)
  period_dir <- file.path(cache_dir, period)
  if (!dir.exists(period_dir)) dir.create(period_dir, recursive = TRUE, showWarnings = FALSE)

  cache_file <- file.path(period_dir, paste0(var_code, ".rds"))

  if (use_cache && file.exists(cache_file)) {
    if (verbose) cli::cli_alert_success(paste0(msg$loading_cache, basename(cache_file)))
    result <- tryCatch(readRDS(cache_file), error = function(e) NULL)
    if (!is.null(result)) return(result)
  }

  base_url  <- "https://portal.inmet.gov.br/uploads/normais/"
  url       <- paste0(base_url, code_link, ".xlsx")
  temp_file <- tempfile(fileext = ".xlsx")

  options(HTTPUserAgent = paste0(
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
    "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
  ))

  dl_ok <- tryCatch({
    withr_old <- getOption("timeout")
    options(timeout = 300)
    on.exit(options(timeout = withr_old), add = TRUE)
    utils::download.file(url, temp_file, mode = "wb", quiet = !verbose)
    TRUE
  }, error = function(e) {
    cli::cli_alert_warning(paste(msg$download_failed, var_code, ":", e$message))
    FALSE
  })

  if (!dl_ok || !file.exists(temp_file)) return(NULL)

  data <- .read_inmet_normals_excel(temp_file, var_code)
  unlink(temp_file)

  if (use_cache && !is.null(data) && nrow(data) > 0) {
    tryCatch(
      saveRDS(data, cache_file),
      error = function(e) {
        if (verbose) cli::cli_alert_warning(paste0(msg$cache_save_error, e$message))
      }
    )
    if (verbose) cli::cli_alert_success(msg$cache_saved)
  }

  data
}

# ── Internal: parse INMET climate normals Excel file ──────────────────────────

#' @keywords internal
#' @noRd
.read_inmet_normals_excel <- function(file_path, var_code) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg readxl} is required to parse INMET Excel files. Install it with {.run install.packages(\"readxl\")}."
    )
  }

  tryCatch({
    # INMET Excel structure (after skip=2, col_names=FALSE):
    #   raw[1,] = month names row (JANEIRO, FEVEREIRO, ...) with merged cells
    #             → first col of each merge has the name; continuation cols are NA
    #   raw[2:,] = station data rows (no separate decade sub-header row)
    # Decade (1/2/3) is inferred from the position within each month's merged
    # cell group: the named cell = decade 1, first NA = decade 2, second NA = 3.
    raw <- readxl::read_xlsx(file_path, skip = 2, col_names = FALSE)

    header_months <- as.character(raw[1, ])
    data          <- raw[2:nrow(raw), ]

    # Build compound column names: month_decade (e.g. "janeiro_1")
    col_names     <- c("codigo", "nome_estacao", "uf")
    current_month <- ""
    month_pos     <- 0L

    for (i in seq(4L, length(header_months))) {
      m <- header_months[[i]]
      if (!is.na(m) && nzchar(m) && m != "NA") {
        current_month <- m |>
          stringi::stri_trans_general("Latin-ASCII") |>
          stringi::stri_replace_all_regex("[^[:alnum:]]+", "_") |>
          stringi::stri_trans_tolower() |>
          stringi::stri_replace_all_regex("^_|_$", "")
        month_pos <- 1L
      } else {
        month_pos <- month_pos + 1L
      }

      col_names <- c(col_names, paste0(current_month, "_", month_pos))
    }

    # make.unique handles rare duplicate headers (e.g. edge cases in some files)
    col_names <- make.unique(col_names, sep = "_d")

    if (length(col_names) != ncol(data)) {
      cli::cli_alert_warning(paste0(
        "Column count mismatch for ", var_code, ": expected ", ncol(data),
        " got ", length(col_names), ". Skipping variable."
      ))
      return(NULL)
    }

    colnames(data) <- col_names
    data$codigo       <- as.character(data$codigo)
    data$nome_estacao <- as.character(data$nome_estacao)
    data$uf           <- as.character(data$uf)
    for (j in seq(4L, ncol(data))) data[[j]] <- as.character(data[[j]])

    data_long <- dplyr::as_tibble(data) |>
      tidyr::pivot_longer(
        cols      = -c("codigo", "nome_estacao", "uf"),
        names_to  = "mes_decada",
        values_to = "valor"
      ) |>
      tidyr::separate(
        "mes_decada",
        into   = c("mes", "decada"),
        sep    = "_(?=[0-9]$)",
        remove = TRUE,
        fill   = "right",
        extra  = "merge"
      ) |>
      dplyr::mutate(
        valor    = suppressWarnings(as.numeric(
          stringi::stri_replace_all_regex(.data$valor, "^-$", NA_character_)
        )),
        var_code = var_code
      ) |>
      dplyr::select("codigo", "nome_estacao", "uf", "mes", "decada", "valor", "var_code")

    data_long
  }, error = function(e) {
    cli::cli_alert_warning(paste("Error reading Excel for", var_code, ":", e$message))
    NULL
  })
}

# ── Internal: multilingual messages ───────────────────────────────────────────

#' @keywords internal
#' @noRd
.normals_messages <- function(lang = "pt") {
  msgs <- list(
    pt = list(
      title          = "Normais Climatológicas INMET",
      loading_dict   = "Carregando catálogo de variáveis (normal_meta)...",
      found_vars     = "Variáveis encontradas para o período:",
      downloading    = "Baixando:",
      downloading_dict = "Baixando catálogo de normais do climasus Data Center...",
      no_data        = "Nenhum dado foi baixado. Verifique a conexão e os códigos de variável.",
      done           = "Download concluído.",
      creating_cache = "Criando diretório de cache: ",
      loading_cache  = "Carregando do cache: ",
      cache_error    = "Falha ao carregar cache. Baixando dados novos...",
      saving_cache   = "Salvando no cache...",
      cache_saved    = "Dados armazenados em cache com sucesso.",
      cache_save_error = "Erro ao salvar cache: ",
      download_failed  = "Falha ao baixar"
    ),
    en = list(
      title          = "INMET Climate Normals",
      loading_dict   = "Loading variables catalogue (normal_meta)...",
      found_vars     = "Variables found for period:",
      downloading    = "Downloading:",
      downloading_dict = "Downloading normals catalogue from climasus Data Center...",
      no_data        = "No data downloaded. Check connection and variable codes.",
      done           = "Download complete.",
      creating_cache = "Creating cache directory: ",
      loading_cache  = "Loading from cache: ",
      cache_error    = "Cache loading failed. Downloading fresh data...",
      saving_cache   = "Saving to cache...",
      cache_saved    = "Data cached successfully.",
      cache_save_error = "Cache save error: ",
      download_failed  = "Failed to download"
    ),
    es = list(
      title          = "Normales Climatológicas INMET",
      loading_dict   = "Cargando catálogo de variables (normal_meta)...",
      found_vars     = "Variables encontradas para el período:",
      downloading    = "Descargando:",
      downloading_dict = "Descargando catálogo de normales del climasus Data Center...",
      no_data        = "No se descargaron datos. Verifique la conexión y los códigos.",
      done           = "Descarga completada.",
      creating_cache = "Creando directorio de cache: ",
      loading_cache  = "Cargando desde cache: ",
      cache_error    = "Fallo al cargar cache. Descargando datos nuevos...",
      saving_cache   = "Guardando en cache...",
      cache_saved    = "Datos almacenados en cache con éxito.",
      cache_save_error = "Error al guardar cache: ",
      download_failed  = "Error al descargar"
    )
  )
  msgs[[lang]] %||% msgs[["pt"]]
}
