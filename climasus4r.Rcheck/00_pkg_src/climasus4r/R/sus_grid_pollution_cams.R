# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "code_muni", "date", "value", "pollutant", "metric_col",
  "n_files", "n_rows", "filename", "bad", "valid", "err"
))

# ── Exported function ─────────────────────────────────────────────────────────

#' Import CAMS Pollution Data for Brazilian Municipalities
#'
#' @description
#' `sus_grid_pollution_cams()` downloads pre-processed CAMS (Copernicus
#' Atmosphere Monitoring Service) daily pollution data for Brazilian
#' municipalities from Zenodo. No API key is required.
#'
#' Data cover six pollutants — PM2.5, PM10, CO, O3, NO2 and SO2 — aggregated
#' to daily means/maxima/minima per municipality from 2003 to 2024. Files are
#' hosted on Zenodo (CC-BY) as Parquet and are downloaded once and cached
#' locally.
#'
#' The return value is a `climasus_df` at `stage = "climate"`,
#' `type = "pollution_cams"`, directly compatible with
#' [sus_climate_anomaly()], [sus_climate_aggregate()], and [sus_mod_dlnm()].
#'
#' @param pollutants Character vector. Pollutants to include. Allowed values:
#'   `"pm25"`, `"pm10"`, `"co"`, `"o3"`, `"no2"`, `"so2"`, or `"all"`.
#'   Default: `c("pm25", "pm10")`.
#'
#' @param metric Character. Daily aggregation statistic. One of:
#'   `"mean"` (default), `"max"`, `"min"`, or `"all"` (all three).
#'
#' @param years Integer vector. Years to include (2003–2024). `NULL` (default)
#'   returns all available years.
#'
#' @param use_cache Logical. Re-use previously downloaded Parquet files.
#'   Default `TRUE`.
#'
#' @param cache_dir Character. Directory for cached Parquet files.
#'   Default `"~/.climasus4r_cache/cams"`.
#'
#' @param lang Character. Message language: `"pt"` (default), `"en"`, `"es"`.
#'
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return A `climasus_df` tibble with columns:
#'   \itemize{
#'     \item `code_muni` — 7-digit IBGE municipality code (character).
#'     \item `date` — Date.
#'     \item `{pollutant}_{metric}` — one numeric column per requested
#'       pollutant × metric combination (e.g. `pm25_mean`, `no2_max`).
#'   }
#'   Metadata: `stage = "climate"`, `type = "pollution_cams"`.
#'
#' @section Data source:
#' Saldanha, R. et al. CAMS pollution daily averages for Brazilian
#' municipalities (2003–2024). Zenodo. CC-BY 4.0.\cr
#' PM2.5: \doi{10.5281/zenodo.16374139} |
#' PM10: \doi{10.5281/zenodo.16419737} |
#' CO: \doi{10.5281/zenodo.18641834} |
#' O3: \doi{10.5281/zenodo.18641945} |
#' NO2: \doi{10.5281/zenodo.18642048} |
#' SO2: \doi{10.5281/zenodo.18642198}
#'
#' @section Units:
#' \itemize{
#'   \item PM2.5, PM10, O3, NO2, SO2: µg/m³
#'   \item CO: ppm
#' }
#'
#' @examples
#' \dontrun{
#' # PM2.5 and NO2 daily means for 2019-2022
#' cams <- sus_grid_pollution_cams(
#'   pollutants = c("pm25", "no2"),
#'   metric     = "mean",
#'   years      = 2019:2022,
#'   lang       = "pt"
#' )
#'
#' sus_meta(cams, "stage")  # "climate"
#' sus_meta(cams, "type")   # "pollution_cams"
#'
#' # All pollutants, all metrics, all years
#' cams_full <- sus_grid_pollution_cams(
#'   pollutants = "all",
#'   metric     = "all",
#'   lang       = "en"
#' )
#' }
#'
#' @seealso [sus_climate_anomaly()], [sus_climate_aggregate()], [sus_mod_dlnm()]
#'
#' @export
#' @importFrom rlang .data is_installed
#' @importFrom dplyr filter select rename collect left_join all_of join_by full_join
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom lubridate year
sus_grid_pollution_cams <- function(
    pollutants = c("pm25", "pm10"),
    metric     = "mean",
    years      = NULL,
    use_cache  = TRUE,
    cache_dir  = "~/.climasus4r_cache/cams",
    lang       = "pt",
    verbose    = TRUE) {

  # ── 1. Validation ───────────────────────────────────────────────────────────

  if (!is.character(lang) || length(lang) != 1 || !lang %in% c("pt", "en", "es")) {
    cli::cli_abort("{.arg lang} must be one of 'pt', 'en', or 'es'.")
  }
  msg <- .cams_msgs[[lang]]

  valid_pollutants <- names(.cams_zenodo_ids)

  if (identical(pollutants, "all")) {
    pollutants <- valid_pollutants
  } else {
    if (!is.character(pollutants) || length(pollutants) == 0) {
      cli::cli_abort(msg$invalid_pollutants_type)
    }
    bad <- paste(setdiff(pollutants, valid_pollutants), collapse = ", ")
    if (nchar(bad) > 0) {
      valid <- paste(valid_pollutants, collapse = ", ")
      cli::cli_abort(msg$invalid_pollutants)
    }
  }

  valid_metrics <- c("mean", "max", "min")
  if (identical(metric, "all")) {
    metric <- valid_metrics
  } else {
    if (!is.character(metric) || length(metric) == 0 ||
        !all(metric %in% valid_metrics)) {
      bad   <- paste(setdiff(metric, valid_metrics), collapse = ", ")
      valid <- paste(valid_metrics, collapse = ", ")
      cli::cli_abort(msg$invalid_metric)
    }
  }

  if (!is.null(years)) {
    if (!is.numeric(years) || any(is.na(years))) {
      cli::cli_abort(msg$invalid_years_type)
    }
    years <- as.integer(years)
    bad_y <- years[years < 2003L | years > 2024L]
    if (length(bad_y) > 0) {
      bad <- paste(bad_y, collapse = ", ")
      cli::cli_abort(c(msg$invalid_years_range, "x" = msg$invalid_years_x))
    }
  }

  if (!is.logical(use_cache) || length(use_cache) != 1) {
    cli::cli_abort(msg$invalid_use_cache)
  }
  if (!is.character(cache_dir) || nchar(trimws(cache_dir)) == 0) {
    cli::cli_abort(msg$invalid_cache_dir)
  }
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)

  # ── 2. Build download manifest ───────────────────────────────────────────────
  # One row per (pollutant, metric) combination
  manifest <- expand.grid(
    pollutant = pollutants,
    metric    = metric,
    stringsAsFactors = FALSE
  )
  manifest$record_id   <- .cams_zenodo_ids[manifest$pollutant]
  manifest$filename    <- paste0(manifest$pollutant, "_", manifest$metric, "_mean.parquet")
  manifest$out_col     <- paste0(manifest$pollutant, "_", manifest$metric)
  manifest$url         <- paste0(
    "https://zenodo.org/records/", manifest$record_id,
    "/files/", manifest$filename, "?download=1"
  )
  manifest$cache_path  <- file.path(cache_dir, manifest$filename)

  n_files <- nrow(manifest)
  if (verbose) {
    cli::cli_h1(msg$title)
    cli::cli_alert_info(glue::glue(msg$download_start, n_files = n_files))
  }

  # ── 3. Download with cache ───────────────────────────────────────────────────
  for (i in seq_len(n_files)) {
    .cams_download_file(
      url        = manifest$url[i],
      cache_path = manifest$cache_path[i],
      use_cache  = use_cache,
      verbose    = verbose,
      msg        = msg
    )
  }

  # ── 4. Read, filter years, normalize columns ─────────────────────────────────
  # Process each (pollutant, metric) and collect a tibble per combination
  result_list <- vector("list", n_files)

  for (i in seq_len(n_files)) {
    cache_path <- manifest$cache_path[i]
    out_col    <- manifest$out_col[i]

    if (is.na(cache_path) || !file.exists(cache_path)) {
      cli::cli_alert_warning(
        glue::glue(msg$skip_missing, filename = manifest$filename[i]))
      next
    }

    df_i <- tryCatch({
      ds <- arrow::open_dataset(cache_path)

      # Filter years lazily
      if (!is.null(years)) {
        ds <- ds |> dplyr::filter(lubridate::year(.data$date) %in% years)
      }

      raw <- dplyr::collect(ds)

      # Normalize: detect the value column (not code_muni, not date)
      id_cols    <- intersect(names(raw), c("code_muni", "id_municipio",
                                             "geocodigo", "cod_municipio"))
      date_cols  <- intersect(names(raw), c("date", "data", "dt"))
      value_cols <- setdiff(names(raw), c(id_cols, date_cols))

      if (length(id_cols) == 0 || length(date_cols) == 0) {
        cli::cli_abort(
          glue::glue(msg$bad_schema, filename = manifest$filename[i],
                     cols = paste(names(raw), collapse = ", ")))
      }

      # Standardise to code_muni / date / {out_col}
      out <- raw |>
        dplyr::rename(
          code_muni = dplyr::all_of(id_cols[1]),
          date      = dplyr::all_of(date_cols[1])
        )

      if (length(value_cols) == 1) {
        out <- dplyr::rename(out, !!out_col := dplyr::all_of(value_cols[1]))
      } else if (length(value_cols) > 1) {
        # Multiple value columns: keep only the one matching the metric name
        # (e.g., "pm25_mean_mean" or "pm25_mean"); fall back to first
        match_col <- value_cols[grep(out_col, value_cols, fixed = TRUE)]
        if (length(match_col) == 0) match_col <- value_cols[1]
        out <- out |>
          dplyr::select(dplyr::all_of(c("code_muni", "date")),
                        !!out_col := dplyr::all_of(match_col[1]))
      } else {
        cli::cli_abort(
          glue::glue(msg$no_value_col, filename = manifest$filename[i]))
      }

      # Ensure types
      out$code_muni <- as.character(out$code_muni)
      out$date      <- as.Date(out$date)

      out
    }, error = function(e) {
      cli::cli_warn(c(
        glue::glue(msg$read_warn, filename = manifest$filename[i]),
        "i" = conditionMessage(e)
      ))
      NULL
    })

    result_list[[i]] <- df_i
  }

  # ── 5. Merge all pollutant × metric columns ──────────────────────────────────
  result_list <- result_list[!vapply(result_list, is.null, logical(1))]
  if (length(result_list) == 0) {
    cli::cli_abort(msg$no_data)
  }

  # Full join on (code_muni, date) so all combinations are preserved
  result <- result_list[[1]]
  if (length(result_list) > 1) {
    for (j in 2:length(result_list)) {
      result <- dplyr::full_join(
        result,
        result_list[[j]],
        by = join_by(code_muni, date)
      )
    }
  }

  result <- result[order(result$code_muni, result$date), ]
  rownames(result) <- NULL
  result <- tibble::as_tibble(result)

  n_rows <- nrow(result)
  if (verbose) {
    cli::cli_alert_success(glue::glue(msg$done, n_rows = n_rows))
  }

  # ── 6. Build climasus_df ─────────────────────────────────────────────────────
  meta <- list(
    system  = NULL,
    stage   = "climate",
    type    = "pollution_cams",
    spatial = FALSE,
    temporal = list(
      start  = min(result$date, na.rm = TRUE),
      end    = max(result$date, na.rm = TRUE),
      unit   = "day",
      source = "zenodo_cams"
    ),
    created          = Sys.time(),
    modified         = Sys.time(),
    pollutants       = pollutants,
    metric           = metric,
    years            = if (is.null(years)) 2003:2024 else years,
    n_municipalities = dplyr::n_distinct(result$code_muni),
    n_observations   = n_rows,
    history          = sprintf(
      "[%s] sus_grid_pollution_cams(): %d pollutant(s) x %d metric(s), %d obs",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      length(pollutants), length(metric), n_rows
    ),
    user = list()
  )

  base_classes <- setdiff(class(result), "climasus_df")
  structure(result, sus_meta = meta, class = c("climasus_df", base_classes))
}


# ── Internal constants ────────────────────────────────────────────────────────

#' Zenodo record IDs for CAMS pollution data, by pollutant
#' @keywords internal
#' @noRd
.cams_zenodo_ids <- c(
  pm25 = "16374139",
  pm10 = "16419737",
  co   = "18641834",
  o3   = "18641945",
  no2  = "18642048",
  so2  = "18642198"
)

#' Measurement units for CAMS pollutants
#' @keywords internal
#' @noRd
.cams_units <- c(
  pm25 = "ug/m3",
  pm10 = "ug/m3",
  co   = "ppm",
  o3   = "ug/m3",
  no2  = "ug/m3",
  so2  = "ug/m3"
)

#' Multilingual messages for sus_grid_pollution_cams()
#' @keywords internal
#' @noRd
.cams_msgs <- list(
  pt = list(
    title                  = "Dados CAMS de Polui\u00e7\u00e3o Atmosf\u00e9rica",
    invalid_pollutants_type = "{.arg pollutants} deve ser um vetor de caracteres.",
    invalid_pollutants     = "{.arg pollutants} inv\u00e1lido(s): {bad}. Use: {valid}.",
    invalid_metric         = "{.arg metric} inv\u00e1lido: {bad}. Use: {valid}.",
    invalid_years_type     = "{.arg years} deve ser um vetor num\u00e9rico sem NA.",
    invalid_years_range    = "{.arg years} deve estar entre 2003 e 2024.",
    invalid_years_x        = "Ano(s) inv\u00e1lido(s): {bad}.",
    invalid_use_cache      = "{.arg use_cache} deve ser TRUE ou FALSE.",
    invalid_cache_dir      = "{.arg cache_dir} deve ser uma string n\u00e3o vazia.",
    download_start         = "Baixando {n_files} arquivo(s) CAMS do Zenodo...",
    cache_hit              = "Cache encontrado: {filename}",
    download_file          = "Baixando: {filename}",
    download_done          = "Conclu\u00eddo: {filename}",
    download_error         = "Falha ao baixar {filename}: {err}",
    skip_missing           = "Arquivo n\u00e3o encontrado no cache: {filename}",
    bad_schema             = "Schema inesperado em {filename} (colunas: {cols}).",
    no_value_col           = "Nenhuma coluna de valor encontrada em {filename}.",
    read_warn              = "N\u00e3o foi poss\u00edvel ler {filename}.",
    no_data                = "Nenhum dado foi lido com sucesso.",
    done                   = "Conclu\u00eddo: {n_rows} observa\u00e7\u00f5es carregadas."
  ),
  en = list(
    title                  = "CAMS Atmospheric Pollution Data",
    invalid_pollutants_type = "{.arg pollutants} must be a character vector.",
    invalid_pollutants     = "Invalid {.arg pollutants}: {bad}. Use: {valid}.",
    invalid_metric         = "Invalid {.arg metric}: {bad}. Use: {valid}.",
    invalid_years_type     = "{.arg years} must be a numeric vector without NA.",
    invalid_years_range    = "{.arg years} must be between 2003 and 2024.",
    invalid_years_x        = "Invalid year(s): {bad}.",
    invalid_use_cache      = "{.arg use_cache} must be TRUE or FALSE.",
    invalid_cache_dir      = "{.arg cache_dir} must be a non-empty string.",
    download_start         = "Downloading {n_files} CAMS file(s) from Zenodo...",
    cache_hit              = "Cache found: {filename}",
    download_file          = "Downloading: {filename}",
    download_done          = "Done: {filename}",
    download_error         = "Failed to download {filename}: {err}",
    skip_missing           = "File not found in cache: {filename}",
    bad_schema             = "Unexpected schema in {filename} (columns: {cols}).",
    no_value_col           = "No value column found in {filename}.",
    read_warn              = "Could not read {filename}.",
    no_data                = "No data was successfully loaded.",
    done                   = "Complete: {n_rows} observations loaded."
  ),
  es = list(
    title                  = "Datos CAMS de Contaminaci\u00f3n Atmosf\u00e9rica",
    invalid_pollutants_type = "{.arg pollutants} debe ser un vector de caracteres.",
    invalid_pollutants     = "{.arg pollutants} inv\u00e1lido(s): {bad}. Use: {valid}.",
    invalid_metric         = "{.arg metric} inv\u00e1lido: {bad}. Use: {valid}.",
    invalid_years_type     = "{.arg years} debe ser un vector num\u00e9rico sin NA.",
    invalid_years_range    = "{.arg years} debe estar entre 2003 y 2024.",
    invalid_years_x        = "A\u00f1o(s) inv\u00e1lido(s): {bad}.",
    invalid_use_cache      = "{.arg use_cache} debe ser TRUE o FALSE.",
    invalid_cache_dir      = "{.arg cache_dir} debe ser una cadena no vac\u00eda.",
    download_start         = "Descargando {n_files} archivo(s) CAMS de Zenodo...",
    cache_hit              = "Cach\u00e9 encontrado: {filename}",
    download_file          = "Descargando: {filename}",
    download_done          = "Completado: {filename}",
    download_error         = "Error al descargar {filename}: {err}",
    skip_missing           = "Archivo no encontrado en cach\u00e9: {filename}",
    bad_schema             = "Esquema inesperado en {filename} (columnas: {cols}).",
    no_value_col           = "Ninguna columna de valor encontrada en {filename}.",
    read_warn              = "No se pudo leer {filename}.",
    no_data                = "No se carg\u00f3 ning\u00fan dato correctamente.",
    done                   = "Completo: {n_rows} observaciones cargadas."
  )
)


# ── Internal helpers ──────────────────────────────────────────────────────────

#' Download one CAMS Parquet file, using cache if available
#' @keywords internal
#' @noRd
.cams_download_file <- function(url, cache_path, use_cache, verbose, msg) {
  filename <- basename(cache_path)

  if (use_cache && file.exists(cache_path) && file.size(cache_path) > 0) {
    if (verbose) {
      cli::cli_alert_success(glue::glue(msg$cache_hit, filename = filename))
    }
    return(invisible(cache_path))
  }

  dir_path <- dirname(cache_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }

  if (verbose) {
    cli::cli_alert_info(glue::glue(msg$download_file, filename = filename))
  }

  tryCatch({
    utils::download.file(url = url, destfile = cache_path,
                         mode = "wb", quiet = !verbose, method = "auto")
    if (verbose) {
      cli::cli_alert_success(glue::glue(msg$download_done, filename = filename))
    }
    invisible(cache_path)
  }, error = function(e) {
    if (file.exists(cache_path)) unlink(cache_path)
    cli::cli_warn(glue::glue(msg$download_error,
                             filename = filename,
                             err      = conditionMessage(e)))
    invisible(NA_character_)
  })
}
