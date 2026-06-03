# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "code_muni", "date", "year", "cell_id",
  "fstdate", "lstdate", "SV", "Lon", "Lat",
  "n_fd_events", "fd_total_days", "fd_mean_severity", "fd_max_severity",
  "duration_days", ".data", "bad", "valid", "err"
))

# ── Exported function ─────────────────────────────────────────────────────────

#' Import Soil Moisture Volatility Index (SMVI / Flash Drought) Data
#'
#' @description
#' `sus_grid_smvi()` downloads the global flash drought inventory based on
#' the Soil Moisture Volatility Index (SMVI; Osman et al., 2024), spatially
#' assigns events to Brazilian municipalities, and returns annual or monthly
#' flash drought statistics as a `climasus_df` compatible with
#' [sus_mod_dlnm()] and [sus_mod_vulnerability_index()].
#'
#' **What is SMVI?** SMVI detects *flash droughts* — rapid, high-impact soil
#' moisture deficits that develop within days to weeks. Unlike SPI/SPEI/PDSI
#' (which measure cumulative drought severity), SMVI captures the *speed* of
#' soil moisture decline by comparing a 5-day running average of root-zone
#' soil moisture (0–100 cm) against a 20-day running average. A flash drought
#' event begins when the 5-day average drops below the 20-day average AND
#' reaches the 20th percentile deficit threshold.
#'
#' **Why SMVI is different from SPI/SPEI/PDSI:**
#' \itemize{
#'   \item SPI/SPEI/PDSI measure *how dry* a period is (severity)
#'   \item SMVI measures *how fast* a drought develops (onset speed) —
#'     flash droughts can devastate crops, trigger wildfires, and
#'     compromise water supply with little warning
#' }
#'
#' **Health connections in Brazil:**
#' \itemize{
#'   \item **Malnutrition & food insecurity**: flash droughts destroy crops
#'     within weeks; 1–3 month lag to increased child malnutrition in
#'     semi-arid Northeast and Amazônia
#'   \item **Leptospirosis**: rapid soil rewetting after flash drought
#'     events concentrates rodents near water, increasing transmission;
#'     1–4 week lag
#'   \item **Leishmaniasis**: drought-driven human migration to water
#'     sources increases sandfly contact; 2-year lag documented in Bahia
#'   \item **Waterborne disease**: water shortage during flash drought
#'     increases dependence on unclean sources
#' }
#'
#' **Data source**: Global Flash Drought Inventory (Osman et al., 2024).
#' Based on NASA GLDAS-2 root-zone soil moisture (1990–2021). No
#' authentication required. Events file (109 MB) covers the full global
#' domain at 0.25° resolution and is downloaded once and cached.
#'
#' @param years Integer vector. Years to include. Must be within 1990–2021.
#'   `NULL` (default) returns all available years.
#'
#' @param municipalities An `sf` POLYGON object (e.g., from
#'   [geobr::read_municipality()]). When provided, flash drought events are
#'   spatially assigned to municipalities and statistics are aggregated.
#'   If `NULL`, returns the raw event data frame for Brazil filtered to
#'   the requested years.
#'
#' @param aggregate_by Character. Temporal resolution of the output when
#'   `municipalities` is provided. One of `"year"` (default) or `"month"`.
#'   Annual aggregation counts flash drought events and their total duration
#'   per municipality per year.
#'
#' @param brazil_only Logical. Filter the global dataset to Brazil's bounding
#'   box (-75 to -28 lon, -35 to 6 lat) before processing. Default `TRUE`.
#'
#' @param use_cache Logical. Reuse cached files. Default `TRUE`.
#'
#' @param cache_dir Character. Root cache directory.
#'   Default `"~/.climasus4r_cache/smvi"`.
#'
#' @param lang Character. Message language: `"pt"` (default), `"en"`, `"es"`.
#'
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return
#' \itemize{
#'   \item If `municipalities` is provided: a `climasus_df` with columns:
#'     \itemize{
#'       \item `code_muni` — 7-digit IBGE municipality code
#'       \item `date` — Date (Jan 1 for annual; first day of month for monthly)
#'       \item `n_fd_events` — integer count of flash drought events
#'       \item `fd_total_days` — total days under flash drought
#'       \item `fd_mean_severity` — mean severity (SV) across events
#'       \item `fd_max_severity` — maximum severity (SV) in the period
#'     }
#'     Metadata: `stage = "climate"`, `type = "smvi"`.
#'   \item If `municipalities = NULL`: a `climasus_df` with raw event data
#'     columns: `cell_id`, `Lon`, `Lat`, `fstdate`, `lstdate`, `SV`,
#'     `duration_days`.
#' }
#'
#' @section Algorithm:
#' SMVI flash drought detection (Osman et al., 2024):
#' \enumerate{
#'   \item Compute 5-day and 20-day running averages of root-zone soil
#'     moisture (RZSM, 0–100 cm) from NASA GLDAS-2 data.
#'   \item A flash drought *onset* occurs when the 5-day average drops
#'     below the 20-day average.
#'   \item A flash drought *event* is confirmed when the RZSM deficit
#'     relative to the 20th percentile threshold is exceeded.
#'   \item Severity (SV) quantifies the cumulative soil moisture deficit
#'     during the event.
#' }
#'
#' @section Data source:
#' Osman, M. et al. (2024). A global flash drought inventory based on soil
#' moisture volatility. *Scientific Data*, 11, 916.
#' \doi{10.1038/s41597-024-03809-9}\cr
#' Data: \url{https://www.hydroshare.org/resource/642ff72592404a17bb85a8a92b4dbcd6/}
#' (CC BY-SA 4.0, no authentication required)
#'
#' @examples
#' \dontrun{
#' library(geobr)
#' mt_mun <- read_municipality(code_muni = "MT", year = 2020)
#'
#' # Annual flash drought statistics for Mato Grosso
#' smvi_mt <- sus_grid_smvi(
#'   years          = 2000:2021,
#'   municipalities = mt_mun,
#'   aggregate_by   = "year",
#'   lang           = "pt"
#' )
#' sus_meta(smvi_mt, "stage")  # "climate"
#' sus_meta(smvi_mt, "type")   # "smvi"
#'
#' # Monthly resolution
#' smvi_mo <- sus_grid_smvi(
#'   years          = 2015:2021,
#'   municipalities = mt_mun,
#'   aggregate_by   = "month"
#' )
#'
#' # Join to health data for DLNM
#' combined <- sus_grid_join(sih_mt, smvi_mt)
#' }
#'
#' @seealso [sus_climate_compute_spi()], [sus_climate_compute_spei()],
#'   [sus_grid_pdsi()], [sus_grid_join()], [sus_mod_dlnm()]
#'
#' @export
#' @importFrom dplyr filter select rename mutate summarise n left_join
#'   join_by n_distinct all_of
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom lubridate year month make_date
#' @importFrom tibble as_tibble
#' @importFrom rlang .data check_installed
sus_grid_smvi <- function(
    years          = NULL,
    municipalities = NULL,
    aggregate_by   = c("year", "month"),
    brazil_only    = TRUE,
    use_cache      = TRUE,
    cache_dir      = "~/.climasus4r_cache/smvi",
    lang           = "pt",
    verbose        = TRUE) {

  # ── 1. Validation ───────────────────────────────────────────────────────────

  if (!is.character(lang) || length(lang) != 1 || !lang %in% c("pt", "en", "es")) {
    cli::cli_abort("{.arg lang} must be 'pt', 'en', or 'es'.")
  }
  msg <- .smvi_msgs[[lang]]

  aggregate_by <- match.arg(aggregate_by)

  if (!is.null(years)) {
    if (!is.numeric(years) || any(is.na(years))) cli::cli_abort(msg$invalid_years_type)
    years <- sort(as.integer(unique(years)))
    bad_y <- years[years < 1990L | years > 2021L]
    if (length(bad_y) > 0) {
      bad <- paste(bad_y, collapse = ", ")
      cli::cli_abort(c(msg$invalid_years_range, "x" = msg$invalid_years_x))
    }
  }

  if (!is.null(municipalities)) {
    if (!requireNamespace("sf", quietly = TRUE)) cli::cli_abort(msg$need_sf)
    if (!inherits(municipalities, "sf")) cli::cli_abort(msg$muni_not_sf)
  }

  if (!is.logical(use_cache) || length(use_cache) != 1) cli::cli_abort(msg$invalid_use_cache)
  if (!is.character(cache_dir) || nchar(trimws(cache_dir)) == 0) cli::cli_abort(msg$invalid_cache_dir)
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)

  rlang::check_installed("httr2", reason = "to download SMVI data from HydroShare")

  # ── 2. Cache paths ───────────────────────────────────────────────────────────
  base_url  <- "http://www.hydroshare.org/resource/642ff72592404a17bb85a8a92b4dbcd6/data/contents"
  lonlat_nc <- file.path(cache_dir, "LONLAT.csv")
  events_gz <- file.path(cache_dir, "SMVI_GLDAS_Events.tar.gz")
  events_dir <- file.path(cache_dir, "events")

  if (verbose) {
    cli::cli_h1(msg$title)
    cli::cli_alert_info(msg$about)
  }

  # ── 3. Download LONLAT.csv ───────────────────────────────────────────────────
  .smvi_download_once(
    url        = paste0(base_url, "/LONLAT.csv"),
    cache_path = lonlat_nc,
    use_cache  = use_cache,
    verbose    = verbose,
    msg        = msg,
    label      = "LONLAT.csv"
  )

  if (!file.exists(lonlat_nc)) cli::cli_abort(msg$lonlat_missing)

  if (verbose) cli::cli_alert_info(msg$reading_lonlat)
  lonlat <- utils::read.csv(lonlat_nc, stringsAsFactors = FALSE)
  # Normalize column names (could be Lon/Lat or lon/lat)
  names(lonlat) <- tolower(names(lonlat))
  if (!"lon" %in% names(lonlat) || !"lat" %in% names(lonlat)) {
    cli::cli_abort(glue::glue(msg$lonlat_bad_cols,
                               cols = paste(names(lonlat), collapse = ", ")))
  }
  lonlat$cell_id <- seq_len(nrow(lonlat))

  # Filter to Brazil bounding box
  if (brazil_only) {
    brazil_cells <- lonlat$lon >= -75 & lonlat$lon <= -28 &
                    lonlat$lat >= -35 & lonlat$lat <=  6
    lonlat_br    <- lonlat[brazil_cells, ]
  } else {
    lonlat_br    <- lonlat
  }
  n_cells <- nrow(lonlat_br)
  if (verbose) cli::cli_alert_info(
    glue::glue(msg$cells_found, n = n_cells))

  # ── 4. Download and extract events archive ───────────────────────────────────
  .smvi_download_once(
    url        = paste0(base_url, "/FD_Events/SMVI_GLDAS_Events.tar.gz"),
    cache_path = events_gz,
    use_cache  = use_cache,
    verbose    = verbose,
    msg        = msg,
    label      = "SMVI_GLDAS_Events.tar.gz (~109 MB)"
  )

  if (!file.exists(events_gz)) cli::cli_abort(msg$events_missing)

  # Extract tar.gz if not already done
  if (!dir.exists(events_dir) ||
      length(list.files(events_dir, pattern = "\\.csv$")) == 0) {
    if (verbose) cli::cli_alert_info(msg$extracting)
    dir.create(events_dir, recursive = TRUE, showWarnings = FALSE)
    utils::untar(events_gz, exdir = events_dir)
  }

  csv_files <- list.files(events_dir, pattern = "SMVI_GLDAS_\\d{4}\\.csv$",
                            full.names = TRUE)
  if (length(csv_files) == 0) {
    cli::cli_abort(msg$no_csv_after_extract)
  }

  # Filter to requested years
  avail_years <- as.integer(stringr::str_extract(basename(csv_files), "\\d{4}"))
  req_years   <- if (is.null(years)) avail_years else years
  use_files   <- csv_files[avail_years %in% req_years]

  if (length(use_files) == 0) cli::cli_abort(msg$no_data)

  # ── 5. Read and filter events to Brazil cells ─────────────────────────────────
  if (verbose) cli::cli_alert_info(
    glue::glue(msg$loading_events, n_years = length(use_files)))

  events_list <- lapply(use_files, function(f) {
    tryCatch({
      ev <- utils::read.csv(f, stringsAsFactors = FALSE)
      names(ev) <- tolower(names(ev))
      # Column names may vary; standardise
      if (!"cell_id" %in% names(ev)) {
        # Assume first column is cell index (1-based)
        names(ev)[1] <- "cell_id"
      }
      ev <- ev[ev$cell_id %in% lonlat_br$cell_id, ]
      ev
    }, error = function(e) {
      cli::cli_warn(glue::glue(msg$read_warn, filename = basename(f)))
      NULL
    })
  })
  events_list <- events_list[!vapply(events_list, is.null, logical(1))]
  if (length(events_list) == 0) cli::cli_abort(msg$no_brazil_events)

  events_df <- do.call(rbind, events_list)

  # Standardise date columns
  date_cols <- intersect(c("fstdate", "lstdate", "onset", "recovery",
                           "start_date", "end_date"), names(events_df))
  if (length(date_cols) < 2) {
    cli::cli_abort(glue::glue(msg$bad_event_cols,
                               cols = paste(names(events_df), collapse = ", ")))
  }
  fst_col <- date_cols[1]
  lst_col <- date_cols[2]
  events_df$fstdate <- as.Date(events_df[[fst_col]])
  events_df$lstdate <- as.Date(events_df[[lst_col]])

  # Duration in days
  events_df$duration_days <- as.integer(events_df$lstdate - events_df$fstdate) + 1L

  # Severity column (SV or sv)
  sv_col <- intersect(c("SV", "sv", "severity"), names(events_df))
  events_df$SV <- if (length(sv_col) > 0) events_df[[sv_col[1]]] else NA_real_

  # Add lat/lon from LONLAT
  events_df <- merge(events_df, lonlat_br[, c("cell_id", "lon", "lat")],
                     by = "cell_id", all.x = TRUE)

  events_df <- tibble::as_tibble(
    events_df[, c("cell_id", "lon", "lat", "fstdate", "lstdate",
                  "duration_days", "SV")]
  )

  n_events <- nrow(events_df)
  if (verbose) cli::cli_alert_info(
    glue::glue(msg$events_loaded, n = n_events))

  # ── 6. Return raw events if no municipalities ─────────────────────────────────
  if (is.null(municipalities)) {
    out_df <- dplyr::rename(events_df,
                             Lon = .data$lon, Lat = .data$lat,
                             SV  = .data$SV)
    out_df$source <- "SMVI_GLDAS"
    meta <- list(
      system = NULL, stage = "climate", type = "smvi", spatial = FALSE,
      temporal = list(
        start = min(events_df$fstdate, na.rm = TRUE),
        end   = max(events_df$lstdate, na.rm = TRUE),
        unit  = "event", source = "HydroShare_SMVI"
      ),
      created = Sys.time(), modified = Sys.time(),
      years   = req_years,
      n_observations = n_events,
      history = sprintf("[%s] sus_grid_smvi(): %d events, no aggregation",
                        format(Sys.time(), "%Y-%m-%d %H:%M:%S"), n_events),
      user = list()
    )
    base_classes <- setdiff(class(out_df), "climasus_df")
    if (verbose) cli::cli_alert_success(
      glue::glue(msg$done_raw, n = n_events))
    return(structure(out_df, sus_meta = meta,
                     class = c("climasus_df", base_classes)))
  }

  # ── 7. Assign flash drought events to municipalities ─────────────────────────
  muni_id_col <- .smvi_detect_muni_col(municipalities)
  if (muni_id_col != "code_muni") {
    municipalities$code_muni <- as.character(municipalities[[muni_id_col]])
  } else {
    municipalities$code_muni <- as.character(municipalities$code_muni)
  }
  municipalities$code_muni <- substr(municipalities$code_muni, 1L, 7L)
  municipalities <- sf::st_transform(municipalities, crs = 4326)
  n_mun <- nrow(municipalities)

  if (verbose) cli::cli_alert_info(
    glue::glue(msg$spatial_join, n_mun = n_mun))

  # Convert grid cell centres to sf POINTS
  cell_sf <- sf::st_as_sf(
    lonlat_br,
    coords  = c("lon", "lat"),
    crs     = 4326,
    remove  = FALSE
  )
  cell_sf <- cell_sf[, c("cell_id", "lon", "lat")]

  # Assign each cell to a municipality
  muni_slim <- municipalities[, "code_muni"]
  cell_joined <- sf::st_join(cell_sf, muni_slim, join = sf::st_within,
                              left = FALSE)
  cell_muni   <- sf::st_drop_geometry(cell_joined)[, c("cell_id", "code_muni")]

  # Attach municipality to events
  events_muni <- merge(events_df, cell_muni, by = "cell_id", all.x = FALSE)
  if (nrow(events_muni) == 0) {
    cli::cli_alert_warning(msg$no_events_in_muni)
  }

  events_muni <- tibble::as_tibble(events_muni)

  # ── 8. Aggregate per municipality per period ─────────────────────────────────
  events_muni <- events_muni |>
    dplyr::mutate(
      year_val = lubridate::year(.data$fstdate),
      month_val = lubridate::month(.data$fstdate)
    )

  if (aggregate_by == "year") {
    result <- events_muni |>
      dplyr::summarise(
        n_fd_events       = dplyr::n(),
        fd_total_days     = sum(.data$duration_days, na.rm = TRUE),
        fd_mean_severity  = mean(.data$SV, na.rm = TRUE),
        fd_max_severity   = max(.data$SV, na.rm = TRUE),
        .by = c(code_muni, year_val)
      ) |>
      dplyr::mutate(date = lubridate::make_date(.data$year_val, 1L, 1L)) |>
      dplyr::select(-"year_val")
  } else {
    result <- events_muni |>
      dplyr::summarise(
        n_fd_events       = dplyr::n(),
        fd_total_days     = sum(.data$duration_days, na.rm = TRUE),
        fd_mean_severity  = mean(.data$SV, na.rm = TRUE),
        fd_max_severity   = max(.data$SV, na.rm = TRUE),
        .by = c(code_muni, year_val, month_val)
      ) |>
      dplyr::mutate(date = lubridate::make_date(.data$year_val,
                                                 .data$month_val, 1L)) |>
      dplyr::select(-"year_val", -"month_val")
  }

  result$n_fd_events   <- as.integer(result$n_fd_events)
  result$fd_total_days <- as.integer(result$fd_total_days)
  result <- result[order(result$code_muni, result$date), ]
  rownames(result) <- NULL
  result <- tibble::as_tibble(result)

  n_rows <- nrow(result)
  if (verbose) cli::cli_alert_success(
    glue::glue(msg$agg_done, n_rows = n_rows, n_mun = n_mun))

  # ── 9. Build climasus_df ──────────────────────────────────────────────────────
  meta <- list(
    system   = NULL,
    stage    = "climate",
    type     = "smvi",
    spatial  = FALSE,
    temporal = list(
      start       = min(result$date, na.rm = TRUE),
      end         = max(result$date, na.rm = TRUE),
      unit        = aggregate_by,
      source      = "HydroShare_SMVI_Osman2024"
    ),
    created          = Sys.time(),
    modified         = Sys.time(),
    years            = req_years,
    aggregate_by     = aggregate_by,
    n_municipalities = n_mun,
    n_observations   = n_rows,
    history          = sprintf(
      "[%s] sus_grid_smvi(): %d events \u2192 %d obs (%s), %d municipalities",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      nrow(events_muni), n_rows, aggregate_by, n_mun
    ),
    user = list()
  )
  base_classes <- setdiff(class(result), "climasus_df")
  structure(result, sus_meta = meta, class = c("climasus_df", base_classes))
}


# ── Internal helpers ──────────────────────────────────────────────────────────

#' Download a file with cache check (SMVI specific)
#' @keywords internal
#' @noRd
.smvi_download_once <- function(url, cache_path, use_cache,
                                 verbose, msg, label) {
  if (use_cache && file.exists(cache_path) && file.size(cache_path) > 0) {
    if (verbose) cli::cli_alert_success(
      glue::glue(msg$cache_hit, filename = label))
    return(invisible(cache_path))
  }
  dir_path <- dirname(cache_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  if (verbose) cli::cli_alert_info(
    glue::glue(msg$download_file, filename = label))
  tryCatch({
    utils::download.file(url = url, destfile = cache_path,
                         mode = "wb", quiet = !verbose, method = "auto")
    if (verbose) cli::cli_alert_success(
      glue::glue(msg$download_done, filename = label))
    invisible(cache_path)
  }, error = function(e) {
    if (file.exists(cache_path)) unlink(cache_path)
    cli::cli_warn(glue::glue(msg$download_error,
                             filename = label, err = conditionMessage(e)))
    invisible(NA_character_)
  })
}

#' Auto-detect municipality identifier column
#' @keywords internal
#' @noRd
.smvi_detect_muni_col <- function(municipalities) {
  candidates <- c("code_muni", "CD_MUN", "CD_GEOCMU", "code_municipality")
  found <- intersect(candidates, names(municipalities))
  if (length(found) > 0) return(found[1])
  for (col in names(municipalities)) {
    vals <- as.character(municipalities[[col]])[!is.na(municipalities[[col]])]
    if (length(vals) > 0 &&
        all(grepl("^\\d{6,7}$", vals[seq_len(min(5L, length(vals)))])))
      return(col)
  }
  cli::cli_abort(c(
    "Could not detect a municipality identifier column.",
    "i" = "Expected one of: {.val {paste(candidates, collapse = ', ')}}."
  ))
}

#' Multilingual messages for sus_grid_smvi()
#' @keywords internal
#' @noRd
.smvi_msgs <- list(
  pt = list(
    title              = "SMVI \u2014 \u00cdndice de Volatilidade de Umidade do Solo (Flash Droughts)",
    about              = "Dados globais 1990-2021 de secas r\u00e1pidas (GLDAS, 0.25\u00b0). Download \u00fanico de ~130 MB.",
    invalid_years_type = "{.arg years} deve ser num\u00e9rico sem NA.",
    invalid_years_range = "{.arg years} deve estar entre 1990 e 2021.",
    invalid_years_x    = "Ano(s) inv\u00e1lido(s): {bad}.",
    need_sf            = "O pacote {.pkg sf} \u00e9 necess\u00e1rio para agregar por munic\u00edpios.",
    muni_not_sf        = "{.arg municipalities} deve ser um objeto {.cls sf}.",
    invalid_use_cache  = "{.arg use_cache} deve ser TRUE ou FALSE.",
    invalid_cache_dir  = "{.arg cache_dir} deve ser uma string n\u00e3o vazia.",
    cache_hit          = "Cache encontrado: {filename}",
    download_file      = "Baixando: {filename}",
    download_done      = "Conclu\u00eddo: {filename}",
    download_error     = "Falha ao baixar {filename}: {err}",
    lonlat_missing     = "N\u00e3o foi poss\u00edvel baixar LONLAT.csv do HydroShare.",
    reading_lonlat     = "Lendo coordenadas de grade (LONLAT.csv)...",
    lonlat_bad_cols    = "LONLAT.csv n\u00e3o tem colunas 'Lon'/'Lat' esperadas. Colunas: {cols}",
    cells_found        = "{n} c\u00e9lula(s) de grade dentro do Brasil.",
    extracting         = "Extraindo arquivo de eventos SMVI...",
    no_csv_after_extract = "Nenhum arquivo CSV encontrado ap\u00f3s extra\u00e7\u00e3o do tar.gz.",
    events_missing     = "Arquivo de eventos n\u00e3o dispon\u00edvel no cache.",
    no_data            = "Nenhum arquivo CSV de eventos para os anos solicitados.",
    loading_events     = "Carregando eventos de {n_years} ano(s)...",
    read_warn          = "N\u00e3o foi poss\u00edvel ler {filename}.",
    no_brazil_events   = "Nenhum evento flash drought encontrado para as c\u00e9lulas brasileiras.",
    bad_event_cols     = "Colunas de data n\u00e3o encontradas no CSV de eventos. Colunas: {cols}",
    events_loaded      = "{n} evento(s) de flash drought carregado(s).",
    spatial_join       = "Atribuindo eventos a {n_mun} munic\u00edpio(s)...",
    no_events_in_muni  = "Nenhum evento encontrado dentro dos pol\u00edgonos municipais.",
    agg_done           = "Conclu\u00eddo: {n_rows} observa\u00e7\u00f5es ({n_mun} munic\u00edpios).",
    done_raw           = "Conclu\u00eddo: {n} eventos brutos retornados."
  ),
  en = list(
    title              = "SMVI \u2014 Soil Moisture Volatility Index (Flash Droughts)",
    about              = "Global 1990-2021 flash drought data (GLDAS, 0.25 deg). Single download ~130 MB.",
    invalid_years_type = "{.arg years} must be numeric without NA.",
    invalid_years_range = "{.arg years} must be between 1990 and 2021.",
    invalid_years_x    = "Invalid year(s): {bad}.",
    need_sf            = "Package {.pkg sf} is required to aggregate by municipality.",
    muni_not_sf        = "{.arg municipalities} must be an {.cls sf} object.",
    invalid_use_cache  = "{.arg use_cache} must be TRUE or FALSE.",
    invalid_cache_dir  = "{.arg cache_dir} must be a non-empty string.",
    cache_hit          = "Cache found: {filename}",
    download_file      = "Downloading: {filename}",
    download_done      = "Done: {filename}",
    download_error     = "Failed to download {filename}: {err}",
    lonlat_missing     = "Could not download LONLAT.csv from HydroShare.",
    reading_lonlat     = "Reading grid coordinates (LONLAT.csv)...",
    lonlat_bad_cols    = "LONLAT.csv does not have expected 'Lon'/'Lat' columns. Found: {cols}",
    cells_found        = "{n} grid cell(s) within Brazil's bounding box.",
    extracting         = "Extracting SMVI events archive...",
    no_csv_after_extract = "No CSV files found after tar.gz extraction.",
    events_missing     = "Events file not available in cache.",
    no_data            = "No events CSV files for the requested years.",
    loading_events     = "Loading events from {n_years} year(s)...",
    read_warn          = "Could not read {filename}.",
    no_brazil_events   = "No flash drought events found for Brazilian grid cells.",
    bad_event_cols     = "Date columns not found in events CSV. Columns: {cols}",
    events_loaded      = "{n} flash drought event(s) loaded.",
    spatial_join       = "Assigning events to {n_mun} municipality/ies...",
    no_events_in_muni  = "No events found within municipality polygons.",
    agg_done           = "Complete: {n_rows} observations ({n_mun} municipalities).",
    done_raw           = "Complete: {n} raw flash drought events returned."
  ),
  es = list(
    title              = "SMVI \u2014 \u00cdndice de Volatilidad de Humedad del Suelo (Sequ\u00edas R\u00e1pidas)",
    about              = "Datos globales 1990-2021 de sequ\u00edas r\u00e1pidas (GLDAS, 0.25\u00b0). Descarga \u00fanica ~130 MB.",
    invalid_years_type = "{.arg years} debe ser num\u00e9rico sin NA.",
    invalid_years_range = "{.arg years} debe estar entre 1990 y 2021.",
    invalid_years_x    = "A\u00f1o(s) inv\u00e1lido(s): {bad}.",
    need_sf            = "El paquete {.pkg sf} es necesario para agregar por municipios.",
    muni_not_sf        = "{.arg municipalities} debe ser un objeto {.cls sf}.",
    invalid_use_cache  = "{.arg use_cache} debe ser TRUE o FALSE.",
    invalid_cache_dir  = "{.arg cache_dir} debe ser una cadena no vac\u00eda.",
    cache_hit          = "Cach\u00e9 encontrado: {filename}",
    download_file      = "Descargando: {filename}",
    download_done      = "Completado: {filename}",
    download_error     = "Error al descargar {filename}: {err}",
    lonlat_missing     = "No se pudo descargar LONLAT.csv de HydroShare.",
    reading_lonlat     = "Leyendo coordenadas de cuadr\u00edcula (LONLAT.csv)...",
    lonlat_bad_cols    = "LONLAT.csv no tiene columnas 'Lon'/'Lat'. Encontradas: {cols}",
    cells_found        = "{n} celda(s) de cuadr\u00edcula dentro de Brasil.",
    extracting         = "Extrayendo archivo de eventos SMVI...",
    no_csv_after_extract = "No se encontraron archivos CSV tras extracci\u00f3n.",
    events_missing     = "Archivo de eventos no disponible en cach\u00e9.",
    no_data            = "No hay archivos CSV de eventos para los a\u00f1os solicitados.",
    loading_events     = "Cargando eventos de {n_years} a\u00f1o(s)...",
    read_warn          = "No se pudo leer {filename}.",
    no_brazil_events   = "No se encontraron eventos de sequ\u00eda r\u00e1pida para celdas brasile\u00f1as.",
    bad_event_cols     = "Columnas de fecha no encontradas. Columnas: {cols}",
    events_loaded      = "{n} evento(s) de sequ\u00eda r\u00e1pida cargado(s).",
    spatial_join       = "Asignando eventos a {n_mun} municipio(s)...",
    no_events_in_muni  = "No se encontraron eventos dentro de los pol\u00edgonos municipales.",
    agg_done           = "Completo: {n_rows} observaciones ({n_mun} municipios).",
    done_raw           = "Completo: {n} eventos brutos retornados."
  )
)
