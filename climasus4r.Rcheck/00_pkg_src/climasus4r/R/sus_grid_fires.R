# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "code_muni", "date", "n_fires", "frp_mean", "frp",
  "lat", "lon", "latitude", "longitude", "datahora",
  "acq_date", "acq_time", "geometry", "n", ".data",
  "bad", "valid", "err", "n_rows", "n_points"
))

# ── Exported function ─────────────────────────────────────────────────────────

#' Import Fire Hotspot Data for Brazilian Municipalities
#'
#' @description
#' `sus_grid_fires()` downloads fire hotspot (active fire) data from INPE
#' Queimadas or NASA FIRMS, optionally aggregates counts and Fire Radiative
#' Power (FRP) to Brazilian municipalities, and returns a `climasus_df`
#' compatible with [sus_mod_dlnm()] and [sus_climate_anomaly()].
#'
#' Fire data is critical for respiratory health analyses in Brazil: smoke from
#' agricultural burning (cerrado, cana-de-acucar) and deforestation fires
#' (Amazonia) are associated with acute respiratory outcomes in SIH/SINAN data.
#'
#' **Sources available:**
#' \itemize{
#'   \item `"inpe"` (default) — INPE Queimadas portal. No authentication.
#'     Historical data from 1998 to present. Brazil only.
#'     DOI: \doi{10.2312/inpe.2022.009}
#'   \item `"firms_modis"` — NASA FIRMS MODIS Collection 6.1.
#'     Requires a free MAP KEY from
#'     \url{https://firms.modaps.eosdis.nasa.gov/api/map_key/}.
#'     Global, 1 km resolution, 2000 to present.
#'   \item `"firms_viirs"` — NASA FIRMS VIIRS SNPP.
#'     Same key as FIRMS MODIS. 375 m resolution, 2012 to present.
#' }
#'
#' @param years Numeric vector of years. INPE: 1998–present.
#'   FIRMS MODIS: 2000–present. FIRMS VIIRS: 2012–present.
#'
#' @param months Integer vector (1–12). Default `1:12`.
#'
#' @param uf Character vector of state codes (e.g., `c("MT", "PA")`).
#'   Case-insensitive. `NULL` = all Brazil. Used to filter INPE queries and
#'   as a shortcut to define `bbox` for FIRMS.
#'
#' @param bbox Numeric vector `c(xmin, ymin, xmax, ymax)` in WGS84 longitude/
#'   latitude. Used by FIRMS for spatial subsetting. If `NULL` and `uf` is
#'   provided, the bbox is computed from Brazilian state centroids. If both
#'   are `NULL`, Brazil's bounding box is used.
#'
#' @param source Character. Data source: `"inpe"` (default), `"firms_modis"`,
#'   or `"firms_viirs"`.
#'
#' @param municipalities An `sf` POLYGON object (e.g., from
#'   [geobr::read_municipality()]). When provided, fire points are spatially
#'   joined to polygons and aggregated to daily municipality-level counts.
#'   If `NULL`, the raw fire point data are returned as a `climasus_df`.
#'
#' @param agg_fun Character. Aggregation strategy when `municipalities` is
#'   provided. Currently `"count"` is supported (count of fire hotspots per
#'   municipality per day). Future versions may support `"frp_sum"`.
#'
#' @param biome Character vector. INPE biome filter. Allowed values:
#'   `"Amazonia"`, `"Cerrado"`, `"Mata Atlantica"`, `"Caatinga"`,
#'   `"Pampa"`, `"Pantanal"`. `NULL` = no filter.
#'
#' @param use_cache Logical. Reuse previously downloaded CSV/JSON files.
#'   Default `TRUE`.
#'
#' @param cache_dir Character. Root cache directory.
#'   Default `"~/.climasus4r_cache/fires"`.
#'
#' @param firms_key Character. NASA FIRMS MAP KEY.
#'   Defaults to `Sys.getenv("FIRMS_MAP_KEY")`. Required when
#'   `source %in% c("firms_modis", "firms_viirs")`.
#'
#' @param lang Character. Message language: `"pt"` (default), `"en"`, `"es"`.
#'
#' @param verbose Logical. Print progress. Default `TRUE`.
#'
#' @return
#' \itemize{
#'   \item If `municipalities` is provided: a `climasus_df` with columns
#'     `date` (Date), `code_muni` (character), `n_fires` (integer count of
#'     hotspots), and `frp_mean` (mean Fire Radiative Power in MW,
#'     `NA` when not available). Metadata: `stage = "climate"`,
#'     `type = "fires"`.
#'   \item If `municipalities = NULL`: a `climasus_df` with raw fire point
#'     columns: `date`, `lat`, `lon`, `frp`, `biome`, `estado`, `source`.
#' }
#'
#' @section INPE Queimadas API:
#' Queries `https://queimadas.dgi.inpe.br/api/focos/` monthly. Each monthly
#' request is cached as a JSON file. Large states in fire-season months may
#' return truncated results due to the API's record limit; use `uf` to
#' narrow the query when needed.
#'
#' @section NASA FIRMS API:
#' Queries `https://firms.modaps.eosdis.nasa.gov/api/area/csv/{key}/{product}/{bbox}/10/{date}`
#' in 10-day chunks (maximum allowed per request). A free MAP KEY is required
#' and must be activated at \url{https://firms.modaps.eosdis.nasa.gov/api/map_key/}.
#'
#' @examples
#' \dontrun{
#' library(geobr)
#' mt_mun <- read_municipality(code_muni = "MT", year = 2020)
#'
#' # INPE fire count for Mato Grosso municipalities, fire season 2020
#' fires <- sus_grid_fires(
#'   years          = 2020,
#'   months         = 7:10,
#'   uf             = "MT",
#'   municipalities = mt_mun,
#'   lang           = "pt"
#' )
#' sus_meta(fires, "stage")  # "climate"
#' sus_meta(fires, "type")   # "fires"
#'
#' # Raw fire points for Amazonia, no aggregation
#' fires_pts <- sus_grid_fires(
#'   years  = 2022,
#'   months = 8,
#'   biome  = "Amazonia",
#'   lang   = "en"
#' )
#'
#' # FIRMS MODIS with API key
#' Sys.setenv(FIRMS_MAP_KEY = "my_key")
#' fires_firms <- sus_grid_fires(
#'   years          = 2023,
#'   months         = 9,
#'   uf             = "MT",
#'   source         = "firms_modis",
#'   municipalities = mt_mun
#' )
#' }
#'
#' @seealso [sus_grid_pollution_cams()], [sus_climate_anomaly()],
#'   [sus_mod_dlnm()]
#'
#' @export
#' @importFrom rlang .data is_installed
#' @importFrom dplyr filter select rename mutate summarise n left_join
#'   join_by n_distinct all_of
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom lubridate year month days_in_month make_date
#' @importFrom tibble as_tibble
sus_grid_fires <- function(
    years,
    months         = 1:12,
    uf             = NULL,
    bbox           = NULL,
    source         = c("inpe", "firms_modis", "firms_viirs"),
    municipalities = NULL,
    agg_fun        = "count",
    biome          = NULL,
    use_cache      = TRUE,
    cache_dir      = "~/.climasus4r_cache/fires",
    firms_key      = NULL,
    lang           = "pt",
    verbose        = TRUE) {

  # ── 1. Validation ───────────────────────────────────────────────────────────

  if (!is.character(lang) || length(lang) != 1 || !lang %in% c("pt", "en", "es")) {
    cli::cli_abort("{.arg lang} must be 'pt', 'en', or 'es'.")
  }
  msg <- .fires_msgs[[lang]]

  source <- match.arg(source)

  # Years
  if (missing(years) || is.null(years)) cli::cli_abort(msg$missing_years)
  if (!is.numeric(years) || any(is.na(years))) cli::cli_abort(msg$invalid_years_type)
  years <- sort(as.integer(unique(years)))

  min_year <- switch(source, inpe = 1998L, firms_modis = 2000L, firms_viirs = 2012L)
  bad_y    <- years[years < min_year | years > as.integer(format(Sys.Date(), "%Y"))]
  if (length(bad_y) > 0) {
    bad <- paste(bad_y, collapse = ", ")
    cli::cli_abort(c(msg$invalid_years_range, "x" = msg$invalid_years_x))
  }

  # Months
  if (!is.numeric(months) || any(is.na(months)) ||
      any(months < 1 | months > 12)) cli::cli_abort(msg$invalid_months)
  months <- sort(as.integer(unique(months)))

  # UF
  valid_ufs <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA",
                 "MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN",
                 "RS","RO","RR","SC","SP","SE","TO")
  if (!is.null(uf)) {
    uf <- toupper(trimws(uf))
    bad_uf <- setdiff(uf, valid_ufs)
    if (length(bad_uf) > 0) {
      bad   <- paste(bad_uf, collapse = ", ")
      valid <- paste(valid_ufs, collapse = ", ")
      cli::cli_abort(msg$invalid_uf)
    }
  }

  # Biome
  valid_biomes <- c("Amazonia", "Cerrado", "Mata Atlantica",
                    "Caatinga", "Pampa", "Pantanal")
  if (!is.null(biome)) {
    bad_biome <- setdiff(biome, valid_biomes)
    if (length(bad_biome) > 0) {
      bad   <- paste(bad_biome, collapse = ", ")
      valid <- paste(valid_biomes, collapse = ", ")
      cli::cli_abort(msg$invalid_biome)
    }
  }

  # FIRMS key
  if (startsWith(source, "firms")) {
    if (is.null(firms_key) || nchar(firms_key) == 0) {
      firms_key <- Sys.getenv("FIRMS_MAP_KEY")
    }
    if (nchar(firms_key) == 0) cli::cli_abort(msg$no_firms_key)
  }

  # Municipalities
  if (!is.null(municipalities)) {
    if (!requireNamespace("sf", quietly = TRUE)) cli::cli_abort(msg$need_sf)
    if (!inherits(municipalities, "sf")) cli::cli_abort(msg$muni_not_sf)
  }

  if (!is.logical(use_cache) || length(use_cache) != 1) cli::cli_abort(msg$invalid_use_cache)
  if (!is.character(cache_dir) || nchar(trimws(cache_dir)) == 0) cli::cli_abort(msg$invalid_cache_dir)
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)

  # FIRMS bbox: use provided, or derive from UF, or use Brazil default
  if (startsWith(source, "firms") && is.null(bbox)) {
    bbox <- if (!is.null(uf)) .fires_uf_bbox(uf) else c(-75, -35, -28, 6)
  }

  # ── 2. Build manifest (year × month) ─────────────────────────────────────────
  manifest_rows <- list()
  for (yr in years) {
    for (mo in months) {
      last_day <- lubridate::days_in_month(lubridate::make_date(yr, mo, 1L))
      row <- list(
        year       = yr,
        month      = mo,
        date_start = sprintf("%04d-%02d-01", yr, mo),
        date_end   = sprintf("%04d-%02d-%02d", yr, mo, last_day),
        cache_path = file.path(cache_dir, source,
                               sprintf("%04d_%02d.csv", yr, mo))
      )
      manifest_rows[[length(manifest_rows) + 1]] <- row
    }
  }
  manifest <- do.call(rbind, lapply(manifest_rows, as.data.frame,
                                     stringsAsFactors = FALSE))
  manifest$year  <- as.integer(manifest$year)
  manifest$month <- as.integer(manifest$month)

  n_months <- nrow(manifest)
  if (verbose) {
    cli::cli_h1(msg$title)
    cli::cli_alert_info(glue::glue(msg$download_start, n_months = n_months))
  }

  # ── 3. Download with cache ────────────────────────────────────────────────────
  rlang::check_installed("httr2",
    reason = "to download fire data from INPE/FIRMS APIs")

  for (i in seq_len(n_months)) {
    .fires_download_month(
      source     = source,
      date_start = manifest$date_start[i],
      date_end   = manifest$date_end[i],
      cache_path = manifest$cache_path[i],
      uf         = uf,
      biome      = biome,
      bbox       = bbox,
      firms_key  = firms_key,
      use_cache  = use_cache,
      verbose    = verbose,
      msg        = msg
    )
  }

  # ── 4. Read and combine all CSV files ────────────────────────────────────────
  all_rows <- lapply(seq_len(n_months), function(i) {
    fp <- manifest$cache_path[i]
    if (!file.exists(fp) || file.size(fp) == 0) return(NULL)
    tryCatch(
      utils::read.csv(fp, stringsAsFactors = FALSE, encoding = "UTF-8"),
      error = function(e) {
        cli::cli_warn(glue::glue(msg$read_warn, filename = basename(fp)))
        NULL
      }
    )
  })
  all_rows <- all_rows[!vapply(all_rows, is.null, logical(1))]
  if (length(all_rows) == 0) cli::cli_abort(msg$no_data)

  raw_df <- do.call(rbind, all_rows)
  raw_df  <- tibble::as_tibble(raw_df)

  # Normalize column names across INPE vs FIRMS
  raw_df <- .fires_normalize_cols(raw_df, source)

  n_points <- nrow(raw_df)
  if (verbose) cli::cli_alert_info(
    glue::glue(msg$points_loaded, n_points = n_points))

  # ── 5. Return raw points if no municipalities ─────────────────────────────────
  if (is.null(municipalities)) {
    raw_df$source <- source
    result <- raw_df[, intersect(names(raw_df),
                                  c("date", "lat", "lon", "frp",
                                    "biome", "estado", "source"))]
    result <- result[order(result$date), ]
    rownames(result) <- NULL
    result <- tibble::as_tibble(result)

    if (verbose) cli::cli_alert_success(
      glue::glue(msg$done_points, n_rows = nrow(result)))

    meta <- .fires_build_meta(result, years, months, source, uf, biome,
                               n_points, nrow(result))
    base_classes <- setdiff(class(result), "climasus_df")
    return(structure(result, sus_meta = meta,
                     class = c("climasus_df", base_classes)))
  }

  # ── 6. Spatial join to municipalities ─────────────────────────────────────────
  if (verbose) cli::cli_alert_info(
    glue::glue(msg$spatial_join, n_mun = nrow(municipalities)))

  # Drop rows missing coordinates
  raw_df <- raw_df[!is.na(raw_df$lat) & !is.na(raw_df$lon), ]

  fire_sf <- tryCatch(
    sf::st_as_sf(raw_df, coords = c("lon", "lat"), crs = 4326, remove = FALSE),
    error = function(e) cli::cli_abort(
      c(msg$sf_error, "i" = conditionMessage(e)))
  )

  # Prepare municipalities
  muni_id_col <- .fires_detect_muni_col(municipalities)
  if (muni_id_col != "code_muni") {
    municipalities$code_muni <- as.character(municipalities[[muni_id_col]])
  } else {
    municipalities$code_muni <- as.character(municipalities$code_muni)
  }
  municipalities$code_muni <- substr(municipalities$code_muni, 1L, 7L)
  municipalities <- sf::st_transform(municipalities, crs = 4326)
  # Keep only needed columns for efficiency
  municipalities_slim <- municipalities[, "code_muni"]

  # Spatial join: assign each fire point to a municipality
  fire_joined <- tryCatch(
    sf::st_join(fire_sf, municipalities_slim, join = sf::st_within,
                left = FALSE),
    error = function(e) cli::cli_abort(
      c(msg$st_join_error, "i" = conditionMessage(e)))
  )

  if (nrow(fire_joined) == 0) {
    cli::cli_alert_warning(msg$no_fires_in_muni)
    # Return empty climasus_df with correct structure
    result <- tibble::tibble(
      code_muni = character(0),
      date      = as.Date(character(0)),
      n_fires   = integer(0),
      frp_mean  = numeric(0)
    )
    meta <- .fires_build_meta(result, years, months, source, uf, biome,
                               n_points, 0L)
    base_classes <- setdiff(class(result), "climasus_df")
    return(structure(result, sus_meta = meta,
                     class = c("climasus_df", base_classes)))
  }

  # Drop geometry for aggregation
  fire_plain <- sf::st_drop_geometry(fire_joined)

  # Aggregate: n_fires + frp_mean per municipality per day
  result <- fire_plain |>
    dplyr::summarise(
      n_fires  = dplyr::n(),
      frp_mean = if ("frp" %in% names(fire_plain) &&
                     !all(is.na(.data$frp)))
                   mean(.data$frp, na.rm = TRUE) else NA_real_,
      .by = c(code_muni, date)
    )
  result$n_fires <- as.integer(result$n_fires)
  result <- result[order(result$code_muni, result$date), ]
  rownames(result) <- NULL
  result <- tibble::as_tibble(result)

  n_rows <- nrow(result)
  if (verbose) cli::cli_alert_success(
    glue::glue(msg$agg_done, n_rows = n_rows,
               n_mun = dplyr::n_distinct(result$code_muni)))

  meta <- .fires_build_meta(result, years, months, source, uf, biome,
                             n_points, n_rows)
  base_classes <- setdiff(class(result), "climasus_df")
  structure(result, sus_meta = meta, class = c("climasus_df", base_classes))
}


# ── Internal constants ────────────────────────────────────────────────────────

#' Approximate bounding boxes for Brazilian states (WGS84: xmin,ymin,xmax,ymax)
#' @keywords internal
#' @noRd
.fires_uf_bbox <- function(uf) {
  bboxes <- list(
    AC = c(-74, -11,  -66,  -7), AL = c(-38, -10,  -35,  -8),
    AP = c(-52,   1,  -49,   4), AM = c(-74,  -9,  -57,   2),
    BA = c(-46, -18,  -37,  -8), CE = c(-41,  -8,  -37,  -3),
    DF = c(-48,  -16, -47, -15), ES = c(-42, -21,  -39, -18),
    GO = c(-53, -19,  -45, -13), MA = c(-48,  -6,  -41,  -1),
    MT = c(-61, -18,  -50,  -7), MS = c(-58, -24,  -51, -17),
    MG = c(-51, -23,  -39, -14), PA = c(-59,  -9,  -46,   2),
    PB = c(-39,  -8,  -34,  -6), PR = c(-54, -27,  -48, -22),
    PE = c(-41,  -9,  -34,  -7), PI = c(-46,  -9,  -40,  -2),
    RJ = c(-45, -23,  -40, -21), RN = c(-38,  -7,  -34,  -4),
    RS = c(-54, -34,  -49, -27), RO = c(-66, -14,  -59,  -7),
    RR = c(-61,   1,  -58,   5), SC = c(-54, -30,  -48, -25),
    SP = c(-53, -25,  -44, -19), SE = c(-38, -11,  -36,  -9),
    TO = c(-50, -13,  -45,  -5)
  )
  selected <- bboxes[uf]
  # Merge bbox of all requested states
  all_coords <- do.call(rbind, selected)
  c(min(all_coords[, 1]), min(all_coords[, 2]),
    max(all_coords[, 3]), max(all_coords[, 4]))
}

#' FIRMS product name per source
#' @keywords internal
#' @noRd
.fires_firms_product <- function(source) {
  switch(source,
    firms_modis = "MODIS_C6_1",
    firms_viirs = "VIIRS_SNPP_SP",
    "MODIS_C6_1"
  )
}

#' Build sus_meta list for fires output
#' @keywords internal
#' @noRd
.fires_build_meta <- function(result, years, months, source, uf, biome,
                               n_raw_points, n_obs) {
  list(
    system   = NULL,
    stage    = "climate",
    type     = "fires",
    spatial  = FALSE,
    temporal = list(
      start  = if (nrow(result) > 0) min(result$date, na.rm = TRUE) else NA,
      end    = if (nrow(result) > 0) max(result$date, na.rm = TRUE) else NA,
      unit   = "day",
      source = source
    ),
    created         = Sys.time(),
    modified        = Sys.time(),
    years           = years,
    months          = months,
    source          = source,
    uf              = uf,
    biome           = biome,
    n_raw_points    = n_raw_points,
    n_observations  = n_obs,
    history         = sprintf(
      "[%s] sus_grid_fires(): source=%s, %d raw points, %d obs",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"), source, n_raw_points, n_obs
    ),
    user = list()
  )
}

#' Normalize raw INPE/FIRMS columns to canonical fire schema
#' @keywords internal
#' @noRd
.fires_normalize_cols <- function(df, source) {
  if (source == "inpe") {
    # INPE fields: lat, lon, datahora, municipio, estado, bioma, frp
    if ("datahora" %in% names(df)) {
      df$date <- as.Date(substr(df$datahora, 1L, 10L))
    } else if ("data" %in% names(df)) {
      df$date <- as.Date(df$data)
    }
    if ("bioma" %in% names(df)) df$biome  <- df$bioma
    if ("estado" %in% names(df)) df$estado <- df$estado
    # lat/lon should be present directly
  } else {
    # FIRMS CSV: latitude, longitude, acq_date, frp
    if ("latitude" %in% names(df))  df$lat  <- df$latitude
    if ("longitude" %in% names(df)) df$lon  <- df$longitude
    if ("acq_date" %in% names(df))  df$date <- as.Date(df$acq_date)
  }
  df$lat  <- as.numeric(df$lat)
  df$lon  <- as.numeric(df$lon)
  df$date <- if ("date" %in% names(df)) as.Date(df$date) else as.Date(NA)
  df$frp  <- if ("frp" %in% names(df)) suppressWarnings(as.numeric(df$frp)) else NA_real_
  df
}

#' Auto-detect municipality identifier column in sf object
#' @keywords internal
#' @noRd
.fires_detect_muni_col <- function(municipalities) {
  candidates <- c("code_muni", "CD_MUN", "CD_GEOCMU", "code_municipality")
  found <- intersect(candidates, names(municipalities))
  if (length(found) > 0) return(found[1])
  for (col in names(municipalities)) {
    vals <- as.character(municipalities[[col]])[!is.na(municipalities[[col]])]
    if (length(vals) > 0 &&
        all(grepl("^\\d{6,7}$", vals[seq_len(min(5, length(vals)))])))
      return(col)
  }
  cli::cli_abort(c(
    "Could not detect a municipality identifier column.",
    "i" = "Expected one of: {.val {paste(candidates, collapse = ', ')}}."
  ))
}

#' Download one month of fire data (INPE or FIRMS) and save as CSV
#' @keywords internal
#' @noRd
.fires_download_month <- function(source, date_start, date_end,
                                   cache_path, uf, biome, bbox,
                                   firms_key, use_cache, verbose, msg) {
  filename <- basename(cache_path)

  if (use_cache && file.exists(cache_path) && file.size(cache_path) > 0) {
    if (verbose) cli::cli_alert_success(
      glue::glue(msg$cache_hit, filename = filename))
    return(invisible(cache_path))
  }

  dir_path <- dirname(cache_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }

  if (verbose) cli::cli_alert_info(
    glue::glue(msg$download_file, filename = filename))

  df <- tryCatch({
    if (source == "inpe") {
      .fires_fetch_inpe(date_start, date_end, uf, biome)
    } else {
      .fires_fetch_firms(date_start, date_end, bbox, firms_key,
                         .fires_firms_product(source))
    }
  }, error = function(e) {
    cli::cli_warn(glue::glue(msg$download_error,
                             filename = filename, err = conditionMessage(e)))
    NULL
  })

  if (!is.null(df) && nrow(df) > 0) {
    utils::write.csv(df, cache_path, row.names = FALSE, fileEncoding = "UTF-8")
    if (verbose) cli::cli_alert_success(
      glue::glue(msg$download_done, filename = filename, n = nrow(df)))
  } else {
    # Write empty file as sentinel so cache check doesn't re-download
    writeLines(character(0), cache_path)
    if (verbose) cli::cli_alert_warning(
      glue::glue(msg$no_fires_period, filename = filename))
  }

  invisible(cache_path)
}

#' Fetch fire data from INPE Queimadas API for one month
#' @keywords internal
#' @noRd
.fires_fetch_inpe <- function(date_start, date_end, uf, biome) {
  base_url <- "https://queimadas.dgi.inpe.br/api/focos/"

  params <- list(
    dataInicio = paste0(date_start, "T00:00:00"),
    dataFim    = paste0(date_end,   "T23:59:59"),
    pais       = "Brasil"
  )

  if (!is.null(uf) && length(uf) > 0) {
    # INPE expects full state name; we provide UF abbreviation — API handles it
    params$estado <- paste(uf, collapse = ",")
  }
  if (!is.null(biome) && length(biome) > 0) {
    params$bioma <- paste(biome, collapse = ",")
  }

  req  <- httr2::request(base_url) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_timeout(60) |>
    httr2::req_retry(max_tries = 3)

  resp <- httr2::req_perform(req)
  body <- httr2::resp_body_string(resp)

  if (!nchar(trimws(body)) || body == "[]" || body == "null") {
    return(data.frame())
  }

  parsed <- jsonlite::fromJSON(body, simplifyDataFrame = TRUE)

  if (!is.data.frame(parsed) && is.list(parsed)) {
    parsed <- as.data.frame(do.call(rbind, parsed))
  }

  if (!is.data.frame(parsed) || nrow(parsed) == 0) return(data.frame())

  parsed
}

#' Fetch fire data from NASA FIRMS API for one month (in 10-day chunks)
#' @keywords internal
#' @noRd
.fires_fetch_firms <- function(date_start, date_end, bbox, firms_key, product) {
  # FIRMS limits: max 10 days per request
  # Chunk the month into 10-day periods
  start_d <- as.Date(date_start)
  end_d   <- as.Date(date_end)

  chunks <- list()
  cur    <- start_d
  while (cur <= end_d) {
    chunk_end <- min(cur + 9L, end_d)
    chunks[[length(chunks) + 1]] <- list(start = cur, end = chunk_end)
    cur <- chunk_end + 1L
  }

  bbox_str <- paste(bbox, collapse = ",")   # W,S,E,N
  all_parts <- lapply(chunks, function(ch) {
    days  <- as.integer(ch$end - ch$start) + 1L
    url   <- sprintf(
      "https://firms.modaps.eosdis.nasa.gov/api/area/csv/%s/%s/%s/%d/%s",
      firms_key, product, bbox_str, days,
      format(ch$start, "%Y-%m-%d")
    )
    req  <- httr2::request(url) |>
      httr2::req_timeout(60) |>
      httr2::req_retry(max_tries = 3)
    resp <- httr2::req_perform(req)
    body <- httr2::resp_body_string(resp)

    if (!nchar(trimws(body)) ||
        startsWith(trimws(body), "latitude") == FALSE) {
      return(NULL)
    }
    tryCatch(utils::read.csv(text = body, stringsAsFactors = FALSE),
             error = function(e) NULL)
  })

  all_parts <- all_parts[!vapply(all_parts, is.null, logical(1))]
  if (length(all_parts) == 0) return(data.frame())
  do.call(rbind, all_parts)
}

#' Multilingual messages for sus_grid_fires()
#' @keywords internal
#' @noRd
.fires_msgs <- list(
  pt = list(
    title                = "Dados de Focos de Inc\u00eandio",
    missing_years        = "{.arg years} \u00e9 obrigat\u00f3rio.",
    invalid_years_type   = "{.arg years} deve ser num\u00e9rico sem NA.",
    invalid_years_range  = "{.arg years} deve ser \u2265 {min_year}.",
    invalid_years_x      = "Ano(s) inv\u00e1lido(s): {bad}.",
    invalid_months       = "{.arg months} deve ser inteiro entre 1 e 12.",
    invalid_uf           = "{.arg uf} inv\u00e1lido: {bad}. Use: {valid}.",
    invalid_biome        = "{.arg biome} inv\u00e1lido: {bad}. Use: {valid}.",
    no_firms_key         = "FIRMS MAP KEY n\u00e3o encontrada. Defina FIRMS_MAP_KEY ou use {.arg firms_key}.",
    need_sf              = "O pacote {.pkg sf} \u00e9 necess\u00e1rio para agrega\u00e7\u00e3o espacial.",
    muni_not_sf          = "{.arg municipalities} deve ser um objeto {.cls sf}.",
    invalid_use_cache    = "{.arg use_cache} deve ser TRUE ou FALSE.",
    invalid_cache_dir    = "{.arg cache_dir} deve ser uma string n\u00e3o vazia.",
    download_start       = "Baixando dados de {n_months} m\u00eas/meses...",
    cache_hit            = "Cache encontrado: {filename}",
    download_file        = "Baixando: {filename}",
    download_done        = "Conclu\u00eddo: {filename} ({n} registros)",
    download_error       = "Falha ao baixar {filename}: {err}",
    no_fires_period      = "Nenhum foco encontrado para {filename}",
    read_warn            = "N\u00e3o foi poss\u00edvel ler {filename}.",
    no_data              = "Nenhum dado de foco encontrado para os par\u00e2metros fornecidos.",
    points_loaded        = "{n_points} foco(s) carregado(s).",
    spatial_join         = "Atribuindo focos a {n_mun} munic\u00edpio(s)...",
    no_fires_in_muni     = "Nenhum foco encontrado dentro dos pol\u00edgonos fornecidos.",
    sf_error             = "Erro ao criar objeto sf a partir das coordenadas.",
    st_join_error        = "Erro no st_join espacial.",
    agg_done             = "Conclu\u00eddo: {n_rows} observa\u00e7\u00f5es ({n_mun} munic\u00edpios).",
    done_points          = "Conclu\u00eddo: {n_rows} focos retornados."
  ),
  en = list(
    title                = "Fire Hotspot Data",
    missing_years        = "{.arg years} is required.",
    invalid_years_type   = "{.arg years} must be numeric without NA.",
    invalid_years_range  = "{.arg years} must be \u2265 {min_year}.",
    invalid_years_x      = "Invalid year(s): {bad}.",
    invalid_months       = "{.arg months} must be integer between 1 and 12.",
    invalid_uf           = "Invalid {.arg uf}: {bad}. Use: {valid}.",
    invalid_biome        = "Invalid {.arg biome}: {bad}. Use: {valid}.",
    no_firms_key         = "FIRMS MAP KEY not found. Set FIRMS_MAP_KEY or use {.arg firms_key}.",
    need_sf              = "Package {.pkg sf} is required for spatial aggregation.",
    muni_not_sf          = "{.arg municipalities} must be an {.cls sf} object.",
    invalid_use_cache    = "{.arg use_cache} must be TRUE or FALSE.",
    invalid_cache_dir    = "{.arg cache_dir} must be a non-empty string.",
    download_start       = "Downloading data for {n_months} month(s)...",
    cache_hit            = "Cache found: {filename}",
    download_file        = "Downloading: {filename}",
    download_done        = "Done: {filename} ({n} records)",
    download_error       = "Failed to download {filename}: {err}",
    no_fires_period      = "No fire hotspots found for {filename}",
    read_warn            = "Could not read {filename}.",
    no_data              = "No fire data found for the provided parameters.",
    points_loaded        = "{n_points} hotspot(s) loaded.",
    spatial_join         = "Assigning hotspots to {n_mun} municipality/ies...",
    no_fires_in_muni     = "No hotspots found within the provided polygons.",
    sf_error             = "Error creating sf object from coordinates.",
    st_join_error        = "Error in spatial st_join.",
    agg_done             = "Complete: {n_rows} observations ({n_mun} municipalities).",
    done_points          = "Complete: {n_rows} fire hotspots returned."
  ),
  es = list(
    title                = "Datos de Focos de Incendio",
    missing_years        = "{.arg years} es obligatorio.",
    invalid_years_type   = "{.arg years} debe ser num\u00e9rico sin NA.",
    invalid_years_range  = "{.arg years} debe ser \u2265 {min_year}.",
    invalid_years_x      = "A\u00f1o(s) inv\u00e1lido(s): {bad}.",
    invalid_months       = "{.arg months} debe ser entero entre 1 y 12.",
    invalid_uf           = "{.arg uf} inv\u00e1lido: {bad}. Use: {valid}.",
    invalid_biome        = "{.arg biome} inv\u00e1lido: {bad}. Use: {valid}.",
    no_firms_key         = "FIRMS MAP KEY no encontrado. Configure FIRMS_MAP_KEY o use {.arg firms_key}.",
    need_sf              = "El paquete {.pkg sf} es necesario para la agregaci\u00f3n espacial.",
    muni_not_sf          = "{.arg municipalities} debe ser un objeto {.cls sf}.",
    invalid_use_cache    = "{.arg use_cache} debe ser TRUE o FALSE.",
    invalid_cache_dir    = "{.arg cache_dir} debe ser una cadena no vac\u00eda.",
    download_start       = "Descargando datos de {n_months} mes/meses...",
    cache_hit            = "Cach\u00e9 encontrado: {filename}",
    download_file        = "Descargando: {filename}",
    download_done        = "Completado: {filename} ({n} registros)",
    download_error       = "Error al descargar {filename}: {err}",
    no_fires_period      = "Ning\u00fan foco encontrado para {filename}",
    read_warn            = "No se pudo leer {filename}.",
    no_data              = "Ning\u00fan dato de foco encontrado para los par\u00e1metros indicados.",
    points_loaded        = "{n_points} foco(s) cargado(s).",
    spatial_join         = "Asignando focos a {n_mun} municipio(s)...",
    no_fires_in_muni     = "Ning\u00fan foco encontrado dentro de los pol\u00edgonos proporcionados.",
    sf_error             = "Error al crear objeto sf desde coordenadas.",
    st_join_error        = "Error en st_join espacial.",
    agg_done             = "Completo: {n_rows} observaciones ({n_mun} municipios).",
    done_points          = "Completo: {n_rows} focos de incendio retornados."
  )
)
