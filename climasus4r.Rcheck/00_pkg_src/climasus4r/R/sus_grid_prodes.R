# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "code_muni", "date", "year", "deforested_area_km2", "n_patches",
  "area_km2_intersected", "state", "uid", "area_km",
  "bad", "valid", "err", "n_rows", "n_mun", "n_files"
))

# ── Exported function ─────────────────────────────────────────────────────────

#' Import PRODES Deforestation Data from TerraBrasilis for Brazilian Municipalities
#'
#' @description
#' `sus_grid_prodes()` downloads INPE PRODES annual deforestation data from the
#' TerraBrasilis WFS API, spatially intersects deforestation polygons with
#' municipality boundaries, and returns municipality-level annual deforestation
#' areas as a `climasus_df` compatible with [sus_mod_dlnm()] and
#' [sus_climate_anomaly()].
#'
#' PRODES (Projeto de Monitoramento do Desmatamento na Amazônia Legal por
#' Satélite) is INPE's primary deforestation monitoring program. Deforestation
#' exposure is a key driver of frontier malaria (SINAN), dengue, leishmaniasis,
#' and respiratory disease from biomass burning in Brazil.
#'
#' @param years Integer vector. Years to download. Availability by biome:
#'   Amazon 2007–present; all others 2000–present.
#'
#' @param biomes Character vector. Biomes to include. Any combination of:
#'   `"Amazon"`, `"Cerrado"`, `"MataAtlantica"`, `"Caatinga"`, `"Pampa"`,
#'   `"Pantanal"`. Default: all six biomes.
#'
#' @param uf Character vector. Optional state filter (e.g., `c("MT", "PA")`).
#'   Reduces download size. `NULL` = all states in the selected biome(s).
#'
#' @param municipalities An `sf` POLYGON object (e.g., from
#'   [geobr::read_municipality()]). When provided, deforestation polygons are
#'   spatially intersected with municipality boundaries and annual areas are
#'   aggregated. If `NULL`, raw deforestation polygons are returned as an sf
#'   `climasus_df`.
#'
#' @param use_cache Logical. Reuse previously downloaded GeoJSON files and
#'   aggregated Parquet caches. Default `TRUE`.
#'
#' @param cache_dir Character. Root cache directory.
#'   Default `"~/.climasus4r_cache/prodes"`.
#'
#' @param lang Character. Message language: `"pt"` (default), `"en"`, `"es"`.
#'
#' @param verbose Logical. Print progress. Default `TRUE`.
#'
#' @return
#' \itemize{
#'   \item If `municipalities` is provided: a `climasus_df` with columns
#'     `code_muni` (character), `date` (Date, Jan 1 of the PRODES year),
#'     `year` (integer), `deforested_area_km2` (numeric, total area of
#'     deforestation polygons intersecting the municipality), `n_patches`
#'     (integer, count of distinct deforestation patches), and `biome`
#'     (character). Metadata: `stage = "climate"`, `type = "prodes"`.
#'   \item If `municipalities = NULL`: the raw deforestation polygon sf object
#'     as a `climasus_df` with columns `year`, `state`, `area_km`, `biome`.
#' }
#'
#' @section PRODES year convention:
#' A PRODES year N covers August 1 of year N-1 through July 31 of year N.
#' The `date` column is set to `YYYY-01-01` by convention (the publication
#' year). For time-series analyses, note that most deforestation peaks occur
#' during the dry season (July–October).
#'
#' @section Data source:
#' INPE PRODES via TerraBrasilis WFS. No authentication required.\cr
#' All biomes accessed via \url{https://terrabrasilis.dpi.inpe.br/geoserver/wfs}
#'
#' @section Packages required:
#' `sf` is required for spatial intersection when `municipalities` is provided.
#' `httr2` and `jsonlite` are used for WFS downloads (in `Suggests`).
#'
#' @examples
#' \dontrun{
#' library(geobr)
#' mt_mun <- read_municipality(code_muni = "MT", year = 2020)
#'
#' # Annual deforestation for Mato Grosso municipalities, Amazon biome
#' prodes <- sus_grid_prodes(
#'   years          = 2015:2022,
#'   biomes         = "Amazon",
#'   uf             = "MT",
#'   municipalities = mt_mun,
#'   lang           = "pt"
#' )
#' sus_meta(prodes, "stage")  # "climate"
#' sus_meta(prodes, "type")   # "prodes"
#'
#' # Raw polygon data (no aggregation)
#' prodes_raw <- sus_grid_prodes(
#'   years  = 2022,
#'   biomes = "Amazon",
#'   uf     = "PA",
#'   lang   = "en"
#' )
#' }
#'
#' @seealso [sus_grid_fires()], [sus_climate_anomaly()], [sus_mod_dlnm()]
#'
#' @export
#' @importFrom rlang .data is_installed
#' @importFrom dplyr filter select rename mutate summarise n full_join join_by
#'   n_distinct all_of
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom tibble as_tibble
sus_grid_prodes <- function(
    years,
    biomes         = c("Amazon", "Cerrado", "MataAtlantica",
                       "Caatinga", "Pampa", "Pantanal"),
    uf             = NULL,
    municipalities = NULL,
    use_cache      = TRUE,
    cache_dir      = "~/.climasus4r_cache/prodes",
    lang           = "pt",
    verbose        = TRUE) {

  # ── 1. Validation ───────────────────────────────────────────────────────────

  if (!is.character(lang) || length(lang) != 1 || !lang %in% c("pt", "en", "es")) {
    cli::cli_abort("{.arg lang} must be 'pt', 'en', or 'es'.")
  }
  msg <- .prodes_msgs[[lang]]

  # Years
  if (missing(years) || is.null(years)) cli::cli_abort(msg$missing_years)
  if (!is.numeric(years) || any(is.na(years))) cli::cli_abort(msg$invalid_years_type)
  years <- sort(as.integer(unique(years)))
  bad_y <- years[years < 1988L | years > as.integer(format(Sys.Date(), "%Y"))]
  if (length(bad_y) > 0) {
    bad <- paste(bad_y, collapse = ", ")
    cli::cli_abort(c(msg$invalid_years_range, "x" = msg$invalid_years_x))
  }

  # Biomes
  valid_biomes <- names(.prodes_wfs_config)
  biomes <- unique(biomes)
  bad_b  <- setdiff(biomes, valid_biomes)
  if (length(bad_b) > 0) {
    bad   <- paste(bad_b, collapse = ", ")
    valid <- paste(valid_biomes, collapse = ", ")
    cli::cli_abort(msg$invalid_biomes)
  }

  # Warn if requested years are before a biome's start year
  for (b in biomes) {
    b_min  <- .prodes_wfs_config[[b]]$min_year
    early  <- years[years < b_min]
    if (length(early) > 0) {
      cli::cli_alert_warning(
        glue::glue(msg$biome_years_warn,
                   biome = b, min_year = b_min,
                   bad = paste(early, collapse = ", ")))
    }
  }
  # Remove years that have NO valid biome (all biomes skip those years)
  years_with_data <- Filter(function(yr) {
    any(vapply(biomes,
               function(b) yr >= .prodes_wfs_config[[b]]$min_year,
               logical(1)))
  }, years)
  if (length(years_with_data) == 0) cli::cli_abort(msg$no_data_params)
  years <- years_with_data

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

  # Municipalities
  if (!is.null(municipalities)) {
    if (!requireNamespace("sf", quietly = TRUE)) cli::cli_abort(msg$need_sf)
    if (!inherits(municipalities, "sf")) cli::cli_abort(msg$muni_not_sf)
  }

  rlang::check_installed("httr2",     reason = "to download PRODES WFS data")
  rlang::check_installed("sf",        reason = "to read GeoJSON from TerraBrasilis")
  rlang::check_installed("jsonlite",  reason = "to parse WFS JSON response")

  if (!is.logical(use_cache) || length(use_cache) != 1) cli::cli_abort(msg$invalid_use_cache)
  if (!is.character(cache_dir) || nchar(trimws(cache_dir)) == 0) cli::cli_abort(msg$invalid_cache_dir)
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)

  # ── 2. Build manifest (biome × year combination) ────────────────────────────
  manifest_rows <- list()
  for (b in biomes) {
    b_min   <- .prodes_wfs_config[[b]]$min_year
    b_years <- years[years >= b_min]
    for (yr in b_years) {
      uf_tag    <- if (!is.null(uf)) paste(sort(uf), collapse = "-") else "all"
      gjson_fn  <- sprintf("prodes_%s_%04d_%s.geojson",
                           tolower(b), yr, uf_tag)
      pq_fn     <- sprintf("prodes_%s_%04d_%s.parquet",
                           tolower(b), yr, uf_tag)
      manifest_rows[[length(manifest_rows) + 1]] <- list(
        biome      = b,
        year       = yr,
        cache_json = file.path(cache_dir, "geojson", gjson_fn),
        cache_pq   = file.path(cache_dir, "parquet",  pq_fn)
      )
    }
  }

  if (length(manifest_rows) == 0) cli::cli_abort(msg$no_data_params)

  manifest <- do.call(rbind, lapply(manifest_rows, as.data.frame,
                                     stringsAsFactors = FALSE))
  manifest$year <- as.integer(manifest$year)

  n_files <- nrow(manifest)
  if (verbose) {
    cli::cli_h1(msg$title)
    cli::cli_alert_info(glue::glue(msg$download_start, n_files = n_files))
  }

  # ── 3. Parquet early-return ──────────────────────────────────────────────────
  if (!is.null(municipalities) && use_cache &&
      all(file.exists(manifest$cache_pq))) {
    if (verbose) cli::cli_alert_success(msg$parquet_cache_hit)
    return(.prodes_build_from_parquet(manifest$cache_pq, verbose, msg))
  }

  # ── 4. Download WFS GeoJSON and process ──────────────────────────────────────

  # Prepare municipalities once
  if (!is.null(municipalities)) {
    muni_id_col <- .prodes_detect_muni_col(municipalities)
    if (muni_id_col != "code_muni") {
      municipalities$code_muni <- as.character(municipalities[[muni_id_col]])
    } else {
      municipalities$code_muni <- as.character(municipalities$code_muni)
    }
    municipalities$code_muni <- substr(municipalities$code_muni, 1L, 7L)
    municipalities <- sf::st_transform(municipalities, crs = 4326)
    municipalities <- sf::st_make_valid(municipalities)
    municipalities_slim <- municipalities[, "code_muni"]
    n_mun <- nrow(municipalities)
    if (verbose) cli::cli_alert_info(
      glue::glue(msg$spatial_start, n_mun = n_mun))
  }

  result_list <- vector("list", n_files)

  for (i in seq_len(n_files)) {
    b       <- manifest$biome[i]
    yr      <- manifest$year[i]
    gjson   <- manifest$cache_json[i]
    pq_path <- manifest$cache_pq[i]

    # Parquet cache hit for this row
    if (!is.null(municipalities) && use_cache &&
        file.exists(pq_path) && file.size(pq_path) > 0) {
      if (verbose) cli::cli_alert_success(
        glue::glue(msg$parquet_hit, filename = basename(pq_path)))
      result_list[[i]] <- .read_parquet_smart(pq_path)
      next
    }

    # Download WFS GeoJSON
    defor_sf <- tryCatch(
      .prodes_fetch_wfs(b, yr, uf, gjson, use_cache, verbose, msg),
      error = function(e) {
        cli::cli_warn(c(
          glue::glue(msg$download_error,
                     filename = sprintf("PRODES %s %d", b, yr)),
          "i" = conditionMessage(e)
        ))
        NULL
      }
    )

    if (is.null(defor_sf) || nrow(defor_sf) == 0) {
      if (verbose) cli::cli_alert_warning(
        glue::glue(msg$no_data_year, biome = b, year = yr))
      next
    }

    # Normalize area column across biomes
    if (!"area_km" %in% names(defor_sf)) {
      defor_sf$area_km <- as.numeric(sf::st_area(defor_sf)) / 1e6
    }
    defor_sf$biome <- b
    defor_sf$year  <- as.integer(yr)

    if (is.null(municipalities)) {
      # Return raw polygons
      result_list[[i]] <- sf::st_drop_geometry(defor_sf)[,
        intersect(names(defor_sf), c("year", "state", "area_km", "biome", "uid"))]
      next
    }

    # Spatial intersection for municipality-level aggregation
    df_i <- tryCatch({
      defor_clean <- sf::st_transform(sf::st_make_valid(defor_sf), crs = 4326)

      # Intersect deforestation with municipalities
      intersection <- sf::st_intersection(defor_clean, municipalities_slim)

      if (nrow(intersection) == 0) return(NULL)

      # Compute intersected area in km²
      intersection$area_km2_intersected <- as.numeric(
        sf::st_area(intersection)) / 1e6

      # Count distinct patches via uid if available
      has_uid <- "uid" %in% names(intersection)

      plain <- sf::st_drop_geometry(intersection)
      agg   <- plain |>
        dplyr::summarise(
          deforested_area_km2 = sum(.data$area_km2_intersected, na.rm = TRUE),
          n_patches = if (has_uid) dplyr::n_distinct(.data$uid)
                      else dplyr::n(),
          .by = c(code_muni, year)
        )
      agg$biome <- b
      agg$date  <- as.Date(sprintf("%04d-01-01", yr))
      agg$n_patches <- as.integer(agg$n_patches)
      agg
    }, error = function(e) {
      cli::cli_warn(c(
        glue::glue(msg$intersect_warn, biome = b, year = yr),
        "i" = conditionMessage(e)
      ))
      NULL
    })

    if (!is.null(df_i) && nrow(df_i) > 0) {
      pq_dir <- dirname(pq_path)
      if (!dir.exists(pq_dir)) {
        dir.create(pq_dir, recursive = TRUE, showWarnings = FALSE)
      }
      tryCatch(
        .write_parquet_smart(tibble::as_tibble(df_i), pq_path),
        error = function(e) cli::cli_alert_warning(
          glue::glue(msg$parquet_write_warn, filename = basename(pq_path)))
      )
    }
    result_list[[i]] <- df_i
  }

  # ── 5. Combine and return ────────────────────────────────────────────────────
  result_list <- result_list[!vapply(result_list, is.null, logical(1))]
  if (length(result_list) == 0) cli::cli_abort(msg$no_data)

  result <- do.call(rbind, result_list)
  result <- result[order(result$biome, result$year,
                          if (!is.null(municipalities)) result$code_muni
                          else result$state), ]
  rownames(result) <- NULL
  result <- tibble::as_tibble(result)

  n_rows <- nrow(result)
  if (verbose) cli::cli_alert_success(
    glue::glue(msg$done,
               n_rows = n_rows,
               n_mun  = if (!is.null(municipalities)) dplyr::n_distinct(result$code_muni) else NA_integer_))

  # ── 6. Build climasus_df ──────────────────────────────────────────────────────
  meta <- list(
    system   = NULL,
    stage    = "climate",
    type     = "prodes",
    spatial  = FALSE,
    temporal = list(
      start  = if (!is.null(municipalities)) min(result$date, na.rm = TRUE)
               else as.Date(sprintf("%04d-01-01", min(result$year, na.rm = TRUE))),
      end    = if (!is.null(municipalities)) max(result$date, na.rm = TRUE)
               else as.Date(sprintf("%04d-01-01", max(result$year, na.rm = TRUE))),
      unit   = "year",
      source = "terrabrasilis_inpe_prodes"
    ),
    created          = Sys.time(),
    modified         = Sys.time(),
    biomes           = biomes,
    years            = years,
    uf               = uf,
    n_municipalities = if (!is.null(municipalities)) nrow(municipalities) else NA_integer_,
    n_observations   = n_rows,
    history          = sprintf(
      "[%s] sus_grid_prodes(): biomes=%s, years=%d-%d, %d obs",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      paste(biomes, collapse = "+"),
      min(years), max(years), n_rows
    ),
    user = list()
  )

  base_classes <- setdiff(class(result), "climasus_df")
  structure(result, sus_meta = meta, class = c("climasus_df", base_classes))
}


# ── Internal helpers ──────────────────────────────────────────────────────────

#' PRODES WFS layer names per biome
#' @keywords internal
#' @noRd
.prodes_wfs_config <- list(
  Amazon = list(
    base      = "https://terrabrasilis.dpi.inpe.br/geoserver/wfs",
    typename  = "prodes-amazon-nb:yearly_deforestation_biome",
    min_year  = 2007L,
    state_field = "state",
    area_field  = "area_km"
  ),
  Cerrado = list(
    base      = "https://terrabrasilis.dpi.inpe.br/geoserver/wfs",
    typename  = "prodes-cerrado-nb:yearly_deforestation",
    min_year  = 2000L,
    state_field = "state",
    area_field  = "area_km"
  ),
  MataAtlantica = list(
    base      = "https://terrabrasilis.dpi.inpe.br/geoserver/wfs",
    typename  = "prodes-mata-atlantica-nb:yearly_deforestation",
    min_year  = 2000L,
    state_field = "state",
    area_field  = "area_km"
  ),
  Caatinga = list(
    base      = "https://terrabrasilis.dpi.inpe.br/geoserver/wfs",
    typename  = "prodes-caatinga-nb:yearly_deforestation",
    min_year  = 2000L,
    state_field = "state",
    area_field  = "area_km"
  ),
  Pampa = list(
    base      = "https://terrabrasilis.dpi.inpe.br/geoserver/wfs",
    typename  = "prodes-pampa-nb:yearly_deforestation",
    min_year  = 2000L,
    state_field = "state",
    area_field  = "area_km"
  ),
  Pantanal = list(
    base      = "https://terrabrasilis.dpi.inpe.br/geoserver/wfs",
    typename  = "prodes-pantanal-nb:yearly_deforestation",
    min_year  = 2000L,
    state_field = "state",
    area_field  = "area_km"
  )
)

#' Download PRODES deforestation polygons via TerraBrasilis WFS
#' @keywords internal
#' @noRd
.prodes_fetch_wfs <- function(biome, year, uf, cache_path,
                               use_cache, verbose, msg) {
  filename <- basename(cache_path)

  if (use_cache && file.exists(cache_path) && file.size(cache_path) > 1000) {
    if (verbose) cli::cli_alert_success(
      glue::glue(msg$cache_hit, filename = filename))
    return(sf::st_read(cache_path, quiet = TRUE))
  }

  dir_path <- dirname(cache_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }

  cfg <- .prodes_wfs_config[[biome]]

  # Build CQL filter
  cql_parts <- sprintf("year=%d", as.integer(year))
  if (!is.null(uf) && length(uf) > 0) {
    uf_filter <- paste(sprintf("'%s'", uf), collapse = ",")
    cql_parts <- paste0(cql_parts, sprintf(" AND %s IN (%s)",
                                            cfg$state_field, uf_filter))
  }

  if (verbose) cli::cli_alert_info(
    glue::glue(msg$download_file,
               filename = sprintf("PRODES %s %d", biome, year)))

  # Download JSON via httr2
  tmp_path <- tempfile(fileext = ".geojson")
  tryCatch({
    httr2::request(cfg$base) |>
      httr2::req_url_query(
        service      = "WFS",
        version      = "2.0.0",
        request      = "GetFeature",
        typeName     = cfg$typename,
        `CQL_FILTER` = cql_parts,
        outputFormat = "application/json"
      ) |>
      httr2::req_timeout(120) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform(path = tmp_path)

    # Validate and move to cache
    defor_sf <- sf::st_read(tmp_path, quiet = TRUE)
    if (nrow(defor_sf) > 0) {
      file.copy(tmp_path, cache_path, overwrite = TRUE)
    }
    unlink(tmp_path)

    if (verbose) cli::cli_alert_success(
      glue::glue(msg$download_done,
                 filename = sprintf("PRODES %s %d", biome, year),
                 n = nrow(defor_sf)))

    defor_sf
  }, error = function(e) {
    unlink(tmp_path)
    stop(conditionMessage(e))
  })
}

#' Assemble climasus_df from pre-existing Parquet caches
#' @keywords internal
#' @noRd
.prodes_build_from_parquet <- function(pq_paths, verbose, msg) {
  parts <- lapply(pq_paths, function(p) {
    tryCatch(.read_parquet_smart(p), error = function(e) NULL)
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) cli::cli_abort(msg$no_data)

  result <- do.call(rbind, parts)
  result <- tibble::as_tibble(result)

  n_rows <- nrow(result)
  if (verbose) cli::cli_alert_success(
    glue::glue(msg$done, n_rows = n_rows,
               n_mun = dplyr::n_distinct(result$code_muni)))

  meta <- list(
    system = NULL, stage = "climate", type = "prodes", spatial = FALSE,
    temporal = list(
      start = min(result$date, na.rm = TRUE),
      end   = max(result$date, na.rm = TRUE),
      source = "terrabrasilis_cache"
    ),
    created = Sys.time(), modified = Sys.time(),
    history = sprintf("[%s] sus_grid_prodes(): from Parquet cache, %d obs",
                      format(Sys.time(), "%Y-%m-%d %H:%M:%S"), n_rows),
    user = list()
  )
  base_classes <- setdiff(class(result), "climasus_df")
  structure(result, sus_meta = meta, class = c("climasus_df", base_classes))
}

#' Auto-detect municipality identifier column in sf object
#' @keywords internal
#' @noRd
.prodes_detect_muni_col <- function(municipalities) {
  candidates <- c("code_muni", "CD_MUN", "CD_GEOCMU", "geocodigo",
                  "code_municipality")
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

#' Multilingual messages for sus_grid_prodes()
#' @keywords internal
#' @noRd
.prodes_msgs <- list(
  pt = list(
    title              = "Dados PRODES de Desmatamento (TerraBrasilis / INPE)",
    missing_years      = "{.arg years} \u00e9 obrigat\u00f3rio.",
    invalid_years_type = "{.arg years} deve ser num\u00e9rico sem NA.",
    invalid_years_range = "{.arg years} deve estar entre 1988 e o ano atual.",
    invalid_years_x    = "Ano(s) inv\u00e1lido(s): {bad}.",
    invalid_biomes     = "{.arg biomes} inv\u00e1lido: {bad}. Use: {valid}.",
    biome_years_warn   = "PRODES {biome} dispon\u00edvel a partir de {min_year}. Ignorando anos {bad}.",
    invalid_uf         = "{.arg uf} inv\u00e1lido: {bad}. Use: {valid}.",
    need_sf            = "O pacote {.pkg sf} \u00e9 necess\u00e1rio para a interse\u00e7\u00e3o espacial.",
    muni_not_sf        = "{.arg municipalities} deve ser um objeto {.cls sf}.",
    invalid_use_cache  = "{.arg use_cache} deve ser TRUE ou FALSE.",
    invalid_cache_dir  = "{.arg cache_dir} deve ser uma string n\u00e3o vazia.",
    no_data_params     = "Nenhum dado dispon\u00edvel para os par\u00e2metros fornecidos.",
    download_start     = "Baixando {n_files} camada(s) PRODES do TerraBrasilis...",
    cache_hit          = "Cache encontrado: {filename}",
    download_file      = "Baixando: {filename}",
    download_done      = "Conclu\u00eddo: {filename} ({n} pol\u00edgonos)",
    download_error     = "Falha ao baixar {filename}: {err}",
    parquet_cache_hit  = "Todos os dados no cache Parquet. Carregando...",
    parquet_hit        = "Cache Parquet: {filename}",
    parquet_write_warn = "N\u00e3o foi poss\u00edvel salvar cache Parquet: {filename}",
    no_data_year       = "Nenhum pol\u00edgono encontrado para {biome} {year}.",
    spatial_start      = "Calculando interse\u00e7\u00f5es com {n_mun} munic\u00edpio(s)...",
    intersect_warn     = "Erro ao processar interse\u00e7\u00e3o para {biome} {year}.",
    no_data            = "Nenhum dado foi processado com sucesso.",
    done               = "Conclu\u00eddo: {n_rows} observa\u00e7\u00f5es ({n_mun} munic\u00edpios)."
  ),
  en = list(
    title              = "PRODES Deforestation Data (TerraBrasilis / INPE)",
    missing_years      = "{.arg years} is required.",
    invalid_years_type = "{.arg years} must be numeric without NA.",
    invalid_years_range = "{.arg years} must be between 1988 and the current year.",
    invalid_years_x    = "Invalid year(s): {bad}.",
    invalid_biomes     = "Invalid {.arg biomes}: {bad}. Use: {valid}.",
    biome_years_warn   = "PRODES {biome} available from {min_year}. Skipping years {bad}.",
    invalid_uf         = "Invalid {.arg uf}: {bad}. Use: {valid}.",
    need_sf            = "Package {.pkg sf} is required for spatial intersection.",
    muni_not_sf        = "{.arg municipalities} must be an {.cls sf} object.",
    invalid_use_cache  = "{.arg use_cache} must be TRUE or FALSE.",
    invalid_cache_dir  = "{.arg cache_dir} must be a non-empty string.",
    no_data_params     = "No data available for the provided parameters.",
    download_start     = "Downloading {n_files} PRODES layer(s) from TerraBrasilis...",
    cache_hit          = "Cache found: {filename}",
    download_file      = "Downloading: {filename}",
    download_done      = "Done: {filename} ({n} polygons)",
    download_error     = "Failed to download {filename}: {err}",
    parquet_cache_hit  = "All data found in Parquet cache. Loading...",
    parquet_hit        = "Parquet cache: {filename}",
    parquet_write_warn = "Could not write Parquet cache: {filename}",
    no_data_year       = "No polygons found for {biome} {year}.",
    spatial_start      = "Computing intersections with {n_mun} municipality/ies...",
    intersect_warn     = "Error processing intersection for {biome} {year}.",
    no_data            = "No data was successfully processed.",
    done               = "Complete: {n_rows} observations ({n_mun} municipalities)."
  ),
  es = list(
    title              = "Datos PRODES de Deforestaci\u00f3n (TerraBrasilis / INPE)",
    missing_years      = "{.arg years} es obligatorio.",
    invalid_years_type = "{.arg years} debe ser num\u00e9rico sin NA.",
    invalid_years_range = "{.arg years} debe estar entre 1988 y el a\u00f1o actual.",
    invalid_years_x    = "A\u00f1o(s) inv\u00e1lido(s): {bad}.",
    invalid_biomes     = "{.arg biomes} inv\u00e1lido: {bad}. Use: {valid}.",
    biome_years_warn   = "PRODES {biome} disponible desde {min_year}. Omitiendo a\u00f1os {bad}.",
    invalid_uf         = "{.arg uf} inv\u00e1lido: {bad}. Use: {valid}.",
    need_sf            = "El paquete {.pkg sf} es necesario para la intersecci\u00f3n espacial.",
    muni_not_sf        = "{.arg municipalities} debe ser un objeto {.cls sf}.",
    invalid_use_cache  = "{.arg use_cache} debe ser TRUE o FALSE.",
    invalid_cache_dir  = "{.arg cache_dir} debe ser una cadena no vac\u00eda.",
    no_data_params     = "No hay datos disponibles para los par\u00e1metros indicados.",
    download_start     = "Descargando {n_files} capa(s) PRODES de TerraBrasilis...",
    cache_hit          = "Cach\u00e9 encontrado: {filename}",
    download_file      = "Descargando: {filename}",
    download_done      = "Completado: {filename} ({n} pol\u00edgonos)",
    download_error     = "Error al descargar {filename}: {err}",
    parquet_cache_hit  = "Todos los datos en cach\u00e9 Parquet. Cargando...",
    parquet_hit        = "Cach\u00e9 Parquet: {filename}",
    parquet_write_warn = "No se pudo guardar cach\u00e9 Parquet: {filename}",
    no_data_year       = "No se encontraron pol\u00edgonos para {biome} {year}.",
    spatial_start      = "Calculando intersecciones con {n_mun} municipio(s)...",
    intersect_warn     = "Error al procesar intersecci\u00f3n para {biome} {year}.",
    no_data            = "No se proces\u00f3 ning\u00fan dato correctamente.",
    done               = "Completo: {n_rows} observaciones ({n_mun} municipios)."
  )
)
