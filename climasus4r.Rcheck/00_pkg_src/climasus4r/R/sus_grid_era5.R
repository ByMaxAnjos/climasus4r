#' Import ERA5-Land Daily Climate Data for Brazilian Municipalities
#'
#' @description
#' `sus_grid_era5()` downloads pre-processed ERA5-Land daily climate aggregates
#' for Latin America from Zenodo and, optionally, spatially aggregates them to
#' Brazilian municipalities. No API key is required.
#'
#' The data source is the **ERA5-Land Daily Aggregates for Latin America** project
#' (Saldanha, rfsaldanha.github.io), which provides NetCDF files (one per
#' variable × month × year) at ~10 km spatial resolution for 1950–2025.
#' Files are hosted on Zenodo under CC-BY 4.0.
#'
#' When `municipalities` is provided, the function returns a `climasus_df` at
#' `stage = "climate"`, `type = "era5_land"` — directly compatible with
#' [sus_climate_anomaly()], [sus_climate_aggregate()], and [sus_mod_dlnm()].
#'
#' @param years Numeric vector of years to import. Must be between 1950 and 2025.
#'   Example: `2010:2023`, `c(2019, 2021)`.
#'
#' @param months Integer vector of months to import (1–12). Default: `1:12`
#'   (all months).
#'
#' @param vars Character vector of variable aliases to download. Allowed values:
#'   \itemize{
#'     \item `"t2m"` — 2 m temperature mean (→ `tair_dry_bulb_c`, °C)
#'     \item `"t2m_max"` — 2 m temperature maximum (→ `tair_max_c`, °C)
#'     \item `"t2m_min"` — 2 m temperature minimum (→ `tair_min_c`, °C)
#'     \item `"td2m"` — 2 m dewpoint temperature mean (→ `dew_point_c`, °C)
#'     \item `"u10"` — 10 m u-component of wind mean (→ `ws_10m_u_m_s`, m/s)
#'     \item `"v10"` — 10 m v-component of wind mean (→ `ws_10m_v_m_s`, m/s)
#'     \item `"sp"` — surface pressure mean (→ `patm_hpa`, hPa)
#'     \item `"tp"` — total precipitation daily sum (→ `rainfall_mm`, mm)
#'     \item `"all"` — all variables above
#'   }
#'   Default: `c("t2m", "tp")`.
#'
#' @param municipalities An `sf` object with polygon geometries representing the
#'   areas to which raster data will be aggregated (typically Brazilian
#'   municipalities from [geobr::read_municipality()]). Must contain a column
#'   identifying each polygon — the function auto-detects `code_muni`, `CD_MUN`,
#'   or `CD_GEOCMU`. If `NULL`, returns a named list of downloaded NetCDF file
#'   paths instead of a `climasus_df`.
#'
#' @param agg_fun Character. Spatial aggregation function applied over raster
#'   pixels within each polygon. Any function supported by
#'   `exactextractr::exact_extract()`: `"mean"` (default, area-weighted),
#'   `"sum"`, `"median"`, `"min"`, `"max"`. For precipitation, `"mean"` gives
#'   the representative areal value.
#'
#' @param use_cache Logical. If `TRUE` (default), downloaded NetCDF files are
#'   stored in `cache_dir` and reused on subsequent calls.
#'
#' @param cache_dir Character. Directory for cached NetCDF files.
#'   Default: `"~/.climasus4r_cache/era5"`. Created automatically if needed.
#'
#' @param parallel Logical. If `TRUE`, downloads run in parallel using the
#'   active [future::plan()]. Do not set a plan inside this function; configure
#'   it beforehand. Default: `FALSE`.
#'
#' @param workers Integer. Number of parallel workers when `parallel = TRUE`.
#'   Default: `2`.
#'
#' @param lang Character. Language for messages: `"pt"` (default), `"en"`,
#'   or `"es"`.
#'
#' @param verbose Logical. If `TRUE` (default), prints progress messages.
#'
#' @return
#' \itemize{
#'   \item If `municipalities` is provided: a `climasus_df` tibble with columns
#'     `code_muni` (character), `date` (Date), and one column per requested
#'     variable (e.g., `tair_dry_bulb_c`, `rainfall_mm`). Metadata:
#'     `stage = "climate"`, `type = "era5_land"`.
#'   \item If `municipalities = NULL`: a named character vector of paths to
#'     the downloaded NetCDF files (named by `"{year}_{month}_{var}"`).
#' }
#'
#' @section Data Source:
#' Saldanha, R. ERA5-Land Daily Aggregates for Latin America (1950–2025).
#' Zenodo. <https://zenodo.org/doi/10.5281/zenodo.10013254>. CC-BY 4.0.
#'
#' @section Unit Conversions Applied:
#' \itemize{
#'   \item Temperature (K → °C): subtract 273.15
#'   \item Precipitation (m → mm): multiply by 1000
#'   \item Pressure (Pa → hPa): divide by 100
#'   \item Wind components (m/s): no conversion needed
#' }
#'
#' @examples
#' \dontrun{
#' library(geobr)
#' mt_mun <- read_municipality(code_muni = "MT", year = 2020)
#'
#' # Temperature and precipitation for Mato Grosso, Q1 2020
#' era5 <- sus_grid_era5(
#'   years          = 2020,
#'   months         = 1:3,
#'   vars           = c("t2m", "tp"),
#'   municipalities = mt_mun,
#'   lang           = "pt"
#' )
#'
#' sus_meta(era5, "stage")  # "climate"
#' sus_meta(era5, "type")   # "era5_land"
#'
#' # All variables, no spatial aggregation (returns file paths)
#' paths <- sus_grid_era5(years = 2020, months = 1, vars = "all")
#' }
#'
#' @seealso [sus_climate_anomaly()], [sus_climate_aggregate()], [sus_mod_dlnm()]
#'
#' @export
#' @importFrom rlang .data
sus_grid_era5 <- function(
    years,
    months         = 1:12,
    vars           = c("t2m", "tp"),
    municipalities = NULL,
    agg_fun        = "mean",
    use_cache      = TRUE,
    cache_dir      = "~/.climasus4r_cache/era5",
    parallel       = FALSE,
    workers        = 2,
    lang           = "pt",
    verbose        = TRUE) {

  # ============================================================================
  # VALIDATION
  # ============================================================================

  # --- lang -------------------------------------------------------------------
  if (!is.character(lang) || length(lang) != 1 || !lang %in% c("pt", "en", "es")) {
    cli::cli_abort("{.arg lang} must be one of 'pt', 'en', or 'es'.")
  }

  # Messages (resolved early so validation errors can use them)
  msg <- .era5_msgs[[lang]]

  # --- years ------------------------------------------------------------------
  if (missing(years) || is.null(years)) {
    cli::cli_abort(msg$missing_years)
  }
  if (!is.numeric(years) || any(is.na(years))) {
    cli::cli_abort(msg$invalid_years_type)
  }
  years <- sort(as.integer(unique(years)))
  bad_years <- years[years < 1950L | years > 2025L]
  if (length(bad_years) > 0) {
    bad <- paste(bad_years, collapse = ", ")
    cli::cli_abort(c(msg$invalid_years_range, "x" = msg$invalid_years_x))
  }

  # --- months -----------------------------------------------------------------
  if (!is.numeric(months) || any(is.na(months)) ||
      any(months < 1 | months > 12)) {
    cli::cli_abort(msg$invalid_months)
  }
  months <- sort(as.integer(unique(months)))

  # --- vars -------------------------------------------------------------------
  valid_var_aliases <- names(.era5_var_map)
  if (identical(vars, "all")) {
    vars <- valid_var_aliases
  } else {
    if (!is.character(vars) || length(vars) == 0) {
      cli::cli_abort(msg$invalid_vars_type)
    }
    bad_vars <- setdiff(vars, valid_var_aliases)
    if (length(bad_vars) > 0) {
      bad   <- paste(bad_vars, collapse = ", ")
      valid <- paste(valid_var_aliases, collapse = ", ")
      cli::cli_abort(msg$invalid_vars)
    }
  }

  # --- municipalities ---------------------------------------------------------
  if (!is.null(municipalities)) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      cli::cli_abort(msg$need_sf)
    }
    if (!inherits(municipalities, "sf")) {
      cli::cli_abort(msg$municipalities_not_sf)
    }
    rlang::check_installed("exactextractr",
      reason = "to aggregate ERA5-Land rasters to municipality polygons")
  }

  # --- agg_fun ----------------------------------------------------------------
  valid_agg <- c("mean", "sum", "median", "min", "max", "majority", "minority",
                 "count", "variety")
  if (!is.character(agg_fun) || length(agg_fun) != 1 ||
      !agg_fun %in% valid_agg) {
    valid <- paste(valid_agg, collapse = ", ")
    cli::cli_abort(c(
      msg$invalid_agg,
      "i" = msg$invalid_agg_i
    ))
  }

  # --- use_cache / cache_dir --------------------------------------------------
  if (!is.logical(use_cache) || length(use_cache) != 1 || is.na(use_cache)) {
    cli::cli_abort(msg$invalid_use_cache)
  }
  if (!is.character(cache_dir) || length(cache_dir) != 1 ||
      nchar(trimws(cache_dir)) == 0) {
    cli::cli_abort(msg$invalid_cache_dir)
  }
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)

  # --- parallel / workers -----------------------------------------------------
  if (!is.logical(parallel) || length(parallel) != 1 || is.na(parallel)) {
    cli::cli_abort(msg$invalid_parallel)
  }
  if (!is.numeric(workers) || length(workers) != 1 ||
      is.na(workers) || workers < 1) {
    cli::cli_abort(msg$invalid_workers)
  }
  workers <- as.integer(workers)

  # ============================================================================
  # REQUIRE TERRA
  # ============================================================================
  rlang::check_installed("terra", reason = "to read ERA5-Land NetCDF files")

  # ============================================================================
  # BUILD DOWNLOAD MANIFEST
  # ============================================================================
  # One entry per (year, month, var alias) combination
  manifest <- expand.grid(
    year  = years,
    month = months,
    alias = vars,
    stringsAsFactors = FALSE
  )
  manifest$alias <- as.character(manifest$alias)

  # Resolve each alias to its Zenodo descriptor
  manifest$indicator <- vapply(manifest$alias, function(a) .era5_var_map[[a]]$ind, character(1))
  manifest$agg_label <- vapply(manifest$alias, function(a) .era5_var_map[[a]]$agg, character(1))
  manifest$out_col   <- vapply(manifest$alias, function(a) .era5_var_map[[a]]$col, character(1))

  # Build filenames and download URLs
  manifest$filename <- mapply(
    .era5_nc_filename,
    manifest$indicator,
    manifest$year,
    manifest$month,
    manifest$agg_label,
    SIMPLIFY = TRUE
  )

  manifest$url <- mapply(
    function(yr, fn) .era5_zenodo_url(yr, fn),
    manifest$year,
    manifest$filename,
    SIMPLIFY = TRUE
  )

  # Cache paths: cache_dir/{year}/{filename}
  manifest$cache_path <- file.path(
    cache_dir,
    as.character(manifest$year),
    manifest$filename
  )

  # Deduplicate by (indicator, year, month) so t2m_max and t2m_min that share
  # the same Zenodo file don't trigger double downloads
  file_manifest <- unique(manifest[, c("year", "month", "indicator", "agg_label",
                                       "filename", "url", "cache_path")])

  n_files <- nrow(file_manifest)
  if (verbose) {
    cli::cli_alert_info(glue::glue(msg$download_start, n_files = n_files))
  }

  # ============================================================================
  # DOWNLOAD WITH CACHE
  # ============================================================================
  if (parallel && n_files > 1) {
    rlang::check_installed("furrr", reason = "for parallel ERA5-Land downloads")
    oplan <- future::plan(future::multisession, workers = workers)
    on.exit(future::plan(oplan), add = TRUE)
    dl_results <- furrr::future_map(
      seq_len(n_files),
      function(i) {
        .era5_download_file(
          url        = file_manifest$url[i],
          cache_path = file_manifest$cache_path[i],
          use_cache  = use_cache,
          verbose    = verbose,
          msg        = msg
        )
      },
      .options = furrr::furrr_options(seed = TRUE)
    )
  } else {
    dl_results <- lapply(seq_len(n_files), function(i) {
      .era5_download_file(
        url        = file_manifest$url[i],
        cache_path = file_manifest$cache_path[i],
        use_cache  = use_cache,
        verbose    = verbose,
        msg        = msg
      )
    })
  }

  # Attach actual paths back to file_manifest
  file_manifest$actual_path <- unlist(dl_results)

  # If no spatial aggregation requested: return file paths
  if (is.null(municipalities)) {
    result_paths <- stats::setNames(
      file_manifest$actual_path,
      paste(file_manifest$year, sprintf("%02d", file_manifest$month),
            file_manifest$indicator, file_manifest$agg_label, sep = "_")
    )
    if (verbose) {
      cli::cli_alert_success(glue::glue(msg$done_paths, n = length(result_paths)))
    }
    return(result_paths)
  }

  # ============================================================================
  # DETECT MUNICIPALITY ID COLUMN
  # ============================================================================
  muni_id_col <- .era5_detect_muni_col(municipalities)
  if (verbose) {
    cli::cli_alert_info(glue::glue(msg$muni_col, col = muni_id_col))
  }

  # Ensure code_muni is always the output column name; copy if different
  if (muni_id_col != "code_muni") {
    municipalities$code_muni <- as.character(municipalities[[muni_id_col]])
  } else {
    municipalities$code_muni <- as.character(municipalities$code_muni)
  }

  # Normalize to 6-digit IBGE code (drop 7th check digit if present)
  municipalities$code_muni <- substr(municipalities$code_muni, 1, 6)

  # Reproject to WGS84 (CRS of ERA5-Land)
  municipalities <- sf::st_transform(municipalities, crs = 4326)

  n_mun <- nrow(municipalities)
  if (verbose) {
    cli::cli_alert_info(glue::glue(msg$agg_start, n_mun = n_mun))
  }

  # ============================================================================
  # EXTRACT RASTER → POLYGONS, PER VAR ALIAS
  # ============================================================================
  # Process var by var. Each alias may map to a different file.
  # We collect a data.frame per alias: (code_muni, date, {out_col})
  alias_results <- lapply(vars, function(alias) {
    vmap     <- .era5_var_map[[alias]]
    indicator <- vmap$ind
    agg_label <- vmap$agg
    out_col   <- vmap$col
    conv_fn   <- vmap$conv

    # Files for this alias (all year × month combinations)
    alias_files <- file_manifest[
      file_manifest$indicator == indicator &
        file_manifest$agg_label == agg_label, ]

    # Process each monthly file
    monthly_dfs <- lapply(seq_len(nrow(alias_files)), function(j) {
      nc_path    <- alias_files$actual_path[j]
      file_year  <- alias_files$year[j]
      file_month <- alias_files$month[j]
      start_date <- as.Date(sprintf("%04d-%02d-01", file_year, file_month))

      tryCatch(
        .era5_extract_monthly(
          nc_path        = nc_path,
          municipalities = municipalities,
          agg_fun        = agg_fun,
          out_col        = out_col,
          conv_fn        = conv_fn,
          start_date     = start_date
        ),
        error = function(e) {
          cli::cli_warn(c(
            glue::glue(msg$extract_warn, file = basename(nc_path)),
            "i" = conditionMessage(e)
          ))
          NULL
        }
      )
    })

    monthly_dfs <- monthly_dfs[!vapply(monthly_dfs, is.null, logical(1))]
    if (length(monthly_dfs) == 0) return(NULL)
    do.call(rbind, monthly_dfs)
  })

  alias_results <- alias_results[!vapply(alias_results, is.null, logical(1))]
  if (length(alias_results) == 0) {
    cli::cli_abort(msg$no_data)
  }

  # ============================================================================
  # MERGE ALL VARIABLE COLUMNS
  # ============================================================================
  base_df <- alias_results[[1]][, c("code_muni", "date")]
  base_df <- unique(base_df)

  for (adf in alias_results) {
    merge_cols <- intersect(names(adf), c("code_muni", "date",
                                           setdiff(names(adf), c("code_muni", "date"))))
    base_df <- merge(base_df, adf, by = c("code_muni", "date"), all = TRUE)
  }

  # Sort
  base_df <- base_df[order(base_df$code_muni, base_df$date), ]
  rownames(base_df) <- NULL

  # Convert to tibble
  result <- tibble::as_tibble(base_df)

  n_rows <- nrow(result)
  if (verbose) {
    cli::cli_alert_success(glue::glue(msg$agg_done, n_rows = n_rows, n_mun = n_mun))
  }

  # ============================================================================
  # BUILD climasus_df
  # ============================================================================
  meta <- list(
    system  = NULL,
    stage   = "climate",
    type    = "era5_land",
    spatial = FALSE,
    temporal = list(
      start  = min(result$date, na.rm = TRUE),
      end    = max(result$date, na.rm = TRUE),
      unit   = "day",
      source = "zenodo_era5land"
    ),
    created         = Sys.time(),
    modified        = Sys.time(),
    years           = years,
    months          = months,
    vars            = vars,
    n_municipalities = n_mun,
    n_observations  = n_rows,
    agg_fun         = agg_fun,
    history         = sprintf(
      "[%s] sus_grid_era5(): %d var(s), %d municipalities, %d obs",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      length(vars), n_mun, n_rows
    ),
    user = list()
  )

  base_classes <- setdiff(class(result), "climasus_df")
  result <- structure(
    result,
    sus_meta = meta,
    class    = c("climasus_df", base_classes)
  )

  result
}


# ==============================================================================
# INTERNAL CONSTANTS
# ==============================================================================

#' Zenodo Record IDs for ERA5-Land Latin America, by year (1950–2025)
#' @keywords internal
#' @noRd
.era5_zenodo_ids <- c(
  "1950" = 10013255L, "1951" = 10013696L, "1952" = 10013781L,
  "1953" = 10014198L, "1954" = 10014369L, "1955" = 10014474L,
  "1956" = 10014693L, "1957" = 10014722L, "1958" = 10014754L,
  "1959" = 10014771L, "1960" = 10014790L, "1961" = 10020497L,
  "1962" = 10020520L, "1963" = 10020530L, "1964" = 10020539L,
  "1965" = 10020552L, "1966" = 10020600L, "1967" = 10020663L,
  "1968" = 10020679L, "1969" = 10020690L, "1970" = 10020859L,
  "1971" = 10021122L, "1972" = 10021300L, "1973" = 10021667L,
  "1974" = 10021706L, "1975" = 10021943L, "1976" = 10021943L,
  "1977" = 10022017L, "1978" = 10022061L, "1979" = 10022145L,
  "1980" = 10022315L, "1981" = 10022536L, "1982" = 10022546L,
  "1983" = 10022561L, "1984" = 10022571L, "1985" = 10022579L,
  "1986" = 10022589L, "1987" = 10022593L, "1988" = 10022607L,
  "1989" = 10022632L, "1990" = 10022641L, "1991" = 10032814L,
  "1992" = 10032859L, "1993" = 10033251L, "1994" = 10033276L,
  "1995" = 10033306L, "1996" = 10033353L, "1997" = 10033755L,
  "1998" = 10033835L, "1999" = 10033983L, "2000" = 10033995L,
  "2001" = 10034036L, "2002" = 10034077L, "2003" = 10034110L,
  "2004" = 10034145L, "2005" = 10034179L, "2006" = 10034204L,
  "2007" = 10034283L, "2008" = 10034323L, "2009" = 10034370L,
  "2010" = 10034386L, "2011" = 10034412L, "2012" = 10034443L,
  "2013" = 10034494L, "2014" = 10034523L, "2015" = 10034541L,
  "2016" = 10034598L, "2017" = 10034630L, "2018" = 10036123L,
  "2019" = 10036132L, "2020" = 10036153L, "2021" = 10036162L,
  "2022" = 10036168L, "2023" = 10889682L, "2024" = 15748090L,
  "2025" = 18256859L
)

#' Variable mapping: alias → (Zenodo indicator, aggregation label, output column, unit conv)
#' @keywords internal
#' @noRd
.era5_var_map <- list(
  t2m     = list(ind = "2m_temperature",          agg = "mean",
                 col = "tair_dry_bulb_c",          conv = function(x) x - 273.15),
  t2m_max = list(ind = "2m_temperature",          agg = "max",
                 col = "tair_max_c",               conv = function(x) x - 273.15),
  t2m_min = list(ind = "2m_temperature",          agg = "min",
                 col = "tair_min_c",               conv = function(x) x - 273.15),
  td2m    = list(ind = "2m_dewpoint_temperature", agg = "mean",
                 col = "dew_point_c",              conv = function(x) x - 273.15),
  u10     = list(ind = "10m_u_component_of_wind", agg = "mean",
                 col = "ws_10m_u_m_s",             conv = identity),
  v10     = list(ind = "10m_v_component_of_wind", agg = "mean",
                 col = "ws_10m_v_m_s",             conv = identity),
  sp      = list(ind = "surface_pressure",        agg = "mean",
                 col = "patm_hpa",                 conv = function(x) x / 100),
  tp      = list(ind = "total_precipitation",     agg = "sum",
                 col = "rainfall_mm",              conv = function(x) x * 1000)
)

#' Multilingual messages for sus_grid_era5()
#' @keywords internal
#' @noRd
.era5_msgs <- list(
  pt = list(
    missing_years       = "{.arg years} \u00e9 obrigat\u00f3rio.",
    invalid_years_type  = "{.arg years} deve ser um vetor num\u00e9rico sem NA.",
    invalid_years_range = "{.arg years} deve estar entre 1950 e 2025.",
    invalid_years_x     = "Ano(s) inv\u00e1lido(s): {bad}.",
    invalid_months      = "{.arg months} deve ser um vetor inteiro entre 1 e 12.",
    invalid_vars_type   = "{.arg vars} deve ser um vetor de caracteres.",
    invalid_vars        = "{.arg vars} cont\u00e9m alias inv\u00e1lido(s): {bad}.\nUse: {valid}.",
    need_sf             = "O pacote {.pkg sf} \u00e9 necess\u00e1rio para agregar por munic\u00edpios.",
    municipalities_not_sf = "{.arg municipalities} deve ser um objeto {.cls sf}.",
    invalid_agg         = "{.arg agg_fun} inv\u00e1lido.",
    invalid_agg_i       = "Op\u00e7\u00f5es v\u00e1lidas: {valid}.",
    invalid_use_cache   = "{.arg use_cache} deve ser TRUE ou FALSE.",
    invalid_cache_dir   = "{.arg cache_dir} deve ser uma string n\u00e3o vazia.",
    invalid_parallel    = "{.arg parallel} deve ser TRUE ou FALSE.",
    invalid_workers     = "{.arg workers} deve ser um inteiro positivo.",
    download_start      = "Baixando {n_files} arquivo(s) ERA5-Land do Zenodo...",
    cache_hit           = "Cache encontrado: {filename}",
    download_file       = "Baixando: {filename}",
    download_done       = "Conclu\u00eddo: {filename}",
    download_error      = "Falha ao baixar {filename}: {err}",
    muni_col            = "Coluna identificadora de munic\u00edpios: {col}",
    agg_start           = "Agregando para {n_mun} munic\u00edpio(s)...",
    agg_done            = "Agrega\u00e7\u00e3o conclu\u00edda: {n_rows} observa\u00e7\u00f5es ({n_mun} munic\u00edpios).",
    extract_warn        = "N\u00e3o foi poss\u00edvel processar {file}.",
    no_data             = "Nenhum dado foi extra\u00eddo com sucesso.",
    done_paths          = "{n} arquivo(s) dispon\u00edvel(is) no cache."
  ),
  en = list(
    missing_years       = "{.arg years} is required.",
    invalid_years_type  = "{.arg years} must be a numeric vector without NA.",
    invalid_years_range = "{.arg years} must be between 1950 and 2025.",
    invalid_years_x     = "Invalid year(s): {bad}.",
    invalid_months      = "{.arg months} must be an integer vector between 1 and 12.",
    invalid_vars_type   = "{.arg vars} must be a character vector.",
    invalid_vars        = "{.arg vars} contains invalid alias(es): {bad}.\nUse: {valid}.",
    need_sf             = "Package {.pkg sf} is required to aggregate by municipality.",
    municipalities_not_sf = "{.arg municipalities} must be an {.cls sf} object.",
    invalid_agg         = "Invalid {.arg agg_fun}.",
    invalid_agg_i       = "Valid options: {valid}.",
    invalid_use_cache   = "{.arg use_cache} must be TRUE or FALSE.",
    invalid_cache_dir   = "{.arg cache_dir} must be a non-empty string.",
    invalid_parallel    = "{.arg parallel} must be TRUE or FALSE.",
    invalid_workers     = "{.arg workers} must be a positive integer.",
    download_start      = "Downloading {n_files} ERA5-Land file(s) from Zenodo...",
    cache_hit           = "Cache found: {filename}",
    download_file       = "Downloading: {filename}",
    download_done       = "Done: {filename}",
    download_error      = "Failed to download {filename}: {err}",
    muni_col            = "Municipality identifier column: {col}",
    agg_start           = "Aggregating to {n_mun} municipality/ies...",
    agg_done            = "Aggregation complete: {n_rows} observations ({n_mun} municipalities).",
    extract_warn        = "Could not process {file}.",
    no_data             = "No data was successfully extracted.",
    done_paths          = "{n} file(s) available in cache."
  ),
  es = list(
    missing_years       = "{.arg years} es obligatorio.",
    invalid_years_type  = "{.arg years} debe ser un vector num\u00e9rico sin NA.",
    invalid_years_range = "{.arg years} debe estar entre 1950 y 2025.",
    invalid_years_x     = "A\u00f1o(s) inv\u00e1lido(s): {bad}.",
    invalid_months      = "{.arg months} debe ser un vector entero entre 1 y 12.",
    invalid_vars_type   = "{.arg vars} debe ser un vector de caracteres.",
    invalid_vars        = "{.arg vars} contiene alias inv\u00e1lido(s): {bad}.\nUse: {valid}.",
    need_sf             = "El paquete {.pkg sf} es necesario para agregar por municipios.",
    municipalities_not_sf = "{.arg municipalities} debe ser un objeto {.cls sf}.",
    invalid_agg         = "{.arg agg_fun} inv\u00e1lido.",
    invalid_agg_i       = "Opciones v\u00e1lidas: {valid}.",
    invalid_use_cache   = "{.arg use_cache} debe ser TRUE o FALSE.",
    invalid_cache_dir   = "{.arg cache_dir} debe ser una cadena no vac\u00eda.",
    invalid_parallel    = "{.arg parallel} debe ser TRUE o FALSE.",
    invalid_workers     = "{.arg workers} debe ser un entero positivo.",
    download_start      = "Descargando {n_files} archivo(s) ERA5-Land de Zenodo...",
    cache_hit           = "Cach\u00e9 encontrado: {filename}",
    download_file       = "Descargando: {filename}",
    download_done       = "Completado: {filename}",
    download_error      = "Error al descargar {filename}: {err}",
    muni_col            = "Columna identificadora de municipios: {col}",
    agg_start           = "Agregando a {n_mun} municipio(s)...",
    agg_done            = "Agregaci\u00f3n completa: {n_rows} observaciones ({n_mun} municipios).",
    extract_warn        = "No se pudo procesar {file}.",
    no_data             = "No se extrajo ning\u00fan dato correctamente.",
    done_paths          = "{n} archivo(s) disponible(s) en cach\u00e9."
  )
)


# ==============================================================================
# INTERNAL HELPERS
# ==============================================================================

#' Build Zenodo filename for one indicator × year × month × aggregation
#' @keywords internal
#' @noRd
.era5_nc_filename <- function(indicator, year, month, agg) {
  last_day <- lubridate::days_in_month(
    as.Date(sprintf("%04d-%02d-01", as.integer(year), as.integer(month)))
  )
  sprintf(
    "%s_%04d-%02d-01_%04d-%02d-%02d_day_%s.nc",
    indicator,
    as.integer(year), as.integer(month),
    as.integer(year), as.integer(month), as.integer(last_day),
    agg
  )
}

#' Build Zenodo download URL for one file
#' @keywords internal
#' @noRd
.era5_zenodo_url <- function(year, filename) {
  record_id <- .era5_zenodo_ids[as.character(as.integer(year))]
  if (is.na(record_id)) {
    cli::cli_abort("No Zenodo record ID found for year {year}.")
  }
  sprintf(
    "https://zenodo.org/records/%d/files/%s?download=1",
    as.integer(record_id),
    filename
  )
}

#' Download one NetCDF file, using cache if available
#' @keywords internal
#' @noRd
.era5_download_file <- function(url, cache_path, use_cache, verbose, msg) {
  filename <- basename(cache_path)

  # Cache hit
  if (use_cache && file.exists(cache_path) && file.size(cache_path) > 0) {
    if (verbose) {
      cli::cli_alert_success(glue::glue(msg$cache_hit, filename = filename))
    }
    return(cache_path)
  }

  # Create directory if needed
  dir_path <- dirname(cache_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }

  if (verbose) {
    cli::cli_alert_info(glue::glue(msg$download_file, filename = filename))
  }

  # Download
  tryCatch({
    utils::download.file(
      url      = url,
      destfile = cache_path,
      mode     = "wb",
      quiet    = !verbose,
      method   = "auto"
    )
    if (verbose) {
      cli::cli_alert_success(glue::glue(msg$download_done, filename = filename))
    }
    cache_path
  }, error = function(e) {
    # Remove partial file on failure
    if (file.exists(cache_path)) unlink(cache_path)
    cli::cli_warn(glue::glue(msg$download_error,
                             filename = filename, err = conditionMessage(e)))
    NA_character_
  })
}

#' Extract raster layers from one monthly NetCDF and aggregate to municipalities
#'
#' Returns a data.frame with columns: code_muni, date, {out_col}
#' @keywords internal
#' @noRd
.era5_extract_monthly <- function(nc_path, municipalities, agg_fun,
                                   out_col, conv_fn, start_date) {
  # Load raster
  r <- terra::rast(nc_path)

  # Derive layer dates
  dates <- tryCatch({
    t <- terra::time(r)
    if (length(t) == terra::nlyr(r) && !all(is.na(t))) {
      as.Date(t)
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (is.null(dates)) {
    # Fallback: build date sequence from start_date
    dates <- seq(start_date, by = "day", length.out = terra::nlyr(r))
  }

  # Name layers with date strings for clean pivot (must be valid R names)
  names(r) <- format(dates, "d%Y%m%d")

  # Ensure municipalities CRS matches raster (ERA5-Land is WGS84)
  muni_crs <- sf::st_crs(municipalities)
  rast_crs <- terra::crs(r, describe = TRUE)
  if (!is.na(muni_crs) && !is.null(rast_crs) &&
      !isTRUE(sf::st_crs(municipalities) == sf::st_crs(4326))) {
    municipalities <- sf::st_transform(municipalities, crs = 4326)
  }

  # Area-weighted extraction: one row per municipality, one col per layer
  agg_df <- exactextractr::exact_extract(
    x        = r,
    y        = municipalities,
    fun      = agg_fun,
    progress = FALSE
  )

  # agg_df columns are named "{agg_fun}.d{YYYYMMDD}"
  # Add municipality identifier
  agg_df$code_muni <- municipalities$code_muni

  # Pivot to long: (code_muni, date, value)
  date_cols <- grep(paste0("^", agg_fun, "\\.d"), names(agg_df), value = TRUE)

  result_long <- tidyr::pivot_longer(
    agg_df,
    cols      = dplyr::all_of(date_cols),
    names_to  = "date_str",
    values_to = "value"
  )

  # Parse date from column name: remove "{agg_fun}.d" prefix
  prefix <- paste0(agg_fun, ".d")
  result_long$date <- as.Date(
    sub(paste0("^", prefix), "", result_long$date_str),
    format = "%Y%m%d"
  )
  result_long$date_str <- NULL

  # Apply unit conversion
  result_long$value <- conv_fn(result_long$value)

  # Rename value column
  names(result_long)[names(result_long) == "value"] <- out_col

  # Keep only needed columns
  result_long[, c("code_muni", "date", out_col)]
}

#' Detect the municipality identifier column in an sf object
#' @keywords internal
#' @noRd
.era5_detect_muni_col <- function(municipalities) {
  candidates <- c("code_muni", "CD_MUN", "CD_GEOCMU", "code_municipality")
  found <- intersect(candidates, names(municipalities))
  if (length(found) > 0) return(found[1])

  # Fallback: first character/numeric column with 6-7 digit values
  for (col in names(municipalities)) {
    vals <- as.character(municipalities[[col]])
    vals <- vals[!is.na(vals)]
    if (length(vals) > 0 && all(grepl("^\\d{6,7}$", vals[seq_len(min(5, length(vals)))]))) {
      return(col)
    }
  }

  cli::cli_abort(
    c(
      "Could not detect a municipality identifier column in {.arg municipalities}.",
      "i" = "Expected one of: {.val {paste(candidates, collapse = ', ')}}.",
      "i" = "Or a column with 6- or 7-digit numeric codes."
    )
  )
}

utils::globalVariables(c(
  "code_muni", "date", "value", "date_str", "alias", "indicator",
  "agg_label", "out_col", "filename", "url", "cache_path", "actual_path",
  "n_files", "n_mun", "n_rows", "bad", "valid", "col", "err",
  "bad_vars", "valid_var_aliases"
))
