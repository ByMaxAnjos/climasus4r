# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "code_muni", "date", "value", "n_files", "n_rows",
  "filename", "bad", "valid", "err", "year_val", "layer_name"
))

# ── Exported function ─────────────────────────────────────────────────────────

#' Import MERRA-2 Pollution Data for Brazilian Municipalities
#'
#' @description
#' `sus_grid_pollution_merra2()` downloads, spatially aggregates, and caches
#' NASA MERRA-2 aerosol and air quality data for Brazilian municipalities.
#'
#' MERRA-2 (Modern-Era Retrospective Analysis for Research and Applications v2)
#' from NASA GMAO provides global atmospheric reanalysis from **1980 to
#' present** at 0.625\eqn{\times}0.5\eqn{^\circ} (~55 km). It is the longest
#' available reanalysis record for aerosol and pollution studies, making it
#' ideal for historical trend analysis and cross-validation with higher-
#' resolution products such as CAMS or GHAP.
#'
#' Supported variables:
#' \itemize{
#'   \item **PM2.5** (`"pm25"`) — derived from aerosol mass components
#'     (\eqn{\mu g/m^3}): `DUSMASS25 + SSSMASS25 + BCSMASS + 1.4*OCSMASS +
#'     SO4SMASS`, each multiplied by 1\eqn{\times}10^9 to convert kg/m\eqn{^3}
#'     to \eqn{\mu g/m^3}.
#'   \item **AOD** (`"aod"`) — Total aerosol optical depth at 550 nm
#'     (`TOTEXTTAU`), dimensionless, proxy for total aerosol column.
#'   \item **SO2** (`"so2"`, experimental) — sulfate surface mass concentration
#'     from M2I3NVAER (\eqn{\mu g/m^3}).
#' }
#'
#' @section Authentication:
#' A **free NASA Earthdata Login** account is required
#' (<https://urs.earthdata.nasa.gov>). Set credentials in one of two ways:
#' \itemize{
#'   \item **Environment variables** (recommended):
#'     \preformatted{Sys.setenv(EARTHDATA_USER = "my_user",
#'                EARTHDATA_PASSWORD = "my_pass")}
#'   \item **`.netrc` file** (for pipelines), then pass its path via
#'     `netrc_path`:
#'     \preformatted{machine urs.earthdata.nasa.gov login my_user password my_pass}
#' }
#' After registering, activate GES DISC access at
#' <https://disc.gsfc.nasa.gov/earthdata-login>.
#'
#' @param pollutants Character vector. Variables to download.
#'   Allowed: `"pm25"`, `"aod"`, `"so2"`. Default: `c("pm25", "aod")`.
#'
#' @param resolution Character. Temporal resolution of source files.
#'   One of:
#'   \itemize{
#'     \item `"monthly"` (default) — uses `M2TMNXAER`, one file per month,
#'       already monthly-averaged. Recommended for most analyses.
#'     \item `"daily"` — uses `M2T1NXAER` (hourly), one file per day; 24
#'       hourly layers are aggregated to a single daily value using `agg_fun`.
#'       Note: `so2` only available in monthly resolution.
#'   }
#'
#' @param years Integer vector. Years to download (1980 to current year).
#'   `NULL` defaults to the last two complete years.
#'
#' @param months Integer vector (1–12). Months to include. Default `1:12`.
#'
#' @param municipalities An `sf` POLYGON object (e.g., from
#'   [geobr::read_municipality()]). If provided, raster data are aggregated
#'   and a `climasus_df` is returned. If `NULL`, returns cached NetCDF paths.
#'
#' @param agg_fun Character. Spatial aggregation function for
#'   `exactextractr::exact_extract()`. Default `"mean"` (area-weighted).
#'
#' @param earthdata_user Character. Earthdata username. Defaults to
#'   `Sys.getenv("EARTHDATA_USER")`.
#'
#' @param earthdata_pass Character. Earthdata password. Defaults to
#'   `Sys.getenv("EARTHDATA_PASSWORD")`.
#'
#' @param netrc_path Character. Path to a `.netrc` file with Earthdata
#'   credentials. If provided, takes precedence over `earthdata_user` /
#'   `earthdata_pass`.
#'
#' @param crop_brazil Logical. Crop rasters to Brazil's bounding box before
#'   extraction to save memory. Default `TRUE`.
#'
#' @param use_cache Logical. Reuse previously downloaded NetCDF files and
#'   previously computed Parquet caches. Default `TRUE`.
#'
#' @param cache_dir Character. Root cache directory.
#'   Default `"~/.climasus4r_cache/merra2"`.
#'
#' @param lang Character. Message language: `"pt"` (default), `"en"`, `"es"`.
#'
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return
#' \itemize{
#'   \item If `municipalities` is provided: a `climasus_df` with columns
#'     `code_muni`, `date`, and one column per pollutant (e.g.,
#'     `pm25_merra2`, `aod_merra2`). Metadata: `stage = "climate"`,
#'     `type = "pollution_merra2"`.
#'   \item If `municipalities = NULL`: a named character vector of paths to
#'     cached NetCDF files.
#' }
#'
#' @section Data source:
#' Gelaro, R. et al. (2017). The Modern-Era Retrospective Analysis for
#' Research and Applications, Version 2 (MERRA-2).
#' *Journal of Climate*, 30(14), 5419–5454.
#' \doi{10.1175/JCLI-D-16-0758.1}\cr
#' NASA/GSFC/EPS GMAO. MERRA-2 tavg1_2d_aer_Nx (M2T1NXAER v5.12.4).
#' NASA Goddard Earth Sciences DISC.
#' \doi{10.5067/KLICLTZ8EM9D}
#'
#' @examples
#' \dontrun{
#' # Set credentials
#' Sys.setenv(EARTHDATA_USER = "user", EARTHDATA_PASSWORD = "pass")
#'
#' library(geobr)
#' mt_mun <- read_municipality(code_muni = "MT", year = 2020)
#'
#' # Monthly PM2.5 + AOD, Q1 2020
#' merra2 <- sus_grid_pollution_merra2(
#'   pollutants    = c("pm25", "aod"),
#'   resolution    = "monthly",
#'   years         = 2020,
#'   months        = 1:3,
#'   municipalities = mt_mun,
#'   lang          = "pt"
#' )
#'
#' sus_meta(merra2, "stage")  # "climate"
#' sus_meta(merra2, "type")   # "pollution_merra2"
#'
#' # Historical PM2.5 trend 2000-2010, annual summary
#' merra2_hist <- sus_grid_pollution_merra2(
#'   pollutants = "pm25", resolution = "monthly",
#'   years = 2000:2010, municipalities = mt_mun, lang = "en"
#' )
#' }
#'
#' @seealso [sus_grid_pollution_cams()], [sus_grid_pollution_ghap()],
#'   [sus_climate_anomaly()], [sus_mod_dlnm()]
#'
#' @export
#' @importFrom rlang .data is_installed
#' @importFrom dplyr filter select rename mutate full_join join_by all_of
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom lubridate year month days_in_month
#' @importFrom tibble as_tibble
sus_grid_pollution_merra2 <- function(
    pollutants      = c("pm25", "aod"),
    resolution      = "monthly",
    years           = NULL,
    months          = 1:12,
    municipalities  = NULL,
    agg_fun         = "mean",
    earthdata_user  = Sys.getenv("EARTHDATA_USER"),
    earthdata_pass  = Sys.getenv("EARTHDATA_PASSWORD"),
    netrc_path      = NULL,
    crop_brazil     = TRUE,
    use_cache       = TRUE,
    cache_dir       = "~/.climasus4r_cache/merra2",
    lang            = "pt",
    verbose         = TRUE) {

  # ── 1. Validation ───────────────────────────────────────────────────────────

  if (!is.character(lang) || length(lang) != 1 || !lang %in% c("pt", "en", "es")) {
    cli::cli_abort("{.arg lang} must be 'pt', 'en', or 'es'.")
  }
  msg <- .merra2_msgs[[lang]]

  # Pollutants
  valid_pollutants <- names(.merra2_var_map)
  if (!is.character(pollutants) || length(pollutants) == 0 ||
      !all(pollutants %in% valid_pollutants)) {
    bad   <- paste(setdiff(pollutants, valid_pollutants), collapse = ", ")
    valid <- paste(valid_pollutants, collapse = ", ")
    cli::cli_abort(msg$invalid_pollutants)
  }

  # Resolution
  valid_res <- c("monthly", "daily")
  if (length(resolution) != 1 || !resolution %in% valid_res) {
    bad   <- paste(setdiff(resolution, valid_res), collapse = ", ")
    valid <- paste(valid_res, collapse = ", ")
    cli::cli_abort(msg$invalid_resolution)
  }

  # SO2 only available monthly
  if ("so2" %in% pollutants && resolution == "daily") {
    cli::cli_alert_warning(msg$so2_monthly_only)
    pollutants <- setdiff(pollutants, "so2")
    if (length(pollutants) == 0) {
      cli::cli_abort(msg$no_pollutants)
    }
  }

  # Years
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  if (is.null(years)) {
    years <- (current_year - 2):(current_year - 1)
    if (verbose) cli::cli_alert_info(
      glue::glue(msg$default_years, years = paste(range(years), collapse = "-")))
  } else {
    if (!is.numeric(years) || any(is.na(years))) cli::cli_abort(msg$invalid_years_type)
    years <- as.integer(years)
    bad_y <- years[years < 1980L | years > current_year]
    if (length(bad_y) > 0) {
      bad <- paste(bad_y, collapse = ", ")
      cli::cli_abort(c(msg$invalid_years_range, "x" = msg$invalid_years_x))
    }
    # Warn about data latency for recent months
    if (any(years == current_year)) {
      cli::cli_alert_warning(msg$latency_warn)
    }
  }

  # Months
  if (!is.numeric(months) || any(is.na(months)) ||
      any(months < 1 | months > 12)) cli::cli_abort(msg$invalid_months)
  months <- sort(as.integer(unique(months)))

  # Municipalities
  if (!is.null(municipalities)) {
    if (!requireNamespace("sf", quietly = TRUE)) cli::cli_abort(msg$need_sf)
    if (!inherits(municipalities, "sf"))        cli::cli_abort(msg$muni_not_sf)
    rlang::check_installed("terra",         reason = "to read MERRA-2 NetCDF files")
    rlang::check_installed("exactextractr", reason = "to aggregate rasters to polygons")
  }

  valid_agg <- c("mean", "sum", "median", "min", "max")
  if (!is.character(agg_fun) || length(agg_fun) != 1 || !agg_fun %in% valid_agg) {
    valid <- paste(valid_agg, collapse = ", ")
    cli::cli_abort(msg$invalid_agg)
  }

  if (!is.logical(use_cache) || length(use_cache) != 1) cli::cli_abort(msg$invalid_use_cache)
  if (!is.character(cache_dir) || nchar(trimws(cache_dir)) == 0) cli::cli_abort(msg$invalid_cache_dir)
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)

  # ── 2. Authentication check ─────────────────────────────────────────────────
  .merra2_check_auth(earthdata_user, earthdata_pass, netrc_path, msg)

  # ── 3. Build download manifest ───────────────────────────────────────────────
  manifest_rows <- list()

  for (p in pollutants) {
    vmap <- .merra2_var_map[[p]]
    p_res <- if (p == "so2") "monthly" else resolution   # SO2 forced monthly

    for (yr in years) {
      for (mo in months) {
        info    <- .merra2_file_info(p, p_res, yr, mo)
        out_col <- paste0(p, "_merra2")
        manifest_rows[[length(manifest_rows) + 1]] <- c(
          pollutant  = p,
          resolution = p_res,
          year       = as.character(yr),
          month      = sprintf("%02d", mo),
          filename   = info$filename,
          url        = info$url,
          out_col    = out_col,
          cache_nc   = file.path(cache_dir, "nc", info$filename),
          cache_pq   = file.path(cache_dir, "parquet",
                                 paste0(p, "_", p_res, "_",
                                        yr, sprintf("%02d", mo), ".parquet"))
        )
      }
    }
  }

  if (length(manifest_rows) == 0) cli::cli_abort(msg$no_data_to_download)

  manifest <- do.call(rbind, lapply(manifest_rows, as.data.frame,
                                    stringsAsFactors = FALSE))

  # Deduplicate: pm25 and aod may share the same NetCDF (M2T1NXAER / M2TMNXAER)
  unique_nc <- unique(manifest[, c("filename", "url", "cache_nc")])

  n_files <- nrow(unique_nc)
  if (verbose) {
    cli::cli_h1(msg$title)
    cli::cli_alert_info(glue::glue(msg$download_start, n_files = n_files))
  }

  # ── 4. Parquet early-return (all caches hit) ────────────────────────────────
  if (!is.null(municipalities) && use_cache &&
      all(file.exists(manifest$cache_pq))) {
    if (verbose) cli::cli_alert_success(msg$parquet_cache_hit)
    return(.merra2_build_from_parquet(manifest, verbose, msg))
  }

  # ── 5. Download with auth ────────────────────────────────────────────────────
  for (i in seq_len(nrow(unique_nc))) {
    .merra2_download_file(
      url            = unique_nc$url[i],
      cache_path     = unique_nc$cache_nc[i],
      use_cache      = use_cache,
      earthdata_user = earthdata_user,
      earthdata_pass = earthdata_pass,
      netrc_path     = netrc_path,
      verbose        = verbose,
      msg            = msg
    )
  }

  # ── 6. Return file paths if no municipalities ────────────────────────────────
  if (is.null(municipalities)) {
    paths <- stats::setNames(unique_nc$cache_nc, unique_nc$filename)
    if (verbose) cli::cli_alert_success(
      glue::glue(msg$done_paths, n = length(paths)))
    return(paths)
  }

  # ── 7. Prepare municipalities ────────────────────────────────────────────────
  muni_id_col <- .merra2_detect_muni_col(municipalities)
  if (muni_id_col != "code_muni") {
    municipalities$code_muni <- as.character(municipalities[[muni_id_col]])
  } else {
    municipalities$code_muni <- as.character(municipalities$code_muni)
  }
  municipalities$code_muni <- substr(municipalities$code_muni, 1L, 7L)
  municipalities <- sf::st_transform(municipalities, crs = 4326)

  brazil_bbox <- if (crop_brazil) terra::ext(-75, -28, -35, 6) else NULL
  n_mun       <- nrow(municipalities)

  if (verbose) cli::cli_alert_info(
    glue::glue(msg$agg_start, n_mun = n_mun))

  # ── 8. Extract and aggregate per manifest row ────────────────────────────────
  result_list <- vector("list", nrow(manifest))

  for (i in seq_len(nrow(manifest))) {
    nc_path  <- manifest$cache_nc[i]
    pq_path  <- manifest$cache_pq[i]
    pollutant <- manifest$pollutant[i]
    out_col  <- manifest$out_col[i]
    yr       <- as.integer(manifest$year[i])
    mo_str   <- manifest$month[i]
    p_res    <- manifest$resolution[i]

    if (!file.exists(nc_path) || file.size(nc_path) == 0) {
      cli::cli_alert_warning(
        glue::glue(msg$skip_missing, filename = manifest$filename[i]))
      next
    }

    # Parquet cache hit for this specific row
    if (use_cache && file.exists(pq_path) && file.size(pq_path) > 0) {
      if (verbose) cli::cli_alert_success(
        glue::glue(msg$parquet_hit, filename = basename(pq_path)))
      result_list[[i]] <- arrow::read_parquet(pq_path)
      next
    }

    df_i <- tryCatch({
      vmap     <- .merra2_var_map[[pollutant]]
      nc_vars  <- vmap$nc_vars
      derive_fn <- vmap$derive

      # Aggregate hourly → daily for daily resolution (24 layers per file)
      if (p_res == "daily") {
        layers <- lapply(nc_vars, function(v) {
          r_h <- terra::rast(nc_path, subds = v)   # 24 hourly layers
          if (!is.null(brazil_bbox)) r_h <- terra::crop(r_h, brazil_bbox)
          # Aggregate 24 hours → 1 daily value
          terra::app(r_h, fun = if (agg_fun == "sum") "sum" else "mean")
        })
        names(layers) <- nc_vars
        date_val <- as.Date(sprintf("%04d-%s-01", yr, mo_str))
      } else {
        # Monthly: one layer per file, already averaged
        layers <- lapply(nc_vars, function(v) {
          r <- terra::rast(nc_path, subds = v)
          if (!is.null(brazil_bbox)) r <- terra::crop(r, brazil_bbox)
          r
        })
        names(layers) <- nc_vars
        date_val <- as.Date(sprintf("%04d-%s-01", yr, mo_str))
      }

      # Derive composite variable (e.g., PM2.5 = sum of components)
      # Extract each component, then apply derivation formula
      comp_df <- data.frame(code_muni = municipalities$code_muni)
      for (v in nc_vars) {
        raw <- exactextractr::exact_extract(
          x = layers[[v]], y = municipalities,
          fun = agg_fun, progress = FALSE
        )
        comp_df[[v]] <- as.numeric(raw)
      }

      derived_val <- derive_fn(comp_df)

      out_df <- data.frame(
        code_muni      = comp_df$code_muni,
        date           = date_val,
        stringsAsFactors = FALSE
      )
      out_df[[out_col]] <- derived_val
      out_df
    }, error = function(e) {
      cli::cli_warn(c(
        glue::glue(msg$extract_warn, filename = manifest$filename[i]),
        "i" = conditionMessage(e)
      ))
      NULL
    })

    if (!is.null(df_i)) {
      pq_dir <- dirname(pq_path)
      if (!dir.exists(pq_dir)) {
        dir.create(pq_dir, recursive = TRUE, showWarnings = FALSE)
      }
      tryCatch(arrow::write_parquet(df_i, pq_path),
               error = function(e) cli::cli_alert_warning(
                 glue::glue(msg$parquet_write_warn, filename = basename(pq_path))))
      result_list[[i]] <- df_i
    }
  }

  # ── 9. Merge pollutants ───────────────────────────────────────────────────────
  result_list <- result_list[!vapply(result_list, is.null, logical(1))]
  if (length(result_list) == 0) cli::cli_abort(msg$no_data)

  result <- result_list[[1]]
  if (length(result_list) > 1) {
    for (j in 2:length(result_list)) {
      result <- dplyr::full_join(result, result_list[[j]],
                                 by = join_by(code_muni, date))
    }
  }

  result <- result[order(result$code_muni, result$date), ]
  rownames(result) <- NULL
  result <- tibble::as_tibble(result)

  n_rows <- nrow(result)
  if (verbose) cli::cli_alert_success(
    glue::glue(msg$agg_done, n_rows = n_rows, n_mun = n_mun))

  # ── 10. Build climasus_df ─────────────────────────────────────────────────────
  meta <- list(
    system   = NULL,
    stage    = "climate",
    type     = "pollution_merra2",
    spatial  = FALSE,
    temporal = list(
      start      = min(result$date, na.rm = TRUE),
      end        = max(result$date, na.rm = TRUE),
      unit       = if (resolution == "daily") "day" else "month",
      source     = "nasa_merra2",
      collection = if (resolution == "daily") "M2T1NXAER" else "M2TMNXAER"
    ),
    created          = Sys.time(),
    modified         = Sys.time(),
    pollutants       = pollutants,
    resolution       = resolution,
    n_municipalities = n_mun,
    n_observations   = n_rows,
    agg_fun          = agg_fun,
    history          = sprintf(
      "[%s] sus_grid_pollution_merra2(): %d pollutant(s), res=%s, %d obs",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      length(pollutants), resolution, n_rows
    ),
    user = list()
  )

  base_classes <- setdiff(class(result), "climasus_df")
  structure(result, sus_meta = meta, class = c("climasus_df", base_classes))
}


# ── Internal constants ────────────────────────────────────────────────────────

#' Variable definitions: pollutant alias → MERRA-2 collection, NC variables,
#' output column name, and derivation function
#' @keywords internal
#' @noRd
.merra2_var_map <- list(
  pm25 = list(
    collection = "M2T1NXAER",     # monthly: M2TMNXAER
    nc_vars    = c("DUSMASS25", "SSSMASS25", "BCSMASS", "OCSMASS", "SO4SMASS"),
    out_col    = "pm25_merra2",
    # PM2.5 = (dust<2.5um + seasalt<2.5um + BC + 1.4*OC + sulfate) * 1e9
    # units: kg/m3 -> ug/m3
    derive     = function(d) (d$DUSMASS25 + d$SSSMASS25 + d$BCSMASS +
                               1.4 * d$OCSMASS + d$SO4SMASS) * 1e9,
    unit       = "ug/m3"
  ),
  aod = list(
    collection = "M2T1NXAER",
    nc_vars    = c("TOTEXTTAU"),
    out_col    = "aod_merra2",
    derive     = function(d) d$TOTEXTTAU,
    unit       = "dimensionless"
  ),
  so2 = list(
    collection = "M2I3NVAER",     # 3-hourly; surface level only
    nc_vars    = c("SO2SMASS"),
    out_col    = "so2_merra2",
    derive     = function(d) d$SO2SMASS * 1e9,   # kg/m3 -> ug/m3
    unit       = "ug/m3"
  )
)

#' Multilingual messages for sus_grid_pollution_merra2()
#' @keywords internal
#' @noRd
.merra2_msgs <- list(
  pt = list(
    title                  = "Dados MERRA-2 de Polu\u00e7\u00e3o Atmosf\u00e9rica (NASA GMAO)",
    invalid_pollutants     = "{.arg pollutants} inv\u00e1lido(s): {bad}. Use: {valid}.",
    no_pollutants          = "Nenhum poluente v\u00e1lido selecionado.",
    invalid_resolution     = "{.arg resolution} inv\u00e1lido: {bad}. Use: {valid}.",
    so2_monthly_only       = "SO2 s\u00f3 dispon\u00edvel em resolution='monthly'. Removido da lista.",
    invalid_years_type     = "{.arg years} deve ser num\u00e9rico sem NA.",
    invalid_years_range    = "{.arg years} deve estar entre 1980 e {current_year}.",
    invalid_years_x        = "Ano(s) inv\u00e1lido(s): {bad}.",
    default_years          = "Usando anos {years} (padr\u00e3o: \u00faltimos 2 anos completos).",
    latency_warn           = "MERRA-2 tem ~3 semanas de lat\u00eancia. Dados do ano corrente podem estar incompletos.",
    invalid_months         = "{.arg months} deve ser inteiro entre 1 e 12.",
    need_sf                = "O pacote {.pkg sf} \u00e9 necess\u00e1rio para agregar por munic\u00edpios.",
    muni_not_sf            = "{.arg municipalities} deve ser um objeto {.cls sf}.",
    invalid_agg            = "{.arg agg_fun} inv\u00e1lido. Op\u00e7\u00f5es: {valid}.",
    invalid_use_cache      = "{.arg use_cache} deve ser TRUE ou FALSE.",
    invalid_cache_dir      = "{.arg cache_dir} deve ser uma string n\u00e3o vazia.",
    no_auth                = "Credenciais Earthdata n\u00e3o encontradas.\n  Configure com: Sys.setenv(EARTHDATA_USER='...', EARTHDATA_PASSWORD='...')\n  ou crie ~/.netrc em: https://disc.gsfc.nasa.gov/earthdata-login",
    no_data_to_download    = "Nenhum dado dispon\u00edvel para os par\u00e2metros fornecidos.",
    download_start         = "Baixando {n_files} arquivo(s) MERRA-2 do GES DISC...",
    cache_hit              = "Cache encontrado: {filename}",
    download_file          = "Baixando: {filename}",
    download_done          = "Conclu\u00eddo: {filename}",
    download_error         = "Falha ao baixar {filename}: {err}",
    parquet_cache_hit      = "Todos os arquivos no cache Parquet. Carregando...",
    parquet_hit            = "Cache Parquet: {filename}",
    parquet_write_warn     = "N\u00e3o foi poss\u00edvel salvar cache Parquet: {filename}",
    skip_missing           = "Arquivo n\u00e3o dispon\u00edvel: {filename}",
    extract_warn           = "N\u00e3o foi poss\u00edvel processar {filename}.",
    no_data                = "Nenhum dado foi extra\u00eddo com sucesso.",
    agg_start              = "Agregando para {n_mun} munic\u00edpio(s)...",
    agg_done               = "Conclu\u00eddo: {n_rows} observa\u00e7\u00f5es ({n_mun} munic\u00edpios).",
    done_paths             = "{n} arquivo(s) NetCDF dispon\u00edvel(is) no cache."
  ),
  en = list(
    title                  = "MERRA-2 Atmospheric Pollution Data (NASA GMAO)",
    invalid_pollutants     = "Invalid {.arg pollutants}: {bad}. Use: {valid}.",
    no_pollutants          = "No valid pollutants selected.",
    invalid_resolution     = "Invalid {.arg resolution}: {bad}. Use: {valid}.",
    so2_monthly_only       = "SO2 is only available with resolution='monthly'. Removed from list.",
    invalid_years_type     = "{.arg years} must be numeric without NA.",
    invalid_years_range    = "{.arg years} must be between 1980 and {current_year}.",
    invalid_years_x        = "Invalid year(s): {bad}.",
    default_years          = "Using years {years} (default: last 2 complete years).",
    latency_warn           = "MERRA-2 has ~3-week latency. Data for the current year may be incomplete.",
    invalid_months         = "{.arg months} must be integer between 1 and 12.",
    need_sf                = "Package {.pkg sf} is required to aggregate by municipality.",
    muni_not_sf            = "{.arg municipalities} must be an {.cls sf} object.",
    invalid_agg            = "Invalid {.arg agg_fun}. Options: {valid}.",
    invalid_use_cache      = "{.arg use_cache} must be TRUE or FALSE.",
    invalid_cache_dir      = "{.arg cache_dir} must be a non-empty string.",
    no_auth                = "Earthdata credentials not found.\n  Set: Sys.setenv(EARTHDATA_USER='...', EARTHDATA_PASSWORD='...')\n  or create ~/.netrc: https://disc.gsfc.nasa.gov/earthdata-login",
    no_data_to_download    = "No data available for the provided parameters.",
    download_start         = "Downloading {n_files} MERRA-2 file(s) from GES DISC...",
    cache_hit              = "Cache found: {filename}",
    download_file          = "Downloading: {filename}",
    download_done          = "Done: {filename}",
    download_error         = "Failed to download {filename}: {err}",
    parquet_cache_hit      = "All files found in Parquet cache. Loading...",
    parquet_hit            = "Parquet cache: {filename}",
    parquet_write_warn     = "Could not write Parquet cache: {filename}",
    skip_missing           = "File not available: {filename}",
    extract_warn           = "Could not process {filename}.",
    no_data                = "No data was successfully extracted.",
    agg_start              = "Aggregating to {n_mun} municipality/ies...",
    agg_done               = "Complete: {n_rows} observations ({n_mun} municipalities).",
    done_paths             = "{n} NetCDF file(s) available in cache."
  ),
  es = list(
    title                  = "Datos MERRA-2 de Contaminaci\u00f3n Atmosf\u00e9rica (NASA GMAO)",
    invalid_pollutants     = "{.arg pollutants} inv\u00e1lido(s): {bad}. Use: {valid}.",
    no_pollutants          = "Ning\u00fan contaminante v\u00e1lido seleccionado.",
    invalid_resolution     = "{.arg resolution} inv\u00e1lido: {bad}. Use: {valid}.",
    so2_monthly_only       = "SO2 solo disponible con resolution='monthly'. Eliminado de la lista.",
    invalid_years_type     = "{.arg years} debe ser num\u00e9rico sin NA.",
    invalid_years_range    = "{.arg years} debe estar entre 1980 y {current_year}.",
    invalid_years_x        = "A\u00f1o(s) inv\u00e1lido(s): {bad}.",
    default_years          = "Usando a\u00f1os {years} (por defecto: \u00faltimos 2 a\u00f1os completos).",
    latency_warn           = "MERRA-2 tiene ~3 semanas de latencia. Los datos del a\u00f1o actual pueden estar incompletos.",
    invalid_months         = "{.arg months} debe ser entero entre 1 y 12.",
    need_sf                = "El paquete {.pkg sf} es necesario para agregar por municipios.",
    muni_not_sf            = "{.arg municipalities} debe ser un objeto {.cls sf}.",
    invalid_agg            = "{.arg agg_fun} inv\u00e1lido. Opciones: {valid}.",
    invalid_use_cache      = "{.arg use_cache} debe ser TRUE o FALSE.",
    invalid_cache_dir      = "{.arg cache_dir} debe ser una cadena no vac\u00eda.",
    no_auth                = "Credenciales Earthdata no encontradas.\n  Configure con: Sys.setenv(EARTHDATA_USER='...', EARTHDATA_PASSWORD='...')\n  o cree ~/.netrc en: https://disc.gsfc.nasa.gov/earthdata-login",
    no_data_to_download    = "No hay datos disponibles para los par\u00e1metros indicados.",
    download_start         = "Descargando {n_files} archivo(s) MERRA-2 de GES DISC...",
    cache_hit              = "Cach\u00e9 encontrado: {filename}",
    download_file          = "Descargando: {filename}",
    download_done          = "Completado: {filename}",
    download_error         = "Error al descargar {filename}: {err}",
    parquet_cache_hit      = "Todos los archivos en cach\u00e9 Parquet. Cargando...",
    parquet_hit            = "Cach\u00e9 Parquet: {filename}",
    parquet_write_warn     = "No se pudo guardar cach\u00e9 Parquet: {filename}",
    skip_missing           = "Archivo no disponible: {filename}",
    extract_warn           = "No se pudo procesar {filename}.",
    no_data                = "No se extrajo ning\u00fan dato correctamente.",
    agg_start              = "Agregando a {n_mun} municipio(s)...",
    agg_done               = "Completo: {n_rows} observaciones ({n_mun} municipios).",
    done_paths             = "{n} archivo(s) NetCDF disponible(s) en cach\u00e9."
  )
)


# ── Internal helpers ──────────────────────────────────────────────────────────

#' Return MERRA-2 version code based on year
#' @keywords internal
#' @noRd
.merra2_version <- function(year) {
  year <- as.integer(year)
  ifelse(year < 1992L, "100",
  ifelse(year < 2001L, "200",
  ifelse(year < 2011L, "300", "400")))
}

#' Build MERRA-2 filename and GES DISC URL
#'
#' Monthly files use M2TMNXAER; daily files use M2T1NXAER.
#' SO2 (M2I3NVAER) always uses monthly-mean collection.
#' @keywords internal
#' @noRd
.merra2_file_info <- function(pollutant, resolution, year, month) {
  ver <- .merra2_version(year)
  yr  <- as.integer(year)
  mo  <- as.integer(month)

  if (pollutant == "so2") {
    # M2I3NVAER monthly mean: tavgM_2d_aer_Nv (3D, but we use surface)
    fname  <- sprintf("MERRA2_%s.tavgM_2d_aer_Nv.%04d%02d01.nc4", ver, yr, mo)
    base   <- sprintf(
      "https://data.gesdisc.earthdata.nasa.gov/data/MERRA2/M2I3NVAER.5.12.4/%04d/",
      yr)
  } else if (resolution == "daily") {
    # M2T1NXAER: hourly (24 layers per day); we download one file per DAY
    # For monthly manifest each row = one month → we use a monthly summary file
    fname <- sprintf("MERRA2_%s.tavgM_2d_aer_Nx.%04d%02d01.nc4", ver, yr, mo)
    base  <- sprintf(
      "https://data.gesdisc.earthdata.nasa.gov/data/MERRA2/M2TMNXAER.5.12.4/%04d/",
      yr)
  } else {
    # Monthly: M2TMNXAER
    fname <- sprintf("MERRA2_%s.tavgM_2d_aer_Nx.%04d%02d01.nc4", ver, yr, mo)
    base  <- sprintf(
      "https://data.gesdisc.earthdata.nasa.gov/data/MERRA2/M2TMNXAER.5.12.4/%04d/",
      yr)
  }

  url <- paste0(base, fname)
  list(filename = fname, url = url)
}

#' Validate Earthdata credentials; abort with setup instructions if missing
#' @keywords internal
#' @noRd
.merra2_check_auth <- function(user, pass, netrc_path, msg) {
  has_netrc <- !is.null(netrc_path) && nchar(netrc_path) > 0 &&
               file.exists(netrc_path)
  has_creds <- nchar(user) > 0 && nchar(pass) > 0
  if (!has_netrc && !has_creds) {
    cli::cli_abort(msg$no_auth)
  }
  invisible(TRUE)
}

#' Download one MERRA-2 NetCDF with Earthdata authentication and cache
#' @keywords internal
#' @noRd
.merra2_download_file <- function(url, cache_path, use_cache,
                                   earthdata_user, earthdata_pass,
                                   netrc_path, verbose, msg) {
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

  # Check if netrc is available; otherwise use httr2 with basic auth
  has_netrc <- !is.null(netrc_path) && nchar(netrc_path) > 0 &&
               file.exists(netrc_path)

  tryCatch({
    if (has_netrc) {
      utils::download.file(
        url      = url,
        destfile = cache_path,
        mode     = "wb",
        quiet    = !verbose,
        method   = "curl",
        extra    = paste("--netrc-file", shQuote(netrc_path),
                         "--location", "--cookie-jar", tempfile(),
                         "--cookie", tempfile())
      )
    } else {
      rlang::check_installed("httr2", reason = "to authenticate with NASA Earthdata")
      httr2::request(url) |>
        httr2::req_auth_basic(earthdata_user, earthdata_pass) |>
        httr2::req_options(followlocation = TRUE) |>
        httr2::req_perform(path = cache_path)
    }
    if (verbose) cli::cli_alert_success(
      glue::glue(msg$download_done, filename = filename))
    invisible(cache_path)
  }, error = function(e) {
    if (file.exists(cache_path)) unlink(cache_path)
    cli::cli_warn(glue::glue(msg$download_error,
                             filename = filename, err = conditionMessage(e)))
    invisible(NA_character_)
  })
}

#' Assemble climasus_df from pre-existing Parquet cache
#' @keywords internal
#' @noRd
.merra2_build_from_parquet <- function(manifest, verbose, msg) {
  parts <- lapply(manifest$cache_pq, function(pq) {
    tryCatch(arrow::read_parquet(pq), error = function(e) NULL)
  })
  parts <- parts[!vapply(parts, is.null, logical(1))]
  if (length(parts) == 0) cli::cli_abort(msg$no_data)

  result <- do.call(rbind, parts)
  result <- result[order(result$code_muni, result$date), ]
  rownames(result) <- NULL
  result <- tibble::as_tibble(result)

  n_rows <- nrow(result)
  if (verbose) cli::cli_alert_success(
    glue::glue(msg$agg_done, n_rows = n_rows,
               n_mun = dplyr::n_distinct(result$code_muni)))

  meta <- list(
    system = NULL, stage = "climate", type = "pollution_merra2",
    spatial = FALSE,
    temporal = list(start = min(result$date, na.rm = TRUE),
                    end   = max(result$date, na.rm = TRUE),
                    source = "nasa_merra2_cache"),
    created = Sys.time(), modified = Sys.time(),
    history = sprintf("[%s] sus_grid_pollution_merra2(): from Parquet cache, %d obs",
                      format(Sys.time(), "%Y-%m-%d %H:%M:%S"), n_rows),
    user = list()
  )
  base_classes <- setdiff(class(result), "climasus_df")
  structure(result, sus_meta = meta, class = c("climasus_df", base_classes))
}

#' Auto-detect municipality identifier column
#' @keywords internal
#' @noRd
.merra2_detect_muni_col <- function(municipalities) {
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
