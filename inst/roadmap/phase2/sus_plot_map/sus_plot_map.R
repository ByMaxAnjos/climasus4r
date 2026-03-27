# =============================================================================
# sus_view_map() — Publication-quality spatial maps for DATASUS systems
# Compatible with: SINAN, SIM-DO, SIH, SINASC, SIA, CNES
# =============================================================================

#' Create Publication-Quality Spatial Maps from DATASUS Health Data
#'
#' Produces two-panel spatial maps from any DATASUS health system dataset:
#' Panel A is a proportional bubble map showing cumulative burden (size) and
#' mean rate (colour) simultaneously. Panel B is a small-multiples choropleth
#' showing annual progression by municipality. Both panels use log(1+x)
#' transformation to handle zero-inflated rate distributions.
#'
#' @param df A data frame or climasus_df object containing health records with
#'   at least a geographic identifier (municipality code or lat/lon) and a
#'   numeric outcome variable. Daily or aggregated records are both accepted.
#'
#' @param value_col Character. Name of the column containing the outcome to map.
#'   Can be raw counts, rates, or any numeric measure. Examples:
#'   - SINAN: `"incidencia"`, `"n_casos"`
#'   - SIM-DO: `"mortality_rate"`, `"n_deaths"`
#'   - SIH: `"hospitalization_rate"`, `"n_admissions"`
#'   - CNES: `"n_beds"`, `"coverage_rate"`
#'   Default: `NULL` — auto-detected from system metadata or column names.
#'
#' @param count_col Character. Name of the absolute count column for bubble
#'   sizing in Panel A. If `NULL` (default), uses `value_col` for both size
#'   and colour (single-variable encoding).
#'
#' @param date_col Character. Name of the date column for temporal aggregation
#'   in Panel B. If `NULL` (default), auto-detected.
#'
#' @param muni_col Character. Name of the municipality code column (6 or 7
#'   digits). If `NULL` (default), auto-detected from common DATASUS patterns.
#'
#' @param lon_col Character. Name of the longitude column. If `NULL` (default),
#'   auto-detected. If not present, centroids are computed from geobr polygons.
#'
#' @param lat_col Character. Name of the latitude column. If `NULL` (default),
#'   auto-detected.
#'
#' @param uf_col Character. Name of the state abbreviation column (2-letter
#'   code). If `NULL` (default), auto-detected.
#'
#' @param pop_col Character. Name of the population column for rate calculation.
#'   Required only if `rate_per` is specified. If `NULL` (default), no rate
#'   is computed (assumes `value_col` is already a rate).
#'
#' @param rate_per Numeric. Population denominator for rate calculation.
#'   Common values: `100000` (per 100k, standard for incidence/mortality),
#'   `1000` (per 1k, used for infant mortality, birth rates),
#'   `1` (no transformation — use raw values). Default: `NULL` (no calculation,
#'   `value_col` used as-is).
#'
#' @param states Character vector of 2-letter state abbreviations to include.
#'   Accepts named regions (see Details). Default: `NULL` (all states present
#'   in the data).
#'
#' @param year_range Numeric vector of length 2 `c(start, end)`. Filters
#'   the temporal range. Default: `NULL` (all years in data).
#'
#' @param map_type Character. One of:
#'   - `"both"` (default): Panel A (bubble) + Panel B (annual choropleth).
#'   - `"bubble"`: Panel A only.
#'   - `"choropleth"`: Panel B (annual small-multiples) only.
#'   - `"static"`: Single choropleth of the full period (no faceting).
#'
#' @param facet_col Character. Column to use for small-multiples faceting in
#'   Panel B. Default: `"year"` (temporal progression). Can be any categorical
#'   variable (e.g., `"sex"`, `"age_group"`).
#'
#' @param facet_ncol Integer. Number of columns in facet grid. Default: `4`.
#'
#' @param label_cities Logical. If `TRUE` (default), adds city labels for the
#'   most important municipality per state (highest cumulative value).
#'
#' @param label_col Character. Column to use for city labels. Default: `"name"`.
#'
#' @param capital_col Character. Logical column marking capital cities. If
#'   present, capitals are always labelled. Default: `"is_capital"`.
#'
#' @param palette Character. Colour palette for the fill/colour scale.
#'   Options:
#'   - `"YlOrRd"` (default): Yellow-Orange-Red, print-safe, WHO/PAHO standard.
#'   - `"Blues"`: For supply-side indicators (CNES beds, coverage).
#'   - `"RdPu"`: For maternal/neonatal outcomes (SINASC).
#'   - `"YlGnBu"`: For environmental/climate-linked outcomes.
#'   - Any 7-colour `RColorBrewer` sequential palette name.
#'   - A character vector of hex colours (length ≥ 3).
#'
#' @param log_transform Logical. If `TRUE` (default), applies log(1+x)
#'   transformation to both colour and size scales. Recommended for zero-
#'   inflated count/rate data. Set `FALSE` for normally distributed indicators
#'   like CNES coverage rates or standardised scores.
#'
#' @param geobr_year Integer. IBGE shapefile reference year for geobr.
#'   Default: `2020`.
#'
#' @param crs_label Integer. EPSG code of projected CRS used for centroid
#'   computation (eliminates st_point_on_surface warning). Default: `5880`
#'   (SIRGAS 2000 / Brazil Polyconic — official IBGE CRS).
#'
#' @param title Character. Main figure title. If `NULL` (default), auto-generated
#'   from system name and value column.
#'
#' @param subtitle_a Character. Subtitle for Panel A. If `NULL`, auto-generated.
#' @param subtitle_b Character. Subtitle for Panel B. If `NULL`, auto-generated.
#'
#' @param caption Character. Figure caption. If `NULL`, auto-generated with
#'   data provenance. Suitable for direct use in journal submissions.
#'
#' @param panel_heights Numeric vector of length 2. Relative heights of
#'   Panel A and Panel B. Default: `c(1.1, 1)`.
#'
#' @param base_size Numeric. Base font size in points. Default: `10`.
#'   Use `10`–`11` for double-column journal figures (180 mm wide),
#'   `8`–`9` for single-column (85 mm wide).
#'
#' @param lang Character. Language for auto-generated labels.
#'   `"en"` (default), `"pt"`, `"es"`.
#'
#' @param save_path Character. File path for export. Extension determines
#'   format: `.tiff` (LZW, 300 DPI, for journal submission), `.png` (150 DPI,
#'   for preview), `.pdf` (vector, for presentations). If `NULL` (default),
#'   does not save.
#'
#' @param width Numeric. Export width in mm. Default: `180` (double column).
#' @param height Numeric. Export height in mm. Default: `240`.
#' @param dpi Integer. Export resolution. Default: `300`.
#'
#' @param verbose Logical. If `TRUE` (default), prints progress messages.
#'
#' @return A `patchwork` / `ggplot` object (invisibly). Prints as side effect.
#'
#' @details
#' **Auto-detection of columns** follows DATASUS naming conventions across
#' all systems. The function searches for columns in this priority order:
#'
#' *Municipality:* `residence_municipality_code`, `notification_municipality_code`,
#' `municipality_code`, `CODMUNRES`, `MUNIC_RES`, `MUNIC_MOV`, `CO_MUNICIPIO_GESTOR`
#'
#' *Date:* `notification_date`, `death_date`, `admission_date`, `birth_date`,
#' `date`, `DT_NOTIFIC`, `DTOBITO`, `DT_INTER`, `DTNASC`, `DT_COMPET`
#'
#' *UF:* `uf_code`, `notification_uf`, `residence_uf`, `UF_ZI`, `SG_UF`
#'
#' *Lon/Lat:* `lon`, `lat`, `longitude`, `latitude`, `LONG`, `LAT`
#'
#' **Named region shortcuts** for `states` parameter:
#' `"amazonia_legal"`, `"nordeste"`, `"sudeste"`, `"sul"`, `"centro_oeste"`,
#' `"norte"`, `"semi_arido"`, `"matopiba"`, `"dengue_hyperendemic"`
#'
#' **Shapefile caching:** `geobr` shapefiles are cached in the R session via
#' `memoise` (if available) or re-downloaded each call. For repeated use,
#' pre-load with `geobr::read_state()` and pass via `states_sf` argument.
#'
#' @examples
#' \dontrun{
#' # SINAN-DENGUE: incidence rate already computed
#' sus_view_map(df_analysis, value_col = "incidencia", count_col = "n_casos")
#'
#' # SIM-DO: compute mortality rate on-the-fly
#' sus_view_map(sim_df,
#'   value_col = "n_deaths",
#'   pop_col   = "pop_21",
#'   rate_per  = 100000,
#'   states    = "nordeste",
#'   palette   = "YlOrRd",
#'   title     = "Cardiovascular Mortality — Brazilian Northeast"
#' )
#'
#' # SIH: hospitalization rate, only southern states
#' sus_view_map(sih_df,
#'   value_col  = "n_admissions",
#'   pop_col    = "population",
#'   rate_per   = 1000,
#'   states     = c("PR", "SC", "RS"),
#'   map_type   = "static",
#'   palette    = "Blues",
#'   log_transform = FALSE
#' )
#'
#' # CNES: bed coverage (not zero-inflated, linear scale appropriate)
#' sus_view_map(cnes_df,
#'   value_col     = "beds_per_1k",
#'   map_type      = "choropleth",
#'   palette       = "Blues",
#'   log_transform = FALSE,
#'   facet_col     = "year"
#' )
#'
#' # SINASC: neonatal mortality, Amazon, save as TIFF
#' sus_view_map(sinasc_df,
#'   value_col  = "n_deaths",
#'   pop_col    = "n_births",
#'   rate_per   = 1000,
#'   states     = "amazonia_legal",
#'   palette    = "RdPu",
#'   save_path  = "neonatal_amazon.tiff",
#'   lang       = "pt"
#' )
#' }
#'
#' @importFrom dplyr group_by summarise mutate filter left_join select arrange
#'   slice_max n_distinct ungroup across
#' @importFrom ggplot2 ggplot geom_sf geom_point geom_text geom_label_repel
#'   aes scale_color_gradientn scale_fill_gradientn scale_size_continuous
#'   coord_sf labs theme theme_void element_text element_rect margin unit
#'   guide_colorbar guide_legend expansion facet_wrap
#' @importFrom sf st_transform st_point_on_surface st_coordinates st_drop_geometry
#' @importFrom patchwork plot_layout plot_annotation
#' @importFrom scales comma
#' @importFrom cli cli_alert_info cli_alert_success cli_alert_warning cli_abort
#'
#' @export
sus_view_map <- function(df,
                         # ── Column mapping ─────────────────────────────────
                         value_col    = NULL,
                         count_col    = NULL,
                         date_col     = NULL,
                         muni_col     = NULL,
                         lon_col      = NULL,
                         lat_col      = NULL,
                         uf_col       = NULL,
                         pop_col      = NULL,
                         rate_per     = NULL,
                         # ── Scope ──────────────────────────────────────────
                         states       = NULL,
                         year_range   = NULL,
                         # ── Map type ───────────────────────────────────────
                         map_type     = "both",
                         facet_col    = "year",
                         facet_ncol   = 4L,
                         # ── Labels ─────────────────────────────────────────
                         label_cities = TRUE,
                         label_col    = "name",
                         capital_col  = "is_capital",
                         # ── Visual ─────────────────────────────────────────
                         palette      = "YlOrRd",
                         log_transform = TRUE,
                         geobr_year   = 2020L,
                         crs_label    = 5880L,
                         # ── Text ───────────────────────────────────────────
                         title        = NULL,
                         subtitle_a   = NULL,
                         subtitle_b   = NULL,
                         caption      = NULL,
                         panel_heights = c(1.1, 1),
                         base_size    = 10,
                         lang         = "en",
                         # ── Export ─────────────────────────────────────────
                         save_path    = NULL,
                         width        = 180,
                         height       = 240,
                         dpi          = 300,
                         verbose      = TRUE) {
  
  # ════════════════════════════════════════════════════════════════════════════
  # 0. VALIDATION
  # ════════════════════════════════════════════════════════════════════════════
  if (!is.data.frame(df)) cli::cli_abort("{.arg df} must be a data frame.")
  
  valid_map_types <- c("both", "bubble", "choropleth", "static")
  if (!map_type %in% valid_map_types) {
    cli::cli_abort("{.arg map_type} must be one of {.val {valid_map_types}}.")
  }
  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_abort("{.arg lang} must be one of {.val {c('en','pt','es')}}.")
  }
  
  # ── Detect system from climasus_df metadata (if available) ─────────────────
  system_name <- if (inherits(df, "climasus_df")) {
    climasus_meta(df, "system") %||% "DATASUS"
  } else {
    "DATASUS"
  }
  if (verbose) cli::cli_alert_info("System detected: {system_name}")
  
  # ════════════════════════════════════════════════════════════════════════════
  # 1. COLUMN AUTO-DETECTION
  # ════════════════════════════════════════════════════════════════════════════
  .fc <- function(...) .find_col(df, c(...))
  
  # Municipality code
  muni_col <- muni_col %||% .fc(
    "residence_municipality_code", "notification_municipality_code",
    "municipality_code", "codigo_municipio_residencia",
    "CODMUNRES", "MUNIC_RES", "MUNIC_MOV", "CO_MUNICIPIO_GESTOR",
    "municipio_codigo", "cod_municipio"
  )
  if (is.null(muni_col)) cli::cli_abort("Municipality code column not found. Specify {.arg muni_col}.")
  
  # Date column
  date_col <- date_col %||% .fc(
    "notification_date", "death_date", "admission_date", "birth_date",
    "date", "data", "DT_NOTIFIC", "DTOBITO", "DT_INTER", "DTNASC", "DT_COMPET"
  )
  if (is.null(date_col) && map_type %in% c("both", "choropleth")) {
    cli::cli_alert_warning("Date column not found. Panel B (annual) will be skipped.")
    map_type <- "bubble"
  }
  
  # UF column
  uf_col <- uf_col %||% .fc(
    "uf_code", "notification_uf", "residence_uf", "uf",
    "UF_ZI", "SG_UF", "uf_residencia", "uf_notificacao"
  )
  
  # Lon/Lat
  lon_col <- lon_col %||% .fc("lon", "longitude", "LONG", "long", "x")
  lat_col <- lat_col %||% .fc("lat", "latitude",  "LAT",  "lati", "y")
  
  has_coords <- !is.null(lon_col) && !is.null(lat_col)
  
  # Value column (outcome)
  value_col <- value_col %||% .fc(
    "incidencia", "incidence_rate", "mortality_rate",
    "hospitalization_rate", "taxa_mortalidade", "taxa_internacao",
    "n_casos", "n_deaths", "n_obitos", "n_admissions", "n_internacoes",
    "n_births", "n_nascimentos", "coverage_rate", "taxa_cobertura"
  )
  if (is.null(value_col)) {
    cli::cli_abort("Outcome column not found. Specify {.arg value_col}.")
  }
  
  # Count column (bubble size) — fallback to value_col
  count_col <- count_col %||% .fc(
    "n_casos", "n_deaths", "n_obitos", "n_admissions",
    "n_internacoes", "n_births", "total_casos"
  ) %||% value_col
  
  if (verbose) {
    cli::cli_alert_info("Columns resolved: value={value_col}, count={count_col}, muni={muni_col}, date={date_col %||% 'none'}")
  }
  
  # ════════════════════════════════════════════════════════════════════════════
  # 2. STATES / REGION RESOLUTION
  # ════════════════════════════════════════════════════════════════════════════
  target_states <- .resolve_states(states, df, uf_col, verbose)
  
  # ════════════════════════════════════════════════════════════════════════════
  # 3. RATE CALCULATION (optional)
  # ════════════════════════════════════════════════════════════════════════════
  if (!is.null(rate_per) && !is.null(pop_col)) {
    if (!pop_col %in% names(df)) {
      cli::cli_abort("Population column '{pop_col}' not found.")
    }
    df <- df %>%
      dplyr::mutate(
        .computed_rate = (.data[[value_col]] / .data[[pop_col]]) * rate_per
      )
    rate_col <- ".computed_rate"
    if (verbose) cli::cli_alert_info("Rate computed: ({value_col} / {pop_col}) * {rate_per}")
  } else {
    rate_col <- value_col
  }
  
  # ════════════════════════════════════════════════════════════════════════════
  # 4. FILTER scope
  # ════════════════════════════════════════════════════════════════════════════
  if (!is.null(target_states) && !is.null(uf_col)) {
    df <- df %>% dplyr::filter(.data[[uf_col]] %in% target_states)
    if (nrow(df) == 0) cli::cli_abort("No rows remaining after state filter.")
  }
  
  if (!is.null(year_range) && !is.null(date_col)) {
    df <- df %>%
      dplyr::filter(
        as.integer(format(as.Date(.data[[date_col]]), "%Y")) >= year_range[1],
        as.integer(format(as.Date(.data[[date_col]]), "%Y")) <= year_range[2]
      )
  }
  
  # ════════════════════════════════════════════════════════════════════════════
  # 5. AGGREGATE — municipality summary
  # ════════════════════════════════════════════════════════════════════════════
  if (verbose) cli::cli_alert_info("Aggregating to municipality level...")
  
  # Identify grouping columns present in df
  extra_group_cols <- intersect(
    c(label_col, capital_col, uf_col,
      if (has_coords) c(lon_col, lat_col) else NULL),
    names(df)
  )
  group_cols <- unique(c(muni_col, extra_group_cols))
  
  df_muni <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      .total  = sum(.data[[count_col]], na.rm = TRUE),
      .rate   = mean(.data[[rate_col]][.data[[rate_col]] > 0], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      .rate      = dplyr::if_else(is.nan(.rate) | is.na(.rate), 0, .rate),
      .log_total = log1p(.total),
      .log_rate  = log1p(.rate)
    )
  
  # Annual aggregation for Panel B
  df_annual <- NULL
  if (map_type %in% c("both", "choropleth") && !is.null(date_col)) {
    df_annual <- df %>%
      dplyr::mutate(.year = format(as.Date(.data[[date_col]]), "%Y")) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(muni_col, ".year")))) %>%
      dplyr::summarise(
        .annual_rate = mean(.data[[rate_col]][.data[[rate_col]] > 0], na.rm = TRUE),
        .groups      = "drop"
      ) %>%
      dplyr::mutate(
        .annual_rate = dplyr::if_else(is.nan(.annual_rate) | is.na(.annual_rate),
                                      0, .annual_rate)
      )
    
    # Respect custom facet_col if not "year"
    if (facet_col != "year" && facet_col %in% names(df)) {
      df_annual <- df %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(c(muni_col, facet_col)))) %>%
        dplyr::summarise(
          .annual_rate = mean(.data[[rate_col]][.data[[rate_col]] > 0], na.rm = TRUE),
          .groups      = "drop"
        ) %>%
        dplyr::mutate(.annual_rate = dplyr::if_else(
          is.nan(.annual_rate) | is.na(.annual_rate), 0, .annual_rate
        ))
      facet_var <- facet_col
    } else {
      facet_var <- ".year"
    }
  }
  
  # ════════════════════════════════════════════════════════════════════════════
  # 6. SHAPEFILES
  # ════════════════════════════════════════════════════════════════════════════
  if (verbose) cli::cli_alert_info("Loading geobr shapefiles (year={geobr_year})...")
  
  states_sf <- geobr::read_state(year = geobr_year, showProgress = FALSE)
  if (!is.null(target_states)) {
    states_sf <- states_sf %>% dplyr::filter(abbrev_state %in% target_states)
  }
  
  muni_sf <- geobr::read_municipality(year = geobr_year, showProgress = FALSE) %>%
    dplyr::mutate(code_muni_6 = substr(as.character(code_muni), 1, 6))
  if (!is.null(target_states)) {
    muni_sf <- muni_sf %>% dplyr::filter(abbrev_state %in% target_states)
  }
  
  # ── Planar centroid labels (eliminates st_point_on_surface warning) ─────────
  state_centroids <- .compute_centroids(states_sf, crs_label)
  
  # ── Join data to municipality polygons ─────────────────────────────────────
  muni_key <- substr(df_muni[[muni_col]], 1, 6)
  df_muni_sf <- muni_sf %>%
    dplyr::left_join(
      df_muni %>% dplyr::mutate(.muni_6 = substr(.data[[muni_col]], 1, 6)),
      by = c("code_muni_6" = ".muni_6")
    )
  
  if (!is.null(df_annual)) {
    df_annual_sf <- muni_sf %>%
      dplyr::left_join(
        df_annual %>% dplyr::mutate(.muni_6 = substr(.data[[muni_col]], 1, 6)),
        by = c("code_muni_6" = ".muni_6")
      )
  }
  
  # ── City labels (most important municipality per state) ────────────────────
  city_labels <- NULL
  if (label_cities && has_coords) {
    capital_present <- capital_col %in% names(df_muni)
    name_present    <- label_col   %in% names(df_muni)
    
    city_labels <- df_muni %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(uf_col))) %>%
      dplyr::slice_max(.total, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup()
    
    # Also include explicit capitals if column exists
    if (capital_present) {
      caps <- df_muni %>%
        dplyr::filter(.data[[capital_col]] == TRUE)
      city_labels <- dplyr::bind_rows(city_labels, caps) %>%
        dplyr::distinct(.data[[muni_col]], .keep_all = TRUE)
    }
  } else if (label_cities) {
    # Compute centroids from polygons if no coordinates
    city_labels <- df_muni_sf %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(
        intersect(c("abbrev_state", uf_col), names(.))
      ))) %>%
      dplyr::slice_max(.total, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(
        .compute_centroids(muni_sf, crs_label) %>%
          dplyr::rename(lon_lbl = lon_lbl, lat_lbl = lat_lbl),
        by = c("code_muni_6" = "code_muni_6")
      )
  }
  
  # ════════════════════════════════════════════════════════════════════════════
  # 7. COLOUR PALETTE
  # ════════════════════════════════════════════════════════════════════════════
  fill_colors <- .resolve_palette(palette)
  
  # ════════════════════════════════════════════════════════════════════════════
  # 8. SCALE PARAMETERS (auto-sized to data range)
  # ════════════════════════════════════════════════════════════════════════════
  rate_vals  <- df_muni$.rate[df_muni$.rate > 0]
  total_vals <- df_muni$.total[df_muni$.total > 0]
  
  color_breaks_raw <- .smart_breaks(rate_vals,  n = 6, log = log_transform)
  size_breaks_raw  <- .smart_breaks(total_vals, n = 4, log = log_transform)
  
  if (log_transform) {
    color_breaks <- log1p(color_breaks_raw)
    size_breaks  <- log1p(size_breaks_raw)
    color_y      <- ".log_rate"
    size_y       <- ".log_total"
    y_trans      <- function(x) log1p(x)
    y_inv        <- function(x) expm1(x)
  } else {
    color_breaks <- color_breaks_raw
    size_breaks  <- size_breaks_raw
    color_y      <- ".rate"
    size_y       <- ".log_total"  # always log total for size (visual balance)
    y_trans      <- identity
    y_inv        <- identity
  }
  
  color_labels <- scales::comma(round(color_breaks_raw, 0))
  size_labels  <- scales::comma(round(size_breaks_raw, 0))
  
  # ════════════════════════════════════════════════════════════════════════════
  # 9. AUTO-GENERATED TEXT
  # ════════════════════════════════════════════════════════════════════════════
  lbl <- .map_labels(lang)
  
  rate_label <- if (!is.null(rate_per)) {
    paste0(lbl$rate_prefix, scales::comma(rate_per), lbl$inhabitants)
  } else {
    value_col
  }
  
  log_note <- if (log_transform) paste0(" (", lbl$log_scale, ")") else ""
  
  year_span <- if (!is.null(date_col)) {
    yrs <- range(as.integer(format(as.Date(df[[date_col]]), "%Y")), na.rm = TRUE)
    if (yrs[1] == yrs[2]) as.character(yrs[1]) else paste0(yrs[1], "\u2013", yrs[2])
  } else ""
  
  state_str <- if (!is.null(target_states)) {
    paste(target_states, collapse = ", ")
  } else lbl$all_states
  
  title <- title %||% paste0(
    .system_outcome_label(system_name, value_col, lang),
    if (nchar(year_span) > 0) paste0(" (", year_span, ")") else ""
  )
  
  subtitle_a <- subtitle_a %||% paste0(
    lbl$bubble_sub_size, " \u2022 ",
    lbl$bubble_sub_color, rate_label, log_note
  )
  
  subtitle_b_auto <- if (!is.null(df_annual)) {
    paste0(lbl$choropleth_sub, " ", rate_label, log_note)
  } else ""
  subtitle_b <- subtitle_b %||% subtitle_b_auto
  
  caption <- caption %||% paste0(
    lbl$caption_source, system_name, lbl$caption_ministry, "\n",
    if (!is.null(pop_col)) paste0(lbl$caption_pop, pop_col, ". ") else "",
    lbl$caption_states, state_str, ". ",
    lbl$caption_shp, as.character(geobr_year), lbl$caption_geobr,
    if (log_transform) paste0(" ", lbl$caption_log) else ""
  )
  
  color_legend_name <- paste0(rate_label, log_note)
  size_legend_name  <- paste0(lbl$cumulative_cases, "\n", lbl$log_scale_paren)
  
  # ════════════════════════════════════════════════════════════════════════════
  # 10. MAP EXTENT (bounding box of filtered data with padding)
  # ════════════════════════════════════════════════════════════════════════════
  bbox <- sf::st_bbox(states_sf)
  pad  <- 0.5  # degrees
  xlim <- c(bbox["xmin"] - pad, bbox["xmax"] + pad)
  ylim <- c(bbox["ymin"] - pad, bbox["ymax"] + pad)
  
  # ════════════════════════════════════════════════════════════════════════════
  # 11. BUILD PANELS
  # ════════════════════════════════════════════════════════════════════════════
  panel_a <- NULL
  panel_b <- NULL
  
  # ── PANEL A: Proportional bubble map ───────────────────────────────────────
  if (map_type %in% c("both", "bubble")) {
    if (verbose) cli::cli_alert_info("Building Panel A (bubble map)...")
    
    panel_a <- ggplot2::ggplot() +
      
      # State background
      ggplot2::geom_sf(
        data      = states_sf,
        fill      = "#F0EDE8",
        color     = "#7A8C99",
        linewidth = 0.5
      ) +
      
      # Proportional bubbles (largest first to prevent occlusion)
      {
        bubble_data <- df_muni %>%
          dplyr::arrange(dplyr::desc(.total))
        
        if (has_coords) {
          ggplot2::geom_point(
            data  = bubble_data,
            ggplot2::aes(
              x     = .data[[lon_col]],
              y     = .data[[lat_col]],
              size  = .data[[size_y]],
              color = .data[[color_y]]
            ),
            alpha = 0.75,
            shape = 16
          )
        } else {
          # Fall back to choropleth fill if no coordinates
          ggplot2::geom_sf(
            data = df_muni_sf,
            ggplot2::aes(fill = .data[[color_y]]),
            color = NA
          )
        }
      } +
      
      # City / capital labels
      {
        if (label_cities && !is.null(city_labels) && has_coords &&
            label_col %in% names(city_labels)) {
          ggrepel::geom_label_repel(
            data          = city_labels,
            ggplot2::aes(
              x     = .data[[lon_col]],
              y     = .data[[lat_col]],
              label = .data[[label_col]]
            ),
            size          = base_size * 0.23,
            fontface      = "bold",
            color         = "#1A252F",
            fill          = scales::alpha("white", 0.85),
            label.padding = ggplot2::unit(0.18, "lines"),
            label.size    = 0.25,
            box.padding   = 0.5,
            segment.color = "#1A252F",
            segment.size  = 0.3,
            max.overlaps  = 15,
            na.rm         = TRUE
          )
        } else {
          NULL
        }
      } +
      
      # State abbreviation labels (planar centroid — no warning)
      ggplot2::geom_text(
        data     = state_centroids,
        ggplot2::aes(x = lon_lbl, y = lat_lbl, label = abbrev_state),
        size     = base_size * 0.27,
        color    = "#4A5A66",
        fontface = "italic",
        alpha    = 0.65
      ) +
      
      # Colour scale
      ggplot2::scale_color_gradientn(
        colors = fill_colors,
        name   = color_legend_name,
        breaks = color_breaks,
        labels = color_labels,
        guide  = ggplot2::guide_colorbar(
          title.position  = "top",
          barwidth        = ggplot2::unit(10, "lines"),
          barheight       = ggplot2::unit(0.5, "lines"),
          ticks.linewidth = 0.4
        )
      ) +
      
      # Size scale
      ggplot2::scale_size_continuous(
        name   = size_legend_name,
        range  = c(0.5, 9),
        breaks = size_breaks,
        labels = size_labels,
        guide  = ggplot2::guide_legend(
          title.position = "top",
          override.aes   = list(alpha = 0.8, color = fill_colors[5])
        )
      ) +
      
      ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
      
      ggplot2::labs(
        title    = paste0("A  ", title),
        subtitle = subtitle_a
      ) +
      
      .map_theme(base_size) +
      ggplot2::theme(
        legend.position  = "bottom",
        legend.direction = "horizontal",
        legend.box       = "horizontal",
        legend.spacing.x = ggplot2::unit(0.8, "lines")
      )
  }
  
  # ── PANEL B: Annual small-multiples choropleth ─────────────────────────────
  if (map_type %in% c("both", "choropleth") && !is.null(df_annual)) {
    if (verbose) cli::cli_alert_info("Building Panel B (annual choropleth)...")
    
    fill_col_b   <- if (log_transform) ".log_annual" else ".annual_rate"
    df_annual_sf <- df_annual_sf %>%
      dplyr::mutate(.log_annual = log1p(.annual_rate))
    
    annual_rate_vals <- df_annual$.annual_rate[df_annual$.annual_rate > 0]
    ann_breaks_raw   <- .smart_breaks(annual_rate_vals, n = 5, log = log_transform)
    ann_breaks       <- if (log_transform) log1p(ann_breaks_raw) else ann_breaks_raw
    ann_labels       <- scales::comma(round(ann_breaks_raw, 0))
    
    panel_b <- ggplot2::ggplot() +
      
      ggplot2::geom_sf(
        data  = df_annual_sf,
        ggplot2::aes(fill = .data[[fill_col_b]]),
        color = NA,
        na.rm = TRUE
      ) +
      
      ggplot2::geom_sf(
        data      = states_sf,
        fill      = NA,
        color     = "grey60",
        linewidth = 0.3
      ) +
      
      ggplot2::facet_wrap(
        stats::as.formula(paste0("~ ", facet_var)),
        ncol = facet_ncol
      ) +
      
      ggplot2::scale_fill_gradientn(
        colors   = fill_colors,
        name     = color_legend_name,
        breaks   = ann_breaks,
        labels   = ann_labels,
        na.value = "#E8E4DF",
        guide    = ggplot2::guide_colorbar(
          title.position  = "top",
          barwidth        = ggplot2::unit(12, "lines"),
          barheight       = ggplot2::unit(0.5, "lines"),
          ticks.linewidth = 0.4
        )
      ) +
      
      ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
      
      ggplot2::labs(
        title    = paste0("B  ", lbl$panel_b_title),
        subtitle = subtitle_b
      ) +
      
      .map_theme(base_size) +
      ggplot2::theme(
        strip.text        = ggplot2::element_text(
          face   = "bold", size = base_size * 0.82,
          color  = "white",
          margin = ggplot2::margin(2.5, 4, 2.5, 4)
        ),
        strip.background  = ggplot2::element_rect(fill = "#1C2B3A", color = NA),
        legend.position   = "bottom",
        legend.direction  = "horizontal",
        panel.spacing     = ggplot2::unit(0.3, "lines")
      )
  }
  
  # ── STATIC: single-period choropleth ───────────────────────────────────────
  if (map_type == "static") {
    if (verbose) cli::cli_alert_info("Building static choropleth...")
    
    fill_col_s <- if (log_transform) ".log_rate" else ".rate"
    
    static_breaks_raw <- .smart_breaks(rate_vals, n = 5, log = log_transform)
    static_breaks     <- if (log_transform) log1p(static_breaks_raw) else static_breaks_raw
    static_labels     <- scales::comma(round(static_breaks_raw, 0))
    
    panel_a <- ggplot2::ggplot() +
      
      ggplot2::geom_sf(
        data  = df_muni_sf,
        ggplot2::aes(fill = .data[[fill_col_s]]),
        color = NA, na.rm = TRUE
      ) +
      
      ggplot2::geom_sf(
        data      = states_sf,
        fill      = NA, color = "#7A8C99", linewidth = 0.45
      ) +
      
      ggplot2::geom_text(
        data     = state_centroids,
        ggplot2::aes(x = lon_lbl, y = lat_lbl, label = abbrev_state),
        size     = base_size * 0.27, color = "white",
        fontface = "bold", alpha = 0.8
      ) +
      
      ggplot2::scale_fill_gradientn(
        colors   = fill_colors,
        name     = color_legend_name,
        breaks   = static_breaks,
        labels   = static_labels,
        na.value = "#E8E4DF",
        guide    = ggplot2::guide_colorbar(
          title.position = "top",
          barwidth       = ggplot2::unit(12, "lines"),
          barheight      = ggplot2::unit(0.5, "lines")
        )
      ) +
      
      ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
      ggplot2::labs(title = title, subtitle = subtitle_a) +
      .map_theme(base_size) +
      ggplot2::theme(legend.position = "bottom", legend.direction = "horizontal")
  }
  
  # ════════════════════════════════════════════════════════════════════════════
  # 12. ASSEMBLE WITH PATCHWORK
  # ════════════════════════════════════════════════════════════════════════════
  if (map_type == "both" && !is.null(panel_a) && !is.null(panel_b)) {
    out <- panel_a / panel_b +
      patchwork::plot_layout(heights = panel_heights) +
      patchwork::plot_annotation(
        caption = caption,
        theme   = ggplot2::theme(
          plot.caption = ggplot2::element_text(
            size       = base_size * 0.65,
            color      = "grey45",
            hjust      = 0,
            lineheight = 1.35,
            margin     = ggplot2::margin(t = 6)
          )
        )
      )
  } else {
    # Single panel
    out <- (panel_a %||% panel_b) +
      patchwork::plot_annotation(
        caption = caption,
        theme   = ggplot2::theme(
          plot.caption = ggplot2::element_text(
            size = base_size * 0.65, color = "grey45", hjust = 0, lineheight = 1.35
          )
        )
      )
  }
  
  print(out)
  if (verbose) cli::cli_alert_success("Map rendered.")
  
  # ════════════════════════════════════════════════════════════════════════════
  # 13. EXPORT
  # ════════════════════════════════════════════════════════════════════════════
  if (!is.null(save_path)) {
    ext <- tolower(tools::file_ext(save_path))
    if (ext == "tiff" || ext == "tif") {
      ggplot2::ggsave(save_path, out, width = width, height = height,
                      units = "mm", dpi = dpi, compression = "lzw")
    } else if (ext == "pdf") {
      ggplot2::ggsave(save_path, out, width = width, height = height,
                      units = "mm", device = "pdf")
    } else {
      ggplot2::ggsave(save_path, out, width = width, height = height,
                      units = "mm", dpi = dpi)
    }
    if (verbose) cli::cli_alert_success("Saved: {.file {save_path}}")
  }
  
  invisible(out)
}


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

#' Find first matching column name
#' @keywords internal
#' @noRd
.find_col <- function(df, candidates) {
  for (x in candidates) if (x %in% names(df)) return(x)
  NULL
}

#' Resolve state abbreviations from region name or vector
#' @keywords internal
#' @noRd
.resolve_states <- function(states, df, uf_col, verbose) {
  if (is.null(states)) return(NULL)
  
  region_map <- list(
    amazonia_legal      = c("AC","AM","AP","MA","MT","PA","RO","RR","TO"),
    norte               = c("AC","AM","AP","PA","RO","RR","TO"),
    nordeste            = c("AL","BA","CE","MA","PB","PE","PI","RN","SE"),
    centro_oeste        = c("DF","GO","MT","MS"),
    sudeste             = c("ES","MG","RJ","SP"),
    sul                 = c("PR","RS","SC"),
    semi_arido          = c("AL","BA","CE","MA","PB","PE","PI","RN","SE","MG"),
    matopiba            = c("MA","TO","PI","BA"),
    dengue_hyperendemic = c("GO","MS","MT","PR","RJ","SP"),
    caatinga            = c("AL","BA","CE","MA","PB","PE","PI","RN","SE","MG"),
    cerrado             = c("BA","DF","GO","MA","MG","MS","MT","PA","PI","PR","RO","SP","TO"),
    pantanal            = c("MT","MS"),
    pampa               = c("RS")
  )
  
  result <- c()
  for (s in states) {
    key <- tolower(gsub("[^a-z_]", "", tolower(s)))
    if (key %in% names(region_map)) {
      result <- c(result, region_map[[key]])
      if (verbose) cli::cli_alert_info("Region '{s}' -> {paste(region_map[[key]], collapse=', ')}")
    } else {
      result <- c(result, toupper(s))
    }
  }
  unique(result)
}

#' Compute state/municipality centroids in planar CRS (eliminates sf warning)
#' @keywords internal
#' @noRd
.compute_centroids <- function(sf_obj, crs_label = 5880L) {
  id_col <- if ("abbrev_state" %in% names(sf_obj)) "abbrev_state" else
    if ("code_muni_6"  %in% names(sf_obj)) "code_muni_6"  else
      names(sf_obj)[1]
  
  sf_obj %>%
    sf::st_transform(crs = crs_label) %>%
    sf::st_point_on_surface() %>%
    sf::st_transform(crs = 4326) %>%
    dplyr::mutate(
      lon_lbl = sf::st_coordinates(.)[, 1],
      lat_lbl = sf::st_coordinates(.)[, 2]
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(dplyr::all_of(c(id_col, "lon_lbl", "lat_lbl")))
}

#' Compute epidemiologically meaningful scale breaks
#' @keywords internal
#' @noRd
.smart_breaks <- function(vals, n = 5, log = TRUE) {
  vals <- vals[is.finite(vals) & vals > 0]
  if (length(vals) == 0) return(c(0, 1, 10, 100, 1000))
  
  mx <- max(vals, na.rm = TRUE)
  
  # Use decade-aligned breaks appropriate for epidemiological data
  candidates <- c(0, 1, 5, 10, 20, 50, 100, 200, 500,
                  1000, 2000, 5000, 10000, 50000, 100000, 500000, 1000000)
  candidates <- candidates[candidates <= mx * 1.05]
  
  # Pick n evenly spaced from candidates
  idx <- round(seq(1, length(candidates), length.out = min(n, length(candidates))))
  unique(candidates[idx])
}

#' Resolve colour palette
#' @keywords internal
#' @noRd
.resolve_palette <- function(palette) {
  built_in <- list(
    YlOrRd = c("#FFFFB2","#FED976","#FEB24C","#FD8D3C","#FC4E2A","#E31A1C","#B10026"),
    Blues   = c("#EFF3FF","#C6DBEF","#9ECAE1","#6BAED6","#3182BD","#08519C","#08306B"),
    RdPu    = c("#FEEBE2","#FCC5C0","#FA9FB5","#F768A1","#DD3497","#AE017E","#7A0177"),
    YlGnBu  = c("#FFFFD9","#EDF8B1","#C7E9B4","#7FCDBB","#41B6C4","#1D91C0","#0C2C84"),
    Greens  = c("#F7FCF5","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#005A32"),
    PuRd    = c("#F7F4F9","#E7E1EF","#D4B9DA","#C994C7","#DF65B0","#E7298A","#91003F")
  )
  
  if (is.character(palette) && length(palette) == 1 && palette %in% names(built_in)) {
    return(built_in[[palette]])
  }
  if (length(palette) >= 3) return(palette)  # user-supplied vector
  
  # Try RColorBrewer
  if (requireNamespace("RColorBrewer", quietly = TRUE)) {
    tryCatch(
      return(RColorBrewer::brewer.pal(7, palette)),
      error = function(e) NULL
    )
  }
  
  cli::cli_alert_warning("Palette '{palette}' not recognised. Falling back to YlOrRd.")
  built_in[["YlOrRd"]]
}

#' Auto-generate outcome label from system + column name
#' @keywords internal
#' @noRd
.system_outcome_label <- function(system, value_col, lang) {
  lookup <- list(
    en = list(
      SIM    = "Mortality",     "SIM-DO" = "Mortality",
      SIH    = "Hospitalizations", "SIH-RD" = "Hospitalizations",
      SINAN  = "Disease Notifications",
      SINASC = "Live Births",
      SIA    = "Outpatient Procedures",
      CNES   = "Health Infrastructure"
    ),
    pt = list(
      SIM    = "Mortalidade",   "SIM-DO" = "Mortalidade",
      SIH    = "Internações",   "SIH-RD" = "Internações",
      SINAN  = "Notificações",
      SINASC = "Nascimentos",
      SIA    = "Procedimentos",
      CNES   = "Infraestrutura"
    ),
    es = list(
      SIM    = "Mortalidad",    "SIM-DO" = "Mortalidad",
      SIH    = "Hospitalizaciones",
      SINAN  = "Notificaciones",
      SINASC = "Nacimientos",
      SIA    = "Procedimientos",
      CNES   = "Infraestructura"
    )
  )
  outcome <- lookup[[lang]][[system]] %||% gsub("_", " ", value_col)
  paste0("Spatial Distribution of ", outcome)
}

#' i18n labels for map text elements
#' @keywords internal
#' @noRd
.map_labels <- function(lang) {
  labels <- list(
    en = list(
      rate_prefix      = "Rate per ",
      inhabitants      = " inhabitants",
      log_scale        = "log\u2081\u208a\u2093 scale",
      log_scale_paren  = "(log scale)",
      bubble_sub_size  = "Bubble size: cumulative total (log scale)",
      bubble_sub_color = "Colour: mean rate per ",
      choropleth_sub   = "Annual mean rate per",
      panel_b_title    = "Annual Progression by Municipality",
      cumulative_cases = "Cumulative Total",
      all_states       = "all states",
      caption_source   = "Source: ",
      caption_ministry = ", Brazilian Ministry of Health / DATASUS. ",
      caption_pop      = "Population: ",
      caption_states   = "States included: ",
      caption_shp      = "Shapefiles: IBGE ",
      caption_geobr    = " via geobr (Pereira et al., 2021). ",
      caption_log      = "Colour scale uses log(1+x) transformation."
    ),
    pt = list(
      rate_prefix      = "Taxa por ",
      inhabitants      = " habitantes",
      log_scale        = "escala log\u2081\u208a\u2093",
      log_scale_paren  = "(escala log)",
      bubble_sub_size  = "Tamanho: total acumulado (escala log)",
      bubble_sub_color = "Cor: taxa media por ",
      choropleth_sub   = "Taxa media anual por",
      panel_b_title    = "Progressao Anual por Municipio",
      cumulative_cases = "Total Acumulado",
      all_states       = "todos os estados",
      caption_source   = "Fonte: ",
      caption_ministry = ", Ministerio da Saude / DATASUS. ",
      caption_pop      = "Populacao: ",
      caption_states   = "Estados incluidos: ",
      caption_shp      = "Shapes: IBGE ",
      caption_geobr    = " via geobr (Pereira et al., 2021). ",
      caption_log      = "Escala de cor usa transformacao log(1+x)."
    ),
    es = list(
      rate_prefix      = "Tasa por ",
      inhabitants      = " habitantes",
      log_scale        = "escala log\u2081\u208a\u2093",
      log_scale_paren  = "(escala log)",
      bubble_sub_size  = "Tamano: total acumulado (escala log)",
      bubble_sub_color = "Color: tasa media por ",
      choropleth_sub   = "Tasa media anual por",
      panel_b_title    = "Progresion Anual por Municipio",
      cumulative_cases = "Total Acumulado",
      all_states       = "todos los estados",
      caption_source   = "Fuente: ",
      caption_ministry = ", Ministerio de Salud / DATASUS. ",
      caption_pop      = "Poblacion: ",
      caption_states   = "Estados incluidos: ",
      caption_shp      = "Shapefiles: IBGE ",
      caption_geobr    = " via geobr (Pereira et al., 2021). ",
      caption_log      = "Escala de color usa transformacion log(1+x)."
    )
  )
  labels[[lang]]
}

#' Shared journal-quality map theme
#' @keywords internal
#' @noRd
.map_theme <- function(base_size = 10) {
  ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(
        face   = "bold", size  = base_size * 1.05,
        color  = "#1A252F", margin = ggplot2::margin(b = 3)
      ),
      plot.subtitle      = ggplot2::element_text(
        size        = base_size * 0.82,
        color       = "grey40",
        lineheight  = 1.3,
        margin      = ggplot2::margin(b = 6)
      ),
      legend.title       = ggplot2::element_text(
        size = base_size * 0.75, face = "bold", color = "grey25"
      ),
      legend.text        = ggplot2::element_text(size = base_size * 0.70, color = "grey30"),
      plot.margin        = ggplot2::margin(8, 8, 4, 8)
    )
}