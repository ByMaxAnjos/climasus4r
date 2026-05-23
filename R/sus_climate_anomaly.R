# =============================================================================
# sus_climate_anomaly.R
# Climate Anomaly Computation vs. INMET Climatological Normals
#
# Theory:
#   WMO (2017) - WMO Guidelines on the Calculation of Climate Normals, No. 1203
#   Perkins, S.E. et al. (2012) - On the measurement of heat waves
#   Gasparrini et al. (2017, Lancet Planet Health) - multi-city burden
#
# Input:
#   observed — climasus_df from sus_climate_inmet() or sus_climate_aggregate()
#   normals  — climasus_df from sus_climate_normals()
#
# Output: climasus_df [stage="climate", type="anomaly"]
#   {var}_obs, {var}_normal, {var}_anomaly
#   (+ {var}_anomaly_pct and/or {var}_anomaly_std depending on method)
# =============================================================================

# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "month_num", "year_val", "decade_num",
  "mes", "decada", "var_code", "valor", "codigo",
  "col_name", "n_years", "sd_obs"
))

# ── Built-in variable mapping: INMET canonical col → normals var_code ─────────
# Expand as new normals variables are added to normal_meta.parquet
.anomaly_var_map <- c(
  tair_max_c      = "t_max",
  tair_min_c      = "t_min",
  tair_dry_bulb_c = "t_mean_comp",
  rainfall_mm     = "precipitation",
  rh_mean_porc    = "rh_mean"
)

# Aggregation type per INMET column ("mean" or "sum")
.anomaly_agg_type <- c(
  tair_max_c      = "mean",
  tair_min_c      = "mean",
  tair_dry_bulb_c = "mean",
  rainfall_mm     = "sum",
  rh_mean_porc    = "mean"
)

# Portuguese month name (ASCII, lowercase) keyed by month number string
.anom_pt_months <- c(
  "1"  = "janeiro",  "2"  = "fevereiro", "3"  = "marco",
  "4"  = "abril",    "5"  = "maio",      "6"  = "junho",
  "7"  = "julho",    "8"  = "agosto",    "9"  = "setembro",
  "10" = "outubro",  "11" = "novembro",  "12" = "dezembro"
)

# ── Local i18n ────────────────────────────────────────────────────────────────
.anom_labels <- list(

  title = list(
    pt = "climasus4r — Anomalias Climáticas vs. Normais INMET",
    en = "climasus4r — Climate Anomalies vs. INMET Normals",
    es = "climasus4r — Anomalías Climáticas vs. Normales INMET"
  ),
  step_validate = list(
    pt = "Validando entradas: {n_obs} obs. | {n_norm} normais...",
    en = "Validating inputs: {n_obs} obs. | {n_norm} normals...",
    es = "Validando entradas: {n_obs} obs. | {n_norm} normales..."
  ),
  step_vars = list(
    pt = "Variáveis mapeadas ({n_vars}): {vars_str}",
    en = "Mapped variables ({n_vars}): {vars_str}",
    es = "Variables mapeadas ({n_vars}): {vars_str}"
  ),
  step_aggregate = list(
    pt = "Agregando observações à escala {time_scale}...",
    en = "Aggregating observations to {time_scale} scale...",
    es = "Agregando observaciones a escala {time_scale}..."
  ),
  step_normals = list(
    pt = "Preparando normais (escala {time_scale})...",
    en = "Preparing normals ({time_scale} scale)...",
    es = "Preparando normales (escala {time_scale})..."
  ),
  step_join = list(
    pt = "Unindo dados de {n_st} estação/ões com normais...",
    en = "Joining {n_st} station(s) with normals...",
    es = "Uniendo {n_st} estación/es con normales..."
  ),
  step_compute = list(
    pt = "Calculando anomalias (método: {method})...",
    en = "Computing anomalies (method: {method})...",
    es = "Calculando anomalías (método: {method})..."
  ),
  done = list(
    pt = "Concluído. {n_rows} períodos | {n_st} estações | {n_vars} variáveis | método: {method}",
    en = "Done. {n_rows} periods | {n_st} stations | {n_vars} variables | method: {method}",
    es = "Listo. {n_rows} períodos | {n_st} estaciones | {n_vars} variables | método: {method}"
  ),

  err_not_observed = list(
    pt = "{.arg observed} deve ser um {.cls climasus_df} de {.fn sus_climate_inmet} ou {.fn sus_climate_aggregate}.",
    en = "{.arg observed} must be a {.cls climasus_df} from {.fn sus_climate_inmet} or {.fn sus_climate_aggregate}.",
    es = "{.arg observed} debe ser un {.cls climasus_df} de {.fn sus_climate_inmet} o {.fn sus_climate_aggregate}."
  ),
  err_not_normals = list(
    pt = "{.arg normals} deve ser um {.cls climasus_df} de {.fn sus_climate_normals}.",
    en = "{.arg normals} must be a {.cls climasus_df} from {.fn sus_climate_normals}.",
    es = "{.arg normals} debe ser un {.cls climasus_df} de {.fn sus_climate_normals}."
  ),
  err_no_station = list(
    pt = "Coluna {.val {col}} não encontrada em {.arg observed}. Ajuste {.arg station_col}.",
    en = "Column {.val {col}} not found in {.arg observed}. Adjust {.arg station_col}.",
    es = "Columna {.val {col}} no encontrada en {.arg observed}. Ajuste {.arg station_col}."
  ),
  err_no_date = list(
    pt = "Coluna de data {.val {col}} não encontrada em {.arg observed}. Ajuste {.arg date_col}.",
    en = "Date column {.val {col}} not found in {.arg observed}. Adjust {.arg date_col}.",
    es = "Columna de fecha {.val {col}} no encontrada en {.arg observed}. Ajuste {.arg date_col}."
  ),
  err_no_vars = list(
    pt = "Nenhuma variável mapeável encontrada. Forneça {.arg vars} explicitamente ou verifique o catálogo com {.fn sus_climate_normals_meta}.",
    en = "No mappable variables found. Provide {.arg vars} explicitly or check the catalogue with {.fn sus_climate_normals_meta}.",
    es = "Ninguna variable mapeable encontrada. Proporcione {.arg vars} explícitamente o consulte el catálogo con {.fn sus_climate_normals_meta}."
  ),
  err_vars_not_in_obs = list(
    pt = "Variáveis não encontradas em {.arg observed}: {.val {miss}}.",
    en = "Variables not found in {.arg observed}: {.val {miss}}.",
    es = "Variables no encontradas en {.arg observed}: {.val {miss}}."
  ),
  err_varcodes_not_in_norm = list(
    pt = "var_code não encontrado em {.arg normals}: {.val {miss}}. Use {.fn sus_climate_normals_meta} para ver os códigos disponíveis.",
    en = "var_code not found in {.arg normals}: {.val {miss}}. Use {.fn sus_climate_normals_meta} to see available codes.",
    es = "var_code no encontrado en {.arg normals}: {.val {miss}}. Use {.fn sus_climate_normals_meta} para ver los códigos disponibles."
  ),
  err_no_join = list(
    pt = "Nenhuma correspondência encontrada após o join. Verifique se os códigos de estação de {.arg observed} coincidem com {.field codigo} em {.arg normals}.",
    en = "No matches found after join. Verify that station codes in {.arg observed} match {.field codigo} in {.arg normals}.",
    es = "Sin coincidencias tras el join. Verifique que los códigos de {.arg observed} coincidan con {.field codigo} en {.arg normals}."
  ),
  warn_std_few_years = list(
    pt = "Anomalia padronizada: {n_low} estação/mês com <3 anos de dados; SD será impreciso.",
    en = "Standardized anomaly: {n_low} station/month(s) with <3 years of data; SD will be imprecise.",
    es = "Anomalía estandarizada: {n_low} estación/mes con <3 años de datos; SD será impreciso."
  ),
  warn_lang = list(
    pt = "Idioma '{lang}' não suportado. Usando 'pt'.",
    en = "Language '{lang}' not supported. Using 'pt'.",
    es = "Idioma '{lang}' no admitido. Usando 'pt'."
  )
)

#' @keywords internal
#' @noRd
.anoml <- function(key, lang, ...) {
  entry <- .anom_labels[[key]]
  if (is.null(entry)) return(key)
  msg <- entry[[lang]] %||% entry[["pt"]]
  if (...length() > 0L) msg <- glue::glue(msg, .envir = rlang::env(...))
  msg
}


# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' Climate Anomaly Computation vs. INMET Climatological Normals
#'
#' Computes climate anomalies by comparing observed meteorological data from
#' [sus_climate_inmet()] against 30-year climatological normals from
#' [sus_climate_normals()]. Three anomaly types are supported: absolute
#' (obs − normal), relative ((obs − normal) / |normal| × 100 %), and
#' standardized z-score ((obs − normal) / σ_obs). The result is a `climasus_df`
#' ready for epidemiological modelling with [sus_mod_dlnm()],
#' [sus_climate_compute_heatwaves()], or [sus_mod_vulnerability_index()].
#'
#' @section Why anomalies matter for climate-health with DATASUS:
#' \itemize{
#'   \item **Multi-city comparability**: standardized anomalies make DLNM
#'     exposure-response curves poolable across cities with very different
#'     baseline climates (Manaus vs. Porto Alegre).
#'   \item **Extreme event attribution**: anomaly peaks pinpoint the dates to
#'     correlate with DATASUS hospitalization or mortality surges.
#'   \item **Climate trend detection**: a positive trend in temperature anomalies
#'     captures local warming for the IPCC Exposure pillar in
#'     [sus_mod_vulnerability_index()].
#'   \item **Vector-borne and waterborne diseases**: precipitation anomalies
#'     detect flood/drought episodes — with a 2–4 week lag — relevant to dengue,
#'     leptospirosis, and diarrheal disease in SINAN.
#'   \item **Thermal comfort indices**: anomaly-adjusted temperature feeds DLNM
#'     models without confounding by mean inter-city temperature differences.
#' }
#'
#' @section Anomaly methods:
#' | Method | Formula | Primary use |
#' |--------|---------|------------|
#' | `"absolute"` | obs − normal | Temperature anomalies (°C) |
#' | `"relative"` | (obs − normal) / \|normal\| × 100 | Precipitation anomaly (%) |
#' | `"standardized"` | (obs − normal) / σ_obs | Multi-city pooling, DLNM input |
#' | `"all"` | All three | Exploratory analysis |
#'
#' @param observed A `climasus_df` from [sus_climate_inmet()] or
#'   [sus_climate_aggregate()]. At minimum must contain: a station identifier
#'   column (see `station_col`), a date column (see `date_col`), and at least
#'   one climate variable column covered by the `vars` mapping.
#' @param normals A `climasus_df` from [sus_climate_normals()]. Must contain
#'   columns `codigo`, `mes`, `decada`, `var_code`, and `valor`. Run
#'   [sus_climate_normals_meta()] to see available `var_code` values.
#' @param vars Named character vector mapping **observed column names** to
#'   **normals `var_code` values**, e.g.
#'   `c(tair_max_c = "t_max", rainfall_mm = "precipitation")`.
#'   `NULL` (default) auto-detects variables using the built-in map for
#'   standard INMET columns.
#' @param method Character. Anomaly computation method: `"absolute"` (default),
#'   `"relative"`, `"standardized"`, or `"all"`.
#' @param time_scale Character. Temporal resolution for the anomaly:
#'   `"monthly"` (default) averages normals across the three decades of each
#'   month and aggregates observations by calendar month; `"decadal"` preserves
#'   the 10-day decade structure of INMET normals and is more precise for
#'   variables such as temperature extremes and precipitation.
#' @param station_col Character. Name of the station identifier column in
#'   `observed`. Default `"station_code"`. Must contain INMET codes matching
#'   the `codigo` column in `normals`.
#' @param date_col Character. Name of the date or datetime column in
#'   `observed`. Default `"date"`. Accepts `Date` or `POSIXct`.
#' @param lang Character. Language for messages: `"pt"` (default), `"en"`,
#'   `"es"`.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return A `climasus_df` at stage `"climate"` and type `"anomaly"`. One row
#'   per station × year × month (or × decade when `time_scale = "decadal"`).
#'   Key columns:
#' \describe{
#'   \item{`station_col`}{Station identifier (same as `station_col` argument).}
#'   \item{`year`}{Calendar year of the observation.}
#'   \item{`month_num`}{Month number (1–12).}
#'   \item{`month_name`}{Portuguese month name, ASCII.}
#'   \item{`decade_num`}{Decade within month (1, 2, or 3). Present only when
#'     `time_scale = "decadal"`.}
#'   \item{`{v}_obs`}{Observed monthly/decadal value (mean or sum depending on
#'     variable type).}
#'   \item{`{v}_normal`}{Climatological normal value.}
#'   \item{`{v}_anomaly`}{Absolute anomaly: obs − normal.}
#'   \item{`{v}_anomaly_pct`}{Relative anomaly (\%): (obs − normal) /
#'     |normal| × 100. Only when `method ∈ c("relative", "all")`.}
#'   \item{`{v}_anomaly_std`}{Standardized anomaly (z-score):
#'     (obs − normal) / σ_obs, where σ is the inter-annual SD of monthly
#'     observations for each station × month. Only when
#'     `method ∈ c("standardized", "all")`.}
#' }
#'
#' @examples
#' \dontrun{
#' # 1. Download data
#' obs  <- sus_climate_inmet(years = 2018:2023, uf = "RJ")
#' norm <- sus_climate_normals(period = "1991-2020",
#'                              target_var = c("t_max", "t_min",
#'                                             "t_mean_comp", "precipitation"))
#'
#' # 2. Monthly absolute anomalies (default)
#' anom <- sus_climate_anomaly(obs, norm)
#'
#' # 3. Standardized anomalies for multi-city DLNM pooling
#' anom_std <- sus_climate_anomaly(obs, norm,
#'                                  method     = "standardized",
#'                                  vars       = c(tair_dry_bulb_c = "t_mean_comp"))
#'
#' # 4. Decadal precipitation anomalies — dengue/leptospirosis lag studies
#' anom_10d <- sus_climate_anomaly(obs, norm,
#'                                  method     = "relative",
#'                                  time_scale = "decadal",
#'                                  vars       = c(rainfall_mm = "precipitation"))
#'
#' # 5. All methods at once — exploratory
#' anom_all <- sus_climate_anomaly(obs, norm, method = "all")
#' }
#'
#' @seealso [sus_climate_normals()], [sus_climate_normals_meta()],
#'   [sus_climate_inmet()], [sus_climate_aggregate()],
#'   [sus_mod_dlnm()], [sus_mod_vulnerability_index()]
#'
#' @export
#' @importFrom dplyr mutate filter select left_join summarise across any_of
#'   n_distinct collect tibble bind_rows
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate month day year
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning
#'   cli_abort
#' @importFrom rlang env sym .data
#' @importFrom glue glue
sus_climate_anomaly <- function(
    observed,
    normals,
    vars        = NULL,
    method      = c("absolute", "relative", "standardized", "all"),
    time_scale  = c("monthly", "decadal"),
    station_col = "station_code",
    date_col    = "date",
    lang        = c("pt", "en", "es"),
    verbose     = TRUE
) {
  method     <- match.arg(method)
  time_scale <- match.arg(time_scale)
  lang <- if (is.character(lang) && length(lang) == 1L) {
    if (!lang %in% c("pt", "en", "es")) {
      cli::cli_alert_warning(glue::glue(.anoml("warn_lang", "pt")))
      "pt"
    } else lang
  } else match.arg(lang)

  if (verbose) cli::cli_h1(.anoml("title", lang))

  # -- 1. Materialize Arrow objects --------------------------------------------
  if (inherits(observed,
               c("arrow_dplyr_query", "Dataset", "ArrowTabular", "Table"))) {
    meta_bkp <- tryCatch(sus_meta(observed), error = function(e) list())
    observed  <- dplyr::collect(observed)
    if (length(meta_bkp) > 0L) observed <- new_climasus_df(observed, meta_bkp)
  }
  if (inherits(normals,
               c("arrow_dplyr_query", "Dataset", "ArrowTabular", "Table"))) {
    normals <- dplyr::collect(normals)
  }

  # -- 2. Validate inputs ------------------------------------------------------
  if (!inherits(observed, "climasus_df"))
    cli::cli_abort(.anoml("err_not_observed", lang))
  if (!inherits(normals, "climasus_df"))
    cli::cli_abort(.anoml("err_not_normals", lang))
  if (!station_col %in% names(observed))
    cli::cli_abort(.anoml("err_no_station", lang, col = station_col))
  if (!date_col %in% names(observed))
    cli::cli_abort(.anoml("err_no_date", lang, col = date_col))

  required_norm_cols <- c("codigo", "mes", "decada", "var_code", "valor")
  missing_nc <- setdiff(required_norm_cols, names(normals))
  if (length(missing_nc) > 0L)
    cli::cli_abort("Missing columns in {.arg normals}: {.val {missing_nc}}.")

  if (verbose)
    cli::cli_alert_info(
      .anoml("step_validate", lang,
             n_obs  = nrow(observed),
             n_norm = nrow(normals))
    )

  # -- 3. Resolve variable mapping ---------------------------------------------
  norm_codes <- unique(normals$var_code)

  if (is.null(vars)) {
    # Auto-detect: default map keys present in observed AND var_codes in normals
    avail     <- intersect(names(.anomaly_var_map), names(observed))
    avail_map <- .anomaly_var_map[avail]
    avail_map <- avail_map[avail_map %in% norm_codes]
    if (length(avail_map) == 0L)
      cli::cli_abort(.anoml("err_no_vars", lang))
    vars <- avail_map
  } else {
    miss_obs  <- setdiff(names(vars), names(observed))
    if (length(miss_obs) > 0L)
      cli::cli_abort(.anoml("err_vars_not_in_obs", lang, miss = miss_obs))
    miss_norm <- setdiff(vars, norm_codes)
    if (length(miss_norm) > 0L)
      cli::cli_abort(.anoml("err_varcodes_not_in_norm", lang, miss = miss_norm))
  }

  if (verbose)
    cli::cli_alert_info(
      .anoml("step_vars", lang,
             n_vars   = length(vars),
             vars_str = paste(paste0(names(vars), "→", vars),
                              collapse = ", "))
    )

  # -- 4. Aggregate observed to target time scale ------------------------------
  if (verbose)
    cli::cli_alert_info(.anoml("step_aggregate", lang, time_scale = time_scale))

  obs_agg <- .anom_aggregate_observed(
    df          = observed,
    vars        = vars,
    station_col = station_col,
    date_col    = date_col,
    time_scale  = time_scale
  )

  # -- 5. Prepare normals as wide table ----------------------------------------
  if (verbose)
    cli::cli_alert_info(.anoml("step_normals", lang, time_scale = time_scale))

  norm_wide <- .anom_prepare_normals(normals, vars, time_scale)

  # Rename codigo → station_col for clean join
  names(norm_wide)[names(norm_wide) == "codigo"] <- station_col

  # -- 6. Join -----------------------------------------------------------------
  n_st <- dplyr::n_distinct(obs_agg[[station_col]])
  if (verbose)
    cli::cli_alert_info(.anoml("step_join", lang, n_st = n_st))

  join_by_cols <- if (time_scale == "monthly") {
    c(station_col, "month_num")
  } else {
    c(station_col, "month_num", "decade_num")
  }

  joined <- dplyr::left_join(obs_agg, norm_wide, by = join_by_cols)

  if (nrow(joined) == 0L)
    cli::cli_abort(.anoml("err_no_join", lang))

  # -- 7. Compute anomaly columns ----------------------------------------------
  if (verbose)
    cli::cli_alert_info(.anoml("step_compute", lang, method = method))

  result <- .anom_compute(
    df          = joined,
    vars        = vars,
    method      = method,
    station_col = station_col,
    lang        = lang,
    verbose     = verbose
  )

  # -- 8. Add month_name column ------------------------------------------------
  result$month_name <- unname(.anom_pt_months[as.character(result$month_num)])

  # Rename year_val → year for cleaner output
  names(result)[names(result) == "year_val"] <- "year"

  # Reorder: station, year, month_num, month_name, [decade_num], then rest
  id_cols    <- intersect(c(station_col, "year", "month_num", "month_name",
                             "decade_num"), names(result))
  other_cols <- setdiff(names(result), id_cols)
  result     <- result[, c(id_cols, other_cols), drop = FALSE]

  # -- 9. Promote to climasus_df -----------------------------------------------
  normal_period <- tryCatch(
    attr(normals, "sus_meta")[["period"]], error = function(e) NULL
  )

  meta <- list(
    system         = tryCatch(sus_meta(observed, "system"), error = function(e) NULL),
    stage          = "climate",
    type           = "anomaly",
    method         = method,
    time_scale     = time_scale,
    normal_period  = normal_period,
    n_stations     = dplyr::n_distinct(result[[station_col]], na.rm = TRUE),
    n_observations = nrow(result),
    n_variables    = length(vars),
    var_map        = vars,
    created        = Sys.time(),
    modified       = Sys.time(),
    history        = sprintf(
      "[%s] Climate anomalies — method: %s; scale: %s; normal period: %s; vars: %s",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      method, time_scale,
      normal_period %||% "unknown",
      paste(names(vars), collapse = ", ")
    ),
    user = list()
  )

  base_classes <- setdiff(class(result), "climasus_df")
  result <- structure(result, sus_meta = meta,
                      class = c("climasus_df", base_classes))

  if (verbose) {
    cli::cli_alert_success(
      .anoml("done", lang,
             n_rows  = nrow(result),
             n_st    = dplyr::n_distinct(result[[station_col]], na.rm = TRUE),
             n_vars  = length(vars),
             method  = method)
    )
  }

  result
}


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

#' Aggregate observed data to monthly or decadal level
#' @keywords internal
#' @noRd
.anom_aggregate_observed <- function(df, vars, station_col, date_col,
                                      time_scale) {
  df2           <- df
  df2$month_num <- lubridate::month(df2[[date_col]])
  df2$year_val  <- lubridate::year(df2[[date_col]])

  group_cols <- c(station_col, "year_val", "month_num")

  if (time_scale == "decadal") {
    day_num        <- lubridate::day(df2[[date_col]])
    df2$decade_num <- dplyr::case_when(
      day_num <= 10L ~ 1L,
      day_num <= 20L ~ 2L,
      .default       = 3L
    )
    group_cols <- c(group_cols, "decade_num")
  }

  obs_cols  <- names(vars)
  sum_vars  <- obs_cols[
    obs_cols %in% names(.anomaly_agg_type) &
      .anomaly_agg_type[obs_cols] == "sum"
  ]
  mean_vars <- setdiff(obs_cols, sum_vars)

  df_sub <- df2[, unique(c(group_cols, obs_cols)), drop = FALSE]

  df_sub |>
    dplyr::summarise(
      dplyr::across(dplyr::any_of(mean_vars), ~ mean(.x, na.rm = TRUE)),
      dplyr::across(dplyr::any_of(sum_vars),  ~ sum(.x,  na.rm = TRUE)),
      .by = dplyr::all_of(group_cols)
    )
}

#' Prepare normals as a wide table ready for joining
#'
#' Monthly: averages across the 3 decades of each station × month.
#' Decadal: keeps the three 10-day rows per month.
#' Returns wide tibble: one `{inmet_col}_normal` column per mapped variable.
#'
#' @keywords internal
#' @noRd
.anom_prepare_normals <- function(normals, vars, time_scale) {
  # vars: named vector c(inmet_col = "var_code")
  norm_codes <- unique(vars)  # var_code values needed

  # Build reverse map: var_code → inmet_col name
  rev_map <- stats::setNames(names(vars), unname(vars))

  # Filter to needed var_codes and add month_num
  norm_sub <- normals[normals$var_code %in% norm_codes, ]

  # Convert PT month name → numeric
  month_idx       <- stats::setNames(
    as.integer(names(.anom_pt_months)),
    unname(.anom_pt_months)
  )
  norm_sub$month_num <- month_idx[norm_sub$mes]

  # Create output column name: e.g. "tair_max_c_normal"
  norm_sub$col_name <- paste0(rev_map[norm_sub$var_code], "_normal")

  if (time_scale == "monthly") {
    # Average across all decades for each station × month × variable
    norm_agg <- norm_sub |>
      dplyr::summarise(
        valor = mean(.data$valor, na.rm = TRUE),
        .by   = c("codigo", "month_num", "col_name")
      )
    tidyr::pivot_wider(
      norm_agg[, c("codigo", "month_num", "col_name", "valor")],
      names_from  = col_name,
      values_from = valor
    )
  } else {
    # Keep decadal structure (decade "1"/"2"/"3" → integer)
    norm_sub$decade_num <- as.integer(norm_sub$decada)
    tidyr::pivot_wider(
      norm_sub[, c("codigo", "month_num", "decade_num", "col_name", "valor")],
      names_from  = col_name,
      values_from = valor
    )
  }
}

#' Compute anomaly columns in-place on the joined table
#' @keywords internal
#' @noRd
.anom_compute <- function(df, vars, method, station_col, lang, verbose) {

  # For standardized: pre-compute inter-annual SD per station × month
  sd_lookup <- NULL
  if (method %in% c("standardized", "all")) {
    obs_cols <- names(vars)
    sd_rows  <- lapply(obs_cols, function(v) {
      if (!v %in% names(df)) return(NULL)
      df |>
        dplyr::summarise(
          sd_obs  = stats::sd(.data[[v]], na.rm = TRUE),
          n_years = dplyr::n_distinct(.data$year_val),
          .by     = c(station_col, "month_num")
        ) |>
        dplyr::mutate(var_col = v)
    })
    sd_lookup <- dplyr::bind_rows(Filter(Negate(is.null), sd_rows))

    n_low <- sum(sd_lookup$n_years < 3L, na.rm = TRUE)
    if (n_low > 0L)
      cli::cli_alert_warning(.anoml("warn_std_few_years", lang, n_low = n_low))
  }

  # Compute anomaly columns for each mapped variable
  for (v in names(vars)) {
    obs_col    <- v
    normal_col <- paste0(v, "_normal")

    if (!obs_col %in% names(df) || !normal_col %in% names(df)) next

    obs_vals    <- as.numeric(df[[obs_col]])
    normal_vals <- as.numeric(df[[normal_col]])

    # Rename observed column to {v}_obs for clarity in output
    names(df)[names(df) == obs_col] <- paste0(v, "_obs")

    # Absolute anomaly (always computed)
    df[[paste0(v, "_anomaly")]] <- obs_vals - normal_vals

    # Relative anomaly (%)
    if (method %in% c("relative", "all")) {
      df[[paste0(v, "_anomaly_pct")]] <- dplyr::if_else(
        abs(normal_vals) > 1e-9,
        (obs_vals - normal_vals) / abs(normal_vals) * 100,
        NA_real_
      )
    }

    # Standardized anomaly (z-score vs. inter-annual SD)
    if (method %in% c("standardized", "all") && !is.null(sd_lookup)) {
      v_sd <- sd_lookup[sd_lookup$var_col == v,
                        c(station_col, "month_num", "sd_obs")]
      df <- dplyr::left_join(df, v_sd,
                              by = c(station_col, "month_num"))
      df[[paste0(v, "_anomaly_std")]] <- dplyr::if_else(
        !is.na(df$sd_obs) & df$sd_obs > 1e-9,
        df[[paste0(v, "_anomaly")]] / df$sd_obs,
        NA_real_
      )
      df$sd_obs <- NULL
    }
  }

  df
}
