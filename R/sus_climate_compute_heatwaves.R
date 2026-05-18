# =============================================================================
# sus_climate_compute_heatwaves.R
# Detection of heatwaves using multiple internationally recognised methodologies
# Accepts climasus_df at stage "climate" (types: inmet, filled, indicators)
# =============================================================================

# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "date_day", "tmax", "tmin", "tmean", "utci_max", "utci_mean",
  "wbgt_max", "wbgt_mean", "hi_max", "hi_mean", "pet_max",
  "n_hours_extreme_wbgt", "n_hours_extreme_utci", "n_hours_extreme_hi",
  "region", "federal_unit", "station_name", "zona_climatica",
  "latitude", "longitude", "yday",
  "above_who", "above_wmo", "above_inmet", "above_ehf",
  "above_utci", "above_wbgt", "above_hi",
  "hw_ehf_raw", "t3", "t30", "t95", "ehi_sig", "ehi_acc", "ehf", "ehf_value",
  "tmax_p", "tmin_p", "tmax_hist", "tmean_hist",
  "utci_p", "wbgt_p", "hi_p",
  "hw_any", "hw_who", "hw_wmo", "hw_inmet", "hw_ehf",
  "hw_utci", "hw_wbgt", "hw_hi",
  "intensity_class", "event_id", "ehf_peak", "ehf_mean",
  "year", "n_events", "total_days_hw", "mean_duration", "max_duration",
  "mean_intensity", "max_intensity", "mean_anomaly",
  "severity_total", "severity_index", "anomaly_mean",
  "start_date", "end_date", "duration_days",
  "temp_mean", "temp_peak", "anomaly_cumulative"
))

# ── Local i18n ────────────────────────────────────────────────────────────────
.hw_labels <- list(
  step_validate = list(
    pt = "Validando entrada...",
    en = "Validating input...",
    es = "Validando entrada..."
  ),
  step_daily = list(
    pt = "Agregando dados horarios para escala diaria...",
    en = "Aggregating hourly data to daily scale...",
    es = "Agregando datos horarios a escala diaria..."
  ),
  step_baseline = list(
    pt = "Calculando limiares historicos (baseline)...",
    en = "Computing historical thresholds (baseline)...",
    es = "Calculando umbrales historicos (linea base)..."
  ),
  step_detect = list(
    pt = "Detectando ondas de calor por metodo...",
    en = "Detecting heatwaves by method...",
    es = "Detectando olas de calor por metodo..."
  ),
  step_events = list(
    pt = "Extraindo e classificando eventos...",
    en = "Extracting and classifying events...",
    es = "Extrayendo y clasificando eventos..."
  ),
  step_summary = list(
    pt = "Gerando resumo anual...",
    en = "Generating annual summary...",
    es = "Generando resumen anual..."
  ),
  done = list(
    pt = "Deteccao concluida: {n_ev} evento(s) em {n_st} estacao(oes).",
    en = "Detection complete: {n_ev} event(s) across {n_st} station(s).",
    es = "Deteccion completada: {n_ev} evento(s) en {n_st} estacion(es)."
  ),
  err_not_climasus = list(
    pt = "O argumento `df` deve ser um objeto {.cls climasus_df}.",
    en = "`df` must be a {.cls climasus_df} object.",
    es = "El argumento `df` debe ser un objeto {.cls climasus_df}."
  ),
  err_stage = list(
    pt = "Dados devem estar no estagio {.val climate}. Estagio atual: {.val {current_stage}}.",
    en = "Data must be at stage {.val climate}. Current stage: {.val {current_stage}}.",
    es = "Los datos deben estar en el etapa {.val climate}. Etapa actual: {.val {current_stage}}."
  ),
  err_type = list(
    pt = "Tipo de dados invalido: {.val {current_type}}. Esperado: {.val inmet}, {.val filled} ou {.val indicators}.",
    en = "Invalid data type: {.val {current_type}}. Expected: {.val inmet}, {.val filled}, or {.val indicators}.",
    es = "Tipo de datos invalido: {.val {current_type}}. Esperado: {.val inmet}, {.val filled} o {.val indicators}."
  ),
  err_no_cols = list(
    pt = "Nenhuma coluna de temperatura encontrada. Esperado: tair_dry_bulb_c, tair_max_c, tair_min_c, utci_c, wbgt_c ou hi_c.",
    en = "No temperature column found. Expected: tair_dry_bulb_c, tair_max_c, tair_min_c, utci_c, wbgt_c, or hi_c.",
    es = "No se encontro ninguna columna de temperatura. Esperado: tair_dry_bulb_c, tair_max_c, tair_min_c, utci_c, wbgt_c o hi_c."
  ),
  err_required_cols = list(
    pt = "Colunas obrigatorias ausentes: {.val {missing_cols}}.",
    en = "Missing required columns: {.val {missing_cols}}.",
    es = "Faltan columnas requeridas: {.val {missing_cols}}."
  ),
  err_baseline_empty = list(
    pt = "Nenhum dado disponivel no periodo baseline especificado.",
    en = "No data available in the specified baseline period.",
    es = "No hay datos disponibles en el periodo baseline especificado."
  ),
  err_date_range = list(
    pt = "Intervalo de datas invalido. Verifique a coluna de datas.",
    en = "Invalid date range. Check your date column.",
    es = "Rango de fechas invalido. Verifique su columna de fechas."
  ),
  warn_method_missing = list(
    pt = "Metodo {.val {m}} solicitado mas coluna {.val {col}} nao encontrada. Metodo ignorado.",
    en = "Method {.val {m}} requested but column {.val {col}} not found. Method skipped.",
    es = "Metodo {.val {m}} solicitado pero no se encontro la columna {.val {col}}. Metodo omitido."
  ),
  warn_short_series = list(
    pt = "Serie temporal curta ({n_days} dias). Resultados podem ser pouco robustos.",
    en = "Short time series ({n_days} days). Results may not be robust.",
    es = "Serie temporal corta ({n_days} dias). Los resultados pueden no ser robustos."
  ),
  warn_short_baseline = list(
    pt = "Periodo baseline curto ({n_years} anos). Recomenda-se ao menos 20 anos.",
    en = "Short baseline period ({n_years} years). At least 20 years recommended.",
    es = "Periodo baseline corto ({n_years} anos). Se recomiendan al menos 20 anos."
  ),
  warn_na_temp = list(
    pt = "{n_na} valor(es) NA em colunas de temperatura. Use sus_climate_fill_gap() antes.",
    en = "{n_na} NA value(s) in temperature columns. Use sus_climate_fill_gap() first.",
    es = "{n_na} valor(es) NA en columnas de temperatura. Use sus_climate_fill_gap() primero."
  ),
  warn_lang = list(
    pt = "Idioma {.val {lang}} nao suportado. Usando {.val pt}.",
    en = "Unsupported language {.val {lang}}. Falling back to {.val pt}.",
    es = "Idioma {.val {lang}} no soportado. Usando {.val pt}."
  ),
  info_baseline = list(
    pt = "Baseline: {start} a {end} ({n_years} anos/anos).",
    en = "Baseline: {start} to {end} ({n_years} year(s)).",
    es = "Baseline: {start} a {end} ({n_years} ano(s))."
  )
)

#' @keywords internal
#' @noRd
.hwl <- function(key, lang, ...) {
  entry <- .hw_labels[[key]]
  if (is.null(entry)) return(key)
  txt <- entry[[lang]] %||% entry[["pt"]]
  if (...length() > 0) glue::glue(txt, .envir = rlang::env(...)) else txt
}


# ── Exported wrapper ──────────────────────────────────────────────────────────

#' Detect heatwaves using multiple standard methodologies
#'
#' Applies seven heatwave detection methods (WHO, WMO, INMET, EHF, UTCI, WBGT,
#' HI) to a `climasus_df` at stage `"climate"` produced by
#' `sus_climate_inmet()`, `sus_climate_fill_gap()`, or
#' `sus_climate_compute_indicators()`. Aggregates hourly data to daily scale,
#' computes smoothed historical percentile thresholds from a reference baseline,
#' identifies heatwave days and discrete events per method, and returns a named
#' list with event, daily-flag, and annual-summary tables — all carrying
#' `sus_meta` at stage `"climate"` / type `"heatwaves"`.
#'
#' @section Implemented methods:
#'
#' **1. WHO** `Tmax > P90(Tmax)` for >= N consecutive days (default N=3).
#' Threshold smoothed over a 15-day rolling window by calendar day-of-year.
#' Reference: WMO & WHO (2015).
#'
#' **2. WMO** `Tmax > P90(Tmax) AND Tmin > P90(Tmin)` for >= N days (default
#' N=5). Double threshold ensures both daytime and nighttime extremes are
#' captured. Reference: Perkins & Alexander (2013).
#'
#' **3. INMET** `Tmax > mean(Tmax_hist) + 5 degC` for >= N days (default N=5).
#' Climatological mean from the baseline period. Designed for Brazilian context.
#' Reference: INMET (2009); MCTI/Gov.br (2025).
#'
#' **4. EHF** Excess Heat Factor: `EHF = EHI_sig x max(1, EHI_acc)` where
#' `EHI_sig = T3 - P95(Tmean)` and `EHI_acc = T3 - T30`. Intensity classified
#' by EHF85 (85th percentile of positive EHF values). Reference: Nairn &
#' Fawcett (2015).
#'
#' **5. UTCI** `UTCImax > P90(UTCImax)` for >= N days (default N=3). Requires
#' `utci_c` column from `sus_climate_compute_indicators()`.
#'
#' **6. WBGT** `WBGTmax > P90(WBGTmax)` for >= N days (default N=3). Requires
#' `wbgt_c` column from `sus_climate_compute_indicators()`.
#'
#' **7. HI** `HImax > P90(HImax)` for >= N days (default N=3). Requires `hi_c`
#' column from `sus_climate_compute_indicators()`.
#'
#' @param df A `climasus_df` at stage `"climate"` with type `"inmet"`,
#'   `"filled"`, or `"indicators"`. Produced by `sus_climate_inmet()`,
#'   `sus_climate_fill_gap()`, or `sus_climate_compute_indicators()`. Must
#'   contain columns `date` and `station_code`. Temperature columns
#'   `tair_max_c`, `tair_min_c`, `tair_dry_bulb_c` are required for WHO/WMO/
#'   INMET/EHF; `utci_c`, `wbgt_c`, `hi_c` are required for UTCI/WBGT/HI
#'   (available after `sus_climate_compute_indicators()`).
#' @param method Character vector. One or more of `"WHO"`, `"WMO"`, `"INMET"`,
#'   `"EHF"`, `"UTCI"`, `"WBGT"`, `"HI"`. Use `"all"` to run every method.
#'   Methods whose required columns are absent emit a warning and are skipped.
#' @param baseline_start Date or character (e.g., `"1981-01-01"`). Start of the
#'   reference period for percentile thresholds. `NULL` uses the full series.
#' @param baseline_end Date or character. End of the reference period.
#'   `NULL` uses the full series.
#' @param percentile Numeric (0–100, default `90`). Percentile used for WHO,
#'   WMO, UTCI, WBGT, and HI threshold calculations.
#' @param min_duration Integer or `NULL`. Minimum consecutive days to qualify as
#'   a heatwave. `NULL` uses method defaults: WHO=3, WMO=5, INMET=5, EHF=3,
#'   UTCI=3, WBGT=3, HI=3.
#' @param lang Character. Output language: `"pt"` (default), `"en"`, `"es"`.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return A named list with class `climasus_hw` and `sus_meta` attribute
#'   (stage `"climate"`, type `"heatwaves"`):
#'   \describe{
#'     \item{`events`}{One row per detected event: `event_id`, `station_code`,
#'       `method`, `start_date`, `end_date`, `duration_days`, `temp_mean`,
#'       `temp_peak`, `anomaly_mean`, `anomaly_cumulative`, `severity_index`,
#'       `ehf_peak`, `ehf_mean`, `intensity_class`, and station metadata.}
#'     \item{`daily`}{Daily aggregated data with logical flag columns
#'       `hw_<method>` and `hw_any`.}
#'     \item{`summary`}{Annual summary by station and method: `year`,
#'       `n_events`, `total_days_hw`, `mean_duration`, `max_duration`,
#'       `mean_intensity`, `max_intensity`, `mean_anomaly`, `severity_total`.}
#'   }
#'
#' @references
#' - Perkins, S.E. & Alexander, L.V. (2013). On the measurement of heat waves.
#'   *Journal of Climate*, 26(13), 4500-4517.
#' - WMO & WHO (2015). *Heatwaves and Health: Guidance on Warning-System
#'   Development*. Geneva.
#' - Nairn, J.R. & Fawcett, R.J.B. (2015). The Excess Heat Factor.
#'   *Int. J. Environ. Res. Public Health*, 12(1), 227-253.
#'   \doi{10.3390/ijerph120100227}
#' - Broede, P. et al. (2012). Deriving the operational procedure for UTCI.
#'   *Int. J. Biometeorology*, 56(3), 481-494. \doi{10.1007/s00484-011-0454-1}
#' - INMET (2009). *Normais Climatologicas do Brasil 1961-1990*. Brasilia.
#'
#' @examples
#' \dontrun{
#' hw <- df_indicators |>
#'   sus_climate_compute_heatwaves(
#'     method         = c("WHO", "INMET", "EHF"),
#'     baseline_start = "2000-01-01",
#'     baseline_end   = "2020-12-31",
#'     lang           = "pt"
#'   )
#'
#' hw$events
#' hw$summary
#' hw$daily[hw$daily$hw_any, ]
#' }
#'
#' @seealso [sus_climate_inmet()], [sus_climate_compute_indicators()]
#'
#' @export
#' @importFrom dplyr mutate select filter group_by summarise arrange left_join
#'   ungroup across all_of n first case_when n_distinct
#' @importFrom tidyr everything
#' @importFrom cli cli_progress_step cli_warn cli_abort cli_alert_success
#'   cli_alert_info cli_h1
#' @importFrom purrr map map_lgl list_rbind
#' @importFrom lubridate yday year
#' @importFrom slider slide_dbl slide_lgl
#' @importFrom rlang .data
#' @importFrom glue glue
sus_climate_compute_heatwaves <- function(
    df,
    method         = c("WHO", "WMO", "INMET", "EHF", "UTCI", "WBGT", "HI"),
    baseline_start = NULL,
    baseline_end   = NULL,
    percentile     = 90,
    min_duration   = NULL,
    lang           = "pt",
    verbose        = TRUE
) {
  rlang::check_installed(
    c("slider", "glue"),
    reason = "to run sus_climate_compute_heatwaves()"
  )

  # Language validation
  if (!lang %in% c("pt", "en", "es")) {
    cli::cli_alert_warning(.hwl("warn_lang", "pt", lang = lang))
    lang <- "pt"
  }

  # Expand "all"
  all_methods <- c("WHO", "WMO", "INMET", "EHF", "UTCI", "WBGT", "HI")
  if (length(method) == 1L && identical(method, "all")) method <- all_methods
  method <- match.arg(method, all_methods, several.ok = TRUE)

  # ── Materialise Arrow ──────────────────────────────────────────────────────
  if (inherits(df, c("arrow_dplyr_query", "Dataset", "ArrowTabular", "Table"))) {
    meta_backup <- tryCatch(sus_meta(df), error = function(e) list())
    df <- dplyr::collect(df)
    if (length(meta_backup) > 0) df <- new_climasus_df(df, meta_backup)
  }

  # ── climasus_df validation ─────────────────────────────────────────────────
  if (!inherits(df, "climasus_df")) {
    cli::cli_abort(.hwl("err_not_climasus", lang))
  }

  current_stage <- sus_meta(df, "stage")
  if (!identical(current_stage, "climate")) {
    cli::cli_abort(.hwl("err_stage", lang, current_stage = current_stage))
  }

  current_type <- sus_meta(df, "type")
  valid_types  <- c("inmet", "filled", "indicators")
  if (!current_type %in% valid_types) {
    cli::cli_abort(.hwl("err_type", lang, current_type = current_type))
  }

  # ── 1. Validate columns ────────────────────────────────────────────────────
  if (verbose) cli::cli_progress_step(.hwl("step_validate", lang))
  df <- .hw_validate(df, method, lang)

  # ── 2. Daily aggregation ───────────────────────────────────────────────────
  if (verbose) cli::cli_progress_step(.hwl("step_daily", lang))
  daily <- .hw_aggregate_daily(df)

  # ── 3. Baseline / thresholds ───────────────────────────────────────────────
  if (verbose) cli::cli_progress_step(.hwl("step_baseline", lang))

  if (!is.null(baseline_start)) baseline_start <- as.Date(baseline_start)
  if (!is.null(baseline_end))   baseline_end   <- as.Date(baseline_end)

  baseline <- .hw_compute_baseline(daily, baseline_start, baseline_end, percentile, lang, verbose)
  daily    <- daily |> dplyr::left_join(baseline, by = c("station_code", "yday"))

  # ── 4. Detection ───────────────────────────────────────────────────────────
  if (verbose) cli::cli_progress_step(.hwl("step_detect", lang))
  daily <- .hw_apply_all_methods(daily, method, min_duration, percentile)

  # ── 5. Event extraction ────────────────────────────────────────────────────
  if (verbose) cli::cli_progress_step(.hwl("step_events", lang))
  events <- .hw_extract_events(daily, method)

  # ── 6. Annual summary ─────────────────────────────────────────────────────
  if (verbose) cli::cli_progress_step(.hwl("step_summary", lang))
  summary_tbl <- .hw_build_summary(events, daily, method)

  # ── Attach sus_meta and return ─────────────────────────────────────────────
  n_ev <- nrow(events)
  n_st <- dplyr::n_distinct(events$station_code)
  if (verbose) {
    cli::cli_alert_success(.hwl("done", lang, n_ev = n_ev, n_st = n_st))
  }

  meta_out <- sus_meta(df)
  meta_out[["stage"]]   <- "climate"
  meta_out[["type"]]    <- "heatwaves"
  ts  <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  meta_out[["history"]] <- c(
    meta_out[["history"]],
    sprintf(
      "[%s] sus_climate_compute_heatwaves(): methods=%s; %d event(s); %d station(s); baseline=%s/%s",
      ts,
      paste(method, collapse = ","),
      n_ev,
      n_st,
      if (is.null(baseline_start)) "full" else as.character(baseline_start),
      if (is.null(baseline_end))   "full" else as.character(baseline_end)
    )
  )

  result <- list(events = events, daily = daily, summary = summary_tbl)
  attr(result, "sus_meta") <- meta_out
  class(result) <- c("climasus_hw", "list")
  result
}


# =============================================================================
# 1. COLUMN VALIDATION
# =============================================================================

#' @keywords internal
#' @noRd
.hw_validate <- function(df, method, lang) {

  required <- c("date", "station_code")
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    cli::cli_abort(.hwl("err_required_cols", lang, missing_cols = missing_cols))
  }

  temp_any <- intersect(
    c("tair_dry_bulb_c", "tair_max_c", "tair_min_c", "utci_c", "wbgt_c", "hi_c"),
    names(df)
  )
  if (length(temp_any) == 0L) {
    cli::cli_abort(.hwl("err_no_cols", lang))
  }

  method_col_map <- list(
    UTCI = "utci_c",
    WBGT = "wbgt_c",
    HI   = "hi_c"
  )
  for (m in intersect(method, names(method_col_map))) {
    col <- method_col_map[[m]]
    if (!col %in% names(df)) {
      cli::cli_warn(.hwl("warn_method_missing", lang, m = m, col = col))
    }
  }

  temp_present <- intersect(
    c("tair_dry_bulb_c", "tair_max_c", "tair_min_c", "utci_c", "wbgt_c", "hi_c"),
    names(df)
  )
  n_na <- sum(vapply(df[temp_present], function(x) sum(is.na(x)), integer(1L)))
  if (n_na > 0L) {
    cli::cli_warn(.hwl("warn_na_temp", lang, n_na = n_na))
  }

  df[["date"]] <- as.POSIXct(df[["date"]])

  date_range <- range(as.Date(df[["date"]]), na.rm = TRUE)
  if (!all(is.finite(date_range))) {
    cli::cli_abort(.hwl("err_date_range", lang))
  }
  n_days <- as.numeric(diff(date_range))
  if (n_days < 365L) {
    cli::cli_warn(.hwl("warn_short_series", lang, n_days = n_days))
  }

  df
}


# =============================================================================
# 2. DAILY AGGREGATION
# =============================================================================

#' @keywords internal
#' @noRd
.hw_aggregate_daily <- function(df) {

  has <- function(col) col %in% names(df)

  df |>
    dplyr::mutate(date_day = as.Date(.data[["date"]])) |>
    dplyr::summarise(
      tmax  = if (has("tair_max_c"))
                max(.data[["tair_max_c"]],     na.rm = TRUE)
              else if (has("tair_dry_bulb_c"))
                max(.data[["tair_dry_bulb_c"]], na.rm = TRUE)
              else NA_real_,

      tmin  = if (has("tair_min_c"))
                min(.data[["tair_min_c"]],      na.rm = TRUE)
              else if (has("tair_dry_bulb_c"))
                min(.data[["tair_dry_bulb_c"]], na.rm = TRUE)
              else NA_real_,

      tmean = if (has("tair_dry_bulb_c"))
                mean(.data[["tair_dry_bulb_c"]], na.rm = TRUE)
              else if (has("tair_max_c") && has("tair_min_c"))
                mean(c(
                  mean(.data[["tair_max_c"]], na.rm = TRUE),
                  mean(.data[["tair_min_c"]], na.rm = TRUE)
                ))
              else NA_real_,

      utci_max  = if (has("utci_c")) max(.data[["utci_c"]],  na.rm = TRUE) else NA_real_,
      utci_mean = if (has("utci_c")) mean(.data[["utci_c"]], na.rm = TRUE) else NA_real_,
      wbgt_max  = if (has("wbgt_c")) max(.data[["wbgt_c"]],  na.rm = TRUE) else NA_real_,
      wbgt_mean = if (has("wbgt_c")) mean(.data[["wbgt_c"]], na.rm = TRUE) else NA_real_,
      hi_max    = if (has("hi_c"))   max(.data[["hi_c"]],    na.rm = TRUE) else NA_real_,
      hi_mean   = if (has("hi_c"))   mean(.data[["hi_c"]],   na.rm = TRUE) else NA_real_,
      pet_max   = if (has("pet_c"))  max(.data[["pet_c"]],   na.rm = TRUE) else NA_real_,

      n_hours_extreme_wbgt = if (has("wbgt_c_flag_extreme"))
        sum(.data[["wbgt_c_flag_extreme"]], na.rm = TRUE) else NA_integer_,
      n_hours_extreme_utci = if (has("utci_c_flag_extreme"))
        sum(.data[["utci_c_flag_extreme"]], na.rm = TRUE) else NA_integer_,
      n_hours_extreme_hi   = if (has("hi_c_flag_extreme"))
        sum(.data[["hi_c_flag_extreme"]],   na.rm = TRUE) else NA_integer_,

      region         = if (has("region"))         dplyr::first(.data[["region"]])         else NA_character_,
      federal_unit   = if (has("federal_unit"))   dplyr::first(.data[["federal_unit"]])   else NA_character_,
      station_name   = if (has("station_name"))   dplyr::first(.data[["station_name"]])   else NA_character_,
      zona_climatica = if (has("zona_climatica")) dplyr::first(.data[["zona_climatica"]]) else NA_character_,
      latitude       = if (has("latitude"))       dplyr::first(.data[["latitude"]])       else NA_real_,
      longitude      = if (has("longitude"))      dplyr::first(.data[["longitude"]])      else NA_real_,

      .by = c("station_code", "date_day")
    ) |>
    dplyr::mutate(
      dplyr::across(where(is.double), ~ ifelse(is.infinite(.x), NA_real_, .x)),
      date_day = as.Date(date_day, origin = "1970-01-01"),
      yday = lubridate::yday(date_day)
    ) |>
    dplyr::arrange(station_code, date_day) |>
    dplyr::filter(!is.na(date_day))
}


# =============================================================================
# 3. BASELINE / THRESHOLDS
# =============================================================================

#' @keywords internal
#' @noRd
.hw_compute_baseline <- function(daily, baseline_start, baseline_end,
                                  percentile, lang, verbose) {

  ref <- daily
  if (!is.null(baseline_start)) ref <- dplyr::filter(ref, date_day >= baseline_start)
  if (!is.null(baseline_end))   ref <- dplyr::filter(ref, date_day <= baseline_end)

  if (nrow(ref) == 0L) {
    cli::cli_abort(.hwl("err_baseline_empty", lang))
  }

  n_years <- dplyr::n_distinct(lubridate::year(ref$date_day))
  if (verbose) {
    cli::cli_alert_info(.hwl("info_baseline", lang,
      start   = min(ref$date_day, na.rm = TRUE),
      end     = max(ref$date_day, na.rm = TRUE),
      n_years = n_years
    ))
  }
  if (n_years < 10L) {
    cli::cli_warn(.hwl("warn_short_baseline", lang, n_years = n_years))
  }

  station_codes <- unique(ref$station_code)

  purrr::map(station_codes, function(st) {
    st_data <- dplyr::filter(ref, station_code == st)

    purrr::map(1:366, function(d) {
      window_days <- ((d - 15L):(d + 15L) - 1L) %% 366L + 1L
      sub <- dplyr::filter(st_data, yday %in% window_days)

      dplyr::as_tibble(
        station_code = st,
        yday         = d,
        tmax_p   = if (sum(!is.na(sub$tmax))     >= 10L)
                     quantile(sub$tmax,     percentile / 100, na.rm = TRUE)
                   else NA_real_,
        tmin_p   = if (sum(!is.na(sub$tmin))     >= 10L)
                     quantile(sub$tmin,     percentile / 100, na.rm = TRUE)
                   else NA_real_,
        tmean_hist   = mean(sub$tmean,  na.rm = TRUE),
        tmax_hist    = mean(sub$tmax,   na.rm = TRUE),
        utci_p   = if ("utci_max" %in% names(sub) && sum(!is.na(sub$utci_max)) >= 10L)
                     quantile(sub$utci_max, percentile / 100, na.rm = TRUE)
                   else NA_real_,
        wbgt_p   = if ("wbgt_max" %in% names(sub) && sum(!is.na(sub$wbgt_max)) >= 10L)
                     quantile(sub$wbgt_max, percentile / 100, na.rm = TRUE)
                   else NA_real_,
        hi_p     = if ("hi_max" %in% names(sub) && sum(!is.na(sub$hi_max)) >= 10L)
                     quantile(sub$hi_max, percentile / 100, na.rm = TRUE)
                   else NA_real_
      )
    }) |> list_rbind()
  }) |> list_rbind()
}


# =============================================================================
# 4. METHOD DETECTION
# =============================================================================

#' @keywords internal
#' @noRd
.hw_apply_all_methods <- function(daily, method, min_duration, percentile) {

  default_dur <- c(WHO = 3L, WMO = 5L, INMET = 5L, EHF = 3L,
                   UTCI = 3L, WBGT = 3L, HI = 3L)

  for (m in method) {
    dur <- if (!is.null(min_duration)) min_duration else default_dur[[m]]
    daily <- switch(m,
      WHO   = .hw_method_who(daily, dur),
      WMO   = .hw_method_wmo(daily, dur),
      INMET = .hw_method_inmet(daily, dur),
      EHF   = .hw_method_ehf(daily, dur),
      UTCI  = .hw_method_utci(daily, dur),
      WBGT  = .hw_method_wbgt(daily, dur),
      HI    = .hw_method_hi(daily, dur),
      daily
    )
  }

  hw_cols_present <- intersect(paste0("hw_", tolower(method)), names(daily))
  if (length(hw_cols_present) > 0L) {
    daily <- daily |>
      dplyr::mutate(
        hw_any = rowSums(dplyr::across(dplyr::all_of(hw_cols_present)), na.rm = TRUE) > 0
      )
  }
  daily
}

#' @keywords internal
#' @noRd
.hw_method_who <- function(daily, min_dur) {
  daily |>
    dplyr::mutate(
      above_who = tmax > tmax_p & !is.na(tmax) & !is.na(tmax_p),
      hw_who    = .hw_consecutive_flag(above_who, min_dur),
      .by = station_code
    ) |>
    dplyr::select(-above_who)
}

#' @keywords internal
#' @noRd
.hw_method_wmo <- function(daily, min_dur) {
  daily |>
    dplyr::mutate(
      above_wmo = tmax > tmax_p & tmin > tmin_p &
                  !is.na(tmax) & !is.na(tmin) & !is.na(tmax_p) & !is.na(tmin_p),
      hw_wmo    = .hw_consecutive_flag(above_wmo, min_dur),
      .by = station_code
    ) |>
    dplyr::select(-above_wmo)
}

#' @keywords internal
#' @noRd
.hw_method_inmet <- function(daily, min_dur) {
  daily |>
    dplyr::mutate(
      above_inmet = tmax > (tmax_hist + 5) & !is.na(tmax) & !is.na(tmax_hist),
      hw_inmet    = .hw_consecutive_flag(above_inmet, min_dur),
      .by = station_code
    ) |>
    dplyr::select(-above_inmet)
}

#' @keywords internal
#' @noRd
.hw_method_ehf <- function(daily, min_dur) {
  daily |>
    dplyr::arrange(date_day, .by_group = FALSE) |>
    dplyr::mutate(
      t3          = slider::slide_dbl(tmean, mean, .before = 2L, .after = 0L, .complete = TRUE),
      t30         = slider::slide_dbl(tmean, mean, .before = 32L, .after = -3L, .complete = FALSE),
      t95         = quantile(tmean, 0.95, na.rm = TRUE),
      ehi_sig     = t3 - t95,
      ehi_acc     = t3 - t30,
      ehf         = ehi_sig * pmax(1, ehi_acc),
      above_ehf   = ehf > 0 & !is.na(ehf),
      hw_ehf_raw  = slider::slide_lgl(above_ehf, any, .before = 0L, .after = 2L, .complete = FALSE),
      hw_ehf      = .hw_consecutive_flag(hw_ehf_raw, min_dur),
      ehf_value   = ifelse(hw_ehf, ehf, NA_real_),
      .by = station_code
    ) |>
    dplyr::select(-t3, -t30, -t95, -ehi_sig, -ehi_acc, -above_ehf, -hw_ehf_raw)
}

#' @keywords internal
#' @noRd
.hw_method_utci <- function(daily, min_dur) {
  if (!"utci_max" %in% names(daily) || all(is.na(daily$utci_max))) {
    return(dplyr::mutate(daily, hw_utci = NA))
  }
  daily |>
    dplyr::mutate(
      above_utci = !is.na(utci_max) & !is.na(utci_p) & utci_max > utci_p,
      hw_utci    = .hw_consecutive_flag(above_utci, min_dur),
      .by = station_code
    ) |>
    dplyr::select(-above_utci)
}

#' @keywords internal
#' @noRd
.hw_method_wbgt <- function(daily, min_dur) {
  if (!"wbgt_max" %in% names(daily) || all(is.na(daily$wbgt_max))) {
    return(dplyr::mutate(daily, hw_wbgt = NA))
  }
  daily |>
    dplyr::mutate(
      above_wbgt = !is.na(wbgt_max) & !is.na(wbgt_p) & wbgt_max > wbgt_p,
      hw_wbgt    = .hw_consecutive_flag(above_wbgt, min_dur),
      .by = station_code
    ) |>
    dplyr::select(-above_wbgt)
}

#' @keywords internal
#' @noRd
.hw_method_hi <- function(daily, min_dur) {
  if (!"hi_max" %in% names(daily) || all(is.na(daily$hi_max))) {
    return(dplyr::mutate(daily, hw_hi = NA))
  }
  daily |>
    dplyr::mutate(
      above_hi = !is.na(hi_max) & !is.na(hi_p) & hi_max > hi_p,
      hw_hi    = .hw_consecutive_flag(above_hi, min_dur),
      .by = station_code
    ) |>
    dplyr::select(-above_hi)
}

#' @keywords internal
#' @noRd
.hw_consecutive_flag <- function(x, min_dur) {
  n      <- length(x)
  result <- logical(n)
  r      <- rle(x)
  ends   <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1L
  for (i in seq_along(r$values)) {
    if (isTRUE(r$values[i]) && r$lengths[i] >= min_dur) {
      result[starts[i]:ends[i]] <- TRUE
    }
  }
  result
}


# =============================================================================
# 5. EVENT EXTRACTION
# =============================================================================

#' @keywords internal
#' @noRd
.hw_extract_events <- function(daily, method) {

  hw_cols_present <- intersect(paste0("hw_", tolower(method)), names(daily))
  if (length(hw_cols_present) == 0L) return(dplyr::as_tibble())

  purrr::map(hw_cols_present, function(col) {
    m_name <- toupper(gsub("^hw_", "", col))

    temp_col <- switch(m_name,
      WHO = "tmax", WMO = "tmax", INMET = "tmax", EHF = "tmean",
      UTCI = "utci_max", WBGT = "wbgt_max", HI = "hi_max", "tmax"
    )
    ref_col <- switch(m_name,
      WHO = "tmax_p", WMO = "tmax_p", INMET = "tmax_hist", EHF = "tmean",
      UTCI = "utci_p", WBGT = "wbgt_p", HI = "hi_p", "tmax_p"
    )

    purrr::map(unique(daily$station_code), function(st) {
      st_data <- dplyr::filter(daily, station_code == st) |>
        dplyr::arrange(date_day)

      flag <- st_data[[col]]
      if (all(is.na(flag)) || !any(flag, na.rm = TRUE)) return(NULL)
      flag[is.na(flag)] <- FALSE

      r      <- rle(flag)
      ends   <- cumsum(r$lengths)
      starts <- ends - r$lengths + 1L

      purrr::map(which(r$values), function(i) {
        idx       <- starts[i]:ends[i]
        sub       <- st_data[idx, ]
        temp_vals <- if (temp_col %in% names(sub)) sub[[temp_col]] else rep(NA_real_, nrow(sub))
        ref_vals  <- if (ref_col  %in% names(sub)) sub[[ref_col]]  else rep(NA_real_, nrow(sub))
        anomaly   <- temp_vals - ref_vals

        ehf_peak_val <- NA_real_
        ehf_mean_val <- NA_real_
        if (m_name == "EHF" && "ehf_value" %in% names(sub)) {
          ehf_peak_val <- max(sub$ehf_value,  na.rm = TRUE)
          ehf_mean_val <- mean(sub$ehf_value, na.rm = TRUE)
        }

        dur <- as.integer(diff(range(sub$date_day)) + 1L)

        dplyr::as_tibble(
          station_code       = st,
          method             = m_name,
          start_date         = as.Date(min(sub$date_day), origin = "1970-01-01"),
          end_date           = as.Date(max(sub$date_day), origin = "1970-01-01"),
          duration_days      = dur,
          temp_mean          = mean(temp_vals, na.rm = TRUE),
          temp_peak          = max(temp_vals,  na.rm = TRUE),
          anomaly_mean       = mean(anomaly,   na.rm = TRUE),
          anomaly_cumulative = sum(anomaly,    na.rm = TRUE),
          severity_index     = dur * mean(anomaly, na.rm = TRUE),
          ehf_peak           = ehf_peak_val,
          ehf_mean           = ehf_mean_val,
          region             = if ("region"         %in% names(sub)) dplyr::first(sub$region)         else NA_character_,
          federal_unit       = if ("federal_unit"   %in% names(sub)) dplyr::first(sub$federal_unit)   else NA_character_,
          station_name       = if ("station_name"   %in% names(sub)) dplyr::first(sub$station_name)   else NA_character_,
          zona_climatica     = if ("zona_climatica" %in% names(sub)) dplyr::first(sub$zona_climatica) else NA_character_,
          latitude           = if ("latitude"       %in% names(sub)) dplyr::first(sub$latitude)       else NA_real_,
          longitude          = if ("longitude"      %in% names(sub)) dplyr::first(sub$longitude)      else NA_real_
        )
      }) |> list_rbind()
    }) |> list_rbind() |>
      (\(ev) {
        if (nrow(ev) == 0L) return(ev)
        ev <- ev |>
          dplyr::arrange(start_date) |>
          dplyr::mutate(event_id = paste0(station_code, "_", m_name, "_", dplyr::row_number()))

        if (m_name == "EHF") {
          all_pos <- unlist(lapply(unique(ev$station_code), function(st) {
            v <- dplyr::filter(daily, station_code == st)[["ehf_value"]]
            v[!is.na(v) & v > 0]
          }))
          if (length(all_pos) > 0L) {
            ehf85 <- quantile(all_pos, 0.85, na.rm = TRUE)
            ev <- ev |>
              dplyr::mutate(
                intensity_class = dplyr::case_when(
                  ehf_peak >= 3 * ehf85 ~ "Extreme (EHW)",
                  ehf_peak >= ehf85     ~ "Severe (SHW)",
                  ehf_peak > 0          ~ "Low Intensity (LIHW)",
                  .default              = NA_character_
                )
              )
          } else {
            ev <- dplyr::mutate(ev, intensity_class = NA_character_)
          }
        } else {
          ev <- dplyr::mutate(ev, intensity_class = NA_character_)
        }
        ev
      })()
  }) |>
    list_rbind() |>
    dplyr::arrange(station_code, method, start_date)
}


# =============================================================================
# 6. ANNUAL SUMMARY
# =============================================================================

#' @keywords internal
#' @noRd
.hw_build_summary <- function(events, daily, method) {

  if (nrow(events) == 0L) {
    return(dplyr::as_tibble(
      year = integer(), station_code = character(), method = character(),
      n_events = integer(), total_days_hw = integer(),
      mean_duration = double(), max_duration = integer(),
      mean_intensity = double(), max_intensity = double(),
      mean_anomaly = double(), severity_total = double()
    ))
  }

  events |>
    dplyr::mutate(year = lubridate::year(start_date)) |>
    dplyr::summarise(
      n_events       = dplyr::n(),
      total_days_hw  = sum(duration_days),
      mean_duration  = mean(duration_days),
      max_duration   = max(duration_days),
      mean_intensity = mean(temp_mean,    na.rm = TRUE),
      max_intensity  = max(temp_peak,     na.rm = TRUE),
      mean_anomaly   = mean(anomaly_mean, na.rm = TRUE),
      severity_total = sum(severity_index, na.rm = TRUE),
      .by = c(year, station_code, method, region, federal_unit, zona_climatica)
    ) |>
    dplyr::arrange(station_code, method, year)
}


# =============================================================================
# CONVENIENCE HELPERS
# =============================================================================

#' Extract heatwave events table from a climasus_hw result
#'
#' @param hw_result A `climasus_hw` list returned by
#'   `sus_climate_compute_heatwaves()`.
#' @param method_filter Character vector. If supplied, keep only these methods.
#' @return A tibble of heatwave events.
#' @export
hw_get_events <- function(hw_result, method_filter = NULL) {
  ev <- hw_result$events
  if (!is.null(method_filter)) ev <- dplyr::filter(ev, method %in% method_filter)
  ev
}

#' Count heatwave events by year, station, and method
#'
#' @param hw_result A `climasus_hw` list returned by
#'   `sus_climate_compute_heatwaves()`.
#' @return A tibble with columns year, station_code, station_name, method,
#'   n_events, total_days_hw, mean_duration.
#' @export
hw_count_by_year <- function(hw_result) {
  cols <- intersect(
    c("year", "station_code", "station_name", "method",
      "n_events", "total_days_hw", "mean_duration"),
    names(hw_result$summary)
  )
  dplyr::select(hw_result$summary, dplyr::all_of(cols))
}

#' Return daily rows where at least one heatwave method is active
#'
#' @param hw_result A `climasus_hw` list returned by
#'   `sus_climate_compute_heatwaves()`.
#' @return A filtered tibble from `hw_result$daily`.
#' @export
hw_active_days <- function(hw_result) {
  hw_result$daily |>
    dplyr::filter(hw_any == TRUE) |>
    dplyr::select(station_code, date_day, tmax, tmean, dplyr::starts_with("hw_"))
}
