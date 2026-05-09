# =============================================================================
# sus_climate_compute_heatwaves.R
# Detection of heatwaves using multiple internationally recognised methodologies
# Compatible with output of sus_climate_compute_indicators (INMET hourly data)
# =============================================================================

#' Detect heatwaves using multiple standard methodologies
#'
#' Applies several heatwave definitions to daily temperature and thermal comfort
#' indices. The function expects an hourly data frame as produced by
#' `sus_climate_compute_indicators()` or a similarly structured INMET dataset.
#' It aggregates to daily scale, computes historical thresholds from a
#' user-defined baseline period, identifies heatwave days with each requested
#' method, extracts discrete events, and returns a list of event, daily, and
#' annual summaries.
#'
#' @section Implemented methods:
#'
#' **1. WHO (World Health Organization / WMO Guidance)**
#' Condition: `Tmax > P90(Tmax)` for N consecutive days (default N=3)
#' where P90(Tmax) is the 90th percentile of daily maximum temperature,
#' smoothed over a 15-day rolling window by calendar day-of-year.
#' Reference: WMO & WHO (2015).
#'
#' **2. WMO (World Meteorological Organization)**
#' Condition: `Tmax > P90(Tmax) AND Tmin > P90(Tmin)` for N consecutive days
#' (default N=5), where P90 denotes the 90th percentile of each temperature variable.
#' Double-threshold approach ensures both daytime and nighttime extremes.
#' Reference: Perkins & Alexander (2013).
#'
#' **3. INMET (Brazilian National Institute of Meteorology)**
#' Condition: `Tmax > mean(Tmax_hist) + 5 deg C` for N consecutive days
#' (default N=5), where mean(Tmax_hist) is the climatological mean of daily
#' maximum temperature computed over the baseline period (typically 1961-1990
#' or user-specified). Designed for Brazilian tropical/subtropical contexts.
#' Reference: INMET (2009); MCTI/Gov.br (2025).
#'
#' **4. EHF (Excess Heat Factor)**
#' Excess Heat Factor calculated as: EHF = EHI_sig * max(1, EHI_acc)
#' where EHI_sig = T3 - P95(Tmean) (physiological signal) and
#' EHI_acc = T3 - T30_mean (acclimatization index).
#' T3 = 3-day centered mean temperature; T30_mean = 30-day prior mean.
#' Days with EHF > 0 are flagged; intensity classified by the 85th percentile
#' of positive EHF values (Low, Severe, Extreme).
#' Reference: Nairn & Fawcett (2015); Nairn et al. (2018).
#'
#' **5. UTCI (Universal Thermal Climate Index)**
#' Condition: `UTCImax > P90(UTCImax)` for N consecutive days (default N=3),
#' where P90 is the 90th percentile smoothed over a 15-day window.
#' UTCI integrates air temperature, humidity, wind speed, and radiation
#' to estimate physiological heat stress (equivalent temperature).
#' Reference: Broede et al. (2012).
#'
#' **6. WBGT (Wet Bulb Globe Temperature)**
#' Condition: `WBGTmax > P90(WBGTmax)` for N consecutive days (default N=3).
#' WBGT combines temperature, humidity, wind, and radiation into a single
#' index used widely in occupational health and sports medicine.
#' Reference: Liljegren et al. (2008); ISO 7243:2017.
#'
#' **7. HI (Heat Index)**
#' Condition: `HImax > P90(HImax)` for N consecutive days (default N=3),
#' where P90 is the 90th percentile smoothed over a 15-day window.
#' Heat Index (apparent temperature) combines temperature and humidity
#' to estimate how hot it feels to human perception.
#' Reference: Steadman (1979a, 1984).
#'
#' @param data A data frame with hourly observations from `sus_climate_inmet()`
#'   or `sus_climate_compute_indicators()`. Must contain columns:
#'   - `date` (POSIX datetime)
#'   - `station_code` (character)
#'   - `tair_dry_bulb_c`, `tair_max_c`, `tair_min_c` (numeric, degrees Celsius)
#'   - `utci_c`, `wbgt_c`, `hi_c` (numeric, for corresponding methods)
#'   Pre-fill missing values using `sus_climate_fill_inmet()` before calling
#'   this function.
#' @param method Character vector. One or more of `"WHO"`, `"WMO"`, `"INMET"`,
#'   `"EHF"`, `"UTCI"`, `"WBGT"`, `"HI"`. Use `"all"` to run all available
#'   methods. Methods requiring missing thermal indices will be ignored with a
#'   warning.
#' @param baseline_start Date or character (e.g., `"1981-01-01"`). Start of the
#'   reference period for percentile-based thresholds. `NULL` uses the entire
#'   data range.
#' @param baseline_end Date or character. End of the reference period.
#'   `NULL` uses the entire data range.
#' @param percentile Numeric (default `90`). Percentile for the threshold
#'   calculation (applied to methods WHO, WMO, UTCI, WBGT, HI).
#' @param min_duration Integer or `NULL`. Minimum consecutive days for a
#'   heatwave. `NULL` uses method-specific defaults: WHO=3, WMO=5,
#'   INMET=5, EHF=3, UTCI=3, WBGT=3, HI=3.
#' @param lang Character. Language for progress messages: `"en"`, `"pt"`,
#'   or `"es"` (default `"en"`).
#' @param verbose Logical. Show progress steps (default `TRUE`).
#'
#' @importFrom dplyr mutate select filter group_by summarise arrange left_join ungroup across
#' @importFrom tidyr everything
#' @importFrom cli cli_progress_step cli_warn cli_abort cli_alert_success cli_alert_info
#' @importFrom purrr map_dfr map_lgl
#' @importFrom lubridate yday year
#' @importFrom slider slide_dbl slide_lgl
#' @importFrom rlang .data
#'
#' @return A list with three data frames:
#'   \describe{
#'     \item{`events`}{One row per detected heatwave event. Columns: event_id,
#'       station_code, method, start_date, end_date, duration_days, temp_mean,
#'       temp_peak, anomaly_mean, anomaly_cumulative, severity_index,
#'       ehf_peak (EHF only), ehf_mean (EHF only), intensity_class (EHF only),
#'       and site metadata.}
#'     \item{`daily`}{The daily aggregated data with logical flags for each
#'       method (columns `hw_<method>`), plus an `hw_any` flag indicating
#'       whether any method detected a heatwave that day.}
#'     \item{`summary`}{Annual summary by station and method: year,
#'       n_events, total_days_hw, mean_duration, max_duration,
#'       mean_intensity, max_intensity, mean_anomaly, severity_total.}
#'   }
#'
#' @references
#'
#' **General heatwave concepts and percentile framework**
#'
#' - Perkins, S.E. (2015). A review on the scientific understanding of
#'   heatwaves--their measurement, driving mechanisms, and changes at the global
#'   scale. *Atmospheric Research*, 164-165, 242-267.
#'   \doi{10.1016/j.atmosres.2015.05.009}
#' - Perkins, S.E. & Alexander, L.V. (2013). On the measurement of heat waves.
#'   *Journal of Climate*, 26(13), 4500-4517.
#' - Fischer, E.M. & Schaer, C. (2010). Consistent geographical patterns of
#'   changes in high-impact European heatwaves. *Nature Geoscience*, 3,
#'   398-403.
#'
#' **WHO / WMO methods**
#'
#' - WMO & WHO (2015). *Heatwaves and Health: Guidance on Warning-System
#'   Development*. World Meteorological Organization, Geneva.
#' - WMO (2021). *Guidelines on the Definition and Monitoring of Extreme Weather
#'   and Climate Events*. World Meteorological Organization.
#'
#' **INMET method**
#'
#' - INMET - Instituto Nacional de Meteorologia (2009). *Normais Climatologicas
#'   do Brasil 1961-1990*. Brasilia: MAPA/INMET.
#' - MCTI/Gov.br (2025). Heatwave reports for Brazil (online).
#'
#' **EHF (Excess Heat Factor)**
#'
#' - Nairn, J.R. & Fawcett, R.J.B. (2015). The Excess Heat Factor: A metric for
#'   heatwave intensity and its use in classifying heatwave severity.
#'   *International Journal of Environmental Research and Public Health*,
#'   12(1), 227-253. \doi{10.3390/ijerph120100227}
#' - Nairn, J.R. et al. (2018). Performance of Excess Heat Factor Severity as a
#'   global heatwave health impact index. *International Journal of
#'   Environmental Research and Public Health*, 15(11), 2494.
#'
#' **UTCI (Universal Thermal Climate Index)**
#'
#' - Broede, P., Fiala, D., Blazejczyk, K., Holmer, I., Jendritzky, G.,
#'   Kampmann, B., Tinz, B. & Havenith, G. (2012). Deriving the operational
#'   procedure for the Universal Thermal Climate Index (UTCI). *International
#'   Journal of Biometeorology*, 56(3), 481-494.
#'   \doi{10.1007/s00484-011-0454-1}
#'
#' **WBGT (Wet Bulb Globe Temperature)**
#'
#' - Liljegren, J.C., Carhart, R.A., Lawday, P., Tschopp, S. & Sharp, R.
#'   (2008). Modeling the wet bulb globe temperature using standard
#'   meteorological measurements. *Journal of Occupational and Environmental
#'   Hygiene*, 5(10), 645-655.
#' - ISO 7243:2017. *Ergonomics of the thermal environment -- Assessment of
#'   heat stress using the WBGT index*. International Organization for
#'   Standardization.
#'
#' **HI (Heat Index)**
#'
#' - Steadman, R.G. (1979a). The assessment of sultriness. Part I: A
#'   temperature-humidity index based on human physiology and clothing science.
#'   *Journal of Applied Meteorology*, 18(7), 861-873.
#' - Steadman, R.G. (1984). A universal scale of apparent temperature. *Journal
#'   of Applied Meteorology and Climatology*, 23(12), 1674-1687.
#'
#' **Brazil health context (SUS)**
#'
#' - Fiocruz (2025/2026). Excess cardiovascular mortality associated with
#'   extreme heatwaves in Southeast Brazil, 2014-2023. *Cadernos de Saude
#'   Publica*.
#' - Observatorio do Clima. Excess mortality during heatwaves in Brazilian
#'   urban areas (2000-2018).
#'
#' @examples
#' \dontrun{
#'   # After computing hourly indicators:
#'   # df_ind <- sus_climate_compute_indicators(...)
#'
#'   hw_result <- sus_climate_compute_heatwaves(
#'     data          = df_ind,
#'     method        = c("WHO", "INMET", "UTCI"),
#'     baseline_start = "2000-01-01",
#'     baseline_end   = "2020-12-31"
#'   )
#'
#'   # Extract event list
#'   head(hw_result$events)
#'
#'   # Annual summary table
#'   hw_result$summary
#'
#'   # Days with at least one active heatwave method
#'   head(hw_result$daily[hw_result$daily$hw_any, ])
#' }
#'
#' @seealso Related climate functions: \code{\link{sus_climate_compute_indicators}},
#'   \code{\link{sus_climate_inmet}}
#'
#' @export
sus_climate_compute_heatwaves <- function(
    data,
    method         = c("WHO", "WMO", "INMET", "EHF", "UTCI", "WBGT", "HI"),
    baseline_start = NULL,
    baseline_end   = NULL,
    percentile     = 90,
    min_duration   = NULL,
    lang           = c("en", "pt", "es"),
    verbose        = TRUE
) {
  
   # ===========================================================
    # PACKAGE CHECK + SEED
    # ===========================================================
    rlang::check_installed(
      c("slider", "jsonlite"),
      reason = "to run sus_climate_compute_heatwaves()"
    )
  
  lang <- match.arg(lang)

  # Expandir "all"
  all_methods <- c("WHO", "WMO", "INMET", "EHF", "UTCI", "WBGT", "HI")
  if (length(method) == 1 && method == "all") method <- all_methods
  method <- match.arg(method, all_methods, several.ok = TRUE)

  # --- 1. VALIDATION ---
  if (verbose) {
    msg <- switch(lang,
      en = "Validating inputs...",
      pt = "Validando inputs...",
      es = "Validando entradas..."
    )
    cli::cli_progress_step(msg)
  }
  data <- .hw_validate(data, method, lang, verbose)

  # --- 2. HOURLY TO DAILY AGGREGATION ---
  if (verbose) {
    msg <- switch(lang,
      en = "Aggregating hourly data to daily scale...",
      pt = "Agregando dados horarios para escala diaria...",
      es = "Agregando datos horarios a escala diaria..."
    )
    cli::cli_progress_step(msg)
  }
  daily <- .hw_aggregate_daily(data)

  # --- 3. BASELINE ---
  if (verbose) {
    msg <- switch(lang,
      en = "Calculating historical thresholds (baseline)...",
      pt = "Calculando limiares historicos (baseline)...",
      es = "Calculando umbrales historicos (linea base)..."
    )
    cli::cli_progress_step(msg)
  }

  if (!base::is.null(baseline_start)) baseline_start <- as.Date(baseline_start)
  if (!base::is.null(baseline_end))   baseline_end   <- as.Date(baseline_end)

  baseline <- .hw_compute_baseline(
    daily, baseline_start, baseline_end, percentile, verbose
  )

  # Unir limiares com a serie diaria
  daily <- daily |>
    dplyr::left_join(baseline, by = c("station_code", "yday"))

  # --- 4. DETECTION BY METHOD ---
  if (verbose) {
    msg <- switch(lang,
      en = "Detecting heatwaves by method...",
      pt = "Detectando ondas de calor por metodo...",
      es = "Detectando olas de calor por metodo..."
    )
    cli::cli_progress_step(msg)
  }

  daily <- .hw_apply_all_methods(daily, method, min_duration, percentile)

  # --- 5. EVENT EXTRACTION ---
  if (verbose) {
    msg <- switch(lang,
      en = "Extracting and classifying events...",
      pt = "Extraindo e classificando eventos...",
      es = "Extrayendo y clasificando eventos..."
    )
    cli::cli_progress_step(msg)
  }

  events <- .hw_extract_events(daily, method)

  # --- 6. SUMMARY ---
  if (verbose) {
    msg <- switch(lang,
      en = "Generating annual summary...",
      pt = "Gerando resumo anual...",
      es = "Generando resumen anual..."
    )
    cli::cli_progress_step(msg)
  }

  summary_tbl <- .hw_build_summary(events, daily, method)

  if (verbose) {
    msg <- switch(lang,
      en = "Detection completed: {nrow(events)} event(s) found.",
      pt = "Deteccao concluida: {nrow(events)} evento(s) encontrado(s).",
      es = "Deteccion completada: {nrow(events)} evento(s) encontrado(s)."
    )
    cli::cli_alert_success(msg)
  }

  list(
    events  = events,
    daily   = daily,
    summary = summary_tbl
  )
}


# =============================================================================
# 1. VALIDATION
# =============================================================================

#' @keywords internal
#' @noRd
.hw_validate <- function(data, method, lang, verbose) {

  # Check required columns (fixed names from climasus4r pipeline)
  required <- c("date", "station_code")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    msg <- switch(lang,
      en = "Missing required columns: {paste(missing_cols, collapse=', ')}",
      pt = "Colunas obrigatorias ausentes: {paste(missing_cols, collapse=', ')}",
      es = "Faltan columnas requeridas: {paste(missing_cols, collapse=', ')}"
    )
    cli::cli_abort(msg)
  }

  # Needs at least one temperature column
  temp_cols <- intersect(
    c("tair_dry_bulb_c", "tair_max_c", "tair_min_c", "tmean",
      "utci_c", "wbgt_c", "hi_c"),
    names(data)
  )
  if (length(temp_cols) == 0) {
    msg <- switch(lang,
      en = "No temperature column found. Expected at least one of: tair_dry_bulb_c, tair_max_c, tair_min_c, utci_c, wbgt_c, hi_c",
      pt = "Nenhuma coluna de temperatura encontrada. Esperava-se ao menos uma de: tair_dry_bulb_c, tair_max_c, tair_min_c, utci_c, wbgt_c, hi_c",
      es = "No se encontro ninguna columna de temperatura. Se esperaba al menos una de: tair_dry_bulb_c, tair_max_c, tair_min_c, utci_c, wbgt_c, hi_c"
    )
    cli::cli_abort(msg)
  }

  # Check if UTCI/WBGT/HI are available when requested
  if ("UTCI" %in% method && !"utci_c" %in% names(data)) {
    msg <- switch(lang, en = "Method 'UTCI' requested but column 'utci_c' not found. Method will be ignored.", pt = "Metodo 'UTCI' solicitado mas coluna 'utci_c' nao encontrada. Metodo sera ignorado.", es = "Metodo 'UTCI' solicitado pero no se encontro la columna 'utci_c'. El metodo sera ignorado.")
    cli::cli_warn(msg)
  }
  if ("WBGT" %in% method && !"wbgt_c" %in% names(data)) {
    msg <- switch(lang, en = "Method 'WBGT' requested but column 'wbgt_c' not found. Method will be ignored.", pt = "Metodo 'WBGT' solicitado mas coluna 'wbgt_c' nao encontrada. Metodo sera ignorado.", es = "Metodo 'WBGT' solicitado pero no se encontro la columna 'wbgt_c'. El metodo sera ignorado.")
    cli::cli_warn(msg)
  }
  if ("HI"   %in% method && !"hi_c"   %in% names(data)) {
    msg <- switch(lang, en = "Method 'HI' requested but column 'hi_c' not found. Method will be ignored.", pt = "Metodo 'HI' solicitado mas coluna 'hi_c' nao encontrada. Metodo sera ignorado.", es = "Metodo 'HI' solicitado pero no se encontro la columna 'hi_c'. El metodo sera ignorado.")
    cli::cli_warn(msg)
  }

  # Converter coluna de data para Date/dttm
  data[["date"]] <- as.POSIXct(data[["date"]])

  # Verificar NAs nas colunas de temperatura
  # Pre-fill using sus_climate_fill_inmet() before calling this function
  temp_cols_present <- intersect(
    c("tair_dry_bulb_c", "tair_max_c", "tair_min_c", "utci_c", "wbgt_c", "hi_c"),
    names(data)
  )
  n_na <- sum(is.na(data[temp_cols_present]))

  if (n_na > 0) {
    msg <- switch(lang,
      en = "{n_na} NA value(s) in temperature columns (use sus_climate_fill_inmet() to pre-fill).",
      pt = "{n_na} valor(es) NA em colunas de temperatura (use sus_climate_fill_inmet() antes).",
      es = "{n_na} valor(es) NA en columnas de temperatura (use sus_climate_fill_inmet() primero)."
    )
    cli::cli_warn(msg)
  }

  # Check minimum period (at least 1 year)
  date_range <- range(as.Date(data[["date"]]), na.rm = TRUE)
  
  # Check if date_range is valid (not Inf/-Inf)
  if (all(is.finite(date_range))) {
    n_days <- as.numeric(diff(date_range))
    if (n_days < 365) {
      msg <- switch(lang,
        en = "Short time series ({n_days} days). Results may not be robust.",
        pt = "Serie temporal curta ({n_days} dias). Resultados podem ser pouco robustos.",
        es = "Serie temporal corta ({n_days} dias). Los resultados pueden no ser robustos."
      )
      cli::cli_warn(msg)
    }
  } else {
    msg <- switch(lang,
      en = "Invalid date range. Check your date column.",
      pt = "Intervalo de datas invalido. Verifique sua coluna de datas.",
      es = "Rango de fechas invalido. Verifique su columna de fechas."
    )
    cli::cli_abort(msg)
  }

  data
}


# =============================================================================
# 2. AGREGACAO DIARIA
# =============================================================================

#' @keywords internal
#' @noRd
.hw_aggregate_daily <- function(data) {

  # Detectar colunas disponiveis
  has <- function(col) col %in% names(data)

  data |>
    dplyr::mutate(date_day = as.Date(date)) |>
    dplyr::group_by(station_code = station_code, date_day) |>
    dplyr::summarise(
      # --- Temperatura do ar ---
      tmax  = if (has("tair_max_c"))     max(tair_max_c,     na.rm = TRUE)
              else if (has("tair_dry_bulb_c")) max(tair_dry_bulb_c, na.rm = TRUE)
              else NA_real_,

      tmin  = if (has("tair_min_c"))     min(tair_min_c,     na.rm = TRUE)
              else if (has("tair_dry_bulb_c")) min(tair_dry_bulb_c, na.rm = TRUE)
              else NA_real_,

      tmean = if (has("tair_dry_bulb_c")) mean(tair_dry_bulb_c, na.rm = TRUE)
              else if (has("tair_max_c") && has("tair_min_c"))
                    mean(c(mean(tair_max_c, na.rm = TRUE), mean(tair_min_c, na.rm = TRUE)))
              else NA_real_,

      # --- indices de conforto termico ---
      utci_max  = if (has("utci_c")) max(utci_c,  na.rm = TRUE) else NA_real_,
      utci_mean = if (has("utci_c")) mean(utci_c, na.rm = TRUE) else NA_real_,
      wbgt_max  = if (has("wbgt_c")) max(wbgt_c,  na.rm = TRUE) else NA_real_,
      wbgt_mean = if (has("wbgt_c")) mean(wbgt_c, na.rm = TRUE) else NA_real_,
      hi_max    = if (has("hi_c"))   max(hi_c,    na.rm = TRUE) else NA_real_,
      hi_mean   = if (has("hi_c"))   mean(hi_c,   na.rm = TRUE) else NA_real_,
      pet_max   = if (has("pet_c"))  max(pet_c,   na.rm = TRUE) else NA_real_,

      # --- Horas com flag de estresse extremo ---
      n_hours_extreme_wbgt = if (has("wbgt_c_flag_extreme")) sum(wbgt_c_flag_extreme, na.rm = TRUE) else NA_integer_,
      n_hours_extreme_utci = if (has("utci_c_flag_extreme")) sum(utci_c_flag_extreme, na.rm = TRUE) else NA_integer_,
      n_hours_extreme_hi   = if (has("hi_c_flag_extreme"))   sum(hi_c_flag_extreme,   na.rm = TRUE) else NA_integer_,

      # --- Metadados (primeiro valor do dia) ---
      region         = if (has("region"))         dplyr::first(region)         else NA_character_,
      federal_unit   = if (has("federal_unit"))   first(federal_unit)   else NA_character_,
      station_name   = if (has("station_name"))   first(station_name)   else NA_character_,
      zona_climatica = if (has("zona_climatica")) first(zona_climatica) else NA_character_,
      latitude       = if (has("latitude"))       dplyr::first(latitude)       else NA_real_,
      longitude      = if (has("longitude"))      dplyr::first(longitude)      else NA_real_,

      .groups = "drop"
    ) |>
    # Substituir -Inf / Inf gerados por max/min em vetores todos-NA
    dplyr::mutate(across(where(is.double), ~ ifelse(is.infinite(.x), NA_real_, .x))) |>
    dplyr::mutate(date_day = as.Date(date_day, origin = "1970-01-01")) |>
    dplyr::arrange(station_code, date_day) |>
    dplyr::mutate(yday = lubridate::yday(date_day)) |>
    # Ensure we dont have empty groups that could cause issues later
    dplyr::filter(!is.na(date_day))
}


# =============================================================================
# 3. BASELINE E LIMIARES
# =============================================================================

#' @keywords internal
#' @noRd
.hw_compute_baseline <- function(daily, baseline_start, baseline_end, percentile, verbose) {

  # Filtrar periodo de referencia
  ref <- daily
  if (!is.null(baseline_start)) ref <- filter(ref, date_day >= baseline_start)
  if (!is.null(baseline_end))   ref <- filter(ref, date_day <= baseline_end)

  # Check if ref has data
  if (nrow(ref) == 0) {
    cli_abort("No data available in the specified baseline period ({baseline_start} to {baseline_end}).")
  }

  n_years <- dplyr::n_distinct(lubridate::year(ref$date_day))
  if (verbose) cli::cli_alert_info("Periodo baseline: {min(ref$date_day, na.rm = TRUE)} a {max(ref$date_day, na.rm = TRUE)} ({n_years} anos)")
  if (n_years < 10)
    cli::cli_warn("Periodo baseline curto ({n_years} anos). Recomenda-se ao menos 20 anos para percentis robustos.")

  # Calcular percentis com janela deslizante de mais ou menos 15 dias (suavizacao)
  # para cada dia-do-ano e por estacao
  station_codes <- unique(ref$station_code)

  purrr::map_dfr(station_codes, function(st) {
    st_data <- filter(ref, station_code == st)

    purrr::map_dfr(1:366, function(d) {
      # Janela de mais ou menos 15 dias em torno do dia d (circularmente no ano)
      window_days <- ((d - 15):(d + 15) - 1) %% 366 + 1
      sub <- filter(st_data, yday %in% window_days)

      tibble(
        station_code    = st,
        yday            = d,
        tmax_p          = if (sum(!is.na(sub$tmax)) >= 10)
                            quantile(sub$tmax, percentile / 100, na.rm = TRUE)
                          else NA_real_,
        tmin_p          = if (sum(!is.na(sub$tmin)) >= 10)
                            quantile(sub$tmin, percentile / 100, na.rm = TRUE)
                          else NA_real_,
        tmean_hist      = mean(sub$tmean, na.rm = TRUE),
        tmax_hist       = mean(sub$tmax, na.rm = TRUE),
        utci_p          = if ("utci_max" %in% names(sub) && sum(!is.na(sub$utci_max)) >= 10)
                            quantile(sub$utci_max, percentile / 100, na.rm = TRUE)
                          else NA_real_,
        wbgt_p          = if ("wbgt_max" %in% names(sub) && sum(!is.na(sub$wbgt_max)) >= 10)
                            quantile(sub$wbgt_max, percentile / 100, na.rm = TRUE)
                          else NA_real_,
        hi_p            = if ("hi_max"   %in% names(sub) && sum(!is.na(sub$hi_max))   >= 10)
                            quantile(sub$hi_max, percentile / 100, na.rm = TRUE)
                          else NA_real_
      )
    })
  })
}


# =============================================================================
# 4. DETECCAO POR METODO
# =============================================================================

#' @keywords internal
#' @noRd
.hw_apply_all_methods <- function(daily, method, min_duration, percentile) {

  # Duracao minima padrao por metodo
  default_dur <- c(
    WHO       = 3,
    WMO       = 5,
    INMET     = 5,
    EHF       = 3,
    UTCI      = 3,
    WBGT      = 3,
    HI        = 3
  )

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
      daily  # fallback
    )
  }

  # Flag combinado: qualquer metodo detectou onda de calor?
  hw_cols <- paste0("hw_", tolower(method))
  hw_cols_present <- intersect(hw_cols, names(daily))

  if (length(hw_cols_present) > 0) {
    daily <- daily |>
      dplyr::mutate(hw_any = rowSums(across(all_of(hw_cols_present)), na.rm = TRUE) > 0)
  }

  daily
}

# --- WHO: tmax > p90, >= N dias consecutivos ---
#' @keywords internal
#' @noRd
.hw_method_who <- function(daily, min_dur) {
  daily |>
    dplyr::group_by(station_code) |>
    dplyr::mutate(
      above_who  = tmax > tmax_p & !is.na(tmax) & !is.na(tmax_p),
      hw_who     = .hw_consecutive_flag(above_who, min_dur)
    ) |>
    dplyr::select(-above_who) |>
    dplyr::ungroup()
}

# --- WMO: tmax > p90 E tmin > p90, >= N dias consecutivos ---
#' @keywords internal
#' @noRd
.hw_method_wmo <- function(daily, min_dur) {
  daily |>
    dplyr::group_by(station_code) |>
    dplyr::mutate(
      above_wmo = tmax > tmax_p & tmin > tmin_p &
                  !is.na(tmax) & !is.na(tmin) &
                  !is.na(tmax_p) & !is.na(tmin_p),
      hw_wmo    = .hw_consecutive_flag(above_wmo, min_dur)
    ) |>
    dplyr::select(-above_wmo) |>
    dplyr::ungroup()
}

# --- INMET: tmax > maximo historico medio + 5 graus C, >= N dias consecutivos ---
#' @keywords internal
#' @noRd
# Based on INMET (2009) and MCTI (2025): heatwave when daily max temp exceeds
# the climatological mean maximum temperature by at least 5 graus C for N+ consecutive days
.hw_method_inmet <- function(daily, min_dur) {
  daily |>
    dplyr::group_by(station_code) |>
    dplyr::mutate(
      above_inmet = tmax > (tmax_hist + 5) &
                    !is.na(tmax) & !is.na(tmax_hist),
      hw_inmet    = .hw_consecutive_flag(above_inmet, min_dur)
    ) |>
    dplyr::select(-above_inmet) |>
    dplyr::ungroup()
}

# --- EHF (Excess Heat Factor) ---
# EHF = EHI_sig x max(1, EHI_acc)
# EHI_sig = (T3 - T95) where T3 = 3 - day mean, T95 = 95th percentile of Tmean
# EHI_acc = (T3 - T30) where T30 = mean of previous 30 days
#' @keywords internal
#' @noRd
.hw_method_ehf <- function(daily, min_dur) {
  daily |>
    dplyr::group_by(station_code) |>
    dplyr::arrange(date_day, .by_group = TRUE) |>
    dplyr::mutate(
      # 3-day moving average (current day + 2 previous)
      t3   = slider::slide_dbl(tmean, mean, .before = 2, .after = 0, .complete = TRUE),
      # 30-day moving average (excluding the 3 most recent days)
      t30  = slider::slide_dbl(tmean, mean, .before = 32, .after = -3, .complete = FALSE),
      # T95: 95th percentile of Tmean for the entire series
      t95  = quantile(tmean, 0.95, na.rm = TRUE),
      
      # EHI_sig (Significance): How much the 3-day temp is above the climatological limit
      ehi_sig = t3 - t95,
      
      # EHI_acc (Acclimatization): How much the 3-day temp is above the recent 30-day mean
      ehi_acc = t3 - t30,
      
      # EHF Final
      ehf  = ehi_sig * pmax(1, ehi_acc),
      
      # EHF > 0 indicates heatwave conditions
      above_ehf = ehf > 0 & !is.na(ehf),
      
      # If a 3-day period has EHF > 0, all 3 days are considered HW days
      # We use a rolling max to flag the current day and the 2 previous days
      hw_ehf_raw = slider::slide_lgl(above_ehf, any, .before = 0, .after = 2, .complete = FALSE),
      
      # Apply minimum duration if specified (default for EHF is usually 3, which is inherent in the T3 calculation)
      hw_ehf = .hw_consecutive_flag(hw_ehf_raw, min_dur),
      
      # Keep EHF value for intensity classification later
      ehf_value = ifelse(hw_ehf, ehf, NA_real_)
    ) |>
    dplyr::select(-t3, -t30, -ehi_sig, -t95, -ehi_acc, -above_ehf, -hw_ehf_raw) |>
    dplyr::ungroup()
}

# --- UTCI: utci_max > percentil 90, >= N dias consecutivos ---
# Estresse moderado >= 26 graus C, forte >= 32 dgreeC, muito forte >= 38 degreeC
#' @keywords internal
#' @noRd
.hw_method_utci <- function(daily, min_dur) {
  if (!"utci_max" %in% names(daily) || all(is.na(daily$utci_max))) {
    return(mutate(daily, hw_utci = NA))
  }
  daily |>
    dplyr::group_by(station_code) |>
    dplyr::mutate(
      above_utci = !is.na(utci_max) & !is.na(utci_p) & utci_max > utci_p,
      hw_utci    = .hw_consecutive_flag(above_utci, min_dur)
    ) |>
    dplyr::select(-above_utci) |>
    dplyr::ungroup()
}

# --- WBGT: wbgt_max > percentil 90, >= N dias consecutivos ---
# Risco saude: > 28 degree C moderado, > 32 degree C alto
#' @keywords internal
#' @noRd
.hw_method_wbgt <- function(daily, min_dur) {
  if (!"wbgt_max" %in% names(daily) || all(is.na(daily$wbgt_max))) {
    return(mutate(daily, hw_wbgt = NA))
  }
  daily |>
    dplyr::group_by(station_code) |>
    dplyr::mutate(
      above_wbgt = !is.na(wbgt_max) & !is.na(wbgt_p) & wbgt_max > wbgt_p,
      hw_wbgt    = .hw_consecutive_flag(above_wbgt, min_dur)
    ) |>
    dplyr::select(-above_wbgt) |>
    dplyr::ungroup()
}

# --- HI (Heat Index): hi_max > percentil 90, >= N dias consecutivos ---
# Perigo: > 39 degreeC, Perigo extremo: > 51 degree C
#' @keywords internal
#' @noRd
.hw_method_hi <- function(daily, min_dur) {
  if (!"hi_max" %in% names(daily) || all(is.na(daily$hi_max))) {
    return(mutate(daily, hw_hi = NA))
  }
  daily |>
    dplyr::group_by(station_code) |>
    dplyr::mutate(
      above_hi = !is.na(hi_max) & !is.na(hi_p) & hi_max > hi_p,
      hw_hi    = .hw_consecutive_flag(above_hi, min_dur)
    ) |>
    dplyr::select(-above_hi) |>
    dplyr::ungroup()
}

# --- Helper: detectar sequencias consecutivas >= min_dur ---
# Retorna vetor logico TRUE nos dias que fazem parte de uma onda de calor
#' @keywords internal
#' @noRd
.hw_consecutive_flag <- function(x, min_dur) {
  n <- length(x)
  result <- logical(n)

  # Usar rle para encontrar runs de TRUE
  r <- rle(x)
  ends <- cumsum(r$lengths)
  starts <- ends - r$lengths + 1

  for (i in seq_along(r$values)) {
    if (isTRUE(r$values[i]) && r$lengths[i] >= min_dur) {
      result[starts[i]:ends[i]] <- TRUE
    }
  }
  result
}


# =============================================================================
# 5. EXTRACAO DE EVENTOS
# =============================================================================

#' @keywords internal
#' @noRd
.hw_extract_events <- function(daily, method) {

  hw_cols <- paste0("hw_", tolower(method))
  hw_cols_present <- intersect(hw_cols, names(daily))

  if (length(hw_cols_present) == 0) return(tibble())

  purrr::map_dfr(hw_cols_present, function(col) {
    m_name <- toupper(gsub("hw_", "", col))

    # Variavel de temperatura relevante por metodo
    temp_col <- switch(m_name,
      WHO   = "tmax",
      WMO   = "tmax",
      INMET = "tmax",
      EHF   = "tmean",
      UTCI  = "utci_max",
      WBGT  = "wbgt_max",
      HI    = "hi_max",
      "tmax"
    )
    ref_col <- switch(m_name,
      WHO   = "tmax_p",
      WMO   = "tmax_p",
      INMET = "tmean_hist",
      EHF   = "tmean",
      UTCI  = "utci_p",
      WBGT  = "wbgt_p",
      HI    = "hi_p",
      "tmax_p"
    )

    purrr::map_dfr(unique(daily$station_code), function(st) {
      st_data <- daily |>
        dplyr::filter(station_code == st) |>
        dplyr::arrange(date_day)

      flag <- st_data[[col]]
      if (all(is.na(flag)) || !any(flag, na.rm = TRUE)) return(NULL)

      # Identificar eventos (grupos de dias consecutivos TRUE)
      flag[is.na(flag)] <- FALSE
      r <- rle(flag)
      ends   <- cumsum(r$lengths)
      starts <- ends - r$lengths + 1

      events_list <- purrr::map_dfr(
        seq_along(r$values)[r$values],
        function(i) {
          idx <- starts[i]:ends[i]
          sub <- st_data[idx, ]

          temp_vals <- if (temp_col %in% names(sub)) sub[[temp_col]] else rep(NA_real_, nrow(sub))
          ref_vals  <- if (ref_col  %in% names(sub)) sub[[ref_col]]  else rep(NA_real_, nrow(sub))

          anomaly <- temp_vals - ref_vals
          
          # Calculate EHF specific metrics if applicable
          ehf_peak <- NA_real_
          ehf_mean <- NA_real_
          if (m_name == "EHF" && "ehf_value" %in% names(sub)) {
            ehf_peak <- max(sub$ehf_value, na.rm = TRUE)
            ehf_mean <- mean(sub$ehf_value, na.rm = TRUE)
          }

          tibble(
            station_code        = st,
            method              = m_name,
            start_date          = as.Date(min(sub$date_day), origin = "1970-01-01"),
            end_date            = as.Date(max(sub$date_day), origin = "1970-01-01"),
            duration_days       = as.integer(diff(range(sub$date_day)) + 1),
            temp_mean           = mean(temp_vals, na.rm = TRUE),
            temp_peak           = max(temp_vals,  na.rm = TRUE),
            anomaly_mean        = mean(anomaly, na.rm = TRUE),
            anomaly_cumulative  = sum(anomaly,  na.rm = TRUE),
            severity_index      = as.integer(duration_days) * mean(anomaly, na.rm = TRUE),
            ehf_peak            = ehf_peak,
            ehf_mean            = ehf_mean,
            region              = if ("region"         %in% names(sub)) first(sub$region)         else NA_character_,
            federal_unit        = if ("federal_unit"   %in% names(sub)) first(sub$federal_unit)   else NA_character_,
            station_name        = if ("station_name"   %in% names(sub)) first(sub$station_name)   else NA_character_,
            zona_climatica      = if ("zona_climatica" %in% names(sub)) first(sub$zona_climatica) else NA_character_,
            latitude            = if ("latitude"       %in% names(sub)) first(sub$latitude)       else NA_real_,
            longitude           = if ("longitude"      %in% names(sub)) first(sub$longitude)      else NA_real_
          )
        }
      )

      if (nrow(events_list) == 0) return(NULL)

      # Adicionar ID unico por estacao + metodo
      events_list <- events_list |>
        dplyr::arrange(start_date) |>
        dplyr::mutate(event_id = paste0(st, "_", m_name, "_", row_number()))
        
      # Apply EHF Intensity Classification if method is EHF
      if (m_name == "EHF") {
        # Calculate EHF85: 85th percentile of all positive EHF values for this station
        all_positive_ehf <- st_data$ehf_value[!is.na(st_data$ehf_value) & st_data$ehf_value > 0]
        if (length(all_positive_ehf) > 0) {
          ehf85 <- quantile(all_positive_ehf, 0.85, na.rm = TRUE)
          
          events_list <- events_list |>
            dplyr::mutate(
              intensity_class = case_when(
                ehf_peak >= 3 * ehf85 ~ "Extreme (EHW)",
                ehf_peak >= ehf85     ~ "Severe (SHW)",
                ehf_peak > 0          ~ "Low Intensity (LIHW)",
                TRUE                  ~ NA_character_
              )
            )
        } else {
          events_list <- events_list |> mutate(intensity_class = NA_character_)
        }
      } else {
        events_list <- events_list |> mutate(intensity_class = NA_character_)
      }
      
      events_list
    })
  }) |>
    dplyr::arrange(station_code, method, start_date)
}


# =============================================================================
# 6. RESUMO ANUAL
# =============================================================================

#' @keywords internal
#' @noRd
.hw_build_summary <- function(events, daily, method) {

  if (nrow(events) == 0) {
    return(tibble(
      year = integer(), station_code = character(), method = character(),
      n_events = integer(), total_days_hw = integer(),
      mean_duration = double(), max_duration = integer(),
      mean_intensity = double(), max_intensity = double()
    ))
  }

  events |>
    dplyr::mutate(year = year(start_date)) |>
    dplyr::group_by(year, station_code, method, region, federal_unit, zona_climatica) |>
    dplyr::summarise(
      n_events       = n(),
      total_days_hw  = sum(duration_days),
      mean_duration  = mean(duration_days),
      max_duration   = max(duration_days),
      mean_intensity = mean(temp_mean,  na.rm = TRUE),
      max_intensity  = max(temp_peak,   na.rm = TRUE),
      mean_anomaly   = mean(anomaly_mean, na.rm = TRUE),
      severity_total = sum(severity_index, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(station_code, method, year)
}


# =============================================================================
# UTILITARIO: INTERPOLACAO LINEAR
# =============================================================================

#' @keywords internal
#' @noRd
.hw_interpolate <- function(x) {
  n <- length(x)
  idx <- seq_len(n)
  non_na <- which(!is.na(x))
  if (length(non_na) < 2) return(x)
  approx(non_na, x[non_na], xout = idx, rule = 2)$y
}


# =============================================================================
# FUNCOES DE CONVENIENCIA / HELPERS EXPORTEVEIS
# =============================================================================

#' Retorna tabela resumida de eventos de ondas de calor
#' @param hw_result output de sus_climate_compute_heatwaves()
#' @param method_filter opcional: filtrar por metodo(s)
#' @noRd
hw_get_events <- function(hw_result, method_filter = NULL) {
  ev <- hw_result$events
  if (!is.null(method_filter)) ev <- filter(ev, method %in% method_filter)
  ev
}

#' Conta eventos por ano e metodo
#' @param hw_result output de sus_climate_compute_heatwaves()
#' @noRd
hw_count_by_year <- function(hw_result) {
  cols <- intersect(
    c("year", "station_code", "station_name", "method", "n_events", "total_days_hw", "mean_duration"),
    names(hw_result$summary)
  )
  hw_result$summary |> select(all_of(cols))
}

#' Retorna dias com onda de calor ativa (qualquer metodo)
#' @param hw_result output de sus_climate_compute_heatwaves()
#' @noRd
hw_active_days <- function(hw_result) {
  hw_result$daily |>
    dplyr::filter(hw_any == TRUE) |>
    dplyr::select(station_code, date_day, tmax, tmean, starts_with("hw_"))
}
