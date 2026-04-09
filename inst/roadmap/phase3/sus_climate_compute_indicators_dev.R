#' @title Compute Bioclimatic Comfort and Thermal Stress Indicators
#'
#' @description
#' `sus_climate_compute_indicators()` calculates a set of bioclimatic and
#' thermal-stress indices from INMET station data, typically produced by
#' `sus_climate_inmet()` or gap-filled by `sus_climate_fill()`.
#'
#' **Available indicators:**
#' \itemize{
#'   \item **WBGT** – Wet Bulb Globe Temperature: integrates air temperature,
#'         humidity and solar radiation. Reference for occupational heat stress.
#'   \item **HI** – Heat Index (Steadman / NWS): apparent temperature felt by
#'         the human body under shade. Valid for T ≥ 26.7 °C and RH ≥ 40%.
#'   \item **THI** – Temperature-Humidity Index: simplified discomfort index
#'         widely used in agronomy and veterinary science.
#'   \item **WCET** – Wind Chill Equivalent Temperature (Canadian formula):
#'         perceived cold under wind exposure. Valid for T ≤ 10 °C.
#'   \item **WCT** – Wind Chill Temperature (US/NWS formula): alternative
#'         formulation using mph. Valid for T ≤ 10 °C.
#'   \item **ET** – Effective Temperature: comfort index combining temperature,
#'         humidity and wind.
#' }
#'
#' **Key features:**
#' \itemize{
#'   \item **Selective computation**: choose one, several or `"all"` indicators
#'   \item **Auto-detection** of datetime and station columns
#'   \item **Graceful missing-data handling**: if a required variable is absent
#'         or fully NA for a station, the indicator is set to `NA` with a warning
#'   \item **Validity masks**: values outside the physical domain of each index
#'         are flagged as `NA` (optional, via `apply_validity_mask`)
#'   \item **Multilingual messages**: `"pt"`, `"en"`, `"es"`
#'   \item **S3 metadata**: result carries a `sus_meta` attribute consistent
#'         with the `climasus_df` class used by the rest of the package
#' }
#'
#' @param df
#'   A data frame or tibble with INMET climate data. Typically the output of
#'   `sus_climate_inmet()` or `sus_climate_fill()`. Must contain a datetime
#'   column and at least the variables required by the requested indicators.
#'   Required variables per indicator:
#'   \itemize{
#'     \item WBGT : `tair_dry_bulb_c`, `rh_mean_porc`, `sr_kj_m2`, `ws_2_m_s`
#'     \item HI   : `tair_dry_bulb_c`, `rh_mean_porc`
#'     \item THI  : `tair_dry_bulb_c`, `rh_mean_porc`
#'     \item WCET : `tair_dry_bulb_c`, `ws_2_m_s`
#'     \item WCT  : `tair_dry_bulb_c`, `ws_2_m_s`
#'     \item ET   : `tair_dry_bulb_c`, `rh_mean_porc`, `ws_2_m_s`
#'   }
#'
#' @param indicators
#'   Character vector of indicator codes to compute, or `"all"` (default).
#'   Valid codes (case-insensitive): `"wbgt"`, `"hi"`, `"thi"`,
#'   `"wcet"`, `"wct"`, `"et"`.
#'
#' @param datetime_col
#'   Character. Name of the datetime column. If `NULL` (default), auto-detected.
#'
#' @param station_col
#'   Character. Name of the station identifier column. If `NULL`, auto-detected.
#'
#' @param apply_validity_mask
#'   Logical. If `TRUE` (default), values outside each index's valid physical
#'   domain are replaced with `NA`:
#'   \itemize{
#'     \item HI: applied only when T ≥ 26.7 °C and RH ≥ 40%
#'     \item WCET / WCT: applied only when T ≤ 10 °C and ws > 1.3 m/s
#'   }
#'
#' @param keep_source_vars
#'   Logical. If `TRUE`, the source climate variables used in calculations
#'   are retained alongside the new indicator columns in the output.
#'   Default: `FALSE` (returns only datetime, station, and indicator columns).
#'
#' @param verbose
#'   Logical. If `TRUE` (default), prints progress and summary messages.
#'
#' @param lang
#'   Character. Message language: `"pt"` (default), `"en"`, or `"es"`.
#'
#' @return
#' A `tibble` (class `climasus_df`) with:
#' \itemize{
#'   \item The datetime column (name preserved from input)
#'   \item The station identifier column
#'   \item One numeric column per computed indicator (e.g. `wbgt_c`,
#'         `hi_c`, `thi_c`, `wcet_c`, `wct_c`, `et_c`)
#'   \item Source climate variables if `keep_source_vars = TRUE`
#'   \item A `sus_meta` attribute with computation metadata
#' }
#'
#' @section Scientific References:
#' \itemize{
#'   \item **WBGT**: Liljegren et al. (2008). Modeling the wet bulb globe
#'         temperature using standard meteorological measurements.
#'         *J. Occup. Environ. Hyg.* 5(10): 645-655.
#'   \item **HI**: Rothfusz (1990). *The Heat Index Equation*. NWS Technical
#'         Attachment SR/SSD 90-23.
#'   \item **THI**: Thom (1959). The discomfort index. *Weatherwise* 12(2): 57-59.
#'   \item **WCET**: Environment Canada (2001). Wind Chill Index.
#'   \item **WCT**: NWS Wind Chill Temperature Index (2001).
#'   \item **ET**: Missenard (1937). Temperatura efectiva. Referenced in
#'         Auliciems & Szokolay (2007).
#' }
#'
#' @examples
#' \dontrun{
#' # ===== All indicators (default) =====
#' indicators <- sus_climate_compute_indicators(climate_data)
#'
#' # ===== Select specific indicators =====
#' thermal <- sus_climate_compute_indicators(
#'   df         = climate_data,
#'   indicators = c("wbgt", "hi", "thi"),
#'   lang       = "pt"
#' )
#'
#' # ===== Keep source variables in output =====
#' full_output <- sus_climate_compute_indicators(
#'   df               = climate_data,
#'   indicators       = "all",
#'   keep_source_vars = TRUE,
#'   verbose          = TRUE
#' )
#'
#' # ===== Pipe from sus_climate_fill() =====
#' climate_data |>
#'   sus_climate_fill(target_var = "all") |>
#'   sus_climate_compute_indicators(indicators = c("wbgt", "hi"))
#' }
#'
#' @export
#' @importFrom rlang .data
sus_climate_compute_indicators <- function(
  df,
  indicators        = "all",
  datetime_col      = NULL,
  station_col       = NULL,
  apply_validity_mask = TRUE,
  keep_source_vars  = FALSE,
  verbose           = TRUE,
  lang              = "pt"
) {

  # ===========================================================
  # INDICATOR REGISTRY
  # Known codes, output column names, required INMET variables
  # ===========================================================
  .indicator_registry <- list(
    wbgt = list(
      label    = "Wet Bulb Globe Temperature",
      out_col  = "wbgt_c",
      requires = c("tair_dry_bulb_c", "rh_mean_porc", "sr_kj_m2", "ws_2_m_s")
    ),
    hi = list(
      label    = "Heat Index",
      out_col  = "hi_c",
      requires = c("tair_dry_bulb_c", "rh_mean_porc")
    ),
    thi = list(
      label    = "Temperature-Humidity Index",
      out_col  = "thi_c",
      requires = c("tair_dry_bulb_c", "rh_mean_porc")
    ),
    wcet = list(
      label    = "Wind Chill Equivalent Temperature (CA)",
      out_col  = "wcet_c",
      requires = c("tair_dry_bulb_c", "ws_2_m_s")
    ),
    wct = list(
      label    = "Wind Chill Temperature (NWS)",
      out_col  = "wct_c",
      requires = c("tair_dry_bulb_c", "ws_2_m_s")
    ),
    et = list(
      label    = "Effective Temperature",
      out_col  = "et_c",
      requires = c("tair_dry_bulb_c", "rh_mean_porc", "ws_2_m_s")
    )
  )

  .all_indicator_codes <- names(.indicator_registry)

  # ===========================================================
  # MESSAGES
  # ===========================================================
  messages <- .get_indicators_messages(lang)

  # ===========================================================
  # COLUMN AUTO-DETECTION (reuse package helpers)
  # ===========================================================
  source_sys   <- .detect_data_source(df)
  datetime_col <- .detect_datetime_column(df, datetime_col, verbose, messages)
  station_col  <- .detect_station_column(df, station_col, source_sys, verbose, messages)

  # ===========================================================
  # RESOLVE indicators  ("all" → full registry)
  # ===========================================================
  if (identical(tolower(indicators), "all") || identical(indicators, "all")) {
    indicators <- .all_indicator_codes
  } else {
    indicators <- tolower(trimws(indicators))
    bad_codes  <- setdiff(indicators, .all_indicator_codes)
    if (length(bad_codes) > 0) {
      cli::cli_abort(
        sprintf(
          messages$unknown_indicators,
          paste(bad_codes, collapse = ", "),
          paste(.all_indicator_codes, collapse = ", ")
        )
      )
    }
  }

  if (verbose) {
    cli::cli_alert_info(
      sprintf(messages$computing_indicators,
              length(indicators),
              paste(toupper(indicators), collapse = ", "))
    )
  }

  # ===========================================================
  # INTERNAL: rename datetime to "date" for consistent handling
  # ===========================================================
  df_work <- df
  if (datetime_col != "date") {
    df_work <- df_work %>%
      dplyr::rename(date = !!rlang::sym(datetime_col))
  }

  # Identify source columns used across all requested indicators
  all_required <- unique(unlist(
    lapply(indicators, function(i) .indicator_registry[[i]]$requires)
  ))

  # Columns present in df
  present_cols  <- colnames(df_work)
  missing_globally <- setdiff(all_required, present_cols)

  if (length(missing_globally) > 0 && verbose) {
    cli::cli_alert_warning(
      sprintf(messages$missing_vars_global,
              paste(missing_globally, collapse = ", "))
    )
  }

  # ===========================================================
  # COMPUTE INDICATORS  (row-wise; vectorised inside each fn)
  # ===========================================================
  result <- df_work %>%
    dplyr::select(date, dplyr::all_of(station_col))

  indicator_stats <- list()

  for (ind in indicators) {
    reg      <- .indicator_registry[[ind]]
    out_col  <- reg$out_col
    required <- reg$requires

    missing_for_ind <- setdiff(required, colnames(df_work))

    if (length(missing_for_ind) > 0) {
      if (verbose) {
        cli::cli_alert_warning(
          sprintf(messages$skipping_indicator,
                  toupper(ind),
                  paste(missing_for_ind, collapse = ", "))
        )
      }
      result[[out_col]] <- NA_real_
      indicator_stats[[ind]] <- list(computed = FALSE,
                                     reason   = "missing_vars",
                                     n_valid  = 0L)
      next
    }

    values <- switch(
      ind,
      wbgt = .compute_wbgt(
        airT      = df_work[["tair_dry_bulb_c"]],
        rh        = df_work[["rh_mean_porc"]],
        sr_kj_m2  = df_work[["sr_kj_m2"]],
        ws        = df_work[["ws_2_m_s"]],
        apply_mask = apply_validity_mask
      ),
      hi   = .compute_hi(
        airT      = df_work[["tair_dry_bulb_c"]],
        rh        = df_work[["rh_mean_porc"]],
        apply_mask = apply_validity_mask
      ),
      thi  = .compute_thi(
        airT = df_work[["tair_dry_bulb_c"]],
        rh   = df_work[["rh_mean_porc"]]
      ),
      wcet = .compute_wcet(
        airT      = df_work[["tair_dry_bulb_c"]],
        ws        = df_work[["ws_2_m_s"]],
        apply_mask = apply_validity_mask
      ),
      wct  = .compute_wct(
        airT      = df_work[["tair_dry_bulb_c"]],
        ws        = df_work[["ws_2_m_s"]],
        apply_mask = apply_validity_mask
      ),
      et   = .compute_et(
        airT = df_work[["tair_dry_bulb_c"]],
        rh   = df_work[["rh_mean_porc"]],
        ws   = df_work[["ws_2_m_s"]]
      )
    )

    result[[out_col]] <- values
    n_valid <- sum(!is.na(values))
    n_total <- length(values)

    indicator_stats[[ind]] <- list(
      computed      = TRUE,
      label         = reg$label,
      out_col       = out_col,
      n_valid       = n_valid,
      n_total       = n_total,
      pct_valid     = round(n_valid / n_total * 100, 2),
      range         = if (n_valid > 0)
                        round(range(values, na.rm = TRUE), 2)
                      else NA
    )

    if (verbose) {
      cli::cli_alert_success(
        sprintf(messages$indicator_done,
                toupper(ind),
                reg$label,
                out_col,
                n_valid,
                n_total,
                round(n_valid / n_total * 100, 1))
      )
    }
  }

  # ===========================================================
  # OPTIONALLY ATTACH SOURCE VARIABLES
  # ===========================================================
  if (keep_source_vars) {
    source_to_add <- intersect(all_required, colnames(df_work))
    result <- result %>%
      dplyr::left_join(
        df_work %>%
          dplyr::select(date,
                        dplyr::all_of(station_col),
                        dplyr::all_of(source_to_add)),
        by = c("date", station_col)
      )
  }

  # ===========================================================
  # RESTORE ORIGINAL DATETIME COLUMN NAME
  # ===========================================================
  if (datetime_col != "date") {
    result <- result %>%
      dplyr::rename(!!rlang::sym(datetime_col) := date)
  }

  date_col_final <- if (datetime_col != "date") datetime_col else "date"

  # ===========================================================
  # S3 CLASS + sus_meta METADATA
  # ===========================================================
  computed_cols <- vapply(
    indicators,
    function(i) .indicator_registry[[i]]$out_col,
    character(1)
  )

  meta <- list(
    system  = NULL,
    stage   = "climate",
    type    = "indicators",
    spatial = inherits(result, "sf"),
    temporal = list(
      start              = min(result[[date_col_final]], na.rm = TRUE),
      end                = max(result[[date_col_final]], na.rm = TRUE),
      source             = source_sys,
      indicators_requested = indicators,
      indicator_stats    = indicator_stats,
      keep_source_vars   = keep_source_vars,
      apply_validity_mask = apply_validity_mask
    ),
    created  = Sys.time(),
    modified = Sys.time(),
    history  = sprintf(
      "[%s] Bioclimatic indicators computed: %s",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      paste(toupper(indicators), collapse = ", ")
    ),
    user = list()
  )

  base_classes <- setdiff(class(result), "climasus_df")
  result <- structure(
    result,
    sus_meta = meta,
    class    = c("climasus_df", base_classes)
  )

  if (verbose) {
    cli::cli_alert_success(
      sprintf(messages$done,
              sum(vapply(indicator_stats, function(x) isTRUE(x$computed), logical(1))),
              length(indicators))
    )
  }

  return(result)
}


# ============================================================================
# INDICATOR CALCULATION HELPERS
# ============================================================================

#' Compute Wet Bulb Globe Temperature (WBGT)
#'
#' @description
#' Simplified WBGT using air temperature, relative humidity, solar radiation
#' and wind speed. Combines natural wet-bulb (Tnw), globe temperature (Tg)
#' and dry-bulb components.
#'
#' Formula:
#' WBGT = 0.7 * Tnw + 0.2 * Tg + 0.1 * Tdb
#'
#' where:
#'   Tnw = Tdb + 0.33 * (rh/100) * exp(0.0514 * Tdb) - 4.0
#'         (approximation by Bernard & Pourmoghani, 1999)
#'   Tg  = Tdb + 0.0144 * (sr_kj_m2 / 3.6)^0.6 / (ws + 0.1)^0.2 - 2.0
#'
#' @param airT Numeric. Air temperature (°C).
#' @param rh   Numeric. Relative humidity (%).
#' @param sr_kj_m2 Numeric. Solar radiation (kJ/m²).
#' @param ws   Numeric. Wind speed (m/s).
#' @param apply_mask Logical. No mask applied to WBGT (returned as-is).
#' @return Numeric vector. WBGT in °C.
#' @keywords internal
#' @noRd
.compute_wbgt <- function(airT, rh, sr_kj_m2, ws, apply_mask = TRUE) {

  # Convert kJ/m² → W/m² (divide by 3.6 for hourly data; keeps formula general)
  sr_wm2 <- sr_kj_m2 / 3.6

  # Natural wet-bulb temperature approximation (Bernard & Pourmoghani, 1999)
  tnw <- airT + 0.33 * (rh / 100) * exp(0.0514 * airT) - 4.0

  # Globe temperature approximation (Liljegren simplified)
  ws_safe <- pmax(ws, 0.1)  # avoid division by zero
  tg <- airT + 0.0144 * pmax(sr_wm2, 0)^0.6 / ws_safe^0.2 - 2.0

  wbgt <- 0.7 * tnw + 0.2 * tg + 0.1 * airT

  return(round(wbgt, 2))
}


#' Compute Heat Index (Rothfusz / NWS)
#'
#' @description
#' Full Rothfusz regression equation. Applicable when T ≥ 26.7 °C (80 °F)
#' and RH ≥ 40%. Below that threshold, a simpler Steadman estimate is used.
#'
#' @param airT Numeric. Air temperature (°C).
#' @param rh   Numeric. Relative humidity (%).
#' @param apply_mask Logical. If TRUE, values where T < 26.7 °C are set to NA.
#' @return Numeric vector. Heat Index in °C.
#' @keywords internal
#' @noRd
.compute_hi <- function(airT, rh, apply_mask = TRUE) {

  airT_f <- (airT * 9 / 5) + 32  # → Fahrenheit

  hi_f <- (
    -42.379
    + 2.04901523   * airT_f
    + 10.14333127  * rh
    - 0.22475541   * airT_f * rh
    - 6.83783e-3   * airT_f^2
    - 5.481717e-2  * rh^2
    + 1.22874e-3   * airT_f^2 * rh
    + 8.5282e-4    * airT_f   * rh^2
    - 1.99e-6      * airT_f^2 * rh^2
  )

  # Two Steadman adjustments for edge conditions
  # Low humidity adjustment (RH < 13% and 80°F <= T <= 112°F)
  adj_low_rh  <- (13 - rh) / 4 * sqrt((17 - abs(airT_f - 95)) / 17)
  low_rh_mask <- rh < 13 & airT_f >= 80 & airT_f <= 112
  hi_f <- ifelse(low_rh_mask, hi_f - adj_low_rh, hi_f)

  # High humidity adjustment (RH > 85% and 80°F <= T <= 87°F)
  adj_high_rh <- (rh - 85) / 10 * ((87 - airT_f) / 5)
  high_rh_mask <- rh > 85 & airT_f >= 80 & airT_f <= 87
  hi_f <- ifelse(high_rh_mask, hi_f + adj_high_rh, hi_f)

  # Convert back to Celsius
  hi_c <- (hi_f - 32) * 5 / 9

  # Validity mask: index only meaningful above ~26.7 °C
  if (apply_mask) {
    hi_c <- ifelse(airT < 26.7, NA_real_, hi_c)
  }

  return(round(hi_c, 2))
}


#' Compute Temperature-Humidity Index (THI)
#'
#' @description
#' Thom (1959) formula — widely used in agronomy and livestock comfort.
#' THI = Tdb - ((1 - RH/100) * (Tdb - 14.4)) / 2
#'
#' @param airT Numeric. Air temperature (°C).
#' @param rh   Numeric. Relative humidity (%).
#' @return Numeric vector. THI (dimensionless, in °C equivalent).
#' @keywords internal
#' @noRd
.compute_thi <- function(airT, rh) {

  thi <- airT - ((1 - rh / 100) * (airT - 14.4)) / 2

  return(round(thi, 2))
}


#' Compute Wind Chill Equivalent Temperature — Canadian formula (WCET)
#'
#' @description
#' Environment Canada (2001) wind chill index. Valid for T ≤ 10 °C and
#' wind speed > 1.3 m/s (walking pace). Formula operates in km/h.
#'
#' WCET = 13.12 + 0.6215*T - 11.37*V^0.16 + 0.3965*T*V^0.16
#' where V is wind speed in km/h.
#'
#' @param airT Numeric. Air temperature (°C).
#' @param ws   Numeric. Wind speed (m/s).
#' @param apply_mask Logical. If TRUE, values where T > 10 °C are set to NA.
#' @return Numeric vector. WCET in °C.
#' @keywords internal
#' @noRd
.compute_wcet <- function(airT, ws, apply_mask = TRUE) {

  ws_kph <- ws * 3.6
  wcet   <- 13.12 + (0.6215 * airT) - (11.37 * ws_kph^0.16) +
            (0.3965 * airT * ws_kph^0.16)

  if (apply_mask) {
    wcet <- ifelse(airT > 10 | ws < 1.3, NA_real_, wcet)
  }

  return(round(wcet, 2))
}


#' Compute Wind Chill Temperature — NWS formula (WCT)
#'
#' @description
#' US National Weather Service formula (2001). Operates in °F and mph;
#' result converted back to °C. Valid for T ≤ 10 °C and ws > 1.3 m/s.
#'
#' WCT_F = 35.74 + 0.6215*T_F - 35.75*V_mph^0.16 + 0.4275*T_F*V_mph^0.16
#'
#' @param airT Numeric. Air temperature (°C).
#' @param ws   Numeric. Wind speed (m/s).
#' @param apply_mask Logical. If TRUE, values where T > 10 °C are set to NA.
#' @return Numeric vector. WCT in °C.
#' @keywords internal
#' @noRd
.compute_wct <- function(airT, ws, apply_mask = TRUE) {

  airT_f  <- (airT * 9 / 5) + 32
  ws_mph  <- ws * 0.621371

  wct_f <- 35.74 + 0.6215 * airT_f - 35.75 * ws_mph^0.16 +
           0.4275 * airT_f * ws_mph^0.16

  # Convert back to Celsius
  wct_c <- (wct_f - 32) * 5 / 9

  if (apply_mask) {
    wct_c <- ifelse(airT > 10 | ws < 1.3, NA_real_, wct_c)
  }

  return(round(wct_c, 2))
}


#' Compute Effective Temperature (ET)
#'
#' @description
#' Missenard (1937) effective temperature, as used in bioclimatology.
#' Combines air temperature, relative humidity and wind speed.
#'
#' ET = Tdb - 0.4 * (Tdb - 10) * (1 - RH/100) - 1.1 * (ws - 0.2)^0.5
#'
#' The formula is bounded so that negative wind-correction terms
#' (ws < 0.04 m/s) do not inflate ET.
#'
#' @param airT Numeric. Air temperature (°C).
#' @param rh   Numeric. Relative humidity (%).
#' @param ws   Numeric. Wind speed (m/s).
#' @return Numeric vector. ET in °C.
#' @keywords internal
#' @noRd
.compute_et <- function(airT, rh, ws) {

  ws_safe <- pmax(ws, 0.04)

  et <- airT -
        0.4 * (airT - 10) * (1 - rh / 100) -
        1.1 * (ws_safe - 0.2)^0.5

  # Physical lower bound: ET cannot fall below absolute zero
  et <- pmax(et, -273.15)

  return(round(et, 2))
}


# ============================================================================
# MULTILINGUAL MESSAGES
# ============================================================================

#' Get Multilingual Messages for Indicator Computation
#' @keywords internal
#' @noRd
.get_indicators_messages <- function(lang = "pt") {

  messages <- list(
    pt = list(
      no_datetime_found    = "Nenhuma coluna de data/hora encontrada.",
      no_station_found     = "Nenhuma coluna de estacao encontrada. Processando todos os dados juntos.",
      station_col_not_found = "Coluna de estacao '%s' nao encontrada.",
      unknown_indicators   = "Indicador(es) desconhecido(s): %s. Opcoes validas: %s.",
      computing_indicators = "Calculando %d indicador(es): %s",
      missing_vars_global  = "Variaveis ausentes no dataframe (alguns indicadores serao NA): %s",
      skipping_indicator   = "Indicador %s ignorado — variavel(is) ausente(s): %s",
      indicator_done       = "OK %s (%s) → coluna '%s': %d/%d valores validos (%.1f%%)",
      done                 = "Concluido: %d/%d indicadores calculados com sucesso."
    ),
    en = list(
      no_datetime_found    = "No datetime column found.",
      no_station_found     = "No station column found. Processing all data together.",
      station_col_not_found = "Station column '%s' not found.",
      unknown_indicators   = "Unknown indicator code(s): %s. Valid options: %s.",
      computing_indicators = "Computing %d indicator(s): %s",
      missing_vars_global  = "Variables absent from dataframe (some indicators will be NA): %s",
      skipping_indicator   = "Skipping indicator %s — missing variable(s): %s",
      indicator_done       = "OK %s (%s) → column '%s': %d/%d valid values (%.1f%%)",
      done                 = "Done: %d/%d indicators computed successfully."
    ),
    es = list(
      no_datetime_found    = "No se encontro columna de fecha/hora.",
      no_station_found     = "No se encontro columna de estacion. Procesando todos los datos juntos.",
      station_col_not_found = "Columna de estacion '%s' no encontrada.",
      unknown_indicators   = "Codigo(s) de indicador desconocido(s): %s. Opciones validas: %s.",
      computing_indicators = "Calculando %d indicador(es): %s",
      missing_vars_global  = "Variables ausentes en el dataframe (algunos indicadores seran NA): %s",
      skipping_indicator   = "Indicador %s omitido — variable(s) ausente(s): %s",
      indicator_done       = "OK %s (%s) → columna '%s': %d/%d valores validos (%.1f%%)",
      done                 = "Completado: %d/%d indicadores calculados correctamente."
    )
  )

  lang <- match.arg(lang, names(messages))
  return(messages[[lang]])
}
