#' @title Compute Bioclimatic and Thermal Stress Indicators (Regionally Adaptive)
#'
#' @description
#' `sus_climate_compute_indicators()` calculates bioclimatic and thermal-stress
#' indices from INMET station data with **automatic regional adaptation** for
#' Brazilian biomes (Amazon, Cerrado, Caatinga, Atlantic Forest, Pampa, Pantanal).
#'
#' **Key improvements:**
#' \itemize{
#'   \item **No destructive masking** - All indicators always calculated
#'   \item **Regional adaptation** - Parameters adjust to local climate
#'   \item **Confidence flags** - Know when values extrapolate recommended ranges
#'   \item **New indicators** - UTCI, PET, Cooling Degree Days, Growing Degree Days
#'   \item **Extensible registry** - Easy to add new indicators
#' }
#'
#' @inheritParams sus_climate_compute_indicators
#' @param region Character. Brazilian region/biome for parameter adaptation:
#'   \itemize{
#'     \item `"auto"` (default): Auto-detect from latitude/longitude or UF
#'     \item `"amazon"`: Amazon biome (high humidity, stable temps)
#'     \item `"cerrado"`: Savanna (dry winters, wet summers)
#'     \item `"caatinga"`: Semiarid (high temps, low humidity)
#'     \item `"atlantic_forest"`: Coastal tropical (moderate humidity)
#'     \item `"pampa"`: Subtropical grasslands (cool winters)
#'     \item `"pantanal"`: Wetlands (extreme seasonal variation)
#'     \item `"southeast"`: Generic for Southeast region
#'     \item `"south"`: Generic for South region (cold winters)
#'   }
#' @param confidence_flags Logical. Add columns with validity flags (e.g., 
#'   `hi_out_of_range`, `wcet_low_temp_warning`). Default: `TRUE`.
#' @param custom_thresholds List. Override default thresholds per indicator.
#'   Example: `list(hi = list(min_temp = 24, min_rh = 35))`
#'
#' @return A `tibble` with:
#'   - All original columns (if `keep_source_vars = TRUE`)
#'   - Indicator columns (always calculated, no NA masking)
#'   - Confidence flag columns (if `confidence_flags = TRUE`)
#'   - `sus_meta` attribute with regional adaptation metadata
#'
#' @export
sus_climate_compute_indicators_adaptive <- function(
  df,
  indicators = "all",
  region = "auto",
  datetime_col = NULL,
  station_col = NULL,
  confidence_flags = TRUE,
  custom_thresholds = NULL,
  keep_source_vars = FALSE,
  verbose = TRUE,
  lang = "pt"
) {

  # ===========================================================
  # EXPANDED INDICATOR REGISTRY
  # ===========================================================
  .indicator_registry <- list(
    # Thermal comfort indices
    wbgt = list(
      name = "Wet Bulb Globe Temperature",
      out_col = "wbgt_c",
      requires = c("tair_dry_bulb_c", "rh_mean_porc", "sr_kj_m2", "ws_2_m_s"),
      thresholds = list(
        warning_high = 31,  # °C - extreme heat stress risk
        warning_low = 15     # °C - cold stress risk
      ),
      fun = .compute_wbgt_adaptive
    ),
    
    hi = list(
      name = "Heat Index",
      out_col = "hi_c",
      requires = c("tair_dry_bulb_c", "rh_mean_porc"),
      thresholds = list(
        min_temp_default = 26.7,  # Standard Rothfusz
        min_temp_tropical = 24.0,  # Adapted for tropics
        min_rh = 40
      ),
      fun = .compute_hi_adaptive
    ),
    
    thi = list(
      name = "Temperature-Humidity Index",
      out_col = "thi_c",
      requires = c("tair_dry_bulb_c", "rh_mean_porc"),
      thresholds = list(
        comfortable = list(min = 20, max = 24),
        stress_high = 28
      ),
      fun = .compute_thi
    ),
    
    wcet = list(
      name = "Wind Chill Equivalent Temperature (CA)",
      out_col = "wcet_c",
      requires = c("tair_dry_bulb_c", "ws_2_m_s"),
      thresholds = list(
        min_temp_for_cold_warning = 10,
        min_wind_for_cold_warning = 1.3
      ),
      fun = .compute_wcet
    ),
    
    wct = list(
      name = "Wind Chill Temperature (NWS)",
      out_col = "wct_c",
      requires = c("tair_dry_bulb_c", "ws_2_m_s"),
      thresholds = list(
        min_temp_for_cold_warning = 10,
        min_wind_for_cold_warning = 1.3
      ),
      fun = .compute_wct
    ),
    
    et = list(
      name = "Effective Temperature",
      out_col = "et_c",
      requires = c("tair_dry_bulb_c", "rh_mean_porc", "ws_2_m_s"),
      fun = .compute_et
    ),
    
    # NEW INDICATORS
    utci = list(
      name = "Universal Thermal Climate Index",
      out_col = "utci_c",
      requires = c("tair_dry_bulb_c", "rh_mean_porc", "ws_2_m_s", "sr_kj_m2"),
      thresholds = list(
        cold_stress = list(extreme = -27, strong = -13, moderate = -13, slight = -13),
        heat_stress = list(extreme = 46, strong = 38, moderate = 32, slight = 26)
      ),
      fun = .compute_utci_simplified
    ),
    
    pet = list(
      name = "Physiological Equivalent Temperature",
      out_col = "pet_c",
      requires = c("tair_dry_bulb_c", "rh_mean_porc", "ws_2_m_s", "sr_kj_m2"),
      fun = .compute_pet_simplified
    ),
    
    cdd = list(
      name = "Cooling Degree Days",
      out_col = "cdd_c",
      requires = c("tair_dry_bulb_c"),
      thresholds = list(base_temp = 18),  # Cooling needed above 18°C
      fun = .compute_cdd
    ),
    
    hdd = list(
      name = "Heating Degree Days",
      out_col = "hdd_c",
      requires = c("tair_dry_bulb_c"),
      thresholds = list(base_temp = 18),  # Heating needed below 18°C
      fun = .compute_hdd
    ),
    
    gdd = list(
      name = "Growing Degree Days",
      out_col = "gdd_c",
      requires = c("tair_dry_bulb_c"),
      thresholds = list(
        base_temp = 10,  # Base for tropical crops
        upper_temp = 30  # Upper threshold for heat stress
      ),
      fun = .compute_gdd
    ),
    
    koppen_humidity = list(
      name = "Köppen Humidity Index",
      out_col = "koppen_humidity",
      requires = c("tair_dry_bulb_c", "rh_mean_porc"),
      fun = .compute_koppen_humidity
    ),
    
    diurnal_range = list(
      name = "Diurnal Temperature Range",
      out_col = "diurnal_range_c",
      requires = c("tair_max_c", "tair_min_c"),
      fun = .compute_diurnal_range
    ),
    
    vapor_pressure = list(
      name = "Actual Vapor Pressure",
      out_col = "vapor_pressure_kpa",
      requires = c("tair_dry_bulb_c", "rh_mean_porc"),
      fun = .compute_vapor_pressure
    ),
    
    heat_stress_risk = list(
      name = "Heat Stress Risk Level",
      out_col = "heat_stress_risk",
      requires = c("tair_dry_bulb_c", "rh_mean_porc"),
      fun = .compute_heat_stress_risk
    )
  )

  # ===========================================================
  # REGION-SPECIFIC ADAPTATIONS
  # ===========================================================
  .region_params <- list(
    amazon = list(
      name = "Amazon",
      hi_min_temp = 24.0,      # Lower threshold for tropics
      hi_min_rh = 40,
      wbgt_high_warning = 28,  # Lower threshold for heat stress
      wbgt_low_warning = 20,
      gdd_base = 15,           # Higher base for tropical crops
      gdd_upper = 35,
      cdd_base = 20,           # Higher cooling threshold
      hdd_base = 18,
      utci_adapt = "tropical"
    ),
    cerrado = list(
      name = "Cerrado",
      hi_min_temp = 25.5,
      hi_min_rh = 35,
      wbgt_high_warning = 30,
      wbgt_low_warning = 18,
      gdd_base = 12,
      gdd_upper = 32,
      cdd_base = 19,
      utci_adapt = "savanna"
    ),
    caatinga = list(
      name = "Caatinga",
      hi_min_temp = 25.0,      # Semiarid adaptation
      hi_min_rh = 30,          # Lower humidity threshold
      wbgt_high_warning = 32,
      wbgt_low_warning = 22,
      gdd_base = 12,
      gdd_upper = 34,
      cdd_base = 20,
      utci_adapt = "arid"
    ),
    atlantic_forest = list(
      name = "Atlantic Forest",
      hi_min_temp = 25.0,
      hi_min_rh = 40,
      wbgt_high_warning = 29,
      wbgt_low_warning = 18,
      gdd_base = 12,
      gdd_upper = 32,
      cdd_base = 19,
      utci_adapt = "humid_tropical"
    ),
    pampa = list(
      name = "Pampa",
      hi_min_temp = 26.0,
      hi_min_rh = 40,
      wbgt_high_warning = 30,
      wbgt_low_warning = 15,
      gdd_base = 8,            # Lower base for temperate crops
      gdd_upper = 30,
      cdd_base = 18,
      hdd_base = 15,           # Heating needed in winter
      utci_adapt = "temperate"
    ),
    pantanal = list(
      name = "Pantanal",
      hi_min_temp = 24.5,
      hi_min_rh = 35,
      wbgt_high_warning = 31,
      wbgt_low_warning = 19,
      gdd_base = 14,
      gdd_upper = 34,
      cdd_base = 20,
      utci_adapt = "wetland"
    ),
    southeast = list(
      name = "Southeast",
      hi_min_temp = 25.5,
      hi_min_rh = 40,
      wbgt_high_warning = 30,
      wbgt_low_warning = 17,
      gdd_base = 10,
      gdd_upper = 32,
      cdd_base = 19,
      utci_adapt = "subtropical"
    ),
    south = list(
      name = "South",
      hi_min_temp = 26.0,
      hi_min_rh = 40,
      wbgt_high_warning = 29,
      wbgt_low_warning = 14,
      gdd_base = 8,
      gdd_upper = 30,
      cdd_base = 18,
      hdd_base = 14,
      utci_adapt = "temperate"
    )
  )

  # ===========================================================
  # AUTO-DETECT REGION
  # ===========================================================
  region <- tolower(region)
  
  if (region == "auto") {
    region <- .detect_region_from_data(df, station_col, verbose)
    if (verbose) {
      cli::cli_alert_info(
        sprintf("Região auto-detectada: %s", 
                .region_params[[region]]$name %||% region)
      )
    }
  }
  
  # Get region-specific parameters
  region_params <- .region_params[[region]] %||% .region_params$southeast
  
  # Merge custom thresholds if provided
  if (!is.null(custom_thresholds)) {
    region_params <- utils::modifyList(region_params, custom_thresholds)
  }

  # ===========================================================
  # COLUMN DETECTION
  # ===========================================================
  messages <- .get_indicators_messages(lang)
  source_sys <- .detect_data_source(df)
  datetime_col <- .detect_datetime_column(df, datetime_col, verbose, messages)
  station_col <- .detect_station_column(df, station_col, source_sys, verbose, messages)

  # ===========================================================
  # RESOLVE INDICATORS
  # ===========================================================
  if (identical(tolower(indicators), "all")) {
    indicators <- names(.indicator_registry)
  } else {
    indicators <- tolower(trimws(indicators))
    bad_codes <- setdiff(indicators, names(.indicator_registry))
    if (length(bad_codes) > 0) {
      cli::cli_abort(
        sprintf("Unknown indicators: %s. Available: %s",
                paste(bad_codes, collapse = ", "),
                paste(names(.indicator_registry), collapse = ", "))
      )
    }
  }

  # ===========================================================
  # PREPARE DATA
  # ===========================================================
  df_work <- df
  if (datetime_col != "date") {
    df_work <- df_work %>%
      dplyr::rename(date = !!rlang::sym(datetime_col))
  }

  # ===========================================================
  # COMPUTE INDICATORS
  # ===========================================================
  result <- df_work %>%
    dplyr::select(date, dplyr::all_of(station_col))
  
  indicator_stats <- list()

  for (ind in indicators) {
    reg <- .indicator_registry[[ind]]
    required <- reg$requires
    
    # Check for required variables
    missing_vars <- setdiff(required, colnames(df_work))
    
    if (length(missing_vars) > 0) {
      if (verbose) {
        cli::cli_alert_warning(
          sprintf("Indicator '%s' skipped - missing: %s",
                  toupper(ind), paste(missing_vars, collapse = ", "))
        )
      }
      result[[reg$out_col]] <- NA_real_
      if (confidence_flags) {
        result[[paste0(reg$out_col, "_missing_vars")]] <- TRUE
      }
      next
    }
    
    # Compute indicator with region adaptation
    if ("fun" %in% names(reg)) {
      # Get parameters for this indicator
      ind_params <- list()
      if (ind == "hi") {
        ind_params$min_temp <- region_params$hi_min_temp
        ind_params$min_rh <- region_params$hi_min_rh
      } else if (ind == "wbgt") {
        ind_params$high_warning <- region_params$wbgt_high_warning
        ind_params$low_warning <- region_params$wbgt_low_warning
      } else if (ind %in% c("gdd", "cdd", "hdd")) {
        ind_params$base_temp <- region_params[[paste0(ind, "_base")]] %||% 
                                reg$thresholds$base_temp
        ind_params$upper_temp <- region_params[[paste0(ind, "_upper")]] %||%
                                reg$thresholds$upper_temp
      } else if (ind == "utci") {
        ind_params$adapt <- region_params$utci_adapt
      }
      
      # Call the indicator function
      # result[[reg$out_col]] <- reg$fun(
      #   df_work,
      #   params = ind_params,
      #   confidence_flags = confidence_flags
      # )
      # Chamada corrigida
      result[[reg$out_col]] <- reg$fun(
        df_work,
        params = ind_params,
        confidence_flags = confidence_flags
      )
            
      # Add confidence flags if requested
      if (confidence_flags && !is.null(reg$thresholds)) {
        flag_col <- paste0(reg$out_col, "_out_of_range")
        result[[flag_col]] <- .check_indicator_validity(
          result[[reg$out_col]], 
          reg$thresholds,
          region_params
        )
      }
    }
    
    # Statistics
    n_valid <- sum(!is.na(result[[reg$out_col]]))
    indicator_stats[[ind]] <- list(
      computed = TRUE,
      name = reg$name,
      out_col = reg$out_col,
      n_valid = n_valid,
      pct_valid = round(n_valid / nrow(result) * 100, 2),
      region_adapted = region,
      thresholds_used = region_params[names(region_params) %in% 
                                      c("hi_min_temp", "hi_min_rh", "wbgt_high_warning")]
    )
    
    if (verbose) {
      cli::cli_alert_success(
        sprintf("✓ %s (%s): %.1f%% valid values", 
                toupper(ind), reg$name, indicator_stats[[ind]]$pct_valid)
      )
    }
  }

  # ===========================================================
  # FINALIZE OUTPUT
  # ===========================================================
  if (keep_source_vars) {
    all_required <- unique(unlist(lapply(indicators, function(i) 
      .indicator_registry[[i]]$requires)))
    source_cols <- intersect(all_required, colnames(df_work))
    result <- result %>%
      dplyr::left_join(
        df_work %>% dplyr::select(date, dplyr::all_of(station_col), dplyr::all_of(source_cols)),
        by = c("date", station_col)
      )
  }

  # Restore datetime column name
  if (datetime_col != "date") {
    result <- result %>%
      dplyr::rename(!!rlang::sym(datetime_col) := date)
  }

  # Add metadata
  date_col_final <- if (datetime_col != "date") datetime_col else "date"
  
  meta <- list(
    system = NULL,
    stage = "climate",
    type = "indicators",
    spatial = inherits(result, "sf"),
    temporal = list(
      start = min(result[[date_col_final]], na.rm = TRUE),
      end = max(result[[date_col_final]], na.rm = TRUE),
      source = source_sys,
      indicators_requested = indicators,
      region_adaptation = region,
      region_params = region_params,
      indicator_stats = indicator_stats,
      confidence_flags = confidence_flags
    ),
    created = Sys.time(),
    modified = Sys.time(),
    history = sprintf(
      "[%s] Adaptive indicators for %s region: %s",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      region,
      paste(toupper(indicators), collapse = ", ")
    )
  )

  base_classes <- setdiff(class(result), "climasus_df")
  result <- structure(
    result,
    sus_meta = meta,
    class = c("climasus_df", base_classes)
  )

  return(result)
}

# ============================================================
# REGION DETECTION HELPER
# ============================================================

#' Detect Brazilian region from station data
#' @keywords internal
#' @noRd
.detect_region_from_data <- function(df, station_col, verbose = TRUE) {
  
  # Try to detect from UF column
  if ("UF" %in% colnames(df)) {
    uf_regions <- list(
      amazon = c("AC", "AM", "AP", "PA", "RO", "RR", "TO"),
      cerrado = c("DF", "GO", "MT", "MS"),
      caatinga = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE"),
      atlantic_forest = c("ES", "RJ", "SP"),
      pampa = c("RS"),
      pantanal = c("MT", "MS"),  # Pantanal spans these
      southeast = c("MG", "RJ", "SP", "ES"),
      south = c("PR", "SC", "RS")
    )
    
    # Get most common UF
    uf_mode <- df %>%
      dplyr::filter(!is.na(UF)) %>%
      dplyr::count(UF, sort = TRUE) %>%
      dplyr::pull(UF) %>%
      .[1]
    
    for (region in names(uf_regions)) {
      if (uf_mode %in% uf_regions[[region]]) {
        if (region == "cerrado" && uf_mode %in% c("MT", "MS")) {
          # Check if it's Cerrado or Pantanal
          if (verbose) cli::cli_alert_info("Ambiguous region: checking coordinates...")
          return(.check_pantanal_vs_cerrado(df))
        }
        return(region)
      }
    }
  }
  
  # Try to detect from latitude
  if ("latitude" %in% colnames(df)) {
    avg_lat <- mean(df$latitude, na.rm = TRUE)
    
    if (avg_lat < -30) return("pampa")
    if (avg_lat < -25) return("south")
    if (avg_lat < -15) return("southeast")
    if (avg_lat < -5) return("cerrado")
    return("amazon")
  }
  
  # Default to southeast (most conservative)
  cli::cli_alert_warning("Could not detect region, defaulting to 'southeast'")
  return("southeast")
}

.check_pantanal_vs_cerrado <- function(df) {
  # Check longitude for Pantanal region
  if ("longitude" %in% colnames(df)) {
    avg_lon <- mean(df$longitude, na.rm = TRUE)
    # Pantanal roughly between -58 to -55 longitude
    if (avg_lon < -55 && avg_lon > -58) return("pantanal")
  }
  return("cerrado")
}

# ============================================================
# ADAPTED INDICATOR FUNCTIONS
# ============================================================

#' Compute WBGT with regional thresholds
#' @keywords internal
#' @noRd
.compute_wbgt_adaptive <- function(df, params = NULL, confidence_flags = NULL) {
  airT <- df$tair_dry_bulb_c
  rh <- df$rh_mean_porc
  sr <- df$sr_kj_m2
  ws <- df$ws_2_m_s
  
  # Standard calculation
  sr_wm2 <- sr / 3.6
  tnw <- airT + 0.33 * (rh / 100) * exp(0.0514 * airT) - 4.0
  ws_safe <- pmax(ws, 0.1)
  tg <- airT + 0.0144 * pmax(sr_wm2, 0)^0.6 / ws_safe^0.2 - 2.0
  wbgt <- 0.7 * tnw + 0.2 * tg + 0.1 * airT
  
  return(round(wbgt, 2))
}

#' Compute Heat Index with regional min temperature
#' @keywords internal
#' @noRd
.compute_hi_adaptive <- function(df, params = NULL, confidence_flags = NULL) {
  airT <- df$tair_dry_bulb_c
  rh <- df$rh_mean_porc
  
  # Use region-specific min temp if provided
  min_temp <- params$min_temp %||% 26.7
  min_rh <- params$min_rh %||% 40
  
  # Always calculate (no masking)
  airT_f <- (airT * 9 / 5) + 32
  
  # Protect against sqrt of negative numbers
  sqrt_term <- 17 - abs(airT_f - 95)
  sqrt_term <- pmax(sqrt_term, 0)  # Avoid negative values
  
  hi_f <- (-42.379 +
           2.04901523 * airT_f +
           10.14333127 * rh -
           0.22475541 * airT_f * rh -
           6.83783e-3 * airT_f^2 -
           5.481717e-2 * rh^2 +
           1.22874e-3 * airT_f^2 * rh +
           8.5282e-4 * airT_f * rh^2 -
           1.99e-6 * airT_f^2 * rh^2)
  
  # Adjustments for low/high humidity (with safety checks)
  if (any(rh < 13 & airT_f >= 80 & airT_f <= 112, na.rm = TRUE)) {
    adj_low_rh <- (13 - rh) / 4 * sqrt(sqrt_term / 17)
    low_rh_mask <- rh < 13 & airT_f >= 80 & airT_f <= 112 & !is.na(adj_low_rh)
    hi_f[low_rh_mask] <- hi_f[low_rh_mask] - adj_low_rh[low_rh_mask]
  }
  
  adj_high_rh <- (rh - 85) / 10 * ((87 - airT_f) / 5)
  high_rh_mask <- rh > 85 & airT_f >= 80 & airT_f <= 87
  hi_f[high_rh_mask] <- hi_f[high_rh_mask] + adj_high_rh[high_rh_mask]
  
  hi_c <- (hi_f - 32) * 5 / 9
  
  return(round(hi_c, 2))
}

#' Compute THI (Temperature-Humidity Index)
#' @keywords internal
#' @noRd
.compute_thi <- function(df, params = NULL, confidence_flags = NULL) {
  airT <- df$tair_dry_bulb_c
  rh <- df$rh_mean_porc
  thi <- airT - ((1 - rh / 100) * (airT - 14.4)) / 2
  return(round(thi, 2))
}

#' Compute WCET (Wind Chill - Canadian)
#' @keywords internal
#' @noRd
.compute_wcet <- function(df, params = NULL, confidence_flags = NULL) {
  airT <- df$tair_dry_bulb_c
  ws <- df$ws_2_m_s
  ws_kph <- ws * 3.6
  wcet <- 13.12 + (0.6215 * airT) - (11.37 * ws_kph^0.16) +
          (0.3965 * airT * ws_kph^0.16)
  return(round(wcet, 2))
}

#' Compute WCT (Wind Chill - US NWS)
#' @keywords internal
#' @noRd
.compute_wct <- function(df, params = NULL, confidence_flags = NULL) {
  airT <- df$tair_dry_bulb_c
  ws <- df$ws_2_m_s
  airT_f <- (airT * 9 / 5) + 32
  ws_mph <- ws * 2.23694  # m/s to mph
  wct_f <- 35.74 + 0.6215 * airT_f - 35.75 * ws_mph^0.16 +
           0.4275 * airT_f * ws_mph^0.16
  wct_c <- (wct_f - 32) * 5 / 9
  return(round(wct_c, 2))
}

#' Compute ET (Effective Temperature)
#' @keywords internal
#' @noRd
.compute_et <- function(df, params = NULL, confidence_flags = NULL) {
  airT <- df$tair_dry_bulb_c
  rh <- df$rh_mean_porc
  ws <- df$ws_2_m_s
  ws_safe <- pmax(ws, 0.04)
  et <- airT - 0.4 * (airT - 10) * (1 - rh / 100) - 1.1 * (ws_safe - 0.2)^0.5
  et <- pmax(et, -273.15)  # Physical lower bound
  return(round(et, 2))
}

#' Compute Cooling Degree Days
#' @keywords internal
#' @noRd
.compute_cdd <- function(df, params = NULL, confidence_flags = NULL) {
  airT <- df$tair_dry_bulb_c
  base_temp <- params$base_temp %||% 18
  cdd <- pmax(airT - base_temp, 0)
  return(round(cdd, 1))
}

#' Compute Heating Degree Days
#' @keywords internal
#' @noRd
.compute_hdd <- function(df, params = NULL, confidence_flags = NULL) {
  airT <- df$tair_dry_bulb_c
  base_temp <- params$base_temp %||% 18
  hdd <- pmax(base_temp - airT, 0)
  return(round(hdd, 1))
}

#' Compute Growing Degree Days
#' @keywords internal
#' @noRd
.compute_gdd <- function(df, params = NULL, confidence_flags = NULL) {
  airT <- df$tair_dry_bulb_c
  base_temp <- params$base_temp %||% 10
  upper_temp <- params$upper_temp %||% 30
  
  # Modified GDD with upper threshold
  t_adj <- pmin(pmax(airT, base_temp), upper_temp)
  gdd <- t_adj - base_temp
  
  return(round(gdd, 1))
}

#' Compute Simplified UTCI (Universal Thermal Climate Index)
#' @keywords internal
#' @noRd
.compute_utci_simplified <- function(df, params = NULL, confidence_flags = NULL) {
  # Simplified UTCI based on multiple regression
  # Reference: Bröde et al. (2012)
  
  airT <- df$tair_dry_bulb_c
  rh <- df$rh_mean_porc
  ws <- df$ws_2_m_s
  sr <- df$sr_kj_m2
  
  # Convert solar radiation to W/m²
  sr_wm2 <- sr / 3.6
  
  # Mean radiant temperature approximation
  t_mrt <- airT + 0.06 * sr_wm2 - 2
  
  # Wind speed at 10m height (approx)
  ws_10m <- ws * 1.5
  
  # Simplified UTCI regression
  utci <- airT + 
    0.05 * (rh - 50) - 
    0.2 * ws_10m + 
    0.01 * (t_mrt - airT) * 2
  
  # Apply regional adaptation if specified
  if (!is.null(params$adapt)) {
    if (params$adapt == "tropical") {
      utci <- utci + 1  # Tropical adjustment
    } else if (params$adapt == "arid") {
      utci <- utci - 0.5  # Arid adjustment
    }
  }
  
  return(round(utci, 2))
}

#' Compute Simplified PET (Physiological Equivalent Temperature)
#' @keywords internal
#' @noRd
.compute_pet_simplified <- function(df, params = NULL, confidence_flags = NULL) {
  # Simplified PET based on MEMI model approximation
  airT <- df$tair_dry_bulb_c
  rh <- df$rh_mean_porc
  ws <- df$ws_2_m_s
  sr <- df$sr_kj_m2
  
  # Convert solar radiation to W/m²
  sr_wm2 <- sr / 3.6
  
  # Simplified PET calculation
  pet <- airT + 
    0.1 * (rh - 50) / 10 -
    0.3 * ws +
    0.05 * sr_wm2 / 100
  
  return(round(pet, 2))
}

#' Compute Köppen Humidity Index
#' @keywords internal
#' @noRd
.compute_koppen_humidity <- function(df, params = NULL, confidence_flags = NULL) {
  # Simplified humidity classification
  rh <- df$rh_mean_porc
  
  dplyr::case_when(
    rh < 30 ~ "Arid",
    rh < 50 ~ "Semi-arid",
    rh < 70 ~ "Humid",
    TRUE ~ "Perhumid"
  )
}

#' Compute Diurnal Temperature Range
#' @keywords internal
#' @noRd
.compute_diurnal_range <- function(df, params = NULL, confidence_flags = NULL) {
  if (!all(c("tair_max_c", "tair_min_c") %in% colnames(df))) {
    return(NA_real_)
  }
  dtr <- df$tair_max_c - df$tair_min_c
  return(round(dtr, 2))
}

#' Compute Actual Vapor Pressure
#' @keywords internal
#' @noRd
.compute_vapor_pressure <- function(df, params = NULL, confidence_flags = NULL) {
  airT <- df$tair_dry_bulb_c
  rh <- df$rh_mean_porc
  
  # Saturation vapor pressure (Tetens formula)
  es <- 0.6108 * exp((17.27 * airT) / (airT + 237.3))
  
  # Actual vapor pressure
  ea <- es * (rh / 100)
  
  return(round(ea, 2))
}

#' Compute Heat Stress Risk Level
#' @keywords internal
#' @noRd
.compute_heat_stress_risk <- function(df, params = NULL, confidence_flags = NULL) {
  airT <- df$tair_dry_bulb_c
  rh <- df$rh_mean_porc
  
  # Simplified risk levels
  dplyr::case_when(
    airT > 40 | (airT > 35 & rh > 80) ~ "Extreme",
    airT > 35 | (airT > 30 & rh > 85) ~ "High",
    airT > 30 | (airT > 28 & rh > 80) ~ "Moderate",
    airT > 26 ~ "Low",
    TRUE ~ "None"
  )
}

#' Check indicator validity based on thresholds
#' @keywords internal
#' @noRd
.check_indicator_validity <- function(values, thresholds, region_params) {
  # Returns TRUE for values outside recommended ranges
  flags <- rep(FALSE, length(values))
  
  if (!is.null(thresholds$warning_high)) {
    flags <- flags | (values > thresholds$warning_high)
  }
  if (!is.null(thresholds$warning_low)) {
    flags <- flags | (values < thresholds$warning_low)
  }
  
  return(flags)
}