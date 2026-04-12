#' @title Compute Bioclimatic and Thermal Stress Indicators
#'
#' @description
#' `sus_climate_compute_indicators()` calculates a comprehensive set of
#' bioclimatic and thermal-stress indices from INMET station data, typically
#' produced by `sus_climate_inmet()` or gap-filled by `sus_climate_fill()`.
#'
#' Supports **optional regional adaptation** for Brazilian biomes (Amazon,
#' Cerrado, Caatinga, Atlantic Forest, Pampa, Pantanal), confidence flags,
#' and an extensible indicator registry.
#'
#' **Available indicators:**
#' \itemize{
#'   \item **WBGT** – Wet Bulb Globe Temperature: integrates air temperature,
#'         humidity and solar radiation. Reference for occupational heat stress.
#'   \item **HI** – Heat Index (Rothfusz / NWS): apparent temperature felt by
#'         the human body under shade. Valid for T ≥ 26.7 °C and RH ≥ 40%
#'         (threshold is regionally adjusted when `region != "none"`).
#'   \item **THI** – Temperature-Humidity Index: simplified discomfort index
#'         widely used in agronomy and veterinary science.
#'   \item **WCET** – Wind Chill Equivalent Temperature (Canadian formula):
#'         perceived cold under wind exposure. Valid for T ≤ 10 °C.
#'   \item **WCT** – Wind Chill Temperature (US/NWS formula): alternative
#'         formulation using mph. Valid for T ≤ 10 °C.
#'   \item **ET** – Effective Temperature: comfort index combining temperature,
#'         humidity and wind.
#'   \item **UTCI** – Universal Thermal Climate Index (simplified regression):
#'         multi-node human heat balance model.
#'   \item **PET** – Physiological Equivalent Temperature (simplified MEMI):
#'         outdoor thermal comfort index.
#'   \item **CDD** – Cooling Degree Days: energy demand for cooling above a
#'         base temperature (default 18 °C, region-adjusted).
#'   \item **HDD** – Heating Degree Days: energy demand for heating below a
#'         base temperature (default 18 °C, region-adjusted).
#'   \item **GDD** – Growing Degree Days: accumulated heat for crop development
#'         (base and upper temperatures are region-adjusted).
#'   \item **diurnal_range** – Diurnal Temperature Range (Tmax − Tmin).
#'   \item **vapor_pressure** – Actual Vapour Pressure (kPa, Tetens formula).
#'   \item **heat_stress_risk** – Categorical heat-stress risk level.
#'   \item **koppen_humidity** – Simplified Köppen humidity classification.
#' }
#'
#' @param df
#'   A data frame or tibble with INMET climate data. Typically the output of
#'   `sus_climate_inmet()` or `sus_climate_fill()`. Must contain a datetime
#'   column and at least the variables required by the requested indicators.
#'   \tabular{ll}{
#'     **Indicator** \tab **Required columns** \cr
#'     WBGT         \tab `tair_dry_bulb_c`, `rh_mean_porc`, `sr_kj_m2`, `ws_2_m_s` \cr
#'     HI           \tab `tair_dry_bulb_c`, `rh_mean_porc` \cr
#'     THI          \tab `tair_dry_bulb_c`, `rh_mean_porc` \cr
#'     WCET         \tab `tair_dry_bulb_c`, `ws_2_m_s` \cr
#'     WCT          \tab `tair_dry_bulb_c`, `ws_2_m_s` \cr
#'     ET           \tab `tair_dry_bulb_c`, `rh_mean_porc`, `ws_2_m_s` \cr
#'     UTCI         \tab `tair_dry_bulb_c`, `rh_mean_porc`, `ws_2_m_s`, `sr_kj_m2` \cr
#'     PET          \tab `tair_dry_bulb_c`, `rh_mean_porc`, `ws_2_m_s`, `sr_kj_m2` \cr
#'     CDD / HDD / GDD  \tab `tair_dry_bulb_c` \cr
#'     diurnal_range    \tab `tair_max_c`, `tair_min_c` \cr
#'     vapor_pressure   \tab `tair_dry_bulb_c`, `rh_mean_porc` \cr
#'     heat_stress_risk \tab `tair_dry_bulb_c`, `rh_mean_porc` \cr
#'     koppen_humidity  \tab `rh_mean_porc`
#'   }
#'
#' @param indicators
#'   Character vector of indicator codes to compute, or `"all"` (default).
#'   Valid codes (case-insensitive): `"wbgt"`, `"hi"`, `"thi"`, `"wcet"`,
#'   `"wct"`, `"et"`, `"utci"`, `"pet"`, `"cdd"`, `"hdd"`, `"gdd"`,
#'   `"diurnal_range"`, `"vapor_pressure"`, `"heat_stress_risk"`,
#'   `"koppen_humidity"`.
#'
#' @param region
#'   Character. Brazilian biome/region for parameter adaptation. Use `"none"`
#'   (default) to apply standard international thresholds, or one of:
#'   `"auto"`, `"amazon"`, `"cerrado"`, `"caatinga"`, `"atlantic_forest"`,
#'   `"pampa"`, `"pantanal"`, `"southeast"`, `"south"`. When `"auto"`, the
#'   region is inferred from the `UF` or `latitude` columns if present,
#'   defaulting to `"southeast"`.
#'
#' @param datetime_col
#'   Character. Name of the datetime column. If `NULL` (default), auto-detected.
#'
#' @param station_col
#'   Character. Name of the station identifier column. If `NULL` (default),
#'   auto-detected.
#'
#' @param apply_validity_mask
#'   Logical. If `TRUE` (default) and `region == "none"`, values outside the
#'   physical domain of each index are replaced with `NA`. When a `region` is
#'   set, out-of-range values are flagged via `confidence_flags` instead of
#'   being masked, so all values are always returned.
#'   \itemize{
#'     \item HI  : `NA` when T < 26.7 °C (or region threshold) or RH < 40%
#'     \item WCET / WCT : `NA` when T > 10 °C or ws ≤ 1.3 m/s
#'   }
#'
#' @param confidence_flags
#'   Logical. If `TRUE` (default when `region != "none"`), adds boolean
#'   `<indicator>_out_of_range` columns flagging values outside the
#'   recommended physical domain for each indicator. When `region == "none"`
#'   and `apply_validity_mask = TRUE`, masking takes precedence and flags are
#'   not added unless this parameter is explicitly set to `TRUE`.
#'
#' @param custom_thresholds
#'   Named list. Override region-specific thresholds per indicator.
#'   Example: `list(hi = list(min_temp = 24, min_rh = 35),
#'                  gdd = list(base_temp = 12, upper_temp = 34))`.
#'   Only used when `region != "none"`.
#'
#' @param keep_source_vars
#'   Logical. If `TRUE`, the source climate variables used in calculations
#'   are retained alongside the new indicator columns. Default: `FALSE`.
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
#'   \item One column per computed indicator (e.g. `wbgt_c`, `hi_c`, `thi_c`,
#'         `wcet_c`, `wct_c`, `et_c`, `utci_c`, `pet_c`, `cdd_c`, `hdd_c`,
#'         `gdd_c`, `diurnal_range_c`, `vapor_pressure_kpa`,
#'         `heat_stress_risk`, `koppen_humidity`)
#'   \item `<col>_out_of_range` flag columns (when `confidence_flags = TRUE`)
#'   \item Source climate variables (when `keep_source_vars = TRUE`)
#'   \item A `sus_meta` attribute with computation metadata
#' }
#'
#' @section Scientific References:
#' \itemize{
#'   \item **WBGT**: Liljegren et al. (2008). *J. Occup. Environ. Hyg.* 5(10).
#'         Natural wet-bulb approximation: Bernard & Pourmoghani (1999).
#'   \item **HI**: Rothfusz (1990). *NWS Technical Attachment SR/SSD 90-23*.
#'   \item **THI**: Thom (1959). *Weatherwise* 12(2): 57-59.
#'   \item **WCET**: Environment Canada (2001). Wind Chill Index.
#'   \item **WCT**: NWS Wind Chill Temperature Index (2001).
#'   \item **ET**: Missenard (1937), via Auliciems & Szokolay (2007).
#'   \item **UTCI**: Bröde et al. (2012). *Int. J. Biometeorol.* 56(4): 481-494.
#'   \item **PET**: Matzarakis et al. (1999). MEMI model.
#'   \item **Vapor pressure**: Tetens (1930) saturation formula.
#' }
#'
#' @examples
#' \dontrun{
#' # --- All standard indicators (no regional adaptation) ---
#' result <- sus_climate_compute_indicators(climate_data)
#'
#' # --- Select specific indicators ---
#' thermal <- sus_climate_compute_indicators(
#'   df         = climate_data,
#'   indicators = c("wbgt", "hi", "thi"),
#'   lang       = "pt"
#' )
#'
#' # --- Regional adaptation (Amazon biome) ---
#' result_amz <- sus_climate_compute_indicators(
#'   df               = climate_data,
#'   region           = "amazon",
#'   confidence_flags = TRUE
#' )
#'
#' # --- Auto-detect region + keep source vars ---
#' sus_climate_compute_indicators(
#'   df               = climate_data,
#'   region           = "auto",
#'   keep_source_vars = TRUE,
#'   verbose          = TRUE
#' )
#'
#' # --- Pipe from sus_climate_fill() ---
#' climate_data |>
#'   sus_climate_fill(target_var = "all") |>
#'   sus_climate_compute_indicators(
#'     indicators = c("wbgt", "hi", "utci"),
#'     region     = "cerrado"
#'   )
#' }
#'
#' @export
#' @importFrom rlang .data
sus_climate_compute_indicators <- function(
  df,
  indicators          = "all",
  region              = "none",
  datetime_col        = NULL,
  station_col         = NULL,
  apply_validity_mask = TRUE,
  confidence_flags    = NULL,   # NULL → auto (TRUE when region != "none")
  custom_thresholds   = NULL,
  keep_source_vars    = FALSE,
  verbose             = TRUE,
  lang                = "pt"
) {

  # -------------------------------------------------------------------------
  # 0. RESOLVE confidence_flags default
  # -------------------------------------------------------------------------
  use_region <- tolower(trimws(region)) != "none"
  if (is.null(confidence_flags)) {
    confidence_flags <- use_region
  }

  # -------------------------------------------------------------------------
  # 1. INDICATOR REGISTRY
  # -------------------------------------------------------------------------
  .indicator_registry <- list(

    wbgt = list(
      label    = "Wet Bulb Globe Temperature",
      out_col  = "wbgt_c",
      requires = c("tair_dry_bulb_c", "rh_mean_porc", "sr_kj_m2", "ws_2_m_s"),
      thresholds = list(warning_high = 31, warning_low = 15)
    ),

    hi = list(
      label    = "Heat Index",
      out_col  = "hi_c",
      requires = c("tair_dry_bulb_c", "rh_mean_porc"),
      thresholds = list(
        min_temp_default  = 26.7,
        min_temp_tropical = 24.0,
        min_rh            = 40
      )
    ),

    thi = list(
      label    = "Temperature-Humidity Index",
      out_col  = "thi_c",
      requires = c("tair_dry_bulb_c", "rh_mean_porc"),
      thresholds = list(comfortable = list(min = 20, max = 24), stress_high = 28)
    ),

    wcet = list(
      label    = "Wind Chill Equivalent Temperature (CA)",
      out_col  = "wcet_c",
      requires = c("tair_dry_bulb_c", "ws_2_m_s"),
      thresholds = list(max_temp = 10, min_wind = 1.3)
    ),

    wct = list(
      label    = "Wind Chill Temperature (NWS)",
      out_col  = "wct_c",
      requires = c("tair_dry_bulb_c", "ws_2_m_s"),
      thresholds = list(max_temp = 10, min_wind = 1.3)
    ),

    et = list(
      label    = "Effective Temperature",
      out_col  = "et_c",
      requires = c("tair_dry_bulb_c", "rh_mean_porc", "ws_2_m_s"),
      thresholds = list()
    ),

    utci = list(
      label    = "Universal Thermal Climate Index",
      out_col  = "utci_c",
      requires = c("tair_dry_bulb_c", "rh_mean_porc", "ws_2_m_s", "sr_kj_m2"),
      thresholds = list(
        heat_stress = list(extreme = 46, strong = 38, moderate = 32, slight = 26),
        cold_stress = list(extreme = -27, strong = -13, moderate = 0)
      )
    ),

    pet = list(
      label    = "Physiological Equivalent Temperature",
      out_col  = "pet_c",
      requires = c("tair_dry_bulb_c", "rh_mean_porc", "ws_2_m_s", "sr_kj_m2"),
      thresholds = list()
    ),

    cdd = list(
      label    = "Cooling Degree Days",
      out_col  = "cdd_c",
      requires = c("tair_dry_bulb_c"),
      thresholds = list(base_temp = 18)
    ),

    hdd = list(
      label    = "Heating Degree Days",
      out_col  = "hdd_c",
      requires = c("tair_dry_bulb_c"),
      thresholds = list(base_temp = 18)
    ),

    gdd = list(
      label    = "Growing Degree Days",
      out_col  = "gdd_c",
      requires = c("tair_dry_bulb_c"),
      thresholds = list(base_temp = 10, upper_temp = 30)
    ),

    diurnal_range = list(
      label    = "Diurnal Temperature Range",
      out_col  = "diurnal_range_c",
      requires = c("tair_dry_bulb_c"),          # fallback always available
      thresholds = list()
    ),

    vapor_pressure = list(
      label    = "Actual Vapour Pressure",
      out_col  = "vapor_pressure_kpa",
      requires = c("tair_dry_bulb_c", "rh_mean_porc"),
      thresholds = list()
    ),

    heat_stress_risk = list(
      label    = "Heat Stress Risk Level",
      out_col  = "heat_stress_risk",
      requires = c("tair_dry_bulb_c", "rh_mean_porc"),
      thresholds = list()
    ),

    koppen_humidity = list(
      label    = "Koppen Humidity Classification",
      out_col  = "koppen_humidity",
      requires = c("rh_mean_porc"),
      thresholds = list()
    )
  )

  .all_codes <- names(.indicator_registry)

  # -------------------------------------------------------------------------
  # 2. MESSAGES
  # -------------------------------------------------------------------------
  messages <- .get_indicators_messages(lang)

  # -------------------------------------------------------------------------
  # 3. COLUMN AUTO-DETECTION
  # -------------------------------------------------------------------------
  source_sys   <- .detect_data_source(df)
  datetime_col <- .detect_datetime_column(df, datetime_col, verbose, messages)
  station_col  <- .detect_station_column(df, station_col, source_sys, verbose, messages)

  # -------------------------------------------------------------------------
  # 4. RESOLVE INDICATORS
  # -------------------------------------------------------------------------
  if (identical(tolower(indicators), "all")) {
    indicators <- .all_codes
  } else {
    indicators <- tolower(trimws(indicators))
    bad_codes  <- setdiff(indicators, .all_codes)
    if (length(bad_codes) > 0) {
      cli::cli_abort(
        sprintf(
          messages$unknown_indicators,
          paste(bad_codes, collapse = ", "),
          paste(.all_codes, collapse = ", ")
        )
      )
    }
  }

  if (verbose) {
    cli::cli_alert_info(
      sprintf(
        messages$computing_indicators,
        length(indicators),
        paste(toupper(indicators), collapse = ", ")
      )
    )
  }

  # -------------------------------------------------------------------------
  # 5. REGIONAL PARAMETERS
  # -------------------------------------------------------------------------
  region <- tolower(trimws(region))

  .region_params <- list(
    amazon = list(
      name               = "Amazon",
      hi_min_temp        = 24.0,
      hi_min_rh          = 40,
      wbgt_high_warning  = 28,
      wbgt_low_warning   = 20,
      gdd_base           = 15,
      gdd_upper          = 35,
      cdd_base           = 20,
      hdd_base           = 18,
      utci_adapt         = "tropical"
    ),
    cerrado = list(
      name               = "Cerrado",
      hi_min_temp        = 25.5,
      hi_min_rh          = 35,
      wbgt_high_warning  = 30,
      wbgt_low_warning   = 18,
      gdd_base           = 12,
      gdd_upper          = 32,
      cdd_base           = 19,
      hdd_base           = 18,
      utci_adapt         = "savanna"
    ),
    caatinga = list(
      name               = "Caatinga",
      hi_min_temp        = 25.0,
      hi_min_rh          = 30,
      wbgt_high_warning  = 32,
      wbgt_low_warning   = 22,
      gdd_base           = 12,
      gdd_upper          = 34,
      cdd_base           = 20,
      hdd_base           = 18,
      utci_adapt         = "arid"
    ),
    atlantic_forest = list(
      name               = "Atlantic Forest",
      hi_min_temp        = 25.0,
      hi_min_rh          = 40,
      wbgt_high_warning  = 29,
      wbgt_low_warning   = 18,
      gdd_base           = 12,
      gdd_upper          = 32,
      cdd_base           = 19,
      hdd_base           = 18,
      utci_adapt         = "humid_tropical"
    ),
    pampa = list(
      name               = "Pampa",
      hi_min_temp        = 26.0,
      hi_min_rh          = 40,
      wbgt_high_warning  = 30,
      wbgt_low_warning   = 15,
      gdd_base           = 8,
      gdd_upper          = 30,
      cdd_base           = 18,
      hdd_base           = 15,
      utci_adapt         = "temperate"
    ),
    pantanal = list(
      name               = "Pantanal",
      hi_min_temp        = 24.5,
      hi_min_rh          = 35,
      wbgt_high_warning  = 31,
      wbgt_low_warning   = 19,
      gdd_base           = 14,
      gdd_upper          = 34,
      cdd_base           = 20,
      hdd_base           = 18,
      utci_adapt         = "wetland"
    ),
    southeast = list(
      name               = "Southeast",
      hi_min_temp        = 25.5,
      hi_min_rh          = 40,
      wbgt_high_warning  = 30,
      wbgt_low_warning   = 17,
      gdd_base           = 10,
      gdd_upper          = 32,
      cdd_base           = 19,
      hdd_base           = 18,
      utci_adapt         = "subtropical"
    ),
    south = list(
      name               = "South",
      hi_min_temp        = 26.0,
      hi_min_rh          = 40,
      wbgt_high_warning  = 29,
      wbgt_low_warning   = 14,
      gdd_base           = 8,
      gdd_upper          = 30,
      cdd_base           = 18,
      hdd_base           = 14,
      utci_adapt         = "temperate"
    )
  )

  if (region == "none") {
    region_params <- NULL
  } else {
    if (region == "auto") {
      region <- .detect_region_from_data(df, station_col, verbose)
      if (verbose) {
        cli::cli_alert_info(
          sprintf(
            if (lang == "pt") "Região auto-detectada: %s"
            else if (lang == "es") "Región auto-detectada: %s"
            else "Auto-detected region: %s",
            .region_params[[region]]$name %||% region
          )
        )
      }
    }

    valid_regions <- names(.region_params)
    if (!region %in% valid_regions) {
      cli::cli_warn(
        sprintf(
          "Unknown region '%s'. Defaulting to 'southeast'. Valid: %s",
          region,
          paste(valid_regions, collapse = ", ")
        )
      )
      region <- "southeast"
    }

    region_params <- .region_params[[region]]

    # Merge custom thresholds
    if (!is.null(custom_thresholds)) {
      region_params <- utils::modifyList(region_params, custom_thresholds)
    }
  }

  # -------------------------------------------------------------------------
  # 6. PREPARE WORKING DATA FRAME
  # -------------------------------------------------------------------------
  df_work <- df
  renamed_date <- FALSE
  if (!is.null(datetime_col) && datetime_col != "date") {
    df_work    <- dplyr::rename(df_work, date = !!rlang::sym(datetime_col))
    renamed_date <- TRUE
  }

  # Warn about variables absent from the whole data frame
  all_required     <- unique(unlist(lapply(indicators, function(i) .indicator_registry[[i]]$requires)))
  missing_globally <- setdiff(all_required, colnames(df_work))
  if (length(missing_globally) > 0 && verbose) {
    cli::cli_alert_warning(
      sprintf(messages$missing_vars_global, paste(missing_globally, collapse = ", "))
    )
  }

  # -------------------------------------------------------------------------
  # 7. INITIALISE RESULT TABLE
  # -------------------------------------------------------------------------
  id_cols <- c("date", station_col)
  id_cols <- id_cols[id_cols %in% colnames(df_work)]
  result  <- dplyr::select(df_work, dplyr::all_of(id_cols))

  indicator_stats <- list()

  # -------------------------------------------------------------------------
  # 8. COMPUTE EACH INDICATOR
  # -------------------------------------------------------------------------
  for (ind in indicators) {

    reg      <- .indicator_registry[[ind]]
    out_col  <- reg$out_col
    required <- reg$requires

    missing_for_ind <- setdiff(required, colnames(df_work))

    # --- Skip if required vars are absent ---
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

    # --- Build indicator-specific params ---
    ind_params <- .build_ind_params(ind, reg, region_params)

    # --- Compute ---
    values <- .dispatch_indicator(
      ind              = ind,
      df               = df_work,
      params           = ind_params,
      apply_mask       = apply_validity_mask && !use_region,
      use_region       = use_region,
      region_params    = region_params
    )

    result[[out_col]] <- values

    # --- Confidence flags ---
    if (confidence_flags && length(reg$thresholds) > 0) {
      flag_col          <- paste0(out_col, "_out_of_range")
      result[[flag_col]] <- .check_indicator_validity(values, reg$thresholds)
    }

    # --- Summary stats ---
    n_valid <- sum(!is.na(values))
    n_total <- length(values)

    indicator_stats[[ind]] <- list(
      computed       = TRUE,
      label          = reg$label,
      out_col        = out_col,
      n_valid        = n_valid,
      n_total        = n_total,
      pct_valid      = round(n_valid / n_total * 100, 2),
      range          = if (n_valid > 0 && is.numeric(values))
                         round(range(values, na.rm = TRUE), 2)
                       else if (n_valid > 0)
                         sort(unique(values[!is.na(values)]))   # categorical: sorted levels
                       else NA,
      region_adapted = if (use_region) region else "none"
    )

    if (verbose) {
      cli::cli_alert_success(
        sprintf(
          messages$indicator_done,
          toupper(ind),
          reg$label,
          out_col,
          n_valid,
          n_total,
          round(n_valid / n_total * 100, 1)
        )
      )
    }
  }

  # -------------------------------------------------------------------------
  # 9. OPTIONALLY ATTACH SOURCE VARIABLES
  # -------------------------------------------------------------------------
  if (keep_source_vars) {
    source_to_add <- intersect(all_required, colnames(df_work))
    if (length(source_to_add) > 0) {
      result <- dplyr::left_join(
        result,
        dplyr::select(df_work, dplyr::all_of(id_cols), dplyr::all_of(source_to_add)),
        by = id_cols
      )
    }
  }

  # -------------------------------------------------------------------------
  # 10. RESTORE ORIGINAL DATETIME COLUMN NAME
  # -------------------------------------------------------------------------
  if (renamed_date && !is.null(datetime_col)) {
    result <- dplyr::rename(result, !!rlang::sym(datetime_col) := date)
  }
  date_col_final <- if (renamed_date && !is.null(datetime_col)) datetime_col else "date"

  # -------------------------------------------------------------------------
  # 11. S3 CLASS + sus_meta ATTRIBUTE
  # -------------------------------------------------------------------------
  meta <- list(
    system  = NULL,
    stage   = "climate",
    type    = "indicators",
    spatial = inherits(result, "sf"),
    temporal = list(
      start                = min(result[[date_col_final]], na.rm = TRUE),
      end                  = max(result[[date_col_final]], na.rm = TRUE),
      source               = source_sys,
      indicators_requested = indicators,
      indicator_stats      = indicator_stats,
      keep_source_vars     = keep_source_vars,
      apply_validity_mask  = apply_validity_mask,
      confidence_flags     = confidence_flags,
      region_adaptation    = if (use_region) region else "none",
      region_params        = region_params
    ),
    created  = Sys.time(),
    modified = Sys.time(),
    history  = sprintf(
      "[%s] Bioclimatic indicators (%s): %s",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      if (use_region) paste("region:", region) else "standard",
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
      sprintf(
        messages$done,
        sum(vapply(indicator_stats, function(x) isTRUE(x$computed), logical(1))),
        length(indicators)
      )
    )
  }

  return(result)
}


# ============================================================================
# INTERNAL HELPERS — DISPATCH & PARAMETER BUILDING
# ============================================================================

#' Build indicator-specific parameters from regional config
#' @keywords internal
#' @noRd
.build_ind_params <- function(ind, reg, region_params) {
  params <- reg$thresholds  # start with registry defaults

  if (is.null(region_params)) return(params)

  if (ind == "hi") {
    params$min_temp <- region_params$hi_min_temp %||% reg$thresholds$min_temp_default
    params$min_rh   <- region_params$hi_min_rh   %||% reg$thresholds$min_rh
  } else if (ind == "wbgt") {
    params$high_warning <- region_params$wbgt_high_warning %||% reg$thresholds$warning_high
    params$low_warning  <- region_params$wbgt_low_warning  %||% reg$thresholds$warning_low
  } else if (ind %in% c("cdd", "hdd", "gdd")) {
    params$base_temp  <- region_params[[paste0(ind, "_base")]]  %||% reg$thresholds$base_temp
    params$upper_temp <- region_params[[paste0(ind, "_upper")]] %||% reg$thresholds$upper_temp
  } else if (ind == "utci") {
    params$adapt <- region_params$utci_adapt
  }

  params
}


#' Dispatch to the correct computation function
#' @keywords internal
#' @noRd
.dispatch_indicator <- function(ind, df, params, apply_mask, use_region, region_params) {

  # Determine HI validity threshold
  hi_min_temp <- if (use_region && !is.null(params$min_temp)) params$min_temp else 26.7
  hi_min_rh   <- if (use_region && !is.null(params$min_rh))   params$min_rh   else 40

  switch(
    ind,

    wbgt = .compute_wbgt(
      airT      = df[["tair_dry_bulb_c"]],
      rh        = df[["rh_mean_porc"]],
      sr_kj_m2  = df[["sr_kj_m2"]],
      ws        = df[["ws_2_m_s"]]
    ),

    hi = .compute_hi(
      airT       = df[["tair_dry_bulb_c"]],
      rh         = df[["rh_mean_porc"]],
      apply_mask = apply_mask,
      min_temp   = hi_min_temp,
      min_rh     = hi_min_rh
    ),

    thi = .compute_thi(
      airT = df[["tair_dry_bulb_c"]],
      rh   = df[["rh_mean_porc"]]
    ),

    wcet = .compute_wcet(
      airT       = df[["tair_dry_bulb_c"]],
      ws         = df[["ws_2_m_s"]],
      apply_mask = apply_mask
    ),

    wct = .compute_wct(
      airT       = df[["tair_dry_bulb_c"]],
      ws         = df[["ws_2_m_s"]],
      apply_mask = apply_mask
    ),

    et = .compute_et(
      airT = df[["tair_dry_bulb_c"]],
      rh   = df[["rh_mean_porc"]],
      ws   = df[["ws_2_m_s"]]
    ),

    utci = .compute_utci(
      airT   = df[["tair_dry_bulb_c"]],
      rh     = df[["rh_mean_porc"]],
      ws     = df[["ws_2_m_s"]],
      sr_kj_m2 = df[["sr_kj_m2"]],
      adapt  = params$adapt
    ),

    pet = .compute_pet(
      airT     = df[["tair_dry_bulb_c"]],
      rh       = df[["rh_mean_porc"]],
      ws       = df[["ws_2_m_s"]],
      sr_kj_m2 = df[["sr_kj_m2"]]
    ),

    cdd = .compute_cdd(
      airT      = df[["tair_dry_bulb_c"]],
      base_temp = params$base_temp %||% 18
    ),

    hdd = .compute_hdd(
      airT      = df[["tair_dry_bulb_c"]],
      base_temp = params$base_temp %||% 18
    ),

    gdd = .compute_gdd(
      airT       = df[["tair_dry_bulb_c"]],
      base_temp  = params$base_temp  %||% 10,
      upper_temp = params$upper_temp %||% 30
    ),

    diurnal_range = .compute_diurnal_range(
      df       = df,
      tmax_col = "tair_max_c",
      tmin_col = "tair_min_c"
    ),

    vapor_pressure = .compute_vapor_pressure(
      airT = df[["tair_dry_bulb_c"]],
      rh   = df[["rh_mean_porc"]]
    ),

    heat_stress_risk = .compute_heat_stress_risk(
      airT = df[["tair_dry_bulb_c"]],
      rh   = df[["rh_mean_porc"]]
    ),

    koppen_humidity = .compute_koppen_humidity(
      rh = df[["rh_mean_porc"]]
    ),

    stop(sprintf("No computation function registered for indicator '%s'.", ind))
  )
}


# ============================================================================
# INDICATOR COMPUTATION FUNCTIONS
# ============================================================================

#' Compute Wet Bulb Globe Temperature (WBGT)
#'
#' @description
#' Simplified WBGT using air temperature, relative humidity, solar radiation
#' and wind speed. Formula: WBGT = 0.7 * Tnw + 0.2 * Tg + 0.1 * Tdb
#'
#' Tnw approximation: Bernard & Pourmoghani (1999).
#' Tg  approximation: Liljegren (simplified).
#'
#' @param airT    Numeric. Air temperature (°C).
#' @param rh      Numeric. Relative humidity (%).
#' @param sr_kj_m2 Numeric. Solar radiation (kJ/m²).
#' @param ws      Numeric. Wind speed (m/s).
#' @return Numeric. WBGT in °C.
#' @keywords internal
#' @noRd
.compute_wbgt <- function(airT, rh, sr_kj_m2, ws) {

  # At night sr_kj_m2 is NA or 0 — treat as 0 (no solar load), not NA
  sr_safe <- ifelse(is.na(sr_kj_m2) | sr_kj_m2 < 0, 0, sr_kj_m2)
  sr_wm2  <- sr_safe / 3.6

  # NA wind speed treated as calm
  ws_filled <- ifelse(is.na(ws), 0, ws)
  ws_safe   <- pmax(ws_filled, 0.1)

  tnw  <- airT + 0.33 * (rh / 100) * exp(0.0514 * airT) - 4.0
  tg   <- airT + 0.0144 * sr_wm2^0.6 / ws_safe^0.2 - 2.0
  wbgt <- 0.7 * tnw + 0.2 * tg + 0.1 * airT

  round(wbgt, 2)
}


#' Compute Heat Index (Rothfusz / NWS)
#'
#' @description
#' Full Rothfusz (1990) regression equation with Steadman adjustments for low
#' and high humidity edge cases. When `apply_mask = TRUE`, values are set to
#' `NA` outside the valid range (T < min_temp or RH < min_rh).
#'
#' @param airT       Numeric. Air temperature (°C).
#' @param rh         Numeric. Relative humidity (%).
#' @param apply_mask Logical. If TRUE, mask invalid range with NA.
#' @param min_temp   Numeric. Minimum temperature threshold (°C, default 26.7).
#' @param min_rh     Numeric. Minimum RH threshold (%, default 40).
#' @return Numeric. Heat Index in °C.
#' @keywords internal
#' @noRd
.compute_hi <- function(airT, rh, apply_mask = TRUE, min_temp = 26.7, min_rh = 40) {

  airT_f <- (airT * 9 / 5) + 32

  # Rothfusz full regression
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

  # Steadman adjustment — low humidity (RH < 13 %, 80 ≤ T_F ≤ 112)
  sqrt_term   <- pmax(17 - abs(airT_f - 95), 0)   # guard against negative sqrt
  adj_low_rh  <- (13 - rh) / 4 * sqrt(sqrt_term / 17)
  low_rh_mask <- rh < 13 & airT_f >= 80 & airT_f <= 112 & !is.na(adj_low_rh)
  hi_f        <- ifelse(low_rh_mask, hi_f - adj_low_rh, hi_f)

  # Steadman adjustment — high humidity (RH > 85 %, 80 ≤ T_F ≤ 87)
  adj_high_rh  <- (rh - 85) / 10 * ((87 - airT_f) / 5)
  high_rh_mask <- rh > 85 & airT_f >= 80 & airT_f <= 87
  hi_f         <- ifelse(high_rh_mask, hi_f + adj_high_rh, hi_f)

  hi_c <- (hi_f - 32) * 5 / 9

  if (apply_mask) {
    hi_c <- ifelse(airT < min_temp | rh < min_rh, NA_real_, hi_c)
  }

  round(hi_c, 2)
}


#' Compute Temperature-Humidity Index (THI)
#'
#' @description Thom (1959): THI = Tdb - ((1 - RH/100) * (Tdb - 14.4)) / 2
#' @param airT Numeric. Air temperature (°C).
#' @param rh   Numeric. Relative humidity (%).
#' @return Numeric. THI (°C equivalent).
#' @keywords internal
#' @noRd
.compute_thi <- function(airT, rh) {
  round(airT - ((1 - rh / 100) * (airT - 14.4)) / 2, 2)
}


#' Compute Wind Chill Equivalent Temperature — Canadian formula (WCET)
#'
#' @description
#' Environment Canada (2001). Formula in km/h.
#' WCET = 13.12 + 0.6215*T - 11.37*V^0.16 + 0.3965*T*V^0.16
#'
#' @param airT       Numeric. Air temperature (°C).
#' @param ws         Numeric. Wind speed (m/s).
#' @param apply_mask Logical. If TRUE, NA when T > 10 °C or ws ≤ 1.3 m/s.
#' @return Numeric. WCET in °C.
#' @keywords internal
#' @noRd
.compute_wcet <- function(airT, ws, apply_mask = TRUE) {

  ws_kph <- ws * 3.6
  wcet   <- 13.12 + 0.6215 * airT - 11.37 * ws_kph^0.16 +
            0.3965 * airT * ws_kph^0.16

  if (apply_mask) {
    wcet <- ifelse(airT > 10 | ws <= 1.3, NA_real_, wcet)
  }

  round(wcet, 2)
}


#' Compute Wind Chill Temperature — NWS formula (WCT)
#'
#' @description
#' US National Weather Service (2001). Operates in °F / mph.
#' WCT_F = 35.74 + 0.6215*T_F - 35.75*V_mph^0.16 + 0.4275*T_F*V_mph^0.16
#'
#' @param airT       Numeric. Air temperature (°C).
#' @param ws         Numeric. Wind speed (m/s).
#' @param apply_mask Logical. If TRUE, NA when T > 10 °C or ws ≤ 1.3 m/s.
#' @return Numeric. WCT in °C.
#' @keywords internal
#' @noRd
.compute_wct <- function(airT, ws, apply_mask = TRUE) {

  airT_f <- (airT * 9 / 5) + 32
  ws_mph <- ws * 0.621371

  wct_f <- 35.74 + 0.6215 * airT_f - 35.75 * ws_mph^0.16 +
           0.4275 * airT_f * ws_mph^0.16

  wct_c <- (wct_f - 32) * 5 / 9

  if (apply_mask) {
    wct_c <- ifelse(airT > 10 | ws <= 1.3, NA_real_, wct_c)
  }

  round(wct_c, 2)
}


#' Compute Effective Temperature (ET)
#'
#' @description
#' Missenard (1937). ET = Tdb - 0.4*(Tdb-10)*(1-RH/100) - 1.1*(ws-0.2)^0.5
#' Wind speed floored at 0.04 m/s to avoid imaginary values.
#'
#' @param airT Numeric. Air temperature (°C).
#' @param rh   Numeric. Relative humidity (%).
#' @param ws   Numeric. Wind speed (m/s).
#' @return Numeric. ET in °C.
#' @keywords internal
#' @noRd
.compute_et <- function(airT, rh, ws) {

  # NA wind speed is treated as calm (0 m/s) — does not invalidate the index
  ws_filled <- ifelse(is.na(ws), 0, ws)
  ws_safe   <- pmax(ws_filled, 0.04)
  et        <- airT - 0.4 * (airT - 10) * (1 - rh / 100) - 1.1 * (ws_safe - 0.2)^0.5
  et        <- pmax(et, -273.15)

  round(et, 2)
}


#' Compute Simplified UTCI (Universal Thermal Climate Index)
#'
#' @description
#' Simplified UTCI regression based on Bröde et al. (2012). Includes optional
#' regional adaptation adjustments.
#'
#' @param airT     Numeric. Air temperature (°C).
#' @param rh       Numeric. Relative humidity (%).
#' @param ws       Numeric. Wind speed (m/s).
#' @param sr_kj_m2 Numeric. Solar radiation (kJ/m²).
#' @param adapt    Character or NULL. Regional adaptation type.
#' @return Numeric. UTCI in °C.
#' @keywords internal
#' @noRd
.compute_utci <- function(airT, rh, ws, sr_kj_m2, adapt = NULL) {

  # NA solar radiation = no solar load (night); NA wind = calm
  sr_safe   <- ifelse(is.na(sr_kj_m2) | sr_kj_m2 < 0, 0, sr_kj_m2)
  ws_filled <- ifelse(is.na(ws), 0, ws)

  sr_wm2 <- sr_safe / 3.6
  t_mrt  <- airT + 0.06 * sr_wm2 - 2         # mean radiant temperature approx
  ws_10m <- ws_filled * 1.5                   # approximate 10 m height

  utci <- airT +
    0.05  * (rh - 50) -
    0.2   * ws_10m +
    0.01  * (t_mrt - airT) * 2

  # Regional adaptation offsets
  if (!is.null(adapt)) {
    utci <- utci + switch(adapt,
      tropical     =  1.0,
      arid         = -0.5,
      wetland      =  0.5,
      temperate    =  0.0,
      subtropical  =  0.3,
      savanna      =  0.2,
      humid_tropical = 0.8,
      0.0
    )
  }

  round(utci, 2)
}


#' Compute Simplified PET (Physiological Equivalent Temperature)
#'
#' @description
#' Simplified approximation of the MEMI model (Matzarakis et al., 1999).
#'
#' @param airT     Numeric. Air temperature (°C).
#' @param rh       Numeric. Relative humidity (%).
#' @param ws       Numeric. Wind speed (m/s).
#' @param sr_kj_m2 Numeric. Solar radiation (kJ/m²).
#' @return Numeric. PET in °C.
#' @keywords internal
#' @noRd
.compute_pet <- function(airT, rh, ws, sr_kj_m2) {

  # NA solar radiation = no solar load (night); NA wind = calm
  sr_safe   <- ifelse(is.na(sr_kj_m2) | sr_kj_m2 < 0, 0, sr_kj_m2)
  ws_filled <- ifelse(is.na(ws), 0, ws)

  sr_wm2 <- sr_safe / 3.6
  pet    <- airT +
    0.1  * (rh - 50) / 10 -
    0.3  * ws_filled +
    0.05 * sr_wm2 / 100

  round(pet, 2)
}


#' Compute Cooling Degree Days (CDD)
#'
#' @param airT      Numeric. Air temperature (°C).
#' @param base_temp Numeric. Base temperature (°C, default 18).
#' @return Numeric. CDD.
#' @keywords internal
#' @noRd
.compute_cdd <- function(airT, base_temp = 18) {
  round(pmax(airT - base_temp, 0), 1)
}


#' Compute Heating Degree Days (HDD)
#'
#' @param airT      Numeric. Air temperature (°C).
#' @param base_temp Numeric. Base temperature (°C, default 18).
#' @return Numeric. HDD.
#' @keywords internal
#' @noRd
.compute_hdd <- function(airT, base_temp = 18) {
  round(pmax(base_temp - airT, 0), 1)
}


#' Compute Growing Degree Days (GDD)
#'
#' @description
#' Modified GDD with upper temperature threshold to account for heat stress.
#'
#' @param airT       Numeric. Air temperature (°C).
#' @param base_temp  Numeric. Base temperature (°C, default 10).
#' @param upper_temp Numeric. Upper ceiling temperature (°C, default 30).
#' @return Numeric. GDD.
#' @keywords internal
#' @noRd
.compute_gdd <- function(airT, base_temp = 10, upper_temp = 30) {
  t_adj <- pmin(pmax(airT, base_temp), upper_temp)
  round(t_adj - base_temp, 1)
}


#' Compute Diurnal Temperature Range
#'
#' @description
#' Computes the true daily temperature range (Tmax_day - Tmin_day) and
#' broadcasts the value to every hour of that day. Strategy:
#'
#' 1. If `tair_max_c` and `tair_min_c` columns exist AND their daily variance
#'    is large enough (sd of daily differences > 1 °C), those columns already
#'    represent daily extremes → compute once per calendar day and broadcast.
#' 2. Otherwise, derive daily Tmax/Tmin from `tair_dry_bulb_c` directly.
#'
#' Requires a `date` column (POSIXct or Date) in `df` for grouping.
#'
#' @param df       Data frame containing climate variables and a `date` column.
#' @param tmax_col Character. Column name for maximum temperature.
#' @param tmin_col Character. Column name for minimum temperature.
#' @return Numeric vector (same length as nrow(df)). DTR in °C, one value per
#'   hour (broadcast from daily aggregate). NA when temperature data is missing.
#' @keywords internal
#' @noRd
.compute_diurnal_range <- function(df, tmax_col = "tair_max_c", tmin_col = "tair_min_c") {

  n <- nrow(df)

  # Extract calendar date for daily grouping
  day_key <- as.Date(df[["date"]])

  has_maxmin_cols <- tmax_col %in% colnames(df) && tmin_col %in% colnames(df)

  if (has_maxmin_cols) {
    tmax_vals <- df[[tmax_col]]
    tmin_vals <- df[[tmin_col]]

    # Check if these columns represent *daily* extremes (high day-to-day variance)
    # by computing the daily mean difference and its standard deviation
    daily_diffs <- tapply(tmax_vals - tmin_vals, day_key, mean, na.rm = TRUE)
    range_sd    <- if (length(daily_diffs) > 1) sd(daily_diffs, na.rm = TRUE) else 0

    if (!is.na(range_sd) && range_sd > 0.5) {
      # Columns already represent daily extremes → aggregate to daily then broadcast
      daily_max <- tapply(tmax_vals, day_key, max, na.rm = TRUE)
      daily_min <- tapply(tmin_vals, day_key, min, na.rm = TRUE)
      dtr_daily <- daily_max - daily_min
      dtr_vec   <- dtr_daily[as.character(day_key)]
      return(round(as.numeric(dtr_vec), 2))
    }
  }

  # Fallback: derive daily Tmax/Tmin from tair_dry_bulb_c
  if (!"tair_dry_bulb_c" %in% colnames(df)) {
    return(rep(NA_real_, n))
  }

  airT      <- df[["tair_dry_bulb_c"]]
  daily_max <- tapply(airT, day_key, max, na.rm = TRUE)
  daily_min <- tapply(airT, day_key, min, na.rm = TRUE)
  dtr_daily <- daily_max - daily_min
  dtr_vec   <- dtr_daily[as.character(day_key)]

  round(as.numeric(dtr_vec), 2)
}


#' Compute Actual Vapour Pressure
#'
#' @description Tetens (1930) saturation formula; actual = es * RH/100.
#' @param airT Numeric. Air temperature (°C).
#' @param rh   Numeric. Relative humidity (%).
#' @return Numeric. Vapour pressure in kPa.
#' @keywords internal
#' @noRd
.compute_vapor_pressure <- function(airT, rh) {
  es <- 0.6108 * exp((17.27 * airT) / (airT + 237.3))
  round(es * (rh / 100), 3)
}


#' Compute Categorical Heat Stress Risk
#'
#' @param airT Numeric. Air temperature (°C).
#' @param rh   Numeric. Relative humidity (%).
#' @return Character vector. Risk level: "None", "Low", "Moderate", "High", "Extreme".
#' @keywords internal
#' @noRd
.compute_heat_stress_risk <- function(airT, rh) {
  dplyr::case_when(
    airT > 40 | (airT > 35 & rh > 80) ~ "Extreme",
    airT > 35 | (airT > 30 & rh > 85) ~ "High",
    airT > 30 | (airT > 28 & rh > 80) ~ "Moderate",
    airT > 26                          ~ "Low",
    TRUE                               ~ "None"
  )
}


#' Compute Simplified Köppen Humidity Classification
#'
#' @param rh Numeric. Relative humidity (%).
#' @return Character vector. Class: "Arid", "Semi-arid", "Humid", "Perhumid".
#' @keywords internal
#' @noRd
.compute_koppen_humidity <- function(rh) {
  dplyr::case_when(
    rh < 30 ~ "Arid",
    rh < 50 ~ "Semi-arid",
    rh < 70 ~ "Humid",
    TRUE    ~ "Perhumid"
  )
}


# ============================================================================
# VALIDITY FLAG HELPER
# ============================================================================

#' Check indicator validity based on warning thresholds
#'
#' Returns TRUE (out-of-range) for values outside the recommended physical
#' domain. Only acts on thresholds named `warning_high` and `warning_low`.
#'
#' @param values     Numeric vector of indicator values.
#' @param thresholds Named list with optional `warning_high` / `warning_low`.
#' @return Logical vector.
#' @keywords internal
#' @noRd
.check_indicator_validity <- function(values, thresholds) {

  # For categorical indicators, return all FALSE
  if (!is.numeric(values)) return(rep(FALSE, length(values)))

  flags <- is.na(values)   # NA values are inherently "out of range"

  if (!is.null(thresholds$warning_high)) {
    flags <- flags | (!is.na(values) & values > thresholds$warning_high)
  }
  if (!is.null(thresholds$warning_low)) {
    flags <- flags | (!is.na(values) & values < thresholds$warning_low)
  }

  flags
}


# ============================================================================
# REGION DETECTION HELPERS
# ============================================================================

#' Detect Brazilian biome/region from station metadata
#'
#' Tries, in order: (1) `UF` column → UF-to-biome mapping, resolving
#' Pantanal/Cerrado ambiguity via longitude; (2) `latitude` column;
#' (3) default to `"southeast"` with a warning.
#'
#' @param df          Data frame.
#' @param station_col Character. Station column name (unused here but kept for
#'   API consistency).
#' @param verbose     Logical.
#' @return Character scalar. Region code.
#' @keywords internal
#' @noRd
.detect_region_from_data <- function(df, station_col, verbose = TRUE) {

  uf_to_region <- list(
    amazon          = c("AC", "AM", "AP", "PA", "RO", "RR", "TO"),
    caatinga        = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE"),
    atlantic_forest = c("ES", "RJ", "SP"),
    pampa           = c("RS"),
    south           = c("PR", "SC"),
    southeast       = c("MG"),
    cerrado         = c("DF", "GO"),
    pantanal_cerrado = c("MT", "MS")   # ambiguous — resolved below
  )

  if ("UF" %in% colnames(df)) {

    uf_mode <- df |>
      dplyr::filter(!is.na(.data$UF)) |>
      dplyr::count(.data$UF, sort = TRUE) |>
      dplyr::pull(.data$UF) |>
      (\(x) x[1])()

    if (!is.na(uf_mode)) {
      for (region in names(uf_to_region)) {
        if (uf_mode %in% uf_to_region[[region]]) {
          if (region == "pantanal_cerrado") {
            return(.resolve_pantanal_cerrado(df))
          }
          return(region)
        }
      }
    }
  }

  if ("latitude" %in% colnames(df)) {
    avg_lat <- mean(df[["latitude"]], na.rm = TRUE)
    if (!is.nan(avg_lat)) {
      if (avg_lat < -30) return("pampa")
      if (avg_lat < -25) return("south")
      if (avg_lat < -15) return("southeast")
      if (avg_lat < -5)  return("cerrado")
      return("amazon")
    }
  }

  if (verbose) cli::cli_alert_warning("Could not detect region; defaulting to 'southeast'.")
  "southeast"
}


#' Resolve Pantanal vs Cerrado for MT / MS states
#' @keywords internal
#' @noRd
.resolve_pantanal_cerrado <- function(df) {
  if ("longitude" %in% colnames(df)) {
    avg_lon <- mean(df[["longitude"]], na.rm = TRUE)
    # Pantanal core: roughly −58 to −55 °W
    if (!is.nan(avg_lon) && avg_lon <= -55 && avg_lon >= -58) {
      return("pantanal")
    }
  }
  "cerrado"
}


# ============================================================================
# MULTILINGUAL MESSAGES
# ============================================================================

#' Get multilingual messages for indicator computation
#' @param lang Character. One of "pt", "en", "es".
#' @return Named list of message templates.
#' @keywords internal
#' @noRd
.get_indicators_messages <- function(lang = "pt") {

  msgs <- list(
    pt = list(
      no_datetime_found     = "Nenhuma coluna de data/hora encontrada.",
      no_station_found      = "Nenhuma coluna de estacao encontrada. Processando todos os dados juntos.",
      station_col_not_found = "Coluna de estacao '%s' nao encontrada.",
      unknown_indicators    = "Indicador(es) desconhecido(s): %s. Opcoes validas: %s.",
      computing_indicators  = "Calculando %d indicador(es): %s",
      missing_vars_global   = "Variaveis ausentes no dataframe (alguns indicadores serao NA): %s",
      skipping_indicator    = "Indicador %s ignorado — variavel(is) ausente(s): %s",
      indicator_done        = "OK %s (%s) → coluna '%s': %d/%d valores validos (%.1f%%)",
      done                  = "Concluido: %d/%d indicadores calculados com sucesso."
    ),
    en = list(
      no_datetime_found     = "No datetime column found.",
      no_station_found      = "No station column found. Processing all data together.",
      station_col_not_found = "Station column '%s' not found.",
      unknown_indicators    = "Unknown indicator code(s): %s. Valid options: %s.",
      computing_indicators  = "Computing %d indicator(s): %s",
      missing_vars_global   = "Variables absent from data frame (some indicators will be NA): %s",
      skipping_indicator    = "Skipping indicator %s — missing variable(s): %s",
      indicator_done        = "OK %s (%s) → column '%s': %d/%d valid values (%.1f%%)",
      done                  = "Done: %d/%d indicators computed successfully."
    ),
    es = list(
      no_datetime_found     = "No se encontro columna de fecha/hora.",
      no_station_found      = "No se encontro columna de estacion. Procesando todos los datos juntos.",
      station_col_not_found = "Columna de estacion '%s' no encontrada.",
      unknown_indicators    = "Codigo(s) de indicador desconocido(s): %s. Opciones validas: %s.",
      computing_indicators  = "Calculando %d indicador(es): %s",
      missing_vars_global   = "Variables ausentes en el dataframe (algunos indicadores seran NA): %s",
      skipping_indicator    = "Indicador %s omitido — variable(s) ausente(s): %s",
      indicator_done        = "OK %s (%s) → columna '%s': %d/%d valores validos (%.1f%%)",
      done                  = "Completado: %d/%d indicadores calculados correctamente."
    )
  )

  lang <- match.arg(lang, names(msgs))
  msgs[[lang]]
}


# ============================================================================
# NULL-COALESCING OPERATOR (define if not already available in package)
# ============================================================================
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (!is.null(x)) x else y
}