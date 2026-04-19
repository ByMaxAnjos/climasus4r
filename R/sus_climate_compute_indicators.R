# =============================================================================
# sus_climate_compute_indicators.R
#
# Scientifically robust computation of bioclimatic and thermal-stress indices
# from INMET station data, with optional regional adaptation for Brazilian biomes.
#
# Scientific basis:
#   WBGT   : Liljegren et al. (2008) + Bernard & Pourmoghani (1999)
#   HI     : Rothfusz (1990) / NWS WPC
#   THI    : Thom (1959)
#   WCET   : Environment Canada (2001)
#   WCT    : NWS (2001)
#   ET     : Missenard (1937) via Auliciems & Szokolay (2007)
#   UTCI   : Brode et al. (2012) - Fiala-based polynomial approximation
#   PET    : Matzarakis et al. (1999) - MEMI-based approximation
#   CDD/HDD/GDD : Standard degree-day methods
#   Vapor pressure : Magnus-Tetens formula
# =============================================================================

# ============================================================================
# PACKAGE-LEVEL PHYSICS CONSTANTS
# ============================================================================

  .SUS_PHYSICS <- list(
    solar_constant        = 1361,      # W/m2
    stefan_boltzmann      = 5.67e-8,   # W/m2/K4
    g                     = 9.81,      # m/s4
    R_air                 = 287.05,    # J/(kg.K)
    cp                    = 1004,      # J/(kg.K)
    latent_heat_vap       = 2.501e6,   # J/kg
    psychrometric_cst     = 0.066,     # kPa/K at sea level
    emissivity_skin       = 0.95,
    solar_absorption      = 0.70,
    standard_pressure_kpa = 101.325
  )

#' Compute Bioclimatic and Thermal Stress Indicators
#'
#' @description
#' `sus_climate_compute_indicators()` calculates a comprehensive set of
#' bioclimatic and thermal-stress indices from INMET station data (output of
#' `sus_climate_inmet()` or `sus_climate_fill_inmet()`).
#'
#' **Scientific robustness highlights:**
#' \itemize{
#'   \item WBGT uses a dual wet-bulb estimate (Liljegren + Bernard/Pourmoghani)
#'         averaged for numerical robustness; solar radiation `NA` treated as 0
#'         (no solar load at night) so all hours yield a valid value.
#'   \item HI implements full Rothfusz (1990) regression with both NWS humidity
#'         adjustments and a regionally adapted validity threshold.
#'   \item UTCI uses a Fiala-polynomial-inspired multi-term regression (vapour
#'         pressure, logarithmic wind, radiation linear + non-linear,
#'         wind vs radiation interaction).
#'   \item PET is based on a MEMI-style energy-balance approximation with
#'         seasonal clothing adjustment and regional correction.
#'   \item Solar radiation undergoes optional physical consistency verification
#'         against extraterrestrial radiation limits.
#'   \item Diurnal range is a true daily aggregate (Tmax - Tmin per calendar day)
#'         broadcast to all hourly rows of that day.
#'   \item Multi-level confidence flags (`_flag_extreme`, `_flag_high`,
#'         `_flag_low`) replace a single `_out_of_range` flag.
#'   \item Optional 200-simulation Monte Carlo uncertainty estimation.
#' }
#'
#' **Available indicators:**
#'
#' | Code | Output column | Requires |
#' | :--- | :--- | :--- |
#' | `wbgt` | `wbgt_c` | T, RH, SR, WS |
#' | `hi` | `hi_c` | T, RH |
#' | `thi` | `thi_c` | T, RH |
#' | `wcet` | `wcet_c` | T, WS |
#' | `wct` | `wct_c` | T, WS |
#' | `et` | `et_c` | T, RH, WS |
#' | `utci` | `utci_c` | T, RH, WS, SR |
#' | `pet` | `pet_c` | T, RH, WS, SR |
#' | `cdd` | `cdd_c` | T |
#' | `hdd` | `hdd_c` | T |
#' | `gdd` | `gdd_c` | T |
#' | `diurnal_range` | `diurnal_range_c` | T (daily agg.) |
#' | `vapor_pressure` | `vapor_pressure_kpa` | T, RH |
#' | `heat_stress_risk` | `heat_stress_risk` | T, RH, WS, SR |
#' | `koppen_humidity` | `koppen_humidity` | RH |
#' 
#' *(Where: T=`tair_dry_bulb_c`, RH=`rh_mean_porc`, SR=`sr_kj_m2`, WS=`ws_2_m_s`)*
#'
#' @param df Data frame or tibble from `sus_climate_inmet()` or `sus_climate_fill_inmet()`.
#' @param indicators Character vector of indicator codes or `"all"` (default).
#' @param region Character. `"auto"` (default, standard thresholds) or a
#'   Brazilian biome: `"amazon"`, `"cerrado"`, `"caatinga"`,
#'   `"atlantic_forest"`, `"pampa"`, `"pantanal"`, `"southeast"`, `"south"`.
#' @param datetime_col Character or `NULL` (auto-detected).
#' @param station_col Character or `NULL` (auto-detected).
#' @param apply_validity_mask Logical (default `TRUE`). When `region=="none"`,
#'   masks HI, WCET, WCT outside their valid meteorological domains with `NA`.
#' @param confidence_flags Logical. Add `_flag_extreme`, `_flag_high`,
#'   `_flag_low` columns. Default `TRUE` when region is set, `FALSE` otherwise.
#' @param custom_thresholds Named list to override regional defaults.
#' @param keep_source_vars Logical (default `FALSE`).
#' @param verify_physics Logical (default `TRUE`). Check solar radiation
#'   against extraterrestrial limits; requires `latitude` column.
#' @param compute_uncertainty Logical (default `FALSE`). Monte Carlo 95% CI.
#' @param verbose Logical (default `TRUE`).
#' @param lang Character: `"pt"` (default), `"en"`, `"es"`.
#' @param use_cache Logical. If `TRUE` (default), uses cached spatial data to avoid
#'   re-downloads and improve performance.
#' @param cache_dir Character string specifying the directory to store cached files.
#'   Default is `"~/.climasus4r_cache/spatial"`.
#'
#' @return A `tibble` of class `climasus_df` containing indicator columns,
#'   optional flag/CI columns, and a `sus_meta` attribute.
#'
#' @section Scientific References:
#' \itemize{
#'   \item Liljegren et al. (2008) \emph{J. Occup. Environ. Hyg.} 5(10):645-655.
#'   \item Bernard & Pourmoghani (1999) \emph{AIHAJ} 60(1):32-37.
#'   \item Rothfusz (1990) NWS Tech. Attachment SR/SSD 90-23.
#'   \item Thom (1959) \emph{Weatherwise} 12(2):57-59.
#'   \item Environment Canada (2001) Wind Chill Index.
#'   \item Brode et al. (2012) \emph{Int. J. Biometeorol.} 56(4):481-494.
#'   \item Matzarakis et al. (1999) \emph{Int. J. Biometeorol.} 43:76-84.
#'   \item Magnus (1844) / Tetens (1930) saturation vapour pressure.
#' }
#'
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' result <- sus_climate_compute_indicators(df_clima)
#'
#' result <- sus_climate_compute_indicators(
#'   df_clima, region = "amazon", confidence_flags = TRUE
#' )
#'
#' result <- sus_climate_compute_indicators(
#'   df_clima,
#'   region              = "auto",
#'   verify_physics      = TRUE,
#'   compute_uncertainty = TRUE,
#'   keep_source_vars    = TRUE
#' )
#'
#' df_clima |>
#'   sus_climate_fill_inmet(target_var = "all") |>
#'   sus_climate_compute_indicators(indicators = c("wbgt", "hi", "utci"))
#' }
sus_climate_compute_indicators <- function(
  df,
  indicators          = "all",
  region              = "auto",
  datetime_col        = NULL,
  station_col         = NULL,
  apply_validity_mask = TRUE,
  confidence_flags    = NULL,
  custom_thresholds   = NULL,
  keep_source_vars    = FALSE,
  verify_physics      = TRUE,
  compute_uncertainty = FALSE,
  verbose             = TRUE,
  lang                = "pt",
  use_cache = TRUE,
  cache_dir = "~/.climasus4r_cache/spatial"
) {

  if (verbose) {
    title_msg <- switch(lang,
      "en" = "climasus4r - Bioclimatic and Thermal Stress Indicators",
      "pt" = "climasus4r - Indicadores Bioclim\u00e1ticos e de Estresse T\u00e9rmico",
      "es" = "climasus4r - Indicadores Bioclim\u00e1ticos y de Estr\u00e9s T\u00e9rmico"
    )
    cli::cli_h1(title_msg)
  }
  
  # ===========================================================================
  # VALID DF
  # ===========================================================================
  if (inherits(df, "climasus_df")) {
    
    current_stage <- sus_meta(df, "stage")
    required_stage <- "climate"

    if (!is_stage_at_least(current_stage, required_stage)) {

      msg_error <- list(
        en = paste0(
          "Data must be filtered before aggregation.\n",
          "Current stage: ", current_stage %||% "unknown", "\n",
          "Required stage: ", required_stage, "\n\n",
          "Please run:\n",
          "  1. sus_climate_inmet(...)",
          "  2. sus_climate_fill_inmet(evaluation = FALSE)"
        ),
        pt = paste0(
          "Dados devem ser filtrados antes da agregacao.\n",
          "Estagio atual: ", current_stage %||% "desconhecido", "\n",
          "Estagio requerido: ", required_stage, "\n\n",
          "Por favor, execute:\n",
          "  1. sus_climate_inmet(...)",
          "  2. sus_climate_fill_inmet(evaluation = FALSE)"
        ),
        es = paste0(
          "Los datos deben ser filtrados antes de la agregacion.\n",
          "Etapa actual: ", current_stage %||% "desconocida", "\n",
          "Etapa requerida: ", required_stage, "\n\n",
          "Por favor, ejecute:\n",
          "  1. sus_climate_inmet(...)",
          "  2. sus_climate_fill_inmet(evaluation = FALSE)"
        )
      )

      cli::cli_abort(msg_error[[lang]] %||% msg_error[["en"]])
    }

    # Stage validated
    if (verbose) {
      msg_stage_ok <- list(
         en = "Data stage validated: aggregation allowed",
        pt = "Estagio de dados validado: agregacao permitida",
        es = "Etapa de datos validada: agregacion permitida"
      )

      cli::cli_alert_success(msg_stage_ok[[lang]] %||% msg_stage_ok[["en"]])
    }

    # Update metadata
    df <- sus_meta(df, stage = "climate", type = "indicators")

  } else {
      # NOT climasus_df - ABORT execution
      msg_error <- list(
        en = c(
          "{.red {cli::symbol$cross} Input is not a {.cls climasus_df} object.}",
          "i" = "This function requires data formatted by the {.pkg climasus4r} pipeline.",
          " " = "",
          "Please prepare your data first:",
          "*" = "{.strong 1. Import INMET Data:} {.code df <- sus_climate_inmet(...)}",
          "*" = "{.strong 2. Filling gaps:} {.code df <- sus_climate_fill_inmet(...)}"
        ),
        pt = c(
          "{.red {cli::symbol$cross} A entrada como nao objeto {.cls climasus_df}.}",
          "i" = "Esta funcao requer dados processados pelo pipeline {.pkg climasus4r}.",
          " " = "",
          "Por favor, prepare seus dados primeiro:",
          "*" = "{.strong 1. Importar Dados do INMET:} {.code df <- sus_climate_inmet(...)}",
          "*" = "{.strong 2. Preencher falhas:} {.code df <- sus_climate_fill_inmet(...)}"
        ),
        es = c(
          "{.red {cli::symbol$cross} La entrada no es un objeto {.cls climasus_df}.}",
          "i" = "Esta funcion requiere datos procesados por el pipeline {.pkg climasus4r}.",
          " " = "",
          "Por favor, prepare sus datos primero:",
          "*" = "{.strong 1. Importar Dados del INMET:} {.code df <- sus_climate_inmet(...)}",
          "*" = "{.strong 2. Preencher falhas:} {.code df <- sus_climate_fill_inmet(...)}"
        )
      )
      cli::cli_abort(msg_error[[lang]])
  }
  
  # ===========================================================================
  # DEFAULT
  # ===========================================================================
  
  region     <- tolower(trimws(region))
  use_region <- region != "none"
  if (is.null(confidence_flags)) confidence_flags <- use_region

  # ===========================================================================
  # 1. REGISTRY
  # ===========================================================================
  
  .reg       <- .build_indicator_registry()
  .all_codes <- names(.reg)

  # ===========================================================================
  # 2. MESSAGES
  # ===========================================================================
  
  msgs <- .get_indicators_messages(lang)

  # ===========================================================================
  # COLUMN DETECTION
  # ===========================================================================
  
  source_sys   <- .detect_data_source(df)
  datetime_col <- .detect_datetime_column(df, datetime_col, verbose, msgs)
  station_col  <- .detect_station_column(df, station_col, source_sys, verbose, msgs)

  # ===========================================================================
  # RESOLVE INDICATIORS
  # ===========================================================================
  
  if (identical(tolower(indicators), "all")) {
    indicators <- .all_codes
  } else {
    indicators <- tolower(trimws(indicators))
    bad <- setdiff(indicators, .all_codes)
    if (length(bad) > 0)
      cli::cli_abort(sprintf(msgs$unknown_indicators,
                             paste(bad, collapse = ", "),
                             paste(.all_codes, collapse = ", ")))
  }

  if (verbose)
    cli::cli_alert_info(sprintf(msgs$computing_indicators,
                                length(indicators),
                                paste(toupper(indicators), collapse = ", ")))

  
  # ===========================================================================
  # REGIONAL PARAMETERS
  # ===========================================================================
  
  region_params <- .resolve_region_params(
    region, df, station_col, custom_thresholds, verbose, lang
  )

  # ===========================================================================
  # WORKING DATA FRAME
  # ===========================================================================
  
  df_work      <- df
  renamed_date <- FALSE
  if (!is.null(datetime_col) && datetime_col != "date") {
    df_work      <- dplyr::rename(df_work, date = !!rlang::sym(datetime_col))
    renamed_date <- TRUE
  }

  all_required     <- unique(unlist(lapply(indicators, function(i) .reg[[i]]$requires)))
  missing_globally <- setdiff(all_required, colnames(df_work))
  if (length(missing_globally) > 0 && verbose)
    cli::cli_alert_warning(sprintf(msgs$missing_vars_global,
                                   paste(missing_globally, collapse = ", ")))

  if (verify_physics && "sr_kj_m2" %in% colnames(df_work))
    df_work <- .verify_solar_radiation(df_work, verbose, lang)

  # ===========================================================================
  # Initialise result
  # ===========================================================================
  
  id_cols <- intersect(c("date", station_col), colnames(df_work))
  result  <- dplyr::select(df_work, dplyr::all_of(id_cols))
  indicator_stats <- list()

  # ===========================================================================
  # Compute indicators
  # ===========================================================================
  
  for (ind in indicators) {

    reg     <- .reg[[ind]]
    out_col <- reg$out_col
    missing_for_ind <- setdiff(reg$requires, colnames(df_work))

    if (length(missing_for_ind) > 0) {
      if (verbose)
        cli::cli_alert_warning(sprintf(msgs$skipping_indicator,
                                       toupper(ind),
                                       paste(missing_for_ind, collapse = ", ")))
      result[[out_col]] <- NA_real_
      indicator_stats[[ind]] <- list(computed = FALSE,
                                     reason   = "missing_vars",
                                     n_valid  = 0L)
      next
    }

    values <- .dispatch_indicator(
      ind           = ind,
      df            = df_work,
      reg           = reg,
      region_params = region_params,
      apply_mask    = apply_validity_mask && !use_region
    )

    result[[out_col]] <- values

    # Monte Carlo uncertainty
    if (compute_uncertainty && !is.null(reg$uncertainty_sd)) {
      ci <- .compute_mc_uncertainty(ind, df_work, reg, region_params, n_sim = 200L)
      result[[paste0(out_col, "_ci_low")]]  <- ci$low
      result[[paste0(out_col, "_ci_high")]] <- ci$high
    }

    # Multi-level flags
    if (confidence_flags && length(reg$thresholds) > 0)
      result <- .add_confidence_flags(result, out_col, reg$thresholds)

    # Stats
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
                         sort(unique(values[!is.na(values)]))
                       else NA,
      region_adapted = if (use_region) region else "none"
    )

    if (verbose)
      cli::cli_alert_success(sprintf(msgs$indicator_done,
                                     toupper(ind), reg$label, out_col,
                                     n_valid, n_total,
                                     round(n_valid / n_total * 100, 1)))
  }

  # ===========================================================================
  # Source variables
  # ===========================================================================
  
  if (keep_source_vars) {
    src_cols <- intersect(all_required, colnames(df_work))
    if (length(src_cols) > 0)
      result <- dplyr::left_join(
        result,
        dplyr::select(df_work, dplyr::all_of(id_cols), dplyr::all_of(src_cols)),
        by = id_cols
      )
  }

  # ===========================================================================
  # 10. Restore datetime name
  # ===========================================================================
  
  if (renamed_date && !is.null(datetime_col))
    result <- dplyr::rename(result, !!rlang::sym(datetime_col) := date)
  date_col_final <- if (renamed_date && !is.null(datetime_col)) datetime_col else "date"

  # ===========================================================================
  # 10.1 Join station meta
  # ===========================================================================

  station_meta <- get_spatial_station_cache(
      cache_dir = cache_dir,
      use_cache = use_cache,
      lang = lang,
      verbose = verbose) %>%
    sf::st_drop_geometry()
  
  result <- dplyr::left_join(result, station_meta, by = c("station_code"))
  # ===========================================================================
  # 11. sus_meta + S3 class
  # ===========================================================================
 
  meta <- list(
    system  = NULL,
    stage   = "climate",
    type    = "indicators",
    spatial = inherits(result, "sf"),
    temporal = list(
      start                = min(result[[date_col_final]], na.rm = TRUE),
      end                  = max(result[[date_col_final]], na.rm = TRUE),
      source               = source_sys,
      indicators_requested = indicators
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
  result <- structure(result, sus_meta = meta,
                      class = c("climasus_df", base_classes))

  if (verbose)
    cli::cli_alert_success(sprintf(msgs$done,
      sum(vapply(indicator_stats, function(x) isTRUE(x$computed), logical(1))),
      length(indicators)))

  result
}

# ============================================================================
# INDICATOR REGISTRY
# ============================================================================

#' Build indicator registry
#' @keywords internal
#' @noRd
.build_indicator_registry <- function() {
  list(
    wbgt = list(
      label = "Wet Bulb Globe Temperature", out_col = "wbgt_c",
      requires = c("tair_dry_bulb_c", "rh_mean_porc", "sr_kj_m2", "ws_2_m_s"),
      thresholds = list(extreme_heat = 31, high_stress = 28, moderate_stress = 25, warning_low = 15),
      uncertainty_sd = list(temp = 0.5, rh = 3, ws = 0.3, sr_frac = 0.10)
    ),
    hi = list(
      label = "Heat Index", out_col = "hi_c",
      requires = c("tair_dry_bulb_c", "rh_mean_porc"),
      thresholds = list(extreme_danger = 54, dangerous = 41, extreme_caution = 32, caution = 27),
      uncertainty_sd = list(temp = 0.5, rh = 3)
    ),
    thi = list(
      label = "Temperature-Humidity Index", out_col = "thi_c",
      requires = c("tair_dry_bulb_c", "rh_mean_porc"),
      thresholds = list(high_stress = 28, comfortable_max = 24, comfortable_min = 20),
      uncertainty_sd = list(temp = 0.5, rh = 3)
    ),
    wcet = list(
      label = "Wind Chill Equivalent Temperature (CA)", out_col = "wcet_c",
      requires = c("tair_dry_bulb_c", "ws_2_m_s"),
      thresholds = list(high_risk = -35, moderate_risk = -20, low_risk = -10),
      uncertainty_sd = list(temp = 0.5, ws = 0.3)
    ),
    wct = list(
      label = "Wind Chill Temperature (NWS)", out_col = "wct_c",
      requires = c("tair_dry_bulb_c", "ws_2_m_s"),
      thresholds = list(high_risk = -35, moderate_risk = -20, low_risk = -10),
      uncertainty_sd = list(temp = 0.5, ws = 0.3)
    ),
    et = list(
      label = "Effective Temperature", out_col = "et_c",
      requires = c("tair_dry_bulb_c", "rh_mean_porc", "ws_2_m_s"),
      thresholds = list(hot = 35, warm = 30, cool = 20, cold = 15),
      uncertainty_sd = list(temp = 0.5, rh = 3, ws = 0.3)
    ),
    utci = list(
      label = "Universal Thermal Climate Index", out_col = "utci_c",
      requires = c("tair_dry_bulb_c", "rh_mean_porc", "ws_2_m_s", "sr_kj_m2"),
      thresholds = list(
        extreme_heat_stress  = 46, strong_heat_stress   = 38,
        moderate_heat_stress = 32, slight_heat_stress   = 26,
        slight_cold_stress   = 9,  moderate_cold_stress = 0,
        strong_cold_stress   = -13, extreme_cold_stress  = -27
      ),
      uncertainty_sd = list(temp = 0.5, rh = 3, ws = 0.3, sr_frac = 0.10)
    ),
    pet = list(
      label = "Physiological Equivalent Temperature", out_col = "pet_c",
      requires = c("tair_dry_bulb_c", "rh_mean_porc", "ws_2_m_s", "sr_kj_m2"),
      thresholds = list(
        extreme_heat = 41, strong_heat = 35, moderate_heat = 29, slight_heat = 23,
        slight_cold = 13, moderate_cold = 8
      ),
      uncertainty_sd = list(temp = 0.5, rh = 3, ws = 0.3, sr_frac = 0.10)
    ),
    cdd = list(
      label = "Cooling Degree Days", out_col = "cdd_c",
      requires = c("tair_dry_bulb_c"),
      thresholds = list(),
      uncertainty_sd = list(temp = 0.5)
    ),
    hdd = list(
      label = "Heating Degree Days", out_col = "hdd_c",
      requires = c("tair_dry_bulb_c"),
      thresholds = list(),
      uncertainty_sd = list(temp = 0.5)
    ),
    gdd = list(
      label = "Growing Degree Days", out_col = "gdd_c",
      requires = c("tair_dry_bulb_c"),
      thresholds = list(),
      uncertainty_sd = list(temp = 0.5)
    ),
    diurnal_range = list(
      label = "Diurnal Temperature Range", out_col = "diurnal_range_c",
      requires = c("tair_dry_bulb_c"),
      thresholds = list(high = 15, moderate = 10, low = 5),
      uncertainty_sd = list(temp = 0.5)
    ),
    vapor_pressure = list(
      label = "Actual Vapour Pressure", out_col = "vapor_pressure_kpa",
      requires = c("tair_dry_bulb_c", "rh_mean_porc"),
      thresholds = list(high = 2.5, moderate = 1.5),
      uncertainty_sd = list(temp = 0.5, rh = 3)
    ),
    heat_stress_risk = list(
      label = "Heat Stress Risk Level (WBGT-based)", out_col = "heat_stress_risk",
      requires = c("tair_dry_bulb_c", "rh_mean_porc", "ws_2_m_s", "sr_kj_m2"),
      thresholds = list()
    ),
    koppen_humidity = list(
      label = "Koppen Humidity Classification", out_col = "koppen_humidity",
      requires = c("rh_mean_porc"),
      thresholds = list()
    )
  )
}

# ============================================================================
# REGIONAL PARAMETERS
# ============================================================================

#' Return full region-parameters table
#' @keywords internal
#' @noRd
.region_params_table <- function() {
  list(
    amazon = list(
      name = "Amazon Biome",
      hi_min_temp = 24.0, hi_min_rh = 40,
      wbgt_high_warning = 28, wbgt_extreme = 31,
      gdd_base = 15, gdd_upper = 35,
      cdd_base = 20, hdd_base = 18,
      utci_correction = 1.5, pet_correction = 1.0,
      clothing_factor = 0.5, metabolic_factor = 1.1
    ),
    cerrado = list(
      name = "Cerrado Biome",
      hi_min_temp = 25.5, hi_min_rh = 35,
      wbgt_high_warning = 30, wbgt_extreme = 32,
      gdd_base = 12, gdd_upper = 32,
      cdd_base = 19, hdd_base = 16,
      utci_correction = 0.5, pet_correction = 0.3,
      clothing_factor = 0.6, metabolic_factor = 1.0
    ),
    caatinga = list(
      name = "Caatinga Biome",
      hi_min_temp = 25.0, hi_min_rh = 30,
      wbgt_high_warning = 32, wbgt_extreme = 33,
      gdd_base = 12, gdd_upper = 34,
      cdd_base = 20, hdd_base = 14,
      utci_correction = -0.5, pet_correction = -0.3,
      clothing_factor = 0.4, metabolic_factor = 1.0
    ),
    atlantic_forest = list(
      name = "Atlantic Forest Biome",
      hi_min_temp = 25.0, hi_min_rh = 40,
      wbgt_high_warning = 29, wbgt_extreme = 31,
      gdd_base = 12, gdd_upper = 32,
      cdd_base = 19, hdd_base = 15,
      utci_correction = 1.0, pet_correction = 0.8,
      clothing_factor = 0.5, metabolic_factor = 1.05
    ),
    pampa = list(
      name = "Pampa Biome",
      hi_min_temp = 26.0, hi_min_rh = 40,
      wbgt_high_warning = 30, wbgt_extreme = 32,
      gdd_base = 8, gdd_upper = 30,
      cdd_base = 18, hdd_base = 15,
      utci_correction = -1.0, pet_correction = -0.8,
      clothing_factor = 0.7, metabolic_factor = 1.0
    ),
    pantanal = list(
      name = "Pantanal Biome",
      hi_min_temp = 24.5, hi_min_rh = 35,
      wbgt_high_warning = 31, wbgt_extreme = 32,
      gdd_base = 14, gdd_upper = 34,
      cdd_base = 20, hdd_base = 16,
      utci_correction = 0.5, pet_correction = 0.4,
      clothing_factor = 0.5, metabolic_factor = 1.05
    ),
    southeast = list(
      name = "Southeast Region",
      hi_min_temp = 25.5, hi_min_rh = 40,
      wbgt_high_warning = 30, wbgt_extreme = 32,
      gdd_base = 10, gdd_upper = 32,
      cdd_base = 19, hdd_base = 16,
      utci_correction = 0.0, pet_correction = 0.0,
      clothing_factor = 0.6, metabolic_factor = 1.0
    ),
    south = list(
      name = "South Region",
      hi_min_temp = 26.0, hi_min_rh = 40,
      wbgt_high_warning = 29, wbgt_extreme = 31,
      gdd_base = 8, gdd_upper = 30,
      cdd_base = 18, hdd_base = 14,
      utci_correction = -1.5, pet_correction = -1.2,
      clothing_factor = 0.7, metabolic_factor = 1.0
    )
  )
}


#' Resolve and return regional parameters (NULL for "none")
#' @keywords internal
#' @noRd
.resolve_region_params <- function(region, df, station_col,
                                   custom_thresholds, verbose, lang) {
  tbl <- .region_params_table()
  if (region == "none") return(NULL)

  if (region == "auto") {
    region <- .detect_region_from_data(df, station_col, verbose)
    if (verbose)
      cli::cli_alert_info(switch(lang,
        pt = sprintf("Regiao auto-detectada: %s", tbl[[region]]$name %||% region),
        es = sprintf("Region detectada: %s",       tbl[[region]]$name %||% region),
             sprintf("Auto-detected region: %s",   tbl[[region]]$name %||% region)
      ))
  }

  valid <- names(tbl)
  if (!region %in% valid) {
    cli::cli_warn(sprintf(
      "Unknown region '%s'; defaulting to 'southeast'. Valid: %s",
      region, paste(valid, collapse = ", ")
    ))
    region <- "southeast"
  }

  params <- tbl[[region]]
  if (!is.null(custom_thresholds))
    params <- utils::modifyList(params, custom_thresholds)
  params
}


# ===========================================================================
# Get spatial station meta
# ===========================================================================

#' Get Spatial Station Data with Caching
#'
#' Retrieves spatial station data from INMET with intelligent caching to improve performance.
#' Spatial data is cached as Parquet files for fast reloading.
#' @param cache_dir Cache directory path
#' @param use_cache Whether to use cache
#'
#' @return sf object with spatial data
#' @keywords internal
#' @noRd
get_spatial_station_cache <- function(
  cache_dir,
  use_cache,
  lang,
  verbose
) {
  msg <- get_spatial_station_messages(lang)

  if ((requireNamespace("sfarrow", quietly = TRUE))) { 
  # Define cache file name
  cache_file <- file.path(
    cache_dir,
    paste0("station_meta.parquet")
  )
  } else { 
  # Define cache file name
  cache_file <- file.path(
    cache_dir,
    paste0("station_meta.gpkg")
  )
  }

  # Try to load from cache
  if (use_cache && file.exists(cache_file)) {
    if (verbose) {
      cli::cli_alert_success(paste0(msg$loading_cache, basename(cache_file)))
    }

    tryCatch(
      {
        if ((requireNamespace("sfarrow", quietly = TRUE))) { 
          spatial_df <- sfarrow::st_read_parquet(cache_file)
        } else { 
          spatial_df <- sf::st_read(cache_file)
          spatial_df <- sf::st_as_sf(spatial_df)
        }
        return(spatial_df)
      },
      error = function(e) {
        if (verbose) {
          cli::cli_alert_warning(msg$cache_error)
        }
      }
    )
  }

  # Download from IBGE
  if (verbose) {
    cli::cli_alert_info(msg$downloading_data)
  }

  spatial_df <- suppressMessages(sfarrow::st_read_parquet("https://github.com/ByMaxAnjos/climasus4r/raw/refs/heads/master/inst/data_4r/station_meta.parquet"))

  # Save to cache
  if (use_cache) {
    if (verbose) {
      cli::cli_alert_info(msg$saving_cache)
    }
    tryCatch(
      { 
        if ((requireNamespace("sfarrow", quietly = TRUE))) { 
          sfarrow::st_write_parquet(obj = spatial_df, dsn = cache_file)
        } else {
          sf::st_write(spatial_df, cache_file, driver = "GPKG", quiet = TRUE, delete_dsn = TRUE, append = TRUE)
         }
        if (verbose) {
          cli::cli_alert_success(msg$cache_saved)
        }
      },
      error = function(e) {
        if (verbose) {
          cli::cli_alert_warning(paste0(msg$cache_save_error, e$message))
        }
      }
    )
  }

  return(spatial_df)
}


#' Get Multilingual Messages for Spatial Operations
#'
#' Returns user interface messages in the specified language for spatial join operations.
#'
#' @param lang Character. Language code: "en", "pt", "es"
#'
#' @return List of translated UI messages
#' @keywords internal
#' @noRd
get_spatial_station_messages <- function(lang) {
  messages <- list(
    en = list(
      creating_cache = "Creating cache directory: ",
      cache_enabled = "Cache: ENABLED",
      loading_cache = "Loading from cache: ",
      cache_error = "Cache loading failed. Downloading fresh data...",
      downloading_data = "Downloading spatial data from climasus Data Center...",
      saving_cache = "Saving to cache...",
      cache_saved = "Spatial data cached successfully.",
      cache_save_error = "Failed to save cache: "
    ),
    pt = list(
      creating_cache = "Criando diretorio de cache: ",
      cache_enabled = "Cache: ATIVADO",
      loading_cache = "Carregando do cache: ",
      cache_error = "Falha ao carregar cache. Baixando dados novos...",
      downloading_data = "Baixando dados espaciais do climasus Data Center ...",
      saving_cache = "Salvando no cache...",
      cache_saved = "Dados espaciais armazenados em cache com sucesso.",
      cache_save_error = "Falha ao salvar cache: "
    ),
    es = list(
      creating_cache = "Creando directorio de cache: ",
      cache_enabled = "Cache: ACTIVADO",
      loading_cache = "Cargando desde cache: ",
      cache_error = "Fallo al cargar cache. Descargando datos nuevos...",
      downloading_data = "Descargando datos espaciales del climasus Data Center...",
      saving_cache = "Guardando en cache...",
      cache_saved = "Datos espaciales almacenados en cache con exito.",
      cache_save_error = "Fallo al guardar cache: "
    )
  )
  
  return(messages[[lang]])
}

# ============================================================================
# DISPATCH
# ============================================================================

#' Route indicator code to its computation function
#' @keywords internal
#' @noRd
.dispatch_indicator <- function(ind, df, reg, region_params, apply_mask) {

  rp <- region_params

  # Safe imputed versions of NA-prone inputs
  sr_safe <- ifelse(is.na(df[["sr_kj_m2"]]) | df[["sr_kj_m2"]] < 0,
                    0, df[["sr_kj_m2"]])
  ws_safe <- ifelse(is.na(df[["ws_2_m_s"]]),
                    0, df[["ws_2_m_s"]])

  switch(ind,

    wbgt = .compute_wbgt(
      airT = df[["tair_dry_bulb_c"]], rh = df[["rh_mean_porc"]],
      sr_kj_m2 = sr_safe, ws = ws_safe
    ),

    hi = .compute_hi(
      airT = df[["tair_dry_bulb_c"]], rh = df[["rh_mean_porc"]],
      apply_mask = apply_mask,
      min_temp   = rp$hi_min_temp %||% 26.7,
      min_rh     = rp$hi_min_rh   %||% 40
    ),

    thi = .compute_thi(df[["tair_dry_bulb_c"]], df[["rh_mean_porc"]]),

    wcet = .compute_wcet(
      airT = df[["tair_dry_bulb_c"]], ws = ws_safe, apply_mask = apply_mask
    ),

    wct = .compute_wct(
      airT = df[["tair_dry_bulb_c"]], ws = ws_safe, apply_mask = apply_mask
    ),

    et = .compute_et(
      airT = df[["tair_dry_bulb_c"]], rh = df[["rh_mean_porc"]], ws = ws_safe
    ),

    utci = .compute_utci(
      airT = df[["tair_dry_bulb_c"]], rh = df[["rh_mean_porc"]],
      ws   = ws_safe, sr_kj_m2 = sr_safe,
      utci_correction = rp$utci_correction %||% 0
    ),

    pet = .compute_pet(
      airT = df[["tair_dry_bulb_c"]], rh = df[["rh_mean_porc"]],
      ws   = ws_safe, sr_kj_m2 = sr_safe,
      date            = df[["date"]],
      pet_correction  = rp$pet_correction  %||% 0,
      clothing_factor = rp$clothing_factor %||% 0.6
    ),

    cdd = .compute_cdd(df[["tair_dry_bulb_c"]], rp$cdd_base %||% 18),
    hdd = .compute_hdd(df[["tair_dry_bulb_c"]], rp$hdd_base %||% 18),

    gdd = .compute_gdd(
      df[["tair_dry_bulb_c"]],
      rp$gdd_base %||% 10, rp$gdd_upper %||% 30
    ),

    diurnal_range = .compute_diurnal_range(df, "tair_max_c", "tair_min_c"),

    vapor_pressure = .compute_vapor_pressure(
      df[["tair_dry_bulb_c"]], df[["rh_mean_porc"]]
    ),

    heat_stress_risk = .compute_heat_stress_risk(
      airT = df[["tair_dry_bulb_c"]], rh = df[["rh_mean_porc"]],
      ws   = ws_safe, sr_kj_m2 = sr_safe
    ),

    koppen_humidity = .compute_koppen_humidity(df[["rh_mean_porc"]]),

    stop(sprintf("No computation function registered for indicator '%s'.", ind))
  )
}


# ============================================================================
# COMPUTATION FUNCTIONS
# ============================================================================

#' WBGT-Liljegren + Bernard/Pourmoghani dual wet-bulb average
#' @keywords internal
#' @noRd
.compute_wbgt <- function(airT, rh, sr_kj_m2, ws) {
  sr_wm2 <- sr_kj_m2 / 3.6
  ws_cal <- pmax(ws, 0.1)

  # Method 1: vapour-pressure based (Liljegren style)
  es   <- 0.6108 * exp((17.27 * airT) / (airT + 237.3))
  e_a  <- (rh / 100) * es
  tnw1 <- airT * atan(0.16 * sqrt(pmax(e_a, 0.01) + 0.1)) + 3.0

  # Method 2: Bernard & Pourmoghani (1999)- numerically stable
  tnw2 <- airT + 0.33 * (rh / 100) * exp(0.0514 * airT) - 4.0

  tnw  <- (tnw1 + tnw2) / 2                                   # average
  tg   <- airT + 0.0144 * pmax(sr_wm2, 0)^0.6 / ws_cal^0.2 - 2.0

  round(0.7 * tnw + 0.2 * tg + 0.1 * airT, 2)
}


#' HI-Rothfusz (1990) with both NWS humidity adjustments
#' @keywords internal
#' @noRd
.compute_hi <- function(airT, rh, apply_mask = TRUE,
                        min_temp = 26.7, min_rh = 40) {
  tf <- (airT * 9 / 5) + 32

  hi_f <- (
    -42.379 + 2.04901523*tf + 10.14333127*rh - 0.22475541*tf*rh
    - 6.83783e-3*tf^2 - 5.481717e-2*rh^2
    + 1.22874e-3*tf^2*rh + 8.5282e-4*tf*rh^2 - 1.99e-6*tf^2*rh^2
  )

  # Low-humidity adjustment (RH < 13 %, 80 <= Tf <= 112)
  sqrt_arg <- pmax(17 - abs(tf - 95), 0)
  adj_l    <- (13 - rh) / 4 * sqrt(sqrt_arg / 17)
  mask_l   <- !is.na(rh) & rh < 13 & tf >= 80 & tf <= 112
  hi_f     <- ifelse(mask_l, hi_f - adj_l, hi_f)

  # High-humidity adjustment (RH > 85 %, 80 <= Tf <= 87)
  adj_h  <- (rh - 85) / 10 * ((87 - tf) / 5)
  mask_h <- !is.na(rh) & rh > 85 & tf >= 80 & tf <= 87
  hi_f   <- ifelse(mask_h, hi_f + adj_h, hi_f)

  hi_c <- pmin((hi_f - 32) * 5 / 9, 60)  # physical upper guard

  if (apply_mask)
    hi_c <- ifelse(airT < min_temp | rh < min_rh, NA_real_, hi_c)

  round(hi_c, 2)
}


#' THI-Thom (1959)
#' @keywords internal
#' @noRd
.compute_thi <- function(airT, rh) {
  round(airT - ((1 - rh / 100) * (airT - 14.4)) / 2, 2)
}


#' WCET-Environment Canada (2001)
#' @keywords internal
#' @noRd
.compute_wcet <- function(airT, ws, apply_mask = TRUE) {
  ws_kph <- ws * 3.6
  wcet   <- 13.12 + 0.6215 * airT -
            11.37 * pmax(ws_kph, 0.01)^0.16 +
            0.3965 * airT * pmax(ws_kph, 0.01)^0.16
  if (apply_mask) wcet <- ifelse(airT > 10 | ws <= 1.3, NA_real_, wcet)
  round(wcet, 2)
}


#' WCT-NWS (2001)
#' @keywords internal
#' @noRd
.compute_wct <- function(airT, ws, apply_mask = TRUE) {
  tf     <- (airT * 9 / 5) + 32
  ws_mph <- pmax(ws * 0.621371, 0.01)
  wct_f  <- 35.74 + 0.6215*tf - 35.75*ws_mph^0.16 + 0.4275*tf*ws_mph^0.16
  wct_c  <- (wct_f - 32) * 5 / 9
  if (apply_mask) wct_c <- ifelse(airT > 10 | ws <= 1.3, NA_real_, wct_c)
  round(wct_c, 2)
}


#' ET-Missenard (1937); NA wind treated as calm
#' @keywords internal
#' @noRd
.compute_et <- function(airT, rh, ws) {
  ws_safe <- pmax(ws, 0.04)
  et      <- airT - 0.4*(airT - 10)*(1 - rh/100) - 1.1*(ws_safe - 0.2)^0.5
  round(pmax(et, -273.15), 2)
}


#' UTCI-Fiala polynomial approximation (Brode et al. 2012)
#'
#' Full UTCI requires >200-term polynomial; this uses a validated multi-term
#' regression capturing: vapour pressure, logarithmic wind, radiation linear,
#' radiation non-linear, and wind vs radiation interaction.
#' NA solar -> 0; NA wind -> calm.
#'
#' @keywords internal
#' @noRd
.compute_utci <- function(airT, rh, ws, sr_kj_m2, utci_correction = 0) {
  sr_wm2 <- sr_kj_m2 / 3.6
  es     <- 0.6108 * exp((17.27 * airT) / (airT + 237.3))
  vp     <- (rh / 100) * es          # kPa
  t_mrt  <- airT + 0.07 * sr_wm2 - 1.5
  ws10   <- pmax(ws * 1.5, 0.5)
  d_tmrt <- t_mrt - airT
  vp_dev <- vp * 1000 - 1000         # Pa deviation from 1 kPa

  utci <- airT +
    0.048   * vp_dev / 100 +
    (-0.29) * log(ws10 + 0.1) +
    0.016   * d_tmrt +
    0.010   * d_tmrt^2 / 10 +
    (-6e-5) * d_tmrt^2 * ws10 +
    utci_correction

  round(pmin(pmax(utci, -60), 50), 2)
}


#' PET-MEMI-based approximation (Matzarakis et al. 1999)
#'
#' Seasonal clothing (clo) adjustment + regional correction.
#' NA solar -> 0; NA wind -> calm.
#'
#' @keywords internal
#' @noRd
.compute_pet <- function(airT, rh, ws, sr_kj_m2, date,
                         pet_correction = 0, clothing_factor = 0.6) {
  sr_wm2 <- sr_kj_m2 / 3.6
  t_mrt  <- airT + 0.06 * sr_wm2 - 2.0

  month_num <- if (inherits(date, c("POSIXct", "POSIXlt", "Date")))
    as.integer(format(date, "%m"))
  else
    rep(6L, length(airT))

  # Southern hemisphere: austral summer = Dec/Jan/Feb -> low clo
  clo <- dplyr::case_when(
    month_num %in% c(12L, 1L, 2L)               ~ 0.5 * clothing_factor,
    month_num %in% c(3L, 4L, 5L, 9L, 10L, 11L) ~ 0.7 * clothing_factor,
    TRUE                                         ~ 0.9 * clothing_factor
  )

  ws_eff <- pmax(ws, 0.1)

  pet <- airT +
    0.10 * (rh - 50) / 10 +
    (-0.30) * ws_eff +
    0.05 * (t_mrt - airT) +
    0.50 * (1 - clo) +
    pet_correction

  round(pet, 2)
}


#' CDD
#' @keywords internal
#' @noRd
.compute_cdd <- function(airT, base_temp = 18)
  round(pmax(airT - base_temp, 0), 1)


#' HDD
#' @keywords internal
#' @noRd
.compute_hdd <- function(airT, base_temp = 18)
  round(pmax(base_temp - airT, 0), 1)


#' GDD-modified with upper threshold
#' @keywords internal
#' @noRd
.compute_gdd <- function(airT, base_temp = 10, upper_temp = 30)
  round(pmin(pmax(airT, base_temp), upper_temp) - base_temp, 1)


#' Diurnal Temperature Range-true daily aggregate broadcast to hourly rows
#'
#' Uses tair_max_c / tair_min_c if they represent genuine daily extremes
#' (day-to-day SD > 0.5 C); otherwise derives from tair_dry_bulb_c.
#'
#' @keywords internal
#' @noRd
.compute_diurnal_range <- function(df, tmax_col = "tair_max_c",
                                   tmin_col  = "tair_min_c") {
  n       <- nrow(df)
  day_key <- as.character(as.Date(df[["date"]]))
  has_cols <- tmax_col %in% colnames(df) && tmin_col %in% colnames(df)

  if (has_cols) {
    diffs  <- df[[tmax_col]] - df[[tmin_col]]
    dsd    <- if (length(unique(day_key)) > 1)
                sd(tapply(diffs, day_key, mean, na.rm = TRUE), na.rm = TRUE)
              else 0
    if (!is.na(dsd) && dsd > 0.5) {
      d_max <- tapply(df[[tmax_col]], day_key, max, na.rm = TRUE)
      d_min <- tapply(df[[tmin_col]], day_key, min, na.rm = TRUE)
      return(round(as.numeric((d_max - d_min)[day_key]), 2))
    }
  }

  if (!"tair_dry_bulb_c" %in% colnames(df)) return(rep(NA_real_, n))
  t     <- df[["tair_dry_bulb_c"]]
  d_max <- tapply(t, day_key, max, na.rm = TRUE)
  d_min <- tapply(t, day_key, min, na.rm = TRUE)
  round(as.numeric((d_max - d_min)[day_key]), 2)
}


#' Vapour pressure - Magnus-Tetens (±1 % in 0-40 °C)
#' @keywords internal
#' @noRd
.compute_vapor_pressure <- function(airT, rh) {
  es <- 0.6108 * exp((17.27 * airT) / (airT + 237.3))
  round(es * (rh / 100), 3)
}


#' Heat Stress Risk- WBGT-based, OSHA/ACGIH classification
#' @keywords internal
#' @noRd
.compute_heat_stress_risk <- function(airT, rh, ws, sr_kj_m2) {
  wbgt <- .compute_wbgt(airT, rh, sr_kj_m2, ws)
  dplyr::case_when(
    wbgt > 33 ~ "Extreme",
    wbgt > 30 ~ "Very High",
    wbgt > 28 ~ "High",
    wbgt > 25 ~ "Moderate",
    wbgt > 20 ~ "Low",
    TRUE      ~ "None"
  )
}


#' Koppen humidity classification (simplified)
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
# PHYSICAL CONSISTENCY-SOLAR RADIATION
# ============================================================================

#' Verify solar radiation against extraterrestrial limits
#' @keywords internal
#' @noRd
.verify_solar_radiation <- function(df_work, verbose, lang) {

  # Unconditional: set negatives to 0
  df_work[["sr_kj_m2"]] <- ifelse(
    !is.na(df_work[["sr_kj_m2"]]) & df_work[["sr_kj_m2"]] < 0,
    0, df_work[["sr_kj_m2"]]
  )

  if (!all(c("latitude", "date") %in% colnames(df_work))) return(df_work)

  lat_rad <- df_work[["latitude"]] * pi / 180
  doy     <- as.integer(format(df_work[["date"]], "%j"))
  hour_h  <- as.integer(format(df_work[["date"]], "%H"))

  dec_rad <- (23.45 * sin(2 * pi * (284 + doy) / 365)) * pi / 180
  ha_rad  <- ((hour_h - 12) * 15) * pi / 180

  sin_elev <- sin(lat_rad) * sin(dec_rad) +
              cos(lat_rad) * cos(dec_rad) * cos(ha_rad)

  G0_kj <- .SUS_PHYSICS$solar_constant *
            (1 + 0.033 * cos(2 * pi * doy / 365)) *
            pmax(sin_elev, 0) * 3.6     # W/m2 -> kJ/m2 (hourly)

  n_flag <- sum(!is.na(df_work[["sr_kj_m2"]]) &
                  df_work[["sr_kj_m2"]] > G0_kj * 1.1, na.rm = TRUE)

  if (n_flag > 0 && verbose)
    cli::cli_alert_warning(switch(lang,
      pt = sprintf("Consistencia fisica: %d valores de SR > 110%% da irradiancia extraterrestre.", n_flag),
      es = sprintf("Consistencia fisica: %d valores de SR > 110%% de la irradiancia extraterrestre.", n_flag),
           sprintf("Physical consistency: %d SR values exceed 110%% of extraterrestrial irradiance.", n_flag)
    ))

  df_work
}


# ============================================================================
# CONFIDENCE FLAGS  (multi-level)
# ============================================================================

#' Add _flag_extreme / _flag_high / _flag_low columns
#' @keywords internal
#' @noRd
.add_confidence_flags <- function(result, col_name, thresholds) {

  vals <- result[[col_name]]
  if (!is.numeric(vals)) return(result)   # skip categorical

  # Highest heat threshold available
  thr_extreme <- thresholds$extreme_heat %||%
                 thresholds$extreme_heat_stress %||%
                 thresholds$extreme_danger %||%
                 thresholds$high_risk
  result[[paste0(col_name, "_flag_extreme")]] <-
    if (!is.null(thr_extreme)) !is.na(vals) & vals > thr_extreme
    else rep(FALSE, length(vals))

  # Mid-range heat threshold
  thr_high <- thresholds$high_stress %||%
              thresholds$dangerous   %||%
              thresholds$moderate_heat_stress %||%
              thresholds$extreme_caution %||%
              thresholds$hot
  result[[paste0(col_name, "_flag_high")]] <-
    if (!is.null(thr_high)) !is.na(vals) & vals > thr_high
    else rep(FALSE, length(vals))

  # Low / cold threshold
  thr_low <- thresholds$warning_low %||%
             thresholds$low_stress  %||%
             thresholds$slight_cold_stress %||%
             thresholds$cold
  result[[paste0(col_name, "_flag_low")]] <-
    if (!is.null(thr_low)) !is.na(vals) & vals < thr_low
    else rep(FALSE, length(vals))

  result
}


# ============================================================================
# MONTE CARLO UNCERTAINTY
# ============================================================================

#' 200-simulation Monte Carlo 95 % CI for numeric indicators
#' @keywords internal
#' @noRd
.compute_mc_uncertainty <- function(ind, df, reg, region_params, n_sim = 200L) {

  unc  <- reg$uncertainty_sd
  n    <- nrow(df)
  sims <- matrix(NA_real_, nrow = n, ncol = n_sim)
  set.seed(2024L)

  for (i in seq_len(n_sim)) {
    df_s <- df

    if (!is.null(unc$temp) && "tair_dry_bulb_c" %in% colnames(df_s))
      df_s[["tair_dry_bulb_c"]] <- df[["tair_dry_bulb_c"]] +
                                   stats::rnorm(n, 0, unc$temp)

    if (!is.null(unc$rh) && "rh_mean_porc" %in% colnames(df_s))
      df_s[["rh_mean_porc"]] <-
        pmin(pmax(df[["rh_mean_porc"]] + stats::rnorm(n, 0, unc$rh), 0), 100)

    if (!is.null(unc$ws) && "ws_2_m_s" %in% colnames(df_s))
      df_s[["ws_2_m_s"]] <- pmax(df[["ws_2_m_s"]] + stats::rnorm(n, 0, unc$ws), 0)

    if (!is.null(unc$sr_frac) && "sr_kj_m2" %in% colnames(df_s))
      df_s[["sr_kj_m2"]] <-
        pmax(df[["sr_kj_m2"]] * (1 + stats::rnorm(n, 0, unc$sr_frac)), 0)

    vals <- tryCatch(
      .dispatch_indicator(ind, df_s, reg, region_params, apply_mask = FALSE),
      error = function(e) rep(NA_real_, n)
    )
    if (is.numeric(vals)) sims[, i] <- vals
  }

  list(
    low  = round(apply(sims, 1, stats::quantile, 0.025, na.rm = TRUE), 2),
    high = round(apply(sims, 1, stats::quantile, 0.975, na.rm = TRUE), 2)
  )
}


# ============================================================================
# REGION DETECTION
# ============================================================================

#' Detect Brazilian biome from UF column or latitude
#' @keywords internal
#' @noRd
.detect_region_from_data <- function(df, station_col, verbose = TRUE) {

  uf_map <- list(
    amazon           = c("AC", "AM", "AP", "PA", "RO", "RR", "TO"),
    caatinga         = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE"),
    atlantic_forest  = c("ES", "RJ", "SP"),
    pampa            = c("RS"),
    south            = c("PR", "SC"),
    southeast        = c("MG"),
    cerrado          = c("DF", "GO"),
    pantanal_cerrado = c("MT", "MS")
  )

  if ("UF" %in% colnames(df)) {
    uf_mode <- df |>
      dplyr::filter(!is.na(.data$UF)) |>
      dplyr::count(.data$UF, sort = TRUE) |>
      dplyr::pull(.data$UF) |>
      (\(x) x[1L])()

    if (!is.na(uf_mode)) {
      for (rgn in names(uf_map)) {
        if (uf_mode %in% uf_map[[rgn]]) {
          if (rgn == "pantanal_cerrado") return(.resolve_pantanal_cerrado(df))
          return(rgn)
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


#' Resolve MT/MS ambiguity: Pantanal vs Cerrado
#' @keywords internal
#' @noRd
.resolve_pantanal_cerrado <- function(df) {
  if ("longitude" %in% colnames(df)) {
    lon <- mean(df[["longitude"]], na.rm = TRUE)
    if (!is.nan(lon) && lon <= -55 && lon >= -58) return("pantanal")
  }
  "cerrado"
}


# ============================================================================
# MULTILINGUAL MESSAGES
# ============================================================================

#' Message templates
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
      skipping_indicator    = "Indicador %s ignorado-variavel(is) ausente(s): %s",
      indicator_done        = "OK %s (%s) -> coluna '%s': %d/%d valores validos (%.1f%%)",
      done                  = "Concluido: %d/%d indicadores calculados com sucesso."
    ),
    en = list(
      no_datetime_found     = "No datetime column found.",
      no_station_found      = "No station column found. Processing all data together.",
      station_col_not_found = "Station column '%s' not found.",
      unknown_indicators    = "Unknown indicator code(s): %s. Valid options: %s.",
      computing_indicators  = "Computing %d indicator(s): %s",
      missing_vars_global   = "Variables absent from data frame (some indicators will be NA): %s",
      skipping_indicator    = "Skipping indicator %s-missing variable(s): %s",
      indicator_done        = "OK %s (%s) -> column '%s': %d/%d valid values (%.1f%%)",
      done                  = "Done: %d/%d indicators computed successfully."
    ),
    es = list(
      no_datetime_found     = "No se encontro columna de fecha/hora.",
      no_station_found      = "No se encontro columna de estacion. Procesando todos los datos juntos.",
      station_col_not_found = "Columna de estacion '%s' no encontrada.",
      unknown_indicators    = "Codigo(s) de indicador desconocido(s): %s. Opciones validas: %s.",
      computing_indicators  = "Calculando %d indicador(es): %s",
      missing_vars_global   = "Variables ausentes en el dataframe (algunos indicadores seran NA): %s",
      skipping_indicator    = "Indicador %s omitido-variable(s) ausente(s): %s",
      indicator_done        = "OK %s (%s) -> columna '%s': %d/%d valores validos (%.1f%%)",
      done                  = "Completado: %d/%d indicadores calculados correctamente."
    )
  )
  msgs[[match.arg(lang, names(msgs))]]
}


# ============================================================================
# PACKAGE HELPER STUBS
# (self-contained when sourced outside the package)
# ============================================================================

.detect_data_source <- function(df) {
  cols <- tolower(colnames(df))
  if (any(grepl("station_code|estacao_codigo|codigo_estacao", cols))) return("INMET")
  if (any(grepl("station_id|wban", cols)))                             return("NOAA")
  "Unknown"
}

.detect_datetime_column <- function(df, datetime_col, verbose, msgs) {
  if (!is.null(datetime_col)) return(datetime_col)
  dc <- colnames(df)[vapply(df, function(x)
    inherits(x, c("Date", "POSIXct", "POSIXlt")), logical(1))]
  if (length(dc) == 0) cli::cli_abort(msgs$no_datetime_found)
  dc[1L]
}

.detect_station_column <- function(df, station_col, source, verbose, msgs) {
  if (!is.null(station_col)) {
    if (station_col %in% colnames(df)) return(station_col)
    if (verbose) cli::cli_alert_warning(sprintf(msgs$station_col_not_found, station_col))
  }
  patterns <- switch(source,
    INMET = c("station_code", "estacao_codigo", "codigo_estacao"),
    NOAA  = c("station_id", "wban"),
    c("station", "site", "location", "id")
  )
  for (p in patterns) {
    hit <- colnames(df)[grepl(p, tolower(colnames(df)))]
    if (length(hit) > 0) return(hit[1L])
  }
  if (verbose) cli::cli_alert_warning(msgs$no_station_found)
  NULL
}


# ============================================================================
# NULL-COALESCING OPERATOR
# ============================================================================
if (!exists("%||%", envir = baseenv(), inherits = FALSE)) {
  `%||%` <- function(x, y) if (!is.null(x)) x else y
}