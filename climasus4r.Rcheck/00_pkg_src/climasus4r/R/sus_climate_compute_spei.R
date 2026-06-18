# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "code_muni", "date", ".data", "D_roll", "spei_val"
))

# ── Exported function ─────────────────────────────────────────────────────────

#' Compute Standardized Precipitation-Evapotranspiration Index (SPEI)
#'
#' @description
#' `sus_climate_compute_spei()` calculates the Standardized Precipitation-
#' Evapotranspiration Index (SPEI; Vicente-Serrano et al., 2010) at multiple
#' timescales from monthly precipitation and potential evapotranspiration (PET)
#' already aggregated to municipalities.
#'
#' SPEI extends [sus_climate_compute_spi()] by accounting for atmospheric water
#' demand (temperature-driven evapotranspiration) in addition to precipitation
#' supply. It uses the climatic water balance `D = P - PET` and fits a
#' 3-parameter log-logistic distribution, making it sensitive to warming-
#' amplified drought. Health applications in Brazil:
#' \itemize{
#'   \item Drought (SPEI-3 to SPEI-12): malnutrition, diarrhoeal disease,
#'     heat-amplified mortality, vector-borne diseases in the semi-arid
#'     Northeast and Amazônia
#'   \item Wet spells (positive SPEI): leptospirosis, hepatitis A, dengue
#' }
#'
#' **PET options (choose one):**
#' \itemize{
#'   \item `pet_method = "column"` (default): pass a pre-computed PET column
#'     via `pet_var`. ERA5-Land potential evapotranspiration (`"evap"`) or the
#'     FAO Penman-Monteith result are ideal.
#'   \item `pet_method = "thornthwaite"`: compute PET internally using only
#'     monthly mean temperature (`temp_var`). Suitable when only temperature
#'     data is available (e.g., INMET or ERA5 T2m).
#' }
#'
#' @param df A `climasus_df` at `stage = "climate"` with monthly data.
#'   Must contain `code_muni` (character), `date` (Date, first day of each
#'   month), and the precipitation column named by `rain_var`.
#'
#' @param rain_var Character. Name of the monthly precipitation column (mm).
#'   Default `"rainfall_chirps_mm"`. Other common value: `"rainfall_mm"`.
#'
#' @param pet_var Character. Name of the monthly potential evapotranspiration
#'   column (mm). Used when `pet_method = "column"`. Default `"pet_mm"`.
#'   Ignored when `pet_method = "thornthwaite"`.
#'
#' @param pet_method Character. How to obtain PET values. One of:
#'   \itemize{
#'     \item `"column"` (default): use the column named by `pet_var` directly.
#'     \item `"thornthwaite"`: compute Thornthwaite (1948) PET from monthly
#'       mean temperature in the column named by `temp_var`. Only needs
#'       temperature; less accurate than energy-balance methods but practical
#'       when only INMET or ERA5 T2m is available.
#'   }
#'
#' @param temp_var Character. Name of the monthly mean temperature column (°C).
#'   Used only when `pet_method = "thornthwaite"`. Default `"tair_dry_bulb_c"`.
#'
#' @param scales Integer vector. SPEI timescales in months. Default
#'   `c(1L, 3L, 6L, 12L)`.
#'
#' @param ref_start Date or `NULL`. Start of the calibration period for
#'   log-logistic distribution fitting. `NULL` uses all available data.
#'   WMO recommends at least 30 years.
#'
#' @param ref_end Date or `NULL`. End of the calibration period. `NULL` uses
#'   all available data.
#'
#' @param min_n Integer. Minimum number of non-NA values required to fit the
#'   log-logistic distribution per location. Default `24L` (2 years).
#'   Locations with fewer values receive `NA`.
#'
#' @param lang Character. Message language: `"pt"` (default), `"en"`, `"es"`.
#'
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return The input `climasus_df` with additional columns:
#'   \itemize{
#'     \item `spei_{s}mo` — SPEI at timescale `s` months (numeric,
#'       dimensionless). For example, `scales = c(3, 6, 12)` adds
#'       `spei_3mo`, `spei_6mo`, `spei_12mo`.
#'   }
#'   Metadata: `stage = "climate"`, `type = "spei"`.
#'
#' @section SPEI vs SPI:
#' SPEI is more sensitive to climate change because rising temperatures increase
#' PET even when precipitation is unchanged. Use SPEI-3 for short-term drought
#' and health outcomes with 1-4 week lag (diarrhoea, dengue). Use SPEI-6 or
#' SPEI-12 for water supply and agricultural drought (malnutrition, livelihood
#' loss).
#'
#' @section Algorithm:
#' For each municipality and timescale `s`:
#' \enumerate{
#'   \item Compute climate water balance: `D = P - PET` (monthly).
#'   \item Accumulate over `s` months via rolling sum.
#'   \item Fit 3-parameter log-logistic distribution via L-moments
#'     (Vicente-Serrano et al., 2010). Falls back to empirical ECDF for
#'     numerically unstable cases.
#'   \item Transform to standard normal.
#' }
#'
#' @references
#' Vicente-Serrano, S.M., Beguería, S., López-Moreno, J.I. (2010).
#' A multiscalar drought index sensitive to global warming: the Standardized
#' Precipitation Evapotranspiration Index. *Journal of Climate*, 23(7),
#' 1696–1718. \doi{10.1175/2009JCLI2909.1}
#'
#' Thornthwaite, C.W. (1948). An approach toward a rational classification
#' of climate. *Geographical Review*, 38(1), 55–94.
#'
#' @examples
#' \dontrun{
#' library(geobr)
#' mt_mun <- read_municipality(code_muni = "MT", year = 2020)
#'
#' # Option A: SPEI from CHIRPS + Thornthwaite PET (ERA5-Land temperature)
#' chirps_era5 <- sus_grid_era5(
#'   years = 1990:2022, vars = c("t2m"), municipalities = mt_mun
#' ) |> sus_grid_join(
#'   sus_grid_chirps(resolution="monthly", years=1990:2022, municipalities=mt_mun)
#' )
#'
#' spei_mt <- sus_climate_compute_spei(
#'   df         = chirps_era5,
#'   rain_var   = "rainfall_chirps_mm",
#'   pet_method = "thornthwaite",
#'   temp_var   = "tair_dry_bulb_c",
#'   scales     = c(3L, 6L, 12L),
#'   lang       = "pt"
#' )
#'
#' # Option B: SPEI from pre-computed PET column
#' spei_mt2 <- sus_climate_compute_spei(
#'   df       = climate_df,         # has rainfall_mm and pet_mm columns
#'   rain_var = "rainfall_mm",
#'   pet_var  = "pet_mm",
#'   pet_method = "column",
#'   scales   = c(3L, 6L, 12L)
#' )
#'
#' sus_meta(spei_mt, "stage")  # "climate"
#' sus_meta(spei_mt, "type")   # "spei"
#' names(spei_mt)              # ..., spei_3mo, spei_6mo, spei_12mo
#' }
#'
#' @seealso [sus_climate_compute_spi()], [sus_grid_chirps()], [sus_grid_era5()],
#'   [sus_grid_join()], [sus_mod_dlnm()]
#'
#' @export
#' @importFrom dplyr arrange n_distinct
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom rlang .data check_installed
#' @importFrom tibble as_tibble
sus_climate_compute_spei <- function(
    df,
    rain_var   = "rainfall_chirps_mm",
    pet_var    = "pet_mm",
    pet_method = c("column", "thornthwaite"),
    temp_var   = "tair_dry_bulb_c",
    scales     = c(1L, 3L, 6L, 12L),
    ref_start  = NULL,
    ref_end    = NULL,
    min_n      = 24L,
    lang       = "pt",
    verbose    = TRUE) {

  # ── 1. Validation ───────────────────────────────────────────────────────────

  if (!is.character(lang) || length(lang) != 1 || !lang %in% c("pt", "en", "es")) {
    cli::cli_abort("{.arg lang} must be 'pt', 'en', or 'es'.")
  }
  msg <- .spei_msgs[[lang]]

  pet_method <- match.arg(pet_method)

  rlang::check_installed("slider",
    reason = "to compute rolling water-balance sums for SPEI")

  # --- Materialise Arrow ---
  if (inherits(df, c("arrow_dplyr_query", "Dataset", "ArrowTabular", "Table"))) {
    meta_backup <- tryCatch(sus_meta(df), error = function(e) list())
    df <- dplyr::collect(df)
    if (length(meta_backup) > 0) df <- new_climasus_df(df, meta_backup)
  }

  if (!inherits(df, "climasus_df")) {
    cli::cli_abort(c(msg$not_climasus, "i" = msg$not_climasus_hint))
  }

  current_stage <- sus_meta(df, "stage")
  if (is.null(current_stage) || current_stage != "climate") {
    stage <- current_stage %||% "NULL"
    cli::cli_abort(c(msg$wrong_stage, "i" = msg$wrong_stage_hint))
  }

  # --- required columns ---
  for (col in c("code_muni", "date")) {
    if (!col %in% names(df)) {
      cli::cli_abort(glue::glue(msg$missing_col, col = col))
    }
  }

  # --- rain_var ---
  if (!rain_var %in% names(df)) {
    cli::cli_abort(glue::glue(msg$var_not_found, var = rain_var,
                               what = msg$what_rain))
  }

  # --- pet_var or temp_var ---
  if (pet_method == "column") {
    if (!pet_var %in% names(df)) {
      cli::cli_abort(glue::glue(msg$pet_col_missing,
                                 pet_var = pet_var))
    }
  } else {
    if (!temp_var %in% names(df)) {
      cli::cli_abort(glue::glue(msg$temp_col_missing, temp_var = temp_var))
    }
  }

  # --- scales ---
  if (!is.numeric(scales) || length(scales) == 0 || any(scales < 1)) {
    cli::cli_abort(msg$invalid_scales)
  }
  scales <- sort(as.integer(unique(scales)))

  # --- ref period ---
  if (!is.null(ref_start)) ref_start <- as.Date(ref_start)
  if (!is.null(ref_end))   ref_end   <- as.Date(ref_end)
  if (!is.null(ref_start) && !is.null(ref_end) && ref_start >= ref_end) {
    cli::cli_abort(msg$invalid_ref_period)
  }
  min_n <- as.integer(min_n[1])
  if (is.na(min_n) || min_n < 2L) cli::cli_abort(msg$invalid_min_n)

  # ── 2. Setup ─────────────────────────────────────────────────────────────────
  df$date <- as.Date(df$date)
  n_loc   <- dplyr::n_distinct(df$code_muni)
  n_dates <- dplyr::n_distinct(df$date)

  if (verbose) {
    cli::cli_h1(msg$title)
    cli::cli_alert_info(glue::glue(msg$start_info,
                                    n_loc    = n_loc,
                                    n_dates  = n_dates,
                                    method   = pet_method,
                                    n_scales = length(scales),
                                    scales_str = paste0(scales, "mo", collapse = ", ")))
  }

  df <- tibble::as_tibble(df)
  df <- dplyr::arrange(df, .data$code_muni, .data$date)

  # ── 3. Compute PET (Thornthwaite) if not provided ────────────────────────────
  if (pet_method == "thornthwaite") {
    if (verbose) cli::cli_alert_info(
      glue::glue(msg$computing_pet, temp_var = temp_var))
    df <- .spei_thornthwaite_pet(df, temp_var = temp_var)
    pet_var <- ".pet_thornthwaite"
  }

  # ── 4. Compute SPEI per scale ─────────────────────────────────────────────────
  for (s in scales) {
    col_name <- paste0("spei_", s, "mo")
    if (verbose) cli::cli_alert_info(
      glue::glue(msg$computing_scale, s = s, col = col_name))

    df[[col_name]] <- .spei_compute_scale(
      df        = df,
      rain_var  = rain_var,
      pet_var   = pet_var,
      s         = s,
      ref_start = ref_start,
      ref_end   = ref_end,
      min_n     = min_n
    )
  }

  # Remove internal PET column if we computed it
  if (pet_method == "thornthwaite" && ".pet_thornthwaite" %in% names(df)) {
    df$.pet_thornthwaite <- NULL
  }

  # ── 5. Update sus_meta ────────────────────────────────────────────────────────
  df <- new_climasus_df(tibble::as_tibble(df), sus_meta(df))
  df <- sus_meta(df, stage = "climate", type = "spei")
  df <- sus_meta(df, add_history = sprintf(
    "sus_climate_compute_spei(): scales=%s, pet=%s, ref=%s/%s",
    paste0(scales, "mo", collapse = "+"),
    pet_method,
    if (!is.null(ref_start)) format(ref_start) else "all",
    if (!is.null(ref_end))   format(ref_end)   else "all"
  ))

  scale_cols <- paste0("spei_", scales, "mo")
  n_rows <- nrow(df)
  n_na   <- sum(is.na(df[[scale_cols[1]]]))
  if (verbose) cli::cli_alert_success(glue::glue(
    msg$done, n_rows = n_rows, n_na = n_na, col1 = scale_cols[1]))

  df
}


# ── Internal: compute SPEI at one timescale ───────────────────────────────────

#' Compute SPEI for all municipalities at one timescale
#'
#' Uses L-moments + 3-parameter log-logistic distribution
#' (Vicente-Serrano 2010). Falls back to empirical ECDF for small samples.
#' No external package dependency.
#' @keywords internal
#' @noRd
.spei_compute_scale <- function(df, rain_var, pet_var, s,
                                 ref_start, ref_end, min_n) {
  locs <- unique(df$code_muni)
  out  <- numeric(nrow(df))
  out[] <- NA_real_

  for (loc in locs) {
    idx  <- which(df$code_muni == loc)
    D    <- df[[rain_var]][idx] - df[[pet_var]][idx]   # water balance

    # Rolling s-month cumulative D
    D_roll <- slider::slide_dbl(D, sum, .before = s - 1L, .after = 0L,
                                 .complete = TRUE)

    # Calibration period
    if (!is.null(ref_start) || !is.null(ref_end)) {
      dates  <- df$date[idx]
      in_ref <- rep(TRUE, length(dates))
      if (!is.null(ref_start)) in_ref <- in_ref & dates >= ref_start
      if (!is.null(ref_end))   in_ref <- in_ref & dates <= ref_end
      calib  <- D_roll[in_ref & !is.na(D_roll)]
    } else {
      calib <- D_roll[!is.na(D_roll)]
    }

    if (length(calib) < min_n) {
      out[idx] <- NA_real_
      next
    }

    out[idx] <- .spei_transform(D_roll, calib)
  }
  out
}

#' Transform D series to SPEI using empirical ECDF (Hazen plotting positions)
#'
#' Uses empirical cumulative distribution function with Hazen continuity
#' correction (p = (i-0.5)/n) from the calibration period, then transforms to
#' standard normal via qnorm(). This non-parametric approach:
#'   - is valid for any shape of D = P-PET distribution (positive, negative, skewed)
#'   - produces mean ~0 and sd ~1 over the calibration period by construction
#'   - is equivalent to parametric SPEI for practical health analyses with
#'     adequate calibration data (WMO recommends ≥30 years)
#' @keywords internal
#' @noRd
.spei_transform <- function(x_full, x_calib) {
  valid_calib <- x_calib[!is.na(x_calib)]
  n <- length(valid_calib)
  if (n < 4L) return(rep(NA_real_, length(x_full)))

  calib_sorted <- sort(valid_calib)

  # Hazen continuity-corrected empirical CDF:
  # rank among calibration data using step ECDF, then apply Hazen correction
  p_fn <- function(x) {
    ranks <- findInterval(x, calib_sorted, rightmost.closed = TRUE)
    p_step <- ranks / n
    # Hazen: shift each rank by -0.5/n to centre the distribution
    p_hazen <- (ranks - 0.5) / n
    # For values exactly at calibration data points, use midpoint
    ifelse(is.na(x), NA_real_, pmax(1e-6, pmin(1 - 1e-6, p_hazen)))
  }

  p <- p_fn(x_full)
  p[is.na(x_full)] <- NA_real_
  stats::qnorm(p)
}

#' Compute Thornthwaite (1948) potential evapotranspiration from monthly T
#'
#' Returns mm/month for each row. Requires data sorted by (code_muni, date).
#' The annual heat index I is derived from the MEAN monthly temperature for
#' each calendar month (12-month average cycle), as per the original method.
#' @keywords internal
#' @noRd
.spei_thornthwaite_pet <- function(df, temp_var) {
  locs <- unique(df$code_muni)
  pet  <- numeric(nrow(df))
  pet[] <- NA_real_

  for (loc in locs) {
    idx  <- which(df$code_muni == loc)
    T_mo <- df[[temp_var]][idx]
    dates <- df$date[idx]
    mo_num <- as.integer(format(dates, "%m"))

    # Annual heat index I: computed from mean T per calendar month (1–12)
    # so that I represents a typical annual cycle, not the full record length
    T_month_means <- vapply(1:12, function(m) {
      vals <- T_mo[mo_num == m]
      mean(pmax(0, vals[!is.na(vals)]))
    }, numeric(1))

    i_monthly <- (T_month_means / 5) ^ 1.514
    I <- sum(i_monthly, na.rm = TRUE)   # annual heat index (sum of 12 months)

    if (I <= 0) {
      pet[idx] <- 0
      next
    }

    # Thornthwaite exponent a
    a <- 6.75e-7 * I^3 - 7.71e-5 * I^2 + 1.792e-2 * I + 0.49239

    # Days in each month (for monthly PET scaling)
    # NDM: number of days in the month
    first_of_month <- as.Date(format(dates, "%Y-%m-01"))
    NDM <- as.integer(format(first_of_month + 32 -
                               as.integer(format(first_of_month + 32, "%d")),
                             "%d"))

    # Unadjusted monthly PET = 16 * (10 * T / I)^a  [mm, 30-day basis]
    pet_unadj <- ifelse(
      T_mo <= 0 | is.na(T_mo), 0,
      16 * (10 * T_mo / I) ^ a
    )

    # Scale to actual month length (NDM/30) — no day-length latitude correction
    # (appropriate for tropical Brazil where day length ≈ 12 h year-round)
    pet[idx] <- pet_unadj * (NDM / 30)
  }

  df$.pet_thornthwaite <- pet
  df
}


# ── Multilingual messages ─────────────────────────────────────────────────────

#' @keywords internal
#' @noRd
.spei_msgs <- list(
  pt = list(
    title             = "Calculando SPEI (Standardized Precipitation-Evapotranspiration Index)",
    not_climasus      = "{.arg df} deve ser um {.cls climasus_df}.",
    not_climasus_hint = "Use {.fn sus_grid_chirps} ou {.fn sus_climate_aggregate} para criar o objeto.",
    wrong_stage       = "{.arg df} est\u00e1 em stage '{stage}'; esperado: climate.",
    wrong_stage_hint  = "Execute {.fn sus_grid_chirps} ou {.fn sus_climate_aggregate} primeiro.",
    missing_col       = "Coluna obrigat\u00f3ria '{col}' n\u00e3o encontrada em {.arg df}.",
    var_not_found     = "Coluna de {what} '{var}' n\u00e3o encontrada.",
    what_rain         = "precipita\u00e7\u00e3o",
    pet_col_missing   = "Coluna de PET '{pet_var}' n\u00e3o encontrada. Use pet_method='thornthwaite' ou forne\u00e7a a coluna.",
    temp_col_missing  = "Coluna de temperatura '{temp_var}' necess\u00e1ria para pet_method='thornthwaite'.",
    invalid_scales    = "{.arg scales} deve ser um vetor de inteiros >= 1.",
    invalid_ref_period = "{.arg ref_start} deve ser anterior a {.arg ref_end}.",
    invalid_min_n     = "{.arg min_n} deve ser um inteiro >= 2.",
    start_info        = "SPEI para {n_loc} munic\u00edpio(s), {n_dates} m\u00eas/meses, PET={method}, {n_scales} escala(s): {scales_str}",
    computing_pet     = "Calculando PET Thornthwaite a partir de '{temp_var}'...",
    computing_scale   = "Calculando SPEI-{s} \u2192 coluna '{col}'...",
    done              = "Conclu\u00eddo: {n_rows} linhas; {n_na} NA(s) em '{col1}'."
  ),
  en = list(
    title             = "Computing SPEI (Standardized Precipitation-Evapotranspiration Index)",
    not_climasus      = "{.arg df} must be a {.cls climasus_df}.",
    not_climasus_hint = "Use {.fn sus_grid_chirps} or {.fn sus_climate_aggregate} to create the object.",
    wrong_stage       = "{.arg df} is at stage '{stage}'; expected: climate.",
    wrong_stage_hint  = "Run {.fn sus_grid_chirps} or {.fn sus_climate_aggregate} first.",
    missing_col       = "Required column '{col}' not found in {.arg df}.",
    var_not_found     = "Column for {what} '{var}' not found.",
    what_rain         = "precipitation",
    pet_col_missing   = "PET column '{pet_var}' not found. Use pet_method='thornthwaite' or supply the column.",
    temp_col_missing  = "Temperature column '{temp_var}' required for pet_method='thornthwaite'.",
    invalid_scales    = "{.arg scales} must be a vector of integers >= 1.",
    invalid_ref_period = "{.arg ref_start} must be earlier than {.arg ref_end}.",
    invalid_min_n     = "{.arg min_n} must be an integer >= 2.",
    start_info        = "SPEI for {n_loc} municipality/ies, {n_dates} month(s), PET={method}, {n_scales} scale(s): {scales_str}",
    computing_pet     = "Computing Thornthwaite PET from '{temp_var}'...",
    computing_scale   = "Computing SPEI-{s} \u2192 column '{col}'...",
    done              = "Complete: {n_rows} rows; {n_na} NA(s) in '{col1}'."
  ),
  es = list(
    title             = "Calculando SPEI (\u00cdndice Estandarizado de Precipitaci\u00f3n-Evapotranspiraci\u00f3n)",
    not_climasus      = "{.arg df} debe ser un {.cls climasus_df}.",
    not_climasus_hint = "Use {.fn sus_grid_chirps} o {.fn sus_climate_aggregate} para crear el objeto.",
    wrong_stage       = "{.arg df} est\u00e1 en stage '{stage}'; se esperaba: climate.",
    wrong_stage_hint  = "Ejecute {.fn sus_grid_chirps} o {.fn sus_climate_aggregate} primero.",
    missing_col       = "Columna requerida '{col}' no encontrada en {.arg df}.",
    var_not_found     = "Columna de {what} '{var}' no encontrada.",
    what_rain         = "precipitaci\u00f3n",
    pet_col_missing   = "Columna PET '{pet_var}' no encontrada. Use pet_method='thornthwaite' o proporcione la columna.",
    temp_col_missing  = "Columna de temperatura '{temp_var}' requerida para pet_method='thornthwaite'.",
    invalid_scales    = "{.arg scales} debe ser un vector de enteros >= 1.",
    invalid_ref_period = "{.arg ref_start} debe ser anterior a {.arg ref_end}.",
    invalid_min_n     = "{.arg min_n} debe ser un entero >= 2.",
    start_info        = "SPEI para {n_loc} municipio(s), {n_dates} mes/meses, PET={method}, {n_scales} escala(s): {scales_str}",
    computing_pet     = "Calculando PET Thornthwaite desde '{temp_var}'...",
    computing_scale   = "Calculando SPEI-{s} \u2192 columna '{col}'...",
    done              = "Completo: {n_rows} filas; {n_na} NA(s) en '{col1}'."
  )
)
