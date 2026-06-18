# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "code_muni", "date", ".data", "rain_rolling", "spi_val",
  "scale_label", "n_valid", "p_zero"
))

# ── Exported function ─────────────────────────────────────────────────────────

#' Compute Standardized Precipitation Index (SPI)
#'
#' @description
#' `sus_climate_compute_spi()` calculates the Standardized Precipitation Index
#' (SPI; McKee et al., 1993) at multiple timescales from monthly cumulative
#' precipitation already aggregated to municipalities.
#'
#' SPI is a dimensionless index of precipitation anomaly relative to a
#' long-term distribution: negative values indicate drought (SPI \eqn{\le}
#' -1 = moderate, \eqn{\le} -1.5 = severe, \eqn{\le} -2 = extreme) and
#' positive values indicate wet conditions. Multi-scale SPI is a leading
#' indicator for several health outcomes in Brazil:
#' \itemize{
#'   \item Drought (SPI-3, SPI-6): diarrhoeal disease, malnutrition,
#'     vector-borne diseases during water scarcity
#'   \item Wet spells (SPI-1, SPI-3): leptospirosis, hepatitis A, dengue
#' }
#'
#' The function requires **monthly** precipitation data (one row per
#' municipality × month). The best upstream source is
#' [sus_grid_chirps()] with `resolution = "monthly"` (0.05°, 1981–present).
#' INMET-derived `rainfall_mm` from [sus_climate_aggregate()] also works.
#'
#' @param df A `climasus_df` at `stage = "climate"` with monthly precipitation.
#'   Must contain `code_muni` (character), `date` (Date, first day of each
#'   month), and the column named by `var`.
#'
#' @param var Character. Name of the monthly precipitation column.
#'   Default `"rainfall_chirps_mm"`. Other common value: `"rainfall_mm"`.
#'
#' @param scales Integer vector. SPI timescales in months. Default
#'   `c(1L, 3L, 6L, 12L)`. Common choices: 1 (soil moisture), 3 (short-term),
#'   6 (agriculture/water supply), 12 (groundwater/streamflow).
#'
#' @param ref_start Date or `NULL`. Start of the calibration period for
#'   gamma distribution fitting. `NULL` (default) uses all available data.
#'   WMO recommends at least 30 years; CHIRPS 1981–2010 is ideal.
#'
#' @param ref_end Date or `NULL`. End of the calibration period.
#'   `NULL` (default) uses all available data.
#'
#' @param min_n Integer. Minimum number of non-NA, non-zero precipitation
#'   values required to fit the gamma distribution per location. Default `24L`
#'   (2 years). Locations with fewer values receive `NA` for all SPI columns.
#'
#' @param lang Character. Message language: `"pt"` (default), `"en"`, `"es"`.
#'
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return The input `climasus_df` with additional columns:
#'   \itemize{
#'     \item `spi_{s}mo` — SPI at timescale `s` months (numeric, dimensionless)
#'   }
#'   For example, `scales = c(1, 3, 6, 12)` adds `spi_1mo`, `spi_3mo`,
#'   `spi_6mo`, `spi_12mo`. Metadata: `stage = "climate"`, `type = "spi"`.
#'
#' @section SPI classification:
#' \preformatted{
#'  SPI ≥  2.0 : Extremely wet
#'  SPI   1.5 to  1.99 : Very wet
#'  SPI   1.0 to  1.49 : Moderately wet
#'  SPI  -0.99 to  0.99 : Near normal
#'  SPI  -1.0 to -1.49 : Moderately dry (D1)
#'  SPI  -1.5 to -1.99 : Severely dry   (D2)
#'  SPI ≤ -2.0 : Extremely dry (D3-D4)
#' }
#'
#' @section Algorithm:
#' For each municipality and timescale `s`:
#' \enumerate{
#'   \item Compute `s`-month rolling sum of precipitation using
#'     [slider::slide_dbl()].
#'   \item Estimate gamma distribution parameters (shape \eqn{\alpha},
#'     rate \eqn{\beta}) via method of moments from the calibration period,
#'     handling zero-inflation with a mixed distribution (McKee et al., 1993).
#'   \item Transform to standard normal: `SPI = qnorm(p0 + (1-p0) *
#'     pgamma(x, shape, rate))`, where `p0` is the proportion of zeros.
#' }
#'
#' @references
#' McKee, T.B., Doesken, N.J., Kleist, J. (1993). The relationship of
#' drought frequency and duration to time scales. *Proceedings of the 8th
#' Conference on Applied Climatology*. American Meteorological Society.
#'
#' @examples
#' \dontrun{
#' library(geobr)
#' mt_mun <- read_municipality(code_muni = "MT", year = 2020)
#'
#' # Download monthly CHIRPS for Mato Grosso, 1990-2022
#' chirps_mt <- sus_grid_chirps(
#'   resolution    = "monthly",
#'   years         = 1990:2022,
#'   municipalities = mt_mun
#' )
#'
#' # Compute SPI at 1-, 3-, 6- and 12-month timescales
#' spi_mt <- sus_climate_compute_spi(
#'   df         = chirps_mt,
#'   var        = "rainfall_chirps_mm",
#'   scales     = c(1L, 3L, 6L, 12L),
#'   ref_start  = as.Date("1991-01-01"),
#'   ref_end    = as.Date("2020-12-31"),
#'   lang       = "pt"
#' )
#'
#' sus_meta(spi_mt, "stage")  # "climate"
#' sus_meta(spi_mt, "type")   # "spi"
#' names(spi_mt)              # ..., spi_1mo, spi_3mo, spi_6mo, spi_12mo
#'
#' # Join to health data
#' combined <- sus_grid_join(health_mt, spi_mt)
#' }
#'
#' @seealso [sus_grid_chirps()], [sus_climate_anomaly()], [sus_grid_join()],
#'   [sus_mod_dlnm()]
#'
#' @export
#' @importFrom dplyr mutate filter across all_of arrange n_distinct
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom rlang .data check_installed
#' @importFrom tibble as_tibble
sus_climate_compute_spi <- function(
    df,
    var       = "rainfall_chirps_mm",
    scales    = c(1L, 3L, 6L, 12L),
    ref_start = NULL,
    ref_end   = NULL,
    min_n     = 24L,
    lang      = "pt",
    verbose   = TRUE) {

  # ── 1. Validation ───────────────────────────────────────────────────────────

  if (!is.character(lang) || length(lang) != 1 || !lang %in% c("pt", "en", "es")) {
    cli::cli_abort("{.arg lang} must be 'pt', 'en', or 'es'.")
  }
  msg <- .spi_msgs[[lang]]

  # Require slider for rolling windows
  rlang::check_installed("slider",
    reason = "to compute rolling-sum precipitation windows for SPI")

  # --- input df ---
  if (inherits(df, c("arrow_dplyr_query", "Dataset", "ArrowTabular", "Table"))) {
    meta_backup <- tryCatch(sus_meta(df), error = function(e) list())
    df <- dplyr::collect(df)
    if (length(meta_backup) > 0) df <- new_climasus_df(df, meta_backup)
  }

  if (!inherits(df, "climasus_df")) {
    cli::cli_abort(c(
      msg$not_climasus,
      "i" = msg$not_climasus_hint
    ))
  }

  current_stage <- sus_meta(df, "stage")
  if (is.null(current_stage) || current_stage != "climate") {
    stage <- current_stage %||% "NULL"
    cli::cli_abort(c(msg$wrong_stage, "i" = msg$wrong_stage_hint))
  }

  # --- var column ---
  if (!is.character(var) || length(var) != 1) cli::cli_abort(msg$invalid_var_type)
  if (!var %in% names(df)) {
    cli::cli_abort(glue::glue(msg$var_not_found, var = var,
                               cols = paste(names(df), collapse = ", ")))
  }

  # --- required columns ---
  for (col in c("code_muni", "date")) {
    if (!col %in% names(df)) {
      cli::cli_abort(glue::glue(msg$missing_col, col = col))
    }
  }

  # --- scales ---
  if (!is.numeric(scales) || length(scales) == 0 || any(scales < 1)) {
    cli::cli_abort(msg$invalid_scales)
  }
  scales <- sort(as.integer(unique(scales)))

  # --- reference period ---
  if (!is.null(ref_start)) ref_start <- as.Date(ref_start)
  if (!is.null(ref_end))   ref_end   <- as.Date(ref_end)
  if (!is.null(ref_start) && !is.null(ref_end) && ref_start >= ref_end) {
    cli::cli_abort(msg$invalid_ref_period)
  }

  # --- min_n ---
  min_n <- as.integer(min_n[1])
  if (is.na(min_n) || min_n < 2L) cli::cli_abort(msg$invalid_min_n)

  # ── 2. Checks on data ───────────────────────────────────────────────────────
  df$date <- as.Date(df$date)
  n_loc   <- dplyr::n_distinct(df$code_muni)
  n_dates <- dplyr::n_distinct(df$date)

  if (verbose) {
    cli::cli_h1(msg$title)
    cli::cli_alert_info(glue::glue(msg$start_info,
                                    n_loc   = n_loc,
                                    n_dates = n_dates,
                                    n_scales = length(scales),
                                    scales_str = paste0(scales, "mo",
                                                        collapse = ", ")))
  }

  # Warn if data looks daily (too many dates per municipality)
  avg_dates_per_loc <- n_dates / max(n_loc, 1L)
  if (avg_dates_per_loc > 36 && max(scales) < avg_dates_per_loc / 12) {
    cli::cli_alert_warning(msg$looks_daily)
  }

  # ── 3. Compute SPI per scale ─────────────────────────────────────────────────
  df <- tibble::as_tibble(df)
  df <- dplyr::arrange(df, .data$code_muni, .data$date)

  for (s in scales) {
    col_name <- paste0("spi_", s, "mo")
    if (verbose) cli::cli_alert_info(
      glue::glue(msg$computing_scale, s = s, col = col_name))

    df[[col_name]] <- .spi_compute_scale(
      df       = df,
      var      = var,
      s        = s,
      ref_start = ref_start,
      ref_end   = ref_end,
      min_n    = min_n
    )
  }

  # ── 4. Update sus_meta ───────────────────────────────────────────────────────
  scale_cols <- paste0("spi_", scales, "mo")
  df <- new_climasus_df(df, sus_meta(new_climasus_df(
    tibble::as_tibble(df),
    sus_meta(df)
  )))

  df <- sus_meta(df, stage = "climate", type = "spi")
  df <- sus_meta(df, add_history = sprintf(
    "sus_climate_compute_spi(): scales=%s, var=%s, ref=%s/%s",
    paste0(scales, "mo", collapse = "+"),
    var,
    if (!is.null(ref_start)) format(ref_start) else "all",
    if (!is.null(ref_end))   format(ref_end)   else "all"
  ))

  n_rows <- nrow(df)
  n_na   <- sum(is.na(df[[scale_cols[1]]]))
  if (verbose) {
    cli::cli_alert_success(glue::glue(
      msg$done,
      n_rows = n_rows,
      n_na   = n_na,
      col1   = scale_cols[1]
    ))
  }

  df
}


# ── Internal: compute SPI for one timescale ───────────────────────────────────

#' Compute SPI at one timescale for all municipalities in df
#' @keywords internal
#' @noRd
.spi_compute_scale <- function(df, var, s, ref_start, ref_end, min_n) {
  locs <- unique(df$code_muni)
  out  <- numeric(nrow(df))
  out[] <- NA_real_

  for (loc in locs) {
    idx <- which(df$code_muni == loc)
    x   <- df[[var]][idx]

    # rolling sum of s months (current + s-1 previous)
    rain_roll <- slider::slide_dbl(x, sum, .before = s - 1L, .after = 0L,
                                    .complete = TRUE)

    # Identify calibration period rows
    if (!is.null(ref_start) || !is.null(ref_end)) {
      dates   <- df$date[idx]
      in_ref  <- rep(TRUE, length(dates))
      if (!is.null(ref_start)) in_ref <- in_ref & dates >= ref_start
      if (!is.null(ref_end))   in_ref <- in_ref & dates <= ref_end
      calib   <- rain_roll[in_ref & !is.na(rain_roll)]
    } else {
      calib <- rain_roll[!is.na(rain_roll)]
    }

    if (length(calib) < min_n) {
      out[idx] <- NA_real_
      next
    }

    # Estimate gamma parameters via method of moments from calibration data
    # Mixed distribution handles zero inflation (common in dryland Brazil)
    spi_vals <- .spi_transform(rain_roll, calib)
    out[idx] <- spi_vals
  }
  out
}

#' Fit gamma to calibration data and transform full series to SPI
#'
#' Uses mixed gamma-zero distribution (McKee et al., 1993).
#' Method of moments: shape = mean^2/var, rate = mean/var.
#' @keywords internal
#' @noRd
.spi_transform <- function(x_full, x_calib) {
  # Separate zeros and positives
  nz   <- x_calib[x_calib > 0 & !is.na(x_calib)]
  p0   <- mean(x_calib == 0, na.rm = TRUE)   # proportion of zeros

  if (length(nz) < 4L) {
    return(rep(NA_real_, length(x_full)))
  }

  # Method of moments for gamma distribution (positive values only)
  mu   <- mean(nz)
  s2   <- stats::var(nz)
  if (s2 <= 0 || is.na(mu) || is.na(s2)) {
    return(rep(NA_real_, length(x_full)))
  }
  shape <- mu^2 / s2
  rate  <- mu   / s2

  # Transform: SPI = qnorm( p0 + (1-p0) * pgamma(x, shape, rate) )
  # For x == 0: CDF is p0; for NA: keep NA
  spi <- rep(NA_real_, length(x_full))
  non_na <- !is.na(x_full)
  if (sum(non_na) == 0) return(spi)

  cdf_vals <- numeric(sum(non_na))
  xv <- x_full[non_na]

  zero_mask     <- xv == 0
  pos_mask      <- xv > 0
  cdf_vals[zero_mask] <- p0
  cdf_vals[pos_mask]  <- p0 + (1 - p0) *
    stats::pgamma(xv[pos_mask], shape = shape, rate = rate)

  # Clamp to avoid Inf in qnorm at boundaries
  eps <- .Machine$double.eps
  cdf_vals <- pmax(eps, pmin(1 - eps, cdf_vals))

  spi[non_na] <- stats::qnorm(cdf_vals)
  spi
}


# ── Multilingual messages ─────────────────────────────────────────────────────

#' @keywords internal
#' @noRd
.spi_msgs <- list(
  pt = list(
    title            = "Calculando SPI (Standardized Precipitation Index)",
    not_climasus     = "{.arg df} deve ser um {.cls climasus_df}.",
    not_climasus_hint = "Use {.fn sus_grid_chirps} ou {.fn sus_climate_aggregate} para criar o objeto.",
    wrong_stage      = "{.arg df} est\u00e1 em stage '{stage}'; esperado: climate.",
    wrong_stage_hint = "Execute {.fn sus_grid_chirps} ou {.fn sus_climate_aggregate} primeiro.",
    invalid_var_type = "{.arg var} deve ser uma string.",
    var_not_found    = "Coluna '{var}' n\u00e3o encontrada. Colunas dispon\u00edveis: {cols}.",
    missing_col      = "Coluna obrigat\u00f3ria '{col}' n\u00e3o encontrada em {.arg df}.",
    invalid_scales   = "{.arg scales} deve ser um vetor de inteiros \u2265 1.",
    invalid_ref_period = "{.arg ref_start} deve ser anterior a {.arg ref_end}.",
    invalid_min_n    = "{.arg min_n} deve ser um inteiro \u2265 2.",
    looks_daily      = "Os dados parecem ser di\u00e1rios (muitas datas por munic\u00edpio). SPI requer dados mensais.",
    start_info       = "SPI para {n_loc} munic\u00edpio(s), {n_dates} m\u00eas/meses, {n_scales} escala(s): {scales_str}",
    computing_scale  = "Calculando SPI-{s} \u2192 coluna '{col}'...",
    done             = "Conclu\u00eddo: {n_rows} linhas; {n_na} NA(s) em '{col1}'."
  ),
  en = list(
    title            = "Computing SPI (Standardized Precipitation Index)",
    not_climasus     = "{.arg df} must be a {.cls climasus_df}.",
    not_climasus_hint = "Use {.fn sus_grid_chirps} or {.fn sus_climate_aggregate} to create the object.",
    wrong_stage      = "{.arg df} is at stage '{stage}'; expected: climate.",
    wrong_stage_hint = "Run {.fn sus_grid_chirps} or {.fn sus_climate_aggregate} first.",
    invalid_var_type = "{.arg var} must be a string.",
    var_not_found    = "Column '{var}' not found. Available columns: {cols}.",
    missing_col      = "Required column '{col}' not found in {.arg df}.",
    invalid_scales   = "{.arg scales} must be a vector of integers \u2265 1.",
    invalid_ref_period = "{.arg ref_start} must be earlier than {.arg ref_end}.",
    invalid_min_n    = "{.arg min_n} must be an integer \u2265 2.",
    looks_daily      = "Data appears to be daily (too many dates per municipality). SPI requires monthly data.",
    start_info       = "SPI for {n_loc} municipality/ies, {n_dates} month(s), {n_scales} scale(s): {scales_str}",
    computing_scale  = "Computing SPI-{s} \u2192 column '{col}'...",
    done             = "Complete: {n_rows} rows; {n_na} NA(s) in '{col1}'."
  ),
  es = list(
    title            = "Calculando SPI (\u00cdndice Estandarizado de Precipitaci\u00f3n)",
    not_climasus     = "{.arg df} debe ser un {.cls climasus_df}.",
    not_climasus_hint = "Use {.fn sus_grid_chirps} o {.fn sus_climate_aggregate} para crear el objeto.",
    wrong_stage      = "{.arg df} est\u00e1 en stage '{stage}'; se esperaba: climate.",
    wrong_stage_hint = "Ejecute {.fn sus_grid_chirps} o {.fn sus_climate_aggregate} primero.",
    invalid_var_type = "{.arg var} debe ser una cadena.",
    var_not_found    = "Columna '{var}' no encontrada. Columnas disponibles: {cols}.",
    missing_col      = "Columna requerida '{col}' no encontrada en {.arg df}.",
    invalid_scales   = "{.arg scales} debe ser un vector de enteros \u2265 1.",
    invalid_ref_period = "{.arg ref_start} debe ser anterior a {.arg ref_end}.",
    invalid_min_n    = "{.arg min_n} debe ser un entero \u2265 2.",
    looks_daily      = "Los datos parecen ser diarios (demasiadas fechas por municipio). SPI requiere datos mensuales.",
    start_info       = "SPI para {n_loc} municipio(s), {n_dates} mes/meses, {n_scales} escala(s): {scales_str}",
    computing_scale  = "Calculando SPI-{s} \u2192 columna '{col}'...",
    done             = "Completo: {n_rows} filas; {n_na} NA(s) en '{col1}'."
  )
)
