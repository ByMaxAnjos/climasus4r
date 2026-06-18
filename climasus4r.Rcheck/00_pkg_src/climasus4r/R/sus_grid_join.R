# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c("code_muni", "date", ".data"))

# ── Exported function ─────────────────────────────────────────────────────────

#' Join Gridded Environmental Data to Health Data
#'
#' @description
#' `sus_grid_join()` merges the municipality × date output of any
#' `sus_grid_*()` function (ERA5, CHIRPS, GHAP, CAMS, fires, PRODES, …) with
#' a health `climasus_df` produced by [sus_spatial_join()] or
#' [sus_data_aggregate()]. The result is a single `climasus_df` at
#' `stage = "climate"` ready for [sus_mod_dlnm()], [sus_climate_anomaly()],
#' and [sus_mod_vulnerability_index()].
#'
#' This function is the bridge between the gridded environmental pipeline
#' (`sus_grid_*`) and the health pipeline (`sus_data_import` → … →
#' `sus_spatial_join`), equivalent to the role [sus_climate_aggregate()] plays
#' for station-based INMET data.
#'
#' @param health_data A `climasus_df` at stage `"spatial"`, `"aggregate"`, or
#'   `"census"`. Must contain `code_muni` (character) and `date` (Date) columns.
#'
#' @param grid_data A `climasus_df` at stage `"climate"` produced by any
#'   `sus_grid_*()` function. Must contain `code_muni` and `date` columns and
#'   at least one environmental variable column.
#'
#' @param by Character vector. Join key columns. Default `c("code_muni", "date")`.
#'   Change to `c("code_muni")` if joining annual grid data (e.g., PRODES) to
#'   monthly health data — the join will broadcast the annual value to all months.
#'
#' @param type_out Character. Overrides `sus_meta$type` in the result.
#'   `NULL` (default) inherits the type from `grid_data`.
#'
#' @param lang Character. Message language: `"pt"` (default), `"en"`, `"es"`.
#'
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return A `climasus_df` with all columns from `health_data` plus the
#'   environmental variable columns from `grid_data`. Metadata:
#'   `stage = "climate"`, `type` inherited from `grid_data` (or `type_out`).
#'   Rows from `health_data` with no matching grid row receive `NA` for grid
#'   columns.
#'
#' @section Temporal granularity mismatch:
#' If `health_data` has monthly dates (`YYYY-MM-01`) and `grid_data` has daily
#' dates, the join will produce `NA`s for most rows. In that case, first
#' aggregate `grid_data` to monthly resolution with
#' [sus_climate_aggregate()] or by passing `by = c("code_muni")` (annual
#' grid data only) or summarising manually before calling `sus_grid_join()`.
#'
#' @examples
#' \dontrun{
#' library(geobr)
#' mt_mun <- read_municipality(code_muni = "MT", year = 2020)
#'
#' # Health data (SIH respiratory hospitalisations)
#' sih_agg <- sus_data_import(system = "SIH", years = 2020, uf = "MT") |>
#'   sus_data_clean_encoding() |>
#'   sus_data_standardize() |>
#'   sus_data_filter_cid(cid_group = "respiratorio") |>
#'   sus_data_aggregate() |>
#'   sus_spatial_join(municipalities = mt_mun)
#'
#' # Gridded climate data
#' era5 <- sus_grid_era5(
#'   years          = 2020,
#'   municipalities = mt_mun,
#'   vars           = c("t2m", "tp")
#' )
#'
#' # Join: one step to connect health + climate
#' combined <- sus_grid_join(sih_agg, era5)
#' sus_meta(combined, "stage")  # "climate"
#' sus_meta(combined, "type")   # "era5_land"
#'
#' # Annual deforestation joined to monthly health data (no date in by)
#' prodes <- sus_grid_prodes(years = 2020, biomes = "Amazon",
#'                            uf = "MT", municipalities = mt_mun)
#' combined2 <- sus_grid_join(sih_agg, prodes, by = "code_muni")
#' }
#'
#' @seealso [sus_grid_era5()], [sus_grid_chirps()], [sus_grid_fires()],
#'   [sus_grid_prodes()], [sus_climate_aggregate()], [sus_mod_dlnm()]
#'
#' @export
#' @importFrom dplyr left_join all_of n_distinct
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom tibble as_tibble
#' @importFrom rlang .data
sus_grid_join <- function(
    health_data,
    grid_data,
    by       = c("code_muni", "date"),
    type_out = NULL,
    lang     = "pt",
    verbose  = TRUE) {

  # ── 1. Validate lang ─────────────────────────────────────────────────────────
  if (!is.character(lang) || length(lang) != 1 || !lang %in% c("pt", "en", "es")) {
    cli::cli_abort("{.arg lang} must be 'pt', 'en', or 'es'.")
  }
  msg <- .grid_join_msgs[[lang]]

  # ── 2. Validate health_data ───────────────────────────────────────────────────
  if (!inherits(health_data, "climasus_df")) {
    cli::cli_abort(c(
      msg$health_not_climasus,
      "i" = msg$health_hint
    ))
  }
  health_stage <- sus_meta(health_data, "stage")
  valid_health_stages <- c("spatial", "aggregate", "census")
  if (is.null(health_stage) || !health_stage %in% valid_health_stages) {
    stage <- health_stage %||% "NULL"
    valid <- paste(valid_health_stages, collapse = ", ")
    cli::cli_abort(c(msg$health_wrong_stage, "i" = msg$health_stage_hint))
  }

  # ── 3. Validate grid_data ─────────────────────────────────────────────────────
  if (!inherits(grid_data, "climasus_df")) {
    cli::cli_abort(c(msg$grid_not_climasus, "i" = msg$grid_hint))
  }
  grid_stage <- sus_meta(grid_data, "stage")
  if (is.null(grid_stage) || grid_stage != "climate") {
    stage <- grid_stage %||% "NULL"
    cli::cli_abort(c(msg$grid_wrong_stage, "i" = msg$grid_stage_hint))
  }

  # ── 4. Validate join keys ────────────────────────────────────────────────────
  if (!is.character(by) || length(by) == 0) {
    cli::cli_abort(msg$invalid_by)
  }
  missing_h <- setdiff(by, names(health_data))
  missing_g <- setdiff(by, names(grid_data))
  if (length(missing_h) > 0) {
    cols <- paste(missing_h, collapse = ", ")
    cli::cli_abort(msg$missing_by_health)
  }
  if (length(missing_g) > 0) {
    cols <- paste(missing_g, collapse = ", ")
    cli::cli_abort(msg$missing_by_grid)
  }

  # ── 5. Detect grid-only columns (what will be added) ─────────────────────────
  grid_cols <- setdiff(names(grid_data), by)
  # Drop sf geometry column if present
  grid_cols <- grid_cols[!grid_cols %in% c("geometry", "geom")]

  if (length(grid_cols) == 0) {
    cli::cli_abort(msg$no_grid_cols)
  }

  # ── 6. Warn about column name collisions ─────────────────────────────────────
  collisions <- intersect(grid_cols, setdiff(names(health_data), by))
  if (length(collisions) > 0) {
    cli::cli_alert_warning(
      glue::glue(msg$column_collision,
                 cols = paste(collisions, collapse = ", ")))
  }

  # ── 7. Temporal granularity check (only when "date" is in by) ───────────────
  if ("date" %in% by) {
    health_dates <- sort(unique(as.Date(health_data$date)))
    grid_dates   <- sort(unique(as.Date(grid_data$date)))

    # Detect if granularity mismatch (e.g., health monthly, grid daily)
    if (length(grid_dates) > 0 && length(health_dates) > 0) {
      pct_match <- mean(grid_dates %in% health_dates)
      if (pct_match < 0.5 && length(grid_dates) > length(health_dates)) {
        cli::cli_alert_warning(glue::glue(
          msg$temporal_mismatch,
          n_health = length(health_dates),
          n_grid   = length(grid_dates),
          pct      = round(pct_match * 100)
        ))
      }
    }
  }

  # ── 8. Log start ─────────────────────────────────────────────────────────────
  if (verbose) {
    cli::cli_h1(msg$title)
    cli::cli_alert_info(glue::glue(
      msg$join_start,
      n_health   = nrow(health_data),
      n_grid_col = length(grid_cols),
      grid_cols  = paste(grid_cols, collapse = ", ")
    ))
  }

  # ── 9. Materialise both to plain tibbles before joining ───────────────────────
  # Drop sf/sfc geometry columns: handle both sf-class objects and plain
  # data frames that carry a geometry column from a previous spatial join.
  geom_cols <- character(0)
  sf_col_attr <- attr(health_data, "sf_column")
  if (!is.null(sf_col_attr) && sf_col_attr %in% names(health_data)) {
    geom_cols <- sf_col_attr
  } else {
    # Detect any sfc columns (geometry stored without sf class)
    is_sfc <- vapply(names(health_data),
                     function(nm) inherits(health_data[[nm]], "sfc"),
                     logical(1L))
    geom_cols <- names(is_sfc)[is_sfc]
  }
  if (inherits(health_data, "sf") && requireNamespace("sf", quietly = TRUE)) {
    health_tbl <- tibble::as_tibble(sf::st_drop_geometry(health_data))
  } else if (length(geom_cols) > 0L) {
    health_tbl <- tibble::as_tibble(
      health_data[, setdiff(names(health_data), geom_cols), drop = FALSE])
  } else {
    health_tbl <- tibble::as_tibble(health_data)
  }
  grid_tbl   <- tibble::as_tibble(
    grid_data[, c(by, grid_cols), drop = FALSE])

  # ── 10. Left join ──────────────────────────────────────────────────────────────
  result <- dplyr::left_join(
    health_tbl,
    grid_tbl,
    by = dplyr::all_of(by)
  )

  # ── 11. Update sus_meta ───────────────────────────────────────────────────────
  grid_type <- sus_meta(grid_data, "type")
  out_type  <- type_out %||% grid_type %||% "grid"

  # Preserve health_data's sus_meta and update key fields
  base_meta           <- sus_meta(health_data)
  base_meta$stage     <- "climate"
  base_meta$type      <- out_type
  base_meta$modified  <- Sys.time()
  base_meta$history   <- c(
    base_meta$history %||% character(0),
    sprintf("[%s] sus_grid_join(): %d col(s) added (%s), join by: %s",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            length(grid_cols),
            paste(grid_cols, collapse = ", "),
            paste(by, collapse = ", "))
  )

  base_classes <- setdiff(class(result), "climasus_df")
  result <- structure(
    tibble::as_tibble(result),
    sus_meta = base_meta,
    class    = c("climasus_df", base_classes)
  )

  n_rows <- nrow(result)
  if (verbose) {
    cli::cli_alert_success(glue::glue(
      msg$join_done,
      n_rows  = n_rows,
      n_na    = sum(is.na(result[[grid_cols[1]]])),
      out_col = grid_cols[1]
    ))
  }

  result
}


# ── Internal messages ─────────────────────────────────────────────────────────

#' Multilingual messages for sus_grid_join()
#' @keywords internal
#' @noRd
.grid_join_msgs <- list(
  pt = list(
    title               = "sus_grid_join: Unindo Dados Gridded ao Pipeline de Sa\u00fade",
    health_not_climasus = "{.arg health_data} deve ser um {.cls climasus_df}.",
    health_hint         = "Use {.fn sus_spatial_join} ou {.fn sus_data_aggregate} para criar o objeto.",
    health_wrong_stage  = "{.arg health_data} est\u00e1 em stage '{stage}'; esperado: spatial, aggregate ou census.",
    health_stage_hint   = "Stages v\u00e1lidos: {valid}.",
    grid_not_climasus   = "{.arg grid_data} deve ser um {.cls climasus_df}.",
    grid_hint           = "Use {.fn sus_grid_era5}, {.fn sus_grid_chirps} ou outra fun\u00e7\u00e3o sus_grid_* para criar o objeto.",
    grid_wrong_stage    = "{.arg grid_data} est\u00e1 em stage '{stage}'; esperado: climate.",
    grid_stage_hint     = "Execute uma fun\u00e7\u00e3o sus_grid_*() para obter dados em stage='climate'.",
    invalid_by          = "{.arg by} deve ser um vetor de caracteres n\u00e3o vazio.",
    missing_by_health   = "Coluna(s) de join ausente em {.arg health_data}: {cols}.",
    missing_by_grid     = "Coluna(s) de join ausente em {.arg grid_data}: {cols}.",
    no_grid_cols        = "{.arg grid_data} n\u00e3o tem colunas al\u00e9m das chaves de join.",
    column_collision    = "Aten\u00e7\u00e3o: coluna(s) '{cols}' existem em ambos os objetos e ser\u00e3o sobrescritas.",
    temporal_mismatch   = "Aviso temporal: health_data tem {n_health} datas \u00fanicas, grid_data tem {n_grid} ({pct}% de match). Considere agregar o clima antes do join.",
    join_start          = "Unindo {n_health} linhas com {n_grid_col} coluna(s) gridded: {grid_cols}",
    join_done           = "Conclu\u00eddo: {n_rows} linhas; {n_na} NA(s) em '{out_col}'."
  ),
  en = list(
    title               = "sus_grid_join: Joining Gridded Data to the Health Pipeline",
    health_not_climasus = "{.arg health_data} must be a {.cls climasus_df}.",
    health_hint         = "Use {.fn sus_spatial_join} or {.fn sus_data_aggregate} to create the object.",
    health_wrong_stage  = "{.arg health_data} is at stage '{stage}'; expected: spatial, aggregate, or census.",
    health_stage_hint   = "Valid stages: {valid}.",
    grid_not_climasus   = "{.arg grid_data} must be a {.cls climasus_df}.",
    grid_hint           = "Use {.fn sus_grid_era5}, {.fn sus_grid_chirps}, or another sus_grid_*() function.",
    grid_wrong_stage    = "{.arg grid_data} is at stage '{stage}'; expected: climate.",
    grid_stage_hint     = "Run a sus_grid_*() function to get data at stage='climate'.",
    invalid_by          = "{.arg by} must be a non-empty character vector.",
    missing_by_health   = "Join column(s) missing from {.arg health_data}: {cols}.",
    missing_by_grid     = "Join column(s) missing from {.arg grid_data}: {cols}.",
    no_grid_cols        = "{.arg grid_data} has no columns beyond the join keys.",
    column_collision    = "Warning: column(s) '{cols}' exist in both objects and will be overwritten.",
    temporal_mismatch   = "Temporal warning: health_data has {n_health} unique dates, grid_data has {n_grid} ({pct}% match). Consider aggregating the climate data before joining.",
    join_start          = "Joining {n_health} rows with {n_grid_col} gridded column(s): {grid_cols}",
    join_done           = "Complete: {n_rows} rows; {n_na} NA(s) in '{out_col}'."
  ),
  es = list(
    title               = "sus_grid_join: Uniendo Datos Gridded al Pipeline de Salud",
    health_not_climasus = "{.arg health_data} debe ser un {.cls climasus_df}.",
    health_hint         = "Use {.fn sus_spatial_join} o {.fn sus_data_aggregate} para crear el objeto.",
    health_wrong_stage  = "{.arg health_data} est\u00e1 en stage '{stage}'; se esperaba: spatial, aggregate o census.",
    health_stage_hint   = "Stages v\u00e1lidos: {valid}.",
    grid_not_climasus   = "{.arg grid_data} debe ser un {.cls climasus_df}.",
    grid_hint           = "Use {.fn sus_grid_era5}, {.fn sus_grid_chirps} u otra funci\u00f3n sus_grid_*().",
    grid_wrong_stage    = "{.arg grid_data} est\u00e1 en stage '{stage}'; se esperaba: climate.",
    grid_stage_hint     = "Ejecute una funci\u00f3n sus_grid_*() para obtener datos en stage='climate'.",
    invalid_by          = "{.arg by} debe ser un vector de caracteres no vac\u00edo.",
    missing_by_health   = "Columna(s) de join ausentes en {.arg health_data}: {cols}.",
    missing_by_grid     = "Columna(s) de join ausentes en {.arg grid_data}: {cols}.",
    no_grid_cols        = "{.arg grid_data} no tiene columnas m\u00e1s all\u00e1 de las claves de join.",
    column_collision    = "Advertencia: la(s) columna(s) '{cols}' existen en ambos objetos y ser\u00e1n sobreescritas.",
    temporal_mismatch   = "Advertencia temporal: health_data tiene {n_health} fechas \u00fanicas, grid_data tiene {n_grid} ({pct}% de coincidencia). Considere agregar el clima antes del join.",
    join_start          = "Uniendo {n_health} filas con {n_grid_col} columna(s) gridded: {grid_cols}",
    join_done           = "Completo: {n_rows} filas; {n_na} NA(s) en '{out_col}'."
  )
)
