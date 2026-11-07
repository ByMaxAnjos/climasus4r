# =============================================================================
# Internal indicators catalog
# =============================================================================
# Each entry: name_pt/en/es, category, formula (raw ratio/proportion \u2014 no
# multiplier baked in), required_cols, multiplier, unit,
# uncertainty_method ("none"|"poisson"|"binomial"),
# numerator_col / denominator_col (formula-space names, for CI only).
# =============================================================================
.sus_indicators_catalog <- list(

  # ---------------------------------------------------------------------------
  # Demographic
  # ---------------------------------------------------------------------------
  dependency_ratio = list(
    name_pt = "Razao de Dependencia",
    name_en = "Dependency Ratio",
    name_es = "Razon de Dependencia",
    category = "demographic",
    formula = "(pop_young + pop_elderly) / pop_working",
    required_cols = c("pop_young", "pop_elderly", "pop_working"),
    multiplier = 100, unit = "%",
    uncertainty_method = "none",
    numerator_col = NULL, denominator_col = NULL,
    source = "census"
  ),
  aging_index = list(
    name_pt = "Indice de Envelhecimento",
    name_en = "Aging Index",
    name_es = "Indice de Envejecimiento",
    category = "demographic",
    formula = "pop_elderly / pop_young",
    required_cols = c("pop_elderly", "pop_young"),
    multiplier = 100, unit = "%",
    uncertainty_method = "none",
    numerator_col = NULL, denominator_col = NULL,
    source = "census"
  ),
  urbanization_rate = list(
    name_pt = "Taxa de Urbanizacao",
    name_en = "Urbanization Rate",
    name_es = "Tasa de Urbanizacion",
    category = "demographic",
    formula = "pop_urban / pop_total",
    required_cols = c("pop_urban", "pop_total"),
    multiplier = 100, unit = "%",
    uncertainty_method = "binomial",
    numerator_col = "pop_urban", denominator_col = "pop_total",
    source = "census"
  ),

  # ---------------------------------------------------------------------------
  # Socioeconomic
  # ---------------------------------------------------------------------------
  illiteracy_rate = list(
    name_pt = "Taxa de Analfabetismo",
    name_en = "Illiteracy Rate",
    name_es = "Tasa de Analfabetismo",
    category = "socioeconomic",
    formula = "pop_illiterate / pop_15_plus",
    required_cols = c("pop_illiterate", "pop_15_plus"),
    multiplier = 100, unit = "%",
    uncertainty_method = "binomial",
    numerator_col = "pop_illiterate", denominator_col = "pop_15_plus",
    source = "census"
  ),
  water_connection_rate = list(
    name_pt = "Cobertura de Agua Encanada",
    name_en = "Water Connection Rate",
    name_es = "Cobertura de Agua Entubada",
    category = "socioeconomic",
    formula = "hh_water / total_hh",
    required_cols = c("hh_water", "total_hh"),
    multiplier = 100, unit = "%",
    uncertainty_method = "binomial",
    numerator_col = "hh_water", denominator_col = "total_hh",
    source = "census"
  ),
  sewage_connection_rate = list(
    name_pt = "Cobertura de Esgoto",
    name_en = "Sewage Connection Rate",
    name_es = "Cobertura de Alcantarillado",
    category = "socioeconomic",
    formula = "hh_sewage / total_hh",
    required_cols = c("hh_sewage", "total_hh"),
    multiplier = 100, unit = "%",
    uncertainty_method = "binomial",
    numerator_col = "hh_sewage", denominator_col = "total_hh",
    source = "census"
  ),
  gini_index = list(
    name_pt = "Indice de Gini",
    name_en = "Gini Index",
    name_es = "Indice de Gini",
    category = "socioeconomic",
    formula = "gini_value",
    required_cols = c("gini_value"),
    multiplier = 1, unit = "index (0-1)",
    uncertainty_method = "none",
    numerator_col = NULL, denominator_col = NULL,
    source = "census"
  ),

  # ---------------------------------------------------------------------------
  # Mortality
  # ---------------------------------------------------------------------------
  infant_mortality_rate = list(
    name_pt = "Taxa de Mortalidade Infantil",
    name_en = "Infant Mortality Rate",
    name_es = "Tasa de Mortalidad Infantil",
    category = "mortality",
    formula = "deaths_infant / live_births",
    required_cols = c("deaths_infant", "live_births"),
    multiplier = 1000, unit = "per 1,000 live births",
    uncertainty_method = "poisson",
    numerator_col = "deaths_infant", denominator_col = "live_births",
    source = "sim_sinasc"
  ),
  maternal_mortality_ratio = list(
    name_pt = "Razao de Mortalidade Materna",
    name_en = "Maternal Mortality Ratio",
    name_es = "Razon de Mortalidad Materna",
    category = "mortality",
    formula = "deaths_maternal / live_births",
    required_cols = c("deaths_maternal", "live_births"),
    multiplier = 100000, unit = "per 100,000 live births",
    uncertainty_method = "poisson",
    numerator_col = "deaths_maternal", denominator_col = "live_births",
    source = "sim_sinasc"
  ),
  premature_dcn_mortality = list(
    name_pt = "Mortalidade Prematura por DCNT",
    name_en = "Premature NCD Mortality Rate",
    name_es = "Mortalidad Prematura por ECNT",
    category = "mortality",
    formula = "deaths_dcn_30_69 / pop_30_69",
    required_cols = c("deaths_dcn_30_69", "pop_30_69"),
    multiplier = 100000, unit = "per 100,000 (30-69 years)",
    uncertainty_method = "poisson",
    numerator_col = "deaths_dcn_30_69", denominator_col = "pop_30_69",
    source = "sim"
  ),
  homicide_rate = list(
    name_pt = "Taxa de Homicidios",
    name_en = "Homicide Rate",
    name_es = "Tasa de Homicidios",
    category = "mortality",
    formula = "deaths_homicide / pop_total",
    required_cols = c("deaths_homicide", "pop_total"),
    multiplier = 100000, unit = "per 100,000",
    uncertainty_method = "poisson",
    numerator_col = "deaths_homicide", denominator_col = "pop_total",
    source = "sim"
  ),
  traffic_mortality_rate = list(
    name_pt = "Taxa de Mortalidade por Transito",
    name_en = "Traffic Mortality Rate",
    name_es = "Tasa de Mortalidad por Transito",
    category = "mortality",
    formula = "deaths_traffic / pop_total",
    required_cols = c("deaths_traffic", "pop_total"),
    multiplier = 100000, unit = "per 100,000",
    uncertainty_method = "poisson",
    numerator_col = "deaths_traffic", denominator_col = "pop_total",
    source = "sim"
  ),

  # ---------------------------------------------------------------------------
  # Morbidity
  # ---------------------------------------------------------------------------
  arbovirus_incidence_rate = list(
    name_pt = "Taxa de Incidencia de Arboviroses",
    name_en = "Arbovirus Incidence Rate",
    name_es = "Tasa de Incidencia de Arbovirus",
    category = "morbidity",
    formula = "cases_arbovirus / pop_total",
    required_cols = c("cases_arbovirus", "pop_total"),
    multiplier = 100000, unit = "per 100,000",
    uncertainty_method = "poisson",
    numerator_col = "cases_arbovirus", denominator_col = "pop_total",
    source = "sinan"
  ),
  tb_incidence_rate = list(
    name_pt = "Taxa de Incidencia de Tuberculose",
    name_en = "TB Incidence Rate",
    name_es = "Tasa de Incidencia de Tuberculosis",
    category = "morbidity",
    formula = "cases_tb_new / pop_total",
    required_cols = c("cases_tb_new", "pop_total"),
    multiplier = 100000, unit = "per 100,000",
    uncertainty_method = "poisson",
    numerator_col = "cases_tb_new", denominator_col = "pop_total",
    source = "sinan"
  ),
  icsap_hospitalization_rate = list(
    name_pt = "Taxa de Internacoes por ICSAP",
    name_en = "ACSC Hospitalization Rate",
    name_es = "Tasa de Hospitalizaciones por ICSAP",
    category = "morbidity",
    formula = "hosp_icsap / pop_total",
    required_cols = c("hosp_icsap", "pop_total"),
    multiplier = 10000, unit = "per 10,000",
    uncertainty_method = "poisson",
    numerator_col = "hosp_icsap", denominator_col = "pop_total",
    source = "sih"
  ),
  respiratory_hospitalization_rate = list(
    name_pt = "Taxa de Internacoes Respiratorias",
    name_en = "Respiratory Hospitalization Rate",
    name_es = "Tasa de Hospitalizaciones Respiratorias",
    category = "morbidity",
    formula = "hosp_resp / pop_total",
    required_cols = c("hosp_resp", "pop_total"),
    multiplier = 10000, unit = "per 10,000",
    uncertainty_method = "poisson",
    numerator_col = "hosp_resp", denominator_col = "pop_total",
    source = "sih"
  ),

  # ---------------------------------------------------------------------------
  # Maternal-child health
  # ---------------------------------------------------------------------------
  low_birth_weight_proportion = list(
    name_pt = "Proporcao de Baixo Peso ao Nascer",
    name_en = "Low Birth Weight Proportion",
    name_es = "Proporcion de Bajo Peso al Nacer",
    category = "maternal_child",
    formula = "births_low_weight / live_births",
    required_cols = c("births_low_weight", "live_births"),
    multiplier = 100, unit = "%",
    uncertainty_method = "binomial",
    numerator_col = "births_low_weight", denominator_col = "live_births",
    source = "sinasc"
  ),
  general_fertility_rate = list(
    name_pt = "Taxa de Fecundidade Geral",
    name_en = "General Fertility Rate",
    name_es = "Tasa de Fecundidad General",
    category = "maternal_child",
    formula = "live_births / pop_women_15_49",
    required_cols = c("live_births", "pop_women_15_49"),
    multiplier = 1000, unit = "per 1,000 women (15-49 years)",
    uncertainty_method = "poisson",
    numerator_col = "live_births", denominator_col = "pop_women_15_49",
    source = "sinasc"
  ),
  cesarean_proportion = list(
    name_pt = "Proporcao de Partos Cesareos",
    name_en = "Cesarean Delivery Proportion",
    name_es = "Proporcion de Partos por Cesarea",
    category = "maternal_child",
    formula = "deliveries_cesarean / total_deliveries",
    required_cols = c("deliveries_cesarean", "total_deliveries"),
    multiplier = 100, unit = "%",
    uncertainty_method = "binomial",
    numerator_col = "deliveries_cesarean", denominator_col = "total_deliveries",
    source = "sinasc"
  ),
  prenatal_early_coverage = list(
    name_pt = "Cobertura de Pre-natal Precoce",
    name_en = "Early Prenatal Care Coverage",
    name_es = "Cobertura de Control Prenatal Precoz",
    category = "maternal_child",
    formula = "prenatal_early / live_births",
    required_cols = c("prenatal_early", "live_births"),
    multiplier = 100, unit = "%",
    uncertainty_method = "binomial",
    numerator_col = "prenatal_early", denominator_col = "live_births",
    source = "sinasc"
  ),

  # ---------------------------------------------------------------------------
  # Health resources
  # ---------------------------------------------------------------------------
  beds_per_capita = list(
    name_pt = "Leitos por Mil Habitantes",
    name_en = "Hospital Beds per 1,000",
    name_es = "Camas por 1.000 Habitantes",
    category = "health_resources",
    formula = "cnes_beds / pop_total",
    required_cols = c("cnes_beds", "pop_total"),
    multiplier = 1000, unit = "per 1,000",
    uncertainty_method = "none",
    numerator_col = NULL, denominator_col = NULL,
    source = "cnes"
  ),
  doctors_per_capita = list(
    name_pt = "Medicos por Mil Habitantes",
    name_en = "Doctors per 1,000",
    name_es = "Medicos por 1.000 Habitantes",
    category = "health_resources",
    formula = "cnes_doctors / pop_total",
    required_cols = c("cnes_doctors", "pop_total"),
    multiplier = 1000, unit = "per 1,000",
    uncertainty_method = "none",
    numerator_col = NULL, denominator_col = NULL,
    source = "cnes"
  ),

  # ---------------------------------------------------------------------------
  # Heat vulnerability (census tract-level proportions)
  # ---------------------------------------------------------------------------
  pct_age_0_4 = list(
    name_pt = "Percentual de Criancas de 0 a 4 Anos",
    name_en = "Share of Children Aged 0-4",
    name_es = "Porcentaje de Ninos de 0 a 4 Anos",
    category = "heat_vulnerability",
    formula = "pop_0_4 / pop_total",
    required_cols = c("pop_0_4", "pop_total"),
    multiplier = 100, unit = "%",
    uncertainty_method = "binomial",
    numerator_col = "pop_0_4", denominator_col = "pop_total",
    source = "census"
  ),
  pct_age_60_plus = list(
    name_pt = "Percentual de Pessoas com 60 Anos ou Mais",
    name_en = "Share of People Aged 60+",
    name_es = "Porcentaje de Personas de 60 Anos o Mas",
    category = "heat_vulnerability",
    formula = "pop_60_plus / pop_total",
    required_cols = c("pop_60_plus", "pop_total"),
    multiplier = 100, unit = "%",
    uncertainty_method = "binomial",
    numerator_col = "pop_60_plus", denominator_col = "pop_total",
    source = "census"
  ),
  single_person_hh_rate = list(
    name_pt = "Taxa de Domicilios Unipessoais",
    name_en = "Single-Person Household Rate",
    name_es = "Tasa de Hogares Unipersonales",
    category = "heat_vulnerability",
    formula = "hh_single / total_hh",
    required_cols = c("hh_single", "total_hh"),
    multiplier = 100, unit = "%",
    uncertainty_method = "binomial",
    numerator_col = "hh_single", denominator_col = "total_hh",
    source = "census"
  ),
  improvised_hh_rate = list(
    name_pt = "Taxa de Domicilios Improvisados",
    name_en = "Improvised Household Rate",
    name_es = "Tasa de Viviendas Improvisadas",
    category = "heat_vulnerability",
    formula = "hh_improvised / total_hh",
    required_cols = c("hh_improvised", "total_hh"),
    multiplier = 100, unit = "%",
    uncertainty_method = "binomial",
    numerator_col = "hh_improvised", denominator_col = "total_hh",
    source = "census"
  ),
  garbage_collection_rate = list(
    name_pt = "Taxa de Domicilios com Coleta de Lixo",
    name_en = "Households with Garbage Collection Rate",
    name_es = "Tasa de Hogares con Recoleccion de Basura",
    category = "heat_vulnerability",
    formula = "hh_garbage / total_hh",
    required_cols = c("hh_garbage", "total_hh"),
    multiplier = 100, unit = "%",
    uncertainty_method = "binomial",
    numerator_col = "hh_garbage", denominator_col = "total_hh",
    source = "census"
  ),
  treeless_hh_rate = list(
    name_pt = "Taxa de Domicilios sem Arborizacao no Entorno",
    name_en = "Households without Street Trees Rate",
    name_es = "Tasa de Hogares sin Arborizacion en el Entorno",
    category = "heat_vulnerability",
    formula = "hh_no_tree / hh_surveyed",
    required_cols = c("hh_no_tree", "hh_surveyed"),
    multiplier = 100, unit = "%",
    uncertainty_method = "binomial",
    numerator_col = "hh_no_tree", denominator_col = "hh_surveyed",
    source = "census"
  ),
  street_lighting_rate = list(
    name_pt = "Taxa de Domicilios com Iluminacao Publica",
    name_en = "Households with Street Lighting Rate",
    name_es = "Tasa de Hogares con Alumbrado Publico",
    category = "heat_vulnerability",
    formula = "hh_lighting / hh_surveyed",
    required_cols = c("hh_lighting", "hh_surveyed"),
    multiplier = 100, unit = "%",
    uncertainty_method = "binomial",
    numerator_col = "hh_lighting", denominator_col = "hh_surveyed",
    source = "census"
  ),
  sidewalk_rate = list(
    name_pt = "Taxa de Domicilios com Calcada",
    name_en = "Households with Sidewalk Rate",
    name_es = "Tasa de Hogares con Acera",
    category = "heat_vulnerability",
    formula = "hh_sidewalk / hh_surveyed",
    required_cols = c("hh_sidewalk", "hh_surveyed"),
    multiplier = 100, unit = "%",
    uncertainty_method = "binomial",
    numerator_col = "hh_sidewalk", denominator_col = "hh_surveyed",
    source = "census"
  ),
  paved_street_rate = list(
    name_pt = "Taxa de Domicilios com Rua Pavimentada",
    name_en = "Households on Paved Street Rate",
    name_es = "Tasa de Hogares en Calle Pavimentada",
    category = "heat_vulnerability",
    formula = "hh_paved / hh_surveyed",
    required_cols = c("hh_paved", "hh_surveyed"),
    multiplier = 100, unit = "%",
    uncertainty_method = "binomial",
    numerator_col = "hh_paved", denominator_col = "hh_surveyed",
    source = "census"
  ),
  black_brown_pop_rate = list(
    name_pt = "Percentual de Populacao Preta e Parda",
    name_en = "Share of Black and Brown Population",
    name_es = "Porcentaje de Poblacion Negra y Parda",
    category = "heat_vulnerability",
    formula = "pop_black_brown / pop_total",
    required_cols = c("pop_black_brown", "pop_total"),
    multiplier = 100, unit = "%",
    uncertainty_method = "binomial",
    numerator_col = "pop_black_brown", denominator_col = "pop_total",
    source = "census"
  )
)


# =============================================================================
# Internal helpers
# =============================================================================

# Resolve formula-space column names to actual df column names via col_mapping.
# Returns a named character vector: names = formula names, values = actual cols.
# Also returns a character vector of missing actual columns.
.resolve_indicator_cols <- function(required_cols, col_mapping, df_cols) {
  resolved <- stats::setNames(required_cols, required_cols)
  for (fn in names(col_mapping)) {
    if (fn %in% required_cols) {
      resolved[[fn]] <- col_mapping[[fn]]
    }
  }
  missing <- resolved[!unname(resolved) %in% df_cols]
  list(resolved = resolved, missing = missing)
}

# Return a character vector of indicator IDs that can be computed given
# the available column names (after applying col_mapping).
.available_indicators <- function(df_cols, col_mapping) {
  Filter(
    function(id) {
      spec <- .sus_indicators_catalog[[id]]
      res  <- .resolve_indicator_cols(spec$required_cols, col_mapping, df_cols)
      length(res$missing) == 0L
    },
    names(.sus_indicators_catalog)
  )
}

# Poisson Garwood exact CI (vectorized).
.poisson_ci <- function(k, n, conf_level, multiplier) {
  alpha <- 1 - conf_level
  k     <- as.numeric(k)
  n     <- as.numeric(n)
  valid <- !is.na(k) & !is.na(n) & n > 0
  low   <- rep(NA_real_, length(k))
  high  <- rep(NA_real_, length(k))
  low[valid]  <- ifelse(
    k[valid] == 0, 0,
    stats::qgamma(alpha / 2, shape = k[valid], rate = 1)
  ) / n[valid] * multiplier
  high[valid] <- stats::qgamma(
    1 - alpha / 2, shape = k[valid] + 1, rate = 1
  ) / n[valid] * multiplier
  list(low = low, high = high)
}

# Binomial Wilson score CI (vectorized).  Input p = x/n (0-1 raw proportion).
.binomial_ci <- function(n, p, conf_level, multiplier) {
  z     <- stats::qnorm(1 - (1 - conf_level) / 2)
  n     <- as.numeric(n)
  p     <- as.numeric(p)
  valid <- !is.na(n) & !is.na(p) & n > 0 & p >= 0 & p <= 1
  low   <- rep(NA_real_, length(n))
  high  <- rep(NA_real_, length(n))
  z2n   <- z^2 / n[valid]
  denom <- 1 + z2n
  ctr   <- (p[valid] + z2n / 2) / denom
  marg  <- z * sqrt(p[valid] * (1 - p[valid]) / n[valid] + z2n^2 / 4) / denom
  low[valid]  <- pmax(0, ctr - marg) * multiplier
  high[valid] <- pmin(1, ctr + marg) * multiplier
  list(low = low, high = high)
}


# =============================================================================
# Exported function
# =============================================================================

#' Compute Socioeconomic and Epidemiological Indicators
#'
#' @description
#' Computes standardised indicators (demographic, socioeconomic, mortality,
#' morbidity, maternal-child health, health resources, heat vulnerability) from
#' columns already present in a `climasus_df`. Run it after
#' [sus_census_join()] (or any step that provides the numerators and
#' denominators) and before modelling/mapping; it works with any DATASUS
#' `system`. Column names from the census step (e.g. `V003_sum`) are mapped to
#' the formula-space names each indicator expects through `col_mapping`. Use
#' [sus_socio_list_indicators()] to see the catalogue, formulas and required
#' columns.
#'
#' @details
#' **Formula evaluation**: each indicator formula is evaluated with
#' `rlang::eval_tidy()` against the resolved column values, multiplied by the
#' indicator's `multiplier` (e.g. 100 for %, 1000 per 1,000). Non-finite
#' results (zero denominators) become `NA`.
#'
#' **Missing columns**: a requested indicator whose required columns are absent
#' (after `col_mapping`) is skipped with a warning; the function errors only if
#' no indicator could be computed.
#'
#' **Confidence intervals**:
#' \itemize{
#'   \item *Poisson (Garwood exact)*: event-count / population-at-risk rates
#'     (mortality, incidence, hospitalisation).
#'   \item *Binomial (Wilson score)*: proportions (%, coverage indices).
#'   \item *None*: ratios with complex numerators or direct index values.
#' }
#'
#' @param df A `climasus_df` whose `sus_meta` stage is `"spatial"` or later,
#'   typically the output of [sus_census_join()]. Plain data frames and
#'   earlier stages error.
#' @param indicators Character vector of indicator IDs, e.g.
#'   `"dependency_ratio"` or `c("aging_index", "water_connection_rate")`.
#'   `NULL` (default) computes every indicator whose required columns are
#'   present (after `col_mapping`); errors if there are none. Unknown IDs give
#'   a warning and are dropped (error if none is valid). IDs: see
#'   [sus_socio_list_indicators()].
#' @param col_mapping Named list mapping formula-space variable names to actual
#'   column names of `df`, e.g.
#'   `list(pop_young = "pop_0_14_sum", total_hh = "V003_sum")`. Names not
#'   mapped are looked up in `df` under their own formula name. Non-list input
#'   errors. Default `list()`.
#' @param confidence_level Numeric scalar strictly between 0 and 1: confidence
#'   level of the Poisson/Binomial intervals (e.g. `0.95`, `0.99`). Other values
#'   error. Default `0.95`.
#' @param add_ci Logical scalar. If `TRUE` (default), adds `_low`/`_high`
#'   columns for indicators with a Poisson or Binomial uncertainty method.
#' @param lang Character scalar. Message language: `"pt"` (default), `"en"` or
#'   `"es"`; other values fall back to `"pt"` with a warning.
#' @param verbose Logical scalar. Print progress messages. Default `TRUE`.
#'
#' @return The input `climasus_df` with new numeric columns:
#'   \itemize{
#'     \item `ind_<id>` - computed value (scaled by the indicator multiplier)
#'     \item `ind_<id>_low` / `ind_<id>_high` - interval bounds (only for
#'       Poisson/Binomial indicators when `add_ci = TRUE`)
#'   }
#'   `sus_meta` is updated to `stage = "census"`, `type = "indicators"` (the
#'   `system` is kept) with a history entry.
#'
#' @examples
#' \dontrun{
#' # df_census: output of sus_census_join()
#' df_ind <- sus_socio_compute_indicators(
#'   df_census,
#'   indicators = c("dependency_ratio", "water_connection_rate"),
#'   col_mapping = list(
#'     pop_young   = "pop_0_14_sum",
#'     pop_elderly = "pop_65_plus_sum",
#'     pop_working = "pop_15_64_sum",
#'     hh_water    = "V111_sum",
#'     total_hh    = "V003_sum"
#'   )
#' )
#' }
#'
#' @seealso [sus_census_join()], [sus_census_select()],
#'   [sus_socio_list_indicators()]
#' @export
#' @importFrom rlang parse_expr eval_tidy
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_warn
#' @importFrom glue glue
sus_socio_compute_indicators <- function(
  df,
  indicators       = NULL,
  col_mapping      = list(),
  confidence_level = 0.95,
  add_ci           = TRUE,
  lang             = "pt",
  verbose          = TRUE
) {
  # --------------------------------------------------------------------------
  # 1. Validation
  # --------------------------------------------------------------------------
  if (verbose) cli::cli_h1("climasus4r - Compute Socioeconomic Indicators")

  if (!lang %in% c("pt", "en", "es")) {
    cli::cli_alert_warning("lang must be 'pt', 'en', or 'es'. Defaulting to 'pt'.")
    lang <- "pt"
  }

  msg <- .get_indicator_messages(lang)

  if (!is.data.frame(df)) cli::cli_abort(msg$error_df_type)

  if (!is.list(col_mapping)) {
    cli::cli_abort(msg$error_col_mapping_type)
  }

  if (!is.numeric(confidence_level) || confidence_level <= 0 || confidence_level >= 1) {
    cli::cli_abort(msg$error_conf_level)
  }

  # Stage check (requires at least "spatial")
  if (inherits(df, "climasus_df")) {
    current_stage  <- sus_meta(df, "stage")
    required_stage <- "spatial"
    if (!is_stage_at_least(current_stage, required_stage)) {
      cli::cli_abort(glue::glue(
        msg$error_stage,
        current  = current_stage %||% "unknown",
        required = required_stage
      ))
    }
  } else {
    cli::cli_abort(msg$error_not_climasus)
  }

  system <- sus_meta(df, "system")

  # --------------------------------------------------------------------------
  # 2. Resolve which indicators to compute
  # --------------------------------------------------------------------------
  df_cols <- names(df)

  if (is.null(indicators)) {
    indicators_to_compute <- .available_indicators(df_cols, col_mapping)
    if (length(indicators_to_compute) == 0L) {
      cli::cli_abort(msg$no_indicators)
    }
    if (verbose) {
      cli::cli_alert_info(glue::glue(
        msg$auto_detected,
        n = length(indicators_to_compute)
      ))
    }
  } else {
    unknown <- setdiff(indicators, names(.sus_indicators_catalog))
    if (length(unknown) > 0L) {
      cli::cli_warn(glue::glue(
        msg$unknown_indicators,
        ids = paste(unknown, collapse = ", ")
      ))
    }
    indicators_to_compute <- intersect(indicators, names(.sus_indicators_catalog))
    if (length(indicators_to_compute) == 0L) cli::cli_abort(msg$no_valid_indicators)
  }

  if (verbose) {
    cli::cli_alert_info(glue::glue(
      msg$computing,
      n = length(indicators_to_compute)
    ))
  }

  # --------------------------------------------------------------------------
  # 3. Compute each indicator
  # --------------------------------------------------------------------------
  n_computed <- 0L
  n_ci       <- 0L

  for (ind_id in indicators_to_compute) {
    spec <- .sus_indicators_catalog[[ind_id]]

    # Localised name for messages
    ind_name <- switch(lang,
      "en" = spec$name_en,
      "es" = spec$name_es,
      spec$name_pt
    )

    # Resolve columns
    res <- .resolve_indicator_cols(spec$required_cols, col_mapping, df_cols)

    if (length(res$missing) > 0L) {
      cli::cli_warn(glue::glue(
        msg$skipping_missing,
        id   = ind_id,
        cols = paste(unname(res$missing), collapse = ", ")
      ))
      next
    }

    # Build named list: formula variable name \u2192 actual column values
    env_list <- lapply(res$resolved, function(actual_col) df[[actual_col]])

    # Evaluate formula
    value <- tryCatch(
      rlang::eval_tidy(
        rlang::parse_expr(spec$formula),
        data = env_list
      ),
      error = function(e) {
        cli::cli_warn(glue::glue(
          msg$formula_error,
          id = ind_id,
          err = conditionMessage(e)
        ))
        NULL
      }
    )
    if (is.null(value)) next
    value[!is.finite(value)] <- NA_real_  # zero denominators -> NA, not Inf/NaN

    df[[paste0("ind_", ind_id)]] <- value * spec$multiplier
    n_computed <- n_computed + 1L

    # Confidence intervals
    if (add_ci && spec$uncertainty_method != "none") {
      num_vals   <- env_list[[spec$numerator_col]]
      denom_vals <- env_list[[spec$denominator_col]]

      if (spec$uncertainty_method == "poisson") {
        ci <- .poisson_ci(num_vals, denom_vals, confidence_level, spec$multiplier)
        df[[paste0("ind_", ind_id, "_low")]]  <- ci$low
        df[[paste0("ind_", ind_id, "_high")]] <- ci$high
        n_ci <- n_ci + 1L

      } else if (spec$uncertainty_method == "binomial") {
        # p is the raw (0-1) proportion; restore from scaled value
        p_raw <- value
        ci <- .binomial_ci(denom_vals, p_raw, confidence_level, spec$multiplier)
        df[[paste0("ind_", ind_id, "_low")]]  <- ci$low
        df[[paste0("ind_", ind_id, "_high")]] <- ci$high
        n_ci <- n_ci + 1L
      }
    }

    if (verbose) {
      cli::cli_alert_success(glue::glue(
        msg$indicator_done,
        name = ind_name,
        unit = spec$unit
      ))
    }
  }

  if (n_computed == 0L) cli::cli_abort(msg$nothing_computed)

  # --------------------------------------------------------------------------
  # 4. Update sus_meta
  # --------------------------------------------------------------------------
  df <- sus_meta(
    df,
    system     = system,
    stage      = "census",
    type       = "indicators",
    add_history = glue::glue(
      "[%s] Computed {n_computed} indicator(s): {paste(indicators_to_compute[seq_len(n_computed)], collapse = ', ')}"
    )
  )

  if (verbose) {
    cli::cli_alert_success(glue::glue(
      msg$success,
      n    = n_computed,
      n_ci = n_ci
    ))
  }

  df
}


#' List Available Indicators in the Catalogue
#'
#' Returns a data frame describing all indicators available in
#' `sus_socio_compute_indicators()`, including their IDs, names,
#' categories, required columns, and uncertainty method.
#'
#' @param lang Character scalar. Language of indicator names: `"pt"`
#'   (default), `"en"` or `"es"`; any other value silently falls back to `"pt"`.
#' @param category Character vector of categories to keep, e.g. `"mortality"`
#'   or `c("demographic", "socioeconomic")`. Valid values: `"demographic"`,
#'   `"socioeconomic"`, `"mortality"`, `"morbidity"`, `"maternal_child"`,
#'   `"health_resources"`, `"heat_vulnerability"`. `NULL` (default) returns all;
#'   unknown values return zero rows.
#'
#' @return A `tibble` with one row per indicator and columns `id` (value for
#'   `indicators` in [sus_socio_compute_indicators()]), `name`, `category`,
#'   `required_cols` (comma-separated formula-space names to map with
#'   `col_mapping`), `formula`, `multiplier`, `unit`, `uncertainty_method`
#'   (`"none"`, `"poisson"` or `"binomial"`), `source`.
#'
#' @seealso [sus_socio_compute_indicators()], [sus_census_join()]
#'
#' @examples
#' sus_socio_list_indicators(lang = "pt")
#' sus_socio_list_indicators(lang = "en", category = "mortality")
#'
#' @export
sus_socio_list_indicators <- function(lang = "pt", category = NULL) {
  rows <- lapply(names(.sus_indicators_catalog), function(id) {
    spec <- .sus_indicators_catalog[[id]]
    name <- switch(lang, "en" = spec$name_en, "es" = spec$name_es, spec$name_pt)
    list(
      id                 = id,
      name               = name,
      category           = spec$category,
      required_cols      = paste(spec$required_cols, collapse = ", "),
      formula            = spec$formula,
      multiplier         = spec$multiplier,
      unit               = spec$unit,
      uncertainty_method = spec$uncertainty_method,
      source             = spec$source
    )
  })

  out <- do.call(rbind.data.frame, c(rows, stringsAsFactors = FALSE))

  if (!is.null(category)) {
    out <- out[out$category %in% category, , drop = FALSE]
  }

  tibble::as_tibble(out)
}


# =============================================================================
# Internal: multilingual messages
# =============================================================================
.get_indicator_messages <- function(lang) {
  list(
    pt = list(
      error_df_type        = "O input 'df' deve ser um data.frame ou climasus_df.",
      error_col_mapping_type = "col_mapping deve ser uma lista nomeada.",
      error_conf_level     = "confidence_level deve ser um numero entre 0 e 1.",
      error_stage          = "Stage atual '{current}' insuficiente. Requerido: '{required}'. Execute sus_spatial_join() primeiro.",
      error_not_climasus   = "O input deve ser um objeto climasus_df. Execute o pipeline climasus4r primeiro.",
      no_indicators        = "Nenhum indicador disponivel com as colunas presentes. Use col_mapping para mapear suas colunas.",
      no_valid_indicators  = "Nenhum dos indicadores solicitados e valido. Use sus_socio_list_indicators() para ver os IDs dispon\u00edveis.",
      auto_detected        = "{n} indicadores detectados automaticamente com base nas colunas dispon\u00edveis.",
      computing            = "Calculando {n} indicadores...",
      unknown_indicators   = "IDs desconhecidos ignorados: {ids}",
      skipping_missing     = "Pulando '{id}': colunas ausentes: {cols}. Use col_mapping para mapear suas colunas.",
      formula_error        = "Erro ao avaliar formula do indicador '{id}': {err}",
      indicator_done       = "  {name} ({unit})",
      success              = "{n} indicadores calculados. {n_ci} com intervalos de confianca.",
      nothing_computed     = "Nenhum indicador foi calculado com sucesso."
    ),
    en = list(
      error_df_type        = "Input 'df' must be a data.frame or climasus_df.",
      error_col_mapping_type = "col_mapping must be a named list.",
      error_conf_level     = "confidence_level must be a number between 0 and 1.",
      error_stage          = "Current stage '{current}' is below required '{required}'. Run sus_spatial_join() first.",
      error_not_climasus   = "Input must be a climasus_df object. Run the climasus4r pipeline first.",
      no_indicators        = "No indicators available with the current columns. Use col_mapping to map your column names.",
      no_valid_indicators  = "None of the requested indicators are valid. Use sus_socio_list_indicators() to see available IDs.",
      auto_detected        = "{n} indicators auto-detected based on available columns.",
      computing            = "Computing {n} indicators...",
      unknown_indicators   = "Unknown indicator IDs skipped: {ids}",
      skipping_missing     = "Skipping '{id}': missing columns: {cols}. Use col_mapping to map your columns.",
      formula_error        = "Error evaluating formula for indicator '{id}': {err}",
      indicator_done       = "  {name} ({unit})",
      success              = "{n} indicator(s) computed. {n_ci} with confidence intervals.",
      nothing_computed     = "No indicators were successfully computed."
    ),
    es = list(
      error_df_type        = "El input 'df' debe ser un data.frame o climasus_df.",
      error_col_mapping_type = "col_mapping debe ser una lista con nombres.",
      error_conf_level     = "confidence_level debe ser un numero entre 0 y 1.",
      error_stage          = "Stage actual '{current}' insuficiente. Requerido: '{required}'. Ejecute sus_spatial_join() primero.",
      error_not_climasus   = "El input debe ser un objeto climasus_df. Ejecute el pipeline climasus4r primero.",
      no_indicators        = "Ningun indicador disponible con las columnas presentes. Use col_mapping para mapear sus columnas.",
      no_valid_indicators  = "Ninguno de los indicadores solicitados es valido. Use sus_socio_list_indicators() para ver los IDs disponibles.",
      auto_detected        = "{n} indicadores detectados automaticamente segun las columnas disponibles.",
      computing            = "Calculando {n} indicadores...",
      unknown_indicators   = "IDs desconocidos ignorados: {ids}",
      skipping_missing     = "Omitiendo '{id}': columnas faltantes: {cols}. Use col_mapping para mapear sus columnas.",
      formula_error        = "Error al evaluar la formula del indicador '{id}': {err}",
      indicator_done       = "  {name} ({unit})",
      success              = "{n} indicador(es) calculado(s). {n_ci} con intervalos de confianza.",
      nothing_computed     = "Ningun indicador fue calculado con exito."
    )
  )[[lang]]
}
