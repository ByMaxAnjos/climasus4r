# =============================================================================
# sus_mod_swot.R
# SWOT Analysis for Climate-Health Surveillance
#
# Theory:
#   IPCC AR6 (2022) - Vulnerability = f(Exposure, Sensitivity, Adaptive Capacity)
#   Andrews (1971) SWOT framework adapted for climate-health public policy
#   Gasparrini et al. (2017, Lancet Planet Health) - multi-city attribution
# Input : Named arguments of pre-processed climasus model objects
# Output: climasus_swot with per-quadrant numeric (0-100) and categorical scores
# =============================================================================

# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "entity", "quadrant", "indicator", "ind_code",
  "raw_value", "norm_score", "direction",
  "S_score", "W_score", "O_score", "T_score",
  "S_cat", "W_cat", "O_cat", "T_cat",
  "n_S", "n_W", "n_O", "n_T",
  "component", "af_pct", "an", "rank",
  "hot_rr", "sensitivity_index",
  "vi_score", "vi_percentile",
  "exposure_score", "sensitivity_score", "adaptive_capacity_score"
))

# ── Local i18n ────────────────────────────────────────────────────────────────
.swot_labels <- list(

  title = list(
    pt = "climasus4r \u2014 An\u00e1lise SWOT Clim\u00e1tico-Sa\u00fade",
    en = "climasus4r \u2014 Climate-Health SWOT Analysis",
    es = "climasus4r \u2014 An\u00e1lisis SWOT Clima-Salud"
  ),
  quadrant_S = list(pt = "For\u00e7as",       en = "Strengths",     es = "Fortalezas"),
  quadrant_W = list(pt = "Fraquezas",      en = "Weaknesses",    es = "Debilidades"),
  quadrant_O = list(pt = "Oportunidades",  en = "Opportunities", es = "Oportunidades"),
  quadrant_T = list(pt = "Amea\u00e7as",    en = "Threats",       es = "Amenazas"),

  ind_adaptive_cap = list(
    pt = "Capacidade Adaptativa (IV)",
    en = "Adaptive Capacity (VI)",
    es = "Capacidad Adaptativa (IV)"
  ),
  ind_sensitivity_vi = list(
    pt = "Sensibilidade Populacional (IV)",
    en = "Population Sensitivity (VI)",
    es = "Sensibilidad Poblacional (IV)"
  ),
  ind_af_strength = list(
    pt = "Baixa Carga Atribu\u00edvel (FA inv.)",
    en = "Low Attributable Burden (AF inv.)",
    es = "Baja Carga Atribuible (FA inv.)"
  ),
  ind_burden_rank_inv = list(
    pt = "Posto de Carga Baixo (inv.)",
    en = "Low Burden Rank (inv.)",
    es = "Bajo Rango de Carga (inv.)"
  ),
  ind_heat_af = list(
    pt = "FA ao Calor (%)",
    en = "Heat AF (%)",
    es = "FA por Calor (%)"
  ),
  ind_cold_af = list(
    pt = "FA ao Frio (%)",
    en = "Cold AF (%)",
    es = "FA por Fr\u00edo (%)"
  ),
  ind_stratum_ineq = list(
    pt = "Desigualdade entre Estratos",
    en = "Stratum Inequality",
    es = "Desigualdad entre Estratos"
  ),
  ind_vi_opportunity = list(
    pt = "Janela de Interven\u00e7\u00e3o (Percentil IV inv.)",
    en = "Intervention Window (VI Percentile inv.)",
    es = "Ventana de Intervenci\u00f3n (Percentil IV inv.)"
  ),
  ind_exposure_low = list(
    pt = "Exposi\u00e7\u00e3o Atual Moderada (Exposi\u00e7\u00e3o inv.)",
    en = "Moderate Current Exposure (Exposure inv.)",
    es = "Exposici\u00f3n Actual Moderada (Exposici\u00f3n inv.)"
  ),
  ind_exposure_threat = list(
    pt = "Exposi\u00e7\u00e3o Clim\u00e1tica (IV)",
    en = "Climate Exposure (VI)",
    es = "Exposici\u00f3n Clim\u00e1tica (IV)"
  ),
  ind_vi_threat = list(
    pt = "\u00cdndice de Vulnerabilidade Global",
    en = "Overall Vulnerability Index",
    es = "\u00cdndice de Vulnerabilidad Global"
  ),
  ind_burden_an = list(
    pt = "N\u00famero Atribu\u00edvel (Carga)",
    en = "Attributable Number (Burden)",
    es = "N\u00famero Atribuible (Carga)"
  ),
  ind_heat_rr = list(
    pt = "RR ao Calor (P95) \u2014 DLNM",
    en = "Heat RR (P95) \u2014 DLNM",
    es = "RR por Calor (P95) \u2014 DLNM"
  ),
  ind_max_stratum_rr = list(
    pt = "RR M\u00e1ximo por Estrato",
    en = "Maximum Stratum RR",
    es = "RR M\u00e1ximo por Estrato"
  ),

  step_extract = list(
    pt = "Extraindo indicadores de {n_inputs} fonte(s)...",
    en = "Extracting indicators from {n_inputs} source(s)...",
    es = "Extrayendo indicadores de {n_inputs} fuente(s)..."
  ),
  step_score = list(
    pt = "Calculando pontua\u00e7\u00f5es SWOT para {n_entities} entidade(s)...",
    en = "Computing SWOT scores for {n_entities} entity/entities...",
    es = "Calculando puntuaciones SWOT para {n_entities} entidad(es)..."
  ),
  done = list(
    pt = "Conclu\u00eddo. Entidades: {n_ent}; Indicadores: {n_ind}; Fontes: {sources}",
    en = "Done. Entities: {n_ent}; Indicators: {n_ind}; Sources: {sources}",
    es = "Listo. Entidades: {n_ent}; Indicadores: {n_ind}; Fuentes: {sources}"
  ),
  err_no_input = list(
    pt = "Pelo menos um objeto climasus deve ser fornecido (vulnerability, af, burden, dlnm ou sensitivity).",
    en = "At least one climasus object must be provided (vulnerability, af, burden, dlnm, or sensitivity).",
    es = "Debe proporcionarse al menos un objeto climasus (vulnerability, af, burden, dlnm o sensitivity)."
  ),
  err_bad_breaks = list(
    pt = "{.arg breaks} deve ser um vetor num\u00e9rico crescente com valores estritamente entre 0 e 100.",
    en = "{.arg breaks} must be a strictly increasing numeric vector with values between 0 and 100 (exclusive).",
    es = "{.arg breaks} debe ser un vector num\u00e9rico estrictamente creciente con valores entre 0 y 100 (exclusivos)."
  ),
  err_bad_labels = list(
    pt = "{.arg labels} deve ter comprimento {n_cats} (length(breaks) + 1).",
    en = "{.arg labels} must have length {n_cats} (length(breaks) + 1).",
    es = "{.arg labels} debe tener longitud {n_cats} (length(breaks) + 1)."
  ),
  warn_lang = list(
    pt = "Idioma '{lang}' n\u00e3o suportado. Usando 'pt'.",
    en = "Language '{lang}' not supported. Using 'pt'.",
    es = "Idioma '{lang}' no admitido. Usando 'pt'."
  )
)

#' @keywords internal
#' @noRd
.swl <- function(key, lang, ...) {
  entry <- .swot_labels[[key]]
  if (is.null(entry)) return(key)
  msg <- entry[[lang]] %||% entry[["pt"]]
  if (...length() > 0L) msg <- glue::glue(msg, .envir = rlang::env(...))
  msg
}

# Default category labels per language (for length(breaks)+1 categories)
.swot_default_labels <- list(
  pt = c("Baixo", "M\u00e9dio", "Alto"),
  en = c("Low", "Medium", "High"),
  es = c("Bajo", "Medio", "Alto")
)


# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' SWOT Analysis for Climate-Health Surveillance
#'
#' Synthesizes pre-computed climasus model objects into a Strengths,
#' Weaknesses, Opportunities, and Threats (SWOT) framework for climate-health
#' risk communication and territorial planning. Each quadrant aggregates
#' normalized indicators (0–100 scale) extracted from the supplied model objects;
#' quadrant scores are returned both as continuous values and as user-defined
#' categorical labels.
#'
#' @section SWOT Quadrant Indicators:
#' | Quadrant | Meaning | Indicators from each input |
#' |----------|---------|---------------------------|
#' | **S** Strengths | Current protective factors | `adaptive_capacity_score` (VI), low total AF% (af/burden), low burden rank |
#' | **W** Weaknesses | Current vulnerabilities | `sensitivity_score` (VI), heat/cold AF% (af), stratum RR inequality (sensitivity) |
#' | **O** Opportunities | Intervention windows | Low VI percentile (VI), low current exposure (VI) |
#' | **T** Threats | Climate-health risks | `exposure_score` + `vi_score` (VI), heat RR at P95 (dlnm), attributable number (burden), max stratum RR (sensitivity) |
#'
#' All indicators are normalized to 0–100 within the supplied data. For
#' **Strength** and **Opportunity** indicators the raw value is already oriented
#' so that higher = better; for **Weakness** and **Threat** indicators, higher
#' means worse. The quadrant score is the mean of available indicator scores.
#'
#' When `vulnerability` and/or `burden` provide multi-city data, the SWOT is
#' computed per city. Single-city inputs (`af`, `dlnm`, `sensitivity`) are
#' broadcast to all detected entities and treated as shared context.
#'
#' @param vulnerability A `climasus_vi` object from
#'   [sus_mod_vulnerability_index()], or `NULL`. Provides per-city VI scores,
#'   exposure, sensitivity, and adaptive capacity indicators.
#' @param af A `climasus_af` object from [sus_mod_af()], or `NULL`. Provides
#'   total, heat, and cold attributable fraction percentages.
#' @param burden A `climasus_burden` object from [sus_mod_burden()], or `NULL`.
#'   Provides city-level ranked attributable numbers and fractions.
#' @param dlnm A `climasus_dlnm` object from [sus_mod_dlnm()], or `NULL`.
#'   Provides the heat exposure-response RR at the 95th percentile of
#'   temperature.
#' @param sensitivity A `climasus_sensitivity` object from
#'   [sus_mod_sensitivity()], or `NULL`. Provides stratum-level RR inequality
#'   and the maximum hot RR across strata.
#' @param score_type Character. Which score types to compute:
#'   `"numeric"` (0–100), `"categorical"` (cut-point labels), or
#'   `"both"` (default).
#' @param breaks Numeric vector. Strictly increasing cut-points in (0, 100)
#'   used to convert numeric scores into categories. Default `c(33, 66)`
#'   produces three categories. Generates `length(breaks) + 1` categories.
#' @param labels Character vector or `NULL`. Category labels with length
#'   `length(breaks) + 1`. `NULL` (default) uses language-appropriate defaults:
#'   Portuguese `c("Baixo", "Médio", "Alto")`, English `c("Low", "Medium",
#'   "High")`, Spanish `c("Bajo", "Medio", "Alto")`.
#' @param city_col Character. Column name of the city/entity identifier in the
#'   `vulnerability$vi_table`. Defaults to `"city"`. Overridden by
#'   `vulnerability$meta$city_col` when available.
#' @param lang Character. Language for labels: `"pt"` (default), `"en"`, `"es"`.
#' @param verbose Logical. Print progress messages. Default `TRUE`.
#'
#' @return A `climasus_swot` list with:
#'   \describe{
#'     \item{`$scores`}{Tibble. One row per entity with: `entity`,
#'       `S_score`, `W_score`, `O_score`, `T_score` (numeric 0–100, `NA`
#'       when no indicators are available for a quadrant), `n_S`, `n_W`,
#'       `n_O`, `n_T` (indicator counts). When `score_type` \eqn{\in}
#'       `c("categorical","both")`: `S_cat`, `W_cat`, `O_cat`, `T_cat`.}
#'     \item{`$indicators`}{Tibble (long). One row per
#'       (entity \eqn{\times} quadrant \eqn{\times} indicator): `entity`,
#'       `quadrant`, `ind_code`, `indicator`, `raw_value`, `norm_score`,
#'       `direction` (`"positive"` or `"negative"`).}
#'     \item{`$meta`}{List: `n_entities`, `n_indicators`, `inputs_used`,
#'       `score_type`, `breaks`, `labels`, `lang`, `call_time`.}
#'   }
#'
#' @examples
#' \dontrun{
#' swot <- sus_mod_swot(
#'   vulnerability = vi_result,
#'   af            = af_result,
#'   burden        = burden_result,
#'   dlnm          = dlnm_result,
#'   score_type    = "both",
#'   lang          = "pt"
#' )
#'
#' swot$scores
#' sus_mod_plot_swot(swot, type = "matrix", lang = "pt")
#' sus_mod_plot_swot(swot, type = "radar",  lang = "en")
#' }
#'
#' @seealso [sus_mod_plot_swot()], [sus_mod_vulnerability_index()],
#'   [sus_mod_af()], [sus_mod_burden()], [sus_mod_dlnm()],
#'   [sus_mod_sensitivity()]
#'
#' @export
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_abort
#' @importFrom dplyr tibble bind_rows
#' @importFrom rlang env
#' @importFrom glue glue
sus_mod_swot <- function(
    vulnerability = NULL,
    af            = NULL,
    burden        = NULL,
    dlnm          = NULL,
    sensitivity   = NULL,
    score_type    = c("both", "numeric", "categorical"),
    breaks        = c(33, 66),
    labels        = NULL,
    city_col      = "city",
    lang          = c("pt", "en", "es"),
    verbose       = TRUE
) {
  score_type <- match.arg(score_type)
  lang <- if (is.character(lang) && length(lang) == 1L) {
    if (!lang %in% c("pt", "en", "es")) {
      cli::cli_alert_warning(glue::glue(.swl("warn_lang", "pt")))
      "pt"
    } else lang
  } else match.arg(lang)

  # -- 1. Validate at least one input ------------------------------------------
  inputs_used <- character(0L)
  if (!is.null(vulnerability)) {
    if (!inherits(vulnerability, "climasus_vi"))
      cli::cli_abort("{.arg vulnerability} must be a {.cls climasus_vi} from {.fn sus_mod_vulnerability_index}.")
    inputs_used <- c(inputs_used, "climasus_vi")
  }
  if (!is.null(af)) {
    if (!inherits(af, "climasus_af"))
      cli::cli_abort("{.arg af} must be a {.cls climasus_af} from {.fn sus_mod_af}.")
    inputs_used <- c(inputs_used, "climasus_af")
  }
  if (!is.null(burden)) {
    if (!inherits(burden, "climasus_burden"))
      cli::cli_abort("{.arg burden} must be a {.cls climasus_burden} from {.fn sus_mod_burden}.")
    inputs_used <- c(inputs_used, "climasus_burden")
  }
  if (!is.null(dlnm)) {
    if (!inherits(dlnm, "climasus_dlnm"))
      cli::cli_abort("{.arg dlnm} must be a {.cls climasus_dlnm} from {.fn sus_mod_dlnm}.")
    inputs_used <- c(inputs_used, "climasus_dlnm")
  }
  if (!is.null(sensitivity)) {
    if (!inherits(sensitivity, "climasus_sensitivity"))
      cli::cli_abort("{.arg sensitivity} must be a {.cls climasus_sensitivity} from {.fn sus_mod_sensitivity}.")
    inputs_used <- c(inputs_used, "climasus_sensitivity")
  }
  if (length(inputs_used) == 0L)
    cli::cli_abort(.swl("err_no_input", lang))

  # -- 2. Validate breaks and labels -------------------------------------------
  if (!is.numeric(breaks) || length(breaks) < 1L ||
      any(diff(c(0, breaks, 100)) <= 0))
    cli::cli_abort(.swl("err_bad_breaks", lang))

  n_cats <- length(breaks) + 1L
  if (is.null(labels)) {
    base_lbl <- .swot_default_labels[[lang]] %||% .swot_default_labels[["pt"]]
    labels <- if (length(base_lbl) == n_cats) {
      base_lbl
    } else {
      paste0("Cat", seq_len(n_cats))
    }
  } else if (length(labels) != n_cats) {
    cli::cli_abort(.swl("err_bad_labels", lang, n_cats = n_cats))
  }

  if (verbose) cli::cli_h1(.swl("title", lang))

  # -- 3. Determine entity names -----------------------------------------------
  entities <- .swot_resolve_entities(vulnerability, burden, city_col)

  # -- 4. Extract indicators per source ----------------------------------------
  if (verbose)
    cli::cli_alert_info(.swl("step_extract", lang, n_inputs = length(inputs_used)))

  ind_rows <- list()

  # 4a. climasus_vi (per-city indicators)
  if (!is.null(vulnerability)) {
    vt      <- vulnerability$vi_table
    vi_col  <- vulnerability$meta$city_col %||% city_col
    if (!vi_col %in% names(vt)) vi_col <- city_col
    city_nm <- if (vi_col %in% names(vt)) as.character(vt[[vi_col]]) else entities

    ind_rows <- c(ind_rows, list(
      # S: adaptive capacity — higher is a strength
      .swot_ind_rows(city_nm, "S", "adaptive_capacity_vi",
                     .swl("ind_adaptive_cap", lang),
                     vt$adaptive_capacity_score,
                     direction = "positive"),
      # W: population sensitivity — higher is a weakness
      .swot_ind_rows(city_nm, "W", "sensitivity_vi",
                     .swl("ind_sensitivity_vi", lang),
                     vt$sensitivity_score,
                     direction = "negative"),
      # O: low VI percentile → intervention window (invert)
      .swot_ind_rows(city_nm, "O", "vi_percentile_inv",
                     .swl("ind_vi_opportunity", lang),
                     100 - vt$vi_percentile,
                     direction = "positive", raw = vt$vi_percentile),
      # O: low current exposure → prevention window (invert)
      .swot_ind_rows(city_nm, "O", "exposure_inv",
                     .swl("ind_exposure_low", lang),
                     100 - vt$exposure_score,
                     direction = "positive", raw = vt$exposure_score),
      # T: climate exposure — higher is a threat
      .swot_ind_rows(city_nm, "T", "exposure_vi",
                     .swl("ind_exposure_threat", lang),
                     vt$exposure_score,
                     direction = "negative"),
      # T: overall VI score — higher is a threat
      .swot_ind_rows(city_nm, "T", "vi_score_threat",
                     .swl("ind_vi_threat", lang),
                     vt$vi_score,
                     direction = "negative")
    ))
  }

  # 4b. climasus_af (single-city, broadcast to all entities)
  if (!is.null(af)) {
    ttl      <- af$total
    tot_row  <- ttl[ttl$component == "total", ]
    heat_row <- ttl[ttl$component == "heat",  ]
    cold_row <- ttl[ttl$component == "cold",  ]

    af_total <- abs(tot_row$af_pct  %||% 0)
    af_heat  <- abs(heat_row$af_pct %||% 0)
    af_cold  <- abs(cold_row$af_pct %||% 0)

    ind_rows <- c(ind_rows, list(
      # S: low total attributable burden (inverted: 100 - AF%)
      .swot_ind_rows(entities, "S", "af_total_inv",
                     .swl("ind_af_strength", lang),
                     pmax(0, 100 - af_total),
                     direction = "positive", raw = af_total),
      # W: heat AF%
      .swot_ind_rows(entities, "W", "heat_af_pct",
                     .swl("ind_heat_af", lang),
                     pmin(100, af_heat),
                     direction = "negative"),
      # W: cold AF%
      .swot_ind_rows(entities, "W", "cold_af_pct",
                     .swl("ind_cold_af", lang),
                     pmin(100, af_cold),
                     direction = "negative")
    ))
  }

  # 4c. climasus_burden (per-city indicators)
  if (!is.null(burden)) {
    bt <- burden$burden_table
    if ("component" %in% names(bt))
      bt <- bt[bt$component == "total", ]
    cities_b <- as.character(bt$city)
    n_b      <- nrow(bt)

    # S: inverted rank → lower burden rank (smaller city burden) = strength
    rank_inv <- if (n_b > 0L) (n_b + 1L - bt$rank) / n_b * 100 else numeric(0L)
    an_norm  <- .swot_norm01(bt$an) * 100

    ind_rows <- c(ind_rows, list(
      .swot_ind_rows(cities_b, "S", "burden_rank_inv",
                     .swl("ind_burden_rank_inv", lang),
                     rank_inv,
                     direction = "positive", raw = bt$rank),
      # T: attributable number — higher is a threat
      .swot_ind_rows(cities_b, "T", "burden_an",
                     .swl("ind_burden_an", lang),
                     an_norm,
                     direction = "negative", raw = bt$an)
    ))
  }

  # 4d. climasus_dlnm (single-city, broadcast to all entities)
  if (!is.null(dlnm)) {
    er <- dlnm$exposure_response
    if (!is.null(er) && nrow(er) > 0L) {
      # RR at the highest available percentile (proxy for P95 heat effect)
      rr_95     <- as.numeric(er$rr[which.max(er$pct)] %||% 1)
      # Scale: RR 1.0 → 0; RR 3.0 → 100 (linear, capped)
      rr_norm   <- pmin(100, pmax(0, (rr_95 - 1) * 50))
      ind_rows <- c(ind_rows, list(
        .swot_ind_rows(entities, "T", "heat_rr_p95",
                       .swl("ind_heat_rr", lang),
                       rr_norm,
                       direction = "negative", raw = rr_95)
      ))
    }
  }

  # 4e. climasus_sensitivity (multi-stratum, broadcast to all entities)
  if (!is.null(sensitivity)) {
    comp <- sensitivity$comparison
    if (!is.null(comp) && nrow(comp) > 0L) {
      hot_rrs <- comp$hot_rr[!is.na(comp$hot_rr)]

      if (length(hot_rrs) >= 2L) {
        # W: RR ratio between highest and lowest stratum (inequality)
        rr_ratio  <- max(hot_rrs) / pmax(1e-6, min(hot_rrs))
        rr_ineq   <- pmin(100, pmax(0, (rr_ratio - 1) * 20))
        ind_rows  <- c(ind_rows, list(
          .swot_ind_rows(entities, "W", "stratum_inequality",
                         .swl("ind_stratum_ineq", lang),
                         rr_ineq,
                         direction = "negative", raw = rr_ratio)
        ))
      }

      if (length(hot_rrs) >= 1L) {
        # T: maximum hot RR across strata
        max_rr     <- max(hot_rrs)
        max_rr_norm <- pmin(100, pmax(0, (max_rr - 1) * 50))
        ind_rows   <- c(ind_rows, list(
          .swot_ind_rows(entities, "T", "max_stratum_rr",
                         .swl("ind_max_stratum_rr", lang),
                         max_rr_norm,
                         direction = "negative", raw = max_rr)
        ))
      }
    }
  }

  # -- 5. Build indicators tibble -----------------------------------------------
  all_inds <- dplyr::bind_rows(ind_rows)
  n_ind    <- nrow(all_inds)

  # -- 6. Aggregate scores per entity × quadrant --------------------------------
  all_entities <- unique(all_inds$entity)
  n_entities   <- length(all_entities)

  if (verbose)
    cli::cli_alert_info(.swl("step_score", lang, n_entities = n_entities))

  scores_list <- lapply(all_entities, function(ent) {
    ent_inds   <- all_inds[all_inds$entity == ent, ]
    quad_score <- function(q) {
      rows <- ent_inds[ent_inds$quadrant == q, ]
      if (nrow(rows) == 0L) return(list(score = NA_real_, n = 0L))
      list(score = mean(rows$norm_score, na.rm = TRUE), n = nrow(rows))
    }
    sq <- quad_score("S")
    wq <- quad_score("W")
    oq <- quad_score("O")
    tq <- quad_score("T")

    dplyr::tibble(
      entity  = ent,
      S_score = sq$score, n_S = as.integer(sq$n),
      W_score = wq$score, n_W = as.integer(wq$n),
      O_score = oq$score, n_O = as.integer(oq$n),
      T_score = tq$score, n_T = as.integer(tq$n)
    )
  })
  scores_tbl <- dplyr::bind_rows(scores_list)

  # -- 7. Add categorical scores (cut-point method) ----------------------------
  if (score_type %in% c("categorical", "both")) {
    cut_vec <- c(0, breaks, 100)
    to_cat  <- function(v) {
      as.character(cut(v, breaks = cut_vec,
                       labels = labels,
                       include.lowest = TRUE, right = TRUE))
    }
    scores_tbl$S_cat <- to_cat(scores_tbl$S_score)
    scores_tbl$W_cat <- to_cat(scores_tbl$W_score)
    scores_tbl$O_cat <- to_cat(scores_tbl$O_score)
    scores_tbl$T_cat <- to_cat(scores_tbl$T_score)
  }

  if (score_type == "numeric") {
    drop_cols  <- c("S_cat", "W_cat", "O_cat", "T_cat")
    scores_tbl <- scores_tbl[, !names(scores_tbl) %in% drop_cols]
  }

  # -- 8. Return ----------------------------------------------------------------
  if (verbose) {
    cli::cli_alert_success(
      .swl("done", lang,
           n_ent   = n_entities,
           n_ind   = n_ind,
           sources = paste(inputs_used, collapse = ", "))
    )
  }

  structure(
    list(
      scores     = scores_tbl,
      indicators = all_inds,
      meta       = list(
        n_entities   = n_entities,
        n_indicators = n_ind,
        inputs_used  = inputs_used,
        score_type   = score_type,
        breaks       = breaks,
        labels       = labels,
        lang         = lang,
        call_time    = Sys.time()
      )
    ),
    class = c("climasus_swot", "list")
  )
}


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

#' Resolve entity names from multi-city inputs
#' @keywords internal
#' @noRd
.swot_resolve_entities <- function(vulnerability, burden, city_col) {
  if (!is.null(vulnerability)) {
    vt  <- vulnerability$vi_table
    col <- vulnerability$meta$city_col %||% city_col
    if (!col %in% names(vt)) {
      chr_cols <- names(vt)[vapply(vt, is.character, logical(1L))]
      col <- if (length(chr_cols) > 0L) chr_cols[[1L]] else names(vt)[[1L]]
    }
    return(as.character(vt[[col]]))
  }
  if (!is.null(burden)) {
    bt <- burden$burden_table
    if ("component" %in% names(bt))
      bt <- bt[bt$component == "total", ]
    return(as.character(bt$city))
  }
  "overall"
}

#' Build a set of indicator rows for the indicators tibble
#' @keywords internal
#' @noRd
.swot_ind_rows <- function(entities, quadrant, ind_code, ind_label,
                            norm_scores, direction = "positive", raw = NULL) {
  n <- length(entities)
  if (length(norm_scores) == 1L && n > 1L)
    norm_scores <- rep(norm_scores, n)
  if (length(norm_scores) < n)
    norm_scores <- c(norm_scores, rep(NA_real_, n - length(norm_scores)))
  if (is.null(raw)) raw <- norm_scores
  if (length(raw) == 1L && n > 1L)
    raw <- rep(as.numeric(raw), n)
  if (length(raw) < n)
    raw <- c(as.numeric(raw), rep(NA_real_, n - length(raw)))

  dplyr::tibble(
    entity     = as.character(entities),
    quadrant   = quadrant,
    ind_code   = ind_code,
    indicator  = ind_label,
    raw_value  = as.numeric(raw)[seq_len(n)],
    norm_score = pmax(0, pmin(100, as.numeric(norm_scores)[seq_len(n)])),
    direction  = direction
  )
}

#' Min-max normalize a numeric vector to range 0-1; returns 0.5 for constants
#' @keywords internal
#' @noRd
.swot_norm01 <- function(x) {
  x   <- as.numeric(x)
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(diff(rng)) || diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[[1L]]) / diff(rng)
}


# =============================================================================
# S3 METHODS
# =============================================================================

#' Print a climasus_swot object
#'
#' @param x A `climasus_swot` object from [sus_mod_swot()].
#' @param ... Ignored.
#' @export
print.climasus_swot <- function(x, ...) {
  m <- x$meta
  cli::cli_h2("climasus_swot")
  cli::cli_text("{.strong Entities}    : {m$n_entities}")
  cli::cli_text("{.strong Indicators}  : {m$n_indicators}")
  cli::cli_text("{.strong Inputs}      : {paste(m$inputs_used, collapse = ', ')}")
  cli::cli_text("{.strong Score type}  : {m$score_type}")
  cli::cli_text("{.strong Breaks}      : {paste(m$breaks, collapse = ', ')}")
  cli::cli_text("{.strong Categories}  : {paste(m$labels, collapse = ' | ')}")
  cli::cli_text("{.strong Computed at} : {format(m$call_time, '%Y-%m-%d %H:%M:%S')}")
  if (nrow(x$scores) > 0L) {
    cli::cli_h3("Scores preview")
    print(x$scores[, intersect(c("entity","S_score","W_score","O_score","T_score",
                                  "S_cat","W_cat","O_cat","T_cat"),
                               names(x$scores))], n = 6L)
  }
  invisible(x)
}

#' Summarise a climasus_swot object
#'
#' @param object A `climasus_swot` object from [sus_mod_swot()].
#' @param ... Ignored.
#' @export
summary.climasus_swot <- function(object, ...) {
  print(object)
  invisible(object)
}

#' Tidy a climasus_swot object into a flat scores tibble
#'
#' @param x A `climasus_swot` object from [sus_mod_swot()].
#' @param ... Ignored.
#' @return A tibble with one row per entity.
#' @export
tidy.climasus_swot <- function(x, ...) {
  x$scores
}
