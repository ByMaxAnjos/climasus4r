# =============================================================================
# sus_mod_spatial_bayes.R
# Bayesian CAR / BYM Disease Mapping with Climate Covariates (CARBayes)
#
# Theory:
#   Besag, J., York, J., & Mollie, A. (1991). Bayesian image restoration, with
#     two applications in spatial statistics. Annals of the Institute of
#     Statistical Mathematics, 43(1), 1-20. (BYM model)
#   Leroux, B. G., Lei, X., & Breslow, N. (2000). Estimation of disease rates
#     in small areas: a new mixed model for spatial dependence. In Statistical
#     Models in Epidemiology, the Environment, and Clinical Trials (pp. 179-191).
#   Lee, D. (2013). CARBayes: an R package for Bayesian spatial modelling with
#     conditional autoregressive priors. Journal of Statistical Software, 55(13).
# Input : climasus_df (or data.frame) at aggregate stage, climasus_weights W
# Output: climasus_spatial_bayes (list) with fixed effects, random effects,
#         smoothed relative risks, DIC, fitted values
# =============================================================================

# -- NSE variable declarations -------------------------------------------------
utils::globalVariables(c(
  "code_muni", "term", "mean", "sd", "lower95", "upper95",
  "phi_mean", "phi_sd", "rr_mean", "rr_lower95", "rr_upper95"
))

# -- Local i18n ----------------------------------------------------------------
.bayes_msgs <- list(

  step_check_pkgs = list(
    pt = "Verificando pacotes necess\u00e1rios (CARBayes, spdep)...",
    en = "Checking required packages (CARBayes, spdep)...",
    es = "Verificando paquetes necesarios (CARBayes, spdep)..."
  ),

  step_validate = list(
    pt = "Validando entradas e ordenando dados por code_muni...",
    en = "Validating inputs and sorting data by code_muni...",
    es = "Validando entradas y ordenando datos por code_muni..."
  ),

  step_build_w = list(
    pt = "Construindo matriz de adjac\u00eancia W ({n} x {n})...",
    en = "Building adjacency matrix W ({n} x {n})...",
    es = "Construyendo matriz de adyacencia W ({n} x {n})..."
  ),

  step_formula = list(
    pt = "Montando f\u00f3rmula: {fml}",
    en = "Building formula: {fml}",
    es = "Construyendo f\u00f3rmula: {fml}"
  ),

  step_mcmc = list(
    pt = "Executando MCMC {model_name} ({n_iter} itera\u00e7\u00f5es, burnin = {burnin}, thin = {thin})...",
    en = "Running MCMC {model_name} ({n_iter} iterations, burnin = {burnin}, thin = {thin})...",
    es = "Ejecutando MCMC {model_name} ({n_iter} iteraciones, burnin = {burnin}, thin = {thin})..."
  ),

  step_extract = list(
    pt = "Extraindo efeitos fixos, aleat\u00f3rios e riscos relativos...",
    en = "Extracting fixed effects, random effects, and relative risks...",
    es = "Extrayendo efectos fijos, aleatorios y riesgos relativos..."
  ),

  done = list(
    pt = "Conclu\u00eddo. DIC = {dic_val} | Itera\u00e7\u00f5es efetivas = {n_eff} | Modelo = {model_name}",
    en = "Done. DIC = {dic_val} | Effective iterations = {n_eff} | Model = {model_name}",
    es = "Listo. DIC = {dic_val} | Iteraciones efectivas = {n_eff} | Modelo = {model_name}"
  ),

  err_no_muni = list(
    pt = "Coluna {.val code_muni} n\u00e3o encontrada em {.arg df}.",
    en = "Column {.val code_muni} not found in {.arg df}.",
    es = "Columna {.val code_muni} no encontrada en {.arg df}."
  ),

  err_no_outcome = list(
    pt = "Coluna de desfecho {.val {outcome}} n\u00e3o encontrada em {.arg df}. Colunas dispon\u00edveis: {.val {avail}}.",
    en = "Outcome column {.val {outcome}} not found in {.arg df}. Available columns: {.val {avail}}.",
    es = "Columna de resultado {.val {outcome}} no encontrada en {.arg df}. Columnas disponibles: {.val {avail}}."
  ),

  err_not_weights = list(
    pt = "{.arg W} deve ser um objeto {.cls climasus_weights} produzido por {.fn sus_mod_spatial_weights}.",
    en = "{.arg W} must be a {.cls climasus_weights} object from {.fn sus_mod_spatial_weights}.",
    es = "{.arg W} debe ser un objeto {.cls climasus_weights} de {.fn sus_mod_spatial_weights}."
  ),

  err_length_mismatch = list(
    pt = "N\u00famero de linhas em {.arg df} ({n_df}) difere do n\u00famero de regi\u00f5es em {.arg W} ({n_w}). Certifique-se de que {.arg df} e {.arg W} cobrem os mesmos munic\u00edpios.",
    en = "Number of rows in {.arg df} ({n_df}) differs from number of regions in {.arg W} ({n_w}). Ensure {.arg df} and {.arg W} cover the same municipalities.",
    es = "El n\u00famero de filas en {.arg df} ({n_df}) difiere del n\u00famero de regiones en {.arg W} ({n_w}). Aseg\u00farese de que {.arg df} y {.arg W} cubren los mismos municipios."
  ),

  err_bad_covariate = list(
    pt = "Covari\u00e1vel(is) ausente(s) em {.arg df}: {.val {missing}}.",
    en = "Covariate(s) missing from {.arg df}: {.val {missing}}.",
    es = "Covariable(s) ausente(s) en {.arg df}: {.val {missing}}."
  ),

  err_bad_offset = list(
    pt = "Coluna de offset {.val {offset}} n\u00e3o encontrada em {.arg df}.",
    en = "Offset column {.val {offset}} not found in {.arg df}.",
    es = "Columna de offset {.val {offset}} no encontrada en {.arg df}."
  ),

  warn_na_outcome = list(
    pt = "{n_na} valor(es) NA em {.val {outcome}}. Linhas com NA ser\u00e3o removidas antes do ajuste.",
    en = "{n_na} NA value(s) in {.val {outcome}}. Rows with NA will be removed before fitting.",
    es = "{n_na} valor(es) NA en {.val {outcome}}. Las filas con NA ser\u00e1n eliminadas antes del ajuste."
  )
)

# -- Internal message helper ---------------------------------------------------
#' @keywords internal
#' @noRd
.brl <- function(key, lang, ...) {
  entry <- .bayes_msgs[[key]]
  if (is.null(entry)) return(key)
  msg <- entry[[lang]] %||% entry[["pt"]]
  if (...length() > 0L) msg <- glue::glue(msg, .envir = rlang::env(...))
  msg
}

# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' Bayesian CAR / BYM Disease Mapping with Climate Covariates
#'
#' @description
#' Fits a Bayesian hierarchical spatial model for disease mapping using
#' Conditional Autoregressive (CAR) priors implemented in the **CARBayes**
#' package. Three model specifications are available: the Besag-York-Mollie
#' (BYM) model with structured and unstructured random effects, the Leroux
#' model with a mixing parameter for spatial dependence, and the independent
#' random effects model. Climate and environmental covariates stored in a
#' `climasus_df` can be passed directly as fixed effects.
#'
#' @section Model specifications:
#'
#' Smoothed relative risks are computed as:
#'
#' \deqn{RR_i = \frac{\hat{Y}_i}{E_i}}
#'
#' where \eqn{\hat{Y}_i} are the posterior mean fitted counts and \eqn{E_i}
#' are the expected counts (offset on the original scale). When no offset is
#' supplied, \eqn{E_i} is set to the overall mean of the fitted values, which
#' is equivalent to a standardised mortality/morbidity ratio. Credible intervals
#' for the RR are derived from the 2.5th and 97.5th percentiles of the
#' MCMC samples for fitted values divided by the expected.
#'
#' @section Prior specification:
#'
#' `prior_tau2` specifies the shape and rate parameters of an
#' Inverse-Gamma prior for the variance component \eqn{\tau^2}:
#' \eqn{\tau^2 \sim \text{Inv-Gamma}(\text{shape}, \text{rate})}.
#' The default `c(1, 0.01)` is weakly informative. Fixed-effect coefficients
#' receive flat (improper) priors.
#'
#' @param df A `data.frame` or `climasus_df` at the aggregate pipeline stage.
#'   Must contain `code_muni` (7-digit IBGE code), the `outcome` column, all
#'   columns named in `covariates`, and optionally an `offset` column.
#'   Rows are sorted by `code_muni` internally to align with `W`.
#' @param outcome Character. Name of the response variable column in `df`
#'   (e.g. `"deaths"`, `"hospitalizations"`).
#' @param W A `climasus_weights` object produced by
#'   [sus_mod_spatial_weights()]. Must contain either a `$W` dense matrix or a
#'   `$nb` neighbour list from which a binary adjacency matrix is built.
#' @param covariates Character vector of covariate column names in `df` to
#'   include as fixed effects. Default `NULL` (intercept-only model).
#' @param offset Character. Name of an **expected counts** column in `df`
#'   (on the natural scale, not log-transformed). Used as `log(offset)` in the
#'   linear predictor for Poisson models. Default `NULL`.
#' @param family Character. Response distribution. One of `"poisson"`
#'   (default), `"binomial"`, or `"gaussian"`.
#' @param model Character. CAR model structure. One of `"bym"` (default),
#'   `"leroux"`, or `"independent"`. See **Model specifications**.
#' @param n_iter Positive integer. Total number of MCMC iterations
#'   (including burn-in). Default `10000`.
#' @param burnin Positive integer. Number of initial MCMC iterations to
#'   discard as burn-in. Must be less than `n_iter`. Default `2000`.
#' @param thin Positive integer. Thinning interval: only every `thin`-th
#'   post-burn-in iteration is retained. Default `10`.
#' @param prior_tau2 Numeric vector of length 2. Shape and rate parameters of
#'   the Inverse-Gamma prior for the variance of spatial random effects.
#'   Default `c(1, 0.01)` (weakly informative).
#' @param seed Integer. Random seed passed to [base::set.seed()] for
#'   reproducible MCMC. Default `42`.
#' @param lang Character. Language for CLI messages: `"pt"` (default),
#'   `"en"`, `"es"`.
#' @param verbose Logical. If `TRUE` (default), print MCMC progress messages
#'   via **cli**.
#'
#' @return A named list of class `c("climasus_spatial_bayes", "list")` with
#'   slots `$fixed` (posterior fixed effects), `$random` (spatial random
#'   effects per municipality), `$rr` (posterior relative risk with 95\%
#'   credible intervals), `$fitted` (posterior mean fitted values), `$dic`
#'   (Deviance Information Criterion), `$model`, `$family`,
#'   `$n_iter_effective`, and `$call`.
#'
#' @references
#' Besag, J., York, J., & Mollie, A. (1991). Bayesian image restoration, with
#' two applications in spatial statistics. \emph{Annals of the Institute of
#' Statistical Mathematics}, 43(1), 1-20.
#'
#' Leroux, B. G., Lei, X., & Breslow, N. (2000). Estimation of disease rates
#' in small areas: a new mixed model for spatial dependence. In
#' \emph{Statistical Models in Epidemiology, the Environment, and Clinical
#' Trials} (pp. 179-191). Springer.
#'
#' Lee, D. (2013). CARBayes: an R package for Bayesian spatial modelling with
#' conditional autoregressive priors. \emph{Journal of Statistical Software},
#' 55(13), 1-24.
#'
#' @seealso
#' [sus_mod_spatial_weights()] to build the required adjacency object,
#' [sus_mod_spatial_moran()] for exploratory spatial autocorrelation,
#' [sus_mod_spatial_scan()] for cluster detection.
#'
#' @examples
#' \dontrun{
#' library(climasus4r)
#' library(geobr)
#'
#' # Build spatial weights for Nordeste
#' shp <- geobr::read_municipality(code_muni = "all", year = 2022)
#' shp_ne <- shp[shp$abbrev_state %in% c("CE", "RN", "PB", "PE"), ]
#' W <- sus_mod_spatial_weights(shp_ne, return_matrix = TRUE)
#'
#' # Prepare aggregated health-climate data (must have code_muni)
#' # agg <- sus_data_aggregate(cleaned_df) |> sus_climate_aggregate()
#'
#' # BYM Poisson model with temperature as covariate
#' result <- sus_mod_spatial_bayes(
#'   df         = agg,
#'   outcome    = "deaths",
#'   W          = W,
#'   covariates = c("temp_mean", "prec_total"),
#'   offset     = "expected_deaths",
#'   family     = "poisson",
#'   model      = "bym",
#'   n_iter     = 10000,
#'   burnin     = 2000,
#'   thin       = 10,
#'   seed       = 42,
#'   lang       = "pt"
#' )
#'
#' print(result)
#' result$rr
#' result$fixed
#' }
#'
#' @export
sus_mod_spatial_bayes <- function(
    df,
    outcome,
    W,
    covariates  = NULL,
    offset      = NULL,
    family      = c("poisson", "binomial", "gaussian"),
    model       = c("bym", "leroux", "independent"),
    n_iter      = 10000L,
    burnin      = 2000L,
    thin        = 10L,
    prior_tau2  = c(1, 0.01),
    seed        = 42L,
    lang        = "pt",
    verbose     = TRUE
) {
  .call <- match.call()
  family <- match.arg(family)
  model  <- match.arg(model)
  lang   <- match.arg(lang, c("pt", "en", "es"))

  # ── 0. dependency check ──────────────────────────────────────────────────────
  if (verbose) cli::cli_progress_step(.brl("step_check_pkgs", lang))
  rlang::check_installed(
    "CARBayes",
    reason = "for Bayesian spatial CAR/BYM models"
  )
  rlang::check_installed(
    "spdep",
    reason = "to build adjacency matrix from neighbour list"
  )

  # ── 1. validate inputs ───────────────────────────────────────────────────────
  if (verbose) cli::cli_progress_step(.brl("step_validate", lang))

  if (!("code_muni" %in% names(df))) {
    cli::cli_abort(.brl("err_no_muni", lang))
  }

  avail <- paste(names(df), collapse = ", ")
  if (!(outcome %in% names(df))) {
    cli::cli_abort(.brl("err_no_outcome", lang, outcome = outcome, avail = avail))
  }

  if (!inherits(W, "climasus_weights")) {
    cli::cli_abort(.brl("err_not_weights", lang))
  }

  if (!is.null(covariates)) {
    missing_cov <- setdiff(covariates, names(df))
    if (length(missing_cov) > 0L) {
      cli::cli_abort(
        .brl("err_bad_covariate", lang, missing = paste(missing_cov, collapse = ", "))
      )
    }
  }

  if (!is.null(offset) && !(offset %in% names(df))) {
    cli::cli_abort(.brl("err_bad_offset", lang, offset = offset))
  }

  # ── 2. sort by code_muni to align with W ────────────────────────────────────
  df_sorted <- df[order(df[["code_muni"]]), , drop = FALSE]

  n_df <- nrow(df_sorted)
  n_w  <- W$n_regions

  if (n_df != n_w) {
    cli::cli_abort(.brl("err_length_mismatch", lang, n_df = n_df, n_w = n_w))
  }

  # NA check on outcome
  n_na <- sum(is.na(df_sorted[[outcome]]))
  if (n_na > 0L) {
    if (verbose) {
      cli::cli_alert_warning(
        .brl("warn_na_outcome", lang, n_na = n_na, outcome = outcome)
      )
    }
    df_sorted <- df_sorted[!is.na(df_sorted[[outcome]]), , drop = FALSE]
  }

  # ── 3. build adjacency matrix W_mat ─────────────────────────────────────────
  if (verbose) {
    cli::cli_progress_step(.brl("step_build_w", lang, n = n_df))
  }

  if (!is.null(W$W)) {
    W_mat <- W$W
  } else {
    W_mat <- spdep::nb2mat(W$nb, style = "B", zero.policy = TRUE)
  }
  # Ensure binary adjacency (0/1 integers) as required by CARBayes
  W_mat <- (W_mat != 0) * 1L
  storage.mode(W_mat) <- "numeric"

  # ── 4. build formula ─────────────────────────────────────────────────────────
  rhs <- if (!is.null(covariates) && length(covariates) > 0L) {
    paste(covariates, collapse = " + ")
  } else {
    "1"
  }

  fml_str <- paste0(outcome, " ~ ", rhs)

  if (!is.null(offset)) {
    fml_str <- paste0(fml_str, " + offset(log(", offset, "))")
  }

  fml <- stats::as.formula(fml_str, env = parent.frame())

  if (verbose) {
    cli::cli_progress_step(.brl("step_formula", lang, fml = fml_str))
  }

  # ── 5. fit CAR model via CARBayes ────────────────────────────────────────────
  model_name <- switch(model,
    bym         = "S.CARbym",
    leroux      = "S.CARleroux",
    independent = "S.CARindependent"
  )

  if (verbose) {
    cli::cli_progress_step(
      .brl("step_mcmc", lang,
           model_name = model_name,
           n_iter     = n_iter,
           burnin     = burnin,
           thin       = thin)
    )
  }

  set.seed(seed)

  fit <- switch(model,
    bym = CARBayes::S.CARbym(
      formula    = fml,
      data       = as.data.frame(df_sorted),
      family     = family,
      W          = W_mat,
      burnin     = as.integer(burnin),
      n.sample   = as.integer(n_iter),
      thin       = as.integer(thin),
      prior.tau2 = prior_tau2,
      verbose    = FALSE
    ),
    leroux = CARBayes::S.CARleroux(
      formula    = fml,
      data       = as.data.frame(df_sorted),
      family     = family,
      W          = W_mat,
      burnin     = as.integer(burnin),
      n.sample   = as.integer(n_iter),
      thin       = as.integer(thin),
      prior.tau2 = prior_tau2,
      verbose    = FALSE
    ),
    independent = CARBayes::S.CARindependent(
      formula    = fml,
      data       = as.data.frame(df_sorted),
      family     = family,
      W          = W_mat,
      burnin     = as.integer(burnin),
      n.sample   = as.integer(n_iter),
      thin       = as.integer(thin),
      prior.tau2 = prior_tau2,
      verbose    = FALSE
    )
  )

  # ── 6. extract results ───────────────────────────────────────────────────────
  if (verbose) cli::cli_progress_step(.brl("step_extract", lang))

  # -- 6a. Fixed effects from fit$summary.results ------------------------------
  # CARBayes summary.results rows: intercept + covariates + variance params
  # We select only rows corresponding to regression coefficients (beta rows)
  sum_res <- fit$summary.results

  # Identify beta rows: they appear before variance/mixing parameter rows
  # Row names containing "tau2", "nu2", "rho", "Sigma" are not fixed effects
  coef_pattern <- "^(Intercept|[A-Za-z_][A-Za-z0-9_\\.]*)"
  excl_pattern <- "(tau2|nu2|rho|Sigma|phi|psi|delta|gamma)"
  all_rows     <- rownames(sum_res)
  beta_rows    <- all_rows[
    grepl(coef_pattern, all_rows) & !grepl(excl_pattern, all_rows)
  ]

  if (length(beta_rows) == 0L) {
    # Fallback: take first row(s) that are not clearly hyperparameters
    beta_rows <- head(all_rows, 1L + length(covariates))
  }

  fixed_df <- data.frame(
    term    = beta_rows,
    mean    = as.numeric(sum_res[beta_rows, "Mean"]),
    sd      = as.numeric(sum_res[beta_rows, "SD"]),
    lower95 = as.numeric(sum_res[beta_rows, "2.5%"]),
    upper95 = as.numeric(sum_res[beta_rows, "97.5%"]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # -- 6b. Random effects (phi samples) ----------------------------------------
  # CARBayes stores spatial random effects in fit$samples$phi
  phi_samples <- fit$samples$phi  # matrix: n_eff x n_areas

  n_iter_effective <- nrow(phi_samples)

  phi_mean_vec <- colMeans(phi_samples)
  phi_sd_vec   <- apply(phi_samples, 2L, stats::sd)

  random_df <- data.frame(
    code_muni = df_sorted[["code_muni"]],
    phi_mean  = phi_mean_vec,
    phi_sd    = phi_sd_vec,
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  # -- 6c. Fitted values (posterior mean, response scale) ----------------------
  fitted_vals <- fit$fitted.values  # posterior mean on response scale

  # -- 6d. Relative risks ------------------------------------------------------
  # Expected counts: if offset provided use exp(log_offset) = offset column
  # Otherwise use overall mean of fitted values as reference
  if (!is.null(offset)) {
    expected_vec <- as.numeric(df_sorted[[offset]])
  } else {
    mean_fitted  <- mean(fitted_vals, na.rm = TRUE)
    expected_vec <- rep(mean_fitted, length(fitted_vals))
  }

  # Compute RR from MCMC samples for fitted values
  # fit$samples$fitted is n_eff x n_areas (on response scale for Poisson)
  fitted_samples <- fit$samples$fitted  # matrix n_eff x n

  rr_samples <- sweep(fitted_samples, 2L, expected_vec, FUN = "/")

  rr_mean_vec   <- colMeans(rr_samples)
  rr_lower_vec  <- apply(rr_samples, 2L, stats::quantile, probs = 0.025)
  rr_upper_vec  <- apply(rr_samples, 2L, stats::quantile, probs = 0.975)

  rr_df <- data.frame(
    code_muni  = df_sorted[["code_muni"]],
    rr_mean    = rr_mean_vec,
    rr_lower95 = rr_lower_vec,
    rr_upper95 = rr_upper_vec,
    stringsAsFactors = FALSE,
    row.names  = NULL
  )

  # -- 6e. DIC -----------------------------------------------------------------
  dic_val <- tryCatch(
    fit$modelfit["DIC"],
    error = function(e) NA_real_
  )

  # ── 7. done message ──────────────────────────────────────────────────────────
  if (verbose) {
    dic_str <- if (!is.na(dic_val)) formatC(dic_val, digits = 2, format = "f") else "NA"
    cli::cli_alert_success(
      .brl("done", lang,
           dic_val    = dic_str,
           n_eff      = n_iter_effective,
           model_name = model_name)
    )
  }

  # ── 8. assemble output ───────────────────────────────────────────────────────
  structure(
    list(
      fixed           = fixed_df,
      random          = random_df,
      rr              = rr_df,
      fitted          = fitted_vals,
      dic             = dic_val,
      model           = model,
      family          = family,
      n_iter_effective = n_iter_effective,
      call            = .call
    ),
    class = c("climasus_spatial_bayes", "list")
  )
}

# =============================================================================
# S3 METHODS
# =============================================================================

#' Print method for climasus_spatial_bayes objects
#'
#' Displays a formatted summary of the Bayesian spatial model fit, including
#' DIC, effective MCMC sample size, model specification, and the top fixed
#' effects with posterior means and 95\% credible intervals.
#'
#' @param x A `climasus_spatial_bayes` object from [sus_mod_spatial_bayes()].
#' @param n_fixed Integer. Maximum number of fixed-effect rows to display.
#'   Default `10L`.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.climasus_spatial_bayes <- function(x, n_fixed = 10L, ...) {
  cli::cli_h1(
    "climasus4r \u2014 Bayesian Spatial Model ({.cls climasus_spatial_bayes})"
  )

  dic_str <- if (!is.na(x$dic)) formatC(x$dic, digits = 2, format = "f") else "NA"

  cli::cli_dl(c(
    "Model"                = x$model,
    "Family"               = x$family,
    "DIC"                  = dic_str,
    "Effective iterations" = as.character(x$n_iter_effective),
    "Areas"                = as.character(nrow(x$rr))
  ))

  cli::cli_rule(left = "Fixed Effects (posterior mean [95% CrI])")

  fx <- x$fixed
  n_show <- min(nrow(fx), as.integer(n_fixed))
  for (i in seq_len(n_show)) {
    term_str  <- fx$term[i]
    mean_str  <- formatC(fx$mean[i],    digits = 4, format = "f")
    lo_str    <- formatC(fx$lower95[i], digits = 4, format = "f")
    hi_str    <- formatC(fx$upper95[i], digits = 4, format = "f")
    cli::cli_text(
      "{.field {term_str}}: {mean_str}  [{lo_str}, {hi_str}]"
    )
  }
  if (nrow(fx) > n_show) {
    cli::cli_text(
      "... {nrow(fx) - n_show} more fixed effect(s) not shown."
    )
  }

  cli::cli_rule(left = "Relative Risk (posterior mean across areas)")
  rr_mean_overall <- mean(x$rr$rr_mean, na.rm = TRUE)
  rr_min          <- min(x$rr$rr_mean,  na.rm = TRUE)
  rr_max          <- max(x$rr$rr_mean,  na.rm = TRUE)

  cli::cli_dl(c(
    "RR mean (all areas)"  = formatC(rr_mean_overall, digits = 4, format = "f"),
    "RR range"             = paste0(
      formatC(rr_min, digits = 4, format = "f"),
      " \u2013 ",
      formatC(rr_max, digits = 4, format = "f")
    )
  ))

  cli::cli_rule()
  invisible(x)
}

#' Summary method for climasus_spatial_bayes objects
#'
#' Alias for [print.climasus_spatial_bayes()].
#'
#' @param object A `climasus_spatial_bayes` object.
#' @param ... Additional arguments passed to [print.climasus_spatial_bayes()].
#'
#' @return Invisibly returns `object`.
#'
#' @export
summary.climasus_spatial_bayes <- function(object, ...) {
  print(object, ...)
  invisible(object)
}
