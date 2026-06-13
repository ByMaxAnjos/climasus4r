# =============================================================================
# sus_mod_spatial_reg.R
# Spatial Regression (SAR / SEM / SDM / SAC) for Climate-Health Associations
#
# Theory:
#   Anselin (1988) - Spatial Econometrics: Methods and Models
#   LeSage & Pace (2009) - Introduction to Spatial Econometrics
#   Bivand et al. (2013) - Applied Spatial Data Analysis with R
#   Elhorst (2014) - Spatial Econometrics: From Cross-Sectional Data to Panels
# Input : data.frame / climasus_df + formula + climasus_weights object
# Output: climasus_spatial_reg with coefficients, spatial params, impacts,
#         AIC comparison, Moran test on residuals
# =============================================================================

# -- NSE variable declarations -------------------------------------------------
utils::globalVariables(c(
  "term", "estimate", "std_error", "z_value", "p_value",
  "direct", "indirect", "total"
))

# -- Local i18n ----------------------------------------------------------------
.reg_msgs <- list(

  title = list(
    pt = "climasus4r \u2014 Regress\u00e3o Espacial ({model_label})",
    en = "climasus4r \u2014 Spatial Regression ({model_label})",
    es = "climasus4r \u2014 Regresi\u00f3n Espacial ({model_label})"
  ),

  step_check = list(
    pt = "Verificando entradas...",
    en = "Checking inputs...",
    es = "Verificando entradas..."
  ),

  step_ols = list(
    pt = "Ajustando baseline OLS para compara\u00e7\u00e3o de AIC...",
    en = "Fitting OLS baseline for AIC comparison...",
    es = "Ajustando OLS de referencia para comparaci\u00f3n de AIC..."
  ),

  step_fit = list(
    pt = "Ajustando modelo {model_label} (m\u00e9todo = {method})...",
    en = "Fitting {model_label} model (method = {method})...",
    es = "Ajustando modelo {model_label} (m\u00e9todo = {method})..."
  ),

  step_impacts = list(
    pt = "Calculando impactos diretos / indiretos / totais (R = {R_val} simula\u00e7\u00f5es)...",
    en = "Computing direct / indirect / total impacts (R = {R_val} simulations)...",
    es = "Calculando impactos directos / indirectos / totales (R = {R_val} simulaciones)..."
  ),

  step_moran = list(
    pt = "Testando autocorrela\u00e7\u00e3o espacial nos res\u00edduos (Moran)...",
    en = "Testing spatial autocorrelation in residuals (Moran)...",
    es = "Probando autocorrelaci\u00f3n espacial en residuos (Moran)..."
  ),

  warn_impacts = list(
    pt = "N\u00e3o foi poss\u00edvel calcular impactos: {msg}. O elemento $impacts ser\u00e1 NULL.",
    en = "Could not compute impacts: {msg}. The $impacts element will be NULL.",
    es = "No se pudieron calcular los impactos: {msg}. El elemento $impacts ser\u00e1 NULL."
  ),

  warn_sf_drop = list(
    pt = "Objeto {.arg df} do tipo {.cls sf}. Geometria descartada com sf::st_drop_geometry().",
    en = "{.arg df} is an {.cls sf} object. Geometry dropped with sf::st_drop_geometry().",
    es = "{.arg df} es un objeto {.cls sf}. Geometr\u00eda descartada con sf::st_drop_geometry()."
  ),

  done = list(
    pt = "Conclu\u00eddo. AIC modelo = {aic_m} | AIC OLS = {aic_ols} | Moran (res\u00edduos) p = {p_moran}",
    en = "Done. Model AIC = {aic_m} | OLS AIC = {aic_ols} | Moran (residuals) p = {p_moran}",
    es = "Listo. AIC modelo = {aic_m} | AIC OLS = {aic_ols} | Moran (residuos) p = {p_moran}"
  ),

  err_not_weights = list(
    pt = "{.arg W} deve ser um objeto {.cls climasus_weights} produzido por {.fn sus_mod_spatial_weights}.",
    en = "{.arg W} must be a {.cls climasus_weights} object from {.fn sus_mod_spatial_weights}.",
    es = "{.arg W} debe ser un objeto {.cls climasus_weights} de {.fn sus_mod_spatial_weights}."
  ),

  err_no_listw = list(
    pt = "{.arg W} n\u00e3o cont\u00e9m o elemento {.val listw}. Reconstrua com {.fn sus_mod_spatial_weights}.",
    en = "{.arg W} does not contain element {.val listw}. Rebuild with {.fn sus_mod_spatial_weights}.",
    es = "{.arg W} no contiene el elemento {.val listw}. Reconstruya con {.fn sus_mod_spatial_weights}."
  ),

  err_not_formula = list(
    pt = "{.arg formula} deve ser um objeto {.cls formula} (ex.: deaths ~ temp + precip).",
    en = "{.arg formula} must be a {.cls formula} object (e.g. deaths ~ temp + precip).",
    es = "{.arg formula} debe ser un objeto {.cls formula} (p.ej.: deaths ~ temp + precip)."
  ),

  err_not_df = list(
    pt = "{.arg df} deve ser um {.cls data.frame} ou {.cls climasus_df}.",
    en = "{.arg df} must be a {.cls data.frame} or {.cls climasus_df}.",
    es = "{.arg df} debe ser un {.cls data.frame} o {.cls climasus_df}."
  )
)

# -- Internal message helper ---------------------------------------------------
#' @keywords internal
#' @noRd
.rgl <- function(key, lang, ...) {
  entry <- .reg_msgs[[key]]
  if (is.null(entry)) return(key)
  msg <- entry[[lang]] %||% entry[["pt"]]
  if (...length() > 0L) {
    vars <- list(...)
    for (nm in names(vars)) msg <- gsub(sprintf("\\{%s\\}", nm), as.character(vars[[nm]]), msg)
  }
  msg
}

# -- Internal: human-readable model labels ------------------------------------
#' @keywords internal
#' @noRd
.reg_model_label <- function(model) {
  switch(
    model,
    lag    = "SAR (Spatial Lag)",
    error  = "SEM (Spatial Error)",
    durbin = "SDM (Spatial Durbin)",
    sac    = "SAC (Spatial Autocorrelation)",
    model
  )
}

# =============================================================================
# EXPORTED FUNCTION
# =============================================================================

#' Spatial Regression (SAR / SEM / SDM / SAC) for Climate-Health Associations
#'
#' Fits maximum-likelihood spatial regression models that account for spatial
#' spillovers between geographic units (e.g. municipalities) in climate-health
#' studies. Four model families are supported: the Spatial Autoregressive Model
#' (SAR / spatial lag), the Spatial Error Model (SEM), the Spatial Durbin Model
#' (SDM), and the Spatial Autocorrelation Model (SAC).
#'
#' @section Model families:
#'
#' \describe{
#'   \item{`"lag"` (SAR)}{
#'     The outcome in unit \eqn{i} depends on a weighted average of outcomes
#'     in neighbouring units (spatial lag). Estimated via
#'     `spatialreg::lagsarlm()`. Contains a spatial lag parameter `rho`.
#'   }
#'   \item{`"error"` (SEM)}{
#'     Spatial dependence enters through a spatially autocorrelated error term.
#'     Estimated via `spatialreg::errorsarlm()`. Contains a spatial error
#'     parameter `lambda`.
#'   }
#'   \item{`"durbin"` (SDM)}{
#'     Extends the SAR model by including spatially lagged covariates (Durbin
#'     terms). Estimated via `spatialreg::lagsarlm()` with `Durbin = TRUE`.
#'     Contains both `rho` and lagged-covariate coefficients.
#'   }
#'   \item{`"sac"` (SAC)}{
#'     Contains both a spatial lag of the outcome (`rho`) and a spatial error
#'     term (`lambda`). Estimated via `spatialreg::sacsarlm()`.
#'   }
#' }
#'
#' @section Impacts:
#'
#' For models with a spatial lag (`lag`, `durbin`, `sac`), the total effect of
#' a covariate is decomposed into *direct* (own-unit), *indirect* (spillover),
#' and *total* effects using `spatialreg::impacts()` with `R` simulations for
#' inference. For the `error` model impacts equal OLS slopes and no
#' decomposition is meaningful; the `$impacts` element will be `NULL`.
#'
#' @section Residual diagnostics:
#'
#' After fitting, [spdep::moran.test()] is applied to the model residuals to
#' verify that spatial autocorrelation has been adequately absorbed. A
#' non-significant Moran p-value indicates a well-specified spatial model.
#'
#' @param df A `data.frame` or `climasus_df` containing all variables in
#'   `formula`. If an `sf` object, geometry is dropped automatically.
#' @param formula A `formula` specifying the outcome and predictors, e.g.
#'   `deaths ~ mean_temp + precip + pib_pc`.
#' @param W A `climasus_weights` object produced by
#'   [sus_mod_spatial_weights()]. Must contain a `listw` element whose
#'   length matches `nrow(df)`.
#' @param model Character. Spatial model family. One of `"lag"` (SAR,
#'   default), `"error"` (SEM), `"durbin"` (SDM), or `"sac"` (SAC).
#' @param method Character. Estimation method passed to the underlying
#'   **spatialreg** function. One of `"eigen"` (default), `"LU"`,
#'   `"Chebyshev"`, or `"MC"`. `"eigen"` is exact; the others are
#'   approximations suitable for large datasets.
#' @param zero_policy Logical. If `TRUE` (default), spatial units with no
#'   neighbours (islands) are allowed and receive a zero spatial lag/error.
#'   Passed to all **spatialreg** and **spdep** calls.
#' @param lang Character. Output language for messages: `"pt"` (default),
#'   `"en"`, or `"es"`.
#' @param verbose Logical. If `TRUE` (default), print progress messages via
#'   **cli**.
#'
#' @return An object of class `c("climasus_spatial_reg", "list")` with the
#'   following named elements:
#'   \describe{
#'     \item{`$model`}{Character. The model argument value.}
#'     \item{`$model_label`}{Character. Human-readable model name.}
#'     \item{`$coefficients`}{`data.frame` with columns `term`, `estimate`,
#'       `std_error`, `z_value`, `p_value` for all covariates (excluding
#'       the spatial parameters `rho`/`lambda`).}
#'     \item{`$rho`}{Numeric spatial lag parameter (`NULL` for the `error`
#'       model).}
#'     \item{`$lambda`}{Numeric spatial error parameter (`NULL` for the
#'       `lag` and `durbin` models).}
#'     \item{`$impacts`}{`data.frame` with columns `term`, `direct`,
#'       `indirect`, `total` (from `spatialreg::impacts()`), or `NULL` for
#'       the `error` model or when impact computation fails.}
#'     \item{`$aic`}{Numeric. AIC of the spatial model.}
#'     \item{`$lm_aic`}{Numeric. AIC of the OLS baseline (same formula,
#'       same data).}
#'     \item{`$moran_residuals`}{Named list with elements `I` (Moran
#'       statistic), `p_value` (analytical p-value), and `z` (z-score).}
#'     \item{`$fitted`}{Numeric vector of fitted values.}
#'     \item{`$residuals`}{Numeric vector of model residuals.}
#'     \item{`$call`}{The matched function call.}
#'   }
#'
#' @examples
#' \dontrun{
#' library(climasus4r)
#'
#' # W <- sus_mod_spatial_weights(shp, style = "W")
#' result <- sus_mod_spatial_reg(
#'   df      = my_df,
#'   formula = deaths ~ mean_temp + precip + pib_pc,
#'   W       = W,
#'   model   = "lag",
#'   lang    = "pt"
#' )
#' print(result)
#' summary(result)
#'
#' # SDM with spillovers
#' result_sdm <- sus_mod_spatial_reg(
#'   df      = my_df,
#'   formula = deaths ~ mean_temp + precip,
#'   W       = W,
#'   model   = "durbin"
#' )
#' result_sdm$impacts
#' }
#'
#' @references
#' Anselin, L. (1988). \emph{Spatial Econometrics: Methods and Models}.
#'   Kluwer Academic Publishers.
#'
#' LeSage, J., & Pace, R. K. (2009). \emph{Introduction to Spatial
#'   Econometrics}. CRC Press / Taylor & Francis.
#'
#' Bivand, R. S., Pebesma, E., & Gomez-Rubio, V. (2013).
#'   \emph{Applied Spatial Data Analysis with R} (2nd ed.). Springer.
#'
#' Elhorst, J. P. (2014). \emph{Spatial Econometrics: From Cross-Sectional
#'   Data to Panels}. Springer.
#'
#' @seealso [sus_mod_spatial_weights()], [sus_mod_spatial_moran()],
#'   [sus_mod_spatial_scan()]
#'
#' @export
sus_mod_spatial_reg <- function(
    df,
    formula,
    W,
    model       = c("lag", "error", "durbin", "sac"),
    method      = c("eigen", "LU", "Chebyshev", "MC"),
    zero_policy = TRUE,
    lang        = "pt",
    verbose     = TRUE
) {
  .call        <- match.call()
  model        <- match.arg(model)
  method       <- match.arg(method)
  lang         <- match.arg(lang, c("pt", "en", "es"))
  model_label  <- .reg_model_label(model)

  # -- 1. Check required packages -----------------------------------------------
  rlang::check_installed(
    "spatialreg",
    reason = "for spatial regression models (SAR/SEM/SDM/SAC)"
  )
  rlang::check_installed(
    "spdep",
    reason = "for Moran test on residuals"
  )

  # -- 2. Validate inputs -------------------------------------------------------
  if (verbose) cli::cli_progress_step(.rgl("step_check", lang))

  if (!inherits(df, "data.frame")) {
    cli::cli_abort(.rgl("err_not_df", lang))
  }

  if (!inherits(formula, "formula")) {
    cli::cli_abort(.rgl("err_not_formula", lang))
  }

  if (!inherits(W, "climasus_weights")) {
    cli::cli_abort(.rgl("err_not_weights", lang))
  }

  if (!("listw" %in% names(W))) {
    cli::cli_abort(.rgl("err_no_listw", lang))
  }

  # Drop sf geometry if present
  if (inherits(df, "sf")) {
    if (verbose) {
      cli::cli_alert_warning(.rgl("warn_sf_drop", lang))
    }
    rlang::check_installed("sf", reason = "to drop geometry from sf objects")
    df <- sf::st_drop_geometry(df)
  }

  listw <- W$listw

  # -- 3. OLS baseline ----------------------------------------------------------
  if (verbose) cli::cli_progress_step(.rgl("step_ols", lang))
  lm_fit  <- stats::lm(formula, data = df)
  lm_aic  <- stats::AIC(lm_fit)

  # -- 4. Fit spatial model -----------------------------------------------------
  if (verbose) {
    cli::cli_progress_step(
      .rgl("step_fit", lang, model_label = model_label, method = method)
    )
  }

  fit <- switch(
    model,
    lag = spatialreg::lagsarlm(
      formula,
      data        = df,
      listw       = listw,
      method      = method,
      zero.policy = zero_policy
    ),
    error = spatialreg::errorsarlm(
      formula,
      data        = df,
      listw       = listw,
      method      = method,
      zero.policy = zero_policy
    ),
    durbin = spatialreg::lagsarlm(
      formula,
      data        = df,
      listw       = listw,
      Durbin      = TRUE,
      method      = method,
      zero.policy = zero_policy
    ),
    sac = spatialreg::sacsarlm(
      formula,
      data        = df,
      listw       = listw,
      method      = method,
      zero.policy = zero_policy
    )
  )

  # -- 5. Extract coefficients --------------------------------------------------
  fit_summary   <- summary(fit)
  coef_mat      <- fit_summary$Coef

  # spatialreg returns matrix with rownames = terms, cols vary by version
  # standard column names: Estimate, Std. Error, z value, Pr(>|z|)
  coef_df <- data.frame(
    term      = rownames(coef_mat),
    estimate  = as.numeric(coef_mat[, "Estimate"]),
    std_error = as.numeric(coef_mat[, grep("Std", colnames(coef_mat), value = TRUE)[1L]]),
    z_value   = as.numeric(coef_mat[, grep("^z|^Z", colnames(coef_mat), value = TRUE)[1L]]),
    p_value   = as.numeric(coef_mat[, grep("^Pr|^p", colnames(coef_mat), value = TRUE)[1L]]),
    stringsAsFactors = FALSE
  )

  # Spatial parameters (NULL when model doesn't include them)
  rho    <- if (model %in% c("lag", "durbin", "sac")) as.numeric(fit$rho)    else NULL
  lambda <- if (model %in% c("error", "sac"))         as.numeric(fit$lambda) else NULL

  # -- 6. Impacts ---------------------------------------------------------------
  impacts_df <- NULL

  if (model %in% c("lag", "durbin", "sac")) {
    if (verbose) {
      cli::cli_progress_step(.rgl("step_impacts", lang, R_val = 200L))
    }
    impacts_res <- tryCatch(
      spatialreg::impacts(fit, listw = listw, R = 200L),
      error = function(e) {
        if (verbose) {
          cli::cli_alert_warning(
            .rgl("warn_impacts", lang, msg = conditionMessage(e))
          )
        }
        NULL
      }
    )

    if (!is.null(impacts_res)) {
      # impacts() returns sarlm.impacts; extract direct/indirect/total
      imp_mat <- as.data.frame(impacts_res$res)
      # row names are the covariate names; columns: Direct Indirect Total
      col_names <- colnames(imp_mat)
      d_col <- grep("(?i)direct",   col_names, value = TRUE)[1L]
      i_col <- grep("(?i)indirect", col_names, value = TRUE)[1L]
      t_col <- grep("(?i)total",    col_names, value = TRUE)[1L]
      impacts_df <- data.frame(
        term     = rownames(imp_mat),
        direct   = as.numeric(imp_mat[[d_col]]),
        indirect = as.numeric(imp_mat[[i_col]]),
        total    = as.numeric(imp_mat[[t_col]]),
        stringsAsFactors = FALSE
      )
    }
  }

  # -- 7. Moran test on residuals -----------------------------------------------
  if (verbose) cli::cli_progress_step(.rgl("step_moran", lang))

  moran_out <- tryCatch({
    mt <- spdep::moran.test(
      stats::residuals(fit),
      listw,
      zero.policy = TRUE
    )
    list(
      I       = as.numeric(mt$estimate["Moran I statistic"]),
      p_value = as.numeric(mt$p.value),
      z       = as.numeric(mt$statistic)
    )
  }, error = function(e) {
    list(I = NA_real_, p_value = NA_real_, z = NA_real_)
  })

  # -- 8. Model AIC -------------------------------------------------------------
  mod_aic <- tryCatch(stats::AIC(fit), error = function(e) NA_real_)

  # -- 9. Done message ----------------------------------------------------------
  if (verbose) {
    aic_m   <- formatC(mod_aic, digits = 2, format = "f")
    aic_ols <- formatC(lm_aic, digits = 2, format = "f")
    p_moran <- if (!is.na(moran_out$p_value)) {
      formatC(moran_out$p_value, digits = 4, format = "f")
    } else {
      "NA"
    }
    cli::cli_alert_success(
      .rgl("done", lang, aic_m = aic_m, aic_ols = aic_ols, p_moran = p_moran)
    )
  }

  # -- 10. Assemble output ------------------------------------------------------
  structure(
    list(
      model            = model,
      model_label      = model_label,
      coefficients     = coef_df,
      rho              = rho,
      lambda           = lambda,
      impacts          = impacts_df,
      aic              = mod_aic,
      lm_aic           = lm_aic,
      moran_residuals  = moran_out,
      fitted           = as.numeric(stats::fitted(fit)),
      residuals        = as.numeric(stats::residuals(fit)),
      call             = .call
    ),
    class = c("climasus_spatial_reg", "list")
  )
}

# =============================================================================
# S3 METHODS
# =============================================================================

#' Print method for climasus_spatial_reg objects
#'
#' Displays a formatted summary of the spatial regression model, including
#' the model type, spatial parameters (rho / lambda), coefficient table,
#' AIC comparison with OLS, and the Moran test result on residuals.
#'
#' @param x A `climasus_spatial_reg` object from [sus_mod_spatial_reg()].
#' @param n_coef Integer. Maximum number of coefficient rows to display.
#'   Default `10`.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @export
print.climasus_spatial_reg <- function(x, n_coef = 10L, ...) {

  cli::cli_h1("climasus4r \u2014 Spatial Regression")
  cli::cli_text("{.strong Model:} {.val {x$model_label}}")
  cli::cli_rule(left = "Spatial Parameters")

  if (!is.null(x$rho)) {
    cli::cli_dl(c(
      "rho (spatial lag)" = formatC(x$rho, digits = 6, format = "f")
    ))
  }
  if (!is.null(x$lambda)) {
    cli::cli_dl(c(
      "lambda (spatial error)" = formatC(x$lambda, digits = 6, format = "f")
    ))
  }

  cli::cli_rule(left = "Coefficients")
  coef_show <- utils::head(x$coefficients, n_coef)
  for (i in seq_len(nrow(coef_show))) {
    r      <- coef_show[i, ]
    stars  <- .reg_pstars(r$p_value)
    cli::cli_text(
      "  {.field {r$term}}: {.val {formatC(r$estimate, digits=5, format='f')}} ",
      "(SE = {formatC(r$std_error, digits=5, format='f')}, ",
      "z = {formatC(r$z_value, digits=3, format='f')}, ",
      "p = {formatC(r$p_value, digits=4, format='f')}) {stars}"
    )
  }
  if (nrow(x$coefficients) > n_coef) {
    cli::cli_text(
      "  ... {nrow(x$coefficients) - n_coef} more term(s). ",
      "Access full table via {.code x$coefficients}."
    )
  }

  cli::cli_rule(left = "Model Fit")
  cli::cli_dl(c(
    "AIC (spatial model)" = formatC(x$aic,    digits = 2, format = "f"),
    "AIC (OLS baseline)"  = formatC(x$lm_aic, digits = 2, format = "f"),
    "delta AIC"           = formatC(x$lm_aic - x$aic, digits = 2, format = "f")
  ))

  cli::cli_rule(left = "Moran Test on Residuals")
  mr <- x$moran_residuals
  cli::cli_dl(c(
    "I"       = if (!is.na(mr$I))       formatC(mr$I,       digits = 6, format = "f") else "NA",
    "z"       = if (!is.na(mr$z))       formatC(mr$z,       digits = 4, format = "f") else "NA",
    "p-value" = if (!is.na(mr$p_value)) formatC(mr$p_value, digits = 4, format = "f") else "NA"
  ))

  if (!is.null(x$impacts)) {
    cli::cli_rule(left = "Direct / Indirect / Total Impacts")
    n_show <- min(nrow(x$impacts), n_coef)
    for (i in seq_len(n_show)) {
      r <- x$impacts[i, ]
      cli::cli_text(
        "  {.field {r$term}}: D = {formatC(r$direct, digits=5, format='f')}, ",
        "I = {formatC(r$indirect, digits=5, format='f')}, ",
        "T = {formatC(r$total, digits=5, format='f')}"
      )
    }
    if (nrow(x$impacts) > n_show) {
      cli::cli_text(
        "  ... {nrow(x$impacts) - n_show} more. Access via {.code x$impacts}."
      )
    }
  }

  cli::cli_rule()
  invisible(x)
}

#' Summary method for climasus_spatial_reg objects
#'
#' Prints the model and returns the key tables invisibly.
#'
#' @param object A `climasus_spatial_reg` object from [sus_mod_spatial_reg()].
#' @param ... Additional arguments passed to [print.climasus_spatial_reg()].
#'
#' @return A named list with elements `coefficients`, `impacts`,
#'   `moran_residuals`, `aic`, and `lm_aic`, invisibly.
#'
#' @export
summary.climasus_spatial_reg <- function(object, ...) {
  print(object, ...)
  invisible(list(
    coefficients    = object$coefficients,
    impacts         = object$impacts,
    moran_residuals = object$moran_residuals,
    aic             = object$aic,
    lm_aic          = object$lm_aic
  ))
}

# -- Internal helper: significance stars -------------------------------------
#' @keywords internal
#' @noRd
.reg_pstars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("***")
  if (p < 0.01)  return("**")
  if (p < 0.05)  return("*")
  if (p < 0.10)  return(".")
  ""
}
