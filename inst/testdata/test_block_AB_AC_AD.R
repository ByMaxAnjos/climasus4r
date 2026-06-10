# ── Grupo AB: Espacial (spatial_weights, moran, bayes, scan, reg) ─────────────

cli::cli_h2("Grupo AB — Espacial")

# ── Teste 1: spatial_weights | queen contiguity ───────────────────────────────
run_test("spatial_weights | queen contiguity", {
  W <- sus_mod_spatial_weights(munic_sf, style = "W", verbose = FALSE)
  W <<- W
  stopifnot(
    inherits(W, "climasus_weights"),
    !is.null(W$listw),
    W$n_regions > 0
  )
  W
})

# ── Teste 2: spatial_moran | global I ────────────────────────────────────────
run_test("spatial_moran | global I", {
  moran_fit <- sus_mod_spatial_moran(agg_city_df, outcome = "n_obitos",
    W = W, permutations = 99L, verbose = FALSE)
  moran_fit <<- moran_fit
  stopifnot(
    inherits(moran_fit, "climasus_spatial_moran"),
    all(c("global", "local") %in% names(moran_fit)),
    "I" %in% names(moran_fit$global)
  )
  moran_fit
})

# ── Teste 3: plot_spatial_moran | scatter ────────────────────────────────────
run_test("plot_spatial_moran | scatter", {
  p <- sus_mod_plot_spatial_moran(moran_fit, municipalities = munic_sf,
    type = "scatter", verbose = FALSE, lang = "pt")
  stopifnot(inherits(p, c("gg", "ggplot", "patchwork")))
  p
})

# ── Teste 4: spatial_bayes | BYM (CARBayes guard) ────────────────────────────
local({
  if (!requireNamespace("CARBayes", quietly = TRUE)) {
    cli::cli_alert_warning("  spatial_bayes: SKIP — CARBayes não instalado")
    return(invisible(NULL))
  }
  run_test("spatial_bayes | BYM", {
    r <- sus_mod_spatial_bayes(agg_city_df, outcome = "n_obitos", W = W,
      model = "bym", n_iter = 1000L, burnin = 200L, verbose = FALSE)
    stopifnot(inherits(r, "climasus_spatial_bayes"))
    r
  })
})

# ── Teste 5: spatial_reg | SAR lag (spatialreg guard) ────────────────────────
local({
  if (!requireNamespace("spatialreg", quietly = TRUE)) {
    cli::cli_alert_warning("  spatial_reg: SKIP — spatialreg não instalado")
    return(invisible(NULL))
  }
  run_test("spatial_reg | SAR lag", {
    r <- sus_mod_spatial_reg(agg_city_df,
      formula = n_obitos ~ tair_dry_bulb_c, W = W, model = "lag",
      verbose = FALSE)
    stopifnot(inherits(r, "climasus_spatial_reg"))
    r
  })
})

# ── Grupo AC: Espaço-temporal (spacetime_bayes + exceedance + predict) ────────

cli::cli_h2("Grupo AC — Espaço-temporal")

local({
  if (!requireNamespace("INLA", quietly = TRUE)) {
    cli::cli_alert_warning("  spacetime_bayes: SKIP — INLA não instalado")
    cli::cli_alert_warning("  spacetime_exceedance: SKIP — INLA não instalado")
    cli::cli_alert_warning("  spacetime_predict: SKIP — INLA não instalado")
    return(invisible(NULL))
  }

  run_test("spacetime_bayes | BYM2 RW1", {
    st_fit <- sus_mod_spacetime_bayes(
      df             = merged_exact,
      outcome        = "n_obitos",
      W              = W,
      time_col       = "date",
      time_unit      = "month",
      family         = "poisson",
      spatial_model  = "bym2",
      temporal_model = "rw1",
      interaction_type = "none",
      compute_waic   = TRUE,
      verbose        = FALSE
    )
    st_fit <<- st_fit
    stopifnot(
      inherits(st_fit, "climasus_spacetime_bayes"),
      all(c("fixed", "rr", "spatial_re", "temporal_re", "meta") %in% names(st_fit)),
      is.data.frame(st_fit$fixed),
      is.data.frame(st_fit$rr)
    )
    st_fit
  })

  run_test("spacetime_exceedance | P(RR>1)", {
    exc_fit <- sus_mod_spacetime_exceedance(
      fit        = st_fit,
      thresholds = c(1.0, 1.5),
      verbose    = FALSE
    )
    exc_fit <<- exc_fit
    stopifnot(
      inherits(exc_fit, "climasus_spacetime_exceedance"),
      all(c("exceedance", "summary", "meta") %in% names(exc_fit)),
      is.data.frame(exc_fit$exceedance)
    )
    exc_fit
  })

  run_test("spacetime_predict | in-sample", {
    pred_fit <- sus_mod_spacetime_predict(
      fit        = st_fit,
      horizon    = 0L,
      include_ci = TRUE,
      verbose    = FALSE
    )
    stopifnot(
      inherits(pred_fit, "climasus_spacetime_pred"),
      all(c("predictions", "meta") %in% names(pred_fit)),
      is.data.frame(pred_fit$predictions),
      all(c("pred_mean", "pred_lower95", "pred_upper95") %in%
            names(pred_fit$predictions))
    )
    pred_fit
  })
})

# ── Grupo AD: sus_mod_swot + sus_mod_plot_swot ────────────────────────────────

cli::cli_h2("Grupo AD — SWOT")

# ── Teste 1: swot | from multiple objects ─────────────────────────────────────
run_test("swot | from multiple objects", {
  swot_fit <- sus_mod_swot(
    vulnerability = vi_fit,
    dlnm          = fits_list[[1]],
    af            = af_list[[1]],
    burden        = burd_fit,
    sensitivity   = sens_fit,
    verbose       = FALSE
  )
  swot_fit <<- swot_fit
  stopifnot(
    inherits(swot_fit, "climasus_swot"),
    is.data.frame(swot_fit$scores) || is.list(swot_fit)
  )
  swot_fit
})

# ── Teste 2: plot_swot | radar ────────────────────────────────────────────────
run_test("plot_swot | radar", {
  p <- sus_mod_plot_swot(swot_fit, type = "radar", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})
