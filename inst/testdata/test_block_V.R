# === GRUPO V: sus_mod_pool + sus_mod_plot_pool ===

cli::cli_h2("Grupo V — sus_mod_pool + sus_mod_plot_pool")

# ── Teste 1: pool | multi-city BLUP ───────────────────────────────────────────
run_test("pool | multi-city BLUP", {
  pool_fit <<- sus_mod_pool(fits_list, blup = TRUE, verbose = FALSE)
  stopifnot(
    inherits(pool_fit, "climasus_pool"),
    all(c("overall_pred", "exposure_curve", "blup_preds", "meta") %in% names(pool_fit)),
    is.data.frame(pool_fit$exposure_curve),
    nrow(pool_fit$exposure_curve) > 0
  )
  pool_fit
})

# ── Teste 2: plot_pool | overall ──────────────────────────────────────────────
run_test("plot_pool | overall", {
  p <- sus_mod_plot_pool(pool_fit, type = "overall", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Teste 3: plot_pool | forest ───────────────────────────────────────────────
run_test("plot_pool | forest", {
  p <- sus_mod_plot_pool(pool_fit, type = "forest", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Teste 4: plot_pool | spaghetti ────────────────────────────────────────────
run_test("plot_pool | spaghetti", {
  p <- sus_mod_plot_pool(pool_fit, type = "spaghetti", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})
