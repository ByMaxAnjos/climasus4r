# === GRUPO W: sus_mod_sensitivity + sus_mod_plot_sensitivity ===

# ── Teste 1: sensitivity | stratified ────────────────────────────────────────
run_test("sensitivity | stratified", {
  sens_fit <- sus_mod_sensitivity(fits_list, verbose = FALSE)
  sens_fit <<- sens_fit
  stopifnot(
    inherits(sens_fit, "climasus_sensitivity"),
    all(c("rr_table", "comparison", "stratum_curves", "meta") %in% names(sens_fit)),
    is.data.frame(sens_fit$comparison),
    all(c("stratum", "hot_rr", "cold_rr", "sensitivity_index") %in% names(sens_fit$comparison))
  )
  sens_fit
})

# ── Teste 2: plot_sensitivity | curves ────────────────────────────────────────
run_test("plot_sensitivity | curves", {
  p <- sus_mod_plot_sensitivity(sens_fit, type = "curves", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Teste 3: plot_sensitivity | scatter ───────────────────────────────────────
run_test("plot_sensitivity | scatter", {
  p <- sus_mod_plot_sensitivity(sens_fit, type = "scatter", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Teste 4: plot_sensitivity | table output ──────────────────────────────────
run_test("plot_sensitivity | table output", {
  tbl <- sus_mod_plot_sensitivity(sens_fit, type = "bar", output_type = "table", verbose = FALSE)
  stopifnot(is.data.frame(tbl), nrow(tbl) > 0)
  tbl
})
