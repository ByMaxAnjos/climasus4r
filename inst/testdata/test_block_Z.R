# === GRUPO Z: sus_mod_vulnerability_index + sus_mod_plot_vulnerability ===

cli::cli_h2("Grupo Z — sus_mod_vulnerability_index + sus_mod_plot_vulnerability")

# ── Teste 1: vulnerability_index | from dlnm fits ────────────────────────────
run_test("vulnerability_index | from dlnm fits", {
  vi_fit <- sus_mod_vulnerability_index(fits = fits_list, verbose = FALSE)
  vi_fit <<- vi_fit
  stopifnot(
    inherits(vi_fit, "climasus_vi"),
    all(c("vi_table", "pillar_scores", "meta") %in% names(vi_fit)),
    is.data.frame(vi_fit$vi_table),
    all(c("city", "vi_score", "vi_rank") %in% names(vi_fit$vi_table))
  )
  vi_fit
})

# ── Teste 2: vulnerability_index | with exposure pillar ──────────────────────
run_test("vulnerability_index | with exposure pillar", {
  expo_df <- data.frame(
    city        = top_cities,
    tair_mean   = c(28.5, 30.1, 27.0),
    n_heatwaves = c(5L, 8L, 3L)
  )
  vi2 <- sus_mod_vulnerability_index(
    exposure_df = expo_df,
    fits        = fits_list,
    verbose     = FALSE
  )
  stopifnot(
    inherits(vi2, "climasus_vi"),
    nrow(vi2$vi_table) == length(top_cities)
  )
  vi2
})

# ── Teste 3: plot_vulnerability | ranking ─────────────────────────────────────
run_test("plot_vulnerability | ranking", {
  p <- sus_mod_plot_vulnerability(vi_fit, type = "ranking", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})
