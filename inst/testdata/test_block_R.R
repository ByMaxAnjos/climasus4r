# === GRUPO R: sus_mod_af + sus_mod_plot_af ===

# ── Teste 1: af | single city ─────────────────────────────────────────────────
run_test("af | single city", {
  r <- af_list[[1]]
  stopifnot(
    inherits(r, "climasus_af"),
    all(c("total", "by_quantile", "meta") %in% names(r)),
    is.data.frame(r$total),
    nrow(r$total) > 0,
    all(c("component", "af", "an") %in% names(r$total))
  )
  r
})

# ── Teste 2: plot_af | bar ────────────────────────────────────────────────────
run_test("plot_af | bar", {
  p <- sus_mod_plot_af(af_list[[1]], type = "bar", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Teste 3: plot_af | forest ─────────────────────────────────────────────────
run_test("plot_af | forest", {
  p <- sus_mod_plot_af(af_list[[1]], type = "forest", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Teste 4: plot_af | table output ───────────────────────────────────────────
run_test("plot_af | table output", {
  tbl <- sus_mod_plot_af(af_list[[1]], type = "bar", output_type = "table", verbose = FALSE)
  stopifnot(is.data.frame(tbl), nrow(tbl) > 0)
  tbl
})
