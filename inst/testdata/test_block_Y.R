# === GRUPO Y: sus_mod_burden + sus_mod_plot_burden ===

# ── Teste 1: burden | from af_list ────────────────────────────────────────────
run_test("burden | from af_list", {
  burd_fit <- sus_mod_burden(af_list, verbose = FALSE)
  burd_fit <<- burd_fit
  stopifnot(
    inherits(burd_fit, "climasus_burden"),
    all(c("burden_table", "total_burden", "concentration", "meta") %in% names(burd_fit)),
    is.data.frame(burd_fit$burden_table),
    all(c("city", "an", "af_pct", "rank") %in% names(burd_fit$burden_table))
  )
  burd_fit
})

# ── Teste 2: plot_burden | lollipop ───────────────────────────────────────────
run_test("plot_burden | lollipop", {
  p <- sus_mod_plot_burden(burd_fit, type = "lollipop", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Teste 3: plot_burden | lorenz ─────────────────────────────────────────────
run_test("plot_burden | lorenz", {
  p <- sus_mod_plot_burden(burd_fit, type = "lorenz", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})
