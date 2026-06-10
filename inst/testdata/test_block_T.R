# === GRUPO T: sus_mod_casecrossover ===

# ── Teste 1: casecrossover | conditional_poisson ──────────────────────────────
run_test("casecrossover | conditional_poisson", {
  city1_exact <- merged_exact[merged_exact$code_muni == top_cities[1], ]
  r <- sus_mod_casecrossover(
    city1_exact,
    outcome_col  = "n_obitos",
    exposure_col = "tair_dry_bulb_c",
    stratum      = "month",
    method       = "conditional_poisson",
    verbose      = FALSE
  )
  stopifnot(
    inherits(r, "climasus_casecrossover"),
    all(c("model", "or_table", "diagnostics", "meta") %in% names(r)),
    is.data.frame(r$or_table),
    "or" %in% names(r$or_table)
  )
  r
})

# ── Teste 2: casecrossover | lag=3 ────────────────────────────────────────────
run_test("casecrossover | lag=3", {
  city1_exact <- merged_exact[merged_exact$code_muni == top_cities[1], ]
  r <- sus_mod_casecrossover(
    city1_exact,
    outcome_col  = "n_obitos",
    exposure_col = "tair_dry_bulb_c",
    lag          = 3L,
    verbose      = FALSE
  )
  stopifnot(
    inherits(r, "climasus_casecrossover"),
    r$meta$lag == 3L
  )
  r
})
