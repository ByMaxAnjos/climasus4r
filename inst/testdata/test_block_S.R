# === GRUPO S: sus_mod_excess (3 métodos) ===

city1_exact <- merged_exact[merged_exact$code_muni == top_cities[1], ]

# ── Teste 1: excess | from_dlnm ───────────────────────────────────────────────
run_test("excess | from_dlnm", {
  r <- sus_mod_excess(fits_list[[1]], method = "from_dlnm", verbose = FALSE)
  stopifnot(
    inherits(r, "climasus_excess"),
    all(c("daily", "total", "meta") %in% names(r)),
    is.data.frame(r$total)
  )
  r
})

# ── Teste 2: excess | spline ───────────────────────────────────────────────────
run_test("excess | spline", {
  r <- sus_mod_excess(city1_exact, outcome_col = "n_obitos", method = "spline",
                     dof_per_year = 4L, verbose = FALSE)
  stopifnot(
    inherits(r, "climasus_excess"),
    nrow(r$daily) > 0
  )
  r
})

# ── Teste 3: excess | serfling ─────────────────────────────────────────────────
run_test("excess | serfling", {
  r <- sus_mod_excess(city1_exact, outcome_col = "n_obitos", method = "serfling",
                     harmonics = 2L, verbose = FALSE)
  stopifnot(
    inherits(r, "climasus_excess"),
    "expected" %in% names(r$daily),
    "excess"   %in% names(r$daily)
  )
  r
})
