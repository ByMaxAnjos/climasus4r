# === GRUPO U: sus_mod_its (Interrupted Time Series) ===

city1_exact <- merged_exact[merged_exact$code_muni == top_cities[1], ]

# ── Teste 1: its | single interruption with counterfactual ────────────────────
run_test("its | single interruption with counterfactual", {
  r <- sus_mod_its(city1_exact, outcome_col = "n_obitos",
    interruption_dates = as.Date("2022-06-01"),
    harmonics = 2L, counterfactual = TRUE, verbose = FALSE)
  stopifnot(
    inherits(r, "climasus_its"),
    all(c("model", "effects_table", "segments", "meta") %in% names(r)),
    is.data.frame(r$effects_table),
    nrow(r$effects_table) > 0
  )
  r
})

# ── Teste 2: its | two interruptions no counterfactual ────────────────────────
run_test("its | two interruptions no counterfactual", {
  r <- sus_mod_its(city1_exact, outcome_col = "n_obitos",
    interruption_dates = as.Date(c("2022-04-01", "2022-09-01")),
    counterfactual = FALSE, verbose = FALSE)
  stopifnot(
    inherits(r, "climasus_its"),
    length(r$segments) == 3L  # 3 segmentos para 2 interrupções
  )
  r
})
