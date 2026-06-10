# ── Grupo H: sus_grid_join() ──────────────────────────────────────────────────

# ── Teste 1: join básico por code_muni + date ──────────────────────────────────
run_test("grid_join | basic code_muni+date", {
  r <- sus_grid_join(health_df, monthly_df,
                     by      = c("code_muni", "date"),
                     verbose = FALSE)
  stopifnot(
    inherits(r, "climasus_df"),
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "agg",
    "rainfall_mm"      %in% names(r),
    "tair_dry_bulb_c"  %in% names(r),
    "n_obitos"         %in% names(r),
    nrow(r) == nrow(health_df)
  )
  r
})

# ── Teste 2: type_out substitui o type herdado do grid ────────────────────────
run_test("grid_join | type_out override", {
  r <- sus_grid_join(health_df, monthly_df,
                     type_out = "merged",
                     verbose  = FALSE)
  stopifnot(
    sus_meta(r, "type") == "merged"
  )
  r
})

# ── Teste 3: todas as colunas de health_df são preservadas ────────────────────
run_test("grid_join | colunas saude preservadas", {
  r <- sus_grid_join(health_df, monthly_df,
                     verbose = FALSE)
  health_cols <- setdiff(names(health_df), c(attr(health_df, "sf_column"), "geom"))
  stopifnot(
    all(health_cols %in% names(r))
  )
  r
})
