# ── Grupo Q: sus_mod_dlnm + sus_mod_plot_dlnm ────────────────────────────────

# ── Teste 1: dlnm | single city ──────────────────────────────────────────────
run_test("dlnm | single city", {
  r <- fits_list[[1]]
  stopifnot(
    inherits(r, "climasus_dlnm"),
    all(c("model", "pred", "exposure_response", "lag_response", "diagnostics") %in% names(r)),
    inherits(r$model, "glm")
  )
  r
})

# ── Teste 2: plot_dlnm | overall ─────────────────────────────────────────────
run_test("plot_dlnm | overall", {
  p <- sus_mod_plot_dlnm(fits_list[[1]], type = "overall", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Teste 3: plot_dlnm | lag ──────────────────────────────────────────────────
run_test("plot_dlnm | lag", {
  p <- sus_mod_plot_dlnm(fits_list[[1]], type = "lag", verbose = FALSE)
  stopifnot(inherits(p, c("gg", "ggplot")))
  p
})

# ── Teste 4: plot_dlnm | table output ────────────────────────────────────────
run_test("plot_dlnm | table output", {
  tbl <- sus_mod_plot_dlnm(fits_list[[1]], type = "overall", output_type = "table",
                            verbose = FALSE)
  # gt retorna lista de gt_tbl; fallback retorna lista de tibbles
  # normalizar para o tibble exposure_response em ambos os casos
  if (is.data.frame(tbl)) {
    out <- tbl
  } else {
    out <- tbl$exposure_response
    if (!is.data.frame(out)) {
      # gt_tbl: extrair dados internos
      out <- as.data.frame(out)
    }
  }
  stopifnot(is.data.frame(out), nrow(out) > 0, "rr" %in% names(out))
  out
})
