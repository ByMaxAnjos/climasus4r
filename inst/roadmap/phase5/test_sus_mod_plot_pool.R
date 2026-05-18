# =============================================================================
# test_sus_mod_plot_pool.R
# 18-section test for sus_mod_plot_pool()
# Uses a hand-crafted climasus_pool mock object -- no mvmeta pipeline needed.
# Run with: devtools::load_all(); source("inst/roadmap/phase5/test_sus_mod_plot_pool.R")
# =============================================================================

devtools::load_all(quiet = TRUE)
library(tibble)

cat("=== test_sus_mod_plot_pool.R ===\n")
PASS <- 0L; FAIL <- 0L

ok <- function(cond, msg) {
  if (isTRUE(cond)) { cat("  PASS:", msg, "\n"); PASS <<- PASS + 1L }
  else               { cat("  FAIL:", msg, "\n"); FAIL <<- FAIL + 1L }
}

# -----------------------------------------------------------------------------
# Section 1 — Build mock climasus_pool
# -----------------------------------------------------------------------------
cat("\n--- Section 1: mock climasus_pool ---\n")

expo_seq   <- seq(15, 38, by = 0.5)
n_pts      <- length(expo_seq)
rr_vals    <- 1 + 0.015 * (expo_seq - 25)^2 / 100

mock_pool <- structure(
  list(
    mvmeta_fit       = NULL,
    pooled_pred      = NULL,
    exposure_response = tibble::tibble(
      pct      = c(0.05, 0.25, 0.50, 0.75, 0.95),
      exposure = c(15, 20, 25, 30, 35),
      rr       = c(1.05, 1.02, 1.00, 1.08, 1.15),
      lo       = c(0.95, 0.95, 0.98, 1.01, 1.05),
      hi       = c(1.15, 1.10, 1.02, 1.15, 1.28)
    ),
    exposure_curve = tibble::tibble(
      exposure = expo_seq,
      rr       = rr_vals,
      lo       = rr_vals - 0.05,
      hi       = rr_vals + 0.08
    ),
    lag_response = tibble::tibble(
      lag    = 0L:21L,
      rr_lag = c(1.05, 1.04, 1.03, 1.02, 1.01, rep(1.00, 17L)),
      lo     = c(1.00, 0.99, 0.98, 0.97, 0.96, rep(0.95, 17L)),
      hi     = c(1.10, 1.09, 1.08, 1.07, 1.06, rep(1.05, 17L))
    ),
    blup_preds = NULL,
    city_table = tibble::tibble(
      city    = c("fortaleza", "recife", "manaus"),
      n       = c(500L, 480L, 460L),
      rr      = c(1.12, 1.08, 1.05),
      lo      = c(1.05, 1.01, 0.98),
      hi      = c(1.20, 1.16, 1.13),
      blup_rr = c(1.10, 1.07, 1.06),
      blup_lo = c(1.04, 1.01, 1.00),
      blup_hi = c(1.17, 1.14, 1.13)
    ),
    heterogeneity = tibble::tibble(
      Q = 3.2, df = 2L, p_value = 0.20, i2 = 37.5
    ),
    meta = list(
      climate_col = "tmax",
      outcome_col = "n_obitos",
      lag_max     = 21L,
      n_cities    = 3L,
      city_names  = c("fortaleza", "recife", "manaus"),
      method      = "reml",
      argvar      = list(fun = "ns", df = 4L),
      arglag      = list(fun = "ns", df = 3L),
      ref_value   = 25.0,
      expo_grid   = expo_seq,
      pred_at     = c(0.05, 0.25, 0.50, 0.75, 0.95),
      call_time   = Sys.time()
    )
  ),
  class = c("climasus_pool", "list")
)

ok(inherits(mock_pool, "climasus_pool"), "mock object is climasus_pool")
ok(nrow(mock_pool$exposure_curve) == n_pts, "exposure_curve has correct nrow")
ok(nrow(mock_pool$city_table) == 3L, "city_table has 3 cities")


# -----------------------------------------------------------------------------
# Section 2 — overall plot (default)
# -----------------------------------------------------------------------------
cat("\n--- Section 2: overall plot ---\n")

p_ov <- sus_mod_plot_pool(mock_pool, type = "overall", lang = "pt")
ok(inherits(p_ov, "ggplot"), "overall returns ggplot object")


# -----------------------------------------------------------------------------
# Section 3 — overall plot layers
# -----------------------------------------------------------------------------
cat("\n--- Section 3: overall layers ---\n")

layer_classes_ov <- vapply(p_ov$layers, function(l) class(l$geom)[1L], character(1L))
ok("GeomRibbon" %in% layer_classes_ov, "overall has ribbon CI layer")
ok("GeomLine"   %in% layer_classes_ov, "overall has line layer")


# -----------------------------------------------------------------------------
# Section 4 — forest plot
# -----------------------------------------------------------------------------
cat("\n--- Section 4: forest plot ---\n")

p_forest <- sus_mod_plot_pool(mock_pool, type = "forest", lang = "en")
ok(inherits(p_forest, "ggplot"), "forest returns ggplot object")

layer_cls_f <- vapply(p_forest$layers, function(l) class(l$geom)[1L], character(1L))
ok("GeomPoint"    %in% layer_cls_f, "forest has point layer")
ok("GeomErrorbar" %in% layer_cls_f, "forest has errorbar layer")


# -----------------------------------------------------------------------------
# Section 5 — forest includes BLUP when available
# -----------------------------------------------------------------------------
cat("\n--- Section 5: forest BLUP layers ---\n")

# blup_rr is NOT NA in mock_pool$city_table
ok(!all(is.na(mock_pool$city_table$blup_rr)), "mock has BLUP values")
# forest plot should have extra (nudged) layers for BLUP
ok(length(p_forest$layers) >= 4L, "forest has 4+ layers when BLUP available")


# -----------------------------------------------------------------------------
# Section 6 — spaghetti falls back to overall when blup_preds = NULL
# -----------------------------------------------------------------------------
cat("\n--- Section 6: spaghetti fallback ---\n")

p_spag_fallback <- suppressWarnings(
  sus_mod_plot_pool(mock_pool, type = "spaghetti", lang = "pt")
)
ok(inherits(p_spag_fallback, "ggplot"), "spaghetti fallback returns ggplot when blup_preds=NULL")


# -----------------------------------------------------------------------------
# Section 7 — spaghetti with synthetic BLUP curves
# -----------------------------------------------------------------------------
cat("\n--- Section 7: spaghetti with mock BLUPs ---\n")

make_fake_crosspred <- function(expo_grid, rr_offset = 0) {
  rr <- 1 + 0.015 * (expo_grid - 25)^2 / 100 + rr_offset
  list(predvar = expo_grid, allRRfit = rr, allRRlow = rr - 0.05, allRRhigh = rr + 0.08)
}

mock_pool_blup <- mock_pool
mock_pool_blup$blup_preds <- list(
  fortaleza = make_fake_crosspred(expo_seq, 0.05),
  recife    = make_fake_crosspred(expo_seq, 0.02),
  manaus    = make_fake_crosspred(expo_seq, 0.00)
)

p_spag <- sus_mod_plot_pool(mock_pool_blup, type = "spaghetti", lang = "en")
ok(inherits(p_spag, "ggplot"), "spaghetti with BLUPs returns ggplot")

layer_cls_sp <- vapply(p_spag$layers, function(l) class(l$geom)[1L], character(1L))
ok(sum(layer_cls_sp == "GeomLine") >= 2L, "spaghetti has city + pooled line layers")


# -----------------------------------------------------------------------------
# Section 8 — output_type = "table"
# -----------------------------------------------------------------------------
cat("\n--- Section 8: output_type = 'table' ---\n")

tbl_ov <- sus_mod_plot_pool(mock_pool, type = "overall", output_type = "table")
ok(is.data.frame(tbl_ov), "table output is data.frame")
ok("exposure" %in% names(tbl_ov), "table has 'exposure' column")

tbl_fo <- sus_mod_plot_pool(mock_pool, type = "forest", output_type = "table")
ok("city" %in% names(tbl_fo), "forest table has 'city' column")


# -----------------------------------------------------------------------------
# Section 9 — output_type = "all"
# -----------------------------------------------------------------------------
cat("\n--- Section 9: output_type = 'all' ---\n")

all_out <- sus_mod_plot_pool(mock_pool, type = "overall", output_type = "all")
ok(is.list(all_out),                  "'all' returns list")
ok(inherits(all_out$plot, "ggplot"),  "'all' $plot is ggplot")
ok(is.data.frame(all_out$table),      "'all' $table is data.frame")


# -----------------------------------------------------------------------------
# Section 10 — interactive = TRUE
# -----------------------------------------------------------------------------
cat("\n--- Section 10: interactive = TRUE ---\n")

skip_plotly <- !requireNamespace("plotly", quietly = TRUE)
if (skip_plotly) {
  cat("  SKIP: plotly not installed\n")
} else {
  p_int <- sus_mod_plot_pool(mock_pool, type = "overall", interactive = TRUE)
  ok(inherits(p_int, "plotly"), "interactive overall is plotly object")
}


# -----------------------------------------------------------------------------
# Section 11 — lang = "en"
# -----------------------------------------------------------------------------
cat("\n--- Section 11: lang = 'en' ---\n")

p_en <- sus_mod_plot_pool(mock_pool, type = "overall", lang = "en")
ok(inherits(p_en, "ggplot"), "lang='en' produces ggplot")
ok(grepl("Pooled|Exposure", p_en$labels$title %||% ""), "title contains English text")


# -----------------------------------------------------------------------------
# Section 12 — lang = "es"
# -----------------------------------------------------------------------------
cat("\n--- Section 12: lang = 'es' ---\n")

p_es <- sus_mod_plot_pool(mock_pool, type = "forest", lang = "es")
ok(inherits(p_es, "ggplot"), "lang='es' produces ggplot")


# -----------------------------------------------------------------------------
# Section 13 — base_size
# -----------------------------------------------------------------------------
cat("\n--- Section 13: base_size ---\n")

p_bs <- sus_mod_plot_pool(mock_pool, type = "overall", base_size = 14L)
ok(p_bs$theme$text$size == 14L, "theme base_size is 14")


# -----------------------------------------------------------------------------
# Section 14 — verbose
# -----------------------------------------------------------------------------
cat("\n--- Section 14: verbose ---\n")

p_v  <- sus_mod_plot_pool(mock_pool, type = "overall", verbose = TRUE)
p_nv <- sus_mod_plot_pool(mock_pool, type = "forest",  verbose = FALSE)
ok(inherits(p_v,  "ggplot"), "verbose=TRUE returns ggplot")
ok(inherits(p_nv, "ggplot"), "verbose=FALSE returns ggplot")


# -----------------------------------------------------------------------------
# Section 15 — save_plot to tempfile
# -----------------------------------------------------------------------------
cat("\n--- Section 15: save_plot ---\n")

tmp <- tempfile(fileext = ".png")
sus_mod_plot_pool(mock_pool, type = "overall", save_plot = tmp)
ok(file.exists(tmp), "PNG saved to tempfile")
unlink(tmp)


# -----------------------------------------------------------------------------
# Section 16 — error on wrong input class
# -----------------------------------------------------------------------------
cat("\n--- Section 16: error on wrong class ---\n")

err_cls <- tryCatch(sus_mod_plot_pool(list(a = 1)), error = function(e) e)
ok(inherits(err_cls, "error"), "error on non-climasus_pool input")


# -----------------------------------------------------------------------------
# Section 17 — error when exposure_curve is NULL
# -----------------------------------------------------------------------------
cat("\n--- Section 17: error when exposure_curve = NULL ---\n")

mock_no_curve <- mock_pool
mock_no_curve$exposure_curve <- NULL

err_nc <- tryCatch(
  sus_mod_plot_pool(mock_no_curve, type = "overall"),
  error = function(e) e
)
ok(inherits(err_nc, "error"), "error when exposure_curve is NULL")


# -----------------------------------------------------------------------------
# Section 18 — unsupported lang fallback
# -----------------------------------------------------------------------------
cat("\n--- Section 18: unsupported lang fallback ---\n")

p_bad <- suppressWarnings(
  sus_mod_plot_pool(mock_pool, type = "overall", lang = "fr")
)
ok(inherits(p_bad, "ggplot"), "unsupported lang falls back to 'pt'")


# -----------------------------------------------------------------------------
cat(sprintf("\n=== RESULT: %d PASS / %d FAIL ===\n", PASS, FAIL))
if (FAIL > 0L) stop(sprintf("%d test(s) failed in test_sus_mod_plot_pool.R", FAIL))
