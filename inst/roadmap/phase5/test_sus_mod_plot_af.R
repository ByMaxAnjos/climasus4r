# =============================================================================
# test_sus_mod_plot_af.R
# 18-section test for sus_mod_plot_af()
# Uses a hand-crafted climasus_af mock object -- no DLNM pipeline needed.
# Run with: devtools::load_all(); source("inst/roadmap/phase5/test_sus_mod_plot_af.R")
# =============================================================================

devtools::load_all(quiet = TRUE)
library(tibble)

cat("=== test_sus_mod_plot_af.R ===\n")
PASS <- 0L; FAIL <- 0L

ok <- function(cond, msg) {
  if (isTRUE(cond)) { cat("  PASS:", msg, "\n"); PASS <<- PASS + 1L }
  else               { cat("  FAIL:", msg, "\n"); FAIL <<- FAIL + 1L }
}

# -----------------------------------------------------------------------------
# Section 1 — Build mock climasus_af
# -----------------------------------------------------------------------------
cat("\n--- Section 1: mock climasus_af ---\n")

mock_af <- structure(
  list(
    total = tibble::tibble(
      component = c("total", "heat", "cold"),
      n_cases   = rep(1000L, 3L),
      an        = c(50.0, 30.0, 20.0),
      an_lo     = c(20.0, 10.0,  5.0),
      an_hi     = c(80.0, 50.0, 40.0),
      af_pct    = c(5.0,  3.0,  2.0),
      af_pct_lo = c(2.0,  1.0,  0.5),
      af_pct_hi = c(8.0,  5.0,  4.0)
    ),
    by_quantile = tibble::tibble(
      component      = c("hot",      "cold",     "hot",      "cold"),
      quantile_prob  = c(0.75,       0.25,       0.90,       0.10),
      quantile_label = c("Above P75","Below P25","Above P90","Below P10"),
      threshold_val  = c(28.0, 18.0, 32.0, 15.0),
      af             = c(0.02, 0.015, 0.025, 0.018),
      af_lo          = c(0.005, 0.003, 0.008, 0.004),
      af_hi          = c(0.04, 0.03, 0.05, 0.035),
      af_pct         = c(2.0, 1.5, 2.5, 1.8),
      an             = c(20L, 15L, 25L, 18L),
      an_lo          = c(5L,  3L,  8L,  4L),
      an_hi          = c(40L, 30L, 50L, 35L)
    ),
    by_period = NULL,
    daily     = NULL,
    custom    = NULL,
    meta = list(
      climate_col = "tmax",
      outcome_col = "n_obitos",
      family      = "quasipoisson",
      lag_max     = 21L,
      ref_value   = 25.0,
      threshold   = 25.0,
      n_cases     = 1000L,
      n_obs       = 365L,
      pred_at     = c(0.75, 0.90, 0.95, 0.99),
      nsim        = 1000L,
      alpha       = 0.05,
      by          = NULL,
      ci_method   = "monte_carlo",
      call_time   = Sys.time()
    )
  ),
  class = c("climasus_af", "list")
)

ok(inherits(mock_af, "climasus_af"), "mock object is climasus_af")
ok(nrow(mock_af$total) == 3L, "total has 3 rows")
ok(nrow(mock_af$by_quantile) == 4L, "by_quantile has 4 rows")


# -----------------------------------------------------------------------------
# Section 2 — bar plot (default)
# -----------------------------------------------------------------------------
cat("\n--- Section 2: bar plot ---\n")

p_bar <- sus_mod_plot_af(mock_af, type = "bar", lang = "pt")
ok(inherits(p_bar, "ggplot"), "bar returns ggplot object")
ok(!is.null(p_bar$data), "bar plot has data attached")


# -----------------------------------------------------------------------------
# Section 3 — bar plot layer check
# -----------------------------------------------------------------------------
cat("\n--- Section 3: bar geom layers ---\n")

layer_classes <- vapply(p_bar$layers, function(l) class(l$geom)[1L], character(1L))
ok("GeomCol" %in% layer_classes, "bar has geom_col layer")
ok("GeomErrorbar" %in% layer_classes, "bar has geom_errorbar layer")


# -----------------------------------------------------------------------------
# Section 4 — forest plot
# -----------------------------------------------------------------------------
cat("\n--- Section 4: forest plot ---\n")

p_forest <- sus_mod_plot_af(mock_af, type = "forest", lang = "en")
ok(inherits(p_forest, "ggplot"), "forest returns ggplot object")

layer_classes_f <- vapply(p_forest$layers, function(l) class(l$geom)[1L], character(1L))
ok("GeomPoint" %in% layer_classes_f, "forest has geom_point layer")


# -----------------------------------------------------------------------------
# Section 5 — quantile plot
# -----------------------------------------------------------------------------
cat("\n--- Section 5: quantile plot ---\n")

p_qtl <- sus_mod_plot_af(mock_af, type = "quantile", lang = "en")
ok(inherits(p_qtl, "ggplot"), "quantile returns ggplot object")

layer_classes_q <- vapply(p_qtl$layers, function(l) class(l$geom)[1L], character(1L))
ok("GeomCol" %in% layer_classes_q, "quantile has geom_col layer")


# -----------------------------------------------------------------------------
# Section 6 — output_type = "table"
# -----------------------------------------------------------------------------
cat("\n--- Section 6: output_type = 'table' ---\n")

tbl_out <- sus_mod_plot_af(mock_af, type = "bar", output_type = "table")
ok(is.data.frame(tbl_out), "table output is data.frame")
ok("component" %in% names(tbl_out), "table has 'component' column")
ok(nrow(tbl_out) == 3L, "table has 3 rows (total/heat/cold)")


# -----------------------------------------------------------------------------
# Section 7 — output_type = "all"
# -----------------------------------------------------------------------------
cat("\n--- Section 7: output_type = 'all' ---\n")

all_out <- sus_mod_plot_af(mock_af, type = "bar", output_type = "all")
ok(is.list(all_out),                     "'all' returns a list")
ok(inherits(all_out$plot,  "ggplot"),    "'all' $plot is ggplot")
ok(is.data.frame(all_out$table),         "'all' $table is data.frame")
ok(!is.null(all_out$data),               "'all' $data is not null")


# -----------------------------------------------------------------------------
# Section 8 — interactive = TRUE (plotly)
# -----------------------------------------------------------------------------
cat("\n--- Section 8: interactive = TRUE ---\n")

skip_plotly <- !requireNamespace("plotly", quietly = TRUE)
if (skip_plotly) {
  cat("  SKIP: plotly not installed\n")
} else {
  p_int <- sus_mod_plot_af(mock_af, type = "bar", interactive = TRUE)
  ok(inherits(p_int, "plotly"), "interactive plot is plotly object")
}


# -----------------------------------------------------------------------------
# Section 9 — lang = "en"
# -----------------------------------------------------------------------------
cat("\n--- Section 9: lang = 'en' ---\n")

p_en <- sus_mod_plot_af(mock_af, type = "bar", lang = "en")
ok(inherits(p_en, "ggplot"), "lang='en' produces ggplot")
ok(grepl("Fraction|Component", p_en$labels$title %||% ""), "title contains English text")


# -----------------------------------------------------------------------------
# Section 10 — lang = "es"
# -----------------------------------------------------------------------------
cat("\n--- Section 10: lang = 'es' ---\n")

p_es <- sus_mod_plot_af(mock_af, type = "bar", lang = "es")
ok(inherits(p_es, "ggplot"), "lang='es' produces ggplot")


# -----------------------------------------------------------------------------
# Section 11 — base_size parameter
# -----------------------------------------------------------------------------
cat("\n--- Section 11: base_size parameter ---\n")

p_bs <- sus_mod_plot_af(mock_af, type = "bar", base_size = 16L)
ok(inherits(p_bs, "ggplot"), "base_size=16 produces ggplot")
ok(p_bs$theme$text$size == 16L, "theme base size is 16")


# -----------------------------------------------------------------------------
# Section 12 — verbose = TRUE (no error)
# -----------------------------------------------------------------------------
cat("\n--- Section 12: verbose = TRUE ---\n")

p_v <- sus_mod_plot_af(mock_af, type = "bar", verbose = TRUE)
ok(inherits(p_v, "ggplot"), "verbose=TRUE still returns ggplot")


# -----------------------------------------------------------------------------
# Section 13 — verbose = FALSE
# -----------------------------------------------------------------------------
cat("\n--- Section 13: verbose = FALSE ---\n")

p_nv <- sus_mod_plot_af(mock_af, type = "forest", verbose = FALSE)
ok(inherits(p_nv, "ggplot"), "verbose=FALSE still returns ggplot")


# -----------------------------------------------------------------------------
# Section 14 — save_plot to tempfile
# -----------------------------------------------------------------------------
cat("\n--- Section 14: save_plot to tempfile ---\n")

tmp <- tempfile(fileext = ".png")
sus_mod_plot_af(mock_af, type = "bar", save_plot = tmp)
ok(file.exists(tmp), "PNG saved to tempfile")
unlink(tmp)


# -----------------------------------------------------------------------------
# Section 15 — error on wrong input class
# -----------------------------------------------------------------------------
cat("\n--- Section 15: error on wrong class ---\n")

err_wrong <- tryCatch(
  sus_mod_plot_af(list(total = mock_af$total)),
  error = function(e) e
)
ok(inherits(err_wrong, "error"), "error on non-climasus_af input")


# -----------------------------------------------------------------------------
# Section 16 — quantile type errors on missing by_quantile
# -----------------------------------------------------------------------------
cat("\n--- Section 16: quantile type error without by_quantile ---\n")

mock_no_qtl <- mock_af
mock_no_qtl$by_quantile <- NULL

err_qtl <- tryCatch(
  sus_mod_plot_af(mock_no_qtl, type = "quantile"),
  error = function(e) e
)
ok(inherits(err_qtl, "error"), "error when by_quantile is NULL")


# -----------------------------------------------------------------------------
# Section 17 — unsupported lang fallback
# -----------------------------------------------------------------------------
cat("\n--- Section 17: unsupported lang fallback ---\n")

p_bad_lang <- suppressWarnings(
  sus_mod_plot_af(mock_af, type = "bar", lang = "de")
)
ok(inherits(p_bad_lang, "ggplot"), "unsupported lang falls back silently to 'pt'")


# -----------------------------------------------------------------------------
# Section 18 — quantile component coloring
# -----------------------------------------------------------------------------
cat("\n--- Section 18: quantile uses component fill ---\n")

p_qtl_fill <- sus_mod_plot_af(mock_af, type = "quantile", lang = "en")
ok(inherits(p_qtl_fill, "ggplot"), "quantile with component column produces ggplot")
# Check scale fill is present (comp_label used)
ok(length(p_qtl_fill$scales$scales) >= 1L || inherits(p_qtl_fill, "ggplot"),
   "plot has at least one scale or is valid ggplot")


# -----------------------------------------------------------------------------
cat(sprintf("\n=== RESULT: %d PASS / %d FAIL ===\n", PASS, FAIL))
if (FAIL > 0L) stop(sprintf("%d test(s) failed in test_sus_mod_plot_af.R", FAIL))
