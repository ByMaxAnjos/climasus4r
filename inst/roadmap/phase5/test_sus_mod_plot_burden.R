# =============================================================================
# test_sus_mod_plot_burden.R
# 18-section test for sus_mod_plot_burden()
# Uses a hand-crafted climasus_burden mock object -- no AF pipeline needed.
# Run with: devtools::load_all(); source("inst/roadmap/phase5/test_sus_mod_plot_burden.R")
# =============================================================================

devtools::load_all(quiet = TRUE)
library(tibble)

cat("=== test_sus_mod_plot_burden.R ===\n")
PASS <- 0L; FAIL <- 0L

ok <- function(cond, msg) {
  if (isTRUE(cond)) { cat("  PASS:", msg, "\n"); PASS <<- PASS + 1L }
  else               { cat("  FAIL:", msg, "\n"); FAIL <<- FAIL + 1L }
}

# -----------------------------------------------------------------------------
# Section 1 — Build mock climasus_burden (AF input, component = "all")
# -----------------------------------------------------------------------------
cat("\n--- Section 1: mock climasus_burden ---\n")

cities <- c("fortaleza", "recife", "manaus", "belem", "salvador")
n_city <- length(cities)

burden_tbl <- do.call(rbind, lapply(seq_along(cities), function(i) {
  data.frame(
    city        = cities[i],
    component   = c("total", "heat", "cold"),
    n_cases     = rep(c(2000, 1500, 1200, 900, 800)[i], 3L),
    an          = c(120, 80, 40, 90, 60, 30, 70, 45, 25, 50, 30, 20, 40, 25, 15)[
      ((i-1)*3+1):(i*3)],
    an_lo       = c(60, 30, 10, 45, 25, 10, 30, 20, 5, 20, 10, 5, 15, 8, 3)[
      ((i-1)*3+1):(i*3)],
    an_hi       = c(180, 130, 70, 135, 95, 50, 110, 70, 45, 80, 50, 35, 65, 42, 27)[
      ((i-1)*3+1):(i*3)],
    af_pct      = c(6.0, 4.0, 2.0, 6.0, 4.0, 2.0, 5.8, 3.75, 2.1, 5.6, 3.3, 2.2, 5.0, 3.1, 1.9)[
      ((i-1)*3+1):(i*3)],
    af_pct_lo   = rep(1.0, 3L),
    af_pct_hi   = rep(9.0, 3L),
    rank        = rep(i, 3L),
    pct_of_total = rep(c(32.4, 24.3, 18.9, 13.5, 10.8)[i], 3L),
    stringsAsFactors = FALSE
  )
}))

conc_pct <- c(32.4, 24.3, 18.9, 13.5, 10.8)

mock_burden <- structure(
  list(
    burden_table  = tibble::as_tibble(burden_tbl),
    concentration = tibble::tibble(
      city           = cities,
      rank           = seq_len(n_city),
      pct_of_total   = conc_pct,
      cumulative_pct = cumsum(conc_pct)
    ),
    total_burden = list(
      an_total    = 370L,
      af_pct_avg  = 5.7,
      top_city    = "fortaleza",
      top_city_an = 120L
    ),
    meta = list(
      input_type = "climasus_af",
      component  = "all",
      rank_by    = "an",
      n_cities   = n_city,
      top_n      = n_city,
      nsim       = 1000L,
      alpha      = 0.05,
      call_time  = Sys.time()
    )
  ),
  class = c("climasus_burden", "list")
)

ok(inherits(mock_burden, "climasus_burden"), "mock object is climasus_burden")
ok(nrow(mock_burden$burden_table) == n_city * 3L, "burden_table has 15 rows (5 cities x 3 components)")
ok(nrow(mock_burden$concentration) == n_city, "concentration has 5 rows")


# -----------------------------------------------------------------------------
# Section 2 — lollipop plot (default)
# -----------------------------------------------------------------------------
cat("\n--- Section 2: lollipop plot ---\n")

p_lolli <- sus_mod_plot_burden(mock_burden, type = "lollipop", lang = "pt")
ok(inherits(p_lolli, "ggplot"), "lollipop returns ggplot object")


# -----------------------------------------------------------------------------
# Section 3 — lollipop layers
# -----------------------------------------------------------------------------
cat("\n--- Section 3: lollipop layers ---\n")

layer_cls_l <- vapply(p_lolli$layers, function(l) class(l$geom)[1L], character(1L))
ok("GeomSegment" %in% layer_cls_l, "lollipop has geom_segment layer")
ok("GeomPoint"   %in% layer_cls_l, "lollipop has geom_point layer")


# -----------------------------------------------------------------------------
# Section 4 — lollipop uses total component only
# -----------------------------------------------------------------------------
cat("\n--- Section 4: lollipop total rows ---\n")

tbl_lolli <- sus_mod_plot_burden(mock_burden, type = "lollipop", output_type = "table")
ok(is.data.frame(tbl_lolli), "lollipop table is data.frame")
ok(all(tbl_lolli$component == "total"), "lollipop table has only 'total' component rows")
ok(nrow(tbl_lolli) == n_city, "lollipop table has one row per city")


# -----------------------------------------------------------------------------
# Section 5 — lorenz plot
# -----------------------------------------------------------------------------
cat("\n--- Section 5: lorenz plot ---\n")

p_lorenz <- sus_mod_plot_burden(mock_burden, type = "lorenz", lang = "en")
ok(inherits(p_lorenz, "ggplot"), "lorenz returns ggplot object")

layer_cls_lo <- vapply(p_lorenz$layers, function(l) class(l$geom)[1L], character(1L))
ok("GeomArea" %in% layer_cls_lo, "lorenz has geom_area shading")
ok("GeomLine" %in% layer_cls_lo, "lorenz has geom_line layer")


# -----------------------------------------------------------------------------
# Section 6 — lorenz table is concentration tibble
# -----------------------------------------------------------------------------
cat("\n--- Section 6: lorenz table ---\n")

tbl_lorenz <- sus_mod_plot_burden(mock_burden, type = "lorenz", output_type = "table")
ok(is.data.frame(tbl_lorenz), "lorenz table is data.frame")
ok("cumulative_pct" %in% names(tbl_lorenz), "lorenz table has 'cumulative_pct'")


# -----------------------------------------------------------------------------
# Section 7 — stacked plot (AF + all component)
# -----------------------------------------------------------------------------
cat("\n--- Section 7: stacked plot ---\n")

p_stack <- sus_mod_plot_burden(mock_burden, type = "stacked", lang = "pt")
ok(inherits(p_stack, "ggplot"), "stacked returns ggplot object")

layer_cls_st <- vapply(p_stack$layers, function(l) class(l$geom)[1L], character(1L))
ok("GeomCol" %in% layer_cls_st, "stacked has geom_col layer")


# -----------------------------------------------------------------------------
# Section 8 — stacked table has heat and cold rows only
# -----------------------------------------------------------------------------
cat("\n--- Section 8: stacked table ---\n")

tbl_stack <- sus_mod_plot_burden(mock_burden, type = "stacked", output_type = "table")
ok(all(tbl_stack$component %in% c("heat", "cold")), "stacked table has only heat/cold rows")
ok(nrow(tbl_stack) == n_city * 2L, "stacked table has 2 rows per city")


# -----------------------------------------------------------------------------
# Section 9 — stacked error for excess input
# -----------------------------------------------------------------------------
cat("\n--- Section 9: stacked error for excess input ---\n")

mock_excess_burden <- mock_burden
mock_excess_burden$meta$input_type <- "climasus_excess"

err_stack <- tryCatch(
  sus_mod_plot_burden(mock_excess_burden, type = "stacked"),
  error = function(e) e
)
ok(inherits(err_stack, "error"), "stacked errors on excess input type")


# -----------------------------------------------------------------------------
# Section 10 — output_type = "all"
# -----------------------------------------------------------------------------
cat("\n--- Section 10: output_type = 'all' ---\n")

all_out <- sus_mod_plot_burden(mock_burden, type = "lollipop", output_type = "all")
ok(is.list(all_out),                  "'all' returns list")
ok(inherits(all_out$plot, "ggplot"),  "'all' $plot is ggplot")
ok(is.data.frame(all_out$table),      "'all' $table is data.frame")
ok(!is.null(all_out$data),            "'all' $data is not null")


# -----------------------------------------------------------------------------
# Section 11 — interactive = TRUE
# -----------------------------------------------------------------------------
cat("\n--- Section 11: interactive = TRUE ---\n")

skip_plotly <- !requireNamespace("plotly", quietly = TRUE)
if (skip_plotly) {
  cat("  SKIP: plotly not installed\n")
} else {
  p_int <- sus_mod_plot_burden(mock_burden, type = "lollipop", interactive = TRUE)
  ok(inherits(p_int, "plotly"), "interactive lollipop is plotly object")
}


# -----------------------------------------------------------------------------
# Section 12 — lang = "en"
# -----------------------------------------------------------------------------
cat("\n--- Section 12: lang = 'en' ---\n")

p_en <- sus_mod_plot_burden(mock_burden, type = "lollipop", lang = "en")
ok(inherits(p_en, "ggplot"), "lang='en' produces ggplot")
ok(grepl("Burden|City|Disease", p_en$labels$title %||% ""), "title contains English text")


# -----------------------------------------------------------------------------
# Section 13 — lang = "es"
# -----------------------------------------------------------------------------
cat("\n--- Section 13: lang = 'es' ---\n")

p_es <- sus_mod_plot_burden(mock_burden, type = "lorenz", lang = "es")
ok(inherits(p_es, "ggplot"), "lang='es' produces ggplot")


# -----------------------------------------------------------------------------
# Section 14 — base_size
# -----------------------------------------------------------------------------
cat("\n--- Section 14: base_size ---\n")

p_bs <- sus_mod_plot_burden(mock_burden, type = "lollipop", base_size = 14L)
ok(p_bs$theme$text$size == 14L, "theme base_size is 14")


# -----------------------------------------------------------------------------
# Section 15 — verbose
# -----------------------------------------------------------------------------
cat("\n--- Section 15: verbose ---\n")

p_v  <- sus_mod_plot_burden(mock_burden, type = "lollipop", verbose = TRUE)
p_nv <- sus_mod_plot_burden(mock_burden, type = "lorenz",   verbose = FALSE)
ok(inherits(p_v,  "ggplot"), "verbose=TRUE returns ggplot")
ok(inherits(p_nv, "ggplot"), "verbose=FALSE returns ggplot")


# -----------------------------------------------------------------------------
# Section 16 — save_plot to tempfile
# -----------------------------------------------------------------------------
cat("\n--- Section 16: save_plot ---\n")

tmp <- tempfile(fileext = ".png")
sus_mod_plot_burden(mock_burden, type = "lollipop", save_plot = tmp)
ok(file.exists(tmp), "PNG saved to tempfile")
unlink(tmp)


# -----------------------------------------------------------------------------
# Section 17 — error on wrong input class
# -----------------------------------------------------------------------------
cat("\n--- Section 17: error on wrong class ---\n")

err_cls <- tryCatch(sus_mod_plot_burden(list(a = 1)), error = function(e) e)
ok(inherits(err_cls, "error"), "error on non-climasus_burden input")


# -----------------------------------------------------------------------------
# Section 18 — unsupported lang fallback
# -----------------------------------------------------------------------------
cat("\n--- Section 18: unsupported lang fallback ---\n")

p_bad <- suppressWarnings(
  sus_mod_plot_burden(mock_burden, type = "lollipop", lang = "jp")
)
ok(inherits(p_bad, "ggplot"), "unsupported lang falls back to 'pt'")


# -----------------------------------------------------------------------------
cat(sprintf("\n=== RESULT: %d PASS / %d FAIL ===\n", PASS, FAIL))
if (FAIL > 0L) stop(sprintf("%d test(s) failed in test_sus_mod_plot_burden.R", FAIL))
