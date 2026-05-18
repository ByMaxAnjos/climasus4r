# =============================================================================
# test_sus_mod_plot_sensitivity.R
# 18-section test for sus_mod_plot_sensitivity()
# Uses a hand-crafted climasus_sensitivity mock object -- no DLNM pipeline needed.
# Run with: devtools::load_all(); source("inst/roadmap/phase5/test_sus_mod_plot_sensitivity.R")
# =============================================================================

devtools::load_all(quiet = TRUE)
library(tibble)

cat("=== test_sus_mod_plot_sensitivity.R ===\n")
PASS <- 0L; FAIL <- 0L

ok <- function(cond, msg) {
  if (isTRUE(cond)) { cat("  PASS:", msg, "\n"); PASS <<- PASS + 1L }
  else               { cat("  FAIL:", msg, "\n"); FAIL <<- FAIL + 1L }
}

# -----------------------------------------------------------------------------
# Section 1 -- Build mock climasus_sensitivity
# -----------------------------------------------------------------------------
cat("\n--- Section 1: mock climasus_sensitivity ---\n")

expo_seq <- seq(15, 38, by = 0.5)
n_pts    <- length(expo_seq)

make_curve <- function(stratum_nm, label_nm, offset = 0) {
  rr <- 1 + 0.015 * (expo_seq - 25)^2 / 100 + offset
  tibble::tibble(
    stratum  = stratum_nm,
    label    = label_nm,
    exposure = expo_seq,
    rr       = rr,
    rr_lo    = rr - 0.05,
    rr_hi    = rr + 0.08
  )
}

mock_stratum_curves <- dplyr::bind_rows(
  make_curve("elderly", "Elderly", 0.05),
  make_curve("male",    "Male",    0.02),
  make_curve("female",  "Female",  0.00)
)

mock_rr_table <- tibble::tibble(
  stratum       = c("elderly", "elderly", "male",   "male",   "female", "female"),
  label         = c("Elderly", "Elderly", "Male",   "Male",   "Female", "Female"),
  component     = c("hot",     "cold",    "hot",    "cold",   "hot",    "cold"),
  quantile_prob = c(0.95,      0.05,      0.95,     0.05,     0.95,     0.05),
  exposure      = c(35.0,      17.0,      34.5,     17.5,     34.0,     18.0),
  rr            = c(1.20,      1.15,      1.12,     1.08,     1.06,     1.04),
  rr_lo         = c(1.10,      1.05,      1.04,     1.01,     0.99,     0.98),
  rr_hi         = c(1.32,      1.27,      1.22,     1.16,     1.14,     1.11),
  ref_exposure  = rep(25.0, 6L)
)

mock_comparison <- tibble::tibble(
  stratum           = c("elderly", "male",   "female"),
  label             = c("Elderly", "Male",   "Female"),
  hot_rr            = c(1.20,      1.12,     1.06),
  hot_rr_lo         = c(1.10,      1.04,     0.99),
  hot_rr_hi         = c(1.32,      1.22,     1.14),
  cold_rr           = c(1.15,      1.08,     1.04),
  cold_rr_lo        = c(1.05,      1.01,     0.98),
  cold_rr_hi        = c(1.27,      1.16,     1.11),
  hot_log_rr        = log(c(1.20,  1.12,     1.06)),
  cold_log_rr       = log(c(1.15,  1.08,     1.04)),
  sensitivity_index = log(c(1.20,  1.12,     1.06)) + log(c(1.15, 1.08, 1.04)),
  hot_rank          = c(1L, 2L, 3L),
  cold_rank         = c(1L, 2L, 3L)
)

mock_sens <- structure(
  list(
    rr_table       = mock_rr_table,
    comparison     = mock_comparison,
    stratum_curves = mock_stratum_curves,
    meta           = list(
      climate_col     = "tmax",
      n_strata        = 3L,
      stratum_names   = c("elderly", "male", "female"),
      stratum_labels  = c(elderly = "Elderly", male = "Male", female = "Female"),
      hot_percentile  = 0.95,
      cold_percentile = 0.05,
      alpha           = 0.05,
      call_time       = Sys.time()
    )
  ),
  class = c("climasus_sensitivity", "list")
)

ok(inherits(mock_sens, "climasus_sensitivity"), "mock is climasus_sensitivity")
ok(nrow(mock_sens$stratum_curves) == n_pts * 3L, "stratum_curves has correct nrow")
ok(nrow(mock_sens$comparison) == 3L, "comparison has 3 rows")
ok(nrow(mock_sens$rr_table) == 6L, "rr_table has 6 rows")


# -----------------------------------------------------------------------------
# Section 2 -- curves type (default)
# -----------------------------------------------------------------------------
cat("\n--- Section 2: curves type ---\n")

p_curves <- sus_mod_plot_sensitivity(mock_sens, type = "curves", lang = "pt")
ok(inherits(p_curves, "ggplot"), "curves returns ggplot object")


# -----------------------------------------------------------------------------
# Section 3 -- curves plot layers
# -----------------------------------------------------------------------------
cat("\n--- Section 3: curves layers ---\n")

layer_cls_cv <- vapply(p_curves$layers, function(l) class(l$geom)[1L], character(1L))
ok("GeomRibbon" %in% layer_cls_cv, "curves has ribbon CI layer")
ok("GeomLine"   %in% layer_cls_cv, "curves has line layer")


# -----------------------------------------------------------------------------
# Section 4 -- scatter type
# -----------------------------------------------------------------------------
cat("\n--- Section 4: scatter type ---\n")

p_scatter <- sus_mod_plot_sensitivity(mock_sens, type = "scatter", lang = "en")
ok(inherits(p_scatter, "ggplot"), "scatter returns ggplot object")


# -----------------------------------------------------------------------------
# Section 5 -- scatter plot layers
# -----------------------------------------------------------------------------
cat("\n--- Section 5: scatter layers ---\n")

layer_cls_sc <- vapply(p_scatter$layers, function(l) class(l$geom)[1L], character(1L))
ok("GeomPoint" %in% layer_cls_sc, "scatter has point layer")
ok("GeomText"  %in% layer_cls_sc, "scatter has text label layer")


# -----------------------------------------------------------------------------
# Section 6 -- bar type
# -----------------------------------------------------------------------------
cat("\n--- Section 6: bar type ---\n")

p_bar <- sus_mod_plot_sensitivity(mock_sens, type = "bar", lang = "es")
ok(inherits(p_bar, "ggplot"), "bar returns ggplot object")


# -----------------------------------------------------------------------------
# Section 7 -- bar plot layers
# -----------------------------------------------------------------------------
cat("\n--- Section 7: bar layers ---\n")

layer_cls_br <- vapply(p_bar$layers, function(l) class(l$geom)[1L], character(1L))
ok("GeomErrorbar" %in% layer_cls_br, "bar has errorbar layer")
ok("GeomPoint"    %in% layer_cls_br, "bar has point layer")


# -----------------------------------------------------------------------------
# Section 8 -- output_type = "table"
# -----------------------------------------------------------------------------
cat("\n--- Section 8: output_type = 'table' ---\n")

tbl_cv <- sus_mod_plot_sensitivity(mock_sens, type = "curves",  output_type = "table")
ok(is.data.frame(tbl_cv),             "curves table is data.frame")
ok("exposure" %in% names(tbl_cv),     "curves table has 'exposure' column")

tbl_sc <- sus_mod_plot_sensitivity(mock_sens, type = "scatter", output_type = "table")
ok("sensitivity_index" %in% names(tbl_sc), "scatter table has 'sensitivity_index' column")

tbl_br <- sus_mod_plot_sensitivity(mock_sens, type = "bar",     output_type = "table")
ok("component" %in% names(tbl_br),    "bar table has 'component' column")


# -----------------------------------------------------------------------------
# Section 9 -- output_type = "all"
# -----------------------------------------------------------------------------
cat("\n--- Section 9: output_type = 'all' ---\n")

all_out <- sus_mod_plot_sensitivity(mock_sens, type = "curves", output_type = "all")
ok(is.list(all_out),                  "'all' returns list")
ok(inherits(all_out$plot, "ggplot"),  "'all' $plot is ggplot")
ok(is.data.frame(all_out$table),      "'all' $table is data.frame")


# -----------------------------------------------------------------------------
# Section 10 -- interactive = TRUE
# -----------------------------------------------------------------------------
cat("\n--- Section 10: interactive = TRUE ---\n")

skip_plotly <- !requireNamespace("plotly", quietly = TRUE)
if (skip_plotly) {
  cat("  SKIP: plotly not installed\n")
} else {
  p_int <- sus_mod_plot_sensitivity(mock_sens, type = "curves", interactive = TRUE)
  ok(inherits(p_int, "plotly"), "interactive curves is plotly object")
}


# -----------------------------------------------------------------------------
# Section 11 -- lang = "en"
# -----------------------------------------------------------------------------
cat("\n--- Section 11: lang = 'en' ---\n")

p_en <- sus_mod_plot_sensitivity(mock_sens, type = "curves", lang = "en")
ok(inherits(p_en, "ggplot"), "lang='en' produces ggplot")
ok(grepl("Exposure|Stratum|Curves", p_en$labels$title %||% ""), "title contains English text")


# -----------------------------------------------------------------------------
# Section 12 -- lang = "es"
# -----------------------------------------------------------------------------
cat("\n--- Section 12: lang = 'es' ---\n")

p_es <- sus_mod_plot_sensitivity(mock_sens, type = "scatter", lang = "es")
ok(inherits(p_es, "ggplot"), "lang='es' produces ggplot")


# -----------------------------------------------------------------------------
# Section 13 -- base_size
# -----------------------------------------------------------------------------
cat("\n--- Section 13: base_size ---\n")

p_bs <- sus_mod_plot_sensitivity(mock_sens, type = "curves", base_size = 14L)
ok(p_bs$theme$text$size == 14L, "theme base_size is 14")


# -----------------------------------------------------------------------------
# Section 14 -- verbose
# -----------------------------------------------------------------------------
cat("\n--- Section 14: verbose ---\n")

p_v  <- sus_mod_plot_sensitivity(mock_sens, type = "curves", verbose = TRUE)
p_nv <- sus_mod_plot_sensitivity(mock_sens, type = "bar",    verbose = FALSE)
ok(inherits(p_v,  "ggplot"), "verbose=TRUE returns ggplot")
ok(inherits(p_nv, "ggplot"), "verbose=FALSE returns ggplot")


# -----------------------------------------------------------------------------
# Section 15 -- save_plot to tempfile
# -----------------------------------------------------------------------------
cat("\n--- Section 15: save_plot ---\n")

tmp <- tempfile(fileext = ".png")
sus_mod_plot_sensitivity(mock_sens, type = "curves", save_plot = tmp)
ok(file.exists(tmp), "PNG saved to tempfile")
unlink(tmp)


# -----------------------------------------------------------------------------
# Section 16 -- error on wrong input class
# -----------------------------------------------------------------------------
cat("\n--- Section 16: error on wrong class ---\n")

err_cls <- tryCatch(sus_mod_plot_sensitivity(list(a = 1)), error = function(e) e)
ok(inherits(err_cls, "error"), "error on non-climasus_sensitivity input")


# -----------------------------------------------------------------------------
# Section 17 -- stratum ordering by sensitivity index
# -----------------------------------------------------------------------------
cat("\n--- Section 17: stratum ordering ---\n")

# The 'curves' plot should factor label by SI desc from comparison
# comparison is ordered: Elderly > Male > Female
p_ord <- sus_mod_plot_sensitivity(mock_sens, type = "curves", lang = "pt")
built_data <- p_ord$data
ok("label" %in% names(built_data), "plot data has 'label' column")
if ("label" %in% names(built_data) && is.factor(built_data$label)) {
  lvls <- levels(built_data$label)
  ok(lvls[1L] == "Elderly", "highest-SI stratum is first factor level in curves")
} else {
  ok(FALSE, "label is factor with SI-ordered levels")
}

# The 'bar' plot should factor label with highest SI at top (reversed for horizontal)
p_bar2 <- sus_mod_plot_sensitivity(mock_sens, type = "bar", lang = "pt")
bar_data <- p_bar2$data
if ("label" %in% names(bar_data) && is.factor(bar_data$label)) {
  bar_lvls <- levels(bar_data$label)
  ok(bar_lvls[length(bar_lvls)] == "Elderly", "highest-SI stratum is last factor level in bar (top of y-axis)")
} else {
  ok(FALSE, "bar label is factor with SI-aware levels")
}


# -----------------------------------------------------------------------------
# Section 18 -- unsupported lang fallback
# -----------------------------------------------------------------------------
cat("\n--- Section 18: unsupported lang fallback ---\n")

p_bad <- suppressWarnings(
  sus_mod_plot_sensitivity(mock_sens, type = "curves", lang = "fr")
)
ok(inherits(p_bad, "ggplot"), "unsupported lang falls back to 'pt'")


# -----------------------------------------------------------------------------
cat(sprintf("\n=== RESULT: %d PASS / %d FAIL ===\n", PASS, FAIL))
if (FAIL > 0L) stop(sprintf("%d test(s) failed in test_sus_mod_plot_sensitivity.R", FAIL))
