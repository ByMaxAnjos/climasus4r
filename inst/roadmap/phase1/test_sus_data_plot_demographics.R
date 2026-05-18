# =============================================================================
# test_sus_data_plot_demographics.R
# Interactive test script for sus_data_plot_demographics()
#
# Usage:
#   source("inst/roadmap/phase1/test_sus_data_plot_demographics.R")
#   OR run sections interactively in RStudio
#
# Sections:
#   0. Setup + synthetic data
#   1. Quick smoke tests (all 8 types, default args)
#   2. Palette tests
#   3. Language tests (pt / en / es)
#   4. Stratified temporal + CI
#   5. Save to file
#   6. Arrow materialization
#   7. Edge cases / error handling
#   8. Dashboard
# =============================================================================


# =============================================================================
# HELPER: simple pass/fail wrapper
# =============================================================================
.test <- function(label, expr) {
  result <- tryCatch({
    out <- force(expr)
    list(ok = TRUE, out = out)
  }, error = function(e) {
    list(ok = FALSE, msg = conditionMessage(e))
  })

  if (result$ok) {
    cat(sprintf("  [PASS] %s\n", label))
  } else {
    cat(sprintf("  [FAIL] %s\n         Error: %s\n", label, result$msg))
  }
  invisible(result)
}


# =============================================================================
# 0. SETUP -- load package + build synthetic climasus_df
# =============================================================================
cat("=== 0. Setup ===\n")

devtools::load_all(quiet = TRUE)

set.seed(42)
n <- 2000

age_groups <- c("0-4", "5-9", "10-14", "15-19", "20-29", "30-39",
                "40-49", "50-59", "60-69", "70-79", "80+")
races      <- c("Branca", "Parda", "Preta", "Amarela", "Indigena")
educations <- c("Sem instrucao", "Fund. incompleto", "Fund. completo",
                "Medio incompleto", "Medio completo", "Superior")
regions    <- c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")
seasons    <- c("Verao", "Outono", "Inverno", "Primavera")

raw_df <- data.frame(
  # demographics
  sex                 = sample(c("Masculino", "Feminino"), n, replace = TRUE),
  race                = sample(races, n, replace = TRUE,
                               prob = c(0.43, 0.46, 0.07, 0.02, 0.02)),
  age_group           = sample(age_groups, n, replace = TRUE),
  education_level     = sample(educations, n, replace = TRUE),
  climate_risk_group  = sample(c("Baixo", "Moderado", "Alto", "Muito alto"),
                               n, replace = TRUE, prob = c(0.3, 0.35, 0.25, 0.1)),
  manager_uf          = sample(regions, n, replace = TRUE),
  # temporal
  year                = sample(2018:2023, n, replace = TRUE),
  month               = sample(paste0(2018, sprintf("%02d", 1:12)), n, replace = TRUE),
  epidemiological_week = sample(1:52, n, replace = TRUE),
  # climate
  astronomical_season = sample(seasons, n, replace = TRUE),
  # mortality / health
  age_years           = sample(0:99, n, replace = TRUE),
  residence_municipality_code = sample(c(2408102, 3304557, 3550308, 1302603),
                                       n, replace = TRUE),
  stringsAsFactors = FALSE
)

df <- new_climasus_df(
  raw_df,
  list(system = "SIM", stage = "filter_demo", type = "mortality")
)

cat(sprintf("  Synthetic df: %d rows, %d cols | stage = filter_demo\n",
            nrow(df), ncol(df)))


# =============================================================================
# 1. SMOKE TESTS -- all 8 types with default args
# =============================================================================
cat("\n=== 1. Smoke tests (all types) ===\n")

.test("type = 'table' (all vars)",
  sus_data_plot_demographics(df, type = "table", verbose = FALSE)
)

.test("type = 'table' (var = 'sex')",
  sus_data_plot_demographics(df, type = "table", var = "sex", verbose = FALSE)
)

.test("type = 'bar' (var = 'sex')",
  sus_data_plot_demographics(df, type = "bar", var = "sex", verbose = FALSE)
)

.test("type = 'bar' (var = 'race')",
  sus_data_plot_demographics(df, type = "bar", var = "race", verbose = FALSE)
)

.test("type = 'bar' (var = 'age_group')",
  sus_data_plot_demographics(df, type = "bar", var = "age_group", verbose = FALSE)
)

.test("type = 'bar' (var = 'education')",
  sus_data_plot_demographics(df, type = "bar", var = "education", verbose = FALSE)
)

.test("type = 'bar' (var = 'climate_risk')",
  sus_data_plot_demographics(df, type = "bar", var = "climate_risk", verbose = FALSE)
)

.test("type = 'bar' (var = 'region')",
  sus_data_plot_demographics(df, type = "bar", var = "region", verbose = FALSE)
)

.test("type = 'pyramid'",
  sus_data_plot_demographics(df, type = "pyramid", verbose = FALSE)
)

.test("type = 'temporal' (month)",
  sus_data_plot_demographics(df, type = "temporal", time_unit = "month", verbose = FALSE)
)

.test("type = 'temporal' (year)",
  sus_data_plot_demographics(df, type = "temporal", time_unit = "year", verbose = FALSE)
)

.test("type = 'temporal' (epi_week)",
  sus_data_plot_demographics(df, type = "temporal", time_unit = "epi_week", verbose = FALSE)
)

.test("type = 'climate'",
  sus_data_plot_demographics(df, type = "climate", verbose = FALSE)
)

.test("type = 'race_equity'",
  sus_data_plot_demographics(df, type = "race_equity", verbose = FALSE)
)

.test("type = 'dashboard'",
  sus_data_plot_demographics(df, type = "dashboard", verbose = FALSE)
)


# =============================================================================
# 2. PALETTE TESTS
# =============================================================================
cat("\n=== 2. Palette tests ===\n")

for (pal in c("lancet","nature","nejm","jco","aaas","sus","colorblind","viridis")) {
  .test(
    sprintf("palette = '%s'", pal),
    sus_data_plot_demographics(df, type = "bar", var = "sex",
                               palette = pal, verbose = FALSE)
  )
}

.test("unknown palette falls back gracefully",
  sus_data_plot_demographics(df, type = "bar", var = "sex",
                             palette = "banana", verbose = FALSE)
)


# =============================================================================
# 3. LANGUAGE TESTS
# =============================================================================
cat("\n=== 3. Language tests ===\n")

for (lng in c("pt","en","es")) {
  .test(
    sprintf("lang = '%s', pyramid", lng),
    sus_data_plot_demographics(df, type = "pyramid", lang = lng, verbose = FALSE)
  )
  .test(
    sprintf("lang = '%s', table", lng),
    sus_data_plot_demographics(df, type = "table", lang = lng, verbose = FALSE)
  )
}

.test("unknown lang falls back to 'pt'",
  sus_data_plot_demographics(df, type = "bar", var = "sex",
                             lang = "fr", verbose = FALSE)
)


# =============================================================================
# 4. STRATIFIED TEMPORAL + CONFIDENCE INTERVALS
# =============================================================================
cat("\n=== 4. Temporal stratification + CI ===\n")

.test("temporal stratified by sex",
  sus_data_plot_demographics(df, type = "temporal", time_unit = "month",
                             fill_var = "sex", verbose = FALSE)
)

.test("temporal stratified by climate_risk_group",
  sus_data_plot_demographics(df, type = "temporal", time_unit = "month",
                             fill_var = "climate_risk_group", verbose = FALSE)
)

.test("temporal with CI (show_ci = TRUE)",
  sus_data_plot_demographics(df, type = "temporal", time_unit = "month",
                             show_ci = TRUE, verbose = FALSE)
)

.test("temporal year with CI + stratification",
  sus_data_plot_demographics(df, type = "temporal", time_unit = "year",
                             fill_var = "sex", show_ci = TRUE, verbose = FALSE)
)

.test("temporal with non-existent fill_var (should warn, not error)",
  sus_data_plot_demographics(df, type = "temporal", fill_var = "nonexistent_col",
                             verbose = FALSE)
)


# =============================================================================
# 5. SAVE TO FILE
# =============================================================================
cat("\n=== 5. Save to file ===\n")

tmp_dir <- tempdir()

.test("save pyramid as PNG",
  sus_data_plot_demographics(df, type = "pyramid",
                             save_path = file.path(tmp_dir, "pyramid.png"),
                             width = 7, height = 5, dpi = 150,
                             verbose = FALSE)
)

.test("save dashboard as PDF",
  sus_data_plot_demographics(df, type = "dashboard",
                             save_path = file.path(tmp_dir, "dashboard.pdf"),
                             width = 12, height = 9,
                             verbose = FALSE)
)

.test("save bar as SVG",
  sus_data_plot_demographics(df, type = "bar", var = "race",
                             save_path = file.path(tmp_dir, "race_bar.svg"),
                             verbose = FALSE)
)

cat("  Saved files in:", tmp_dir, "\n")
cat("  Files:", paste(list.files(tmp_dir, pattern = "\\.(png|pdf|svg)$"),
                      collapse = ", "), "\n")


# =============================================================================
# 6. ARROW MATERIALIZATION
# =============================================================================
cat("\n=== 6. Arrow materialization ===\n")

if (requireNamespace("arrow", quietly = TRUE)) {
  arrow_tbl <- arrow::as_arrow_table(raw_df)

  .test("bare Arrow Table (no climasus_df) -- bar",
    sus_data_plot_demographics(arrow_tbl, type = "bar", var = "sex", verbose = FALSE)
  )

  # Arrow lazy query (dplyr on Arrow)
  arrow_lazy <- arrow_tbl |> dplyr::filter(year >= 2020)

  .test("Arrow lazy query -- pyramid",
    sus_data_plot_demographics(arrow_lazy, type = "pyramid", verbose = FALSE)
  )

  # climasus_df wrapped in Arrow
  arrow_climasus <- arrow::as_arrow_table(raw_df)
  attr(arrow_climasus, "sus_meta") <- list(
    system = "SIM", stage = "filter_demo", type = "mortality"
  )

  .test("Arrow Table with sus_meta attr -- temporal",
    sus_data_plot_demographics(arrow_climasus, type = "temporal",
                               time_unit = "month", verbose = FALSE)
  )
} else {
  cat("  [SKIP] arrow package not installed\n")
}


# =============================================================================
# 7. EDGE CASES + ERROR HANDLING
# =============================================================================
cat("\n=== 7. Edge cases / error handling ===\n")

# Stage too early (should error)
df_early <- new_climasus_df(
  raw_df,
  list(system = "SIM", stage = "clean", type = "mortality")
)
.test("stage 'clean' (too early) -- should ERROR",
  tryCatch(
    sus_data_plot_demographics(df_early, type = "bar", var = "sex", verbose = FALSE),
    error = function(e) stop("EXPECTED error caught: ", conditionMessage(e))
  )
)
# Correct interpretation: FAIL above means the error WAS raised as expected.
# Rephrase as explicit check:
cat("  (above FAIL is expected -- stage validation working)\n")

ok_stage_error <- tryCatch({
  sus_data_plot_demographics(df_early, type = "bar", var = "sex", verbose = FALSE)
  FALSE
}, error = function(e) TRUE)
cat(sprintf("  [%s] stage 'clean' correctly rejected\n",
            if (ok_stage_error) "PASS" else "FAIL"))

# Invalid type
ok_type_error <- tryCatch({
  sus_data_plot_demographics(df, type = "scatterplot", verbose = FALSE)
  FALSE
}, error = function(e) TRUE)
cat(sprintf("  [%s] invalid type 'scatterplot' correctly rejected\n",
            if (ok_type_error) "PASS" else "FAIL"))

# bar with missing var
ok_var_error <- tryCatch({
  sus_data_plot_demographics(df, type = "bar", var = NULL, verbose = FALSE)
  FALSE
}, error = function(e) TRUE)
cat(sprintf("  [%s] type='bar' with var=NULL correctly rejected\n",
            if (ok_var_error) "PASS" else "FAIL"))

# Data with only one sex level (pyramid edge case)
df_one_sex <- new_climasus_df(
  dplyr::filter(raw_df, sex == "Masculino"),
  list(system = "SIM", stage = "filter_demo", type = "mortality")
)
.test("pyramid with single-sex data",
  sus_data_plot_demographics(df_one_sex, type = "pyramid", verbose = FALSE)
)

# Very small dataset
df_tiny <- new_climasus_df(
  raw_df[1:5, ],
  list(system = "SIM", stage = "filter_demo", type = "mortality")
)
.test("bar with n=5 rows",
  sus_data_plot_demographics(df_tiny, type = "bar", var = "sex", verbose = FALSE)
)

# Missing columns (no sex column)
df_nosex <- new_climasus_df(
  dplyr::select(raw_df, -sex),
  list(system = "SIM", stage = "filter_demo", type = "mortality")
)
.test("pyramid without sex column (should warn/error gracefully)",
  tryCatch(
    sus_data_plot_demographics(df_nosex, type = "pyramid", verbose = FALSE),
    error = function(e) stop("graceful error: ", conditionMessage(e))
  )
)

# caption_suffix
.test("caption_suffix appended",
  sus_data_plot_demographics(df, type = "bar", var = "sex",
                             caption_suffix = "Study: RN 2018-2023 | doi:10.0000/test",
                             verbose = FALSE)
)

# base_size
.test("base_size = 14",
  sus_data_plot_demographics(df, type = "bar", var = "sex",
                             base_size = 14, verbose = FALSE)
)


# =============================================================================
# 8. DASHBOARD -- inspect composite layout
# =============================================================================
# HEATMAP TESTS -- cross-demographic tile matrix
# =============================================================================
cat("\n=== HEATMAP: cross-tabulation matrix ===\n")

# ---- Auto variable selection (age_group x race) ----------------------------
.test("heatmap auto-detect row/col (age_group x race), pct_row",
  sus_data_plot_demographics(df, type = "heatmap", verbose = FALSE)
)

# ---- Explicit variable combinations ----------------------------------------
.test("heatmap age_group x sex, pct_row",
  sus_data_plot_demographics(df, type = "heatmap",
                             heatmap_row = "age_group", heatmap_col = "sex",
                             fill_metric = "pct_row", verbose = FALSE)
)

.test("heatmap age_group x race, pct_col",
  sus_data_plot_demographics(df, type = "heatmap",
                             heatmap_row = "age_group", heatmap_col = "race",
                             fill_metric = "pct_col", verbose = FALSE)
)

.test("heatmap education x climate_risk, count",
  sus_data_plot_demographics(df, type = "heatmap",
                             heatmap_row = "education", heatmap_col = "climate_risk",
                             fill_metric = "count", verbose = FALSE)
)

.test("heatmap sex x region, pct_total",
  sus_data_plot_demographics(df, type = "heatmap",
                             heatmap_row = "sex", heatmap_col = "region",
                             fill_metric = "pct_total", verbose = FALSE)
)

# ---- All palettes ----------------------------------------------------------
for (pal_nm in c("lancet", "nature", "nejm", "sus")) {
  .test(sprintf("heatmap palette = '%s'", pal_nm),
    sus_data_plot_demographics(df, type = "heatmap",
                               heatmap_row = "age_group", heatmap_col = "sex",
                               palette = pal_nm, verbose = FALSE)
  )
}

# ---- Multilingual ----------------------------------------------------------
for (lng in c("pt", "en", "es")) {
  .test(sprintf("heatmap lang = '%s'", lng),
    sus_data_plot_demographics(df, type = "heatmap",
                               heatmap_row = "age_group", heatmap_col = "race",
                               lang = lng, verbose = FALSE)
  )
}

# ---- Interactive (plotly) --------------------------------------------------
if (requireNamespace("plotly", quietly = TRUE)) {
  .test("heatmap interactive (plotly)",
    sus_data_plot_demographics(df, type = "heatmap",
                               heatmap_row = "age_group", heatmap_col = "race",
                               interactive = TRUE, verbose = FALSE)
  )
} else {
  cat("  [SKIP] plotly not installed\n")
}

# ---- Unknown fill_metric falls back gracefully -----------------------------
.test("heatmap unknown fill_metric falls back to pct_row",
  sus_data_plot_demographics(df, type = "heatmap",
                             fill_metric = "banana", verbose = FALSE)
)

# ---- Same variable as row and col should error gracefully ------------------
ok_same_var <- tryCatch({
  sus_data_plot_demographics(df, type = "heatmap",
                             heatmap_row = "sex", heatmap_col = "sex",
                             verbose = FALSE)
  FALSE
}, error = function(e) TRUE)
cat(sprintf("  [%s] same row/col variable correctly rejected\n",
            if (ok_same_var) "PASS" else "FAIL"))

# ---- Save as PNG -----------------------------------------------------------
tmp_dir <- tempdir()
.test("heatmap saved as PNG",
  sus_data_plot_demographics(df, type = "heatmap",
                             heatmap_row = "age_group", heatmap_col = "race",
                             save_path = file.path(tmp_dir, "heatmap_age_race.png"),
                             width = 9, height = 6, dpi = 150, verbose = FALSE)
)
cat("  Saved heatmap to:", file.path(tmp_dir, "heatmap_age_race.png"), "\n")


# =============================================================================
# 8. DASHBOARD -- visual inspection
# =============================================================================
cat("\n=== 8. Dashboard visual inspection ===\n")

dash_pt <- sus_data_plot_demographics(
  df,
  type      = "dashboard",
  palette   = "lancet",
  lang      = "pt",
  verbose   = TRUE
)

cat("  Dashboard class:", paste(class(dash_pt), collapse = ", "), "\n")

# Render in active device
if (interactive()) {
  print(dash_pt)

  cat("\nRendering all types one-by-one for visual inspection...\n")
  for (tp in c("table","bar","pyramid","temporal","climate","race_equity")) {
    cat("  ->", tp, "\n")
    p <- sus_data_plot_demographics(df, type = tp,
                                    var = if (tp == "bar") "race" else NULL,
                                    lang = "pt", verbose = FALSE)
    if (inherits(p, "ggplot")) print(p)
    if (inherits(p, c("gt_tbl","datatables"))) print(p)
    Sys.sleep(0.5)
  }
}

cat("\n=== All tests complete ===\n")
