# =============================================================================
# test_sus_data_quality_report.R
# Test examples for the upgraded sus_data_quality_report()
#
# Usage:
#   source("inst/roadmap/phase1/test_sus_data_quality_report.R")
#   OR run sections interactively in RStudio
#
# Sections:
#   0. Setup
#   1. Synthetic datasets (clean, degraded, edge cases)
#   2. Console output -- all three languages
#   3. Output formats (gt, markdown, html)
#   4. Pipeline audit -- different stages
#   5. Selective checks (check_icd, check_dates, top_n)
#   6. Arrow materialization
#   7. Return value inspection
#   8. Error handling
# =============================================================================


# =============================================================================
# HELPER
# =============================================================================
.test <- function(label, expr) {
  result <- tryCatch({ force(expr); list(ok = TRUE) },
                     error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
  if (result$ok) cat(sprintf("  [PASS] %s\n", label))
  else           cat(sprintf("  [FAIL] %s\n         %s\n", label, result$msg))
  invisible(result)
}


# =============================================================================
# 0. SETUP
# =============================================================================
cat("=== 0. Setup ===\n")
devtools::load_all(quiet = TRUE)

set.seed(99)
N <- 1500

# ---- A) Clean, well-structured dataset (high quality score expected) --------
raw_clean <- data.frame(
  # Demographics
  sex                 = sample(c("Masculino", "Feminino"), N, replace = TRUE),
  race                = sample(c("Branca", "Parda", "Preta", "Amarela", "Indigena"), N,
                               replace = TRUE, prob = c(0.43, 0.46, 0.07, 0.02, 0.02)),
  age_years           = sample(0:99, N, replace = TRUE),
  age_group           = sample(c("0-4", "5-14", "15-29", "30-44", "45-59", "60+"),
                               N, replace = TRUE),
  education_level     = sample(c("Sem instrucao", "Fund. incompleto",
                                 "Medio completo", "Superior"), N, replace = TRUE),
  climate_risk_group  = sample(c("Baixo", "Moderado", "Alto", "Muito alto"), N,
                               replace = TRUE, prob = c(0.3, 0.35, 0.25, 0.1)),
  # Temporal
  year                = sample(2018:2023, N, replace = TRUE),
  month               = sample(paste0(sample(2018:2023, N, replace = TRUE),
                                       sprintf("%02d", sample(1:12, N, replace = TRUE))),
                               N, replace = TRUE),
  epidemiological_week = sample(1:52, N, replace = TRUE),
  # ICD-10
  underlying_cause    = sample(c("I21", "I63", "J18", "C50", "K92", "E11"), N,
                               replace = TRUE),
  # Geographic
  residence_municipality_code = sample(c(2408102L, 3304557L, 3550308L, 1302603L),
                                        N, replace = TRUE),
  manager_uf          = sample(c("RN", "SP", "CE", "AM"), N, replace = TRUE),
  # Dates (proper Date class)
  death_date          = as.Date("2018-01-01") + sample(0:2000, N, replace = TRUE),
  stringsAsFactors    = FALSE
)

df_clean <- new_climasus_df(
  raw_clean,
  list(
    system  = "SIM",
    stage   = "filter_demo",
    type    = "mortality",
    history = c(
      "[2024-03-01 08:00:00] Imported SIM data: 1500 records",
      "[2024-03-01 08:01:00] Standardised columns for SIM",
      "[2024-03-01 08:02:00] Filtered by ICD-10: circulatory (I00-I99)",
      "[2024-03-01 08:03:00] Demographic filter applied: age >= 0"
    )
  )
)

# ---- B) Degraded dataset (low quality -- many issues to flag) ---------------
raw_bad <- raw_clean
# 30% missing race
raw_bad$race[sample(N, round(N * 0.30))] <- NA
# 25% missing education
raw_bad$education_level[sample(N, round(N * 0.25))] <- NA
# Implausible ages
raw_bad$age_years[sample(N, 20)] <- sample(c(-5, 150, 200), 20, replace = TRUE)
# Future dates
raw_bad$death_date[sample(N, 30)] <- Sys.Date() + sample(1:365, 30, replace = TRUE)
# Pre-1900 dates
raw_bad$death_date[sample(N, 10)] <- as.Date("1899-12-31") - sample(1:100, 10, replace = TRUE)
# Invalid ICD codes
raw_bad$underlying_cause[sample(N, 50)] <- sample(c("ZZZ", "999", "ABC"), 50, replace = TRUE)
# Introduce 50 duplicate rows
raw_bad <- rbind(raw_bad, raw_bad[sample(N, 50), ])

df_bad <- new_climasus_df(
  raw_bad,
  list(system = "SIM", stage = "filter_demo", type = "mortality")
)

# ---- C) Minimal dataset (only mandatory columns, no derived vars) -----------
df_minimal <- new_climasus_df(
  data.frame(
    sex       = sample(c("Masculino", "Feminino"), 200, replace = TRUE),
    age_years = sample(20:80, 200, replace = TRUE),
    underlying_cause = sample(c("I21", "J18", NA), 200, replace = TRUE,
                               prob = c(0.5, 0.4, 0.1)),
    stringsAsFactors = FALSE
  ),
  list(system = "SIM", stage = "clean", type = "mortality")
)

# ---- D) Dataset at "aggregate" stage (full pipeline history) ----------------
df_aggregate <- new_climasus_df(
  raw_clean,
  list(
    system  = "SIH",
    stage   = "aggregate",
    type    = "agg",
    history = c(
      "[2024-04-01 09:00:00] Imported SIH data: 1500 records",
      "[2024-04-01 09:01:00] Encoding cleaned",
      "[2024-04-01 09:02:00] Standardised columns for SIH",
      "[2024-04-01 09:03:00] Filtered diagnoses: respiratory (J00-J99)",
      "[2024-04-01 09:04:00] Demographic filter applied",
      "[2024-04-01 09:05:00] Variables derived: age_group, epi_week",
      "[2024-04-01 09:06:00] Aggregated by municipality x month"
    )
  )
)

cat("  df_clean    :", nrow(df_clean), "rows | stage:", sus_meta(df_clean, "stage"), "\n")
cat("  df_bad      :", nrow(df_bad),   "rows | stage:", sus_meta(df_bad,   "stage"), "\n")
cat("  df_minimal  :", nrow(df_minimal), "rows | stage:", sus_meta(df_minimal, "stage"), "\n")
cat("  df_aggregate:", nrow(df_aggregate), "rows | stage:", sus_meta(df_aggregate, "stage"), "\n")


# =============================================================================
# 1. CONSOLE OUTPUT -- three languages
# =============================================================================
cat("\n=== 1. Console output -- languages ===\n")

.test("console lang = 'pt' (clean data)",
  sus_data_quality_report(df_clean, lang = "pt", verbose = FALSE)
)

.test("console lang = 'en' (clean data)",
  sus_data_quality_report(df_clean, lang = "en", verbose = FALSE)
)

.test("console lang = 'es' (clean data)",
  sus_data_quality_report(df_clean, lang = "es", verbose = FALSE)
)

.test("unknown lang falls back to pt",
  sus_data_quality_report(df_clean, lang = "fr", verbose = FALSE)
)


# =============================================================================
# 2. QUALITY SCORE COMPARISON: clean vs degraded
# =============================================================================
cat("\n=== 2. Quality score comparison ===\n")

r_clean <- sus_data_quality_report(df_clean, lang = "en", verbose = FALSE)
r_bad   <- sus_data_quality_report(df_bad,   lang = "en", verbose = FALSE)

cat(sprintf("  df_clean score : %.1f / 100\n", r_clean$score))
cat(sprintf("  df_bad   score : %.1f / 100\n", r_bad$score))

if (r_clean$score > r_bad$score) {
  cat("  [PASS] Clean data scores higher than degraded data\n")
} else {
  cat("  [FAIL] Expected clean > bad\n")
}

# Inspect specific quality flags in degraded data
bad_miss <- r_bad$missing$by_column
cat(sprintf("\n  Columns flagged as CRITICAL in df_bad: %d\n",
            sum(bad_miss$quality_flag == "critical")))
cat(sprintf("  Columns flagged as WARN    in df_bad: %d\n",
            sum(bad_miss$quality_flag == "warn")))
cat(sprintf("  Duplicate rows detected          : %d\n", r_bad$overview$n_dup))
cat(sprintf("  Implausible age values           : %d\n",
            r_bad$demographics$age$n_implausible))
cat(sprintf("  Future dates detected            : %d\n",
            sum(sapply(r_bad$dates, `[[`, "n_future"))))
cat(sprintf("  ICD validity                     : %.1f%%\n",
            r_bad$icd$pct_valid))


# =============================================================================
# 3. OUTPUT FORMATS
# =============================================================================
cat("\n=== 3. Output formats ===\n")
tmp <- tempdir()

# gt table
.test("output_format = 'gt'", {
  rlang::check_installed("gt")
  tbl <- sus_data_quality_report(df_clean, output_format = "gt",
                                  lang = "en", verbose = FALSE)
  stopifnot(inherits(tbl, "gt_tbl"))
  tbl
})

# Markdown
md_path <- file.path(tmp, "quality_report.md")
.test("output_format = 'markdown' (pt)",
  sus_data_quality_report(df_clean, output_format = "markdown",
                           output_file = md_path, lang = "pt", verbose = FALSE)
)
if (file.exists(md_path)) {
  lines <- readLines(md_path)
  cat(sprintf("  Markdown: %d lines, saved to %s\n", length(lines), md_path))
  cat("  First 5 lines:\n")
  for (l in utils::head(lines, 5)) cat("    ", l, "\n")
}

# HTML
html_path <- file.path(tmp, "quality_report.html")
.test("output_format = 'html' (en)",
  sus_data_quality_report(df_clean, output_format = "html",
                           output_file = html_path, lang = "en", verbose = FALSE)
)
if (file.exists(html_path)) {
  lines <- readLines(html_path)
  cat(sprintf("  HTML: %d lines, saved to %s\n", length(lines), html_path))
}

# Auto-generated filename (no output_file argument)
.test("markdown with auto-generated filename",
  sus_data_quality_report(df_clean, output_format = "markdown", lang = "pt",
                           verbose = FALSE)
)


# =============================================================================
# 4. PIPELINE AUDIT -- different stages
# =============================================================================
cat("\n=== 4. Pipeline audit across stages ===\n")

# stage = "clean" (early pipeline -- few functions expected)
df_stage_clean <- new_climasus_df(
  raw_clean,
  list(system = "SIM", stage = "clean", type = "mortality",
       history = c("[2024-01-01 10:00:00] Imported SIM data"))
)

r_early <- sus_data_quality_report(df_stage_clean, lang = "en", verbose = FALSE)
cat(sprintf("  Stage 'clean'     | fns detected: %d | pending stages: %d\n",
            length(r_early$pipeline$functions_applied),
            length(setdiff(
              c("import","clean","stand","filter_cid","filter_demo",
                "derive","aggregate","spatial","census","climate"),
              r_early$pipeline$stages_reached))))

r_demo <- sus_data_quality_report(df_clean, lang = "en", verbose = FALSE)
cat(sprintf("  Stage 'filter_demo' | fns detected: %d | pending stages: %d\n",
            length(r_demo$pipeline$functions_applied),
            length(setdiff(
              c("import","clean","stand","filter_cid","filter_demo",
                "derive","aggregate","spatial","census","climate"),
              r_demo$pipeline$stages_reached))))

r_agg <- sus_data_quality_report(df_aggregate, lang = "en", verbose = FALSE)
cat(sprintf("  Stage 'aggregate'   | fns detected: %d | pending stages: %d\n",
            length(r_agg$pipeline$functions_applied),
            length(setdiff(
              c("import","clean","stand","filter_cid","filter_demo",
                "derive","aggregate","spatial","census","climate"),
              r_agg$pipeline$stages_reached))))

# Functions listed for aggregate stage
cat("  Functions at aggregate stage:\n")
for (fn in r_agg$pipeline$functions_applied)
  cat(sprintf("    v %s\n", fn))


# =============================================================================
# 5. SELECTIVE CHECKS
# =============================================================================
cat("\n=== 5. Selective checks ===\n")

.test("check_icd = FALSE (skip ICD section)",
  sus_data_quality_report(df_clean, check_icd = FALSE, verbose = FALSE)
)

.test("check_dates = FALSE (skip date section)",
  sus_data_quality_report(df_clean, check_dates = FALSE, verbose = FALSE)
)

.test("check_icd = FALSE AND check_dates = FALSE",
  sus_data_quality_report(df_clean, check_icd = FALSE, check_dates = FALSE,
                           verbose = FALSE)
)

.test("top_n = 3 (only top 3 rows in frequency tables)",
  sus_data_quality_report(df_clean, top_n = 3, verbose = FALSE)
)

.test("top_n = 20 (extended frequency tables)",
  sus_data_quality_report(df_clean, top_n = 20, verbose = FALSE)
)

.test("verbose = TRUE (shows progress messages)",
  sus_data_quality_report(df_clean, lang = "pt", verbose = TRUE)
)


# =============================================================================
# 6. PLAIN DATA.FRAME (no sus_meta)
# =============================================================================
cat("\n=== 6. Plain data.frame (no climasus_df) ===\n")

plain_df <- as.data.frame(raw_clean)  # strip climasus_df class

.test("plain data.frame -- no history, stage = 'unknown'", {
  r <- sus_data_quality_report(plain_df, lang = "en", verbose = FALSE)
  stopifnot(r$meta$stage == "unknown")
  stopifnot(length(r$pipeline$functions_applied) == 0L)
  r
})


# =============================================================================
# 7. ARROW MATERIALIZATION
# =============================================================================
cat("\n=== 7. Arrow materialization ===\n")

if (requireNamespace("arrow", quietly = TRUE)) {

  arrow_tbl <- arrow::as_arrow_table(raw_clean)
  .test("bare Arrow Table (auto-materialised)", {
    r <- sus_data_quality_report(arrow_tbl, lang = "en", verbose = FALSE)
    stopifnot(is.numeric(r$score))
    r
  })

  # Arrow with sus_meta attribute preserved
  attr(arrow_tbl, "sus_meta") <- list(
    system = "SIM", stage = "filter_demo", type = "mortality"
  )
  .test("Arrow Table with sus_meta attribute", {
    r <- sus_data_quality_report(arrow_tbl, lang = "pt", verbose = FALSE)
    stopifnot(r$meta$stage == "filter_demo")
    r
  })

} else {
  cat("  [SKIP] arrow package not available\n")
}


# =============================================================================
# 8. RETURN VALUE INSPECTION
# =============================================================================
cat("\n=== 8. Return value structure ===\n")

r <- sus_data_quality_report(df_clean, lang = "en", verbose = FALSE)

cat("\n  Top-level keys:\n")
cat("  ", paste(names(r), collapse = ", "), "\n")

cat("\n  $meta:\n")
cat(sprintf("    stage   = %s\n", r$meta$stage))
cat(sprintf("    system  = %s\n", r$meta$system))
cat(sprintf("    type    = %s\n", r$meta$type))
cat(sprintf("    history = %d entries\n", r$meta$n_history))

cat("\n  $pipeline$functions_applied:\n")
for (fn in r$pipeline$functions_applied) cat(sprintf("    %s\n", fn))

cat("\n  $overview:\n")
cat(sprintf("    n_rows  = %s\n", format(r$overview$n_rows, big.mark = ",")))
cat(sprintf("    n_cols  = %d\n",  r$overview$n_cols))
cat(sprintf("    n_dup   = %d\n",  r$overview$n_dup))

cat("\n  $missing:\n")
cat(sprintf("    completeness_score = %.1f\n", r$missing$completeness_score))
cat(sprintf("    n_complete_cols    = %d\n",   r$missing$n_complete_cols))
cat(sprintf("    n_warn_cols        = %d\n",   r$missing$n_warn_cols))
cat(sprintf("    n_critical_cols    = %d\n",   r$missing$n_critical_cols))

cat("\n  $demographics names:\n")
cat("  ", paste(names(r$demographics), collapse = ", "), "\n")

cat(sprintf("\n  $icd$pct_valid      = %.1f%%\n", r$icd$pct_valid))
cat(sprintf("  $icd$n_unique       = %d\n", r$icd$n_unique))

cat(sprintf("\n  $geographic$municipalities$n_unique = %d\n",
            r$geographic$municipalities$n_unique))

cat(sprintf("\n  $score             = %.1f / 100\n", r$score))

# Programmatic use: assert score above threshold
score_threshold <- 70
if (r$score >= score_threshold) {
  cat(sprintf("  [PASS] Score >= %d quality threshold\n", score_threshold))
} else {
  cat(sprintf("  [FAIL] Score below %d threshold\n", score_threshold))
}


# =============================================================================
# 9. ERROR HANDLING
# =============================================================================
cat("\n=== 9. Error handling ===\n")

# Invalid input type
ok_non_df <- tryCatch({
  sus_data_quality_report(list(a = 1, b = 2), verbose = FALSE)
  FALSE
}, error = function(e) TRUE)
cat(sprintf("  [%s] Non-data.frame input correctly rejected\n",
            if (ok_non_df) "PASS" else "FAIL"))

# Invalid output_format
ok_fmt <- tryCatch({
  sus_data_quality_report(df_clean, output_format = "excel", verbose = FALSE)
  FALSE
}, error = function(e) TRUE)
cat(sprintf("  [%s] Invalid output_format correctly rejected\n",
            if (ok_fmt) "PASS" else "FAIL"))


# =============================================================================
# 10. SINAN / SIH / SINASC SYSTEM VARIANTS
# =============================================================================
cat("\n=== 10. DATASUS system variants ===\n")

make_df <- function(system, stage, ...) {
  new_climasus_df(
    data.frame(
      sex       = sample(c("M", "F"), 200, replace = TRUE),
      age_years = sample(15:80, 200, replace = TRUE),
      ...
    ),
    list(system = system, stage = stage, type = "mortality")
  )
}

# SINAN -- uses CS_SEXO and CS_RACA column names
df_sinan <- new_climasus_df(
  data.frame(
    CS_SEXO   = sample(c("M", "F"), 300, replace = TRUE),
    CS_RACA   = sample(c("1","2","3","4","5",NA), 300, replace = TRUE),
    NU_IDADE_N = sample(0:99, 300, replace = TRUE),
    CS_ESCOL_N = sample(c("0","1","2","3","4"), 300, replace = TRUE),
    DIAG_PRINC = sample(c("A90","B34","J18","Z00",NA), 300, replace = TRUE),
    SG_UF_NOT  = sample(c("RN","SP","CE"), 300, replace = TRUE),
    stringsAsFactors = FALSE
  ),
  list(system = "SINAN", stage = "filter_demo", type = "mortality")
)

.test("SINAN column names (CS_SEXO, CS_RACA, DIAG_PRINC)", {
  r <- sus_data_quality_report(df_sinan, lang = "en", verbose = FALSE)
  stopifnot(!is.null(r$demographics$sex))
  stopifnot(!is.null(r$icd))
  r
})

# SIM raw column names
df_sim_raw <- new_climasus_df(
  data.frame(
    SEXO     = sample(c("1","2","9"), 300, replace = TRUE),
    RACACOR  = sample(c("1","2","3","4","5",NA), 300, replace = TRUE),
    CAUSABAS = sample(c("I219","I639","J189",NA), 300, replace = TRUE),
    CODMUNRES = sample(c(2408102L, 3304557L), 300, replace = TRUE),
    stringsAsFactors = FALSE
  ),
  list(system = "SIM", stage = "stand", type = "mortality")
)

.test("SIM raw column names (SEXO, RACACOR, CAUSABAS, CODMUNRES)", {
  r <- sus_data_quality_report(df_sim_raw, lang = "pt", verbose = FALSE)
  stopifnot(!is.null(r$demographics$sex))
  stopifnot(!is.null(r$geographic$municipalities))
  r
})


# =============================================================================
cat("\n=== All tests complete ===\n")
