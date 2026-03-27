# ==============================================================================
# test_sus_read_rap.R — Testes de Integração: família sus_read_rap
# ==============================================================================
# Como rodar:
#   source("sus_read_rap.R")
#   source("test_sus_read_rap.R")
# ==============================================================================

.pass <- function(d) message(sprintf("  \u2713 PASS: %s", d))
.fail <- function(d, m) stop(sprintf("  \u2717 FAIL: %s\n    %s", d, m), call. = FALSE)
.expect_true  <- function(e, d) if (isTRUE(e)) .pass(d) else .fail(d, deparse(substitute(e)))
.expect_error <- function(e, d) tryCatch({ force(e); .fail(d, "no error thrown") },
                                          error = function(x) .pass(d))
.expect_class <- function(o, c, d) if (inherits(o, c)) .pass(d) else .fail(d, class(o)[1])

# ==============================================================================
message("\n=== Iniciando testes: sus_read_rap ===\n")

# ── Fixture: criar arquivo RAP de teste ────────────────────────────────────────
.make_test_rap <- function(path,
                            uf        = '"SP"',
                            years     = "2020:2022",
                            system    = '"SIM-DO"',
                            time_unit = '"month"',
                            seed      = 42L) {
  content <- c(
    "#!/usr/bin/env Rscript",
    "# ============================================================================",
    "# Pipeline Reprodutível — Pipeline Mortalidade Cardiovascular",
    "# ============================================================================",
    "# Versão R  : R version 4.4.0 (2024-04-24)",
    "# Data      : 2025-01-10",
    "# Pacote    : climasus4r 0.1.0",
    "# Plataforma: linux",
    "# ============================================================================",
    "",
    sprintf("set.seed(%dL)", seed),
    "",
    "params <- list(",
    sprintf("  uf         = %s,", uf),
    sprintf("  years      = %s,", years),
    sprintf("  system     = %s,", system),
    sprintf("  time_unit  = %s,", time_unit),
    '  output_dir = "resultados",',
    '  lang       = "pt",',
    sprintf("  seed       = %dL", seed),
    ")",
    "",
    "df_resultado <- tryCatch({",
    "",
    sprintf("  sus_data_import(uf = %s, year = %s, system = %s, lang = params$lang, parallel = TRUE)", uf, years, system),
    "  |>",
    '  sus_data_clean_encoding(lang = params$lang)',
    "  |>",
    '  sus_data_standardize(lang = params$lang)',
    "  |>",
    '  sus_data_filter_cid(disease_group = "cardiovascular", lang = params$lang)',
    "  |>",
    '  sus_data_aggregate(time_unit = "month", lang = params$lang)',
    "",
    "}, error = function(e) stop(e))",
    "",
    "utils::sessionInfo()"
  )
  writeLines(content, path, useBytes = FALSE)
  invisible(path)
}

# ── Teste 1: sus_read_rap — leitura básica ─────────────────────────────────────
test_read_basic <- function() {
  message("[ 1 ] sus_read_rap — leitura básica")
  f <- tempfile(fileext = ".R")
  .make_test_rap(f)
  on.exit(unlink(c(f, paste0(f, ".bak"))))

  rap <- sus_read_rap(f, lang = "pt", validate = FALSE)

  .expect_class(rap, "rap_object", "retorna rap_object")
  .expect_true(rap$format == "script",        "formato script detectado")
  .expect_true(!is.null(rap$params$uf),       "uf extraído")
  .expect_true(!is.null(rap$params$years),    "years extraído")
  .expect_true(!is.null(rap$params$system),   "system extraído")
  .expect_true(length(rap$steps) >= 4L,       "≥4 etapas detectadas")
  .expect_true(is.character(rap$raw),         "raw preservado")
  .expect_true(nchar(rap$source) > 0L,        "source preenchido")
}

# ── Teste 2: sus_read_rap — arquivo inexistente ────────────────────────────────
test_read_missing <- function() {
  message("[ 2 ] sus_read_rap — arquivo inexistente")
  .expect_error(
    sus_read_rap("/tmp/nao_existe_12345.R"),
    "erro para arquivo inexistente"
  )
}

# ── Teste 3: sus_read_rap — file_path inválido ────────────────────────────────
test_read_bad_path <- function() {
  message("[ 3 ] sus_read_rap — file_path inválido")
  .expect_error(sus_read_rap(""), "erro para string vazia")
  .expect_error(sus_read_rap(NULL), "erro para NULL")
  .expect_error(sus_read_rap(42L), "erro para tipo errado")
}

# ── Teste 4: print.rap_object ──────────────────────────────────────────────────
test_print_method <- function() {
  message("[ 4 ] print.rap_object")
  f <- tempfile(fileext = ".R")
  .make_test_rap(f)
  on.exit(unlink(f))

  rap <- sus_read_rap(f, validate = FALSE)
  out <- capture.output(print(rap))

  .expect_true(any(grepl("rap_object", out)), "classe impressa")
  .expect_true(any(grepl("source",     out)), "source impresso")
  .expect_true(any(grepl("steps",      out)), "steps impresso")
}

# ── Teste 5: sus_inspect_rap — resumo único ────────────────────────────────────
test_inspect_single <- function() {
  message("[ 5 ] sus_inspect_rap — resumo único")
  f <- tempfile(fileext = ".R")
  .make_test_rap(f)
  on.exit(unlink(f))

  rap <- sus_read_rap(f, validate = FALSE)
  out <- capture.output(sus_inspect_rap(rap, verbose = TRUE))

  .expect_true(any(grepl("Source",  out)), "Source impresso")
  .expect_true(any(grepl("Params",  out)), "Params impresso")
  .expect_true(any(grepl("steps",   out, ignore.case = TRUE)), "steps impressos")
}

# ── Teste 6: sus_inspect_rap — diff com parâmetros diferentes ─────────────────
test_inspect_diff <- function() {
  message("[ 6 ] sus_inspect_rap — diff entre dois RAPs")
  f1 <- tempfile(fileext = ".R")
  f2 <- tempfile(fileext = ".R")
  .make_test_rap(f1, uf = '"SP"', years = "2020:2022")
  .make_test_rap(f2, uf = '"RJ"', years = "2021:2023")
  on.exit(unlink(c(f1, f2)))

  r1  <- sus_read_rap(f1, validate = FALSE)
  r2  <- sus_read_rap(f2, validate = FALSE)
  res <- sus_inspect_rap(r1, r2, verbose = FALSE)

  .expect_true(!is.null(res$diff),                              "diff gerado")
  .expect_true(length(res$diff$params_changed) > 0L,            "parâmetros diferem")
  uf_diff <- Filter(function(d) d$key == "uf", res$diff$params_changed)
  .expect_true(length(uf_diff) > 0L,                            "uf diferente detectado")
}

# ── Teste 7: sus_inspect_rap — diff com pipelines idênticos ───────────────────
test_inspect_diff_identical <- function() {
  message("[ 7 ] sus_inspect_rap — diff entre RAPs idênticos")
  f <- tempfile(fileext = ".R")
  .make_test_rap(f)
  on.exit(unlink(f))

  rap <- sus_read_rap(f, validate = FALSE)
  res <- capture.output(sus_inspect_rap(rap, rap, verbose = FALSE))

  .expect_true(any(grepl("identical", res, ignore.case = TRUE)), "idênticos detectados")
}

# ── Teste 8: sus_run_rap — dry_run ────────────────────────────────────────────
test_run_dry <- function() {
  message("[ 8 ] sus_run_rap — dry_run")
  f <- tempfile(fileext = ".R")
  .make_test_rap(f)
  on.exit(unlink(f))

  rap <- sus_read_rap(f, validate = FALSE)
  out <- capture.output(sus_run_rap(rap, dry_run = TRUE, lang = "pt"))

  .expect_true(any(grepl("sus_data_import", out)), "pipeline impresso no dry_run")
}

# ── Teste 9: sus_run_rap — argumento inválido ─────────────────────────────────
test_run_bad_arg <- function() {
  message("[ 9 ] sus_run_rap — objeto não-rap")
  .expect_error(sus_run_rap(list(a = 1)), "erro para objeto não-rap")
}

# ── Teste 10: sus_update_rap — patch de parâmetro ─────────────────────────────
test_update_params <- function() {
  message("[10] sus_update_rap — patch de uf")
  f <- tempfile(fileext = ".R")
  .make_test_rap(f, uf = '"SP"')
  on.exit(unlink(c(f, paste0(f, ".bak"))))

  rap     <- sus_read_rap(f, validate = FALSE)
  rap_upd <- sus_update_rap(rap, uf = '"RJ"', backup = TRUE, lang = "pt")

  .expect_class(rap_upd, "rap_object", "retorna rap_object atualizado")

  # Verificar que o arquivo foi realmente modificado
  lines <- readLines(f)
  .expect_true(any(grepl('"RJ"', lines)), "arquivo contém novo valor de uf")

  # Verificar que backup foi criado
  .expect_true(file.exists(paste0(f, ".bak")), "backup .bak criado")
}

# ── Teste 11: sus_update_rap — sem patches ────────────────────────────────────
test_update_no_patches <- function() {
  message("[11] sus_update_rap — sem parâmetros")
  f <- tempfile(fileext = ".R")
  .make_test_rap(f)
  on.exit(unlink(f))

  rap     <- sus_read_rap(f, validate = FALSE)
  rap_ret <- sus_update_rap(rap, lang = "pt")

  .expect_class(rap_ret, "rap_object", "retorna rap_object sem modificação")
}

# ── Teste 12: .diff_raps — version drift ──────────────────────────────────────
test_diff_version_drift <- function() {
  message("[12] .diff_raps — version drift de pacotes")
  r1 <- structure(list(
    params   = list(uf = "SP"),
    steps    = list(list(function_name = "sus_data_import")),
    metadata = list(packages = list(climasus4r = "0.1.0", dplyr = "1.1.0")),
    structure = list(type = "Pipeline Genérico")
  ), class = c("rap_object", "list"))

  r2 <- structure(list(
    params   = list(uf = "SP"),
    steps    = list(list(function_name = "sus_data_import")),
    metadata = list(packages = list(climasus4r = "0.2.0", dplyr = "1.1.0")),
    structure = list(type = "Pipeline Genérico")
  ), class = c("rap_object", "list"))

  diff <- .diff_raps(r1, r2)
  .expect_true(length(diff$version_drift) > 0L,   "drift de versão detectado")
  .expect_true(diff$version_drift[[1L]]$pkg == "climasus4r", "pacote correto identificado")
}

# ==============================================================================
tests <- list(
  test_read_basic, test_read_missing, test_read_bad_path,
  test_print_method, test_inspect_single, test_inspect_diff,
  test_inspect_diff_identical, test_run_dry, test_run_bad_arg,
  test_update_params, test_update_no_patches, test_diff_version_drift
)

results <- vapply(tests, function(t) {
  tryCatch({ t(); TRUE }, error = function(e) { message(conditionMessage(e)); FALSE })
}, logical(1L))

n_pass <- sum(results)
n_fail <- sum(!results)
message(sprintf("\n=== Resultado: %d/%d testes passaram ===\n", n_pass, length(results)))
if (n_fail > 0L) stop(sprintf("%d teste(s) falharam.", n_fail), call. = FALSE)
