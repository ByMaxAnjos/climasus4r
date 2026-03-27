# ==============================================================================
# test_sus_export_raps.R — Testes Básicos de Integração
# Pacote: climasus4r
# ==============================================================================
# Como rodar:
#   source("test_sus_export_raps.R")
#   Ou via testthat: testthat::test_file("test_sus_export_raps.R")
# ==============================================================================

# ── Helpers de teste leve (sem dependência de testthat) ────────────────────────
.pass <- function(desc) message(sprintf("  \u2713 PASS: %s", desc))
.fail <- function(desc, msg) stop(sprintf("  \u2717 FAIL: %s\n    %s", desc, msg), call. = FALSE)

.expect_true  <- function(expr, desc) if (isTRUE(expr))  .pass(desc) else .fail(desc, deparse(substitute(expr)))
.expect_error <- function(expr, desc) {
  tryCatch({ force(expr); .fail(desc, "nenhum erro lançado") },
           error = function(e) .pass(desc))
}
.expect_class <- function(obj, cls, desc) {
  if (inherits(obj, cls)) .pass(desc) else .fail(desc, sprintf("classe: %s", class(obj)[1]))
}

# ==============================================================================
message("\n=== Iniciando testes: sus_export_raps ===\n")

# ── Teste 1: extract_pipeline_structure — expressão básica ────────────────────
test_pipeline_basic <- function() {
  message("[ 1 ] extract_pipeline_structure — expressão básica")

  expr <- quote(
    sus_data_import(uf = "SP", year = 2020L, system = "SIM-DO", lang = "pt") |>
      sus_data_clean_encoding(lang = "pt") |>
      sus_data_standardize(lang = "pt") |>
      sus_data_filter_cid(disease_group = "cardiovascular", lang = "pt") |>
      sus_data_aggregate(time_unit = "month", lang = "pt")
  )

  s <- extract_pipeline_structure(expr)

  .expect_class(s,              "list",              "retorna lista")
  .expect_true(length(s$steps) == 5L,                "5 etapas detectadas")
  .expect_true(!is.null(s$input_params$uf),           "uf extraído")
  .expect_true(!is.null(s$input_params$years),        "years extraído")
  .expect_true(!is.null(s$input_params$system),       "system extraído")
  .expect_true(s$type == "Mortalidade Cardiovascular","tipo identificado corretamente")
  .expect_true(length(s$functions_used) == 5L,        "funções únicas OK")
}

# ── Teste 2: extract_pipeline_structure — pipe magrittr %>% ───────────────────
test_pipeline_magrittr <- function() {
  message("[ 2 ] extract_pipeline_structure — pipe magrittr")

  expr <- quote(
    sus_data_import(uf = "AM", year = 2015:2020, system = "SINAN-DENGUE") %>%
      sus_data_clean_encoding() %>%
      sus_data_aggregate(time_unit = "season")
  )

  s <- extract_pipeline_structure(expr)
  .expect_true(length(s$steps) == 3L, "3 etapas com pipe magrittr")
  .expect_true(grepl("SINAN", s$type), "tipo SINAN identificado")
}

# ── Teste 3: extract_pipeline_structure — pipeline inválido ───────────────────
test_pipeline_invalid <- function() {
  message("[ 3 ] extract_pipeline_structure — expressão não-pipeline")

  .expect_error(
    extract_pipeline_structure(quote(1 + 1)),
    "lança erro em expressão não-pipeline"
  )
}

# ── Teste 4: identify_pipeline_type ────────────────────────────────────────────
test_identify_type <- function() {
  message("[ 4 ] identify_pipeline_type")

  make_step <- function(fn, params = list()) {
    list(function_name = fn, important_params = params)
  }

  steps_resp <- list(
    make_step("sus_data_import"),
    make_step("sus_data_filter_cid", list(disease_group = "respiratory"))
  )
  .expect_true(
    grepl("Respirat", identify_pipeline_type(steps_resp)),
    "tipo respiratório detectado"
  )

  steps_heat <- list(
    make_step("sus_data_import"),
    make_step("sus_data_filter_cid", list(disease_group = "heat_related"))
  )
  .expect_true(
    grepl("Calor", identify_pipeline_type(steps_heat)),
    "tipo heat_related detectado"
  )

  steps_generic <- list(make_step("sus_data_import"))
  .expect_true(
    identify_pipeline_type(steps_generic) == "Pipeline Genérico",
    "tipo genérico como fallback"
  )
}

# ── Teste 5: create_script_content ────────────────────────────────────────────
test_create_script <- function() {
  message("[ 5 ] create_script_content")

  expr <- quote(
    sus_data_import(uf = "SP", year = 2020L, system = "SIM-DO", lang = "pt") |>
      sus_data_aggregate(time_unit = "month", lang = "pt")
  )
  s    <- extract_pipeline_structure(expr)
  meta <- create_pipeline_metadata(s, "pt", list(
    exec_date = "2025-01-01", r_version = "R version 4.4.0",
    platform = "linux", packages = list(climasus4r = "0.1.0")
  ))

  content <- create_script_content(s, meta, TRUE, "standard", "pt", list(
    exec_date = "2025-01-01", r_version = "R version 4.4.0",
    platform = "linux", packages = list(climasus4r = "0.1.0")
  ))

  .expect_true(is.character(content),             "retorna character")
  .expect_true(length(content) > 10L,             "conteúdo tem >10 linhas")
  .expect_true(any(grepl("set.seed", content)),   "set.seed presente")
  .expect_true(any(grepl("sessionInfo", content)),"sessionInfo presente")
  .expect_true(any(grepl("tryCatch", content)),   "tryCatch presente")
  .expect_true(any(grepl("params <-", content)),  "bloco params presente")
}

# ── Teste 6: create_function_content ─────────────────────────────────────────
test_create_function <- function() {
  message("[ 6 ] create_function_content")

  expr <- quote(
    sus_data_import(uf = "RJ", year = 2021L, system = "SIM-DO") |>
      sus_data_aggregate(time_unit = "week")
  )
  s    <- extract_pipeline_structure(expr)
  meta <- create_pipeline_metadata(s, "pt", list())

  content <- create_function_content(s, meta, TRUE, "standard", "pt")

  .expect_true(is.character(content),                     "retorna character")
  .expect_true(any(grepl("run_pipeline", content)),       "função nomeada run_pipeline")
  .expect_true(any(grepl("@export",      content)),       "tag @export roxygen presente")
  .expect_true(any(grepl("set.seed",     content)),       "set.seed no corpo")
  .expect_true(any(grepl("stopifnot",    content)),       "validação com stopifnot")
  .expect_true(any(grepl("invisible",    content)),       "retorno invisível")
}

# ── Teste 7: .validate_file_path — arquivo já existente ──────────────────────
test_file_exists_guard <- function() {
  message("[ 7 ] .validate_file_path — arquivo existente sem overwrite")

  tmp <- tempfile(fileext = ".R")
  writeLines("x <- 1", tmp)
  on.exit(unlink(tmp))

  .expect_error(
    .validate_file_path(tmp, "script", FALSE, "pt"),
    "erro quando arquivo existe e overwrite=FALSE"
  )
}

# ── Teste 8: .validate_file_path — path válido ────────────────────────────────
test_file_valid_path <- function() {
  message("[ 8 ] .validate_file_path — path string vazia")

  .expect_error(
    .validate_file_path("", "script", FALSE, "pt"),
    "erro para string vazia"
  )
}

# ── Teste 9: .collect_session_metadata ────────────────────────────────────────
test_session_metadata <- function() {
  message("[ 9 ] .collect_session_metadata")

  m <- .collect_session_metadata()
  .expect_class(m, "list",        "retorna lista")
  .expect_true(!is.null(m$r_version),   "r_version presente")
  .expect_true(!is.null(m$platform),    "platform presente")
  .expect_true(!is.null(m$exec_date),   "exec_date presente")
  .expect_true(
    m$platform %in% c("windows", "macos", "linux", "unix"),
    "platform é valor reconhecido"
  )
}

# ── Teste 10: sus_export_raps — argumento format inválido ─────────────────────
test_invalid_format <- function() {
  message("[10] sus_export_raps — format inválido")

  .expect_error(
    sus_export_raps(pipeline = quote(sus_data_import()), format = "excel"),
    "erro para formato desconhecido"
  )
}

# ==============================================================================
# Executar todos os testes
# ==============================================================================
tests <- list(
  test_pipeline_basic,
  test_pipeline_magrittr,
  test_pipeline_invalid,
  test_identify_type,
  test_create_script,
  test_create_function,
  test_file_exists_guard,
  test_file_valid_path,
  test_session_metadata,
  test_invalid_format
)

results <- vapply(tests, function(t) {
  tryCatch({ t(); TRUE }, error = function(e) { message(conditionMessage(e)); FALSE })
}, logical(1L))

n_pass <- sum(results)
n_fail <- sum(!results)

message(sprintf("\n=== Resultado: %d/%d testes passaram ===\n", n_pass, length(results)))

if (n_fail > 0L) {
  stop(sprintf("%d teste(s) falharam.", n_fail), call. = FALSE)
}
