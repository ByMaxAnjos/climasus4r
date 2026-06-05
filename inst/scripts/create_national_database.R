#!/usr/bin/env Rscript
# =============================================================================
# create_national_database.R
# Baixa todos os dados DATASUS para o Brasil e salva como dataset Parquet
# Hive-particionado — pronto para arrow::open_dataset() e sus_data_*.
#
# Uso:
#   Rscript inst/scripts/create_national_database.R
#   Rscript inst/scripts/create_national_database.R --systems SIM,SIH,SINAN
#   Rscript inst/scripts/create_national_database.R --years 2015:2023
#   Rscript inst/scripts/create_national_database.R --years last30
#   Rscript inst/scripts/create_national_database.R --ufs SP,RJ,MG
#   Rscript inst/scripts/create_national_database.R --ufs sudeste
#   Rscript inst/scripts/create_national_database.R --workers 6
#
# Estrutura de saída (Hive-partitioned):
#   {output-dir}/
#   ├── manifest.csv
#   ├── sim/DO/uf=AC/year=2010/data.parquet
#   ├── sih/RD/uf=RO/year=2010/month=01/data.parquet
#   ├── sinan/DENGUE/year=2010/data.parquet    ← sem UF (download nacional)
#   ├── sia/PA/uf=SP/year=2015/month=03/data.parquet
#   ├── cnes/ST/uf=MG/year=2018/month=06/data.parquet
#   └── sinasc/uf=AC/year=2022/data.parquet
#
# Lendo o dataset depois:
#   library(arrow)
#   arrow::open_dataset("~/datasus_br/sim/DO/") |>
#     dplyr::filter(year >= 2015) |>
#     dplyr::collect() |>
#     sus_data_standardize() |>
#     sus_data_aggregate(time_unit = "year")
# =============================================================================

suppressPackageStartupMessages({
  if (!requireNamespace("climasus4r", quietly = TRUE)) {
    devtools::load_all(".", quiet = TRUE)
  } else {
    library(climasus4r)
  }
})

# =============================================================================
# CONFIGURAÇÃO DOS SISTEMAS
# =============================================================================

# Anual por UF
.SYS_ANNUAL <- c(
  "SIM-DO", "SIM-DOFET", "SIM-DOEXT", "SIM-DOINF", "SIM-DOMAT",
  "SINASC"
)

# Anual NACIONAL — microdatasus baixa Brasil inteiro por ano (sem loop de UF)
.SYS_ANNUAL_NATIONAL <- c(
  "SINAN-DENGUE", "SINAN-CHIKUNGUNYA", "SINAN-ZIKA", "SINAN-MALARIA",
  "SINAN-CHAGAS", "SINAN-LEISHMANIOSE-VISCERAL",
  "SINAN-LEISHMANIOSE-TEGUMENTAR", "SINAN-LEPTOSPIROSE"
)

# Mensal por UF
.SYS_MONTHLY <- c(
  "SIH-RD", "SIH-RJ", "SIH-SP", "SIH-ER",
  "SIA-PA", "SIA-AM", "SIA-AQ", "SIA-PS", "SIA-AD", "SIA-AR",
  "CNES-LT", "CNES-ST", "CNES-DC", "CNES-EQ", "CNES-SR", "CNES-HB",
  "CNES-PF", "CNES-EP", "CNES-RC", "CNES-IN", "CNES-EE", "CNES-EF", "CNES-GM"
)

.ALL_SYSTEMS <- c(.SYS_ANNUAL, .SYS_ANNUAL_NATIONAL, .SYS_MONTHLY)

# Mapa família → sub-sistemas (para --systems SIM,SIH,...)
.FAMILY_MAP <- list(
  SIM    = grep("^SIM",    .ALL_SYSTEMS, value = TRUE),
  SINAN  = grep("^SINAN",  .ALL_SYSTEMS, value = TRUE),
  SINASC = "SINASC",
  SIH    = grep("^SIH",    .ALL_SYSTEMS, value = TRUE),
  SIA    = grep("^SIA",    .ALL_SYSTEMS, value = TRUE),
  CNES   = grep("^CNES",   .ALL_SYSTEMS, value = TRUE)
)

.ALL_UFS <- c(
  "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA",
  "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN",
  "RS", "RO", "RR", "SC", "SP", "SE", "TO"
)

# Regiões aceitas no --ufs (mesmo mapa do sus_data_import)
.REGIONS <- list(
  norte        = c("AC","AP","AM","PA","RO","RR","TO"),
  nordeste     = c("AL","BA","CE","MA","PB","PE","PI","RN","SE"),
  centro_oeste = c("DF","GO","MT","MS"),
  sudeste      = c("ES","MG","RJ","SP"),
  sul          = c("PR","RS","SC")
)

# =============================================================================
# PARSING DE ARGUMENTOS
# =============================================================================

parse_args <- function() {
  raw  <- commandArgs(trailingOnly = TRUE)
  args <- list(
    "output-dir"    = "~/datasus_br",
    "systems"       = "all",
    "years"         = "last30",
    "ufs"           = "all",
    "months"        = "1:12",
    "workers"       = "4",
    "skip-existing" = "TRUE",
    "verbose"       = "TRUE"
  )
  i <- 1L
  while (i <= length(raw)) {
    key <- sub("^--", "", raw[i])
    val <- if (i < length(raw) && !startsWith(raw[i + 1L], "--")) {
      i <- i + 1L; raw[i]
    } else "TRUE"
    args[[key]] <- val
    i <- i + 1L
  }

  # Resolve ano
  ano_atual <- as.integer(format(Sys.Date(), "%Y"))
  years <- if (args[["years"]] == "last30") {
    max(1996L, ano_atual - 29L):ano_atual
  } else {
    as.integer(eval(parse(text = args[["years"]])))
  }
  # Garante piso em 1996 (limite do microdatasus)
  years <- years[years >= 1996L]

  list(
    output_dir    = path.expand(args[["output-dir"]]),
    systems       = args[["systems"]],
    years         = years,
    ufs           = args[["ufs"]],
    months        = as.integer(eval(parse(text = args[["months"]]))),
    workers       = as.integer(args[["workers"]]),
    skip_existing = isTRUE(as.logical(args[["skip-existing"]])),
    verbose       = isTRUE(as.logical(args[["verbose"]]))
  )
}

# =============================================================================
# RESOLUÇÃO DE SISTEMAS E UFs
# =============================================================================

resolve_systems <- function(arg) {
  if (identical(arg, "all")) return(.ALL_SYSTEMS)
  tokens <- toupper(trimws(strsplit(arg, ",")[[1]]))
  out <- character(0)
  for (t in tokens) {
    if (t %in% names(.FAMILY_MAP)) {
      out <- c(out, .FAMILY_MAP[[t]])
    } else if (t %in% .ALL_SYSTEMS) {
      out <- c(out, t)
    } else {
      cli::cli_alert_warning("Sistema/família desconhecido ignorado: '{t}'")
    }
  }
  unique(out)
}

resolve_ufs <- function(arg) {
  if (identical(arg, "all")) return(.ALL_UFS)
  key <- tolower(trimws(arg))
  if (key %in% names(.REGIONS)) return(.REGIONS[[key]])
  ufs <- toupper(trimws(strsplit(arg, ",")[[1]]))
  bad <- setdiff(ufs, .ALL_UFS)
  if (length(bad)) cli::cli_alert_warning("UFs inválidas ignoradas: {paste(bad, collapse=', ')}")
  intersect(ufs, .ALL_UFS)
}

# =============================================================================
# MATRIZ DE TAREFAS
# =============================================================================

build_task_matrix <- function(systems, ufs, years, months) {
  rows <- list()

  for (sys in systems) {
    is_national <- sys %in% .SYS_ANNUAL_NATIONAL
    is_monthly  <- sys %in% .SYS_MONTHLY

    if (is_national) {
      for (yr in years)
        rows[[length(rows) + 1L]] <- list(
          system = sys, uf = "BR", year = yr, month = NA_integer_,
          national = TRUE
        )
    } else if (is_monthly) {
      for (uf in ufs) for (yr in years) for (mo in months)
        rows[[length(rows) + 1L]] <- list(
          system = sys, uf = uf, year = yr, month = mo,
          national = FALSE
        )
    } else {
      for (uf in ufs) for (yr in years)
        rows[[length(rows) + 1L]] <- list(
          system = sys, uf = uf, year = yr, month = NA_integer_,
          national = FALSE
        )
    }
  }
  rows
}

# =============================================================================
# CAMINHOS E MANIFESTO
# =============================================================================

task_id_str <- function(system, uf, year, month) {
  paste(system, uf, year,
        if (!is.na(month)) sprintf("%02d", month) else "NA",
        sep = "_")
}

task_output_path <- function(output_dir, task) {
  parts  <- strsplit(task$system, "-")[[1]]
  family <- tolower(parts[1])
  sub    <- if (length(parts) > 1) paste(parts[-1], collapse = "-") else ""
  seg    <- if (nchar(sub) > 0) file.path(family, sub) else family

  yr_seg <- paste0("year=", task$year)
  mo_seg <- if (!is.na(task$month)) paste0("month=", sprintf("%02d", task$month)) else NULL

  if (task$national) {
    do.call(file.path, c(list(output_dir, seg, yr_seg), as.list(mo_seg), list("data.parquet")))
  } else {
    uf_seg <- paste0("uf=", task$uf)
    do.call(file.path, c(list(output_dir, seg, uf_seg, yr_seg), as.list(mo_seg), list("data.parquet")))
  }
}

manifest_file <- function(output_dir) file.path(output_dir, "manifest.csv")

load_manifest <- function(output_dir) {
  mp <- manifest_file(output_dir)
  if (file.exists(mp)) {
    utils::read.csv(mp, stringsAsFactors = FALSE,
                    colClasses = c(month = "integer", year = "integer",
                                   rows = "integer", size_kb = "numeric"))
  } else {
    data.frame(
      task_id = character(), system = character(), uf = character(),
      year = integer(), month = integer(), status = character(),
      file_path = character(), rows = integer(), size_kb = numeric(),
      downloaded_at = character(), error_msg = character(),
      stringsAsFactors = FALSE
    )
  }
}

save_manifest <- function(manifest, output_dir) {
  utils::write.csv(manifest, manifest_file(output_dir),
                   row.names = FALSE, quote = TRUE)
}

make_record <- function(task, status, rows = NA_integer_,
                        size_kb = NA_real_, error_msg = NA_character_) {
  data.frame(
    task_id       = task_id_str(task$system, task$uf, task$year, task$month),
    system        = task$system,
    uf            = task$uf,
    year          = task$year,
    month         = task$month,
    status        = status,
    file_path     = NA_character_,
    rows          = rows,
    size_kb       = size_kb,
    downloaded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    error_msg     = error_msg,
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# DOWNLOAD DE UMA TAREFA
# Retorna data.frame de uma linha para o manifesto.
# Nunca joga erro — captura tudo e registra status.
# =============================================================================

download_task <- function(task, output_dir, skip_existing, verbose) {
  tid   <- task_id_str(task$system, task$uf, task$year, task$month)
  fpath <- task_output_path(output_dir, task)

  # ── 1. Pular se já existe ──────────────────────────────────────────────────
  if (skip_existing && file.exists(fpath) && file.size(fpath) > 0L) {
    if (verbose) cli::cli_alert_success("[SKIP] {tid}")
    rec <- make_record(task, "done",
                       size_kb = round(file.size(fpath) / 1024, 1))
    rec$file_path <- fpath
    return(rec)
  }

  # ── 2. Tentar download ──────────────────────────────────────────────────────
  if (verbose) {
    periodo <- if (!is.na(task$month)) {
      paste0(task$year, "/", sprintf("%02d", task$month))
    } else as.character(task$year)
    cli::cli_alert_info("[DOWN] {task$system} | {task$uf} | {periodo}")
  }

  # Para SINAN nacional: qualquer UF serve (microdatasus retorna BR todo)
  uf_param <- if (task$national) "RO" else task$uf
  mo_param <- if (is.na(task$month)) NULL else task$month

  df <- tryCatch({
    raw <- sus_data_import(
      system    = task$system,
      uf        = uf_param,
      year      = task$year,
      month     = mo_param,
      use_cache = TRUE,
      backend   = "tibble",
      verbose   = FALSE
    )
    # Materializa se vier como lazy Arrow
    if (inherits(raw, c("arrow_dplyr_query", "Dataset", "ArrowTabular", "Table"))) {
      dplyr::collect(raw)
    } else {
      raw
    }
  }, error = function(e) {
    list(error = conditionMessage(e))
  })

  # ── 3. Erro de rede / FTP → registra "error", continua ────────────────────
  if (is.list(df) && !is.data.frame(df)) {
    cli::cli_alert_danger("[FAIL] {tid}: {df$error}")
    return(make_record(task, "error", error_msg = df$error))
  }

  # ── 4. Sem dado disponível → "empty", continua ────────────────────────────
  if (is.null(df) || nrow(df) == 0L) {
    if (verbose) cli::cli_alert_warning("[EMPTY] {tid} — sem dados para este período")
    return(make_record(task, "empty", rows = 0L,
                       error_msg = "0 rows (periodo indisponivel no DATASUS)"))
  }

  # ── 5. Sucesso → grava Parquet ─────────────────────────────────────────────
  dir.create(dirname(fpath), recursive = TRUE, showWarnings = FALSE)
  tryCatch(
    climasus4r:::.write_parquet_smart(as.data.frame(df), fpath),
    error = function(e) {
      cli::cli_alert_danger("[WRITE FAIL] {tid}: {conditionMessage(e)}")
      stop(e)
    }
  )

  sz  <- round(file.size(fpath) / 1024, 1)
  nr  <- nrow(df)
  if (verbose) cli::cli_alert_success("[OK] {tid} — {nr} linhas | {sz} KB")

  rec <- make_record(task, "done", rows = nr, size_kb = sz)
  rec$file_path <- fpath
  rec
}

# =============================================================================
# ESTIMATIVA DE TAMANHO
# =============================================================================

print_size_estimate <- function(tasks) {
  n_ann  <- sum(sapply(tasks, function(t) !t$national && is.na(t$month)))
  n_mon  <- sum(sapply(tasks, function(t) !t$national && !is.na(t$month)))
  n_nat  <- sum(sapply(tasks, function(t) t$national))

  cli::cli_h2("Tarefas geradas")
  cli::cli_text("  Anuais por UF    : {n_ann}")
  cli::cli_text("  Mensais por UF   : {n_mon}")
  cli::cli_text("  Nacionais (SINAN): {n_nat}")
  cli::cli_text("  TOTAL            : {length(tasks)}")

  est_gb <- round(n_ann * 0.005 + n_mon * 0.010 + n_nat * 0.020, 1)
  cli::cli_text("")
  cli::cli_alert_info("Estimativa de armazenamento: ~{est_gb} GB (Parquet comprimido)")
  cli::cli_alert_info("SIA representa 60-70% do volume se incluída — considere --systems SIM,SIH,SINAN,SINASC para início")
}

# =============================================================================
# EXECUÇÃO PRINCIPAL
# =============================================================================

main <- function() {
  args <- parse_args()

  cli::cli_h1("climasus4r — Download Nacional DATASUS")
  cli::cli_text("Output  : {args$output_dir}")
  cli::cli_text("Anos    : {min(args$years)}–{max(args$years)} ({length(args$years)} anos)")
  cli::cli_text("Workers : {args$workers}")

  systems <- resolve_systems(args$systems)
  ufs     <- resolve_ufs(args$ufs)

  cli::cli_text("Sistemas: {length(systems)} ({paste(unique(sub('-.*','',systems)),collapse=', ')})")
  cli::cli_text("UFs     : {length(ufs)} ({paste(head(ufs,5),collapse=',')}{if(length(ufs)>5) ',...' else ''})")

  dir.create(args$output_dir, recursive = TRUE, showWarnings = FALSE)

  # Gerar e exibir estimativa
  tasks <- build_task_matrix(systems, ufs, args$years, args$months)
  print_size_estimate(tasks)

  # Carregar manifesto (retoma downloads interrompidos)
  manifest <- load_manifest(args$output_dir)

  done_ids <- manifest$task_id[manifest$status == "done"]
  pending  <- Filter(function(t) {
    !(task_id_str(t$system, t$uf, t$year, t$month) %in% done_ids)
  }, tasks)

  cli::cli_text("")
  cli::cli_alert_info("{length(pending)} tarefas pendentes de {length(tasks)} total")

  if (length(pending) == 0L) {
    cli::cli_alert_success("Tudo já baixado! Dataset em: {args$output_dir}")
    return(invisible(manifest))
  }

  # Configurar paralelismo
  use_parallel <- args$workers > 1L &&
    requireNamespace("future",  quietly = TRUE) &&
    requireNamespace("furrr",   quietly = TRUE)

  if (use_parallel) {
    future::plan(future::multisession, workers = args$workers)
    on.exit(future::plan(future::sequential), add = TRUE)
  } else if (args$workers > 1L) {
    cli::cli_alert_warning("'future'/'furrr' não encontrados — modo sequencial")
  }

  cli::cli_h2("Iniciando downloads")
  t0 <- proc.time()

  results <- if (use_parallel) {
    furrr::future_map(
      pending,
      function(t) download_task(t, args$output_dir, args$skip_existing, args$verbose),
      .options = furrr::furrr_options(seed = TRUE)
    )
  } else {
    lapply(pending, function(t) {
      download_task(t, args$output_dir, args$skip_existing, args$verbose)
    })
  }

  # Atualizar manifesto
  new_rows <- do.call(rbind, results)
  manifest <- rbind(manifest, new_rows)
  save_manifest(manifest, args$output_dir)

  # ── Relatório final ──────────────────────────────────────────────────────────
  elapsed  <- round((proc.time() - t0)["elapsed"] / 60, 1)
  n_done   <- sum(manifest$status == "done")
  n_empty  <- sum(manifest$status == "empty")
  n_error  <- sum(manifest$status == "error")
  total_gb <- round(sum(manifest$size_kb, na.rm = TRUE) / 1024^2, 2)

  cli::cli_rule()
  cli::cli_h1("RELATÓRIO FINAL")
  cli::cli_alert_success("Sucesso (done) : {n_done}")
  cli::cli_alert_warning("Vazio  (empty) : {n_empty}  ← período indisponível no DATASUS")
  cli::cli_alert_danger( "Erro   (error) : {n_error}  ← candidatos a retry manual")
  cli::cli_text("Tempo          : {elapsed} minutos")
  cli::cli_text("Total em disco : {total_gb} GB")
  cli::cli_text("Manifesto      : {manifest_file(args$output_dir)}")
  cli::cli_rule()

  if (n_error > 0L) {
    cli::cli_h2("Erros registrados (candidates para --retry-errors)")
    erros <- manifest[manifest$status == "error", c("task_id", "error_msg")]
    print(erros, row.names = FALSE)
  }

  cli::cli_h2("Como usar o dataset")
  cli::cli_code(c(
    'library(arrow)',
    'library(dplyr)',
    '',
    paste0('# Abre lazily (sem carregar na memoria)'),
    paste0('ds <- arrow::open_dataset("', args$output_dir, '/sim/DO/")'),
    '',
    '# Filtra e processa so o necessario',
    'ds |>',
    '  filter(year >= 2015) |>',
    '  collect() |>',
    '  sus_data_standardize() |>',
    '  sus_data_aggregate(time_unit = "year")'
  ))

  invisible(manifest)
}

main()
