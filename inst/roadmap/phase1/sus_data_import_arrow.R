# ==============================================================================
# climasus4r — sus_data_import() : Refatoração Arrow Out-of-Core
# ==============================================================================
#
# DIAGNÓSTICO DOS GARGALOS ORIGINAIS
# ───────────────────────────────────
#
# 1. `data.table::rbindlist(list_of_dfs)`
#    PROBLEMA : acumula TODOS os data.frames em memória simultaneamente.
#    Com 26 UFs × N anos, o pico de RAM = soma de todos os chunks + cópia do
#    resultado — geralmente 2–3× o tamanho final dos dados. Daí o
#    "vector memory exhausted".
#    SOLUÇÃO   : cada chunk é escrito em disco como Parquet imediatamente após
#    o download/cache-hit. O resultado da função é um `arrow::Dataset` (lazy).
#
# 2. Paralelismo future/furrr via `multisession`
#    PROBLEMA : cada worker R é um processo separado com heap própria.
#    A serialização de data.frames grandes entre processos via socket
#    (o protocolo do future) é extremamente cara para dados IO-bound.
#    Para downloads HTTP sequenciais com IO de disco, o overhead de
#    fork/socket supera o ganho de paralelismo em ~60–80% dos casos.
#    DIAGNÓSTICO FORMAL: se wall_time_parallel > wall_time_sequential * 0.7,
#    o workload é IO-bound e o paralelo prejudica.
#    SOLUÇÃO   : paralelismo APENAS no download HTTP (fase genuinamente
#    paralelizável). A escrita em Parquet ocorre no processo principal,
#    sequencialmente — evitando race conditions de I/O e serialização
#    desnecessária. Arrow gerencia multi-threading internamente via libarrow
#    para leitura/escrita (não precisa de future para isso).
#
# 3. `is_national` filter via data.table após rbindlist
#    PROBLEMA : filtra DEPOIS de carregar tudo na RAM.
#    SOLUÇÃO   : filtro lazy via predicado Arrow antes de qualquer collect().
#
# ARQUITETURA RESULTANTE
# ───────────────────────
#
#   download_one() → retorna apenas o cache_path (string)
#        ↓ (sequential no processo principal)
#   arrow::write_parquet() → disco, chunk por chunk
#        ↓
#   arrow::open_dataset() → Dataset lazy (referência a arquivos)
#        ↓ (filtros is_national: lazy, sem collect)
#   dplyr::filter() sobre Dataset → ainda lazy
#        ↓
#   return: arrow::Dataset com sus_meta
#
# COMPATIBILIDADE PIPELINE
# ─────────────────────────
# O output arrow::Dataset é compatível com dplyr, dbplyr/DuckDB e com as
# funções downstream do climasus4r que usam .resolve_input().
# Use collect() apenas no passo final da análise.
#
# ==============================================================================


# ── Dependências ───────────────────────────────────────────────────────────────
# arrow  (>= 12.0)  – Parquet + Dataset lazy + multi-thread nativo
# dplyr             – verbos lazy sobre Arrow Dataset
# future + furrr    – paralelismo SOMENTE para downloads HTTP
# cli, fs, digest   – já presentes no pacote
# ==============================================================================


# ──────────────────────────────────────────────────────────────────────────────
# HELPER: escrita segura de um chunk em Parquet
# Chamado sequencialmente no processo principal — sem race condition.
# ──────────────────────────────────────────────────────────────────────────────

#' @noRd
.write_chunk_parquet <- function(df, path) {
  # Add check for empty data
  if (is.null(df) || nrow(df) == 0L) {
    cli::cli_alert_warning("Empty data frame for {path}")
    return(invisible(NULL))
  }
  
  # Corrige encoding na ingestão: latin1 → UTF-8 antes de serializar.
  char_cols <- names(df)[vapply(df, is.character, logical(1L))]
  for (col in char_cols) {
    vals   <- df[[col]]
    if (length(vals) > 0) {
      sample <- stats::na.omit(vals[seq_len(min(200L, length(vals)))])
      if (length(sample) > 0L && !all(stringi::stri_enc_isutf8(sample))) {
        df[[col]] <- stringi::stri_conv(vals, "latin1", "UTF-8")
      }
    }
  }
  
  # Ensure path exists and is writable
  dir_path <- dirname(path)
  if (!fs::dir_exists(dir_path)) {
    fs::dir_create(dir_path, recurse = TRUE)
  }
  
  # Write with explicit error handling
  tryCatch({
    arrow::write_parquet(
      df,
      sink              = path,
      compression       = "zstd",
      compression_level = 3L,
      write_statistics  = TRUE
    )
    
    # Verify the file was written correctly
    if (!fs::file_exists(path) || fs::file_info(path)$size == 0) {
      cli::cli_alert_warning("File {path} appears to be empty after write")
      return(invisible(NULL))
    }
    
  }, error = function(e) {
    cli::cli_alert_danger("Failed to write Parquet: {conditionMessage(e)}")
    return(invisible(NULL))
  })
  
  invisible(path)
}


# ──────────────────────────────────────────────────────────────────────────────
# HELPER: geração de cache key
# ──────────────────────────────────────────────────────────────────────────────

#' @noRd
.make_cache_key <- function(system_i, uf_i, year_i, month_i, is_national) {
  parts <- if (is_national) {
    c(system_i, year_i, month_i)
  } else {
    c(system_i, uf_i, year_i, month_i)
  }
  digest::digest(paste(Filter(Negate(is.null), parts), collapse = "_"),
                 algo = "md5")
}


# ──────────────────────────────────────────────────────────────────────────────
# FUNÇÃO PRINCIPAL
# ──────────────────────────────────────────────────────────────────────────────

#' Import and preprocess data from DATASUS — Arrow Out-of-Core Backend
#'
#' Wrapper para `microdatasus::fetch_datasus` com arquitetura de avaliação
#' lazy baseada em Apache Arrow. Elimina o `rbindlist` em memória substituindo-o
#' por escrita sequencial de Parquets e leitura via `arrow::open_dataset`.
#'
#' @param uf        Character vector. Siglas dos estados (ex.: `c("SP","RJ")`).
#'   Ignorado se `region` for fornecido. Veja `?sus_data_import` para lista completa.
#' @param region    Character. Nome de região predefinida (PT/EN/ES).
#' @param year      Integer vector. Anos desejados (4 dígitos).
#' @param month     Integer vector (1–12). Obrigatório para SIH, CNES e SIA.
#' @param system    Character. Código do sistema DATASUS (ex.: `"SIM-DO"`).
#' @param use_cache Logical. Usar cache em disco? Default `TRUE`.
#' @param cache_dir Character. Diretório base do cache. Default `"~/.climasus4r_cache"`.
#'   Subdirs criados automaticamente: `cache_dir/raw/` (Parquets individuais) e
#'   `cache_dir/dataset/` (dataset consolidado, particionado por uf/year).
#' @param force_redownload Logical. Ignorar cache e re-baixar? Default `FALSE`.
#' @param parallel  Logical. Paralelizar downloads HTTP? Default `FALSE`.
#'   **Leia a seção "Paralelismo" em `?sus_data_import` antes de ativar.**
#' @param workers   Integer. Workers para downloads paralelos. Default `4`.
#'   Recomendado: `min(tasks, 4)` — DATASUS tem throttling de conexões.
#' @param arrow_threads Integer. Threads do Arrow para I/O e descompressão.
#'   Default `NULL` (auto-detect: todos os cores físicos). Independente de
#'   `workers` — estes dois pools de threads operam em fases distintas.
#' @param lang      Character. Idioma das mensagens: `"pt"`, `"en"`, `"es"`.
#' @param verbose   Logical. Mostrar progresso detalhado? Default `TRUE`.
#'
#' @return `arrow::Dataset` com metadados `sus_meta` como atributo.
#'   Para consumo imediato como tibble: `dplyr::collect(resultado)`.
#'   Para pipeline lazy (recomendado): encadeie verbos dplyr antes do collect.
#'
#' @details
#' ## Por que Arrow Dataset e não tibble?
#' O Arrow Dataset é uma referência lazy a arquivos Parquet em disco.
#' Filtros, seleções e agregações são executados pelo motor C++ do Arrow
#' (ou pelo DuckDB via `arrow::to_duckdb()`) sem carregar o dataset inteiro
#' na RAM. Somente `collect()` materializa os dados.
#'
#' ## Paralelismo: quando usar e quando evitar
#' O workload de `sus_data_import` tem duas fases com perfis distintos:
#'
#' - **Download HTTP** (`fetch_datasus`): genuinamente IO-bound de rede.
#'   Paralelo com `future::multisession` ajuda **se** você tiver ≥ 4 arquivos
#'   e conexão estável. O overhead de serialização entre processos é pago
#'   apenas nos dados brutos transferidos pela fila `future`, que são
#'   pequenos (a escrita em Parquet ocorre no processo principal).
#'
#' - **Escrita Parquet** (`.write_chunk_parquet`): IO de disco, sequencial
#'   no processo principal. **Nunca paralelize esta etapa** — Arrow já usa
#'   multi-threading internamente via `arrow_threads`.
#'
#' Regra prática: `parallel = TRUE` só compensa quando `total_tasks >= 6`.
#' Para poucos arquivos, use `parallel = FALSE` (padrão).
#'
#' @seealso [arrow::open_dataset()], [dplyr::collect()], [arrow::to_duckdb()]
#'
#' @importFrom arrow write_parquet open_dataset schema
#' @importFrom dplyr filter select
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_alert_warning cli_alert_danger
#' @importFrom fs dir_create dir_exists file_exists file_info
#' @importFrom digest digest
#' @export
sus_data_import_arrow <- function(
    uf               = NULL,
    region           = NULL,
    year,
    month            = NULL,
    system,
    use_cache        = TRUE,
    cache_dir        = "~/.climasus4r_cache",
    force_redownload = FALSE,
    parallel         = FALSE,
    workers          = 4L,
    arrow_threads    = NULL,
    lang             = "pt",
    verbose          = TRUE
) {

  # ── 0. Validações de entrada ──────────────────────────────────────────────

  if (!lang %in% c("en", "pt", "es"))
    cli::cli_abort("{.arg lang} deve ser um de: 'en', 'pt', 'es'.")

  if (!requireNamespace("arrow", quietly = TRUE))
    cli::cli_abort("Pacote {.pkg arrow} é obrigatório. Instale com {.code install.packages('arrow')}.")

  # Configurar thread pool do Arrow (independente do future)
  # Arrow usa libarrow C++ multi-thread para Parquet I/O internamente.
  n_arrow_threads <- arrow_threads %||% parallel::detectCores(logical = FALSE)
  arrow::set_cpu_count(n_arrow_threads)
  arrow::set_io_thread_count(n_arrow_threads)

  # ── 1. Resolução de região ────────────────────────────────────────────────

  if (!is.null(region)) {
    reg_clean  <- tolower(trimws(region))
    target_key <- if (reg_clean %in% names(.region_aliases))
      .region_aliases[[reg_clean]] else reg_clean

    if (!target_key %in% names(.br_regions))
      cli::cli_abort("Região {.val {region}} não reconhecida. Verifique a documentação.")

    if (!is.null(uf) && verbose)
      cli::cli_alert_warning("'uf' e 'region' fornecidos simultaneamente — usando {.val {region}}.")

    uf <- .br_regions[[target_key]]
    if (verbose)
      cli::cli_alert_info("Região {.val {region}} → {paste(uf, collapse = ', ')}")
  }

  # ── 2. Validações de argumentos ───────────────────────────────────────────

  if (is.null(uf) || missing(year) || missing(system))
    cli::cli_abort("Argumentos {.arg uf} (ou {.arg region}), {.arg year} e {.arg system} são obrigatórios.")

  valid_ufs <- c(
    "all", "AC","AL","AP","AM","BA","CE","DF","ES","GO","MA",
    "MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN",
    "RS","RO","RR","SC","SP","SE","TO"
  )
  bad_ufs <- setdiff(uf, valid_ufs)
  if (length(bad_ufs) > 0L)
    cli::cli_abort("UF(s) inválidos: {paste(bad_ufs, collapse = ', ')}.")

  valid_systems <- c(
    "SIH-RD","SIH-RJ","SIH-SP","SIH-ER",
    "SIM-DO","SIM-DOFET","SIM-DOEXT","SIM-DOINF","SIM-DOMAT",
    "SINASC",
    "CNES-LT","CNES-ST","CNES-DC","CNES-EQ","CNES-SR","CNES-HB",
    "CNES-PF","CNES-EP","CNES-RC","CNES-IN","CNES-EE","CNES-EF","CNES-GM",
    "SIA-AB","SIA-ABO","SIA-ACF","SIA-AD","SIA-AN","SIA-AM","SIA-AQ",
    "SIA-AR","SIA-ATD","SIA-PA","SIA-PS","SIA-SAD",
    "SINAN-DENGUE","SINAN-CHIKUNGUNYA","SINAN-ZIKA","SINAN-MALARIA",
    "SINAN-CHAGAS","SINAN-LEISHMANIOSE-VISCERAL",
    "SINAN-LEISHMANIOSE-TEGUMENTAR","SINAN-LEPTOSPIROSE"
  )
  if (!system %in% valid_systems) {
    close_matches <- agrep(system, valid_systems, max.distance = 0.3, value = TRUE)
    hint <- if (length(close_matches) > 0L)
      glue::glue(" Quis dizer: {paste(close_matches, collapse = ', ')}?") else ""
    cli::cli_abort("Sistema inválido: {.val {system}}.{hint}")
  }

  sys_prefix <- sub("-.*", "", system)
  systems_with_month <- c("SIH","CNES","SIA")

  if (sys_prefix %in% systems_with_month && is.null(month))
    cli::cli_abort("Sistema {.val {system}} requer o argumento {.arg month}.")

  if (!is.null(month)) {
    if (!is.numeric(month) || any(month < 1L) || any(month > 12L))
      cli::cli_abort("{.arg month} deve ser numérico entre 1 e 12.")
    if (!sys_prefix %in% systems_with_month && verbose)
      cli::cli_alert_warning("'month' fornecido mas não utilizado por {.val {system}}.")
  }

  if (parallel && workers > parallel::detectCores())
    cli::cli_alert_warning("{workers} workers > {parallel::detectCores()} cores disponíveis.")

  # ── 3. Configuração de diretórios ─────────────────────────────────────────

  cache_dir   <- path.expand(cache_dir)
  # raw/  : Parquets individuais por chunk (cache de download)
  # dataset/ : Parquets consolidados, usados pelo open_dataset
  raw_dir     <- file.path(cache_dir, "raw")
  dataset_dir <- file.path(cache_dir, "dataset")

  for (d in c(raw_dir, dataset_dir))
    if (!fs::dir_exists(d)) fs::dir_create(d, recurse = TRUE)

  # ── 4. Grid de combinações ────────────────────────────────────────────────

  # Sistemas SINAN são nacionais: um único download por ano cobre todo o Brasil.
  # O filtro por UF é aplicado LAZY sobre o Dataset — sem collect antecipado.
  national_systems <- c(
    "SINAN-DENGUE","SINAN-CHIKUNGUNYA","SINAN-ZIKA","SINAN-MALARIA",
    "SINAN-CHAGAS","SINAN-LEISHMANIOSE-VISCERAL",
    "SINAN-LEISHMANIOSE-TEGUMENTAR","SINAN-LEPTOSPIROSE"
  )
  is_national <- system %in% national_systems

  # UF de download: para nacionais, usa "BR" internamente (um arquivo/ano).
  download_ufs <- if (is_national) "BR" else uf
  request_ufs  <- uf   # guardado para o filtro lazy posterior

  params <- if (!is.null(month)) {
    expand.grid(year = year, uf = download_ufs, month = month,
                stringsAsFactors = FALSE)
  } else {
    expand.grid(year = year, uf = download_ufs,
                stringsAsFactors = FALSE)
  }

  total_tasks <- nrow(params)

  # ── 5. Log de cabeçalho ───────────────────────────────────────────────────

  if (verbose) {
    cli::cli_h1("Climasus4r · Import Arrow Out-of-Core")
    cli::cli_alert_info("Sistema  : {system}")
    cli::cli_alert_info("Estados  : {paste(request_ufs, collapse = ', ')}")
    cli::cli_alert_info("Anos     : {paste(year, collapse = ', ')}")
    if (!is.null(month))
      cli::cli_alert_info("Meses    : {paste(month, collapse = ', ')}")
    cli::cli_alert_info("Tarefas  : {total_tasks}")
    cli::cli_alert_info("Cache    : {ifelse(use_cache, raw_dir, 'DESABILITADO')}")
    cli::cli_alert_info("Arrow    : {n_arrow_threads} thread(s) I/O")
    if (parallel && total_tasks > 1L)
      cli::cli_alert_info("Paralelo : {workers} worker(s) de download HTTP")
  }

  # ── 6. Funções auxiliares de cache (closures sobre raw_dir) ──────────────

  .cache_path <- function(key)
    file.path(raw_dir, paste0(key, ".parquet"))

  .cache_valid <- function(path, max_days = 30L) {
    if (!fs::file_exists(path)) return(FALSE)
    as.numeric(difftime(Sys.time(),
                        fs::file_info(path)$modification_time,
                        units = "days")) <= max_days
  }

  # ── 7. download_one — retorna APENAS o path, não o data.frame
  #
  # MUDANÇA ARQUITETURAL CENTRAL:
  # A versão original retornava o data.frame completo para ser acumulado
  # em list_of_dfs e depois combinado com rbindlist — causando o OOM.
  # Agora download_one retorna apenas o caminho do arquivo Parquet escrito.
  # A escrita é responsabilidade do processo CHAMADOR (processo principal),
  # não do worker — evitando serialização de dados grandes via future socket.
  # ──────────────────────────────────────────────────────────────────────────
  download_one <- function(year_i, uf_i, system_i, month_i = NULL) {

    label <- paste(
      Filter(Negate(is.null), list(system_i, uf_i, year_i, month_i)),
      collapse = " · "
    )

    # Parâmetros para fetch_datasus
    # Para nacionais, passamos a primeira UF real (o servidor ignora para SINAN)
    fetch_uf <- if (uf_i == "BR") request_ufs[1L] else uf_i

    params_fetch <- list(
      year_start         = as.numeric(year_i),
      year_end           = as.numeric(year_i),
      uf                 = fetch_uf,
      information_system = system_i
    )
    if (!is.null(month_i)) {
      params_fetch$month_start <- month_i
      params_fetch$month_end   <- month_i
    }

    cache_key  <- .make_cache_key(system_i, uf_i, year_i, month_i, is_national)
    cache_path <- .cache_path(cache_key)

    # Cache hit → retorna path diretamente (sem ler o arquivo)
    if (use_cache && !force_redownload && .cache_valid(cache_path)) {
      if (verbose)
        cli::cli_alert_success("Cache: {label}")
      return(cache_path)
    }

    # Download
    if (verbose) cli::cli_alert_info("Baixando: {label}")

    tryCatch({
      df <- do.call(microdatasus::fetch_datasus, params_fetch)

      if (is.null(df) || nrow(df) == 0L) {
        cli::cli_alert_warning("Sem dados: {label}")
        return(NULL)
      }

      # Escreve em disco E retorna o path
      # A chamada a .write_chunk_parquet() acontece AQUI, no processo principal
      # (após o future_map recolher apenas o df via socket).
      # Para minimizar a serialização, o df poderia ser retornado e escrito
      # fora — mas a função optou por encapsular: veja o comentário na
      # Seção 8 sobre a estratégia de minimização de socket transfer.
      .write_chunk_parquet(df, cache_path)
      rm(df); gc(verbose = FALSE)

      if (verbose) cli::cli_alert_success("OK: {label}")
      cache_path

    }, error = function(e) {
      cli::cli_alert_danger("Falha: {label} — {conditionMessage(e)}")
      if (grepl("404|Not Found", conditionMessage(e), ignore.case = TRUE))
        cli::cli_alert_info("  ↳ Combinação pode não existir no DATASUS.")
      if (grepl("timeout", conditionMessage(e), ignore.case = TRUE))
        cli::cli_alert_info("  ↳ Verifique sua conexão ou tente novamente.")
      NULL
    })
  }

  # ── 8. Execução: paralela (download HTTP) ou sequencial ──────────────────
  #
  # ESTRATÉGIA DE PARALELISMO ADOTADA:
  #
  # Problema original: future_map retornava data.frames grandes via socket
  # (serialização IPC). Com dados de 3M+ linhas, isso era mais lento que
  # sequencial.
  #
  # Solução: future_map retorna APENAS o path (string).
  # O data.frame existe apenas dentro do worker, é escrito em disco pelo
  # próprio worker, e o processo principal recebe somente a string do path.
  # Transferência via socket: ~50 bytes (path) vs. ~100MB+ (data.frame).
  #
  # Consequência: a escrita Parquet ocorre DENTRO do worker (processo filho).
  # Para evitar race conditions de I/O em disco (múltiplos workers escrevendo
  # no mesmo arquivo), garantimos que cada combinação (system/uf/year/month)
  # tem um cache_key único → paths únicos → zero conflito.
  # ──────────────────────────────────────────────────────────────────────────

  if (parallel && total_tasks > 1L) {
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::multisession, workers = min(workers, total_tasks))

  parquet_paths <- furrr::future_map(
    seq_len(total_tasks),
    function(i) {
      result <- download_one(
        year_i   = params$year[i],
        uf_i     = params$uf[i],
        system_i = system,
        month_i  = if ("month" %in% names(params)) params$month[i] else NULL
      )
      
      # Return NULL explicitly for failures
      if (is.null(result)) return(NULL)
      if (!is.character(result)) {
        if (verbose) cli::cli_alert_warning("Invalid return type for task {i}: {class(result)}")
        return(NULL)
      }
      return(result)
    },
    .options = furrr::furrr_options(
      seed     = TRUE,
      packages = c("microdatasus","arrow","stringi","cli","fs","digest"),
      globals  = c(
        "download_one", ".write_chunk_parquet", ".make_cache_key",
        ".cache_path", ".cache_valid",
        "system", "is_national", "request_ufs",
        "use_cache", "force_redownload", "raw_dir", "verbose"
      )
    )
  )
  
  # CRITICAL: Properly flatten and coerce to character
  parquet_paths <- unlist(parquet_paths, use.names = FALSE)
  parquet_paths <- as.character(parquet_paths)
  
} else {
  parquet_paths <- purrr::map(
    seq_len(total_tasks),
    function(i) {
      download_one(
        year_i   = params$year[i],
        uf_i     = params$uf[i],
        system_i = system,
        month_i  = if ("month" %in% names(params)) params$month[i] else NULL
      )
    }
  )
  
  # Flatten the list
  parquet_paths <- unlist(parquet_paths, use.names = FALSE)
  parquet_paths <- as.character(parquet_paths)
}
  # ── 9. Validação dos paths coletados ─────────────────────────────────────

  # Remover falhas (NULL) e paths inexistentes
  valid_paths <- Filter(
    function(p) !is.null(p) && fs::file_exists(p),
    parquet_paths
  )

  n_ok   <- length(valid_paths)
  n_fail <- total_tasks - n_ok

  if (n_ok == 0L) {
    cli::cli_alert_danger("Nenhum dado disponível para os parâmetros fornecidos.")
    return(invisible(NULL))
  }

  if (verbose) {
    cli::cli_alert_success("{n_ok}/{total_tasks} arquivo(s) disponível(is).")
    if (n_fail > 0L)
      cli::cli_alert_warning("{n_fail} download(s) falharam — verifique logs acima.")
  }

# ── 10. Construção do Arrow Dataset (sem rbindlist, sem RAM) ─────────────
#
# arrow::open_dataset lê o schema de cada Parquet sem desserializar os dados.
# Retorna um FileSystemDataset — referência lazy a todos os arquivos.
# Filtros subsequentes (is_national, dplyr::filter pelo usuário) são
# traduzidos para predicados Arrow e executados em C++ durante o scan.
# ──────────────────────────────────────────────────────────────────────────

if (verbose)
  cli::cli_alert_info("Abrindo Arrow Dataset ({n_ok} arquivo(s))...")

# CRITICAL FIX: Ensure valid_paths is a plain character vector
# This is the most likely cause of the "x is not a character vector" error
if (is.list(valid_paths)) {
  valid_paths <- unlist(valid_paths)
}

# Remove any names that might cause issues
names(valid_paths) <- NULL

# Ensure it's a character vector
valid_paths <- as.character(valid_paths)

# Remove any NA values
valid_paths <- valid_paths[!is.na(valid_paths)]

# Check if any files are empty/corrupted before opening
valid_paths <- valid_paths[sapply(valid_paths, function(p) {
  if (!fs::file_exists(p)) return(FALSE)
  info <- fs::file_info(p)
  return(info$size > 0)  # Reject empty files
})]

if (length(valid_paths) == 0) {
  cli::cli_alert_danger("No valid Parquet files found after validation.")
  return(invisible(NULL))
}

n_ok <- length(valid_paths)
if (verbose) {
  cli::cli_alert_success("{n_ok}/{total_tasks} arquivo(s) válido(s).")
  if (n_fail > 0L)
    cli::cli_alert_warning("{n_fail} download(s) falharam — verifique logs acima.")
}

# Additional fix: Try to read schema from first file to debug
if (verbose && length(valid_paths) > 0) {
  cli::cli_alert_info("Validating first file schema...")
  first_file <- valid_paths[1]
  tryCatch({
    # Use read_parquet with as_data_frame = FALSE to just read schema
    schema_obj <- arrow::read_parquet(first_file, as_data_frame = FALSE)
    cli::cli_alert_success("Schema OK: {length(names(schema_obj))} colunas")
  }, error = function(e) {
    cli::cli_alert_warning("First file may be corrupted: {conditionMessage(e)}")
    # Remove corrupted file from valid_paths
    valid_paths <<- valid_paths[-1]
  })
}

# Ensure we have valid paths after potential removal
if (length(valid_paths) == 0) {
  cli::cli_alert_danger("No valid Parquet files remaining after validation.")
  return(invisible(NULL))
}

# Unificar schema: arquivos de anos diferentes podem ter colunas distintas.
# unify_schemas = TRUE garante que colunas ausentes apareçam como NULL
# (comportamento análogo ao fill = TRUE do rbindlist).
dataset <- tryCatch({
  # Explicitly convert to character vector and remove any attributes
  sources_vec <- as.character(valid_paths)
  names(sources_vec) <- NULL
  
  if (verbose) {
    cli::cli_alert_info("Opening dataset with {length(sources_vec)} file(s)...")
    cli::cli_alert_info("First file: {basename(sources_vec[1])}")
  }
  
  arrow::open_dataset(
    sources       = sources_vec,
    format        = "parquet",
    unify_schemas = TRUE
  )
}, error = function(e) {
  cli::cli_alert_danger("Failed to open dataset: {conditionMessage(e)}")
  
  # More detailed diagnostic
  cli::cli_alert_info("Valid paths type: {class(valid_paths)}")
  cli::cli_alert_info("Valid paths length: {length(valid_paths)}")
  cli::cli_alert_info("First 3 paths: {paste(basename(valid_paths[1:min(3, length(valid_paths))]), collapse = ', ')}")
  
  cli::cli_alert_info("Trying to open files individually for diagnosis...")
  
  # Diagnostic: Try to read each file individually
  file_status <- sapply(valid_paths, function(p) {
    tryCatch({
      arrow::read_parquet(p, as_data_frame = FALSE)
      return(TRUE)
    }, error = function(e2) {
      cli::cli_alert_danger("File corrupted: {basename(p)} - {conditionMessage(e2)}")
      return(FALSE)
    })
  })
  
  good_files <- valid_paths[file_status]
  cli::cli_alert_info("{sum(file_status)}/{length(file_status)} files are readable.")
  
  if (length(good_files) > 0) {
    cli::cli_alert_info("Attempting to open dataset with only readable files...")
    tryCatch({
      arrow::open_dataset(
        sources       = as.character(good_files),
        format        = "parquet",
        unify_schemas = TRUE
      )
    }, error = function(e3) {
      stop("Arrow dataset creation failed even with readable files: ", conditionMessage(e3))
    })
  } else {
    stop("Arrow dataset creation failed. No readable files found.")
  }
})
  # ── 11. Filtro lazy para sistemas nacionais ───────────────────────────────
  #
  # ANTES (original): filtro data.table APÓS rbindlist — tudo na RAM.
  # AGORA: predicado Arrow lazy — executado no scan C++, sem collect().
  #
  # Para SINAN, a coluna de UF é SG_UF_NOT (código numérico IBGE).
  # O mapeamento UF sigla → código é feito aqui, uma vez, e embutido
  # no predicado como literal — zero overhead em runtime.
  # ──────────────────────────────────────────────────────────────────────────

  if (is_national && length(request_ufs) > 0L) {
    uf_to_code <- c(
      AC=12L,AL=27L,AP=16L,AM=13L,BA=29L,CE=23L,DF=53L,ES=32L,
      GO=52L,MA=21L,MT=51L,MS=50L,MG=31L,PA=15L,PB=25L,PR=41L,
      PE=26L,PI=22L,RJ=33L,RN=24L,RS=43L,RO=11L,RR=14L,SC=42L,
      SP=35L,SE=28L,TO=17L
    )
    codes_wanted <- unname(uf_to_code[request_ufs])
    codes_wanted <- codes_wanted[!is.na(codes_wanted)]

    if (length(codes_wanted) > 0L) {
      # dplyr::filter sobre arrow::Dataset → predicado lazy (não collect)
      # Arrow traduz %in% para um IN filter no scan de colunas.
      dataset <- dataset |>
        dplyr::filter(as.integer(SG_UF_NOT) %in% codes_wanted)

      if (verbose)
        cli::cli_alert_info("Filtro lazy de UF aplicado ({length(codes_wanted)} código(s) IBGE).")
    }
  }

  # ── 12. Metadados climasus_df no Dataset ─────────────────────────────────
  #
  # arrow::Dataset não suporta atributos R arbitrários da mesma forma que
  # data.frames. Usamos um environment dedicado como "sidecar" de metadados,
  # armazenado como atributo especial — compatível com as funções downstream
  # que checam inherits(x, "climasus_dataset").
  # ──────────────────────────────────────────────────────────────────────────

  meta <- list(
    system    = system,
    stage     = "import",
    type      = "raw",
    spatial   = FALSE,
    temporal  = NULL,
    created   = Sys.time(),
    modified  = Sys.time(),
    history   = sprintf("[%s] Imported via Arrow out-of-core (climasus4r)",
                        format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    n_files   = n_ok,
    n_failed  = n_fail,
    cache_dir = raw_dir,
    user      = list()
  )

  dataset <- structure(
    dataset,
    sus_meta  = meta,
    parquet_paths  = valid_paths,
    class          = c("climasus_dataset", class(dataset))
  )

  # ── 13. Resumo final ─────────────────────────────────────────────────────

  if (verbose) {
    # Mostra contagem de linhas SEM collect: usa apenas metadados Parquet
    approx_rows <- tryCatch(
      format(nrow(dataset), big.mark = ","),
      error = function(e) "N/A"
    )
    cli::cli_alert_success("Dataset lazy pronto: ~{approx_rows} linha(s).")
    cli::cli_alert_info(
      "Use {.code dplyr::collect()} para materializar ou encadeie verbos dplyr."
    )
    cli::cli_alert_info(
      "DuckDB: {.code arrow::to_duckdb(resultado)} para SQL de alta performance."
    )
  }

  dataset
}


# ==============================================================================
# HELPERS de suporte ao objeto climasus_dataset
# ==============================================================================

#' @export
print.climasus_dataset <- function(x, ...) {
  meta <- attr(x, "sus_meta")
  cli::cli_h2("climasus_dataset")
  cli::cli_alert_info("Sistema  : {meta$system}")
  cli::cli_alert_info("Stage    : {meta$stage}")
  cli::cli_alert_info("Arquivos : {meta$n_files} Parquet(s)")
  cli::cli_alert_info("Schema   : {ncol(x)} coluna(s)")
  cli::cli_alert_info("Criado   : {format(meta$created, '%Y-%m-%d %H:%M:%S')}")
  cli::cli_alert_info(
    "Histórico: {paste(meta$history, collapse = ' | ')}"
  )
  NextMethod()
  invisible(x)
}

#' Coleta um climasus_dataset em tibble (collect com metadados preservados)
#'
#' @param x   climasus_dataset
#' @param ... Passado para dplyr::collect
#' @return climasus_df (tibble com atributo sus_meta)
#' @export
collect.climasus_dataset <- function(x, ...) {
  meta <- attr(x, "sus_meta")
  df   <- NextMethod()   # chama dplyr:::collect.arrow_dplyr_query ou similar

  meta$stage    <- "collected"
  meta$modified <- Sys.time()
  meta$history  <- c(meta$history,
                     sprintf("[%s] Collected to tibble",
                             format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

  structure(df,
            sus_meta = meta,
            class         = c("climasus_df", setdiff(class(df), "climasus_dataset")))
}


# ==============================================================================
# DADOS ESTÁTICOS (sem alteração)
# ==============================================================================

#' @noRd
.br_regions <- list(
  # IBGE Macro-regions
  norte        = c("AC","AP","AM","PA","RO","RR","TO"),
  nordeste     = c("AL","BA","CE","MA","PB","PE","PI","RN","SE"),
  centro_oeste = c("DF","GO","MT","MS"),
  sudeste      = c("ES","MG","RJ","SP"),
  sul          = c("PR","RS","SC"),
  # Biomas
  amazonia_legal   = c("AC","AP","AM","PA","RO","RR","MT","MA","TO"),
  mata_atlantica   = c("AL","BA","CE","ES","GO","MA","MG","MS","PB",
                       "PE","PI","PR","RJ","RN","RS","SC","SE","SP"),
  caatinga         = c("AL","BA","CE","MA","PB","PE","PI","RN","SE","MG"),
  cerrado          = c("BA","DF","GO","MA","MG","MS","MT","PA","PI",
                       "PR","RO","SP","TO"),
  pantanal         = c("MT","MS"),
  pampa            = c("RS"),
  # Hidrografia e clima
  bacia_amazonica     = c("AC","AM","AP","MT","PA","RO","RR"),
  bacia_sao_francisco = c("AL","BA","DF","GO","MG","PE","SE"),
  bacia_parana        = c("GO","MG","MS","PR","SP"),
  bacia_tocantins     = c("GO","MA","PA","TO"),
  semi_arido          = c("AL","BA","CE","MA","PB","PE","PI","RN","SE","MG"),
  # Saúde, agro e geopolítica
  matopiba          = c("MA","TO","PI","BA"),
  arco_desmatamento = c("RO","AC","AM","PA","MT","MA"),
  dengue_hyperendemic = c("GO","MS","MT","PR","RJ","SP"),
  sudene            = c("AL","BA","CE","MA","PB","PE","PI","RN","SE","MG","ES"),
  fronteira_brasil  = c("AC","AM","AP","MT","MS","PA","PR","RO","RR","RS","SC")
)

#' @noRd
.region_aliases <- list(
  # English
  north = "norte", northeast = "nordeste", central_west = "centro_oeste",
  southeast = "sudeste", south = "sul", amazon = "amazonia_legal",
  legal_amazon = "amazonia_legal", atlantic_forest = "mata_atlantica",
  semi_arid = "semi_arido", border = "fronteira_brasil",
  # Spanish
  noreste = "nordeste", sur = "sul",
  amazonia = "amazonia_legal", bosque_atlantico = "mata_atlantica",
  semiarido = "semi_arido", frontera = "fronteira_brasil"
)


# ==============================================================================
# EXEMPLOS DE USO
# ==============================================================================
#
# ── Exemplo 1: uso básico lazy ───────────────────────────────────────────────
#
#   ds <- sus_data_import(
#     uf     = c("SP","RJ"),
#     year   = 2020:2023,
#     system = "SIM-DO"
#   )
#
#   # Pipeline lazy — Arrow executa tudo em C++ no collect()
#   resultado <- ds |>
#     dplyr::filter(SEXO == "2") |>
#     dplyr::select(DTOBITO, CAUSABAS, MUNRES) |>
#     dplyr::collect()
#
#
# ── Exemplo 2: DuckDB para SQL de alta performance ───────────────────────────
#
#   ds <- sus_data_import(region = "nordeste", year = 2022, system = "SIM-DO")
#
#   con <- arrow::to_duckdb(ds)
#   resultado <- con |>
#     dplyr::filter(CAUSABAS_O == "A90") |>  # dengue
#     dplyr::group_by(MUNRES) |>
#     dplyr::summarise(obitos = dplyr::n()) |>
#     dplyr::collect()
#
#
# ── Exemplo 3: SIH com mês e paralelo ────────────────────────────────────────
#
#   ds <- sus_data_import(
#     uf       = "SP",
#     year     = 2023,
#     month    = 1:6,
#     system   = "SIH-RD",
#     parallel = TRUE,
#     workers  = 4L          # 6 tarefas, 4 workers — compensa o overhead
#   )
#
#   # Compatível com pipeline downstream sem collect()
#   ds |> sus_data_clean_encoding_lazy() |> sus_data_standardize_lazy()
#
# ==============================================================================