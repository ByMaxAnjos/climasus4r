# ==============================================================================
# climasus4r — sus_data_import()
# Backend unificado: Apache Arrow out-of-core + DuckDB lazy
# ==============================================================================
#
# ARQUITETURA
# ───────────
#
#   download_one_raw()          → worker (multisession ou sequencial)
#        │ cache hit            → list(type="path",  value=path)
#        │ download novo        → list(type="df",    value=df, path=path)
#        ↓
#   loop no processo principal  → .write_chunk_parquet() por df novo
#        ↓
#   arrow::open_dataset()       → FileSystemDataset lazy (sem RAM)
#        ↓
#   dplyr::filter() lazy        → filtro de UF para SINAN (predicado C++)
#        ↓
#   return: climasus_dataset    → subclasse de arrow::Dataset
#
# OUTPUT
# ──────
#   • arrow::Dataset  → pipeline lazy (padrão, sem collect)
#   • dplyr verbos encadeados antes de collect() → executados em C++/DuckDB
#   • arrow::to_duckdb(ds) → SQL de alta performance
#   • dplyr::collect(ds)   → materializa como tibble/climasus_df
#
# DEPENDÊNCIAS
# ────────────
#   arrow (>= 12.0), dplyr, future, furrr, purrr
#   cli, fs, digest, stringi, microdatasus
# ==============================================================================


# ──────────────────────────────────────────────────────────────────────────────
# HELPER: escrita segura de um chunk em Parquet
# Sempre chamado no processo principal — sem race condition de I/O.
# ──────────────────────────────────────────────────────────────────────────────

#' @noRd
.write_chunk_parquet <- function(df, path) {
  if (is.null(df) || nrow(df) == 0L) {
    cli::cli_alert_warning("Chunk vazio, ignorado: {basename(path)}")
    return(invisible(NULL))
  }

  # Encoding: latin1 → UTF-8 antes de serializar para Parquet
  char_cols <- names(df)[vapply(df, is.character, logical(1L))]
  for (col in char_cols) {
    vals <- df[[col]]
    if (length(vals) > 0L) {
      sample_vals <- stats::na.omit(vals[seq_len(min(200L, length(vals)))])
      if (length(sample_vals) > 0L &&
          !all(stringi::stri_enc_isutf8(sample_vals))) {
        df[[col]] <- stringi::stri_conv(vals, "latin1", "UTF-8")
      }
    }
  }

  dir_path <- dirname(path)
  if (!fs::dir_exists(dir_path)) fs::dir_create(dir_path, recurse = TRUE)

  # tryCatch retorna flag booleana — evita que path seja retornado após falha
  ok <- tryCatch({
    arrow::write_parquet(
      df,
      sink              = path,
      compression       = "zstd",
      compression_level = 3L,
      write_statistics  = TRUE
    )
    TRUE
  }, error = function(e) {
    cli::cli_alert_danger("Falha ao escrever Parquet: {conditionMessage(e)}")
    FALSE
  })

  if (!ok) return(invisible(NULL))

  if (!fs::file_exists(path) || fs::file_info(path)$size == 0L) {
    cli::cli_alert_warning("Arquivo vazio após escrita: {basename(path)}")
    return(invisible(NULL))
  }

  invisible(path)
}


# ──────────────────────────────────────────────────────────────────────────────
# HELPER: geração de cache key (MD5 dos parâmetros)
# ──────────────────────────────────────────────────────────────────────────────

#' @noRd
.make_cache_key <- function(system_i, uf_i, year_i, month_i, is_national) {
  parts <- if (is_national) {
    c(system_i, as.character(year_i), if (!is.null(month_i)) as.character(month_i))
  } else {
    c(system_i, uf_i, as.character(year_i), if (!is.null(month_i)) as.character(month_i))
  }
  digest::digest(paste(parts, collapse = "_"), algo = "md5")
}


# ──────────────────────────────────────────────────────────────────────────────
# FUNÇÃO PRINCIPAL
# ──────────────────────────────────────────────────────────────────────────────

#' Importa microdados do DATASUS com backend Arrow out-of-core
#'
#' Wrapper para `microdatasus::fetch_datasus` com avaliação lazy baseada em
#' Apache Arrow. Substitui o `rbindlist` em memória por escrita sequencial de
#' Parquets individuais + `arrow::open_dataset`, eliminando picos de RAM.
#' O resultado é um `climasus_dataset` (subclasse de `arrow::Dataset`),
#' compatível com dplyr, DuckDB e funções downstream do climasus4r.
#'
#' @param uf Character vector. Siglas dos estados (ex.: `c("SP","RJ")`).
#'   Ignorado se `region` for fornecido.
#'   Valores válidos: AC, AL, AP, AM, BA, CE, DF, ES, GO, MA, MT, MS, MG, PA,
#'   PB, PR, PE, PI, RJ, RN, RS, RO, RR, SC, SP, SE, TO, ou `"all"`.
#' @param region Character. Nome de região predefinida (PT/EN/ES). Sobrescreve
#'   `uf` quando fornecido. Regiões disponíveis — **IBGE:** `"norte"`,
#'   `"nordeste"`, `"centro_oeste"`, `"sudeste"`, `"sul"`. **Biomas:**
#'   `"amazonia_legal"`, `"mata_atlantica"`, `"caatinga"`, `"cerrado"`,
#'   `"pantanal"`, `"pampa"`. **Hidrografia/clima:** `"bacia_amazonica"`,
#'   `"bacia_sao_francisco"`, `"bacia_parana"`, `"bacia_tocantins"`,
#'   `"semi_arido"`. **Geopolítica/saúde:** `"matopiba"`,
#'   `"arco_desmatamento"`, `"dengue_hyperendemic"`, `"sudene"`,
#'   `"fronteira_brasil"`. Aceita nomes em EN e ES (ex.: `"northeast"`,
#'   `"noreste"`).
#' @param year Integer vector. Anos desejados (4 dígitos).
#' @param month Integer vector (1–12). Obrigatório para SIH, CNES e SIA.
#'   Ignorado (com aviso) para SIM, SINAN e SINASC.
#' @param system Character. Código do sistema DATASUS. Sistemas disponíveis:
#'   **SIM:** `"SIM-DO"`, `"SIM-DOFET"`, `"SIM-DOEXT"`, `"SIM-DOINF"`,
#'   `"SIM-DOMAT"`. **SIH:** `"SIH-RD"`, `"SIH-RJ"`, `"SIH-SP"`, `"SIH-ER"`.
#'   **SINASC:** `"SINASC"`. **CNES:** `"CNES-LT"` … `"CNES-GM"` (13 tipos).
#'   **SIA:** `"SIA-AB"` … `"SIA-SAD"` (12 tipos). **SINAN:**
#'   `"SINAN-DENGUE"`, `"SINAN-CHIKUNGUNYA"`, `"SINAN-ZIKA"`,
#'   `"SINAN-MALARIA"`, `"SINAN-CHAGAS"`, `"SINAN-LEISHMANIOSE-VISCERAL"`,
#'   `"SINAN-LEISHMANIOSE-TEGUMENTAR"`, `"SINAN-LEPTOSPIROSE"`.
#' @param use_cache Logical. Usar cache em disco? Default `TRUE`. Cache
#'   expirado após 30 dias.
#' @param cache_dir Character. Diretório base do cache.
#'   Default `"~/.climasus4r_cache"`. Subdirs criados automaticamente:
#'   `raw/` (Parquets por chunk) e `dataset/` (reservado para uso futuro).
#' @param force_redownload Logical. Ignorar cache e re-baixar tudo? Default
#'   `FALSE`. Útil quando suspeitar de cache corrompido.
#' @param parallel Logical. Paralelizar downloads HTTP? Default `FALSE`.
#'   Use `TRUE` apenas com `total_tasks >= 6` — o overhead de
#'   `future::multisession` não compensa para poucas tarefas.
#' @param workers Integer. Workers para downloads paralelos. Default `4`.
#'   Recomendado: `min(total_tasks, 4)` — DATASUS tem throttling de conexões.
#' @param arrow_threads Integer. Threads internas do Arrow para I/O Parquet.
#'   Default `NULL` (auto: todos os cores físicos). Pool independente de
#'   `workers` — operam em fases distintas da pipeline.
#' @param lang Character. Idioma das mensagens CLI: `"pt"` (padrão), `"en"`,
#'   `"es"`.
#' @param verbose Logical. Exibir progresso detalhado? Default `TRUE`.
#'
#' @return Um `climasus_dataset` (subclasse de `arrow::Dataset`) com atributo
#'   `sus_meta`. O dataset é **lazy** — nenhum dado é carregado na RAM até
#'   que `dplyr::collect()` seja chamado.
#'
#'   Padrões de uso:
#'   ```r
#'   # Pipeline lazy (recomendado)
#'   ds |> dplyr::filter(...) |> dplyr::select(...) |> dplyr::collect()
#'
#'   # DuckDB — SQL de alta performance
#'   arrow::to_duckdb(ds) |> dplyr::filter(...) |> dplyr::collect()
#'
#'   # Materialização direta
#'   dplyr::collect(ds)
#'   ```
#'
#' @details
#' ## Por que Arrow Dataset e não tibble?
#' O Arrow Dataset é uma referência lazy a arquivos Parquet em disco.
#' Filtros, seleções e agregações são executados pelo motor C++ do Arrow
#' (ou DuckDB via `arrow::to_duckdb()`) sem carregar o dataset inteiro na RAM.
#' Somente `collect()` materializa os dados. Para datasets nacionais (SINAN),
#' o filtro de UF é aplicado como predicado lazy — sem `collect` antecipado.
#'
#' ## Cache
#' Cada combinação (sistema/UF/ano/mês) gera um arquivo `.parquet` com nome
#' baseado em MD5 dos parâmetros. Cache válido por 30 dias. Para sistemas
#' nacionais (SINAN), a chave de cache omite a UF (um arquivo por ano cobre
#' todos os estados).
#'
#' ## Paralelismo
#' O workload tem duas fases: download HTTP (IO de rede, paralelizável) e
#' escrita Parquet (IO de disco, sempre no processo principal). O Arrow já
#' usa multi-threading interno via `arrow_threads` para a fase de escrita —
#' não é necessário paralelizar com `future` nessa etapa.
#'
#' @seealso [arrow::open_dataset()], [arrow::to_duckdb()], [dplyr::collect()]
#'
#' @importFrom arrow write_parquet open_dataset set_cpu_count set_io_thread_count
#' @importFrom dplyr filter collect
#' @importFrom cli cli_h1 cli_h2 cli_alert_info cli_alert_success
#'   cli_alert_warning cli_alert_danger cli_abort
#' @importFrom fs dir_create dir_exists file_exists file_info
#' @importFrom digest digest
#' @importFrom stringi stri_enc_isutf8 stri_conv
#' @export
#'
#' @examples
#' \dontrun{
#' #  Exemplo 1: mortalidade lazy
#' ds <- sus_data_import(
#'   uf     = c("SP", "RJ"),
#'   year   = 2020:2023,
#'   system = "SIM-DO"
#' )
#' resultado <- ds |>
#'   dplyr::filter(SEXO == "2") |>
#'   dplyr::select(DTOBITO, CAUSABAS, MUNRES) |>
#'   dplyr::collect()
#'
#' # Exemplo 2: SINAN via DuckDB
#' ds <- sus_data_import(region = "nordeste", year = 2022, system = "SINAN-DENGUE")
#' arrow::to_duckdb(ds) |>
#'   dplyr::filter(CAUSABAS_O == "A90") |>
#'   dplyr::group_by(MUNRES) |>
#'   dplyr::summarise(casos = dplyr::n()) |>
#'   dplyr::collect()
#'
#' #  Exemplo 3: SIH mensal com paralelo 
#' ds <- sus_data_import(
#'   uf       = "SP",
#'   year     = 2023,
#'   month    = 1:6,
#'   system   = "SIH-RD",
#'   parallel = TRUE,
#'   workers  = 4L
#' )
#' }
sus_data_import_arrow <- function(
    uf                = NULL,
    region            = NULL,
    year,
    month             = NULL,
    system,
    use_cache         = TRUE,
    cache_dir         = "~/.climasus4r_cache/data",
    force_redownload  = FALSE,
    parallel          = FALSE,
    workers           = 4L,
    arrow_threads     = NULL,
    lang              = "pt",
    verbose           = TRUE
) {

  #  0. Validacoes basicas 

  if (!lang %in% c("en", "pt", "es"))
    cli::cli_abort("{.arg lang} deve ser um de: 'en', 'pt', 'es'.")
  
  #Check if arrow pak installed
  #check_arrow(lang=lang)

  # Thread pool do Arrow: independente do future, gerenciado por libarrow C++
  n_arrow_threads <- arrow_threads %||% parallel::detectCores(logical = FALSE)
  arrow::set_cpu_count(n_arrow_threads)
  arrow::set_io_thread_count(n_arrow_threads)

  # 1. Resolucao de regiao -> UF 

   if (!is.null(region)) {
    reg_clean <- tolower(region)
    target_key <- if (reg_clean %in% names(.region_aliases)) .region_aliases[[reg_clean]] else reg_clean
    if (target_key %in% names(.br_regions)) {
      if (!is.null(uf) && verbose) {
      cli::cli_alert_warning("Both {.arg uf} and {.arg region} provided. Using region mapping for: {.val {region}}.")
      }
     uf <- .br_regions[[target_key]]
      if (verbose) cli::cli_alert_info("Region {.val {region}} expanded to: {paste(uf, collapse = ', ')}")
    } else {
      cli::cli_abort("Region {.val {region}} not recognized. Check documentation for valid regions.")
    }
  }

  #  2. Validacao de argumentos
  # Input validation
  if (is.null(uf) || missing(year) || missing(system)) {
    cli::cli_alert_danger("Arguments {.arg uf} (or {.arg region}), {.arg year}, and {.arg system} are required.")
    stop("Missing required arguments.")
  }
  
  # Validate UF codes
  valid_ufs <- c("all", "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", 
                 "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", 
                 "RS", "RO", "RR", "SC", "SP", "SE", "TO")
  
  invalid_ufs <- setdiff(uf, valid_ufs)
  
  if (length(invalid_ufs) > 0) {
    cli::cli_alert_danger("Invalid UF codes: {paste(invalid_ufs, collapse = ', ')}")
    cli::cli_alert_info("Valid UF codes are: 'all', AC, AL, AP, AM, BA, CE, DF, ES, GO, MA, MT, MS, MG, PA, PB, PR, PE, PI, RJ, RN, RS, RO, RR, SC, SP, SE, TO")
    stop("Invalid UF codes provided.")
  }

  # Validate system codes
  valid_systems <- c(
    # SIH Systems
    "SIH-RD", "SIH-RJ", "SIH-SP", "SIH-ER",
    
    # SIM Systems
    "SIM-DO", "SIM-DOFET", "SIM-DOEXT", "SIM-DOINF", "SIM-DOMAT",
    
    # SINASC
    "SINASC",
    
    # CNES Systems
    "CNES-LT", "CNES-ST", "CNES-DC", "CNES-EQ", "CNES-SR", 
    "CNES-HB", "CNES-PF", "CNES-EP", "CNES-RC", "CNES-IN", 
    "CNES-EE", "CNES-EF", "CNES-GM",
    
    # SIA Systems
    "SIA-AB", "SIA-ABO", "SIA-ACF", "SIA-AD", "SIA-AN", 
    "SIA-AM", "SIA-AQ", "SIA-AR", "SIA-ATD", "SIA-PA", 
    "SIA-PS", "SIA-SAD",
    
    # SINAN Systems
    "SINAN-DENGUE", "SINAN-CHIKUNGUNYA", "SINAN-ZIKA", 
    "SINAN-MALARIA", "SINAN-CHAGAS", 
    "SINAN-LEISHMANIOSE-VISCERAL", 
    "SINAN-LEISHMANIOSE-TEGUMENTAR", "SINAN-LEPTOSPIROSE"
  )
  
  # Categorize systems for better error messages
  system_categories <- list(
    "SIH" = c("SIH-RD", "SIH-RJ", "SIH-SP", "SIH-ER"),
    "SIM" = c("SIM-DO", "SIM-DOFET", "SIM-DOEXT", "SIM-DOINF", "SIM-DOMAT"),
    "SINASC" = "SINASC",
    "CNES" = c("CNES-LT", "CNES-ST", "CNES-DC", "CNES-EQ", "CNES-SR", 
               "CNES-HB", "CNES-PF", "CNES-EP", "CNES-RC", "CNES-IN", 
               "CNES-EE", "CNES-EF", "CNES-GM"),
    "SIA" = c("SIA-AB", "SIA-ABO", "SIA-ACF", "SIA-AD", "SIA-AN", 
              "SIA-AM", "SIA-AQ", "SIA-AR", "SIA-ATD", "SIA-PA", 
              "SIA-PS", "SIA-SAD"),
    "SINAN" = c("SINAN-DENGUE", "SINAN-CHIKUNGUNYA", "SINAN-ZIKA", 
                "SINAN-MALARIA", "SINAN-CHAGAS", 
                "SINAN-LEISHMANIOSE-VISCERAL", 
                "SINAN-LEISHMANIOSE-TEGUMENTAR", "SINAN-LEPTOSPIROSE")
  )
  
  # Check if system is valid
  if (!system %in% valid_systems) {
    cli::cli_alert_danger("Invalid system: {system}")
    
    # Provide helpful suggestions
    cli::cli_alert_info("Valid systems are:")
    
    # Show by category
    for (category in names(system_categories)) {
      if (any(grepl(toupper(category), toupper(system)))) {
        cli::cli_alert_info("  {category} systems:")
        for (sys in system_categories[[category]]) {
          cli::cli_alert_info("    - {sys}")
        }
      }
    }
    
    # Also check for close matches
    possible_matches <- agrep(system, valid_systems, max.distance = 0.3, value = TRUE)
    if (length(possible_matches) > 0) {
      cli::cli_alert_info("Did you mean one of these?")
      for (match in possible_matches) {
        cli::cli_alert_info("  - {match}")
      }
    }
    
    stop("Invalid system provided.")
  }

  # Valid month
  system_prefix_month <- sub("-.*", "", system)
  systems_requiring_month <- c("SIH", "CNES", "SIA")
  
  if (system_prefix_month %in% systems_requiring_month && is.null(month)) {
    cli::cli_alert_danger("System {system} requires the 'month' argument.")
    cli::cli_alert_info("Please provide months as a vector (e.g., month = 1:12 or month = 1).")
    stop("Missing required argument: month. Please use month for 'SIH', 'CNES' and 'SIA' systems.")
  }
  
  if (!is.null(month)) {
    if (!is.numeric(month) || any(month < 1) || any(month > 12)) {
      cli::cli_alert_danger("Invalid month. Must be a numeric vector between 1 and 12.")
      stop("Invalid month provided.")
    }
    
    # 3. Aviso se o usuario fornecer mes para sistemas que geralmente são anuais (SIM, SINASC, SINAN)
    if (!system_prefix_month %in% systems_requiring_month) {
      cli::cli_alert_warning("Parameter 'month' is provided but typically not used for {system_prefix} systems.")
      cli::cli_alert_info("These systems usually aggregate data by year. The 'month' filter might be ignored by the server.")
    }
  }

  #Check month
  if (parallel && workers > parallel::detectCores()) {
    cli::cli_alert_warning(
      "{workers} workers requested but only {parallel::detectCores()} cores available"
    )
  }

  # 3. Diretorios de cache 

  cache_dir   <- path.expand(cache_dir)
  raw_dir     <- file.path(cache_dir, "raw")     # Parquets individuais
  dataset_dir <- file.path(cache_dir, "dataset") # Reservado para uso futuro

  for (d in c(raw_dir, dataset_dir))
    if (!fs::dir_exists(d)) fs::dir_create(d, recurse = TRUE)

  #4. Configuracao: sistemas nacionais vs. por UF
  #
  # Sistemas SINAN entregam dados nacionais num unico arquivo por ano.
  # Para eles: download_ufs = "BR" (uma tarefa/ano), e o filtro por UF
  # e aplicado LAZY sobre o Dataset resultante — sem collect antecipado.

  national_systems <- c(
    "SINAN-DENGUE","SINAN-CHIKUNGUNYA","SINAN-ZIKA","SINAN-MALARIA",
    "SINAN-CHAGAS","SINAN-LEISHMANIOSE-VISCERAL",
    "SINAN-LEISHMANIOSE-TEGUMENTAR","SINAN-LEPTOSPIROSE"
  )
  is_national  <- system %in% national_systems
  request_ufs  <- uf                              # UFs pedidas pelo usuario
  download_ufs <- if (is_national) "BR" else uf  # UFs efetivas de download

  # Mapeamento UF sigla - codigo IBGE (usado no filtro lazy de SINAN)
  uf_to_code <- c(
    AC=12L,AL=27L,AP=16L,AM=13L,BA=29L,CE=23L,DF=53L,ES=32L,
    GO=52L,MA=21L,MT=51L,MS=50L,MG=31L,PA=15L,PB=25L,PR=41L,
    PE=26L,PI=22L,RJ=33L,RN=24L,RS=43L,RO=11L,RR=14L,SC=42L,
    SP=35L,SE=28L,TO=17L
  )

  #  5. Grid de combinacoes 

  if (!is.null(month)) {
    params <- expand.grid(
      year  = year,
      uf    = download_ufs,
      month = month,
      stringsAsFactors = FALSE
    )
  } else {
    params <- expand.grid(
      year = year,
      uf   = download_ufs,
      stringsAsFactors = FALSE
    )
  }
  total_tasks <- nrow(params)

  #  6. Log de cabecalho 

  if (verbose) {
    cli::cli_h1("climasus4r - Datasus Import")
    cli::cli_alert_info("Sistema   : {system}")
    cli::cli_alert_info("Estados   : {paste(request_ufs, collapse = ', ')}")
    cli::cli_alert_info("Anos      : {paste(year, collapse = ', ')}")
    if (!is.null(month))
      cli::cli_alert_info("Meses     : {paste(month, collapse = ', ')}")
    cli::cli_alert_info("Tarefas   : {total_tasks}")
    cli::cli_alert_info(
      "Cache     : {ifelse(use_cache, raw_dir, 'DESABILITADO')}"
    )
    cli::cli_alert_info("Arrow I/O : {n_arrow_threads} thread(s)")
    if (is_national)
      cli::cli_alert_info(
        "Nacional  : download unico/ano, filtro lazy por UF apos abertura do dataset."
      )
    if (parallel && total_tasks > 1L)
      cli::cli_alert_info("Paralelo  : {min(workers, total_tasks)} worker(s) HTTP")
  }

  #7. Closures de cache (dependem de raw_dir) 

  .cache_path <- function(key) file.path(raw_dir, paste0(key, ".parquet"))

  .cache_valid <- function(path, max_days = 30L) {
    if (!fs::file_exists(path)) return(FALSE)
    age <- as.numeric(
      difftime(Sys.time(), fs::file_info(path)$modification_time, units = "days")
    )
    age <= max_days
  }

  # 8. download_one_raw 
  #
  # Executado dentro do worker (paralelo) ou inline (sequencial).
  # Retorna APENAS:
  #   list(type="path", value=path)          — cache hit (zero serializacao)
  #   list(type="df",   value=df, path=path) — download novo (df via socket)
  #   NULL                                   — falha irrecuperavel
  #
  # A escrita Parquet NUNCA ocorre aqui — ocorre no processo principal (§9),
  # evitando: (a) race conditions de I/O em disco, (b) necessidade de
  # serializar df+path via socket future, (c) dependencia de permissao de
  # escrita nos workers (falha silenciosa em NFS/Windows).

  download_one_raw <- function(year_i, uf_i, system_i, month_i = NULL) {
    label <- paste(
      Filter(Negate(is.null), list(system_i, uf_i, year_i, month_i)),
      collapse = " · "
    )

    fetch_uf   <- if (uf_i == "BR") request_ufs[1L] else uf_i
    cache_key  <- .make_cache_key(system_i, uf_i, year_i, month_i, is_national)
    cache_path <- .cache_path(cache_key)

    # Cache hit: retorna apenas o path — nada e serializado via socket
    if (use_cache && !force_redownload && .cache_valid(cache_path)) {
      if (verbose) cli::cli_alert_success("Cache: {label}")
      return(list(type = "path", value = cache_path))
    }

    if (verbose) cli::cli_alert_info("Baixando: {label}")

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

    tryCatch({
      df <- do.call(microdatasus::fetch_datasus, params_fetch)

      if (is.null(df) || nrow(df) == 0L) {
        cli::cli_alert_warning("Sem dados: {label}")
        return(NULL)
      }

      list(type = "df", value = df, path = cache_path, label = label)

    }, error = function(e) {
      cli::cli_alert_danger("Falha: {label} — {conditionMessage(e)}")
      if (grepl("404|Not Found",  conditionMessage(e), ignore.case = TRUE))
        cli::cli_alert_info("  -> Combinacao pode nao existir no DATASUS.")
      if (grepl("timeout",        conditionMessage(e), ignore.case = TRUE))
        cli::cli_alert_info("  -> Verifique a conexao ou tente novamente.")
      NULL
    })
  }

  # 9. Execucao dos downloads (paralelo HTTP ou sequencial)
  #
  # Paralelo: future_map retorna list(type, value/path) via socket -
  #   transferência maxima = tamanho do df bruto (inevitavel para downloads novos).
  #   Para cache hits a transferencia es aprox. 50 bytes (apenas o path).
  # Sequencial: purrr::map, sem overhead de serializacao.

  run_task <- function(i) {
    download_one_raw(
      year_i   = params$year[i],
      uf_i     = params$uf[i],
      system_i = system,
      month_i  = if ("month" %in% names(params)) params$month[i] else NULL
    )
  }

  if (parallel && total_tasks > 1L) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = min(workers, total_tasks))

    raw_results <- furrr::future_map(
      seq_len(total_tasks),
      run_task,
      .options = furrr::furrr_options(
        seed     = TRUE,
        packages = c("microdatasus", "arrow", "stringi", "cli", "fs", "digest"),
        globals  = c(
          "download_one_raw", ".make_cache_key", ".cache_path", ".cache_valid",
          "system", "is_national", "request_ufs",
          "use_cache", "force_redownload", "raw_dir", "verbose",
          "params"
        )
      )
    )
  } else {
    raw_results <- purrr::map(seq_len(total_tasks), run_task)
  }

  #  10. Escrita Parquet no processo principal 
  #
  # Itera sobre raw_results: cache hits → coleta o path diretamente;
  # downloads novos -> escreve com .write_chunk_parquet() e depois libera df.
  # rm() + gc() imediatamente apos a escrita para nao acumular na heap.

  parquet_paths <- character(0L)

  for (res in raw_results) {
    if (is.null(res)) next

    if (res$type == "path") {
      parquet_paths <- c(parquet_paths, res$value)

    } else if (res$type == "df") {
      written <- .write_chunk_parquet(res$value, res$path)
      rm(res); gc(verbose = FALSE)
      if (!is.null(written)) {
        if (verbose)
          cli::cli_alert_success("Salvo: {basename(written)}")
        parquet_paths <- c(parquet_paths, written)
      }
    }
  }
  rm(raw_results); gc(verbose = FALSE)

  # 11. Validacao dos paths

  file_sizes <- fs::file_info(
    parquet_paths[fs::file_exists(parquet_paths)]
  )$size

  valid_paths <- parquet_paths[
    !is.na(parquet_paths) &
    nchar(parquet_paths) > 0L &
    fs::file_exists(parquet_paths) &
    fs::file_info(parquet_paths)$size > 0L
  ]
  names(valid_paths) <- NULL

  n_ok   <- length(valid_paths)
  n_fail <- total_tasks - n_ok

  if (n_ok == 0L) {
    cli::cli_alert_danger(
      "Nenhum dado disponivel para os parametros fornecidos."
    )
    return(invisible(NULL))
  }

  if (verbose) {
    cli::cli_alert_success("{n_ok}/{total_tasks} arquivo(s) Parquet valido(s).")
    if (n_fail > 0L)
      cli::cli_alert_warning(
        "{n_fail} download(s) falharam — verifique os logs acima."
      )
  }

  # 12. Arrow Dataset lazy (sem rbindlist, sem RAM)
  #
  # open_dataset le apenas os footers Parquet (metadados/schema) — sem
  # desserializar dados. unify_schemas = TRUE garante que colunas ausentes
  # em alguns arquivos aparecam como NULL (equivalente ao fill=TRUE do
  # rbindlist), essencial quando diferentes anos tem schemas distintos.

  if (verbose)
    cli::cli_alert_info("Abrindo Arrow Dataset ({n_ok} arquivo(s))...")

  dataset <- tryCatch({
    arrow::open_dataset(
      sources       = valid_paths,
      format        = "parquet",
      unify_schemas = TRUE
    )
  }, error = function(e) {
    cli::cli_alert_danger("Falha ao abrir dataset: {conditionMessage(e)}")
    cli::cli_alert_info("Verificando arquivos individualmente...")

    # Diagnostico: isola arquivos corrompidos
    readable <- Filter(function(p) {
      tryCatch({
        arrow::open_dataset(p, format = "parquet")
        TRUE
      }, error = function(e2) {
        cli::cli_alert_warning(
          "Corrompido: {basename(p)} — {conditionMessage(e2)}"
        )
        FALSE
      })
    }, valid_paths)

    if (length(readable) == 0L)
      cli::cli_abort("Nenhum arquivo Parquet legivel. Verifique os logs.")

    cli::cli_alert_info(
      "Reabrindo com {length(readable)}/{n_ok} arquivo(s) legiveis..."
    )
    arrow::open_dataset(
      sources       = readable,
      format        = "parquet",
      unify_schemas = TRUE
    )
  })

  # 13. Filtro lazy de UF para sistemas nacionais (SINAN) 
  #
  # Predicado Arrow: executado em C++ durante o scan, sem collect().
  # Coluna SG_UF_NOT usa codigo numerico IBGE — mapeamento feito aqui uma vez
  # e embutido como literal no predicado.

  if (is_national && length(request_ufs) > 0L) {
    codes_wanted <- unname(uf_to_code[request_ufs])
    codes_wanted <- codes_wanted[!is.na(codes_wanted)]

    if (length(codes_wanted) > 0L) {
      dataset <- dataset |>
        dplyr::filter(as.integer(SG_UF_NOT) %in% codes_wanted)

      if (verbose)
        cli::cli_alert_info(
          "Filtro lazy de UF: {length(codes_wanted)} codigo(s) IBGE aplicado(s)."
        )
    }
  }

  #  14. Metadados climasus_dataset
  #
  # arrow::Dataset nao suporta atributos R arbitrarios de forma persistente.
  # sus_meta es armazenado como atributo R no wrapper S3 climasus_dataset —
  # acessevel via attr(ds, "sus_meta") e preservado por collect().

  meta <- list(
    system    = NULL,
    stage     = "import",
    type      = "raw",
    spatial   = FALSE,
    temporal  = NULL,
    backend   = "parquet",
    created   = Sys.time(),
    modified  = Sys.time(),
    history   = sprintf(
      "[%s] Imported via Arrow out-of-core (climasus4r)",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ),
    n_files   = n_ok,
    n_failed  = n_fail,
    cache_dir = raw_dir,
    user      = list()
  )

  dataset <- structure(
    dataset,
    sus_meta      = meta,
    parquet_paths = valid_paths,
    class         = c("climasus_dataset", class(dataset))
  )

  # 15. Resumo final 

  if (verbose) {
    # num_rows em FileSystemDataset sem filtros usa metadados Parquet (rapido).
    # Em datasets filtrados (SINAN lazy), evita scan completo.
    approx_rows <- tryCatch({
      if (inherits(dataset, "FileSystemDataset")) {
        format(dataset$num_rows, big.mark = ".", decimal.mark = ",")
      } else {
        "N/A (dataset filtrado — use nrow(dplyr::collect(...)) para contar)"
      }
    }, error = function(e) "N/A")

    cli::cli_alert_success("Dataset lazy pronto: ~{approx_rows} linha(s).")
    # cli::cli_alert_info(
    #   "Pipeline lazy : {.code ds |> dplyr::filter(...) |> dplyr::collect()}"
    # )
    # cli::cli_alert_info(
    #   "DuckDB        : {.code arrow::to_duckdb(ds) |> dplyr::filter(...) |> dplyr::collect()}"
    # )
  }

  return(dataset)
}


# ==============================================================================
# S3: print / collect para climasus_dataset
# ==============================================================================

#' @export
print.climasus_dataset <- function(x, ...) {
  meta <- attr(x, "sus_meta")
  cli::cli_h2("climasus_dataset")
  cli::cli_alert_info("Sistema   : {meta$system}")
  cli::cli_alert_info("Stage     : {meta$stage}")
  cli::cli_alert_info("Arquivos  : {meta$n_files} Parquet(s) ({meta$n_failed} falha(s))")
  cli::cli_alert_info("Schema    : {ncol(x)} coluna(s)")
  cli::cli_alert_info("Cache dir : {meta$cache_dir}")
  cli::cli_alert_info("Criado    : {format(meta$created, '%Y-%m-%d %H:%M:%S')}")
  cli::cli_alert_info(
    "Histórico : {paste(meta$history, collapse = ' | ')}"
  )
  NextMethod()
  invisible(x)
}


#' Materializa um climasus_dataset como tibble preservando os metadados
#'
#' Equivalente a `dplyr::collect()` com preservação do atributo `sus_meta`
#' e promocao da classe para `climasus_df`.
#'
#' @param x   Um `climasus_dataset`.
#' @param ... Argumentos adicionais passados para `dplyr::collect()`.
#' @return Um `climasus_df` (tibble com atributo `sus_meta`).
#' @export
collect.climasus_dataset <- function(x, ...) {
  meta <- attr(x, "sus_meta")
  df   <- NextMethod()

  # Promote to climasus_df when the package is loaded (devtools::load_all or
  # installed); fall back to attaching sus_meta as a plain attribute when
  # the roadmap script is sourced standalone (package not loaded).
  if (exists("new_climasus_df", mode = "function")) {
    df <- new_climasus_df(df, meta)
    if (exists("sus_meta", mode = "function"))
      df <- sus_meta(df, stage = "import", backend = "tibble",
                     add_history = "Collected Arrow Dataset to in-memory tibble")
  } else {
    attr(df, "sus_meta") <- meta
  }

  df
}



# ==============================================================================
# DADOS ESTATICOS
# ==============================================================================

#' RegiOes geograficas do Brasil para o argumento `region`
#' @noRd
.br_regions <- list(
  # IBGE Macro-regiões
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
  matopiba            = c("MA","TO","PI","BA"),
  arco_desmatamento   = c("RO","AC","AM","PA","MT","MA"),
  dengue_hyperendemic = c("GO","MS","MT","PR","RJ","SP"),
  sudene              = c("AL","BA","CE","MA","PB","PE","PI","RN","SE","MG","ES"),
  fronteira_brasil    = c("AC","AM","AP","MT","MS","PA","PR","RO","RR","RS","SC")
)

#' Aliases multilíngues para o argumento `region` (EN/ES → chave PT interna)
#' @noRd
.region_aliases <- list(
  # English
  north           = "norte",
  northeast       = "nordeste",
  central_west    = "centro_oeste",
  southeast       = "sudeste",
  south           = "sul",
  amazon          = "amazonia_legal",
  legal_amazon    = "amazonia_legal",
  atlantic_forest = "mata_atlantica",
  semi_arid       = "semi_arido",
  border          = "fronteira_brasil",
  # Spanish
  noreste         = "nordeste",
  sur             = "sul",
  amazonia        = "amazonia_legal",
  bosque_atlantico = "mata_atlantica",
  semiarido       = "semi_arido",
  frontera        = "fronteira_brasil"
)