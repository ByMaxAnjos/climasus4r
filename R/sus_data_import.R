#' Import and preprocess data from DATASUS with intelligent caching
#'
#' This function acts as a wrapper for `microdatasus::fetch_datasus`,
#' simplifying the download and reading of data from Brazilian public health
#' information systems (SIM, SINAN, SIH, SIA, CNES, SINASC).
#' It includes parallel processing, caching, and user-friendly CLI feedback.
#'
#' @param uf Character scalar or vector of two-letter state codes (upper case),
#'   e.g. `"AM"` or `c("SP", "RJ")`; `"all"` is also accepted by the
#'   validation (all states; no UF filter is applied to the result). Valid
#'   codes: AC, AL, AP, AM, BA, CE, DF, ES, GO, MA, MT, MS, MG, PA, PB, PR,
#'   PE, PI, RJ, RN, RS, RO, RR, SC, SP, SE, TO. **Ignored (with a warning)
#'   when `region` is supplied.** Invalid codes abort with an error. Either
#'   `uf` or `region` is required (default `NULL`). For national SINAN
#'   systems a single national file is downloaded per year and then filtered
#'   to the requested UF(s).
#' @param region Character scalar (a single name, not a vector) selecting a
#'   predefined group of states, e.g. `"norte"`, `"amazonia_legal"`,
#'   `"matopiba"`. Case-insensitive; Portuguese keys and the English/Spanish
#'   aliases (e.g. `"north"`, `"legal_amazon"`, `"sur"`, `"semiarido"`) are
#'   accepted. Takes precedence over `uf`. Default `NULL`. An unrecognised name
#'   aborts with an error. Available regions:
#'   
#'   **IBGE Macro-regions:**
#'   * `"norte"`: c("AC", "AP", "AM", "PA", "RO", "RR", "TO")
#'   * `"nordeste"`: c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE")
#'   * `"centro_oeste"`: c("DF", "GO", "MT", "MS")
#'   * `"sudeste"`: c("ES", "MG", "RJ", "SP")
#'   * `"sul"`: c("PR", "RS", "SC")
#'   
#'   **Biomes (Ecological Borders):**
#'   * `"amazonia_legal"`: c("AC", "AP", "AM", "PA", "RO", "RR", "MT", "MA", "TO")
#'   * `"mata_atlantica"`: c("AL", "BA", "CE", "ES", "GO", "MA", "MG", "MS", "PB", "PE", "PI", "PR", "RJ", "RN", "RS", "SC", "SE", "SP")
#'   * `"caatinga"`: c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "MG")
#'   * `"cerrado"`: c("BA", "DF", "GO", "MA", "MG", "MS", "MT", "PA", "PI", "PR", "RO", "SP", "TO")
#'   * `"pantanal"`: c("MT", "MS")
#'   * `"pampa"`: c("RS")
#' 
#'   **Hydrography & Climate:**
#'   * `"bacia_amazonia"`: c("AC", "AM", "AP", "MT", "PA", "RO", "RR")
#'   * `"bacia_sao_francisco"`: c("AL", "BA", "DF", "GO", "MG", "PE", "SE")
#'   * `"bacia_parana"`: c("GO", "MG", "MS", "PR", "SP")
#'   * `"bacia_tocantins"`: c("GO", "MA", "PA", "TO")
#'   * `"semi_arido"`: c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "MG")
#'   
#'   **Health, Agriculture & Geopolitics:**
#'   * `"matopiba"`: c("MA", "TO", "PI", "BA")
#'   * `"arco_desmatamento"`: c("RO", "AC", "AM", "PA", "MT", "MA")
#'   * `"dengue_hyperendemic"`: c("GO", "MS", "MT", "PR", "RJ", "SP")
#'   * `"sudene"`: c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "MG", "ES")
#'   * `"fronteira_brasil"`: c("AC", "AM", "AP", "MT", "MS", "PA", "PR", "RO", "RR", "RS", "SC")
#' 
#' @param year Required. Numeric (4-digit) year(s): a scalar (`2023`), a range
#'   (`2020:2025`), or an explicit vector (`c(2020, 2022, 2024)`). Every
#'   element is downloaded as a separate file, so `c(2020, 2024)` means
#'   exactly those two years, **not** the range 2020-2024; use `2020:2024`
#'   for a range. Character years that coerce to numbers (`"2023"`) are
#'   passed through `as.numeric()`. Available years depend on the DATASUS
#'   system and are not validated here; a combination with no published file
#'   is reported as "no data"/failed and skipped. The total number of downloads
#'   is `length(uf) * length(year) * length(month)` (months only for monthly
#'   systems).
#' @param month Integer scalar or vector with months `1`-`12`, e.g. `1`,
#'   `1:6`, `c(1, 7)` (exactly January and July). **Required** (non-`NULL`) for
#'   the monthly systems SIH, CNES and SIA; the function aborts otherwise.
#'   Values outside 1-12 abort. For annual systems (SIM, SINAN, SINASC) a
#'   non-`NULL` value triggers a warning and has no effect on the download.
#'   The same months are applied to every year in `year`. Default `NULL`.
#' @param system Required. Character scalar with exactly one system code from
#'   the list below (case-sensitive; vectors are not supported). An invalid
#'   value aborts and suggests close matches. Available systems:
#'   
#'   **Mortality Systems (SIM - Mortality Information System):**
#'   * `"SIM-DO"`: Death certificates (Declaracoes de Obito) - Complete dataset
#'   * `"SIM-DOFET"`: Fetal deaths (Obitos Fetais)
#'   * `"SIM-DOEXT"`: External causes deaths (Obitos por Causas Externas)
#'   * `"SIM-DOINF"`: Infant deaths (Obitos Infantis)
#'   * `"SIM-DOMAT"`: Maternal deaths (Obitos Maternos)
#'   
#'   **Hospitalization Systems (SIH - Hospital Information System):**
#'   * `"SIH-RD"`: Hospital Admission Authorizations (AIH - Autorizacoes de Internacao Hospitalar)
#'   * `"SIH-RJ"`: Hospital Admission Authorizations - Rio de Janeiro specific
#'   * `"SIH-SP"`: Hospital Admission Authorizations - Sao Paulo specific
#'   * `"SIH-ER"`: Emergency Room Records (Prontuarios de Emergencia)
#'   
#'   **Notifiable Diseases (SINAN - Notifiable Diseases Information System):**
#'   * `"SINAN-DENGUE"`: Dengue fever cases
#'   * `"SINAN-CHIKUNGUNYA"`: Chikungunya cases
#'   * `"SINAN-ZIKA"`: Zika virus cases
#'   * `"SINAN-MALARIA"`: Malaria cases
#'   * `"SINAN-CHAGAS"`: Chagas disease cases
#'   * `"SINAN-LEISHMANIOSE-VISCERAL"`: Visceral leishmaniasis cases
#'   * `"SINAN-LEISHMANIOSE-TEGUMENTAR"`: Cutaneous leishmaniasis cases
#'   * `"SINAN-LEPTOSPIROSE"`: Leptospirosis cases
#'   
#'   **Outpatient Systems (SIA - Outpatient Information System):**
#'   * `"SIA-AB"`: Primary Care (Atencao Basica)
#'   * `"SIA-ABO"`: Dental Procedures (Procedimentos Odontologicos)
#'   * `"SIA-ACF"`: Pharmaceutical Assistance (Assistencia Farmaceutica)
#'   * `"SIA-AD"`: High Complexity (Alta Complexidade/Diferenciada)
#'   * `"SIA-AN"`: Home Care (Atencao Domiciliar)
#'   * `"SIA-AM"`: Medical Specialties (Ambulatorio de Especialidades)
#'   * `"SIA-AQ"`: Strategic Actions (Acoes Estrategicas)
#'   * `"SIA-AR"`: Regulation (Regulacao)
#'   * `"SIA-ATD"`: Urgency/Emergency (Urgencia/Emergencia)
#'   * `"SIA-PA"`: Hospital Outpatient (Procedimentos Ambulatoriais em Hospital)
#'   * `"SIA-PS"`: Psychosocial Care (Atencao Psicossocial)
#'   * `"SIA-SAD"`: Specialized Care (Atencao Especializada)
#'   
#'   **Health Establishments (CNES - National Health Establishment Registry):**
#'   * `"CNES-LT"`: Beds (Leitos)
#'   * `"CNES-ST"`: Health Professionals (Profissionais de Saude)
#'   * `"CNES-DC"`: Equipment (Equipamentos) - Detailed
#'   * `"CNES-EQ"`: Equipment (Equipamentos) - Summary
#'   * `"CNES-SR"`: Specialized Services (Servicos Especializados)
#'   * `"CNES-HB"`: Hospital Beds (Leitos Hospitalares)
#'   * `"CNES-PF"`: Health Professionals Detailed (Pessoal Fisico)
#'   * `"CNES-EP"`: Teaching Participants (Participantes do Ensino)
#'   * `"CNES-RC"`: Hospital Class (Classificacao Hospitalar)
#'   * `"CNES-IN"`: Hospital Indicators (Indicadores Hospitalares)
#'   * `"CNES-EE"`: Educational Entities (Entidades de Ensino)
#'   * `"CNES-EF"`: Teaching Facilities (Instalacoes de Ensino)
#'   * `"CNES-GM"`: Management/Support (Gestao e Apoio)
#'   
#'   **Live Births (SINASC - Live Birth Information System):**
#'   * `"SINASC"`: Live Birth Declarations (Declaracoes de Nascidos Vivos)
#' @param backend Character scalar, `"arrow"` (default) or `"tibble"`.
#'
#'   - `"arrow"`: each UF/year/month file is cached as Parquet and the result
#'     is a lazy Arrow dataset (class `climasus_dataset`) that is not loaded
#'     into memory; use [dplyr::collect()] to materialise it. If the Arrow
#'     path fails, a warning is issued and the function falls back to
#'     `"tibble"`.
#'
#'   - `"tibble"`: data is fully loaded into memory as a `climasus_df`
#'     (cached as RDS). Simpler, but may be slow or fail for large datasets.
#'
#'   Any value other than `"arrow"` uses the tibble path.
#' @param city Character vector of municipality names (e.g., `"Porto Velho"`,
#'   `c("Manaus", "Belem")`) or 6/7-digit code strings. Case-insensitive;
#'   accents are normalised; the search is restricted to the requested `uf`
#'   when possible. Unknown names are skipped with a warning and fuzzy
#'   suggestions. Combined (union) with `municipality_code`. Applied after the
#'   download to the combined data. Default `NULL` (no municipality filter).
#' @param municipality_code Character or numeric vector of 6- or 7-digit IBGE
#'   municipality codes, e.g. `c(110020, 1302603)`. Combined (union) with
#'   `city`; both the 6-digit (DATASUS) and 7-digit (IBGE) forms of each code
#'   are matched. Default `NULL`.
#' @param use_cache Logical scalar, default `TRUE`. Reuse files already
#'   downloaded to `cache_dir`. The cache key is system + UF + year + month
#'   (national SINAN files are keyed without UF). Files older than 30 days are
#'   considered stale and re-downloaded.
#' @param cache_dir Character scalar, directory for cached files (`~` is
#'   expanded; created if missing). Default `"~/.climasus4r_cache/data"`.
#'   The arrow backend stores Parquet files under `cache_dir/raw`.
#' @param force_redownload Logical scalar, default `FALSE`. If `TRUE`, ignores
#'   the cache and downloads everything again (useful for corrupted or
#'   outdated files).
#' @param filter_residence_uf Logical. For state-level systems (SIM, SIH, SIA,
#'   SINASC), keep only records whose municipality code belongs to the
#'   requested `uf` (default `TRUE`). Set to `FALSE` to keep every record in
#'   the state file, e.g. SIH admissions of patients living in another state.
#'   Needed to build residence-based national series, where an admission in
#'   one state must be assigned to the patient's state of residence. Has no
#'   effect on national systems (SINAN).
#' @param parallel Logical scalar, default `FALSE`. If `TRUE`, download
#'   multiple UF/year/month combinations in parallel (via `future`). A warning
#'   is issued if `workers` exceeds the number of detected cores.
#' @param workers Integer, default `4`. Number of parallel workers; only used
#'   when `parallel = TRUE`.
#' @param lang Character scalar: `"pt"` (default), `"en"` or `"es"`. Language
#'   of console messages; any other value aborts.
#' @param verbose Logical scalar, default `TRUE`. Print progress (cache hits,
#'   downloads, filters).
#'
#' @return With `backend = "arrow"` (default): a lazy Arrow dataset of class
#'   `climasus_dataset` (not in memory; `dplyr::collect()` returns a
#'   `climasus_df`). With `backend = "tibble"` (or after fallback): a
#'   `climasus_df` tibble combining all UFs/years/months with the original
#'   DATASUS columns (as returned by `microdatasus`). `sus_meta` is set to
#'   `stage = "import"`, `type = "raw"`.
#'
#'   **Note**: large datasets (especially SIA and SIH) may require a lot of
#'   memory (1 GB+ for national annual data); prefer the arrow backend.
#'
#' @details
#' ## Data Sources
#' All data is sourced from the Brazilian Ministry of Health's DATASUS portal
#' (http://datasus.saude.gov.br).
#'
#' ## Caching System
#' One file is cached per system/UF/year(/month) combination (Parquet for
#' `backend = "arrow"`, RDS for `"tibble"`). Cached files older than 30 days are
#' re-downloaded.
#'
#' ## Pipeline position
#' First step of the pipeline (stage `"import"`). Next: [sus_data_clean_encoding()]
#' and [sus_data_standardize()].
#'
#' ## Year and month semantics
#' `year` and `month` are expanded as a full grid with `uf`:
#' `year = 2020:2022, month = 1:3` downloads 9 files per UF. Vectors are taken
#' literally (`c(2020, 2024)` = two years).
#'
#' ## Residence filter
#' For state-level systems, rows whose municipality code does not belong to
#' the requested UF are dropped unless `filter_residence_uf = FALSE`.
#'
#' ## Parallel Processing
#' With `parallel = TRUE`, combinations are downloaded concurrently. For large
#' downloads consider more `workers` if cores and memory allow.
#'
#' @seealso
#' * [sus_data_clean_encoding()], [sus_data_standardize()], [sus_data_read()]
#' * Official DATASUS documentation: \url{http://datasus.saude.gov.br}
#' * Microdatasus package: \url{https://github.com/rfsaldanha/microdatasus}
#'
#' @importFrom glue glue
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic example: Mortality data for Rio de Janeiro in 2022
#' df_sim <- sus_data_import(
#'   uf = "RJ", 
#'   year = 2022, 
#'   system = "SIM-DO",
#'   use_cache = TRUE
#' )
#' 
#' # Dengue cases for two states with parallel processing
#' df_dengue <- sus_data_import(
#'   uf = c("SP", "MG"), 
#'   year = 2023, 
#'   system = "SINAN-DENGUE",
#'   parallel = TRUE,
#'   workers = 3
#' )
#' 
#' # Hospitalizations with monthly specification
#' df_hospital <- sus_data_import(
#'   uf = "SP",
#'   year = 2024,
#'   month = 1:6,  # January to June
#'   system = "SIH-RD",
#'   verbose = TRUE
#' )
#' 
#' # Non-contiguous years: exactly 2020 and 2024 (NOT 2020-2024)
#' df_two <- sus_data_import(uf = "AM", year = c(2020, 2024), system = "SIM-DO")
#'
#' # A whole region instead of uf
#' df_n <- sus_data_import(region = "norte", year = 2022, system = "SIM-DO")
#'
#' # Force re-download ignoring cache
#' df_births <- sus_data_import(
#'   uf = "BA",
#'   year = 2020:2022,
#'   system = "SINASC",
#'   use_cache = TRUE,
#'   force_redownload = TRUE  # Refresh cached data
#' )
#' }
#' 
#' @references
#' Brazilian Ministry of Health. DATASUS. \url{http://datasus.saude.gov.br}
#' 
#' SALDANHA, Raphael de Freitas; BASTOS, Ronaldo Rocha; BARCELLOS, Christovam. Microdatasus: pacote para download e pre-processamento de microdados do Departamento de Informatica do SUS (DATASUS). Cad. Saude Publica, Rio de Janeiro , v. 35, n. 9, e00032419, 2019. Available from https://doi.org/10.1590/0102-311x00032419.

sus_data_import <- function(uf = NULL,
                            region = NULL,
                            year,
                            month = NULL,
                            system,
                            backend = "arrow",
                            city = NULL,
                            municipality_code = NULL,
                            use_cache = TRUE,
                            cache_dir = "~/.climasus4r_cache/data",
                            force_redownload = FALSE,
                            filter_residence_uf = TRUE,
                            parallel = FALSE,
                            workers = 4,
                            lang = "pt",
                            verbose = TRUE) {
  
  # --- Backend Selection ---
  if (backend == "arrow") {
    result <- tryCatch({
      .import_arrow_internal(
        uf = uf,
        region = region,
        year = year,
        system = system,
        month = month,
        city = city,
        municipality_code = municipality_code,
        use_cache = use_cache, 
        cache_dir = cache_dir, 
        force_redownload = force_redownload, 
        filter_residence_uf = filter_residence_uf,
        parallel = parallel,
        workers = workers, 
        lang = lang, 
        verbose = verbose  # Passar explicitamente
      )
    }, error = function(e) {
      cli::cli_alert_warning(
        "Lazy (Arrow) falhou: {conditionMessage(e)}. ",
        "Retornando em modo tibble."
      )
      NULL
    })
    
    if (!is.null(result)) {
      return(result)
    }
  }
  # Fallback: tibble
  .import_tibble_internal(
        uf = uf, 
        region = region,
        year = year, 
        system = system, 
        month = month, 
        city = city, 
        municipality_code = municipality_code,
        use_cache = use_cache, 
        cache_dir = cache_dir, 
        force_redownload = force_redownload, 
        filter_residence_uf = filter_residence_uf,
        parallel = parallel,
        workers = workers, 
        lang = lang, 
        verbose = verbose  # Passar explicitamente
  )
}  
 # ===========================================================================
 # INTERNAL FUNCTIONS
 # ===========================================================================
  
.import_tibble_internal <- function(
     uf, region, year, system, month, city, municipality_code,
     use_cache, cache_dir, force_redownload, filter_residence_uf, parallel,
     workers, lang, verbose
    ) {
  # Validate language
  if (!lang %in% c("en", "pt", "es")) {
    cli::cli_abort("lang must be one of: 'en', 'pt', 'es'")
  }
  #Check if arrow pak installed
  check_arrow(lang=lang)

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
    
    # 3. Aviso se o usuario fornecer mes para sistemas que geralmente sao anuais (SIM, SINASC, SINAN)
    if (!system_prefix_month %in% systems_requiring_month) {
      cli::cli_alert_warning("Parameter 'month' is provided but typically not used for {system_prefix_month} systems.")
      cli::cli_alert_info("These systems usually aggregate data by year. The 'month' filter might be ignored by the server.")
    }
  }
  #Check month
  if (parallel && workers > parallel::detectCores()) {
    cli::cli_alert_warning(
      "{workers} workers requested but only {parallel::detectCores()} cores available"
    )
  }

  #National system for efficient download
  national_systems <- c(
    "SINAN-DENGUE", "SINAN-CHIKUNGUNYA", "SINAN-ZIKA",
    "SINAN-MALARIA", "SINAN-CHAGAS", "SINAN-LEISHMANIOSE-VISCERAL",
    "SINAN-LEISHMANIOSE-TEGUMENTAR", "SINAN-LEPTOSPIROSE"
  )

  # Setup cache directory
  cache_dir <- path.expand(cache_dir)
  if (use_cache) {
    if (!fs::dir_exists(cache_dir)) {
      if (verbose) cli::cli_alert_info("Creating cache directory: {cache_dir}")
      fs::dir_create(cache_dir, recursive = TRUE)
    }
  }

  # City / municipality_code resolve to IBGE codes BEFORE downloads (fail fast).
  # muni_meta (~5 570 rows) is always in RAM; spatial cache lives under cache_dir.
  all_muni_codes_import <- NULL

  if (!is.null(city) || !is.null(municipality_code)) {
    spatial_cache_dir <- file.path(dirname(cache_dir)) #Tirei o (, "spatial")
    if (!fs::dir_exists(spatial_cache_dir))
      fs::dir_create(spatial_cache_dir, recurse = TRUE)

    muni_meta_import <- tryCatch(
      get_spatial_municipio_cache(
        cache_dir = spatial_cache_dir,
        use_cache = use_cache,
        lang      = lang,
        verbose   = verbose
      ),
      error = function(e) {
        cli::cli_alert_warning(
          "Nao foi possivel carregar metadados municipais: {conditionMessage(e)}"
        )
        NULL
      }
    )

    # Restrict city search to requested UF(s) — prevents cross-state name matches.
    # Falls back to searching all municipalities when uf_code column is absent
    # or when the UF filter would return 0 rows (e.g. stale cache with old schema).
    if (!is.null(uf) && !is.null(muni_meta_import) &&
        !identical(tolower(uf), "all")) {
      if ("uf_code" %in% names(muni_meta_import)) {
        uf_filtered <- muni_meta_import[
          toupper(muni_meta_import$uf_code) %in% toupper(uf), , drop = FALSE
        ]
        if (nrow(uf_filtered) > 0L) {
          muni_meta_import <- uf_filtered
        }
      }
    }

    collected_codes <- character(0L)
    n_city_not_found <- 0L

    if (!is.null(city) && !is.null(muni_meta_import)) {
      meta_norm <- stringi::stri_trans_general(
        tolower(muni_meta_import$no_accents), "Latin-ASCII"
      )
      for (city_i in city) {
        input <- trimws(as.character(city_i))

        if (grepl("^\\d{6,7}$", input)) {
          idx <- if (nchar(input) == 7L)
            which(muni_meta_import$municipio == input)
          else
            which(substr(muni_meta_import$municipio, 1L, 6L) == input)
          if (length(idx) > 0L) {
            collected_codes <- c(collected_codes, muni_meta_import$municipio[idx])
          } else {
            cli::cli_alert_warning("Codigo de municipio {.val {input}} nao encontrado.")
            n_city_not_found <- n_city_not_found + 1L
          }
          next
        }

        input_norm <- stringi::stri_trans_general(tolower(input), "Latin-ASCII")
        idx <- which(meta_norm == input_norm)
        if (length(idx) == 0L) {
          orig_norm <- stringi::stri_trans_general(
            tolower(muni_meta_import$name), "Latin-ASCII"
          )
          idx <- which(orig_norm == input_norm)
        }

        if (length(idx) > 0L) {
          collected_codes <- c(collected_codes, muni_meta_import$municipio[idx])
        } else {
          msg_nf <- switch(lang,
            en = paste0("City '", city_i, "' not found."),
            es = paste0("Ciudad '", city_i, "' no encontrada."),
            paste0("Municipio '", city_i, "' nao encontrado.")
          )
          cli::cli_alert_warning(msg_nf)
          n_city_not_found <- n_city_not_found + 1L

          prefix_idx <- which(startsWith(meta_norm, input_norm))
          max_dist   <- max(1L, min(3L, floor(nchar(input_norm) * 0.25)))
          distances  <- utils::adist(input_norm, meta_norm, ignore.case = TRUE)[1L, ]
          fuzzy_idx  <- which(distances <= max_dist)
          fuzzy_idx  <- fuzzy_idx[order(distances[fuzzy_idx])]
          close_idx  <- utils::head(unique(c(prefix_idx, fuzzy_idx)), 5L)

          if (length(close_idx) > 0L) {
            cli::cli_alert_info(switch(lang,
              en = "Did you mean:",
              es = "\u00bfQuiso decir:",
              "Voce quis dizer:"
            ))
            for (s in sprintf("%s (%s \u2014 %s)",
                              muni_meta_import$name[close_idx],
                              muni_meta_import$uf_code[close_idx],
                              muni_meta_import$municipio[close_idx]))
              cli::cli_li(s)
          } else {
            cli::cli_alert_info(
              "Nenhuma sugestao disponivel. Verifique a grafia ou use o codigo IBGE."
            )
          }
        }
      }
    }

    if (n_city_not_found > 0L) {
      msg_agg <- switch(lang,
        en = paste0("[METODOLOGIA] ", n_city_not_found, " of ", length(city),
                    " requested municipalities were not found and were skipped."),
        es = paste0("[METODOLOGIA] ", n_city_not_found, " de ", length(city),
                    " municipios solicitados no fueron encontrados y se omitieron."),
        paste0("[METODOLOGIA] ", n_city_not_found, " de ", length(city),
              " municipios solicitados nao foram encontrados e foram ignorados.")
      )
      cli::cli_alert_warning(msg_agg)
    }

    if (!is.null(municipality_code))
      collected_codes <- c(collected_codes, as.character(municipality_code))

    if (length(collected_codes) > 0L) {
      all_muni_codes_import <- unique(c(
        as.character(collected_codes),
        substr(collected_codes, 1L, 6L)
      ))
      if (verbose) {
        msg_muni <- switch(lang,
          en = paste0("Municipality filter: ", length(all_muni_codes_import),
                      " code(s) (6+7-digit). Applied after download."),
          es = paste0("Filtro de municipio: ", length(all_muni_codes_import),
                      " codigo(s) (formatos 6+7 digitos). Se aplica tras la descarga."),
          paste0("Filtro de municipio: ", length(all_muni_codes_import),
                 " codigo(s) (formatos 6+7 digitos). Aplicado apos download.")
        )
        cli::cli_alert_info(msg_muni)
      }
    } else {
      cli::cli_alert_warning(
        "Nenhum municipio resolvido. Filtro de municipio sera ignorado."
      )
    }
  }

  # Header
  if (verbose) {
    cli::cli_h1("Climasus4r Data Import")
    cli::cli_alert_info("System: {system}")
    cli::cli_alert_info("States: {paste(uf, collapse = ', ')}")
    cli::cli_alert_info("Years: {paste(year, collapse = ', ')}")
    if (!is.null(month)) {
      cli::cli_alert_info("Months: {paste(month, collapse = ', ')}")
    }
    cli::cli_alert_info("Total downloads: {length(uf) * length(year)}")
    cli::cli_alert_info("Cache: {ifelse(use_cache, 'ENABLED', 'DISABLED')}")
    if (use_cache) cli::cli_alert_info("Cache directory: {cache_dir}")
    if (parallel && length(uf) * length(year) > 1) {
      cli::cli_alert_info("Parallel processing: ENABLED ({workers} workers)")
    }
  }

  #New: Is national sys
  is_national <- system %in% national_systems

  # UF -> IBGE state code (2-digit) — always available for all branches
  uf_to_code <- c(
    "AC" = 12, "AL" = 27, "AP" = 16, "AM" = 13, "BA" = 29, "CE" = 23,
    "DF" = 53, "ES" = 32, "GO" = 52, "MA" = 21, "MT" = 51, "MS" = 50,
    "MG" = 31, "PA" = 15, "PB" = 25, "PR" = 41, "PE" = 26, "PI" = 22,
    "RJ" = 33, "RN" = 24, "RS" = 43, "RO" = 11, "RR" = 14, "SC" = 42,
    "SP" = 35, "SE" = 28, "TO" = 17
  )

  if (is_national) {
    # Criamos params com apenas UMA combinacao (primeira UF)
    # Mas mantemos todas as UFs para filtragem posterior
    params <- expand.grid(
      year = year,
      uf = uf[1],  # Usa apenas a primeira UF para download
      system = system,
      stringsAsFactors = FALSE
    )

    # Guarda todas as UFs para filtragem
    all_ufs <- uf
  } else {
    # Para sistemas normais, criamos grid completo
    params <- expand.grid(
      year = year,
      uf = uf,
      system = system,
      stringsAsFactors = FALSE
    )
  }
  # Create a grid of all combinations
  # params <- expand.grid(
  #   year = year,
  #   uf = uf,
  #   system = system,
  #   stringsAsFactors = FALSE
  # )
  # Add month to params if specified
  # if (!is.null(month)) {
  #   params <- expand.grid(
  #     year = year,
  #     #uf = uf,
  #     uf = if (is_national) uf[1] else uf,
  #     month = month,
  #     system = system,
  #     stringsAsFactors = FALSE
  #   )
  #   if (is_national) all_ufs <- uf
  # }
  if (!is.null(month)) {
    if (is_national) {
      # Para sistemas nacionais com mes, criar grid com apenas uma UF
      params <- expand.grid(
        year = year,
        uf = uf[1],  # Apenas a primeira UF
        month = month,
        system = system,
        stringsAsFactors = FALSE
      )
      all_ufs <- uf  # Guardar todas para filtragem
    } else {
      # Para sistemas normais, criar grid completo
      params <- expand.grid(
        year = year,
        uf = uf,
        month = month,
        system = system,
        stringsAsFactors = FALSE
      )
    }
  }
  # Function to generate cache key
  # generate_cache_key <- function(year_i, uf_i, system_i, month_i = NULL) {
  #   if (!is.null(month_i)) {
  #     key_string <- paste(system_i, uf_i, year_i, month_i, sep = "_")
  #   } else {
  #     key_string <- paste(system_i, uf_i, year_i, sep = "_")
  #   }
  #   digest::digest(key_string, algo = "md5")
  # }
  generate_cache_key <- function(year_i, uf_i, system_i, month_i = NULL, is_national = FALSE) {
  if (is_national) {
    # Para sistemas nacionais, ignoramos UF na chave
    if (!is.null(month_i)) {
      key_string <- paste(system_i, year_i, month_i, sep = "_")
    } else {
      key_string <- paste(system_i, year_i, sep = "_")
    }
  } else {
    if (!is.null(month_i)) {
      key_string <- paste(system_i, uf_i, year_i, month_i, sep = "_")
    } else {
      key_string <- paste(system_i, uf_i, year_i, sep = "_")
    }
  }
  if (rlang::is_installed("digest")) {
    digest::digest(key_string, algo = "md5")
  } else {
    gsub("[^a-zA-Z0-9]", "_", key_string)
  }
}
  # Function to get cache file path
  get_cache_path <- function(cache_key) {
    if (requireNamespace("arrow", quietly = TRUE)) { 
      file.path(cache_dir, paste0(cache_key, ".parquet"))
    } else { 
      file.path(cache_dir, paste0(cache_key, ".rds"))
    }    
  }

  # Function to check if cache is valid (not older than 30 days by default)
  is_cache_valid <- function(cache_path, max_age_days = 30) {
    if (!fs::file_exists(cache_path)) return(FALSE)
    
    file_info <- fs::file_info(cache_path)
    cache_age <- difftime(Sys.time(), file_info$modification_time, units = "days")
    
    cache_age <= max_age_days
  }
  
  # Load data from cache
load_from_cache <- function(cache_path, year_i, uf_i, system_i, month_i = NULL, is_national = FALSE) {
  if (verbose) {
    if (is_national) {
      if (!is.null(month_i)) {
        cli::cli_alert_success("Loading from cache (national): {system_i} - {year_i} - Month {month_i}")
      } else {
        cli::cli_alert_success("Loading from cache (national): {system_i} - {year_i}")
      }
    } else {
      if (!is.null(month_i)) {
        cli::cli_alert_success("Loading from cache: {system_i} - {uf_i} - {year_i} - Month {month_i}")
      } else {
        cli::cli_alert_success("Loading from cache: {system_i} - {uf_i} - {year_i}")
      }
    }
  }
  
  if (requireNamespace("arrow", quietly = TRUE)) {
    return(suppressMessages(suppressWarnings(arrow::read_parquet(cache_path))))
  } else {
    return(readRDS(cache_path))
  }
}

# Save data to cache
save_to_cache <- function(data, cache_path, year_i, uf_i, system_i, month_i = NULL, is_national = FALSE) {
  if (verbose) {
    if (is_national) {
      if (!is.null(month_i)) {
        cli::cli_alert_info("Saving to cache (national): {system_i} - {year_i} - Month {month_i}")
      } else {
        cli::cli_alert_info("Saving to cache (national): {system_i} - {year_i}")
      }
    } else {
      if (!is.null(month_i)) {
        cli::cli_alert_info("Saving to cache: {system_i} - {uf_i} - {year_i} - Month {month_i}")
      } else {
        cli::cli_alert_info("Saving to cache: {system_i} - {uf_i} - {year_i}")
      }
    }
  }
  
  if (requireNamespace("arrow", quietly = TRUE)) {
    suppressMessages(suppressWarnings(arrow::write_parquet(data, cache_path)))
  } else {
    saveRDS(data, cache_path)
  }
  
  return(data)
}

  
  # Auxiliary function to download and process a single combination
  download_one <- function(year_i, uf_i, system_i, month_i = NULL, p = NULL, 
                           use_cache, force_redownload, cache_dir, verbose, is_national=FALSE) {
    
    if (verbose && !is.null(p)) {
        if (is_national) {
        p(message = sprintf("Processing (National-SINAN): %s - %s", system_i, year_i))
      } else { 
          if (!is.null(month_i)) {
          p(message = sprintf("Processing: %s - %s - %s - Month %s", system_i, uf_i, year_i, month_i))
        } else {
          p(message = sprintf("Processing: %s - %s - %s", system_i, uf_i, year_i))
        }
      }
      
    }
    
    # Generate cache key and path
    cache_key <- generate_cache_key(year_i, uf_i, system_i, month_i, is_national)
    # The tibble cache stores post-filter data: keep unfiltered data apart
    if (!filter_residence_uf) cache_key <- paste0(cache_key, "_nofilter")
    cache_path <- get_cache_path(cache_key)
    
    # Check cache first (if enabled and not forcing re-download)
    if (use_cache && !force_redownload && fs::file_exists(cache_path) && is_cache_valid(cache_path)) {
      if (verbose && !is.null(p)) {
        if (!is.null(month_i)) {
          p(message = sprintf("From cache: %s - %s - %s - Month %s", system_i, uf_i, year_i, month_i))
        } else {
          p(message = sprintf("From cache: %s - %s - %s", system_i, uf_i, year_i))
        }
      }
      tryCatch({
        return(load_from_cache(cache_path, year_i, uf_i, system_i, month_i, is_national))
      }, error = function(e) {
        if (verbose) {
          cli::cli_alert_warning("Cache corrupted, re-downloading: {system_i} - {uf_i} - {year_i}")
        }
      })
    }
    
    # Download from DATASUS
    tryCatch({
      if (verbose && !is.null(p)) {
        if (!is.null(month_i)) {
          p(message = sprintf("Downloading: %s - %s - %s - Month %s", system_i, uf_i, year_i, month_i))
        } else {
          p(message = sprintf("Downloading: %s - %s - %s", system_i, uf_i, year_i))
        }
      }
      # Prepare parameters for fetch_datasus
      params_list <- list(
        year_start = as.numeric(year_i),
        year_end = as.numeric(year_i),
        uf = uf_i,
        information_system = system_i
      )
      # Add month parameters if specified
      if (!is.null(month_i)) {
        params_list$month_start <- month_i
        params_list$month_end <- month_i
      }

      data <- do.call(microdatasus::fetch_datasus, params_list)

      # Safety-net: keep only rows whose municipality prefix matches the requested state
      if (filter_residence_uf && !is_national && !is.null(data) && nrow(data) > 0L) {
        state_code <- sprintf("%02d", as.integer(uf_to_code[toupper(uf_i)]))
        if (!is.na(state_code) && nchar(state_code) == 2L) {
          muni_candidates <- c(
            "CODMUNRES", "CODMUNOCOR", "MUNIC_RES", "MUNIC_MOV",
            "CODMUNNASC", "CODMUN", "PA_MUNPCN", "MUNAIH"
          )
          mf <- muni_candidates[muni_candidates %in% names(data)][1L]
          if (!is.na(mf)) {
            vals    <- substr(as.character(data[[mf]]), 1L, 2L)
            n_before <- nrow(data)
            data    <- data[vals %in% state_code | is.na(data[[mf]]), , drop = FALSE]
            n_removed <- n_before - nrow(data)
            if (n_removed > 0L && verbose)
              cli::cli_alert_info(
                "Filtro UF '{uf_i}': {n_removed} registro(s) de outro(s) estado(s) removido(s)."
              )
          }
        }
      }

      # Save to cache if enabled
      if (use_cache && !is.null(data) && nrow(data) > 0) {
        save_to_cache(data, cache_path, year_i, uf_i, system_i, month_i, is_national)
      }
      
      if (verbose && !is.null(p)) {
        if (!is.null(month_i)) {
          p(message = sprintf("Completed: %s - %s - %s - Month %s", system_i, uf_i, year_i, month_i))
        } else {
          p(message = sprintf("Completed: %s - %s - %s", system_i, uf_i, year_i))
        }
      }
      
      return(data)
      
    }, error = function(e) {
      if (verbose) {
        if (!is.null(month_i)) {
          cli::cli_alert_warning("Failed to download: {system_i} - {uf_i} - {year_i} - Month {month_i}")
        } else {
          cli::cli_alert_warning("Failed to download: {system_i} - {uf_i} - {year_i}")
        }
        cli::cli_alert_danger("Error: {e$message}")
        
        # Provide helpful suggestions based on error
        if (grepl("character string.*standard unambiguous format", e$message)) {
          cli::cli_alert_info("Try: Check if system '{system_i}' exists in DATASUS")
          cli::cli_alert_info("Try: Verify year {year_i} is available for {system_i}")
          cli::cli_alert_info("Try: The system name might be incorrect")
        }
        
        if (grepl("timeout", e$message, ignore.case = TRUE)) {
          cli::cli_alert_info("Try: Increase timeout value (current: {timeout}s)")
          cli::cli_alert_info("Try: Check your internet connection")
        }
        
        if (grepl("404|Not Found", e$message)) {
          cli::cli_alert_info("Try: The data might not be available for this combination")
          cli::cli_alert_info("Try: Check DATASUS website for data availability")
        }
      }
      return(NULL)
    })
  }
  n_months <- if (is.null(month)) 1 else length(month)
  #total_tasks <- length(uf) * length(year) * n_months
  total_tasks <- nrow(params)
  # Execute downloads (parallel or sequential)
  if (parallel && total_tasks > 1) {

   old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)

    future::plan(
      future::multisession,
      workers = workers
    )
    if (verbose) {cli::cli_alert_info("Parallel processing enabled ({workers} workers)")}

    list_of_dfs <- furrr::future_map(
      seq_len(nrow(params)),
      function(i) {

        if (!is.null(month)) {
          download_one(
            year_i = params$year[i],
            uf_i = params$uf[i],
            system_i = params$system[i],
            #month_i = params$month[i],
            month_i = if ("month" %in% names(params)) params$month[i] else NULL,
            use_cache = use_cache,
            force_redownload = force_redownload,
            cache_dir = cache_dir,
            verbose = FALSE,
            is_national = is_national
          )
        } else {
          download_one(
            year_i = params$year[i],
            uf_i = params$uf[i],
            system_i = params$system[i],
            month_i = NULL,
            use_cache = use_cache,
            force_redownload = force_redownload,
            cache_dir = cache_dir,
            verbose = verbose,
            is_national = is_national
          )
        }

      },
      .options = furrr::furrr_options(
        seed = TRUE,
        packages = c(
          "cli",
          "fs",
          "digest",
          "microdatasus",
          "dplyr",
          "arrow",
          "data.table"
        ), globals = c(
          "download_one",
          "generate_cache_key",
          "get_cache_path",
          "is_cache_valid",
          "load_from_cache",
          "save_to_cache",
          "is_national",
          "use_cache",
          "force_redownload",
          "filter_residence_uf",
          "cache_dir"
        )
      )
    )

  } else {
    list_of_dfs <- purrr::map(
      seq_len(nrow(params)),
      function(i) {
        download_one(
          year_i = params$year[i],
          uf_i = params$uf[i],
          system_i = params$system[i],
          month_i = if (!is.null(month)) params$month[i] else NULL,
          use_cache = use_cache,
          force_redownload = force_redownload,
          cache_dir = cache_dir,
          verbose = verbose,
          is_national = is_national
        )
      }
    )
  }

  
  # Remove any NULL results from failed downloads
  list_of_dfs <- list_of_dfs[!sapply(list_of_dfs, is.null)]
  
  if (length(list_of_dfs) == 0) {
    cli::cli_alert_danger("No data was successfully downloaded.")
    return(NULL)
  }
  
  # Combine all dataframes into one
  if (verbose) {
    cli::cli_alert_info("Combining {length(list_of_dfs)} datasets...")
  }
  
  #combined_data <- dplyr::bind_rows(list_of_dfs)
  # fill = TRUE: monthly files of one year can differ in column set (layout changes)
  combined_data <- data.table::rbindlist(list_of_dfs, use.names = TRUE, fill = TRUE)
  
  #NEW national system
  # Se for sistema nacional, filtrar pelos UFs solicitados
  if (is_national && exists("all_ufs")) {

    # Identificar coluna de UF (diferentes sistemas podem ter nomes diferentes)
    uf_codes <- uf_to_code[all_ufs]
    uf_cols <- c("SG_UF_NOT")
    combined_data <- data.table::as.data.table(combined_data)
    uf_col <- uf_cols[uf_cols %in% names(combined_data)][1]
    combined_data <- combined_data[as.factor(get(uf_col)) %in% uf_codes]
  }

  # Municipality filter (city / municipality_code)
  if (!is.null(all_muni_codes_import) && length(all_muni_codes_import) > 0L) {
    # muni_col_patterns <- c(
    #   "CODMUNRES", "MUNRES", "MUNAIH", "ID_MN_RESI",
    #   "PA_MUNPCN", "CODMUNPAC", "CODUFMUN", "MUNNOT",
    #   "MUNIC_RES", "SP_GESTOR"
    # )
    muni_col_patterns <- c(
      # Originais fornecidos (residencia)
      "CODMUNRES", "MUNRES", "MUNAIH", "ID_MN_RESI",
      "PA_MUNPCN", "CODMUNPAC",
      "MUNIC_RES", "SP_GESTOR",

      # Ocorrencia e notificacao -- NAO e residencia (CODUFMUN = municipio
      # de ocorrencia no SIM; MUNNOT = municipio de notificacao no SINAN).
      # Mantidos aqui apenas como ultimo recurso, depois de todo campo de
      # residencia acima, para nao serem escolhidos por engano quando ambos
      # existirem na mesma base (ver correcao analoga em
      # sus_data_filter_demographics()).
      "CODUFMUN", "MUNNOT", "CODMUNOCOR", "MUNOCOR", "ID_MN_OCOR",
      "CODMUNNASC", "CODMUNNATU", "CODMUNINF", "MUNIC_OCOR",

      # Adicionais SIH  SIA (Hospitalar e Ambulatorial)
      "MUNIC_ESTAB", "CODMUNEST", "MUN_ESTAB", "CODMUNSERV",
      "PA_MUNORG", "SP_MUNINV", "SP_MUNCID",

      # Adicionais CNES  IBGE (Gerais)
      "IBGE", "COD_MUNICIPIO", "MUNICIPIO_CODIGO", "CODMUN",
      "COD_IBGE", "CODMUNI", "MUN_RESIDENCIA"
    )
    combined_data <- data.table::as.data.table(combined_data)
    combined_cols  <- names(combined_data)
    muni_col       <- muni_col_patterns[muni_col_patterns %in% combined_cols][1L]

    if (!is.na(muni_col) && length(muni_col) > 0L) {
      # NOTE: Municipality filter is applied LAZY in .import_arrow_internal, not here
      # This keeps tibble data complete for downstream Arrow conversion
      n_before <- nrow(combined_data)
      n_after <- n_before  # No filtering here - will be done lazy later

      # if (verbose) {
      #   msg_filt <- switch(lang,
      #     en = paste0("Municipality filter on '", muni_col, "': ",
      #                 format(n_after,  big.mark = ","), " of ",
      #                 format(n_before, big.mark = ","), " rows retained."),
      #     es = paste0("Filtro de municipio en '", muni_col, "': ",
      #                 format(n_after,  big.mark = ","), " de ",
      #                 format(n_before, big.mark = ","), " registros retenidos."),
      #     paste0("Filtro de municipio na coluna '", muni_col, "': ",
      #            format(n_after,  big.mark = ","), " de ",
      #            format(n_before, big.mark = ","), " registros retidos.")
      #   )
      #   cli::cli_alert_info(msg_filt)
      # }
      if (verbose) {
        # Construir descrição do filtro aplicado
        if (!is.null(city) && !is.null(municipality_code)) {
          filtro_desc <- switch(lang,
            en = paste0("Municipality filter (city: ", 
                        paste(city, collapse = ", "), 
                        "; code: ", 
                        paste(municipality_code, collapse = ", "), 
                        ")"),
            es = paste0("Filtro de municipio (ciudad: ", 
                        paste(city, collapse = ", "), 
                        "; codigo: ", 
                        paste(municipality_code, collapse = ", "), 
                        ")"),
            paste0("Filtro de municipio (cidade: ", 
                  paste(city, collapse = ", "), 
                  "; codigo: ", 
                  paste(municipality_code, collapse = ", "), 
                  ")")
          )
        } else if (!is.null(city)) {
          filtro_desc <- switch(lang,
            en = paste0("Municipality filter (city: ", 
                        paste(city, collapse = ", "), 
                        ")"),
            es = paste0("Filtro de municipio (ciudad: ", 
                        paste(city, collapse = ", "), 
                        ")"),
            paste0("Filtro de municipio (cidade: ", 
                  paste(city, collapse = ", "), 
                  ")")
          )
        } else if (!is.null(municipality_code)) {
          filtro_desc <- switch(lang,
            en = paste0("Municipality filter (code: ", 
                        paste(municipality_code, collapse = ", "), 
                        ")"),
            es = paste0("Filtro de municipio (codigo: ", 
                        paste(municipality_code, collapse = ", "), 
                        ")"),
            paste0("Filtro de municipio (codigo: ", 
                  paste(municipality_code, collapse = ", "), 
                  ")")
          )
        }
        
        msg_filt <- switch(lang,
          en = paste0(filtro_desc, ": ",
                      format(n_after,  big.mark = ","), " of ",
                      format(n_before, big.mark = ","), " rows retained."),
          es = paste0(filtro_desc, ": ",
                      format(n_after,  big.mark = ","), " de ",
                      format(n_before, big.mark = ","), " registros retenidos."),
          paste0(filtro_desc, ": ",
                format(n_after,  big.mark = ","), " de ",
                format(n_before, big.mark = ","), " registros retidos.")
        )
        cli::cli_alert_info(msg_filt)
      }

      # Zero-row diagnostic: list available municipalities
      if (n_after == 0L && length(available_muni_codes) > 0L) {
        muni_meta_diag <- if (exists("muni_meta_import")) muni_meta_import else NULL
        show_available_municipalities_internal(
          available_muni_codes = available_muni_codes,
          muni_meta            = muni_meta_diag,
          cache_dir            = file.path(dirname(cache_dir), "spatial"),
          use_cache            = use_cache,
          lang                 = lang
        )
      }
    } else {
      msg_nocol <- switch(lang,
        en = paste0(
          "Municipality column not found in combined dataset. Filter skipped. ",
          "Available (first 10): ",
          paste(utils::head(combined_cols, 10L), collapse = ", ")
        ),
        es = "Columna de municipio no encontrada. Filtro omitido.",
        paste0(
          "Coluna de municipio nao encontrada no dataset combinado. ",
          "Filtro ignorado. Colunas disponiveis (10 primeiras): ",
          paste(utils::head(combined_cols, 10L), collapse = ", ")
        )
      )
      cli::cli_alert_warning(msg_nocol)
    }
  }

  # Add cache metadata as attribute
  if (use_cache) {
    cache_info <- list(
      cache_dir = cache_dir,
      cached_items = nrow(params),
      retrieved_from_cache = nrow(params) - length(list_of_dfs),
      cache_size = sum(file.info(list.files(cache_dir, full.names = TRUE))$size, na.rm = TRUE),
      timestamp = Sys.time()
    )
    attr(combined_data, "cache_info") <- cache_info
  }
    
  if (verbose) {
    cli::cli_alert_success("Process completed successfully!")
    cli::cli_alert_info("Total records: {format(nrow(combined_data), big.mark = ',')}")
    cli::cli_alert_info("Total columns: {ncol(combined_data)}")
    if (use_cache) {
      cli::cli_alert_info("Cache saved to: {cache_dir}")
    }
  }
  
  if (!inherits(combined_data, "climasus_df")) {
      # Create new climasus_df
      meta <- list(
        system = NULL,
        stage = "import",
        type = "raw",
        spatial = FALSE,
        temporal = NULL,
        created = Sys.time(),
        modified = Sys.time(),
        history = sprintf(
          "[%s] Imported datasus with climasus4r",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        ),
        user = list()
      )

      base_classes <- setdiff(class(combined_data), "climasus_df")
      combined_data <- structure(
        combined_data,
        sus_meta = meta,
        class = c("climasus_df", base_classes)
      )
    } else {
      # Already climasus_df - update metadata
      combined_data <- sus_meta(combined_data, system = system, stage = "import", type = "raw")
      combined_data <- sus_meta(combined_data, add_history = "Imported datasus with climasus4r")
    }
    return(combined_data)
}

.import_arrow_internal <- function(
     uf, region, year, system, month, city, municipality_code,
     use_cache, cache_dir, force_redownload, filter_residence_uf, parallel,
     workers, lang, verbose
    ) {
  #  0. Validacoes basicas 
  if (!lang %in% c("en", "pt", "es"))
    cli::cli_abort("{.arg lang} deve ser um de: 'en', 'pt', 'es'.")
  
  # Fail fast if arrow is absent — triggers clean fallback to tibble backend
  if (!requireNamespace("arrow", quietly = TRUE)) {
    cli::cli_abort("arrow not available", call = NULL)
  }

  arrow_threads = NULL
  # Thread pool do Arrow: independente do future, gerenciado por libarrow C++
  n_arrow_threads <- arrow_threads %||% parallel::detectCores(logical = FALSE)
  if (requireNamespace("arrow", quietly = TRUE)) {
    suppressMessages(suppressWarnings({
      arrow::set_cpu_count(n_arrow_threads)
      arrow::set_io_thread_count(n_arrow_threads)
    }))
  }

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
    
    # 3. Aviso se o usuario fornecer mes para sistemas que geralmente sa anuais (SIM, SINASC, SINAN)
    if (!system_prefix_month %in% systems_requiring_month) {
      cli::cli_alert_warning("Parameter 'month' is provided but typically not used for {system_prefix_month} systems.")
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
  # e aplicado LAZY sobre o Dataset resultante- sem collect antecipado.

  national_systems <- c(
    "SINAN-DENGUE","SINAN-CHIKUNGUNYA","SINAN-ZIKA","SINAN-MALARIA",
    "SINAN-CHAGAS","SINAN-LEISHMANIOSE-VISCERAL",
    "SINAN-LEISHMANIOSE-TEGUMENTAR","SINAN-LEPTOSPIROSE"
  )
  # ===========================================================================
  # new Filtr city
  # ===========================================================================
  all_muni_codes_import <- NULL

if (!is.null(city) || !is.null(municipality_code)) {
    spatial_cache_dir <- file.path(dirname(cache_dir))
    if (!fs::dir_exists(spatial_cache_dir))
      fs::dir_create(spatial_cache_dir, recurse = TRUE)

    muni_meta_import <- tryCatch(
      get_spatial_municipio_cache(
        cache_dir = spatial_cache_dir,
        use_cache = use_cache,
        lang      = lang,
        verbose   = verbose
      ),
      error = function(e) {
        cli::cli_alert_warning(
          "Nao foi possivel carregar metadados municipais: {conditionMessage(e)}"
        )
        NULL
      }
    )

    # Restrict city search to requested UF(s) — prevents cross-state name matches.
    # Falls back to searching all municipalities when uf_code column is absent
    # or when the UF filter would return 0 rows (e.g. stale cache with old schema).
    if (!is.null(uf) && !is.null(muni_meta_import) &&
        !identical(tolower(uf), "all")) {
      if ("uf_code" %in% names(muni_meta_import)) {
        uf_filtered <- muni_meta_import[
          toupper(muni_meta_import$uf_code) %in% toupper(uf), , drop = FALSE
        ]
        if (nrow(uf_filtered) > 0L) {
          muni_meta_import <- uf_filtered
        }
      }
    }

    collected_codes <- character(0L)
    n_city_not_found <- 0L

    if (!is.null(city) && !is.null(muni_meta_import)) {
      meta_norm <- stringi::stri_trans_general(
        tolower(muni_meta_import$no_accents), "Latin-ASCII"
      )
      for (city_i in city) {
        input <- trimws(as.character(city_i))

        if (grepl("^\\d{6,7}$", input)) {
          idx <- if (nchar(input) == 7L)
            which(muni_meta_import$municipio == input)
          else
            which(substr(muni_meta_import$municipio, 1L, 6L) == input)
          if (length(idx) > 0L) {
            collected_codes <- c(collected_codes, muni_meta_import$municipio[idx])
          } else {
            cli::cli_alert_warning("Codigo de municipio {.val {input}} nao encontrado.")
            n_city_not_found <- n_city_not_found + 1L
          }
          next
        }

        input_norm <- stringi::stri_trans_general(tolower(input), "Latin-ASCII")
        idx <- which(meta_norm == input_norm)
        if (length(idx) == 0L) {
          orig_norm <- stringi::stri_trans_general(
            tolower(muni_meta_import$name), "Latin-ASCII"
          )
          idx <- which(orig_norm == input_norm)
        }

        if (length(idx) > 0L) {
          collected_codes <- c(collected_codes, muni_meta_import$municipio[idx])
        } else {
          msg_nf <- switch(lang,
            en = paste0("City '", city_i, "' not found."),
            es = paste0("Ciudad '", city_i, "' no encontrada."),
            paste0("Municipio '", city_i, "' nao encontrado.")
          )
          cli::cli_alert_warning(msg_nf)
          n_city_not_found <- n_city_not_found + 1L

          prefix_idx <- which(startsWith(meta_norm, input_norm))
          max_dist   <- max(1L, min(3L, floor(nchar(input_norm) * 0.25)))
          distances  <- utils::adist(input_norm, meta_norm, ignore.case = TRUE)[1L, ]
          fuzzy_idx  <- which(distances <= max_dist)
          fuzzy_idx  <- fuzzy_idx[order(distances[fuzzy_idx])]
          close_idx  <- utils::head(unique(c(prefix_idx, fuzzy_idx)), 5L)

          if (length(close_idx) > 0L) {
            cli::cli_alert_info(switch(lang,
              en = "Did you mean:",
              es = "\u00bfQuiso decir:",
              "Voce quis dizer:"
            ))
            for (s in sprintf("%s (%s \u2014 %s)",
                              muni_meta_import$name[close_idx],
                              muni_meta_import$uf_code[close_idx],
                              muni_meta_import$municipio[close_idx]))
              cli::cli_li(s)
          } else {
            cli::cli_alert_info(
              "Nenhuma sugestao disponivel. Verifique a grafia ou use o codigo IBGE."
            )
          }
        }
      }
    }

    if (n_city_not_found > 0L) {
      msg_agg <- switch(lang,
        en = paste0("[METODOLOGIA] ", n_city_not_found, " of ", length(city),
                    " requested municipalities were not found and were skipped."),
        es = paste0("[METODOLOGIA] ", n_city_not_found, " de ", length(city),
                    " municipios solicitados no fueron encontrados y se omitieron."),
        paste0("[METODOLOGIA] ", n_city_not_found, " de ", length(city),
              " municipios solicitados nao foram encontrados e foram ignorados.")
      )
      cli::cli_alert_warning(msg_agg)
    }

    if (!is.null(municipality_code))
      collected_codes <- c(collected_codes, as.character(municipality_code))

    if (length(collected_codes) > 0L) {
      all_muni_codes_import <- unique(c(
        as.character(collected_codes),
        substr(collected_codes, 1L, 6L)
      ))
      if (verbose) {
        msg_muni <- switch(lang,
          en = paste0("Municipality filter: ", length(all_muni_codes_import),
                      " code(s) (6+7-digit). Applied after download."),
          es = paste0("Filtro de municipio: ", length(all_muni_codes_import),
                      " codigo(s) (formatos 6+7 digitos). Se aplica tras la descarga."),
          paste0("Filtro de municipio: ", length(all_muni_codes_import),
                " codigo(s) (formatos 6+7 digitos). Aplicado apos download.")
        )
        cli::cli_alert_info(msg_muni)
      }
      
    } else {
      cli::cli_alert_warning(
        "Nenhum municipio resolvido. Filtro de municipio sera ignorado."
      )
    }
  }

  
  # ===========================================================================
  # check if national
  # ===========================================================================
  
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

  download_one_raw <- function(year_i, uf_i, system_i, month_i = NULL) {
    label <- paste(
      Filter(Negate(is.null), list(system_i, uf_i, year_i, month_i)),
      collapse = " . "
    )

    fetch_uf   <- if (uf_i == "BR") request_ufs[1L] else uf_i
    cache_key  <- .make_cache_key(system_i, uf_i, year_i, month_i, is_national)
    cache_path <- .cache_path(cache_key)

    # Cache hit: retorna apenas o path - nada e serializado via socket
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
      cli::cli_alert_danger("Falha: {label} - {conditionMessage(e)}")
      if (grepl("404|Not Found",  conditionMessage(e), ignore.case = TRUE))
        cli::cli_alert_info("  -> Combinacao pode nao existir no DATASUS.")
      if (grepl("timeout",        conditionMessage(e), ignore.case = TRUE))
        cli::cli_alert_info("  -> Verifique a conexao ou tente novamente.")
      NULL
    })
  }

  # 9. Execucao dos downloads (paralelo HTTP ou sequencial)

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
        "{n_fail} download(s) falharam - verifique os logs acima."
      )
  }

  # 12. Open dataset: arrow (lazy) → DuckDB → rbind fallback

  if (verbose)
    cli::cli_alert_info("Abrindo dataset ({n_ok} arquivo(s))...")

  dataset <- if (requireNamespace("arrow", quietly = TRUE)) {
    # arrow path: fully lazy, memory-efficient
    tryCatch({
      suppressMessages(suppressWarnings(
        arrow::open_dataset(
          sources       = valid_paths,
          format        = "parquet",
          unify_schemas = TRUE
        )
      ))
    }, error = function(e) {
      cli::cli_alert_danger("Falha ao abrir Arrow dataset: {conditionMessage(e)}")
      cli::cli_alert_info("Verificando arquivos individualmente...")
      readable <- Filter(function(p) {
        tryCatch({
          suppressMessages(suppressWarnings(
            arrow::open_dataset(p, format = "parquet")
          ))
          TRUE
        }, error = function(e2) {
          cli::cli_alert_warning("Corrompido: {basename(p)}")
          FALSE
        })
      }, valid_paths)
      if (length(readable) == 0L)
        cli::cli_abort("Nenhum arquivo Parquet legivel. Verifique os logs.")
      cli::cli_alert_info("Reabrindo com {length(readable)}/{n_ok} arquivo(s)...")
      suppressMessages(suppressWarnings(
        arrow::open_dataset(sources = readable, format = "parquet",
                            unify_schemas = TRUE)
      ))
    })
  } else {
    # Fallback: DuckDB lazy (duckdb is in Imports, always available)
    if (verbose) cli::cli_alert_info("arrow indisponivel \u2014 usando DuckDB.")
    con_ds <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    glob   <- paste0("'", valid_paths, "'", collapse = ", ")
    tryCatch(
      dplyr::tbl(con_ds, paste0("read_parquet([", glob, "], union_by_name=true)")),
      error = function(e) {
        DBI::dbDisconnect(con_ds, shutdown = TRUE)
        cli::cli_alert_warning("DuckDB falhou, usando rbind...")
        do.call(rbind, lapply(valid_paths, .read_parquet_smart))
      }
    )
  }

  # 13. Filtro lazy de UF — aplicado a TODOS os sistemas
  if (length(request_ufs) > 0L) {
    codes_wanted    <- unname(uf_to_code[toupper(request_ufs)])
    codes_wanted    <- codes_wanted[!is.na(codes_wanted)]
    state_str_codes <- sprintf("%02d", as.integer(codes_wanted))

    if (length(codes_wanted) > 0L) {
      if (is_national) {
        # SINAN: SG_UF_NOT contém código IBGE numérico direto
        dataset <- dataset |>
          dplyr::filter(as.integer(SG_UF_NOT) %in% codes_wanted)
      } else {
        # Sistemas estaduais: filtrar pelo prefixo (2 dígitos) do código municipal
        muni_col_candidates <- c(
          "CODMUNRES", "CODMUNOCOR", "MUNIC_RES", "MUNIC_MOV",
          "CODMUNNASC", "CODMUN", "PA_MUNPCN", "MUNAIH"
        )
        ds_cols <- names(dataset)
        mf      <- muni_col_candidates[muni_col_candidates %in% ds_cols][1L]
        if (filter_residence_uf && !is.na(mf) && !is.null(mf)) {
          uf_pattern <- paste0("^(", paste(state_str_codes, collapse = "|"), ")")
          dataset <- dataset |>
            dplyr::filter(
              is.na(!!rlang::sym(mf)) |
              grepl(uf_pattern, !!rlang::sym(mf))
            )
          if (verbose)
            cli::cli_alert_info(
              "Filtro lazy de UF na coluna '{mf}' ({length(codes_wanted)} UF(s))."
            )
        }
      }

      if (verbose)
        cli::cli_alert_info(
          "Filtro de UF: {length(codes_wanted)} codigo(s) IBGE aplicado(s)."
        )
    }
  }

  # Municipality filter (city / municipality_code)
if (!is.null(all_muni_codes_import) && length(all_muni_codes_import) > 0L) {
    muni_col_patterns <- c(
      # Originais fornecidos (residencia)
      "CODMUNRES", "MUNRES", "MUNAIH", "ID_MN_RESI",
      "PA_MUNPCN", "CODMUNPAC",
      "MUNIC_RES", "SP_GESTOR", "MUN_RES",

      # Ocorrencia e notificacao -- NAO e residencia (CODUFMUN = municipio
      # de ocorrencia no SIM; MUNNOT = municipio de notificacao no SINAN).
      # Mantidos aqui apenas como ultimo recurso, depois de todo campo de
      # residencia acima, para nao serem escolhidos por engano quando ambos
      # existirem na mesma base (ver correcao analoga em
      # sus_data_filter_demographics()).
      "CODUFMUN", "MUNNOT", "CODMUNOCOR", "MUNOCOR", "ID_MN_OCOR",
      "CODMUNNASC", "CODMUNNATU", "CODMUNINF", "MUNIC_OCOR",

      # Adicionais SIH  SIA (Hospitalar e Ambulatorial)
      "MUNIC_ESTAB", "CODMUNEST", "MUN_ESTAB", "CODMUNSERV",
      "PA_MUNORG", "SP_MUNINV", "SP_MUNCID",
      
      # Adicionais CNES  IBGE (Gerais)
      "IBGE", "COD_MUNICIPIO", "MUNICIPIO_CODIGO", "CODMUN",
      "COD_IBGE", "CODMUNI", "MUN_RESIDENCIA"
    ) 
    # Para Arrow Dataset, pegamos os nomes das colunas do schema
    combined_cols <- names(dataset)
    muni_col <- muni_col_patterns[muni_col_patterns %in% combined_cols][1L]

    if (!is.na(muni_col) && length(muni_col) > 0L) {
      
      # Para contar linhas antes do filtro (lazy)
      n_before <- dataset %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::collect() %>%
        dplyr::pull(n)
      
      # Snapshot available codes for zero-row diagnostic (precisa coletar)
      available_muni_codes <- dataset %>%
        dplyr::select(!!rlang::sym(muni_col)) %>%
        dplyr::distinct() %>%
        dplyr::collect() %>%
        dplyr::pull(!!rlang::sym(muni_col)) %>%
        as.character()
      
      available_muni_codes <- available_muni_codes[
        !is.na(available_muni_codes) & nchar(available_muni_codes) >= 6L
      ]

      # Aplicar filtro no Arrow Dataset (lazy)
      dataset <- dataset %>%
        dplyr::filter(
          as.character(!!rlang::sym(muni_col)) %in% all_muni_codes_import
        )
      
      # Contar linhas depois do filtro (lazy)
      n_after <- dataset %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::collect() %>%
        dplyr::pull(n)

      # if (verbose) {
      #   msg_filt <- switch(lang,
      #     en = paste0("Municipality filter on '", muni_col, "': ",
      #                 format(n_after,  big.mark = ","), " of ",
      #                 format(n_before, big.mark = ","), " rows retained."),
      #     es = paste0("Filtro de municipio en '", muni_col, "': ",
      #                 format(n_after,  big.mark = ","), " de ",
      #                 format(n_before, big.mark = ","), " registros retenidos."),
      #     paste0("Filtro de municipio na coluna '", muni_col, "': ",
      #           format(n_after,  big.mark = ","), " de ",
      #           format(n_before, big.mark = ","), " registros retidos.")
      #   )
      #   cli::cli_alert_info(msg_filt)
      # }
       # }
      if (verbose) {
        # Construir descrição do filtro aplicado
        if (!is.null(city) && !is.null(municipality_code)) {
          filtro_desc <- switch(lang,
            en = paste0("Municipality filter (city: ", 
                        paste(city, collapse = ", "), 
                        "; code: ", 
                        paste(municipality_code, collapse = ", "), 
                        ")"),
            es = paste0("Filtro de municipio (ciudad: ", 
                        paste(city, collapse = ", "), 
                        "; codigo: ", 
                        paste(municipality_code, collapse = ", "), 
                        ")"),
            paste0("Filtro de municipio (cidade: ", 
                  paste(city, collapse = ", "), 
                  "; codigo: ", 
                  paste(municipality_code, collapse = ", "), 
                  ")")
          )
        } else if (!is.null(city)) {
          filtro_desc <- switch(lang,
            en = paste0("Municipality filter (city: ", 
                        paste(city, collapse = ", "), 
                        ")"),
            es = paste0("Filtro de municipio (ciudad: ", 
                        paste(city, collapse = ", "), 
                        ")"),
            paste0("Filtro de municipio (cidade: ", 
                  paste(city, collapse = ", "), 
                  ")")
          )
        } else if (!is.null(municipality_code)) {
          filtro_desc <- switch(lang,
            en = paste0("Municipality filter (code: ", 
                        paste(municipality_code, collapse = ", "), 
                        ")"),
            es = paste0("Filtro de municipio (codigo: ", 
                        paste(municipality_code, collapse = ", "), 
                        ")"),
            paste0("Filtro de municipio (codigo: ", 
                  paste(municipality_code, collapse = ", "), 
                  ")")
          )
        }
        
        msg_filt <- switch(lang,
          en = paste0(filtro_desc, ": ",
                      format(n_after,  big.mark = ","), " of ",
                      format(n_before, big.mark = ","), " rows retained."),
          es = paste0(filtro_desc, ": ",
                      format(n_after,  big.mark = ","), " de ",
                      format(n_before, big.mark = ","), " registros retenidos."),
          paste0(filtro_desc, ": ",
                format(n_after,  big.mark = ","), " de ",
                format(n_before, big.mark = ","), " registros retidos.")
        )
        cli::cli_alert_info(msg_filt)
      }

      # Zero-row diagnostic: list available municipalities
      if (n_after == 0L && length(available_muni_codes) > 0L) {
        muni_meta_diag <- if (exists("muni_meta_import")) muni_meta_import else NULL
        show_available_municipalities_internal(
          available_muni_codes = available_muni_codes,
          muni_meta            = muni_meta_diag,
          cache_dir            = file.path(dirname(cache_dir), "spatial"),
          use_cache            = use_cache,
          lang                 = lang
        )
      }
    } else {
      msg_nocol <- switch(lang,
        en = paste0(
          "Municipality column not found in combined dataset. Filter skipped. ",
          "Available (first 10): ",
          paste(utils::head(combined_cols, 10L), collapse = ", ")
        ),
        es = "Columna de municipio no encontrada. Filtro omitido.",
        paste0(
          "Coluna de municipio nao encontrada no dataset combinado. ",
          "Filtro ignorado. Colunas disponiveis (10 primeiras): ",
          paste(utils::head(combined_cols, 10L), collapse = ", ")
        )
      )
      cli::cli_alert_warning(msg_nocol)
    }
  }



  #  14. Metadados climasus_dataset

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
        "N/A (dataset filtrado - use nrow(dplyr::collect(...)) para contar)"
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

  # 16. Aplicar filtro de municipality (lazy)
  if (!is.null(all_muni_codes_import) && length(all_muni_codes_import) > 0) {
    # Detectar coluna de município (varia por sistema)
    muni_col <- NULL
    schema_names <- names(dataset)

    if ("municipio_residencia" %in% schema_names) {
      muni_col <- "municipio_residencia"
    } else if ("municipio_estabelecimento" %in% schema_names) {
      muni_col <- "municipio_estabelecimento"
    } else if ("municipio_codigo_ibge" %in% schema_names) {
      muni_col <- "municipio_codigo_ibge"
    } else if ("municipio" %in% schema_names) {
      muni_col <- "municipio"
    }

    if (!is.null(muni_col)) {
      # Aplicar filtro lazily (6 e 7 dígitos)
      dataset <- dplyr::filter(
        dataset,
        substr(as.character(!!rlang::sym(muni_col)), 1, 7) %in% all_muni_codes_import |
          substr(as.character(!!rlang::sym(muni_col)), 1, 6) %in% all_muni_codes_import
      )

      if (verbose) {
        cli::cli_alert_success(
          "Municipio filter applied (lazy): {length(all_muni_codes_import)} code(s)"
        )
      }
    }
  }

  return(dataset)
}

# ===========================================================================
# Helper functions
# ===========================================================================

#Helper functions
#' @title Regional Definitions for Brazilian States
#' @description Internal dataset with state to region mappings including Biomes, 
#' Hydrography, and Epidemiological clusters.
#' @noRd
.br_regions <- list(
  # --- IBGE Macro-regions ---
  norte = c("AC", "AP", "AM", "PA", "RO", "RR", "TO"),
  nordeste = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE"),
  centro_oeste = c("DF", "GO", "MT", "MS"),
  sudeste = c("ES", "MG", "RJ", "SP"),
  sul = c("PR", "RS", "SC"),
  
  # --- Biomes (Ecological Borders) ---
  amazonia_legal = c("AC", "AP", "AM", "PA", "RO", "RR", "MT", "MA", "TO"),
  mata_atlantica = c("AL", "BA", "CE", "ES", "GO", "MA", "MG", "MS", "PB", 
                     "PE", "PI", "PR", "RJ", "RN", "RS", "SC", "SE", "SP"),
  caatinga = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "MG"),
  cerrado = c("BA", "DF", "GO", "MA", "MG", "MS", "MT", "PA", "PI", "PR", "RO", "SP", "TO"),
  pantanal = c("MT", "MS"),
  pampa = c("RS"),
  
  # --- Hydrography & Climate ---
  bacia_amazonica = c("AC", "AM", "AP", "MT", "PA", "RO", "RR"),
  bacia_sao_francisco = c("AL", "BA", "DF", "GO", "MG", "PE", "SE"),
  bacia_parana = c("GO", "MG", "MS", "PR", "SP"),
  bacia_tocantins = c("GO", "MA", "PA", "TO"),
  semi_arido = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "MG"),
  
  # --- Health, Agriculture & Geopolitics ---
  matopiba = c("MA", "TO", "PI", "BA"),
  arco_desmatamento = c("RO", "AC", "AM", "PA", "MT", "MA"),
  dengue_hyperendemic = c("GO", "MS", "MT", "PR", "RJ", "SP"),
  sudene = c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "MG", "ES"),
  fronteira_brasil = c("AC", "AM", "AP", "MT", "MS", "PA", "PR", "RO", "RR", "RS", "SC")
)

#' @description Multilingual aliases for the region argument (EN, ES, PT).
#' @noRd
.region_aliases <- list(
  # English
  north = "norte", northeast = "nordeste", central_west = "centro_oeste", 
  southeast = "sudeste", south = "sul", amazon = "amazonia_legal", 
  legal_amazon = "amazonia_legal", atlantic_forest = "mata_atlantica",
  semi_arid = "semi_arido", border = "fronteira_brasil",
  # Spanish
  noreste = "nordeste", sudeste = "sudeste", sur = "sul", 
  amazonia = "amazonia_legal", bosque_atlantico = "mata_atlantica",
  semiarido = "semi_arido", frontera = "fronteira_brasil"
)

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
    "Historico : {paste(meta$history, collapse = ' | ')}"
  )
  NextMethod()
  invisible(x)
}


#' Materializa um climasus_dataset como tibble preservando os metadados
#'
#' Equivalente a `dplyr::collect()` com preservacao do atributo `sus_meta`
#' e promocao da classe para `climasus_df`.
#'
#' @param x   Um `climasus_dataset`.
#' @param ... Argumentos adicionais passados para `dplyr::collect()`.
#' @return Um `climasus_df` (tibble com atributo `sus_meta`).
#' @noRd
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

# HELPER: escrita segura de um chunk em Parquet
# Sempre chamado no processo principal- sem race condition de I/O.


#' @noRd
.write_chunk_parquet <- function(df, path) {
  if (is.null(df) || nrow(df) == 0L) {
    cli::cli_alert_warning("Chunk vazio, ignorado: {basename(path)}")
    return(invisible(NULL))
  }

  # Encoding: latin1  UTF-8 antes de serializar para Parquet
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

  # tryCatch retorna flag booleana - evita que path seja retornado apos falha
  ok <- tryCatch({
    suppressMessages(suppressWarnings(
      arrow::write_parquet(
        df,
        sink              = path,
        compression       = "zstd",
        compression_level = 3L,
        write_statistics  = TRUE
      )
    ))
    TRUE
  }, error = function(e) {
    cli::cli_alert_danger("Falha ao escrever Parquet: {conditionMessage(e)}")
    FALSE
  })

  if (!ok) return(invisible(NULL))

  if (!fs::file_exists(path) || fs::file_info(path)$size == 0L) {
    cli::cli_alert_warning("Arquivo vazio apos escrita: {basename(path)}")
    return(invisible(NULL))
  }

  invisible(path)
}



# HELPER
#' @noRd
.make_cache_key <- function(system_i, uf_i, year_i, month_i, is_national) {
  parts <- if (is_national) {
    c(system_i, as.character(year_i), if (!is.null(month_i)) as.character(month_i))
  } else {
    c(system_i, uf_i, as.character(year_i), if (!is.null(month_i)) as.character(month_i))
  }
  key_string <- paste(parts, collapse = "_")
  if (rlang::is_installed("digest")) {
    digest::digest(key_string, algo = "md5")
  } else {
    gsub("[^a-zA-Z0-9]", "_", key_string)
  }
}
