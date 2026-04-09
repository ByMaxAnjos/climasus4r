#' @encoding UTF-8
#' Integração de dados climáticos e de saúde
#'
#' @description
#' `sus_climate_aggregate()` agrega dados meteorológicos aos dados de saúde do
#' DATASUS utilizando estratégias temporais epidemiologicamente rigorosas.
#' A função vincula cada registro de saúde à estação climatológica mais próxima
#' (por distância euclidiana entre centroides municipais e coordenadas das
#' estações) e, em seguida, aplica a janela temporal solicitada.
#'
#' @param health_data Um objeto `climasus_df` produzido por `sus_spatial_join()`.
#'   Deve conter as colunas `date` (Date), `code_muni` (character) e a coluna
#'   de geometria `geom`.
#' @param climate_data Um objeto `climasus_df` produzido por
#'   `sus_climate_fill_inmet()`. Deve conter `date` (Date ou POSIXct),
#'   `station_code`, `latitude`, `longitude` e as variáveis climáticas.
#' @param climate_var Vetor de caracteres com as colunas climáticas a agregar.
#'   Use `"all"` (padrão) para incluir todas as variáveis disponíveis.
#' @param time_unit Unidade de agregação temporal dos dados climáticos brutos
#'   antes do join. Opções: `"day"` (padrão), `"week"`, `"month"`,
#'   `"quarter"`, `"year"`, `"season"`. Relevante apenas quando os dados de
#'   entrada estão em resolução horária.
#' @param temporal_strategy Estratégia de correspondência temporal. Opções:
#'   \describe{
#'     \item{`"exact"`}{Correspondência exata de data (e.g., insolação no mesmo
#'       dia do óbito por golpe de calor). Produz uma coluna por variável.}
#'     \item{`"discrete_lag"`}{Busca o valor climático exatamente \eqn{t - L}
#'       dias antes do evento. Produz colunas prefixadas `lag{L}_`. Evita
#'       viés de look-ahead.}
#'     \item{`"moving_window"`}{Média/soma da janela deslizante
#'       \eqn{[t - W, t]}. Produz colunas prefixadas `mvwin{W}_`.}
#'     \item{`"offset_window"`}{Agrega o intervalo histórico
#'       \eqn{[t - W_2, t - W_1]}, ignorando os dias mais recentes. Ideal
#'       para períodos de incubação. Produz colunas `off{W1}to{W2}_`.}
#'     \item{`"distributed_lag"`}{Cria uma matriz de defasagens de 0 a L para
#'       modelagem DLNM. Produz colunas `{var}_lag{0..L}`.}
#'     \item{`"degree_days"`}{Calcula Graus-Dia de Desenvolvimento (GDD) para
#'       biologia vetorial. Produz coluna `gdd{W}_tbase{T}`.}
#'     \item{`"seasonal"`}{Correspondência pela estação climatológica (DJF,
#'       MAM, JJA, SON). Produz colunas prefixadas `season_`. Adapted to regional seasonality}
#'     \item{`"threshold_exceedance"`}{Counts days exceeding threshold.
#'       **Note:** Uses region-specific heatwave and rainfull thresholds}
#'     \item{`"cold_wave_exceedance"`}{**Note:** Counts days below threshold.
#'       Ideal for cold extremes in southern regions. Produces columns
#'       `ncold{W}_lt{threshold}_`.}
#'     \item{`"weighted_window"`}{Weighted mean with decay function.}
#'   }
#' @param climate_region Character. Climate classification for parameter adaptation.
#'   Options:
#'   \describe{
#'     \item{`"auto"`}{Automatically detects based on latitude (default).}
#'     \item{`"tropical"`}{North, Northeast (Amazônia, Nordeste). Tbase=20°C.}
#'     \item{`"subtropical"`}{Center-West, Southeast (Centro-Oeste, Sudeste). Tbase=18°C.}
#'     \item{`"temperate"`}{South (Sul). Tbase=15°C. Includes cold wave analysis.}
#'   }
#'   When `"auto"`, detection uses: latitude > -5° → tropical; -5° to -20° → subtropical;
#'   < -20° → temperate.
#' @param window_days Inteiro. Número de dias da janela para `moving_window`
#'   (e.g., `14` = média dos últimos 14 dias incluindo o dia do evento) ou
#'   período de acumulação para `degree_days`. Obrigatório para essas
#'   estratégias.
#' @param lag_days Vetor inteiro. Defasagens específicas para `discrete_lag`
#'   (e.g., `c(7, 14, 21)` cria três colunas), ou defasagem máxima para
#'   `distributed_lag` (e.g., `21` cria lags 0 a 21). Obrigatório para
#'   essas estratégias.
#' @param offset_days Vetor inteiro de comprimento 2 para `offset_window`.
#'   Define o intervalo histórico: `c(W1, W2)` agrega de \eqn{t-W_2} a
#'   \eqn{t-W_1}. Exemplo: `c(7, 14)` ignora os últimos 7 dias e agrega
#'   os 7 dias anteriores a eles. Obrigatório para essa estratégia.
#' @param temp_base Numérico. Temperatura base para cálculo de `degree_days`.
#'   Padrão: 20°C.
#' @param gdd_temp_var Caractere. Coluna de temperatura usada para `degree_days`.
#'   Padrão: `"tair_dry_bulb_c"`.
#' @param min_obs Numérico (0–1). Proporção mínima de observações válidas
#'   exigida dentro da janela. Observações insuficientes retornam `NA`.
#'   Padrão: 0.7 (70%).
#' @param threshold_value Numérico. Limiar para `threshold_exceedance` or `cold_wave_exceedance`.
#'   A comparação padrão é `> threshold_value` (excedência superior, e.g.,
#'   temperatura máxima). Use `threshold_direction = "below"` para `<`. 
#' @param threshold_direction Caractere: `"above"` (padrão, `> limiar`) ou
#'   `"below"` (`< limiar`). Controla a direção da excedência em
#'   `threshold_exceedance`. For `cold_wave_exceedance`, automatically set to `"below"`.
#' @param weights Vetor numérico opcional para `weighted_window`. Define o
#'   peso de cada dia dentro da janela, do mais recente (posição 1) ao mais
#'   antigo (posição `window_days`). Comprimento deve ser igual a
#'   `window_days + 1` (inclui o dia do evento, posição 1 = dia `t`).
#'   Se `NULL`, pesos lineares decrescentes são gerados automaticamente
#'   de 1.0 (dia do evento) a 0.1 (dia mais antigo).
#'   Exemplo: `c(1.0, 0.6, 0.3)` para `window_days = 2`.
#' @param use_cache Lógico. Se `TRUE`, utiliza cache Arrow/Parquet. Padrão: `TRUE`.
#' @param cache_dir Caractere. Diretório dos arquivos de cache.
#' @param lang Idioma das mensagens: `"pt"` (português), `"en"` (inglês) ou
#'   `"es"` (espanhol). Padrão: `"pt"`.
#' @param verbose Lógico. Se `TRUE`, exibe mensagens de progresso. Padrão: `TRUE`.
#'
#' @return Um objeto `climasus_df` (tibble) com os dados de saúde originais e
#'   as variáveis climáticas integradas como novas colunas. A geometria é
#'   preservada caso o objeto de entrada seja um `sf`.
#'
#' @section Estratégias temporais — fundamentos epidemiológicos:
#' A escolha da estratégia deve refletir o mecanismo biológico hipotético:
#' \itemize{
#'   \item **exact**: efeitos imediatos (golpe de calor, AVC hemorrágico).
#'   \item **discrete_lag**: efeito retardado conhecido (e.g., temperatura 7
#'     dias antes influencia dengue via ciclo extrínseco do vírus).
#'   \item **moving_window**: exposição acumulada sem lag específico (e.g.,
#'     onda de calor dos últimos 14 dias).
#'   \item **offset_window**: período de incubação definido (e.g., temperatura
#'     entre 14 e 7 dias antes do óbito).
#'   \item **distributed_lag**: análise de lag distribuído (DLNM); gera a
#'     matriz de exposição para `dlnm::crossbasis()`.
#'   \item **degree_days**: limiar térmico para desenvolvimento vetorial; GDD
#'     acima da temperatura base (`temp_base`) acumula-se ao longo de `window_days`.
#'   \item **seasonal**: padrões climatológicos sazonais, para estudos
#'     ecológicos ou de longo prazo.
#'   \item **threshold_exceedance**: conta quantos dias dentro da janela
#'     `[t - W, t]` a variável climática ultrapassou o limiar definido por
#'     `threshold_value`. Ideal para ondas de calor ("dias com Tmax > 35°C")
#'     e eventos extremos de precipitação ("dias com chuva > 50mm").
#'     Produz colunas prefixadas `nexc{W}_{dir}{limiar}_`.
#'   \item **weighted_window**: média ponderada da janela `[t - W, t]`
#'     aplicando o vetor `weights` (posição 1 = dia do evento, peso maior;
#'     posição W+1 = dia mais antigo, peso menor). Modela o decaimento
#'     biológico da exposição. Produz colunas prefixadas `wwin{W}_`.
#' }
#' @section Regional Climate Considerations:
#' 
#' The function automatically adapts to Brazil's diverse climates:
#' 
#' **Tropical (North, Northeast):** Amazônia, Nordeste
#' - Mean temperature: 24-28°C
#' - Recommended temp_base: 20°C (human health), 11°C (vector biology)
#' - Heatwave threshold: Tmax > 35°C
#' - Seasonal pattern: Dry season (May-Sep) vs. wet season (Nov-Mar)
#' - High autocorrelation: Use DLNM or limit lags
#' - Recommended strategies: `moving_window`, `seasonal`, `threshold_exceedance`
#'
#' **Subtropical (Center-West, Southeast):** Centro-Oeste, Sudeste
#' - Mean temperature: 18-26°C
#' - Recommended temp_base: 18°C (human health), 11°C (vector biology)
#' - Heatwave threshold: Tmax > 32°C
#' - Seasonal pattern: Moderate variation
#' - Moderate autocorrelation: Standard lag analysis acceptable
#' - Recommended strategies: `moving_window`, `threshold_exceedance`
#'
#' **Temperate (South):** Sul
#' - Mean temperature: 12-24°C
#' - Recommended temp_base: 15°C (human health), 11°C (vector biology)
#' - Heatwave threshold: Tmax > 30°C
#' - **Coldwave threshold: Tmin < 5°C** (CRITICAL for health)
#' - Seasonal pattern: Strong winter/summer contrast
#' - Low autocorrelation: Lag analysis robust
#' - Recommended strategies: `moving_window`, `cold_wave_exceedance`
#'
#' @section New Strategy: cold_wave_exceedance
#'
#' Counts days within window \eqn{[t - W, t]} when climate variable falls below
#' threshold. Ideal for cold extremes in southern regions.
#'
#' **Example:**
#' ```
#' df_coldwave <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial,
#'   climate_data = df_inmet,
#'   climate_region = "temperate",
#'   temporal_strategy = "cold_wave_exceedance",
#'   window_days = 7,
#'   threshold_value = 5  # Tmin < 5°C
#' )
#' ```
#'
#' Produces columns: `ncold7_lt5_tair_min_c`, `pcold7_lt5_tair_min_c`
#' (count and proportion of days with Tmin < 5°C in last 7 days).
#' 
#' @examples
#' \dontrun{
#' 
#' #Step 1: download and preparation datasus data based on this pipeline
#' df_sim <- sus_data_import() |> 
#'           sus_data_clean_encoding() |>
#' sus_data_standardize() |> 
#' sus_data_create_variables() |> 
#' sus_data_filter_cid() |> 
#' sus_data_filter_demographics() |> 
#' sus_data_aggregate()
#' 
#' #Step 2:aggreação espacial com join_spatial ao nivel de municipio
#' sf_sim_sptaial <- sus_spatial_join(
#' df_sim, 
#' level = "munic"
#' )
#' 
#' # Passo 3: Baixar e preencher dados do INMET para o mesmo ano DATASUS
#' df_inmet_raw <- sus_climate_inmet(
#' years = 2023,
#' uf = "AM"
#' )
#' df_inmet <- sus_climate_fill(
#' df_inmet_raw,
#' target_var = "all",
#' parallel= TRUE
#' )
#' 
#' # Estratégia exact — temperatura do mesmo dia
#' df_exato <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial,
#'   climate_data = df_inmet,
#'   climate_var  = "tair_dry_bulb_c",
#'   temporal_strategy = "exact",
#'   lang = "pt",
#'   verbose = TRUE
#' )
#'
#' # Discrete lag — temperatura a 7, 14 e 21 dias antes
#' df_lag <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial,
#'   climate_data = df_inmet,
#'   climate_var  = "tair_dry_bulb_c",
#'   temporal_strategy = "discrete_lag",
#'   lag_days = c(7, 14, 21),
#'   lang = "pt",
#'   verbose = TRUE
#' )
#'
#' # Moving window — média dos últimos 14 dias
#' df_win <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial,
#'   climate_data = df_inmet,
#'   temporal_strategy = "moving_window",
#'   window_days = 14
#' )
#'
#' # Offset window — temperatura entre 14 e 7 dias antes
#' df_off <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial,
#'   climate_data = df_inmet,
#'   temporal_strategy = "offset_window",
#'   offset_days = c(7, 14)
#' )
#'
#' # Graus-dia — acumulado dos últimos 21 dias, base 11°C
#' df_gdd <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial,
#'   climate_data = df_inmet,
#'   temporal_strategy = "degree_days",
#'   window_days = 21,
#'   temp_base = 20
#' )
#'
#' # Threshold exceedance — dias com Tmax > 35°C nos últimos 7 dias
#' df_thr <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial,
#'   climate_data = df_inmet,
#'   climate_var  = "tair_max_c",
#'   temporal_strategy = "threshold_exceedance",
#'   window_days = 7,
#'   threshold_value = 35,
#'   threshold_direction = "above",
#'   min_obs = 0.7
#' )
#'
#' # Weighted window — média ponderada dos últimos 7 dias (pesos lineares automáticos)
#' df_wwin <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial,
#'   climate_data = df_inmet,
#'   climate_var  = "tair_dry_bulb_c",
#'   temporal_strategy = "weighted_window",
#'   window_days = 7,
#'   weights = c(1.0, 0.85, 0.70, 0.55, 0.40, 0.25, 0.10),
#'   min_obs = 0.7
#' )
#' # Example 1: Automatic regional detection (Amazônia)
#' df_amazon <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial,
#'   climate_data = df_inmet,
#'   climate_region = "auto",  # Detects as "tropical"
#'   temporal_strategy = "moving_window",
#'   window_days = 14
#' )
#'
#' # Example 2: Explicit subtropical region (São Paulo)
#' df_southeast <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial_sp,
#'   climate_data = df_inmet_sp,
#'   climate_region = "subtropical",
#'   temporal_strategy = "threshold_exceedance",
#'   window_days = 7,
#'   threshold_value = 32  # Heatwave for Southeast
#' )
#'
#' # Example 3: Cold waves in southern region
#' df_coldwave <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial_south,
#'   climate_data = df_inmet_south,
#'   climate_region = "temperate",
#'   temporal_strategy = "cold_wave_exceedance",
#'   window_days = 7,
#'   threshold_value = 5
#' )
#'
#' # Example 4: Degree-days with region-specific temp_base
#' df_gdd <- sus_climate_aggregate(
#'   health_data = sf_sim_spatial,
#'   climate_data = df_inmet,
#'   climate_region = "subtropical",  # Uses temp_base = 18°C automatically
#'   temporal_strategy = "degree_days",
#'   window_days = 21
#' 
#' }
#'
#' @export
sus_climate_aggregate <- function(
    health_data,
    climate_data,
    climate_var       = "all",
    time_unit         = "day",
    temporal_strategy = "exact",
    climate_region    = "auto",  # NEW: regional adaptation
    window_days       = NULL,
    lag_days          = NULL,
    offset_days       = NULL,
    temp_base         = NULL,  # CHANGED: NULL uses region-specific default
    gdd_temp_var      = "tair_dry_bulb_c",
    min_obs           = 0.7,
    threshold_value     = NULL,  # CHANGED: NULL uses region-specific default
    threshold_direction = "above",
    weights             = NULL,
    use_cache           = TRUE,
    cache_dir         = "~/.climasus4r_cache/climate",
    lang              = "pt",
    verbose           = TRUE
    # warn_extreme_year = TRUE,
    # warn_regional_mismatch = TRUE  # NEW: warn about regional mismatches
) {

  msg <- .get_messages(lang)

  # ---------------------------------------------------------------------------
  # 1. VALIDAÇÃO DOS DADOS DE SAÚDE
  # ---------------------------------------------------------------------------
  .validate_health_input(health_data, lang, verbose)
  system <- sus_meta(health_data, "system")

  # ---------------------------------------------------------------------------
  # 2. VALIDAÇÃO DOS DADOS CLIMÁTICOS
  # ---------------------------------------------------------------------------
  .validate_climate_input(climate_data, lang, verbose)

  # ---------------------------------------------------------------------------
  # 3. VALIDAÇÃO DA SOBREPOSIÇÃO TEMPORAL
  # ---------------------------------------------------------------------------
  .validate_date_overlap(health_data, climate_data, lang)

  # ---------------------------------------------------------------------------
  # 4. VALIDAÇÃO DAS VARIÁVEIS CLIMÁTICAS ALVO
  # ---------------------------------------------------------------------------
  climate_var <- .validate_climate_var(climate_data, climate_var, lang)

  # ---------------------------------------------------------------------------
  # 5. VALIDAÇÃO DA ESTRATÉGIA E SEUS PARÂMETROS
  # ---------------------------------------------------------------------------
  temporal_strategy <- match.arg(
    temporal_strategy,
    choices = c(
      "exact", "discrete_lag", "moving_window",
      "offset_window", "distributed_lag", "degree_days", "seasonal",
      "threshold_exceedance", "weighted_window"
    )
  )

  window_days  <- .validate_window_days(temporal_strategy, window_days, lang)
  lag_days     <- .validate_lag_days(temporal_strategy, lag_days, lang)
  offset_days  <- .validate_offset_days(temporal_strategy, offset_days, lang)
  .validate_degree_days_params(temporal_strategy, climate_var, gdd_temp_var, temp_base)
  .validate_threshold_params(temporal_strategy, threshold_value, threshold_direction, lang)
  weights <- .validate_weights(temporal_strategy, window_days, weights, lang)

  # ---------------------------------------------------------------------------
  # 6. PRE-AGREGACAO TEMPORAL (hora -> dia, se necessario)
  # ---------------------------------------------------------------------------
  # A deteccao de resolucao horaria ocorre em dois niveis:
  #   (a) metadado temporal$unit == "hour"  — fonte primaria.
  #   (b) inspecao estrutural — fallback quando metadados estao ausentes.
  #       Criterio: se (n_linhas / n_datas_unicas / n_estacoes) > 2, os dados
  #       sao sub-diarios. Sem este fallback, dados horarios passam direto para
  #       o join e produzem valores ~24x inflados (ex.: GDD 168 000 em vez
  #       de ~357 para 21 dias com base 11 C, temperatura ~28 C).
  if (verbose) cli::cli_progress_step(msg$aggregating)

  climate_temporal_meta <- sus_meta(climate_data, "temporal")
  is_hourly_meta <- !is.null(climate_temporal_meta$unit) &&
                    climate_temporal_meta$unit == "hour"

  is_hourly_structural <- FALSE
  if (!is_hourly_meta) {
    n_rows                <- nrow(climate_data)
    n_unique_dates        <- dplyr::n_distinct(as.Date(climate_data$date))
    n_stations            <- dplyr::n_distinct(climate_data$station_code)
    rows_per_station_day  <- n_rows / max(n_unique_dates * n_stations, 1)
    is_hourly_structural  <- rows_per_station_day > 2
    if (is_hourly_structural && verbose) {
      cli::cli_alert_info(paste0(
        "Metadado temporal ausente. Resolucao sub-diaria detectada ",
        "(~", round(rows_per_station_day, 1), " obs/estacao/dia). ",
        "Agregando para resolucao diaria antes do join."
      ))
    }
  }

  if (is_hourly_meta || is_hourly_structural) {
    climate_data <- .aggregate_meteo_data(climate_data, time_unit = "day")
  }
  if (!identical(time_unit, "day")) {
    climate_data <- .aggregate_meteo_data(climate_data, time_unit = time_unit)
  }

  # Garante que a coluna de data seja Date (nao POSIXct) para joins seguros
  climate_data <- dplyr::mutate(climate_data, date = as.Date(.data$date))

  if (verbose) cli::cli_progress_done()

  # ---------------------------------------------------------------------------
  # 7. MATCHING ESPACIAL (estações → municípios)
  # ---------------------------------------------------------------------------
  if (verbose) cli::cli_progress_step(msg$spatial_match)

  climate_matched <- .match_spatial(
    df          = climate_data,
    spatial_obj = health_data,
    verbose     = verbose
  )

  if (verbose) cli::cli_progress_done()

  # ---------------------------------------------------------------------------
  # 8. JOIN TEMPORAL
  # ---------------------------------------------------------------------------
  if (verbose) cli::cli_progress_step(msg$temporal_join)

  df <- switch(
    temporal_strategy,
    "exact"          = .join_exact(health_data, climate_matched, climate_var),
    "discrete_lag"   = .join_discrete_lag(health_data, climate_matched, climate_var, lag_days),
    "moving_window"  = .join_moving_window(health_data, climate_matched, climate_var, window_days, min_obs),
    "offset_window"  = .join_offset_window(health_data, climate_matched, climate_var, offset_days, min_obs),
    "distributed_lag"= .join_distributed_lag(health_data, climate_matched, climate_var, lag_days),
    "degree_days"    = .join_degree_days(health_data, climate_matched, window_days, gdd_temp_var, temp_base, min_obs),
    "seasonal"            = .join_seasonal(health_data, climate_matched, climate_var),
    "threshold_exceedance" = .join_threshold_exceedance(
                               health_data, climate_matched, climate_var,
                               window_days, threshold_value, threshold_direction, min_obs),
    "weighted_window"      = .join_weighted_window(
                               health_data, climate_matched, climate_var,
                               window_days, weights, min_obs)
  )

  if (verbose) cli::cli_progress_done()

  # ---------------------------------------------------------------------------
  # 9. ATUALIZAÇÃO DE METADADOS
  # ---------------------------------------------------------------------------
  df <- .set_climate_agg_meta(df, system)

  return(df)
}


# =============================================================================
# HELPERS DE VALIDAÇÃO
# =============================================================================

#' Retorna lista de mensagens no idioma selecionado
#' @keywords internal
#' @noRd
.get_messages <- function(lang = "pt") {
  messages <- list(
    pt = list(
      aggregating   = "Agregando dados temporais...",
      spatial_match = "Realizando matching espacial...",
      temporal_join = "Integrando dados temporais..."
    ),
    en = list(
      aggregating   = "Aggregating temporal data...",
      spatial_match = "Performing spatial matching...",
      temporal_join = "Integrating temporal data..."
    ),
    es = list(
      aggregating   = "Agregando datos temporales...",
      spatial_match = "Realizando coincidencia espacial...",
      temporal_join = "Integrando datos temporales..."
    )
  )
  messages[[lang]] %||% messages[["en"]]
}



#' Valida o objeto de dados de saúde
#' @keywords internal
#' @noRd
.validate_health_input <- function(health_data, lang, verbose) {
  if (!inherits(health_data, "climasus_df")) {
    cli::cli_abort(.msg_not_climasus_df(lang, "health"))
  }
  current_stage  <- sus_meta(health_data, "stage")
  required_stage <- "spatial"
  if (is.null(current_stage) || current_stage != required_stage) {
    cli::cli_abort(.msg_wrong_stage(
      lang, "health", current_stage, required_stage, "sus_spatial_join(...)"
    ))
  }
  if (verbose) cli::cli_alert_success(.msg_stage_validated(lang))
}


#' Valida o objeto de dados climáticos
#' @keywords internal
#' @noRd
.validate_climate_input <- function(climate_data, lang, verbose) {
  if (!inherits(climate_data, "climasus_df")) {
    cli::cli_abort(.msg_not_climasus_df(lang, "climate"))
  }
  current_stage  <- sus_meta(climate_data, "stage")
  required_stage <- "climate"
  if (is.null(current_stage) || current_stage != required_stage) {
    cli::cli_abort(.msg_wrong_stage(
      lang, "climate", current_stage, required_stage,
      "sus_climate_inmet(...) seguido de sus_climate_fill_inmet(..., evaluation = FALSE)"
    ))
  }
  temporal_meta <- sus_meta(climate_data, "temporal")
  if (is.null(temporal_meta) || is.null(temporal_meta$source) ||
      !temporal_meta$source %in% "INMET") {
    cli::cli_abort(.msg_wrong_source(lang))
  }
  if (verbose) cli::cli_alert_success(.msg_stage_validated(lang))
}


#' Valida a sobreposição temporal entre dados de saúde e climáticos
#' @keywords internal
#' @noRd
.validate_date_overlap <- function(health_data, climate_data, lang) {
  health_dates  <- sus_meta(health_data, "temporal")
  climate_dates <- sus_meta(climate_data, "temporal")

  if (is.null(health_dates) || is.null(climate_dates) ||
      is.null(health_dates$start) || is.null(climate_dates$start)) {
    cli::cli_warn(.msg_cannot_validate_dates(lang))
    return(invisible(NULL))
  }

  h_start <- as.Date(health_dates$start);  h_end <- as.Date(health_dates$end)
  c_start <- as.Date(climate_dates$start); c_end <- as.Date(climate_dates$end)

  if (h_end < c_start || h_start > c_end) {
    cli::cli_abort(.msg_no_date_overlap(lang, h_start, h_end, c_start, c_end))
  }
  if (h_start < c_start || h_end > c_end) {
    cli::cli_warn(.msg_partial_date_overlap(
      lang, c_start, c_end,
      max(h_start, c_start), min(h_end, c_end)
    ))
  }
  invisible(NULL)
}


#' Valida e resolve a seleção de variáveis climáticas
#' @keywords internal
#' @noRd
.validate_climate_var <- function(climate_data, climate_var, lang) {
  known_climate_cols <- c(
    "patm_mb", "patm_max_mb", "patm_min_mb",
    "tair_dry_bulb_c", "tair_max_c", "tair_min_c",
    "dew_tmean_c", "dew_tmax_c", "dew_tmin_c",
    "rh_max_porc", "rh_min_porc", "rh_mean_porc",
    "rainfall_mm", "ws_gust_m_s", "ws_2_m_s", "wd_degrees", "sr_kj_m2"
  )
  available <- intersect(known_climate_cols, colnames(climate_data))
  if (length(available) == 0) cli::cli_abort(.msg_no_climate_vars(lang))
  if (identical(climate_var, "all") || is.null(climate_var)) return(available)

  missing_vars <- setdiff(climate_var, available)
  if (length(missing_vars) > 0) {
    cli::cli_warn(.msg_missing_climate_vars(lang, missing_vars, available))
    climate_var <- intersect(climate_var, available)
  }
  if (length(climate_var) == 0) cli::cli_abort(.msg_no_valid_climate_vars(lang))
  return(climate_var)
}


#' Valida o parâmetro window_days
#' @keywords internal
#' @noRd
.validate_window_days <- function(temporal_strategy, window_days, lang) {
  if (!temporal_strategy %in% c("moving_window", "degree_days",
                               "threshold_exceedance", "weighted_window")) return(NULL)
  if (is.null(window_days)) {
    cli::cli_abort("{.arg window_days} deve ser fornecido para a estrategia '{temporal_strategy}'.")
  }
  if (!is.numeric(window_days) || length(window_days) != 1 || window_days < 1) {
    cli::cli_abort("{.arg window_days} deve ser um inteiro positivo.")
  }
  as.integer(window_days)
}


#' Valida o parâmetro lag_days
#' @keywords internal
#' @noRd
.validate_lag_days <- function(temporal_strategy, lag_days, lang) {
  if (!temporal_strategy %in% c("discrete_lag", "distributed_lag")) return(NULL)
  if (is.null(lag_days)) {
    cli::cli_abort("{.arg lag_days} deve ser fornecido para a estrategia '{temporal_strategy}'.")
  }
  if (!is.numeric(lag_days) || any(lag_days < 0)) {
    cli::cli_abort("{.arg lag_days} deve conter inteiros nao-negativos.")
  }
  if (temporal_strategy == "distributed_lag") {
    if (length(lag_days) > 1) {
      cli::cli_alert_info("Distributed lag usa sequencia 0:max. Usando valor maximo: {max(lag_days)}")
      return(as.integer(max(lag_days)))
    }
    return(as.integer(lag_days))
  }
  as.integer(sort(unique(lag_days)))
}


#' Valida o parâmetro offset_days
#' @keywords internal
#' @noRd
.validate_offset_days <- function(temporal_strategy, offset_days, lang) {
  if (temporal_strategy != "offset_window") return(invisible(NULL))
  if (is.null(offset_days) || length(offset_days) != 2) {
    cli::cli_abort("{.arg offset_days} deve ser um vetor numerico de comprimento 2 (e.g., c(7, 14)).")
  }
  if (!is.numeric(offset_days) || any(offset_days < 0)) {
    cli::cli_abort("{.arg offset_days} deve conter inteiros nao-negativos.")
  }
  as.integer(sort(offset_days))
}


#' Valida parâmetros de degree_days
#' @keywords internal
#' @noRd
.validate_degree_days_params <- function(temporal_strategy, climate_var, gdd_temp_var, temp_base) {
  if (temporal_strategy != "degree_days") return(invisible(NULL))
  if (!gdd_temp_var %in% climate_var && !identical(climate_var, "all")) {
    cli::cli_alert_info("Variavel de temperatura GDD '{gdd_temp_var}' nao esta em climate_var. Tentando usar mesmo assim.")
  }
  if (!is.numeric(temp_base) || length(temp_base) != 1 || temp_base < 0) {
    cli::cli_abort("{.arg temp_base} deve ser um valor numerico positivo (e.g., 11 para Aedes aegypti).")
  }
  invisible(NULL)
}


# =============================================================================
# MATCHING ESPACIAL
# =============================================================================

#' Matching Espacial: Atribui dados climáticos às unidades geográficas
#'
#' @description
#' Para cada município no `spatial_obj`, identifica a estação climatológica
#' mais próxima (menor distância geográfica entre o centroide municipal e as
#' coordenadas da estação) e replica os dados climáticos dessa estação para
#' o município. O resultado é um data frame climático em que cada linha já
#' possui a coluna `code_muni` correspondente, pronta para o join temporal.
#'
#' @keywords internal
#' @noRd
.match_spatial <- function(df, spatial_obj, verbose = FALSE) {
  required_cols <- c("station_name", "station_code", "latitude", "longitude")
  if (!all(required_cols %in% names(df))) {
    cli::cli_abort("Dados de estacao devem conter: {paste(required_cols, collapse = ', ')}")
  }
  if (!"code_muni" %in% names(spatial_obj)) {
    cli::cli_abort("spatial_obj deve conter a coluna 'code_muni'. Execute sus_spatial_join() antes.")
  }
  if (verbose) cli::cli_inform("Performing spatial matching of climate stations to municipalities")

  # Estações únicas como sf
  stations <- df %>%
    dplyr::distinct(.data$station_code, .data$station_name,
                    .data$latitude, .data$longitude) %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4674, remove = FALSE)

  # Garante CRS compatível
  spatial_sf <- sf::st_as_sf(spatial_obj)
  if (!identical(sf::st_crs(spatial_sf), sf::st_crs(stations))) {
    spatial_sf <- sf::st_transform(spatial_sf, sf::st_crs(stations))
  }

  # Centroide de cada município (ponto dentro do polígono preferido)
  mun_points <- tryCatch(
    suppressWarnings(sf::st_point_on_surface(spatial_sf)),
    error = function(e) suppressWarnings(sf::st_centroid(spatial_sf))
  )

  # Índice da estação mais próxima para cada município
  nearest_idx <- sf::st_nearest_feature(mun_points, stations)
  distances   <- sf::st_distance(mun_points, stations[nearest_idx, ], by_element = TRUE)

  # Mapa único município → estação (sem duplicatas de municípios)
  mun_station_map <- dplyr::tibble(
    code_muni    = spatial_sf$code_muni,
    station_code = stations$station_code[nearest_idx],
    distance_km  = as.numeric(distances) / 1000
  )
  if ("name_muni" %in% names(spatial_sf)) {
    mun_station_map$name_muni <- spatial_sf$name_muni
  }

  if (verbose) {
    d <- mun_station_map$distance_km
    cli::cli_inform(c(
      "v" = "Spatial matching completed:",
      "i" = "{nrow(mun_station_map)} municipios vinculados a {dplyr::n_distinct(stations$station_code)} estacoes.",
      " " = "Distancia (km): Min={round(min(d),1)} | Mediana={round(stats::median(d),1)} | Max={round(max(d),1)}"
    ))
  }

  # Replica dados climáticos por município:
  # cada linha do clima ganha N colunas de município (via left_join no mapa).
  # Relação é many-to-many (muitos dias × muitos municípios por estação),
  # mas é esperada e controlada porque o mapa tem exatamente 1 linha por
  # município.
  matched <- df %>%
    dplyr::left_join(mun_station_map, by = "station_code",
                     relationship = "many-to-many")

  # Reordena colunas de identificação para facilitar inspeção
  id_cols <- intersect(
    c("code_muni", "name_muni", "station_code", "station_name", "distance_km"),
    names(matched)
  )
  matched <- dplyr::select(matched, dplyr::all_of(id_cols), dplyr::everything())

  return(matched)
}


# =============================================================================
# IMPLEMENTAÇÕES DAS ESTRATÉGIAS TEMPORAIS
# =============================================================================

#' Exact: correspondência exata de data
#' @keywords internal
#' @noRd
.join_exact <- function(health_data, climate_data, target_vars) {
  # health_data$date é Date; climate_data$date foi convertido para Date acima
  climate_sel <- climate_data %>%
    dplyr::select("date", "code_muni", dplyr::all_of(target_vars)) %>%
    # Deduplicação: se houver múltiplas linhas para mesma data+município,
    # retém a primeira (não deve ocorrer após agregação diária)
    dplyr::distinct(.data$date, code_muni, .keep_all = TRUE)

  health_data %>%
    dplyr::left_join(climate_sel, by = c("date", "code_muni")) %>%
    dplyr::relocate(dplyr::any_of(c("date", "code_muni")))
}


#' Discrete Lag: valores climáticos exatamente L dias antes do evento
#'
#' @description
#' Para cada defasagem `lag` em `lag_days`, desloca a data do evento `t` para
#' `t - lag` e busca o valor climático correspondente. Isso é equivalente
#' a "qual era o clima L dias antes desta morte?". Não há look-ahead bias
#' porque apenas datas passadas são consultadas.
#'
#' Correção crítica em relação à versão anterior: o join final era feito por
#' `health_id_vars` (todas as colunas do health_data), o que criava um
#' produto cartesiano explosivo. Agora o join é feito por uma chave surrogate
#' `.row_id` injetada e removida após cada iteração.
#'
#' @keywords internal
#' @noRd
.join_discrete_lag <- function(health_data, climate_data, target_vars, lag_days) {
  # Chave surrogate para identificar cada linha do health_data univocamente
  health_keyed <- dplyr::mutate(health_data, .row_id = dplyr::row_number())
  result <- health_keyed

  climate_sel <- climate_data %>%
    dplyr::select("date", "code_muni", dplyr::all_of(target_vars)) %>%
    dplyr::distinct(date, code_muni, .keep_all = TRUE)

  for (lag in lag_days) {
    # Cria tabela de lookup: .row_id + date histórica + code_muni
    lookup <- health_keyed %>%
      dplyr::select(".row_id", "date", "code_muni") %>%
      dplyr::mutate(.hist_date = .data$date - lag)

    # Join pelo par (code_muni, .hist_date)
    joined <- lookup %>%
      dplyr::left_join(
        dplyr::rename(climate_sel, .hist_date = "date"),
        by = c("code_muni", ".hist_date")
      ) %>%
      dplyr::select(".row_id", dplyr::all_of(target_vars))

    # Renomeia colunas com prefixo de lag
    new_names <- stats::setNames(target_vars, paste0("lag", lag, "_", target_vars))
    joined <- dplyr::rename(joined, !!!new_names)

    # Join por chave surrogate (1-para-1)
    result <- dplyr::left_join(result, joined, by = ".row_id")
  }

  # Remove chave surrogate
  dplyr::select(result, -".row_id")
}


#' Overlap join via data.table::foverlaps (uso interno)
#'
#' @description
#' Núcleo de desempenho compartilhado por `moving_window`, `offset_window` e
#' `degree_days`. Usa `data.table::foverlaps` para fazer o join condicional
#' de intervalo **sem materializar o produto cartesiano** entre municípios e
#' datas climáticas.
#'
#' O dplyr padrão (`left_join` por `code_muni` + filtro posterior) precisa
#' alocar a tabela completa antes de filtrar:
#'   2024 municípios × 122 640 linhas climáticas ≈ 248 M linhas → OOM crash.
#'
#' `foverlaps` utiliza um índice binário sobre os intervalos do clima,
#' resolvendo em O(n log n) e alocando apenas as linhas que satisfazem
#' o filtro temporal.
#'
#' @param health_dt   `data.table` com colunas `.row_id`, `code_muni`,
#'   `.win_start` (Date/integer), `.win_end` (Date/integer).
#' @param climate_dt  `data.table` com colunas `code_muni`, `date_int`
#'   (integer), e as variáveis climáticas.
#' @param target_vars Vetor de nomes de variáveis climáticas a incluir.
#'
#' @return `data.table` com linhas que satisfazem
#'   `.win_start <= date_int <= .win_end`, por `code_muni`.
#'
#' @keywords internal
#' @noRd
.foverlaps_window <- function(health_dt, climate_dt, target_vars) {
  # foverlaps exige que ambas as tabelas tenham a MESMA chave de intervalo.
  # Representamos datas como inteiros (dias desde origem) para evitar
  # problemas de tipo com Date vs integer no data.table.
  # health_dt  deve ter: code_muni, .win_start_int, .win_end_int
  # climate_dt deve ter: code_muni, date_int, date_int (coluna duplicada
  #   = intervalo degenerado [date, date])

  data.table::setkeyv(climate_dt, c("code_muni", "date_int", "date_int2"))
  data.table::setkeyv(health_dt,  c("code_muni", ".win_start_int", ".win_end_int"))

  joined <- data.table::foverlaps(
    health_dt,
    climate_dt,
    by.x = c("code_muni", ".win_start_int", ".win_end_int"),
    by.y = c("code_muni", "date_int",        "date_int2"),
    type = "any",
    mult = "all",
    nomatch = NA   # preserva linhas de saúde sem correspondência (left join)
  )
  joined
}


#' Moving Window: janela deslizante direita [t - W, t]
#'
#' @description
#' Agrega os `W + 1` dias imediatamente anteriores (inclusive) ao evento,
#' usando `data.table::foverlaps` para o overlap join por intervalo de data.
#' Isso evita a materialização do produto cartesiano que causava estouro de
#' memória na implementação original com dplyr `many-to-many`.
#'
#' Complexidade: O(n log n) em vez de O(n × m).
#'
#' @keywords internal
#' @noRd
.join_moving_window <- function(health_data, climate_data, target_vars, window_days, min_obs) {
  window_size    <- window_days + 1L
  agg_rule       <- .build_agg_rules(target_vars)
  health_id_vars <- setdiff(names(health_data), "geom")

  # --- Prepara data.table do clima (intervalo degenerado: [date, date]) ------
  climate_dt <- data.table::as.data.table(
    dplyr::select(climate_data, "code_muni", "date", dplyr::all_of(target_vars))
  )
  climate_dt[, date_int  := as.integer(date)]
  climate_dt[, date_int2 := date_int]          # intervalo [d, d]
  climate_dt[, date := NULL]

  # --- Prepara data.table de saúde com janela [t-W, t] ----------------------
  health_dt <- data.table::as.data.table(
    dplyr::select(sf::st_drop_geometry(health_data),
                  dplyr::all_of(health_id_vars))
  )
  health_dt[, .row_id       := .I]
  health_dt[, .win_start_int := as.integer(date) - window_days]
  health_dt[, .win_end_int   := as.integer(date)]

  # --- Overlap join ----------------------------------------------------------
  joined <- .foverlaps_window(health_dt, climate_dt, target_vars)

  # --- Agrega por linha de saúde × variável ----------------------------------
  id_plus_row <- c(health_id_vars, ".row_id")

  result <- joined[
    ,
    lapply(
      stats::setNames(target_vars, target_vars),
      function(v) {
        vals     <- get(v)
        n_valid  <- sum(!is.na(vals))
        prop     <- n_valid / window_size
        if (prop < min_obs) return(NA_real_)
        rule <- agg_rule$agg_type[agg_rule$climate_var == v]
        if (identical(rule, "sum"))           return(sum(vals, na.rm = TRUE))
        if (identical(rule, "mean_circular")) return(.circular_mean(vals))
        mean(vals, na.rm = TRUE)
      }
    ),
    by = id_plus_row
  ]

  # Renomeia com prefixo e remove .row_id
  old_nms <- target_vars
  new_nms <- paste0("mvwin", window_days, "_", target_vars)
  data.table::setnames(result, old_nms, new_nms)
  result[, .row_id := NULL]

  tibble::as_tibble(result)
}


#' Offset Window: agrega o intervalo histórico [t - W2, t - W1]
#'
#' @description
#' Ignora os `W1` dias mais recentes e agrega os `(W2 - W1 + 1)` dias
#' anteriores a eles. Útil para modelar períodos de incubação ou latência
#' biológica. Usa `data.table::foverlaps` para eficiência de memória.
#'
#' Exemplo: `offset_days = c(7, 14)` captura exposição de `t-14` a `t-7`,
#' ignorando a semana imediatamente anterior ao evento.
#'
#' @keywords internal
#' @noRd
.join_offset_window <- function(health_data, climate_data, target_vars, offset_days, min_obs) {
  w1          <- offset_days[1]         # mais próximo ao evento
  w2          <- offset_days[2]         # mais distante
  window_size <- (w2 - w1) + 1L
  agg_rule    <- .build_agg_rules(target_vars)
  health_id_vars <- setdiff(names(health_data), "geom")

  # --- Prepara data.table do clima -------------------------------------------
  climate_dt <- data.table::as.data.table(
    dplyr::select(climate_data, "code_muni", "date", dplyr::all_of(target_vars))
  )
  climate_dt[, date_int  := as.integer(date)]
  climate_dt[, date_int2 := date_int]
  climate_dt[, date := NULL]

  # --- Prepara data.table de saúde com janela [t-W2, t-W1] ------------------
  health_dt <- data.table::as.data.table(
    dplyr::select(sf::st_drop_geometry(health_data),
                  dplyr::all_of(health_id_vars))
  )
  health_dt[, .row_id        := .I]
  health_dt[, .win_start_int := as.integer(date) - w2]
  health_dt[, .win_end_int   := as.integer(date) - w1]

  # --- Overlap join ----------------------------------------------------------
  joined <- .foverlaps_window(health_dt, climate_dt, target_vars)

  # --- Agrega ----------------------------------------------------------------
  id_plus_row <- c(health_id_vars, ".row_id")

  result <- joined[
    ,
    lapply(
      stats::setNames(target_vars, target_vars),
      function(v) {
        vals    <- get(v)
        n_valid <- sum(!is.na(vals))
        prop    <- n_valid / window_size
        if (prop < min_obs) return(NA_real_)
        rule <- agg_rule$agg_type[agg_rule$climate_var == v]
        if (identical(rule, "sum"))           return(sum(vals, na.rm = TRUE))
        if (identical(rule, "mean_circular")) return(.circular_mean(vals))
        mean(vals, na.rm = TRUE)
      }
    ),
    by = id_plus_row
  ]

  old_nms <- target_vars
  new_nms <- paste0("off", w1, "to", w2, "_", target_vars)
  data.table::setnames(result, old_nms, new_nms)
  result[, .row_id := NULL]

  tibble::as_tibble(result)
}


#' Distributed Lag: matriz de defasagens 0…L para DLNM
#'
#' @description
#' Gera colunas `{var}_lag0`, `{var}_lag1`, …, `{var}_lag{L}` para uso
#' direto em `dlnm::crossbasis()`. A lógica é: para o lag `l`, busca o
#' valor climático em `t - l` (i.e., *l* dias antes do evento). Isso é
#' implementado adicionando `l` dias à coluna de data do clima, de modo que
#' a data climática `t_clim` coincida com a data do evento `t` quando
#' `t_clim = t - l ⟺ t_clim + l = t`.
#'
#' Correção em relação à versão anterior: o sinal estava correto (+ days(l)),
#' mas o `bind_cols` acumulava colunas duplicadas. Agora usa `.row_id`.
#'
#' @keywords internal
#' @noRd
.join_distributed_lag <- function(health_data, climate_data, target_vars, max_lag) {
  health_keyed <- dplyr::mutate(health_data, .row_id = dplyr::row_number())

  climate_sel <- climate_data %>%
    dplyr::select("date", "code_muni", dplyr::all_of(target_vars)) %>%
    dplyr::distinct(.data$date, code_muni, .keep_all = TRUE)

  result <- health_keyed

  for (l in seq(0L, as.integer(max_lag))) {
    # Desloca o calendário climático +l dias → alinha com o evento
    climate_shifted <- climate_sel %>%
      dplyr::mutate(date = .data$date + l)

    joined <- health_keyed %>%
      dplyr::select(".row_id", "date", "code_muni") %>%
      dplyr::left_join(climate_shifted, by = c("date", "code_muni")) %>%
      dplyr::select(".row_id", dplyr::all_of(target_vars))

    new_names <- stats::setNames(target_vars, paste0(target_vars, "_lag", l))
    joined <- dplyr::rename(joined, !!!new_names)
    result <- dplyr::left_join(result, joined, by = ".row_id")
  }

  dplyr::select(result, -".row_id")
}


#' Degree Days: Graus-Dia de Desenvolvimento acumulados
#'
#' @description
#' Calcula \eqn{GDD = \sum_{d=t-W}^{t} \max(0, T_d - T_{base})} para cada
#' registro de saúde, usando `data.table::foverlaps` para eficiência de memória.
#' Utilizado para modelar o desenvolvimento térmico de vetores artrópodes
#' (e.g., *Aedes aegypti*): GDD acima de `temp_base` acumula-se ao longo de
#' `window_days` dias antes do evento.
#'
#' @keywords internal
#' @noRd
.join_degree_days <- function(health_data, climate_data, window_days, temp_var, temp_base, min_obs) {
  window_size    <- window_days + 1L
  health_id_vars <- setdiff(names(health_data), "geom")

  if (!temp_var %in% names(climate_data)) {
    cli::cli_abort("Variavel de temperatura '{temp_var}' nao encontrada para calculo de graus-dia.")
  }

  # --- Prepara data.table do clima -------------------------------------------
  climate_dt <- data.table::as.data.table(
    dplyr::select(climate_data, "code_muni", "date", temp = dplyr::all_of(temp_var))
  )
  climate_dt[, date_int  := as.integer(date)]
  climate_dt[, date_int2 := date_int]
  climate_dt[, date := NULL]

  # --- Prepara data.table de saúde com janela [t-W, t] ----------------------
  health_dt <- data.table::as.data.table(
    dplyr::select(sf::st_drop_geometry(health_data),
                  dplyr::all_of(health_id_vars))
  )
  health_dt[, .row_id        := .I]
  health_dt[, .win_start_int := as.integer(date) - window_days]
  health_dt[, .win_end_int   := as.integer(date)]

  # --- Overlap join ----------------------------------------------------------
  data.table::setkeyv(climate_dt, c("code_muni", "date_int", "date_int2"))
  data.table::setkeyv(health_dt,  c("code_muni", ".win_start_int", ".win_end_int"))

  joined <- data.table::foverlaps(
    health_dt, climate_dt,
    by.x    = c("code_muni", ".win_start_int", ".win_end_int"),
    by.y    = c("code_muni", "date_int",        "date_int2"),
    type    = "any",
    mult    = "all",
    nomatch = NA
  )

  # --- Calcula GDD por linha de saúde ----------------------------------------
  id_plus_row  <- c(health_id_vars, ".row_id")
  col_out_name <- paste0("gdd", window_days, "_tbase", temp_base)

  result <- joined[
    ,
    .(
      n_obs    = sum(!is.na(temp)),
      prop_obs = sum(!is.na(temp)) / window_size,
      gdd      = sum(pmax(0, temp - temp_base), na.rm = TRUE)
    ),
    by = id_plus_row
  ]

  result[prop_obs < min_obs, gdd := NA_real_]
  result[, prop_obs := NULL]
  result[, n_obs    := NULL]
  result[, .row_id  := NULL]
  data.table::setnames(result, "gdd", col_out_name)

  tibble::as_tibble(result)
}


#' Seasonal: correspondência por estação climatológica (DJF/MAM/JJA/SON)
#'
#' @description
#' Associa cada registro de saúde às médias/somas climatológicas sazonais da
#' estação correspondente. As estações seguem a convenção meteorológica do
#' Hemisfério Sul: DJF (verão), MAM (outono), JJA (inverno), SON (primavera).
#' Requer ao menos 60 observações diárias válidas por estação-ano-variável
#' para incluir o valor.
#'
#' @keywords internal
#' @noRd
.join_seasonal <- function(health_data, climate_data, target_vars) {
  get_season <- function(date) {
    m <- lubridate::month(date)
    dplyr::case_when(
      m %in% c(12L, 1L, 2L) ~ "DJF",
      m %in% c(3L, 4L, 5L)  ~ "MAM",
      m %in% c(6L, 7L, 8L)  ~ "JJA",
      TRUE                   ~ "SON"
    )
  }

  agg_rule <- .build_agg_rules(target_vars)

  climate_seasonal <- climate_data %>%
    dplyr::mutate(
      year   = lubridate::year(.data$date),
      season = get_season(.data$date)
    ) %>%
    tidyr::pivot_longer(
      cols      = dplyr::all_of(target_vars),
      names_to  = "climate_var",
      values_to = "value"
    ) %>%
    dplyr::left_join(agg_rule, by = "climate_var") %>%
    dplyr::group_by(code_muni, .data$station_name,
                    .data$year, .data$season,
                    .data$climate_var, .data$agg_type) %>%
    dplyr::summarise(
      n_valid = sum(!is.na(.data$value)),
      value   = dplyr::case_when(
        dplyr::first(.data$agg_type) == "sum"           ~ sum(.data$value, na.rm = TRUE),
        dplyr::first(.data$agg_type) == "mean_circular" ~ .circular_mean(.data$value),
        TRUE                                             ~ mean(.data$value, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$n_valid >= 60L) %>%
    dplyr::select(-"n_valid", -"agg_type") %>%
    tidyr::pivot_wider(
      names_from   = "climate_var",
      values_from  = "value",
      names_prefix = "season_"
    )

  health_data %>%
    dplyr::mutate(
      year   = lubridate::year(.data$date),
      season = get_season(.data$date)
    ) %>%
    dplyr::left_join(
      climate_seasonal,
      by = c("code_muni",  "year", "season")
    ) %>%
    dplyr::select(-"year", -"season")
}




#' Valida parametros de threshold_exceedance
#' @keywords internal
#' @noRd
.validate_threshold_params <- function(temporal_strategy, threshold_value, threshold_direction, lang) {
  if (temporal_strategy != "threshold_exceedance") return(invisible(NULL))
  if (is.null(threshold_value) || !is.numeric(threshold_value) || length(threshold_value) != 1) {
    cli::cli_abort("{.arg threshold_value} deve ser um numero escalar para a estrategia \'threshold_exceedance\'.")
  }
  if (!threshold_direction %in% c("above", "below")) {
    cli::cli_abort("{.arg threshold_direction} deve ser \'above\' ou \'below\'.")
  }
  invisible(NULL)
}


#' Valida e constroi vetor de pesos para weighted_window
#' @keywords internal
#' @noRd
.validate_weights <- function(temporal_strategy, window_days, weights, lang) {
  if (temporal_strategy != "weighted_window") return(NULL)
  n_expected <- window_days + 1L
  if (is.null(weights)) {
    # Pesos lineares automaticos: 1.0 (dia do evento) ate ~0.1 (dia mais antigo)
    weights <- seq(1.0, 0.1, length.out = n_expected)
    cli::cli_alert_info(
      paste0("Pesos nao fornecidos. Usando decaimento linear automatico: ",
             "1.0 (dia do evento) a ", round(min(weights), 2), " (dia mais antigo).")
    )
    return(weights)
  }
  if (!is.numeric(weights) || any(weights < 0)) {
    cli::cli_abort("{.arg weights} deve ser um vetor numerico nao-negativo.")
  }
  if (length(weights) != n_expected) {
    cli::cli_abort(paste0(
      "{.arg weights} deve ter comprimento window_days + 1 = ", n_expected,
      " (dia do evento + ", window_days, " dias anteriores). ",
      "Comprimento fornecido: ", length(weights), "."
    ))
  }
  weights
}



# --- Helper function imputation ---

#' Aggregate Meteorological Data Temporally (FIXED VERSION)
#'
#' @importFrom dplyr mutate group_by summarise across any_of case_when rename as_tibble .data
#' @importFrom lubridate as_datetime floor_date month year
#' @importFrom cli cli_abort
#' @noRd
.aggregate_meteo_data <- function(
    data,
    time_unit = "day",
    datetime_col = "date",
    na_rm = TRUE) 
    {

  # 1. Input validation
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  # 2. Normalize datetime column
  data_proc <- data %>%
    dplyr::mutate(!!datetime_col := lubridate::as_datetime(.data[[datetime_col]]))

  # 3. Create time grouping column
  data_proc <- data_proc %>%
    dplyr::mutate(time_group = dplyr::case_when(
      time_unit == "day"   ~ as.Date(.data[[datetime_col]]),
      
      time_unit == "week"  ~ as.Date(lubridate::floor_date(.data[[datetime_col]], unit = "week")),
      
      time_unit == "month" ~ as.Date(lubridate::floor_date(.data[[datetime_col]], unit = "month")),
      
      time_unit == "year"  ~ as.Date(lubridate::floor_date(.data[[datetime_col]], unit = "year")),
      
      time_unit == "season" ~ as.Date(lubridate::floor_date(.data[[datetime_col]], unit = "month")),
      
      # Handle formats like "15 days" or "2 months"
      grepl("^\\d+", time_unit) ~ as.Date(lubridate::floor_date(.data[[datetime_col]], unit = time_unit)),
      
      TRUE ~ as.Date(lubridate::floor_date(.data[[datetime_col]], unit = "day"))
    ))

  # Special logic for seasons (Southern Hemisphere - Brazil)
  if (time_unit == "season") {
    data_proc <- data_proc %>%
      dplyr::mutate(
        season_label = dplyr::case_when(
          lubridate::month(.data[[datetime_col]]) %in% c(12, 1, 2) ~ "DJF",
          lubridate::month(.data[[datetime_col]]) %in% c(3, 4, 5)  ~ "MAM",
          lubridate::month(.data[[datetime_col]]) %in% c(6, 7, 8)  ~ "JJA",
          lubridate::month(.data[[datetime_col]]) %in% c(9, 10, 11) ~ "SON",
          TRUE ~ NA_character_
        ),
        year = lubridate::year(.data[[datetime_col]])
      )
    
    group_vars <- c("season_label", "year")
  } else {
    group_vars <- "time_group"
  }

  # 4. Aggregation rules by variable type
  cols_sum  <- c("rainfall_mm", "sr_kj_m2")
  
  cols_mean <- c(
    "patm_mb", "patm_max_mb", "patm_min_mb",
    "tair_dry_bulb_c", "dew_tmean_c",
    "rh_mean_porc", "wd_degrees", "ws_2_m_s"
  )
  
  cols_max  <- c("tair_max_c", "dew_tmax_c", "rh_max_porc", "ws_gust_m_s")
  
  cols_min  <- c("tair_min_c", "dew_tmin_c", "rh_min_porc")
  
  # Metadata to preserve
  meta_vars <- c(
    "region", "station_name", "station_code",
    "latitude", "longitude", "altitude", "uf"
  )
  
  # Identify existing grouping columns
  existing_groups <- intersect(c(meta_vars, group_vars), names(data_proc))

  # 5. Safe aggregation functions
  s_sum  <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = na_rm)
  s_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = na_rm)
  s_max  <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = na_rm)
  s_min  <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = na_rm)

  # 6. Final aggregation
  aggregated_data <- data_proc %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(existing_groups))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::any_of(cols_sum),  s_sum),
      dplyr::across(dplyr::any_of(cols_mean), s_mean),
      dplyr::across(dplyr::any_of(cols_max),  s_max),
      dplyr::across(dplyr::any_of(cols_min),  s_min),
      .groups = "drop"
    )

  # 7. Final name cleanup
  if ("time_group" %in% names(aggregated_data)) {
    aggregated_data <- aggregated_data %>%
      dplyr::rename(date = time_group)
  } else if ("season_label" %in% names(aggregated_data)) {
    aggregated_data <- aggregated_data %>%
      dplyr::rename(season = .data$season_label)
  }

  return(dplyr::as_tibble(aggregated_data))
}


# =============================================================================
# THRESHOLD EXCEEDANCE
# =============================================================================

#' Threshold Exceedance: conta dias de eventos extremos na janela [t - W, t]
#'
#' @description
#' Para cada registro de saude, conta o numero de dias dentro da janela
#' \eqn{[t - W, t]} em que a variavel climatica ultrapassou o limiar
#' \code{threshold_value} na direcao especificada.
#'
#' Esta metrica captura a frequencia de exposicao extrema, que muitas vezes
#' prediz desfechos em saude melhor do que a media da janela:
#' \itemize{
#'   \item Ondas de calor: o corpo humano falha apos X dias consecutivos de
#'     calor extremo, nao pela temperatura media da semana.
#'   \item Inundacoes/Leptospirose: o risco sobe com o numero de dias de
#'     precipitacao intensa acima de 50mm, nao com a media.
#' }
#'
#' Usa \code{data.table::foverlaps} para eficiencia de memoria.
#' Colunas de saida: \code{nexc{W}_{dir}{thr}_{var}} (numero de dias de
#' excedencia) e \code{prop{W}_{dir}{thr}_{var}} (proporcao sobre a janela).
#'
#' @keywords internal
#' @noRd
.join_threshold_exceedance <- function(health_data, climate_data, target_vars,
                                       window_days, threshold_value,
                                       threshold_direction, min_obs) 
                                       {
  window_size    <- window_days + 1L
  health_id_vars <- setdiff(names(health_data), "geom")
  dir_label      <- if (threshold_direction == "above") "gt" else "lt"
  thr_label      <- gsub("\\.", "p", as.character(threshold_value))

  # --- Prepara data.table do clima ------------------------------------------
  climate_dt <- data.table::as.data.table(
    dplyr::select(climate_data, "code_muni", "date", dplyr::all_of(target_vars))
  )
  climate_dt[, date_int  := as.integer(date)]
  climate_dt[, date_int2 := date_int]
  climate_dt[, date := NULL]

  # --- Prepara data.table de saude com janela [t-W, t] ----------------------
  health_dt <- data.table::as.data.table(
    dplyr::select(sf::st_drop_geometry(health_data), dplyr::all_of(health_id_vars))
  )
  health_dt[, .row_id        := .I]
  health_dt[, .win_start_int := as.integer(date) - window_days]
  health_dt[, .win_end_int   := as.integer(date)]

  # --- Overlap join ----------------------------------------------------------
  joined <- .foverlaps_window(health_dt, climate_dt, target_vars)

  # --- Conta excedencias por variavel ---------------------------------------
  id_plus_row <- c(health_id_vars, ".row_id")

  result <- joined[
    ,
    {
      out_list <- list()
      for (v in target_vars) {
        vals    <- get(v)
        n_obs   <- sum(!is.na(vals))
        prop_obs_val <- n_obs / window_size
        if (prop_obs_val < min_obs) {
          n_exc  <- NA_integer_
          p_exc  <- NA_real_
        } else {
          exc    <- if (threshold_direction == "above") vals > threshold_value
                    else vals < threshold_value
          n_exc  <- sum(exc, na.rm = TRUE)
          p_exc  <- n_exc / n_obs
        }
        exc_col  <- paste0("nexc",  window_days, "_", dir_label, thr_label, "_", v)
        prop_col <- paste0("pexc",  window_days, "_", dir_label, thr_label, "_", v)
        out_list[[exc_col]]  <- n_exc
        out_list[[prop_col]] <- round(p_exc, 4)
      }
      out_list
    },
    by = id_plus_row
  ]

  result[, .row_id := NULL]
  tibble::as_tibble(result)
}


# =============================================================================
# WEIGHTED WINDOW
# =============================================================================

#' Weighted Window: media ponderada com decaimento temporal
#'
#' @description
#' Aplica um vetor de pesos \code{weights} aos dias da janela
#' \eqn{[t - W, t]}, dando mais importancia aos dias recentes (biologicamente
#' mais relevantes) e menos aos dias mais distantes. A media ponderada e:
#'
#' \deqn{\bar{X}_w = \frac{\sum_{d=0}^{W} w_d \cdot X_{t-d}}{\sum_{d=0}^{W} w_d \cdot \mathbb{1}[X_{t-d} \text{ nao e NA}]}}
#'
#' Onde \eqn{w_0} e o peso do dia do evento (posicao 1 no vetor
#' \code{weights}) e \eqn{w_W} e o peso do dia mais antigo (posicao W+1).
#'
#' Aplicacoes clinicas:
#' \itemize{
#'   \item PM2.5/asma: impacto forte nas primeiras 48h, dilui depois.
#'   \item Doencas infecciosas: modelar incubacao com mais peso no periodo
#'     de maior probabilidade de contato infeccioso.
#' }
#'
#' Para variaveis de soma (rainfall_mm, sr_kj_m2), a funcao calcula a
#' \emph{soma ponderada} em vez de media ponderada — comportamento
#' epidemiologicamente correto (precipitacao recente pesa mais no escoamento).
#' Para \code{wd_degrees}, usa media circular ponderada (von Mises).
#'
#' Usa \code{data.table::foverlaps} para eficiencia de memoria.
#' Colunas de saida: \code{wwin{W}_{var}}.
#'
#' @keywords internal
#' @noRd
.join_weighted_window <- function(health_data, climate_data, target_vars,
                                   window_days, weights, min_obs) 
                                   {
  window_size    <- window_days + 1L
  agg_rule       <- .build_agg_rules(target_vars)
  health_id_vars <- setdiff(names(health_data), "geom")

  # weights[1] = dia do evento (lag 0), weights[W+1] = dia mais antigo (lag W)
  # Revertemos para alinhar com a ordenacao do join (date_int decrescente nao
  # e garantida), entao usamos o deslocamento: peso = weights[lag + 1]
  # onde lag = win_end_int - date_int  (0 = dia do evento, W = mais antigo).

  # --- Prepara data.table do clima ------------------------------------------
  climate_dt <- data.table::as.data.table(
    dplyr::select(climate_data, "code_muni", "date", dplyr::all_of(target_vars))
  )
  climate_dt[, date_int  := as.integer(date)]
  climate_dt[, date_int2 := date_int]
  climate_dt[, date := NULL]

  # --- Prepara data.table de saude ------------------------------------------
  health_dt <- data.table::as.data.table(
    dplyr::select(sf::st_drop_geometry(health_data), dplyr::all_of(health_id_vars))
  )
  health_dt[, .row_id        := .I]
  health_dt[, .win_start_int := as.integer(date) - window_days]
  health_dt[, .win_end_int   := as.integer(date)]

  # --- Overlap join ----------------------------------------------------------
  joined <- .foverlaps_window(health_dt, climate_dt, target_vars)

  # Calcula o lag de cada observacao climatica relativa ao dia do evento
  # lag = 0 para dia do evento, lag = W para dia mais antigo
  joined[, .lag := .win_end_int - date_int]

  # --- Media/soma ponderada por variavel ------------------------------------
  id_plus_row <- c(health_id_vars, ".row_id")

  result <- joined[
    ,
    {
      out_list <- list()
      for (v in target_vars) {
        vals     <- get(v)
        lags     <- .lag
        w_vec    <- weights[lags + 1L]   # indexacao: lag 0 -> weights[1]
        not_na   <- !is.na(vals)
        n_obs    <- sum(not_na)
        prop_val <- n_obs / window_size
        if (prop_val < min_obs) {
          out_list[[v]] <- NA_real_
          next
        }
        rule <- agg_rule$agg_type[agg_rule$climate_var == v]
        val_w <- if (identical(rule, "sum")) {
          # Soma ponderada (grandezas acumulativas)
          sum(vals[not_na] * w_vec[not_na])
        } else if (identical(rule, "mean_circular")) {
          # Media circular ponderada via decomposicao vetorial
          rad   <- vals[not_na] * pi / 180
          w_ok  <- w_vec[not_na]
          atan2(sum(w_ok * sin(rad)), sum(w_ok * cos(rad))) * 180 / pi
        } else {
          # Media ponderada padrao
          sum(vals[not_na] * w_vec[not_na]) / sum(w_vec[not_na])
        }
        out_list[[v]] <- val_w
      }
      out_list
    },
    by = id_plus_row
  ]

  old_nms <- target_vars
  new_nms <- paste0("wwin", window_days, "_", target_vars)
  data.table::setnames(result, old_nms, new_nms)
  result[, .row_id := NULL]

  tibble::as_tibble(result)
}

# =============================================================================
# FUNÇÕES UTILITÁRIAS
# =============================================================================

#' Regras de agregação por tipo de variável climática
#'
#' @description
#' Define se cada variável deve ser somada (`sum`), calculada como média
#' aritmética (`mean`) ou como média circular (`mean_circular`):
#' - `rainfall_mm`, `sr_kj_m2`: grandezas aditivas → soma.
#' - `wd_degrees`: ângulo circular → média vetorial (von Mises).
#' - Demais variáveis: médias aritméticas.
#'
#' @keywords internal
#' @noRd
.build_agg_rules <- function(vars) {
  dplyr::tibble(
    climate_var = vars,
    agg_type    = dplyr::case_when(
      vars %in% c("rainfall_mm", "sr_kj_m2") ~ "sum",
      vars == "wd_degrees"                    ~ "mean_circular",
      TRUE                                    ~ "mean"
    )
  )
}


#' Média circular (direção do vento)
#'
#' @description
#' Calcula a média vetorial de ângulos em graus usando decomposição em
#' componentes seno/cosseno. Evita o artefato da média aritmética de ângulos
#' (e.g., média de 1° e 359° deve ser 0°, não 180°).
#'
#' @keywords internal
#' @noRd
.circular_mean <- function(x, na_rm = TRUE) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(NA_real_)
  rad <- x * pi / 180
  atan2(mean(sin(rad)), mean(cos(rad))) * 180 / pi
}


#' Atualiza metadados do objeto climasus_df após agregação
#' @keywords internal
#' @noRd
.set_climate_agg_meta <- function(df, system) {
  if (!inherits(df, "climasus_df")) {
    meta <- list(
      system   = system,
      stage    = "climate",
      type     = "agg",
      spatial  = inherits(df, "sf"),
      temporal = NULL,
      created  = Sys.time(),
      modified = Sys.time(),
      history  = sprintf("[%s] Climate aggregation", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      user     = list()
    )
    base_classes <- setdiff(class(df), "climasus_df")
    structure(df, sus_meta = meta, class = c("climasus_df", base_classes))
  } else {
    sus_meta(df, stage = "climate", type = "agg")
  }
}




# =============================================================================
# HELPERS DE MENSAGENS DE ERRO
# =============================================================================

.msg_not_climasus_df <- function(lang, data_type) {
  pipeline_steps <- list(
    health = list(
      pt = c(
        "  1. sus_data_import() ou sus_data_read()",
        "  2. sus_data_clean_encoding()",
        "  3. sus_data_standardize()",
        "  4. sus_data_filter_cid()",
        "  5. sus_data_filter_demographics()",
        "  6. sus_data_aggregate()",
        "  7. sus_spatial_join()"
      ),
      en = c(
        "  1. sus_data_import() or sus_data_read()",
        "  2. sus_data_clean_encoding()",
        "  3. sus_data_standardize()",
        "  4. sus_data_filter_cid()",
        "  5. sus_data_filter_demographics()",
        "  6. sus_data_aggregate()",
        "  7. sus_spatial_join()"
      )
    ),
    climate = list(
      pt = c(
        "  1. sus_climate_inmet()",
        "  2. sus_climate_fill_inmet(evaluation = FALSE)"
      ),
      en = c(
        "  1. sus_climate_inmet()",
        "  2. sus_climate_fill_inmet(evaluation = FALSE)"
      )
    )
  )
  steps <- pipeline_steps[[data_type]][[lang]] %||% pipeline_steps[[data_type]][["en"]]
  base  <- list(
    pt = "Entrada nao e um objeto climasus_df.",
    en = "Input is not a climasus_df object.",
    es = "La entrada no es un objeto climasus_df."
  )
  paste0(
    base[[lang]] %||% base[["en"]],
    "\nEsta funcao requer dados do pipeline CLIMASUS4r.\n\nPrepare seus dados:\n",
    paste(steps, collapse = "\n")
  )
}

.msg_wrong_stage <- function(lang, data_type, current, required, hint) {
  labels <- list(
    health  = list(pt = "saude",  en = "health",  es = "salud"),
    climate = list(pt = "clima",  en = "climate", es = "clima")
  )
  dl <- labels[[data_type]][[lang]] %||% labels[[data_type]][["en"]]
  base <- list(
    pt = paste0("Dados de {dl} possuem estagio invalido.\n",
                "Atual: {current} | Requerido: {required}\n",
                "Execute: {dl}_data <- {hint}"),
    en = paste0("{dl} data has invalid stage.\n",
                "Current: {current} | Required: {required}\n",
                "Please run: {dl}_data <- {hint}")
  )
  glue::glue(base[[lang]] %||% base[["en"]],
             dl = dl, current = current %||% "unknown",
             required = required, hint = hint)
}

.msg_wrong_source <- function(lang) {
  msgs <- list(
    pt = "Dados climaticos devem ser da fonte INMET.",
    en = "Climate data must be from INMET source.",
    es = "Los datos climaticos deben ser de fuente INMET."
  )
  msgs[[lang]] %||% msgs[["en"]]
}

.msg_stage_validated <- function(lang) {
  msgs <- list(
    pt = "Estagio de dados validado: agregacao permitida",
    en = "Data stage validated: aggregation allowed",
    es = "Etapa de datos validada: agregacion permitida"
  )
  msgs[[lang]] %||% msgs[["en"]]
}

.msg_cannot_validate_dates <- function(lang) {
  msgs <- list(
    pt = "Nao foi possivel validar sobreposicao de datas: metadados temporais ausentes.",
    en = "Cannot validate date overlap: temporal metadata missing.",
    es = "No se puede validar superposicion de fechas: metadatos temporales faltantes."
  )
  msgs[[lang]] %||% msgs[["en"]]
}

.msg_no_date_overlap <- function(lang, h_start, h_end, c_start, c_end) {
  msgs <- list(
    pt = paste0("Sem sobreposicao de datas entre saude e clima.\n",
                "Saude: ", h_start, " a ", h_end, "\n",
                "Clima: ", c_start, " a ", c_end),
    en = paste0("No date overlap between health and climate data.\n",
                "Health: ", h_start, " to ", h_end, "\n",
                "Climate: ", c_start, " to ", c_end)
  )
  msgs[[lang]] %||% msgs[["en"]]
}

.msg_partial_date_overlap <- function(lang, c_start, c_end, eff_start, eff_end) {
  msgs <- list(
    pt = paste0("Dados de saude ultrapassam o periodo climatico.\n",
                "Clima disponivel: ", c_start, " a ", c_end, "\n",
                "Periodo efetivo: ", eff_start, " a ", eff_end),
    en = paste0("Health data extends beyond climate range.\n",
                "Climate available: ", c_start, " to ", c_end, "\n",
                "Effective period: ", eff_start, " to ", eff_end)
  )
  msgs[[lang]] %||% msgs[["en"]]
}

.msg_no_climate_vars <- function(lang) {
  msgs <- list(
    pt = "Nenhuma variavel climatica reconhecida nos dados.",
    en = "No recognized climate variables found in data.",
    es = "No se encontraron variables climaticas reconocidas."
  )
  msgs[[lang]] %||% msgs[["en"]]
}

.msg_missing_climate_vars <- function(lang, missing, available) {
  msgs <- list(
    pt = paste0("Variaveis nao encontradas: ", paste(missing, collapse = ", "),
                ". Usando: ", paste(available, collapse = ", ")),
    en = paste0("Variables not found: ", paste(missing, collapse = ", "),
                ". Using: ", paste(available, collapse = ", "))
  )
  msgs[[lang]] %||% msgs[["en"]]
}

.msg_no_valid_climate_vars <- function(lang) {
  msgs <- list(
    pt = "Nenhuma variavel climatica valida apos validacao.",
    en = "No valid climate variables remaining after validation.",
    es = "No quedan variables climaticas validas."
  )
  msgs[[lang]] %||% msgs[["en"]]
}
