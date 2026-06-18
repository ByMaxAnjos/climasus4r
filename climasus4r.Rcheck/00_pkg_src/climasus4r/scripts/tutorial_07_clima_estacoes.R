# =============================================================================
# tutorial_07_clima_estacoes.R
# -----------------------------------------------------------------------------
# PROPOSITO
#   Gerar UMA VEZ, localmente, todas as figuras e tabelas canonicas usadas pelo
#   Tutorial 7 (Integracao Climatica por Estacoes - INMET) da vignette
#   climasus4r. Esta etapa do pipeline (stage = "climate") cobre cinco funcoes:
#     - sus_climate_inmet()      importa a serie horaria de uma estacao INMET
#     - sus_climate_fill_inmet() preenche lacunas via XGBoost por estacao
#     - sus_climate_plot_fill()  visualiza observado vs imputado
#     - sus_climate_normals()    baixa as normais climatologicas (1991-2020)
#     - sus_climate_aggregate()  alinha o clima a serie de saude (estrategias
#                                temporais / lag)
#   A analise pesada (downloads INMET, treino XGBoost) roda AQUI; o arquivo .Rmd
#   apenas carrega os artefatos prontos (PNG via include_graphics, RDS via
#   readRDS).
#
# COMO RODAR
#   Rode a partir da RAIZ do pacote (pasta climasus4r/):
#       Rscript scripts/tutorial_07_clima_estacoes.R
#   ou, em sessao interativa com o working directory na raiz:
#       source("scripts/tutorial_07_clima_estacoes.R")
#
# ARTEFATOS GERADOS  (prefixo "inmet_")
#   Figuras (vignettes-pt/figuras/):
#     - inmet_serie.png            serie de temperatura observada da estacao A701
#     - inmet_fill.png             observado vs imputado (sus_climate_plot_fill)
#     - inmet_normais.png          ciclo anual das normais de temperatura (1991-2020)
#     - inmet_lag.png              exposicao por estrategia temporal (exact x movel)
#   Tabelas (vignettes-pt/dados/):
#     - inmet_resumo.rds           resumo da estacao importada (cobertura, lacunas)
#     - inmet_fill_metricas.rds    metricas de avaliacao do preenchimento (RMSE/MAE/R2)
#   Encadeamento (caso condutor):
#     - caso_clima_estacao.rds     serie de saude + clima alinhado, entrada do T8
#
# CASO CONDUTOR (atravessa os 9 tutoriais)
#   "Temperatura e desfechos de saude em idosos na Regiao Metropolitana de Sao
#   Paulo, 2014-2019". Aqui IMPORTAMOS a serie da estacao INMET de referencia da
#   RMSP -- Sao Paulo - Mirante de Santana (codigo A701) --, preenchemos lacunas,
#   calculamos as normais e ALINHAMOS a temperatura a serie diaria de saude
#   herdada do Tutorial 4 (dados/caso_serie.rds).
#
# ENTRADA : dados/caso_serie.rds        (serie de saude agregada do T4)
# SAIDA   : dados/caso_clima_estacao.rds (saude + clima alinhado, para o T8)
#
# OBS.: TODA chamada de rede / pacote Suggests (arrow, xgboost, furrr, ggsci,
#       DT, patchwork, sf) e protegida por requireNamespace()/tryCatch e degrada
#       graciosamente (mensagem) se indisponivel. O script e ILUSTRATIVO.
# =============================================================================

# --- Infraestrutura compartilhada -------------------------------------------
source("scripts/_setup_tutoriais.R")

COR_PRIMARIA   <- "#2E7D32"
COR_SECUNDARIA <- "#558B2F"
COR_ACENTO     <- "#EF6C00"

# Parametros do caso condutor
ANOS          <- 2014:2019
UF            <- "SP"
ESTACAO       <- "A701"             # Sao Paulo - Mirante de Santana (RMSP)
VAR_TEMP      <- "tair_dry_bulb_c"  # temperatura media do bulbo seco (degC)


# =============================================================================
# BLOCO 0 - Carregar a entrada encadeada (serie de saude do T4)
# =============================================================================
# O T4 (Criacao de Variaveis & Agregacao) salvou a serie diaria de obitos
# respiratorios/cardiovasculares de idosos da RMSP em dados/caso_serie.rds.
# Carregamos com guarda file.exists(); se ausente, seguimos com um exemplo
# minimo sintetico apenas para ilustrar o alinhamento clima x saude.
message(">> Bloco 0: carregando a serie de saude do Tutorial 4...")

entrada_path <- file.path("vignettes-pt", "dados", "caso_serie.rds")

if (file.exists(entrada_path)) {
  caso_serie <- readRDS(entrada_path)
  message("   Serie de saude carregada de: ", entrada_path)
  # A serie do T4 deve conter, no minimo, uma coluna de data diaria e a
  # contagem do desfecho. Tentamos detectar a coluna de data de forma robusta.
  saude_df <- dplyr::as_tibble(caso_serie)
} else {
  message("   AVISO: ", entrada_path, " nao encontrado. ",
          "Gerando serie de saude MINIMA sintetica (apenas ilustrativa).")
  set.seed(2014)
  datas <- seq(as.Date("2014-01-01"), as.Date("2019-12-31"), by = "day")
  saude_df <- dplyr::tibble(
    date      = datas,
    code_muni = "355030",  # Sao Paulo (IBGE 6 digitos)
    obitos    = stats::rpois(length(datas), lambda = 8)
  )
}


# =============================================================================
# BLOCO 1 - sus_climate_inmet(): importar a serie horaria da estacao A701
# =============================================================================
# sus_climate_inmet() baixa, padroniza e aplica controle de qualidade aos dados
# horarios do INMET. Filtramos pela estacao de referencia da RMSP (A701) via
# station_code. O retorno e um climasus_df (stage="climate", type="inmet") com
# colunas canonicas (tair_dry_bulb_c, rh_mean_porc, rainfall_mm, ...).
message(">> Bloco 1: importando a serie INMET da estacao ", ESTACAO, "...")

clima_horario <- tryCatch(
  sus_climate_inmet(
    years        = ANOS,
    uf           = UF,
    station_code = ESTACAO,
    use_cache    = TRUE,
    parallel     = TRUE,
    workers      = 4,
    lang         = "pt",
    verbose      = TRUE
  ),
  error = function(e) {
    message("   sus_climate_inmet() indisponivel (rede/arrow?): ",
            conditionMessage(e))
    NULL
  }
)

# Fallback ilustrativo: serie horaria sintetica com lacunas, caso a rede falhe.
if (is.null(clima_horario)) {
  message("   Gerando serie horaria INMET MINIMA sintetica (ilustrativa).")
  set.seed(701)
  horas <- seq(as.POSIXct("2014-01-01 00:00", tz = "UTC"),
               as.POSIXct("2019-12-31 23:00", tz = "UTC"), by = "hour")
  n <- length(horas)
  # Ciclo sazonal (inverno mais frio no Sul/Sudeste) + ciclo diario + ruido
  doy <- as.integer(format(horas, "%j"))
  hr  <- as.integer(format(horas, "%H"))
  temp <- 20 - 4 * cos(2 * pi * doy / 365) - 3 * cos(2 * pi * hr / 24) +
    stats::rnorm(n, 0, 1.5)
  # Introduz ~12% de lacunas (MCAR) para ilustrar o preenchimento
  temp[sample.int(n, floor(0.12 * n))] <- NA_real_
  clima_horario <- dplyr::tibble(
    station_code    = ESTACAO,
    station_name    = "SAO PAULO - MIRANTE DE SANTANA",
    UF              = UF,
    latitude        = -23.4961,
    longitude       = -46.6201,
    altitude        = 792,
    date            = horas,
    year            = as.integer(format(horas, "%Y")),
    tair_dry_bulb_c = round(temp, 1),
    rh_mean_porc    = pmin(pmax(round(70 + stats::rnorm(n, 0, 8)), 0), 100),
    rainfall_mm     = pmax(round(stats::rgamma(n, 0.05, 0.5), 1), 0)
  )
  # Promove a climasus_df minimo para manter o contrato do pipeline
  attr(clima_horario, "sus_meta") <- list(
    system = NULL, stage = "climate", type = "inmet",
    years = ANOS, ufs = UF, station_codes = ESTACAO
  )
  class(clima_horario) <- c("climasus_df", class(clima_horario))
}


# =============================================================================
# BLOCO 2 - Figura + tabela: cobertura e lacunas da serie importada
# =============================================================================
message(">> Bloco 2: figura da serie observada e resumo de cobertura...")

clima_tbl <- dplyr::as_tibble(clima_horario)

# Serie diaria (media) da temperatura observada, para uma figura legivel
serie_dia <- clima_tbl |>
  dplyr::mutate(dia = as.Date(date)) |>
  dplyr::group_by(dia) |>
  dplyr::summarise(
    temp_media = mean(.data[[VAR_TEMP]], na.rm = TRUE),
    .groups = "drop"
  )

p_serie <- ggplot2::ggplot(serie_dia, ggplot2::aes(x = dia, y = temp_media)) +
  ggplot2::geom_line(color = COR_PRIMARIA, linewidth = 0.5, na.rm = TRUE) +
  ggplot2::labs(
    title    = "Temperatura media diaria observada - estacao A701",
    subtitle = "Sao Paulo - Mirante de Santana (INMET), 2014-2019",
    x        = "Data",
    y        = "Temperatura do ar (°C)"
  ) +
  tema_climasus()

save_fig(p_serie, "inmet_serie", width = 8, height = 5)

# Resumo de cobertura / lacunas por variavel-chave (antes do preenchimento)
vars_resumo <- intersect(
  c("tair_dry_bulb_c", "rh_mean_porc", "rainfall_mm"),
  names(clima_tbl)
)
inmet_resumo <- dplyr::tibble(
  Variavel = vars_resumo,
  N_observacoes = vapply(vars_resumo, function(v) sum(!is.na(clima_tbl[[v]])), integer(1)),
  N_lacunas     = vapply(vars_resumo, function(v) sum(is.na(clima_tbl[[v]])), integer(1)),
  Pct_lacunas   = vapply(vars_resumo,
                         function(v) round(100 * mean(is.na(clima_tbl[[v]])), 1),
                         numeric(1))
) |> as.data.frame()
save_tbl(inmet_resumo, "inmet_resumo")


# =============================================================================
# BLOCO 3 - sus_climate_fill_inmet(): avaliacao + preenchimento por XGBoost
# =============================================================================
# Primeiro rodamos em MODO AVALIACAO (run_evaluation = TRUE): cria lacunas
# artificiais MCAR, imputa e devolve metricas por estacao (RMSE, MAE, R2...).
# Depois rodamos em MODO PRODUCAO para preencher de fato as lacunas reais.
# Requer xgboost + furrr + zoo + MASS (Suggests) -> protegido por tryCatch.
message(">> Bloco 3: preenchimento de lacunas com XGBoost...")

tem_fill <- all(vapply(c("xgboost", "furrr", "zoo", "MASS"),
                       function(p) requireNamespace(p, quietly = TRUE),
                       logical(1)))

clima_preenchido <- NULL
inmet_fill_metricas <- NULL

if (tem_fill) {
  # --- Modo avaliacao ---
  aval <- tryCatch(
    sus_climate_fill_inmet(
      df                = clima_horario,
      target_var        = VAR_TEMP,
      quality_threshold = 0.4,
      run_evaluation    = TRUE,
      gap_percentage    = 0.2,
      parallel          = TRUE,
      workers           = 4,
      lang              = "pt",
      verbose           = TRUE
    ),
    error = function(e) {
      message("   avaliacao do preenchimento pulada: ", conditionMessage(e)); NULL
    }
  )
  if (!is.null(aval) && !is.null(aval[[VAR_TEMP]]$metrics)) {
    inmet_fill_metricas <- as.data.frame(aval[[VAR_TEMP]]$metrics)
    save_tbl(inmet_fill_metricas, "inmet_fill_metricas")
  }

  # --- Modo producao ---
  clima_preenchido <- tryCatch(
    sus_climate_fill_inmet(
      df                = clima_horario,
      target_var        = VAR_TEMP,
      quality_threshold = 0.4,
      run_evaluation    = FALSE,
      keep_features     = FALSE,
      parallel          = TRUE,
      workers           = 4,
      lang              = "pt",
      verbose           = TRUE
    ),
    error = function(e) {
      message("   preenchimento em producao pulado: ", conditionMessage(e)); NULL
    }
  )
} else {
  message("   xgboost/furrr/zoo/MASS indisponiveis: preenchimento pulado.")
}

# Fallback ilustrativo do preenchimento (interpolacao simples) se XGBoost falhar
if (is.null(clima_preenchido)) {
  message("   Gerando preenchimento ILUSTRATIVO (interpolacao linear).")
  flag_col <- paste0("is_imputed_", VAR_TEMP)
  clima_preenchido <- clima_tbl
  faltantes <- is.na(clima_preenchido[[VAR_TEMP]])
  clima_preenchido[[VAR_TEMP]] <- stats::approx(
    seq_along(clima_preenchido[[VAR_TEMP]]),
    clima_preenchido[[VAR_TEMP]],
    seq_along(clima_preenchido[[VAR_TEMP]]),
    rule = 2
  )$y
  clima_preenchido[[flag_col]] <- faltantes
  attr(clima_preenchido, "sus_meta") <- list(
    system = NULL, stage = "climate", type = "filled"
  )
  class(clima_preenchido) <- c("climasus_df", class(dplyr::as_tibble(clima_preenchido)))

  if (is.null(inmet_fill_metricas)) {
    inmet_fill_metricas <- data.frame(
      station = ESTACAO,
      rmse = NA_real_, mae = NA_real_, r_squared = NA_real_,
      smape = NA_real_, n_gaps = sum(faltantes)
    )
    save_tbl(inmet_fill_metricas, "inmet_fill_metricas")
  }
}


# =============================================================================
# BLOCO 4 - sus_climate_plot_fill(): observado vs imputado
# =============================================================================
# Visualiza a serie com as lacunas reais e os valores imputados destacados.
# Modo estatico (interactive = FALSE -> ggplot2). Requer ggplot2/ggsci/DT/
# patchwork/htmltools (Suggests) -> protegido.
message(">> Bloco 4: figura observado vs imputado...")

tem_plot <- all(vapply(c("ggplot2", "ggsci", "DT", "patchwork", "htmltools"),
                       function(p) requireNamespace(p, quietly = TRUE),
                       logical(1)))

p_fill <- tryCatch({
  if (tem_plot && inherits(clima_preenchido, "climasus_df") &&
      inherits(clima_horario, "climasus_df")) {
    sus_climate_plot_fill(
      df_filled   = clima_preenchido,
      df_original = clima_horario,
      target_var  = VAR_TEMP,
      interactive = FALSE,
      output_type = "plot",
      lang        = "pt",
      verbose     = FALSE
    )
  } else {
    stop("dependencias do plot ou classe climasus_df indisponiveis")
  }
}, error = function(e) {
  message("   sus_climate_plot_fill() indisponivel: ", conditionMessage(e))
  # Fallback: figura simples destacando pontos imputados
  flag_col <- paste0("is_imputed_", VAR_TEMP)
  d <- dplyr::as_tibble(clima_preenchido) |>
    dplyr::mutate(dia = as.Date(date)) |>
    dplyr::group_by(dia) |>
    dplyr::summarise(
      temp = mean(.data[[VAR_TEMP]], na.rm = TRUE),
      imputado = any(.data[[flag_col]] %in% TRUE),
      .groups = "drop"
    )
  ggplot2::ggplot(d, ggplot2::aes(x = dia, y = temp)) +
    ggplot2::geom_line(color = COR_PRIMARIA, linewidth = 0.4, na.rm = TRUE) +
    ggplot2::geom_point(
      data = dplyr::filter(d, imputado),
      color = COR_ACENTO, size = 0.8, na.rm = TRUE
    ) +
    ggplot2::labs(
      title    = "Preenchimento de lacunas: observado vs imputado",
      subtitle = "Estacao A701 - pontos em laranja indicam dias com imputacao",
      x = "Data", y = "Temperatura do ar (°C)"
    ) +
    tema_climasus()
})

if (!is.null(p_fill) && inherits(p_fill, "ggplot")) {
  save_fig(p_fill, "inmet_fill", width = 8, height = 5)
} else {
  message("   AVISO: inmet_fill.png nao gerado (plot interativo ou NULL).")
}


# =============================================================================
# BLOCO 5 - sus_climate_normals(): normais climatologicas 1991-2020
# =============================================================================
# Baixa as normais climatologicas de 30 anos do INMET (baseline climatologico).
# Filtramos as variaveis de temperatura e ilustramos o ciclo anual para a UF de
# SP. Requer rede + readxl/arrow -> protegido por tryCatch.
message(">> Bloco 5: normais climatologicas (1991-2020)...")

normais <- tryCatch(
  sus_climate_normals(
    period     = "1991-2020",
    target_var = c("t_max", "t_min", "t_mean_comp"),
    use_cache  = TRUE,
    lang       = "pt",
    verbose    = TRUE
  ),
  error = function(e) {
    message("   sus_climate_normals() indisponivel: ", conditionMessage(e)); NULL
  }
)

p_normais <- tryCatch({
  if (!is.null(normais)) {
    meses_ord <- c("janeiro", "fevereiro", "marco", "abril", "maio", "junho",
                   "julho", "agosto", "setembro", "outubro", "novembro", "dezembro")
    dat <- dplyr::as_tibble(normais) |>
      dplyr::filter(.data$uf == UF, !is.na(.data$valor)) |>
      dplyr::group_by(.data$mes, .data$var_code) |>
      dplyr::summarise(valor = mean(.data$valor, na.rm = TRUE), .groups = "drop") |>
      dplyr::mutate(mes = factor(.data$mes, levels = meses_ord))
    if (nrow(dat) == 0) stop("sem normais para a UF")
    ggplot2::ggplot(dat, ggplot2::aes(x = mes, y = valor,
                                      color = var_code, group = var_code)) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::geom_point(size = 1.6) +
      ggplot2::scale_color_manual(values = cf_colors) +
      ggplot2::labs(
        title    = "Normais climatologicas de temperatura (1991-2020)",
        subtitle = paste0("Media das estacoes do estado de ", UF),
        x = "Mes", y = "Temperatura (°C)", color = "Variavel"
      ) +
      tema_climasus() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  } else stop("normais NULL")
}, error = function(e) {
  message("   figura de normais pulada: ", conditionMessage(e)); NULL
})

if (!is.null(p_normais)) {
  save_fig(p_normais, "inmet_normais", width = 8, height = 5)
} else {
  message("   AVISO: inmet_normais.png nao gerado (sem rede / sem dados).")
}


# =============================================================================
# BLOCO 6 - sus_climate_aggregate(): alinhar o clima a serie de saude
# =============================================================================
# Liga cada registro de saude a estacao mais proxima e aplica a estrategia
# temporal. Comparamos duas estrategias epidemiologicas:
#   - "exact"        : temperatura do MESMO dia (efeito agudo do calor)
#   - "moving_window": media movel de 7 dias (exposicao cumulativa, RECOMENDADO)
# sus_climate_aggregate() espera health_data com 'date', 'code_muni' e geometria
# (sf). Como esta etapa e ilustrativa e a juncao espacial e o T5, protegemos a
# chamada e, se nao houver geometria, fazemos um alinhamento direto por data.
message(">> Bloco 6: alinhamento temporal clima x saude...")

# Serie diaria de temperatura preenchida (entrada do alinhamento)
clima_diario <- dplyr::as_tibble(clima_preenchido) |>
  dplyr::mutate(date = as.Date(date)) |>
  dplyr::group_by(date) |>
  dplyr::summarise(
    tair_dry_bulb_c = mean(.data[[VAR_TEMP]], na.rm = TRUE),
    .groups = "drop"
  )

# Detecta a coluna de data da serie de saude do T4 de forma robusta
col_data_saude <- intersect(c("date", "data", "dia"), names(saude_df))[1]
if (is.na(col_data_saude)) {
  # tenta a primeira coluna Date/POSIXct
  is_dt <- vapply(saude_df, function(x) inherits(x, c("Date", "POSIXct")), logical(1))
  col_data_saude <- names(saude_df)[which(is_dt)[1]]
}

saude_clima <- tryCatch({
  if (is.na(col_data_saude)) stop("coluna de data da serie de saude nao detectada")
  saude_norm <- saude_df |>
    dplyr::mutate(date = as.Date(.data[[col_data_saude]]))

  # Estrategia "exact": temperatura do proprio dia
  exact <- clima_diario |> dplyr::rename(temp_exact = tair_dry_bulb_c)

  # Estrategia "moving_window" (7 dias): media movel retroativa (t-6, t)
  mw <- clima_diario |>
    dplyr::arrange(date) |>
    dplyr::mutate(
      temp_mw7 = zoo::rollapply(tair_dry_bulb_c, width = 7, FUN = mean,
                                align = "right", fill = NA, na.rm = TRUE)
    ) |>
    dplyr::select(date, temp_mw7)

  saude_norm |>
    dplyr::left_join(exact, by = "date") |>
    dplyr::left_join(mw, by = "date")
}, error = function(e) {
  message("   alinhamento direto pulado: ", conditionMessage(e)); NULL
})

# Se houver geometria (sf) na serie de saude, ilustramos a chamada REAL.
tem_sf <- requireNamespace("sf", quietly = TRUE) && inherits(saude_df, "sf")
if (tem_sf) {
  saude_clima_real <- tryCatch(
    sus_climate_aggregate(
      health_data       = saude_df,
      climate_data      = clima_preenchido,
      climate_var       = VAR_TEMP,
      temporal_strategy = "moving_window",
      window_days       = 7,
      lang              = "pt",
      verbose           = TRUE
    ),
    error = function(e) {
      message("   sus_climate_aggregate() (sf) pulado: ", conditionMessage(e)); NULL
    }
  )
  if (!is.null(saude_clima_real)) saude_clima <- saude_clima_real
}

# --- Figura: exposicao por estrategia (exact x media movel 7d) ---
p_lag <- tryCatch({
  if (is.null(saude_clima) || !"temp_exact" %in% names(saude_clima)) {
    stop("serie alinhada indisponivel")
  }
  comp <- saude_clima |>
    dplyr::select(date, temp_exact, temp_mw7) |>
    dplyr::filter(date >= as.Date("2014-06-01"), date <= as.Date("2014-09-30")) |>
    tidyr::pivot_longer(c("temp_exact", "temp_mw7"),
                        names_to = "estrategia", values_to = "temp")
  if (nrow(comp) == 0) stop("janela vazia")
  comp <- comp |>
    dplyr::mutate(estrategia = dplyr::recode(estrategia,
      temp_exact = "exact (mesmo dia)",
      temp_mw7   = "moving_window (7 dias)"))
  ggplot2::ggplot(comp, ggplot2::aes(x = date, y = temp, color = estrategia)) +
    ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = c(COR_ACENTO, COR_PRIMARIA)) +
    ggplot2::labs(
      title    = "Exposicao a temperatura por estrategia temporal",
      subtitle = "Inverno de 2014, RMSP - 'exact' vs media movel de 7 dias",
      x = "Data", y = "Temperatura (°C)", color = "Estrategia"
    ) +
    tema_climasus()
}, error = function(e) {
  message("   figura de estrategias pulada: ", conditionMessage(e)); NULL
})

if (!is.null(p_lag)) {
  save_fig(p_lag, "inmet_lag", width = 8, height = 5)
} else {
  message("   AVISO: inmet_lag.png nao gerado.")
}


# =============================================================================
# BLOCO 7 - Encadeamento: salvar a serie saude + clima para o Tutorial 8
# =============================================================================
message(">> Bloco 7: salvando saude + clima alinhado para o proximo tutorial...")

caso_clima_estacao <- list(
  saude_clima   = saude_clima,
  clima_diario  = clima_diario,
  normais       = if (!is.null(normais)) dplyr::as_tibble(normais) else NULL,
  fill_metricas = inmet_fill_metricas,
  meta = list(
    uf = UF, anos = ANOS, estacao = ESTACAO, variavel = VAR_TEMP,
    estrategias = c("exact", "moving_window(7)")
  )
)
save_tbl(caso_clima_estacao, "caso_clima_estacao")

message("== Concluido. Artefatos em vignettes-pt/figuras/ e vignettes-pt/dados/ ==")
