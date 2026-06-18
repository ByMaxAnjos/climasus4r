# =============================================================================
# tutorial_08_clima_grade.R
# -----------------------------------------------------------------------------
# PROPOSITO
#   Gerar UMA VEZ, localmente, todas as figuras e tabelas canonicas usadas pelo
#   Tutorial 8 (Dados Climaticos e Ambientais em Grade) da vignette climasus4r.
#   A etapa cobre a familia de funcoes de GRADE do pacote, com foco em ERA5-Land
#   (~10 km, do Zenodo) agregado ao municipio de Sao Paulo, e os derivados
#   climaticos: anomalias, indicadores bioclimaticos, ondas de calor, SPI e SPEI.
#   A analise pesada (downloads, agregacao raster->poligono) roda AQUI; o .Rmd
#   apenas carrega os artefatos prontos (PNG via include_graphics, RDS via
#   readRDS).
#
# COMO RODAR
#   Rode a partir da RAIZ do pacote (pasta climasus4r/):
#       Rscript scripts/tutorial_08_clima_grade.R
#   ou, em sessao interativa com o working directory na raiz:
#       source("scripts/tutorial_08_clima_grade.R")
#
# ARTEFATOS GERADOS  (prefixo "grid_")
#   Figuras (vignettes-pt/figuras/):
#     - grid_serie.png            serie diaria de Tmean ERA5-Land (Sao Paulo)
#     - grid_anomalia.png         anomalia mensal de temperatura (z-score / abs)
#     - grid_ondas_calor.png      timeline/calendario de ondas de calor detectadas
#     - grid_spi_spei.png         SPI x SPEI multiescala (drought monitor)
#   Tabelas (vignettes-pt/dados/):
#     - grid_resumo.rds           resumo da serie ERA5-Land (n obs, periodo, vars)
#     - grid_eventos_hw.rds       eventos de onda de calor (amostra)
#
# ENTRADA : dados/caso_serie.rds        (serie de saude diaria do T4; guarda file.exists)
# SAIDA   : dados/caso_clima_grade.rds  (saude + clima de grade alinhado, para o T9)
#
# CASO CONDUTOR (atravessa os 9 tutoriais)
#   "Temperatura e desfechos de saude em idosos na Regiao Metropolitana de Sao
#   Paulo, 2014-2019". Neste tutorial importamos ERA5-Land para o municipio de
#   Sao Paulo e construimos uma EXPOSICAO ALTERNATIVA/COMPLEMENTAR a das estacoes
#   INMET (T7): temperatura em grade, anomalias e ondas de calor. Esses
#   indicadores entram como covariaveis no modelo DLNM do T9.
#
# NOTA SOBRE REDE/SUGGESTS
#   ERA5-Land exige acesso ao Zenodo (rede) e os pacotes terra, sf, exactextractr
#   e geobr. SPI/SPEI exigem slider; ondas de calor exigem slider e ggsci/plotly.
#   TODAS as chamadas sensiveis estao protegidas com requireNamespace()/tryCatch.
#   Se algo faltar, o script degrada graciosamente (mensagem) e segue, gerando o
#   maximo de artefatos possivel a partir de um exemplo minimo sintetico.
# =============================================================================

# --- Infraestrutura compartilhada -------------------------------------------
source("scripts/_setup_tutoriais.R")

COR_PRIMARIA   <- "#2E7D32"
COR_SECUNDARIA <- "#558B2F"
COR_ACENTO     <- "#EF6C00"

ANOS          <- 2014:2019
CODIGO_SP_6   <- "355030"    # Sao Paulo, IBGE 6 digitos
CODIGO_SP_7   <- "3550308"   # Sao Paulo, IBGE 7 digitos


# =============================================================================
# BLOCO 0 - Carregar a serie de saude encadeada (entrada do T8)
# =============================================================================
# O T4/T7 salvam a serie diaria de saude (obitos respiratorios/cardiovasculares
# de idosos da RMSP). Aqui ela e apenas o "esqueleto" temporal/espacial sobre o
# qual alinhamos o clima de grade. Se ausente, geramos um esqueleto minimo.
message(">> Bloco 0: carregando a serie de saude (caso_serie.rds)...")

entrada_path <- file.path("vignettes-pt", "dados", "caso_serie.rds")

if (file.exists(entrada_path)) {
  caso_serie <- readRDS(entrada_path)
  saude_df   <- dplyr::as_tibble(caso_serie)
  message("   Serie de saude carregada de: ", entrada_path)
} else {
  message("   AVISO: ", entrada_path, " nao encontrado. ",
          "Gerando esqueleto MINIMO sintetico (apenas ilustrativo).")
  set.seed(2014)
  datas    <- seq(as.Date("2014-01-01"), as.Date("2019-12-31"), by = "day")
  saude_df <- dplyr::tibble(
    date      = datas,
    code_muni = CODIGO_SP_6,
    obitos    = stats::rpois(length(datas), lambda = 8)
  )
}

# Detecta a coluna de data da serie de saude de forma robusta
col_data_saude <- intersect(c("date", "data", "data_obito"), names(saude_df))
col_data_saude <- if (length(col_data_saude) > 0) col_data_saude[[1]] else NA_character_


# =============================================================================
# BLOCO 1 - Importar ERA5-Land (~10 km) para o municipio de Sao Paulo
# =============================================================================
# sus_grid_era5() baixa agregados diarios do ERA5-Land (Latin America, Zenodo,
# Saldanha) e, com 'municipalities' (um objeto sf), agrega o raster para os
# poligonos municipais. Sem API key. Variaveis pedidas:
#   t2m -> tair_dry_bulb_c (media), t2m_max -> tair_max_c, t2m_min -> tair_min_c,
#   tp  -> rainfall_mm (soma diaria).
# Retorna climasus_df [stage="climate", type="era5_land"] com code_muni, date e
# uma coluna por variavel.
message(">> Bloco 1: importando ERA5-Land para Sao Paulo (pode baixar dados)...")

era5_sp <- NULL

sp_geom <- tryCatch({
  if (!requireNamespace("geobr", quietly = TRUE) ||
      !requireNamespace("sf", quietly = TRUE)) {
    message("   geobr/sf indisponiveis: pulando download de geometria.")
    NULL
  } else {
    mun <- suppressMessages(
      geobr::read_municipality(code_muni = "SP", year = 2020, showProgress = FALSE)
    )
    # Filtra apenas o municipio de Sao Paulo (IBGE 7 digitos)
    mun[as.character(mun$code_muni) == CODIGO_SP_7, ]
  }
}, error = function(e) {
  message("   Falha ao obter geometria de Sao Paulo: ", conditionMessage(e)); NULL
})

if (!is.null(sp_geom) &&
    requireNamespace("terra", quietly = TRUE) &&
    requireNamespace("exactextractr", quietly = TRUE)) {
  era5_sp <- tryCatch(
    sus_grid_era5(
      years          = ANOS,
      months         = 1:12,
      vars           = c("t2m", "t2m_max", "t2m_min", "tp"),
      municipalities = sp_geom,
      agg_fun        = "mean",   # valor areal representativo do municipio
      use_cache      = TRUE,
      parallel       = FALSE,
      lang           = "pt",
      verbose        = TRUE
    ),
    error = function(e) {
      message("   sus_grid_era5() indisponivel: ", conditionMessage(e)); NULL
    }
  )
} else {
  message("   terra/exactextractr/geometria indisponiveis: ERA5-Land sera sintetico.")
}

# --- Fallback sintetico (apenas para a vignette renderizar/encadear) --------
# Se a rede/pacotes nao estiverem disponiveis, criamos uma serie diaria
# climatologicamente plausivel para Sao Paulo (clima subtropical de altitude):
# Tmean ~ 19 C com sazonalidade de inverno (jun-ago mais frio).
if (is.null(era5_sp)) {
  message("   Construindo ERA5-Land SINTETICO (clima plausivel de Sao Paulo).")
  set.seed(2014)
  datas  <- seq(as.Date("2014-01-01"), as.Date("2019-12-31"), by = "day")
  doy    <- as.integer(format(datas, "%j"))
  # Sazonalidade: pico no verao austral (~jan), vale no inverno (~jul)
  ciclo  <- 4.5 * cos(2 * pi * (doy - 15) / 365)
  tmean  <- 19 + ciclo + stats::rnorm(length(datas), 0, 1.6)
  era5_sp <- dplyr::tibble(
    code_muni       = CODIGO_SP_6,
    date            = datas,
    tair_dry_bulb_c = round(tmean, 2),
    tair_max_c      = round(tmean + abs(stats::rnorm(length(datas), 5, 1.2)), 2),
    tair_min_c      = round(tmean - abs(stats::rnorm(length(datas), 5, 1.2)), 2),
    rainfall_mm     = round(pmax(0, stats::rgamma(length(datas), 0.6, 0.25)), 1)
  )
  attr(era5_sp, "sus_meta") <- list(
    system = NULL, stage = "climate", type = "era5_land",
    history = "[sintetico] serie ERA5-Land ilustrativa (sem rede)"
  )
  class(era5_sp) <- c("climasus_df", class(dplyr::tibble()))
}

era5_df <- dplyr::as_tibble(era5_sp)


# =============================================================================
# BLOCO 2 - Tabela-resumo da serie ERA5-Land
# =============================================================================
message(">> Bloco 2: resumo da serie ERA5-Land...")

vars_clim <- intersect(
  c("tair_dry_bulb_c", "tair_max_c", "tair_min_c", "rainfall_mm"),
  names(era5_df)
)

grid_resumo <- data.frame(
  Variavel = vars_clim,
  Media    = vapply(vars_clim, function(v) round(mean(era5_df[[v]], na.rm = TRUE), 2), numeric(1)),
  Minimo   = vapply(vars_clim, function(v) round(min(era5_df[[v]],  na.rm = TRUE), 2), numeric(1)),
  Maximo   = vapply(vars_clim, function(v) round(max(era5_df[[v]],  na.rm = TRUE), 2), numeric(1)),
  N_obs    = vapply(vars_clim, function(v) sum(!is.na(era5_df[[v]])), integer(1)),
  row.names = NULL,
  stringsAsFactors = FALSE
)
save_tbl(grid_resumo, "grid_resumo")


# =============================================================================
# BLOCO 3 - Figura: serie diaria de temperatura media ERA5-Land
# =============================================================================
message(">> Bloco 3: serie diaria de Tmean ERA5-Land...")

serie_plot <- era5_df |>
  dplyr::filter(!is.na(.data$tair_dry_bulb_c)) |>
  dplyr::arrange(.data$date)

p_serie <- ggplot2::ggplot(serie_plot, ggplot2::aes(x = date, y = tair_dry_bulb_c)) +
  ggplot2::geom_line(color = COR_PRIMARIA, linewidth = 0.4, alpha = 0.85) +
  ggplot2::labs(
    title    = "Temperatura media diaria - ERA5-Land (~10 km)",
    subtitle = "Municipio de Sao Paulo, 2014-2019 (agregacao areal por exactextractr)",
    x        = "Data",
    y        = expression("Temperatura media ("*degree*"C)")
  ) +
  tema_climasus()

save_fig(p_serie, "grid_serie", width = 8, height = 5)


# =============================================================================
# BLOCO 4 - Anomalias climaticas (sus_climate_anomaly)
# =============================================================================
# sus_climate_anomaly() compara observacoes a normais climatologicas do INMET
# (sus_climate_normals). Como ERA5-Land NAO traz station_code/normais INMET,
# ilustramos a anomalia de forma DIDATICA: anomalia mensal vs. a climatologia
# mensal da propria serie (obs - media do mes), e o z-score (obs - media)/sd.
# Esta e a MESMA matematica de method="absolute"/"standardized", apenas com a
# referencia derivada localmente quando nao ha normais INMET disponiveis.
message(">> Bloco 4: anomalias mensais de temperatura...")

anom_mensal <- era5_df |>
  dplyr::filter(!is.na(.data$tair_dry_bulb_c)) |>
  dplyr::mutate(
    ano = as.integer(format(.data$date, "%Y")),
    mes = as.integer(format(.data$date, "%m"))
  ) |>
  dplyr::summarise(
    t_obs = mean(.data$tair_dry_bulb_c, na.rm = TRUE),
    .by   = c("ano", "mes")
  ) |>
  dplyr::mutate(
    t_normal      = ave(t_obs, mes, FUN = function(x) mean(x, na.rm = TRUE)),
    sd_mes        = ave(t_obs, mes, FUN = function(x) stats::sd(x, na.rm = TRUE)),
    anomalia      = t_obs - t_normal,
    anomalia_std  = ifelse(sd_mes > 1e-9, anomalia / sd_mes, NA_real_),
    periodo       = as.Date(sprintf("%04d-%02d-01", ano, mes))
  ) |>
  dplyr::arrange(periodo)

p_anom <- ggplot2::ggplot(anom_mensal, ggplot2::aes(x = periodo, y = anomalia, fill = anomalia > 0)) +
  ggplot2::geom_col(width = 22, show.legend = FALSE) +
  ggplot2::geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
  ggplot2::scale_fill_manual(values = c(`TRUE` = COR_ACENTO, `FALSE` = COR_SECUNDARIA)) +
  ggplot2::labs(
    title    = "Anomalia mensal de temperatura - ERA5-Land",
    subtitle = "Desvio (obs - normal mensal local), Sao Paulo, 2014-2019",
    x        = "Mes",
    y        = expression("Anomalia ("*degree*"C)")
  ) +
  tema_climasus()

save_fig(p_anom, "grid_anomalia", width = 8, height = 5)


# =============================================================================
# BLOCO 5 - Ondas de calor (sus_climate_compute_heatwaves + plot)
# =============================================================================
# sus_climate_compute_heatwaves() exige um climasus_df [stage="climate",
# type in {inmet, filled, indicators}] com colunas date e station_code, e
# temperaturas tair_max_c/tair_min_c/tair_dry_bulb_c. Adaptamos a serie ERA5
# para esse contrato (station_code = code_muni) e aplicamos os metodos WHO,
# WMO e INMET. Em seguida, sus_climate_plot_heatwaves() gera a visualizacao.
message(">> Bloco 5: deteccao de ondas de calor...")

hw_result <- NULL
eventos_hw <- NULL

if (requireNamespace("slider", quietly = TRUE)) {
  # Monta o climasus_df no contrato esperado pela funcao de ondas de calor
  hw_input <- era5_df |>
    dplyr::transmute(
      station_code   = .data$code_muni,
      date           = .data$date,
      tair_dry_bulb_c = .data$tair_dry_bulb_c,
      tair_max_c     = if ("tair_max_c" %in% names(era5_df)) .data$tair_max_c else .data$tair_dry_bulb_c,
      tair_min_c     = if ("tair_min_c" %in% names(era5_df)) .data$tair_min_c else .data$tair_dry_bulb_c
    )
  attr(hw_input, "sus_meta") <- list(stage = "climate", type = "inmet",
                                     history = "[derivado] ERA5-Land -> contrato heatwaves")
  class(hw_input) <- c("climasus_df", class(dplyr::tibble()))

  hw_result <- tryCatch(
    sus_climate_compute_heatwaves(
      df             = hw_input,
      method         = c("WHO", "WMO", "INMET"),
      baseline_start = "2014-01-01",
      baseline_end   = "2019-12-31",
      percentile     = 90,
      lang           = "pt",
      verbose        = TRUE
    ),
    error = function(e) {
      message("   sus_climate_compute_heatwaves() falhou: ", conditionMessage(e)); NULL
    }
  )
} else {
  message("   pacote 'slider' indisponivel: ondas de calor puladas.")
}

# Tabela de eventos (amostra) + figura
if (!is.null(hw_result) && nrow(hw_result$events) > 0) {
  eventos_hw <- hw_result$events |>
    dplyr::select(dplyr::any_of(c(
      "method", "start_date", "end_date", "duration_days",
      "temp_peak", "anomaly_mean", "intensity_class"
    ))) |>
    utils::head(12) |>
    as.data.frame()
  save_tbl(eventos_hw, "grid_eventos_hw")

  # Figura via sus_climate_plot_heatwaves (static ggplot, type="timeline")
  p_hw <- tryCatch({
    if (requireNamespace("ggsci", quietly = TRUE)) {
      sus_climate_plot_heatwaves(
        hw_result,
        type        = "trend",
        interactive = FALSE,
        lang        = "pt"
      )
    } else NULL
  }, error = function(e) {
    message("   sus_climate_plot_heatwaves() falhou: ", conditionMessage(e)); NULL
  })

  if (!is.null(p_hw) && inherits(p_hw, "ggplot")) {
    save_fig(p_hw, "grid_ondas_calor", width = 8, height = 5)
  } else {
    # Fallback: barras de eventos por ano e metodo a partir do summary
    resumo_hw <- hw_result$summary |>
      dplyr::group_by(year, method) |>
      dplyr::summarise(n = sum(n_events, na.rm = TRUE), .groups = "drop")
    p_hw_fb <- ggplot2::ggplot(resumo_hw,
                               ggplot2::aes(x = factor(year), y = n, fill = method)) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::scale_fill_manual(values = cf_colors) +
      ggplot2::labs(
        title    = "Ondas de calor detectadas por ano e metodo",
        subtitle = "ERA5-Land, Sao Paulo, 2014-2019 (WHO/WMO/INMET)",
        x = "Ano", y = "Numero de eventos", fill = "Metodo"
      ) +
      tema_climasus()
    save_fig(p_hw_fb, "grid_ondas_calor", width = 8, height = 5)
  }
} else {
  message("   AVISO: grid_ondas_calor.png / grid_eventos_hw nao gerados.")
}


# =============================================================================
# BLOCO 6 - SPI e SPEI (secas e periodos umidos)
# =============================================================================
# sus_climate_compute_spi() e sus_climate_compute_spei() exigem dados MENSAIS
# (uma linha por municipio x mes) com code_muni e date (1o dia do mes). Agregamos
# a serie diaria ERA5-Land para mensal: precipitacao por SOMA, temperatura por
# MEDIA. SPI usa apenas precipitacao; SPEI usa P - PET (Thornthwaite via temp).
message(">> Bloco 6: SPI e SPEI multiescala...")

spi_spei_df <- NULL

if (requireNamespace("slider", quietly = TRUE)) {
  mensal <- era5_df |>
    dplyr::mutate(date_m = as.Date(format(.data$date, "%Y-%m-01"))) |>
    dplyr::summarise(
      rainfall_mm     = sum(.data$rainfall_mm,     na.rm = TRUE),
      tair_dry_bulb_c = mean(.data$tair_dry_bulb_c, na.rm = TRUE),
      .by = c("code_muni", "date_m")
    ) |>
    dplyr::rename(date = date_m) |>
    dplyr::arrange(.data$code_muni, .data$date)

  attr(mensal, "sus_meta") <- list(stage = "climate", type = "era5_land",
                                    history = "[derivado] ERA5-Land mensal p/ SPI/SPEI")
  class(mensal) <- c("climasus_df", class(dplyr::tibble()))

  # SPI (so precipitacao). Escalas curtas, pois a serie tem so 6 anos (72 meses).
  spi_out <- tryCatch(
    sus_climate_compute_spi(
      df     = mensal,
      var    = "rainfall_mm",
      scales = c(1L, 3L, 6L),
      min_n  = 24L,
      lang   = "pt",
      verbose = TRUE
    ),
    error = function(e) {
      message("   sus_climate_compute_spi() falhou: ", conditionMessage(e)); NULL
    }
  )

  # SPEI (P - PET; PET via Thornthwaite a partir da temperatura)
  spei_out <- tryCatch(
    sus_climate_compute_spei(
      df         = if (!is.null(spi_out)) spi_out else mensal,
      rain_var   = "rainfall_mm",
      pet_method = "thornthwaite",
      temp_var   = "tair_dry_bulb_c",
      scales     = c(1L, 3L, 6L),
      min_n      = 24L,
      lang       = "pt",
      verbose    = TRUE
    ),
    error = function(e) {
      message("   sus_climate_compute_spei() falhou: ", conditionMessage(e)); NULL
    }
  )

  spi_spei_df <- if (!is.null(spei_out)) dplyr::as_tibble(spei_out)
                 else if (!is.null(spi_out)) dplyr::as_tibble(spi_out)
                 else NULL
} else {
  message("   pacote 'slider' indisponivel: SPI/SPEI pulados.")
}

# Figura: SPI-3 x SPEI-3 ao longo do tempo (drought monitor)
if (!is.null(spi_spei_df) &&
    all(c("spi_3mo", "spei_3mo") %in% names(spi_spei_df))) {
  long_idx <- spi_spei_df |>
    dplyr::select(date, spi_3mo, spei_3mo) |>
    tidyr::pivot_longer(c(spi_3mo, spei_3mo),
                        names_to = "indice", values_to = "valor") |>
    dplyr::filter(!is.na(valor)) |>
    dplyr::mutate(indice = ifelse(indice == "spi_3mo", "SPI-3", "SPEI-3"))

  p_spi <- ggplot2::ggplot(long_idx, ggplot2::aes(x = date, y = valor, color = indice)) +
    ggplot2::geom_hline(yintercept = c(-1, 1), linetype = "dashed",
                        color = "grey60", linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::scale_color_manual(values = c("SPI-3" = COR_PRIMARIA, "SPEI-3" = COR_ACENTO)) +
    ggplot2::labs(
      title    = "SPI-3 vs SPEI-3 - monitor de seca/umidade",
      subtitle = "ERA5-Land mensal, Sao Paulo, 2014-2019 (linhas: +/-1 = limiar moderado)",
      x = "Mes", y = "Indice (z-score)", color = "Indice"
    ) +
    tema_climasus()
  save_fig(p_spi, "grid_spi_spei", width = 8, height = 5)
} else {
  message("   AVISO: grid_spi_spei.png nao gerado (SPI/SPEI indisponiveis).")
}


# =============================================================================
# BLOCO 7 - sus_climate_compute_indicators (ilustrativo)
# =============================================================================
# Os indicadores bioclimaticos (WBGT, HI, UTCI, ...) precisam de umidade,
# radiacao e vento, que a serie ERA5-Land minima deste tutorial nao traz. Aqui
# apenas DEMONSTRAMOS a chamada (sem gerar figura) e registramos no log que, em
# fluxo real, ela e aplicada sobre dados horarios completos do INMET (T7) ou
# ERA5-Land com td2m/u10/v10/sp.
message(">> Bloco 7: sus_climate_compute_indicators (apenas demonstrativo).")
message("   (Requer T, RH, SR, WS horarios; serie ERA5 minima nao os contem.)")


# =============================================================================
# BLOCO 8 - Encadeamento: alinhar clima de grade a saude e salvar p/ T9
# =============================================================================
# Juntamos a serie de saude (esqueleto temporal) com a temperatura ERA5-Land
# diaria, produzindo a exposicao de grade que o T9 (modelagem DLNM) usara como
# alternativa/complemento a exposicao de estacao (T7). Salvamos uma LISTA com a
# serie diaria, a serie mensal+SPI/SPEI e os eventos de onda de calor.
message(">> Bloco 8: alinhando clima de grade a saude e salvando para o T9...")

saude_clima <- NULL
if (!is.na(col_data_saude)) {
  saude_tmp <- saude_df
  saude_tmp[["date"]] <- as.Date(saude_tmp[[col_data_saude]])
  saude_clima <- dplyr::left_join(
    saude_tmp,
    era5_df |> dplyr::select(dplyr::any_of(c("date", vars_clim))),
    by = "date"
  )
}

caso_clima_grade <- list(
  era5_diario   = era5_df,
  saude_clima   = saude_clima,
  spi_spei      = spi_spei_df,
  ondas_calor   = if (!is.null(hw_result)) hw_result$events else NULL,
  meta = list(
    uf        = "SP",
    code_muni = CODIGO_SP_6,
    anos      = ANOS,
    fonte     = "ERA5-Land (Zenodo, ~10 km)",
    vars      = vars_clim
  )
)
save_tbl(caso_clima_grade, "caso_clima_grade")

message("== Concluido. Artefatos em vignettes-pt/figuras/ e vignettes-pt/dados/ ==")
