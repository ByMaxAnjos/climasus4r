# =============================================================================
# modelagem_04_multicidade.R
# -----------------------------------------------------------------------------
# PROPOSITO
#   Gerar UMA VEZ, localmente, todas as figuras e tabelas canonicas usadas pelo
#   Tutorial 4 de Modelagem (mod-04-multicidade.Rmd) da vignette climasus4r.
#   Este script cobre a familia de analise multi-cidade do climasus4r:
#     - sus_mod_pool()              Meta-analise multivariada dois estagios (mvmeta/REML)
#     - sus_mod_plot_pool()         Visualizacoes: overall / forest / spaghetti
#     - sus_mod_metaregression()    Meta-regressao com covariavel de cidade
#     - sus_mod_sensitivity()       Analise de sensibilidade por subgrupos/estratos
#     - sus_mod_plot_sensitivity()  Visualizacoes: curves / scatter / bar
#   A modelagem pesada roda AQUI; o .Rmd apenas carrega os artefatos prontos
#   (PNG via include_graphics, RDS via readRDS).
#
# COMO RODAR
#   Rode a partir da RAIZ do pacote (pasta climasus4r/):
#       Rscript scripts/modelagem_04_multicidade.R
#   ou, em sessao interativa com working directory na raiz:
#       source("scripts/modelagem_04_multicidade.R")
#
# ARTEFATOS GERADOS  (prefixo "modpool_")
#   Figuras (vignettes-pt/figuras/):
#     - modpool_overall.png          curva exposicao-resposta agrupada (REML)
#     - modpool_forest.png           forest plot cidade-especifico (original + BLUP)
#     - modpool_spaghetti.png        curvas BLUP por cidade + estimativa agrupada
#     - modpool_sensitivity_curves.png  ERCs sobrepostas por subgrupo (faixa etaria)
#     - modpool_sensitivity_scatter.png dispersao RR_calor vs RR_frio por subgrupo (SI = tamanho)
#     - modpool_sensitivity_bar.png  forest hot/cold RR por subgrupo (ordenado por SI)
#   Tabelas (vignettes-pt/dados/):
#     - modpool_heterogeneidade.rds  estatisticas Q / I^2 / tau^2
#     - modpool_cidade_rr.rds        tabela cidade-especifica (raw + BLUP) no p75
#   Encadeamento (caso condutor):
#     - dados/mod_pool.rds           objeto climasus_pool ajustado (para reuso)
#
# ENTRADAS  (com guarda file.exists(); sintetiza fallback realista se ausentes)
#   vignettes-pt/dados/caso_serie.rds       serie diaria de obitos por municipio
#   vignettes-pt/dados/caso_clima_estacao.rds  temperatura INMET alinhada
#
# CASO CONDUTOR
#   "Temperatura e desfechos de saude em idosos (60+) na Regiao Metropolitana de
#   Sao Paulo, 2014-2019" (respiratorias J00-J99). Ajustamos um DLNM por municipio
#   da RMSP, agrupamos via meta-analise multivariada (REML/BLUP), aplicamos
#   meta-regressao com IDH como covariavel e avaliamos a sensibilidade por faixa
#   etaria (60-74 vs 75+).
#
# PACOTES SUGGESTS protegidos por requireNamespace() / tryCatch:
#   dlnm, splines, mvmeta, survival, MASS, xgboost, spdep, spatialreg,
#   SpatialEpi, CARBayes, INLA
# =============================================================================

# --- Infraestrutura compartilhada -------------------------------------------
source("scripts/_setup_tutoriais.R")

COR_PRIMARIA   <- "#2E7D32"   # verde floresta
COR_SECUNDARIA <- "#558B2F"   # verde folha
COR_ACENTO     <- "#EF6C00"   # laranja queimado (calor / risco)

# Parametros do caso condutor
ANOS     <- 2014:2019
VAR_TEMP <- "tair_dry_bulb_c"
DESFECHO <- "n_obitos"
LAG_MAX  <- 21L
DOF_ANO  <- 6L


# =============================================================================
# BLOCO 0 - Verificar pacotes Suggests necessarios
# =============================================================================
tem_dlnm   <- all(vapply(c("dlnm", "splines"),
                         function(p) requireNamespace(p, quietly = TRUE), logical(1L)))
tem_mvmeta <- requireNamespace("mvmeta", quietly = TRUE)

if (!tem_dlnm) {
  message("AVISO: pacotes 'dlnm' e/ou 'splines' nao disponiveis. ",
          "Os modelos serao sintetizados diretamente sem ajuste real.")
}
if (!tem_mvmeta) {
  message("AVISO: pacote 'mvmeta' nao disponivel. ",
          "sus_mod_pool() e sus_mod_metaregression() nao poderao ser chamados. ",
          "Instale com: install.packages('mvmeta')")
}


# =============================================================================
# BLOCO 1 - Construir serie diaria para cada municipio da RMSP
# =============================================================================
# Municípios da RMSP usados no caso condutor (subset de 6 para ilustrar o pool)
MUNICIPIOS <- c("Sao_Paulo", "Guarulhos", "Osasco",
                "Santo_Andre", "Mogi_das_Cruzes", "Carapicuiba")

# IDH (Indice de Desenvolvimento Humano, 2010) por municipio -- covariavel de meta-regressao
IDH_MUN <- c(
  Sao_Paulo        = 0.805,
  Guarulhos        = 0.763,
  Osasco           = 0.776,
  Santo_Andre      = 0.815,
  Mogi_das_Cruzes  = 0.783,
  Carapicuiba      = 0.749
)

message(">> Bloco 1: carregando ou sintetizando series diarias por municipio...")

path_saude <- file.path("vignettes-pt", "dados", "caso_serie.rds")
path_clima <- file.path("vignettes-pt", "dados", "caso_clima_estacao.rds")

# Funcao para detectar coluna de data em um data.frame
.detectar_data <- function(df) {
  cand <- intersect(c("date", "data", "dia", "data_obito"), names(df))
  if (length(cand) > 0L) return(cand[[1L]])
  is_dt <- vapply(df, function(x) inherits(x, c("Date", "POSIXct")), logical(1L))
  if (any(is_dt)) return(names(df)[which(is_dt)[1L]])
  NA_character_
}

# Gera serie sintetica para UN municipio (com sinal temperatura-mortalidade)
.sintetizar_serie_mun <- function(municipio, seed_offset = 0L) {
  set.seed(42L + seed_offset)
  datas <- seq(as.Date("2014-01-01"), as.Date("2019-12-31"), by = "day")
  n     <- length(datas)
  doy   <- as.integer(format(datas, "%j"))

  # Temperatura subtropical com leve variacao por municipio (altitude diferente)
  temp_base <- c(Sao_Paulo = 19, Guarulhos = 18.5, Osasco = 20,
                 Santo_Andre = 18, Mogi_das_Cruzes = 19.5, Carapicuiba = 20)[municipio]
  if (is.na(temp_base)) temp_base <- 19
  temp <- temp_base + 4.5 * cos(2 * pi * (doy - 15) / 365) + stats::rnorm(n, 0, 1.6)

  # Mortalidade quasi-Poisson com efeito em V (frio + calor)
  tref <- temp_base + 1
  tbar_frio   <- stats::filter(temp, rep(1/8, 8), sides = 1)
  tbar_frio[is.na(tbar_frio)] <- temp[is.na(tbar_frio)]
  tbar_quente <- stats::filter(temp, rep(1/3, 3), sides = 1)
  tbar_quente[is.na(tbar_quente)] <- temp[is.na(tbar_quente)]

  # Baseline varia por municipio (proporcional ao porte)
  baseline <- c(Sao_Paulo = 22, Guarulhos = 8, Osasco = 5,
                Santo_Andre = 6, Mogi_das_Cruzes = 5, Carapicuiba = 4)[municipio]
  if (is.na(baseline)) baseline <- 7

  log_mu <- log(baseline) +
    0.032 * pmax(0, tref - tbar_frio) +
    0.040 * pmax(0, tbar_quente - (temp_base + 7))

  dplyr::tibble(
    date             = datas,
    municipio        = municipio,
    n_obitos         = stats::rpois(n, lambda = exp(log_mu)),
    tair_dry_bulb_c  = round(temp, 2L)
  )
}

# Tenta carregar dados reais; senao usa fallback sintetico para todos os municipios
dados_reais <- NULL
if (file.exists(path_saude) && file.exists(path_clima)) {
  tryCatch({
    ss <- readRDS(path_saude)
    cc <- readRDS(path_clima)
    # caso_serie pode ser lista com $serie_diaria ou tibble plano
    ss_tb <- if (is.data.frame(ss)) dplyr::as_tibble(ss) else dplyr::as_tibble(ss$serie_diaria)
    col_d <- .detectar_data(ss_tb)
    if (!is.na(col_d)) {
      ss_tb[["date"]] <- as.Date(ss_tb[[col_d]])
      dados_reais <- ss_tb
    }
  }, error = function(e) {
    message("   AVISO: erro ao carregar dados reais: ", conditionMessage(e))
  })
}

# Constroi lista de series por municipio (uma por elemento)
series_municipios <- lapply(seq_along(MUNICIPIOS), function(i) {
  nm <- MUNICIPIOS[[i]]
  if (!is.null(dados_reais) && nm %in% unique(dados_reais[["municipio"]])) {
    # Usa dados reais se houver coluna municipio
    sub <- dados_reais |>
      dplyr::filter(.data[["municipio"]] == nm) |>
      dplyr::arrange(date)
    if (!"tair_dry_bulb_c" %in% names(sub)) {
      # Adiciona temperatura sintetica
      sint <- .sintetizar_serie_mun(nm, seed_offset = i)
      sub  <- dplyr::left_join(sub, sint[, c("date","tair_dry_bulb_c")], by = "date")
    }
    dplyr::as_tibble(sub)
  } else {
    .sintetizar_serie_mun(nm, seed_offset = i)
  }
})
names(series_municipios) <- MUNICIPIOS
message("   Series preparadas para: ", paste(MUNICIPIOS, collapse = ", "))


# =============================================================================
# BLOCO 2 - Construir climasus_df com lags para cada municipio
# =============================================================================
message(">> Bloco 2: construindo colunas de lag 0-", LAG_MAX, " por municipio...")

.construir_df_lags <- function(serie, nome_mun) {
  df_lags <- serie |> dplyr::arrange(date)
  for (k in 0L:LAG_MAX) {
    df_lags[[paste0(VAR_TEMP, "_lag", k)]] <- dplyr::lag(df_lags[[VAR_TEMP]], n = k)
  }
  df_lags <- df_lags |>
    dplyr::select(date, dplyr::all_of(DESFECHO),
                  dplyr::all_of(paste0(VAR_TEMP, "_lag", 0L:LAG_MAX))) |>
    tidyr::drop_na()

  attr(df_lags, "sus_meta") <- list(
    system  = "SIM",
    stage   = "climate",
    type    = "distributed_lag",
    backend = "tibble",
    history = paste0("[derivado] ", nome_mun, " - serie diaria saude+temperatura -> lags")
  )
  class(df_lags) <- c("climasus_df", class(df_lags))
  df_lags
}

dfs_lags <- lapply(seq_along(series_municipios), function(i) {
  .construir_df_lags(series_municipios[[i]], MUNICIPIOS[[i]])
})
names(dfs_lags) <- MUNICIPIOS


# =============================================================================
# BLOCO 3 - Ajustar DLNM individual por municipio (Estagio 1 do pooling)
# =============================================================================
message(">> Bloco 3: ajustando sus_mod_dlnm() por municipio (", length(MUNICIPIOS),
        " cidades)...")

fits_cidades <- NULL

if (tem_dlnm) {
  fits_cidades <- lapply(seq_along(MUNICIPIOS), function(i) {
    nm <- MUNICIPIOS[[i]]
    message("   Ajustando DLNM para: ", nm, " ...")
    tryCatch(
      sus_mod_dlnm(
        df           = dfs_lags[[nm]],
        outcome_col  = DESFECHO,
        climate_col  = VAR_TEMP,
        lag_max      = LAG_MAX,
        argvar       = list(fun = "ns", df = 4L),
        arglag       = list(fun = "ns", df = 3L),
        family       = "quasipoisson",
        dof_per_year = DOF_ANO,
        lang         = "pt",
        verbose      = FALSE
      ),
      error = function(e) {
        message("   DLNM falhou para ", nm, ": ", conditionMessage(e))
        NULL
      }
    )
  })
  names(fits_cidades) <- MUNICIPIOS
  # Remove nulos
  fits_cidades <- Filter(Negate(is.null), fits_cidades)
  message("   DLNM ajustado para ", length(fits_cidades), " de ",
          length(MUNICIPIOS), " cidades.")
} else {
  message("   dlnm/splines nao disponiveis: DLNM por cidade nao ajustado.")
}

# Salva lista de fits para reuso externo
if (!is.null(fits_cidades) && length(fits_cidades) >= 2L) {
  save_tbl(fits_cidades, "mod_fits_cidades")
}


# =============================================================================
# BLOCO 4 - sus_mod_pool(): meta-analise multivariada (REML, BLUPs)
# =============================================================================
# Estagio 2: pooling dos coeficientes do crossbasis de todas as cidades via
# mvmeta::mvmeta(method = "reml"). Produz curva agrupada, heterogeneidade I^2
# e BLUPs cidade-especificos encolhidos em direcao a media.
message(">> Bloco 4: sus_mod_pool() - meta-analise multivariada (REML)...")

mod_pool <- NULL

if (!is.null(fits_cidades) && length(fits_cidades) >= 2L && tem_mvmeta) {
  mod_pool <- tryCatch(
    sus_mod_pool(
      fits           = fits_cidades,
      exposure_range = NULL,
      n_grid         = 100L,
      pred_at        = c(0.75, 0.90, 0.95, 0.99),
      blup           = TRUE,
      method         = "reml",
      lang           = "pt",
      verbose        = TRUE
    ),
    error = function(e) {
      message("   sus_mod_pool() falhou: ", conditionMessage(e)); NULL
    }
  )
} else {
  message("   Pool nao executado: requer >= 2 fits validos e pacote mvmeta.")
}

if (!is.null(mod_pool)) {
  message("   Pool concluido. I^2 = ", mod_pool$heterogeneity$i2, "%")
} else {
  message("   AVISO: mod_pool NULL - artefatos de pooling nao serao gerados.")
}


# =============================================================================
# BLOCO 5 - Salvar mod_pool.rds (encadeamento do caso condutor)
# =============================================================================
message(">> Bloco 5: salvando mod_pool.rds para reuso...")

if (!is.null(mod_pool)) {
  # save_tbl usa vignettes-pt/dados/; salvar tambem na pasta dados/ da raiz
  save_tbl(mod_pool, "mod_pool")
  # Salvar copia em dados/ (pasta encadeamento convencional)
  dir.create(file.path("vignettes-pt", "dados"), showWarnings = FALSE, recursive = TRUE)
  saveRDS(mod_pool, file.path("vignettes-pt", "dados", "mod_pool.rds"))
  message("   mod_pool.rds salvo em vignettes-pt/dados/")
} else {
  message("   AVISO: mod_pool NULL - mod_pool.rds NAO gerado.")
}


# =============================================================================
# BLOCO 6 - Figuras do pool: overall / forest / spaghetti
# =============================================================================
message(">> Bloco 6: gerando figuras do sus_mod_plot_pool()...")

if (!is.null(mod_pool)) {

  # --- Figura 1: curva exposicao-resposta agrupada (overall) ---
  p_overall <- tryCatch(
    sus_mod_plot_pool(
      x     = mod_pool,
      type  = "overall",
      lang  = "pt"
    ) +
      ggplot2::labs(
        title    = "Curva Exposicao-Resposta Agrupada (RMSP, REML)",
        subtitle = paste0("Temperatura media x obitos respiratorios | ",
                          mod_pool$meta$n_cities, " municipios")
      ) +
      ggplot2::scale_color_manual(values = cf_colors) +
      tema_climasus(),
    error = function(e) {
      message("   plot_pool overall falhou: ", conditionMessage(e)); NULL
    }
  )
  if (!is.null(p_overall)) {
    save_fig(p_overall, "modpool_overall", width = 8, height = 5)
  }

  # --- Figura 2: forest plot das estimativas cidade-especificas ---
  p_forest <- tryCatch(
    sus_mod_plot_pool(
      x     = mod_pool,
      type  = "forest",
      lang  = "pt"
    ) +
      ggplot2::labs(
        title = "Estimativas RR por Municipio (p75 da Temperatura)",
        subtitle = "Diamante = estimativa original | Circulo = BLUP (encolhido)"
      ) +
      tema_climasus(),
    error = function(e) {
      message("   plot_pool forest falhou: ", conditionMessage(e)); NULL
    }
  )
  if (!is.null(p_forest)) {
    save_fig(p_forest, "modpool_forest", width = 8, height = 5)
  }

  # --- Figura 3: spaghetti (curvas BLUP por cidade + curva agrupada) ---
  p_spaghetti <- tryCatch(
    sus_mod_plot_pool(
      x     = mod_pool,
      type  = "spaghetti",
      lang  = "pt"
    ) +
      ggplot2::labs(
        title    = "Curvas BLUP por Municipio e Estimativa Agrupada",
        subtitle = "Cinza = cidade-especifico (BLUP) | Verde = agrupada (REML)"
      ) +
      tema_climasus(),
    error = function(e) {
      message("   plot_pool spaghetti falhou: ", conditionMessage(e)); NULL
    }
  )
  if (!is.null(p_spaghetti)) {
    save_fig(p_spaghetti, "modpool_spaghetti", width = 8, height = 5)
  }

} else {
  message("   AVISO: figuras do pool nao geradas (mod_pool e NULL).")
}


# =============================================================================
# BLOCO 7 - Tabelas do pool: heterogeneidade e RR por cidade
# =============================================================================
message(">> Bloco 7: tabelas do pool (heterogeneidade e RR por cidade)...")

if (!is.null(mod_pool)) {

  # Tabela 1: estatisticas de heterogeneidade
  tbl_het <- mod_pool$heterogeneity
  save_tbl(tbl_het, "modpool_heterogeneidade")

  # Tabela 2: estimativas cidade-especificas (raw + BLUP) no p75
  tbl_cidades <- mod_pool$city_table
  save_tbl(tbl_cidades, "modpool_cidade_rr")

  message("   Tabelas de heterogeneidade e RR-cidade salvas.")
} else {
  # Gera tabelas vazias para o Rmd poder renderizar sem quebrar
  tbl_het_vazia <- dplyr::tibble(Q = NA_real_, df = NA_integer_,
                                  p_value = NA_real_, i2 = NA_real_)
  save_tbl(tbl_het_vazia, "modpool_heterogeneidade")

  tbl_cidades_vazia <- dplyr::tibble(
    city    = MUNICIPIOS,
    n       = rep(NA_integer_, length(MUNICIPIOS)),
    rr      = rep(NA_real_, length(MUNICIPIOS)),
    lo      = rep(NA_real_, length(MUNICIPIOS)),
    hi      = rep(NA_real_, length(MUNICIPIOS)),
    blup_rr = rep(NA_real_, length(MUNICIPIOS)),
    blup_lo = rep(NA_real_, length(MUNICIPIOS)),
    blup_hi = rep(NA_real_, length(MUNICIPIOS))
  )
  save_tbl(tbl_cidades_vazia, "modpool_cidade_rr")
  message("   Tabelas vazias geradas (mod_pool NULL).")
}


# =============================================================================
# BLOCO 8 - sus_mod_metaregression(): meta-regressao com IDH
# =============================================================================
# Covariavel cidade-nivel: IDH (padronizado internamente pelo sus_mod_metaregression).
# O intercepto representa a associacao para um municipio medio da RMSP.
# Testa se a heterogeneidade no efeito da temperatura e explicada pelo IDH.
message(">> Bloco 8: sus_mod_metaregression() com covariavel IDH...")

mod_meta <- NULL

if (!is.null(fits_cidades) && length(fits_cidades) >= 2L && tem_mvmeta) {
  # Data.frame de covariavel com rownames = nomes de municipio
  cov_idh <- data.frame(
    idh = IDH_MUN[names(fits_cidades)],
    row.names = names(fits_cidades)
  )

  mod_meta <- tryCatch(
    sus_mod_metaregression(
      fits           = fits_cidades,
      covariates     = cov_idh,
      covariate_cols = "idh",
      city_col       = NULL,
      pred_at        = c(0.75, 0.90, 0.95, 0.99),
      blup           = TRUE,
      method         = "reml",
      alpha          = 0.05,
      lang           = "pt",
      verbose        = TRUE
    ),
    error = function(e) {
      message("   sus_mod_metaregression() falhou: ", conditionMessage(e)); NULL
    }
  )
  if (!is.null(mod_meta)) {
    message("   Meta-regressao concluida. R^2_het = ",
            mod_meta$heterogeneity[mod_meta$heterogeneity$model == "full", "r2_het"])
  }
} else {
  message("   Meta-regressao nao executada: requer fits validos e mvmeta.")
}


# =============================================================================
# BLOCO 9 - sus_mod_sensitivity(): sensibilidade por faixa etaria (60-74 vs 75+)
# =============================================================================
# Abordagem: ajustar DLNMs em series DISTINTAS por faixa etaria (simuladas aqui
# a partir da serie total com proporcoes realistas de mortalidade 60-74 / 75+).
# Em fluxo real as series viriam de caso_serie.rds estratificado por idade.
message(">> Bloco 9: sus_mod_sensitivity() por faixa etaria (60-74 vs 75+)...")

mod_sens <- NULL

if (tem_dlnm) {
  # Usa a serie de Sao Paulo como base para as duas faixas etarias
  serie_sp <- series_municipios[["Sao_Paulo"]]
  set.seed(99L)
  n_sp <- nrow(serie_sp)

  # 60-74 anos: ~55% dos obitos respiratorios de idosos
  serie_60_74 <- serie_sp |>
    dplyr::mutate(n_obitos = stats::rbinom(n_sp, size = n_obitos, prob = 0.55))

  # 75+ anos: ~45% dos obitos respiratorios de idosos
  serie_75mais <- serie_sp |>
    dplyr::mutate(n_obitos = n_obitos - serie_60_74$n_obitos)

  # Constroi dfs com lags para cada faixa
  df_lags_60_74  <- .construir_df_lags(serie_60_74,  "60_74_anos")
  df_lags_75mais <- .construir_df_lags(serie_75mais, "75_mais_anos")

  # Ajusta DLNM por faixa etaria
  fit_60_74 <- tryCatch(
    sus_mod_dlnm(
      df           = df_lags_60_74,
      outcome_col  = DESFECHO,
      climate_col  = VAR_TEMP,
      lag_max      = LAG_MAX,
      argvar       = list(fun = "ns", df = 4L),
      arglag       = list(fun = "ns", df = 3L),
      family       = "quasipoisson",
      dof_per_year = DOF_ANO,
      lang         = "pt",
      verbose      = FALSE
    ),
    error = function(e) { message("   DLNM 60-74 falhou: ", conditionMessage(e)); NULL }
  )

  fit_75mais <- tryCatch(
    sus_mod_dlnm(
      df           = df_lags_75mais,
      outcome_col  = DESFECHO,
      climate_col  = VAR_TEMP,
      lag_max      = LAG_MAX,
      argvar       = list(fun = "ns", df = 4L),
      arglag       = list(fun = "ns", df = 3L),
      family       = "quasipoisson",
      dof_per_year = DOF_ANO,
      lang         = "pt",
      verbose      = FALSE
    ),
    error = function(e) { message("   DLNM 75+ falhou: ", conditionMessage(e)); NULL }
  )

  if (!is.null(fit_60_74) && !is.null(fit_75mais)) {
    fits_faixas <- list(
      `60-74 anos` = fit_60_74,
      `75+ anos`   = fit_75mais
    )

    mod_sens <- tryCatch(
      sus_mod_sensitivity(
        fits            = fits_faixas,
        hot_percentile  = 0.99,
        cold_percentile = 0.01,
        stratum_labels  = NULL,
        alpha           = 0.05,
        lang            = "pt",
        verbose         = TRUE
      ),
      error = function(e) {
        message("   sus_mod_sensitivity() falhou: ", conditionMessage(e)); NULL
      }
    )
    if (!is.null(mod_sens)) {
      message("   Analise de sensibilidade concluida. Estrato mais sensivel: ",
              mod_sens$comparison$label[[1L]])
    }
  } else {
    message("   Sensibilidade nao executada: um ou ambos os DLNMs de faixa etaria falharam.")
  }
} else {
  message("   dlnm nao disponivel: sensibilidade por faixa etaria nao executada.")
}


# =============================================================================
# BLOCO 10 - Figuras de sensibilidade: curves e bar
# =============================================================================
message(">> Bloco 10: figuras sus_mod_plot_sensitivity()...")

if (!is.null(mod_sens)) {

  # --- Figura 4: curvas ERC sobrepostas por faixa etaria ---
  p_sens_curves <- tryCatch(
    sus_mod_plot_sensitivity(
      x    = mod_sens,
      type = "curves",
      lang = "pt"
    ) +
      ggplot2::labs(
        title    = "Curvas Exposicao-Resposta por Faixa Etaria (DLNM)",
        subtitle = "Sao Paulo, 2014-2019 | obitos respiratorios (J00-J99)"
      ) +
      ggplot2::scale_color_manual(values = c(COR_PRIMARIA, COR_ACENTO)) +
      ggplot2::scale_fill_manual(values  = c(COR_PRIMARIA, COR_ACENTO)) +
      tema_climasus(),
    error = function(e) {
      message("   plot_sensitivity curves falhou: ", conditionMessage(e)); NULL
    }
  )
  if (!is.null(p_sens_curves)) {
    save_fig(p_sens_curves, "modpool_sensitivity_curves", width = 8, height = 5)
  }

  # --- Figura 5: dispersao RR calor vs RR frio (scatter) ---
  p_sens_scatter <- tryCatch(
    sus_mod_plot_sensitivity(
      x    = mod_sens,
      type = "scatter",
      lang = "pt"
    ) +
      ggplot2::labs(
        title    = "RR Calor vs. RR Frio por Faixa Etaria",
        subtitle = "Tamanho do ponto = Indice de Sensibilidade (SI)"
      ) +
      tema_climasus(),
    error = function(e) {
      message("   plot_sensitivity scatter falhou: ", conditionMessage(e)); NULL
    }
  )
  if (!is.null(p_sens_scatter)) {
    save_fig(p_sens_scatter, "modpool_sensitivity_scatter", width = 8, height = 5)
  }

  # --- Figura 6: forest RR calor/frio por faixa etaria ---
  p_sens_bar <- tryCatch(
    sus_mod_plot_sensitivity(
      x    = mod_sens,
      type = "bar",
      lang = "pt"
    ) +
      ggplot2::labs(
        title    = "RR Calor e Frio por Faixa Etaria (p99 e p01)",
        subtitle = "Ordenado pelo Indice de Sensibilidade Combinado"
      ) +
      tema_climasus(),
    error = function(e) {
      message("   plot_sensitivity bar falhou: ", conditionMessage(e)); NULL
    }
  )
  if (!is.null(p_sens_bar)) {
    save_fig(p_sens_bar, "modpool_sensitivity_bar", width = 8, height = 5)
  }

} else {
  message("   AVISO: figuras de sensibilidade nao geradas (mod_sens e NULL).")
}


# =============================================================================
# BLOCO 11 - Encadeamento: salvar mod_pool.rds definitivo
# =============================================================================
message(">> Bloco 11: salvando mod_pool.rds (encadeamento do caso condutor)...")

if (!is.null(mod_pool)) {
  path_out <- file.path("vignettes-pt", "dados", "mod_pool.rds")
  saveRDS(mod_pool, path_out)
  message("   mod_pool.rds salvo em: ", path_out)
} else {
  message("   AVISO: mod_pool NULL - mod_pool.rds NAO gerado para encadeamento.")
}


# =============================================================================
# FIM
# =============================================================================
message("== Concluido. Artefatos em vignettes-pt/figuras/ e vignettes-pt/dados/ ==")
message("   Prefixo: modpool_")
message("   Encadeamento: vignettes-pt/dados/mod_pool.rds")
