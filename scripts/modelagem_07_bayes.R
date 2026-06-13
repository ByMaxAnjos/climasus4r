# =============================================================================
# modelagem_07_bayes.R
# -----------------------------------------------------------------------------
# PROPOSITO
#   Gerar UMA VEZ, localmente, todas as figuras e tabelas canonicas usadas pelo
#   Tutorial de Modelagem 07 (Modelos Bayesianos Espaciais e Espaco-Temporais)
#   da vignette climasus4r.
#   Esta etapa cobre o mapeamento Bayesiano de risco e a modelagem espaco-
#   temporal hierarquica com INLA:
#     - sus_mod_spatial_bayes()         BYM / Leroux / BYM2 (CARBayes ou INLA)
#     - sus_mod_plot_spatial_bayes()    Mapa RR, incerteza, coeficientes
#     - sus_mod_spacetime_bayes()       BYM2 + RW2 + interacao Knorr-Held
#     - sus_mod_plot_spacetime()        Mapa RR, temporal, interacao, excedencia
#     - sus_mod_spacetime_exceedance()  P(RR > limiar) por celula espaco-temporal
#     - sus_mod_spacetime_predict()     Previsao aproximada via preditor linear
#
#   A analise pesada (MCMC / INLA) roda AQUI; o .Rmd apenas carrega os
#   artefatos prontos (PNG via include_graphics; RDS via readRDS).
#
# COMO RODAR
#   Rode a partir da RAIZ do pacote (pasta climasus4r/):
#       Rscript scripts/modelagem_07_bayes.R
#   ou, em sessao interativa com o working directory na raiz:
#       source("scripts/modelagem_07_bayes.R")
#
# ARTEFATOS GERADOS  (prefixo "modbayes_" para espacial; "modst_" para espaco-temporal)
#   Figuras (vignettes-pt/figuras/):
#     - modbayes_rr_bym.png           Mapa coropletico RR (modelo BYM cross-section)
#     - modbayes_incerteza_bym.png    Mapa largura IC95% (incerteza posterior BYM)
#     - modbayes_coef_bym.png         Forest plot coeficientes fixos BYM
#     - modst_temporal.png            Tendencia temporal RR (modelo espaco-temporal)
#     - modst_rr_map.png              Mapa RR agregado espaco-temporal
#     - modbayes_excedencia.png       Mapa P(RR > 1.5) por municipio
#     - modbayes_predicao.png         Previsao out-of-sample (horizon = 2)
#   Tabelas (vignettes-pt/dados/):
#     - modbayes_resumo_fixos.rds     Efeitos fixos do modelo espacial BYM
#     - modbayes_n_excedencia.rds     Contagem de celulas com P(RR > t) > 0.80
#   Encadeamento (reuso nos tutoriais seguintes):
#     - mod_spacetime_fit.rds         Objeto climasus_spacetime_bayes ajustado
#
# ENTRADAS
#   dados/caso_espacial.rds   sf de municipios da RMSP (code_muni + n_obitos)
#   dados/mod_weights.rds     climasus_weights (pesos Queen, gerado no Tutorial 06)
#   dados/caso_serie.rds      serie diaria de obitos por municipio (opcional)
#   Se ausentes, sintetizamos um painel minimo realista da RMSP com aviso.
#
# CASO CONDUTOR
#   "Temperatura e desfechos de saude em idosos (60+) na RM de Sao Paulo,
#   2014-2019". Neste tutorial:
#     1) Ajustamos um modelo BYM cross-seccional para o mapa de risco medio;
#     2) Construimos um painel espaco-temporal (area x ano) e ajustamos o modelo
#        BYM2 + RW2 com interacao tipo I de Knorr-Held;
#     3) Calculamos probabilidades de excedencia P(RR > 1.0, 1.5, 2.0);
#     4) Fazemos uma previsao aproximada de 2 periodos a frente.
#
# OBS.: TODA chamada a pacote Suggests (CARBayes, INLA, spdep, sf, ggplot2,
#       patchwork) e protegida por requireNamespace()/tryCatch e degrada
#       graciosamente. O script nao instala pacotes automaticamente.
# =============================================================================

# --- Infraestrutura compartilhada -------------------------------------------
source("scripts/_setup_tutoriais.R")

COR_PRIMARIA   <- "#2E7D32"
COR_SECUNDARIA <- "#558B2F"
COR_ACENTO     <- "#EF6C00"

RMSP_CODES <- c(
  3503901L, 3505708L, 3506607L, 3509502L, 3510609L,
  3513009L, 3513801L, 3515004L, 3515103L, 3516200L,
  3516309L, 3518800L, 3519055L, 3522208L, 3522505L,
  3523107L, 3523800L, 3524709L, 3526902L, 3529401L,
  3530607L, 3534401L, 3539103L, 3539806L, 3543303L,
  3544103L, 3545001L, 3546801L, 3547304L, 3547809L,
  3548708L, 3548807L, 3549953L, 3552502L, 3552809L,
  3554102L, 3556453L, 3557204L, 3558808L
)

RMSP_NAMES <- c(
  "Aruja", "Barueri", "Biritiba-Mirim", "Caieiras", "Cajamar",
  "Carapicuiba", "Cotia", "Diadema", "Embu das Artes", "Embu-Guacu",
  "Ferraz de Vasconcelos", "Francisco Morato", "Franco da Rocha",
  "Guararema", "Guarulhos", "Itapecerica da Serra", "Itapevi",
  "Itaquaquecetuba", "Jandira", "Juquitiba", "Mairipora", "Maua",
  "Mogi das Cruzes", "Osasco", "Pirapora do Bom Jesus", "Poa",
  "Ribeiro Pires", "Salesopolis", "Santa Isabel", "Santana de Parnaiba",
  "Sao Bernardo do Campo", "Sao Caetano do Sul", "Sao Lourenco da Serra",
  "Sao Paulo", "Sao Paulo Ext", "Suzano", "Taboao da Serra",
  "Vargem Grande Paulista", "Embu das Artes 2"
)

N_ANOS  <- 6L   # 2014-2019
N_MUNI  <- length(RMSP_CODES)


# =============================================================================
# BLOCO 0 - Carregar / sintetizar dados de entrada
# =============================================================================
message(">> Bloco 0: carregando dados de entrada (caso_espacial + mod_weights)...")

# --- 0a. sf de municipios (cross-section) -----------------------------------
rmsp_sf <- NULL

if (file.exists("vignettes-pt/dados/caso_espacial.rds")) {
  obj_esp <- readRDS("vignettes-pt/dados/caso_espacial.rds")
  if (requireNamespace("sf", quietly = TRUE) &&
      "geom" %in% names(obj_esp) &&
      inherits(obj_esp[["geom"]], "sfc")) {
    rmsp_sf <- sf::st_as_sf(obj_esp)
    # Agregar: media do periodo por municipio para analise transversal
    rmsp_sf <- dplyr::group_by(rmsp_sf, code_muni) |>
      dplyr::summarise(
        n_obitos  = mean(n_obitos, na.rm = TRUE),
        name_muni = dplyr::first(name_muni),
        .groups = "drop"
      )
    message("   caso_espacial.rds carregado: ", nrow(rmsp_sf), " municipios.")
  }
}

if (is.null(rmsp_sf) || nrow(rmsp_sf) < 10L) {
  message("   [AVISO] caso_espacial.rds ausente ou com municipios insuficientes (< 10). ",
          "Sintetizando painel RMSP minimo para fins didaticos.")

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Pacote 'sf' necessario. Instale com: install.packages('sf')")
  }

  set.seed(2024L)
  pop_sim     <- round(runif(N_MUNI, 5e4, 2e6))
  taxa_base   <- 8 + 0.8 * seq_len(N_MUNI) / N_MUNI
  taxa_base[1:8] <- taxa_base[1:8] * 1.6
  n_obs_sim   <- round(pop_sim * taxa_base / 1e4 * N_ANOS)
  temp_media  <- 20 + rnorm(N_MUNI, sd = 1.5)
  temp_media[1:8] <- temp_media[1:8] + 2.5

  lon_base <- -46.9; lat_base <- -24.0; d <- 0.12
  nx <- 7L
  grid_list <- vector("list", N_MUNI)
  for (i in seq_len(N_MUNI)) {
    ix <- (i - 1L) %% nx
    iy <- (i - 1L) %/% nx
    xmin <- lon_base + ix * d; xmax <- xmin + d * 0.9
    ymin <- lat_base - iy * d; ymax <- ymin - d * 0.9
    grid_list[[i]] <- sf::st_polygon(list(rbind(
      c(xmin, ymin), c(xmax, ymin), c(xmax, ymax),
      c(xmin, ymax), c(xmin, ymin)
    )))
  }

  rmsp_sf <- sf::st_sf(
    code_muni = as.character(RMSP_CODES),
    name_muni = RMSP_NAMES,
    n_obitos  = n_obs_sim,
    pop       = pop_sim,
    temp_med  = temp_media,
    geometry  = sf::st_sfc(grid_list, crs = 4674L)
  )
  message("   Painel sintetico RMSP gerado: ", nrow(rmsp_sf), " municipios.")
}

# Garantir code_muni como character
rmsp_sf$code_muni <- as.character(rmsp_sf$code_muni)

# Adicionar variaveis auxiliares se ausentes
if (!"pop" %in% names(rmsp_sf)) {
  set.seed(42L)
  rmsp_sf$pop <- round(runif(nrow(rmsp_sf), 5e4, 2e6))
}
if (!"temp_med" %in% names(rmsp_sf)) {
  set.seed(43L)
  rmsp_sf$temp_med <- round(rnorm(nrow(rmsp_sf), mean = 21, sd = 1.5), 1)
}

# Esperados pelo offset: proporcional a populacao (SMR)
rmsp_sf$expected <- with(sf::st_drop_geometry(rmsp_sf),
                         pop / sum(pop) * sum(n_obitos))
rmsp_sf$n_obitos <- as.integer(round(rmsp_sf$n_obitos))

n_muni_rmsp <- nrow(rmsp_sf)
message("   Municipios disponiveis: ", n_muni_rmsp)


# --- 0b. Pesos espaciais (mod_weights.rds) -----------------------------------
mod_weights <- NULL

if (file.exists("vignettes-pt/dados/mod_weights.rds")) {
  mod_weights <- readRDS("vignettes-pt/dados/mod_weights.rds")
  message("   mod_weights.rds carregado.")
} else {
  message("   [AVISO] mod_weights.rds nao encontrado. ",
          "Tentando construir via sus_mod_spatial_weights()...")
  mod_weights <- tryCatch({
    sus_mod_spatial_weights(
      municipalities = rmsp_sf,
      style          = "W",
      queen          = TRUE,
      snap           = 1e-3,
      zero_policy    = TRUE,
      return_matrix  = FALSE,
      lang           = "pt",
      verbose        = TRUE
    )
  }, error = function(e) {
    message("   [AVISO] sus_mod_spatial_weights falhou: ", conditionMessage(e))
    NULL
  })
  if (!is.null(mod_weights)) {
    save_tbl(mod_weights, "mod_weights")
    message("   mod_weights.rds salvo como fallback.")
  }
}

if (is.null(mod_weights)) {
  stop("Objeto de pesos espaciais nao disponivel. Execute o Tutorial 06 primeiro.")
}


# =============================================================================
# BLOCO 1 - Modelo Bayesiano Espacial Cross-seccional (BYM via CARBayes)
# =============================================================================
message("\n>> Bloco 1: ajustando modelo BYM cross-seccional (CARBayes)...")

# Dados tabulares (sem geometria) para sus_mod_spatial_bayes
df_cross <- sf::st_drop_geometry(rmsp_sf)

fit_bym <- NULL

fit_bym <- tryCatch({
  sus_mod_spatial_bayes(
    df         = df_cross,
    outcome    = "n_obitos",
    W          = mod_weights,
    covariates = "temp_med",
    offset     = "expected",
    family     = "poisson",
    model      = "bym",
    n_iter     = 10000L,
    burnin     = 2000L,
    thin       = 10L,
    prior_tau2 = c(1, 0.01),
    seed       = 42L,
    lang       = "pt",
    verbose    = TRUE
  )
}, error = function(e) {
  message("   [AVISO] sus_mod_spatial_bayes (BYM) falhou: ", conditionMessage(e))
  NULL
})

if (!is.null(fit_bym)) {
  print(fit_bym)

  # Salvar tabela de efeitos fixos
  save_tbl(fit_bym$fixed, "modbayes_resumo_fixos")

  # --- Figura 1: Mapa de Risco Relativo (BYM) --------------------------------
  p_rr_bym <- tryCatch({
    sus_mod_plot_spatial_bayes(
      x              = fit_bym,
      municipalities = rmsp_sf,
      type           = "rr",
      lang           = "pt"
    )
  }, error = function(e) {
    message("   [AVISO] plot RR BYM falhou: ", conditionMessage(e)); NULL
  })

  if (!is.null(p_rr_bym)) {
    p_rr_bym <- p_rr_bym +
      ggplot2::labs(subtitle = "Modelo BYM | Poisson | RMSP, 2014-2019") +
      tema_climasus()
    save_fig(p_rr_bym, "modbayes_rr_bym", width = 8, height = 6)
  }

  # --- Figura 2: Mapa de Incerteza (largura IC95%) ----------------------------
  p_inc_bym <- tryCatch({
    sus_mod_plot_spatial_bayes(
      x              = fit_bym,
      municipalities = rmsp_sf,
      type           = "uncertainty",
      lang           = "pt"
    )
  }, error = function(e) {
    message("   [AVISO] plot incerteza BYM falhou: ", conditionMessage(e)); NULL
  })

  if (!is.null(p_inc_bym)) {
    save_fig(p_inc_bym, "modbayes_incerteza_bym", width = 8, height = 6)
  }

  # --- Figura 3: Forest plot de coeficientes fixos ----------------------------
  p_coef_bym <- tryCatch({
    sus_mod_plot_spatial_bayes(
      x    = fit_bym,
      type = "coef",
      lang = "pt"
    )
  }, error = function(e) {
    message("   [AVISO] plot coef BYM falhou: ", conditionMessage(e)); NULL
  })

  if (!is.null(p_coef_bym)) {
    p_coef_bym <- p_coef_bym + tema_climasus()
    save_fig(p_coef_bym, "modbayes_coef_bym", width = 7, height = 4)
  }

} else {
  message("   Modelo BYM nao ajustado. Figuras 1-3 nao geradas.")
}


# =============================================================================
# BLOCO 2 - Construir painel espaco-temporal (area x ano, 2014-2019)
# =============================================================================
message("\n>> Bloco 2: construindo painel espaco-temporal (municipio x ano)...")

# Tentar carregar serie diaria (caso_serie.rds)
painel_st <- NULL

if (file.exists("vignettes-pt/dados/caso_serie.rds")) {
  serie_diaria <- readRDS("vignettes-pt/dados/caso_serie.rds")
  # Agregar: sum de n_obitos por (code_muni, ano)
  if (all(c("code_muni", "n_obitos") %in% names(serie_diaria))) {
    if ("date" %in% names(serie_diaria)) {
      serie_diaria$date <- as.Date(serie_diaria$date)
      serie_diaria$ano  <- as.integer(format(serie_diaria$date, "%Y"))
    } else if ("ano" %in% names(serie_diaria)) {
      serie_diaria$ano <- as.integer(serie_diaria$ano)
    }
    if ("ano" %in% names(serie_diaria)) {
      painel_st <- dplyr::group_by(serie_diaria, code_muni, ano) |>
        dplyr::summarise(
          n_obitos = sum(n_obitos, na.rm = TRUE),
          .groups  = "drop"
        )
      message("   Painel espaco-temporal construido a partir de caso_serie.rds: ",
              nrow(painel_st), " linhas.")
    }
  }
}

# Fallback sintetico: painel area x ano com sazionalidade e heterogeneidade
if (is.null(painel_st) || nrow(painel_st) < N_MUNI * 2L) {
  message("   [AVISO] caso_serie.rds ausente ou insuficiente. ",
          "Sintetizando painel espaco-temporal minimo com aviso.")

  set.seed(2025L)
  anos_v    <- 2014:2019

  painel_rows <- lapply(seq_len(N_MUNI), function(i) {
    muni_code  <- as.character(RMSP_CODES[i])
    pop_i      <- rmsp_sf$pop[rmsp_sf$code_muni == muni_code]
    if (length(pop_i) == 0) pop_i <- 5e5
    temp_med_i <- rmsp_sf$temp_med[rmsp_sf$code_muni == muni_code]
    if (length(temp_med_i) == 0) temp_med_i <- rnorm(1, 21, 1.5)
    exp_i      <- pop_i / sum(rmsp_sf$pop) * sum(rmsp_sf$n_obitos)

    # Risco relativo latente: cluster espacial nos primeiros 8 municipios
    rr_spat <- if (i <= 8L) exp(rnorm(1, 0.4, 0.1)) else exp(rnorm(1, 0, 0.1))

    lapply(seq_along(anos_v), function(j) {
      ano_j <- anos_v[j]
      # Tendencia temporal crescente
      rr_temp <- exp(0.02 * (j - 1))
      lambda  <- exp_i * rr_spat * rr_temp * exp(rnorm(1, 0, 0.05))
      data.frame(
        code_muni = muni_code,
        ano       = ano_j,
        n_obitos  = as.integer(max(0L, round(rpois(1, lambda)))),
        pop       = as.integer(pop_i),
        temp_med  = temp_med_i + rnorm(1, 0, 0.3),
        stringsAsFactors = FALSE
      )
    })
  })

  painel_st <- do.call(rbind, do.call(c, painel_rows))
  row.names(painel_st) <- NULL
  message("   Painel sintetico gerado: ", nrow(painel_st), " linhas (",
          N_MUNI, " municipios x ", length(anos_v), " anos).")
}

# Adicionar expected (offset) proporcional a populacao se ausente
if (!"pop" %in% names(painel_st)) {
  painel_st <- dplyr::left_join(
    painel_st,
    sf::st_drop_geometry(rmsp_sf[, c("code_muni", "pop")]),
    by = "code_muni"
  )
}
if (!"expected" %in% names(painel_st)) {
  pop_total <- sum(painel_st$pop, na.rm = TRUE)
  obs_total <- sum(painel_st$n_obitos, na.rm = TRUE)
  painel_st$expected <- painel_st$pop / pop_total * obs_total
  painel_st$expected <- pmax(painel_st$expected, 0.001)
}

# Adicionar temp_med se ausente
if (!"temp_med" %in% names(painel_st)) {
  painel_st <- dplyr::left_join(
    painel_st,
    sf::st_drop_geometry(rmsp_sf[, c("code_muni", "temp_med")]),
    by = "code_muni"
  )
}

# Garantir code_muni como character e ano como integer
painel_st$code_muni <- as.character(painel_st$code_muni)
painel_st$ano       <- as.integer(painel_st$ano)
painel_st$n_obitos  <- as.integer(painel_st$n_obitos)

message("   Painel final: ", nrow(painel_st), " linhas | ",
        length(unique(painel_st$code_muni)), " municipios | ",
        length(unique(painel_st$ano)), " anos.")


# =============================================================================
# BLOCO 3 - Modelo Bayesiano Espaco-Temporal (BYM2 + RW2 + Interacao I)
# =============================================================================
message("\n>> Bloco 3: ajustando modelo espaco-temporal (BYM2 + RW2, INLA)...")

fit_st <- NULL

fit_st <- tryCatch({
  if (!requireNamespace("INLA", quietly = TRUE)) {
    message("   [AVISO] Pacote INLA nao instalado. ",
            "Instale com: install.packages('INLA', ",
            "repos='https://inla.r-inla-download.org/R/stable')")
    NULL
  } else {
    sus_mod_spacetime_bayes(
      df               = painel_st,
      outcome          = "n_obitos",
      W                = mod_weights,
      time_col         = "ano",
      time_unit        = "year",
      covariates       = "temp_med",
      offset           = "expected",
      family           = "poisson",
      spatial_model    = "bym2",
      temporal_model   = "rw2",
      interaction_type = "I",
      pc_prior_u       = 0.5,
      pc_prior_alpha   = 0.01,
      compute_waic     = TRUE,
      compute_cpo      = FALSE,
      exceedance_threshold = 1.0,
      n_samples        = 1000L,
      seed             = 42L,
      lang             = "pt",
      verbose          = TRUE
    )
  }
}, error = function(e) {
  message("   [AVISO] sus_mod_spacetime_bayes falhou: ", conditionMessage(e))
  NULL
})

# Salvar o objeto ajustado para reuso
if (!is.null(fit_st)) {
  print(fit_st)
  save_tbl(fit_st, "mod_spacetime_fit")
  message("   mod_spacetime_fit.rds salvo.")
} else {
  message("   Modelo espaco-temporal nao ajustado. ",
          "Figuras do Bloco 3 nao serao geradas.")
}


# =============================================================================
# BLOCO 4 - Visualizacoes do modelo espaco-temporal
# =============================================================================
message("\n>> Bloco 4: gerando visualizacoes do modelo espaco-temporal...")

if (!is.null(fit_st)) {

  # --- Figura 4: Tendencia temporal do RR ------------------------------------
  p_temporal <- tryCatch({
    sus_mod_plot_spacetime(
      x    = fit_st,
      type = "temporal",
      lang = "pt"
    )
  }, error = function(e) {
    message("   [AVISO] sus_mod_plot_spacetime temporal falhou: ", conditionMessage(e))
    NULL
  })

  if (!is.null(p_temporal)) {
    p_temporal <- p_temporal + tema_climasus()
    save_fig(p_temporal, "modst_temporal", width = 8, height = 5)
  }

  # --- Figura 5: Mapa coropletico RR (media temporal) ------------------------
  p_rr_map_st <- tryCatch({
    sus_mod_plot_spacetime(
      x              = fit_st,
      type           = "rr_map",
      municipalities = rmsp_sf,
      lang           = "pt"
    )
  }, error = function(e) {
    message("   [AVISO] sus_mod_plot_spacetime rr_map falhou: ", conditionMessage(e))
    NULL
  })

  if (!is.null(p_rr_map_st)) {
    save_fig(p_rr_map_st, "modst_rr_map", width = 8, height = 6)
  }

  # --- Figura 6: Coeficientes fixos (forest plot) ----------------------------
  p_coef_st <- tryCatch({
    sus_mod_plot_spacetime(
      x    = fit_st,
      type = "coef",
      lang = "pt"
    )
  }, error = function(e) {
    message("   [AVISO] sus_mod_plot_spacetime coef falhou: ", conditionMessage(e))
    NULL
  })

  if (!is.null(p_coef_st)) {
    p_coef_st <- p_coef_st + tema_climasus()
    save_fig(p_coef_st, "modst_coef", width = 7, height = 4)
  }

} else {
  message("   Figuras espaco-temporais nao geradas (modelo ausente).")
}


# =============================================================================
# BLOCO 5 - Probabilidades de Excedencia P(RR > t)
# =============================================================================
message("\n>> Bloco 5: calculando probabilidades de excedencia P(RR > t)...")

exc_resultado <- NULL

if (!is.null(fit_st)) {
  exc_resultado <- tryCatch({
    sus_mod_spacetime_exceedance(
      fit        = fit_st,
      thresholds = c(1.0, 1.5, 2.0),
      lang       = "pt",
      verbose    = TRUE
    )
  }, error = function(e) {
    message("   [AVISO] sus_mod_spacetime_exceedance falhou: ", conditionMessage(e))
    NULL
  })

  if (!is.null(exc_resultado)) {
    print(exc_resultado)

    # Salvar contagem de celulas acima do limiar
    save_tbl(exc_resultado$n_exceed, "modbayes_n_excedencia")

    # --- Figura 7: Mapa P(RR > 1.5) por municipio (media temporal) ------------
    # Agregar a media de p_gt_1_5 por municipio
    exc_df_muni <- NULL
    if ("p_gt_1_5" %in% names(exc_resultado$exceedance)) {
      exc_df_muni <- dplyr::group_by(exc_resultado$exceedance, code_muni) |>
        dplyr::summarise(
          p_gt_1_5_media = mean(p_gt_1_5, na.rm = TRUE),
          .groups = "drop"
        )
    } else if ("p_gt_1_0" %in% names(exc_resultado$exceedance)) {
      exc_df_muni <- dplyr::group_by(exc_resultado$exceedance, code_muni) |>
        dplyr::summarise(
          p_gt_1_0_media = mean(p_gt_1_0, na.rm = TRUE),
          .groups = "drop"
        )
    }

    if (!is.null(exc_df_muni)) {
      exc_sf <- tryCatch({
        col_p <- names(exc_df_muni)[2]
        exc_sf_tmp <- dplyr::left_join(
          rmsp_sf,
          exc_df_muni,
          by = "code_muni"
        )
        p_exc <- ggplot2::ggplot(exc_sf_tmp) +
          ggplot2::geom_sf(ggplot2::aes(fill = .data[[col_p]]),
                           color = "grey80", linewidth = 0.15) +
          ggplot2::scale_fill_viridis_c(
            option   = "inferno",
            na.value = "grey90",
            name     = "P(RR>limiar)"
          ) +
          ggplot2::labs(
            title    = "Probabilidade de Excedencia P(RR > limiar)",
            subtitle = "Media temporal por municipio | RMSP, 2014-2019",
            caption  = "Fonte: climasus4r / sus_mod_spacetime_exceedance()"
          ) +
          ggplot2::theme_void(base_size = 12) +
          ggplot2::theme(
            legend.position   = "right",
            plot.title        = ggplot2::element_text(face = "bold", hjust = 0.5),
            plot.subtitle     = ggplot2::element_text(hjust = 0.5,
                                                      color = COR_SECUNDARIA)
          )
        p_exc
      }, error = function(e) {
        message("   [AVISO] plot excedencia falhou: ", conditionMessage(e))
        NULL
      })

      if (!is.null(exc_sf)) {
        save_fig(exc_sf, "modbayes_excedencia", width = 8, height = 6)
      }
    }

  } else {
    message("   Excedencia nao calculada. Figura 7 nao gerada.")
  }
}


# =============================================================================
# BLOCO 6 - Previsao aproximada (horizon = 2 periodos)
# =============================================================================
message("\n>> Bloco 6: gerando previsao aproximada (horizon = 2 periodos)...")

pred_resultado <- NULL

if (!is.null(fit_st)) {
  pred_resultado <- tryCatch({
    sus_mod_spacetime_predict(
      fit        = fit_st,
      newdata    = NULL,
      horizon    = 2L,
      include_ci = TRUE,
      lang       = "pt",
      verbose    = TRUE
    )
  }, error = function(e) {
    message("   [AVISO] sus_mod_spacetime_predict falhou: ", conditionMessage(e))
    NULL
  })

  if (!is.null(pred_resultado)) {
    print(pred_resultado)

    # --- Figura 8: Grafico de previsao por periodo ---------------------------
    p_pred <- tryCatch({
      pred_df <- pred_resultado$predictions
      # Resumir por periodo (media sobre municipios)
      pred_sum <- dplyr::group_by(pred_df, time_idx) |>
        dplyr::summarise(
          pred_mean    = mean(pred_mean,    na.rm = TRUE),
          pred_lower95 = mean(pred_lower95, na.rm = TRUE),
          pred_upper95 = mean(pred_upper95, na.rm = TRUE),
          .groups = "drop"
        )

      ggplot2::ggplot(pred_sum,
                      ggplot2::aes(x = time_idx, y = pred_mean)) +
        ggplot2::geom_ribbon(
          ggplot2::aes(ymin = pred_lower95, ymax = pred_upper95),
          fill = COR_PRIMARIA, alpha = 0.25
        ) +
        ggplot2::geom_line(color = COR_PRIMARIA, linewidth = 1.1) +
        ggplot2::geom_point(color = COR_ACENTO, size = 2.5) +
        ggplot2::labs(
          title    = "Previsao: Contagem Media Prevista por Periodo",
          subtitle = paste0("Horizonte = ", pred_resultado$horizon,
                            " periodos | Preditor linear aproximado"),
          x = "Indice de Tempo (periodo)",
          y = "Contagem prevista (media RMSP)",
          caption = "Fonte: climasus4r / sus_mod_spacetime_predict()"
        ) +
        tema_climasus()
    }, error = function(e) {
      message("   [AVISO] plot previsao falhou: ", conditionMessage(e))
      NULL
    })

    if (!is.null(p_pred)) {
      save_fig(p_pred, "modbayes_predicao", width = 8, height = 5)
    }
  }
}


# =============================================================================
# BLOCO 7 - Sumario final e checagem de artefatos
# =============================================================================
message("\n>> Bloco 7: checagem dos artefatos gerados...")

artefatos_fig <- c(
  "modbayes_rr_bym", "modbayes_incerteza_bym", "modbayes_coef_bym",
  "modst_temporal", "modst_rr_map", "modst_coef",
  "modbayes_excedencia", "modbayes_predicao"
)

artefatos_tbl <- c(
  "modbayes_resumo_fixos", "modbayes_n_excedencia", "mod_spacetime_fit"
)

for (nm in artefatos_fig) {
  path <- file.path("vignettes-pt", "figuras", paste0(nm, ".png"))
  status <- if (file.exists(path)) "OK" else "AUSENTE"
  message("   [fig] ", nm, ".png -> ", status)
}

for (nm in artefatos_tbl) {
  path <- file.path("vignettes-pt", "dados", paste0(nm, ".rds"))
  status <- if (file.exists(path)) "OK" else "AUSENTE"
  message("   [tbl] ", nm, ".rds -> ", status)
}

message("\n>> modelagem_07_bayes.R concluido.")
