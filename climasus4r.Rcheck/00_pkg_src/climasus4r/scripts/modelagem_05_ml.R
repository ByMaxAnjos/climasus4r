  # =============================================================================
  # modelagem_05_ml.R
  # -----------------------------------------------------------------------------
  # PROPOSITO
  #   Gerar UMA VEZ, localmente, todas as figuras e tabelas canonicas usadas pelo
  #   Tutorial de Modelagem 05 (Machine Learning — XGBoost) da vignette climasus4r.
  #   Este script cobre a familia sus_mod_ml / sus_mod_plot_ml:
  #     - sus_mod_ml()           XGBoost (count:poisson), CV agrupada por municipio,
  #                              early stopping, importancia de variaveis
  #     - sus_mod_plot_ml()      Graficos: importance / fit / cv_log
  #   A analise pesada (treinamento XGBoost, validacao cruzada) roda AQUI; o .Rmd
  #   apenas carrega os artefatos prontos (PNG via include_graphics, RDS via readRDS).
  #
  # COMO RODAR
  #   Rode a partir da RAIZ do pacote (pasta climasus4r/):
  #       Rscript scripts/modelagem_05_ml.R
  #   ou, em sessao interativa com o working directory na raiz:
  #       source("scripts/modelagem_05_ml.R")
  #
  # ARTEFATOS GERADOS  (prefixo "modml_")
  #   Figuras (vignettes-pt/figuras/):
  #     - modml_importance.png     importancia das variaveis (Gain) -- top 15
  #     - modml_fit.png            observado vs. predito CV fora da amostra (R2, RMSE)
  #     - modml_cv_log.png         curva de aprendizado por rodada (treino vs. CV)
  #     - modml_features_ts.png    serie temporal dos top-3 features (contexto)
  #     - modml_performance.png    barras de metricas de desempenho (RMSE/MAE treino vs. CV)
  #   Tabelas (vignettes-pt/dados/):
  #     - modml_resumo.rds         metricas de desempenho (RMSE/MAE/R2/Pearson, treino x CV)
  #     - modml_importance_tbl.rds tabela de importancia das variaveis (Feature, Gain, ...)
  #   Encadeamento (caso condutor):
  #     - mod_ml_fit.rds           objeto climasus_ml ajustado, entrada para relatorios/RAPs
  #
  # CASO CONDUTOR
  #   "Temperatura e desfechos de saude em idosos (60+) na Regiao Metropolitana de
  #   Sao Paulo, 2014-2019" (respiratorias J00-J99).
  #   Previsao da contagem diaria de obitos a partir de features climaticas (temperatura,
  #   umidade, pressao), de calendario (dia da semana, mes, tendencia) e socioespaciais
  #   (municipio como fator). Validacao cruzada agrupada por municipio (id_col) para
  #   evitar vazamento de dados entre treino e validacao.
  #
  # ENTRADAS
  #   dados/caso_serie.rds            (serie diaria de obitos, do pipeline principal)
  #   dados/caso_clima_estacao.rds    (temperatura/umidade diaria por municipio, INMET)
  #   dados/caso_socio.rds            (dados socioeconomicos para features adicionais)
  #   Se ausentes, gera serie SINTETICA MINIMA REALISTA com sinal temperatura-saude.
  #
  # DEPENDENCIAS PESADAS (Suggests)
  #   xgboost -- OBRIGATORIO para sus_mod_ml(); verificado com requireNamespace().
  #   Todos os demais pacotes Suggests (dlnm, mvmeta, splines, survival, MASS,
  #   spdep, spatialreg, SpatialEpi, CARBayes, INLA) NAO sao usados neste tutorial.
  # =============================================================================

  # --- Infraestrutura compartilhada -------------------------------------------
  source("scripts/_setup_tutoriais.R")

  COR_PRIMARIA   <- "#2E7D32"
  COR_SECUNDARIA <- "#558B2F"
  COR_ACENTO     <- "#EF6C00"

  # --- Verificacao xgboost (Suggests -- OBRIGATORIO para este tutorial) --------
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    stop(
      "[modelagem_05_ml.R] O pacote 'xgboost' e necessario para este tutorial.\n",
      "  Instale com: install.packages('xgboost')\n"
    )
  }

  # Parametros do caso condutor
  ANOS     <- 2014:2019
  DESFECHO <- "n_obitos"
  ID_COL   <- "municipio"   # agrupamento para CV sem vazamento entre cidades
  NFOLD    <- 5L
  SEED     <- 42L


  # =============================================================================
  # BLOCO 0 - Carregar entradas encadeadas
  # =============================================================================
  # Precisamos de: serie diaria de saude (date + n_obitos + municipio) e, se
  # disponivel, informacoes socioeconomicas. Carregamos com guarda file.exists();
  # se faltar, geramos serie SINTETICA MINIMA REALISTA (aviso explicito).
  message(">> Bloco 0: carregando entradas (caso_serie.rds, caso_clima_estacao.rds, caso_socio.rds)...")

  path_serie <- file.path("vignettes-pt", "dados", "caso_serie.rds")
  path_clima <- file.path("vignettes-pt", "dados", "caso_clima_estacao.rds")
  path_socio <- file.path("vignettes-pt", "dados", "caso_socio.rds")

  # ---- Helper: detectar coluna de data ----------------------------------------
  .detectar_data <- function(df) {
    cand <- intersect(c("date", "data", "dia", "data_obito"), names(df))
    if (length(cand) > 0L) return(cand[[1L]])
    is_dt <- vapply(df, function(x) inherits(x, c("Date", "POSIXct")), logical(1))
    if (any(is_dt)) return(names(df)[which(is_dt)[[1L]]])
    NA_character_
  }

  # ---- Tentar carregar serie de saude -----------------------------------------
  serie_raw <- NULL
  if (file.exists(path_serie)) {
    tryCatch({
      raw_obj <- readRDS(path_serie)
      # caso_serie pode ser lista com $serie_diaria ou tibble plano
      serie_raw <- if (is.data.frame(raw_obj)) dplyr::as_tibble(raw_obj) else dplyr::as_tibble(raw_obj$serie_diaria)
      message("   Serie de saude carregada: ", nrow(serie_raw), " linhas.")
    }, error = function(e) {
      message("   AVISO: nao foi possivel ler caso_serie.rds: ", conditionMessage(e))
    })
  }

  # ---- Tentar carregar dados climaticos de estacao (INMET) --------------------
  # caso_clima_estacao.rds pode fornecer tair_c, rh_pct etc. agregados por municipio
  clima_raw <- NULL
  if (file.exists(path_clima)) {
    tryCatch({
      clima_raw <- dplyr::as_tibble(readRDS(path_clima))
      message("   Dados climaticos de estacao carregados: ", nrow(clima_raw), " linhas.")
    }, error = function(e) {
      message("   AVISO: nao foi possivel ler caso_clima_estacao.rds: ", conditionMessage(e))
    })
  }

  # ---- Tentar carregar socioeconomicos ----------------------------------------
  socio_raw <- NULL
  if (file.exists(path_socio)) {
    tryCatch({
      raw_socio <- readRDS(path_socio)
      # caso_socio pode ser sf (com coluna geom) — remover geometria antes
      if (inherits(raw_socio, c("sf", "sfc"))) {
        if (requireNamespace("sf", quietly = TRUE)) {
          raw_socio <- sf::st_drop_geometry(raw_socio)
        } else {
          geom_cols <- vapply(raw_socio, function(x) inherits(x, c("sfc", "sfg")), logical(1))
          raw_socio <- raw_socio[, !geom_cols, drop = FALSE]
        }
      }
      socio_raw <- dplyr::as_tibble(raw_socio)
      message("   Dados socioeconomicos carregados: ", nrow(socio_raw), " linhas.")
    }, error = function(e) {
      message("   AVISO: nao foi possivel ler caso_socio.rds: ", conditionMessage(e))
    })
  }

  # =============================================================================
  # BLOCO 1 - Construir data.frame de features (ou gerar fallback sintetico)
  # =============================================================================
  # O XGBoost precisa de: outcome (n_obitos), features numericas (clima + calendario
  # + indicadores), e uma coluna de agrupamento para CV (municipio).
  message(">> Bloco 1: construindo data.frame de features...")

  construir_serie_sintetica <- function(seed = 42L) {
    message("   *** Gerando serie SINTETICA MINIMA REALISTA (demonstracao apenas). ***")
    message("   *** Para resultados reais, encadeie os tutoriais anteriores.       ***")
    set.seed(seed)

    # RMSP: 39 municipios, 6 anos, dados diarios
    n_munic    <- 12L           # subconjunto representativo da RMSP
    municipios <- paste0("Municipio_", seq_len(n_munic))
    datas      <- seq(as.Date("2014-01-01"), as.Date("2019-12-31"), by = "day")
    n_dias     <- length(datas)

    # Temperatura sintetica: media 19 C (SP altitude), variacao sazonal e ruido
    dia_do_ano  <- as.integer(format(datas, "%j"))
    temp_base   <- 19 - 6 * cos(2 * pi * (dia_do_ano - 30) / 365)  # inverno mais frio
    temp_ruido  <- stats::arima.sim(list(ar = 0.7), n = n_dias, sd = 1.5)
    temperatura <- as.numeric(temp_base + temp_ruido)

    # Umidade relativa: inversamente correlacionada com temp (inverno seco em SP)
    umidade <- pmax(30, pmin(95, 70 - 0.8 * (temperatura - 19) +
      stats::rnorm(n_dias, 0, 5)))

    # Pressao: ligeiramente anticorrelacionada com temp
    pressao <- 920 + stats::rnorm(n_dias, 0, 3)

    # Montar painel municipio x dia
    painel <- do.call(rbind, lapply(seq_len(n_munic), function(i) {
      # Cada municipio tem populacao diferente -> escala de obitos diferente
      pop_escala <- runif(1, 0.5, 2.5)
      # Efeito V: mais obitos no frio (< 14 C) e no calor (> 26 C)
      efeito_temp <- ifelse(temperatura < 14, 1.3 * (14 - temperatura)^0.5,
                    ifelse(temperatura > 26, 1.2 * (temperatura - 26)^0.5, 0))
      # Sazonalidade base (inverno respiratorio)
      sazon_base  <- 5 + 3 * cos(2 * pi * (dia_do_ano - 195) / 365)
      lambda      <- pmax(0.1, pop_escala * (sazon_base + efeito_temp +
        stats::rnorm(n_dias, 0, 0.5)))
      obitos      <- stats::rpois(n_dias, lambda)

      data.frame(
        date       = datas,
        municipio  = municipios[[i]],
        n_obitos   = obitos,
        tair_c     = round(temperatura + stats::rnorm(n_dias, 0, 0.3), 2),
        rh_pct     = round(umidade + stats::rnorm(n_dias, 0, 1), 1),
        pressao_hpa = round(pressao + stats::rnorm(n_dias, 0, 0.5), 1),
        stringsAsFactors = FALSE
      )
    }))
    dplyr::as_tibble(painel)
  }

  # ---- Tentar construir features a partir dos dados reais ---------------------
  df_features <- NULL

  if (!is.null(serie_raw)) {
    tryCatch({
      col_data <- .detectar_data(serie_raw)
      if (!is.na(col_data)) {
        df_temp <- dplyr::mutate(serie_raw, date = as.Date(.data[[col_data]]))

        # Coluna de contagem: aceita n_obitos, obitos, n, casos, count
        col_cont <- intersect(c("n_obitos", "obitos", "n", "casos", "n_casos", "count"),
                              names(df_temp))

        # Coluna de municipio: aceita municipio, name_muni, city, muni, code_muni
        col_muni <- intersect(c("municipio", "name_muni", "city", "muni",
                                "code_muni", "municipio_nome",
                                "codigo_municipio_residencia", "codigo_municipio"),
                              names(df_temp))

        if (length(col_cont) > 0L) {
          df_temp[["n_obitos"]] <- as.numeric(df_temp[[col_cont[[1L]]]])
        } else {
          df_temp[["n_obitos"]] <- 1  # cada linha e um evento
        }

        if (length(col_muni) > 0L) {
          df_temp[["municipio"]] <- as.character(df_temp[[col_muni[[1L]]]])
        } else {
          df_temp[["municipio"]] <- "SP"
        }

        # Selecionar colunas base
        df_base <- dplyr::select(df_temp, date, municipio, n_obitos)

        # Adicionar features climaticas (se presentes) ou derivar do socio
        feat_clim <- intersect(c("tair_c", "tair_dry_bulb_c", "temp", "temperatura",
                                "tmax", "tmin", "tmean",
                                "rh_pct", "umidade_relativa", "rh",
                                "pressao_hpa", "pressao"),
                              names(df_temp))
        if (length(feat_clim) > 0L) {
          df_base <- dplyr::bind_cols(
            df_base,
            dplyr::select(df_temp, dplyr::all_of(feat_clim))
          )
        } else if (!is.null(clima_raw)) {
          # Tentar vincular clima de caso_clima_estacao.rds por municipio + data
          tryCatch({
            col_data_c <- .detectar_data(clima_raw)
            col_muni_c <- intersect(c("municipio", "name_muni", "code_muni", "city"),
                                    names(clima_raw))
            if (!is.na(col_data_c) && length(col_muni_c) > 0L) {
              clima_join <- dplyr::rename(clima_raw,
                date      = dplyr::all_of(col_data_c),
                municipio = dplyr::all_of(col_muni_c[[1L]])
              )
              clima_join <- dplyr::mutate(clima_join, date = as.Date(date))
              feat_clim2 <- intersect(c("tair_c", "tair_dry_bulb_c", "tmean",
                                        "rh_pct", "umidade_relativa",
                                        "pressao_hpa", "pressao"),
                                      names(clima_join))
              if (length(feat_clim2) > 0L) {
                clima_slim <- dplyr::select(
                  clima_join,
                  date, municipio, dplyr::all_of(feat_clim2)
                )
                df_base <- dplyr::left_join(df_base, clima_slim,
                                            by = c("date", "municipio"))
                message("   Features climaticas vinculadas de caso_clima_estacao.rds.")
              }
            }
          }, error = function(e) {
            message("   AVISO: vinculacao de clima falhou: ", conditionMessage(e))
          })
          # Se ainda sem colunas climaticas, gerar sinteticas
          if (!any(c("tair_c", "tair_dry_bulb_c", "tmean") %in% names(df_base))) {
            message("   AVISO: colunas climaticas nao detectadas em caso_serie.rds nem em caso_clima_estacao.rds.")
            message("   Adicionando features sinteticas correlacionadas para demonstracao.")
            n   <- nrow(df_base)
            doy <- as.integer(format(df_base$date, "%j"))
            df_base$tair_c      <- round(19 - 6 * cos(2 * pi * (doy - 30) / 365) +
              stats::rnorm(n, 0, 1.2), 2)
            df_base$rh_pct      <- round(pmax(30, 70 - 0.5 * df_base$tair_c +
              stats::rnorm(n, 0, 3)), 1)
            df_base$pressao_hpa <- round(920 + stats::rnorm(n, 0, 2), 1)
          }
        }  # fecha else if (!is.null(clima_raw))

        # Fallback final: se nenhuma fonte de temperatura foi vinculada
        # (sem feat_clim em df_temp e sem clima_raw), gerar sintetico.
        if (!any(c("tair_c", "tair_dry_bulb_c", "tmean") %in% names(df_base))) {
          message("   AVISO: nenhuma fonte de temperatura disponivel. Gerando features climaticas sinteticas.")
          n_fb   <- nrow(df_base)
          doy_fb <- as.integer(format(df_base$date, "%j"))
          df_base$tair_c      <- round(19 - 6 * cos(2 * pi * (doy_fb - 30) / 365) +
            stats::rnorm(n_fb, 0, 1.2), 2)
          df_base$rh_pct      <- round(pmax(30, 70 - 0.5 * df_base$tair_c +
            stats::rnorm(n_fb, 0, 3)), 1)
          df_base$pressao_hpa <- round(920 + stats::rnorm(n_fb, 0, 2), 1)
        }

        if (nrow(df_base) > 10L) {
          df_features <- df_base
          message("   Features construidas a partir de caso_serie.rds: ",
                  nrow(df_features), " obs.")
        }
      }
    }, error = function(e) {
      message("   AVISO: erro ao processar caso_serie.rds: ", conditionMessage(e))
    })
  }
  # ---- Fallback sintetico (se nao conseguiu construir features) ---------------
  if (is.null(df_features)) {
    df_features <- construir_serie_sintetica(seed = SEED)
  }

  # ---- Padronizar nomes de colunas climaticas ---------------------------------
  # Garantir: tair_c, rh_pct, pressao_hpa
  if (!"tair_c" %in% names(df_features) &&
      any(c("tair_dry_bulb_c", "tmean", "temp", "temperatura") %in% names(df_features))) {
    col_t <- intersect(c("tair_dry_bulb_c", "tmean", "temp", "temperatura"),
                      names(df_features))[[1L]]
    df_features <- dplyr::rename(df_features, tair_c = dplyr::all_of(col_t))
  }
  if (!"rh_pct" %in% names(df_features) &&
      any(c("umidade_relativa", "rh") %in% names(df_features))) {
    col_rh <- intersect(c("umidade_relativa", "rh"), names(df_features))[[1L]]
    df_features <- dplyr::rename(df_features, rh_pct = dplyr::all_of(col_rh))
  }
  if (!"pressao_hpa" %in% names(df_features) &&
      "pressao" %in% names(df_features)) {
    df_features <- dplyr::rename(df_features, pressao_hpa = pressao)
  }

  # ---- Engenharia de features de calendario -----------------------------------
  df_features <- df_features |>
    dplyr::mutate(
      dia_do_ano   = as.integer(format(date, "%j")),
      dia_semana   = as.integer(format(date, "%u")),   # 1=seg, 7=dom
      mes          = as.integer(format(date, "%m")),
      ano          = as.integer(format(date, "%Y")),
      # Tendencia linear (dias desde inicio do periodo)
      tendencia    = as.integer(date - min(date)),
      # Indicadores binarios sazonais
      inverno      = as.integer(mes %in% 6:8),
      verao        = as.integer(mes %in% c(12, 1, 2)),  # 12:2 geraria 12,11,...,2
      fim_semana   = as.integer(dia_semana >= 6),
      # Transformacoes climaticas
      tair_c_sq    = tair_c^2,
      tair_c_7d    = as.numeric(stats::filter(tair_c, rep(1/7, 7), sides = 1)),
      tair_c_14d   = as.numeric(stats::filter(tair_c, rep(1/14, 14), sides = 1))
    ) |>
    dplyr::filter(!is.na(tair_c_14d))   # remove NAs do inicio da serie por medias moveis

  # ---- Adicionar features socioespaciais (se disponivel) ----------------------
  if (!is.null(socio_raw)) {
    tryCatch({
      col_muni_s <- intersect(c("municipio", "name_muni", "code_muni", "city"),
                              names(socio_raw))
      col_gini   <- intersect(c("gini", "gini_renda", "desigualdade"), names(socio_raw))
      col_idhm   <- intersect(c("idhm", "idh", "idhm_renda"), names(socio_raw))

      if (length(col_muni_s) > 0L) {
        socio_join <- dplyr::rename(socio_raw, municipio = dplyr::all_of(col_muni_s[[1L]]))
        if (length(col_gini) > 0L)
          socio_join <- dplyr::rename(socio_join, gini = dplyr::all_of(col_gini[[1L]]))
        if (length(col_idhm) > 0L)
          socio_join <- dplyr::rename(socio_join, idhm = dplyr::all_of(col_idhm[[1L]]))

        colunas_join <- intersect(c("municipio", "gini", "idhm"), names(socio_join))
        socio_slim   <- dplyr::distinct(
          dplyr::select(socio_join, dplyr::all_of(colunas_join))
        )
        df_features  <- dplyr::left_join(df_features, socio_slim,
                                        by = "municipio",
                                        relationship = "many-to-one")
        message("   Features socioespaciais vinculadas.")
      }
    }, error = function(e) {
      message("   AVISO: vinculacao socioeconomica falhou: ", conditionMessage(e))
    })
  }

  # ---- Remover colunas nao-numericas e auxiliares antes do ajuste -------------
  # (sus_mod_ml resolve feature_cols automaticamente: todas as numericas exceto
  #  outcome_col e id_col)
  df_model <- dplyr::select(df_features,
    -dplyr::any_of(c("date", "data", "dia")))   # remove colunas Date

  message("   Data.frame final: ", nrow(df_model), " obs x ", ncol(df_model), " colunas.")
  message("   Municipios (id_col): ",
          length(unique(df_model[[ID_COL]])), " grupos.")


  # =============================================================================
  # BLOCO 2 - Ajustar o modelo XGBoost com sus_mod_ml()
  # =============================================================================
  # Argumentos reais lidos de R/sus_mod_ml.R:
  #   sus_mod_ml(df, outcome_col, feature_cols=NULL, id_col=NULL,
  #              objective="count:poisson", nrounds=500L, max_depth=6L, eta=0.05,
  #              subsample=0.8, colsample_bytree=0.8, min_child_weight=1,
  #              nfold=5L, early_stopping=50L, seed=42L, lang="pt", verbose=TRUE)
  message(">> Bloco 2: ajustando modelo XGBoost (sus_mod_ml)...")

  ml_fit <- tryCatch(
    sus_mod_ml(
      df               = df_model,
      outcome_col      = DESFECHO,
      feature_cols     = NULL,        # auto: todas as numericas exceto outcome e id
      id_col           = ID_COL,      # CV agrupada por municipio (evita vazamento)
      objective        = "count:poisson",
      nrounds          = 500L,
      max_depth        = 6L,
      eta              = 0.05,
      subsample        = 0.8,
      colsample_bytree = 0.8,
      min_child_weight = 1,
      nfold            = NFOLD,
      early_stopping   = 50L,
      seed             = SEED,
      lang             = "pt",
      verbose          = TRUE
    ),
    error = function(e) {
      message("   ERRO em sus_mod_ml(): ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(ml_fit)) {
    stop("[modelagem_05_ml.R] Nao foi possivel ajustar o modelo. Verifique as mensagens acima.")
  }

  message("   Modelo ajustado. Best nrounds = ", ml_fit$meta$best_nrounds,
          " | R2-CV = ", round(ml_fit$performance$R2_cv, 3),
          " | RMSE-CV = ", round(ml_fit$performance$RMSE_cv, 3))


  # =============================================================================
  # BLOCO 3 - Gerar figuras com sus_mod_plot_ml()
  # =============================================================================
  # Argumentos reais lidos de R/sus_mod_plot_ml.R:
  #   sus_mod_plot_ml(x, type=c("importance","fit","cv_log"),
  #                  output_type=c("plot","table","all"),
  #                  n_top=20L, interactive=FALSE, base_size=12L,
  #                  save_plot=NULL, lang="pt", verbose=FALSE)
  message(">> Bloco 3: gerando figuras (sus_mod_plot_ml)...")

  # --- Fig 1: Importancia das variaveis (type = "importance") ------------------
  p_importance <- tryCatch(
    sus_mod_plot_ml(
      x           = ml_fit,
      type        = "importance",
      output_type = "plot",
      n_top       = 15L,
      lang        = "pt"
    ) +
      tema_climasus() +
      ggplot2::labs(
        caption = paste0(
          "climasus4r | XGBoost (count:poisson) | n = ", ml_fit$meta$n_obs_used,
          " obs | best_nrounds = ", ml_fit$meta$best_nrounds
        )
      ),
    error = function(e) {
      message("   AVISO (fig importance): ", conditionMessage(e)); NULL
    }
  )
  if (!is.null(p_importance))
    save_fig(p_importance, "modml_importance", width = 9, height = 6, dpi = 150)

  # --- Fig 2: Observado vs. predito CV (type = "fit") --------------------------
  p_fit <- tryCatch(
    sus_mod_plot_ml(
      x           = ml_fit,
      type        = "fit",
      output_type = "plot",
      lang        = "pt"
    ) +
      tema_climasus() +
      ggplot2::labs(
        caption = paste0(
          "climasus4r | Pearson-CV = ", round(ml_fit$performance$Pearson_cv, 3),
          " | R²-CV = ", round(ml_fit$performance$R2_cv, 3),
          " | RMSE-CV = ", round(ml_fit$performance$RMSE_cv, 3)
        )
      ),
    error = function(e) {
      message("   AVISO (fig fit): ", conditionMessage(e)); NULL
    }
  )
  if (!is.null(p_fit))
    save_fig(p_fit, "modml_fit", width = 7, height = 6, dpi = 150)

  # --- Fig 3: Curva de aprendizado por rodada (type = "cv_log") ----------------
  p_cv_log <- tryCatch(
    sus_mod_plot_ml(
      x           = ml_fit,
      type        = "cv_log",
      output_type = "plot",
      lang        = "pt"
    ) +
      tema_climasus() +
      ggplot2::labs(
        caption = paste0(
          "climasus4r | early_stopping = 50 | eta = ", ml_fit$meta$eta,
          " | max_depth = ", ml_fit$meta$max_depth
        )
      ),
    error = function(e) {
      message("   AVISO (fig cv_log): ", conditionMessage(e)); NULL
    }
  )
  if (!is.null(p_cv_log))
    save_fig(p_cv_log, "modml_cv_log", width = 8, height = 5, dpi = 150)

  # --- Fig 4: Serie temporal dos top-3 features e observado --------------------
  # Mostra contexto temporal: como os features mais importantes variam junto
  # com o desfecho ao longo de um ano de exemplo (2017).
  p_features_ts <- tryCatch({
    top3_feat <- utils::head(ml_fit$importance$Feature, 3L)
    feat_disp  <- intersect(top3_feat, names(df_features))  # precisa de date

    if (length(feat_disp) > 0L && "date" %in% names(df_features)) {
      df_ts <- df_features |>
        dplyr::filter(ano == 2017L) |>
        dplyr::group_by(date) |>
        dplyr::summarise(
          n_obitos = sum(n_obitos, na.rm = TRUE),
          dplyr::across(dplyr::all_of(feat_disp[1L]), ~ mean(.x, na.rm = TRUE),
                        .names = "{.col}"),
          .groups = "drop"
        ) |>
        dplyr::arrange(date)

      # Normalizar colunas para comparacao visual
      df_long <- dplyr::bind_rows(
        dplyr::transmute(df_ts, date, variavel = "Obitos (escala)",
          valor = scale(n_obitos)[, 1]),
        dplyr::transmute(df_ts, date, variavel = feat_disp[[1L]],
          valor = scale(.data[[feat_disp[[1L]]]])[, 1])
      )

      ggplot2::ggplot(df_long,
        ggplot2::aes(x = date, y = valor, color = variavel)) +
        ggplot2::geom_line(alpha = 0.8, linewidth = 0.7) +
        ggplot2::scale_color_manual(
          values = c(cf_colors[[1L]], cf_colors[[3L]]),
          name   = NULL
        ) +
        ggplot2::labs(
          title    = "Série temporal: feature mais importante vs. óbitos (2017)",
          subtitle = paste0("Variavel normalizada (z-score) | Feature: ", feat_disp[[1L]]),
          x        = "Data", y = "Z-score",
          caption  = "climasus4r | RMSP, idosos 60+ | causas respiratorias"
        ) +
        tema_climasus() +
        ggplot2::theme(legend.position = "top")
    } else {
      NULL
    }
  }, error = function(e) {
    message("   AVISO (fig features_ts): ", conditionMessage(e)); NULL
  })
  if (!is.null(p_features_ts))
    save_fig(p_features_ts, "modml_features_ts", width = 9, height = 5, dpi = 150)

  # --- Fig 5: Comparacao de metricas treino vs. CV (barras) --------------------
  p_performance <- tryCatch({
    perf <- ml_fit$performance
    df_perf <- data.frame(
      metrica  = rep(c("RMSE", "MAE", "R²"), 2L),
      conjunto = rep(c("Treino", "CV (OOF)"), each = 3L),
      valor    = c(
        round(perf$RMSE_train, 3), round(perf$MAE_train, 3), round(perf$R2_train, 3),
        round(perf$RMSE_cv, 3),    round(perf$MAE_cv, 3),    round(perf$R2_cv, 3)
      ),
      stringsAsFactors = FALSE
    )
    df_perf$metrica  <- factor(df_perf$metrica,  levels = c("RMSE", "MAE", "R²"))
    df_perf$conjunto <- factor(df_perf$conjunto, levels = c("Treino", "CV (OOF)"))

    ggplot2::ggplot(df_perf,
      ggplot2::aes(x = metrica, y = valor, fill = conjunto)) +
      ggplot2::geom_col(position = "dodge", alpha = 0.85) +
      ggplot2::geom_text(
        ggplot2::aes(label = valor),
        position = ggplot2::position_dodge(width = 0.9),
        vjust = -0.4, size = 3.5, color = "gray20"
      ) +
      ggplot2::scale_fill_manual(
        values = c("Treino" = cf_colors[[1L]], "CV (OOF)" = cf_colors[[3L]]),
        name   = NULL
      ) +
      ggplot2::facet_wrap(~ metrica, scales = "free_y", nrow = 1L) +
      ggplot2::labs(
        title    = "Desempenho do modelo: treino vs. validação cruzada (OOF)",
        subtitle = paste0("XGBoost count:poisson | ", NFOLD,
                          " folds agrupados por município | best_nrounds = ",
                          perf$best_nrounds),
        x = NULL, y = "Valor da métrica",
        caption  = "climasus4r | RMSP, idosos 60+ | OOF = out-of-fold (CV sem vazamento)"
      ) +
      tema_climasus() +
      ggplot2::theme(
        legend.position = "top",
        strip.text      = ggplot2::element_blank()
      )
  }, error = function(e) {
    message("   AVISO (fig performance): ", conditionMessage(e)); NULL
  })
  if (!is.null(p_performance))
    save_fig(p_performance, "modml_performance", width = 9, height = 5, dpi = 150)


  # =============================================================================
  # BLOCO 4 - Gerar tabelas
  # =============================================================================
  message(">> Bloco 4: gerando tabelas...")

  # --- Tabela 1: Metricas de desempenho ----------------------------------------
  perf_tbl <- data.frame(
    Conjunto  = c("Treino (in-sample)", "Validação CV (OOF)"),
    RMSE      = c(round(ml_fit$performance$RMSE_train, 3),
                  round(ml_fit$performance$RMSE_cv,    3)),
    MAE       = c(round(ml_fit$performance$MAE_train,  3),
                  round(ml_fit$performance$MAE_cv,     3)),
    R2        = c(round(ml_fit$performance$R2_train,   3),
                  round(ml_fit$performance$R2_cv,      3)),
    Pearson   = c(NA_real_,
                  round(ml_fit$performance$Pearson_cv, 3)),
    stringsAsFactors = FALSE
  )
  save_tbl(perf_tbl, "modml_resumo")
  message("   Tabela de desempenho salva (modml_resumo.rds).")

  # --- Tabela 2: Importancia das variaveis (top 15) ----------------------------
  imp_tbl <- utils::head(ml_fit$importance, 15L)
  save_tbl(imp_tbl, "modml_importance_tbl")
  message("   Tabela de importancia salva (modml_importance_tbl.rds).")


  # =============================================================================
  # BLOCO 5 - Salvar objeto ajustado (encadeamento)
  # =============================================================================
  message(">> Bloco 5: salvando objeto climasus_ml (mod_ml_fit.rds)...")

  path_mod_ml <- file.path("vignettes-pt", "dados", "mod_ml_fit.rds")
  tryCatch({
    saveRDS(ml_fit, file = path_mod_ml)
    message("   Objeto salvo em: ", path_mod_ml)
  }, error = function(e) {
    message("   ERRO ao salvar mod_ml_fit.rds: ", conditionMessage(e))
  })


  # =============================================================================
  # RESUMO FINAL
  # =============================================================================
  message("\n=== modelagem_05_ml.R concluido ===")
  message("Figuras (vignettes-pt/figuras/):")
  message("  modml_importance.png  -- importancia das variaveis (Gain)")
  message("  modml_fit.png         -- observado vs. predito CV")
  message("  modml_cv_log.png      -- curva de aprendizado por rodada")
  message("  modml_features_ts.png -- feature mais import. x obitos (2017)")
  message("  modml_performance.png -- metricas treino vs. CV")
  message("Tabelas (vignettes-pt/dados/):")
  message("  modml_resumo.rds          -- metricas de desempenho")
  message("  modml_importance_tbl.rds  -- importancia das variaveis")
  message("Encadeamento (vignettes-pt/dados/):")
  message("  mod_ml_fit.rds            -- objeto climasus_ml completo")
  message("best_nrounds = ", ml_fit$meta$best_nrounds,
          " | RMSE-CV = ", round(ml_fit$performance$RMSE_cv, 3),
          " | R2-CV = ", round(ml_fit$performance$R2_cv, 3))
