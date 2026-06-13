  # =============================================================================
  # modelagem_06_espacial.R
  # -----------------------------------------------------------------------------
  # PROPOSITO
  #   Gerar UMA VEZ, localmente, todas as figuras e tabelas canonicas usadas pelo
  #   Tutorial de Modelagem 06 (Modelos Espaciais) da vignette climasus4r.
  #   Esta etapa cobre o bloco de autocorrelacao, regressao e varredura espacial:
  #     - sus_mod_spatial_weights()       Matriz de pesos de contiguidade (spdep)
  #     - sus_mod_spatial_moran()         I de Moran global + LISA local
  #     - sus_mod_plot_spatial_moran()    Mapa LISA + diagrama de dispersao Moran
  #     - sus_mod_spatial_reg()           Regressao espacial SAR/SEM/SDM/SAC
  #     - sus_mod_spatial_scan()          Estatistica de varredura de Kulldorff
  #     - sus_mod_plot_spatial_scan()     Mapa coro-pletico de clusters (RR)
  #
  #   A analise pesada (construcao de pesos, permutacoes, ML espacial, scan) roda
  #   AQUI; o .Rmd apenas carrega os artefatos prontos (PNG / RDS).
  #
  # COMO RODAR
  #   Rode a partir da RAIZ do pacote (pasta climasus4r/):
  #       Rscript scripts/modelagem_06_espacial.R
  #   ou, em sessao interativa com o working directory na raiz:
  #       source("scripts/modelagem_06_espacial.R")
  #
  # ARTEFATOS GERADOS  (prefixo "modmoran_")
  #   Figuras (vignettes-pt/figuras/):
  #     - modmoran_lisa_mapa.png         Mapa coropletico LISA (HH/LL/HL/LH/NS)
  #     - modmoran_lisa_scatter.png      Diagrama de dispersao de Moran
  #     - modmoran_sar_impactos.png      Impactos direto/indireto/total (SAR)
  #     - modmoran_scan_rr.png           Mapa RR do cluster mais provavel (Kulldorff)
  #     - modmoran_scan_cat.png          Mapa categorico de aglomerados
  #   Tabelas (vignettes-pt/dados/):
  #     - modmoran_moran_global.rds      I de Moran global (tabela de resultados)
  #     - modmoran_coef_sar.rds          Coeficientes do modelo SAR
  #     - modmoran_aic_comparacao.rds    Comparacao de AIC (OLS/SAR/SEM)
  #     - modmoran_moran_residuos_sar.rds Moran nos residuos do modelo SAR
  #   Encadeamento:
  #     - mod_weights.rds                Objeto climasus_weights para reuso
  #
  # CASO CONDUTOR
  #   "Temperatura e desfechos de saude em idosos (60+) na RM de Sao Paulo,
  #   2014-2019". Aqui agregamos os obitos por municipio (media do periodo),
  #   construimos a matriz de vizinhanca Queen sobre a RMSP, testamos
  #   autocorrelacao espacial (Moran + LISA), ajustamos um modelo SAR e
  #   detectamos clusters com o scan de Kulldorff.
  #
  # ENTRADAS
  #   dados/caso_espacial.rds  (sf de municipios + n_obitos, do pipeline anterior)
  #   dados/caso_serie.rds     (serie diaria de obitos por municipio, opcional)
  #   Se ausentes, sintetizamos um painel minimo realista da RMSP com aviso.
  #
  # SAIDA
  #   dados/mod_weights.rds    objeto climasus_weights (peso Queen RMSP)
  #
  # OBS.: TODA chamada a pacote Suggests (spdep, spatialreg, SpatialEpi, sf,
  #       ggplot2, patchwork) e protegida por requireNamespace()/tryCatch e
  #       degrada graciosamente. O script nao instala pacotes automaticamente.
  # =============================================================================

  # --- Infraestrutura compartilhada -------------------------------------------
  source("scripts/_setup_tutoriais.R")

  COR_PRIMARIA   <- "#2E7D32"
  COR_SECUNDARIA <- "#558B2F"
  COR_ACENTO     <- "#EF6C00"


  # =============================================================================
  # BLOCO 0 - Carregar / sintetizar dados de entrada
  # =============================================================================
  message(">> Bloco 0: carregando dados de entrada (RMSP, obitos por municipio)...")

  # Verificar e carregar caso_espacial.rds (sf de municipios)
  rmsp_sf <- NULL

  if (file.exists("vignettes-pt/dados/caso_espacial.rds")) {
    obj_esp <- readRDS("vignettes-pt/dados/caso_espacial.rds")
    # Se tiver coluna geom e for sf-like, tentamos recuperar o sf
    if (requireNamespace("sf", quietly = TRUE) &&
        "geom" %in% names(obj_esp) &&
        inherits(obj_esp[["geom"]], "sfc")) {
      rmsp_sf <- sf::st_as_sf(obj_esp)
      # Agregar: media de n_obitos por municipio (para analise transversal)
      rmsp_sf <- dplyr::group_by(rmsp_sf, code_muni) |>
        dplyr::summarise(
          n_obitos   = mean(n_obitos, na.rm = TRUE),
          name_muni  = dplyr::first(name_muni),
          .groups = "drop"
        )
      message("   caso_espacial.rds carregado: ", nrow(rmsp_sf), " municipios.")
    }
  }

  # Fallback: sintetizar painel minimo realista da RMSP (39 municipios)
  if (is.null(rmsp_sf) || nrow(rmsp_sf) < 3L) {
    message("   [AVISO] caso_espacial.rds ausente ou insuficiente. ",
            "Sintetizando painel RMSP minimo para fins didaticos.")

    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("Pacote 'sf' e necessario para sintetizar ou usar dados espaciais.")
    }

    # Codigos IBGE de 7 digitos dos 39 municipios oficiais da RMSP (2019)
    # O padrao IBGE usa 7 digitos (codigo de UF 2 dig + codigo do municipio 5 dig)
    rmsp_codes <- c(
      3503901L, 3505708L, 3506607L, 3509502L, 3510609L,
      3513009L, 3513801L, 3515004L, 3515103L, 3516200L,
      3516309L, 3518800L, 3519055L, 3522208L, 3522505L,
      3523107L, 3523800L, 3524709L, 3526902L, 3529401L,
      3530607L, 3534401L, 3539103L, 3539806L, 3543303L,
      3544103L, 3545001L, 3546801L, 3547304L, 3547809L,
      3548708L, 3548807L, 3549953L, 3552502L, 3552809L,
      3554102L, 3556453L, 3557204L, 3558808L
    )

    # Nomes curtos correspondentes (mesma ordem dos codigos)
    rmsp_names <- c(
      "Aruja", "Barueri", "Biritiba-Mirim", "Caieiras", "Cajamar",
      "Carapicuiba", "Cotia", "Diadema", "Embu das Artes", "Embu-Guacu",
      "Ferraz de Vasconcelos", "Francisco Morato", "Franco da Rocha",
      "Guararema", "Guarulhos", "Itapecerica da Serra", "Itapevi",
      "Itaquaquecetuba", "Jandira", "Juquitiba", "Mairipora", "Maua",
      "Mogi das Cruzes", "Osasco", "Pirapora do Bom Jesus", "Poá",
      "Ribeiro Pires", "Salesopolis", "Santa Isabel", "Santana de Parnaiba",
      "Sao Bernardo do Campo", "Sao Caetano do Sul", "Sao Lourenco da Serra",
      "Sao Paulo", "Sao Paulo (Ext)", "Suzano", "Taboao da Serra",
      "Vargem Grande Paulista", "Embu das Artes 2"
    )

    set.seed(2024L)
    n_muni <- length(rmsp_codes)

    # Taxa base proporcional ao tamanho (simula heterogeneidade real)
    pop_sim      <- round(runif(n_muni, 5e4, 2e6))
    taxa_base    <- 8 + 0.8 * seq_len(n_muni) / n_muni   # gradiente sul-norte
    # Adicionar cluster espacial artificial nos primeiros 8 municipios (centro)
    taxa_base[1:8] <- taxa_base[1:8] * 1.6
    n_obitos_sim <- round(pop_sim * taxa_base / 1e4 * 5)   # 5 anos

    # Temperatura media anual (simula gradiente espacial)
    temp_media   <- 20 + rnorm(n_muni, sd = 1.5)
    temp_media[1:8] <- temp_media[1:8] + 2.5   # cluster de calor

    # Construir grid de poligonos sinteticos (grade 7x6 aprox.)
    # Cada poligono e um quadrado de 0.1 grau de lado na RMSP
    lon_base <- -46.9; lat_base <- -24.0; d <- 0.12
    nx <- 7L; ny <- ceiling(n_muni / nx)
    grid_list <- vector("list", n_muni)
    for (i in seq_len(n_muni)) {
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
      code_muni = as.character(rmsp_codes),
      name_muni = rmsp_names,
      n_obitos  = n_obitos_sim,
      pop       = pop_sim,
      temp_med  = temp_media,
      geometry  = sf::st_sfc(grid_list, crs = 4674L)
    )

    message("   Painel sintetico RMSP gerado: ", nrow(rmsp_sf), " municipios.")
  }

  # Garantir code_muni como character
  rmsp_sf$code_muni <- as.character(rmsp_sf$code_muni)

  # Adicionar variaveis auxiliares se nao existirem (para regressao)
  if (!"pop" %in% names(rmsp_sf)) {
    set.seed(42L)
    rmsp_sf$pop <- round(runif(nrow(rmsp_sf), 5e4, 2e6))
  }
  if (!"temp_med" %in% names(rmsp_sf)) {
    set.seed(43L)
    rmsp_sf$temp_med <- round(rnorm(nrow(rmsp_sf), mean = 21, sd = 1.5), 1)
  }

  # Taxa de mortalidade (por 100 mil) como desfecho continuo para regressao
  rmsp_sf$taxa_obitos <- with(sf::st_drop_geometry(rmsp_sf),
                              n_obitos / pop * 1e5)

  n_muni_rmsp <- nrow(rmsp_sf)
  message("   Municipios disponíveis para análise: ", n_muni_rmsp)


  # =============================================================================
  # BLOCO 1 - Construir matriz de pesos espaciais (Queen, row-standardised)
  # =============================================================================
  message("\n>> Bloco 1: construindo matriz de pesos espaciais (Queen contiguidade)...")

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
    print(mod_weights)
    # Salvar para reuso em outros tutoriais
    save_tbl(mod_weights, "mod_weights")
    message("   mod_weights.rds salvo.")
  } else {
    message("   [AVISO] Objeto de pesos nao gerado. Verifique o pacote spdep.")
  }


  # =============================================================================
  # BLOCO 2 - Autocorrelacao espacial: I de Moran global + LISA local
  # =============================================================================
  message("\n>> Bloco 2: calculando autocorrelacao espacial (Moran global + LISA)...")

  moran_resultado <- NULL

  if (!is.null(mod_weights)) {
    # Dados tabulares sem geometria (sus_mod_spatial_moran aceita data.frame)
    df_tabular <- sf::st_drop_geometry(rmsp_sf)

    moran_resultado <- tryCatch({
      sus_mod_spatial_moran(
        df           = df_tabular,
        outcome      = "taxa_obitos",
        W            = mod_weights,
        municipalities = NULL,
        permutations = 999L,
        alpha        = 0.05,
        adjust_p     = "fdr",
        lang         = "pt",
        verbose      = TRUE
      )
    }, error = function(e) {
      message("   [AVISO] sus_mod_spatial_moran falhou: ", conditionMessage(e))
      NULL
    })

    if (!is.null(moran_resultado)) {
      print(moran_resultado)

      # Salvar tabela global
      save_tbl(moran_resultado$global, "modmoran_moran_global")

      # --- Figura 1: Mapa LISA ------------------------------------------------
      p_lisa_mapa <- tryCatch({
        sus_mod_plot_spatial_moran(
          x              = moran_resultado,
          municipalities = rmsp_sf,
          type           = "map",
          alpha          = 0.05,
          lang           = "pt"
        )
      }, error = function(e) {
        message("   [AVISO] plot LISA mapa: ", conditionMessage(e)); NULL
      })

      if (!is.null(p_lisa_mapa)) {
        save_fig(p_lisa_mapa, "modmoran_lisa_mapa", width = 8, height = 6)
      }

      # --- Figura 2: Diagrama de dispersao de Moran ---------------------------
      p_lisa_scatter <- tryCatch({
        sus_mod_plot_spatial_moran(
          x    = moran_resultado,
          type = "scatter",
          alpha = 0.05,
          lang  = "pt"
        )
      }, error = function(e) {
        message("   [AVISO] plot Moran scatter: ", conditionMessage(e)); NULL
      })

      if (!is.null(p_lisa_scatter)) {
        save_fig(p_lisa_scatter, "modmoran_lisa_scatter", width = 7, height = 5)
      }
    }
  }


  # =============================================================================
  # BLOCO 3 - Regressao espacial: modelo SAR (Spatial Lag)
  # =============================================================================
  message("\n>> Bloco 3: ajustando regressao espacial SAR (spatial lag)...")

  reg_resultado <- NULL

  if (!is.null(mod_weights)) {
    df_reg <- sf::st_drop_geometry(rmsp_sf)

    reg_resultado <- tryCatch({
      sus_mod_spatial_reg(
        df      = df_reg,
        formula = taxa_obitos ~ temp_med,
        W       = mod_weights,
        model   = "lag",
        method  = "eigen",
        zero_policy = TRUE,
        lang    = "pt",
        verbose = TRUE
      )
    }, error = function(e) {
      message("   [AVISO] sus_mod_spatial_reg (lag) falhou: ", conditionMessage(e))
      NULL
    })

    if (!is.null(reg_resultado)) {
      print(reg_resultado)

      # Salvar coeficientes
      save_tbl(reg_resultado$coefficients, "modmoran_coef_sar")

      # --- Figura 3: Grafico de impactos direto/indireto/total ----------------
      # Como sus_mod_spatial_reg nao tem funcao plot propria para impactos,
      # construimos um grafico de barras com os impactos diretamente.
      if (!is.null(reg_resultado$impacts) && nrow(reg_resultado$impacts) > 0) {
        imp_df <- reg_resultado$impacts

        # Transformar em formato longo para ggplot2
        imp_long <- tidyr::pivot_longer(
          imp_df,
          cols      = c("direct", "indirect", "total"),
          names_to  = "componente",
          values_to = "valor"
        )
        imp_long$componente <- factor(
          imp_long$componente,
          levels = c("direct", "indirect", "total"),
          labels = c("Direto", "Indireto", "Total")
        )

        p_impactos <- ggplot2::ggplot(
          imp_long,
          ggplot2::aes(x = componente, y = valor, fill = componente)
        ) +
          ggplot2::geom_col(width = 0.55, show.legend = FALSE) +
          ggplot2::geom_hline(yintercept = 0, linewidth = 0.6, color = "grey40") +
          ggplot2::scale_fill_manual(values = c(
            "Direto"   = COR_PRIMARIA,
            "Indireto" = COR_SECUNDARIA,
            "Total"    = COR_ACENTO
          )) +
          ggplot2::facet_wrap(~term, scales = "free_y") +
          ggplot2::labs(
            title    = "Impactos Direto, Indireto e Total (SAR)",
            subtitle = paste0("Modelo SAR — taxa de óbitos ~ températura média"),
            x        = "Componente",
            y        = "Estimativa de impacto",
            caption  = "Impactos via spatialreg::impacts() com R = 200 simulações."
          ) +
          tema_climasus()

        save_fig(p_impactos, "modmoran_sar_impactos", width = 8, height = 5)
      } else {
        message("   [INFO] Nenhum impacto disponivel para plotagem (modelo error ou falha).")
      }

      # Tambem ajustar SEM para comparacao (AIC)
      reg_sem <- tryCatch({
        sus_mod_spatial_reg(
          df      = df_reg,
          formula = taxa_obitos ~ temp_med,
          W       = mod_weights,
          model   = "error",
          method  = "eigen",
          zero_policy = TRUE,
          lang    = "pt",
          verbose = FALSE
        )
      }, error = function(e) {
        message("   [AVISO] sus_mod_spatial_reg (error/SEM) falhou: ", conditionMessage(e))
        NULL
      })

      if (!is.null(reg_sem)) {
        message("   SEM ajustado: AIC = ", round(reg_sem$aic, 2))

        # Salvar tabela de comparação AIC (OLS / SAR / SEM)
        tbl_aic <- data.frame(
          modelo = c("OLS", "SAR", "SEM"),
          AIC    = c(
            round(reg_resultado$lm_aic, 2),
            round(reg_resultado$aic,    2),
            round(reg_sem$aic,          2)
          ),
          stringsAsFactors = FALSE
        )
        save_tbl(tbl_aic, "modmoran_aic_comparacao")
        message("   Tabela de AIC salva.")
      }

      # Salvar tabela de diagnóstico Moran nos resídudos (SAR)
      if (!is.null(reg_resultado$moran_residuals)) {
        mr <- reg_resultado$moran_residuals
        tbl_moran_res <- data.frame(
          I       = round(mr$I,       6),
          z       = round(mr$z,       4),
          p_value = round(mr$p_value, 4),
          stringsAsFactors = FALSE
        )
        save_tbl(tbl_moran_res, "modmoran_moran_residuos_sar")
        message("   Tabela Moran resíduos SAR salva.")
      }
    }
  }


  # =============================================================================
  # BLOCO 4 - Estatistica de varredura de Kulldorff (deteccao de clusters)
  # =============================================================================
  message("\n>> Bloco 4: executando varredura espacial de Kulldorff...")

  scan_resultado <- NULL

  if (requireNamespace("SpatialEpi", quietly = TRUE) &&
      requireNamespace("sf", quietly = TRUE)) {

    # Preparar data.frame com cases + population para scan
    df_scan <- data.frame(
      code_muni  = as.character(rmsp_sf$code_muni),
      cases      = as.integer(round(rmsp_sf$n_obitos)),
      population = as.integer(rmsp_sf$pop),
      stringsAsFactors = FALSE
    )

    scan_resultado <- tryCatch({
      sus_mod_spatial_scan(
        df            = df_scan,
        cases         = "cases",
        population    = "population",
        municipalities = rmsp_sf,
        expected      = NULL,
        max_pop_frac  = 0.5,
        n_simulations = 999L,
        alpha         = 0.05,
        lang          = "pt",
        verbose       = TRUE
      )
    }, error = function(e) {
      message("   [AVISO] sus_mod_spatial_scan falhou: ", conditionMessage(e))
      NULL
    })

    if (!is.null(scan_resultado)) {
      print(scan_resultado)

      # --- Figura 4: Mapa de Risco Relativo (RR continuo) ---------------------
      p_scan_rr <- tryCatch({
        sus_mod_plot_spatial_scan(
          x              = scan_resultado,
          municipalities = rmsp_sf,
          show_rr        = TRUE,
          alpha          = 0.05,
          lang           = "pt"
        )
      }, error = function(e) {
        message("   [AVISO] plot scan RR: ", conditionMessage(e)); NULL
      })

      if (!is.null(p_scan_rr)) {
        save_fig(p_scan_rr, "modmoran_scan_rr", width = 8, height = 6)
      }

      # --- Figura 5: Mapa categorico de aglomerados ---------------------------
      p_scan_cat <- tryCatch({
        sus_mod_plot_spatial_scan(
          x              = scan_resultado,
          municipalities = rmsp_sf,
          show_rr        = FALSE,
          alpha          = 0.05,
          lang           = "pt"
        )
      }, error = function(e) {
        message("   [AVISO] plot scan categorico: ", conditionMessage(e)); NULL
      })

      if (!is.null(p_scan_cat)) {
        save_fig(p_scan_cat, "modmoran_scan_cat", width = 8, height = 6)
      }
    }
  } else {
    message("   [AVISO] Pacote SpatialEpi nao disponivel. ",
            "Instale com: install.packages('SpatialEpi')")
  }


  # =============================================================================
  # BLOCO 5 - Salvar objeto de pesos (encadeamento)
  # =============================================================================
  message("\n>> Bloco 5: salvando artefatos de encadeamento...")

  if (!is.null(mod_weights)) {
    # Ja foi salvo no Bloco 1, re-confirmar
    if (file.exists("vignettes-pt/dados/mod_weights.rds")) {
      message("   mod_weights.rds ja existe em vignettes-pt/dados/.")
    } else {
      save_tbl(mod_weights, "mod_weights")
    }
  } else {
    message("   [AVISO] mod_weights nao disponivel para salvar.")
  }


  # =============================================================================
  # SUMARIO FINAL
  # =============================================================================
  message("\n", strrep("=", 70))
  message("SUMARIO DE ARTEFATOS GERADOS")
  message(strrep("-", 70))

  artefatos_fig <- c(
    "modmoran_lisa_mapa", "modmoran_lisa_scatter",
    "modmoran_sar_impactos", "modmoran_scan_rr", "modmoran_scan_cat"
  )
  artefatos_tbl <- c(
    "modmoran_moran_global", "modmoran_coef_sar", "modmoran_aic_comparacao",
    "modmoran_moran_residuos_sar", "mod_weights"
  )

  for (nm in artefatos_fig) {
    caminho <- file.path("vignettes-pt", "figuras", paste0(nm, ".png"))
    status  <- if (file.exists(caminho)) "OK" else "AUSENTE"
    message("  [", status, "] ", caminho)
  }
  for (nm in artefatos_tbl) {
    caminho <- file.path("vignettes-pt", "dados", paste0(nm, ".rds"))
    status  <- if (file.exists(caminho)) "OK" else "AUSENTE"
    message("  [", status, "] ", caminho)
  }
  message(strrep("=", 70))
  message("Script concluido.")
