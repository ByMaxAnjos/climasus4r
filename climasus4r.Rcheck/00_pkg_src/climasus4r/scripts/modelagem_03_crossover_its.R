# =============================================================================
# modelagem_03_crossover_its.R
# -----------------------------------------------------------------------------
# PROPÓSITO
#   Gerar UMA VEZ, localmente, todas as figuras e tabelas canônicas usadas pelo
#   Tutorial de Modelagem 03 (Case-Crossover e Séries Temporais Interrompidas)
#   da vignette climasus4r. A modelagem pesada roda AQUI; o arquivo .Rmd apenas
#   carrega os artefatos prontos (PNG via include_graphics, RDS via readRDS).
#
# COMO RODAR
#   Rode a partir da RAIZ do pacote (pasta climasus4r/):
#       Rscript scripts/modelagem_03_crossover_its.R
#   ou, em sessão interativa com o working directory na raiz:
#       source("scripts/modelagem_03_crossover_its.R")
#
# ARTEFATOS GERADOS (prefixo: modcc_)
#   Figuras (vignettes-pt/figuras/):
#     modcc_or_forest.png         IRR floresta: associação temperatura × óbitos
#     modcc_or_lag.png            IRR por defasagem (lag 0, 1, 2, 3, 4, 5, 6)
#     modcc_its_serie.png         ITS: série observada + contrafactual
#     modcc_its_forest.png        ITS: RR efeito imediato + mudança de tendência
#     modcc_its_segmentos.png     ITS: média de óbitos por segmento
#     modcc_its_prevenidos.png    ITS: eventos prevenidos/excedentes por dia
#   Tabelas (vignettes-pt/dados/):
#     modcc_or_table.rds          Tabela de OR (lag-0 e lag 0-6)
#     modcc_its_effects.rds       Tabela de efeitos ITS
#   Encadeamento (caso condutor):
#     mod_cc_its.rds              Objeto ajustado (lista: cc0, cc06, its1),
#                                 entrada para tutoriais subsequentes
#
# CASO CONDUTOR
#   "Temperatura e desfechos de saúde em idosos na RMSP, 2014–2019"
#   Case-crossover: OR temperatura × óbitos respiratórios (J00-J99), lag-0 e
#   lag 0-6, estratificado por mês.
#   ITS: interrupção em 2014-10-01 (início de episódio de calor extremo / marco
#   hipotético de mudança de padrão climático), com contrafactual e 2 harmônicos.
#
# PACOTES SUGGESTS PROTEGIDOS
#   survival (clogit): protegido com requireNamespace/tryCatch
# =============================================================================

# --- Infraestrutura compartilhada -------------------------------------------
source("scripts/_setup_tutoriais.R")

COR_PRIMARIA   <- "#2E7D32"
COR_SECUNDARIA <- "#558B2F"
COR_ACENTO     <- "#EF6C00"

# =============================================================================
# BLOCO 1 — Carregar dados de entrada
# =============================================================================
message(">> Bloco 1: carregando dados do caso condutor...")

# --- 1a. Série diária de óbitos (caso_serie.rds) ----------------------------
path_serie <- file.path("vignettes-pt", "dados", "caso_serie.rds")

if (file.exists(path_serie)) {
  raw_serie <- readRDS(path_serie)
  # O objeto é uma lista com $serie_diaria (colunas: date, codigo_municipio_residencia, n_obitos)
  df_serie <- raw_serie$serie_diaria
  # Agregar por data (soma de óbitos de todos os municípios da RMSP)
  df_serie <- df_serie |>
    dplyr::group_by(date) |>
    dplyr::summarise(n_obitos = sum(n_obitos, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::arrange(date)
  message("   caso_serie.rds carregado: ", nrow(df_serie), " dias.")
} else {
  message("   AVISO: caso_serie.rds nao encontrado. Sintetizando serie minima realista.")
  set.seed(42)
  datas <- seq(as.Date("2014-01-01"), as.Date("2019-12-31"), by = "day")
  n <- length(datas)
  # Sazonalidade: mais óbitos no inverno (jun-ago), linha de base ~8/dia
  doy <- as.integer(format(datas, "%j"))
  sazon <- 2 * cos(2 * pi * doy / 365.25) + 1.5 * cos(4 * pi * doy / 365.25)
  lambda <- exp(2.1 + 0.25 * sazon)
  df_serie <- data.frame(
    date     = datas,
    n_obitos = stats::rpois(n, lambda)
  )
  message("   Serie sintetica gerada: ", n, " dias (aviso: nao usar para publicacao).")
}

# --- 1b. Temperatura diária (caso_clima_estacao.rds) -------------------------
path_clima <- file.path("vignettes-pt", "dados", "caso_clima_estacao.rds")

if (file.exists(path_clima)) {
  df_clima <- readRDS(path_clima)
  # Garantir colunas date e tair_dry_bulb_c (média das estações)
  if (!"date" %in% names(df_clima))
    stop("caso_clima_estacao.rds deve conter coluna 'date'.")
  if (!"tair_dry_bulb_c" %in% names(df_clima)) {
    # Tentar detectar coluna de temperatura
    tcol <- grep("tair|temp|temperatura", names(df_clima),
                 ignore.case = TRUE, value = TRUE)[1]
    if (!is.na(tcol)) {
      df_clima <- dplyr::rename(df_clima, tair_dry_bulb_c = dplyr::all_of(tcol))
      message("   Temperatura: coluna renomeada de '", tcol, "' para 'tair_dry_bulb_c'.")
    } else {
      stop("Nenhuma coluna de temperatura encontrada em caso_clima_estacao.rds.")
    }
  }
  df_clima <- df_clima |>
    dplyr::group_by(date) |>
    dplyr::summarise(tair_dry_bulb_c = mean(tair_dry_bulb_c, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::mutate(date = as.Date(date))
  message("   caso_clima_estacao.rds carregado.")
} else {
  message("   AVISO: caso_clima_estacao.rds nao encontrado. Sintetizando temperatura minima realista.")
  set.seed(7)
  datas <- seq(as.Date("2014-01-01"), as.Date("2019-12-31"), by = "day")
  n <- length(datas)
  doy <- as.integer(format(datas, "%j"))
  # RMSP: media ~19°C, amplitude ~6°C, pico no verao (jan)
  tmed <- 19 - 6 * cos(2 * pi * doy / 365.25) + stats::rnorm(n, 0, 1.5)
  df_clima <- data.frame(date = datas, tair_dry_bulb_c = round(tmed, 1))
  message("   Temperatura sintetica gerada (aviso: nao usar para publicacao).")
}

# --- 1c. Junção série + clima -----------------------------------------------
df_joint <- dplyr::inner_join(df_serie, df_clima, by = "date")
message("   Dataset conjunto: ", nrow(df_joint), " dias completos.")

# =============================================================================
# BLOCO 2 — Case-Crossover (sus_mod_casecrossover)
# =============================================================================
message("\n>> Bloco 2: ajustando case-crossover...")

# --- 2a. Lag-0 (same-day temperature) ---------------------------------------
cc0 <- tryCatch(
  sus_mod_casecrossover(
    data         = df_joint,
    outcome_col  = "n_obitos",
    exposure_col = "tair_dry_bulb_c",
    stratum      = "month",
    lag          = 0L,
    method       = "conditional_poisson",
    family       = "quasipoisson",
    alpha        = 0.05,
    lang         = "pt",
    verbose      = TRUE
  ),
  error = function(e) {
    message("   ERRO no case-crossover lag-0: ", conditionMessage(e))
    NULL
  }
)

if (!is.null(cc0)) {
  message("   CC lag-0 OK. OR = ", round(cc0$or_table$or, 4),
          " [", round(cc0$or_table$or_lo, 4), ", ",
          round(cc0$or_table$or_hi, 4), "]")
}

# --- 2b. Média de lags 0-6 --------------------------------------------------
cc06 <- tryCatch(
  sus_mod_casecrossover(
    data         = df_joint,
    outcome_col  = "n_obitos",
    exposure_col = "tair_dry_bulb_c",
    stratum      = "month",
    lag          = 0L:6L,
    method       = "conditional_poisson",
    family       = "quasipoisson",
    alpha        = 0.05,
    lang         = "pt",
    verbose      = TRUE
  ),
  error = function(e) {
    message("   ERRO no case-crossover lag 0-6: ", conditionMessage(e))
    NULL
  }
)

if (!is.null(cc06)) {
  message("   CC lag 0-6 OK. OR = ", round(cc06$or_table$or, 4))
}

# --- 2c. (Opcional) clogit se survival disponível ---------------------------
cc_clogit <- NULL
if (requireNamespace("survival", quietly = TRUE)) {
  cc_clogit <- tryCatch(
    sus_mod_casecrossover(
      data         = df_joint,
      outcome_col  = "n_obitos",
      exposure_col = "tair_dry_bulb_c",
      stratum      = "month",
      lag          = 0L,
      method       = "clogit",
      alpha        = 0.05,
      lang         = "pt",
      verbose      = FALSE
    ),
    error = function(e) {
      message("   AVISO: clogit falhou: ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(cc_clogit))
    message("   CC clogit OK. OR = ", round(cc_clogit$or_table$or, 4))
} else {
  message("   AVISO: pacote 'survival' nao instalado — clogit ignorado.")
}

# =============================================================================
# BLOCO 3 — Figuras do Case-Crossover
# =============================================================================
message("\n>> Bloco 3: gerando figuras case-crossover...")

# --- 3a. Gráfico de floresta: OR por especificação de lag -------------------
if (!is.null(cc0) && !is.null(cc06)) {
  or_comp <- dplyr::bind_rows(
    dplyr::mutate(cc0$or_table,  modelo = "Lag 0"),
    dplyr::mutate(cc06$or_table, modelo = "Lag 0-6 (média)")
  )

  p_forest <- ggplot2::ggplot(
    or_comp,
    ggplot2::aes(x = or, y = modelo, xmin = or_lo, xmax = or_hi, color = modelo)
  ) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed",
                        color = "grey50", linewidth = 0.6) +
    ggplot2::geom_errorbarh(height = 0.18, linewidth = 0.9) +
    ggplot2::geom_point(size = 4, shape = 18) +
    ggplot2::scale_color_manual(
      values = c("Lag 0" = COR_PRIMARIA, "Lag 0-6 (média)" = COR_ACENTO),
      guide  = "none"
    ) +
    ggplot2::scale_x_continuous(
      name   = "IRR (IC 95%)",
      limits = c(
        min(or_comp$or_lo) * 0.98,
        max(or_comp$or_hi) * 1.02
      )
    ) +
    ggplot2::scale_y_discrete(name = NULL) +
    ggplot2::labs(
      title    = "Case-Crossover: IRR da Temperatura sobre Óbitos Respiratórios",
      subtitle = "RMSP, idosos 60+, 2014–2019 | Estratificado por ano-mês",
      caption  = "Método: Poisson Condicional (quasipoisson); estimador: IRR; IC: 95%"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0("IRR=", round(or, 3), " [", round(or_lo, 3),
                       "; ", round(or_hi, 3), "]")
      ),
      nudge_y = 0.25, size = 3.2, color = "grey30"
    ) +
    tema_climasus()

  save_fig(p_forest, "modcc_or_forest")
}

# --- 3b. OR por defasagem individual (lag 0 a 6 dias) -----------------------
or_lags <- lapply(0L:6L, function(k) {
  fit_k <- tryCatch(
    sus_mod_casecrossover(
      data         = df_joint,
      outcome_col  = "n_obitos",
      exposure_col = "tair_dry_bulb_c",
      stratum      = "month",
      lag          = k,
      method       = "conditional_poisson",
      family       = "quasipoisson",
      lang         = "pt",
      verbose      = FALSE
    ),
    error = function(e) NULL
  )
  if (is.null(fit_k)) return(NULL)
  dplyr::mutate(fit_k$or_table, lag_k = k)
})
or_lags_df <- dplyr::bind_rows(Filter(Negate(is.null), or_lags))

if (nrow(or_lags_df) > 0L) {
  p_lag <- ggplot2::ggplot(
    or_lags_df,
    ggplot2::aes(x = lag_k, y = or, ymin = or_lo, ymax = or_hi)
  ) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed",
                        color = "grey50", linewidth = 0.6) +
    ggplot2::geom_ribbon(fill = COR_PRIMARIA, alpha = 0.20) +
    ggplot2::geom_line(color = COR_PRIMARIA, linewidth = 1.1) +
    ggplot2::geom_point(color = COR_PRIMARIA, size = 3, shape = 19) +
    ggplot2::scale_x_continuous(
      name   = "Defasagem (dias)",
      breaks = 0:6
    ) +
    ggplot2::scale_y_continuous(name = "IRR (IC 95%)") +
    ggplot2::labs(
      title    = "Case-Crossover: IRR por Defasagem Individual (Lag 0–6)",
      subtitle = "Associação temperatura × óbitos respiratórios | RMSP, 2014–2019",
      caption  = "Cada ponto = modelo independente com lag fixo; Poisson condicional (quasipoisson); estimador: IRR"
    ) +
    tema_climasus()

  save_fig(p_lag, "modcc_or_lag")
}

# =============================================================================
# BLOCO 4 — Tabela de OR (artefato de tabela)
# =============================================================================
message("\n>> Bloco 4: salvando tabela de OR...")

if (!is.null(cc0) && !is.null(cc06)) {
  tbl_or <- dplyr::bind_rows(
    dplyr::mutate(cc0$or_table,  especificacao = "Lag 0"),
    dplyr::mutate(cc06$or_table, especificacao = "Lag 0-6 (media)")
  ) |>
    dplyr::select(especificacao, lag_spec, or, or_lo, or_hi, p_value) |>
    dplyr::mutate(
      dplyr::across(c(or, or_lo, or_hi), \(x) round(x, 4)),
      p_value = signif(p_value, 3)
    )
  save_tbl(tbl_or, "modcc_or_table")
}

# =============================================================================
# BLOCO 5 — Série Temporal Interrompida (sus_mod_its)
# =============================================================================
message("\n>> Bloco 5: ajustando ITS...")

# Data de interrupção: 2014-10-01 (início hipotético de episódio de calor
# extremo / mudança de padrão na RMSP — uso didático)
DATA_INTERRUPCAO <- as.Date("2014-10-01")

its1 <- tryCatch(
  sus_mod_its(
    data               = df_joint,
    outcome_col        = "n_obitos",
    date_col           = "date",
    interruption_dates = DATA_INTERRUPCAO,
    harmonics          = 2L,
    family             = "quasipoisson",
    covariates         = NULL,
    alpha              = 0.05,
    counterfactual     = TRUE,
    lang               = "pt",
    verbose            = TRUE
  ),
  error = function(e) {
    message("   ERRO no ITS: ", conditionMessage(e))
    NULL
  }
)

if (!is.null(its1)) {
  ef <- its1$effects[1L, ]
  message("   ITS OK. Efeito imediato: RR = ", round(ef$level_ratio, 3),
          " [", round(ef$level_ci_lo, 3), ", ", round(ef$level_ci_hi, 3), "]",
          "  p = ", signif(ef$level_p, 3))
}

# =============================================================================
# BLOCO 6 — Figuras do ITS
# =============================================================================
message("\n>> Bloco 6: gerando figuras ITS...")

# --- 6a. Série observada + contrafactual + quebra ---------------------------
if (!is.null(its1) && !is.null(its1$counterfactual)) {
  cf_data <- its1$counterfactual

  p_its_serie <- ggplot2::ggplot(cf_data, ggplot2::aes(x = date)) +
    # Contrafactual com IC
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = cf_lo, ymax = cf_hi),
      fill = COR_ACENTO, alpha = 0.15
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = counterfactual),
      color = COR_ACENTO, linewidth = 0.9, linetype = "dashed"
    ) +
    # Série observada
    ggplot2::geom_line(
      ggplot2::aes(y = observed),
      color = "grey40", linewidth = 0.4, alpha = 0.7
    ) +
    # Série ajustada (fitted)
    ggplot2::geom_line(
      ggplot2::aes(y = predicted),
      color = COR_PRIMARIA, linewidth = 0.9
    ) +
    # Linha de interrupção
    ggplot2::geom_vline(
      xintercept = as.numeric(DATA_INTERRUPCAO),
      linetype = "dotted", color = "firebrick", linewidth = 0.8
    ) +
    ggplot2::annotate(
      "text",
      x     = DATA_INTERRUPCAO + 30,
      y     = max(cf_data$observed, na.rm = TRUE) * 0.97,
      label = paste0("Interrupção\n", format(DATA_INTERRUPCAO, "%d/%m/%Y")),
      color = "firebrick", size = 3, hjust = 0
    ) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::scale_y_continuous(name = "Óbitos respiratórios (diários)") +
    ggplot2::labs(
      title    = "ITS: Série Temporal Interrompida — Óbitos Respiratórios (RMSP)",
      subtitle = "Verde: ajustado | Laranja tracejado: contrafactual (tendência pré) | Cinza: observado",
      caption  = "Modelo: Poisson segmentado (quasipoisson) + 2 harmônicos sazonais; Bernal et al. (2017)",
      x        = "Data"
    ) +
    tema_climasus()

  save_fig(p_its_serie, "modcc_its_serie", width = 10, height = 5)
}

# --- 6b. Gráfico de floresta: RR efeito imediato e tendência ----------------
if (!is.null(its1)) {
  ef2 <- its1$effects

  # Painel superior: RR de nível (efeito imediato)
  p_its_forest <- ggplot2::ggplot(ef2, ggplot2::aes(
    y     = label,
    x     = level_ratio,
    xmin  = level_ci_lo,
    xmax  = level_ci_hi
  )) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed",
                        color = "grey50", linewidth = 0.7) +
    ggplot2::geom_errorbarh(height = 0.15, linewidth = 1.0,
                            color = COR_PRIMARIA) +
    ggplot2::geom_point(size = 5, shape = 18, color = COR_PRIMARIA) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = paste0("RR=", round(level_ratio, 3),
                       " [", round(level_ci_lo, 3), "; ",
                       round(level_ci_hi, 3), "]  p=",
                       signif(level_p, 2))
      ),
      nudge_y = 0.22, size = 3.3, color = "grey20"
    ) +
    ggplot2::scale_x_continuous(name = "RR de Nível (IC 95%)") +
    ggplot2::scale_y_discrete(name = NULL) +
    ggplot2::labs(
      title    = "ITS: Efeito Imediato da Interrupção (Mudança de Nível)",
      subtitle = paste0("Data: ", format(DATA_INTERRUPCAO, "%d/%m/%Y"),
                        " | Desfecho: óbitos respiratórios, RMSP, idosos 60+"),
      caption  = "RR = exp(beta_step); Bernal et al. (2017)"
    ) +
    tema_climasus()

  save_fig(p_its_forest, "modcc_its_forest")
}

# --- 6c. Médias por segmento (pré / pós interrupção) -----------------------
if (!is.null(its1)) {
  seg_data <- its1$segments

  seg_long <- tidyr::pivot_longer(
    seg_data,
    cols = c("mean_observed", "mean_predicted", "mean_counterfactual"),
    names_to  = "tipo",
    values_to = "media"
  ) |>
    dplyr::mutate(
      tipo = dplyr::recode(tipo,
        mean_observed       = "Observado",
        mean_predicted      = "Ajustado",
        mean_counterfactual = "Contrafactual"
      ),
      tipo = factor(tipo, levels = c("Observado", "Ajustado", "Contrafactual"))
    )

  p_its_seg <- ggplot2::ggplot(
    seg_long,
    ggplot2::aes(x = segment, y = media, fill = tipo)
  ) +
    ggplot2::geom_col(position = ggplot2::position_dodge(0.8),
                      width = 0.7, alpha = 0.88) +
    ggplot2::scale_fill_manual(
      name   = NULL,
      values = c(
        "Observado"      = COR_PRIMARIA,
        "Ajustado"       = COR_SECUNDARIA,
        "Contrafactual"  = COR_ACENTO
      )
    ) +
    ggplot2::scale_y_continuous(name = "Média diária de óbitos") +
    ggplot2::scale_x_discrete(name = "Segmento") +
    ggplot2::labs(
      title    = "ITS: Média Diária de Óbitos por Segmento",
      subtitle = "Comparação observado vs. ajustado vs. contrafactual",
      caption  = paste0("N dias pré = ", its1$meta$n_pre,
                        "; N dias pós = ", its1$meta$n_obs - its1$meta$n_pre)
    ) +
    tema_climasus() +
    ggplot2::theme(legend.position = "bottom")

  save_fig(p_its_seg, "modcc_its_segmentos")
}

# --- 6d. Série de eventos prevenidos (contrafactual - observado) ---------------
if (!is.null(its1) && !is.null(its1$counterfactual)) {
  prev_data <- its1$counterfactual

  p_its_prev <- ggplot2::ggplot(prev_data, ggplot2::aes(x = date, y = prevented)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        color = "grey50", linewidth = 0.7) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = pmin(prevented, 0), ymax = pmax(prevented, 0)),
      fill = COR_PRIMARIA, alpha = 0.25
    ) +
    ggplot2::geom_line(color = COR_PRIMARIA, linewidth = 0.8) +
    ggplot2::geom_vline(
      xintercept = as.numeric(DATA_INTERRUPCAO),
      linetype = "dotted", color = "firebrick", linewidth = 0.8
    ) +
    ggplot2::annotate(
      "text",
      x     = DATA_INTERRUPCAO + 30,
      y     = max(abs(prev_data$prevented), na.rm = TRUE) * 0.9,
      label = paste0("Interrupção\n", format(DATA_INTERRUPCAO, "%d/%m/%Y")),
      color = "firebrick", size = 3, hjust = 0
    ) +
    ggplot2::scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    ggplot2::scale_y_continuous(name = "Eventos prevenidos (ou excedentes) por dia") +
    ggplot2::labs(
      title    = "ITS: Eventos Prevenidos Diários (Contrafactual − Observado)",
      subtitle = "Positivo = redução; Negativo = excesso em relação à tendência pré-interrupção",
      caption  = "Contrafactual: projeção do modelo sem os termos D_j(t) e S_j(t); Bernal et al. (2017)",
      x        = "Data"
    ) +
    tema_climasus()

  save_fig(p_its_prev, "modcc_its_prevenidos")
}

# =============================================================================
# BLOCO 7 — Tabela de efeitos ITS
# =============================================================================
message("\n>> Bloco 7: salvando tabela de efeitos ITS...")

if (!is.null(its1)) {
  tbl_its <- its1$effects |>
    dplyr::mutate(
      dplyr::across(c(level_ratio, level_ci_lo, level_ci_hi,
                      slope_ratio_annual), \(x) round(x, 4)),
      level_p = signif(level_p, 3),
      slope_p = signif(slope_p, 3)
    )
  save_tbl(tbl_its, "modcc_its_effects")
}

# =============================================================================
# BLOCO 8 — Salvar objeto ajustado (mod_cc_its.rds) para reuso
# =============================================================================
message("\n>> Bloco 8: salvando mod_cc_its.rds...")

mod_cc_its <- list(
  cc0      = cc0,
  cc06     = cc06,
  cc_clogit = cc_clogit,
  its1     = its1,
  meta = list(
    caso_condutor      = "RMSP, idosos 60+, obitos respiratorios J00-J99, 2014-2019",
    exposure_col       = "tair_dry_bulb_c",
    outcome_col        = "n_obitos",
    interruption_dates = DATA_INTERRUPCAO,
    script             = "scripts/modelagem_03_crossover_its.R",
    gerado_em          = Sys.time()
  )
)

save_tbl(mod_cc_its, "mod_cc_its")
message("   mod_cc_its.rds salvo em vignettes-pt/dados/mod_cc_its.rds")

message("\n>> Script concluido. Artefatos gerados:")
message("   Figuras: vignettes-pt/figuras/modcc_or_forest.png")
message("            vignettes-pt/figuras/modcc_or_lag.png")
message("            vignettes-pt/figuras/modcc_its_serie.png")
message("            vignettes-pt/figuras/modcc_its_forest.png")
message("            vignettes-pt/figuras/modcc_its_segmentos.png")
message("            vignettes-pt/figuras/modcc_its_prevenidos.png")
message("   Tabelas: vignettes-pt/dados/modcc_or_table.rds")
message("            vignettes-pt/dados/modcc_its_effects.rds")
message("   Cadeia : vignettes-pt/dados/mod_cc_its.rds")
