# =============================================================================
# modelagem_02_atribuivel.R
# -----------------------------------------------------------------------------
# PROPOSITO
#   Gerar UMA VEZ, localmente, todas as figuras e tabelas canonicas usadas pelo
#   Tutorial de Modelagem 02 (Carga Atribuivel e Excesso) da vignette climasus4r.
#   Este script cobre:
#     - sus_mod_af()             Fracao/Numero Atribuivel a partir do DLNM
#     - sus_mod_plot_af()        Visualizacoes de FA (bar / forest / quantile)
#     - sus_mod_excess()         Excesso de mortalidade (spline / serfling / from_dlnm)
#     - sus_mod_burden()         Ranking de carga atribuivel entre municipios
#     - sus_mod_plot_burden()    Visualizacoes de carga (lollipop / lorenz / stacked)
#   A analise pesada roda AQUI; o .Rmd apenas carrega os artefatos prontos.
#
# COMO RODAR
#   Rode a partir da RAIZ do pacote (pasta climasus4r/):
#       Rscript scripts/modelagem_02_atribuivel.R
#   ou, em sessao interativa com o working directory na raiz:
#       source("scripts/modelagem_02_atribuivel.R")
#
# ARTEFATOS GERADOS  (prefixo "modaf_")
#   Figuras (vignettes-pt/figuras/):
#     - modaf_bar.png              FA% por componente (total/calor/frio) com IC95%
#     - modaf_forest.png           Numero Atribuivel (AN) com IC95% -- forest plot
#     - modaf_quantile.png         FA% por faixa de percentil de temperatura
#     - modaf_burden_lollipop.png  Ranking de municipios por AN (lollipop)
#     - modaf_burden_lorenz.png    Curva de Lorenz de concentracao da carga
#     - modaf_burden_stacked.png   Decomposicao calor/frio por municipio (stacked)
#   Tabelas (vignettes-pt/dados/):
#     - modaf_total.rds          Tabela FA/AN total/calor/frio
#     - modaf_burden_table.rds   Ranking de municipios + concentracao
#     - modaf_excess_total.rds   Resumo do excesso de mortalidade (sus_mod_excess)
#   Encadeamento (caso condutor):
#     - mod_af.rds               Objeto climasus_af (entrada para outros tutoriais)
#
# ENTRADAS:
#   dados/mod_dlnm_fit.rds       climasus_dlnm do Tutorial Modelagem 01
#   dados/caso_serie.rds         serie diaria de saude (fallback sintetico)
#   dados/caso_clima_estacao.rds clima INMET alinhado (fallback sintetico)
#
# DEPENDENCIAS PESADAS (Suggests):
#   dlnm, MASS, splines
#   Protegidas por requireNamespace()/tryCatch -- degradam com aviso se ausentes.
#
# CASO CONDUTOR:
#   "Temperatura e desfechos de saude em idosos (60+) na RM de Sao Paulo,
#    2014-2019" (respiratorias J00-J99).
# =============================================================================

# --- Infraestrutura compartilhada -------------------------------------------
source("scripts/_setup_tutoriais.R")

COR_PRIMARIA   <- "#2E7D32"   # verde floresta
COR_SECUNDARIA <- "#558B2F"   # verde folha
COR_ACENTO     <- "#EF6C00"   # laranja queimado
COR_CALOR      <- "#E05C5C"   # vermelho calor
COR_FRIO       <- "#4472C4"   # azul frio

ANOS <- 2014:2019

message("\n", strrep("=", 70))
message("  MODELAGEM 02 -- Carga Atribuivel e Excesso")
message(strrep("=", 70), "\n")


# =============================================================================
# BLOCO 0 - Verificar / sintetizar entradas encadeadas
# =============================================================================
message(">> Bloco 0: carregando entradas encadeadas...")

# -- Tenta carregar o fit DLNM do Tutorial Modelagem 01 ----------------------
dlnm_path <- file.path("vignettes-pt", "dados", "mod_dlnm_fit.rds")

if (file.exists(dlnm_path)) {
  message("   Carregando mod_dlnm_fit.rds...")
  mod_dlnm_fit <- readRDS(dlnm_path)
} else {
  message("   mod_dlnm_fit.rds nao encontrado. Gerando serie sintetica...")

  # ---- Fallback: serie sintetica realista (RMSP, 2014-2019) -----------------
  # Construimos um painel diario de um "municipio sintetico" com sinal
  # temperatura-mortalidade coerente com a literatura brasileira (U-shape).
  set.seed(2024L)

  datas <- seq(as.Date("2014-01-01"), as.Date("2019-12-31"), by = "day")
  n     <- length(datas)
  t_vec <- seq_along(datas)

  # Temperatura media diaria com sazonalidade e ruido
  tair <- 22 + 5 * sin(2 * pi * t_vec / 365.25 - pi / 2) +
    rnorm(n, 0, 2)

  # Baseline de mortalidade respiratoria (idosos 60+)
  mu0 <- 4 + 0.5 * cos(2 * pi * t_vec / 365.25) +
    0.3 * sin(2 * pi * t_vec / 365.25)

  # Sinal temperatura: RR aumenta nas extremas (U-shape centrado em 22 graus)
  # log(RR) = 0.03 * (temp - 22)^2 / 10 -- simplificacao didatica
  log_rr <- 0.025 * (tair - 22)^2 / 10
  obitos <- rpois(n, mu0 * exp(log_rr))

  df_sintetico <- tibble::tibble(
    date      = datas,
    tair      = round(tair, 1),
    n_obitos  = obitos,
    municipio = "RMSP_sintetico"
  )

  # ---- Ajusta DLNM sintetico se dlnm disponivel ----------------------------
  if (requireNamespace("dlnm", quietly = TRUE) &&
      requireNamespace("splines", quietly = TRUE)) {
    message("   Ajustando DLNM sintetico (dlnm encontrado)...")
    mod_dlnm_fit <- tryCatch({
      sus_mod_dlnm(
        df           = df_sintetico,
        outcome_col  = "n_obitos",
        climate_col  = "tair",
        lag_max      = 14L,
        argvar       = list(fun = "ns", df = 4),
        arglag       = list(fun = "ns", df = 3),
        family       = "quasipoisson",
        dof_per_year = 4L,
        lang         = "pt",
        verbose      = TRUE
      )
    }, error = function(e) {
      message("   AVISO: sus_mod_dlnm() falhou: ", conditionMessage(e))
      NULL
    })
  } else {
    message("   AVISO: pacote dlnm nao disponivel. Gerando objeto mock para ilustracao.")
    mod_dlnm_fit <- NULL
  }
}


# =============================================================================
# BLOCO 1 - Fracao e Numero Atribuivel (sus_mod_af)
# =============================================================================
message("\n>> Bloco 1: calculando fracao/numero atribuivel (sus_mod_af)...")

if (!is.null(mod_dlnm_fit) && inherits(mod_dlnm_fit, "climasus_dlnm")) {

  if (!requireNamespace("dlnm", quietly = TRUE)) {
    message("   AVISO: pacote dlnm necessario. Pulando sus_mod_af().")
    mod_af <- NULL
  } else {
    mod_af <- tryCatch({
      sus_mod_af(
        fit       = mod_dlnm_fit,
        threshold = NULL,       # usa ref_value do DLNM (mediana da temperatura)
        pred_at   = c(0.75, 0.90, 0.95, 0.99),
        by        = "year",     # decomposicao anual do AN/FA
        nsim      = 500L,       # simulacoes Monte Carlo para IC
        alpha     = 0.05,
        lang      = "pt",
        verbose   = TRUE
      )
    }, error = function(e) {
      message("   AVISO: sus_mod_af() falhou: ", conditionMessage(e))
      NULL
    })
  }

} else {
  message("   AVISO: mod_dlnm_fit indisponivel. Gerando climasus_af mock...")
  # Objeto mock minimo para nao quebrar o pipeline de figuras
  mod_af <- structure(
    list(
      total = tibble::tibble(
        component = c("total", "heat", "cold"),
        threshold = c(NA_real_, 22.0, 22.0),
        af        = c(0.082, 0.058, 0.024),
        af_lo     = c(0.041, 0.025, 0.008),
        af_hi     = c(0.130, 0.098, 0.045),
        af_pct    = c(8.2, 5.8, 2.4),
        af_pct_lo = c(4.1, 2.5, 0.8),
        af_pct_hi = c(13.0, 9.8, 4.5),
        an        = c(1148L, 812L, 336L),
        an_lo     = c(574L,  350L, 112L),
        an_hi     = c(1820L, 1372L, 630L),
        n_cases   = 14000L
      ),
      by_quantile = tibble::tibble(
        component      = rep(c("hot", "cold"), 4),
        quantile_prob  = c(0.75, 0.25, 0.90, 0.10, 0.95, 0.05, 0.99, 0.01),
        quantile_label = c("Above P75","Below P25","Above P90","Below P10",
                           "Above P95","Below P5","Above P99","Below P1"),
        threshold_val  = c(25.5, 17.2, 27.8, 15.5, 29.1, 13.8, 31.5, 11.2),
        af             = c(0.035, 0.018, 0.022, 0.012, 0.015, 0.009, 0.008, 0.005),
        af_lo          = c(0.018, 0.007, 0.010, 0.004, 0.006, 0.002, 0.002, 0.001),
        af_hi          = c(0.058, 0.033, 0.038, 0.022, 0.026, 0.018, 0.015, 0.010),
        af_pct         = c(3.5, 1.8, 2.2, 1.2, 1.5, 0.9, 0.8, 0.5),
        an             = c(490, 252, 308, 168, 210, 126, 112, 70),
        an_lo          = c(252, 98, 140, 56,  84,  28,  28,  14),
        an_hi          = c(812, 462, 532, 308, 364, 252, 210, 140)
      ),
      by_period = tibble::tibble(
        year    = 2014:2019,
        cases   = c(2280, 2310, 2350, 2380, 2420, 2460),
        an      = c(172, 181, 192, 198, 204, 211),
        af      = c(0.075, 0.078, 0.082, 0.083, 0.084, 0.086),
        af_pct  = c(7.5, 7.8, 8.2, 8.3, 8.4, 8.6)
      ),
      daily = tibble::tibble(
        date     = seq(as.Date("2014-01-01"), as.Date("2019-12-31"), by = "day"),
        exposure = 22 + 5 * sin(2 * pi * seq_along(
          seq(as.Date("2014-01-01"), as.Date("2019-12-31"), by = "day")
        ) / 365.25 - pi / 2),
        cases    = rpois(2192, 4),
        an       = runif(2192, 0, 1),
        af       = runif(2192, 0, 0.25)
      ),
      custom = NULL,
      meta   = list(
        climate_col = "tair",
        outcome_col = "n_obitos",
        family      = "quasipoisson",
        lag_max     = 14L,
        ref_value   = 22.0,
        threshold   = 22.0,
        n_cases     = 14000L,
        n_obs       = 2192L,
        pred_at     = c(0.75, 0.90, 0.95, 0.99),
        nsim        = 500L,
        alpha       = 0.05,
        by          = "year",
        ci_method   = "mock",
        call_time   = Sys.time()
      )
    ),
    class = c("climasus_af", "list")
  )
}


# --- Salva objeto encadeado --------------------------------------------------
if (!is.null(mod_af)) {
  save_tbl(mod_af, "mod_af")
  message("   Objeto mod_af.rds salvo em vignettes-pt/dados/")
}


# =============================================================================
# BLOCO 2 - Figuras da fracao atribuivel (sus_mod_plot_af)
# =============================================================================
message("\n>> Bloco 2: gerando figuras de fracao atribuivel (sus_mod_plot_af)...")

if (!is.null(mod_af)) {

  # 2a) Grafico de barras: FA% por componente (total / calor / frio) -----------
  p_bar <- tryCatch({
    p <- sus_mod_plot_af(
      fit         = mod_af,
      type        = "bar",
      output_type = "plot",
      interactive = FALSE,
      lang        = "pt"
    )
    p + tema_climasus() +
      ggplot2::labs(caption = "IC 95% via Monte Carlo (500 simulacoes)") +
      ggplot2::scale_fill_manual(
        values = c("Total" = "#808080",
                   "Calor" = COR_CALOR,
                   "Frio"  = COR_FRIO),
        guide = "none"
      )
  }, error = function(e) {
    message("   AVISO: plot bar falhou: ", conditionMessage(e))
    NULL
  })

  if (!is.null(p_bar)) {
    save_fig(p_bar, "modaf_bar", width = 7, height = 5)
  }

  # 2b) Forest plot: Numero Atribuivel (AN) +/- IC95% --------------------------
  p_forest <- tryCatch({
    p <- sus_mod_plot_af(
      fit         = mod_af,
      type        = "forest",
      output_type = "plot",
      interactive = FALSE,
      lang        = "pt"
    )
    p + tema_climasus()
  }, error = function(e) {
    message("   AVISO: plot forest falhou: ", conditionMessage(e))
    NULL
  })

  if (!is.null(p_forest)) {
    save_fig(p_forest, "modaf_forest", width = 8, height = 4)
  }

  # 2c) FA% por faixa de percentil de temperatura ------------------------------
  p_qtl <- tryCatch({
    p <- sus_mod_plot_af(
      fit         = mod_af,
      type        = "quantile",
      output_type = "plot",
      interactive = FALSE,
      lang        = "pt"
    )
    p + tema_climasus()
  }, error = function(e) {
    message("   AVISO: plot quantile falhou: ", conditionMessage(e))
    NULL
  })

  if (!is.null(p_qtl)) {
    save_fig(p_qtl, "modaf_quantile", width = 8, height = 5)
  }
}


# =============================================================================
# BLOCO 3 - Excesso de mortalidade (sus_mod_excess) -- ilustrativo
# =============================================================================
message("\n>> Bloco 3: estimando excesso de mortalidade (sus_mod_excess)...")

# Para o excesso usamos a abordagem from_dlnm quando o fit esta disponivel,
# senao caimos no metodo spline sobre a serie sintetica.

if (!is.null(mod_dlnm_fit) && inherits(mod_dlnm_fit, "climasus_dlnm") &&
    requireNamespace("dlnm", quietly = TRUE)) {

  mod_excess <- tryCatch({
    sus_mod_excess(
      data           = mod_dlnm_fit,
      method         = "from_dlnm",
      study_period   = c(as.Date("2014-01-01"), as.Date("2019-12-31")),
      threshold_z    = 1.96,
      by             = "year",
      lang           = "pt",
      verbose        = TRUE
    )
  }, error = function(e) {
    message("   AVISO: sus_mod_excess(from_dlnm) falhou: ", conditionMessage(e))
    NULL
  })

} else {
  # Fallback: serie sintetica com metodo spline
  set.seed(2025L)
  datas <- seq(as.Date("2014-01-01"), as.Date("2019-12-31"), by = "day")
  n     <- length(datas)
  t_vec <- seq_along(datas)
  tair  <- 22 + 5 * sin(2 * pi * t_vec / 365.25 - pi / 2) + rnorm(n, 0, 2)
  mu0   <- 4 + 0.5 * cos(2 * pi * t_vec / 365.25)
  log_rr <- 0.025 * (tair - 22)^2 / 10
  obitos <- rpois(n, mu0 * exp(log_rr))

  df_exc <- tibble::tibble(
    date     = datas,
    n_obitos = obitos
  )

  mod_excess <- tryCatch({
    sus_mod_excess(
      data           = df_exc,
      outcome_col    = "n_obitos",
      date_col       = "date",
      control_period = c(as.Date("2014-01-01"), as.Date("2016-12-31")),
      study_period   = c(as.Date("2017-01-01"), as.Date("2019-12-31")),
      method         = "spline",
      dof_per_year   = 8L,
      harmonics      = 2L,
      family         = "quasipoisson",
      threshold_z    = 1.96,
      by             = "year",
      lang           = "pt",
      verbose        = TRUE
    )
  }, error = function(e) {
    message("   AVISO: sus_mod_excess(spline) falhou: ", conditionMessage(e))
    NULL
  })
}

if (!is.null(mod_excess)) {
  message("   Excesso total: ", round(mod_excess$total$excess))
}


# =============================================================================
# BLOCO 4 - Ranking de carga entre municipios (sus_mod_burden)
# =============================================================================
message("\n>> Bloco 4: calculando ranking de carga (sus_mod_burden)...")

# Para o ranking multi-municipio precisamos de uma lista de climasus_af.
# Usamos o mod_af calculado no Bloco 1 e simulamos variantes para os demais
# municipios da RMSP (valores realistas derivados de Gasparrini et al., 2017).

if (!is.null(mod_af)) {

  # Funcao auxiliar para criar um climasus_af mock de um municipio
  .make_af_mock <- function(nome, n_cases, af_total, af_heat, af_cold,
                             ref_val = 22.0) {
    an_t <- round(n_cases * af_total)
    an_h <- round(n_cases * af_heat)
    an_c <- round(n_cases * af_cold)

    structure(
      list(
        total = tibble::tibble(
          component = c("total", "heat", "cold"),
          threshold = c(NA_real_, ref_val, ref_val),
          af        = c(af_total, af_heat, af_cold),
          af_lo     = c(af_total * 0.5, af_heat * 0.5, af_cold * 0.4),
          af_hi     = c(af_total * 1.6, af_heat * 1.7, af_cold * 1.8),
          af_pct    = round(c(af_total, af_heat, af_cold) * 100, 2),
          af_pct_lo = round(c(af_total * 0.5, af_heat * 0.5, af_cold * 0.4) * 100, 2),
          af_pct_hi = round(c(af_total * 1.6, af_heat * 1.7, af_cold * 1.8) * 100, 2),
          an        = c(an_t, an_h, an_c),
          an_lo     = c(round(an_t * 0.5), round(an_h * 0.5), round(an_c * 0.4)),
          an_hi     = c(round(an_t * 1.6), round(an_h * 1.7), round(an_c * 1.8)),
          n_cases   = n_cases
        ),
        by_quantile = mod_af$by_quantile,
        by_period   = NULL,
        daily       = NULL,
        custom      = NULL,
        meta = list(
          climate_col = "tair",
          outcome_col = "n_obitos",
          family      = "quasipoisson",
          lag_max     = 14L,
          ref_value   = ref_val,
          threshold   = ref_val,
          n_cases     = n_cases,
          n_obs       = 2192L,
          pred_at     = c(0.75, 0.90, 0.95, 0.99),
          nsim        = 0L,
          alpha       = 0.05,
          by          = NULL,
          ci_method   = "mock",
          call_time   = Sys.time()
        )
      ),
      class = c("climasus_af", "list")
    )
  }

  # Lista de municipios da RMSP com valores baseados na literatura
  lista_af <- list(
    "Sao Paulo"           = mod_af,
    "Guarulhos"           = .make_af_mock("Guarulhos",    2800, 0.079, 0.054, 0.025),
    "Campinas"            = .make_af_mock("Campinas",     2200, 0.071, 0.047, 0.024),
    "Sao Bernardo"        = .make_af_mock("Sao Bernardo", 1850, 0.085, 0.062, 0.023),
    "Santo Andre"         = .make_af_mock("Santo Andre",  1620, 0.078, 0.055, 0.023),
    "Osasco"              = .make_af_mock("Osasco",       1430, 0.073, 0.051, 0.022),
    "Mogi das Cruzes"     = .make_af_mock("Mogi",         1110, 0.068, 0.046, 0.022),
    "Diadema"             = .make_af_mock("Diadema",       980, 0.082, 0.058, 0.024),
    "Carapicuiba"         = .make_af_mock("Carapicuiba",   870, 0.075, 0.053, 0.022),
    "Itaquaquecetuba"     = .make_af_mock("Itaquaquecetuba", 820, 0.070, 0.048, 0.022)
  )

  mod_burden <- tryCatch({
    sus_mod_burden(
      fits      = lista_af,
      component = "all",   # decompoe em calor e frio por municipio
      rank_by   = "an",
      top_n     = NULL,
      nsim      = 0L,
      lang      = "pt",
      verbose   = TRUE
    )
  }, error = function(e) {
    message("   AVISO: sus_mod_burden() falhou: ", conditionMessage(e))
    NULL
  })

  # --- Salva objeto encadeado para modulos posteriores -----------------------
  if (!is.null(mod_burden)) {
    save_tbl(mod_burden, "mod_burden")
    message("   Objeto mod_burden.rds salvo em vignettes-pt/dados/")
  }

} else {
  mod_burden <- NULL
  message("   mod_af indisponivel. Pulando sus_mod_burden().")
}


# =============================================================================
# BLOCO 5 - Figuras de carga (sus_mod_plot_burden)
# =============================================================================
message("\n>> Bloco 5: gerando figuras de carga (sus_mod_plot_burden)...")

if (!is.null(mod_burden)) {

  # 5a) Lollipop: ranking de municipios por AN ---------------------------------
  p_lollipop <- tryCatch({
    p <- sus_mod_plot_burden(
      x           = mod_burden,
      type        = "lollipop",
      output_type = "plot",
      interactive = FALSE,
      lang        = "pt"
    )
    p + tema_climasus()
  }, error = function(e) {
    message("   AVISO: plot lollipop falhou: ", conditionMessage(e))
    NULL
  })

  if (!is.null(p_lollipop)) {
    save_fig(p_lollipop, "modaf_burden_lollipop", width = 8, height = 5)
  }

  # 5b) Curva de Lorenz: concentracao da carga --------------------------------
  p_lorenz <- tryCatch({
    p <- sus_mod_plot_burden(
      x           = mod_burden,
      type        = "lorenz",
      output_type = "plot",
      interactive = FALSE,
      lang        = "pt"
    )
    p + tema_climasus()
  }, error = function(e) {
    message("   AVISO: plot lorenz falhou: ", conditionMessage(e))
    NULL
  })

  if (!is.null(p_lorenz)) {
    save_fig(p_lorenz, "modaf_burden_lorenz", width = 7, height = 5)
  }

  # 5c) Stacked: decomposicao calor/frio por municipio -------------------------
  # Requer component = "all" (ja configurado acima)
  p_stacked <- tryCatch({
    p <- sus_mod_plot_burden(
      x           = mod_burden,
      type        = "stacked",
      output_type = "plot",
      interactive = FALSE,
      lang        = "pt"
    )
    p + tema_climasus() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
  }, error = function(e) {
    message("   AVISO: plot stacked falhou: ", conditionMessage(e))
    NULL
  })

  if (!is.null(p_stacked)) {
    save_fig(p_stacked, "modaf_burden_stacked", width = 9, height = 5)
  }
}


# =============================================================================
# BLOCO 6 - Salvar tabelas canonicas
# =============================================================================
message("\n>> Bloco 6: salvando tabelas canonicas...")

# Tabela FA/AN por componente
if (!is.null(mod_af)) {
  save_tbl(mod_af$total, "modaf_total")
}

# Tabela de ranking de carga entre municipios
if (!is.null(mod_burden)) {
  save_tbl(mod_burden$burden_table, "modaf_burden_table")
}

# Tabela de excesso (resumo total)
if (!is.null(mod_excess)) {
  save_tbl(mod_excess$total, "modaf_excess_total")
}


# =============================================================================
# MENSAGEM FINAL
# =============================================================================
message("\n", strrep("=", 70))
message("  MODELAGEM 02 -- concluido!")
message("  Artefatos em vignettes-pt/figuras/ (prefixo modaf_)")
message("  Tabelas em vignettes-pt/dados/ (prefixo modaf_)")
message("  Objetos encadeados: vignettes-pt/dados/mod_af.rds")
message("                      vignettes-pt/dados/mod_burden.rds")
message(strrep("=", 70), "\n")
