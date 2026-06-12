# =============================================================================
# tutorial_09_modelagem.R
# -----------------------------------------------------------------------------
# PROPOSITO
#   Gerar UMA VEZ, localmente, todas as figuras e tabelas canonicas usadas pelo
#   Tutorial 9 (Modelagem Epidemiologica) da vignette climasus4r. Esta e a ETAPA
#   FINAL do pipeline (chapeu bioestatistico) e cobre a familia sus_mod_*:
#     - sus_mod_dlnm()           Distributed Lag Non-linear Model (exposicao x lag)
#     - sus_mod_af()             Fracao/Numero atribuivel (calor/frio) a partir do DLNM
#     - sus_mod_its()            Series temporais interrompidas (intervencoes)
#     - sus_mod_casecrossover()  Case-crossover time-stratified (controle por desenho)
#     - sus_mod_pool()           Meta-analise multivariada multi-cidade (mvmeta)
#     - sus_mod_plot_dlnm()      Visualizacoes cientificas do DLNM
#   A analise pesada (ajuste GLM, crossbasis, simulacao Monte Carlo) roda AQUI; o
#   .Rmd apenas carrega os artefatos prontos (PNG via include_graphics, RDS via
#   readRDS).
#
# COMO RODAR
#   Rode a partir da RAIZ do pacote (pasta climasus4r/):
#       Rscript scripts/tutorial_09_modelagem.R
#   ou, em sessao interativa com o working directory na raiz:
#       source("scripts/tutorial_09_modelagem.R")
#
# ARTEFATOS GERADOS  (prefixo "mod_")
#   Figuras (vignettes-pt/figuras/):
#     - mod_dlnm_surface.png   superficie/contorno log(RR) exposicao x lag
#     - mod_dlnm_overall.png   curva exposicao-resposta cumulativa (RR, IC95%)
#     - mod_dlnm_lag.png       curva lag-resposta no percentil 75 da exposicao
#     - mod_af.png             fracao atribuivel total / calor / frio (IC95%)
#   Tabelas (vignettes-pt/dados/):
#     - mod_resumo.rds         resumo do ajuste DLNM (especificacao + RR + diagnosticos)
#     - mod_af_resumo.rds      fracao/numero atribuivel por componente (total/calor/frio)
#   Encadeamento (caso condutor):
#     - caso_modelo.rds        saidas tidy do modelo (entrada para relatorios/RAPs)
#
# CASO CONDUTOR (atravessa os 9 tutoriais)
#   "Temperatura e desfechos de saude em idosos (60+) na Regiao Metropolitana de
#   Sao Paulo, 2014-2019" (respiratorias J00-J99 e cardiovasculares I00-I99).
#   Aqui juntamos a SERIE DIARIA DE SAUDE (caso_serie.rds, T4) com a TEMPERATURA
#   da estacao INMET A701 ja alinhada (caso_clima_estacao.rds, T7), construimos a
#   estrutura de lags distribuidos e ajustamos o DLNM quasi-Poisson para estimar a
#   relacao temperatura-mortalidade e a carga atribuivel ao calor e ao frio.
#
# ENTRADAS : dados/caso_serie.rds          (serie de saude diaria do T4)
#            dados/caso_clima_estacao.rds   (saude + clima alinhado do T7; usamos
#                                            $clima_diario: date + tair_dry_bulb_c)
# SAIDA    : dados/caso_modelo.rds          (saidas tidy do modelo, para o T9)
#
# OBS.: TODA chamada a pacote Suggests/estatistico (dlnm, splines, mvmeta, MASS,
#       survival, ggsci, patchwork) e protegida por requireNamespace()/tryCatch e
#       degrada graciosamente (mensagem). Se as entradas encadeadas faltarem,
#       geramos uma serie SINTETICA MINIMA REALISTA (com sinal temperatura-saude)
#       apenas para ilustrar, sem quebrar o pipeline.
# =============================================================================

# --- Infraestrutura compartilhada -------------------------------------------
source("scripts/_setup_tutoriais.R")

COR_PRIMARIA   <- "#2E7D32"   # verde floresta (frio / protecao relativa)
COR_SECUNDARIA <- "#558B2F"   # verde folha
COR_ACENTO     <- "#EF6C00"   # laranja queimado (calor / risco)

# Parametros do caso condutor
ANOS       <- 2014:2019
VAR_TEMP   <- "tair_dry_bulb_c"  # temperatura media do bulbo seco (degC)
DESFECHO   <- "n_obitos"         # contagem diaria de obitos (idosos 60+)
LAG_MAX    <- 21L                # janela de lag (dias) -- efeito agudo + tardio do frio
DOF_ANO    <- 6L                 # gl/ano para controle de sazonalidade (Brasil subtropical)


# =============================================================================
# BLOCO 0 - Carregar as entradas encadeadas (saude do T4 + clima do T7)
# =============================================================================
# Precisamos de DUAS coisas: a serie diaria de saude (contagem de obitos) e a
# serie diaria de temperatura. Carregamos ambas com guarda file.exists(); se
# qualquer uma faltar, geramos uma serie sintetica minima REALISTA (com sinal
# temperatura-mortalidade em V) apenas para ilustrar o ajuste do modelo.
message(">> Bloco 0: carregando saude (caso_serie.rds) e clima (caso_clima_estacao.rds)...")

path_saude <- file.path("vignettes-pt", "dados", "caso_serie.rds")
path_clima <- file.path("vignettes-pt", "dados", "caso_clima_estacao.rds")

# --- Detecta a coluna de data de forma robusta ------------------------------
.detectar_data <- function(df) {
  cand <- intersect(c("date", "data", "dia", "data_obito"), names(df))
  if (length(cand) > 0) return(cand[[1]])
  is_dt <- vapply(df, function(x) inherits(x, c("Date", "POSIXct")), logical(1))
  if (any(is_dt)) return(names(df)[which(is_dt)[1]])
  NA_character_
}

# --- Serie diaria de saude (date + n_obitos) --------------------------------
saude_diaria <- NULL
if (file.exists(path_saude)) {
  caso_serie <- readRDS(path_saude)
  saude_df   <- dplyr::as_tibble(caso_serie)
  col_data   <- .detectar_data(saude_df)
  if (!is.na(col_data)) {
    saude_df[["date"]] <- as.Date(saude_df[[col_data]])
    # Detecta uma coluna de contagem ja agregada; senao conta linhas por dia
    col_cont <- intersect(c("n_obitos", "obitos", "n", "casos", "n_casos", "count"),
                          names(saude_df))
    saude_diaria <- if (length(col_cont) > 0) {
      saude_df |>
        dplyr::summarise(n_obitos = sum(.data[[col_cont[[1]]]], na.rm = TRUE),
                         .by = date)
    } else {
      saude_df |> dplyr::summarise(n_obitos = dplyr::n(), .by = date)
    }
    saude_diaria <- dplyr::arrange(saude_diaria, date)
    message("   Serie de saude carregada de: ", path_saude,
            " (", nrow(saude_diaria), " dias)")
  } else {
    message("   AVISO: coluna de data nao detectada em ", path_saude, ".")
  }
} else {
  message("   AVISO: ", path_saude, " nao encontrado.")
}

# --- Serie diaria de temperatura (date + tair_dry_bulb_c) -------------------
clima_diario <- NULL
if (file.exists(path_clima)) {
  caso_clima <- readRDS(path_clima)
  cd <- tryCatch(caso_clima$clima_diario, error = function(e) NULL)
  if (!is.null(cd) && all(c("date", VAR_TEMP) %in% names(cd))) {
    clima_diario <- dplyr::as_tibble(cd) |>
      dplyr::transmute(date = as.Date(date), tair_dry_bulb_c = .data[[VAR_TEMP]]) |>
      dplyr::arrange(date)
    message("   Serie de clima carregada de: ", path_clima,
            " (", nrow(clima_diario), " dias)")
  } else {
    # tenta extrair de $saude_clima (coluna temp_exact, alinhada no T7)
    sc <- tryCatch(caso_clima$saude_clima, error = function(e) NULL)
    if (!is.null(sc) && "temp_exact" %in% names(sc)) {
      clima_diario <- dplyr::as_tibble(sc) |>
        dplyr::transmute(date = as.Date(date), tair_dry_bulb_c = temp_exact) |>
        dplyr::distinct() |>
        dplyr::arrange(date)
      message("   Serie de clima derivada de caso_clima_estacao$saude_clima.")
    }
  }
} else {
  message("   AVISO: ", path_clima, " nao encontrado.")
}

# --- Fallback sintetico REALISTA (so se faltar saude ou clima) --------------
# Serie diaria de 6 anos para Sao Paulo: temperatura subtropical de altitude
# (Tmean ~ 19 C, inverno mais frio) e mortalidade de idosos com efeito em V:
# excesso de obitos no FRIO (lag distribuido 0-7) e, menor, no CALOR (lag 0-2).
if (is.null(clima_diario) || is.null(saude_diaria)) {
  message("   Gerando serie SINTETICA MINIMA REALISTA (sinal temperatura-saude).")
  set.seed(2014)
  datas <- seq(as.Date("2014-01-01"), as.Date("2019-12-31"), by = "day")
  n     <- length(datas)
  doy   <- as.integer(format(datas, "%j"))
  # Sazonalidade: pico de calor no verao austral (~jan), vale no inverno (~jul)
  temp  <- 19 + 4.5 * cos(2 * pi * (doy - 15) / 365) + stats::rnorm(n, 0, 1.6)

  if (is.null(clima_diario)) {
    clima_diario <- dplyr::tibble(date = datas, tair_dry_bulb_c = round(temp, 2))
  } else {
    # Se ja temos clima real, usa-o para gerar a saude sintetica coerente
    cd2  <- clima_diario |> dplyr::filter(date %in% datas)
    temp <- cd2$tair_dry_bulb_c[match(datas, cd2$date)]
    temp[is.na(temp)] <- stats::median(temp, na.rm = TRUE)
  }

  if (is.null(saude_diaria)) {
    tref      <- 20                       # conforto termico (referencia da V)
    # efeito do FRIO: media da temperatura nos ultimos 0-7 dias (lag distribuido)
    tbar_frio <- stats::filter(temp, rep(1 / 8, 8), sides = 1)
    tbar_frio[is.na(tbar_frio)] <- temp[is.na(tbar_frio)]
    # efeito do CALOR: media curta 0-2 dias (resposta aguda)
    tbar_quente <- stats::filter(temp, rep(1 / 3, 3), sides = 1)
    tbar_quente[is.na(tbar_quente)] <- temp[is.na(tbar_quente)]
    log_mu <- log(12) +
      0.035 * pmax(0, tref - tbar_frio) +     # frio: ~3.5% por degC abaixo de 20
      0.045 * pmax(0, tbar_quente - 26)        # calor: ~4.5% por degC acima de 26
    saude_diaria <- dplyr::tibble(
      date     = datas,
      n_obitos = stats::rpois(n, lambda = exp(log_mu))
    )
  }
}

# --- Junta saude + temperatura por data -------------------------------------
serie <- dplyr::inner_join(saude_diaria, clima_diario, by = "date") |>
  dplyr::arrange(date) |>
  tidyr::drop_na(n_obitos, tair_dry_bulb_c)

message("   Serie integrada saude + temperatura: ", nrow(serie), " dias (",
        format(min(serie$date)), " a ", format(max(serie$date)), ").")


# =============================================================================
# BLOCO 1 - Construir a estrutura de LAGS DISTRIBUIDOS (entrada do DLNM)
# =============================================================================
# sus_mod_dlnm() espera um climasus_df [stage="climate", type="distributed_lag"]
# com a coluna 'date', o desfecho e as colunas {VAR_TEMP}_lag0 ... _lag{LAG_MAX}.
# Em fluxo real essas colunas vem de
#   sus_climate_aggregate(temporal_strategy = "distributed_lag", lag_days = 21).
# Aqui, como a serie ja esta alinhada por data, construimos os lags diretamente
# com dplyr::lag() sobre a serie diaria ordenada.
message(">> Bloco 1: construindo colunas de lag 0-", LAG_MAX, " da temperatura...")

df_lags <- serie
for (k in 0:LAG_MAX) {
  df_lags[[paste0(VAR_TEMP, "_lag", k)]] <- dplyr::lag(serie$tair_dry_bulb_c, n = k)
}
# Mantem apenas date, desfecho e as colunas de lag (descarta a temp 'crua')
df_lags <- df_lags |>
  dplyr::select(date, dplyr::all_of(DESFECHO),
                dplyr::all_of(paste0(VAR_TEMP, "_lag", 0:LAG_MAX)))

# Promove a climasus_df no contrato esperado (stage="climate", distributed_lag).
# Os construtores internos do pacote nao sao exportados; replicamos o padrao
# usado nos demais scripts de tutorial (atributo sus_meta + classe).
df_dl <- dplyr::as_tibble(df_lags)
attr(df_dl, "sus_meta") <- list(
  system  = "SIM",
  stage   = "climate",
  type    = "distributed_lag",
  backend = "tibble",
  history = "[derivado] serie diaria saude + temperatura -> lags distribuidos (T9)"
)
class(df_dl) <- c("climasus_df", class(df_dl))


# =============================================================================
# BLOCO 2 - sus_mod_dlnm(): ajuste do DLNM quasi-Poisson
# =============================================================================
# Crossbasis bidimensional: exposicao em spline natural (df=4, captura o V
# calor+frio) x lag em spline natural (df=3, decaimento suave). Familia
# quasi-Poisson (robusta a superdispersao, Armstrong 2006). Controle de
# sazonalidade via ns(tempo) com dof_per_year=6 (Brasil subtropical).
message(">> Bloco 2: ajustando sus_mod_dlnm() (quasi-Poisson, lag_max=", LAG_MAX, ")...")

tem_dlnm <- all(vapply(c("dlnm", "splines"),
                       function(p) requireNamespace(p, quietly = TRUE),
                       logical(1)))

fit <- NULL
if (tem_dlnm) {
  fit <- tryCatch(
    sus_mod_dlnm(
      df           = df_dl,
      outcome_col  = DESFECHO,
      climate_col  = VAR_TEMP,
      lag_max      = LAG_MAX,
      argvar       = list(fun = "ns", df = 4),
      arglag       = list(fun = "ns", df = 3),
      family       = "quasipoisson",
      dof_per_year = DOF_ANO,
      lang         = "pt",
      verbose      = TRUE
    ),
    error = function(e) {
      message("   sus_mod_dlnm() falhou: ", conditionMessage(e)); NULL
    }
  )
} else {
  message("   Pacotes 'dlnm'/'splines' indisponiveis: DLNM pulado.")
}


# =============================================================================
# BLOCO 3 - sus_mod_af(): fracao atribuivel ao calor e ao frio
# =============================================================================
# A partir do DLNM ajustado, decompoe a carga total de obitos em componentes de
# CALOR (acima do limiar de conforto) e FRIO (abaixo). IC por simulacao de Monte
# Carlo (MASS::mvrnorm). Reduzimos nsim para 200 (exploratorio rapido); use
# 5000 para qualidade de publicacao. Quebra mensal opcional (by="month").
message(">> Bloco 3: sus_mod_af() (fracao atribuivel, Monte Carlo)...")

af <- NULL
if (!is.null(fit)) {
  af <- tryCatch(
    sus_mod_af(
      fit     = fit,
      by      = "month",
      nsim    = 200L,
      lang    = "pt",
      verbose = TRUE
    ),
    error = function(e) {
      message("   sus_mod_af() falhou: ", conditionMessage(e)); NULL
    }
  )
} else {
  message("   DLNM indisponivel: fracao atribuivel pulada.")
}


# =============================================================================
# BLOCO 4 - Figuras do DLNM (construidas a partir do objeto fit)
# =============================================================================
# As figuras sao montadas diretamente dos componentes pre-calculados do fit
# ($pred, $exposure_response, $lag_response) com a paleta verde do projeto, o que
# as torna robustas a ausencia de ggsci/patchwork (deps de sus_mod_plot_dlnm()).
message(">> Bloco 4: figuras do DLNM (surface, overall, lag)...")

if (!is.null(fit) && !is.null(fit$pred)) {
  pred    <- fit$pred
  ref_val <- fit$meta$ref_value

  # --- Fig 1: superficie/contorno log(RR) exposicao x lag ------------------
  lag_seq <- as.integer(seq(pred$lag[1L], pred$lag[2L]))
  exp_vec <- as.numeric(pred$predvar)
  n_exp   <- length(exp_vec); n_lag <- length(lag_seq)
  df_surf <- dplyr::tibble(
    exposicao = rep(exp_vec, times = n_lag),
    lag       = rep(lag_seq, each = n_exp),
    log_rr    = log(as.vector(pred$matRRfit))
  )
  p_surface <- ggplot2::ggplot(df_surf,
                               ggplot2::aes(x = lag, y = exposicao, fill = log_rr)) +
    ggplot2::geom_tile() +
    ggplot2::geom_contour(ggplot2::aes(z = log_rr),
                          breaks = c(-0.2, -0.1, 0, 0.1, 0.2),
                          color = "white", linewidth = 0.3, alpha = 0.6) +
    ggplot2::scale_fill_gradient2(
      low = COR_PRIMARIA, mid = "white", high = COR_ACENTO, midpoint = 0,
      name = "log(RR)"
    ) +
    ggplot2::labs(
      title    = "Superficie de resposta DLNM: temperatura x lag",
      subtitle = "log(RR) de obitos de idosos por temperatura e dias de atraso, RMSP",
      x = "Lag (dias)", y = expression("Temperatura ("*degree*"C)")
    ) +
    tema_climasus()
  save_fig(p_surface, "mod_dlnm_surface", width = 8, height = 5)

  # --- Fig 2: curva exposicao-resposta cumulativa (overall) ----------------
  df_overall <- dplyr::tibble(
    exposicao = as.numeric(pred$predvar),
    rr        = as.numeric(pred$allRRfit),
    lo        = as.numeric(pred$allRRlow),
    hi        = as.numeric(pred$allRRhigh)
  )
  p_overall <- ggplot2::ggplot(df_overall, ggplot2::aes(x = exposicao)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi),
                         fill = COR_SECUNDARIA, alpha = 0.20) +
    ggplot2::geom_line(ggplot2::aes(y = rr), color = COR_PRIMARIA, linewidth = 1) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed",
                        color = COR_ACENTO, linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = ref_val, linetype = "dotted",
                        color = "grey40", linewidth = 0.6) +
    ggplot2::labs(
      title    = "Curva exposicao-resposta cumulativa (RR)",
      subtitle = sprintf("Efeito cumulativo da temperatura sobre obitos de idosos (ref = mediana = %.1f°C)", ref_val),
      x = expression("Temperatura ("*degree*"C)"), y = "Risco relativo (IC 95%)"
    ) +
    tema_climasus()
  save_fig(p_overall, "mod_dlnm_overall", width = 8, height = 5)

  # --- Fig 3: curva lag-resposta no percentil 75 da exposicao --------------
  df_lag <- fit$lag_response
  p_lag <- ggplot2::ggplot(df_lag, ggplot2::aes(x = lag)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi),
                         fill = COR_SECUNDARIA, alpha = 0.20) +
    ggplot2::geom_line(ggplot2::aes(y = rr), color = COR_PRIMARIA, linewidth = 1) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed",
                        color = COR_ACENTO, linewidth = 0.6) +
    ggplot2::labs(
      title    = "Curva lag-resposta (exposicao no percentil 75)",
      subtitle = "RR especifico por dia de atraso entre temperatura e obito",
      x = "Lag (dias)", y = "Risco relativo (IC 95%)"
    ) +
    tema_climasus()
  save_fig(p_lag, "mod_dlnm_lag", width = 8, height = 5)
} else {
  message("   AVISO: figuras do DLNM nao geradas (fit indisponivel).")
}


# =============================================================================
# BLOCO 5 - Figura da fracao atribuivel (total / calor / frio)
# =============================================================================
message(">> Bloco 5: figura da fracao atribuivel...")

if (!is.null(af) && !is.null(af$total)) {
  df_af <- af$total |>
    dplyr::mutate(
      componente = dplyr::recode(component,
                                 total = "Total", heat = "Calor", cold = "Frio"),
      componente = factor(componente, levels = c("Total", "Calor", "Frio"))
    )
  p_af <- ggplot2::ggplot(df_af,
                          ggplot2::aes(x = componente, y = af_pct, fill = componente)) +
    ggplot2::geom_col(width = 0.6, show.legend = FALSE) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = af_pct_lo, ymax = af_pct_hi),
                           width = 0.2, color = "grey30") +
    ggplot2::geom_hline(yintercept = 0, color = "grey40", linewidth = 0.4) +
    ggplot2::scale_fill_manual(values = c(Total = COR_SECUNDARIA,
                                          Calor = COR_ACENTO,
                                          Frio  = COR_PRIMARIA)) +
    ggplot2::labs(
      title    = "Fracao atribuivel de obitos por componente termico",
      subtitle = "Percentual de obitos de idosos atribuivel a temperatura (IC 95% Monte Carlo)",
      x = NULL, y = "Fracao atribuivel (%)"
    ) +
    tema_climasus()
  save_fig(p_af, "mod_af", width = 8, height = 5)
} else {
  message("   AVISO: mod_af.png nao gerado (fracao atribuivel indisponivel).")
}


# =============================================================================
# BLOCO 6 - Tabelas-resumo (mod_resumo, mod_af_resumo)
# =============================================================================
message(">> Bloco 6: tabelas-resumo do modelo...")

if (!is.null(fit)) {
  m <- fit$models
  d <- fit$diagnostics
  mod_resumo <- data.frame(
    Parametro = c("Exposicao climatica", "Desfecho", "Familia GLM",
                  "Lag maximo (dias)", "N observacoes (dias)",
                  "Valor de referencia (°C)",
                  "RR no P75 (vs referencia)", "Lag de efeito pico (dias)",
                  "Razao de dispersao", "AIC (Poisson)"),
    Valor = c(
      fit$meta$climate_col,
      fit$meta$outcome_col,
      fit$meta$family,
      as.character(fit$meta$lag_max),
      as.character(fit$meta$n),
      sprintf("%.1f", fit$meta$ref_value),
      sprintf("%.3f [%.3f; %.3f]", m$rr, m$lo, m$hi),
      as.character(m$lag_peak),
      sprintf("%.2f (%s)", d$disp_ratio, d$disp_category),
      if (!is.na(d$aic_poisson)) sprintf("%.1f", d$aic_poisson) else "NA"
    ),
    stringsAsFactors = FALSE
  )
  save_tbl(mod_resumo, "mod_resumo")
} else {
  message("   AVISO: mod_resumo.rds nao gerado (fit indisponivel).")
}

if (!is.null(af) && !is.null(af$total)) {
  mod_af_resumo <- af$total |>
    dplyr::transmute(
      Componente = dplyr::recode(component,
                                 total = "Total", heat = "Calor", cold = "Frio"),
      `FA (%)`        = sprintf("%.2f [%.2f; %.2f]", af_pct, af_pct_lo, af_pct_hi),
      `NA (obitos)`   = sprintf("%.0f [%.0f; %.0f]", an, an_lo, an_hi),
      `N obitos`      = n_cases
    ) |>
    as.data.frame()
  save_tbl(mod_af_resumo, "mod_af_resumo")
} else {
  message("   AVISO: mod_af_resumo.rds nao gerado.")
}


# =============================================================================
# BLOCO 7 - Encadeamento: salvar saidas tidy do modelo (caso_modelo.rds)
# =============================================================================
# Guardamos apenas as saidas TIDY (leves) do modelo -- nao o objeto glm completo
# -- para alimentar relatorios reprodutiveis (RAPs) e comparacoes futuras.
message(">> Bloco 7: salvando caso_modelo.rds...")

caso_modelo <- list(
  dlnm = if (!is.null(fit)) list(
    models            = fit$models,
    exposure_response = fit$exposure_response,
    lag_response      = fit$lag_response,
    diagnostics       = fit$diagnostics,
    meta              = fit$meta[c("climate_col", "outcome_col", "lag_max",
                                   "ref_value", "family", "ns_df", "n")]
  ) else NULL,
  af = if (!is.null(af)) list(
    total       = af$total,
    by_quantile = af$by_quantile,
    by_period   = af$by_period,
    meta        = af$meta
  ) else NULL,
  meta = list(
    uf        = "SP",
    anos      = ANOS,
    desfecho  = DESFECHO,
    variavel  = VAR_TEMP,
    lag_max   = LAG_MAX,
    n_dias    = nrow(serie),
    fit_ok    = !is.null(fit)
  )
)
save_tbl(caso_modelo, "caso_modelo")

message("== Concluido. Artefatos em vignettes-pt/figuras/ e vignettes-pt/dados/ ==")
