# =============================================================================
# modelagem_01_dlnm.R
# -----------------------------------------------------------------------------
# PROPOSITO
#   Gerar UMA VEZ, localmente, todas as figuras e tabelas canonicas usadas pelo
#   Tutorial de Modelagem 01 (DLNM — Modelo de Defasagem Distribuida Nao-Linear)
#   da vignette climasus4r. Esta e a ETAPA DE MODELAGEM com chapeu bioestatistico
#   e cobre integralmente:
#     - sus_mod_dlnm()        ajuste do DLNM quasi-Poisson temperatura x obitos
#     - sus_mod_plot_dlnm()   todos os type= (overall, lag, surface, contour,
#                              slice, distribution, series)
#   A analise pesada (crossbasis, GLM, crosspred) roda AQUI; o .Rmd apenas
#   carrega os artefatos prontos (PNG via include_graphics, RDS via readRDS).
#
# COMO RODAR
#   Rode a partir da RAIZ do pacote (pasta climasus4r/):
#       Rscript scripts/modelagem_01_dlnm.R
#   ou, em sessao interativa com o working directory na raiz:
#       source("scripts/modelagem_01_dlnm.R")
#
# ARTEFATOS GERADOS  (prefixo "moddlnm_")
#   Figuras (vignettes-pt/figuras/):
#     moddlnm_overall.png       curva exposicao-resposta cumulativa (RR, IC 95%)
#     moddlnm_lag.png           curva lag-resposta no percentil 75
#     moddlnm_contour.png       mapa de contorno bidimensional exposicao x lag
#     moddlnm_slice.png         curvas de resposta por lag especifico (fatiadas)
#     moddlnm_distribution.png  histograma/densidade da exposicao com percentis
#     moddlnm_series.png        serie temporal desfecho + exposicao
#   Tabelas (vignettes-pt/dados/):
#     moddlnm_resumo.rds        resumo do ajuste (especificacao + RR + diagnosticos)
#     moddlnm_expo_resp.rds     curva exposicao-resposta nos percentis pred_at
#   Saida para reuso (caso condutor):
#     dados/mod_dlnm_fit.rds    objeto fit completo (entrada para M2 e M8)
#
# CASO CONDUTOR (atravessa os tutoriais de modelagem)
#   "Temperatura e desfechos de saude em idosos (60+) na Regiao Metropolitana de
#   Sao Paulo, 2014-2019" (respiratorias J00-J99).
#   Entrada encadeada: caso_serie.rds (serie diaria obitos) +
#                      caso_clima_estacao.rds (temperatura INMET A701)
#
# OBS.: TODA chamada a Suggests (dlnm, splines, ggsci, patchwork, plotly, gt)
#       e protegida com requireNamespace()/tryCatch e degrada graciosamente.
#       Se as entradas encadeadas faltarem, uma serie SINTETICA MINIMA REALISTA
#       e gerada para que o script rode sem depender do pipeline anterior.
# =============================================================================

# --- Infraestrutura compartilhada -------------------------------------------
source("scripts/_setup_tutoriais.R")

COR_PRIMARIA   <- "#2E7D32"   # verde floresta
COR_SECUNDARIA <- "#558B2F"   # verde folha
COR_ACENTO     <- "#EF6C00"   # laranja queimado (sinal de calor/risco)

# Parametros do caso condutor
ANOS         <- 2014:2019
VAR_TEMP     <- "tair_dry_bulb_c"   # temperatura media do bulbo seco (degC)
DESFECHO     <- "n_obitos"          # contagem diaria de obitos (idosos 60+)
LAG_MAX      <- 21L                 # janela de lag (dias)
DOF_ANO      <- 4L                  # gl/ano para controle de sazonalidade
ARGVAR       <- list(fun = "ns", df = 4)   # base exposicao (V calor+frio)
ARGLAG       <- list(fun = "ns", df = 3)   # base lag (decaimento suave)
PRED_AT      <- c(0.25, 0.50, 0.75, 0.90, 0.95, 0.99)


# =============================================================================
# BLOCO 0 - Carregar as entradas encadeadas (saude T4 + clima T7)
# =============================================================================
# Precisamos de duas series: obitos diarios (caso_serie.rds) e temperatura
# diaria (caso_clima_estacao.rds). Carregamos com guarda file.exists(); se
# qualquer uma faltar, geramos uma serie sintetica realista para ilustrar.
message(">> Bloco 0: carregando entradas (caso_serie.rds + caso_clima_estacao.rds)...")

path_saude <- file.path("vignettes-pt", "dados", "caso_serie.rds")
path_clima <- file.path("vignettes-pt", "dados", "caso_clima_estacao.rds")

# Helper: detecta coluna de data de forma robusta
.detectar_data <- function(df) {
  cand <- intersect(c("date", "data", "dia", "data_obito"), names(df))
  if (length(cand) > 0) return(cand[[1]])
  is_dt <- vapply(df, function(x) inherits(x, c("Date", "POSIXct")), logical(1))
  if (any(is_dt)) return(names(df)[which(is_dt)[1]])
  NA_character_
}

# --- Serie diaria de saude (date + n_obitos) ---------------------------------
saude_diaria <- NULL
if (file.exists(path_saude)) {
  caso_serie <- readRDS(path_saude)
  saude_df   <- dplyr::as_tibble(caso_serie)
  col_data   <- .detectar_data(saude_df)
  if (!is.na(col_data)) {
    saude_df[["date"]] <- as.Date(saude_df[[col_data]])
    col_cont <- intersect(
      c("n_obitos", "obitos", "n", "casos", "n_casos", "count"),
      names(saude_df)
    )
    saude_diaria <- if (length(col_cont) > 0) {
      saude_df |>
        dplyr::summarise(n_obitos = sum(.data[[col_cont[[1]]]], na.rm = TRUE),
                         .by = date)
    } else {
      saude_df |> dplyr::summarise(n_obitos = dplyr::n(), .by = date)
    }
    saude_diaria <- dplyr::arrange(saude_diaria, date)
    message("   Serie de saude carregada: ", nrow(saude_diaria), " dias.")
  } else {
    message("   AVISO: coluna de data nao detectada em caso_serie.rds.")
  }
} else {
  message("   AVISO: caso_serie.rds nao encontrado.")
}

# --- Serie diaria de temperatura (date + tair_dry_bulb_c) --------------------
clima_diario <- NULL
if (file.exists(path_clima)) {
  caso_clima <- readRDS(path_clima)
  cd <- tryCatch(caso_clima$clima_diario, error = function(e) NULL)
  if (!is.null(cd) && all(c("date", VAR_TEMP) %in% names(cd))) {
    clima_diario <- dplyr::as_tibble(cd) |>
      dplyr::transmute(date = as.Date(date),
                       tair_dry_bulb_c = .data[[VAR_TEMP]]) |>
      dplyr::arrange(date)
    message("   Serie de clima carregada: ", nrow(clima_diario), " dias.")
  } else {
    # tentativa alternativa: $saude_clima$temp_exact
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
  message("   AVISO: caso_clima_estacao.rds nao encontrado.")
}

# --- Fallback sintetico REALISTA (so se faltar saude ou clima) ---------------
# Serie de 6 anos para a RMSP: temperatura subtropical (Tmean ~ 19 C, inverno
# mais frio) e mortalidade de idosos com efeito em V (frio tardio + calor agudo).
if (is.null(clima_diario) || is.null(saude_diaria)) {
  message("   Gerando serie SINTETICA MINIMA REALISTA (sinal V temperatura-saude).")
  set.seed(2014)
  datas <- seq(as.Date("2014-01-01"), as.Date("2019-12-31"), by = "day")
  n     <- length(datas)
  doy   <- as.integer(format(datas, "%j"))
  # Sazonalidade: pico de calor em janeiro, vale em julho
  temp  <- 19 + 4.5 * cos(2 * pi * (doy - 15) / 365) + stats::rnorm(n, 0, 1.6)

  if (is.null(clima_diario)) {
    clima_diario <- dplyr::tibble(date = datas, tair_dry_bulb_c = round(temp, 2))
  } else {
    cd2  <- clima_diario |> dplyr::filter(date %in% datas)
    temp <- cd2$tair_dry_bulb_c[match(datas, cd2$date)]
    temp[is.na(temp)] <- stats::median(temp, na.rm = TRUE)
  }

  if (is.null(saude_diaria)) {
    tref        <- 20
    # efeito do FRIO: media movel 0-7 dias (lag distribuido)
    tbar_frio   <- stats::filter(temp, rep(1 / 8, 8), sides = 1)
    tbar_frio[is.na(tbar_frio)] <- temp[is.na(tbar_frio)]
    # efeito do CALOR: media movel 0-2 dias (resposta aguda)
    tbar_quente <- stats::filter(temp, rep(1 / 3, 3), sides = 1)
    tbar_quente[is.na(tbar_quente)] <- temp[is.na(tbar_quente)]
    log_mu <- log(12) +
      0.035 * pmax(0, tref - tbar_frio) +       # ~3.5%/degC abaixo de 20
      0.045 * pmax(0, tbar_quente - 26)          # ~4.5%/degC acima de 26
    saude_diaria <- dplyr::tibble(
      date     = datas,
      n_obitos = stats::rpois(n, lambda = exp(log_mu))
    )
    message("   Serie de saude sintetica gerada (N = ", nrow(saude_diaria), " dias).")
  }
}

# --- Juntar saude + temperatura por data -------------------------------------
serie <- dplyr::inner_join(saude_diaria, clima_diario, by = "date") |>
  dplyr::arrange(date) |>
  tidyr::drop_na(n_obitos, tair_dry_bulb_c)

message("   Serie integrada: ", nrow(serie), " dias (",
        format(min(serie$date)), " a ", format(max(serie$date)), ").")


# =============================================================================
# BLOCO 1 - Construir a estrutura de LAGS DISTRIBUIDOS
# =============================================================================
# sus_mod_dlnm() espera um climasus_df [stage="climate", type="distributed_lag"]
# com colunas: date, n_obitos, tair_dry_bulb_c_lag0, ..., tair_dry_bulb_c_lag21.
# Em fluxo real essas colunas vem de
#   sus_climate_aggregate(temporal_strategy = "distributed_lag", lag_days = 21).
# Aqui, com a serie ja alinhada, geramos os lags via dplyr::lag().
message(">> Bloco 1: construindo colunas de lag 0-", LAG_MAX, " da temperatura...")

df_lags <- serie
for (k in 0:LAG_MAX) {
  df_lags[[paste0(VAR_TEMP, "_lag", k)]] <- dplyr::lag(serie$tair_dry_bulb_c, n = k)
}

# Mantem apenas date, desfecho e colunas de lag
df_lags <- df_lags |>
  dplyr::select(
    date,
    dplyr::all_of(DESFECHO),
    dplyr::all_of(paste0(VAR_TEMP, "_lag", 0:LAG_MAX))
  )

# Promove a climasus_df (stage="climate", type="distributed_lag")
df_dl <- dplyr::as_tibble(df_lags)
attr(df_dl, "sus_meta") <- list(
  system  = "SIM",
  stage   = "climate",
  type    = "distributed_lag",
  backend = "tibble",
  history = paste0(
    "[", Sys.time(), "] modelagem_01_dlnm.R: serie diaria RMSP 2014-2019 ",
    "-> lags distribuidos tair_dry_bulb_c (lag 0-", LAG_MAX, ")"
  )
)
class(df_dl) <- c("climasus_df", class(df_dl))

message("   df_dl pronto: ", nrow(df_dl), " linhas, ",
        ncol(df_dl), " colunas (", LAG_MAX + 1, " colunas de lag).")


# =============================================================================
# BLOCO 2 - sus_mod_dlnm(): ajuste do DLNM quasi-Poisson
# =============================================================================
# Crossbasis: exposicao = spline natural df=4 (captura curva em V: calor+frio)
#             lag       = spline natural df=3 (decaimento suave, 0-21 dias)
# Familia: quasi-Poisson (robusta a superdispersao; Armstrong, 2006).
# Controle de sazonalidade: ns(tempo, df = dof_per_year * n_anos).
message(">> Bloco 2: ajustando sus_mod_dlnm() (quasi-Poisson, lag_max=", LAG_MAX, ")...")

tem_dlnm <- all(vapply(
  c("dlnm", "splines", "MASS"),
  function(p) requireNamespace(p, quietly = TRUE),
  logical(1)
))

fit <- NULL
if (tem_dlnm) {
  fit <- tryCatch(
    sus_mod_dlnm(
      df           = df_dl,
      outcome_col  = DESFECHO,
      climate_col  = VAR_TEMP,
      lag_max      = LAG_MAX,
      argvar       = ARGVAR,
      arglag       = ARGLAG,
      family       = "quasipoisson",
      dof_per_year = DOF_ANO,
      ref_value    = NULL,      # usa mediana automaticamente
      pred_at      = PRED_AT,
      alpha        = 0.05,
      lang         = "pt",
      verbose      = TRUE
    ),
    error = function(e) {
      message("   ERRO em sus_mod_dlnm(): ", conditionMessage(e))
      NULL
    }
  )
  if (!is.null(fit)) {
    message("   DLNM ajustado. RR(p75) = ", round(fit$models$rr, 3),
            " [", round(fit$models$lo, 3), "; ", round(fit$models$hi, 3), "]",
            " | lag pico: ", fit$models$lag_peak, " dias",
            " | dispersao: ", round(fit$diagnostics$disp_ratio, 2))
  }
} else {
  message("   AVISO: pacotes 'dlnm'/'splines' indisponiveis. DLNM pulado.")
  message("   Instale com: install.packages(c('dlnm', 'splines'))")
}


# =============================================================================
# BLOCO 3 - Figuras via sus_mod_plot_dlnm() — todos os type=
# =============================================================================
# Cada type= de sus_mod_plot_dlnm() e mapeado para um arquivo PNG canonico com
# prefixo "moddlnm_". A funcao e protegida por tryCatch; as figuras criadas
# diretamente via ggplot2 sao o fallback caso ggsci/patchwork faltarem.
message(">> Bloco 3: gerando figuras do DLNM (todos os type=)...")

tem_plot_deps <- all(vapply(
  c("ggplot2", "ggsci", "patchwork"),
  function(p) requireNamespace(p, quietly = TRUE),
  logical(1)
))

if (!is.null(fit) && !is.null(fit$pred)) {

  # --- Figura 1: curva exposicao-resposta cumulativa (overall) ---------------
  if (tem_plot_deps) {
    p_overall <- tryCatch(
      sus_mod_plot_dlnm(fit, type = "overall", output_type = "plot",
                        interactive = FALSE, lang = "pt"),
      error = function(e) { message("   AVISO overall: ", conditionMessage(e)); NULL }
    )
  } else {
    # Fallback manual com ggplot2 puro
    df_overall <- dplyr::tibble(
      exposicao = as.numeric(fit$pred$predvar),
      rr = as.numeric(fit$pred$allRRfit),
      lo = as.numeric(fit$pred$allRRlow),
      hi = as.numeric(fit$pred$allRRhigh)
    )
    p_overall <- ggplot2::ggplot(df_overall, ggplot2::aes(x = exposicao)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi),
                           fill = COR_SECUNDARIA, alpha = 0.20) +
      ggplot2::geom_line(ggplot2::aes(y = rr), color = COR_PRIMARIA, linewidth = 1) +
      ggplot2::geom_hline(yintercept = 1, linetype = "dashed",
                          color = COR_ACENTO, linewidth = 0.7) +
      ggplot2::geom_vline(xintercept = fit$meta$ref_value,
                          linetype = "dotted", color = "grey40", linewidth = 0.6) +
      ggplot2::labs(
        title    = "Curva exposicao-resposta cumulativa (RR)",
        subtitle = sprintf("DLNM quasi-Poisson | referencia = %.1f°C (mediana)",
                           fit$meta$ref_value),
        x = expression("Temperatura ("*degree*"C)"),
        y = "Risco Relativo (IC 95%)",
        caption = "Gasparrini et al. (2010) Stat Med"
      ) +
      tema_climasus()
  }
  if (!is.null(p_overall)) save_fig(p_overall, "moddlnm_overall", width = 8, height = 5)

  # --- Figura 2: curva lag-resposta no percentil 75 --------------------------
  if (tem_plot_deps) {
    p_lag <- tryCatch(
      sus_mod_plot_dlnm(fit, type = "lag", output_type = "plot",
                        interactive = FALSE, lang = "pt"),
      error = function(e) { message("   AVISO lag: ", conditionMessage(e)); NULL }
    )
  } else {
    df_lag <- fit$lag_response
    p_lag <- ggplot2::ggplot(df_lag, ggplot2::aes(x = lag)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi),
                           fill = COR_SECUNDARIA, alpha = 0.20) +
      ggplot2::geom_line(ggplot2::aes(y = rr), color = COR_PRIMARIA, linewidth = 1) +
      ggplot2::geom_hline(yintercept = 1, linetype = "dashed",
                          color = COR_ACENTO, linewidth = 0.6) +
      ggplot2::labs(
        title    = "Curva lag-resposta (percentil 75 da exposicao)",
        subtitle = "RR especifico por dia de atraso | RMSP 2014-2019",
        x = "Lag (dias)", y = "Risco Relativo (IC 95%)",
        caption = "Gasparrini et al. (2010); Bhaskaran et al. (2013)"
      ) +
      tema_climasus()
  }
  if (!is.null(p_lag)) save_fig(p_lag, "moddlnm_lag", width = 8, height = 5)

  # --- Figura 3: mapa de contorno (contour) -----------------------------------
  if (tem_plot_deps) {
    p_contour <- tryCatch(
      sus_mod_plot_dlnm(fit, type = "contour", output_type = "plot",
                        interactive = FALSE, lang = "pt"),
      error = function(e) { message("   AVISO contour: ", conditionMessage(e)); NULL }
    )
  } else {
    lag_seq  <- as.integer(seq(fit$pred$lag[1L], fit$pred$lag[2L]))
    exp_vec  <- as.numeric(fit$pred$predvar)
    n_exp    <- length(exp_vec); n_lag <- length(lag_seq)
    df_cont  <- dplyr::tibble(
      exposicao = rep(exp_vec, times = n_lag),
      lag       = rep(lag_seq, each  = n_exp),
      log_rr    = log(as.vector(fit$pred$matRRfit))
    )
    p_contour <- ggplot2::ggplot(df_cont,
                                 ggplot2::aes(x = lag, y = exposicao, fill = log_rr)) +
      ggplot2::geom_tile() +
      ggplot2::geom_contour(ggplot2::aes(z = log_rr),
                            breaks = c(-0.2, -0.1, 0, 0.1, 0.2),
                            color = "white", linewidth = 0.3, alpha = 0.6) +
      ggplot2::scale_fill_gradient2(low = COR_PRIMARIA, mid = "white",
                                    high = COR_ACENTO, midpoint = 0,
                                    name = "log(RR)") +
      ggplot2::labs(
        title    = "Mapa de contorno DLNM: temperatura x lag",
        subtitle = "log(RR) de obitos de idosos por temperatura e dias de atraso",
        x = "Lag (dias)", y = expression("Temperatura ("*degree*"C)"),
        caption = "Gasparrini (2011); Gasparrini et al. (2014)"
      ) +
      tema_climasus()
  }
  if (!is.null(p_contour)) save_fig(p_contour, "moddlnm_contour", width = 8, height = 5)

  # --- Figura 4: curvas de resposta por lag especifico (slice) ----------------
  if (tem_plot_deps) {
    p_slice <- tryCatch(
      sus_mod_plot_dlnm(fit, type = "slice", output_type = "plot",
                        lags_at = c(0L, 3L, 7L, 14L, 21L),
                        interactive = FALSE, lang = "pt"),
      error = function(e) { message("   AVISO slice: ", conditionMessage(e)); NULL }
    )
    if (!is.null(p_slice)) save_fig(p_slice, "moddlnm_slice", width = 8, height = 5)
  } else {
    message("   INFO: slice nao gerado (ggsci/patchwork indisponivel).")
  }

  # --- Figura 5: distribuicao da exposicao ------------------------------------
  if (tem_plot_deps) {
    p_dist <- tryCatch(
      sus_mod_plot_dlnm(fit, type = "distribution", output_type = "plot",
                        interactive = FALSE, lang = "pt"),
      error = function(e) { message("   AVISO distribution: ", conditionMessage(e)); NULL }
    )
  } else {
    lag0_col <- paste0(VAR_TEMP, "_lag0")
    df_dist  <- dplyr::tibble(exposicao = fit$data_daily[[lag0_col]])
    qtls     <- stats::quantile(df_dist$exposicao,
                                c(0.10, 0.25, 0.50, 0.75, 0.90, 0.99), na.rm = TRUE)
    p_dist <- ggplot2::ggplot(df_dist, ggplot2::aes(x = exposicao)) +
      ggplot2::geom_histogram(bins = 35, fill = COR_PRIMARIA, alpha = 0.45,
                              color = "white", linewidth = 0.2) +
      ggplot2::geom_vline(xintercept = as.numeric(qtls),
                          linetype = "dashed", color = COR_ACENTO,
                          linewidth = 0.45, alpha = 0.7) +
      ggplot2::labs(
        title    = "Distribuicao da temperatura diaria (RMSP, 2014-2019)",
        subtitle = "Histograma com percentis-chave marcados (P10, P25, P50, P75, P90, P99)",
        x = expression("Temperatura ("*degree*"C)"), y = "Frequencia",
        caption = "Bhaskaran et al. (2013)"
      ) +
      tema_climasus()
  }
  if (!is.null(p_dist)) save_fig(p_dist, "moddlnm_distribution", width = 8, height = 5)

  # --- Figura 6: serie temporal desfecho + exposicao (series) ----------------
  if (tem_plot_deps) {
    p_series <- tryCatch(
      sus_mod_plot_dlnm(fit, type = "series", output_type = "plot",
                        interactive = FALSE, lang = "pt"),
      error = function(e) { message("   AVISO series: ", conditionMessage(e)); NULL }
    )
  } else {
    lag0_col <- paste0(VAR_TEMP, "_lag0")
    df_ts    <- fit$data_daily |>
      dplyr::select(date, y, dplyr::any_of(lag0_col)) |>
      dplyr::arrange(date)
    y_range   <- range(df_ts$y, na.rm = TRUE)
    exp_range <- range(df_ts[[lag0_col]], na.rm = TRUE)
    df_ts$exp_norm <- (df_ts[[lag0_col]] - exp_range[1]) /
                      diff(exp_range) * diff(y_range) + y_range[1]
    p_series <- ggplot2::ggplot(df_ts, ggplot2::aes(x = date)) +
      ggplot2::geom_col(ggplot2::aes(y = y),
                        fill = COR_PRIMARIA, alpha = 0.5, width = 1) +
      ggplot2::geom_line(ggplot2::aes(y = exp_norm),
                         color = COR_ACENTO, linewidth = 0.6, alpha = 0.8) +
      ggplot2::scale_x_date(date_labels = "%b\n%Y", date_breaks = "6 months") +
      ggplot2::labs(
        title    = "Serie temporal: obitos de idosos e temperatura (RMSP)",
        subtitle = "Barras = n. obitos diarios; linha = temperatura media (eixo secundario escalado)",
        x = NULL, y = "Obitos diarios",
        caption = "Bhaskaran et al. (2013)"
      ) +
      tema_climasus()
  }
  if (!is.null(p_series)) save_fig(p_series, "moddlnm_series", width = 10, height = 5)

} else {
  message("   AVISO: figuras nao geradas (fit ou pred indisponivel).")
}


# =============================================================================
# BLOCO 4 - Tabelas-resumo do DLNM
# =============================================================================
message(">> Bloco 4: tabelas-resumo...")

if (!is.null(fit)) {
  m <- fit$models
  d <- fit$diagnostics

  # Tabela 1: especificacao + resumo estatistico do modelo
  moddlnm_resumo <- data.frame(
    Parametro = c(
      "Exposicao climatica",
      "Desfecho",
      "Familia GLM",
      "Lag maximo (dias)",
      "Base exposicao (argvar)",
      "Base lag (arglag)",
      "gl/ano (sazonalidade)",
      "N observacoes (dias)",
      "Valor de referencia (degC)",
      "RR cumulativo no P75",
      "IC 95% RR (P75)",
      "Lag de efeito pico (dias)",
      "Razao de dispersao (phi)",
      "Categoria dispersao",
      "AIC (Poisson re-fit)",
      "Ljung-Box p-valor",
      "Autocorrelacao residual"
    ),
    Valor = c(
      fit$meta$climate_col,
      fit$meta$outcome_col,
      fit$meta$family,
      as.character(fit$meta$lag_max),
      paste0("ns(df=", ARGVAR$df, ")"),
      paste0("ns(df=", ARGLAG$df, ")"),
      as.character(DOF_ANO),
      as.character(fit$meta$n),
      sprintf("%.1f", fit$meta$ref_value),
      sprintf("%.4f", m$rr),
      sprintf("[%.4f; %.4f]", m$lo, m$hi),
      as.character(m$lag_peak),
      sprintf("%.3f", d$disp_ratio),
      d$disp_category,
      if (!is.na(d$aic_poisson)) sprintf("%.1f", d$aic_poisson) else "NA",
      sprintf("%.4f", d$autocorr_pval),
      if (isTRUE(d$has_autocorr)) "Sim (p < 0.05)" else "Nao"
    ),
    stringsAsFactors = FALSE
  )
  save_tbl(moddlnm_resumo, "moddlnm_resumo")

  # Tabela 2: curva exposicao-resposta (RR nos percentis pred_at)
  save_tbl(fit$exposure_response, "moddlnm_expo_resp")

  message("   Tabelas salvas: moddlnm_resumo.rds, moddlnm_expo_resp.rds")
} else {
  message("   AVISO: tabelas nao geradas (fit indisponivel).")
}


# =============================================================================
# BLOCO 5 - Encadeamento: salvar mod_dlnm_fit.rds (entrada para M2 e M8)
# =============================================================================
# O objeto fit completo (climasus_dlnm) e salvo para reuso nos tutoriais
# de modelagem M2 (fracao atribuivel) e M8 (meta-analise multi-cidade).
# Se o fit nao estiver disponivel, salva um objeto mínimo informativo.
message(">> Bloco 5: salvando dados/mod_dlnm_fit.rds...")

if (!is.null(fit)) {
  save_tbl(fit, "mod_dlnm_fit")
  # Salva tambem na raiz dados/ para encadeamento com tutoriais M2 e M8
  dir.create("dados", showWarnings = FALSE, recursive = TRUE)
  saveRDS(fit, file.path("dados", "mod_dlnm_fit.rds"))
  message("   mod_dlnm_fit.rds salvo com sucesso.")
  message("   Componentes: $model, $crossbasis, $pred, $exposure_response,")
  message("                $lag_response, $models, $data_daily, $diagnostics, $meta")
  message("   Caminhos: vignettes-pt/dados/mod_dlnm_fit.rds  e  dados/mod_dlnm_fit.rds")
} else {
  mod_dlnm_fit_vazio <- list(
    disponivel = FALSE,
    motivo     = "DLNM nao ajustado (dlnm/splines/MASS indisponiveis ou erro no ajuste)",
    call_time  = Sys.time()
  )
  save_tbl(mod_dlnm_fit_vazio, "mod_dlnm_fit")
  dir.create("dados", showWarnings = FALSE, recursive = TRUE)
  saveRDS(mod_dlnm_fit_vazio, file.path("dados", "mod_dlnm_fit.rds"))
  message("   AVISO: mod_dlnm_fit.rds salvo como objeto vazio (fit indisponivel).")
}

message("")
message("== Concluido. Artefatos (prefixo moddlnm_) em: ==")
message("   vignettes-pt/figuras/  (PNG: overall, lag, contour, slice, distribution, series)")
message("   vignettes-pt/dados/    (RDS: moddlnm_resumo, moddlnm_expo_resp, mod_dlnm_fit)")
message("   dados/                 (RDS: mod_dlnm_fit  <- encadeamento M2 e M8)")
