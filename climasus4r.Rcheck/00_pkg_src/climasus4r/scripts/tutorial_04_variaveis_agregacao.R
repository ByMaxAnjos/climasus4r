# =============================================================================
# tutorial_04_variaveis_agregacao.R
# -----------------------------------------------------------------------------
# PROPOSITO
#   Gerar UMA VEZ, localmente, todas as figuras e tabelas canonicas usadas pelo
#   Tutorial 4 (Criacao de Variaveis e Agregacao) da vignette climasus4r. A
#   etapa cobre quatro funcoes do pipeline:
#       - sus_data_create_variables()   cria faixas etarias e variaveis de
#                                       calendario / sazonais
#       - sus_data_aggregate()          agrega microdados individuais em serie
#                                       temporal (contagens por data/municipio)
#       - sus_data_plot_aggregate_ts()  visualiza a serie temporal agregada
#       - sus_data_plot_aggregate_map() mapeia a distribuicao espacial agregada
#   A analise pesada roda AQUI; o arquivo .Rmd apenas carrega os artefatos
#   prontos (PNG via include_graphics, RDS via readRDS).
#
# COMO RODAR
#   Rode a partir da RAIZ do pacote (pasta climasus4r/):
#       Rscript scripts/tutorial_04_variaveis_agregacao.R
#   ou, em sessao interativa com o working directory na raiz:
#       source("scripts/tutorial_04_variaveis_agregacao.R")
#
# ARTEFATOS GERADOS
#   Figuras (vignettes-pt/figuras/):
#     - agg_serie_diaria.png       curva epidemica diaria (sus_data_plot_aggregate_ts)
#     - agg_sazonalidade.png       boxplot sazonal mensal (sus_data_plot_aggregate_ts)
#     - agg_serie_faixa.png        serie mensal por faixa etaria (ggplot direto)
#     - agg_mapa_municipios.png    mapa de bolhas municipal (sus_data_plot_aggregate_map)
#   Tabelas (vignettes-pt/dados/):
#     - agg_resumo.rds             resumo da agregacao (n. de periodos, total de eventos)
#     - agg_amostra_variaveis.rds  amostra das variaveis derivadas (faixa etaria/calendario)
#   Encadeamento (caso condutor):
#     - caso_serie.rds             serie temporal agregada, entrada do Tutorial 5
#
# CASO CONDUTOR (atravessa os 9 tutoriais)
#   "Temperatura e desfechos de saude em idosos (60+) na Regiao Metropolitana de
#   Sao Paulo, 2014-2019". Nesta etapa partimos dos OBITOS RESPIRATORIOS de
#   idosos JA FILTRADOS no Tutorial 3 (caso_filtrado.rds), criamos faixas
#   etarias e variaveis de calendario e agregamos em serie temporal diaria e
#   mensal por municipio. A serie resultante (caso_serie.rds) alimenta a junçao
#   espacial (Tutorial 5) e, mais adiante, a modelagem clima-saude (Tutorial 9).
#
# OBSERVACAO SOBRE ENCADEAMENTO
#   Os Tutoriais 2 e 3 podem ainda nao ter sido executados. Este script tenta
#   carregar dados/caso_filtrado.rds (guarda file.exists()); se ausente, avisa e
#   segue com um EXEMPLO MINIMO sintetico que tem a mesma estrutura de colunas
#   padronizadas (data_obito, codigo_municipio_residencia, idade, sexo). Assim o
#   tutorial sempre renderiza e ilustra a etapa de ponta a ponta.
# =============================================================================

# --- Infraestrutura compartilhada -------------------------------------------
# Carrega pacotes (climasus4r, ggplot2, dplyr), tema_climasus(), paleta verde e
# os helpers save_fig() e save_tbl(). Cria figuras/ e dados/ se faltarem.
source("scripts/_setup_tutoriais.R")

# Cores da paleta "Climate Forest" usadas em destaques pontuais
COR_PRIMARIA   <- "#2E7D32"
COR_SECUNDARIA <- "#558B2F"
COR_ACENTO     <- "#EF6C00"

# Janela do estudo
ANOS <- 2014:2019


# =============================================================================
# BLOCO 0 - Carregar a entrada encadeada (saida do Tutorial 3)
# =============================================================================
# Caminho do .rds produzido pela etapa de filtragem. A guarda file.exists()
# garante que o script roda mesmo antes de o Tutorial 3 ter sido executado.
message(">> Bloco 0: carregando a entrada encadeada (caso_filtrado.rds)...")

entrada_path <- file.path("vignettes-pt", "dados", "caso_filtrado.rds")

# Helper: promove um data.frame "padronizado/filtrado" a climasus_df no stage
# "stand", que e o minimo exigido por sus_data_create_variables() e
# sus_data_aggregate(). Em uso real, esse stage ja vem do Tutorial 2/3.
.como_climasus_stand <- function(d, system = "SIM-DO") {
  if (inherits(d, "climasus_df")) return(d)
  d <- dplyr::as_tibble(d)
  ns <- asNamespace("climasus4r")
  if (exists("new_climasus_df", envir = ns, mode = "function")) {
    construtor <- get("new_climasus_df", envir = ns)
    meta <- list(
      system  = system,
      stage   = "stand",
      type    = "stand",
      backend = "tibble",
      history = sprintf("[%s] minimal example (tutorial 04)",
                        format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      user    = list()
    )
    return(construtor(d, meta))
  }
  d
}

# Gera um exemplo minimo sintetico com a estrutura de colunas PADRONIZADAS que
# a etapa de filtragem (Tutorial 3) entregaria: obitos respiratorios de idosos
# na Regiao Metropolitana de Sao Paulo. Usado apenas quando caso_filtrado.rds
# ainda nao existe, para que o pipeline ilustrativo rode de ponta a ponta.
.exemplo_minimo <- function(anos = ANOS) {
  set.seed(2024)
  # Alguns municipios da RMSP (codigo IBGE de 6 digitos)
  munis <- c("355030",  # Sao Paulo
             "351380",  # Guarulhos
             "351880",  # Osasco
             "350950",  # Santo Andre
             "354870")  # Sao Bernardo do Campo
  datas_possiveis <- seq(as.Date(paste0(min(anos), "-01-01")),
                         as.Date(paste0(max(anos), "-12-31")),
                         by = "day")
  n <- 6000L
  # Sazonalidade de inverno: maior risco em jun-ago (mortalidade respiratoria)
  peso_mes <- c(0.6, 0.6, 0.7, 0.9, 1.2, 1.6, 1.8, 1.6, 1.1, 0.9, 0.7, 0.6)
  pesos <- peso_mes[as.integer(format(datas_possiveis, "%m"))]
  datas <- sample(datas_possiveis, n, replace = TRUE, prob = pesos)
  dplyr::tibble(
    data_obito                  = datas,
    codigo_municipio_residencia = sample(munis, n, replace = TRUE,
                                          prob = c(0.55, 0.15, 0.12, 0.10, 0.08)),
    idade                       = sample(60:99, n, replace = TRUE),
    age_years                   = NA_real_,  # idade ja em anos via coluna 'idade'
    sexo                        = sample(c("Masculino", "Feminino"), n, replace = TRUE),
    causa_basica                = sample(sprintf("J%02d", 0:99), n, replace = TRUE)
  ) |>
    dplyr::mutate(age_years = idade)
}

usou_exemplo_minimo <- FALSE
if (file.exists(entrada_path)) {
  caso_filtrado <- readRDS(entrada_path)
  # A saida do Tutorial 3 pode ser uma lista (varios desfechos) ou um df unico.
  # Para esta etapa usamos o desfecho principal: obitos respiratorios de idosos.
  base_obitos <- if (is.list(caso_filtrado) && !is.data.frame(caso_filtrado)) {
    caso_filtrado$respiratorio %||% caso_filtrado$obitos %||% caso_filtrado[[1]]
  } else {
    caso_filtrado
  }
  message("   Entrada carregada de: ", entrada_path)
} else {
  usou_exemplo_minimo <- TRUE
  base_obitos <- .exemplo_minimo()
  message("   AVISO: ", entrada_path, " nao encontrado.")
  message("          Usando EXEMPLO MINIMO sintetico (rode os Tutoriais 2-3 para o caso real).")
}

# Guarda adicional: a entrada pode existir porem estar vazia (Tutorial 3
# degradado, p.ex. sem rede). Nesse caso caimos para o exemplo minimo, para que
# o tutorial sempre ilustre a etapa de ponta a ponta.
if (is.null(base_obitos) || !is.data.frame(base_obitos) || nrow(base_obitos) == 0) {
  usou_exemplo_minimo <- TRUE
  base_obitos <- .exemplo_minimo()
  message("   AVISO: entrada encadeada vazia/invalida (Tutorial 3 degradado?).")
  message("          Usando EXEMPLO MINIMO sintetico (rode os Tutoriais 2-3 para o caso real).")
}

# Garante o stage minimo ("stand") exigido pelas funcoes desta etapa.
base_obitos <- .como_climasus_stand(base_obitos, system = "SIM-DO")


# =============================================================================
# BLOCO 1 - Criar variaveis derivadas: faixas etarias + calendario + sazonais
# =============================================================================
# sus_data_create_variables() cria, a partir da idade e da data do evento:
#   - faixas etarias do usuario (age_breaks / age_labels via cut())
#   - faixa de risco climatico (grupo_risco_climatico) e faixa IBGE quinquenal
#   - variaveis de calendario (ano, mes, dia_semana, trimestre, ...)
#   - sazonalidade (estacao_astronomica ou estacao_climatica + estacao_seca_chuvosa
#     quando climate_region e informada)
# Aqui usamos faixas voltadas ao recorte de idosos do caso condutor.
message(">> Bloco 1: criando variaveis derivadas (faixas etarias e calendario)...")

obitos_vars <- tryCatch(
  sus_data_create_variables(
    base_obitos,
    create_age_groups    = TRUE,
    age_breaks           = c(60, 70, 80, Inf),     # idosos jovens / 70-79 / 80+
    age_labels           = c("60-69", "70-79", "80+"),
    create_calendar_vars = TRUE,                   # ano, mes, trimestre, ...
    create_climate_vars  = TRUE,                   # estacao do ano
    climate_region       = "sudeste",              # regiao climatica do caso
    hemisphere           = "south",                # Brasil
    backend              = "tibble",
    lang                 = "pt",
    verbose              = TRUE
  ),
  error = function(e) {
    message("   sus_data_create_variables() falhou: ", conditionMessage(e))
    base_obitos
  }
)

obitos_vars_df <- dplyr::as_tibble(obitos_vars)

# Amostra das variaveis derivadas para a vignette (tabela agg_amostra_variaveis).
col_derivadas <- intersect(
  c("data_obito", "idade", "age_years", "age_group",
    "grupo_risco_climatico", "faixa_etaria_ibge",
    "ano", "mes", "trimestre", "estacao_climatica", "estacao_seca_chuvosa",
    "estacao_astronomica"),
  names(obitos_vars_df)
)
amostra_variaveis <- obitos_vars_df |>
  dplyr::select(dplyr::all_of(col_derivadas)) |>
  utils::head(10) |>
  as.data.frame()
save_tbl(amostra_variaveis, "agg_amostra_variaveis")


# =============================================================================
# BLOCO 2 - Agregar em serie temporal DIARIA (contagem de obitos por municipio)
# =============================================================================
# sus_data_aggregate() agrega os microdados individuais em contagens por
# periodo. Com fun = "count" o nome da coluna agregada e inferido do sistema
# (SIM -> n_obitos). complete_dates = TRUE preenche dias sem obito com zero,
# produzindo uma serie SEM lacunas (essencial para DLNM no Tutorial 9).
message(">> Bloco 2: agregando em serie temporal diaria...")

serie_diaria <- tryCatch(
  sus_data_aggregate(
    obitos_vars,
    time_unit      = "day",      # uma observacao por dia
    fun            = "count",    # contagem de registros (obitos)
    group_by       = NULL,       # agrega no codigo de municipio detectado
    complete_dates = TRUE,       # preenche dias sem evento com zero
    backend        = "tibble",
    lang           = "pt",
    verbose        = TRUE
  ),
  error = function(e) {
    message("   agregacao diaria falhou: ", conditionMessage(e)); NULL
  }
)


# =============================================================================
# BLOCO 3 - Agregar em serie MENSAL por faixa etaria (para encadeamento)
# =============================================================================
# Mesma funcao, agora com time_unit = "month" e group_by = "age_group". Esta e
# a serie que SALVAMOS como caso_serie.rds para o proximo tutorial.
message(">> Bloco 3: agregando em serie mensal por faixa etaria...")

serie_mensal <- tryCatch(
  sus_data_aggregate(
    obitos_vars,
    time_unit      = "month",
    fun            = "count",
    group_by       = "age_group",   # faixa etaria criada no Bloco 1
    complete_dates = TRUE,
    backend        = "tibble",
    lang           = "pt",
    verbose        = TRUE
  ),
  error = function(e) {
    message("   agregacao mensal falhou: ", conditionMessage(e)); NULL
  }
)


# =============================================================================
# BLOCO 4 - Tabela-resumo da agregacao
# =============================================================================
message(">> Bloco 4: tabela-resumo da agregacao...")

.resumo_de <- function(df_agg, rotulo, unidade) {
  if (is.null(df_agg)) return(NULL)
  d <- dplyr::as_tibble(df_agg)
  col_n <- intersect(c("n_obitos", "n", "count"), names(d))[1]
  data.frame(
    Serie            = rotulo,
    Unidade_temporal = unidade,
    N_periodos       = length(unique(d$date)),
    Total_eventos    = if (!is.na(col_n)) sum(d[[col_n]], na.rm = TRUE) else NA_integer_,
    Inicio           = as.character(min(d$date, na.rm = TRUE)),
    Fim              = as.character(max(d$date, na.rm = TRUE)),
    stringsAsFactors = FALSE
  )
}

resumo <- dplyr::bind_rows(
  .resumo_de(serie_diaria, "Diaria (todos os obitos)",      "dia"),
  .resumo_de(serie_mensal, "Mensal (por faixa etaria)",     "mes")
)
if (nrow(resumo) == 0) {
  resumo <- data.frame(
    Serie = "indisponivel",
    Observacao = "Agregacao nao pode ser executada nesta sessao.",
    stringsAsFactors = FALSE
  )
}
save_tbl(resumo, "agg_resumo")


# =============================================================================
# BLOCO 5 - Figura: serie temporal diaria (sus_data_plot_aggregate_ts)
# =============================================================================
# sus_data_plot_aggregate_ts() produz visualizacoes de qualidade de publicacao
# a partir de um climasus_df no stage "aggregate". plot_type = "epidemic"
# desenha a curva epidemica (area + linha + suavizacao loess).
message(">> Bloco 5: figura da serie temporal diaria (curva epidemica)...")

if (!is.null(serie_diaria)) {
  p_serie <- tryCatch(
    sus_data_plot_aggregate_ts(
      serie_diaria,
      plot_type     = "epidemic",   # curva epidemica
      smooth_method = "loess",      # tendencia suavizada
      palette       = "lancet",
      lang          = "pt",
      verbose       = FALSE
    ),
    error = function(e) {
      message("   plot epidemic falhou: ", conditionMessage(e)); NULL
    }
  )
  if (!is.null(p_serie)) {
    save_fig(p_serie, "agg_serie_diaria", width = 8, height = 5)
  } else {
    message("   AVISO: agg_serie_diaria.png NAO foi gerado.")
  }
} else {
  message("   AVISO: agg_serie_diaria.png NAO foi gerado (serie diaria ausente).")
}


# =============================================================================
# BLOCO 6 - Figura: sazonalidade mensal (sus_data_plot_aggregate_ts)
# =============================================================================
# Mesmo motor de plotagem, agora com plot_type = "seasonal": boxplot da
# distribuicao mensal, util para evidenciar a sazonalidade de inverno da
# mortalidade respiratoria de idosos.
message(">> Bloco 6: figura de sazonalidade mensal (boxplot)...")

if (!is.null(serie_diaria)) {
  p_sazonal <- tryCatch(
    sus_data_plot_aggregate_ts(
      serie_diaria,
      plot_type = "seasonal",   # boxplot mensal
      palette   = "lancet",
      lang      = "pt",
      verbose   = FALSE
    ),
    error = function(e) {
      message("   plot seasonal falhou: ", conditionMessage(e)); NULL
    }
  )
  if (!is.null(p_sazonal)) {
    save_fig(p_sazonal, "agg_sazonalidade", width = 8, height = 5)
  } else {
    message("   AVISO: agg_sazonalidade.png NAO foi gerado.")
  }
}


# =============================================================================
# BLOCO 7 - Figura: serie mensal por faixa etaria (ggplot direto)
# =============================================================================
# Complementa a figura padrao com uma serie estratificada por faixa etaria, que
# e a estrutura salva para o proximo tutorial. Construida com ggplot2 + tema do
# projeto para manter a identidade visual "Climate Forest".
message(">> Bloco 7: figura da serie mensal por faixa etaria...")

if (!is.null(serie_mensal)) {
  d_mensal <- dplyr::as_tibble(serie_mensal)
  col_n    <- intersect(c("n_obitos", "n", "count"), names(d_mensal))[1]
  if (!is.na(col_n) && "age_group" %in% names(d_mensal)) {
    p_faixa <- ggplot2::ggplot(
      d_mensal,
      ggplot2::aes(x = as.Date(date), y = .data[[col_n]],
                   color = age_group, group = age_group)
    ) +
      ggplot2::geom_line(linewidth = 0.7, na.rm = TRUE) +
      ggplot2::scale_color_manual(values = cf_colors, name = "Faixa etaria") +
      ggplot2::labs(
        title    = "Obitos respiratorios mensais por faixa etaria",
        subtitle = "Idosos (60+), Regiao Metropolitana de Sao Paulo, 2014-2019",
        x        = "Mes",
        y        = "Numero de obitos"
      ) +
      tema_climasus()
    save_fig(p_faixa, "agg_serie_faixa", width = 8, height = 5)
  } else {
    message("   AVISO: agg_serie_faixa.png NAO foi gerado (coluna de contagem ou age_group ausente).")
  }
}


# =============================================================================
# BLOCO 8 - Figura: mapa municipal (sus_data_plot_aggregate_map)
# =============================================================================
# sus_data_plot_aggregate_map() mapeia a distribuicao espacial das contagens
# agregadas por municipio. map_type = "bubble" nao exige geobr/sf; o coropletico
# exige esses pacotes (Suggests) e cai para bolhas se ausentes. Protegemos a
# chamada porque ela acessa metadados municipais (cache/rede).
message(">> Bloco 8: figura do mapa municipal (bolhas)...")

# Para o mapa precisamos de uma serie agregada que PRESERVE o codigo de
# municipio como coluna. Agregamos por mes E por codigo de municipio.
serie_muni <- tryCatch(
  sus_data_aggregate(
    obitos_vars,
    time_unit      = "month",
    fun            = "count",
    group_by       = "codigo_municipio_residencia",
    complete_dates = FALSE,
    backend        = "tibble",
    lang           = "pt",
    verbose        = FALSE
  ),
  error = function(e) {
    message("   agregacao por municipio falhou: ", conditionMessage(e)); NULL
  }
)

if (!is.null(serie_muni)) {
  p_mapa <- tryCatch(
    sus_data_plot_aggregate_map(
      serie_muni,
      map_type      = "bubble",   # nao exige geobr/sf
      log_scale     = TRUE,
      palette       = "YlOrRd",
      state_borders = TRUE,       # cai para bolhas sem fronteiras se geobr ausente
      lang          = "pt",
      verbose       = FALSE
    ),
    error = function(e) {
      message("   mapa municipal falhou (metadados/rede indisponiveis?): ",
              conditionMessage(e)); NULL
    }
  )
  if (!is.null(p_mapa)) {
    save_fig(p_mapa, "agg_mapa_municipios", width = 8, height = 6)
  } else {
    message("   AVISO: agg_mapa_municipios.png NAO foi gerado (requer metadados municipais).")
  }
} else {
  message("   AVISO: agg_mapa_municipios.png NAO foi gerado (serie por municipio ausente).")
}


# =============================================================================
# BLOCO 9 - Encadeamento: salvar a serie temporal para o Tutorial 5
# =============================================================================
# A serie mensal por faixa etaria e o objeto principal desta etapa. Salvamos
# tambem a serie por municipio (para a junçao espacial) e metadados do caso.
message(">> Bloco 9: salvando a serie temporal para o proximo tutorial...")

caso_serie <- list(
  serie_mensal_faixa = if (!is.null(serie_mensal)) dplyr::as_tibble(serie_mensal) else NULL,
  serie_diaria       = if (!is.null(serie_diaria)) dplyr::as_tibble(serie_diaria) else NULL,
  serie_municipio    = if (!is.null(serie_muni))   dplyr::as_tibble(serie_muni)   else NULL,
  meta = list(
    uf              = "SP",
    anos            = ANOS,
    sistema         = "SIM-DO",
    desfecho        = "Obitos respiratorios (CID-10 J00-J99) em idosos 60+",
    age_breaks      = c(60, 70, 80, Inf),
    exemplo_minimo  = usou_exemplo_minimo
  )
)
save_tbl(caso_serie, "caso_serie")

message("== Concluido. Artefatos em vignettes-pt/figuras/ e vignettes-pt/dados/ ==")
