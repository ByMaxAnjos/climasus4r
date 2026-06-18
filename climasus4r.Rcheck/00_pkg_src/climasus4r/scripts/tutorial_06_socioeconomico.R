# =============================================================================
# tutorial_06_socioeconomico.R
# -----------------------------------------------------------------------------
# PROPOSITO
#   Gerar UMA VEZ, localmente, todas as figuras e tabelas canonicas usadas pelo
#   Tutorial 6 (Vinculo de Dados Socioeconomicos) da vignette climasus4r. A
#   etapa SOCIOECONOMICA vincula variaveis do Censo Demografico (populacao,
#   vulnerabilidade) aos dados de saude ja espacializados e calcula indicadores
#   socioeconomicos e epidemiologicos padronizados. A analise pesada (download
#   do censo via censobr, agregacao por municipio) roda AQUI; o arquivo .Rmd
#   apenas carrega os artefatos prontos (PNG via include_graphics, RDS via
#   readRDS).
#
#   Funcoes da etapa cobertas (TODAS):
#     - sus_census_join()              vincula microdados/setores do censo
#     - sus_census_select()            explorador de variaveis do censo
#     - sus_socio_compute_indicators() calcula indicadores padronizados (+ IC)
#     - sus_socio_list_indicators()    catalogo de indicadores disponiveis
#
# COMO RODAR
#   Rode a partir da RAIZ do pacote (pasta climasus4r/):
#       Rscript scripts/tutorial_06_socioeconomico.R
#   ou, em sessao interativa com o working directory na raiz:
#       source("scripts/tutorial_06_socioeconomico.R")
#
# ARTEFATOS GERADOS
#   Figuras (vignettes-pt/figuras/):
#     - socio_catalogo_categorias.png  n. de indicadores por categoria
#     - socio_vulnerabilidade.png      ranking municipal de vulnerabilidade
#     - socio_indicador_ic.png         indicador com intervalo de confianca
#     - socio_dispersao_idosos.png     % idosos x cobertura de esgoto
#   Tabelas (vignettes-pt/dados/):
#     - socio_resumo.rds               indicadores por municipio (amostra)
#     - socio_catalogo.rds             catalogo de indicadores (sus_socio_list)
#   Encadeamento (caso condutor):
#     - caso_socio.rds                 objeto enriquecido, entrada do Tutorial 7
#
# CASO CONDUTOR (atravessa os 9 tutoriais)
#   "Temperatura e desfechos de saude em idosos na Regiao Metropolitana de Sao
#   Paulo, 2014-2019". Nesta etapa, partindo dos obitos respiratorios/cardio
#   ja espacializados por municipio (Tutorial 5), vinculamos variaveis do Censo
#   2010 (populacao, domicilios) e calculamos indicadores de vulnerabilidade
#   (envelhecimento, cobertura de esgoto/agua, dependencia) que servirao de
#   covariaveis/modificadores de efeito na modelagem clima-saude.
#
# ENCADEAMENTO
#   ENTRADA : vignettes-pt/dados/caso_espacial.rds   (saida do Tutorial 5)
#   SAIDA   : vignettes-pt/dados/caso_socio.rds       (entrada do Tutorial 7)
#
# ROBUSTEZ
#   Este script e ILUSTRATIVO e pre-renderizado. Toda chamada de rede/pacote
#   Suggests (censobr, sf, geobr, arrow) e protegida por requireNamespace()/
#   tryCatch e degrada graciosamente com uma mensagem se algo nao estiver
#   disponivel, mantendo a geracao dos demais artefatos.
# =============================================================================

# --- Infraestrutura compartilhada -------------------------------------------
source("scripts/_setup_tutoriais.R")

COR_PRIMARIA   <- "#2E7D32"
COR_SECUNDARIA <- "#558B2F"
COR_ACENTO     <- "#EF6C00"

set.seed(2026)  # reprodutibilidade do exemplo minimo de fallback


# =============================================================================
# BLOCO 0 - Carregar a entrada encadeada (Tutorial 5) ou exemplo minimo
# =============================================================================
# O caso condutor avanca a cada tutorial. Aqui carregamos os obitos ja
# espacializados por municipio (stage "spatial", com geometria sf e a coluna
# de codigo IBGE de 7 digitos code_muni_7, exigida por sus_census_join()).
# Se o artefato do Tutorial 5 ainda nao existir, construimos um exemplo minimo
# com alguns municipios da Regiao Metropolitana de Sao Paulo, apenas para que
# o script rode e gere figuras ilustrativas.
message(">> Bloco 0: carregando a entrada encadeada (caso_espacial.rds)...")

ENTRADA <- file.path("vignettes-pt", "dados", "caso_espacial.rds")

# Codigos IBGE (7 digitos) de municipios da RMSP usados no exemplo minimo.
rmsp_munis <- c(
  "3550308",  # Sao Paulo
  "3518800",  # Guarulhos
  "3548708",  # Santo Andre
  "3547809",  # Sao Bernardo do Campo
  "3529401",  # Maua
  "3543402",  # Ribeirao Pires
  "3552502",  # Suzano
  "3513801"   # Diadema
)
rmsp_nomes <- c(
  "Sao Paulo", "Guarulhos", "Santo Andre", "Sao Bernardo do Campo",
  "Maua", "Ribeirao Pires", "Suzano", "Diadema"
)

if (file.exists(ENTRADA)) {
  caso_espacial <- readRDS(ENTRADA)
  message("   Entrada do Tutorial 5 carregada com sucesso.")
} else {
  message("   AVISO: ", ENTRADA, " ausente. Usando EXEMPLO MINIMO (RMSP).")
  # data.frame simples com a coluna-chave de juncao (code_muni_7) e contagens
  # ilustrativas de obitos por municipio. Promovido a climasus_df stage
  # "spatial" para satisfazer a guarda de estagio de sus_census_join().
  caso_espacial <- dplyr::tibble(
    code_muni_7  = rmsp_munis,
    name_muni    = rmsp_nomes,
    obitos_resp  = c(28100, 3200, 2600, 2900, 1500, 380, 720, 980),
    obitos_cardio = c(41200, 4800, 3900, 4300, 2200, 560, 1050, 1450)
  )
  # tenta promover a climasus_df se o helper estiver acessivel
  caso_espacial <- tryCatch(
    {
      structure(
        caso_espacial,
        sus_meta = list(
          system = "SIM", stage = "spatial", type = "munic",
          backend = "tibble",
          history = sprintf("[%s] exemplo minimo socio",
                            format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          user = list()
        ),
        class = c("climasus_df", class(caso_espacial))
      )
    },
    error = function(e) caso_espacial
  )
}

# Extrai um data.frame de trabalho (sem geometria) para os calculos abaixo.
# Se a entrada for sf, dropa a geometria para as agregacoes ilustrativas.
caso_df <- tryCatch({
  if (requireNamespace("sf", quietly = TRUE) && inherits(caso_espacial, "sf")) {
    sf::st_drop_geometry(caso_espacial)
  } else {
    dplyr::as_tibble(caso_espacial)
  }
}, error = function(e) dplyr::as_tibble(caso_espacial))


# =============================================================================
# BLOCO 1 - Catalogo de indicadores: sus_socio_list_indicators()
# =============================================================================
# sus_socio_list_indicators() retorna o catalogo completo de indicadores
# disponiveis (id, nome, categoria, colunas exigidas, formula, multiplicador,
# unidade, metodo de incerteza, fonte). Nao depende de rede.
message(">> Bloco 1: catalogo de indicadores (sus_socio_list_indicators)...")

catalogo <- tryCatch(
  sus_socio_list_indicators(lang = "pt"),
  error = function(e) {
    message("   sus_socio_list_indicators indisponivel: ", conditionMessage(e)); NULL
  }
)

if (!is.null(catalogo)) {
  # Tabela-resumo (amostra de colunas mais uteis ao leitor)
  catalogo_tbl <- as.data.frame(catalogo)[,
    intersect(c("id", "name", "category", "unit", "uncertainty_method", "source"),
              names(catalogo)), drop = FALSE]
  save_tbl(utils::head(catalogo_tbl, 22), "socio_catalogo")

  # Figura 1 - numero de indicadores por categoria
  por_cat <- catalogo |>
    dplyr::count(category, name = "n") |>
    dplyr::arrange(dplyr::desc(n))

  p_cat <- ggplot2::ggplot(
    por_cat,
    ggplot2::aes(x = stats::reorder(category, n), y = n, fill = category)
  ) +
    ggplot2::geom_col(width = 0.7, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = n), hjust = -0.3,
                       color = COR_PRIMARIA, fontface = "bold") +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = grDevices::colorRampPalette(cf_colors)(nrow(por_cat))) +
    ggplot2::labs(
      title    = "Indicadores disponiveis por categoria",
      subtitle = "Catalogo de sus_socio_compute_indicators()",
      x = NULL, y = "Numero de indicadores"
    ) +
    tema_climasus()
  save_fig(p_cat, "socio_catalogo_categorias", width = 8, height = 5)
} else {
  message("   AVISO: socio_catalogo / socio_catalogo_categorias NAO gerados.")
}


# =============================================================================
# BLOCO 2 - Explorar variaveis do censo: sus_census_select()
# =============================================================================
# sus_census_select() abre um explorador interativo (HTML) das variaveis do
# censo OU retorna codigos em modo programatico. Em renderizacao automatica,
# usamos output = "codes" (nao abre navegador) para obter os codigos das
# variaveis de populacao do Censo 2010.
message(">> Bloco 2: explorando variaveis do censo (sus_census_select)...")

vars_pop <- tryCatch(
  sus_census_select(
    dataset = "population",
    year    = 2010,
    output  = "codes",   # "browser" (padrao), "console" ou "codes"
    lang    = "pt",
    verbose = FALSE
  ),
  error = function(e) {
    message("   sus_census_select indisponivel: ", conditionMessage(e)); NULL
  }
)
if (!is.null(vars_pop)) {
  message("   ", length(vars_pop), " codigos de variaveis de populacao disponiveis.")
}


# =============================================================================
# BLOCO 3 - Vincular o censo: sus_census_join()
# =============================================================================
# sus_census_join() baixa um dataset do censo (population/households/...) via
# censobr, filtra os municipios presentes nos dados (pela coluna code_muni_7),
# agrega os microdados ao nivel municipal (aggregation_fun) e faz left_join
# nos dados de saude. Exige stage >= "spatial" e a coluna code_muni_7.
# Toda a operacao depende de rede + censobr: protegida por requireNamespace.
message(">> Bloco 3: vinculando o Censo 2010 (sus_census_join)...")

caso_enriquecido <- NULL
if (requireNamespace("censobr", quietly = TRUE)) {
  caso_enriquecido <- tryCatch(
    sus_census_join(
      df              = caso_espacial,
      dataset         = "population",   # microdados de populacao
      year            = 2010,           # 2010 (padrao) ou 2000
      census_vars     = if (!is.null(vars_pop)) utils::head(vars_pop, 6) else NULL,
      aggregation_fun = "sum",          # soma por municipio (populacao total)
      use_cache       = TRUE,
      cache_dir       = "~/.climasus4r_cache/census",
      lang            = "pt",
      verbose         = TRUE
    ),
    error = function(e) {
      message("   sus_census_join indisponivel: ", conditionMessage(e)); NULL
    }
  )
} else {
  message("   Pacote 'censobr' nao instalado; pulando sus_census_join().")
}


# =============================================================================
# BLOCO 4 - Calcular indicadores: sus_socio_compute_indicators()
# =============================================================================
# sus_socio_compute_indicators() avalia formulas do catalogo sobre as colunas
# presentes. Como os nomes do censo (ex.: V0001_sum) diferem dos nomes de
# formula (ex.: pop_total), usamos col_mapping para conectar uns aos outros.
# Aqui o exemplo e ILUSTRATIVO: caso o censo nao tenha sido vinculado, criamos
# colunas socioeconomicas sinteticas no exemplo minimo para demonstrar o
# calculo (taxa de envelhecimento, cobertura de esgoto/agua, dependencia).
message(">> Bloco 4: calculando indicadores (sus_socio_compute_indicators)...")

# Base para os indicadores: usa o objeto enriquecido pelo censo se disponivel;
# caso contrario, monta colunas demograficas/domiciliares sinteticas sobre o
# exemplo minimo (apenas para ilustrar o calculo e as figuras).
base_ind <- caso_enriquecido
if (is.null(base_ind)) {
  message("   Censo indisponivel: usando colunas socioeconomicas sinteticas.")
  n_m <- nrow(caso_df)
  pop_total   <- c(12300000, 1390000, 720000, 840000, 480000, 120000, 300000, 420000)[seq_len(n_m)]
  pop_elderly <- round(pop_total * runif(n_m, 0.09, 0.16))
  pop_young   <- round(pop_total * runif(n_m, 0.16, 0.24))
  pop_working <- pop_total - pop_elderly - pop_young
  total_hh    <- round(pop_total / runif(n_m, 2.8, 3.4))
  hh_sewage   <- round(total_hh * runif(n_m, 0.70, 0.99))
  hh_water    <- round(total_hh * runif(n_m, 0.85, 0.99))

  base_ind <- caso_df |>
    dplyr::mutate(
      pop_total   = pop_total,
      pop_elderly = pop_elderly,
      pop_young   = pop_young,
      pop_working = pop_working,
      total_hh    = total_hh,
      hh_sewage   = hh_sewage,
      hh_water    = hh_water
    )
  # promove a climasus_df stage "spatial" para satisfazer a guarda
  base_ind <- tryCatch(
    structure(
      base_ind,
      sus_meta = list(system = "SIM", stage = "spatial", type = "munic",
                      backend = "tibble",
                      history = "[exemplo] socio", user = list()),
      class = c("climasus_df", setdiff(class(base_ind), "climasus_df"))
    ),
    error = function(e) base_ind
  )
}

# col_mapping conecta nomes de formula -> colunas reais do objeto. Quando os
# nomes ja coincidem (exemplo sintetico), o mapeamento e identidade; com censo
# real, aponte para as colunas agregadas (ex.: pop_total = "V0001_sum").
mapeamento <- list(
  pop_elderly = "pop_elderly",
  pop_young   = "pop_young",
  pop_working = "pop_working",
  pop_total   = "pop_total",
  total_hh    = "total_hh",
  hh_sewage   = "hh_sewage",
  hh_water    = "hh_water"
)

caso_socio <- tryCatch(
  sus_socio_compute_indicators(
    df               = base_ind,
    indicators       = c("aging_index", "dependency_ratio",
                         "sewage_connection_rate", "water_connection_rate"),
    col_mapping      = mapeamento,
    confidence_level = 0.95,
    add_ci           = TRUE,
    lang             = "pt",
    verbose          = TRUE
  ),
  error = function(e) {
    message("   sus_socio_compute_indicators indisponivel: ", conditionMessage(e)); NULL
  }
)

if (is.null(caso_socio)) {
  # fallback: mantem a base sem indicadores para nao quebrar o encadeamento
  caso_socio <- base_ind
}

socio_df <- tryCatch({
  if (requireNamespace("sf", quietly = TRUE) && inherits(caso_socio, "sf")) {
    sf::st_drop_geometry(caso_socio)
  } else {
    dplyr::as_tibble(caso_socio)
  }
}, error = function(e) dplyr::as_tibble(caso_socio))


# =============================================================================
# BLOCO 5 - Tabela-resumo dos indicadores por municipio
# =============================================================================
message(">> Bloco 5: tabela-resumo dos indicadores por municipio...")

col_nome <- intersect(c("name_muni", "name_munic", "code_muni_7"), names(socio_df))[1]
cols_ind <- grep("^ind_", names(socio_df), value = TRUE)
cols_ind_principais <- setdiff(cols_ind, grep("_(low|high)$", cols_ind, value = TRUE))

if (length(cols_ind_principais) > 0 && !is.na(col_nome)) {
  resumo <- socio_df |>
    dplyr::select(dplyr::all_of(c(col_nome, cols_ind_principais))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(cols_ind_principais),
                                ~ round(as.numeric(.x), 1))) |>
    utils::head(10) |>
    as.data.frame()
  save_tbl(resumo, "socio_resumo")
} else {
  message("   AVISO: socio_resumo NAO gerado (sem indicadores calculados).")
}


# =============================================================================
# BLOCO 6 - Figura: ranking municipal de vulnerabilidade (envelhecimento)
# =============================================================================
message(">> Bloco 6: ranking de vulnerabilidade municipal...")

ind_aging <- if ("ind_aging_index" %in% names(socio_df)) "ind_aging_index" else NA
if (!is.na(ind_aging) && !is.na(col_nome)) {
  rk <- socio_df |>
    dplyr::transmute(
      municipio = .data[[col_nome]],
      valor     = as.numeric(.data[[ind_aging]])
    ) |>
    dplyr::filter(!is.na(valor)) |>
    dplyr::arrange(dplyr::desc(valor)) |>
    utils::head(12)

  p_vuln <- ggplot2::ggplot(
    rk, ggplot2::aes(x = stats::reorder(municipio, valor), y = valor)
  ) +
    ggplot2::geom_col(width = 0.7, fill = COR_PRIMARIA) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f", valor)),
                       hjust = -0.2, color = COR_PRIMARIA, fontface = "bold") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title    = "Indice de Envelhecimento por municipio",
      subtitle = "Idosos por 100 jovens (Censo 2010) - RMSP",
      x = NULL, y = "Indice de Envelhecimento (%)"
    ) +
    tema_climasus()
  save_fig(p_vuln, "socio_vulnerabilidade", width = 8, height = 5)
} else {
  message("   AVISO: socio_vulnerabilidade NAO gerado.")
}


# =============================================================================
# BLOCO 7 - Figura: indicador com intervalo de confianca (cobertura de esgoto)
# =============================================================================
# Indicadores de proporcao recebem IC de Wilson (binomial) quando add_ci=TRUE,
# nas colunas ind_<id>_low / ind_<id>_high. Ilustramos a cobertura de esgoto.
message(">> Bloco 7: indicador com intervalo de confianca (esgoto)...")

id_ic <- "ind_sewage_connection_rate"
if (all(c(id_ic, paste0(id_ic, "_low"), paste0(id_ic, "_high")) %in% names(socio_df)) &&
    !is.na(col_nome)) {
  dic <- socio_df |>
    dplyr::transmute(
      municipio = .data[[col_nome]],
      valor     = as.numeric(.data[[id_ic]]),
      lo        = as.numeric(.data[[paste0(id_ic, "_low")]]),
      hi        = as.numeric(.data[[paste0(id_ic, "_high")]])
    ) |>
    dplyr::filter(!is.na(valor)) |>
    dplyr::arrange(valor) |>
    utils::head(12)

  p_ic <- ggplot2::ggplot(
    dic, ggplot2::aes(x = stats::reorder(municipio, valor), y = valor)
  ) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi),
                           width = 0.25, color = COR_SECUNDARIA, linewidth = 0.7) +
    ggplot2::geom_point(color = COR_ACENTO, size = 2.4) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title    = "Cobertura de esgoto com IC 95% (Wilson)",
      subtitle = "Proporcao de domicilios ligados a rede - RMSP, Censo 2010",
      x = NULL, y = "Cobertura de esgoto (%)"
    ) +
    tema_climasus()
  save_fig(p_ic, "socio_indicador_ic", width = 8, height = 5)
} else {
  message("   AVISO: socio_indicador_ic NAO gerado.")
}


# =============================================================================
# BLOCO 8 - Figura: dispersao % idosos x cobertura de esgoto
# =============================================================================
# Relaciona vulnerabilidade demografica (envelhecimento) a vulnerabilidade de
# infraestrutura (esgoto) - hipotese de confundimento/modificacao de efeito na
# relacao temperatura-saude em idosos.
message(">> Bloco 8: dispersao envelhecimento x esgoto...")

if (all(c("ind_aging_index", "ind_sewage_connection_rate") %in% names(socio_df))) {
  disp <- socio_df |>
    dplyr::transmute(
      municipio   = if (!is.na(col_nome)) .data[[col_nome]] else NA_character_,
      envelhece   = as.numeric(.data[["ind_aging_index"]]),
      esgoto      = as.numeric(.data[["ind_sewage_connection_rate"]])
    ) |>
    dplyr::filter(!is.na(envelhece), !is.na(esgoto))

  p_disp <- ggplot2::ggplot(disp, ggplot2::aes(x = esgoto, y = envelhece)) +
    ggplot2::geom_point(color = COR_PRIMARIA, size = 2.6) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                         color = COR_ACENTO, linewidth = 0.8) +
    ggplot2::labs(
      title    = "Envelhecimento x cobertura de esgoto",
      subtitle = "Municipios da RMSP (Censo 2010)",
      x = "Cobertura de esgoto (%)",
      y = "Indice de Envelhecimento (%)"
    ) +
    tema_climasus()
  save_fig(p_disp, "socio_dispersao_idosos", width = 8, height = 5)
} else {
  message("   AVISO: socio_dispersao_idosos NAO gerado.")
}


# =============================================================================
# BLOCO 9 - Encadeamento: salvar o objeto enriquecido para o Tutorial 7
# =============================================================================
# Salvamos o objeto com os indicadores socioeconomicos calculados. O Tutorial 7
# (Clima baseado em estacoes - INMET) o carregara como ponto de partida para
# integrar a exposicao climatica aos municipios com vulnerabilidade conhecida.
message(">> Bloco 9: salvando caso_socio.rds para o proximo tutorial...")

save_tbl(caso_socio, "caso_socio")

message("== Concluido. Artefatos em vignettes-pt/figuras/ e vignettes-pt/dados/ ==")
