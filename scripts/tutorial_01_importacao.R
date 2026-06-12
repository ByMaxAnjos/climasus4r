# =============================================================================
# tutorial_01_importacao.R
# -----------------------------------------------------------------------------
# PROPOSITO
#   Gerar UMA VEZ, localmente, todas as figuras e tabelas canonicas usadas pelo
#   Tutorial 1 (Importacao) da vignette climasus4r. A etapa de IMPORTACAO no
#   climasus4r refere-se exclusivamente aos microdados do DATASUS, baixados pela
#   funcao sus_data_import(). A analise pesada (downloads) roda AQUI; o arquivo
#   .Rmd apenas carrega os artefatos prontos (PNG via include_graphics, RDS via
#   readRDS).
#
# COMO RODAR
#   Rode a partir da RAIZ do pacote (pasta climasus4r/):
#       Rscript scripts/tutorial_01_importacao.R
#   ou, em sessao interativa com o working directory na raiz:
#       source("scripts/tutorial_01_importacao.R")
#
# ARTEFATOS GERADOS
#   Figuras (vignettes-pt/figuras/):
#     - import_registros_por_sistema.png  n. de registros importados por sistema
#     - import_serie_obitos_sp.png        serie mensal de obitos (SIM-DO), SP
#     - import_bench_cache.png            benchmark: 1o download vs cache
#     - import_bench_parallel.png         benchmark: workers = 1 vs 4
#   Tabelas (vignettes-pt/dados/):
#     - import_sistemas.rds        sistemas x subsistemas DATASUS (anual/mensal)
#     - import_estrutura_sim.rds   glimpse/head do SIM-DO importado
#     - import_regioes.rds         grupos de 'region' e respectivas UFs
#   Encadeamento (caso condutor):
#     - caso_base_importado.rds    objeto bruto multi-sistema, entrada do Tutorial 2
#
# CASO CONDUTOR (atravessa os 9 tutoriais)
#   "Temperatura e desfechos de saude em idosos na Regiao Metropolitana de Sao
#   Paulo, 2014-2019". Neste tutorial montamos a BASE BRUTA multi-sistema:
#   obitos (SIM-DO), internacoes (SIH-RD) e dengue (SINAN-DENGUE) para Sao Paulo.
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
# BLOCO 1 - Importar a base bruta multi-sistema do DATASUS
# =============================================================================
# sus_data_import() e a unica porta de entrada da etapa de importacao. Aqui
# baixamos tres sistemas que sustentam o caso condutor:
#   - SIM-DO       : obitos (mortalidade) - sistema ANUAL (month e ignorado)
#   - SIH-RD       : internacoes hospitalares - sistema MENSAL (exige month)
#   - SINAN-DENGUE : casos de dengue - sistema ANUAL ("nacional")
message(">> Bloco 1: importando microdados do DATASUS (SP, 2014-2019)...")

# 1a) Mortalidade (SIM-DO) - anual. backend "tibble" para manipulacao direta.
obitos <- sus_data_import(
  uf        = "SP",
  year      = ANOS,
  system    = "SIM-DO",
  backend   = "tibble",
  use_cache = TRUE,
  parallel  = TRUE,
  workers   = 4,
  lang      = "pt",
  verbose   = TRUE
)

# 1b) Internacoes (SIH-RD) - MENSAL: o argumento month e obrigatorio.
#     Restringimos a janelas de inverno (jun-ago), quando a mortalidade
#     respiratoria de idosos costuma ser mais alta, para manter o exemplo leve.
internacoes <- tryCatch(
  sus_data_import(
    uf        = "SP",
    year      = ANOS,
    month     = 6:8,
    system    = "SIH-RD",
    backend   = "tibble",
    use_cache = TRUE,
    parallel  = TRUE,
    workers   = 4,
    lang      = "pt",
    verbose   = TRUE
  ),
  error = function(e) {
    message("   SIH-RD indisponivel: ", conditionMessage(e)); NULL
  }
)

# 1c) Dengue (SINAN-DENGUE) - anual. Caso secundario (clima x arboviroses).
dengue <- tryCatch(
  sus_data_import(
    uf        = "SP",
    year      = ANOS,
    system    = "SINAN-DENGUE",
    backend   = "tibble",
    use_cache = TRUE,
    lang      = "pt",
    verbose   = TRUE
  ),
  error = function(e) {
    message("   SINAN-DENGUE indisponivel: ", conditionMessage(e)); NULL
  }
)

obitos_df <- dplyr::as_tibble(obitos)


# =============================================================================
# BLOCO 2 - Recurso de recorte espacial: city / municipality_code e region
# =============================================================================
# city/municipality_code resolvem para codigos IBGE ANTES do download (fail-fast).
# Aqui mostramos a importacao restrita ao municipio de Sao Paulo (IBGE 3550308).
message(">> Bloco 2: importacao restrita por municipio (Sao Paulo, 3550308)...")

obitos_muni_sp <- tryCatch(
  sus_data_import(
    uf                = "SP",
    year              = ANOS,
    system            = "SIM-DO",
    municipality_code = "3550308",   # municipio de Sao Paulo
    backend           = "tibble",
    use_cache         = TRUE,
    lang              = "pt",
    verbose           = TRUE
  ),
  error = function(e) {
    message("   Importacao por municipio indisponivel: ", conditionMessage(e)); NULL
  }
)

# Tabela-resumo dos grupos de 'region' aceitos por sus_data_import().
# (Atalho para baixar varias UFs de uma vez: macrorregioes, biomas, bacias.)
regioes <- data.frame(
  Grupo = c("sudeste", "sul", "nordeste", "norte", "centro_oeste",
            "amazonia_legal", "mata_atlantica", "cerrado",
            "bacia_sao_francisco", "semi_arido"),
  Tipo = c("Macrorregiao IBGE", "Macrorregiao IBGE", "Macrorregiao IBGE",
           "Macrorregiao IBGE", "Macrorregiao IBGE",
           "Bioma/fronteira ecologica", "Bioma", "Bioma",
           "Bacia hidrografica", "Recorte climatico"),
  UFs = c(
    "ES, MG, RJ, SP",
    "PR, RS, SC",
    "AL, BA, CE, MA, PB, PE, PI, RN, SE",
    "AC, AP, AM, PA, RO, RR, TO",
    "DF, GO, MT, MS",
    "AC, AP, AM, PA, RO, RR, MT, MA, TO",
    "AL, BA, CE, ES, GO, MA, MG, MS, PB, PE, PI, PR, RJ, RN, RS, SC, SE, SP",
    "BA, DF, GO, MA, MG, MS, MT, PA, PI, PR, RO, SP, TO",
    "AL, BA, DF, GO, MG, PE, SE",
    "AL, BA, CE, MA, PB, PE, PI, RN, SE, MG"
  ),
  stringsAsFactors = FALSE
)
save_tbl(regioes, "import_regioes")


# =============================================================================
# BLOCO 3 - Tabela-resumo dos sistemas/subsistemas suportados
# =============================================================================
message(">> Bloco 3: tabela-resumo dos sistemas DATASUS...")

sistemas <- data.frame(
  Sistema = c("SIM", "SIH", "SINASC", "SINAN", "CNES", "SIA"),
  Descricao = c(
    "Mortalidade (declaracoes de obito)",
    "Internacoes hospitalares (AIH)",
    "Nascidos vivos",
    "Agravos de notificacao (doencas notificaveis)",
    "Estabelecimentos de saude",
    "Atendimentos ambulatoriais"
  ),
  Exemplos_system = c(
    "SIM-DO, SIM-DOFET, SIM-DOINF, SIM-DOMAT, SIM-DOEXT",
    "SIH-RD, SIH-RJ, SIH-SP, SIH-ER",
    "SINASC",
    "SINAN-DENGUE, SINAN-ZIKA, SINAN-CHIKUNGUNYA, SINAN-MALARIA, ...",
    "CNES-LT, CNES-ST, CNES-EQ, CNES-PF, ...",
    "SIA-PA, SIA-AB, SIA-AD, ..."
  ),
  Periodicidade = c(
    "Anual (month ignorado)",
    "Mensal (exige month)",
    "Anual (month ignorado)",
    "Anual (month ignorado)",
    "Mensal (exige month)",
    "Mensal (exige month)"
  ),
  stringsAsFactors = FALSE
)
save_tbl(sistemas, "import_sistemas")


# =============================================================================
# BLOCO 4 - Estrutura do objeto importado (SIM-DO)
# =============================================================================
# head() das colunas mais relevantes do SIM-DO bruto, para o leitor reconhecer
# a estrutura tipica (causa basica, idade codificada, municipio de residencia).
message(">> Bloco 4: estrutura do SIM-DO importado...")

colunas_chave <- intersect(
  c("DTOBITO", "DTNASC", "SEXO", "IDADE", "RACACOR", "CAUSABAS",
    "CODMUNRES", "CODMUNOCOR"),
  names(obitos_df)
)
estrutura_sim <- obitos_df |>
  dplyr::select(dplyr::all_of(colunas_chave)) |>
  utils::head(10) |>
  as.data.frame()
save_tbl(estrutura_sim, "import_estrutura_sim")


# =============================================================================
# BLOCO 5 - Figura: numero de registros importados por sistema
# =============================================================================
message(">> Bloco 5: contagem de registros por sistema...")

registros <- dplyr::tibble(
  sistema = c("SIM-DO\n(obitos)", "SIH-RD\n(internacoes)", "SINAN\n(dengue)"),
  n = c(
    nrow(obitos_df),
    if (!is.null(internacoes)) nrow(dplyr::as_tibble(internacoes)) else NA_integer_,
    if (!is.null(dengue))      nrow(dplyr::as_tibble(dengue))      else NA_integer_
  )
) |>
  dplyr::filter(!is.na(n))

p_registros <- ggplot2::ggplot(
  registros,
  ggplot2::aes(x = stats::reorder(sistema, -n), y = n, fill = sistema)
) +
  ggplot2::geom_col(width = 0.65, show.legend = FALSE) +
  ggplot2::geom_text(ggplot2::aes(label = format(n, big.mark = ".")),
                     vjust = -0.4, color = COR_PRIMARIA, fontface = "bold") +
  ggplot2::scale_fill_manual(values = cf_colors) +
  ggplot2::labs(
    title    = "Registros importados por sistema DATASUS",
    subtitle = "Estado de Sao Paulo, 2014-2019 (SIH-RD: apenas inverno, jun-ago)",
    x        = NULL,
    y        = "Numero de registros"
  ) +
  tema_climasus()

save_fig(p_registros, "import_registros_por_sistema", width = 8, height = 5)


# =============================================================================
# BLOCO 6 - Figura: serie mensal de obitos (SIM-DO)
# =============================================================================
message(">> Bloco 6: serie mensal de obitos (SIM-DO)...")

serie_mensal <- obitos_df |>
  dplyr::mutate(data_obito = lubridate::dmy(DTOBITO)) |>   # DTOBITO = ddmmaaaa
  dplyr::filter(!is.na(data_obito)) |>
  dplyr::mutate(mes = lubridate::floor_date(data_obito, "month")) |>
  dplyr::count(mes, name = "obitos")

p_serie <- ggplot2::ggplot(serie_mensal, ggplot2::aes(x = mes, y = obitos)) +
  ggplot2::geom_line(color = COR_PRIMARIA, linewidth = 0.8) +
  ggplot2::geom_point(color = COR_PRIMARIA, size = 1.1) +
  ggplot2::labs(
    title    = "Obitos mensais (todas as causas) - SIM-DO",
    subtitle = "Estado de Sao Paulo, 2014-2019",
    x        = "Mes",
    y        = "Numero de obitos"
  ) +
  tema_climasus()

save_fig(p_serie, "import_serie_obitos_sp", width = 8, height = 5)


# =============================================================================
# BLOCO 7 - Benchmark de CACHE: 1o download vs leitura do cache
# =============================================================================
# Mede o tempo de uma importacao forcando novo download (force_redownload=TRUE)
# contra a 2a chamada, que le do cache. Demonstra o ganho do cache inteligente.
message(">> Bloco 7: benchmark de cache (pode baixar dados)...")

bench_cache <- tryCatch({
  t_download <- system.time(
    sus_data_import(uf = "SP", year = 2019, system = "SIM-DO",
                    use_cache = TRUE, force_redownload = TRUE,
                    parallel = FALSE, lang = "pt", verbose = FALSE)
  )[["elapsed"]]

  t_cache <- system.time(
    sus_data_import(uf = "SP", year = 2019, system = "SIM-DO",
                    use_cache = TRUE, force_redownload = FALSE,
                    parallel = FALSE, lang = "pt", verbose = FALSE)
  )[["elapsed"]]

  dplyr::tibble(
    etapa  = factor(c("1o download\n(rede)", "2a chamada\n(cache)"),
                    levels = c("1o download\n(rede)", "2a chamada\n(cache)")),
    tempo  = c(t_download, t_cache)
  )
}, error = function(e) {
  message("   benchmark de cache pulado: ", conditionMessage(e)); NULL
})

if (!is.null(bench_cache)) {
  speedup_cache <- round(bench_cache$tempo[1] / max(bench_cache$tempo[2], 1e-6), 1)
  p_cache <- ggplot2::ggplot(bench_cache, ggplot2::aes(x = etapa, y = tempo, fill = etapa)) +
    ggplot2::geom_col(width = 0.6, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f s", tempo)),
                       vjust = -0.4, color = COR_PRIMARIA, fontface = "bold") +
    ggplot2::scale_fill_manual(values = c(COR_ACENTO, COR_PRIMARIA)) +
    ggplot2::labs(
      title    = "Cache inteligente: tempo de importacao",
      subtitle = sprintf("SIM-DO SP 2019 - cache ~%sx mais rapido que o download", speedup_cache),
      x = NULL, y = "Tempo decorrido (s)"
    ) +
    tema_climasus()
  save_fig(p_cache, "import_bench_cache", width = 8, height = 5)
} else {
  message("   AVISO: import_bench_cache.png NAO foi gerado (sem internet?).")
}


# =============================================================================
# BLOCO 8 - Benchmark de PARALELISMO: workers = 1 vs workers = 4
# =============================================================================
# Mede a importacao de varios anos em serie (workers=1) contra paralela
# (workers=4). force_redownload=FALSE para medir o efeito do paralelismo no
# processamento, nao a variabilidade da rede.
message(">> Bloco 8: benchmark de paralelismo...")

bench_par <- tryCatch({
  # Garante que os anos ja estao em cache (download previo, fora da medicao).
  invisible(sus_data_import(uf = "SP", year = ANOS, system = "SIM-DO",
                            use_cache = TRUE, parallel = TRUE, workers = 4,
                            lang = "pt", verbose = FALSE))

  t_serial <- system.time(
    sus_data_import(uf = "SP", year = ANOS, system = "SIM-DO",
                    use_cache = TRUE, parallel = FALSE, workers = 1,
                    lang = "pt", verbose = FALSE)
  )[["elapsed"]]

  t_par <- system.time(
    sus_data_import(uf = "SP", year = ANOS, system = "SIM-DO",
                    use_cache = TRUE, parallel = TRUE, workers = 4,
                    lang = "pt", verbose = FALSE)
  )[["elapsed"]]

  dplyr::tibble(
    modo  = factor(c("Serial\n(workers = 1)", "Paralelo\n(workers = 4)"),
                   levels = c("Serial\n(workers = 1)", "Paralelo\n(workers = 4)")),
    tempo = c(t_serial, t_par)
  )
}, error = function(e) {
  message("   benchmark de paralelismo pulado: ", conditionMessage(e)); NULL
})

if (!is.null(bench_par)) {
  speedup_par <- round(bench_par$tempo[1] / max(bench_par$tempo[2], 1e-6), 1)
  p_par <- ggplot2::ggplot(bench_par, ggplot2::aes(x = modo, y = tempo, fill = modo)) +
    ggplot2::geom_col(width = 0.6, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f s", tempo)),
                       vjust = -0.4, color = COR_PRIMARIA, fontface = "bold") +
    ggplot2::scale_fill_manual(values = c(COR_SECUNDARIA, COR_PRIMARIA)) +
    ggplot2::labs(
      title    = "Paralelismo: importacao de 6 anos (2014-2019)",
      subtitle = sprintf("SIM-DO SP - paralelo ~%sx mais rapido (speedup = t_serial / t_paralelo)", speedup_par),
      x = NULL, y = "Tempo decorrido (s)"
    ) +
    tema_climasus()
  save_fig(p_par, "import_bench_parallel", width = 8, height = 5)
} else {
  message("   AVISO: import_bench_parallel.png NAO foi gerado (sem internet?).")
}


# =============================================================================
# BLOCO 9 - Encadeamento: salvar a base bruta para o Tutorial 2
# =============================================================================
# O caso condutor avanca a cada tutorial. Salvamos a base bruta multi-sistema
# (lista de objetos importados) para que o Tutorial 2 (Encoding & Padronizacao)
# a carregue como ponto de partida.
message(">> Bloco 9: salvando a base bruta para o proximo tutorial...")

caso_base_importado <- list(
  obitos      = obitos_df,
  internacoes = if (!is.null(internacoes)) dplyr::as_tibble(internacoes) else NULL,
  dengue      = if (!is.null(dengue))      dplyr::as_tibble(dengue)      else NULL,
  meta        = list(uf = "SP", anos = ANOS,
                     sistemas = c("SIM-DO", "SIH-RD", "SINAN-DENGUE"))
)
save_tbl(caso_base_importado, "caso_base_importado")

message("== Concluido. Artefatos em vignettes-pt/figuras/ e vignettes-pt/dados/ ==")
