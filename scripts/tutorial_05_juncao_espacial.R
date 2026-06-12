# =============================================================================
# tutorial_05_juncao_espacial.R
# -----------------------------------------------------------------------------
# PROPOSITO
#   Gerar UMA VEZ, localmente, todas as figuras e tabelas canonicas usadas pelo
#   Tutorial 5 (Juncao Espacial) da vignette climasus4r. A etapa de JUNCAO
#   ESPACIAL liga a serie de saude (saida do Tutorial 4) a unidades geograficas
#   oficiais do Brasil (municipios da RMSP) e, opcionalmente, a dados ambientais
#   em grade. As funcoes cobertas sao:
#       - sus_spatial_join()  : liga dados de saude a poligonos/pontos do geobr
#       - sus_grid_join()     : une dados ambientais em grade (sus_grid_*) a saude
#   A analise (downloads de geometrias) roda AQUI; o .Rmd apenas carrega os
#   artefatos prontos (PNG via include_graphics, RDS via readRDS).
#
# COMO RODAR
#   Rode a partir da RAIZ do pacote (pasta climasus4r/):
#       Rscript scripts/tutorial_05_juncao_espacial.R
#   ou, em sessao interativa com o working directory na raiz:
#       source("scripts/tutorial_05_juncao_espacial.R")
#
# ARTEFATOS GERADOS
#   Figuras (vignettes-pt/figuras/):
#     - geo_mapa_obitos.png   coropleto: obitos respiratorios por municipio (RMSP)
#     - geo_serie.png         serie temporal apos a juncao espacial
#     - geo_cobertura.png     diagnostico: municipios com/sem correspondencia
#   Tabelas (vignettes-pt/dados/):
#     - geo_resumo.rds        resumo da juncao (linhas antes/depois, nivel, CRS)
#     - geo_amostra.rds       amostra das colunas-chave do objeto sf resultante
#   Encadeamento (caso condutor):
#     - caso_espacial.rds     objeto espacial, entrada do Tutorial 6 (Socioeconomico)
#
# ENTRADA ENCADEADA
#   dados/caso_serie.rds  (saida do Tutorial 4 - Variaveis & Agregacao). Se ausente,
#   o script avisa e segue com um EXEMPLO MINIMO sintetico para nao quebrar.
#
# CASO CONDUTOR (atravessa os 9 tutoriais)
#   "Temperatura e desfechos de saude em idosos na Regiao Metropolitana de Sao
#   Paulo, 2014-2019". Nesta etapa atribuimos a cada registro de saude a sua
#   geometria municipal, base para o vinculo socioeconomico (T6) e a modelagem.
# =============================================================================

# --- Infraestrutura compartilhada -------------------------------------------
source("scripts/_setup_tutoriais.R")

COR_PRIMARIA   <- "#2E7D32"
COR_SECUNDARIA <- "#558B2F"
COR_ACENTO     <- "#EF6C00"

# Municipios da Regiao Metropolitana de Sao Paulo (codigos IBGE de 6 digitos,
# como saem do SIM/SIH apos a padronizacao). Subconjunto ilustrativo da RMSP.
RMSP_COD6 <- c(
  "355030",  # Sao Paulo
  "351380",  # Guarulhos
  "354780",  # Osasco
  "351880",  # Itaquaquecetuba
  "354870",  # Maua
  "354340",  # Mogi das Cruzes
  "352940",  # Diadema
  "351060"   # Carapicuiba
)

# =============================================================================
# BLOCO 1 - Carregar a entrada encadeada (saida do Tutorial 4)
# =============================================================================
# A etapa anterior (T4) salvou a serie agregada de obitos respiratorios por
# municipio e data. Carregamos com guarda file.exists(); se ausente, geramos um
# exemplo minimo sintetico (climasus_df em stage 'aggregate') para o script rodar.
message(">> Bloco 1: carregando a serie de saude (dados/caso_serie.rds)...")

entrada_path <- file.path("vignettes-pt", "dados", "caso_serie.rds")

construir_exemplo_minimo <- function() {
  message("   caso_serie.rds ausente: usando EXEMPLO MINIMO sintetico.")
  set.seed(2024)
  datas <- seq(as.Date("2014-01-01"), as.Date("2019-12-01"), by = "month")
  grade <- expand.grid(
    code_muni = RMSP_COD6,
    date      = datas,
    stringsAsFactors = FALSE
  )
  grade$obitos <- stats::rpois(nrow(grade), lambda = 8)
  df <- dplyr::as_tibble(grade)

  # Promove a climasus_df em stage 'aggregate' (pre-requisito de sus_spatial_join).
  meta <- list(
    system   = "SIM",
    stage    = "aggregate",
    type     = "time_series",
    backend  = "tibble",
    spatial  = FALSE,
    temporal = list(resolution = "month",
                    start = min(df$date), end = max(df$date)),
    created  = Sys.time(),
    modified = Sys.time(),
    history  = sprintf("[%s] exemplo minimo (T5)",
                       format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    user     = list()
  )
  structure(df, sus_meta = meta,
            class = c("climasus_df", class(df)))
}

if (file.exists(entrada_path)) {
  caso_serie <- readRDS(entrada_path)
  # O T4 pode salvar uma lista (com a serie em um elemento) ou o df diretamente.
  serie_saude <- if (is.data.frame(caso_serie)) {
    caso_serie
  } else {
    caso_serie$serie_municipio %||% caso_serie$serie %||%
      caso_serie$obitos %||% caso_serie[[1]]
  }
  # Restaura a classe climasus_df perdida na serializacao dentro de lista:
  # o atributo sus_meta sobrevive ao saveRDS, mas a classe pode nao acompanhar.
  if (!inherits(serie_saude, "climasus_df") &&
      !is.null(attr(serie_saude, "sus_meta"))) {
    class(serie_saude) <- c("climasus_df", class(serie_saude))
  }
} else {
  serie_saude <- construir_exemplo_minimo()
}

# Garante a presenca da coluna de codigo municipal de 6 digitos esperada.
if (!"code_muni" %in% names(serie_saude)) {
  cand <- intersect(
    c("codigo_municipio_residencia", "residence_municipality_code",
      "codigo_municipio", "municipality_code", "CODMUNRES"),
    names(serie_saude)
  )[1]
  if (!is.na(cand)) serie_saude$code_muni <- substr(as.character(serie_saude[[cand]]), 1, 6)
}
n_antes <- nrow(serie_saude)
message("   linhas de entrada: ", format(n_antes, big.mark = "."))


# =============================================================================
# BLOCO 2 - sus_spatial_join(): ligar a serie aos municipios (nivel "munic")
# =============================================================================
# sus_spatial_join() detecta a coluna geografica (join_col = NULL -> auto),
# baixa as geometrias municipais do geobr (com cache em ~/.climasus4r_cache/
# spatial) e devolve um objeto sf em stage = "spatial". Aqui usamos level =
# "munic", o nivel administrativo padrao. Protegemos sf/geobr/rede com tryCatch.
message(">> Bloco 2: sus_spatial_join() ao nivel municipal (geobr)...")

caso_espacial <- tryCatch({
  if (!requireNamespace("sf", quietly = TRUE) ||
      !requireNamespace("geobr", quietly = TRUE)) {
    stop("Pacotes 'sf' e/ou 'geobr' indisponiveis.")
  }
  # sus_spatial_join() renomeia a coluna de juncao para 'code_muni' apos o join;
  # removemos o 'code_muni' auxiliar (Bloco 1) para evitar nomes duplicados.
  serie_para_join <- serie_saude
  if (all(c("code_muni", "codigo_municipio_residencia") %in% names(serie_para_join))) {
    serie_para_join$code_muni <- NULL
  }
  sus_spatial_join(
    df        = serie_para_join,
    level     = "munic",     # municipio (padrao); outros: biomes, metro_area, cep...
    join_col  = NULL,         # auto-deteccao da coluna de codigo municipal
    lang      = "pt",
    use_cache = TRUE,         # reaproveita geometrias ja baixadas
    cache_dir = "~/.climasus4r_cache/spatial",
    verbose   = TRUE
  )
}, error = function(e) {
  message("   sus_spatial_join indisponivel: ", conditionMessage(e)); NULL
})


# =============================================================================
# BLOCO 3 - (Ilustrativo) sus_grid_join(): anexar clima em grade a saude
# =============================================================================
# sus_grid_join() une a saida de qualquer funcao sus_grid_*() (ERA5, CHIRPS...),
# em stage = "climate", ao objeto de saude por c("code_muni","date"). Aqui apenas
# DEMONSTRAMOS a chamada com um grid sintetico (clima real entra no T8). O codigo
# real e mostrado, eval=FALSE, na vignette; aqui ele e protegido e ilustrativo.
message(">> Bloco 3: sus_grid_join() ilustrativo (grid sintetico)...")

caso_clima_demo <- tryCatch({
  if (is.null(caso_espacial)) stop("sem objeto espacial para demonstrar o join")
  # Grid sintetico: temperatura media mensal por municipio (stage 'climate').
  base_sf <- sf::st_drop_geometry(caso_espacial)
  grid_df <- dplyr::distinct(dplyr::as_tibble(base_sf), code_muni, date)
  set.seed(7)
  grid_df$t2m <- round(stats::rnorm(nrow(grid_df), mean = 21, sd = 4), 1)
  meta_grid <- list(
    system = "ERA5", stage = "climate", type = "era5_land", backend = "tibble",
    created = Sys.time(), modified = Sys.time(),
    history = "[grid sintetico]", user = list()
  )
  grid_clim <- structure(grid_df, sus_meta = meta_grid,
                          class = c("climasus_df", class(grid_df)))
  sus_grid_join(
    health_data = caso_espacial,
    grid_data   = grid_clim,
    by          = c("code_muni", "date"),  # chave espaco-temporal
    type_out    = NULL,                      # herda o type do grid
    lang        = "pt",
    verbose     = TRUE
  )
}, error = function(e) {
  message("   sus_grid_join (demo) pulado: ", conditionMessage(e)); NULL
})


# =============================================================================
# BLOCO 4 - Tabela-resumo da juncao espacial
# =============================================================================
message(">> Bloco 4: tabela-resumo da juncao...")

crs_txt <- tryCatch({
  if (!is.null(caso_espacial)) {
    epsg <- sf::st_crs(caso_espacial)$epsg
    if (!is.na(epsg)) paste0("EPSG:", epsg) else "definido"
  } else "indisponivel (sf/geobr ausentes)"
}, error = function(e) "indisponivel")

n_depois  <- if (!is.null(caso_espacial)) nrow(caso_espacial) else NA_integer_
n_munis   <- if (!is.null(caso_espacial)) {
  dplyr::n_distinct(sf::st_drop_geometry(caso_espacial)$code_muni)
} else {
  NA_integer_
}

geo_resumo <- data.frame(
  Item = c(
    "Funcao",
    "Nivel (level)",
    "Coluna de juncao",
    "Sistema de referencia (CRS)",
    "Linhas antes da juncao",
    "Linhas depois da juncao",
    "Municipios distintos",
    "Tipo de geometria"
  ),
  Valor = c(
    "sus_spatial_join()",
    "munic (municipio)",
    "code_muni (auto-detectada)",
    crs_txt,
    format(n_antes,  big.mark = "."),
    if (is.na(n_depois)) "indisponivel" else format(n_depois, big.mark = "."),
    if (is.na(n_munis))  "indisponivel" else as.character(n_munis),
    "MULTIPOLYGON (municipio)"
  ),
  stringsAsFactors = FALSE
)
save_tbl(geo_resumo, "geo_resumo")

# Amostra das colunas-chave do objeto sf resultante (sem a geometria).
geo_amostra <- if (!is.null(caso_espacial)) {
  cols <- intersect(
    c("code_muni", "name_muni", "abbrev_state", "date", "obitos"),
    names(sf::st_drop_geometry(caso_espacial))
  )
  sf::st_drop_geometry(caso_espacial) |>
    dplyr::select(dplyr::all_of(cols)) |>
    utils::head(10) |>
    as.data.frame()
} else {
  data.frame(
    aviso = "Objeto espacial indisponivel: rode com sf + geobr instalados e internet.",
    stringsAsFactors = FALSE
  )
}
save_tbl(geo_amostra, "geo_amostra")


# =============================================================================
# BLOCO 5 - Figura: mapa coropleto de obitos por municipio (RMSP)
# =============================================================================
message(">> Bloco 5: mapa coropleto por municipio...")

if (!is.null(caso_espacial)) {
  # Agrega obitos por municipio para o mapa (soma na janela 2014-2019).
  # Nota: summarise() em um climasus_df+sf pode despachar pelo metodo tibble e
  # perder a geometria; agregamos sem geometria e religamos os poligonos depois.
  base_no_geo <- sf::st_drop_geometry(caso_espacial)
  val_col <- intersect(c("n_obitos", "obitos", "n", "value"),
                       names(base_no_geo))[1]
  if (is.na(val_col)) {
    base_no_geo$obitos <- 1L
    val_col <- "obitos"
  }
  totais <- base_no_geo |>
    dplyr::group_by(code_muni) |>
    dplyr::summarise(
      total = sum(.data[[val_col]], na.rm = TRUE),
      .groups = "drop"
    )
  geom_col  <- attr(caso_espacial, "sf_column") %||% "geom"
  geo_unico <- as.data.frame(caso_espacial)[
    !duplicated(caso_espacial$code_muni), c("code_muni", geom_col)
  ]
  mapa_sf <- sf::st_as_sf(dplyr::left_join(geo_unico, totais, by = "code_muni"))

  p_mapa <- ggplot2::ggplot(mapa_sf) +
    ggplot2::geom_sf(ggplot2::aes(fill = total), color = "white", linewidth = 0.2) +
    ggplot2::scale_fill_gradient(low = "#A5D6A7", high = COR_PRIMARIA,
                                 name = "Obitos") +
    ggplot2::labs(
      title    = "Obitos respiratorios por municipio - apos juncao espacial",
      subtitle = "Regiao Metropolitana de Sao Paulo, 2014-2019 (sus_spatial_join, nivel munic)",
      x = NULL, y = NULL
    ) +
    tema_climasus()
  save_fig(p_mapa, "geo_mapa_obitos", width = 8, height = 5)
} else {
  message("   AVISO: geo_mapa_obitos.png NAO foi gerado (sf/geobr ausentes?).")
}


# =============================================================================
# BLOCO 6 - Figura: serie temporal preservada apos a juncao
# =============================================================================
message(">> Bloco 6: serie temporal apos a juncao...")

serie_para_plot <- if (!is.null(caso_espacial)) {
  sf::st_drop_geometry(caso_espacial)
} else {
  dplyr::as_tibble(serie_saude)
}

if (all(c("date") %in% names(serie_para_plot))) {
  val_col <- intersect(c("n_obitos", "obitos", "n", "value"),
                       names(serie_para_plot))[1]
  if (is.na(val_col)) { serie_para_plot$obitos <- 1L; val_col <- "obitos" }

  serie_mensal <- serie_para_plot |>
    dplyr::mutate(mes = lubridate::floor_date(as.Date(date), "month")) |>
    dplyr::group_by(mes) |>
    dplyr::summarise(total = sum(.data[[val_col]], na.rm = TRUE), .groups = "drop")

  p_serie <- ggplot2::ggplot(serie_mensal, ggplot2::aes(x = mes, y = total)) +
    ggplot2::geom_line(color = COR_PRIMARIA, linewidth = 0.8) +
    ggplot2::geom_point(color = COR_PRIMARIA, size = 1.1) +
    ggplot2::labs(
      title    = "Serie mensal preservada apos a juncao espacial",
      subtitle = "A juncao adiciona geometria sem alterar a estrutura temporal (RMSP)",
      x = "Mes", y = "Obitos respiratorios"
    ) +
    tema_climasus()
  save_fig(p_serie, "geo_serie", width = 8, height = 5)
} else {
  message("   AVISO: geo_serie.png NAO foi gerado (sem coluna 'date').")
}


# =============================================================================
# BLOCO 7 - Figura: diagnostico de cobertura (com/sem correspondencia)
# =============================================================================
# sus_spatial_join() usa inner_join: codigos sem geometria correspondente sao
# descartados. Esta figura quantifica a taxa de correspondencia (codigos de
# entrada que encontraram geometria), um controle de qualidade do MAUP/joins.
message(">> Bloco 7: diagnostico de cobertura da juncao...")

cod_entrada <- unique(substr(as.character(serie_saude$code_muni), 1, 6))
cod_saida   <- if (!is.null(caso_espacial)) {
  unique(substr(as.character(sf::st_drop_geometry(caso_espacial)$code_muni), 1, 6))
} else {
  character(0)
}

n_match    <- length(intersect(cod_entrada, cod_saida))
n_no_match <- length(setdiff(cod_entrada, cod_saida))

cobertura <- dplyr::tibble(
  status = factor(c("Com geometria", "Sem correspondencia"),
                  levels = c("Com geometria", "Sem correspondencia")),
  n      = c(n_match, n_no_match)
)

p_cob <- ggplot2::ggplot(cobertura, ggplot2::aes(x = status, y = n, fill = status)) +
  ggplot2::geom_col(width = 0.6, show.legend = FALSE) +
  ggplot2::geom_text(ggplot2::aes(label = n), vjust = -0.4,
                     color = COR_PRIMARIA, fontface = "bold") +
  ggplot2::scale_fill_manual(values = c(COR_PRIMARIA, COR_ACENTO)) +
  ggplot2::labs(
    title    = "Cobertura da juncao espacial (codigos municipais)",
    subtitle = "inner_join descarta codigos sem geometria correspondente no geobr",
    x = NULL, y = "Municipios distintos"
  ) +
  tema_climasus()
save_fig(p_cob, "geo_cobertura", width = 8, height = 5)


# =============================================================================
# BLOCO 8 - Encadeamento: salvar o objeto espacial para o Tutorial 6
# =============================================================================
message(">> Bloco 8: salvando o objeto espacial para o proximo tutorial...")

saida <- if (!is.null(caso_espacial)) caso_espacial else serie_saude
save_tbl(saida, "caso_espacial")

message("== Concluido. Artefatos em vignettes-pt/figuras/ e vignettes-pt/dados/ ==")
