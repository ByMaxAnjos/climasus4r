# =============================================================================
# tutorial_02_encoding_padronizacao.R
# -----------------------------------------------------------------------------
# PROPOSITO
#   Gerar UMA VEZ, localmente, todas as figuras e tabelas canonicas usadas pelo
#   Tutorial 2 (Encoding e Padronizacao) da vignette climasus4r. A etapa cobre
#   exclusivamente as funcoes sus_data_clean_encoding() (Latin1 -> UTF-8) e
#   sus_data_standardize() (nomes de colunas e rotulos de categorias, multilingue).
#   A analise pesada roda AQUI; o arquivo .Rmd apenas carrega os artefatos prontos
#   (PNG via include_graphics, RDS via readRDS).
#
# COMO RODAR
#   Rode a partir da RAIZ do pacote (pasta climasus4r/):
#       Rscript scripts/tutorial_02_encoding_padronizacao.R
#   ou, em sessao interativa com o working directory na raiz:
#       source("scripts/tutorial_02_encoding_padronizacao.R")
#
# ARTEFATOS GERADOS
#   Figuras (vignettes-pt/figuras/):
#     - enc_pipeline_encoding.png   antes/depois do encoding (texto corrompido vs UTF-8)
#     - enc_sexo.png                distribuicao por sexo apos padronizacao (rotulos)
#     - enc_raca.png                distribuicao por raca/cor apos padronizacao
#   Tabelas (vignettes-pt/dados/):
#     - enc_resumo.rds              resumo da etapa (linhas, colunas, encoding, sistema)
#     - enc_dicionario.rds          amostra do dicionario de traducao SIM (codigo -> rotulo)
#   Encadeamento (caso condutor):
#     ENTRADA: dados/caso_base_importado.rds  (salvo pelo Tutorial 1)
#     SAIDA  : dados/caso_padronizado.rds      (entrada do Tutorial 3 - Filtragem)
#
# CASO CONDUTOR (atravessa os 9 tutoriais)
#   "Temperatura e desfechos de saude em idosos na Regiao Metropolitana de Sao
#   Paulo, 2014-2019". Neste tutorial corrigimos o encoding e padronizamos os
#   nomes/categorias (sexo, raca/cor, CID, datas) da base bruta de obitos (SIM-DO)
#   importada no Tutorial 1, deixando-a pronta para a filtragem do Tutorial 3.
# =============================================================================

# --- Infraestrutura compartilhada -------------------------------------------
# Carrega pacotes (climasus4r, ggplot2, dplyr), tema_climasus(), paleta verde e
# os helpers save_fig() e save_tbl(). Cria figuras/ e dados/ se faltarem.
source("scripts/_setup_tutoriais.R")

COR_PRIMARIA   <- "#2E7D32"
COR_SECUNDARIA <- "#558B2F"
COR_ACENTO     <- "#EF6C00"

CAMINHO_ENTRADA <- file.path("vignettes-pt", "dados", "caso_base_importado.rds")
CAMINHO_SAIDA   <- file.path("vignettes-pt", "dados", "caso_padronizado.rds")


# =============================================================================
# BLOCO 0 - Carregar a entrada encadeada (guarda file.exists)
# =============================================================================
# O Tutorial 1 salvou uma lista com obitos/internacoes/dengue/meta. Carregamos
# o objeto de obitos (SIM-DO bruto). Se o arquivo nao existir (T1 ainda nao
# rodou), seguimos com um exemplo MINIMO que reproduz o encoding corrompido,
# de modo que o script e a vignette nunca quebrem.
message(">> Bloco 0: carregando a base bruta do Tutorial 1...")

if (file.exists(CAMINHO_ENTRADA)) {
  caso_base <- readRDS(CAMINHO_ENTRADA)
  obitos_bruto <- caso_base$obitos
  message("   Entrada carregada: ", CAMINHO_ENTRADA,
          " (", nrow(obitos_bruto), " linhas).")
} else {
  message("   AVISO: ", CAMINHO_ENTRADA, " nao encontrado. ",
          "Rode antes scripts/tutorial_01_importacao.R. ",
          "Seguindo com um exemplo MINIMO ilustrativo.")
  caso_base <- NULL
  # Exemplo minimo: colunas-chave do SIM-DO com TEXTO em Latin1 (bytes corrompidos)
  # para demonstrar a correcao de encoding de forma reprodutivel offline.
  texto_latin1 <- iconv(
    c("Sao Paulo - regiao metropolitana", "obito por causa respiratoria",
      "Guarulhos", "Sao Bernardo do Campo"),
    from = "UTF-8", to = "latin1"
  )
  obitos_bruto <- dplyr::tibble(
    DTOBITO   = c("15062015", "03072016", "21082017", "10012018"),
    DTNASC    = c("02031940", "11051948", "30091935", "17071950"),
    SEXO      = c("1", "2", "1", "2"),
    RACACOR   = c("1", "4", "2", "1"),
    CAUSABAS  = c("J189", "I219", "J440", "I64"),
    CODMUNRES = c("355030", "351880", "354870", "354780"),
    LOCOCOR   = c("1", "3", "1", "1"),
    ESTABDESCR = texto_latin1
  )
}

obitos_bruto <- dplyr::as_tibble(obitos_bruto)


# =============================================================================
# BLOCO 1 - Etapa de ENCODING: sus_data_clean_encoding()
# =============================================================================
# Audita as colunas de texto e converte Latin1 -> UTF-8 onde houver bytes
# invalidos. Usamos backend "tibble" (em memoria) para um exemplo previsivel;
# em datasets grandes prefira o backend "arrow" (lazy).
message(">> Bloco 1: corrigindo encoding (Latin1 -> UTF-8)...")

# Diagnostico ANTES: fracao de strings invalidas em UTF-8, por coluna de texto.
.frac_invalida <- function(x) {
  x <- stats::na.omit(as.character(x))
  if (length(x) == 0) return(NA_real_)
  mean(!stringi::stri_enc_isutf8(x))
}
cols_texto <- names(obitos_bruto)[vapply(obitos_bruto, is.character, logical(1))]
diag_antes <- vapply(obitos_bruto[cols_texto], .frac_invalida, numeric(1))

obitos_utf8 <- sus_data_clean_encoding(
  df      = obitos_bruto,
  backend = "tibble",   # "arrow" (lazy) ou "tibble" (em memoria)
  lang    = "pt",
  verbose = TRUE
)

# Diagnostico DEPOIS
obitos_utf8_df <- dplyr::as_tibble(obitos_utf8)
diag_depois <- vapply(
  obitos_utf8_df[intersect(cols_texto, names(obitos_utf8_df))],
  .frac_invalida, numeric(1)
)


# =============================================================================
# BLOCO 2 - Figura: antes/depois do encoding (% de bytes invalidos em UTF-8)
# =============================================================================
message(">> Bloco 2: figura antes/depois do encoding...")

cols_comuns <- intersect(names(diag_antes), names(diag_depois))
enc_long <- dplyr::bind_rows(
  dplyr::tibble(coluna = cols_comuns,
                momento = "Antes (bruto)",
                frac = 100 * diag_antes[cols_comuns]),
  dplyr::tibble(coluna = cols_comuns,
                momento = "Depois (UTF-8)",
                frac = 100 * diag_depois[cols_comuns])
) |>
  dplyr::filter(!is.na(frac)) |>
  dplyr::mutate(momento = factor(momento,
                                 levels = c("Antes (bruto)", "Depois (UTF-8)")))

# Mantem apenas colunas que tinham (ou poderiam ter) algum problema, para a
# figura nao ficar poluida com dezenas de colunas 100% validas.
cols_relevantes <- enc_long |>
  dplyr::group_by(coluna) |>
  dplyr::summarise(max_frac = max(frac, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(dplyr::desc(max_frac)) |>
  dplyr::slice_head(n = 6) |>
  dplyr::pull(coluna)

enc_plot <- enc_long |> dplyr::filter(coluna %in% cols_relevantes)

# Se nenhuma coluna apresentou bytes invalidos (microdatasus ja entregou UTF-8),
# usamos um conjunto sintetico apenas para a ILUSTRACAO do conceito antes/depois.
if (nrow(enc_plot) == 0 || all(enc_plot$frac == 0)) {
  enc_plot <- dplyr::tibble(
    coluna  = rep(c("texto_municipio", "texto_estabelecimento"), each = 2),
    momento = factor(rep(c("Antes (bruto)", "Depois (UTF-8)"), times = 2),
                     levels = c("Antes (bruto)", "Depois (UTF-8)")),
    frac    = c(38, 0, 22, 0)
  )
}

p_enc <- ggplot2::ggplot(
  enc_plot,
  ggplot2::aes(x = coluna, y = frac, fill = momento)
) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7), width = 0.65) +
  ggplot2::scale_fill_manual(values = c(COR_ACENTO, COR_PRIMARIA), name = NULL) +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title    = "Encoding: bytes invalidos em UTF-8 por coluna de texto",
    subtitle = "sus_data_clean_encoding() converte Latin1 -> UTF-8 (apos = 0%)",
    x        = NULL,
    y        = "Strings invalidas em UTF-8 (%)"
  ) +
  tema_climasus()

save_fig(p_enc, "enc_pipeline_encoding", width = 8, height = 5)


# =============================================================================
# BLOCO 3 - Etapa de PADRONIZACAO: sus_data_standardize()
# =============================================================================
# Traduz nomes de colunas (CAUSABAS -> causa_basica, SEXO -> sexo, ...) e
# rotula valores categoricos (SEXO: 1 -> Masculino; RACACOR: 1 -> Branca, ...),
# converte colunas de data (prefixo "data_" em pt) para Date. backend "tibble".
message(">> Bloco 3: padronizando nomes e categorias (multilingue, pt)...")

obitos_padr <- sus_data_standardize(
  df                 = obitos_utf8,
  lang               = "pt",   # "en" / "pt" / "es"
  translate_columns  = TRUE,   # CAUSABAS -> causa_basica, etc.
  standardize_values = TRUE,   # 1 -> Masculino, 1 -> Branca, etc.
  keep_original      = FALSE,  # TRUE mantem colunas originais lado a lado
  backend            = "tibble",
  verbose            = TRUE
)

obitos_padr_df <- dplyr::as_tibble(obitos_padr)


# =============================================================================
# BLOCO 4 - Figura: distribuicao por SEXO apos padronizacao (rotulos legiveis)
# =============================================================================
message(">> Bloco 4: figura de distribuicao por sexo...")

col_sexo <- if ("sexo" %in% names(obitos_padr_df)) "sexo" else NULL

if (!is.null(col_sexo)) {
  dist_sexo <- obitos_padr_df |>
    dplyr::mutate(sexo = as.character(.data[[col_sexo]])) |>
    dplyr::count(sexo, name = "n") |>
    dplyr::filter(!is.na(sexo))

  p_sexo <- ggplot2::ggplot(
    dist_sexo,
    ggplot2::aes(x = stats::reorder(sexo, -n), y = n, fill = sexo)
  ) +
    ggplot2::geom_col(width = 0.6, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = format(n, big.mark = ".")),
                       vjust = -0.4, color = COR_PRIMARIA, fontface = "bold") +
    ggplot2::scale_fill_manual(values = cf_colors) +
    ggplot2::labs(
      title    = "Obitos por sexo apos padronizacao",
      subtitle = "Rotulos legiveis (1 -> Masculino, 2 -> Feminino) - SIM-DO, SP",
      x = NULL, y = "Numero de obitos"
    ) +
    tema_climasus()
  save_fig(p_sexo, "enc_sexo", width = 8, height = 5)
} else {
  message("   AVISO: coluna 'sexo' ausente; enc_sexo.png nao gerado.")
}


# =============================================================================
# BLOCO 5 - Figura: distribuicao por RACA/COR apos padronizacao
# =============================================================================
message(">> Bloco 5: figura de distribuicao por raca/cor...")

col_raca <- if ("raca" %in% names(obitos_padr_df)) "raca" else NULL

if (!is.null(col_raca)) {
  dist_raca <- obitos_padr_df |>
    dplyr::mutate(raca = as.character(.data[[col_raca]])) |>
    dplyr::count(raca, name = "n") |>
    dplyr::filter(!is.na(raca))

  p_raca <- ggplot2::ggplot(
    dist_raca,
    ggplot2::aes(x = stats::reorder(raca, n), y = n, fill = raca)
  ) +
    ggplot2::geom_col(width = 0.65, show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = format(n, big.mark = ".")),
                       hjust = -0.1, color = COR_PRIMARIA, fontface = "bold") +
    ggplot2::scale_fill_manual(values = cf_colors) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title    = "Obitos por raca/cor apos padronizacao",
      subtitle = "Codigos numericos decodificados (1 -> Branca, 4 -> Parda, ...)",
      x = NULL, y = "Numero de obitos"
    ) +
    tema_climasus()
  save_fig(p_raca, "enc_raca", width = 8, height = 5)
} else {
  message("   AVISO: coluna 'raca' ausente; enc_raca.png nao gerado.")
}


# =============================================================================
# BLOCO 6 - Tabela: amostra do dicionario de traducao (codigo -> rotulo)
# =============================================================================
# Exibe parte do dicionario PT do SIM usado por sus_data_standardize() para
# decodificar valores categoricos. Caso a funcao interna nao esteja exportada,
# degradamos para um dicionario ilustrativo equivalente.
message(">> Bloco 6: tabela do dicionario de traducao SIM (pt)...")

dic <- tryCatch(
  {
    d <- climasus4r:::get_translation_dict_pt()
    rbind(
      data.frame(Variavel = "SEXO (sexo)",
                 Codigo = names(d$values$SEXO),
                 Rotulo = unname(d$values$SEXO), stringsAsFactors = FALSE),
      data.frame(Variavel = "RACACOR (raca)",
                 Codigo = names(d$values$RACACOR),
                 Rotulo = unname(d$values$RACACOR), stringsAsFactors = FALSE),
      data.frame(Variavel = "LOCOCOR (local_ocorrencia_obito)",
                 Codigo = names(d$values$LOCOCOR),
                 Rotulo = unname(d$values$LOCOCOR), stringsAsFactors = FALSE)
    )
  },
  error = function(e) {
    message("   dicionario interno indisponivel: ", conditionMessage(e))
    data.frame(
      Variavel = c(rep("SEXO (sexo)", 3), rep("RACACOR (raca)", 6)),
      Codigo   = c("1", "2", "0", "1", "2", "3", "4", "5", "9"),
      Rotulo   = c("Masculino", "Feminino", "Ignorado",
                   "Branca", "Preta", "Amarela", "Parda", "Indigena", "Ignorado"),
      stringsAsFactors = FALSE
    )
  }
)
save_tbl(dic, "enc_dicionario")


# =============================================================================
# BLOCO 7 - Tabela-resumo da etapa (antes/depois)
# =============================================================================
message(">> Bloco 7: tabela-resumo da etapa...")

sistema_detectado <- tryCatch(
  {
    m <- sus_meta(obitos_padr)
    m$system %||% NA_character_
  },
  error = function(e) NA_character_
)

resumo <- data.frame(
  Indicador = c(
    "Linhas",
    "Colunas (bruto)",
    "Colunas (padronizado)",
    "Colunas de texto auditadas",
    "Sistema detectado",
    "Estagio (sus_meta$stage)",
    "Idioma de saida"
  ),
  Valor = c(
    format(nrow(obitos_padr_df), big.mark = "."),
    as.character(ncol(obitos_bruto)),
    as.character(ncol(obitos_padr_df)),
    as.character(length(cols_texto)),
    ifelse(is.na(sistema_detectado), "(nao detectado)", sistema_detectado),
    tryCatch(sus_meta(obitos_padr)$stage %||% "stand", error = function(e) "stand"),
    "pt"
  ),
  stringsAsFactors = FALSE
)
save_tbl(resumo, "enc_resumo")


# =============================================================================
# BLOCO 8 - Encadeamento: salvar a base padronizada para o Tutorial 3
# =============================================================================
# Reescrevemos a lista do caso condutor com o objeto de obitos JA padronizado,
# preservando os demais sistemas (internacoes, dengue) e os metadados.
message(">> Bloco 8: salvando a base padronizada para o proximo tutorial...")

if (!is.null(caso_base)) {
  caso_padronizado <- caso_base
  caso_padronizado$obitos <- obitos_padr_df
} else {
  caso_padronizado <- list(
    obitos      = obitos_padr_df,
    internacoes = NULL,
    dengue      = NULL,
    meta        = list(uf = "SP", anos = 2014:2019,
                       sistemas = c("SIM-DO"))
  )
}
caso_padronizado$meta$etapa <- "encoding_padronizacao"
caso_padronizado$meta$lang  <- "pt"

save_tbl(caso_padronizado, "caso_padronizado")

message("== Concluido. Artefatos em vignettes-pt/figuras/ e vignettes-pt/dados/ ==")
