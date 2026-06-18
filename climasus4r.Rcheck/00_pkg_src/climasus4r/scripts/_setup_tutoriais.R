# ============================================================================
# _setup_tutoriais.R
# Infraestrutura compartilhada dos tutoriais do climasus4r.
#
# IMPORTANTE: os scripts de tutorial (scripts/tutorial_*.R) rodam SEMPRE a
# partir da RAIZ do pacote. Portanto, todos os caminhos relativos abaixo
# (vignettes-pt/figuras e vignettes-pt/dados) assumem o diretório de trabalho
# na raiz do projeto. Use:  source("scripts/_setup_tutoriais.R")
#
# O que este arquivo provê (contrato fixo):
#   - Carrega os pacotes: climasus4r, ggplot2, dplyr
#   - tema_climasus()  : tema ggplot2 do projeto ("Climate Forest")
#   - cf_palette       : vetor de cores da paleta verde do projeto
#   - save_fig(plot, name, width = 8, height = 5, dpi = 150)
#       -> salva PNG em vignettes-pt/figuras/<name>.png
#   - save_tbl(obj, name)
#       -> saveRDS em vignettes-pt/dados/<name>.rds
# ============================================================================

# --- Pacotes ----------------------------------------------------------------
suppressPackageStartupMessages({
  library(climasus4r)
  library(ggplot2)
  library(dplyr)
})

# --- Paleta "Climate Forest" -------------------------------------------------
# Verde primário, verde secundário, acento (laranja), texto escuro, fundo claro.
cf_palette <- c(
  primary   = "#2E7D32",  # verde floresta
  secondary = "#558B2F",  # verde folha
  accent    = "#EF6C00",  # laranja queimado
  text_main = "#1B5E20",  # texto escuro
  bg_page   = "#F1F8E9"   # fundo menta suave
)

# Sequência de cores pronta para escalas discretas em gráficos.
cf_colors <- unname(c(
  cf_palette[["primary"]],
  cf_palette[["secondary"]],
  cf_palette[["accent"]],
  "#A5D6A7",  # verde claro
  "#1B5E20"   # verde muito escuro
))

# --- Tema ggplot2 do projeto -------------------------------------------------
tema_climasus <- function(base_size = 12,
                          base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Texto e títulos
      plot.title = ggplot2::element_text(
        color = cf_palette[["text_main"]],
        face = "bold",
        size = ggplot2::rel(1.25),
        margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle = ggplot2::element_text(
        color = cf_palette[["secondary"]],
        size = ggplot2::rel(1.0),
        margin = ggplot2::margin(b = 10)
      ),
      plot.caption = ggplot2::element_text(
        color = "#52665A",
        size = ggplot2::rel(0.8),
        hjust = 0
      ),
      axis.title = ggplot2::element_text(
        color = cf_palette[["text_main"]],
        size = ggplot2::rel(0.95)
      ),
      axis.text = ggplot2::element_text(color = "#3A4A3E"),
      # Grade suave
      panel.grid.major = ggplot2::element_line(
        color = "grey88",
        linewidth = 0.35
      ),
      panel.grid.minor = ggplot2::element_blank(),
      # Legenda
      legend.title = ggplot2::element_text(
        color = cf_palette[["text_main"]],
        face = "bold"
      ),
      legend.position = "bottom",
      # Fundo limpo
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      strip.text = ggplot2::element_text(
        color = cf_palette[["text_main"]],
        face = "bold"
      ),
      plot.margin = ggplot2::margin(12, 12, 12, 12)
    )
}

# --- Diretórios de saída -----------------------------------------------------
# Criados a partir da raiz do pacote; não falham se já existirem.
.fig_dir <- file.path("vignettes-pt", "figuras")
.tbl_dir <- file.path("vignettes-pt", "dados")
dir.create(.fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(.tbl_dir, showWarnings = FALSE, recursive = TRUE)

# --- Helpers de salvamento ---------------------------------------------------

# Salva um gráfico ggplot como PNG em vignettes-pt/figuras/<name>.png
save_fig <- function(plot, name, width = 8, height = 5, dpi = 150) {
  path <- file.path("vignettes-pt", "figuras", paste0(name, ".png"))
  ggplot2::ggsave(
    filename = path,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    units = "in",
    bg = "white"
  )
  message("Figura salva em: ", path)
  invisible(path)
}

# Salva qualquer objeto R (tabela, data.frame, etc.) como RDS em
# vignettes-pt/dados/<name>.rds
save_tbl <- function(obj, name) {
  path <- file.path("vignettes-pt", "dados", paste0(name, ".rds"))
  saveRDS(obj, file = path)
  message("Tabela salva em: ", path)
  invisible(path)
}
