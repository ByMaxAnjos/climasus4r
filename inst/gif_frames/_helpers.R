# =============================================================================
# _helpers.R
# -----------------------------------------------------------------------------
# Infraestrutura compartilhada dos scripts scripts/gif_frames/frame_*.R.
# Cada frame_*.R roda SOZINHO (Rscript scripts/gif_frames/frame_05_...R) a
# partir da RAIZ do pacote e escreve seu PNG em man/figures/frames/.
#
# O painel de codigo usa destaque de sintaxe REAL (highr::hilight(), o mesmo
# tokenizer que o knitr usa para colorir chunks de R), nao apenas texto verde.
# Renderizado como HTML/CSS e capturado com webshot2 (Chrome headless) --
# magick::image_annotate() quebra em QUALQUER chamada de texto nesta maquina
# (bug do delegate de fonte do ImageMagick), entao evitamos texto via magick
# em todos os paineis.
# =============================================================================

devtools::load_all(quiet = TRUE)
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

.frames_dir <- file.path("man", "figures", "frames")
dir.create(.frames_dir, showWarnings = FALSE, recursive = TRUE)

.code_css <- '
body { margin:0; background:#1e1e2e; }
pre.code {
  font-family: "SF Mono","Menlo","Consolas",monospace;
  font-size: 18px; line-height: 1.7;
  color: #cdd6f4; background:#1e1e2e;
  padding: 22px 28px; margin:0; white-space: pre;
}
.hl.kwd { color:#89b4fa; font-weight:600; }  /* nome de funcao */
.hl.kwc { color:#fab387; }                   /* nome de argumento */
.hl.kwb { color:#f38ba8; font-weight:600; }  /* atribuicao / pipe */
.hl.sng { color:#a6e3a1; }                   /* string */
.hl.str { color:#a6e3a1; }
.hl.num { color:#f9e2af; }                   /* numero */
.hl.com { color:#6c7086; font-style: italic; } /* comentario */
.hl.opt { color:#cdd6f4; }
.hl.def { color:#cdd6f4; }
'

# Renderiza `code_lines` (codigo R real, uma linha por elemento) com destaque
# de sintaxe e retorna o caminho do PNG gerado (fundo escuro, largura fixa).
codigo_imagem <- function(code_lines, name, width_px = NULL) {
  rlang::check_installed(c("highr", "webshot2", "htmltools"),
                          reason = "para renderizar o painel de codigo com destaque de sintaxe")
  spans <- paste(highr::hilight(code_lines, format = "html"), collapse = "\n")
  # largura dinamica: garante que a linha mais longa nao seja cortada, mesmo
  # com chamadas reais de varios argumentos (ex.: sus_mod_dlnm(), com 6+ args)
  if (is.null(width_px)) {
    max_chars <- max(nchar(code_lines))
    width_px <- max(1200, 56 + max_chars * 11)
  }
  height_px <- 60 + length(code_lines) * 34
  html <- sprintf(
    '<!DOCTYPE html><html><head><meta charset="utf-8"><style>%s</style></head><body><pre class="code">%s</pre></body></html>',
    .code_css, spans
  )
  html_path <- file.path(tempdir(), paste0(name, "_code.html"))
  png_path  <- file.path(.frames_dir, paste0(name, "_code.png"))
  writeLines(html, html_path)
  webshot2::webshot(html_path, file = png_path, vwidth = width_px, vheight = height_px, delay = 0.3)
  png_path
}

# Empilha a imagem de codigo (topo) sobre o grafico `plot` (ggplot ou caminho
# de PNG ja pronto, ex.: screenshot do plotly) e salva o frame combinado.
save_frame <- function(code_lines, plot, name) {
  code_png <- codigo_imagem(code_lines, name)
  plot_png <- file.path(tempdir(), paste0(name, "_plot.png"))
  if (is.character(plot)) {
    file.copy(plot, plot_png, overwrite = TRUE)
  } else {
    ggplot2::ggsave(plot_png, plot, width = 8, height = 5, dpi = 150, bg = "white")
  }

  code_img <- magick::image_read(code_png)
  plot_img <- magick::image_read(plot_png)
  w <- max(magick::image_info(code_img)$width, magick::image_info(plot_img)$width)
  resize_w <- magick::geometry_size_pixels(width = w)
  code_img <- magick::image_resize(code_img, resize_w)
  plot_img <- magick::image_background(magick::image_resize(plot_img, resize_w), "white")

  combo <- magick::image_append(c(code_img, plot_img), stack = TRUE)
  path <- file.path(.frames_dir, paste0(name, ".png"))
  magick::image_write(combo, path)
  message("Frame salvo em: ", path)
  invisible(path)
}
