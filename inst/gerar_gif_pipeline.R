# =============================================================================
# gerar_gif_pipeline.R
# -----------------------------------------------------------------------------
# PROPOSITO
#   Montador do GIF do pipeline climasus4r. Cada etapa vive em seu proprio
#   script standalone e testavel em scripts/gif_frames/frame_NN_*.R (rode
#   qualquer um sozinho, ex.: Rscript scripts/gif_frames/frame_05_dlnm_
#   contorno.R). Este script so roda todos em ordem e monta o GIF a partir
#   dos PNGs que eles escrevem em man/figures/frames/.
#
#   Sem sus_data_plot_aggregate_map() (mapas removidos a pedido). Cada frame
#   mostra, acima do grafico, a chamada real do climasus4r que o gerou.
#   Termina no DLNM (superficie 3D exposicao x lag).
#
# COMO RODAR
#   Rscript scripts/gerar_gif_pipeline.R
#
# SAIDA
#   man/figures/climasus4r_pipeline.gif
# =============================================================================

if (!requireNamespace("magick", quietly = TRUE)) {
  stop("Pacote 'magick' necessario. Instale com install.packages('magick').")
}

# nome do frame_NN_*.R -> quantos frames repetir no GIF (tempo de exibicao)
etapas <- c(
  "frame_01_serie_epidemica.R"     = 8,
  "frame_02_serie_sazonal.R"       = 8,
  "frame_03_clima_distribuicao.R"  = 8,
  "frame_04_dlnm_distribuicao.R"   = 8,
  "frame_05_dlnm_contorno.R"       = 10,
  "frame_06_dlnm_rr_geral.R"       = 10,
  "frame_07_dlnm_rr_lag.R"         = 10,
  "frame_08_dlnm_superficie3d.R"   = 24
)

for (script in names(etapas)) {
  message(">> Rodando scripts/gif_frames/", script, "...")
  source(file.path("scripts", "gif_frames", script))
}

frames_dir <- file.path("man", "figures", "frames")
frame_names <- sub("^frame_(.*)\\.R$", "\\1", names(etapas))
frame_paths <- file.path(frames_dir, paste0(frame_names, ".png"))
missing <- frame_paths[!file.exists(frame_paths)]
if (length(missing) > 0) {
  stop("Frame(s) nao encontrado(s): ", paste(missing, collapse = ", "))
}

message(">> Montando GIF (", length(frame_paths), " etapas reais)...")
canvas <- magick::geometry_size_pixels(1200, 840)
frames <- unlist(Map(function(path, hold) {
  img <- magick::image_read(path)
  img <- magick::image_resize(img, canvas)
  img <- magick::image_background(img, "white")
  rep(list(img), hold)
}, frame_paths, unname(etapas)), recursive = FALSE)

anim <- magick::image_animate(magick::image_join(frames), fps = 5, loop = 0)

out_dir <- "man/figures"
out_path <- file.path(out_dir, "climasus4r_pipeline.gif")
magick::image_write(anim, out_path)

message(">> GIF salvo em: ", out_path)
message(">> Tamanho: ", round(file.size(out_path) / 1024 / 1024, 2), " MB")
