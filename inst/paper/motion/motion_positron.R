# =============================================================================
# motion_positron.R
# -----------------------------------------------------------------------------
# Gera um motion (MP4, 1920x1080) do pipeline de inst/paper/workflow_paper.R
# com o visual da IDE Positron: o codigo e digitado no editor, executado no
# Console, os objetos aparecem em Variables e os graficos REAIS em Plots.
# No fim mostra todos os tipos de sus_mod_plot_dlnm() (superficie 3D girando)
# e um cartao com os logos do climasus4r e do climasus.
#
# COMO RODAR (a partir da raiz do pacote)
#   Rscript inst/paper/motion/motion_positron.R
#   Na 1a vez roda o pipeline (~2-3 min, usa o cache do sus_data_import()).
#   Depois reaproveita build/motion_data.js; para refazer os graficos:
#   MOTION_REFRESH=1 Rscript inst/paper/motion/motion_positron.R
#   Versao curta (~30 s, com titulo no inicio) para redes sociais:
#   MOTION_SHORT=1 Rscript inst/paper/motion/motion_positron.R
#
# REQUER: chromote, jsonlite, plotly, ggplot2 e ffmpeg no PATH.
# SAIDA:  inst/paper/motion/climasus4r_motion.mp4 (ou climasus4r_motion_30s.mp4)
# =============================================================================

fps       <- 30
dir_m     <- file.path("inst", "paper", "motion")
dir_b     <- file.path(dir_m, "build")
data_js   <- file.path(dir_b, "motion_data.js")
short     <- nzchar(Sys.getenv("MOTION_SHORT"))
out_mp4   <- file.path(dir_m, if (short) "climasus4r_motion_30s.mp4" else "climasus4r_motion.mp4")
src_file  <- file.path("inst", "paper", "workflow_paper.R")

# Graficos do DLNM que nao estao no workflow; "overall" ja esta (fig_3).
# A superficie 3D fica por ultimo (gira na tela).
dlnm_extra <- c(
  "",
  "# Todos os tipos de grafico do DLNM",
  'sus_mod_plot_dlnm(fit_dlnm, type = "lag", lang = "pt", base_size = 16)',
  'sus_mod_plot_dlnm(fit_dlnm, type = "slice", lang = "pt", base_size = 16)',
  'sus_mod_plot_dlnm(fit_dlnm, type = "contour", lang = "pt", base_size = 16)',
  'sus_mod_plot_dlnm(fit_dlnm, type = "distribution", lang = "pt", base_size = 16)',
  'sus_mod_plot_dlnm(fit_dlnm, type = "series", lang = "pt", base_size = 16)',
  'sus_mod_plot_dlnm(fit_dlnm, type = "surface", lang = "pt")'
)

# -----------------------------------------------------------------------------
# 1. Codigo -> passos (um por expressao de topo, com os comentarios acima dela)
# -----------------------------------------------------------------------------
split_steps <- function(lines) {
  exprs <- parse(text = lines, keep.source = TRUE)
  ends  <- vapply(attr(exprs, "srcref"), function(s) s[3], integer(1))
  from  <- c(1L, utils::head(ends, -1) + 1L)
  Map(function(e, a, b) list(expr = e, code = paste0(paste(lines[a:b], collapse = "\n"), "\n")),
      as.list(exprs), from, ends)
}

lines <- readLines(src_file, encoding = "UTF-8")
lines <- lines[!grepl("tibble::view", lines, fixed = TRUE)]  # visualizador de dados fica de fora
while (!nzchar(trimws(lines[length(lines)]))) lines <- lines[-length(lines)]
steps <- split_steps(c(lines, dlnm_extra))

# `fig_N <- ...` so desenha no painel Plots quando impresso: insere `fig_N`
# depois de cada atribuicao que o workflow nao imprime.
assigned <- function(e) if (is.call(e) && identical(e[[1]], as.name("<-"))) as.character(e[[2]]) else NA_character_
printed  <- vapply(steps, function(s) if (is.name(s$expr)) as.character(s$expr) else NA_character_, "")
expanded <- list()
for (s in steps) {
  expanded[[length(expanded) + 1]] <- s
  nm <- assigned(s$expr)
  if (!is.na(nm) && startsWith(nm, "fig_") && !nm %in% printed) {
    expanded[[length(expanded) + 1]] <- list(expr = as.name(nm), code = paste0(nm, "\n"))
  }
}
steps <- expanded

# -----------------------------------------------------------------------------
# 2. Roda o pipeline de verdade e salva graficos/variaveis (com cache)
# -----------------------------------------------------------------------------
if (!file.exists(data_js) || nzchar(Sys.getenv("MOTION_REFRESH"))) {
  suppressMessages(devtools::load_all(quiet = TRUE))
  rlang::check_installed(c("jsonlite", "plotly", "ggplot2"), reason = "para gerar o motion")
  dir.create(file.path(dir_b, "plots"), recursive = TRUE, showWarnings = FALSE)

  describe <- function(x) {
    if (is.data.frame(x)) sprintf("%s [%s x %s]", class(x)[1], format(nrow(x), big.mark = "."), ncol(x))
    else class(x)[1]
  }

  surface <- NULL
  out <- vector("list", length(steps))
  for (i in seq_along(steps)) {
    s <- steps[[i]]
    message(">> [", i, "/", length(steps), "] ", strsplit(trimws(s$code), "\n")[[1]][1])
    res <- withVisible(eval(s$expr, envir = globalenv()))
    step <- list(code = s$code,
                 status = regmatches(s$code, regexpr("sus_[a-z_]+", s$code)))
    if (!length(step$status)) step$status <- NULL
    nm <- assigned(s$expr)
    if (!is.na(nm)) step$var <- list(name = nm, desc = describe(get(nm, envir = globalenv())))
    if (res$visible && inherits(res$value, "plotly")) {
      b <- plotly::plotly_build(res$value)$x
      surface <- list(data = b$data, layout = b$layout)
      step$surface <- TRUE
    } else if (res$visible && inherits(res$value, "ggplot")) {
      png <- file.path("plots", sprintf("plot_%02d.png", i))
      ggplot2::ggsave(file.path(dir_b, png), res$value, width = 12, height = 9.6, dpi = 130, bg = "white")
      step$plot <- file.path("build", png)
    }
    out[[i]] <- step
  }

  motion <- list(
    steps = out, surface = surface,
    r_version = paste0("R ", getRversion()),
    r_banner  = paste0(R.version.string, " -- climasus4r ", utils::packageVersion("climasus4r")),
    logo_hex  = "logo_climasus4r_hex.png",
    logo_word = "logo_climasus_horizontal.png",
    url = "github.com/ByMaxAnjos/climasus4r"
  )
  json <- jsonlite::toJSON(motion, auto_unbox = TRUE, null = "null", na = "null", digits = NA, force = TRUE)
  writeLines(paste0("window.MOTION = ", json, ";"), data_js, useBytes = TRUE)
}

# Ritmo do video (segundos; cps = caracteres digitados/s). Fica fora do cache
# para ajustar sem rodar o pipeline de novo.
timing <- if (short) {
  list(cps = 320, min_type = 0.1, run_delay = 0.15, plot_hold = 1.1, surface_hold = 3.5, other_hold = 0.2, end_hold = 2.5,
       intro = list(hold = 2.6, title = "Do DATASUS ao DLNM em um script R",
                    subtitle = "95% menos tempo de trabalho \u00b7 45\u00d7 mais r\u00e1pido que o fluxo manual"))
} else {
  list(cps = 60, min_type = 0.5, run_delay = 0.35, plot_hold = 2.8, surface_hold = 8, other_hold = 0.7, end_hold = 5)
}
writeLines(paste0("Object.assign(window.MOTION, ", jsonlite::toJSON(timing, auto_unbox = TRUE), ");"),
           file.path(dir_b, "motion_timing.js"))

# -----------------------------------------------------------------------------
# 3. Captura quadro a quadro (Chrome headless) e monta o MP4 (ffmpeg)
# -----------------------------------------------------------------------------
if (!nzchar(Sys.which("ffmpeg"))) stop("ffmpeg nao encontrado no PATH (brew install ffmpeg).")
file.copy(list.files(system.file("htmlwidgets/lib/plotlyjs", package = "plotly"),
                     pattern = "plotly.*min\\.js$", full.names = TRUE)[1],
          file.path(dir_m, "plotly.min.js"), overwrite = TRUE)

frames_dir <- file.path(dir_b, "frames")
unlink(frames_dir, recursive = TRUE)
dir.create(frames_dir, recursive = TRUE)

b <- chromote::ChromoteSession$new(width = 1920, height = 1080)
b$go_to(paste0("file://", normalizePath(file.path(dir_m, "positron.html"))))
js <- function(code) b$Runtime$evaluate(code, awaitPromise = TRUE, returnByValue = TRUE)$result$value
total <- js("init()")
n <- ceiling(total * fps)
message(">> Capturando ", n, " quadros (", round(total, 1), " s)...")
for (i in seq_len(n)) {
  js(sprintf("render(%f)", (i - 1) / fps))
  shot <- b$Page$captureScreenshot(format = "jpeg", quality = 92)
  writeBin(jsonlite::base64_dec(shot$data), file.path(frames_dir, sprintf("%05d.jpg", i)))
  if (i %% 300 == 0) message("   ", i, "/", n)
}
b$close()

system2("ffmpeg", c("-y", "-loglevel", "error", "-framerate", fps,
                    "-i", shQuote(file.path(frames_dir, "%05d.jpg")),
                    "-c:v", "libx264", "-pix_fmt", "yuv420p", "-crf", "18", shQuote(out_mp4)))
unlink(frames_dir, recursive = TRUE)
message(">> Motion salvo em: ", out_mp4, " (", round(file.size(out_mp4) / 1024^2, 1), " MB)")
