# Frame 8/8 do GIF do pipeline (final): sus_mod_plot_dlnm(type = "surface", interactive = TRUE)
# Rode sozinho:  Rscript scripts/gif_frames/frame_08_dlnm_superficie3d.R
#
# type = "surface" com interactive = FALSE cai no mesmo mapa de contorno de
# type = "contour" (o 3-D de verdade so existe via plotly). Por isso geramos
# o widget plotly real e tiramos um screenshot estatico com webshot2 -- ainda
# e o output genuino de sus_mod_plot_dlnm(), so que capturado como PNG.
source("scripts/gif_frames/_helpers.R")

fit <- readRDS("dados/mod_dlnm_fit.rds")

p_surface <- sus_mod_plot_dlnm(fit, type = "contour", interactive = FALSE,
                                lang = "pt", verbose = FALSE) 
  plotly::config(displayModeBar = FALSE)

surface_html <- file.path(tempdir(), "dlnm_superficie3d.html")
surface_shot  <- file.path(tempdir(), "dlnm_superficie3d_raw.png")
htmlwidgets::saveWidget(p_surface, surface_html, selfcontained = TRUE)
webshot2::webshot(surface_html, file = surface_shot, vwidth = 1200, vheight = 900, delay = 1)

save_frame(
  c('serie   <- dplyr::inner_join(saude_diaria, clima_diario, by = "date")',
    'for (k in 0:21) df_lags[[paste0("tair_dry_bulb_c_lag", k)]] <- dplyr::lag(serie$tair_dry_bulb_c, k)',
    'fit <- sus_mod_dlnm(',
    '  df_dl, outcome_col = "n_obitos", climate_col = "tair_dry_bulb_c", lag_max = 21,',
    '  argvar = list(fun = "ns", df = 4), arglag = list(fun = "ns", df = 3),',
    '  family = "quasipoisson", dof_per_year = 4',
    ')',
    'sus_mod_plot_dlnm(fit, type = "surface", interactive = TRUE)'),
  surface_shot,
  "08_dlnm_superficie3d"
)
