# Frame 4/8 do GIF do pipeline: sus_mod_plot_dlnm(type = "distribution")
# Rode sozinho:  Rscript scripts/gif_frames/frame_04_dlnm_distribuicao.R
source("scripts/gif_frames/_helpers.R")

fit <- readRDS("dados/mod_dlnm_fit.rds")

save_frame(
  c('serie   <- dplyr::inner_join(saude_diaria, clima_diario, by = "date")',
    'for (k in 0:21) df_lags[[paste0("tair_dry_bulb_c_lag", k)]] <- dplyr::lag(serie$tair_dry_bulb_c, k)',
    'fit <- sus_mod_dlnm(',
    '  df_dl, outcome_col = "n_obitos", climate_col = "tair_dry_bulb_c", lag_max = 21,',
    '  argvar = list(fun = "ns", df = 4), arglag = list(fun = "ns", df = 3),',
    '  family = "quasipoisson", dof_per_year = 4',
    ')',
    'sus_mod_plot_dlnm(fit, type = "distribution")'),
  sus_mod_plot_dlnm(fit, type = "distribution", lang = "pt", verbose = FALSE),
  "04_dlnm_distribuicao"
)
