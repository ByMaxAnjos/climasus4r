# Frame 6/8 do GIF do pipeline: sus_mod_plot_dlnm(type = "overall")
# Rode sozinho:  Rscript scripts/gif_frames/frame_06_dlnm_rr_geral.R
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
    'sus_mod_plot_dlnm(fit, type = "overall")'),
  sus_mod_plot_dlnm(fit, type = "overall", lang = "pt", verbose = FALSE),
  "06_dlnm_rr_geral"
)
