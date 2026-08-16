# Frame 3/8 do GIF do pipeline: sus_climate_plot_aggregate(plot_type = "distribution")
# Rode sozinho:  Rscript scripts/gif_frames/frame_03_clima_distribuicao.R
source("scripts/gif_frames/_helpers.R")

clima <- readRDS("vignettes-pt/dados/cw_daily_A801.rds") |> dplyr::rename(date = date_day)

save_frame(
  c('df_inmet  <- sus_climate_inmet(years = 2020:2023, uf = "RS", use_cache = TRUE)',
    'df_filled <- sus_climate_fill_inmet(df_inmet, target_var = "tair_dry_bulb_c")',
    'df_ind    <- sus_climate_compute_indicators(df_filled, indicators = "all", region = "auto")',
    'cw_all    <- sus_climate_compute_coldwaves(df_ind, method = "all", percentile = 10)',
    'sus_climate_plot_aggregate(cw_all$daily, climate_cols = c("tmax","tmin","tmean"),',
    '                            plot_type = "distribution")'),
  sus_climate_plot_aggregate(clima, climate_cols = c("tmax", "tmin", "tmean"),
                              plot_type = "distribution", lang = "pt", verbose = FALSE),
  "03_clima_distribuicao"
)
