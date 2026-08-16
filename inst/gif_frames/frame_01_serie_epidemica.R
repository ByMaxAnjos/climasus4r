# Frame 1/8 do GIF do pipeline: sus_data_plot_aggregate_ts(plot_type = "epidemic")
# Rode sozinho:  Rscript scripts/gif_frames/frame_01_serie_epidemica.R
source("scripts/gif_frames/_helpers.R")

caso_serie <- readRDS("vignettes-pt/dados/caso_serie.rds")

save_frame(
  c('obitos      <- sus_data_import(uf = "SP", year = 2014:2019, system = "SIM-DO")',
    'obitos      <- sus_data_clean_encoding(obitos)',
    'obitos      <- sus_data_standardize(obitos, translate_columns = TRUE, standardize_values = TRUE)',
    'resp        <- sus_data_filter_cid(obitos, disease_group = "respiratory",',
    '                                    match_type = "starts_with")',
    'resp_idosos <- sus_data_filter_demographics(resp, age_range = c(60, Inf), city = RMSP_CIDADES)',
    'obitos_vars <- sus_data_create_variables(resp_idosos, create_age_groups = TRUE,',
    '                                          create_climate_vars = TRUE)',
    'serie       <- sus_data_aggregate(obitos_vars, time_unit = "day", fun = "count")',
    'sus_data_plot_aggregate_ts(serie, value_col = "n_obitos", plot_type = "epidemic")'),
  sus_data_plot_aggregate_ts(caso_serie$serie_diaria, value_col = "n_obitos",
                              plot_type = "epidemic", lang = "pt", verbose = FALSE),
  "01_serie_epidemica"
)
