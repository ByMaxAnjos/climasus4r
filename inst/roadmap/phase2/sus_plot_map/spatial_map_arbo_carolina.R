library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)
library(geobr)        # Brazilian shapefiles (IBGE)
library(sf)           # spatial operations
library(ggrepel)      # capital city labels
library(climasus4r)

# ── 1. CARREGAMENTO DE DADOS BASE (Rodar apenas uma vez) ──────────────────────

# 1.1 População
pop25 <- read.dbc::read.dbc("/Users/maxanjos/Downloads/POPTBR25.dbc") %>% 
  mutate(pop_25 = POPULACAO) %>% 
  select(-ANO, -POPULACAO)

# 1.2 Municípios (Coordenadas e RGI)
muni <- data.table::fread("https://raw.githubusercontent.com/mapaslivres/municipios-br/refs/heads/main/tabelas/municipios.csv",
                          encoding = "UTF-8", showProgress = FALSE)
muni_prep <- muni %>%
  mutate(
    rgi = as.character(rgi),
    code6 = substr(as.character(municipio), 1, 6)
  )

# 1.3 Shapefiles (Geometrias)
amazon_states <- c("AC","AM","AP","MA","MT","PA","RO","RR","TO")

amazon_states <- "RJ"
# Download dinâmico do IBGE (Malha 2024)
url <- "https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2024/Brasil/BR_UF_2024.zip"
temp_zip <- tempfile(fileext = ".zip")
temp_dir <- tempdir()
download.file(url, destfile = temp_zip, mode = "wb", quiet = TRUE)
unzip(temp_zip, exdir = temp_dir)
shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)

states_sf <- sf::st_read(shp_file[1], options = "ENCODING=UTF-8", quiet = TRUE) %>% 
  rename(abbrev_state = SIGLA_UF) %>% 
  filter(abbrev_state %in% amazon_states)

# Centróides Projetados (Evita avisos geográficos do sf)
state_centroids <- states_sf %>%
  sf::st_transform(crs = 5880) %>%
  sf::st_point_on_surface() %>%
  sf::st_transform(crs = 4326) %>%
  dplyr::mutate(
    lon_lbl = sf::st_coordinates(.)[, 1],
    lat_lbl = sf::st_coordinates(.)[, 2]
  ) %>%
  sf::st_drop_geometry() %>%
  dplyr::select(abbrev_state, lon_lbl, lat_lbl)

# Paleta de cores para os mapas
incidence_palette <- c("#2F4F8F", "#5D739F", "#F4D19B", "#F9AC4A", "#E65100")


# ── 2. FUNÇÃO GERADORA DE PAINÉIS ─────────────────────────────────────────────

# Esta função fará o download, limpeza e plotagem dinâmica para qualquer doença
create_disease_panel <- function(sys_name, disease_title, panel_letter) {
  
  message(paste("Processing data for:", disease_title))
  
  # 2.1 Importar e limpar dados
  df_raw <- sus_data_import(year = 2025, uf = NULL, region = "amazonia", system = sys_name)
  
  df_stand <- df_raw %>% 
    sus_data_clean_encoding(lang = "en") %>% 
    sus_data_standardize(lang="en") %>% 
    sus_data_create_variables(lang="en") %>% 
    sus_data_filter_demographics(sex = c("Male", "Female"))
  
  df_agg <- sus_data_aggregate(df_stand)
  
  # 2.2 Joins e Cálculos
  df_analysis <- df_agg %>%
    mutate(notification_municipality_code = as.character(notification_municipality_code)) %>%
    left_join(muni_prep, by = c("notification_municipality_code" = "code6")) %>%
    mutate(municipio = as.factor(municipio)) %>% 
    left_join(pop25, by = c("municipio" = "MUNIC_RES")) %>%
    mutate(
      incidencia = (n_casos / pop_25) * 100000,
      uf_label = paste0("State: ", uf_code)
    )
  
  # 2.3 Sumarização Municipal
  df_summary <- df_analysis %>%
    group_by(notification_municipality_code, name, uf_code, lon, lat, pop_25, is_capital) %>%
    summarise(
      total_casos      = sum(n_casos, na.rm = TRUE),
      mean_incidencia  = mean(incidencia[incidencia > 0], na.rm = TRUE),
      years_with_cases = n_distinct(format(date[n_casos > 0], "%Y")),
      .groups          = "drop"
    ) %>%
    mutate(
      mean_incidencia = ifelse(is.nan(mean_incidencia) | is.na(mean_incidencia), 0, mean_incidencia)
    )
  
  # Capitais para Labels
  capitals <- df_summary %>%
    filter(is_capital == TRUE | name %in% c("Manaus", "Belém", "Porto Velho", "Macapá", "Boa Vista", "Rio Branco", "Palmas", "São Luís", "Cuiabá")) %>%
    group_by(uf_code) %>%
    slice_max(total_casos, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  study_sites <- tibble::tribble(
    ~site, ~code, ~state, ~lat, ~lon,
    "Parque Estadual Chandless", "PEC", "AC", -9.358425, -69.926583,
    "Parque Zoobotânico UFAC", "PZ-UFAC", "AC", -9.961136, -67.865661,
    "Horto Florestal Rio Branco", "HFRB", "AC", -9.944578, -67.829364,
    "ESEC de Cuniã", "ESEC-CUNIA", "RO", -8.045672, -63.486600,
    "Campus UNIR Porto Velho", "UNIR", "RO", -8.833900, -63.933100
  )
  
  # 2.4 Geração do Plot (Padrão Lancet/Science)
  p <- ggplot() +
    geom_sf(data = states_sf, fill = "#F0EDE8", color = "#7A8C99", linewidth = 0.55) +
    # Como as escalas dinâmicas já têm trans = "log1p", repassamos os dados brutos na estética aes()
    geom_point(
      data = df_summary %>% arrange(desc(total_casos)),
      aes(x = lon, y = lat, size = total_casos, color = mean_incidencia),
      alpha = 0.75, shape = 16
    ) +
    geom_label_repel(
      data = capitals, aes(x = lon, y = lat, label = name),
      size = 2.4, fontface = "bold", color = "#1A252F", fill = alpha("white", 0.82),
      label.padding = unit(0.18, "lines"), label.size = 0.25, box.padding = 0.6,
      segment.color = "#1A252F", segment.size = 0.3, max.overlaps = 15, na.rm = TRUE
    ) +
    geom_text(
      data = state_centroids, aes(x = lon_lbl, y = lat_lbl, label = abbrev_state),
      size = 2.8, color = "#4A5A66", fontface = "italic", alpha = 0.6
    ) +
    scale_color_gradientn(
      colors  = incidence_palette,
      name    = expression(atop("Mean Incidence", "(per 100k, log"[1+x]*" scale)")),
      trans   = "log1p", 
      breaks  = scales::breaks_extended(n = 6),
      labels  = scales::label_comma(),
      guide   = guide_colorbar(
        title.position = "top", barwidth = unit(10, "lines"), barheight = unit(0.55, "lines"), ticks.linewidth = 0.4
      )
    ) +
    scale_size_continuous(
      name   = "Cumulative Cases\n(log scale)",
      range  = c(0.5, 9),
      trans  = "log1p",
      breaks = scales::breaks_log(n = 5),
      labels = scales::label_log(),
      guide  = guide_legend(
        title.position = "top", override.aes = list(alpha = 0.8, color = "#FD8D3C")
      )
    ) +
    labs(title = paste(panel_letter, " ", disease_title, " Burden (2025)", sep="")) +
    theme_void(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", size = 11, color = "#1A252F", margin = margin(b = 3)),
      legend.position  = "bottom",
      legend.direction = "horizontal",
      legend.box       = "horizontal",
      legend.title     = element_text(size = 7.5, face = "bold", color = "grey25"),
      legend.text      = element_text(size = 7, color = "grey30"),
      legend.spacing.x = unit(0.8, "lines"),
      plot.margin      = margin(8, 8, 4, 8)
    )
  
  return(p)
}


# ── 3. GERANDO OS PAINÉIS E COMBINANDO COM PATCHWORK ──────────────────────────

# NOTA: Verifique se os nomes dos sistemas ("SINAN-CHIKUNGUNYA", etc.) 
# correspondem exatamente à nomenclatura aceita pelo seu pacote climasus4r.
panel_A <- create_disease_panel("SINAN-DENGUE", "Dengue", "A")
panel_B <- create_disease_panel("SINAN-CHIKUNGUNYA", "Chikungunya", "B")
panel_C <- create_disease_panel("SINAN-ZIKA", "Zika", "C")
#panel_D <- create_disease_panel("SINAN-FEBRE-AMARELA", "Yellow Fever", "D")

# O Patchwork unifica as legendas automaticamente para publicações!
final_plot <- (panel_A | panel_B) / 
  (panel_C | panel_D) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# Salvar em alta resolução para periódico (TIFF ou PDF)
ggsave("arbovirus_burden_2025.pdf", plot = final_plot, width = 12, height = 10, dpi = 300)




