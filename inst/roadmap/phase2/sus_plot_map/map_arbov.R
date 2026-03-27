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
#incidence_palette <- c("#2F4F8F", "#5D739F", "#F4D19B", "#F9AC4A", "#E65100")
# # Paleta "Lancet Burden" (Amarelo -> Laranja -> Vermelho -> Bordô Escuro)
incidence_palette <- c("#FFEDA0", "#FEB24C", "#FC4E2A", "#E31A1C", "#800026")
# 
# # Paleta "Science Plasma" (Amarelo vibrante -> Laranja -> Rosa Choque -> Roxo Escuro)
# incidence_palette <- c("#F0F921", "#F89540", "#CC4678", "#7E03A8", "#0D0887")
# 
# 
# # Paleta "Ice to Fire" (Azul Profundo -> Azul Claro -> Amarelo -> Laranja -> Vermelho Escuro)
# incidence_palette <- c("#313695", "#74ADD1", "#FFFFBF", "#F46D43", "#A50026")


#Amazon study areas
study_sites <- tibble::tribble(
  ~site, ~code, ~state, ~lat, ~lon,
  "Parque Estadual Chandless", "PEC", "AC", -9.358425, -69.926583,
  "Parque Zoobotânico UFAC", "PZ-UFAC", "AC", -9.961136, -67.865661,
  "Horto Florestal Rio Branco", "HFRB", "AC", -9.944578, -67.829364,
  "ESEC de Cuniã", "ESEC-CUNIA", "RO", -8.045672, -63.486600,
  "Campus UNIR Porto Velho", "UNIR", "RO", -8.833900, -63.933100
) %>% 
  dplyr::mutate(
    label = dplyr::case_when(
      code == "PEC" ~ "Chandless State Park (PEC)",
      code == "PZ-UFAC" ~ "Zoobotanical Park (UFAC)",
      code == "HFRB" ~ "Horto Florestal de Rio Branco",
      code == "ESEC-CUNIA" ~ "ESEC de Cuniã",
      code == "UNIR" ~ "UNIR Campus"
    )
  )

 
study_sites_rj <- tibble::tribble(
  ~site, ~code, ~state, ~lat, ~lon,
  "Fiocruz Atlantic Forest Biological Station", "EFMA", "RJ", -22.933833, -43.400306
) %>%
  dplyr::mutate(
    label = "Fiocruz Atlantic Forest Biological Station (EFMA)"
  )

# ── 2. FUNÇÃO GERADORA DE PAINÉIS ─────────────────────────────────────────────

# Esta função fará o download, limpeza e plotagem dinâmica para qualquer doença
create_disease_panel <- function(sys_name, panel_letter) {
  
  # 2.1 Importar e limpar dados
  df_raw <- sus_data_import(year = 2025, uf = amazon_states, region = NULL, system = sys_name)
  
  df_stand <- df_raw %>% 
    sus_data_clean_encoding(lang = "en") %>% 
    sus_data_standardize(lang="en") %>% 
    sus_create_variables(lang="en") %>% 
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
    ) %>% filter(mean_incidencia > 0)
  
  # Capitais para Labels
  capitals <- df_summary %>%
    filter(is_capital == TRUE | name %in% c("Manaus", "Belém", "Porto Velho", "Macapá", "Boa Vista", "Rio Branco", "Palmas", "São Luís", "Cuiabá")) %>%
    group_by(uf_code) %>%
    slice_max(total_casos, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  # 2.4 Geração do Plot (Padrão Lancet/Science)
  p <- ggplot() +
    geom_sf(data = states_sf, fill = "#F0EDE8", color = "#7A8C99", linewidth = 0.55) +
    
    # 1. Mudança para shape = 21 (preenchimento + borda)
    # 2. Uso do dado bruto no fill (sem log1p manual)
    geom_point(
      data = df_summary %>% arrange(desc(total_casos)),
      aes(x = lon, y = lat, size = total_casos, fill = mean_incidencia),
      shape = 21,           # Círculo com preenchimento e borda
      color = "white",      # Cor da borda
      stroke = 0.25,        # Espessura da borda (bem fina)
      alpha = 0.85          # Menos transparência já que a borda separa os pontos
    ) +
    
    geom_label_repel(
      data = capitals, aes(x = lon, y = lat, label = name),
      size = 1.4, fontface = "bold", color = "#1A252F", fill = alpha("white", 0.82),
      label.padding = unit(0.18, "lines"), label.size = 0.25, box.padding = 0.6,
      segment.color = "#1A252F", segment.size = 0.3, max.overlaps = 15, na.rm = TRUE
    ) +
    geom_text(
      data = state_centroids, aes(x = lon_lbl, y = lat_lbl, label = abbrev_state),
      size = 2.8, color = "#4A5A66", fontface = "italic", alpha = 0.6
    ) +
    
    # 3. Alterado de scale_color para scale_fill_gradientn (por causa do shape = 21)
    scale_fill_gradientn(
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
      range  = c(0.5, 5), # 4. AQUI É ONDE DIMINUIMOS O TAMANHO DOS CÍRCULOS
      trans  = "log1p",
      breaks = scales::breaks_log(n = 5),
      labels = scales::label_log(),
      guide  = guide_legend(
        title.position = "top", 
        # Ajustamos o override para refletir o shape 21 na legenda
        override.aes = list(shape = 21, fill = "#FD8D3C", color = "white", stroke = 0.25, alpha = 0.85)
      )
    ) +
    # Study sites
    geom_point(
      data = study_sites_rj,
      aes(x = lon, y = lat),
      shape = 24,
      size = 4.5,
      fill = "#2C7FB8",
      color = "black",
      stroke = 0.8
    ) +
    
    geom_label_repel(
      data = study_sites_rj,
      aes(x = lon, y = lat, label = label),
      size = 2.7,
      fontface = "bold",
      color = "#1A252F",
      fill = alpha("white", 0.9),
      label.padding = unit(0.2, "lines"),
      label.size = 0.25,
      box.padding = 0.7,
      segment.color = "grey30",
      segment.size = 0.35,
      max.overlaps = 20
    ) +
    labs(title = paste(panel_letter, " ", sep="")) +
    theme_void(base_size = 12) +
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
panel_A_rj <- create_disease_panel("SINAN-DENGUE", "A - Dengue")
panel_B_rj <- create_disease_panel("SINAN-CHIKUNGUNYA", "B - Chikungunya")
panel_C_rj <- create_disease_panel("SINAN-ZIKA", "C - Zika")
#panel_D <- create_disease_panel("SINAN-FEBRE-AMARELA", "Yellow Fever", "D")

# O Patchwork unifica as legendas automaticamente para publicações!


final_plot <- (panel_A_rj + panel_B_rj + panel_C_rj) + 
  plot_layout(guides = "collect") + # Une as legendas para ganhar espaço lateral
  plot_annotation(
    #title = "Cumulative Burden and Mean Incidence Rate of Major Arboviruses \nin the Brazilian Amazon (2025)",
    title = "Cumulative Burden and Mean Incidence Rate of Major Arboviruses \nin the Rio de Janeiro State (2025)",
    #subtitle = "Bubble size: total cases (log scale) \u2022 Colour: mean daily incidence per 100,000 (log scale)",
    #subtitle = "Bubble size: total cases (log scale) • Colour: mean daily incidence per 100,000 (log scale)\nHighlighted symbols indicate ecological study sites in Acre and Rondônia.",
    subtitle = "Bubble size: total cases (log scale) • Colour: mean daily incidence per 100,000 (log scale)\nHighlighted symbols indicate ecological study site in Rio de Janeiro State.",
    # caption = paste0(
    #   "Data: Notifiable Diseases Information System (SINAN) / Brazilian Ministry of Health / DATASUS. ",
    #   "Population: IBGE 2025 estimates. ",
    #   #"Amazon Legal region states: AC, AM, AP, MA, MT, PA, RO, RR, TO.\n",
    #   "Shapefiles: IBGE 2020.\n",
    #   "Incidence = (cases / population) \u00d7 100,000. ",
    #   "Y-axis and fill use log(1+x) transformation to accommodate zero-inflated distributions.\n",
    #   "Study sites highlighted on the map represent forest ecosystems used for biodiversity monitoring in the western Amazon: Parque Estadual Chandless (PEC),\n",
    #   "Parque Zoobotânico da Universidade Federal do Acre (PZ-UFAC),Horto Florestal de Rio Branco (HFRB), Estação Ecológica de Cuniã (ESEC-Cuniã),\n",
    #   "and the Universidade Federal de Rondônia campus (UNIR).\n"
    # ),
    caption = paste0(
      "Data: Notifiable Diseases Information System (SINAN) / Brazilian Ministry of Health / DATASUS. ",
      "Population: IBGE 2025 estimates. ",
      #"Amazon Legal region states: AC, AM, AP, MA, MT, PA, RO, RR, TO.\n",
      "Shapefiles: IBGE 2020.\n",
      "Incidence = (cases / population) \u00d7 100,000. ",
      "Y-axis and fill use log(1+x) transformation to accommodate zero-inflated distributions.\n",
      "Study site highlighted on the map represent Fiocruz Atlantic Forest Biological Station in the Rio de Janeiro state"
    ),
    theme = theme(
      plot.title    = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 14, color = "grey30", hjust = 0.5, margin = margin(b = 15))
    )
  ) & 
  theme(
    plot.caption = element_text(
      size       = 10,
      color      = "grey45",
      hjust      = 0,
      lineheight = 1.35,
      margin     = margin(t = 6)
    )
  ) &
  theme(
    plot.margin = margin(2, 2, 2, 2, "mm"),
    legend.position = "bottom",
    legend.margin = margin(t = -5) # Aproxima a legenda dos mapas
  )


ggsave("arbovirus_burden_rio_study_area_2025.png", 
       plot = final_plot, 
       width = 31.7, 
       height = 21, 
       units = "cm", 
       dpi = 300,
       scale = 1) # Ajuste para 0.8 ou 0.9 se precisar de mais espaço para o mapa

