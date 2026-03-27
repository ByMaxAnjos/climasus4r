# =============================================================================
# SPATIAL MAP — DENGUE INCIDENCE, BRAZILIAN AMAZON (2014–2021)
# Publication-quality figure for scientific journal
#
# SPATIAL REPRESENTATION STRATEGY:
#
# Three options were considered:
#
# 1. Choropleth polygon map (geobr polygons filled by incidence)
#    Best for: administrative comparisons, policy audiences
#    Problem: Large municipalities (AM, PA) visually dominate; tiny coastal
#    municipalities in MA are invisible. Area ≠ burden. Not recommended when
#    municipality sizes vary by orders of magnitude (Amazon context).
#
# 2. Proportional symbol map (circles scaled by total cases, colour = incidence)
#    Best for: communicating BOTH burden (size) and risk (colour) simultaneously
#    Epidemiological justification: separates absolute burden (relevant for
#    health system planning) from population-adjusted risk (relevant for
#    disease ecology). Standard in Lancet ID, PLOS NTDs, Emerging Inf Diseases.
#    SELECTED as primary map.
#
# 3. Kernel density / continuous surface (interpolated raster)
#    Best for: continuous ecological processes (malaria, leishmaniasis)
#    Problem: Dengue is strongly urban and tied to Aedes aegypti habitat.
#    Interpolating across the Amazon forest creates spurious "risk" in
#    uninhabited areas. Not appropriate here.
#
# FINAL DESIGN:
#   - Panel A: Proportional bubble map (mean annual incidence as colour,
#              total cumulative cases 2014–2021 as bubble size)
#   - Panel B: Small multiples — annual mean incidence choropleth faceted
#              by year, showing epidemic wave progression
#   Combined with patchwork for a two-panel journal figure.
# =============================================================================

library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)
library(geobr)        # Brazilian shapefiles (IBGE)
library(sf)           # spatial operations
library(ggrepel)      # capital city labels
library(climasus4r)


# DATAUSUS - CLIMASUS4R ---------------------------------------------------
zika_25 <- sus_data_import(year = 2025, 
                             uf = NULL,
                             region = "amazonia",
                             system = "SINAN-ZIKA")
df_stand <- zika_25 %>% 
  sus_data_clean_encoding(lang = "en") %>% 
  sus_data_standardize(lang="en")

df_amazon_create <- df_stand %>% 
  sus_create_variables(lang="en")

df_amazon_demo <- df_amazon_create %>% 
  sus_data_filter_demographics(sex = c("Male", "Female"))

#Aggregate daily notifications
df_amazon_agg <- sus_data_aggregate(df_amazon_demo)



# GET MUNI ----------------------------------------------------------------

muni <- data.table::fread("https://raw.githubusercontent.com/mapaslivres/municipios-br/refs/heads/main/tabelas/municipios.csv",
                          encoding = "UTF-8", showProgress = FALSE)
saveRDS(muni, "municipios_met.rds")

muni$rgi <- as.character(muni$rgi)

muni_prep <- muni %>%
  mutate(
    # Converte para caractere, pega os 6 primeiros e salva como chave
    code6 = substr(as.character(municipio), 1, 6)
  )

# Join with muni  ---------------------------------------------------------
df_amazon_prep <- df_amazon_agg %>%
  mutate(
    notification_municipality_code = as.character(notification_municipality_code)
  )

df_sinan <- df_amazon_prep %>%
  left_join(muni_prep, by = c("notification_municipality_code" = "code6"))



# Join with pop25 ---------------------------------------------------------

pop25 <- read.dbc::read.dbc("/Users/maxanjos/Downloads/POPTBR25.dbc") %>% 
  mutate(pop_25 = POPULACAO) %>% 
  select(-ANO, -POPULACAO)
saveRDS(pop25, "pop25.rds")
df_sinan <- df_sinan %>%
  mutate(municipio = as.factor(municipio)) %>% 
  left_join(pop25, by = c("municipio" = "MUNIC_RES"))


df_analysis <- df_sinan %>%
  mutate(
    # Cálculo da incidência por 100k habitantes (essencial para comparar cidades de tamanhos diferentes)
    incidencia = (n_casos / pop_25) * 100000,
    # Criando labels amigáveis para os Estados
    uf_label = paste0("State: ", uf_code)
  )
glimpse(df_analysis)

# ── 0. Aggregate to municipality level ────────────────────────────────────────
# One row per municipality: mean annual incidence + cumulative cases
df_muni_summary <- df_analysis %>%
  group_by(
    notification_municipality_code,
    name, uf_code, lon, lat, pop_25, is_capital
  ) %>%
  summarise(
    total_casos        = sum(n_casos, na.rm = TRUE),
    mean_incidencia    = mean(incidencia[incidencia > 0], na.rm = TRUE),  # mean of non-zero days
    max_incidencia     = max(incidencia, na.rm = TRUE),
    years_with_cases   = n_distinct(format(date[n_casos > 0], "%Y")),
    .groups            = "drop"
  ) %>%
  # Replace NaN (municipalities with zero cases throughout) with 0
  mutate(
    mean_incidencia = ifelse(is.nan(mean_incidencia) | is.na(mean_incidencia), 0, mean_incidencia),
    # log1p for bubble sizing (avoids giant Manaus bubble dominating)
    log_total       = log1p(total_casos)
  )

# Annual summary for small-multiples panel
df_annual <- df_analysis %>%
  mutate(year = format(date, "%Y")) %>%
  group_by(
    notification_municipality_code,
    name, uf_code, lon, lat, year
  ) %>%
  summarise(
    annual_incidencia = mean(incidencia[incidencia > 0], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    annual_incidencia = ifelse(is.nan(annual_incidencia) | is.na(annual_incidencia),
                               0, annual_incidencia)
  )

# ── 1. Load shapefiles ────────────────────────────────────────────────────────
# States in the Amazon Legal region
amazon_states <- c("AC","AM","AP","MA","MT","PA","RO","RR","TO")

message("Loading shapefiles from geobr...")


# State borders — loaded in geographic WGS84 (EPSG:4326) by geobr
options(timeout = 600)
# states_sf <- get_spatial_munic_cache(level ="state", verbose = T) %>%
#   filter(abbrev_state %in% amazon_states)

url <- "https://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2024/Brasil/BR_UF_2024.zip"
temp_zip <- tempfile(fileext = ".zip")
temp_dir <- tempdir()
download.file(url, destfile = temp_zip, mode = "wb")
unzip(temp_zip, exdir = temp_dir)
shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
states_sf <- sf::st_read(shp_file[1], options = "ENCODING=UTF-8") %>% 
  rename(abbrev_state = SIGLA_UF) %>% 
  filter(abbrev_state %in% amazon_states)


# Municipality polygons (for annual choropleth)
muni_sf <- get_spatial_munic_cache(level ="munic", verbose = T)  %>%
  filter(abbrev_state %in% amazon_states) %>%
  mutate(code_muni_6 = substr(as.character(code_muni), 1, 6))

# ── FIX: compute state label centroids in a planar CRS ───────────────────────
# geom_sf_text() internally calls st_point_on_surface() which warns when the
# CRS is geographic (degrees). Solution: project to SIRGAS 2000 / Brazil
# Polyconic (EPSG:5880) — the official IBGE projected CRS — compute centroids
# there, then extract coordinates and back-transform to WGS84 for plotting.
# This eliminates the warning entirely and produces geometrically correct
# label positions (important for concave polygons like Amazonas and Pará).

states_projected <- sf::st_transform(states_sf, crs = 5880)

state_centroids <- states_projected %>%
  sf::st_point_on_surface() %>%          # correct centroid on planar geometry
  sf::st_transform(crs = 4326) %>%       # back to WGS84 for ggplot2 4674
  dplyr::mutate(
    lon_lbl = sf::st_coordinates(.)[, 1],
    lat_lbl = sf::st_coordinates(.)[, 2]
  ) %>%
  sf::st_drop_geometry() %>%             # plain data frame — no sf needed for geom_text
  dplyr::select(abbrev_state, lon_lbl, lat_lbl)

# Join annual data to polygons
df_annual_sf <- muni_sf %>%
  left_join(
    df_annual %>%
      mutate(muni_6 = substr(notification_municipality_code, 1, 6)),
    by = c("code_muni_6" = "muni_6")
  )

# ── 2. Capital cities for labelling ───────────────────────────────────────────
capitals <- df_muni_summary %>%
  filter(is_capital == TRUE | name %in% c(
    "Manaus", "Belém", "Porto Velho", "Macapá", "Boa Vista",
    "Rio Branco", "Palmas", "São Luís", "Cuiabá"
  )) %>%
  group_by(uf_code) %>%
  slice_max(total_casos, n = 1) %>%
  ungroup()

# ── 3. COLOR PALETTE — sequential, print-safe, colorblind-friendly ────────────
# YlOrRd from ColorBrewer: universally recognized for disease burden maps
# Used in WHO, PAHO, and Lancet ID cartography
# incidence_palette <- c(
#   "#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C",
#   "#FC4E2A", "#E31A1C", "#B10026"
# )

incidence_palette <- c(
  "#2F4F8F",  # azul profundo
  "#5D739F",  # azul médio
  "#F4D19B",  # dourado claro
  "#F9AC4A",  # laranja vibrante
  "#E65100"   # laranja intenso
)

# ── 4. PANEL A — Proportional bubble map (overall 2014–2021) ─────────────────
panel_a <- ggplot() +
  
  # State polygon background (subtle fill, clear borders)
  geom_sf(
    data      = states_sf,
    fill      = "#F0EDE8",
    color     = "#7A8C99",
    linewidth = 0.55
  ) +
  
  # Proportional bubbles:
  # SIZE  = log1p(total_casos) → visual hierarchy of absolute burden
  # COLOR = mean_incidencia    → population-adjusted epidemic risk
  # Sorted so small bubbles plot on top of large ones (prevents occlusion)
  geom_point(
    data = df_muni_summary %>% arrange(desc(log_total)),
    aes(
      x     = lon,
      y     = lat,
      size  = log_total,
      color = log1p(mean_incidencia)
    ),
    alpha = 0.75,
    shape = 16     # filled circle, no border artefact
  ) +
  
  # Capital city labels
  geom_label_repel(
    data          = capitals,
    aes(x = lon, y = lat, label = name),
    size          = 2.4,
    fontface      = "bold",
    color         = "#1A252F",
    fill          = alpha("white", 0.82),
    label.padding = unit(0.18, "lines"),
    label.size    = 0.25,
    box.padding   = 0.6,
    segment.color = "#1A252F",
    segment.size  = 0.3,
    max.overlaps  = 15,
    na.rm         = TRUE
  ) +
  
  # State abbreviation labels — precomputed planar centroids (no sf warning)
  geom_text(
    data     = state_centroids,
    aes(x = lon_lbl, y = lat_lbl, label = abbrev_state),
    size     = 2.8,
    color    = "#4A5A66",
    fontface = "italic",
    alpha    = 0.6
  ) +
  
  # Color scale: log1p mean incidence with back-transformed breaks
  scale_color_gradientn(
    colors  = incidence_palette,
    name    = expression(atop("Mean Incidence", "(per 100k, log"[1+x]*" scale)")),
    # trans = "log1p" faz a transformação internamente nos dados
    trans   = "log1p", 
    # breaks = breaks_extended() cria quebras bonitas dinamicamente
    breaks  = scales::breaks_extended(n = 6),
    # labels = label_comma() garante que 1000 apareça como 1,000
    labels  = scales::label_comma(),
    guide   = guide_colorbar(
      title.position = "top",
      barwidth        = unit(10, "lines"),
      barheight       = unit(0.55, "lines"),
      ticks.linewidth = 0.4
    )
  ) +
  
  scale_size_continuous(
    name   = "Cumulative Cases\n(log scale)",
    range  = c(0.5, 9),
    # trans = "log1p" transforma o tamanho e permite que as quebras 
    # e labels sejam calculados nos valores originais (back-transformed)
    trans  = "log1p",
    breaks = scales::breaks_log(n = 5),
    labels = scales::label_log(),
    guide  = guide_legend(
      title.position = "top",
      override.aes   = list(alpha = 0.8, color = "#FD8D3C")
    )
  ) +
  
  # coord_sf(
  #   xlim = c(-74, -44),
  #   ylim = c(-18,   6),
  #   expand = FALSE
  # ) +
  
  labs(
    title    = "A  Cumulative Dengue Burden and Mean Incidence Rate (2025)",
    subtitle = "Bubble size: total cases (log scale) \u2022 Colour: mean daily incidence per 100,000 (log scale)"
  ) +
  
  theme_void(base_size = 10) +
  theme(
    plot.title       = element_text(face = "bold", size = 10.5, color = "#1A252F",
                                    margin = margin(b = 3)),
    plot.subtitle    = element_text(size = 8, color = "grey40", margin = margin(b = 6)),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.box       = "horizontal",
    legend.title     = element_text(size = 7.5, face = "bold", color = "grey25"),
    legend.text      = element_text(size = 7, color = "grey30"),
    legend.spacing.x = unit(0.8, "lines"),
    plot.margin      = margin(8, 8, 4, 8)
  )







# ── 5. PANEL B — Annual small-multiples choropleth ────────────────────────────
panel_b <- ggplot() +
  
  geom_sf(
    data      = df_annual_sf,
    aes(fill  = log1p(annual_incidencia)),
    color     = NA,          # no polygon borders — too dense at municipality level
    na.rm     = TRUE
  ) +
  
  geom_sf(
    data      = states_sf,
    fill      = NA,
    color     = "grey65",
    linewidth = 0.35
  ) +
  
  #facet_wrap(~ year, ncol = 4) +
  
  scale_fill_gradientn(
    colors   = incidence_palette,
    name     = expression("Mean Incidence per 100k (log"[1+x]*")"),
    # Define a transformação log(1+x) diretamente na escala
    trans    = "log1p",
    # Gera quebras "bonitas" e dinâmicas (ex: 0, 100, 500, 1000...)
    breaks   = scales::breaks_extended(n = 5),
    # Formata os números com vírgula para milhares
    labels   = scales::label_comma(),
    na.value = "#E8E4DF",
    guide    = guide_colorbar(
      title.position  = "top",
      barwidth        = unit(12, "lines"),
      barheight       = unit(0.5, "lines"),
      ticks.linewidth = 0.4
    )
  ) +
  
  # coord_sf(
  #   xlim   = c(-74, -44),
  #   ylim   = c(-18,   6),
  #   expand = FALSE
  # ) +
  
  labs(
    title    = "B  Annual Progression of Dengue Incidence by Municipality",
    subtitle = "Each panel shows the mean daily incidence rate per 100,000 inhabitants for that year"
  ) +
  
  theme_void(base_size = 9) +
  theme(
    plot.title      = element_text(face = "bold", size = 10.5, color = "#1A252F",
                                   margin = margin(b = 3)),
    plot.subtitle   = element_text(size = 8, color = "grey40", margin = margin(b = 6)),
    strip.text      = element_text(face = "bold", size = 8.5, color = "white",
                                   margin = margin(2.5, 4, 2.5, 4)),
    strip.background = element_rect(fill = "#1C2B3A", color = NA),
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.title     = element_text(size = 7.5, face = "bold", color = "grey25"),
    legend.text      = element_text(size = 7, color = "grey30"),
    panel.spacing    = unit(0.3, "lines"),
    plot.margin      = margin(8, 8, 4, 8)
  )

# ── 6. ASSEMBLE with patchwork ────────────────────────────────────────────────
map_combined <- panel_a / panel_b +
  plot_layout(heights = c(1.1, 1)) +
  plot_annotation(
    caption = paste0(
      "Data: SINAN-DENGUE, Brazilian Ministry of Health / DATASUS. ",
      "Population: IBGE 2025 estimates. ",
      "Amazon Legal region states: AC, AM, AP, MA, MT, PA, RO, RR, TO.\n",
      "Shapefiles: IBGE 2020 via geobr (Pereira et al., 2021). ",
      "Incidence = (cases / population) \u00d7 100,000. ",
      "Y-axis and fill use log(1+x) transformation to accommodate zero-inflated distributions."
    ),
    theme = theme(
      plot.caption = element_text(
        size       = 6.5,
        color      = "grey45",
        hjust      = 0,
        lineheight = 1.35,
        margin     = margin(t = 6)
      )
    )
  )

print(map_combined)

# ── 7. Export ─────────────────────────────────────────────────────────────────
# Full-page figure: 180 mm wide (double column), 240 mm tall
ggsave(
  filename    = "dengue_amazon_map.tiff",
  plot        = map_combined,
  width       = 180,
  height      = 240,
  units       = "mm",
  dpi         = 300,
  compression = "lzw"
)

ggsave(
  filename = "dengue_amazon_map_preview.png",
  plot     = map_combined,
  width    = 180,
  height   = 240,
  units    = "mm",
  dpi      = 150
)

message("\u2713 Map saved. Review preview PNG before TIFF submission.")




get_spatial_munic_cache <- function(
    level = "munic",
    cache_dir = "/Users/maxanjos/.climasus4r_cache/spatial",
    use_cache = TRUE, 
    lang,
    verbose
) {
  msg <- get_spatial_messages(lang)
  
  if ((requireNamespace("sfarrow", quietly = TRUE))) { 
    # Define cache file name
    cache_file <- file.path(
      cache_dir,
      paste0(level, "_", ".parquet")
    )
  } else { 
    # Define cache file name
    cache_file <- file.path(
      cache_dir,
      paste0(level, "_", ".gpkg")
    )
  }
  
  # Try to load from cache
  if (use_cache && file.exists(cache_file)) {
    if (verbose) {
      cli::cli_alert_success(paste0(msg$loading_cache, basename(cache_file)))
    }
    
    tryCatch(
      {
        if ((requireNamespace("sfarrow", quietly = TRUE))) { 
          spatial_df <- sfarrow::st_read_parquet(cache_file)
        } else { 
          spatial_df <- sf::st_read(cache_file)
          spatial_df <- sf::st_as_sf(spatial_df)
        }
        return(spatial_df)
      },
      error = function(e) {
        if (verbose) {
          cli::cli_alert_warning(msg$cache_error)
        }
      }
    )
  }
  
  # Download from IBGE
  if (verbose) {
    cli::cli_alert_info(msg$downloading_data)
  }
  
  options(timeout = 600)
  spatial_df <- switch(
    level,
    "munic" = geobr::read_municipality(
      code_muni = "all",
      simplified = TRUE,
      showProgress = verbose,
      cache = FALSE,
    ),
    "state" = geobr::read_state(
      code_state = "all",
      simplified = TRUE,
      showProgress = verbose,
      cache = FALSE,
    )
  )
  
  # Save to cache
  if (use_cache) {
    if (verbose) {
      cli::cli_alert_info(msg$saving_cache)
    }
    
    tryCatch(
      { 
        if ((requireNamespace("sfarrow", quietly = TRUE))) { 
          sfarrow::st_write_parquet(obj = spatial_df, dsn = cache_file)
        } else {
          sf::st_write(spatial_df, cache_file, driver = "GPKG", quiet = TRUE, delete_dsn = TRUE, append = TRUE)
        }
        if (verbose) {
          cli::cli_alert_success(msg$cache_saved)
        }
      },
      error = function(e) {
        if (verbose) {
          cli::cli_alert_warning(paste0(msg$cache_save_error, e$message))
        }
      }
    )
  }
  
  return(spatial_df)
}

get_spatial_messages <- function(lang) {
  messages <- list(
    en = list(
      invalid_level = "Invalid geographic level specified.",
      valid_levels = "Valid levels are: 'state', 'munic', 'census', 'cep'.",
      system_column_missing = "Column 'system' not found in data. Please run detect_health_system() first.",
      cep_restricted = "Level 'cep' is only available for SIH and CNES systems.",
      cep_allowed = "CEP geocoding is available for this system.",
      detecting_column = "Auto-detecting geographic column...",
      no_column_found = "No valid geographic column found for the selected level.",
      expected_columns = "Expected columns: ",
      column_detected = "Geographic column detected: ",
      column_not_found = "Column not found in data frame: ",
      creating_cache = "Creating cache directory: ",
      cache_enabled = "Cache: ENABLED",
      loading_cache = "Loading from cache: ",
      cache_error = "Cache loading failed. Downloading fresh data...",
      downloading_data = "Downloading spatial data from IBGE...",
      saving_cache = "Saving to cache...",
      cache_saved = "Spatial data cached successfully.",
      cache_save_error = "Failed to save cache: ",
      joining_data = "Performing spatial join...",
      rows_removed = "Removed rows with missing geometries: ",
      geocoding_cep = "Geocoding postal codes (CEP)...",
      geocoding_count = "Geocoding unique CEPs: ",
      geocoding_error = "Geocoding failed: ",
      join_success = "Spatial join completed successfully!",
      final_rows = "Final dataset rows: "
    ),
    pt = list(
      invalid_level = "Nivel geografico invalido especificado.",
      valid_levels = "Niveis validos sao: 'state', 'munic', 'census', 'cep'.",
      system_column_missing = "Coluna 'system' nao encontrada nos dados. Execute detect_health_system() primeiro.",
      cep_restricted = "Nivel 'cep' disponivel apenas para os sistemas SIH e CNES.",
      cep_allowed = "Geocodificacao de CEP disponivel para este sistema.",
      detecting_column = "Auto-detectando coluna geografica...",
      no_column_found = "Nenhuma coluna geografica valida encontrada para o nivel selecionado.",
      expected_columns = "Colunas esperadas: ",
      column_detected = "Coluna geografica detectada: ",
      column_not_found = "Coluna nao encontrada no data frame: ",
      creating_cache = "Criando diretorio de cache: ",
      cache_enabled = "Cache: ATIVADO",
      loading_cache = "Carregando do cache: ",
      cache_error = "Falha ao carregar cache. Baixando dados novos...",
      downloading_data = "Baixando dados espaciais do IBGE...",
      saving_cache = "Salvando no cache...",
      cache_saved = "Dados espaciais armazenados em cache com sucesso.",
      cache_save_error = "Falha ao salvar cache: ",
      joining_data = "Realizando juncao espacial...",
      rows_removed = "Linhas removidas com geometrias faltantes: ",
      geocoding_cep = "Geocodificando codigos postais (CEP)...",
      geocoding_count = "Geocodificando CEPs unicos: ",
      geocoding_error = "Geocodificacao falhou: ",
      join_success = "Juncao espacial concluida com sucesso!",
      final_rows = "Linhas no dataset final: "
    ),
    es = list(
      invalid_level = "Nivel geografico invalido especificado.",
      valid_levels = "Niveles validos son: 'state', 'munic', 'census', 'cep'.",
      system_column_missing = "Columna 'system' no encontrada en los datos. Ejecute detect_health_system() primero.",
      cep_restricted = "Nivel 'cep' disponible solo para los sistemas SIH y CNES.",
      cep_allowed = "Geocodificacion de CEP disponible para este sistema.",
      detecting_column = "Auto-detectando columna geografica...",
      no_column_found = "Ninguna columna geografica valida encontrada para el nivel seleccionado.",
      expected_columns = "Columnas esperadas: ",
      column_detected = "Columna geografica detectada: ",
      column_not_found = "Columna no encontrada en el data frame: ",
      creating_cache = "Creando directorio de cache: ",
      cache_enabled = "Cache: ACTIVADO",
      loading_cache = "Cargando desde cache: ",
      cache_error = "Fallo al cargar cache. Descargando datos nuevos...",
      downloading_data = "Descargando datos espaciales del IBGE...",
      saving_cache = "Guardando en cache...",
      cache_saved = "Datos espaciales almacenados en cache con exito.",
      cache_save_error = "Fallo al guardar cache: ",
      joining_data = "Realizando union espacial...",
      rows_removed = "Filas eliminadas con geometrias faltantes: ",
      geocoding_cep = "Geocodificando codigos postales (CEP)...",
      geocoding_count = "Geocodificando CEPs unicos: ",
      geocoding_error = "Geocodificacion fallo: ",
      join_success = "Union espacial completada con exito!",
      final_rows = "Filas en el dataset final: "
    )
  )
  
  return(messages[[lang]])
}
