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

# ── 0. Aggregate to municipality level ────────────────────────────────────────
# One row per municipality: mean annual incidence + cumulative cases
df_muni_summary <- df_analysis %>%
  group_by(
    notification_municipality_code,
    name, uf_code, lon, lat, pop_21, is_capital
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

# State borders (for outline layer)
# states_sf <- geobr::read_state(year = 2020, showProgress = FALSE) %>%
#   filter(abbrev_state %in% amazon_states)
states_sf <- get_spatial_munic_cache(
  level = "states",
  lang = "pt",
  verbose = TRUE
) %>%
  filter(abbrev_state %in% amazon_states)

# Municipality polygons (for annual choropleth)
muni_sf <- geobr::read_municipality(year = 2020, showProgress = FALSE) %>%
  filter(abbrev_state %in% amazon_states) %>%
  mutate(code_muni_6 = substr(as.character(code_muni), 1, 6))

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
incidence_palette <- c(
  "#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C",
  "#FC4E2A", "#E31A1C", "#B10026"
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
  
  # State abbreviation labels (centroid)
  geom_sf_text(
    data     = states_sf,
    aes(label = abbrev_state),
    size     = 2.8,
    color    = "#4A5A66",
    fontface = "italic",
    alpha    = 0.6
  ) +
  
  # Color scale: log1p mean incidence with back-transformed breaks
  scale_color_gradientn(
    colors  = incidence_palette,
    name    = expression(atop("Mean Incidence", "(per 100k, log"[1+x]*" scale)")),
    breaks  = log1p(c(0, 50, 200, 500, 1000, 2000)),
    labels  = c("0", "50", "200", "500", "1,000", "2,000"),
    guide   = guide_colorbar(
      title.position = "top",
      barwidth       = unit(10, "lines"),
      barheight      = unit(0.55, "lines"),
      ticks.linewidth = 0.4
    )
  ) +
  
  # Size scale: log1p total cases with back-transformed labels
  scale_size_continuous(
    name   = "Cumulative Cases\n(log scale)",
    range  = c(0.5, 9),
    breaks = log1p(c(100, 1000, 10000, 100000)),
    labels = c("100", "1,000", "10,000", "100,000"),
    guide  = guide_legend(
      title.position = "top",
      override.aes   = list(alpha = 0.8, color = "#FD8D3C")
    )
  ) +
  
  coord_sf(
    xlim = c(-74, -44),
    ylim = c(-18,   6),
    expand = FALSE
  ) +
  
  labs(
    title    = "A  Cumulative Dengue Burden and Mean Incidence Rate (2014\u20132021)",
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
  
  facet_wrap(~ year, ncol = 4) +
  
  scale_fill_gradientn(
    colors   = incidence_palette,
    name     = expression("Mean Incidence per 100k (log"[1+x]*")"),
    breaks   = log1p(c(0, 50, 200, 500, 1500)),
    labels   = c("0", "50", "200", "500", "1,500"),
    na.value = "#E8E4DF",
    guide    = guide_colorbar(
      title.position  = "top",
      barwidth        = unit(12, "lines"),
      barheight       = unit(0.5, "lines"),
      ticks.linewidth = 0.4
    )
  ) +
  
  coord_sf(
    xlim   = c(-74, -44),
    ylim   = c(-18,   6),
    expand = FALSE
  ) +
  
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
      "Population: IBGE 2021 estimates. ",
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