# =============================================================================
# exemplo_sus_grid_plot.R
# Demonstracao de sus_grid_plot() -- mapas de grade (raster) climatica/ambiental
# =============================================================================

library(climasus4r)
library(terra)

# -----------------------------------------------------------------------------
# 1. Exemplo rapido com raster sintetico (nao precisa de internet)
# -----------------------------------------------------------------------------

set.seed(1)
r <- rast(nrows = 100, ncols = 100, xmin = -60, xmax = -40, ymin = -30, ymax = -10)
values(r) <- rnorm(ncell(r), mean = 25, sd = 3)
names(r) <- "rainfall_chirps_mm"

sus_grid_plot(r, lang = "pt")

# Paleta divergente (ex.: anomalias) e sem fronteiras estaduais
sus_grid_plot(r, diverging = TRUE, name = "Anomalia (mm)", state_borders = FALSE)

# Raster com varias camadas (ex.: uma por mes) -- facet automatico
r_multi <- rast(nrows = 50, ncols = 50, xmin = -60, xmax = -40,
                 ymin = -30, ymax = -10, nlyrs = 4)
values(r_multi) <- rnorm(ncell(r_multi) * 4, mean = 0, sd = 1)
names(r_multi) <- paste0("mes_", 1:4)

sus_grid_plot(r_multi, layer = "all", diverging = TRUE, name = "Anomalia",
              state_borders = FALSE, lang = "pt")

# -----------------------------------------------------------------------------
# 2. Uso real: baixando dados de grade e plotando (precisa de internet)
# -----------------------------------------------------------------------------

## a) Direto de um SpatRaster em memoria (raster_area = TRUE)
# r_chirps <- sus_grid_chirps(
#   resolution    = "annual",
#   years         = 2022,
#   raster_area   = TRUE
# )
# sus_grid_plot(r_chirps, lang = "pt")

## b) A partir dos caminhos em cache (sem carregar tudo em memoria de uma vez)
paths <- sus_grid_chirps(resolution = "monthly", years = 2022)
sus_grid_plot(paths, layer = "all", lang = "pt")   # facet por mes
sus_grid_plot(paths, layer = 1, lang = "pt")       # so o primeiro mes

## c) Com fronteiras municipais sobrepostas
# library(geobr)
# mt_mun <- read_municipality(code_muni = "MT", year = 2020)
# r_era5 <- sus_grid_era5(
#   variables      = "t2m",
#   years          = 2022,
#   months         = 1,
#   raster_area    = "MT"   # recorta e mascara para o estado do Mato Grosso
# )
# sus_grid_plot(r_era5, municipalities = mt_mun, lang = "pt",
#               name = "Temperatura (°C)")

## d) Interativo (plotly)
# sus_grid_plot(r_chirps, interactive = TRUE, lang = "pt")
