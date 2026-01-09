# climasus4r: Kit de Herramientas Integrado para Análisis de Datos de Salud, Clima y Medio Ambiente

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test coverage](https://codecov.io/gh/ByMaxAnjos/climasus4r/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ByMaxAnjos/climasus4r)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen.svg)](https://github.com/ByMaxAnjos/climasus4r)
<!-- badges: end -->

---

**climasus4r** es un conjunto de herramientas integradas en R diseñado para optimizar el análisis de datos de salud, clima y medio ambiente en Brasil. Desarrollado en el marco del proyecto INCT Conexão – Amazônia, automatiza y estandariza pasos críticos en flujos de trabajo de investigación epidemiológica y ambiental, promoviendo reproducibilidad, eficiencia y escalabilidad.

Basado en el sólido ecosistema del paquete [`microdatasus`](https://github.com/rfsaldanha/microdatasus), climasus4r amplía la funcionalidad al incorporar rutinas especializadas para estudios de clima y salud, reduciendo significativamente el esfuerzo requerido para la adquisición, limpieza, integración y preparación de datos.

## 🌐 Otros Idiomas

- [Português](../pt/index.html) | [English](../en/index.html)

## Instalación

```r
# Instale remotes si no lo tiene
if (!require("remotes")) {
  install.packages("remotes")
}

# Instale CLIMASUS4r
remotes::install_github("ByMaxAnjos/climasus4r", dependencies = TRUE, upgrade = "never")
```

## Inicio Rápido

```r
library(climasus4r)
library(dplyr)

# Pipeline completo: Datos listos para análisis
df_analisis <- sus_data_import(
  uf = "SP",
  year = 2023,
  system = "SIM-DO"
) |>
  sus_data_clean_encoding(lang = "es") |>
  sus_data_standardize(lang = "es") |>
  sus_data_filter_cid(disease_group = "respiratory", lang = "es") |>
  sus_create_variables(create_age_groups = TRUE, lang = "es")
```

Para más información, consulte los [Tutoriales](articles/tutorials.html) y la [Documentación Completa](reference/index.html).
