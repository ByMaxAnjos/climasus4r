<p align="right">
  <img src="https://github.com/ByMaxAnjos/climasus4r/blob/master/inst/figures/logo.png?raw=true"
       alt="climasus4r logo"
       width="150"/>
</p>

# climasus4r

> **Análisis Espacio-temporales Integrados de Salud, Clima y Ambiente en Brasil**


<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test coverage](https://codecov.io/gh/ByMaxAnjos/climasus4r/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ByMaxAnjos/climasus4r)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen.svg)](https://github.com/ByMaxAnjos/climasus4r)
<!-- badges: end -->

---

[**climasus4r**](https://bymaxanjos.github.io/climasus4r/) es un conjunto de herramientas integradas en R diseñado para optimizar el análisis de datos de salud, clima y medio ambiente en Brasil. Desarrollado en el marco del proyecto INCT Conexão – Amazônia, automatiza y estandariza pasos críticos en flujos de trabajo de investigación epidemiológica y ambiental, promoviendo reproducibilidad, eficiencia y escalabilidad.

Basado en el sólido ecosistema del paquete [`microdatasus`](https://github.com/rfsaldanha/microdatasus), climasus4r amplía la funcionalidad al incorporar rutinas especializadas para estudios de clima y salud, reduciendo significativamente el esfuerzo requerido para la adquisición, limpieza, integración y preparación de datos.

## 🌐 Otros Idiomas

- [Português](../pt/index.html) | [English](../en/index.html)

## Instalación

**climasus4r** se encuentra actualmente en desarrollo activo. La versión más reciente se puede instalar directamente desde GitHub, lo que garantiza el acceso a las funciones más actualizadas. Antes de la instalación, es necesario tener el paquete remotes, que permite la instalación de paquetes alojados en GitHub.

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
## Infraestructura de datos 

La fase de infraestructura de **climasus4r** proporciona un canal completo de extremo a extremo para la preparación de datos sanitarios, desde la adquisición bruta hasta los datos listos para el análisis. Con 9 funciones principales, puede transformar los datos de DATASUS en series temporales agregadas, estandarizadas y listas para el modelado en cuestión de minutos.

```r
DATOS BRUTOS (DATASUS)
    ↓
[1] sus_data_import()           → Adquisición paralela
    ↓
[2] sus_data_clean_encoding()   → Corrección de codificación
    ↓
[3] sus_data_standardize()      → Estandarización multilingüe
    ↓
[4] sus_data_filter_cid()       → Filtrado por enfermedad
    ↓
[5] sus_create_variables()      → Creación de variables
    ↓
[6] sus_data_filter_demographics() → Filtrado demográfico
    ↓
[7] sus_data_quality_report()   → Verificación de calidad
    ↓
[8] sus_data_aggregate()        → Agregación temporal
    ↓
[9] sus_data_export()           → Exportación con metadatos
    ↓
DATOS LISTOS PARA EL ANÁLISIS
```

Para más información, consulte los [Tutoriales](articles/tutorials.html) y la [Documentación Completa](reference/index.html).
