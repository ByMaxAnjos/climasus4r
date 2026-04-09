<p align="center">
  <img src="https://github.com/ByMaxAnjos/climasus4r/blob/master/inst/figures/logo.png?raw=true"
       alt="climasus4r logo"
       width="190"/>
</p>

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

## 📦 Descripción General de las Funciones

| Categoría | Función | Descripción |
| :--- | :--- | :--- |
| **📥 Importación y Exportación** | `sus_data_import()` | Importa y preprocesa datos de DATASUS con caché inteligente. |
| | `sus_data_read()` | Lectura optimizada de datos procesados con soporte paralelo. |
| | `sus_data_export()` | Exporta datos procesados preservando metadatos. |
| **🧹 Limpieza y Estandarización** | `sus_data_clean_encoding()` | Detecta y corrige problemas de codificación de caracteres. |
| | `sus_data_standardize()` | Estandariza nombres de columnas y valores de datos del SUS. |
| | `sus_data_create_variables()` | Crea variables derivadas para análisis epidemiológico. |
| **🔍 Filtros y Selección** | `sus_data_filter_cid()` | Filtra por códigos CIE-10 o grupos de enfermedades (multilingüe). |
| | `sus_data_filter_demographics()` | Filtra datos por variables demográficas (edad, sexo, raza). |
| **🗺️ Espacial y Censo** | `sus_spatial_join()` | Vincula datos del SUS a mallas geográficas brasileñas. |
| | `sus_census_join()` | Enriquece datos de salud con variables socioeconómicas del Censo. |
| | `sus_data_aggregate()` | Agrega datos de salud en series temporales. |
| **📊 Calidad y Metadatos** | `sus_data_quality_report()` | Genera informes detallados sobre la calidad de los datos. |
| | `sus_data_cid_select()` | Enumera los grupos de enfermedades disponibles para filtrar. |
| | `sus_census_select()` | Explorador interactivo de variables del Censo. |
| **⚡ Caché** | `sus_cache_clear()` | Gestiona y limpia el almacenamiento local de archivos. |


## Sistemas Soportados

**climasus4r** permite el acceso simplificado y estandarizado a los principales sistemas de información del DATASUS mediante la integración con el paquete **microdatasus**. Esta integración automatiza la recolección de datos brutos de diferentes bases del sistema de salud brasileño, abarcando información de epidemiología, mortalidad, hospitalizaciones y red asistencial. A partir de estos datos, climasus4r organiza, limpia y estructura la información, transformando bases complejas del DATASUS en conjuntos de datos listos para análisis estadístico y estudios espacio-temporales.

#### **1. SIM (Sistema de Información sobre Mortalidad)**

* `"SIM-DO"`: Declaraciones de Defunción (Dataset completo)
* `"SIM-DOFET"`: Defunciones Fetales
* `"SIM-DOEXT"`: Defunciones por Causas Externas
* `"SIM-DOINF"`: Defunciones Infantiles
* `"SIM-DOMAT"`: Defunciones Maternas

#### **2. SIH (Sistema de Información Hospitalaria)**

* `"SIH-RD"`: AIH (Autorizaciones de Internación Hospitalaria) - General
* `"SIH-RJ"`: AIH - Específico para Río de Janeiro
* `"SIH-SP"`: AIH - Específico para São Paulo
* `"SIH-ER"`: Registros de Emergencia

#### **3. SINAN (Sistema de Información de Agravios de Notificación)**

* `"SINAN-DENGUE"`: Casos de Dengue
* `"SINAN-CHIKUNGUNYA"`: Casos de Chikungunya
* `"SINAN-ZIKA"`: Casos de virus Zika
* `"SINAN-MALARIA"`: Casos de Malaria
* `"SINAN-CHAGAS"`: Casos de Enfermedad de Chagas
* `"SINAN-LEISHMANIOSE-VISCERAL"`: Leishmaniasis Visceral
* `"SINAN-LEISHMANIOSE-TEGUMENTAR"`: Leishmaniasis Tegumentaria
* `"SINAN-LEPTOSPIROSE"`: Casos de Leptospirosis

#### **4. SIA (Sistema de Información Ambulatoria)**

* `"SIA-AB"`: Atención Básica (Primaria)
* `"SIA-ABO"`: Procedimientos Odontológicos
* `"SIA-ACF"`: Asistencia Farmacéutica
* `"SIA-AD"`: Alta Complejidad/Diferenciada
* `"SIA-AN"`: Atención Domiciliaria
* `"SIA-AM"`: Ambulatorio de Especialidades
* `"SIA-AQ"`: Acciones Estratégicas
* `"SIA-AR"`: Regulación
* `"SIA-ATD"`: Urgencia/Emergencia
* `"SIA-PA"`: Procedimientos Ambulatorios en Hospital
* `"SIA-PS"`: Atención Psicosocial
* `"SIA-SAD"`: Atención Especializada

#### **5. CNES (Catastro Nacional de Establecimientos de Salud)**

* `"CNES-LT"`: Camas (Lechos)
* `"CNES-ST"`: Profesionales de Salud
* `"CNES-DC"`: Equipos (Detallado)
* `"CNES-EQ"`: Equipos (Resumen)
* `"CNES-SR"`: Servicios Especializados
* `"CNES-HB"`: Camas Hospitalarias (Histórico)
* `"CNES-PF"`: Personal Físico (Profesionales)
* `"CNES-EP"`: Participantes de Enseñanza
* `"CNES-RC"`: Clasificación Hospitalaria
* `"CNES-IN"`: Indicadores Hospitalarios
* `"CNES-EE"`: Entidades de Enseñanza
* `"CNES-EF"`: Instalaciones de Enseñanza
* `"CNES-GM"`: Gestión y Apoyo

#### **6. SINASC (Sistema de Información sobre Nacidos Vivos)**

* `"SINASC"`: Declaraciones de Nacidos Vivos

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
  sus_data_create_variables(create_age_groups = TRUE, lang = "es")
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
[5] sus_data_create_variables()      → Creación de variables
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

Para más información, consulte los [Tutoriales](..es/articles/tutorials.html) y la [Documentación Completa](..es/reference/index.html).

## 🌲 ¿Tienes comentarios o sugerencias?
¿Tienes una idea para mejorar o encontraste un error? ¡Nos encantaría escucharte! Haz clic en el botón de abajo para crear un nuevo *issue* en GitHub y compartir tus comentarios directamente con nosotros.

<a href='https://github.com/ByMaxAnjos/climasus4r/issues/new'>
  <button type="button" class="btn" style="background-color: #2E7D32; color: white; padding: 8px 16px; font-size: 14px; font-weight: bold; border: none; border-radius: 6px; cursor: pointer; transition: background-color 0.3s;">
    Abrir un issue en el repositorio GitHub
  </button>
</a>