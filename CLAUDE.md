# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

**climasus4r** is an R package for integrated health-climate-environment data analysis in Brazil. It automates the full data pipeline: importing DATASUS health data (SIM, SIH, SINAN, SIA, CNES, SINASC), integrating climate (INMET, ERA5) and environmental data, spatiotemporal aggregation, and producing reproducible analytical pipelines (RAPs). Multilingual output (Portuguese, Spanish, English) is a first-class feature.

## Development Commands

```r
# Install dependencies and load package for development
devtools::install_deps()
devtools::load_all()

# Document (regenerate man/ from Roxygen2 comments)
devtools::document()

# Build and check package
devtools::build()
devtools::check()

# Run R CMD check (standard)
R CMD build .
R CMD check climasus4r_*.tar.gz

# Install locally
devtools::install()
```

There is no testthat test suite yet (package is v0.0.0.9000 experimental stage).

## Architecture

### Core S3 Class: `climasus_df`

The central data structure is `climasus_df`, a subclass of `tbl_df`/tibble with an embedded `sus_meta` attribute. Every exported function receives a `climasus_df`, mutates its metadata, and returns a `climasus_df`. This is defined in `R/utils-S3.R`.

**Metadata fields** (stored in the `sus_meta` attribute):
- `system` — DATASUS source system (SIM, SIH, SIA, SINAN, CNES, SINASC)
- `stage` — Pipeline stage (import → clean → stand → filter_cid → filter_demo → derive → aggregate → spatial → climate → census)
- `type` — Data type/aggregation strategy
- `backend` — Storage backend (tibble, parquet, duckdb)
- `history` — Timestamped character vector of processing steps
- `user` — Free-form user metadata list

**Controlled vocabularies** are defined as internal constants at the top of `R/utils-S3.R`: `.climasus_systems`, `.climasus_stages`, `.climasus_types`, `.climasus_backends`, `.climasus_stage_order`.

### Key Internal Functions (`R/utils-S3.R`)

- `new_climasus_df()` — Low-level constructor; sets class and `sus_meta` attribute
- `validate_climasus_df()` — Validates metadata fields against controlled vocabularies
- `ensure_climasus_df()` — Used by pipeline functions to promote a plain tibble to `climasus_df` when chaining
- `sus_meta()` — Public interface to read/write metadata fields

### Metadata Persistence Across Backends

Metadata survives serialization via backend-specific encoding:
- **Tibble**: `sus_meta` R attribute (in-memory only)
- **Parquet**: JSON-serialized under the `"climasus_meta"` key in the Arrow schema (`R/utils-S3.R` → `write_parquet_climasus()`)
- **DuckDB**: Stored in a companion table named `<view_name>__meta`

Conversion functions: `as_arrow_climasus()`, `from_arrow_climasus()`, `as_duckdb_climasus()`, `from_duckdb_climasus()`, `write_parquet_climasus()`, `write_duckdb_climasus()`.

### Data Pipeline Flow

Functions are designed for magrittr pipe chaining:

```r
sus_data_import() %>%
  sus_data_clean_encoding() %>%
  sus_data_standardize() %>%
  sus_data_filter_cid() %>%
  sus_data_filter_demographics() %>%
  sus_data_create_variables() %>%
  sus_data_aggregate() %>%
  sus_climate_inmet() %>%
  sus_climate_fill_inmet() %>%
  sus_climate_aggregate()
```

Each step validates that the input has the appropriate `stage` and advances the metadata.

### Multilingual System

`R/utils-i18n.R` (~9,300 lines) contains translation dictionaries for column names and UI messages across `"pt"` (default), `"en"`, and `"es"`. All user-facing functions accept a `lang` parameter. Do not hardcode Portuguese strings in function output—use the i18n helpers.

### Climate Integration (`R/utils-clim.R`)

Handles INMET CSV parsing, meteorological variable mappings, missing data quality flags, and temporal aggregation strategies (exact, discrete_lag, moving_window, offset_window, distributed_lag, degree_days, seasonal, threshold_exceedance, weighted_window, cold_wave_exceedance). INMET station metadata is stored as a Parquet file at `inst/data_4r/station_meta.parquet`.

### File Naming Conventions

- `sus_*.R` — Main exported pipeline functions (one major function per file, typically)
- `utils-*.R` — Shared utilities: `utils-S3.R` (class system), `utils-clim.R` (climate), `utils-i18n.R` (translations), `utils-pipe.R` (pipe re-export)
- `*_explore.R` — Exploratory/helper functions

### Dependencies

**Core imports**: `dplyr`, `tidyr`, `stringr`, `stringi`, `cli`, `lubridate`, `magrittr`, `rlang`, `purrr`, `furrr`, `future`, `data.table`, `glue`, `fs`, `digest`, `DBI`, `duckdb`, `jsonlite`, `microdatasus` (GitHub: `rfsaldanha/microdatasus`), `read.dbc`.

**Conditional (Suggests)**: `arrow`/`parquet` for Parquet support, `sf`/`geobr`/`sfarrow` for spatial, `censobr` for census, `ggplot2`/`plotly`/`patchwork` for visualization.

Use `rlang::check_installed()` or `requireNamespace()` when calling Suggested packages so the core package works without them.

### Roxygen2 Patterns

- Markdown enabled (`Roxygen: list(markdown = TRUE)` in DESCRIPTION)
- All exports use `@export`; internal helpers use `@keywords internal` and `@noRd`
- Global variables used in `dplyr`/`data.table` NSE must be declared with `utils::globalVariables()` (see existing declarations in each file) to pass `R CMD check`
- Run `devtools::document()` after any Roxygen change before checking

### Parallel Processing

Parallel execution uses the `future`/`furrr` framework. Functions that support parallelism respect the user's active `future::plan()`. Do not set the plan inside package functions; let the user control it.

## Sistema de Agentes

Este projeto utiliza um ecossistema de **21 agentes especialistas** para coordenar análise, implementação e revisão de código. Os prompts e instruções estão em `agents/`.

### Governança e Roteamento

Consulte **`agents/agent-governance.instructions.md`** para:
- Regra geral de qual agente acionar
- Tabela de roteamento com critérios claros de entrada/saída
- Fluxos recomendados por tipo de problema (estrutural, local, operacional, dados, funcional)
- Regras de desempate ("quero entender o projeto?" → ProjectAnalyst; "quero saber se está lento?" → PerformanceEngineer)

**Resumo**: Comece pelo agente mais específico. Se a demanda for ampla/ambígua → `TechnicalCoordinator`. Se precisar entender o projeto → `ProjectAnalyst`.

### Instruções Compartilhadas

Consulte **`agents/climasus-agents.instructions.md`** para:
- Linguagem padrão (português brasileiro)
- Formato de saída esperado (resumo executivo, contexto, problemas, pontos fortes, recomendações, priorização, próximos agentes)
- Classificação de achados (fato, risco, má prática, lacuna, sugestão)
- Modos de operação (Análise vs. Implementação)
- Princípios gerais (clareza > sofisticação; evidência > suposição)

### Agentes Disponíveis

| # | Agente | Arquivo | Quando Acionar |
|---|--------|---------|---|
| 1 | **TechnicalCoordinator** | `TechnicalCoordinator.agent.md` | Demanda ampla, ambígua ou multiárea |
| 2 | **ProjectAnalyst** | `ProjectAnalyst.agent.md` | Precisa entender estrutura ou contexto do projeto |
| 3 | **SoftwareArchitect** | `SoftwareArchitect.agent.md` | Problema estrutural ou recorrente |
| 4 | **BackendEngineer** | `BackendEngineer.agent.md` | Problema em APIs, integrações ou backend |
| 5 | **DevOpsEngineer** | `DevOpsEngineer.agent.md` | Problema na entrega, CI/CD ou containerização |
| 6 | **SecurityEngineer** | `SecurityEngineer.agent.md` | Risco de segurança ou dados sensíveis |
| 7 | **DatabaseSpecialist** | `DatabaseSpecialist.agent.md` | Problema de persistência, SQL ou schema |
| 8 | **JavaScriptTypeScriptDeveloper** | `JavaScriptTypeScriptDeveloper.agent.md` | Problema em UI, estado, rotas ou JS/TS |
| 9 | **DocumentationReviewer** | `DocumentationReviewer.agent.md` | Documentação ruim, desatualizada ou incompleta |
| 10 | **DocumentationWriter** | `DocumentationWriter.agent.md` | Criar ou refatorar documentação |
| 11 | **PerformanceEngineer** | `PerformanceEngineer.agent.md` | Sistema lento ou custoso |
| 12 | **ObservabilityEngineer** | `ObservabilityEngineer.agent.md` | Difícil diagnosticar em produção |
| 13 | **DataEngineer** | `DataEngineer.agent.md` | Problema no pipeline ou processamento de dados |
| 14 | **DataScientist** | `DataScientist.agent.md` | Objetivo é insight analítico ou exploração |
| 15 | **PythonCodeReviewer** | `PythonCodeReviewer.agent.md` | Clareza, manutenção ou padrões em Python |
| 16 | **RCodeReviewer** | `RCodeReviewer.agent.md` | Clareza, manutenção ou padrões em R |
| 17 | **TestEngineer** | `TestEngineer.agent.md` | Risco de regressão ou falta de confiança |
| 18 | **RequirementsAnalyst** | `RequirementsAnalyst.agent.md` | Ambiguidade funcional ou requisitos vagos |
| 19 | **PromptReviewer** | `PromptReviewer.agent.md` | Agente vago, amplo ou inconsistente |
| 20 | **DATASUSSpecialist** | `DATASUSSpecialist.agent.md` | Dados DATASUS, SIM-DO, CID-10, DBC, encoding |
| 21 | **PackageEngineer** | `PackageEngineer.agent.md` | Preparar pacote R/Python para CRAN/PyPI |

### Fluxos Recomendados

- **Projeto desconhecido**: TechnicalCoordinator → ProjectAnalyst → SoftwareArchitect → especialistas
- **Problema local de código**: especialista local → TestEngineer (se houver risco de regressão)
- **Problema estrutural**: ProjectAnalyst → SoftwareArchitect → especialista(s) afetado(s)
- **Problema operacional**: DevOpsEngineer → ObservabilityEngineer → PerformanceEngineer/SecurityEngineer
- **Problema de dados**: DataEngineer → DatabaseSpecialist → DataScientist
- **Dúvida funcional**: RequirementsAnalyst → especialista técnico → TestEngineer