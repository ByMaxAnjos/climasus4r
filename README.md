<p align="center">
  <img src="https://github.com/ByMaxAnjos/climasus4r/blob/master/inst/figures/logo.png?raw=true"
       alt="climasus4r logo"
       width="190"/>
</p>


> **Análises Espaço-temporal Integradas de Saúde, Clima e Ambiente no Brasil**

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test coverage](https://codecov.io/gh/ByMaxAnjos/climasus4r/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ByMaxAnjos/climasus4r)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen.svg)](https://github.com/ByMaxAnjos/climasus4r)
<!-- badges: end -->


O [**climasus4r**](https://bymaxanjos.github.io/climasus4r/) transforma como pesquisadores analisam a interseção de clima, saúde e ambiente no Brasil. Este pacote R integra dados de múltiplas fontes—sistemas de saúde, estações meteorológicas, imagens de satélite e bancos de dados socioeconômicos—em um framework analítico unificado. O que normalmente levaria meses de manipulação manual de dados agora leva minutos ou segundos, graças a pipelines automatizados que garantem reprodutibilidade e rigor científico. Com o climasus4r usuários podem compartilhar não apenas resultados, mas fluxos de trabalho analíticos inteiros que outros podem verificar e construir sobre.

## Por que usar climasus4r?

> | Aspecto | climasus4r | Pacotes Similares |
> |---|---|---|
> | **Integração Saúde-Clima** | ✓ Nativa | ✗ Requer integração manual |
> | **Dados SUS Automatizados** | ✓ Sim (6 sistemas) | ✗ Requer download manual |
> | **RAPs (Reprodutibilidade)** | ✓ Integrado | ✗ Requer documentação manual |
> | **Multilíngue** | ✓ PT/ES/EN | ✗ Geralmente apenas EN |
> | **Processamento Paralelo** | ✓ Nativo | ✗ Requer configuração manual |
> | **Agregação Espaciotemporal** | ✓ Flexível (15+ opções) | ✗ Limitado |
> | **Foco em Contexto Brasileiro** | ✓ Sim | ✗ Genérico |


## Principais Funcionalidades

> - **Integração Unificada de Dados:** Combine dados de saúde (SUS), clima (INMET, ERA5), ambientais (MapBiomas, AQI) e socioeconômicos (IBGE) de forma transparente.
> - **Pipelines Automatizados:** Importe, limpe, padronize e agregue dados com poucas linhas de código.
> - **Workflows Reprodutíveis:** Gere RAPs (Reproducible Analytical Pipelines) que documentam cada etapa da sua análise.
> - **Agregação Flexível:** Analise dados em escalas diárias, semanais, mensais, sazonais ou anuais; em níveis municipais, regionais ou nacionais.
> - **Suporte Multilíngue:** Saídas em Português, Espanhol ou Inglês.
> - **Alta Performance:** Processamento paralelo para lidar com grandes volumes de dados de forma eficiente.


# Instalação

O pacote `climasus4r` está disponível via r-universe e GitHub. Recomendamos a **Opção 1** para a maioria dos usuários — não requer `remotes`/`devtools` nem token do GitHub.

---

## Opção 1: Instalação via r-universe (Recomendado)

```r
install.packages("climasus4r", repos = c(
  "https://bymaxanjos.r-universe.dev",
  "https://cloud.r-project.org"
))
```

## Opção 2: Instalação via GitHub

Caso você ainda não tenha o pacote `remotes` instalado, execute o código abaixo:

::: callout-tip
**Pré-requisito::** Se você já tem o   `remotes` ou `devtools` instalado, pode pular
essa etapa.
:::

```r
if (!require("remotes")) { install.packages("remotes")}
```

```r
# Instalar ou atualizar o pacote diretamente do GitHub
remotes::install_github("ByMaxAnjos/climasus4r", upgrade = "never")
```

::: callout-warning
**Atualizações:** O `climasus4r` está em desenvolvimento ativo. Para atualizar, basta executar o comando acima novamente. O R irá sobrescrever a versão antiga automaticamente. Após a atualização, recomenda-se reiniciar a sessão (Menu: Session > Restart R).
:::

## Opção 3: Instalação via Arquivo Local (.zip)

Esta opção é recomendada para ambientes com instabilidade de conexão, restrições de rede ou uso em treinamentos com múltiplos usuários.

1. **Baixar o pacote**: Clique [AQUI](https://github.com/ByMaxAnjos/climasus4r/archive/refs/heads/master.zip) para baixar o .zip
2. **Extrair o arquivo**: Extraia o conteúdo em uma pasta de fácil acesso (ex: Downloads ou Desktop).
3. **Instalar localmente**: Execute o comando abaixo, ajustando o caminho para a pasta onde você extraiu o arquivo:

```r
# IMPORTANTE: Altere o caminho abaixo para o local onde você salvou o pacote
remotes::install_local(
  "C:/Caminho/Para/Sua/Pasta/climasus4r-master", 
  upgrade = "never"
)
```

## Carregando o pacote

Após a instalação, carregue o pacote sempre que iniciar uma nova sessão no R para liberar as funções:
```r
library(climasus4r)
```

## Início Rápido

```r
library(climasus4r)

# Pipeline completo da Fase 1: Dados prontos para análise em 8 passos
df_analise <- sus_data_import(
  uf = "SP",
  year = 2023,
  system = "SIM-DO"
) |>
  sus_data_clean_encoding(lang = "pt") |>
  sus_data_standardize(lang = "pt") |>
  sus_data_filter_cid(disease_group = "respiratory", lang = "pt") |>
  sus_create_variables(
    create_age_groups = TRUE,
    create_calendar_vars = TRUE,
    lang = "pt"
  ) |>
  sus_data_filter_demographics(
    age_range = c(0, 5),  # Crianças menores de 5 anos
    sex = c("Feminino", "Masculino"),
    lang = "pt"
  ) |>
  sus_data_aggregate(
    time_unit = "day",
    lang = "pt"
  )
#Save
sus_data_export(df_analise,
    file_path = "dados_respiratorias_pediatricas_sp_2023.csv",
    format = "csv",
    include_metadata = TRUE,
    lang = "pt"
  )
```

---

# climasus4r — Catálogo Completo de Funções

> Pipeline integrado de análise saúde-clima-ambiente no Brasil.  
> ~65 funções exportadas · 81 arquivos R · idiomas: `pt` (padrão), `en`, `es`

---

## Pipeline de Processamento

```
sus_data_import() / sus_data_read()
        ↓
sus_data_clean_encoding() → sus_data_standardize()
        ↓
sus_data_filter_cid() → sus_data_filter_demographics() → sus_data_create_variables()
        ↓
sus_data_aggregate()
        ↓
sus_spatial_join()  |  sus_climate_*/sus_grid_*  |  sus_socio_add_census()
        ↓
sus_mod_dlnm() / sus_mod_casecrossover() / sus_mod_ml() / sus_mod_spatial_*() ...
        ↓
sus_mod_plot_*() / sus_data_plot_*() / sus_data_export()
```

---

## 1. Metadados e Classe Central

| Função | Arquivo | Input | Output | Descrição |
|--------|---------|-------|--------|-----------|
| `sus_meta()` | `climasus_meta.R` | `x` (climasus_df), `field`, `add_history`, `valid_values` | lista / campo / climasus_df atualizado | Lê ou escreve metadados do pipeline (stage, type, system, history) |

---

## 2. Importação

| Função | Arquivo | Input | Output (stage) | Descrição |
|--------|---------|-------|----------------|-----------|
| `sus_data_import()` | `sus_data_import.R` | `system`, `uf`, `year`, `month`, `lang` | `"import"` | Baixa dados via microdatasus (SIM, SIH, SINAN, SIA, CNES, SINASC) |
| `sus_data_read()` | `sus_data_read.R` | `path`, `system`, `lang` | `"import"` | Lê arquivos SUS locais (.dbc, .parquet, .csv) |

---

## 3. Preparação e Limpeza

| Função | Arquivo | Input (stage) | Output (stage) | Descrição |
|--------|---------|---------------|----------------|-----------|
| `sus_data_clean_encoding()` | `sus_data_clean_encoding.R` | `"import"` | `"clean"` | Corrige encoding Latin-1/UTF-8 e caracteres malformados |
| `sus_data_standardize()` | `sus_data_standardize.R` | `"clean"` | `"stand"` | Padroniza nomes de colunas e tipos entre sistemas SUS |

---

## 4. Filtragem e Derivação

| Função | Arquivo | Input (stage) | Output (stage) | Descrição |
|--------|---------|---------------|----------------|-----------|
| `sus_data_filter_cid()` | `sus_data_filter_cid.R` | `"stand"` | `"filter_cid"` | Filtra por capítulos ou códigos CID-10 |
| `sus_data_filter_demographics()` | `sus_data_filter_demographics.R` | `"filter_cid"` | `"filter_demo"` | Filtra por faixa etária, sexo e localidade de residência |
| `sus_data_create_variables()` | `sus_create_variables.R` | `"filter_demo"` | `"derive"` | Cria variáveis derivadas (grupos etários, dia da semana, sazonalidade) |
| `sus_filter_cid_explore()` | `sus_filter_cid_explore.R` | `df`, `lang`, `output` | viewer / tibble | Navegador interativo de grupos CID-10 |
| `sus_data_cid_select()` | `sus_data_filter_cid.R` | `df`, `cid_group`, `lang` | climasus_df filtrado | Seleção de desfechos por grupo CID predefinido |

---

## 5. Agregação e Exportação

| Função | Arquivo | Input | Output | Descrição |
|--------|---------|-------|--------|-----------|
| `sus_data_aggregate()` | `sus_data_aggregate.R` | climasus_df, `by`, `unit`, `lang` | `"aggregate"` | Agrega registros por município × data × variável |
| `sus_data_export()` | `sus_data_export.R` | climasus_df, `path`, `format` | arquivo .parquet / .duckdb | Exporta dados processados |
| `sus_data_quality_report()` | `sus_data_quality_report.R` | climasus_df, `lang`, `output` | HTML/PDF | Relatório diagnóstico de completude e consistência |

---

## 6. Visualização de Dados de Saúde

| Função | Arquivo | Input | Output | Descrição |
|--------|---------|-------|--------|-----------|
| `sus_data_plot_demographics()` | `sus_data_plot_demographics.R` | climasus_df, `lang` | ggplot2 | Pirâmide etária, sexo, distribuições demográficas |
| `sus_data_plot_aggregate_ts()` | `sus_data_plot_aggregate_ts.R` | climasus_df, `by`, `lang` | ggplot2 | Série temporal de desfechos agregados |
| `sus_data_plot_aggregate_map()` | `sus_data_plot_aggregate_map.R` | climasus_df, `year`, `lang` | ggplot2 / mapa | Mapa por município dos desfechos |

---

## 7. Integração Espacial

| Função | Arquivo | Input | Output (stage) | Descrição |
|--------|---------|-------|----------------|-----------|
| `sus_spatial_join()` | `sus_join_spatial.R` | climasus_df, `level` (munic/state/biome), `lang`, `use_cache` | `"spatial"` + sf | Vincula dados SUS a limites administrativos brasileiros via geobr |

---

## 8. Dados Climáticos — Estações INMET

| Função | Arquivo | Input | Output | Descrição |
|--------|---------|-------|--------|-----------|
| `sus_climate_inmet()` | `sus_climate_inmet.R` | `stations`, `start_date`, `end_date`, `lang` | type="inmet" | Importa dados horários INMET (temperatura, umidade, chuva, vento) |
| `sus_climate_fill_inmet()` | `sus_climate_fill_gap.R` | climasus_df (inmet), `method` | type="filled" | Preenche lacunas por interpolação espacial ou regressão |
| `sus_climate_normals()` | `sus_climate_normals.R` | `stations`, `period`, `lang` | normais climatológicas | Médias de 30 anos por estação |
| `sus_climate_normals_meta()` | `sus_climate_normals.R` | `stations`, `lang` | tibble de metadados | Lista normais disponíveis por estação |
| `sus_climate_anomaly()` | `sus_climate_anomaly.R` | `observed`, `normals`, `method`, `vars` | type="anomaly" | Anomalias climáticas (absoluta / relativa / padronizada) |
| `sus_climate_aggregate()` | `sus_climate_aggregate.R` | health_data, climate_data, `climate_var`, `temporal_strategy`, `lag_days` | climasus_df integrado | Integra clima ao dado de saúde com 10 estratégias temporais |
| `sus_climate_compute_heatwaves()` | `sus_climate_compute_heatwaves.R` | climasus_df, `method`, `baseline_*`, `percentile` | lista climasus_hw (`$events`, `$daily`, `$summary`) | Detecta ondas de calor (WHO, WMO, ECCA, local) |
| `sus_climate_compute_spi()` | `sus_climate_compute_spi.R` | climasus_df, `scale`, `lang` | SPI | Índice Padronizado de Precipitação |
| `sus_climate_compute_spei()` | `sus_climate_compute_spei.R` | climasus_df, `scale`, `lang` | SPEI | Índice de Precipitação-Evapotranspiração Padronizado |
| `sus_climate_compute_indicators()` | `sus_climate_compute_indicators.R` | climasus_df, `indicators`, `region`, `keep_source_vars` | wbgt_c, hi_c, utci_c, pet_c, cdd_c, hdd_c… | 15+ índices bioclimáticos e de estresse térmico |

**Auxiliares de Ondas de Calor:**

| Função | Arquivo | Input | Output | Descrição |
|--------|---------|-------|--------|-----------|
| `hw_get_events()` | `sus_climate_compute_heatwaves.R` | climasus_hw | tibble de eventos | Extrai eventos discretos de onda de calor |
| `hw_count_by_year()` | `sus_climate_compute_heatwaves.R` | climasus_hw | tibble anual | Contagem anual de eventos e dias de onda de calor |
| `hw_active_days()` | `sus_climate_compute_heatwaves.R` | climasus_hw | tibble filtrado | Filtra dias com onda de calor ativa |

**Visualização Climática:**

| Função | Arquivo | Input | Output | Descrição |
|--------|---------|-------|--------|-----------|
| `sus_climate_plot_aggregate()` | `sus_climate_plot_aggregate.R` | climasus_df, `lang` | ggplot2 | Série temporal de variáveis climáticas |
| `sus_climate_plot_fill()` | `sus_climate_plot_fill.R` | climasus_df, `lang` | ggplot2 | Padrão de dados faltantes e preenchimento |
| `sus_climate_plot_heatwaves()` | `sus_climate_plot_heatwaves.R` | climasus_hw, `lang` | ggplot2 | Timeline e frequência das ondas de calor |

---

## 9. Dados Gradeados — Satélite / Reanálise

| Função | Arquivo | Input | Output | Descrição |
|--------|---------|-------|--------|-----------|
| `sus_grid_chirps()` | `sus_grid_chirps.R` | `bbox`, datas, `lang` | precipitação CHIRPS | Chuva diária 0,05° |
| `sus_grid_era5()` | `sus_grid_era5.R` | `bbox`, `variables`, datas | ERA5 | Temperatura, umidade, vento, radiação |
| `sus_grid_fires()` | `sus_grid_fires.R` | `bbox`, datas | queimadas | Focos de incêndio FIRMS/PRODES |
| `sus_grid_pdsi()` | `sus_grid_pdsi.R` | `bbox`, datas | PDSI | Índice Palmer de Severidade de Seca |
| `sus_grid_smvi()` | `sus_grid_smvi.R` | `bbox`, `indices`, datas | NDVI/NDWI/EVI | Índices de vegetação e umidade do solo |
| `sus_grid_prodes()` | `sus_grid_prodes.R` | `bbox`, `year` | desmatamento | Desmatamento acumulado PRODES/INPE |
| `sus_grid_pollution_cams()` | `sus_grid_pollution_cams.R` | `bbox`, `pollutants`, datas | poluentes CAMS | Qualidade do ar Copernicus |
| `sus_grid_pollution_ghap()` | `sus_grid_pollution_ghap.R` | `bbox`, datas | PM2.5 GHAP | Material particulado fino |
| `sus_grid_pollution_merra2()` | `sus_grid_pollution_merra2.R` | `bbox`, `variables`, datas | aerossóis MERRA-2 | Aerossóis e gases NASA |
| `sus_grid_join()` | `sus_grid_join.R` | health_df, grid_df, `by` | climasus_df integrado | Vincula dados gradeados ao dataset por município × data |

---

## 10. Dados Socioeconômicos — Censo

| Função | Arquivo | Input | Output (stage) | Descrição |
|--------|---------|-------|----------------|-----------|
| `sus_census_select()` | `sus_census_explore.R` | `dataset`, `year`, `lang` | data.frame / vetor | Navega variáveis censitárias disponíveis (censobr) |
| `sus_socio_add_census()` | `sus_socio_add_census.R` | climasus_df, `variables`, `year` | `"census"` | Vincula renda, escolaridade, saneamento ao dataset |

---

## 11. Modelagem Epidemiológica

### 11a. Exposição-Resposta

| Função | Arquivo | Input | Output (S3) | Descrição |
|--------|---------|-------|-------------|-----------|
| `sus_mod_dlnm()` | `sus_mod_dlnm.R` | climasus_df, `outcome_col`, `climate_col`, `lag_max`, `argvar`, `arglag`, `family`, `covariates`, `ref_value`, `alpha`, `lang` | `climasus_dlnm` | DLNM — superfície lag-exposição para associação clima-saúde |
| `sus_mod_casecrossover()` | `sus_mod_casecrossover.R` | climasus_df, `outcome_col`, `exposure_col`, `stratum`, `lag`, `method`, `family`, `alpha`, `lang` | `climasus_casecrossover` | Caso-cruzado com estratificação temporal; Poisson condicional ou clogit |
| `sus_mod_its()` | `sus_mod_its.R` | climasus_df, `outcome_col`, `interruption_dates`, `harmonics`, `family`, `covariates`, `alpha`, `lang` | `climasus_its` | Série temporal interrompida para avaliação de intervenções |
| `sus_mod_excess()` | `sus_mod_excess.R` | climasus_df, `outcome_col`, `control_period`, `study_period`, `method`, `dof_per_year`, `family`, `alpha`, `lang` | `climasus_excess` | Excesso de mortalidade/morbidade via contrafactual spline/Serfling |

### 11b. Análise de Impacto

| Função | Arquivo | Input | Output (S3) | Descrição |
|--------|---------|-------|-------------|-----------|
| `sus_mod_af()` | `sus_mod_af.R` | `climasus_dlnm`, `threshold`, `range`, `by`, `nsim`, `alpha`, `lang` | `climasus_af` | Fração e número atribuível ao calor/frio a partir do DLNM |
| `sus_mod_burden()` | `sus_mod_burden.R` | lista de `climasus_dlnm`, `component`, `rank_by`, `top_n`, `nsim`, `alpha`, `lang` | `climasus_burden` | Carga de doença e ranking de cidades; curvas de concentração |

### 11c. Pooling Multi-cidade

| Função | Arquivo | Input | Output (S3) | Descrição |
|--------|---------|-------|-------------|-----------|
| `sus_mod_pool()` | `sus_mod_pool.R` | lista de `climasus_dlnm`, `exposure_range`, `n_grid`, `blup`, `method`, `lang` | `climasus_pool` | Pooling dois estágios via mvmeta |
| `sus_mod_metaregression()` | `sus_mod_metaregression.R` | lista de `climasus_dlnm`, `covariates`, `covariate_cols`, `blup`, `method`, `lang` | `climasus_metaregression` | Meta-regressão com covariáveis de cidade; BLUPs preditivos |

### 11d. Machine Learning e Sensibilidade

| Função | Arquivo | Input | Output (S3) | Descrição |
|--------|---------|-------|-------------|-----------|
| `sus_mod_ml()` | `sus_mod_ml.R` | climasus_df, `outcome_col`, `feature_cols`, `id_col`, `objective`, `nrounds`, `seed`, `lang` | `climasus_ml` | XGBoost para predição de desfechos em saúde |
| `sus_mod_sensitivity()` | `sus_mod_sensitivity.R` | lista de `climasus_dlnm`, `hot_percentile`, `cold_percentile`, `stratum_labels`, `alpha`, `lang` | `climasus_sensitivity` | Comparação de RR por estratos de vulnerabilidade |

---

## 12. Epidemiologia Espacial e Espaço-Temporal

| Função | Arquivo | Input | Output | Descrição |
|--------|---------|-------|--------|-----------|
| `sus_mod_spatial_weights()` | `sus_mod_spatial_weights.R` | sf (climasus_df), `style`, `order` | pesos espaciais | Matriz de contiguidade/distância |
| `sus_mod_spatial_moran()` | `sus_mod_spatial_moran.R` | climasus_df (spatial), weights, `var_col` | I de Moran global e LISA | Autocorrelação espacial |
| `sus_mod_spatial_scan()` | `sus_mod_spatial_scan.R` | climasus_df, `outcome_col`, `pop_col`, `method` | clusters | Estatística de varredura espacial |
| `sus_mod_spatial_bayes()` | `sus_mod_spatial_bayes.R` | climasus_df (spatial), `outcome_col`, `model` | suavização Bayesiana | Modelos BYM / ICAR |
| `sus_mod_spatial_reg()` | `sus_mod_spatial_reg.R` | climasus_df (spatial), `formula`, `type` | SAR / SEM / SDM | Regressão espacial |
| `sus_mod_spacetime_bayes()` | `sus_mod_spacetime_bayes.R` | climasus_df (spatial+temporal), `model` | INLA / Stan | Modelo Bayesiano espaço-temporal |
| `sus_mod_spacetime_exceedance()` | `sus_mod_spacetime_exceedance.R` | `climasus_spacetime_bayes`, `threshold` | probabilidades | Excedências espaço-temporais |
| `sus_mod_spacetime_predict()` | `sus_mod_spacetime_predict.R` | `climasus_spacetime_bayes`, `newdata` | predições | Predições espaço-temporais |
| `sus_mod_vulnerability_index()` | `sus_mod_vulnerability_index.R` | climasus_df, `exposure_cols`, `sensitivity_cols`, `adaptive_cols`, `method` | índice composto | Índice de Vulnerabilidade Climática |
| `sus_mod_swot()` | `sus_mod_swot.R` | climasus_df, `dimensions`, `lang` | `climasus_swot` | Análise SWOT do sistema de saúde frente ao clima |

---

## 13. Visualização de Modelos

| Função | Arquivo | Input | Output | Descrição |
|--------|---------|-------|--------|-----------|
| `sus_mod_plot_dlnm()` | `sus_mod_plot_dlnm.R` | `climasus_dlnm`, `type` (surface/exposure/lag) | ggplot2 / plotly | Curva exposição-resposta, lag-resposta, superfície 3D |
| `sus_mod_plot_af()` | `sus_mod_plot_af.R` | `climasus_af` | ggplot2 | Fração atribuível por período e limiar |
| `sus_mod_plot_burden()` | `sus_mod_plot_burden.R` | `climasus_burden` | ggplot2 | Ranking de carga de doença; curva de Lorenz |
| `sus_mod_plot_pool()` | `sus_mod_plot_pool.R` | `climasus_pool`, `type` | ggplot2 | Curvas pooled + BLUPs; forest plot |
| `sus_mod_plot_ml()` | `sus_mod_plot_ml.R` | `climasus_ml`, `type` (importance/shap/pred) | ggplot2 | Importância de variáveis e SHAP values |
| `sus_mod_plot_sensitivity()` | `sus_mod_plot_sensitivity.R` | `climasus_sensitivity` | ggplot2 | Heatmap de RR por estratos de vulnerabilidade |
| `sus_mod_plot_spatial_bayes()` | `sus_mod_plot_spatial_bayes.R` | `climasus_spatial_bayes` | ggplot2 / tmap | Mapas suavizados Bayesianos (SIR, risco relativo) |
| `sus_mod_plot_spatial_moran()` | `sus_mod_plot_spatial_moran.R` | `climasus_moran` | ggplot2 | Moran scatterplot e LISA map |
| `sus_mod_plot_spatial_scan()` | `sus_mod_plot_spatial_scan.R` | `climasus_scan` | ggplot2 / tmap | Mapa de clusters detectados |
| `sus_mod_plot_spacetime()` | `sus_mod_plot_spacetime.R` | `climasus_spacetime_bayes`, `type` | ggplot2 / animação | Animação espaço-temporal / facet-map |
| `sus_mod_plot_vulnerability()` | `sus_mod_plot_vulnerability.R` | climasus_df (vulnerability) | ggplot2 / tmap | Mapas e perfis de vulnerabilidade |
| `sus_mod_plot_swot()` | `sus_mod_plot_swot.R` | `climasus_swot` | ggplot2 | Quadrante SWOT |

---

## 14. Infraestrutura e Utilitários

| Função | Arquivo | Input | Output | Descrição |
|--------|---------|-------|--------|-----------|
| `sus_as_arrow()` | `utils-S3.R` | climasus_df | Arrow Table (lazy) | Converte para backend Arrow/Parquet |
| `sus_as_duckdb()` | `utils-S3.R` | climasus_df | DuckDB relation | Converte para backend DuckDB |
| `write_parquet_climasus()` | `sus_data_export.R` | climasus_df, `path` | arquivo .parquet | Serializa com metadados embutidos no schema Arrow |
| `write_duckdb_climasus()` | `sus_data_export.R` | climasus_df, `con`, `name` | tabela DuckDB | Persiste com tabela companion de metadados |
| `sus_cache_clear()` | `utils.R` | — | logical | Limpa cache global de downloads |
| `sus_cache_info()` | `utils.R` | — | lista | Estatísticas de uso do cache |
| `sus_install_deps()` | `utils.R` | `packages`, `lang` | logical | Instala dependências opcionais (sf, arrow, censobr…) |
| `%>%` | `utils-pipe.R` | — | objeto encadeado | Re-exportação do pipe magrittr |

---

## Resumo por Estágio do Pipeline

| # | Estágio | Funções-chave | Propósito |
|---|---------|---------------|-----------|
| 1 | **Importação** | `sus_data_import`, `sus_data_read` | Baixar/ler dados SUS brutos |
| 2 | **Limpeza** | `sus_data_clean_encoding`, `sus_data_standardize` | Preparar dados |
| 3 | **Filtragem** | `sus_data_filter_cid`, `sus_data_filter_demographics` | Selecionar subconjuntos |
| 4 | **Derivação** | `sus_data_create_variables`, `sus_data_aggregate` | Calcular variáveis e agregar |
| 5 | **Espacial** | `sus_spatial_join` | Vincular a geometrias |
| 6 | **Clima (Estações)** | `sus_climate_inmet`, `sus_climate_aggregate`, `sus_climate_compute_*` | Integrar INMET |
| 7 | **Clima (Grade)** | `sus_grid_chirps`, `sus_grid_era5`, `sus_grid_pollution_*` | Integrar satélite/reanálise |
| 8 | **Socioeconômico** | `sus_socio_add_census` | Vincular censo |
| 9 | **Modelagem** | `sus_mod_dlnm`, `sus_mod_casecrossover`, `sus_mod_ml`, `sus_mod_spatial_*` | Análise epidemiológica |
| 10 | **Visualização** | `sus_mod_plot_*`, `sus_data_plot_*`, `sus_climate_plot_*` | Gráficos e mapas |
| 11 | **Exportação** | `sus_data_export`, `write_parquet_climasus` | Persistir resultados |

---

## Financiamento

O projeto climasus4r é financiado pelo Ministério da Saúde, pela Fundação Oswaldo Cruz Rondônia (FIOCRUZ-RO / CCSRO) e pelo **Instituto Nacional de Ciência e Tecnologia de Pesquisa e Conhecimento de Excelência da Amazônia Ocidental - INCT-CONEXAO**. O fomento abrange:

* **Núcleo Permanente de Climatologia (NPC)** INCT-CONEXÃO
* **Núcleo Avançado de Climatologia (NAC)** INCT-CONEXÃO


---

## Agradecimentos

- Equipe do **microdatasus** pela infraestrutura fundamental de importação de dados
- **DATASUS** por fornecer acesso aberto aos dados de saúde brasileiros
- **INCT Conexão - Amazônia** pelo financiamento e suporte
- Todos os colaboradores e testadores que ajudaram a melhorar o pacote


---

## Inspiração

- Pacote R [`microdatasus`](https://github.com/rfsaldanha/microdatasus)

---

## Contato

- **Mantenedor**: Max Anjos
- **Email**: maxanjos@campus.ul.pt
- **GitHub**: [https://github.com/ByMaxAnjos/climasus4r](https://github.com/ByMaxAnjos/climasus4r)
- **Issues**: [https://github.com/ByMaxAnjos/climasus4r/issues](https://github.com/ByMaxAnjos/climasus4r/issues)
