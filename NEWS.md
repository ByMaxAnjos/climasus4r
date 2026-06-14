# climasus4r 1.0.0

First stable release. The package now covers the full health–climate–environment data pipeline
for Brazil, from raw DATASUS import through epidemiological modelling.

## Pipeline de Dados de Saúde

* `sus_data_import()` — importa dados do DATASUS (SIM, SIH, SINAN, SIA, CNES, SINASC) com
  suporte a múltiplos estados e anos, cache automático e processamento paralelo via `future`/`furrr`
* `sus_data_read()` — lê arquivos `.dbc` locais sem depender de download
* `sus_data_clean_encoding()` — detecta e corrige problemas de codificação Latin1/UTF-8 em campos
  de texto brasileiros
* `sus_data_standardize()` — padroniza nomes de colunas (80+ variáveis) e valores categóricos
  (sexo, raça, estado civil, escolaridade) com saída multilíngue (pt/en/es)
* `sus_data_filter_cid()` — filtragem flexível por CID-10: código exato, prefixo, intervalo ou
  capítulo completo
* `sus_data_filter_demographics()` — filtragem por variáveis demográficas (faixa etária, sexo,
  raça, município de residência)
* `sus_create_variables()` — criação de variáveis derivadas (idade em anos, faixa etária,
  sazonalidade, indicadores de completitude)
* `sus_data_aggregate()` — agrega registros individuais em contagens temporais por município,
  estado ou região, com múltiplas estratégias (semanal, mensal, anual)
* `sus_data_export()` — exporta `climasus_df` para CSV, Parquet ou DuckDB preservando metadados
* `sus_data_quality_report()` — gera relatório de completitude, duplicatas e inconsistências
* `sus_filter_cid_explore()` — ferramenta interativa para explorar a tabela CID-10

## Visualização de Dados

* `sus_data_plot_aggregate_ts()` — série temporal de eventos agregados com bandas de confiança
* `sus_data_plot_aggregate_map()` — mapa coroplético municipal/estadual de contagens ou taxas
* `sus_data_plot_demographics()` — pirâmide etária e distribuições demográficas

## Dados Climáticos de Estações (INMET)

* `sus_climate_inmet()` — importa dados horáros e diários de estações meteorológicas INMET,
  com 4.600+ estações e metadados embutidos em `inst/data_4r/station_meta.parquet`
* `sus_climate_fill_gap()` — preenche lacunas de dados com interpolação temporal, média histórica
  ou krigagem espacial entre estações vizinhas
* `sus_climate_aggregate()` — vincula dados climáticos de estações aos registros de saúde por
  município usando 10 estratégias de agregação temporal (exact, moving_window, distributed_lag,
  degree_days, threshold_exceedance, cold_wave_exceedance e outras)
* `sus_climate_anomaly()` — calcula anomalias em relação às normais climatológicas
* `sus_climate_normals()` — recupera normais climatológicas WMO 1991–2020
* `sus_climate_compute_heatwaves()` — detecta ondas de calor com critérios configuráveis
  (Excalibur, HeatWatch, Perkins, EHF)
* `sus_climate_compute_indicators()` — computa índices de extremos climáticos (ETCCDIs)
* `sus_climate_compute_spei()` — índice padronizado de precipitação-evapotranspiração (SPEI)
* `sus_climate_compute_spi()` — índice padronizado de precipitação (SPI)
* `sus_climate_uniplu()` — unifica séries pluviométricas de múltiplas estações por município
* `sus_climate_plot_aggregate()` — visualiza distribuição climática vinculada aos dados de saúde
* `sus_climate_plot_fill()` — diagnóstico visual do preenchimento de lacunas
* `sus_climate_plot_heatwaves()` — visualiza ondas de calor detectadas

## Dados Climáticos em Grade

* `sus_grid_era5()` — extrai variáveis ERA5 (temperatura, vento, umidade, precipitação) para
  municípios brasileiros via `exactextractr`
* `sus_grid_chirps()` — precipitação diária CHIRPS v2.0
* `sus_grid_fires()` — focos de incêndio FIRMS/INPE
* `sus_grid_pdsi()` — índice de seca Palmer (PDSI)
* `sus_grid_pollution_cams()` — qualidade do ar CAMS (PM2.5, PM10, NO2, O3, SO2)
* `sus_grid_pollution_ghap()` — PM2.5 global GHAP
* `sus_grid_pollution_merra2()` — PM2.5 MERRA-2 NASA
* `sus_grid_prodes()` — desmatamento PRODES/INPE
* `sus_grid_smvi()` — índice de vegetação SMVI/MapBiomas
* `sus_grid_join()` — integra múltiplas grades à tabela de saúde em um único passo

## Junção Espacial

* `sus_join_spatial()` — vincula registros de saúde a polígonos municipais/estaduais via `geobr`,
  com suporte a joins por código IBGE ou coordenadas

## Dados Socioeconômicos

* `sus_socio_add_census()` — integra indicadores do Censo IBGE 2010/2022 via `censobr`
* `sus_socio_compute_indicators()` — calcula índices compostos de vulnerabilidade socioeconômica
  (IDH-M, Gini, renda per capita, cobertura de saneamento)
* `sus_census_explore()` — ferramenta exploratória para navegar variáveis censitárias disponíveis

## Modelagem Epidemiológica

* `sus_mod_dlnm()` — modelos de defasagem distribuída não-linear (DLNM) com bases de splines
  naturais, suporte a múltiplos poluentes/variáveis e seleção de defasagem automática
* `sus_mod_af()` — fração e número atribuível (AF/AN) com intervalos de confiança por bootstrap
* `sus_mod_burden.R()` — carga de doença atribuível ao clima com decomposição por temperatura
  (frio, moderado, calor) e incerteza propagada
* `sus_mod_casecrossover()` — design de caso-cruzado para exposições agudas com estratificação
  temporal customizável
* `sus_mod_excess()` — mortalidade em excesso por método quasi-Poisson ou ARIMA
* `sus_mod_its()` — análise de séries temporais interrompidas (ITS) para avaliação de intervenções
* `sus_mod_pool()` — pooling de estimativas multi-cidade via meta-análise de efeitos fixos/aleatórios
* `sus_mod_metaregression()` — meta-regressão para modificação de efeito por covariáveis municipais
* `sus_mod_sensitivity()` — análise de sensibilidade de escolhas metodológicas (defasagem, nós,
  referência de temperatura)
* `sus_mod_ml()` — modelos de machine learning (XGBoost, Random Forest) para predição de desfechos
* `sus_mod_swot()` — análise de risco por quintis de exposição (SWOT epidemiológico)
* `sus_mod_vulnerability_index()` — índice de vulnerabilidade climático-epidemiológica composto

## Análise Espacial

* `sus_mod_spatial_bayes()` — suavização Bayesiana espacial (BYM2) via `CARBayes`
* `sus_mod_spatial_moran()` — autocorrelação espacial global e local de Moran
* `sus_mod_spatial_reg()` — regressão espacial (SLM, SEM, SDM) via `spatialreg`
* `sus_mod_spatial_scan()` — detecção de clusters espaciais via scan statistics (`SpatialEpi`)
* `sus_mod_spatial_weights()` — construção de matrizes de pesos espaciais (contiguidade, distância,
  k-vizinhos) com verificação de conectividade
* `sus_mod_spacetime_bayes()` — modelos espaço-temporais Bayesianos com estrutura AR(1)
* `sus_mod_spacetime_exceedance()` — probabilidade de excedência espaço-temporal com mapa de risco
* `sus_mod_spacetime_predict()` — predição espaço-temporal com intervalos de predição

## Visualização de Modelos

* `sus_mod_plot_dlnm()` — superfície 3D e curvas de resposta-defasagem do DLNM
* `sus_mod_plot_af()` — gráficos de fração atribuível por faixa de temperatura
* `sus_mod_plot_burden()` — decomposição da carga de doença em barras empilhadas
* `sus_mod_plot_pool()` — forest plot de estimativas multi-cidade
* `sus_mod_plot_sensitivity()` — heatmap de sensibilidade metodológica
* `sus_mod_plot_ml()` — importância de variáveis e curvas de aprendizado (ML)
* `sus_mod_plot_spacetime()` — mapas animados e facetados de risco espaço-temporal
* `sus_mod_plot_spatial_bayes()` — mapas de risco relativo e incerteza Bayesiana
* `sus_mod_plot_spatial_moran()` — LISA cluster map e Moran scatterplot
* `sus_mod_plot_spatial_scan()` — mapa de clusters significativos
* `sus_mod_plot_swot()` — gráfico de quartis de risco SWOT
* `sus_mod_plot_vulnerability()` — mapa coroplético do índice de vulnerabilidade

## Infraestrutura

* Classe S3 `climasus_df` com atributo `sus_meta` para rastreabilidade de metadados ao longo
  do pipeline (sistema, estágio, backend, histórico de processamento)
* Persistência de metadados em Parquet (schema Arrow) e DuckDB (tabela companion `__meta`)
* Suporte a backends tibble, Parquet e DuckDB com conversão transparente
* Sistema i18n completo (português, inglês, espanhol) para nomes de colunas e mensagens
* Processamento paralelo via `future`/`furrr` respeitando o plano do usuário
