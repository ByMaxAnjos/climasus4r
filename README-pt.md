# climasus4r:: Análises Espaço-temporal Integradas de Saúde, Clima e Ambiente no Brasil

<img src="https://github.com/ByMaxAnjos/climasus4r/blob/master/inst/figures/logo.png?raw=true" 
     alt="climasus4r Logo" 
     width="140" 
     style="float: right; margin-left: 10px;">

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test coverage](https://codecov.io/gh/ByMaxAnjos/climasus4r/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ByMaxAnjos/climasus4r)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen.svg)](https://github.com/ByMaxAnjos/climasus4r)
<!-- badges: end -->

---

O **climasus4r** é um pacote integrado de ferramentas em R desenvolvido para otimizar a análise de dados de saúde, clima e ambiente no Brasil. O pacote foi concebido no âmbito do projeto INCT Conexão – Amazônia e tem como objetivo automatizar e padronizar etapas críticas do fluxo de trabalho em pesquisas epidemiológicas e ambientais, promovendo reprodutibilidade, eficiência e escalabilidade.

Baseado no sólido ecossistema do pacote [`microdatasus`](https://github.com/rfsaldanha/microdatasus), o climasus4r expande suas funcionalidades ao incorporar rotinas especializadas para estudos em clima e saúde, reduzindo significativamente o esforço necessário para aquisição, limpeza, integração e preparação dos dados.

## 🌐 Outros Idiomas

- [English](../en/index.html) | [Español](../es/index.html)

## Instalação

O **climasus4r** encontra-se atualmente em desenvolvimento ativo. A versão mais recente pode ser instalada diretamente a partir do GitHub, garantindo acesso às funcionalidades mais atualizadas. Antes da instalação, é necessário ter o pacote remotes, que permite a instalação de pacotes hospedados no GitHub.

```r
# Instale o remotes se ainda não o tiver
if (!require("remotes")) {
  install.packages("remotes")
}

# Instale o CLIMASUS4r
remotes::install_github("ByMaxAnjos/climasus4r", dependencies = TRUE, upgrade = "never")
```

## Início Rápido

```r
library(climasus4r)
library(dplyr)

# Pipeline completo: Dados prontos para análise
df_analise <- sus_data_import(
  uf = "SP",
  year = 2023,
  system = "SIM-DO"
) |>
  sus_data_clean_encoding(lang = "pt") |>
  sus_data_standardize(lang = "pt") |>
  sus_data_filter_cid(disease_group = "respiratory", lang = "pt") |>
  sus_create_variables(create_age_groups = TRUE, lang = "pt")
```

## Infraestrutura de Dados 

A Fase 1 do **climasus4r** fornece um pipeline end-to-end completo para preparação de dados de saúde, desde a aquisição bruta até dados prontos para análise. Com 9 funções principais, você pode transformar dados do DATASUS em séries temporais agregadas, padronizadas e prontas para modelagem em minutos.

```r
DADOS BRUTOS (DATASUS)
    ↓
[1] sus_data_import()           → Aquisição paralela
    ↓
[2] sus_data_clean_encoding()   → Correção de encoding
    ↓
[3] sus_data_standardize()      → Padronização multilíngue
    ↓
[4] sus_data_filter_cid()       → Filtragem por doença
    ↓
[5] sus_create_variables()      → Criação de variáveis
    ↓
[6] sus_data_filter_demographics() → Filtragem demográfica
    ↓
[7] sus_data_quality_report()   → Verificação de qualidade
    ↓
[8] sus_data_aggregate()        → Agregação temporal
    ↓
[9] sus_data_export()           → Exportação com metadados
    ↓
DADOS PRONTOS PARA ANÁLISE
```
Para mais informações, consulte os [Tutoriais](articles/tutorials.html) e a [Documentação Completa](reference/index.html).
