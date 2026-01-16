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

A fase de infraestrutura do **climasus4r** fornece um pipeline end-to-end completo para preparação de dados de saúde, desde a aquisição bruta até dados prontos para análise. Com 9 funções principais, você pode transformar dados do DATASUS em séries temporais agregadas, padronizadas e prontas para modelagem em minutos.

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

## Sistemas Suportados

O **climasus4r** permite o acesso simplificado e padronizado aos principais sistemas de informação do DATASUS por meio da integração com o pacote **microdatasus**. Essa integração automatiza a coleta de dados brutos de diferentes bases do sistema de saúde brasileiro, abrangendo informações de epidemiologia, mortalidade, internações hospitalares e rede assistencial. A partir desses dados, o climasus4r organiza, limpa e estrutura as informações, transformando bases complexas do DATASUS em conjuntos de dados prontos para análise estatística e estudos espaço-temporais.

#### **1. SIM (Sistema de Informação sobre Mortalidade)**
* `"SIM-DO"`: Declarações de Óbito (Dataset completo)
* `"SIM-DOFET"`: Óbitos Fetais
* `"SIM-DOEXT"`: Óbitos por Causas Externas
* `"SIM-DOINF"`: Óbitos Infantis
* `"SIM-DOMAT"`: Óbitos Maternos

#### **2. SIH (Sistema de Informação Hospitalar)**
* `"SIH-RD"`: AIH (Autorizações de Internação Hospitalar) - Geral
* `"SIH-RJ"`: AIH - Específico para o Rio de Janeiro
* `"SIH-SP"`: AIH - Específico para São Paulo
* `"SIH-ER"`: Prontuários de Emergência

#### **3. SINAN (Sistema de Informação de Agravos de Notificação)**
* `"SINAN-DENGUE"`: Casos de Dengue
* `"SINAN-CHIKUNGUNYA"`: Casos de Chikungunya
* `"SINAN-ZIKA"`: Casos de Zika vírus
* `"SINAN-MALARIA"`: Casos de Malária
* `"SINAN-CHAGAS"`: Casos de Doença de Chagas
* `"SINAN-LEISHMANIOSE-VISCERAL"`: Leishmaniose Visceral
* `"SINAN-LEISHMANIOSE-TEGUMENTAR"`: Leishmaniose Tegumentar
* `"SINAN-LEPTOSPIROSE"`: Casos de Leptospirose

#### **4. SIA (Sistema de Informação Ambulatorial)**
* `"SIA-AB"`: Atenção Básica
* `"SIA-ABO"`: Procedimentos Odontológicos
* `"SIA-ACF"`: Assistência Farmacêutica
* `"SIA-AD"`: Alta Complexidade/Diferenciada
* `"SIA-AN"`: Atenção Domiciliar
* `"SIA-AM"`: Ambulatório de Especialidades
* `"SIA-AQ"`: Ações Estratégicas
* `"SIA-AR"`: Regulação
* `"SIA-ATD"`: Urgência/Emergência
* `"SIA-PA"`: Procedimentos Ambulatoriais em Hospital
* `"SIA-PS"`: Atenção Psicossocial
* `"SIA-SAD"`: Atenção Especializada

#### **5. CNES (Cadastro Nacional de Estabelecimentos de Saúde)**
* `"CNES-LT"`: Leitos
* `"CNES-ST"`: Profissionais de Saúde
* `"CNES-DC"`: Equipamentos (Detalhado)
* `"CNES-EQ"`: Equipamentos (Resumo)
* `"CNES-SR"`: Serviços Especializados
* `"CNES-HB"`: Leitos Hospitalares
* `"CNES-PF"`: Pessoal Físico (Profissionais)
* `"CNES-EP"`: Participantes do Ensino
* `"CNES-RC"`: Classificação Hospitalar
* `"CNES-IN"`: Indicadores Hospitalares
* `"CNES-EE"`: Entidades de Ensino
* `"CNES-EF"`: Instalações de Ensino
* `"CNES-GM"`: Gestão e Apoio

#### **6. SINASC (Sistema de Informação sobre Nascidos Vivos)**
* `"SINASC"`: Declarações de Nascidos Vivos

Para mais informações, consulte os [Tutoriais](articles/tutorials.html) e a [Documentação Completa](reference/index.html).
