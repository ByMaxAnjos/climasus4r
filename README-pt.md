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

---

O [**climasus4r**](https://bymaxanjos.github.io/climasus4r/) é um pacote integrado de ferramentas em R desenvolvido para otimizar a análise de dados de saúde, clima e ambiente no Brasil. O pacote foi concebido no âmbito do projeto INCT Conexão – Amazônia e tem como objetivo automatizar e padronizar etapas críticas do fluxo de trabalho em pesquisas epidemiológicas e ambientais, promovendo reprodutibilidade, eficiência e escalabilidade.

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
remotes::install_github("ByMaxAnjos/climasus4r", upgrade = "never")
```


## 📦 Visão Geral das Funções

| Categoria | Função | Descrição |
| :--- | :--- | :--- |
| **📥 Importação e Exportação** | `sus_data_import()` | Importa e pré-processa dados do DATASUS com cache inteligente. |
| | `sus_data_read()` | Leitura otimizada de dados processados com suporte paralelo. |
| | `sus_data_export()` | Exporta dados processados preservando metadados. |
| **🧹 Limpeza e Padronização** | `sus_data_clean_encoding()` | Detecta e corrige problemas de codificação de caracteres. |
| | `sus_data_standardize()` | Padroniza nomes de colunas e valores dos dados do SUS. |
| | `sus_data_create_variables()` | Cria variáveis derivadas para análise epidemiológica. |
| **🔍 Filtros e Seleção** | `sus_data_filter_cid()` | Filtra por códigos CID-10 ou grupos de doenças (multilíngue). |
| | `sus_data_filter_demographics()` | Filtra dados por variáveis demográficas (idade, sexo, raça). |
| **🗺️ Espacial e Censo** | `sus_spatial_join()` | Vincula dados do SUS às malhas geográficas brasileiras. |
| | `sus_census_join()` | Enriquece dados de saúde com variáveis socioeconômicas do Censo. |
| | `sus_data_aggregate()` | Agrega dados de saúde em séries temporais. |
| **📊 Qualidade e Metadados** | `sus_data_quality_report()` | Gera relatórios detalhados sobre a qualidade dos dados. |
| | `sus_data_cid_select()` | Lista os grupos de doenças disponíveis para filtro. |
| | `sus_census_select()` | Explorador interativo de variáveis do Censo. |
| **⚡ Cache** | `sus_cache_clear()` | Gerencia e limpa o armazenamento local de arquivos. |


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
  sus_data_create_variables(create_age_groups = TRUE, lang = "pt")
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
[5] sus_data_create_variables()      → Criação de variáveis
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

Para mais informações, consulte os [Tutoriais](..pt/articles/tutorials.html) e a [Documentação Completa](..pt/reference/index.html).

## 🌲 Tem feedback ou sugestões?
Você tem alguma ideia de melhoria ou encontrou algum erro? Adoraríamos ouvir você! Clique no botão abaixo para abrir uma nova *issue* no GitHub e compartilhar suas sugestões diretamente conosco.

<a href='https://github.com/ByMaxAnjos/climasus4r/issues/new'>
  <button type="button" class="btn" style="background-color: #2E7D32; color: white; padding: 8px 16px; font-size: 14px; font-weight: bold; border: none; border-radius: 6px; cursor: pointer; transition: background-color 0.3s;">
    Abrir uma issue no repositório GitHub
  </button>
</a>