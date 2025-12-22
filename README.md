# climasus4r: Kit de Ferramentas Integrado para Análise de Dados de Saúde, Clima e Ambiente

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test coverage](https://codecov.io/gh/ByMaxAnjos/climasus4r/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ByMaxAnjos/climasus4r)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen.svg)](https://github.com/ByMaxAnjos/climasus4r)
<!-- badges: end -->

## Visão Geral

O **climasus4r** é um kit de ferramentas integrado em R, projetado para otimizar a análise de dados de saúde (SUS), clima e ambiente no Brasil. Desenvolvido como parte do projeto **INCT Conexão - Amazônia**, este pacote automatiza as etapas mais trabalhosas de aquisição, limpeza, integração e análise de dados, garantindo fluxos de trabalho de pesquisa padronizados e reprodutíveis.

O pacote se baseia no excelente trabalho fornecido pelo [`microdatasus`](https://github.com/rfsaldanha/microdatasus), adicionando funções especializadas para pesquisa em clima e saúde, incluindo:

- **Aquisição de dados em paralelo** de múltiplos estados e anos
- **Suporte multi-sistema** para todos os 6 principais sistemas de saúde brasileiros (SIM, SINASC, SINAN, SIH, SIA, CNES)
- **Correção de codificação aprimorada** para texto em português do Brasil
- **Nomes de colunas e valores padronizados** com traduções multilíngues (EN/PT/ES)
- **Filtragem abrangente da CID-10** com mais de 54 grupos de doenças predefinidos
- **Classificação de doenças sensíveis ao clima** para pesquisa epidemiológica
- **Interface totalmente multilíngue** para colaboração internacional

## Instalação

Atualmente em desenvolvimento. Instale a versão mais recente do GitHub:

```r
# Instale o remotes se ainda não o tiver
if (!require("remotes")) {
  install.packages("remotes")
}

# Instale o CLIMASUS4r
remotes::install_github("ByMaxAnjos/climasus4r", upgrade = "never", quiet = TRUE)

# Atualize com frequência para obter as melhorias mais recentes
remove.packages("climasus4r")
remotes::install_github("ByMaxAnjos/climasus4r", upgrade = "never", quiet = TRUE)
```

## Início Rápido

```r
library(climasus4r)

# Pipeline completo: Importar → Limpar → Padronizar → Filtrar
df_respiratorias <- sus_data_import(
  uf = "SP",
  ano = 2023,
  sistema = "SIM-DO"
) |>
  sus_data_clean_encoding() |>
  sus_data_standardize(lang = "pt") |>
  sus_data_filter_cid(disease_group = "respiratory", lang = "pt")
```

## Fase 1: Infraestrutura de Dados ✅

### Funções Principais

#### 1. `sus_data_import()` - Aquisição de Dados

Importe dados do DATASUS com suporte a **processamento paralelo** para múltiplos estados e anos.

```r
# Um único estado e ano
df <- sus_data_import(uf = "RJ", ano = 2022, sistema = "SIM-DO")

# Múltiplos estados e anos com processamento paralelo
df <- sus_data_import(
  uf = c("RJ", "SP", "MG", "ES"),
  ano = 2018:2022,
  sistema = "SIM-DO",
  parallel = TRUE,
  workers = 4,
  use_cache = TRUE
)
```

**Sistemas Suportados:**
- **SIM** (Mortalidade): `"SIM-DO"`, `"SIM-DOEXT"`, `"SIM-DOFET"`, `"SIM-DOMAT"`
- **SINASC** (Nascidos Vivos): `"SINASC"`
- **SINAN** (Agravos de Notificação): `"SINAN-DENGUE"`, `"SINAN-CHIKUNGUNYA"`, `"SINAN-ZIKA"`, `"SINAN-MALARIA"`, etc.
- **SIH** (Internações Hospitalares): `"SIH-RD"`, `"SIH-SP"`
- **SIA** (Ambulatorial): `"SIA-PA"`, `"SIA-PS"`
- **CNES** (Estabelecimentos de Saúde): `"CNES-ST"`, `"CNES-PF"`

**Recursos:**
- ✅ Cache automático para evitar downloads redundantes
- ✅ Processamento paralelo para aquisição de dados mais rápida
- ✅ Barras de progresso com feedback do `cli`

---

#### 2. `sus_data_clean_encoding()` - Correção de Codificação

Detecte e corrija problemas de codificação de caracteres automaticamente.

```r
df_limpo <- sus_data_clean_encoding(df_bruto, lang = "pt", verbose = TRUE)
```

**O que faz:**
- Verifica todas as colunas de texto em busca de problemas de codificação
- Corrige conflitos comuns entre Latin1 e UTF-8
- Informa quais colunas foram corrigidas
- Atua como uma rede de segurança após o pré-processamento do `microdatasus`
- Suporta mensagens multilíngues (EN/PT/ES)

---

#### 3. `sus_data_standardize()` - Padronização de Dados

Padronize nomes de colunas e valores categóricos com **suporte multilíngue**.

```r
# Padronização em inglês (padrão)
df_en <- sus_data_standardize(df_limpo, lang = "en")

# Padronização em português
df_pt <- sus_data_standardize(df_limpo, lang = "pt")

# Padronização em espanhol
df_es <- sus_data_standardize(df_limpo, lang = "es")
```

**Transformações:**

| Original (DATASUS) | Inglês | Português | Espanhol |
|--------------------|------------------|--------------|----------------|
| `DTOBITO` | `death_date` | `data_obito` | `fecha_muerte` |
| `SEXO` | `sex` | `sexo` | `sexo` |
| `RACACOR` | `race` | `raca` | `raza` |
| `CAUSABAS` | `underlying_cause` | `causa_basica` | `causa_basica` |
| `TIPOBITO` | `death_type` | `tipo_obito` | `tipo_muerte` |

---

#### 4. `sus_data_filter_cid()` - Filtragem CID-10

Filtre dados por códigos CID-10 com **mais de 54 grupos de doenças predefinidos** e opções de correspondência flexíveis.

```r
# Filtrar por grupo de doenças (mais fácil!)
df_respiratorias <- sus_data_filter_cid(
  df,
  disease_group = "respiratory",
  lang = "pt"
)

# Filtrar por códigos CID explícitos
df_cardio <- sus_data_filter_cid(
  df,
  icd_codes = "I00-I99",
  match_type = "starts_with",
  lang = "pt"
)

# Filtrar por códigos específicos
df_iam <- sus_data_filter_cid(
  df,
  icd_codes = c("I21", "I22"),  # Infarto agudo do miocárdio
  match_type = "starts_with",
  lang = "pt"
)
```

**Mais de 54 Grupos de Doenças Predefinidos:**

| Categoria | Grupos | Exemplos | Descrição |
|-------------------------|--------|--------------------------------------------------------------------------------|--------------------------------|
| **Doenças Infecciosas** | 15 | `dengue`, `zika`, `chikungunya`, `malaria`, `tuberculose`, `covid19`, `hidricas`, `vetoriais` | Infecções sensíveis ao clima |
| **Cardiovasculares** | 6 | `cardiovasculares`, `hipertensivas`, `isquemicas_coracao`, `cerebrovasculares`, `insuficiencia_cardiaca` | Condições relacionadas ao calor |
| **Respiratórias** | 6 | `respiratorias`, `respiratorias_agudas`, `respiratorias_cronicas`, `pneumonia`, `asma`, `dpoc` | Impactos da qualidade do ar |
| **Neoplasias** | 2 | `neoplasias`, `neoplasias_malignas` | Carga de câncer |
| **Endócrinas/Metabólicas** | 2 | `diabetes`, `metabolicas` | Doenças crônicas |
| **Causas Externas** | 6 | `causas_externas`, `acidentes`, `violencia`, `acidentes_transporte`, `afogamento`, `exposicao_calor` | Desastres climáticos |
| **Especiais Clima-Saúde** | 4 | `sensiveis_clima_todas`, `relacionadas_calor`, `relacionadas_frio`, `clima_extremo` | Prioridades epidemiológicas |
| **Por Faixa Etária** | 2 | `respiratorias_pediatricas`, `cardiovasculares_idosos` | Populações vulneráveis |
| **Sindrômicos** | 3 | `sindrome_febril`, `sindrome_respiratoria`, `sindrome_diarreica` | Vigilância |

**Listar todos os grupos disponíveis:**
```r
# Listar todos os grupos
list_disease_groups(lang = "pt")

# Listar apenas grupos sensíveis ao clima
list_disease_groups(climate_sensitive_only = TRUE, lang = "pt")

# Obter detalhes sobre um grupo específico
get_disease_group_details("dengue", lang = "pt")
```
---

### Exemplo de Pipeline Completo

```r
library(climasus4r)
library(dplyr)

# Pipeline 1: Doenças respiratórias em português
df_respiratorias_pt <- sus_data_import(
  uf = "SP",
  ano = 2023,
  sistema = "SIM-DO",
  use_cache = TRUE
) |>
  sus_data_clean_encoding(lang = "pt") |>
  sus_data_standardize(lang = "pt") |>
  sus_data_filter_cid(disease_group = "respiratory", lang = "pt")

# Pipeline 2: Doenças cardiovasculares em inglês
df_cardio_en <- sus_data_import(
  uf = c("RJ", "SP", "MG"),
  ano = 2020:2023,
  sistema = "SIM-DO",
  parallel = TRUE
) |>
  sus_data_clean_encoding(lang = "en") |>
  sus_data_standardize(lang = "en") |>
  sus_data_filter_cid(disease_group = "cardiovascular", lang = "en")

# Pipeline 3: Doenças sensíveis ao clima em espanhol
df_clima_es <- sus_data_import(
  uf = "AM",
  ano = 2023,
  sistema = "SIM-DO"
) |>
  sus_data_clean_encoding(lang = "es") |>
  sus_data_standardize(lang = "es") |>
  sus_data_filter_cid(disease_group = "climate_sensitive_all", lang = "es")
```

---

## Roadmap

### Fase 2: Integração Socioeconômica (Em Andamento)
* Vinculação de limites geográficos
* Integração de dados socioeconômicos do IBGE (população, PIB, IDH)
* Operações espaciais ponderadas pela população
* Correspondência de setores censitários

### Fase 3: Integração Ambiental (Planejada)
* Importação de dados meteorológicos do INMET
* Integração de dados de qualidade do ar (CETESB, INPE)
* Processamento de dados de satélite (MODIS, Sentinel)
* Algoritmos de correspondência de exposição

### Fase 4: Análise Espacial (Planejada)
* Suavização espacial bayesiana
* Detecção de clusters espaciais (SaTScan, Kulldorff)
* Indicadores locais de associação espacial (LISA)
* Modelos de regressão espacial

### Fase 5: Análise Temporal e Preditiva (Planejada)
* Modelos não lineares de defasagem distribuída (DLNM)
* Cálculo de fração atribuível
* Decomposição de séries temporais
* Wrappers de previsão de aprendizado de máquina

---

## Financiamento

O projeto climasus4r é financiado pelo Ministério da Saúde e pela Fundação Oswaldo Cruz Rondônia (FIOCRUZ-RO / CCSRO), vinculado ao **Instituto Nacional de Ciência e Tecnologia de Pesquisa e Conhecimento de Excelência da Amazônia Ocidental - INCT-CONEXAO (@inct_conexao)**, processo nº 408474/2024-6. O fomento abrange o:

* **Núcleo Permanente de Climatologia (NPC)** INCT-CONEXÃO
* **Núcleo Avançado de Climatologia (NAC)** INCT-CONEXÃO

---

## Agradecimentos

- Equipe do **microdatasus** pela infraestrutura fundamental de importação de dados
- **DATASUS** por fornecer acesso aberto aos dados de saúde brasileiros

---

## Contato

- **Mantenedor**: Max Anjos
- **Email**: [maxanjos@campus.ul.pt]
- **GitHub**: [https://github.com/ByMaxAnjos/climasus4r](https://github.com/ByMaxAnjos/climasus4r)
- **Issues**: [https://github.com/ByMaxAnjos/climasus4r/issues](https://github.com/ByMaxAnjos/climasus4r/issues)
