# climasus4r: Kit de Ferramentas Integrado para Análise de Dados de Saúde, Clima e Ambiente

<img align="right" src="https://github.com/ByMaxAnjos/climasus4r/blob/master/figures/climasus4r_logo.png?raw=true" alt="climasus4r Logo" width="140">


<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test coverage](https://codecov.io/gh/ByMaxAnjos/climasus4r/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ByMaxAnjos/climasus4r)
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen.svg)](https://github.com/ByMaxAnjos/climasus4r)
<!-- badges: end -->


---

O **climasus4r** é um kit de ferramentas integrado em R, projetado para otimizar a análise de dados de saúde (SUS), clima e ambiente no Brasil. Desenvolvido como parte do projeto **INCT Conexão - Amazônia**, este pacote automatiza as etapas mais trabalhosas de aquisição, limpeza, integração e análise de dados, garantindo fluxos de trabalho de pesquisa padronizados e reprodutíveis.

O pacote se baseia no excelente trabalho fornecido pelo [`microdatasus`](https://github.com/rfsaldanha/microdatasus), adicionando funções especializadas para pesquisa em clima e saúde, incluindo:

- **Aquisição de dados em paralelo** de múltiplos estados e anos
- **Suporte multi-sistema** para todos os 6 principais sistemas de saúde brasileiros (SIM, SINASC, SINAN, SIH, SIA, CNES)
- **Correção de codificação aprimorada** para texto em português do Brasil
- **Nomes de colunas e valores padronizados** com traduções multilíngues (EN/PT/ES)
- **Filtragem abrangente da CID-10** com mais de 54 grupos de doenças predefinidos
- **Agregação temporal flexível** com sazonalidade brasileira
- **Criação automática de variáveis** epidemiológicas e temporais
- **Relatórios de qualidade de dados** automatizados
- **Filtragem demográfica avançada** com grupos etários personalizáveis
- **Exportação com metadados** para reprodutibilidade
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
remotes::install_github("ByMaxAnjos/climasus4r", dependencies = TRUE, upgrade = "never", quiet = TRUE)

# Atualize com frequência para obter as melhorias mais recentes
remove.packages("climasus4r")
remotes::install_github("ByMaxAnjos/climasus4r", dependencies = TRUE, upgrade = "never", quiet = TRUE)
```

## Início Rápido

```r
library(climasus4r)
library(dplyr)

# Pipeline completo da Fase 1: Dados prontos para análise em 9 passos
df_analise <- sus_data_import(
  uf = "SP",
  ano = 2023,
  sistema = "SIM-DO"
) |>
  sus_data_clean_encoding(lang = "pt") |>
  sus_data_standardize(lang = "pt") |>
  sus_data_filter_cid(disease_group = "respiratory", lang = "pt") |>
  sus_create_variables(
    age_groups = TRUE,
    calendar_vars = TRUE,
    season_method = "brazilian",
    lang = "pt"
  ) |>
  sus_data_filter_demographics(
    age_range = c(0, 5),  # Crianças menores de 5 anos
    sex = "all",
    lang = "pt"
  ) |>
  sus_data_aggregate(
    time_unit = "month",
    fill_gaps = TRUE,
    lang = "pt"
  ) |>
  sus_data_export(
    path = "dados_respiratorias_pediatricas_sp_2023.csv",
    format = "csv",
    include_metadata = TRUE,
    lang = "pt"
  )
```

---

## Fase 1: Infraestrutura de Dados ✅ COMPLETA

A Fase 1 do `climasus4r` fornece um **pipeline end-to-end completo** para preparação de dados de saúde, desde a aquisição bruta até dados prontos para análise. Com **9 funções principais**, você pode transformar dados do DATASUS em séries temporais agregadas, padronizadas e prontas para modelagem em minutos.

### Arquitetura do Pipeline

```
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

---

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

**Cobertura:**
- 189 colunas traduzidas (todos os sistemas)
- 94 mapeamentos de valores categóricos
- 3 idiomas (EN/PT/ES)

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

#### 5. `sus_create_variables()` - Criação de Variáveis Epidemiológicas

Crie automaticamente variáveis de **idade**, **calendário** e **sazonalidade** essenciais para análises de séries temporais e DLNM.

```r
df_com_vars <- sus_create_variables(
  df,
  age_groups = TRUE,
  age_breaks = c(0, 5, 15, 25, 45, 65, Inf),
  age_labels = c("0-4", "5-14", "15-24", "25-44", "45-64", "65+"),
  calendar_vars = TRUE,
  season_method = "brazilian",  # Estações do Hemisfério Sul
  lang = "pt"
)
```

**Variáveis Criadas:**

| Tipo | Variáveis | Descrição |
|------|-----------|-----------|
| **Idade** | `age_years`, `age_group` | Idade em anos + faixas etárias personalizáveis |
| **Calendário** | `year`, `month`, `day`, `weekday`, `week`, `quarter` | Variáveis temporais para controle de confundidores |
| **Sazonalidade** | `season` | Estações brasileiras (Verão, Outono, Inverno, Primavera) |

**Cálculo Inteligente de Idade (3 Níveis):**

A função usa uma hierarquia de 3 níveis para calcular idade:

1. **TIER 1**: Busca coluna de idade direta (ex: `IDADE`, `age_years`)
2. **TIER 2**: Calcula de datas (PADRÃO OURO): `Data do Evento - Data de Nascimento`
3. **TIER 3**: Decodifica código DATASUS (ex: `4035` → 35 anos, `3024` → 2 anos)

**Sazonalidade Brasileira (Hemisfério Sul):**

```r
# Estações alinhadas com o calendário brasileiro
# Verão:     Dez-Jan-Fev (início: 1º Dezembro)
# Outono:    Mar-Abr-Mai (início: 1º Março)
# Inverno:   Jun-Jul-Ago (início: 1º Junho)
# Primavera: Set-Out-Nov (início: 1º Setembro)
```

**Exemplo:**
```r
# Criar variáveis para análise de dengue (sensível à sazonalidade)
df_dengue <- df |>
  sus_data_filter_cid(disease_group = "dengue", lang = "pt") |>
  sus_create_variables(
    age_groups = TRUE,
    age_breaks = c(0, 15, 60, Inf),
    age_labels = c("Criancas", "Adultos", "Idosos"),
    calendar_vars = TRUE,
    season_method = "brazilian",
    lang = "pt"
  )
```

---

#### 6. `sus_data_filter_demographics()` - Filtragem Demográfica

Filtre dados por **idade**, **sexo** e **raça/cor** com suporte multilíngue.

```r
# Filtrar crianças menores de 5 anos
df_pediatrico <- sus_data_filter_demographics(
  df,
  age_range = c(0, 5),
  sex = "all",
  lang = "pt"
)

# Filtrar idosos do sexo masculino
df_idosos_masc <- sus_data_filter_demographics(
  df,
  age_range = c(65, Inf),
  sex = "male",
  lang = "pt"
)

# Filtrar adultos por raça/cor
df_adultos_pretos <- sus_data_filter_demographics(
  df,
  age_range = c(18, 60),
  sex = "all",
  race = "black",
  lang = "pt"
)
```

**Opções de Filtragem:**

| Parâmetro | Opções | Descrição |
|-----------|--------|-----------|
| `age_range` | `c(min, max)` | Intervalo de idade em anos (use `Inf` para sem limite) |
| `sex` | `"male"`, `"female"`, `"all"` | Sexo biológico |
| `race` | `"white"`, `"black"`, `"brown"`, `"yellow"`, `"indigenous"`, `"all"` | Raça/cor (IBGE) |

---

#### 7. `sus_data_quality_report()` - Relatório de Qualidade de Dados

Gere relatórios abrangentes de qualidade de dados com **verificações automáticas** e **visualizações**.

```r
# Gerar relatório completo
relatorio <- sus_data_quality_report(
  df,
  output_format = "html",
  output_path = "relatorio_qualidade.html",
  lang = "pt"
)

# Apenas verificações (sem salvar)
verificacoes <- sus_data_quality_report(
  df,
  output_format = "console",
  lang = "pt"
)
```

**Verificações Incluídas:**

| Categoria | Verificações | Descrição |
|-----------|--------------|-----------|
| **Completude** | % valores ausentes por coluna | Identifica colunas problemáticas |
| **Consistência** | Datas inválidas, idades negativas | Detecta erros lógicos |
| **Duplicatas** | Registros duplicados | Identifica possíveis erros de importação |
| **Distribuições** | Histogramas, tabelas de frequência | Visualiza padrões nos dados |
| **Cobertura Temporal** | Gaps na série temporal | Identifica períodos faltantes |

**Formatos de Saída:**
- `"console"`: Imprime no console
- `"html"`: Relatório HTML interativo
- `"pdf"`: Relatório PDF para publicação
- `"json"`: Dados estruturados para processamento

---

#### 8. `sus_data_aggregate()` - Agregação Temporal

Agregue dados em séries temporais com **flexibilidade temporal máxima** e **preenchimento automático de gaps**.

```r
# Agregação mensal (padrão)
df_mensal <- sus_data_aggregate(
  df,
  time_unit = "month",
  date_col = "death_date",
  fill_gaps = TRUE,
  lang = "pt"
)

# Agregação semanal
df_semanal <- sus_data_aggregate(
  df,
  time_unit = "week",
  fill_gaps = TRUE,
  lang = "pt"
)

# Agregação sazonal (estações brasileiras)
df_sazonal <- sus_data_aggregate(
  df,
  time_unit = "season",
  fill_gaps = TRUE,
  lang = "pt"
)

# Agregação por pentads (5 dias) para ondas de calor
df_pentads <- sus_data_aggregate(
  df,
  time_unit = "5 days",
  fill_gaps = TRUE,
  lang = "pt"
)
```

**Unidades Temporais Suportadas:**

| Unidade | Sintaxe | Uso Epidemiológico |
|---------|---------|-------------------|
| **Diária** | `"day"` | DLNM, análises de curto prazo |
| **Pentads** | `"5 days"` | Ondas de calor (efeito cumulativo) |
| **Semanal** | `"week"` | Padrão epidemiológico |
| **Quinzenal** | `"2 weeks"` | Malária, doenças com incubação longa |
| **Mensal** | `"month"` | Tendências, padrões sazonais |
| **Trimestral** | `"quarter"` ou `"3 months"` | Relatórios SUS |
| **Semestral** | `"6 months"` | Avaliação de políticas |
| **Anual** | `"year"` | Mudanças climáticas de longo prazo |
| **Sazonal** | `"season"` | Dengue, Influenza (estações brasileiras) |

**Recursos:**
- ✅ Preenchimento automático de gaps com zeros
- ✅ Sazonalidade brasileira (Hemisfério Sul)
- ✅ Agregação por múltiplos grupos (ex: por município)
- ✅ Suporte a qualquer unidade temporal via `lubridate`

**Exemplo Avançado:**
```r
# Agregação mensal por município e faixa etária
df_agregado <- df |>
  sus_create_variables(age_groups = TRUE, lang = "pt") |>
  sus_data_aggregate(
    time_unit = "month",
    group_by = c("municipality_code", "age_group"),
    fill_gaps = TRUE,
    lang = "pt"
  )
```

---

#### 9. `sus_data_export()` - Exportação com Metadados

Exporte dados com **metadados completos** para garantir reprodutibilidade.

```r
# Exportar como CSV com metadados
sus_data_export(
  df,
  path = "dados_analise.csv",
  format = "csv",
  include_metadata = TRUE,
  lang = "pt"
)

# Exportar como RDS (formato R nativo)
sus_data_export(
  df,
  path = "dados_analise.rds",
  format = "rds",
  include_metadata = TRUE,
  lang = "pt"
)

# Exportar como Parquet (formato eficiente)
sus_data_export(
  df,
  path = "dados_analise.parquet",
  format = "parquet",
  include_metadata = TRUE,
  lang = "pt"
)
```

**Metadados Incluídos:**

| Informação | Descrição |
|------------|-----------|
| **Data de Criação** | Timestamp da exportação |
| **Versão do Pacote** | Versão do `climasus4r` usada |
| **Pipeline Aplicado** | Funções e parâmetros usados |
| **Sistema de Saúde** | SIM, SINASC, SINAN, etc. |
| **Período Temporal** | Anos e UFs incluídos |
| **Transformações** | Filtros e agregações aplicados |
| **Dicionário de Dados** | Descrição de cada coluna |

**Formatos Suportados:**
- `"csv"`: Texto delimitado (universal)
- `"rds"`: Formato R nativo (preserva tipos)
- `"parquet"`: Formato colunar eficiente (recomendado para big data)
- `"xlsx"`: Excel (para compartilhamento com não-programadores)

---

### Exemplos de Pipelines Completos

#### Pipeline 1: Análise de Doenças Respiratórias Pediátricas

```r
library(climasus4r)
library(dplyr)

# Preparar dados de doenças respiratórias em crianças < 5 anos
df_resp_ped <- sus_data_import(
  uf = c("SP", "RJ", "MG"),
  ano = 2018:2023,
  sistema = "SIM-DO",
  parallel = TRUE
) |>
  sus_data_clean_encoding(lang = "pt") |>
  sus_data_standardize(lang = "pt") |>
  sus_data_filter_cid(disease_group = "respiratory", lang = "pt") |>
  sus_create_variables(
    age_groups = TRUE,
    age_breaks = c(0, 1, 5, Inf),
    age_labels = c("< 1 ano", "1-4 anos", "5+ anos"),
    calendar_vars = TRUE,
    season_method = "brazilian",
    lang = "pt"
  ) |>
  sus_data_filter_demographics(
    age_range = c(0, 5),
    sex = "all",
    lang = "pt"
  ) |>
  sus_data_aggregate(
    time_unit = "month",
    group_by = "age_group",
    fill_gaps = TRUE,
    lang = "pt"
  ) |>
  sus_data_export(
    path = "respiratorias_pediatricas_sudeste_2018_2023.csv",
    format = "csv",
    include_metadata = TRUE,
    lang = "pt"
  )
```

#### Pipeline 2: Análise de Dengue com Sazonalidade

```r
# Preparar dados de dengue com foco em sazonalidade
df_dengue <- sus_data_import(
  uf = "AM",
  ano = 2015:2023,
  sistema = "SINAN-DENGUE"
) |>
  sus_data_clean_encoding(lang = "pt") |>
  sus_data_standardize(lang = "pt") |>
  sus_create_variables(
    age_groups = TRUE,
    calendar_vars = TRUE,
    season_method = "brazilian",  # Estações do Hemisfério Sul
    lang = "pt"
  ) |>
  sus_data_aggregate(
    time_unit = "season",  # Agregação por estação
    fill_gaps = TRUE,
    lang = "pt"
  ) |>
  sus_data_export(
    path = "dengue_sazonal_amazonas_2015_2023.csv",
    format = "csv",
    include_metadata = TRUE,
    lang = "pt"
  )
```

#### Pipeline 3: Análise de Mortalidade Cardiovascular em Idosos

```r
# Preparar dados de mortalidade cardiovascular em idosos
df_cardio_idosos <- sus_data_import(
  uf = "SP",
  ano = 2020:2023,
  sistema = "SIM-DO"
) |>
  sus_data_clean_encoding(lang = "pt") |>
  sus_data_standardize(lang = "pt") |>
  sus_data_filter_cid(disease_group = "cardiovascular", lang = "pt") |>
  sus_create_variables(
    age_groups = TRUE,
    age_breaks = c(0, 65, 75, 85, Inf),
    age_labels = c("< 65", "65-74", "75-84", "85+"),
    calendar_vars = TRUE,
    season_method = "brazilian",
    lang = "pt"
  ) |>
  sus_data_filter_demographics(
    age_range = c(65, Inf),  # Apenas idosos
    sex = "all",
    lang = "pt"
  ) |>
  sus_data_quality_report(
    output_format = "html",
    output_path = "relatorio_qualidade_cardio_idosos.html",
    lang = "pt"
  ) |>
  sus_data_aggregate(
    time_unit = "week",
    group_by = "age_group",
    fill_gaps = TRUE,
    lang = "pt"
  ) |>
  sus_data_export(
    path = "cardio_idosos_sp_2020_2023.parquet",
    format = "parquet",
    include_metadata = TRUE,
    lang = "pt"
  )
```

#### Pipeline 4: Análise de Ondas de Calor (Pentads)

```r
# Preparar dados para análise de ondas de calor usando pentads (5 dias)
df_calor <- sus_data_import(
  uf = c("SP", "RJ"),
  ano = 2023,
  sistema = "SIM-DO",
  parallel = TRUE
) |>
  sus_data_clean_encoding(lang = "pt") |>
  sus_data_standardize(lang = "pt") |>
  sus_data_filter_cid(disease_group = "heat_related", lang = "pt") |>
  sus_create_variables(
    age_groups = TRUE,
    calendar_vars = TRUE,
    season_method = "brazilian",
    lang = "pt"
  ) |>
  sus_data_aggregate(
    time_unit = "5 days",  # Pentads para efeito cumulativo
    fill_gaps = TRUE,
    lang = "pt"
  ) |>
  sus_data_export(
    path = "ondas_calor_pentads_sp_rj_2023.csv",
    format = "csv",
    include_metadata = TRUE,
    lang = "pt"
  )
```

---

## Funções Helper

### Detecção de Sistema de Saúde

```r
# Detectar automaticamente qual sistema de saúde
sistema <- detect_health_system(df)
# Retorna: "SIM", "SINASC", "SINAN", "SIH", "SIA", "CNES", ou "UNKNOWN"

# Obter descrição do sistema
descricao <- get_system_description(sistema, lang = "pt")
```

### Grupos de Doenças

```r
# Listar todos os grupos disponíveis
grupos <- list_disease_groups(lang = "pt")

# Listar apenas grupos sensíveis ao clima
grupos_clima <- list_disease_groups(climate_sensitive_only = TRUE, lang = "pt")

# Obter detalhes de um grupo específico
detalhes <- get_disease_group_details("dengue", lang = "pt")
```

---

## Roadmap

### ✅ Fase 1: Infraestrutura de Dados (COMPLETA)
* ✅ Aquisição de dados em paralelo
* ✅ Correção de codificação
* ✅ Padronização multilíngue
* ✅ Filtragem por CID-10
* ✅ Criação de variáveis epidemiológicas
* ✅ Filtragem demográfica
* ✅ Relatórios de qualidade
* ✅ Agregação temporal flexível
* ✅ Exportação com metadados

### 🔄 Fase 2: Integração Socioeconômica (Em Andamento)
* Vinculação de limites geográficos
* Integração de dados socioeconômicos do IBGE (população, PIB, IDH)
* Operações espaciais ponderadas pela população
* Correspondência de setores censitários

### 📅 Fase 3: Integração Ambiental (Planejada)
* Importação de dados meteorológicos do INMET
* Integração de dados de qualidade do ar (CETESB, INPE)
* Processamento de dados de satélite (MODIS, Sentinel)
* Algoritmos de correspondência de exposição

### 📅 Fase 4: Análise Espacial (Planejada)
* Suavização espacial bayesiana
* Detecção de clusters espaciais (SaTScan, Kulldorff)
* Indicadores locais de associação espacial (LISA)
* Modelos de regressão espacial

### 📅 Fase 5: Análise Temporal e Preditiva (Planejada)
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

## Contribuindo

Contribuições são bem-vindas! Por favor, consulte nosso [Guia de Contribuição](CONTRIBUTING.md) para detalhes sobre como participar do desenvolvimento do `climasus4r`.

---

## Agradecimentos

- Equipe do **microdatasus** pela infraestrutura fundamental de importação de dados
- **DATASUS** por fornecer acesso aberto aos dados de saúde brasileiros
- **INCT Conexão - Amazônia** pelo financiamento e suporte
- Todos os colaboradores e testadores que ajudaram a melhorar o pacote

---

## Citação

Se você usar o `climasus4r` em sua pesquisa, por favor cite:

```
Anjos, M. (2024). climasus4r: Kit de Ferramentas Integrado para Análise de Dados de Saúde, Clima e Ambiente. 
R package version 0.3.0. https://github.com/ByMaxAnjos/climasus4r
```

---

## Contato

- **Mantenedor**: Max Anjos
- **Email**: maxanjos@campus.ul.pt
- **GitHub**: [https://github.com/ByMaxAnjos/climasus4r](https://github.com/ByMaxAnjos/climasus4r)
- **Issues**: [https://github.com/ByMaxAnjos/climasus4r/issues](https://github.com/ByMaxAnjos/climasus4r/issues)
