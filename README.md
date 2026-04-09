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

O pacote `climasus4r`está disponível no GitHub e pode ser instalado de duas formas. Recomendamos a **Opção 1** para a maioria dos usuários.

---
Caso você ainda não tenha o pacote `remotes` instalado, execute o código abaixo:

::: callout-tip
**Pré-requisito::** Se você já tem o   `remotes` ou `devtools` instalado, pode pular
essa etapa.
:::

```r
if (!require("remotes")) { install.packages("remotes")}
```

## Opção 1: Instalação via GitHub (Recomendado)

Esta é a forma mais rápida de obter a versão mais recente:

```r
# Instalar ou atualizar o pacote diretamente do GitHub
remotes::install_github("ByMaxAnjos/climasus4r", upgrade = "never")
```

::: callout-warning
**Atualizações:** O `climasus4r` está em desenvolvimento ativo. Para atualizar, basta executar o comando acima novamente. O R irá sobrescrever a versão antiga automaticamente. Após a atualização, recomenda-se reiniciar a sessão (Menu: Session > Restart R).
:::

## Opção 2: Instalação via Arquivo Local (.zip)

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

### **Sistemas Suportados**

O pacote permite, por meio do pacote microdatsus, o acesso simplificado aos principais sistemas de informação do DATASUS, cobrindo epidemiologia, mortalidade, internações e rede assistencial:

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

# Pipeline completo da Fase 1: Dados prontos para análise em 8 passos
df_analise <- sus_data_import(
  uf = "SP",
  year = 2023,
  system = "SIM-DO"
) |>
  sus_data_clean_encoding(lang = "pt") |>
  sus_data_standardize(lang = "pt") |>
  sus_data_filter_cid(disease_group = "respiratory", lang = "pt") |>
  sus_data_create_variables(
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

## Exemplo: Infraestrutura de Dados ✅ 

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

---

### Funções Principais

#### 1. `sus_data_import()` - Aquisição de Dados

Importe dados do DATASUS com suporte a **processamento paralelo** para múltiplos estados e anos.

```r
# Um único estado e ano
df <- sus_data_import(uf = "RJ", year = 2022, system = "SIM-DO")

# Múltiplos estados e anos com processamento paralelo
df <- sus_data_import(
  uf = c("RJ", "SP", "MG", "ES"),
  year = 2018:2022,
  system = "SIM-DO",
  parallel = TRUE,
  workers = 4
)
```

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

Filtre dados por códigos CID-10 com **mais de 50 grupos de doenças predefinidos** e opções de correspondência flexíveis.

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

#### 5. `sus_data_create_variables()` - Criação de Variáveis Epidemiológicas

Crie automaticamente variáveis de **idade**, **calendário** e **sazonalidade** essenciais para análises de séries temporais e DLNM.

```r
df_com_vars <- sus_data_create_variables(
  df,
  create_age_groups = TRUE,
  age_breaks = c(0, 5, 15, 25, 45, 65, Inf),
  age_labels = c("0-4", "5-14", "15-24", "25-44", "45-64", "65+"),
  create_calendar_vars = TRUE,
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
  sus_data_create_variables(
    create_age_groups = TRUE,
    age_breaks = c(0, 15, 60, Inf),
    age_labels = c("Criancas", "Adultos", "Idosos"),
    create_calendar_vars = TRUE,
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
  sex = c("Masculino"),
  lang = "pt"
)

# Filtrar idosos do sexo masculino
df_idosos_masc <- sus_data_filter_demographics(
  df,
  age_range = c(65, Inf),
  sex = "male",
  lang = "en"
)

# Filtrar adultos por raça/cor
df_adultos_pretos <- sus_data_filter_demographics(
  df,
  age_range = c(18, 60),
  race = "black",
  lang = "en"
)
```

**Opções de Filtragem:**

| Parâmetro | Opções | Descrição |
|-----------|--------|-----------|
| `age_range` | `c(min, max)` | Intervalo de idade em anos (use `Inf` para sem limite) |
| `sex` | `"male"`, `"female"`, `"ignoared"` | Sexo biológico |
| `race` | `"white"`, `"black"`, `"brown"`, `"yellow"`, `"indigenous"` | Raça/cor (IBGE) |

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


---

#### 8. `sus_data_aggregate()` - Agregação Temporal

Agregue dados em séries temporais com **flexibilidade temporal máxima** e **preenchimento automático de gaps**.

```r
# Agregação mensal (padrão)
df_mensal <- sus_data_aggregate(
  df,
  time_unit = "month",
  date_col = "death_date",
  lang = "pt"
)

# Agregação semanal
df_semanal <- sus_data_aggregate(
  df,
  time_unit = "week",
  lang = "pt"
)

# Agregação sazonal (estações brasileiras)
df_sazonal <- sus_data_aggregate(
  df,
  time_unit = "season",
  lang = "pt"
)

# Agregação por pentads (5 dias) para ondas de calor
df_pentads <- sus_data_aggregate(
  df,
  time_unit = "5 days",
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
  sus_data_create_variables(age_groups = TRUE, lang = "pt") |>
  sus_data_aggregate(
    time_unit = "month",
    group_by = c("municipality_code", "age_group"),
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
  file_path = "dados_analise.csv",
  format = "csv",
  include_metadata = TRUE,
  lang = "pt"
)

# Exportar como RDS (formato R nativo)
sus_data_export(
  df,
  file_path = "dados_analise.rds",
  format = "rds",
  include_metadata = TRUE,
  lang = "pt"
)

# Exportar como Parquet (formato eficiente)
sus_data_export(
  df,
  file_path = "dados_analise.parquet",
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

---

### Exemplos de Pipelines Completos

#### Pipeline 1: Análise de Doenças Respiratórias Pediátricas

```r
library(climasus4r)

# Preparar dados de doenças respiratórias em crianças < 5 anos
df_resp_ped <- sus_data_import(
  uf = c("SP", "RJ", "MG"),
  year = 2018:2023,
  system = "SIM-DO",
  parallel = TRUE
) |>
  sus_data_clean_encoding(lang = "pt") |>
  sus_data_standardize(lang = "pt") |>
  sus_data_filter_cid(disease_group = "respiratory", lang = "pt") |>
  sus_data_create_variables(
    create_age_groups = TRUE,
    age_breaks = c(0, 1, 5, Inf),
    age_labels = c("< 1 ano", "1-4 anos", "5+ anos"),
    lang = "pt"
  ) |>
  sus_data_filter_demographics(
    age_range = c(0, 5),
    sex = c("Feminino", "Masculino"),
    lang = "pt"
  ) |>
  sus_data_aggregate(
    time_unit = "month",
    group_by = "age_group",
    lang = "pt"
  )
#Save
  sus_data_export(df_resp_ped,
    file_path = "respiratorias_pediatricas_sudeste_2018_2023.csv",
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
  year = 2015:2023,
  system = "SINAN-DENGUE",
  parallel = TRUE,
) |>
  sus_data_clean_encoding(lang = "pt") |>
  sus_data_standardize(lang = "pt") |>
  sus_data_create_variables(
    create_age_groups = TRUE,
    create_calendar_vars = TRUE,
    lang = "pt"
  ) |>
  sus_data_aggregate(
    time_unit = "season",  # Agregação por estação
    lang = "pt"
  )
  #save
  sus_data_export(df_dengue,
    file_path = "dengue_sazonal_amazonas_2015_2023.csv",
    format = "csv",
    include_metadata = TRUE,
    lang = "pt"
  )
```

#### Pipeline 3: Análise de Mortalidade Cardiovascular em Idosos

```r
# Preparar dados de mortalidade cardiovascular em idosos
df_cardio_idosos <- sus_data_import(
  uf = "AM",
  ano = 2020:2023,
  sistema = "SIM-DO"
) |>
  sus_data_clean_encoding(lang = "pt") |>
  sus_data_standardize(lang = "pt") |>
  sus_data_filter_cid(disease_group = "cardiovascular", lang = "pt") |>
  sus_data_create_variables(
    create_age_groups = TRUE,
    age_breaks = c(0, 65, 75, 85, Inf),
    age_labels = c("< 65", "65-74", "75-84", "85+"),
    create_calendar_vars = TRUE,
    lang = "pt"
  ) |>
  sus_data_filter_demographics(
    age_range = c(65, Inf),  # Apenas idosos
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
    lang = "pt"
  ) |>
  sus_data_export(
    file_path = "cardio_idosos_sp_2020_2023.parquet",
    format = "parquet",
    include_metadata = TRUE,
    lang = "pt"
  )
```

#### Pipeline 4: Análise de Ondas de Calor

```r
# Preparar dados para análise de ondas de calor usando pentads (5 dias)
df_calor <- sus_data_import(
  uf = c("AM", "RO"),
  year = 2023,
  system = "SIM-DO",
  parallel = TRUE
) |>
  sus_data_clean_encoding(lang = "pt") |>
  sus_data_standardize(lang = "pt") |>
  sus_data_filter_cid(disease_group = "heat_related", lang = "pt") |>
  sus_data_create_variables(
    create_age_groups = TRUE,
    lang = "pt"
  ) |>
  sus_data_aggregate(
    time_unit = "5 days",  # Pentads para efeito cumulativo
    lang = "pt"
  ) |>
  sus_data_export(
    file_path = "ondas_calor_pentads_sp_rj_2023.csv",
    format = "csv",
    include_metadata = TRUE,
    lang = "pt"
  )
```

---

## Funções Helper

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

## Agenda

### ✅ Fase 1: Infraestrutura de Dados
* ✅ Aquisição de dados em paralelo
* ✅ Correção de codificação
* ✅ Padronização multilíngue
* ✅ Filtragem por CID-10
* ✅ Criação de variáveis epidemiológicas
* ✅ Filtragem demográfica
* ✅ Relatórios de qualidade
* ✅ Agregação temporal flexível
* ✅ Exportação com metadados

### 🔄 Fase 2: Integração Socioeconômica e territorial (Em Andamento)
* ✅ Vinculação de limites geográficos
* ✅ Integração de dados socioeconômicos do IBGE (população, PIB, renda, etc)
* Operações espaciais ponderadas pela população
* Cálculo de indicadores sociais e de saúde.

### 📅 Fase 3: Integração Climática e Ambiental (Planejada)
* Importação de dados meteorológicos do INMET e NOAA
* Integração com dados do CCSRO e FIORES (INCT-CONEXAO)
* Integração de dados de qualidade do ar 
* Processamento de dados de satélite 
* * Cálculo de indicadores climáticos e de saúde.

### 📅 Fase 4: Análise Espacial (Planejada)
* Suavização espacial bayesiana
* Detecção de clusters espaciais (SaTScan, Kulldorff)
* Indicadores locais de associação espacial (LISA)
* Modelos de regressão espacial
* Modelos de risco relativo às internações e mortes
* Modelo bivariado quasi poison

### 📅 Fase 5: Análise Temporal e Preditiva (Planejada)
* Modelos não lineares de defasagem distribuída (DLNM)
* Cálculo de fração atribuível
* Decomposição de séries temporais
* Wrappers de previsão de aprendizado de máquina
* Modelo de bioprognose baseado na previsão do tempo para 3 dias futuros
* Modelos de bioprognose baseados em cenários climáticos de curto (3 meses) médio (5-10 anos) e longo prazo (50-100 anos)

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
