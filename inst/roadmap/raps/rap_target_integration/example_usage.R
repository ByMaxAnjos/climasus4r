
# Create example usage file and README

example_usage = '''# ==============================================================================
# examples/sus_targets_examples.R — Exemplos de Uso da Integração targets
# ==============================================================================

library(climasus4r)
library(targets)

# ==============================================================================
# EXEMPLO 1: Exportar RAP existente para targets
# ==============================================================================

# Ler um RAP previamente exportado
rap <- sus_read_rap("pipelines/meu_pipeline_cardio.R")

# Converter para pipeline targets com branching por UF
sus_export_targets(
  pipeline = rap,
  file_path = "_targets_cardio.R",
  branching = "uf",           # Um branch por estado
  parallel_workers = 4,       # 4 workers paralelos
  include_qc = TRUE,          # Incluir validação de qualidade
  overwrite = TRUE
)

# Executar o pipeline
tar_make(script = "_targets_cardio.R")

# ==============================================================================
# EXEMPLO 2: Criar pipeline targets dinâmico em tempo real
# ==============================================================================

# Definir pipeline climasus4r
pipeline_expr <- quote(
  sus_data_import(uf = "SP", year = 2020:2022, system = "SIM-DO") |>
    sus_data_clean_encoding() |>
    sus_data_standardize() |>
    sus_data_filter_cid(disease_group = "cardiovascular") |>
    sus_data_aggregate(time_unit = "month")
)

# Exportar diretamente para targets
sus_export_targets(
  pipeline = pipeline_expr,
  file_path = "_targets.R",
  branching = "cross",        # Todas combinações UF × Ano
  parallel_workers = 8
)

# ==============================================================================
# EXEMPLO 3: Uso Avançado com Fábrica de Targets
# ==============================================================================

# Em seu _targets.R principal:
library(targets)
library(climasus4r)

# Ler múltiplos RAPs
rap_cardio <- sus_read_rap("raps/cardiovascular.R")
rap_resp <- sus_read_rap("raps/respiratorio.R")

# Criar lista combinada de targets
list(
  # Dados brutos compartilhados
  tar_target(raw_population, read_pop_data()),
  
  # Pipeline 1: Cardiovascular (branching por UF)
  sus_tar_factory(
    rap_cardio, 
    name = "cardio", 
    branching = "uf",
    include_qc = TRUE
  ),
  
  # Pipeline 2: Respiratório (branching por ano)
  sus_tar_factory(
    rap_resp, 
    name = "resp", 
    branching = "year",
    include_qc = TRUE
  ),
  
  # Análise comparativa
  tar_target(
    comparison,
    compare_diseases(cardio_results, resp_results)
  ),
  
  # Relatório final
  tar_render(
    report,
    "relatorio_final.Rmd",
    params = list(cardio = cardio_results, resp = resp_results)
  )
)

# ==============================================================================
# EXEMPLO 4: Fast Interchange (Intercâmbio Rápido)
# ==============================================================================

# Criar e exportar uma "receita" compacta do pipeline
rap <- sus_read_rap("meu_pipeline.R")

# Exportar receita (YAML compacto)
sus_export_rap_recipe(
  rap = rap,
  file_path = "recipes/cardio_v1.yaml",
  include_data = TRUE,    # Incluir hash para rastreabilidade
  compress = TRUE         # Criar .zip
)

# Em outra máquina ou momento:
# Importar e executar imediatamente
resultados <- sus_import_rap_recipe(
  recipe_path = "recipes/cardio_v1.yaml.zip",
  override_params = list(
    uf = c("RJ", "ES", "MG"),  # Sobrescrever UFs
    years = 2021:2023          # Sobrescrever anos
  ),
  execute = TRUE,              # Executar via targets
  workers = 4
)

# ==============================================================================
# EXEMPLO 5: Execução Programática com Cache
# ==============================================================================

# Criar objeto tar_rap
rap <- sus_read_rap_targets(
  "pipelines/analise_calor.R",
  tar_config = list(
    store = "_targets_store",
    workers = 4
  )
)

# Executar com controle total
run <- sus_run_targets(
  rap = rap,
  workers = 4,
  reporter = "verbose"
)

# Acessar resultados
dados <- run$results

# Checar metadados
print(run$rap$targets_meta)

# ==============================================================================
# EXEMPLO 6: Visualização e Monitoramento
# ==============================================================================

# Visualizar grafo do pipeline
sus_vis_pipeline(rap)

# Checar status
sus_tar_status(rap)

# Inspecionar objeto RAP
sus_inspect_rap(rap$rap)

# Comparar versões
rap_v1 <- sus_read_rap("pipelines/v1.R")
rap_v2 <- sus_read_rap("pipelines/v2.R")
sus_inspect_rap(rap_v1, rap_v2)

# ==============================================================================
# EXEMPLO 7: Pipeline Multi-Estado com Cross-Branching
# ==============================================================================

# Configurar análise para todos estados do Sudeste
pipeline_sudeste <- quote(
  sus_data_import(
    uf = c("SP", "RJ", "MG", "ES"),
    year = 2020:2023,
    system = "SIM-DO"
  ) |>
    sus_data_filter_cid(disease_group = "heat_related") |>
    sus_data_aggregate(time_unit = "week")
)

# Exportar com branching cruzado (16 combinações: 4 UFs × 4 anos)
sus_export_targets(
  pipeline = pipeline_sudeste,
  branching = "cross",
  parallel_workers = 8,
  include_qc = TRUE
)

# Executar - cada combinação UF+Ano é um branch independente
tar_make()

# Resultados são automaticamente combinados
tar_read(climasus_pipeline_results)

# ==============================================================================
# EXEMPLO 8: Integração com Quarto/RMarkdown
# ==============================================================================

# Criar pipeline que gera relatórios parametrizados
list(
  sus_tar_factory(
    rap = sus_read_rap("base_pipeline.R"),
    name = "analise_base",
    branching = "uf"
  ),
  
  # Gerar um relatório por UF (branching dinâmico)
  tar_target(
    relatorio_uf,
    quarto::quarto_render(
      "template_relatorio.qmd",
      execute_params = list(
        uf = analise_base_uf_list,
        dados = analise_base_results
      ),
      output_file = paste0("relatorio_", analise_base_uf_list, ".html")
    ),
    pattern = map(analise_base_uf_list)
  )
)
'''

readme = '''# Integração climasus4r + targets

Sistema de integração entre pipelines climasus4r e o pacote `targets` do R, permitindo workflows reprodutíveis, paralelizados e com cache automático.

## 🎯 Características

- **Branching Dinâmico**: Execute análises em paralelo para múltiplas UFs, anos ou combinações
- **Fast Interchange**: Exporte/importe pipelines como receitas compactas (YAML)
- **Cache Inteligente**: Reexecute apenas o que mudou graças ao `targets`
- **Escalabilidade**: De 1 a 100+ workers sem mudar o código
- **Reprodutibilidade**: Metadados completos de sessão e dependências

## 📦 Instalação

```r
# Instalar climasus4r (se ainda não tiver)
devtools::install_github("climasus4r/climasus4r")

# Instalar targets
install.packages("targets")
```

## 🚀 Uso Rápido

### 1. Exportar RAP para targets

```r
library(climasus4r)

# Ler pipeline existente
rap <- sus_read_rap("meu_pipeline.R")

# Exportar com branching por UF (paralelização automática)
sus_export_targets(
  rap,
  file_path = "_targets.R",
  branching = "uf",
  parallel_workers = 4
)

# Executar
targets::tar_make()
```

### 2. Fast Interchange (Compartilhamento)

```r
# Exportar receita compacta
sus_export_rap_recipe(rap, "minha_analise.yaml", compress = TRUE)

# Em outro ambiente: importar e executar
resultados <- sus_import_rap_recipe(
  "minha_analise.yaml.zip",
  override_params = list(uf = "RJ", years = 2023),
  execute = TRUE
)
```

## 🔧 Funções Principais

| Função | Descrição |
|--------|-----------|
| `sus_export_targets()` | Exporta RAP como pipeline `_targets.R` |
| `sus_read_rap_targets()` | Lê RAP como objeto tar_rap_object |
| `sus_tar_factory()` | Cria lista de targets para integração manual |
| `sus_run_targets()` | Executa RAP via targets com controle total |
| `sus_export_rap_recipe()` | Exporta receita YAML compacta |
| `sus_import_rap_recipe()` | Importa e executa receita |
| `sus_vis_pipeline()` | Visualiza grafo do pipeline |

## 🌿 Estratégias de Branching

- `"none"`: Execução única (padrão)
- `"uf"`: Um branch por estado (paralelização geográfica)
- `"year"`: Um branch por ano (paralelização temporal)
- `"cross"`: Todas combinações UF × Ano

## 📊 Arquitetura

```
┌─────────────────┐     ┌──────────────────┐     ┌─────────────────┐
│  sus_read_rap   │────▶│  tar_rap_object  │────▶│ sus_tar_factory │
│  (arquivo .R)   │     │  (S3 class)      │     │ (targets list)  │
└─────────────────┘     └──────────────────┘     └─────────────────┘
                              │
                              ▼
                       ┌──────────────────┐
                       │ sus_run_targets  │
                       │ (orquestração)   │
                       └──────────────────┘
```

## 📝 Exemplos Avançados

Veja `examples/sus_targets_examples.R` para casos de uso completos incluindo:
- Múltiplos pipelines combinados
- Geração de relatórios parametrizados
- Análises comparativas entre doenças
- Workflows com 50+ estados

## 🤝 Contribuição

Contribuições são bem-vindas! Por favor, siga o estilo de código existente e inclua testes para novas funcionalidades.

## 📄 Licença

MIT License - veja LICENSE para detalhes.
'''

print("Arquivos criados:")
print("1. examples/sus_targets_examples.R")
print("2. README.md")
print(f"\\nExemplos: {len(example_usage.split(chr(10)))} linhas")
print(f"README: {len(readme.split(chr(10)))} linhas")
