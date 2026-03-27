# Biblioteca de Indicadores Expandida (v2.0) para sus_socio_indicators

# Carregar lista base
source("/home/ubuntu/indicadores_climasus4r.R") # INDICATORS_LIST

# Adicionar novos indicadores
NEW_INDICATORS <- list(

  # --- Saneamento (Censo 2022) ---
  "water_connection_rate" = list(
    type = "proportion",
    unit = "percent",
    data_sources = c("CENSO"),
    formula_pt = "(v111 / v003) * 100",
    numerator_logic = "v111",
    denominator_logic = "v003"
  ),

  "sewage_connection_rate" = list(
    type = "proportion",
    unit = "percent",
    data_sources = c("CENSO"),
    formula_pt = "(v309 / v003) * 100",
    numerator_logic = "v309",
    denominator_logic = "v003"
  ),

  # --- Mercado de Trabalho ---
  "unemployment_rate" = list(
    type = "rate",
    unit = "percent",
    data_sources = c("CENSO", "PNAD"),
    formula_pt = "(Desocupados / Força de Trabalho) * 100",
    numerator_logic = "unemployed",
    denominator_logic = "labor_force"
  ),

  "gini_index" = list(
    type = "index",
    unit = "ratio",
    data_sources = c("CENSO"),
    formula_pt = "Índice de Gini de rendimento domiciliar per capita",
    numerator_logic = "gini_calc",
    denominator_logic = "1"
  ),

  # --- Performance de Saúde (IPS) ---
  "prenatal_early_coverage" = list(
    type = "proportion",
    unit = "percent",
    data_sources = c("SINASC"),
    formula_pt = "(Pré-natal até 12ª semana / Total Nascidos) * 100",
    numerator_logic = "sinasc_prenatal_early",
    denominator_logic = "sinasc_live_births"
  ),

  "vaccination_coverage_infant" = list(
    type = "proportion",
    unit = "percent",
    data_sources = c("SIA", "POP"),
    formula_pt = "Cobertura vacinal em crianças < 1 ano",
    numerator_logic = "sia_vaccines_applied",
    denominator_logic = "pop_0_1"
  )
)

# Mesclar listas
INDICATORS_LIST <- c(INDICATORS_LIST, NEW_INDICATORS)

# Salvar versão atualizada
save(INDICATORS_LIST, file = "INDICATORS_LIST_V2.RData")
