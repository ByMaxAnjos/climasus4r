# Indicadores para sus_socio_indicators

# Estrutura: list(
#   indicator_name = list(
#     type = "rate" / "proportion" / "index",
#     unit = "per 1000" / "per 100k" / "percent" / "ratio",
#     data_sources = c("SIM", "SINASC", "SIH", "CNES", "SINAN", "POP"),
#     formula_pt = "Descricao da formula em portugues",
#     formula_en = "Formula description in English",
#     formula_es = "Descripcion de la formula en espanol",
#     numerator_logic = "Logica de calculo do numerador (dplyr/rlang)",
#     denominator_logic = "Logica de calculo do denominador (dplyr/rlang)"
#   )
# )

INDICATORS_LIST <- list(

  # ============================================================================
  # A. Indicadores Demograficos e Socioeconomicos (Fonte: Censo/Estimativas)
  # ============================================================================

  "dependency_ratio" = list(
    type = "ratio",
    unit = "ratio",
    data_sources = c("POP"),
    formula_pt = "(Pop <15 + Pop >64) / Pop 15-64",
    formula_en = "(Pop <15 + Pop >64) / Pop 15-64",
    formula_es = "(Pop <15 + Pop >64) / Pop 15-64",
    numerator_logic = "pop_0_14 + pop_65_plus",
    denominator_logic = "pop_15_64"
  ),

  "aging_index" = list(
    type = "index",
    unit = "percent",
    data_sources = c("POP"),
    formula_pt = "(Pop >64 / Pop <15) * 100",
    formula_en = "(Pop >64 / Pop <15) * 100",
    formula_es = "(Pop >64 / Pop <15) * 100",
    numerator_logic = "pop_65_plus",
    denominator_logic = "pop_0_14"
  ),

  "urbanization_rate" = list(
    type = "proportion",
    unit = "percent",
    data_sources = c("CENSO"),
    formula_pt = "% da população vivendo em área urbana",
    formula_en = "% of population living in urban area",
    formula_es = "% de la población que vive en zona urbana",
    numerator_logic = "pop_urban",
    denominator_logic = "pop_total"
  ),

  "household_density" = list(
    type = "ratio",
    unit = "ratio",
    data_sources = c("CENSO"),
    formula_pt = "Média de moradores por dormitório (Proxy de aglomeração)",
    formula_en = "Average residents per bedroom (Proxy for crowding)",
    formula_es = "Promedio de residentes por dormitorio (Proxy de hacinamiento)",
    numerator_logic = "total_residents",
    denominator_logic = "total_bedrooms"
  ),

  # ============================================================================
  # B. Indicadores de Mortalidade (Fonte: SIM + SINASC)
  # ============================================================================

  "infant_mortality_rate" = list(
    type = "rate",
    unit = "per 1000",
    data_sources = c("SIM", "SINASC"),
    formula_pt = "(Óbitos < 1 ano / Nascidos Vivos) * 1.000",
    formula_en = "(Deaths < 1 year / Live Births) * 1,000",
    formula_es = "(Defunciones < 1 año / Nacidos Vivos) * 1.000",
    numerator_logic = "sim_deaths_0_1y",
    denominator_logic = "sinasc_live_births"
  ),

  "maternal_mortality_ratio" = list(
    type = "ratio",
    unit = "per 100k",
    data_sources = c("SIM", "SINASC"),
    formula_pt = "(Óbitos maternos / Nascidos Vivos) * 100.000",
    formula_en = "(Maternal Deaths / Live Births) * 100,000",
    formula_es = "(Defunciones Maternas / Nacidos Vivos) * 100.000",
    numerator_logic = "sim_maternal_deaths",
    denominator_logic = "sinasc_live_births"
  ),

  "premature_dcn_mortality" = list(
    type = "rate",
    unit = "per 100k",
    data_sources = c("SIM", "POP"),
    formula_pt = "(Óbitos DCNT 30-69 anos / Pop 30-69) * 100.000 (ODS/ONU)",
    formula_en = "(NCD Deaths 30-69 years / Pop 30-69) * 100,000 (SDG/UN)",
    formula_es = "(Defunciones ENF 30-69 años / Pob 30-69) * 100.000 (ODS/ONU)",
    numerator_logic = "sim_deaths_dcn_30_69", # CID-10 C00-C97, I00-I99, J30-J98, E10-E14
    denominator_logic = "pop_30_69"
  ),

  "homicide_rate" = list(
    type = "rate",
    unit = "per 100k",
    data_sources = c("SIM", "POP"),
    formula_pt = "(Óbitos por Homicídio / Pop Total) * 100.000",
    formula_en = "(Homicide Deaths / Total Pop) * 100,000",
    formula_es = "(Defunciones por Homicidio / Pob Total) * 100.000",
    numerator_logic = "sim_deaths_homicide", # CID-10 X85-Y09
    denominator_logic = "pop_total"
  ),

  "traffic_mortality_rate" = list(
    type = "rate",
    unit = "per 100k",
    data_sources = c("SIM", "POP"),
    formula_pt = "(Óbitos no Trânsito / Pop Total) * 100.000",
    formula_en = "(Traffic Deaths / Total Pop) * 100,000",
    formula_es = "(Defunciones por Trafico / Pob Total) * 100.000",
    numerator_logic = "sim_deaths_traffic", # CID-10 V01-V99
    denominator_logic = "pop_total"
  ),

  # ============================================================================
  # C. Indicadores de Morbidade e Epidemiológicos (Fonte: SINAN + SIH)
  # ============================================================================

  "arbovirus_incidence_rate" = list(
    type = "rate",
    unit = "per 100k",
    data_sources = c("SINAN", "POP"),
    formula_pt = "(Casos de Dengue+Zika+Chikungunya / Pop Total) * 100.000",
    formula_en = "(Dengue+Zika+Chikungunya Cases / Total Pop) * 100,000",
    formula_es = "(Casos de Dengue+Zika+Chikungunya / Pob Total) * 100.000",
    numerator_logic = "sinan_cases_arbovirus",
    denominator_logic = "pop_total"
  ),

  "tb_incidence_rate" = list(
    type = "rate",
    unit = "per 100k",
    data_sources = c("SINAN", "POP"),
    formula_pt = "(Novos casos TB / Pop Total) * 100.000",
    formula_en = "(New TB Cases / Total Pop) * 100,000",
    formula_es = "(Nuevos Casos de TB / Pob Total) * 100.000",
    numerator_logic = "sinan_cases_tb_new",
    denominator_logic = "pop_total"
  ),

  "icsap_hospitalization_rate" = list(
    type = "rate",
    unit = "per 10k",
    data_sources = c("SIH", "POP"),
    formula_pt = "(Internações por Condições Sensíveis à Atenção Primária / Pop Total) * 10.000",
    formula_en = "(Hospitalizations for Ambulatory Care Sensitive Conditions / Total Pop) * 10,000",
    formula_es = "(Hospitalizaciones por Condiciones Sensibles a la Atención Primaria / Pob Total) * 10.000",
    numerator_logic = "sih_hospitalizations_icsap",
    denominator_logic = "pop_total"
  ),

  "respiratory_hospitalization_rate" = list(
    type = "rate",
    unit = "per 10k",
    data_sources = c("SIH", "POP"),
    formula_pt = "(Internações por Doenças Respiratórias / Pop Total) * 10.000",
    formula_en = "(Respiratory Disease Hospitalizations / Total Pop) * 10,000",
    formula_es = "(Hospitalizaciones por Enfermedades Respiratorias / Pob Total) * 10.000",
    numerator_logic = "sih_hospitalizations_resp", # CID-10 J00-J99
    denominator_logic = "pop_total"
  ),

  # ============================================================================
  # D. Indicadores de Saúde Materno-Infantil (Fonte: SINASC)
  # ============================================================================

  "low_birth_weight_proportion" = list(
    type = "proportion",
    unit = "percent",
    data_sources = c("SINASC"),
    formula_pt = "% nascidos com peso < 2.500g",
    formula_en = "% of births with weight < 2,500g",
    formula_es = "% de nacimientos con peso < 2.500g",
    numerator_logic = "sinasc_births_low_weight",
    denominator_logic = "sinasc_live_births"
  ),

  "general_fertility_rate" = list(
    type = "rate",
    unit = "per 1000",
    data_sources = c("SINASC", "POP"),
    formula_pt = "(Nascidos Vivos / Mulheres 15-49 anos) * 1.000",
    formula_en = "(Live Births / Women 15-49 years) * 1,000",
    formula_es = "(Nacidos Vivos / Mujeres 15-49 años) * 1.000",
    numerator_logic = "sinasc_live_births",
    denominator_logic = "pop_women_15_49"
  ),

  "cesarean_proportion" = list(
    type = "proportion",
    unit = "percent",
    data_sources = c("SINASC"),
    formula_pt = "(Partos Cesáreos / Total Partos) * 100",
    formula_en = "(Cesarean Deliveries / Total Deliveries) * 100",
    formula_es = "(Partos por Cesárea / Total Partos) * 100",
    numerator_logic = "sinasc_cesarean_deliveries",
    denominator_logic = "sinasc_total_deliveries"
  ),

  # ============================================================================
  # E. Indicadores de Recursos e Cobertura (Fonte: CNES + POP)
  # ============================================================================

  "beds_per_capita" = list(
    type = "rate",
    unit = "per 1000",
    data_sources = c("CNES", "POP"),
    formula_pt = "(Total Leitos / Pop Total) * 1.000",
    formula_en = "(Total Beds / Total Pop) * 1,000",
    formula_es = "(Total Camas / Pob Total) * 1.000",
    numerator_logic = "cnes_total_beds",
    denominator_logic = "pop_total"
  ),

  "doctors_per_capita" = list(
    type = "rate",
    unit = "per 1000",
    data_sources = c("CNES", "POP"),
    formula_pt = "(Total Médicos / Pop Total) * 1.000",
    formula_en = "(Total Doctors / Total Pop) * 1,000",
    formula_es = "(Total Médicos / Pob Total) * 1.000",
    numerator_logic = "cnes_total_doctors",
    denominator_logic = "pop_total"
  ),
  
  # ============================================================================
  # E. Indicadores de Saneamento, Mercado de trabalho e performace de saude 
  # ============================================================================
  
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
  
  # --- Performance de Saude (IPS) ---
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

# Exportar a lista para ser usada na funcao
save(INDICATORS_LIST, file = "INDICATORS_LIST.RData")
