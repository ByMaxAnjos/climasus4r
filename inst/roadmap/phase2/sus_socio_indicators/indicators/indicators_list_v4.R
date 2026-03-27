# This file defines the master catalog of indicators for climasus4r.
# It is structured as a nested list for easy metadata-driven access.
# All names are in English for internal consistency, but labels are multilingual.

INDICATORS_LIST <- list(
  # ============================================================================
  # 1. MORTALITY INDICATORS (RIPSA/OPAS)
  # ============================================================================
  "infant_mortality_rate" = list(
    name_pt = "Taxa de Mortalidade Infantil",
    name_en = "Infant Mortality Rate",
    name_es = "Tasa de Mortalidad Infantil",
    unit = "per 1,000 live births",
    multiplier = 1000,
    category = "Mortality",
    source = "SIM, SINASC, POP",
    formula = "n_deaths_infant / n_live_births",
    required_cols = c("n_deaths_infant", "n_live_births"),
    uncertainty_method = "Poisson" # Method for confidence interval
  ),
  "premature_dcn_mortality" = list(
    name_pt = "Taxa de Mortalidade Prematura por DCNT",
    name_en = "Premature NCD Mortality Rate",
    name_es = "Tasa de Mortalidad Prematura por ENTs",
    unit = "per 100,000 population (30-69 years)",
    multiplier = 100000,
    category = "Mortality",
    source = "SIM, POP",
    formula = "n_deaths_dcn_30_69 / pop_30_69",
    required_cols = c("n_deaths_dcn_30_69", "pop_30_69"),
    uncertainty_method = "Poisson"
  ),
  "maternal_mortality_ratio" = list(
    name_pt = "Razão de Mortalidade Materna",
    name_en = "Maternal Mortality Ratio",
    name_es = "Razón de Mortalidad Materna",
    unit = "per 100,000 live births",
    multiplier = 100000,
    category = "Mortality",
    source = "SIM, SINASC",
    formula = "n_deaths_maternal / n_live_births",
    required_cols = c("n_deaths_maternal", "n_live_births"),
    uncertainty_method = "Poisson"
  ),
  
  # ============================================================================
  # 2. DEMOGRAPHIC INDICATORS (CENSO)
  # ============================================================================
  "dependency_ratio" = list(
    name_pt = "Razão de Dependência",
    name_en = "Dependency Ratio",
    name_es = "Razón de Dependencia",
    unit = "per 100 population (15-64 years)",
    multiplier = 100,
    category = "Demographic",
    source = "CENSO",
    formula = "(pop_0_14 + pop_65_plus) / pop_15_64",
    required_cols = c("pop_0_14", "pop_65_plus", "pop_15_64"),
    uncertainty_method = "None"
  ),
  "aging_index" = list(
    name_pt = "Índice de Envelhecimento",
    name_en = "Aging Index",
    name_es = "Índice de Envejecimiento",
    unit = "per 100 population (0-14 years)",
    multiplier = 100,
    category = "Demographic",
    source = "CENSO",
    formula = "pop_65_plus / pop_0_14",
    required_cols = c("pop_65_plus", "pop_0_14"),
    uncertainty_method = "None"
  ),
  
  # ============================================================================
  # 3. SOCIOECONOMIC VULNERABILITY INDICATORS (IVS/SoVI)
  # ============================================================================
  "illiteracy_rate" = list(
    name_pt = "Taxa de Analfabetismo",
    name_en = "Illiteracy Rate",
    name_es = "Tasa de Analfabetismo",
    unit = "percent",
    multiplier = 100,
    category = "Socioeconomic",
    source = "CENSO",
    formula = "n_illiterate / pop_15_plus",
    required_cols = c("n_illiterate", "pop_15_plus"),
    uncertainty_method = "Binomial" # For proportions
  ),
  "water_connection_rate" = list(
    name_pt = "Taxa de Conexão de Água (ICA)",
    name_en = "Water Connection Rate (ICA)",
    name_es = "Water Connection Rate (ICA)",
    unit = "percent",
    multiplier = 100,
    category = "Socioeconomic",
    source = "CENSO",
    formula = "n_households_with_water / n_households_total",
    required_cols = c("n_households_with_water", "n_households_total"),
    uncertainty_method = "Binomial"
  ),
  "sewage_connection_rate" = list(
    name_pt = "Taxa de Conexão de Esgoto (ICE)",
    name_en = "Sewage Connection Rate (ICE)",
    name_es = "Sewage Connection Rate (ICE)",
    unit = "percent",
    multiplier = 100,
    category = "Socioeconomic",
    source = "CENSO",
    formula = "n_households_with_sewage / n_households_total",
    required_cols = c("n_households_with_sewage", "n_households_total"),
    uncertainty_method = "Binomial"
  ),
  "gini_index" = list(
    name_pt = "Índice de Gini (Renda)",
    name_en = "Gini Index (Income)",
    name_es = "Índice de Gini (Ingreso)",
    unit = "index (0 to 1)",
    multiplier = 1,
    category = "Socioeconomic",
    source = "CENSO",
    formula = "gini_index_value", # Assumes value is pre-calculated or directly available
    required_cols = c("gini_index_value"),
    uncertainty_method = "None"
  ),
  
  # ============================================================================
  # 4. SYNTHETIC INDICES (IVS, IPS) - Requires pre-calculated components
  # ============================================================================
  "ivs_score" = list(
    name_pt = "Índice de Vulnerabilidade Social (IVS)",
    name_en = "Social Vulnerability Index (SVI)",
    name_es = "Índice de Vulnerabilidad Social (IVS)",
    unit = "score (0 to 1)",
    multiplier = 1,
    category = "Synthetic Index",
    source = "IPEA/CENSO",
    formula = "ivs_score_value", # Assumes value is pre-calculated
    required_cols = c("ivs_score_value"),
    uncertainty_method = "None"
  )
)
