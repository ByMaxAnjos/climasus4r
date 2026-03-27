# INDICATORS_LIST v3.0 - Variable Mapping Architecture
# This list maps conceptual indicators to required columns in an enriched dataframe.

INDICATORS_LIST <- list(
  
  # --- DEMOGRAPHIC INDICATORS ---
  "infant_mortality_rate" = list(
    name_pt = "Taxa de Mortalidade Infantil",
    required_vars = list(
      numerator = "n_deaths_infant", # Default column from SIM
      denominator = "n_live_births"  # Default column from SINASC
    ),
    multiplier = 1000,
    unit = "per 1,000 live births"
  ),
  
  "dependency_ratio" = list(
    name_pt = "Razao de Dependencia",
    required_vars = list(
      pop_young = "pop_0_14",    # Default from census enrichment
      pop_old = "pop_65_plus",   # Default from census enrichment
      pop_active = "pop_15_64"   # Default from census enrichment
    ),
    formula = "(pop_young + pop_old) / pop_active * 100",
    unit = "percent"
  ),

  # --- SOCIOECONOMIC INDICATORS (CENSUS BASED) ---
  "water_connection_rate" = list(
    name_pt = "Taxa de Conexao de Agua",
    required_vars = list(
      numerator = "v111",    # Census 2022 variable
      denominator = "v003"   # Total households
    ),
    multiplier = 100,
    unit = "percent"
  ),

  "illiteracy_rate" = list(
    name_pt = "Taxa de Analfabetismo",
    required_vars = list(
      numerator = "pop_illiterate_15_plus",
      denominator = "pop_15_plus"
    ),
    multiplier = 100,
    unit = "percent"
  )
)
