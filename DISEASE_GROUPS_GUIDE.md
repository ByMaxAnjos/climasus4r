# Disease Groups Guide - climasus4r

## Overview

The climasus4r package includes **50+ predefined epidemiological disease groups** that allow users to filter health data without needing to know specific ICD-10 codes. This is particularly useful for:

- **Teaching and training** in public health
- **Epidemiological surveillance** workflows
- **Climate-health research** focusing on climate-sensitive diseases
- **Exploratory data analysis** without deep ICD-10 knowledge

---

## Quick Start

### Basic Usage

```r
# Filter by disease group instead of ICD codes
sim_data <- sus_data_import(
  uf = "SP",
  year = 2023,
  system = "SIM-DO",
  use_cache = TRUE,
  verbose = TRUE
  ) %>% 
  sus_data_clean_enconding() %>% 
  sus_data_standardize()

df_dengue <- sus_data_filter_cid(
  sim_data,
  disease_group = "dengue") #

# List all available groups
list_disease_groups(lang = "en")

# Get details about a specific group
get_disease_group_details("cardiovascular", lang = "pt")
```

---

## Complete Disease Group Catalog

### 🦠 Infectious and Parasitic Diseases (Chapter I: A00-B99)

#### Climate-Sensitive Infectious Diseases

| Group Name | ICD Codes | Climate Factors | Description |
|------------|-----------|-----------------|-------------|
| `diarrheal` | A00-A09 | Temperature, precipitation, flooding | Intestinal infectious diseases including cholera, typhoid, shigellosis |
| `tuberculosis` | A15-A19 | Temperature, humidity, seasonality | Respiratory and other forms of tuberculosis |
| `zoonotic_bacterial` | A20-A28 | Precipitation, flooding, temperature | Plague, tularemia, anthrax, leptospirosis |
| `leptospirosis` | A27 | Precipitation, flooding | Leptospirosis (Weil's disease) |
| `hansen` | A30 | - | Leprosy (Hansen's disease) |

#### Vector-Borne Diseases

| Group Name | ICD Codes | Climate Factors | Description |
|------------|-----------|-----------------|-------------|
| `vector_borne` | A75-A79, A90-A99 | Temperature, precipitation, humidity | All vector-borne diseases |
| `dengue` | A90-A91 | Temperature, precipitation, humidity | Classic and hemorrhagic dengue |
| `chikungunya` | A92.0 | Temperature, precipitation | Chikungunya fever |
| `zika` | A92.5, P35.4, Q02 | Temperature, precipitation | Zika virus and associated microcephaly |
| `yellow_fever` | A95 | Temperature, precipitation, deforestation | Sylvatic and urban yellow fever |
| `malaria` | B50-B54 | Temperature, precipitation, humidity | Malaria by Plasmodium |
| `leishmaniasis` | B55 | Temperature, humidity, deforestation | Visceral and cutaneous leishmaniasis |
| `chagas` | B57 | Temperature, housing conditions | Chagas disease (American trypanosomiasis) |

#### Waterborne and Parasitic Diseases

| Group Name | ICD Codes | Climate Factors | Description |
|------------|-----------|-----------------|-------------|
| `schistosomiasis` | B65 | Water availability, temperature | Schistosomiasis (bilharzia) |
| `helminthiasis` | B65-B83 | Temperature, humidity, sanitation | Helminth infections (worms) |

---

### 🎗️ Neoplasms (Chapter II: C00-D48)

| Group Name | ICD Codes | Climate Factors | Description |
|------------|-----------|-----------------|-------------|
| `skin_cancer` | C43-C44 | UV radiation, temperature | Melanoma and other skin cancers |
| `respiratory_cancer` | C30-C39 | Air pollution | Lung, bronchus, and trachea cancer |

---

### 🩺 Endocrine, Nutritional and Metabolic Diseases (Chapter IV: E00-E90)

| Group Name | ICD Codes | Climate Factors | Description |
|------------|-----------|-----------------|-------------|
| `diabetes` | E10-E14 | Temperature, heatwaves | Type 1, type 2, and other forms of diabetes |
| `malnutrition` | E40-E46 | Drought, food security | Protein-energy malnutrition |

---

### ❤️ Cardiovascular Diseases (Chapter IX: I00-I99)

| Group Name | ICD Codes | Climate Factors | Description |
|------------|-----------|-----------------|-------------|
| `cardiovascular` | I00-I99 | Temperature, heatwaves, cold spells | All diseases of the circulatory system |
| `ischemic_heart` | I20-I25 | Temperature, heatwaves, cold spells, air pollution | Myocardial infarction, angina, and other ischemic diseases |
| `acute_myocardial_infarction` | I21-I22 | Temperature, air pollution | Heart attack |
| `cerebrovascular` | I60-I69 | Temperature, heatwaves, cold spells | Stroke, cerebral hemorrhage, and others |
| `stroke` | I63-I64 | Temperature, air pollution | Ischemic and unspecified stroke |
| `hypertension` | I10-I15 | Temperature, heatwaves | Essential and secondary hypertension |

---

### 🫁 Respiratory Diseases (Chapter X: J00-J99)

| Group Name | ICD Codes | Climate Factors | Description |
|------------|-----------|-----------------|-------------|
| `respiratory` | J00-J99 | Temperature, air pollution, humidity, seasonality | All diseases of the respiratory system |
| `acute_respiratory` | J00-J06, J20-J22 | Temperature, humidity, seasonality | Common cold, flu, acute bronchitis |
| `influenza_pneumonia` | J09-J18 | Temperature, humidity, seasonality | Flu and pneumonia |
| `pneumonia` | J12-J18 | Temperature, humidity, air pollution | Viral, bacterial, and unspecified pneumonia |
| `asthma` | J45-J46 | Air pollution, temperature, humidity, allergens | Asthma and status asthmaticus |
| `copd` | J40-J44 | Air pollution, temperature | Chronic obstructive pulmonary disease |

---

### 🩹 Other Systems

| Group Name | ICD Codes | Climate Factors | Description |
|------------|-----------|-----------------|-------------|
| `digestive` | K00-K93 | - | Diseases of the digestive system |
| `skin_infections` | L00-L08 | Temperature, humidity | Bacterial and viral skin infections |
| `renal` | N00-N29 | Temperature, dehydration | Diseases of kidneys and urinary tract |
| `pregnancy_complications` | O00-O99 | Temperature, heatwaves | Complications of pregnancy, childbirth, and puerperium |
| `perinatal` | P00-P96 | Temperature, heatwaves | Conditions originating in the perinatal period |
| `congenital` | Q00-Q99 | - | Congenital malformations and chromosomal abnormalities |
| `microcephaly` | Q02 | Temperature, precipitation | Microcephaly (Zika-associated) |
| `ill_defined` | R00-R99 | - | Symptoms, signs, and abnormal findings |

---

### 🚑 Injuries and External Causes (Chapters XIX-XX: S00-Y98)

| Group Name | ICD Codes | Climate Factors | Description |
|------------|-----------|-----------------|-------------|
| `injuries` | S00-T98 | Extreme weather, flooding, heatwaves | Injuries, poisonings, and other external causes |
| `transport_accidents` | V01-V99 | Extreme weather, flooding | Traffic and transport accidents |
| `drowning` | W65-W74 | Flooding, precipitation | Drowning and submersion |
| `heat_exposure` | X30 | Temperature, heatwaves | Exposure to excessive natural heat |
| `cold_exposure` | X31 | Temperature, cold spells | Exposure to excessive natural cold |
| `natural_disasters` | X36-X39 | Extreme weather, precipitation, flooding | Victims of lightning, earthquakes, floods, storms |

---

### 🌡️ Special Climate-Health Groups

These are **composite groups** designed specifically for climate-health research:

| Group Name | ICD Codes | Climate Factors | Description |
|------------|-----------|-----------------|-------------|
| `heat_related` | E86, T67, X30 | Temperature, heatwaves | Dehydration, heat stroke, heat exhaustion |
| `waterborne` | A00-A09, A27 | Precipitation, flooding, water quality | Diseases transmitted through contaminated water |
| `air_pollution_related` | J20-J22, J40-J46, I20-I25, I60-I69 | Air pollution, temperature | Respiratory and cardiovascular diseases associated with pollution |
| `climate_sensitive_all` | Multiple chapters | All climate factors | Complete set of diseases with documented climate sensitivity |

---

### 👶 Age-Specific Groups

| Group Name | ICD Codes | Climate Factors | Description |
|------------|-----------|-----------------|-------------|
| `pediatric_respiratory` | J00-J06, J09-J18, J20-J22 | Temperature, humidity, air pollution | Acute respiratory infections in children |
| `elderly_cardiovascular` | I20-I25, I60-I69 | Temperature, heatwaves, cold spells, air pollution | Heart and cerebrovascular diseases in elderly population |

---

### 🔬 Syndromic Groups (for surveillance)

| Group Name | ICD Codes | Climate Factors | Description |
|------------|-----------|-----------------|-------------|
| `fever_syndrome` | A90-A99, B50-B54 | Temperature, precipitation, humidity | Diseases characterized by fever (syndromic surveillance) |
| `respiratory_syndrome` | J00-J22, J40-J46 | Temperature, air pollution, humidity | Acute and chronic respiratory diseases (syndromic surveillance) |
| `diarrheal_syndrome` | A00-A09 | Temperature, precipitation, flooding | Acute diarrheal diseases (syndromic surveillance) |

---

## Usage Examples

### Example 1: Basic Filtering by Disease Group

```r
library(climasus4r)

# Load SIM mortality data
sim_data <- sus_data_import(
  system = "SIM-DO",
  states = "SP",
  years = 2020) %>% 
  sus_data_clean_enconding() %>% 
  sus_data_standardize()

# Filter cardiovascular deaths
df_cardio <- sus_data_filter_cid(
  sim_data,
  disease_group = "cardiovascular",
  lang = "pt"
)

# Output (Portuguese):
# ═══ Grupo Epidemiológico de Doenças ═══
# ℹ Grupo: Doenças cardiovasculares
# ℹ Descrição: Todas as doenças do aparelho circulatório
# ℹ Códigos CID: I00-I99
# ⚠ Grupo sensível a variáveis climáticas
# ℹ Fatores climáticos: temperature, heatwaves, cold_spells
# ℹ Registros originais: 150.234
# ✔ Registros filtrados: 45.678
# ℹ Percentual retido: 30.4%
```

### Example 2: Climate-Sensitive Diseases

```r
# Filter all climate-sensitive diseases
df_climate <- sus_data_filter_cid(
  sim_data,
  disease_group = "climate_sensitive_all",
  lang = "en"
)

# Get only vector-borne diseases
df_vector <- sus_data_filter_cid(
  sinan_data,
  disease_group = "vector_borne",
  lang = "en"
)
```

### Example 3: Specific Diseases for Research

```r
# Dengue cases
df_dengue <- sus_data_filter_cid(
  sinan_dengue,
  disease_group = "dengue",
  lang = "pt"
)

# Acute myocardial infarction
df_ami <- sus_data_filter_cid(
  sih_data,
  disease_group = "acute_myocardial_infarction",
  lang = "en"
)

# Respiratory diseases in children
df_pediatric <- sus_data_filter_cid(
  sih_data,
  disease_group = "pediatric_respiratory",
  lang = "es"
)
```

### Example 4: Exploring Available Groups

```r
# List all groups in Portuguese
all_groups <- list_disease_groups(lang = "pt")
print(all_groups)

# List only climate-sensitive groups
climate_groups <- list_disease_groups(
  climate_sensitive_only = TRUE,
  lang = "en"
)

# Get details about a specific group
get_disease_group_details("dengue", lang = "pt")

# Output:
# ═══ Grupo de Doenças: Dengue ═══
# ℹ Nome interno: dengue
# ℹ Descrição: Dengue clássica e hemorrágica
# ℹ Códigos CID: A90, A91
# ℹ Sensível ao clima: Sim
# ℹ Fatores climáticos: temperature, precipitation, humidity
```

### Example 5: Comparing Groups vs Explicit Codes

```r
# Traditional way (requires ICD knowledge)
df_cardio_old <- sus_data_filter_cid(
  sim_data,
  icd_codes = "I00-I99",
  lang = "en"
)

# New way (easier, more semantic)
df_cardio_new <- sus_data_filter_cid(
  sim_data,
  disease_group = "cardiovascular",
  lang = "en"
)

# Both produce the same result!
identical(df_cardio_old, df_cardio_new)  # TRUE
```

---

## Technical Details

### How Disease Groups Work

1. **Internal Dictionary**: All groups are stored in `.icd_disease_groups` (not exported)
2. **Automatic Translation**: When `disease_group` is provided, it's automatically converted to ICD codes
3. **Validation**: The function validates that the group exists and provides helpful error messages
4. **Metadata**: Each group includes:
   - ICD code ranges
   - Multilingual labels and descriptions
   - Climate sensitivity flag
   - Associated climate factors

### Climate Sensitivity Classification

A disease group is marked as **climate-sensitive** if there is documented evidence that its incidence, severity, or distribution is influenced by:

- **Temperature** (heat, cold)
- **Precipitation** (flooding, drought)
- **Humidity**
- **Air pollution** (often climate-related)
- **Extreme weather events**
- **Seasonality**
- **Environmental changes** (deforestation, urbanization)

### Extending the Dictionary

Researchers can propose new groups by:

1. Identifying epidemiologically relevant disease clusters
2. Providing ICD code ranges
3. Documenting climate sensitivity (if applicable)
4. Submitting via [GitHub Issues](https://github.com/ByMaxAnjos/climasus4r/issues)

---

## References

- **ICD-10**: International Classification of Diseases, 10th Revision (WHO)
- **Climate-Health Literature**: IPCC, Lancet Countdown, WHO Climate and Health
- **Brazilian Context**: DATASUS, Ministério da Saúde
