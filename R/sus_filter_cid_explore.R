#' Interactive Disease Groups Explorer
#'
#' Opens an interactive HTML interface to explore disease groups and climate factors
#' for use with `sus_data_filter_cid()`.
#'
#' @param lang Character string specifying language. Options: `"pt"` (Portuguese,
#'   default), `"en"` (English), `"es"` (Spanish).
#' @param output Character string specifying output format. Options:
#'   \itemize{
#'     \item `"browser"` - Interactive HTML in web browser (default)
#'     \item `"console"` - Simple list in R console
#'   }
#' @param filter_climate Logical. If `TRUE`, shows only climate-sensitive diseases.
#' @param verbose Logical. If `TRUE` (default), prints informative messages.
#'
#' @return Depending on `output`:
#'   - `"browser"`: Opens HTML interface, returns invisible data.frame
#'   - `"console"`: Prints summary, returns invisible data.frame
#'
#' @details
#' This function helps users discover available disease groups without needing
#' to know specific ICD-10 codes. The interactive interface allows:
#' - Multi-select disease groups (Ctrl/Cmd + Click)
#' - Filter by climate factors (temperature, precipitation, etc.)
#' - Copy group names for use in `sus_data_filter_cid()`
#' - View ICD-10 codes and descriptions
#'
#' @examples
#' \dontrun{
#' # Open interactive explorer
#' sus_filter_cid_explore()
#'
#' # Explore only climate-sensitive diseases
#' sus_filter_cid_explore(filter_climate = TRUE)
#'
#' # Get disease group names for programmatic use
#' groups <- sus_filter_cid_explore(
#'   lang = "pt"
#' )
#'
#' # Use in sus_data_filter_cid
#' data <- sus_data_filter_cid(
#'   df = my_data,
#'   disease_group = groups[1]
#' )
#' }
#'
#' @export
#' @importFrom cli cli_h1 cli_h2 cli_h3 cli_alert_info cli_alert_success 
#' @importFrom cli cli_alert_warning cli_rule cli_bullets
sus_filter_cid_explore <- function(lang = "pt",
                                   output = "browser",
                                   filter_climate = FALSE,
                                   verbose = TRUE) {
  
  # ==========================================================================
  # 1. VALIDATION
  # ==========================================================================
  
  valid_langs <- c("pt", "en", "es")
  if (!lang %in% valid_langs) {
    cli::cli_abort("Invalid language. Options: {paste(valid_langs, collapse = ', ')}")
  }
  
  valid_outputs <- c("browser", "console")
  if (!output %in% valid_outputs) {
    cli::cli_abort("Invalid output. Options: {paste(valid_outputs, collapse = ', ')}")
  }
  
  # ==========================================================================
  # 2. LOAD DISEASE GROUPS DATA
  # ==========================================================================
  
  if (verbose) {
    cli::cli_alert_info("Loading disease groups...")
  }
  
  # Get disease groups data (embedded in function)
  groups_df <- get_disease_groups_data()
  
  # Filter by climate if requested
  if (filter_climate) {
    groups_df <- groups_df[groups_df$climate_sensitive, ]
    if (verbose) {
      cli::cli_alert_info("Filtered to {nrow(groups_df)} climate-sensitive groups")
    }
  }
  
  # ==========================================================================
  # 3. GENERATE OUTPUT
  # ==========================================================================

  if (output == "console") {
    # Console output
    if (verbose) {
      cli::cli_h1("Disease Groups Explorer")
      cli::cli_rule()
      
      cli::cli_alert_info("Total groups: {nrow(groups_df)}")
      cli::cli_alert_info("Categories: {paste(unique(groups_df$category), collapse = ', ')}")
      cli::cli_alert_info("Climate-sensitive: {sum(groups_df$climate_sensitive)}")
      cli::cli_alert_info("Language: {lang}")
      cli::cli_rule()
      
      # Summary by category
      for (cat in unique(groups_df$category)) {
        cat_groups <- groups_df[groups_df$category == cat, ]
        n_total <- nrow(cat_groups)
        n_climate <- sum(cat_groups$climate_sensitive)
        
        cli::cli_h3("{cat} ({n_total} groups)")
        cli::cli_bullets(c(
          "*" = "Climate-sensitive: {n_climate}",
          "*" = "Sample groups: {paste(head(cat_groups$name, 3), collapse = ', ')}"
        ))
        cli::cli_rule()
      }
      
      cli::cli_alert_info("Use {.code output = 'codes'} to get all group names")
      cli::cli_alert_info("Use {.code output = 'browser'} for interactive interface")
    }
    
    return(invisible(groups_df))
  }
  
  # ==========================================================================
  # 4. BROWSER OUTPUT (HTML INTERFACE)
  # ==========================================================================
  
  if (verbose) {
    cli::cli_alert_info("Generating interactive HTML interface...")
  }
  
  # Generate HTML
  html_content <- generate_disease_groups_html(groups_df, lang)
  
  # Write to temporary file
  temp_file <- tempfile(fileext = ".html")
  writeLines(html_content, temp_file, useBytes = TRUE)
  
  if (verbose) {
    cli::cli_alert_success("Opening interactive explorer in browser...")
  }
  
  # Open in browser
  utils::browseURL(temp_file)
  
  return(invisible(groups_df))
}


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Get disease groups data (embedded)
#' @noRd
get_disease_groups_data <- function(lang = "pt") {
  if (!lang %in% c("pt", "en", "es")) {
    lang <- "pt"  # default
  }
  
  # Lista completa dos 58 grupos
  groups <- list(
    # Infectious diseases
    list(name = "diarrheal", icd_codes = "A00-A09", 
         climate_factors = "Temperature, precipitation, flooding",
         description_pt = "Doencas infecciosas intestinais incluindo colera, febre tifoide, shigelose",
         description_en = "Intestinal infectious diseases including cholera, typhoid, shigellosis",
         description_es = "Enfermedades infecciosas intestinales incluyendo colera, fiebre tifoidea, shigelosis",
         category = "Infectious", climate_sensitive = TRUE),
    
    list(name = "tuberculosis", icd_codes = "A15-A19",
         climate_factors = "Temperature, humidity, seasonality",
         description_pt = "Tuberculose respiratoria e outras formas",
         description_en = "Respiratory and other forms of tuberculosis",
         description_es = "Tuberculosis respiratoria y otras formas",
         category = "Infectious", climate_sensitive = TRUE),
    
    list(name = "dengue", icd_codes = "A90-A91",
         climate_factors = "Temperature, precipitation, humidity",
         description_pt = "Dengue classica e hemorragica",
         description_en = "Classic and hemorrhagic dengue",
         description_es = "Dengue clasico y hemorragico",
         category = "Infectious", climate_sensitive = TRUE),
    
    list(name = "chikungunya", icd_codes = "A92.0",
         climate_factors = "Temperature, precipitation",
         description_pt = "Febre de chikungunya",
         description_en = "Chikungunya fever",
         description_es = "Fiebre de chikungunya",
         category = "Infectious", climate_sensitive = TRUE),
    
    list(name = "zika", icd_codes = "A92.5, P35.4, Q02",
         climate_factors = "Temperature, precipitation",
         description_pt = "Zika virus e microcefalia associada",
         description_en = "Zika virus and associated microcephaly",
         description_es = "Virus Zika y microcefalia asociada",
         category = "Infectious", climate_sensitive = TRUE),
    
    list(name = "malaria", icd_codes = "B50-B54",
         climate_factors = "Temperature, precipitation, humidity",
         description_pt = "Malaria por Plasmodium",
         description_en = "Malaria by Plasmodium",
         description_es = "Malaria por Plasmodium",
         category = "Infectious", climate_sensitive = TRUE),
    
    list(name = "leishmaniasis", icd_codes = "B55",
         climate_factors = "Temperature, humidity, deforestation",
         description_pt = "Leishmaniose visceral e cutanea",
         description_en = "Visceral and cutaneous leishmaniasis",
         description_es = "Leishmaniasis visceral y cutanea",
         category = "Infectious", climate_sensitive = TRUE),
    
    list(name = "chagas", icd_codes = "B57",
         climate_factors = "Temperature, housing conditions",
         description_pt = "Doenca de Chagas (tripanossomiase americana)",
         description_en = "Chagas disease (American trypanosomiasis)",
         description_es = "Enfermedad de Chagas (tripanosomiasis americana)",
         category = "Infectious", climate_sensitive = TRUE),
        
    list(name = "zoonotic_bacterial", icd_codes = "A20-A28",
         climate_factors = "Precipitation, flooding, temperature",
         description_pt = "Peste, tularemia, antraz, leptospirose",
         description_en = "Plague, tularemia, anthrax, leptospirosis",
         description_es = "Peste, tularemia, antrax, leptospirosis",
         category = "Infectious", climate_sensitive = TRUE),
    
    list(name = "leptospirosis", icd_codes = "A27",
         climate_factors = "Precipitation, flooding",
         description_pt = "Leptospirose (doenca de Weil)",
         description_en = "Leptospirosis (Weil's disease)",
         description_es = "Leptospirosis (enfermedad de Weil)",
         category = "Infectious", climate_sensitive = TRUE),
    
    list(name = "hansen", icd_codes = "A30",
         climate_factors = "None",
         description_pt = "Hanseniase (lepra)",
         description_en = "Leprosy (Hansen's disease)",
         description_es = "Lepra (enfermedad de Hansen)",
         category = "Infectious", climate_sensitive = FALSE),
    
    list(name = "yellow_fever", icd_codes = "A95",
         climate_factors = "Temperature, precipitation, deforestation",
         description_pt = "Febre amarela silvestre e urbana",
         description_en = "Sylvatic and urban yellow fever",
         description_es = "Fiebre amarilla selvatica y urbana",
         category = "Infectious", climate_sensitive = TRUE),
    
    list(name = "schistosomiasis", icd_codes = "B65",
         climate_factors = "Water availability, temperature",
         description_pt = "Esquistossomose (barriga d'agua)",
         description_en = "Schistosomiasis (bilharzia)",
         description_es = "Esquistosomiasis (bilharzia)",
         category = "Infectious", climate_sensitive = TRUE),
    
    list(name = "helminthiasis", icd_codes = "B65-B83",
         climate_factors = "Temperature, humidity, sanitation",
         description_pt = "Infeccoes por helmintos (vermes)",
         description_en = "Helminth infections (worms)",
         description_es = "Infecciones por helmintos (gusanos)",
         category = "Infectious", climate_sensitive = TRUE),
    
    list(name = "vector_borne", icd_codes = "A90-A92, A95, B50-B55",
         climate_factors = "Temperature, precipitation, humidity",
         description_pt = "Dengue, febre amarela, zika, chikungunya, malaria, leishmaniose",
         description_en = "Dengue, yellow fever, zika, chikungunya, malaria, leishmaniasis",
         description_es = "Dengue, fiebre amarilla, zika, chikungunya, malaria, leishmaniasis",
         category = "Infectious", climate_sensitive = TRUE),
    
    # Cardiovascular
    list(name = "cardiovascular", icd_codes = "I00-I99",
         climate_factors = "Temperature, heatwaves, cold spells",
         description_pt = "Doencas do coracao e sistema circulatorio",
         description_en = "Heart and circulatory system diseases",
         description_es = "Enfermedades del corazon y sistema circulatorio",
         category = "Cardiovascular", climate_sensitive = TRUE),
    
    list(name = "ischemic_heart", icd_codes = "I20-I25",
         climate_factors = "Temperature, heatwaves, cold spells, air pollution",
         description_pt = "Doenca arterial coronariana, infarto do miocardio",
         description_en = "Coronary artery disease, myocardial infarction",
         description_es = "Enfermedad arterial coronaria, infarto de miocardio",
         category = "Cardiovascular", climate_sensitive = TRUE),
    
    list(name = "stroke", icd_codes = "I63-I64",
         climate_factors = "Temperature, air pollution",
         description_pt = "AVC e ataque isquemico transitorio",
         description_en = "Stroke and transient ischemic attack",
         description_es = "Accidente cerebrovascular y ataque isquemico transitorio",
         category = "Cardiovascular", climate_sensitive = TRUE),
    
    # Respiratory
    list(name = "respiratory", icd_codes = "J00-J99",
         climate_factors = "Temperature, air pollution, humidity, seasonality",
         description_pt = "Doencas do sistema respiratorio",
         description_en = "Diseases of the respiratory system",
         description_es = "Enfermedades del sistema respiratorio",
         category = "Respiratory", climate_sensitive = TRUE),
    
    list(name = "pneumonia", icd_codes = "J12-J18",
         climate_factors = "Temperature, humidity, air pollution",
         description_pt = "Pneumonia viral, bacteriana e nao especificada",
         description_en = "Viral, bacterial, and unspecified pneumonia",
         description_es = "Neumonia viral, bacteriana y no especificada",
         category = "Respiratory", climate_sensitive = TRUE),
    
    list(name = "asthma", icd_codes = "J45-J46",
         climate_factors = "Air pollution, temperature, humidity, allergens",
         description_pt = "Asma e estado de mal asmatico",
         description_en = "Asthma and status asthmaticus",
         description_es = "Asma y estado asmatico",
         category = "Respiratory", climate_sensitive = TRUE),
    
    list(name = "copd", icd_codes = "J40-J44",
         climate_factors = "Air pollution, temperature",
         description_pt = "Doenca pulmonar obstrutiva cronica",
         description_en = "Chronic obstructive pulmonary disease",
         description_es = "Enfermedad pulmonar obstructiva cronica",
         category = "Respiratory", climate_sensitive = TRUE),
    
    # Injuries
    list(name = "heat_exposure", icd_codes = "X30",
         climate_factors = "Temperature, heatwaves",
         description_pt = "Exposicao ao calor natural excessivo",
         description_en = "Exposure to excessive natural heat",
         description_es = "Exposicion a calor natural excesivo",
         category = "Injuries", climate_sensitive = TRUE),
    
    list(name = "cold_exposure", icd_codes = "X31",
         climate_factors = "Temperature, cold spells",
         description_pt = "Exposicao ao frio natural excessivo",
         description_en = "Exposure to excessive natural cold",
         description_es = "Exposicion a frio natural excesivo",
         category = "Injuries", climate_sensitive = TRUE),
    
    list(name = "drowning", icd_codes = "W65-W74",
         climate_factors = "Flooding, precipitation",
         description_pt = "Afogamento e submersao",
         description_en = "Drowning and submersion",
         description_es = "Ahogamiento y sumersion",
         category = "Injuries", climate_sensitive = TRUE),
    
    # Composite groups
    list(name = "heat_related", icd_codes = "E86, T67, X30",
         climate_factors = "Temperature, heatwaves",
         description_pt = "Desidratacao, insolacao, exaustao pelo calor",
         description_en = "Dehydration, heat stroke, heat exhaustion",
         description_es = "Deshidratacion, insolacion, agotamiento por calor",
         category = "Composite", climate_sensitive = TRUE),
    
    list(name = "waterborne", icd_codes = "A00-A09, A27",
         climate_factors = "Precipitation, flooding, water quality",
         description_pt = "Doencas transmitidas pela agua contaminada",
         description_en = "Diseases transmitted through contaminated water",
         description_es = "Enfermedades transmitidas por agua contaminada",
         category = "Composite", climate_sensitive = TRUE),
    
    # Neoplasms
    list(name = "skin_cancer", icd_codes = "C43-C44",
         climate_factors = "UV radiation, temperature",
         description_pt = "Melanoma e outros canceres de pele",
         description_en = "Melanoma and other skin cancers",
         description_es = "Melanoma y otros canceres de piel",
         category = "Neoplasms", climate_sensitive = TRUE),
    
    list(name = "respiratory_cancer", icd_codes = "C33-C34",
         climate_factors = "Air pollution",
         description_pt = "Cancer de pulmao, bronquios e traqueia",
         description_en = "Lung, bronchus, and trachea cancer",
         description_es = "Cancer de pulmon, bronquios y traquea",
         category = "Neoplasms", climate_sensitive = TRUE),
    
    # Endocrine diseases
    list(name = "diabetes", icd_codes = "E10-E14",
         climate_factors = "Temperature, heatwaves",
         description_pt = "Diabetes tipo 1, tipo 2 e outras formas",
         description_en = "Type 1, type 2, and other forms of diabetes",
         description_es = "Diabetes tipo 1, tipo 2 y otras formas",
         category = "Endocrine", climate_sensitive = TRUE),
    
    list(name = "malnutrition", icd_codes = "E40-E46",
         climate_factors = "Drought, food security",
         description_pt = "Desnutricao proteico-calorica",
         description_en = "Protein-energy malnutrition",
         description_es = "Desnutricion proteico-energetica",
         category = "Endocrine", climate_sensitive = TRUE),
    
    # Mental disorders
    list(name = "mental_disorders", icd_codes = "F00-F99",
         climate_factors = "Temperature extremes, seasonality, natural disasters",
         description_pt = "Depressao, ansiedade, esquizofrenia, transtornos por uso de substancias",
         description_en = "Depression, anxiety, schizophrenia, substance use disorders",
         description_es = "Depresion, ansiedad, esquizofrenia, trastornos por uso de sustancias",
         category = "Mental", climate_sensitive = TRUE),
    
    # Neurological disorders
    list(name = "neurological_disorders", icd_codes = "G00-G99",
         climate_factors = "Temperature, humidity, seasonality",
         description_pt = "Doencas do sistema nervoso (inclui meningite, epilepsia, enxaqueca)",
         description_en = "Diseases of the nervous system (includes meningitis, epilepsy, migraine)",
         description_es = "Enfermedades del sistema nervoso (incluye meningitis, epilepsia, migrana)",
         category = "Neurological", climate_sensitive = TRUE),
    
    # Additional Cardiovascular
    list(name = "cerebrovascular", icd_codes = "I60-I69",
         climate_factors = "Temperature, air pollution",
         description_pt = "AVC e ataque isquemico transitorio",
         description_en = "Stroke and transient ischemic attack",
         description_es = "Accidente cerebrovascular y ataque isquemico transitorio",
         category = "Cardiovascular", climate_sensitive = TRUE),
    
    list(name = "hypertension", icd_codes = "I10-I15",
         climate_factors = "Temperature, seasonality",
         description_pt = "Pressao arterial elevada",
         description_en = "High blood pressure",
         description_es = "Presion arterial elevada",
         category = "Cardiovascular", climate_sensitive = TRUE),
    
    list(name = "heart_failure", icd_codes = "I50",
         climate_factors = "Temperature, humidity",
         description_pt = "Insuficiencia cardiaca cronica e aguda",
         description_en = "Chronic and acute heart failure",
         description_es = "Insuficiencia cardiaca cronica y aguda",
         category = "Cardiovascular", climate_sensitive = TRUE),
    
    list(name = "arrhythmias", icd_codes = "I47-I49",
         climate_factors = "Temperature, heatwaves",
         description_pt = "Ritmos cardiacos anormais",
         description_en = "Abnormal heart rhythms",
         description_es = "Ritmos cardiacos anormales",
         category = "Cardiovascular", climate_sensitive = TRUE),
    
    # Additional Respiratory
    list(name = "upper_respiratory", icd_codes = "J00-J06",
         climate_factors = "Temperature, humidity, seasonality",
         description_pt = "Resfriado, gripe, bronquite aguda",
         description_en = "Common cold, flu, acute bronchitis",
         description_es = "Resfriado comun, gripe, bronquitis aguda",
         category = "Respiratory", climate_sensitive = TRUE),
    
    list(name = "influenza_pneumonia", icd_codes = "J09-J18",
         climate_factors = "Temperature, humidity, seasonality",
         description_pt = "Gripe e pneumonia",
         description_en = "Flu and pneumonia",
         description_es = "Gripe y neumonia",
         category = "Respiratory", climate_sensitive = TRUE),
    
    # Other categories
    list(name = "digestive", icd_codes = "K00-K93",
         climate_factors = "None",
         description_pt = "Doencas do aparelho digestivo",
         description_en = "Diseases of the digestive system",
         description_es = "Enfermedades del sistema digestivo",
         category = "Digestive", climate_sensitive = FALSE),
    
    list(name = "skin_infections", icd_codes = "L00-L08",
         climate_factors = "Temperature, humidity",
         description_pt = "Infeccoes bacterianas e virais da pele",
         description_en = "Bacterial and viral skin infections",
         description_es = "Infecciones bacterianas y virales de la piel",
         category = "Skin", climate_sensitive = TRUE),
    
    list(name = "renal", icd_codes = "N00-N39",
         climate_factors = "Temperature, dehydration",
         description_pt = "Doencas dos rins e vias urinarias",
         description_en = "Diseases of kidneys and urinary tract",
         description_es = "Enfermedades de rinones y vias urinarias",
         category = "Genitourinary", climate_sensitive = TRUE),
    
    list(name = "pregnancy_complications", icd_codes = "O00-O99",
         climate_factors = "Temperature, heatwaves",
         description_pt = "Complicacoes da gravidez, parto e puerperio",
         description_en = "Complications of pregnancy, childbirth, and puerperium",
         description_es = "Complicaciones del embarazo, parto y puerperio",
         category = "Pregnancy", climate_sensitive = TRUE),
    
    list(name = "perinatal", icd_codes = "P00-P96",
         climate_factors = "Temperature, heatwaves",
         description_pt = "Afeccoes originadas no periodo perinatal",
         description_en = "Conditions originating in the perinatal period",
         description_es = "Afecciones originadas en el periodo perinatal",
         category = "Perinatal", climate_sensitive = TRUE),
    
    list(name = "congenital", icd_codes = "Q00-Q99",
         climate_factors = "None",
         description_pt = "Malformacoes congenitas, deformidades e anomalias cromossomicas",
         description_en = "Congenital malformations, deformations, and chromosomal abnormalities",
         description_es = "Malformaciones congenitas, deformaciones y anomalias cromosomicas",
         category = "Congenital", climate_sensitive = FALSE),
    
    list(name = "microcephaly", icd_codes = "Q02",
         climate_factors = "Temperature, precipitation",
         description_pt = "Microcefalia (associada a Zika)",
         description_en = "Microcephaly (Zika-associated)",
         description_es = "Microcefalia (asociada a Zika)",
         category = "Congenital", climate_sensitive = TRUE),
    
    list(name = "ill_defined", icd_codes = "R00-R99",
         climate_factors = "None",
         description_pt = "Sintomas, sinais e achados anormais",
         description_en = "Symptoms, signs, and abnormal findings",
         description_es = "Sintomas, signos y hallazgos anormales",
         category = "Ill-defined", climate_sensitive = FALSE),
    
    # External causes
    list(name = "injuries", icd_codes = "S00-T98",
         climate_factors = "Extreme weather, flooding, heatwaves",
         description_pt = "Lesoes, envenenamentos e outras causas externas",
         description_en = "Injuries, poisonings, and other external causes",
         description_es = "Lesiones, envenenamientos y otras causas externas",
         category = "Injuries", climate_sensitive = TRUE),
    
    list(name = "transport_accidents", icd_codes = "V01-V99",
         climate_factors = "Extreme weather, flooding",
         description_pt = "Acidentes de transito e transporte",
         description_en = "Traffic and transport accidents",
         description_es = "Accidentes de trafico y transporte",
         category = "External", climate_sensitive = TRUE),
    
    list(name = "natural_disasters", icd_codes = "X33-X39",
         climate_factors = "Extreme weather, precipitation, flooding",
         description_pt = "Vitimas de raios, terremotos, inundacoes, tempestades",
         description_en = "Victims of lightning, earthquakes, floods, storms",
         description_es = "Victimas de rayos, terremotos, inundaciones, tormentas",
         category = "External", climate_sensitive = TRUE),
    
    list(name = "suicide_self_harm", icd_codes = "X60-X84",
         climate_factors = "Temperature extremes, seasonality, sunlight exposure, heat waves, natural disasters",
         description_pt = "Lesoes autoprovocadas intencionalmente e suicidio",
         description_en = "Intentional self-harm and suicide",
         description_es = "Lesiones autoinfligidas intencionalmente y suicidio",
         category = "External", climate_sensitive = TRUE),
    
    # Climate-Health groups
    list(name = "air_pollution_related", icd_codes = "J40-J47, I20-I25",
         climate_factors = "Air pollution, temperature",
         description_pt = "Doencas respiratorias e cardiovasculares associadas a poluicao",
         description_en = "Respiratory and cardiovascular diseases associated with pollution",
         description_es = "Enfermedades respiratorias y cardiovasculares asociadas a la contaminacion",
         category = "Climate-Health", climate_sensitive = TRUE),
    
    list(name = "climate_sensitive_all", icd_codes = "Multiple",
         climate_factors = "Temperature, precipitation, humidity, air pollution, extreme weather",
         description_pt = "Conjunto completo de doencas com sensibilidade climatica documentada",
         description_en = "Complete set of diseases with documented climate sensitivity",
         description_es = "Conjunto completo de enfermedades con sensibilidad climatica documentada",
         category = "Climate-Health", climate_sensitive = TRUE),
    
    # Age-Specific groups
    list(name = "pediatric_respiratory", icd_codes = "J00-J22",
         climate_factors = "Temperature, humidity, air pollution",
         description_pt = "Infeccoes respiratorias agudas em criancas",
         description_en = "Acute respiratory infections in children",
         description_es = "Infecciones respiratorias agudas en ninos",
         category = "Age-Specific", climate_sensitive = TRUE),
    
    list(name = "elderly_cardiovascular", icd_codes = "I20-I25, I60-I69",
         climate_factors = "Temperature, heatwaves, cold spells, air pollution",
         description_pt = "Doencas cardiacas e cerebrovasculares em populacao idosa",
         description_en = "Heart and cerebrovascular diseases in elderly population",
         description_es = "Enfermedades cardiacas y cerebrovasculares en poblacion anciana",
         category = "Age-Specific", climate_sensitive = TRUE),
    
    # Syndromic groups
    list(name = "fever_syndrome", icd_codes = "A00-B99, R50",
         climate_factors = "Temperature, precipitation, humidity",
         description_pt = "Doencas caracterizadas por febre (vigilancia sindromica)",
         description_en = "Diseases characterized by fever (syndromic surveillance)",
         description_es = "Enfermedades caracterizadas por fiebre (vigilancia sindromica)",
         category = "Syndromic", climate_sensitive = TRUE),
    
    list(name = "respiratory_syndrome", icd_codes = "J00-J99",
         climate_factors = "Temperature, air pollution, humidity",
         description_pt = "Doencas respiratorias agudas e cronicas (vigilancia sindromica)",
         description_en = "Acute and chronic respiratory diseases (syndromic surveillance)",
         description_es = "Enfermedades respiratorias agudas y cronicas (vigilancia sindromica)",
         category = "Syndromic", climate_sensitive = TRUE),
    
    list(name = "diarrheal_syndrome", icd_codes = "A00-A09",
         climate_factors = "Temperature, precipitation, flooding",
         description_pt = "Doencas diarreicas agudas (vigilancia sindromica)",
         description_en = "Acute diarrheal diseases (syndromic surveillance)",
         description_es = "Enfermedades diarreicas agudas (vigilancia sindromica)",
         category = "Syndromic", climate_sensitive = TRUE)
  )
  
  # Converter para data.frame
  df_list <- lapply(groups, function(g) {
    description_col <- paste0("description_", lang)
    description <- g[[description_col]]
    
    data.frame(
      name = g$name,
      icd_codes = g$icd_codes,
      climate_factors = g$climate_factors,
      description = description,
      category = g$category,
      climate_sensitive = g$climate_sensitive,
      stringsAsFactors = FALSE
    )
  })
  
  df <- do.call(rbind, df_list)
  
  return(df)
}


#' Generate HTML interface for disease groups
#' @noRd
generate_disease_groups_html <- function(groups_df, lang) {
  
  # Get translations
  text <- get_ui_text_disease_groups(lang)
  
  # Build table rows
  table_rows <- ""
  current_category <- ""
  
  for (i in 1:nrow(groups_df)) {
    row <- groups_df[i, ]
    
    # Add category header if new category
    if (row$category != current_category) {
      current_category <- row$category
      category_count <- sum(groups_df$category == current_category)
      
      table_rows <- paste0(table_rows, sprintf(
        '<tr class="category-header">
          <td colspan="6" class="category-title">
            <div class="category-header-content">
              <span class="category-icon">%s</span>
              <span class="category-name">%s</span>
              <span class="category-stats">%d %s</span>
            </div>
          </td>
        </tr>',
        get_category_icon(current_category),
        current_category,
        category_count,
        text$groups_label
      ))
    }
    
    # Climate factors as chips
    climate_chips <- ""
    if (row$climate_sensitive && row$climate_factors != "-") {
      factors <- strsplit(row$climate_factors, ",")[[1]]
      for (factor in factors) {
        factor <- trimws(factor)
        climate_chips <- paste0(climate_chips, sprintf(
          '<span class="climate-chip climate-%s" title="%s">%s</span>',
          tolower(gsub(" ", "-", factor)),
          factor,
          factor
        ))
      }
    }
    
    # Climate sensitive badge
    climate_badge <- ""
    if (row$climate_sensitive) {
      climate_badge <- sprintf(
        '<span class="climate-badge" title="%s">\U0001F321</span>',
        text$climate_sensitive
      )
    }
    
    # Row
    table_rows <- paste0(table_rows, sprintf(
      '<tr class="group-row" data-name="%s" data-category="%s" data-climate="%s">
        <td class="name-cell">
          <div class="name-container">
            <code class="group-name">%s</code>
            <span class="copy-icon" onclick="copySingleGroup(\'%s\')" title="%s">\U0001F4C4</span>
          </div>
        </td>
        <td class="icd-cell"><code>%s</code></td>
        <td class="climate-cell">%s</td>
        <td class="description-cell" title="%s">%s</td>
        <td class="badge-cell">%s</td>
      </tr>',
      row$name,
      row$category,
      tolower(row$climate_sensitive),
      row$name,
      row$name,
      text$copy_single,
      row$icd_codes,
      climate_chips,
      row$description,
      substr(row$description, 1, 80),
      climate_badge
    ))
  }
  
  # Build complete HTML
  html <- sprintf('<!DOCTYPE html>
<html lang="%s">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>%s</title>
    %s
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>%s</h1>
            <p>%s</p>
        </div>
        
        <div class="stats-bar">
            <div>%s: <strong id="totalCount">%d</strong></div>
            <div>%s: <strong id="climateCount">%d</strong></div>
            <div>%s: <strong id="selectedCount">0</strong></div>
        </div>
        
        <div class="controls">
            <button class="btn btn-primary" onclick="copySelectedGroups()">
                <span>\U0001F4C4</span> %s
            </button>
            <button class="btn btn-secondary" onclick="filterClimate()">
                <span>\U0001F321</span> %s
            </button>
            <button class="btn btn-secondary" onclick="clearFilters()">
                <span>\U0001F504</span> %s
            </button>
        </div>
        
        <div id="codeHelp" class="code-help">
            <strong>%s:</strong><br>
            <code id="codeOutput">c()</code>
        </div>
        
        <div class="table-container">
            <table id="groupsTable">
                <thead>
                    <tr>
                        <th width="200">%s</th>
                        <th width="150">%s</th>
                        <th width="250">%s</th>
                        <th>%s</th>
                        <th width="80">%s</th>
                    </tr>
                </thead>
                <tbody>
                    %s
                </tbody>
            </table>
        </div>
        
        <div class="tips">
            <h3>%s</h3>
            <ul>
                <li>%s</li>
                <li>%s</li>
                <li>%s</li>
            </ul>
        </div>
        
        <div class="footer">
            %s<br>
            <small>Generated at %s</small>
        </div>
    </div>
    %s
</body>
</html>',
    lang,
    text$title,
    get_css_styles(),
    text$title,
    text$subtitle,
    text$total_groups,
    nrow(groups_df),
    text$climate_sensitive,
    sum(groups_df$climate_sensitive),
    text$selected,
    text$copy_selected,
    text$filter_climate,
    text$clear_filters,
    text$ready_to_use,
    text$group_name,
    text$icd_codes,
    text$climate_factors,
    text$description,
    text$climate_label,
    table_rows,
    text$tips_title,
    text$tip1,
    text$tip2,
    text$tip3,
    text$footer,
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    get_javascript(text)
  )
  
  return(html)
}


#' Get UI text translations
#' @noRd
get_ui_text_disease_groups <- function(lang) {
  if (lang == "pt") {
    return(list(
      title = "Explorador de Grupos de Doencas - CLIMASUS4R",
      subtitle = "Explore grupos de doencas e fatores climaticos para analises de clima e saude",
      total_groups = "Total de grupos",
      climate_sensitive = "Sensiveis ao clima",
      selected = "Selecionados",
      copy_selected = "Copiar Selecionados",
      filter_climate = "Filtrar Clima",
      clear_filters = "Limpar Filtros",
      ready_to_use = "Pronto para usar em sus_data_filter_cid()",
      group_name = "Grupo",
      icd_codes = "Codigos CID-10",
      climate_factors = "Fatores Climaticos",
      description = "Descricao",
      climate_label = "Clima",
      groups_label = "grupos",
      copy_single = "Copiar este grupo",
      tips_title = "Dicas de Uso",
      tip1 = "Clique em uma linha para selecionar um grupo",
      tip2 = "Use Ctrl/Cmd + Clique para selecao multipla",
      tip3 = "Filtre por fatores climaticos usando os chips coloridos",
      footer = "CLIMASUS4R - Climate, Health & Environmental Data Integration",
      no_selection = "Nenhum grupo selecionado"
    ))
  } else if (lang == "es") {
    return(list(
      title = "Explorador de Grupos de Enfermedades - CLIMASUS4R",
      subtitle = "Explore grupos de enfermedades y factores climaticos para analisis de clima y salud",
      total_groups = "Total de grupos",
      climate_sensitive = "Sensibles al clima",
      selected = "Seleccionados",
      copy_selected = "Copiar Seleccionados",
      filter_climate = "Filtrar Clima",
      clear_filters = "Limpiar Filtros",
      ready_to_use = "Listo para usar en sus_data_filter_cid()",
      group_name = "Grupo",
      icd_codes = "Cadigos CIE-10",
      climate_factors = "Factores Climaticos",
      description = "Descripcion",
      climate_label = "Clima",
      groups_label = "grupos",
      copy_single = "Copiar este grupo",
      tips_title = "Consejos de Uso",
      tip1 = "Haga clic en una fila para seleccionar un grupo",
      tip2 = "Use Ctrl/Cmd + Clic para seleccion multiple",
      tip3 = "Filtre por factores climaticos usando los chips de colores",
      footer = "CLIMASUS4R - Integracion de Datos de Clima, Salud y Ambiente",
      no_selection = "Ningun grupo seleccionado"
    ))
  } else {
    return(list(
      title = "Disease Groups Explorer - CLIMASUS4R",
      subtitle = "Explore disease groups and climate factors for climate-health analysis",
      total_groups = "Total groups",
      climate_sensitive = "Climate-sensitive",
      selected = "Selected",
      copy_selected = "Copy Selected",
      filter_climate = "Filter Climate",
      clear_filters = "Clear Filters",
      ready_to_use = "Ready to use in sus_data_filter_cid()",
      group_name = "Group",
      icd_codes = "ICD-10 Codes",
      climate_factors = "Climate Factors",
      description = "Description",
      climate_label = "Climate",
      groups_label = "groups",
      copy_single = "Copy this group",
      tips_title = "Usage Tips",
      tip1 = "Click a row to select a group",
      tip2 = "Use Ctrl/Cmd + Click for multiple selection",
      tip3 = "Filter by climate factors using colored chips",
      footer = "CLIMASUS4R - Climate, Health & Environmental Data Integration",
      no_selection = "No groups selected"
    ))
  }
}


#' Get category icon
#' @noRd
get_category_icon <- function(category) {
  icons <- list(
    "Infectious" = "\U0001F9A0",       #  MICROBE
    "Cardiovascular" = "\U00002764",    #  RED HEART
    "Respiratory" = "\U0001FAC1",       #  LUNGS
    "Injuries" = "\U0001F691",          #  AMBULANCE
    "Composite" = "\U0001F321",         #  THERMOMETER
    "Neoplasms" = "\U0001F397",         # REMINDER RIBBON
    "Endocrine" = "\U0001FA7A",         #  STETHOSCOPE
    "Mental" = "\U0001F9E0",            # BRAIN
    "Neurological" = "\U0001F9E0",      #  BRAIN
    "Endocrine" = "\U0001F9E0",         # BRAIN (for diabetes/metabolic)
    "Neoplasms" = "\U0001F4CA",         # BAR CHART (for statistics/cancer)
    "Digestive" = "\U0001FAD9",         #  BEANS (for digestive system)
    "Skin" = "\U0001F3FF",              # DARK SKIN TONE (representing skin)
    "Genitourinary" = "\U0001F4A9",     #  PILE OF POO (for urinary/kidney)
    "Pregnancy" = "\U0001F476",         #  BABY
    "Perinatal" = "\U0001F476",         #  BABY
    "Congenital" = "\U0001F5BC",        # FRAME WITH PICTURE (for malformations)
    "Ill-defined" = "\U00002753",       #  QUESTION MARK
    "Climate-Health" = "\U000026C5",    #  SUN BEHIND CLOUD
    "Age-Specific" = "\U0001F465",      #  BUSTS IN SILHOUETTE
    "Syndromic" = "\U0001F9EA",         #  TEST TUBE
    "External" = "\U000026A0",          #  WARNING SIGN
    "Other" = "\U0001F4C4"              #  CLIPBOARD
  )
  
  if (!is.null(icons[[category]])) {
    return(icons[[category]])
  } else {
    for (cat_name in names(icons)) {
      if (grepl(category, cat_name, ignore.case = TRUE) || 
          grepl(cat_name, category, ignore.case = TRUE)) {
        return(icons[[cat_name]])
      }
    }
  }
  
  return("\U0001F4C4")  # CLIPBOARD (default)
}


#' Get CSS styles (Climate Forest theme)
#' @noRd
get_css_styles <- function() {
  return('<style>
    /* Climate Forest Theme */
    :root {
        --forest-dark: #2C5530;
        --forest-medium: #4A7C59;
        --forest-light: #8FB996;
        --earth-dark: #8B4513;
        --earth-light: #D2691E;
        --sky-light: #E8F4F8;
        --text-dark: #2C3E50;
        --text-light: #5D6D7E;
        --success: #27AE60;
        --warning: #F39C12;
        --danger: #E74C3C;
    }
    
    * {
        margin: 0;
        padding: 0;
        box-sizing: border-box;
    }
    
    body {
        font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, 
                     "Helvetica Neue", Arial, sans-serif;
        background: linear-gradient(135deg, var(--sky-light) 0%, #f5f9fc 100%);
        color: var(--text-dark);
        line-height: 1.6;
        padding: 20px;
        min-height: 100vh;
    }
    
    .container {
        max-width: 1600px;
        margin: 0 auto;
        background: white;
        border-radius: 16px;
        box-shadow: 0 10px 40px rgba(44, 83, 48, 0.15);
        overflow: hidden;
        border: 1px solid rgba(139, 69, 19, 0.1);
    }
    
    .header {
        background: linear-gradient(135deg, #1e3a28 0%, #2C5530 100%);
        color: #f0f7f0;
        padding: 40px;
        text-align: center;
        position: relative;
        overflow: hidden;
    }
    
    .header h1 {
        font-size: 2.5em;
        margin-bottom: 10px;
        font-weight: 700;
        text-shadow: 0 2px 6px rgba(0,0,0,0.4);
        color: #8FB996;
        letter-spacing: 0.5px;
    }
    
    .header p {
        font-size: 1.2em;
        opacity: 0.95;
        color: #8FB996;
        text-shadow: 0 1px 3px rgba(0, 0, 0, 0.3);
        font-weight: 400;
    }
    
    .stats-bar {
        background: linear-gradient(to right, var(--forest-light) 0%, #B8D8C0 100%);
        padding: 20px 40px;
        display: flex;
        justify-content: space-between;
        align-items: center;
        font-size: 1.1em;
        color: var(--forest-dark);
        border-bottom: 2px solid rgba(139, 69, 19, 0.1);
    }
    
    .stats-bar strong {
        color: var(--earth-dark);
        font-size: 1.4em;
        margin-left: 5px;
    }
    
    .controls {
        padding: 25px 40px;
        background: #f8faf9;
        display: flex;
        gap: 15px;
        flex-wrap: wrap;
        border-bottom: 1px solid #e8f0e8;
    }
    
    .btn {
        padding: 12px 24px;
        border: none;
        border-radius: 8px;
        font-weight: 600;
        cursor: pointer;
        transition: all 0.3s ease;
        font-size: 1em;
        display: flex;
        align-items: center;
        gap: 8px;
    }
    
    .btn-primary {
        background: var(--forest-medium);
        color: white;
        border: 1px solid rgba(44, 83, 48, 0.2);
    }
    
    .btn-primary:hover {
        background: var(--forest-dark);
        transform: translateY(-2px);
    }
    
    .btn-secondary {
        background: var(--forest-light);
        color: var(--forest-dark);
        border: 1px solid rgba(44, 83, 48, 0.2);
    }
    
    .btn-secondary:hover {
        background: var(--forest-medium);
        color: white;
    }
    
    .code-help {
        background: linear-gradient(to right, #F1F8E9 0%, #E8F5E9 100%);
        padding: 20px;
        margin: 20px 40px;
        border-radius: 10px;
        border-left: 5px solid var(--success);
        font-family: "Consolas", "Monaco", monospace;
        font-size: 0.95em;
        display: none;
        border: 1px solid #C8E6C9;
    }
    
    .code-help.show {
        display: block;
        animation: fadeIn 0.3s ease;
    }
    
    .table-container {
        max-height: 700px;
        overflow-y: auto;
        padding: 0;
        margin: 0 20px;
        border-radius: 10px;
        border: 1px solid #e8f0e8;
    }
    
    table {
        width: 100%;
        border-collapse: collapse;
        background: white;
    }
    
    th {
        background: linear-gradient(to bottom, var(--forest-light) 0%, #9FC9A6 100%);
        padding: 18px 15px;
        text-align: left;
        font-weight: 600;
        color: var(--forest-dark);
        border-bottom: 3px solid var(--forest-medium);
        position: sticky;
        top: 0;
        z-index: 10;
        font-size: 0.95em;
        text-transform: uppercase;
        letter-spacing: 0.5px;
    }
    
    td {
        padding: 15px;
        border-bottom: 1px solid #f0f5f0;
        vertical-align: middle;
    }
    
    .category-header {
        background: linear-gradient(to right, #f8faf9 0%, #f0f7f0 100%) !important;
    }
    
    .category-title {
        padding: 15px !important;
        font-weight: 700;
        color: var(--forest-dark);
        border-bottom: 2px solid var(--forest-light);
        font-size: 1.1em;
    }
    
    .category-header-content {
        display: flex;
        align-items: center;
        gap: 15px;
    }
    
    .category-icon {
        font-size: 1.3em;
    }
    
    .category-stats {
        margin-left: auto;
        font-size: 0.9em;
        color: var(--earth-light);
        font-weight: 500;
    }
    
    .group-row {
        cursor: pointer;
        transition: all 0.2s ease;
        border-left: 4px solid transparent;
    }
    
    .group-row:hover {
        background: #f8faf9 !important;
        border-left-color: var(--forest-light);
    }
    
    .group-row.selected {
        background: #E8F5E9 !important;
        border-left-color: var(--success);
        box-shadow: inset 3px 0 0 var(--success);
    }
    
    .name-container {
        display: flex;
        align-items: center;
        gap: 10px;
    }
    
    .group-name {
        font-family: "Consolas", "Monaco", monospace;
        font-weight: 600;
        color: var(--forest-dark);
        background: #f0f7f0;
        padding: 4px 8px;
        border-radius: 4px;
        font-size: 0.95em;
    }
    
    .copy-icon {
        cursor: pointer;
        opacity: 0.5;
        transition: all 0.2s ease;
        font-size: 1.1em;
    }
    
    .copy-icon:hover {
        opacity: 1;
        transform: scale(1.2);
    }
    
    .climate-chip {
        display: inline-block;
        padding: 4px 10px;
        margin: 2px;
        border-radius: 12px;
        font-size: 0.85em;
        font-weight: 500;
        background: #E8F5E9;
        color: var(--forest-dark);
        border: 1px solid #C8E6C9;
    }
    
    .climate-chip.climate-temperature {
        background: #FFEBEE;
        color: #C62828;
        border-color: #EF9A9A;
    }
    
    .climate-chip.climate-precipitation {
        background: #E3F2FD;
        color: #1565C0;
        border-color: #90CAF9;
    }
    
    .climate-chip.climate-humidity {
        background: #F3E5F5;
        color: #6A1B9A;
        border-color: #CE93D8;
    }
    
    .climate-chip.climate-air-pollution {
        background: #FFF3E0;
        color: #E65100;
        border-color: #FFCC80;
    }
    
    .climate-badge {
        font-size: 1.3em;
        cursor: help;
    }
    
    .tips {
        padding: 30px 40px;
        background: #f8faf9;
        border-top: 1px solid #e8f0e8;
    }
    
    .tips h3 {
        color: var(--forest-dark);
        margin-bottom: 15px;
        font-size: 1.2em;
    }
    
    .tips ul {
        list-style-position: inside;
        color: var(--text-light);
        line-height: 2;
    }
    
    .tips li {
        margin-bottom: 8px;
    }
    
    .footer {
        text-align: center;
        padding: 20px;
        background: var(--forest-dark);
        color: #8FB996;
        font-size: 0.9em;
    }
    
    @keyframes fadeIn {
        from {
            opacity: 0;
            transform: translateY(-10px);
        }
        to {
            opacity: 1;
            transform: translateY(0);
        }
    }
  </style>')
}


#' Get JavaScript code
#' @noRd
get_javascript <- function(text) {
  return(sprintf('
    <script>
        // Row selection
        document.querySelectorAll(".group-row").forEach(row => {
            row.addEventListener("click", function(e) {
                if (e.target.closest(".copy-icon")) {
                    return;
                }
                
                if (e.ctrlKey || e.metaKey) {
                    this.classList.toggle("selected");
                } else {
                    this.classList.add("selected");
                    document.querySelectorAll(".group-row.selected").forEach(otherRow => {
                        if (otherRow !== this) {
                            otherRow.classList.remove("selected");
                        }
                    });
                }
                
                updateSelectedCount();
                e.stopPropagation();
            });
        });
        
        function updateSelectedCount() {
            const selected = document.querySelectorAll(".group-row.selected").length;
            document.getElementById("selectedCount").textContent = selected;
            
            const codeHelp = document.getElementById("codeHelp");
            
            if (selected > 0) {
                showCopyHelp();
                codeHelp.classList.add("show");
            } else {
                codeHelp.classList.remove("show");
            }
        }
        
        function showCopyHelp() {
            const selected = document.querySelectorAll(".group-row.selected");
            const names = Array.from(selected).map(row => row.getAttribute("data-name"));
            const codeString = \'c("\' + names.join(\'", "\') + \'")\';
            
            document.getElementById("codeOutput").textContent = codeString;
        }
        
        function copySingleGroup(name) {
            const codeString = \'c("\' + name + \'")\';
            
            navigator.clipboard.writeText(codeString).then(() => {
                showNotification("Group copied: " + name);
            });
        }
        
        function copySelectedGroups() {
            const selected = document.querySelectorAll(".group-row.selected");
            if (selected.length === 0) {
                showNotification("%s", "warning");
                return;
            }
            
            const names = Array.from(selected).map(row => row.getAttribute("data-name"));
            const codeString = \'c("\' + names.join(\'", "\') + \'")\';
            
            navigator.clipboard.writeText(codeString).then(() => {
                showNotification("\U00002705 " + selected.length + " groups copied to clipboard");
                console.log("\\nReady to use in sus_data_filter_cid():\\n");
                console.log(codeString);
            });
        }
        
        function filterClimate() {
            document.querySelectorAll(".group-row").forEach(row => {
                const isClimate = row.getAttribute("data-climate") === "true";
                row.style.display = isClimate ? "" : "none";
            });
            
            showNotification("Filtered to climate-sensitive groups");
        }
        
        function clearFilters() {
            document.querySelectorAll(".group-row").forEach(row => {
                row.style.display = "";
            });
            
            showNotification("Filters cleared");
        }
        
        function showNotification(message, type = "success") {
            const oldNotifications = document.querySelectorAll(".notification");
            oldNotifications.forEach(n => n.remove());
            
            const notification = document.createElement("div");
            notification.className = `notification notification-${type}`;
            notification.textContent = message;
            notification.style.cssText = `
                position: fixed;
                top: 20px;
                right: 20px;
                padding: 15px 20px;
                background: ${type === "success" ? "#27AE60" : "#F39C12"};
                color: white;
                border-radius: 8px;
                box-shadow: 0 4px 12px rgba(0,0,0,0.15);
                z-index: 1000;
                animation: fadeIn 0.3s ease;
                font-weight: 500;
            `;
            
            document.body.appendChild(notification);
            
            setTimeout(() => {
                notification.style.opacity = "0";
                notification.style.transform = "translateX(100px)";
                setTimeout(() => notification.remove(), 300);
            }, 3000);
        }
        
        updateSelectedCount();
    </script>
  ', text$no_selection))
}
