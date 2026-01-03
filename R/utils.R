#' Utilities functions for climasus4r
#'
#' @keywords internal
#' @name utils
NULL

#' Get translation dictionary: SIM 
#'
#' Returns a comprehensive translation dictionary for column names and values
#' across Portuguese (pt), Spanish (es), and English (en).
#'
#' @return A list containing translation mappings.
#' Get Translation Dictionary for EN
#'
#' Internal function - Returns EN translations
#' @keywords internal
#' @noRd

get_translation_dict_en <- function() {
  
  # ==========================================================================
  # COLUMN NAME TRANSLATIONS
  # Organized by category for easy maintenance and team collaboration
  # ==========================================================================
  
  columns <- c(
    
    # ------------------------------------------------------------------------
    # BASIC IDENTIFICATION AND DEATH TYPE
    # ------------------------------------------------------------------------
    "TIPOBITO" = "death_type",
    "DTOBITO" = "death_date",
    "HORAOBITO" = "death_time",
    "NUMERODO" = "death_certificate_number",
    "CAUSABAS" = "underlying_cause",
    "CAUSABAS_O" = "original_underlying_cause",
    "CB_PRE" = "pre_selection_underlying_cause",
    "CAUSAMAT" = "maternal_cause",
    
    # ------------------------------------------------------------------------
    # DECEASED PERSONAL DATA
    # ------------------------------------------------------------------------
    "NATURAL" = "birthplace_municipality",
    "CODMUNNATU" = "birthplace_municipality_code",
    "DTNASC" = "birth_date",
    "IDADE" = "age_code",
    "SEXO" = "sex",
    "RACACOR" = "race",
    "ESTCIV" = "marital_status",
    
    # ------------------------------------------------------------------------
    # DECEASED EDUCATION AND OCCUPATION
    # ------------------------------------------------------------------------
    "ESC" = "education",
    "ESC2010" = "education_2010",
    "SERIESCFAL" = "deceased_education_grade",
    "ESCFALAGRI" = "aggregated_deceased_education",
    "OCUP" = "occupation",
    
    # ------------------------------------------------------------------------
    # RESIDENCE AND OCCURRENCE LOCATION
    # ------------------------------------------------------------------------
    "CODMUNRES" = "residence_municipality_code",
    "LOCOCOR" = "death_location",
    "CODMUNOCOR" = "occurrence_municipality_code",
    
    # ------------------------------------------------------------------------
    # MOTHER DATA (FOR FETAL DEATH OR UNDER 1 YEAR)
    # ------------------------------------------------------------------------
    "IDADEMAE" = "mother_age",
    "ESCMAE" = "mother_education",
    "ESCMAE2010" = "mother_education_2010",
    "SERIESCMAE" = "mother_education_grade",
    "ESCMAEAGR1" = "aggregated_mother_education",
    "OCUPMAE" = "mother_occupation",
    "QTDFILVIVO" = "number_of_living_children",
    "QTDFILMORT" = "number_of_dead_children",
    
    # ------------------------------------------------------------------------
    # PREGNANCY AND BIRTH DATA
    # ------------------------------------------------------------------------
    "SEMAGESTAC" = "gestational_weeks",
    "GESTACAO" = "gestational_age",
    "GRAVIDEZ" = "pregnancy_type",
    "PARTO" = "delivery_type",
    "OBITOPARTO" = "death_timing_delivery",
    "MORTEPARTO" = "death_timing_partum",
    "PESO" = "birth_weight",
    "NUMERODN" = "birth_certificate_number",
    
    # ------------------------------------------------------------------------
    # CAUSES OF DEATH (DEATH CERTIFICATE LINES)
    # ------------------------------------------------------------------------
    "LINHAA" = "cause_line_a",
    "LINHAB" = "cause_line_b",
    "LINHAC" = "cause_line_c",
    "LINHAD" = "cause_line_d",
    "LINHAII" = "cause_line_ii",
    "ATESTADO" = "death_certificate",
    
    # ------------------------------------------------------------------------
    # MATERNAL AND PREGNANCY-RELATED DEATH
    # ------------------------------------------------------------------------
    "TPMORTEOCO" = "death_timing_pregnancy",
    "TPOBITOCOR" = "corrected_death_type",
    "OBITOGRAV" = "death_during_pregnancy",
    "OBITOPUERP" = "death_during_puerperium",
    
    # ------------------------------------------------------------------------
    # DEATH CIRCUMSTANCES
    # ------------------------------------------------------------------------
    "CIRCOBITO" = "death_circumstance",
    "ACIDTRAB" = "work_accident",
    "FONTE" = "information_source",
    
    # ------------------------------------------------------------------------
    # MEDICAL ASSISTANCE AND PROCEDURES
    # ------------------------------------------------------------------------
    "ASSISTMED" = "medical_assistance",
    "NECROPSIA" = "autopsy",
    "EXAME" = "examination_performed",
    "CIRURGIA" = "surgery_performed",
    
    # ------------------------------------------------------------------------
    # INVESTIGATION INFORMATION
    # ------------------------------------------------------------------------
    "DTINVESTIG" = "investigation_date",
    "DTCONINV" = "investigation_conclusion_date",
    "DTCADINV" = "investigation_registration_date",
    "DTCONCASO" = "case_conclusion_date",
    "DTCADINF" = "information_registration_date",
    "FONTENV" = "investigation_source",
    "FONTES" = "combined_sources",
    "TPRESGINFO" = "information_retrieval_type",
    "TPNIVELINV" = "investigator_level_type",
    "ALTCAUSA" = "cause_modified",
    "TPPOS" = "death_investigated",
    
    # ------------------------------------------------------------------------
    # ADMINISTRATIVE AND SYSTEM DATA
    # ------------------------------------------------------------------------
    "ORIGEM" = "record_origin",
    "CODESTAB" = "health_facility_code",
    "ATESTANTE" = "certifier",
    "DTCADASTRO" = "registration_date",
    "DTRECEBIM" = "receipt_date",
    "DTRECORIGA" = "original_receipt_date",
    "DTDIGITA" = "digitization_date",
    "NUMEROLOTE" = "batch_number",
    "STCODIFICA" = "coding_status",
    "CODIFICADO" = "coded",
    "VERSAOSIST" = "system_version",
    "VERSAOSCB" = "underlying_cause_selector_version",
    "STDOEPIDEM" = "epidemiological_death_certificate_status",
    "STIONOVA" = "new_death_certificate_status",
    
    # ------------------------------------------------------------------------
    # CALCULATED FIELDS AND METADATA
    # ------------------------------------------------------------------------
    "DIFDATA" = "date_difference",
    "NUDIASOBCO" = "days_death_to_conclusion",
    
    # ------------------------------------------------------------------------
    # COMMON DEMOGRAPHICS (All Systems) - Existing
    # ------------------------------------------------------------------------
    "CS_SEXO" = "sex",
    "CS_RACA" = "race",
    "ESTCIVMAE" = "mother_marital_status",
    "DT_NASC" = "birth_date",
    "NASC" = "birth_date",
    "DT_NOTIFIC" = "notification_date",
    "DT_SIN_PRI" = "first_symptom_date",
    "DT_INTER" = "admission_date",
    "DT_SAIDA" = "discharge_date",
    "NU_IDADE_N" = "age",
    "IDADEPAI" = "father_age",
    "ID_MN_RESI" = "residence_municipality",
    "MUNIC_RES" = "residence_municipality",
    "CODMUNNASC" = "birth_municipality_code",
    "CODUFMUN" = "municipality_code",
    "CS_ESCOL_N" = "education",
    "CODOCUPMAE" = "mother_occupation_code",
    "ID_OCUPA_N" = "occupation",
    # ------------------------------------------------------------------------
    # ADDITIONAL SIM FIELDS
    # ------------------------------------------------------------------------
    "ESTABDESCR" = "facility_description",
    "COMUNSVOIM" = "svo_iml_municipality",
    "DTATESTADO" = "certificate_date",
    "FONTEINV" = "investigation_source",
    "ESCFALAGR1" = "aggregated_deceased_education",
    "STDONOVA" = "new_death_certificate_status",
    "NUDIASOBIN" = "days_death_to_investigation",
    "NUDIASINF" = "days_information",
    "FONTESINF" = "information_sources",
    "CONTADOR" = "counter"
  )
  
  
  # ==========================================================================
  # CATEGORICAL VALUE TRANSLATIONS
  # Decode numeric codes to human-readable labels
  # ==========================================================================
  
  values <- list(
    
    # TIPOBITO - Death type
    "TIPOBITO" = c(
      "1" = "Fetal",
      "2" = "Non-fetal"
    ),
    
    # SEXO - Sex of deceased
    "SEXO" = c(
      "1" = "Male",
      "2" = "Female",
      "0" = "Ignored"
    ),
    
    # RACACOR - Race/color
    "RACACOR" = c(
      "1" = "White",
      "2" = "Black",
      "3" = "Yellow",
      "4" = "Brown",
      "5" = "Indigenous",
      "9" = "Ignored"
    ),
    
    # ESTCIV - Marital status
    "ESTCIV" = c(
      "1" = "Single",
      "2" = "Married",
      "3" = "Widowed",
      "4" = "Legally separated/divorced",
      "5" = "Stable union",
      "9" = "Ignored"
    ),
    
    # ESC - Education (old format)
    "ESC" = c(
      "1" = "None",
      "2" = "1 to 3 years",
      "3" = "4 to 7 years",
      "4" = "8 to 11 years",
      "5" = "12 years or more",
      "9" = "Ignored"
    ),
    
    # ESC2010 - Education 2010
    "ESC2010" = c(
      "0" = "No education",
      "1" = "Elementary I (1st to 4th grade)",
      "2" = "Elementary II (5th to 8th grade)",
      "3" = "High school (formerly 2nd grade)",
      "4" = "Incomplete higher education",
      "5" = "Complete higher education",
      "9" = "Ignored"
    ),
    
    # ESCFALAGRI - Aggregated deceased education
    "ESCFALAGRI" = c(
      "00" = "No education",
      "01" = "Incomplete Elementary I",
      "02" = "Complete Elementary I",
      "03" = "Incomplete Elementary II",
      "04" = "Complete Elementary II",
      "05" = "Incomplete High School",
      "06" = "Complete High School",
      "07" = "Incomplete Higher Education",
      "08" = "Complete Higher Education",
      "09" = "Ignored",
      "10" = "Incomplete or unspecified Elementary I",
      "11" = "Incomplete or unspecified Elementary II",
      "12" = "Incomplete or unspecified High School"
    ),
    
    # LOCOCOR - Death occurrence location
    "LOCOCOR" = c(
      "1" = "Hospital",
      "2" = "Other health facility",
      "3" = "Home",
      "4" = "Public place",
      "5" = "Other",
      "6" = "Indigenous village",
      "9" = "Ignored"
    ),
    
    # ESCMAE - Mother's education (old format)
    "ESCMAE" = c(
      "1" = "None",
      "2" = "1 to 3 years",
      "3" = "4 to 7 years",
      "4" = "8 to 11 years",
      "5" = "12 years or more",
      "9" = "Ignored"
    ),
    
    # ESCMAE2010 - Mother's education 2010
    "ESCMAE2010" = c(
      "0" = "No education",
      "1" = "Elementary I (1st to 4th grade)",
      "2" = "Elementary II (5th to 8th grade)",
      "3" = "High school (formerly 2nd grade)",
      "4" = "Incomplete higher education",
      "5" = "Complete higher education",
      "9" = "Ignored"
    ),
    
    # ESCMAEAGR1 - Aggregated mother education
    "ESCMAEAGR1" = c(
      "00" = "No education",
      "01" = "Incomplete Elementary I",
      "02" = "Complete Elementary I",
      "03" = "Incomplete Elementary II",
      "04" = "Complete Elementary II",
      "05" = "Incomplete High School",
      "06" = "Complete High School",
      "07" = "Incomplete Higher Education",
      "08" = "Complete Higher Education",
      "09" = "Ignored",
      "10" = "Incomplete or unspecified Elementary I",
      "11" = "Incomplete or unspecified Elementary II",
      "12" = "Incomplete or unspecified High School"
    ),
    
    # GESTACAO - Gestational age (old format)
    "GESTACAO" = c(
      "1" = "Less than 22 weeks",
      "2" = "22 to 27 weeks",
      "3" = "28 to 31 weeks",
      "4" = "32 to 36 weeks",
      "5" = "37 to 41 weeks",
      "6" = "42 weeks or more"
    ),
    
    # SEMAGESTAC - Gestational weeks
    # Numerical values from 01 to 52, 99 = ignored
    
    # GRAVIDEZ - Pregnancy type
    "GRAVIDEZ" = c(
      "1" = "Single",
      "2" = "Twin",
      "3" = "Triple or more",
      "9" = "Ignored"
    ),
    
    # PARTO - Delivery type
    "PARTO" = c(
      "1" = "Vaginal",
      "2" = "Cesarean",
      "9" = "Ignored"
    ),
    
    # OBITOPARTO - Death timing in relation to delivery
    "OBITOPARTO" = c(
      "1" = "Before",
      "2" = "During",
      "3" = "After",
      "9" = "Ignored"
    ),
    
    # MORTEPARTO - Death timing in relation to partum
    "MORTEPARTO" = c(
      "1" = "Before",
      "2" = "During",
      "3" = "After",
      "9" = "Ignored"
    ),
    
    # TPMORTEOCO - When death occurred
    "TPMORTEOCO" = c(
      "1" = "During pregnancy",
      "2" = "During delivery",
      "3" = "During abortion",
      "4" = "Up to 42 days after delivery",
      "5" = "43 days to 1 year after pregnancy",
      "8" = "Did not occur in these periods",
      "9" = "Ignored"
    ),
    
    # TPOBITOCOR - Corrected death occurrence timing
    "TPOBITOCOR" = c(
      "1" = "During pregnancy",
      "2" = "During abortion",
      "3" = "After abortion",
      "4" = "During delivery or up to 1 hour after",
      "5" = "During puerperium up to 42 days after delivery",
      "6" = "Between 43 days and up to 1 year after delivery",
      "7" = "Investigation did not identify death timing",
      "8" = "More than one year after delivery",
      "9" = "Death did not occur in previous circumstances"
    ),
    
    # CIRCOBITO - Violent death type
    "CIRCOBITO" = c(
      "1" = "Accident",
      "2" = "Suicide",
      "3" = "Homicide",
      "4" = "Other",
      "9" = "Ignored"
    ),
    
    # ACIDTRAB - Work accident
    "ACIDTRAB" = c(
      "1" = "Yes",
      "2" = "No",
      "9" = "Ignored"
    ),
    
    # FONTE - Information source
    "FONTE" = c(
      "1" = "Police report",
      "2" = "Hospital",
      "3" = "Family",
      "4" = "Other",
      "9" = "Ignored"
    ),
    
    # FONTENV - Investigation source
    "FONTENV" = c(
      "1" = "Maternal and/or Infant Death Committee",
      "2" = "Home visit / Family interview",
      "3" = "Health facility / Medical record",
      "4" = "Related to other databases",
      "5" = "SVO",
      "6" = "IML",
      "7" = "Other source",
      "8" = "Multiple sources",
      "9" = "Ignored"
    ),
    
    # ASSISTMED - Received medical assistance
    "ASSISTMED" = c(
      "1" = "Yes",
      "2" = "No",
      "9" = "Ignored"
    ),
    
    # NECROPSIA - Autopsy performed
    "NECROPSIA" = c(
      "1" = "Yes",
      "2" = "No",
      "9" = "Ignored"
    ),
    
    # EXAME - Examination performed
    "EXAME" = c(
      "1" = "Yes",
      "2" = "No",
      "9" = "Ignored"
    ),
    
    # CIRURGIA - Surgery performed
    "CIRURGIA" = c(
      "1" = "Yes",
      "2" = "No",
      "9" = "Ignored"
    ),
    
    # OBITOGRAV - Death during pregnancy
    "OBITOGRAV" = c(
      "1" = "Yes",
      "2" = "No",
      "9" = "Ignored"
    ),
    
    # OBITOPUERP - Death during puerperium
    "OBITOPUERP" = c(
      "1" = "Yes, up to 42 days after delivery",
      "2" = "Yes, 43 days to 1 year",
      "3" = "No",
      "9" = "Ignored"
    ),
    
    # ORIGEM - Record origin
    "ORIGEM" = c(
      "1" = "Oracle",
      "2" = "State database via FTP",
      "3" = "SEADE database",
      "9" = "Ignored"
    ),
    
    # STCODIFICA - Coding status
    "STCODIFICA" = c(
      "S" = "Coder",
      "N" = "Non-coder"
    ),
    
    # CODIFICADO - Form coded
    "CODIFICADO" = c(
      "S" = "Coded",
      "N" = "Not coded"
    ),
    
    # STDOEPIDEM - Epidemiological death certificate status
    "STDOEPIDEM" = c(
      "1" = "Yes",
      "0" = "No"
    ),
    
    # STIONOVA - New death certificate status
    "STIONOVA" = c(
      "1" = "Yes",
      "0" = "No"
    ),
    
    # TPRESGINFO - Information retrieval type
    "TPRESGINFO" = c(
      "01" = "Did not add or correct information",
      "02" = "Yes, allowed retrieval of new information",
      "03" = "Yes, allowed correction of originally informed causes"
    ),
    
    # TPNIVELINV - Investigator level type
    "TPNIVELINV" = c(
      "E" = "State",
      "R" = "Regional",
      "M" = "Municipal"
    ),
    
    # ALTCAUSA - Cause modified after investigation
    "ALTCAUSA" = c(
      "1" = "Yes",
      "2" = "No"
    ),
    
    # TPPOS - Death investigated
    "TPPOS" = c(
      "1" = "Yes",
      "2" = "No"
    ),
    
    # IDADE - Age unit (first digit of age code)
    "UNIDADE_IDADE" = c(
      "0" = "Ignored",
      "1" = "Minute",
      "2" = "Hour",
      "3" = "Day",
      "4" = "Month",
      "5" = "Year"
    ),
    
    # Existing value translations from original function
    "LOCNASC" = c(
      "1" = "Hospital",
      "2" = "Other health facility",
      "3" = "Home",
      "4" = "Other"
    ),
    
    "ESTCIVMAE" = c(
      "1" = "Single",
      "2" = "Married",
      "3" = "Widowed",
      "4" = "Legally separated",
      "5" = "Common-law marriage"
    ),
    
    "CONSULTAS" = c(
      "1" = "None",
      "2" = "1 to 3",
      "3" = "4 to 6",
      "4" = "7 or more"
    ),
    
    "RACA_COR" = c(
      "01" = "White",
      "02" = "Black",
      "03" = "Yellow",
      "04" = "Brown",
      "05" = "Indigenous"
    ),
    
    "MORTE" = c(
      "0" = "No",
      "1" = "Yes"
    ),
    
    "CS_SEXO" = c(
      "M" = "Male",
      "F" = "Female",
      "I" = "Ignored"
    ),
    
    "CS_RACA" = c(
      "1" = "White",
      "2" = "Black",
      "3" = "Yellow",
      "4" = "Brown",
      "5" = "Indigenous",
      "9" = "Ignored"
    ),
    
    "CS_GESTANT" = c(
      "1" = "1st trimester",
      "2" = "2nd trimester",
      "3" = "3rd trimester",
      "4" = "Gestational age ignored",
      "5" = "Not applicable",
      "6" = "Not pregnant",
      "9" = "Ignored"
    ),
    
    "EVOLUCAO" = c(
      "1" = "Cure",
      "2" = "Death by disease",
      "3" = "Death by other causes",
      "9" = "Ignored"
    ),
    # ------------------------------------------------------------------------
    # ADDITIONAL CATEGORICAL VALUES
    # ------------------------------------------------------------------------
    
    # FONTEINV - Investigation source
    "FONTEINV" = c(
      "1" = "Maternal and/or Infant Death Committee",
      "2" = "Home visit / Family interview",
      "3" = "Health facility / Medical record",
      "4" = "Related to other databases",
      "5" = "SVO",
      "6" = "IML",
      "7" = "Other source",
      "8" = "Multiple sources",
      "9" = "Ignored"
    ),
    
    # STDONOVA - New death certificate status
    "STDONOVA" = c(
      "1" = "Yes",
      "0" = "No"
    ),
    
    # FONTESINF - Information sources
    "FONTESINF" = c(
      "S" = "Yes",
      "X" = "No"
    ),
    
    # ESCFALAGR1 - Aggregated deceased education
    "ESCFALAGR1" = c(
      "00" = "No education",
      "01" = "Incomplete Elementary I",
      "02" = "Complete Elementary I",
      "03" = "Incomplete Elementary II",
      "04" = "Complete Elementary II",
      "05" = "Incomplete High School",
      "06" = "Complete High School",
      "07" = "Incomplete Higher Education",
      "08" = "Complete Higher Education",
      "09" = "Ignored",
      "10" = "Incomplete or unspecified Elementary I",
      "11" = "Incomplete or unspecified Elementary II",
      "12" = "Incomplete or unspecified High School"
    )
  )
  
  return(list(columns = columns, values = values))
}

#' Get Translation Dictionary for PT : SIM
#'
#' Internal function that returns PT translations organized by system and category.
#' This structure facilitates team collaboration and translation maintenance.
#'
#' @return List with columns and values translations
#' @keywords internal
#' @noRd

get_translation_dict_pt <- function() {
  
  # ==========================================================================
  # COLUMN NAME TRANSLATIONS
  # Organizado por categorias para fácil manutenção e colaboração da equipe
  # ==========================================================================
  
  columns <- c(
    
    # ------------------------------------------------------------------------
    # IDENTIFICACAO BASICA E TIPO DE OBITO
    # ------------------------------------------------------------------------
    "TIPOBITO" = "tipo_obito",
    "DTOBITO" = "data_obito",
    "HORAOBITO" = "hora_obito",
    "NUMERODO" = "numero_declaracao_obito",
    "CAUSABAS" = "causa_basica",
    "CAUSABAS_O" = "causa_basica_original",
    "CB_PRE" = "causa_basica_pre_selecao",
    "CAUSAMAT" = "causa_materna",
    
    # ------------------------------------------------------------------------
    # DADOS PESSOAIS DO FALECIDO
    # ------------------------------------------------------------------------
    "NATURAL" = "municipio_nascimento",
    "CODMUNNATU" = "codigo_municipio_nascimento",
    "DTNASC" = "data_nascimento",
    "IDADE" = "codigo_idade",
    "SEXO" = "sexo",
    "RACACOR" = "raca",
    "ESTCIV" = "estado_civil",
    
    # ------------------------------------------------------------------------
    # ESCOLARIDADE E OCUPACAO DO FALECIDO
    # ------------------------------------------------------------------------
    "ESC" = "escolaridade",
    "ESC2010" = "escolaridade_2010",
    "SERIESCFAL" = "serie_escolar_falecido",
    "ESCFALAGRI" = "escolaridade_falecido_agregada",
    "OCUP" = "ocupacao",
    
    # ------------------------------------------------------------------------
    # LOCAL DE RESIDENCIA E OCORRENCIA
    # ------------------------------------------------------------------------
    "CODMUNRES" = "codigo_municipio_residencia",
    "LOCOCOR" = "local_ocorrencia_obito",
    "CODMUNOCOR" = "codigo_municipio_ocorrencia",
    
    # ------------------------------------------------------------------------
    # DADOS DA MAE (PARA OBITO FETAL OU MENOR DE 1 ANO)
    # ------------------------------------------------------------------------
    "IDADEMAE" = "idade_mae",
    "ESCMAE" = "escolaridade_mae",
    "ESCMAE2010" = "escolaridade_mae_2010",
    "SERIESCMAE" = "serie_escolar_mae",
    "ESCMAEAGR1" = "escolaridade_mae_agregada",
    "OCUPMAE" = "ocupacao_mae",
    "QTDFILVIVO" = "quantidade_filhos_vivos",
    "QTDFILMORT" = "quantidade_filhos_mortos",
    
    # ------------------------------------------------------------------------
    # DADOS DA GRAVIDEZ E NASCIMENTO
    # ------------------------------------------------------------------------
    "SEMAGESTAC" = "semanas_gestacao",
    "GESTACAO" = "idade_gestacional",
    "GRAVIDEZ" = "tipo_gravidez",
    "PARTO" = "tipo_parto",
    "OBITOPARTO" = "momento_obito_parto",
    "MORTEPARTO" = "momento_morte_parto",
    "PESO" = "peso_nascimento",
    "NUMERODN" = "numero_declaracao_nascimento",
    
    # ------------------------------------------------------------------------
    # CAUSAS DA MORTE (LINHAS DA DO)
    # ------------------------------------------------------------------------
    "LINHAA" = "linha_causa_a",
    "LINHAB" = "linha_causa_b",
    "LINHAC" = "linha_causa_c",
    "LINHAD" = "linha_causa_d",
    "LINHAII" = "linha_causa_ii",
    "ATESTADO" = "atestado_obito",
    
    # ------------------------------------------------------------------------
    # MORTE MATERNA E RELACIONADA A GRAVIDEZ
    # ------------------------------------------------------------------------
    "TPMORTEOCO" = "momento_morte_gestacao",
    "TPOBITOCOR" = "tipo_obito_corrigido",
    "OBITOGRAV" = "obito_gravidez",
    "OBITOPUERP" = "obito_puerperio",
    
    # ------------------------------------------------------------------------
    # CIRCUNSTANCIAS DA MORTE
    # ------------------------------------------------------------------------
    "CIRCOBITO" = "circunstancia_obito",
    "ACIDTRAB" = "acidente_trabalho",
    "FONTE" = "fonte_informacao",
    
    # ------------------------------------------------------------------------
    # ASSISTENCIA MEDICA E PROCEDIMENTOS
    # ------------------------------------------------------------------------
    "ASSISTMED" = "assistencia_medica",
    "NECROPSIA" = "necropsia",
    "EXAME" = "exame_realizado",
    "CIRURGIA" = "cirurgia_realizada",
    
    # ------------------------------------------------------------------------
    # INFORMACOES SOBRE INVESTIGACAO
    # ------------------------------------------------------------------------
    "DTINVESTIG" = "data_investigacao",
    "DTCONINV" = "data_conclusao_investigacao",
    "DTCADINV" = "data_cadastro_investigacao",
    "DTCONCASO" = "data_conclusao_caso",
    "DTCADINF" = "data_cadastro_informacao",
    "FONTENV" = "fonte_investigacao",
    "FONTES" = "fontes_combinadas",
    "TPRESGINFO" = "tipo_resgate_informacao",
    "TPNIVELINV" = "tipo_nivel_investigador",
    "ALTCAUSA" = "alteracao_causa",
    "TPPOS" = "obito_investigado",
    
    # ------------------------------------------------------------------------
    # DADOS ADMINISTRATIVOS E DE SISTEMA
    # ------------------------------------------------------------------------
    "ORIGEM" = "origem_registro",
    "CODESTAB" = "codigo_estabelecimento",
    "ATESTANTE" = "atestante",
    "DTCADASTRO" = "data_cadastro",
    "DTRECEBIM" = "data_recebimento",
    "DTRECORIGA" = "data_recebimento_original",
    "DTDIGITA" = "data_digitacao",
    "NUMEROLOTE" = "numero_lote",
    "STCODIFICA" = "status_codificacao",
    "CODIFICADO" = "codificado",
    "VERSAOSIST" = "versao_sistema",
    "VERSAOSCB" = "versao_seletor_causa_basica",
    "STDOEPIDEM" = "status_do_epidemiologica",
    "STIONOVA" = "status_do_nova",
    
    # ------------------------------------------------------------------------
    # CAMPOS CALCULADOS E METADADOS
    # ------------------------------------------------------------------------
    "DIFDATA" = "diferenca_data",
    "NUDIASOBCO" = "numero_dias_obito_conclusao",

    # ------------------------------------------------------------------------
    # CAMPOS ADICIONAIS DO SIM
    # ------------------------------------------------------------------------
    "ESTABDESCR" = "descricao_estabelecimento",
    "COMUNSVOIM" = "municipio_svo_iml",
    "DTATESTADO" = "data_atestado",
    "FONTEINV" = "fonte_investigacao",
    "ESCFALAGR1" = "escolaridade_falecido_agregada",
    "STDONOVA" = "status_declaracao_nova",
    "NUDIASOBIN" = "dias_obito_investigacao",
    "NUDIASINF" = "dias_informacao",
    "FONTESINF" = "fontes_informacao",
    "CONTADOR" = "contador"
  )
  
  
  # ==========================================================================
  # TRADUCOES DE VALORES CATEGORICOS
  # Decodifica codigos numericos para rotulos legiveis
  # ==========================================================================
  
  values <- list(
    
    # TIPOBITO - Tipo de obito
    "TIPOBITO" = c(
      "1" = "Fetal",
      "2" = "Nao fetal"
    ),
    
    # SEXO - Sexo do falecido
    "SEXO" = c(
      "1" = "Masculino",
      "2" = "Feminino",
      "0" = "Ignorado"
    ),
    
    # RACACOR - Raca/cor
    "RACACOR" = c(
      "1" = "Branca",
      "2" = "Preta",
      "3" = "Amarela",
      "4" = "Parda",
      "5" = "Indigena",
      "9" = "Ignorado"
    ),
    
    # ESTCIV - Estado civil
    "ESTCIV" = c(
      "1" = "Solteiro",
      "2" = "Casado",
      "3" = "Viuvo",
      "4" = "Separado judicialmente/divorciado",
      "5" = "Uniao estavel",
      "9" = "Ignorado"
    ),
    
    # ESC - Escolaridade (formato antigo)
    "ESC" = c(
      "1" = "Nenhuma",
      "2" = "1 a 3 anos",
      "3" = "4 a 7 anos",
      "4" = "8 a 11 anos",
      "5" = "12 anos e mais",
      "9" = "Ignorado"
    ),
    
    # ESC2010 - Escolaridade 2010
    "ESC2010" = c(
      "0" = "Sem escolaridade",
      "1" = "Fundamental I (1 a 4 serie)",
      "2" = "Fundamental II (5 a 8 serie)",
      "3" = "Medio (antigo 2 Grau)",
      "4" = "Superior incompleto",
      "5" = "Superior completo",
      "9" = "Ignorado"
    ),
    
    # ESCFALAGRI - Escolaridade do falecido agregada
    "ESCFALAGRI" = c(
      "00" = "Sem escolaridade",
      "01" = "Fundamental I incompleto",
      "02" = "Fundamental I completo",
      "03" = "Fundamental II incompleto",
      "04" = "Fundamental II completo",
      "05" = "Ensino Medio incompleto",
      "06" = "Ensino Medio completo",
      "07" = "Superior incompleto",
      "08" = "Superior completo",
      "09" = "Ignorado",
      "10" = "Fundamental I incompleto ou inespecifico",
      "11" = "Fundamental II incompleto ou inespecifico",
      "12" = "Ensino Medio incompleto ou inespecifico"
    ),
    
    # LOCOCOR - Local de ocorrencia do obito
    "LOCOCOR" = c(
      "1" = "Hospital",
      "2" = "Outro estabelecimento de saude",
      "3" = "Domicilio",
      "4" = "Via publica",
      "5" = "Outros",
      "6" = "Aldeia indigena",
      "9" = "Ignorado"
    ),
    
    # ESCMAE - Escolaridade da mae (formato antigo)
    "ESCMAE" = c(
      "1" = "Nenhuma",
      "2" = "1 a 3 anos",
      "3" = "4 a 7 anos",
      "4" = "8 a 11 anos",
      "5" = "12 anos e mais",
      "9" = "Ignorado"
    ),
    
    # ESCMAE2010 - Escolaridade da mae 2010
    "ESCMAE2010" = c(
      "0" = "Sem escolaridade",
      "1" = "Fundamental I (1 a 4 serie)",
      "2" = "Fundamental II (5 a 8 serie)",
      "3" = "Medio (antigo 2 Grau)",
      "4" = "Superior incompleto",
      "5" = "Superior completo",
      "9" = "Ignorado"
    ),
    
    # ESCMAEAGR1 - Escolaridade da mae agregada
    "ESCMAEAGR1" = c(
      "00" = "Sem escolaridade",
      "01" = "Fundamental I incompleto",
      "02" = "Fundamental I completo",
      "03" = "Fundamental II incompleto",
      "04" = "Fundamental II completo",
      "05" = "Ensino Medio incompleto",
      "06" = "Ensino Medio completo",
      "07" = "Superior incompleto",
      "08" = "Superior completo",
      "09" = "Ignorado",
      "10" = "Fundamental I incompleto ou inespecifico",
      "11" = "Fundamental II incompleto ou inespecifico",
      "12" = "Ensino Medio incompleto ou inespecifico"
    ),
    
    # GESTACAO - Idade gestacional (formato antigo)
    "GESTACAO" = c(
      "1" = "Menos de 22 semanas",
      "2" = "22 a 27 semanas",
      "3" = "28 a 31 semanas",
      "4" = "32 a 36 semanas",
      "5" = "37 a 41 semanas",
      "6" = "42 e + semanas"
    ),
    
    # SEMAGESTAC - Semanas de gestacao
    # Valores numericos de 01 a 52, 99 = ignorado
    
    # GRAVIDEZ - Tipo de gravidez
    "GRAVIDEZ" = c(
      "1" = "Unica",
      "2" = "Dupla",
      "3" = "Tripla e mais",
      "9" = "Ignorada"
    ),
    
    # PARTO - Tipo de parto
    "PARTO" = c(
      "1" = "Vaginal",
      "2" = "Cesareo",
      "9" = "Ignorado"
    ),
    
    # OBITOPARTO - Morte em relacao ao parto
    "OBITOPARTO" = c(
      "1" = "Antes",
      "2" = "Durante",
      "3" = "Depois",
      "9" = "Ignorado"
    ),
    
    # MORTEPARTO - Momento do obito em relacao ao parto
    "MORTEPARTO" = c(
      "1" = "Antes",
      "2" = "Durante",
      "3" = "Apos",
      "9" = "Ignorado"
    ),
    
    # TPMORTEOCO - A morte ocorreu
    "TPMORTEOCO" = c(
      "1" = "Na gravidez",
      "2" = "No parto",
      "3" = "No abortamento",
      "4" = "Ate 42 dias apos o termino do parto",
      "5" = "De 43 dias a 1 ano apos o termino da gestacao",
      "8" = "Nao ocorreu nestes periodos",
      "9" = "Ignorado"
    ),
    
    # TPOBITOCOR - Momento da ocorrencia do obito corrigido
    "TPOBITOCOR" = c(
      "1" = "Durante a gestacao",
      "2" = "Durante o abortamento",
      "3" = "Apos o abortamento",
      "4" = "No parto ou ate 1 hora apos o parto",
      "5" = "No puerperio ate 42 dias apos o parto",
      "6" = "Entre 43 dias e ate 1 ano apos o parto",
      "7" = "A investigacao nao identificou o momento do obito",
      "8" = "Mais de um ano apos o parto",
      "9" = "O obito nao ocorreu nas circunstancias anteriores"
    ),
    
    # CIRCOBITO - Tipo de morte violenta
    "CIRCOBITO" = c(
      "1" = "Acidente",
      "2" = "Suicidio",
      "3" = "Homicidio",
      "4" = "Outros",
      "9" = "Ignorado"
    ),
    
    # ACIDTRAB - Acidente do trabalho
    "ACIDTRAB" = c(
      "1" = "Sim",
      "2" = "Nao",
      "9" = "Ignorado"
    ),
    
    # FONTE - Fonte da informacao
    "FONTE" = c(
      "1" = "Ocorrencia policial",
      "2" = "Hospital",
      "3" = "Familia",
      "4" = "Outra",
      "9" = "Ignorado"
    ),
    
    # FONTENV - Fonte de investigacao
    "FONTENV" = c(
      "1" = "Comite de Morte Materna e/ou Infantil",
      "2" = "Visita domiciliar / Entrevista familia",
      "3" = "Estabelecimento de Saude / Prontuario",
      "4" = "Relacionado com outros bancos de dados",
      "5" = "SVO",
      "6" = "IML",
      "7" = "Outra fonte",
      "8" = "Multiplas fontes",
      "9" = "Ignorado"
    ),
    
    # ASSISTMED - Recebeu assistencia medica
    "ASSISTMED" = c(
      "1" = "Sim",
      "2" = "Nao",
      "9" = "Ignorado"
    ),
    
    # NECROPSIA - Necropsia realizada
    "NECROPSIA" = c(
      "1" = "Sim",
      "2" = "Nao",
      "9" = "Ignorado"
    ),
    
    # EXAME - Realizacao de exame
    "EXAME" = c(
      "1" = "Sim",
      "2" = "Nao",
      "9" = "Ignorado"
    ),
    
    # CIRURGIA - Realizacao de cirurgia
    "CIRURGIA" = c(
      "1" = "Sim",
      "2" = "Nao",
      "9" = "Ignorado"
    ),
    
    # OBITOGRAV - Obito na gravidez
    "OBITOGRAV" = c(
      "1" = "Sim",
      "2" = "Nao",
      "9" = "Ignorado"
    ),
    
    # OBITOPUERP - Obito no puerperio
    "OBITOPUERP" = c(
      "1" = "Sim, ate 42 dias apos o parto",
      "2" = "Sim, de 43 dias a 1 ano",
      "3" = "Nao",
      "9" = "Ignorado"
    ),
    
    # ORIGEM - Origem do registro
    "ORIGEM" = c(
      "1" = "Oracle",
      "2" = "Banco estadual disponibilizado via FTP",
      "3" = "Banco SEADE",
      "9" = "Ignorado"
    ),
    
    # STCODIFICA - Status de codificacao
    "STCODIFICA" = c(
      "S" = "Codificadora",
      "N" = "Nao codificadora"
    ),
    
    # CODIFICADO - Formulario codificado
    "CODIFICADO" = c(
      "S" = "Codificado",
      "N" = "Nao codificado"
    ),
    
    # STDOEPIDEM - Status de DO Epidemiologica
    "STDOEPIDEM" = c(
      "1" = "Sim",
      "0" = "Nao"
    ),
    
    # STIONOVA - Status de DO Nova
    "STIONOVA" = c(
      "1" = "Sim",
      "0" = "Nao"
    ),
    
    # TPRESGINFO - Tipo de resgate de informacao
    "TPRESGINFO" = c(
      "01" = "Nao acrescentou nem corrigiu informacao",
      "02" = "Sim, permitiu o resgate de novas informacoes",
      "03" = "Sim, permitiu a correcao de alguma das causas informadas originalmente"
    ),
    
    # TPNIVELINV - Tipo de nivel investigador
    "TPNIVELINV" = c(
      "E" = "Estadual",
      "R" = "Regional",
      "M" = "Municipal"
    ),
    
    # ALTCAUSA - Alteracao da causa apos investigacao
    "ALTCAUSA" = c(
      "1" = "Sim",
      "2" = "Nao"
    ),
    
    # TPPOS - Obito investigado
    "TPPOS" = c(
      "1" = "Sim",
      "2" = "Nao"
    ),
    
    # IDADE - Unidade de idade (primeiro digito do codigo)
    "UNIDADE_IDADE" = c(
      "0" = "Ignorado",
      "1" = "Minuto",
      "2" = "Hora",
      "3" = "Dia",
      "4" = "Mes",
      "5" = "Ano"
    ),
    # FONTEINV - Fonte da investigacao
    "FONTEINV" = c(
      "1" = "Comite de Morte Materna e/ou Infantil",
      "2" = "Visita domiciliar / Entrevista familia",
      "3" = "Estabelecimento de Saude / Prontuario",
      "4" = "Relacionado com outros bancos de dados",
      "5" = "SVO",
      "6" = "IML",
      "7" = "Outra fonte",
      "8" = "Multiplas fontes",
      "9" = "Ignorado"
    ),
    
    # STDONOVA - Status de DO Nova
    "STDONOVA" = c(
      "1" = "Sim",
      "0" = "Nao"
    ),
    
    # FONTESINF - Fontes de informacao
    "FONTESINF" = c(
      "S" = "Sim",
      "X" = "Nao"
    ),
    
    "ESCFALAGR1" = c(
      "00" = "Sem escolaridade",
      "01" = "Fundamental I incompleto",
      "02" = "Fundamental I completo",
      "03" = "Fundamental II incompleto",
      "04" = "Fundamental II completo",
      "05" = "Ensino Medio incompleto",
      "06" = "Ensino Medio completo",
      "07" = "Superior incompleto",
      "08" = "Superior completo",
      "09" = "Ignorado",
      "10" = "Fundamental I incompleto ou inespecifico",
      "11" = "Fundamental II incompleto ou inespecifico",
      "12" = "Ensino Medio incompleto ou inespecifico"
    )
  )
  
  return(list(columns = columns, values = values))
}


#' Get Translation Dictionary for ES: SIM
#'
#' Internal function that returns ES translations organized by system and category.
#' This structure facilitates team collaboration and translation maintenance.
#'
#' @return List with columns and values translations
#' @keywords internal
#' @noRd

get_translation_dict_es <- function() {
  
  # ==========================================================================
  # COLUMN NAME TRANSLATIONS
  # Organized by category for easy maintenance and team collaboration
  # ==========================================================================
  
  columns <- c(
    
    # ------------------------------------------------------------------------
    # IDENTIFICACION BASICA Y TIPO DE MUERTE
    # ------------------------------------------------------------------------
    "TIPOBITO" = "tipo_muerte",
    "DTOBITO" = "fecha_muerte",
    "HORAOBITO" = "hora_muerte",
    "NUMERODO" = "numero_certificado_muerte",
    "CAUSABAS" = "causa_basica",
    "CAUSABAS_O" = "causa_basica_original",
    "CB_PRE" = "causa_basica_pre_seleccion",
    "CAUSAMAT" = "causa_materna",
    
    # ------------------------------------------------------------------------
    # DATOS PERSONALES DEL FALLECIDO
    # ------------------------------------------------------------------------
    "NATURAL" = "municipio_nacimiento",
    "CODMUNNATU" = "codigo_municipio_nacimiento",
    "DTNASC" = "fecha_nacimiento",
    "IDADE" = "codigo_edad",
    "SEXO" = "sexo",
    "RACACOR" = "raza",
    "ESTCIV" = "estado_civil",
    
    # ------------------------------------------------------------------------
    # EDUCACION Y OCUPACION DEL FALLECIDO
    # ------------------------------------------------------------------------
    "ESC" = "escolaridad",
    "ESC2010" = "escolaridad_2010",
    "SERIESCFAL" = "grado_escolar_fallecido",
    "ESCFALAGRI" = "escolaridad_fallecido_agregada",
    "OCUP" = "ocupacion",
    
    # ------------------------------------------------------------------------
    # LUGAR DE RESIDENCIA Y OCURRENCIA
    # ------------------------------------------------------------------------
    "CODMUNRES" = "codigo_municipio_residencia",
    "LOCOCOR" = "lugar_ocurrencia_muerte",
    "CODMUNOCOR" = "codigo_municipio_ocurrencia",
    
    # ------------------------------------------------------------------------
    # DATOS DE LA MADRE (PARA MUERTE FETAL O MENOR DE 1 ANO)
    # ------------------------------------------------------------------------
    "IDADEMAE" = "edad_madre",
    "ESCMAE" = "escolaridad_madre",
    "ESCMAE2010" = "escolaridad_madre_2010",
    "SERIESCMAE" = "grado_escolar_madre",
    "ESCMAEAGR1" = "escolaridad_madre_agregada",
    "OCUPMAE" = "ocupacion_madre",
    "QTDFILVIVO" = "numero_hijos_vivos",
    "QTDFILMORT" = "numero_hijos_muertos",
    
    # ------------------------------------------------------------------------
    # DATOS DEL EMBARAZO Y NACIMIENTO
    # ------------------------------------------------------------------------
    "SEMAGESTAC" = "semanas_gestacion",
    "GESTACAO" = "edad_gestacional",
    "GRAVIDEZ" = "tipo_embarazo",
    "PARTO" = "tipo_parto",
    "OBITOPARTO" = "momento_muerte_parto",
    "MORTEPARTO" = "momento_muerte_partum",
    "PESO" = "peso_nacimiento",
    "NUMERODN" = "numero_certificado_nacimiento",
    
    # ------------------------------------------------------------------------
    # CAUSAS DE LA MUERTE (LINEAS DEL CERTIFICADO)
    # ------------------------------------------------------------------------
    "LINHAA" = "linea_causa_a",
    "LINHAB" = "linea_causa_b",
    "LINHAC" = "linea_causa_c",
    "LINHAD" = "linea_causa_d",
    "LINHAII" = "linea_causa_ii",
    "ATESTADO" = "certificado_muerte",
    
    # ------------------------------------------------------------------------
    # MUERTE MATERNA Y RELACIONADA CON EL EMBARAZO
    # ------------------------------------------------------------------------
    "TPMORTEOCO" = "momento_muerte_embarazo",
    "TPOBITOCOR" = "tipo_muerte_corregido",
    "OBITOGRAV" = "muerte_durante_embarazo",
    "OBITOPUERP" = "muerte_durante_puerperio",
    
    # ------------------------------------------------------------------------
    # CIRCUNSTANCIAS DE LA MUERTE
    # ------------------------------------------------------------------------
    "CIRCOBITO" = "circunstancia_muerte",
    "ACIDTRAB" = "accidente_trabajo",
    "FONTE" = "fuente_informacion",
    
    # ------------------------------------------------------------------------
    # ASISTENCIA MEDICA Y PROCEDIMIENTOS
    # ------------------------------------------------------------------------
    "ASSISTMED" = "asistencia_medica",
    "NECROPSIA" = "autopsia",
    "EXAME" = "examen_realizado",
    "CIRURGIA" = "cirugia_realizada",
    
    # ------------------------------------------------------------------------
    # INFORMACION SOBRE LA INVESTIGACION
    # ------------------------------------------------------------------------
    "DTINVESTIG" = "fecha_investigacion",
    "DTCONINV" = "fecha_conclusion_investigacion",
    "DTCADINV" = "fecha_registro_investigacion",
    "DTCONCASO" = "fecha_conclusion_caso",
    "DTCADINF" = "fecha_registro_informacion",
    "FONTENV" = "fuente_investigacion",
    "FONTES" = "fuentes_combinadas",
    "TPRESGINFO" = "tipo_recuperacion_informacion",
    "TPNIVELINV" = "tipo_nivel_investigador",
    "ALTCAUSA" = "causa_modificada",
    "TPPOS" = "muerte_investigada",
    
    # ------------------------------------------------------------------------
    # DATOS ADMINISTRATIVOS Y DEL SISTEMA
    # ------------------------------------------------------------------------
    "ORIGEM" = "origen_registro",
    "CODESTAB" = "codigo_establecimiento_salud",
    "ATESTANTE" = "certificador",
    "DTCADASTRO" = "fecha_registro",
    "DTRECEBIM" = "fecha_recepcion",
    "DTRECORIGA" = "fecha_recepcion_original",
    "DTDIGITA" = "fecha_digitacion",
    "NUMEROLOTE" = "numero_lote",
    "STCODIFICA" = "estado_codificacion",
    "CODIFICADO" = "codificado",
    "VERSAOSIST" = "version_sistema",
    "VERSAOSCB" = "version_selector_causa_basica",
    "STDOEPIDEM" = "estado_certificado_muerte_epidemiologica",
    "STIONOVA" = "estado_certificado_muerte_nuevo",
    
    # ------------------------------------------------------------------------
    # CAMPOS CALCULADOS Y METADATOS
    # ------------------------------------------------------------------------
    "DIFDATA" = "diferencia_fecha",
    "NUDIASOBCO" = "dias_muerte_conclusion",
    
    # ------------------------------------------------------------------------
    # DATOS DEMOGRAFICOS COMUNES (Todos los Sistemas) - Existente
    # ------------------------------------------------------------------------
    "CS_SEXO" = "sexo",
    "CS_RACA" = "raza",
    "ESTCIVMAE" = "estado_civil_madre",
    "DT_NASC" = "fecha_nacimiento",
    "NASC" = "fecha_nacimiento",
    "DT_NOTIFIC" = "fecha_notificacion",
    "DT_SIN_PRI" = "fecha_primer_sintoma",
    "DT_INTER" = "fecha_internacion",
    "DT_SAIDA" = "fecha_alta",
    "NU_IDADE_N" = "edad",
    "IDADEPAI" = "edad_padre",
    "ID_MN_RESI" = "municipio_residencia",
    "MUNIC_RES" = "municipio_residencia",
    "CODMUNNASC" = "codigo_municipio_nacimiento",
    "CODUFMUN" = "codigo_municipio",
    "CS_ESCOL_N" = "escolaridad",
    "CODOCUPMAE" = "codigo_ocupacion_madre",
    "ID_OCUPA_N" = "ocupacion",
    # ------------------------------------------------------------------------
    # CAMPOS ADICIONALES DEL SIM
    # ------------------------------------------------------------------------
    "ESTABDESCR" = "descripcion_establecimiento",
    "COMUNSVOIM" = "municipio_svo_iml",
    "DTATESTADO" = "fecha_certificado",
    "FONTEINV" = "fuente_investigacion",
    "ESCFALAGR1" = "escolaridad_fallecido_agregada",
    "STDONOVA" = "estado_certificado_muerte_nuevo",
    "NUDIASOBIN" = "dias_muerte_investigacion",
    "NUDIASINF" = "dias_informacion",
    "FONTESINF" = "fuentes_informacion",
    "CONTADOR" = "contador"
  )
  
  
  # ==========================================================================
  # TRADUCCIONES DE VALORES CATEGORICOS
  # Decodifica codigos numericos a etiquetas legibles
  # ==========================================================================
  
  values <- list(
    
    # TIPOBITO - Tipo de muerte
    "TIPOBITO" = c(
      "1" = "Fetal",
      "2" = "No fetal"
    ),
    
    # SEXO - Sexo del fallecido
    "SEXO" = c(
      "1" = "Masculino",
      "2" = "Femenino",
      "0" = "Ignorado"
    ),
    
    # RACACOR - Raza/color
    "RACACOR" = c(
      "1" = "Blanca",
      "2" = "Negra",
      "3" = "Amarilla",
      "4" = "Parda",
      "5" = "Indigena",
      "9" = "Ignorado"
    ),
    
    # ESTCIV - Estado civil
    "ESTCIV" = c(
      "1" = "Soltero",
      "2" = "Casado",
      "3" = "Viudo",
      "4" = "Separado judicialmente/divorciado",
      "5" = "Union estable",
      "9" = "Ignorado"
    ),
    
    # ESC - Escolaridad (formato antiguo)
    "ESC" = c(
      "1" = "Ninguna",
      "2" = "1 a 3 anos",
      "3" = "4 a 7 anos",
      "4" = "8 a 11 anos",
      "5" = "12 anos o mas",
      "9" = "Ignorado"
    ),
    
    # ESC2010 - Escolaridad 2010
    "ESC2010" = c(
      "0" = "Sin escolaridad",
      "1" = "Primaria I (1 a 4 serie)",
      "2" = "Primaria II (5 a 8 serie)",
      "3" = "Secundaria (antiguo 2 Grado)",
      "4" = "Superior incompleto",
      "5" = "Superior completo",
      "9" = "Ignorado"
    ),
    
    # ESCFALAGRI - Escolaridad del fallecido agregada
    "ESCFALAGRI" = c(
      "00" = "Sin escolaridad",
      "01" = "Primaria I incompleta",
      "02" = "Primaria I completa",
      "03" = "Primaria II incompleta",
      "04" = "Primaria II completa",
      "05" = "Secundaria incompleta",
      "06" = "Secundaria completa",
      "07" = "Superior incompleto",
      "08" = "Superior completo",
      "09" = "Ignorado",
      "10" = "Primaria I incompleta o inespecifica",
      "11" = "Primaria II incompleta o inespecifica",
      "12" = "Secundaria incompleta o inespecifica"
    ),
    
    # LOCOCOR - Lugar de ocurrencia de la muerte
    "LOCOCOR" = c(
      "1" = "Hospital",
      "2" = "Otro establecimiento de salud",
      "3" = "Domicilio",
      "4" = "Via publica",
      "5" = "Otros",
      "6" = "Aldea indigena",
      "9" = "Ignorado"
    ),
    
    # ESCMAE - Escolaridad de la madre (formato antiguo)
    "ESCMAE" = c(
      "1" = "Ninguna",
      "2" = "1 a 3 anos",
      "3" = "4 a 7 anos",
      "4" = "8 a 11 anos",
      "5" = "12 anos o mas",
      "9" = "Ignorado"
    ),
    
    # ESCMAE2010 - Escolaridad de la madre 2010
    "ESCMAE2010" = c(
      "0" = "Sin escolaridad",
      "1" = "Primaria I (1 a 4 serie)",
      "2" = "Primaria II (5 a 8 serie)",
      "3" = "Secundaria (antiguo 2 Grado)",
      "4" = "Superior incompleto",
      "5" = "Superior completo",
      "9" = "Ignorado"
    ),
    
    # ESCMAEAGR1 - Escolaridad de la madre agregada
    "ESCMAEAGR1" = c(
      "00" = "Sin escolaridad",
      "01" = "Primaria I incompleta",
      "02" = "Primaria I completa",
      "03" = "Primaria II incompleta",
      "04" = "Primaria II completa",
      "05" = "Secundaria incompleta",
      "06" = "Secundaria completa",
      "07" = "Superior incompleto",
      "08" = "Superior completo",
      "09" = "Ignorado",
      "10" = "Primaria I incompleta o inespecifica",
      "11" = "Primaria II incompleta o inespecifica",
      "12" = "Secundaria incompleta o inespecifica"
    ),
    
    # GESTACAO - Edad gestacional (formato antiguo)
    "GESTACAO" = c(
      "1" = "Menos de 22 semanas",
      "2" = "22 a 27 semanas",
      "3" = "28 a 31 semanas",
      "4" = "32 a 36 semanas",
      "5" = "37 a 41 semanas",
      "6" = "42 semanas o mas"
    ),
    
    # SEMAGESTAC - Semanas de gestacion
    # Valores numericos de 01 a 52, 99 = ignorado
    
    # GRAVIDEZ - Tipo de embarazo
    "GRAVIDEZ" = c(
      "1" = "Unica",
      "2" = "Doble",
      "3" = "Triple o mas",
      "9" = "Ignorada"
    ),
    
    # PARTO - Tipo de parto
    "PARTO" = c(
      "1" = "Vaginal",
      "2" = "Cesarea",
      "9" = "Ignorado"
    ),
    
    # OBITOPARTO - Momento de la muerte en relacion al parto
    "OBITOPARTO" = c(
      "1" = "Antes",
      "2" = "Durante",
      "3" = "Despues",
      "9" = "Ignorado"
    ),
    
    # MORTEPARTO - Momento de la muerte en relacion al partum
    "MORTEPARTO" = c(
      "1" = "Antes",
      "2" = "Durante",
      "3" = "Despues",
      "9" = "Ignorado"
    ),
    
    # TPMORTEOCO - Cuando ocurrio la muerte
    "TPMORTEOCO" = c(
      "1" = "Durante el embarazo",
      "2" = "Durante el parto",
      "3" = "Durante el aborto",
      "4" = "Hasta 42 dias despues del parto",
      "5" = "43 dias a 1 ano despues del embarazo",
      "8" = "No ocurrio en estos periodos",
      "9" = "Ignorado"
    ),
    
    # TPOBITOCOR - Momento de ocurrencia de la muerte corregido
    "TPOBITOCOR" = c(
      "1" = "Durante el embarazo",
      "2" = "Durante el aborto",
      "3" = "Despues del aborto",
      "4" = "Durante el parto o hasta 1 hora despues",
      "5" = "Durante el puerperio hasta 42 dias despues del parto",
      "6" = "Entre 43 dias y hasta 1 ano despues del parto",
      "7" = "La investigacion no identifico el momento de la muerte",
      "8" = "Mas de un ano despues del parto",
      "9" = "La muerte no ocurrio en las circunstancias anteriores"
    ),
    
    # CIRCOBITO - Tipo de muerte violenta
    "CIRCOBITO" = c(
      "1" = "Accidente",
      "2" = "Suicidio",
      "3" = "Homicidio",
      "4" = "Otros",
      "9" = "Ignorado"
    ),
    
    # ACIDTRAB - Accidente de trabajo
    "ACIDTRAB" = c(
      "1" = "Si",
      "2" = "No",
      "9" = "Ignorado"
    ),
    
    # FONTE - Fuente de informacion
    "FONTE" = c(
      "1" = "Ocurrencia policial",
      "2" = "Hospital",
      "3" = "Familia",
      "4" = "Otra",
      "9" = "Ignorado"
    ),
    
    # FONTENV - Fuente de investigacion
    "FONTENV" = c(
      "1" = "Comite de Muerte Materna y/o Infantil",
      "2" = "Visita domiciliaria / Entrevista familiar",
      "3" = "Establecimiento de Salud / Prontuario",
      "4" = "Relacionado con otras bases de datos",
      "5" = "SVO",
      "6" = "IML",
      "7" = "Otra fuente",
      "8" = "Multiples fuentes",
      "9" = "Ignorado"
    ),
    
    # ASSISTMED - Recibio asistencia medica
    "ASSISTMED" = c(
      "1" = "Si",
      "2" = "No",
      "9" = "Ignorado"
    ),
    
    # NECROPSIA - Autopsia realizada
    "NECROPSIA" = c(
      "1" = "Si",
      "2" = "No",
      "9" = "Ignorado"
    ),
    
    # EXAME - Examen realizado
    "EXAME" = c(
      "1" = "Si",
      "2" = "No",
      "9" = "Ignorado"
    ),
    
    # CIRURGIA - Cirugia realizada
    "CIRURGIA" = c(
      "1" = "Si",
      "2" = "No",
      "9" = "Ignorado"
    ),
    
    # OBITOGRAV - Muerte durante el embarazo
    "OBITOGRAV" = c(
      "1" = "Si",
      "2" = "No",
      "9" = "Ignorado"
    ),
    
    # OBITOPUERP - Muerte durante el puerperio
    "OBITOPUERP" = c(
      "1" = "Si, hasta 42 dias despues del parto",
      "2" = "Si, 43 dias a 1 ano",
      "3" = "No",
      "9" = "Ignorado"
    ),
    
    # ORIGEM - Origen del registro
    "ORIGEM" = c(
      "1" = "Oracle",
      "2" = "Base de datos estatal via FTP",
      "3" = "Base de datos SEADE",
      "9" = "Ignorado"
    ),
    
    # STCODIFICA - Estado de codificacion
    "STCODIFICA" = c(
      "S" = "Codificador",
      "N" = "No codificador"
    ),
    
    # CODIFICADO - Formulario codificado
    "CODIFICADO" = c(
      "S" = "Codificado",
      "N" = "No codificado"
    ),
    
    # STDOEPIDEM - Estado de certificado de muerte epidemiologica
    "STDOEPIDEM" = c(
      "1" = "Si",
      "0" = "No"
    ),
    
    # STIONOVA - Estado de certificado de muerte nuevo
    "STIONOVA" = c(
      "1" = "Si",
      "0" = "No"
    ),
    
    # TPRESGINFO - Tipo de recuperacion de informacion
    "TPRESGINFO" = c(
      "01" = "No anadio ni corrigio informacion",
      "02" = "Si, permitio la recuperacion de nueva informacion",
      "03" = "Si, permitio la correccion de alguna de las causas informadas originalmente"
    ),
    
    # TPNIVELINV - Tipo de nivel investigador
    "TPNIVELINV" = c(
      "E" = "Estatal",
      "R" = "Regional",
      "M" = "Municipal"
    ),
    
    # ALTCAUSA - Causa modificada despues de investigacion
    "ALTCAUSA" = c(
      "1" = "Si",
      "2" = "No"
    ),
    
    # TPPOS - Muerte investigada
    "TPPOS" = c(
      "1" = "Si",
      "2" = "No"
    ),
    
    # IDADE - Unidad de edad (primer digito del codigo)
    "UNIDAD_EDAD" = c(
      "0" = "Ignorado",
      "1" = "Minuto",
      "2" = "Hora",
      "3" = "Dia",
      "4" = "Mes",
      "5" = "Ano"
    ),
    
    # Traducciones de valores existentes de la funcion original
    "LOCNASC" = c(
      "1" = "Hospital",
      "2" = "Otro establecimiento de salud",
      "3" = "Domicilio",
      "4" = "Otros"
    ),
    
    "ESTCIVMAE" = c(
      "1" = "Soltera",
      "2" = "Casada",
      "3" = "Viuda",
      "4" = "Separada judicialmente",
      "5" = "Union consensual"
    ),
    
    "CONSULTAS" = c(
      "1" = "Ninguna",
      "2" = "1 a 3 veces",
      "3" = "4 a 6 veces",
      "4" = "7 o mas veces"
    ),
    
    "RACA_COR" = c(
      "01" = "Blanca",
      "02" = "Negra",
      "03" = "Amarilla",
      "04" = "Parda",
      "05" = "Indigena"
    ),
    
    "MORTE" = c(
      "0" = "No",
      "1" = "Si"
    ),
    
    "CS_SEXO" = c(
      "M" = "Masculino",
      "F" = "Femenino",
      "I" = "Ignorado"
    ),
    
    "CS_RACA" = c(
      "1" = "Blanca",
      "2" = "Negra",
      "3" = "Amarilla",
      "4" = "Parda",
      "5" = "Indigena",
      "9" = "Ignorado"
    ),
    
    "CS_GESTANT" = c(
      "1" = "1er trimestre",
      "2" = "2do trimestre",
      "3" = "3er trimestre",
      "4" = "Edad gestacional ignorada",
      "5" = "No aplica",
      "6" = "No embarazada",
      "9" = "Ignorado"
    ),
    
    "EVOLUCAO" = c(
      "1" = "Cura",
      "2" = "Muerte por la enfermedad",
      "3" = "Muerte por otras causas",
      "9" = "Ignorado"
    ),
    
    # FONTEINV - Fuente de investigacion
    "FONTEINV" = c(
      "1" = "Comite de Muerte Materna y/o Infantil",
      "2" = "Visita domiciliaria / Entrevista familiar",
      "3" = "Establecimiento de Salud / Prontuario",
      "4" = "Relacionado con otras bases de datos",
      "5" = "SVO",
      "6" = "IML",
      "7" = "Otra fuente",
      "8" = "Multiples fuentes",
      "9" = "Ignorado"
    ),
    
    # STDONOVA - Estado de certificado de muerte nuevo
    "STDONOVA" = c(
      "1" = "Si",
      "0" = "No"
    ),
    
    # FONTESINF - Fuentes de informacion
    "FONTESINF" = c(
      "S" = "Si",
      "X" = "No"
    ),
    
    # ESCFALAGR1 - Escolaridad del fallecido agregada
    "ESCFALAGR1" = c(
      "00" = "Sin escolaridad",
      "01" = "Primaria I incompleta",
      "02" = "Primaria I completa",
      "03" = "Primaria II incompleta",
      "04" = "Primaria II completa",
      "05" = "Secundaria incompleta",
      "06" = "Secundaria completa",
      "07" = "Superior incompleto",
      "08" = "Superior completo",
      "09" = "Ignorado",
      "10" = "Primaria I incompleta o inespecifica",
      "11" = "Primaria II incompleta o inespecifica",
      "12" = "Secundaria incompleta o inespecifica"
    )
  )
  
  return(list(columns = columns, values = values))
}

#' Get the Translation Dictionary for SINAN (English)
#'
#' Returns a list containing dictionaries to translate the column names
#' and categorical variable values of the SINAN system into English.
#' The goal is to standardize and facilitate data analysis.
#'
#' @return A list with two elements
#' @keywords internal
#' @noRd

get_translation_dict_en_sinan <- function() {

  # ==========================================================================
  # COLUMN NAME TRANSLATIONS
  # Maps original SINAN names to a standardized English snake_case format.
  # ==========================================================================

  columns <- c(
    # --- Notification and Disease Identification ---
    "TP_NOT" = "notification_type",
    "ID_AGRAVO" = "disease",
    "DT_NOTIFIC" = "notification_date",
    "SEM_NOT" = "notification_week",
    "NU_ANO" = "notification_year",
    "SG_UF_NOT" = "notification_uf",
    "ID_MUNICIP" = "notification_municipality_code",
    "ID_REGIONA" = "regional_health_code",
    "ID_UNIDADE" = "health_unit_code",

    # --- Patient Data ---
    "DT_SIN_PRI" = "first_symptoms_date",
    "SEM_PRI" = "first_symptoms_week",
    "ANO_NASC" = "birth_year",
    "NU_IDADE_N" = "age_code",
    "CS_SEXO" = "sex",
    "CS_GESTANT" = "gestation_status",
    "CS_RACA" = "race",
    "CS_ESCOL_N" = "education_level",
    "SG_UF" = "residence_uf",
    "ID_MN_RESI" = "residence_municipality_code",
    "ID_RG_RESI" = "residence_regional_code",
    "ID_PAIS" = "residence_country_code",
    "ID_OCUPA_N" = "occupation",

    # --- Investigation and Closure ---
    "DT_INVEST" = "investigation_date",
    "CLASSI_FIN" = "final_classification",
    "CRITERIO" = "confirmation_criteria",
    "EVOLUCAO" = "case_evolution",
    "DT_OBITO" = "death_date",
    "DT_ENCERRA" = "closure_date",

    # --- Clinical Signs and Symptoms ---
    "FEBRE" = "symptom_fever",
    "MIALGIA" = "symptom_myalgia",
    "CEFALEIA" = "symptom_headache",
    "EXANTEMA" = "symptom_exanthem",
    "VOMITO" = "symptom_vomiting",
    "NAUSEA" = "symptom_nausea",
    "DOR_COSTAS" = "symptom_back_pain",
    "CONJUNTVIT" = "symptom_conjunctivitis",
    "ARTRITE" = "symptom_arthritis",
    "ARTRALGIA" = "symptom_arthralgia",
    "PETEQUIA_N" = "symptom_petechiae",
    "LEUCOPENIA" = "symptom_leukopenia",
    "LACO" = "symptom_tourniquet_test_positive",
    "DOR_RETRO" = "symptom_retro_orbital_pain",
    "MANI_HEMOR" = "hemorrhagic_manifestation",
    "EPISTAXE" = "symptom_epistaxis",
    "GENGIVO" = "symptom_gingivorrhagia",
    "METRO" = "symptom_metrorrhagia",
    "PETEQUIAS" = "symptom_spontaneous_petechiae",
    "HEMATURA" = "symptom_hematuria",
    "SANGRAM" = "symptom_digestive_tract_bleeding",

    # --- Pre-existing Conditions / Comorbidities ---
    "DIABETES" = "comorbidity_diabetes",
    "HEMATOLOG" = "comorbidity_hematologic_disease",
    "HEPATOPAT" = "comorbidity_liver_disease",
    "RENAL" = "comorbidity_renal_disease",
    "HIPERTENSA" = "comorbidity_hypertension",
    "ACIDO_PEPT" = "comorbidity_peptic_acid_disease",
    "AUTO_IMUNE" = "comorbidity_autoimmune_disease",

    # --- Warning Signs (Dengue) ---
    "ALRM_HIPOT" = "warning_sign_hypotension",
    "ALRM_PLAQ" = "warning_sign_platelet_drop",
    "ALRM_VOM" = "warning_sign_persistent_vomiting",
    "ALRM_SANG" = "warning_sign_mucosal_bleed",
    "ALRM_HEMAT" = "warning_sign_hematocrit_increase",
    "ALRM_ABDOM" = "warning_sign_abdominal_pain",
    "ALRM_LETAR" = "warning_sign_lethargy_irritability",
    "ALRM_HEPAT" = "warning_sign_hepatomegaly",
    "ALRM_LIQ" = "warning_sign_fluid_accumulation",
    "DT_ALRM" = "warning_signs_date",

    # --- Severity Signs (Severe Dengue) ---
    "GRAV_PULSO" = "severe_sign_weak_pulse",
    "GRAV_CONV" = "severe_sign_convergent_bp",
    "GRAV_ENCH" = "severe_sign_slow_capillary_refill",
    "GRAV_INSUF" = "severe_sign_respiratory_failure",
    "GRAV_TAQUI" = "severe_sign_tachycardia",
    "GRAV_EXTRE" = "severe_sign_cold_extremities",
    "GRAV_HIPOT" = "severe_sign_late_hypotension",
    "GRAV_HEMAT" = "severe_sign_hematemesis",
    "GRAV_MELEN" = "severe_sign_melena",
    "GRAV_METRO" = "severe_sign_voluminous_metrorrhagia",
    "GRAV_SANG" = "severe_sign_cns_bleeding",
    "GRAV_AST" = "severe_sign_ast_alt_high",
    "GRAV_MIOC" = "severe_sign_myocarditis",
    "GRAV_CONSC" = "severe_sign_consciousness_alteration",
    "GRAV_ORGAO" = "severe_sign_organ_failure",
    "DT_GRAV" = "severity_signs_date",

    # --- Laboratory Data ---
    "DT_CHIK_S1" = "chik_s1_collection_date",
    "DT_CHIK_S2" = "chik_s2_collection_date",
    "RES_CHIKS1" = "chik_s1_result",
    "RES_CHIKS2" = "chik_s2_result",
    "DT_PRNT" = "prnt_collection_date",
    "RESUL_PRNT" = "prnt_result",
    "DT_SORO" = "dengue_serology_collection_date",
    "RESUL_SORO" = "dengue_serology_result",
    "DT_NS1" = "ns1_collection_date",
    "RESUL_NS1" = "ns1_result",
    "DT_VIRAL" = "viral_isolation_collection_date",
    "RESUL_VI_N" = "viral_isolation_result",
    "DT_PCR" = "pcr_collection_date",
    "RESUL_PCR_" = "pcr_result",
    "SOROTIPO" = "serotype",
    "HISTOPA_N" = "histopathology_result",
    "IMUNOH_N" = "immunohistochemistry_result",
    "LACO_N" = "tourniquet_test_result",
    "PLASMATICO" = "plasma_leakage",
    "EVIDENCIA" = "plasma_leakage_evidence",
    "PLAQ_MENOR" = "platelets_below_100k",
    "CON_FHD" = "dengue_hemorrhagic_fever_confirmation",
    "COMPLICA" = "complications",

    # --- Hospitalization and Infection Location Data ---
    "HOSPITALIZ" = "hospitalization",
    "DT_INTERNA" = "hospitalization_date",
    "UF" = "hospitalization_uf",
    "MUNICIPIO" = "hospitalization_municipality",
    "TPAUTOCTO" = "autochthonous_case",
    "COUFINF" = "probable_infection_uf",
    "COPAISINF" = "probable_infection_country",
    "COMUNINF" = "probable_infection_municipality",

    # --- Administrative and System Data ---
    "DOENCA_TRA" = "work_related_disease",
    "CLINC_CHIK" = "chikungunya_clinical_presentation",
    "TP_SISTEMA" = "origin_system_type",
    "NDUPLIC_N" = "duplication_number",
    "DT_DIGITA" = "entry_date",
    "CS_FLXRET" = "return_flow_status",
    "FLXRECEBI" = "flow_received",
    "MIGRADO_W" = "migrated_from_sinan_net"
  )

  # ==========================================================================
  # CATEGORICAL VALUE TRANSLATIONS
  # Decodes numeric codes into human-readable English labels.
  # ==========================================================================

  values <- list(
    "TP_NOT" = c("2" = "Individual"),
    "CS_SEXO" = c("M" = "Male", "F" = "Female", "I" = "Ignored"),
    "CS_GESTANT" = c(
      "1" = "1st Trimester", "2" = "2nd Trimester", "3" = "3rd Trimester",
      "4" = "Gestational age ignored", "5" = "No", "6" = "Not applicable", "9" = "Ignored"
    ),
    "CS_RACA" = c(
      "1" = "White", "2" = "Black", "3" = "Asian", "4" = "Brown", "5" = "Indigenous", "9" = "Ignored"
    ),
    "CS_ESCOL_N" = c(
      "0" = "Illiterate", "1" = "1-4 years of primary school (incomplete)", "2" = "4 years of primary school (complete)",
      "3" = "5-8 years of primary school (incomplete)", "4" = "Primary school (complete)",
      "5" = "High school (incomplete)", "6" = "High school (complete)",
      "7" = "Higher education (incomplete)", "8" = "Higher education (complete)",
      "9" = "Ignored", "10" = "Not applicable"
    ),
    "CLASSI_FIN" = c(
      "5" = "Discarded", "10" = "Dengue", "11" = "Dengue with warning signs",
      "12" = "Severe Dengue", "13" = "Chikungunya"
    ),
    "CRITERIO" = c("1" = "Laboratory", "2" = "Clinical-epidemiological", "3" = "Under investigation"),
    "EVOLUCAO" = c(
      "1" = "Cure", "2" = "Death by disease", "3" = "Death by other causes",
      "4" = "Death under investigation", "9" = "Ignored"
    ),
    "HOSPITALIZ" = c("1" = "Yes", "2" = "No", "9" = "Ignored"),
    "TPAUTOCTO" = c("1" = "Yes", "2" = "No", "3" = "Indeterminate"),
    "DOENCA_TRA" = c("1" = "Yes", "2" = "No", "9" = "Ignored"),
    "CLINC_CHIK" = c("1" = "Acute", "2" = "Chronic"),
    "SOROTIPO" = c("1" = "DEN-1", "2" = "DEN-2", "3" = "DEN-3", "4" = "DEN-4"),

    # --- Standard Yes/No/Ignored (1, 2, 9) variables ---
    "YES_NO_IGNORED" = c("1" = "Yes", "2" = "No", "9" = "Ignored"),
    # --- Standard Lab Result (1, 2, 3, 4) variables ---
    "LAB_RESULT" = c("1" = "Positive", "2" = "Negative", "3" = "Inconclusive", "4" = "Not performed")
  )

  # Apply the standard mapping to all symptom/sign variables
  symptom_vars <- c(
    "FEBRE", "MIALGIA", "CEFALEIA", "EXANTEMA", "VOMITO", "NAUSEA", "DOR_COSTAS",
    "CONJUNTVIT", "ARTRITE", "ARTRALGIA", "PETEQUIA_N", "LEUCOPENIA", "LACO", "DOR_RETRO",
    "DIABETES", "HEMATOLOG", "HEPATOPAT", "RENAL", "HIPERTENSA", "ACIDO_PEPT", "AUTO_IMUNE",
    "ALRM_HIPOT", "ALRM_PLAQ", "ALRM_VOM", "ALRM_SANG", "ALRM_HEMAT", "ALRM_ABDOM",
    "ALRM_LETAR", "ALRM_HEPAT", "ALRM_LIQ", "GRAV_PULSO", "GRAV_CONV", "GRAV_ENCH",
    "GRAV_INSUF", "GRAV_TAQUI", "GRAV_EXTRE", "GRAV_HIPOT", "GRAV_HEMAT", "GRAV_MELEN",
    "GRAV_METRO", "GRAV_SANG", "GRAV_AST", "GRAV_MIOC", "GRAV_CONSC", "GRAV_ORGAO",
    "MANI_HEMOR", "EPISTAXE", "GENGIVO", "METRO", "PETEQUIAS", "HEMATURA", "SANGRAM",
    "PLASMATICO", "EVIDENCIA", "PLAQ_MENOR"
  )
  for (var in symptom_vars) {
    values[[var]] <- values[["YES_NO_IGNORED"]]
  }

  # Apply the standard mapping to all lab result variables
  lab_result_vars <- c(
    "RES_CHIKS1", "RES_CHIKS2", "RESUL_PRNT", "RESUL_SORO", "RESUL_NS1",
    "RESUL_VI_N", "RESUL_PCR_", "HISTOPA_N", "IMUNOH_N", "LACO_N"
  )
  for (var in lab_result_vars) {
    values[[var]] <- values[["LAB_RESULT"]]
  }

  # Remove the templates as they are not real columns
  values[["YES_NO_IGNORED"]] <- NULL
  values[["LAB_RESULT"]] <- NULL

  return(list(columns = columns, values = values))
}

#' Obtem o Dicionario de Traducao para o SINAN (Portugues)
#'
#' 
#' Retorna uma lista contendo dicionarios para traduzir os nomes das colunas
#' e os valores das variaveis categoricas do sistema SINAN para o portugues.
#' O objetivo e padronizar e facilitar a analise dos dados.
#'
#' @return Uma lista com dois elementos
#' @keywords internal
#' @noRd
get_translation_dict_pt_sinan <- function() {

  # ==========================================================================
  # TRADUCAO DOS NOMES DAS COLUNAS
  # Mapeia os nomes originais do SINAN para um formato padronizado em portugues.
  # ==========================================================================

  columns <- c(
    # --- Identificacao da Notificacao e Agravo ---
    "TP_NOT" = "tipo_notificacao",
    "ID_AGRAVO" = "agravo",
    "DT_NOTIFIC" = "data_notificacao",
    "SEM_NOT" = "semana_notificacao",
    "NU_ANO" = "ano_notificacao",
    "SG_UF_NOT" = "uf_notificacao",
    "ID_MUNICIP" = "codigo_municipio_notificacao",
    "ID_REGIONA" = "codigo_regional_saude",
    "ID_UNIDADE" = "codigo_unidade_saude",

    # --- Dados do Paciente ---
    "DT_SIN_PRI" = "data_primeiros_sintomas",
    "SEM_PRI" = "semana_primeiros_sintomas",
    "ANO_NASC" = "ano_nascimento",
    "NU_IDADE_N" = "codigo_idade",
    "CS_SEXO" = "sexo",
    "CS_GESTANT" = "gestante",
    "CS_RACA" = "raca",
    "CS_ESCOL_N" = "escolaridade",
    "SG_UF" = "uf_residencia",
    "ID_MN_RESI" = "codigo_municipio_residencia",
    "ID_RG_RESI" = "codigo_regional_residencia",
    "ID_PAIS" = "codigo_pais_residencia",
    "ID_OCUPA_N" = "ocupacao",

    # --- Investigacao e Encerramento ---
    "DT_INVEST" = "data_investigacao",
    "CLASSI_FIN" = "classificacao_final",
    "CRITERIO" = "criterio_confirmacao",
    "EVOLUCAO" = "evolucao_caso",
    "DT_OBITO" = "data_obito",
    "DT_ENCERRA" = "data_encerramento",

    # --- Sinais e Sintomas Clinicos ---
    "FEBRE" = "sinal_febre",
    "MIALGIA" = "sinal_mialgia",
    "CEFALEIA" = "sinal_cefaleia",
    "EXANTEMA" = "sinal_exantema",
    "VOMITO" = "sinal_vomito",
    "NAUSEA" = "sinal_nausea",
    "DOR_COSTAS" = "sinal_dor_costas",
    "CONJUNTVIT" = "sinal_conjuntivite",
    "ARTRITE" = "sinal_artrite",
    "ARTRALGIA" = "sinal_artralgia",
    "PETEQUIA_N" = "sinal_petequias",
    "LEUCOPENIA" = "sinal_leucopenia",
    "LACO" = "sinal_prova_laco_positiva",
    "DOR_RETRO" = "sinal_dor_retro_orbital",
    "MANI_HEMOR" = "manifestacao_hemorragica",
    "EPISTAXE" = "sinal_epistaxe",
    "GENGIVO" = "sinal_gengivorragia",
    "METRO" = "sinal_metrorragia",
    "PETEQUIAS" = "sinal_petequias_espontaneas",
    "HEMATURA" = "sinal_hematuria",
    "SANGRAM" = "sinal_sangramento_trato_digestivo",

    # --- Doencas Pre-existentes / Comorbidades ---
    "DIABETES" = "comorbidade_diabetes",
    "HEMATOLOG" = "comorbidade_doenca_hematologica",
    "HEPATOPAT" = "comorbidade_hepatopatia",
    "RENAL" = "comorbidade_doenca_renal",
    "HIPERTENSA" = "comorbidade_hipertensao_arterial",
    "ACIDO_PEPT" = "comorbidade_doenca_acido_peptica",
    "AUTO_IMUNE" = "comorbidade_doenca_autoimune",

    # --- Sinais de Alarme (Dengue) ---
    "ALRM_HIPOT" = "alarme_hipotensao",
    "ALRM_PLAQ" = "alarme_queda_plaquetas",
    "ALRM_VOM" = "alarme_vomito_persistente",
    "ALRM_SANG" = "alarme_sangramento_mucosa",
    "ALRM_HEMAT" = "alarme_aumento_hematocrito",
    "ALRM_ABDOM" = "alarme_dor_abdominal",
    "ALRM_LETAR" = "alarme_letargia_irritabilidade",
    "ALRM_HEPAT" = "alarme_hepatomegalia",
    "ALRM_LIQ" = "alarme_acumulo_liquidos",
    "DT_ALRM" = "data_sinais_alarme",

    # --- Sinais de Gravidade (Dengue Grave) ---
    "GRAV_PULSO" = "grave_pulso_debil",
    "GRAV_CONV" = "grave_pa_convergente",
    "GRAV_ENCH" = "grave_enchimento_capilar_lento",
    "GRAV_INSUF" = "grave_insuficiencia_respiratoria",
    "GRAV_TAQUI" = "grave_taquicardia",
    "GRAV_EXTRE" = "grave_extremidades_frias",
    "GRAV_HIPOT" = "grave_hipotensao_arterial",
    "GRAV_HEMAT" = "grave_hematemese",
    "GRAV_MELEN" = "grave_melena",
    "GRAV_METRO" = "grave_metrorragia_volumosa",
    "GRAV_SANG" = "grave_sangramento_snc",
    "GRAV_AST" = "grave_ast_alt_elevado",
    "GRAV_MIOC" = "grave_miocardite",
    "GRAV_CONSC" = "grave_alteracao_consciencia",
    "GRAV_ORGAO" = "grave_falencia_orgaos",
    "DT_GRAV" = "data_sinais_gravidade",

    # --- Dados Laboratoriais ---
    "DT_CHIK_S1" = "data_coleta_chik_s1",
    "DT_CHIK_S2" = "data_coleta_chik_s2",
    "RES_CHIKS1" = "resultado_chik_s1",
    "RES_CHIKS2" = "resultado_chik_s2",
    "DT_PRNT" = "data_coleta_prnt",
    "RESUL_PRNT" = "resultado_prnt",
    "DT_SORO" = "data_coleta_sorologia_dengue",
    "RESUL_SORO" = "resultado_sorologia_dengue",
    "DT_NS1" = "data_coleta_ns1",
    "RESUL_NS1" = "resultado_ns1",
    "DT_VIRAL" = "data_coleta_isolamento_viral",
    "RESUL_VI_N" = "resultado_isolamento_viral",
    "DT_PCR" = "data_coleta_pcr",
    "RESUL_PCR_" = "resultado_pcr",
    "SOROTIPO" = "sorotipo",
    "HISTOPA_N" = "resultado_histopatologia",
    "IMUNOH_N" = "resultado_imunohistoquimica",
    "LACO_N" = "resultado_prova_laco",
    "PLASMATICO" = "extravasamento_plasmatico",
    "EVIDENCIA" = "evidencia_extravasamento",
    "PLAQ_MENOR" = "plaquetas_menor_100mil",
    "CON_FHD" = "confirmacao_febre_hemorragica",
    "COMPLICA" = "complicacoes",

    # --- Dados de Hospitalizacao e Local de Infeccao ---
    "HOSPITALIZ" = "hospitalizacao",
    "DT_INTERNA" = "data_internacao",
    "UF" = "uf_hospitalizacao",
    "MUNICIPIO" = "municipio_hospitalizacao",
    "TPAUTOCTO" = "caso_autoctone",
    "COUFINF" = "uf_provavel_infeccao",
    "COPAISINF" = "pais_provavel_infeccao",
    "COMUNINF" = "municipio_provavel_infeccao",

    # --- Dados Administrativos e de Sistema ---
    "DOENCA_TRA" = "doenca_trabalho",
    "CLINC_CHIK" = "apresentacao_clinica_chikungunya",
    "TP_SISTEMA" = "tipo_sistema_origem",
    "NDUPLIC_N" = "numero_duplicidade",
    "DT_DIGITA" = "data_digitacao",
    "CS_FLXRET" = "status_fluxo_retorno",
    "FLXRECEBI" = "fluxo_recebido",
    "MIGRADO_W" = "migrado_sinan_net"
  )

  # ==========================================================================
  # TRADUCAO DE VALORES CATEGORICOS
  # Decodifica os codigos para rotulos legiveis em portugues.
  # ==========================================================================

  values <- list(
    "SG_UF" = c("12" = "AC", "13" = "AM", "15" = "PA", "14" = "RR", "17" = "TO",
  "27" = "AL", "29" = "BA", "23" = "CE", "PE" = "26", "RN" = "24",
  "32" = "ES", "31" = "MG", "33" = "RJ", "35" = "SP",
  "41" = "PR", "42" = "SC", "43" = "RS",
  "53" = "DF", "52" = "GO", "51" = "MT", "50" = "MS"
  ),
  "UF" = c("12" = "AC", "13" = "AM", "15" = "PA", "14" = "RR", "17" = "TO",
  "27" = "AL", "29" = "BA", "23" = "CE", "PE" = "26", "RN" = "24",
  "32" = "ES", "31" = "MG", "33" = "RJ", "35" = "SP",
  "41" = "PR", "42" = "SC", "43" = "RS",
  "53" = "DF", "52" = "GO", "51" = "MT", "50" = "MS"
  ),
  "COUFINF" = c("12" = "AC", "13" = "AM", "15" = "PA", "14" = "RR", "17" = "TO",
  "27" = "AL", "29" = "BA", "23" = "CE", "PE" = "26", "RN" = "24",
  "32" = "ES", "31" = "MG", "33" = "RJ", "35" = "SP",
  "41" = "PR", "42" = "SC", "43" = "RS",
  "53" = "DF", "52" = "GO", "51" = "MT", "50" = "MS"
  ),
  "SG_UF_NOT" = c("12" = "AC", "13" = "AM", "15" = "PA", "14" = "RR", "17" = "TO",
  "27" = "AL", "29" = "BA", "23" = "CE", "PE" = "26", "RN" = "24",
  "32" = "ES", "31" = "MG", "33" = "RJ", "35" = "SP",
  "41" = "PR", "42" = "SC", "43" = "RS",
  "53" = "DF", "52" = "GO", "51" = "MT", "50" = "MS"
  ),
    "TP_NOT" = c("2" = "Individual"),
    "CS_SEXO" = c("M" = "Masculino", "F" = "Feminino", "I" = "Ignorado"),
    "CS_GESTANT" = c(
      "1" = "1 Trimestre", "2" = "2 Trimestre", "3" = "3 Trimestre",
      "4" = "Idade gestacional ignorada", "5" = "Nao", "6" = "Nao se aplica", "9" = "Ignorado"
    ),
    "CS_RACA" = c(
      "1" = "Branca", "2" = "Preta", "3" = "Amarela", "4" = "Parda", "5" = "Indigena", "9" = "Ignorado"
    ),
    "CS_ESCOL_N" = c(
      "0" = "Analfabeto", "1" = "1 a 4 serie incompleta EF", "2" = "4 serie completa EF",
      "3" = "5 a 8 serie incompleta EF", "4" = "Ensino fundamental completo",
      "5" = "Ensino medio incompleto", "6" = "Ensino medio completo",
      "7" = "Educacao superior incompleta", "8" = "Educacao superior completa",
      "9" = "Ignorado", "10" = "Nao se aplica"
    ),
    "CLASSI_FIN" = c(
      "5" = "Descartado", "10" = "Dengue", "11" = "Dengue com sinais de alarme",
      "12" = "Dengue grave", "13" = "Chikungunya"
    ),
    "CRITERIO" = c("1" = "Laboratorio", "2" = "Clinico-epidemiologico", "3" = "Em investigacao"),
    "EVOLUCAO" = c(
      "1" = "Cura", "2" = "Obito pelo agravo", "3" = "Obito por outras causas",
      "4" = "Obito em investigacao", "9" = "Ignorado"
    ),
    "HOSPITALIZ" = c("1" = "Sim", "2" = "Nao", "9" = "Ignorado"),
    "TPAUTOCTO" = c("1" = "Sim", "2" = "Nao", "3" = "Indeterminado"),
    "DOENCA_TRA" = c("1" = "Sim", "2" = "Nao", "9" = "Ignorado"),
    "CLINC_CHIK" = c("1" = "Aguda", "2" = "Cronica"),
    "SOROTIPO" = c("1" = "DEN-1", "2" = "DEN-2", "3" = "DEN-3", "4" = "DEN-4"),
    
    # --- Variaveis Sim/Nao/Ignorado (padrao 1, 2, 9) ---
    "SINAL_PADRAO" = c("1" = "Sim", "2" = "Nao", "9" = "Ignorado"),
    "RESULTADO_LAB_PADRAO" = c("1" = "Positivo", "2" = "Negativo", "3" = "Inconclusivo", "4" = "Nao realizado")
  )
  
  # Adiciona o mapeamento padrao para todas as variaveis de sinais/sintomas
  sinais_vars <- c(
    "FEBRE", "MIALGIA", "CEFALEIA", "EXANTEMA", "VOMITO", "NAUSEA", "DOR_COSTAS",
    "CONJUNTVIT", "ARTRITE", "ARTRALGIA", "PETEQUIA_N", "LEUCOPENIA", "LACO", "DOR_RETRO",
    "DIABETES", "HEMATOLOG", "HEPATOPAT", "RENAL", "HIPERTENSA", "ACIDO_PEPT", "AUTO_IMUNE",
    "ALRM_HIPOT", "ALRM_PLAQ", "ALRM_VOM", "ALRM_SANG", "ALRM_HEMAT", "ALRM_ABDOM",
    "ALRM_LETAR", "ALRM_HEPAT", "ALRM_LIQ", "GRAV_PULSO", "GRAV_CONV", "GRAV_ENCH",
    "GRAV_INSUF", "GRAV_TAQUI", "GRAV_EXTRE", "GRAV_HIPOT", "GRAV_HEMAT", "GRAV_MELEN",
    "GRAV_METRO", "GRAV_SANG", "GRAV_AST", "GRAV_MIOC", "GRAV_CONSC", "GRAV_ORGAO",
    "MANI_HEMOR", "EPISTAXE", "GENGIVO", "METRO", "PETEQUIAS", "HEMATURA", "SANGRAM",
    "PLASMATICO", "EVIDENCIA", "PLAQ_MENOR"
  )
  for (var in sinais_vars) {
    values[[var]] <- values[["SINAL_PADRAO"]]
  }
  
  # Adiciona o mapeamento padrao para resultados de laboratorio
  lab_vars <- c(
    "RES_CHIKS1", "RES_CHIKS2", "RESUL_PRNT", "RESUL_SORO", "RESUL_NS1",
    "RESUL_VI_N", "RESUL_PCR_", "HISTOPA_N", "IMUNOH_N", "LACO_N"
  )
  for (var in lab_vars) {
    values[[var]] <- values[["RESULTADO_LAB_PADRAO"]]
  }

  # Remove os templates que nao sao colunas reais
  values[["SINAL_PADRAO"]] <- NULL
  values[["RESULTADO_LAB_PADRAO"]] <- NULL

  return(list(columns = columns, values = values))
}

#' Obtiene el Diccionario de Traduccion para SINAN (Espanol)
#'
#' Devuelve una lista que contiene diccionarios para traducir los nombres de las columnas
#' y los valores de las variables categoricas del sistema SINAN al espanol.
#' El objetivo es estandarizar y facilitar el analisis de datos.
#'
#' @return Una lista con dos elementos
#' @keywords internal
#' @noRd
get_translation_dict_es_sinan <- function() {

  # ==========================================================================
  # TRADUCCION DE NOMBRES DE COLUMNAS
  # Mapea los nombres originales de SINAN a un formato estandarizado en espanol.
  # ==========================================================================

  columns <- c(
    # --- Identificacion de la Notificacion y Enfermedad ---
    "TP_NOT" = "tipo_notificacion",
    "ID_AGRAVO" = "enfermedad",
    "DT_NOTIFIC" = "fecha_notificacion",
    "SEM_NOT" = "semana_notificacion",
    "NU_ANO" = "ano_notificacion",
    "SG_UF_NOT" = "uf_notificacion",
    "ID_MUNICIP" = "codigo_municipio_notificacion",
    "ID_REGIONA" = "codigo_regional_salud",
    "ID_UNIDADE" = "codigo_unidad_salud",

    # --- Datos del Paciente ---
    "DT_SIN_PRI" = "fecha_primeros_sintomas",
    "SEM_PRI" = "semana_primeros_sintomas",
    "ANO_NASC" = "ano_nacimiento",
    "NU_IDADE_N" = "codigo_edad",
    "CS_SEXO" = "sexo",
    "CS_GESTANT" = "estado_gestacion",
    "CS_RACA" = "raza",
    "CS_ESCOL_N" = "nivel_educativo",
    "SG_UF" = "uf_residencia",
    "ID_MN_RESI" = "codigo_municipio_residencia",
    "ID_RG_RESI" = "codigo_regional_residencia",
    "ID_PAIS" = "codigo_pais_residencia",
    "ID_OCUPA_N" = "ocupacion",

    # --- Investigacion y Cierre ---
    "DT_INVEST" = "fecha_investigacion",
    "CLASSI_FIN" = "clasificacion_final",
    "CRITERIO" = "criterio_confirmacion",
    "EVOLUCAO" = "evolucion_caso",
    "DT_OBITO" = "fecha_fallecimiento",
    "DT_ENCERRA" = "fecha_cierre",

    # --- Signos y Sintomas Clinicos ---
    "FEBRE" = "sintoma_fiebre",
    "MIALGIA" = "sintoma_mialgia",
    "CEFALEIA" = "sintoma_cefalea",
    "EXANTEMA" = "sintoma_exantema",
    "VOMITO" = "sintoma_vomito",
    "NAUSEA" = "sintoma_nausea",
    "DOR_COSTAS" = "sintoma_dolor_espalda",
    "CONJUNTVIT" = "sintoma_conjuntivitis",
    "ARTRITE" = "sintoma_artritis",
    "ARTRALGIA" = "sintoma_artralgia",
    "PETEQUIA_N" = "sintoma_petequias",
    "LEUCOPENIA" = "sintoma_leucopenia",
    "LACO" = "sintoma_prueba_torniquete_positiva",
    "DOR_RETRO" = "sintoma_dolor_retro_orbital",
    "MANI_HEMOR" = "manifestacion_hemorragica",
    "EPISTAXE" = "sintoma_epistaxis",
    "GENGIVO" = "sintoma_gingivorragia",
    "METRO" = "sintoma_metrorragia",
    "PETEQUIAS" = "sintoma_petequias_espontaneas",
    "HEMATURA" = "sintoma_hematuria",
    "SANGRAM" = "sintoma_sangrado_digestivo",

    # --- Enfermedades Preexistentes / Comorbilidades ---
    "DIABETES" = "comorbilidad_diabetes",
    "HEMATOLOG" = "comorbilidad_enfermedad_hematologica",
    "HEPATOPAT" = "comorbilidad_hepatopatia",
    "RENAL" = "comorbilidad_enfermedad_renal",
    "HIPERTENSA" = "comorbilidad_hipertension_arterial",
    "ACIDO_PEPT" = "comorbilidad_enfermedad_acido_peptica",
    "AUTO_IMUNE" = "comorbilidad_enfermedad_autoinmune",

    # --- Signos de Alarma (Dengue) ---
    "ALRM_HIPOT" = "alarma_hipotension",
    "ALRM_PLAQ" = "alarma_caida_plaquetas",
    "ALRM_VOM" = "alarma_vomito_persistente",
    "ALRM_SANG" = "alarma_sangrado_mucosa",
    "ALRM_HEMAT" = "alarma_aumento_hematocrito",
    "ALRM_ABDOM" = "alarma_dolor_abdominal",
    "ALRM_LETAR" = "alarma_letargo_irritabilidad",
    "ALRM_HEPAT" = "alarma_hepatomegalia",
    "ALRM_LIQ" = "alarma_acumulacion_liquidos",
    "DT_ALRM" = "fecha_signos_alarma",

    # --- Signos de Gravedad (Dengue Grave) ---
    "GRAV_PULSO" = "grave_pulso_debil",
    "GRAV_CONV" = "grave_pa_convergente",
    "GRAV_ENCH" = "grave_llenado_capilar_lento",
    "GRAV_INSUF" = "grave_insuficiencia_respiratoria",
    "GRAV_TAQUI" = "grave_taquicardia",
    "GRAV_EXTRE" = "grave_extremidades_frias",
    "GRAV_HIPOT" = "grave_hipotension_tardia",
    "GRAV_HEMAT" = "grave_hematemesis",
    "GRAV_MELEN" = "grave_melena",
    "GRAV_METRO" = "grave_metrorragia_voluminosa",
    "GRAV_SANG" = "grave_sangrado_snc",
    "GRAV_AST" = "grave_ast_alt_elevado",
    "GRAV_MIOC" = "grave_miocarditis",
    "GRAV_CONSC" = "grave_alteracion_conciencia",
    "GRAV_ORGAO" = "grave_fallo_organico",
    "DT_GRAV" = "fecha_signos_gravedad",

    # --- Datos de Laboratorio ---
    "DT_CHIK_S1" = "fecha_recoleccion_chik_s1",
    "DT_CHIK_S2" = "fecha_recoleccion_chik_s2",
    "RES_CHIKS1" = "resultado_chik_s1",
    "RES_CHIKS2" = "resultado_chik_s2",
    "DT_PRNT" = "fecha_recoleccion_prnt",
    "RESUL_PRNT" = "resultado_prnt",
    "DT_SORO" = "fecha_recoleccion_serologia_dengue",
    "RESUL_SORO" = "resultado_serologia_dengue",
    "DT_NS1" = "fecha_recoleccion_ns1",
    "RESUL_NS1" = "resultado_ns1",
    "DT_VIRAL" = "fecha_recoleccion_aislamiento_viral",
    "RESUL_VI_N" = "resultado_aislamiento_viral",
    "DT_PCR" = "fecha_recoleccion_pcr",
    "RESUL_PCR_" = "resultado_pcr",
    "SOROTIPO" = "serotipo",
    "HISTOPA_N" = "resultado_histopatologia",
    "IMUNOH_N" = "resultado_inmunohistoquimica",
    "LACO_N" = "resultado_prueba_torniquete",
    "PLASMATICO" = "extravasacion_plasmatica",
    "EVIDENCIA" = "evidencia_extravasacion",
    "PLAQ_MENOR" = "plaquetas_menor_100mil",
    "CON_FHD" = "confirmacion_fiebre_hemorragica",
    "COMPLICA" = "complicaciones",

    # --- Datos de Hospitalizacion y Lugar de Infeccion ---
    "HOSPITALIZ" = "hospitalizacion",
    "DT_INTERNA" = "fecha_hospitalizacion",
    "UF" = "uf_hospitalizacion",
    "MUNICIPIO" = "municipio_hospitalizacion",
    "TPAUTOCTO" = "caso_autoctono",
    "COUFINF" = "uf_probable_infeccion",
    "COPAISINF" = "pais_probable_infeccion",
    "COMUNINF" = "municipio_probable_infeccion",

    # --- Datos Administrativos y de Sistema ---
    "DOENCA_TRA" = "enfermedad_laboral",
    "CLINC_CHIK" = "presentacion_clinica_chikungunya",
    "TP_SISTEMA" = "tipo_sistema_origen",
    "NDUPLIC_N" = "numero_duplicidad",
    "DT_DIGITA" = "fecha_digitacion",
    "CS_FLXRET" = "estado_flujo_retorno",
    "FLXRECEBI" = "flujo_recibido",
    "MIGRADO_W" = "migrado_de_sinan_net"
  )

  # ==========================================================================
  # TRADUCCION DE VALORES CATEGORICOS
  # Decodifica los codigos a etiquetas legibles en espanol.
  # ==========================================================================

  values <- list(
    "TP_NOT" = c("2" = "Individual"),
    "CS_SEXO" = c("M" = "Masculino", "F" = "Femenino", "I" = "Ignorado"),
    "CS_GESTANT" = c(
      "1" = "1er Trimestre", "2" = "2do Trimestre", "3" = "3er Trimestre",
      "4" = "Edad gestacional ignorada", "5" = "No", "6" = "No aplica", "9" = "Ignorado"
    ),
    "CS_RACA" = c(
      "1" = "Blanca", "2" = "Negra", "3" = "Asiatica", "4" = "Parda", "5" = "Indigena", "9" = "Ignorado"
    ),
    "CS_ESCOL_N" = c(
      "0" = "Analfabeto", "1" = "Primaria incompleta (1-4 anos)", "2" = "Primaria 4 anos completa",
      "3" = "Primaria incompleta (5-8 anos)", "4" = "Primaria completa",
      "5" = "Secundaria incompleta", "6" = "Secundaria completa",
      "7" = "Superior incompleta", "8" = "Superior completa",
      "9" = "Ignorado", "10" = "No aplica"
    ),
    "CLASSI_FIN" = c(
      "5" = "Descartado", "10" = "Dengue", "11" = "Dengue con signos de alarma",
      "12" = "Dengue grave", "13" = "Chikungunya"
    ),
    "CRITERIO" = c("1" = "Laboratorio", "2" = "Clinico-epidemiologico", "3" = "En investigacion"),
    "EVOLUCAO" = c(
      "1" = "Curacion", "2" = "Fallecimiento por la enfermedad", "3" = "Fallecimiento por otras causas",
      "4" = "Fallecimiento en investigacion", "9" = "Ignorado"
    ),
    "HOSPITALIZ" = c("1" = "Si", "2" = "No", "9" = "Ignorado"),
    "TPAUTOCTO" = c("1" = "Si", "2" = "No", "3" = "Indeterminado"),
    "DOENCA_TRA" = c("1" = "Si", "2" = "No", "9" = "Ignorado"),
    "CLINC_CHIK" = c("1" = "Aguda", "2" = "Cronica"),
    "SOROTIPO" = c("1" = "DEN-1", "2" = "DEN-2", "3" = "DEN-3", "4" = "DEN-4"),

    # --- Plantillas para valores estandar ---
    "SI_NO_IGNORADO" = c("1" = "Si", "2" = "No", "9" = "Ignorado"),
    "RESULTADO_LAB" = c("1" = "Positivo", "2" = "Negativo", "3" = "No concluyente", "4" = "No realizado")
  )

  # Aplicar la plantilla estandar a todas las variables de signos/sintomas
  sintomas_vars <- c(
    "FEBRE", "MIALGIA", "CEFALEIA", "EXANTEMA", "VOMITO", "NAUSEA", "DOR_COSTAS",
    "CONJUNTVIT", "ARTRITE", "ARTRALGIA", "PETEQUIA_N", "LEUCOPENIA", "LACO", "DOR_RETRO",
    "DIABETES", "HEMATOLOG", "HEPATOPAT", "RENAL", "HIPERTENSA", "ACIDO_PEPT", "AUTO_IMUNE",
    "ALRM_HIPOT", "ALRM_PLAQ", "ALRM_VOM", "ALRM_SANG", "ALRM_HEMAT", "ALRM_ABDOM",
    "ALRM_LETAR", "ALRM_HEPAT", "ALRM_LIQ", "GRAV_PULSO", "GRAV_CONV", "GRAV_ENCH",
    "GRAV_INSUF", "GRAV_TAQUI", "GRAV_EXTRE", "GRAV_HIPOT", "GRAV_HEMAT", "GRAV_MELEN",
    "GRAV_METRO", "GRAV_SANG", "GRAV_AST", "GRAV_MIOC", "GRAV_CONSC", "GRAV_ORGAO",
    "MANI_HEMOR", "EPISTAXE", "GENGIVO", "METRO", "PETEQUIAS", "HEMATURA", "SANGRAM",
    "PLASMATICO", "EVIDENCIA", "PLAQ_MENOR"
  )
  for (var in sintomas_vars) {
    values[[var]] <- values[["SI_NO_IGNORADO"]]
  }

  # Aplicar la plantilla estandar a todos los resultados de laboratorio
  resultados_lab_vars <- c(
    "RES_CHIKS1", "RES_CHIKS2", "RESUL_PRNT", "RESUL_SORO", "RESUL_NS1",
    "RESUL_VI_N", "RESUL_PCR_", "HISTOPA_N", "IMUNOH_N", "LACO_N"
  )
  for (var in resultados_lab_vars) {
    values[[var]] <- values[["RESULTADO_LAB"]]
  }

  # Eliminar las plantillas ya que no son columnas reales
  values[["SI_NO_IGNORADO"]] <- NULL
  values[["RESULTADO_LAB"]] <- NULL

  return(list(columns = columns, values = values))
}

#' Obtem o Dicionario de Traducao para o SIH (Portugues)
#'
#' Retorna uma lista contendo dicionarios para traduzir os nomes das colunas
#' e os valores das variaveis categoricas do Sistema de Informacoes Hospitalares (SIH)
#' para o portugues. O objetivo e padronizar e facilitar a analise dos dados.
#'
#' @return Uma lista com dois elementos
#' @keywords internal
#' @noRd
get_translation_dict_pt_sih <- function() {

  # ==========================================================================
  # TRADUCAO DOS NOMES DAS COLUNAS
  # Mapeia os nomes originais do SIH para um formato padronizado em portugues.
  # ==========================================================================

  columns <- c(
    # --- Identificacao da AIH e Competencia ---
    "UF_ZI" = "uf_gestor",
    "ANO_CMPT" = "ano_competencia",
    "MES_CMPT" = "mes_competencia",
    "ESPEC" = "especialidade_leito",
    "CGC_HOSP" = "cnpj_hospital",
    "N_AIH" = "numero_aih",
    "IDENT" = "identificador_aih",
    "SEQ_AIH5" = "sequencial_aih_longa_permanencia",

    # --- Dados do Paciente ---
    "CEP" = "cep_paciente",
    "MUNIC_RES" = "codigo_municipio_residencia",
    "NASC" = "data_nascimento",
    "SEXO" = "sexo",
    "IDADE" = "idade",
    "COD_IDADE" = "codigo_unidade_idade",
    "INSTRU" = "instrucao",
    "NUM_FILHOS" = "numero_filhos",
    "RACA_COR" = "raca",
    "ETNIA" = "etnia_indigena",
    "NACIONAL" = "nacionalidade",
    "HOMONIMO" = "homonimo",

    # --- Dados da Internacao ---
    "DT_INTER" = "data_internacao",
    "DT_SAIDA" = "data_saida",
    "DIAS_PERM" = "dias_permanencia",
    "DIAR_ACOM" = "diarias_acompanhante",
    "QT_DIARIAS" = "quantidade_diarias",
    "PROC_SOLIC" = "procedimento_solicitado",
    "PROC_REA" = "procedimento_realizado",
    "CAR_INT" = "carater_internacao",
    "MORTE" = "obito",
    "COBRANCA" = "motivo_saida_permanencia",
    "CID_NOTIF" = "cid_notificacao",
    "CID_ASSO" = "cid_causa_associada",
    "CID_MORTE" = "cid_obito",
    "DIAG_PRINC" = "diagnostico_principal",
    "DIAG_SECUN" = "diagnostico_secundario",
    # Diagnosticos secundarios adicionais
    "DIAGSEC1" = "diagnostico_secundario_1",
    "DIAGSEC2" = "diagnostico_secundario_2",
    "DIAGSEC3" = "diagnostico_secundario_3",
    "DIAGSEC4" = "diagnostico_secundario_4",
    "DIAGSEC5" = "diagnostico_secundario_5",
    "DIAGSEC6" = "diagnostico_secundario_6",
    "DIAGSEC7" = "diagnostico_secundario_7",
    "DIAGSEC8" = "diagnostico_secundario_8",
    "DIAGSEC9" = "diagnostico_secundario_9",
    "TPDISEC1" = "tipo_diagnostico_secundario_1",
    "TPDISEC2" = "tipo_diagnostico_secundario_2",
    "TPDISEC3" = "tipo_diagnostico_secundario_3",
    "TPDISEC4" = "tipo_diagnostico_secundario_4",
    "TPDISEC5" = "tipo_diagnostico_secundario_5",
    "TPDISEC6" = "tipo_diagnostico_secundario_6",
    "TPDISEC7" = "tipo_diagnostico_secundario_7",
    "TPDISEC8" = "tipo_diagnostico_secundario_8",
    "TPDISEC9" = "tipo_diagnostico_secundario_9",

    # --- UTI e UCI ---
    "UTI_MES_IN" = "uti_dias_mes_inicial",
    "UTI_MES_AN" = "uti_dias_mes_anterior",
    "UTI_MES_AL" = "uti_dias_mes_alta",
    "UTI_MES_TO" = "uti_dias_mes_total",
    "MARCA_UTI" = "tipo_uti_utilizada",
    "UTI_INT_IN" = "uti_diarias_intermediaria_inicial",
    "UTI_INT_AN" = "uti_diarias_intermediaria_anterior",
    "UTI_INT_AL" = "uti_diarias_intermediaria_alta",
    "UTI_INT_TO" = "uti_diarias_intermediaria_total",
    "VAL_UCI" = "valor_uci",
    "MARCA_UCI" = "tipo_uci_utilizada",

    # --- Valores da AIH ---
    "VAL_SH" = "valor_servicos_hospitalares",
    "VAL_SP" = "valor_servicos_profissionais",
    "VAL_TOT" = "valor_total_aih",
    "VAL_UTI" = "valor_uti",
    "US_TOT" = "valor_total_dolar",
    "VAL_SADT" = "valor_sadt",
    "VAL_RN" = "valor_recem_nascido",
    "VAL_ACOMP" = "valor_acompanhante",
    "VAL_ORTP" = "valor_ortese_protese",
    "VAL_SANGUE" = "valor_sangue",
    "VAL_SADTSR" = "valor_sadt_sem_remuneracao",
    "VAL_TRANSP" = "valor_transporte",
    "VAL_OBSANG" = "valor_opme_sangue",
    "VAL_PED1AC" = "valor_permanencia_maior",
    "VAL_SH_FED" = "valor_complemento_federal_sh",
    "VAL_SP_FED" = "valor_complemento_federal_sp",
    "VAL_SH_GES" = "valor_complemento_gestor_sh",
    "VAL_SP_GES" = "valor_complemento_gestor_sp",

    # --- Dados do Estabelecimento ---
    "NATUREZA" = "natureza_juridica_antiga",
    "NAT_JUR" = "natureza_juridica",
    "GESTAO" = "tipo_gestao_hospital",
    "MUNIC_MOV" = "codigo_municipio_estabelecimento",
    "CNES" = "codigo_cnes",
    "CNPJ_MANT" = "cnpj_mantenedora",
    "INFEHOSP" = "status_infeccao_hospitalar",
    "COMPLEX" = "complexidade",
    "FINANC" = "tipo_financiamento",
    "FAEC_TP" = "subtipo_financiamento_faec",
    "REGCT" = "regra_contratual",

    # --- Dados de Ginecologia e Obstetricia ---
    "IND_VDRL" = "exame_vdrl_realizado",
    "GESTRISCO" = "gestante_risco",
    "INSC_PN" = "numero_pre_natal",
    "CONTRACEP1" = "contraceptivo_principal",
    "CONTRACEP2" = "contraceptivo_secundario",

    # --- Dados Administrativos e de Pessoal ---
    "CBOR" = "cbo_paciente",
    "CNAER" = "cnae_acidente_trabalho",
    "VINCPREV" = "vinculo_previdencia",
    "GESTOR_COD" = "motivo_autorizacao_gestor",
    "GESTOR_TP" = "tipo_gestor",
    "GESTOR_CPF" = "cpf_gestor",
    "GESTOR_DT" = "data_autorizacao_gestor",
    "CPF_AUT" = "cpf_autorizador",
    "AUD_JUST" = "justificativa_auditor_cns",
    "SIS_JUST" = "justificativa_estabelecimento_cns",

    # --- Dados de Sistema e Remessa ---
    "RUBRICA" = "rubrica",
    "NUM_PROC" = "numero_procedimento",
    "TOT_PT_SP" = "total_pontos_sp",
    "SEQUENCIA" = "sequencial_aih_remessa",
    "REMESSA" = "numero_remessa"
  )

  # ==========================================================================
  # TRADUCAO DE VALORES CATEGORICOS
  # Decodifica os codigos para rotulos legiveis em portugues.
  # ==========================================================================

  values <- list(
    "IDENT" = c("1" = "Principal", "3" = "Longa permanencia", "5" = "Continuacao"),
    "SEXO" = c("1" = "Masculino", "3" = "Feminino", "0" = "Ignorado", "9" = "Ignorado"),
    "INSTRU" = c(
      "1" = "Analfabeto", "2" = "1 Grau", "3" = "2 Grau", "4" = "3 Grau", "9" = "Ignorado"
    ),
    "MORTE" = c("1" = "Sim", "0" = "Nao"),
    "CAR_INT" = c("1" = "Eletivo", "2" = "Urgencia", "3" = "Acidente no local de trabalho ou a servico da empresa", "4" = "Acidente no trajeto para o trabalho", "5" = "Outros tipos de acidente de transito", "6" = "Outras lesoes e envenenamentos por agentes quimicos ou fisicos"),
    "VINCPREV" = c(
      "1" = "Autonomo", "2" = "Desempregado", "3" = "Aposentado", "4" = "Nao segurado",
      "5" = "Empregado", "6" = "Empregador", "7" = "Beneficiario", "9" = "Ignorado"
    ),
    "GESTOR_TP" = c("E" = "Estadual", "M" = "Municipal"),
    "GESTRISCO" = c("1" = "Sim", "0" = "Nao"),
    "RACA_COR" = c(
      "01" = "Branca", "02" = "Preta", "03" = "Amarela", "04" = "Parda", "05" = "Indigena", "99" = "Sem informacao"
    ),
    "FINANC" = c(
      "1" = "Recursos proprios", "2" = "Custeio", "3" = "Investimento",
      "4" = "FAEC", "5" = "Outros", "6" = "Media e alta complexidade"
    ),
    "COMPLEX" = c("1" = "Atencao basica", "2" = "Media complexidade", "3" = "Alta complexidade"),
    "NATUREZA" = c(
      "10" = "Administracao Publica", "20" = "Entidades Empresariais",
      "30" = "Entidades sem Fins Lucrativos", "40" = "Pessoas Fisicas",
      "50" = "Organizacoes Internacionais", "99" = "Nao se aplica"
    )
  )

  return(list(columns = columns, values = values))
}

#' Get the Translation Dictionary for SIH (English)
#'
#' Returns a list containing dictionaries to translate the column names
#' and categorical variable values of the Hospital Information System (SIH)
#' into English. The goal is to standardize and facilitate data analysis.
#'
#' @return A list with two elements
#' @keywords internal
#' @noRd
get_translation_dict_en_sih <- function() {

  # ==========================================================================
  # COLUMN NAME TRANSLATIONS
  # Maps original SIH names to a standardized English snake_case format.
  # ==========================================================================

  columns <- c(
    # --- AIH and Competence Period Identification ---
    "UF_ZI" = "manager_uf",
    "ANO_CMPT" = "competence_year",
    "MES_CMPT" = "competence_month",
    "ESPEC" = "bed_specialty",
    "CGC_HOSP" = "hospital_cnpj",
    "N_AIH" = "aih_number",
    "IDENT" = "aih_identifier",
    "SEQ_AIH5" = "long_stay_aih_sequence",

    # --- Patient Data ---
    "CEP" = "patient_zip_code",
    "MUNIC_RES" = "residence_municipality_code",
    "NASC" = "birth_date",
    "SEXO" = "sex",
    "IDADE" = "age",
    "COD_IDADE" = "age_unit_code",
    "INSTRU" = "education_level",
    "NUM_FILHOS" = "number_of_children",
    "RACA_COR" = "race",
    "ETNIA" = "ethnicity",
    "NACIONAL" = "nationality",
    "HOMONIMO" = "homonym_indicator",

    # --- Admission Data ---
    "DT_INTER" = "admission_date",
    "DT_SAIDA" = "discharge_date",
    "DIAS_PERM" = "length_of_stay",
    "DIAR_ACOM" = "companion_days",
    "QT_DIARIAS" = "number_of_days",
    "PROC_SOLIC" = "requested_procedure",
    "PROC_REA" = "performed_procedure",
    "CAR_INT" = "admission_type",
    "MORTE" = "death",
    "COBRANCA" = "discharge_reason",
    "CID_NOTIF" = "notification_cid",
    "CID_ASSO" = "associated_cause_cid",
    "CID_MORTE" = "death_cause_cid",
    "DIAG_PRINC" = "main_diagnosis_cid",
    "DIAG_SECUN" = "secondary_diagnosis_cid",
    # Additional secondary diagnoses
    "DIAGSEC1" = "secondary_diagnosis_1",
    "DIAGSEC2" = "secondary_diagnosis_2",
    "DIAGSEC3" = "secondary_diagnosis_3",
    "DIAGSEC4" = "secondary_diagnosis_4",
    "DIAGSEC5" = "secondary_diagnosis_5",
    "DIAGSEC6" = "secondary_diagnosis_6",
    "DIAGSEC7" = "secondary_diagnosis_7",
    "DIAGSEC8" = "secondary_diagnosis_8",
    "DIAGSEC9" = "secondary_diagnosis_9",
    "TPDISEC1" = "secondary_diagnosis_type_1",
    "TPDISEC2" = "secondary_diagnosis_type_2",
    "TPDISEC3" = "secondary_diagnosis_type_3",
    "TPDISEC4" = "secondary_diagnosis_type_4",
    "TPDISEC5" = "secondary_diagnosis_type_5",
    "TPDISEC6" = "secondary_diagnosis_type_6",
    "TPDISEC7" = "secondary_diagnosis_type_7",
    "TPDISEC8" = "secondary_diagnosis_type_8",
    "TPDISEC9" = "secondary_diagnosis_type_9",

    # --- ICU and Intermediate Care Unit ---
    "UTI_MES_IN" = "icu_days_initial_month",
    "UTI_MES_AN" = "icu_days_previous_month",
    "UTI_MES_AL" = "icu_days_discharge_month",
    "UTI_MES_TO" = "icu_days_total",
    "MARCA_UTI" = "icu_type_used",
    "UTI_INT_IN" = "intermediate_care_days_initial",
    "UTI_INT_AN" = "intermediate_care_days_previous",
    "UTI_INT_AL" = "intermediate_care_days_discharge",
    "UTI_INT_TO" = "intermediate_care_days_total",
    "VAL_UCI" = "intermediate_care_unit_value",
    "MARCA_UCI" = "intermediate_care_unit_type",

    # --- AIH Values ---
    "VAL_SH" = "value_hospital_services",
    "VAL_SP" = "value_professional_services",
    "VAL_TOT" = "value_total_aih",
    "VAL_UTI" = "value_icu",
    "US_TOT" = "value_total_usd",
    "VAL_SADT" = "value_sadt", # Diagnostic and Therapeutic Support Service
    "VAL_RN" = "value_newborn",
    "VAL_ACOMP" = "value_companion",
    "VAL_ORTP" = "value_orthotics_prosthetics",
    "VAL_SANGUE" = "value_blood",
    "VAL_SADTSR" = "value_sadt_no_remuneration",
    "VAL_TRANSP" = "value_transport",
    "VAL_OBSANG" = "value_opme_blood",
    "VAL_PED1AC" = "value_prolonged_stay",
    "VAL_SH_FED" = "value_federal_complement_hs",
    "VAL_SP_FED" = "value_federal_complement_ps",
    "VAL_SH_GES" = "value_manager_complement_hs",
    "VAL_SP_GES" = "value_manager_complement_ps",

    # --- Facility Data ---
    "NATUREZA" = "legal_nature_old",
    "NAT_JUR" = "legal_nature",
    "GESTAO" = "hospital_management_type",
    "MUNIC_MOV" = "facility_municipality_code",
    "CNES" = "cnes_code",
    "CNPJ_MANT" = "maintaining_entity_cnpj",
    "INFEHOSP" = "hospital_infection_status",
    "COMPLEX" = "complexity_level",
    "FINANC" = "financing_type",
    "FAEC_TP" = "faec_financing_subtype",
    "REGCT" = "contract_rule",

    # --- Gynecology and Obstetrics Data ---
    "IND_VDRL" = "vdrl_test_performed",
    "GESTRISCO" = "high_risk_pregnancy",
    "INSC_PN" = "prenatal_registration_number",
    "CONTRACEP1" = "contraceptive_method_1",
    "CONTRACEP2" = "contraceptive_method_2",

    # --- Administrative and Personnel Data ---
    "CBOR" = "patient_cbo_code",
    "CNAER" = "work_accident_cnae_code",
    "VINCPREV" = "social_security_link",
    "GESTOR_COD" = "manager_authorization_reason",
    "GESTOR_TP" = "manager_type",
    "GESTOR_CPF" = "manager_cpf",
    "GESTOR_DT" = "manager_authorization_date",
    "CPF_AUT" = "authorizer_cpf",
    "AUD_JUST" = "auditor_justification_cns",
    "SIS_JUST" = "facility_justification_cns",

    # --- System and Remittance Data ---
    "RUBRICA" = "rubric",
    "NUM_PROC" = "procedure_number",
    "TOT_PT_SP" = "total_points_sp",
    "SEQUENCIA" = "aih_sequence_in_remittance",
    "REMESSA" = "remittance_number"
  )

  # ==========================================================================
  # CATEGORICAL VALUE TRANSLATIONS
  # Decodes codes into human-readable English labels.
  # ==========================================================================

  values <- list(
    "IDENT" = c("1" = "Main", "3" = "Long Stay", "5" = "Continuation"),
    "SEXO" = c("1" = "Male", "3" = "Female", "0" = "Ignored", "9" = "Ignored"),
    "INSTRU" = c(
      "1" = "Illiterate", "2" = "Primary School", "3" = "High School", "4" = "Higher Education", "9" = "Ignored"
    ),
    "MORTE" = c("1" = "Yes", "0" = "No"),
    "CAR_INT" = c(
      "1" = "Elective", "2" = "Urgency", "3" = "Workplace accident",
      "4" = "Commuting accident", "5" = "Other traffic accidents",
      "6" = "Other injuries and poisonings"
    ),
    "VINCPREV" = c(
      "1" = "Self-employed", "2" = "Unemployed", "3" = "Retired", "4" = "Not insured",
      "5" = "Employed", "6" = "Employer", "7" = "Beneficiary", "9" = "Ignored"
    ),
    "GESTOR_TP" = c("E" = "State", "M" = "Municipal"),
    "GESTRISCO" = c("1" = "Yes", "0" = "No"),
    "RACA_COR" = c(
      "01" = "White", "02" = "Black", "03" = "Asian", "04" = "Brown", "05" = "Indigenous", "99" = "No information"
    ),
    "FINANC" = c(
      "1" = "Own Resources", "2" = "Costing", "3" = "Investment",
      "4" = "FAEC", "5" = "Others", "6" = "Medium and High Complexity"
    ),
    "COMPLEX" = c("1" = "Basic Care", "2" = "Medium Complexity", "3" = "High Complexity"),
    "NATUREZA" = c(
      "10" = "Public Administration", "20" = "Business Entities",
      "30" = "Non-profit Entities", "40" = "Individuals",
      "50" = "International Organizations", "99" = "Not applicable"
    )
  )

  return(list(columns = columns, values = values))
}

#' Obtiene el Diccionario de Traduccion para SIH (Espanol)
#'
#' Devuelve una lista que contiene diccionarios para traducir los nombres de las columnas
#' y los valores de las variables categoricas del Sistema de Informaciones Hospitalarias (SIH)
#' al espanol. El objetivo es estandarizar y facilitar el analisis de datos.
#'
#' @return Una lista con dos elementos
#' @keywords internal
#' @noRd
get_translation_dict_es_sih <- function() {

  # ==========================================================================
  # TRADUCCION DE NOMBRES DE COLUMNAS
  # Mapea los nombres originales de SIH a un formato estandarizado en espanol.
  # ==========================================================================

  columns <- c(
    # --- Identificacion de la AIH y Competencia ---
    "UF_ZI" = "uf_gestor",
    "ANO_CMPT" = "ano_competencia",
    "MES_CMPT" = "mes_competencia",
    "ESPEC" = "especialidad_cama",
    "CGC_HOSP" = "cnpj_hospital",
    "N_AIH" = "numero_aih",
    "IDENT" = "identificador_aih",
    "SEQ_AIH5" = "secuencial_aih_larga_estancia",

    # --- Datos del Paciente ---
    "CEP" = "codigo_postal_paciente",
    "MUNIC_RES" = "codigo_municipio_residencia",
    "NASC" = "fecha_nacimiento",
    "SEXO" = "sexo",
    "IDADE" = "edad",
    "COD_IDADE" = "codigo_unidad_edad",
    "INSTRU" = "nivel_instruccion",
    "NUM_FILHOS" = "numero_hijos",
    "RACA_COR" = "raza",
    "ETNIA" = "etnia_indigena",
    "NACIONAL" = "nacionalidad",
    "HOMONIMO" = "homonimo_indicador",

    # --- Datos de la Hospitalizacion ---
    "DT_INTER" = "fecha_hospitalizacion",
    "DT_SAIDA" = "fecha_alta",
    "DIAS_PERM" = "dias_estancia",
    "DIAR_ACOM" = "dias_acompanante",
    "QT_DIARIAS" = "cantidad_diarias",
    "PROC_SOLIC" = "procedimiento_solicitado",
    "PROC_REA" = "procedimiento_realizado",
    "CAR_INT" = "caracter_hospitalizacion",
    "MORTE" = "fallecimiento",
    "COBRANCA" = "motivo_alta_permanencia",
    "CID_NOTIF" = "cid_notificacion",
    "CID_ASSO" = "cid_causa_asociada",
    "CID_MORTE" = "cid_fallecimiento",
    "DIAG_PRINC" = "diagnostico_principal_cid",
    "DIAG_SECUN" = "diagnostico_secundario_cid",
    # Diagnosticos secundarios adicionales
    "DIAGSEC1" = "diagnostico_secundario_1",
    "DIAGSEC2" = "diagnostico_secundario_2",
    "DIAGSEC3" = "diagnostico_secundario_3",
    "DIAGSEC4" = "diagnostico_secundario_4",
    "DIAGSEC5" = "diagnostico_secundario_5",
    "DIAGSEC6" = "diagnostico_secundario_6",
    "DIAGSEC7" = "diagnostico_secundario_7",
    "DIAGSEC8" = "diagnostico_secundario_8",
    "DIAGSEC9" = "diagnostico_secundario_9",
    "TPDISEC1" = "tipo_diagnostico_secundario_1",
    "TPDISEC2" = "tipo_diagnostico_secundario_2",
    "TPDISEC3" = "tipo_diagnostico_secundario_3",
    "TPDISEC4" = "tipo_diagnostico_secundario_4",
    "TPDISEC5" = "tipo_diagnostico_secundario_5",
    "TPDISEC6" = "tipo_diagnostico_secundario_6",
    "TPDISEC7" = "tipo_diagnostico_secundario_7",
    "TPDISEC8" = "tipo_diagnostico_secundario_8",
    "TPDISEC9" = "tipo_diagnostico_secundario_9",

    # --- UCI y Unidad de Cuidados Intermedios ---
    "UTI_MES_IN" = "uci_dias_mes_inicial",
    "UTI_MES_AN" = "uci_dias_mes_anterior",
    "UTI_MES_AL" = "uci_dias_mes_alta",
    "UTI_MES_TO" = "uci_dias_mes_total",
    "MARCA_UTI" = "tipo_uci_utilizada",
    "UTI_INT_IN" = "cuidados_intermedios_dias_inicial",
    "UTI_INT_AN" = "cuidados_intermedios_dias_anterior",
    "UTI_INT_AL" = "cuidados_intermedios_dias_alta",
    "UTI_INT_TO" = "cuidados_intermedios_dias_total",
    "VAL_UCI" = "valor_unidad_cuidados_intermedios",
    "MARCA_UCI" = "tipo_unidad_cuidados_intermedios",

    # --- Valores de la AIH ---
    "VAL_SH" = "valor_servicios_hospitalarios",
    "VAL_SP" = "valor_servicios_profesionales",
    "VAL_TOT" = "valor_total_aih",
    "VAL_UTI" = "valor_uci",
    "US_TOT" = "valor_total_usd",
    "VAL_SADT" = "valor_sadt", # Servicio de Apoyo Diagnostico y Terapeutico
    "VAL_RN" = "valor_recien_nacido",
    "VAL_ACOMP" = "valor_acompanante",
    "VAL_ORTP" = "valor_ortesis_protesis",
    "VAL_SANGUE" = "valor_sangre",
    "VAL_SADTSR" = "valor_sadt_sin_remuneracion",
    "VAL_TRANSP" = "valor_transporte",
    "VAL_OBSANG" = "valor_opme_sangre",
    "VAL_PED1AC" = "valor_estancia_prolongada",
    "VAL_SH_FED" = "valor_complemento_federal_sh",
    "VAL_SP_FED" = "valor_complemento_federal_sp",
    "VAL_SH_GES" = "valor_complemento_gestor_sh",
    "VAL_SP_GES" = "valor_complemento_gestor_sp",

    # --- Datos del Establecimiento ---
    "NATUREZA" = "naturaleza_juridica_antigua",
    "NAT_JUR" = "naturaleza_juridica",
    "GESTAO" = "tipo_gestion_hospital",
    "MUNIC_MOV" = "codigo_municipio_establecimiento",
    "CNES" = "codigo_cnes",
    "CNPJ_MANT" = "cnpj_entidad_mantenedora",
    "INFEHOSP" = "estado_infeccion_hospitalaria",
    "COMPLEX" = "complejidad",
    "FINANC" = "tipo_financiamiento",
    "FAEC_TP" = "subtipo_financiamiento_faec",
    "REGCT" = "regla_contractual",

    # --- Datos de Ginecologia y Obstetricia ---
    "IND_VDRL" = "examen_vdrl_realizado",
    "GESTRISCO" = "embarazo_riesgo",
    "INSC_PN" = "numero_pre_natal",
    "CONTRACEP1" = "anticonceptivo_principal",
    "CONTRACEP2" = "anticonceptivo_secundario",

    # --- Datos Administrativos y de Personal ---
    "CBOR" = "cbo_paciente",
    "CNAER" = "cnae_accidente_trabajo",
    "VINCPREV" = "vinculo_seguridad_social",
    "GESTOR_COD" = "motivo_autorizacion_gestor",
    "GESTOR_TP" = "tipo_gestor",
    "GESTOR_CPF" = "cpf_gestor",
    "GESTOR_DT" = "fecha_autorizacion_gestor",
    "CPF_AUT" = "cpf_autorizador",
    "AUD_JUST" = "justificacion_auditor_cns",
    "SIS_JUST" = "justificacion_establecimiento_cns",

    # --- Datos de Sistema y Remesa ---
    "RUBRICA" = "rubrica",
    "NUM_PROC" = "numero_procedimiento",
    "TOT_PT_SP" = "total_puntos_sp",
    "SEQUENCIA" = "secuencial_aih_en_remesa",
    "REMESSA" = "numero_remesa"
  )

  # ==========================================================================
  # TRADUCCION DE VALORES CATEGORICOS
  # Decodifica los codigos a etiquetas legibles en espanol.
  # ==========================================================================

  values <- list(
    "IDENT" = c("1" = "Principal", "3" = "Larga estancia", "5" = "Continuacion"),
    "SEXO" = c("1" = "Masculino", "3" = "Femenino", "0" = "Ignorado", "9" = "Ignorado"),
    "INSTRU" = c(
      "1" = "Analfabeto", "2" = "Educacion primaria", "3" = "Educacion secundaria", "4" = "Educacion superior", "9" = "Ignorado"
    ),
    "MORTE" = c("1" = "Si", "0" = "No"),
    "CAR_INT" = c(
      "1" = "Electiva", "2" = "Urgencia", "3" = "Accidente en el lugar de trabajo",
      "4" = "Accidente en el trayecto al trabajo", "5" = "Otros accidentes de transito",
      "6" = "Otras lesiones y envenenamientos"
    ),
    "VINCPREV" = c(
      "1" = "Autonomo", "2" = "Desempleado", "3" = "Jubilado", "4" = "No asegurado",
      "5" = "Empleado", "6" = "Empleador", "7" = "Beneficiario", "9" = "Ignorado"
    ),
    "GESTOR_TP" = c("E" = "Estatal", "M" = "Municipal"),
    "GESTRISCO" = c("1" = "Si", "0" = "No"),
    "RACA_COR" = c(
      "01" = "Blanca", "02" = "Negra", "03" = "Asiatica", "04" = "Parda", "05" = "Indigena", "99" = "Sin informacion"
    ),
    "FINANC" = c(
      "1" = "Recursos Propios", "2" = "Costeo", "3" = "Inversion",
      "4" = "FAEC", "5" = "Otros", "6" = "Media y Alta Complejidad"
    ),
    "COMPLEX" = c("1" = "Atencion Basica", "2" = "Media Complejidad", "3" = "Alta Complejidad"),
    "NATUREZA" = c(
      "10" = "Administracion Publica", "20" = "Entidades Empresariales",
      "30" = "Entidades sin Fines de Lucro", "40" = "Personas Fisicas",
      "50" = "Organizaciones Internacionales", "99" = "No aplica"
    )
  )

  return(list(columns = columns, values = values))
}

#' Obtem o Dicionario de Traducao para o SIA (Portugues)
#'
#' Retorna uma lista contendo dicionarios para traduzir os nomes das colunas
#' e os valores das variaveis categoricas do Sistema de Informacoes Ambulatoriais (SIA)
#' para o portugues. O objetivo e padronizar e facilitar a analise dos dados.
#'
#' @return Uma lista com dois elementos
#' @keywords internal
#' @noRd
get_translation_dict_pt_sia <- function() {

  # ==========================================================================
  # TRADUCAO DOS NOMES DAS COLUNAS
  # Mapeia os nomes originais do SIA/PA para um formato padronizado em portugues.
  # ==========================================================================

  columns <- c(
    # --- Identificacao do Procedimento e Competencia ---
    "PA_CODUNI" = "codigo_unidade_saude",
    "PA_GESTAO" = "codigo_gestao",
    "PA_CONDIC" = "condicao_gestao",
    "PA_UFMUN" = "uf_municipio_estabelecimento",
    "PA_REGCT" = "regra_contratual",
    "PA_INCOUT" = "incremento_outros",
    "PA_INCURG" = "incremento_urgencia",
    "PA_MVM" = "data_processamento",
    "PA_CMP" = "data_competencia",
    "PA_PROC_ID" = "procedimento_id",
    "PA_DOCORIG" = "documento_origem",
    "PA_AUTORIZ" = "numero_autorizacao",

    # --- Identificacao do Prestador ---
    "PA_TPUPS" = "tipo_unidade",
    "PA_TIPPRE" = "tipo_prestador",
    "PA_MN_IND" = "estabelecimento_mantido_individual",
    "PA_CNPJCPF" = "cnpj_cpf_estabelecimento",
    "PA_CNPJMNT" = "cnpj_mantenedora",
    "PA_CNPJ_CC" = "cnpj_cessao_credito",
    "PA_NAT_JUR" = "natureza_juridica",
    "PA_INE" = "codigo_ine_equipe",

    # --- Financiamento ---
    "PA_TPFIN" = "tipo_financiamento",
    "PA_SUBFIN" = "subtipo_financiamento",
    "PA_NIVCPL" = "nivel_complexidade",
    "PA_VL_CF" = "valor_complemento_federal",
    "PA_VL_CL" = "valor_complemento_local",
    "PA_VL_INC" = "valor_incremento",

    # --- Profissional e Atendimento ---
    "PA_CNSMED" = "cns_profissional",
    "PA_CBOCOD" = "cbo_profissional",
    "PA_CATEND" = "carater_atendimento",
    "PA_SRV_C" = "codigo_servico_especializado",

    # --- Paciente ---
    "PA_MUNPCN" = "codigo_municipio_paciente",
    "PA_IDADE" = "idade_paciente",
    "IDADEMIN" = "idade_minima_procedimento",
    "IDADEMAX" = "idade_maxima_procedimento",
    "PA_SEXO" = "sexo",
    "PA_RACACOR" = "raca",
    "PA_ETNIA" = "etnia_paciente",
    "PA_FLIDADE" = "flag_compatibilidade_idade",

    # --- Saida e Diagnostico ---
    "PA_MOTSAI" = "motivo_saida",
    "PA_OBITO" = "obito",
    "PA_ENCERR" = "encerramento",
    "PA_PERMAN" = "permanencia",
    "PA_ALTA" = "alta",
    "PA_TRANSF" = "transferencia",
    "PA_CIDPRI" = "cid_principal",
    "PA_CIDSEC" = "cid_secundario",
    "PA_CIDCAS" = "cid_causas_associadas",

    # --- Quantidades e Valores ---
    "PA_QTDPRO" = "quantidade_produzida",
    "PA_QTDAPR" = "quantidade_aprovada",
    "PA_VALPRO" = "valor_produzido",
    "PA_VALAPR" = "valor_aprovado",
    "PA_UFDIF" = "diferenca_uf_residencia",
    "PA_MNDIF" = "diferenca_municipio_residencia",
    "PA_DIF_VAL" = "diferenca_valor",
    "NU_VPA_TOT" = "valor_produzido_total_agregado",
    "NU_PA_TOT" = "valor_aprovado_total_agregado",

    # --- Flags e Indicadores ---
    "PA_INDICA" = "indicador_situacao_producao",
    "PA_CODOCO" = "codigo_ocorrencia",
    "PA_FLQT" = "flag_erro_quantidade",
    "PA_FLER" = "flag_erro_apac"
  )

  # ==========================================================================
  # TRADUCAO DE VALORES CATEGORICOS
  # Decodifica os codigos para rotulos legiveis em portugues.
  # ==========================================================================

  values <- list(
    "PA_TPUPS" = c("36" = "Estabelecimento de Saude"), # Exemplo, necessita de tabela completa
    "PA_TIPPRE" = c("1" = "Publico", "2" = "Privado"), # Exemplo, necessita de tabela completa
    "PA_TPFIN" = c(
      "01" = "Atencao Basica (PAB)", "02" = "Assistencia Farmaceutica",
      "04" = "FAEC", "05" = "Incentivo", "06" = "Media e Alta Complexidade"
    ),
    "PA_NIVCPL" = c("1" = "Atencao Basica", "2" = "Media Complexidade", "3" = "Alta Complexidade"),
    "PA_DOCORIG" = c(
      "P" = "APAC Principal", "S" = "APAC Secundaria", "C" = "BPA Consolidado",
      "I" = "BPA Individualizado", "A" = "RAAS Atencao Domiciliar", "B" = "RAAS Psicossocial"
    ),
    "PA_MOTSAI" = c(
      "11" = "Alta curado", "12" = "Alta melhorado", "14" = "Alta a pedido",
      "15" = "Alta com previsao de retorno", "16" = "Alta por evasao",
      "21" = "Transferencia para outro estabelecimento",
      "41" = "Obito com DO", "42" = "Obito sem DO",
      "51" = "Encerramento administrativo"
    ),
    "PA_CATEND" = c(
      "01" = "Eletivo", "02" = "Urgencia", "03" = "Acidente no local de trabalho",
      "04" = "Acidente no trajeto para o trabalho", "05" = "Outros acidentes de transito",
      "06" = "Outras lesoes/envenenamentos"
    ),
    "PA_FLIDADE" = c(
      "0" = "Idade nao exigida", "1" = "Idade compativel",
      "2" = "Idade fora da faixa", "3" = "Idade inexistente"
    ),
    "PA_SEXO" = c("M" = "Masculino", "F" = "Femenino", "I" = "Ignorado"),
    "PA_RACACOR" = c(
      "01" = "Branca", "02" = "Preta", "03" = "Parda",
      "04" = "Amarela", "05" = "Indigena", "99" = "Sem informacao"
    ),
    "PA_INDICA" = c("0" = "Nao aprovado", "5" = "Aprovado total", "6" = "Aprovado parcial"),
    "PA_NAT_JUR" = c(
      "1000" = "Administracao Publica", "2000" = "Entidades Empresariais",
      "3000" = "Entidades sem Fins Lucrativos", "4000" = "Pessoas Fisicas",
      "5000" = "Organizacoes Internacionais"
    ),
    # --- Variaveis Sim/Nao (padrao 1, 0) ---
    "SIM_NAO_FLAG" = c("1" = "Sim", "0" = "Nao"),
    "PA_OBITO" = c("1" = "Sim", "0" = "Nao"),
    "PA_ENCERR" = c("1" = "Sim", "0" = "Nao"),
    "PA_PERMAN" = c("1" = "Sim", "0" = "Nao"),
    "PA_ALTA" = c("1" = "Sim", "0" = "Nao"),
    "PA_TRANSF" = c("1" = "Sim", "0" = "Nao"),
    "PA_UFDIF" = c("1" = "Sim", "0" = "Nao"),
    "PA_MNDIF" = c("1" = "Sim", "0" = "Nao")
  )

  return(list(columns = columns, values = values))
}

#' Get the Translation Dictionary for SIA (English)
#'
#' Returns a list containing dictionaries to translate the column names
#' and categorical variable values of the Outpatient Information System (SIA)
#' into English. The goal is to standardize and facilitate data analysis.
#'
#' @return A list with two elements
#' @keywords internal
#' @noRd
get_translation_dict_en_sia <- function() {

  # ==========================================================================
  # COLUMN NAME TRANSLATIONS
  # Maps original SIA/PA names to a standardized English snake_case format.
  # ==========================================================================

  columns <- c(
    # --- Procedure and Competence Period Identification ---
    "PA_CODUNI" = "health_unit_code",
    "PA_GESTAO" = "management_code",
    "PA_CONDIC" = "management_condition",
    "PA_UFMUN" = "facility_uf_municipality",
    "PA_REGCT" = "contract_rule",
    "PA_INCOUT" = "other_increment",
    "PA_INCURG" = "urgency_increment",
    "PA_MVM" = "processing_date",
    "PA_CMP" = "competence_date",
    "PA_PROC_ID" = "procedure_id",
    "PA_DOCORIG" = "origin_document",
    "PA_AUTORIZ" = "authorization_number",

    # --- Provider Identification ---
    "PA_TPUPS" = "unit_type",
    "PA_TIPPRE" = "provider_type",
    "PA_MN_IND" = "maintained_by_individual_facility",
    "PA_CNPJCPF" = "facility_cnpj_cpf",
    "PA_CNPJMNT" = "maintaining_entity_cnpj",
    "PA_CNPJ_CC" = "credit_assignment_cnpj",
    "PA_NAT_JUR" = "legal_nature",
    "PA_INE" = "team_ine_code",

    # --- Financing ---
    "PA_TPFIN" = "financing_type",
    "PA_SUBFIN" = "financing_subtype",
    "PA_NIVCPL" = "complexity_level",
    "PA_VL_CF" = "federal_complement_value",
    "PA_VL_CL" = "local_complement_value",
    "PA_VL_INC" = "increment_value",

    # --- Professional and Care ---
    "PA_CNSMED" = "professional_cns",
    "PA_CBOCOD" = "professional_cbo_code",
    "PA_CATEND" = "care_character",
    "PA_SRV_C" = "specialized_service_code",

    # --- Patient ---
    "PA_MUNPCN" = "patient_municipality_code",
    "PA_IDADE" = "patient_age",
    "IDADEMIN" = "procedure_min_age",
    "IDADEMAX" = "procedure_max_age",
    "PA_SEXO" = "patient_sex",
    "PA_RACACOR" = "patient_race_color",
    "PA_ETNIA" = "patient_ethnicity",
    "PA_FLIDADE" = "age_compatibility_flag",

    # --- Discharge and Diagnosis ---
    "PA_MOTSAI" = "discharge_reason",
    "PA_OBITO" = "death",
    "PA_ENCERR" = "closure",
    "PA_PERMAN" = "permanence",
    "PA_ALTA" = "discharge",
    "PA_TRANSF" = "transfer",
    "PA_CIDPRI" = "main_diagnosis_cid",
    "PA_CIDSEC" = "secondary_diagnosis_cid",
    "PA_CIDCAS" = "associated_causes_cid",

    # --- Quantities and Values ---
    "PA_QTDPRO" = "quantity_produced",
    "PA_QTDAPR" = "quantity_approved",
    "PA_VALPRO" = "value_produced",
    "PA_VALAPR" = "value_approved",
    "PA_UFDIF" = "residence_uf_difference",
    "PA_MNDIF" = "residence_municipality_difference",
    "PA_DIF_VAL" = "value_difference",
    "NU_VPA_TOT" = "total_produced_value_agg",
    "NU_PA_TOT" = "total_approved_value_agg",

    # --- Flags and Indicators ---
    "PA_INDICA" = "production_status_indicator",
    "PA_CODOCO" = "occurrence_code",
    "PA_FLQT" = "quantity_error_flag",
    "PA_FLER" = "apac_error_flag"
  )

  # ==========================================================================
  # CATEGORICAL VALUE TRANSLATIONS
  # Decodes codes into human-readable English labels.
  # ==========================================================================

  values <- list(
    "PA_TPUPS" = c("36" = "Health Facility"), # Example, needs full table
    "PA_TIPPRE" = c("1" = "Public", "2" = "Private"), # Example, needs full table
    "PA_TPFIN" = c(
      "01" = "Basic Care (PAB)", "02" = "Pharmaceutical Assistance",
      "04" = "FAEC", "05" = "Incentive", "06" = "Medium and High Complexity"
    ),
    "PA_NIVCPL" = c("1" = "Basic Care", "2" = "Medium Complexity", "3" = "High Complexity"),
    "PA_DOCORIG" = c(
      "P" = "Main APAC", "S" = "Secondary APAC", "C" = "Consolidated BPA",
      "I" = "Individualized BPA", "A" = "Home Care RAAS", "B" = "Psychosocial RAAS"
    ),
    "PA_MOTSAI" = c(
      "11" = "Discharge (cured)", "12" = "Discharge (improved)", "14" = "Discharge by request",
      "15" = "Discharge with return planned", "16" = "Discharge by evasion",
      "21" = "Transfer to another facility",
      "41" = "Death with death certificate", "42" = "Death without death certificate",
      "51" = "Administrative closure"
    ),
    "PA_CATEND" = c(
      "01" = "Elective", "02" = "Urgency", "03" = "Workplace accident",
      "04" = "Commuting accident", "05" = "Other traffic accidents",
      "06" = "Other injuries/poisonings"
    ),
    "PA_FLIDADE" = c(
      "0" = "Age not required", "1" = "Compatible age",
      "2" = "Age out of range", "3" = "Non-existent age"
    ),
    "PA_SEXO" = c("M" = "Male", "F" = "Female", "I" = "Ignored"),
    "PA_RACACOR" = c(
      "01" = "White", "02" = "Black", "03" = "Brown",
      "04" = "Asian", "05" = "Indigenous", "99" = "No information"
    ),
    "PA_INDICA" = c("0" = "Not approved", "5" = "Fully approved", "6" = "Partially approved"),
    "PA_NAT_JUR" = c(
      "1000" = "Public Administration", "2000" = "Business Entities",
      "3000" = "Non-profit Entities", "4000" = "Individuals",
      "5000" = "International Organizations"
    ),
    # --- Yes/No Flags (1, 0) ---
    "YES_NO_FLAG" = c("1" = "Yes", "0" = "No"),
    "PA_OBITO" = c("1" = "Yes", "0" = "No"),
    "PA_ENCERR" = c("1" = "Yes", "0" = "No"),
    "PA_PERMAN" = c("1" = "Yes", "0" = "No"),
    "PA_ALTA" = c("1" = "Yes", "0" = "No"),
    "PA_TRANSF" = c("1" = "Yes", "0" = "No"),
    "PA_UFDIF" = c("1" = "Yes", "0" = "No"),
    "PA_MNDIF" = c("1" = "Yes", "0" = "No")
  )

  return(list(columns = columns, values = values))
}

#' Obtiene el Diccionario de Traduccion para SIA (Espanol)
#'
#' Devuelve una lista que contiene diccionarios para traducir los nombres de las columnas
#' y los valores de las variables categoricas del Sistema de Informaciones Ambulatorias (SIA)
#' al espanol. El objetivo es estandarizar y facilitar el analisis de datos.
#'
#' @return Una lista con dos elementos
#' @keywords internal
#' @noRd
get_translation_dict_es_sia <- function() {

  # ==========================================================================
  # TRADUCCION DE NOMBRES DE COLUMNAS
  # Mapea los nombres originales de SIA/PA a un formato estandarizado en espanol.
  # ==========================================================================

  columns <- c(
    # --- Identificacion del Procedimiento y Competencia ---
    "PA_CODUNI" = "codigo_unidad_salud",
    "PA_GESTAO" = "codigo_gestion",
    "PA_CONDIC" = "condicion_gestion",
    "PA_UFMUN" = "uf_municipio_establecimiento",
    "PA_REGCT" = "regla_contractual",
    "PA_INCOUT" = "incremento_otros",
    "PA_INCURG" = "incremento_urgencia",
    "PA_MVM" = "fecha_procesamiento",
    "PA_CMP" = "fecha_competencia",
    "PA_PROC_ID" = "procedimiento_id",
    "PA_DOCORIG" = "documento_origen",
    "PA_AUTORIZ" = "numero_autorizacion",

    # --- Identificacion del Proveedor ---
    "PA_TPUPS" = "tipo_unidad",
    "PA_TIPPRE" = "tipo_proveedor",
    "PA_MN_IND" = "establecimiento_mantenido_individual",
    "PA_CNPJCPF" = "cnpj_cpf_establecimiento",
    "PA_CNPJMNT" = "cnpj_entidad_mantenedora",
    "PA_CNPJ_CC" = "cnpj_cesion_credito",
    "PA_NAT_JUR" = "naturaleza_juridica",
    "PA_INE" = "codigo_ine_equipo",

    # --- Financiamiento ---
    "PA_TPFIN" = "tipo_financiamiento",
    "PA_SUBFIN" = "subtipo_financiamiento",
    "PA_NIVCPL" = "nivel_complejidad",
    "PA_VL_CF" = "valor_complemento_federal",
    "PA_VL_CL" = "valor_complemento_local",
    "PA_VL_INC" = "valor_incremento",

    # --- Profesional y Atencion ---
    "PA_CNSMED" = "cns_profesional",
    "PA_CBOCOD" = "cbo_profesional",
    "PA_CATEND" = "caracter_atencion",
    "PA_SRV_C" = "codigo_servicio_especializado",

    # --- Paciente ---
    "PA_MUNPCN" = "codigo_municipio_paciente",
    "PA_IDADE" = "edad_paciente",
    "IDADEMIN" = "edad_minima_procedimiento",
    "IDADEMAX" = "edad_maxima_procedimiento",
    "PA_SEXO" = "sexo",
    "PA_RACACOR" = "raza_color_paciente",
    "PA_ETNIA" = "etnia_paciente",
    "PA_FLIDADE" = "flag_compatibilidad_edad",

    # --- Alta y Diagnostico ---
    "PA_MOTSAI" = "motivo_alta",
    "PA_OBITO" = "fallecimiento",
    "PA_ENCERR" = "cierre",
    "PA_PERMAN" = "permanencia",
    "PA_ALTA" = "alta",
    "PA_TRANSF" = "transferencia",
    "PA_CIDPRI" = "cid_principal",
    "PA_CIDSEC" = "cid_secundario",
    "PA_CIDCAS" = "cid_causas_asociadas",

    # --- Cantidades y Valores ---
    "PA_QTDPRO" = "cantidad_producida",
    "PA_QTDAPR" = "cantidad_aprobada",
    "PA_VALPRO" = "valor_producido",
    "PA_VALAPR" = "valor_aprobado",
    "PA_UFDIF" = "diferencia_uf_residencia",
    "PA_MNDIF" = "diferencia_municipio_residencia",
    "PA_DIF_VAL" = "diferencia_valor",
    "NU_VPA_TOT" = "valor_producido_total_agregado",
    "NU_PA_TOT" = "valor_aprobado_total_agregado",

    # --- Flags e Indicadores ---
    "PA_INDICA" = "indicador_estado_produccion",
    "PA_CODOCO" = "codigo_ocurrencia",
    "PA_FLQT" = "flag_error_cantidad",
    "PA_FLER" = "flag_error_apac"
  )

  # ==========================================================================
  # TRADUCCION DE VALORES CATEGORICOS
  # Decodifica los codigos a etiquetas legibles en espanol.
  # ==========================================================================

  values <- list(
    "PA_TPUPS" = c("36" = "Establecimiento de Salud"), # Ejemplo, necesita tabla completa
    "PA_TIPPRE" = c("1" = "Publico", "2" = "Privado"), # Ejemplo, necesita tabla completa
    "PA_TPFIN" = c(
      "01" = "Atencion Basica (PAB)", "02" = "Asistencia Farmaceutica",
      "04" = "FAEC", "05" = "Incentivo", "06" = "Media y Alta Complejidad"
    ),
    "PA_NIVCPL" = c("1" = "Atencion Basica", "2" = "Media Complejidad", "3" = "Alta Complejidad"),
    "PA_DOCORIG" = c(
      "P" = "APAC Principal", "S" = "APAC Secundaria", "C" = "BPA Consolidado",
      "I" = "BPA Individualizado", "A" = "RAAS Atencion Domiciliaria", "B" = "RAAS Psicosocial"
    ),
    "PA_MOTSAI" = c(
      "11" = "Alta por curacion", "12" = "Alta por mejoria", "14" = "Alta a peticion",
      "15" = "Alta con prevision de retorno", "16" = "Alta por evasion",
      "21" = "Transferencia a otro establecimiento",
      "41" = "Fallecimiento con certificado", "42" = "Fallecimiento sin certificado",
      "51" = "Cierre administrativo"
    ),
    "PA_CATEND" = c(
      "01" = "Electivo", "02" = "Urgencia", "03" = "Accidente en el lugar de trabajo",
      "04" = "Accidente en el trayecto al trabajo", "05" = "Otros accidentes de transito",
      "06" = "Otras lesiones/envenenamientos"
    ),
    "PA_FLIDADE" = c(
      "0" = "Edad no requerida", "1" = "Edad compatible",
      "2" = "Edad fuera de rango", "3" = "Edad inexistente"
    ),
    "PA_SEXO" = c("M" = "Masculino", "F" = "Femenino", "I" = "Ignorado"),
    "PA_RACACOR" = c(
      "01" = "Blanca", "02" = "Negra", "03" = "Parda",
      "04" = "Asiatica", "05" = "Indigena", "99" = "Sin informacion"
    ),
    "PA_INDICA" = c("0" = "No aprobado", "5" = "Aprobado total", "6" = "Aprobado parcial"),
    "PA_NAT_JUR" = c(
      "1000" = "Administracion Publica", "2000" = "Entidades Empresariales",
      "3000" = "Entidades sin Fines de Lucro", "4000" = "Personas Fisicas",
      "5000" = "Organizaciones Internacionales"
    ),
    # --- Flags Si/No (1, 0) ---
    "SI_NO_FLAG" = c("1" = "Si", "0" = "No"),
    "PA_OBITO" = c("1" = "Si", "0" = "No"),
    "PA_ENCERR" = c("1" = "Si", "0" = "No"),
    "PA_PERMAN" = c("1" = "Si", "0" = "No"),
    "PA_ALTA" = c("1" = "Si", "0" = "No"),
    "PA_TRANSF" = c("1" = "Si", "0" = "No"),
    "PA_UFDIF" = c("1" = "Si", "0" = "No"),
    "PA_MNDIF" = c("1" = "Si", "0" = "No")
  )

  return(list(columns = columns, values = values))
}


#' Obtem o Dicionario de Traducao para o CNES (Portugues)
#'
#' @description
#' Retorna uma lista contendo dicionarios para traduzir os nomes das colunas
#' e os valores das variaveis categoricas do Cadastro Nacional de Estabelecimentos
#' de Saude (CNES) para o portugues. O objetivo e padronizar e facilitar a
#' analise dos dados.
#'
#' @return Uma lista com dois elementos:
#'   - `columns`: um vetor nomeado para traduzir os nomes das colunas.
#'   - `values`: uma lista de vetores nomeados para decodificar valores categoricos.
#'
#' @keywords internal
#' @noRd
get_translation_dict_pt_cnes <- function() {

  # ==========================================================================
  # TRADUCAO DOS NOMES DAS COLUNAS (CNES)
  # Mapeia os nomes originais do CNES para um formato padronizado em portugues.
  # ==========================================================================

  columns <- c(
    # --- Identificacao do Estabelecimento e Localizacao ---
    "CNES" = "cnes",
    "CODUFMUN" = "codigo_municipio",
    "REGSAUDE" = "regiao_saude",
    "MICR_REG" = "micro_regiao_saude",
    "DISTRSAN" = "distrito_sanitario",
    "DISTRADM" = "distrito_administrativo",
    "COD_CEP" = "cep_paciente",

    # --- Identificacao Juridica e Administrativa ---
    "CPF_CNPJ" = "cpf_cnpj",
    "PF_PJ" = "pessoa_fisica_juridica",
    "NIV_DEP" = "grau_dependencia",
    "CNPJ_MAN" = "cnpj_mantenedora",
    "ESFERA_A" = "esfera_administrativa",
    "VINC_SUS" = "vinculo_sus",
    "RETENCAO" = "retencao_tributos",
    "ATIVIDAD" = "atividade_ensino_pesquisa",
    "NATUREZA" = "natureza_organizacao",
    "NAT_JUR" = "natureza_juridica",
    "CLIENTEL" = "fluxo_clientela",
    "TP_UNID" = "tipo_unidade",
    "TURNO_AT" = "turno_atendimento",
    "NIV_HIER" = "nivel_hierarquia",
    "TP_PREST" = "tipo_prestador",
    "TPGESTAO" = "tipo_gestao",

    # --- Contratos e Alvaras ---
    "CONTRATM" = "contrato_municipal",
    "DT_PUBLM" = "data_publicacao_contrato_municipal",
    "CONTRATE" = "contrato_estadual",
    "DT_PUBLE" = "data_publicacao_contrato_estadual",
    "ALVARA" = "numero_alvara",
    "DT_EXPED" = "data_expedicao_alvara",
    "ORGEXPED" = "orgao_expedidor_alvara",

    # --- Avaliacoes e Certificacoes ---
    "AV_ACRED" = "avaliacao_acreditacao_hospitalar",
    "CLASAVAL" = "classificacao_acreditacao",
    "DT_ACRED" = "data_acreditacao",
    "AV_PNASS" = "avaliacao_pnass",
    "DT_PNASS" = "data_avaliacao_pnass",

    # --- Gestao de Programas e Niveis de Atencao ---
    "GESPRG1E" = "gestao_atencao_basica_estadual",
    "GESPRG1M" = "gestao_atencao_basica_municipal",
    "GESPRG2E" = "gestao_media_complexidade_amb_estadual",
    "GESPRG2M" = "gestao_media_complexidade_amb_municipal",
    "GESPRG3E" = "gestao_programa_3_estadual", # Nome generico conforme layout
    "GESPRG3M" = "gestao_programa_3_municipal",
    "GESPRG4E" = "gestao_alta_complexidade_amb_estadual",
    "GESPRG4M" = "gestao_alta_complexidade_amb_municipal",
    "GESPRG5E" = "gestao_media_complexidade_hosp_estadual",
    "GESPRG5M" = "gestao_media_complexidade_hosp_municipal",
    "GESPRG6E" = "gestao_alta_complexidade_hosp_estadual",
    "GESPRG6M" = "gestao_alta_complexidade_hosp_municipal",
    "NIVATE_A" = "possui_nivel_atencao_ambulatorial",
    "NIVATE_H" = "possui_nivel_atencao_hospitalar",

    # --- Leitos e Instalacoes ---
    "QTLEITP1" = "qtd_leitos_cirurgicos",
    "QTLEITP2" = "qtd_leitos_clinicos",
    "QTLEITP3" = "qtd_leitos_complementares",
    "LEITHOSP" = "possui_leito_hospitalar",
    "URGEMERG" = "possui_urgencia_emergencia",
    "ATENDAMB" = "possui_atendimento_ambulatorial",
    "CENTRCIR" = "possui_centro_cirurgico",
    "CENTROBS" = "possui_centro_obstetrico",
    "CENTRNEO" = "possui_unidade_neonatal",
    "ATENDHOS" = "possui_atendimento_hospitalar",

    # --- Servicos de Apoio ---
    "SERAPOIO" = "possui_servico_apoio",
    "SERAP01P" = "servico_apoio_same_proprio",
    "SERAP01T" = "servico_apoio_same_terceirizado",
    "SERAP02P" = "servico_apoio_social_proprio",
    "SERAP02T" = "servico_apoio_social_terceirizado",
    "SERAP03P" = "servico_apoio_farmacia_proprio",
    "SERAP03T" = "servico_apoio_farmacia_terceirizado",
    # ... (e assim por diante para todos os SERAP*)

    # --- Gestao de Residuos ---
    "RES_BIOL" = "coleta_residuo_biologico",
    "RES_QUIM" = "coleta_residuo_quimico",
    "RES_RADI" = "coleta_rejeito_radioativo",
    "RES_COMU" = "coleta_residuo_comum",
    "COLETRES" = "possui_coleta_residuos",

    # --- Comissoes ---
    "COMISSAO" = "possui_comissao",
    "COMISS01" = "comissao_etica_medica",
    "COMISS02" = "comissao_etica_enfermagem",
    "COMISS03" = "comissao_farmacia_terapeutica",
    "COMISS04" = "comissao_controle_infeccao",
    "COMISS05" = "comissao_apropriacao_custos",
    "COMISS06" = "comissao_cipa",
    "COMISS07" = "comissao_revisao_prontuarios",
    "COMISS08" = "comissao_revisao_documentacao",
    "COMISS09" = "comissao_analise_obitos_biopsias",
    "COMISS10" = "comissao_investigacao_epidemiologica",
    "COMISS11" = "comissao_notificacao_doencas",
    "COMISS12" = "comissao_controle_zoonoses",

    # --- Atendimentos por Convenio ---
    "ATEND_PR" = "possui_atendimento_prestado",
    "AP01CV01" = "atende_internacao_sus",
    "AP01CV02" = "atende_internacao_particular",
    # ... (e assim por diante para todos os AP*CV*)

    # --- Outros ---
    "DT_ATUAL" = "data_atualizacao",
    "COMPETEN" = "competencia"
  )

  # ==========================================================================
  # TRADUCAO DE VALORES CATEGORICOS (CNES)
  # Decodifica os codigos para rotulos legiveis em portugues.
  # ==========================================================================

  values <- list(
    "PF_PJ" = c("1" = "Pessoa Fisica", "3" = "Pessoa Juridica"),
    "NIV_DEP" = c("1" = "Individual", "3" = "Mantida"),
    "VINC_SUS" = c("1" = "Sim", "0" = "Nao"),
    "TPGESTAO" = c("M" = "Municipal", "E" = "Estadual", "D" = "Dupla"),
    "ESFERA_A" = c(
      "01" = "Federal", "02" = "Estadual", "03" = "Municipal", "04" = "Privada"
    ),
    "ATIVIDAD" = c(
      "01" = "Ensino", "02" = "Pesquisa", "03" = "Ensino e Pesquisa", "04" = "Nenhuma"
    ),
    "NATUREZA" = c(
      "10" = "Administracao Publica", "20" = "Entidades Empresariais",
      "30" = "Entidades sem Fins Lucrativos", "40" = "Pessoas Fisicas",
      "50" = "Organizacoes Internacionais"
    ),
    "TP_PREST" = c("40" = "Publico", "50" = "Privado"),
    "TERCEIRO" = c("1" = "Sim", "0" = "Nao", "2" = "Nao"), # Mapeando 2 para Nao
    
    # --- Flags Sim/Nao (padrao 1 para Sim, 0 para Nao) ---
    "SIM_NAO_FLAG" = c("1" = "Sim", "0" = "Nao")
  )

  # Lista de todas as colunas que usam o padrao Sim/Nao
  sim_nao_vars <- c(
    "AV_ACRED", "AV_PNASS", "GESPRG1E", "GESPRG1M", "GESPRG2E", "GESPRG2M",
    "GESPRG3E", "GESPRG3M", "GESPRG4E", "GESPRG4M", "GESPRG5E", "GESPRG5M",
    "GESPRG6E", "GESPRG6M", "NIVATE_A", "NIVATE_H", "LEITHOSP", "URGEMERG",
    "ATENDAMB", "CENTRCIR", "CENTROBS", "CENTRNEO", "ATENDHOS", "SERAPOIO",
    "RES_BIOL", "RES_QUIM", "RES_RADI", "RES_COMU", "COLETRES", "COMISSAO",
    "COMISS01", "COMISS02", "COMISS03", "COMISS04", "COMISS05", "COMISS06",
    "COMISS07", "COMISS08", "COMISS09", "COMISS10", "COMISS11", "COMISS12",
    "ATEND_PR", "AP01CV01", "AP01CV02", "AP01CV03", "AP01CV04", "AP01CV05",
    "AP01CV06", "AP01CV07", "AP02CV01", "AP02CV02", "AP02CV03", "AP02CV04",
    "AP02CV05", "AP02CV06", "AP02CV07", "AP03CV01", "AP03CV02", "AP03CV03",
    "AP03CV04", "AP03CV05", "AP03CV06", "AP03CV07", "AP04CV01", "AP04CV02",
    "AP04CV03", "AP04CV04", "AP04CV05", "AP04CV06", "AP04CV07", "AP05CV01",
    "AP05CV02", "AP05CV03", "AP05CV04", "AP05CV05", "AP05CV06", "AP05CV07",
    "AP06CV01", "AP06CV02", "AP06CV03", "AP06CV04", "AP06CV05", "AP06CV06",
    "AP06CV07", "AP07CV01", "AP07CV02", "AP07CV03", "AP07CV04", "AP07CV05",
    "AP07CV06", "AP07CV07",
    "SERAP01P", "SERAP01T", "SERAP02P", "SERAP02T", "SERAP03P", "SERAP03T"
    # Adicionar outros SERAP* e demais flags aqui
  )

  for (var in sim_nao_vars) {
    if (var %in% names(columns)) { # Verifica se a coluna existe no mapeamento
       values[[var]] <- values[["SIM_NAO_FLAG"]]
    }
  }
  values[["SIM_NAO_FLAG"]] <- NULL # Remove o template

  return(list(columns = columns, values = values))
}

#' Get the Translation Dictionary for CNES (English)
#'
#' @description
#' Returns a list containing dictionaries to translate the column names
#' and categorical variable values of the National Registry of Health Facilities (CNES)
#' into English. The goal is to standardize and facilitate data analysis.
#'
#' @return A list with two elements:
#'   - `columns`: a named vector to translate column names.
#'   - `values`: a list of named vectors to decode categorical values.
#'
#' @keywords internal
#' @noRd
get_translation_dict_en_cnes <- function() {

  # ==========================================================================
  # COLUMN NAME TRANSLATIONS (CNES)
  # Maps original CNES names to a standardized English snake_case format.
  # ==========================================================================

  columns <- c(
    # --- Facility Identification and Location ---
    "CNES" = "cnes_id",
    "CODUFMUN" = "municipality_code",
    "REGSAUDE" = "health_region",
    "MICR_REG" = "health_micro_region",
    "DISTRSAN" = "sanitary_district",
    "DISTRADM" = "administrative_district",
    "COD_CEP" = "zip_code",

    # --- Legal and Administrative Identification ---
    "CPF_CNPJ" = "cpf_cnpj",
    "PF_PJ" = "person_type", # Physical or Juridical Person
    "NIV_DEP" = "dependency_level",
    "CNPJ_MAN" = "maintaining_entity_cnpj",
    "ESFERA_A" = "administrative_sphere",
    "VINC_SUS" = "sus_link",
    "RETENCAO" = "tax_retention",
    "ATIVIDAD" = "teaching_research_activity",
    "NATUREZA" = "organization_nature",
    "NAT_JUR" = "legal_nature",
    "CLIENTEL" = "clientele_flow",
    "TP_UNID" = "unit_type",
    "TURNO_AT" = "service_shift",
    "NIV_HIER" = "hierarchy_level",
    "TP_PREST" = "provider_type",
    "TPGESTAO" = "management_type",

    # --- Contracts and Permits ---
    "CONTRATM" = "municipal_contract",
    "DT_PUBLM" = "municipal_contract_publication_date",
    "CONTRATE" = "state_contract",
    "DT_PUBLE" = "state_contract_publication_date",
    "ALVARA" = "permit_number",
    "DT_EXPED" = "permit_issue_date",
    "ORGEXPED" = "permit_issuing_body",

    # --- Evaluations and Certifications ---
    "AV_ACRED" = "hospital_accreditation_evaluation",
    "CLASAVAL" = "accreditation_classification",
    "DT_ACRED" = "accreditation_date",
    "AV_PNASS" = "pnass_evaluation",
    "DT_PNASS" = "pnass_evaluation_date",

    # --- Program Management and Care Levels ---
    "GESPRG1E" = "management_basic_care_state",
    "GESPRG1M" = "management_basic_care_municipal",
    "GESPRG2E" = "management_medium_complexity_amb_state",
    "GESPRG2M" = "management_medium_complexity_amb_municipal",
    "GESPRG3E" = "management_program_3_state",
    "GESPRG3M" = "management_program_3_municipal",
    "GESPRG4E" = "management_high_complexity_amb_state",
    "GESPRG4M" = "management_high_complexity_amb_municipal",
    "GESPRG5E" = "management_medium_complexity_hosp_state",
    "GESPRG5M" = "management_medium_complexity_hosp_municipal",
    "GESPRG6E" = "management_high_complexity_hosp_state",
    "GESPRG6M" = "management_high_complexity_hosp_municipal",
    "NIVATE_A" = "has_ambulatory_care_level",
    "NIVATE_H" = "has_hospital_care_level",

    # --- Beds and Facilities ---
    "QTLEITP1" = "qty_beds_surgical",
    "QTLEITP2" = "qty_beds_clinical",
    "QTLEITP3" = "qty_beds_complementary",
    "LEITHOSP" = "has_hospital_bed",
    "URGEMERG" = "has_emergency_unit",
    "ATENDAMB" = "has_ambulatory_care",
    "CENTRCIR" = "has_surgical_center",
    "CENTROBS" = "has_obstetric_center",
    "CENTRNEO" = "has_neonatal_unit",
    "ATENDHOS" = "has_hospital_care",

    # --- Support Services ---
    "SERAPOIO" = "has_support_service",
    "SERAP01P" = "support_service_medical_records_own",
    "SERAP01T" = "support_service_medical_records_outsourced",
    "SERAP02P" = "support_service_social_work_own",
    "SERAP02T" = "support_service_social_work_outsourced",
    "SERAP03P" = "support_service_pharmacy_own",
    "SERAP03T" = "support_service_pharmacy_outsourced",
    # ... and so on for all SERAP* fields

    # --- Waste Management ---
    "RES_BIOL" = "collects_biological_waste",
    "RES_QUIM" = "collects_chemical_waste",
    "RES_RADI" = "collects_radioactive_waste",
    "RES_COMU" = "collects_common_waste",
    "COLETRES" = "has_waste_collection",

    # --- Committees ---
    "COMISSAO" = "has_committee",
    "COMISS01" = "committee_medical_ethics",
    "COMISS02" = "committee_nursing_ethics",
    "COMISS03" = "committee_pharmacy_therapeutics",
    "COMISS04" = "committee_hospital_infection_control",
    "COMISS05" = "committee_cost_appropriation",
    "COMISS06" = "committee_cipa", # Internal Commission for Accident Prevention
    "COMISS07" = "committee_medical_record_review",
    "COMISS08" = "committee_medical_documentation_review",
    "COMISS09" = "committee_death_biopsy_analysis",
    "COMISS10" = "committee_epidemiological_investigation",
    "COMISS11" = "committee_disease_notification",
    "COMISS12" = "committee_zoonoses_vectors_control",

    # --- Services by Agreement ---
    "ATEND_PR" = "has_service_provided",
    "AP01CV01" = "provides_hospitalization_sus",
    "AP01CV02" = "provides_hospitalization_private",
    # ... and so on for all AP*CV* fields

    # --- Other ---
    "DT_ATUAL" = "update_date",
    "COMPETEN" = "competence"
  )

  # ==========================================================================
  # CATEGORICAL VALUE TRANSLATIONS (CNES)
  # Decodes codes into human-readable English labels.
  # ==========================================================================

  values <- list(
    "PF_PJ" = c("1" = "Individual", "3" = "Juridical Entity"),
    "NIV_DEP" = c("1" = "Individual", "3" = "Maintained"),
    "VINC_SUS" = c("1" = "Yes", "0" = "No"),
    "TPGESTAO" = c("M" = "Municipal", "E" = "State", "D" = "Dual"),
    "ESFERA_A" = c(
      "01" = "Federal", "02" = "State", "03" = "Municipal", "04" = "Private"
    ),
    "ATIVIDAD" = c(
      "01" = "Teaching", "02" = "Research", "03" = "Teaching and Research", "04" = "None"
    ),
    "NATUREZA" = c(
      "10" = "Public Administration", "20" = "Business Entities",
      "30" = "Non-profit Entities", "40" = "Individuals",
      "50" = "International Organizations"
    ),
    "TP_PREST" = c("40" = "Public", "50" = "Private"),
    "TERCEIRO" = c("1" = "Yes", "0" = "No", "2" = "No"), # Mapping 2 to No

    # --- Standard Yes/No Flag (1 for Yes, 0 for No) ---
    "YES_NO_FLAG" = c("1" = "Yes", "0" = "No")
  )

  # List of all columns that use the standard Yes/No pattern
  yes_no_vars <- c(
    "AV_ACRED", "AV_PNASS", "GESPRG1E", "GESPRG1M", "GESPRG2E", "GESPRG2M",
    "GESPRG3E", "GESPRG3M", "GESPRG4E", "GESPRG4M", "GESPRG5E", "GESPRG5M",
    "GESPRG6E", "GESPRG6M", "NIVATE_A", "NIVATE_H", "LEITHOSP", "URGEMERG",
    "ATENDAMB", "CENTRCIR", "CENTROBS", "CENTRNEO", "ATENDHOS", "SERAPOIO",
    "COLETRES", "COMISSAO",
    "COMISS01", "COMISS02", "COMISS03", "COMISS04", "COMISS05", "COMISS06",
    "COMISS07", "COMISS08", "COMISS09", "COMISS10", "COMISS11", "COMISS12",
    "ATEND_PR", "AP01CV01", "AP01CV02", "AP01CV03", "AP01CV04", "AP01CV05",
    "AP01CV06", "AP01CV07", "AP02CV01", "AP02CV02", "AP02CV03", "AP02CV04",
    "AP02CV05", "AP02CV06", "AP02CV07", "AP03CV01", "AP03CV02", "AP03CV03",
    "AP03CV04", "AP03CV05", "AP03CV06", "AP03CV07", "AP04CV01", "AP04CV02",
    "AP04CV03", "AP04CV04", "AP04CV05", "AP04CV06", "AP04CV07", "AP05CV01",
    "AP05CV02", "AP05CV03", "AP05CV04", "AP05CV05", "AP05CV06", "AP05CV07",
    "AP06CV01", "AP06CV02", "AP06CV03", "AP06CV04", "AP06CV05", "AP06CV06",
    "AP06CV07", "AP07CV01", "AP07CV02", "AP07CV03", "AP07CV04", "AP07CV05",
    "AP07CV06", "AP07CV07",
    "SERAP01P", "SERAP01T", "SERAP02P", "SERAP02T", "SERAP03P", "SERAP03T"
    # Add other SERAP* and other flags here
  )

  for (var in yes_no_vars) {
    # Check if the column name exists in the main column mapping
    if (var %in% names(columns)) {
       values[[var]] <- values[["YES_NO_FLAG"]]
    }
  }
  values[["YES_NO_FLAG"]] <- NULL # Remove the template

  return(list(columns = columns, values = values))
}

#' Obtiene el Diccionario de Traduccion para CNES (Espanol)
#'
#' @description
#' Devuelve una lista que contiene diccionarios para traducir los nombres de las columnas
#' y los valores de las variables categoricas del Cadastro Nacional de Estabelecimentos
#' de Saude (CNES) al espanol. El objetivo es estandarizar y facilitar el analisis de datos.
#'
#' @return Una lista con dos elementos:
#'   - `columns`: un vector nombrado para traducir los nombres de las columnas.
#'   - `values`: una lista de vectores nombrados para decodificar valores categoricos.
#'
#' @keywords internal
#' @noRd
get_translation_dict_es_cnes <- function() {

  # ==========================================================================
  # TRADUCCION DE NOMBRES DE COLUMNAS (CNES)
  # Mapea los nombres originales de CNES a un formato estandarizado en espanol.
  # ==========================================================================

  columns <- c(
    # --- Identificacion del Establecimiento y Ubicacion ---
    "CNES" = "cnes_id",
    "CODUFMUN" = "codigo_municipio",
    "REGSAUDE" = "region_salud",
    "MICR_REG" = "micro_region_salud",
    "DISTRSAN" = "distrito_sanitario",
    "DISTRADM" = "distrito_administrativo",
    "COD_CEP" = "codigo_postal",

    # --- Identificacion Juridica y Administrativa ---
    "CPF_CNPJ" = "cpf_cnpj",
    "PF_PJ" = "tipo_persona", # Persona Fisica o Juridica
    "NIV_DEP" = "grado_dependencia",
    "CNPJ_MAN" = "cnpj_entidad_mantenedora",
    "ESFERA_A" = "esfera_administrativa",
    "VINC_SUS" = "vinculo_sus",
    "RETENCAO" = "retencion_tributos",
    "ATIVIDAD" = "actividad_ensenanza_investigacion",
    "NATUREZA" = "naturaleza_organizacion",
    "NAT_JUR" = "naturaleza_juridica",
    "CLIENTEL" = "flujo_clientela",
    "TP_UNID" = "tipo_unidad",
    "TURNO_AT" = "turno_atencion",
    "NIV_HIER" = "nivel_jerarquia",
    "TP_PREST" = "tipo_proveedor",
    "TPGESTAO" = "tipo_gestion",

    # --- Contratos y Licencias ---
    "CONTRATM" = "contrato_municipal",
    "DT_PUBLM" = "fecha_publicacion_contrato_municipal",
    "CONTRATE" = "contrato_estatal",
    "DT_PUBLE" = "fecha_publicacion_contrato_estatal",
    "ALVARA" = "numero_licencia",
    "DT_EXPED" = "fecha_expedicion_licencia",
    "ORGEXPED" = "organo_expedidor_licencia",

    # --- Evaluaciones y Certificaciones ---
    "AV_ACRED" = "evaluacion_acreditacion_hospitalaria",
    "CLASAVAL" = "clasificacion_acreditacion",
    "DT_ACRED" = "fecha_acreditacion",
    "AV_PNASS" = "evaluacion_pnass",
    "DT_PNASS" = "fecha_evaluacion_pnass",

    # --- Gestion de Programas y Niveles de Atencion ---
    "GESPRG1E" = "gestion_atencion_basica_estatal",
    "GESPRG1M" = "gestion_atencion_basica_municipal",
    "GESPRG2E" = "gestion_media_complejidad_amb_estatal",
    "GESPRG2M" = "gestion_media_complejidad_amb_municipal",
    "GESPRG3E" = "gestion_programa_3_estatal",
    "GESPRG3M" = "gestion_programa_3_municipal",
    "GESPRG4E" = "gestion_alta_complejidad_amb_estatal",
    "GESPRG4M" = "gestion_alta_complejidad_amb_municipal",
    "GESPRG5E" = "gestion_media_complejidad_hosp_estatal",
    "GESPRG5M" = "gestion_media_complejidad_hosp_municipal",
    "GESPRG6E" = "gestion_alta_complejidad_hosp_estatal",
    "GESPRG6M" = "gestion_alta_complejidad_hosp_municipal",
    "NIVATE_A" = "tiene_nivel_atencion_ambulatoria",
    "NIVATE_H" = "tiene_nivel_atencion_hospitalaria",

    # --- Camas e Instalaciones ---
    "QTLEITP1" = "cant_camas_quirurgicas",
    "QTLEITP2" = "cant_camas_clinicas",
    "QTLEITP3" = "cant_camas_complementarias",
    "LEITHOSP" = "tiene_cama_hospitalaria",
    "URGEMERG" = "tiene_unidad_urgencias",
    "ATENDAMB" = "tiene_atencion_ambulatoria",
    "CENTRCIR" = "tiene_centro_quirurgico",
    "CENTROBS" = "tiene_centro_obstetrico",
    "CENTRNEO" = "tiene_unidad_neonatal",
    "ATENDHOS" = "tiene_atencion_hospitalaria",

    # --- Servicios de Apoyo ---
    "SERAPOIO" = "tiene_servicio_apoyo",
    "SERAP01P" = "servicio_apoyo_archivos_medicos_propio",
    "SERAP01T" = "servicio_apoyo_archivos_medicos_tercerizado",
    "SERAP02P" = "servicio_apoyo_social_propio",
    "SERAP02T" = "servicio_apoyo_social_tercerizado",
    "SERAP03P" = "servicio_apoyo_farmacia_propio",
    "SERAP03T" = "servicio_apoyo_farmacia_tercerizado",
    # ... y asi sucesivamente para todos los campos SERAP*

    # --- Gestion de Residuos ---
    "RES_BIOL" = "recolecta_residuo_biologico",
    "RES_QUIM" = "recolecta_residuo_quimico",
    "RES_RADI" = "recolecta_residuo_radiactivo",
    "RES_COMU" = "recolecta_residuo_comun",
    "COLETRES" = "tiene_recoleccion_residuos",

    # --- Comites ---
    "COMISSAO" = "tiene_comite",
    "COMISS01" = "comite_etica_medica",
    "COMISS02" = "comite_etica_enfermeria",
    "COMISS03" = "comite_farmacia_terapeutica",
    "COMISS04" = "comite_control_infeccion",
    "COMISS05" = "comite_apropiacion_costos",
    "COMISS06" = "comite_cipa", # Comision Interna de Prevencion de Accidentes
    "COMISS07" = "comite_revision_historias_clinicas",
    "COMISS08" = "comite_revision_documentacion_medica",
    "COMISS09" = "comite_analisis_fallecimientos_biopsias",
    "COMISS10" = "comite_investigacion_epidemiologica",
    "COMISS11" = "comite_notificacion_enfermedades",
    "COMISS12" = "comite_control_zoonosis_vectores",

    # --- Atencion por Convenio ---
    "ATEND_PR" = "tiene_atencion_prestada",
    "AP01CV01" = "atiende_hospitalizacion_sus",
    "AP01CV02" = "atiende_hospitalizacion_particular",
    # ... y asi sucesivamente para todos los campos AP*CV*

    # --- Otros ---
    "DT_ATUAL" = "fecha_actualizacion",
    "COMPETEN" = "competencia"
  )

  # ==========================================================================
  # TRADUCCION DE VALORES CATEGORICOS (CNES)
  # Decodifica los codigos a etiquetas legibles en espanol.
  # ==========================================================================

  values <- list(
    "PF_PJ" = c("1" = "Persona Fisica", "3" = "Persona Juridica"),
    "NIV_DEP" = c("1" = "Individual", "3" = "Mantenida"),
    "VINC_SUS" = c("1" = "Si", "0" = "No"),
    "TPGESTAO" = c("M" = "Municipal", "E" = "Estatal", "D" = "Doble"),
    "ESFERA_A" = c(
      "01" = "Federal", "02" = "Estatal", "03" = "Municipal", "04" = "Privada"
    ),
    "ATIVIDAD" = c(
      "01" = "Ensenanza", "02" = "Investigacion", "03" = "Ensenanza e Investigacion", "04" = "Ninguna"
    ),
    "NATUREZA" = c(
      "10" = "Administracion Publica", "20" = "Entidades Empresariales",
      "30" = "Entidades sin Fines de Lucro", "40" = "Personas Fisicas",
      "50" = "Organizaciones Internacionales"
    ),
    "TP_PREST" = c("40" = "Publico", "50" = "Privado"),
    "TERCEIRO" = c("1" = "Si", "0" = "No", "2" = "No"), # Mapeando 2 a No

    # --- Plantilla Si/No (1 para Si, 0 para No) ---
    "SI_NO_FLAG" = c("1" = "Si", "0" = "No")
  )

  # Lista de todas las columnas que usan el patron Si/No
  si_no_vars <- c(
    "AV_ACRED", "AV_PNASS", "GESPRG1E", "GESPRG1M", "GESPRG2E", "GESPRG2M",
    "GESPRG3E", "GESPRG3M", "GESPRG4E", "GESPRG4M", "GESPRG5E", "GESPRG5M",
    "GESPRG6E", "GESPRG6M", "NIVATE_A", "NIVATE_H", "LEITHOSP", "URGEMERG",
    "ATENDAMB", "CENTRCIR", "CENTROBS", "CENTRNEO", "ATENDHOS", "SERAPOIO",
    "COLETRES", "COMISSAO",
    "COMISS01", "COMISS02", "COMISS03", "COMISS04", "COMISS05", "COMISS06",
    "COMISS07", "COMISS08", "COMISS09", "COMISS10", "COMISS11", "COMISS12",
    "ATEND_PR", "AP01CV01", "AP01CV02", "AP01CV03", "AP01CV04", "AP01CV05",
    "AP01CV06", "AP01CV07", "AP02CV01", "AP02CV02", "AP02CV03", "AP02CV04",
    "AP02CV05", "AP02CV06", "AP02CV07", "AP03CV01", "AP03CV02", "AP03CV03",
    "AP03CV04", "AP03CV05", "AP03CV06", "AP03CV07", "AP04CV01", "AP04CV02",
    "AP04CV03", "AP04CV04", "AP04CV05", "AP04CV06", "AP04CV07", "AP05CV01",
    "AP05CV02", "AP05CV03", "AP05CV04", "AP05CV05", "AP05CV06", "AP05CV07",
    "AP06CV01", "AP06CV02", "AP06CV03", "AP06CV04", "AP06CV05", "AP06CV06",
    "AP06CV07", "AP07CV01", "AP07CV02", "AP07CV03", "AP07CV04", "AP07CV05",
    "AP07CV06", "AP07CV07",
    "SERAP01P", "SERAP01T", "SERAP02P", "SERAP02T", "SERAP03P", "SERAP03T"
    # Anadir otros SERAP* y demas flags aqui
  )

  for (var in si_no_vars) {
    if (var %in% names(columns)) { # Verifica si la columna existe en el mapeo
       values[[var]] <- values[["SI_NO_FLAG"]]
    }
  }
  values[["SI_NO_FLAG"]] <- NULL # Elimina la plantilla

  return(list(columns = columns, values = values))
}


#' Obtem o Dicionario de Traducao para o SINASC (Portugues)
#'
#' @description
#' Retorna uma lista contendo dicionarios para traduzir os nomes das colunas
#' e os valores das variaveis categoricas do Sistema de Informacoes sobre Nascidos Vivos (SINASC)
#' para o portugues. O objetivo e padronizar e facilitar a analise dos dados.
#'
#' @return Uma lista com dois elementos:
#'   - `columns`: um vetor nomeado para traduzir os nomes das colunas.
#'   - `values`: uma lista de vetores nomeados para decodificar valores categoricos.
#'
#' @keywords internal
#' @noRd
get_translation_dict_pt_sinasc <- function() {

  # ==========================================================================
  # TRADUCAO DOS NOMES DAS COLUNAS (SINASC)
  # Mapeia os nomes originais do SINASC para um formato padronizado em portugues.
  # ==========================================================================

  columns <- c(
    # --- Identificacao da Declaracao de Nascido Vivo (DN) ---
    "NUMERODN" = "numero_dn",
    "CODINST" = "codigo_estabelecimento_gerador",
    "UFINFORM" = "uf_informante",
    "DTCADASTRO" = "data_cadastro",
    "DTRECEBIM" = "data_recebimento",

    # --- Local e Data do Nascimento ---
    "LOCNASC" = "local_nascimento",
    "CODESTAB" = "codigo_estabelecimento_nascimento",
    "CODMUNNASC" = "codigo_municipio_nascimento",
    "CODBAINASC" = "codigo_bairro_nascimento",
    "DTNASC" = "data_nascimento",
    "HORANASC" = "hora_nascimento",

    # --- Dados da Mae ---
    "IDADEMAE" = "idade_mae",
    "ESTCIVMAE" = "estado_civil_mae",
    "ESCMAE" = "escolaridade_mae",
    "CODOCUPMAE" = "cbo_ocupacao_mae",
    "QTDFILVIVO" = "qtd_filhos_vivos",
    "QTDFILMORT" = "qtd_filhos_mortos",
    "CODMUNRES" = "codigo_municipio_residencia",
    "CODBAIRES" = "codigo_bairro_residencia",

    # --- Gestacao e Parto ---
    "GESTACAO" = "semanas_gestacao",
    "GRAVIDEZ" = "tipo_gravidez",
    "PARTO" = "tipo_parto",
    "CONSULTAS" = "numero_consultas_pre_natal",

    # --- Dados do Recem-Nascido ---
    "SEXO" = "sexo",
    "APGAR1" = "apgar_1_minuto",
    "APGAR5" = "apgar_5_minutos",
    "RACACOR" = "raca",
    "PESO" = "peso_nascimento_gramas",
    "IDANOMAL" = "anomalia_congenita",
    "CODANOMAL" = "codigo_anomalia_congenita"
  )

  # ==========================================================================
  # TRADUCAO DE VALORES CATEGORICOS (SINASC)
  # Decodifica os codigos para rotulos legiveis em portugues.
  # ==========================================================================

  values <- list(
    "LOCNASC" = c(
      "1" = "Hospital",
      "2" = "Outro estabelecimento de saude",
      "3" = "Domicilio",
      "4" = "Outros",
      "9" = "Ignorado"
    ),
    "ESTCIVMAE" = c(
      "1" = "Solteira",
      "2" = "Casada",
      "3" = "Viuva",
      "4" = "Separado judicialmente/Divorciada",
      "5" = "Uniao estavel", # Presente em algumas versoes do layout
      "9" = "Ignorado"
    ),
    "ESCMAE" = c(
      "1" = "Nenhuma",
      "2" = "1 a 3 anos",
      "3" = "4 a 7 anos",
      "4" = "8 a 11 anos",
      "5" = "12 anos e mais",
      "9" = "Ignorado"
    ),
    "GESTACAO" = c(
      "1" = "Menos de 22 semanas",
      "2" = "22 a 27 semanas",
      "3" = "28 a 31 semanas",
      "4" = "32 a 36 semanas",
      "5" = "37 a 41 semanas",
      "6" = "42 semanas e mais",
      "9" = "Ignorado"
    ),
    "GRAVIDEZ" = c(
      "1" = "Unica",
      "2" = "Dupla",
      "3" = "Tripla e mais",
      "9" = "Ignorada"
    ),
    "PARTO" = c(
      "1" = "Vaginal",
      "2" = "Cesareo",
      "9" = "Ignorado"
    ),
    "CONSULTAS" = c(
      "1" = "Nenhuma",
      "2" = "1 a 3",
      "3" = "4 a 6",
      "4" = "7 e mais",
      "9" = "Ignorado"
    ),
    "SEXO" = c(
      "1" = "Masculino",
      "2" = "Feminino",
      "0" = "Ignorado" # Algumas versoes usam '0'
    ),
    "RACACOR" = c(
      "1" = "Branca",
      "2" = "Preta",
      "3" = "Amarela",
      "4" = "Parda",
      "5" = "Indigena",
      "9" = "Ignorado" # Embora o layout nao mostre, e uma boa pratica incluir
    ),
    "IDANOMAL" = c(
      "1" = "Sim",
      "2" = "Nao",
      "9" = "Ignorado"
    )
  )

  return(list(columns = columns, values = values))
}


#' Get the Translation Dictionary for SINASC (English)
#'
#' @description
#' Returns a list containing dictionaries to translate the column names
#' and categorical variable values of the Live Birth Information System (SINASC)
#' into English. The goal is to standardize and facilitate data analysis.
#'
#' @return A list with two elements:
#'   - `columns`: a named vector to translate column names.
#'   - `values`: a list of named vectors to decode categorical values.
#'
#' @keywords internal
#' @noRd
get_translation_dict_en_sinasc <- function() {

  # ==========================================================================
  # COLUMN NAME TRANSLATIONS (SINASC)
  # Maps original SINASC names to a standardized English snake_case format.
  # ==========================================================================

  columns <- c(
    # --- Live Birth Certificate (DN) Identification ---
    "NUMERODN" = "live_birth_certificate_number",
    "CODINST" = "generating_facility_code",
    "UFINFORM" = "informant_uf",
    "DTCADASTRO" = "registration_date",
    "DTRECEBIM" = "reception_date",

    # --- Location and Date of Birth ---
    "LOCNASC" = "place_of_birth",
    "CODESTAB" = "birth_facility_code",
    "CODMUNNASC" = "birth_municipality_code",
    "CODBAINASC" = "birth_neighborhood_code",
    "DTNASC" = "birth_date",
    "HORANASC" = "birth_time",

    # --- Mother's Data ---
    "IDADEMAE" = "mother_age",
    "ESTCIVMAE" = "mother_marital_status",
    "ESCMAE" = "mother_education_level",
    "CODOCUPMAE" = "mother_occupation_cbo",
    "QTDFILVIVO" = "qty_living_children",
    "QTDFILMORT" = "qty_dead_children",
    "CODMUNRES" = "residence_municipality_code",
    "CODBAIRES" = "residence_neighborhood_code",

    # --- Pregnancy and Delivery ---
    "GESTACAO" = "gestational_weeks",
    "GRAVIDEZ" = "pregnancy_type",
    "PARTO" = "delivery_type",
    "CONSULTAS" = "prenatal_visits_count",

    # --- Newborn's Data ---
    "SEXO" = "sex",
    "APGAR1" = "apgar_1_minute",
    "APGAR5" = "apgar_5_minutes",
    "RACACOR" = "race",
    "PESO" = "birth_weight_grams",
    "IDANOMAL" = "congenital_anomaly",
    "CODANOMAL" = "congenital_anomaly_code"
  )

  # ==========================================================================
  # CATEGORICAL VALUE TRANSLATIONS (SINASC)
  # Decodes codes into human-readable English labels.
  # ==========================================================================

  values <- list(
    "LOCNASC" = c(
      "1" = "Hospital",
      "2" = "Other health facility",
      "3" = "Home",
      "4" = "Others",
      "9" = "Ignored"
    ),
    "ESTCIVMAE" = c(
      "1" = "Single",
      "2" = "Married",
      "3" = "Widowed",
      "4" = "Separated/Divorced",
      "5" = "Consensual Union",
      "9" = "Ignored"
    ),
    "ESCMAE" = c(
      "1" = "None",
      "2" = "1 to 3 years",
      "3" = "4 to 7 years",
      "4" = "8 to 11 years",
      "5" = "12 years and more",
      "9" = "Ignored"
    ),
    "GESTACAO" = c(
      "1" = "Less than 22 weeks",
      "2" = "22 to 27 weeks",
      "3" = "28 to 31 weeks",
      "4" = "32 to 36 weeks",
      "5" = "37 to 41 weeks",
      "6" = "42 weeks and more",
      "9" = "Ignored"
    ),
    "GRAVIDEZ" = c(
      "1" = "Single",
      "2" = "Double",
      "3" = "Triple and more",
      "9" = "Ignored"
    ),
    "PARTO" = c(
      "1" = "Vaginal",
      "2" = "Cesarean",
      "9" = "Ignored"
    ),
    "CONSULTAS" = c(
      "1" = "None",
      "2" = "1 to 3",
      "3" = "4 to 6",
      "4" = "7 and more",
      "9" = "Ignored"
    ),
    "SEXO" = c(
      "1" = "Male",
      "2" = "Female",
      "0" = "Ignored"
    ),
    "RACACOR" = c(
      "1" = "White",
      "2" = "Black",
      "3" = "Asian", # 'Amarela'
      "4" = "Brown", # 'Parda'
      "5" = "Indigenous",
      "9" = "Ignored"
    ),
    "IDANOMAL" = c(
      "1" = "Yes",
      "2" = "No",
      "9" = "Ignored"
    )
  )

  return(list(columns = columns, values = values))
}


#' Obtiene el Diccionario de Traduccion para SINASC (Espanol)
#'
#' @description
#' Devuelve una lista que contiene diccionarios para traducir los nombres de las columnas
#' y los valores de las variables categoricas del Sistema de Informacion sobre Nacidos Vivos (SINASC)
#' al espanol. El objetivo es estandarizar y facilitar el analisis de datos.
#'
#' @return Una lista con dos elementos:
#'   - `columns`: un vector nombrado para traducir los nombres de las columnas.
#'   - `values`: una lista de vectores nombrados para decodificar valores categoricos.
#'
#' @keywords internal
#' @noRd
get_translation_dict_es_sinasc <- function() {

  # ==========================================================================
  # TRADUCCION DE NOMBRES DE COLUMNAS (SINASC)
  # Mapea los nombres originales de SINASC a un formato estandarizado en espanol.
  # ==========================================================================

  columns <- c(
    # --- Identificacion de la Declaracion de Nacido Vivo (DN) ---
    "NUMERODN" = "numero_dn",
    "CODINST" = "codigo_establecimiento_generador",
    "UFINFORM" = "uf_informante",
    "DTCADASTRO" = "fecha_registro",
    "DTRECEBIM" = "fecha_recepcion",

    # --- Lugar y Fecha de Nacimiento ---
    "LOCNASC" = "lugar_nacimiento",
    "CODESTAB" = "codigo_establecimiento_nacimiento",
    "CODMUNNASC" = "codigo_municipio_nacimiento",
    "CODBAINASC" = "codigo_barrio_nacimiento",
    "DTNASC" = "fecha_nacimiento",
    "HORANASC" = "hora_nacimiento",

    # --- Datos de la Madre ---
    "IDADEMAE" = "edad_madre",
    "ESTCIVMAE" = "estado_civil_madre",
    "ESCMAE" = "escolaridad_madre",
    "CODOCUPMAE" = "cbo_ocupacion_madre",
    "QTDFILVIVO" = "cant_hijos_vivos",
    "QTDFILMORT" = "cant_hijos_fallecidos",
    "CODMUNRES" = "codigo_municipio_residencia",
    "CODBAIRES" = "codigo_barrio_residencia",

    # --- Gestacion y Parto ---
    "GESTACAO" = "semanas_gestacion",
    "GRAVIDEZ" = "tipo_embarazo",
    "PARTO" = "tipo_parto",
    "CONSULTAS" = "numero_consultas_prenatales",

    # --- Datos del Recien Nacido ---
    "SEXO" = "sexo",
    "APGAR1" = "apgar_1_minuto",
    "APGAR5" = "apgar_5_minutos",
    "RACACOR" = "raza",
    "PESO" = "peso_al_nacer_gramos",
    "IDANOMAL" = "anomalia_congenita",
    "CODANOMAL" = "codigo_anomalia_congenita"
  )

  # ==========================================================================
  # TRADUCCION DE VALORES CATEGORICOS (SINASC)
  # Decodifica los codigos a etiquetas legibles en espanol.
  # ==========================================================================

  values <- list(
    "LOCNASC" = c(
      "1" = "Hospital",
      "2" = "Otro establecimiento de salud",
      "3" = "Domicilio",
      "4" = "Otros",
      "9" = "Ignorado"
    ),
    "ESTCIVMAE" = c(
      "1" = "Soltera",
      "2" = "Casada",
      "3" = "Viuda",
      "4" = "Separada/Divorciada",
      "5" = "Union consensual",
      "9" = "Ignorado"
    ),
    "ESCMAE" = c(
      "1" = "Ninguna",
      "2" = "1 a 3 anos",
      "3" = "4 a 7 anos",
      "4" = "8 a 11 anos",
      "5" = "12 anos y mas",
      "9" = "Ignorado"
    ),
    "GESTACAO" = c(
      "1" = "Menos de 22 semanas",
      "2" = "22 a 27 semanas",
      "3" = "28 a 31 semanas",
      "4" = "32 a 36 semanas",
      "5" = "37 a 41 semanas",
      "6" = "42 semanas y mas",
      "9" = "Ignorado"
    ),
    "GRAVIDEZ" = c(
      "1" = "Unico",
      "2" = "Doble",
      "3" = "Triple y mas",
      "9" = "Ignorado"
    ),
    "PARTO" = c(
      "1" = "Vaginal",
      "2" = "Cesarea",
      "9" = "Ignorado"
    ),
    "CONSULTAS" = c(
      "1" = "Ninguna",
      "2" = "1 a 3",
      "3" = "4 a 6",
      "4" = "7 y mas",
      "9" = "Ignorado"
    ),
    "SEXO" = c(
      "1" = "Masculino",
      "2" = "Femenino",
      "0" = "Ignorado"
    ),
    "RACACOR" = c(
      "1" = "Blanca",
      "2" = "Negra",
      "3" = "Asiatica", # 'Amarela'
      "4" = "Parda",
      "5" = "Indigena",
      "9" = "Ignorado"
    ),
    "IDANOMAL" = c(
      "1" = "Si",
      "2" = "No",
      "9" = "Ignorado"
    )
  )

  return(list(columns = columns, values = values))
}



#' Get UI messages in specified language
#'
#' Returns user interface messages in the specified language.
#'
#' @param lang Character. Language code: "pt", "es", or "en".
#'
#' @return A list of translated UI messages.
#' @keywords internal
get_ui_messages <- function(lang = "en") {
  
  messages <- list(
    en = list(
      data_import_header = "Climasus4r Data Import",
      system = "System",
      states = "States",
      years = "Years",
      total_downloads = "Total downloads",
      parallel_enabled = "Parallel processing: ENABLED",
      workers = "workers",
      download_completed = "Download and combination completed successfully!",
      total_records = "Total records",
      total_columns = "Total columns",
      encoding_header = "Climasus4r Encoding Cleanup",
      checking_columns = "Checking text columns for encoding issues...",
      corrected_columns = "Corrected column(s)",
      affected_columns = "Affected columns",
      no_correction_needed = "All text columns have correct encoding. No corrections needed.",
      standardization_header = "Climasus4r Data Standardization",
      original_columns = "Original columns",
      original_records = "Original records",
      translated_columns = "Translated column names to",
      standardized_values = "Standardized values in categorical variable(s)",
      standardization_completed = "Standardization completed!",
      final_columns = "Final columns",
      filtering_header = "Climasus4r ICD-10 Filtering",
      icd_column = "ICD column",
      filter_codes = "Filter codes",
      match_type = "Match type",
      filtering_completed = "Filtering completed!",
      records_kept = "Records kept",
      records_removed = "Records removed",
      detected_system = "Detected health system"
    ),
    pt = list(
      data_import_header = "climasus4r Importa\u00e7\u00e3o de Dados",
      system = "Sistema",
      states = "Estados",
      years = "Anos",
      total_downloads = "Total de downloads",
      parallel_enabled = "Processamento paralelo: ATIVADO",
      workers = "trabalhadores",
      download_completed = "Download e combina\u00e7\u00e3o conclu\u00eddos com sucesso!",
      total_records = "Total de registros",
      total_columns = "Total de colunas",
      encoding_header = "climasus4r Limpeza de Codifica\u00e7\u00e3o",
      checking_columns = "Verificando colunas de texto para problemas de codifica\u00e7\u00e3o...",
      corrected_columns = "Coluna(s) corrigida(s)",
      affected_columns = "Colunas afetadas",
      no_correction_needed = "Todas as colunas de texto t\u00eam codifica\u00e7\u00e3o correta. Nenhuma corre\u00e7\u00e3o necess\u00e1ria.",
      standardization_header = "climasus4r Padroniza\u00e7\u00e3o de Dados",
      original_columns = "Colunas originais",
      original_records = "Registros originais",
      translated_columns = "Nomes de colunas traduzidos para",
      standardized_values = "Valores padronizados em vari\u00e1vel(is) categ\u00f3rica(s)",
      standardization_completed = "Padroniza\u00e7\u00e3o conclu\u00edda!",
      final_columns = "Colunas finais",
      filtering_header = "climasus4r Filtragem por CID-10",
      icd_column = "Coluna CID",
      filter_codes = "C\u00f3digos de filtro",
      match_type = "Tipo de correspond\u00eancia",
      filtering_completed = "Filtragem conclu\u00edda!",
      records_kept = "Registros mantidos",
      records_removed = "Registros removidos",
      detected_system = "Sistema de sau\u00eddde detectado"
    ),
    es = list(
      data_import_header = "climasus4r Importaci\u00f3n de Datos",
      system = "Sistema",
      states = "Estados",
      years = "A\u00f1os",
      total_downloads = "Total de descargas",
      parallel_enabled = "Procesamiento paralelo: ACTIVADO",
      workers = "trabajadores",
      download_completed = "\u00a1Descarga y combinaci\u00f3n completadas con \u00e9xito!",
      total_records = "Total de registros",
      total_columns = "Total de columnas",
      encoding_header = "climasus4r Limpieza de Codificaci\u00f3n",
      checking_columns = "Verificando columnas de texto para problemas de codificaci\u00f3n...",
      corrected_columns = "Columna(s) corregida(s)",
      affected_columns = "Columnas afectadas",
      no_correction_needed = "Todas las columnas de texto tienen codificaci\u00f3n correcta. No se necesitan correcciones.",
      standardization_header = "climasus4r Estandarizaci\u00f3n de Datos",
      original_columns = "Columnas originales",
      original_records = "Registros originales",
      translated_columns = "Nombres de columnas traducidos a",
      standardized_values = "Valores estandarizados en variable(s) categ\u00f3rica(s)",
      standardization_completed = "\u00a1Estandarizaci\u00f3n completada!",
      final_columns = "Columnas finales",
      filtering_header = "climasus4r Filtrado por CIE-10",
      icd_column = "Columna CIE",
      filter_codes = "C\u00f3digos de filtro",
      match_type = "Tipo de coincidencia",
      filtering_completed = "\u00a1Filtrado completado!",
      records_kept = "Registros mantenidos",
      records_removed = "Registros eliminados",
      detected_system = "Sistema de salud detectado"
    )
  )
  
  if (!lang %in% names(messages)) {
    warning(sprintf("Language '%s' not supported for UI messages. Defaulting to English.", lang))
    lang <- "en"
  }
  
  return(messages[[lang]])
}


#' Detect Health System from Data Frame
#'
#' Automatically detects which Brazilian health system (SIM, SINASC, SINAN, SIH, SIA, CNES)
#' the data frame belongs to based on characteristic columns. Works with both original
#' DATASUS column names and translated names (EN/PT/ES) from sus_data_standardize().
#'
#' @param df A data.frame containing SUS health data
#'
#' @return Character string with system name: "SIM", "SINASC", "SINAN", "SIH", "SIA", "CNES", or "UNKNOWN"
#'
#' @details
#' The function checks for characteristic columns in three languages:
#' - Original DATASUS names (uppercase, Portuguese)
#' - English translations (lowercase with underscores)
#' - Portuguese translations (lowercase with underscores)
#' - Spanish translations (lowercase with underscores)
#'
#' This ensures compatibility whether data has been processed through
#' sus_data_standardize() or not.
#'
#' @examples
#' \dontrun{
#' # With original DATASUS names
#' system <- detect_health_system(raw_sim_data)  # Returns "SIM"
#'
#' # With translated names (after sus_data_standardize)
#' standardized_data <- sus_data_standardize(raw_sim_data, lang = "en")
#' system <- detect_health_system(standardized_data)  # Still returns "SIM"
#' }
#' @keywords internal
detect_health_system <- function(df) {
  cols <- names(df)
  
  # ============================================================================
  # SIM (Mortality System)
  # ============================================================================
  # Original: CAUSABAS, TIPOBITO, DTOBITO, LINHAA, LINHAB, etc.
  # EN: underlying_cause, death_type, death_date, cause_line_a, etc.
  # PT: causa_basica, tipo_obito, data_obito, linha_causa_a, etc.
  # ES: causa_basica, tipo_muerte, fecha_muerte, linea_causa_a, etc.
  
  sim_indicators <- c(
    # Original DATASUS
    "CAUSABAS", "TIPOBITO", "DTOBITO", "LINHAA", "LINHAB",
    # English
    "underlying_cause", "death_type", "death_date", "cause_line_a", "cause_line_b",
    # Portuguese
    "causa_basica", "tipo_obito", "data_obito", "linha_causa_a", "linha_causa_b",
    # Spanish
    "causa_basica", "tipo_muerte", "fecha_muerte", "linea_causa_a", "linea_causa_b"
  )
  
  if (sum(sim_indicators %in% cols) >= 2) {
    return("SIM")
  }
  
  # ============================================================================
  # SINASC (Live Births System)
  # ============================================================================
  # Original: LOCNASC, APGAR1, APGAR5, DTNASC, IDADEMAE
  # EN: birth_place, apgar_1min, apgar_5min, birth_date, mother_age
  # PT: local_nascimento, apgar_1min, apgar_5min, data_nascimento, idade_mae
  # ES: lugar_nacimiento, apgar_1min, apgar_5min, fecha_nacimiento, edad_madre
  
  sinasc_indicators <- c(
    # Original DATASUS
    "LOCNASC", "APGAR1", "APGAR5", "DTNASC", "IDADEMAE",
    # English
    "birth_place", "apgar_1min", "apgar_5min", "birth_date", "mother_age",
    # Portuguese
    "local_nascimento", "apgar_1min", "apgar_5min", "data_nascimento", "idade_mae",
    # Spanish
    "lugar_nacimiento", "apgar_1min", "apgar_5min", "fecha_nacimiento", "edad_madre"
  )
  
  if (sum(sinasc_indicators %in% cols) >= 2) {
    return("SINASC")
  }
  
  # ============================================================================
  # SINAN (Notifiable Diseases System)
  # ============================================================================
  # Original: DT_NOTIFIC, ID_AGRAVO, TP_NOT, DT_SIN_PRI
  # EN: notification_date, disease_code, notification_type, symptom_date
  # PT: data_notificacao, codigo_agravo, tipo_notificacao, data_sintomas
  # ES: fecha_notificacion, codigo_enfermedad, tipo_notificacion, fecha_sintomas
  
  sinan_indicators <- c(
    # Original DATASUS
    "DT_NOTIFIC", "ID_AGRAVO", "TP_NOT", "DT_SIN_PRI",
    # English
    "notification_date", "disease_code", "notification_type", "symptom_date",
    # Portuguese
    "data_notificacao", "codigo_agravo", "tipo_notificacao", "data_sintomas",
    # Spanish
    "fecha_notificacion", "codigo_enfermedad", "tipo_notificacion", "fecha_sintomas"
  )
  
  if (sum(sinan_indicators %in% cols) >= 2) {
    return("SINAN")
  }
  
  # ============================================================================
  # SIH (Hospital Admissions System)
  # ============================================================================
  # Original: N_AIH, DIAG_PRINC, QT_DIARIAS, DT_INTER, DT_SAIDA
  # EN: aih_number, primary_diagnosis, length_of_stay, admission_date, discharge_date
  # PT: numero_aih, diagnostico_principal, dias_internacao, data_internacao, data_saida
  # ES: numero_aih, diagnostico_principal, dias_internacion, fecha_internacion, fecha_alta
  
  sih_indicators <- c(
    # Original DATASUS
    "N_AIH", "DIAG_PRINC", "QT_DIARIAS", "DT_INTER", "DT_SAIDA",
    # English
    "aih_number", "primary_diagnosis", "length_of_stay", "admission_date", "discharge_date",
    # Portuguese
    "numero_aih", "diagnostico_principal", "dias_internacao", "data_internacao", "data_saida",
    # Spanish
    "numero_aih", "diagnostico_principal", "dias_internacion", "fecha_internacion", "fecha_alta"
  )
  
  if (sum(sih_indicators %in% cols) >= 2) {
    return("SIH")
  }
  
  # ============================================================================
  # SIA (Outpatient System)
  # ============================================================================
  # Original: PA_PROC_ID, PA_CIDPRI, PA_AUTORIZ, PA_CBOCOD
  # EN: procedure_id, primary_icd, authorization_number, occupation_code
  # PT: codigo_procedimento, cid_principal, numero_autorizacao, codigo_ocupacao
  # ES: codigo_procedimiento, cie_principal, numero_autorizacion, codigo_ocupacion
  
  sia_indicators <- c(
    # Original DATASUS
    "PA_PROC_ID", "PA_CIDPRI", "PA_AUTORIZ", "PA_CBOCOD",
    # English
    "procedure_id", "primary_icd", "authorization_number", "occupation_code",
    # Portuguese
    "codigo_procedimento", "cid_principal", "numero_autorizacao", "codigo_ocupacao",
    # Spanish
    "codigo_procedimiento", "cie_principal", "numero_autorizacion", "codigo_ocupacion"
  )
  
  if (sum(sia_indicators %in% cols) >= 2) {
    return("SIA")
  }
  
  # ============================================================================
  # CNES (Health Establishments Registry)
  # ============================================================================
  # Original: CNES, TP_UNID, NATUREZA, ESFERA_A
  # EN: cnes_code, facility_type, legal_nature, administrative_sphere
  # PT: codigo_cnes, tipo_unidade, natureza_juridica, esfera_administrativa
  # ES: codigo_cnes, tipo_unidad, naturaleza_juridica, esfera_administrativa
  
  cnes_indicators <- c(
    # Original DATASUS
    "CNES", "TP_UNID", "NATUREZA", "ESFERA_A",
    # English
    "cnes_code", "facility_type", "legal_nature", "administrative_sphere",
    # Portuguese
    "codigo_cnes", "tipo_unidade", "natureza_juridica", "esfera_administrativa",
    # Spanish
    "codigo_cnes", "tipo_unidad", "naturaleza_juridica", "esfera_administrativa"
  )
  
  if (sum(cnes_indicators %in% cols) >= 2) {
    return("CNES")
  }
  
  # ============================================================================
  # If no system detected
  # ============================================================================
  return("UNKNOWN")
}

# ============================================================================
# Helper function to get system description
# ============================================================================

#' Get Health System Description
#'
#' Returns a human-readable description of the health system in the specified language.
#'
#' @param system Character. System code ("SIM", "SINASC", "SINAN", "SIH", "SIA", "CNES")
#' @param lang Character. Language ("en", "pt", "es"). Default "en".
#'
#' @return Character string with system description
#'
#' @keywords internal
get_system_description <- function(system, lang = "en") {
  
  descriptions <- list(
    SIM = list(
      en = "Mortality Information System (SIM)",
      pt = "Sistema de Informacoes sobre Mortalidade (SIM)",
      es = "Sistema de Informacion sobre Mortalidad (SIM)"
    ),
    SINASC = list(
      en = "Live Birth Information System (SINASC)",
      pt = "Sistema de Informacoes sobre Nascidos Vivos (SINASC)",
      es = "Sistema de Informacion sobre Nacidos Vivos (SINASC)"
    ),
    SINAN = list(
      en = "Notifiable Diseases Information System (SINAN)",
      pt = "Sistema de Informacao de Agravos de Notificacao (SINAN)",
      es = "Sistema de Informacion de Enfermedades de Notificacion (SINAN)"
    ),
    SIH = list(
      en = "Hospital Information System (SIH)",
      pt = "Sistema de Informacoes Hospitalares (SIH)",
      es = "Sistema de Informacion Hospitalaria (SIH)"
    ),
    SIA = list(
      en = "Outpatient Information System (SIA)",
      pt = "Sistema de Informacoes Ambulatoriais (SIA)",
      es = "Sistema de Informacion Ambulatoria (SIA)"
    ),
    CNES = list(
      en = "National Registry of Health Establishments (CNES)",
      pt = "Cadastro Nacional de Estabelecimentos de Saude (CNES)",
      es = "Registro Nacional de Establecimientos de Salud (CNES)"
    ),
    UNKNOWN = list(
      en = "Unknown system",
      pt = "Sistema desconhecido",
      es = "Sistema desconocido"
    )
  )
  
  if (system %in% names(descriptions)) {
    return(descriptions[[system]][[lang]])
  } else {
    return(descriptions[["UNKNOWN"]][[lang]])
  }
}
# Additional helper functions for cache management

#' Clear the Climasus4r cache
#'
#' @param cache_dir Character. Cache directory to clear. Default is "~/.climasus4r_cache".
#' @param older_than_days Numeric. Only clear files older than this many days. NULL clears all.
#' @param verbose Logical. If TRUE, prints information about what was cleared.
#'
#' @importFrom stats median sd quantile
#' @export
#'
#' @examples
#' \dontrun{
#' # Clear all cache
#' clear_climasus_cache()
#'
#' # Clear only files older than 90 days
#' clear_climasus_cache(older_than_days = 90)
#' }
clear_climasus_cache <- function(cache_dir = "~/.climasus4r_cache", 
                                 older_than_days = NULL,
                                 verbose = TRUE) {
  
  cache_dir <- path.expand(cache_dir)
  
  if (!fs::dir_exists(cache_dir)) {
    if (verbose) cli::cli_alert_info("Cache directory does not exist: {cache_dir}")
    return(invisible(NULL))
  }
  
  cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  
  if (length(cache_files) == 0) {
    if (verbose) cli::cli_alert_info("No cache files found in {cache_dir}")
    return(invisible(NULL))
  }
  
  if (!is.null(older_than_days)) {
    file_info <- fs::file_info(cache_files)
    file_age <- difftime(Sys.time(), file_info$modification_time, units = "days")
    files_to_delete <- cache_files[file_age > older_than_days]
  } else {
    files_to_delete <- cache_files
  }
  
  if (length(files_to_delete) == 0) {
    if (verbose) cli::cli_alert_info("No files match the criteria for deletion.")
    return(invisible(NULL))
  }
  
  total_size <- sum(file.info(files_to_delete)$size, na.rm = TRUE)
  
  if (verbose) {
    cli::cli_alert_info("Clearing cache: {cache_dir}")
    cli::cli_alert_info("Files to delete: {length(files_to_delete)}")
    cli::cli_alert_info("Total size: {format(total_size, big.mark = ',')} bytes")
    
    if (interactive()) {
      proceed <- utils::menu(
        c("Yes", "No"),
        title = paste("Are you sure you want to delete", length(files_to_delete), "files?")
      )
      if (proceed != 1) {
        cli::cli_alert_info("Cache clearing cancelled.")
        return(invisible(NULL))
      }
    }
  }
  
  unlink(files_to_delete)
  
  if (verbose) {
    cli::cli_alert_success("Successfully cleared {length(files_to_delete)} cache files.")
  }
  
  return(invisible(length(files_to_delete)))
}

#' Get cache information
#'
#' @param cache_dir Character. Cache directory to inspect. Default is "~/.climasus4r_cache".
#' @param verbose Logical. If TRUE, prints detailed information.
#'
#' @return A list with cache statistics.
#' @export
#'
#' @examples
#' \dontrun{
#' cache_info <- get_climasus_cache_info()
#' }
get_climasus_cache_info <- function(cache_dir = "~/.climasus4r_cache", 
                                    verbose = TRUE) {
  
  cache_dir <- path.expand(cache_dir)
  
  if (!fs::dir_exists(cache_dir)) {
    if (verbose) cli::cli_alert_info("Cache directory does not exist: {cache_dir}")
    return(list(
      exists = FALSE,
      cache_dir = cache_dir,
      file_count = 0,
      total_size = 0
    ))
  }
  
  cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = TRUE)
  
  if (length(cache_files) == 0) {
    cache_stats <- list(
      exists = TRUE,
      cache_dir = cache_dir,
      file_count = 0,
      total_size = 0,
      newest_file = NA,
      oldest_file = NA
    )
  } else {
    file_info <- fs::file_info(cache_files)
    
    cache_stats <- list(
      exists = TRUE,
      cache_dir = cache_dir,
      file_count = length(cache_files),
      total_size = sum(file_info$size, na.rm = TRUE),
      newest_file = max(file_info$modification_time, na.rm = TRUE),
      oldest_file = min(file_info$modification_time, na.rm = TRUE),
      avg_file_size = mean(file_info$size, na.rm = TRUE),
      median_file_size = median(file_info$size, na.rm = TRUE)
    )
    
    # Parse file names to get system distribution
    file_names <- basename(cache_files)
    if (verbose) {
      cli::cli_h2("Cache Information")
      cli::cli_alert_info("Cache directory: {cache_dir}")
      cli::cli_alert_info("Total files: {cache_stats$file_count}")
      cli::cli_alert_info("Total size: {format(cache_stats$total_size, big.mark = ',')} bytes")
      cli::cli_alert_info("Oldest file: {cache_stats$oldest_file}")
      cli::cli_alert_info("Newest file: {cache_stats$newest_file}")
    }
  }
  
  return(cache_stats)
}


# Internal ICD-10 Disease Groups Dictionary
# 
# This internal object contains comprehensive disease group classifications
# organized by epidemiological relevance and climate sensitivity.
# 
# @keywords internal
# @noRd

.icd_disease_groups <- list(

  # ============================================================================
  # CHAPTER I: INFECTIOUS AND PARASITIC DISEASES (A00-B99)
  # ============================================================================

  # --- Climate-Sensitive Infectious Diseases ---

  diarrheal = list(
    codes = c("A00-A09"),
    label = list(
      pt = "Doencas diarreicas",
      en = "Diarrheal diseases",
      es = "Enfermedades diarreicas"
    ),
    description = list(
      pt = "Doencas infecciosas intestinais incluindo colera, febre tifoide, shigelose",
      en = "Intestinal infectious diseases including cholera, typhoid, shigellosis",
      es = "Enfermedades infecciosas intestinales incluyendo colera, fiebre tifoidea, shigelosis"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "precipitation", "flooding")
  ),

  tuberculosis = list(
    codes = c("A15-A19"),
    label = list(
      pt = "Tuberculose",
      en = "Tuberculosis",
      es = "Tuberculosis"
    ),
    description = list(
      pt = "Tuberculose respiratoria e outras formas",
      en = "Respiratory and other forms of tuberculosis",
      es = "Tuberculosis respiratoria y otras formas"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "humidity", "seasonality")
  ),

  zoonotic_bacterial = list(
    codes = c("A20-A28"),
    label = list(
      pt = "Zoonoses bacterianas",
      en = "Bacterial zoonoses",
      es = "Zoonosis bacterianas"
    ),
    description = list(
      pt = "Peste, tularemia, antraz, leptospirose",
      en = "Plague, tularemia, anthrax, leptospirosis",
      es = "Peste, tularemia, antrax, leptospirosis"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("precipitation", "flooding", "temperature")
  ),

  leptospirosis = list(
    codes = c("A27"),
    label = list(
      pt = "Leptospirose",
      en = "Leptospirosis",
      es = "Leptospirosis"
    ),
    description = list(
      pt = "Leptospirose (doenca de Weil)",
      en = "Leptospirosis (Weil's disease)",
      es = "Leptospirosis (enfermedad de Weil)"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("precipitation", "flooding")
  ),

  hansen = list(
    codes = c("A30"),
    label = list(
      pt = "Hanseniase",
      en = "Leprosy",
      es = "Lepra"
    ),
    description = list(
      pt = "Hanseniase (lepra)",
      en = "Leprosy (Hansen's disease)",
      es = "Lepra (enfermedad de Hansen)"
    ),
    climate_sensitive = FALSE,
    climate_factors = NULL
  ),

  vector_borne = list(
    codes = c("A75-A79", "A90-A99"),
    label = list(
      pt = "Doencas transmitidas por vetores",
      en = "Vector-borne diseases",
      es = "Enfermedades transmitidas por vectores"
    ),
    description = list(
      pt = "Dengue, febre amarela, zika, chikungunya, malaria",
      en = "Dengue, yellow fever, zika, chikungunya, malaria",
      es = "Dengue, fiebre amarilla, zika, chikungunya, malaria"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "precipitation", "humidity")
  ),

  dengue = list(
    codes = c("A90", "A91"),
    label = list(
      pt = "Dengue",
      en = "Dengue",
      es = "Dengue"
    ),
    description = list(
      pt = "Dengue classica e hemorragica",
      en = "Classic and hemorrhagic dengue",
      es = "Dengue clasico y hemorragico"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "precipitation", "humidity")
  ),

  chikungunya = list(
    codes = c("A92.0"),
    label = list(
      pt = "Chikungunya",
      en = "Chikungunya",
      es = "Chikungunya"
    ),
    description = list(
      pt = "Febre de chikungunya",
      en = "Chikungunya fever",
      es = "Fiebre de chikungunya"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "precipitation")
  ),

  zika = list(
    codes = c("A92.5", "P35.4", "Q02"),
    label = list(
      pt = "Zika e complicacoes",
      en = "Zika and complications",
      es = "Zika y complicaciones"
    ),
    description = list(
      pt = "Zika virus e microcefalia associada",
      en = "Zika virus and associated microcephaly",
      es = "Virus Zika y microcefalia asociada"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "precipitation")
  ),

  yellow_fever = list(
    codes = c("A95"),
    label = list(
      pt = "Febre amarela",
      en = "Yellow fever",
      es = "Fiebre amarilla"
    ),
    description = list(
      pt = "Febre amarela silvestre e urbana",
      en = "Sylvatic and urban yellow fever",
      es = "Fiebre amarilla selvatica y urbana"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "precipitation", "deforestation")
  ),

  malaria = list(
    codes = c("B50-B54"),
    label = list(
      pt = "Malaria",
      en = "Malaria",
      es = "Malaria"
    ),
    description = list(
      pt = "Malaria por Plasmodium",
      en = "Malaria by Plasmodium",
      es = "Malaria por Plasmodium"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "precipitation", "humidity")
  ),

  leishmaniasis = list(
    codes = c("B55"),
    label = list(
      pt = "Leishmaniose",
      en = "Leishmaniasis",
      es = "Leishmaniasis"
    ),
    description = list(
      pt = "Leishmaniose visceral e cutanea",
      en = "Visceral and cutaneous leishmaniasis",
      es = "Leishmaniasis visceral y cutanea"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "humidity", "deforestation")
  ),

  chagas = list(
    codes = c("B57"),
    label = list(
      pt = "Doenca de Chagas",
      en = "Chagas disease",
      es = "Enfermedad de Chagas"
    ),
    description = list(
      pt = "Doenca de Chagas (tripanossomiase americana)",
      en = "Chagas disease (American trypanosomiasis)",
      es = "Enfermedad de Chagas (tripanosomiasis americana)"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "housing_conditions")
  ),

  schistosomiasis = list(
    codes = c("B65"),
    label = list(
      pt = "Esquistossomose",
      en = "Schistosomiasis",
      es = "Esquistosomiasis"
    ),
    description = list(
      pt = "Esquistossomose (barriga d'agua)",
      en = "Schistosomiasis (bilharzia)",
      es = "Esquistosomiasis (bilharzia)"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("water_availability", "temperature")
  ),

  helminthiasis = list(
    codes = c("B65-B83"),
    label = list(
      pt = "Helmintiases",
      en = "Helminthiases",
      es = "Helmintiasis"
    ),
    description = list(
      pt = "Infeccoes por helmintos (vermes)",
      en = "Helminth infections (worms)",
      es = "Infecciones por helmintos (gusanos)"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "humidity", "sanitation")
  ),

  # ============================================================================
  # CHAPTER II: NEOPLASMS (C00-D48)
  # ============================================================================

  skin_cancer = list(
    codes = c("C43-C44"),
    label = list(
      pt = "Cancer de pele",
      en = "Skin cancer",
      es = "Cancer de piel"
    ),
    description = list(
      pt = "Melanoma e outros canceres de pele",
      en = "Melanoma and other skin cancers",
      es = "Melanoma y otros canceres de piel"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("uv_radiation", "temperature")
  ),

  respiratory_cancer = list(
    codes = c("C30-C39"),
    label = list(
      pt = "Cancer respiratorio",
      en = "Respiratory cancer",
      es = "Cancer respiratorio"
    ),
    description = list(
      pt = "Cancer de pulmao, bronquios e traqueia",
      en = "Lung, bronchus, and trachea cancer",
      es = "Cancer de pulmon, bronquios y traquea"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("air_pollution")
  ),

  # ============================================================================
  # CHAPTER IV: ENDOCRINE, NUTRITIONAL AND METABOLIC DISEASES (E00-E90)
  # ============================================================================

  diabetes = list(
    codes = c("E10-E14"),
    label = list(
      pt = "Diabetes mellitus",
      en = "Diabetes mellitus",
      es = "Diabetes mellitus"
    ),
    description = list(
      pt = "Diabetes tipo 1, tipo 2 e outras formas",
      en = "Type 1, type 2, and other forms of diabetes",
      es = "Diabetes tipo 1, tipo 2 y otras formas"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "heatwaves")
  ),

  malnutrition = list(
    codes = c("E40-E46"),
    label = list(
      pt = "Desnutricao",
      en = "Malnutrition",
      es = "Desnutricion"
    ),
    description = list(
      pt = "Desnutricao proteico-calorica",
      en = "Protein-energy malnutrition",
      es = "Desnutricion proteico-energetica"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("drought", "food_security")
  ),

  # ============================================================================
  # CHAPTER V: MENTAL AND BEHAVIORAL DISORDERS (F00-F99)
  # ============================================================================

  mental_disorders = list(
    codes = c("F00-F99"),
    label = list(
      pt = "Transtornos mentais e comportamentais",
      en = "Mental and behavioral disorders",
      es = "Trastornos mentales y del comportamiento"
    ),
    description = list(
      pt = "Capitulo V da CID-10, abrangendo todos os transtornos mentais, incluindo depressao, ansiedade, esquizofrenia e transtornos por uso de substancias.",
      en = "Chapter V of ICD-10, covering all mental disorders, including depression, anxiety, schizophrenia, and substance use disorders.",
      es = "Capitulo V de la CIE-10, que abarca todos los trastornos mentales, incluyendo depresion, ansiedad, esquizofrenia y trastornos por uso de sustancias."
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature_extremes", "heat_waves", "natural_disasters", "seasonal_changes", "air_pollution"),
    data_sources = list(
      primary = c("SIH", "SIA", "SIM"),
      notes = list(
        pt = "Dados de transtornos mentais podem ser encontrados no SIH (internacoes), SIA (atendimentos ambulatoriais) e SIM (mortalidade por suicidio, que e uma consequencia, mas registrada em outro capitulo).",
        en = "Mental health data can be found in SIH (hospitalizations), SIA (outpatient care), and SIM (suicide mortality, which is a consequence but registered in another chapter).",
        es = "Los datos de salud mental se pueden encontrar en SIH (hospitalizaciones), SIA (atencion ambulatoria) y SIM (mortalidad por suicidio, que es una consecuencia pero registrada en otro capitulo)."
      )
    ),
    research_notes = list(
      pt = "Transtornos mentais sao altamente sensiveis a fatores climaticos como ondas de calor (aumento de agitacao, suicidios), desastres naturais (TEPT), variacoes sazonais (depressao sazonal) e poluicao do ar (inflamacao neural).",
      en = "Mental disorders are highly sensitive to climate factors such as heat waves (increased agitation, suicides), natural disasters (PTSD), seasonal variations (seasonal depression), and air pollution (neural inflammation).",
      es = "Los trastornos mentales son altamente sensibles a factores climaticos como olas de calor (aumento de agitacion, suicidios), desastres naturales (TEPT), variaciones estacionales (depresion estacional) y contaminacion del aire (inflamacion neural)."
    )
  ),

  climate_sensitive_mental_disorders = list(
    codes = c("F30-F33", "F41", "F43", "F50"),
    label = list(
      pt = "Transtornos mentais sensiveis ao clima",
      en = "Climate-sensitive mental disorders",
      es = "Trastornos mentales sensibles al clima"
    ),
    description = list(
      pt = "Subconjunto de transtornos mentais com forte evidencia de sensibilidade a fatores climaticos: transtornos do humor (depressao, bipolar), ansiedade, reacao ao estresse e transtornos alimentares.",
      en = "Subset of mental disorders with strong evidence of sensitivity to climate factors: mood disorders (depression, bipolar), anxiety, stress-related disorders, and eating disorders.",
      es = "Subconjunto de trastornos mentales con fuerte evidencia de sensibilidad a factores climaticos: trastornos del humor (depresion, bipolar), ansiedad, trastornos relacionados con el estres y trastornos alimentarios."
    ),
    climate_sensitive = TRUE,
    climate_factors = c("heat_waves", "seasonal_light_changes", "natural_disasters", "air_pollution", "temperature_variability"),
    notes = list(
      pt = "Inclui: Transtornos do humor (F30-F33), Transtornos de ansiedade (F41), Reacao ao estresse grave (F43), Transtornos alimentares (F50).",
      en = "Includes: Mood disorders (F30-F33), Anxiety disorders (F41), Reaction to severe stress (F43), Eating disorders (F50).",
      es = "Incluye: Trastornos del humor (F30-F33), Trastornos de ansiedad (F41), Reaccion al estres grave (F43), Trastornos alimentarios (F50)."
    )
  ),

  # ============================================================================
  # CHAPTER IX: CIRCULATORY SYSTEM (I00-I99)
  # ============================================================================

  cardiovascular = list(
    codes = c("I00-I99"),
    label = list(
      pt = "Doencas cardiovasculares",
      en = "Cardiovascular diseases",
      es = "Enfermedades cardiovasculares"
    ),
    description = list(
      pt = "Todas as doencas do aparelho circulatorio",
      en = "All diseases of the circulatory system",
      es = "Todas las enfermedades del sistema circulatorio"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "heatwaves", "cold_spells")
  ),

  ischemic_heart = list(
    codes = c("I20-I25"),
    label = list(
      pt = "Doencas isquemicas do coracao",
      en = "Ischemic heart diseases",
      es = "Enfermedades isquemicas del corazon"
    ),
    description = list(
      pt = "Infarto, angina e outras doencas isquemicas",
      en = "Myocardial infarction, angina, and other ischemic diseases",
      es = "Infarto de miocardio, angina y otras enfermedades isquemicas"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "heatwaves", "cold_spells", "air_pollution")
  ),

  acute_myocardial_infarction = list(
    codes = c("I21-I22"),
    label = list(
      pt = "Infarto agudo do miocardio",
      en = "Acute myocardial infarction",
      es = "Infarto agudo de miocardio"
    ),
    description = list(
      pt = "Infarto do miocardio",
      en = "Heart attack",
      es = "Ataque cardiaco"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "air_pollution")
  ),

  cerebrovascular = list(
    codes = c("I60-I69"),
    label = list(
      pt = "Doencas cerebrovasculares",
      en = "Cerebrovascular diseases",
      es = "Enfermedades cerebrovasculares"
    ),
    description = list(
      pt = "AVC, hemorragia cerebral e outras",
      en = "Stroke, cerebral hemorrhage, and others",
      es = "Accidente cerebrovascular, hemorragia cerebral y otras"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "heatwaves", "cold_spells")
  ),

  stroke = list(
    codes = c("I63-I64"),
    label = list(
      pt = "Acidente vascular cerebral",
      en = "Stroke",
      es = "Accidente cerebrovascular"
    ),
    description = list(
      pt = "AVC isquemico e nao especificado",
      en = "Ischemic and unspecified stroke",
      es = "ACV isquemico y no especificado"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "air_pollution")
  ),

  hypertension = list(
    codes = c("I10-I15"),
    label = list(
      pt = "Hipertensao arterial",
      en = "Hypertension",
      es = "Hipertension arterial"
    ),
    description = list(
      pt = "Hipertensao essencial e secundaria",
      en = "Essential and secondary hypertension",
      es = "Hipertension esencial y secundaria"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "heatwaves")
  ),

  # ============================================================================
  # CHAPTER X: RESPIRATORY SYSTEM (J00-J99)
  # ============================================================================

  respiratory = list(
    codes = c("J00-J99"),
    label = list(
      pt = "Doencas respiratorias",
      en = "Respiratory diseases",
      es = "Enfermedades respiratorias"
    ),
    description = list(
      pt = "Todas as doencas do aparelho respiratorio",
      en = "All diseases of the respiratory system",
      es = "Todas las enfermedades del sistema respiratorio"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "air_pollution", "humidity", "seasonality")
  ),

  acute_respiratory = list(
    codes = c("J00-J06", "J20-J22"),
    label = list(
      pt = "Infeccoes respiratorias agudas",
      en = "Acute respiratory infections",
      es = "Infecciones respiratorias agudas"
    ),
    description = list(
      pt = "Resfriado, gripe, bronquite aguda",
      en = "Common cold, flu, acute bronchitis",
      es = "Resfriado comun, gripe, bronquitis aguda"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "humidity", "seasonality")
  ),

  influenza_pneumonia = list(
    codes = c("J09-J18"),
    label = list(
      pt = "Influenza e pneumonia",
      en = "Influenza and pneumonia",
      es = "Influenza y neumonia"
    ),
    description = list(
      pt = "Gripe e pneumonia",
      en = "Flu and pneumonia",
      es = "Gripe y neumonia"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "humidity", "seasonality")
  ),

  pneumonia = list(
    codes = c("J12-J18"),
    label = list(
      pt = "Pneumonia",
      en = "Pneumonia",
      es = "Neumonia"
    ),
    description = list(
      pt = "Pneumonia viral, bacteriana e nao especificada",
      en = "Viral, bacterial, and unspecified pneumonia",
      es = "Neumonia viral, bacteriana y no especificada"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "humidity", "air_pollution")
  ),

  asthma = list(
    codes = c("J45-J46"),
    label = list(
      pt = "Asma",
      en = "Asthma",
      es = "Asma"
    ),
    description = list(
      pt = "Asma e estado de mal asmatico",
      en = "Asthma and status asthmaticus",
      es = "Asma y estado asmatico"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("air_pollution", "temperature", "humidity", "allergens")
  ),

  copd = list(
    codes = c("J40-J44"),
    label = list(
      pt = "DPOC",
      en = "COPD",
      es = "EPOC"
    ),
    description = list(
      pt = "Doenca pulmonar obstrutiva cronica",
      en = "Chronic obstructive pulmonary disease",
      es = "Enfermedad pulmonar obstructiva cronica"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("air_pollution", "temperature")
  ),

  # ============================================================================
  # CHAPTER XI: DIGESTIVE SYSTEM (K00-K93)
  # ============================================================================

  digestive = list(
    codes = c("K00-K93"),
    label = list(
      pt = "Doencas digestivas",
      en = "Digestive diseases",
      es = "Enfermedades digestivas"
    ),
    description = list(
      pt = "Doencas do aparelho digestivo",
      en = "Diseases of the digestive system",
      es = "Enfermedades del sistema digestivo"
    ),
    climate_sensitive = FALSE,
    climate_factors = NULL
  ),

  # ============================================================================
  # CHAPTER XII: SKIN AND SUBCUTANEOUS TISSUE (L00-L99)
  # ============================================================================

  skin_infections = list(
    codes = c("L00-L08"),
    label = list(
      pt = "Infeccoes de pele",
      en = "Skin infections",
      es = "Infecciones de piel"
    ),
    description = list(
      pt = "Infeccoes bacterianas e virais da pele",
      en = "Bacterial and viral skin infections",
      es = "Infecciones bacterianas y virales de la piel"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "humidity")
  ),

  # ============================================================================
  # CHAPTER XIV: GENITOURINARY SYSTEM (N00-N99)
  # ============================================================================

  renal = list(
    codes = c("N00-N29"),
    label = list(
      pt = "Doencas renais",
      en = "Renal diseases",
      es = "Enfermedades renales"
    ),
    description = list(
      pt = "Doencas dos rins e vias urinarias",
      en = "Diseases of kidneys and urinary tract",
      es = "Enfermedades de rinones y vias urinarias"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "dehydration")
  ),

  # ============================================================================
  # CHAPTER XV: PREGNANCY, CHILDBIRTH AND PUERPERIUM (O00-O99)
  # ============================================================================

  pregnancy_complications = list(
    codes = c("O00-O99"),
    label = list(
      pt = "Complicacoes da gravidez",
      en = "Pregnancy complications",
      es = "Complicaciones del embarazo"
    ),
    description = list(
      pt = "Complicacoes da gravidez, parto e puerperio",
      en = "Complications of pregnancy, childbirth, and puerperium",
      es = "Complicaciones del embarazo, parto y puerperio"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "heatwaves")
  ),

  # ============================================================================
  # CHAPTER XVI: PERINATAL PERIOD (P00-P96)
  # ============================================================================

  perinatal = list(
    codes = c("P00-P96"),
    label = list(
      pt = "Afeccoes perinatais",
      en = "Perinatal conditions",
      es = "Afecciones perinatales"
    ),
    description = list(
      pt = "Afeccoes originadas no periodo perinatal",
      en = "Conditions originating in the perinatal period",
      es = "Afecciones originadas en el periodo perinatal"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "heatwaves")
  ),

  # ============================================================================
  # CHAPTER XVII: CONGENITAL MALFORMATIONS (Q00-Q99)
  # ============================================================================

  congenital = list(
    codes = c("Q00-Q99"),
    label = list(
      pt = "Malformacoes congenitas",
      en = "Congenital malformations",
      es = "Malformaciones congenitas"
    ),
    description = list(
      pt = "Malformacoes congenitas, deformidades e anomalias cromossomicas",
      en = "Congenital malformations, deformations, and chromosomal abnormalities",
      es = "Malformaciones congenitas, deformaciones y anomalias cromosomicas"
    ),
    climate_sensitive = FALSE,
    climate_factors = NULL
  ),

  microcephaly = list(
    codes = c("Q02"),
    label = list(
      pt = "Microcefalia",
      en = "Microcephaly",
      es = "Microcefalia"
    ),
    description = list(
      pt = "Microcefalia (associada a Zika)",
      en = "Microcephaly (Zika-associated)",
      es = "Microcefalia (asociada a Zika)"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "precipitation")
  ),

  # ============================================================================
  # CHAPTER XVIII: ILL-DEFINED CONDITIONS (R00-R99)
  # ============================================================================

  ill_defined = list(
    codes = c("R00-R99"),
    label = list(
      pt = "Sintomas mal definidos",
      en = "Ill-defined symptoms",
      es = "Sintomas mal definidos"
    ),
    description = list(
      pt = "Sintomas, sinais e achados anormais",
      en = "Symptoms, signs, and abnormal findings",
      es = "Sintomas, signos y hallazgos anormales"
    ),
    climate_sensitive = FALSE,
    climate_factors = NULL
  ),

  # ============================================================================
  # CHAPTER XIX: INJURIES (S00-T98)
  # ============================================================================

  injuries = list(
    codes = c("S00-T98"),
    label = list(
      pt = "Lesoes e envenenamentos",
      en = "Injuries and poisonings",
      es = "Lesiones y envenenamientos"
    ),
    description = list(
      pt = "Lesoes, envenenamentos e outras causas externas",
      en = "Injuries, poisonings, and other external causes",
      es = "Lesiones, envenenamientos y otras causas externas"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("extreme_weather", "flooding", "heatwaves")
  ),

  # ============================================================================
  # CHAPTER XX: EXTERNAL CAUSES (V01-Y98)
  # ============================================================================

  transport_accidents = list(
    codes = c("V01-V99"),
    label = list(
      pt = "Acidentes de transporte",
      en = "Transport accidents",
      es = "Accidentes de transporte"
    ),
    description = list(
      pt = "Acidentes de transito e transporte",
      en = "Traffic and transport accidents",
      es = "Accidentes de trafico y transporte"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("extreme_weather", "flooding")
  ),

  drowning = list(
    codes = c("W65-W74"),
    label = list(
      pt = "Afogamento",
      en = "Drowning",
      es = "Ahogamiento"
    ),
    description = list(
      pt = "Afogamento e submersao",
      en = "Drowning and submersion",
      es = "Ahogamiento y sumersion"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("flooding", "precipitation")
  ),

  heat_exposure = list(
    codes = c("X30"),
    label = list(
      pt = "Exposicao ao calor",
      en = "Heat exposure",
      es = "Exposicion al calor"
    ),
    description = list(
      pt = "Exposicao ao calor natural excessivo",
      en = "Exposure to excessive natural heat",
      es = "Exposicion a calor natural excesivo"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "heatwaves")
  ),

  cold_exposure = list(
    codes = c("X31"),
    label = list(
      pt = "Exposicao ao frio",
      en = "Cold exposure",
      es = "Exposicion al frio"
    ),
    description = list(
      pt = "Exposicao ao frio natural excessivo",
      en = "Exposure to excessive natural cold",
      es = "Exposicion a frio natural excesivo"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "cold_spells")
  ),

  natural_disasters = list(
    codes = c("X36-X39"),
    label = list(
      pt = "Desastres naturais",
      en = "Natural disasters",
      es = "Desastres naturales"
    ),
    description = list(
      pt = "Vitimas de raios, terremotos, inundacoes, , tempestades",
      en = "Victims of lightning, earthquakes, floods, storms",
      es = "Victimas de rayos, terremotos, inundaciones, tormentas"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("extreme_weather", "precipitation", "flooding")
  ),

  suicide_self_harm = list(
    codes = c("X60-X84", "Y87.0"),
    label = list(
      pt = "Suicidio e autolesao",
      en = "Suicide and self-harm",
      es = "Suicidio y autolesion"
    ),
    description = list(
      pt = "Lesoes autoprovocadas intencionalmente e suicidio (CID-10 capitulo XX)",
      en = "Intentional self-harm and suicide (ICD-10 chapter XX)",
      es = "Lesiones autoinfligidas intencionalmente y suicidio (CID-10 capitulo XX)"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature_extremes", "seasonality", "sunlight_exposure", "heat_waves", "natural_disasters"),
    research_notes = list(
      pt = "Taxas de suicidio mostram correlacao com variacoes sazonais (maior incidencia na primavera), ondas de calor (aumento de impulsividade) e eventos climaticos extremos (perdas economicas e sociais).",
      en = "Suicide rates show correlation with seasonal variations (higher incidence in spring), heat waves (increased impulsivity) and extreme weather events (economic and social losses).",
      es = "Las tasas de suicidio muestran correlacion con variaciones estacionales (mayor incidencia en primavera), olas de calor (aumento de impulsividad) y eventos climaticos extremos (perdidas economicas y sociales)."
    )
  ),

  # ============================================================================
  # SPECIAL CLIMATE-HEALTH GROUPS
  # ============================================================================

  heat_related = list(
    codes = c("E86", "T67", "X30"),
    label = list(
      pt = "Doencas relacionadas ao calor",
      en = "Heat-related illnesses",
      es = "Enfermedades relacionadas con el calor"
    ),
    description = list(
      pt = "Desidratacao, insolacao, exaustao pelo calor",
      en = "Dehydration, heat stroke, heat exhaustion",
      es = "Deshidratacion, insolacion, agotamiento por calor"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "heatwaves")
  ),

  waterborne = list(
    codes = c("A00-A09", "A27"),
    label = list(
      pt = "Doencas de veiculacao hidrica",
      en = "Waterborne diseases",
      es = "Enfermedades de transmision hidrica"
    ),
    description = list(
      pt = "Doencas transmitidas pela agua contaminada",
      en = "Diseases transmitted through contaminated water",
      es = "Enfermedades transmitidas por agua contaminada"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("precipitation", "flooding", "water_quality")
  ),

  air_pollution_related = list(
    codes = c("J20-J22", "J40-J46", "I20-I25", "I60-I69"),
    label = list(
      pt = "Doencas relacionadas a poluicao do ar",
      en = "Air pollution-related diseases",
      es = "Enfermedades relacionadas con la contaminacion del aire"
    ),
    description = list(
      pt = "Doencas respiratorias e cardiovasculares associadas a poluicao",
      en = "Respiratory and cardiovascular diseases associated with pollution",
      es = "Enfermedades respiratorias y cardiovasculares asociadas a la contaminacion"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("air_pollution", "temperature")
  ),

  climate_sensitive_all = list(
    codes = c(
      "A00-A09", "A15-A19", "A20-A28", "A75-A79", "A90-A99",
      "B50-B57", "B65-B83",
      "C43-C44",
      "E10-E14", "E40-E46",
      "I00-I99",
      "J00-J99",
      "L00-L08",
      "N00-N29",
      "O00-O99",
      "P00-P96",
      "S00-T98",
      "V01-Y98"
    ),
    label = list(
      pt = "Todas as doencas sensiveis ao clima",
      en = "All climate-sensitive diseases",
      es = "Todas las enfermedades sensibles al clima"
    ),
    description = list(
      pt = "Conjunto completo de doencas com sensibilidade climatica documentada",
      en = "Complete set of diseases with documented climate sensitivity",
      es = "Conjunto completo de enfermedades con sensibilidad climatica documentada"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "precipitation", "humidity", "air_pollution", "extreme_weather")
  ),

  # ============================================================================
  # AGE-SPECIFIC GROUPS
  # ============================================================================

  pediatric_respiratory = list(
    codes = c("J00-J06", "J09-J18", "J20-J22"),
    label = list(
      pt = "Doencas respiratorias pediatricas",
      en = "Pediatric respiratory diseases",
      es = "Enfermedades respiratorias pediatricas"
    ),
    description = list(
      pt = "Infeccoes respiratorias agudas em criancas",
      en = "Acute respiratory infections in children",
      es = "Infecciones respiratorias agudas en ninos"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "humidity", "air_pollution")
  ),

  elderly_cardiovascular = list(
    codes = c("I20-I25", "I60-I69"),
    label = list(
      pt = "Doencas cardiovasculares em idosos",
      en = "Elderly cardiovascular diseases",
      es = "Enfermedades cardiovasculares en ancianos"
    ),
    description = list(
      pt = "Doencas cardiacas e cerebrovasculares em populacao idosa",
      en = "Heart and cerebrovascular diseases in elderly population",
      es = "Enfermedades cardiacas y cerebrovasculares en poblacion anciana"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "heatwaves", "cold_spells", "air_pollution")
  ),

  # ============================================================================
  # SYNDROMIC GROUPS (for surveillance)
  # ============================================================================

  fever_syndrome = list(
    codes = c("A90-A99", "B50-B54"),
    label = list(
      pt = "Sindrome febril",
      en = "Febrile syndrome",
      es = "Sindrome febril"
    ),
    description = list(
      pt = "Doencas caracterizadas por febre (vigilancia sindromica)",
      en = "Diseases characterized by fever (syndromic surveillance)",
      es = "Enfermedades caracterizadas por fiebre (vigilancia sindromica)"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "precipitation", "humidity")
  ),

  respiratory_syndrome = list(
    codes = c("J00-J22", "J40-J46"),
    label = list(
      pt = "Sindrome respiratoria",
      en = "Respiratory syndrome",
      es = "Sindrome respiratorio"
    ),
    description = list(
      pt = "Doencas respiratorias agudas e cronicas (vigilancia sindromica)",
      en = "Acute and chronic respiratory diseases (syndromic surveillance)",
      es = "Enfermedades respiratorias agudas y cronicas (vigilancia sindromica)"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "air_pollution", "humidity")
  ),

  diarrheal_syndrome = list(
    codes = c("A00-A09"),
    label = list(
      pt = "Sindrome diarreica",
      en = "Diarrheal syndrome",
      es = "Sindrome diarreico"
    ),
    description = list(
      pt = "Doencas diarreicas agudas (vigilancia sindromica)",
      en = "Acute diarrheal diseases (syndromic surveillance)",
      es = "Enfermedades diarreicas agudas (vigilancia sindromica)"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "precipitation", "flooding")
  ),
  
  # ============================================================================
  # NEUROLOGICAL
  # ============================================================================
  
  neurological_disorders = list(
    codes = c("G00-G99"),
    label = list(
      pt = "Doencas neurologicas",
      en = "Neurological disorders",
      es = "Enfermedades neurologicas"
    ),
    description = list(
      pt = "Doencas do sistema nervoso (inclui meningite, epilepsia, enxaqueca)",
      en = "Diseases of the nervous system (includes meningitis, epilepsy, migraine)",
      es = "Enfermedades del sistema nervioso (incluye meningitis, epilepsia, migrana)"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("temperature", "humidity", "seasonality", "weather space")
  )
)



#' Get available disease groups
#'
#' Returns a character vector of all available disease group names.
#'
#' @param climate_sensitive_only Logical. If TRUE, returns only climate-sensitive groups.
#' @param lang Character. Language for sorting ("en", "pt", "es"). Default "en".
#'
#' @return Character vector of disease group names.
#'
#' @keywords internal
#' @noRd
get_available_disease_groups <- function(climate_sensitive_only = FALSE, lang = "en") {
  
  groups <- names(.icd_disease_groups)
  
  if (climate_sensitive_only) {
    groups <- groups[sapply(.icd_disease_groups, function(x) x$climate_sensitive)]
  }
  
  return(sort(groups))
}


#' Get disease group information
#'
#' Returns detailed information about a specific disease group.
#'
#' @param group_name Character. Name of the disease group.
#' @param lang Character. Language for labels ("en", "pt", "es"). Default "en".
#'
#' @return List with group information.
#'
#' @keywords internal
#' @noRd
get_disease_group_info <- function(group_name, lang = "en") {
  
  if (!group_name %in% names(.icd_disease_groups)) {
    stop("Invalid disease group: ", group_name)
  }
  
  group <- .icd_disease_groups[[group_name]]
  
  return(list(
    name = group_name,
    label = group$label[[lang]],
    description = if (!is.null(group$description)) group$description[[lang]] else NULL,
    codes = group$codes,
    climate_sensitive = group$climate_sensitive,
    climate_factors = group$climate_factors
  ))
}