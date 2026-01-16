#' Utilities functions for climasus4r international
#'
#' @keywords internal
#' @name utils-i18n
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


#' Obtem o Dicionario de Traducao para o Censo 2010 - Registro de Pessoas (Portugues)
#'
#' @description
#' Retorna uma lista contendo dicionarios para traduzir os nomes das colunas
#' e os valores das variaveis categoricas do Censo Demografico 2010 (Registro de Pessoas)
#' para o portugues. O objetivo e padronizar e facilitar a analise dos dados.
#'
#' @return Uma lista com dois elementos:
#'   - `columns`: um vetor nomeado para traduzir os nomes das colunas.
#'   - `values`: uma lista de vetores nomeados para decodificar valores categoricos.
#'
#' @keywords internal
#' @noRd
get_census_dictionary_pt_population <- function() {
  
  # ==========================================================================
  # TRADUCAO DOS NOMES DAS COLUNAS (CENSO 2010 - REGISTRO DE PESSOAS)
  # Mapeia os codigos das variaveis do Censo para um formato padronizado em portugues.
  # ==========================================================================
  
  columns <- c(
    # --- Identificacao Geografica e Controle ---
    "V0001" = "unidade_federacao",
    "V0002" = "codigo_municipio",
    "V0011" = "area_ponderacao",
    "V0300" = "controle",
    "V0010" = "peso_amostral",
    "V1001" = "regiao_geografica",
    "V1002" = "codigo_mesorregiao",
    "V1003" = "codigo_microrregiao",
    "V1004" = "codigo_regiao_metropolitana",
    "V1006" = "situacao_domicilio",
    
    # --- Caracteristicas da Pessoa no Domicilio ---
    "V0502" = "relacao_parentesco_responsavel",
    "V0504" = "ordem_logica",
    "V0601" = "sexo",
    "V6033" = "idade_calculada_anos_meses",
    "V6036" = "idade_calculada_anos",
    "V6037" = "idade_calculada_meses",
    "V6040" = "forma_declaracao_idade",
    "V0606" = "cor_raca",
    "V0613" = "registro_nascimento",
    "V0614" = "dificuldade_permanente_enxergar",
    "V0615" = "dificuldade_permanente_ouvir",
    "V0616" = "dificuldade_permanente_caminhar_subir",
    "V0617" = "deficiencia_mental_intelectual",
    "V0618" = "nasceu_municipio",
    "V0619" = "nasceu_unidade_federacao",
    "V0620" = "nacionalidade",
    "V0621" = "ano_fixou_residencia_brasil",
    "V0622" = "uf_pais_estrangeiro_nascimento",
    "V6222" = "codigo_uf_nascimento",
    "V6224" = "codigo_pais_nascimento",
    "V0623" = "tempo_moradia_uf",
    "V0624" = "tempo_moradia_municipio",
    "V0625" = "tipo_residencia_anterior",
    "V6252" = "codigo_uf_residencia_anterior",
    "V6254" = "codigo_municipio_residencia_anterior",
    "V6256" = "codigo_pais_residencia_anterior",
    "V0626" = "residencia_31_julho_2005",
    "V6262" = "codigo_uf_residencia_2005",
    "V6264" = "codigo_municipio_residencia_2005",
    "V6266" = "codigo_pais_residencia_2005",
    
    # --- Educacao ---
    "V0627" = "sabe_ler_escrever",
    "V0628" = "frequenta_escola_creche",
    "V0629" = "curso_frequenta",
    "V0630" = "serie_ano_frequenta",
    "V0631" = "serie_frequenta",
    "V0632" = "conclusao_outro_curso_superior",
    "V0633" = "curso_mais_elevado_frequentou",
    "V0634" = "conclusao_curso_elevado",
    "V0635" = "especie_curso_mais_elevado_concluido",
    "V6400" = "nivel_instrucao",
    "V6352" = "codigo_curso_superior_graduacao",
    "V6354" = "codigo_curso_mestrado",
    "V6356" = "codigo_curso_doutorado",
    "V0636" = "local_frequencia_escola",
    "V6362" = "codigo_uf_frequencia_escola",
    "V6364" = "codigo_municipio_frequencia_escola",
    "V6366" = "codigo_pais_frequencia_escola",
    
    # --- Relacoes Familiares e Conjugais ---
    "V0637" = "vive_companhia_conjuge",
    "V0638" = "numero_ordem_conjuge_companheiro",
    "V0639" = "natureza_uniao",
    "V0640" = "estado_civil",
    "V0604" = "tem_mae_viva",
    "V0605" = "numero_ordem_mae",
    
    # --- Trabalho e Renda ---
    "V0641" = "trabalhou_semana_referencia",
    "V0642" = "tinha_trabalho_afastado",
    "V0643" = "ajudou_sem_pagamento",
    "V0644" = "trabalhou_agricultura_consumo_proprio",
    "V0645" = "quantos_trabalhos",
    "V6461" = "codigo_ocupacao",
    "V6471" = "codigo_atividade",
    "V0648" = "condicao_trabalho",
    "V0649" = "quantas_pessoas_empregava",
    "V0650" = "contribuinte_previdencia",
    "V0651" = "tipo_rendimento_trabalho_principal",
    "V6511" = "valor_rendimento_bruto_principal",
    "V6513" = "rendimento_trabalho_principal",
    "V6514" = "rendimento_principal_salarios_minimos",
    "V0652" = "tipo_rendimento_outros_trabalhos",
    "V6521" = "valor_rendimento_bruto_outros_trabalhos",
    "V6524" = "rendimento_outros_trabalhos_salarios_minimos",
    "V6525" = "rendimento_todos_trabalhos",
    "V6526" = "rendimento_todos_trabalhos_salarios_minimos",
    "V6527" = "rendimento_mensal_total_julho_2010",
    "V6528" = "rendimento_mensal_total_salarios_minimos",
    "V6529" = "rendimento_domiciliar_julho_2010",
    "V6530" = "rendimento_domiciliar_salarios_minimos",
    "V6531" = "rendimento_domiciliar_per_capita",
    "V6532" = "rendimento_domiciliar_per_capita_salarios_minimos",
    "V0653" = "horas_trabalho_semanal",
    "V0654" = "procurou_trabalho_julho_2010",
    "V0655" = "disponivel_assumir_trabalho",
    "V0656" = "rendimento_aposentadoria_pensao",
    "V0657" = "rendimento_bolsa_familia_peti",
    "V0658" = "rendimento_outros_programas_sociais",
    "V0659" = "rendimento_outras_fontes",
    "V6591" = "valor_total_outros_rendimentos",
    "V0660" = "local_trabalho",
    "V6602" = "codigo_uf_trabalho",
    "V6604" = "codigo_municipio_trabalho",
    "V6606" = "codigo_pais_trabalho",
    "V0661" = "retorna_trabalho_casa_diariamente",
    "V0662" = "tempo_deslocamento_casa_trabalho",
    
    # --- Fecundidade ---
    "V0663" = "teve_filhos_nascidos_vivos",
    "V6631" = "quantos_filhos_nascidos_vivos",
    "V6632" = "quantas_filhas_nascidas_vivas",
    "V6633" = "total_filhos_nascidos_vivos",
    "V0664" = "filhos_estavam_vivos_julho_2010",
    "V6641" = "quantos_filhos_vivos",
    "V6642" = "quantas_filhas_vivas",
    "V6643" = "total_filhos_vivos",
    "V0665" = "sexo_ultimo_filho",
    "V6660" = "idade_ultimo_filho",
    "V6664" = "filho_nascido_vivo_12_meses_anteriores",
    "V0667" = "este_filho_estava_vivo",
    "V0668" = "conhece_mes_ano_filho_faleceu",
    "V6681" = "mes_filho_faleceu",
    "V6682" = "ano_filho_faleceu",
    "V0669" = "teve_filhos_nascidos_mortos",
    "V6691" = "quantos_filhos_nascidos_mortos",
    "V6692" = "quantas_filhas_nascidas_mortas",
    "V6693" = "total_filhos_nascidos_mortos",
    "V6800" = "total_filhos_nascidos_vivos_mortos",
    
    # --- Informacoes da Coleta ---
    "V0670" = "quem_prestou_informacoes",
    "V0671" = "numero_ordem_informante",
    
    # --- Variaveis Auxiliares e Construidas ---
    "V6900" = "condicao_atividade_semana_referencia",
    "V6910" = "condicao_ocupacao_semana_referencia",
    "V6920" = "situacao_ocupacao_semana_referencia",
    "V6930" = "posicao_ocupacao_categoria_emprego",
    "V6940" = "subgrupo_categoria_emprego",
    "V6121" = "codigo_religiao",
    
    # --- Informacoes Familiares ---
    "V5020" = "numero_familia",
    "V5060" = "numero_pessoas_familia",
    "V5070" = "rendimento_familiar_per_capita",
    "V5080" = "rendimento_familiar_per_capita_salarios_minimos",
    "V5030" = "tipo_unidade_domestica",
    "V5040" = "indicadora_familia",
    "V5090" = "tipo_composicao_familiar_principal",
    "V5100" = "tipo_composicao_familiar_secundaria",
    "V5130" = "ordem_logica_familia",
    "V5110" = "condicao_contribuicao_previdencia_principal",
    "V5120" = "condicao_contribuicao_previdencia_qualquer_trabalho",
    
    # --- Marcas de Imputacao (variaveis M) ---
    "M0502" = "marca_imputacao_V0502",
    "M0601" = "marca_imputacao_V0601",
    "M6033" = "marca_imputacao_V6033",
    "M0606" = "marca_imputacao_V0606",
    "M0613" = "marca_imputacao_V0613",
    "M0614" = "marca_imputacao_V0614",
    "M0615" = "marca_imputacao_V0615",
    "M0616" = "marca_imputacao_V0616",
    "M0617" = "marca_imputacao_V0617",
    "M0618" = "marca_imputacao_V0618",
    "M0619" = "marca_imputacao_V0619",
    "M0620" = "marca_imputacao_V0620",
    "M0621" = "marca_imputacao_V0621",
    "M0622" = "marca_imputacao_V0622",
    "M6222" = "marca_imputacao_V6222",
    "M6224" = "marca_imputacao_V6224",
    "M0623" = "marca_imputacao_V0623",
    "M0624" = "marca_imputacao_V0624",
    "M0625" = "marca_imputacao_V0625",
    "M6252" = "marca_imputacao_V6252",
    "M6254" = "marca_imputacao_V6254",
    "M6256" = "marca_imputacao_V6256",
    "M0626" = "marca_imputacao_V0626",
    "M6262" = "marca_imputacao_V6262",
    "M6264" = "marca_imputacao_V6264",
    "M6266" = "marca_imputacao_V6266",
    "M0627" = "marca_imputacao_V0627",
    "M0628" = "marca_imputacao_V0628",
    "M0629" = "marca_imputacao_V0629",
    "M0630" = "marca_imputacao_V0630",
    "M0631" = "marca_imputacao_V0631",
    "M0632" = "marca_imputacao_V0632",
    "M0633" = "marca_imputacao_V0633",
    "M0634" = "marca_imputacao_V0634",
    "M0635" = "marca_imputacao_V0635",
    "M6352" = "marca_imputacao_V6352",
    "M6354" = "marca_imputacao_V6354",
    "M6356" = "marca_imputacao_V6356",
    "M0636" = "marca_imputacao_V0636",
    "M6362" = "marca_imputacao_V6362",
    "M6364" = "marca_imputacao_V6364",
    "M6366" = "marca_imputacao_V6366",
    "M0637" = "marca_imputacao_V0637",
    "M0638" = "marca_imputacao_V0638",
    "M0639" = "marca_imputacao_V0639",
    "M0640" = "marca_imputacao_V0640",
    "M0641" = "marca_imputacao_V0641",
    "M0642" = "marca_imputacao_V0642",
    "M0643" = "marca_imputacao_V0643",
    "M0644" = "marca_imputacao_V0644"
  )
  
  # ==========================================================================
  # TRADUCAO DE VALORES CATEGORICOS (CENSO 2010 - REGISTRO DE PESSOAS)
  # Decodifica os codigos para rotulos legiveis em portugues sem acentos.
  # ==========================================================================
  
  values <- list(
    "V0001" = c(
      "11" = "Rondonia", "12" = "Acre", "13" = "Amazonas",
      "14" = "Roraima", "15" = "Para", "16" = "Amapa",
      "17" = "Tocantins", "21" = "Maranhao", "22" = "Piaui",
      "23" = "Ceara", "24" = "Rio Grande do Norte", "25" = "Paraiba",
      "26" = "Pernambuco", "27" = "Alagoas", "28" = "Sergipe",
      "29" = "Bahia", "31" = "Minas Gerais", "32" = "Espirito Santo",
      "33" = "Rio de Janeiro", "35" = "Sao Paulo", "41" = "Parana",
      "42" = "Santa Catarina", "43" = "Rio Grande do Sul",
      "50" = "Mato Grosso do Sul", "51" = "Mato Grosso",
      "52" = "Goias", "53" = "Distrito Federal"
    ),
    
    "V1001" = c(
      "1" = "Regiao norte (uf=11 a 17)",
      "2" = "Regiao nordeste (uf=21 a 29)",
      "3" = "Regiao sudeste (uf=31 a 33 e 35)",
      "4" = "Regiao sul (uf=41 a 43)",
      "5" = "Regiao centro-oeste (uf=50 a 53)"
    ),
    
    "V1006" = c(
      "1" = "Urbana",
      "2" = "Rural"
    ),
    
    "V0502" = c(
      "01" = "Pessoa responsavel pelo domicilio",
      "02" = "Conjuge ou companheiro(a) de sexo diferente",
      "03" = "Conjuge ou companheiro(a) do mesmo sexo",
      "04" = "Filho(a) do responsavel e do conjuge",
      "05" = "Filho(a) somente do responsavel",
      "06" = "Enteado(a)",
      "07" = "Genro ou nora",
      "08" = "Pai, mae, padrasto ou madrasta",
      "09" = "Sogro(a)",
      "10" = "Neto(a)",
      "11" = "Bisneto(a)",
      "12" = "Irmao ou irma",
      "13" = "Avo ou avo",
      "14" = "Outro parente",
      "15" = "Agregado(a)",
      "16" = "Convivente",
      "17" = "Pensionista",
      "18" = "Empregado(a) domestico(a)",
      "19" = "Parente do(a) empregado(a) domestico(a)",
      "20" = "Individual em domicilio coletivo"
    ),
    
    "V0601" = c(
      "1" = "Masculino",
      "2" = "Feminino"
    ),
    
    "V6040" = c(
      "1" = "Data de nascimento",
      "2" = "Idade declarada"
    ),
    
    "V0606" = c(
      "1" = "Branca",
      "2" = "Preta",
      "3" = "Amarela",
      "4" = "Parda",
      "5" = "Indigena",
      "9" = "Ignorado"
    ),
    
    "V0613" = c(
      "1" = "Do cartorio",
      "2" = "Declaracao de nascido vivo (DNV) do hospital ou da maternidade",
      "3" = "Registro administrativo de nascimento indigena (RANI)",
      "4" = "Nao tem",
      "5" = "Nao sabe",
      "9" = "Ignorado"
    ),
    
    "V0614" = c(
      "1" = "Sim, nao consegue de modo algum",
      "2" = "Sim, grande dificuldade",
      "3" = "Sim, alguma dificuldade",
      "4" = "Nao, nenhuma dificuldade",
      "9" = "Ignorado"
    ),
    
    "V0615" = c(
      "1" = "Sim, nao consegue de modo algum",
      "2" = "Sim, grande dificuldade",
      "3" = "Sim, alguma dificuldade",
      "4" = "Nao, nenhuma dificuldade",
      "9" = "Ignorado"
    ),
    
    "V0616" = c(
      "1" = "Sim, nao consegue de modo algum",
      "2" = "Sim, grande dificuldade",
      "3" = "Sim, alguma dificuldade",
      "4" = "Nao, nenhuma dificuldade",
      "9" = "Ignorado"
    ),
    
    "V0617" = c(
      "1" = "Sim",
      "2" = "Nao",
      "9" = "Ignorado"
    ),
    
    "V0618" = c(
      "1" = "Sim e sempre morou",
      "2" = "Sim mas morou em outro municipio ou pais estrangeiro",
      "3" = "Nao"
    ),
    
    "V0619" = c(
      "1" = "Sim, e sempre morou",
      "2" = "Sim, mas morou em outra UF ou pais estrangeiro",
      "3" = "Nao"
    ),
    
    "V0620" = c(
      "1" = "Brasileiro nato",
      "2" = "Naturalizado brasileiro",
      "3" = "Estrangeiro"
    ),
    
    "V0622" = c(
      "1" = "UF",
      "2" = "Pais estrangeiro"
    ),
    
    "V0625" = c(
      "1" = "UF/Municipio",
      "2" = "Pais estrangeiro"
    ),
    
    "V0626" = c(
      "1" = "UF/Municipio",
      "2" = "Pais estrangeiro"
    ),
    
    "V0627" = c(
      "1" = "Sim",
      "2" = "Nao"
    ),
    
    "V0628" = c(
      "1" = "Sim, publica",
      "2" = "Sim, particular",
      "3" = "Nao, ja frequentou",
      "4" = "Nao, nunca frequentou"
    ),
    
    "V0629" = c(
      "01" = "Creche",
      "02" = "Pre-escolar (maternal e jardim da infancia)",
      "03" = "Classe de alfabetizacao - CA",
      "04" = "Alfabetizacao de jovens e adultos",
      "05" = "Regular do ensino fundamental",
      "06" = "Educacao de jovens e adultos - EJA - ou supletivo do ensino fundamental",
      "07" = "Regular do ensino medio",
      "08" = "Educacao de jovens e adultos - EJA - ou supletivo do ensino medio",
      "09" = "Superior de graduacao",
      "10" = "Especializacao de nivel superior (minimo de 360 horas)",
      "11" = "Mestrado",
      "12" = "Doutorado"
    ),
    
    "V0630" = c(
      "01" = "Primeiro ano",
      "02" = "Primeira serie / Segundo ano",
      "03" = "Segunda serie / Terceiro ano",
      "04" = "Terceira serie / Quarto ano",
      "05" = "Quarta serie / Quinto ano",
      "06" = "Quinta serie / Sexto ano",
      "07" = "Sexta serie / Setimo ano",
      "08" = "Setima serie / Oitavo ano",
      "09" = "Oitava serie / Nono ano",
      "10" = "Nao seriado"
    ),
    
    "V0631" = c(
      "1" = "Primeira serie",
      "2" = "Segunda serie",
      "3" = "Terceira serie",
      "4" = "Quarta serie",
      "5" = "Nao seriado"
    ),
    
    "V0632" = c(
      "1" = "Sim",
      "2" = "Nao"
    ),
    
    "V0633" = c(
      "01" = "Creche, pre-escolar (maternal e jardim de infancia), classe de alfabetizacao - CA",
      "02" = "Alfabetizacao de jovens e adultos",
      "03" = "Antigo primario (elementar)",
      "04" = "Antigo ginasio (medio 1o ciclo)",
      "05" = "Ensino fundamental ou 1o grau (da 1a a 3a serie/ do 1o ao 4o ano)",
      "06" = "Ensino fundamental ou 1o grau (4a serie/ 5o ano)",
      "07" = "Ensino fundamental ou 1o grau (da 5a a 8a serie/ 6o ao 9o ano)",
      "08" = "Supletivo do ensino fundamental ou do 1o grau",
      "09" = "Antigo cientifico, classico, etc. (medio 2o ciclo)",
      "10" = "Regular ou supletivo do ensino medio ou do 2o grau",
      "11" = "Superior de graduacao",
      "12" = "Especializacao de nivel superior (minimo de 360 horas)",
      "13" = "Mestrado",
      "14" = "Doutorado"
    ),
    
    "V0634" = c(
      "1" = "Sim",
      "2" = "Nao"
    ),
    
    "V0635" = c(
      "1" = "Superior de graduacao",
      "2" = "Mestrado",
      "3" = "Doutorado"
    ),
    
    "V6400" = c(
      "1" = "Sem instrucao e fundamental incompleto",
      "2" = "Fundamental completo e medio incompleto",
      "3" = "Medio completo e superior incompleto",
      "4" = "Superior completo",
      "5" = "Nao determinado"
    ),
    
    "V0636" = c(
      "1" = "Neste municipio",
      "2" = "Em outro municipio",
      "3" = "Em pais estrangeiro"
    ),
    
    "V0637" = c(
      "1" = "Sim",
      "2" = "Nao, mas viveu",
      "3" = "Nao, nunca viveu"
    ),
    
    "V0639" = c(
      "1" = "Casamento civil e religioso",
      "2" = "So casamento civil",
      "3" = "So casamento religioso",
      "4" = "Uniao consensual"
    ),
    
    "V0640" = c(
      "1" = "Casado(a)",
      "2" = "Desquitado(a) ou separado(a) judicialmente",
      "3" = "Divorciado(a)",
      "4" = "Viuvo(a)",
      "5" = "Solteiro(a)"
    ),
    
    "V0641" = c(
      "1" = "Sim",
      "2" = "Nao"
    ),
    
    "V0642" = c(
      "1" = "Sim",
      "2" = "Nao"
    ),
    
    "V0643" = c(
      "1" = "Sim",
      "2" = "Nao"
    ),
    
    "V0644" = c(
      "1" = "Sim",
      "2" = "Nao"
    ),
    
    "V0645" = c(
      "1" = "Um",
      "2" = "Dois ou mais"
    ),
    
    "V0648" = c(
      "1" = "Empregado com carteira de trabalho assinada",
      "2" = "Militar do exercito, marinha, aeronautica, policia militar ou corpo de bombeiros",
      "3" = "Empregado pelo regime juridico dos funcionarios publicos",
      "4" = "Empregado sem carteira de trabalho assinada",
      "5" = "Conta propria",
      "6" = "Empregador",
      "7" = "Nao remunerado"
    ),
    
    "V0649" = c(
      "1" = "1 a 5 pessoas",
      "2" = "6 ou mais pessoas"
    ),
    
    "V0650" = c(
      "1" = "Sim, no trabalho principal",
      "2" = "Sim, em outro trabalho",
      "3" = "Nao"
    ),
    
    "V0651" = c(
      "0" = "Nao tem",
      "1" = "Em dinheiro, produtos ou mercadorias",
      "2" = "Somente em beneficios"
    ),
    
    "V0652" = c(
      "0" = "Nao tem",
      "1" = "Em dinheiro, produtos ou mercadorias",
      "2" = "Somente em beneficios"
    ),
    
    "V0654" = c(
      "1" = "Sim",
      "2" = "Nao"
    ),
    
    "V0655" = c(
      "1" = "Sim",
      "2" = "Nao"
    ),
    
    "V0656" = c(
      "0" = "Nao",
      "1" = "Sim",
      "9" = "Ignorado"
    ),
    
    "V0657" = c(
      "0" = "Nao",
      "1" = "Sim",
      "9" = "Ignorado"
    ),
    
    "V0658" = c(
      "0" = "Nao",
      "1" = "Sim",
      "9" = "Ignorado"
    ),
    
    "V0659" = c(
      "0" = "Nao",
      "1" = "Sim",
      "9" = "Ignorado"
    ),
    
    "V0660" = c(
      "1" = "No proprio domicilio",
      "2" = "Apenas neste municipio, mas nao no proprio domicilio",
      "3" = "Em outro municipio",
      "4" = "Em pais estrangeiro",
      "5" = "Em mais de um municipio ou pais"
    ),
    
    "V0661" = c(
      "1" = "Sim",
      "2" = "Nao"
    ),
    
    "V0662" = c(
      "1" = "Ate 05 minutos",
      "2" = "De 06 minutos ate meia hora",
      "3" = "Mais de meia hora ate uma hora",
      "4" = "Mais de uma hora ate duas horas",
      "5" = "Mais de duas horas"
    ),
    
    "V0663" = c(
      "1" = "Teve filhos nascidos vivos",
      "2" = "Nao teve nenhum filho nascido vivo"
    ),
    
    "V0664" = c(
      "1" = "Filhos vivos",
      "2" = "Nao sabe"
    ),
    
    "V0665" = c(
      "1" = "Masculino",
      "2" = "Feminino"
    ),
    
    "V6664" = c(
      "1" = "Sim",
      "0" = "Nao"
    ),
    
    "V0667" = c(
      "1" = "Sim",
      "2" = "Nao",
      "9" = "Nao sabe"
    ),
    
    "V0668" = c(
      "1" = "Sabe o mes e ano ou somente o ano",
      "2" = "Nao sabe"
    ),
    
    "V0669" = c(
      "1" = "Teve filho nascido morto",
      "2" = "Nao teve filho nascido morto",
      "3" = "Nao sabe"
    ),
    
    "V0670" = c(
      "1" = "A propria pessoa",
      "2" = "Outro morador",
      "3" = "Nao morador",
      "9" = "Ignorado"
    ),
    
    "V6900" = c(
      "1" = "Economicamente ativas",
      "2" = "Nao economicamente ativas"
    ),
    
    "V6910" = c(
      "1" = "Ocupadas",
      "2" = "Desocupadas"
    ),
    
    "V6920" = c(
      "1" = "Ocupadas",
      "2" = "Nao ocupadas"
    ),
    
    "V6930" = c(
      "1" = "Empregados com carteira de trabalho assinada",
      "2" = "Militares e funcionarios publicos estatutarios",
      "3" = "Empregados sem carteira de trabalho assinada",
      "4" = "Conta propria",
      "5" = "Empregadores",
      "6" = "Nao remunerados",
      "7" = "Trabalhadores na producao para o proprio consumo"
    ),
    
    "V6940" = c(
      "1" = "Trabalhadores domesticos com carteira de trabalho assinada",
      "2" = "Trabalhadores domesticos sem carteira de trabalho assinada",
      "3" = "Demais empregados com carteira de trabalho assinada",
      "4" = "Militares e funcionarios publicos estatutarios",
      "5" = "Demais empregados sem carteira de trabalho assinada"
    ),
    
    "V0604" = c(
      "1" = "Sim e mora neste domicilio",
      "2" = "Sim e mora em outro domicilio",
      "3" = "Nao",
      "4" = "Nao sabe",
      "9" = "Ignorado"
    ),
    
    "V5020" = c(
      "01" = "Familias unicas ou conviventes principais",
      "02" = "Familia convivente - segunda",
      "03" = "Familia convivente - terceira",
      "04" = "Familia convivente - quarta",
      "05" = "Familia convivente - quinta",
      "06" = "Familia convivente - sexta",
      "07" = "Familia convivente - setima",
      "08" = "Familia convivente - oitava",
      "09" = "Familia convivente - nona"
    ),
    
    "V5030" = c(
      "1" = "Unipessoal",
      "2" = "Duas pessoas ou mais sem parentesco",
      "3" = "Duas pessoas ou mais com parentesco"
    ),
    
    "V5040" = c(
      "1" = "Arranjo familiar",
      "0" = "Arranjo nao familiar"
    ),
    
    "V5090" = c(
      "1" = "Casal sem filho(s)",
      "2" = "Casal sem filho(s) e com parente(s)",
      "3" = "Casal com filho(s)",
      "4" = "Casal com filho(s) e com parente(s)",
      "5" = "Mulher sem conjuge com filho(s)",
      "6" = "Mulher sem conjuge com filho(s) e com parente(s)",
      "7" = "Homem sem conjuge com filho(s)",
      "8" = "Homem sem conjuge com filho(s) e com parente(s)",
      "9" = "Outro"
    ),
    
    "V5100" = c(
      "1" = "Casal sem filho(s)",
      "2" = "Casal com filho(s)",
      "3" = "Mulher sem conjuge com filho(s)"
    ),
    
    "V5110" = c(
      "Contribuintes" = "Contribuintes",
      "Nao contribuintes" = "Nao contribuintes"
    ),
    
    "V5120" = c(
      "Contribuintes" = "Contribuintes",
      "Nao contribuintes" = "Nao contribuintes"
    ),
    
    # Marcas de Imputacao (variaveis M)
    "M0502" = c("1" = "Sim", "2" = "Nao"),
    "M0601" = c("1" = "Sim", "2" = "Nao"),
    "M6033" = c("1" = "Sim", "2" = "Nao"),
    "M0606" = c("1" = "Sim", "2" = "Nao"),
    "M0613" = c("1" = "Sim", "2" = "Nao"),
    "M0614" = c("1" = "Sim", "2" = "Nao"),
    "M0615" = c("1" = "Sim", "2" = "Nao"),
    "M0616" = c("1" = "Sim", "2" = "Nao"),
    "M0617" = c("1" = "Sim", "2" = "Nao"),
    "M0618" = c("1" = "Sim", "2" = "Nao"),
    "M0619" = c("1" = "Sim", "2" = "Nao"),
    "M0620" = c("1" = "Sim", "2" = "Nao"),
    "M0621" = c("1" = "Sim", "2" = "Nao"),
    "M0622" = c("1" = "Sim", "2" = "Nao"),
    "M6222" = c("1" = "Sim", "2" = "Nao"),
    "M6224" = c("1" = "Sim", "2" = "Nao"),
    "M0623" = c("1" = "Sim", "2" = "Nao"),
    "M0624" = c("1" = "Sim", "2" = "Nao"),
    "M0625" = c("1" = "Sim", "2" = "Nao"),
    "M6252" = c("1" = "Sim", "2" = "Nao"),
    "M6254" = c("1" = "Sim", "2" = "Nao"),
    "M6256" = c("1" = "Sim", "2" = "Nao"),
    "M0626" = c("1" = "Sim", "2" = "Nao"),
    "M6262" = c("1" = "Sim", "2" = "Nao"),
    "M6264" = c("1" = "Sim", "2" = "Nao"),
    "M6266" = c("1" = "Sim", "2" = "Nao"),
    "M0627" = c("1" = "Sim", "2" = "Nao"),
    "M0628" = c("1" = "Sim", "2" = "Nao"),
    "M0629" = c("1" = "Sim", "2" = "Nao"),
    "M0630" = c("1" = "Sim", "2" = "Nao"),
    "M0631" = c("1" = "Sim", "2" = "Nao"),
    "M0632" = c("1" = "Sim", "2" = "Nao"),
    "M0633" = c("1" = "Sim", "2" = "Nao"),
    "M0634" = c("1" = "Sim", "2" = "Nao"),
    "M0635" = c("1" = "Sim", "2" = "Nao"),
    "M6352" = c("1" = "Sim", "2" = "Nao"),
    "M6354" = c("1" = "Sim", "2" = "Nao"),
    "M6356" = c("1" = "Sim", "2" = "Nao"),
    "M0636" = c("1" = "Sim", "2" = "Nao"),
    "M6362" = c("1" = "Sim", "2" = "Nao"),
    "M6364" = c("1" = "Sim", "2" = "Nao"),
    "M6366" = c("1" = "Sim", "2" = "Nao"),
    "M0637" = c("1" = "Sim", "2" = "Nao"),
    "M0638" = c("1" = "Sim", "2" = "Nao"),
    "M0639" = c("1" = "Sim", "2" = "Nao"),
    "M0640" = c("1" = "Sim", "2" = "Nao"),
    "M0641" = c("1" = "Sim", "2" = "Nao"),
    "M0642" = c("1" = "Sim", "2" = "Nao"),
    "M0643" = c("1" = "Sim", "2" = "Nao"),
    "M0644" = c("1" = "Sim", "2" = "Nao")
  )
  
  return(list(columns = columns, values = values))
}

#' Get Translation Dictionary for 2010 Census - Population Record (English)
#'
#' @description
#' Returns a list containing dictionaries to translate column names
#' and categorical variable values from the 2010 Demographic Census (Population Record)
#' to English. The goal is to standardize and facilitate data analysis.
#'
#' @return A list with two elements:
#'   - `columns`: a named vector for translating column names.
#'   - `values`: a list of named vectors for decoding categorical values.
#'
#' @keywords internal
#' @noRd
get_census_dictionary_en_population <- function() {
  
  # ==========================================================================
  # COLUMN NAME TRANSLATION (CENSUS 2010 - POPULATION RECORD)
  # Maps Census variable codes to standardized English names.
  # ==========================================================================
  
  columns <- c(
    # --- Geographic Identification and Control ---
    "V0001" = "federal_unit",
    "V0002" = "municipality_code",
    "V0011" = "weighting_area",
    "V0300" = "control",
    "V0010" = "sample_weight",
    "V1001" = "geographic_region",
    "V1002" = "mesoregion_code",
    "V1003" = "microregion_code",
    "V1004" = "metropolitan_region_code",
    "V1006" = "household_situation",
    
    # --- Household Person Characteristics ---
    "V0502" = "relationship_to_household_head",
    "V0504" = "logical_order",
    "V0601" = "sex",
    "V6033" = "calculated_age_years_months",
    "V6036" = "calculated_age_years",
    "V6037" = "calculated_age_months",
    "V6040" = "age_declaration_method",
    "V0606" = "race_color",
    "V0613" = "birth_registration",
    "V0614" = "permanent_seeing_difficulty",
    "V0615" = "permanent_hearing_difficulty",
    "V0616" = "permanent_walking_climbing_difficulty",
    "V0617" = "mental_intellectual_disability",
    "V0618" = "born_in_municipality",
    "V0619" = "born_in_federal_unit",
    "V0620" = "nationality",
    "V0621" = "year_settled_in_brazil",
    "V0622" = "uf_foreign_country_birth",
    "V6222" = "uf_birth_code",
    "V6224" = "country_birth_code",
    "V0623" = "residence_time_uf",
    "V0624" = "residence_time_municipality",
    "V0625" = "previous_residence_type",
    "V6252" = "previous_uf_residence_code",
    "V6254" = "previous_municipality_residence_code",
    "V6256" = "previous_country_residence_code",
    "V0626" = "residence_july_31_2005",
    "V6262" = "uf_residence_2005_code",
    "V6264" = "municipality_residence_2005_code",
    "V6266" = "country_residence_2005_code",
    
    # --- Education ---
    "V0627" = "can_read_write",
    "V0628" = "attends_school_daycare",
    "V0629" = "course_attending",
    "V0630" = "grade_year_attending",
    "V0631" = "grade_attending",
    "V0632" = "completed_other_higher_education",
    "V0633" = "highest_course_attended",
    "V0634" = "completed_highest_course",
    "V0635" = "type_highest_completed_course",
    "V6400" = "education_level",
    "V6352" = "higher_education_course_code",
    "V6354" = "masters_course_code",
    "V6356" = "doctorate_course_code",
    "V0636" = "school_attendance_location",
    "V6362" = "school_attendance_uf_code",
    "V6364" = "school_attendance_municipality_code",
    "V6366" = "school_attendance_country_code",
    
    # --- Family and Marital Relations ---
    "V0637" = "lives_with_spouse_partner",
    "V0638" = "spouse_partner_order_number",
    "V0639" = "union_type",
    "V0640" = "marital_status",
    "V0604" = "has_living_mother",
    "V0605" = "mother_order_number",
    
    # --- Work and Income ---
    "V0641" = "worked_reference_week",
    "V0642" = "had_job_temporarily_away",
    "V0643" = "helped_without_payment",
    "V0644" = "worked_agriculture_self_consumption",
    "V0645" = "how_many_jobs",
    "V6461" = "occupation_code",
    "V6471" = "activity_code",
    "V0648" = "employment_status",
    "V0649" = "how_many_people_employed",
    "V0650" = "social_security_contributor",
    "V0651" = "main_job_income_type",
    "V6511" = "main_job_gross_income_value",
    "V6513" = "main_job_income",
    "V6514" = "main_job_income_minimum_wages",
    "V0652" = "other_jobs_income_type",
    "V6521" = "other_jobs_gross_income_value",
    "V6524" = "other_jobs_income_minimum_wages",
    "V6525" = "all_jobs_income",
    "V6526" = "all_jobs_income_minimum_wages",
    "V6527" = "total_monthly_income_july_2010",
    "V6528" = "total_monthly_income_minimum_wages",
    "V6529" = "household_income_july_2010",
    "V6530" = "household_income_minimum_wages",
    "V6531" = "household_income_per_capita",
    "V6532" = "household_income_per_capita_minimum_wages",
    "V0653" = "weekly_work_hours",
    "V0654" = "sought_work_july_2010",
    "V0655" = "available_to_take_job",
    "V0656" = "pension_retirement_income",
    "V0657" = "bolsa_familia_peti_income",
    "V0658" = "other_social_programs_income",
    "V0659" = "other_sources_income",
    "V6591" = "total_other_income_value",
    "V0660" = "work_location",
    "V6602" = "work_uf_code",
    "V6604" = "work_municipality_code",
    "V6606" = "work_country_code",
    "V0661" = "returns_work_home_daily",
    "V0662" = "commute_time_home_work",
    
    # --- Fertility ---
    "V0663" = "had_live_births",
    "V6631" = "how_many_sons_born_alive",
    "V6632" = "how_many_daughters_born_alive",
    "V6633" = "total_children_born_alive",
    "V0664" = "children_alive_july_2010",
    "V6641" = "how_many_sons_alive",
    "V6642" = "how_many_daughters_alive",
    "V6643" = "total_children_alive",
    "V0665" = "last_child_sex",
    "V6660" = "last_child_age",
    "V6664" = "child_born_alive_12_months_prior",
    "V0667" = "this_child_was_alive",
    "V0668" = "knows_month_year_child_died",
    "V6681" = "month_child_died",
    "V6682" = "year_child_died",
    "V0669" = "had_stillbirths",
    "V6691" = "how_many_sons_stillborn",
    "V6692" = "how_many_daughters_stillborn",
    "V6693" = "total_children_stillborn",
    "V6800" = "total_children_born_alive_stillborn",
    
    # --- Collection Information ---
    "V0670" = "who_provided_information",
    "V0671" = "informant_order_number",
    
    # --- Auxiliary and Constructed Variables ---
    "V6900" = "economic_activity_status_reference_week",
    "V6910" = "employment_status_reference_week",
    "V6920" = "occupation_status_reference_week",
    "V6930" = "occupation_position_employment_category",
    "V6940" = "subgroup_employment_category",
    "V6121" = "religion_code",
    
    # --- Family Information ---
    "V5020" = "family_number",
    "V5060" = "number_people_family",
    "V5070" = "family_income_per_capita",
    "V5080" = "family_income_per_capita_minimum_wages",
    "V5030" = "household_unit_type",
    "V5040" = "family_arrangement_indicator",
    "V5090" = "main_family_composition_type",
    "V5100" = "secondary_family_composition_type",
    "V5130" = "logical_order_family",
    "V5110" = "social_security_contribution_main_job",
    "V5120" = "social_security_contribution_any_job",
    
    # --- Imputation Flags (M variables) ---
    "M0502" = "imputation_flag_V0502",
    "M0601" = "imputation_flag_V0601",
    "M6033" = "imputation_flag_V6033",
    "M0606" = "imputation_flag_V0606",
    "M0613" = "imputation_flag_V0613",
    "M0614" = "imputation_flag_V0614",
    "M0615" = "imputation_flag_V0615",
    "M0616" = "imputation_flag_V0616",
    "M0617" = "imputation_flag_V0617",
    "M0618" = "imputation_flag_V0618",
    "M0619" = "imputation_flag_V0619",
    "M0620" = "imputation_flag_V0620",
    "M0621" = "imputation_flag_V0621",
    "M0622" = "imputation_flag_V0622",
    "M6222" = "imputation_flag_V6222",
    "M6224" = "imputation_flag_V6224",
    "M0623" = "imputation_flag_V0623",
    "M0624" = "imputation_flag_V0624",
    "M0625" = "imputation_flag_V0625",
    "M6252" = "imputation_flag_V6252",
    "M6254" = "imputation_flag_V6254",
    "M6256" = "imputation_flag_V6256",
    "M0626" = "imputation_flag_V0626",
    "M6262" = "imputation_flag_V6262",
    "M6264" = "imputation_flag_V6264",
    "M6266" = "imputation_flag_V6266",
    "M0627" = "imputation_flag_V0627",
    "M0628" = "imputation_flag_V0628",
    "M0629" = "imputation_flag_V0629",
    "M0630" = "imputation_flag_V0630",
    "M0631" = "imputation_flag_V0631",
    "M0632" = "imputation_flag_V0632",
    "M0633" = "imputation_flag_V0633",
    "M0634" = "imputation_flag_V0634",
    "M0635" = "imputation_flag_V0635",
    "M6352" = "imputation_flag_V6352",
    "M6354" = "imputation_flag_V6354",
    "M6356" = "imputation_flag_V6356",
    "M0636" = "imputation_flag_V0636",
    "M6362" = "imputation_flag_V6362",
    "M6364" = "imputation_flag_V6364",
    "M6366" = "imputation_flag_V6366",
    "M0637" = "imputation_flag_V0637",
    "M0638" = "imputation_flag_V0638",
    "M0639" = "imputation_flag_V0639",
    "M0640" = "imputation_flag_V0640",
    "M0641" = "imputation_flag_V0641",
    "M0642" = "imputation_flag_V0642",
    "M0643" = "imputation_flag_V0643",
    "M0644" = "imputation_flag_V0644"
  )
  
  # ==========================================================================
  # CATEGORICAL VALUES TRANSLATION (CENSUS 2010 - POPULATION RECORD)
  # Decodes numeric codes to readable English labels.
  # ==========================================================================
  
  values <- list(
    "V0001" = c(
      "11" = "Rondonia", "12" = "Acre", "13" = "Amazonas",
      "14" = "Roraima", "15" = "Para", "16" = "Amapa",
      "17" = "Tocantins", "21" = "Maranhao", "22" = "Piaui",
      "23" = "Ceara", "24" = "Rio Grande do Norte", "25" = "Paraiba",
      "26" = "Pernambuco", "27" = "Alagoas", "28" = "Sergipe",
      "29" = "Bahia", "31" = "Minas Gerais", "32" = "Espirito Santo",
      "33" = "Rio de Janeiro", "35" = "Sao Paulo", "41" = "Parana",
      "42" = "Santa Catarina", "43" = "Rio Grande do Sul",
      "50" = "Mato Grosso do Sul", "51" = "Mato Grosso",
      "52" = "Goias", "53" = "Distrito Federal"
    ),
    
    "V1001" = c(
      "1" = "North region (states 11 to 17)",
      "2" = "Northeast region (states 21 to 29)",
      "3" = "Southeast region (states 31 to 33 and 35)",
      "4" = "South region (states 41 to 43)",
      "5" = "Central-West region (states 50 to 53)"
    ),
    
    "V1006" = c(
      "1" = "Urban",
      "2" = "Rural"
    ),
    
    "V0502" = c(
      "01" = "Household head",
      "02" = "Spouse or opposite-sex partner",
      "03" = "Spouse or same-sex partner",
      "04" = "Child of head and spouse",
      "05" = "Child of head only",
      "06" = "Stepchild",
      "07" = "Son-in-law or daughter-in-law",
      "08" = "Father, mother, stepfather or stepmother",
      "09" = "Father-in-law or mother-in-law",
      "10" = "Grandchild",
      "11" = "Great-grandchild",
      "12" = "Brother or sister",
      "13" = "Grandfather or grandmother",
      "14" = "Other relative",
      "15" = "Agregado(a) (lodger)",
      "16" = "Convivente (cohabitant)",
      "17" = "Pensioner",
      "18" = "Domestic employee",
      "19" = "Relative of domestic employee",
      "20" = "Individual in collective household"
    ),
    
    "V0601" = c(
      "1" = "Male",
      "2" = "Female"
    ),
    
    "V6040" = c(
      "1" = "Birth date",
      "2" = "Declared age"
    ),
    
    "V0606" = c(
      "1" = "White",
      "2" = "Black",
      "3" = "Yellow (Asian)",
      "4" = "Brown (Mixed)",
      "5" = "Indigenous",
      "9" = "Unknown"
    ),
    
    "V0613" = c(
      "1" = "From registry office",
      "2" = "Live birth declaration (DNV) from hospital or maternity",
      "3" = "Administrative indigenous birth registration (RANI)",
      "4" = "Does not have",
      "5" = "Does not know",
      "9" = "Unknown"
    ),
    
    "V0614" = c(
      "1" = "Yes, cannot at all",
      "2" = "Yes, great difficulty",
      "3" = "Yes, some difficulty",
      "4" = "No, no difficulty",
      "9" = "Unknown"
    ),
    
    "V0615" = c(
      "1" = "Yes, cannot at all",
      "2" = "Yes, great difficulty",
      "3" = "Yes, some difficulty",
      "4" = "No, no difficulty",
      "9" = "Unknown"
    ),
    
    "V0616" = c(
      "1" = "Yes, cannot at all",
      "2" = "Yes, great difficulty",
      "3" = "Yes, some difficulty",
      "4" = "No, no difficulty",
      "9" = "Unknown"
    ),
    
    "V0617" = c(
      "1" = "Yes",
      "2" = "No",
      "9" = "Unknown"
    ),
    
    "V0618" = c(
      "1" = "Yes and always lived here",
      "2" = "Yes but lived in another municipality or foreign country",
      "3" = "No"
    ),
    
    "V0619" = c(
      "1" = "Yes, and always lived here",
      "2" = "Yes, but lived in another state or foreign country",
      "3" = "No"
    ),
    
    "V0620" = c(
      "1" = "Native Brazilian",
      "2" = "Naturalized Brazilian",
      "3" = "Foreigner"
    ),
    
    "V0622" = c(
      "1" = "State (UF)",
      "2" = "Foreign country"
    ),
    
    "V0625" = c(
      "1" = "State/Municipality",
      "2" = "Foreign country"
    ),
    
    "V0626" = c(
      "1" = "State/Municipality",
      "2" = "Foreign country"
    ),
    
    "V0627" = c(
      "1" = "Yes",
      "2" = "No"
    ),
    
    "V0628" = c(
      "1" = "Yes, public",
      "2" = "Yes, private",
      "3" = "No, previously attended",
      "4" = "No, never attended"
    ),
    
    "V0629" = c(
      "01" = "Daycare",
      "02" = "Preschool (nursery and kindergarten)",
      "03" = "Literacy class - CA",
      "04" = "Youth and adult literacy",
      "05" = "Regular elementary school",
      "06" = "Youth and adult education - EJA - or elementary school equivalency",
      "07" = "Regular high school",
      "08" = "Youth and adult education - EJA - or high school equivalency",
      "09" = "Higher education (undergraduate)",
      "10" = "Higher education specialization (minimum 360 hours)",
      "11" = "Master's degree",
      "12" = "Doctorate degree"
    ),
    
    "V0630" = c(
      "01" = "First year",
      "02" = "First grade / Second year",
      "03" = "Second grade / Third year",
      "04" = "Third grade / Fourth year",
      "05" = "Fourth grade / Fifth year",
      "06" = "Fifth grade / Sixth year",
      "07" = "Sixth grade / Seventh year",
      "08" = "Seventh grade / Eighth year",
      "09" = "Eighth grade / Ninth year",
      "10" = "Ungraded"
    ),
    
    "V0631" = c(
      "1" = "First grade",
      "2" = "Second grade",
      "3" = "Third grade",
      "4" = "Fourth grade",
      "5" = "Ungraded"
    ),
    
    "V0632" = c(
      "1" = "Yes",
      "2" = "No"
    ),
    
    "V0633" = c(
      "01" = "Daycare, preschool (nursery and kindergarten), literacy class - CA",
      "02" = "Youth and adult literacy",
      "03" = "Former primary (elementary)",
      "04" = "Former gymnasium (middle school 1st cycle)",
      "05" = "Elementary school or 1st grade (1st to 3rd grade/1st to 4th year)",
      "06" = "Elementary school or 1st grade (4th grade/5th year)",
      "07" = "Elementary school or 1st grade (5th to 8th grade/6th to 9th year)",
      "08" = "Elementary school equivalency or 1st grade equivalency",
      "09" = "Former scientific, classical, etc. (middle school 2nd cycle)",
      "10" = "Regular or equivalency high school or 2nd grade",
      "11" = "Higher education (undergraduate)",
      "12" = "Higher education specialization (minimum 360 hours)",
      "13" = "Master's degree",
      "14" = "Doctorate degree"
    ),
    
    "V0634" = c(
      "1" = "Yes",
      "2" = "No"
    ),
    
    "V0635" = c(
      "1" = "Higher education (undergraduate)",
      "2" = "Master's degree",
      "3" = "Doctorate degree"
    ),
    
    "V6400" = c(
      "1" = "No schooling and incomplete elementary",
      "2" = "Complete elementary and incomplete high school",
      "3" = "Complete high school and incomplete higher education",
      "4" = "Complete higher education",
      "5" = "Undetermined"
    ),
    
    "V0636" = c(
      "1" = "In this municipality",
      "2" = "In another municipality",
      "3" = "In a foreign country"
    ),
    
    "V0637" = c(
      "1" = "Yes",
      "2" = "No, but previously lived with",
      "3" = "No, never lived with"
    ),
    
    "V0639" = c(
      "1" = "Civil and religious marriage",
      "2" = "Civil marriage only",
      "3" = "Religious marriage only",
      "4" = "Consensual union"
    ),
    
    "V0640" = c(
      "1" = "Married",
      "2" = "Legally separated",
      "3" = "Divorced",
      "4" = "Widowed",
      "5" = "Single"
    ),
    
    "V0641" = c(
      "1" = "Yes",
      "2" = "No"
    ),
    
    "V0642" = c(
      "1" = "Yes",
      "2" = "No"
    ),
    
    "V0643" = c(
      "1" = "Yes",
      "2" = "No"
    ),
    
    "V0644" = c(
      "1" = "Yes",
      "2" = "No"
    ),
    
    "V0645" = c(
      "1" = "One",
      "2" = "Two or more"
    ),
    
    "V0648" = c(
      "1" = "Employee with signed work card",
      "2" = "Military (army, navy, air force, military police or fire department)",
      "3" = "Employee under public servant legal regime",
      "4" = "Employee without signed work card",
      "5" = "Self-employed",
      "6" = "Employer",
      "7" = "Unpaid"
    ),
    
    "V0649" = c(
      "1" = "1 to 5 people",
      "2" = "6 or more people"
    ),
    
    "V0650" = c(
      "1" = "Yes, in main job",
      "2" = "Yes, in another job",
      "3" = "No"
    ),
    
    "V0651" = c(
      "0" = "Does not have",
      "1" = "In money, products or goods",
      "2" = "Only in benefits"
    ),
    
    "V0652" = c(
      "0" = "Does not have",
      "1" = "In money, products or goods",
      "2" = "Only in benefits"
    ),
    
    "V0654" = c(
      "1" = "Yes",
      "2" = "No"
    ),
    
    "V0655" = c(
      "1" = "Yes",
      "2" = "No"
    ),
    
    "V0656" = c(
      "0" = "No",
      "1" = "Yes",
      "9" = "Unknown"
    ),
    
    "V0657" = c(
      "0" = "No",
      "1" = "Yes",
      "9" = "Unknown"
    ),
    
    "V0658" = c(
      "0" = "No",
      "1" = "Yes",
      "9" = "Unknown"
    ),
    
    "V0659" = c(
      "0" = "No",
      "1" = "Yes",
      "9" = "Unknown"
    ),
    
    "V0660" = c(
      "1" = "At own home",
      "2" = "Only in this municipality, but not at own home",
      "3" = "In another municipality",
      "4" = "In a foreign country",
      "5" = "In more than one municipality or country"
    ),
    
    "V0661" = c(
      "1" = "Yes",
      "2" = "No"
    ),
    
    "V0662" = c(
      "1" = "Up to 05 minutes",
      "2" = "06 minutes to half hour",
      "3" = "More than half hour to one hour",
      "4" = "More than one hour to two hours",
      "5" = "More than two hours"
    ),
    
    "V0663" = c(
      "1" = "Had live births",
      "2" = "Had no live births"
    ),
    
    "V0664" = c(
      "1" = "Living children",
      "2" = "Does not know"
    ),
    
    "V0665" = c(
      "1" = "Male",
      "2" = "Female"
    ),
    
    "V6664" = c(
      "1" = "Yes",
      "0" = "No"
    ),
    
    "V0667" = c(
      "1" = "Yes",
      "2" = "No",
      "9" = "Does not know"
    ),
    
    "V0668" = c(
      "1" = "Knows month and year or only year",
      "2" = "Does not know"
    ),
    
    "V0669" = c(
      "1" = "Had stillbirth",
      "2" = "Had no stillbirth",
      "3" = "Does not know"
    ),
    
    "V0670" = c(
      "1" = "The person themselves",
      "2" = "Another resident",
      "3" = "Non-resident",
      "9" = "Unknown"
    ),
    
    "V6900" = c(
      "1" = "Economically active",
      "2" = "Not economically active"
    ),
    
    "V6910" = c(
      "1" = "Employed",
      "2" = "Unemployed"
    ),
    
    "V6920" = c(
      "1" = "Occupied",
      "2" = "Not occupied"
    ),
    
    "V6930" = c(
      "1" = "Employees with signed work card",
      "2" = "Military and statutory public servants",
      "3" = "Employees without signed work card",
      "4" = "Self-employed",
      "5" = "Employers",
      "6" = "Unpaid",
      "7" = "Workers in production for own consumption"
    ),
    
    "V6940" = c(
      "1" = "Domestic workers with signed work card",
      "2" = "Domestic workers without signed work card",
      "3" = "Other employees with signed work card",
      "4" = "Military and statutory public servants",
      "5" = "Other employees without signed work card"
    ),
    
    "V0604" = c(
      "1" = "Yes and lives in this household",
      "2" = "Yes and lives in another household",
      "3" = "No",
      "4" = "Does not know",
      "9" = "Unknown"
    ),
    
    "V5020" = c(
      "01" = "Unique or main cohabiting families",
      "02" = "Second cohabiting family",
      "03" = "Third cohabiting family",
      "04" = "Fourth cohabiting family",
      "05" = "Fifth cohabiting family",
      "06" = "Sixth cohabiting family",
      "07" = "Seventh cohabiting family",
      "08" = "Eighth cohabiting family",
      "09" = "Ninth cohabiting family"
    ),
    
    "V5030" = c(
      "1" = "Single-person",
      "2" = "Two or more people without kinship",
      "3" = "Two or more people with kinship"
    ),
    
    "V5040" = c(
      "1" = "Family arrangement",
      "0" = "Non-family arrangement"
    ),
    
    "V5090" = c(
      "1" = "Couple without children",
      "2" = "Couple without children with relative(s)",
      "3" = "Couple with children",
      "4" = "Couple with children with relative(s)",
      "5" = "Woman without spouse with children",
      "6" = "Woman without spouse with children with relative(s)",
      "7" = "Man without spouse with children",
      "8" = "Man without spouse with children with relative(s)",
      "9" = "Other"
    ),
    
    "V5100" = c(
      "1" = "Couple without children",
      "2" = "Couple with children",
      "3" = "Woman without spouse with children"
    ),
    
    "V5110" = c(
      "Contribuintes" = "Contributors",
      "Nao contribuintes" = "Non-contributors"
    ),
    
    "V5120" = c(
      "Contribuintes" = "Contributors",
      "Nao contribuintes" = "Non-contributors"
    ),
    
    # Imputation Flags (M variables)
    "M0502" = c("1" = "Yes", "2" = "No"),
    "M0601" = c("1" = "Yes", "2" = "No"),
    "M6033" = c("1" = "Yes", "2" = "No"),
    "M0606" = c("1" = "Yes", "2" = "No"),
    "M0613" = c("1" = "Yes", "2" = "No"),
    "M0614" = c("1" = "Yes", "2" = "No"),
    "M0615" = c("1" = "Yes", "2" = "No"),
    "M0616" = c("1" = "Yes", "2" = "No"),
    "M0617" = c("1" = "Yes", "2" = "No"),
    "M0618" = c("1" = "Yes", "2" = "No"),
    "M0619" = c("1" = "Yes", "2" = "No"),
    "M0620" = c("1" = "Yes", "2" = "No"),
    "M0621" = c("1" = "Yes", "2" = "No"),
    "M0622" = c("1" = "Yes", "2" = "No"),
    "M6222" = c("1" = "Yes", "2" = "No"),
    "M6224" = c("1" = "Yes", "2" = "No"),
    "M0623" = c("1" = "Yes", "2" = "No"),
    "M0624" = c("1" = "Yes", "2" = "No"),
    "M0625" = c("1" = "Yes", "2" = "No"),
    "M6252" = c("1" = "Yes", "2" = "No"),
    "M6254" = c("1" = "Yes", "2" = "No"),
    "M6256" = c("1" = "Yes", "2" = "No"),
    "M0626" = c("1" = "Yes", "2" = "No"),
    "M6262" = c("1" = "Yes", "2" = "No"),
    "M6264" = c("1" = "Yes", "2" = "No"),
    "M6266" = c("1" = "Yes", "2" = "No"),
    "M0627" = c("1" = "Yes", "2" = "No"),
    "M0628" = c("1" = "Yes", "2" = "No"),
    "M0629" = c("1" = "Yes", "2" = "No"),
    "M0630" = c("1" = "Yes", "2" = "No"),
    "M0631" = c("1" = "Yes", "2" = "No"),
    "M0632" = c("1" = "Yes", "2" = "No"),
    "M0633" = c("1" = "Yes", "2" = "No"),
    "M0634" = c("1" = "Yes", "2" = "No"),
    "M0635" = c("1" = "Yes", "2" = "No"),
    "M6352" = c("1" = "Yes", "2" = "No"),
    "M6354" = c("1" = "Yes", "2" = "No"),
    "M6356" = c("1" = "Yes", "2" = "No"),
    "M0636" = c("1" = "Yes", "2" = "No"),
    "M6362" = c("1" = "Yes", "2" = "No"),
    "M6364" = c("1" = "Yes", "2" = "No"),
    "M6366" = c("1" = "Yes", "2" = "No"),
    "M0637" = c("1" = "Yes", "2" = "No"),
    "M0638" = c("1" = "Yes", "2" = "No"),
    "M0639" = c("1" = "Yes", "2" = "No"),
    "M0640" = c("1" = "Yes", "2" = "No"),
    "M0641" = c("1" = "Yes", "2" = "No"),
    "M0642" = c("1" = "Yes", "2" = "No"),
    "M0643" = c("1" = "Yes", "2" = "No"),
    "M0644" = c("1" = "Yes", "2" = "No")
  )
  
  return(list(columns = columns, values = values))
}


#' Obtiene el Diccionario de Traduccion para el Censo 2010 - Registro de Personas (Espanol)
#'
#' @description
#' Retorna una lista conteniendo diccionarios para traducir los nombres de las columnas
#' y los valores de las variables categoricas del Censo Demografico 2010 (Registro de Personas)
#' al espanol. El objetivo es estandarizar y facilitar el analisis de los datos.
#'
#' @return Una lista con dos elementos:
#'   - `columns`: un vector nombrado para traducir los nombres de las columnas.
#'   - `values`: una lista de vectores nombrados para decodificar valores categoricos.
#'
#' @keywords internal
#' @noRd
get_census_dictionary_es_population <- function() {
  
  # ==========================================================================
  # TRADUCCION DE NOMBRES DE COLUMNAS (CENSO 2010 - REGISTRO DE PERSONAS)
  # Mapea los codigos de variables del Censo a un formato estandarizado en espanol.
  # ==========================================================================
  
  columns <- c(
    # --- Identificacion Geografica y Control ---
    "V0001" = "unidad_federativa",
    "V0002" = "codigo_municipio",
    "V0011" = "area_ponderacion",
    "V0300" = "control",
    "V0010" = "peso_muestral",
    "V1001" = "region_geografica",
    "V1002" = "codigo_mesorregion",
    "V1003" = "codigo_microrregion",
    "V1004" = "codigo_region_metropolitana",
    "V1006" = "situacion_domicilio",
    
    # --- Caracteristicas de la Persona en el Domicilio ---
    "V0502" = "relacion_parentesco_responsable",
    "V0504" = "orden_logica",
    "V0601" = "sexo",
    "V6033" = "edad_calculada_anos_meses",
    "V6036" = "edad_calculada_anos",
    "V6037" = "edad_calculada_meses",
    "V6040" = "forma_declaracion_edad",
    "V0606" = "color_raza",
    "V0613" = "registro_nacimiento",
    "V0614" = "dificultad_permanente_ver",
    "V0615" = "dificultad_permanente_oir",
    "V0616" = "dificultad_permanente_caminar_subir",
    "V0617" = "deficiencia_mental_intelectual",
    "V0618" = "nacio_municipio",
    "V0619" = "nacio_unidad_federativa",
    "V0620" = "nacionalidad",
    "V0621" = "ano_establecio_residencia_brasil",
    "V0622" = "uf_pais_extranjero_nacimiento",
    "V6222" = "codigo_uf_nacimiento",
    "V6224" = "codigo_pais_nacimiento",
    "V0623" = "tiempo_residencia_uf",
    "V0624" = "tiempo_residencia_municipio",
    "V0625" = "tipo_residencia_anterior",
    "V6252" = "codigo_uf_residencia_anterior",
    "V6254" = "codigo_municipio_residencia_anterior",
    "V6256" = "codigo_pais_residencia_anterior",
    "V0626" = "residencia_31_julio_2005",
    "V6262" = "codigo_uf_residencia_2005",
    "V6264" = "codigo_municipio_residencia_2005",
    "V6266" = "codigo_pais_residencia_2005",
    
    # --- Educacion ---
    "V0627" = "sabe_leer_escribir",
    "V0628" = "asiste_escuela_guarderia",
    "V0629" = "curso_asiste",
    "V0630" = "serie_ano_asiste",
    "V0631" = "serie_asiste",
    "V0632" = "conclusion_otro_curso_superior",
    "V0633" = "curso_mas_elevado_asistio",
    "V0634" = "conclusion_curso_elevado",
    "V0635" = "especie_curso_mas_elevado_concluido",
    "V6400" = "nivel_instruccion",
    "V6352" = "codigo_curso_superior_grado",
    "V6354" = "codigo_curso_maestria",
    "V6356" = "codigo_curso_doctorado",
    "V0636" = "local_asistencia_escuela",
    "V6362" = "codigo_uf_asistencia_escuela",
    "V6364" = "codigo_municipio_asistencia_escuela",
    "V6366" = "codigo_pais_asistencia_escuela",
    
    # --- Relaciones Familiares y Conyugales ---
    "V0637" = "vive_compania_conyuge",
    "V0638" = "numero_orden_conyuge_companero",
    "V0639" = "naturaleza_union",
    "V0640" = "estado_civil",
    "V0604" = "tiene_madre_viva",
    "V0605" = "numero_orden_madre",
    
    # --- Trabajo e Ingresos ---
    "V0641" = "trabajo_semana_referencia",
    "V0642" = "tenia_trabajo_ausente",
    "V0643" = "ayudo_sin_pago",
    "V0644" = "trabajo_agricultura_consumo_propio",
    "V0645" = "cuantos_trabajos",
    "V6461" = "codigo_ocupacion",
    "V6471" = "codigo_actividad",
    "V0648" = "condicion_trabajo",
    "V0649" = "cuantas_personas_empleaba",
    "V0650" = "contribuyente_prevision_social",
    "V0651" = "tipo_ingreso_trabajo_principal",
    "V6511" = "valor_ingreso_bruto_principal",
    "V6513" = "ingreso_trabajo_principal",
    "V6514" = "ingreso_principal_salarios_minimos",
    "V0652" = "tipo_ingreso_otros_trabajos",
    "V6521" = "valor_ingreso_bruto_otros_trabajos",
    "V6524" = "ingreso_otros_trabajos_salarios_minimos",
    "V6525" = "ingreso_todos_trabajos",
    "V6526" = "ingreso_todos_trabajos_salarios_minimos",
    "V6527" = "ingreso_mensual_total_julio_2010",
    "V6528" = "ingreso_mensual_total_salarios_minimos",
    "V6529" = "ingreso_domiciliario_julio_2010",
    "V6530" = "ingreso_domiciliario_salarios_minimos",
    "V6531" = "ingreso_domiciliario_per_capita",
    "V6532" = "ingreso_domiciliario_per_capita_salarios_minimos",
    "V0653" = "horas_trabajo_semanal",
    "V0654" = "busco_trabajo_julio_2010",
    "V0655" = "disponible_asumir_trabajo",
    "V0656" = "ingreso_jubilacion_pension",
    "V0657" = "ingreso_bolsa_familia_peti",
    "V0658" = "ingreso_otros_programas_sociales",
    "V0659" = "ingreso_otras_fuentes",
    "V6591" = "valor_total_otros_ingresos",
    "V0660" = "local_trabajo",
    "V6602" = "codigo_uf_trabajo",
    "V6604" = "codigo_municipio_trabajo",
    "V6606" = "codigo_pais_trabajo",
    "V0661" = "regresa_trabajo_casa_diariamente",
    "V0662" = "tiempo_desplazamiento_casa_trabajo",
    
    # --- Fecundidad ---
    "V0663" = "tuvo_hijos_nacidos_vivos",
    "V6631" = "cuantos_hijos_nacidos_vivos",
    "V6632" = "cuantas_hijas_nacidas_vivas",
    "V6633" = "total_hijos_nacidos_vivos",
    "V0664" = "hijos_estaban_vivos_julio_2010",
    "V6641" = "cuantos_hijos_vivos",
    "V6642" = "cuantas_hijas_vivas",
    "V6643" = "total_hijos_vivos",
    "V0665" = "sexo_ultimo_hijo",
    "V6660" = "edad_ultimo_hijo",
    "V6664" = "hijo_nacido_vivo_12_meses_anteriores",
    "V0667" = "este_hijo_estaba_vivo",
    "V0668" = "conoce_mes_ano_hijo_fallecio",
    "V6681" = "mes_hijo_fallecio",
    "V6682" = "ano_hijo_fallecio",
    "V0669" = "tuvo_hijos_nacidos_muertos",
    "V6691" = "cuantos_hijos_nacidos_muertos",
    "V6692" = "cuantas_hijas_nacidas_muertas",
    "V6693" = "total_hijos_nacidos_muertos",
    "V6800" = "total_hijos_nacidos_vivos_muertos",
    
    # --- Informacion de la Recoleccion ---
    "V0670" = "quien_proporciono_informacion",
    "V0671" = "numero_orden_informante",
    
    # --- Variables Auxiliares y Construidas ---
    "V6900" = "condicion_actividad_semana_referencia",
    "V6910" = "condicion_ocupacion_semana_referencia",
    "V6920" = "situacion_ocupacion_semana_referencia",
    "V6930" = "posicion_ocupacion_categoria_empleo",
    "V6940" = "subgrupo_categoria_empleo",
    "V6121" = "codigo_religion",
    
    # --- Informacion Familiar ---
    "V5020" = "numero_familia",
    "V5060" = "numero_personas_familia",
    "V5070" = "ingreso_familiar_per_capita",
    "V5080" = "ingreso_familiar_per_capita_salarios_minimos",
    "V5030" = "tipo_unidad_domestica",
    "V5040" = "indicadora_familia",
    "V5090" = "tipo_composicion_familiar_principal",
    "V5100" = "tipo_composicion_familiar_secundaria",
    "V5130" = "orden_logica_familia",
    "V5110" = "condicion_contribucion_prevision_principal",
    "V5120" = "condicion_contribucion_prevision_cualquier_trabajo",
    
    # --- Marcas de Imputacion (variables M) ---
    "M0502" = "marca_imputacion_V0502",
    "M0601" = "marca_imputacion_V0601",
    "M6033" = "marca_imputacion_V6033",
    "M0606" = "marca_imputacion_V0606",
    "M0613" = "marca_imputacion_V0613",
    "M0614" = "marca_imputacion_V0614",
    "M0615" = "marca_imputacion_V0615",
    "M0616" = "marca_imputacion_V0616",
    "M0617" = "marca_imputacion_V0617",
    "M0618" = "marca_imputacion_V0618",
    "M0619" = "marca_imputacion_V0619",
    "M0620" = "marca_imputacion_V0620",
    "M0621" = "marca_imputacion_V0621",
    "M0622" = "marca_imputacion_V0622",
    "M6222" = "marca_imputacion_V6222",
    "M6224" = "marca_imputacion_V6224",
    "M0623" = "marca_imputacion_V0623",
    "M0624" = "marca_imputacion_V0624",
    "M0625" = "marca_imputacion_V0625",
    "M6252" = "marca_imputacion_V6252",
    "M6254" = "marca_imputacion_V6254",
    "M6256" = "marca_imputacion_V6256",
    "M0626" = "marca_imputacion_V0626",
    "M6262" = "marca_imputacion_V6262",
    "M6264" = "marca_imputacion_V6264",
    "M6266" = "marca_imputacion_V6266",
    "M0627" = "marca_imputacion_V0627",
    "M0628" = "marca_imputacion_V0628",
    "M0629" = "marca_imputacion_V0629",
    "M0630" = "marca_imputacion_V0630",
    "M0631" = "marca_imputacion_V0631",
    "M0632" = "marca_imputacion_V0632",
    "M0633" = "marca_imputacion_V0633",
    "M0634" = "marca_imputacion_V0634",
    "M0635" = "marca_imputacion_V0635",
    "M6352" = "marca_imputacion_V6352",
    "M6354" = "marca_imputacion_V6354",
    "M6356" = "marca_imputacion_V6356",
    "M0636" = "marca_imputacion_V0636",
    "M6362" = "marca_imputacion_V6362",
    "M6364" = "marca_imputacion_V6364",
    "M6366" = "marca_imputacion_V6366",
    "M0637" = "marca_imputacion_V0637",
    "M0638" = "marca_imputacion_V0638",
    "M0639" = "marca_imputacion_V0639",
    "M0640" = "marca_imputacion_V0640",
    "M0641" = "marca_imputacion_V0641",
    "M0642" = "marca_imputacion_V0642",
    "M0643" = "marca_imputacion_V0643",
    "M0644" = "marca_imputacion_V0644"
  )
  
  # ==========================================================================
  # TRADUCCION DE VALORES CATEGORICOS (CENSO 2010 - REGISTRO DE PERSONAS)
  # Decodifica los codigos numericos a etiquetas legibles en espanol sin acentos.
  # ==========================================================================
  
  values <- list(
    "V0001" = c(
      "11" = "Rondonia", "12" = "Acre", "13" = "Amazonas",
      "14" = "Roraima", "15" = "Para", "16" = "Amapa",
      "17" = "Tocantins", "21" = "Maranhao", "22" = "Piaui",
      "23" = "Ceara", "24" = "Rio Grande do Norte", "25" = "Paraiba",
      "26" = "Pernambuco", "27" = "Alagoas", "28" = "Sergipe",
      "29" = "Bahia", "31" = "Minas Gerais", "32" = "Espirito Santo",
      "33" = "Rio de Janeiro", "35" = "Sao Paulo", "41" = "Parana",
      "42" = "Santa Catarina", "43" = "Rio Grande do Sul",
      "50" = "Mato Grosso do Sul", "51" = "Mato Grosso",
      "52" = "Goias", "53" = "Distrito Federal"
    ),
    
    "V1001" = c(
      "1" = "Region norte (uf=11 a 17)",
      "2" = "Region noreste (uf=21 a 29)",
      "3" = "Region sureste (uf=31 a 33 y 35)",
      "4" = "Region sur (uf=41 a 43)",
      "5" = "Region centro-oeste (uf=50 a 53)"
    ),
    
    "V1006" = c(
      "1" = "Urbana",
      "2" = "Rural"
    ),
    
    "V0502" = c(
      "01" = "Persona responsable del domicilio",
      "02" = "Conyuge o companero(a) de sexo diferente",
      "03" = "Conyuge o companero(a) del mismo sexo",
      "04" = "Hijo(a) del responsable y del conyuge",
      "05" = "Hijo(a) solamente del responsable",
      "06" = "Hijastro(a)",
      "07" = "Yerno o nuera",
      "08" = "Padre, madre, padrastro o madrastra",
      "09" = "Suegro(a)",
      "10" = "Nieto(a)",
      "11" = "Bisnieto(a)",
      "12" = "Hermano o hermana",
      "13" = "Abuelo o abuela",
      "14" = "Otro pariente",
      "15" = "Agregado(a)",
      "16" = "Conviviente",
      "17" = "Pensionista",
      "18" = "Empleado(a) domestico(a)",
      "19" = "Pariente del(a) empleado(a) domestico(a)",
      "20" = "Individual en domicilio colectivo"
    ),
    
    "V0601" = c(
      "1" = "Masculino",
      "2" = "Femenino"
    ),
    
    "V6040" = c(
      "1" = "Fecha de nacimiento",
      "2" = "Edad declarada"
    ),
    
    "V0606" = c(
      "1" = "Blanca",
      "2" = "Negra",
      "3" = "Amarilla",
      "4" = "Parda",
      "5" = "Indigena",
      "9" = "Ignorado"
    ),
    
    "V0613" = c(
      "1" = "Del registro civil",
      "2" = "Declaracion de nacido vivo (DNV) del hospital o de la maternidad",
      "3" = "Registro administrativo de nacimiento indigena (RANI)",
      "4" = "No tiene",
      "5" = "No sabe",
      "9" = "Ignorado"
    ),
    
    "V0614" = c(
      "1" = "Si, no puede de ningun modo",
      "2" = "Si, gran dificultad",
      "3" = "Si, alguna dificultad",
      "4" = "No, ninguna dificultad",
      "9" = "Ignorado"
    ),
    
    "V0615" = c(
      "1" = "Si, no puede de ningun modo",
      "2" = "Si, gran dificultad",
      "3" = "Si, alguna dificultad",
      "4" = "No, ninguna dificultad",
      "9" = "Ignorado"
    ),
    
    "V0616" = c(
      "1" = "Si, no puede de ningun modo",
      "2" = "Si, gran dificultad",
      "3" = "Si, alguna dificultad",
      "4" = "No, ninguna dificultad",
      "9" = "Ignorado"
    ),
    
    "V0617" = c(
      "1" = "Si",
      "2" = "No",
      "9" = "Ignorado"
    ),
    
    "V0618" = c(
      "1" = "Si y siempre vivio",
      "2" = "Si pero vivio en otro municipio o pais extranjero",
      "3" = "No"
    ),
    
    "V0619" = c(
      "1" = "Si, y siempre vivio",
      "2" = "Si, pero vivio en otra UF o pais extranjero",
      "3" = "No"
    ),
    
    "V0620" = c(
      "1" = "Brasileno nato",
      "2" = "Naturalizado brasileno",
      "3" = "Extranjero"
    ),
    
    "V0622" = c(
      "1" = "UF",
      "2" = "Pais extranjero"
    ),
    
    "V0625" = c(
      "1" = "UF/Municipio",
      "2" = "Pais extranjero"
    ),
    
    "V0626" = c(
      "1" = "UF/Municipio",
      "2" = "Pais extranjero"
    ),
    
    "V0627" = c(
      "1" = "Si",
      "2" = "No"
    ),
    
    "V0628" = c(
      "1" = "Si, publica",
      "2" = "Si, particular",
      "3" = "No, ya asistio",
      "4" = "No, nunca asistio"
    ),
    
    "V0629" = c(
      "01" = "Guarderia",
      "02" = "Preescolar (maternal y jardin de infancia)",
      "03" = "Clase de alfabetizacion - CA",
      "04" = "Alfabetizacion de jovenes y adultos",
      "05" = "Regular de la ensenanza fundamental",
      "06" = "Educacion de jovenes y adultos - EJA - o supletivo de la ensenanza fundamental",
      "07" = "Regular de la ensenanza media",
      "08" = "Educacion de jovenes y adultos - EJA - o supletivo de la ensenanza media",
      "09" = "Superior de grado",
      "10" = "Especializacion de nivel superior (minimo de 360 horas)",
      "11" = "Maestria",
      "12" = "Doctorado"
    ),
    
    "V0630" = c(
      "01" = "Primer ano",
      "02" = "Primera serie / Segundo ano",
      "03" = "Segunda serie / Tercer ano",
      "04" = "Tercera serie / Cuarto ano",
      "05" = "Cuarta serie / Quinto ano",
      "06" = "Quinta serie / Sexto ano",
      "07" = "Sexta serie / Septimo ano",
      "08" = "Septima serie / Octavo ano",
      "09" = "Octava serie / Noveno ano",
      "10" = "No seriado"
    ),
    
    "V0631" = c(
      "1" = "Primera serie",
      "2" = "Segunda serie",
      "3" = "Tercera serie",
      "4" = "Cuarta serie",
      "5" = "No seriado"
    ),
    
    "V0632" = c(
      "1" = "Si",
      "2" = "No"
    ),
    
    "V0633" = c(
      "01" = "Guarderia, preescolar (maternal y jardin de infancia), clase de alfabetizacion - CA",
      "02" = "Alfabetizacion de jovenes y adultos",
      "03" = "Antiguo primario (elemental)",
      "04" = "Antiguo gimnasio (medio 1er ciclo)",
      "05" = "Ensenanza fundamental o 1er grado (de la 1a a la 3a serie/ del 1o al 4o ano)",
      "06" = "Ensenanza fundamental o 1er grado (4a serie/ 5o ano)",
      "07" = "Ensenanza fundamental o 1er grado (de la 5a a la 8a serie/ 6o al 9o ano)",
      "08" = "Supletivo de la ensenanza fundamental o del 1er grado",
      "09" = "Antiguo cientifico, clasico, etc. (medio 2do ciclo)",
      "10" = "Regular o supletivo de la ensenanza media o del 2do grado",
      "11" = "Superior de grado",
      "12" = "Especializacion de nivel superior (minimo de 360 horas)",
      "13" = "Maestria",
      "14" = "Doctorado"
    ),
    
    "V0634" = c(
      "1" = "Si",
      "2" = "No"
    ),
    
    "V0635" = c(
      "1" = "Superior de grado",
      "2" = "Maestria",
      "3" = "Doctorado"
    ),
    
    "V6400" = c(
      "1" = "Sin instruccion y fundamental incompleto",
      "2" = "Fundamental completo y medio incompleto",
      "3" = "Medio completo y superior incompleto",
      "4" = "Superior completo",
      "5" = "No determinado"
    ),
    
    "V0636" = c(
      "1" = "En este municipio",
      "2" = "En otro municipio",
      "3" = "En pais extranjero"
    ),
    
    "V0637" = c(
      "1" = "Si",
      "2" = "No, pero vivio",
      "3" = "No, nunca vivio"
    ),
    
    "V0639" = c(
      "1" = "Matrimonio civil y religioso",
      "2" = "Solo matrimonio civil",
      "3" = "Solo matrimonio religioso",
      "4" = "Union consensual"
    ),
    
    "V0640" = c(
      "1" = "Casado(a)",
      "2" = "Desquitado(a) o separado(a) judicialmente",
      "3" = "Divorciado(a)",
      "4" = "Viudo(a)",
      "5" = "Soltero(a)"
    ),
    
    "V0641" = c(
      "1" = "Si",
      "2" = "No"
    ),
    
    "V0642" = c(
      "1" = "Si",
      "2" = "No"
    ),
    
    "V0643" = c(
      "1" = "Si",
      "2" = "No"
    ),
    
    "V0644" = c(
      "1" = "Si",
      "2" = "No"
    ),
    
    "V0645" = c(
      "1" = "Uno",
      "2" = "Dos o mas"
    ),
    
    "V0648" = c(
      "1" = "Empleado con tarjeta de trabajo firmada",
      "2" = "Militar del ejercito, marina, aeronautica, policia militar o cuerpo de bomberos",
      "3" = "Empleado por el regimen juridico de los funcionarios publicos",
      "4" = "Empleado sin tarjeta de trabajo firmada",
      "5" = "Cuenta propia",
      "6" = "Empleador",
      "7" = "No remunerado"
    ),
    
    "V0649" = c(
      "1" = "1 a 5 personas",
      "2" = "6 o mas personas"
    ),
    
    "V0650" = c(
      "1" = "Si, en el trabajo principal",
      "2" = "Si, en otro trabajo",
      "3" = "No"
    ),
    
    "V0651" = c(
      "0" = "No tiene",
      "1" = "En dinero, productos o mercaderias",
      "2" = "Solamente en beneficios"
    ),
    
    "V0652" = c(
      "0" = "No tiene",
      "1" = "En dinero, productos o mercaderias",
      "2" = "Solamente en beneficios"
    ),
    
    "V0654" = c(
      "1" = "Si",
      "2" = "No"
    ),
    
    "V0655" = c(
      "1" = "Si",
      "2" = "No"
    ),
    
    "V0656" = c(
      "0" = "No",
      "1" = "Si",
      "9" = "Ignorado"
    ),
    
    "V0657" = c(
      "0" = "No",
      "1" = "Si",
      "9" = "Ignorado"
    ),
    
    "V0658" = c(
      "0" = "No",
      "1" = "Si",
      "9" = "Ignorado"
    ),
    
    "V0659" = c(
      "0" = "No",
      "1" = "Si",
      "9" = "Ignorado"
    ),
    
    "V0660" = c(
      "1" = "En el propio domicilio",
      "2" = "Solamente en este municipio, pero no en el propio domicilio",
      "3" = "En otro municipio",
      "4" = "En pais extranjero",
      "5" = "En mas de un municipio o pais"
    ),
    
    "V0661" = c(
      "1" = "Si",
      "2" = "No"
    ),
    
    "V0662" = c(
      "1" = "Hasta 05 minutos",
      "2" = "De 06 minutos hasta media hora",
      "3" = "Mas de media hora hasta una hora",
      "4" = "Mas de una hora hasta dos horas",
      "5" = "Mas de dos horas"
    ),
    
    "V0663" = c(
      "1" = "Tuvo hijos nacidos vivos",
      "2" = "No tuvo ningun hijo nacido vivo"
    ),
    
    "V0664" = c(
      "1" = "Hijos vivos",
      "2" = "No sabe"
    ),
    
    "V0665" = c(
      "1" = "Masculino",
      "2" = "Femenino"
    ),
    
    "V6664" = c(
      "1" = "Si",
      "0" = "No"
    ),
    
    "V0667" = c(
      "1" = "Si",
      "2" = "No",
      "9" = "No sabe"
    ),
    
    "V0668" = c(
      "1" = "Sabe el mes y ano o solamente el ano",
      "2" = "No sabe"
    ),
    
    "V0669" = c(
      "1" = "Tuvo hijo nacido muerto",
      "2" = "No tuvo hijo nacido muerto",
      "3" = "No sabe"
    ),
    
    "V0670" = c(
      "1" = "La propia persona",
      "2" = "Otro residente",
      "3" = "No residente",
      "9" = "Ignorado"
    ),
    
    "V6900" = c(
      "1" = "Economicamente activas",
      "2" = "No economicamente activas"
    ),
    
    "V6910" = c(
      "1" = "Ocupadas",
      "2" = "Desocupadas"
    ),
    
    "V6920" = c(
      "1" = "Ocupadas",
      "2" = "No ocupadas"
    ),
    
    "V6930" = c(
      "1" = "Empleados con tarjeta de trabajo firmada",
      "2" = "Militares y funcionarios publicos estatutarios",
      "3" = "Empleados sin tarjeta de trabajo firmada",
      "4" = "Cuenta propia",
      "5" = "Empleadores",
      "6" = "No remunerados",
      "7" = "Trabajadores en la produccion para el propio consumo"
    ),
    
    "V6940" = c(
      "1" = "Trabajadores domesticos con tarjeta de trabajo firmada",
      "2" = "Trabajadores domesticos sin tarjeta de trabajo firmada",
      "3" = "Demas empleados con tarjeta de trabajo firmada",
      "4" = "Militares y funcionarios publicos estatutarios",
      "5" = "Demas empleados sin tarjeta de trabajo firmada"
    ),
    
    "V0604" = c(
      "1" = "Si y vive en este domicilio",
      "2" = "Si y vive en otro domicilio",
      "3" = "No",
      "4" = "No sabe",
      "9" = "Ignorado"
    ),
    
    "V5020" = c(
      "01" = "Familias unicas o convivientes principales",
      "02" = "Familia conviviente - segunda",
      "03" = "Familia conviviente - tercera",
      "04" = "Familia conviviente - cuarta",
      "05" = "Familia conviviente - quinta",
      "06" = "Familia conviviente - sexta",
      "07" = "Familia conviviente - septima",
      "08" = "Familia conviviente - octava",
      "09" = "Familia conviviente - novena"
    ),
    
    "V5030" = c(
      "1" = "Unipersonal",
      "2" = "Dos personas o mas sin parentesco",
      "3" = "Dos personas o mas con parentesco"
    ),
    
    "V5040" = c(
      "1" = "Arreglo familiar",
      "0" = "Arreglo no familiar"
    ),
    
    "V5090" = c(
      "1" = "Pareja sin hijo(s)",
      "2" = "Pareja sin hijo(s) y con pariente(s)",
      "3" = "Pareja con hijo(s)",
      "4" = "Pareja con hijo(s) y con pariente(s)",
      "5" = "Mujer sin conyuge con hijo(s)",
      "6" = "Mujer sin conyuge con hijo(s) y con pariente(s)",
      "7" = "Hombre sin conyuge con hijo(s)",
      "8" = "Hombre sin conyuge con hijo(s) y con pariente(s)",
      "9" = "Otro"
    ),
    
    "V5100" = c(
      "1" = "Pareja sin hijo(s)",
      "2" = "Pareja con hijo(s)",
      "3" = "Mujer sin conyuge con hijo(s)"
    ),
    
    "V5110" = c(
      "Contribuintes" = "Contribuyentes",
      "Nao contribuintes" = "No contribuyentes"
    ),
    
    "V5120" = c(
      "Contribuintes" = "Contribuyentes",
      "Nao contribuintes" = "No contribuyentes"
    ),
    
    # Marcas de Imputacion (variables M)
    "M0502" = c("1" = "Si", "2" = "No"),
    "M0601" = c("1" = "Si", "2" = "No"),
    "M6033" = c("1" = "Si", "2" = "No"),
    "M0606" = c("1" = "Si", "2" = "No"),
    "M0613" = c("1" = "Si", "2" = "No"),
    "M0614" = c("1" = "Si", "2" = "No"),
    "M0615" = c("1" = "Si", "2" = "No"),
    "M0616" = c("1" = "Si", "2" = "No"),
    "M0617" = c("1" = "Si", "2" = "No"),
    "M0618" = c("1" = "Si", "2" = "No"),
    "M0619" = c("1" = "Si", "2" = "No"),
    "M0620" = c("1" = "Si", "2" = "No"),
    "M0621" = c("1" = "Si", "2" = "No"),
    "M0622" = c("1" = "Si", "2" = "No"),
    "M6222" = c("1" = "Si", "2" = "No"),
    "M6224" = c("1" = "Si", "2" = "No"),
    "M0623" = c("1" = "Si", "2" = "No"),
    "M0624" = c("1" = "Si", "2" = "No"),
    "M0625" = c("1" = "Si", "2" = "No"),
    "M6252" = c("1" = "Si", "2" = "No"),
    "M6254" = c("1" = "Si", "2" = "No"),
    "M6256" = c("1" = "Si", "2" = "No"),
    "M0626" = c("1" = "Si", "2" = "No"),
    "M6262" = c("1" = "Si", "2" = "No"),
    "M6264" = c("1" = "Si", "2" = "No"),
    "M6266" = c("1" = "Si", "2" = "No"),
    "M0627" = c("1" = "Si", "2" = "No"),
    "M0628" = c("1" = "Si", "2" = "No"),
    "M0629" = c("1" = "Si", "2" = "No"),
    "M0630" = c("1" = "Si", "2" = "No"),
    "M0631" = c("1" = "Si", "2" = "No"),
    "M0632" = c("1" = "Si", "2" = "No"),
    "M0633" = c("1" = "Si", "2" = "No"),
    "M0634" = c("1" = "Si", "2" = "No"),
    "M0635" = c("1" = "Si", "2" = "No"),
    "M6352" = c("1" = "Si", "2" = "No"),
    "M6354" = c("1" = "Si", "2" = "No"),
    "M6356" = c("1" = "Si", "2" = "No"),
    "M0636" = c("1" = "Si", "2" = "No"),
    "M6362" = c("1" = "Si", "2" = "No"),
    "M6364" = c("1" = "Si", "2" = "No"),
    "M6366" = c("1" = "Si", "2" = "No"),
    "M0637" = c("1" = "Si", "2" = "No"),
    "M0638" = c("1" = "Si", "2" = "No"),
    "M0639" = c("1" = "Si", "2" = "No"),
    "M0640" = c("1" = "Si", "2" = "No"),
    "M0641" = c("1" = "Si", "2" = "No"),
    "M0642" = c("1" = "Si", "2" = "No"),
    "M0643" = c("1" = "Si", "2" = "No"),
    "M0644" = c("1" = "Si", "2" = "No")
  )
  
  return(list(columns = columns, values = values))
}

#' Obtem o Dicionario de Traducao para Domicilios (Censo 2010)
#'
#' @description
#' Retorna uma lista contendo dicionarios para traduzir os nomes das colunas
#' e os valores das variaveis categoricas do arquivo de Microdados de Domicilios
#' do Censo 2010 (IBGE) para o portugues padronizado.
#'
#' @return Uma lista com dois elementos:
#'   - `columns`: um vetor nomeado para traduzir os nomes das colunas.
#'   - `values`: uma lista de vetores nomeados para decodificar valores categoricos.
#'
#' @keywords internal
#' @noRd
get_translate_dictionary_pt_households <- function() {
  list(
    # --- 1. Renomeacao das Colunas (Vxxxx -> nome_legivel) ---
    columns = c(
      "V0001" = "uf",
      "V0002" = "codigo_municipio",
      "V0011" = "area_ponderacao",
      "V0300" = "controle",
      "V0010" = "peso_amostral",
      "V1001" = "regiao_geografica",
      "V1002" = "codigo_mesorregiao",
      "V1003" = "codigo_microrregiao",
      "V1004" = "codigo_regiao_metropolitana",
      "V1005" = "situacao_setor_detalhada",
      "V1006" = "situacao_domicilio",
      "V4001" = "especie_unidade_visitada",
      "V4002" = "tipo_especie_domicilio",
      "V0201" = "condicao_ocupacao",
      "V2011" = "valor_aluguel_reais",
      "V2012" = "aluguel_salarios_minimos",
      "V0202" = "material_paredes_externas",
      "V0203" = "numero_comodos",
      "V6203" = "densidade_morador_comodo",
      "V0204" = "numero_dormitorios",
      "V6204" = "densidade_morador_dormitorio",
      "V0205" = "numero_banheiros_exclusivos",
      "V0206" = "tem_sanitario_ou_buraco",
      "V0207" = "tipo_esgotamento_sanitario",
      "V0208" = "forma_abastecimento_agua",
      "V0209" = "tem_agua_canalizada",
      "V0210" = "destino_lixo",
      "V0211" = "tem_energia_eletrica",
      "V0212" = "tem_medidor_energia",
      "V0213" = "tem_radio",
      "V0214" = "tem_televisao",
      "V0215" = "tem_maquina_lavar",
      "V0216" = "tem_geladeira",
      "V0217" = "tem_celular",
      "V0218" = "tem_telefone_fixo",
      "V0219" = "tem_microcomputador",
      "V0220" = "tem_internet",
      "V0221" = "tem_motocicleta",
      "V0222" = "tem_automovel",
      "V0301" = "tem_morador_no_exterior",
      "V0401" = "total_moradores",
      "V0402" = "responsabilidade_domicilio",
      "V0701" = "ocorreu_falecimento_ano_anterior",
      "V6529" = "rendimento_mensal_domiciliar",
      "V6530" = "rendimento_domiciliar_salarios_minimos",
      "V6531" = "rendimento_per_capita",
      "V6532" = "rendimento_per_capita_salarios_minimos",
      "V6600" = "tipo_familia",
      "V6210" = "adequacao_moradia"
    ),
    
    # --- 2. Traducao dos Valores Categoricos (Codigo -> Rotulo) ---
    categories = list(
      "V0001" = c(
        "11" = "Rondonia", "12" = "Acre", "13" = "Amazonas", "14" = "Roraima", 
        "15" = "Para", "16" = "Amapa", "17" = "Tocantins", "21" = "Maranhao", 
        "22" = "Piaui", "23" = "Ceara", "24" = "Rio Grande do Norte", "25" = "Paraiba", 
        "26" = "Pernambuco", "27" = "Alagoas", "28" = "Sergipe", "29" = "Bahia", 
        "31" = "Minas Gerais", "32" = "Espirito Santo", "33" = "Rio de Janeiro", 
        "35" = "Sao Paulo", "41" = "Parana", "42" = "Santa Catarina", 
        "43" = "Rio Grande do Sul", "50" = "Mato Grosso do Sul", 
        "51" = "Mato Grosso", "52" = "Goias", "53" = "Distrito Federal"
      ),
      
      "V1001" = c(
        "1" = "Norte", "2" = "Nordeste", "3" = "Sudeste", 
        "4" = "Sul", "5" = "Centro-Oeste"
      ),
      
      "V1006" = c("1" = "Urbana", "2" = "Rural"),
      
      "V1005" = c(
        "1" = "Area urbanizada", "2" = "Area nao urbanizada", 
        "3" = "Area urbanizada isolada", "4" = "Area rural de extensao urbana",
        "5" = "Aglomerado rural povoado", "6" = "Aglomerado rural nucleo",
        "7" = "Aglomerado rural outros", "8" = "Area rural exclusive aglomerado"
      ),
      
      "V4001" = c(
        "01" = "Particular permanente ocupado",
        "02" = "Particular permanente ocupado (sem entrevista)",
        "05" = "Particular improvisado ocupado",
        "06" = "Coletivo com morador"
      ),
      
      "V4002" = c(
        "11" = "Casa", "12" = "Casa de vila/condominio", "13" = "Apartamento",
        "14" = "Habitacao em casa de comodos/cortico", "15" = "Oca ou maloca",
        "51" = "Tenda ou barraca", "52" = "Dentro de estabelecimento",
        "53" = "Outro (vagao/trailer/gruta)", "61" = "Asilo/orfanato",
        "62" = "Hotel/pensao", "63" = "Alojamento trabalhadores",
        "64" = "Penitenciaria/presidio", "65" = "Outro coletivo"
      ),
      
      "V0201" = c(
        "1" = "Proprio (ja pago)", "2" = "Proprio (ainda pagando)",
        "3" = "Alugado", "4" = "Cedido por empregador",
        "5" = "Cedido de outra forma", "6" = "Outra condicao"
      ),
      
      "V0202" = c(
        "1" = "Alvenaria c/ revestimento", "2" = "Alvenaria s/ revestimento",
        "3" = "Madeira aparelhada", "4" = "Taipa revestida",
        "5" = "Taipa nao revestida", "6" = "Madeira aproveitada",
        "7" = "Palha", "8" = "Outro material", "9" = "Sem parede"
      ),
      
      "V0205" = c(
        "0" = "Zero", "1" = "Um", "2" = "Dois", "3" = "Tres", 
        "4" = "Quatro", "5" = "Cinco", "6" = "Seis", 
        "7" = "Sete", "8" = "Oito", "9" = "Nove ou mais"
      ),
      
      "V0207" = c(
        "1" = "Rede geral de esgoto/pluvial", "2" = "Fossa septica",
        "3" = "Fossa rudimentar", "4" = "Vala",
        "5" = "Rio, lago ou mar", "6" = "Outro"
      ),
      
      "V0208" = c(
        "01" = "Rede geral", "02" = "Poco/nascente na propriedade",
        "03" = "Poco/nascente fora propriedade", "04" = "Carro-pipa",
        "05" = "Agua chuva (cisterna)", "06" = "Agua chuva (outra forma)",
        "07" = "Rio/acude/lago", "08" = "Outra",
        "09" = "Poco/nascente na aldeia", "10" = "Poco/nascente fora aldeia"
      ),
      
      "V0209" = c(
        "1" = "Sim (pelo menos um comodo)", 
        "2" = "Sim (so no terreno)", 
        "3" = "Nao"
      ),
      
      "V0210" = c(
        "1" = "Coletado", "2" = "Cacamba servico limpeza",
        "3" = "Queimado na propriedade", "4" = "Enterrado na propriedade",
        "5" = "Jogado terreno baldio", "6" = "Jogado rio/lago/mar",
        "7" = "Outro destino"
      ),
      
      "V0211" = c("1" = "Sim (cia distribuidora)", "2" = "Sim (outras fontes)", "3" = "Nao"),
      "V0212" = c("1" = "Sim (exclusivo)", "2" = "Sim (comum)", "3" = "Nao tem"),
      
      # Variaveis Binarias (Sim/Nao)
      "V0206" = c("1" = "Sim", "2" = "Nao"),
      "V0213" = c("1" = "Sim", "2" = "Nao"),
      "V0214" = c("1" = "Sim", "2" = "Nao"),
      "V0215" = c("1" = "Sim", "2" = "Nao"),
      "V0216" = c("1" = "Sim", "2" = "Nao"),
      "V0217" = c("1" = "Sim", "2" = "Nao"),
      "V0218" = c("1" = "Sim", "2" = "Nao"),
      "V0219" = c("1" = "Sim", "2" = "Nao"),
      "V0220" = c("1" = "Sim", "2" = "Nao"),
      "V0221" = c("1" = "Sim", "2" = "Nao"),
      "V0222" = c("1" = "Sim", "2" = "Nao"),
      "V0301" = c("1" = "Sim", "2" = "Nao"),
      "V0701" = c("1" = "Sim", "2" = "Nao"),
      
      "V0402" = c("1" = "Apenas um morador", "2" = "Mais de um morador", "9" = "Ignorado"),
      "V6600" = c("1" = "Unipessoal", "2" = "Nuclear", "3" = "Estendida", "4" = "Composta"),
      "V6210" = c("1" = "Adequada", "2" = "Semiadequada", "3" = "Inadequada")
    )
  )
}

#' Get Translation Dictionary for Households (Census 2010)
#'
#' @description
#' Returns a list containing dictionaries to translate column names and 
#' categorical variable values from the 2010 Census Households Microdata 
#' (IBGE) into standardized English.
#'
#' @return A list with two elements:
#'   - `columns`: a named vector to translate column names.
#'   - `values`: a list of named vectors to decode categorical values.
#'
#' @keywords internal
#' @noRd
get_translate_dictionary_en_households <- function() {

  # ==========================================================================
  # COLUMN NAME TRANSLATION (HOUSEHOLDS - CENSUS 2010)
  # Maps original codes (Vxxxx) to descriptive English names.
  # Source: 2010_dictionary_microdata_households.html
  # ==========================================================================

  columns <- c(
    # --- Identification and Location ---
    "V0001" = "federation_unit",
    "V0002" = "municipality_code",
    "V0011" = "weighting_area",
    "V0300" = "control_number",
    "V0010" = "sample_weight",

    # --- Regionalization ---
    "V1001" = "geographic_region",
    "V1002" = "mesoregion_code",
    "V1003" = "microregion_code",
    "V1004" = "metropolitan_area_code",
    "V1005" = "census_tract_situation_detailed", # Detailed Urban/Rural
    "V1006" = "household_situation",             # Simple Urban/Rural

    # --- Household Characteristics ---
    "V4001" = "visited_unit_type",
    "V4002" = "household_type",
    "V0201" = "occupancy_condition",
    "V0202" = "predominant_wall_material",
    "V0203" = "number_of_rooms",
    "V6203" = "resident_room_density",
    "V0204" = "number_of_bedrooms",
    "V6204" = "resident_bedroom_density",
    "V0205" = "number_of_bathrooms",
    "V0206" = "has_sanitary_facility",
    "V0207" = "sewage_disposal_type",
    "V0208" = "water_supply_source",
    "V0209" = "piped_water_existence",
    "V0210" = "garbage_disposal",
    "V0211" = "electricity_existence",
    "V0212" = "electricity_meter_existence",

    # --- Assets and Durables (Yes/No) ---
    "V0213" = "has_radio",
    "V0214" = "has_television",
    "V0215" = "has_washing_machine",
    "V0216" = "has_refrigerator",
    "V0217" = "has_cellphone",
    "V0218" = "has_landline_phone",
    "V0219" = "has_computer",
    "V0220" = "has_internet_access",
    "V0221" = "has_motorcycle",
    "V0222" = "has_automobile",

    # --- Family and Residents ---
    "V0301" = "resident_abroad_last_year",
    "V0401" = "total_residents",
    "V0402" = "household_responsibility",
    "V0701" = "death_in_household_last_year",
    "V6600" = "household_unit_type", # Family structure
    "V6210" = "housing_adequacy",

    # --- Financial Values ---
    "V2011" = "rent_value",
    "V2012" = "rent_minimum_wages",
    "V6529" = "monthly_household_income",
    "V6530" = "household_income_min_wages",
    "V6531" = "per_capita_income",
    "V6532" = "per_capita_income_min_wages"
  )

  # ==========================================================================
  # CATEGORICAL VALUES TRANSLATION
  # Decodes numeric codes into readable English labels.
  # ==========================================================================

  values <- list(
    "V0001" = c(
      "11" = "Rondonia", "12" = "Acre", "13" = "Amazonas", "14" = "Roraima",
      "15" = "Para", "16" = "Amapa", "17" = "Tocantins",
      "21" = "Maranhao", "22" = "Piaui", "23" = "Ceara", "24" = "Rio Grande do Norte",
      "25" = "Paraiba", "26" = "Pernambuco", "27" = "Alagoas", "28" = "Sergipe",
      "29" = "Bahia",
      "31" = "Minas Gerais", "32" = "Espirito Santo", "33" = "Rio de Janeiro",
      "35" = "Sao Paulo",
      "41" = "Parana", "42" = "Santa Catarina", "43" = "Rio Grande do Sul",
      "50" = "Mato Grosso do Sul", "51" = "Mato Grosso", "52" = "Goias",
      "53" = "Distrito Federal"
    ),

    "V1001" = c(
      "1" = "North", "2" = "Northeast", "3" = "Southeast", "4" = "South", "5" = "Center-West"
    ),

    "V1006" = c("1" = "Urban", "2" = "Rural"),

    "V1005" = c(
      "1" = "Urbanized area",
      "2" = "Non-urbanized area",
      "3" = "Isolated urbanized area",
      "4" = "Rural area with urban extension",
      "5" = "Rural settlement - Village",
      "6" = "Rural settlement - Nucleus",
      "7" = "Rural settlement - Other",
      "8" = "Rural area exclusive of settlements"
    ),

    "V4001" = c(
      "01" = "Occupied permanent private household",
      "02" = "Occupied permanent private household without interview",
      "05" = "Occupied improvised private household",
      "06" = "Collective household with residents"
    ),

    "V4002" = c(
      "11" = "House",
      "12" = "House in a gated community or village",
      "13" = "Apartment",
      "14" = "Tenement or rooming house",
      "15" = "Indigenous hut (Oca/Maloca)",
      "51" = "Tent",
      "52" = "Inside establishment",
      "53" = "Other (wagon, trailer, cave, etc.)",
      "61" = "Asylum, orphanage, and similar",
      "62" = "Hotel, boarding house, and similar",
      "63" = "Workers' lodging",
      "64" = "Penitentiary, jail, or detention center",
      "65" = "Other collective household"
    ),

    "V0201" = c(
      "1" = "Owned by resident - fully paid",
      "2" = "Owned by resident - still paying",
      "3" = "Rented",
      "4" = "Provided by employer",
      "5" = "Provided in another way",
      "6" = "Other condition"
    ),

    "V0202" = c(
      "1" = "Masonry with coating",
      "2" = "Masonry without coating",
      "3" = "Appropriate wood (planed)",
      "4" = "Coated rammed earth/wattle and daub",
      "5" = "Uncoated rammed earth/wattle and daub",
      "6" = "Reused wood/Scrap material",
      "7" = "Straw",
      "8" = "Other material",
      "9" = "No walls"
    ),

    "V0205" = c(
      "0" = "Zero", "1" = "One", "2" = "Two", "3" = "Three",
      "4" = "Four", "5" = "Five", "6" = "Six",
      "7" = "Seven", "8" = "Eight", "9" = "Nine or more"
    ),

    "V0206" = c("1" = "Yes", "2" = "No"), # Has sanitary facility

    "V0207" = c(
      "1" = "General sewage or rainwater network",
      "2" = "Septic tank",
      "3" = "Rudimentary cesspit",
      "4" = "Ditch",
      "5" = "River, lake, or sea",
      "6" = "Other"
    ),

    "V0208" = c(
      "01" = "General distribution network",
      "02" = "Well or spring on property",
      "03" = "Well or spring outside property",
      "04" = "Water truck",
      "05" = "Rainwater (cistern)",
      "06" = "Rainwater (other storage)",
      "07" = "River, reservoir, lake, or creek",
      "08" = "Other",
      "09" = "Well or spring in village (Indigenous)",
      "10" = "Well or spring outside village (Indigenous)"
    ),

    "V0209" = c(
      "1" = "Yes, in at least one room",
      "2" = "Yes, only on property/plot",
      "3" = "No"
    ),

    "V0210" = c(
      "1" = "Collected directly by cleaning service",
      "2" = "Placed in cleaning service skip/dumpster",
      "3" = "Burned on property",
      "4" = "Buried on property",
      "5" = "Dumped on open lot or public area",
      "6" = "Dumped in river, lake, or sea",
      "7" = "Other destination"
    ),

    "V0211" = c(
      "1" = "Yes, from distribution company",
      "2" = "Yes, from other sources",
      "3" = "No electricity"
    ),

    "V0212" = c(
      "1" = "Yes, exclusive use",
      "2" = "Yes, common use",
      "3" = "No meter"
    ),

    # --- Boolean Variables (Assets & Flags) ---
    "V0213" = c("1" = "Yes", "2" = "No"),
    "V0214" = c("1" = "Yes", "2" = "No"),
    "V0215" = c("1" = "Yes", "2" = "No"),
    "V0216" = c("1" = "Yes", "2" = "No"),
    "V0217" = c("1" = "Yes", "2" = "No"),
    "V0218" = c("1" = "Yes", "2" = "No"),
    "V0219" = c("1" = "Yes", "2" = "No"),
    "V0220" = c("1" = "Yes", "2" = "No"),
    "V0221" = c("1" = "Yes", "2" = "No"),
    "V0222" = c("1" = "Yes", "2" = "No"),
    "V0301" = c("1" = "Yes", "2" = "No"),
    "V0701" = c("1" = "Yes", "2" = "No"),

    "V0402" = c(
      "1" = "Single resident",
      "2" = "More than one resident",
      "9" = "Ignored"
    ),

    "V6600" = c(
      "1" = "One-person",
      "2" = "Nuclear",
      "3" = "Extended",
      "4" = "Composite"
    ),

    "V6210" = c(
      "1" = "Adequate",
      "2" = "Semi-adequate",
      "3" = "Inadequate"
    )
  )

  return(list(columns = columns, values = values))
}


#' Obtener Diccionario de Traduccion para Hogares (Censo 2010)
#'
#' @description
#' Devuelve una lista que contiene diccionarios para traducir los nombres de las 
#' columnas y los valores de las variables categoricas del archivo de Microdatos 
#' de Hogares (Domicilios) del Censo 2010 (IBGE) al espanol estandarizado.
#'
#' @return Una lista con dos elementos:
#'   - `columns`: un vector con nombre para traducir nombres de columnas.
#'   - `values`: una lista de vectores con nombre para decodificar valores.
#'
#' @keywords internal
#' @noRd
get_translate_dictionary_es_households <- function() {

  # ==========================================================================
  # TRADUCCIoN DE COLUMNAS (DOMICILIOS - CENSO 2010)
  # Mapea codigos originales (Vxxxx) a nombres descriptivos en espanol.
  # ==========================================================================

  columns <- c(
    # --- Identificacion y Ubicacion ---
    "V0001" = "unidad_federativa",
    "V0002" = "codigo_municipio",
    "V0011" = "area_ponderacion",
    "V0300" = "numero_control",
    "V0010" = "peso_muestral",

    # --- Regionalizacion ---
    "V1001" = "region_geografica",
    "V1002" = "codigo_mesorregion",
    "V1003" = "codigo_microrregion",
    "V1004" = "codigo_region_metropolitana",
    "V1005" = "situacion_sector_detallada",
    "V1006" = "situacion_domicilio", # Urbana/Rural

    # --- Caracteristicas del Domicilio ---
    "V4001" = "especie_unidad_visitada",
    "V4002" = "tipo_domicilio",
    "V0201" = "condicion_ocupacion",
    "V0202" = "material_predominante_paredes",
    "V0203" = "numero_ambientes",        # "Comodos" -> Ambientes/Habitaciones
    "V6203" = "densidad_habitante_ambiente",
    "V0204" = "numero_dormitorios",
    "V6204" = "densidad_habitante_dormitorio",
    "V0205" = "numero_banios",           # Banos
    "V0206" = "tiene_instalacion_sanitaria",
    "V0207" = "tipo_desague_sanitario",
    "V0208" = "forma_abastecimiento_agua",
    "V0209" = "existencia_agua_canalizada",
    "V0210" = "destino_basura",
    "V0211" = "existencia_energia_electrica",
    "V0212" = "existencia_medidor_energia",

    # --- Bienes y Servicios (Si/No) ---
    "V0213" = "tiene_radio",
    "V0214" = "tiene_television",
    "V0215" = "tiene_lavarropas",
    "V0216" = "tiene_heladera",         # Ou refrigerador
    "V0217" = "tiene_celular",
    "V0218" = "tiene_telefono_fijo",
    "V0219" = "tiene_computadora",
    "V0220" = "tiene_internet",
    "V0221" = "tiene_motocicleta",
    "V0222" = "tiene_automovil",

    # --- Familia y Residentes ---
    "V0301" = "residente_exterior_anio_anterior",
    "V0401" = "total_residentes",
    "V0402" = "responsabilidad_domicilio",
    "V0701" = "fallecimiento_hogar_anio_anterior",
    "V6600" = "tipo_unidad_domestica",
    "V6210" = "adecuacion_vivienda",

    # --- Valores Financieros ---
    "V2011" = "valor_alquiler",
    "V2012" = "alquiler_salarios_minimos",
    "V6529" = "ingreso_mensual_hogar",
    "V6530" = "ingreso_hogar_salarios_minimos",
    "V6531" = "ingreso_per_capita",
    "V6532" = "ingreso_per_capita_salarios_minimos"
  )

  # ==========================================================================
  # TRADUCCION DE VALORES CATEGORICOS
  # Decodifica codigos numericos a etiquetas legibles en espanol.
  # ==========================================================================

  values <- list(
    # Mantenemos los nombres propios de los estados en portugues/original
    "V0001" = c(
      "11" = "Rondonia", "12" = "Acre", "13" = "Amazonas", "14" = "Roraima",
      "15" = "Para", "16" = "Amapa", "17" = "Tocantins",
      "21" = "Maranhao", "22" = "Piaui", "23" = "Ceara", "24" = "Rio Grande do Norte",
      "25" = "Paraiba", "26" = "Pernambuco", "27" = "Alagoas", "28" = "Sergipe",
      "29" = "Bahia",
      "31" = "Minas Gerais", "32" = "Espirito Santo", "33" = "Rio de Janeiro",
      "35" = "Sao Paulo",
      "41" = "Parana", "42" = "Santa Catarina", "43" = "Rio Grande do Sul",
      "50" = "Mato Grosso do Sul", "51" = "Mato Grosso", "52" = "Goias",
      "53" = "Distrito Federal"
    ),

    "V1001" = c(
      "1" = "Norte", "2" = "Noreste", "3" = "Sudeste", 
      "4" = "Sur", "5" = "Centro-Oeste"
    ),

    "V1006" = c("1" = "Urbana", "2" = "Rural"),

    "V1005" = c(
      "1" = "Area urbanizada",
      "2" = "Area no urbanizada",
      "3" = "Area urbanizada aislada",
      "4" = "Area rural de extension urbana",
      "5" = "Aglomerado rural - Poblado",
      "6" = "Aglomerado rural - Nucleo",
      "7" = "Aglomerado rural - Otros",
      "8" = "Area rural exclusiva (sin aglomerado)"
    ),

    "V4001" = c(
      "01" = "Domicilio particular permanente ocupado",
      "02" = "Domicilio particular permanente ocupado (sin entrevista)",
      "05" = "Domicilio particular improvisado ocupado",
      "06" = "Domicilio colectivo con residentes"
    ),

    "V4002" = c(
      "11" = "Casa",
      "12" = "Casa en condominio o villa",
      "13" = "Apartamento",
      "14" = "Habitacion en casa de vecindad/inquilinato (cortico)", 
      "15" = "Choza indigena (Oca/Maloca)",
      "51" = "Tienda de campana o carpa",
      "52" = "Dentro de establecimiento",
      "53" = "Otro (vagon, remolque, cueva, etc.)",
      "61" = "Asilo, orfanato y similares",
      "62" = "Hotel, pension y similares",
      "63" = "Alojamiento de trabajadores",
      "64" = "Penitenciaria, presidio o centro de detencion",
      "65" = "Otro domicilio colectivo"
    ),

    "V0201" = c(
      "1" = "Propio (ya pagado)",
      "2" = "Propio (aun pagando)",
      "3" = "Alquilado",
      "4" = "Cedido por empleador",
      "5" = "Cedido de otra forma",
      "6" = "Otra condicion"
    ),

    "V0202" = c(
      "1" = "Mamposteria/Albanileria con revestimiento",
      "2" = "Mamposteria/Albanileria sin revestimiento",
      "3" = "Madera apropiada para construccion",
      "4" = "Adobe/Tapial revestido",
      "5" = "Adobe/Tapial no revestido",
      "6" = "Madera aprovechada/desecho",
      "7" = "Paja",
      "8" = "Otro material",
      "9" = "Sin paredes"
    ),

    "V0205" = c(
      "0" = "Cero", "1" = "Uno", "2" = "Dos", "3" = "Tres",
      "4" = "Cuatro", "5" = "Cinco", "6" = "Seis",
      "7" = "Siete", "8" = "Ocho", "9" = "Nueve o mas"
    ),

    "V0206" = c("1" = "Si", "2" = "No"), # Tiene bano/hoyo

    "V0207" = c(
      "1" = "Red general de desague o pluvial",
      "2" = "Fosa septica",
      "3" = "Fosa rudimentaria/Pozo negro",
      "4" = "Zanja",
      "5" = "Rio, lago o mar",
      "6" = "Otro"
    ),

    "V0208" = c(
      "01" = "Red general de distribucion",
      "02" = "Pozo o manantial en la propiedad",
      "03" = "Pozo o manantial fuera de la propiedad",
      "04" = "Camion cisterna",
      "05" = "Agua de lluvia (cisterna)",
      "06" = "Agua de lluvia (otra forma)",
      "07" = "Rio, embalse, lago o arroyo",
      "08" = "Otra",
      "09" = "Pozo o manantial en la aldea (Indigena)",
      "10" = "Pozo o manantial fuera de la aldea (Indigena)"
    ),

    "V0209" = c(
      "1" = "Si, en al menos un ambiente",
      "2" = "Si, solo en la propiedad/terreno",
      "3" = "No"
    ),

    "V0210" = c(
      "1" = "Recolectada directamente por servicio de limpieza",
      "2" = "Colocada en contenedor de servicio de limpieza",
      "3" = "Quemada en la propiedad",
      "4" = "Enterrada en la propiedad",
      "5" = "Arrojada en terreno baldio o via publica",
      "6" = "Arrojada a rio, lago o mar",
      "7" = "Otro destino"
    ),

    "V0211" = c(
      "1" = "Si, de compania distribuidora",
      "2" = "Si, de otras fuentes",
      "3" = "No existe energia electrica"
    ),

    "V0212" = c(
      "1" = "Si, de uso exclusivo",
      "2" = "Si, de uso comun",
      "3" = "No tiene medidor"
    ),

    # --- Variables Booleanas (Activos & Flags) ---
    "V0213" = c("1" = "Si", "2" = "No"),
    "V0214" = c("1" = "Si", "2" = "No"),
    "V0215" = c("1" = "Si", "2" = "No"),
    "V0216" = c("1" = "Si", "2" = "No"),
    "V0217" = c("1" = "Si", "2" = "No"),
    "V0218" = c("1" = "Si", "2" = "No"),
    "V0219" = c("1" = "Si", "2" = "No"),
    "V0220" = c("1" = "Si", "2" = "No"),
    "V0221" = c("1" = "Si", "2" = "No"),
    "V0222" = c("1" = "Si", "2" = "No"),
    "V0301" = c("1" = "Si", "2" = "No"),
    "V0701" = c("1" = "Si", "2" = "No"),

    "V0402" = c(
      "1" = "Un solo residente",
      "2" = "Mas de un residente",
      "9" = "Ignorado"
    ),

    "V6600" = c(
      "1" = "Unipersonal",
      "2" = "Nuclear",
      "3" = "Extendida",
      "4" = "Compuesta"
    ),

    "V6210" = c(
      "1" = "Adecuada",
      "2" = "Semi-adecuada",
      "3" = "Inadecuada"
    )
  )

  return(list(columns = columns, values = values))
}


#' Obtem o Dicionario de Traducao para Familias (Censo 2000)
#'
#' @description
#' Retorna uma lista contendo dicionarios para traduzir os nomes das colunas
#' e os valores das variaveis categoricas do arquivo de Microdados de Familias
#' do Censo 2000 (IBGE) para o portugues padronizado.
#'
#' @return Uma lista com dois elementos:
#'   - `columns`: um vetor nomeado para traduzir os nomes das colunas.
#'   - `values`: uma lista de vetores nomeados para decodificar valores categoricos.
#'
#' @keywords internal
#' @noRd
get_translate_dictionary_pt_families <- function() {

  columns <- c(
    # --- Identificacao e Localizacao ---
    "V0102" = "unidade_federacao",
    "V1002" = "codigo_mesorregiao",
    "V1003" = "codigo_microrregiao",
    "V0103" = "codigo_municipio",
    "V0104" = "codigo_distrito",
    "V0105" = "codigo_subdistrito",
    "V0300" = "controle",
    "V0404" = "numero_ordem_familia",
    "P001"  = "peso_amostral",
    "AREAP" = "area_ponderacao",

    # --- Regionalizacao ---
    "V1001" = "regiao_geografica",
    "V1004" = "regiao_metropolitana",

    # --- Caracteristicas da Familia ---
    "CODV0404"   = "tipo_familia_convivio",
    "CODV0404_2" = "tipo_composicao_familia",
    "V7400"      = "tamanho_familia",
    "CODV7400"   = "classe_tamanho_familia",
    "V7400A"     = "numero_homens",
    "CODV7400A"  = "classe_numero_homens",
    "V7400B"     = "numero_mulheres",
    "CODV7400B"  = "classe_numero_mulheres",

    # --- Rendimento ---
    "V4614B"        = "rendimento_nominal_familiar",
    "CODV4615B"     = "classe_rendimento_familiar",
    "V4614C"        = "rendimento_nominal_responsavel",
    "CODV4615C"     = "classe_rendimento_responsavel",
    "V4616_7400"    = "rendimento_per_capita",
    "CODV4615_7400" = "classe_rendimento_per_capita"
  )

  values <- list(
    "V0102" = c(
      "11"="Rondonia", "12"="Acre", "13"="Amazonas", "14"="Roraima", "15"="Para",
      "16"="Amapa", "17"="Tocantins", "21"="Maranhao", "22"="Piaui", "23"="Ceara",
      "24"="Rio Grande do Norte", "25"="Paraiba", "26"="Pernambuco", "27"="Alagoas",
      "28"="Sergipe", "29"="Bahia", "31"="Minas Gerais", "32"="Espirito Santo",
      "33"="Rio de Janeiro", "35"="Sao Paulo", "41"="Parana", "42"="Santa Catarina",
      "43"="Rio Grande do Sul", "50"="Mato Grosso do Sul", "51"="Mato Grosso",
      "52"="Goias", "53"="Distrito Federal"
    ),
    "V1001" = c(
      "1" = "Norte", "2" = "Nordeste", "3" = "Sudeste", "4" = "Sul", "5" = "Centro-Oeste"
    ),
    "CODV0404" = c(
      "0" = "Unica (uma so familia)",
      "1" = "Familias conviventes: 1a familia",
      "2" = "Familias conviventes: 2a familia",
      "3" = "Familias conviventes: 3a familia",
      "4" = "Familias conviventes: 4a familia",
      "5" = "Familias conviventes: 5a familia e mais",
      "9" = "Morador individual"
    ),
    "CODV0404_2" = c(
      "01" = "Casal sem filhos",
      "02" = "Casal com filhos menores de 14 anos",
      "03" = "Casal com filhos de 14 anos ou mais",
      "04" = "Casal com filhos de idades variadas",
      "05" = "Mae com filhos menores de 14 anos",
      "06" = "Mae com filhos de 14 anos ou mais",
      "07" = "Mae com filhos de idades variadas",
      "08" = "Pai com filhos menores de 14 anos",
      "09" = "Pai com filhos de 14 anos ou mais",
      "10" = "Pai com filhos de idades variadas",
      "11" = "Outros tipos de familias",
      "12" = "Morador individual"
    ),
    "CODV4615B" = c(
      "01" = "Ate 0,25 salario minimo",
      "02" = "Mais de 0,25 a 0,5 salario minimo",
      "03" = "Mais de 0,5 a 1 salario minimo",
      "04" = "Mais de 1 a 2 salarios minimos",
      "05" = "Mais de 2 a 3 salarios minimos",
      "06" = "Mais de 3 a 5 salarios minimos",
      "07" = "Mais de 5 a 10 salarios minimos",
      "08" = "Mais de 10 a 15 salarios minimos",
      "09" = "Mais de 15 a 20 salarios minimos",
      "10" = "Mais de 20 a 30 salarios minimos",
      "11" = "Mais de 30 salarios minimos",
      "12" = "Sem rendimento"
    )
  )
  # Reutilizar mapa de rendimentos para as outras variaveis de classe de renda
  values[["CODV4615_7400"]] <- values[["CODV4615B"]]
  
  # Mapa ligeiramente diferente para responsavel (tem mais faixas)
  values[["CODV4615C"]] <- c(
      "01" = "Ate 0,25 salario minimo",
      "02" = "Mais de 0,25 a 0,5 salario minimo",
      "03" = "Mais de 0,5 a 0,75 salario minimo",
      "04" = "Mais de 0,75 a 1 salario minimo",
      "05" = "Mais de 1 a 1,25 salarios minimos",
      "06" = "Mais de 1,25 a 1,5 salarios minimos",
      "07" = "Mais de 1,5 a 2 salarios minimos",
      "08" = "Mais de 2 a 3 salarios minimos",
      "09" = "Mais de 3 a 5 salarios minimos",
      "10" = "Mais de 5 a 10 salarios minimos",
      "11" = "Mais de 10 a 15 salarios minimos",
      "12" = "Mais de 15 a 20 salarios minimos",
      "13" = "Mais de 20 a 30 salarios minimos",
      "14" = "Mais de 30 salarios minimos",
      "15" = "Sem rendimento"
  )

  return(list(columns = columns, values = values))
}

#' Get Translation Dictionary for Families (Census 2000)
#'
#' @description
#' Returns a list containing dictionaries to translate column names and 
#' categorical variable values from the 2000 Census Families Microdata 
#' (IBGE) into standardized English.
#'
#' @return A list with two elements:
#'   - `columns`: a named vector to translate column names.
#'   - `values`: a list of named vectors to decode categorical values.
#'
#' @keywords internal
#' @noRd
get_translate_dictionary_en_families <- function() {

  columns <- c(
    # --- Identification and Location ---
    "V0102" = "federation_unit",
    "V1002" = "mesoregion_code",
    "V1003" = "microregion_code",
    "V0103" = "municipality_code",
    "V0104" = "district_code",
    "V0105" = "subdistrict_code",
    "V0300" = "control_number",
    "V0404" = "family_order_number",
    "P001"  = "sample_weight",
    "AREAP" = "weighting_area",

    # --- Regionalization ---
    "V1001" = "geographic_region",
    "V1004" = "metropolitan_area",

    # --- Family Characteristics ---
    "CODV0404"   = "family_living_arrangement",
    "CODV0404_2" = "family_composition_type",
    "V7400"      = "family_size",
    "CODV7400"   = "family_size_class",
    "V7400A"     = "number_of_men",
    "CODV7400A"  = "number_of_men_class",
    "V7400B"     = "number_of_women",
    "CODV7400B"  = "number_of_women_class",

    # --- Income ---
    "V4614B"        = "family_nominal_income",
    "CODV4615B"     = "family_income_class",
    "V4614C"        = "household_head_income",
    "CODV4615C"     = "household_head_income_class",
    "V4616_7400"    = "per_capita_income",
    "CODV4615_7400" = "per_capita_income_class"
  )

  values <- list(
    "V0102" = c(
      "11"="Rondonia", "12"="Acre", "13"="Amazonas", "14"="Roraima", "15"="Para",
      "16"="Amapa", "17"="Tocantins", "21"="Maranhao", "22"="Piaui", "23"="Ceara",
      "24"="Rio Grande do Norte", "25"="Paraiba", "26"="Pernambuco", "27"="Alagoas",
      "28"="Sergipe", "29"="Bahia", "31"="Minas Gerais", "32"="Espirito Santo",
      "33"="Rio de Janeiro", "35"="Sao Paulo", "41"="Parana", "42"="Santa Catarina",
      "43"="Rio Grande do Sul", "50"="Mato Grosso do Sul", "51"="Mato Grosso",
      "52"="Goias", "53"="Distrito Federal"
    ),
    "V1001" = c(
      "1" = "North", "2" = "Northeast", "3" = "Southeast", "4" = "South", "5" = "Center-West"
    ),
    "CODV0404" = c(
      "0" = "Single family (only one family in household)",
      "1" = "Cohabiting families: 1st family",
      "2" = "Cohabiting families: 2nd family",
      "3" = "Cohabiting families: 3rd family",
      "4" = "Cohabiting families: 4th family",
      "5" = "Cohabiting families: 5th family or more",
      "9" = "Individual resident"
    ),
    "CODV0404_2" = c(
      "01" = "Couple without children",
      "02" = "Couple with children under 14",
      "03" = "Couple with children 14 or older",
      "04" = "Couple with children of mixed ages",
      "05" = "Mother with children under 14",
      "06" = "Mother with children 14 or older",
      "07" = "Mother with children of mixed ages",
      "08" = "Father with children under 14",
      "09" = "Father with children 14 or older",
      "10" = "Father with children of mixed ages",
      "11" = "Other family types",
      "12" = "Individual resident"
    ),
    "CODV4615B" = c(
      "01" = "Up to 0.25 minimum wage",
      "02" = "More than 0.25 to 0.5 minimum wage",
      "03" = "More than 0.5 to 1 minimum wage",
      "04" = "More than 1 to 2 minimum wages",
      "05" = "More than 2 to 3 minimum wages",
      "06" = "More than 3 to 5 minimum wages",
      "07" = "More than 5 to 10 minimum wages",
      "08" = "More than 10 to 15 minimum wages",
      "09" = "More than 15 to 20 minimum wages",
      "10" = "More than 20 to 30 minimum wages",
      "11" = "More than 30 minimum wages",
      "12" = "No income"
    )
  )
  values[["CODV4615_7400"]] <- values[["CODV4615B"]]
  
  values[["CODV4615C"]] <- c(
      "01" = "Up to 0.25 minimum wage",
      "02" = "More than 0.25 to 0.5 minimum wage",
      "03" = "More than 0.5 to 0.75 minimum wage",
      "04" = "More than 0.75 to 1 minimum wage",
      "05" = "More than 1 to 1.25 minimum wages",
      "06" = "More than 1.25 to 1.5 minimum wages",
      "07" = "More than 1.5 to 2 minimum wages",
      "08" = "More than 2 to 3 minimum wages",
      "09" = "More than 3 to 5 minimum wages",
      "10" = "More than 5 to 10 minimum wages",
      "11" = "More than 10 to 15 minimum wages",
      "12" = "More than 15 to 20 minimum wages",
      "13" = "More than 20 to 30 minimum wages",
      "14" = "More than 30 minimum wages",
      "15" = "No income"
  )

  return(list(columns = columns, values = values))
}

#' Obtener Diccionario de Traduccion para Familias (Censo 2000)
#'
#' @description
#' Devuelve una lista con diccionarios para traducir los nombres de las columnas
#' y los valores de las variables categoricas del archivo de Microdatos de Familias
#' del Censo 2000 (IBGE) al espanol estandarizado.
#'
#' @return Una lista con dos elementos:
#'   - `columns`: un vector con nombre para traducir nombres de columnas.
#'   - `values`: una lista de vectores con nombre para decodificar valores.
#'
#' @keywords internal
#' @noRd
get_translate_dictionary_es_families <- function() {

  columns <- c(
    # --- Identificacion y Localizacion ---
    "V0102" = "unidad_federacion",
    "V1002" = "codigo_mesorregion",
    "V1003" = "codigo_microrregion",
    "V0103" = "codigo_municipio",
    "V0104" = "codigo_distrito",
    "V0105" = "codigo_subdistrito",
    "V0300" = "control",
    "V0404" = "numero_orden_familia",
    "P001"  = "peso_muestral",
    "AREAP" = "area_ponderacion",

    # --- Regionalizacion ---
    "V1001" = "region_geografica",
    "V1004" = "area_metropolitana",

    # --- Caracteristicas de la Familia ---
    "CODV0404"   = "tipo_familia_convivencia",
    "CODV0404_2" = "tipo_composicion_familia",
    "V7400"      = "tamano_familia",
    "CODV7400"   = "clase_tamano_familia",
    "V7400A"     = "numero_hombres",
    "CODV7400A"  = "clase_numero_hombres",
    "V7400B"     = "numero_mujeres",
    "CODV7400B"  = "clase_numero_mujeres",

    # --- Ingresos ---
    "V4614B"        = "ingreso_nominal_familiar",
    "CODV4615B"     = "clase_ingreso_familiar",
    "V4614C"        = "ingreso_nominal_jefe",
    "CODV4615C"     = "clase_ingreso_jefe",
    "V4616_7400"    = "ingreso_per_capita",
    "CODV4615_7400" = "clase_ingreso_per_capita"
  )

  values <- list(
    "V0102" = c(
      "11"="Rondonia", "12"="Acre", "13"="Amazonas", "14"="Roraima", "15"="Para",
      "16"="Amapa", "17"="Tocantins", "21"="Maranhao", "22"="Piaui", "23"="Ceara",
      "24"="Rio Grande do Norte", "25"="Paraiba", "26"="Pernambuco", "27"="Alagoas",
      "28"="Sergipe", "29"="Bahia", "31"="Minas Gerais", "32"="Espirito Santo",
      "33"="Rio de Janeiro", "35"="Sao Paulo", "41"="Parana", "42"="Santa Catarina",
      "43"="Rio Grande do Sul", "50"="Mato Grosso do Sur", "51"="Mato Grosso",
      "52"="Goias", "53"="Distrito Federal"
    ),
    "V1001" = c(
      "1" = "Norte", "2" = "Nordeste", "3" = "Sudeste", "4" = "Sur", "5" = "Centro-Oeste"
    ),
    "CODV0404" = c(
      "0" = "Unica (una sola familia en el domicilio)",
      "1" = "Familias convivientes: 1a familia",
      "2" = "Familias conviventes: 2a familia",
      "3" = "Familias conviventes: 3a familia",
      "4" = "Familias conviventes: 4a familia",
      "5" = "Familias convivientes: 5a familia o mas",
      "9" = "Residente individual"
    ),
    "CODV0404_2" = c(
      "01" = "Pareja sin hijos",
      "02" = "Pareja con hijos menores de 14 anos",
      "03" = "Pareja con hijos de 14 anos o mas",
      "04" = "Pareja con hijos de edades variadas",
      "05" = "Madre con hijos menores de 14 anos",
      "06" = "Madre con hijos de 14 anos o mas",
      "07" = "Madre con hijos de edades variadas",
      "08" = "Padre con hijos menores de 14 anos",
      "09" = "Padre con hijos de 14 anos o mas",
      "10" = "Padre con hijos de edades variadas",
      "11" = "Otros tipos de familias",
      "12" = "Residente individual"
    ),
    "CODV4615B" = c(
      "01" = "Hasta 0,25 salario minimo",
      "02" = "Mas de 0,25 a 0,5 salario minimo",
      "03" = "Mas de 0,5 a 1 salario minimo",
      "04" = "Mas de 1 a 2 salarios minimos",
      "05" = "Mas de 2 a 3 salarios minimos",
      "06" = "Mas de 3 a 5 salarios minimos",
      "07" = "Mas de 5 a 10 salarios minimos",
      "08" = "Mas de 10 a 15 salarios minimos",
      "09" = "Mas de 15 a 20 salarios minimos",
      "10" = "Mas de 20 a 30 salarios minimos",
      "11" = "Mas de 30 salarios minimos",
      "12" = "Sin ingresos"
    )
  )
  values[["CODV4615_7400"]] <- values[["CODV4615B"]]
  
  values[["CODV4615C"]] <- c(
      "01" = "Hasta 0,25 salario minimo",
      "02" = "Mas de 0,25 a 0,5 salario minimo",
      "03" = "Mas de 0,5 a 0,75 salario minimo",
      "04" = "Mas de 0,75 a 1 salario minimo",
      "05" = "Mas de 1 a 1,25 salarios minimos",
      "06" = "Mas de 1,25 a 1,5 salarios minimos",
      "07" = "Mas de 1,5 a 2 salarios minimos",
      "08" = "Mas de 2 a 3 salarios minimos",
      "09" = "Mas de 3 a 5 salarios minimos",
      "10" = "Mas de 5 a 10 salarios minimos",
      "11" = "Mas de 10 a 15 salarios minimos",
      "12" = "Mas de 15 a 20 salarios minimos",
      "13" = "Mas de 20 a 30 salarios minimos",
      "14" = "Mas de 30 salarios minimos",
      "15" = "Sin ingresos"
  )

  return(list(columns = columns, values = values))
}

#' Obtem o Dicionario de Traducao para Mortalidade (Censo 2010)
#'
#' @description
#' Retorna uma lista contendo dicionarios para traduzir os nomes das colunas
#' e os valores das variaveis categoricas do arquivo de Microdados de Mortalidade
#' do Censo 2010 (IBGE) para o portugues padronizado.
#'
#' @return Uma lista com dois elementos:
#'   - `columns`: um vetor nomeado para traduzir os nomes das colunas.
#'   - `values`: uma lista de vetores nomeados para decodificar valores categoricos.
#'
#' @keywords internal
#' @noRd
get_translate_dictionary_pt_mortality <- function() {

  columns <- c(
    # --- Identificacao e Localizacao ---
    "V0001" = "unidade_federacao",
    "V0002" = "codigo_municipio",
    "V0011" = "area_ponderacao",
    "V0300" = "controle",
    "V0010" = "peso_amostral",

    # --- Regionalizacao ---
    "V1001" = "regiao_geografica",
    "V1002" = "codigo_mesorregiao",
    "V1003" = "codigo_microrregiao",
    "V1004" = "codigo_regiao_metropolitana",
    "V1006" = "situacao_domicilio", # Urbana/Rural
    "V1005" = "situacao_setor",

    # --- Dados do Falecimento ---
    "V0703" = "mes_ano_falecimento",
    "V0704" = "sexo_falecido",
    "V7051" = "idade_falecer_anos",
    "V7052" = "idade_falecer_meses",

    # --- Marcas de Imputacao ---
    "M0703" = "imputacao_mes_ano_falecimento",
    "M0704" = "imputacao_sexo",
    "M7051" = "imputacao_idade_anos",
    "M7052" = "imputacao_idade_meses"
  )

  values <- list(
    "V0001" = c(
      "11"="Rondonia", "12"="Acre", "13"="Amazonas", "14"="Roraima", "15"="Para",
      "16"="Amapa", "17"="Tocantins", "21"="Maranhao", "22"="Piaui", "23"="Ceara",
      "24"="Rio Grande do Norte", "25"="Paraiba", "26"="Pernambuco", "27"="Alagoas",
      "28"="Sergipe", "29"="Bahia", "31"="Minas Gerais", "32"="Espirito Santo",
      "33"="Rio de Janeiro", "35"="Sao Paulo", "41"="Parana", "42"="Santa Catarina",
      "43"="Rio Grande do Sul", "50"="Mato Grosso do Sul", "51"="Mato Grosso",
      "52"="Goias", "53"="Distrito Federal"
    ),
    "V1001" = c(
      "1" = "Norte",
      "2" = "Nordeste",
      "3" = "Sudeste",
      "4" = "Sul",
      "5" = "Centro-Oeste"
    ),
    "V1006" = c(
      "1" = "Urbana",
      "2" = "Rural"
    ),
    "V1005" = c(
      "1" = "Area urbanizada",
      "2" = "Area nao urbanizada",
      "3" = "Area urbanizada isolada",
      "4" = "Area rural de extensao urbana",
      "5" = "Aglomerado rural (povoado)",
      "6" = "Aglomerado rural (nucleo)",
      "7" = "Aglomerado rural (outros)",
      "8" = "Area rural exclusive aglomerado rural"
    ),
    "V0703" = c(
      "01" = "Agosto de 2009",
      "02" = "Setembro de 2009",
      "03" = "Outubro de 2009",
      "04" = "Novembro de 2009",
      "05" = "Dezembro de 2009",
      "06" = "Janeiro de 2010",
      "07" = "Fevereiro de 2010",
      "08" = "Marco de 2010",
      "09" = "Abril de 2010",
      "10" = "Maio de 2010",
      "11" = "Junho de 2010",
      "12" = "Julho de 2010",
      "99" = "Ignorado"
    ),
    "V0704" = c(
      "1" = "Masculino",
      "2" = "Feminino",
      "9" = "Ignorado"
    ),
    # Marcas de Imputacao (Padrao Sim/Nao)
    "M0703" = c("1" = "Sim", "2" = "Nao"),
    "M0704" = c("1" = "Sim", "2" = "Nao"),
    "M7051" = c("1" = "Sim", "2" = "Nao"),
    "M7052" = c("1" = "Sim", "2" = "Nao")
  )

  return(list(columns = columns, values = values))
}

#' Get Translation Dictionary for Mortality (Census 2010)
#'
#' @description
#' Returns a list containing dictionaries to translate column names and 
#' categorical variable values from the 2010 Census Mortality Microdata 
#' (IBGE) into standardized English.
#'
#' @return A list with two elements:
#'   - `columns`: a named vector to translate column names.
#'   - `values`: a list of named vectors to decode categorical values.
#'
#' @keywords internal
#' @noRd
get_translate_dictionary_en_mortality <- function() {

  columns <- c(
    # --- Identification and Location ---
    "V0001" = "federation_unit",
    "V0002" = "municipality_code",
    "V0011" = "weighting_area",
    "V0300" = "control_number",
    "V0010" = "sample_weight",

    # --- Regionalization ---
    "V1001" = "geographic_region",
    "V1002" = "mesoregion_code",
    "V1003" = "microregion_code",
    "V1004" = "metropolitan_area_code",
    "V1006" = "household_situation", # Urban/Rural
    "V1005" = "sector_situation",

    # --- Mortality Data ---
    "V0703" = "month_year_of_death",
    "V0704" = "deceased_sex",
    "V7051" = "age_at_death_years",
    "V7052" = "age_at_death_months",

    # --- Imputation Flags ---
    "M0703" = "imputation_month_year_death",
    "M0704" = "imputation_sex",
    "M7051" = "imputation_age_years",
    "M7052" = "imputation_age_months"
  )

  values <- list(
    "V0001" = c(
      "11"="Rondonia", "12"="Acre", "13"="Amazonas", "14"="Roraima", "15"="Para",
      "16"="Amapa", "17"="Tocantins", "21"="Maranhao", "22"="Piaui", "23"="Ceara",
      "24"="Rio Grande do Norte", "25"="Paraiba", "26"="Pernambuco", "27"="Alagoas",
      "28"="Sergipe", "29"="Bahia", "31"="Minas Gerais", "32"="Espirito Santo",
      "33"="Rio de Janeiro", "35"="Sao Paulo", "41"="Parana", "42"="Santa Catarina",
      "43"="Rio Grande do Sul", "50"="Mato Grosso do Sul", "51"="Mato Grosso",
      "52"="Goias", "53"="Distrito Federal"
    ),
    "V1001" = c(
      "1" = "North", "2" = "Northeast", "3" = "Southeast", "4" = "South", "5" = "Center-West"
    ),
    "V1006" = c(
      "1" = "Urban",
      "2" = "Rural"
    ),
    "V1005" = c(
      "1" = "Urbanized area",
      "2" = "Non-urbanized area",
      "3" = "Isolated urbanized area",
      "4" = "Rural area with urban extension",
      "5" = "Rural settlement (povoado)",
      "6" = "Rural settlement (nucleo)",
      "7" = "Rural settlement (other)",
      "8" = "Rural area excluding settlements"
    ),
    "V0703" = c(
      "01" = "August 2009",
      "02" = "September 2009",
      "03" = "October 2009",
      "04" = "November 2009",
      "05" = "December 2009",
      "06" = "January 2010",
      "07" = "February 2010",
      "08" = "March 2010",
      "09" = "April 2010",
      "10" = "May 2010",
      "11" = "June 2010",
      "12" = "July 2010",
      "99" = "Ignored"
    ),
    "V0704" = c(
      "1" = "Male",
      "2" = "Female",
      "9" = "Ignored"
    ),
    # Imputation Flags
    "M0703" = c("1" = "Yes", "2" = "No"),
    "M0704" = c("1" = "Yes", "2" = "No"),
    "M7051" = c("1" = "Yes", "2" = "No"),
    "M7052" = c("1" = "Yes", "2" = "No")
  )

  return(list(columns = columns, values = values))
}

#' Obtener Diccionario de Traduccion para Mortalidad (Censo 2010)
#'
#' @description
#' Devuelve una lista con diccionarios para traducir los nombres de las columnas
#' y los valores de las variables categoricas del archivo de Microdatos de 
#' Mortalidad del Censo 2010 (IBGE) al espanol estandarizado.
#'
#' @return Una lista con dos elementos:
#'   - `columns`: un vector con nombre para traducir nombres de columnas.
#'   - `values`: una lista de vectores con nombre para decodificar valores.
#'
#' @keywords internal
#' @noRd
get_translate_dictionary_es_mortality <- function() {

  columns <- c(
    # --- Identificacion y Localizacion ---
    "V0001" = "unidad_federacion",
    "V0002" = "codigo_municipio",
    "V0011" = "area_ponderacion",
    "V0300" = "control",
    "V0010" = "peso_muestral",

    # --- Regionalizacion ---
    "V1001" = "region_geografica",
    "V1002" = "codigo_mesorregion",
    "V1003" = "codigo_microrregion",
    "V1004" = "codigo_region_metropolitana",
    "V1006" = "situacion_domicilio", # Urbana/Rural
    "V1005" = "situacion_sector",

    # --- Datos de Fallecimiento ---
    "V0703" = "mes_ano_fallecimiento",
    "V0704" = "sexo_fallecido",
    "V7051" = "edad_fallecer_anos",
    "V7052" = "edad_fallecer_meses",

    # --- Marcas de Imputacion ---
    "M0703" = "imputacion_mes_ano_fallecimiento",
    "M0704" = "imputacion_sexo",
    "M7051" = "imputacion_edad_anos",
    "M7052" = "imputacion_edad_meses"
  )

  values <- list(
    "V0001" = c(
      "11"="Rondonia", "12"="Acre", "13"="Amazonas", "14"="Roraima", "15"="Para",
      "16"="Amapa", "17"="Tocantins", "21"="Maranhao", "22"="Piaui", "23"="Ceara",
      "24"="Rio Grande do Norte", "25"="Paraiba", "26"="Pernambuco", "27"="Alagoas",
      "28"="Sergipe", "29"="Bahia", "31"="Minas Gerais", "32"="Espirito Santo",
      "33"="Rio de Janeiro", "35"="Sao Paulo", "41"="Parana", "42"="Santa Catarina",
      "43"="Rio Grande do Sul", "50"="Mato Grosso do Sur", "51"="Mato Grosso",
      "52"="Goias", "53"="Distrito Federal"
    ),
    "V1001" = c(
      "1" = "Norte", "2" = "Nordeste", "3" = "Sudeste", "4" = "Sur", "5" = "Centro-Oeste"
    ),
    "V1006" = c(
      "1" = "Urbana",
      "2" = "Rural"
    ),
    "V1005" = c(
      "1" = "Area urbanizada",
      "2" = "Area no urbanizada",
      "3" = "Area urbanizada aislada",
      "4" = "Area rural de extension urbana",
      "5" = "Aglomerado rural (poblado)",
      "6" = "Aglomerado rural (nucleo)",
      "7" = "Aglomerado rural (otros)",
      "8" = "Area rural exclusivo aglomerado rural"
    ),
    "V0703" = c(
      "01" = "Agosto de 2009",
      "02" = "Septiembre de 2009",
      "03" = "Octubre de 2009",
      "04" = "Noviembre de 2009",
      "05" = "Diciembre de 2009",
      "06" = "Enero de 2010",
      "07" = "Febrero de 2010",
      "08" = "Marzo de 2010",
      "09" = "Abril de 2010",
      "10" = "Mayo de 2010",
      "11" = "Junio de 2010",
      "12" = "Julio de 2010",
      "99" = "Ignorado"
    ),
    "V0704" = c(
      "1" = "Masculino",
      "2" = "Femenino",
      "9" = "Ignorado"
    ),
    # Marcas de Imputacion
    "M0703" = c("1" = "Si", "2" = "No"),
    "M0704" = c("1" = "Si", "2" = "No"),
    "M7051" = c("1" = "Si", "2" = "No"),
    "M7052" = c("1" = "Si", "2" = "No")
  )

  return(list(columns = columns, values = values))
}


#' Obtem o Dicionario de Traducao para Emigracao (Censo 2010)
#'
#' @description
#' Retorna uma lista contendo dicionarios para traduzir os nomes das colunas
#' e os valores das variaveis categoricas do arquivo de Microdados de Emigracao
#' do Censo 2010 (IBGE) para o portugues padronizado.
#'
#' @return Uma lista com dois elementos:
#'   - `columns`: um vetor nomeado para traduzir os nomes das colunas.
#'   - `values`: uma lista de vetores nomeados para decodificar valores categoricos.
#'
#' @keywords internal
#' @noRd
get_translate_dictionary_pt_emigration <- function() {

  columns <- c(
    # --- Identificacao e Localizacao ---
    "V0001" = "unidade_federacao",
    "V0002" = "codigo_municipio",
    "V0011" = "area_ponderacao",
    "V0300" = "controle",
    "V0010" = "peso_amostral",

    # --- Regionalizacao ---
    "V1001" = "regiao_geografica",
    "V1002" = "codigo_mesorregiao",
    "V1003" = "codigo_microrregiao",
    "V1004" = "codigo_regiao_metropolitana",
    "V1006" = "situacao_domicilio",
    "V1005" = "situacao_setor",

    # --- Dados do Emigrante ---
    "V0303" = "sexo_emigrante",
    "V0304" = "ano_nascimento_emigrante",
    "V0305" = "ano_ultima_partida_emigrante",
    "V3061" = "pais_residencia_julho_2010",

    # --- Marcas de Imputacao ---
    "M0303" = "imputacao_sexo_emigrante",
    "M0304" = "imputacao_ano_nascimento",
    "M0305" = "imputacao_ano_ultima_partida",
    "M3061" = "imputacao_pais_residencia"
  )

  values <- list(
    "V0001" = c(
      "11"="Rondonia", "12"="Acre", "13"="Amazonas", "14"="Roraima", "15"="Para",
      "16"="Amapa", "17"="Tocantins", "21"="Maranhao", "22"="Piaui", "23"="Ceara",
      "24"="Rio Grande do Norte", "25"="Paraiba", "26"="Pernambuco", "27"="Alagoas",
      "28"="Sergipe", "29"="Bahia", "31"="Minas Gerais", "32"="Espirito Santo",
      "33"="Rio de Janeiro", "35"="Sao Paulo", "41"="Parana", "42"="Santa Catarina",
      "43"="Rio Grande do Sul", "50"="Mato Grosso do Sul", "51"="Mato Grosso",
      "52"="Goias", "53"="Distrito Federal"
    ),
    "V1001" = c(
      "1" = "Norte",
      "2" = "Nordeste",
      "3" = "Sudeste",
      "4" = "Sul",
      "5" = "Centro-Oeste"
    ),
    "V1006" = c(
      "1" = "Urbana",
      "2" = "Rural"
    ),
    "V1005" = c(
      "1" = "Area urbanizada",
      "2" = "Area nao urbanizada",
      "3" = "Area urbanizada isolada",
      "4" = "Area rural de extensao urbana",
      "5" = "Aglomerado rural (povoado)",
      "6" = "Aglomerado rural (nucleo)",
      "7" = "Aglomerado rural (outros)",
      "8" = "Area rural exclusive aglomerado rural"
    ),
    "V0303" = c(
      "1" = "Masculino",
      "2" = "Feminino",
      "9" = "Ignorado"
    ),
    # Marcas de Imputacao (Padrao Sim/Nao)
    "M0303" = c("1" = "Sim", "2" = "Nao"),
    "M0304" = c("1" = "Sim", "2" = "Nao"),
    "M0305" = c("1" = "Sim", "2" = "Nao"),
    "M3061" = c("1" = "Sim", "2" = "Nao")
  )

  return(list(columns = columns, values = values))
}

#' Get Translation Dictionary for Emigration (Census 2010)
#'
#' @description
#' Returns a list containing dictionaries to translate column names and 
#' categorical variable values from the 2010 Census Emigration Microdata 
#' (IBGE) into standardized English.
#'
#' @return A list with two elements:
#'   - `columns`: a named vector to translate column names.
#'   - `values`: a list of named vectors to decode categorical values.
#'
#' @keywords internal
#' @noRd
get_translate_dictionary_en_emigration <- function() {

  columns <- c(
    # --- Identification and Location ---
    "V0001" = "federation_unit",
    "V0002" = "municipality_code",
    "V0011" = "weighting_area",
    "V0300" = "control_number",
    "V0010" = "sample_weight",

    # --- Regionalization ---
    "V1001" = "geographic_region",
    "V1002" = "mesoregion_code",
    "V1003" = "microregion_code",
    "V1004" = "metropolitan_area_code",
    "V1006" = "household_situation",
    "V1005" = "sector_situation",

    # --- Emigrant Data ---
    "V0303" = "emigrant_sex",
    "V0304" = "emigrant_birth_year",
    "V0305" = "emigrant_last_departure_year",
    "V3061" = "country_of_residence_july_2010",

    # --- Imputation Flags ---
    "M0303" = "imputation_emigrant_sex",
    "M0304" = "imputation_birth_year",
    "M0305" = "imputation_last_departure_year",
    "M3061" = "imputation_country_of_residence"
  )

  values <- list(
    "V0001" = c(
      "11"="Rondonia", "12"="Acre", "13"="Amazonas", "14"="Roraima", "15"="Para",
      "16"="Amapa", "17"="Tocantins", "21"="Maranhao", "22"="Piaui", "23"="Ceara",
      "24"="Rio Grande do Norte", "25"="Paraiba", "26"="Pernambuco", "27"="Alagoas",
      "28"="Sergipe", "29"="Bahia", "31"="Minas Gerais", "32"="Espirito Santo",
      "33"="Rio de Janeiro", "35"="Sao Paulo", "41"="Parana", "42"="Santa Catarina",
      "43"="Rio Grande do Sul", "50"="Mato Grosso do Sul", "51"="Mato Grosso",
      "52"="Goias", "53"="Distrito Federal"
    ),
    "V1001" = c(
      "1" = "North", "2" = "Northeast", "3" = "Southeast", "4" = "South", "5" = "Center-West"
    ),
    "V1006" = c(
      "1" = "Urban",
      "2" = "Rural"
    ),
    "V1005" = c(
      "1" = "Urbanized area",
      "2" = "Non-urbanized area",
      "3" = "Isolated urbanized area",
      "4" = "Rural area with urban extension",
      "5" = "Rural settlement (povoado)",
      "6" = "Rural settlement (nucleo)",
      "7" = "Rural settlement (other)",
      "8" = "Rural area excluding settlements"
    ),
    "V0303" = c(
      "1" = "Male",
      "2" = "Female",
      "9" = "Ignored"
    ),
    # Imputation Flags
    "M0303" = c("1" = "Yes", "2" = "No"),
    "M0304" = c("1" = "Yes", "2" = "No"),
    "M0305" = c("1" = "Yes", "2" = "No"),
    "M3061" = c("1" = "Yes", "2" = "No")
  )

  return(list(columns = columns, values = values))
}

#' Obtener Diccionario de Traduccion para Emigracion (Censo 2010)
#'
#' @description
#' Devuelve una lista con diccionarios para traducir los nombres de las columnas
#' y los valores de las variables categoricas del archivo de Microdatos de 
#' Emigracion del Censo 2010 (IBGE) al espanol estandarizado.
#'
#' @return Una lista con dos elementos:
#'   - `columns`: un vector con nombre para traducir nombres de columnas.
#'   - `values`: una lista de vectores con nombre para decodificar valores.
#'
#' @keywords internal
#' @noRd
get_translate_dictionary_es_emigration <- function() {

  columns <- c(
    # --- Identificacion y Localizacion ---
    "V0001" = "unidad_federacion",
    "V0002" = "codigo_municipio",
    "V0011" = "area_ponderacion",
    "V0300" = "control",
    "V0010" = "peso_muestral",

    # --- Regionalizacion ---
    "V1001" = "region_geografica",
    "V1002" = "codigo_mesorregion",
    "V1003" = "codigo_microrregion",
    "V1004" = "codigo_region_metropolitana",
    "V1006" = "situacion_domicilio",
    "V1005" = "situacion_sector",

    # --- Datos del Emigrante ---
    "V0303" = "sexo_emigrante",
    "V0304" = "ano_nacimiento_emigrante",
    "V0305" = "ano_ultima_partida_emigrante",
    "V3061" = "pais_residencia_julio_2010",

    # --- Marcas de Imputacion ---
    "M0303" = "imputacion_sexo_emigrante",
    "M0304" = "imputacion_ano_nacimiento",
    "M0305" = "imputacion_ano_ultima_partida",
    "M3061" = "imputacion_pais_residencia"
  )

  values <- list(
    "V0001" = c(
      "11"="Rondonia", "12"="Acre", "13"="Amazonas", "14"="Roraima", "15"="Para",
      "16"="Amapa", "17"="Tocantins", "21"="Maranhao", "22"="Piaui", "23"="Ceara",
      "24"="Rio Grande do Norte", "25"="Paraiba", "26"="Pernambuco", "27"="Alagoas",
      "28"="Sergipe", "29"="Bahia", "31"="Minas Gerais", "32"="Espirito Santo",
      "33"="Rio de Janeiro", "35"="Sao Paulo", "41"="Parana", "42"="Santa Catarina",
      "43"="Rio Grande do Sul", "50"="Mato Grosso do Sur", "51"="Mato Grosso",
      "52"="Goias", "53"="Distrito Federal"
    ),
    "V1001" = c(
      "1" = "Norte", "2" = "Nordeste", "3" = "Sudeste", "4" = "Sur", "5" = "Centro-Oeste"
    ),
    "V1006" = c(
      "1" = "Urbana",
      "2" = "Rural"
    ),
    "V1005" = c(
      "1" = "Area urbanizada",
      "2" = "Area no urbanizada",
      "3" = "Area urbanizada aislada",
      "4" = "Area rural de extension urbana",
      "5" = "Aglomerado rural (poblado)",
      "6" = "Aglomerado rural (nucleo)",
      "7" = "Aglomerado rural (otros)",
      "8" = "Area rural exclusivo aglomerado rural"
    ),
    "V0303" = c(
      "1" = "Masculino",
      "2" = "Femenino",
      "9" = "Ignorado"
    ),
    # Marcas de Imputacion
    "M0303" = c("1" = "Si", "2" = "No"),
    "M0304" = c("1" = "Si", "2" = "No"),
    "M0305" = c("1" = "Si", "2" = "No"),
    "M3061" = c("1" = "Si", "2" = "No")
  )

  return(list(columns = columns, values = values))
}

