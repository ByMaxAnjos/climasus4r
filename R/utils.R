#' Utilities functions for climasus4r
#'
#' @keywords internal
#' @name utils
NULL

#' Get translation dictionary
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

#' Get Translation Dictionary for PT
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


#' Get Translation Dictionary for ES
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