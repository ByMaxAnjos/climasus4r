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
get_translation_dict_en <- function() {
  
  # ==========================================================================
  # COLUMN NAME TRANSLATIONS
  # Organized by category for easy maintenance and team collaboration
  # ==========================================================================
  
  columns <- c(
    
    # ------------------------------------------------------------------------
    # COMMON DEMOGRAPHICS (All Systems)
    # Used across SIM, SINASC, SINAN, SIH
    # ------------------------------------------------------------------------
    "SEXO" = "sex",
    "CS_SEXO" = "sex",
    "RACACOR" = "race",
    "CS_RACA" = "race",
    "ESTCIV" = "marital_status",
    "ESTCIVMAE" = "mother_marital_status",
    
    # ------------------------------------------------------------------------
    # DATES (All Systems)
    # Birth, death, notification, and administrative dates
    # ------------------------------------------------------------------------
    "DTOBITO" = "death_date",
    "HORAOBITO" = "death_time",
    "DTNASC" = "birth_date",
    "DT_NASC" = "birth_date",
    "NASC" = "birth_date",
    "DT_NOTIFIC" = "notification_date",
    "DT_SIN_PRI" = "symptom_date",
    "DT_INTER" = "admission_date",
    "DT_SAIDA" = "discharge_date",
    
    # ------------------------------------------------------------------------
    # AGE VARIABLES (All Systems)
    # Age codes and decomposed age units
    # ------------------------------------------------------------------------
    "IDADE" = "age_code",
    "NU_IDADE_N" = "age",
    "IDADEMAE" = "mother_age",
    "IDADEPAI" = "father_age",
    
    # ------------------------------------------------------------------------
    # GEOGRAPHIC CODES (All Systems)
    # Municipality and region codes for residence, birth, occurrence
    # ------------------------------------------------------------------------
    "CODMUNRES" = "residence_municipality_code",
    "ID_MN_RESI" = "residence_municipality",
    "MUNIC_RES" = "residence_municipality",
    "CODMUNNATU" = "birthplace_municipality_code",
    "CODMUNNASC" = "birth_municipality_code",
    "CODMUNOCOR" = "occurrence_municipality_code",
    "CODUFMUN" = "municipality_code",
    
    # ------------------------------------------------------------------------
    # EDUCATION & OCCUPATION (SIM, SINASC, SINAN)
    # Schooling years and occupation codes
    # ------------------------------------------------------------------------
    "ESC" = "education",
    "ESCMAE" = "mother_education",
    "CS_ESCOL_N" = "education",
    "OCUP" = "occupation",
    "CODOCUPMAE" = "mother_occupation_code",
    "ID_OCUPA_N" = "occupation",
    
    # ------------------------------------------------------------------------
    # ICD/DIAGNOSIS - SIM (Mortality System) - Used by sus_data_filter_cid
    # Underlying and contributing causes of death
    # ------------------------------------------------------------------------
    "CAUSABAS" = "underlying_cause",
    "CAUSABAS_O" = "underlying_cause_original",
    "LINHAA" = "cause_line_a",
    "LINHAB" = "cause_line_b",
    "LINHAC" = "cause_line_c",
    "LINHAD" = "cause_line_d",
    "LINHAII" = "cause_line_ii",
    
    # ------------------------------------------------------------------------
    # ICD/DIAGNOSIS - SIH (Hospital Admissions)
    # Primary and secondary diagnoses for hospitalizations
    # ------------------------------------------------------------------------
    "DIAG_PRINC" = "primary_diagnosis",
    "DIAG_SECUN" = "secondary_diagnosis",
    "DIAGSEC1" = "secondary_diagnosis_1",
    "DIAGSEC2" = "secondary_diagnosis_2",
    "DIAGSEC3" = "secondary_diagnosis_3",
    "DIAGSEC4" = "secondary_diagnosis_4",
    "DIAGSEC5" = "secondary_diagnosis_5",
    "DIAGSEC6" = "secondary_diagnosis_6",
    "DIAGSEC7" = "secondary_diagnosis_7",
    "DIAGSEC8" = "secondary_diagnosis_8",
    "DIAGSEC9" = "secondary_diagnosis_9",
    "CID_MORTE" = "death_cause",
    "CID_NOTIF" = "notifiable_disease",
    
    # ------------------------------------------------------------------------
    # ICD/DIAGNOSIS - SIA (Outpatient)
    # ICD codes for ambulatory procedures
    # ------------------------------------------------------------------------
    "PA_CIDPRI" = "primary_icd",
    "PA_CIDSEC" = "secondary_icd",
    "PA_CIDCAS" = "associated_icd",
    
    # ------------------------------------------------------------------------
    # DEATH CIRCUMSTANCES - SIM
    # Type, location, and circumstances of death
    # ------------------------------------------------------------------------
    "TIPOBITO" = "death_type",
    "LOCOCOR" = "death_location",
    "CIRCOBITO" = "death_circumstance",
    "ACIDTRAB" = "work_accident",
    "FONTE" = "information_source",
    
    # ------------------------------------------------------------------------
    # MATERNAL DEATH - SIM
    # Pregnancy-related death indicators
    # ------------------------------------------------------------------------
    "OBITOGRAV" = "death_during_pregnancy",
    "OBITOPUERP" = "death_during_puerperium",
    "OBITOPARTO" = "death_during_delivery",
    "ASSISTMED" = "medical_assistance",
    "NECROPSIA" = "autopsy",
    
    # ------------------------------------------------------------------------
    # BIRTH INFORMATION - SINASC
    # Place, weight, APGAR, gestation, delivery type
    # ------------------------------------------------------------------------
    "NUMERODV" = "birth_certificate_number",
    "LOCNASC" = "birth_place",
    "PESO" = "birth_weight",
    "APGAR1" = "apgar_1min",
    "APGAR5" = "apgar_5min",
    "GESTACAO" = "gestational_age",
    "SEMAGESTAC" = "gestational_weeks",
    "GRAVIDEZ" = "pregnancy_type",
    "PARTO" = "delivery_type",
    "CONSULTAS" = "prenatal_consultations",
    "DTULTMENST" = "last_menstruation_date",
    "TPMETESTIM" = "gestational_age_estimation_method",
    "TPAPRESENT" = "fetal_presentation",
    "STTRABPART" = "labor_induced",
    "STCESPARTO" = "cesarean_before_labor",
    
    # ------------------------------------------------------------------------
    # MOTHER INFORMATION - SINASC
    # Previous pregnancies, children, race
    # ------------------------------------------------------------------------
    "QTDFILMORT" = "previous_dead_children",
    "QTDGESTANT" = "previous_pregnancies",
    "QTDPARTNOR" = "previous_normal_deliveries",
    "QTDPARTCES" = "previous_cesarean_deliveries",
    "RACACORMAE" = "mother_race",
    "RACACORPAI" = "father_race",
    
    # ------------------------------------------------------------------------
    # CONGENITAL ANOMALY - SINASC
    # Presence and code of congenital anomalies
    # ------------------------------------------------------------------------
    "IDANOMAL" = "congenital_anomaly",
    "CODANOMAL" = "congenital_anomaly_code",
    
    # ------------------------------------------------------------------------
    # SINAN SPECIFIC
    # Notifiable diseases system variables
    # ------------------------------------------------------------------------
    "SEM_NOT" = "notification_week",
    "NU_ANO" = "year",
    "SG_UF_NOT" = "notification_state",
    "ID_AGRAVO" = "disease_code",
    "SEM_PRI" = "symptom_week",
    "CS_GESTANT" = "pregnant",
    "ID_RG_RESI" = "residence_region",
    "ID_PAIS" = "country",
    "DT_INVEST" = "investigation_date",
    "ID_UNIDADE" = "health_unit",
    "DT_DIGITA" = "digitization_date",
    "CLASSI_FIN" = "final_classification",
    "EVOLUCAO" = "outcome",
    "DT_ENCERRA" = "closure_date",
    "DT_OBITO" = "death_date",
    
    # ------------------------------------------------------------------------
    # SIH SPECIFIC (Hospital Admissions)
    # AIH number, procedures, costs, length of stay
    # ------------------------------------------------------------------------
    "N_AIH" = "aih_number",
    "IDENT" = "identification",
    "CEP" = "postal_code",
    "QT_DIARIAS" = "length_of_stay",
    "PROC_REA" = "procedure_performed",
    "VAL_TOT" = "total_value",
    "VAL_UTI" = "icu_value",
    "US_TOT" = "total_daily_rate",
    "DT_ATEND" = "care_date",
    "RACA_COR" = "race",
    "MORTE" = "death",
    "COBRANCA" = "billing_reason",
    "NATUREZA" = "facility_type",
    "GESTAO" = "management_type",
    "MUNIC_MOV" = "movement_municipality",
    "COD_IDADE" = "age_unit",
    
    # ------------------------------------------------------------------------
    # SIA SPECIFIC (Outpatient)
    # Authorization, procedures, costs
    # ------------------------------------------------------------------------
    "PA_AUTORIZ" = "authorization_number",
    "PA_PROC_ID" = "procedure_id",
    "PA_TPUPS" = "facility_type",
    "PA_TIPPRE" = "billing_type",
    "PA_MN_IND" = "individual_municipality",
    "PA_CNPJCPF" = "provider_cnpj_cpf",
    "PA_CNPJMNT" = "maintainer_cnpj",
    "PA_CNSMED" = "physician_cns",
    "PA_CBOCOD" = "occupation_code",
    "PA_QTDPRO" = "procedure_quantity",
    "PA_QTDAPR" = "approved_quantity",
    "PA_VALAPR" = "approved_value",
    "PA_UFDIF" = "different_state",
    "PA_MNDIF" = "different_municipality",
    "PA_DIF_VAL" = "value_difference",
    "IDADEMIN" = "age_min",
    "IDADEMAX" = "age_max",
    "NU_VPA_TOT" = "total_vpa",
    "NU_PA_TOT" = "total_pa",
    "PA_CMP" = "competence",
    "PA_DTATEN" = "care_date",
    
    # ------------------------------------------------------------------------
    # CNES SPECIFIC (Health Establishments)
    # Facility codes, type, management, administrative data
    # ------------------------------------------------------------------------
    "CNES" = "cnes_code",
    "REGSAUDE" = "health_region",
    "MICR_REG" = "micro_region",
    "DISTRSAN" = "sanitary_district",
    "DISTRADM" = "administrative_district",
    "TPGESTAO" = "management_type",
    "PF_PJ" = "legal_entity_type",
    "CPF_CNPJ" = "cpf_cnpj",
    "NIV_HIER" = "hierarchical_level",
    "TP_UNID" = "facility_type",
    "TURNO_AT" = "operating_hours",
    "NIV_ATEN" = "care_level",
    "RETENCAO" = "retention",
    "ATIVIDAD" = "activity",
    "CLIENTEL" = "clientele",
    "TP_PREST" = "provider_type",
    "CO_BANCO" = "bank_code",
    "CO_AGENC" = "agency_code",
    "C_CORREN" = "account_number",
    "CONTRATM" = "contract_month",
    "CONTRATA" = "contract_year",
    "DT_PUBLM" = "publication_month",
    "DT_PUBLA" = "publication_year",
    "DT_EXTRM" = "extraction_month",
    "DT_EXTRA" = "dt_extra",
    "ESFERA_A" = "administrative_sphere",
    "ATIVIDA2" = "activity_2",
    "RETENC2" = "retention_2",
    "NATUREZ2" = "legal_nature_2",
    "CLIENTE2" = "clientele_2",
    "TP_PRES2" = "provider_type_2",
    "NIVATE_A" = "care_level_a",
    
    # ------------------------------------------------------------------------
    # ADMINISTRATIVE - SIM
    # Certificate numbers, coder, system version, dates
    # ------------------------------------------------------------------------
    "NUMERODO" = "death_certificate_number",
    "NATURAL" = "birthplace_municipality",
    "CODESTAB" = "health_facility_code",
    "ATESTANTE" = "certifier",
    "STCODIFICA" = "coding_status",
    "CODIFICADO" = "coded",
    "VERSAOSIST" = "system_version",
    "DTCADASTRO" = "registration_date",
    "DTRECEBIM" = "receipt_date"
  )
  
  
  # ==========================================================================
  # CATEGORICAL VALUE TRANSLATIONS
  # Decode numeric codes to human-readable labels
  # ==========================================================================
  
  values <- list(
    
    # TIPOBITO
    "TIPOBITO" = c(
      "1" = "Fetal",
      "2" = "Non-fetal"
    ),

    # SEXO
    "SEXO" = c(
      "1" = "Male",
      "2" = "Female",
      "0" = "Ignored",
      "3" = "Female"
    ),

    # RACACOR
    "RACACOR" = c(
      "1" = "White",
      "2" = "Black",
      "3" = "Yellow",
      "4" = "Brown",
      "5" = "Indigenous"
    ),

    # ESTCIV
    "ESTCIV" = c(
      "1" = "Single",
      "2" = "Married",
      "3" = "Widowed",
      "4" = "Legally separated",
      "5" = "Common-law marriage",
      "9" = "Ignored"
    ),

    # ESC
    "ESC" = c(
      "1" = "None",
      "2" = "1 to 3 years",
      "3" = "4 to 7 years",
      "4" = "8 to 11 years",
      "5" = "12 years or more",
      "9" = "Ignored"
    ),

    # LOCOCOR
    "LOCOCOR" = c(
      "1" = "Hospital",
      "2" = "Other health facility",
      "3" = "Home",
      "4" = "Public place",
      "5" = "Other",
      "9" = "Ignored"
    ),

    # LOCNASC
    "LOCNASC" = c(
      "1" = "Hospital",
      "2" = "Other health facility",
      "3" = "Home",
      "4" = "Other"
    ),

    # ESTCIVMAE
    "ESTCIVMAE" = c(
      "1" = "Single",
      "2" = "Married",
      "3" = "Widowed",
      "4" = "Legally separated",
      "5" = "Common-law marriage"
    ),

    # ESCMAE
    "ESCMAE" = c(
      "1" = "None",
      "2" = "1 to 3 years",
      "3" = "4 to 7 years",
      "4" = "8 to 11 years",
      "5" = "12 years or more"
    ),

    # GESTACAO
    "GESTACAO" = c(
      "1" = "Less than 22 weeks",
      "2" = "22 to 27 weeks",
      "3" = "28 to 31 weeks",
      "4" = "32 to 36 weeks",
      "5" = "37 to 41 weeks",
      "6" = "42 weeks or more"
    ),

    # GRAVIDEZ
    "GRAVIDEZ" = c(
      "1" = "Single",
      "2" = "Twin",
      "3" = "Triple or more"
    ),

    # PARTO
    "PARTO" = c(
      "1" = "Vaginal",
      "2" = "Cesarean"
    ),

    # CONSULTAS
    "CONSULTAS" = c(
      "1" = "None",
      "2" = "1 to 3",
      "3" = "4 to 6",
      "4" = "7 or more"
    ),

    # RACA_COR
    "RACA_COR" = c(
      "01" = "White",
      "02" = "Black",
      "03" = "Yellow",
      "04" = "Brown",
      "05" = "Indigenous"
    ),

    # MORTE
    "MORTE" = c(
      "0" = "No",
      "1" = "Yes"
    ),

    # CS_SEXO
    "CS_SEXO" = c(
      "M" = "Male",
      "F" = "Female",
      "I" = "Ignored"
    ),

    # CS_RACA
    "CS_RACA" = c(
      "1" = "White",
      "2" = "Black",
      "3" = "Yellow",
      "4" = "Brown",
      "5" = "Indigenous",
      "9" = "Ignored"
    ),

    # CS_GESTANT
    "CS_GESTANT" = c(
      "1" = "1st trimester",
      "2" = "2nd trimester",
      "3" = "3rd trimester",
      "4" = "Gestational age ignored",
      "5" = "Not applicable",
      "6" = "Not pregnant",
      "9" = "Ignored"
    ),

    # EVOLUCAO
    "EVOLUCAO" = c(
      "1" = "Cure",
      "2" = "Death by disease",
      "3" = "Death by other causes",
      "9" = "Ignored"
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
  # Organized by category for easy maintenance and team collaboration
  # ==========================================================================
  
  columns <- c(
    
    # ------------------------------------------------------------------------
    # COMMON DEMOGRAPHICS (All Systems)
    # Used across SIM, SINASC, SINAN, SIH
    # ------------------------------------------------------------------------
    "SEXO" = "sexo",
    "CS_SEXO" = "sexo",
    "RACACOR" = "raca",
    "CS_RACA" = "raca",
    "ESTCIV" = "estado_civil",
    "ESTCIVMAE" = "estado_civil_mae",
    
    # ------------------------------------------------------------------------
    # DATES (All Systems)
    # Birth, death, notification, and administrative dates
    # ------------------------------------------------------------------------
    "DTOBITO" = "data_obito",
    "HORAOBITO" = "hora_obito",
    "DTNASC" = "data_nascimento",
    "DT_NASC" = "data_nascimento",
    "NASC" = "data_nascimento",
    "DT_NOTIFIC" = "data_notificacao",
    "DT_SIN_PRI" = "data_sintomas",
    "DT_INTER" = "data_internacao",
    "DT_SAIDA" = "data_saida",
    
    # ------------------------------------------------------------------------
    # AGE VARIABLES (All Systems)
    # Age codes and decomposed age units
    # ------------------------------------------------------------------------
    "IDADE" = "codigo_idade",
    "NU_IDADE_N" = "idade",
    "IDADEMAE" = "idade_mae",
    "IDADEPAI" = "idade_pai",
    
    # ------------------------------------------------------------------------
    # GEOGRAPHIC CODES (All Systems)
    # Municipality and region codes for residence, birth, occurrence
    # ------------------------------------------------------------------------
    "CODMUNRES" = "codigo_municipio_residencia",
    "ID_MN_RESI" = "municipio_residencia",
    "MUNIC_RES" = "municipio_residencia",
    "CODMUNNATU" = "codigo_municipio_nascimento",
    "CODMUNNASC" = "codigo_municipio_nascimento",
    "CODMUNOCOR" = "codigo_municipio_ocorrencia",
    "CODUFMUN" = "codigo_municipio",
    
    # ------------------------------------------------------------------------
    # EDUCATION & OCCUPATION (SIM, SINASC, SINAN)
    # Schooling years and occupation codes
    # ------------------------------------------------------------------------
    "ESC" = "escolaridade",
    "ESCMAE" = "escolaridade_mae",
    "CS_ESCOL_N" = "escolaridade",
    "OCUP" = "ocupacao",
    "CODOCUPMAE" = "codigo_ocupacao_mae",
    "ID_OCUPA_N" = "ocupacao",
    
    # ------------------------------------------------------------------------
    # ICD/DIAGNOSIS - SIM (Mortality System)
    # Underlying and contributing causes of death
    # ------------------------------------------------------------------------
    "CAUSABAS" = "causa_basica",
    "CAUSABAS_O" = "causa_basica_original",
    "LINHAA" = "linha_causa_a",
    "LINHAB" = "linha_causa_b",
    "LINHAC" = "linha_causa_c",
    "LINHAD" = "linha_causa_d",
    "LINHAII" = "linha_causa_ii",
    
    # ------------------------------------------------------------------------
    # ICD/DIAGNOSIS - SIH (Hospital Admissions)
    # Primary and secondary diagnoses for hospitalizations
    # ------------------------------------------------------------------------
    "DIAG_PRINC" = "diagnostico_principal",
    "DIAG_SECUN" = "diagnostico_secundario",
    "DIAGSEC1" = "diagnostico_secundario_1",
    "DIAGSEC2" = "diagnostico_secundario_2",
    "DIAGSEC3" = "diagnostico_secundario_3",
    "DIAGSEC4" = "diagnostico_secundario_4",
    "DIAGSEC5" = "diagnostico_secundario_5",
    "DIAGSEC6" = "diagnostico_secundario_6",
    "DIAGSEC7" = "diagnostico_secundario_7",
    "DIAGSEC8" = "diagnostico_secundario_8",
    "DIAGSEC9" = "diagnostico_secundario_9",
    "CID_MORTE" = "causa_morte",
    "CID_NOTIF" = "doenca_notificavel",
    
    # ------------------------------------------------------------------------
    # ICD/DIAGNOSIS - SIA (Outpatient)
    # ICD codes for ambulatory procedures
    # ------------------------------------------------------------------------
    "PA_CIDPRI" = "cid_principal",
    "PA_CIDSEC" = "cid_secundario",
    "PA_CIDCAS" = "cid_associado",
    
    # ------------------------------------------------------------------------
    # DEATH CIRCUMSTANCES - SIM
    # Type, location, and circumstances of death
    # ------------------------------------------------------------------------
    "TIPOBITO" = "tipo_obito",
    "LOCOCOR" = "local_ocorrencia",
    "CIRCOBITO" = "circunstancia_obito",
    "ACIDTRAB" = "acidente_trabalho",
    "FONTE" = "fonte_informacao",
    
    # ------------------------------------------------------------------------
    # MATERNAL DEATH - SIM
    # Pregnancy-related death indicators
    # ------------------------------------------------------------------------
    "OBITOGRAV" = "obito_gravidez",
    "OBITOPUERP" = "obito_puerperio",
    "OBITOPARTO" = "obito_parto",
    "ASSISTMED" = "assistencia_medica",
    "NECROPSIA" = "necropsia",
    
    # ------------------------------------------------------------------------
    # BIRTH INFORMATION - SINASC
    # Place, weight, APGAR, gestation, delivery type
    # ------------------------------------------------------------------------
    "NUMERODV" = "numero_declaracao_nascimento",
    "LOCNASC" = "local_nascimento",
    "PESO" = "peso_nascimento",
    "APGAR1" = "apgar_1min",
    "APGAR5" = "apgar_5min",
    "GESTACAO" = "idade_gestacional",
    "SEMAGESTAC" = "semanas_gestacao",
    "GRAVIDEZ" = "tipo_gravidez",
    "PARTO" = "tipo_parto",
    "CONSULTAS" = "consultas_pre_natal",
    "DTULTMENST" = "data_ultima_menstruacao",
    "TPMETESTIM" = "metodo_estimacao_idade_gestacional",
    "TPAPRESENT" = "apresentacao_fetal",
    "STTRABPART" = "trabalho_parto_induzido",
    "STCESPARTO" = "cesareo_antes_trabalho_parto",
    
    # ------------------------------------------------------------------------
    # MOTHER INFORMATION - SINASC
    # Previous pregnancies, children, race
    # ------------------------------------------------------------------------
    "QTDFILMORT" = "filhos_mortos_anteriores",
    "QTDGESTANT" = "gestacoes_anteriores",
    "QTDPARTNOR" = "partos_normais_anteriores",
    "QTDPARTCES" = "partos_cesareos_anteriores",
    "RACACORMAE" = "raca_mae",
    "RACACORPAI" = "raca_pai",
    
    # ------------------------------------------------------------------------
    # CONGENITAL ANOMALY - SINASC
    # Presence and code of congenital anomalies
    # ------------------------------------------------------------------------
    "IDANOMAL" = "anomalia_congenita",
    "CODANOMAL" = "codigo_anomalia_congenita",
    
    # ------------------------------------------------------------------------
    # SINAN SPECIFIC
    # Notifiable diseases system variables
    # ------------------------------------------------------------------------
    "SEM_NOT" = "semana_notificacao",
    "NU_ANO" = "ano",
    "SG_UF_NOT" = "uf_notificacao",
    "ID_AGRAVO" = "codigo_agravo",
    "SEM_PRI" = "semana_sintomas",
    "CS_GESTANT" = "gestante",
    "ID_RG_RESI" = "regiao_residencia",
    "ID_PAIS" = "pais",
    "DT_INVEST" = "data_investigacao",
    "ID_UNIDADE" = "unidade_saude",
    "DT_DIGITA" = "data_digitacao",
    "CLASSI_FIN" = "classificacao_final",
    "EVOLUCAO" = "evolucao",
    "DT_ENCERRA" = "data_encerramento",
    "DT_OBITO" = "data_obito",
    
    # ------------------------------------------------------------------------
    # SIH SPECIFIC (Hospital Admissions)
    # AIH number, procedures, costs, length of stay
    # ------------------------------------------------------------------------
    "N_AIH" = "numero_aih",
    "IDENT" = "identificacao",
    "CEP" = "cep",
    "QT_DIARIAS" = "dias_internacao",
    "PROC_REA" = "procedimento_realizado",
    "VAL_TOT" = "valor_total",
    "VAL_UTI" = "valor_uti",
    "US_TOT" = "diaria_total",
    "DT_ATEND" = "data_atendimento",
    "RACA_COR" = "raca",
    "MORTE" = "obito",
    "COBRANCA" = "motivo_cobranca",
    "NATUREZA" = "natureza_estabelecimento",
    "GESTAO" = "tipo_gestao",
    "MUNIC_MOV" = "municipio_movimentacao",
    "COD_IDADE" = "unidade_idade",
    
    # ------------------------------------------------------------------------
    # SIA SPECIFIC (Outpatient)
    # Authorization, procedures, costs
    # ------------------------------------------------------------------------
    "PA_AUTORIZ" = "numero_autorizacao",
    "PA_PROC_ID" = "codigo_procedimento",
    "PA_TPUPS" = "tipo_estabelecimento",
    "PA_TIPPRE" = "tipo_prestador",
    "PA_MN_IND" = "municipio_individual",
    "PA_CNPJCPF" = "cnpj_cpf_prestador",
    "PA_CNPJMNT" = "cnpj_mantenedor",
    "PA_CNSMED" = "cns_medico",
    "PA_CBOCOD" = "codigo_ocupacao",
    "PA_QTDPRO" = "quantidade_procedimento",
    "PA_QTDAPR" = "quantidade_aprovada",
    "PA_VALAPR" = "valor_aprovado",
    "PA_UFDIF" = "uf_diferente",
    "PA_MNDIF" = "municipio_diferente",
    "PA_DIF_VAL" = "diferenca_valor",
    "IDADEMIN" = "idade_minima",
    "IDADEMAX" = "idade_maxima",
    "NU_VPA_TOT" = "vpa_total",
    "NU_PA_TOT" = "pa_total",
    "PA_CMP" = "competencia",
    "PA_DTATEN" = "data_atendimento",
    
    # ------------------------------------------------------------------------
    # CNES SPECIFIC (Health Establishments)
    # Facility codes, type, management, administrative data
    # ------------------------------------------------------------------------
    "CNES" = "codigo_cnes",
    "REGSAUDE" = "regiao_saude",
    "MICR_REG" = "micro_regiao",
    "DISTRSAN" = "distrito_sanitario",
    "DISTRADM" = "distrito_administrativo",
    "TPGESTAO" = "tipo_gestao",
    "PF_PJ" = "tipo_pessoa_juridica",
    "CPF_CNPJ" = "cpf_cnpj",
    "NIV_HIER" = "nivel_hierarquico",
    "TP_UNID" = "tipo_unidade",
    "TURNO_AT" = "turno_atendimento",
    "NIV_ATEN" = "nivel_atencao",
    "RETENCAO" = "retencao",
    "ATIVIDAD" = "atividade",
    "CLIENTEL" = "clientela",
    "TP_PREST" = "tipo_prestador",
    "CO_BANCO" = "codigo_banco",
    "CO_AGENC" = "codigo_agencia",
    "C_CORREN" = "conta_corrente",
    "CONTRATM" = "mes_contrato",
    "CONTRATA" = "ano_contrato",
    "DT_PUBLM" = "mes_publicacao",
    "DT_PUBLA" = "ano_publicacao",
    "DT_EXTRM" = "mes_extracao",
    "DT_EXTRA" = "dt_extra",
    "ESFERA_A" = "esfera_administrativa",
    "ATIVIDA2" = "atividade_2",
    "RETENC2" = "retencao_2",
    "NATUREZ2" = "natureza_juridica_2",
    "CLIENTE2" = "clientela_2",
    "TP_PRES2" = "tipo_prestador_2",
    "NIVATE_A" = "nivel_atencao_a",
    
    # ------------------------------------------------------------------------
    # ADMINISTRATIVE - SIM
    # Certificate numbers, coder, system version, dates
    # ------------------------------------------------------------------------
    "NUMERODO" = "numero_declaracao_obito",
    "NATURAL" = "municipio_nascimento",
    "CODESTAB" = "codigo_estabelecimento",
    "ATESTANTE" = "atestante",
    "STCODIFICA" = "status_codificacao",
    "CODIFICADO" = "codificado",
    "VERSAOSIST" = "versao_sistema",
    "DTCADASTRO" = "data_cadastro",
    "DTRECEBIM" = "data_recebimento"
  )
  
  
  # ==========================================================================
  # CATEGORICAL VALUE TRANSLATIONS
  # Decode numeric codes to human-readable labels
  # ==========================================================================
  
  values <- list(
    
    # TIPOBITO
    "TIPOBITO" = c(
      "1" = "Fetal",
      "2" = "Nao fetal"
    ),

    # SEXO
    "SEXO" = c(
      "1" = "Masculino",
      "2" = "Feminino",
      "0" = "Ignorado",
      "3" = "Feminino"
    ),

    # RACACOR
    "RACACOR" = c(
      "1" = "Branca",
      "2" = "Preta",
      "3" = "Amarela",
      "4" = "Parda",
      "5" = "Indigena"
    ),

    # ESTCIV
    "ESTCIV" = c(
      "1" = "Solteiro",
      "2" = "Casado",
      "3" = "Viuvo",
      "4" = "Separado judicialmente",
      "5" = "Uniao consensual",
      "9" = "Ignorado"
    ),

    # ESC
    "ESC" = c(
      "1" = "Nenhum",
      "2" = "1 a 3 anos",
      "3" = "4 a 7 anos",
      "4" = "8 a 11 anos",
      "5" = "12 anos ou mais",
      "9" = "Ignorado"
    ),

    # LOCOCOR
    "LOCOCOR" = c(
      "1" = "Hospital",
      "2" = "Outro estabelecimento de saude",
      "3" = "Domicilio",
      "4" = "Via publica",
      "5" = "Outros",
      "9" = "Ignorado"
    ),

    # LOCNASC
    "LOCNASC" = c(
      "1" = "Hospital",
      "2" = "Outro estabelecimento de saude",
      "3" = "Domicilio",
      "4" = "Outros"
    ),

    # ESTCIVMAE
    "ESTCIVMAE" = c(
      "1" = "Solteira",
      "2" = "Casada",
      "3" = "Viuva",
      "4" = "Separada judicialmente",
      "5" = "Uniao consensual"
    ),

    # ESCMAE
    "ESCMAE" = c(
      "1" = "Nenhum",
      "2" = "1 a 3 anos",
      "3" = "4 a 7 anos",
      "4" = "8 a 11 anos",
      "5" = "12 anos ou mais"
    ),

    # GESTACAO
    "GESTACAO" = c(
      "1" = "Menos de 22 semanas",
      "2" = "22 a 27 semanas",
      "3" = "28 a 31 semanas",
      "4" = "32 a 36 semanas",
      "5" = "37 a 41 semanas",
      "6" = "42 semanas ou mais"
    ),

    # GRAVIDEZ
    "GRAVIDEZ" = c(
      "1" = "Unica",
      "2" = "Dupla",
      "3" = "Tripla e mais"
    ),

    # PARTO
    "PARTO" = c(
      "1" = "Vaginal",
      "2" = "Cesareo"
    ),

    # CONSULTAS
    "CONSULTAS" = c(
      "1" = "Nenhuma",
      "2" = "1 a 3 vezes",
      "3" = "4 a 6 vezes",
      "4" = "7 ou mais vezes"
    ),

    # RACA_COR
    "RACA_COR" = c(
      "01" = "Branca",
      "02" = "Preta",
      "03" = "Amarela",
      "04" = "Parda",
      "05" = "Indigena"
    ),

    # MORTE
    "MORTE" = c(
      "0" = "Nao",
      "1" = "Sim"
    ),

    # CS_SEXO
    "CS_SEXO" = c(
      "M" = "Masculino",
      "F" = "Feminino",
      "I" = "Ignorado"
    ),

    # CS_RACA
    "CS_RACA" = c(
      "1" = "Branca",
      "2" = "Preta",
      "3" = "Amarela",
      "4" = "Parda",
      "5" = "Indigena",
      "9" = "Ignorado"
    ),

    # CS_GESTANT
    "CS_GESTANT" = c(
      "1" = "1o trimestre",
      "2" = "2o trimestre",
      "3" = "3o trimestre",
      "4" = "Idade gestacional ignorada",
      "5" = "Nao se aplica",
      "6" = "Nao gestante",
      "9" = "Ignorado"
    ),

    # EVOLUCAO
    "EVOLUCAO" = c(
      "1" = "Cura",
      "2" = "Obito pela doenca",
      "3" = "Obito por outras causas",
      "9" = "Ignorado"
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
    # COMMON DEMOGRAPHICS (All Systems)
    # Used across SIM, SINASC, SINAN, SIH
    # ------------------------------------------------------------------------
    "SEXO" = "sexo",
    "CS_SEXO" = "sexo",
    "RACACOR" = "raza",
    "CS_RACA" = "raza",
    "ESTCIV" = "estado_civil",
    "ESTCIVMAE" = "estado_civil_madre",
    
    # ------------------------------------------------------------------------
    # DATES (All Systems)
    # Birth, death, notification, and administrative dates
    # ------------------------------------------------------------------------
    "DTOBITO" = "fecha_muerte",
    "HORAOBITO" = "hora_muerte",
    "DTNASC" = "fecha_nacimiento",
    "DT_NASC" = "fecha_nacimiento",
    "NASC" = "fecha_nacimiento",
    "DT_NOTIFIC" = "fecha_notificacion",
    "DT_SIN_PRI" = "fecha_sintomas",
    "DT_INTER" = "fecha_internacion",
    "DT_SAIDA" = "fecha_alta",
    
    # ------------------------------------------------------------------------
    # AGE VARIABLES (All Systems)
    # Age codes and decomposed age units
    # ------------------------------------------------------------------------
    "IDADE" = "codigo_edad",
    "NU_IDADE_N" = "edad",
    "IDADEMAE" = "edad_madre",
    "IDADEPAI" = "edad_padre",
    
    # ------------------------------------------------------------------------
    # GEOGRAPHIC CODES (All Systems)
    # Municipality and region codes for residence, birth, occurrence
    # ------------------------------------------------------------------------
    "CODMUNRES" = "codigo_municipio_residencia",
    "ID_MN_RESI" = "municipio_residencia",
    "MUNIC_RES" = "municipio_residencia",
    "CODMUNNATU" = "codigo_municipio_nacimiento",
    "CODMUNNASC" = "codigo_municipio_nacimiento",
    "CODMUNOCOR" = "codigo_municipio_ocurrencia",
    "CODUFMUN" = "codigo_municipio",
    
    # ------------------------------------------------------------------------
    # EDUCATION & OCCUPATION (SIM, SINASC, SINAN)
    # Schooling years and occupation codes
    # ------------------------------------------------------------------------
    "ESC" = "escolaridad",
    "ESCMAE" = "escolaridad_madre",
    "CS_ESCOL_N" = "escolaridad",
    "OCUP" = "ocupacion",
    "CODOCUPMAE" = "codigo_ocupacion_madre",
    "ID_OCUPA_N" = "ocupacion",
    
    # ------------------------------------------------------------------------
    # ICD/DIAGNOSIS - SIM (Mortality System)
    # Underlying and contributing causes of death
    # ------------------------------------------------------------------------
    "CAUSABAS" = "causa_basica",
    "CAUSABAS_O" = "causa_basica_original",
    "LINHAA" = "linea_causa_a",
    "LINHAB" = "linea_causa_b",
    "LINHAC" = "linea_causa_c",
    "LINHAD" = "linea_causa_d",
    "LINHAII" = "linea_causa_ii",
    
    # ------------------------------------------------------------------------
    # ICD/DIAGNOSIS - SIH (Hospital Admissions)
    # Primary and secondary diagnoses for hospitalizations
    # ------------------------------------------------------------------------
    "DIAG_PRINC" = "diagnostico_principal",
    "DIAG_SECUN" = "diagnostico_secundario",
    "DIAGSEC1" = "diagnostico_secundario_1",
    "DIAGSEC2" = "diagnostico_secundario_2",
    "DIAGSEC3" = "diagnostico_secundario_3",
    "DIAGSEC4" = "diagnostico_secundario_4",
    "DIAGSEC5" = "diagnostico_secundario_5",
    "DIAGSEC6" = "diagnostico_secundario_6",
    "DIAGSEC7" = "diagnostico_secundario_7",
    "DIAGSEC8" = "diagnostico_secundario_8",
    "DIAGSEC9" = "diagnostico_secundario_9",
    "CID_MORTE" = "causa_muerte",
    "CID_NOTIF" = "enfermedad_notificable",
    
    # ------------------------------------------------------------------------
    # ICD/DIAGNOSIS - SIA (Outpatient)
    # ICD codes for ambulatory procedures
    # ------------------------------------------------------------------------
    "PA_CIDPRI" = "cie_principal",
    "PA_CIDSEC" = "cie_secundario",
    "PA_CIDCAS" = "cie_asociado",
    
    # ------------------------------------------------------------------------
    # DEATH CIRCUMSTANCES - SIM
    # Type, location, and circumstances of death
    # ------------------------------------------------------------------------
    "TIPOBITO" = "tipo_muerte",
    "LOCOCOR" = "lugar_ocurrencia",
    "CIRCOBITO" = "circunstancia_muerte",
    "ACIDTRAB" = "accidente_trabajo",
    "FONTE" = "fuente_informacion",
    
    # ------------------------------------------------------------------------
    # MATERNAL DEATH - SIM
    # Pregnancy-related death indicators
    # ------------------------------------------------------------------------
    "OBITOGRAV" = "muerte_embarazo",
    "OBITOPUERP" = "muerte_puerperio",
    "OBITOPARTO" = "muerte_parto",
    "ASSISTMED" = "asistencia_medica",
    "NECROPSIA" = "necropsia",
    
    # ------------------------------------------------------------------------
    # BIRTH INFORMATION - SINASC
    # Place, weight, APGAR, gestation, delivery type
    # ------------------------------------------------------------------------
    "NUMERODV" = "numero_certificado_nacimiento",
    "LOCNASC" = "lugar_nacimiento",
    "PESO" = "peso_nacimiento",
    "APGAR1" = "apgar_1min",
    "APGAR5" = "apgar_5min",
    "GESTACAO" = "edad_gestacional",
    "SEMAGESTAC" = "semanas_gestacion",
    "GRAVIDEZ" = "tipo_embarazo",
    "PARTO" = "tipo_parto",
    "CONSULTAS" = "consultas_prenatales",
    "DTULTMENST" = "fecha_ultima_menstruacion",
    "TPMETESTIM" = "metodo_estimacion_edad_gestacional",
    "TPAPRESENT" = "presentacion_fetal",
    "STTRABPART" = "trabajo_parto_inducido",
    "STCESPARTO" = "cesarea_antes_trabajo_parto",
    
    # ------------------------------------------------------------------------
    # MOTHER INFORMATION - SINASC
    # Previous pregnancies, children, race
    # ------------------------------------------------------------------------
    "QTDFILMORT" = "hijos_muertos_anteriores",
    "QTDGESTANT" = "embarazos_anteriores",
    "QTDPARTNOR" = "partos_normales_anteriores",
    "QTDPARTCES" = "partos_cesareos_anteriores",
    "RACACORMAE" = "raza_madre",
    "RACACORPAI" = "raza_padre",
    
    # ------------------------------------------------------------------------
    # CONGENITAL ANOMALY - SINASC
    # Presence and code of congenital anomalies
    # ------------------------------------------------------------------------
    "IDANOMAL" = "anomalia_congenita",
    "CODANOMAL" = "codigo_anomalia_congenita",
    
    # ------------------------------------------------------------------------
    # SINAN SPECIFIC
    # Notifiable diseases system variables
    # ------------------------------------------------------------------------
    "SEM_NOT" = "semana_notificacion",
    "NU_ANO" = "ano",
    "SG_UF_NOT" = "estado_notificacion",
    "ID_AGRAVO" = "codigo_enfermedad",
    "SEM_PRI" = "semana_sintomas",
    "CS_GESTANT" = "embarazada",
    "ID_RG_RESI" = "region_residencia",
    "ID_PAIS" = "pais",
    "DT_INVEST" = "fecha_investigacion",
    "ID_UNIDADE" = "unidad_salud",
    "DT_DIGITA" = "fecha_digitacion",
    "CLASSI_FIN" = "clasificacion_final",
    "EVOLUCAO" = "evolucion",
    "DT_ENCERRA" = "fecha_cierre",
    "DT_OBITO" = "fecha_muerte",
    
    # ------------------------------------------------------------------------
    # SIH SPECIFIC (Hospital Admissions)
    # AIH number, procedures, costs, length of stay
    # ------------------------------------------------------------------------
    "N_AIH" = "numero_aih",
    "IDENT" = "identificacion",
    "CEP" = "codigo_postal",
    "QT_DIARIAS" = "dias_internacion",
    "PROC_REA" = "procedimiento_realizado",
    "VAL_TOT" = "valor_total",
    "VAL_UTI" = "valor_uci",
    "US_TOT" = "diaria_total",
    "DT_ATEND" = "fecha_atencion",
    "RACA_COR" = "raza",
    "MORTE" = "muerte",
    "COBRANCA" = "motivo_cobro",
    "NATUREZA" = "naturaleza_establecimiento",
    "GESTAO" = "tipo_gestion",
    "MUNIC_MOV" = "municipio_movimiento",
    "COD_IDADE" = "unidad_edad",
    
    # ------------------------------------------------------------------------
    # SIA SPECIFIC (Outpatient)
    # Authorization, procedures, costs
    # ------------------------------------------------------------------------
    "PA_AUTORIZ" = "numero_autorizacion",
    "PA_PROC_ID" = "codigo_procedimiento",
    "PA_TPUPS" = "tipo_establecimiento",
    "PA_TIPPRE" = "tipo_prestador",
    "PA_MN_IND" = "municipio_individual",
    "PA_CNPJCPF" = "cnpj_cpf_prestador",
    "PA_CNPJMNT" = "cnpj_mantenedor",
    "PA_CNSMED" = "cns_medico",
    "PA_CBOCOD" = "codigo_ocupacion",
    "PA_QTDPRO" = "cantidad_procedimiento",
    "PA_QTDAPR" = "cantidad_aprobada",
    "PA_VALAPR" = "valor_aprobado",
    "PA_UFDIF" = "estado_diferente",
    "PA_MNDIF" = "municipio_diferente",
    "PA_DIF_VAL" = "diferencia_valor",
    "IDADEMIN" = "edad_minima",
    "IDADEMAX" = "edad_maxima",
    "NU_VPA_TOT" = "vpa_total",
    "NU_PA_TOT" = "pa_total",
    "PA_CMP" = "competencia",
    "PA_DTATEN" = "fecha_atencion",
    
    # ------------------------------------------------------------------------
    # CNES SPECIFIC (Health Establishments)
    # Facility codes, type, management, administrative data
    # ------------------------------------------------------------------------
    "CNES" = "codigo_cnes",
    "REGSAUDE" = "region_salud",
    "MICR_REG" = "micro_region",
    "DISTRSAN" = "distrito_sanitario",
    "DISTRADM" = "distrito_administrativo",
    "TPGESTAO" = "tipo_gestion",
    "PF_PJ" = "tipo_persona_juridica",
    "CPF_CNPJ" = "cpf_cnpj",
    "NIV_HIER" = "nivel_jerarquico",
    "TP_UNID" = "tipo_unidad",
    "TURNO_AT" = "turno_atencion",
    "NIV_ATEN" = "nivel_atencion",
    "RETENCAO" = "retencion",
    "ATIVIDAD" = "actividad",
    "CLIENTEL" = "clientela",
    "TP_PREST" = "tipo_prestador",
    "CO_BANCO" = "codigo_banco",
    "CO_AGENC" = "codigo_agencia",
    "C_CORREN" = "cuenta_corriente",
    "CONTRATM" = "mes_contrato",
    "CONTRATA" = "ano_contrato",
    "DT_PUBLM" = "mes_publicacion",
    "DT_PUBLA" = "ano_publicacion",
    "DT_EXTRM" = "mes_extraccion",
    "DT_EXTRA" = "dt_extra",
    "ESFERA_A" = "esfera_administrativa",
    "ATIVIDA2" = "actividad_2",
    "RETENC2" = "retencion_2",
    "NATUREZ2" = "naturaleza_juridica_2",
    "CLIENTE2" = "clientela_2",
    "TP_PRES2" = "tipo_prestador_2",
    "NIVATE_A" = "nivel_atencion_a",
    
    # ------------------------------------------------------------------------
    # ADMINISTRATIVE - SIM
    # Certificate numbers, coder, system version, dates
    # ------------------------------------------------------------------------
    "NUMERODO" = "numero_certificado_muerte",
    "NATURAL" = "municipio_nacimiento",
    "CODESTAB" = "codigo_establecimiento",
    "ATESTANTE" = "certificador",
    "STCODIFICA" = "estado_codificacion",
    "CODIFICADO" = "codificado",
    "VERSAOSIST" = "version_sistema",
    "DTCADASTRO" = "fecha_registro",
    "DTRECEBIM" = "fecha_recepcion"
  )
  
  
  # ==========================================================================
  # CATEGORICAL VALUE TRANSLATIONS
  # Decode numeric codes to human-readable labels
  # ==========================================================================
  
  values <- list(
    
    # TIPOBITO
    "TIPOBITO" = c(
      "1" = "Fetal",
      "2" = "No fetal"
    ),

    # SEXO
    "SEXO" = c(
      "1" = "Masculino",
      "2" = "Femenino",
      "0" = "Ignorado",
      "3" = "Femenino"
    ),

    # RACACOR
    "RACACOR" = c(
      "1" = "Blanca",
      "2" = "Negra",
      "3" = "Amarilla",
      "4" = "Parda",
      "5" = "Indigena"
    ),

    # ESTCIV
    "ESTCIV" = c(
      "1" = "Soltero",
      "2" = "Casado",
      "3" = "Viudo",
      "4" = "Separado judicialmente",
      "5" = "Union consensual",
      "9" = "Ignorado"
    ),

    # ESC
    "ESC" = c(
      "1" = "Ninguno",
      "2" = "1 a 3 anos",
      "3" = "4 a 7 anos",
      "4" = "8 a 11 anos",
      "5" = "12 anos o mas",
      "9" = "Ignorado"
    ),

    # LOCOCOR
    "LOCOCOR" = c(
      "1" = "Hospital",
      "2" = "Otro establecimiento de salud",
      "3" = "Domicilio",
      "4" = "Via publica",
      "5" = "Otros",
      "9" = "Ignorado"
    ),

    # LOCNASC
    "LOCNASC" = c(
      "1" = "Hospital",
      "2" = "Otro establecimiento de salud",
      "3" = "Domicilio",
      "4" = "Otros"
    ),

    # ESTCIVMAE
    "ESTCIVMAE" = c(
      "1" = "Soltera",
      "2" = "Casada",
      "3" = "Viuda",
      "4" = "Separada judicialmente",
      "5" = "Union consensual"
    ),

    # ESCMAE
    "ESCMAE" = c(
      "1" = "Ninguno",
      "2" = "1 a 3 anos",
      "3" = "4 a 7 anos",
      "4" = "8 a 11 anos",
      "5" = "12 anos o mas"
    ),

    # GESTACAO
    "GESTACAO" = c(
      "1" = "Menos de 22 semanas",
      "2" = "22 a 27 semanas",
      "3" = "28 a 31 semanas",
      "4" = "32 a 36 semanas",
      "5" = "37 a 41 semanas",
      "6" = "42 semanas o mas"
    ),

    # GRAVIDEZ
    "GRAVIDEZ" = c(
      "1" = "Unica",
      "2" = "Doble",
      "3" = "Triple y mas"
    ),

    # PARTO
    "PARTO" = c(
      "1" = "Vaginal",
      "2" = "Cesarea"
    ),

    # CONSULTAS
    "CONSULTAS" = c(
      "1" = "Ninguna",
      "2" = "1 a 3 veces",
      "3" = "4 a 6 veces",
      "4" = "7 o mas veces"
    ),

    # RACA_COR
    "RACA_COR" = c(
      "01" = "Blanca",
      "02" = "Negra",
      "03" = "Amarilla",
      "04" = "Parda",
      "05" = "Indigena"
    ),

    # MORTE
    "MORTE" = c(
      "0" = "No",
      "1" = "Si"
    ),

    # CS_SEXO
    "CS_SEXO" = c(
      "M" = "Masculino",
      "F" = "Femenino",
      "I" = "Ignorado"
    ),

    # CS_RACA
    "CS_RACA" = c(
      "1" = "Blanca",
      "2" = "Negra",
      "3" = "Amarilla",
      "4" = "Parda",
      "5" = "Indigena",
      "9" = "Ignorado"
    ),

    # CS_GESTANT
    "CS_GESTANT" = c(
      "1" = "1er trimestre",
      "2" = "2do trimestre",
      "3" = "3er trimestre",
      "4" = "Edad gestacional ignorada",
      "5" = "No aplica",
      "6" = "No embarazada",
      "9" = "Ignorado"
    ),

    # EVOLUCAO
    "EVOLUCAO" = c(
      "1" = "Cura",
      "2" = "Muerte por la enfermedad",
      "3" = "Muerte por otras causas",
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
      pt = "Vitimas de raios, terremotos, inundacoes, tempestades",
      en = "Victims of lightning, earthquakes, floods, storms",
      es = "Victimas de rayos, terremotos, inundaciones, tormentas"
    ),
    climate_sensitive = TRUE,
    climate_factors = c("extreme_weather", "precipitation", "flooding")
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