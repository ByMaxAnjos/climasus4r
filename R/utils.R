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
#' @param lang Character. Language code: "pt" (Portuguese), "es" (Spanish), or "en" (English).
#'
#' @return A list containing translation mappings.
#' @keywords internal
get_translation_dict <- function(lang = "en") {
  
  # Validate language
  if (!lang %in% c("en", "pt", "es")) {
    warning(sprintf("Language '%s' not supported. Defaulting to English.", lang))
    lang <- "en"
  }
  
  # Column name translations - Comprehensive coverage for all systems
  column_translations <- list(
    en = list(
      # Common demographics (all systems)
      "SEXO" = "sex",
      "CS_SEXO" = "sex",
      "RACACOR" = "race",
      "CS_RACA" = "race",
      "ESTCIV" = "marital_status",
      "ESTCIVMAE" = "mother_marital_status",
      
      # Dates (all systems)
      "DTOBITO" = "death_date",
      "DTNASC" = "birth_date",
      "DT_NASC" = "birth_date",
      "NASC" = "birth_date",
      "DT_NOTIFIC" = "notification_date",
      "DT_DIAG" = "diagnosis_date",
      "DTNASC_MAE" = "mother_birth_date",
      "HORAOBITO" = "death_time",
      
      # Age (all systems)
      "IDADE" = "age_code",
      "NU_IDADE_N" = "age_years",
      "IDADEMAE" = "mother_age",
      "IDADEanos" = "age_years",
      "IDADEmeses" = "age_months",
      "IDADEdias" = "age_days",
      "IDADEhoras" = "age_hours",
      "IDADEminutos" = "age_minutes",
      
      # Geographic (all systems)
      "CODMUNRES" = "residence_municipality_code",
      "ID_MN_RESI" = "residence_municipality_code",
      "MUNIC_RES" = "residence_municipality_code",
      "CODMUNNATU" = "birth_municipality_code",
      "CODMUNNASC" = "birth_municipality_code",
      "ID_MUNICIP" = "notification_municipality_code",
      "CODMUNOCOR" = "occurrence_municipality_code",
      "munResNome" = "residence_municipality_name",
      "munResUf" = "residence_state",
      "munResLat" = "residence_latitude",
      "munResLon" = "residence_longitude",
      
      # ICD/Diagnosis - SIM
      "CAUSABAS" = "underlying_cause",
      "LINHAA" = "cause_line_a",
      "LINHAB" = "cause_line_b",
      "LINHAC" = "cause_line_c",
      "LINHAD" = "cause_line_d",
      "LINHAII" = "cause_line_ii",
      "CAUSABAS_O" = "underlying_cause_original",
      "CAUSAMAT" = "maternal_cause",
      
      # ICD/Diagnosis - SIH
      "DIAG_PRINC" = "primary_diagnosis",
      "DIAG_SECUN" = "secondary_diagnosis",
      "CID_MORTE" = "death_cause",
      "CID_NOTIF" = "notified_cause",
      "CID_ASSO" = "associated_cause",
      "DIAGSEC1" = "secondary_diagnosis_1",
      "DIAGSEC2" = "secondary_diagnosis_2",
      "DIAGSEC3" = "secondary_diagnosis_3",
      "DIAGSEC4" = "secondary_diagnosis_4",
      "DIAGSEC5" = "secondary_diagnosis_5",
      "DIAGSEC6" = "secondary_diagnosis_6",
      "DIAGSEC7" = "secondary_diagnosis_7",
      "DIAGSEC8" = "secondary_diagnosis_8",
      "DIAGSEC9" = "secondary_diagnosis_9",
      
      # ICD/Diagnosis - SIA
      "PA_CIDPRI" = "primary_icd",
      "PA_CIDSEC" = "secondary_icd",
      "PA_CIDCAS" = "associated_icd",
      
      # Education (SIM, SINASC, SINAN)
      "ESC" = "education",
      "ESC2010" = "education_2010",
      "ESCMAE" = "mother_education",
      "ESCMAE2010" = "mother_education_2010",
      "CS_ESCOL_N" = "education",
      "ESCMAEAGR1" = "mother_education_grouped",
      "ESCFALAGR1" = "education_grouped",
      
      # Occupation (SIM, SINASC)
      "OCUP" = "occupation",
      "OCUPMAE" = "mother_occupation",
      
      # Death type - SIM
      "TIPOBITO" = "death_type",
      "TPMORTEOCO" = "death_occurrence_type",
      "LOCOCOR" = "occurrence_place",
      "CIRCOBITO" = "death_circumstance",
      "ACIDTRAB" = "work_accident",
      "TPOBITOCOR" = "corrected_death_type",
      
      # Birth - SINASC
      "LOCNASC" = "birth_place",
      "PESO" = "weight",
      "APGAR1" = "apgar_1min",
      "APGAR5" = "apgar_5min",
      "GESTACAO" = "gestation_weeks",
      "SEMAGESTAC" = "gestation_weeks",
      "GRAVIDEZ" = "pregnancy_type",
      "PARTO" = "delivery_type",
      "QTDFILVIVO" = "living_children",
      "QTDFILMORT" = "dead_children",
      "OBITOPARTO" = "death_during_delivery",
      "OBITOGRAV" = "death_during_pregnancy",
      "OBITOPUERP" = "death_during_puerperium",
      
      # SINAN specific
      "TP_NOT" = "notification_type",
      "ID_AGRAVO" = "disease_code",
      "CS_GESTANT" = "pregnant",
      "SEM_NOT" = "notification_week",
      "SEM_DIAG" = "diagnosis_week",
      "ANO_NASC" = "birth_year",
      "NU_ANO" = "year",
      "SG_UF_NOT" = "notification_state",
      "SG_UF" = "state",
      "ID_REGIONA" = "regional_code",
      "ID_RG_RESI" = "residence_regional_code",
      "ID_UNIDADE" = "health_unit_code",
      
      # SIH specific
      "N_AIH" = "aih_number",
      "PROC_REA" = "procedure_performed",
      "PROC_SOLIC" = "procedure_requested",
      "QT_DIARIAS" = "hospital_days",
      "VAL_TOT" = "total_value",
      "VAL_SH" = "hospital_service_value",
      "VAL_SP" = "professional_service_value",
      "VAL_SADT" = "diagnostic_therapeutic_value",
      "UTI_MES_TO" = "icu_total_months",
      "UTI_INT_TO" = "icu_total_days",
      "MARCA_UTI" = "icu_marker",
      "ANO_CMPT" = "competence_year",
      "MES_CMPT" = "competence_month",
      "ESPEC" = "specialty",
      
      # SIA specific
      "PA_PROC_ID" = "procedure_code",
      "PA_TPFIN" = "financing_type",
      "PA_SUBFIN" = "sub_financing",
      "PA_AUTORIZ" = "authorization",
      "PA_CNSMED" = "physician_cns",
      "PA_CBOCOD" = "cbo_code",
      "PA_MOTSAI" = "exit_reason",
      "PA_OBITO" = "death",
      "PA_ENCERR" = "closure",
      
      # CNES specific
      "CNES" = "cnes_code",
      "CODUFMUN" = "municipality_code",
      "TP_UNID" = "unit_type",
      "NATUREZA" = "legal_nature",
      "ESFERA_A" = "administrative_sphere",
      "TPGESTAO" = "management_type",
      "NIV_HIER" = "hierarchical_level",
      "TURNO_AT" = "service_hours",
      "CLIENTEL" = "clientele",
      "REGSAUDE" = "health_region",
      "MICR_REG" = "micro_region",
      
      # Establishment codes
      "CODESTAB" = "establishment_code",
      "ESTABDESCR" = "establishment_description",
      
      # Other common fields
      "ORIGEM" = "origin",
      "CONTADOR" = "counter",
      "NUMEROLOTE" = "batch_number",
      "FONTE" = "source",
      "VERSAOSIST" = "system_version"
    ),
    pt = list(
      # Common demographics
      "SEXO" = "sexo",
      "CS_SEXO" = "sexo",
      "RACACOR" = "raca_cor",
      "CS_RACA" = "raca_cor",
      "ESTCIV" = "estado_civil",
      "ESTCIVMAE" = "estado_civil_mae",
      
      # Dates
      "DTOBITO" = "data_obito",
      "DTNASC" = "data_nascimento",
      "DT_NASC" = "data_nascimento",
      "NASC" = "data_nascimento",
      "DT_NOTIFIC" = "data_notificacao",
      "DT_DIAG" = "data_diagnostico",
      "DTNASC_MAE" = "data_nascimento_mae",
      "HORAOBITO" = "hora_obito",
      
      # Age
      "IDADE" = "codigo_idade",
      "NU_IDADE_N" = "idade_anos",
      "IDADEMAE" = "idade_mae",
      "IDADEanos" = "idade_anos",
      "IDADEmeses" = "idade_meses",
      "IDADEdias" = "idade_dias",
      "IDADEhoras" = "idade_horas",
      "IDADEminutos" = "idade_minutos",
      
      # Geographic
      "CODMUNRES" = "codigo_municipio_residencia",
      "ID_MN_RESI" = "codigo_municipio_residencia",
      "MUNIC_RES" = "codigo_municipio_residencia",
      "CODMUNNATU" = "codigo_municipio_nascimento",
      "CODMUNNASC" = "codigo_municipio_nascimento",
      "ID_MUNICIP" = "codigo_municipio_notificacao",
      "CODMUNOCOR" = "codigo_municipio_ocorrencia",
      "munResNome" = "nome_municipio_residencia",
      "munResUf" = "uf_residencia",
      "munResLat" = "latitude_residencia",
      "munResLon" = "longitude_residencia",
      
      # ICD/Diagnosis - SIM
      "CAUSABAS" = "causa_basica",
      "LINHAA" = "linha_causa_a",
      "LINHAB" = "linha_causa_b",
      "LINHAC" = "linha_causa_c",
      "LINHAD" = "linha_causa_d",
      "LINHAII" = "linha_causa_ii",
      "CAUSABAS_O" = "causa_basica_original",
      "CAUSAMAT" = "causa_materna",
      
      # ICD/Diagnosis - SIH
      "DIAG_PRINC" = "diagnostico_principal",
      "DIAG_SECUN" = "diagnostico_secundario",
      "CID_MORTE" = "causa_morte",
      "CID_NOTIF" = "causa_notificada",
      "CID_ASSO" = "causa_associada",
      "DIAGSEC1" = "diagnostico_secundario_1",
      "DIAGSEC2" = "diagnostico_secundario_2",
      "DIAGSEC3" = "diagnostico_secundario_3",
      "DIAGSEC4" = "diagnostico_secundario_4",
      "DIAGSEC5" = "diagnostico_secundario_5",
      "DIAGSEC6" = "diagnostico_secundario_6",
      "DIAGSEC7" = "diagnostico_secundario_7",
      "DIAGSEC8" = "diagnostico_secundario_8",
      "DIAGSEC9" = "diagnostico_secundario_9",
      
      # ICD/Diagnosis - SIA
      "PA_CIDPRI" = "cid_principal",
      "PA_CIDSEC" = "cid_secundario",
      "PA_CIDCAS" = "cid_associado",
      
      # Education
      "ESC" = "escolaridade",
      "ESC2010" = "escolaridade_2010",
      "ESCMAE" = "escolaridade_mae",
      "ESCMAE2010" = "escolaridade_mae_2010",
      "CS_ESCOL_N" = "escolaridade",
      "ESCMAEAGR1" = "escolaridade_mae_agrupada",
      "ESCFALAGR1" = "escolaridade_agrupada",
      
      # Occupation
      "OCUP" = "ocupacao",
      "OCUPMAE" = "ocupacao_mae",
      
      # Death type - SIM
      "TIPOBITO" = "tipo_obito",
      "TPMORTEOCO" = "tipo_morte_ocorrencia",
      "LOCOCOR" = "local_ocorrencia",
      "CIRCOBITO" = "circunstancia_obito",
      "ACIDTRAB" = "acidente_trabalho",
      "TPOBITOCOR" = "tipo_obito_corrigido",
      
      # Birth - SINASC
      "LOCNASC" = "local_nascimento",
      "PESO" = "peso",
      "APGAR1" = "apgar_1min",
      "APGAR5" = "apgar_5min",
      "GESTACAO" = "semanas_gestacao",
      "SEMAGESTAC" = "semanas_gestacao",
      "GRAVIDEZ" = "tipo_gravidez",
      "PARTO" = "tipo_parto",
      "QTDFILVIVO" = "filhos_vivos",
      "QTDFILMORT" = "filhos_mortos",
      "OBITOPARTO" = "obito_durante_parto",
      "OBITOGRAV" = "obito_durante_gravidez",
      "OBITOPUERP" = "obito_durante_puerperio",
      
      # SINAN specific
      "TP_NOT" = "tipo_notificacao",
      "ID_AGRAVO" = "codigo_agravo",
      "CS_GESTANT" = "gestante",
      "SEM_NOT" = "semana_notificacao",
      "SEM_DIAG" = "semana_diagnostico",
      "ANO_NASC" = "ano_nascimento",
      "NU_ANO" = "ano",
      "SG_UF_NOT" = "uf_notificacao",
      "SG_UF" = "uf",
      "ID_REGIONA" = "codigo_regional",
      "ID_RG_RESI" = "codigo_regional_residencia",
      "ID_UNIDADE" = "codigo_unidade_saude",
      
      # SIH specific
      "N_AIH" = "numero_aih",
      "PROC_REA" = "procedimento_realizado",
      "PROC_SOLIC" = "procedimento_solicitado",
      "QT_DIARIAS" = "dias_internacao",
      "VAL_TOT" = "valor_total",
      "VAL_SH" = "valor_servico_hospitalar",
      "VAL_SP" = "valor_servico_profissional",
      "VAL_SADT" = "valor_diagnostico_terapeutico",
      "UTI_MES_TO" = "uti_total_meses",
      "UTI_INT_TO" = "uti_total_dias",
      "MARCA_UTI" = "marcador_uti",
      "ANO_CMPT" = "ano_competencia",
      "MES_CMPT" = "mes_competencia",
      "ESPEC" = "especialidade",
      
      # SIA specific
      "PA_PROC_ID" = "codigo_procedimento",
      "PA_TPFIN" = "tipo_financiamento",
      "PA_SUBFIN" = "sub_financiamento",
      "PA_AUTORIZ" = "autorizacao",
      "PA_CNSMED" = "cns_medico",
      "PA_CBOCOD" = "codigo_cbo",
      "PA_MOTSAI" = "motivo_saida",
      "PA_OBITO" = "obito",
      "PA_ENCERR" = "encerramento",
      
      # CNES specific
      "CNES" = "codigo_cnes",
      "CODUFMUN" = "codigo_municipio",
      "TP_UNID" = "tipo_unidade",
      "NATUREZA" = "natureza_juridica",
      "ESFERA_A" = "esfera_administrativa",
      "TPGESTAO" = "tipo_gestao",
      "NIV_HIER" = "nivel_hierarquico",
      "TURNO_AT" = "horario_atendimento",
      "CLIENTEL" = "clientela",
      "REGSAUDE" = "regiao_saude",
      "MICR_REG" = "micro_regiao",
      
      # Establishment codes
      "CODESTAB" = "codigo_estabelecimento",
      "ESTABDESCR" = "descricao_estabelecimento",
      
      # Other common fields
      "ORIGEM" = "origem",
      "CONTADOR" = "contador",
      "NUMEROLOTE" = "numero_lote",
      "FONTE" = "fonte",
      "VERSAOSIST" = "versao_sistema"
    ),
    es = list(
      # Common demographics
      "SEXO" = "sexo",
      "CS_SEXO" = "sexo",
      "RACACOR" = "raza_color",
      "CS_RACA" = "raza_color",
      "ESTCIV" = "estado_civil",
      "ESTCIVMAE" = "estado_civil_madre",
      
      # Dates
      "DTOBITO" = "fecha_muerte",
      "DTNASC" = "fecha_nacimiento",
      "DT_NASC" = "fecha_nacimiento",
      "NASC" = "fecha_nacimiento",
      "DT_NOTIFIC" = "fecha_notificacion",
      "DT_DIAG" = "fecha_diagnostico",
      "DTNASC_MAE" = "fecha_nacimiento_madre",
      "HORAOBITO" = "hora_muerte",
      
      # Age
      "IDADE" = "codigo_edad",
      "NU_IDADE_N" = "edad_anos",
      "IDADEMAE" = "edad_madre",
      "IDADEanos" = "edad_anos",
      "IDADEmeses" = "edad_meses",
      "IDADEdias" = "edad_dias",
      "IDADEhoras" = "edad_horas",
      "IDADEminutos" = "edad_minutos",
      
      # Geographic
      "CODMUNRES" = "codigo_municipio_residencia",
      "ID_MN_RESI" = "codigo_municipio_residencia",
      "MUNIC_RES" = "codigo_municipio_residencia",
      "CODMUNNATU" = "codigo_municipio_nacimiento",
      "CODMUNNASC" = "codigo_municipio_nacimiento",
      "ID_MUNICIP" = "codigo_municipio_notificacion",
      "CODMUNOCOR" = "codigo_municipio_ocurrencia",
      "munResNome" = "nombre_municipio_residencia",
      "munResUf" = "uf_residencia",
      "munResLat" = "latitud_residencia",
      "munResLon" = "longitud_residencia",
      
      # ICD/Diagnosis - SIM
      "CAUSABAS" = "causa_basica",
      "LINHAA" = "linea_causa_a",
      "LINHAB" = "linea_causa_b",
      "LINHAC" = "linea_causa_c",
      "LINHAD" = "linea_causa_d",
      "LINHAII" = "linea_causa_ii",
      "CAUSABAS_O" = "causa_basica_original",
      "CAUSAMAT" = "causa_materna",
      
      # ICD/Diagnosis - SIH
      "DIAG_PRINC" = "diagnostico_principal",
      "DIAG_SECUN" = "diagnostico_secundario",
      "CID_MORTE" = "causa_muerte",
      "CID_NOTIF" = "causa_notificada",
      "CID_ASSO" = "causa_asociada",
      "DIAGSEC1" = "diagnostico_secundario_1",
      "DIAGSEC2" = "diagnostico_secundario_2",
      "DIAGSEC3" = "diagnostico_secundario_3",
      "DIAGSEC4" = "diagnostico_secundario_4",
      "DIAGSEC5" = "diagnostico_secundario_5",
      "DIAGSEC6" = "diagnostico_secundario_6",
      "DIAGSEC7" = "diagnostico_secundario_7",
      "DIAGSEC8" = "diagnostico_secundario_8",
      "DIAGSEC9" = "diagnostico_secundario_9",
      
      # ICD/Diagnosis - SIA
      "PA_CIDPRI" = "cie_principal",
      "PA_CIDSEC" = "cie_secundario",
      "PA_CIDCAS" = "cie_asociado",
      
      # Education
      "ESC" = "escolaridad",
      "ESC2010" = "escolaridad_2010",
      "ESCMAE" = "escolaridad_madre",
      "ESCMAE2010" = "escolaridad_madre_2010",
      "CS_ESCOL_N" = "escolaridad",
      "ESCMAEAGR1" = "escolaridad_madre_agrupada",
      "ESCFALAGR1" = "escolaridad_agrupada",
      
      # Occupation
      "OCUP" = "ocupacion",
      "OCUPMAE" = "ocupacion_madre",
      
      # Death type - SIM
      "TIPOBITO" = "tipo_muerte",
      "TPMORTEOCO" = "tipo_muerte_ocurrencia",
      "LOCOCOR" = "lugar_ocurrencia",
      "CIRCOBITO" = "circunstancia_muerte",
      "ACIDTRAB" = "accidente_trabajo",
      "TPOBITOCOR" = "tipo_muerte_corregido",
      
      # Birth - SINASC
      "LOCNASC" = "lugar_nacimiento",
      "PESO" = "peso",
      "APGAR1" = "apgar_1min",
      "APGAR5" = "apgar_5min",
      "GESTACAO" = "semanas_gestacion",
      "SEMAGESTAC" = "semanas_gestacion",
      "GRAVIDEZ" = "tipo_embarazo",
      "PARTO" = "tipo_parto",
      "QTDFILVIVO" = "hijos_vivos",
      "QTDFILMORT" = "hijos_muertos",
      "OBITOPARTO" = "muerte_durante_parto",
      "OBITOGRAV" = "muerte_durante_embarazo",
      "OBITOPUERP" = "muerte_durante_puerperio",
      
      # SINAN specific
      "TP_NOT" = "tipo_notificacion",
      "ID_AGRAVO" = "codigo_agravio",
      "CS_GESTANT" = "embarazada",
      "SEM_NOT" = "semana_notificacion",
      "SEM_DIAG" = "semana_diagnostico",
      "ANO_NASC" = "ano_nacimiento",
      "NU_ANO" = "ano",
      "SG_UF_NOT" = "uf_notificacion",
      "SG_UF" = "uf",
      "ID_REGIONA" = "codigo_regional",
      "ID_RG_RESI" = "codigo_regional_residencia",
      "ID_UNIDADE" = "codigo_unidad_salud",
      
      # SIH specific
      "N_AIH" = "numero_aih",
      "PROC_REA" = "procedimiento_realizado",
      "PROC_SOLIC" = "procedimiento_solicitado",
      "QT_DIARIAS" = "dias_internacion",
      "VAL_TOT" = "valor_total",
      "VAL_SH" = "valor_servicio_hospitalario",
      "VAL_SP" = "valor_servicio_profesional",
      "VAL_SADT" = "valor_diagnostico_terapeutico",
      "UTI_MES_TO" = "uci_total_meses",
      "UTI_INT_TO" = "uci_total_dias",
      "MARCA_UTI" = "marcador_uci",
      "ANO_CMPT" = "ano_competencia",
      "MES_CMPT" = "mes_competencia",
      "ESPEC" = "especialidad",
      
      # SIA specific
      "PA_PROC_ID" = "codigo_procedimiento",
      "PA_TPFIN" = "tipo_financiamiento",
      "PA_SUBFIN" = "sub_financiamiento",
      "PA_AUTORIZ" = "autorizacion",
      "PA_CNSMED" = "cns_medico",
      "PA_CBOCOD" = "codigo_cbo",
      "PA_MOTSAI" = "motivo_salida",
      "PA_OBITO" = "muerte",
      "PA_ENCERR" = "cierre",
      
      # CNES specific
      "CNES" = "codigo_cnes",
      "CODUFMUN" = "codigo_municipio",
      "TP_UNID" = "tipo_unidad",
      "NATUREZA" = "naturaleza_juridica",
      "ESFERA_A" = "esfera_administrativa",
      "TPGESTAO" = "tipo_gestion",
      "NIV_HIER" = "nivel_jerarquico",
      "TURNO_AT" = "horario_atencion",
      "CLIENTEL" = "clientela",
      "REGSAUDE" = "region_salud",
      "MICR_REG" = "micro_region",
      
      # Establishment codes
      "CODESTAB" = "codigo_establecimiento",
      "ESTABDESCR" = "descripcion_establecimiento",
      
      # Other common fields
      "ORIGEM" = "origen",
      "CONTADOR" = "contador",
      "NUMEROLOTE" = "numero_lote",
      "FONTE" = "fuente",
      "VERSAOSIST" = "version_sistema"
    )
  )
  
  # Categorical value translations (same as before, works across systems)
  value_translations <- list(
    sex = list(
      en = list("1" = "Male", "2" = "Female", "0" = "Ignored", "M" = "Male", "F" = "Female",
                "Masculino" = "Male", "Feminino" = "Female", "Ignorado" = "Ignored"),
      pt = list("1" = "Masculino", "2" = "Feminino", "0" = "Ignorado", "M" = "Masculino", "F" = "Feminino",
                "Male" = "Masculino", "Female" = "Feminino", "Ignored" = "Ignorado"),
      es = list("1" = "Masculino", "2" = "Femenino", "0" = "Ignorado", "M" = "Masculino", "F" = "Femenino",
                "Male" = "Masculino", "Female" = "Femenino", "Ignored" = "Ignorado")
    ),
    race = list(
      en = list("1" = "White", "2" = "Black", "3" = "Yellow", "4" = "Brown", "5" = "Indigenous", "0" = "Ignored",
                "Branca" = "White", "Preta" = "Black", "Amarela" = "Yellow", "Parda" = "Brown", 
                "Indigena" = "Indigenous", "Ignorado" = "Ignored"),
      pt = list("1" = "Branca", "2" = "Preta", "3" = "Amarela", "4" = "Parda", "5" = "Indigena", "0" = "Ignorado",
                "White" = "Branca", "Black" = "Preta", "Yellow" = "Amarela", "Brown" = "Parda",
                "Indigenous" = "Indigena", "Ignored" = "Ignorado"),
      es = list("1" = "Blanca", "2" = "Negra", "3" = "Amarilla", "4" = "Parda", "5" = "Indigena", "0" = "Ignorado",
                "White" = "Blanca", "Black" = "Negra", "Yellow" = "Amarilla", "Brown" = "Parda",
                "Indigenous" = "Indigena", "Ignored" = "Ignorado")
    ),
    death_type = list(
      en = list("1" = "Fetal", "2" = "Non-fetal",
                "Fetal" = "Fetal", "Nao Fetal" = "Non-fetal"),
      pt = list("1" = "Fetal", "2" = "Nao Fetal",
                "Fetal" = "Fetal", "Non-fetal" = "Nao Fetal"),
      es = list("1" = "Fetal", "2" = "No Fetal",
                "Fetal" = "Fetal", "Non-fetal" = "No Fetal")
    ),
    marital_status = list(
      en = list("1" = "Single", "2" = "Married", "3" = "Widowed", "4" = "Legally separated", 
                "5" = "Stable union", "9" = "Ignored",
                "Solteiro" = "Single", "Casado" = "Married", "Viuvo" = "Widowed"),
      pt = list("1" = "Solteiro", "2" = "Casado", "3" = "Viuvo", "4" = "Separado judicialmente",
                "5" = "Uniao estavel", "9" = "Ignorado",
                "Single" = "Solteiro", "Married" = "Casado", "Widowed" = "Viuvo"),
      es = list("1" = "Soltero", "2" = "Casado", "3" = "Viudo", "4" = "Separado judicialmente",
                "5" = "Union estable", "9" = "Ignorado",
                "Single" = "Soltero", "Married" = "Casado", "Widowed" = "Viudo")
    )
  )
  
  return(list(
    columns = column_translations[[lang]],
    values = value_translations,
    lang = lang
  ))
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

#' Map system codes to their respective processing functions
#'
#' @param system The system code (e.g., "SIM-DO", "SINAN-DENGUE")
#' @return The name of the processing function to use
#' @noRd
get_processing_function <- function(system) {
  system_mappings <- list(
    # SIM systems
    "SIM-DO" = "process_sim",
    "SIM-DOFET" = "process_sim",
    "SIM-DOEXT" = "process_sim",
    "SIM-DOINF" = "process_sim",
    "SIM-DOMAT" = "process_sim",
    
    # SINASC
    "SINASC" = "process_sinasc",
    
    # SIH systems
    "SIH-RD" = "process_sih",
    "SIH-RJ" = "process_sih",
    "SIH-SP" = "process_sih",
    "SIH-ER" = "process_sih",
    
    # CNES systems
    "CNES-LT" = "process_cnes",
    "CNES-ST" = "process_cnes",
    "CNES-DC" = "process_cnes",
    "CNES-EQ" = "process_cnes",
    "CNES-SR" = "process_cnes",
    "CNES-HB" = "process_cnes",
    "CNES-PF" = "process_cnes",
    "CNES-EP" = "process_cnes",
    "CNES-RC" = "process_cnes",
    "CNES-IN" = "process_cnes",
    "CNES-EE" = "process_cnes",
    "CNES-EF" = "process_cnes",
    "CNES-GM" = "process_cnes",
    
    # SIA systems
    "SIA-AB" = "process_sia",
    "SIA-ABO" = "process_sia",
    "SIA-ACF" = "process_sia",
    "SIA-AD" = "process_sia",
    "SIA-AN" = "process_sia",
    "SIA-AM" = "process_sia",
    "SIA-AQ" = "process_sia",
    "SIA-AR" = "process_sia",
    "SIA-ATD" = "process_sia",
    "SIA-PA" = "process_sia",
    "SIA-PS" = "process_sia",
    "SIA-SAD" = "process_sia",
    
    # SINAN systems
    "SINAN-DENGUE" = "process_sinan_dengue",
    "SINAN-CHIKUNGUNYA" = "process_sinan_chikungunya",
    "SINAN-ZIKA" = "process_sinan_zika",
    "SINAN-MALARIA" = "process_sinan_malaria",
    "SINAN-CHAGAS" = "process_sinan_chagas",
    "SINAN-LEISHMANIOSE-TEGUMENTAR" = "process_sinan_leishmaniose_tegumentar",
    "SINAN-LEISHMANIOSE-VISCERAL" = "process_sinan_leishmaniose_visceral",
    "SINAN-LEPTOSPIROSE" = "process_sinan_malaria"  # Usa process_sinan_malaria como fallback
  )
  
  return(system_mappings[[system]])
}

#' Check if a processing function exists and is available
#'
#' @param func_name Name of the processing function
#' @return Logical indicating if the function exists
#' @noRd
check_processing_function <- function(func_name) {
  exists(func_name, mode = "function")
}

#' Process data according to system type
#'
#' @param data Raw data from DATASUS
#' @param system System code
#' @param verbose Whether to print messages
#' @return Processed data
#' @noRd
process_data_by_system <- function(data, system, verbose = TRUE) {
  if (is.null(data) || nrow(data) == 0) {
    return(data)
  }
  
  # Get the appropriate processing function
  process_func_name <- get_processing_function(system)
  
  if (is.null(process_func_name)) {
    if (verbose) {
      cli::cli_alert_warning("No processing function defined for system: {system}")
      cli::cli_alert_info("Returning raw data without processing")
    }
    return(data)
  }
  
  # Check if the function exists
  if (!check_processing_function(process_func_name)) {
    if (verbose) {
      cli::cli_alert_warning("Processing function '{process_func_name}' not found")
      cli::cli_alert_info("Make sure the function is defined or the package is loaded")
      cli::cli_alert_info("Returning raw data without processing")
    }
    return(data)
  }
  
  # Apply the processing function
  tryCatch({
    if (verbose) {
      cli::cli_alert_info("Processing {system} data with {process_func_name}()...")
    }
    
    # Get the function from the global environment
    process_func <- get(process_func_name, mode = "function")
    
    # Apply processing
    processed_data <- process_func(data)
    
    if (verbose) {
      cli::cli_alert_success("Data processing completed for {system}")
      cli::cli_alert_info("Before processing: {nrow(data)} rows, {ncol(data)} columns")
      cli::cli_alert_info("After processing: {nrow(processed_data)} rows, {ncol(processed_data)} columns")
    }
    
    return(processed_data)
    
  }, error = function(e) {
    if (verbose) {
      cli::cli_alert_warning("Failed to process {system} data: {e$message}")
      cli::cli_alert_info("Returning raw data")
    }
    return(data)
  })
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