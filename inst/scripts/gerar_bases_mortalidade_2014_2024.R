## Base de mortalidade Brasil 2014-2024 (SIM/DO) + recortes por grupo de causa
## Gera 1 parquet com a base completa e 1 parquet por grupo de causa (CAUSABAS preservada em todos).

library(climasus4r)
library(arrow)
library(dplyr)

out_dir <- "~/datasus_br/sim/mortalidade_2014_2024"
fs::dir_create(out_dir)

## 1) Base completa, padronizada, todas as causas ------------------------------
base <- arrow::open_dataset("~/datasus_br/sim/DO/") |>
  dplyr::filter(year >= 2014, year <= 2024) |>
  dplyr::collect() |>
  sus_data_standardize()

write_parquet_climasus(base, file.path(out_dir, "mortalidade_todas_causas_2014_2024.parquet"))

## 2) Grupos de causa (a partir da base completa já padronizada) --------------
grupos <- list(
  causas_naturais       = "A00-R99",
  cardiovasculares      = "I00-I99",
  respiratorias         = "J00-J99",
  acidentes_lesoes      = c("V01-X59", "Y85-Y86"),
  transtornos_mentais   = "F00-F99",
  sistema_nervoso       = "G00-G99"
)

for (nome in names(grupos)) {
  df_grupo <- sus_data_filter_cid(base, icd_codes = grupos[[nome]])
  write_parquet_climasus(
    df_grupo,
    file.path(out_dir, paste0("mortalidade_", nome, "_2014_2024.parquet"))
  )
}
