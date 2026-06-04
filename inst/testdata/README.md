# DATASUS Test Database — climasus4r

Sample Parquet files covering all major DATASUS systems supported by climasus4r.

## Parameters

| Parameter | Value |
|-----------|-------|
| Primary UF | Rondônia (RO) |
| Exceptions | SIH-RJ uses RJ; SIH-SP uses SP |
| Year | 2022 |
| Month | January 2022 (for monthly systems: SIH, SIA, CNES) |
| Format | Apache Parquet |

## Files (35 total)

### SIM (5 files)
- `sim/SIM_DO_RO_2022.parquet` — 10,277 rows | Death certificates
- `sim/SIM_DOFET_RO_2022.parquet` — 27,394 rows | Fetal deaths (national)
- `sim/SIM_DOEXT_RO_2022.parquet` — 152,945 rows | External causes (national)
- `sim/SIM_DOINF_RO_2022.parquet` — 32,257 rows | Infant deaths (national)
- `sim/SIM_DOMAT_RO_2022.parquet` — 1,370 rows | Maternal deaths

### SIH (4 files)
- `sih/SIH_RD_RO_2022_01.parquet` — 9,256 rows | Hospital admissions (RO, Jan/22)
- `sih/SIH_RJ_RJ_2022_01.parquet` — 3,457 rows | RJ-specific AIH
- `sih/SIH_SP_SP_2022_01.parquet` — 3,407,996 rows | SP-specific AIH
- `sih/SIH_ER_RO_2022_01.parquet` — 664 rows | Emergency

### SINAN (7 files)
- `sinan/SINAN_DENGUE_RO_2022.parquet` — 14,252 rows
- `sinan/SINAN_CHIKUNGUNYA_RO_2022.parquet` — 517 rows
- `sinan/SINAN_ZIKA_RO_2022.parquet` — 303 rows
- `sinan/SINAN_CHAGAS_RO_2022.parquet` — 35 rows
- `sinan/SINAN_LV_RO_2022.parquet` — 6 rows | Visceral leishmaniasis
- `sinan/SINAN_LT_RO_2022.parquet` — 769 rows | Cutaneous leishmaniasis
- `sinan/SINAN_LEPTOSPIROSE_RO_2022.parquet` — 368 rows
- NOTE: SINAN-MALARIA returned no records for RO 2022

### SIA (6 files)
- `sia/SIA_AD_RO_2022_01.parquet` — 970 rows | Alta complexidade
- `sia/SIA_AM_RO_2022_01.parquet` — 7,895 rows | Ambulatorio especialidades
- `sia/SIA_AQ_RO_2022_01.parquet` — 2,650 rows | Acoes estrategicas
- `sia/SIA_AR_RO_2022_01.parquet` — 129 rows | Regulacao
- `sia/SIA_PA_RO_2022_01.parquet` — 236,364 rows | Procedimentos ambulatoriais
- `sia/SIA_PS_RO_2022_01.parquet` — 2,379 rows | Saude mental
- NOTE: SIA-AB/ABO/ACF/AN/ATD/SAD not available for RO/Jan-2022

### CNES (12 files)
- `cnes/CNES_LT/ST/DC/EQ/SR/HB/PF/EP/RC/IN/EF/GM_RO_2022_01.parquet`
- 690 to 41,917 rows each
- NOTE: CNES-EE — FTP 550 unavailable

### SINASC (1 file)
- `sinasc/SINASC_RO_2022.parquet` — 24,901 rows | Live births

## Quick Start

```r
library(climasus4r)
df <- .read_parquet_smart("inst/testdata/sim/SIM_DO_RO_2022.parquet") |>
  new_climasus_df(list(stage = "import", system = "SIM"))
df |> sus_data_clean_encoding() |> sus_data_standardize() |>
  sus_data_aggregate(time_unit = "month")
```

## Tests
```r
source("inst/testdata/test_pipeline.R")
```

## Regenerating
```r
source("inst/testdata/create_testdata.R")
```
