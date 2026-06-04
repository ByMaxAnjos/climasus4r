# DATASUS Test Database — climasus4r

Sample Parquet files covering all major DATASUS systems supported by climasus4r.
Includes **36 Parquet files** across 6 systems.

## Parameters

| Parameter | Value |
|-----------|-------|
| Primary UF | Rondônia (RO) |
| Exceptions | SIH-RJ uses RJ; SIH-SP uses SP; SINAN-MALARIA is national |
| Year | 2022 |
| Month | January 2022 (for monthly systems: SIH, SIA, CNES) |
| Format | Apache Parquet |

## Quick Start

```r
library(climasus4r)
# Load any sample file
df <- .read_parquet_smart("inst/testdata/sim/SIM_DO_RO_2022.parquet") |>
  new_climasus_df(list(stage = "import", system = "SIM"))
df |> sus_data_clean_encoding() |> sus_data_standardize() |>
  sus_meta(backend = "tibble") |>
  sus_data_aggregate(time_unit = "month", backend = "tibble")
# Run all pipeline tests
source("inst/testdata/test_pipeline.R")
```

## SIM (5 files — Rondônia, 2022)
| File | Rows | Notes |
|------|------|-------|
| `sim/SIM_DO_RO_2022.parquet` | 10,277 | Death certificates |
| `sim/SIM_DOFET_RO_2022.parquet` | 27,394 | Fetal deaths (national file) |
| `sim/SIM_DOEXT_RO_2022.parquet` | 152,945 | External causes (national file) |
| `sim/SIM_DOINF_RO_2022.parquet` | 32,257 | Infant deaths (national file) |
| `sim/SIM_DOMAT_RO_2022.parquet` | 1,370 | Maternal deaths |

## SIH (4 files — Jan/2022)
| File | Rows | Notes |
|------|------|-------|
| `sih/SIH_RD_RO_2022_01.parquet` | 9,256 | Hospital admissions RO |
| `sih/SIH_RJ_RJ_2022_01.parquet` | 3,457 | AIH Rio de Janeiro |
| `sih/SIH_SP_SP_2022_01.parquet` | 3,407,996 | AIH São Paulo |
| `sih/SIH_ER_RO_2022_01.parquet` | 664 | Emergency RO |

## SINAN (8 files — 2022)
| File | Rows | Notes |
|------|------|-------|
| `sinan/SINAN_DENGUE_RO_2022.parquet` | 14,252 | Rondônia |
| `sinan/SINAN_CHIKUNGUNYA_RO_2022.parquet` | 517 | Rondônia |
| `sinan/SINAN_ZIKA_RO_2022.parquet` | 303 | Rondônia |
| `sinan/SINAN_CHAGAS_RO_2022.parquet` | 35 | Rondônia |
| `sinan/SINAN_LV_RO_2022.parquet` | 6 | Visceral leishmaniasis |
| `sinan/SINAN_LT_RO_2022.parquet` | 769 | Cutaneous leishmaniasis |
| `sinan/SINAN_LEPTOSPIROSE_RO_2022.parquet` | 368 | Rondônia |
| `sinan/SINAN_MALARIA_BR_2022.parquet` | 3,411 | **National file** (microdatasus limitation: UF filter not supported for malaria) |

## SIA (6 files — Rondônia, Jan/2022)
| File | Rows | Notes |
|------|------|-------|
| `sia/SIA_PA_RO_2022_01.parquet` | 236,364 | Procedimentos ambulatoriais |
| `sia/SIA_AM_RO_2022_01.parquet` | 7,895 | Ambulatorio especialidades |
| `sia/SIA_AQ_RO_2022_01.parquet` | 2,650 | Acoes estrategicas |
| `sia/SIA_PS_RO_2022_01.parquet` | 2,379 | Saude mental |
| `sia/SIA_AD_RO_2022_01.parquet` | 970 | Alta complexidade |
| `sia/SIA_AR_RO_2022_01.parquet` | 129 | Regulacao |

> **Not available:** SIA-AB, ABO, ACF, AN, ATD, SAD — FTP 550 error on DataSUS (files do not exist for any state/year tested). These subsystems may be discontinued or not yet published via microdatasus.

## CNES (12 files — Rondônia, Jan/2022)
| File | Rows |
|------|------|
| `cnes/CNES_PF_RO_2022_01.parquet` | 41,917 |
| `cnes/CNES_EQ_RO_2022_01.parquet` | 10,413 |
| `cnes/CNES_ST_RO_2022_01.parquet` | 3,903 |
| `cnes/CNES_SR_RO_2022_01.parquet` | 7,773 |
| `cnes/CNES_EP_RO_2022_01.parquet` | 693 |
| `cnes/CNES_LT_RO_2022_01.parquet` | 690 |
| `cnes/CNES_DC_RO_2022_01.parquet` | 26 |
| `cnes/CNES_HB_RO_2022_01.parquet` | 185 |
| `cnes/CNES_IN_RO_2022_01.parquet` | 79 |
| `cnes/CNES_RC_RO_2022_01.parquet` | 50 |
| `cnes/CNES_EF_RO_2022_01.parquet` | 1 |
| `cnes/CNES_GM_RO_2022_01.parquet` | 4 |

> **Not available:** CNES-EE — FTP 550 error (file not found on DataSUS for any state tested).

## SINASC (1 file — Rondônia, 2022)
| File | Rows |
|------|------|
| `sinasc/SINASC_RO_2022.parquet` | 24,901 |

## Notes on Missing Systems

| System | Status | Reason |
|--------|--------|--------|
| SINAN-MALARIA (UF-filtered) | Not available per UF | microdatasus downloads national file; UF codes are numeric IBGE codes (not 2-letter). Bug in sus_data_import filter. Saved as national: `SINAN_MALARIA_BR_2022.parquet` |
| SIA-AB, ABO, ACF, AN, ATD, SAD | Not available | FTP 550 on all states/years tested. Files may be discontinued or not in microdatasus 2.5.0 |
| CNES-EE | Not available | FTP 550 on all states tested |

## Regenerating
```r
source("inst/testdata/create_testdata.R")
```
