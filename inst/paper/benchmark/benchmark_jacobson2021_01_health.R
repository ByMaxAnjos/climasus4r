# ---------------------------------------------------------------------------
# Benchmark 2 -- step 1: health series for Jacobson et al. (2021),
# Int J Environ Res Public Health 18:5550, doi:10.3390/ijerph18115550.
#
# Outcome: daily deaths from respiratory diseases (ICD-10 J00-J99) among people
# aged 60+ resident in each of the 27 state capitals, 2000-2017 (SIM-DO).
# One RDS per capital in `out_dir`, so the script can be resumed.
# ---------------------------------------------------------------------------
library(climasus4r)

capitals <- c(
  AC = "1200401", AL = "2704302", AP = "1600303", AM = "1302603",
  BA = "2927408", CE = "2304400", DF = "5300108", ES = "3205309",
  GO = "5208707", MA = "2111300", MT = "5103403", MS = "5002704",
  MG = "3106200", PA = "1501402", PB = "2507507", PR = "4106902",
  PE = "2611606", PI = "2211001", RJ = "3304557", RN = "2408102",
  RS = "4314902", RO = "1100205", RR = "1400100", SC = "4205407",
  SP = "3550308", SE = "2800308", TO = "1721000"
)

out_dir <- "inst/paper/benchmark/data/health"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (uf in names(capitals)) {
  f <- file.path(out_dir, paste0(uf, ".rds"))
  if (file.exists(f)) next
  res <- withCallingHandlers(tryCatch({
    sus_data_import(uf = uf, year = 2000:2017, system = "SIM-DO") |>
      sus_data_clean_encoding(lang = "en") |>
      sus_data_standardize(lang = "en") |>
      sus_data_filter_cid(icd_codes = "J00-J99", lang = "en") |>
      # age_years must exist before filtering by age; otherwise the age filter
      # is skipped with only a warning
      sus_data_create_variables(
        create_age_groups    = TRUE,
        create_calendar_vars = TRUE,
        lang                 = "en"
      ) |>
      sus_data_filter_demographics(
        age_range         = c(60, Inf),
        municipality_code = capitals[[uf]],
        lang              = "en"
      ) |>
      sus_data_aggregate(
        time_unit = "day",
        group_by  = "residence_municipality_code",
        lang      = "en"
      ) |>
      dplyr::collect()
  }, error = function(e) e), warning = function(w) if (grepl("age_years", conditionMessage(w))) stop("age filter not applied: ", conditionMessage(w)))
  saveRDS(res, f)
  message(uf, ": ", if (inherits(res, "error")) conditionMessage(res) else nrow(res), " rows")
}
