# =============================================================================
# scripts/tutorial_ondas_frio.R
# Pre-rendering script: generates figures and tables for
# vignettes-pt/ondas_de_frio.Rmd
#
# Run once locally BEFORE knitting the Rmd:
#   Rscript scripts/tutorial_ondas_frio.R
#
# Output:
#   vignettes-pt/figuras/cw_*.png
#   vignettes-pt/dados/cw_*.rds
# =============================================================================

source("scripts/_setup_tutoriais.R")

library(climasus4r)
library(dplyr)

# ── Ensure output directories exist ──────────────────────────────────────────
if (!dir.exists("vignettes-pt/figuras")) dir.create("vignettes-pt/figuras", recursive = TRUE)
if (!dir.exists("vignettes-pt/dados"))   dir.create("vignettes-pt/dados",   recursive = TRUE)

# ── 1. Download INMET — Porto Alegre (A801), 2010–2023 ───────────────────────
message("Etapa 1: Baixando dados INMET — Porto Alegre (A801), 2020–2023 ...")
df_inmet <- sus_climate_inmet(
  years        = 2020:2023,
  uf = "RS",
  use_cache    = TRUE,
  parallel     = FALSE,
  lang         = "pt",
  verbose      = TRUE
)

# ── 2. Gap-filling ────────────────────────────────────────────────────────────
message("Etapa 2: Preenchendo lacunas ...")
df_filled <- sus_climate_fill_inmet(df_inmet,   target_var = "tair_dry_bulb_c", lang = "pt")
df_filled <- sus_climate_fill_inmet(df_filled,  target_var = "tair_max_c",      lang = "pt")
df_filled <- sus_climate_fill_inmet(df_filled,  target_var = "tair_min_c",      lang = "pt")
df_filled <- sus_climate_fill_inmet(df_filled,  target_var = "rh_pct",          lang = "pt")

# ── 3. Compute thermal indicators ─────────────────────────────────────────────
message("Etapa 3: Calculando indicadores climaticos ...")
df_indicators <- sus_climate_compute_indicators(
  df                  = df_filled,
  indicators          = "all",
  region              = "auto",
  apply_validity_mask = TRUE,
  verify_physics      = TRUE,
  keep_source_vars    = TRUE,
  verbose             = TRUE,
  lang                = "pt"
)

# ── 4. Detect cold waves (all methods) ────────────────────────────────────────
message("Etapa 4: Detectando ondas de frio (todos os metodos) ...")
cw_all <- sus_climate_compute_coldwaves(
  df             = df_indicators,
  method         = "all",
  baseline_start = "2010-01-01",
  baseline_end   = "2020-12-31",
  percentile     = 10,
  lang           = "pt",
  verbose        = TRUE
)

# ── 5. Save intermediate objects ─────────────────────────────────────────────
message("Salvando objetos intermediarios ...")
saveRDS(cw_all$events,  "vignettes-pt/dados/cw_events_A801.rds")
saveRDS(cw_all$summary, "vignettes-pt/dados/cw_summary_A801.rds")
saveRDS(cw_all$daily,   "vignettes-pt/dados/cw_daily_A801.rds")

# ── 6. Generate and save figures ─────────────────────────────────────────────
message("Gerando figuras ...")

# 6.1 Timeline — all methods
p_timeline <- sus_climate_plot_coldwaves(
  cw_result   = cw_all,
  type        = "timeline",
  interactive = FALSE,
  lang        = "pt"
)
save_fig(p_timeline, "cw_timeline_A801")

# 6.2 Timeline — INMET only
p_timeline_inmet <- sus_climate_plot_coldwaves(
  cw_result   = cw_all,
  type        = "timeline",
  method      = "INMET",
  interactive = FALSE,
  lang        = "pt"
)

save_fig(p_timeline_inmet, "cw_timeline_inmet_A801")

# 6.3 Calendar — WHO method
p_calendar_who <- sus_climate_plot_coldwaves(
  cw_result   = cw_all,
  type        = "calendar",
  method      = "WHO",
  interactive = FALSE,
  lang        = "pt"
)
save_fig(p_calendar_who, "cw_calendar_who_A801")

# 6.4 Calendar — cw_any (any method)
p_calendar_any <- sus_climate_plot_coldwaves(
  cw_result   = cw_all,
  type        = "calendar",
  method      = NULL,
  interactive = FALSE,
  lang        = "pt"
)
save_fig(p_calendar_any, "cw_calendar_any_A801")

# 6.5 Intensity — ECF method
p_intensity <- sus_climate_plot_coldwaves(
  cw_result   = cw_all,
  type        = "intensity",
  method      = "EHF",
  interactive = FALSE,
  lang        = "pt"
)
save_fig(p_intensity, "cw_intensity_ecf_A801")

# 6.6 Intensity — all methods
p_intensity_all <- sus_climate_plot_coldwaves(
  cw_result   = cw_all,
  type        = "intensity",
  method      = NULL,
  interactive = FALSE,
  lang        = "pt"
)
save_fig(p_intensity_all, "cw_intensity_all_A801")

# 6.7 Trend — all methods
p_trend <- sus_climate_plot_coldwaves(
  cw_result   = cw_all,
  type        = "trend",
  interactive = FALSE,
  lang        = "pt"
)
save_fig(p_trend, "cw_trend_A801")

# 6.8 Trend — WHO and INMET comparison
p_trend_who_inmet <- sus_climate_plot_coldwaves(
  cw_result   = cw_all,
  type        = "trend",
  method      = c("WHO", "INMET"),
  interactive = FALSE,
  lang        = "pt"
)
save_fig(p_trend_who_inmet, "cw_trend_who_inmet_A801")

message("Pre-renderizacao concluida com sucesso.")
message("Figuras salvas em: vignettes-pt/figuras/")
message("Dados salvos em:   vignettes-pt/dados/")
message("")
message("Para renderizar o tutorial:")
message("  rmarkdown::render('vignettes-pt/ondas_de_frio.Rmd')")
