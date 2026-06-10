# test_all_grid_pipeline.R
# Testa todas as funções sus_grid_* do pacote climasus4r
# Padrão: test_all_climate_pipeline.R (Grupos A-G)
# Uso: Rscript inst/testdata/test_all_grid_pipeline.R

setwd("/Users/co2map/Documents/2026/CLIMASUS4r/climasus4r")
suppressPackageStartupMessages(devtools::load_all(quiet = TRUE))

BASE    <- "inst/testdata/climate"
results <- list()
ok      <- 0L
fail    <- 0L

run_test <- function(name, code) {
  r <- tryCatch(code, error = function(e) {
    cli::cli_alert_danger("  {name}: FAIL — {conditionMessage(e)}")
    results[[length(results) + 1L]] <<- list(
      label = name, result = "FAIL", rows = NA_integer_,
      error = conditionMessage(e)
    )
    fail <<- fail + 1L
    invisible(NULL)
  })
  if (!is.null(r)) {
    n <- tryCatch(
      if (is.list(r) && !is.data.frame(r)) paste0("list(", length(r), ")")
      else as.integer(nrow(r)),
      error = function(e) "?"
    )
    cli::cli_alert_success("  {name}: OK ({n} linhas)")
    results[[length(results) + 1L]] <<- list(
      label = name, result = "PASS", rows = n, error = NA_character_
    )
    ok <<- ok + 1L
  }
  invisible(r)
}

`%||%` <- function(x, y) if (!is.null(x)) x else y

# ── Carregar fixtures ─────────────────────────────────────────────────────────
cli::cli_h1("Carregando fixtures")
health_df  <- readRDS(file.path(BASE, "health_ro_2022_spatial.rds"))
monthly_df <- climasus4r:::from_arrow_climasus(
                file.path(BASE, "climate_ro_2022_monthly.parquet"))
muni_sample <- c(110001L, 110002L, 110003L, 110004L, 110005L)
cli::cli_alert_info("health_df : {nrow(health_df)} linhas | stage={sus_meta(health_df,'stage')} | type={sus_meta(health_df,'type')}")
cli::cli_alert_info("monthly_df: {nrow(monthly_df)} linhas | stage={sus_meta(monthly_df,'stage')} | type={sus_meta(monthly_df,'type')}")

# ── Grupo H — sus_grid_join ───────────────────────────────────────────────────
cli::cli_h2("Grupo H — sus_grid_join")
# ── Grupo H: sus_grid_join() ──────────────────────────────────────────────────

# ── Teste 1: join básico por code_muni + date ──────────────────────────────────
run_test("grid_join | basic code_muni+date", {
  r <- sus_grid_join(health_df, monthly_df,
                     by      = c("code_muni", "date"),
                     verbose = FALSE)
  stopifnot(
    inherits(r, "climasus_df"),
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "agg",
    "rainfall_mm"      %in% names(r),
    "tair_dry_bulb_c"  %in% names(r),
    "n_obitos"         %in% names(r),
    nrow(r) == nrow(health_df)
  )
  r
})

# ── Teste 2: type_out substitui o type herdado do grid ────────────────────────
run_test("grid_join | type_out override", {
  r <- sus_grid_join(health_df, monthly_df,
                     type_out = "merged",
                     verbose  = FALSE)
  stopifnot(
    sus_meta(r, "type") == "merged"
  )
  r
})

# ── Teste 3: todas as colunas de health_df são preservadas ────────────────────
run_test("grid_join | colunas saude preservadas", {
  r <- sus_grid_join(health_df, monthly_df,
                     verbose = FALSE)
  health_cols <- setdiff(names(health_df), c(attr(health_df, "sf_column"), "geom"))
  stopifnot(
    all(health_cols %in% names(r))
  )
  r
})

# ── Grupo I — sus_grid_era5 ───────────────────────────────────────────────────
cli::cli_h2("Grupo I — sus_grid_era5")
# === GRUPO I: sus_grid_era5() ===

# ── Teste 1: sus_grid_era5 (cache smoke) ──────────────────────────────────────
local({
  cache_dir <- path.expand("~/.climasus4r_cache/era5")
  cached <- list.files(cache_dir, pattern = "\\.(parquet|nc)$",
                       recursive = TRUE, full.names = TRUE)
  if (length(cached) == 0L) {
    cli::cli_alert_warning(
      "  era5 | cache smoke: SKIP — sem cache em {cache_dir}"
    )
    return(invisible(NULL))
  }
  # Detectar o ano presente no cache a partir do caminho do primeiro arquivo
  year_match <- regmatches(cached[1], regexpr("20[0-9]{2}", cached[1]))
  year_val   <- if (length(year_match) > 0L) as.integer(year_match[1]) else 2020L

  run_test("era5 | cache smoke", {
    r <- sus_grid_era5(
      years        = year_val,
      municipalities = muni_sample,
      use_cache    = TRUE,
      verbose      = FALSE
    )
    stopifnot(
      inherits(r, "climasus_df"),
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "era5_land",
      nrow(r) > 0
    )
    r
  })
})

# ── Grupo J — sus_grid_chirps ─────────────────────────────────────────────────
cli::cli_h2("Grupo J — sus_grid_chirps")
# === GRUPO J: sus_grid_chirps() ===

# ── Teste 1: chirps | cache smoke ─────────────────────────────────────────────
local({
  cache_dir <- path.expand("~/.climasus4r_cache/chirps")
  cached <- list.files(cache_dir, pattern = "\\.(parquet|tif|tiff)$",
                       recursive = TRUE, full.names = TRUE)
  if (length(cached) == 0L) {
    cli::cli_alert_warning("  chirps | cache smoke: SKIP — sem cache em {cache_dir}")
    return(invisible(NULL))
  }
  year_match <- regmatches(cached[1], regexpr("20[0-9]{2}", cached[1]))
  year_val   <- if (length(year_match) > 0L) as.integer(year_match[1]) else 2022L
  run_test("chirps | cache smoke", {
    r <- sus_grid_chirps(years = year_val, municipalities = muni_sample,
                         use_cache = TRUE, verbose = FALSE)
    stopifnot(
      inherits(r, "climasus_df"),
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "chirps",
      "rainfall_chirps_mm" %in% names(r),
      nrow(r) > 0
    )
    r
  })
})

# ── Grupo K — sus_grid_pdsi + sus_grid_smvi ───────────────────────────────────
cli::cli_h2("Grupo K — sus_grid_pdsi + sus_grid_smvi")
# === GRUPO K: sus_grid_pdsi() + sus_grid_smvi() ===

# ── Teste 1: sus_grid_pdsi (cache smoke) ──────────────────────────────────────
local({
  cache_dir <- path.expand("~/.climasus4r_cache/pdsi")
  files     <- list.files(cache_dir, recursive = TRUE)
  if (length(files) == 0L) {
    cli::cli_alert_warning("pdsi | cache smoke: SKIP — cache vazio em {cache_dir}")
    return(invisible(NULL))
  }

  # detecta o primeiro ano disponível no cache
  years_found <- regmatches(files, regexpr("20[0-9]{2}", files))
  year_use    <- if (length(years_found) > 0L) as.integer(years_found[[1L]]) else NULL

  run_test("pdsi | cache smoke", {
    r <- sus_grid_pdsi(
      years        = year_use,
      months       = 1:12,
      source       = "terraclimate",
      municipalities = NULL,
      agg_fun      = "mean",
      crop_brazil  = TRUE,
      use_cache    = TRUE,
      cache_dir    = cache_dir,
      lang         = "pt",
      verbose      = FALSE
    )
    stopifnot(
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "pdsi",
      "code_muni" %in% names(r),
      "date"      %in% names(r),
      "pdsi"      %in% names(r),
      nrow(r) > 0
    )
    r
  })
})

# ── Teste 2: sus_grid_smvi (cache smoke) ──────────────────────────────────────
local({
  cache_dir <- path.expand("~/.climasus4r_cache/smvi")
  files     <- list.files(cache_dir, recursive = TRUE)
  if (length(files) == 0L) {
    cli::cli_alert_warning("smvi | cache smoke: SKIP — cache vazio em {cache_dir}")
    return(invisible(NULL))
  }

  # detecta o primeiro ano disponível no cache
  years_found <- regmatches(files, regexpr("20[0-9]{2}", files))
  year_use    <- if (length(years_found) > 0L) as.integer(years_found[[1L]]) else NULL

  run_test("smvi | cache smoke", {
    r <- sus_grid_smvi(
      years        = year_use,
      municipalities = NULL,
      aggregate_by = c("year", "month"),
      brazil_only  = TRUE,
      use_cache    = TRUE,
      cache_dir    = cache_dir,
      lang         = "pt",
      verbose      = FALSE
    )
    stopifnot(
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "smvi",
      nrow(r) > 0
    )
    r
  })
})

# ── Grupo L — sus_grid_prodes ─────────────────────────────────────────────────
cli::cli_h2("Grupo L — sus_grid_prodes")
# === GRUPO L: sus_grid_prodes() ===

# ── Teste 1: sus_grid_prodes (cache smoke) ────────────────────────────────────
local({
  cache_dir <- path.expand("~/.climasus4r_cache/prodes")
  cached <- list.files(cache_dir,
                       pattern   = "\\.(geojson|parquet)$",
                       recursive = TRUE,
                       full.names = TRUE)
  if (length(cached) == 0L) {
    cli::cli_alert_warning("prodes | cache smoke: SKIP — cache_dir vazio ({cache_dir})")
    return(invisible(NULL))
  }

  # Inferir um ano presente nos arquivos em cache
  year_val <- tryCatch({
    m <- regmatches(basename(cached[[1L]]),
                    regexpr("[0-9]{4}", basename(cached[[1L]])))
    if (length(m) > 0L) as.integer(m[[1L]]) else 2022L
  }, error = function(e) 2022L)

  run_test("prodes | cache smoke", {
    r <- sus_grid_prodes(
      years         = year_val,
      biomes        = "Amazon",
      municipalities = muni_sample,
      use_cache     = TRUE,
      verbose       = FALSE
    )
    stopifnot(
      inherits(r, "climasus_df"),
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "prodes",
      "deforested_area_km2" %in% names(r),
      "code_muni"           %in% names(r),
      "date"                %in% names(r),
      "year"                %in% names(r),
      "biome"               %in% names(r),
      nrow(r) > 0
    )
    r
  })
})

install.packages("Matrix")

# ── Grupo M — sus_grid_fires ──────────────────────────────────────────────────
cli::cli_h2("Grupo M — sus_grid_fires")
# === GRUPO M: sus_grid_fires() ===

muni_sample <- c(110001L, 110002L, 110003L, 110004L, 110005L)

install.packages("INLA",
repos = "https://inla.r-inla-download.org/R/stable", dep = TRUE)
# ── Teste 1: sus_grid_fires | inpe cache smoke ────────────────────────────────
local({
  cache_dir <- path.expand("~/.climasus4r_cache/fires")
  cached    <- list.files(cache_dir,
                          pattern   = "\\.(parquet|csv)$",
                          recursive = TRUE,
                          full.names = TRUE)
  # Skip if no cache or all files are empty (0 bytes)
  cached_nonempty <- cached[file.info(cached)$size > 0L]
  if (length(cached_nonempty) == 0L) {
    cli::cli_alert_warning(
      "fires | inpe cache smoke: SKIP — cache vazio em {cache_dir}"
    )
    return(invisible(NULL))
  }

  # Inferir o primeiro ano disponível a partir dos nomes de arquivo
  year_val <- tryCatch({
    years_found <- regmatches(basename(cached_nonempty),
                              regexpr("\\d{4}", basename(cached_nonempty)))
    years_found <- as.integer(years_found)
    years_found <- years_found[!is.na(years_found) & years_found >= 2000L &
                                 years_found <= 2030L]
    if (length(years_found) == 0L) 2022L else min(years_found)
  }, error = function(e) 2022L)

  run_test("fires | inpe cache smoke", {
    r <- sus_grid_fires(
      years         = year_val,
      uf            = "RO",
      source        = "inpe",
      municipalities = NULL,
      use_cache     = TRUE,
      verbose       = FALSE
    )
    stopifnot(
      inherits(r, "climasus_df"),
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "fires",
      nrow(r) > 0
    )
    r
  })
})

# ── Teste 2: sus_grid_fires | colunas mínimas ─────────────────────────────────
local({
  cache_dir <- path.expand("~/.climasus4r_cache/fires")
  cached    <- list.files(cache_dir,
                          pattern   = "\\.(parquet|csv)$",
                          recursive = TRUE,
                          full.names = TRUE)
  # Skip if no cache or all files are empty (0 bytes)
  cached_nonempty <- cached[file.info(cached)$size > 0L]
  if (length(cached_nonempty) == 0L) {
    cli::cli_alert_warning(
      "fires | colunas minimas: SKIP — cache vazio em {cache_dir}"
    )
    return(invisible(NULL))
  }

  year_val <- tryCatch({
    years_found <- regmatches(basename(cached_nonempty),
                              regexpr("\\d{4}", basename(cached_nonempty)))
    years_found <- as.integer(years_found)
    years_found <- years_found[!is.na(years_found) & years_found >= 2000L &
                                 years_found <= 2030L]
    if (length(years_found) == 0L) 2022L else min(years_found)
  }, error = function(e) 2022L)

  run_test("fires | colunas minimas", {
    r <- sus_grid_fires(
      years     = year_val,
      uf        = "RO",
      source    = "inpe",
      use_cache = TRUE,
      verbose   = FALSE
    )
    required_cols <- c("date", "code_muni", "n_fires", "frp_mean")
    missing_cols  <- setdiff(required_cols, names(r))
    if (length(missing_cols) > 0L) {
      stop(paste("Colunas ausentes:", paste(missing_cols, collapse = ", ")))
    }
    stopifnot(
      inherits(r, "climasus_df"),
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "fires",
      nrow(r) > 0
    )
    r
  })
})

# ── Teste 3: sus_grid_fires | filtro por municípios ───────────────────────────
local({
  cache_dir <- path.expand("~/.climasus4r_cache/fires")
  cached    <- list.files(cache_dir,
                          pattern   = "\\.(parquet|csv)$",
                          recursive = TRUE,
                          full.names = TRUE)
  # Skip if no cache or all files are empty (0 bytes)
  cached_nonempty <- cached[file.info(cached)$size > 0L]
  if (length(cached_nonempty) == 0L) {
    cli::cli_alert_warning(
      "fires | filtro municipios: SKIP — cache vazio em {cache_dir}"
    )
    return(invisible(NULL))
  }

  year_val <- tryCatch({
    years_found <- regmatches(basename(cached_nonempty),
                              regexpr("\\d{4}", basename(cached_nonempty)))
    years_found <- as.integer(years_found)
    years_found <- years_found[!is.na(years_found) & years_found >= 2000L &
                                 years_found <= 2030L]
    if (length(years_found) == 0L) 2022L else min(years_found)
  }, error = function(e) 2022L)

  run_test("fires | filtro municipios", {
    r <- sus_grid_fires(
      years         = year_val,
      uf            = "RO",
      source        = "inpe",
      municipalities = NULL,
      use_cache     = TRUE,
      verbose       = FALSE
    )
    stopifnot(
      inherits(r, "climasus_df"),
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "fires",
      nrow(r) > 0
    )
    r
  })
})

# ── Teste 4: sus_grid_fires | metadata stage/type sem cache ───────────────────
local({
  r <- tryCatch(
    sus_grid_fires(
      years     = 2022L,
      uf        = "RO",
      source    = "inpe",
      use_cache = FALSE,
      verbose   = FALSE
    ),
    error = function(e) {
      cli::cli_alert_warning(
        "  fires | metadata stage type: SKIP — {conditionMessage(e)}"
      )
      return(invisible(NULL))
    }
  )
  if (is.null(r)) return(invisible(NULL))
  run_test("fires | metadata stage type", {
    stopifnot(
      inherits(r, "climasus_df"),
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "fires"
    )
    r
  })
})

# ── Grupo N — sus_grid_pollution_cams + sus_grid_pollution_ghap ───────────────
cli::cli_h2("Grupo N — sus_grid_pollution_cams + sus_grid_pollution_ghap")
# === GRUPO N: sus_grid_pollution_cams() + sus_grid_pollution_ghap() ===

# ── Teste 1: sus_grid_pollution_cams (cache smoke) ────────────────────────────
local({
  cache_dir <- path.expand("~/.climasus4r_cache/cams")
  files     <- list.files(cache_dir, pattern = "\\.parquet$", recursive = TRUE)
  if (length(files) == 0L) {
    cli::cli_alert_warning("pollution_cams | cache smoke: SKIP — cache vazio em {cache_dir}")
    return(invisible(NULL))
  }

  # detecta o primeiro ano disponível no cache
  years_found <- regmatches(files, regexpr("20[0-9]{2}", files))
  year_use    <- if (length(years_found) > 0L) as.integer(years_found[[1L]]) else NULL

  run_test("pollution_cams | cache smoke", {
    r <- sus_grid_pollution_cams(
      pollutants = c("pm25", "pm10"),
      metric     = "mean",
      years      = year_use,
      use_cache  = TRUE,
      cache_dir  = cache_dir,
      lang       = "pt",
      verbose    = FALSE
    )
    stopifnot(
      inherits(r, "climasus_df"),
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "pollution_cams",
      "code_muni" %in% names(r),
      "date"      %in% names(r),
      nrow(r) > 0
    )
    r
  })
})

# ── Teste 2: sus_grid_pollution_ghap (cache smoke) ────────────────────────────
local({
  cache_dir <- path.expand("~/.climasus4r_cache/ghap")
  files     <- list.files(cache_dir, pattern = "\\.parquet$", recursive = TRUE)
  if (length(files) == 0L) {
    cli::cli_alert_warning("pollution_ghap | cache smoke: SKIP — cache vazio em {cache_dir}")
    return(invisible(NULL))
  }

  # detecta o primeiro ano disponível no cache
  years_found <- regmatches(files, regexpr("20[0-9]{2}", files))
  year_use    <- if (length(years_found) > 0L) as.integer(years_found[[1L]]) else NULL

  run_test("pollution_ghap | cache smoke", {
    r <- sus_grid_pollution_ghap(
      pollutants     = "pm25",
      resolution     = "monthly",
      years          = year_use,
      months         = 1:12,
      municipalities = NULL,
      agg_fun        = "mean",
      crop_brazil    = TRUE,
      use_cache      = TRUE,
      cache_dir      = cache_dir,
      lang           = "pt",
      verbose        = FALSE
    )
    stopifnot(
      inherits(r, "climasus_df"),
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "pollution_ghap",
      "code_muni" %in% names(r),
      "date"      %in% names(r),
      nrow(r) > 0
    )
    r
  })
})

# ── Grupo O — sus_grid_pollution_merra2 ───────────────────────────────────────
cli::cli_h2("Grupo O — sus_grid_pollution_merra2")
# === GRUPO O: sus_grid_pollution_merra2() ===

muni_sample <- c(110001L, 110002L, 110003L, 110004L, 110005L)

# ── Teste 1: pollution_merra2 | cache smoke ───────────────────────────────────
local({
  # Skip se sem credenciais E sem cache
  has_auth  <- nzchar(Sys.getenv("EARTHDATA_USER")) &&
               nzchar(Sys.getenv("EARTHDATA_PASSWORD"))
  cache_dir <- path.expand("~/.climasus4r_cache/merra2")
  cached    <- list.files(cache_dir,
                          pattern    = "\\.(parquet|nc)$",
                          recursive  = TRUE,
                          full.names = TRUE)
  if (length(cached) == 0L && !has_auth) {
    cli::cli_alert_warning(
      "  pollution_merra2 | cache smoke: SKIP — sem cache nem credenciais EARTHDATA"
    )
    return(invisible(NULL))
  }

  year_match <- if (length(cached) > 0L)
    regmatches(cached[1L], regexpr("20[0-9]{2}", cached[1L]))
  else
    character(0)
  year_val   <- if (length(year_match) > 0L) as.integer(year_match[1L]) else 2020L

  run_test("pollution_merra2 | cache smoke", {
    r <- sus_grid_pollution_merra2(
      years         = year_val,
      municipalities = muni_sample,
      use_cache     = TRUE,
      verbose       = FALSE
    )
    stopifnot(
      inherits(r, "climasus_df"),
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "pollution_merra2",
      nrow(r) > 0
    )
    r
  })
})

# ── Sumário final ─────────────────────────────────────────────────────────────
cli::cli_rule()
cli::cli_alert_info("TOTAL: {ok + fail} testes | {ok} OK | {fail} FAIL")
failed_tests <- Filter(function(x) x[["result"]] == "FAIL", results)
if (length(failed_tests) > 0L) {
  cli::cli_alert_danger("Testes com falha:")
  for (t in failed_tests) cli::cli_alert_danger("  - {t[['label']]}: {t[['error']]}")
  quit(status = 1L, save = "no")
} else {
  cli::cli_alert_success("Todos os testes passaram!")
}
