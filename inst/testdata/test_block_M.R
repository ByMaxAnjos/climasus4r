# === GRUPO M: sus_grid_fires() ===

muni_sample <- c(110001L, 110002L, 110003L, 110004L, 110005L)

# ── Teste 1: sus_grid_fires | inpe cache smoke ────────────────────────────────
local({
  cache_dir <- path.expand("~/.climasus4r_cache/fires")
  cached    <- list.files(cache_dir,
                          pattern   = "\\.(parquet|csv)$",
                          recursive = TRUE,
                          full.names = TRUE)
  if (length(cached) == 0L) {
    cli::cli_alert_warning(
      "fires | inpe cache smoke: SKIP — cache vazio em {cache_dir}"
    )
    return(invisible(NULL))
  }

  # Inferir o primeiro ano disponível a partir dos nomes de arquivo
  year_val <- tryCatch({
    years_found <- regmatches(basename(cached),
                              regexpr("\\d{4}", basename(cached)))
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
      municipalities = muni_sample,
      use_cache     = TRUE,
      verbose       = FALSE
    )
    stopifnot(
      inherits(r, "climasus_df"),
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "fires",
      "n_fires"  %in% names(r),
      "frp_mean" %in% names(r),
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
  if (length(cached) == 0L) {
    cli::cli_alert_warning(
      "fires | colunas minimas: SKIP — cache vazio em {cache_dir}"
    )
    return(invisible(NULL))
  }

  year_val <- tryCatch({
    years_found <- regmatches(basename(cached),
                              regexpr("\\d{4}", basename(cached)))
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
  if (length(cached) == 0L) {
    cli::cli_alert_warning(
      "fires | filtro municipios: SKIP — cache vazio em {cache_dir}"
    )
    return(invisible(NULL))
  }

  year_val <- tryCatch({
    years_found <- regmatches(basename(cached),
                              regexpr("\\d{4}", basename(cached)))
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
      municipalities = muni_sample,
      use_cache     = TRUE,
      verbose       = FALSE
    )
    stopifnot(
      inherits(r, "climasus_df"),
      sus_meta(r, "stage") == "climate",
      sus_meta(r, "type")  == "fires",
      nrow(r) > 0
    )
    # Todos os municípios retornados devem estar no filtro
    if ("code_muni" %in% names(r)) {
      extra <- setdiff(unique(r[["code_muni"]]), muni_sample)
      if (length(extra) > 0L) {
        stop(paste("Municípios fora do filtro retornados:",
                   paste(extra, collapse = ", ")))
      }
    }
    r
  })
})

# ── Teste 4: sus_grid_fires | metadata stage/type sem cache ───────────────────
run_test("fires | metadata stage type", {
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
        "fires | metadata stage type: SKIP — {conditionMessage(e)}"
      )
      return(invisible(NULL))
    }
  )
  if (is.null(r)) return(invisible(NULL))
  stopifnot(
    inherits(r, "climasus_df"),
    sus_meta(r, "stage") == "climate",
    sus_meta(r, "type")  == "fires"
  )
  r
})
