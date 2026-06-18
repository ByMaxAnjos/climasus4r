#' Display the climasus4r Pipeline Overview
#'
#' Prints a colour-coded pipeline overview to the console and, optionally,
#' opens a self-contained HTML page in the browser (or RStudio viewer) showing
#' all ten stages of the climasus4r health–climate–environment workflow plus
#' utility functions.
#'
#' @param lang Character. Language: `"pt"` (default), `"en"`, or `"es"`.
#' @param output Character vector. Outputs to produce: `"console"`, `"html"`,
#'   or both (default). Multiple values are accepted.
#' @param open Logical. If `TRUE` (default) and the session is interactive,
#'   opens the HTML file automatically.
#'
#' @return Invisibly returns the path to the HTML file (or `NULL` when
#'   `"html"` is not in `output`). Called for its side effects.
#'
#' @examples
#' \dontrun{
#' sus_welcome()
#' sus_welcome(lang = "en")
#' sus_welcome(output = "html", open = TRUE)
#' sus_welcome(lang = "es", output = c("console", "html"))
#' }
#'
#' @export
#' @importFrom cli cli_rule cli_text style_bold style_italic col_blue col_green col_cyan col_magenta col_yellow
#' @importFrom rlang is_interactive
sus_welcome <- function(
    lang   = "pt",
    output = c("console", "html"),
    open   = TRUE
) {
  lang   <- match.arg(lang,   c("pt", "en", "es"))
  output <- match.arg(output, c("console", "html"), several.ok = TRUE)

  ver <- tryCatch(
    as.character(utils::packageVersion("climasus4r")),
    error = function(e) "1.0.0"
  )

  stages <- .welcome_stages(lang)

  if ("console" %in% output) .welcome_console(stages, lang, ver)

  html_path <- NULL
  if ("html" %in% output) {
    html_path <- .welcome_html(stages, lang, ver)
    if (open && rlang::is_interactive()) {
      if (requireNamespace("rstudioapi", quietly = TRUE) &&
          rstudioapi::isAvailable()) {
        rstudioapi::viewer(html_path)
      } else {
        utils::browseURL(html_path)
      }
    }
  }

  invisible(html_path)
}

# ── Stage definitions ─────────────────────────────────────────────────────────
# num = 0 marks the utility section (rendered differently: no number badge,
# no arrow, distinct colour scheme).

.welcome_stages <- function(lang) {
  raw <- list(
    list(num = 1, color = "blue",
         name = c(pt = "IMPORTAÇÃO",          en = "IMPORT",                    es = "IMPORTACIÓN"),
         fns  = c("sus_data_import()", "sus_data_read()"),
         desc = c(
           pt = "Importa dados do DATASUS (SIM, SIH, SINAN, SIA, CNES, SINASC) com cache automático e processamento paralelo",
           en = "Imports DATASUS data (SIM, SIH, SINAN, SIA, CNES, SINASC) with automatic caching and parallel processing",
           es = "Importa datos del DATASUS (SIM, SIH, SINAN, SIA, CNES, SINASC) con caché automático y procesamiento paralelo"
         )),
    list(num = 2, color = "blue",
         name = c(pt = "PREPARAÇÃO E LIMPEZA", en = "DATA PREPARATION",         es = "PREPARACIÓN"),
         fns  = c("sus_data_clean_encoding()", "sus_data_standardize()"),
         desc = c(
           pt = "Corrige encoding Latin1/UTF-8, padroniza 80+ nomes de colunas e valores categóricos",
           en = "Fixes Latin1/UTF-8 encoding, standardises 80+ column names and categorical values",
           es = "Corrige encoding Latin1/UTF-8, estandariza 80+ nombres de columnas y valores categóricos"
         )),
    list(num = 3, color = "blue",
         name = c(pt = "FILTRAGEM E DERIVAÇÃO",  en = "FILTERING & VARIABLES",  es = "FILTRADO Y DERIVACIÓN"),
         fns  = c("sus_data_filter_cid()", "sus_data_filter_demographics()", "sus_create_variables()"),
         desc = c(
           pt = "Filtra por CID-10, variáveis demográficas e cria variáveis derivadas (idade, sazonalidade, faixas)",
           en = "Filters by ICD-10, demographic variables and creates derived variables (age, seasonality, bands)",
           es = "Filtra por CIE-10, variables demográficas y crea variables derivadas (edad, estacionalidad, franjas)"
         )),
    list(num = 4, color = "blue",
         name = c(pt = "AGREGAÇÃO",              en = "AGGREGATION",             es = "AGREGACIÓN"),
         fns  = c("sus_data_aggregate()", "sus_data_quality_report()"),
         desc = c(
           pt = "Agrega registros individuais por município/data e gera relatório de qualidade dos dados",
           en = "Aggregates individual records by municipality/date and generates a data quality report",
           es = "Agrega registros individuales por municipio/fecha y genera reporte de calidad de datos"
         )),
    list(num = 5, color = "green",
         name = c(pt = "INTEGRAÇÃO ESPACIAL",    en = "SPATIAL JOIN",            es = "INTEGRACIÓN ESPACIAL"),
         fns  = c("sus_join_spatial()"),
         desc = c(
           pt = "Vincula dados de saúde a polígonos municipais e estaduais brasileiros via geobr + sf",
           en = "Links health data to Brazilian municipal and state polygons via geobr + sf",
           es = "Vincula datos de salud a polígonos municipales y estatales de Brasil via geobr + sf"
         )),
    list(num = 6, color = "cyan",
         name = c(pt = "INTEGRAÇÃO CLIMÁTICA",   en = "CLIMATE DATA",            es = "DATOS CLIMÁTICOS"),
         fns  = c("sus_climate_inmet()", "sus_climate_fill_gap()", "sus_climate_aggregate()",
                  "sus_grid_era5()", "sus_grid_chirps()", "sus_grid_pollution_cams()",
                  "sus_grid_fires()", "sus_climate_compute_heatwaves()", "sus_climate_compute_coldwaves()",
                  "sus_climate_compute_spei()"),
         desc = c(
           pt = "Estações INMET, ERA5, CHIRPS, qualidade do ar (CAMS, MERRA-2), incêndios, ondas de calor e frio, SPEI/SPI",
           en = "INMET stations, ERA5, CHIRPS, air quality (CAMS, MERRA-2), fires, heat and cold waves, SPEI/SPI",
           es = "Estaciones INMET, ERA5, CHIRPS, calidad del aire (CAMS, MERRA-2), incendios, olas de calor y frío, SPEI/SPI"
         )),
    list(num = 7, color = "cyan",
         name = c(pt = "SOCIOECONÔMICO",          en = "SOCIOECONOMIC",           es = "SOCIOECONÓMICO"),
         fns  = c("sus_socio_add_census()", "sus_socio_compute_indicators()"),
         desc = c(
           pt = "Adiciona indicadores do Censo IBGE e índices compostos de vulnerabilidade socioeconômica",
           en = "Adds IBGE Census indicators and composite socioeconomic vulnerability indices",
           es = "Agrega indicadores del Censo IBGE e índices compuestos de vulnerabilidad socioeconómica"
         )),
    list(num = 8, color = "magenta",
         name = c(pt = "MODELAGEM EPIDEMIOLÓGICA", en = "EPIDEMIOLOGICAL MODELLING", es = "MODELADO EPIDEMIOLÓGICO"),
         fns  = c("sus_mod_dlnm()", "sus_mod_af()", "sus_mod_burden()",
                  "sus_mod_casecrossover()", "sus_mod_excess()", "sus_mod_its()",
                  "sus_mod_pool()", "sus_mod_metaregression()", "sus_mod_sensitivity()",
                  "sus_mod_ml()", "sus_mod_spatial_bayes()", "sus_mod_spacetime_bayes()"),
         desc = c(
           pt = "DLNM, fração atribuível, carga de doença, caso-cruzado, ITS, pooling, meta-regressão, ML e análise espacial Bayesiana",
           en = "DLNM, attributable fraction, disease burden, case-crossover, ITS, pooling, meta-regression, ML and Bayesian spatial analysis",
           es = "DLNM, fracción atribuible, carga de enfermedad, caso-cruzado, ITS, pooling, meta-regresión, ML y análisis espacial Bayesiano"
         )),
    list(num = 9, color = "yellow",
         name = c(pt = "VISUALIZAÇÃO",            en = "VISUALIZATION",           es = "VISUALIZACIÓN"),
         fns  = c(
           # Dados de saúde
           "sus_data_plot_demographics()", "sus_data_plot_aggregate_ts()", "sus_data_plot_aggregate_map()",
           # Clima
           "sus_climate_plot_aggregate()", "sus_climate_plot_fill()",
           "sus_climate_plot_heatwaves()", "sus_climate_plot_coldwaves()",
           # Modelos
           "sus_mod_plot_dlnm()", "sus_mod_plot_af()", "sus_mod_plot_burden()",
           "sus_mod_plot_pool()", "sus_mod_plot_sensitivity()", "sus_mod_plot_ml()",
           "sus_mod_plot_spacetime()", "sus_mod_plot_spatial_bayes()", "sus_mod_plot_spatial_moran()",
           "sus_mod_plot_spatial_scan()", "sus_mod_plot_swot()", "sus_mod_plot_vulnerability()"
         ),
         desc = c(
           pt = "Pirâmide etária, série temporal, mapa coroplético; ondas de calor e frio; superfícies DLNM, forest plot, carga, spatial scan, vulnerabilidade",
           en = "Demographic pyramid, time series, choropleth map; heat and cold waves; DLNM surfaces, forest plot, burden, spatial scan, vulnerability",
           es = "Pirámide demográfica, serie temporal, mapa coroplético; olas de calor y frío; superficies DLNM, forest plot, carga, spatial scan, vulnerabilidad"
         )),
    list(num = 10, color = "yellow",
         name = c(pt = "EXPORTAÇÃO",              en = "EXPORT",                  es = "EXPORTACIÓN"),
         fns  = c("sus_data_export()", "write_parquet_climasus()", "write_duckdb_climasus()"),
         desc = c(
           pt = "Exporta para CSV, Parquet ou DuckDB preservando todos os metadados do pipeline (sus_meta)",
           en = "Exports to CSV, Parquet or DuckDB preserving all pipeline metadata (sus_meta)",
           es = "Exporta a CSV, Parquet o DuckDB preservando todos los metadatos del pipeline (sus_meta)"
         )),
    # num = 0 → utility section (no stage number, no connecting arrow)
    list(num = 0, color = "slate",
         name = c(pt = "UTILITÁRIOS",             en = "UTILITIES",               es = "UTILIDADES"),
         fns  = c("sus_meta()", "sus_chat()"),
         desc = c(
           pt = "sus_meta() lê e escreve metadados do pipeline em qualquer backend · sus_chat() abre o assistente de IA do climasus4r",
           en = "sus_meta() reads and writes pipeline metadata across any backend · sus_chat() opens the climasus4r AI assistant",
           es = "sus_meta() lee y escribe metadatos del pipeline en cualquier backend · sus_chat() abre el asistente de IA de climasus4r"
         ))
  )

  lapply(raw, function(s) {
    s$name_l <- s$name[[lang]]
    s$desc_l <- s$desc[[lang]]
    s
  })
}

# ── Console output ────────────────────────────────────────────────────────────

.welcome_console <- function(stages, lang, ver) {
  title <- switch(lang,
    pt = "Pipeline Integrado de Análise Saúde–Clima–Ambiente no Brasil",
    en = "Integrated Health–Climate–Environment Analysis Pipeline for Brazil",
    es = "Pipeline Integrado de Análisis Salud–Clima–Ambiente en Brasil"
  )
  lbl <- switch(lang,
    pt = list(lang = "Idioma",   docs = "Docs", cite = "Citação",  util = "Utilitários"),
    en = list(lang = "Language", docs = "Docs", cite = "Citation", util = "Utilities"),
    es = list(lang = "Idioma",   docs = "Docs", cite = "Citar",    util = "Utilidades")
  )

  color_fn <- function(col) {
    switch(col,
      blue    = cli::col_blue,
      green   = cli::col_green,
      cyan    = cli::col_cyan,
      magenta = cli::col_magenta,
      yellow  = cli::col_yellow,
      slate   = function(x) cli::col_grey(x),
      identity
    )
  }

  cli::cli_rule(
    left  = cli::style_bold(paste0("climasus4r ", ver)),
    right = "github.com/ByMaxAnjos/climasus4r"
  )
  cli::cli_text(cli::style_italic(title))
  cli::cli_rule()
  cat("\n")

  # Pipeline stages (num > 0)
  pipe_stages <- Filter(function(s) s$num > 0, stages)
  for (i in seq_along(pipe_stages)) {
    s  <- pipe_stages[[i]]
    fn <- color_fn(s$color)
    cat(fn(cli::style_bold(paste0("  ", s$num, ". ", s$name_l))), "\n")
    # Wrap function names: 3 per line for readability
    fns_rows <- split(s$fns, ceiling(seq_along(s$fns) / 3))
    for (row in fns_rows) cat("     ", paste(row, collapse = "  "), "\n")
    cat("\n")
    if (i < length(pipe_stages)) cat("               ↓\n\n")
  }

  # Utility section (num == 0)
  util_stages <- Filter(function(s) s$num == 0, stages)
  if (length(util_stages) > 0) {
    cli::cli_rule()
    for (s in util_stages) {
      fn <- color_fn(s$color)
      cat(fn(cli::style_bold(paste0("  ◆ ", s$name_l))), "\n")
      cat("     ", paste(s$fns, collapse = "  "), "\n\n")
    }
  }

  cli::cli_rule()
  cli::cli_text(paste0(lbl$lang, ": {.val ", lang, "} · Backends: tibble · parquet · duckdb"))
  cli::cli_text(paste0(lbl$docs, ": {.url https://byMaxAnjos.github.io/climasus4r}"))
  cli::cli_text(paste0(lbl$cite, ': {.code citation("climasus4r")}'))
  cli::cli_rule()

  invisible(NULL)
}

# ── HTML output ───────────────────────────────────────────────────────────────

.welcome_html <- function(stages, lang, ver) {
  title <- switch(lang,
    pt = "Pipeline Integrado de Análise Saúde–Clima–Ambiente",
    en = "Integrated Health–Climate–Environment Analysis Pipeline",
    es = "Pipeline Integrado de Análisis Salud–Clima–Ambiente"
  )
  subtitle <- switch(lang,
    pt = paste0("Brasil · climasus4r v", ver),
    en = paste0("Brazil · climasus4r v", ver),
    es = paste0("Brasil · climasus4r v", ver)
  )
  footer_label <- switch(lang,
    pt = paste0("climasus4r v", ver,
      " &middot; Idioma: ", lang,
      " &middot; Backends: tibble &middot; parquet &middot; duckdb &middot; ",
      "<a href='https://github.com/ByMaxAnjos/climasus4r'>GitHub</a>"),
    en = paste0("climasus4r v", ver,
      " &middot; Language: ", lang,
      " &middot; Backends: tibble &middot; parquet &middot; duckdb &middot; ",
      "<a href='https://github.com/ByMaxAnjos/climasus4r'>GitHub</a>"),
    es = paste0("climasus4r v", ver,
      " &middot; Idioma: ", lang,
      " &middot; Backends: tibble &middot; parquet &middot; duckdb &middot; ",
      "<a href='https://github.com/ByMaxAnjos/climasus4r'>GitHub</a>")
  )

  palette <- list(
    blue    = list(bg = "#dbe4ff", border = "#4c6ef5", text = "#1e3a8a", badge = "#4c6ef5"),
    green   = list(bg = "#d3f9d8", border = "#2f9e44", text = "#1b5e20", badge = "#2f9e44"),
    cyan    = list(bg = "#c3fae8", border = "#0ca678", text = "#004d38", badge = "#0ca678"),
    magenta = list(bg = "#e5dbff", border = "#7950f2", text = "#3b0764", badge = "#7950f2"),
    yellow  = list(bg = "#fff3bf", border = "#e67700", text = "#7c4a00", badge = "#f08c00"),
    slate   = list(bg = "#f1f5f9", border = "#94a3b8", text = "#1e293b", badge = "#64748b")
  )

  pipe_stages <- Filter(function(s) s$num > 0, stages)
  util_stages <- Filter(function(s) s$num == 0, stages)

  # Build pipeline cards
  pipe_cards <- vapply(seq_along(pipe_stages), function(i) {
    s <- pipe_stages[[i]]
    p <- palette[[s$color]]
    pills <- paste(
      sprintf('<span class="pill" style="background:%s">%s</span>', p$badge, s$fns),
      collapse = "\n        "
    )
    arrow <- if (i < length(pipe_stages))
      sprintf('\n  <div class="arrow" style="color:%s">&#8595;</div>', p$border)
    else ""
    paste0(
      sprintf('\n  <div class="card" style="border-color:%s;background:%s">\n', p$border, p$bg),
      sprintf('    <div class="card-head">\n      <span class="num" style="background:%s">%d</span>\n      <span class="name" style="color:%s">%s</span>\n    </div>\n',
              p$badge, s$num, p$text, s$name_l),
      sprintf('    <div class="pills">\n        %s\n    </div>\n', pills),
      sprintf('    <p class="desc" style="color:%s">%s</p>\n  </div>', p$text, s$desc_l),
      arrow
    )
  }, character(1))

  # Build utility cards
  util_cards <- if (length(util_stages) > 0) {
    util_label <- switch(lang,
      pt = "Utilitários", en = "Utilities", es = "Utilidades"
    )
    inner <- vapply(util_stages, function(s) {
      p <- palette[["slate"]]
      pills <- paste(
        sprintf('<span class="pill" style="background:%s">%s</span>', p$badge, s$fns),
        collapse = "\n        "
      )
      paste0(
        sprintf('<div class="util-card" style="border-color:%s;background:%s">', p$border, p$bg),
        sprintf('<div class="card-head"><span class="util-icon" style="background:%s">&#9670;</span><span class="name" style="color:%s">%s</span></div>',
                p$badge, p$text, s$name_l),
        sprintf('<div class="pills">\n        %s\n    </div>', pills),
        sprintf('<p class="desc" style="color:%s">%s</p></div>', p$text, s$desc_l)
      )
    }, character(1))
    paste0(
      '\n  <div class="util-section">\n',
      '    <div class="util-divider"><span>', util_label, '</span></div>\n',
      '    ', paste(inner, collapse = "\n    "), '\n',
      '  </div>'
    )
  } else ""

  css <- paste0(
    '*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}',
    'body{font-family:-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,sans-serif;',
         'background:#0f172a;color:#e2e8f0;min-height:100vh;padding:2rem 1rem}',
    '.wrap{max-width:760px;margin:0 auto}',
    'header{text-align:center;margin-bottom:2.5rem}',
    '.logo{font-size:2.4rem;font-weight:800;letter-spacing:-1px;',
          'background:linear-gradient(135deg,#4c6ef5 0%,#0ca678 50%,#7950f2 100%);',
          '-webkit-background-clip:text;-webkit-text-fill-color:transparent;background-clip:text}',
    '.sub{color:#94a3b8;font-size:.9rem;margin-top:.35rem}',
    '.card{width:100%;border:2px solid;border-radius:12px;padding:1rem 1.3rem;',
          'transition:transform .15s,box-shadow .15s;cursor:default}',
    '.card:hover{transform:translateX(5px);box-shadow:-5px 5px 18px rgba(0,0,0,.35)}',
    '.card-head{display:flex;align-items:center;gap:.7rem;margin-bottom:.6rem}',
    '.num{color:#fff;font-weight:700;font-size:.75rem;border-radius:50%;',
         'width:26px;height:26px;display:flex;align-items:center;justify-content:center;flex-shrink:0}',
    '.name{font-weight:700;font-size:.88rem;letter-spacing:.04em;text-transform:uppercase}',
    '.pills{display:flex;flex-wrap:wrap;gap:.3rem;margin-bottom:.5rem}',
    '.pill{font-family:"SF Mono","Fira Code",Consolas,monospace;font-size:.67rem;',
          'padding:.17rem .42rem;border-radius:5px;color:#fff;font-weight:600;opacity:.92}',
    '.desc{font-size:.78rem;line-height:1.55;opacity:.82}',
    '.arrow{text-align:center;font-size:1.5rem;padding:.15rem 0;opacity:.65;user-select:none}',
    '.util-section{margin-top:1.8rem}',
    '.util-divider{display:flex;align-items:center;gap:.8rem;margin-bottom:1rem;color:#64748b;font-size:.78rem;font-weight:600;letter-spacing:.06em;text-transform:uppercase}',
    '.util-divider::before,.util-divider::after{content:"";flex:1;height:1px;background:#1e293b}',
    '.util-card{border:2px solid;border-radius:12px;padding:1rem 1.3rem;',
               'transition:transform .15s,box-shadow .15s;cursor:default}',
    '.util-card:hover{transform:translateX(5px);box-shadow:-5px 5px 18px rgba(0,0,0,.25)}',
    '.util-icon{color:#fff;font-size:.7rem;border-radius:4px;',
               'width:26px;height:26px;display:flex;align-items:center;justify-content:center;flex-shrink:0}',
    'footer{text-align:center;margin-top:2.5rem;color:#64748b;font-size:.76rem;line-height:1.9}',
    'footer a{color:#818cf8;text-decoration:none}',
    'footer a:hover{text-decoration:underline}'
  )

  html <- paste0(
    '<!DOCTYPE html>\n<html lang="', lang, '">\n<head>\n',
    '<meta charset="UTF-8">\n',
    '<meta name="viewport" content="width=device-width,initial-scale=1.0">\n',
    '<title>climasus4r v', ver, '</title>\n',
    '<style>', css, '</style>\n</head>\n<body>\n',
    '<div class="wrap">\n',
    '  <header>\n',
    '    <div class="logo">climasus4r</div>\n',
    '    <div class="sub">', title, '<br><small>', subtitle, '</small></div>\n',
    '  </header>\n',
    paste(pipe_cards, collapse = "\n"),
    util_cards,
    '\n  <footer>', footer_label, '</footer>\n',
    '</div>\n</body>\n</html>'
  )

  path <- tempfile(fileext = ".html")
  writeLines(html, path, useBytes = FALSE)
  path
}
