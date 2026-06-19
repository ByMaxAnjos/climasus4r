# =============================================================================
# sus_rap_template.R  - Scaffold a full reproducible analysis project directory.
#
# Exported: sus_rap_template()
# =============================================================================

utils::globalVariables(character(0L))

#' Scaffold a Reproducible Analytical Pipeline Project
#'
#' Creates a ready-to-use project directory with a `_targets.R` pipeline,
#' a parameterised Quarto report, optional `renv` dependency snapshot,
#' optional GitHub Actions CI workflow, and a structured file layout
#' following RAP best practices.
#'
#' @section Generated structure:
#' ```
#' <name>/
#' |-- _targets.R          # targets pipeline
#' |-- run.R               # convenience runner: source + tar_make()
#' |-- analysis.qmd        # parameterised Quarto report
#' |-- R/
#' |   `-- functions.R     # custom analysis functions
#' |-- data/               # raw data (git-ignored)
#' |-- output/             # results (git-ignored)
#' |-- renv/               # renv library (when include_renv = TRUE)
#' |-- renv.lock
#' |-- .gitignore
#' `-- README.md
#' ```
#'
#' @param path `character(1)`  - Parent directory where `<name>/` will be
#'   created (e.g. `"~/projetos"`).
#' @param name `character(1)`  - Project name; becomes the directory name.
#' @param system `character(1)`  - DATASUS system for the template pipeline
#'   (e.g. `"SIM-DO"`, `"SINAN"`, `"SIH"`). Default `"SIM-DO"`.
#' @param include_renv `logical(1)`  - Initialise `renv` for dependency
#'   management. Requires the `renv` package. Default `TRUE`.
#' @param include_targets `logical(1)`  - Create `_targets.R` and `run.R`.
#'   Default `TRUE`.
#' @param include_quarto `logical(1)`  - Create `analysis.qmd`. Default `TRUE`.
#' @param include_github_actions `logical(1)`  - Create
#'   `.github/workflows/rap.yml`. Default `FALSE`.
#' @param lang `character(1)`  - Language for generated comments and messages:
#'   `"pt"` (default), `"en"`, or `"es"`.
#'
#' @return Invisibly, the absolute path of the created project directory.
#' @export
#'
#' @importFrom cli cli_h1 cli_alert_info cli_alert_success cli_abort
#' @importFrom rlang %||%
#'
#' @examples
#' \dontrun{
#' sus_rap_template(
#'   path   = "~/projetos",
#'   name   = "dengue_am",
#'   system = "SINAN",
#'   lang   = "pt"
#' )
#' }
sus_rap_template <- function(
    path,
    name,
    system                  = "SIM-DO",
    include_renv            = TRUE,
    include_targets         = TRUE,
    include_quarto          = TRUE,
    include_github_actions  = FALSE,
    lang                    = "pt"
) {
  lang <- match.arg(lang, c("pt", "en", "es"))

  if (!is.character(path) || length(path) != 1L || nchar(path) == 0L)
    cli::cli_abort("`path` deve ser uma string nao vazia.")
  if (!is.character(name) || length(name) != 1L || nchar(name) == 0L)
    cli::cli_abort("`name` deve ser uma string nao vazia.")

  proj_dir <- file.path(path, name)
  if (dir.exists(proj_dir))
    cli::cli_abort("Diretorio '{proj_dir}' ja existe.")

  cli::cli_h1(if (lang == "en") "Creating RAP project: {name}" else
              if (lang == "es") "Creando proyecto RAP: {name}" else
                                "Criando projeto RAP: {name}")

  # -- Create directory structure ----------------------------------------------
  dirs_to_create <- c(proj_dir,
                      file.path(proj_dir, "R"),
                      file.path(proj_dir, "data"),
                      file.path(proj_dir, "output"))
  for (d in dirs_to_create) dir.create(d, recursive = TRUE)
  cli::cli_alert_info("Estrutura de diretorios criada.")

  # -- _targets.R -------------------------------------------------------------
  if (include_targets) {
    .write_template_file(proj_dir, "_targets.R",
                         .rap_tmpl_targets(name, system, lang))
    .write_template_file(proj_dir, "run.R",
                         .rap_tmpl_run(lang))
    cli::cli_alert_info("_targets.R e run.R criados.")
  }

  # -- Quarto report ----------------------------------------------------------
  if (include_quarto) {
    .write_template_file(proj_dir, "analysis.qmd",
                         .rap_tmpl_quarto(name, system, lang))
    cli::cli_alert_info("analysis.qmd criado.")
  }

  # -- R/functions.R ----------------------------------------------------------
  .write_template_file(proj_dir, file.path("R", "functions.R"),
                       .rap_tmpl_functions(lang))

  # -- .gitignore -------------------------------------------------------------
  .write_template_file(proj_dir, ".gitignore",
                       c("data/", "output/", "_targets/", ".Rproj.user/",
                         "renv/library/", "*.Rproj", ".Rhistory", ".RData"))

  # -- README.md --------------------------------------------------------------
  .write_template_file(proj_dir, "README.md",
                       .rap_tmpl_readme(name, system, lang))

  # -- GitHub Actions ---------------------------------------------------------
  if (include_github_actions) {
    gha_dir <- file.path(proj_dir, ".github", "workflows")
    dir.create(gha_dir, recursive = TRUE)
    .write_template_file(gha_dir, "rap.yml",
                         .rap_tmpl_gha(name, lang))
    cli::cli_alert_info(".github/workflows/rap.yml criado.")
  }

  # -- renv --------------------------------------------------------------------
  if (include_renv) {
    rlang::check_installed("renv", reason = "para inicializar renv",
                           call = NULL)
    renv::init(project = proj_dir, bare = TRUE)
    cli::cli_alert_info("renv inicializado.")
  }

  cli::cli_alert_success(
    if (lang == "en") "Project '{name}' ready at {.path {proj_dir}}" else
    if (lang == "es") "Proyecto '{name}' listo en {.path {proj_dir}}" else
                      "Projeto '{name}' pronto em {.path {proj_dir}}"
  )

  invisible(proj_dir)
}


# -- Internal template content generators -------------------------------------

#' @keywords internal
#' @noRd
.write_template_file <- function(dir, filename, lines) {
  writeLines(lines, file.path(dir, filename), useBytes = FALSE)
}

#' @keywords internal
#' @noRd
.rap_tmpl_targets <- function(name, system, lang) {
  c(
    "# _targets.R  - Generated by climasus4r::sus_rap_template()",
    sprintf("# Project: %s", name),
    "",
    "library(targets)",
    "library(climasus4r)",
    "",
    "list(",
    "  # -- Parameters ----------------------------------------------------------",
    "  tar_target(params, list(",
    "    uf         = \"SP\",   # edit: UF code(s)",
    "    years      = 2019:2021,  # edit: year range",
    sprintf("    system     = \"%s\",", system),
    "    time_unit  = \"month\",",
    "    output_dir = \"output\",",
    sprintf("    lang       = \"%s\"", lang),
    "  )),",
    "",
    "  # -- Import ---------------------------------------------------------------",
    "  tar_target(raw_data,",
    "    sus_data_import(",
    "      uf       = params$uf,",
    "      years    = params$years,",
    "      system   = params$system,",
    "      parallel = TRUE,",
    "      lang     = params$lang",
    "    )",
    "  ),",
    "",
    "  # -- Clean & standardise --------------------------------------------------",
    "  tar_target(clean_data,",
    "    raw_data |>",
    "      sus_data_clean_encoding(lang = params$lang) |>",
    "      sus_data_standardize(lang = params$lang)",
    "  ),",
    "",
    "  # -- Aggregate ------------------------------------------------------------",
    "  tar_target(agg_data,",
    "    clean_data |>",
    "      sus_data_aggregate(by = params$time_unit, lang = params$lang)",
    "  ),",
    "",
    "  # -- Quality control ------------------------------------------------------",
    "  tar_target(qc_report,",
    "    sus_data_quality_report(clean_data, lang = params$lang)",
    "  ),",
    "",
    "  NULL",
    ")"
  )
}

#' @keywords internal
#' @noRd
.rap_tmpl_run <- function(lang) {
  c(
    "#!/usr/bin/env Rscript",
    "# run.R  - execute the targets pipeline",
    "library(targets)",
    "targets::tar_make()"
  )
}

#' @keywords internal
#' @noRd
.rap_tmpl_quarto <- function(name, system, lang) {
  title_str <- if (lang == "en") sprintf("Analysis: %s", name) else
               if (lang == "es") sprintf("Analisis: %s", name) else
                                  sprintf("Analise: %s", name)
  c(
    "---",
    sprintf("title: \"%s\"", title_str),
    sprintf("author: \"%s\"", Sys.getenv("USER", "Analista")),
    sprintf("date: \"%s\"", format(Sys.Date(), "%Y-%m-%d")),
    "format:",
    "  html:",
    "    toc: true",
    "    code-fold: true",
    "    theme: flatly",
    "execute:",
    "  echo: true",
    "  warning: false",
    "params:",
    "  uf: \"SP\"",
    "  years: \"2019:2021\"",
    sprintf("  system: \"%s\"", system),
    sprintf("  lang: \"%s\"", lang),
    "---",
    "",
    "```{r setup, include=FALSE}",
    "library(targets)",
    "library(climasus4r)",
    "library(dplyr)",
    "```",
    "",
    "## Pipeline results",
    "",
    "```{r results}",
    "tar_load(agg_data)",
    "head(agg_data)",
    "```",
    ""
  )
}

#' @keywords internal
#' @noRd
.rap_tmpl_functions <- function(lang) {
  c(
    "# R/functions.R  - Custom analysis functions",
    "# Add project-specific helper functions here.",
    ""
  )
}

#' @keywords internal
#' @noRd
.rap_tmpl_readme <- function(name, system, lang) {
  if (lang == "en") {
    c(
      sprintf("# %s", name),
      "",
      sprintf("Reproducible analysis pipeline using `climasus4r`  - system: %s.", system),
      "",
      "## How to reproduce",
      "",
      "1. Install dependencies: `renv::restore()`",
      "2. Run the pipeline: `targets::tar_make()`",
      "3. View the report: open `analysis.qmd` in RStudio and render.",
      ""
    )
  } else if (lang == "es") {
    c(
      sprintf("# %s", name),
      "",
      sprintf("Pipeline de analisis reproducible con `climasus4r`  - sistema: %s.", system),
      "",
      "## Como reproducir",
      "",
      "1. Instalar dependencias: `renv::restore()`",
      "2. Ejecutar el pipeline: `targets::tar_make()`",
      "3. Ver el informe: abrir `analysis.qmd` en RStudio y renderizar.",
      ""
    )
  } else {
    c(
      sprintf("# %s", name),
      "",
      sprintf("Pipeline de analise reprodutivel usando `climasus4r`  - sistema: %s.", system),
      "",
      "## Como reproduzir",
      "",
      "1. Instalar dependencias: `renv::restore()`",
      "2. Executar o pipeline: `targets::tar_make()`",
      "3. Ver o relatorio: abrir `analysis.qmd` no RStudio e renderizar.",
      ""
    )
  }
}

#' @keywords internal
#' @noRd
.rap_tmpl_gha <- function(name, lang) {
  c(
    "name: Reproducible Analysis Pipeline",
    "",
    "on:",
    "  push:",
    "    branches: [main]",
    "  pull_request:",
    "    branches: [main]",
    "  workflow_dispatch:",
    "",
    "jobs:",
    "  pipeline:",
    "    runs-on: ubuntu-latest",
    "    steps:",
    "      - uses: actions/checkout@v4",
    "",
    "      - uses: r-lib/actions/setup-r@v2",
    "        with:",
    "          r-version: '4.4'",
    "",
    "      - uses: r-lib/actions/setup-renv@v2",
    "",
    "      - name: Run targets pipeline",
    "        run: Rscript run.R",
    "",
    "      - name: Upload results",
    "        uses: actions/upload-artifact@v4",
    "        with:",
    "          name: pipeline-results",
    "          path: output/",
    ""
  )
}
