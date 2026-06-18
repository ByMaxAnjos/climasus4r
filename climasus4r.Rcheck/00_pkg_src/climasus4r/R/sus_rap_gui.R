# =============================================================================
# sus_rap_gui.R — Shiny GUI for editing and executing RAPs interactively.
#
# Exported: sus_rap_gui(), sus_rap_addin_export()
# =============================================================================

utils::globalVariables(character(0L))

#' Launch an Interactive GUI for Editing and Running RAPs
#'
#' Opens a Shiny application that lets users visually configure pipeline
#' parameters (UF, years, system, aggregation), preview the generated code,
#' and execute or export the RAP — all without writing R code directly.
#'
#' @section Interface panels:
#' \describe{
#'   \item{Left}{Parameter editors: UF checkboxes, year range slider, system
#'     dropdown, aggregation unit selector, language selector.}
#'   \item{Center}{Step-by-step pipeline card view showing each function and
#'     its arguments.}
#'   \item{Right}{Live log console. Buttons: **Preview**, **Run**, **Export**,
#'     **Save Recipe**.}
#' }
#'
#' @param rap A `rap_object` (from [sus_rap_read()]) to pre-populate the
#'   interface. Pass `NULL` (default) to start from a blank template.
#' @param launch.browser `logical(1)` — Open in the system browser.
#'   Default `TRUE`.
#' @param ... Additional arguments forwarded to `shiny::runApp()`.
#'
#' @return Does not return; runs the Shiny application interactively.
#' @export
#'
#' @importFrom cli cli_abort
#' @importFrom rlang %||%
#'
#' @examples
#' \dontrun{
#' sus_rap_gui()
#'
#' rap <- sus_rap_read("pipeline_sp.R")
#' sus_rap_gui(rap)
#' }
sus_rap_gui <- function(rap = NULL, launch.browser = TRUE, ...) {
  rlang::check_installed("shiny", reason = "para abrir a interface grafica RAP")

  if (!is.null(rap) && !inherits(rap, "rap_object"))
    cli::cli_abort("`rap` deve ser um {.cls rap_object} ou NULL.")

  # ── Extract initial values from rap (if provided) ──────────────────────────
  init_uf        <- paste(rap$params$uf        %||% "SP", collapse = ", ")
  init_years_min <- min(rap$params$years        %||% 2019L)
  init_years_max <- max(rap$params$years        %||% 2022L)
  init_system    <- rap$params$system           %||% "SIM-DO"
  init_time_unit <- rap$params$time_unit        %||% "month"
  init_lang      <- rap$params$lang             %||% "pt"
  init_steps     <- if (!is.null(rap) && length(rap$steps) > 0L) {
    vapply(rap$steps, function(s) s$function_name %||% "?", character(1L))
  } else {
    c("sus_data_import", "sus_data_clean_encoding",
      "sus_data_standardize", "sus_data_aggregate")
  }

  systems   <- c("SIM-DO", "SIH", "SINAN", "SIA", "CNES", "SINASC")
  time_units <- c("day", "week", "month", "quarter", "year")
  langs      <- c("pt", "en", "es")

  # ── UI ────────────────────────────────────────────────────────────────────
  ui <- shiny::fluidPage(
    shiny::titlePanel("climasus4r — RAP Editor"),
    shiny::fluidRow(

      # Left panel: parameters
      shiny::column(3,
        shiny::wellPanel(
          shiny::h4("Parametros"),
          shiny::textInput("uf",     "UF(s) (separadas por virgula):", value = init_uf),
          shiny::sliderInput("years", "Anos:", min = 2000L, max = 2024L,
                              value = c(init_years_min, init_years_max),
                              step = 1L, sep = ""),
          shiny::selectInput("system",    "Sistema DATASUS:", choices = systems,
                              selected = init_system),
          shiny::selectInput("time_unit", "Agregacao temporal:", choices = time_units,
                              selected = init_time_unit),
          shiny::selectInput("lang",      "Idioma:", choices = langs,
                              selected = init_lang),
          shiny::textInput("output_dir", "Diretorio de saida:", value = "resultados"),
          shiny::hr(),
          shiny::h4("Acoes"),
          shiny::actionButton("btn_preview", "Previa do Codigo",
                               class = "btn btn-info btn-block"),
          shiny::br(),
          shiny::actionButton("btn_run",     "Executar Pipeline",
                               class = "btn btn-success btn-block"),
          shiny::br(),
          shiny::actionButton("btn_export",  "Exportar RAP",
                               class = "btn btn-primary btn-block"),
          shiny::br(),
          shiny::actionButton("btn_recipe",  "Salvar Receita YAML",
                               class = "btn btn-warning btn-block")
        )
      ),

      # Center panel: pipeline steps
      shiny::column(5,
        shiny::h4("Etapas do Pipeline"),
        shiny::uiOutput("pipeline_cards")
      ),

      # Right panel: log console
      shiny::column(4,
        shiny::h4("Log"),
        shiny::verbatimTextOutput("log_console", placeholder = TRUE),
        shiny::hr(),
        shiny::h4("Codigo Gerado"),
        shiny::verbatimTextOutput("code_preview", placeholder = TRUE)
      )
    )
  )

  # ── Server ─────────────────────────────────────────────────────────────────
  server <- function(input, output, session) {

    log_msgs <- shiny::reactiveVal(character(0L))
    code_txt <- shiny::reactiveVal("")

    .append_log <- function(msg) {
      log_msgs(c(log_msgs(), paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg)))
    }

    .build_rap <- function() {
      ufs   <- trimws(strsplit(input$uf, ",")[[1L]])
      years <- seq(input$years[1L], input$years[2L])
      structure(
        list(
          metadata  = list(),
          params    = list(
            uf        = ufs,
            years     = years,
            system    = input$system,
            time_unit = input$time_unit,
            lang      = input$lang,
            output_dir = input$output_dir,
            seed      = 42L
          ),
          steps = lapply(init_steps, function(fn)
            list(function_name = fn, important_params = list(), line = "")),
          structure = list(
            type          = "Pipeline Interativo",
            input_params  = list(uf = ufs, years = years, system = input$system),
            output_params = list(time_unit = input$time_unit),
            total_steps   = length(init_steps),
            functions_used = init_steps
          ),
          source = NULL,
          format = "interactive",
          raw    = character(0L)
        ),
        class = c("rap_object", "list")
      )
    }

    # Pipeline cards
    output$pipeline_cards <- shiny::renderUI({
      fns <- init_steps
      cards <- lapply(seq_along(fns), function(i) {
        shiny::div(
          class = "well",
          style = "padding: 8px; margin-bottom: 6px;",
          shiny::strong(sprintf("%d. %s()", i, fns[i]))
        )
      })
      do.call(shiny::tagList, cards)
    })

    # Log output
    output$log_console <- shiny::renderText({
      paste(log_msgs(), collapse = "\n")
    })

    # Code preview
    output$code_preview <- shiny::renderText(code_txt())

    # Preview button
    shiny::observeEvent(input$btn_preview, {
      rap  <- .build_rap()
      code <- tryCatch(
        paste(sus_rap_export(
          pipeline   = quote(placeholder),
          file_path  = NULL,
          format     = "script",
          lang       = input$lang,
          overwrite  = TRUE
        ), collapse = "\n"),
        error = function(e) paste("Erro:", e$message)
      )
      code_txt(code)
      .append_log("Previa gerada.")
    })

    # Export button
    shiny::observeEvent(input$btn_export, {
      rap       <- .build_rap()
      out_file  <- file.path(input$output_dir,
                              paste0("rap_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R"))
      tryCatch({
        if (!dir.exists(input$output_dir))
          dir.create(input$output_dir, recursive = TRUE)
        sus_rap_export(
          pipeline  = quote(placeholder),
          file_path = out_file,
          format    = "script",
          lang      = input$lang,
          overwrite = TRUE
        )
        .append_log(paste("RAP exportado:", out_file))
      }, error = function(e) .append_log(paste("Erro na exportacao:", e$message)))
    })

    # Save recipe button
    shiny::observeEvent(input$btn_recipe, {
      rap      <- .build_rap()
      out_file <- file.path(input$output_dir,
                             paste0("rap_recipe_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".yaml"))
      tryCatch({
        if (!dir.exists(input$output_dir))
          dir.create(input$output_dir, recursive = TRUE)
        sus_rap_recipe(rap, file_path = out_file, overwrite = TRUE, lang = input$lang)
        .append_log(paste("Receita salva:", out_file))
      }, error = function(e) .append_log(paste("Erro ao salvar receita:", e$message)))
    })

    # Run button
    shiny::observeEvent(input$btn_run, {
      shiny::showNotification(
        "A execucao do pipeline pode levar varios minutos. Monitore o console R.",
        type = "warning", duration = 8L
      )
      rap <- .build_rap()
      tryCatch({
        .append_log("Iniciando execucao...")
        sus_rap_run(rap, dry_run = TRUE, lang = input$lang)
        .append_log("Use dry_run = FALSE no console R para executar de verdade.")
      }, error = function(e) .append_log(paste("Erro:", e$message)))
    })
  }

  shiny::runApp(shiny::shinyApp(ui, server),
                launch.browser = launch.browser, ...)
}


# =============================================================================
# sus_rap_addin_export() — RStudio addin
# =============================================================================

#' RStudio Addin: Export Selected Pipeline as RAP
#'
#' Captures the selected code in the active RStudio source editor, parses it
#' as a `climasus4r` pipeline, and exports it as a RAP script via
#' [sus_rap_export()]. The output file is placed in the same directory as the
#' active source file.
#'
#' Registered in `inst/rstudio/addins.dcf` as an interactive addin.
#'
#' @return Called for its side effect (creates a `.R` file).
#' @export
#' @keywords internal
sus_rap_addin_export <- function() {
  rlang::check_installed("rstudioapi", reason = "para usar o addin RStudio")

  ctx <- rstudioapi::getSourceEditorContext()
  sel <- ctx$selection[[1L]]$text

  if (nchar(trimws(sel)) == 0L) {
    rstudioapi::showDialog(
      "climasus4r RAP Addin",
      "Selecione um pipeline no editor antes de usar o addin."
    )
    return(invisible(NULL))
  }

  pipeline_expr <- tryCatch(
    parse(text = sel)[[1L]],
    error = function(e) {
      rstudioapi::showDialog("climasus4r RAP Addin",
        paste("Falha ao parsear o codigo selecionado:", e$message))
      return(NULL)
    }
  )

  if (is.null(pipeline_expr)) return(invisible(NULL))

  out_dir  <- dirname(ctx$path)
  out_file <- file.path(out_dir,
                         paste0("rap_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R"))

  tryCatch({
    sus_rap_export(pipeline = pipeline_expr, file_path = out_file,
                   format = "script", lang = "pt", overwrite = FALSE)
    rstudioapi::showDialog("climasus4r RAP Addin",
                            paste("Pipeline exportado com sucesso:\n", out_file))
  }, error = function(e) {
    rstudioapi::showDialog("climasus4r RAP Addin",
                            paste("Erro na exportacao:", e$message))
  })

  invisible(NULL)
}
