# =============================================================================
# sus_data_plot_aggregate_map.R
# Choropleth / bubble map of aggregated health counts -- climasus4r package
#
# Exported:
#   sus_data_plot_aggregate_map()   -- main dispatcher
#
# Internal helpers (@noRd):
#   .map_detect_outcome_col()       -- auto-detect outcome column
#   .map_detect_muni_col()          -- auto-detect municipality code column
#   .map_palette_colors()           -- resolve palette string to colour vector
#   .map_msgs                       -- i18n message list
# =============================================================================

# ── NSE variable declarations ─────────────────────────────────────────────────
utils::globalVariables(c(
  "municipio", "lon", "lat", "name", "uf_code", "is_capital", "pop_25",
  "total_cases", "fill_var", "rate", "n_periods",
  "abbrev_state", "lon_lbl", "lat_lbl",
  ".data"
))

# =============================================================================
#  MAIN EXPORTED FUNCTION
# =============================================================================

#' Plot Municipal Map of Aggregated Health Data
#'
#' @description
#' Renders a bubble or choropleth map showing the spatial distribution of
#' health events (counts or incidence rates) at the Brazilian municipal level.
#' Designed for data produced by [sus_data_aggregate()].
#'
#' **Bubble map** (`map_type = "bubble"`) places proportionally sized,
#' colour-encoded circles at each municipality centroid.  State boundaries are
#' drawn as a background polygon layer.
#'
#' **Choropleth map** (`map_type = "choropleth"`) fills municipality polygons
#' downloaded via [geobr::read_municipality()].  Requires `geobr` and `sf`.
#' If these packages are unavailable the function falls back to a bubble map
#' with a warning.
#'
#' @param df A `climasus_df` at `stage = "aggregate"` or later (output of
#'   [sus_data_aggregate()]).
#' @param value_col Character.  Name of the health-count column to map.
#'   If `NULL` (default) the column is auto-detected using the same priority
#'   order as [sus_climate_plot_aggregate()]:
#'   `n_obitos`, `n_internacoes`, `n_nascimentos`, `n_casos`,
#'   `n_procedimentos`, `n_estabelecimentos` and their English / Spanish
#'   equivalents.
#' @param map_type Character.  `"bubble"` (default) or `"choropleth"`.
#' @param rate_per_100k Logical.  If `TRUE`, divides `total_cases` by `pop_25`
#'   (from `municipio_meta`) and multiplies by 1e5 to compute incidence per
#'   100,000 inhabitants.  Default `FALSE`.
#' @param period Date scalar or `c(start, end)` vector.  When supplied, rows
#'   in `df` whose `date` column falls outside the interval are removed before
#'   aggregation.  `NULL` (default) uses all rows.
#' @param top_n Integer.  If supplied, only the top N municipalities by
#'   `total_cases` are labelled on the map.  `NULL` disables top-N labels.
#' @param state_borders Logical.  Overlay state boundary polygons
#'   (downloaded via `geobr`).  Default `TRUE`.
#' @param show_labels Logical.  Annotate capital cities with text labels
#'   (bubble map only).  Default `TRUE`.
#' @param palette Character.  Colour palette for the fill / bubble fill scale.
#'   Any [RColorBrewer::brewer.pal()] sequential/diverging palette name
#'   (e.g. `"YlOrRd"`, `"Blues"`, `"RdYlBu"`) or one of the `ggsci` aliases
#'   `"lancet"` / `"nejm"`.  Default `"YlOrRd"`.
#' @param log_scale Logical.  Apply `log1p` transformation to the colour
#'   scale.  Default `TRUE`.
#' @param title Character.  Map title.  `NULL` uses a built-in multilingual
#'   default.
#' @param base_size Numeric.  Base font size for `theme_void()`.  Default `11`.
#' @param interactive Logical.  If `TRUE`, wraps the ggplot2 object with
#'   `plotly::ggplotly()`.  Requires `plotly`.  Default `FALSE`.
#' @param use_cache Logical.  Cache municipality metadata to disk.
#'   Default `TRUE`.
#' @param cache_dir Character.  Directory for the disk cache.
#'   Default `"~/.climasus4r_cache/spatial"`.
#' @param lang Character.  Language for messages and axis labels:
#'   `"pt"` (default), `"en"`, `"es"`.
#' @param verbose Logical.  Print progress messages.  Default `TRUE`.
#'
#' @return A `ggplot2` object (class `"gg"` / `"ggplot"`), or a `plotly`
#'   object when `interactive = TRUE`.  The function does **not** modify `df`
#'   or advance the pipeline `stage`.
#'
#' @seealso [sus_data_aggregate()], [sus_climate_plot_aggregate()],
#'   [sus_join_spatial()]
#'
#' @examples
#' \dontrun{
#' library(climasus4r)
#'
#' # Monthly deaths by municipality
#' df_agg <- sus_data_import(uf = "RN", year = 2022, system = "SIM-DO") |>
#'   sus_data_standardize() |>
#'   sus_data_aggregate(time_unit = "month",
#'                      group_by  = "codigo_municipio_residencia")
#'
#' # Default bubble map (Portuguese labels)
#' sus_data_plot_aggregate_map(df_agg, lang = "pt")
#'
#' # Choropleth with incidence rate per 100k, English labels
#' sus_data_plot_aggregate_map(
#'   df_agg,
#'   map_type      = "choropleth",
#'   rate_per_100k = TRUE,
#'   palette       = "Blues",
#'   lang          = "en"
#' )
#'
#' # Interactive plotly bubble map
#' sus_data_plot_aggregate_map(df_agg, interactive = TRUE, lang = "pt")
#' }
#'
#' @importFrom dplyr summarise left_join n_distinct group_by across all_of
#'   mutate filter arrange desc slice_max ungroup
#' @importFrom stats quantile
#' @export
sus_data_plot_aggregate_map <- function(
    df,
    value_col      = NULL,
    map_type       = c("bubble", "choropleth"),
    rate_per_100k  = FALSE,
    period         = NULL,
    top_n          = NULL,
    state_borders  = TRUE,
    show_labels    = TRUE,
    palette        = "YlOrRd",
    log_scale      = TRUE,
    title          = NULL,
    base_size      = 11,
    interactive    = FALSE,
    use_cache      = TRUE,
    cache_dir      = "~/.climasus4r_cache/spatial",
    lang           = "pt",
    verbose        = TRUE
) {

  # ---------------------------------------------------------------------------
  # 0.  Package checks
  # ---------------------------------------------------------------------------
  rlang::check_installed("ggplot2",
    reason = "required to build the map plot.")

  # ---------------------------------------------------------------------------
  # 1.  Validate lang
  # ---------------------------------------------------------------------------
  lang <- match.arg(lang, c("pt", "en", "es"))

  # ---------------------------------------------------------------------------
  # 2.  i18n message bundle
  # ---------------------------------------------------------------------------
  .msgs <- list(
    pt = list(
      title_bubble      = "Distribui\u00e7\u00e3o Municipal de Casos",
      title_choropleth  = "Mapa Coropl\u00e9tico Municipal",
      loading_meta      = "Carregando metadados municipais...",
      computing_rate    = "Calculando taxa por 100.000 habitantes...",
      n_muni            = "{n} munic\u00edpios mapeados",
      geobr_missing     = "geobr n\u00e3o dispon\u00edvel: fronteiras estaduais omitidas.",
      fallback_bubble   = "Coropleth requer geobr+sf. Usando mapa de bolhas.",
      done              = "{n} munic\u00edpios, tipo '{type}'.",
      stage_warn        = "stage '{stage}' n\u00e3o \u00e9 'aggregate' ou posterior. O mapa pode estar incompleto.",
      no_outcome        = "Nenhuma coluna de desfecho detectada em {.arg df}. Informe {.arg value_col}.",
      no_muni_col       = "Nenhuma coluna de c\u00f3digo municipal detectada em {.arg df}.",
      no_join           = "Nenhum munic\u00edpio foi pareado com municipio_meta. Verifique os c\u00f3digos.",
      col_used          = "Coluna de desfecho usada: {col}",
      muni_col_used     = "Coluna municipal usada: {col}",
      period_filter     = "Filtrando per\u00edodo: {s} \u2013 {e}",
      fill_label        = "Casos",
      rate_label        = "Taxa / 100k hab.",
      size_label        = "Total de casos",
      caption           = "climasus4r | Fonte: DATASUS"
    ),
    en = list(
      title_bubble      = "Municipal Case Distribution",
      title_choropleth  = "Municipal Choropleth Map",
      loading_meta      = "Loading municipality metadata...",
      computing_rate    = "Computing rate per 100,000 pop.",
      n_muni            = "{n} municipalities mapped",
      geobr_missing     = "geobr unavailable: state borders omitted.",
      fallback_bubble   = "Choropleth requires geobr+sf. Using bubble map.",
      done              = "{n} municipalities, type '{type}'.",
      stage_warn        = "stage '{stage}' is not 'aggregate' or later. Map may be incomplete.",
      no_outcome        = "No health-outcome column detected in {.arg df}. Supply {.arg value_col}.",
      no_muni_col       = "No municipality-code column detected in {.arg df}.",
      no_join           = "No municipality matched municipio_meta. Check your codes.",
      col_used          = "Outcome column used: {col}",
      muni_col_used     = "Municipality column used: {col}",
      period_filter     = "Filtering period: {s} \u2013 {e}",
      fill_label        = "Cases",
      rate_label        = "Rate / 100k pop.",
      size_label        = "Total cases",
      caption           = "climasus4r | Source: DATASUS"
    ),
    es = list(
      title_bubble      = "Distribuci\u00f3n Municipal de Casos",
      title_choropleth  = "Mapa Coropl\u00e9tico Municipal",
      loading_meta      = "Cargando metadatos municipales...",
      computing_rate    = "Calculando tasa por 100.000 hab.",
      n_muni            = "{n} municipios mapeados",
      geobr_missing     = "geobr no disponible: l\u00edmites estatales omitidos.",
      fallback_bubble   = "Coropl\u00e9tico requiere geobr+sf. Usando burbujas.",
      done              = "{n} municipios, tipo '{type}'.",
      stage_warn        = "stage '{stage}' no es 'aggregate' ni posterior. El mapa puede estar incompleto.",
      no_outcome        = "Ninguna columna de desenlace detectada en {.arg df}. Indique {.arg value_col}.",
      no_muni_col       = "Ninguna columna de c\u00f3digo municipal detectada en {.arg df}.",
      no_join           = "Ning\u00fan municipio se emparej\u00f3 con municipio_meta. Verifique los c\u00f3digos.",
      col_used          = "Columna de desenlace usada: {col}",
      muni_col_used     = "Columna municipal usada: {col}",
      period_filter     = "Filtrando per\u00edodo: {s} \u2013 {e}",
      fill_label        = "Casos",
      rate_label        = "Tasa / 100k hab.",
      size_label        = "Total de casos",
      caption           = "climasus4r | Fuente: DATASUS"
    )
  )
  msg <- .msgs[[lang]]

  map_type <- match.arg(map_type)

  # ---------------------------------------------------------------------------
  # 3.  Materialise Arrow lazy table if needed
  # ---------------------------------------------------------------------------
  if (inherits(df, "ArrowObject") ||
      isTRUE(attr(df, "is_arrow_lazy")) ||
      inherits(df, "arrow_dplyr_query")) {
    df <- as.data.frame(df)
  }

  # ---------------------------------------------------------------------------
  # 4.  Class & stage validation
  # ---------------------------------------------------------------------------
  if (!inherits(df, "climasus_df")) {
    cli::cli_warn(c(
      "!" = "{.arg df} is not a {.cls climasus_df} object.",
      "i" = "Results may be unreliable. Run through the climasus4r pipeline first."
    ))
  }

  meta  <- tryCatch(sus_meta(df), error = function(e) NULL)
  stage <- if (!is.null(meta)) meta$stage else "unknown"
  type  <- if (!is.null(meta)) meta$type  else "unknown"

  # Stage order: aggregate is position 7 (1-indexed)
  .valid_stages <- c("aggregate", "spatial", "climate", "census")
  if (!is.na(stage) && !stage %in% .valid_stages) {
    s <- stage
    cli::cli_warn(glue::glue(msg$stage_warn, stage = s))
  }

  # ---------------------------------------------------------------------------
  # 5.  Auto-detect outcome column
  # ---------------------------------------------------------------------------
  if (is.null(value_col)) {
    value_col <- .map_detect_outcome_col(df)
    if (is.null(value_col)) {
      cli::cli_abort(msg$no_outcome)
    }
    if (verbose) {
      col <- value_col
      col <- value_col; cli::cli_alert_success(msg$col_used)
    }
  } else {
    if (!value_col %in% names(df)) {
      cli::cli_abort("Column {.val {value_col}} not found in {.arg df}.")
    }
  }

  # ---------------------------------------------------------------------------
  # 6.  Auto-detect municipality code column
  # ---------------------------------------------------------------------------
  muni_col <- .map_detect_muni_col(df)
  if (is.null(muni_col)) {
    cli::cli_abort(msg$no_muni_col)
  }
  if (verbose) {
    col <- muni_col
    col <- muni_col; cli::cli_alert_success(msg$muni_col_used)
  }

  # ---------------------------------------------------------------------------
  # 7.  Optional period filter
  # ---------------------------------------------------------------------------
  if (!is.null(period)) {
    if (!"date" %in% names(df)) {
      cli::cli_warn("Column 'date' not found; period filter skipped.")
    } else {
      period <- as.Date(period)
      p_start <- period[1]
      p_end   <- if (length(period) >= 2) period[2] else Sys.Date()
      if (verbose) {
        s <- format(p_start); e <- format(p_end)
        cli::cli_alert_info(glue::glue(msg$period_filter, s = s, e = e))
      }
      df <- df[!is.na(df$date) &
                 df$date >= p_start &
                 df$date <= p_end, ]
    }
  }

  # ---------------------------------------------------------------------------
  # 8.  Aggregate df by municipality code
  # ---------------------------------------------------------------------------
  df[[muni_col]] <- as.character(df[[muni_col]])

  # Trim to 6 digits if 7-digit codes are present
  df$._muni6 <- substr(df[[muni_col]], 1L, 6L)

  if ("date" %in% names(df)) {
    muni_agg <- df |>
      dplyr::group_by(.data$._muni6) |>
      dplyr::summarise(
        total_cases = sum(.data[[value_col]], na.rm = TRUE),
        n_periods   = dplyr::n_distinct(.data$date),
        .groups     = "drop"
      )
  } else {
    muni_agg <- df |>
      dplyr::group_by(.data$._muni6) |>
      dplyr::summarise(
        total_cases = sum(.data[[value_col]], na.rm = TRUE),
        n_periods   = dplyr::n(),
        .groups     = "drop"
      )
  }

  # ---------------------------------------------------------------------------
  # 9.  Load municipality metadata
  # ---------------------------------------------------------------------------
  if (verbose) cli::cli_alert_info(msg$loading_meta)

  muni_meta <- tryCatch(
    get_spatial_municipio_cache(cache_dir, use_cache, lang, verbose),
    error = function(e) {
      cli::cli_warn(c(
        "!" = "Could not load municipio_meta: {e$message}",
        "i" = "Map will have no spatial reference."
      ))
      NULL
    }
  )

  if (is.null(muni_meta) || nrow(muni_meta) == 0L) {
    cli::cli_abort("municipio_meta could not be loaded. Cannot build map.")
  }

  # Ensure 6-digit join key in meta
  muni_meta$._code6 <- substr(as.character(muni_meta$municipio), 1L, 6L)

  # ---------------------------------------------------------------------------
  # 10.  Join
  # ---------------------------------------------------------------------------
  muni_data <- dplyr::left_join(
    muni_agg,
    muni_meta,
    by = c("._muni6" = "._code6")
  )

  n_matched <- sum(!is.na(muni_data$lon))
  if (n_matched == 0L) {
    cli::cli_abort(msg$no_join)
  }

  if (verbose) {
    n <- n_matched
    cli::cli_alert_success(glue::glue(msg$n_muni, n = n))
  }

  # ---------------------------------------------------------------------------
  # 11.  Incidence rate (optional)
  # ---------------------------------------------------------------------------
  if (rate_per_100k) {
    if (verbose) cli::cli_alert_info(msg$computing_rate)
    if (!"pop_25" %in% names(muni_data)) {
      cli::cli_warn(c(
        "!" = "Column {.val pop_25} not found in municipio_meta.",
        "i" = "Falling back to raw counts."
      ))
      rate_per_100k <- FALSE
    } else {
      muni_data$rate <- ifelse(
        !is.na(muni_data$pop_25) & muni_data$pop_25 > 0,
        (muni_data$total_cases / muni_data$pop_25) * 1e5,
        NA_real_
      )
    }
  }

  muni_data$fill_var <- if (rate_per_100k) muni_data$rate else muni_data$total_cases

  # Remove rows with missing coordinates
  muni_plot <- muni_data[!is.na(muni_data$lon) & !is.na(muni_data$lat), ]

  # ---------------------------------------------------------------------------
  # 12.  Colour palette
  # ---------------------------------------------------------------------------
  color_vec <- .map_palette_colors(palette, n = 9L)

  # ---------------------------------------------------------------------------
  # 13.  Axis / legend labels
  # ---------------------------------------------------------------------------
  fill_lbl <- if (rate_per_100k) msg$rate_label else msg$fill_label
  size_lbl <- msg$size_label
  cap_lbl  <- msg$caption

  # Title
  default_title <- if (map_type == "bubble") msg$title_bubble else msg$title_choropleth
  map_title <- if (!is.null(title)) title else default_title

  # Scale transformation
  sc_trans <- if (log_scale) "log1p" else "identity"

  # ---------------------------------------------------------------------------
  # 14.  State borders (geobr)
  # ---------------------------------------------------------------------------
  state_sf <- NULL
  if (state_borders || map_type == "choropleth") {
    has_geobr <- requireNamespace("geobr",  quietly = TRUE)
    has_sf    <- requireNamespace("sf",     quietly = TRUE)
    if (!has_geobr || !has_sf) {
      cli::cli_warn(msg$geobr_missing)
      state_borders <- FALSE
      if (map_type == "choropleth") {
        cli::cli_warn(msg$fallback_bubble)
        map_type <- "bubble"
      }
    } else {
      state_sf <- tryCatch(
        {
          s <- geobr::read_state(year = 2020, simplified = TRUE,
                                 showProgress = FALSE)
          sf::st_transform(s, crs = 4326)
        },
        error = function(e) {
          cli::cli_warn(c(
            "!" = "geobr::read_state() failed: {e$message}",
            "i" = "State borders will be omitted."
          ))
          NULL
        }
      )
      if (is.null(state_sf)) state_borders <- FALSE
    }
  }

  # ---------------------------------------------------------------------------
  # 15.  Build scale_fill layer (shared between bubble & choropleth)
  # ---------------------------------------------------------------------------
  fill_scale <- ggplot2::scale_fill_gradientn(
    colors = color_vec,
    trans  = sc_trans,
    name   = fill_lbl,
    labels = scales::label_comma(),
    guide  = ggplot2::guide_colorbar(
      barwidth  = ggplot2::unit(10, "lines"),
      barheight = ggplot2::unit(0.5, "lines"),
      title.position = "top",
      title.hjust    = 0.5
    )
  )

  # ---------------------------------------------------------------------------
  # 16.  Map-type branch
  # ---------------------------------------------------------------------------

  if (map_type == "bubble") {
    # ── 16a. Bubble map ────────────────────────────────────────────────────

    # Capital city labels
    capitals <- NULL
    if (show_labels && "is_capital" %in% names(muni_plot)) {
      caps_raw <- muni_plot[isTRUE(muni_plot$is_capital) |
                              muni_plot$is_capital %in% c(TRUE, 1L, "TRUE", "1"), ]
      if (nrow(caps_raw) > 0L && "uf_code" %in% names(caps_raw)) {
        # One label per state: city with most cases
        caps_raw$._uf <- as.character(caps_raw$uf_code)
        caps_by_uf <- split(caps_raw, caps_raw$._uf)
        capitals <- do.call(rbind, lapply(caps_by_uf, function(sub) {
          sub[which.max(sub$total_cases), , drop = FALSE]
        }))
      } else if (nrow(caps_raw) > 0L) {
        capitals <- caps_raw
      }
    }

    # Top-N municipality labels (overrides capital labels if both non-NULL)
    top_labels <- NULL
    if (!is.null(top_n) && is.numeric(top_n) && top_n > 0L) {
      top_labels <- muni_plot[
        order(muni_plot$total_cases, decreasing = TRUE)[
          seq_len(min(top_n, nrow(muni_plot)))
        ], , drop = FALSE
      ]
    }

    # Arrange by descending total_cases so large bubbles are drawn first (back)
    muni_plot <- muni_plot[order(muni_plot$total_cases, decreasing = TRUE), ]

    p <- ggplot2::ggplot()

    # State outlines
    if (!is.null(state_sf)) {
      p <- p + ggplot2::geom_sf(
        data      = state_sf,
        fill      = "#F0EDE8",
        color     = "#7A8C99",
        linewidth = 0.45,
        inherit.aes = FALSE
      )
    }

    # Bubble layer
    p <- p + ggplot2::geom_point(
      data  = muni_plot,
      ggplot2::aes(
        x    = .data$lon,
        y    = .data$lat,
        size = .data$total_cases,
        fill = .data$fill_var
      ),
      shape  = 21,
      color  = "white",
      stroke = 0.2,
      alpha  = 0.85
    )

    # Size scale
    p <- p + ggplot2::scale_size_continuous(
      range  = c(0.8, 8),
      trans  = "log1p",
      name   = size_lbl,
      guide  = ggplot2::guide_legend(
        override.aes = list(
          shape  = 21,
          fill   = "#FD8D3C",
          color  = "white",
          stroke = 0.25,
          alpha  = 0.85
        )
      )
    )

    # Fill scale
    p <- p + fill_scale

    # Capital / top-N text labels
    label_data <- if (!is.null(top_labels)) top_labels else capitals
    if (!is.null(label_data) && nrow(label_data) > 0L &&
        "name" %in% names(label_data)) {
      if (requireNamespace("ggrepel", quietly = TRUE)) {
        p <- p + ggrepel::geom_label_repel(
          data          = label_data,
          ggplot2::aes(x = .data$lon, y = .data$lat,
                       label = .data$name),
          size          = 2.0,
          fontface      = "bold",
          color         = "#1A252F",
          fill          = scales::alpha("white", 0.82),
          label.padding = ggplot2::unit(0.15, "lines"),
          label.size    = 0.2,
          box.padding   = 0.55,
          segment.color = "#1A252F",
          segment.size  = 0.3,
          max.overlaps  = 20,
          na.rm         = TRUE,
          inherit.aes   = FALSE
        )
      } else {
        p <- p + ggplot2::geom_text(
          data        = label_data,
          ggplot2::aes(x = .data$lon, y = .data$lat,
                       label = .data$name),
          size        = 2.2,
          color       = "#3A3A3A",
          fontface    = "bold",
          alpha       = 0.8,
          inherit.aes = FALSE
        )
      }
    }

  } else {
    # ── 16b. Choropleth map ────────────────────────────────────────────────
    # geobr and sf are available (checked in step 14)

    muni_poly <- tryCatch(
      {
        mp <- geobr::read_municipality(year = 2020, simplified = TRUE,
                                       showProgress = FALSE)
        sf::st_transform(mp, crs = 4326)
      },
      error = function(e) {
        cli::cli_warn(c(
          "!" = "geobr::read_municipality() failed: {e$message}",
          "i" = msg$fallback_bubble
        ))
        NULL
      }
    )

    if (is.null(muni_poly)) {
      # Fallback to bubble
      return(
        sus_data_plot_aggregate_map(
          df            = df,
          value_col     = value_col,
          map_type      = "bubble",
          rate_per_100k = rate_per_100k,
          period        = NULL,   # already filtered
          top_n         = top_n,
          state_borders = state_borders,
          show_labels   = show_labels,
          palette       = palette,
          log_scale     = log_scale,
          title         = title,
          base_size     = base_size,
          interactive   = interactive,
          use_cache     = FALSE,  # already loaded
          cache_dir     = cache_dir,
          lang          = lang,
          verbose       = FALSE
        )
      )
    }

    # Standardise code column in geobr output
    code_col_poly <- if ("code_muni" %in% names(muni_poly)) "code_muni" else
      names(muni_poly)[grepl("code|muni|ibge", names(muni_poly),
                             ignore.case = TRUE)][1]

    muni_poly$._code6_poly <- substr(
      as.character(muni_poly[[code_col_poly]]), 1L, 6L
    )

    # Join health data to polygons
    poly_joined <- dplyr::left_join(
      muni_poly,
      muni_data[, c("._muni6", "total_cases", "fill_var"), drop = FALSE],
      by = c("._code6_poly" = "._muni6")
    )

    p <- ggplot2::ggplot()

    # Municipality polygons (choropleth fill)
    p <- p + ggplot2::geom_sf(
      data      = poly_joined,
      ggplot2::aes(fill = .data$fill_var),
      color     = "white",
      linewidth = 0.1,
      inherit.aes = FALSE
    )

    p <- p + fill_scale

    # State borders on top
    if (!is.null(state_sf)) {
      p <- p + ggplot2::geom_sf(
        data      = state_sf,
        fill      = NA,
        color     = "#5A6E7A",
        linewidth = 0.5,
        inherit.aes = FALSE
      )
    }
  }

  # ---------------------------------------------------------------------------
  # 17.  Common theme & labels
  # ---------------------------------------------------------------------------
  p <- p +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::labs(
      title   = map_title,
      caption = cap_lbl
    ) +
    ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(
      legend.position    = "bottom",
      legend.direction   = "horizontal",
      legend.box         = "horizontal",
      legend.title       = ggplot2::element_text(size = base_size - 1,
                                                  face = "bold"),
      legend.text        = ggplot2::element_text(size = base_size - 2),
      plot.margin        = ggplot2::margin(4, 4, 4, 4),
      plot.title         = ggplot2::element_text(face = "bold",
                                                  size = base_size + 1,
                                                  hjust = 0.5),
      plot.caption       = ggplot2::element_text(size   = base_size - 2,
                                                  color  = "#888888",
                                                  hjust  = 1)
    )

  # ---------------------------------------------------------------------------
  # 18.  Verbose summary
  # ---------------------------------------------------------------------------
  if (verbose) {
    n    <- n_matched
    tp   <- type
    cli::cli_alert_success(glue::glue(msg$done, n = n, type = tp))
  }

  # ---------------------------------------------------------------------------
  # 19.  Interactive wrapper
  # ---------------------------------------------------------------------------
  if (interactive) {
    rlang::check_installed("plotly",
      reason = "required when interactive = TRUE.")
    return(plotly::ggplotly(p))
  }

  p
}


# =============================================================================
#  INTERNAL HELPERS
# =============================================================================

#' Auto-detect health-outcome column
#' @keywords internal
#' @noRd
.map_detect_outcome_col <- function(df) {
  # Exclude known non-outcome columns
  meta_cols <- c(
    "date", "code_muni", "code_muni_7", "name_muni",
    "codigo_municipio_residencia", "codigo_municipio_ocorrencia",
    "codigo_municipio_nascimento", "codigo_municipio",
    "residence_municipality_code", "municipality_code",
    "codigo_municipio_notificacao", "codigo_municipio_paciente",
    "uf_municipio_estabelecimento", "CODMUNRES", "MUNI_RES",
    "code_state", "abbrev_state", "name_state",
    "geom", "geometry", "._muni6"
  )
  candidate <- setdiff(names(df), meta_cols)
  num_cols  <- candidate[vapply(candidate, function(col) {
    is.integer(df[[col]]) || is.numeric(df[[col]])
  }, logical(1))]

  # Priority: canonical outcome-column name patterns
  preferred_rx <- c(
    "^n_obitos$", "^n_internacoes$", "^n_nascimentos$",
    "^n_casos$",  "^n_procedimentos$", "^n_estabelecimentos$",
    "^n_deaths$", "^n_hospitalizations$", "^n_births$",
    "^n_cases$",  "^n_procedures$", "^n_establishments$",
    "^n_muertes$", "^n_hospitalizaciones$", "^n_nacimientos$",
    "^n_procedimientos$", "^n_establecimientos$",
    "^n_", "obito", "internac", "caso", "morte",
    "hospitaliz", "death", "case", "admission", "birth", "nascimento"
  )

  for (rx in preferred_rx) {
    matched <- num_cols[grepl(rx, num_cols, ignore.case = TRUE, perl = TRUE)]
    if (length(matched) > 0L) return(matched[1])
  }

  if (length(num_cols) > 0L) return(num_cols[1])
  NULL
}

#' Auto-detect municipality code column
#' @keywords internal
#' @noRd
.map_detect_muni_col <- function(df) {
  priority <- c(
    "codigo_municipio_residencia",
    "residence_municipality_code",
    "codigo_municipio_ocorrencia",
    "codigo_municipio_ocurrencia",
    "occurrence_municipality_code",
    "codigo_municipio_notificacao",
    "codigo_municipio_notificacion",
    "notification_municipality_code",
    "codigo_municipio_nascimento",
    "codigo_municipio_nacimiento",
    "birth_municipality_code",
    "codigo_municipio_paciente",
    "patient_municipality_code",
    "uf_municipio_estabelecimento",
    "facility_uf_municipality",
    "uf_municipio_establecimiento",
    "codigo_municipio",
    "municipality_code",
    "code_muni",
    "CODMUNRES",
    "MUNI_RES"
  )
  found <- priority[priority %in% names(df)]
  if (length(found) > 0L) return(found[1])

  # Fallback: any column whose name looks like a municipality code
  rx <- "muni|municipio|municipality|ibge"
  fallback <- names(df)[grepl(rx, names(df), ignore.case = TRUE)]
  if (length(fallback) > 0L) return(fallback[1])

  NULL
}

#' Resolve palette string to colour vector
#' @keywords internal
#' @noRd
.map_palette_colors <- function(palette, n = 9L) {
  # ggsci aliases
  if (palette %in% c("lancet", "nejm")) {
    if (requireNamespace("ggsci", quietly = TRUE)) {
      pal_fn <- switch(palette,
        lancet = ggsci::pal_lancet(),
        nejm   = ggsci::pal_nejm()
      )
      cols <- pal_fn(min(n, 9L))
      return(cols)
    }
    # Fallback if ggsci not available
    palette <- "YlOrRd"
  }

  # RColorBrewer palettes
  if (requireNamespace("RColorBrewer", quietly = TRUE)) {
    # Check if it's a valid brewer palette
    all_pals <- rownames(RColorBrewer::brewer.pal.info)
    if (palette %in% all_pals) {
      max_n <- RColorBrewer::brewer.pal.info[palette, "maxcolors"]
      return(RColorBrewer::brewer.pal(min(n, max_n), palette))
    }
  }

  # Built-in fallback: YlOrRd-like gradient
  grDevices::colorRampPalette(
    c("#FFEDA0", "#FEB24C", "#FC4E2A", "#E31A1C", "#800026")
  )(n)
}
