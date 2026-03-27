#' @title Explore the Catalog of Socio-demographic Indicators (Climate Forest UI)
#'
#' @description
#' Provides multiple interfaces to explore the indicators catalog:
#' - Browser: Interactive HTML with Climate Forest theme
#' - DT: DataTables viewer in RStudio
#' - Console: Simple text output
#'
#' @param viewer Character. Interface type: "browser" (default), "dt", or "console".
#' @param lang Language for the catalog: "pt" (default), "en", or "es".
#'
#' @return A tibble containing the indicator catalog (invisibly if viewer is not "console").
#'
#' @importFrom dplyr bind_rows mutate
#' @importFrom tibble as_tibble
#' @export
sus_socio_indicator_explore <- function(viewer = "browser", lang = "pt") {
  
  # 1. Load Metadata
  source("/home/ubuntu/climasus4r/R/indicators_list_v4.R")
  
  # 2. Transform nested list to flat tibble
  catalog <- lapply(names(INDICATORS_LIST), function(id) {
    item <- INDICATORS_LIST[[id]]
    
    # Select name based on language
    name_col <- switch(lang,
                       "pt" = item$name_pt,
                       "en" = item$name_en,
                       "es" = item$name_es)
    
    tibble::tibble(
      id = id,
      name = name_col,
      category = item$category,
      unit = item$unit,
      source = item$source,
      formula = item$formula
    )
  }) %>% 
    dplyr::bind_rows()
  
  # 3. Route to appropriate viewer
  if (viewer == "browser") {
    .open_climate_forest_browser(catalog, lang)
  } else if (viewer == "dt") {
    .open_dt_viewer(catalog, lang)
  } else if (viewer == "console") {
    print(catalog)
  }
  
  invisible(catalog)
}

# ============================================================================
# HELPER: Climate Forest HTML Browser
# ============================================================================
.open_climate_forest_browser <- function(catalog, lang) {
  
  # Color scheme: Climate Forest
  colors <- list(
    primary = "#1b4332",      # Dark forest green
    secondary = "#40916c",    # Medium forest green
    accent = "#52b788",       # Light forest green
    light = "#d8f3dc",        # Very light green
    text = "#1b1b1b",         # Dark text
    border = "#2d6a4f"        # Forest border
  )
  
  # Language strings
  labels <- list(
    pt = list(title = "Catalogo de Indicadores - climasus4r", 
              search = "Buscar...", 
              category = "Categoria",
              formula = "Formula",
              source = "Fonte"),
    en = list(title = "Indicators Catalog - climasus4r",
              search = "Search...",
              category = "Category",
              formula = "Formula",
              source = "Source"),
    es = list(title = "Catalogo de Indicadores - climasus4r",
              search = "Buscar...",
              category = "Categoria",
              formula = "Formula",
              source = "Fuente")
  )[[lang]]
  
  # Build HTML
  html_content <- sprintf('
<!DOCTYPE html>
<html lang="%s">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>%s</title>
  <style>
    * {
      margin: 0;
      padding: 0;
      box-sizing: border-box;
    }
    body {
      font-family: "Segoe UI", Tahoma, Geneva, Verdana, sans-serif;
      background: linear-gradient(135deg, %s 0%%, %s 100%%);
      color: %s;
      padding: 20px;
      min-height: 100vh;
    }
    .container {
      max-width: 1200px;
      margin: 0 auto;
      background: white;
      border-radius: 12px;
      box-shadow: 0 8px 32px rgba(0,0,0,0.1);
      overflow: hidden;
    }
    .header {
      background: linear-gradient(135deg, %s 0%%, %s 100%%);
      color: white;
      padding: 40px 20px;
      text-align: center;
    }
    .header h1 {
      font-size: 32px;
      margin-bottom: 10px;
      text-shadow: 2px 2px 4px rgba(0,0,0,0.2);
    }
    .header p {
      font-size: 14px;
      opacity: 0.9;
    }
    .search-box {
      padding: 20px;
      border-bottom: 2px solid %s;
      background: %s;
    }
    .search-box input {
      width: 100%%;
      padding: 12px 15px;
      border: 2px solid %s;
      border-radius: 6px;
      font-size: 14px;
      transition: border-color 0.3s;
    }
    .search-box input:focus {
      outline: none;
      border-color: %s;
    }
    .table-wrapper {
      overflow-x: auto;
      padding: 20px;
    }
    table {
      width: 100%%;
      border-collapse: collapse;
    }
    th {
      background: %s;
      color: white;
      padding: 15px;
      text-align: left;
      font-weight: 600;
      border-bottom: 2px solid %s;
    }
    td {
      padding: 12px 15px;
      border-bottom: 1px solid #e0e0e0;
    }
    tr:hover {
      background: %s;
    }
    .category-badge {
      display: inline-block;
      background: %s;
      color: white;
      padding: 4px 12px;
      border-radius: 20px;
      font-size: 12px;
      font-weight: 600;
    }
    .footer {
      background: %s;
      color: white;
      padding: 20px;
      text-align: center;
      font-size: 12px;
    }
  </style>
</head>
<body>
  <div class="container">
    <div class="header">
      <h1>%s</h1>
      <p>Explore the complete catalog of health and demographic indicators</p>
    </div>
    <div class="search-box">
      <input type="text" id="searchInput" placeholder="%s" onkeyup="filterTable()">
    </div>
    <div class="table-wrapper">
      <table id="indicatorTable">
        <thead>
          <tr>
            <th>ID</th>
            <th>Name</th>
            <th>%s</th>
            <th>Unit</th>
            <th>%s</th>
          </tr>
        </thead>
        <tbody>
',
    lang,
    labels$title,
    colors$primary,
    colors$secondary,
    colors$text,
    colors$primary,
    colors$secondary,
    colors$border,
    colors$light,
    colors$border,
    colors$secondary,
    colors$secondary,
    colors$border,
    colors$light,
    colors$accent,
    colors$border,
    labels$title,
    labels$search,
    labels$category,
    labels$source
  )
  
  # Add table rows
  for (i in seq_len(nrow(catalog))) {
    html_content <- paste0(html_content, sprintf('
          <tr>
            <td><code>%s</code></td>
            <td>%s</td>
            <td><span class="category-badge">%s</span></td>
            <td>%s</td>
            <td>%s</td>
          </tr>
',
      catalog$id[i],
      catalog$name[i],
      catalog$category[i],
      catalog$unit[i],
      catalog$source[i]
    ))
  }
  
  # Close HTML
  html_content <- paste0(html_content, '
        </tbody>
      </table>
    </div>
    <div class="footer">
      <p>climasus4r - Health, Climate & Spatial Analysis for Public Health</p>
    </div>
  </div>
  
  <script>
    function filterTable() {
      const input = document.getElementById("searchInput");
      const filter = input.value.toUpperCase();
      const table = document.getElementById("indicatorTable");
      const rows = table.getElementsByTagName("tr");
      
      for (let i = 1; i < rows.length; i++) {
        const text = rows[i].textContent || rows[i].innerText;
        rows[i].style.display = text.toUpperCase().includes(filter) ? "" : "none";
      }
    }
  </script>
</body>
</html>
')
  
  # Save and open HTML
  temp_file <- tempfile(fileext = ".html")
  writeLines(html_content, temp_file)
  utils::browseURL(temp_file)
}

# ============================================================================
# HELPER: DataTables Viewer
# ============================================================================
.open_dt_viewer <- function(catalog, lang) {
  if (requireNamespace("DT", quietly = TRUE)) {
    DT::datatable(
      catalog,
      options = list(pageLength = 10, scrollX = TRUE),
      caption = "climasus4r - Indicators Catalog",
      rownames = FALSE,
      filter = "top"
    )
  } else {
    message("Package 'DT' not found. Use viewer='console' or install DT for interactive tables.")
    print(catalog)
  }
}
