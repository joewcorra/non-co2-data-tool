library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(sf)

# ── Dummy Data Generation ──────────────────────────────────────────────────────

set.seed(42)

countries <- c(
  "United States", "China", "India", "Russia", "Brazil",
  "Germany", "Japan", "Canada", "Australia", "South Korea",
  "Mexico", "Indonesia", "Saudi Arabia", "South Africa", "France",
  "United Kingdom", "Italy", "Argentina", "Iran", "Turkey"
)

iso3 <- c(
  "USA", "CHN", "IND", "RUS", "BRA",
  "DEU", "JPN", "CAN", "AUS", "KOR",
  "MEX", "IDN", "SAU", "ZAF", "FRA",
  "GBR", "ITA", "ARG", "IRN", "TUR"
)

gases    <- c("All Gases (CO2e)", "CH4", "N2O", "HFCs", "PFCs", "SF6")
sectors  <- c("Agriculture", "Energy", "Industrial Processes", "Waste")
years    <- 2000:2025
regions  <- c("All", "Asia", "Europe", "North America", "South America", "Africa", "Oceania")

country_region <- tibble(
  country = countries,
  iso3    = iso3,
  region  = c("North America", "Asia", "Asia", "Europe", "South America",
               "Europe", "Asia", "North America", "Oceania", "Asia",
               "North America", "Asia", "Asia", "Africa", "Europe",
               "Europe", "Europe", "South America", "Asia", "Asia")
)

# Base emissions (MtCO2e) — roughly plausible magnitudes
base_emissions <- tibble(
  country = countries,
  iso3    = iso3
) |>
  mutate(base = c(6000, 12000, 3500, 2500, 1200,
                  800,  1300, 730,  560,  650,
                  720,  900,  680,  480,  450,
                  560,  430,  380,  720,  500))

# Expand to full grid: country × year × sector × gas
emissions_data <- tidyr::expand_grid(
  country = countries,
  year    = years,
  sector  = sectors,
  gas     = gases[-1]   # individual gases; CO2e is derived
) |>
  left_join(base_emissions, by = "country") |>
  mutate(
    sector_share = case_when(
      sector == "Energy"                ~ 0.55,
      sector == "Agriculture"           ~ 0.22,
      sector == "Industrial Processes"  ~ 0.15,
      sector == "Waste"                 ~ 0.08
    ),
    gas_share = case_when(
      gas == "CH4"  ~ 0.45,
      gas == "N2O"  ~ 0.30,
      gas == "HFCs" ~ 0.15,
      gas == "PFCs" ~ 0.06,
      gas == "SF6"  ~ 0.04
    ),
    trend   = 1 + (year - 2000) * 0.012,
    noise   = runif(n(), 0.88, 1.12),
    emissions = base * sector_share * gas_share * trend * noise
  ) |>
  left_join(country_region |> select(country, region), by = "country") |>
  select(country, iso3, region, year, sector, gas, emissions)

# Helper: aggregate to CO2e totals for the map
get_map_data <- function(data_type, gas_sel, year_sel, region_sel) {
  df <- emissions_data |>
    filter(year == year_sel)

  if (region_sel != "All") df <- df |> filter(region == region_sel)

  if (gas_sel != "All Gases (CO2e)") {
    df <- df |> filter(gas == gas_sel)
  }

  df |>
    group_by(country, iso3) |>
    summarise(total = sum(emissions), .groups = "drop")
}

get_pie_data <- function(data_type, gas_sel, year_sel, country_sel = NULL) {
  df <- emissions_data |> filter(year == year_sel)

  if (!is.null(country_sel) && length(country_sel) > 0) {
    df <- df |> filter(country %in% country_sel)
  }

  if (gas_sel != "All Gases (CO2e)") {
    df <- df |> filter(gas == gas_sel)
  }

  df |>
    group_by(sector) |>
    summarise(total = sum(emissions), .groups = "drop")
}

# ── World GeoJSON (Natural Earth via CDN) ─────────────────────────────────────
# We'll use the rnaturalearth package if available, else a bundled URL
world_geojson_url <- "https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_110m_admin_0_countries.geojson"

# ── UI ────────────────────────────────────────────────────────────────────────

sector_colors <- c(
  Agriculture          = "#8BC34A",
  Energy               = "#FFC107",
  `Industrial Processes` = "#1565C0",
  Waste                = "#8D6E63"
)

ui <- fluidPage(
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Source+Sans+3:wght@300;400;600;700&family=Source+Serif+4:wght@400;600&display=swap"
    ),
    tags$style(HTML("

      /* ── Reset & Base ── */
      * { box-sizing: border-box; margin: 0; padding: 0; }

      body {
        font-family: 'Source Sans 3', sans-serif;
        background: #f4f6f9;
        color: #1a2332;
        font-size: 14px;
      }

      /* ── Top banner ── */
      .app-banner {
        background: #1a3a5c;
        color: white;
        padding: 10px 24px;
        display: flex;
        align-items: center;
        gap: 16px;
        border-bottom: 3px solid #c8a951;
      }
      .app-banner .epa-seal {
        width: 52px; height: 52px;
        background: white;
        border-radius: 50%;
        display: flex; align-items: center; justify-content: center;
        font-weight: 700; font-size: 13px; color: #1a3a5c;
        flex-shrink: 0;
        letter-spacing: -0.5px;
      }
      .app-banner-text h1 {
        font-family: 'Source Serif 4', serif;
        font-size: 18px; font-weight: 600;
        line-height: 1.2;
      }
      .app-banner-text p {
        font-size: 12px; opacity: 0.8; margin-top: 2px;
      }

      /* ── Layout ── */
      .app-body {
        display: flex;
        gap: 0;
        min-height: calc(100vh - 74px);
      }

      /* ── Sidebar ── */
      .sidebar-panel {
        width: 260px;
        flex-shrink: 0;
        background: #ffffff;
        border-right: 1px solid #dde3ec;
        padding: 20px 16px;
        display: flex;
        flex-direction: column;
        gap: 16px;
      }

      .sidebar-section-label {
        font-size: 11px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.08em;
        color: #6b7a99;
        margin-bottom: 6px;
      }

      .sidebar-panel .form-group { margin-bottom: 0; }

      .sidebar-panel label {
        font-weight: 600;
        font-size: 13px;
        color: #1a2332;
        margin-bottom: 4px;
        display: block;
      }

      .sidebar-panel .form-control {
        border: 1px solid #c8d0de;
        border-radius: 4px;
        font-size: 13px;
        color: #1a2332;
        background: #f9fafb;
        height: 36px;
        padding: 0 10px;
        width: 100%;
        appearance: none;
        background-image: url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='12' height='12' viewBox='0 0 12 12'%3E%3Cpath fill='%236b7a99' d='M6 8L1 3h10z'/%3E%3C/svg%3E\");
        background-repeat: no-repeat;
        background-position: right 10px center;
        cursor: pointer;
        transition: border-color 0.15s;
      }
      .sidebar-panel .form-control:focus {
        border-color: #1a6bb5;
        outline: none;
        background-color: #fff;
      }

      .btn-go {
        width: 100%;
        background: #1a6bb5;
        color: white;
        border: none;
        border-radius: 4px;
        height: 40px;
        font-size: 14px;
        font-weight: 700;
        letter-spacing: 0.05em;
        cursor: pointer;
        transition: background 0.15s;
        margin-top: 4px;
      }
      .btn-go:hover { background: #145a9a; }

      .sidebar-divider {
        border: none;
        border-top: 1px solid #eaecf2;
        margin: 4px 0;
      }

      /* Selected countries chips */
      .selected-countries {
        font-size: 12px;
        color: #444f6b;
        line-height: 1.6;
      }
      .selected-countries strong {
        display: block;
        font-size: 11px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.07em;
        color: #6b7a99;
        margin-bottom: 4px;
      }
      .country-chip {
        display: inline-block;
        background: #e8f0fb;
        color: #1a3a5c;
        border-radius: 12px;
        padding: 2px 8px;
        margin: 2px 2px 2px 0;
        font-size: 11px;
        font-weight: 600;
      }
      .clear-btn {
        background: none;
        border: none;
        color: #c0392b;
        font-size: 11px;
        font-weight: 600;
        cursor: pointer;
        padding: 0;
        text-decoration: underline;
        margin-top: 4px;
        display: block;
      }

      /* ── Main content ── */
      .main-panel {
        flex: 1;
        padding: 16px;
        display: flex;
        flex-direction: column;
        gap: 16px;
        min-width: 0;
      }

      /* ── Summary bar ── */
      .summary-bar {
        background: #1a3a5c;
        color: white;
        border-radius: 6px;
        padding: 10px 18px;
        display: flex;
        align-items: center;
        gap: 24px;
        font-size: 13px;
      }
      .summary-bar .total-label { opacity: 0.75; }
      .summary-bar .total-value {
        font-family: 'Source Serif 4', serif;
        font-size: 22px;
        font-weight: 600;
        color: #c8a951;
      }
      .summary-bar .total-unit { font-size: 11px; opacity: 0.65; }
      .summary-divider {
        width: 1px; height: 36px;
        background: rgba(255,255,255,0.2);
      }

      /* ── Viz row ── */
      .viz-row {
        display: flex;
        gap: 16px;
        flex: 1;
        min-height: 480px;
      }

      .viz-card {
        background: white;
        border-radius: 6px;
        border: 1px solid #dde3ec;
        display: flex;
        flex-direction: column;
        overflow: hidden;
      }

      .viz-card-header {
        padding: 10px 14px 8px;
        border-bottom: 1px solid #eaecf2;
        display: flex;
        align-items: baseline;
        gap: 8px;
      }
      .viz-card-header h2 {
        font-family: 'Source Serif 4', serif;
        font-size: 14px;
        font-weight: 600;
        color: #1a2332;
      }
      .viz-card-header span {
        font-size: 11px;
        color: #6b7a99;
      }

      .map-card { flex: 3; }
      .pie-card { flex: 2; }

      .map-card .leaflet-container { height: 100% !important; }

      /* ── Footer ── */
      .app-footer {
        background: #1a3a5c;
        color: rgba(255,255,255,0.6);
        font-size: 11px;
        padding: 8px 24px;
        text-align: center;
      }

      /* Shiny output wrappers */
      #map_out { flex: 1; }
      #pie_out { flex: 1; }
      .viz-card > .shiny-bound-output { flex: 1; }
    "))
  ),

  # Banner
  div(class = "app-banner",
    div(class = "epa-seal", "EPA"),
    div(class = "app-banner-text",
      tags$h1("Global Non-CO\u2082 GHG Emissions Viewer"),
      tags$p("Prototype \u2022 Data: Synthetic (for demonstration only) \u2022 GWPs: IPCC AR5")
    )
  ),

  # Body
  div(class = "app-body",

    # ── Sidebar ──
    div(class = "sidebar-panel",
      div(class = "sidebar-section-label", "Filters"),

      div(class = "form-group",
        tags$label("Data Type"),
        selectInput("data_type", NULL,
          choices  = c("Emissions", "Consumption"),
          selected = "Emissions", width = "100%")
      ),

      div(class = "form-group",
        tags$label("Greenhouse Gas"),
        selectInput("gas", NULL,
          choices  = gases,
          selected = "All Gases (CO2e)", width = "100%")
      ),

      div(class = "form-group",
        tags$label("Year"),
        selectInput("year", NULL,
          choices  = rev(years),
          selected = 2025, width = "100%")
      ),

      div(class = "form-group",
        tags$label("Region"),
        selectInput("region", NULL,
          choices  = regions,
          selected = "All", width = "100%")
      ),

      actionButton("go", "Go", class = "btn-go"),

      tags$hr(class = "sidebar-divider"),

      # Selected countries
      div(class = "selected-countries",
        tags$strong("Map Selection"),
        uiOutput("selected_chips"),
        actionButton("clear_sel", "Clear selection", class = "clear-btn")
      ),

      tags$hr(class = "sidebar-divider"),

      div(class = "sidebar-section-label", "About"),
      div(style = "font-size:11px; color:#6b7a99; line-height:1.6;",
        "Click a country on the map to drill into its sector breakdown.
         Hold Shift to select up to 4 countries. Prototype only \u2014
         all values are synthetic."
      )
    ),

    # ── Main ──
    div(class = "main-panel",

      # Summary bar
      uiOutput("summary_bar"),

      # Viz row
      div(class = "viz-row",

        div(class = "viz-card map-card",
          div(class = "viz-card-header",
            tags$h2("Non-CO\u2082 Emissions by Country"),
            tags$span("Click to select \u2022 Shift+click for up to 4 countries")
          ),
          leafletOutput("map_out", height = "100%")
        ),

        div(class = "viz-card pie-card",
          div(class = "viz-card-header",
            tags$h2("Sector Breakdown"),
            uiOutput("pie_subtitle")
          ),
          plotlyOutput("pie_out", height = "100%")
        )
      )
    )
  ),

  div(class = "app-footer",
    "U.S. Environmental Protection Agency \u2022 Office of Atmospheric Protection \u2022
     Prototype for demonstration purposes only"
  ),

  # JS for shift-click multi-select on map
  tags$script(HTML("
    var selectedCountries = [];

    Shiny.addCustomMessageHandler('clearSelection', function(msg) {
      selectedCountries = [];
      Shiny.setInputValue('selected_countries', [], {priority: 'event'});
    });

    $(document).on('shiny:connected', function() {
      // Shift-click handled via leaflet click events sent from R
    });
  "))
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Reactive values
  rv <- reactiveValues(
    selected_countries = character(0),
    map_data  = NULL,
    pie_data  = NULL,
    world_sf  = NULL
  )

  # Load world geometry once
  observe({
    withProgress(message = "Loading map geometry...", {
      tryCatch({
        world <- sf::read_sf(world_geojson_url)
        rv$world_sf <- world
      }, error = function(e) {
        showNotification("Could not load map geometry. Check internet connection.",
                         type = "error")
      })
    })
  })

  # Recompute on Go
  observeEvent(input$go, {
    rv$map_data <- get_map_data(input$data_type, input$gas,
                                as.integer(input$year), input$region)
    rv$pie_data <- get_pie_data(input$data_type, input$gas,
                                as.integer(input$year),
                                if (length(rv$selected_countries) > 0)
                                  rv$selected_countries else NULL)
  }, ignoreNULL = FALSE)

  # Also recompute when selection changes
  observeEvent(rv$selected_countries, {
    rv$pie_data <- get_pie_data(input$data_type, input$gas,
                                as.integer(input$year),
                                if (length(rv$selected_countries) > 0)
                                  rv$selected_countries else NULL)
  })

  # Clear selection
  observeEvent(input$clear_sel, {
    rv$selected_countries <- character(0)
  })

  # ── Summary bar ──
  output$summary_bar <- renderUI({
    req(rv$map_data)
    total <- sum(rv$map_data$total, na.rm = TRUE)
    total_fmt <- format(round(total, 1), big.mark = ",")

    n_countries <- nrow(rv$map_data)
    gas_label   <- input$gas
    year_label  <- input$year

    div(class = "summary-bar",
      div(
        div(class = "total-label", paste("World Total \u2014", year_label)),
        div(
          span(class = "total-value", total_fmt),
          span(class = "total-unit", " MtCO\u2082e")
        )
      ),
      div(class = "summary-divider"),
      div(
        div(class = "total-label", "Gas"),
        div(style = "font-weight:600; font-size:13px;", gas_label)
      ),
      div(class = "summary-divider"),
      div(
        div(class = "total-label", "Countries"),
        div(style = "font-weight:600; font-size:13px;", n_countries)
      ),
      if (length(rv$selected_countries) > 0) {
        tagList(
          div(class = "summary-divider"),
          div(
            div(class = "total-label", "Selected"),
            div(style = "font-weight:600; font-size:13px;",
                paste(rv$selected_countries, collapse = ", "))
          )
        )
      }
    )
  })

  # ── Map ──
  output$map_out <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = 10, lat = 20, zoom = 2)
  })

  # Update map choropleth
  observe({
    req(rv$map_data, rv$world_sf)

    world <- rv$world_sf
    map_d <- rv$map_data

    # Join on ISO_A3
    world_joined <- world |>
      left_join(map_d, by = c("ADM0_A3" = "iso3"))

    pal <- colorNumeric(
      palette = c("#d6e4f7", "#1a6bb5", "#0d2d54"),
      domain  = world_joined$total,
      na.color = "#e8eaed"
    )

    # Highlight selected
    selected <- rv$selected_countries
    world_joined$is_selected <- world_joined$NAME %in% selected

    leafletProxy("map_out") |>
      clearShapes() |>
      clearControls() |>
      addPolygons(
        data        = world_joined,
        fillColor   = ~pal(total),
        fillOpacity = 0.8,
        color       = ~ifelse(is_selected, "#c8a951", "#ffffff"),
        weight      = ~ifelse(is_selected, 2.5, 0.5),
        opacity     = 1,
        layerId     = ~NAME,
        label       = ~lapply(paste0(
          "<strong>", NAME, "</strong><br/>",
          ifelse(is.na(total), "No data",
                 paste0(format(round(total, 1), big.mark = ","), " MtCO\u2082e"))
        ), HTML),
        labelOptions = labelOptions(
          style     = list("font-family" = "Source Sans 3, sans-serif",
                           "font-size"   = "12px",
                           "padding"     = "6px 10px"),
          direction = "auto"
        ),
        highlightOptions = highlightOptions(
          weight      = 2,
          color       = "#c8a951",
          fillOpacity = 0.95,
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        pal      = pal,
        values   = world_joined$total,
        title    = "MtCO\u2082e",
        position = "bottomleft",
        opacity  = 0.8,
        labFormat = labelFormat(big.mark = ",")
      )
  })

  # Handle map clicks (with shift = multi-select up to 4)
  observeEvent(input$map_out_shape_click, {
    click <- input$map_out_shape_click
    cname <- click$id
    if (is.null(cname) || cname == "") return()

    # Only accept countries in our dataset
    valid <- emissions_data |> distinct(country) |> pull(country)
    if (!(cname %in% valid)) return()

    current <- rv$selected_countries

    if (cname %in% current) {
      # Deselect
      rv$selected_countries <- setdiff(current, cname)
    } else {
      if (length(current) < 4) {
        rv$selected_countries <- c(current, cname)
      } else {
        showNotification("Maximum 4 countries selected.", type = "warning", duration = 2)
      }
    }
  })

  # ── Pie chart ──
  output$pie_out <- renderPlotly({
    req(rv$pie_data)
    df <- rv$pie_data

    plot_ly(df,
      labels  = ~sector,
      values  = ~total,
      type    = "pie",
      marker  = list(
        colors = unname(sector_colors[df$sector]),
        line   = list(color = "#ffffff", width = 1.5)
      ),
      textinfo      = "percent",
      hovertemplate = "<b>%{label}</b><br/>%{value:,.1f} MtCO\u2082e<br/>%{percent}<extra></extra>",
      textfont      = list(family = "Source Sans 3", size = 12, color = "#ffffff"),
      insidetextorientation = "radial"
    ) |>
      layout(
        showlegend = TRUE,
        legend = list(
          orientation = "h",
          x = 0, y = -0.1,
          font = list(family = "Source Sans 3", size = 11, color = "#1a2332")
        ),
        margin = list(t = 10, b = 50, l = 10, r = 10),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)"
      ) |>
      config(displayModeBar = FALSE)
  })

  # Pie subtitle
  output$pie_subtitle <- renderUI({
    if (length(rv$selected_countries) == 0) {
      tags$span("World total \u2014 hover to explore")
    } else {
      tags$span(paste(rv$selected_countries, collapse = " + "))
    }
  })

  # Selected country chips
  output$selected_chips <- renderUI({
    if (length(rv$selected_countries) == 0) {
      div(style = "color:#aab0c4; font-style:italic; font-size:11px;",
          "None \u2014 click map to select")
    } else {
      tagList(
        lapply(rv$selected_countries, function(c) {
          span(class = "country-chip", c)
        })
      )
    }
  })
}

# ── Run ───────────────────────────────────────────────────────────────────────
shinyApp(ui, server)
