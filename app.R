library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(tidyr)
library(sf)

# ── Data ──────────────────────────────────────────────────────────────────────

non_co2_data <- readr::read_csv("non_co2_data.csv", show_col_types = FALSE)

# The 'region' column is actually ISO3 — rename for clarity
non_co2_data <- non_co2_data |>
  rename(iso3 = region)

# ── Sector / subsector structure ──────────────────────────────────────────────

sector_meta <- list(
  Agriculture = list(
    col      = "agriculture",
    color    = "#8BC34A",
    subsectors = list(
      "Livestock"              = "livestock",
      "Rice Cultivation"       = "rice_cultivation",
      "Soil & Cropland"        = "soil_cropland",
      "Other Ag CH4 & N2O"     = "other_ag_ch4_and_n2o"
    )
  ),
  Energy = list(
    col      = "energy",
    color    = "#FFC107",
    subsectors = list(
      "Biomass"                = "biomass",
      "Coal Mining"            = "coal_mining",
      "Oil & Natural Gas"      = "oil_and_natural_gas_systems",
      "Stationary & Mobile"    = "stationary_and_mobile",
      "Other Energy"           = "other_energy"
    )
  ),
  `Industrial Processes` = list(
    col      = "industrial_processes",
    color    = "#1565C0",
    subsectors = list(
      "Refrigeration & AC"     = "refrigeration_and_air_conditioning",
      "Electronic Power Sys."  = "electronic_power_systems",
      "Nitric & Adipic Acid"   = "nitric_and_adipic_acid_production",
      "HCFC-22 Production"     = "hcfc_22_production",
      "Aluminum Production"    = "aluminum_production",
      "Magnesium Production"   = "magnesium_production",
      "Semiconductor Mfg."     = "semiconductor_manufacturing",
      "Foams Mfg. & Disposal"  = "foams_mfg_use_and_disposal",
      "Fire Protection"        = "fire_protection",
      "Aerosols"               = "aerosols",
      "Solvent Use"            = "solvent_use",
      "Other IP CH4 & N2O"     = "other_ip_ch4_and_n2o"
    )
  ),
  Waste = list(
    col      = "waste",
    color    = "#8D6E63",
    subsectors = list(
      "Landfills"              = "landfills",
      "Wastewater"             = "wastewater",
      "Other Waste CH4 & N2O"  = "other_waste_ch4_and_n2o"
    )
  )
)

sector_names  <- names(sector_meta)
sector_cols   <- sapply(sector_meta, `[[`, "col")
sector_colors <- sapply(sector_meta, `[[`, "color")

# World GeoJSON
world_geojson_url <-
  "https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_110m_admin_0_countries.geojson"

# ── Helpers ───────────────────────────────────────────────────────────────────

# Top-level sector pie (optionally filtered to selected countries)
get_sector_pie <- function(selected_countries = NULL) {
  df <- non_co2_data
  if (!is.null(selected_countries) && length(selected_countries) > 0)
    df <- df |> filter(country %in% selected_countries)

  tibble(
    sector = sector_names,
    total  = sapply(sector_cols, function(col) sum(df[[col]], na.rm = TRUE))
  )
}

# Subsector pie for a single clicked sector
get_subsector_pie <- function(sector_name, selected_countries = NULL) {
  df <- non_co2_data
  if (!is.null(selected_countries) && length(selected_countries) > 0)
    df <- df |> filter(country %in% selected_countries)

  subsectors <- sector_meta[[sector_name]]$subsectors
  tibble(
    subsector = names(subsectors),
    total     = sapply(subsectors, function(col) sum(df[[col]], na.rm = TRUE))
  ) |> filter(total > 0)
}

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Source+Sans+3:wght@300;400;600;700&family=Source+Serif+4:wght@400;600&display=swap"
    ),
    tags$style(HTML("
      * { box-sizing: border-box; margin: 0; padding: 0; }
      body {
        font-family: 'Source Sans 3', sans-serif;
        background: #f4f6f9; color: #1a2332; font-size: 14px;
      }

      .app-banner {
        background: #1a3a5c; color: white;
        padding: 10px 24px; display: flex; align-items: center; gap: 16px;
        border-bottom: 3px solid #c8a951;
      }
      .app-banner .ape-seal {
        width: 52px; height: 52px; background: white; border-radius: 50%;
        display: flex; align-items: center; justify-content: center;
        font-weight: 700; font-size: 13px; color: #1a3a5c;
        flex-shrink: 0; letter-spacing: -0.5px;
      }
      .app-banner-text h1 {
        font-family: 'Source Serif 4', serif; font-size: 18px;
        font-weight: 600; line-height: 1.2;
      }
      .app-banner-text p { font-size: 12px; opacity: 0.8; margin-top: 2px; }

      .app-body { display: flex; gap: 0; min-height: calc(100vh - 74px); }

      .sidebar-panel {
        width: 240px; flex-shrink: 0; background: #ffffff;
        border-right: 1px solid #dde3ec; padding: 20px 16px;
        display: flex; flex-direction: column; gap: 14px;
      }
      .sidebar-section-label {
        font-size: 11px; font-weight: 700; text-transform: uppercase;
        letter-spacing: 0.08em; color: #6b7a99; margin-bottom: 4px;
      }
      .sidebar-panel label {
        font-weight: 600; font-size: 13px; color: #1a2332;
        margin-bottom: 4px; display: block;
      }
      .sidebar-panel .form-control {
        border: 1px solid #c8d0de; border-radius: 4px; font-size: 13px;
        color: #1a2332; background: #f9fafb; height: 36px;
        padding: 0 10px; width: 100%; appearance: none;
        background-image: url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='12' height='12' viewBox='0 0 12 12'%3E%3Cpath fill='%236b7a99' d='M6 8L1 3h10z'/%3E%3C/svg%3E\");
        background-repeat: no-repeat; background-position: right 10px center;
        cursor: pointer; transition: border-color 0.15s;
      }
      .sidebar-panel .form-control:focus { border-color: #1a6bb5; outline: none; background: #fff; }
      .sidebar-panel .form-group { margin-bottom: 0; }
      .sidebar-divider { border: none; border-top: 1px solid #eaecf2; margin: 2px 0; }

      .selected-countries { font-size: 12px; color: #444f6b; line-height: 1.6; }
      .selected-countries strong {
        display: block; font-size: 11px; font-weight: 700;
        text-transform: uppercase; letter-spacing: 0.07em;
        color: #6b7a99; margin-bottom: 4px;
      }
      .country-chip {
        display: inline-block; background: #e8f0fb; color: #1a3a5c;
        border-radius: 12px; padding: 2px 8px; margin: 2px 2px 2px 0;
        font-size: 11px; font-weight: 600;
      }
      .clear-btn {
        background: none; border: none; color: #c0392b; font-size: 11px;
        font-weight: 600; cursor: pointer; padding: 0;
        text-decoration: underline; margin-top: 4px; display: block;
      }

      .main-panel {
        flex: 1; padding: 16px; display: flex;
        flex-direction: column; gap: 14px; min-width: 0;
      }

      .summary-bar {
        background: #1a3a5c; color: white; border-radius: 6px;
        padding: 10px 18px; display: flex; align-items: center;
        gap: 24px; font-size: 13px;
      }
      .summary-bar .total-label { opacity: 0.75; }
      .summary-bar .total-value {
        font-family: 'Source Serif 4', serif; font-size: 22px;
        font-weight: 600; color: #c8a951;
      }
      .summary-bar .total-unit { font-size: 11px; opacity: 0.65; }
      .summary-divider { width: 1px; height: 36px; background: rgba(255,255,255,0.2); }

      .viz-row { display: flex; gap: 14px; flex: 1; min-height: 500px; }
      .viz-card {
        background: white; border-radius: 6px; border: 1px solid #dde3ec;
        display: flex; flex-direction: column; overflow: hidden;
      }
      .viz-card-header {
        padding: 10px 14px 8px; border-bottom: 1px solid #eaecf2;
        display: flex; align-items: baseline; justify-content: space-between;
      }
      .viz-card-header h2 {
        font-family: 'Source Serif 4', serif; font-size: 14px;
        font-weight: 600; color: #1a2332;
      }
      .viz-card-header span { font-size: 11px; color: #6b7a99; }
      .map-card { flex: 3; }
      .pie-card { flex: 2; }

      .drilldown-breadcrumb {
        display: flex; align-items: center; gap: 6px;
        font-size: 12px; color: #6b7a99;
      }
      .back-btn {
        background: none; border: 1px solid #c8d0de; border-radius: 3px;
        color: #1a6bb5; font-size: 11px; font-weight: 600;
        padding: 2px 7px; cursor: pointer; height: auto;
        line-height: 1.4;
      }
      .back-btn:hover { background: #e8f0fb; }

      .app-footer {
        background: #1a3a5c; color: rgba(255,255,255,0.6);
        font-size: 11px; padding: 8px 24px; text-align: center;
      }

      #map_out { flex: 1; }
      #pie_out { flex: 1; }
      .viz-card > .shiny-bound-output { flex: 1; }
    "))
  ),

  div(class = "app-banner",
    div(class = "ape-seal", "   "),
    div(class = "app-banner-text",
      tags$h1("Global Non-CO\u2082 GHG Emissions Viewer"),
      tags$p("Source: U.S. EPA \u2022 GWPs: IPCC AR5 (2014) \u2022 Units: MtCO\u2082e \u2022 For UI testing purposes only. Not affiliated with EPA or any U.S. government agency.")
    )
  ),

  div(class = "app-body",

    div(class = "sidebar-panel",
      div(class = "sidebar-section-label", "Filters"),

      div(class = "form-group",
        tags$label("Sector"),
        selectInput("sector_filter", NULL,
          choices  = c("All Sectors", sector_names),
          selected = "All Sectors", width = "100%")
      ),

      tags$hr(class = "sidebar-divider"),
      div(class = "sidebar-section-label", "Map Selection"),
      div(class = "selected-countries",
        uiOutput("selected_chips"),
        actionButton("clear_sel", "Clear selection", class = "clear-btn")
      ),

      tags$hr(class = "sidebar-divider"),
      div(class = "sidebar-section-label", "Instructions"),
      div(style = "font-size:11px; color:#6b7a99; line-height:1.7;",
        tags$b("Map:"), " Click a country to select it.",
        " Click again to deselect. Up to 4 countries.", tags$br(), tags$br(),
        tags$b("Pie:"), " Click a sector slice to drill into subsectors.",
        " Use \u2018\u2190 Back\u2019 to return to sectors."
      )
    ),

    div(class = "main-panel",

      uiOutput("summary_bar"),

      div(class = "viz-row",
        div(class = "viz-card map-card",
          div(class = "viz-card-header",
            tags$h2("Non-CO\u2082 Emissions by Country"),
            tags$span("Click a country to filter the pie chart")
          ),
          leafletOutput("map_out", height = "100%")
        ),

        div(class = "viz-card pie-card",
          div(class = "viz-card-header",
            tags$h2("Sector Breakdown"),
            uiOutput("pie_header_right")
          ),
          plotlyOutput("pie_out", height = "100%")
        )
      )
    )
  ),

  div(class = "app-footer",
    "For UI testing purposes only. Not affiliated with EPA or any U.S. government agency."
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  rv <- reactiveValues(
    selected_countries = character(0),
    world_sf           = NULL,
    drilldown_sector   = NULL
  )

  # Load geometry once
  observe({
    withProgress(message = "Loading map geometry...", {
      tryCatch({
        rv$world_sf <- sf::read_sf(world_geojson_url)
      }, error = function(e) {
        showNotification("Could not load map geometry.", type = "error")
      })
    })
  })

  # Map display data — re-total by selected sector if filtered
  map_totals <- reactive({
    df <- non_co2_data
    if (input$sector_filter != "All Sectors") {
      col <- sector_meta[[input$sector_filter]]$col
      df  <- df |> mutate(total_emissions = .data[[col]])
    }
    df |> select(iso3, country, total_emissions)
  })

  # Joined world sf
  map_joined <- reactive({
    req(rv$world_sf)
    rv$world_sf |>
      left_join(map_totals(), by = c("ADM0_A3" = "iso3")) |>
      mutate(is_selected = NAME %in% rv$selected_countries)
  })

  # ── Base map ──
  output$map_out <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = 10, lat = 20, zoom = 2)
  })

  # ── Choropleth update ──
  observe({
    req(map_joined())
    mj <- map_joined()

    pal <- colorNumeric(
      palette  = c("#d6e4f7", "#1a6bb5", "#0d2d54"),
      domain   = mj$total_emissions,
      na.color = "#e8eaed"
    )

    leafletProxy("map_out") |>
      clearShapes() |> clearControls() |>
      addPolygons(
        data        = mj,
        fillColor   = ~pal(total_emissions),
        fillOpacity = 0.8,
        color       = ~ifelse(is_selected, "#c8a951", "#ffffff"),
        weight      = ~ifelse(is_selected, 2.5, 0.5),
        opacity     = 1,
        layerId     = ~NAME,
        label       = ~lapply(paste0(
          "<strong>", NAME, "</strong><br/>",
          ifelse(is.na(total_emissions), "No data",
                 paste0(format(round(total_emissions, 1), big.mark = ","),
                        " MtCO\u2082e"))
        ), HTML),
        labelOptions = labelOptions(
          style = list("font-family" = "Source Sans 3, sans-serif",
                       "font-size" = "12px", "padding" = "6px 10px"),
          direction = "auto"
        ),
        highlightOptions = highlightOptions(
          weight = 2, color = "#c8a951",
          fillOpacity = 0.95, bringToFront = TRUE
        )
      ) |>
      addLegend(
        pal = pal, values = mj$total_emissions,
        title = "MtCO\u2082e", position = "bottomleft",
        opacity = 0.8, labFormat = labelFormat(big.mark = ",")
      )
  })

  # ── Map click ──
  observeEvent(input$map_out_shape_click, {
    cname <- input$map_out_shape_click$id
    if (is.null(cname) || cname == "") return()
    if (!(cname %in% non_co2_data$country)) return()

    current <- rv$selected_countries
    if (cname %in% current) {
      rv$selected_countries <- setdiff(current, cname)
    } else if (length(current) < 4) {
      rv$selected_countries <- c(current, cname)
    } else {
      showNotification("Maximum 4 countries selected.", type = "warning", duration = 2)
    }
    rv$drilldown_sector <- NULL
  })

  observeEvent(input$clear_sel, {
    rv$selected_countries <- character(0)
    rv$drilldown_sector   <- NULL
  })

  # ── Pie data ──
  pie_df <- reactive({
    sel <- if (length(rv$selected_countries) > 0) rv$selected_countries else NULL

    if (is.null(rv$drilldown_sector)) {
      get_sector_pie(sel) |>
        rename(label = sector, value = total) |>
        mutate(color = sector_colors[label])
    } else {
      sname <- rv$drilldown_sector
      base_color <- sector_meta[[sname]]$color
      df <- get_subsector_pie(sname, sel)
      n  <- nrow(df)
      df |>
        rename(label = subsector, value = total) |>
        mutate(color = colorRampPalette(c(base_color, "#aacce8"))(n))
    }
  })

  # ── Pie chart ──
  output$pie_out <- renderPlotly({
    df     <- pie_df()
    is_top <- is.null(rv$drilldown_sector)

    p <- plot_ly(df,
      labels  = ~label, values = ~value, type = "pie",
      source  = "pie_click",
      marker  = list(colors = ~color,
                     line   = list(color = "#ffffff", width = 1.5)),
      textinfo      = "percent",
      hovertemplate = "<b>%{label}</b><br/>%{value:,.1f} MtCO\u2082e<br/>%{percent}<extra></extra>",
      textfont      = list(family = "Source Sans 3", size = 12, color = "#ffffff"),
      insidetextorientation = "radial",
      customdata    = ~label
    ) |>
      layout(
        showlegend = TRUE,
        legend = list(
          orientation = "h", x = 0, y = -0.12,
          font = list(family = "Source Sans 3", size = 11, color = "#1a2332")
        ),
        margin        = list(t = 10, b = 60, l = 10, r = 10),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        annotations   = if (is_top) list(list(
          text = "Click a slice to drill into subsectors",
          x = 0.5, y = -0.22, xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 10, color = "#6b7a99", family = "Source Sans 3")
        )) else list()
      ) |>
      config(displayModeBar = FALSE)

    p
  })

  # ── Pie click → drilldown ──
  observeEvent(event_data("plotly_click", source = "pie_click"), {
    click <- event_data("plotly_click", source = "pie_click")
    if (is.null(click)) return()
    clicked_label <- click$customdata[[1]]
    if (is.null(rv$drilldown_sector) && clicked_label %in% sector_names) {
      rv$drilldown_sector <- clicked_label
    }
  })

  observeEvent(input$pie_back, {
    rv$drilldown_sector <- NULL
  })

  # ── Pie header right ──
  output$pie_header_right <- renderUI({
    sel_label <- if (length(rv$selected_countries) == 0) "World total" else
      paste(rv$selected_countries, collapse = " + ")

    if (is.null(rv$drilldown_sector)) {
      div(class = "drilldown-breadcrumb", tags$span(sel_label))
    } else {
      div(class = "drilldown-breadcrumb",
        actionButton("pie_back", "\u2190 Back", class = "back-btn"),
        tags$span(paste0(sel_label, " \u203a ", rv$drilldown_sector))
      )
    }
  })

  # ── Summary bar ──
  output$summary_bar <- renderUI({
    df  <- map_totals()
    sel <- rv$selected_countries

    if (length(sel) > 0) {
      total <- sum(df[df$country %in% sel, "total_emissions"], na.rm = TRUE)
      scope <- paste(sel, collapse = " + ")
    } else {
      total <- sum(df$total_emissions, na.rm = TRUE)
      scope <- "World Total"
    }

    div(class = "summary-bar",
      div(
        div(class = "total-label", scope),
        div(span(class = "total-value", format(round(total, 1), big.mark = ",")),
            span(class = "total-unit", " MtCO\u2082e"))
      ),
      div(class = "summary-divider"),
      div(
        div(class = "total-label", "Sector Filter"),
        div(style = "font-weight:600; font-size:13px;", input$sector_filter)
      ),
      div(class = "summary-divider"),
      div(
        div(class = "total-label", "Countries in Dataset"),
        div(style = "font-weight:600; font-size:13px;", nrow(non_co2_data))
      )
    )
  })

  # ── Country chips ──
  output$selected_chips <- renderUI({
    if (length(rv$selected_countries) == 0) {
      div(style = "color:#aab0c4; font-style:italic; font-size:11px;",
          "None \u2014 click map to select")
    } else {
      tagList(lapply(rv$selected_countries, function(c) span(class = "country-chip", c)))
    }
  })
}

shinyApp(ui, server)
