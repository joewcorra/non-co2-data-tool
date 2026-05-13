# ============================================================================== 
# Global Non-CO2 GHG Emissions Viewer
# ==============================================================================
# DEVELOPER NOTES — WIRING IN FULL DATASET
#
# When the full dataset arrives, three stubs need to be replaced:
#
# 1. GREENHOUSE GAS filter
# Current: stub column `greenhouse_gas = "All Gases"` on every row
# Replace: real `greenhouse_gas` column; values like "Methane", "Nitrous Oxide",
# "Fluorinated Gases"
# Location: filter_data() reactive, labelled ## GAS FILTER STUB ##
#
# 2. YEAR filter
# Current: stub column `year = 2025` on every row
# Replace: real `year` column; values 1990, 1995, 2000 … 2080 (5-yr increments)
# Location: filter_data() reactive, labelled ## YEAR FILTER STUB ##
#
# 3. REGION filter (country groupings)
# Current: stub list `region_groups` with placeholder ISO3 vectors
# Replace: real mapping table; supply a named list or lookup tibble of
# region_name -> character vector of iso3 codes
# Location: top of script, labelled ## REGION GROUPS STUB ##
#
# Everything downstream (map, pie, table, summary bar) reads from filter_data()
# so no other changes should be needed.
# ==============================================================================

library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(tidyr)
library(sf)
library(DT)

# ── Data ──────────────────────────────────────────────────────────────────────

non_co2_data <- readr::read_csv("non_co2_data.csv", show_col_types = FALSE) |>
  rename(iso3 = region)

# ── Stub columns (remove when full data arrives) ───────────────────────────────

## YEAR FILTER STUB — replace with real `year` column
non_co2_data <- non_co2_data |> mutate(year = 2025L)

## GAS FILTER STUB — replace with real `greenhouse_gas` column
non_co2_data <- non_co2_data |> mutate(greenhouse_gas = "All Gases")

# ── Region groups ──────────────────────────────────────────────────────────────
## REGION GROUPS STUB — replace iso3 vectors with real membership lists

region_groups <- list(
  "European Union" = c(
    "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA",
    "DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD",
    "POL","PRT","ROU","SVK","SVN","ESP","SWE"
  ),
  "OPEC" = c(
    "DZA","AGO","COG","GNQ","GAB","IRN","IRQ","KWT","LBY",
    "NGA","SAU","ARE","VEN"
  ),
  "G7" = c("CAN","FRA","DEU","ITA","JPN","GBR","USA"),
  "G20" = c(
    "ARG","AUS","BRA","CAN","CHN","FRA","DEU","IND","IDN","ITA",
    "JPN","MEX","RUS","SAU","ZAF","KOR","TUR","GBR","USA"
  ),
  "BRICS" = c("BRA","RUS","IND","CHN","ZAF")
)

# ── Sector / subsector metadata ───────────────────────────────────────────────

sector_meta <- list(
  Agriculture = list(
    col = "agriculture",
    color = "#8BC34A",
    subsectors = list(
      "Livestock" = "livestock",
      "Rice Cultivation" = "rice_cultivation",
      "Soil & Cropland" = "soil_cropland",
      "Other Ag CH4 & N2O" = "other_ag_ch4_and_n2o"
    )
  ),
  Energy = list(
    col = "energy",
    color = "#FFC107",
    subsectors = list(
      "Biomass" = "biomass",
      "Coal Mining" = "coal_mining",
      "Oil & Natural Gas" = "oil_and_natural_gas_systems",
      "Stationary & Mobile" = "stationary_and_mobile",
      "Other Energy" = "other_energy"
    )
  ),
  `Industrial Processes` = list(
    col = "industrial_processes",
    color = "#1565C0",
    subsectors = list(
      "Refrigeration & AC" = "refrigeration_and_air_conditioning",
      "Electronic Power Sys." = "electronic_power_systems",
      "Nitric & Adipic Acid" = "nitric_and_adipic_acid_production",
      "HCFC-22 Production" = "hcfc_22_production",
      "Aluminum Production" = "aluminum_production",
      "Magnesium Production" = "magnesium_production",
      "Semiconductor Mfg." = "semiconductor_manufacturing",
      "Foams Mfg. & Disposal" = "foams_mfg_use_and_disposal",
      "Fire Protection" = "fire_protection",
      "Aerosols" = "aerosols",
      "Solvent Use" = "solvent_use",
      "Other IP CH4 & N2O" = "other_ip_ch4_and_n2o"
    )
  ),
  Waste = list(
    col = "waste",
    color = "#8D6E63",
    subsectors = list(
      "Landfills" = "landfills",
      "Wastewater" = "wastewater",
      "Other Waste CH4 & N2O" = "other_waste_ch4_and_n2o"
    )
  )
)

sector_names <- names(sector_meta)
sector_cols <- sapply(sector_meta, `[[`, "col")
sector_colors <- sapply(sector_meta, `[[`, "color")

# ── Filter choices (derived from data + stubs) ────────────────────────────────

year_choices <- sort(unique(non_co2_data$year), decreasing = TRUE)
gas_choices <- c("All Gases", sort(setdiff(unique(non_co2_data$greenhouse_gas), "All Gases")))

# ── GeoJSON source ────────────────────────────────────────────────────────────

world_geojson_url <-
  "https://raw.githubusercontent.com/nvkelso/natural-earth-vector/master/geojson/ne_110m_admin_0_countries.geojson"

# ── Helper functions ──────────────────────────────────────────────────────────

# Core filter: gas + year + region grouping
# Returns a country-level tibble with total_emissions and sector columns intact
apply_filters <- function(gas, year, region_group, sector) {
  df <- non_co2_data
  
  ## GAS FILTER STUB — swap condition when real gas column exists
  if (gas != "All Gases") df <- df |> filter(greenhouse_gas == gas)
  
  ## YEAR FILTER STUB — swap condition when real year column exists
  df <- df |> filter(year == !!year)
  
  # Region grouping: restrict to member iso3s
  if (region_group != "All Regions") {
    member_iso3 <- region_groups[[region_group]]
    df <- df |> filter(iso3 %in% member_iso3)
  }
  
  # Re-total by sector if sector filter applied
  if (sector != "All Sectors") {
    col <- sector_meta[[sector]]$col
    df <- df |> mutate(total_emissions = .data[[col]])
  }
  
  df
}

get_sector_pie <- function(df, selected_countries = NULL) {
  if (!is.null(selected_countries) && length(selected_countries) > 0)
    df <- df |> filter(country %in% selected_countries)
  tibble(
    sector = sector_names,
    total = sapply(sector_cols, function(col) sum(df[[col]], na.rm = TRUE))
  )
}

get_subsector_pie <- function(df, sector_name, selected_countries = NULL) {
  if (!is.null(selected_countries) && length(selected_countries) > 0)
    df <- df |> filter(country %in% selected_countries)
  subsectors <- sector_meta[[sector_name]]$subsectors
  tibble(
    subsector = names(subsectors),
    total = sapply(subsectors, function(col) sum(df[[col]], na.rm = TRUE))
  ) |> filter(total > 0)
}

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Source+Sans+3:wght@300;400;600;700&family=Source+Serif+4:wght@400;600&display=swap"
    ),
    tags$style(HTML("
      * { box-sizing: border-box; margin: 0; padding: 0; }
      body {
        font-family: 'Source Sans 3', sans-serif;
        background: #f4f6f9; color: #1a2332; font-size: 14px;
      }

      /* ── Banner ── */
      .app-banner {
        background: #1a3a5c; color: white; padding: 10px 24px;
        display: flex; align-items: center; gap: 16px;
        border-bottom: 3px solid #c8a951;
      }
      .app-banner-text h1 {
        font-family: 'Source Serif 4', serif; font-size: 18px;
        font-weight: 600; line-height: 1.2;
      }
      .app-banner-text p { font-size: 12px; opacity: 0.8; margin-top: 2px; }

      /* ── Layout ── */
      .app-body { display: flex; gap: 0; min-height: calc(100vh - 74px); }

      /* ── Sidebar ── */
      .sidebar-panel {
        width: 240px; flex-shrink: 0; background: #ffffff;
        border-right: 1px solid #dde3ec; padding: 20px 16px;
        display: flex; flex-direction: column; gap: 14px;
        overflow-y: auto;
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

      /* stub badge on disabled filters */
      .stub-badge {
        display: inline-block; background: #fff3cd; color: #856404;
        border: 1px solid #ffc107; border-radius: 3px;
        font-size: 10px; font-weight: 700; padding: 1px 5px;
        vertical-align: middle; margin-left: 4px; letter-spacing: 0.03em;
      }

      /* ── Country chips ── */
      .selected-countries { font-size: 12px; color: #444f6b; line-height: 1.6; }
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

      /* ── Main panel ── */
      .main-panel {
        flex: 1; padding: 16px; display: flex;
        flex-direction: column; gap: 14px; min-width: 0;
      }

      /* ── Summary bar ── */
      .summary-bar {
        background: #1a3a5c; color: white; border-radius: 6px;
        padding: 10px 18px; display: flex; align-items: center;
        gap: 20px; font-size: 13px; flex-wrap: wrap;
      }
      .summary-bar .total-label { opacity: 0.75; font-size: 11px; }
      .summary-bar .total-value {
        font-family: 'Source Serif 4', serif; font-size: 22px;
        font-weight: 600; color: #c8a951;
      }
      .summary-bar .total-unit { font-size: 11px; opacity: 0.65; }
      .summary-divider { width: 1px; height: 36px; background: rgba(255,255,255,0.2); }
      .summary-item { font-weight: 600; font-size: 13px; }

      /* ── Viz cards ── */
      .viz-row { display: flex; gap: 14px; min-height: 500px; }
      .viz-card {
        background: white; border-radius: 6px; border: 1px solid #dde3ec;
        display: flex; flex-direction: column; overflow: hidden;
      }
      .viz-card-header {
        padding: 10px 14px 8px; border-bottom: 1px solid #eaecf2;
        display: flex; align-items: center; justify-content: space-between;
        gap: 8px;
      }
      .viz-card-header h2 {
        font-family: 'Source Serif 4', serif; font-size: 14px;
        font-weight: 600; color: #1a2332; white-space: nowrap;
      }
      .viz-card-header span { font-size: 11px; color: #6b7a99; }
      .map-card { flex: 3; }
      .pie-card { flex: 2; }

      /* ── Drilldown breadcrumb ── */
      .drilldown-breadcrumb {
        display: flex; align-items: center; gap: 6px;
        font-size: 11px; color: #6b7a99;
      }
      .back-btn {
        background: none; border: 1px solid #c8d0de; border-radius: 3px;
        color: #1a6bb5; font-size: 11px; font-weight: 600;
        padding: 2px 7px; cursor: pointer; height: auto; line-height: 1.4;
      }
      .back-btn:hover { background: #e8f0fb; }

      /* ── Data table card ── */
      .table-card { margin-top: 0; }
      .table-card .viz-card-header { cursor: pointer; user-select: none; }
      .table-card .viz-card-header:hover { background: #f9fafb; }
      .collapse-icon { font-size: 13px; color: #6b7a99; transition: transform 0.2s; }
      .collapse-icon.open { transform: rotate(180deg); }

      /* DT overrides */
      .dataTables_wrapper { padding: 12px 14px 14px; }
      .dataTables_filter input {
        border: 1px solid #c8d0de; border-radius: 4px;
        font-size: 12px; padding: 3px 8px; font-family: 'Source Sans 3', sans-serif;
      }
      table.dataTable thead th {
        font-size: 12px; font-weight: 700; color: #1a2332;
        background: #f4f6f9; border-bottom: 2px solid #dde3ec;
        font-family: 'Source Sans 3', sans-serif;
      }
      table.dataTable tbody td {
        font-size: 12px; font-family: 'Source Sans 3', sans-serif;
        color: #1a2332;
      }
      table.dataTable tbody tr:hover { background: #f0f5fb !important; }
      .dt-buttons .btn {
        background: #1a6bb5; color: white; border: none; border-radius: 4px;
        font-size: 11px; font-weight: 600; padding: 4px 10px;
        font-family: 'Source Sans 3', sans-serif; cursor: pointer;
      }
      .dt-buttons .btn:hover { background: #145a9a; }

      /* ── Footer ── */
      .app-footer {
        background: #1a3a5c; color: rgba(255,255,255,0.6);
        font-size: 11px; padding: 8px 24px; text-align: center;
      }

      /* Leaflet/plotly fill */
      #map_out { flex: 1; min-height: 0; }
      #pie_out { flex: 1; min-height: 0; }
      .viz-card > .shiny-bound-output { flex: 1; min-height: 0; }
    "))
  ),
  
  # ── Banner ──
  div(class = "app-banner",
      div(class = "app-banner-text",
          tags$h1("Global Non-CO\u2082 GHG Emissions Viewer"),
          tags$p("Source: U.S. EPA \u2022 GWPs: IPCC AR5 (2014) \u2022 Units: MtCO\u2082e \u2022
              Not an official EPA product")
      )
  ),
  
  div(class = "app-body",
      
      # ── Sidebar ──
      div(class = "sidebar-panel",
          
          div(class = "sidebar-section-label", "Filters"),
          
          # Greenhouse gas — stub flagged
          div(class = "form-group",
              tags$label(
                "Greenhouse Gas",
                tags$span(class = "stub-badge", "STUB")
              ),
              selectInput("gas_filter", NULL,
                          choices = gas_choices,
                          selected = "All Gases",
                          width = "100%")
          ),
          
          # Year — stub flagged
          div(class = "form-group",
              tags$label(
                "Year",
                tags$span(class = "stub-badge", "STUB")
              ),
              selectInput("year_filter", NULL,
                          choices = year_choices,
                          selected = max(year_choices),
                          width = "100%")
          ),
          
          # Region grouping — stub flagged
          div(class = "form-group",
              tags$label(
                "Region",
                tags$span(class = "stub-badge", "STUB")
              ),
              selectInput("region_filter", NULL,
                          choices = c("All Regions", names(region_groups)),
                          selected = "All Regions",
                          width = "100%")
          ),
          
          # Sector
          div(class = "form-group",
              tags$label("Sector"),
              selectInput("sector_filter", NULL,
                          choices = c("All Sectors", sector_names),
                          selected = "All Sectors",
                          width = "100%")
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
              tags$b("Map:"), " Click a country to filter the pie & table.",
              " Click again to deselect. Up to 4 countries.", tags$br(), tags$br(),
              tags$b("Pie:"), " Click a sector to drill into subsectors.",
              " Use \u2018\u2190 Back\u2019 to return.", tags$br(), tags$br(),
              tags$b("Table:"), " Reflects all active filters. Click column",
              " headers to sort. Use the Download button to export CSV."
          )
      ),
      
      # ── Main ──
      div(class = "main-panel",
          
          uiOutput("summary_bar"),
          
          # Viz row
          div(class = "viz-row",
              div(class = "viz-card map-card",
                  div(class = "viz-card-header",
                      tags$h2("Emissions by Country"),
                      tags$span("Click to select \u2022 darker = higher")
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
          ),
          
          # Data table (collapsible)
          div(class = "viz-card table-card",
              div(class = "viz-card-header",
                  id = "table_toggle",
                  tags$h2("Data Table"),
                  div(style = "display:flex; align-items:center; gap:8px;",
                      tags$span(id = "table_row_count", ""),
                      tags$span(class = "collapse-icon open", id = "collapse_icon", "\u25be")
                  )
              ),
              div(id = "table_body",
                  DTOutput("data_table")
              )
          )
      )
  ),
  
  div(class = "app-footer",
      "Data: U.S. Environmental Protection Agency \u2022 Not an official EPA product \u2022
     Not affiliated with the U.S. Government"
  ),
  
  # Collapse toggle JS
  tags$script(HTML("
    $(document).on('click', '#table_toggle', function() {
      $('#table_body').slideToggle(200);
      $('#collapse_icon').toggleClass('open');
    });
  "))
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    selected_countries = character(0),
    world_sf = NULL,
    drilldown_sector = NULL
  )
  
  # ── Load geometry once ──
  observe({
    withProgress(message = "Loading map geometry...", {
      tryCatch({
        rv$world_sf <- sf::read_sf(world_geojson_url)
      }, error = function(e) {
        showNotification("Could not load map geometry. Check internet connection.",
                         type = "error")
      })
    })
  })
  
  # ── Core filtered dataset ──
  # All downstream reactives read from here — swap stubs in apply_filters() only
  filter_data <- reactive({
    apply_filters(
      gas = input$gas_filter,
      year = as.integer(input$year_filter),
      region_group = input$region_filter,
      sector = input$sector_filter
    )
  })
  
  # ── Map totals ──
  map_totals <- reactive({
    filter_data() |> select(iso3, country, total_emissions)
  })
  
  # ── Joined world sf ──
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
      palette = c("#d6e4f7", "#1a6bb5", "#0d2d54"),
      domain = mj$total_emissions,
      na.color = "#e8eaed"
    )
    
    leafletProxy("map_out") |>
      clearShapes() |> clearControls() |>
      addPolygons(
        data = mj,
        fillColor = ~pal(total_emissions),
        fillOpacity = 0.8,
        color = ~ifelse(is_selected, "#c8a951", "#ffffff"),
        weight = ~ifelse(is_selected, 2.5, 0.5),
        opacity = 1,
        layerId = ~NAME,
        label = ~lapply(paste0(
          "<strong>", NAME, "</strong><br/>",
          ifelse(is.na(total_emissions), "No data",
                 paste0(format(round(total_emissions, 1), big.mark = ","), " MtCO\u2082e"))
        ), HTML),
        labelOptions = labelOptions(
          style = list("font-family" = "Source Sans 3, sans-serif",
                       "font-size" = "12px", "padding" = "6px 10px"),
          direction = "auto"
        ),
        highlightOptions = highlightOptions(
          weight = 2, color = "#c8a951", fillOpacity = 0.95, bringToFront = TRUE
        )
      ) |>
      addLegend(
        pal = pal, values = mj$total_emissions,
        title = "MtCO\u2082e", position = "bottomleft",
        opacity = 0.8, labFormat = labelFormat(big.mark = ",")
      )
  })
  
  # ── Map click → select / deselect ──
  observeEvent(input$map_out_shape_click, {
    cname <- input$map_out_shape_click$id
    if (is.null(cname) || cname == "") return()
    
    # Only accept countries present in the currently filtered data
    if (!(cname %in% filter_data()$country)) {
      showNotification(
        paste0(cname, " is not in the current region filter."),
        type = "warning", duration = 2
      )
      return()
    }
    
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
  
  # Clear filter changes should also reset map selection
  observeEvent(list(input$gas_filter, input$year_filter,
                    input$region_filter, input$sector_filter), {
                      rv$selected_countries <- character(0)
                      rv$drilldown_sector <- NULL
                    }, ignoreInit = TRUE)
  
  observeEvent(input$clear_sel, {
    rv$selected_countries <- character(0)
    rv$drilldown_sector <- NULL
  })
  
  # ── Pie data ──
  pie_df <- reactive({
    fd <- filter_data()
    sel <- if (length(rv$selected_countries) > 0) rv$selected_countries else NULL
    
    if (is.null(rv$drilldown_sector)) {
      get_sector_pie(fd, sel) |>
        rename(label = sector, value = total) |>
        mutate(color = sector_colors[label])
    } else {
      sname <- rv$drilldown_sector
      base_color <- sector_meta[[sname]]$color
      df <- get_subsector_pie(fd, sname, sel)
      n <- nrow(df)
      df |>
        rename(label = subsector, value = total) |>
        mutate(color = colorRampPalette(c(base_color, "#aacce8"))(n))
    }
  })
  
  # ── Pie chart ──
  output$pie_out <- renderPlotly({
    df <- pie_df()
    is_top <- is.null(rv$drilldown_sector)
    
    plot_ly(df,
            labels = ~label, values = ~value, type = "pie",
            source = "pie_click",
            marker = list(colors = ~color,
                          line = list(color = "#ffffff", width = 1.5)),
            textinfo = "percent",
            hovertemplate = "<b>%{label}</b><br/>%{value:,.1f} MtCO\u2082e<br/>%{percent}<extra></extra>",
            textfont = list(family = "Source Sans 3", size = 12, color = "#ffffff"),
            insidetextorientation = "radial",
            customdata = ~label
    ) |>
      layout(
        showlegend = TRUE,
        legend = list(
          orientation = "h", x = 0, y = -0.12,
          font = list(family = "Source Sans 3", size = 11, color = "#1a2332")
        ),
        margin = list(t = 10, b = 60, l = 10, r = 10),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        annotations = if (is_top) list(list(
          text = "Click a slice to drill into subsectors",
          x = 0.5, y = -0.22, xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 10, color = "#6b7a99", family = "Source Sans 3")
        )) else list()
      ) |>
      config(displayModeBar = FALSE)
  })
  
  # ── Pie drilldown ──
  observeEvent(event_data("plotly_click", source = "pie_click"), {
    click <- event_data("plotly_click", source = "pie_click")
    if (is.null(click)) return()
    clicked_label <- click$customdata[[1]]
    if (is.null(rv$drilldown_sector) && clicked_label %in% sector_names)
      rv$drilldown_sector <- clicked_label
  })
  
  observeEvent(input$pie_back, { rv$drilldown_sector <- NULL })
  
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
  
  # ── Data table ──
  # Presentable country-level table matching all active filters
  table_data <- reactive({
    fd <- filter_data()
    sel <- rv$selected_countries
    
    df <- if (length(sel) > 0) fd |> filter(country %in% sel) else fd
    
    # Select and rename display columns
    df |>
      select(
        Country = country,
        `Total (MtCO2e)` = total_emissions,
        Agriculture = agriculture,
        Energy = energy,
        `Industrial Proc.` = industrial_processes,
        Waste = waste
      ) |>
      arrange(desc(`Total (MtCO2e)`)) |>
      mutate(across(where(is.numeric), \(x) round(x, 1)))
  })
  
  output$data_table <- renderDT({
    datatable(
      table_data(),
      extensions = "Buttons",
      rownames = FALSE,
      options = list(
        dom = "Bfrtip",
        buttons = list(
          list(extend = "csv", text = "Download CSV",
               filename = paste0("non_co2_emissions_", input$year_filter)),
          list(extend = "excel", text = "Download Excel",
               filename = paste0("non_co2_emissions_", input$year_filter))
        ),
        pageLength = 15,
        order = list(list(1, "desc")),
        columnDefs = list(
          list(className = "dt-right", targets = 1:5)
        ),
        language = list(
          search = "Filter table:"
        )
      ),
      class = "stripe hover compact"
    ) |>
      formatRound(columns = 2:6, digits = 1) |>
      formatStyle(
        "Total (MtCO2e)",
        background = styleColorBar(range(table_data()$`Total (MtCO2e)`, na.rm = TRUE),
                                   "#d6e4f7"),
        backgroundSize = "100% 90%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })
  
  # ── Summary bar ──
  output$summary_bar <- renderUI({
    fd <- filter_data()
    sel <- rv$selected_countries
    
    if (length(sel) > 0) {
      total <- sum(fd[fd$country %in% sel, "total_emissions"], na.rm = TRUE)
      scope <- paste(sel, collapse = " + ")
    } else {
      total <- sum(fd$total_emissions, na.rm = TRUE)
      scope <- if (input$region_filter == "All Regions") "World Total" else input$region_filter
    }
    
    n_shown <- nrow(fd)
    
    div(class = "summary-bar",
        div(
          div(class = "total-label", scope),
          div(
            span(class = "total-value", format(round(total, 1), big.mark = ",")),
            span(class = "total-unit", " MtCO\u2082e")
          )
        ),
        div(class = "summary-divider"),
        div(
          div(class = "total-label", "Year"),
          div(class = "summary-item", input$year_filter)
        ),
        div(class = "summary-divider"),
        div(
          div(class = "total-label", "Gas"),
          div(class = "summary-item", input$gas_filter)
        ),
        div(class = "summary-divider"),
        div(
          div(class = "total-label", "Sector"),
          div(class = "summary-item", input$sector_filter)
        ),
        div(class = "summary-divider"),
        div(
          div(class = "total-label", "Countries Shown"),
          div(class = "summary-item", n_shown)
        )
    )
  })
  
  # ── Country chips ──
  output$selected_chips <- renderUI({
    if (length(rv$selected_countries) == 0) {
      div(style = "color:#aab0c4; font-style:italic; font-size:11px;",
          "None \u2014 click map to select")
    } else {
      tagList(lapply(rv$selected_countries,
                     function(c) span(class = "country-chip", c)))
    }
  })
}

shinyApp(ui, server)
