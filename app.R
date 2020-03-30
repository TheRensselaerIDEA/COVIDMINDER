#### Library and Data Imports ####
source("modules/Source.R")
source("modules/data_load.R")
source("modules/preprocessing.R")

#### UI Code ####
ui <- navbarPage(
  title="CovidMinder",
  tabPanel("COVID-19 Testing/State",
           fluidRow(
              column(3, HTML("Plot: Disparity Index</br>Total COVID-19 Testing/State</br>UPDATE: Using South Korea testing rate")),
              column(9, leafletOutput(outputId = "mymap", width="100%"))
            )
  )
)

#### Server Code ####
server <- function(input, output, session) {
  
  # Render leaflet plot with all information in hover
  output$mymap <- renderLeaflet({
    colors <- c("#ef8a62","#fddbc7","#f7f7f7","#d1e5f0","#67a9cf")
    bins <- c(-5, -2, -1, -.5, 0, .5, 1, 2, 3)
    pal2 <- leaflet::colorBin(colors, domain = states$tests_ldi, bins = bins, reverse=FALSE)
    labels2 <- sprintf(
      "<strong>%s</strong><br/>
      Hypertension Mortality Rate DI: %.2g<br/>
      Older At Risk Adults DI: %.2g<br/>
      At Risk Adults DI: %.2g<br/>
      <span style='background-color: #e1eaea'>Total Tests vs South Korea DI: %.2g</span><br/>
      Hospital Beds DI: %.2g",
      states$NAME, states$ht_death_rate_ldi, states$older_at_risk_ldi, states$at_risk_ldi, states$tests_ldi, states$hosp_beds_ldi
    ) %>% lapply(htmltools::HTML)
    
    leaflet(states.original) %>%
      setView(-96, 37.8, 4) %>% 
      addPolygons(
      fillColor = ~pal2(states$tests_ldi),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels2,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>% 
      addLegend(pal = pal2, values = ~states$tests_ldi, opacity = 0.7, title = "Disparity Index<br/>Total Tests",
                position = "bottomright") %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
        #Remove personal API key
        })
}

#### Set up Shiny App ####
shinyApp(ui = ui, server = server)
