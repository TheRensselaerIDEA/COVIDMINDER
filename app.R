#### Library and Data Imports ####
source("modules/Source.R")
source("modules/data_load.R")
source("modules/preprocessing.R")

#### UI Code ####
ui <- navbarPage(
  title="CovidMinder",
  tabPanel("INTERVENTION: COVID-19 Testing",
           fluidRow(
              column(3, HTML("<b>Nationwide Disparity Index</b></br>
                             Total COVID-19 Testing/State</br>
                             <i>Illustrating disparity of US states vs South Korea testing rate</i>")),
              column(9, leafletOutput(outputId = "map.testing", width="100%"))
            )
  ),
  tabPanel("RISK: Hospital Beds",
           fluidRow(
             column(3, HTML("<b>Nationwide Disparity Index</b></br>
                             Total Hospital Beds/State</br>
                             <i>Illustrating disparity of US states vs US average</i>")),
             column(9, leafletOutput(outputId = "map.hospital", width="100%"))
           )
  ),
  tabPanel("RISK: Hypertension Mortality",
           fluidRow(
             column(3, HTML("<b>Nationwide Disparity Index</b></br>
                             Hypertension Mortality Rate/State</br>
                             <i>Illustrating disparity of US states vs US average</i><br><br>
                            <a href='https://ccforum.biomedcentral.com/articles/10.1186/s13054-020-2833-7'>Studies from Wuhan, China</a> have indicated a
                            higher incidence of hypertension in the histories of patients admitted with severe COVID-19")),
             column(9, leafletOutput(outputId = "map.hypertension", width="100%"))
           )
  ),
  tabPanel("OUTCOMES: COVID-19 Death Rates",
           fluidRow(
             column(3, HTML("<b>Nationwide Disparity Index</b></br>
                             COVID-19 Deaths vs Cases/State</br>
                             <i>Illustrating disparity of US states vs US average</i><br><br>
                            Here, 'overrepresented'(blue) indicates that a state's COVID-19 death rate is higher than the US rate")),
             column(9, leafletOutput(outputId = "map.covid_deaths", width="100%"))
           )
  )
  
)

#### Server Code ####
server <- function(input, output, session) {
  
  # Render leaflet plot with all information in hover
  output$map.testing <- renderLeaflet({
    colors <- c("#ef8a62","#fddbc7","#f7f7f7","#d1e5f0","#67a9cf")
    bins <- c(-5, -2, -1, -.2, .2, 1, 2, 3)
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
  
  output$map.hypertension <- renderLeaflet({
    colors <- c("#ef8a62","#fddbc7","#f7f7f7","#d1e5f0","#67a9cf")
    bins <- c(-5, -2, -1, -.2, .2, 1, 2, 3)
    pal2 <- leaflet::colorBin(colors, domain = states$ht_death_rate_ldi, bins = bins, reverse=FALSE)
    labels2 <- sprintf(
      "<strong>%s</strong><br/>
      <span style='background-color: #e1eaea'>Hypertension Mortality Rate DI: %.2g</span><br/>
      Older At Risk Adults DI: %.2g<br/>
      At Risk Adults DI: %.2g<br/>
      Total Tests vs South Korea DI: %.2g<br/>
      Hospital Beds DI: %.2g</span>",
      states$NAME, states$ht_death_rate_ldi, states$older_at_risk_ldi, states$at_risk_ldi, states$tests_ldi, states$hosp_beds_ldi
    ) %>% lapply(htmltools::HTML)
    
    leaflet(states.original) %>%
      setView(-96, 37.8, 4) %>% 
      addPolygons(
        fillColor = ~pal2(states$ht_death_rate_ldi),
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
      addLegend(pal = pal2, values = ~states$ht_death_rate_ldi, opacity = 0.7, title = "Disparity Index<br/>Hypertension Mortality Rate",
                position = "bottomright") %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    #Remove personal API key
  })

  output$map.hospital <- renderLeaflet({
    colors <- c("#ef8a62","#fddbc7","#f7f7f7","#d1e5f0","#67a9cf")
    bins <- c(-5, -2, -1, -.2, .2, 1, 2, 3)
    pal2 <- leaflet::colorBin(colors, domain = states$hosp_beds_ldi, bins = bins, reverse=FALSE)
    labels2 <- sprintf(
      "<strong>%s</strong><br/>
      Hypertension Mortality Rate DI: %.2g<br/>
      Older At Risk Adults DI: %.2g<br/>
      At Risk Adults DI: %.2g<br/>
      Total Tests vs South Korea DI: %.2g<br/>
      <span style='background-color: #e1eaea'>Hospital Beds DI: %.2g</span>",
      states$NAME, states$ht_death_rate_ldi, states$older_at_risk_ldi, states$at_risk_ldi, states$tests_ldi, states$hosp_beds_ldi
    ) %>% lapply(htmltools::HTML)
    
    leaflet(states.original) %>%
      setView(-96, 37.8, 4) %>% 
      addPolygons(
        fillColor = ~pal2(states$hosp_beds_ldi),
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
      addLegend(pal = pal2, values = ~states$hosp_beds_ldi, opacity = 0.7, title = "Disparity Index<br/>Hospital Beds",
                position = "bottomright") %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    #Remove personal API key
  })
  
  output$map.covid_deaths <- renderLeaflet({
    colors <- c("grey","#ef8a62","#fddbc7","#f7f7f7","#d1e5f0","#67a9cf")
    bins <- c(-Inf, -5, -2, -1, -.2, .2, 1, 2, 3)
#    bins <- c(-5, -2, -1, -.2, .2, 1, 2, 3)
    pal2 <- leaflet::colorBin(colors, domain = states$deaths_cases_ldi, bins = bins, reverse=FALSE)
    labels2 <- sprintf(
      "<strong>%s</strong><br/>
      <span style='background-color: #e1eaea'>COVID-19 Death Rate (vs Cases) DI: %.2g</span><br/>
      Hypertension Mortality Rate DI: %.2g<br/>
      Older At Risk Adults DI: %.2g<br/>
      At Risk Adults DI: %.2g<br/>
      Total Tests vs South Korea DI: %.2g<br/>
      Hospital Beds DI: %.2g",
      states$NAME, states$deaths_cases_ldi, states$ht_death_rate_ldi, states$older_at_risk_ldi, states$at_risk_ldi, states$tests_ldi, states$hosp_beds_ldi
    ) %>% lapply(htmltools::HTML)
    
    leaflet(states.original) %>%
      setView(-96, 37.8, 4) %>% 
      addPolygons(
        fillColor = ~pal2(states$deaths_cases_ldi),
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
      addLegend(pal = pal2, values = ~states$deaths_cases_ldi, opacity = 0.7, title = "Disparity Index<br/>COVID-19 Deaths vs Cases",
                position = "bottomright") %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    #Remove personal API key
  })
  
}

#### Set up Shiny App ####
shinyApp(ui = ui, server = server)
