#### Library and Data Imports ####
source("modules/Source.R")
source("modules/data_load.R")
source("modules/preprocessing.R")

#### UI Code ####
ui <- navbarPage(
  title="COVIDMinder",
  tabPanel("OUTCOMES: COVID-19 Mortality Rates",
           fluidRow(
             column(3, HTML("<b>Nationwide Disparity Index</b></br>
                             COVID-19 Mortality Rates/State</br>
                             <i>Illustrating disparity of US states vs US average</i><br><br>
                            Here, <span style='color:#67a9cf'>over-represented</span> indicates that a 
                            state's COVID-19 mortality rate is higher than the US rate")
                    # ,
                    # radioButtons("pUS.6", "Compare with:",
                    #              c("United States" = 0.01925,
                    #                "China" = 0.04023,
                    #                "Italy" = 0.11392,
                    #                "Germany" = 0.00969,
                    #                "Spain" = 0.08772,
                    #                "UK" = 0.06285))
             ),
             column(9, leafletOutput(outputId = "map.covid_deaths", width="100%"))
           ),
           fluidRow(column(10,
             HTML("<p>&nbsp;</p><p><b>EXPLAINATION:</b> The goal of these visualizations is to examine state disparities in COVID-19 
factors having to  do with risk, mediations (e.g. testing, hospital beds), and outcomes (e.g. deaths, cases).    
A common measure, the <i>disparity index</i> is used to represent the difference between the observed 
rate in the state and some baseline rate.</p>
<p>The advantage of the disparity index is that represents how far off target the observed rate is. </p>
<p>Mathematically,
<code>tau = log((x/(1-x))/(y/(1-y)))</code>
where <i>x</i> would be some state's rate or probability, and <i>y</i> would be the US rate or some rate 
we're comparing against (e.g. South Korea's rate of testing, which we use in one of the plots).  
You can think of this as a <i>log odds ratio</i> except the comparison is to a reference population instead of to the usual 'not in the state' population.</p>
<p>We developed this measure as part of our <i>Machine Learning Fairness Research.</i></p>
<p><code>DI > 0.2</code> is considered overrepresented, <code>DI < 0.2</code> is considered underrepresented.</p> ")
           ))
  ),
  tabPanel("MEDIATION: COVID-19 Testing",
           fluidRow(
              column(3, HTML("<b>Nationwide Disparity Index</b></br>
                             Total COVID-19 Testing/State</br>
                             <i>Illustrating disparity of US states vs South Korea testing rate</i><br><br>
                             Here, <span style='color:#ef8a62'>under-represented</span> indicates that a state's testing rate is lower than the South Korean rate")),
              column(9, leafletOutput(outputId = "map.testing", width="100%"))
            )
  ),
  tabPanel("MEDIATION: Hospital Beds",
           fluidRow(
             column(3, HTML("<b>Nationwide Disparity Index</b></br>
                             Total Hospital Beds/State</br>
                             <i>Illustrating disparity of US states vs US average</i><br><br>
                             Here, <span style='color:#ef8a62'>under-represented</span> indicates that a state's hospital bed availablity is lower than the US rate")),
             column(9, leafletOutput(outputId = "map.hospital", width="100%"))
           )
  )
  # ,
  # tabPanel("RISK: Hypertension Mortality",
  #          fluidRow(
  #            column(3, HTML("<b>Nationwide Disparity Index</b></br>
  #                            Hypertension Mortality Rate/State</br>
  #                            <i>Illustrating disparity of US states vs US average</i><br><br>
  #                           <a href='https://ccforum.biomedcentral.com/articles/10.1186/s13054-020-2833-7'>Studies from Wuhan, China</a> have indicated a
  #                           higher incidence of hypertension in the histories of patients admitted with severe COVID-19<br><br>
  #                           Here, <span style='color:#67a9cf'>over-represented</span> indicates that a state's hypertension mortality is higher than the US rate")),
  #            column(9, leafletOutput(outputId = "map.hypertension", width="100%"))
  #          )
  # )
)

#### Server Code ####
server <- function(input, output, session) {
  
  # Render leaflet plot with all information in hover
  output$map.testing <- renderLeaflet({
    colors <- c("#b2182b","#ef8a62","#fddbc7","#f7f7f7","#d1e5f0","#67a9cf")
    bins <- c(-5, -2, -1, -.2, .2, 1, 2, 3)
    pal2 <- leaflet::colorBin(colors, domain = states$tests_ldi, bins = bins, reverse=FALSE)
    labels2 <- sprintf(
      "<strong>%s</strong><br/>
      COVID-19 Mortality Rate DI: %.2g<br/>
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
    colors <- c("#b2182b","#ef8a62","#fddbc7","#f7f7f7","#d1e5f0","#67a9cf")
    bins <- c(-5, -2, -1, -.2, .2, 1, 2, 3)
    pal2 <- leaflet::colorBin(colors, domain = states$ht_death_rate_ldi, bins = bins, reverse=FALSE)
    labels2 <- sprintf(
      "<strong>%s</strong><br/>
      COVID-19 Mortality Rate DI: %.2g<br/>
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
    colors <- c("#b2182b","#ef8a62","#fddbc7","#f7f7f7","#d1e5f0","#67a9cf")
    bins <- c(-5, -2, -1, -.2, .2, 1, 2, 3)
    pal2 <- leaflet::colorBin(colors, domain = states$hosp_beds_ldi, bins = bins, reverse=FALSE)
    labels2 <- sprintf(
      "<strong>%s</strong><br/>
      COVID-19 Mortality Rate DI: %.2g<br/>
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
    colors <- c("grey", "#b2182b", "#ef8a62", "#fddbc7", "#f7f7f7", "#d1e5f0", "#67a9cf")
    bins <- c(-Inf, -5, -2, -1, -.2, .2, 1, 2, 5)
#    bins <- c(-5, -2, -1, -.2, .2, 1, 2, 3)
    pal2 <- leaflet::colorBin(colors, domain = states$death_rate_ldi, bins = bins, reverse=FALSE)
    labels2 <- sprintf(
      "<strong>%s</strong><br/>
      <span style='background-color: #e1eaea'>COVID-19 Mortality Rate DI: %.2g</span><br/>
      Total Tests vs South Korea DI: %.2g<br/>
      Hospital Beds DI: %.2g",
      states$NAME, states$death_rate_ldi, states$ht_death_rate_ldi, states$older_at_risk_ldi, states$at_risk_ldi, states$tests_ldi, states$hosp_beds_ldi
    ) %>% lapply(htmltools::HTML)
    
    leaflet(states.original) %>%
      setView(-96, 37.8, 4) %>% 
      addPolygons(
        fillColor = ~pal2(states$death_rate_ldi),
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
      addLegend(pal = pal2, 
                values = ~states$death_rate_ldi, 
                opacity = 0.7, 
                title = "Disparity Index<br/>COVID-19 Mortality Rates",
                position = "bottomright"
                ) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    #Remove personal API key
  })
  
}

#### Set up Shiny App ####
shinyApp(ui = ui, server = server)
