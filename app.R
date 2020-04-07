#### Library and Data Imports ####
source("modules/Source.R")
source("modules/data_load.R")
source("modules/preprocessing.R")

ldi_explanation_text <- "<p>&nbsp;</p><p><b>EXPLANATION:</b> The goal of these visualizations is to examine 
nationwide disparities in COVID-19factors having to do with risks, mediations (e.g. testing, hospital beds), 
and outcomes (e.g. deaths, cases). A common measure, the <i>disparity index</i> is used to represent the 
difference between the observed rate in the state and some baseline rate.</p>
<p>The advantage of the disparity index is that represents how far off a target standard the observed rate is. </p>
<p>Mathematically,
<code>DI = log(x/y)</code> or <code>DI = log(y/x)</code>
depending upon whether being above or below the target is preferred. 
<ul>
<li>In the case of hospital beds or rate of testing, <i>x</i> would be some state's rate , and <i>y</i> would be the US rate or some rate 
we're comparing against (e.g. South Korea's testing or Italy's hospital beds).</li>
<li>In the case of mortality rates, <i>x</i> would be the target rate (e.g. some national rate, including the US), and <i>y</i> would be the individual state's rate.</li>
</ul>
</p>"

#### UI Code ####
ui <- 
  tagList(
    tags$head(
      tags$title("COVIDMINDER")
    ),
  navbarPage(
  theme="style.css",
  title=tags$div(class="title-text",
            img(class="logo", src="Rensselaer_round.png"),
            HTML("COVID<b>MINDER</b>")),
  tabPanel(tags$div(class="tab-title",style="text-align:center;", #For some reason, unresponsive to class
                    HTML("<b>OUTCOMES:</b></br>COVID-19 Mortality Rates")),
           sidebarLayout(
             sidebarPanel(
             HTML("<b>Nationwide Disparity Index</b></br>
                             COVID-19 Mortality Rates/State</br>
                             <i>Illustrating disparity of US states vs US average</i><br><br>
                            Here, <span style='color:#b2182b'><b>shades of red</b></span> indicate that a 
                            state's COVID-19 mortality rate is higher than the US rate<br><br>
                            Data source: <a href='https://bit.ly/3dMWRP6'>JHU daily reports</a> (04-06-2020)"),
             HTML(ldi_explanation_text), width=4),
             mainPanel(
              leafletOutput(outputId = "map.covid_deaths", height="85vh"), width=8)
           )
  ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<b>MEDIATION:</b></br>COVID-19 Testing")),
           fluidRow(
              column(3, HTML("<b>Nationwide Disparity Index</b></br>
                             Total COVID-19 Testing/State</br>
                             <i>Illustrating disparity of US states vs South Korea testing rate</i><br><br>
                             Here, <span style='color:#b2182b'><b>shades of red</b></span> indicate that a 
                             state's testing rate is lower than the South Korean rate<br><br>
                             Data source: <a href='https://covidtracking.com/api'>The COVID Tracking Project daily reports</a> (04-06-2020)")),
              column(9, leafletOutput(outputId = "map.testing", width="100%"))
           ),
           fluidRow(column(10,
                           HTML(ldi_explanation_text)
           ))
  ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<b>MEDIATION:</b></br>Hospital Beds")),
           fluidRow(
             column(3, HTML("<b>Nationwide Disparity Index</b></br>
                             Total Hospital Beds/State</br>
                             <i>Illustrating disparity of US beds/1000 vs Italy rate (3.2/1000)</i><br><br>
                             Here, <span style='color:#b2182b'><b>shades of red</b></span> indicate that a 
                             state's hospital bed availablity is lower than the rate in <b>Italy</b><br/><br>
                             Data sources: <br/><a href='https://data.oecd.org/healtheqt/hospital-beds.htm'>OECD Data</a><br/>
                            <a href='https://bit.ly/2V0CYLU'>Kaiser Family Foundation</a>")),
             column(9, leafletOutput(outputId = "map.hospital", width="100%"))
           ),
           fluidRow(column(10,
                           HTML(ldi_explanation_text)
           ))
  ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<b>RISK:</b></br>Cardiovascular Diseases")),
           fluidRow(
             column(3, HTML("<b>Nationwide Disparity Index</b></br>
                             Mortality from total cardiovascular diseases (per 100k)</br>
                             <i>Illustrating disparity of US rate/100k vs US average</i><br><br>
                            Here, <span style='color:#b2182b'><b>shades of red</b></span> indicate that a 
                            state's mortality rate from total cardiovascular diseases is 
                            <b>higher</b> than the US rate<br/><br>
                            Data source: <br/><a href='https://bit.ly/2V1Zl3I'>CDC (2017)</a>
                            ")),
             column(9, leafletOutput(outputId = "map.cardio", width="100%"))
           ),
           fluidRow(column(10,
                           HTML(ldi_explanation_text)
           ))
  ),
  tabPanel("NY: COVID-19 mortality rates",
           fluidRow(
             column(3, HTML("<b>New York Disparity Index</b></br>
                             COVID-19 Mortality Rates/State</br>
                             <i>Illustrating disparity of NY counties vs NY average</i><br><br>
                            Here, <span style='color:#b2182b'><b>shades of red</b></span> indicate that a 
                            state's COVID-19 mortality rate is higher than the NY rate")),
             column(9, leafletOutput(outputId = "map.NY", width="100%"))
           ),
           fluidRow(column(10,
                           HTML(ldi_explanation_text)
           ))
  )
  )
)
#### Server Code ####
server <- function(input, output, session) {
  
  # Render leaflet plot with all information in hover
  output$map.testing <- renderLeaflet({
    # colors <- c("#b2182b","#ef8a62","#fddbc7","#f7f7f7","#d1e5f0","#67a9cf","#426C85")
    # bins <- c(-5, -2, -1, -.2, .2, 1, 2, 5)
    # pal2 <- leaflet::colorBin(colors, domain = states$tests_ldi, bins = bins, reverse=TRUE)
    colors <- c("#426C85","#67a9cf","#d1e5f0","#f7f7f7","#fddbc7","#ef8a62","#b2182b")
    bins <- c(5, 2, 1, .2, -.2, -1, -2, -5)
    pal2 <- leaflet::colorBin(colors, domain = states$tests_ldi, bins = bins, reverse=FALSE)
    labels2 <- sprintf(
      "<strong>%s</strong><br/>
      COVID-19 Mortality Rate DI: %.2g<br/>
      <span style='background-color: #e1eaea'>Total Tests vs South Korea DI: %.2g</span><br/>
      Hospital Beds DI: %.2g<br>
      Cardio Mortality Rate DI: %.2g",
      states$NAME, states$death_rate_ldi, states$tests_ldi, states$hosp_beds_ldi,states$cardio_death_rate_ldi
    ) %>% lapply(htmltools::HTML)
    
    leaflet(states.shapes, width="100%", height="100%") %>%
      setView(-96, 37.8, 4) %>% # TODO: Doesn't seem to do anything
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
      addLegend(pal = pal2, values = ~states$tests_ldi, opacity = 0.7, title = "Disparity Index<br/>Total Tests vs. South Korea",
                position = "bottomright") %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
        })
  
  output$map.cardio <- renderLeaflet({
    colors <- c("#426C85","#67a9cf","#d1e5f0","#f7f7f7","#fddbc7","#ef8a62","#b2182b")
    bins <- c(5, 2, 1, .2, -.2, -1, -2, -5)
    pal2 <- leaflet::colorBin(colors, domain = states$cardio_death_rate_ldi, bins = bins, reverse=FALSE)
    labels2 <- sprintf(
      "<strong>%s</strong><br/>
      COVID-19 Mortality Rate DI: %.2g<br/>
      Total Tests vs South Korea DI: %.2g<br/>
      Hospital Beds DI: %.2g<br/>
      <span style='background-color: #e1eaea'>Cardio Mortality Rate DI: %.2g</span><br/>",
      states$NAME, states$death_rate_ldi, states$tests_ldi, states$hosp_beds_ldi,states$cardio_death_rate_ldi
    ) %>% lapply(htmltools::HTML)
    
    leaflet(states.shapes) %>%
      setView(-96, 37.8, 4) %>% 
      addPolygons(
        fillColor = ~pal2(states$cardio_death_rate_ldi),
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
      addLegend(pal = pal2, values = ~states$cardio_death_rate_ldi, opacity = 0.7, title = "Disparity Index<br/>Cardio Mortality Rate",
                position = "bottomright") %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    #Remove personal API key
  })

  output$map.hospital <- renderLeaflet({
    colors <- c("#426C85","#67a9cf","#d1e5f0","#f7f7f7","#fddbc7","#ef8a62","#b2182b")
    bins <- c(5, 2, 1, .2, -.2, -1, -2, -5)
    pal2 <- leaflet::colorBin(colors, domain = states$hosp_beds_ldi, bins = bins, reverse=FALSE)
    labels2 <- sprintf(
      "<strong>%s</strong><br/>
      COVID-19 Mortality Rate DI: %.2g<br/>
      Total Tests vs South Korea DI: %.2g<br/>
      <span style='background-color: #e1eaea'>Hospital Beds vs Italy DI: %.2g</span><br>
      Cardio Mortality Rate DI: %.2g",
      states$NAME, states$death_rate_ldi, states$tests_ldi, states$hosp_beds_ldi,states$cardio_death_rate_ldi
    ) %>% lapply(htmltools::HTML)
    
    leaflet(states.shapes) %>%
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
      addLegend(pal = pal2, values = ~states$hosp_beds_ldi, opacity = 0.7, title = "Disparity Index<br/>Hospital Beds vs Italy",
                position = "bottomright") %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    #Remove personal API key
  })
  
  output$map.covid_deaths <- renderLeaflet({
    # colors <- c("grey", "#b2182b", "#ef8a62", "#fddbc7", "#f7f7f7", "#d1e5f0", "#67a9cf","#426C85")
    # bins <- c(-Inf, -5, -2, -1, -.2, .2, 1, 2, 5)
    colors <- c("grey","#426C85","#67a9cf","#d1e5f0","#f7f7f7","#fddbc7","#ef8a62","#b2182b")
    bins <- c(5, 2, 1, .2, -.2, -1, -2, -5,-Inf)
    pal2 <- leaflet::colorBin(colors, domain = states$death_rate_ldi, bins = bins, reverse=FALSE)
    labels2 <- sprintf(
      "<strong>%s</strong><br/>
      <span style='background-color: #e1eaea'>COVID-19 Mortality Rate DI: %.2g</span><br/>
      Total Tests vs South Korea DI: %.2g<br/>
      Hospital Beds DI: %.2g<br>
      Cardio Mortality Rate DI: %.2g",
      states$NAME, states$death_rate_ldi, states$tests_ldi, states$hosp_beds_ldi,states$cardio_death_rate_ldi
    ) %>% lapply(htmltools::HTML)
    
    leaflet(states.shapes) %>%
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
  
  output$map.NY <- renderLeaflet({
    colors <- c("grey","#426C85","#67a9cf","#d1e5f0","#f7f7f7","#fddbc7","#ef8a62","#b2182b")
    bins <- c(10, 5, 2, 1, .2, -.2, -1, -2, -5)
    pal2 <- leaflet::colorBin(colors, domain = NY.data$death_rate_ldi, bins = bins, reverse=FALSE)
    
    NY.shape$county_fips <- paste(as.data.frame(NY.shape)$STATEFP, as.data.frame(NY.shape)$COUNTYFP, sep = '')
    NY.data <- dplyr::left_join(as.data.frame(NY.shape), as.data.frame(NY.data), by = c("county_fips" = "FIPS"))
    
    labels <- sprintf(
      "<strong>%s</strong><br/>
      <span style='background-color: #e1eaea'>COVID-19 Mortality Rate DI: %.2g</span><br/>
      Deaths: %g<br/>
      Population: %d",
      NY.data$County, NY.data$death_rate_ldi, NY.data$deaths, NY.data$Population
    ) %>% lapply(htmltools::HTML)

    leaflet(NY.shape) %>%
      setView(-74.006, 42.714, 6) %>% 
      addPolygons(
        fillColor = ~pal2(NY.data$death_rate_ldi),
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
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>% 
      addLegend(pal = pal2, 
                values = ~NY.data$death_rate_ldi, 
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
