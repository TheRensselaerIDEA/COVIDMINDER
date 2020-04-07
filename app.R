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

rpi_accessibility_link <- "<div class='center'><p><a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Statement of Accessibility</a></p></div>"

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
                    HTML("<b>OUTCOMES:</b></br>COVID-19 Mortality Rates (USA)")),
           sidebarLayout(
             sidebarPanel(
             HTML("<h4><b>How do COVID-19 mortality rates compare across the United States?</b></h4>
                             <i>This map compares the COVID-19 mortality rates of individual states with the US rate.
                            This map is updated daily.</i><br><br>
                            Here, <span style='color:#b2182b'><b>shades of red</b></span> indicate that a 
                            state's COVID-19 mortality rate is higher than the US rate<br><br>
                            Data source: <a href='https://bit.ly/3dMWRP6'>JHU daily reports</a> (04-06-2020)"),
             HTML(ldi_explanation_text), 
             HTML(rpi_accessibility_link), width=4),
             mainPanel(leafletOutput(outputId = "map.covid_deaths", height="85vh"), width=8)
           )
  ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<b>MEDIATION:</b></br>COVID-19 Testing (USA)")),
           sidebarLayout(
             sidebarPanel(HTML("<h4><b>How do COVID-19 testing rates across the US compare with South Korea?</b></h4>
                             <i>This map compares rates of COVID-19 tssting in US states vs South Korea's testing rate. 
                             This map is updated daily.</i><br><br>
                             Here, <span style='color:#b2182b'><b>shades of red</b></span> indicate that a 
                             state's testing rate is lower than the South Korean rate<br><br>
                             Data source: <a href='https://covidtracking.com/api'>The COVID Tracking Project daily reports</a> (04-06-2020)"),
                          HTML(ldi_explanation_text), 
                          HTML(rpi_accessibility_link), width=4),
             mainPanel(leafletOutput(outputId = "map.testing", height="85vh"), width=8)
           )
  ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<b>MEDIATION:</b></br>Hospital Beds (USA)")),
           sidebarLayout(
             sidebarPanel(HTML("<h4><b>How does the availability of hospital beds across the United States compare with Italy?</b></h4>
                             <i>This map compares the availability of hospital beds in US states vs the rate in Italy (3.2 beds/1000). 
                             This map uses recent historical figures and does not reflect 'surge' capacity.</i><br><br>
                             Here, <span style='color:#b2182b'><b>shades of red</b></span> indicate that a 
                             state's hospital bed availablity is lower than the rate in <b>Italy</b><br/><br>
                             Data sources: <br/><a href='https://data.oecd.org/healtheqt/hospital-beds.htm'> Organisation for Economic Co-operation and Development</a>
                             and <a href='https://bit.ly/2V0CYLU'>Kaiser Family Foundation</a>"),
                          HTML(ldi_explanation_text), 
                          HTML(rpi_accessibility_link), width=4),
             mainPanel(leafletOutput(outputId = "map.hospital", height="85vh"), width=8)
           )
  ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<b>RISK:</b></br>Cardiovascular Diseases (USA)")),
           sidebarLayout(
             sidebarPanel(HTML("<h4><b>How do cardiovascular mortality rates across the US compare with the national average?</b></h4>
                             <i>The map compares individual state mortality rates related to cardiovascular diseases (per 100k)
                            with the US rate. In recent literature, COVID-19 risk has been
                            linked to certain cardiovascular diseases, including hypertension. 
                            This map uses recent historical figures. </i><br><br>
                            Here, <span style='color:#b2182b'><b>shades of red</b></span> indicate that a 
                            state's mortality rate from total cardiovascular diseases is 
                            <b>higher</b> than the US rate<br/><br>
                            Data source: <br/><a href='https://bit.ly/2V1Zl3I'>CDC (2017)</a>
                            "),
                          HTML(ldi_explanation_text), 
                          HTML(rpi_accessibility_link), width=4),
             mainPanel(leafletOutput(outputId = "map.cardio", height="85vh"), width=8)
           )
  ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<b>STATE VIEW:</b></br>COVID-19 mortality rates (NY)")),
           sidebarLayout(
             sidebarPanel(HTML("<h4><b>How do COVID-19 mortality rates compare across New York State?</b></h4>
                             <i>This map compares the COVID-19 mortality rates of NY counties with the NY average. 
                            This map is updated daily. </i><br><br>
                            Here, <span style='color:#b2182b'><b>shades of red</b></span> indicate that a 
                            state's COVID-19 mortality rate is higher than the NY rate.<br>
                            Data source: <a href='https://bit.ly/3dMWRP6'>JHU daily reports</a> (04-06-2020)"),
                          HTML(ldi_explanation_text), 
                          HTML(rpi_accessibility_link), width=4),
             mainPanel(leafletOutput(outputId = "map.NY", height="85vh"), width=8)
           )
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
      "<strong>%s</strong> State<br/>
      Total Tests vs South Korea DI: %.2g",
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
      addLegend(pal = pal2, values = ~states$tests_ldi, opacity = 0.7, title = "Disparity Index<br/>US Total Tests vs. South Korea",
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
      Cardio Mortality Rate DI: %.2g<br/>",
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
      addLegend(pal = pal2, values = ~states$cardio_death_rate_ldi, opacity = 0.7, title = "Disparity Index<br/>US Cardio Mortality Rate",
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
      Hospital Beds vs Italy DI: %.2g",
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
      addLegend(pal = pal2, values = ~states$hosp_beds_ldi, opacity = 0.7, title = "Disparity Index<br/>US Hospital Beds vs Italy",
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
      COVID-19 Mortality Rate DI: %.2g",
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
                title = "Disparity Index<br/>US COVID-19 Mortality Rates",
                position = "bottomright"
                ) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    #Remove personal API key
  })
  
  output$map.NY <- renderLeaflet({
    colors <- c("grey","#426C85","#67a9cf","#d1e5f0","#f7f7f7","#fddbc7","#ef8a62","#b2182b")
#    bins <- c(10, 5, 2, 1, .2, -.2, -1, -2, -5)
    bins <- c(5, 2, 1, .2, -.2, -1, -2, -5,-Inf)
    pal2 <- leaflet::colorBin(colors, domain = NY.data$death_rate_ldi, bins = bins, reverse=FALSE)
    
    NY.shape$county_fips <- paste(as.data.frame(NY.shape)$STATEFP, as.data.frame(NY.shape)$COUNTYFP, sep = '')
    NY.data <- dplyr::left_join(as.data.frame(NY.shape), as.data.frame(NY.data), by = c("county_fips" = "FIPS"))
    
    labels <- sprintf(
      "<strong>%s</strong><br/>
      COVID-19 Mortality Rate DI: %.2g<br>
      COVID-19 actual deaths: %g<br/>
      County population: %d",
      NY.data$County, NY.data$death_rate_ldi, NY.data$deaths, NY.data$Population
    ) %>% lapply(htmltools::HTML)

    leaflet(NY.shape) %>%
      setView(-76.071782, 42.991989, 7) %>%  # Set to the geographic center of NY
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
                title = "Disparity Index<br/>NY COVID-19 Mortality Rates",
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
