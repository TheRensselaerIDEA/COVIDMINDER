#### Library and Data Imports ####
source("modules/Source.R")
source("modules/data_load.R")
source("modules/preprocessing.R")

ldi_explanation_text <- "<p>&nbsp;</p><p><b>EXPLANATION:</b> These visualizations highlight 
nationwide disparities in COVID-19 factors having to do with determinants, mediations (e.g. testing, hospital beds), 
and outcomes (e.g. deaths, cases). A common measure, the <i>disparity index</i> is used to represent the 
difference between the observed rate in the state or county and some baseline rate.</p>
<p>The disparity index is <i>intuitive</i>; it represents how far off a chosen standard the observed rate is. </p>
<p>Mathematically,
<code>DI = log(x/y)</code> or <code>DI = -log(x/y)</code>
depending upon whether a rate being above or below the target is detrimental. 
<ul>
<li>In the case of hospital beds or rate of testing, <i>x</i> would be some state's rate , and <i>y</i> would be the US rate or some rate 
we're comparing against (e.g. South Korea's testing or Italy's hospital beds).</li>
<li>In the case of mortality rates, <i>x</i> would be the target rate (e.g. some national rate, including the US), and <i>y</i> would be the individual state or county's rate.</li>
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
                    HTML("<b>OUTCOME (USA):</b></br>COVID-19 Deaths")),
           sidebarLayout(
             sidebarPanel(
             HTML("<h4><b>How do COVID-19 mortality rates compare across the United States?</b></h4>
                             <i>This map compares the COVID-19 mortality rates of individual states with the US rate.
                            This map is updated daily.</i><br><br>
                            Here, <span style='color:#b2182b'><b>shades of red</b></span> indicate that a 
                            state's COVID-19 mortality rate is higher than the US rate.<br><br>
                            <b>DATA SOURCE:</b> <a href='http://bit.ly/39PMWpD'>JHU CSSE (daily)</a><br>
                            <b>ANALYSIS:</b> <a href='http://idea.rpi.edu'>The Rensselaer IDEA</a>
                            <br>"),
             HTML(ldi_explanation_text), 
             #HTML(rpi_accessibility_link), 
             width=4),
             mainPanel(leafletOutput(outputId = "map.covid_deaths", height="85vh"), width=8)
           )
  ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<b>MEDIATION (USA):</b></br>COVID-19 Testing")),
           sidebarLayout(
             sidebarPanel(HTML("<h4><b>How do COVID-19 testing rates across the US compare with South Korea?</b></h4>
                             <i>This map compares rates of COVID-19 tssting in US states vs South Korea's testing rate. 
                             This map is updated daily.</i><br><br>
                             Here, <span style='color:#b2182b'><b>shades of red</b></span> indicate that a 
                             state's testing rate is lower than the South Korean rate.<br><br>
                             <b>DATA SOURCES:</b> <a href='https://bit.ly/2JRhDiX'>COVID Tracking Project (daily)</a> and 
                            <a href='https://bit.ly/3aXpBmD'>Organisation for Economic Co-operation and Development</a><br>
                            <b>ANALYSIS:</b> <a href='http://idea.rpi.edu'>The Rensselaer IDEA</a>
                            <br>"),
                          HTML(ldi_explanation_text), 
                          #HTML(rpi_accessibility_link), 
                          width=4),
             mainPanel(leafletOutput(outputId = "map.testing", height="85vh"), width=8)
           )
  ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<b>MEDIATION (USA):</b></br>Hospital Beds")),
           sidebarLayout(
             sidebarPanel(HTML("<h4><b>How does the availability of hospital beds across the United States compare with Italy?</b></h4>
                             <i>This map compares the availability of hospital beds in US states vs the rate in Italy (3.2 beds/1000). 
                             This map uses recent historical figures and does not reflect 'surge' capacity.</i><br><br>
                             Here, <span style='color:#b2182b'><b>shades of red</b></span> indicate that a 
                             state's hospital bed availablity is lower than the rate in <b>Italy</b><br><br>
                             <b>DATA SOURCE:</b> <a href='https://bit.ly/2V0CYLU'>Kaiser Family Foundation</a><br>
                            <b>ANALYSIS:</b> <a href='http://idea.rpi.edu'>The Rensselaer IDEA</a>
                            <br>"),
                          HTML(ldi_explanation_text), 
                          #HTML(rpi_accessibility_link), 
                          width=4),
             mainPanel(leafletOutput(outputId = "map.hospital", height="85vh"), width=8)
           )
  ),
  ## DON"T DELETE! We may restore this or re-purpose
  # tabPanel(tags$div(class="tab-title",style="text-align:center;",
  #                   HTML("<b>DETERMINANT (USA):</b></br>Cardiovascular Diseases")),
  #          sidebarLayout(
  #            sidebarPanel(HTML("<h4><b>How do cardiovascular mortality rates across the US compare with the national average?</b></h4>
  #                            <i>The map compares individual state mortality rates related to cardiovascular diseases (per 100k)
  #                           with the US rate. In recent literature, COVID-19 risk has been
  #                           linked to certain cardiovascular diseases, including hypertension.
  #                           This map uses recent historical figures. </i><br><br>
  #                           Here, <span style='color:#b2182b'><b>shades of red</b></span> indicate that a 
  #                           state's mortality rate from total cardiovascular diseases is 
  #                           <b>higher</b> than the US rate.
  #                           "),
  #                         HTML(ldi_explanation_text), 
  #                         #HTML(rpi_accessibility_link), 
  #                         width=4),
  #            mainPanel(leafletOutput(outputId = "map.cardio", height="85vh"), width=8)
  #          )
  # ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<b>DETERMINANT (USA):</b></br>Diabetes")),
           sidebarLayout(
             sidebarPanel(HTML("<h4><b>How do diabetes rates across the US compare with the national average?</b></h4>
                             <i>The map compares the percentage of a state's population with diabetes 
                            with the US percentage.</i><br><br>
                            Here, <span style='color:#b2182b'><b>shades of red</b></span> indicate that a 
                            state's mortality rate from total cardiovascular diseases is 
                            <b>higher</b> than the US rate.
                            <b>NOTE:</b> <i>Diabetes has been reported to be a risk factor for the severity 
                            of COVID-19 and at the same time patients have to control their glucose while living in a
                            with a decreased and more variable food intake.</i>  
                            (See Sten Madsbad, <a href='https://bit.ly/34yW1BD'>COVID-19 Infection in People with Diabetes</a>)<br><br>
                            <b>DATA SOURCES:</b> <a href='https://bit.ly/34mYLBP'>County Health Rankings</a> and 
                            <a href='https://bit.ly/2V1Zl3I'>CDC</a><br>
                            <b>ANALYSIS:</b> <a href='http://idea.rpi.edu'>The Rensselaer IDEA</a>
                            "),
                          HTML(ldi_explanation_text), 
                          #HTML(rpi_accessibility_link), 
                          width=4),
             mainPanel(leafletOutput(outputId = "map.diabetes", height="85vh"), width=8)
           )
  ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<b>OUTCOME (NY):</b></br>COVID-19 Deaths")),
           sidebarLayout(
             sidebarPanel(HTML("<h4><b>How do New York State COVID-19 mortality rates compare with the US rate?</b></h4>
                             <i>This map compares the COVID-19 mortality rates of NY counties with the United States average. 
                            This map is updated daily. </i><br><br>
                            Here, <span style='color:#b2182b'><b>shades of red</b></span> indicate that a 
                            county's COVID-19 mortality rate is higher than the US rate.<br><br>
                            <B>DATA SOURCES:</b> <a href='http://bit.ly/39PMWpD'>JHU CSSE (daily)</a> and 
                            <a href='https://on.ny.gov/2yOj1AD'>New York State Dept. of Health COVID19Tracker (daily)</a><br>
                            <b>ANALYSIS:</b> <a href='http://idea.rpi.edu'>The Rensselaer IDEA</a>
                            <br>"),
                          HTML(ldi_explanation_text), 
                          #HTML(rpi_accessibility_link), 
                          width=4),
             mainPanel(leafletOutput(outputId = "map.NY.deaths", height="85vh"), width=8)
           )
  ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<b>OUTCOME (NY):</b></br>COVID-19 Cases")),
           sidebarLayout(
             sidebarPanel(HTML("<h4><b>How do COVID-19 cases compare across New York State?</b></h4>
                             <i>This map compares the COVID-19 case rates for NY counties with the NY average. 
                            This map is updated daily. </i><br><br>
                            Here, <span style='color:#b2182b'><b>shades of red</b></span> indicate that a 
                            county's COVID-19 case rate (cases per county population) is higher than the NY rate.<br><br>
                            <b>NOTE:</b> <i>Test counts and results are assigned to a county based on this order of preference: 
                            1) the patient’s address, 2) the ordering healthcare provider’s address, or 3) the ordering facility’s 
                            address.</i>  (New York State Dept. of Health)<br><br>
                            <b>DATA SOURCE:</b> <a href='https://on.ny.gov/39VXuCO'>heath.data.ny.gov (daily)</a><br>
                            <b>ANALYSIS:</b> <a href='http://idea.rpi.edu'>The Rensselaer IDEA</a>
                            <br>"),
                          HTML(ldi_explanation_text), 
                          #HTML(rpi_accessibility_link), 
                          width=4),
             mainPanel(leafletOutput(outputId = "map.NY.cases", height="85vh"), width=8)
           )
  ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<b>DETERMINANT (NY):</b></br>Diabetes")),
           sidebarLayout(
             sidebarPanel(HTML("<h4><b>How do rates of diabetes compare across counties in New York State?</b></h4>
                             <i>This map compares the diabetes rates for NY counties with the NY average. 
                            This map is based on historical data (2017). </i><br><br>
                            Here, <span style='color:#b2182b'><b>shades of red</b></span> indicate that a 
                            county's diabetes rate is higher than the NY rate.<br><br>
                            <b>NOTE:</b> <i>Diabetes has been reported to be a risk factor for the severity 
                            of COVID-19 and at the same time patients have to control their glucose while living in a
                            with a decreased and more variable food intake.</i>  
                            (See Sten Madsbad, <a href='https://bit.ly/34yW1BD'>COVID-19 Infection in People with Diabetes</a>)<br><br>
                            <b>DATA SOURCES:</b> <a href='https://bit.ly/34mYLBP'>County Health Rankings</a> and 
                            <a href='https://bit.ly/2V1Zl3I'>CDC</a><br>
                            <b>ANALYSIS:</b> <a href='http://idea.rpi.edu'>The Rensselaer IDEA</a>
                            "),
                          HTML(ldi_explanation_text), 
                          #HTML(rpi_accessibility_link), 
                          width=4),
             mainPanel(leafletOutput(outputId = "map.NY.diabetes", height="85vh"), width=8)
           )
  ),
  tags$br(),
  footer = fluidRow(class = "navbar navbar-default footer", 
                    column(6,
                           HTML("<b>ABOUT: </b>
                                <b>COVIDMINDER</b> is an open source interactive application 
                                aimed at helping health care providers and other stakeholders 
                                understand various COVID-19 related measures across the United States.<br>
                                <a href='http://idea.rpi.edu/'>The Rensselaer Institute for Data Exploration and Applications</a><br>
                                <a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Statement of Accessibility</a>
                                ")
                           ),
                    # column(3, 
                    #        HTML("<b>DATA SOURCES:</b>
                    #             <a href='http://bit.ly/39PMWpD'>JHU CSSE</a>;
                    #             <a href='https://bit.ly/2JRhDiX'>COVID Tracking Project</a>;
                    #             <a href='https://bit.ly/3aXpBmD'>Organisation for Economic Co-operation and Development</a>; 
                    #             <a href='https://bit.ly/2V0CYLU'>Kaiser Family Foundation</a>;
                    #             <a href='https://bit.ly/2V1Zl3I'>CDC</a>;
                    #             <a href='https://bit.ly/34mYLBP'>County Health Rankings</a>;
                    #             <a href='https://on.ny.gov/39VXuCO'>heath.data.ny.gov</a>
                    #             ")
                    #        ),
                    column(3, 
                           HTML("<b>LINKS:</b>
                                <a href='https://github.com/TheRensselaerIDEA/COVID-DI-Prototype'>COVIDMINDER github</a>
                                ")
                           )
                    )
  )
)
#### Server Code ####
server <- function(input, output, session) {
  
  # Render leaflet plot with all information in hover
  output$map.testing <- renderLeaflet({
    # colors <- c("#426C85","#67a9cf","#d1e5f0","#f7f7f7","#fddbc7","#ef8a62","#b2182b")
    # bins <- c(5, 2, 1, .2, -.2, -1, -2, -5)
    colors <- c("#253494","#4575B4", "#74ADD1","#ABD9E9","white","#FDAE61","#F46D43", "#D73027", "#BD0026")
    bins <- c(5, 3, 2, 1, .2, -.2, -1, -2, -3, -5)
    pal2 <- leaflet::colorBin(colors, domain = states$tests_ldi, bins = bins, reverse=FALSE)
#    browser()
    labels2 <- sprintf(
      "<strong>%s</strong> State<br/>
      Testing Rate vs South Korea DI: %.2g<br>
      Testing Rate: %.1f /1000",
      states$NAME, states$tests_ldi, states$tests_per_1000*1000
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
    colors <- c("#253494","#4575B4", "#74ADD1","#ABD9E9","white","#FDAE61","#F46D43", "#D73027", "#BD0026")
    bins <- c(5, 3, 2, 1, .2, -.2, -1, -2, -3, -5)
    pal2 <- leaflet::colorBin(colors, domain = states$cardio_death_rate_ldi, bins = bins, reverse=FALSE)
    labels2 <- sprintf(
      "<strong>%s</strong><br/>
      Cardio Mortality Rate DI: %.2g<br/>
      Cardio Mortality Rate: %.1f /100K",
      states$NAME, states$cardio_death_rate_ldi, states$cardio_deaths_p_100000*100000
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

  output$map.diabetes <- renderLeaflet({
    colors <- c("#253494","#4575B4", "#74ADD1","#ABD9E9","white","#FDAE61","#F46D43", "#D73027", "#BD0026")
    bins <- c(5, 3, 2, 1, .2, -.2, -1, -2, -3, -5)
    pal2 <- leaflet::colorBin(colors, domain = states$diabetes_rate_ldi, bins = bins, reverse=FALSE)
    labels2 <- sprintf(
      "<strong>%s</strong><br/>
      Diabetes Percentage DI: %.2g<br/>
      Diabetes Percentage: %.1f pct",
      states$NAME, states$diabetes_rate_ldi, states$pct_Adults_with_Diabetes
    ) %>% lapply(htmltools::HTML)
    
    leaflet(states.shapes) %>%
      setView(-96, 37.8, 4) %>% 
      addPolygons(
        fillColor = ~pal2(states$diabetes_rate_ldi),
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
      addLegend(pal = pal2, values = ~states$diabetes_rate_ldi, opacity = 0.7, title = "Disparity Index<br/>US Diabetes Rate",
                position = "bottomright") %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    #Remove personal API key
  })
  
  output$map.hospital <- renderLeaflet({
    # colors <- c("#426C85","#67a9cf","#d1e5f0","#f7f7f7","#fddbc7","#ef8a62","#b2182b")
    # bins <- c(5, 2, 1, .2, -.2, -1, -2, -5)
    colors <- c("#253494","#4575B4", "#74ADD1","#ABD9E9","white","#FDAE61","#F46D43", "#D73027", "#BD0026")
    bins <- c(5, 3, 2, 1, .2, -.2, -1, -2, -3, -5)
    pal2 <- leaflet::colorBin(colors, domain = states$hosp_beds_ldi, bins = bins, reverse=FALSE)
    labels2 <- sprintf(
      "<strong>%s</strong><br/>
      Hospital Beds vs Italy DI: %.2g",
      states$NAME, states$hosp_beds_ldi
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
    # colors <- c("#426C85","#67a9cf","#d1e5f0","#f7f7f7","#fddbc7","#ef8a62","#b2182b")
    # bins <- c(5, 2, 1, .2, -.2, -1, -2, -5,-Inf)
    colors <- c("#253494","#4575B4", "#74ADD1","#ABD9E9","white","#FDAE61","#F46D43", "#D73027", "#BD0026")
    bins <- c(5, 3, 2, 1, .2, -.2, -1, -2, -3, -5)
    pal2 <- leaflet::colorBin(colors, domain = states$death_rate_ldi, bins = bins, reverse=FALSE)
#    browser() 
    labels2 <- sprintf(
      "<strong>%s</strong><br/>
      COVID-19 Mortality Rate DI: %.2g<br>
      COVID-19 Mortality Rate: %.1f /100k",
      states$NAME, states$death_rate_ldi, states$covid_death_rate*100000
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
  
  output$map.NY.deaths <- renderLeaflet({
    #colors <- c("#426C85","#67a9cf","#d1e5f0","#f7f7f7","#fddbc7","#ef8a62","#b2182b")
    #bins <- c(5, 2, 1, .2, -.2, -1, -2, -5,-Inf)
    colors <- c("#253494","#4575B4", "#74ADD1","#ABD9E9","white","#FDAE61","#F46D43", "#D73027", "#BD0026")
    bins <- c(5, 3, 2, 1, .2, -.2, -1, -2, -3, -5)
    
    pal2 <- leaflet::colorBin(colors, domain = NY.data$death_rate_ldi, bins = bins, reverse=FALSE)
    
    NY.shape$county_fips <- paste(as.data.frame(NY.shape)$STATEFP, as.data.frame(NY.shape)$COUNTYFP, sep = '')
    NY.data <- dplyr::left_join(as.data.frame(NY.shape), as.data.frame(NY.data), by = c("county_fips" = "FIPS"))
    
    labels <- sprintf(
      "<strong>%s</strong><br/>
      COVID-19 Mortality Rate DI: %.2g<br>
      COVID-19 Mortality Rate: %.1f /100k",
      NY.data$County, NY.data$death_rate_ldi, (NY.data$deaths/NY.data$Population)*100000
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
  
  output$map.NY.cases <- renderLeaflet({
    # colors <- c("#426C85","#67a9cf","#d1e5f0","#f7f7f7","#fddbc7","#ef8a62","#b2182b")
    # bins <- c(5, 2, 1, .2, -.2, -1, -2, -5,-Inf)
    colors <- c("#253494","#4575B4", "#74ADD1","#ABD9E9","white","#FDAE61","#F46D43", "#D73027", "#BD0026")
    bins <- c(5, 3, 2, 1, .2, -.2, -1, -2, -3, -5)
    pal2 <- leaflet::colorBin(colors, domain = NY.data$case_rate_ldi, bins = bins, reverse=FALSE)
    
    NY.shape$county_fips <- paste(as.data.frame(NY.shape)$STATEFP, as.data.frame(NY.shape)$COUNTYFP, sep = '')
    NY.data <- dplyr::left_join(as.data.frame(NY.shape), as.data.frame(NY.data), by = c("county_fips" = "FIPS"))
    
    labels <- sprintf(
      "<strong>%s</strong><br/>
      COVID-19 Case Rate DI: %.2g<br>
      COVID-19 Case Rate: %.1f /100k",
      NY.data$County, NY.data$case_rate_ldi, (NY.data$cases/NY.data$Population)*100000
    ) %>% lapply(htmltools::HTML)
    
    leaflet(NY.shape) %>%
      setView(-76.071782, 42.991989, 7) %>%  # Set to the geographic center of NY
      addPolygons(
        fillColor = ~pal2(NY.data$case_rate_ldi),
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
                values = ~NY.data$case_rate_ldi, 
                opacity = 0.7, 
                title = "Disparity Index<br/>NY COVID-19 Cases",
                position = "bottomright"
      ) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light",
        accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
    #Remove personal API key
  })
  
  output$map.NY.diabetes <- renderLeaflet({
    # colors <- c("#426C85","#67a9cf","#d1e5f0","#f7f7f7","#fddbc7","#ef8a62","#b2182b")
    # bins <- c(5, 2, 1, .2, -.2, -1, -2, -5,-Inf)
    colors <- c("#253494","#4575B4", "#74ADD1","#ABD9E9","white","#FDAE61","#F46D43", "#D73027", "#BD0026")
    bins <- c(5, 3, 2, 1, .2, -.2, -1, -2, -3, -5)
    pal2 <- leaflet::colorBin(colors, domain = NY.data$diabetes_ldi, bins = bins, reverse=FALSE)
    
    NY.shape$county_fips <- paste(as.data.frame(NY.shape)$STATEFP, as.data.frame(NY.shape)$COUNTYFP, sep = '')
    NY.data <- dplyr::left_join(as.data.frame(NY.shape), as.data.frame(NY.data), by = c("county_fips" = "FIPS"))
    
    labels <- sprintf(
      "<strong>%s</strong><br/>
      Diabetes Rate DI: %.2g<br>
      Diabetes Rate: %.1f pct",
      NY.data$County, NY.data$diabetes_ldi, NY.data$pct_Adults_with_Diabetes
    ) %>% lapply(htmltools::HTML)
    
    leaflet(NY.shape) %>%
      setView(-76.071782, 42.991989, 7) %>%  # Set to the geographic center of NY
      addPolygons(
        fillColor = ~pal2(NY.data$diabetes_ldi),
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
                values = ~NY.data$diabetes_ldi, 
                opacity = 0.7, 
                title = "Disparity Index<br/>NY Diabetes Rates",
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
