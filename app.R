#### Library and Data Imports ####
source("modules/Source.R")
source("modules/data_load.R")
source("modules/preprocessing.R")

# Leaving this in case we need it
# TODO: Implement other text as strings like this...
rpi_accessibility_link <- "<div class='center'><p><a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Statement of Accessibility</a></p></div>"

footer_text <- "<br><span style='font-size: 80%;'><b>COVIDMINDER analysis and visualizations</b> by students and staff
                                of <a href='http://idea.rpi.edu/'>The Rensselaer Institute for Data Exploration 
                                and Applications</a> at <a href='http://rpi.edu/'>Rensselaer Polytechnic Institute</a>. 
                                <b>COVIDMINDER</b> is an open source project; see the 
                                <a href='https://github.com/TheRensselaerIDEA/COVIDMINDER'>COVIDMINDER github</a>
                                for more information. 
                                <i><a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Statement 
                                of Accessibility</a></i></span>"

#### UI Code ####
ui <- 
  tagList(
    tags$head(
      tags$title("COVIDMINDER: Where you live matters") 
    ),
  navbarPage(
  theme="style.css",
  title=tags$div(class="title-text",
            img(class="logo", src="Rensselaer_round.png"),
            HTML("COVID<b>MINDER</b>")),
  tabPanel(tags$div(class="tab-title",style="text-align:center;", #For some reason, unresponsive to class
                    HTML("<span style='font-size: 80%;'><b>OUTCOME (USA)</b></br>Mortality Rate</span>")),
           sidebarLayout(
             sidebarPanel(
             HTML("<strong>Outcome: What are the disparities between states  in  rates of COVID-19 deaths per 100k population 
                   as compared to the average USA rate of COVID-19 deaths per 100k population? </strong><br><br>
                    <span style='font-size: 90%;'>
                    <strong>Outcome</strong> 
                    disparities depend on the spread of the virus, social <strong>determinants</strong> that put the population 
                    at risk, and <strong>mediations</strong> used/available to combat the virus.<br><br>
                    The  rate of COVID-19 deaths per 100k in a state is: <br>
                    <ul>
                      <li>
                        <span style='color:#b2182b;font-weight:bold;'>Higher than US average rate for disparity index &gt; 0.2 (RED)</span>
                      </li>
                      <li>
                        <span style='color:white;font-weight:bold;'>About equal to US average rate for -0.2 &lt; disparity index &lt; 0.2 (WHITE)</span>
                      </li>
                      <li>
                        <span style='color:#253494;font-weight:bold;'>Lower than US average rate for disparity index &lt; -0.2 (BLUE)</span>
                      </li>
                    </ul>
                    Darker colors indicate greater disparity.<br><br>
                    
                    <strong>Mortality Rate</strong> = number of COVID-19 deaths per 100K population<br>
                    <strong>Death Rate Disparity Index</strong> = log(Mortality Rate  in state/mean Mortality Rate of US)<br>
                    <strong>Date:</strong> 04/09/2020<br><br>

                    <b>DATA SOURCE:</b> <a href='http://bit.ly/39PMWpD'>JHU CSSE (daily)</a><br>
                    </span>
                    "),
             HTML(footer_text),
             width=4),
             mainPanel(tags$h4(class="map-title", "COVID-19 Mortality Rate Disparities by State Compared to Average US Rate"),
                        leafletOutput(outputId = "map.covid_deaths", height="100%"), width=8)
           )
  ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<span style='font-size: 80%;'><b>MEDIATION (USA)</b></br>COVID-19 Testing</span>")),
           sidebarLayout(fluid=FALSE,
             sidebarPanel(HTML("<strong>Mediation: What are the disparities between states  in  rates of COVID-19 testing per 100k population 
                              as compared to the South Korean rate of COVID-19 testing per 100k population? </strong><br><br>
                              <span style='font-size: 90%;'>
                              Disparity in <strong>Outcomes</strong> like COVID-like deaths depend on the spread of the virus, social 
                              <strong>determinants</strong> that put the population at increased risk, and <strong>mediations</strong>
                              used/available to combat the virus. South Korea is used as the testing reference rate since South 
                              Korea successfully used testing to “flatten the curve”.<br><br>
                               The rate of testing per 100k in a state is: <br>
                               <ul>
                                 <li>
                                 <span style='color:#b2182b;font-weight:bold;'>Lower than South Korean testing rate for  disparity index &gt; 0.2 (RED)</span>
                                 </li>
                                 <li>
                                 <span style='color:white;font-weight:bold;'>About equal to South Korean testing rate for  -0.2 &lt; disparity index &lt; 0.2 (WHITE)</span>
                                 </li>
                                 <li>
                                 <span style='color:#253494;font-weight:bold;'>Higher than South Korean testing rate for disparity index &lt; -0.2 (BLUE)</span>
                                 </li>
                               </ul>
                               Darker colors indicate greater disparity.<br><br>
                               
                               <strong>Testing Rate</strong> = number of COVID-19 tests per 100K population <br>
                               <strong>Testing Rate Disparity Index</strong> = -log(Testing Rate  in state/Testing Rate in South Korea) <br>
                               <strong>Date:</strong> 04/09/2020 <br><br>
                               
                               <b>DATA SOURCE:</b> <a href='http://bit.ly/39PMWpD'>JHU CSSE (daily)</a><br>
                               </span>"),
                          HTML(footer_text),
                          width=4),
             
             mainPanel(tags$h4(class="map-title", "COVID-19 Testing Rate Disparities by State Compared to Average South Korean Rate"),
                       leafletOutput(outputId = "map.testing", height="100%"), width=8)
           )
  ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<span style='font-size: 80%;'><b>MEDIATION (USA)</b></br>Hospital Beds</span>")),
           sidebarLayout(
             sidebarPanel(HTML("<strong>Mediation: What are the disparities between states  in  the rate of hospital beds 
                                per 100k population as compared to the  rate of hospital 
                                beds per 100k population  in Italy? </strong><br><br>
                                <span style='font-size: 90%;'>
                                Disparity in <strong>Outcomes</strong> like COVID-like deaths depend on the spread of the virus,
                                social <strong>determinants</strong> that put the population at increased risk, and 
                                <strong>mediations</strong> used/available to combat the virus. Italy has a higher hospital 
                                bed rate than the US, yet still faced challenges meeting peak COVID bed needs. Thus we use 
                                Italy’s rate as a minimum target rate.<br><br>
                                The rate of hospital beds per 100k in a state is<br>
                               <ul>
                                <li>
                                 <span style='color:#b2182b;font-weight:bold;'>Lower than Italian rate for disparity index &gt; 0.2 (RED)</span>
                                 </li>
                                 <li>
                                 <span style='color:white;font-weight:bold;'>About equal to Italian rate for -0.2 &lt;disparity index &lt; 0.2 (WHITE)</span>
                                 </li>
                                 <li>
                                 <span style='color:#253494;font-weight:bold;'>Higher than Italian rate for disparity index &lt; -0.2 (BLUE)</span>
                                 </li>
                               </ul>
                               Darker colors indicate greater disparity.<br><br>
                               
                               <strong>Testing Rate</strong> = number of COVID-19 tests per 100K population <br>
                               <strong>Testing Rate Disparity Index</strong> = -log(Testing Rate  in state/Testing Rate in Italy) <br>
                               <strong>Date:</strong> 04/09/2020 <br><br>
                               
                               <b>DATA SOURCE:</b> <a href='https://bit.ly/2V0CYLU'>Kaiser Family Foundation</a><br>

                               </span>"),
                          HTML(footer_text),
                          width=4),
             
             mainPanel(tags$h4(class="map-title", "COVID-19 Hospital Bed Rate Disparities by State Compared to Average Italian Rate"),
                       leafletOutput(outputId = "map.hospital", height="100%"), width=8)
           )
  ),

  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<span style='font-size: 80%;'><b>DETERMINANT (USA)</b></br>Diabetes</span>")),
           sidebarLayout(
             sidebarPanel(HTML("<strong>Determinant: What are the disparities between states in rate of diabetes patients 
                                per 100k population per state as compared to the average United States rate 
                                of diabetes  per 100k population? </strong><br><br>
                                <span style='font-size: 90%;'>
                                <strong>Outcome</strong> disparities depend on the spread of the virus, social 
                                <strong>determinants</strong> that put the population at increased risk, and 
                                <strong>mediations</strong> used/available to combat the virus. Since diabetes puts patients 
                                at increased risk of contracting and dying from COVID-19, areas with higher diabetes rates 
                                may face increased COVID-19 burdens. <br><br>
                               The  rate of diabetes deaths per 100k in a state is<br>
                               <ul>
                               <li>
                               <span style='color:#b2182b;font-weight:bold;'>Higher than US average rate for disparity index &gt; 0.2 (RED)</span>
                               </li>
                               <li>
                               <span style='color:white;font-weight:bold;'>About equal to US average rate for -0.2 &lt;disparity index &lt; 0.2 (WHITE)</span>
                               </li>
                               <li>
                               <span style='color:#253494;font-weight:bold;'>Lower than US average rate for disparity index &lt; -0.2 (BLUE)</span>
                               </li>
                               </ul>
                               Darker colors indicate greater disparity.<br><br>
                               
                               <strong>Diabetes Rate</strong> = number of diabetic patients per 100K population <br>
                               <strong>Diabetes Disparity Index</strong> = log(Diabetes Rate in state/average Diabetes Rate in US)<br>
                               <strong>Date:</strong> 2016<br><br>
                               
                               <b>DATA SOURCE:</b> <a href='https://bit.ly/34mYLBP'>County Health Rankings</a> and 
                                  <a href='https://bit.ly/2V1Zl3I'>CDC</a><br>
                          </span>"),
                          HTML(footer_text),
                          width=4),
             
             mainPanel(tags$h4(class="map-title", "COVID-19 Cardiovascular Mortality Rates by State Compared to Average US Rate"),
                       leafletOutput(outputId = "map.diabetes", height="100%"), width=8)
           )
  ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<span style='font-size: 80%;'><b>OUTCOME (NY)</b></br>Mortality Rate</span>")),
           sidebarLayout(
             sidebarPanel(HTML("<strong>Outcome: What are the disparities between counties of New York
                                in rates of COVID-19 deaths per 100k population as compared to the average USA rate?</strong> <br>  
                                <span style='font-size: 90%;'>
                                <strong>Outcome</strong> disparities depend on the spread of the virus, social 
                                <strong>determinants</strong> that put the population at risk, and <strong>mediations</strong> 
                                used/available to combat the virus.<br><br>
                               The rate of COVID-19 deaths per 100k in a county is<br>
                               <ul>
                               <li>
                               <span style='color:#b2182b;font-weight:bold;'>Higher than US average rate for disparity index &gt; 0.2 (RED)</span>
                               </li>
                               <li>
                               <span style='color:white;font-weight:bold;'>About equal to US average rate for -0.2 &lt;disparity index &lt; 0.2 (WHITE)</span>
                               </li>
                               <li>
                               <span style='color:#253494;font-weight:bold;'>Lower than US average rate for disparity index &lt; -0.2 (BLUE)</span>
                               </li>
                               </ul>
                               Darker colors indicate greater disparity.<br><br>
                               
                               <strong>Mortality Rate</strong> = number of COVID-19 deaths per 100K population<br>
                               <strong>Death Rate Disparity Index</strong> = log(Mortality Rate in state/mean Mortality Rate in US)<br>
                               <strong>Date:</strong> 04/09/2020 (updated daily) <br><br>
                               
                               <b>DATA SOURCE:</b> <a href='http://bit.ly/39PMWpD'>JHU CSSE (daily)</a> and 
                               <a href='https://on.ny.gov/2yOj1AD'>New York State Dept. of Health COVID19Tracker (daily)</a><br>

                               </span>"),
                          HTML(footer_text),
                          width=4),
             
             mainPanel(tags$h4(class="map-title", "COVID-19 Mortality Rate Disparities by County in New York Compared to Average US Rate"),
                       leafletOutput(outputId = "map.NY.deaths", height="100%"), width=8)
           )
  ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<span style='font-size: 80%;'><b>OUTCOME (NY)</b></br>COVID-19 Cases</span>")),
           sidebarLayout(
             sidebarPanel(HTML("<strong>Outcome: What are the disparities between New York counties in the rate of COVID-19 
                                cases per 100k population as compared to the average United States 
                                rate of COVID-19 cases per 100k population?  </strong> <br><br>
                                <span style='font-size: 90%;'>
                                <strong>Outcome</strong> disparities depend on the spread of the virus, social 
                                <strong>determinants</strong> that put the population at risk, and <strong>mediations</strong> 
                                used/available to combat the virus. <br><br>
                               The rate of COVID-19 deaths per 100k in a county is<br>
                               <ul>
                               <li>
                               <span style='color:#b2182b;font-weight:bold;'>Higher than US average rate for disparity index &gt; 0.2 (RED)</span>
                               </li>
                               <li>
                               <span style='color:#000000'>About equal to US average rate for -0.2 &lt;disparity index &lt; 0.2 (WHITE)</span>
                               </li>
                               <li>
                               <span style='color:#253494;font-weight:bold;'>Lower than US average rate for disparity index &lt; -0.2 (BLUE)</span>
                               </li>
                               </ul>
                               Darker colors indicate greater disparity.<br><br>
                               
                               <strong>Mortality Rate</strong> = number of COVID-19 deaths per 100K population<br>
                               <strong>Death Rate Disparity Index</strong> = log (COVID-19 Case Rate in state/mean COVID_19 Case Rate in US) <br>
                               <strong>Date:</strong> 04/09/2020 (updated daily) <br><br>
                               
                               <b>DATA SOURCE:</b> <a href='https://on.ny.gov/39VXuCO'>heath.data.ny.gov (daily)</a><br>
                          </span>"),
                          HTML(footer_text),
                          width=4),
             
             mainPanel(tags$h4(class="map-title", "COVID-19 Case Rate Disparities by County in New York  Compared to Average US Rate"),
                       leafletOutput(outputId = "map.NY.cases", height="100%"), width=8)
           )
  ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<span style='font-size: 80%;'><b>DETERMINANT (NY)</b></br>Diabetes</span>")),
           sidebarLayout(
             sidebarPanel(HTML("<strong>Determinant: What are the disparities between New York counties in the rate 
                                of diabetes patients per 100k population as compared to the average United 
                                States rate of diabetes  per 100k population?</strong><br><br>
                                <span style='font-size: 90%;'>
                                <strong>Outcome</strong> disparities depend on the spread of the virus, social 
                                <strong>determinants</strong> that put the population at increased risk, and 
                                <strong>mediations</strong> used/available to combat the virus.Since diabetes puts patients 
                                at increased risk of contracting and dying from COVID-19, areas with higher diabetes rates may 
                                face increased COVID-19 burdens.<br><br>
                               The  rate of diabetes patients per 100k in county  is<br>
                               <ul>
                               <li>
                               <span style='color:#b2182b;font-weight:bold;'>Higher than US average rate for disparity index &gt; 0.2 (RED)</span>
                               </li>
                               <li>
                               <span style='color:white;font-weight:bold;'>About equal to US average rate for -0.2 &lt; disparity index &lt; 0.2 (WHITE)</span>
                               </li>
                               <li>
                               <span style='color:#253494;font-weight:bold;'>Lower than US average rate for disparity index &lt; -0.2 (BLUE)</span>
                               </li>
                               </ul>
                               Darker colors indicate greater disparity.<br><br>
                               
                               <strong>Diabetes Rate</strong> = number of diabetic patients  per 100K population <br>
                               <strong>Diabetes Disparity Index</strong> = log(Diabetes Rate in state/average Diabetes Rate in US)<br>
                               <strong>Date:</strong> 2016<br><br>
                               
                               <b>DATA SOURCE:</b> <a href='https://bit.ly/34mYLBP'>County Health Rankings</a> and 
                                  <a href='https://bit.ly/2V1Zl3I'>CDC</a><br>

                               </span>"),
                          HTML(footer_text),
                          width=4),
             
             mainPanel(tags$h4(class="map-title", "COVID-19 Diabetes Rates by County in New York Compared to Average US Rate"),
                       leafletOutput(outputId = "map.NY.diabetes", height="100%"), width=8)
           )
  )
  # ,
  # footer = fluidRow(class = "navbar navbar-default footer", 
  #                   column(10,
  #                          HTML("<b>COVIDMINDER analysis and visualizations</b> by students and staff
  #                               of <a href='http://idea.rpi.edu/'>The Rensselaer Institute for Data Exploration 
  #                               and Applications</a> at <a href='http://rpi.edu/'>Rensselaer Polytechnic Institute</a>. 
  #                               <b>COVIDMINDER</b> is an open source project; see the 
  #                               <a href='https://github.com/TheRensselaerIDEA/COVIDMINDER'>COVIDMINDER github</a>
  #                               for more information. 
  #                               <i><a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Statement 
  #                               of Accessibility</a></i>
  #                               ")
  #                          )
  #                   )
  )
)
#### Server Code ####
server <- function(input, output, session) {
  
  # Render leaflet plot with all information in hover
  output$map.testing <- renderLeaflet({
    
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
    
    colors <- c("#253494","#4575B4", "#74ADD1","#ABD9E9","white","#FDAE61","#F46D43", "#D73027", "#BD0026")
    bins <- c(5, 3, 2, 1, .2, -.2, -1, -2, -3, -5)
    pal2 <- leaflet::colorBin(colors, domain = states$death_rate_ldi, bins = bins, reverse=FALSE)

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
