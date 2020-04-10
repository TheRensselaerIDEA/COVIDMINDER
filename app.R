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
      tags$title("COVIDMINDER: Where you live matters") 
    ),
  navbarPage(
  theme="style.css",
  title=tags$div(class="title-text",
            img(class="logo", src="Rensselaer_round.png"),
            HTML("COVID<b>MINDER</b>")),
  tabPanel(tags$div(class="tab-title",style="text-align:center;", #For some reason, unresponsive to class
                    HTML("<b>OUTCOME (USA)</b></br>Mortality Rate")),
           sidebarLayout(
             sidebarPanel(
             HTML("<strong>Outcome Disparity:</strong> The rate of COVID-19 deaths per 100k population per state varies greatly
                    as compared to the mean United States rate of COVID-19 deaths per 100k population. <strong>Outcome</strong> 
                    disparities depend on the spread of the virus, social <strong>determinants</strong> that put the population 
                    at risk, and <strong>mediations</strong> used/available to combat the virus.<br><br>
                    The  rate of covid-19 deaths per 100k in a state is: <br>
                    <ul>
                      <li>
                        Higher than US average rate for disparity index > 0.2 <span style='color:#b2182b'>(RED)</span>
                      </li>
                      <li>
                        About US average rate for -0.2 <disparity index < 0.2 <span style='color:#000000'>(WHITE)</span>
                      </li>
                      <li>
                        Lower than US average rate for disparity index < -0.2 <span style='color:#253494'>(BLUE)</span>
                      </li>
                    </ul>
                    Darker colors indicate more disparity.<br><br>
                    
                    <strong>Mortality Rate</strong> = number of COVID-19 deaths per 100K population<br>
                    <strong>Death Rate Disparity Index</strong> = log(Mortality Rate  in state/mean Mortality Rate of US)<br>
                    <strong>Date:</strong> 04/09/2020<br><br>

                    <b>DATA SOURCE:</b> <a href='http://bit.ly/39PMWpD'>JHU CSSE (daily)</a><br>
                    <b>ANALYSIS:</b> <a href='https://www.rpi.edu/'>Rensselaer Polytechnic Institute</a><br>
                    <a href='https://idea.rpi.edu/'>Institute for Data Application and Explorations (IDEA)</a><br>
                    <strong>LINKS:</strong> <a href='https://github.com/TheRensselaerIDEA/COVID-DI-Prototype'>COVIDMinder Github</a><br>
                      <a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Accessibility Statement<a><br>
                      <a href='https://github.com/TheRensselaerIDEA/COVID-DI-Prototype/wiki'>More information</a><br>"),
             #HTML(rpi_accessibility_link), 
             width=4),
             mainPanel(tags$h3(class="map-title", "COVID-19 Mortality Rate Disparities by State Compared to Average US Rate"),
                        leafletOutput(outputId = "map.covid_deaths", height="100%"), width=8)
           )
  ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<b>MEDIATION (USA)</b></br>COVID-19 Testing")),
           sidebarLayout(fluid=FALSE,
             sidebarPanel(HTML("<strong>Mediation Disparity:</strong> The rate of COVID-19 per 100k population per state varies
                              greatly  as compared to the South Korean rate of COVID-19 testing per 100k population. Disparity 
                              in <strong>Outcomes</strong> like COVID-like deaths depend on the spread of the virus, social 
                              <strong>determinants</strong> that put the population at increased risk, and <strong>mediations</strong>
                              used/available to combat the virus. South Korea is used as the testing reference rate since South 
                              Korea successfully used testing to “flatten the curve”.<br><br>
                               The rate of testing per 100k in a state is: <br>
                               <ul>
                                 <li>
                                 Lower than South Korean testing rate for  disparity index > 0.2 <span style='color:#b2182b'>(RED)</span>
                                 </li>
                                 <li>
                                 About equal South Korean testing rate for  -0.2 <disparity index < 0.2 <span style='color:#000000'>(WHITE)</span>
                                 </li>
                                 <li>
                                 Higher than South Korean testing rate for disparity index < -0.2 <span style='color:#253494'>(BLUE)</span>
                                 </li>
                               </ul>
                               Darker colors indicate more disparity.<br><br>
                               
                               <strong>Testing Rate</strong> = number of COVID-19 tests per 100K population <br>
                               <strong>Testing Rate Disparity Index</strong> = -log(Testing Rate  in state/Testing Rate in South Korea) <br>
                               <strong>Date:</strong> 04/09/2020 <br><br>
                               
                               <b>DATA SOURCE:</b> <a href='http://bit.ly/39PMWpD'>JHU CSSE (daily)</a><br>
                               <b>ANALYSIS:</b> <a href='https://www.rpi.edu/'>Rensselaer Polytechnic Institute</a><br>
                               <a href='https://idea.rpi.edu/'>Institute for Data Application and Explorations (IDEA)</a><br>
                               <strong>LINKS:</strong> <a href='https://github.com/TheRensselaerIDEA/COVID-DI-Prototype'>COVIDMinder Github</a><br>
                               <a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Accessibility Statement<a><br>
                               <a href='https://github.com/TheRensselaerIDEA/COVID-DI-Prototype/wiki'>More information</a><br>"),
                          #HTML(rpi_accessibility_link), 
                          width=4),
             mainPanel(tags$h3(class="map-title", "COVID-19 Testing Rate Disparities by State Compared to Average South Korean Rate"),
                       leafletOutput(outputId = "map.testing", height="100%"), width=8)
           )
  ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<b>MEDIATION (USA)</b></br>Hospital Beds")),
           sidebarLayout(
             sidebarPanel(HTML("<strong>Mediation Disparity:</strong> This map shows that the rate of hospital beds 
                                per 100k population across the states varies greatly as compared to the  rate of hospital 
                                beds per 100k population  in Italy. Disparity in <strong>Outcomes</strong> like COVID-like deaths depend on the spread of the virus,
                                social <strong>determinants</strong> that put the population at increased risk, and 
                                <strong>mediations</strong> used/available to combat the virus. Italy has a higher hospital 
                                bed rate than the US, yet still faced challenges meeting peak COVID bed needs. Thus we use 
                                Italy’s rate as a minimum target rate.<br><br>
                                The rate of hospital beds per 100k in a state is<br>
                               <ul>
                                <li>
                                 Lower than Italian rate for disparity index > 0.2 <span style='color:#b2182b'>(RED)</span>
                                 </li>
                                 <li>
                                 About equal to Italian rate for -0.2 <disparity index < 0.2 <span style='color:#000000'>(WHITE)</span>
                                 </li>
                                 <li>
                                 Higher than Italian rate for disparity index < -0.2 <span style='color:#253494'>(BLUE)</span>
                                 </li>
                               </ul>
                               Darker colors indicate more disparity.<br><br>
                               
                               <strong>Testing Rate</strong> = number of COVID-19 tests per 100K population <br>
                               <strong>Testing Rate Disparity Index</strong> = -log(Testing Rate  in state/Testing Rate in Italy) <br>
                               <strong>Date:</strong> 04/09/2020 <br><br>
                               
                               <b>DATA SOURCE:</b> <a href='https://bit.ly/2V0CYLU'>Kaiser Family Foundation</a><br>
                               <b>ANALYSIS:</b> <a href='https://www.rpi.edu/'>Rensselaer Polytechnic Institute</a><br>
                               <a href='https://idea.rpi.edu/'>Institute for Data Application and Explorations (IDEA)</a><br>
                               <strong>LINKS:</strong> <a href='https://github.com/TheRensselaerIDEA/COVID-DI-Prototype'>COVIDMinder Github</a><br>
                               <a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Accessibility Statement<a><br>
                               <a href='https://github.com/TheRensselaerIDEA/COVID-DI-Prototype/wiki'>More information</a><br>"),
                          #HTML(rpi_accessibility_link), 
                          width=4),
             mainPanel(tags$h3(class="map-title", "COVID-19 Hospital Bed Rate Disparities by State Compared to Average Italian Rate"),
                       leafletOutput(outputId = "map.hospital", height="100%"), width=8)
           )
  ),
  # DON"T DELETE! We may restore this or re-purpose
  # tabPanel(tags$div(class="tab-title",style="text-align:center;",
  #                   HTML("<b>DETERMINANT (USA)</b></br>Cardiovascular Diseases")),
  #          sidebarLayout(
  #            sidebarPanel(HTML("<strong>Determinant Disparity:</strong> This map shows that the deaths due to cardiovascular 
  #                               disease per 100k population per state varies greatly as compared to the average United States 
  #                               rate of cardiovascular disease rates  per 100k population. <strong>Outcome</strong> disparities depend on the spread of the virus, social 
  #                               <strong>determinants</strong> that put the population at increased risk, and <strong>mediations</strong>
  #                               used/available to combat the virus. Since Cardiovascular Disease puts patients at increased 
  #                               risk of contracting and dying from COVID-19, areas with higher cardiovascular mortality rates 
  #                               may face increased COVID-19 burdens.<br><br>
  #                              The  rate of covid-19 deaths per 100k in a state is<br>
  #                              <ul>
  #                                <li>
  #                                Higher than US average rate for disparity index > 0.2 <span style='color:#b2182b'>(RED)</span>
  #                                </li>
  #                                <li>
  #                                About equal to US average rate for -0.2 <disparity index < 0.2 <span style='color:#000000'>(WHITE)</span>
  #                                </li>
  #                                <li>
  #                                Lower than US average rate for disparity index < -0.2 <span style='color:#253494'>(BLUE)</span>
  #                                </li>
  #                              </ul>
  #                              Darker colors indicate more disparity.<br><br>
  #                              
  #                              <strong>Cardiovascular Mortality Rate</strong> = number of diabetics individuals per 100K population<br>
  #                              <strong>Death Rate Disparity Index</strong> = log(Cardiovascular Mortality Rate  in state/average Cardiovascular Mortality Rate in US)<br>
  #                              <strong>Date:</strong> 04/09/2020 <br><br>
  #                              
  #                              <b>DATA SOURCE:</b> <a href='http://bit.ly/39PMWpD'>NA</a><br>
  #                              <b>ANALYSIS:</b> <a href='https://www.rpi.edu/'>Rensselaer Polytechnic Institute</a><br>
  #                              <a href='https://idea.rpi.edu/'>Institute for Data Application and Explorations (IDEA)</a><br>
  #                              <strong>LINKS:</strong> <a href='https://github.com/TheRensselaerIDEA/COVID-DI-Prototype'>COVIDMinder Github</a><br>
  #                              <a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Accessibility Statement<a><br>
  #                              <a href='https://github.com/TheRensselaerIDEA/COVID-DI-Prototype/wiki'>More information</a><br>"),
  #                         #HTML(rpi_accessibility_link),
  #                         width=4),
  #            mainPanel(tags$h3(class="map-title", "COVID-19 Diabetes Rates by State Compared to Average US Rate"),
  #                       leafletOutput(outputId = "map.cardio", height="100%"), width=8)
  #          )
  # ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<b>DETERMINANT (USA)</b></br>Diabetes")),
           sidebarLayout(
             sidebarPanel(HTML("<strong>Determinant Disparity:</strong> This map shows that the rate of diabetes patients 
                                per 100k population per state varies greatly as compared to the average United States rate 
                                of diabetes  per 100k population. <strong>Outcome</strong> disparities depend on the spread of the virus, social 
                                <strong>determinants</strong> that put the population at increased risk, and 
                                <strong>mediations</strong> used/available to combat the virus. Since diabetes puts patients 
                                at increased risk of contracting and dying from COVID-19, areas with higher diabetes rates 
                                may face increased COVID-19 burdens. <br><br>
                               The  rate of diabetes deaths per 100k in a state is<br>
                               <ul>
                               <li>
                               Higher than US average rate for disparity index > 0.2 <span style='color:#b2182b'>(RED)</span>
                               </li>
                               <li>
                               About equal to US average rate for -0.2 <disparity index < 0.2 <span style='color:#000000'>(WHITE)</span>
                               </li>
                               <li>
                               Lower than US average rate for disparity index < -0.2 <span style='color:#253494'>(BLUE)</span>
                               </li>
                               </ul>
                               Darker colors indicate more disparity.<br><br>
                               
                               <strong>Diabetes Rate</strong> = number of diabetic patients per 100K population <br>
                               <strong>Diabetes Disparity Index</strong> = log(Diabetes Rate in state/average Diabetes Rate in US)<br>
                               <strong>Date:</strong> 2016<br><br>
                               
                               <b>DATA SOURCE:</b> <a href='https://bit.ly/34mYLBP'>County Health Rankings</a> and 
                                  <a href='https://bit.ly/2V1Zl3I'>CDC</a><br>
                               <b>ANALYSIS:</b> <a href='https://www.rpi.edu/'>Rensselaer Polytechnic Institute</a><br>
                               <a href='https://idea.rpi.edu/'>Institute for Data Application and Explorations (IDEA)</a><br>
                               <strong>LINKS:</strong> <a href='https://github.com/TheRensselaerIDEA/COVID-DI-Prototype'>COVIDMinder Github</a><br>
                               <a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Accessibility Statement<a><br>
                               <a href='https://github.com/TheRensselaerIDEA/COVID-DI-Prototype/wiki'>More information</a><br>"),
                          #HTML(rpi_accessibility_link), 
                          width=4),
             mainPanel(tags$h3(class="map-title", "COVID-19 Cardiovascular Mortality Rates by State Compared to Average US Rate"),
                       leafletOutput(outputId = "map.diabetes", height="100%"), width=8)
           )
  ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<b>OUTCOME (NY)</b></br>Mortality Rate")),
           sidebarLayout(
             sidebarPanel(HTML("<strong>Outcome Disparity:</strong> This map shows that in New York the rate of COVID-19 deaths 
                                per 100k population per county varies greatly as compared to the average United States rate of 
                                COVID-19 deaths per 100k population.  
                                <strong>Outcome</strong> disparities depend on the spread of the virus, social 
                                <strong>determinants</strong> that put the population at risk, and <strong>mediations</strong> 
                                used/available to combat the virus.<br><br>
                               The rate of covid-19 deaths per 100k in a county is<br>
                               <ul>
                               <li>
                               Higher than US average rate for disparity index > 0.2 <span style='color:#b2182b'>(RED)</span>
                               </li>
                               <li>
                               About equal to US average rate for -0.2 <disparity index < 0.2 <span style='color:#000000'>(WHITE)</span>
                               </li>
                               <li>
                               Lower than US average rate for disparity index < -0.2 <span style='color:#253494'>(BLUE)</span>
                               </li>
                               </ul>
                               Darker colors indicate more disparity.<br><br>
                               
                               <strong>Mortality Rate</strong> = number of COVID-19 deaths per 100K population<br>
                               <strong>Death Rate Disparity Index</strong> = log [Mortality Rate  in state/mean Mortality Rate in US)<br>
                               <strong>Date:</strong> 04/09/2020 (updated daily) <br><br>
                               
                               <b>DATA SOURCE:</b> <a href='http://bit.ly/39PMWpD'>JHU CSSE (daily)</a> and 
                               <a href='https://on.ny.gov/2yOj1AD'>New York State Dept. of Health COVID19Tracker (daily)</a><br>
                               <b>ANALYSIS:</b> <a href='https://www.rpi.edu/'>Rensselaer Polytechnic Institute</a><br>
                               <a href='https://idea.rpi.edu/'>Institute for Data Application and Explorations (IDEA)</a><br>
                               <strong>LINKS:</strong> <a href='https://github.com/TheRensselaerIDEA/COVID-DI-Prototype'>COVIDMinder Github</a><br>
                               <a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Accessibility Statement<a><br>
                               <a href='https://github.com/TheRensselaerIDEA/COVID-DI-Prototype/wiki'>More information</a><br>"),
                          #HTML(rpi_accessibility_link), 
                          width=4),
             mainPanel(tags$h3(class="map-title", "COVID-19 Mortality Rate Disparities by County in New York Compared to Average US Rate"),
                       leafletOutput(outputId = "map.NY.deaths", height="100%"), width=8)
           )
  ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<b>OUTCOME (NY)</b></br>COVID-19 Cases")),
           sidebarLayout(
             sidebarPanel(HTML("<strong>Outcome Disparity:</strong> This map shows that in New York the rate of COVID-19 
                                cases per 100k population per county varies greatly as compared to the average United States 
                                rate of COVID-19 cases per 100k population.  
                                <strong>Outcome</strong> disparities depend on the spread of the virus, social 
                                <strong>determinants</strong> that put the population at risk, and <strong>mediations</strong> 
                                used/available to combat the virus. <br><br>
                               The rate of covid-19 deaths per 100k in a county is<br>
                               <ul>
                               <li>
                               Higher than US average rate for disparity index > 0.2 <span style='color:#b2182b'>(RED)</span>
                               </li>
                               <li>
                               About equal to US average rate for -0.2 <disparity index < 0.2 <span style='color:#000000'>(WHITE)</span>
                               </li>
                               <li>
                               Lower than US average rate for disparity index < -0.2 <span style='color:#253494'>(BLUE)</span>
                               </li>
                               </ul>
                               Darker colors indicate more disparity.<br><br>
                               
                               <strong>Mortality Rate</strong> = number of COVID-19 deaths per 100K population<br>
                               <strong>Death Rate Disparity Index</strong> = log (COVID-19 Case Rate in state/mean COVID_19 Case Rate in US) <br>
                               <strong>Date:</strong> 04/09/2020 (updated daily) <br><br>
                               
                               <b>DATA SOURCE:</b> <a href='https://on.ny.gov/39VXuCO'>heath.data.ny.gov (daily)</a><br>
                               <b>ANALYSIS:</b> <a href='https://www.rpi.edu/'>Rensselaer Polytechnic Institute</a><br>
                               <a href='https://idea.rpi.edu/'>Institute for Data Application and Explorations (IDEA)</a><br>
                               <strong>LINKS:</strong> <a href='https://github.com/TheRensselaerIDEA/COVID-DI-Prototype'>COVIDMinder Github</a><br>
                               <a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Accessibility Statement<a><br>
                               <a href='https://github.com/TheRensselaerIDEA/COVID-DI-Prototype/wiki'>More information</a><br>"),
                          #HTML(rpi_accessibility_link), 
                          width=4),
             mainPanel(tags$h3(class="map-title", "COVID-19 Case Rate Disparities by County in New York  Compared to Average US Rate"),
                       leafletOutput(outputId = "map.NY.cases", height="100%"), width=8)
           )
  ),
  tabPanel(tags$div(class="tab-title",style="text-align:center;",
                    HTML("<b>DETERMINANT (NY)</b></br>Diabetes")),
           sidebarLayout(
             sidebarPanel(HTML("<strong>Determinant Disparity:</strong> This map shows that the rate of diabetes patients 
                                per 100k population per county in New York varies greatly as compared to the average United 
                                States rate of diabetes  per 100k population.
                                <strong>Outcome</strong> disparities depend on the spread of the virus, social 
                                <strong>determinants</strong> that put the population at increased risk, and 
                                <strong>mediations</strong> used/available to combat the virus.Since diabetes puts patients 
                                at increased risk of contracting and dying from COVID-19, areas with higher diabetes rates may 
                                face increased COVID-19 burdens.<br><br>
                               The  rate of diabetes patients per 100k in county  is<br>
                               <ul>
                               <li>
                               Higher than US average rate for disparity index > 0.2 <span style='color:#b2182b'>(RED)</span>
                               </li>
                               <li>
                               About equal to US average rate for -0.2 <disparity index < 0.2 <span style='color:#000000'>(WHITE)</span>
                               </li>
                               <li>
                               Lower than US average rate for disparity index < -0.2 <span style='color:#253494'>(BLUE)</span>
                               </li>
                               </ul>
                               Darker colors indicate more disparity.<br><br>
                               
                               <strong>Diabetes Rate</strong> = number of diabetic patients  per 100K population <br>
                               <strong>Diabetes Disparity Index</strong> = log( Diabetes Rate  in state/average Diabetes Rate in US)<br>
                               <strong>Date:</strong> 2016<br><br>
                               
                               <b>DATA SOURCE:</b> <a href='https://bit.ly/34mYLBP'>County Health Rankings</a> and 
                                  <a href='https://bit.ly/2V1Zl3I'>CDC</a><br>
                               <b>ANALYSIS:</b> <a href='https://www.rpi.edu/'>Rensselaer Polytechnic Institute</a><br>
                               <a href='https://idea.rpi.edu/'>Institute for Data Application and Explorations (IDEA)</a><br>
                               <strong>LINKS:</strong> <a href='https://github.com/TheRensselaerIDEA/COVID-DI-Prototype'>COVIDMinder Github</a><br>
                               <a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Accessibility Statement<a><br>
                               <a href='https://github.com/TheRensselaerIDEA/COVID-DI-Prototype/wiki'>More information</a><br>"),
                          #HTML(rpi_accessibility_link), 
                          width=4),
             mainPanel(tags$h3(class="map-title", "COVID-19 Diabetes Rates by County in New York Compared to Average US Rate"),
                       leafletOutput(outputId = "map.NY.diabetes", height="100%"), width=8)
           )
  ),
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
                    column(3, 
                           HTML("<b>LINKS:</b>
                                <a href='https://github.com/TheRensselaerIDEA/COVIDMINDER'>COVIDMINDER github</a>
                                ")
                           )
                    )
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
