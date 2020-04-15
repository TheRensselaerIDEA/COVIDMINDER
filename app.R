#### Library and Data Imports ####
source("modules/Source.R")
source("modules/data_load.R")
source("modules/preprocessing.R")

# Leaving this in case we need it
# TODO: Implement other text as strings like this...
rpi_accessibility_link <- "<div class='center'><p><a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Statement of Accessibility</a></p></div>"

footer_text <- "<br><div style='font-size: 80%;'><b>COVIDMINDER analysis and visualizations</b> by students and staff
                                of <a href='http://idea.rpi.edu/'>The Rensselaer Institute for Data Exploration 
                                and Applications</a> at <a href='http://rpi.edu/'>Rensselaer Polytechnic Institute</a>. 
                                <b>COVIDMINDER</b> is an open source project implemented on the <a href='https://shiny.rstudio.com/'>R Shiny platform</a>;
                                see the <a href='https://github.com/TheRensselaerIDEA/COVIDMINDER'>COVIDMINDER github</a>
                                for more information. <br><br>
                                Thanks for using <b>COVIDMINDER!</b> Please take a few moments 
                                to fill out our short <a href='https://forms.gle/8LwiYAVXXN7mu9wR6'>comments form.</a><br>
                                <i><a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Statement 
                                of Accessibility</a></i></div>"

whatisit_text <-"<div style='font-size:80%;line-height:1.3;'><strong>COVIDMINDER</strong> reveals the regional disparities 
                                in outcomes, determinants, and mediations of the COVID-19 pandemic. Outcomes are the direct 
                                effects of COVID-19. Social and Economic Determinants are pre-existing risk factors that impact 
                                COVID-19 outcomes. Mediations are resources and programs used to combat the pandemic.</div><br>"

comments_link <-"<div style='font-size:80%;line-height:1.3;'>Thanks for using <b>COVIDMINDER!</b> Please take a few moments to fill out our short <a href='https://forms.gle/8LwiYAVXXN7mu9wR6'>comments form.</a></div>"

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
                        HTML("<div style='font-size:80%;line-height:1.3;'><b>OUTCOME (USA)</b></br>Mortality Rate</div>")),
               sidebarLayout(
                 sidebarPanel(
                   id = "sidebar_us_mort",
                   HTML(whatisit_text),
                   HTML("<div style='font-weight:bold;line-height:1.3;'>
                   Outcome: What are the disparities between states  in  rates of COVID-19 deaths per 100k population 
                   when compared to the average USA rate? </div><br>
                    <div style='font-size:90%;line-height:1.2;'>
                    The  rate of COVID-19 deaths per 100k in a state is: <br>
                    <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than US avg. rate for disparity index &gt; 0.2</div>
                    <div>&nbsp;&nbsp;&nbsp;<span style='background: #ffffff; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to US avg. rate for -0.2 &lt; disparity index &lt; 0.2</div>
                    <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than US avg. rate for disparity index &lt; -0.2 </div>
                    <i>Darker shades indicate greater disparity.</i><br><br>
                    
                    <strong>Mortality Rate</strong> = number of COVID-19 deaths per 100K population<br>
                    <strong>Death Rate Disparity Index</strong> = log(Mortality Rate  in state/mean Mortality Rate of US)<br>
                    <strong>Date:</strong> 04/14/2020<br><br>

                    <b>DATA SOURCE:</b> <a href='http://bit.ly/39PMWpD'>JHU CSSE (daily)</a><br>
                    </div>
                    "),
                   HTML(footer_text),
                   width=4),
                 mainPanel(
                   id = "mainpanel_us_mort",
                   tags$h4(class="map-title", "COVID-19 Mortality Rate Disparities by State Compared to Average US Rate"),
                           leafletOutput(outputId = "map.covid_deaths", height="100%"), width=8)
               ), 
               tags$script(src = "style.js")
      ),
      tabPanel(tags$div(class="tab-title",style="text-align:center;",
                        HTML("<div style='font-size:80%;line-height:1.3;'><b>MEDIATION (USA)</b></br>COVID-19 Testing</div>")),
               sidebarLayout(fluid=FALSE,
                             sidebarPanel(
                               id = "sidebar_us_test",
                               HTML(whatisit_text),
                               HTML("<div style='font-weight:bold;line-height:1.3;'>
                              Mediation: What are the disparities between states  in  rates of COVID-19 testing per 100k population 
                              when compared to the South Korean rate? </div><br>
                              <div style='font-size:90%;line-height:1.2;'>
                              South Korea is used as our testing reference rate since South 
                              Korea successfully used testing to “flatten the curve”.<br><br>
                               The rate of testing per 100k in a state is: <br>
                                 <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than South Korean testing rate for disparity index &gt; 0.2</div>
                                 <div>&nbsp;&nbsp;&nbsp;<span style='background: #ffffff; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to South Korean testing rate for -0.2 &lt; disparity index &lt; 0.2</div>
                                 <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than South Korean testing rate for disparity index &lt; -0.2</div>
                               <i>Darker shades indicate greater disparity.</i><br><br>
                               
                               <strong>Testing Rate</strong> = number of COVID-19 tests per 100K population <br>
                               <strong>Testing Rate Disparity Index</strong> = log(Testing Rate  in state/Testing Rate in South Korea) <br>
                               <strong>Date:</strong> 04/14/2020 <br><br>
                               
                               <b>DATA SOURCE:</b> <a href='http://bit.ly/39PMWpD'>JHU CSSE (daily)</a><br>
                               </div>"),
                               HTML(footer_text),
                               width=4),
                             
                             mainPanel(id = "mainpanel_us_test",
                               tags$h4(class="map-title", "COVID-19 Testing Rate Disparities by State Compared to Average South Korean Rate"),
                                       leafletOutput(outputId = "map.testing", height="100%"), width=8)
               )
      ),
      tabPanel(tags$div(class="tab-title",style="text-align:center;",
                        HTML("<div style='font-size:80%;line-height:1.3;'><b>MEDIATION (USA)</b></br>Hospital Beds</div>")),
               sidebarLayout(
                 sidebarPanel(
                   id = "sidebar_us_hosp",
                   HTML(whatisit_text),
                   HTML("<div style='font-weight:bold;line-height:1.3;'>
                     Mediation: What are the disparities between states  in  the rate of hospital beds 
                                per 100k population when compared to the rate in Italy? </div><br>
                                <div style='font-size:90%;line-height:1.2;'>
                                Italy has a higher hospital 
                                bed rate than the US, yet still faced challenges meeting peak COVID bed needs. Thus we use 
                                Italy’s rate as a minimum target rate.<br><br>
                                The rate of hospital beds per 100k in a state is<br>
                                 <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than Italian rate for disparity index &gt; 0.2</div>
                                 <div>&nbsp;&nbsp;&nbsp;<span style='background: #ffffff; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to Italian rate for -0.2 &lt;disparity index &lt; 0.2</div>
                                 <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than Italian rate for disparity index &lt; -0.2</div>
                               <i>Darker shades indicate greater disparity.</i><br><br>
                               
                               <strong>Testing Rate</strong> = number of COVID-19 tests per 100K population <br>
                               <strong>Testing Rate Disparity Index</strong> = log(Testing Rate  in state/Testing Rate in Italy) <br>
                               <strong>Date:</strong> 04/14/2020 <br><br>
                               
                               <b>DATA SOURCE:</b> <a href='https://bit.ly/2V0CYLU'>Kaiser Family Foundation</a><br>

                               </div>"),
                   HTML(footer_text),
                   width=4),
                 
                 mainPanel(id = "mainpanel_us_hosp",
                   tags$h4(class="map-title", "COVID-19 Hospital Bed Rate Disparities by State Compared to Average Italian Rate"),
                           leafletOutput(outputId = "map.hospital", height="100%"), width=8)
               )
      ),
      
      tabPanel(tags$div(class="tab-title",style="text-align:center;",
                        HTML("<div style='font-size:80%;line-height:1.3;'><b>DETERMINANT (USA)</b></br>Diabetes</div>")),
               sidebarLayout(
                 sidebarPanel(
                   id = "sidebar_us_db",
                   HTML(whatisit_text),
                   HTML("<div style='font-weight:bold;line-height:1.3;'>
                    Determinant: What are the disparities between states in rate of diabetes patients 
                                per 100k population per state when compared to the average United States rate? </div><br>
                                <div style='font-size:90%;line-height:1.2;'>
                                Diabetes puts patients at increased risk of contracting and dying from COVID-19, 
                                so areas with higher diabetes rates may face increased COVID-19 burdens. <br><br>
                               The  rate of diabetes deaths per 100k in a state is<br>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than US avg. rate for disparity index &gt; 0.2</div>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #ffffff; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to US avg. rate for -0.2 &lt;disparity index &lt; 0.2</div>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than US avg. rate for disparity index &lt; -0.2</div>
                               <i>Darker shades indicate greater disparity.</i><br><br>
                               
                               <strong>Diabetes Rate</strong> = number of diabetic patients per 100K population <br>
                               <strong>Diabetes Disparity Index</strong> = log(Diabetes Rate in state/average Diabetes Rate in US)<br>
                               <strong>Date:</strong> 2020<br><br>
                               
                               <b>DATA SOURCE:</b> <a href='https://bit.ly/34mYLBP'>County Health Rankings</a> and 
                                  <a href='https://bit.ly/2V1Zl3I'>CDC</a><br>
                          </div>"),
                   HTML(footer_text),
                   width=4),
                 
                 mainPanel(id = "mainpanel_us_db",
                   tags$h4(class="map-title", "US Diabetes Rate Disparities by State Compared to Average US Rate"),
                           leafletOutput(outputId = "map.diabetes", height="100%"), width=8)
               )
      ),
      tabPanel(tags$div(class="tab-title",style="text-align:center;",
                        HTML("<div style='font-size:80%;line-height:1.3;'><b>OUTCOME (NY)</b></br>Mortality Rate</div>")),
               sidebarLayout(
                 sidebarPanel(
                   id = "sidebar_ny_mort",
                   HTML(whatisit_text),
                   HTML("<div style='font-weight:bold;line-height:1.3;'>
                     Outcome: What are the disparities between counties of New York
                                in rates of COVID-19 deaths per 100k population when compared to the average USA rate?</div><br>  
                                <div style='font-size:90%;line-height:1.2;'>
                                
                               The rate of COVID-19 deaths per 100k in a county is<br>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than US avg. rate for disparity index &gt; 0.2</div>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #ffffff; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to US avg. rate for -0.2 &lt;disparity index &lt; 0.2</div>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than US avg. rate for disparity index &lt; -0.2</div>
                               <i>Darker shades indicate greater disparity.</i><br><br>
                               
                               <strong>Mortality Rate</strong> = number of COVID-19 deaths per 100K population<br>
                               <strong>Death Rate Disparity Index</strong> = log(Mortality Rate in state/mean Mortality Rate in US)<br>
                               <strong>Date:</strong> 04/14/2020 (updated daily) <br><br>
                               
                               <b>DATA SOURCE:</b> <a href='http://bit.ly/39PMWpD'>JHU CSSE (daily)</a> and 
                               <a href='https://on.ny.gov/2yOj1AD'>New York State Dept. of Health COVID19Tracker (daily)</a><br>

                               </div>"),
                   HTML(footer_text),
                   width=4),
                 
                 mainPanel(id = "mainpanel_ny_mort",
                   tags$h4(class="map-title", "COVID-19 Mortality Rate Disparities by County in New York Compared to Average US Rate"),
                           leafletOutput(outputId = "map.NY.deaths", height="100%"), width=8)
               )
      ),
      tabPanel(tags$div(class="tab-title",style="text-align:center;",
                        HTML("<div style='font-size:80%;line-height:1.3;'><b>OUTCOME (NY)</b></br>COVID-19 Cases</div>")),
               sidebarLayout(
                 sidebarPanel(
                   id = "sidebar_ny_cases",
                   HTML(whatisit_text),
                   HTML("<div style='font-weight:bold;line-height:1.3;'>
                      Outcome: What are the disparities between New York counties in the rate of COVID-19 
                                cases per 100k population when compared to the average United States 
                                rate?  </div> <br>
                                <div style='font-size:90%;line-height:1.2;'>
                                
                               The rate of COVID-19 deaths per 100k in a county is<br>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than US avg. rate for disparity index &gt; 0.2</div>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #ffffff; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to US avg. rate for -0.2 &lt;disparity index &lt; 0.2</div>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than US avg. rate for disparity index &lt; -0.2</div>
                               <i>Darker shades indicate greater disparity.</i><br><br>
                               
                               <strong>Mortality Rate</strong> = number of COVID-19 deaths per 100K population<br>
                               <strong>Death Rate Disparity Index</strong> = log (COVID-19 Case Rate in state/mean COVID_19 Case Rate in US) <br>
                               <strong>Date:</strong> 04/14/2020 (updated daily) <br><br>
                               
                               <b>DATA SOURCE:</b> <a href='https://on.ny.gov/39VXuCO'>heath.data.ny.gov (daily)</a><br>
                          </div>"),
                   HTML(footer_text),
                   width=4),
                 
                 mainPanel(id = "mainpanel_ny_cases",
                   tags$h4(class="map-title", "COVID-19 Case Rate Disparities by County in New York  Compared to Average US Rate"),
                           leafletOutput(outputId = "map.NY.cases", height="100%"), width=8)
               )
      ),
      tabPanel(tags$div(class="tab-title",style="text-align:center;",
                        HTML("<div style='font-size:80%;line-height:1.3;'><b>OUTCOME (NY)</b></br>COVID-19 Cases over Time</div>")),
               sidebarLayout(
                 sidebarPanel(
                   HTML(whatisit_text),
                   HTML("<div style='font-weight:bold;line-height:1.3;'>
                      Outcome: How have COVID-19 Cases increased across New York State over time?</div> <br>"),
                   img(src="New-York-Regional-Map.png",style="width: 90%;padding-left: 10%;"),
                   HTML("<div style='font-size:90%;line-height:1.2;'>
                         <br><br>
                         <b>Date:</b> 04/14/2020<br><br>
                         <b>DATA SOURCE:</b> <a href='https://on.ny.gov/39VXuCO'>heath.data.ny.gov (daily)</a><br>
                         </div>"),
                   HTML(footer_text),
                   width=4),
                 
                 mainPanel(plotOutput(outputId = "NY.cases.TS", height="500px", 
                                      click=clickOpts(id ="plot_click")), 
                           uiOutput("click_info"), 
                           width = 8)
      )
      ),
      tabPanel(tags$div(class="tab-title",style="text-align:center;",
                        HTML("<div style='font-size:80%;line-height:1.3;'><b>DETERMINANT (NY)</b></br>Diabetes</div>")),
               sidebarLayout(
                 sidebarPanel(
                   id = "sidebar_ny_det",
                   HTML(whatisit_text),
                   HTML("<div style='font-weight:bold;line-height:1.3;'>
                     Determinant: What are the disparities between New York counties in the rate 
                                of diabetes patients per 100k population when compared to the average United 
                                States rate?</div><br>
                                <div style='font-size:90%;line-height:1.2;'>
                                Diabetes puts patients at increased risk of contracting and dying from COVID-19, 
                                so areas with higher diabetes rates may face increased COVID-19 burdens. <br><br>
                               The  rate of diabetes patients per 100k in a county  is<br>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than US avg. rate for disparity index &gt; 0.2</div>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #ffffff; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to US avg. rate for -0.2 &lt; disparity index &lt; 0.2</div>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than US avg. rate for disparity index &lt; -0.2</div>
                               <i>Darker shades indicate greater disparity.</i><br><br>
                               
                               <strong>Diabetes Rate</strong> = number of diabetic patients  per 100K population <br>
                               <strong>Diabetes Disparity Index</strong> = log(Diabetes Rate in state/average Diabetes Rate in US)<br>
                               <strong>Date:</strong> 2020<br><br>
                               
                               <b>DATA SOURCE:</b> <a href='https://bit.ly/34mYLBP'>County Health Rankings</a> and 
                                  <a href='https://bit.ly/2V1Zl3I'>CDC</a><br>

                               </div>"),
                   HTML(footer_text),
                   width=4),
                 
                 mainPanel(id = "mainpanel_ny_det",
                   tags$h4(class="map-title", "COVID-19 Diabetes Rate Disparities by County in New York Compared to Average US Rate"),
                           leafletOutput(outputId = "map.NY.diabetes", height="100%"), width=8)
               )
      )
    )
  )
#### Server Code ####
server <- function(input, output, session) {
  
  # Render leaflet plot with all information in hover
  output$map.testing <- renderLeaflet({
    
    colors <- c("#253494","#4575B4", "#74ADD1","#ABD9E9","#f7f7f7","#FDAE61","#F46D43", "#D73027", "#BD0026")
    bins <- c(5, 3, 2, 1, .2, -.2, -1, -2, -3, -5)
    pal2 <- leaflet::colorBin(colors, domain = states$tests_ldi, bins = bins, reverse=TRUE)
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
        weight = 1,
        opacity = 1,
        color = "#330000",
        dashArray = "1",
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
                values = ~states$tests_ldi, 
                opacity = 0.7, 
                title = "Disparity Index<br/>US Total Tests vs. South Korea",
                position = "bottomright",
                labFormat = function(type, cuts, p) { n = length(cuts) 
                   cuts[n] = paste0(cuts[n]," lower") 
                   # for (i in c(1,seq(3,(n-1)))){cuts[i] = paste0(cuts[i],"—")} 
                   for (i in c(1,seq(2,(n-1)))){cuts[i] = paste0(cuts[i]," — ")} 
                   cuts[2] = paste0(cuts[2]," higher") 
                   paste0(str_remove(cuts[-n],"higher"), str_remove(cuts[-1],"—"))
                }
                ) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light"))
  })
  
  output$map.cardio <- renderLeaflet({
    
    colors <- c("#253494","#4575B4", "#74ADD1","#ABD9E9","#f7f7f7","#FDAE61","#F46D43", "#D73027", "#BD0026")
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
        weight = 1,
        opacity = 1,
        color = "#330000",
        dashArray = "1",
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
        id = "mapbox.light"))
    #Remove personal API key
  })
  
  output$map.diabetes <- renderLeaflet({
    
    colors <- c("#253494","#4575B4", "#74ADD1","#ABD9E9","#f7f7f7","#FDAE61","#F46D43", "#D73027", "#BD0026")
    bins <- c(5, 3, 2, 1, .2, -.2, -1, -2, -3, -5)
    pal2 <- leaflet::colorBin(colors, domain = states$diabetes_rate_ldi, bins = bins, reverse=FALSE)
    labels2 <- sprintf(
      "<strong>%s</strong><br/>
      Diabetes Rate DI: %.2g<br/>
      Diabetes Rate: %.1f per 100k",
      states$NAME, states$diabetes_rate_ldi, states$pct_Adults_with_Diabetes*1000
    ) %>% lapply(htmltools::HTML)
    
    leaflet(states.shapes) %>%
      setView(-96, 37.8, 4) %>% 
      addPolygons(
        fillColor = ~pal2(states$diabetes_rate_ldi),
        weight = 1,
        opacity = 1,
        color = "#330000",
        dashArray = "1",
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
                values = ~states$diabetes_rate_ldi, 
                opacity = 0.7, title = "Disparity Index<br/>US Diabetes Rate",
                position = "bottomright",
                labFormat = function(type, cuts, p) { n = length(cuts) 
                cuts[n] = paste0(cuts[n]," lower") 
                # for (i in c(1,seq(3,(n-1)))){cuts[i] = paste0(cuts[i],"—")} 
                for (i in c(1,seq(2,(n-1)))){cuts[i] = paste0(cuts[i]," — ")} 
                cuts[2] = paste0(cuts[2]," higher") 
                paste0(str_remove(cuts[-n],"higher"), str_remove(cuts[-1],"—"))
                }) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light"))
    #Remove personal API key
  })
  
  output$map.hospital <- renderLeaflet({
    
    colors <- c("#253494","#4575B4", "#74ADD1","#ABD9E9","#f7f7f7","#FDAE61","#F46D43", "#D73027", "#BD0026")
    bins <- c(5, 3, 2, 1, .2, -.2, -1, -2, -3, -5)
    pal2 <- leaflet::colorBin(colors, domain = states$hosp_beds_ldi, bins = bins, reverse=TRUE)
    labels2 <- sprintf(
      "<strong>%s</strong><br/>
      Hospital Beds vs Italy DI: %.2g",
      states$NAME, states$hosp_beds_ldi
    ) %>% lapply(htmltools::HTML)
    
    leaflet(states.shapes) %>%
      setView(-96, 37.8, 4) %>% 
      addPolygons(
        fillColor = ~pal2(states$hosp_beds_ldi),
        weight = 1,
        opacity = 1,
        color = "#330000",
        dashArray = "1",
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
                values = ~states$hosp_beds_ldi, 
                opacity = 0.7, 
                title = "Disparity Index<br/>US Hospital Beds vs Italy",
                position = "bottomright",
                labFormat = function(type, cuts, p) { n = length(cuts) 
                cuts[n] = paste0(cuts[n]," lower") 
                # for (i in c(1,seq(3,(n-1)))){cuts[i] = paste0(cuts[i],"—")} 
                for (i in c(1,seq(2,(n-1)))){cuts[i] = paste0(cuts[i]," — ")} 
                cuts[2] = paste0(cuts[2]," higher") 
                paste0(str_remove(cuts[-n],"higher"), str_remove(cuts[-1],"—"))
                }
                ) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light"))
    #Remove personal API key
  })
  
  output$map.covid_deaths <- renderLeaflet({
    
    colors <- c("#253494","#4575B4", "#74ADD1","#ABD9E9","#f7f7f7","#FDAE61","#F46D43", "#D73027", "#BD0026")
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
        weight = 1,
        opacity = 1,
        color = "#330000",
        dashArray = "1",
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
                position = "bottomright",
                labFormat = function(type, cuts, p) { n = length(cuts) 
                cuts[n] = paste0(cuts[n]," lower") 
                # for (i in c(1,seq(3,(n-1)))){cuts[i] = paste0(cuts[i],"—")} 
                for (i in c(1,seq(2,(n-1)))){cuts[i] = paste0(cuts[i]," — ")} 
                cuts[2] = paste0(cuts[2]," higher") 
                paste0(str_remove(cuts[-n],"higher"), str_remove(cuts[-1],"—"))
                }
      ) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light"))
    #Remove personal API key
  })
  
  output$map.NY.deaths <- renderLeaflet({
    
    colors <- c("#253494","#4575B4", "#74ADD1","#ABD9E9","#f7f7f7","#FDAE61","#F46D43", "#D73027", "#BD0026")
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
      setView(-76.071782, 42.991989, 6) %>%  # Set to the geographic center of NY
      addPolygons(
        fillColor = ~pal2(NY.data$death_rate_ldi),
        weight = 1,
        opacity = 1,
        color = "#330000",
        dashArray = "1",
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
                position = "bottomright",
                labFormat = function(type, cuts, p) { n = length(cuts) 
                cuts[n] = paste0(cuts[n]," lower") 
                # for (i in c(1,seq(3,(n-1)))){cuts[i] = paste0(cuts[i],"—")} 
                for (i in c(1,seq(2,(n-1)))){cuts[i] = paste0(cuts[i]," — ")} 
                cuts[2] = paste0(cuts[2]," higher") 
                paste0(str_remove(cuts[-n],"higher"), str_remove(cuts[-1],"—"))
                }
      ) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light"))
    #Remove personal API key
  })
  
  output$map.NY.cases <- renderLeaflet({
    
    colors <- c("#253494","#4575B4", "#74ADD1","#ABD9E9","#f7f7f7","#FDAE61","#F46D43", "#D73027", "#BD0026")
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
      setView(-76.071782, 42.991989, 6) %>%  # Set to the geographic center of NY
      addPolygons(
        fillColor = ~pal2(NY.data$case_rate_ldi),
        weight = 1,
        opacity = 1,
        color = "#330000",
        dashArray = "1",
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
                position = "bottomright",
                labFormat = function(type, cuts, p) { n = length(cuts) 
                cuts[n] = paste0(cuts[n]," lower") 
                # for (i in c(1,seq(3,(n-1)))){cuts[i] = paste0(cuts[i],"—")} 
                for (i in c(1,seq(2,(n-1)))){cuts[i] = paste0(cuts[i]," — ")} 
                cuts[2] = paste0(cuts[2]," higher") 
                paste0(str_remove(cuts[-n],"higher"), str_remove(cuts[-1],"—"))
                }
      ) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light"))
    #Remove personal API key
  })
  
  output$map.NY.diabetes <- renderLeaflet({
    
    colors <- c("#253494","#4575B4", "#74ADD1","#ABD9E9","#f7f7f7","#FDAE61","#F46D43", "#D73027", "#BD0026")
    bins <- c(5, 3, 2, 1, .2, -.2, -1, -2, -3, -5)
    pal2 <- leaflet::colorBin(colors, domain = NY.data$diabetes_ldi, bins = bins, reverse=FALSE)
    
    NY.shape$county_fips <- paste(as.data.frame(NY.shape)$STATEFP, as.data.frame(NY.shape)$COUNTYFP, sep = '')
    NY.data <- dplyr::left_join(as.data.frame(NY.shape), as.data.frame(NY.data), by = c("county_fips" = "FIPS"))
    
    labels <- sprintf(
      "<strong>%s</strong><br/>
      Diabetes Rate DI: %.2g<br>
      Diabetes Rate: %.1f per 100k",
      NY.data$County, NY.data$diabetes_ldi, NY.data$pct_Adults_with_Diabetes*1000
    ) %>% lapply(htmltools::HTML)
    
    leaflet(NY.shape) %>%
      setView(-76.071782, 42.991989, 6) %>%  # Set to the geographic center of NY
      addPolygons(
        fillColor = ~pal2(NY.data$diabetes_ldi),
        weight = 1,
        opacity = 1,
        color = "#330000",
        dashArray = "1",
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
                position = "bottomright",
                labFormat = function(type, cuts, p) { n = length(cuts) 
                cuts[n] = paste0(cuts[n]," lower") 
                # for (i in c(1,seq(3,(n-1)))){cuts[i] = paste0(cuts[i],"—")} 
                for (i in c(1,seq(2,(n-1)))){cuts[i] = paste0(cuts[i]," — ")} 
                cuts[2] = paste0(cuts[2]," higher") 
                paste0(str_remove(cuts[-n],"higher"), str_remove(cuts[-1],"—"))
                }
      ) %>%
      addProviderTiles("MapBox", options = providerTileOptions(
        id = "mapbox.light"))
    #Remove personal API key
  })
  
  output$NY.cases.TS <- renderPlot({
 
    highlight_points <- covid_NY_TS_plot.cases %>% 
      filter( 
                County == "New York State" & date == as.Date("2020-03-30") |
                County == "Albany" & date == as.Date("2020-03-26") |
                # County == "Allegany" & date == as.Date("2020-03-29") |
                County == "Bronx" & date == as.Date("2020-03-25") |
                County == "Broome" & date == as.Date("2020-04-02") |
                # County == "Cattaraugus" & date == as.Date("2020-03-30") |
                County == "Cayuga" & date == as.Date("2020-04-02") |
                County == "Chautauqua" & date == as.Date("2020-04-10") |
                # County == "Chemung" & date == as.Date("2020-04-10") |
                County == "Chenango" & date == as.Date("2020-04-12") |
                County == "Clinton" & date == as.Date("2020-03-26") |
                # County == "Columbia" & date == as.Date("2020-03-29") |
                County == "Cortland" & date == as.Date("2020-03-25") |
                # County == "Delaware" & date == as.Date("2020-04-02") |
                County == "Dutchess" & date == as.Date("2020-03-30") |
                County == "Erie" & date == as.Date("2020-04-02") |
                # County == "Essex" & date == as.Date("2020-04-10") |
                # County == "Franklin" & date == as.Date("2020-04-10") |
                # County == "Fulton" & date == as.Date("2020-04-12") |
                County == "Genesee" & date == as.Date("2020-03-26") |
                # County == "Greene" & date == as.Date("2020-03-29") |
                County == "Hamilton" & date == as.Date("2020-03-25") |
                County == "Herkimer" & date == as.Date("2020-04-02") |
                # County == "Jefferson" & date == as.Date("2020-03-30") |
                County == "Kings" & date == as.Date("2020-04-02") |
                # County == "Lewis" & date == as.Date("2020-04-10") |
                # County == "Livingston" & date == as.Date("2020-04-10") |
                County == "Madison" & date == as.Date("2020-04-12") |
                # County == "Monroe" & date == as.Date("2020-03-26") |
                # County == "Montgomery" & date == as.Date("2020-03-29") |
                County == "Nassau" & date == as.Date("2020-03-25") |
                County == "New York" & date == as.Date("2020-04-02") |
                County == "Manhattan" & date == as.Date("2020-03-30") |
                County == "Niagara" & date == as.Date("2020-04-02") |
                County == "Oneida" & date == as.Date("2020-04-10") |
                County == "Onondaga" & date == as.Date("2020-04-10") |
                # County == "Ontario" & date == as.Date("2020-04-12") |
                County == "Orange" & date == as.Date("2020-03-26") |
                County == "Orleans" & date == as.Date("2020-03-29") |
                County == "Oswego" & date == as.Date("2020-03-25") |
                County == "Otsego" & date == as.Date("2020-04-02") |
                # County == "Putnam" & date == as.Date("2020-03-30") |
                County == "Queens" & date == as.Date("2020-04-02") |
                County == "Rensselaer" & date == as.Date("2020-04-10") |
                County == "Richmond" & date == as.Date("2020-04-01") |
                County == "Rockland" & date == as.Date("2020-04-12") |
                County == "St. Lawrence" & date == as.Date("2020-03-26") |
                County == "Saratoga" & date == as.Date("2020-03-29") |
                County == "Schenectady" & date == as.Date("2020-03-25") |
                County == "Schoharie" & date == as.Date("2020-04-02") |
                County == "Schuyler" & date == as.Date("2020-03-30") |
                County == "Seneca" & date == as.Date("2020-04-02") |
                # County == "Steuben" & date == as.Date("2020-04-10") |
                County == "Suffolk" & date == as.Date("2020-04-10") |
                County == "Sullivan" & date == as.Date("2020-04-12") |
                # County == "Tioga" & date == as.Date("2020-03-26") |
                County == "Tompkins" & date == as.Date("2020-03-29") |
                # County == "Ulster" & date == as.Date("2020-03-25") |
                # County == "Warren" & date == as.Date("2020-04-02") |
                # County == "Washington" & date == as.Date("2020-03-30") |
                # County == "Wayne" & date == as.Date("2020-04-02") |
                County == "Westchester" & date == as.Date("2020-04-10") |
                # County == "Wyoming" & date == as.Date("2020-04-10") |
                County == "Yates" & date == as.Date("2020-04-12")
      )
    
    NY_region_palette.df <- NY_counties_regions %>%
      select(Region,Color) %>% 
      distinct(Region,Color)
    
    NY_region_palette <- setNames(as.character(NY_region_palette.df$Color), as.character(NY_region_palette.df$Region))
    
    covid_NY_TS_plot.cases %>%
      ggplot(aes(date, 
                 cases, 
                 color = Region,
                 group=County)) +
      scale_color_manual(values=NY_region_palette) +
      geom_line(size=1) +
      scale_y_continuous(
        trans = "log10",
        breaks = c(10,100,500,1000,5000,10000, 50000)
      ) +
      scale_x_datetime(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%b %d") +
      ylab("Cumulative Number of Cases") + 
      ggtitle("New York State COVID-19 Cases per County (Mar-Apr 2020)")  + 
      geom_label_repel(data=highlight_points,  aes(label=County), box.padding = unit(1.75, 'lines')) + 
      NULL
    
      })
  
  output$click_info <- renderPrint({
    hover <- input$plot_click

    point <- nearPoints(covid_NY_TS_plot.cases, hover, threshold = 5, addDist = TRUE)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")

    # actual tooltip created as wellPanel
    if (nrow(point) != 0) {
      if (point$County == "New York State"){
        wellPanel(
        # style = style,
        p(HTML(paste0(point$County,": ",point$cases," COVID-19 cases as of ",point$date)))
      )
      } else {
        wellPanel(
          # style = style,
          p(HTML(paste0(point$County," County: ",point$cases," COVID-19 cases as of ",point$date)))
        )
        
      }
  }
  })
  
    
  
}

#### Set up Shiny App ####
shinyApp(ui = ui, server = server)
