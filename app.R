#### Library and Data Imports ####
source("modules/Source.R")
source("modules/data_load.R")
source("modules/preprocessing.R")

update_date <- "05-27-2020" # makes it easy to change all occurances when we update

moving.avg.window <- 7 # WARNING: Behavior for moving.avg.window > number of report dates for a region is undefined.
                       # (i.e. a 20 day window if Catskill Region has 19 report dates.)
height <- "600px"# plot heights

# Leaving this in case we need it
# TODO: Implement other text as strings like this...
rpi_accessibility_link <- "<div class='center'><p><a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Statement of Accessibility</a></p></div>"

footer_text <- "<h3><br><div>COVID<b>MINDER analysis and visualizations</b> by students and staff
                                of <a href='http://idea.rpi.edu/'>The Rensselaer Institute for Data Exploration 
                                and Applications</a> at <a href='http://rpi.edu/'>Rensselaer Polytechnic Institute</a>. 
                                <b>COVIDMINDER</b> is an open source project implemented on the <a href='https://shiny.rstudio.com/'>R Shiny platform</a>;
                                see the <a href='https://github.com/TheRensselaerIDEA/COVIDMINDER'>COVIDMINDER github</a>
                                for more information. <br><br>
                                <a href='https://forms.gle/8LwiYAVXXN7mu9wR6'><img src='comment.png' style='float:left;width:40px;margin-right:5px;' ></a>
                                Thanks for using <b>COVIDMINDER!</b> Please take a few moments 
                                to fill out our short <a href='https://forms.gle/8LwiYAVXXN7mu9wR6'>comments form.</a></h3><br><br>
                                "
                                #<i><a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Statement 
                                #of Accessibility</a></i></div>"

whatisit_text_abt <-"<div><h3>COVID<b>MINDER</b> reveals the regional disparities 
                                in outcomes, determinants, and mediations of the COVID-19 pandemic. Outcomes are the direct 
                                effects of COVID-19. Social and Economic Determinants are pre-existing risk factors that impact 
                                COVID-19 outcomes. Mediations are resources and programs used to combat the pandemic.</h3></div>"

whatisit_text <- "COVIDMINDER reveals the regional disparities in outcomes, determinants, and mediations of the COVID-19 pandemic. Outcomes are the direct effects of COVID-19. Social and Economic Determinants are pre-existing risk factors that impact COVID-19 outcomes. Mediations are resources and programs used to combat the pandemic."


comments_link <-"<a href='https://forms.gle/8LwiYAVXXN7mu9wR6'><img src='comment.png' style='float:left;width:40px;padding-right:2px;' ></a>
                                Thanks for using <b>COVIDMINDER!</b> Please take a few moments 
                                to fill out our short <a href='https://forms.gle/8LwiYAVXXN7mu9wR6'>comments form.</a><br><br>
                                <i><a href='https://info.rpi.edu/statement-of-accessibility'>Rensselaer Statement 
                                of Accessibility</a></i>"

# For URL parameterization
url1 <- url2 <- ""

#### UI Code ####
ui <- 
  tagList(
    tags$head(tags$title("COVIDMINDER: Where you live matters")),
    tags$head(includeHTML("www/analytics.html")),
    
    # WIDTH Getting code
    # tags$head(tags$script('
    #                     var width = 0;
    #                     $(document).on("shiny:connected", function(e) {
    #                       width = window.innerWidth;
    #                       Shiny.onInputChange("width", width);
    #                     });
    #                     $(window).resize(function(e) {
    #                       width = window.innerWidth;
    #                       Shiny.onInputChange("width", width);
    #                     });
    #                     ')),
    navbarPage(
      id="tab",
      theme="style.css",
      title=tags$div(class="title-text",
                     title = whatisit_text,
                     img(class="logo", src="Rensselaer_round.png"),
                     HTML("COVID<b>MINDER</b>")),
      navbarMenu(menuName = "outcome_maps_menu",
                 HTML("<div><b>OUTCOME (MAPS)</b></div>"),
      tabPanel(tags$div(class="tab-title",style="text-align:center;", #For some reason, unresponsive to class
                        HTML("<div><b>OUTCOME (USA)</b></br>Mortality Rate</div>")),
               value="outcome_usa_mortality",
               fluidPage(
                 fluidRow(class = "page_title",tags$h1("OUTCOME: USA COVID-19 Mortality Rates Disparities")),
                 fluidRow(class = "page_title",tags$h2("What are the disparities between states  in  rates of COVID-19 deaths per 100k population 
                   when compared to the average USA rate?")),
                fluidRow(class = "map-container",
               column(8,
                        id = "mainpanel_us_mort",
                        tags$h3(class="map-title", "COVID-19 Mortality Rate Disparities by State Compared to Average US Rate"),
                        leafletOutput(outputId = "map.covid_deaths", height=height))
               ,
               column(4,
                      id = "sidebar_us_mort",
                      #HTML(whatisit_text),
                      HTML(paste0("<div>The rate of COVID-19 deaths per 100k in a state is: <br>
                    <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than US avg. rate for disparity index &gt; 0.2</div>
                    <div>&nbsp;&nbsp;&nbsp;<span style='background: #f7f7f7; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to US avg. rate for -0.2 &lt; disparity index &lt; 0.2</div>
                    <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than US avg. rate for disparity index &lt; -0.2 </div>
                    <i>Darker shades indicate greater disparity.</i><br><br>
                    
                    <strong>Mortality Rate</strong> = number of COVID-19 deaths per 100K population<br>
                    <strong>Death Rate Disparity Index</strong> = log(Mortality Rate  in state/mean Mortality Rate of US)<br>
                    <strong>Date: </strong>",update_date,"<br><br>

                    <b>DATA SOURCE:</b> <a href='http://bit.ly/39PMWpD'>JHU CSSE (daily)</a><br>
                    </div>
                    ")),
                      #HTML(footer_text),
               ))), 
               #tags$script(src = "style.js")
      ), 
      tabPanel(tags$div(class="tab-title",style="text-align:center;", #For some reason, unresponsive to class
                        HTML("<div><b>OUTCOME (USA)</b></br>Racial/Ethnic Disparity</div>")),
               value="outcome_usa_racial_disparity",
               fluidPage(
                 fluidRow(class="page_title", tags$h1("OUTCOME: Racial/Ethnic Disparities of COVID-19 Mortality")),
                 fluidRow(class="page_title", tags$h2("Do minorities make up a higher percentage of COVID-19 deaths across the United States when compared to 
                          their population percentage?")),
                 fluidRow(class="map-container",
                   column(8,
                           id = "mainpanel_us_mort_race",
                           tags$h3(class="map-title", "COVID-19 Mortality Rate Disparities by State by Race/Ethnicity"),
                           #HTML("<br><br>"),
                           tags$div(class = "select-bar",
                                    selectInput(inputId = "race",
                                                label = NULL,
                                                choices =  c("Non-hispanic White"="nhw",
                                                             "Non-hispanic American Indian/Alaska Native"="nhaian",
                                                             "Non-hispanic Asian Pacific Islander"="nhapi",
                                                             "Hispanic/Latino (total)"="hlt",
                                                             "Non-hispanic Black/African American"="nhbaa"),
                                                selected = "nhbaa")),
                           leafletOutput(outputId = "map.covid_deaths.race", height=height)),
                          column(4,
                                 id = "sidebar_us_mort_race",
                                 #HTML(whatisit_text),
                                 HTML(paste0("
                          <div>
                          <a href='https://bit.ly/2Krl5RG'>Evidence suggests</a> that COVID-19 deaths may be higher for certain racial/ethnic groups.<br><br>
                          If the percentage of COVID-19 deaths experienced by a racial/ethnic group is higher than that 
                          group’s population percentage for a region, this suggests that COVID-19 may have a disparate 
                          impact on that group in that region. Social and economic determinants may contribute to this disparity.
                          <br><br>",
                                             
                                             "For each racial/ethnic group, the proportion of COVID-19 deaths for that group is:<br>
                          <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than population percentage for disparity index &gt; 0.2</div>
                          <div>&nbsp;&nbsp;&nbsp;<span style='background: #f7f7f7; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to the population percentage for -0.2 &lt;disparity index &lt; 0.2</div>
                          <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than population percentage for disparity index &lt; -0.2</div>
                          <i>Darker shades indicate greater disparity.</i><br><br>
                               
                          <strong>Group COVID-19 Death Percentage</strong> = number of COVID-19 deaths for group/total COVID-19 deaths<br>
                          <strong>Population Percentage</strong> = number of residents from that group/ total number of residents<br>
                          <strong>Death Rate Disparity Index (DI)</strong> = log(Group COVID-19 Death Percentage/Population Percentage)
                          <br><br>
                          <strong>Date: </strong>",update_date,"<br><br>
                          <b>DATA SOURCE:</b> <a href='https://data.cdc.gov/resource/pj7m-y5uh.csv'>data.cdc.gov</a><br>
                          </div>"))
                                 #HTML(footer_text),
                          )
                 
                 )
               ), 
      ), 
      tabPanel(tags$div(class="tab-title",style="text-align:center;",
                        HTML("<div><b>OUTCOME (NY)</b></br>Mortality Rate</div>")),
               value="outcome_ny_mortality",
               fluidPage(
                   fluidRow(class="page_title", tags$h1("OUTCOME: New York rates of COVID-19 Mortality")),
                   fluidRow(class="page_title", tags$h2("What are the disparities between counties of New York
                               in rates of COVID-19 deaths per 100k population when compared to the average USA rate?")),
                   fluidRow(class="map-container",
                   column(8,id = "mainpanel_ny_mort",
                           tags$h3(class="map-title", "COVID-19 Mortality Rate Disparities by County in New York Compared to Average US Rate"),
                           leafletOutput(outputId = "map.NY.deaths", height = height)),
                   column(4,
                          id = "sidebar_ny_mort",
                          #HTML(whatisit_text),
                          HTML(paste0("<div>
                               The rate of COVID-19 deaths per 100k in a county is<br>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than US avg. rate for disparity index &gt; 0.2</div>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #f7f7f7; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to US avg. rate for -0.2 &lt;disparity index &lt; 0.2</div>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than US avg. rate for disparity index &lt; -0.2</div>
                               <i>Darker shades indicate greater disparity.</i><br><br>
                               
                               <strong>Mortality Rate</strong> = number of COVID-19 deaths per 100K population<br>
                               <strong>Death Rate Disparity Index</strong> = log(Mortality Rate in state/mean Mortality Rate in US)<br>
                               <strong>Date: </strong>",update_date,"<br><br>
                               
                               <b>DATA SOURCE:</b> <a href='http://bit.ly/39PMWpD'>JHU CSSE (daily)</a> and 
                               <a href='https://on.ny.gov/2VehafT'>New York State Dept. of Health COVIDTracker (daily)</a><br>
                               
                               </div>"))
                          #HTML(footer_text),
                   ))
                 
                 
                 
                 )),
      tabPanel(tags$div(class="tab-title",style="text-align:center;",
                        HTML("<div><b>OUTCOME (NY)</b></br>COVID-19 Cases</div>")),
               value="outcome_ny_cases",
               fluidPage(
                 fluidRow(class="page_title", tags$h1("OUTCOME: New York rates of COVID-19 Cases")),
                 fluidRow(class="page_title", tags$h2("What are the disparities between New York counties in the rate of COVID-19 
                               cases per 100k population when compared to the average United States 
                               rate?")),
                 fluidRow(class = "map-container",
                 column(8, id = "mainpanel_ny_cases",
                         tags$h3(class="map-title", "COVID-19 Case Rate Disparities by County in New York  Compared to Average US Rate"),
                         leafletOutput(outputId = "map.NY.cases", height=height)),
                 column(4,
                   id = "sidebar_ny_cases",
                   #HTML(whatisit_text),
                   HTML(paste0("<div>
                               
                               The rate of COVID-19 deaths per 100k in a county is<br>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than US avg. rate for disparity index &gt; 0.2</div>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #f7f7f7; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to US avg. rate for -0.2 &lt;disparity index &lt; 0.2</div>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than US avg. rate for disparity index &lt; -0.2</div>
                               <i>Darker shades indicate greater disparity.</i><br><br>
                               
                               <strong>Mortality Rate</strong> = number of COVID-19 deaths per 100K population<br>
                               <strong>Date: </strong>",update_date,"<br><br>
                               
                               <b>DATA SOURCE:</b> <a href='https://on.ny.gov/39VXuCO'>heath.data.ny.gov (daily)</a><br>
                               </div>")),
                   )
                 )
                   )
                 )),
      navbarMenu(menuName = "outcome_plots_menu",
                 #HTML("<div style='font-size:90%;line-height:1.3;'><b>OUTCOME (GRAPHS)</b><br>Select a state outcome</div>"),
                 HTML("<div><b>OUTCOME (GRAPHS)</b></div>"),
                 
               tabPanel(title=tags$div(class="tab-title",style="text-align:center;",
                            HTML("<div><b>OUTCOME (NY)</b></br>COVID-19 Trends in new Cases (Region)</div>")),
               value="outcome_ny_new_cases",
               fluidPage( 
                 fluidRow(class="page_title", tags$h1("OUTCOME: New York trends of new COVID-19 Cases")),
                 fluidRow(class="page_title", tags$h2("How have new COVID-19 Cases been mitigated in New York State over time?")),
                 fluidRow(class = "map-container",
                 column(8, style=paste0("height:",height,";"), id = "mainpanel_ny_new_case",
                           tags$div(class = "page_title",
                           selectInput(inputId = "NYRegion3",
                                       label = "NY Regions",
                                       choices = c("All Regions", sort(unique(covid_NY_TS_plot.cases$Region))),
                                       selected = "All Regions"),
                           dateRangeInput(inputId = "NYDate.ma",
                                          label = "Date Range",
                                          min = min(covid_NY_TS_plot.cases$date),
                                          max = as.Date(update_date, format = "%m-%d-%Y") - 1,
                                          start = as.Date(update_date, format = "%m-%d-%Y") - 31,
                                          end = as.Date(update_date, format = "%m-%d-%Y") - 1),
                           radioButtons(inputId = "rate.ma",
                                        label = "",
                                        choices = c("Overall", "Per/100k"),
                                        selected = "Per/100k")),
                           tags$div(class = "NY_case_plots",
                                    plotOutput(outputId = "NY.cases.ma", height="100%", 
                                               click = clickOpts(id ="NY.cases.TS_click_ma"),
                                               dblclick = "NY.cases.TS_dblclick",
                                               brush = brushOpts(
                                                 id = "NY.cases.TS_brush",
                                                 resetOnNew = TRUE))
                           ),
                           HTML("<div style='position:absolute;bottom:0;'>
                                <br>To zoom plot, click and drag, then double-click in select box<br>
                                To un-zoom, double-click in plot<br>
                                For region details, single-click on line<br>
                                </div>")
                           ),
                 column(4, id = "sidebar_ny_new_case",
                        img(src="New-York-Regional-Map.png",style="width: 90%;padding-left: 10%;"),
                        HTML(paste0("<div>
                               <strong>Date: </strong>",update_date,"<br><br>
                               <b>DATA SOURCE:</b> <a href='https://on.ny.gov/39VXuCO'>heath.data.ny.gov (daily)</a><br>
                               </div>")),
                        uiOutput("click_info_ma")
                 ))
               )
      ),
      tabPanel(tags$div(class="tab-title",style="text-align:center;",
                        HTML("<div><b>OUTCOME (NY)</b></br>COVID-19 Cases over Time (County)</div>")),
               value="outcome_ny_cases_time",
               fluidPage(
                 fluidRow(class="page_title", tags$h1("OUTCOME: New York Counties total COVID-19 Cases over time")),
                 fluidRow(class="page_title", tags$h2("How have COVID-19 Cases increased across New York State over time?")),
                 fluidRow(class = "map-container",
                          
                          column(8, style=paste0("height:",height,";"),id = "mainpanel_ny_CoT",
                                 tags$div(
                                   # selectInput(inputId = "NYRegion",
                                   #             label = "NY Regions",
                                   #             choices = c("All Regions", sort(unique(covid_NY_TS_plot.cases$Region))),
                                   #             selected = "All Regions"),
                                   selectInput(inputId = "NYCounty",
                                               label = "NY Counties",
                                               choices = c("All Counties", sort(unique(covid_NY_TS_plot.cases$County))),
                                               selected = 1)
                                 ),
                                 dateRangeInput(inputId = "NYCoTDate",
                                                label = "Date Range",
                                                min = min(covid_NY_TS_plot.cases$date),
                                                max = as.Date(update_date, format = "%m-%d-%Y") - 1,
                                                start = as.Date(update_date, format = "%m-%d-%Y") - 62,
                                                end = as.Date(update_date, format = "%m-%d-%Y") - 1),
                                 radioButtons(inputId = "rate.CoT",
                                              label = "",
                                              choices = c("Overall", "Per/100k"),
                                              selected = "Per/100k"),
                                 tags$div(class = "NY_case_plots",
                                          plotOutput(outputId = "NY.cases.TS", height="100%", 
                                                     click = clickOpts(id ="NY.cases.TS_click"),
                                                     dblclick = "NY.cases.TS_dblclick",
                                                     brush = brushOpts(
                                                       id = "NY.cases.TS_brush",
                                                       resetOnNew = TRUE))
                                 ),
                                 HTML("<div style='position:absolute;bottom:0;'>
                                <br>To zoom plot, click and drag, then double-click in select box<br>
                                To un-zoom, double-click in plot<br>
                                For county details, single-click on line<br>
                                </div>")),
                 column(4,
                   id = "sidebar_ny_CoT",
                   img(src="New-York-Regional-Map.png",style="width: 90%;padding-left: 10%;"),
                   HTML(paste0("<div>
                               <br><br>
                               <strong>Date: </strong>",update_date,"<br><br>
                               <b>DATA SOURCE:</b> <a href='https://on.ny.gov/39VXuCO'>heath.data.ny.gov (daily)</a><br>
                               </div>")),
                   uiOutput("click_info"))
                 )
                   )
                   ),
      tabPanel(title=tags$div(class="tab-title",style="text-align:center;",
                              HTML("<div><b>OUTCOME (NY)</b></br>COVID-19 Cases over Time (Region)</div>")),
               value="outcome_ny_cases_time_region",
               fluidPage(
                 fluidRow(class="page_title", tags$h1("OUTCOME: New York Region total COVID-19 Cases over time")),
                 fluidRow(class="page_title", tags$h2("How have COVID-19 Cases increased across New York State over time?")),
                 fluidRow(class = "map-container",
                 column(8,style=paste0("height:",height,";"), id = "mainpanel_ny_CoT_region",
                           selectInput(inputId = "NYRegion2",
                                       label = "NY Regions",
                                       choices = c("All Regions", sort(unique(covid_NY_TS_plot.cases$Region))),
                                       selected = "All Regions"),
                           dateRangeInput(inputId = "NYRegionDate",
                                          label = "Date Range",
                                          min = min(covid_NY_TS_plot.cases$date),
                                          max = as.Date(update_date, format = "%m-%d-%Y") - 1,
                                          start = as.Date(update_date, format = "%m-%d-%Y") - 62,
                                          end = as.Date(update_date, format = "%m-%d-%Y") - 1),
                           radioButtons(inputId = "rate.CoT.reg",
                                        label = "",
                                        choices = c("Overall", "Per/100k"),
                                        selected = "Per/100k"),
                           tags$div(class = "NY_case_plots",
                                    plotOutput(outputId = "NY.cases.TS.region", height="100%", 
                                               click = clickOpts(id ="NY.cases.TS_click_reg"),
                                               dblclick = "NY.cases.TS_dblclick",
                                               brush = brushOpts(
                                                 id = "NY.cases.TS_brush",
                                                 resetOnNew = TRUE))
                           ),
                           HTML("<div style='position:absolute;bottom:0;'>
                                <br>To zoom plot, click and drag, then double-click in select box<br>
                                To un-zoom, double-click in plot<br>
                                For county details, single-click on line<br>
                                </div>")),
                 column(4, id = "sidebar_ny_CoT_region",
                        img(src="New-York-Regional-Map.png",style="width: 90%;padding-left: 10%;"),
                        HTML(paste0("<div>
                               <br><br>
                               <strong>Date: </strong>",update_date,"<br><br>
                               <b>DATA SOURCE:</b> <a href='https://on.ny.gov/39VXuCO'>heath.data.ny.gov (daily)</a><br>
                               </div>")),
                        uiOutput("click_info_reg"))
                 )
               )
      ),
      tabPanel(tags$div(class="tab-title",style="text-align:center;",
                        HTML("<div><b>OUTCOME (NY)</b></br>COVID-19 Racial Disparity</div>")),
               value="outcome_ny_racial_disparity",
               fluidPage(
                 fluidRow(class="page_title", tags$h1("OUTCOME: New York Racial Disparities of COVID-19 mortality")),
                 fluidRow(class="page_title", tags$h2("Do minorities make up a higher percentage of COVID-19 deaths when compared to 
                        their population percentage? Do New York City and the rest of New York State have 
                        different disparities in minority COVID-19 deaths?")),
                 fluidRow(class = "map-container",
                 column(8,style=paste0("height:",height,";"), id = "mainpanel_ny_race", 
                          plotOutput(outputId = "NY.race.nys", height="50%"), 
                          plotOutput(outputId = "NY.race.nyc", height="50%")),
                 column(4,
                   id = "sidebar_ny_race",
                   #HTML(whatisit_text),
                   HTML("
                        <div>
                        <a href='https://bit.ly/2Krl5RG'>Evidence suggests</a> that COVID-19 deaths may be higher for certain racial/ethnic groups.<br><br>
                        If the percentage  of COVID-19 deaths experienced by a racial/ethnic group is higher than that 
                        group’s population percentage for a region, this suggests that COVID-19 may have a disparate 
                        impact on that group in that region. Social and economic determinants may contribute to this disparity. <br><br>"),
                   HTML("For each racial/ethnic group, the proportion of COVID-19 deaths for that group is:<br>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than population percentage for disparity index &gt; 0.2</div>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #f7f7f7; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to the population percentage for -0.2 &lt;disparity index &lt; 0.2</div>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than population percentage for disparity index &lt; -0.2</div>
                               <i>Darker shades indicate greater disparity.</i><br><br>
                               
                               <strong>Group COVID-19 Death Percentage</strong> = number of COVID-19 deaths for group/total COVID-19 deaths<br>
                               <strong>Population Percentage</strong> = number of residents from that group/ total number of residents<br>
                               <strong>Death Rate Disparity Index</strong> = log(Group COVID-19 Death Percentage/Population Percentage)
                               <br>
                        </div>"
                   ),
                   HTML(paste0("<div>
                               <br><br>
                               <strong>Date: </strong>",update_date,"<br><br>
                               <b>DATA SOURCE:</b> <a href='https://on.ny.gov/2VehafT'>New York State Dept. of Health COVIDTracker (daily)</a><br>
                               </div>")))
                 )
               )
      ),
      tabPanel(tags$div(class="tab-title",style="text-align:center;",
                        HTML("<div><b>OUTCOME (CT)</b></br>COVID-19 Racial Disparity</div>")),
               value="outcome_ct_racial_disparity",
               fluidPage(
                 fluidRow(class="page_title", tags$h1("OUTCOME: Connecticut Racial Disparities of COVID-19 mortality")),
                 fluidRow(class="page_title", tags$h2("Do minorities in Connecticut make up a higher percentage of COVID-19 deaths when compared to 
                        their population percentage?")),
                 fluidRow(class = "map-container",
                 column(8, id = "mainpanel_ct_race", 
                          plotOutput(outputId = "NY.race.ct", height=height)),
                 column(4, 
                   id = "sidebar_ct_race",
                   #HTML(whatisit_text),
                   HTML("
                        <div>
                        <a href='https://bit.ly/2Krl5RG'>Evidence suggests</a> that COVID-19 deaths may be higher for certain racial/ethnic groups.<br><br>
                        If the percentage  of COVID-19 deaths experienced by a racial/ethnic group is higher than that 
                        group’s population percentage for a region, this suggests that COVID-19 may have a disparate 
                        impact on that group in that region. Social and economic determinants may contribute to this disparity. <br><br>"),
                   HTML("For each racial/ethnic group, the proportion of COVID-19 deaths for that group is:<br>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than population percentage for disparity index &gt; 0.2</div>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #f7f7f7; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to the population percentage for -0.2 &lt;disparity index &lt; 0.2</div>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than population percentage for disparity index &lt; -0.2</div>
                               <i>Darker shades indicate greater disparity.</i><br><br>
                               
                               <strong>Group COVID-19 Death Percentage</strong> = number of COVID-19 deaths for group/total COVID-19 deaths<br>
                               <strong>Population Percentage</strong> = number of residents from that group/ total number of residents<br>
                               <strong>Death Rate Disparity Index</strong> = log(Group COVID-19 Death Percentage/Population Percentage)
                               <br>
                        </div>"
                   ),
                   HTML(paste0("<div>
                               <br><br>
                               <strong>Date: </strong>",update_date,"<br><br>
                               <b>DATA SOURCE:</b> <a href='https://bit.ly/3bJ77GZ'>ct.gov</a><br>
                               </div>")))
               )
      )
      )),
      navbarMenu(menuName = "mediation_menu",
                 #HTML("<div style='font-size:90%;line-height:1.3;'><b>MEDIATION</b><br>Select a USA mediation</div>"),
                 HTML("<div><b>MEDIATION</b></div>"),
                 tabPanel(tags$div(class="tab-title",style="text-align:center;",
                        HTML("<div><b>MEDIATION (USA)</b></br>COVID-19 Testing</div>")),
               value="mediation_usa_testing",
               fluidPage(
                 fluidRow(class="page_title", tags$h1("MEDIATION: Nationwide testing disparities compared to top testing countries")),
                 fluidRow(class="page_title", tags$h2("What are the disparities between US states  in  their rates of COVID-19 testing per 1k population 
                  when compared to the average rates from other countries? When compared with the current average
                  US rate?")),
                 fluidRow(class = "map-container",
                 column(8, id = "mainpanel_us_test",
                        tags$h3(class="map-title", paste0("COVID-19 Testing Rate Disparities by State Compared to Selected Country")),
                        HTML("<br><br>"),
                        tags$div(class="select-bar",
                                 selectInput(inputId = "country",
                                             label = NULL,
                                             choices = country_testing_choices,
                                             selected = "de")),
                        leafletOutput(outputId = "map.testing", height=height)),
                 column(4,
                   id = "sidebar_us_test",
                   #HTML(whatisit_text),
                   HTML(paste0("
                  <div>
                  Several countries significantly effected by COVID-19 can be used as testing reference rates. 
                  Some of these countries are regarded as having successfully  used testing to “flatten the curve”,
                  while others are still in the midst of dealing with the crisis.<br><br>
                   The rate of testing per 1k in a state is: <br>
                     <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than selected country testing rate for disparity index &gt; 0.2</div>
                     <div>&nbsp;&nbsp;&nbsp;<span style='background: #f7f7f7; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to selected country testing rate for -0.2 &lt; disparity index &lt; 0.2</div>
                     <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than selected country testing rate for disparity index &lt; -0.2</div>
                   <i>Darker shades indicate greater disparity.</i><br><br>
                   
                   <strong>Testing Rate</strong> = number of COVID-19 tests per 1K population <br>
                   <strong>Testing Rate Disparity Index</strong> = log(Testing Rate in state/Testing Rate in selected country) <br>
        <strong>Date: </strong>",update_date,"<br><br>
                   
                   <b>DATA SOURCES:</b> <a href='http://bit.ly/39PMWpD'>JHU CSSE (daily)</a>, 
                   <a href='https://ourworldindata.org/coronavirus'>Our World in Data</a>
                   </div>")))
                 )
               )
      ),
      tabPanel(tags$div(class="tab-title",style="text-align:center;",
                        HTML("<div><b>MEDIATION (USA)</b></br>Hospital Beds</div>")),
               value="mediation_usa_hospital_beds",
               fluidPage(
                 fluidRow(class="page_title", tags$h1("MEDIATION: Nationwide Hospital Bed Disparities vs Italy")),
                 fluidRow(class="page_title", tags$h2("What are the disparities between states  in  the rate of hospital beds 
                                per 100k population when compared to the rate in Italy?")),
                 fluidRow(class = "map-container",
                 column(8,id = "mainpanel_us_hosp",
                       tags$h3(class="map-title", "COVID-19 Hospital Bed Rate Disparities by State Compared to Average Italian Rate"),
                       leafletOutput(outputId = "map.hospital", height=height)),
                 column(4,
                   id = "sidebar_us_hosp",
                   #HTML(whatisit_text),
                   HTML(paste0("
                                <div>
                                Italy has a higher hospital 
                                bed rate than the US, yet still faced challenges meeting peak COVID bed needs. Thus we use 
                                Italy’s rate as a minimum target rate.<br><br>
                                The rate of hospital beds per 100k in a state is<br>
                                 <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than Italian rate for disparity index &gt; 0.2</div>
                                 <div>&nbsp;&nbsp;&nbsp;<span style='background: #f7f7f7; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to Italian rate for -0.2 &lt;disparity index &lt; 0.2</div>
                                 <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than Italian rate for disparity index &lt; -0.2</div>
                               <i>Darker shades indicate greater disparity.</i><br><br>
                               
                               <strong>Testing Rate</strong> = number of COVID-19 tests per 100K population <br>
                               <strong>Testing Rate Disparity Index</strong> = log(Testing Rate  in state/Testing Rate in Italy) <br>
                               <strong>Date: </strong>",update_date,"<br><br>
                               
                               <b>DATA SOURCE:</b> <a href='https://bit.ly/2V0CYLU'>Kaiser Family Foundation</a><br>

                               </div>")))
                 )
               )
      )),
      navbarMenu(menuName ="determinant_menu",
                 HTML("<div><b>DETERMINANT</b></div>"),
               tabPanel(tags$div(class="tab-title",style="text-align:center;",
                     HTML("<div><b>DETERMINANT</b></br>USA</div>")),
               value="determinant_usa",
               fluidPage(
                 fluidRow(class="page_title", uiOutput("us_det_title")),
                 fluidRow(class="page_title", uiOutput("us_det_subtitle")),
                 fluidRow(class = "map-container",
                column(8, id = "mainpanel_us_db",
                       tags$h3(class="map-title", textOutput("us_det_map_title")),
                       tags$br(),tags$br(),
                       tags$div(class = "select-bar",
                                selectInput(inputId = "determinant",
                                            label = NULL,
                                            choices = c("Diabetes", "Obesity", "Heart Disease"),
                                            selected = "Diabetes"
                                )),
                       leafletOutput(outputId = "map.determinant", height=height)),
                 column(4,
                   id = "sidebar_us_db",
                               uiOutput("sb_us_det_output"),
                               HTML(
                               "<div>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than US avg. rate for disparity index &gt; 0.2</div>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #f7f7f7; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to US avg. rate for -0.2 &lt;disparity index &lt; 0.2</div>
                               <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than US avg. rate for disparity index &lt; -0.2</div>
                               <i>Darker shades indicate greater disparity.</i><br><br></div>"),
                               uiOutput("sb_us_det_footer")))
               )
      ),
      tabPanel(tags$div(class="tab-title",style="text-align:center;",
                        HTML("<div><b>DETERMINANT</b></br>NY</div>")),
               value="determinant_ny",
               fluidPage(
                 fluidRow(class="page_title", uiOutput("ny_det_title")),
                 fluidRow(class="page_title", uiOutput("ny_det_subtitle")),
                 fluidRow(class = "map-container",
                 column(8, id = "mainpanel_ny_det",
                   tags$h3(class="map-title", textOutput("ny_det_map_title")),
                   tags$br(),tags$br(),
                   tags$div(class = "select-bar",
                            selectInput(inputId = "determinant_NY",
                                        label = NULL,
                                        choices = c("Diabetes", "Obesity"), # , "Obesity", "Heart Disease"
                                        selected = "Diabetes"
                            )),
                   leafletOutput(outputId = "map.NY.determinant", height=height)),
                 column(4,
                   id = "sidebar_ny_det",
                   uiOutput("sb_ny_det_output"),
                   HTML("
                        <div>
                       <div>&nbsp;&nbsp;&nbsp;<span style='background: #BD0026; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Higher</strong> than US avg. rate for disparity index &gt; 0.2</div>
                       <div>&nbsp;&nbsp;&nbsp;<span style='background: #f7f7f7; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> About equal</strong> to US avg. rate for -0.2 &lt; disparity index &lt; 0.2</div>
                       <div>&nbsp;&nbsp;&nbsp;<span style='background: #253494; border-radius: 50%; font-size: 11px; opacity: 0.7;'>&nbsp&nbsp&nbsp&nbsp</span><strong> Lower</strong> than US avg. rate for disparity index &lt; -0.2</div>
                       <i>Darker shades indicate greater disparity.</i><br><br>
                       </div>"),
                   uiOutput("sb_ny_det_footer")))
               )
      )
      ),
      navbarMenu(menuName ="about_menu",
                 #HTML("<div style='font-size:90%;line-height:1.3;'><b>ABOUT</b><br>Project Information</div>"),
                 HTML("<div><b>ABOUT</b></div>"),
                 tabPanel(tags$div(class="tab-title",style="text-align:center;",
                        HTML("<div><b>About</b></div>")),
               value="about",
               fluidRow(
                 column(8,offset=2,class="about",
                        HTML(whatisit_text_abt),
                        HTML(footer_text))
               )
               )
      )
    ), 
    # Footer
    fluidRow(
      column(12, class = "footer",
             hr(),
             HTML("<a href='?tab=about'>About</a>&emsp;"),
             HTML("<a href='https://idea.rpi.edu/'>Institute for Data Exploration and Applications (IDEA)</a>&emsp;"),
             HTML("<a href='https://github.com/TheRensselaerIDEA/COVIDMINDER'>COVIDMINDER GitHub</a>&emsp;"),
             HTML("<a href='https://info.rpi.edu/statement-of-accessibility'>Accessibility</a>&emsp;"),
             HTML("<a href='https://forms.gle/8LwiYAVXXN7mu9wR6'>
                  <span title='Thanks for using COVIDMINDER! Please take a few moments to fill out our short comments form.'>Comments</span></a>")
             )
    )
    #,tags$script(src = "style.js")
  )

#### Server Code ####
server <- function(input, output, session) {
  # Leaflet plot colors
  colors <- c("#253494","#4575B4", "#74ADD1","#ABD9E9","#f7f7f7","#FDAE61","#F46D43", "#D73027", "#BD0026")
  bins <- c(5, 3, 2, 1, .2, -.2, -1, -2, -3, -5)
  
  # Join NY shape and data, may move to processing 
  
  # Render leaflet plot with all information in hover
  output$map.testing <- renderLeaflet({
    # browser()
    country <- input$country # selected country
    
    # modify states to have selected columns for our plot
    tests_ldi <- states %>% 
      select(starts_with("tests_ldi")) %>%
      select(ends_with(country))
    
    states <- data.frame(states, "tests_ldi"=unlist(tests_ldi)) # Append to states
    
    
    pal2 <- leaflet::colorBin(colors, domain = states$tests_ldi, bins = bins, reverse=TRUE)
#    browser()
    labels2 <- sprintf(
      paste0("<strong>%s</strong> State<br/>
      Testing Rate vs ", toupper(country)," DI: %.2g<br>
      Testing Rate: %.1f /1000"),
      states$NAME, states$tests_ldi, states$tests_per_1000
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
                title = paste0("Disparity Index<br/>US Total Tests vs. ",toupper(country)),
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
  
  output$map.determinant <- renderLeaflet({
    det <- input$determinant
    if ("Diabetes" %in% det) {
      states$ldi <- states$diabetes_rate_ldi
      states$pct <- states$pct_Adults_with_Diabetes*1000
    }
    else if ("Obesity" %in% det) {
      states$ldi <- states$obesity_ldi.us
      states$pct <- states$pct_Adults_with_Obesity * 1000
    }
    else if ("Heart Disease" %in% det) {
      states$ldi <- states$cardio_death_rate_BNH_ldi
      states$pct <- states$cardio_deaths_p_Black_Non_Hispanic
      det <- paste0(det, " Death")
    }
    
    labels2 <- sprintf(
      paste0("<strong>%s</strong><br/>",
        det, " Rate DI: %.2g<br/>",
        det, " Rate: %.1f per 100k"),
      states$NAME, states$ldi, states$pct
    ) %>% lapply(htmltools::HTML)
    pal2 <- leaflet::colorBin(colors, domain = states$ldi, bins = bins, reverse=FALSE)
    
    leaflet(states.shapes) %>%
      setView(-96, 37.8, 4) %>% 
      addPolygons(
        fillColor = ~pal2(states$ldi),
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
                opacity = 0.7, title = paste0("Disparity Index<br/>US ",det, " Rate"),
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
  }
  )
  
  output$us_det_map_title <- renderText ({
    select.det <- input$determinant
    paste0("US ",select.det," Rate Disparities by State Compared to Average US Rate")
  })
  
  output$ny_det_map_title <- renderText ({
    select.det <- input$determinant_NY
    paste0("NY ",select.det," Rate Disparities by County Compared to Average US Rate")
  })
  
  output$us_det_title <- renderUI ({
    select.det <- input$determinant
    tags$h1(paste0("DETERMINANT: Nationwide ",select.det," Disparities"))
  })
  
  output$ny_det_title <- renderUI ({
    select.det <- input$determinant_NY
    tags$h1(paste0("DETERMINANT: New York ",select.det," Disparities"))
  })
  
  output$us_det_subtitle <- renderUI ({
    select.det <- input$determinant
    if ( select.det == "Diabetes") {
      tags$h2("What are the disparities between states in rate of diabetes patients 
              per 100k population per state when compared to the average United States rate?")
    }
    else if ( select.det == "Obesity") {
      tags$h2("What are the disparities between states in percent of obese patients 
              per state when compared to the average United States rate?")
    }
    else if ( select.det == "Heart Disease") {
      tags$h2("What are the disparities between states in rate of deaths (black non-hispanic) due to heart disease
               per 100k population per state when compared to the average United States rate?")
    }
  })
  
  output$ny_det_subtitle <- renderUI ({
    select.det <- input$determinant_NY
    if ( select.det == "Diabetes") {
      tags$h2("What are the disparities between New York counties in the rate 
                                of diabetes patients per 100k population when compared to the average United 
                                States rate?")
    }
    else if ( select.det == "Obesity") {
      tags$h2("What are the disparities between New York counties in the rate 
                                of people with Obesitys per 100k population when compared to the average United 
                                States rate?")
    }
    else if ( select.det == "Heart Disease") {
      
    }
  })
  
  output$sb_us_det_output <- renderUI ({
    select.det <- input$determinant
    if ( select.det == "Diabetes") {
      tagList(
        tags$p("Diabetes puts patients at increased risk of contracting and dying from COVID-19, 
      so areas with higher diabetes rates may face increased COVID-19 burdens."),
        tags$p("The  rate of diabetes deaths per 100k in a state is:")
      )
    }
    else if ( select.det == "Obesity") {
      tagList(
        tags$p("Obesity puts patients at increased risk of contracting and dying from COVID-19, so areas with higher rates of obesity may face increased COVID-19 burdens."),
        tags$p("The  rate of obesity in a state is:")
      )
    }
    else if ( select.det == "Heart Disease") {
      tagList(
        tags$p("Heart disease patients at increased risk of contracting and dying from COVID-19,
                                 so areas with a history of higher heart disease mortality may face increased COVID-19 burdens.
                                 Furthermore, some ethnic groups have higher mortality rates due to heart disease than other groups."),
        tags$p("The rate of deaths due to heart disease (black non-hispanic) per 100k in a state is:")
      )
    }
  })
  
  output$sb_ny_det_output <- renderUI ({
    select.det <- input$determinant_NY
    if ( select.det == "Diabetes") {
      tagList(
        tags$p("Diabetes puts patients at increased risk of contracting and dying from COVID-19, 
                so areas with higher diabetes rates may face increased COVID-19 burdens."),
        tags$p("The rate of diabetes patients per 100k in a county is:")
      )
    }
    else if ( select.det == "Obesity") {
      tagList(
        tags$p("Obesity puts patients at increased risk of contracting and dying from COVID-19, so areas with higher rates of obesity may face increased COVID-19 burdens."),
        tags$p("The  rate of obesity in a county is:")
      )
    }
    else if ( select.det == "Heart Disease") {
      #tagList(
      #  tags$p("Heart disease patients at increased risk of contracting and dying from COVID-19,
      #                           so areas with a history of higher heart disease mortality may face increased COVID-19 burdens.
      #                           Furthermore, some ethnic groups have higher mortality rates due to heart disease than other groups."),
      #  tags$p("The rate of deaths due to heart disease (black non-hispanic) per 100k in a state is:")
      #)
    }
  })
  
  output$sb_us_det_footer <- renderUI ({
    select.det <- input$determinant
    if ( select.det == "Diabetes") {
      tagList(
        tags$div(HTML(paste0(
          "<strong>Diabetes Rate</strong> = number of diabetic patients per 100K population <br>
          <strong>Diabetes Disparity Index</strong> = log(Diabetes Rate in state/average Diabetes Rate in US)<br>
          <strong>Date: </strong>","2020"," <br><br>
          <b>DATA SOURCE:</b> <a href='https://bit.ly/34mYLBP'>County Health Rankings</a> and 
          <a href='https://bit.ly/2V1Zl3I'>CDC</a><br>")))
      )
    }
    else if ( select.det == "Obesity") {
      tagList(
        tags$div(HTML(paste0(
          "<strong>Obesity Rate</strong> = number of obese patients per 100K population <br>
                                <strong>Obesity Disparity Index</strong> = log(Obesity Rate in state/average Obesity Rate in US)<br>
                                <strong>Date: </strong>","2016","<br><br>
                                
                                <b>DATA SOURCE:</b> <a href='https://stateofchildhoodobesity.org/adult-obesity/'>State of Childhood Obesity</a>"
        )))
      )
    }
    else if ( select.det == "Heart Disease") {
      tags$div(HTML(
        "<strong>Heart Disease Death Rate (BNH)</strong> = number of heart disease deaths (black non-hispanic) per 100K population <br>
                                <strong>Heart Disease Death Disparity Index (BNH)</strong> = log(Heart Disease Death Rate (BNH) in state/average Heart Disease Death Rate in US)<br>
                                <strong>Date: </strong> 2015<br><br>
       
                                <b>DATA SOURCE:</b> <a href='https://sortablestats.cdc.gov/#/indicator'>CDC</a><br>"
      ))
    }
  })
  output$sb_ny_det_footer <- renderUI ({
    select.det <- input$determinant_NY
    if ( select.det == "Diabetes") {
      tagList(
        tags$div(HTML(paste0(
          "  <strong>Diabetes Rate</strong> = number of diabetic patients  per 100K population <br>
             <strong>Diabetes Disparity Index</strong> = log(Diabetes Rate in state/average Diabetes Rate in US)<br>
             <strong>Date: </strong> 2020<br><br>
             <b>DATA SOURCE:</b> <a href='https://bit.ly/34mYLBP'>County Health Rankings</a> and 
             <a href='https://bit.ly/2V1Zl3I'>CDC</a><br>")))
      )
    }
    else if ( select.det == "Obesity") {
      tagList(
        tags$div(HTML(paste0(
          "  <strong>Obesity Rate</strong> = number of self reported Obese people per 100K population <br>
             <strong>Obesity Disparity Index</strong> = log(Obesity Rate in state/average Obesity Rate in US)<br>
             <strong>Date: </strong> 2016<br><br>
             <b>DATA SOURCE:</b> <a href='https://bit.ly/34mYLBP'>County Health Rankings</a> and 
             <a href='https://bit.ly/2V1Zl3I'>CDC</a><br>")))
      )
    }
    else if ( select.det == "Heart Disease") {
    }
  })

  output$map.hospital <- renderLeaflet({
    
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
    
    pal2 <- leaflet::colorBin(colors, domain = states$death_rate_ldi, bins = bins, reverse=FALSE)
    
    labels2 <- sprintf(
      "<strong>%s</strong><br/>
      COVID-19 Mortality Rate DI: %.2g<br>
      COVID-19 Mortality Rate: %.1f /100k<br><br>",
      # Total COVID-19-related Executive Orders: %.0f<br>
      # Total COVID-19-related Bills: %.0f" ,
        states$NAME, states$death_rate_ldi , 
        states$covid_death_rate*100000
#        states$covid_eo, states$covid_bills
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

  output$map.covid_deaths.race <- renderLeaflet({
    
    race <- input$race # selected race

    # modify states to have selected columns for our plot
    death_rate_ldi_race <- states %>% 
      select(starts_with("death")) %>%
      select(ends_with(race))

    race_deaths_pct <- states %>% 
      select(starts_with(race)) %>%
      select(ends_with("deaths_pct"))
    
    race_wd_pop_pct <- states %>% 
      select(starts_with(race)) %>%
      select(ends_with("wd_pop_pct"))
    
    states <- data.frame(states, "death_rate_ldi_race"=unlist(death_rate_ldi_race)) # Append to states
    states <- data.frame(states, "race_deaths_pct"=unlist(race_deaths_pct)) # Append to states
    states <- data.frame(states, "race_wd_pop_pct"=unlist(race_wd_pop_pct)) # Append to states

    pal2 <- leaflet::colorBin(colors, domain = states$death_rate_ldi_race, bins = bins, reverse=FALSE)
    
    labels2 <- sprintf(
      paste0("<strong>%s</strong> (",toupper(race),")<br/>
      COVID-19 Mortality Pct DI: %.2g<br>
      COVID-19 Mortality Pct: %.1f<br>
      Percentage of population (weighted): %.1f<br><br>"),
      # Total COVID-19-related Executive Orders: %.0f<br>
      # Total COVID-19-related Bills: %.0f"),
      states$NAME, states$death_rate_ldi_race, 
      states$race_deaths_pct, 
      states$race_wd_pop_pct
      # states$covid_eo,states$covid_bills
    ) %>% lapply(htmltools::HTML)
    
    leaflet(states.shapes) %>%
      setView(-96, 37.8, 4) %>% 
      addPolygons(
        fillColor = ~pal2(states$death_rate_ldi_race),
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
                values = ~states$death_rate_ldi_race, 
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
    
    
    pal2 <- leaflet::colorBin(colors, domain = NY.data$death_rate_ldi, bins = bins, reverse=FALSE)
    
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
    
    pal2 <- leaflet::colorBin(colors, domain = NY.data$case_rate_ldi, bins = bins, reverse=FALSE)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>
      COVID-19 Case Rate DI: %.2g<br>
      COVID-19 Case Rate: %.1f /100k",
#      NY.data$County, NY.data$case_rate_ldi, (NY.data$cases/NY.data$Population)*100000
      NY.data$County, NY.data$case_rate_ldi, NY.data$case_rate*100000
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
  
  output$map.NY.determinant <- renderLeaflet({
    det <- input$determinant_NY
    if ("Diabetes" %in% det) {
      NY.data$ldi <- NY.data$diabetes_ldi
      NY.data$pct <-  NY.data$pct_Adults_with_Diabetes*1000
    }
    else if ("Obesity" %in% det) {
      NY.data$ldi <- NY.data$obesity_ldi
      NY.data$pct <-  NY.data$pct_Adults_with_Obesity*1000
    }
    
    pal2 <- leaflet::colorBin(colors, domain = NY.data$ldi, bins = bins, reverse=FALSE)
    labels <- sprintf(
      paste0("<strong>%s</strong><br/>",
      det," Rate DI: %.2g<br>",
      det," Rate: %.1f per 100k"),
      NY.data$County, NY.data$ldi, NY.data$pct
    ) %>% lapply(htmltools::HTML)
    
    leaflet(NY.shape) %>%
      setView(-76.071782, 42.991989, 6) %>%  # Set to the geographic center of NY
      addPolygons(
        fillColor = ~pal2(NY.data$ldi),
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
                title = paste0("Disparity Index<br/>NY ",det," Rates"),
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

  # This sets the range for zooming the following plot
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$NY.cases.ma <- renderPlot({
    # browser()
    selected.region <- input$NYRegion3
    select.rate <- input$rate.ma
    select.date <- input$NYDate.ma
    range <- as.numeric(select.date[2]) - as.numeric(select.date[1])
    
    #width <- input$width
    select.size <- 2
    
    if (selected.region == "All Regions") {
      selected.region <- sort(unique(covid_NY_TS_plot.cases$Region))
      select.size <- 1
    }
    
    if (select.rate=="Overall") {
      covid_NY_TS_plot.ma <- covid_NY_TS_plot.cases %>%
      group_by(Region, date) %>%
      summarise(diff = sum(diff)) %>%
      mutate(ma = c(numeric(moving.avg.window-1), zoo::rollmean(diff, moving.avg.window, align = "right"))) %>%
      filter(ma > 0)
      y_lab <- paste0("New Cases (",moving.avg.window," day Average)")
      gg_title <- paste0("New York State New COVID-19 Case Trends (",moving.avg.window," day Average)")
    }
    else {
      covid_NY_TS_plot.ma <- covid_NY_TS_plot.cases %>%
      group_by(Region, date) %>%
      summarise(p_diff = sum(p_diff)) %>%
      mutate(ma = c(numeric(moving.avg.window-1), zoo::rollmean(p_diff, moving.avg.window, align = "right"))) %>%
      filter(ma > 0)
      y_lab <- paste0("New Cases (",moving.avg.window," day Average) per 100k")
      gg_title <- paste0("New York State New COVID-19 Case Trends per 100k (",moving.avg.window," day Average)")
    }
    
    #if (width > 844) {
    if (TRUE) {
      tick.legend <- NULL
    }
    else {
      tick.legend <- theme(legend.position = "none")
    }
    
    
    highlight_points <- covid_NY_TS_plot.ma %>%
      dplyr::filter( 
          Region == "Adirondack" & date == select.date[1] + ((1*((range%/%11)+1)) %% range) |
            Region == "Capital District" & date == select.date[1] + ((2*((range%/%11)+1)) %% range) |
            Region == "Catskill" & date == select.date[1] + ((3*((range%/%11)+1)) %% range) |
            Region == "Central New York" & date == select.date[1] + ((4*((range%/%11)+1)) %% range) |
            Region == "Chautauqua-Alleghany" & date == select.date[1] + ((5*((range%/%11)+1)) %% range) |
            Region == "Eastern Hudson Valley" & date == select.date[1] + ((6*((range%/%11)+1)) %% range) |
            Region == "Finger Lakes" & date == select.date[1] + ((7*((range%/%11)+1)) %% range) |
            Region == "Long Island" & date == select.date[1] + ((8*((range%/%11)+1)) %% range) |
            Region == "New York City" & date == select.date[1] + ((9*((range%/%11)+1)) %% range) |
            Region == "New York State" & date == select.date[1] + ((10*((range%/%11)+1)) %% range) |
            Region == "Niagara Frontier" & date == select.date[1] + ((11*((range%/%11)+1)) %% range) |
            Region == "Thousand Island" & date == select.date[1] + ((12*((range%/%11)+1)) %% range)
      )
    
    NY_region_palette.df <- NY_counties_regions %>%
      dplyr::select(Region,Color) %>% 
      dplyr::distinct(Region,Color)
    
    NY_region_palette <- setNames(as.character(NY_region_palette.df$Color), as.character(NY_region_palette.df$Region))
    

    
    covid_NY_TS_plot.ma %>%
      filter(date >= select.date[1] & date <= select.date[2]) %>%
      ggplot(aes(date, 
                 ma, 
                 color = Region)) +
      scale_color_manual(values=NY_region_palette) +
      geom_line(size=1) +
      scale_y_continuous(
        trans = "log10",
        breaks = c(10,25,100,250,500,1000,2500,5000,10000)
      ) +
      scale_x_datetime(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%b %d") +
      ylab(y_lab) + 
      xlab("Date") +
      ggtitle(gg_title)  +  
      gghighlight(Region %in% selected.region, use_direct_label=FALSE) +
      geom_line(size=select.size) + 
      geom_label_repel(data=highlight_points,  aes(label=Region), box.padding = unit(1.75, 'lines')) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
      geom_vline(aes(xintercept=as_datetime("2020-03-20"), linetype="Gov. Cuomo issues stay-at-home order"), color = "black") + 
      scale_linetype_manual(name = "Events", 
                            values = c(2), 
                            guide = guide_legend(override.aes = list(color = c("black")))) +
      tick.legend +
      NULL
    
  })
  
  
  output$NY.cases.TS <- renderPlot({
    # browser()
    #selected.region <- input$NYRegion
    selected.county <- input$NYCounty
    select.rate <- input$rate.CoT
    select.date <- input$NYCoTDate
    
    range <- as.numeric(select.date[2]) - as.numeric(select.date[1])
    #print(range)
    
    select.size <- 2
    if (selected.county == "All Counties") {
      selected.county <- sort(unique(covid_NY_TS_plot.cases$County))
      select.size <- 1
    }
    
    if (select.rate=="Overall") {
      covid_NY_TS <- covid_NY_TS_plot.cases %>%
        mutate(y = cases)
      y_lab <- "Cumulative Number of Cases"
      title <- "New York State COVID-19 Cases per County (Mar-May 2020)"
    }
    else {
      covid_NY_TS <- covid_NY_TS_plot.cases %>%
        mutate(y = p_cases) %>%
        filter(y >= 10)
      y_lab <- "Cases per 100K Population"
      title <- "New York State COVID-19 Cases per 100K Population by County (Mar-May 2020)"
    }
    
    highlight_points <- covid_NY_TS %>% 
      dplyr::filter( 
        County == "Albany" & date == select.date[1] + ((1*((range%/%40)+1)) %% range) |
          # County == "Allegany" & date == as.Date("2020-03-29") |
          County == "Bronx" & date == select.date[1] + ((3*((range%/%40)+1)) %% range) |
          County == "Broome" & date == select.date[1] + ((4*((range%/%40)+1)) %% range) |
          # County == "Cattaraugus" & date == as.Date("2020-03-30") |
          County == "Cayuga" & date == select.date[1] + ((6*((range%/%40)+1)) %% range) |
          County == "Chautauqua" & date ==select.date[1] + ((7*((range%/%40)+1)) %% range) |
          # County == "Chemung" & date == as.Date("2020-04-10") |
          County == "Chenango" & date == select.date[1] + ((9*((range%/%40)+1)) %% range) |
          County == "Clinton" & date == select.date[1] + ((10*((range%/%40)+1)) %% range) |
          # County == "Columbia" & date == as.Date("2020-03-29") |
          County == "Cortland" & date == select.date[1] + ((12*((range%/%40)+1)) %% range) |
          # County == "Delaware" & date == as.Date("2020-04-02") |
          County == "Dutchess" & date == select.date[1] + ((14*((range%/%40)+1)) %% range)|
          County == "Erie" & date == select.date[1] + ((15*((range%/%40)+1)) %% range) |
          # County == "Essex" & date == as.Date("2020-04-10") |
          # County == "Franklin" & date == as.Date("2020-04-10") |
          # County == "Fulton" & date == as.Date("2020-04-12") |
          County == "Genesee" & date == select.date[1] + ((19*((range%/%40)+1)) %% range) |
          # County == "Greene" & date == as.Date("2020-03-29") |
          County == "Hamilton" & date == select.date[1] + ((20*((range%/%40)+1)) %% range) |
          County == "Herkimer" & date == select.date[1] + ((21*((range%/%40)+1)) %% range) |
          # County == "Jefferson" & date == as.Date("2020-03-30") |
          County == "Kings" & date ==select.date[1] + ((23*((range%/%40)+1)) %% range) |
          # County == "Lewis" & date == as.Date("2020-04-10") |
          # County == "Livingston" & date == as.Date("2020-04-10") |
          County == "Madison" & date == select.date[1] + ((26*((range%/%40)+1)) %% range) |
          # County == "Monroe" & date == as.Date("2020-03-26") |
          # County == "Montgomery" & date == as.Date("2020-03-29") |
          County == "Nassau" & date == select.date[1] + ((27*((range%/%40)+1)) %% range) |
          County == "New York" & date == select.date[1] + ((28*((range%/%40)+1)) %% range) |
          County == "New York State" & date == select.date[1] + ((29*((range%/%40)+1)) %% range) |
          County == "Manhattan" & date == select.date[1] + ((30*((range%/%40)+1)) %% range) |
          County == "Niagara" & date == select.date[1] + ((31*((range%/%40)+1)) %% range) |
          County == "Oneida" & date == select.date[1] + ((32*((range%/%40)+1)) %% range) |
          County == "Onondaga" & date == select.date[1] + ((33*((range%/%40)+1)) %% range) |
          # County == "Ontario" & date == as.Date("2020-04-12") |
          County == "Orange" & date == select.date[1] + ((35*((range%/%40)+1)) %% range) |
          County == "Orleans" & date == select.date[1] + ((36*((range%/%40)+1)) %% range) |
          County == "Oswego" & date == select.date[1] + ((37*((range%/%40)+1)) %% range) |
          County == "Otsego" & date == select.date[1] + ((38*((range%/%40)+1)) %% range) |
          County == "Putnam" & date == select.date[1] + ((39*((range%/%40)+1)) %% range) |
          County == "Queens" & date == select.date[1] + ((40*((range%/%40)+1)) %% range) |
          County == "Rensselaer" & date == select.date[1] + ((41*((range%/%40)+1)) %% range) |
          County == "Richmond" & date == select.date[1] + ((42*((range%/%40)+1)) %% range) |
          County == "Rockland" & date == select.date[1] + ((43*((range%/%40)+1)) %% range) |
          County == "St. Lawrence" & date == select.date[1] + ((44*((range%/%40)+1)) %% range) |
          County == "Saratoga" & date == select.date[1] + ((45*((range%/%40)+1)) %% range) |
          County == "Schenectady" & date == select.date[1] + ((46*((range%/%40)+1)) %% range) |
          County == "Schoharie" & date == select.date[1] + ((47*((range%/%40)+1)) %% range) |
          County == "Schuyler" & date == select.date[1] + ((48*((range%/%40)+1)) %% range) |
          County == "Seneca" & date == select.date[1] + ((49*((range%/%40)+1)) %% range) |
          # County == "Steuben" & date == as.Date("2020-04-10") |
          County == "Suffolk" & date == select.date[1] + ((51*((range%/%40)+1)) %% range) |
          County == "Sullivan" & date == select.date[1] + ((52*((range%/%40)+1)) %% range) |
          # County == "Tioga" & date == as.Date("2020-03-26") |
          County == "Tompkins" & date == select.date[1] + ((53*((range%/%40)+1)) %% range) |
          County == "Ulster" & date == select.date[1] + ((54*((range%/%40)+1)) %% range) |
          # County == "Warren" & date == as.Date("2020-04-02") |
          # County == "Washington" & date == as.Date("2020-03-30") |
          # County == "Wayne" & date == as.Date("2020-04-02") |
          County == "Westchester" & date == select.date[1] + ((58*((range%/%40)+1)) %% range) |
          # County == "Wyoming" & date == as.Date("2020-04-10") |
          County == "Yates" & date == select.date[1] + ((60*((range%/%40)+1)) %% range)
      )
    
    NY_region_palette.df <- NY_counties_regions %>%
      dplyr::select(Region,Color) %>% 
      dplyr::distinct(Region,Color)
    
    NY_region_palette <- setNames(as.character(NY_region_palette.df$Color), as.character(NY_region_palette.df$Region))
    
    covid_NY_TS %>%
      filter(date >= select.date[1] & date <= select.date[2]) %>%
      ggplot(aes(date, 
                 y, 
                 color = Region,
                 group = County)) +
      scale_color_manual(values=NY_region_palette) +
      geom_line(size=1) +
      scale_y_continuous(
        trans = "log10",
        breaks = c(10,100,500,1000,5000,10000, 50000)
      ) +
      scale_x_datetime(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%b %d") +
      ylab(y_lab) + 
      xlab("Date") +
      ggtitle(title)  +  
      gghighlight(County %in% selected.county, use_direct_label=FALSE) +
      geom_line(size=select.size) + 
      geom_label_repel(data=highlight_points,  aes(label=County), box.padding = unit(1.75, 'lines')) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
      geom_vline(aes(xintercept=as_datetime("2020-03-20"), linetype="Gov. Cuomo issues stay-at-home order"), color = "black") + 
      scale_linetype_manual(name = "Events", 
                            values = c(2), 
                            guide = guide_legend(override.aes = list(color = c("black")))) +
      NULL
      })
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$NY.cases.TS.region <- renderPlot({
    # browser()
    selected.region <- input$NYRegion2
    select.size <- 2
    select.rate <- input$rate.CoT.reg
    select.date <- input$NYRegionDate
    range <- as.numeric(select.date[2]) - as.numeric(select.date[1])
    
    if (selected.region == "All Regions") {
      selected.region <- sort(unique(covid_NY_TS_plot.cases$Region))
      select.size <- 1
    }
    
    
    covid_NY_TS.reg <- covid_NY_TS_plot.cases %>% 
      group_by(Region, date) %>%
      summarise(cases = sum(cases), p_cases = mean(p_cases))
    
    if (select.rate == "Overall") {
      covid_NY_TS.reg <- covid_NY_TS.reg %>%
        mutate(y = cases)
      y_lab <- "Cumulative Number of Cases"
      title <- "New York State COVID-19 Cases per Region (Mar-May 2020)"
    }
    else {
      covid_NY_TS.reg <- covid_NY_TS.reg %>%
        mutate(y = p_cases)
      y_lab <- "Cases per 100K Population"
      title <- "New York State COVID-19 Cases per 100K Population by Region (Mar-May 2020)"
    }
    
    highlight_points <- covid_NY_TS.reg %>%
      dplyr::filter( 
        Region == "Adirondack" & date == select.date[1] + ((1*((range%/%11)+1)) %% range) |
          Region == "Capital District" & date == select.date[1] + ((2*((range%/%11)+1)) %% range) |
          Region == "Catskill" & date == select.date[1] + ((3*((range%/%11)+1)) %% range) |
          Region == "Central New York" & date == select.date[1] + ((4*((range%/%11)+1)) %% range) |
          Region == "Chautauqua-Alleghany" & date == select.date[1] + ((5*((range%/%11)+1)) %% range) |
          Region == "Eastern Hudson Valley" & date == select.date[1] + ((6*((range%/%11)+1)) %% range) |
          Region == "Finger Lakes" & date == select.date[1] + ((7*((range%/%11)+1)) %% range) |
          Region == "Long Island" & date == select.date[1] + ((8*((range%/%11)+1)) %% range) |
          Region == "New York City" & date == select.date[1] + ((9*((range%/%11)+1)) %% range) |
          Region == "New York State" & date == select.date[1] + ((10*((range%/%11)+1)) %% range) |
          Region == "Niagara Frontier" & date == select.date[1] + ((11*((range%/%11)+1)) %% range) |
          Region == "Thousand Island" & date == select.date[1] + ((12*((range%/%11)+1)) %% range)
      )
    
    NY_region_palette.df <- NY_counties_regions %>%
      dplyr::select(Region,Color) %>% 
      dplyr::distinct(Region,Color)
    
    NY_region_palette <- setNames(as.character(NY_region_palette.df$Color), as.character(NY_region_palette.df$Region))
    
    covid_NY_TS.reg %>%
    filter(date >= select.date[1] & date <= select.date[2]) %>%
    ggplot(aes(date, 
                 y, 
                 color = Region)) +
      scale_color_manual(values=NY_region_palette) +
      geom_line(size=1) +
      scale_y_continuous(
        trans = "log10",
        breaks = c(10,100,500,1000,5000,10000, 50000)
      ) +
      scale_x_datetime(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%b %d") +
      ylab(y_lab) + 
      xlab("Date") +
      ggtitle(title)  +  
      gghighlight(Region %in% selected.region, use_direct_label=FALSE) +
      geom_line(size=select.size) + 
      # TODO: Region specific labels
      geom_label_repel(data=highlight_points,  aes(label=Region), box.padding = unit(1.75, 'lines')) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
      geom_vline(aes(xintercept=as_datetime("2020-03-20"), linetype="Gov. Cuomo issues stay-at-home order"), color = "black") + 
      scale_linetype_manual(name = "Events", 
                            values = c(2), 
                            guide = guide_legend(override.aes = list(color = c("black")))) +
      NULL
    
  })
  
  output$NY.cases.TS.rates.reg <- renderPlot({
    # browser()
    selected.region <- input$NYRegion.rates.reg
    
    select.size <- 2
    
    if (selected.region == "All Regions") {
      selected.region <- sort(unique(covid_NY_TS_plot.cases$Region))
      select.size <- 1
    }
    
    highlight_points <- covid_NY_TS_plot.cases %>% 
      group_by(Region, date) %>%
      summarize(cases = mean(cases), log_cases = mean(log_cases), p_cases = mean(p_cases)) %>%
      dplyr::filter( 
        Region == "Adirondack" & date == as.Date("2020-04-26") |
          Region == "Capital District" & date == as.Date("2020-04-25") |
          Region == "Catskill" & date == as.Date("2020-04-12") |
          Region == "Central New York" & date == as.Date("2020-04-12") |
          Region == "Chautauqua-Alleghany" & date == as.Date("2020-04-10") |
          Region == "Eastern Hudson Valley" & date == as.Date("2020-04-12") |
          Region == "Finger Lakes" & date == as.Date("2020-04-26") |
          Region == "Long Island" & date == as.Date("2020-04-25") |
          Region == "New York City" & date == as.Date("2020-04-12") |
          Region == "New York State" & date == as.Date("2020-04-02") |
          Region == "Niagara Frontier" & date == as.Date("2020-04-26") |
          Region == "Thousand Island" & date == as.Date("2020-04-25")
      )
    
    NY_region_palette.df <- NY_counties_regions %>%
      dplyr::select(Region,Color) %>% 
      dplyr::distinct(Region,Color)
    
    NY_region_palette <- setNames(as.character(NY_region_palette.df$Color), as.character(NY_region_palette.df$Region))
    
    covid_NY_TS_plot.cases %>% 
      group_by(Region, date) %>%
      summarize(cases = mean(cases), log_cases = mean(log_cases), p_cases = mean(p_cases)) %>%
      dplyr::filter(p_cases >= 10) %>%
      ggplot(aes(x=date, y=p_cases, color = Region)) +
      scale_color_manual(values=NY_region_palette) +
      geom_line(size=1) +
      scale_y_continuous(
        trans = "log10",
        breaks = c(10,50,100,500,1000,5000)
      ) +
      scale_x_datetime(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%b %d") +
      ylab("Cases per 100K Population") + 
      ggtitle("New York State COVID-19 Cases per 100K Population by Region (Mar-May 2020)")  +
      gghighlight(Region %in% selected.region, use_direct_label=FALSE) +
      geom_line(size=select.size) + 
      geom_label_repel(data=highlight_points,  aes(label=Region), segment.color="black", force=10) + 
      # TODO: New highlight points for Regions
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE) +
      geom_vline(aes(xintercept=as_datetime("2020-03-20"), linetype="Gov. Cuomo issues stay-at-home order"), color = "black") + 
      scale_linetype_manual(name = "Events", 
                            values = c(2), 
                            guide = guide_legend(override.aes = list(color = c("black")))) +
      NULL
    
  })
  
  # This sets the range for zooming the following plot
  
  output$NY.cases.TS.rates <- renderPlot({
    # browser()
    selected.region <- input$NYRegion.rates
    selected.county <- input$NYCounty.rates
    select.date <- input$NYCoTDate
    
    #if (is.null(selected.county)) {
    #  selected.county <- "All Counties"
    #}
    select.size <- 2
    if (selected.county != "All Counties") {
      selected.region <- "All Regions"
    }
    
    if (selected.region == "All Regions") {
      selected.region <- sort(unique(covid_NY_TS_plot.cases$Region))
      if (selected.county == "All Counties") {
        selected.county <- sort(unique(covid_NY_TS_plot.cases$County))
        select.size <- 1
      }
    }
    else {
      selected.county <- sort(unique(covid_NY_TS_plot.cases$County))
    }
    
    highlight_points <- covid_NY_TS_plot.cases %>% 
      dplyr::filter( 
        County == "Albany" & date == as.Date("2020-04-26") |
          # County == "Allegany" & date == as.Date("2020-03-29") |
          County == "Bronx" & date == as.Date("2020-04-25") |
          County == "Broome" & date == as.Date("2020-04-12") |
          # County == "Cattaraugus" & date == as.Date("2020-03-30") |
          County == "Cayuga" & date == as.Date("2020-04-12") |
          County == "Chautauqua" & date == as.Date("2020-04-10") |
          # County == "Chemung" & date == as.Date("2020-04-10") |
          County == "Chenango" & date == as.Date("2020-04-12") |
          County == "Clinton" & date == as.Date("2020-04-26") |
          # County == "Columbia" & date == as.Date("2020-03-29") |
          County == "Cortland" & date == as.Date("2020-04-25") |
          # County == "Delaware" & date == as.Date("2020-04-02") |
          County == "Dutchess" & date == as.Date("2020-04-12") |
          County == "Erie" & date == as.Date("2020-04-02") |
          # County == "Essex" & date == as.Date("2020-04-10") |
          # County == "Franklin" & date == as.Date("2020-04-10") |
          # County == "Fulton" & date == as.Date("2020-04-12") |
          County == "Genesee" & date == as.Date("2020-04-26") |
          # County == "Greene" & date == as.Date("2020-03-29") |
          County == "Hamilton" & date == as.Date("2020-04-25") |
          County == "Herkimer" & date == as.Date("2020-04-12") |
          # County == "Jefferson" & date == as.Date("2020-03-30") |
          County == "Kings" & date == as.Date("2020-04-12") |
          # County == "Lewis" & date == as.Date("2020-04-10") |
          # County == "Livingston" & date == as.Date("2020-04-10") |
          County == "Madison" & date == as.Date("2020-04-12") |
          # County == "Monroe" & date == as.Date("2020-03-26") |
          # County == "Montgomery" & date == as.Date("2020-03-29") |
          County == "Nassau" & date == as.Date("2020-04-15") |
          County == "New York" & date == as.Date("2020-04-10") |
          County == "New York State" & date == as.Date("2020-04-12") |
          County == "Manhattan" & date == as.Date("2020-04-10") |
          County == "Niagara" & date == as.Date("2020-04-12") |
          County == "Oneida" & date == as.Date("2020-04-10") |
          County == "Onondaga" & date == as.Date("2020-04-10") |
          # County == "Ontario" & date == as.Date("2020-04-12") |
          County == "Orange" & date == as.Date("2020-04-18") |
          County == "Orleans" & date == as.Date("2020-04-01") |
          County == "Oswego" & date == as.Date("2020-04-25") |
          County == "Otsego" & date == as.Date("2020-04-12") |
          County == "Putnam" & date == as.Date("2020-04-20") |
          County == "Queens" & date == as.Date("2020-04-12") |
          County == "Rensselaer" & date == as.Date("2020-04-10") |
          County == "Richmond" & date == as.Date("2020-04-11") |
          County == "Rockland" & date == as.Date("2020-04-12") |
          County == "St. Lawrence" & date == as.Date("2020-04-26") |
          County == "Saratoga" & date == as.Date("2020-04-01") |
          County == "Schenectady" & date == as.Date("2020-04-25") |
          County == "Schoharie" & date == as.Date("2020-04-12") |
          County == "Schuyler" & date == as.Date("2020-04-10") |
          County == "Seneca" & date == as.Date("2020-04-12") |
          # County == "Steuben" & date == as.Date("2020-04-10") |
          County == "Suffolk" & date == as.Date("2020-04-10") |
          County == "Sullivan" & date == as.Date("2020-04-12") |
          # County == "Tioga" & date == as.Date("2020-03-26") |
          County == "Tompkins" & date == as.Date("2020-04-10") |
          County == "Ulster" & date == as.Date("2020-04-20") |
          # County == "Warren" & date == as.Date("2020-04-02") |
          # County == "Washington" & date == as.Date("2020-03-30") |
          # County == "Wayne" & date == as.Date("2020-04-02") |
          County == "Westchester" & date == as.Date("2020-04-10") |
          # County == "Wyoming" & date == as.Date("2020-04-10") |
          County == "Yates" & date == as.Date("2020-04-12")
      )
    
    NY_region_palette.df <- NY_counties_regions %>%
      dplyr::select(Region,Color) %>% 
      dplyr::distinct(Region,Color)
    
    NY_region_palette <- setNames(as.character(NY_region_palette.df$Color), as.character(NY_region_palette.df$Region))
    
      covid_NY_TS_plot.cases %>% 
        dplyr::filter(p_cases >= 10) %>%
        ggplot(aes(x=date, y=p_cases, color = Region, group=County)) +
        scale_color_manual(values=NY_region_palette) +
        geom_line(size=1) +
        scale_y_continuous(
          trans = "log10",
          breaks = c(10,50,100,500,1000,5000)
        ) +
      scale_x_datetime(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%b %d") +
      ylab("Cases per 100K Population") + 
      ggtitle("New York State COVID-19 Cases per 100K Population by County (Mar-May 2020)")  +
        gghighlight(County %in% selected.county & Region %in% selected.region, use_direct_label=FALSE) +
        geom_line(size=select.size) + 
      geom_label_repel(data=highlight_points,  aes(label=County), segment.color="black", force=10) + 
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE) +
        geom_vline(aes(xintercept=as_datetime("2020-03-20"), linetype="Gov. Cuomo issues stay-at-home order"), color = "black") + 
        scale_linetype_manual(name = "Events", 
                              values = c(2), 
                              guide = guide_legend(override.aes = list(color = c("black")))) +
        NULL
    
  })
  

  
  output$click_info <- renderPrint({
    hover <- input$NY.cases.TS_click
    selected.county <- input$NYCounty
    select.rate <- input$rate.CoT
    select.date <- input$NYCoTDate
    
    if (select.rate == "Overall") {
      covid_NY_TS <- covid_NY_TS_plot.cases %>%
        mutate(y = cases)
      per <- ": "
    }
    else {
      covid_NY_TS <- covid_NY_TS_plot.cases %>%
        mutate(y = p_cases) %>%
        filter(y >= 10)
      per <- "/100k: "
    }
    point <- nearPoints(covid_NY_TS, hover, threshold = 5, addDist = TRUE, maxpoints = 1,
                        xvar="date", yvar="y")
    # browser()
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
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); height:15%;",
                    "right:", left_px + 2, "px; top:", top_px + 2, "px;")

    # actual tooltip created as wellPanel
    if (nrow(point) != 0) {
      if (point$County == "New York State"){
        wellPanel(
        # style = style,
        class = "gg_tooltip",
        h3(HTML(paste0("<b>",point$County,"</b><br>Total Cases",per,format(round(point$y),big.mark = ","),"<br>Date: ",point$date)))
      )
      } else {
        wellPanel(
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",point$County," County</b><br>Total Cases",per,format(round(point$y),big.mark = ","),"<br>Date: ",point$date)))
        )
        
      }
    }
    else if(selected.county != "All Counties") {
      yesterday <- select.date[2]
      point <- covid_NY_TS %>%
        filter(County == selected.county & date == yesterday)
      if (selected.county == "New York State"){
        wellPanel(
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",selected.county,"</b><br>Total Cases",per,format(round(point[1,]$y),big.mark = ","),"<br>Date: ",yesterday)))
        )
      } else {
        wellPanel(
          # style = style,
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",selected.county," County</b><br>Total Cases",per,format(round(point[1,]$y),big.mark = ","),"<br>Date: ",yesterday)))
        )
        
      }
    }
  })
  
  output$click_info_ma <- renderPrint({
    hover <- input$NY.cases.TS_click_ma
    selected.region <- input$NYRegion3
    select.rate <- input$rate.ma
    select.date <- input$NYDate.ma
    
    if (select.rate=="Overall") {
      covid_NY_TS_plot.ma <- covid_NY_TS_plot.cases %>%
      group_by(Region, date) %>%
      summarise(diff = sum(diff)) %>%
      mutate(ma = c(numeric(moving.avg.window-1), zoo::rollmean(diff, moving.avg.window, align = "right"))) %>%
      filter(ma > 0)# %>%
      #filter(date >= date[1] & date <= date[2])
      per <- ": "
    }
    else {
      covid_NY_TS_plot.ma <- covid_NY_TS_plot.cases %>%
      group_by(Region, date) %>%
      summarise(p_diff = sum(p_diff)) %>%
      mutate(ma = c(numeric(moving.avg.window-1), zoo::rollmean(p_diff, moving.avg.window, align = "right"))) %>%
      filter(ma > 0)# %>%
      #filter(date >= date[1] & date <= date[2])
      per <- "/100k: "
    }
    
    point <- covid_NY_TS_plot.ma %>%
      nearPoints(hover, threshold = 5, addDist = TRUE, maxpoints = 1,
                        xvar="date", yvar="ma")
    # browser()
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
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); 
                    width:30%; padding: 0; margin: 0;",
                    "left:", 0, "px;")
    
    # actual tooltip created as wellPanel
    if (nrow(point) != 0) {
      avg_window <- as.Date(point$date, format = "%m-%d-%Y") - moving.avg.window + 1
      if (point$Region == "New York State"){
        wellPanel(
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",point$Region,"</b><br>Average New Cases",per,format(round(point$ma),big.mark = ","),"<br>Window: ",avg_window, " to ", point$date)))
        )
      } else {
        wellPanel(
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",point$Region," Region</b><br>Average New Cases",per,format(round(point$ma),big.mark = ","),"<br>Window: ",avg_window, " to ", point$date)))
        )
        
      }
    }
    else if(selected.region != "All Regions") {
      yesterday <- select.date[2]
      avg_window <- as.Date(yesterday, format = "%m-%d-%Y") - moving.avg.window + 1
      point <- covid_NY_TS_plot.ma %>%
        filter(Region == selected.region & date == yesterday)
      if (selected.region == "New York State"){
        wellPanel(
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",selected.region,"</b><br>Average New Cases",per,format(round(point[1,]$ma), big.mark = ","),"<br>Window: ",avg_window, " to ", yesterday)))
        )
      } else {
        wellPanel(
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",selected.region," Region</b><br>Average New Cases",per,format(round(point[1,]$ma), big.mark = ","),"<br>Window: ",avg_window, " to ", yesterday)))
        )
      }
    }
  })
  
  output$click_info_reg <- renderPrint({
    hover <- input$NY.cases.TS_click_reg
    selected.region <- input$NYRegion2
    select.region <- input$rate.CoT.reg
    select.date <- input$NYRegionDate
    
    covid_NY_TS.reg <- covid_NY_TS_plot.cases %>%
      group_by(Region, date) %>%
      summarise(cases = sum(cases), p_cases = mean(p_cases))
    
    if (select.region == "Overall") {
      covid_NY_TS.reg <- covid_NY_TS.reg %>%
        mutate(y = cases)
      per <- ": "
    }
    else {
      covid_NY_TS.reg <- covid_NY_TS.reg %>%
        mutate(y = p_cases)
      per <- "/100k: "
    }
    
    point <- covid_NY_TS.reg %>%
      nearPoints(hover, threshold = 5, addDist = TRUE, maxpoints = 1,
                        xvar="date", yvar="y")
    # browser()
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
      if (point$Region == "New York State"){
        wellPanel(
          # style = style,
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",point$Region,"</b><br>Total Cases",per,format(round(point$y),big.mark = ","),"<br>Date: ",point$date)))
          )
      } else {
        wellPanel(
          # style = style,
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",point$Region," Region</b><br>Total Cases",per,format(round(point$y),big.mark = ","),"<br>Date: ",point$date)))
        )
        
      }
    }
    else if(selected.region != "All Regions") {
      yesterday <- select.date[2]
      point <- covid_NY_TS.reg %>%
        filter(Region == selected.region & date == yesterday)
      if (selected.region == "New York State"){
        wellPanel(
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",point$Region,"</b><br>Total Cases",per,format(round(point[1,]$y),big.mark = ","),"<br>Date: ",yesterday)))
        )
      } else {
        wellPanel(
          # style = style,
          class = "gg_tooltip",
          h3(HTML(paste0("<b>",point$Region," Region</b><br>Total Cases",per,format(round(point[1,]$y),big.mark = ","),"<br>Date: ",yesterday)))
          )
      }
    }
  })
  
  output$click_info_rates <- renderPrint({
    hover <- input$NY.cases.TS.rates_click
    selected.county <- input$NYCounty.rates
    
    point <- nearPoints(covid_NY_TS_plot.cases, hover, threshold = 5, addDist = TRUE, maxpoints = 1,
                        xvar="date", yvar="p_cases")
    
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
          p(HTML(paste0(point$County,": ",format(round(point$p_cases),big.mark = ",")," COVID-19 cases per 100K on ",point$date)))
        )
      } else {
        wellPanel(
          # style = style,
          p(HTML(paste0(point$County," County: ",format(round(point$p_cases),big.mark = ",")," COVID-19 cases per 100K on ",point$date)))
        )
        
      }
    }
    else if(selected.county != "All Counties") {
      yesterday <- as.Date(update_date, format = "%m-%d-%Y") - 1
      point <- covid_NY_TS_plot.cases %>%
        filter(County == selected.county & date == yesterday)
      if (selected.county == "New York State"){
        wellPanel(
          p(HTML(paste0(selected.county,": ",format(round(point[1,]$p_cases),big.mark = ",")," COVID-19 cases per 100K on ",yesterday)))
        )
      } else {
        wellPanel(
          # style = style,
          p(HTML(paste0(selected.county," County: ",format(round(point[1,]$p_cases),big.mark = ",")," COVID-19 cases per 100K on ",yesterday)))
        )
      }
    }
  })
  
  output$click_info_rates_reg <- renderPrint({
    hover <- input$NY.cases.TS.rates_click.reg
    selected.region <- input$NYRegion.rates.reg
    
    point <- covid_NY_TS_plot.cases %>%
      group_by(Region, date) %>%
      summarise(p_cases = mean(p_cases)) %>%
      nearPoints (hover, threshold = 5, addDist = TRUE, maxpoints = 1,
                        xvar="date", yvar="p_cases")
    
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
      if (point$Region == "New York State"){
        wellPanel(
          # style = style,
          p(HTML(paste0(point$Region,": ",format(round(point$p_cases),big.mark = ",")," COVID-19 cases per 100K on ",point$date)))
        )
      } else {
        wellPanel(
          # style = style,
          p(HTML(paste0(point$Region," Region: ",format(round(point$p_cases),big.mark = ",")," COVID-19 cases per 100K on ",point$date)))
        )
      }
    }
    else if(selected.region != "All Regions") {
      yesterday <- as.Date(update_date, format = "%m-%d-%Y") - 1
      point <- covid_NY_TS_plot.cases %>%
        group_by(Region, date)  %>%
        summarise(p_cases = mean(p_cases)) %>%
        filter(Region == selected.region & date == yesterday)
      if (selected.region == "New York State"){
        wellPanel(
          p(HTML(paste0(selected.region,": ",format(round(point[1,]$p_cases),big.mark = ",")," COVID-19 cases per 100K on ",yesterday)))
        )
      } else {
        wellPanel(
          # style = style,
          p(HTML(paste0(selected.region," Region: ",format(round(point[1,]$p_cases),big.mark = ",")," COVID-19 cases per 100K on ",yesterday)))
        )
      }
    }
  })
  
  observeEvent(input$NY.cases.TS_dblclick, {
    brush <- input$NY.cases.TS_brush
    if (!is.null(brush)) {
      #browser()
      ranges$x <- as.POSIXct(c(brush$xmin, brush$xmax), origin="1970-01-01")
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  }) 

  observeEvent(input$NY.cases.TS.rates_dblclick, {
    brush2 <- input$NY.cases.TS.rates_brush
    if (!is.null(brush2)) {
      #browser()
      ranges2$x <- as.POSIXct(c(brush2$xmin, brush2$xmax), origin="1970-01-01")
      ranges2$y <- c(brush2$ymin, brush2$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  }) 
  
  output$NY.race.nys <- renderPlot({
    
    # Creating dataframe for death disparity data from health.ny.gov's COVID-19 tracker
    # See NYS COVIDTracker https://on.ny.gov/2VehafT for current numbers; the following are current as of 04/19
    NYS_Dis.df <- data.frame("Race.Ethnicity" =        c("Hispanic", "Black", "White", "Asian"), 
                             "Percent.of.Pop" =        c(12,  9, 74, 4), 
                             "Percent.of.Fatalities" = c(14, 18, 60, 4))
    
    
    # Creating columns to measure disparity between state pop percent and fatality percent
    NYS_Dis_m.df <- NYS_Dis.df %>%
      mutate(Dis = -log(Percent.of.Pop/Percent.of.Fatalities)) %>%
      arrange(desc(Race.Ethnicity))
    
    # Setup: COVIDMINDER Colors and DI bins
    di_pal <- leaflet::colorBin(colors, domain = NYS_Dis_m.df$Dis, bins = bins, reverse=FALSE)
    
    # Create color column with correct mapping
    NYS_Dis_m.df <- NYS_Dis_m.df %>% 
      mutate(Race.Ethnicity = ordered(Race.Ethnicity, levels=Race.Ethnicity)) %>% 
      mutate(color = di_pal(Dis))
    
    # Plotting NYS dispairity between NYS pop percent and fatality percent
    NYS_Dis_m.df %>% 
      ggplot(aes(x = Race.Ethnicity, 
                 y = Dis,
                 fill = NYS_Dis_m.df$Race.Ethnicity
      )) + guides(fill = FALSE) + coord_flip() +
      geom_bar(stat = "Identity", colour="black") +
      scale_fill_manual(values=NYS_Dis_m.df$color, name = "Race/Ethnicity") +
      theme_minimal() + 
      theme(axis.title.x = element_text(size = 14, vjust = -1),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12)) +
      xlab("Race/Ethnicity") + 
      theme(axis.text.y = element_text(face="bold", size=14)) +
      ylab("Disparity") + 
      scale_y_continuous(
        breaks = c(-5,-3,-2,-1,-.2,.2,1,2,3,5),
        limits = c(-1.5,1.5)
      ) +
      labs(title = "Racial/ethnic disparities in % of COVID-19 deaths as compared to population %",
           subtitle = "For  New York State (excluding New York City)",
           caption = "Source: covid19tracker.health.ny.gov") +
      theme(
                legend.position = "none",
        plot.title = element_text(vjust = 0, face="bold",size=18)) + 
      geom_hline(aes(yintercept=-0.2, linetype="Lower Bound"), color = "#253494") +
      geom_hline(aes(yintercept= 0.2, linetype="Upper Bound"), color = "#BD0026") +
      scale_linetype_manual(name = "Target Levels",
                            values = c(2,
                                       2),
                            guide = guide_legend(override.aes = list(color = c("#253494",
                                                                               "#BD0026")))) +
      annotate(geom="text", y=-1, x="Hispanic", label="Lower", color="#253494", size=10) +
      annotate(geom="text", y=1, x="Hispanic", label="Higher", color="#BD0026", size=10) +
      NULL
  })

  output$NY.race.nyc <- renderPlot({
    
    # Creating dataframe for death disparity data from health.ny.gov's COVID-19 tracker
    # See NYS COVIDTracker https://on.ny.gov/2VehafT for current numbers; the following are current as of 04/19

    NYC_Dis.df <- data.frame("Race.Ethnicity" =        c("Hispanic", "Black", "White", "Asian"), 
                             "Percent.of.Pop" =        c(29, 22, 32, 14), 
                             "Percent.of.Fatalities" = c(34, 28, 27,  7))
    
    
    # Creating columns to measure disparity between city pop percent and fatality percent
    NYC_Dis_m.df <- NYC_Dis.df %>%
      mutate(Dis = -log(Percent.of.Pop/Percent.of.Fatalities)) %>%
      arrange(desc(Race.Ethnicity))
    
    # Setup: COVIDMINDER Colors and DI bins
    di_pal <- leaflet::colorBin(colors, domain = NYC_Dis_m.df$Dis, bins = bins, reverse=FALSE)
    
    # Create color column with correct mapping
    NYC_Dis_m.df <- NYC_Dis_m.df %>% 
      mutate(Race.Ethnicity = ordered(Race.Ethnicity, levels=Race.Ethnicity)) %>% 
      mutate(color = di_pal(Dis)) 
    
    # Plotting NYS dispairity between NYS pop percent and fatality percent
    NYC_Dis.p <- NYC_Dis_m.df %>% 
      ggplot(aes(x = Race.Ethnicity, 
                 y = Dis,
                 fill = NYC_Dis_m.df$Race.Ethnicity
      )) + guides(fill = FALSE) + coord_flip() +
      geom_bar(stat = "Identity", colour="black")
    
    NYC_Dis.p + 
      scale_fill_manual(values=NYC_Dis_m.df$color, name = "Race/Ethnicity") + 
      theme_minimal() + 
      theme(axis.title.x = element_text(size = 14, vjust = -1),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12)) +
      xlab("Race/Ethnicity") +
      theme(axis.text.y = element_text(face="bold", size=14)) +
      ylab("Disparity") + 
      scale_y_continuous(
        breaks = c(-5,-3,-2,-1,-.2,.2,1,2,3,5),
        limits = c(-1.5,1.5)
      ) +
      labs(title = "Racial/ethnic disparities in % of COVID-19 deaths as compared to population %",
           subtitle = "For New York City",
           caption = "Source: covid19tracker.health.ny.gov") +
      theme(
        legend.position = "none",
        plot.title = element_text(vjust = 0, face="bold",size=18)) + 
      geom_hline(aes(yintercept=-0.2, linetype="Lower Bound"), color = "#253494") +
      geom_hline(aes(yintercept= 0.2, linetype="Upper Bound"), color = "#BD0026") +
      scale_linetype_manual(name = "Target Levels",
                            values = c(2,
                                       2),
                            guide = guide_legend(override.aes = list(color = c("#253494",
                                                                               "#BD0026")))) +
      annotate(geom="text", y=-1, x="Hispanic", label="Lower", color="#253494", size=10) +
      annotate(geom="text", y=1, x="Hispanic", label="Higher", color="#BD0026", size=10) +
      NULL
  
    })
  
  output$NY.race.ct <- renderPlot({
    
    # Data source: ct.gov
    CT_Dis.df <- data.frame("Race.Ethnicity" =        c("Hispanic", "Black", "White", "Asian"), 
                            "Percent.of.Pop" =        c(16.5,  12, 66.5, 4.9), 
                            "Percent.of.Fatalities" = c(8.9, 14.8, 67.9, 1.3))
    
    # Creating columns to measure disparity between state pop percent and fatality percent
    CT_Dis_m.df <- CT_Dis.df %>%
      mutate(Dis = -log(Percent.of.Pop/Percent.of.Fatalities))
    
    # Setup: COVIDMINDER Colors and DI bins
    di_pal <- leaflet::colorBin(colors, domain = CT_Dis_m.df$Dis, bins = bins, reverse=FALSE)
    
    
    # Create color column with correct mapping
    CT_Dis_m.df <- CT_Dis_m.df %>% 
      mutate(Race.Ethnicity = ordered(Race.Ethnicity, levels=Race.Ethnicity)) %>% 
      mutate(color = di_pal(Dis))
    
    # Plotting dispairity between CT pop percent and fatality percent
    CT_Dis_m.df %>% 
      ggplot(aes(x = Race.Ethnicity, 
                 y = Dis,
                 fill = CT_Dis_m.df$Race.Ethnicity
      )) + guides(fill = FALSE) + coord_flip() +
      geom_bar(stat = "Identity", colour="black") +
      scale_fill_manual(values=CT_Dis_m.df$color, name = "Race/Ethnicity") +
      theme_minimal() + 
      theme(axis.title.x = element_text(size = 14, vjust = -1),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12)) +
      xlab("Race/Ethnicity") + 
      theme(axis.text.y = element_text(face="bold", size=14)) +
      ylab("Disparity") + 
      scale_y_continuous(
        breaks = c(-5,-3,-2,-1,-.2,.2,1,2,3,5),
        limits = c(-1.5,1.5)
      ) +
      labs(title = "Racial/ethnic disparities in % of COVID-19 deaths compared to population %",
           subtitle = "For Connecticut",
           caption = "Source: ct.gov") +
      theme(
        legend.position = "none",
        plot.title = element_text(vjust = 0, face="bold",size=18)) + 
      geom_hline(aes(yintercept=-0.2, linetype="Lower Bound"), color = "#253494") +
      geom_hline(aes(yintercept= 0.2, linetype="Upper Bound"), color = "#BD0026") +
      scale_linetype_manual(name = "Target Levels",
                            values = c(2,
                                       2),
                            guide = guide_legend(override.aes = list(color = c("#253494",
                                                                               "#BD0026")))) +
      annotate(geom="text", y=-1, x="White", label="Lower", color="#253494", size=12) +
      annotate(geom="text", y=1, x="White", label="Higher", color="#BD0026", size=12) +
      NULL
    
  })
  
  ### The following code deals with setting or responding to parameterized URLs
  observe(print(input$tab))
  
  observe({
    # This "does the right thing" for an incoming URL
    # suppose url is http://127.0.0.1:5682/?tab=tab3c/plot
    query <- parseQueryString(session$clientData$url_search)
    
    if(!is.null(query$tab)) {
      url <- strsplit(query$tab,"/")[[1]]
      url1 <<- url[1]
      url2 <<- url[2]
      updateTabsetPanel(session, 'tab', url1)
    }
  })
  
  
  observe({
    # Trigger this observer every time an input changes
    params <- reactiveValuesToList(input)
    session$doBookmark()
  })
  
  onBookmarked(function(url) {
    # Construct the replacement URL:
    url.new <- paste0(
      session$clientData$url_protocol,"//",
      session$clientData$url_hostname,
      session$clientData$url_pathname,
      "?tab=",
      session$clientData$url_port,
      session$input$tab
    )
    #TODO: Special handling for tabs with selectors!
    # browser()
    updateQueryString(url.new)
  })
  
  observe({ # this observer executes once, when the page loads
    
    data <- parseQueryString(session$clientData$url_search)
    
    # browser()
    # the navbar tab and tabpanel variables are two variables 
    # we have to pass to the client for the update to take place
    # if nav is defined, send a message to the client to set the nav tab
    if (! is.null(data$page)) {
      session$sendCustomMessage(type='setNavbar', data)
    }
    
    # if the tab variable is defined, send a message to client to update the tab
    if (any(sapply(data[c('outcome_usa_mortality', 
                          'outcome_usa_racial_disparity',
                          'outcome_ny_mortality',
                          'outcome_ny_cases', 
                          'outcome_ny_racial_disparity',
                          'outcome_ct_racial_disparity',
                          'outcome_ny_new_cases',
                          #'outcome_ny_cases_rate',
                          #'outcome_ny_cases_rate_regions',
                          'outcome_ny_cases_time',
                          'outcome_ny_cases_time_region',
                          'mediation_usa_testing',
                          'mediation_usa_hospital_beds',
                          'determinant_usa',
                          #'determinant_usa_obesity',
                          'determinant_ny'
    )], 
    Negate(is.null)))) {
      # browser()
      session$sendCustomMessage(type='setTab', data)
    }
    
  })
}

#### Set up Shiny App ####
shinyApp(ui = ui, server = server, enableBookmarking = "url")
