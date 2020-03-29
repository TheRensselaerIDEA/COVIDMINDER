# Library and Data Imports ------------------------------------------------
source("Source.R")
states.import <- geojsonio::geojson_read("json/us-states.json", what = "sp")
# Import population
population <- read_csv("population.csv")

# Import provider capacity
provider_capacity <- read_csv("provider_capacity.csv")

# Import state testing 
state_covid_testing <- read_csv("state_covid_testing.csv")

# Import at risk adults
# NOTE: Includes all at-risk and share of at-risk over 60
at_risk_adults <- read_csv("at_risk_adults.csv") 

# Import hypertension

hypertension_mortality <- read_csv("hypertension_mortality.csv")
hypertension_mortality$p_ht_death_rate <-hypertension_mortality$p_ht_death_rate / 100000


# Data Processing ---------------------------------------------------------
# Can preprocess and remove this code in future
## Provider Capacity (Hospital beds) fixing
# Fix column names
colnames(provider_capacity) <- c("NAME","total_hosp_beds","hosp_beds_per_1000","total_CHCs","CHC_delivery_sites")
provider_capacity$p_hosp_beds <-provider_capacity$hosp_beds_per_1000/1000 

# Calculate pUS
pUS.1 <- as.numeric(provider_capacity[which(provider_capacity$NAME=="United States"),"p_hosp_beds"])

# ldi.fct <- function(x) {log((x/(1-x))/(pUS/(1-pUS)))} # In case we need a stand-alone definition...

hosp_beds_ldi <- unlist(lapply(provider_capacity$p_hosp_beds, FUN=function(x){log((x/(1-x))/(pUS.1/(1-pUS.1)))}))
provider_capacity <- data.frame(provider_capacity, hosp_beds_ldi)
provider_capacity <- provider_capacity[match(states$NAME, provider_capacity$NAME),]
provider_capacity <- provider_capacity[1:52,]
states <- data.frame(states, "hosp_beds_ldi"=provider_capacity$hosp_beds_ldi) # Append to states

## COVID-19 Testing fixing
colnames(state_covid_testing) <- c("NAME","total_num_tests","tests_pos_results")
# Inner join to add population
state_covid_testing <- left_join(state_covid_testing, population, by = c('NAME'))

# TODO: This is really just the test rate, not "per 1000"
state_covid_testing <- state_covid_testing %>% 
  mutate(tests_per_1000 = total_num_tests / Population)

pUS.2 <- as.numeric(state_covid_testing[which(state_covid_testing$NAME=="United States"),"tests_per_1000"])

pSK.2 <- 6768 / 1000000

#tests_ldi <- unlist(lapply(state_covid_testing$tests_per_1000, FUN=function(x){log((x/(1-x))/(pUS.2/(1-pUS.2)))}))
tests_ldi <- unlist(lapply(state_covid_testing$tests_per_1000, FUN=function(x){log((x/(1-x))/(pSK.2/(1-pSK.2)))}))

state_covid_testing <- data.frame(state_covid_testing, tests_ldi)

state_covid_testing <- state_covid_testing[match(states$NAME, state_covid_testing$NAME),]

state_covid_testing <- state_covid_testing[1:52,]

states <- data.frame(states, "tests_ldi"=state_covid_testing$tests_ldi) # Append to states

## At-risk Adults fixing

# Calculate pUS
# All at-risk
pUS.3 <- as.numeric(at_risk_adults[which(at_risk_adults$NAME=="United States"),"p_at_risk_adults"])

at_risk_ldi <- unlist(lapply(at_risk_adults$p_at_risk_adults, FUN=function(x){log((x/(1-x))/(pUS.3/(1-pUS.3)))}))

at_risk_adults <- data.frame(at_risk_adults, at_risk_ldi)

#Older at-risk
pUS.4 <- as.numeric(at_risk_adults[which(at_risk_adults$NAME=="United States"),"p_older_at_risk_adults"])

older_at_risk_ldi <- unlist(lapply(at_risk_adults$p_older_at_risk_adults, FUN=function(x){log((x/(1-x))/(pUS.4/(1-pUS.4)))}))

at_risk_adults <- data.frame(at_risk_adults, older_at_risk_ldi)

# RE-order to match states ordering
at_risk_adults <- at_risk_adults[match(states$NAME, at_risk_adults$NAME),]

at_risk_adults <- at_risk_adults[1:52,]

# Append the new columns to states
states <- data.frame(states, "at_risk_ldi"=at_risk_adults$at_risk_ldi) # Append to states

states <- data.frame(states, "older_at_risk_ldi"=at_risk_adults$older_at_risk_ldi) # Append to states

# Hypertension mortality
pUS.5 <- as.numeric(hypertension_mortality[which(hypertension_mortality$NAME=="United States"),"p_ht_death_rate"])

ht_death_rate_ldi <- unlist(lapply(hypertension_mortality$p_ht_death_rate, FUN=function(x){log((x/(1-x))/(pUS.5/(1-pUS.5)))}))

hypertension_mortality <- data.frame(hypertension_mortality, ht_death_rate_ldi)

# RE-order to match states ordering
hypertension_mortality <- hypertension_mortality[match(states$NAME, hypertension_mortality$NAME),]

hypertension_mortality <- hypertension_mortality[1:52,]

# Append the new column to states
states <- data.frame(states, "ht_death_rate_ldi"=hypertension_mortality$ht_death_rate_ldi) # Append to states



# UI Code -----------------------------------------------------------------
ui <- navbarPage(
  title="CovidMinder",
  tabPanel("COVID-19 Testing/State",
           mainPanel(leafletOutput(outputId = "mymap"))
           )
)


# Server Code -------------------------------------------------------------
server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    colors <- c("#ef8a62","#fddbc7","#f7f7f7","#d1e5f0","#67a9cf")
    bins2 <- c(-5, -2, -1, -.5, 0, .5, 1, 2, 3)
    pal2 <- colorBin(colors, domain = states$tests_ldi, bins = bins, reverse=FALSE)
    labels2 <- sprintf(
      "<strong>%s</strong><br/>
  Hypertension Mortality Rate DI: %.2g<br/>
  Older At Risk Adults DI: %.2g<br/>
  At Risk Adults DI: %.2g<br/>
  <span style='background-color: #e1eaea'>Total Tests vs South Korea DI: %.2g</span><br/>
  Hospital Beds DI: %.2g",
      states$NAME, states$ht_death_rate_ldi, states$older_at_risk_ldi, states$at_risk_ldi, states$tests_ldi, states$hosp_beds_ldi
    ) %>% lapply(htmltools::HTML)
    leaflet(states.import) %>%
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
        #accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
        accessToken = 'pk.eyJ1IjoiZmlndWVqMyIsImEiOiJjazhkb2RidGkwd2htM2tvd3UzaXp2bGp0In0.o8rTdhI-Iz24zxcug-C6jg')) 
  })
}

shinyApp(ui = ui, server = server)
