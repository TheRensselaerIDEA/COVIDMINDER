# Import states json
states.original <- geojsonio::geojson_read("data/json/us-states.json", what = "sp")
states <- states.original

# Import population data
population <- read_csv("data/csv/population.csv")

# Import provider capacity data
provider_capacity <- read_csv("data/csv/provider_capacity.csv")

# Import state testing data
state_covid_testing <- read_csv("data/csv/state_covid_testing.csv")

# Import at risk adults data
# (NOTE: THis includes all at-risk and share of at-risk over 60)
at_risk_adults <- read_csv("data/csv/at_risk_adults.csv") 

# Import hypertension data
hypertension_mortality <- read_csv("data/csv/hypertension_mortality.csv")

# Import COVID-19 Cases & Deaths data
covid_data_states <- read_csv("data/csv/covid_data_states.csv")
covid_data_states$p_deaths_cases <- covid_data_states$covid19_deaths / covid_data_states$covid19_cases
