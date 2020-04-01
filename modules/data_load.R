# Import states json
states.shapes <- readRDS("data/json/us_projection.Rds")

# Convert to dataframe state data
states <- states.shapes
states <- data.frame(states)
states <- states[c("fips_state", "name")]
colnames(states) <- c("FIPS", "NAME")

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

# join in population column
covid_data_states <- left_join(covid_data_states, population, by = c('NAME'))

# NEW: Need to calculate these because JHU data doesn't pre-calculate
covid_data_states$calc_case_rate <- covid_data_states$covid19_cases/covid_data_states$Population
covid_data_states$calc_death_rate <- covid_data_states$covid19_cases/covid_data_states$Population

# Divide-by-mil only needed for KFF data
# covid_data_states$p_death_rate <- covid_data_states$deaths_per_mil/1000000
covid_data_states$p_death_rate <- covid_data_states$calc_death_rate


