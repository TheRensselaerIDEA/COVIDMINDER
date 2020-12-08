# Import states json
states.shapes <- readRDS("data/json/us_projection.Rds")

#Dataset for all state mortality/cases at county level
todays.case.data <- read_csv("data/csv/todays_case_data.csv")

# load in 
library(datasets)
state.abr <- cbind.data.frame(state.abb, as.character(state.name), state.center$x, state.center$y)
colnames(state.abr) <- c("abr", "name", "lon", "lat")
state.abr[state.abr$abr == "AK", "lon"] <- -153.4937
state.abr[state.abr$abr == "AK", "lat"] <- 64.2008
state.abr[state.abr$abr == "HI", "lon"] <- -157.532
state.abr[state.abr$abr == "HI", "lat"] <- 20.57503
state.abr[state.abr$abr == "MI", "lon"] <- -86.41704
state.abr[state.abr$abr == "MI", "lat"] <- 44.9534
state.abr$Region <- state.region

# attach the full name of each state to each row (currently each row only has the state's abbreviated name)
todays.case.data <- todays.case.data %>%
  left_join(state.abr[c("abr", "name")], by=c("State" = "abr")) %>%
  mutate(name = as.character(name))
todays.case.data[todays.case.data$State == "DC", "name"] <- "District of Columbia"


# Convert to dataframe state data
# the states data frame simply associates state names with their FIPS 
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

# Import cardio data (NEW)
# UPDATED: 16 Apr (new source!)
#cardio_deaths_2017 <- read_csv("data/csv/cardio_deaths_2017.csv")
# NOTE: Includes ethnicities; already in p_100K format
cardio_deaths_2015 <- read_csv("data/csv/HeartDisease_DeathRate_States_CDC_2015.csv")

# Import US Diabetes data
# Diabetes is a comorbidity of severe COVID-19 cases
diabetes_data_states <- read_csv("data/csv/diabetes_data_states.csv")
# Import County level diabetes
diabetes_data_counties <- read_csv("data/csv/diabetes_data_counties.csv")

# Import US Obesity data: see https://stateofchildhoodobesity.org/adult-obesity/
# Obesity is a comorbitity of severe COVID-19 cases
obesity_data_states <- read_csv("data/csv/obesity_data_states.csv")
obesity_data_counties <- read_csv("data/csv/obesity_data_counties.csv")

# Import US cronic respiratory disease (CRD) mortality data: 
# http://ghdx.healthdata.org/record/ihme-data/united-states-chronic-respiratory-disease-mortality-rates-county-1980-2014
CRD_data_counties <- read_csv("data/csv/chronicrespiratorydiseases_counties.csv")
CRD_data_counties$FIPS <- as.numeric(CRD_data_counties$FIPS)


# Import per-state per-race disparity data
covid_racial_data_states.wide <- read_csv("data/csv/states_cdc_racial_wide.csv")

# Important per-state legislative and EO data
covid_eo_bills <- read_csv("data/csv/Covid_EO.csv")

# State Policy Processing
state_policy <- read_csv("data/csv/state_policy.csv")
state_policy.df <- state_policy[5:55,] # Raw data points
state_policy.md <- state_policy[1:4,] # Metadata

# Properly format all data defined as date data.
state_policy.df[as.vector(state_policy.md[4,] == "date")] <- lapply(state_policy.df[as.vector(state_policy.md[4,] == "date")], 
                                                                    function(x){parse_date_time(x, c("%m/%d/%y"))})

# Test rates from countries we are comparing to 
total_test_rates.df <- read_csv("data/csv/owid_glb_test_rates.csv")

# Obesity rates from countries we are comparing to
owid_data.obesity.rate <- read.csv("data/csv/owid_obese_pct_2016.csv")
colnames(owid_data.obesity.rate) <- c("Entity", "Code", "Year", "Pct_obese_adult")

# Time series data for nationwide, counties and states
covid_TS_counties_long.cases <- read_csv("data/csv/time_series/covid_TS_counties_long.cases.csv")
covid_TS_state_long.cases <- read_csv("data/csv/time_series/covid_TS_state_long.cases.csv")
covid_TS_US_long.cases <- read_csv("data/csv/time_series/covid_TS_US_long.cases.csv")


##National Determinants
#data from GWAS_ADJ_P and GWAS_MRR
GWAS_ADJ_P <- readRDS('data/GWAS_ADJ_P.rds')


GWAS_MRR <- readRDS('data/GWAS_MRR.rds')

## 2016 Presidential Election Results
# Derived from: 
# This includes ldi calculations!
election_results_counties <- read_csv("data/csv/election_results_counties.csv")
