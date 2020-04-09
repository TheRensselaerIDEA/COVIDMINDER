# Daily data importer
# Source: COVID Tracking Project (api): 
library(tidyverse)

# Download states raw data 
statesURL <- "https://covidtracking.com/api/states.csv"
download.file(statesURL, paste0("data/csv/", "states_testing_raw.csv"))

# Import raw into R
todays_raw_data <- read_csv(paste0("data/csv/", "states_testing_raw.csv"))

# Transform to match our structure
state_covid_testing <- todays_raw_data %>%
#  filter(Country_Region == "US") %>%
  filter(!state %in% c("AS") ) %>%
  select(state,positive,negative,total) 

# Adjust names
colnames(state_covid_testing) <- c("Abbreviation","positive","negative","total_num_tests")

states_abbreviations <- read_csv(paste0("data/csv/", "states_abbreviations.csv"))

# join in population column
state_covid_testing <- left_join(state_covid_testing, states_abbreviations, by = c('Abbreviation'))

state_covid_testing <- state_covid_testing[,-1]

# Create a "United States" row
covid_testing_united_states <- state_covid_testing %>%
  summarize(total_num_tests=sum(na.omit(total_num_tests)), positive=sum(na.omit(positive)), negative=sum(na.omit(negative)))
            
covid_testing_united_states$NAME <- 'United States'
covid_testing_united_states <- covid_testing_united_states[,c("NAME","positive","negative","total_num_tests")]

state_covid_testing <- data.frame(rbind(covid_testing_united_states, state_covid_testing))
state_covid_testing <- state_covid_testing %>%
  filter(!is.na(NAME))

# Make backup of existing data
write_csv(read_csv("data/csv/state_covid_testing.csv"),"data/csv/state_covid_testing.csv.bak")

# write out new dataframe to file system 
write_csv(state_covid_testing,"data/csv/state_covid_testing.csv")
