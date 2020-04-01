# Daily data importer
# Source: COVID Tracking Project (api): 
library(tidyverse)

# Download states raw data 
statesURL <- "https://covidtracking.com/api/states.csv"
download.file(statesURL, paste0("data/csv/", "states_testing_raw.csv"))

# Import raw into R
todays_raw_data <- read_csv(paste0("data/csv/", "states_testing_raw.csv"))

# Transform to match our structure
covid_data_states <- todays_raw_data %>%
  filter(Country_Region == "US") %>%
  filter(!Province_State %in% c("Diamond Princess","Grand Princess","Northern Mariana Islands","Virgin Islands") ) %>%
  select(Province_State,Last_Update,Confirmed,Deaths,Recovered) %>%
  group_by(Province_State) %>%
  summarize(Confirmed=sum(Confirmed), Deaths=sum(Deaths),Recovered=sum(Recovered))

# Change names to match app
colnames(covid_data_states) <- c("NAME","covid19_cases","covid19_deaths","covid19_recovered")

# Create a "United States" row
covid_data_united_states <- covid_data_states %>%
  summarize(covid19_cases=sum(covid19_cases), covid19_deaths=sum(covid19_deaths),covid19_recovered=sum(covid19_recovered))

covid_data_united_states$NAME <- 'United States'
covid_data_united_states <- covid_data_united_states[,c("NAME","covid19_cases","covid19_deaths","covid19_recovered")]

covid_data_states <- data.frame(rbind(covid_data_united_states, covid_data_states))

# Make backup of existing data
write_csv(read_csv("data/csv/covid_data_states.csv"),"data/csv/covid_data_states.csv.bak")

# write out new dataframe to file system 
write_csv(covid_data_states,"data/csv/covid_data_states.csv")
