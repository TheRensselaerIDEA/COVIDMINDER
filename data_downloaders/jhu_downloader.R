# COVIDMINDER Daily data downloader (2 of 4)
# Source: JHU Daily Reports (github): https://bit.ly/3dMWRP6
# FILES CREATED/UPDATED BY THIS SCRIPT:
# "data/csv/04-XX-2020.csv"  (where 'XX' is the current day)
# "data/csv/covid_data_states.csv"
# "data/csv/covid_data_states.csv.bak"
library(tidyverse)

# curl newest data from JHU github
# (You must edit the date below)
dateURL.1 <- "04-21-2020.csv"
dateURL.2 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"

# Write raw data to file system; use date-based name syntax as above
download.file(paste0(dateURL.2,dateURL.1), paste0("data/csv/", dateURL.1))

# Import raw into R
todays_raw_data <- read_csv(paste0("data/csv/", dateURL.1))

# Transform to match our structure
covid_data_states <- todays_raw_data %>%
  dplyr::filter(Country_Region == "US") %>%
  dplyr::filter(!Province_State %in% c("Diamond Princess","Grand Princess","Northern Mariana Islands","Virgin Islands") ) %>%
  dplyr::select(Province_State,Last_Update,Confirmed,Deaths,Recovered) %>%
  dplyr::group_by(Province_State) %>%
  dplyr::summarize(Confirmed=sum(Confirmed), Deaths=sum(Deaths),Recovered=sum(Recovered))

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
