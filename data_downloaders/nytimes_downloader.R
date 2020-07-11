# Daily data importer
# Source: NY Times Daily Reports (github): https://github.com/nytimes/covid-19-data
# copied 11 Apr 2020 from the COVID-Notebooks repo
library(tidyverse)

# curl newest data from JHU github
# (You must edit the date below in two places to be yesterday's date)
# so if today is 4/10/2020 use 2020-04-09
currDate <- "2020-04-10"
dateURL.1 <- "NYTIMES_temporary.csv"
dateURL.2 <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

# Write raw data to file system; use date-based name syntax as above
download.file(dateURL.2, paste0("data/csv/", dateURL.1))

# Import raw into R
todays_raw_data <- read_csv(paste0("data/csv/", dateURL.1))

# Transform to match our structure
covid_data_counties <- todays_raw_data %>%
  filter(state == "New York") %>%
  filter(date == currDate) %>%
  select(fips,county,cases,deaths)

# Change column names to match app
colnames(covid_data_counties) <- c('FIPS','NAME','covid19_cases','covid19_deaths')

# Create a new york state row
covid_data_new_york <- covid_data_counties %>%
  summarize(covid19_cases=sum(covid19_cases), covid19_deaths=sum(covid19_deaths))

covid_data_new_york$FIPS <- 36
covid_data_new_york$NAME <- 'New York'
covid_data_new_york <- covid_data_new_york[,c("FIPS", "NAME", "covid19_cases", "covid19_deaths")]

covid_data_counties <- data.frame(rbind(covid_data_new_york, covid_data_counties))

# Make backup of existing data
write_csv(read_csv("data/csv/state_covid_mortality.csv"),"data/csv/state_covid_mortality.csv.bak")

# Write data frame to file system
write_csv(covid_data_counties, "data/csv/state_covid_mortality.csv")

# Delete raw data file because it is large and updated daily
if (file.exists(paste0('data/csv/', dateURL.1)))
  file.remove(paste0('data/csv/', dateURL.1))
