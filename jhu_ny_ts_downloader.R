# Daily data importer
# Source: JHU Daily Reports (github): https://bit.ly/3dMWRP6

# NOTE: This is a special version to prep daily TS data for NY State

library(tidyverse)
library(lubridate)
library(stringr)
library(plotly)

# curl newest TIME SERIES data from JHU github
# dateURL.1 <- "time_series_covid19_confirmed_US.csv"   # download cases
dateURL.1 <- "time_series_covid19_deaths_US.csv"        # download deaths
dateURL.2 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

# Write raw data to file system; use JHU syntax as above
download.file(paste0(dateURL.2,dateURL.1), paste0("data/csv/time_series/", dateURL.1))

# Import raw into R
todays_TS_data <- read_csv(paste0("data/csv/time_series/", dateURL.1))

# Create a NY county population list
NY_county_data <- todays_TS_data %>%
  filter(Country_Region == "US") %>%
  filter(Province_State == "New York") %>%
  select(FIPS, Admin2,Lat,Long_,Population)

colnames(NY_county_data)[2] <- "County"

# Write it out 
write_csv(NY_county_data, "data/csv/time_series/NY_county_data.csv")

# Transform to match our structure
covid_NY_TS_counties <- todays_TS_data %>%
  filter(Country_Region == "US") %>%
  filter(Province_State == "New York") %>%
#  select(-UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Lat, -Long_, -Combined_Key, -Population) %>%
  select(-UID, -iso2, -iso3, -code3, -Combined_Key,-Country_Region) %>%
  group_by(FIPS, Admin2,Province_State) %>%
  select(-Lat, -Long_, -Population) %>%
  summarize_all(sum)

# Change colnames to match app
colnames(covid_NY_TS_counties)[2] <- "County"

colnames(covid_NY_TS_counties)[3] <- "State"

# Create a "New York State" row
covid_TS_New_York <- covid_NY_TS_counties[,-c(1,2)] %>%
  group_by(State) %>%
  summarize_all(sum)

colnames(covid_TS_New_York)[1] <- 'County'
covid_TS_New_York[1,1] <- "New York State"

# Prepend New York State summary to counties summary
# THIS IS "WIDE"!
covid_NY_TS_counties <- data.frame(rbind(covid_TS_New_York, covid_NY_TS_counties[,-c(1,3)]))

covid_NY_counties <- covid_NY_TS_counties[,c(1,ncol(covid_NY_TS_counties))] 

colnames(covid_NY_counties) <- c("state","deaths")

# Make backup of existing WIDE data
write_csv(read_csv("data/csv/time_series/covid_TS_counties_wide.csv"),"data/csv/time_series/covid_NY_TS_counties_wide.csv.bak")
write_csv(read_csv("data/csv/time_series/covid_NY_counties.csv"),"data/csv/time_series/covid_NY_counties.csv.bak")

# write out new WIDE dataframe to file system
write_csv(covid_NY_TS_counties,"data/csv/time_series/covid_NY_TS_counties_wide.csv")
write_csv(covid_NY_counties,"data/csv/time_series/covid_NY_counties.csv")

# NOW "gather" to create "LONG" version
covid_NY_TS_counties_long <- covid_NY_TS_counties %>%
  gather(date,deaths,2:ncol(covid_NY_TS_counties))

# Make date column an actual R date_time
covid_NY_TS_counties_long$date <- str_sub(covid_NY_TS_counties_long$date, 2,-1)
covid_NY_TS_counties_long$date <- parse_date_time(covid_NY_TS_counties_long$date, c("%m.%d.%y"))

covid_NY_TS_counties_long$County <- factor(covid_NY_TS_counties_long$County)

covid_NY_TS_counties_long <- covid_NY_TS_counties_long %>% 
    filter(deaths >= 2)

# Make backup of existing LONG data
write_csv(read_csv("data/csv/time_series/covid_NY_TS_counties_long.csv"),"data/csv/time_series/covid_NY_TS_counties_long.csv.bak")

# write out new LONG dataframe to file system
write_csv(covid_NY_TS_counties_long,"data/csv/time_series/covid_NY_TS_counties_long.csv")


#### Quickie plot to verify
covid_NY_TS_plot <- covid_NY_TS_counties_long %>%
  group_by(date)

covid_NY_TS_plot$log_deaths <- log10(covid_NY_TS_plot$deaths)

p.log <- covid_NY_TS_plot %>% 
  mutate(
    County = County,     # use County to define separate curves
    Date = update(date, year = 1)  # use a constant year for the x-axis
  ) %>% 
  ggplot(aes(Date, log_deaths, color = County)) +
  geom_line() +
  ggtitle("New York State COVID-19 Deaths (log10 scale) (Jan - Apr 2020)")

p.log
#ggplotly(p.log)

# p <- covid_TS_plot %>% 
#   mutate(
#     State = NAME,     # use NAME to define separate curves
#     Date = update(date, year = 1)  # use a constant year for the x-axis
#   ) %>% 
#   ggplot(aes(Date, deaths, color = State)) +
#   geom_line() +
#   ggtitle("COVID-19 Deaths (Jan - Apr 2020)")
# 
# p
