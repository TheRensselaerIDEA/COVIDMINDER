# Daily data importer
# Source: JHU Daily Reports (github): https://bit.ly/3dMWRP6

# UPDATE: Now pulling daily & cumulative NY county-level data from: https://health.data.ny.gov/resource/xdss-u53e.csv

# NOTE: This is a special version to prep daily TS data for NY State

library(tidyverse)
library(lubridate)
library(stringr)
library(plotly)

# curl newest TIME SERIES data from JHU github
dateURL.1.cases <- "time_series_covid19_confirmed_US.csv"   # download cases
dateURL.1.deaths <- "time_series_covid19_deaths_US.csv"        # download deaths
dateURL.2 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

# Write raw data to file system; use JHU syntax as above
download.file(paste0(dateURL.2,dateURL.1.cases), paste0("data/csv/time_series/", dateURL.1.cases))
download.file(paste0(dateURL.2,dateURL.1.deaths), paste0("data/csv/time_series/", dateURL.1.deaths))

# NEW: Download NY daily testing results
download.file("https://health.data.ny.gov/resource/xdss-u53e.csv",paste0("data/csv/time_series/NY_daily_testing.csv"))

# Import raw into R
todays_TS_data.cases <- read_csv(paste0("data/csv/time_series/", dateURL.1.cases))
todays_TS_data.deaths <- read_csv(paste0("data/csv/time_series/", dateURL.1.deaths))

todays_TS_NY_testing <- read_csv("data/csv/time_series/NY_daily_testing.csv")

# Force it to most recent day (for now)
todays_NY_testing <- todays_TS_NY_testing %>% 
  filter(test_date == max(test_date))

# Read in NY county population list 
NY_county_data <- read_csv("data/csv/time_series/NY_population.csv")

# Join above County data with daily testing results
NY_county_data <- inner_join(NY_county_data, todays_NY_testing, by = c("County" = "county"))

# Add cumulative case rate
NY_county_data$case_rate <- NY_county_data$cumulative_number_of_positives/NY_county_data$Population

# Write it out 
write_csv(NY_county_data, "data/csv/time_series/NY_county_data.csv")

# NEW YORK STATE DEATHS
# Transform to match our structure: NY deaths
covid_NY_TS_counties.deaths <- todays_TS_data.deaths %>%
  filter(Country_Region == "US") %>%
  filter(Province_State == "New York") %>%
#  select(-UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Lat, -Long_, -Combined_Key, -Population) %>%
  select(-UID, -iso2, -iso3, -code3, -Combined_Key,-Country_Region) %>%
  group_by(FIPS, Admin2,Province_State) %>%
  select(-Lat, -Long_, -Population) %>%
  summarize_all(sum)

# Change colnames to match app
colnames(covid_NY_TS_counties.deaths)[2] <- "County"

colnames(covid_NY_TS_counties.deaths)[3] <- "State"

# Create a "New York State" row
covid_TS_New_York.deaths <- covid_NY_TS_counties.deaths[,-c(1,2)] %>%
  group_by(State) %>%
  summarize_all(sum)

colnames(covid_TS_New_York.deaths)[1] <- 'County'
covid_TS_New_York.deaths[1,1] <- "New York State"

# Prepend New York State summary to counties summary
# THIS IS "WIDE"!
covid_NY_TS_counties.deaths <- data.frame(rbind(covid_TS_New_York.deaths, covid_NY_TS_counties.deaths[,-c(1,3)]))

# The "right most" column is the cumulative tally to-date
# DISABLED until JHU gets their act togethter
# covid_NY_counties.deaths <- covid_NY_TS_counties.deaths[,c(1,ncol(covid_NY_TS_counties.deaths))] 
# colnames(covid_NY_counties.deaths) <- c("county","deaths")

# UPDATE: Manually curated, from NYSDOH
# Disable this when JHU gets their act together on NYS...
covid_NY_counties.deaths <- read_csv("data/csv/time_series/covid_NY_counties.deaths.manual.csv")

# Make backup of existing WIDE data
write_csv(read_csv("data/csv/time_series/covid_NY_TS_counties_wide.deaths.csv"),"data/csv/time_series/covid_NY_TS_counties_wide.deaths.csv.bak")
write_csv(read_csv("data/csv/time_series/covid_NY_counties.deaths.csv"),"data/csv/time_series/covid_NY_counties.deaths.csv.bak")

# write out new WIDE dataframe to file system
write_csv(covid_NY_TS_counties.deaths,"data/csv/time_series/covid_NY_TS_counties_wide.deaths.csv")
write_csv(covid_NY_counties.deaths,"data/csv/time_series/covid_NY_counties.deaths.csv")

# NOW "gather" to create "LONG" version
covid_NY_TS_counties_long.deaths <- covid_NY_TS_counties.deaths %>%
  gather(date,deaths,2:ncol(covid_NY_TS_counties.deaths))

# Make date column an actual R date_time
covid_NY_TS_counties_long.deaths$date <- str_sub(covid_NY_TS_counties_long.deaths$date, 2,-1)
covid_NY_TS_counties_long.deaths$date <- parse_date_time(covid_NY_TS_counties_long.deaths$date, c("%m.%d.%y"))

covid_NY_TS_counties_long.deaths$County <- factor(covid_NY_TS_counties_long.deaths$County)

# Make backup of existing LONG data
write_csv(read_csv("data/csv/time_series/covid_NY_TS_counties_long.deaths.csv"),"data/csv/time_series/covid_NY_TS_counties_long.deaths.csv.bak")

# write out new LONG dataframe to file system
write_csv(covid_NY_TS_counties_long.deaths,"data/csv/time_series/covid_NY_TS_counties_long.deaths.csv")

#### Quickie plot to verify

# # Set number to clean up plot; comment out when running to update data!
 covid_NY_TS_counties_long.deaths <- covid_NY_TS_counties_long.deaths %>%
     filter(deaths >= 2)%>%
   filter(County != "Unassigned")

 covid_NY_TS_plot.deaths <- covid_NY_TS_counties_long.deaths %>%
   group_by(date)

 covid_NY_TS_plot.deaths$log_deaths <- log10(covid_NY_TS_plot.deaths$deaths)

# ## Test: NY Deaths plot
p.log.deaths <- covid_NY_TS_plot.deaths %>%
  mutate(
    County = County,     # use County to define separate curves
    Date = update(date, year = 1)  # use a constant year for the x-axis
  ) %>%
  ggplot(aes(Date, log_deaths, color = County)) +
  geom_line() +
  ylab("log(Cumulative Number of Deaths") + 
  ggtitle("New York State COVID-19 Deaths (log scale) (Mar-Apr 2020) (4/13/2020)")

p.log.deaths

# NEW YORK STATE CASES
# Transform to match our structure: NY deaths
covid_NY_TS_counties.cases <- todays_TS_data.cases %>%
  filter(Country_Region == "US") %>%
  filter(Province_State == "New York") %>%
  #  select(-UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Lat, -Long_, -Combined_Key, -Population) %>%
  select(-UID, -iso2, -iso3, -code3, -Combined_Key,-Country_Region) %>%
  group_by(FIPS, Admin2,Province_State) %>%
  select(-Lat, -Long_) %>%
  summarize_all(sum)

# Change colnames to match app
colnames(covid_NY_TS_counties.cases)[2] <- "County"

colnames(covid_NY_TS_counties.cases)[3] <- "State"

# Create a "New York State" row
covid_TS_New_York.cases <- covid_NY_TS_counties.cases[,-c(1,2)] %>%
  group_by(State) %>%
  summarize_all(sum)

colnames(covid_TS_New_York.cases)[1] <- 'County'
covid_TS_New_York.cases[1,1] <- "New York State"

# Prepend New York State summary to counties summary
# THIS IS "WIDE"!
covid_NY_TS_counties.cases <- data.frame(rbind(covid_TS_New_York.cases, covid_NY_TS_counties.cases[,-c(1,3)]))

# The "right most" column is the cumulative tally to-date
covid_NY_counties.cases <- covid_NY_TS_counties.cases[,c(1,ncol(covid_NY_TS_counties.cases))] 

colnames(covid_NY_counties.cases) <- c("county","cases")

# Make backup of existing WIDE data
write_csv(read_csv("data/csv/time_series/covid_NY_TS_counties_wide.cases.csv"),"data/csv/time_series/covid_NY_TS_counties_wide.cases.csv.bak")
write_csv(read_csv("data/csv/time_series/covid_NY_counties.cases.csv"),"data/csv/time_series/covid_NY_counties.cases.csv.bak")

# write out new WIDE dataframe to file system
write_csv(covid_NY_TS_counties.cases,"data/csv/time_series/covid_NY_TS_counties_wide.cases.csv")
write_csv(covid_NY_counties.cases,"data/csv/time_series/covid_NY_counties.cases.csv")

# NOW "gather" to create "LONG" version
covid_NY_TS_counties_long.cases <- covid_NY_TS_counties.cases %>%
  gather(date,cases,2:ncol(covid_NY_TS_counties.cases))

# Make date column an actual R date_time
covid_NY_TS_counties_long.cases$date <- str_sub(covid_NY_TS_counties_long.cases$date, 2,-1)
covid_NY_TS_counties_long.cases$date <- parse_date_time(covid_NY_TS_counties_long.cases$date, c("%m.%d.%y"))

covid_NY_TS_counties_long.cases$County <- factor(covid_NY_TS_counties_long.cases$County)

# Make backup of existing LONG data
write_csv(read_csv("data/csv/time_series/covid_NY_TS_counties_long.cases.csv"),"data/csv/time_series/covid_NY_TS_counties_long.cases.csv.bak")

# write out new LONG dataframe to file system
write_csv(covid_NY_TS_counties_long.cases,"data/csv/time_series/covid_NY_TS_counties_long.cases.csv")

# Create COMBINED data frame
covid_NY_TS_counties_long <- left_join(covid_NY_TS_counties_long.deaths, covid_NY_TS_counties_long.cases, by = c('County', 'date'))

write_csv(read_csv("data/csv/time_series/covid_NY_TS_counties_long.csv"),"data/csv/time_series/covid_NY_TS_counties_long.csv.bak")
write_csv(covid_NY_TS_counties_long,"data/csv/time_series/covid_NY_TS_counties_long.csv")

# Create COMBINED data frame
# UPDATE: We want to use the NY API data for cases: NY_county_data
# covid_NY_counties <- left_join(covid_NY_counties.deaths, covid_NY_counties.cases, by = c('county'))
covid_NY_counties <- left_join(covid_NY_counties.deaths, NY_county_data[,c(2,8)], by = c('county'='County'))

# Adjust to match app
colnames(covid_NY_counties) <- c("county","deaths","cases")
# Need cumulative
covid_NY_counties[1,3] <- sum(na.omit(covid_NY_counties$cases))

write_csv(read_csv("data/csv/time_series/covid_NY_counties.csv"),"data/csv/time_series/covid_NY_counties.csv.bak")
write_csv(covid_NY_counties,"data/csv/time_series/covid_NY_counties.csv")

#### Quickie plot to verify
# Set number to clean up plot; comment out when running to update data!
covid_NY_TS_counties_long <- covid_NY_TS_counties_long %>% 
   filter(cases >= 1) %>%
  filter(County != "Unassigned")

# Need regions for times series plots!
NY_counties_regions <- read_csv("data/csv/time_series/NY_counties_regions.csv")

covid_NY_TS_counties_long <- dplyr::inner_join(covid_NY_TS_counties_long, as.data.frame(NY_counties_regions), by = c("County" = "County"))

covid_NY_TS_plot.cases <- covid_NY_TS_counties_long %>%
  group_by(date)

covid_NY_TS_plot.cases$log_cases <- log10(covid_NY_TS_plot.cases$cases)

highlight_points <- covid_NY_TS_plot.cases %>% 
                     filter(County == "New York State" & date == as.Date("2020-03-26") |
                            County == "New York" & date == as.Date("2020-03-29") |
                            County == "Suffolk" & date == as.Date("2020-04-01") |
                            County == "Nassau" & date == as.Date("2020-04-02") |
                            County == "Westchester" & date == as.Date("2020-04-02")
                     )

p.log.cases <- covid_NY_TS_plot.cases %>%
  ggplot(aes(date, cases, color = Region, group=County)) +
  geom_line() +
  scale_y_continuous(
    trans = "log10",
    breaks = c(10,100,500,1000,5000,10000, 50000)
  ) +
  scale_x_datetime(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%b %d") +
  ylab("Cumulative Number of Cases") + 
  ggtitle("New York State COVID-19 Cases (Mar-Apr 2020)")  + 
  geom_text_repel(data=highlight_points,  aes(label=County)) + 
  NULL
#library(plotly)
p.log.cases
#ggplotly(p.log.cases)