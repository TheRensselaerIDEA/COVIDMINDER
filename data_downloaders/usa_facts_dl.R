# Author: Jose Figueroa
# https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
# Provides nationwide county level case, deaths, and population data. 
# https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv
# https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv
# https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_county_population_usafacts.csv
# Files updated by this script are:
# "data/csv/covid_confirmed_usafacts.csv"
# "data/csv/covid_deaths_usafacts.csv"
# "data/csv/covid_county_population_usafacts.csv"
# "data/csv/time_series/covid_TS_counties_long.cases.csv"
# "data/csv/time_series/covid_TS_counties_long.cases.csv.bak"
# "data/csv/time_series/covid_TS_states_long.cases.csv"
# "data/csv/time_series/covid_TS_states_long.cases.csv.bak"
# "data/csv/time_series/covid_TS_US_long.cases.csv"
# "data/csv/time_series/covid_TS_US_long.cases.csv.bak"
# "data/csv/time_series/covid_NY_TS_counties_wide.deaths.csv"
# "data/csv/time_series/covid_NY_TS_counties_wide.deaths.csv.bak"
# "data/csv/time_series/covid_NY_counties.deaths.csv"
# "data/csv/time_series/covid_NY_counties.deaths.csv.bak"
# "data/csv/time_series/covid_NY_TS_counties_long.deaths.csv"
# "data/csv/time_series/covid_NY_TS_counties_long.deaths.csv.bak"
# "data/csv/time_series/covid_NY_TS_plot.deaths.csv"
# "data/csv/time_series/covid_NY_TS_plot.deaths.csv.bak"
# "data/csv/todays_case_data.csv"
# "data/csv/todays_case_data.csv.bak"

library(dplyr)
library(stringr)
library(lubridate)

base.url <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/"
case.file <- "covid_confirmed_usafacts.csv"
death.file <- "covid_deaths_usafacts.csv"
pop.file <- "covid_county_population_usafacts.csv"
file.dir <- "data/csv/time_series/"

# Download latest files
download.file(paste0(base.url,case.file), paste0(file.dir,case.file))
download.file(paste0(base.url,death.file), paste0(file.dir,death.file))
# Population not needed daily
# download.file(paste0(base.url,pop.file), paste0(file.dir,pop.file))

# Read in latest data
todays.raw.case.data <- read_csv(paste0(file.dir,case.file))
todays.raw.death.data <- read_csv(paste0(file.dir,death.file))
population <- read_csv(paste0(file.dir,pop.file))  %>%
  filter(countyFIPS > 1000)

# State report card data
todays.case.data <- todays.raw.case.data %>%
  filter(countyFIPS > 1000) %>%
  select(countyFIPS, `County Name`, State, ncol(todays.raw.case.data)) %>%
  rename(c("County" = "County Name"))
colnames(todays.case.data)[4] <- "Cases"

todays.case.data <- inner_join(todays.case.data, population[c(1,4)], by=c("countyFIPS" = "countyFIPS")) %>%
  filter(population > 0)

todays.death.data <- todays.raw.death.data %>%
  filter(countyFIPS > 1000) %>%
  select(1, ncol(todays.raw.death.data))
colnames(todays.death.data)[2] <- "Mortality"

todays.case.data <- inner_join(todays.case.data, todays.death.data, by=c("countyFIPS" = "countyFIPS"))

todays.case.data <- todays.case.data %>%
  mutate(County = str_remove_all(County, regex(" County", ignore_case = T)))

# Backup case data
write_csv(read_csv("data/csv/todays_case_data.csv"), "data/csv/todays_case_data.csv.bak")
write_csv(todays.case.data, "data/csv/todays_case_data.csv")


# PLOT implimentation

### County Level ###
covid_TS_counties_long.cases <- todays.raw.case.data %>%
  tidyr::gather(date,cases,5:ncol(todays.raw.case.data))
covid_TS_counties_long.death <- todays.raw.death.data %>%
  tidyr::gather(date,deaths,5:ncol(todays.raw.death.data))

# Make date column an actual R date_time
covid_TS_counties_long.cases$date <- parse_date_time(covid_TS_counties_long.cases$date, c("%m/%d/%y"))
covid_TS_counties_long.death$date <- parse_date_time(covid_TS_counties_long.death$date, c("%m/%d/%y"))

# Factor does not make sense here as there may be distint, same named counties
covid_TS_counties_long.cases <- covid_TS_counties_long.cases %>%
  rename(County = `County Name`) %>%
  mutate(County = str_remove_all(County, regex(" County", ignore_case = T)))

# Join death data
covid_TS_counties_long.cases <- inner_join(covid_TS_counties_long.cases, 
                                           covid_TS_counties_long.death[c(1,5,6)], 
                                           by=c("countyFIPS" = "countyFIPS", "date" = "date"))
# Filter small data
covid_TS_counties_long.cases <- covid_TS_counties_long.cases %>%
  filter(countyFIPS > 1000) %>%
  filter(cases > 5)

# Join population data
covid_TS_counties_long.cases <- left_join(covid_TS_counties_long.cases, population[c(1,4)], by=c("countyFIPS" = "countyFIPS"))

# Per100k columns
covid_TS_counties_long.cases$p_cases <- covid_TS_counties_long.cases$cases/covid_TS_counties_long.cases$population*100000
covid_TS_counties_long.cases$p_deaths <- covid_TS_counties_long.cases$deaths/covid_TS_counties_long.cases$population*100000

# Changes in cases
covid_TS_counties_long.cases %>% group_by(County, State) %>% 
  mutate(diff = ifelse(as.Date(date - 1) == lag(date), cases - lag(cases), cases)) -> 
  covid_TS_counties_long.cases

covid_TS_counties_long.cases %>% 
  mutate(p_diff = ifelse(as.Date(date - 1) == lag(date), p_cases - lag(p_cases), p_cases)) %>%
  ungroup() -> covid_TS_counties_long.cases

covid_TS_counties_long.cases$diff <- ifelse(is.na(covid_TS_counties_long.cases$diff), covid_TS_counties_long.cases$cases, covid_TS_counties_long.cases$diff)
covid_TS_counties_long.cases$p_diff <- ifelse(is.na(covid_TS_counties_long.cases$p_diff), covid_TS_counties_long.cases$p_cases, covid_TS_counties_long.cases$p_diff)

# Changes in deaths
covid_TS_counties_long.cases %>% group_by(County, State) %>% 
  mutate(d_diff = ifelse(as.Date(date - 1) == lag(date), deaths - lag(deaths), deaths)) -> 
  covid_TS_counties_long.cases

covid_TS_counties_long.cases %>% 
  mutate(p.d_diff = ifelse(as.Date(date - 1) == lag(date), p_deaths - lag(p_deaths), p_deaths)) %>%
  ungroup() -> covid_TS_counties_long.cases

covid_TS_counties_long.cases$d_diff <- ifelse(is.na(covid_TS_counties_long.cases$d_diff), covid_TS_counties_long.cases$deaths, covid_TS_counties_long.cases$d_diff)
covid_TS_counties_long.cases$p.d_diff <- ifelse(is.na(covid_TS_counties_long.cases$p.d_diff), covid_TS_counties_long.cases$p_deaths, covid_TS_counties_long.cases$p.d_diff)

# Backup case time series data
write_csv(read_csv("data/csv/time_series/covid_TS_counties_long.cases.csv"), "data/csv/time_series/covid_TS_counties_long.cases.csv.bak")
write_csv(covid_TS_counties_long.cases, "data/csv/time_series/covid_TS_counties_long.cases.csv")


### State Level ###

covid_TS_state_long.cases <- todays.raw.case.data %>%
  tidyr::gather(date,cases,5:ncol(todays.raw.case.data))
covid_TS_state_long.death <- todays.raw.death.data %>%
  tidyr::gather(date,deaths,5:ncol(todays.raw.death.data))

# Make date column an actual R date_time
covid_TS_state_long.cases$date <- parse_date_time(covid_TS_state_long.cases$date, c("%m/%d/%y"))
covid_TS_state_long.death$date <- parse_date_time(covid_TS_state_long.death$date, c("%m/%d/%y"))

# Factor does not make sense here as there may be distint, same named counties
covid_TS_state_long.cases <- covid_TS_state_long.cases %>%
  group_by(State, date) %>%
  summarise(cases = sum(cases))

covid_TS_state_long.death <- covid_TS_state_long.death %>%
  group_by(State, date) %>%
  summarise(deaths = sum(deaths))

# Join death data
covid_TS_state_long.cases <- inner_join(
  covid_TS_state_long.cases, covid_TS_state_long.death, by=c("State" = "State", "date" = "date"))

# Filter small data
covid_TS_state_long.cases <- covid_TS_state_long.cases %>%
  filter(cases > 5)

# Join population data
covid_TS_state_long.cases <- left_join(
  covid_TS_state_long.cases, 
  population %>% group_by(State) %>% summarise(population = sum(population)), 
  by=c("State" = "State"))

# Per100k columns
covid_TS_state_long.cases$p_cases <- covid_TS_state_long.cases$cases/covid_TS_state_long.cases$population*100000

covid_TS_state_long.cases$p_deaths <- covid_TS_state_long.cases$deaths/covid_TS_state_long.cases$population*100000

# Changes in cases
covid_TS_state_long.cases %>% group_by(State) %>% 
  mutate(diff = ifelse(as.Date(date - 1) == lag(date), cases - lag(cases), cases)) -> 
  covid_TS_state_long.cases

covid_TS_state_long.cases %>% 
  mutate(p_diff = ifelse(as.Date(date - 1) == lag(date), p_cases - lag(p_cases), p_cases)) %>%
  ungroup() -> covid_TS_state_long.cases

covid_TS_state_long.cases$diff <- ifelse(is.na(covid_TS_state_long.cases$diff), covid_TS_state_long.cases$cases, covid_TS_state_long.cases$diff)
covid_TS_state_long.cases$p_diff <- ifelse(is.na(covid_TS_state_long.cases$p_diff), covid_TS_state_long.cases$p_cases, covid_TS_state_long.cases$p_diff)

# Changes in deaths
covid_TS_state_long.cases %>% group_by(State) %>% 
  mutate(d_diff = ifelse(as.Date(date - 1) == lag(date), deaths - lag(deaths), deaths)) -> 
  covid_TS_state_long.cases

covid_TS_state_long.cases %>% 
  mutate(p.d_diff = ifelse(as.Date(date - 1) == lag(date), p_deaths - lag(p_deaths), p_deaths)) %>%
  ungroup() -> covid_TS_state_long.cases

covid_TS_state_long.cases$d_diff <- ifelse(is.na(covid_TS_state_long.cases$d_diff), covid_TS_state_long.cases$deaths, covid_TS_state_long.cases$d_diff)
covid_TS_state_long.cases$p.d_diff <- ifelse(is.na(covid_TS_state_long.cases$p.d_diff), covid_TS_state_long.cases$p_deaths, covid_TS_state_long.cases$p.d_diff)

# Backup case time series data
write_csv(read_csv("data/csv/time_series/covid_TS_state_long.cases.csv"), "data/csv/time_series/covid_TS_state_long.cases.csv.bak")
write_csv(covid_TS_state_long.cases, "data/csv/time_series/covid_TS_state_long.cases.csv")

### US Level ###

covid_TS_US_long.cases <- todays.raw.case.data %>%
  tidyr::gather(date,cases,5:ncol(todays.raw.case.data))
covid_TS_US_long.death <- todays.raw.death.data %>%
  tidyr::gather(date,deaths,5:ncol(todays.raw.death.data))

# Make date column an actual R date_time
covid_TS_US_long.cases$date <- parse_date_time(covid_TS_US_long.cases$date, c("%m/%d/%y"))
covid_TS_US_long.death$date <- parse_date_time(covid_TS_US_long.death$date, c("%m/%d/%y"))

# Factor does not make sense here as there may be distint, same named counties
covid_TS_US_long.cases <- covid_TS_US_long.cases %>%
  group_by(date) %>%
  summarise(cases = sum(cases))

covid_TS_US_long.death <- covid_TS_US_long.death %>%
  group_by(date) %>%
  summarise(deaths = sum(deaths))

# Join death data
covid_TS_US_long.cases <- inner_join(
  covid_TS_US_long.cases, covid_TS_US_long.death, by=c("date" = "date"))

# Filter small data
covid_TS_US_long.cases <- covid_TS_US_long.cases %>%
  filter(cases > 5)

# Join population data
US.pop <- sum(population$population)

# Per100k columns
covid_TS_US_long.cases$p_cases <- covid_TS_US_long.cases$cases/US.pop*100000
covid_TS_US_long.cases$p_deaths <- covid_TS_US_long.cases$deaths/US.pop*100000

# Changes in cases
covid_TS_US_long.cases %>%
  mutate(diff = ifelse(as.Date(date - 1) == lag(date), cases - lag(cases), cases)) -> 
  covid_TS_US_long.cases

covid_TS_US_long.cases %>% 
  mutate(p_diff = ifelse(as.Date(date - 1) == lag(date), p_cases - lag(p_cases), p_cases)) -> 
  covid_TS_US_long.cases

covid_TS_US_long.cases$diff <- ifelse(is.na(covid_TS_US_long.cases$diff), covid_TS_US_long.cases$cases, covid_TS_US_long.cases$diff)
covid_TS_US_long.cases$p_diff <- ifelse(is.na(covid_TS_US_long.cases$p_diff), covid_TS_US_long.cases$p_cases, covid_TS_US_long.cases$p_diff)

# Changes in deaths
covid_TS_US_long.cases %>% 
  mutate(d_diff = ifelse(as.Date(date - 1) == lag(date), deaths - lag(deaths), deaths)) -> 
  covid_TS_US_long.cases

covid_TS_US_long.cases %>% 
  mutate(p.d_diff = ifelse(as.Date(date - 1) == lag(date), p_deaths - lag(p_deaths), p_deaths)) -> 
  covid_TS_US_long.cases

covid_TS_US_long.cases$d_diff <- ifelse(is.na(covid_TS_US_long.cases$d_diff), covid_TS_US_long.cases$deaths, covid_TS_US_long.cases$d_diff)
covid_TS_US_long.cases$p.d_diff <- ifelse(is.na(covid_TS_US_long.cases$p.d_diff), covid_TS_US_long.cases$p_deaths, covid_TS_US_long.cases$p.d_diff)


# Backup case time series data
write_csv(read_csv("data/csv/time_series/covid_TS_US_long.cases.csv"), "data/csv/time_series/covid_TS_US_long.cases.csv.bak")
write_csv(covid_TS_US_long.cases, "data/csv/time_series/covid_TS_US_long.cases.csv")

# Grab NY specific data
deaths.TS <- todays.raw.death.data %>%
  mutate(`County Name` = str_remove_all(`County Name`, regex(" County", ignore_case = T)))
todays_TS_data.NY.deaths <- deaths.TS %>% 
  dplyr::filter(State == "NY") %>%
  dplyr::select(-c(countyFIPS, State, stateFIPS)) %>%
  dplyr::rename(County = `County Name`)

# Organize columns
# todays_TS_data.NY.deaths <- todays_TS_data.NY.deaths[c(1,ncol(todays_TS_data.NY.deaths),3:ncol(todays_TS_data.NY.deaths)-1)]


# Need to create NYState summary row here...
TS_New_York_State.deaths <- todays_TS_data.NY.deaths[,-c(1)] %>%
  dplyr::summarize_all(sum)

County <- as.vector(c("New York State"))
TS_New_York_State.deaths <- data.frame(cbind(County,TS_New_York_State.deaths))

TS_New_York_State.deaths.long <- TS_New_York_State.deaths %>% 
  tidyr::gather(date,deaths,2:ncol(TS_New_York_State.deaths))

#Data type
TS_New_York_State.deaths.long$date <- str_sub(TS_New_York_State.deaths.long$date, 2,-1)
TS_New_York_State.deaths.long$date <- parse_date_time(TS_New_York_State.deaths.long$date, c("%m.%d.%y"))
TS_New_York_State.deaths.long$County <- as.factor(TS_New_York_State.deaths.long$County)

# Long version of data
todays_TS_data.NY.deaths.long <- todays_TS_data.NY.deaths %>% 
  tidyr::gather(date,deaths,2:ncol(todays_TS_data.NY.deaths))

todays_TS_data.NY.deaths.long$date <- parse_date_time(todays_TS_data.NY.deaths.long$date, c("%m/%d/%y"))
todays_TS_data.NY.deaths.long$County <- as.factor(todays_TS_data.NY.deaths.long$County)

# Now we can rbind NYS and NY counties long
todays_TS_data.NY.deaths.long <- data.frame(rbind(TS_New_York_State.deaths.long, todays_TS_data.NY.deaths.long))


# NEW YORK STATE DEATHS
# Transform to match our structure: NY deaths

covid_NY_TS_counties.deaths <- deaths.TS %>%
  dplyr::filter(State == "NY") %>%
  dplyr::select(-stateFIPS) %>%
  dplyr::group_by(countyFIPS, `County Name`,State) %>%
  dplyr::rename(County = `County Name`)  %>%
  dplyr::rename(FIPS = countyFIPS) %>%
  dplyr::summarize_all(sum)

# Create NYS row
covid_TS_New_York.deaths <- covid_NY_TS_counties.deaths [,-c(1,2)] %>%
  dplyr::group_by(State) %>%
  dplyr::summarize_all(sum)

colnames(covid_TS_New_York.deaths)[1] <- 'County'
covid_TS_New_York.deaths[1,1] <- "New York State"

# Prepend New York State summary to counties summary
# THIS IS "WIDE"!
covid_NY_TS_counties.deaths <- data.frame(rbind(covid_TS_New_York.deaths, covid_NY_TS_counties.deaths[-c(1,2),-c(1,3)]))

# The "right most" column is the cumulative tally to-date
covid_NY_counties.deaths <- covid_NY_TS_counties.deaths[,c(1,ncol(covid_NY_TS_counties.deaths))] 

colnames(covid_NY_counties.deaths) <- c("county","deaths")

# Make backup of existing WIDE data
write_csv(read_csv("data/csv/time_series/covid_NY_TS_counties_wide.deaths.csv"),"data/csv/time_series/covid_NY_TS_counties_wide.deaths.csv.bak")
write_csv(read_csv("data/csv/time_series/covid_NY_counties.deaths.csv"),"data/csv/time_series/covid_NY_counties.deaths.csv.bak")

# write out new WIDE dataframe to file system
write_csv(covid_NY_TS_counties.deaths,"data/csv/time_series/covid_NY_TS_counties_wide.deaths.csv")
write_csv(covid_NY_counties.deaths,"data/csv/time_series/covid_NY_counties.deaths.csv")

# LONG county ts
covid_NY_TS_counties_long.deaths <- covid_NY_TS_counties.deaths %>%
  tidyr::gather(date,deaths,2:ncol(covid_NY_TS_counties.deaths))

# Make date column an actual R date_time
covid_NY_TS_counties_long.deaths$date <- str_sub(covid_NY_TS_counties_long.deaths$date, 2,-1)
covid_NY_TS_counties_long.deaths$date <- parse_date_time(covid_NY_TS_counties_long.deaths$date, c("%m.%d.%y"))

covid_NY_TS_counties_long.deaths$County <- factor(covid_NY_TS_counties_long.deaths$County)

# Make backup of existing LONG data
write_csv(read_csv("data/csv/time_series/covid_NY_TS_counties_long.deaths.csv"),"data/csv/time_series/covid_NY_TS_counties_long.deaths.csv.bak")

# write out new LONG dataframe to file system
write_csv(covid_NY_TS_counties_long.deaths,"data/csv/time_series/covid_NY_TS_counties_long.deaths.csv")

covid_NY_TS_counties_long.deaths <- covid_NY_TS_counties_long.deaths %>%
  dplyr::filter(deaths > 0)%>%
  dplyr::filter(County != "Unassigned")
covid_NY_TS_plot.deaths <- covid_NY_TS_counties_long.deaths %>%
  dplyr::group_by(date)
covid_NY_TS_plot.deaths$log_deaths <- log10(covid_NY_TS_plot.deaths$deaths)
NY_population <- read_csv("data/csv/time_series/NY_population.csv")
covid_NY_TS_plot.deaths <- dplyr::inner_join(covid_NY_TS_plot.deaths, as.data.frame(NY_population), by = c("County" = "County"))
covid_NY_TS_plot.deaths <- covid_NY_TS_plot.deaths %>% 
  dplyr::select(-FIPS)

# Append death rates per county!
covid_NY_TS_plot.deaths <- covid_NY_TS_plot.deaths %>%
  dplyr::mutate(p_deaths = (deaths/Population)*100000) %>%
  dplyr::mutate(log_p_deaths = log10(p_deaths)) 

# Regions for plots
NY_counties_regions <- read_csv("data/csv/time_series/NY_counties_regions.csv")

covid_NY_TS_plot.deaths <- dplyr::inner_join(covid_NY_TS_plot.deaths, as.data.frame(NY_counties_regions), by = c("County" = "County"))

# Make backup of existing LONG data
write_csv(read_csv("data/csv/time_series/covid_NY_TS_plot.deaths.csv"),"data/csv/time_series/covid_NY_TS_plot.deaths.csv.bak")

# make sure we have the same version for our app plot!
write_csv(covid_NY_TS_plot.deaths, "data/csv/time_series/covid_NY_TS_plot.deaths.csv")
