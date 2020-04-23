# COVIDMINDER Daily data downloader (4 of 5)
# Source: JHU Daily Reports (github): https://bit.ly/3dMWRP6
# FILES UPDATED BY THIS SCRIPT:
# "data/csv/time_series/time_series_covid19_confirmed_US.csv"
# "data/csv/time_series/time_series_covid19_deaths_US.csv"
# "data/csv/time_series/NY_county_data.csv"
# "data/csv/time_series/covid_NY_TS_counties_wide.deaths.csv"
# "data/csv/time_series/covid_NY_counties.deaths.csv"
# "data/csv/time_series/covid_NY_TS_counties_wide.deaths.csv.bak"
# "data/csv/time_series/covid_NY_counties.deaths.csv.bak"
# "data/csv/time_series/covid_NY_TS_counties_long.deaths.csv"
# "data/csv/time_series/covid_NY_TS_counties_long.deaths.csv.bak"
# "data/csv/time_series/covid_NY_counties.csv"
# "data/csv/time_series/covid_NY_counties.csv.bak"
# "data/csv/time_series/covid_NY_TS_counties_long.cases.csv"
# "data/csv/time_series/covid_NY_TS_counties_long.cases.csv.bak"
# "data/csv/time_series/covid_NY_TS_plot.cases.csv"   # TODO: check this!

# UPDATE: Now pulling daily & cumulative NY county-level data from: https://health.data.ny.gov/resource/xdss-u53e.csv

# NOTE: This is a special version to prep daily TS data for NY State

library(tidyverse)
library(lubridate)
library(stringr)
library(plotly)
library(ggrepel)

# curl newest TIME SERIES data from JHU github
dateURL.1.cases <- "time_series_covid19_confirmed_US.csv"   # download cases
dateURL.1.deaths <- "time_series_covid19_deaths_US.csv"        # download deaths
dateURL.2 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

# Write raw data to file system; use JHU syntax as above
download.file(paste0(dateURL.2,dateURL.1.cases), paste0("data/csv/time_series/", dateURL.1.cases))
download.file(paste0(dateURL.2,dateURL.1.deaths), paste0("data/csv/time_series/", dateURL.1.deaths))

# NEW: Download NY daily testing results
download.file("https://health.data.ny.gov/resource/xdss-u53e.csv",paste0("data/csv/time_series/NY_daily_testing.csv"))

# Import raw into R (JHU)
todays_TS_data.cases <- read_csv(paste0("data/csv/time_series/", dateURL.1.cases))
todays_TS_data.deaths <- read_csv(paste0("data/csv/time_series/", dateURL.1.deaths))

# Prep NY TS data (JHU); this has one column per test date
todays_TS_data.NY.cases <- todays_TS_data.cases %>% 
  dplyr::filter(Province_State == "New York") %>%
  dplyr::select(-UID, -iso2, -iso3, -code3, -FIPS, -Province_State,-Country_Region, -Lat, -Long_, -Combined_Key) %>%
  dplyr::rename(County = Admin2) 

# Need to create NYState summary row here...
TS_New_York_State.cases <- todays_TS_data.NY.cases[,-c(1)] %>%
  dplyr::summarize_all(sum)

County <- as.vector(c("New York State"))

TS_New_York_State.cases <- data.frame(cbind(County,TS_New_York_State.cases))
# Convert this to "long" first
TS_New_York_State.cases.long <- TS_New_York_State.cases %>% 
  tidyr::gather(test_date,cases,2:ncol(TS_New_York_State.cases))

# Make sure the NYS summary stuff is properly structured
TS_New_York_State.cases.long$test_date <- str_sub(TS_New_York_State.cases.long$test_date, 2,-1)
TS_New_York_State.cases.long$test_date <- parse_date_time(TS_New_York_State.cases.long$test_date, c("%m.%d.%y"))
TS_New_York_State.cases.long$County <- as.factor(TS_New_York_State.cases.long$County)

# Create long version of JHU TS
todays_TS_data.NY.cases.long <- todays_TS_data.NY.cases %>% 
  tidyr::gather(test_date,cases,2:ncol(todays_TS_data.NY.cases))

todays_TS_data.NY.cases.long$test_date <- parse_date_time(todays_TS_data.NY.cases.long$test_date, c("%m/%d/%y"))
todays_TS_data.NY.cases.long$County <- as.factor(todays_TS_data.NY.cases.long$County)

# Now we can rbind NYS and NY counties long
todays_TS_data.NY.cases.long <- data.frame(rbind(TS_New_York_State.cases.long, todays_TS_data.NY.cases.long))

# Adjusting JHU dates to NY API...
# todays_TS_data.NY.cases.JHU <- todays_TS_data.NY.cases.long %>% 
#   filter(test_date == (max(test_date))) %>% 
#   mutate(test_date = test_date - 60*60*24) # JHU's dates are 1 day "behind"

# Complete JHU time series: todays_TS_data.NY.cases.JHU

# Read in from NY API (long): This gives correct NYC-area data! 
# Starts at 03-28-2020
todays_TS_NY_testing <- read_csv("data/csv/time_series/NY_daily_testing.csv")

# todays_TS_data.NY.cases.adj <- todays_TS_data.NY.cases.long %>% 
#   mutate(test_date = test_date - 60*60*24) # JHU's dates are 1 day "behind"
# 
# # This creates a joined-up time series
# todays_TS_NY_testing <- left_join(todays_TS_data.NY.cases.adj, todays_TS_NY_testing, by = c("County"="county","test_date"="test_date")) %>%
#   mutate(cumulative_number_of_positives = ifelse(is.na(cumulative_number_of_positives), cases, cumulative_number_of_positives)) 

# Force it to most recent day (for now)
todays_NY_testing <- todays_TS_NY_testing %>% 
  dplyr::filter(test_date == (max(test_date))) 

todays_NY_testing$county <- as.factor(todays_NY_testing$county)

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
  dplyr::filter(Country_Region == "US") %>%
  dplyr::filter(Province_State == "New York") %>%
  dplyr::select(-UID, -iso2, -iso3, -code3, -Combined_Key,-Country_Region) %>%
  dplyr::group_by(FIPS, Admin2, Province_State) %>%
  dplyr::select(-Lat, -Long_, -Population) %>%
  dplyr::summarize_all(sum)

# Change colnames to match app
colnames(covid_NY_TS_counties.deaths)[2] <- "County"

colnames(covid_NY_TS_counties.deaths)[3] <- "State"

# Create a "New York State" row
covid_TS_New_York.deaths <- covid_NY_TS_counties.deaths[,-c(1,2)] %>%
  dplyr::group_by(State) %>%
  dplyr::summarize_all(sum)

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
  tidyr::gather(date,deaths,2:ncol(covid_NY_TS_counties.deaths))

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
   dplyr::filter(deaths >= 2)%>%
   dplyr::filter(County != "Unassigned")

 covid_NY_TS_plot.deaths <- covid_NY_TS_counties_long.deaths %>%
   dplyr::group_by(date)

 covid_NY_TS_plot.deaths$log_deaths <- log10(covid_NY_TS_plot.deaths$deaths)

# ## Test: NY Deaths plot
p.log.deaths <- covid_NY_TS_plot.deaths %>%
  dplyr::mutate(
    County = County,     # use County to define separate curves
    Date = update(date, year = 1)  # use a constant year for the x-axis
  ) %>%
  ggplot(aes(Date, log_deaths, color = County)) +
  geom_line() +
  ylab("log(Cumulative Number of Deaths") + 
  ggtitle("New York State COVID-19 Deaths (log scale) (Mar-Apr 2020) (4/13/2020)")

p.log.deaths

# NEW YORK STATE CASES
# Transform to match our structure: NY cases

covid_NY_TS_counties.cases <- todays_TS_data.cases %>%
  dplyr::filter(Country_Region == "US") %>%
  dplyr::filter(Province_State == "New York") %>%
  #  select(-UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Lat, -Long_, -Combined_Key, -Population) %>%
  dplyr::select(-UID, -iso2, -iso3, -code3, -Combined_Key,-Country_Region) %>%
  dplyr::group_by(FIPS, Admin2,Province_State) %>%
  dplyr::select(-Lat, -Long_) %>%
  dplyr::summarize_all(sum)

# Change colnames to match app
colnames(covid_NY_TS_counties.cases)[2] <- "County"

colnames(covid_NY_TS_counties.cases)[3] <- "State"

# Create a "New York State" row
covid_TS_New_York.cases <- covid_NY_TS_counties.cases[,-c(1,2)] %>%
  dplyr::group_by(State) %>%
  dplyr::summarize_all(sum)

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
  tidyr::gather(date,cases,2:ncol(covid_NY_TS_counties.cases))

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
covid_NY_TS_counties_long <- covid_NY_TS_counties_long.cases %>% 
  dplyr::filter(cases >= 5) %>%
  dplyr::filter(County != "Unassigned")

# Need regions for times series plots!
NY_counties_regions <- read_csv("data/csv/time_series/NY_counties_regions.csv")

covid_NY_TS_counties_long <- dplyr::inner_join(covid_NY_TS_counties_long, as.data.frame(NY_counties_regions), by = c("County" = "County"))

covid_NY_TS_plot.cases <- covid_NY_TS_counties_long %>%
  dplyr::group_by(date)

covid_NY_TS_plot.cases$log_cases <- log10(covid_NY_TS_plot.cases$cases)

# Append population
NY_population <- read_csv("data/csv/time_series/NY_population.csv")

covid_NY_TS_plot.cases <- dplyr::inner_join(covid_NY_TS_plot.cases, as.data.frame(NY_population), by = c("County" = "County"))
covid_NY_TS_plot.cases <- covid_NY_TS_plot.cases %>% 
  dplyr::select(-FIPS)

# Append case rates per county!
covid_NY_TS_plot.cases <- covid_NY_TS_plot.cases %>%
  dplyr::mutate(p_cases = (cases/Population)*100000) %>%
  dplyr::mutate(log_p_cases = log10(p_cases)) 

# make sure we have the same version for our app plot!
write_csv(covid_NY_TS_plot.cases, "data/csv/time_series/covid_NY_TS_plot.cases.csv")

highlight_points <- covid_NY_TS_plot.cases %>% 
  dplyr::filter( County == "Albany" & date == as.Date("2020-03-26") |
            # County == "Allegany" & date == as.Date("2020-03-29") |
            County == "Bronx" & date == as.Date("2020-03-25") |
            # County == "Broome" & date == as.Date("2020-04-02") |
            # County == "Cattaraugus" & date == as.Date("2020-03-30") |
            # County == "Cayuga" & date == as.Date("2020-04-02") |
            # County == "Chautauqua" & date == as.Date("2020-04-10") |
            # County == "Chemung" & date == as.Date("2020-04-10") |
            # County == "Chenango" & date == as.Date("2020-04-12") |
            # County == "Clinton" & date == as.Date("2020-03-26") |
            # County == "Columbia" & date == as.Date("2020-03-29") |
            # County == "Cortland" & date == as.Date("2020-03-25") |
            # County == "Delaware" & date == as.Date("2020-04-02") |
            County == "Dutchess" & date == as.Date("2020-04-06") |
            # County == "Erie" & date == as.Date("2020-04-02") |
            # County == "Essex" & date == as.Date("2020-04-10") |
            # County == "Franklin" & date == as.Date("2020-04-10") |
            # County == "Fulton" & date == as.Date("2020-04-12") |
            # County == "Genesee" & date == as.Date("2020-03-26") |
            # County == "Greene" & date == as.Date("2020-03-29") |
            # County == "Hamilton" & date == as.Date("2020-03-25") |
            # County == "Herkimer" & date == as.Date("2020-04-02") |
            # County == "Jefferson" & date == as.Date("2020-03-30") |
            County == "Kings" & date == as.Date("2020-04-02") |
            # County == "Lewis" & date == as.Date("2020-04-10") |
            # County == "Livingston" & date == as.Date("2020-04-10") |
            # County == "Madison" & date == as.Date("2020-04-12") |
            # County == "Monroe" & date == as.Date("2020-03-26") |
            # County == "Montgomery" & date == as.Date("2020-03-29") |
            County == "Nassau" & date == as.Date("2020-04-12") |
            County == "New York" & date == as.Date("2020-04-12") |
            County == "New York State" & date == as.Date("2020-04-12") |
            County == "Manhattan" & date == as.Date("2020-03-30") |
            # County == "Niagara" & date == as.Date("2020-04-02") |
            # County == "Oneida" & date == as.Date("2020-04-10") |
            # County == "Onondaga" & date == as.Date("2020-04-10") |
            # County == "Ontario" & date == as.Date("2020-04-12") |
            County == "Orange" & date == as.Date("2020-04-09") |
            # County == "Orleans" & date == as.Date("2020-04-12") |
            # County == "Oswego" & date == as.Date("2020-03-25") |
            # County == "Otsego" & date == as.Date("2020-04-02") |
            County == "Putnam" & date == as.Date("2020-04-01") |
            County == "Queens" & date == as.Date("2020-04-02") |
            # County == "Rensselaer" & date == as.Date("2020-04-10") |
            County == "Richmond" & date == as.Date("2020-04-01") |
            County == "Rockland" & date == as.Date("2020-04-12") |
            # County == "St. Lawrence" & date == as.Date("2020-03-26") |
            # County == "Saratoga" & date == as.Date("2020-03-29") |
            # County == "Schenectady" & date == as.Date("2020-03-25") |
            # County == "Schoharie" & date == as.Date("2020-04-02") |
            # County == "Schuyler" & date == as.Date("2020-03-30") |
            # County == "Seneca" & date == as.Date("2020-04-02") |
            # County == "Steuben" & date == as.Date("2020-04-10") |
            County == "Suffolk" & date == as.Date("2020-04-10") |
            County == "Sullivan" & date == as.Date("2020-04-12") |
            # County == "Tioga" & date == as.Date("2020-03-26") |
            # County == "Tompkins" & date == as.Date("2020-03-29") |
            County == "Ulster" & date == as.Date("2020-04-10") |
            # County == "Warren" & date == as.Date("2020-04-02") |
            # County == "Washington" & date == as.Date("2020-03-30") |
            # County == "Wayne" & date == as.Date("2020-04-02") |
            County == "Westchester" & date == as.Date("2020-04-10") 
            # County == "Wyoming" & date == as.Date("2020-04-10") 
            # County == "Yates" & date == as.Date("2020-04-12")
  )

# p.log.cases <- covid_NY_TS_plot.cases %>%
#   ggplot(aes(date, cases, color = Region, group=County)) +
#   geom_line() +
#   scale_y_continuous(
#     trans = "log10",
#     breaks = c(10,100,500,1000,5000,10000, 50000)
#   ) +
#   scale_x_datetime(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%b %d") +
#   ylab("Cumulative Number of Cases") + 
#   ggtitle("New York State COVID-19 Cases (Mar-Apr 2020)")  + 
#   geom_label_repel(data=highlight_points,  aes(label=County), segment.color="black", force=8) + 
#   NULL
# #library(plotly)
# p.log.cases
# #ggplotly(p.log.cases)

NY_region_palette.df <- NY_counties_regions %>%
  dplyr::select(Region,Color) %>% 
  dplyr::distinct(Region,Color)

NY_region_palette <- setNames(as.character(NY_region_palette.df$Color), as.character(NY_region_palette.df$Region))

p.case.rates <- covid_NY_TS_plot.cases %>%
  ggplot(aes(x=date, y=p_cases, color = Region, group=County)) +
  scale_color_manual(values=NY_region_palette) +
  geom_line(size=1) +
  scale_y_continuous(
    trans = "log10"
  # #   breaks = c(10,100,500,1000,5000,10000, 50000)
  ) +
  scale_x_datetime(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%b %d") +
  ylab("Cases per 100K Population") + 
  ggtitle("New York State COVID-19 Cases per 100K Population by County (Mar-Apr 2020)")  + 
  geom_label_repel(data=highlight_points,  aes(label=County), segment.color="black", force=8) + 
  geom_vline(aes(xintercept=as_datetime("2020-03-20"), linetype="Gov. Cuomo issues stay-at-home order"), color = "black") + 
  NULL

p.case.rates