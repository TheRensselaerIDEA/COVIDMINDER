# Daily data importer
# Source: JHU Daily Reports (github): https://bit.ly/3dMWRP6
library(tidyverse)
library(lubridate)
library(stringr)

# curl newest TIME SERIES data from JHU github
# (You must edit the date below)
dateURL.1 <- "time_series_covid19_confirmed_US.csv"
dateURL.2 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

# Write raw data to file system; use JHU syntax as above
download.file(paste0(dateURL.2,dateURL.1), paste0("data/csv/time_series/", dateURL.1))

# Import raw into R
todays_TS_data <- read_csv(paste0("data/csv/time_series/", dateURL.1))

# Transform to match our structure
covid_TS_states <- todays_TS_data %>%
  filter(Country_Region == "US") %>%
  filter(!Province_State %in% c("Diamond Princess","Grand Princess","Northern Mariana Islands","Virgin Islands") ) %>%
#  select(-UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Country_Region, -Lat, -Long_, -Combined_Key) %>%
  select(-UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Lat, -Long_, -Combined_Key) %>%
  group_by(Province_State,Country_Region) %>%
  summarize_all(sum)

# Change colnames to match app
colnames(covid_TS_states)[1] <- "NAME"

# Create a "United States" row
covid_TS_united_states <- covid_TS_states[,-1] %>%
  group_by(Country_Region) %>%
  summarize_all(sum)

colnames(covid_TS_united_states)[1] <- 'NAME'
covid_TS_united_states[1,1] <- "United States"

# Prepend United States summary to states summary
# THIS IS "WIDE"!
covid_TS_states <- data.frame(rbind(covid_TS_united_states, covid_TS_states[,-2]))

# Make backup of existing WIDE data
write_csv(read_csv("data/csv/time_series/covid_TS_states_wide.csv"),"data/csv/time_series/covid_TS_states_wide.csv.bak")

# write out new WIDE dataframe to file system
write_csv(covid_TS_states,"data/csv/time_series/covid_TS_states_wide.csv")

# NOW "gather" to create "LONG" version
covid_TS_states_long <- covid_TS_states %>%
  gather(date,cases,2:72)

# Make date column an actual R date_time
covid_TS_states_long$date <- str_sub(covid_TS_states_long$date, 2,-1)
covid_TS_states_long$date <- parse_date_time(covid_TS_states_long$date, c("%m.%d.%y"))

# Make backup of existing LONG data
write_csv(read_csv("data/csv/time_series/covid_TS_states_long.csv"),"data/csv/time_series/covid_TS_states_long.csv.bak")

# write out new LONG dataframe to file system
write_csv(covid_TS_states_long,"data/csv/time_series/covid_TS_states_long.csv")


#### Quickie plot to verify
covid_TS_plot <- covid_TS_states_long %>%
  group_by(date)

covid_TS_plot$log_cases <- log10(covid_TS_plot$cases)

p <- covid_TS_plot %>% 
  mutate(
    State = factor(NAME),     # use year to define separate curves
    Date = update(date, year = 1)  # use a constant year for the x-axis
  ) %>% 
  ggplot(aes(Date, log_cases, color = State)) +
  geom_line() +
  ggtitle("COVID-19 Confirmed Cases (log10 scale) (Jan - Apr 2020)")

p
