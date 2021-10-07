# COVIDMINDER Daily data downloader (8 of 8)
# UPDATED: 14 Sep 2021
# SOURCE: CDC Vaccinations: "https://data.cdc.gov/api/views/unsk-b7fc/rows.csv"
# FILES UPDATED BY THIS SCRIPT:
# "data/csv/states_cdc_vax_raw.csv"
# "data/csv/state_vaccinations.csv"
library(tidyverse)
library(lubridate)

states_abbreviations <- read_rds("data/csv/states_abbreviations.Rds")
state_population <- read_rds("data/csv/population.Rds")
  
# Download states raw data 
cdc_vax_URL <- "https://data.cdc.gov/api/views/unsk-b7fc/rows.csv"
download.file(cdc_vax_URL, paste0("data/csv/", "states_cdc_vax_raw.csv"))

# Import raw into R
todays_raw_vax_data <- read_csv(paste0("data/csv/", "states_cdc_vax_raw.csv"))

todays_raw_vax_data$Date <- mdy(todays_raw_vax_data$Date) # Convert to date

state_codes_to_exclude <- c("BP2", "IH2", "VA2", "DD2", "LTC", "MH", "MP", "VI", "GU", "PW", "RP", "FM", "AS", "PR")

# Select only the columns we need
# Filter by most recent date
# Exclude territories
todays_raw_vax_data <- todays_raw_vax_data %>% 
  select(Date, Location, Series_Complete_Pop_Pct ) %>%
  mutate(Vax_pct = Series_Complete_Pop_Pct) %>%
  filter(Date == max(Date)) %>%
  filter(!(Location %in% state_codes_to_exclude )) %>% 
  left_join(states_abbreviations, by=c("Location" = "Abbreviation")) %>%
  left_join(state_population, by=c("NAME" = "NAME")) %>%
  mutate(Vax_total = ((Vax_pct/100) * Population)) %>% 
  mutate(Vax_rate = Vax_pct * 1000) %>% 
  select(-c(Rank, Series_Complete_Pop_Pct)) # %>% mutate(vax_per_1000 = Vax_rate/100)

# Move United States to top
matched <- todays_raw_vax_data$NAME %in% c("United States")
todays_raw_vax_data <- rbind(todays_raw_vax_data[matched,], todays_raw_vax_data[!matched,])

# Make backup of existing data
write_csv(read_csv("data/csv/state_vaccinations.csv"),"data/csv/state_vaccinations.csv.bak")

# write out new dataframe to file system 
write_csv(todays_raw_vax_data,"data/csv/state_vaccinations.csv")
