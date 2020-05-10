# COVIDMINDER Daily data downloader (6 of 7)
# SOURCE: Our World in Data: https://covid.ourworldindata.org/data/owid-covid-data.csv
# FILES UPDATED BY THIS SCRIPT:
# "data/csv/owid_testing_raw.csv"
# "data/csv/owid_covid_testing.csv"
# "data/csv/owid_covid_testing.csv.bak"
library(tidyverse)

# Download states raw data 
owidURL <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
download.file(owidURL, paste0("data/csv/", "owid_testing_raw.csv"))

# Import raw into R
todays_raw_owid_data <- read_csv(paste0("data/csv/", "owid_testing_raw.csv"))

# Check in on Germany; WHY no testing rate?!?!
todays_raw_owid_data %>% filter(iso_code == "DEU") %>%
  select(iso_code, location, date, total_tests_per_thousand, new_tests_per_thousand) %>%
  slice(which.max(as.Date(date, '%Y-%m-%d'))) 
  
# We only care about total tests and new tests
todays_raw_owid_data <- todays_raw_owid_data %>%
  select(iso_code, location, date, total_tests_per_thousand, new_tests_per_thousand) %>%
  drop_na(total_tests_per_thousand, new_tests_per_thousand) %>%
  group_by(iso_code) %>%
  slice(which.max(as.Date(date, '%Y-%m-%d'))) %>%
  arrange(desc(total_tests_per_thousand))

# Check rates against our list (30 Apr)
check_iso_codes <- c("PRT","ITA","RUS","USA","GBR","CAN","CHE")

check_raw_owid_data <- todays_raw_owid_data %>%
  filter(iso_code %in% check_iso_codes)

# Make backup of existing data
write_csv(read_csv("data/csv/state_covid_testing.csv"),"data/csv/owid_covid_testing.csv.bak")

# write out new dataframe to file system 
write_csv(state_covid_testing,"data/csv/owid_covid_testing.csv")
