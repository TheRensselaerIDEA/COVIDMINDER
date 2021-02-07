# COVIDMINDER Daily data downloader (6 of 7)
# SOURCE: Our World in Data: https://covid.ourworldindata.org/data/owid-covid-data.csv
# FILES UPDATED BY THIS SCRIPT:
# "data/csv/owid_testing_raw.csv"
# "data/csv/owid_covid_testing.csv"
# "data/csv/owid_covid_testing.csv.bak"
library(tidyverse)
library(sqldf) # Added 01/05/2021

# Download states raw data 
owidURL <- "https://covid.ourworldindata.org/data/owid-covid-data.csv"
download.file(owidURL, paste0("data/csv/", "owid_testing_raw.csv"))

# Import raw into R
# UPDATE: Only import USA data
# todays_raw_owid_data <- read_csv(paste0("data/csv/", "owid_testing_raw.csv"))
todays_raw_owid_data <- read.csv.sql("data/csv/owid_testing_raw.csv", 
             sql = "SELECT * FROM file WHERE iso_code = 'USA'", eol = "\n")

todays_raw_owid_data$date <- as.Date(todays_raw_owid_data$date)
todays_raw_owid_data$total_tests <- as.numeric(todays_raw_owid_data$total_tests)
todays_raw_owid_data$population <- as.numeric(todays_raw_owid_data$population)

# Check rates against our list (30 Apr)
check_iso_codes <- c("PRT","ITA","RUS","USA","GBR","CAN","CHE")
check_iso_codes2 <- c("USA","BEL","PRT","CHE","ITA","ESP","IRL","DEU","CAN","RUS","GBR")

# Grab latest reported total test rate for each country - Jose
owid_data.total.test.rate <- todays_raw_owid_data %>% 
  #filter(!is.na(total_tests_per_thousand) & iso_code %in% check_iso_codes2) %>%
  filter(!is.na(total_tests) & iso_code %in% check_iso_codes2) %>%
  # filter(total_tests > 1) %>%
  #select(iso_code, location, date, total_tests_per_thousand) %>%
  select(iso_code, location, date, total_tests, population) %>%
  group_by(iso_code) %>%
  mutate(total_tests_per_thousand = total_tests/population * 1000) %>%
  filter(date == max(date)) %>%
  top_n(n=1)

# write out new dataframe to file system 
write_csv(owid_data.total.test.rate,"data/csv/owid_glb_test_rates.csv")


# Obesity data - 2016

#raw_owid_data.ob <- read_csv(paste0("data/csv/", "share-of-adults-defined-as-obese.csv"))
#owid_data.obesity.rate <- raw_owid_data.ob %>%
#  filter(Code %in% check_iso_codes2) %>%
#  group_by(Code) %>%
#  filter(Year == max(Year)) %>%
#  top_n(n=1)
#write_csv(owid_data.obesity.rate,"data/csv/owid_obese_pct_2016.csv")


# Check in on Germany; WHY no testing rate?!?!
#todays_raw_owid_data %>% filter(iso_code == "DEU") %>%
#  select(iso_code, location, date, total_tests_per_thousand, new_tests_per_thousand) %>%
#  slice(which.max(as.Date(date, '%Y-%m-%d'))) 
  
# We only care about total tests and new tests
todays_raw_owid_data <- todays_raw_owid_data %>%
  select(iso_code, location, date, total_tests_per_thousand, new_tests_per_thousand) %>%
  drop_na(total_tests_per_thousand, new_tests_per_thousand) %>%
  group_by(iso_code) %>%
  slice(which.max(as.Date(date, '%Y-%m-%d'))) %>%
  arrange(desc(total_tests_per_thousand))

check_raw_owid_data <- todays_raw_owid_data %>%
  filter(iso_code %in% check_iso_codes)

# Make backup of existing data
write_csv(read_csv("data/csv/state_covid_testing.csv"),"data/csv/owid_covid_testing.csv.bak")

# write out new dataframe to file system 
write_csv(state_covid_testing,"data/csv/owid_covid_testing.csv")
