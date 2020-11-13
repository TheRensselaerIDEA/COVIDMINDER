# COVIDMINDER Daily data downloader (5 of 7)
# UPDATED: 30 April 2020
# SOURCE: CDC Racial: "https://data.cdc.gov/resource/pj7m-y5uh.csv"
# FILES UPDATED BY THIS SCRIPT:
# "data/csv/states_cdc_racial_raw.csv"
# "data/csv/states_cdc_racial_wide.csv"
# "data/csv/states_cdc_racial_wide.csv.bak"
library(tidyverse)

# Download states raw data 
cdc_race_URL <- "https://data.cdc.gov/resource/pj7m-y5uh.csv"
download.file(cdc_race_URL, paste0("data/csv/", "states_cdc_racial_raw.csv"))

# Import raw into R
#todays_raw_racial_data.bak <- todays_raw_racial_data 
todays_raw_racial_data <- read_csv(paste0("data/csv/", "states_cdc_racial_raw.csv"))

# Rename raw columns
todays_raw_racial_data <- todays_raw_racial_data %>% 
  rename(NAME = state) %>%
  rename(nhw = non_hispanic_white) %>%
  rename(nhbaa = non_hispanic_black_african_american) %>% 
  rename(nhaian = non_hispanic_american_indian_alaska_native) %>% 
  rename(nhapi = non_hispanic_asian_pacific_islander) %>%
  rename(hlt = hispanic_latino_total) %>%
  select(-footnote)

# Rewrite indicator column
# UPDATED: They changed an indicator name!!
todays_raw_racial_data <- todays_raw_racial_data %>%
  mutate(indicator = replace(indicator, indicator == "Distribution of COVID-19 deaths (%)", "deaths_pct")) %>%
  mutate(indicator = replace(indicator, indicator == "Weighted distribution of population (%)", "wd_pop_pct")) %>%
  mutate(indicator = replace(indicator, indicator == "Unweighted distribution of population (%)", "un_pop_pct")) %>%
  mutate(indicator = replace(indicator, indicator == "Count of COVID-19 deaths", "deaths_num")) 

# Transform to wide
todays_raw_racial_data.wide <- todays_raw_racial_data %>%
  #pivot_wider(names_from = indicator, values_from = 6:11)
  pivot_wider(id_cols = c(data_as_of,start_week,end_week,NAME), names_from = indicator, values_from = c(nhw, nhbaa, nhaian,  nhapi,     hlt,  other))

# Make backup of existing data
yesterday_data <- read_csv("data/csv/states_cdc_racial_wide.csv")
write_csv(yesterday_data,"data/csv/states_cdc_racial_wide.csv.bak")
# write out new dataframe to file system 
write_csv(todays_raw_racial_data.wide,"data/csv/states_cdc_racial_wide.csv")

write_csv(todays_raw_racial_data.wide,"data/csv/states_cdc_racial_wide.csv.bak")
  