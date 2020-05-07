# COVIDMINDER Daily data downloader (7 of 7)
# Special to support Malik's COVID WarRoom
# Source: JHU Daily Reports (github): https://bit.ly/3dMWRP6
# FILES UPDATED BY THIS SCRIPT:
# "data/csv/time_series/time_series_covid19_deaths_US.csv"
# "data/csv/time_series/NYSByCountyCOVID.csv"
# "data/csv/time_series/NYSByCountyCOVID.csv.bak"
# "data/csv/time_series/CountyQuarItol.csv"
# "data/csv/time_series/CountyQuarItol.csv.bak"
library(tidyverse)
library(lubridate)
library(stringr)
library(stringi)
#install.packages("R.utils")
library(R.utils)

# curl newest TIME SERIES data from JHU github
# (You must edit the date below)
dateURL.1 <- "time_series_covid19_deaths_US.csv"
dateURL.2 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

# Write raw data to file system; use JHU syntax as above
download.file(paste0(dateURL.2,dateURL.1), paste0("data/csv/time_series/", dateURL.1))

# Import raw into R
todays_TS_data <- read_csv(paste0("data/csv/time_series/", dateURL.1))

# Transform to match our structure
# Structure should be: "County Name", "Population", date-labels columns

covid_TS_states_wr <- todays_TS_data %>%
  dplyr::filter(Country_Region == "US") %>%  # Start with USA; open up later
  dplyr::filter(!Province_State %in% c("Diamond Princess",
                                       "Grand Princess",
                                       "Northern Mariana Islands",
                                       "Virgin Islands",
                                       "American Samoa",
                                       "Guam",
                                       "Puerto Rico") ) %>%
#  select(-UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Country_Region, -Lat, -Long_, -Combined_Key) %>%
  dplyr::select(-UID, -iso2, -iso3, -code3, -FIPS, -Lat, -Long_, -Combined_Key) %>%
  dplyr::select(-Country_Region) %>%
  rename("County Name" = Admin2) 

covid_states <- unique(covid_TS_states_wr$Province_State)
covid_states <-  covid_states[!covid_states %in% c("New York")]
covid_states <- insert(covid_states, 1, c("New York"))

covid_TS_states_wr <- covid_TS_states_wr %>% 
  arrange(match(Province_State, covid_states)) %>%
  select(-Province_State)

# Rename columns
wr_colnames <- colnames(covid_TS_states_wr)
wr_colnames <- stri_replace_last_fixed(wr_colnames, "/20","")   

colnames(covid_TS_states_wr)<- wr_colnames

wr_colnames <-  wr_colnames[!wr_colnames %in% c("County Name","Population")]

wr_colnames <- rev(wr_colnames)
wr_colnames <- insert(wr_colnames, 1, c("Population"))
wr_colnames <- insert(wr_colnames, 1, c("County Name"))

covid_TS_states_wr <- covid_TS_states_wr[,wr_colnames]

# Make backup of existing data
write_csv(read_csv("data/csv/time_series/NYSByCountyCOVID.csv"),"data/csv/time_series/NYSByCountyCOVID.csv.bak")

# write out new dataframe to file system
write_csv(covid_TS_states_wr,"data/csv/time_series/NYSByCountyCOVID.csv")

# Now create the quarantine data
# Quarantine=0.05 if pop<50K, 0.1 if pop<100K,0.2 if pop<1M and 0.25 otherwise. 
#      These quarantine numbers can probably be lowered a little as it seems the world has gotten more conservative.
# Itol=50 if pop<50K, 100 if pop<100K,250 if pop<500K, 500 if pop<1.5M, 1000 if pop<3M and 500*ceil(pop/3000000) 
covid_quar_states_wr <- covid_TS_states_wr %>% 
  select(`County Name`,Population) %>% 
  mutate(Quarantine = case_when(
           Population <  50000  ~ 0.05,
           Population < 100000  ~ 0.10,
           Population < 1000000 ~ 0.20,
          TRUE                  ~  0.25
  )) %>%
  mutate(ITOL = case_when(
    Population <  50000  ~ 50,
    Population < 100000  ~ 100,
    Population < 500000  ~ 500,
    Population < 1500000 ~ 500,
    Population < 3000000 ~ 1000,
    TRUE                ~  500 * ceiling(Population/3000000)
  )) %>%
  rename("County" = "County Name") %>%
  rename("Pop" = "Population") 

covid_quar_states_wr <- covid_quar_states_wr[,c("County","Quarantine","ITOL","Pop")]

# Make backup of existing data
write_csv(read_csv("data/csv/time_series/CountyQuarItol.csv"),"data/csv/time_series/CountyQuarItol.csv.bak")

# write out new dataframe to file system
write_csv(covid_TS_states_wr,"data/csv/time_series/CountyQuarItol.csv")

