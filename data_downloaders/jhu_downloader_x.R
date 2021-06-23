# COVIDMINDER Daily data downloader (2 of 7)
# Source: JHU Daily Reports (github): https://bit.ly/3dMWRP6
# FILES CREATED/UPDATED BY THIS SCRIPT:
# "data/csv/04-XX-2020.csv"  (where 'XX' is the date two days ago)
# "data/csv/covid_data_states.csv" - Stopped June 12th 2020 (Jose)
# "data/csv/covid_data_states.csv.bak" - Stopped June 12th (Jose)
# library(tidyverse)

# curl newest data from JHU github
# UPDATE: Default download is *yesterday* (current day - 1)
# dateURL.1 <- paste0(toString(format(as.Date(Sys.time())-2, "%m-%d-%Y")),".csv")
#dateURL.1 <- paste0("07-05-2020",".csv")
dateURL.1 <- paste0("12-28-2020",".csv")
dateURL.1 <- paste0(toString(format(as.Date(Sys.time())-2, "%m-%d-%Y")),".csv")

dateURL.2 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"

# Write raw data to file system; use date-based name syntax as above
download.file(paste0(dateURL.2,dateURL.1), paste0("data/csv/", dateURL.1))

