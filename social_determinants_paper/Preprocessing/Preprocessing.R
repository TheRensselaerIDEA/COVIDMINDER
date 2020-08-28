knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = "../")

source("./Social_Determinants/Modules/Source.R")

# Change the date by hand
date_of_study = "07-06-2020"

# Parallel
# args <- commandArgs()
# date = args[6]
# date_of_study = paste(date,"-2020",sep="")

column_names <- data.frame()

#------------------------------------------------------------------------------------------------------------------------------------------

# Import exposure PM2.5 data
county_pm <- read.csv("./Social_Determinants/Data/county_pm25.csv")
county_pm$fips = str_pad(county_pm$fips, 5, pad = "0")

# pm2.5 average over 17 years
county_pm_aggregated <- county_pm %>% 
  filter(!is.na(pm25)) %>%
  filter(!is.na(fips)) %>%
  group_by(fips) %>% 
  dplyr::summarise(mean_pm25 = mean(pm25))

county_pm_aggregated_names <- data.frame(column = names(county_pm_aggregated)[2:ncol(county_pm_aggregated)])
county_pm_aggregated_names$source <- "county_pm_aggregated"
column_names <- rbind(column_names, county_pm_aggregated_names)

# temperature and relative humidity average over 17 years
county_temp = read.csv("./Social_Determinants/Data/temp_seasonal_county.csv")
county_temp$fips = str_pad(county_temp$fips, 5, pad = "0")

county_temp_aggregated = county_temp %>% 
  group_by(fips) %>% 
  dplyr::summarise(mean_winter_temp= mean(winter_tmmx, na.rm=TRUE),
                   mean_summer_temp= mean(summer_tmmx, na.rm=TRUE),
                   mean_winter_rm= mean(winter_rmax, na.rm=TRUE),
                   mean_summer_rm= mean(summer_rmax, na.rm=TRUE))

county_temp_aggregated_names <- data.frame(column = names(county_temp_aggregated)[2:ncol(county_temp_aggregated)])
county_temp_aggregated_names$source <- "county_temp_aggregated"
column_names <- rbind(column_names, county_temp_aggregated_names)

aggregate_pm_temp = merge(county_pm_aggregated,county_temp_aggregated,by="fips",all.x = T)

#------------------------------------------------------------------------------------------------------------------------------------------

# Historical data
covid_hist = read.csv(text=getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-30-2020.csv")))
covid_us_hist = subset(covid_hist, Country_Region == "US" & is.na(FIPS)==F)
#covid_us_hist = subset(covid_us_hist, select = -c(Country_Region, Last_Update, Combined_Key))

# Import outcome data from JHU CSSE
covid = read.csv(text=getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",date_of_study,".csv")))
covid_us = subset(covid,Country_Region == "US")
covid_us <- rbind.fill(covid_us,subset(covid_us_hist, (!(FIPS %in% covid_us$FIPS))  & Confirmed == 0 & Deaths == 0 & is.na(FIPS)==F))
covid_us$FIPS = str_pad(covid_us$FIPS, 5, pad = "0")
covid_us = subset(covid_us, select = -c(Admin2, Province_State, Country_Region, Last_Update, Combined_Key))

covid_us_names <- data.frame(column = names(covid_us)[2:ncol(covid_us)])
covid_us_names$source <- "covid_us"
column_names <- rbind(column_names, covid_us_names)

aggregate_pm_temp_covid = merge(aggregate_pm_temp,covid_us,by.x="fips",by.y = "FIPS")

#------------------------------------------------------------------------------------------------------------------------------------------

# Import census

county_census <- read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/census_county_interpolated.csv"))
county_census <- subset(county_census, select = -c(X))

county_census_aggregated = subset(county_census, year==2016)
county_census_aggregated <- county_census_aggregated[,c(1,3,4,5,7,8,10,11,6,9,12,13,14)]

county_census_aggregated$q_popdensity = 1
quantile_popdensity = quantile(county_census_aggregated$popdensity,c(0.2,0.4,0.6,0.8))
county_census_aggregated$q_popdensity[county_census_aggregated$popdensity<=quantile_popdensity[1]] = 1
county_census_aggregated$q_popdensity[county_census_aggregated$popdensity>quantile_popdensity[1] &
                                         county_census_aggregated$popdensity<=quantile_popdensity[2]] = 2
county_census_aggregated$q_popdensity[county_census_aggregated$popdensity>quantile_popdensity[2] &
                                         county_census_aggregated$popdensity<=quantile_popdensity[3]] = 3
county_census_aggregated$q_popdensity[county_census_aggregated$popdensity>quantile_popdensity[3] &
                                         county_census_aggregated$popdensity<=quantile_popdensity[4]] = 4
county_census_aggregated$q_popdensity[county_census_aggregated$popdensity>quantile_popdensity[4]] = 5

county_census_aggregated$fips = str_pad(county_census_aggregated$fips, 5, pad = "0")

county_census_aggregated_names <- data.frame(column = names(county_census_aggregated)[2:ncol(county_census_aggregated)])
county_census_aggregated_names$source <- "county_census_aggregated"
column_names <- rbind(column_names, county_census_aggregated_names)

aggregate_pm_temp_covid_census = merge(aggregate_pm_temp_covid,county_census_aggregated,by.x="fips",by.y = "fips")

#------------------------------------------------------------------------------------------------------------------------------------------

county_base_mortality = read.table("./Social_Determinants/Data/county_base_mortality.txt", header = TRUE)
county_old_mortality = read.table("./Social_Determinants/Data/county_old_mortality.txt", header = TRUE)
county_014_mortality = read.table("./Social_Determinants/Data/county_014_mortality.txt", header = TRUE)
county_1544_mortality = read.table("./Social_Determinants/Data/county_1544_mortality.txt", header = TRUE)
county_4564_mortality = read.table("./Social_Determinants/Data/county_4564_mortality.txt", header = TRUE)

# county base mortality (covid mortality?)
colnames(county_old_mortality)[4] = c("older_Population")
colnames(county_014_mortality)[4] = c("014_Population")
colnames(county_1544_mortality)[4] = c("1544_Population")
colnames(county_4564_mortality)[4] = c("4564_Population")

county_base_mortality = merge(county_base_mortality,county_old_mortality[,c(2,4)] ,by = "County.Code", all.x = T)
county_base_mortality = merge(county_base_mortality,county_014_mortality[,c(2,4)] ,by = "County.Code",all.x = T)
county_base_mortality = merge(county_base_mortality,county_1544_mortality[,c(2,4)] ,by = "County.Code",all.x = T)
county_base_mortality = merge(county_base_mortality,county_4564_mortality[,c(2,4)] ,by = "County.Code",all.x = T)

county_base_mortality$older_pecent = county_base_mortality$older_Population/county_base_mortality$Population
county_base_mortality$"young_pecent" = county_base_mortality$"014_Population"/county_base_mortality$Population
county_base_mortality$"prime_pecent" = county_base_mortality$"1544_Population"/county_base_mortality$Population
county_base_mortality$"mid_pecent" = county_base_mortality$"4564_Population"/county_base_mortality$Population
county_base_mortality$"older_pecent"[is.na(county_base_mortality$"older_pecent")] = 0
county_base_mortality$"prime_pecent"[is.na(county_base_mortality$"prime_pecent")] = 0
county_base_mortality$"mid_pecent"[is.na(county_base_mortality$"mid_pecent")] = 0
county_base_mortality$"young_pecent"[is.na(county_base_mortality$"young_pecent")] = 0

county_base_mortality$County.Code = str_pad(county_base_mortality$County.Code, 5, pad = "0")
county_base_mortality <- county_base_mortality[,c(1,12:15)]

county_base_mortality_names <- data.frame(column = names(county_base_mortality)[2:ncol(county_base_mortality)])
county_base_mortality_names$source <- "county_base_mortality"
column_names <- rbind(column_names, county_base_mortality_names)

aggregate_pm_temp_covid_census_mortality = merge(aggregate_pm_temp_covid_census,county_base_mortality,by.x = "fips",by.y = "County.Code",all.x = T)

#------------------------------------------------------------------------------------------------------------------------------------------

# County Health Rankings Master dataset

chr <- read_csv("./Social_Determinants/Data/2020CHR.csv")
chr <- chr[, -grep("Quartile", colnames(chr))]
chr <- chr[, -grep("95", colnames(chr))]
chr <- chr[, -grep("\\(AIAN\\)", colnames(chr))]
chr <- chr[, -grep("\\(Asian\\)", colnames(chr))]
chr <- chr[, -grep("\\(Black\\)", colnames(chr))]
chr <- chr[, -grep("\\(Hispanic\\)", colnames(chr))]
chr <- chr[, -grep("\\(White\\)", colnames(chr))]
chr <- subset(chr, select = -c(Deaths, Unreliable, Population))
chr <- dplyr::rename(chr, c("pct_diabetes" = `% Adults with Diabetes`,
                            "pct_obesity" = `% Adults with Obesity`, 
                            "pct_age65" = `% 65 and over`, 
                            "pre_covid_deaths" = `# Deaths`, 
                            "pre_covid_death_rate" = `Age-Adjusted Death Rate`, 
                            "child_deaths" = `# Deaths_1`, 
                            "infant_deaths" = `# Deaths_2`,
                            "# less than 18 years of age" = `Population_1`, 
                            "suicide_deaths"= `# Deaths_3`))

chr_names <- data.frame(column = names(chr)[2:ncol(chr)])
chr_names$source <- "chr"
column_names <- rbind(column_names, chr_names)

#------------------------------------------------------------------------------------------------------------------------------------------

# State test and policy
state_test = read.csv(url("https://covidtracking.com/api/v1/states/daily.csv"))
state_test = subset(state_test, date ==paste0(substring(str_remove_all(date_of_study, "-"),5,8),substring(str_remove_all(date_of_study, "-"),1,4)))[,-20]
state_test <- subset(state_test, select = -c(date, lastUpdateEt, dateModified, checkTimeEt, dateChecked, hash, grade))
state_test <- state_test[, -grep("Score", colnames(state_test))]

statecode = read_csv("./Social_Determinants/Data/statecode.csv")

# Import social distancing measure data
distancing <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=1894978869")
colnames(distancing)[2] = "state"
colnames(distancing)[4] = "stay_at_home"

reopen <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=1269444822')
colnames(reopen)[2] = "state"
colnames(reopen)[4] = "reopen"

closure <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=257109301')
colnames(closure)[2] = "state"
colnames(closure)[4] = "reclosure"

mask <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=1489353670')
colnames(mask)[2] = "state"
colnames(mask)[4] = "mask"

# merging data
state_test = merge(state_test,statecode[,c(1,3)],by.x = "state" ,by.y = "Code" )
state_test = merge(state_test,distancing[,c(2,4)],by = "state")
state_test = merge(state_test,reopen[,c(2,4)],by = "state")
state_test = merge(state_test,closure[,c(2,4)],by = "state")
state_test = merge(state_test,mask[,c(2,4)],by = "state")

#state_test$date_since_social = as.numeric(as.Date(Sys.Date()) - as.Date((strptime(state_test$stay_at_home, "%m/%d/%Y"))))
state_test$date_since_social = as.numeric(as.Date(strptime(date_of_study, "%m-%d-%Y")) - as.Date((strptime(state_test$stay_at_home, "%m/%d/%Y"))))
state_test[is.na(state_test$date_since_social)==T,]$date_since_social = 0
state_test$date_since_social[state_test$date_since_social < 0] = 0

state_test$date_since_reopen = as.numeric(as.Date(strptime(date_of_study, "%m-%d-%Y")) - as.Date((strptime(state_test$reopen, "%m/%d/%Y"))))
state_test[is.na(state_test$date_since_reopen)==T,]$date_since_reopen = 0
state_test$date_since_reopen[state_test$date_since_reopen < 0] = 0

state_test$date_since_reclosure = as.numeric(as.Date(strptime(date_of_study, "%m-%d-%Y")) - as.Date((strptime(state_test$reclosure, "%m/%d/%Y"))))
state_test[is.na(state_test$date_since_reclosure)==T,]$date_since_reclosure = 0
state_test$date_since_reclosure[state_test$date_since_reclosure < 0] = 0

state_test$date_since_mask = as.numeric(as.Date(strptime(date_of_study, "%m-%d-%Y")) - as.Date((strptime(state_test$mask, "%m/%d/%Y"))))
state_test$date_since_mask[is.na(state_test$date_since_mask)==T] = 0
state_test$date_since_mask[state_test$date_since_mask < 0] = 0
state_test <- subset(state_test, select = -c(dataQualityGrade, fips, score))
state_test <- dplyr::rename(state_test, c(state_deaths = death))

state_test_names <- data.frame(column = names(state_test)[1:ncol(state_test)])
state_test_names$source <- "state_test"
state_test_names <- state_test_names[-c(39), ]
column_names <- rbind(column_names, state_test_names)

aggregate_chr_policy = merge(chr,state_test,by="State")

aggregate_pm_temp_covid_census_mortality_chr_policy = merge(aggregate_pm_temp_covid_census_mortality, aggregate_chr_policy, by.x = "fips", by.y = "FIPS")

#------------------------------------------------------------------------------------------------------------------------------------------

# Hospitals
hospitals = read.csv(text=getURL("https://opendata.arcgis.com/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"))
hospitals$BEDS[hospitals$BEDS < 0] = NA

county_hospitals_aggregated = hospitals %>%
  group_by(COUNTYFIPS) %>%
  dplyr::summarise(beds = sum(BEDS, na.rm=TRUE))
county_hospitals_aggregated$COUNTYFIPS = str_pad(county_hospitals_aggregated$COUNTYFIPS, 5, pad = "0")

county_hospitals_aggregated_names <- data.frame(column = names(county_hospitals_aggregated)[2:ncol(county_hospitals_aggregated)])
county_hospitals_aggregated_names$source <- "county_hospitals_aggregated"
column_names <- rbind(column_names, county_hospitals_aggregated_names)

aggregate_pm_temp_covid_census_mortality_chr_policy_beds = merge(aggregate_pm_temp_covid_census_mortality_chr_policy,county_hospitals_aggregated,by.x = "fips",by.y = "COUNTYFIPS",all.x = T)
aggregate_pm_temp_covid_census_mortality_chr_policy_beds$beds[is.na(aggregate_pm_temp_covid_census_mortality_chr_policy_beds$beds)] = 0

#------------------------------------------------------------------------------------------------------------------------------------------

# Import outcome data from JHU CSSE, calculate the timing of the 1st confirmed case for each county
date_of_all = format(seq(as.Date("2020-03-22"), as.Date(strptime(date_of_study,"%m-%d-%Y")), by = "days"),"%m-%d-%Y")
covid_us_daily_confirmed = lapply(date_of_all,
                                  function(date_of_all){
                                    covid_daily = read.csv(text=getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",date_of_all,".csv")))
                                    covid_daily = covid_daily[!duplicated(covid_daily$FIPS),]
                                    covid_daily = subset(covid_daily,Country_Region == "US" & is.na(FIPS)!=T & Confirmed >0)
                                    covid_daily = subset(covid_daily,select=c(FIPS))
                                    covid_daily$date_since = as.numeric(as.Date(strptime(date_of_study,"%m-%d-%Y")) - as.Date(strptime(date_of_all, "%m-%d-%Y")))
                                    return(covid_daily)
                                  }
)

covid_county_confirmed <- covid_us_daily_confirmed[[1]]
for (i in 1:length(date_of_all)){
  covid_county_confirmed <- rbind.fill(covid_county_confirmed, subset(covid_us_daily_confirmed[[i]], (!(FIPS %in% covid_county_confirmed$FIPS))))
}

covid_county_confirmed$FIPS <- str_pad(covid_county_confirmed$FIPS, 5, pad = "0")

covid_county_confirmed_names <- data.frame(column = names(covid_county_confirmed)[2:ncol(covid_county_confirmed)])
covid_county_confirmed_names$source <- "covid_county_confirmed"
column_names <- rbind(column_names, covid_county_confirmed_names)

aggregate_pm_temp_covid_census_mortality_chr_policy_beds_first = merge(aggregate_pm_temp_covid_census_mortality_chr_policy_beds,covid_county_confirmed,
                                                                       by.x = "fips",by.y = "FIPS", all.x = T)
aggregate_pm_temp_covid_census_mortality_chr_policy_beds_first$date_since[is.na(aggregate_pm_temp_covid_census_mortality_chr_policy_beds_first$date_since)] = 0

#------------------------------------------------------------------------------------------------------------------------------------------

# CDC

cdc <- readRDS('./Social_Determinants/MM_data/CDC/cdc.data.imputed.Rds')
cdc <- subset(cdc, period == '2015-2017')
cdc <- data.frame(split(cdc, cdc$death_cause))
cdc <- subset(cdc, select = c(All.Cause.county_fips, 
                              All.Cause.death_rate, 
                              Assault.death_rate, 
                              Cancer.death_rate, 
                              Cardiovascular.death_rate, 
                              Despair.death_rate))
cdc <- dplyr::rename(cdc, c(FIPS = All.Cause.county_fips))

cdc_names <- data.frame(column = names(cdc)[2:ncol(cdc)])
cdc_names$source <- "cdc"
column_names <- rbind(column_names, cdc_names)

aggregate_pm_temp_covid_census_mortality_chr_policy_beds_first_cdc = merge(aggregate_pm_temp_covid_census_mortality_chr_policy_beds_first,cdc,
                                                                           by.x = "fips",by.y = "FIPS", all.x = T)

#------------------------------------------------------------------------------------------------------------------------------------------

# Lung Disease dataset

lungdisease <- read_csv("./Social_Determinants/Data/lungdiseaseestimates_uscounties.csv")

lungdisease$PediatricAsthma         <- lungdisease$PediatricAsthma / lungdisease$TotalPopulation  * 10^5
lungdisease$AdultAsthma             <- lungdisease$AdultAsthma / lungdisease$TotalPopulation  * 10^5
lungdisease$COPD                    <- lungdisease$COPD / lungdisease$TotalPopulation  * 10^5
lungdisease$AdultChronicLungDisease <- lungdisease$AdultChronicLungDisease / lungdisease$TotalPopulation  * 10^5
lungdisease$LungCancer              <- lungdisease$LungCancer / lungdisease$TotalPopulation  * 10^5

lungdisease <- subset(lungdisease, select = -c(`State or County`, TotalPopulation))

lungdisease_names <- data.frame(column = names(lungdisease)[2:ncol(lungdisease)])
lungdisease_names$source <- "lungdisease"
column_names <- rbind(column_names, lungdisease_names)

aggregate_data = merge(aggregate_pm_temp_covid_census_mortality_chr_policy_beds_first_cdc, lungdisease, 
                       by.x = "fips",by.y = "FIPS", all.x = T)

#------------------------------------------------------------------------------------------------------------------------------------------

# Import FB survey on covid-like sympton data
script <- getURL("https://raw.githubusercontent.com/cmu-delphi/delphi-epidata/main/src/client/delphi_epidata.R", ssl.verifypeer = FALSE)
eval(parse(text = script))

# Request FB survey data from CMU COVIDcast Delphi Research Group
aggregate_data$cli  = 
  sapply(aggregate_data$fips, 
         function(fips){
           if (Epidata$covidcast('fb-survey', 'smoothed_cli', 'day', 'county', list(Epidata$range(20200401, paste0(substring(str_remove_all(date_of_study, "-"),5,8),substring(str_remove_all(date_of_study, "-"),1,4)))),fips)[[2]]!="no results"){
             return(mean(sapply(Epidata$covidcast('fb-survey', 'smoothed_cli', 'day', 'county', list(Epidata$range(20200401, paste0(substring(str_remove_all(date_of_study, "-"),5,8),substring(str_remove_all(date_of_study, "-"),1,4)))),fips)[[2]],function(i){i$value}),na.rm=T))
           }else {return(NA)}})

FB_names <- data.frame(column = c("cli"))
FB_names$source <- "FB"
column_names <- rbind(column_names, FB_names)

#------------------------------------------------------------------------------------------------------------------------------------------

# Rural/Urban code

NCHSURCodes2013 <- read_csv("./Social_Determinants/Data/NCHSURCodes2013.csv")
NCHSURCodes2013 <- subset(NCHSURCodes2013, select = c(FIPS, `2013 code`))

NCHSURCodes2013_names <- data.frame(column = names(NCHSURCodes2013)[2:2])
NCHSURCodes2013_names$source <- "NCHSURCodes2013"
column_names <- rbind(column_names, NCHSURCodes2013_names)

aggregate_data = merge(aggregate_data, NCHSURCodes2013, by.x = "fips",by.y = "FIPS", all.x = T)

#------------------------------------------------------------------------------------------------------------------------------------------

# Saving data frame to Rds file
file = paste("./Social_Determinants/Preprocessing_FTS_Outputs/", date_of_study, "data.Rds",sep = "")
saveRDS(aggregate_data, file)

saveRDS(column_names, "./Social_Determinants/Preprocessing/column_names.Rds")

#------------------------------------------------------------------------------------------------------------------------------------------
# Combine five boroughs of NYC

# aggregatedata[aggregatedata$Admin2=="New York City",]$population =
#   subset(aggregatedata,Admin2=="New York City"& Province_State=="New York")$population +
#   subset(aggregatedata, Admin2=="Bronx"& Province_State=="New York")$population +
#   subset(aggregatedata, Admin2=="Kings"& Province_State=="New York")$population +
#   subset(aggregatedata, Admin2=="Queens"& Province_State=="New York")$population +
#   subset(aggregatedata, Admin2=="Richmond"& Province_State=="New York")$population
# aggregatedata[aggregatedata$Admin2=="New York City",]$beds =
#   subset(aggregatedata,Admin2=="New York City"& Province_State=="New York")$beds +
#   subset(aggregatedata, Admin2=="Bronx"& Province_State=="New York")$beds +
#   subset(aggregatedata, Admin2=="Kings"& Province_State=="New York")$beds +
#   subset(aggregatedata, Admin2=="Queens"& Province_State=="New York")$beds +
#   subset(aggregatedata, Admin2=="Richmond"& Province_State=="New York")$beds
# 
# vars <- unlist(lapply(aggregate_data, is.numeric))
# 
# aggregatedata[aggregatedata$Admin2=="New York City",][,vars] = 
#   sapply(vars,function(var){
#     (subset(aggregatedata,Admin2=="New York City"& Province_State=="New York")[,var] * subset(aggregatedata,Admin2=="New York City"& Province_State=="New York")$population +
#        subset(aggregatedata, Admin2=="Bronx"& Province_State=="New York")[,var]        * subset(aggregatedata,Admin2=="Bronx"& Province_State=="New York")$population +
#        subset(aggregatedata, Admin2=="Kings"& Province_State=="New York")[,var]        * subset(aggregatedata,Admin2=="Kings"& Province_State=="New York")$population +
#        subset(aggregatedata, Admin2=="Queens"& Province_State=="New York")[,var]       * subset(aggregatedata,Admin2=="Queens"& Province_State=="New York")$population +
#        subset(aggregatedata, Admin2=="Richmond"& Province_State=="New York")[,var]     * subset(aggregatedata,Admin2=="Richmond"& Province_State=="New York")$population)/(
#          subset(aggregatedata,Admin2=="New York City"& Province_State=="New York")$population+subset(aggregatedata,Admin2=="Bronx"& Province_State=="New York")$population+
#            subset(aggregatedata, Admin2=="Kings"& Province_State=="New York")$population+ subset(aggregatedata,Admin2=="Queens"& Province_State=="New York")$population +
#            subset(aggregatedata,Admin2=="Richmond"& Province_State=="New York")$population)
#   })
# aggregatedata = subset(aggregatedata, !(Admin2=="Bronx"& Province_State=="New York")&
#                          !(Admin2=="Kings"& Province_State=="New York")&
#                          !(Admin2=="Queens"& Province_State=="New York")&
#                          !(Admin2=="Richmond"& Province_State=="New York"))

#------------------------------------------------------------------------------------------------------------------------------------------

# Import NCHS Urban-Rural Classification Scheme for Counties
# NCHSURCodes2013 = read_csv("./Social_Determinants/Data/NCHSURCodes2013.csv")
# NCHSURCodes2013$FIPS = str_pad(NCHSURCodes2013$FIPS, 5, pad = "0")

# aggregate_pm_census_cdc_test_beds = merge(aggregate_pm_census_cdc_test_beds,NCHSURCodes2013[,c(1,7)],
#                                           by.x = "fips",by.y="FIPS", all.x = T)

# BRFSS
# county_brfss<-read.csv("./Social_Determinants/Data/analytic_data2020.csv")
# county_brfss <- county_brfss[-c(1), ]
# county_brfss <- subset(county_brfss, select = -c(State.FIPS.Code, County.FIPS.Code, State.Abbreviation, Name, Release.Year, County.Ranked..Yes.1.No.0.))
# county_brfss <- dplyr::rename(county_brfss, c(fips = X5.digit.FIPS.Code))
# county_brfss$fips = str_pad(county_brfss$fips, 5, pad = "0")
# county_brfss <- county_brfss[, -grep("CI", colnames(county_brfss))]
# county_brfss <- county_brfss[, -grep("nreliable", colnames(county_brfss))]
# county_brfss <- county_brfss[, -grep("numerator", colnames(county_brfss))]
# county_brfss <- county_brfss[, -grep("denominator", colnames(county_brfss))]
# county_brfss_names <- data.frame(name = names(county_brfss)[2:223])
# county_brfss_names$source <- "county_brfss"
