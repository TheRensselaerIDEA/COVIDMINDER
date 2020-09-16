knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = "../")

source("./Modules/Source.R")

# Change the date here
date_of_study = "06-14-2020"

# MortalityMinder datesets
#------------------------------------------------------------------------------------------------------------------------------------------

# CDC

# Used columns
#-------------------------------------------------------------------------------------------------
#| All.Cause.county_fips         | All.Cause.death_rate          | Assault.death_rate            |
#-------------------------------------------------------------------------------------------------
#| Cancer.death_rate             | Cardiovascular.death_rate     | Despair.death_rate            |
#-------------------------------------------------------------------------------------------------

# Unused columns
#-------------------------------------------------------------------------------------------------
#| All.Cause.urban_2013          | All.Cause.population          | All.Cause.death_num           |
#-------------------------------------------------------------------------------------------------
#| Assault.death_num             | Cancer.death_num              | Cardiovascular.death_num      |
#-------------------------------------------------------------------------------------------------
#| Despair.death_num             |
#---------------------------------


cdc <- readRDS('MM_data/data/CDC/cdc.data.imputed.Rds')

cdc <- subset(cdc, period == '2015-2017')

cdc <- data.frame(split(cdc, cdc$death_cause))

cdc <- subset(cdc, select = c(All.Cause.county_fips, 
                              All.Cause.death_rate, 
                              Assault.death_rate, 
                              Cancer.death_rate, 
                              Cardiovascular.death_rate, 
                              Despair.death_rate))

cdc <- rename(cdc, c(FIPS = All.Cause.county_fips))

# Social Determinants datesets
#------------------------------------------------------------------------------------------------------------------------------------------

# County Health Rankings Master dataset

# Used columns
#-------------------------------------------------------------------------------------------------
#| FIPS                          | `% Adults with Diabetes`      | `% Adults with Obesity`       |
#-------------------------------------------------------------------------------------------------
#| `% 65 and over`               | `Age-Adjusted Death Rate`     |
#-----------------------------------------------------------------

# Unused columns
#-------------------------------------------------------------------------------------------------
#| `% Smokers`                                   | `% Excessive Drinking`                        |
#-------------------------------------------------------------------------------------------------
#| `Average Daily PM2.5`                         | `% Fair or Poor Health`                       |
#-------------------------------------------------------------------------------------------------

chr <- read_csv("Data/2020CHR.csv")

chr <- subset(chr, select = c(FIPS, 
                              `% Adults with Diabetes`, 
                              `% Adults with Obesity`, 
                              `% 65 and over`, 
                              `Age-Adjusted Death Rate`))

chr <- rename(chr, c("pct_diabetes" = `% Adults with Diabetes`,
                     "pct_obesity" = `% Adults with Obesity`, 
                     "pct_age65" = `% 65 and over`, 
                     "death_rate" = `Age-Adjusted Death Rate`))

chr <- subset(chr, select = -c(Population))

#------------------------------------------------------------------------------------------------------------------------------------------

# Lung Disease dataset

lungdisease <- read_csv("Data/lungdiseaseestimates_uscounties.csv")

lungdisease$PediatricAsthma         <- lungdisease$PediatricAsthma / lungdisease$TotalPopulation
lungdisease$AdultAsthma             <- lungdisease$AdultAsthma / lungdisease$TotalPopulation
lungdisease$COPD                    <- lungdisease$COPD / lungdisease$TotalPopulation
lungdisease$AdultChronicLungDisease <- lungdisease$AdultChronicLungDisease / lungdisease$TotalPopulation
lungdisease$LungCancer              <- lungdisease$LungCancer / lungdisease$TotalPopulation

lungdisease <- subset(lungdisease, select = -c(`State or County`, TotalPopulation))

#------------------------------------------------------------------------------------------------------------------------------------------

# Historical data
covid_hist = read.csv(text=getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-30-2020.csv"))
covid_us_hist = subset(covid_hist, Country_Region == "US" & is.na(FIPS)==F)

# Import outcome data from JHU CSSE
covid = read.csv(text=getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",date_of_study,".csv")))
covid_us = subset(covid,Country_Region == "US")
covid_us = rbind(covid_us,subset(covid_us_hist, (!(FIPS %in% covid_us$FIPS))  & Confirmed == 0 & Deaths == 0 & is.na(FIPS)==F))
covid_us$FIPS = str_pad(covid_us$FIPS, 5, pad = "0")

# Import exposure PM2.5 data
county_pm <- read.csv("./Data/county_pm25.csv")
county_pm$fips = str_pad(county_pm$fips, 5, pad = "0")

county_temp = read.csv("./Data/temp_seasonal_county.csv")
county_pm$fips = str_pad(county_pm$fips, 5, pad = "0")
# Import census, brfss, testing, mortality, hosptial beds data as potential confounders
county_census = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/census_county_interpolated.csv"))
county_brfss<-read.csv("Data/analytic_data2020.csv",skip = 1)
county_brfss<-county_brfss[,c('fipscode','v011_rawvalue','v009_rawvalue')]
names(county_brfss)<-c('fips','obese','smoke')
county_brfss$fips = str_pad(county_brfss$fips, 5, pad = "0")

state_test = read.csv(text=getURL("https://covidtracking.com/api/v1/states/daily.csv"))
state_test = subset(state_test, date ==paste0(substring(str_remove_all(date_of_study, "-"),5,8),substring(str_remove_all(date_of_study, "-"),1,4)))[,-20]
statecode = read_csv("./Data/statecode.csv")

hospitals = read.csv(text=getURL("https://opendata.arcgis.com/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"))
hospitals$BEDS[hospitals$BEDS < 0] = NA

county_base_mortality = read.table("./Data/county_base_mortality.txt", header = TRUE)
county_old_mortality = read.table("./Data/county_old_mortality.txt", header = TRUE)
county_014_mortality = read.table("./Data/county_014_mortality.txt", header = TRUE)
county_1544_mortality = read.table("./Data/county_1544_mortality.txt", header = TRUE)
county_4564_mortality = read.table("./Data/county_4564_mortality.txt", header = TRUE)

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

# Import NCHS Urban-Rural Classification Scheme for Counties
NCHSURCodes2013 = read_csv("./Data/NCHSURCodes2013.csv")
NCHSURCodes2013$FIPS = str_pad(NCHSURCodes2013$FIPS, 5, pad = "0")
# Import FB survey on covid-like sympton data
script <- getURL("https://raw.githubusercontent.com/cmu-delphi/delphi-epidata/master/src/client/delphi_epidata.R", ssl.verifypeer = FALSE)
eval(parse(text = script))

# Import social distancing measure data
state_policy = read_csv("./Data/state_policy0410.csv")
colnames(state_policy)[6] = "stay_at_home"

reopen <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=1269444822')
colnames(reopen)[2] = "reopen"

closure <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=257109301')
colnames(closure)[2] = "reclosure"

mask <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=1489353670')
colnames(mask)[2] = "mask"

# merging data
state_test = merge(state_test,statecode,by.x = "state" ,by.y = "Code" )
state_test = merge(state_test,state_policy[,c(1,6)],by = "State")
state_test = merge(state_test,reopen[,c(1,2)],by = "State")
state_test = merge(state_test,closure[,c(1,2)],by = "State")
state_test = merge(state_test,mask[,c(1,2)],by = "State")

#state_test$date_since_social = as.numeric(as.Date(Sys.Date()) - as.Date((strptime(state_test$stay_at_home, "%m/%d/%Y"))))
state_test$date_since_social = as.numeric(as.Date(strptime(date_of_study, "%m-%d-%Y")) - as.Date((strptime(state_test$stay_at_home, "%m/%d/%Y"))))
state_test[is.na(state_test$date_since_social)==T,]$date_since_social = 0

state_test$date_since_reopen = as.numeric(as.Date(strptime(date_of_study, "%m-%d-%Y")) - as.Date((strptime(state_test$reopen, "%m/%d/%Y"))))
state_test[is.na(state_test$date_since_reopen)==T,]$date_since_reopen = 0

state_test$date_since_reclosure = as.numeric(as.Date(strptime(date_of_study, "%m-%d-%Y")) - as.Date((strptime(state_test$reclosure, "%m/%d/%Y"))))
state_test[is.na(state_test$date_since_reclosure)==T,]$date_since_reclosure = 0

state_test$date_since_mask = as.numeric(as.Date(strptime(date_of_study, "%m-%d-%Y")) - as.Date((strptime(state_test$mask, "%m/%d/%Y"))))
state_test[is.na(state_test$date_since_mask)==T,]$date_since_mask = 0

# pm2.5 average over 17 years
county_pm_aggregated <- county_pm %>% 
  filter(!is.na(pm25)) %>%
  filter(!is.na(fips)) %>%
  group_by(fips) %>% 
  dplyr::summarise(mean_pm25 = mean(pm25))

head(county_pm_aggregated)
# temperature and relative humidity average over 17 years

county_temp_aggregated = county_temp %>% 
  group_by(fips) %>% 
  summarise(mean_winter_temp= mean(winter_tmmx, na.rm=TRUE),
            mean_summer_temp= mean(summer_tmmx, na.rm=TRUE),
            mean_winter_rm= mean(winter_rmax, na.rm=TRUE),
            mean_summer_rm= mean(summer_rmax, na.rm=TRUE))

county_pm_aggregated = merge(county_pm_aggregated,county_temp_aggregated,by="fips",all.x = T)

hospitals
county_hospitals_aggregated = hospitals %>%
  group_by(COUNTYFIPS) %>%
  summarise(beds = sum(BEDS, na.rm=TRUE))

#county_hospitals_aggregated$COUNTYFIPS = str_pad(county_hospitals_aggregated$COUNTYFIPS, 5, pad = "0")

stop()
county_census_aggregated2 = subset(county_census, year==2016)

county_census_aggregated2$q_popdensity = 1
quantile_popdensity = quantile(county_census_aggregated2$popdensity,c(0.2,0.4,0.6,0.8))
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity<=quantile_popdensity[1]] = 1
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity>quantile_popdensity[1] &
                                         county_census_aggregated2$popdensity<=quantile_popdensity[2]] = 2
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity>quantile_popdensity[2] &
                                         county_census_aggregated2$popdensity<=quantile_popdensity[3]] = 3
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity>quantile_popdensity[3] &
                                         county_census_aggregated2$popdensity<=quantile_popdensity[4]] = 4
county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity>quantile_popdensity[4]] = 5

county_census_aggregated2$fips = str_pad(county_census_aggregated2$fips, 5, pad = "0")
county_census_aggregated2 = merge(county_census_aggregated2,county_brfss,
                                  by="fips",all.x=T)

county_pm_aggregated$fips = str_pad(county_pm_aggregated$fips, 5, pad = "0")
aggregate_pm = merge(county_pm_aggregated,covid_us,by.x="fips",by.y = "FIPS")

aggregate_pm_census = merge(aggregate_pm,county_census_aggregated2,by.x="fips",by.y = "fips")

county_base_mortality$County.Code = str_pad(county_base_mortality$County.Code, 5, pad = "0")

aggregate_pm_census_cdc = merge(aggregate_pm_census,county_base_mortality[,c(1,4,12:15)],by.x = "fips",by.y = "County.Code",all.x = T)

aggregate_pm_census_cdc = aggregate_pm_census_cdc[is.na(aggregate_pm_census_cdc$fips) ==F,]

aggregate_pm_census_cdc_test = merge(aggregate_pm_census_cdc,state_test[,-22],by.x="Province_State",by.y = "State")

colnames(aggregate_pm_census_cdc_test)[2] <- "fips"
aggregate_pm_census_cdc_test
county_hospitals_aggregated
aggregate_pm_census_cdc_test_beds = merge(aggregate_pm_census_cdc_test,county_hospitals_aggregated,by.x = "fips",by.y = "COUNTYFIPS",all.x = T)

aggregate_pm_census_cdc_test_beds$beds[is.na(aggregate_pm_census_cdc_test_beds$beds)] = 0

# Import outcome data from JHU CSSE, calculate the timing of the 1st confirmed case for each county
date_of_all = format(seq(as.Date("2020-03-22"), as.Date(strptime(date_of_study,"%m-%d-%Y")), by = "days"),"%m-%d-%Y")
covid_us_daily_confirmed = lapply(date_of_all,
                                  function(date_of_all){
                                    covid_daily = read.csv(text=getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",date_of_all,".csv")))
                                    covid_daily = covid_daily[!duplicated(covid_daily$FIPS),]
                                    return(subset(covid_daily,Country_Region == "US" & is.na(FIPS)!=T & Confirmed >0 ))
                                  }
)

covid_us_new_confirmed = list()
covid_us_new_confirmed[1] = covid_us_daily_confirmed[1]
covid_us_new_confirmed[[1]]$date_since = length(covid_us_daily_confirmed) 

covid_us_new_confirmed[2:length(date_of_all)] =  lapply(2:(length(covid_us_daily_confirmed)),
                                                        function(i){
                                                          covid_us_new_confirmed =subset(covid_us_daily_confirmed[[i]],!(FIPS %in% unlist(sapply(1:(i-1),function(k)covid_us_daily_confirmed[[k]]$FIPS))))
                                                          covid_us_new_confirmed$date_since = length(covid_us_daily_confirmed) - i + 1
                                                          return(covid_us_new_confirmed)
                                                        })

covid_us_new_confirmed.df <- do.call("rbind", covid_us_new_confirmed)[,c("FIPS","date_since")]
covid_us_new_confirmed.df$FIPS = str_pad(covid_us_new_confirmed.df$FIPS, 5, pad = "0")
aggregate_pm_census_cdc_test_beds = merge(aggregate_pm_census_cdc_test_beds,covid_us_new_confirmed.df,
                                          by.x = "fips",by.y = "FIPS", all.x = T)
aggregate_pm_census_cdc_test_beds$date_since[is.na(aggregate_pm_census_cdc_test_beds$date_since)] = 0

aggregate_pm_census_cdc_test_beds = merge(aggregate_pm_census_cdc_test_beds,NCHSURCodes2013[,c(1,7)],
                                          by.x = "fips",by.y="FIPS", all.x = T)

# Combine five boroughs of NYC
aggregate_pm_census_cdc_test_beds[aggregate_pm_census_cdc_test_beds$Admin2=="New York City",]$population =
  subset(aggregate_pm_census_cdc_test_beds,Admin2=="New York City"& Province_State=="New York")$population +
  subset(aggregate_pm_census_cdc_test_beds, Admin2=="Bronx"& Province_State=="New York")$population +
  subset(aggregate_pm_census_cdc_test_beds, Admin2=="Kings"& Province_State=="New York")$population +
  subset(aggregate_pm_census_cdc_test_beds, Admin2=="Queens"& Province_State=="New York")$population +
  subset(aggregate_pm_census_cdc_test_beds, Admin2=="Richmond"& Province_State=="New York")$population
aggregate_pm_census_cdc_test_beds[aggregate_pm_census_cdc_test_beds$Admin2=="New York City",]$beds =
  subset(aggregate_pm_census_cdc_test_beds,Admin2=="New York City"& Province_State=="New York")$beds +
  subset(aggregate_pm_census_cdc_test_beds, Admin2=="Bronx"& Province_State=="New York")$beds +
  subset(aggregate_pm_census_cdc_test_beds, Admin2=="Kings"& Province_State=="New York")$beds +
  subset(aggregate_pm_census_cdc_test_beds, Admin2=="Queens"& Province_State=="New York")$beds +
  subset(aggregate_pm_census_cdc_test_beds, Admin2=="Richmond"& Province_State=="New York")$beds

vars = c("mean_pm25","poverty","medianhousevalue","medhouseholdincome","pct_owner_occ",
         "education","pct_blk","hispanic","older_pecent","prime_pecent","mid_pecent","obese","smoke",
         "mean_summer_temp","mean_summer_rm","mean_winter_temp","mean_winter_rm")
aggregate_pm_census_cdc_test_beds[aggregate_pm_census_cdc_test_beds$Admin2=="New York City",][,vars] = 
  sapply(vars,function(var){
    (subset(aggregate_pm_census_cdc_test_beds,Admin2=="New York City"& Province_State=="New York")[,var]*subset(aggregate_pm_census_cdc_test_beds,Admin2=="New York City"& Province_State=="New York")$population +
       subset(aggregate_pm_census_cdc_test_beds, Admin2=="Bronx"& Province_State=="New York")[,var]*subset(aggregate_pm_census_cdc_test_beds,Admin2=="Bronx"& Province_State=="New York")$population +
       subset(aggregate_pm_census_cdc_test_beds, Admin2=="Kings"& Province_State=="New York")[,var]*subset(aggregate_pm_census_cdc_test_beds,Admin2=="Kings"& Province_State=="New York")$population +
       subset(aggregate_pm_census_cdc_test_beds, Admin2=="Queens"& Province_State=="New York")[,var]*subset(aggregate_pm_census_cdc_test_beds,Admin2=="Queens"& Province_State=="New York")$population +
       subset(aggregate_pm_census_cdc_test_beds, Admin2=="Richmond"& Province_State=="New York")[,var]*subset(aggregate_pm_census_cdc_test_beds,Admin2=="Richmond"& Province_State=="New York")$population)/(
         subset(aggregate_pm_census_cdc_test_beds,Admin2=="New York City"& Province_State=="New York")$population+subset(aggregate_pm_census_cdc_test_beds,Admin2=="Bronx"& Province_State=="New York")$population+
           subset(aggregate_pm_census_cdc_test_beds, Admin2=="Kings"& Province_State=="New York")$population+ subset(aggregate_pm_census_cdc_test_beds,Admin2=="Queens"& Province_State=="New York")$population +
           subset(aggregate_pm_census_cdc_test_beds,Admin2=="Richmond"& Province_State=="New York")$population)
  })
aggregate_pm_census_cdc_test_beds = subset(aggregate_pm_census_cdc_test_beds,
                                           !(Admin2=="Bronx"& Province_State=="New York")&
                                             !(Admin2=="Kings"& Province_State=="New York")&
                                             !(Admin2=="Queens"& Province_State=="New York")&
                                             !(Admin2=="Richmond"& Province_State=="New York"))

# Request FB survey data from CMU COVIDcast Delphi Research Group
aggregate_pm_census_cdc_test_beds$cli  = 
  sapply(aggregate_pm_census_cdc_test_beds$fips, 
         function(fips){
           if (Epidata$covidcast('fb-survey', 'smoothed_cli', 'day', 'county', list(Epidata$range(20200401, paste0(substring(str_remove_all(date_of_study, "-"),5,8),substring(str_remove_all(date_of_study, "-"),1,4)))),fips)[[2]]!="no results"){
             return(mean(sapply(Epidata$covidcast('fb-survey', 'smoothed_cli', 'day', 'county', list(Epidata$range(20200401, paste0(substring(str_remove_all(date_of_study, "-"),5,8),substring(str_remove_all(date_of_study, "-"),1,4)))),fips)[[2]],function(i){i$value}),na.rm=T))
           }else {return(NA)}})

# Merging datasets on FIPS
aggregate_chr = merge(aggregate_pm_census_cdc_test_beds, chr, by.x = "fips", by.y = "FIPS", all.x = T)
aggregate_chr_cdc = merge(aggregate_chr, cdc, by.x = "fips", by.y = "FIPS", all.x = T)
aggregate_chr_cdc_lung = merge(aggregate_chr_cdc, lungdisease, by.x = "fips", by.y = "FIPS", all.x = T)

# Saving data frame to Rds file
file = paste("Preprocessing_FTS_Outputs/", date_of_study, "data.Rds",sep = "")
saveRDS(aggregate_chr_cdc_lung, file)
