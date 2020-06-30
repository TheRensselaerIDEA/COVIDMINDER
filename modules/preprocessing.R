#### Preprocessing

# Set provider capacity column names
colnames(provider_capacity) <- c("NAME","total_hosp_beds","hosp_beds_per_1000","total_CHCs","CHC_delivery_sites")
# Calculate hospital beds per 1000
provider_capacity$p_hosp_beds <-provider_capacity$hosp_beds_per_1000/1000 

# Calculate pUS
pUS.1 <- as.numeric(provider_capacity[which(provider_capacity$NAME=="United States"),"p_hosp_beds"])
pSK.1 <- 12.3/1000 # South Korean rate
pIT.1 <- 3.2/1000 # Italy rate
pDE.1 <- 8.0/1000 # Germany rate

#hosp_beds_ldi <- unlist(lapply(provider_capacity$p_hosp_beds, FUN=function(x){log((x/(1-x))/(pUS.1/(1-pUS.1)))}))
#hosp_beds_ldi <- unlist(lapply(provider_capacity$p_hosp_beds, FUN=function(x){log((x/(1-x))/(pSK.1/(1-pSK.1)))}))
#hosp_beds_ldi <- unlist(lapply(provider_capacity$p_hosp_beds, FUN=function(x){log((x/(1-x))/(pIT.1/(1-pIT.1)))}))
#hosp_beds_ldi <- unlist(lapply(provider_capacity$p_hosp_beds, FUN=function(x){-log(x/pIT.1)}))
hosp_beds_ldi <- unlist(lapply(provider_capacity$p_hosp_beds, FUN=function(x){log(x/pIT.1)}))

provider_capacity <- data.frame(provider_capacity, hosp_beds_ldi)
provider_capacity <- provider_capacity[match(states$NAME, provider_capacity$NAME),]
provider_capacity <- provider_capacity[1:51,]

provider_capacity <- provider_capacity %>% 
  mutate(hosp_beds_ldi = replace(hosp_beds_ldi, hosp_beds_ldi < -5, -5)) 

states <- data.frame(states, "hosp_beds_ldi"=provider_capacity$hosp_beds_ldi) # Append to states

## COVID-19 Testing fixing
# colnames(state_covid_testing) <- c("NAME","total_num_tests","tests_pos_results")
# Inner join to add population
state_covid_testing <- left_join(state_covid_testing, population, by = c('NAME'))

# TODO: This is really just the test rate, not "per 1000"
state_covid_testing <- state_covid_testing %>% 
  mutate(tests_per_1000 = total_num_tests / Population)  # This is actual rate, not "per 1000"
state_covid_testing$tests_per_1000 <- state_covid_testing$tests_per_1000 * 1000

# Use current US rate
pUS.2 <- as.numeric(state_covid_testing[which(state_covid_testing$NAME=="United States"),"tests_per_1000"])

# Scraped values (updated every few days): see https://bit.ly/2yMyjFX
# Checked: 13 May 2020

#pUS.2 <- total_test_rates.df[total_test_rates.df$iso_code == "USA",]$total_tests_per_thousand #/ 1000
pPR.2 <- total_test_rates.df[total_test_rates.df$iso_code == "PRT",]$total_tests_per_thousand #/ 1000
pSP.2 <- total_test_rates.df[total_test_rates.df$iso_code == "ESP",]$total_tests_per_thousand #/ 1000
pBE.2 <- total_test_rates.df[total_test_rates.df$iso_code == "BEL",]$total_tests_per_thousand #/ 1000
pIT.2 <- total_test_rates.df[total_test_rates.df$iso_code == "ITA",]$total_tests_per_thousand #/ 1000
pRU.2 <- total_test_rates.df[total_test_rates.df$iso_code == "RUS",]$total_tests_per_thousand #/ 1000
pCH.2 <- total_test_rates.df[total_test_rates.df$iso_code == "CHE",]$total_tests_per_thousand #/ 1000
pDE.2 <- total_test_rates.df[total_test_rates.df$iso_code == "DEU",]$total_tests_per_thousand #/ 1000
pIR.2 <- total_test_rates.df[total_test_rates.df$iso_code == "IRL",]$total_tests_per_thousand #/ 1000
pCA.2 <- total_test_rates.df[total_test_rates.df$iso_code == "CAN",]$total_tests_per_thousand #/ 1000
pUK.2 <- total_test_rates.df[total_test_rates.df$iso_code == "GBR",]$total_tests_per_thousand #/ 1000

# for drop-down
country_testing_choices <- c("us","pr","sp","be","ch","it","ru","ir","de","ca","uk")

names(country_testing_choices) <- c(paste0("United States (" ,round(pUS.2),"/1000)"),
                                    paste0("Portugal ("      ,round(pPR.2),"/1000)"),
                                    paste0("Spain ("         ,round(pSP.2),"/1000)"),
                                    paste0("Belgium ("       ,round(pBE.2),"/1000)"),
                             paste0("Switzerland ("   ,round(pCH.2),"/1000)"),
                             paste0("Italy ("         ,round(pIT.2),"/1000)"),
                             paste0("Russia ("        ,round(pRU.2),"/1000)"),
                             paste0("Ireland ("       ,round(pIR.2),"/1000)"),
                             paste0("Germany ("       ,round(pDE.2),"/1000)"),
                             paste0("Canada ("        ,round(pCA.2),"/1000)"),
                             paste0("United Kingdom (",round(pUK.2),"/1000)"))

# Calculate state DIs based on a country's selected rate
# UPDATE: make several values available . See https://bit.ly/2yMyjFX for current rates!
tests_ldi.us <- unlist(lapply(state_covid_testing$tests_per_1000, FUN=function(x){log(x/pUS.2)}))
tests_ldi.be <- unlist(lapply(state_covid_testing$tests_per_1000, FUN=function(x){log(x/pBE.2)}))
tests_ldi.pr <- unlist(lapply(state_covid_testing$tests_per_1000, FUN=function(x){log(x/pPR.2)}))
tests_ldi.ch <- unlist(lapply(state_covid_testing$tests_per_1000, FUN=function(x){log(x/pCH.2)}))
tests_ldi.it <- unlist(lapply(state_covid_testing$tests_per_1000, FUN=function(x){log(x/pIT.2)}))
tests_ldi.sp <- unlist(lapply(state_covid_testing$tests_per_1000, FUN=function(x){log(x/pSP.2)}))
tests_ldi.ir <- unlist(lapply(state_covid_testing$tests_per_1000, FUN=function(x){log(x/pIR.2)}))
tests_ldi.de <- unlist(lapply(state_covid_testing$tests_per_1000, FUN=function(x){log(x/pDE.2)}))
tests_ldi.ca <- unlist(lapply(state_covid_testing$tests_per_1000, FUN=function(x){log(x/pCA.2)}))
tests_ldi.uk <- unlist(lapply(state_covid_testing$tests_per_1000, FUN=function(x){log(x/pUK.2)}))
tests_ldi.ru <- unlist(lapply(state_covid_testing$tests_per_1000, FUN=function(x){log(x/pRU.2)}))

# Write to data frame
state_covid_testing <- data.frame(state_covid_testing, tests_ldi.us)
state_covid_testing <- data.frame(state_covid_testing, tests_ldi.be)
state_covid_testing <- data.frame(state_covid_testing, tests_ldi.pr)
state_covid_testing <- data.frame(state_covid_testing, tests_ldi.ch)
state_covid_testing <- data.frame(state_covid_testing, tests_ldi.it)
state_covid_testing <- data.frame(state_covid_testing, tests_ldi.sp)
state_covid_testing <- data.frame(state_covid_testing, tests_ldi.ir)
state_covid_testing <- data.frame(state_covid_testing, tests_ldi.de)
state_covid_testing <- data.frame(state_covid_testing, tests_ldi.ca)
state_covid_testing <- data.frame(state_covid_testing, tests_ldi.uk)
state_covid_testing <- data.frame(state_covid_testing, tests_ldi.ru)

state_covid_testing <- state_covid_testing[match(states$NAME, state_covid_testing$NAME),]

state_covid_testing <- state_covid_testing[1:51,]

state_covid_testing <- state_covid_testing %>% 
  mutate(tests_ldi.us = replace(tests_ldi.us, tests_ldi.us < -5, -5)) %>%
  mutate(tests_ldi.be = replace(tests_ldi.be, tests_ldi.be < -5, -5)) %>%
  mutate(tests_ldi.pr = replace(tests_ldi.pr, tests_ldi.pr < -5, -5)) %>%
  mutate(tests_ldi.ch = replace(tests_ldi.ch, tests_ldi.ch < -5, -5)) %>%
  mutate(tests_ldi.it = replace(tests_ldi.it, tests_ldi.it < -5, -5)) %>%
  mutate(tests_ldi.sp = replace(tests_ldi.sp, tests_ldi.sp < -5, -5)) %>%
  mutate(tests_ldi.ir = replace(tests_ldi.ir, tests_ldi.ir < -5, -5)) %>%
  mutate(tests_ldi.de = replace(tests_ldi.de, tests_ldi.de < -5, -5)) %>%
  mutate(tests_ldi.ca = replace(tests_ldi.ca, tests_ldi.ca < -5, -5)) %>%
  mutate(tests_ldi.ru = replace(tests_ldi.ru, tests_ldi.ru < -5, -5)) %>%
  mutate(tests_ldi.uk = replace(tests_ldi.uk, tests_ldi.uk < -5, -5)) 

states <- data.frame(states, "tests_per_1000"=state_covid_testing$tests_per_1000) # Append to states
states <- data.frame(states, "Population"=state_covid_testing$Population) # Append to states (reference)

states <- data.frame(states, "tests_ldi.us"=state_covid_testing$tests_ldi.us) # Append to states
states <- data.frame(states, "tests_ldi.be"=state_covid_testing$tests_ldi.be) # Append to states
states <- data.frame(states, "tests_ldi.pr"=state_covid_testing$tests_ldi.pr) # Append to states
states <- data.frame(states, "tests_ldi.ch"=state_covid_testing$tests_ldi.ch) # Append to states
states <- data.frame(states, "tests_ldi.it"=state_covid_testing$tests_ldi.it) # Append to states
states <- data.frame(states, "tests_ldi.sp"=state_covid_testing$tests_ldi.sp) # Append to states
states <- data.frame(states, "tests_ldi.ir"=state_covid_testing$tests_ldi.ir) # Append to states
states <- data.frame(states, "tests_ldi.de"=state_covid_testing$tests_ldi.de) # Append to states
states <- data.frame(states, "tests_ldi.ca"=state_covid_testing$tests_ldi.ca) # Append to states
states <- data.frame(states, "tests_ldi.uk"=state_covid_testing$tests_ldi.uk) # Append to states
states <- data.frame(states, "tests_ldi.ru"=state_covid_testing$tests_ldi.ru) # Append to states

## At-risk Adults fixing

# Calculate pUS
# All at-risk
pUS.3 <- as.numeric(at_risk_adults[which(at_risk_adults$NAME=="United States"),"p_at_risk_adults"])

at_risk_ldi <- unlist(lapply(at_risk_adults$p_at_risk_adults, FUN=function(x){log((x/(1-x))/(pUS.3/(1-pUS.3)))}))

at_risk_adults <- data.frame(at_risk_adults, at_risk_ldi)

#Older at-risk
pUS.4 <- as.numeric(at_risk_adults[which(at_risk_adults$NAME=="United States"),"p_older_at_risk_adults"])

older_at_risk_ldi <- unlist(lapply(at_risk_adults$p_older_at_risk_adults, FUN=function(x){log((x/(1-x))/(pUS.4/(1-pUS.4)))}))

at_risk_adults <- data.frame(at_risk_adults, older_at_risk_ldi)

# at_risk_adults <- at_risk_adults %>% 
#   mutate(at_risk_adults = replace(at_risk_adults, at_risk_adults < -5, -5)) %>%
#   mutate(older_at_risk_ldi = replace(older_at_risk_ldi, older_at_risk_ldi < -5, -5)) 

# RE-order to match states ordering
at_risk_adults <- at_risk_adults[match(states$NAME, at_risk_adults$NAME),]

at_risk_adults <- at_risk_adults[1:51,]

# Append the new columns to states
states <- data.frame(states, "at_risk_ldi"=at_risk_adults$at_risk_ldi) # Append to states

states <- data.frame(states, "older_at_risk_ldi"=at_risk_adults$older_at_risk_ldi) # Append to states

# Cardio mortality (NEW)
# UPDATED: 16 Apr (new source and math corrections)
cardio_deaths_2015$p_Overall <- cardio_deaths_2015$p_Overall/100000
pUS.5 <- as.numeric(cardio_deaths_2015[which(cardio_deaths_2015$NAME=="United States"),"p_Overall"])

#cardio_death_rate_ldi <- unlist(lapply(cardio_deaths_2017$cardio_deaths_p_100000, FUN=function(x){log((x/(1-x))/(pUS.5/(1-pUS.5)))}))
cardio_death_rate_ALL_ldi <- unlist(lapply(cardio_deaths_2015$p_Overall, FUN=function(x){-log(pUS.5/x)}))
cardio_death_rate_BNH_ldi <- unlist(lapply(cardio_deaths_2015$p_Black_Non_Hispanic, FUN=function(x){-log(pUS.5/x)}))

cardio_deaths_2015 <- data.frame(cardio_deaths_2015, cardio_death_rate_ALL_ldi)
cardio_deaths_2015 <- data.frame(cardio_deaths_2015, cardio_death_rate_BNH_ldi)

cardio_deaths_2015 <- cardio_deaths_2015 %>% 
  mutate(cardio_death_rate_ALL_ldi = replace(cardio_death_rate_ALL_ldi, cardio_death_rate_ALL_ldi < -5, -5)) %>%
  mutate(cardio_death_rate_BNH_ldi = replace(cardio_death_rate_BNH_ldi, cardio_death_rate_BNH_ldi < -5, -5)) 

# RE-order to match states ordering
cardio_deaths_2015 <- cardio_deaths_2015[match(states$NAME, cardio_deaths_2015$NAME),]

cardio_deaths_2015 <- cardio_deaths_2015[1:51,]

# Append the new Cardio columns to states
states <- data.frame(states, "Heart Disease Mortality_rate"=cardio_deaths_2015$p_Overall, check.names = F) # Append to states
states <- data.frame(states, "cardio_deaths_p_Black_Non_Hispanic"=cardio_deaths_2015$p_Black_Non_Hispanic, check.names = F) # Append to states
states <- data.frame(states, "Heart Disease Mortality_rate_ldi"=cardio_deaths_2015$cardio_death_rate_ALL_ldi, check.names = F) # Append to states
states <- data.frame(states, "cardio_death_rate_BNH_ldi"=cardio_deaths_2015$cardio_death_rate_BNH_ldi, check.names = F) # Append to states




# COVID-19 Deaths per COVID-19 Case - UPDATE: Moved to USA Facts data
US.pop <-  population[population$NAME == "United States",]$Population
us_covid_data <- todays.case.data %>%
  summarise(
    Cases = sum(Cases),
    Population = US.pop,
    Mortality = sum(Mortality)
  ) %>%
  mutate(Case_rate = Cases/Population) %>%
  mutate(Mortality_rate = Mortality/Population)

covid_data_states <- todays.case.data %>% 
  group_by(State, name) %>%
  summarise(
    Cases = sum(Cases),
    Population = sum(population),
    Mortality = sum(Mortality)
  ) %>%
  mutate(Case_rate = Cases/Population) %>%
  mutate(Mortality_rate = Mortality/Population)

#pUS.6.cases <- as.numeric(covid_data_states[which(covid_data_states$NAME=="United States"),"calc_case_rate"])
pUS.6.cases <- us_covid_data$Case_rate
#pUS.6.deaths <- as.numeric(covid_data_states[which(covid_data_states$NAME=="United States"),"p_death_rate"])
pUS.6.deaths <- us_covid_data$Mortality_rate

#death_rate_ldi <- unlist(lapply(covid_data_states$p_death_rate, FUN=function(x){log((x/(1-x))/(pUS.6/(1-pUS.6)))}))
Case_rate_ldi <- unlist(lapply(covid_data_states$Case_rate, FUN=function(x){-log(pUS.6.cases/x)}))
Mortality_rate_ldi <- unlist(lapply(covid_data_states$Mortality_rate, FUN=function(x){-log(pUS.6.deaths/x)}))
#browser()
#ldi's for individual state report cards - Jose 
todays.case.data$Case_rate <- todays.case.data$Cases/todays.case.data$population
todays.case.data$Case_rate_ldi <- unlist(lapply(todays.case.data$Case_rate, FUN=function(x){-log(pUS.6.cases/x)})) 
todays.case.data <- todays.case.data %>%
  mutate(Case_rate_ldi = replace(Case_rate_ldi, Case_rate_ldi < -5, -5))

todays.case.data$Mortality_rate <- todays.case.data$Mortality/todays.case.data$population
todays.case.data$Mortality_rate_ldi <- unlist(lapply(todays.case.data$Mortality_rate, FUN=function(x){-log(pUS.6.deaths/x)}))
todays.case.data <- todays.case.data %>%
  mutate(Mortality_rate_ldi = replace(Mortality_rate_ldi, Mortality_rate_ldi < -5, -5))

covid_data_states <- data.frame(covid_data_states, Mortality_rate_ldi)
covid_data_states <- data.frame(covid_data_states, Case_rate_ldi)

covid_data_states <- covid_data_states %>% 
  mutate(Mortality_rate_ldi = replace(Mortality_rate_ldi, Mortality_rate_ldi < -5, -5)) %>%
  mutate(Case_rate_ldi = replace(Case_rate_ldi, Case_rate_ldi < -5, -5)) 

# RE-order to match states ordering
covid_data_states <- covid_data_states[match(states$NAME, covid_data_states$name),]

# Append the new column to states
covid_data_states <- covid_data_states[1:51,]

states <- data.frame(states, "Overall Mortality_rate_ldi"=covid_data_states$Mortality_rate_ldi, check.names = F) # Append to states
states <- data.frame(states, "Overall Mortality_rate"=covid_data_states$Mortality_rate, check.names = F) # Append to states
states <- data.frame(states, "Overall Case_rate_ldi"=covid_data_states$Case_rate_ldi, check.names = F) # Append to states
states <- data.frame(states, "Overall Case_rate"=covid_data_states$Case_rate, check.names = F) # Append to states

##### US Racial Disparity
# DI's based on covid_racial_data_states.wide
# Percent cases / percent population, per-state

# Unweighted population pct
# covid_racial_data_states.wide$death_rate_ldi_nhw <- log(covid_racial_data_states.wide$nhw_deaths_pct / covid_racial_data_states.wide$nhw_un_pop_pct)
# covid_racial_data_states.wide$death_rate_ldi_nhbaa <- log(covid_racial_data_states.wide$nhbaa_deaths_pct / covid_racial_data_states.wide$nhbaa_un_pop_pct)
# covid_racial_data_states.wide$death_rate_ldi_nhaian <- log(covid_racial_data_states.wide$nhaian_deaths_pct / covid_racial_data_states.wide$nhaian_un_pop_pct)
# covid_racial_data_states.wide$death_rate_ldi_nhapi <- log(covid_racial_data_states.wide$nhapi_deaths_pct / covid_racial_data_states.wide$nhapi_un_pop_pct)
# covid_racial_data_states.wide$death_rate_ldi_hlt <- log(covid_racial_data_states.wide$hlt_deaths_pct / covid_racial_data_states.wide$hlt_un_pop_pct)
# covid_racial_data_states.wide$death_rate_ldi_other <- log(covid_racial_data_states.wide$other_deaths_pct / covid_racial_data_states.wide$other_un_pop_pct)

# Weighted population percentage
covid_racial_data_states.wide$death_rate_ldi_nhw <- log(covid_racial_data_states.wide$nhw_deaths_pct / covid_racial_data_states.wide$nhw_wd_pop_pct)
covid_racial_data_states.wide$death_rate_ldi_nhbaa <- log(covid_racial_data_states.wide$nhbaa_deaths_pct / covid_racial_data_states.wide$nhbaa_wd_pop_pct)
covid_racial_data_states.wide$death_rate_ldi_nhaian <- log(covid_racial_data_states.wide$nhaian_deaths_pct / covid_racial_data_states.wide$nhaian_wd_pop_pct)
covid_racial_data_states.wide$death_rate_ldi_nhapi <- log(covid_racial_data_states.wide$nhapi_deaths_pct / covid_racial_data_states.wide$nhapi_wd_pop_pct)
covid_racial_data_states.wide$death_rate_ldi_hlt <- log(covid_racial_data_states.wide$hlt_deaths_pct / covid_racial_data_states.wide$hlt_wd_pop_pct)
covid_racial_data_states.wide$death_rate_ldi_other <- log(covid_racial_data_states.wide$other_deaths_pct / covid_racial_data_states.wide$other_wd_pop_pct)

# colnames(covid_racial_data_states.wide) <- covid_racial_data_states.wide %>%
#   rename(NAME = state)

# Join our new columns in by NAME
# NOTE: This is cleaner than elsewhere and imports ALL of the race/ethnicity data
states <- dplyr::left_join(states, covid_racial_data_states.wide[,-1], by = c("NAME" = "NAME"))


#####
# NY specific calculations: Death Rates
#pNY.6.deaths <- sum(NY.data$deaths)/sum(NY.data$Population)
#pNY.6.cases <- sum(NY.data$cases)/sum(NY.data$Population)
#pNY.6.diabetes <- as.numeric(NY_counties_diabetes[1,"pct_Adults_with_Diabetes"])
#pNY.6.obesity <- as.numeric(NY_counties_diabetes[1,"pct_Adults_with_Obesity"])

#NY.data <- transform(NY.data, death_rate = deaths/Population)
#NY.data <- transform(NY.data, case_rate = cases/Population) # We already do this

#NY.data <- NY.data %>% 
#  filter(!County == c("Out of NY","Unassigned"))

# NY.data$death_rate_ldi <- unlist(lapply(NY.data$death_rate, FUN=function(x){-log(pNY.6.deaths/x)}))  # vs NY rate
#NY.data$death_rate_ldi <- unlist(lapply(NY.data$death_rate, FUN=function(x){-log(pUS.6.deaths/x)})) # vs UR rate

#NY.data$case_rate_ldi <- unlist(lapply(NY.data$case_rate, FUN=function(x){-log(pUS.6.cases/x)}))

# Need this for NY Diabetes and obesity...
pUS.7.diabetes <- as.numeric(diabetes_data_states[which(diabetes_data_states$State=="United States"),"pct_Adults_with_Diabetes"])/100
pUS.8.obesity <- as.numeric(obesity_data_states[which(obesity_data_states$State=="United States"),"pct_Adults_with_Obesity"])/100

#NY.data$diabetes_ldi <- unlist(lapply(NY.data$pct_Adults_with_Diabetes, FUN=function(x){-log(pUS.7.diabetes/x)}))
#NY.data$obesity_ldi <- unlist(lapply(NY.data$pct_Adults_with_Obesity, FUN=function(x){-log(pUS.8.obesity/x)}))

# Clean up the ranges
#NY.data <- NY.data %>% 
#  mutate(death_rate_ldi = replace(death_rate_ldi, death_rate_ldi < -5, -5)) %>%
#  mutate(case_rate_ldi = replace(case_rate_ldi, case_rate_ldi < -5, -5))# %>%
  #mutate(diabetes_ldi = replace(diabetes_ldi, diabetes_ldi < -5, -5)) %>%
  #mutate(obesity_ldi = replace(obesity_ldi, obesity_ldi < -5, -5))

### NEW: US Diabetes Rates
#pUS.7 <- as.numeric(diabetes_data_states[which(diabetes_data_states$State=="United States"),"pct_Adults_with_Diabetes"])

diabetes_rate_ldi <- unlist(lapply(diabetes_data_states$pct_Adults_with_Diabetes, FUN=function(x){-log(pUS.7.diabetes/(x/100))}))

diabetes_data_states <- data.frame(diabetes_data_states, diabetes_rate_ldi)

diabetes_data_states <- diabetes_data_states %>% 
  mutate(diabetes_rate_ldi = replace(diabetes_rate_ldi, diabetes_rate_ldi < -5, -5)) 

# RE-order to match states ordering
diabetes_data_states <- diabetes_data_states[match(states$NAME, diabetes_data_states$State),]

# Append the new column to states
diabetes_data_states <- diabetes_data_states[1:51,]

states <- data.frame(states, "Diabetes_rate_ldi"=diabetes_data_states$diabetes_rate_ldi, check.names = F) # Append to states
states <- data.frame(states, "Diabetes_rate"=diabetes_data_states$pct_Adults_with_Diabetes/100, check.names = F) # Append to states

# State report card version (county level)
todays.case.data <- inner_join(todays.case.data, 
                               diabetes_data_counties[c("CountyFIPS","Percentage")], 
                               by=c("countyFIPS" = "CountyFIPS")) %>%
  rename(Diabetes_rate = Percentage)
todays.case.data$Diabetes_rate <- as.numeric(todays.case.data$Diabetes_rate)/100 # Percentage
todays.case.data$Diabetes_rate_ldi <- unlist(lapply(todays.case.data$Diabetes_rate, FUN=function(x){-log(pUS.7.diabetes/(x))}))
todays.case.data <- todays.case.data %>%
  mutate(Diabetes_rate_ldi = replace(Diabetes_rate_ldi, Diabetes_rate_ldi < -5, -5))


### NEW: US Obesity Rates
# owid_data.obesity.rate
pUS.8.obesity <- as.numeric(obesity_data_states[which(obesity_data_states$State=="United States"),"pct_Adults_with_Obesity"])/100


# Calculate state DIs based on a country's selected rate
# UPDATE: make several values available . See https://bit.ly/2yMyjFX for current rates!

obesity_ldi.us <- unlist(lapply(obesity_data_states$pct_Adults_with_Obesity, FUN=function(x){-log(pUS.8.obesity/(x/100))}))

# Write to data frame
obesity_data_states <- data.frame(obesity_data_states, obesity_ldi.us)

# RE-order to match states ordering
obesity_data_states <- obesity_data_states[match(states$NAME, obesity_data_states$State),]

# Append the new column to states
obesity_data_states <- obesity_data_states[1:51,]
obesity_data_states <- obesity_data_states %>% 
  mutate(obesity_ldi.us = replace(obesity_ldi.us, obesity_ldi.us < -5, -5))


states <- data.frame(states, "Obesity_rate"=obesity_data_states$pct_Adults_with_Obesity/100, check.names = F) # Append to states
states <- data.frame(states, "Obesity_rate_ldi"=obesity_data_states$obesity_ldi.us, check.names = F) # Append to states

# State Report Cards - Obesity
obesity_data_counties$FIPS <- as.numeric(obesity_data_counties$FIPS)
todays.case.data <- inner_join(todays.case.data,
                               obesity_data_counties[c("FIPS", "% Adults with Obesity")] ,
                               by=c("countyFIPS" = "FIPS")) %>%
  rename(Obesity_rate = `% Adults with Obesity`)
todays.case.data$Obesity_rate <- as.numeric(todays.case.data$Obesity_rate)/100 # Percentage
todays.case.data$Obesity_rate_ldi <- unlist(lapply(todays.case.data$Obesity_rate, FUN=function(x){-log(pUS.8.obesity/(x))}))
todays.case.data <- todays.case.data %>%
  mutate(Obesity_rate_ldi = replace(Obesity_rate_ldi, Obesity_rate_ldi < -5, -5))

# NEW: US chronic respiratory disease mortality
pUS.9.CRD <- CRD_data_counties[CRD_data_counties$CNTY == "United States",]$MortalityRate2014/100000
states <- states %>%
  left_join(CRD_data_counties[c("CNTY", "MortalityRate2014")],
            by = c("NAME" = "CNTY")) %>%
  mutate(MortalityRate2014 = MortalityRate2014/100000) %>%
  rename("CRD Mortality_rate" = MortalityRate2014) %>%
  mutate("CRD Mortality_rate_ldi" = -log(pUS.9.CRD/(`CRD Mortality_rate`))) %>%
  mutate("CRD Mortality_rate_ldi" = replace(`CRD Mortality_rate_ldi`, `CRD Mortality_rate_ldi` < -5, -5))

todays.case.data <- left_join(todays.case.data,
                               CRD_data_counties[c("FIPS", "MortalityRate2014")],
                               by = c("countyFIPS" = "FIPS")) %>%
  mutate(MortalityRate2014 = MortalityRate2014/100000) %>%
  rename("CRD Mortality_rate" = MortalityRate2014) %>%
  mutate("CRD Mortality_rate_ldi" = -log(pUS.9.CRD/(`CRD Mortality_rate`))) %>%
  mutate("CRD Mortality_rate_ldi" = replace(`CRD Mortality_rate_ldi`, `CRD Mortality_rate_ldi` < -5, -5))

## Needed for NY TS plot

# Pre-filter to remove small numbers
covid_NY_TS_counties_long <- covid_NY_TS_counties_long %>% 
  filter(cases >= 2) %>%
  filter(County != "Unassigned")

covid_NY_TS_counties_long.cases <- covid_NY_TS_counties_long.cases %>% 
  filter(cases >= 2) %>%
  filter(County != "Unassigned")

covid_NY_TS_plot <- covid_NY_TS_counties_long %>%
  group_by(date)

covid_NY_TS_counties_long <- dplyr::inner_join(covid_NY_TS_counties_long, as.data.frame(NY_counties_regions), by = c("County" = "County"))

# NOTE: The new TS plot is using this special version
covid_NY_TS_counties_long.cases <- dplyr::inner_join(covid_NY_TS_counties_long.cases, as.data.frame(NY_counties_regions), by = c("County" = "County"))

# covid_NY_TS_plot.cases <- covid_NY_TS_counties_long.cases %>%
#   group_by(date)
# Do it this way to be safe:
covid_NY_TS_plot.cases <- read_csv("data/csv/time_series/covid_NY_TS_plot.cases.csv")

# Creates difference in cases from previous recorded date. First date is equal to case reported
covid_NY_TS_plot.cases %>% group_by(County) %>% 
  mutate(diff = ifelse(as.Date(date - 1) == lag(date), cases - lag(cases), cases)) -> 
  covid_NY_TS_plot.cases

covid_NY_TS_plot.cases %>% 
  mutate(p_diff = ifelse(as.Date(date - 1) == lag(date), p_cases - lag(p_cases), p_cases)) %>%
  ungroup() -> covid_NY_TS_plot.cases

covid_NY_TS_plot.cases$diff <- ifelse(is.na(covid_NY_TS_plot.cases$diff), covid_NY_TS_plot.cases$cases, covid_NY_TS_plot.cases$diff)
covid_NY_TS_plot.cases$p_diff <- ifelse(is.na(covid_NY_TS_plot.cases$p_diff), covid_NY_TS_plot.cases$p_cases, covid_NY_TS_plot.cases$p_diff)

covid_NY_TS_plot.deaths <- read_csv("data/csv/time_series/covid_NY_TS_plot.deaths.csv")
# Creates difference in deaths from previous recorded date. First date is equal to deaths reported
covid_NY_TS_plot.deaths %>% group_by(County) %>% 
  mutate(diff = ifelse(as.Date(date - 1) == lag(date), deaths - lag(deaths), deaths)) -> 
  covid_NY_TS_plot.deaths

covid_NY_TS_plot.deaths %>% 
  mutate(p_diff = ifelse(as.Date(date - 1) == lag(date), p_deaths - lag(p_deaths), p_deaths)) %>%
  ungroup() -> covid_NY_TS_plot.deaths

covid_NY_TS_plot.deaths$diff <- ifelse(is.na(covid_NY_TS_plot.deaths$diff), covid_NY_TS_plot.deaths$deaths, covid_NY_TS_plot.deaths$diff)
covid_NY_TS_plot.deaths$p_diff <- ifelse(is.na(covid_NY_TS_plot.deaths$p_diff), covid_NY_TS_plot.deaths$p_deaths, covid_NY_TS_plot.deaths$p_diff)


# Legislative action 
# Executive Orders (EO) and Total Bills

# RE-order EO to match states ordering
covid_eo_bills <- covid_eo_bills[match(states$NAME, covid_eo_bills$NAME),]

# Append the new column to states
covid_eo_bills <- covid_eo_bills[1:51,]

states <- data.frame(states, "covid_eo"=covid_eo_bills$Total_EO, check.names = F) # Append to states
states <- data.frame(states, "covid_bills"=covid_eo_bills$Total_Bills, check.names = F) # Append to states

# State Ranking
time.period <- 7
ranking <- covid_TS_state_long.cases %>%
  group_by(State) %>%
  filter(State != "DC") %>%
  top_n(time.period, wt=date) %>%
  summarise(
    population = max(population),
    cases.delta = sum(p_diff)/(time.period*100000),
    deaths.delta =  sum(p.d_diff)/(time.period*100000),
    cases.pct = (max(p_cases) - min(p_cases))/max(p_cases),
    deaths.pct = (max(p_deaths) - min(p_deaths))/max(p_deaths)
  ) %>%
  #mutate(points = 0.5*(cases.delta + deaths.delta)) %>%
  arrange(cases.pct) %>%
  mutate(rank = row_number()) %>%
  left_join(state.abr[c("abr", "name")],
            by = c("State" = "abr"))


# US LDI from rankings
US.ranking <- covid_TS_US_long.cases %>%
  top_n(time.period, wt=date) %>%
  summarise(
    cases.delta = sum(p_diff)/(time.period*100000),
    deaths.delta =  sum(p.d_diff)/(time.period*100000)
    )

ranking.ldi <- ranking %>%
  mutate(`Daily Case_rate_ldi` = -log(US.ranking$cases.delta/cases.delta)) %>%
  mutate(`Daily Case_rate_ldi` = replace(`Daily Case_rate_ldi`, `Daily Case_rate_ldi` < -5, -5)) %>%
  mutate(`Daily Mortality_rate_ldi` = -log(US.ranking$deaths.delta/deaths.delta)) %>%
  mutate(`Daily Mortality_rate_ldi` = replace(`Daily Mortality_rate_ldi`, `Daily Mortality_rate_ldi` < -5, -5)) %>%
  rename(`Daily Case_rate` = cases.delta) %>%
  rename(`Daily Mortality_rate` = deaths.delta)

states <- states %>%
  left_join(ranking.ldi[c("name", "Daily Case_rate", "Daily Case_rate_ldi", "Daily Mortality_rate", "Daily Mortality_rate_ldi")],
            by = c("NAME" = "name"))

# County "Ranking"
#todays.case.data$`Daily Case_rate` <- covid_TS_counties_long.cases
ct.ranking <- covid_TS_counties_long.cases %>%
  group_by(countyFIPS) %>%
  top_n(time.period, wt=date) %>%
  summarise(
    `Daily Case_rate` = sum(p_diff)/(time.period*100000),
    `Daily Mortality_rate` =  sum(p.d_diff)/(time.period*100000)
  ) %>%
  mutate(`Daily Mortality_rate_ldi` = -log(US.ranking$deaths.delta/`Daily Mortality_rate`)) %>%
  mutate(`Daily Mortality_rate_ldi` = replace(`Daily Mortality_rate_ldi`, `Daily Mortality_rate_ldi` < -5, -5)) %>%
  mutate(`Daily Case_rate_ldi` = -log(US.ranking$cases.delta/`Daily Case_rate`)) %>%
  mutate(`Daily Case_rate_ldi` = replace(`Daily Case_rate_ldi`, `Daily Case_rate_ldi` < -5, -5))

todays.case.data <- todays.case.data %>%
  left_join(ct.ranking,
            by = c("countyFIPS" = "countyFIPS"))

