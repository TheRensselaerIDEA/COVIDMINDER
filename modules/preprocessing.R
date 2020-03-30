#### Preprocessing

# Calculate hypertension death rate per 100000
hypertension_mortality$p_ht_death_rate <-hypertension_mortality$p_ht_death_rate / 100000

# Set provider capacity column names
colnames(provider_capacity) <- c("NAME","total_hosp_beds","hosp_beds_per_1000","total_CHCs","CHC_delivery_sites")
# Calculate hospital beds per 1000 per 1000
provider_capacity$p_hosp_beds <-provider_capacity$hosp_beds_per_1000/1000 

# Calculate pUS
pUS.1 <- as.numeric(provider_capacity[which(provider_capacity$NAME=="United States"),"p_hosp_beds"])

# ldi.fct <- function(x) {log((x/(1-x))/(pUS/(1-pUS)))} # In case we need a stand-alone definition...

hosp_beds_ldi <- unlist(lapply(provider_capacity$p_hosp_beds, FUN=function(x){log((x/(1-x))/(pUS.1/(1-pUS.1)))}))
provider_capacity <- data.frame(provider_capacity, hosp_beds_ldi)
provider_capacity <- provider_capacity[match(states$NAME, provider_capacity$NAME),]
provider_capacity <- provider_capacity[1:52,]
states <- data.frame(states, "hosp_beds_ldi"=provider_capacity$hosp_beds_ldi) # Append to states

## COVID-19 Testing fixing
# colnames(state_covid_testing) <- c("NAME","total_num_tests","tests_pos_results")
# Inner join to add population
state_covid_testing <- left_join(state_covid_testing, population, by = c('NAME'))

# TODO: This is really just the test rate, not "per 1000"
state_covid_testing <- state_covid_testing %>% 
  mutate(tests_per_1000 = total_num_tests / Population)

pUS.2 <- as.numeric(state_covid_testing[which(state_covid_testing$NAME=="United States"),"tests_per_1000"])

pSK.2 <- 6768 / 1000000

#tests_ldi <- unlist(lapply(state_covid_testing$tests_per_1000, FUN=function(x){log((x/(1-x))/(pUS.2/(1-pUS.2)))}))
tests_ldi <- unlist(lapply(state_covid_testing$tests_per_1000, FUN=function(x){log((x/(1-x))/(pSK.2/(1-pSK.2)))}))

state_covid_testing <- data.frame(state_covid_testing, tests_ldi)

state_covid_testing <- state_covid_testing[match(states$NAME, state_covid_testing$NAME),]

state_covid_testing <- state_covid_testing[1:52,]

states <- data.frame(states, "tests_ldi"=state_covid_testing$tests_ldi) # Append to states

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

# RE-order to match states ordering
at_risk_adults <- at_risk_adults[match(states$NAME, at_risk_adults$NAME),]

at_risk_adults <- at_risk_adults[1:52,]

# Append the new columns to states
states <- data.frame(states, "at_risk_ldi"=at_risk_adults$at_risk_ldi) # Append to states

states <- data.frame(states, "older_at_risk_ldi"=at_risk_adults$older_at_risk_ldi) # Append to states

# Hypertension mortality
pUS.5 <- as.numeric(hypertension_mortality[which(hypertension_mortality$NAME=="United States"),"p_ht_death_rate"])

ht_death_rate_ldi <- unlist(lapply(hypertension_mortality$p_ht_death_rate, FUN=function(x){log((x/(1-x))/(pUS.5/(1-pUS.5)))}))

hypertension_mortality <- data.frame(hypertension_mortality, ht_death_rate_ldi)

# RE-order to match states ordering
hypertension_mortality <- hypertension_mortality[match(states$NAME, hypertension_mortality$NAME),]

hypertension_mortality <- hypertension_mortality[1:52,]

# Append the new column to states
states <- data.frame(states, "ht_death_rate_ldi"=hypertension_mortality$ht_death_rate_ldi) # Append to states

# COVID-19 Deaths per COVID-19 Case
pUS.6 <- as.numeric(covid_data_states[which(covid_data_states$NAME=="United States"),"p_deaths_cases"])

deaths_cases_ldi <- unlist(lapply(covid_data_states$p_deaths_cases, FUN=function(x){log((x/(1-x))/(pUS.6/(1-pUS.6)))}))

covid_data_states <- data.frame(covid_data_states, deaths_cases_ldi)

# RE-order to match states ordering
covid_data_states <- covid_data_states[match(states$NAME, covid_data_states$NAME),]

# Append the new column to states
covid_data_states <- covid_data_states[1:52,]

states <- data.frame(states, "deaths_cases_ldi"=covid_data_states$deaths_cases_ldi) # Append to states
