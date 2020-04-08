#### Preprocessing

# Calculate hypertension death rate per 100000
cardio_deaths_2017$cardio_deaths_p_100000 <-cardio_deaths_2017$cardio_deaths_p_100000 / 100000

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
hosp_beds_ldi <- unlist(lapply(provider_capacity$p_hosp_beds, FUN=function(x){-log(x/pIT.1)}))

provider_capacity <- data.frame(provider_capacity, hosp_beds_ldi)
provider_capacity <- provider_capacity[match(states$NAME, provider_capacity$NAME),]
provider_capacity <- provider_capacity[1:51,]
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
# tests_ldi <- unlist(lapply(state_covid_testing$tests_per_1000, FUN=function(x){log((x/(1-x))/(pSK.2/(1-pSK.2)))}))
tests_ldi <- unlist(lapply(state_covid_testing$tests_per_1000, FUN=function(x){-log(x/pSK.2)}))

state_covid_testing <- data.frame(state_covid_testing, tests_ldi)

state_covid_testing <- state_covid_testing[match(states$NAME, state_covid_testing$NAME),]

state_covid_testing <- state_covid_testing[1:51,]

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

at_risk_adults <- at_risk_adults[1:51,]

# Append the new columns to states
states <- data.frame(states, "at_risk_ldi"=at_risk_adults$at_risk_ldi) # Append to states

states <- data.frame(states, "older_at_risk_ldi"=at_risk_adults$older_at_risk_ldi) # Append to states

# Cardio mortality (NEW)
pUS.5 <- as.numeric(cardio_deaths_2017[which(cardio_deaths_2017$NAME=="United States"),"cardio_deaths_p_100000"])

#cardio_death_rate_ldi <- unlist(lapply(cardio_deaths_2017$cardio_deaths_p_100000, FUN=function(x){log((x/(1-x))/(pUS.5/(1-pUS.5)))}))
cardio_death_rate_ldi <- unlist(lapply(cardio_deaths_2017$cardio_deaths_p_100000, FUN=function(x){-log(pUS.5/x)}))

cardio_deaths_2017 <- data.frame(cardio_deaths_2017, cardio_death_rate_ldi)

# RE-order to match states ordering
cardio_deaths_2017 <- cardio_deaths_2017[match(states$NAME, cardio_deaths_2017$NAME),]

cardio_deaths_2017 <- cardio_deaths_2017[1:51,]

# Append the new column to states
states <- data.frame(states, "cardio_death_rate_ldi"=cardio_deaths_2017$cardio_death_rate_ldi) # Append to states

# COVID-19 Deaths per COVID-19 Case
pUS.6 <- as.numeric(covid_data_states[which(covid_data_states$NAME=="United States"),"p_death_rate"])

#death_rate_ldi <- unlist(lapply(covid_data_states$p_death_rate, FUN=function(x){log((x/(1-x))/(pUS.6/(1-pUS.6)))}))
death_rate_ldi <- unlist(lapply(covid_data_states$p_death_rate, FUN=function(x){-log(pUS.6/x)}))

covid_data_states <- data.frame(covid_data_states, death_rate_ldi)

# RE-order to match states ordering
covid_data_states <- covid_data_states[match(states$NAME, covid_data_states$NAME),]

# Append the new column to states
covid_data_states <- covid_data_states[1:51,]

states <- data.frame(states, "death_rate_ldi"=covid_data_states$death_rate_ldi) # Append to states

# NY specific calculations: Death Rates
pNY.6.deaths <- sum(NY.data$deaths)/sum(NY.data$Population)
pNY.6.cases <- sum(NY.data$cases)/sum(NY.data$Population)
  
NY.data <- transform(NY.data, death_rate = deaths/Population)
#NY.data <- transform(NY.data, case_rate = cases/Population) # We already do this

NY.data <- NY.data %>% 
  filter(!County == c("Out of NY","Unassigned"))

NY.data$death_rate_ldi <- unlist(lapply(NY.data$death_rate, FUN=function(x){-log(pNY.6.deaths/x)}))
#NY.data$death_rate_ldi[NY.data$deaths == 0] <- NA

NY.data$case_rate_ldi <- unlist(lapply(NY.data$case_rate, FUN=function(x){-log(pNY.6.cases/x)}))
#NY.data$case_rate_ldi[NY.data$cases == 0] <- NA
