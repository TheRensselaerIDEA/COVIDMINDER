library(ggplot2)
library(tidyverse)
library(usmap)

state_covid_testing <-read.csv("state_covid_testing.csv")
state_covid_testing

mean(state_covid_testing$positive)
mean(state_covid_testing$negative)

#positive vs testsper1000
ggplot(filter(state_covid_testing, NAME %in% c("Montana", "Alaska", "Idaho","Florida","Nebraska","Missouri","Texas","Mississppi","Tennessee","Alabama","New Jersey","Connecticut","New York","Massachusetts","New Hampshire","Maine","Vermont","Rhode Island","Illinois","Michigan")),
       aes(x=tests_per_1000,
           y=positive,
           color=NAME))+
  geom_point()

#negative vs testsper1000
ggplot(filter(state_covid_testing, NAME %in% c("Montana", "Alaska", "Idaho","Florida","Nebraska","Missouri","Texas","Mississppi","Tennessee","Alabama","New Jersey","Connecticut","New York","Massachusetts","New Hampshire","Maine","Vermont","Rhode Island","Illinois","Michigan")),
       aes(x=tests_per_1000,
           y=negative,
           color=NAME))+
  geom_point()

#positve 
ggscatter(state_covid_testing, x = "tests_per_1000", y = "positive",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Tests per 1000", ylab = "Positive Cases")

#negative
ggscatter(state_covid_testing, x = "tests_per_1000", y = "negative",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Tests per 1000", ylab = "Negative Cases")


ggplot(state_covid_testing, aes(x = tests_per_1000, y = positive)) +
           geom_point()

rcorr(x, type="pearson") 
rcorr(as.matrix(state_covid_testing))

kruskal.test(x=state_covid_testing$tests_per_1000, g=state_covid_testing$positive)
kruskal.test(x=state_covid_testing$tests_per_1000, g=state_covid_testing$negative)

state_covid_mortality <- read.csv("state_covid_mortality.csv")
state_covid_mortality

ggplot(data=state_covid_mortality, aes(x=NAME, y=covid19_deaths))+geom_point()+  ylim(c(10, 8000))
cor.test(state_covid_mortality$covid19_cases, state_covid_mortality$covid19_deaths)

covid_NY_counties_deaths <- read.csv(file.choose())
NY_populations <- read.csv("NY_population.csv")

#NY_county_data <- read.csv("NY_county_data.csv")

myfulldata = merge(covid_NY_counties_deaths , NY_populations, by.x="county", by.y="County")

#deaths <- myfulldata$deaths
#populations <- myfulldata$Population

mortality_rate_sa = myfulldata$deaths/myfulldata$Population
myfulldata$mortality_rate <- c(mortality_rate_sa)

#plot of crude rate
ggplot(data=myfulldata, aes(x=county, y=mortality_rate))+geom_point()+  ylim(c(0, .004))

stadper100000 = mortality_rate_sa *100000
myfulldata$mortality_per_100000 <- c(stadper100000)

#plot of per100K rate
ggplot(data=myfulldata, aes(x=county, y=mortality_per_100000))+geom_point()+  ylim(c(0, 350))
ggplot(data=myfulldata, aes(x=county, y=mortality_per_100000))+geom_point()+  ylim(c(10, 350))

mean(myfulldata$mortality_per_100000)

#NY map for per100K rate
usmap::plot_usmap("counties", fill = "red", alpha = 0.25,
                  include = c("NY"))
  
plot_usmap("counties", myfulldata, values = "mortality_per_100000", 
            include = "NY") +
  ggplot2::scale_fill_continuous(low="green", high="red", guide=FALSE)
  
  scale_fill_continuous(low = "white", high = "red", name = "mortality rate", label=scales::comma)+
  labs(title="NY counties with mortality rate",subtitle="hi")+
  theme(legend.position = "right")

plot_usmap(regions = c("counties"),
           include = c("NY"), exclude = c(), data = data.frame(FIPS),
           values = "mortality_rate", theme = theme_map(), labels = FALSE,
           label_color = "black")

#quantile
qqnorm(myfulldata$mortality_per_100000)
qqline(myfulldata$mortality_per_100000, col="red")

#compare to obesity
obesity_data_counties <- read.csv("obesity_data_counties.csv")
obesity_data_counties

myfulldataweight = merge(myfulldata , obesity_data_counties, by="FIPS")

cor.test(myfulldataweight$mortality_per_100000,myfulldataweight$X..Adults.with.Obesity)
plot(myfulldataweight$X..Adults.with.Obesity, myfulldataweight$mortality_per_100000)
lm(myfulldataweight)

#compare to chronic respiratory diseases
chronicrespiratorydiseases_counties <- read.csv("chronicrespiratorydiseases_counties.csv")
chronicrespiratorydiseases_counties

myfulldataresp = merge(myfulldata , chronicrespiratorydiseases_counties, by="FIPS")

cor.test(myfulldataresp$mortality_per_100000,myfulldataresp$MortalityRate2014)
plot(myfulldataresp$MortalityRate2014, myfulldataresp$mortality_per_100000, xlab="Chronic Respiratory Mortality Rate", ylab="COVID Mortality Rate")
lm(myfulldataresp$mortality_per_100000~myfulldataresp$MortalityRate2014)

##chronic resp vs states 
covid_data_states<- read.csv("covid_data_states.csv")

dataforstatesresp = merge(covid_data_states, chronicrespiratorydiseases_counties, by.x="name", by.y="CNTY")

stateper100000 = dataforstatesresp$Mortality_rate *100000
dataforstatesresp$mortality_per_100K <- c(stateper100000)

cor.test(dataforstatesresp$mortality_per_100K,dataforstatesresp$MortalityRate2014, xlab ="Chronic Respiratory Mortality Rate", ylab="COVID Mortality Rate")
plot(dataforstatesresp$MortalityRate2014, dataforstatesresp$mortality_per_100K,xlab ="Chronic Respiratory Disease Mortality Rate", ylab ="COVID Mortality Rate")

#10 best and worst
ggplot(filter(dataforstatesresp, name %in% c("Montana", "Alaska", "Idaho","Florida","Nebraska","Missouri","Texas","Mississppi","Tennessee","Alabama","New Jersey","Connecticut","New York","Massachusetts","New Hampshire","Maine","Vermont","Rhode Island","Illinois","Michigan")),
       aes(x=MortalityRate2014,
           y=mortality_per_100K,
           color=name))+
  geom_point()

#random
ggplot(filter(dataforstatesresp, name %in% c("Florida","California","New Jersey","Connecticut","New York")),
       aes(x=MortalityRate2014,
           y=mortality_per_100K,
           color=name))+
  geom_point()

#3 smallest/greatest population
ggplot(filter(dataforstatesresp, name %in% c("Wyoming","Vermont","Alaska","California","Texas", "Florida")),
       aes(x=MortalityRate2014,
           y=mortality_per_100K,
           color=name))+
  geom_point()

#asthma
lungdiseaseestimates_uscounties <- read.csv("lungdiseaseestimates_uscounties.csv")

asthmacomparison = merge(myfulldata , lungdiseaseestimates_uscounties, by="FIPS")

#will not consider one of the variables as integers
asthmaadultrate = asthmacomparison$AdultAsthma/asthmacomparison$Population


asthmaadultper100K = asthmaadultrate *100000
asthmacomparison$adultasthma_per_100K <- c(asthmaadultper100K)