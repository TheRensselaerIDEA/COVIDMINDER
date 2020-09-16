setwd("allcause_state")

all_cause_02 <- read.table(file = 'Underlying Cause of Death, All Causes, State, 2000-2002.txt', 
                           sep = '\t', header = TRUE, stringsAsFactors = FALSE)
all_cause_02$Years <- '2000-2002'

all_cause_05 <- read.table(file = 'Underlying Cause of Death, All Causes, State, 2003-2005.txt', 
                           sep = '\t', header = TRUE, stringsAsFactors = FALSE)
all_cause_05$Years <- '2003-2005'

all_cause_08 <- read.table(file = 'Underlying Cause of Death, All Causes, State, 2006-2008.txt', 
                           sep = '\t', header = TRUE, stringsAsFactors = FALSE)
all_cause_08$Years <- '2006-2008'

all_cause_11 <- read.table(file = 'Underlying Cause of Death, All Causes, State, 2009-2011.txt', 
                           sep = '\t', header = TRUE, stringsAsFactors = FALSE)
all_cause_11$Years <- '2009-2011'

all_cause_14 <- read.table(file = 'Underlying Cause of Death, All Causes, State, 2012-2014.txt', 
                           sep = '\t', header = TRUE, stringsAsFactors = FALSE)
all_cause_14$Years <- '2012-2014'

all_cause_17 <- read.table(file = 'Underlying Cause of Death, All Causes, State, 2015-2017.txt', 
                           sep = '\t', header = TRUE, stringsAsFactors = FALSE)
all_cause_17$Years <- '2015-2017'

all_cause <- rbind(all_cause_02, all_cause_05, all_cause_08, all_cause_11, all_cause_14, all_cause_17)

all_cause$Cause <- 'All Cause'
all_cause$State[all_cause$Notes == 'Total'] <- 'United States'

setwd("..")

setwd("assault_state")

assault_02 <- read.table(file = 'Underlying Cause of Death, Assault, State, 2000-2002.txt', 
                           sep = '\t', header = TRUE, stringsAsFactors = FALSE)
assault_02$Years <- '2000-2002'

assault_05 <- read.table(file = 'Underlying Cause of Death, Assault, State, 2003-2005.txt', 
                           sep = '\t', header = TRUE, stringsAsFactors = FALSE)
assault_05$Years <- '2003-2005'

assault_08 <- read.table(file = 'Underlying Cause of Death, Assault, State, 2006-2008.txt', 
                           sep = '\t', header = TRUE, stringsAsFactors = FALSE)
assault_08$Years <- '2006-2008'

assault_11 <- read.table(file = 'Underlying Cause of Death, Assault, State, 2009-2011.txt', 
                           sep = '\t', header = TRUE, stringsAsFactors = FALSE)
assault_11$Years <- '2009-2011'

assault_14 <- read.table(file = 'Underlying Cause of Death, Assault, State, 2012-2014.txt', 
                           sep = '\t', header = TRUE, stringsAsFactors = FALSE)
assault_14$Years <- '2012-2014'

assault_17 <- read.table(file = 'Underlying Cause of Death, Assault, State, 2015-2017.txt', 
                           sep = '\t', header = TRUE, stringsAsFactors = FALSE)
assault_17$Years <- '2015-2017'

assault <- rbind(assault_02, assault_05, assault_08, assault_11, assault_14, assault_17)

assault$Cause <- 'Assault'
assault$State[assault$Notes == 'Total'] <- 'United States'

setwd("..")

setwd("cancer_state")

cancer_02 <- read.table(file = 'Underlying Cause of Death, Cancer, State, 2000-2002.txt', 
                         sep = '\t', header = TRUE, stringsAsFactors = FALSE)
cancer_02$Years <- '2000-2002'

cancer_05 <- read.table(file = 'Underlying Cause of Death, Cancer, State, 2003-2005.txt', 
                         sep = '\t', header = TRUE, stringsAsFactors = FALSE)
cancer_05$Years <- '2003-2005'

cancer_08 <- read.table(file = 'Underlying Cause of Death, Cancer, State, 2006-2008.txt', 
                         sep = '\t', header = TRUE, stringsAsFactors = FALSE)
cancer_08$Years <- '2006-2008'

cancer_11 <- read.table(file = 'Underlying Cause of Death, Cancer, State, 2009-2011.txt', 
                         sep = '\t', header = TRUE, stringsAsFactors = FALSE)
cancer_11$Years <- '2009-2011'

cancer_14 <- read.table(file = 'Underlying Cause of Death, Cancer, State, 2012-2014.txt', 
                         sep = '\t', header = TRUE, stringsAsFactors = FALSE)
cancer_14$Years <- '2012-2014'

cancer_17 <- read.table(file = 'Underlying Cause of Death, Cancer, State, 2015-2017.txt', 
                         sep = '\t', header = TRUE, stringsAsFactors = FALSE)
cancer_17$Years <- '2015-2017'

cancer <- rbind(cancer_02, cancer_05, cancer_08, cancer_11, cancer_14, cancer_17)

cancer$Cause <- 'Cancer'
cancer$State[cancer$Notes == 'Total'] <- 'United States'

setwd("..")

setwd("cardiovascular_state")

cardiovascular_02 <- read.table(file = 'Underlying Cause of Death, State, 2000-2002.txt', 
                        sep = '\t', header = TRUE, stringsAsFactors = FALSE)
cardiovascular_02$Years <- '2000-2002'

cardiovascular_05 <- read.table(file = 'Underlying Cause of Death, State, 2003-2005.txt', 
                        sep = '\t', header = TRUE, stringsAsFactors = FALSE)
cardiovascular_05$Years <- '2003-2005'

cardiovascular_08 <- read.table(file = 'Underlying Cause of Death, State, 2006-2008.txt', 
                        sep = '\t', header = TRUE, stringsAsFactors = FALSE)
cardiovascular_08$Years <- '2006-2008'

cardiovascular_11 <- read.table(file = 'Underlying Cause of Death, State, 2009-2011.txt', 
                        sep = '\t', header = TRUE, stringsAsFactors = FALSE)
cardiovascular_11$Years <- '2009-2011'

cardiovascular_14 <- read.table(file = 'Underlying Cause of Death, State, 2012-2014.txt', 
                        sep = '\t', header = TRUE, stringsAsFactors = FALSE)
cardiovascular_14$Years <- '2012-2014'

cardiovascular_17 <- read.table(file = 'Underlying Cause of Death, State, 2015-2017.txt', 
                        sep = '\t', header = TRUE, stringsAsFactors = FALSE)
cardiovascular_17$Years <- '2015-2017'

cardiovascular <- rbind(cardiovascular_02, cardiovascular_05, cardiovascular_08, cardiovascular_11, cardiovascular_14, cardiovascular_17)

cardiovascular$Cause <- 'Cardiovascular'
cardiovascular$State[cardiovascular$Notes == 'Total'] <- 'United States'

setwd("..")

setwd("despair_state")

despair_02 <- read.table(file = 'Underlying Cause of Death, Despair, State, 2000-2002.txt', 
                                sep = '\t', header = TRUE, stringsAsFactors = FALSE)
despair_02$Years <- '2000-2002'

despair_05 <- read.table(file = 'Underlying Cause of Death, Despair, State, 2003-2005.txt', 
                                sep = '\t', header = TRUE, stringsAsFactors = FALSE)
despair_05$Years <- '2003-2005'

despair_08 <- read.table(file = 'Underlying Cause of Death, Despair, State, 2006-2008.txt', 
                                sep = '\t', header = TRUE, stringsAsFactors = FALSE)
despair_08$Years <- '2006-2008'

despair_11 <- read.table(file = 'Underlying Cause of Death, Despair, State, 2009-2011.txt', 
                                sep = '\t', header = TRUE, stringsAsFactors = FALSE)
despair_11$Years <- '2009-2011'

despair_14 <- read.table(file = 'Underlying Cause of Death, Despair, State, 2012-2014.txt', 
                                sep = '\t', header = TRUE, stringsAsFactors = FALSE)
despair_14$Years <- '2012-2014'

despair_17 <- read.table(file = 'Underlying Cause of Death, Despair, State, 2015-2017.txt', 
                                sep = '\t', header = TRUE, stringsAsFactors = FALSE)
despair_17$Years <- '2015-2017'

despair <- rbind(despair_02, despair_05, despair_08, despair_11, despair_14, despair_17)

despair$Cause <- 'Despair'
despair$State[despair$Notes == 'Total'] <- 'United States'

setwd("..")

state_natl_death_rates <- rbind(all_cause, assault, cancer, cardiovascular, despair)
saveRDS(state_natl_death_rates, file = "state_natl_death_rates.Rds")
