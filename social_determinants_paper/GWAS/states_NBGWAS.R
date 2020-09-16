knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = "../")

source("./social_determinants_paper/Modules/Source.R")

args <- commandArgs()

state <- c(args[6:length(args)])

sampledata<-readRDS('./social_determinants_paper/Preprocessing_FTS_Outputs/07-12-2020data.Rds')
statesplit <- split(sampledata, sampledata$state)


# Ignore states with less than 2 counties
for (name in names(statesplit)) {
  if (nrow(statesplit[[name]]) < 2){
    statesplit <- statesplit[names(statesplit) != name]
  }
}

for (i in 1:length(names(statesplit))) {
  name = names(statesplit)[i]
  if (strcmp(name, state) == FALSE) {
    next
  }
  
  #statesplit[[state]]$`YPLL Rate (AIAN)`
    
  for (interested_var in colnames(statesplit[[name]]) ) {
    
    if (interested_var %in%  c ("Deaths","hispanic", "pct_blk", "pct_asian", "pct_white", "pct_native", "q_popdensity", "Median Household Income", 
                                "education", "beds", "population", "date_since", "date_since_mask", "fips")) {
      next
    }
  
    sub_sampledata <- subset(statesplit[[name]], select = c ("Deaths","hispanic", "pct_blk", "pct_asian", "pct_white", "pct_native", "q_popdensity", "Median Household Income", 
                                                   "education", "beds", "population", "date_since", "date_since_mask", "State", interested_var))
  
  
    colnames(sub_sampledata)[ncol(sub_sampledata)] = "i_var"
    
    if (strcmp(unname(sapply(sub_sampledata, typeof)[ncol(sub_sampledata)]), "character")) {
      next
      s = paste("starting ", state, " model with : ", interested_var, " (factoring) \n", sep="")
      cat(s)
      In.loop.model=glm.nb(Deaths ~ scale(hispanic) + scale(pct_blk) + scale(pct_asian) + scale(pct_white) + scale(pct_native)
                             + factor(q_popdensity)
                             + scale(log(`Median Household Income`))+scale(education) + scale(beds/population)
                           #  + scale(date_since) 
                           #  + scale(date_since_mask)
                             + factor(i_var)
                             #+ (1|State)
                             + offset(log(population)), data = sub_sampledata)
    } else {
      s = paste("starting ", state, " model with : ", interested_var, " (scaling) \n", sep="")
      cat(s)
      if (any(is.na(sub_sampledata$i_var))) {
        s = paste("skipping ", state, " model with : ", interested_var, " some values are NA \n", sep="")
        cat(s)
        next
      } else if (all(sub_sampledata$i_var == sub_sampledata$i_var[1]) )   {
        s = paste("skipping ", state, " model with : ", interested_var, " all values are the same \n", sep="")
        cat(s)
        next
      }
      In.loop.model=glm.nb(Deaths ~ scale(hispanic) + scale(pct_blk) + scale(pct_asian) + scale(pct_white) + scale(pct_native)
                             + factor(q_popdensity)
                             + scale(log(`Median Household Income`))+scale(education) + scale(beds/population)
                            # + scale(date_since) 
                            # + scale(date_since_mask)
                             + scale(i_var)
                           #  + (1|State)
                             + offset(log(population)), data = sub_sampledata)
    }
    
    
    
    MRR_file = paste("./social_determinants_paper/GWAS/state_results/", state, "_GWAS_MRR.rds", sep = "")
    P_file = paste("./social_determinants_paper/GWAS/state_results/", state, "_GWAS_P.rds", sep = "")
    ADJ_P_file = paste("./social_determinants_paper/GWAS/state_results/", state, "_GWAS_ADJ_P.rds", sep = "")
    
    GWAS_MRR <- readRDS(MRR_file)
    GWAS_P <- readRDS(P_file)
    GWAS_ADJ_P <- readRDS(ADJ_P_file)
    
    # Interleaving here between threads could leave some columns out... make sure to check after para. done
    
    GWAS_MRR[[interested_var]]   <- exp(summary(In.loop.model)$coefficients[2:13,1])
    GWAS_P[[interested_var]]     <- summary(In.loop.model)$coefficients[2:13,4]
    
    GWAS_ADJ_P[[interested_var]] <- p.adjust(summary(In.loop.model)$coefficients[2:13,4], 
                                             method = 'BH', 
                                             n = length(summary(In.loop.model)$coefficients[2:13,4]))
    
    
    saveRDS(GWAS_MRR, MRR_file)
    saveRDS(GWAS_P, P_file)
    saveRDS(GWAS_ADJ_P, ADJ_P_file)
  }
}
  

