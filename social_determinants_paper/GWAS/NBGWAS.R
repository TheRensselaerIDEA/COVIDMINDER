#############

knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = "./")

source("./social_determinants_paper/Modules/Source.R")

#------------------------------------------------------------------------------------------------------------------------------------------
# Uncomment and run this chunk of code before GWAS to reset/format the destination file

# GWAS_MRR <- readRDS("./social_determinants_paper/GWAS/GWAS_MRR.rds")
# GWAS_P <- readRDS("./social_determinants_paper/GWAS/GWAS_P.rds")
# GWAS_ADJ_P <- readRDS("./social_determinants_paper/GWAS/GWAS_ADJ_P.rds")
# 
# GWAS_MRR <- subset(GWAS_MRR, select = c())
# GWAS_P <- subset(GWAS_P, select = c())
# GWAS_ADJ_P <- subset(GWAS_ADJ_P, select = c())
# 
# names = c("Infant Mortality Rate", "Cancer.death_rate", "Suicide Rate (Age-Adjusted)", "Assault.death_rate", "Despair.death_rate", "Motor Vehicle Mortality Rate",
#           "Drug Overdose Mortality Rate", "All.Cause.death_rate", "PediatricAsthma", "AdultChronicLungDisease", "% Not Proficient in English",
#           "% Insufficient Sleep", "% Unemployed", "% Drive Alone to Work", "% Long Commute - Drives Alone", "% Food Insecure",
#           "% With Access to Exercise Opportunities", "% Frequent Mental Distress", "% Smokers", "% Excessive Drinking", "Overcrowding",
#           "% less than 18 years of age", "% Homeowners", "% Severe Housing Cost Burden", "Average Number of Physically Unhealthy Days",
#           "Social Association Rate", "Segregation index", "Average Daily PM2.5", "Presence of Water Violation", "Average Grade Performance",
#           "High School Graduation Rate", "Preventable Hospitalization Rate", "Primary Care Physicians Rate", "Other Primary Care Provider Rate",
#           "% With Annual Mammogram", "% Uninsured", "% Fair or Poor Health", "% Vaccinated", "Chlamydia Rate", "Mental Health Provider Rate",
#           "HIV Prevalence Rate")
# 
# for (name in names) {
#   GWAS_P$placeholder_name <- NA
#   GWAS_ADJ_P$placeholder_name <- NA
#   GWAS_MRR$placeholder_name <- NA
#   names(GWAS_P)[names(GWAS_P) == "placeholder_name"] <- name
#   names(GWAS_ADJ_P)[names(GWAS_ADJ_P) == "placeholder_name"] <- name
#   names(GWAS_MRR)[names(GWAS_MRR) == "placeholder_name"] <- name
# }
# 
# saveRDS(GWAS_ADJ_P, "./social_determinants_paper/GWAS/GWAS_ADJ_P.rds")
# saveRDS(GWAS_P, "./social_determinants_paper/GWAS/GWAS_P.rds")
# saveRDS(GWAS_MRR, "./social_determinants_paper/GWAS/GWAS_MRR.rds")

#------------------------------------------------------------------------------------------------------------------------------------------

## Get variable in the loop
args <- commandArgs()

interested_var_s <- c(args[6:length(args)])
interested_var = ""
start = 1
for (i in interested_var_s) {
  if (start == 1) {
    interested_var <- paste(interested_var, i, sep = "")
    start = 0
  } else {
    interested_var <- paste(interested_var, i, sep = " ")
  }
}
interested_var <- str_remove_all(interested_var, "[\\\\]")
cat(interested_var)


sampledata <- readRDS('./social_determinants_paper/Preprocessing_FTS_Outputs/07-05-2020data.Rds')

s = paste("INTERESTED VAR = " , interested_var, "\n", sep = "")
cat(s)

sub_sampledata <- subset(sampledata, select = c ("Deaths","hispanic", "pct_blk", "pct_asian", "pct_white", "pct_native", "q_popdensity", "medhouseholdincome", 
                                                 "education", "beds", "population", "date_since", "date_since_mask", "State", interested_var))

colnames(sub_sampledata)[ncol(sub_sampledata)] = "i_var"


if (strcmp(unname(sapply(sub_sampledata, typeof)[ncol(sub_sampledata)]), "character")) {
  s = paste("starting model with : ", interested_var, "\n", sep="")
  cat(s)
  In.loop.model=glmer.nb(Deaths ~ scale(hispanic) + scale(pct_blk) + scale(pct_asian) + scale(pct_white) + scale(pct_native)
                         + factor(q_popdensity)
                         + scale(log(medhouseholdincome))+scale(education) + scale(beds/population)
                         + scale(date_since) 
                         + scale(date_since_mask)
                         + factor(i_var)
                         + (1|State)
                         + offset(log(population)), data = sub_sampledata)
} else {
  s = paste("starting model with : ", interested_var, "\n", sep="")
  cat(s)
  In.loop.model=glmer.nb(Deaths ~ scale(hispanic) + scale(pct_blk) + scale(pct_asian) + scale(pct_white) + scale(pct_native)
                         + factor(q_popdensity)
                         + scale(log(medhouseholdincome))+scale(education) + scale(beds/population)
                         + scale(date_since) 
                         + scale(date_since_mask)
                         + scale(i_var)
                         + (1|State)
                         + offset(log(population)), data = sub_sampledata)
}

GWAS_MRR <- readRDS("./social_determinants_paper/GWAS/GWAS_MRR.rds")
GWAS_P <- readRDS("./social_determinants_paper/GWAS/GWAS_P.rds")
GWAS_ADJ_P <- readRDS("./social_determinants_paper/GWAS/GWAS_ADJ_P.rds")

# Interleaving here between threads could leave some columns out... make sure to check after para. done

GWAS_MRR[[interested_var]]   <- summary(In.loop.model)[10]$coefficients[2:16,1]
GWAS_P[[interested_var]]     <- summary(In.loop.model)[10]$coefficients[2:16,4]

GWAS_ADJ_P[[interested_var]] <- p.adjust(summary(In.loop.model)[10]$coefficients[2:16,4], 
                                         method = 'BH', 
                                         n = length(summary(In.loop.model)[10]$coefficients[2:16,4]))


print("SAVED")
saveRDS(GWAS_ADJ_P, "./social_determinants_paper/GWAS/GWAS_ADJ_P.rds")
saveRDS(GWAS_P, "./social_determinants_paper/GWAS/GWAS_P.rds")
saveRDS(GWAS_MRR, "./social_determinants_paper/GWAS/GWAS_MRR.rds")