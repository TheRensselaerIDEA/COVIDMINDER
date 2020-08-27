#############

#Nationwide GWAS with Negative Binomial Mixed Model
setwd("/data/Social_Determinants")

###dependencies
library("MASS")
library("lme4")
library('caret')
library('blmeco')
library(VineCopula)
library(sgof)
library(tidyverse)
library(cvms)
library(pracma)


#source("Modules/Source.R")
#source("GWAS/helper.R")

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

  
sampledata<-readRDS('Preprocessing_FTS_Outputs/07-12-2020data.Rds')

#for (name in colnames(sampledata)) {
#x <- c(34,35,43,47,52,53,61,62,68,69,70,76,82,94,95,106,119,120, 131,132,153,160,163,165,167,169,176,182,183, 184, 185, 186, 187, 188,200,208,216,236,238,240,256,288, 289, 
#       290, 291, 292, 293, 294, 295, 296, 297, 298, 299, 300, 301, 302, 303, 304)
#print(length(x))
#for ( i in x) {
#  name = colnames(sampledata)[i]
#  s <- paste("\"",name, "\" ", sep = "")
#  cat(s)
#}



s = paste("INTERESTED VAR = " , interested_var, "\n", sep = "")
cat(s)

sub_sampledata <- subset(sampledata, select = c ("Deaths","hispanic", "pct_blk", "pct_asian", "pct_white", "pct_native", "q_popdensity", "Median Household Income", 
                                                 "education", "beds", "population", "date_since", "date_since_mask", "State", interested_var))

colnames(sub_sampledata)[ncol(sub_sampledata)] = "i_var"


if (strcmp(unname(sapply(sub_sampledata, typeof)[ncol(sub_sampledata)]), "character")) {
    s = paste("starting model with : ", interested_var, "\n", sep="")
    cat(s)
    In.loop.model=glmer.nb(Deaths ~ scale(hispanic) + scale(pct_blk) + scale(pct_asian) + scale(pct_white) + scale(pct_native)
                       + factor(q_popdensity)
                       + scale(log(`Median Household Income`))+scale(education) + scale(beds/population)
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
                       + scale(log(`Median Household Income`))+scale(education) + scale(beds/population)
                       + scale(date_since) 
                       + scale(date_since_mask)
                       + scale(i_var)
                       + (1|State)
                       + offset(log(population)), data = sub_sampledata)
}

GWAS_MRR <- readRDS("GWAS/GWAS_MRR.rds")

GWAS_P <- readRDS("GWAS/GWAS_P.rds")
GWAS_ADJ_P <- readRDS("GWAS/GWAS_ADJ_P.rds")

# Interleaving here between threads could leave some columns out... make sure to check after para. done

GWAS_MRR[[interested_var]]   <- summary(In.loop.model)[10]$coefficients[2:16,1]
GWAS_P[[interested_var]]     <- summary(In.loop.model)[10]$coefficients[2:16,4]

GWAS_ADJ_P[[interested_var]] <- p.adjust(summary(In.loop.model)[10]$coefficients[2:16,4], 
                                     method = 'BH', 
                                     n = length(summary(In.loop.model)[10]$coefficients[2:16,4]))


print("SAVED")
saveRDS(GWAS_ADJ_P, "GWAS/GWAS_ADJ_P.rds")
saveRDS(GWAS_P, "GWAS/GWAS_P.rds")
saveRDS(GWAS_MRR, "GWAS/GWAS_MRR.rds")


