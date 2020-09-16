knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = "./")

source("./social_determinants_paper/Modules/Source.R")

dates_names = c("03-29", "04-05", "04-12", "04-19", "04-26", "05-03", "05-17", "05-24", "05-31", "06-07", "06-14", "06-21", "06-28", "07-05", "07-12")
# dates_names = c("03-30", "04-06", "04-13", "04-20", "04-27", "05-04", "05-11", "05-18", "05-25", "06-01", "06-08", "06-15", "06-22", "06-29", "07-06", "07-13")

COEF = c("(Intercept)",
         "scale(hispanic)","scale(pct_blk)","scale(pct_asian)","scale(pct_white)","scale(pct_native)",
         "factor(q_popdensity)2","factor(q_popdensity)3","factor(q_popdensity)4","factor(q_popdensity)5",
         "scale(log(`Median Household Income`))",
         "scale(date_since_social)","scale(date_since)","scale(date_since_reopen)","scale(date_since_reclosure)","scale(date_since_mask)",
         "scale(pct_obesity)","scale(pct_age65)","scale(pct_diabetes)",
         "scale(LungCancer)","scale(COPD)","scale(AdultAsthma)","scale(PediatricAsthma)",
         "scale(All.Cause.death_rate)")

COEF_1 = c("(Intercept)",
         "Percent Hispanic","Percent African American","Percent Asian","Percent White","Percent Native American",
         "1st Quartile of Population Density","2nd Quartile of Population Density","3rd Quartile of Population Density","4th Quartile of Population Density",
         "Median Household Income",
         "Days since social isolation","Days Since First Infection","Days since reopening","Days since reclosure","Days Since Mask Required",
         "Percent Obese","Percent Age 65 and Older","Percent Diabetic",
         "Percent with Lung Cancer","Percent with COPD","Percent Adult Asthma","Percent Pediatric Asthma",
         "Pre-COVID All Cause Mortality Rate")

ALL_C = data.frame(coefficients = COEF)
ALL_P = data.frame(coefficients = COEF)
ALL_sig = data.frame(coefficients = COEF)

for (i in 1:length(dates_names)){
  fname <- paste("./social_determinants_paper/TemporalResults/NationalModel/",dates_names[i],".rda",sep="")
  load(fname)
  summ <- eval(as.name(dates_names[i]))
  c <- exp(summ[10]$coefficients[,1])
  p <- summ[10]$coefficients[,4]
  p <- p.adjust(p, method = 'BH', n = length(p))
  
  # merge into master data frame
  ALL_C$date <- c[match(ALL_C$coefficients, names(c))]
  names(ALL_C)[names(ALL_C) == 'date'] <- dates_names[i]
  ALL_P$date <- p[match(ALL_P$coefficients, names(p))]
  names(ALL_P)[names(ALL_P) == 'date'] <- dates_names[i]
}

ALL_C$coefficients = COEF_1
ALL_P$coefficients = COEF_1

ALL_C_sig <- data.frame(coefficients = COEF_1)
ALL_P_sig <- data.frame(coefficients = COEF_1)
for (i in 1:length(dates_names)){
  column_C <- c()
  column_P <- c()
  for (j in 1:length(COEF_1)){
    c <- ALL_C[[dates_names[i]]][j]
    p <- ALL_P[[dates_names[i]]][j]
    if (!is.na(p) & p <= 0.05){
      column_C <- append(column_C, c, after = length(column_C))
      column_P <- append(column_P, p, after = length(column_P))
    }else{
      column_C <- append(column_C, NA, after = length(column_C))
      column_P <- append(column_P, NA, after = length(column_P))
    }
  }
  ALL_C_sig$date <- column_C
  names(ALL_C_sig)[names(ALL_C_sig) == 'date'] <- dates_names[i]
  ALL_P_sig$date <- column_P
  names(ALL_P_sig)[names(ALL_P_sig) == 'date'] <- dates_names[i]
}

ALL_C_sig <- ALL_C_sig[rowSums(is.na(ALL_C_sig)) != ncol(ALL_C_sig)-1, ]
ALL_P_sig <- ALL_P_sig[rowSums(is.na(ALL_P_sig)) != ncol(ALL_P_sig)-1, ]


saveRDS(ALL_C, file = './social_determinants_paper/TemporalResults/NationalModel/ALL_C.rds')
saveRDS(ALL_P, file = './social_determinants_paper/TemporalResults/NationalModel/ALL_P.rds')

saveRDS(ALL_C_sig, file = './social_determinants_paper/TemporalResults/NationalModel/ALL_C_sig.rds')
saveRDS(ALL_P_sig, file = './social_determinants_paper/TemporalResults/NationalModel/ALL_P_sig.rds')