knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = "../")

source("./social_determinants_paper/Modules/Source.R")

# set datafile from parallelism
# args <- commandArgs()
# datafile = args[6]

# set datefile by hand
datafile = './social_determinants_paper/Preprocessing_FTS_Outputs/07-05-2020data.Rds'

aggregate_pm_census_cdc_test_beds_age_diabete_obesity_heart <- readRDS(datafile)

combined.mode.nb.random.off.main = glmer.nb(Deaths 
                                            ~ scale(hispanic) + scale(pct_blk) + scale(pct_asian) + scale(pct_white) + scale(pct_native)
                                            + factor(q_popdensity)
                                            + scale(log(medhouseholdincome)) 
                                            + scale(education) 
                                            + scale(beds/population)
                                            + scale(date_since_social)
                                            + scale(date_since)
                                            + scale(date_since_reopen)
                                            + scale(date_since_reclosure)
                                            + scale(date_since_mask)
                                            + scale(pct_obesity)
                                            + scale(pct_age65) 
                                            + scale(pct_diabetes)
                                            + scale(LungCancer)
                                            + scale(COPD)
                                            + scale(AdultAsthma)
                                            + scale(Cardiovascular.death_rate)
                                            + (1|state)
                                            + offset(log(population)), data = aggregate_pm_census_cdc_test_beds_age_diabete_obesity_heart, 
                                            )
# save results in parallelism
date = substr(datafile, 49,53)
assign(date, summary(combined.mode.nb.random.off.main))
fname = paste("./social_determinants_paper/TemporalResults/NationalModel/",date,".rda",sep="")
do.call(save, list(date, file=fname))
