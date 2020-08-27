#!/usr/bin/env bash

# LEFT TO RUN:
declare -a var_names=("\% Fair or Poor Health" "Average Number of Physically Unhealthy Days" "\% Smokers" "\% With Access to Exercise Opportunities" 
"\# Chlamydia Cases" "Chlamydia Rate" "\% Uninsured" "\# Primary Care Physicians" "\# Mental Health Providers" "Mental Health Provider Rate" "Mental Health Provider Ratio"
"Preventable Hosp. Rate \(White\)" "\% Screened \(White\)" "Labor Force" "\% Unemployed" "\# Households" "Average Daily PM2.5" "Presence of Water Violation" 
"\# Workers who Drive Alone" "\% Long Commute - Drives Alone"  "\% Frequent Physical Distress" "\# HIV Cases" "\# Food Insecure" "\# Limited Access" 
"\# Drug Overdose Deaths" "\# Motor Vehicle Deaths" "MV Mortality Rate \(White\)" "\% Insufficient Sleep" "\# Uninsured_1" "\% Uninsured_1" "\# Uninsured_2" "\% Uninsured_2" 
"Other Primary Care Provider Rate" "Average Grade Performance \(White\)_1" "Segregation index"  "\# Homeowners" "\# Households with Severe Cost Burden" 
"\# less than 18 years of age" "\% Not Proficient in English"  "All.Cause.death_rate" "infant_deaths" "suicide_deaths" "reclosure" "mask" "date_since_social" "date_since_reopen" 
"date_since_reclosure"  "beds" "date_since_mask" "date_since" "Assault.death_rate" "Cancer.death_rate" "Cardiovascular.death_rate" "Despair.death_rate" "PediatricAsthma" "AdultAsthma" 
"COPD" "AdultChronicLungDisease")


                                                  
fname="NBGWAS.R"


for var in "${var_names[@]}"; do {
  echo `Rscript ./GWAS/$fname $var`&
} done
