#!/usr/bin/env bash

declare -a var_names=("Infant Mortality Rate" "Cancer.death_rate" "Suicide Rate (Age-Adjusted)" "Assault.death_rate" "Despair.death_rate" "Motor Vehicle Mortality Rate"
                          "Drug Overdose Mortality Rate" "All.Cause.death_rate" "PediatricAsthma" "AdultChronicLungDisease"
                      "% Not Proficient in English" "% Insufficient Sleep" "% Unemployed" "% Drive Alone to Work" "% Long Commute - Drives Alone"
                          "% Food Insecure" "% With Access to Exercise Opportunities" "% Frequent Mental Distress" "% Smokers" "% Excessive Drinking"
                          "Overcrowding" "% less than 18 years of age" "% Homeowners" "% Severe Housing Cost Burden"
                          "Average Number of Physically Unhealthy Days" "Social Association Rate" "Segregation index" "Average Daily PM2.5"
                          "Presence of Water Violation" "Average Grade Performance" "High School Graduation Rate"
                      "Preventable Hospitalization Rate" "Primary Care Physicians Rate" "Other Primary Care Provider Rate" "% With Annual Mammogram"
                          "% Uninsured" "% Fair or Poor Health" "% Vaccinated" "Chlamydia Rate" "Mental Health Provider Rate" "HIV Prevalence Rate")

fname="NBGWAS.R"


for var in "${var_names[@]}"; do {
  echo `Rscript ./social_determinants_paper/GWAS/$fname $var`&
} done
