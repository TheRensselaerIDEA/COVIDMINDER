#!/usr/bin/env bash


# DO NOT RUN THIS SCRIPT WITHOUT CONSULTING TLB

# LEFT TO RUN:
declare -a var_names=()

# ALL STATES:
#"AL" "AR" "AZ" "CA" "CO"  "FL" "GA" "IA" "ID" "IL"  "KS" "KY"
#            "LA"  "MI" "MN" "MO" "MS" "MT"  "NE"  "NM" "NJ" "NV" 
#            "NY"  "OK" "OR" "PA"  "SD"  "TX" "UT" "VA"  "WA" "WI" "WV" 
# "CT" "DE" "IN" "MA" "MD" "ME" "NC" "ND" "NH" "OH" "SC" "TN" "VT" "WY"


# Not included:  ( bad pop data )
# CT, DE,  MA, MD, NH, NJ

# included but missing popdens[2]:
# "IN" "KY" "ME" "NC" "ND" "OH" "SC" "TN" "VT" "WY"

fname="states_NBGWAS.R"


for var in "${var_names[@]}"; do {
  echo `Rscript $fname $var`&
} done
