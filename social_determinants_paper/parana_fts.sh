#!/usr/bin/env bash

# declare dates we want to go through
# mask: 4/10   |   reopen: 4/24   |   reclosure: 6/26
# declare -a dates_names=("03-29")
# declare -a dates_names=("04-12" "04-19")
# declare -a dates_names=("04-26" "05-03" "05-10" "05-17" "05-24" "05-31" "06-07" "06-14" "06-21")
# declare -a dates_names=("06-28" "07-05" "07-12")
# declare -a dates_names=("03-29" "04-05" "04-12" "04-19" "04-26" "05-03" "05-10" "05-17" "05-24" "05-31" "06-07" "06-14" "06-21" "06-28" "07-05" "07-12")
# declare -a dates_names=("03-30" "04-06")
# declare -a dates_names=("04-13" "04-20")
# declare -a dates_names=("04-27" "05-04" "05-11" "05-18" "05-25" "06-01" "06-08" "06-15" "06-22")
# declare -a dates_names=("06-29" "07-06" "07-13")
declare -a dates_names=("03-29" "04-05" "04-12" "04-19" "04-26" "05-03" "05-10" "05-17" "05-24" "05-31" "06-07" "06-14" "06-21" "06-28" "07-05" "07-12" "03-30" "04-06" "04-13" "04-20" "04-27" "05-04" "05-11" "05-18" "05-25" "06-01" "06-08" "06-15" "06-22" "06-29" "07-06" "07-13")

# declare R files we want to run
fname="Preprocessing.R"

# Preprocessing
for date in "${dates_names[@]}"; do {
  echo `Rscript ./social_determinants_paper/Preprocessing/$fname  $date`&
} done

# Analyses
# for date in "${dates_names[@]}"; do {
#   echo `Rscript ./social_determinants_paper/Analyses/$fname  ./Preprocessing_FTS_Outputs/$date\-2020data.Rds`&
# } done


# Visualization
# echo `Rscript ./social_determinants_paper/Auto_Figures.R ${dates_names[@]}`

# echo `Rscript Figure.R`