# TemporalResults/NationalModel
This is the folder containing results of temporal analysis conducted with national model.

# MM-DD.rda

Files in this form are national model results using data at that date.

# All_*.rds

[`ALL_C.rds`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/TemporalResults/NationalModel/ALL_C.rds): Dataframe containing MRR value of all variables over all dates.

[`ALL_C_sig.rds`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/TemporalResults/NationalModel/ALL_C_sig.rds): Dataframe containing MRR value of only significant variables over all dates.

[`ALL_P.rds`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/TemporalResults/NationalModel/ALL_P.rds): Dataframe containing p-value of all variables over all dates.

[`ALL_P_sig.rds`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/TemporalResults/NationalModel/ALL_P_sig.rds): Dataframe containing p-value of only significant variables over all dates.

# Loader

[`Loader.R`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/TemporalResults/NationalModel/Loader.R): file that read in MM-DD.rda files to create ALL_*.rds files.

# Visualization

[`figure.rmd`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/TemporalResults/NationalModel/figure.rmd): file that read ALL_*.rds files to create plots.