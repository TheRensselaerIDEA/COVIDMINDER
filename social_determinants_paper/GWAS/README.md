# GWAS

This folder contains codes and results of Social Determinants Analysis.

# National Social Determinants Analysis

1. [`NBGWAS.rmd`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/GWAS/NBGWAS.rmd) contains code and explanation of national social determinants analysis. *Running time of this file is approximatedly 7 hours.

2. [`NBGWAS.R`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/GWAS/NBGWAS.R) contains only the code and is designed to run with [`parasdwas.sh`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/GWAS/parasdwas.sh) to shorten running time(~2H).

3. [`GWAS_P.rds`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/GWAS/GWAS_P.rds) contains p-values(raw).
[`GWAS_ADJ_P.rds`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/GWAS/GWAS_ADJ_P.rds) contains adjusted p-values using Benjamini-Hochberg. 
[`GWAS_MRR.rds`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/GWAS/GWAS_MRR.rds) contains MRRs.

# States Social Determinants Analysis

1. [`states_NBGWAS.rmd`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/GWAS/states_NBGWAS.rmd) contains code and explanation of states social determinants analysis.

2. [`states_NBGWAS.R`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/GWAS/states_NBGWAS.R) contains only the code and is designed to run with [`sparanbsdwas.sh`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/GWAS/sparanbsdwas.sh) to shorten running time.

3. [`state_results`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/GWAS/state_results) contains results of all states social determinants analysis. 

# Usage of parallelism

When using bash scripts, type in commands below in terminal.

```shell
bash ./social_determinants_paper/GWAS/parasdwas.sh
```
```shell
bash ./social_determinants_paper/GWAS/sparanbsdwas.sh
```