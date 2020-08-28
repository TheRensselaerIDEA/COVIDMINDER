knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = "./")

source("Modules/Source.R")

GWAS_MRR <- readRDS("GWAS/GWAS_MRR.rds")

GWAS_P <- readRDS("GWAS/GWAS_P.rds")
GWAS_ADJ_P <- readRDS("GWAS/GWAS_ADJ_P.rds")

saveRDS(GWAS_ADJ_P, "GWAS/GWAS_ADJ_P.rds")
saveRDS(GWAS_P, "GWAS/GWAS_P.rds")
saveRDS(GWAS_MRR, "GWAS/GWAS_MRR.rds")