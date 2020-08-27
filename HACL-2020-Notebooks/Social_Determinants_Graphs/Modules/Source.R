##### Load packages ####

# List of packages used:
# tidyverse
# sp
# raster
# dplyr
# sf
# stringr
# ggplot2
# grid
# pBrackets
# gridExtra
# lme4
# maps
# glmmTMB
# gamm4
# MASS
# RCurl
# httr
# data.table
# devtools
# NSAPHutils  # For threading w/ big data
# NSAPHplatform
# lubridate 
# eatGet      # Saving glmer models to memory      # does not work in this version of R
#~~~~~~~~~~~~~~~ From Shayom's code
# blmeco
# VineCopula
# PerformanceAnalytics
# ggfortify
# foreign
# psych                # Could not find this package during install
# ResourceSelection
# ROCR
# klaR
# heplots
# hermite
# lattice
# boot
# DescTools
# car
# multcompView
# emmeans
# pscl
# robust
# AER
# usmap
# stats
# cluster


libs <- c("tidyverse", "sp", "raster", "ggplot2", "plyr", "dplyr", "sf", "stringr", "grid", "pBrackets", "gridExtra", "lme4", "maps", 
          "glmmTMB", "gamm4", "MASS", "RCurl", "httr", "data.table", "devtools", "lubridate", "blmeco", "VineCopula", "PerformanceAnalytics", 
          "ggfortify", "foreign", "ResourceSelection", "ROCR", "klaR", "heplots", "hermite", "lattice", "boot", "DescTools", "car",
          "multcompView", "emmeans", "pscl", "robust", "AER", "usmap", "gsheet", "gplots", "stats", "cluster", "ggpubr")

for (lib in libs){
  if (!require(lib, character.only = T)) {
    install.packages(lib)
  }
}

if (!require("NSAPHutils")){
  devtools::install_github("NSAPH/NSAPHutils") # need to be accessed via devtools
}

library("tidyverse")
library("sp")
library("raster")
library("ggplot2")
library("plyr")
library("dplyr")
library("sf")
library("stringr")
library("grid") 
library("pBrackets") 
library("gridExtra")
library("lme4")
library("maps")
library("glmmTMB")
library("gamm4")
library("MASS")
library("RCurl")
library("httr")
library("data.table")
library("devtools")
library("lubridate")
library("blmeco")
library("VineCopula")
library("PerformanceAnalytics")
library("ggfortify")
library("foreign")
library("ResourceSelection")
library("ROCR")
library("klaR")
library("heplots")
library("hermite")
library("lattice")
library("boot")
library("DescTools")
library("car")
library("multcompView")
library("emmeans")
library("pscl")
library("robust")
library("AER")
library("usmap")
library("gsheet")
library("gplots")
library("stats")
library("cluster")
library("NSAPHutils")