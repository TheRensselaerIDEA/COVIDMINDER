# List of all libraries
libs <- c("tidyverse", "sp", "raster", "ggplot2", "plyr", "dplyr", "sf", "stringr", "grid", "pBrackets", "gridExtra", "lme4", "maps", 
          "glmmTMB", "gamm4", "MASS", "RCurl", "httr", "data.table", "devtools", "lubridate", "blmeco", "VineCopula", "PerformanceAnalytics", 
          "ggfortify", "foreign", "ResourceSelection", "ROCR", "klaR", "heplots", "hermite", "lattice", "boot", "DescTools", "car",
          "multcompView", "emmeans", "pscl", "robust", "AER", "usmap", "gsheet", "gplots", "stats", "cluster", "ggpubr", "caret", "pROC", 
          "cvms", "sgof", "kableExtra", "pracma")

for (lib in libs){
  # if required, install before loading
  if (!require(lib, character.only = T)) {
    install.packages(lib)
  }
  do.call(library, list(lib))
}

# Special case: need to be accessed via devtools
if (!require("NSAPHutils")){
  devtools::install_github("NSAPH/NSAPHutils")
}

library("NSAPHutils")