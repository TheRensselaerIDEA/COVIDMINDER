##### Load packages ####

# List of packages used:
# - dplyr
# - gghighlight
# - ggrepel
# - geojsonio
# - gt
# - htmltools
# - leaflet
# - leaflet.extras
# - mapview
# - shiny
# - tidyverse
# - zoo

options(repos=structure(c(CRAN="http://cran.rstudio.com"))) # Needed for Rstudio server side

dependencies <- c("shiny", "dplyr", "gghighlight", "ggrepel", "geojsonio", "gt", "htmltools", "leaflet", "leaflet.extras", "mapview", "R6", "shiny","tidyverse", "zoo")

# Check and install packages not yet available
install.dependencies <- dependencies[!(dependencies %in% installed.packages()[, "Package"])]
if (length(install.dependencies) > 0) {
  install.packages(install.dependencies)
}

# Load all packages
library(dplyr)
library(geojsonio)
library(gghighlight)
library(ggrepel)
library(gt)
library(htmltools)
library(leaflet)
library(leaflet.extras)
library(mapview)
library(R6)
library(shiny)
library(tidyverse)
library(lubridate)
library(zoo)
#library(rsconnect) # required by JSE for pre-release deployment on shinyapps.io 
