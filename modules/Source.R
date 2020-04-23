##### Load packages ####

# List of packages used:
# - dplyr
# - gghighlight
# - ggrepel
# - geojsonio
# - htmltools
# - leaflet
# - leaflet.extras
# - shiny
# - tidyverse
dependencies <- c("shiny", "dplyr", "gghighlight", "ggrepel", "geojsonio", "htmltools", "leaflet", "leaflet.extras", "tidyverse")

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
library(htmltools)
library(leaflet)
library(leaflet.extras)
library(shiny)
library(tidyverse)
library(lubridate)
#library(rsconnect) # required by JSE for pre-release deployment on shinyapps.io 
