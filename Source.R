##### Load packages

# List of packages used:
# - dplyr
# - geojsonio
# - htmltools
# - leaflet
# - leaflet.extras
# - shiny
# - tidyverse
dependencies <- c("shiny", "dplyr", "geojsonio", "htmltools", "leaflet", "leaflet.extras", "tidyverse")

# Check and install packages not yet available
install.dependencies <- dependencies[!(install.dependencies %in% installed.packages()[, "Package"])]
if (length(install.dependencies) > 0) {
  install.packages(install.dependencies)
}

# Load all packages
library(dplyr)
library(geojsonio)
library(htmltools)
library(leaflet)
library(leaflet.extras)
library(shiny)
library(tidyverse)