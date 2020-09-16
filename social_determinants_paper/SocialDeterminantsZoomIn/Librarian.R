# README:----------------------------------------------------------------------------------+
# DESC:
#   This file is used to load all packages needed for the app.
#   Manage packages in a single file, help dealing with naming space issue
#     - e.g. `dplyr` and `plyr` select() override problem
#   This file should be sourced first beforing running any other module

# NOTE: 
#   DO NOT LOAD ANY OTHER LIBRARY IN ANY OTHER MODULES

# PACKAGES: 
#   NULL
# -----------------------------------------------------------------------------------------+

# All libraries for loaders should be listed below
#   - Including packages for data importing and data manipulation
LIBS_LOADER <- c(
  # Readers
  "readr", "readxl",
  
  # Data manipulation
  "lubridate", "tidyr", "dplyr", "stringr", "janitor", "purrr", "textshape", "stats", 
  "magrittr", "zoo", "textshape", "forcats", "stringdist"
)

# All libraries for analysis should be listed below
#   - e.g. `randomForest`
LIBS_ANALYSIS <- c(
  "randomForest", "cluster"
)

# All libraries for build the shiny-d3 app should be listed below
#   - e.g. `shiny`
LIBS_SHINYAPP <- c(
  "shiny", "shinythemes", "shinyWidgets", "shinydashboard", 
  "r2d3", "RColorBrewer", "viridis", "shinyjs", "grid",
  "htmltools"
)

# All libraries for plotting should be listed below
#   - e.g.`ggplot2`
# Note: DO NOT PUT SHINY PACKAGES HERE
LIBS_PLOT <- c(
  "ggplot2", "plotly", "ggrepel", 
  
  # Choropleth plot
  "RUnit", "mapproj", "maps", "leaflet", "leaflet.extras"
)


# LIST STORING ALL VECTOR OF LIBS
LIBS_ALL <- list(
  "app" = LIBS_SHINYAPP,
  "plot" = LIBS_PLOT,
  "analysis" = LIBS_ANALYSIS,
  "loader" = LIBS_LOADER
)


# ACTUALL LOADER
# LIBS_RC SHOULD BE NULL IF ALL PACKAGES LOADED SUCCESSFULLY
LIBS_RC <- lapply(
  LIBS_ALL, 
  function(lib_vec) {
    rc <- c()
    for (p in lib_vec) {
      if (!require(p, character.only = T)) {
         install.packages(p)
        rc <- c(rc, require(p, character.only = T))
      }
    }
    return(rc)
  }
)

# SPECIAL PACKAGES
if (!require("devtools", character.only = T)) {
   install.packages("devtools")
   require("devtools", character.only = T)
}

if (!require("urbnmapr", character.only = T)) {
   devtools::install_github("UrbanInstitute/urbnmapr")
  require("urbnmapr", character.only = T)
}

# Clean-up's
rm(list = ls(all.names = T))
