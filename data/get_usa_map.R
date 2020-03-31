#### Get USA albers map ####
# Set working directory to 'data' folder before you run this script

# Load 'albersusa' library through git
library(albersusa)

# Load the data and save to the file 'us_projection.Rds'
data <- usa_composite()
saveRDS(data, "json/us_projection.Rds")
