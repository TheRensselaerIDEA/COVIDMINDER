library(profvis)
source("./app.R")
profvis({
  runApp()
})