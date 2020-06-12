#' GGPLOT output for COVIDMINDER
#' 
#'  This script assumes todats.case.data has already been imported by parent script.
#'  @usage get_y_label
#'  @field y.value \code{character} The name of the data to be displayed on the y axis of ggplot.
#'  @return \{list(character)} Returns user ledgible string to be written in on plot.
#'  
#'  @usage geo_plot
#'  @field state.choice \code{character} The 2 character initial representing the state of choice.
#'  @field feature \code{character} The feature to be accessed by get_ldi.
#'  @field reverse \code{boolean} Optional boolean variable to reverse color scheme in state heatmap. 
#'  Default is False.

get_y_label <- function(y.value) {
  if (y.value == "cases") {
    return("COVID-19 Cases")
  }
  if (y.value == "p_cases") {
    return("COVID-19 Cases per 100k")
  }
  if (y.value == "diff") {
    return("New COVID-19 Cases")
  }
  if (y.value == "p_diff") {
    return("New COVID-19 Cases per 100k")
  }
  if (y.value == "deaths") {
    return("COVID-19 Deaths")
  }
  if (y.value == "p_deaths") {
    return("COVID-19 Deaths per 100k")
  }
  else {
    return("")
  }
}