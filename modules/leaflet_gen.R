#'  @title Leaflet output for COVIDMINDER
#'  @author Jose Figueroa
#' 
#'  This script assumes todats.case.data has already been imported by parent script.
#'  @usage get_ldi
#'  @field feature \code{character} The name of the raw data to be displayed in LDI format. I.e
#'  "Case" would be raw numbers, with corresponding rows "Case_rate" and "Case_rate_ldi".
#'  @return \{list(character)} Returns appended \_rate and \_rate\_ldi strings to be accesed in dataframe.
#'  
#'  @usage geo_plot
#'  @field state.choice \code{character} The 2 character initial representing the state of choice.
#'  @field feature \code{character} The feature to be accessed by get_ldi.
#'  @field reverse \code{boolean} Optional boolean variable to reverse color scheme in state heatmap. 
#'  Default is False.

colors <- c("#253494","#4575B4", "#74ADD1","#ABD9E9","#f7f7f7","#FDAE61","#F46D43", "#D73027", "#BD0026")
bins <- c(5, 3, 2, 1, .2, -.2, -1, -2, -3, -5)

get_ldi <- function(feature) {
  rate <- paste0(feature,"_rate")
  ldi <- paste0(rate, "_ldi")
  return(c(ldi, rate))
}

get_zoom <- function(state.choice) {
  if (state.choice %in% c("TX", "CA")) {
    zoom = 5
  }
  else if(state.choice %in% "AK") {
    zoom = 3
  }
  else {
    zoom = 6
  }
}

get_colorbar <- function(reverse) {
  if (reverse) {
    .colors <- rev(colors)
  }
  else {
    .colors <- colors
  }
  
  .colors <- lapply(.colors, function(x){paste0("<li class = 'legend-colorsquares' style='background-color: ",x,";'></li>")})
  return(paste0(.colors, collapse = ''))
}

geo.plot <- function(state.choice, 
                     feature, 
                     reverse=F,
                     title = "") {
  # Feature: Case, Mortality...
  # US Data: ....
  ldi_feature <- get_ldi(feature)
  if (state.choice == "US") {
    shapes <- states.shapes 
    dataset <- states
    dataset$Name <- dataset$NAME
    state.choice <- "State"
  }
  else {
    dataset <- todays.case.data %>%
      filter(State == state.choice)
    shapes <- readRDS(paste("data/shape_files/", state.choice, ".Rds", sep = ""))
    shapes$countyFIPS <- as.numeric(paste(as.data.frame(shapes)$STATEFP, as.data.frame(shapes)$COUNTYFP, sep = ''))
    dataset <- dplyr::left_join(as.data.frame(shapes), as.data.frame(dataset), by = c("countyFIPS" = "countyFIPS")) 
    dataset$Name <- dataset$County
    m.a.w <- ""
  }
  
  pal2 <- leaflet::colorBin(colors, domain = dataset[,ldi_feature[1]], bins = bins, reverse=reverse)
  
  labels <- sprintf(
    paste0("<strong>%s</strong><br/>",
    feature," Rate DI: %.2g<br>",
    feature," Rate: %.1f /100k"),
    dataset$Name, dataset[,ldi_feature[1]], (dataset[,ldi_feature[2]])*100000
  ) %>% lapply(htmltools::HTML)
  
  
  l <- leaflet(shapes) %>%
            addPolygons(
              fillColor = ~pal2(dataset[,ldi_feature[1]]),
              weight = 1,
              opacity = 1,
              color = "#330000",
              dashArray = "1",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
            addControl(
              HTML(paste0(
                "<div>
                  <b>",state.choice," ", feature, " Rates vs. US Average</b>
                  <sup title='A log disparity index is used to compare respective values to average US rate. A disparity index < -0.2 indicates values below the US average. A disparity index > 0.2 indicates above the US average.' class='fa fa-info-circle'></sup>
                </div></br>",
                "<div class='labels'>
                  <div style='float:left;'>Lower</div>
                  <div style='float:right;'>Higher</div>
                </div>
                <ul class='legend-colorbar'>",
                  get_colorbar(reverse),
                "</ul>"
              )),
              position = "bottomright",
              className = "info legend"
            ) %>%
            # addLegend(pal = pal2,
            #           values = ~dataset[ldi_feature[1]],
            #           opacity = 0.7,
            #           title = paste0("Disparity Index<br/>",state.choice," ",feature," Rates"),
            #           position = "bottomright",
            #           labFormat = function(type, cuts, p) { n = length(cuts)
            #           cuts[n] = "Lower"
            #           # for (i in c(1,seq(3,(n-1)))){cuts[i] = paste0(cuts[i],"—")}
            #           for (i in c(1,seq(2,(n-1)))){cuts[i] = ""}
            #           cuts[2] = "Higher"
            #           paste0(str_remove(cuts[-n],"Higher"), str_remove(cuts[-1],"—"))
            #           }
            # ) %>%
            addProviderTiles("MapBox", options = providerTileOptions(
              id = "mapbox.light"))
  if (title != "") {
    l <- l %>%
      addControl(title, position = "topleft", className = "leaflet-map-title")
  }
  return(l)
}