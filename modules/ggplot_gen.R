#'  @title GGPLOT output for COVIDMINDER
#'  @author Jose Figueroa
#' 
#'  This script assumes state.abr, covid_TS_counties_long.cases has already been imported by parent script.
#'  @usage get_y_label
#'  @field y.value \code{character} The name of the data to be displayed on the y axis of ggplot.
#'  @return \{list(character)} Returns user ledgible string to be written in on plot.
#'  
#'  @usage get_diff
#'  @field y.value \code{character} The name of the data to be displayed on the y axis of ggplot.
#'  @return \{list(character)} Returns correlating 'diff' column name to y.value.
#'  
#'  @usage ggplot.state
#'  @field selected.state \code{character} The 2 character initial representing the state of choice.
#'  @field y.value \code{character} The feature to be represented on the y axis of the time series.
#'  @field moving.avg.window \code{integer} Window for moving average smoothing factor (to the left of data point).
#'  @field remove.title \code{boolean} Optional boolean variable to remove title in time series. Default is False.


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

get_dif <- function(y.value) {
  if (y.value == "cases") {
    return("diff")
  }
  if (y.value == "deaths") {
    return("d_diff")
  }
  if (y.value == "p_cases") {
    return("p_diff")
  }
  if (y.value == "p_deaths") {
    return("p.d_diff")
  }
}

ggplot.state <- function(selected.state="NY", 
                         y.value="cases", 
                         moving.avg.window=7, 
                         case.cut=200, 
                         remove.title = F,
                         max.labels=10 # To be implimented
                         ){
  
  y_label <- get_y_label(y.value)
  state.name <- state.abr[state.abr$abr==selected.state,"name"]
  m.a.w <- ""
  if (y.value == "diff" | y.value == "p_diff") {
    m.a.w <- paste0(" (",moving.avg.window," day Average)")
  }
  
  if (remove.title) {
    gg_title <- NULL
  }
  else {
    gg_title <- ggtitle(paste0(state.name, " ", y_label, " over time",m.a.w))
  }
  
  
  covid_TS_counties.cases.plot <- covid_TS_counties_long.cases %>%
    select(-c(countyFIPS, stateFIPS)) %>%
    filter(State == selected.state) %>%
    group_by(County) %>% 
    filter(n() >= moving.avg.window) %>%
    mutate(diff = c(numeric(moving.avg.window-1), zoo::rollmean(diff, moving.avg.window, align = "right"))) %>%
    mutate(p_diff = c(numeric(moving.avg.window-1), zoo::rollmean(p_diff, moving.avg.window, align = "right"))) %>%
    ungroup()
  
  if (selected.state == "NY") {
    nyc.population <- covid_TS_counties.cases.plot %>%
      filter(County %in% c("New York", "Kings", "Queens", "Bronx", "Richmond")) %>%
      group_by(County) %>%
      top_n(n=1, wt=date) %>%
      select(population)
    nyc.population <- sum(nyc.population$population)
    
    NYC <- covid_TS_counties.cases.plot %>%
      filter(County %in% c("New York", "Kings", "Queens", "Bronx", "Richmond")) %>%
      group_by(State, date) %>%
      summarise(
        County = "New York City",
        cases = sum(cases),
        deaths = sum(deaths),
        population = nyc.population,
        p_cases = sum(cases)*100000/nyc.population,
        p_deaths = sum(deaths)*100000/nyc.population,
        diff = sum(diff),
        p_diff = sum(diff)*100000/nyc.population,
        d_diff = sum(d_diff),
        p.d_diff = sum(p.d_diff)*100000/nyc.population
      ) 
    covid_TS_counties.cases.plot <- covid_TS_counties.cases.plot %>%
      filter(!County %in% c("New York", "Kings", "Queens", "Bronx", "Richmond")) %>%
      rbind.data.frame(NYC)
    
  }
  
  state <- covid_TS_state_long.cases %>%
    filter(State == selected.state) %>%
    filter(n() >= moving.avg.window) %>%
    mutate(diff = c(numeric(moving.avg.window-1), zoo::rollmean(diff, moving.avg.window, align = "right"))) %>%
    mutate(p_diff = c(numeric(moving.avg.window-1), zoo::rollmean(p_diff, moving.avg.window, align = "right")))
  
  state$County = selected.state
  
  covid_TS_counties.cases.plot <-  covid_TS_counties.cases.plot %>%
    group_by(County) %>%
    filter(max(cases) > case.cut) %>%
    ungroup() %>%
    rbind.data.frame(state) %>%
    filter(get(y.value) > 0)
  
  
  county.num <- covid_TS_counties.cases.plot %>% 
    select(County) %>%
    unique()
  
  colnames(county.num) <- c("County")
  county.num <- county.num %>%
    mutate(num = row_number()) %>%
    mutate(County = as.character(County))
  
  n_county <- nrow(county.num)
  highlight_points <- covid_TS_counties.cases.plot  %>%
    left_join(county.num, by=c("County" = "County")) %>%
    group_by(County) %>%
    mutate(range = as.numeric(as.Date(max(date))) - as.numeric(as.Date(min(date)))) %>%
    mutate(max_date = as.Date(max(date))) %>%
    mutate(max_date = max_date - ((num*((range%/%n_county) + 1))%%range))  %>%
    filter(date == max_date) %>%
    top_n(1, wt=max_date) 
  
  library(RColorBrewer)
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  county.num$Color <- col_vector[1:n_county]
  
  region_palette <- setNames(as.character(county.num$Color), as.character(county.num$County))
  region_palette[selected.state] <- "#A8A8A8"
  
  g <- covid_TS_counties.cases.plot %>%
    ggplot(aes_string(
      x="date",
      y=y.value,
      color="County",
      group="County")) +
    scale_color_manual(values=region_palette, guide=guide_legend(title.position = "top",title.hjust = 0.5)) +
    geom_line(size=1) +
    scale_y_continuous(
      trans = "log10",
      breaks = c(10,100,500,1000,5000,10000, 50000)
    ) +
    scale_x_datetime(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%b %d") +
    ylab(y_label) + 
    xlab("Date") +
    #theme(legend.position = "none") +
    geom_label_repel(
      data=highlight_points,  
      aes(label=County, fill=County), 
      box.padding = unit(1.75, 'lines'),
      color = "black",
      size = 5,
      show.legend = FALSE
    ) +
    scale_color_manual(values=region_palette, aesthetics = c("fill")) +
    geom_vline(aes(xintercept=state_policy.df[state_policy.df$POSTCODE == selected.state,]$END_STHM, linetype=paste0(selected.state," ends stay at home order")), color = "red") + 
    geom_vline(aes(xintercept=state_policy.df[state_policy.df$POSTCODE == selected.state,]$STAYHOME, linetype=paste0(selected.state," begins stay at home order")), color = "blue") + 
    scale_linetype_manual(name = "Events", 
                          values = c(2,2), 
                          guide = guide_legend(title.position = "top",title.hjust = 0.5,override.aes = list(color = c("blue", "red")), direction = "vertical")) +
    theme(legend.position = "bottom") +
    gg_title +
    NULL
  return(g)
}
