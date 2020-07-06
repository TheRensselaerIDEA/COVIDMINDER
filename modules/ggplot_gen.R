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
#'  @return \code{ggplot} GGplot object with a county level time series.
#'  
#'  @usage ggplot.US
#'  @field y.value \code{character} The feature to be represented on the y axis of the time series.
#'  @field moving.avg.window \code{integer} Window for moving average smoothing factor (to the left of data point).
#'  @field remove.title \code{boolean} Optional boolean variable to remove title in time series. Default is False.
#'  @return \code{ggplot} GGplot object with a state level time series.
#'  
#'  @usage ggbar.overall   (TODO: Should change to ggbar.state)
#'  @field selected.state \code{character} The 2 character initial representing the state of choice.
#'  @field y.value \code{character} The feature to be represented on the y axis of the time series.
#'  @field moving.avg.window \code{integer} Window for moving average smoothing factor (to the left of data point).
#'  @field remove.title \code{boolean} Optional boolean variable to remove title in time series. Default is False.
#'  @return \code{ggplot} GGplot object with cumilative bar chart as well as moving average lines.
#'  
#'  @usage ggbar.US
#'  @field y.value \code{character} The feature to be represented on the y axis of the time series.
#'  @field moving.avg.window \code{integer} Window for moving average smoothing factor (to the left of data point).
#'  @field remove.title \code{boolean} Optional boolean variable to remove title in time series. Default is False.
#'  @return \code{ggplot} GGplot object with cumilative bar chart for United States as well as moving average lines.
  
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

ggplot.state <- function(selected.state = "NY", 
                         y.value = "diff", 
                         moving.avg.window=7, 
                         counties = c(), 
                         remove.title = F,
                         date = "",
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
    gg_title <- ggtitle(paste0(state.name, " ", y_label, " ",m.a.w, " [", date, "]"))
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
    filter(County %in% counties) %>%
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
      breaks = c(10,25,100,250,500,1000,2500,5000,10000,25000,50000),
      label = scales::comma
    ) +
    scale_x_datetime(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%b") +
    labs(x = "Date",
         y = y_label,
         caption = "<strong>Data Source:</strong> USA Facts, tinyurl.com/statepolicies") +
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
    theme(legend.position = "bottom", 
          title = element_textbox_simple(hjust = 0.5, size = 18),
          axis.title.x = element_markdown(size = 16, lineheight = 24),
          axis.title.y = element_markdown(size = 16),
          axis.text = element_markdown(size = 12, face = "bold"),
          legend.text = element_markdown(size = 14),
          legend.title = element_markdown(size = 16),
          plot.caption = element_textbox_simple(halign = 1)) +
    gg_title
  return(g)
}

ggbar.overall <- function(selected.state = "NY", 
                          y.value="p_cases", 
                          moving.avg.window=14, 
                          remove.title = F,
                          date = "") {
  state <- covid_TS_state_long.cases %>%
    filter(State == selected.state)
  
  my_diff <- get_dif(y.value)
  category <- get_y_label(y.value)
  
  if (remove.title) {
    gg_title <- NULL
  }
  else {
    gg_title <- ggtitle(paste0(selected.state, " ", category, " Over Time (", date,")"))
  }
  
  
  state_cases <- state[c("date", y.value, my_diff)]
  state_cases <- state_cases %>%
    rename(Values = all_of(y.value)) %>%
    rename(diff = all_of(my_diff)) %>%
    mutate(diff.ma =  c(diff[1:7-1], zoo::rollmean(diff, 7, align="right"))) %>%
    #mutate(pct_increase =diff.ma/Values*100) %>%
    mutate(pct_increase =diff/Values*100) %>%
    mutate(ma = c(numeric(moving.avg.window-1), zoo::rollmean(Values, moving.avg.window, align = "right")))
  #state_cases[state_cases$diff.ma > 0 & state_cases$pct_increase > 5, "pct_increase"] <- 5
  state_cases[state_cases$diff > 0 & state_cases$pct_increase > 5, "pct_increase"] <- 5
  state_cases[is.na(state_cases$pct_increase) | state_cases$pct_increase <= 0, "pct_increase"] <- NA
  state_cases$Type <- "State Moving Average"
  
  US.ma <- covid_TS_US_long.cases[c("date", y.value, my_diff)] %>%
    rename(Values = all_of(y.value)) %>%
    rename(diff = all_of(my_diff)) %>%
    mutate(diff.ma =  c(diff[1:7-1], zoo::rollmean(diff, 7, align="right"))) %>%
    #mutate(pct_increase =diff.ma/Values*100) %>%
    mutate(pct_increase =diff/Values*100) %>%
    mutate(ma = c(numeric(moving.avg.window-1), zoo::rollmean(Values, moving.avg.window, align = "right"))) %>%
    filter(ma > 0) %>%
    filter(date > min(state_cases$date))
  US.ma$Type <- "US Moving Average"
  state.us.ma <- rbind.data.frame(state_cases, US.ma)
  
  END_STHM <- paste0(selected.state," ends stay at home order")
  STHM <- paste0(selected.state," begins stay at home order")
  
  return(state_cases %>%
           filter(Values > 0) %>%
           ggplot() +
           geom_col(aes(x=date, y=Values, fill = pct_increase)) +
           scale_fill_gradient(high = "#ff0000", 
                               low = "#ffffff",
                               limit = c(0,5),
                               breaks = c(2.5,5),
                               labels = c("2.5%","5%+"),
                               na.value = "skyblue",
                               guide = guide_colorbar(title = paste0("Percentage change in ", category),
                                                      title.hjust = 0.5,
                                                      title.position = "top", 
                                                      label.hjust = 0.5, 
                                                      barwidth = 15,
                                                      frame.colour = "black")) +
           geom_point(data = state_cases, aes(x=date, y=Values, size = ""), shape = NA, colour = "skyblue") +
           guides(size=guide_legend(title = "No Change", 
                                    override.aes=list(shape=15, size = 8), 
                                    title.position = "top",
                                    title.hjust = 0.5)) +
           geom_line(data = state.us.ma,aes( x=date, y=ma, color=Type, group=Type), arrow=arrow(ends="last"), show.legend = F)  + 
           geom_line(data = state.us.ma,aes( x=date, y=ma, color=Type, group=Type)) +
           geom_vline(aes(xintercept=state_policy.df[state_policy.df$POSTCODE == selected.state,]$END_STHM, color = END_STHM), linetype="longdash", size = 1, show.legend = F) +
           geom_vline(aes(xintercept=state_policy.df[state_policy.df$POSTCODE == selected.state,]$STAYHOME, color= STHM), linetype="longdash", size = 1, show.legend = F) +
           scale_color_manual(name = "Line Types", 
                              values = c("blue", "red", "black", "steelblue"),
                              guide = guide_legend(title.position = "top",
                                                   title.hjust = 0.5,
                                                   direction = "vertical")) +
           scale_x_datetime(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%b") +
           scale_y_continuous(label = scales::comma) +
           #ylab(get_y_label(y.value)) + 
           #xlab("Date") +
           labs(x = "Date",
                y = get_y_label(y.value),
                caption = "<strong>Data Source:</strong> USA Facts, tinyurl.com/statepolicies") +
           theme(legend.position = "bottom", 
                 legend.direction = "horizontal",
                 title = element_textbox_simple(hjust = 0.5, size = 18),
                 axis.title.x = element_markdown(size = 16, lineheight = 24),
                 axis.title.y = element_markdown(size = 16),
                 axis.text = element_markdown(size = 12, face = "bold"),
                 legend.text = element_markdown(size = 14),
                 legend.title = element_markdown(size = 16),
                 plot.caption = element_textbox_simple(halign = 1)) +
           gg_title
  )
}

ggbar.US <- function(y.value="cases", 
                     moving.avg.window=14, 
                     remove.title = F,
                     date = "") {
  my_diff <- get_dif(y.value)
  category <- get_y_label(y.value)
  
  if (remove.title) {
    gg_title <- NULL
  }
  else {
    gg_title <- ggtitle(paste0("United States ", category, " over time (",date,")"))
  }
  
  
  US.ma <- covid_TS_US_long.cases %>%
    rename(Values = all_of(y.value)) %>%
    rename(my_diff = all_of(my_diff)) %>%
    mutate(diff.ma =  c(my_diff[1:7-1], zoo::rollmean(my_diff, 7, align="right"))) %>%
    #mutate(pct_increase =diff.ma/Values*100) %>%
    mutate(pct_increase =my_diff/Values*100) %>%
    mutate(ma = c(numeric(moving.avg.window-1), zoo::rollmean(Values, moving.avg.window, align = "right"))) %>%
    filter(ma > 0)
  #US.ma[US.ma$diff.ma > 0 & US.ma$pct_increase > 5, "pct_increase"] <- 5
  US.ma[US.ma$my_diff > 0 & US.ma$pct_increase > 5, "pct_increase"] <- 5
  US.ma[is.na(US.ma$pct_increase) | US.ma$pct_increase <= 0, "pct_increase"] <- NA
  US.ma$Type <- "US Moving Average"
  
  return(US.ma %>%
           ggplot() +
           geom_col(aes(x=date, y=Values, fill = pct_increase)) +
           scale_fill_gradient(high = "#ff0000", 
                               low = "#ffffff",
                               limit = c(0,5),
                               breaks = c(2.5,5),
                               labels = c("2.5%","5%+"),
                               na.value = "skyblue",
                               guide = guide_colorbar(title = paste0("Percentage change in ", category),
                                                      title.hjust = 0.5,
                                                      title.position = "top", 
                                                      label.hjust = 0.5, 
                                                      barwidth = 15,
                                                      frame.colour = "black")) +
           geom_point(aes(x=date, y=Values, size = ""), shape = NA, colour = "skyblue") +
           guides(size=guide_legend(title = "No Change", 
                                    override.aes=list(shape=15, size = 8), 
                                    title.position = "top",
                                    title.hjust = 0.5)) +
           geom_line(aes(x=date, y=ma), color="black", arrow=arrow(ends="last"), show.legend = F)  + 
           geom_line(aes( x=date, y=ma, linetype=Type), color="black") +
           guides(linetype = guide_legend(title = "Line Type",
                                          title.position = "top",
                                          title.hjust = 0.5)) +
           scale_x_datetime(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%b") +
           scale_y_continuous(label = scales::comma) +
           labs(x = "Date",
                y = get_y_label(y.value),
                caption = "<strong>Data Source:</strong> USA Facts") +
           theme(legend.position = "bottom", 
                 legend.direction = "horizontal",
                 title = element_textbox_simple(hjust = 0.5, size = 18),
                 axis.title.x = element_markdown(size = 16, lineheight = 24),
                 axis.title.y = element_markdown(size = 16),
                 axis.text = element_markdown(size = 12, face = "bold"),
                 legend.text = element_markdown(size = 14),
                 legend.title = element_markdown(size = 16),
                 plot.caption = element_textbox_simple(halign = 1)) +
           gg_title + 
           NULL
  )
}

ggplot.US <- function(y.value="cases", 
                      moving.avg.window=7,
                      selected.states = c(), 
                      remove.title = F, 
                      date = "",
                      max.labels=10) {
  
  y_label <- get_y_label(y.value)
  m.a.w <- ""
  if (y.value == "diff" | y.value == "p_diff") {
    m.a.w <- paste0(" (",moving.avg.window," day Average)")
  }
  
  if (remove.title) {
    gg_title <- NULL
  }
  else {
    gg_title <- ggtitle(paste0("United States ", y_label, " ",m.a.w, " [",date,"]"))
  }
  
  
  covid_TS_state.cases.plot <- covid_TS_state_long.cases %>%
    select(-c(population)) %>%
    filter(State %in% selected.states) %>%
    group_by(State) %>% 
    filter(n() >= moving.avg.window) %>%
    mutate(diff = c(numeric(moving.avg.window-1), zoo::rollmean(diff, moving.avg.window, align = "right"))) %>%
    mutate(p_diff = c(numeric(moving.avg.window-1), zoo::rollmean(p_diff, moving.avg.window, align = "right"))) %>%
    ungroup()
  
  US <- covid_TS_US_long.cases %>%
    mutate(diff = c(numeric(moving.avg.window-1), zoo::rollmean(diff, moving.avg.window, align = "right"))) %>%
    mutate(p_diff = c(numeric(moving.avg.window-1), zoo::rollmean(p_diff, moving.avg.window, align = "right")))
  
  US$State = "US"
  
  covid_TS_state.cases.plot <-  covid_TS_state.cases.plot %>%
    group_by(State) %>%
    ungroup() %>%
    rbind.data.frame(US) %>%
    filter(get(y.value) > 1)
  
  
  state.num <- covid_TS_state.cases.plot %>% 
    select(State) %>%
    unique() %>%
    left_join(state.abr[c("abr", "name", "Region")], by=c("State" = "abr")) %>%
    mutate(name = as.character(unlist(name))) %>%
    mutate(Region = as.character(unlist(Region))) %>%
    mutate(num = row_number())
  state.num[state.num$State == "US", "name"] <- "United States"
  state.num[state.num$State == "US", "Region"] <- "United States"
  
  covid_TS_state.cases.plot <- covid_TS_state.cases.plot  %>%
    left_join(state.num, by=c("State" = "State"))
  
  n_state <- nrow(state.num)
  highlight_points <- covid_TS_state.cases.plot  %>%
    group_by(name) %>%
    mutate(range = as.numeric(as.Date(max(date))) - as.numeric(as.Date(min(date)))) %>%
    mutate(max_date = as.Date(max(date))) %>%
    mutate(max_date = max_date - ((num*((range%/%n_state) + 1))%%range))  %>%
    filter(date == max_date) %>%
    top_n(1, wt=max_date) %>%
    ungroup()
  
  #n_region <- length(unique(state.num$Region)) # 5
  library(RColorBrewer)
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  state.num$Color <- "black"
  state.num[state.num$Region == "West", "Color"] <- col_vector[1]
  state.num[state.num$Region == "South", "Color"] <- col_vector[5]
  state.num[state.num$Region == "North Central", "Color"] <- col_vector[6]
  state.num[state.num$Region == "Northeast", "Color"] <- col_vector[7]
  
  region_palette <- setNames(as.character(state.num$Color), as.character(state.num$Region))
  
  g <- covid_TS_state.cases.plot %>%
    ggplot(aes_string(
      x="date",
      y=y.value,
      color = "Region",
      group = "name")) +
    scale_color_manual(values=region_palette, guide=guide_legend(title.position = "top",title.hjust = 0.5)) +
    geom_line(size=1.5) +
    scale_y_continuous(
      trans = "log10",
      breaks = c(10,25,100,250,500,1000,2500,5000,10000,25000,50000),
      labels = scales::comma
    ) +
    scale_x_datetime(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%b") +
    #ylab(y_label) + 
    #xlab("Date") +
    labs(x = "Date",
         y = y_label,
         caption = "<strong>Data Source:</strong> USA Facts") +
    #theme(legend.position = "none") +
    geom_label_repel(
      data=highlight_points,  
      aes(label=name, color=Region), 
      box.padding = unit(1.75, 'lines'),
      segment.color = "black",
      size = 5,
      show.legend = FALSE
    ) +
    scale_color_manual(values=region_palette, aesthetics = c("color")) +
    guides(color = guide_legend(title = "Region",
                                title.position = "left")) +
    theme(legend.position = "bottom", 
          title = element_textbox_simple(hjust = 0.5, size = 18),
          axis.title.x = element_markdown(size = 16, lineheight = 24),
          axis.title.y = element_markdown(size = 16),
          axis.text = element_markdown(size = 12, face = "bold"),
          legend.text = element_markdown(size = 14),
          legend.title = element_markdown(size = 16),
          plot.caption = element_textbox_simple(halign = 1)) +
    gg_title
  return(g)
}
