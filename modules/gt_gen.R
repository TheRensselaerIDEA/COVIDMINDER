#'  @title GT table output for COVIDMINDER
#'  @author Jose Figueroa
#' 
#'  This script assumes ranking dataframe has already been imported by parent script.
#'  @usage gt.ranking
#'  @field entries \code{integer} The number of entries visible in the ranking table. Range (1:50)
#'  @field order \code{function} Function defining the ordering based on state rank. 
#'  Default is identity function (in order), other option is dplyr::desc
#'  @return \{gt} Returns gt table object representing selected ranking of states.
#'  
#'  @usage numeric_fmt
#'  @field .x \code(numeric) Number to be formatted for human readable output.
#'  
#'  @usage date_fmt
#'  @field .x \code(date) Date object to be formatted into human readable output
#'  
#'  @usage sup{n}_fmt
#'  @field .x \code{character} Output to have {n} superscript added to the end of it
#'  
#'  @usage stats.table
#'  @field selected.state \code{character} The 2 character initial representing the state of choice.
#'  @return \code{gt} Returns gt table with various state statistics.

numeric_fmt <- function(.x) {
  num.x <- as.numeric(.x)
  str.x <- format(round(num.x), big.mark = ",")
  str.x
}

date_fmt <- function(.x) {
  date.x <- as.Date(.x)
  str.x <- format(date.x, "%m-%d-%Y")
  str.x
}

sup_fmt <- function(.x, sup=1) {
  md(glue::glue("{.x}<sup>{sup}</sup>"))
}

sup2_fmt <- function(.x) {
  sup_fmt(.x, sup=2)
}

sup3_fmt <- function(.x) {
  sup_fmt(.x, sup=3)
}

gt.ranking <- function(entries = 50, 
                       order=function(x){x}) {
  ranking %>%
    arrange(order(rank)) %>%
    filter(row_number() <= entries) %>%
    select(name, cases.pct, deaths.pct, rank) %>%
    gt() %>%
    tab_header(
      title = paste0("State Rankings"),
      subtitle = paste0("Based on ",time.period," day change in COVID-19 Cases")
    ) %>%
    tab_source_note(
      source_note = md("Data Source: [USA Facts](https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/)")
    ) %>%
    fmt_percent(c("cases.pct", "deaths.pct"), decimals = 1) %>%
    cols_label(
      name = "State",
      cases.pct = "Change in Cases",
      deaths.pct = "Change in Deaths",
      rank = "Rank"
    ) %>%
    cols_move_to_start(columns = vars(rank)) %>%
    tab_options(container.width = "100%",
                table.width = "100%")
}

stats.table <- function(selected_state="NY") {
  # TODO: Instead of row numbers, add aditional column specifying data type (For number formating)
  state_name <- paste0(state.abr[state.abr$abr == selected_state, "name"])
  
  stats <- covid_TS_state_long.cases %>%
    filter(State == selected_state) %>%
    top_n(n=1, wt=date) %>%
    select(diff, cases, p_cases, d_diff, deaths, p_deaths) %>%
    t()
  
  tests <- state_covid_testing %>%
    filter(NAME == state_name) %>%
    select(total_num_tests, tests_per_1000) %>%
    mutate(tests_per_1000 = tests_per_1000 * 100) %>%
    t()
  
  policy <- state_policy.df %>%
    filter(POSTCODE == selected_state) %>% 
    select(STAYHOME, END_STHM) %>%
    t()
  
  stats <- stats %>%
    rbind.data.frame(tests, policy)
  
  state.title <- paste0(state_name, " Stats")
  stats$features <- c("New Cases", "Overall Cases",  "Overall Cases per 100k", "New Deaths", "Overall Deaths", "Overall Deaths per 100k", "Overall Tests", "Overall Tests per 100k", "Stay At Home Order Start Date", "Stay At Home Order End Date")
  
  stats %>%
    mutate(row_num = row_number()) %>%
    gt() %>%
    fmt(c("features"), row = row_num < 7, fns = sup_fmt) %>%
    fmt(c("features"), row = row_num >= 7 & row_num < 9, fns = sup2_fmt) %>%
    fmt(c("features"), row = row_num >= 9, fns = sup3_fmt) %>%
    cols_move_to_start(c("features")) %>%
    tab_header(
      title = md(paste0("**", state.title, "**")),
      subtitle = paste0("How is ", state_name, " performing across various COVID-19 metrics?")
    ) %>%
    tab_source_note(
      source_note = md("Data Source: [USA Facts<sup>1</sup>](https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/), [OWID<sup>2</sup>](https://covid.ourworldindata.org/data/owid-covid-data.csv)")
    ) %>%
    tab_source_note(
      source_note = md("Data Source: Raifman J, Nocka K, Jones D, Bor J, Lipson S, Jay J, and Chan P. (2020). 'COVID-19 US state policy database.' Available at: www.tinyurl.com/statepolicies<sup>3</sup>")
    ) %>%
    fmt(c("V1"), row = row_num < 9, fns = numeric_fmt) %>%
    fmt(c("V1"), row = row_num >= 9, fns = date_fmt) %>%
    cols_hide(c("row_num")) %>%
    tab_options(column_labels.hidden = T,
                container.width = "100%",
                table.width = "100%")
}

US.stats.table <- function() {
  # TODO: Instead of row numbers, add aditional column specifying data type (For number formating)
  name <- "United States"
  
  stats <- covid_TS_US_long.cases %>%
    top_n(n=1, wt=date) %>%
    select(diff, cases, p_cases, d_diff, deaths, p_deaths) %>%
    t()
  
  tests <- state_covid_testing %>%
    select(total_num_tests) %>%
    summarise(total_num_tests = sum(total_num_tests)) %>%
    mutate(tests_per_100k = total_num_tests/US.pop * 100000) %>%
    t()
  
  STHM.count <- state_policy.df %>%
    filter(!is.na(STAYHOME) & is.na(END_STHM)) %>% 
    tally() 
  STHM.count <- STHM.count$n
  
  stats <- stats %>%
    rbind.data.frame(tests, STHM.count)
  
  state.title <- paste0(name, " COVID-19 Stats")
  stats$features <- c("New Cases",  "Overall Cases",  "Overall Cases per 100k", "New Deaths", "Overall Deaths", "Overall Deaths per 100k", "Overall Tests", "Overall Tests per 100k", 'Number of States with active "Stay at Home" orders')
  
  stats %>%
    mutate(row_num = row_number()) %>%
    gt() %>%
    fmt(c("features"), row = row_num < 7, fns = sup_fmt) %>%
    fmt(c("features"), row = row_num >= 7 & row_num < 9, fns = sup2_fmt) %>%
    fmt(c("features"), row = row_num >= 9, fns = sup3_fmt) %>%
    cols_move_to_start(c("features")) %>%
    tab_header(
      title = md(paste0("**", state.title, "**")),
      subtitle = paste0("How is the US performing across various COVID-19 metrics?")
    ) %>%
    tab_source_note(
      source_note = md("Data Source: [USA Facts<sup>1</sup>](https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/), [OWID<sup>2</sup>](https://covid.ourworldindata.org/data/owid-covid-data.csv)")
    ) %>%
    tab_source_note(
      source_note = md("Data Source: Raifman J, Nocka K, Jones D, Bor J, Lipson S, Jay J, and Chan P. (2020). 'COVID-19 US state policy database.' Available at: [www.tinyurl.com/statepolicies<sup>3</sup>](https://www.tinyurl.com/statepolicies)")
    ) %>%
    fmt(c("V1"),fns = numeric_fmt) %>%
    cols_hide(c("row_num")) %>%
    tab_options(column_labels.hidden = T,
                container.width = "100%",
                table.width = "100%")
}
