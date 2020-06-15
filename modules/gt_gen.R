#'  @title GT table output for COVIDMINDER
#'  @author Jose Figueroa
#' 
#'  This script assumes ranking dataframe has already been imported by parent script.
#'  @usage gt.ranking
#'  @field entries \code{integer} The number of entries visible in the ranking table. Range (1:50)
#'  @field order \code{function} Function defining the ordering based on state rank. 
#'  Default is identity function (in order), other option is dplyr::desc
#'  @return \{gt} Returns gt table object representing selected ranking of states.

gt.ranking <- function(entries = 50, 
                       order=function(x){x}) {
  ranking %>%
    arrange(order(rank)) %>%
    filter(row_number() <= entries) %>%
    select(name, cases.delta, deaths.delta, rank) %>%
    gt() %>%
    tab_header(
      title = paste0("State Rankings"),
      subtitle = "Based on 14 day change in COVID-19 Cases"
    ) %>%
    tab_source_note(
      source_note = md("Data Source: [USA Facts](https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/)")
    ) %>%
    fmt_percent(c("cases.delta", "deaths.delta"), decimals = 1) %>%
    cols_label(
      name = "State",
      cases.delta = "Change in Cases",
      deaths.delta = "Change in Deaths",
      rank = "Rank"
    ) %>%
    cols_move_to_start(columns = vars(rank)) %>%
    tab_options(container.width = "100%",
                table.width = "100%")
}