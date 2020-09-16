#' Used in app.R and/or elsewhere
#' 
#' @note Helper function for reading CDC_WONDER deaths rate data
#'      Three years in a row
#'      Skip `Notes` and `Age Adjusted Rate` and `Crude Rate`
#'
#' @param cdc.file
#' @param cdc.period
#' @param death.cause
#' @param suppress.sub
#'
#' @return 
#'
#' @examples
#' 
#' @author John Erickson (based on others)
#' @export

cdc.reader <- function(cdc.file, cdc.period, death.cause, suppress.sub) {
  ## Import raw data
  readr::read_delim(
    file = cdc.file,
    delim = '\t',
    col_types = cols(
      "Notes" = col_skip(),
      # "Age Adjusted Rate" = col_skip(),
      "Crude Rate" = col_skip(),
      "Population" = col_character(),
      "Deaths" = col_character(),
      "2013 Urbanization Code" = col_skip()
    )
  ) %>% 
    
    ## Unify names
    janitor::clean_names() %>% 
    dplyr::select(
      county_name = county,
      county_fips = county_code,
      state_name = state,
      state_fips = state_code,
      death_num = deaths,
      population = population,
      urban_2013 = x2013_urbanization
    ) %>% 
    
    ## Basic type conversion
    dplyr::mutate(
      county_name = as.character(county_name),
      county_fips = as.character(county_fips),
      death_num = parse_number(death_num, na = c("Missing", "Suppressed", "Unreliable")),
      population = parse_number(population, na = c("Missing", "Suppressed", "Unreliable")),
      urban_2013 = as.factor(urban_2013)
    ) %>%
    
    ## General data manipulation
    dplyr::mutate(
      # Recode `Suppressed` to `1` 
      # Supressed counties have 9 or less deaths so we set it to 0.1
      # In this step, supressed entry has been substituded to NA
      death_num = dplyr::if_else(is.na(death_num), suppress.sub, death_num),
      # population = ifelse(is.na(population), -1, population),
      
      # Calculate crude rate by hand
      # Mortality rate is number of deaths per 10000 people
      death_rate = (death_num / population) * 10^5,
      
      # Addition of all kinds of "stamps"
      # Adding time stamp
      period = cdc.period,
      
      # Add cause of death
      death_cause = death.cause
    ) %>%
    
    ## General tidying of data
    dplyr::mutate(
      # Clean county names
      county_name = stringr::str_replace(county_name, " Parish", ''),
      county_name = stringr::str_replace(county_name, pattern = " County, ", replacement = ','),
      county_name = stringr::str_trim(county_name, side = "both"),
      county_fips = stringr::str_trim(county_fips, side = "both")
    ) %>%
    tidyr::separate(
      col = county_name,
      into = c("county_name", "state_abbr"),
      sep = ','
    ) %>% 
    dplyr::mutate(
      state_abbr = stringr::str_trim(state_abbr, side = "both"),
      state_name = stringr::str_trim(state_name, side = "both")
    ) %>%
    dplyr::select(
      # Rearrangement
      period, state_name, state_abbr, county_name, 
      county_fips, urban_2013, population, death_num, death_rate, death_cause
    ) %>% 
    as.data.frame()
}

#' Used in app.R and/or elsewhere
#' 
#' @note Importing and Conversion
#'
#' @param cdc.file
#' @param cdc.periods
#' @param death.cause
#' @param suppress.sub
#'
#' @return 
#'
#' @examples
#' 
#' @author John Erickson (based on others)
#' @export

cdc.reader.batch <- function(cdc.files, cdc.periods, cdc.cause, suppress.sub = 0.5) {
  
  if (length(cdc.files) != length(cdc.periods)) {
    stop("Lengths of cdc.files and cdc.periods are not equal")
  }
  
  # Collection of dataframes (in list)
  cdc.collection.list <- list()
  for (cdc.i in 1:length(cdc.files)) {
    cdc.collection.list[[cdc.i]] <- cdc.reader(
      cdc.files[cdc.i], 
      cdc.periods[cdc.i], 
      cdc.cause,
      suppress.sub
    )
  }
  
  return(dplyr::bind_rows(cdc.collection.list))
}

#' Used in app.R and/or elsewhere
#' 
#' @note Importing and Conversion
#'
#' @param cdc.data.long
#' @param state.abbr
#' @param death.cause
#'
#' @return 
#'
#' @examples
#' 
#' @author John Erickson (based on others)
#' @export

cdc.mort.mat <- function(cdc.data.long, state.abbr, death.cause = "Despair") {
  
  tmp <- cdc.data.long
  if (state.abbr != "US") {
    tmp <- dplyr::filter(cdc.data.long, state_abbr == state.abbr)
  }else {
    tmp <- cdc.data.long
  }
  
  dplyr::filter(tmp, death_cause == death.cause) %>%
    tidyr::drop_na(county_fips) %>%
    dplyr::select(county_fips, death_rate, period) %>%
    dplyr::group_by(period) %>% 
    dplyr::mutate(id=1:n()) %>%
    tidyr::spread(key = period, value = death_rate) %>%
    dplyr::arrange(county_fips)
}

#' Used in app.R and/or elsewhere
#' 
#' @note Importing and Conversion
#'
#' @param cdc.data.long
#' @param state.abbr
#' @param death.cause
#'
#' @return 
#'
#' @examples
#' 
#' @author Hongrui Zhang (based on others)
#' @export

cdc.countymort.mat <- function(cdc.data.long, state.abbr, county.choice, death.cause = "Despair") {
  tmp <- cdc.data.long
  true_county_name = county.choice
  if (endsWith(county.choice, "County")){
    true_county_name <- substr(county.choice, 0, nchar(county.choice)-7)
  }
  state.data = dplyr::filter(tmp, death_cause == death.cause & state_abbr == state.abbr) %>%
    tidyr::drop_na(county_fips) 
  county.data = dplyr::filter(state.data, county_name == true_county_name)
  if (nrow(county.data) == 0){
    all.county = unique(state.data$county_name)
    highest.score = - Inf
    best.string = ""
    for (county in all.county){
      curr.score =  stringdist::stringsim(county, true_county_name)
      if (curr.score > highest.score){
        highest.score = curr.score
        best.string = county
      }
    }
    county.data = dplyr::filter(state.data, county_name == best.string)
  } 
  county.data %>%
    dplyr::select(county_fips, death_rate, period)%>%
    tidyr::spread(key = period, value = death_rate) %>%
    dplyr::arrange(county_fips)
}