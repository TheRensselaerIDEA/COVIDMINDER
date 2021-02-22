

# Read in new Social Determinants definitions
SocialDeterminants <- read_csv("./data/social_det_gen/covid_determinants_data/SocialDeterminants.csv")
final.determinants <- SocialDeterminants[SocialDeterminants$Keep == 1,]["Code"]
# these are columns we will select from the actual chr dataset
final.determinants <- append(final.determinants$Code, "county_fips", after = 0)

# Load all data
chr.data.2019 <- readRDS("./data/social_det_gen/covid_determinants_data/chr.data.2019.rds")
chr.data.2019 <- chr.data.2019 %>%
  as_data_frame %>%
  select(final.determinants)

# Load name map and its inverse
# this is a tibble associating the name of each determinant column to it's human readable name
# eg 20th_percentile_income = 20th Percentile Income 
chr.namemap.2019 <- SocialDeterminants %>% select("Code", "Name")
chr.namemap.2019 <- column_to_rownames(chr.namemap.2019, "Code")
names(chr.namemap.2019)[1] <- "name"

geo.namemap <- readRDS("./data/social_det_gen/covid_determinants_data/geo.namemap.rds")



date_of_study = Sys.Date()

chr <- read_csv("./data/social_det_gen/covid_determinants_data/2020CHR.csv")
chr <- chr[, -grep("Quartile", colnames(chr))]
chr <- chr[, -grep("95", colnames(chr))]
chr <- subset(chr, select = c(FIPS, Population))

geo <- subset(geo.namemap, select = -c(state_fips))
geo$county_fips <- str_pad(geo$county_fips, 5, pad = "0")

# Historical data
covid_hist = read.csv(text=getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-22-2020.csv")))
covid_us_hist = subset(covid_hist, Country_Region == "US" & is.na(FIPS)==F)
covid_us_hist = subset(covid_us_hist, select = c(FIPS, Deaths))
covid_us_hist$FIPS <- str_pad(covid_us_hist$FIPS, 5, pad = "0")
covid_us_hist = merge(x = covid_us_hist,y = geo, by.x = "FIPS", by.y = "county_fips", all.y = TRUE)
covid_us_hist = merge(x = covid_us_hist,y = chr, by = "FIPS", all.x = TRUE)
covid_us_hist$period = "03-22-2020"

date_of_all = format(seq(as.Date("2020-03-23"), as.Date(strptime(date_of_study,"%Y-%m-%d")), by = "month"),"%m-%d-%Y")

for (i in 1:length(date_of_all)){
  covid_daily = read.csv(text=getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",date_of_all[i],".csv")))
  covid_daily = subset(covid_daily,Country_Region == "US" & is.na(FIPS)==F)
  covid_daily = subset(covid_daily, select = c(FIPS, Deaths))
  covid_daily$FIPS <- str_pad(covid_daily$FIPS, 5, pad = "0")
  covid_daily = merge(x = covid_daily,y = geo, by.x = "FIPS", by.y = "county_fips", all.y = TRUE)
  covid_daily = merge(x = covid_daily,y = chr, by = "FIPS", all.x = TRUE)
  covid_daily$period = date_of_all[i]
  covid_us_hist <- rbind(covid_us_hist, covid_daily)
}

covid_us_hist <- dplyr::rename(covid_us_hist, c(county_fips = FIPS, death_num = Deaths, population = Population))
covid_us_hist$death_rate <- covid_us_hist$death_num/covid_us_hist$population*100000


km.func <- function(data.mat, cluster.num=3, seed=200) {
  
  set.seed(seed)
  cluster.num <- min(nrow(data.mat) - 1, cluster.num)
  data.mat <- na.omit(data.mat)
  
  km.result <- kmeans(dplyr::select(data.mat, -county_fips), cluster.num)
  return(
    tibble(
      county_fips = dplyr::pull(data.mat, county_fips),
      cluster = as.character(km.result$cluster)
    )
  )
}

kendall.func <- function(x.data, sd.data) {
  
  # TODO's:
  #   1. Align social determinants and "x" data by "county_fips"
  #   2. Re-separate "x column and sd data frame with select
  #   3. Do cor.test
  #   4. Rename the variables to VAR
  
  align <- dplyr::left_join(x.data, sd.data, by = "county_fips") %>% 
    dplyr::select(-dplyr::one_of(c("county_fips", "state_name", "county_name")))
  #browser()
  x <- as.numeric(dplyr::pull(align, 1))
  sd <- dplyr::select(align, -1)
  
  cor.res <- list()
  for (n in names(sd)) {
    y <- dplyr::pull(sd, n)
    if (is_character(y)) {
      # print(y)
      y <- readr::parse_number(y)
    }
    
    if (sum(is.na(y)) < 0.5 * length(y)) {
      # print(tibble(x, y, n))
      cor.res[[n]] <- cor.test(
        x = x, y = y, use = "pairwise.complete.obs", method = "kendall", exact = F
      )
    }
  }
  
  tibble(
    chr_code = names(cor.res),
    kendall_cor = sapply(cor.res, function(r) r$estimate),
    kendall_p = sapply(cor.res, function(r) r$p.value)
  )
}

#### state determinant output generation ####
generate_output <- function (state_choice){
  # this funciton takes an abbreviated state name and outputs kendall correlation
  if(state_choice %in% c("HI","WY")){
    # these are states not having enough data
    filename <- paste0("./data/social_det_gen/covid_determinants_output/kendall_cor", state_choice, ".csv")
    file.create(filename)
    fileConn<-file(filename)
    writeLines(c("chr_code,kendall_cor,kendall_p,DIR"), fileConn)
    close(fileConn)
    return(1)
  }
  covid.data <- subset(covid_us_hist, period %in% date_of_all)
  covid.data <- covid.data %>% drop_na(death_num,population)
  
  mort.rate <- covid.data %>% dplyr::filter(
    state_abbr == state_choice,
    period == tail(date_of_all, n=1)
  ) %>%
    dplyr::mutate(
      # death_rate = death_num / population * 10^5
      #death_rate = cut(death_rate, bin.geo.mort("Despair"))
    ) %>%
    dplyr::select(county_fips, death_rate)
  
  kendall.cor <- mort.rate %>% 
    dplyr::mutate(VAR = death_rate) %>%
    kendall.func(chr.data.2019) %>%
    dplyr::mutate(
      DIR = dplyr::if_else(
        kendall_cor <= 0,
        "Protective",
        "Destructive"
      ),
      chr_code = chr.namemap.2019[chr_code, 1]
    ) %>% na.omit()
  
  # Sort by kendall.cor
  kendall.cor.new <- kendall.cor %>% 
    dplyr::filter(kendall_p < 0.1) %>% 
    dplyr::arrange(desc(kendall_cor)) %>% 
    dplyr::top_n(15, abs(kendall_cor)) %>% 
    dplyr::mutate(chr_code = reorder(chr_code, kendall_cor))
  
  write_csv(kendall.cor.new, paste0("./data/social_det_gen/covid_determinants_output/kendall_cor", state_choice, ".csv"))
}

make_state_det_image <- function(state_name){
  # state choice is a string selected from R's builtin array state.name
  state_choice = state.abb[which(state.name == state_name)]
  # this function takes some prebaked data and makes a realtime determinants plot given some state selection
  input <- read.csv(paste0("./data/social_det_gen/covid_determinants_output/kendall_cor", state_choice, ".csv"))
  if(nrow(input) > 0) {
    # updatePickerInput(session, "determinant_choice", selected = kendall.cor.new$chr_code[[1]])
    output_plot <- input %>% 
      ggplot(
        aes(
          #x = reorder(chr_code, kendall_cor), 
          x = chr_code, 
          y = kendall_cor, 
          color = DIR, 
          fill = DIR)
      ) + 
      
      # Lolipop chart
      geom_point(stat = 'identity', size = 12) + 
      geom_segment(
        size = 1,
        aes(
          y = 0, 
          #x = reorder(chr_code, kendall_cor), 
          x = chr_code, 
          yend = kendall_cor, 
          #xend = reorder(chr_code, kendall_cor), 
          xend = chr_code, 
          color = DIR
        )
      ) +
      geom_text(
        aes(
          label = chr_code, 
          y = ifelse(DIR == "Protective", 0.1, -0.1),
          hjust = ifelse(DIR == "Protective", 0, 1)
        ), 
        color = "#565254", 
        size = 4
      ) +
      geom_text(
        aes(label = round(kendall_cor, 2)), 
        color = "#565254", 
        size = 3
      ) +
      
      # Coordinates
      coord_flip() + 
      scale_y_continuous(breaks = seq(-1, 1, by = .2), limits = c(-1, 1)) +
      
      # Themes
      geom_hline(yintercept = .0, linetype = "dashed") + 
      labs(
        y = "Correlation",
        x = NULL,
        title = paste("Factors Associated with COVID-19 Deaths for", state.name[which(state.abb == state_choice)]),
      
        fill = "Relationship",
        color = "Relationship"
      )
  }
  #Display something else when there are no significant SD
  else {
    
    # empty plot, then put text on it ?
    output_plot <- ggplot() + theme_void() +
      geom_text(aes(x = 0, y = 0, label="There are no significant social determinants."))
    
  }
  return(output_plot)
  # for saving the output plot to a folder
  #ggsave(paste("./data/social_det_gen/covid_determinants_output/images/kendall", state_choice,".png", sep=""),output_plot, width = 6, height = 10)
}
# apply the generation function to each state and write data to csv
# commented out because I've already generated the data and I want to use it to generate the images using make_state_det_image on a per-state basis
#lapply(state.abb, generate_output)
#lapply(state.abb, make_state_det_image)
# generate plot for each csv and save to ./covid_determinants_output/images



