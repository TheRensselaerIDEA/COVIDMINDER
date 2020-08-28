knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = "../")

source("./Modules/Source.R")

# Principal Component Analysis
pca.func <- function(data.mat, row.name, seed = 20) {
  set.seed(seed)
  pca.result <- prcomp(data.mat, retx = TRUE, center = FALSE)
  pca.result[["row.name"]] <- row.name
  return(pca.result)
}

# km.func

#' Kmeans clustering function
#'
#' @param data.mat data.frame in which one column is "county_fips" and other columns
#'                  are data to cluster by
#' @param cluster.num number of clusters to form 
#' @param seed random seed to use, is variable so that results can be replicated
#'
#' @return a tribble with two columns:
#'    county_fips: unique identifier for each county
#'    cluster: number corresponding to cluster county belongs to
#'
#' @examples
#' cdc.data %>%
#'   cdc.mort.mat(input$state_choice, input$death_cause) %>%
#'   km.func(4)
#'   
#' km.func(cdc.mort.mat(cdc.data, "NY", "Despair"), 5)
#' 
#' @author 
#' @export
km.func <- function(data.mat, cluster.num=3, seed=200) {
  
  set.seed(seed)
  cluster.num <- min(nrow(data.mat) - 1, cluster.num)
  data.mat <- na.omit(data.mat)
  
  km.result <- kmeans(dplyr::select(data.mat, -fips), cluster.num)
  return(
    tibble(
      fips = dplyr::pull(data.mat, fips),
      cluster = as.character(km.result$cluster)
    )
  )
}


# diana.func

#' Diana clustering function
#'
#' @param data.mat data.frame in which one column is "county_fips" and other columns
#'                  are data to cluster by
#' @param cluster.num number of clusters to form 
#' @param seed random seed to use, is variable so that results can be replicated
#'
#' @return a tribble with two columns:
#'    county_fips: unique identifier for each county
#'    cluster: number corresponding to cluster county belongs to
#'    
#' @examples
#' cdc.data %>%
#'   cdc.mort.mat(input$state_choice, input$death_cause) %>%
#'   diana.func(4)
#'   
#'   
#' diana.func(cdc.mort.mat(cdc.data, "NY", "Despair"), 5)
#'    
#' @author Ross DeVito
#' @export
diana.func <- function(data.mat, cluster.num=3, seed=200) {
  set.seed(seed)
  data.mat <- na.omit(data.mat)
  
  diana.tree <- diana(dplyr::select(data.mat, -county_fips), 
                      metric = "euclidean", 
                      stand = FALSE,
                      stop.at.k = FALSE,
                      trace.lev = 0)
  diana.clusters <- cutree(as.hclust(diana.tree), k = cluster.num)
  return(tibble(county.fips = data.mat$county_fips, cluster = diana.clusters))
}


# cluster.counties

#' Clusters counties using some specified clustering method
#' 
#' @note implemented in this way so that if/when additional clustering method
#'        is added, it can be added to function in an else if
#'
#' @param county.data data.frame in which one column is "county_fips" and other columns
#'                     are data to cluster by
#' @param cluster.method method to be used to cluster counties. One of set {"kmeans", "diana"}
#' @param cluster.num optional argument used by clustering methods where you must provide
#'                      desired number of clusters (e.g. kmeans)
#' @param seed random seed to be used by clustering algorithm
#'
#' @return a tribble with (num counties in state) rows and two columns:
#'    county_fips: unique identifier for each county
#'    cluster: number corresponding to cluster county belongs to
#'    
#'    e.g.
#'    # A tibble: 67 x 2
#'    county_fips cluster
#'    <chr>       <chr>  
#'  1 01001       1      
#'  2 01003       4      
#'  3 01005       2      
#'  4 01007       4      
#'  5 01009       4      
#'  6 01011       2      
#'  7 01013       2      
#'  8 01015       1      
#'  9 01017       1      
#'  10 01019       4      
#'  # … with 57 more rows
#'
#' @examples
#' state <- "AL"
#' death.cause <- "Despair"
#' cluster.counties(cdc.mort.mat(cdc.data, state, death.cause),
#'                  cluster.method="kmeans",
#'                  cluster.num=4)
#' 
#' 
#' cluster.counties(cdc.mort.mat(cdc.data, input$state_choice, input$death_cause),
#'                  cluster.method="diana",
#'                  cluster.num=4)
#' 
#' @author Ross DeVito
#' @export
cluster.counties <- function(county.data, cluster.method="kmeans", cluster.num=4, seed=200) {
  if (cluster.method == "kmeans"){
    return(km.func(county.data, cluster.num, seed))
  } else if (cluster.method == "diana") {
    return(diana.func(county.data, cluster.num, seed))
  }
}

#cdc.mort.mat

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
    tidyr::spread(key = period, value = death_rate) %>%
    dplyr::arrange(county_fips)
}


# order.county.clusters

#' Given a dataframe of counties and their original cluster and data.frame that can
#'  be used to map from original cluster to ordered cluster, returns a data.frame that
#'  gives counties with their ordered cluster
#'
#' @param county.clusters a data.frame or tribble in the form returned by cluster.counties()
#' @param cluster.order.map a data.frame that has the original cluster names as the row names
#'                           and the ordered clusters in a column "ord". Data.frames returned
#'                           by get.cluster.order.map are of this form
#'
#' @return a tribble with (num counties in state) rows and two columns:
#'    county_fips: unique identifier for each county
#'    cluster: number corresponding to cluster county belongs to, ordered such that 1 has the
#'              lowest deathrate the highest cluster number (the number of clusters) the highest
#'              
#'    Same format at cluster.counties(). See for example
#'
#' @examples
#' state <- "AL"
#' death.cause <- "Despair"
#' county.clusters <- cluster.counties(cdc.mort.mat(cdc.data, state, death.cause),
#'                                      cluster.method="kmeans",
#'                                      cluster.num=4)
#' cluster.deathrates <- get.cluster.deathrate.during.time(county.clusters, cdc.data, death.cause)
#' cluster.order.map <- get.cluster.order.map(cluster.deathrates)
#' county.clusters.ordered <- order.county.clusters(county.clusters, cluster.order.map)
#' 
#' 
#' order.county.clusters(mort.cluster.raw(), mort.cluster.map())
#' 
#' @author Ross DeVito
#' @export
#' order.county.clusters(mort.cluster.raw(), mort.cluster.map())
order.county.clusters <- function(county.clusters, cluster.order.map) {
  return(
    dplyr::mutate(county.clusters, cluster = cluster.order.map[cluster, "ord"])
  )
}

# Kendall correlation core function
kendall.func <- function(x.data, sd.data) {
  
  # TODO's:
  #   1. Align social determinants and "x" data by "county_fips"
  #   2. Re-separate "x column and sd data frame with select
  #   3. Do cor.test
  #   4. Rename the variables to VAR
  
  align <- dplyr::left_join(x.data, sd.data, by = "fips") %>% 
    dplyr::select(-dplyr::one_of(c("fips")))
  #browser()
  x <- as.numeric(dplyr::pull(align, VAR))
  sd <- dplyr::select(align, -VAR)
  
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
    code = names(cor.res),
    kendall_cor = sapply(cor.res, function(r) r$estimate),
    kendall_p = sapply(cor.res, function(r) r$p.value)
  )
}
