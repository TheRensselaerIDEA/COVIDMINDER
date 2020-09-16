#' Used to visualize optimal k for k means clustering
#' 
#' @note most recent use is beta.01
#'
#' @param data 
#' @param cluster.num 
#' @param seed 
#'
#' @return a plot used to visualize optimal k for k means clustering
#'
#' @examples
#' In beta.01:
#' # Plot that help select different clustering algo
#' mort.mat <- cdc.generateMat.state(cdc.data, input$state_choice)
#' km.wssplot(select(mort.mat, -c(1, 2)))
#' 
#' @author 
#' @export
km.wssplot <- function(data, cluster.num = 25, seed = 20){
  
  # Initialize table
  cluster.num <- min(nrow(data) - 1, cluster.num)
  wss <- data.frame(cluster = 1:cluster.num, quality = 0)
  
  # Do k-means clustering, reduce data points to `cluster.num` clusters
  for (i in 1:cluster.num){
    set.seed(seed)
    wss[i, "quality"] <- sum(kmeans(data, centers = i)$withinss)
  }
  
  # Plot the qualities of k-means by cluster
  plot <- ggplot(data = wss, aes(x = cluster, y = quality)) +
    geom_point() +
    geom_line() +
    labs(
      x = "Cluster", 
      y = "Quality"
    ) +
    scale_x_continuous(breaks = 1:cluster.num)
  return(plot)
}

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
  
  km.result <- kmeans(dplyr::select(data.mat, -county_fips), cluster.num)
  return(
    tibble(
      county_fips = dplyr::pull(data.mat, county_fips),
      cluster = as.character(km.result$cluster)
    )
  )
}

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

#' Given a set of county to cluster number pairs and the full cdc.data,
#' for each cluster for each time window in the data (e.g. 2000-2002, 2003-2005)
#' an average death rate is calculated.
#' 
#' Fiters out counties with na death_num or population values to avoid na death_rate
#'  return values.
#'
#' @param mort.clusters a data.frame or tribble in the same form as one returned by 
#'                        cluster counties. It has two columns:
#'                              county_fips: unique identifier for each county
#'                              cluster: number corresponding to cluster county belongs to 
#' @param full.cdc.data Full cdc dataset from Loader_CDC.R cdc.data
#' @param death.cause String that matches death_cause column in full.cdc.data
#'                      ("Despair", "Assault", "Cancer", or "Cardiovascular")
#'
#' @return a tribble that for each cluster for each time window has an average death rate
#'    and a count of counties in that cluster (count of counties does not vary with time).
#'    Is of dimension (n clusters) * (n time windows) rows and four columns:
#'      period: time window (e.g. 2000-2002, 2003-2005)
#'      cluster: cluster number used to indentify cluster
#'      death_rate: average death rate for that cluster in that time window
#'      count: number of counties in that cluster, not time variant 
#'      
#'   e.g.
#'   A tibble: 24 x 4
#'    period      cluster death_rate count
#'    <chr>       <chr>        <dbl> <int>
#'    1 2000-2002  1            24.0   33
#'    2 2000-2002  2            6.25   13
#'    3 2000-2002  3            7.89   7
#'    4 2000-2002  4            31.4   14
#'    5 2003-2005  1            24.5   33
#'    6 2003-2005  2            2.06   13
#'    7 2003-2005  3            1.86   7
#'    8 2003-2005  4            34.2   14
#'    9 2006-2008  1            34.0   33
#'   10 2006-2008  2            2.06   13
#'   … with 14 more rows
#'
#' @examples
#' state <- "AL"
#' death.cause <- "Despair"
#' county.clusters <- cluster.counties(cdc.mort.mat(cdc.data, state, death.cause),
#'                                      cluster.method="kmeans",
#'                                      cluster.num=4)
#' cluster.deathrates.counts <- get.cluster.deathrate.during.time(county.clusters, 
#'                                                                cdc.data, 
#'                                                                death.cause)
#' 
#' 
#' get.cluster.deathrate.during.time(mort.cluster.raw(), 
#'                                   cdc.data,
#'                                   death.cause=input$death_cause)
#' 
#' @author Ross DeVito
#' @export
get.cluster.deathrate.during.time <- function(mort.clusters, full.cdc.data, death.cause) {
  full.cdc.data[is.na(full.cdc.data)] <- 0
  return(
    full.cdc.data %>%
      dplyr::filter(death_cause == death.cause) %>%
      dplyr::right_join(mort.clusters, by = "county_fips") %>%
      dplyr::group_by(period, cluster) %>%
      dplyr::summarise(
        death_rate = sum(death_num) / max(sum(population), 1) * 10^5,
        count = n()
      ) %>% 
      dplyr::ungroup()
  )
}

#' Ranks clusters by death rate in a given time period and returns a data.frame
#'  that can be used to map the original cluster number to the ordered number
#' 
#' The resulting rank gives the cluster with the lowest deathrate the ord value
#'  1 and the cluster with the highest deathrate the highest ord
#'
#' @param cluster.deathrates a data.frame or tribble in the form of one returned 
#'                            by get.cluster.deathrate.during.time
#' @param time.period a string found in period column of cluster.deathrates
#'
#' @return a data.frame where rowname is the string of the cluster number (e.g. "1", "2")
#'          and their are two columns:
#'            count: number of counties in that cluster
#'            ord: the ordered clusters by deathrate such that 1 has the lowest deathrate 
#'                  and the highest value the highest deathrate
#'          e.g.
#'              count ord
#'            2    13   1
#'            1    33   2
#'            3     7   3
#'            4    14   4
#'
#' @examples
#' cluster.deathrates <- get.cluster.deathrate.during.time(county.clusters, cdc.data, death.cause)
#' get.cluster.order.map(cluster.deathrates)
#' get.cluster.order.map(cluster.deathrates, "2000-2002")
#' 
#' 
#' get.cluster.order.map(mort.avg.cluster.raw(), time.period="2015-2017")
#' 
#' @author Ross DeVito
#' @export
get.cluster.order.map <- function(cluster.deathrates, time.period="2015-2017") {
  return(
    cluster.deathrates %>% 
      dplyr::filter(period == time.period) %>%
      dplyr::arrange(death_rate) %>% 
      dplyr::mutate(ord = as.character(1:n())) %>% 
      dplyr::select(-c(period, death_rate)) %>% 
      textshape::column_to_rownames("cluster")
  )
}

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

#' Given a data.frame in the form returned by get.cluster.deathrate.during.time and
#'  a data.frame that can be used to map from original cluster to ordered cluster, 
#'  returns a data.frame that is the same as the first data.frame, but with cluster
#'  being the ordered cluster
#'
#' @param cluster.deathrates.dt a data.frame in the form returned by get.cluster.deathrate.during.time
#' @param cluster.order.map a data.frame that has the original cluster names as the row names
#'                           and the ordered clusters in a column "ord". Data.frames returned
#'                           by get.cluster.order.map are of this form
#'
#' @return a tribble that for each cluster for each time window has an average death rate
#'          and a count of counties in that cluster (count of counties does not vary with time).
#'          Is of dimension (n clusters) * (n time windows) rows and four columns:
#'            period: time window (e.g. 2000-2002, 2003-2005)
#'            cluster: number corresponding to cluster county belongs to, ordered such that 1 has the
#'                      lowest deathrate the highest cluster number (the number of clusters) the highest
#'            death_rate: average death rate for that cluster in that time window
#'            count: number of counties in that cluster, not time variant 
#'            
#'          Same format at get.cluster.deathrate.during.time(). See for example
#'
#' @examples
#' state <- "AL"
#' death.cause <- "Despair"
#' county.clusters <- cluster.counties(cdc.mort.mat(cdc.data, state, death.cause),
#'                                      cluster.method="kmeans",
#'                                      cluster.num=4)
#' cluster.deathrates <- get.cluster.deathrate.during.time(county.clusters, cdc.data, death.cause)
#' cluster.order.map <- get.cluster.order.map(cluster.deathrates)
#' cluster.deathrates.ordered <- order.cluster.deathrate.during.time(cluster.deathrates, cluster.order.map)
#' 
#' 
#' order.cluster.deathrate.during.time(mort.avg.cluster.raw(), mort.cluster.map())
#' 
#' @author Ross DeVito
#' @export
order.cluster.deathrate.during.time <- function(cluster.deathrates.dt, cluster.order.map) {
  return(dplyr::mutate(cluster.deathrates.dt, cluster = cluster.order.map[cluster, "ord"])
  )
}