knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = "../")

source("./Modules/Source.R")
source("./GWAS/helper.R")

data <- readRDS("./Preprocessing_FTS_Outputs/06-28-2020data.Rds")

to_cluster <- subset(data, select = c(fips, Deaths))

to_cluster <- drop_na(to_cluster)

mort.cluster.raw <- cluster.counties(to_cluster, cluster.method="kmeans", cluster.num=4)

mort.avg.cluster.raw <- data %>%
                          dplyr::right_join(mort.cluster.raw, by = "fips") %>%
                          dplyr::group_by(cluster) %>%
                          dplyr::summarise(
                            death_rate = sum(Deaths) / sum(Population) * 10^5,
                            count = n()
                          ) %>% 
                          dplyr::ungroup()

mort.cluster.map <- mort.avg.cluster.raw %>% 
                      dplyr::arrange(death_rate) %>% 
                      dplyr::mutate(ord = as.character(1:n())) %>% 
                      dplyr::select(-c(death_rate)) %>% 
                      textshape::column_to_rownames("cluster")

mort.cluster.ord <- order.county.clusters(mort.cluster.raw, mort.cluster.map)

mort.avg.cluster.ord <- dplyr::mutate(mort.avg.cluster.raw, cluster = mort.cluster.map[cluster, "ord"])

mort.rate <- data %>% 
               dplyr::select(fips, death_rate)

nums <- unlist(lapply(data, is.numeric))
numeric_data <- data[ , nums]
numeric_data$fips <- data$fips

kendall.cor <- mort.rate %>% 
                 dplyr::mutate(VAR = death_rate) %>%
                 kendall.func(numeric_data) %>%
                 dplyr::mutate(
                   DIR = dplyr::if_else(
                     kendall_cor <= 0,
                     "Protective",
                     "Destructive"
                     )
                   #chr_code = chr.namemap.2019[chr_code, 1]
                 ) %>% na.omit() %>% 
                 dplyr::arrange(desc(kendall_cor)) %>% 
                 dplyr::mutate(code = reorder(code, kendall_cor))

#using Benjamin Hochberg's p.adjust() method to adjust P-values for multiple hypothesis testing to filter out factors that are less relevant.
corrected_pvalues <- p.adjust(kendall.cor$kendall_p)
selected_SDs <- cbind(kendall.cor, Corrected_P_Vals = corrected_pvalues)
selected_SDs <- selected_SDs %>%
  dplyr::filter(Corrected_P_Vals < 0.05) %>% 
  dplyr::arrange(desc(kendall_cor))

saveRDS(selected_SDs, "./GWAS/selected_SDs_1.Rds")

#Printing out the few selected factors for deaths of despair.
print(selected_SDs)