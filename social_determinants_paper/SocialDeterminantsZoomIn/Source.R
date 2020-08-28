source("Librarian.R")
library(withr)
library(stringdist)

# Read in new Social Determinants definitions
SocialDeterminants <- read_csv("SocialDeterminants.csv")

final.determinants <- SocialDeterminants[SocialDeterminants$Keep == 1,]["Code"]
final.determinants <- append(final.determinants$Code, "county_fips", after = 0)

# Load all data
chr.data.2019 <- readRDS("chr.data.2019.rds")
chr.data.2019 <- chr.data.2019 %>%
  as_data_frame %>%
  select(final.determinants)

# Load name map and its inverse
chr.namemap.2019 <- SocialDeterminants %>% select("Code", "Name")
chr.namemap.2019 <- column_to_rownames(chr.namemap.2019, "Code")
names(chr.namemap.2019)[1] <- "name"

geo.namemap <- readRDS("geo.namemap.rds")

source("CDC_Lib.R")         # Function definitions from Loader_CDC.R
source("Clustering_Lib.R")
source("Analyzer_Correlation.R")