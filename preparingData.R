# preparingData.R
# Author: Gabriela Morgenshtern
# Date: Jan 2020
# Version 1.0
#
# This script works through downloading, processing, and integrating information from the
# STRING and GOSlim databases on Saccharomyces cerevisiae, in preparation for visualizing
# a network of high-confidence yeast genes with the "mitotic cell cycle" GOSlim annotation.
#
# Adapted from Boris Steipe's material for BCH441
#
# Dataset Access:
#      STRING data source:
#        Download page: https://string-db.org/cgi/download.pl?species_text=Saccharomyces+cerevisiae
#        Data: (20.8 mb) https://string-db.org/download/protein.links.full.v11.0/4932.protein.links.full.v11.0.txt.gz
#
#      GOSlim data source:
#        Info page: http://geneontology.org/docs/go-subset-guide/
#        Data: (3 mb) https://downloads.yeastgenome.org/curation/literature/go_slim_mapping.tab
#

if (!require(readr, quietly = TRUE)) {
  install.packages("readr")
}
if (!require(dplyr, quietly = TRUE)) {
  install.packages("dplyr")
}

library(readr)
library(dplyr)

# =============================================================================
# 1. Preparing STRING Data
# =============================================================================
#
# STRING has scored information on functional protein-protein interactions
# To read STRING data, it first needs to be downloaded from the online database

onlineData = gzcon(
  url(
    "https://string-db.org/download/protein.links.full.v11.0/4932.protein.links.full.v11.0.txt.gz"
  )
)
STR <- readr::read_delim(onlineData, delim = " ")

#####
# 1.1 Exploring our data
# You can also run these functions prior to subsetting, of course,
# but given the size of the dataset, let's focus on learning only about our columns of interest today

dim(STR) # get number of variables (16), and number of cases (rows, 1,845,966) in your dataset
head(STR) # first 5 rows of data
dplyr::glimpse(STR) # dplyr equivalent to head()
summary(STR) # simple stats on each column
#####

#####
# 1.2 How to subset our data:
#
# Filtering for columns of interest:
# Subset only IDs and combined_score column using base R functionality
STR <- STR[, c("protein1", "protein2", "combined_score")]

######
# Same, but using the dyplr. dplyr a worthwhile library to know for 
# data science tasks in R, and the select() function has many options
# that make filtering for specific variables (columns) in your dataset easier
# for more options, run ?dplyr::select in your R console
STR <- dplyr::select(STR, starts_with("protein"), combined_score)
######

# subset for the 100,000 highest confidence edges, then take a look at your data
STR <- STR[(STR$combined_score > 920),] # exploratory approach, ~106,000 edges
dplyr::glimpse(STR)

# Using dplyr for more precision:
STR <- dplyr::arrange(STR, desc(combined_score))
STR <- head(STR, 100000)
dplyr::glimpse(STR)

######
# Alternatively, top_n combines the functionality of both arrange() and head()
STR <- dplyr::top_n(STR, 100000, combined_score)
dplyr::glimpse(STR)
######
#####

#####
# 1.3 Clean the data into a more readable format
# IDs are formatted like 4932.YAL005C ... drop the "4932." prefix
# gsub(pattern, replacement, x): Use a regular expression in the pattern input 
STR$protein1 <- gsub("^4932\\.", "", STR$protein1)
STR$protein2 <- gsub("^4932\\.", "", STR$protein2)

# View your text-replacement results:
head(STR)
#####

# get a vector of gene names in this list
myIntxGenes <- unique(c(STR$protein1, STR$protein2))  

# =============================================================================
# 2. Preparing GOSlim Data
# =============================================================================
#
# GOSlim annotations give a broad overview of GO content 
# To read GOSlim data, it first needs to be downloaded from the online database
# TSVs files have is Tab Separated Values, which is a lot like a CSV, just delineated with
# a different (tab) character
onlineData = url("https://downloads.yeastgenome.org/curation/literature/go_slim_mapping.tab")

# NOTE: this call may take 1-2 minutes to complete
Gsl <- read.csv(
  onlineData,
  col.names = c(
    "ID",
    "name",
    "SGDId",
    "Ontology",
    "termName",
    "termID",
    "status"
  ),
  sep = "\t"
)

#####
# 2.1 Exploring our data

head(Gsl)
dplyr::glimpse(Gsl)
summary(Gsl)

# What cell cycle names does it contain?
myGslTermNames <- unique(Gsl$termName)  
length(myGslTermNames) # 169 unique terms

myGslTermNames[grep("cycle", myGslTermNames)]
# [1] "regulation of cell cycle"  "mitotic cell cycle"  "meiotic cell cycle"
#####

#####
# 2.2 Subsetting our data
# Choose "mitotic cell cycle" as the GOslim term to subset with, 
# then filter out for duplicate genes in your final list

scCCgenes <- unique(Gsl$ID[Gsl$termName == "mitotic cell cycle"])
# length(scCCgenes)  # 324 genes annotated to that term

######
# Alternative with dplyr
scCCgenes2 <- dplyr::select(filter(Gsl, termName == "mitotic cell cycle"), ID)
######
#####

#####
# 2.3 Integrating relevant scCCgenes data into our STR data
#
# How many of our scCCgenes can be found in the high-confidence 
# (combined score) STR data we've prepared?
sum(scCCgenes %in% myIntxGenes)  # 301 genes have high-confidence interactions
  
# Define scCCnet: the S. Cervisiae Cell Cycle network
# Subset all rows for which BOTH genes are in the GOslim cell cycle set
scCCnet <- STR[(STR$protein1 %in% scCCgenes) &
                 (STR$protein2 %in% scCCgenes),]

nrow(scCCnet) # 2455

######
# Alternative with dplyr
scCCnet2 <- dplyr::filter(STR, STR$protein1 %in% scCCgenes, STR$protein2 %in% scCCgenes)
nrow(scCCnet2) # 2455
######

# How many genes are there?
length(unique(c(scCCnet$protein1, scCCnet$protein2)))  # 276

# Each edge is listed twice - now remove duplicates.
#
# Step 1: make a vector: sort two names so the frist one is alphabetically
#         smaller han the second one. This brings the two names into a defined
#         order. Then concatenate them with a "." - the resulting string
#         is always the same, for any order. E.g. c("A", "B") gives "A.B"
#         and c("B", "A") also gives "A.B". This identifies duplicates.
#
# Vectorized functions like apply() are a unique concept to R:
# They work not just on a single value, but on a whole vector of values at the same time
x <- apply(
  cbind(scCCnet$protein1,
        scCCnet$protein2),
  1,
  FUN = function(x) {
    return(paste(sort(x), collapse = "."))
  }
)
head(x) # "YAL016W.YGR040W" "YAL016W.YOR014W" "YAL016W.YDL188C" ... etc.
sum(duplicated(x))  # 1227

# Step 2: drop all rows that contain duplicates in x
scCCnet <- scCCnet[!duplicated(x),]

# Confirm we didn't lose genes
length(unique(c(scCCnet$protein1, scCCnet$protein2)))  # 276, no change
# Network has 276 nodes, 1280 edges

save(scCCnet, file = "./data/scCCnet.RData")

# load("./data/scCCnet.RData")
# Use the statement above to load the scCCnet object when needed.
# This object can be used as any R dataframe you've seen today

# [END]
