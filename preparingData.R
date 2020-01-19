# preparingData.R
# Author: Gabriela Morgenshtern
# Date: November 2019
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

if (! require(readr, quietly = TRUE)) {
  install.packages("readr")
}

library(readr)

# =============================================================================
# 1. Preparing STRING Data
# =============================================================================
# 
# STRING has scored information on functional protein-protein interactions
# To read STRING data, it first needs to be downloaded from database 
# and put into our /data directory. We've already done that for you
# EXPLAIN RDS and saving space and loading RDS objects

onlineData = gzcon(url("https://string-db.org/download/protein.links.full.v11.0/4932.protein.links.full.v11.0.txt.gz"))
STR <- read_delim(onlineData, delim = " ")

# 1. How to subset our data:

# Subset only IDs and combined_score column
STR <- STR[ , c("protein1", "protein2", "combined_score")]

head(STR)
sum(STR$combined_score > 909)  # 100270 edges

# subset for 100,000 highest confidence edges
STR <- STR[(STR$combined_score > 909), ]

# IDs are formatted like 4932.YAL005C ... drop the "4932." prefix
# gsub(pattern, replacement, x): Use a regular expression to replace
# 
STR$protein1 <- gsub("^4932\\.", "", STR$protein1)
STR$protein2 <- gsub("^4932\\.", "", STR$protein2)

# View your text-replacement results:
head(STR)

# get a vector of gene names in this list
myIntxGenes <- unique(c(STR$protein1, STR$protein2))  # yeast systematic gene
# names


# GOSlim functional annotations
# EXPLAIN WHAT IS GOSlim
# Read GOSlim data  (needs to be downloaded from database, see URL in Notes)
# tsv is Tab Separated Values, which is a lot like a CSV, just delineated with 
# a different (tab) character
onlineData = url("https://downloads.yeastgenome.org/curation/literature/go_slim_mapping.tab")

Gsl <- read.csv(onlineData,
                col.names = c("ID",
                              "name",
                              "SGDId",
                              "Ontology",
                              "termName",
                              "termID",
                              "status"),
                sep = "\t")

# head(Gsl)
#
# What cell cycle names does it contain?
# myGslTermNames <- unique(Gsl$termName)  # 169 unique terms
# myGslTermNames[grep("cycle", myGslTermNames)]
# [1] "regulation of cell cycle"  "mitotic cell cycle"  "meiotic cell cycle"
#
# Choose "mitotic cell cycle" as the GOslim term to subset with
#

scCCgenes <- unique(Gsl$ID[Gsl$termName == "mitotic cell cycle"])
# length(scCCgenes)  # 324 genes annotated to that term

# sum(scCCgenes %in% myIntxGenes)  # 294 of these have high-confidence
#                                     # interactions

# Define scCCnet ... the S. Cervisiae Cell Cycle network
# Subset all rows for which BOTH genes are in the GOslim cell cycle set
scCCnet <- STR[(STR$protein1 %in% scCCgenes) &
                 (STR$protein2 %in% scCCgenes), ]

# How many genes are there?
# length(unique(c(scCCnet$protein1, scCCnet$protein2)))  #261

# Each edge is listed twice - now remove duplicates.
#
# Step 1: make a vector: sort two names so the frist one is alphabetically
#         smaller han the second one. This brings the two names into a defined
#         order. Then concatenate them with a "." - the resulting string
#         is always the same, for any order. E.g. c("A", "B") gives "A.B"
#         and c("B", "A") also gives "A.B". This identifies duplicates.
# 
# EXPLAIN VECTORIZED FNS IN R
x <- apply(cbind(scCCnet$protein1,
                 scCCnet$protein2),
           1,
           FUN = function(x) { return(paste(sort(x), collapse = ".")) })
# head(x) # "YAL016W.YGR040W" "YAL016W.YOR014W" "YAL016W.YDL188C" ... etc.

# sum(duplicated(x))  # 1280

# Step 2: drop all rows that contain duplicates in x
scCCnet <- scCCnet[! duplicated(x), ]

# Confirm we didn't lose genes
# length(unique(c(mySubnet$protein1, mySubnet$protein2)))  # 261, no change
# Network has 261 nodes, 1280 edges

save(scCCnet, file = "./data/scCCnet.RData")

# load("./data/scCCnet.RData")   
# Use the statement above to load the object when needed
# This line will load an scCCnet object which you can 
# interact with as any R dataframe interactions

# [END]
