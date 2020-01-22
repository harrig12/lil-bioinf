# plotting.R
# Author: Cait Harrigan
# Date: Jan 2020
# Version 1.1

# =============================================================================
# 1. Set Up
# =============================================================================
# With the STRING data that we downloaded and previewed previously, we will 
# create a network (aka. graph) representing protein interactions, and observe how 
# certain proteins group together in "communities" with shared similarities

# load an R package that will be useful for our task
if (!require(igraph)) {
  install.packages("igraph")
  library(igraph)
}

# load data from STRING database, prepared in getData.R
load("~/Downloads/scCCnet.rdata")
preparedData <- scCCnet
#load("./STRING.RData")

# =============================================================================
# 2. Plot network

# This sections is modified from Boris Steipe's 
# FND-MAT-Graphs_and_networks.R Version 1.0 
# https://github.com/hyginn/ABC-units
# =============================================================================

# Create an igraph object from our STRING network, by plotting the network 
# according to community membership of each protein (represented as nodes)

# An edge in our network is defined between a pair of proteins. For all the pairs 
# in our dataset, we want to create an edge. We select these as columns 1 and 2 of 
# our prepared dataset. 
stringNetwork <- graph_from_edgelist(as.matrix(preparedData[c(1,2)]), directed = F)

# With functions from iGraph package, prepare network for plotting
communities <- cluster_infomap(stringNetwork)

# Play with the parameters of this function for cool networks
networkLayout <- layout_with_graphopt(stringNetwork, charge = 0.002, mass=100, spring.constant = 0.5)

# Have a look at other possible layout algorithms available
help.search("layout_with")

# Plot!
plot( stringNetwork,
      layout = networkLayout,
      rescale = F,
      xlim = c(min(networkLayout[,1]) * 0.99, max(networkLayout[,1]) * 1.01),
      ylim = c(min(networkLayout[,2]) * 0.99, max(networkLayout[,2]) * 1.01),
      vertex.color=rainbow(max(membership(communities)+1))[membership(communities)+1],
      vertex.size = 700 + (90 * degree(stringNetwork)),
      vertex.label = NA,
      main = "Network from STRING data"
    )

# What biological hypotheses or conclusions can we draw?

# [END]
