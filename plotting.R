# plotting.R
# Author: Cait Harrigan
# Date: November 2019
# Version 1.0

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
# 2. Define functions
# =============================================================================

# We will define a few functions that let us do certain operations on the data.

getGraphSize <- function(G){
  # Find the difference in sizes between the largest 
  # and second largest communities in graph G
  # Parameters:
  #   G: an igraph
  #
  # Value: an integer
  
  # get cluster sizes
  sizes <- sizes(cluster_infomap(G))
  
  # order sizes as a vector
  sizesOrdered <- as.vector(sizes)
  
  # in some cases there is only one community
  if (length(sizesOrdered) == 1){
    # in this case, only return this community
    return(sizesOrdered[1])
  }
  
  # find difference between first and second largest community
  return(sizesOrdered[1] - sizesOrdered[2])
}

randGraph <- function(G){
  # Make a random graph of same degree as an input igraph G
  # Parameters:
  #   G: an igraph
  #
  # Value: an undirected igraph with same degree as G
  
  d <- degree(G)
  return(sample_degseq(d, method = "vl"))
}

# =============================================================================
# 3. Plot network

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
networkLayout <- layout_with_graphopt(stringNetwork, charge = 0.002, mass=100, spring.constant = 0.5)

# Plot!
plot( stringNetwork,
      layout = networkLayout,
      rescale = F,
      xlim = c(min(networkLayout[,1]) * 0.99, max(networkLayout[,1]) * 1.01),
      ylim = c(min(networkLayout[,2]) * 0.99, max(networkLayout[,2]) * 1.01),
      vertex.color=rainbow(max(membership(comms)+1))[membership(comms)+1],
      vertex.size = 700 + (90 * degree(stringNetwork)),
      vertex.label = NA,
      main = "Network from STRING data"
    )

# What biological hypotheses or conclusions can we draw?

# [END]