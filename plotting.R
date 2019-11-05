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

# With these settings, size of a node increases with degree, colour is dictated by 
# community membership. We can see that there are several fairly well connected nodes 
# with high degree of connectivity to other nodes. These nodes tend to be members of 
# larger communities - this makes sense biologically, we could expect a protein in a 
# large group of interacting proteins has many interactions. There are two distinctly 
# large and well connected communities.









#What is the size gap in our scCCnet based graph between the 1st and 2nd
#largest communities?
set.seed(1234)
getSize(stringNetwork)

#The size gap is 6. How does this compare to a randomly generated graph?

#Produce 5 random graphs
rGs <- list()
for (i in seq(5)){
  rGs[[i]] <- igraphToRIgraph(stringNetwork)
}

#Find the size gap for each graph
rGSamples <- lapply(rGs, getSize)
rGSamples <- unlist(rGSamples)

#Create a histogram of size gaps in randomly generated graphs
#Modified from the FND-MAT-Graphs_and_networks.R Version 1.0
brk <- seq(min(rGSamples)-0.5, max(rGSamples)+0.5, by=1)
hist(rGSamples, breaks = brk, col="coral2",
     xlim = c(min(rGSamples)-1,max(rGSamples)), xaxt = "n",
     main = "5 samples of randomly generated graph",
     xlab = "Size difference between largest and second largest community", ylab = "Frequency")
axis(side = 1, at = min(rGSamples):max(rGSamples))

#6 is very rarely, if ever the size gap between the first and second largest communities
#in a randomly generated graph.

#Lets compare with a histogam of 5 samples of the scCCnet generated graph
#This represents the distribution only one graph, where differing community
#structure comes solely from the call to cluster_infomap(). However, the mean
#will be meaningful to compare with the random graph samples

myGSamples <- replicate(5, getSize(stringNetwork))

brk <- seq(min(myGSamples)-0.5, max(myGSamples)+0.5, by=1)
hist(myGSamples, breaks = brk, col="cornflowerblue",
     xlim = c(-1,max(myGSamples)), xaxt = "n",
     main = "5 samples of scCCnet generated graph",
     xlab = "Size difference between largest and second largest community", ylab = "Freqency")
axis(side = 1, at = 0:max(myGSamples))


#Box plot of community structures to see these side to side
samples <- cbind(rGSamples, myGSamples)
boxplot.matrix(samples, col = c("coral2", "cornflowerblue"),
               main = "Community structure of 5 samples",
               ylab = "Size difference between largest and second largest community",
               xlab = "Origin")

#Test difference in mean between scCCnet and random graph samples for significance
t.test(rGSamples, myGSamples)

#We cannot accept the null hypothesis; there is a significant difference in the means of
#the two groups.

#The average size gap in the 1st and 2nd largest community for random
#graphs sharing the same degree as the scCCnet network is 243.120, versus scCCnet's 7.236.
#They do not seem to share a similar community structure, the scCCnet community structure
#is distictly difference from that of a random graph of the same degree.
#This implies that the community structure of the cell cycle related proteins in scCCnet
#is related to their biological function. This is expected, as the edges in the graph
#were generated from the interaction scores of these proteins. We can anticipate
#that interacting proteins have some shared biological function towards the cell cycle.
