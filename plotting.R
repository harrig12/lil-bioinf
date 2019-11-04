#load data prepared from STRING database
load("~/Downloads/scCCnet.RData")

if (!require(igraph)) {
  install.packages("igraph")
  library(igraph)
}


igraphToSize <- function(G){
  # Find the difference in sizes between the largest and second largest communities
  # in graph G
  # Parameters:
  #   G: an igraph
  #
  # Value: an integer
  
  #get cluster sizes
  sizes <- sizes(cluster_infomap(G))
  #order sizes as a vector
  sizesOrdered <- as.vector(sizes)
  
  #find difference between first and second largest community
  #in some cases there is only one community
  if (length(sizesOrdered) == 1){
    return(sizesOrdered[1])
  }
  
  return(sizesOrdered[1] - sizesOrdered[2])
}

igraphToRIgraph <- function(G){
  # Make a random graph of same degree as an input igraph G
  # Parameters:
  #   G: an igraph
  #
  # Value: an undirected igraph with same degree as G
  
  myDegree <- degree(G)
  return(sample_degseq(myDegree, method = "vl"))
}

#Create an igraph object from scCCnet
#Exclude the combined scores
set.seed(12543)
myG <- graph_from_edgelist(as.matrix(scCCnet[-3]), directed = F)

#Plot the scCCnet network according to community membership
#Modified from the FND-MAT-Graphs_and_networks.R Version 1.0

comms <- cluster_infomap(myG)
myGxy <- layout_with_graphopt(myG, charge = 0.0017, mass=100, spring.constant = .5)

oPar <- par(mar= c(0,4,4,4)) # Turn margins off
plot(myG,
     layout = myGxy,
     rescale = F,
     xlim = c(min(myGxy[,1]) * 0.99, max(myGxy[,1]) * 1.01),
     ylim = c(min(myGxy[,2]) * 0.99, max(myGxy[,2]) * 1.01),
     vertex.color=rainbow(max(membership(comms)+1))[membership(comms)+1],
     vertex.size = 700 + (90 * degree(myG)),
     vertex.label = NA,
     main = "scCCnet Network")
par(oPar)

#Size of a node increases with degree, colour is dictated by community membership.
#We can see that there are several fairly well connected nodes with high degree.
#These nodes tend to be members of larger communities - this makes sense biologically,
#we could expect a protein in a large group of interacting proteins has many interactions.
#There are two distinctly large and well connected communities.

#What is the size gap in our scCCnet based graph between the 1st and 2nd
#largest communities?
set.seed(1234)
igraphToSize(myG)

#The size gap is 6. How does this compare to a randomly generated graph?

#Produce 5 random graphs
rGs <- list()
for (i in seq(5)){
  rGs[[i]] <- igraphToRIgraph(myG)
}

#Find the size gap for each graph
rGSamples <- lapply(rGs, igraphToSize)
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

myGSamples <- replicate(5, igraphToSize(myG))

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
