#### Search for "California" 
library(igraph)

## read the edges and merge with edge names
caedges <- read.csv("CaliforniaEdges.csv")
casites <- scan("CaliforniaNodes.txt", "character")
edgemat <- cbind(casites[caedges$from], casites[caedges$to])
## create a graph
calink <- graph.edgelist(edgemat)

## consider the neighborhood of latimes.com landing page
## just one link away
latimes <- graph.neighborhood(calink, 1, V(calink)["http://www.latimes.com/HOME/"])[[1]]
V(latimes)$color <- "gold"
V(latimes)["http://www.latimes.com/HOME/"]$color <- "red"
plot(latimes, vertex.label=NA, vertex.frame.color=0, edge.arrow.width=.75)
## two links away
latimes2 <- graph.neighborhood(calink, 2, V(calink)["http://www.latimes.com/HOME/"])[[1]]
V(latimes2)$color <- "green"  
V(latimes2)[V(latimes)$name]$color <- "gold"
V(latimes2)["http://www.latimes.com/HOME/"]$color <- "red"
plot(latimes2,  edge.arrow.width=0, edge.curved=FALSE,
     vertex.label=NA, vertex.frame.color=0, vertex.size=6)

## betweenness
V(calink)$name[order(betweenness(calink), decreasing=TRUE)[1:10]]

## page rank
search <- page.rank(calink)$vector
casites[order(search, decreasing=TRUE)[1:20]]
