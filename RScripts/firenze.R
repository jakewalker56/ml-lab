Project = "~/github"

path= "/projects/data/"
setwd(paste(Project, path, sep=""))

#### **** Marriage in Renaissance Florence **** ####

## load the igraph package
library(igraph) 

florence <- as.matrix(read.table("firenze.txt"))

## create the graph object
marriage <- graph.edgelist(florence, directed=FALSE)

## set some color atributes (V() gives back the 'vertices' = nodes)
V(marriage)$color = "pink"
V(marriage)["Medici"]$color <- "blue"
V(marriage)$frame.color = 0
V(marriage)$label.color = "black"

## plot it
plot(marriage, edge.curved=FALSE)

## print the degree for each family
sort(degree(marriage))

## calculate and color a couple shortest paths
PtoA <- get.shortest.paths(marriage, from="Peruzzi", to="Acciaiuoli")
## you shortest paths are a list, with element vpath.  
## First element of vpath is then your vector 
## of vertices along the path.  confusing, sorry.
V(marriage)$name[PtoA$vpath[[1]]]
GtoS <- get.shortest.paths(marriage, from="Ginori", to="Strozzi")
V(marriage)$name[GtoS$vpath[[1]]]
## set fat edges and default grey (E() returns edges)
E(marriage)$width <- 2
E(marriage)$color <- "grey"
E(marriage, path=PtoA$vpath[[1]])$color <- "green"
E(marriage, path=GtoS$vpath[[1]])$color <- "blue"
plot(marriage)

## print the betweenness for each family
sort(round(betweenness(marriage),1))

