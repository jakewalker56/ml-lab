Project = "~/github"

path= "/projects/data/"
setwd(paste(Project, path, sep=""))


## actors network example
?plot.igraph
library(igraph)
library(gamlr)
library(hash)
source("../RScripts/naref.R")

### GRAPH
## read in a graph in the `graphml' formal: xml for graphs.
## it warns about pre-specified ids, but we want this here
## (these ids match up with the castlists in movies.txt)
actnet <- read.graph("actors.graphml",format="graphml")

### TRANSACTION
## read in the table of actor ids for movies
## this is a bit complex, because the movie names
## contain all sorts of special characters.
movies <- read.table("movies.txt", sep="\t", 
                     row.names=1, as.is=TRUE, comment.char="", quote="")

## it's a 1 column matrix.  treat it like a vector
movies <- drop(as.matrix(movies))
## each element is a comma-separated set of actor ids.  
## use `strsplit' to break these out
movies <- strsplit(movies,",")

## and finally, match ids to names from actnet
casts <- lapply(movies, 
                function(m) V(actnet)$name[match(m,V(actnet)$id)])
## check it
casts['True Romance']
## format as arules transaction baskets
library(arules)
casttrans <- as(casts, "transactions")

## Set up STM information
castsize <- unlist(lapply(casts, function(m) length(m)))
## see ?rep.int: we're just repeating movie names for each cast member
acti <- factor(rep.int(names(casts),times=castsize))

## actors
actj <- factor(unlist(casts), levels=V(actnet)$name)

## format as STM (if you specify without `x', its binary 0/1)
actmat <- sparseMatrix(i=as.numeric(acti),j=as.numeric(actj),
                       dimnames=list(movie=levels(acti),actor=levels(actj)))

## count the number of appearences by actor
nroles <- colSums(actmat)
names(nroles) <- colnames(actmat)

### [1] The actors network has an edge if the two actors were in the same
### movie. Plot the entire actors network.
help(igraph)
summary(actnet)
#plot(actnet, edge.curved=FALSE, vertex.label=NA, vertex.size=3, vertex.color="blue")

### [2] Plot the neighborhoods for “Bacon, Kevin” at orders 1-3.
### How does the size of the network change with order?
nei1 <- graph.neighborhood(actnet, order=1,
                           V(actnet)["Bacon, Kevin"])[[1]]
E(nei1)$color="grey"
E(nei1)$lty=1
V(nei1)$label=V(nei1)$name
V(nei1)["Bacon, Kevin"]$color="gold"
#mylayout = layout.auto(nei1)
#plot(nei1, layout=mylayout)
neighborhood.size(actnet, order=1, V(actnet)["Bacon, Kevin"])
summary(nei1)

nei2 <- graph.neighborhood(actnet, order=2,
                           V(actnet)["Bacon, Kevin"])[[1]]
E(nei2)$color="grey"
E(nei2)$lty=1
V(nei2)$shape="circle"
V(nei2)$color="blue"
#mylayout = layout.auto(nei2)

#plot(nei2, vertex.label=NA, vertex.size=10, layout=mylayout)
E(nei2)$lty=0
V(nei2)$shape="none"
V(nei2)["Bacon, Kevin"]$color="gold"
V(nei2)["Bacon, Kevin"]$shape="circle"
#plot(nei2, vertex.label=NA, vertex.size=10, add=TRUE, layout=mylayout)

neighborhood.size(actnet, order=2, V(actnet)["Bacon, Kevin"])
summary(nei2)

nei3 <- graph.neighborhood(actnet, order=3,
                           V(actnet)["Bacon, Kevin"])[[1]]
E(nei3)$color="grey"
E(nei3)$lty=1
V(nei3)$shape="circle"
V(nei3)$color="blue"
#mylayout = layout.auto(nei3)

#plot(nei3, vertex.label=NA, vertex.size=7, layout=mylayout)
E(nei3)$lty=0
V(nei3)$shape="none"
V(nei3)["Bacon, Kevin"]$color="gold"
V(nei3)["Bacon, Kevin"]$shape="circle"
#plot(nei3, vertex.label=NA, vertex.size=7, add=TRUE, layout=mylayout)

neighborhood.size(actnet, order=3, V(actnet)["Bacon, Kevin"])
summary(nei3)

size = hash()
multiple = hash()

#randomly select some people to use  as our comparison
V(actnet)[sample(ncol(actmat))[1:3]]

names = c("Bacon, Kevin", "Neill, Sam", "Petelius, Pirkka-Pekka")
iter=7
for(name2 in names) {
  print(name2)
  size[name2] = c(0)
  multiple[name2] = c(0)
  for(i in 1:iter) {
    print(i)
    size[name2] = c(size[[name2]], neighborhood.size(actnet, order=i, V(actnet)[name2]))
    multiple[name2] = c(multiple[[name2]], neighborhood.size(actnet, order=(i+1), V(actnet)[name2])/neighborhood.size(actnet, order=i, V(actnet)[name2]))
  }
}
cols=rainbow(length(names))
size[[names[1]]]
plot(size[[names[1]]][-1])
lines(size[[names[1]]][-1])
length(names)
for(i in 2:length(names)){
  points(1:iter, size[[names[i]]][-1], col=cols[i])
  lines(1:iter, size[[names[i]]][-1], col=cols[i])
}

### [3] Who were the most common actors? Who were most connected?
### Pick a pair of actors and describe the shortest path between them.

sort(nroles)
tail(sort(degree(actnet)), n=10)
sort(betweenness(actnet))
#path <- get.shortest.paths(actnet, from="Bacon, Kevin", to=V(actnet)[sample(ncol(actmat))[1]]$name)
#path

path <- get.shortest.paths(actnet, from="Bacon, Kevin", to="Sulkanen, Aarno")
V(actnet)[path$vpath[[1]]]

#path <- get.shortest.paths(actnet, from="Bacon, Kevin", to="Nero, Franco")
V(actnet)$label = NA
V(actnet)$shape = "circle"
V(actnet)$color = "blue"
E(actnet)$lty <- 1
E(actnet)$width <- 2
E(actnet)$color <- "grey"
mylayout = layout.auto(actnet)
plot(actnet, edge.curved=FALSE, vertex.size=7, layout=mylayout)

V(actnet)[path$vpath[[1]]]$label=V(actnet)[path$vpath[[1]]]$name
E(actnet)$lty <- 0
E(actnet, path=path$vpath[[1]])$lty <- 1
E(actnet, path=path$vpath[[1]])$color <- "green"
plot(actnet, edge.curved=FALSE, add=TRUE, vertex.size=3, layout=mylayout)

### [4] Find pairwise actor-cast association rules with at least 0.01%
### support and 10% confidence. Describe what you find.

castrules <- apriori(casttrans,
                     parameter=list(support=.0001, confidence=.1, maxlen=2))

lhs(castrules)
summary(castrules)
inspect(castrules)

sparselhs = as(lhs(castrules), "ngCMatrix")
uniquelhs = unique(summary(sparselhs)$i)

sparserhs = as(rhs(castrules), "ngCMatrix")
uniquerhs = unique(summary(sparserhs)$i)


length(uniquelhs)
length(uniquerhs)
ncol(actmat)
length(castrules)
.2/191
nroles["Bacon, Kevin"]/(nrow(casttrans))
2/(nrow(casttrans))
1/(nrow(casttrans))
length(nroles[nroles < 2])
inspect(subset(castrules, subset=rhs%in%"Bacon, Kevin"))
inspect(subset(castrules, subset=rhs%in%"Corrêa e Castro, Cláudio"))

nroles["McClurg, Edie"]

##Create edgelist from pairs rules
pairs <- labels(castrules) 
pairs <- gsub("\\{|\\}","",pairs)##remove curly brackets and replace them with nothing in pairs
pairs <- strsplit(pairs," => ") ##split all pairs up at the arrow. Every pair is two items
pairs <- do.call(rbind,pairs) ##put together into big matrix
pairs[1:3,] ##see what this looks like
pairs <- pairs[pairs[,1]!="",] # no lhs (remove left hand side)

sort(summary(factor(sort(pairs))))
actnet2=graph.edgelist(pairs)
actnet2=as.undirected(actnet2)
V(actnet2)["Bacon, Kevin"]
V(actnet2)$color="cyan"
V(actnet2)$shape="circle"
V(actnet2)["Bacon, Kevin"]$color = "gold"
mylayout = layout.auto(actnet2)
plot(actnet2,vertex.label=NA,vertex.size=7,edge.curved=FALSE, layout=mylayout)

V(actnet2)$shape="none"
E(actnet2)$lty = 0
V(actnet2)["Bacon, Kevin"]$shape = "circle"
plot(actnet2,vertex.label=NA,vertex.size=7,edge.curved=FALSE, add=TRUE, layout=mylayout)

### [+] What would be a regression based alternative to ARules?
### Execute it for a single RHS actor.

name = "Bacon, Kevin"
y <- actmat[, grep(name,colnames(actmat))]
x <- (drop(actmat[, -grep(name,colnames(actmat))]))


fitlin <- gamlr(x, y, lmr=.01, verb=TRUE, standardize=FALSE, family="binomial")
summary(fitlin)

plot(fitlin, main=paste("Logistic regression on ", name, sep=""))
summary(fitlin)[which.min(AICc(fitlin)),]
Baic <- coef(fitlin)
Baic[Baic != 0]
length(Baic[Baic != 0])

Bcv1se <- coef(fitlin)[colnames(x),]
Bcv1se[Bcv1se !=0]

sort(Bcv1se[Bcv1se !=0])
names(Bcv1se[Bcv1se !=0])
nroles[names(sort(Bcv1se[Bcv1se !=0]))]
barplot(sort(Bcv1se[Bcv1se !=0]))

x2 <-barplot(sort(Bcv1se[Bcv1se !=0]), xaxt="n", space=1.5)
labs <- names(sort(Bcv1se[Bcv1se !=0]))
text(cex=.6, x=x2+2, y=-2.25, labs, xpd=TRUE, srt=45, pos=2)

?text

nroles[names(which.min(Bcv1se))]
nroles[names(which.max(Bcv1se))]
name_movies = rownames(actmat[actmat[,name],])
movies_in_common_max = name_movies[name_movies %in% rownames(actmat[actmat[,names(which.max(Bcv1se))],])]
movies_in_common_max

V(actnet)[path$vpath[[1]]]
name1="Bacon, Kevin" 
name2="McClurg, Edie"
name_movies = rownames(actmat[actmat[,name1],])
movies_in_common = name_movies[name_movies %in% rownames(actmat[actmat[,name2],])]
movies_in_common
