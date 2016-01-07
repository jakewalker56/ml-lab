####################  Association Rules #####################

### *** LastFM play counts *** ###
lastfm <- read.csv("lastfm.csv")
lastfm$user <- factor(lastfm$user)

## Use the a-rules package for association rules
library(arules)
## there is an entire ecosystem of packages around arules
## you need to first create a list of baskets: vectors of items by consumer

## Here's how we do the formatting here:
## split data into a list of artists for each user
playlists <- split(x=lastfm$artist, f=lastfm$user)
## re-move artist repretition in these lists
playlists <- lapply(playlists, unique)
## tell R to treat this as a special arules `transactions' class.
playtrans <- as(playlists, "transactions")

## now apply the actual 'apriori' algorithm
# you can add a list of arguments called 'parameter'.  Here, we look at
# only rules with support > .01 & confidence >.5 & length (# artists) <= 3
musicrules <- apriori(playtrans, 
	parameter=list(support=.01, confidence=.5, maxlen=3))
                         
## take a look
inspect(musicrules)
## Choose any subset you want. 
inspect(subset(musicrules, subset=lift > 5))
inspect(subset(musicrules, subset=confidence > 0.6))
inspect(subset(musicrules, subset=support > .02 & confidence > 0.6))
inspect(subset(musicrules, subset=lhs%in%"t.i."))

# grab a larger list of pairwise artist connections
artrules <- apriori(playtrans, 
	parameter=list(support=.001, confidence=.1, maxlen=2))
## extract the rules as strings, then change into an edge matrix
## print pairs at each step to see what I've done
pairs <- labels(artrules)
pairs <- gsub("\\{|\\}","",pairs)
pairs <- strsplit(pairs," => ")
pairs <- do.call(rbind,pairs)
pairs <- pairs[pairs[,1]!="",] # no lhs

library(igraph)
musicnet <- graph.edgelist(pairs)
musicnet <- as.undirected(musicnet)
## full plot takes time a bit of time
V(musicnet)$color <- "cyan"
# the plot tries to force distances proportional to connectivity
# imagine nodes connected by elastic bands that you are pulling apart
# you can either add color/size/etc attributes to the graph itself, or just when you plot
# edge.curved puts straight lines between vertices; it looks good and is faster
plot(musicnet, vertex.label=NA, vertex.size=3, edge.curved=FALSE)

## look at some measures of connectivity
mbetween <- betweenness(musicnet)
mdegree <- degree(musicnet)
which.max(mbetween)

## and raw popularity
playcount <- table(lastfm$artist)[names(mdegree)]

## they're all closely related
plot(mdegree,mbetween,log="xy")
plot(playcount,mbetween,log="xy")
## some outlier stand out
plot(playcount,mbetween,log="xy",ylab="betweenness",col="grey50",bty="n")
points(playcount["pidżama porno"],mbetween["pidżama porno"],col=2)
text(playcount["pidżama porno"],
	mbetween["pidżama porno"]*2,labels="pidżama porno",col=2) 
points(playcount["[unknown]"],mbetween["[unknown]"],col=4)
text(playcount["[unknown]"],
	mbetween["[unknown]"]*1/2,labels="[unknown]",col=4) 


## grab the neighborhood of some bands
band <- "rush" 
nei <- graph.neighborhood(musicnet, 1, V(musicnet)[band])[[1]]
V(nei)[band]$color <- "gold"
V(nei)$label.color = "black"
V(nei)$frame.color = NA
plot(nei, edge.curved=FALSE)
