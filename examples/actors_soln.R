## actors network example

source("actors_start.R")

## [1] Just plot the whole thing
V(actnet)$color <- "dodgerblue"
V(actnet)$label.color = "black"
V(actnet)$frame.color = NA
# takes some time...
plot(actnet, vertex.label=NA, vertex.size=3, edge.curved=FALSE)

## [2] Neighborhoods
nei1<- graph.neighborhood(actnet, 1, V(actnet)["Bacon, Kevin"])[[1]]
nei2<- graph.neighborhood(actnet, 2, V(actnet)["Bacon, Kevin"])[[1]]
nei3<- graph.neighborhood(actnet, 3, V(actnet)["Bacon, Kevin"])[[1]]

# they grow quickly
plot(nei1, edge.curved=FALSE)
plot(nei2, edge.curved=FALSE,vertex.size=3,vertex.label=NA)
plot(nei3, edge.curved=FALSE,vertex.size=3,vertex.label=NA)

# and by 3 we're up to nearly the whole graph
length(V(nei1))
length(V(nei2))
length(V(nei3))
length(V(actnet))

## [3] connectivity
## degree: it looks like those with high degree are from TV... perhaps due to larger cast sizes, or many 'guest stars'?
md <- degree(actnet)
names(md)[order(-md)[1:10]]
## betweenness: Sam Niell has been in hollywood blockbusters (hunt for red october) along with many smaller film and TV roles with british productions.  I guess he's a link between those two worlds.  Same for Ben Kingsly.  You might have build stories for the others.
mb <- betweenness(actnet)
names(mb)[order(-mb)[1:10]]
# number of roles: those with many roles are different from the most connected (there is some overlap: Ron Jeremy, crossover star I guess).  Zivojinovic and others were in many small european productions, but it looks like they didn't do much outside of their respective communities.
names(nroles)[order(-nroles)[1:10]]

## shortest path examples...
V(actnet)$name[get.shortest.paths(actnet, 
	from="Arquette, Patricia", to="Arquette, Rosanna")$vpath[[1]]] # via true romance
V(actnet)$name[get.shortest.paths(actnet, 
	from="Bacon, Kevin", to="Pfeiffer, Michelle")$vpath[[1]]] 
	
## [4] Association Rules

## create a set of movie rules and inspect them
movierules <- apriori(casttrans,parameter=list(maxlen=2,
								support=0.0001,confidence=0.1))

## High support rules: looks like television casts if you google
inspect(subset(movierules, subset=lift > 100 & support > 0.001))

## whatever story you tell is great.
## I'm looking at the shared cast of two Kevin Bacon movies...
inspect(subset(movierules, subset=lhs%in%"Bacon, Kevin" & confidence>.1))
# I know it is two, because support(lhs AND rhs) = 0.0001396 and
2/nrow(actmat)
# what is p(kevin bacon)?
inspect(subset(movierules, subset=rhs%in%"Bacon, Kevin" & confidence>.1))
0.2/191.0133 # confidence over lift
# how many movies?
nrow(actmat)*0.2/191.0133
sum(actmat[,"Bacon, Kevin"]) # it matches!

## BONUS: a regression version of association rules

## design for specific actors
actor <- "Sheen, Martin"
y <- actmat[,actor]
xact <- actmat[,colnames(actmat)!=actor]

library(gamlr)
fitact <- gamlr(xact,y,family="binomial")
B <- coef(fitact)[-1,]
B[order(-B)[1:5]] 

## compare to the association rules: first is the same, the rest are not.
## this is because once you know about the Robert Carradine connection,
## the other top association rules no longer matter for logistic regression.
## (since RC is also highly associated with those other actors, like Kelly, Moira)
inspect(subset(movierules, subset=rhs%in%"Sheen, Martin" & confidence>.1))

## or, standardize=FALSE
fitact <- gamlr(xact,y,family="binomial", standardize=FALSE)
B <- coef(fitact)[-1,]
B[order(-B)[1:5]] 


