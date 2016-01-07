Project = "~/github"

path= "/projects/data/"
setwd(paste(Project, path, sep=""))

## microfinance network 
## data from BANERJEE, CHANDRASEKHAR, DUFLO, JACKSON 2012

## data on 8622 households
hh <- read.csv("microfi_households.csv", row.names="hh")

source("../RScripts/naref.R")
library(igraph)
library(gamlr)

## We'll kick off with a bunch of network stuff.
## This will be covered in more detail in lecture 6.
## get igraph off of CRAN if you don't have it
## install.packages("igraph")
## this is a tool for network analysis
## (see http://igraph.sourceforge.net/)

edges <- read.table("microfi_edges.txt", colClasses="character")
## edges holds connections between the household ids
summary(edges)
hhnet <- graph.edgelist(as.matrix(edges))
hhnet <- as.undirected(hhnet) # two-way connections.
summary(hhnet)
head(edges)

## igraph is all about plotting.  
V(hhnet) ## our 8000+ household vertices
## Each vertex (node) has some attributes, and we can add more.
V(hhnet)$village <- as.character(hh[V(hhnet),'village'])
## we'll color them by village membership
vilcol <- rainbow(nlevels(hh$village))
names(vilcol) <- levels(hh$village)
V(hhnet)$color = vilcol[V(hhnet)$village]
## drop HH labels from plot
V(hhnet)$label=NA
V(hhnet)$name

# graph plots try to force distances proportional to connectivity
# imagine nodes connected by elastic bands that you are pulling apart
# The graphs can take a very long time, but I've found
# edge.curved=FALSE speeds things up a lot.  Not sure why.

## we'll use induced.subgraph and plot a couple villages 
village1 <- induced.subgraph(hhnet, v=which(V(hhnet)$village=="1"))
village33 <- induced.subgraph(hhnet, v=which(V(hhnet)$village=="33"))
villagesizes = c()
for(i in levels(factor(V(hhnet)$village))) {
  villagesizes <- c(villagesizes, length(which(V(hhnet)$village==i)))
}
summary(villagesizes)
plot(villagesizes)

# vertex.size=3 is small.  default is 15
plot(village1, vertex.size=3, edge.curved=FALSE)
plot(village33, vertex.size=3, edge.curved=FALSE)

######  now, on to your homework stuff

## match id's; I call these 'zebras' because they are like crosswalks
zebra <- match(rownames(hh), V(hhnet)$name)
head(zebra)
head(V(hhnet)$name)

## calculate the `degree' of each hh: 
##  number of commerce/friend/family connections
degree <- degree(hhnet)[zebra]
names(degree) <- rownames(hh)
degree[is.na(degree)] <- 0 # unconnected houses, not in our graph

## if you run a full glm, it takes forever and is an overfit mess
# > summary(full <- glm(loan ~ degree + .^2, data=hh, family="binomial"))
# Warning messages:
# 1: glm.fit: algorithm did not converge 
# 2: glm.fit: fitted probabilities numerically 0 or 1 occurred 

#[1] I’d transform degree to create our treatment variable d. 
# What would you do and why? 

# Take the log(degree), because of the distribution.  There's no reason 
# to think degree is linear- the marginal difference between 30 
# connections and 31 connections is probably not the same as the 
# marginal difference between 1 connection and 2 connections 
hist(degree)
d <- log(degree + 1)
hist(d)

#what about degree/village size?  Should the effect of degree change 
#depending on what percentage of your village you know?

#[2]. Build a model to predict d from x, our controls. Comment on how 
# tight the fit is, and what that implies for estimation of a 
# treatment effect. 

# In order to standardize beds and rooms, we need to factor everything else. 
# Can we partially standardize?
# If we factor everything, it gets turned into 1/0 values in the model.matrix
# In general, how do we deal with standardization with factored variables?

#factor stuff so we can 
y = hh$loan
hh$loan <- NULL
#remove village column for refactoring
v <- factor(hh$village)
hh$village <- NULL
## refactor village to have NA reference level
v <- naref(v)

hh$electricity <- factor(hh$electricity)
hh$leader <- factor(hh$leader)
hh <- naref(hh)

x = sparse.model.matrix(~ (v + .)*., data=hh)[,-1]
dim(x)

q2_d_reg <- gamlr(x, d, lambda.min.ratio = 0.001,
                           family="gaussian", standardize=TRUE)
summary(q2_d_reg)
BICseg <- which.min(AICc(q2_d_reg))
BICseg
q2_d_reg
scb.bic <- coef(q2_d_reg, s=BICseg)
summary(scb.bic)
summary(q2_d_reg)
summary(q2_d_reg)[BICseg,]
Baicc <- coef(q2_d_reg)
length(Baicc[Baicc != 0])
Baicc[Baicc != 0]
scb.bic
logLik(q2_d_reg)
plot(q2_d_reg)
summary(q2_d_reg)[BICseg,]

q2_d_reg$lambda[which.min(AICc(q2_d_reg))]

summary(q2_d_reg)

dhat <- predict(q2_d_reg, x, type="response") 
cor(drop(dhat),d)^2
AICseg <- which.min(AICc(q2_d_reg))
summary(q2_d_reg)[AICseg,]
plot(d)
plot(d)
plot(drop(dhat)-d)
plot(drop(dhat))
sum((dhat - mean(dhat))*(d - mean(d)))
?cor
cor(drop(dhat),d, use="everything")^2
cor(drop(dhat),d, use="all.obs")^2
cor(drop(dhat),d, use="complete.obs")^2
cor(drop(dhat),d, use="na.or.complete")^2
cor(drop(dhat),d, use="pairwise.complete.obs")^2
cor(drop(dhat),d, method="kendall")^2
cor(drop(dhat),d, method="spearman")^2
coef(q2_d_reg, s=1)
gamlr:::gamlr

q2_d_reg$dev[40]
q2_d_reg$dev[1]

1-q2_d_reg$dev[40]/q2_d_reg$dev[1]
1-sum( (d-dhat)^2 )/sum( (d-mean(d))^2 )

SST = sum((d-mean(d))^2)
SSE = sum((d-dhat)^2)
SSE
(sum((d-mean(d))*(dhat-mean(dhat)))/(sd(dhat)*sd(d)*(length(dhat)-1)))^2

?cov
head(dhat)
d
cov(drop(dhat), d-drop(dhat))

r = cor(drop(dhat),d)^2
colnames(coef(q2_d_reg) [coef(q2_d_reg) != 0])
p = length(coef(q2_d_reg) [coef(q2_d_reg) != 0])
n = length(y)
n  
ar2 = r - (1-r)*(p)/(n-p-1)
ar2

#[3]. Use predictions from [2] in an estimator for effect of d on loan. 
plot(dhat)
plot(dhat,d,bty="n",pch=21,bg=8) 
causal <- gamlr(cBind(d,dhat,x),y, lambda.min.ratio = 0.001, free=2,family="binomial")
coef(causal)["d",] # AICc says degree has a causal effect.
coef(causal)

plot(causal)

# what is yhat here?  Is it the probability?  Or is it the logit 
# value we have to play games with to get the prob?

yhat <- predict(causal, cBind(d,dhat,x), type="response")
cor(drop(yhat),y)^2

plot(yhat,y,bty="n",pch=21,bg=8) 

# [4]. Compare the results from [3] to those from a straight 
# (naive) lasso for loan on d and x. Explain why they are 
# similar or different. 
causal_naive <- gamlr(cBind(d,x),y,lambda.min.ratio = 0.001, family="binomial")
coef(causal_naive)["d",] # AICc says degree has a causal effect.
coef(causal_naive)


#generally pretty simillar, though the naive estimate of d 
#coefficient is actually lower than the controlled scenario.

#....that's actually super weird, right? (NOTE: Seems like we 
#just got unlucky- rerunning cv gives a much higher value of .07
#instead of .03.  I've upped the nfolds, but still can't get
#consistent results.  We should probably bootstrap!

#anyway, they are simillar because our r2 on predicting d is
#quite low.  There aren't a lot of things covarying with d
#in our data set.

#[5]. Bootstrap your estimator from [3] and describe the 
#uncertainty. 

n <- nrow(x)

## Bootstrapping our lasso causal estimator is easy
gamb <- c() # empty gamma
for(b in 1:20){
  ## create a matrix of resampled indices
  ib <- sample(1:n, n, replace=TRUE)
  ## create the resampled data
  xb <- x[ib,]
  db <- d[ib]
  yb <- y[ib]
  ## run the treatment regression
  treatb <- gamlr(xb,db,lambda.min.ratio=.001)
  dhatb <- predict(treatb, xb, type="response")
  
  fitb <- gamlr(cBind(db,dhatb,xb),yb,free=2,lambda.min.ratio=.001,family="binomial")
  gamb <- c(gamb,coef(fitb)["db",])
  print(b)
}
summary(gamb) 

#[+]. Can you think of how you’d design an experiment to 
# estimate the treatment effect of network degree?

# You'd need to make the degree independent of all the other
# variables.  
# -assign degree: randomly select people to put into social 
#   situations to increase their degree, then look at data in the
#   future.  This seems like a bad idea, since artificial degrees
#   may be distinct from organic degrees in causal effect.  You'd 
#   also have to worry about who you were putting them in touch 
#   with- rich connections may be more useful than poor 
#   connections, e.g.
# -You could force people to move to a new village in which they
#   have no connections.  If you just looked at who moved and who
#   didn't, then you no longer have a random sample....
# -An experiment ultimately means you have to ASSIGN a value
#   to a participant.  I can't think of any other value you could 
#   assign in this case that makes any sense.'
