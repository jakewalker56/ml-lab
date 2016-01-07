### *** data on tv shows from NBC *** ###

shows <- read.csv("nbc_showdetails.csv", row.names=1) ## show details; ratings and engagement

## take a look at the types of shows
plot(GRP ~ PE, data=shows, bg=c(4,2,3)[shows$Genre], pch=21, log="y")
legend("bottomright", legend=levels(shows$Genre), fill=c(4,2,3), bty="n")

## Now read the pilot focus group survey results
## for each question, 1=strongly disagree, 5=strongly agree.
## 1: 'The show makes me feel ____', 2: 'I found the show ____'
survey <- read.csv("nbc_pilotsurvey.csv", as.is=TRUE) 
survey$Show <- factor(survey$Show, levels=rownames(shows))

### Now look at PCA of the survey responses.  
## This is a common way to treat survey data
pcanbc <- prcomp(survey[,-(1:2)], scale=TRUE)
znbc <- predict(pcanbc)  
## again, just same thing as the below:
z <- scale(survey[,-(1:2)])%*%pcanbc$rotation
all(z==znbc)

## plot and interpret
plot(pcanbc, main="")
mtext(side=1, "Pilot-Survey PCs",  line=1, font=2)
round(pcanbc$rotation[,1:2],1) 

### Principal components regression

## First, aggregate average PCs by show
zpilot <- aggregate(znbc,by=list(Show=survey$Show),mean)
rownames(zpilot) <- zpilot$Show
zpilot <- zpilot[,-1]

## look at a plot of them
plot(zpilot[,1:2], col=0, # col=0 to get an empy plot
	ylim=c(-.6,.65), xlim=c(-2,2), # hides "monarch cove",living with ed" but these are all tiny 
	main="shows sized by PE") 
text(zpilot[,1:2], labels=rownames(zpilot), 
	col=c("navy","red","green")[shows$Genre], # color by genre
	cex=shows$PE/mean(shows$PE)) # size by show

## First, aggregate average survey results by show
Xpilot <- aggregate(survey[,-(1:2)],  ## -(1:2) to remove the 'show' and 'viewer'
				by=list(Show=survey$Show), mean)
## aggregate adds the 'by' variable levels back in; 
## we'll strip it (show names) and use them as rownames
rownames(Xpilot) <- Xpilot[,1]
Xpilot <- Xpilot[,-1]
all(rownames(Xpilot)==rownames(shows)) ## sanity check

## It looks like there is indeed signal here
plot(shows$PE, Xpilot[,"Q1_Engaged"])

PCApilot <- prcomp(Xpilot, scale=TRUE)
## the picture is less clear now.
## first is clearly in 'bad' direction,  
## but second loads high on both boring and comforted...
round(PCApilot$rotation[,1:2],1) 

## calculate pc directions
zpilot <- predict(PCApilot)

## do regression onto the first two
PE <- shows$PE
## convert to a data frame so glm can keep track of names
zdf <- as.data.frame(zpilot)
summary(PEglm <- glm(PE ~ ., data=zdf[,1:2]))

library(gamlr) # to get AICc, plus we'll do lasso below

## Get glm fits on 1:20 factors
kfits <- lapply(1:20, # do the below for K=1:20
	function(K) glm(PE~., data=zdf[,1:K,drop=FALSE]))
aicc <- sapply(kfits, AICc) # apply AICc to each fit
which.min(aicc) ## it likes 2 factors best
## you could also use BIC
bic <- sapply(kfits, BIC) 
which.min(bic) ## also likes 2


## now the lasso
lassoPCR <- cv.gamlr(x=zpilot, y=PE)
## lasso.1se agrees with IC on first 2, then grabs a couple extra
coef(lassoPCR, s="min") 

## plot 'em (class notes show BIC)
par(mfrow=c(1,2))
plot(aicc, pch=21, bg="maroon", xlab="K", ylab="AICc")
plot(lassoPCR) 

## compare to an un-factorized lasso
## nfolds=40 for leave-one-out!
lasso <- cv.gamlr(x=as.matrix(Xpilot), y=PE, nfolds=40)
plot(lasso) ## it chooses nothing!
## since you haven't simplified into linear factors 
## the estimation variance overwhelms any signal


#### WEEK 9 TREES STUFF

## new packages
library(tree)
library(randomForest)
library(gamlr)

## read in the NBC show characteristics
nbc <- read.csv("nbc_showdetails.csv")

## lets look at the show demographics for predicting genre
demos <- read.csv("nbc_demographics.csv")
genre <- nbc$Genre
## tree fit; it knows to fit a classification tree since genre is a factor.
## for two-level factors (e.g. spam) make sure you do factor(spam)
genretree <- tree(genre ~ ., data=demos[,-1], mincut=1)
## tree plot
plot(genretree, col=8, lwd=2)
## print the predictive probabilities
text(genretree)

## example of prediction (type="class"  to get max prob classifications back)
genrepred <- predict(genretree, newdata=demos[,-1], type="class")

## example of random forest for classification
genrerf <- randomForest(genre ~ ., data=demos[,-1], importance=TRUE)
varImpPlot(genrerf,type=1)
## random forest also just gives you the max prob class.
genrerfclass <- predict(genrerf, newdata=demos[,-1])

## create a separate binary column for each Genre,
## make it a data frame, and re-name them for convenience 
## as always, you should do this explicitly to avoid bugs
x <- as.data.frame(model.matrix(PE ~ Genre + GRP, data=nbc)[,-1])
names(x) <- c("reality","comedy","GRP")
PE <- nbc$PE

## here's the regression tree.
nbctree <- tree(PE ~ ., data=x, mincut=1)
## now plot it
par(mfrow=c(1,2))
plot(nbctree, col=8)
text(nbctree, cex=.75, font=2)
## add a look at fit using the predict function
par(mai=c(.8,.8,.2,.2))
plot(PE ~ GRP, data=nbc, col=c(4,2,3)[nbc$Genre], pch=20, ylim=c(45,90))
newgrp <- seq(1,3000,length=1000)
lines(newgrp, predict(nbctree, newdata=data.frame(GRP=newgrp, drama=1, comedy=0, reality=0)), col=4)
lines(newgrp, predict(nbctree, newdata=data.frame(GRP=newgrp, drama=0, comedy=1, reality=0)), col=3)
lines(newgrp, predict(nbctree, newdata=data.frame(GRP=newgrp, drama=0, comedy=0, reality=1)), col=2)

