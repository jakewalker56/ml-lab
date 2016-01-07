
###### *** California Housing Data *** ######
library(tree)
library(gamlr)
library(randomForest)

## median home values in various census tracts
## lat/long are centroids of the tract
## response value is log(medianhomeval)
ca <- read.csv("CAhousing.csv")
ca$AveBedrms <- ca$totalBedrooms/ca$households
ca$AveRooms <- ca$totalRooms/ca$households
ca$AveOccupancy <- ca$population/ca$households
logMedVal <- log(ca$medianHouseValue)
ca <- ca[,-c(4,5,9)] # lose lmedval and the room totals

## create a full matrix of interactions (only necessary for linear model)
## do the normalization only for main variables.
XXca <- model.matrix(~.*longitude*latitude, data=data.frame(scale(ca)))[,-1]

## what would a lasso linear model fit look like?
## it likes a pretty complicated model
par(mfrow=c(1,2))
plot(capen <- cv.gamlr(x=XXca, y=logMedVal))
plot(capen$gamlr)
round(coef(capen),2)

#### Trees

## First, lets do it with CART
## no need for interactions; the tree finds them automatically
catree <- tree(logMedVal ~ ., data=ca) 
plot(catree, col=8, lwd=2)
text(catree)
## looks like the most complicated tree is best! 
cvca <- cv.tree(catree)
cvca$size[which.min(cvca$dev)]
plot(cvca)

## Next, with random forest (takes some time to run)
## limit the number of trees and the minimum tree size for speed
## add importance=TRUE so that we store the variable importance information
carf <- randomForest(logMedVal ~ ., data=ca, ntree=250, nodesize=25, importance=TRUE)
## variable importance plot. Add type=1 to plot % contribution to MSE
varImpPlot(carf,  type=1, pch=21, bg="navy", main='RF variable importance')

##### prediction and plotting fit

## the spatial mapping is totally extra; no need to replicate
par(mfrow=c(1,2))
plot(calasso <- cv.gamlr(x=XXca, y=logMedVal))
plot(calasso$gamlr)
round(coef(calasso),2)

## predicted values
yhatlasso <- predict(calasso, XXca)
yhattree <- predict(catree, ca)
yhatrf <- predict(carf, ca) ## takes time!  drawback of RFs

## plot the predictions by location
## the plotting is a bit complex here
## no need to figure it out if you don't want

## set up some color maps for fit and residuals
predcol = heat.colors(9)[9:1] ## see help(heat.colors)
residcol = c('red','orange',0,'turquoise','blue')
predbreaks = c(9,10,10.5,11,11.5,12,12.5,13,13.5,14.5) # borders of pred color bins
residbreaks = c(-3,-2,-1,1,2,3) # borders of resid color bins
## simple utility functions
predmap <- function(y){
  return(predcol[cut(drop(y),predbreaks)]) ## cut sorts into bins
}
residmap <- function(e){
  return(residcol[cut(drop(e), residbreaks)]) ## cut sorts into bins
}

### plot fit and resids

## again: this image plotting is all extra!  
## using the maps package to add california outline
library(maps)
par(mfrow=c(2,3))
## preds
map('state', 'california') 
points(ca[,1:2], col=predmap(yhatlasso), pch=20, cex=.5)
mtext("lasso fitted")
legend("topright", title="prediction", bty="n",
       fill=predcol[c(1,4,7,9)], legend=c("20k","100k","400k","1mil"))
map('state', 'california') 
points(ca[,1:2], col=predmap(yhattree), pch=20, cex=.5)
mtext("cart fitted")
map('state', 'california') 
points(ca[,1:2], col=predmap(yhatrf), pch=20, cex=.5)
mtext("rf fitted")
## resids
map('state', 'california') 
points(ca[,1:2], col=residmap(logMedVal - yhatlasso), cex=1.5)
mtext("lasso resid")
legend("topright", title="residuals", bty="n", fill=residcol[-3], legend=c(-2,-1, 1,2))
map('state', 'california') 
points(ca[,1:2], col=residmap(logMedVal - yhattree), cex=1.5)
mtext("cart resid")
map('state', 'california') 
points(ca[,1:2], col=residmap(logMedVal - yhatrf), cex=1.5)
mtext("rf resid")


## Out of sample prediction (takes a while since RF is slow)
MSE <- list(LASSO=NULL, CART=NULL, RF=NULL)
for(i in 1:10){
  train <- sample(1:nrow(ca), 5000)
  
  lin <- cv.gamlr(x=XXca[train,], y=logMedVal[train])
  yhat.lin <- drop(predict(lin, XXca[-train,]))
  MSE$LASSO <- c( MSE$LASSO, var(logMedVal[-train] - yhat.lin))

  rt <- tree(logMedVal[train] ~ ., data=ca[train,])
  yhat.rt <- predict(rt, newdata=ca[-train,])
  MSE$CART <- c( MSE$CART, var(logMedVal[-train] - yhat.rt))

  rf <- randomForest(logMedVal[train] ~ ., data=ca[train,], ntree=250, nodesize=25)
  yhat.rf <- predict(rf, newdata=ca[-train,])
  MSE$RF <- c( MSE$RF, var(logMedVal[-train] - yhat.rf) )
 
  cat(i)
} 
boxplot(MSE, col="dodgerblue", xlab="model", ylab="MSE")

