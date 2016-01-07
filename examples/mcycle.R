
### *** Motorcycle Data *** ###
library(tree)
library(randomForest)
library(MASS) # data

data(mcycle)

par(mfrow=c(1,2))
mct <- tree(accel ~ times, data=mcycle)
plot(mct, col=8)
text(mct, cex=.75, font=2)

## create a grid of times to predict over
timegrid <- data.frame(times=seq(1,60,length=1000))

## plot my fitted tree surface
plot(mcycle, pch=21, bg=8)
lines(timegrid$times, predict(mct, newdata=timegrid), col=2, lwd=3)

## now do it with random forests
rfmc <- randomForest(accel ~ times, data=mcycle)
pred.rfmc <- predict(rfmc, timegrid)
plot(mcycle, pch=21, bg=8)
lines(timegrid$times, pred.rfmc, col=2, lwd=2)
## plot doesn't produce a nice dendrogram... why?
plot(rfmc) # this is just the MSE as you increase your sample of trees
