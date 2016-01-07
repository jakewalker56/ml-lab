
###### *** Gas Octane Data *** ######

gas <- read.csv("gasoline.csv")
octane <- gas[,1]
nir <- as.matrix(gas[,-1])
plot(nir[1,], type='l', ylab='nir')
for(i in 2:60) 
	lines(nir[i,], col=rainbow(60)[i])

### marginal regression
phi <- cor(nir, octane)/apply(nir,2,sd) 
z <- nir%*%phi
fwd <- glm(octane ~ z)
plot(fwd$fit, octane, pch=21, bg="lightgreen", 
	xlab="marginal regression fit")

### Partial Least Squares
## PLS 4 
library(textir)
summary( gaspls <- pls(x=nir, y=octane,  K=4) )
plot(gaspls, pch=21, bg=8)


### fit glmnet lasso for comparison
gasgl <- cv.gamlr(x=nir, y=octane)
plot(gasgl) # it chooses a dense model (many parameters)
## this is the setting were PLS/MR can thrive.

### PC Regression for reference
gaspca <- prcomp(nir, scale=TRUE)
gaspc <- predict(gaspca)
summary(glm(gas$octane ~ gaspc[,1:10]))
## some plausible models
BIC(glm(octane ~ gaspc[,1:5]))
BIC(glm(octane ~ gaspc[,1:6]))
BIC(glm(octane ~ gaspc[,1:7]))
BIC(glm(octane ~ gaspc[,1:8]))
## use 6 PC's
summary(glm(octane ~ gaspc[,1:6]), k=log(nrow(gas)))

