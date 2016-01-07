Project = "~/github"

path= "/projects/data/"
setwd(paste(Project, path, sep=""))


## nhl hockey analysis

## the data is in gamlr.  
## You need to first install this, 
## via install.packages("gamlr")
library(gamlr) # loads Matrix as well
library(MatrixModels)
help(hockey) # describes the hockey data and shows an example regression

data(hockey) # load the data

source("../RScripts/naref.R") ## Source Prof. Taddy's ref. factor functions

# Combine the covariates all together
x <- cBind(config,team,player) # cBind binds together two sparse matrices
# build 'y': home vs away, binary response
y <- goal$homegoal
nhlreg <- gamlr(x, y, lambda.min.ratio = 0.01,
                free=1:(ncol(config)+ncol(team)), ## free denotes unpenalized columns
                family="binomial", standardize=FALSE)
plot(nhlreg)
plot(AICc(nhlreg))
?gamlr
summary(nhlreg)
nhlreg$alpha[which.min(AICc(nhlreg))]

## coefficients (grab only the players)
# AICc selection 
Baicc <- coef(nhlreg)[colnames(player),]
hist(Baicc, breaks=seq(-40,30,l=500))
summary(Baicc)

length(Baicc[Baicc != 0])
BICseg <- which.min(AICc(nhlreg))
BICseg
nhlreg
scb.bic <- coef(nhlreg, s=BICseg)
summary(scb.bic)
summary(nhlreg)
summary(nhlreg)[BICseg,]

length(Baicc[Baicc != 0])

AICc(scb.bic)

logLik(nhlreg)

cv.nhlreg <- cv.gamlr(x, y, 
                free=1:(ncol(config)+ncol(team)), ## free denotes unpenalized columns
                family="binomial", standardize=FALSE, verb=TRUE)
plot(cv.nhlreg)
plot(cv.nhlreg$gamlr)
cv.nhlreg$seg.min
summary(cv.nhlreg)
length(colnames(player))
head(colnames(player))
coef(cv.nhlreg)
coef(cv.nhlreg, select="min")
log(cv.nhlreg$lambda.min)
log(cv.nhlreg$lambda.1se)
log(nhlreg$lambda.min)
log(nhlreg$lambda.1se)
log(1.202420e-04)
summary(cv.nhlreg)
cv.nhlreg$seg.min

plot(cv.)

x2 <- cBind(player)

nhlreg3 <- gamlr(x2, y,
                 family="binomial", standardize=FALSE, lambda.min.ratio=0.000001)
summary(nhlreg3)

plot(nhlreg3)
AICc(nhlreg3)

set_1 = colnames(player)
set_2 = names(coef(nhlreg3)[,1][coef(nhlreg3)[,1] != 0])
length(set_1)
length(set_2)
(set_1[set_1 %in% set_2])
setdiff(set_2, set_1)
summary(y)
typeof(y)
cv.nhlreg3 <- cv.gamlr(x2, y,
                 family="binomial", standardize=FALSE, lambda.min.ratio=0.000001)
head(y)
plot(cv.nhlreg3)
plot(cv.nhlreg3$gamlr)
cv.nhlreg3$seg.min
summary(cv.nhlreg3)


ll <- log(nhlreg$lambda) ## the sequence of lambdas
n = length(colnames(player))
par(mfrow=c(1,1))
plot(cv.nhlreg)
plot(ll, AIC(nhlreg)/n, 
     xlab="log lambda", ylab="IC/n", pch=21, bg="orange")
abline(v=ll[which.min(AIC(nhlreg))], col="orange", lty=3)
abline(v=ll[which.min(BIC(nhlreg))], col="green", lty=3)
abline(v=ll[which.min(AICc(nhlreg))], col="black", lty=3)
points(ll, BIC(nhlreg)/n, pch=21, bg="green")
points(ll, AICc(nhlreg)/n, pch=21, bg="black")
legend("topleft", bty="n",
       fill=c("black","orange","green"),legend=c("AICc","AIC","BIC"))

## all metrics, together in a path plot.
plot(nhlreg, col="grey")
abline(v=ll[which.min(AICc(nhlreg))], col="black", lty=2)
abline(v=ll[which.min(AIC(nhlreg))], col="orange", lty=2)
abline(v=ll[which.min(BIC(nhlreg))], col="green", lty=2)
abline(v=log(cv.nhlreg$lambda.min), col="blue", lty=2)
abline(v=log(cv.nhlreg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, 
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV.min","CV.1se"))

