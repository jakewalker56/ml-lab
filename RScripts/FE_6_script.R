Dropbox = "C:\\users/Jake/Dropbox"
#Mac/Unix
Dropbox = "~/Dropbox"

path= "/Booth/Winter 2014/Financial Econometrics/Week 7/"
setwd(paste(Dropbox, path, sep=""))
spdaily = read.csv("sp500daily.csv")
library(fGarch)
library(nortest)
library(tseries)

summary(spdaily)

rseries = log(spdaily$Adj.Close) - log(spdaily$AR1)
summary(rseries)

gfit = garchFit(formula = ~aparch(1, 1), delta=1, include.delta=FALSE, include.mean=FALSE, data = rseries)
gfit
summary(gfit)
?garchFit
summary(gfit@residuals)
hist(gfit@residuals/gfit@sigma.t)
ad.test(gfit@residuals/gfit@sigma.t)
qqnorm(gfit@residuals/gfit@sigma.t)
abline(0,1)
kurtosis(gfit@residuals/gfit@sigma.t)
skewness(gfit@residuals/gfit@sigma.t)

#6
#a
uw = mean return of portfolio
sigmaw^2 = variance of portfolio
rf= risk free rate
sigmasp^2 = variance of sp500
w = portfolio weight of sp500

uw = w * usp + (1-w)* rf
sigmaw^2 = var(uw) = var(w * usp + (1-w)* rf) = var(w*usp) = w^2 * sigmasp^2

#b
U = uw - 1 * lambda * sigmaw^2 / 2 = w * usp + (1-w) * rf -1 * lambda * (w^2 * sigmasp^2) / 2
dU/dw = usp - rf - lambda * w * sigmasp^2
w = (usp - rf) / (lambda * sigmasp^2)

#c
rf = 0.02/252
usp = mean(rseries)
sigmasp = stdev(rseries)
lambda = 5
w = (usp - rf) / (lambda * sigmasp^2)
w

#d
Uunc = w * usp + (1-w) * rf -1 * lambda * (w^2 * sigmasp^2) / 2
Uunc

#7
n = 2
?garch
gmodel = garch(rseries, order = c(c(2,1),1))
summary(gmodel)

#gmodel$residuals
wt = (usp - rf) / (lambda * gmodel$fitted.values[,1]^2)
plot(wt, type="l", col="blue")
maxg =  max(gmodel$fitted.values[(n+1):length(gmodel$fitted.values[,1]),1])
lines(gmodel$fitted.values[,1] * 3.5/ maxg, col="red")

rwt = wt[(n+1):length(wt)] * usp + (1-wt[(n+1):length(wt)]) * rf
rbar = mean(rwt)
rbar

Uopt = rbar -1 * lambda * mean((rwt - rbar)^2) / 2
Uopt

Uunc
w * usp + (1-w) * rf