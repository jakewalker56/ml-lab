#Windows
Dropbox = "C:\\users/Jake/Dropbox"
#Mac/Unix
Dropbox = "~/Dropbox"

path= "/Booth/Winter 2014/Financial Econometrics/Week 9/"
setwd(paste(Dropbox, path, sep=""))
trade = read.csv("tradedata.csv")
bid = read.csv("bidask.csv")

library(fGarch)
library(nortest)
library(tseries)
library(vars)
library(apt)

#1
summary(trade)
varmodel = VAR(trade, p=1, type="const")
varmodel
predict(varmodel, n.ahead=3, ci=0.95)

summary(varmodel$datamat)
length(varmodel$varresult)
summary(varmodel$varresult[1]$dmidprice)
summary(varmodel$varresult[2]$dur)
summary(varmodel$varresult[3]$x)

dmidprice.l1=trade$dmidprice[length(trade$dmidprice)]
dur.l1=trade$dur[length(trade$dur)]
x.l1=trade$x[length(trade$x)]
pred <-data.frame(dmidprice=numeric(0), dur=numeric(0), x=numeric(0))
for(i in 1:3)
{
  #forecast ahead: multiply the last values by the coefficient matrix, add the constant
  dmidprice = varmodel$varresult[1]$dmidprice$coef[1] * dmidprice.l1 + 
    varmodel$varresult[1]$dmidprice$coef[2] * dur.l1 + 
    varmodel$varresult[1]$dmidprice$coef[3] * x.l1 + 
    varmodel$varresult[1]$dmidprice$coef[4];
  
  dur = varmodel$varresult[2]$dur$coef[1] * dmidprice.l1 + 
    varmodel$varresult[2]$dur$coef[2] * dur.l1 + 
    varmodel$varresult[2]$dur$coef[3] * x.l1 + 
    varmodel$varresult[2]$dur$coef[4];
  
  x = varmodel$varresult[3]$x$coef[1] * dmidprice.l1 + 
    varmodel$varresult[3]$x$coef[2] * dur.l1 + 
    varmodel$varresult[3]$x$coef[3] * x.l1 + 
    varmodel$varresult[3]$x$coef[4];

  pred <- rbind(pred, c(dmidprice, dur, x));
  dmidprice.l1 = dmidprice
  dur.l1 = dur
  x.l1= x
}
pred
acf(varmodel$varresult[1]$x$resid)

#5
summary(bid)
adf.test(log(bid$bid), alternative="stationary", k=0)
adf.test(log(bid$ask), alternative="stationary", k=0)

#construct difference
bid$dif = log(bid$ask) - log(bid$bid)
summary(bid$dif)
adf.test(bid$dif, alternative="stationary", k=0)

bidmin = data.frame(bid = log(bid$bid), ask = log(bid$ask))
bidecm = data.frame(bid = log(bid$bid[2:length(bid$bid)]) - log(bid$bid[1:length(bid$bid)-1]), 
                    ask = log(bid$ask[2:length(bid$ask)]) - log(bid$ask[1:length(bid$ask)-1]),
                    diff = log(bid$bid[2:length(bid$bid)]) - log(bid$ask[2:length(bid$ask)]))

bidecmindicator = data.frame(bid = log(bid$bid[2:length(bid$bid)]) - log(bid$bid[1:length(bid$bid)-1]), 
                    ask = log(bid$ask[2:length(bid$ask)]) - log(bid$ask[1:length(bid$ask)-1]),
                    diff = log(bid$bid[2:length(bid$bid)]) - log(bid$ask[2:length(bid$ask)]),
                    indicator = bid$indicator[2:length(bid$indicator)])

bidecmindicatorinteract = data.frame(bid = log(bid$bid[2:length(bid$bid)]) - log(bid$bid[1:length(bid$bid)-1]), 
                    ask = log(bid$ask[2:length(bid$ask)]) - log(bid$ask[1:length(bid$ask)-1]),
                    diff = log(bid$bid[2:length(bid$bid)]) - log(bid$ask[2:length(bid$ask)]),
                    indicator = bid$indicator[2:length(bid$indicator)],
                    interact =  bid$indicator[2:length(bid$indicator)] * bid$volume[2:length(bid$volume)])

x = as.ts(bidmin$ask);
y = as.ts(bidmin$bid);
ecm = ecmSymFit(x=x, y=y, p=1)
?ecmSymFit
bidmin$ask
test = data.frame(ask = bidmin$ask[length(bidmin$ask)] - bidmin$ask[length(bidmin$ask)-1], 
                   bid = bidmin$bid[length(bidmin$bid)] - bidmin$bid[length(bidmin$bid)-1], 
                   z = (bidmin$ask[length(bidmin$ask)] - bidmin$bid[length(bidmin$bid)]));
test

summary(ecm$ecm.x)
predask = bidmin$ask[length(bidmin$ask)] + ecm$ecm.x$coef[1] + ecm$ecm.x$coef[2] * test$ask + ecm$ecm.x$coef[3] * test$bid + ecm$ecm.x$coef[4] * test$z
predbid = bidmin$bid[length(bidmin$bid)] + ecm$ecm.y$coef[1] + ecm$ecm.y$coef[2] * test$ask + ecm$ecm.y$coef[3] * test$bid + ecm$ecm.y$coef[4] * test$z
predask
predbid

bidvarmodel = VAR(bidmin, p=1, type="const")
bidvarmodel
summary(bidvarmodel)

predict(bidvarmodel, n.ahead = 1)

ecmmodel = VAR(bidecm, p=1, type="const")
ecmmodel

ecmindicatormodel = VAR(bidecmindicator, p=1, type="const")
ecmindicatormodel

ecmindicatorinteractionmodel = VAR(bidecmindicatorinteract, p=1, type="const")
ecmindicatorinteractionmodel

summary(ecmindicatormodel$varresult$bid)
summary(ecmindicatormodel$varresult$ask)
summary(ecmindicatorinteractionmodel$varresult$bid)
summary(ecmindicatorinteractionmodel$varresult$ask)

garchecm = garch(log(bid$midquote), order = c(c(1,1),0))
summary(garchecm)
bidscaledz = data.frame(bid = log(bid$bid[2:length(bid$bid)]) - log(bid$bid[1:length(bid$bid)-1]), 
                                                  ask = log(bid$ask[2:length(bid$ask)]) - log(bid$ask[1:length(bid$ask)-1]),
                                                  diff = log(bid$bid[2:length(bid$bid)]) - log(bid$ask[2:length(bid$ask)]),
                                                  scaledz = (log(bid$bid[2:length(bid$bid)]) - log(bid$ask[2:length(bid$ask)])) * garchecm$fitted.values[2:length(bid$ask)])

scaledzmodel = VAR(bidscaledz, p=1, type="const")
scaledzmodel