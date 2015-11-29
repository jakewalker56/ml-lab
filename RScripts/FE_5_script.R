#Windows
Dropbox = "C:\\users/Jake/Dropbox"
#Mac/Unix
Dropbox = "~/Dropbox"

path= "/Booth/Winter 2014/Financial Econometrics/Week 5/"
setwd(paste(Dropbox, path, sep=""))
spdaily = read.csv("sp500daily.csv")
library(tseries)
library(forecast)
summary(spdaily)

#2.a
adf.test(log(spdaily$Close), alternative="stationary", k=0)

#2.b
?arima
model=arima(log(spdaily$Close), order=c(1,0,0))
plot(log(spdaily$Close), type="l")

summary(model)
model
model$coef[1]
(1-model$coef[1]) * model$coef[2]
sqrt(diag(model$var.coef)) 

#2.c

spforecast <- data.frame(pred = log(spdaily$Close[length(spdaily$Close)]) + .00103302 * (1:50))
spforecast$pred_lo <- data.frame(pred_lo = spforecast$pred - (2 * ((1:50) * 0.0001335)^0.5))
spforecast$pred_hi <- data.frame(pred_hi = spforecast$pred + (2 * ((1:50) * 0.0001335)^0.5))
spforecast

#2.d
plot(spforecast$pred, ylim=c(7.3, 7.8), type="l")
lines(spforecast$pred_lo, col="red")
lines(spforecast$pred_hi, col="red")

#2.e

#2.f

#2.g
spreturnforecast <- data.frame(pred = .00103302 * (1:50))
spreturnforecast$pred_lo <- data.frame(pred_lo = spreturnforecast$pred - (2 * ((1:50) * 0.0001335)^0.5))
spreturnforecast$pred_hi <- data.frame(pred_hi = spreturnforecast$pred + (2 * ((1:50) * 0.0001335)^0.5))
spreturnforecast
plot(spreturnforecast$pred, ylim=c(-.15, .25), type="l")
lines(spreturnforecast$pred_lo, col="red")
lines(spreturnforecast$pred_hi, col="red")

#2.h
abline(-.1, 0)
0.0001335^0.5
2 * 0.01155422
(0.02310844^2-.00103302*4*0.1)^0.5
0.00103302 * 2
(0.02310844 + (0.000534 + 0.000413208)) / 0.00206604
(0.02310844 + (0.01099054)) / 0.00206604
5.865279^2
16.50451^2


#3.a
returns = ((spdaily$Close[2:length(spdaily$Close)] - spdaily$Close[1:length(spdaily$Close)-1]) / (spdaily$Close[1:length(spdaily$Close)-1]))^2
returns

k=100
?sum
avg_returns = c();
for (i in 1:(length(returns)-k))
{
  avg_returns[i] = sum(returns[i:(i+k)])/k
}
avg_returns
plot(returns[k:length(returns)], type="l")
lines(avg_returns, type="l", col="red")

#3.b
k=252
?sum
avg_returns = c();
for (i in 1:(length(returns)-k))
{
  avg_returns[i] = sum(returns[i:(i+k)])/k
}
avg_returns
plot(returns[k:length(returns)], type="l")
lines(avg_returns, type="l", col="red")
