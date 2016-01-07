#Windows
Dropbox = "C:\\users/Jake/Dropbox"
#Mac/Unix
Dropbox = "~/Dropbox"

path= "/Booth/Winter 2014/Financial Econometrics/Week 8/"
setwd(paste(Dropbox, path, sep=""))
spdaily = read.csv("sp500_1990.csv")
sp = read.csv("sp.csv")
vix = read.csv("vix.csv")
library(fGarch)
library(nortest)
library(tseries)

#1
summary(sp)
sp$Adj.Close = rev(sp$Adj.Close)
sp$AR1 = c(sp$Adj.Close[1], sp$Adj.Close[1:length(sp$Adj.Close)-1])
sp$returns = log(sp$Adj.Close) - log(sp$AR1)
spfit = garch(sp$returns, order = c(c(1,1),0))


all_r = vector()
all_r
for(j in 1:2000)
{
last_r = sp$returns[length(sp$returns)]
last_h = abs(spfit$fitted[length(spfit$fitted)-1])^2
h = spfit$coef[1] + spfit$coef[2] * last_r^2 + spfit$coef[3] * last_h
last_r = rnorm(n=1, m=0, sd=1) * h^0.5
last_h = h
total = last_r
for(i in 1:29)
{
  h = spfit$coef[1] + spfit$coef[2] * last_r^2 + spfit$coef[3] * last_h
  last_r = rnorm(n=1, m=0, sd=1) * h^0.5
  last_h = h
  total = total + last_r
}
all_r <- c(all_r, total)
}
hist(all_r, breaks=19)
mean(all_r)
max(all_r)
min(all_r)

all_r2 = vector()
all_r2
for(j in 1:2000)
{
  rands = sample(2:length(sp$returns), 30, replace=T)
  last_r = sp$returns[length(sp$returns)]
  last_h = abs(spfit$fitted[length(spfit$fitted)-1])^2
  h = spfit$coef[1] + spfit$coef[2] * last_r^2 + spfit$coef[3] * last_h
  last_r = rnorm(n=1, m=0, sd=1) * h^0.5
  last_h = h
  total = last_r
  for(i in 2:30)
  {
    h = spfit$coef[1] + spfit$coef[2] * last_r^2 + spfit$coef[3] * last_h
    last_r =(sp$returns[rands[i]] / spfit$fitted[rands[i]] ) * h^0.5
    last_h = h
    total = total + last_r
  }
  all_r2 <- c(all_r2, total)
}
hist(all_r2, breaks=19)
mean(all_r2)
max(all_r2)
min(all_r2)

skewness(all_r)
skewness(all_r2)

kurtosis(all_r)
kurtosis(all_r2)

#1% value at risk is value below which 1% of cases falls
sorted_r = sort(all_r)
sorted_r[length(sorted_r)/100]

sorted_r2 = sort(all_r2)
sorted_r2[length(sorted_r2)/100]

#15%
x15 = length(sorted_r);
for(i in 1:length(sorted_r))
{
  if(sorted_r[i] <= -0.15)
  {
    x15 = i;
  }
  else
  {
    break;
  }
}
x15/length(sorted_r)

x152 = length(sorted_r2);
for(i in 1:length(sorted_r2))
{
  if(sorted_r2[i] <= -0.15)
  {
    x152 = i;
  }
  else
  {
    break;
  }
}
x152/length(sorted_r2)

x15plus = 0;
for(i in 1:length(sorted_r))
{
  if(sorted_r[i] >= 0.15)
  {
    x15plus = i;
    break;
  }
}
x15plus/length(sorted_r)

x15plus2 = 0;
for(i in 1:length(sorted_r2))
{
  if(sorted_r2[i] >= 0.15)
  {
    x15plus2 = i;
    break;
  }
}

x15plus2/length(sorted_r2)

#2
summary(spdaily)
spdaily$Adj.Close = rev(spdaily$Adj.Close)
spdaily$AR1 = c(spdaily$Adj.Close[1], spdaily$Adj.Close[1:length(spdaily$Adj.Close)-1])
spdaily$returns = log(spdaily$Adj.Close) - log(spdaily$AR1)
spdaily$crash = spdaily$returns < -.03
spdaily$crash_numeric = spdaily$crash + 0
mean(spdaily$crash_numeric)
hist(spdaily$crash_numeric)
plot(spdaily$crash_numeric)

#gfit = garchFit(formula = ~aparch(1, 1), delta=1, include.delta=FALSE, include.mean=FALSE, data = spdaily$returns)
gfit = garch(spdaily$returns, order = c(c(1,1),0))
summary(gfit)
plot(predict(gfit)[,1], type="l")
spdaily$ht = predict(gfit)[,1]^2
model=lm(crash ~ ht, data=spdaily)
summary(model)

#3
vix$Adj.Close = rev(vix$Adj.Close)

spdaily$prev_vix = c(vix$Adj.Close[1], vix$Adj.Close[1:length(spdaily$returns) - 1])
model=lm(crash ~ ht + prev_vix, data=spdaily)
summary(model)
plot(model$fit, type="l")
total_r = c(0);
for(i in 2:length(spdaily$returns))
{
   if(model$fit[i - 1] <= 0.05)
   {
     total_r = c(total_r, spdaily$returns[i]);
   }
}
sort(total_r)[length(total_r)/100]
sort(spdaily$returns)[length(spdaily$returns)/100]