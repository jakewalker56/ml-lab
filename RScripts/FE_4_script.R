#Windows
#Dropbox = "C:\\users/Jake/Dropbox"
#Mac/Unix
Dropbox = "~/Dropbox"

path= "/Booth/Winter 2014/Financial Econometrics/Week 4/"
setwd(paste(Dropbox, path, sep=""))
fedfunds = read.csv("fedfunds.csv")

summary(fedfunds)
fedfunds

#2.a
png(filename=paste(sep="", Dropbox, path, "2.a.1.png"))
plot(fedfunds$FFO, type="l");
dev.off();
mean(fedfunds$FFO);
sd(fedfunds$FFO);
se(fedfunds$FFO);
png(filename=paste(sep="", Dropbox, path, "2.a.2.png"))
acf(fedfunds$FFO)
dev.off();

#2.b
var(fedfunds$FFO);

#2.c
?arima
model.BIC <- numeric(0);
for(i in 1:7)
{
  for(j in 1:7)
  {
    if(j==1 && i ==1)
      next;
    model = arima(fedfunds$FFO, order= c(i,0,j));
    model.BIC <- c(model.BIC, model$aic);        
  }
}

model.BIC


eBIC <- exp(-0.5*(model.BIC-min(model.BIC)))
max(round(probs <- eBIC/sum(eBIC), 5))
round(probs <- eBIC/sum(eBIC), 5)
model.BIC;

acf(fedfunds$FFO)
pacf(fedfunds$FFO)
?arima
model1 = arima(fedfunds$FFO, order= c(4,0,1));
model2 = arima(fedfunds$FFO, order= c(3,0,4));
model1
model2

png(filename=paste(sep="", Dropbox, path, "2.c.1.png"))
acf(model1$resid)
dev.off();

png(filename=paste(sep="", Dropbox, path, "2.c.2.png"))
pacf(model1$resid)
dev.off();

png(filename=paste(sep="", Dropbox, path, "2.c.3.png"))
plot(model1$resid, type="p", pch=20)
dev.off();

png(filename=paste(sep="", Dropbox, path, "2.c.4.png"))
acf(model2$resid)
dev.off()

png(filename=paste(sep="", Dropbox, path, "2.c.5.png"))
pacf(model2$resid)
dev.off()

png(filename=paste(sep="", Dropbox, path, "2.c.6.png"))
plot(model2$resid, type="p", pch=20)
dev.off();

#2.d

predicted1 = fedfunds$FFO + model1$resid
predicted1
png(filename=paste(sep="", Dropbox, path, "2.d.1.png"))
plot(predicted1, col="blue")
lines(fedfunds$FFO, col="red")
dev.off();

predicted2 = fedfunds$FFO + model2$resid
predicted2
png(filename=paste(sep="", Dropbox, path, "2.d.2.png"))
plot(predicted2, col="blue")
lines(fedfunds$FFO, col="red")
dev.off()

#3.a
#if prediction is higher than future rate, we gain actual - future.
#otherwise, we gain future - actual
returns1 <- numeric(0);
returns2 <- numeric(0);
for(i in 2:length(predicted1))
{
  if(predicted1[i] > fedfunds$Future[i])
  {
    returns1 <- c(returns1, fedfunds$FFO[i] - fedfunds$Future[i])
  }
  else
  {
    returns1 <- c(returns1, fedfunds$Future[i] - fedfunds$FFO[i])
  }
}
for(i in 2:length(predicted2))
{
  if(predicted2[i] > fedfunds$Future[i])
  {
    returns2 <- c(returns2, fedfunds$FFO[i] - fedfunds$Future[i])
  }
  else
  {
    returns2 <- c(returns2, fedfunds$Future[i] - fedfunds$FFO[i])
  }
}
png(filename=paste(sep="", Dropbox, path, "3.a.1.png"))
plot(returns1)
dev.off()
png(filename=paste(sep="", Dropbox, path, "3.a.2.png"))
plot(returns2)
dev.off()

#3.b
var(returns1)
mean(returns1)

var(returns2)
mean(returns2)

#3.c
t1 = mean(returns1)/(sd(returns1)/(length(returns1)^0.5))
t1
t2 = mean(returns2)/(sd(returns2)/(length(returns2)^0.5))
t2

#5
png(filename=paste(sep="", Dropbox, path, "5.a.1.png"))
plot(c(fedfunds$FFO, predict(model1, n.ahead=12*20)$pred), type="l")
dev.off()