Dropbox = "C:\\users/Jake/Dropbox"
#Dropbox = "~/Dropbox"
path= "/Booth/Winter 2014/Financial Econometrics/Week 2/"
setwd(paste(Dropbox, path, sep=""))
hp = read.csv("hpchicago.csv")
mortgage = read.csv("mortgage.csv")
mortgage

#1.a
acf(hp$ret_raw[2:length(hp$ret_raw)], lag.max = 48)

#1.c
acf(hp$ret_sa[2:length(hp$ret_sa)], lag.max = 48)


#1.d
x=hp$ret_sa[2:(length(hp$ret_sa)-1)]
model = lm(hp$ret_sa[3:length(hp$ret_sa)] ~ x)
model

#1.e
acf(model$resid)

#1.f
plot(hp$ret_sa[3:length(hp$ret_sa)], ylab="Returns", pch=20, col="blue")
points(predict(model), pch=13, col="red")
legend("bottomleft", c("Actual", "Prediction"), pch = c(20,13), col=c("blue", "red"))

#1.g
Xf <- data.frame(x=c(0.015155474))
predict(model, newdata=Xf, interval="prediction")
summary(model)$sigma

#2.a
acf(mortgage$VALUE, lag.max = 48)

#2.b
x=mortgage$VALUE[1:(length(mortgage$VALUE)-1)]
model = lm(mortgage$VALUE[2:length(mortgage$VALUE)] ~ x)
model

#2.c
acf(model$resid)

#2.d
mortgage$VALUE
Xf <- data.frame(x=c(4.46))
predict(model, newdata=Xf, interval="prediction")
summary(model)$sigma
