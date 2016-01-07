setwd("~/Dropbox/Booth/Fall 2013/Regression/Data")
transforms = read.csv("transforms.csv")
cheese = read.csv("cheese.csv")
summary(transforms)
print_legend <- function (location, model){
  legend(location, paste("y = ", round(model$coefficients[2], digits = 2), "x ", (if(sign(model$coef[1]) == 1) "+" else "-"), abs(round(model$coefficients[1], digits=2))))
  return(0)
}

#1.1.i- 1
i = 1
y_model = lm(transforms[,2*i] ~ transforms[,2*i-1])
plot(transforms[,2*i-1], transforms[,2*i], xlab="X1", ylab="Y1")
abline(y_model, col="red")
print_legend("topright", y_model)
plot(transforms[,2*i-1], rstudent(y_model), xlab="X1", ylab="rstudent(Y1)")
qqnorm(rstudent(y_model))
abline(0,1, col="red")
hist(rstudent(y_model), xlab="rstudent(Y1)")
ylog = log(transforms[,2*i]) 
x = transforms[,2*i-1]
ylog_model = lm(ylog ~ x, y = TRUE)
plot(x, ylog_model$y, xlab="X1", ylab="log(y)")
abline(ylog_model)
legend("topright", paste("log(y) = ", round(ylog_model$coefficients[2], digits = 2), "x ", (if(sign(ylog_model$coef[1]) == 1) "+" else "-"), abs(round(ylog_model$coefficients[1], digits=2))))
plot(x, transforms[,2*i], ylim=c(0,2), xlab="X1", ylab="Y1")
xgrid=data.frame(x=seq(min(x)-.2, max(x), (max(x)-min(x))/100))
lines(xgrid$x, matrix(exp(predict(ylog_model,newdata=xgrid))), col="red")
ylog_model$coef[1]
legend("topright", paste(
  "y = exp(", 
  round(ylog_model$coef[2], digits = 2), "x) * exp(",
  abs(round(ylog_model$coefficients[1], digits=2)), ")"
))

x = transforms[,2*i-1]
x2 = transforms[,2*i-1]^2
y = transforms[,2*i]
ypoly_model = lm(y ~ x + x2, y = TRUE)
plot(x, y, ylim=c(0, 2))
xgrid=seq(min(x)-.2, max(x), (max(x)-min(x))/100)
lines(xgrid, ypoly_model$coef[1] + ypoly_model$coef[2]*xgrid + ypoly_model$coef[3]*xgrid^2, col = "red")
legend("topright", paste(
  "y = ", 
  round(ypoly_model$coef[3], digits = 2), "x^2 ",
  (if(sign(ypoly_model$coef[2]) == 1) "+" else "-"), abs(round(ypoly_model$coef[2], digits = 2)), "x ",
  (if(sign(ypoly_model$coef[1]) == 1) "+" else "-"), abs(round(ypoly_model$coefficients[1], digits=2))
  ))
par(mfrow=c(1,2))
plot(transforms[,2*i-1], rstudent(ylog_model), xlab=paste("X1"), ylab=paste("rstudent(log(Y1))"))
plot(transforms[,2*i-1], rstudent(ypoly_model), xlab=paste("X1"), ylab=paste("rstudent(poly(Y1))"))

qqnorm(rstudent(ylog_model), main="log")
abline(0,1, col="red")
qqnorm(rstudent(ypoly_model), main = "poly")
abline(0,1, col="red")
hist(rstudent(ylog_model), xlab="rstudent(log(y))")
hist(rstudent(ypoly_model), xlab="rstudent(poly(y))")
par(mfrow=c(1,1))


#1.1.i- 2
i = 2
y_model = lm(transforms[,2*i] ~ transforms[,2*i-1])
plot(transforms[,2*i-1], transforms[,2*i], xlab="X2", ylab="Y2")
abline(y_model, col="red")
print_legend("bottomright", y_model)
plot(transforms[,2*i-1], rstudent(y_model), xlab="X2", ylab="rstudent(Y2)")
qqnorm(rstudent(y_model))
abline(0,1, col="red")
hist(rstudent(y_model), xlab="rstudent(Y2)")

x = transforms[,2*i-1]
x2 = transforms[,2*i-1]^2
y = transforms[,2*i]
ypoly_model = lm(y ~ x + x2, y = TRUE)
plot(x, y)
xgrid=seq(min(x)-.2, max(x), (max(x)-min(x))/100)
lines(xgrid, ypoly_model$coef[1] + ypoly_model$coef[2]*xgrid + ypoly_model$coef[3]*xgrid^2, col = "red")
legend("bottomright", paste(
  "y = ", 
  round(ypoly_model$coef[3], digits = 2), "x^2 ",
  (if(sign(ypoly_model$coef[2]) == 1) "+" else "-"), abs(round(ypoly_model$coef[2], digits = 2)), "x ",
  (if(sign(ypoly_model$coef[1]) == 1) "+" else "-"), abs(round(ypoly_model$coefficients[1], digits=2))
))
plot(transforms[,2*i-1], rstudent(ypoly_model), xlab=paste("X2"), ylab=paste("rstudent(poly(Y2))"))
qqnorm(rstudent(ypoly_model))
abline(0,1, col="red")
hist(rstudent(ypoly_model), xlab="rstudent(poly(Y2))")

#1.1.i- 3
##TODO: remove outliers, replot
i = 3
y_model = lm(transforms[,2*i] ~ transforms[,2*i-1])
plot(transforms[,2*i-1], transforms[,2*i], xlab="X3", ylab="Y3")
abline(y_model, )
print_legend("topright", y_model)
plot(transforms[,2*i-1], rstudent(y_model), xlab="X3", ylab="rstudent(Y3)")
qqnorm(rstudent(y_model))
abline(0,1, col="red")
hist(rstudent(y_model), xlab="rstudent(Y3)")
y = transforms[,2*i]
x = transforms[,2*i-1]
xlog=log(x)
xlog_model = lm(y ~ xlog, y = TRUE)
plot(xlog, xlog_model$y)
abline(xlog_model, col="red")
legend("topright", paste(
  "y = ", 
  round(xlog_model$coef[2], digits = 2), "*log(x) +",
  abs(round(xlog_model$coefficients[1], digits=2))
))

plot(x, y)
xgrid=data.frame(x=seq(min(x), max(x), (max(x)-min(x))/100))
xloggrid=data.frame(xlog=log(xgrid$x))
lines(xgrid$x, matrix(predict(xlog_model,newdata=xloggrid)), col="red")
legend("topright", paste(
  "y = ", 
  round(xlog_model$coef[2], digits = 2), "*log(x) +",
  abs(round(xlog_model$coefficients[1], digits=2))
))

plot(xlog, rstudent(xlog_model), xlab=paste("log(X3)"), ylab=paste("rstudent(Y3)"))
qqnorm(rstudent(xlog_model))
abline(0,1, col="red")
hist(rstudent(xlog_model), xlab="rstudent(log(X3))")



#1.1.i- 4
i = 4
y_model = lm(transforms[,2*i] ~ transforms[,2*i-1])
plot(transforms[,2*i-1], transforms[,2*i], xlab="X4", ylab="Y4")
abline(y_model)
print_legend("topleft", y_model)
plot(transforms[,2*i-1], rstudent(y_model), xlab="X4", ylab="rstudent(Y4)")
qqnorm(rstudent(y_model))
abline(0,1, col="red")
hist(rstudent(y_model), xlab="rstudent(Y4)")
ylog = log(transforms[,2*i]) 
x = transforms[,2*i-1]
xlog = log(x)
ylog_model = lm(ylog ~ xlog, y = TRUE)
plot(xlog, ylog_model$y, ylab="log(Y4)", xlab="log(X4)")
abline(ylog_model, col="red")
legend("topleft", paste(
  "log(y) = ", 
  round(ylog_model$coef[2], digits = 2), "*log(x) +",
  abs(round(ylog_model$coefficients[1], digits=2))
))
plot(x, transforms[,2*i], xlab="X4", ylab="Y4")
xgrid=data.frame(xlog=seq(min(xlog)-.2, max(xlog), (max(xlog)-min(xlog))/100))
lines(exp(xgrid$x), matrix(exp(predict(ylog_model,newdata=xgrid))), col="red")
legend("topleft", paste(
  "y = exp(", 
  round(ylog_model$coef[2], digits = 2), "x) * exp(",
  abs(round(ylog_model$coefficients[1], digits=2)), ")"
))

x = transforms[,2*i-1]
x2 = transforms[,2*i-1]^2
y = transforms[,2*i]
ypoly_model = lm(y ~ x + x2, y = TRUE)
plot(x, y)
xgrid=seq(min(x)-.2, max(x), (max(x)-min(x))/100)
lines(xgrid, ypoly_model$coef[1] + ypoly_model$coef[2]*xgrid + ypoly_model$coef[3]*xgrid^2, col = "red")
legend("topleft", paste(
  "y = ", 
  round(ypoly_model$coef[3], digits = 2), "x^2 ",
  (if(sign(ypoly_model$coef[2]) == 1) "+" else "-"), abs(round(ypoly_model$coef[2], digits = 2)), "x ",
  (if(sign(ypoly_model$coef[1]) == 1) "+" else "-"), abs(round(ypoly_model$coefficients[1], digits=2))
))
par(mfrow=c(1,2))
plot(xlog, rstudent(ylog_model), xlab=paste("log(X4)"), ylab=paste("rstudent(log(Y4))"))
plot(transforms[,2*i-1], rstudent(ypoly_model), xlab=paste("X4"), ylab=paste("rstudent(poly(Y4))"))
qqnorm(rstudent(ylog_model))
abline(0,1, col="red")
qqnorm(rstudent(ypoly_model))
abline(0,1, col="red")
hist(rstudent(ylog_model))
hist(rstudent(ypoly_model))
par(mfrow=c(1,1))

#1.2.i
price_ad = cheese[cheese$disp == 1,]
price_noad = cheese[cheese$disp == 0,]
summary(cheese)
summary(log(price_ad$vol))
summary(log(price_noad$vol))

par(mfrow=c(1,1))
boxplot(log(vol)~disp, cheese, names=c("no display", "display"), ylab="log(sales)")
exp(0.44)
model<-lm(cheese$vol ~ cheese$disp)
anovamodel_disp <- anova(model)
ssr = anovamodel_disp$Sum[1]
sse = anovamodel_disp$Sum[2]
sst = ssr + sse
#percent explained by the grouping with sex
ssr
sst
ssr / sst
anovamodel_disp
summary(anovamodel_disp)

#1.2.ii
#todo: anova model
price_ad = cheese[cheese$disp == 1,]
price_noad = cheese[cheese$disp == 0,]

model_ad=lm(log(vol)~log(price), price_ad) 
model_noad=lm(log(vol)~log(price), price_noad) 
model_ad_inv=lm(log(price)~log(vol), price_ad) 
model_noad_inv=lm(log(price)~log(vol), price_noad) 
summary(model_ad)$coefficients
summary(model_ad_inv)$coefficients
summary(model_noad)$coefficients
summary(model_noad_inv)$coefficients


plot(log(price_noad$price), log(price_noad$vol), main="No Display", xlab="log(Price)", ylab="log(Sales)", ylim=c(4,11))
abline(model_noad, col="red")
legend("bottomleft", paste(
  "log(Sales) = ", 
  round(model_noad$coef[2], digits = 4), "*log(Price) +",
  abs(round(model_noad$coefficients[1], digits=2))
))

plot(log(price_ad$price), log(price_ad$vol), xlab="log(Price)", ylab="log(Sales)", main="Display", ylim=c(3,12))
abline(model_ad, col="red")
legend("bottomleft", paste(
  "log(Sales) = ", 
  round(model_ad$coef[2], digits = 4), "*log(Price) +",
  abs(round(model_ad$coefficients[1], digits=2))
))

summary(model_noad)
summary(model_ad)$coefficients
summary(model_noad)$coefficients
sdb1 = (summary(model_ad)$coefficients[2,2]^2 + summary(model_noad)$coefficients[2,2]^2)^0.5
sdb1
eb1 = summary(model_ad)$coefficients[2,1]-summary(model_noad)$coefficients[2,1]
eb1
qnorm(0.975, mean=eb1, sd=sdb1)
qnorm(0.025, mean=eb1, sd=sdb1)

#1.3.i
news = read.csv("newspaper.csv")
plot(news$daily, news$Sunday, xlab="daily", ylab="Sunday", pch=20)
news_lm = lm(Sunday ~ daily, news)
abline(news_lm, col="red")
legend("topleft", paste(
  "Sunday = ", 
  round(news_lm$coef[2], digits = 2), "*daily +",
  abs(round(news_lm$coefficients[1], digits=2))
))

confint(news_lm)
summary(news_lm)

Xf <- data.frame(daily=c(225))
predict(news_lm, newdata=Xf, interval="prediction", se.fit=TRUE)
summary(news_lm)$sigma 


plot(log(news$daily), log(news$Sunday), pch=20, xlab="log(daily)", ylab="log(Sunday)", ylim = c(4.5, 8))
log_sunday = log(news$Sunday)
log_daily = log(news$daily)
log_news_lm = lm(log_sunday ~ log_daily)
abline(log_news_lm, col="red")
confint(log_news_lm)
legend("bottomright", paste(
  "log(Sunday) = ", 
  round(news_lm$coef[2], digits = 2), "*log(daily) +",
  abs(round(news_lm$coefficients[1], digits=2))
))
summary(log_news_lm)
Xf <- data.frame(log_daily=c(log(225)))
prediction = predict(log_news_lm, newdata=Xf, interval="prediction", se.fit=TRUE)
exp(prediction$fit)
prediction$fit

Xf <- data.frame(log_daily=log(seq(1,2000,10)))
prediction = predict(log_news_lm, newdata=Xf, interval="prediction", se.fit=TRUE)
lines(Xf$log_daily, matrix(prediction$fit[,2]))
lines(Xf$log_daily, prediction$fit[,3])

plot(news$daily, news$Sunday, pch=20)
Xf <- data.frame(log_daily=log(seq(1,2000,10)))
prediction = predict(log_news_lm, newdata=Xf, interval="prediction", se.fit=TRUE)
lines(exp(Xf$log_daily), exp(matrix(prediction$fit[,2])))
lines(exp(Xf$log_daily), exp(prediction$fit[,3]))
lines(exp(Xf$log_daily), exp(matrix(prediction$fit[,1])), col="red")
news_lm
plot(log(news$daily), rstudent(log_news_lm))
plot(news$daily, rstudent(news_lm))