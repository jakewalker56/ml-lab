setwd("~/Dropbox/Booth/Fall 2013/Regression/Data")
setwd("C:\\Users\\Jake\\Dropbox\\Booth\\Fall 2013\\Regression\\Data")

#1
housetax = read.csv("housetax.csv")
summary(housetax)
#plot(housetax$sqft, housetax$price,pch=20, xlab="sq ft", ylab="price")
png(filename="C:\\Users\\Jake\\Dropbox\\Booth\\Fall 2013\\Regression\\Homeworks\\Midterm\\1.1.png")
plot(housetax, pch=20)
dev.off()
#all three strong positive correlation, but all 3 have strong trumpeting toward the right- variance
#increases as values increase

#2
model = lm(tax~price, data=housetax)
png(filename="C:\\Users\\Jake\\Dropbox\\Booth\\Fall 2013\\Regression\\Homeworks\\Midterm\\2.1.png")
plot(housetax$price, housetax$tax,pch=20, xlab="price", ylab="tax")
abline(model, col="red")
legend("topleft", paste("y = ", round(model$coefficients[2], digits = 2), "x +", round(model$coefficients[1], digits=2)), fill="red")
dev.off()
summary(model)
#76% of variability explained by relationship, Pr(>|t|) << 0.05, high confidence in statistically significant
#relationship

#price in units of $100, tax in units of $1
#increase in price of $100 leads to an increase in tax of B1 (slope = rise / run)
#B1 = 0.70278, std error = 0.03782
qnorm(0.975, mean=0.70278, sd=0.03782)
qnorm(0.025, mean=0.70278, sd=0.03782)
confint(model)

#3
#lots of variance around the extreme right, trumpeting behavior- try log(y) transform
tax = housetax$tax
price = housetax$price
sqft = housetax$sqft
price2 = price^2
logtax = log(housetax$tax)
logprice = log(housetax$price)
logprice2 = logprice^2
logsqft = log(sqft)
sqft2 = sqft^2

sqftmodel = lm(tax~price + sqft)
logmodel = lm(logtax~logprice)
logxmodel = lm(tax~logprice)
logymodel = lm(logtax~price)
loglogsqftmodel = lm(logtax~logprice + logsqft)
logxlogsqftmodel = lm(tax~logprice + logsqft)
model = lm(tax~price, data=housetax)
logpolymodel = lm(logtax~logprice + logprice2)
logxpolymodel = lm(tax~logprice + logprice2)

logpolylogsqftmodel = lm(logtax~logprice + logprice2 + logsqft)
logxpolylogsqftmodel = lm(tax~logprice + logprice2 + logsqft)
polymodel = lm(tax~price + price2)
polysqftmodel = lm(tax~price + price2 + sqft)
polypolysqftmodel = lm(tax~price + price2 + sqft + sqft2)
polylogsqftmodel = lm(tax~price + price2 + logsqft)

png(filename="C:\\Users\\Jake\\Dropbox\\Booth\\Fall 2013\\Regression\\Homeworks\\Midterm\\3.1.png")
plot(price, rstudent(model), pch = 20)
dev.off()
png(filename="C:\\Users\\Jake\\Dropbox\\Booth\\Fall 2013\\Regression\\Homeworks\\Midterm\\3.2.png")
hist(rstudent(model))
dev.off()

png(filename="C:\\Users\\Jake\\Dropbox\\Booth\\Fall 2013\\Regression\\Homeworks\\Midterm\\3.3.png")
plot(log(housetax$price), log(housetax$tax), pch=20, xlab="log(price)", ylab="log(tax)")
abline(logmodel, col="red")
Xf <- data.frame(logprice=log(seq(1,3000,10)))
prediction = predict(logmodel, newdata=Xf, interval="prediction", se.fit=TRUE)
lines(Xf$logprice, matrix(prediction$fit[,2]))
lines(Xf$logprice, prediction$fit[,3])
legend("topleft", paste("y = ", round(logmodel$coefficients[2], digits = 4), "x ", round(logmodel$coefficients[1], digits=2)), fill="red")
dev.off()

png(filename="C:\\Users\\Jake\\Dropbox\\Booth\\Fall 2013\\Regression\\Homeworks\\Midterm\\3.4.png")
plot(log(housetax$price), housetax$tax, pch=20, xlab="log(price)", ylab="tax")
abline(logxmodel, col="red")
Xf <- data.frame(logprice=log(seq(1,3000,10)))
prediction = predict(logxmodel, newdata=Xf, interval="prediction", se.fit=TRUE)
lines(Xf$logprice, matrix(prediction$fit[,2]))
lines(Xf$logprice, prediction$fit[,3])
legend("topleft", paste("y = ", round(logxmodel$coefficients[2], digits = 4), "x -", abs(round(logxmodel$coefficients[1], digits=2))), fill="red")
dev.off()

plot(log(housetax$price), log(housetax$tax), pch=20, xlab="log(price)", ylab="log(tax)")
abline(logmodel, col="red")
Xf <- data.frame(logprice=log(seq(1,3000,10)))
prediction = predict(logmodel, newdata=Xf, interval="prediction", se.fit=TRUE)
lines(Xf$logprice, matrix(prediction$fit[,2]))
lines(Xf$logprice, prediction$fit[,3])
legend("topleft", paste("y = ", round(logmodel$coefficients[2], digits = 4), "x -", abs(round(logmodel$coefficients[1], digits=2))), fill="red")


summary(logmodel)
summary(model)
summary(logxmodel)

#//3.5, 3.6- by hand
par(mfrow=c(1,2))
plot(logprice, rstudent(logmodel), ylab="rstudent(log(tax))")
plot(logprice, rstudent(logxmodel), ylab="rstudent(tax)")
hist(rstudent(logmodel))
hist(rstudent(logxmodel))
par(mfrow=c(1,1))

png(filename="C:\\Users\\Jake\\Dropbox\\Booth\\Fall 2013\\Regression\\Homeworks\\Midterm\\3.7.png")
plot(housetax$price, housetax$tax, pch=20)
Xf <- data.frame(logprice=log(seq(1,3000,10)))
prediction = predict(logxmodel, newdata=Xf, interval="prediction", se.fit=TRUE)
lines(exp(Xf$logprice), matrix(prediction$fit[,2]))
lines(exp(Xf$logprice), prediction$fit[,3])
lines(exp(Xf$logprice), matrix(prediction$fit[,1]), col="red")
abline(originalmodel, col="green")
legend("topleft", c(
       paste("y = ", round(logxmodel$coefficients[2], digits = 4), "*log(x) ", round(logxmodel$coefficients[1], digits=2)),
       paste("y = ", round(model$coefficients[2], digits = 4), "x +", round(model$coefficients[1], digits=2))), 
       fill=c("red", "green"))
dev.off()


#4/5
png(filename="C:\\Users\\Jake\\Dropbox\\Booth\\Fall 2013\\Regression\\Homeworks\\Midterm\\4.1.png")
plot(log(housetax$price), housetax$tax, pch=20, xlab="log(price)", ylab="tax")
Xf <- data.frame(logprice=log(seq(1,3000,10)), logprice2=(log(seq(1,3000,10))^2))
prediction = predict(logxpolymodel, newdata=Xf, interval="prediction", se.fit=TRUE)
lines(Xf$logprice, matrix(prediction$fit[,2]))
lines(Xf$logprice, prediction$fit[,3])
lines(Xf$logprice, prediction$fit[,1], col="red")
legend("topleft", paste(
  "y = ", 
  round(logxpolymodel$coef[3], digits = 2), "x^2 ",
  (if(sign(logxpolymodel$coef[2]) == 1) "+" else "-"), abs(round(logxpolymodel$coef[2], digits = 2)), "x ",
  (if(sign(logxpolymodel$coef[1]) == 1) "+" else "-"), abs(round(logxpolymodel$coefficients[1], digits=2))
), fill = "red")
dev.off()

png(filename="C:\\Users\\Jake\\Dropbox\\Booth\\Fall 2013\\Regression\\Homeworks\\Midterm\\4.2.png")
plot(log(housetax$price), log(housetax$tax), pch=20, xlab="log(price)", ylab="log(tax)")
Xf <- data.frame(logprice=log(seq(1,3000,10)), logprice2=(log(seq(1,3000,10))^2))
prediction = predict(logpolymodel, newdata=Xf, interval="prediction", se.fit=TRUE)
lines(Xf$logprice, matrix(prediction$fit[,2]))
lines(Xf$logprice, prediction$fit[,3])
lines(Xf$logprice, prediction$fit[,1], col="red")
legend("topleft", paste(
  "y = ", 
  round(logpolymodel$coef[3], digits = 2), "x^2 ",
  (if(sign(logpolymodel$coef[2]) == 1) "+" else "-"), abs(round(logpolymodel$coef[2], digits = 2)), "x ",
  (if(sign(logpolymodel$coef[1]) == 1) "+" else "-"), abs(round(logpolymodel$coefficients[1], digits=2))
), fill = "red")
dev.off()

png(filename="C:\\Users\\Jake\\Dropbox\\Booth\\Fall 2013\\Regression\\Homeworks\\Midterm\\4.3.png")
plot(housetax$price, housetax$tax, pch=20)
Xf <- data.frame(price=seq(1,3000,10), price2=seq(1,3000,10)^2)
prediction = predict(polymodel, newdata=Xf, interval="prediction", se.fit=TRUE)
lines(Xf$price, matrix(prediction$fit[,2]))
lines(Xf$price, prediction$fit[,3])
lines(Xf$price, prediction$fit[,1], col="red")
legend("topleft", paste(
  "y = ", 
  round(polymodel$coef[3], digits = 8), "x^2 ",
  (if(sign(polymodel$coef[2]) == 1) "+" else "-"), abs(round(polymodel$coef[2], digits = 2)), "x ",
  (if(sign(polymodel$coef[1]) == 1) "+" else "-"), abs(round(polymodel$coefficients[1], digits=2))
), fill = "red")
dev.off()


summary(logxpolymodel)
summary(logpolymodel)
summary(polymodel)

par(mfrow=c(1,3))
plot(logprice, rstudent(logxpolymodel), pch=20)
plot(logprice, rstudent(logpolymodel), pch=20)
plot(price, rstudent(polymodel), pch=20)
hist(rstudent(logxpolymodel))
hist(rstudent(logpolymodel))
hist(rstudent(polymodel))
par(mfrow=c(1,1))

#6
par(mfrow=c(1,4))

summary(polymodel)
summary(sqftmodel)
summary(polysqftmodel)
summary(polypolysqftmodel)

hist(rstudent(polymodel))
hist(rstudent(sqftmodel))
hist(rstudent(polysqftmodel))
hist(rstudent(polypolysqftmodel))

plot(price, rstudent(polymodel))
plot(price, rstudent(sqftmodel))
plot(price, rstudent(polysqftmodel))
plot(price, rstudent(polypolysqftmodel))

par(mfrow=c(1,1))

#---------

plot(logprice, housetax$tax, pch=20)
Xf <- data.frame(logprice=log(seq(1,3000,10)))
prediction = predict(logxmodel, newdata=Xf, interval="prediction", se.fit=TRUE)
lines(Xf$logprice, matrix(prediction$fit[,2]))
lines(Xf$logprice, prediction$fit[,3])
lines(Xf$logprice, prediction$fit[,1], col="red")
summary(logxmodel)

plot(price, logtax, pch=20)
Xf <- data.frame(price=seq(1,3000,10))
prediction = predict(logymodel, newdata=Xf, interval="prediction", se.fit=TRUE)
lines(Xf$price, matrix(prediction$fit[,2]))
lines(Xf$price, prediction$fit[,3])
lines(Xf$price, prediction$fit[,1], col="red")
summary(logymodel)

#todo: frame the data x, y, logx, logy = 4x4 graph, interesting

plot(data.frame(price=price, tax=tax, logprice= logprice, logtax= logtax))
