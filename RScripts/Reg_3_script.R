setwd("~/Dropbox/Booth/Fall 2013/Regression/Data")
#1.1.i
news = read.csv("newspaper.csv")
plot(news$daily, news$Sunday,pch=20)

#1.1.ii
plot(news$daily, news$Sunday, pch=20)
news_lm = lm(Sunday ~ daily, news)
abline(news_lm, col="red")
confint(news_lm)
legend("topleft", "y = 1.2767*x + 76.01", fill = 2)
#TODO: figure out how to do this by hand
# qt(0.025, length(news[,1]) - 2)
# qt(0.975, length(news[,1]) - 2) * summary(news_lm)$sigma
# sx = 
# summary(news_lm)$sigma

#1.1.iii
summary(news_lm)

#1.1.iv
Xf <- data.frame(daily=c(225))
predict(news_lm, newdata=Xf, interval="prediction", se.fit=TRUE)
summary(news_lm)$sigma 
(27.57 ^2 + 154.32 ^2)^0.5

#todo: do by hand
#expected_value = ((summary(news_lm)$sigma)^2 + (sfit)^2)^0.5

#1.3.i
#1.3.ii
#1.3.iii
crime = read.csv("crime.csv")
crime$Ed
crime$LF
crime$W
summary(crime)

plot(crime$Ed, crime$CR, xlab="Ed", ylab="CR")
model=lm(CR ~ Ed, crime)
abline(model)
legend("topleft", paste("y = ", round(model$coefficients[2], digits = 2), "x ", round(model$coefficients[1], digits=2)))
summary(model)

plot(crime$LF, crime$CR, xlab = "LF", ylab="CR")
model=lm(CR ~ LF, crime)
abline(model)
legend("topleft", paste("y = ", round(model$coefficients[2], digits = 2), "x ", round(model$coefficients[1], digits=2)))
summary(model)

plot(crime$W, crime$CR, xlab = "W", ylab="CR")
model=lm(CR ~ W, crime)
abline(model)
legend("topleft", paste("y = ", round(model$coefficients[2], digits = 2), "x + ", round(model$coefficients[1], digits=2)))
summary(model)

plot(crime$Ed, crime$LF, xlab = "Ed", ylab="LF")
abline(lm(LF ~ Ed, crime))
plot(crime$Ed, crime$W, xlab="Ed", ylab="W")
abline(lm(W ~ Ed, crime))
plot(crime$LF, crime$W, xlab="LF", ylab="W")
abline(lm(W ~ LF, crime))

residuals = crime$CR - predict(lm(CR ~ W, crime))
residuals
plot(crime$W, residuals, xlab="W")

#1.3.iv
crime_lm = lm(CR ~ W, crime)
w = data.frame(W=c(275))
predict(crime_lm, w, interval="prediction", level=0.9)

#1.4.i
tractors = read.csv("tractor.csv")
model=lm(cost ~ age, tractors)
age = data.frame(age=c(3))
predict(model, age, interval="prediction", se.fit=TRUE)
summary(model)$sigma^2
76.94126^2
(5919 + 96277)^0.5
ranges = data.frame(age=seq(0,10, 0.5))
predictions=predict(model,ranges, interval="prediction")
plot(ranges$age, predictions[,1], ylim=c(-500,2500), xlab="years", ylab="cost")
lines(ranges$age, predictions[,2])
lines(ranges$age, predictions[,3])

anova(model)$Sum[2]
anova(model)$Sum[1]

#1.2

SST=1.50773 * 10^12
SSE=18007.56^2*16
SSR = (SST - )
SSE
SSR
R_2 = SSR/SST

e_val = 0.9821 * 822000 + 6805 
R_2
(822000 - 622186)^2/(17 * 302724^2)
(1 + 1/18 + (822000 - 622186)^2/(17 * 302724^2))^0.5 

se = 18007.56 * 1.0398
qt(0.975, 16) * se + e_val
qt(0.025, 16) * se + e_val

se = 0.01443
e_val = 0.9821
qt(0.975, 16) * se + e_val
qt(0.025, 16) * se + e_val

summary(lm(Ed ~ CR, crime))
summary(lm(CR ~ Ed, crime))

18007.56^2
0.9966^0.5

SST/17
?predict