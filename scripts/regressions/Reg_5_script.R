setwd("C:\\Users/Jake/Dropbox/Booth/Fall 2013/Regression/Data")
nutrition = read.csv("nutrition.csv")
beef = read.csv("beef.csv")

#1.1
png(filename="C:\\Users\\Jake\\Dropbox\\Booth\\Fall 2013\\Regression\\Homeworks\\HW5\\1.1.png")

plot(nutrition$age, nutrition$woh, xlab="Age", ylab="WoH", ylim=c(0.4, 1.1), pch=20)
model = lm(woh~age, data=nutrition)
abline(model)
xgrid=data.frame(age=seq(-5,75,0.5))
prediction = predict(model,newdata=xgrid, interval="prediction", se.fit=TRUE)
lines(xgrid$age, prediction$fit[,2], col="red")
lines(xgrid$age, prediction$fit[,3], col="red")
legend("topleft", paste("y = ", round(model$coefficients[2], digits = 4), "x ", round(model$coefficients[1], digits=2)))

dev.off()

#1.2
plot(nutrition$age, nutrition$woh, xlab="Age", ylab="WoH", ylim=c(0.4, 1.1), pch=20)

young <- nutrition$age < 7
new_nutrition = data.frame(nutrition, young)
new_model = lm(woh ~ age * young, data = new_nutrition)
summary(new_model)

age = seq(-10,75,0.5)
youngTRUE = age > -100
xgrid=data.frame(age, young=youngTRUE)
xgrid
prediction1 = predict(new_model,newdata=xgrid, interval="prediction", se.fit=TRUE)
lines(xgrid$age, prediction1$fit[,1], col="black")

youngFALSE = age < -100
xgrid=data.frame(age, young=youngFALSE)
xgrid
prediction2 = predict(new_model,newdata=xgrid, interval="prediction", se.fit=TRUE)
lines(xgrid$age, prediction2$fit[,1], col="black")


legend("bottomright", c(
  paste("y = ", round(new_model$coefficients[2], digits = 4), "* age + ", round(new_model$coefficients[1], digits=2)),
  paste("y = ", round(new_model$coefficients[2] + new_model$coefficients[4], digits = 4), "* age + ", round(new_model$coefficients[1] + new_model$coefficients[3], digits=2))
  ))

confint(new_model)

#1.3
png(filename="C:\\Users\\Jake\\Dropbox\\Booth\\Fall 2013\\Regression\\Homeworks\\HW5\\1.3.png")

plot(nutrition$age, nutrition$woh, xlab="Age", ylab="WoH", ylim=c(0.4, 1.1), pch=20)

age = seq(0.5,7.5,0.5)
youngTRUE = age > 0
xgrid=data.frame(age, young=youngTRUE)
prediction1 = predict(new_model,newdata=xgrid, interval="prediction", se.fit=TRUE)
lines(xgrid$age, prediction1$fit[,1], col="black")
lines(xgrid$age, prediction1$fit[,2], col="red")
lines(xgrid$age, prediction1$fit[,3], col="red")

age = seq(7.5,71.5,0.5)
youngFALSE = age < 0
xgrid=data.frame(age, young=youngFALSE)
prediction2 = predict(new_model,newdata=xgrid, interval="prediction", se.fit=TRUE)
lines(xgrid$age, prediction2$fit[,1], col="black")
lines(xgrid$age, prediction2$fit[,2], col="red")
lines(xgrid$age, prediction2$fit[,3], col="red")

legend("bottomright", c(
  paste("y = ", round(new_model$coefficients[2], digits = 4), "* age + ", round(new_model$coefficients[1], digits=2)),
  paste("y = ", round(new_model$coefficients[2] + new_model$coefficients[4], digits = 4), "* age + ", round(new_model$coefficients[1] + new_model$coefficients[3], digits=2))
))
dev.off()

#1.4
# The partial F-test is concerned with the hypotheses
# H0 : dbase+1 = dbase+2 =    = dfull = 0
# H1 : at least one j 6= 0 for j > dbase :
anova(model,new_model)

#2.1
png(filename="C:\\Users\\Jake\\Dropbox\\Booth\\Fall 2013\\Regression\\Homeworks\\HW5\\2.1.png")
r <- rank(beef$VAL)
cols <- heat.colors(length(beef$VAL))[r]
plot(beef$SIZE, beef$YES, col=cols, xlab="Size", ylab="Yes")
dev.off()

#2.2
model = lm(YES ~ log(VAL) + SIZE, data = beef)
summary(model)

#2.3
model1 = lm(YES ~ log(VAL)*SIZE, data = beef)
summary(model1)

anova(model, model1)
