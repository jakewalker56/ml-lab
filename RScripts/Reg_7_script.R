Dropbox = "C:\\users/Jake/Dropbox"
Dropbox = "~/Dropbox"
path= "/Booth/Fall 2013/Regression/Homeworks/HW7/"
setwd(paste(Dropbox,"/Booth/Fall 2013/Regression/Data", sep=""))
gas = read.csv("UKGasConsumption.csv")
gas$log_gas = log(gas$gas)

#1.1
gas
summary(gas)
length(gas$gas)

acf(gas$gas)
QR <- 5:108
cos4 <- cos(QR*pi/2)
sin4 <- sin(QR*pi/2)
gas$last4 <- c(0,0,0,0,gas$log_gas[QR-4])
gas$last1 <- c(0,0,0,0,gas$log_gas[QR-1])

plot(gas$year, gas$log_gas)
plot(gas$quarter, gas$log_gas)

par(mfrow=c(3,1))
png(filename=paste(sep="", Dropbox, path, "1.3.png"))
plot(gas$pop, gas$log_gas, type="b")
plot(gas$gdp, gas$log_gas, type="b")
plot(log(gas$gdp), gas$log_gas, type="b")
dev.off()
par(mfrow=c(1,1))

gas$log_gdp = log(gas$gdp)

graph = data.frame(log_gas=gas$log_gas[QR])
graph$year = gas$year[QR]
graph$pop = gas$pop[QR]
graph$gdp = gas$gdp[QR]
graph$quarter = gas$quarter[QR]

graph2 = data.frame(log_gas=gas$log_gas[QR])
graph2$last4 = gas$last4[QR]
graph2$last1 = gas$last1[QR]
graph2$log_gdp = gas$log_gdp[QR]
graph2$log_pop = log(gas$pop[QR])

png(filename=paste(sep="", Dropbox, path, "1.1.png"))
plot(graph)
dev.off()
png(filename=paste(sep="", Dropbox, path, "1.4.png"))
plot(graph2)
dev.off()

png(filename=paste(sep="", Dropbox, path, "1.2.png"))
acf(gas$log_gas)
dev.off()

#  gas$Q1 = gas$quarter == 1
#  gas$Q2 = gas$quarter == 2
#  gas$Q3 = gas$quarter == 3
# gas$Q4 = gas$quarter == 4


newgas = data.frame(log_gas=gas$log_gas[QR])
newgas$year = gas$year[QR]
newgas$pop = gas$pop[QR]
#newgas$gdp = gas$gdp[QR]
newgas$quarter = gas$quarter[QR]
newgas$last4 = gas$last4[QR]
newgas$last1 = gas$last1[QR]
newgas$log_gdp = gas$log_gdp[QR]
# newgas$Q1 = gas$Q1[QR]
# newgas$Q2 = gas$Q2[QR]
# newgas$Q3 = gas$Q3[QR]


# reg0 = lm(log_gas[QR] ~ year[QR] + quarter[QR] + pop[QR] + gdp[QR] + sin4 + cos4 + last4[QR], data=gas)
# summary(reg0)

reg1 = lm(log_gas ~ year + quarter + pop + log_gdp + sin4 + cos4 + last4 + last1, data=newgas)
summary(reg1)

reg2 = lm(log_gas ~ year + quarter + pop + log_gdp + sin4 + last4 + last1, data=newgas)
summary(reg2)

reg3 = lm(log_gas ~ year + quarter + log_gdp + sin4 + last4, data=newgas)
summary(reg3)

reg4 = lm(log_gas ~ year + sin4 + last4, data=newgas)
summary(reg4)

reg5 = lm(log_gas ~ . + .^2 + sin4 + cos4, data=newgas)
summary(reg5)

reg6=lm(log_gas ~ sin4 + last1 + log_gdp + year*last4 + year*log_gdp + year*last1 + pop*last1 + pop*log_gdp + quarter * last1 + log_gdp*last4 + last1 * log_gdp, data=newgas)
summary(reg6)

reg7=lm(log_gas ~ sin4 + last1 + log_gdp + year*last4 + year*log_gdp + year*last1 + pop*last1 + pop*log_gdp + log_gdp*last4 + last1 * log_gdp - year - pop - quarter, data=newgas)
summary(reg7)

reg8=lm(log_gas ~ sin4 + last1 + log_gdp + year*log_gdp + year*last1 + pop*last1 + pop*log_gdp + log_gdp*last4 + last1 * log_gdp - year - pop - quarter - last4, data=newgas)
summary(reg8)


null <- lm(log_gas ~ 1, data=newgas)
full <- lm(log_gas ~ . + .^2 + sin4 + cos4, data=newgas)

fwd <- step(null, scope=formula(full), direction="forward", k=log(length(newgas$log_gas)))
summary(fwd)

model.BIC <- c(reg1=extractAIC(reg1, k=log(length(QR)))[2],
               reg2=extractAIC(reg2, k=log(length(QR)))[2],
               reg3=extractAIC(reg3, k=log(length(QR)))[2],
               reg4=extractAIC(reg4, k=log(length(QR)))[2],
               reg5=extractAIC(reg5, k=log(length(QR)))[2],
               reg6=extractAIC(reg6, k=log(length(QR)))[2],
               reg7=extractAIC(reg7, k=log(length(QR)))[2],
               reg8=extractAIC(reg8, k=log(length(QR)))[2],
               reg9=extractAIC(fwd, k=log(length(QR)))[2])
model.BIC

eBIC <- exp(-0.5*(model.BIC-min(model.BIC)))
round(probs <- eBIC/sum(eBIC), 5)

index <- sample(1:length(newgas$log_gas),length(newgas$log_gas)/10)
index
train <- newgas[-index,] 
train$sin4 <- sin4[-index]
test <- newgas[index,]
test$sin4 <- sin4[index]
train
test

reg3 = lm(log_gas ~ year + quarter + log_gdp + sin4 + last4, data=train)
summary(reg3)

reg4 = lm(log_gas ~ year + log_gdp + sin4 + last4, data=train)
summary(reg4)

reg8=lm(log_gas ~ sin4 + last1 + log_gdp + year*log_gdp + year*last1 + pop*last1 + pop*log_gdp + log_gdp*last4 + last1 * log_gdp - year - pop - quarter - last4, data=train)
summary(reg8)

reg9 = lm(log_gas ~ last4 + pop + sin4, data=train)
summary(reg9)


error3 <- predict(reg3, newdata=test)-test$log_gas
error4 <- predict(reg4, newdata=test)-test$log_gas
error8 <- predict(reg8, newdata=test)-test$log_gas
error9 <- predict(reg9, newdata=test)-test$log_gas

mean(error3^2)
mean(error4^2)
mean(error8^2)
mean(error9^2)


#1.2
png(filename=paste(sep="", Dropbox, path, "1.5.png"))
plot(rstudent(fwd))
dev.off()

png(filename=paste(sep="", Dropbox, path, "1.6.png"))
plot(rstudent(fwd), type="l")
dev.off()

png(filename=paste(sep="", Dropbox, path, "1.7.png"))
plot(newgas$year, rstudent(fwd))
dev.off()

png(filename=paste(sep="", Dropbox, path, "1.8.png"))
hist(rstudent(fwd))
dev.off()

png(filename=paste(sep="", Dropbox, path, "1.9.png"))
qqnorm(rstudent(fwd))
abline(0,1)
dev.off()

png(filename=paste(sep="", Dropbox, path, "1.10.png"))
acf(rstudent(fwd))
dev.off()

