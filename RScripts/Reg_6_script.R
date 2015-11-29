Dropbox = "C:\\users/Jake/Dropbox"
#Dropbox = "~/Dropbox"
path= "/Booth/Fall 2013/Regression/Homeworks/HW6/"
setwd(paste(Dropbox,"/Booth/Fall 2013/Regression/Data", sep=""))
smsa = read.csv("smsa.csv")
news = read.csv("newspaper.csv")

#1.1
paste(sep="", Dropbox, path, "1.1.png")
png(filename=paste(sep="", Dropbox, path, "1.1.png"))
summary(smsa)
par(mfrow=c(3,3))
for(i in 2:16)
{
  if(colnames(smsa)[i] == "Mortality") { next }
  if(i == 12)
  {
    dev.off()
    png(filename=paste(sep="", Dropbox, path, "1.2.png"))
    par(mfrow=c(2,3))
  }
  plot(smsa[,i],smsa$Mortality, xlab=paste(colnames(smsa)[i]), ylab="Mortality") 
}
dev.off()
#transform NOxPot, Pop, PHouse, Income, HCPot, WC, PopD, RelHum, S02Pot to log()

transformed_smsa = data.frame(Mortality = smsa$Mortality)

png(filename=paste(sep="", Dropbox, path, "1.3.png"))
par(mfrow=c(3,3))
for(i in 2:16)
{
  if(colnames(smsa)[i] == "Mortality") { next }
  if(colnames(smsa)[i] == "NOxPot" || 
       colnames(smsa)[i] == "Pop" || 
       colnames(smsa)[i] == "PHouse" || 
       colnames(smsa)[i] == "Income" || 
       colnames(smsa)[i] == "HCPot" || 
       colnames(smsa)[i] == "WC" || 
       colnames(smsa)[i] == "PopD" || 
       colnames(smsa)[i] == "S02Pot" || 
       colnames(smsa)[i] == "RelHum") 
  { 
    transformed_smsa[,eval(paste(sep="", colnames(smsa)[i], "_log"))] <- log(smsa[,i])
    plot(log(smsa[,i]),smsa$Mortality, xlab=paste("log(", colnames(smsa)[i], ")"), ylab="Mortality") 
  }
  else
    {
      transformed_smsa[,eval(colnames(smsa)[i])] <- smsa[,i]
      #plot(smsa[,i],smsa$Mortality, xlab=paste(colnames(smsa)[i]), ylab="Mortality") 
    }
}
dev.off()

#1.2
summary(transformed_smsa)
#transform NOxPlot, Pop, PHouse, Income, HCPot, WC, PopD, RelHum to log()
null <- lm(Mortality ~ 1, data=transformed_smsa)
full <- lm(Mortality ~ ., data=transformed_smsa)

summary(full)

#1.3
reg1<- lm(Mortality ~ JanT + Rain + NonWht + NOxPot_log, data=transformed_smsa)
summary(reg1)
anova(reg1, full)

reg2<- lm(Mortality ~ JanT + Rain + NonWht + NOxPot_log + Edu, data=transformed_smsa)
summary(reg2)

reg3<- lm(Mortality ~ JanT + Rain + NonWht + NOxPot_log + WC_log, data=transformed_smsa)
summary(reg3)

reg4<- lm(Mortality ~ JanT + Rain + NonWht + NOxPot_log + HCPot_log, data=transformed_smsa)
summary(reg4)

anova(reg2,reg1)
anova(reg3,reg1)
anova(reg4,reg1)

reg5<- lm(Mortality ~ JanT + Rain + NonWht + NOxPot_log + Edu + WC_log, data=transformed_smsa)
summary(reg5)

reg6<- lm(Mortality ~ JanT + Rain + NonWht + NOxPot_log + Edu + HCPot_log, data=transformed_smsa)
summary(reg6)

anova(reg5,reg2)
anova(reg6,reg2)

summary(reg2)

#1.4
fwd <- step(null, scope=formula(full), direction="forward", k=log(length(transformed_smsa$Mortality)))
summary(fwd)

anova(reg2, fwd)

plot(transformed_smsa$S02Pot_log, transformed_smsa$NOxPot_log)

BIC <- c(reg1=extractAIC(reg1, k=log(length(transformed_smsa$Mortality)))[2],
         reg2=extractAIC(reg2, k=log(length(transformed_smsa$Mortality)))[2])
BIC <-  c(BIC, reg3 = extractAIC(fwd, k=log(length(transformed_smsa$Mortality)))[2])
BIC

eBIC <- exp(-0.5*(BIC-min(BIC)))
round(probs <- eBIC/sum(eBIC), 5)


#1.5
#TODO: Use BIC?
null <- lm(Mortality ~ 1, data=transformed_smsa)
full <- lm(Mortality ~ . + .^2, data=transformed_smsa)
fwd <- step(null, scope=formula(full), direction="forward", k=log(length(transformed_smsa$Mortality)))

summary(fwd)
extractAIC(fwd, k=log(length(transformed_smsa$Mortality)))
extractAIC(reg2,  k=log(length(transformed_smsa$Mortality)))

reg7 <- lm(Mortality ~ JanT + Rain + S02Pot_log*NonWht, data=transformed_smsa)
summary(reg7)
extractAIC(reg7,  k=log(length(transformed_smsa$Mortality)))

BIC <- c(reg1=extractAIC(reg2, k=log(length(transformed_smsa$Mortality)))[2],
         reg2=extractAIC(fwd, k=log(length(transformed_smsa$Mortality)))[2],
         reg3=extractAIC(reg7, k=log(length(transformed_smsa$Mortality)))[2])
BIC

eBIC <- exp(-0.5*(BIC-min(BIC)))
round(probs <- eBIC/sum(eBIC), 5)

#1.6
#TODO: reference notes for how to use sapply()

index <- sample(1:length(transformed_smsa$Mortality),length(transformed_smsa$Mortality)/10)
index
train <- transformed_smsa[-index,] 
test <- transformed_smsa[index,]
train
test
reg1 = lm(Mortality ~ JanT + Rain + NonWht + NOxPot_log + Edu, data=train)
reg2 = lm(Mortality ~ JanT + Rain + S02Pot_log * NonWht + Edu, data=train)
reg3 = lm(Mortality ~ JanT + Rain + S02Pot_log * NonWht, data=train)

summary(reg1)
summary(reg2)
summary(reg3)

error1 <- predict(reg1, newdata=test)-test$Mortality
error2 <- predict(reg2, newdata=test)-test$Mortality
error3 <- predict(reg3, newdata=test)-test$Mortality

mean(error1^2)
mean(error2^2)
mean(error3^2)

#2.1
#revisiting our models from HW 3 and 4:
norm.log <- function(model,y) {
  t.y <- log(y)
  s <- sqrt(sum(residuals(model)^2)/length(residuals(model)))
  LL <- sum(log(dnorm(t.y,mean=predict(model), sd=s)*1/y)) 
  LL 
} 
norm.test <- function(model,y) {
  s <- sqrt(sum(residuals(model)^2)/length(residuals(model)))
  LL <- sum(log(dnorm(y,mean=predict(model), sd=s))) 
  LL 
} 

#HW3
news_lm = lm(Sunday ~ daily, news)
#HW4
log_sunday = log(news$Sunday)
log_daily = log(news$daily)
log_news_lm = lm(log_sunday ~ log_daily)

plot(log_daily, log_sunday)
plot(news$daily, news$Sunday)

model.BIC <- c(reg1=extractAIC(news_lm, k=log(length(news$Sunday)))[2],
               reg2=extractAIC(log_news_lm, k=log(length(news$Sunday)))[2])
model.BIC

eBIC <- exp(-0.5*(model.BIC-min(model.BIC)))
round(probs <- eBIC/sum(eBIC), 5)

model.BIC <- c(-2*norm.test(news_lm,news$Sunday)+log(length(news$Sunday))*3, 
               -2*norm.log(log_news_lm,news$Sunday)+log(length(news$Sunday))*2)
model.BIC
eBIC <- exp(-0.5*(model.BIC-min(model.BIC)))
round(probs <- eBIC/sum(eBIC), 5)



#TODO: compare with BIC