#### ******* German Credit Data ******* ####

## read data and create some `interesting' variables
credit <- read.csv("credit.csv")

## re-level the credit history and checking account status
credit$history = factor(credit$history, levels=c("A30","A31","A32","A33","A34"))
levels(credit$history) = c("good","good","poor","poor","terrible")
## a few others
credit$foreign <- factor(credit$foreign, levels=c("A201","A202"), labels=c("foreign","german"))
credit$rent <- factor(credit$housing=="A151")
credit$purpose <- factor(credit$purpose, levels=c("A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A410"))
levels(credit$purpose) <- c("newcar","usedcar",rep("goods/repair",4),"edu",NA,"edu","biz","biz")

## plot a couple of dimensions
par(mfrow=c(1,2))
plot(factor(Default) ~ history, data=credit, col=c(8,2), ylab="Default") ## surprise!
plot(factor(Default) ~ purpose, data=credit, col=c(8,2), ylab="")
## the dangers of choice-based sampling!  

## build a design matrix 
library(gamlr)
source("naref.R")
credit <- naref(credit)
credx <- sparse.model.matrix( ~ (duration + amount +
                                installment + age + history +
                                purpose + foreign + rent)^2, 
                    data=credit)
default <- credit$Default

## lasso
credscore <- cv.gamlr(credx, default, family="binomial")

par(mfrow=c(1,2))
plot(credscore$gamlr)
plot(credscore)

sum(coef(credscore)!=0) # 1se
sum(coef(credscore, s="min")!=0) # min
sum(coef(credscore$gamlr)!=0) # AICc

## classification!

## What are the underlying default probabilities
## In sample probability estimates
pred <- predict(credscore, credx, type="response")
pred <- drop(pred) # remove the sparse Matrix formatting
data.frame(default,pred)[1:20,]

## what are our misclassification rates?
rule <- 1/5 # move this around to see how these change

sum( (pred>rule)[default==0] )/sum(pred>rule) ## false positive rate
sum( (pred<rule)[default==1] )/sum(pred<rule) ## false negative rate

sum( (pred>rule)[default==1] )/sum(default==1) ## sensitivity
sum( (pred<rule)[default==0] )/sum(default==0) ## specificity

## roc curve and fitted distributions
source("roc.R")
roc(p=pred, y=default, bty="n")
## our 1/5 rule cutoff
points(x= 1-mean((pred<.2)[default==0]), 
	y=mean((pred>.2)[default==1]), 
	cex=1.5, pch=20, col='red') 
## a standard `max prob' (p=.5) rule
points(x= 1-mean((pred<.5)[default==0]), 
	y=mean((pred>.5)[default==1]), 
	cex=1.5, pch=20, col='blue') 
legend("topleft",fill=c("red","blue"),
	legend=c("p=1/5","p=1/2"),bty="n",title="cutoff")













