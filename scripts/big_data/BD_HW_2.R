Note:  Log(p/(1-p)) = B0+B1X + ....
Log odds is a liear model, so a B can be seen as a multiplier on odds
if B = -.3699, then exp(-.3699) = 0.69 = the odds decrease by 30% (multiplied by 0.7)
Deviance for the null model is deviance with B=0

SC <- read.csv("semiconductor.csv")

#Mac/Unix
Project = "~/github"

path= "/projects/data/"
setwd(paste(Project, path, sep=""))
getwd()

##### ******** Mortgage and Home Sales Data ******** #####

## Read in the data
homes <- read.csv("homes2004.csv")
library(xtable)
source("../RScripts/fdr.R")
source("../RScripts/deviance.R")
homes <- homes[homes$VALUE > 3000,]
homes$STATE <- relevel(homes$STATE, "IL")
homes$STATE


plot(homes$BATHS)

# conditional vs marginal value
# par(mfrow=c(1,1)) # 1 row, 1 columns of plots 
hist((homes$VALUE), col="grey", xlab="home value", main="", 
    breaks=seq(0,2500000,l=50))
f_price = factor(homes$VALUE)
summary(f_price)
plot(VALUE ~ factor(BATHS), 
     col=rainbow(8), data=homes[homes$BATHS<8,],
     xlab="number of bathrooms", ylab="home value")

# create a var for downpayment being greater than 20%
homes$gt20dwn <- 
  factor(0.2<(homes$LPRICE-homes$AMMORT)/homes$LPRICE)

# some quick plots.  Do more to build your intuition!
par(mfrow=c(1,1)) 
plot(VALUE ~ STATE, data=homes, 
     col=rainbow(nlevels(homes$STATE)), 
     ylim=c(0,10^6), cex.axis=.65)
plot(gt20dwn ~ FRSTHO, data=homes, 
     col=c(1,3), xlab="Buyer's First Home?", 
     ylab="Greater than 20% down")

names(homes)

## code hints 

## Q2 
# regress log(VALUE) on everything except AMMORT and LPRICE 
homes$VALUE
pricey <- glm(log(VALUE) ~ .-AMMORT-LPRICE, data=homes)
# extract pvalues
summary(pricey)$coef[-1,4]
pvals <- summary(pricey)$coef[-1,4]
summary(pricey)$coef[-1,4]
summary(pricey)
q=0.1
pvals[order(pvals)]
homes[homes$STATE=="IN",]
a = fdr_cut(pvals, q, TRUE)
a
pvals
names(pvals[pvals > a])
plot(homes$STATE, homes$VALUE)
homes$STATE
summary(pricey)
pricey2 = glm(as.formula(paste("log(VALUE) ~ .-AMMORT-LPRICE-",
                      paste(sapply(names(pvals)[pvals>a], strsplit, split="Y$"),collapse="-"),
                      sep="")), data=homes)

names(summary(pricey2)$coef[-1,4])
1-summary(pricey)$deviance/summary(pricey)$null.deviance
1-summary(pricey2)$deviance/summary(pricey2)$null.deviance

0.3053729

## Q3: 
# - don't forget family="binomial"!
# - use +A*B in forumula to add A interacting with B

## Q4
# this is your training sample
gt100 <- which(homes$VALUE>1e5)
# ybar and null deviance


ybar <- mean(homes$gt20dwn[-gt100]==TRUE)
ybar
as.logical(homes$gt20dwn[-gt100])
D0 <- deviance(y=homes$gt20dwn[-gt100], pred=ybar, family="binomial")
D0
D0 <- deviance(y=homes$gt20dwn[-gt100], pred=ybar, family="binomial")


1-summary(pricey)$deviance/summary(pricey)$null.deviance

xnumeric <- model.matrix(log(VALUE) ~ .-AMMORT-LPRICE, data=homes)
names(as.data.frame(xnumeric))
fit <- glm(log(homes$VALUE) ~ ., data=as.data.frame(xnumeric))
fit2 <- glm(log(homes$VALUE) ~ ., data=as.data.frame(xnumeric[,pvals<a]))
summary(pvals>=a)
summary(fit2)
names(summary(fit2)$coef[-1,4])
1-summary(fit2)$deviance/summary(fit2)$null.deviance


xnumeric <- model.matrix(log(VALUE) ~ .-AMMORT-LPRICE, data=homes)
xnumeric[,'(Intercept)']
xnumeric[,pvals]

colnames(xnumeric[,-1])

xnumeric$'(Intercept)'
xnumeric[, pvals <=a]
xnumericframe <- as.data.frame(xnumeric)
#(Intercept) will show up in the frame:
names(xnumericframe)
#Remove the intercept term
xnumericframe$'(Intercept)' <- NULL
#Look, it's gone!
names(xnumericframe)
#Now xnumericframe matches with pvals
names(pvals)
#Now this matches with the nonsig array created earlier in the code
names(xnumericframe[,pvals<a])
#MAGIC!!!
fit2 <- glm(log(homes$VALUE) ~ ., data=as.data.frame(xnumericframe[,pvals<=a]))
#fit2 matches pricey2 calculated earlier in the code
summary(fit2)
summary(pricey2)

