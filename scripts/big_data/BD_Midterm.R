Project = "~/github"

path= "/projects/data/"
setwd(paste(Project, path, sep=""))

### if you don't yet have data.table, run install.packages("data.table")
#install.packages("data.table")
library(data.table)
library(gamlr)
source("../RScripts/naref.R")
source("../RScripts/fdr.R")
source("../RScripts/Cost.R")
source("../RScripts/roc.R")
source("../RScripts/coef_form.R")
source("../RScripts/kurtosis.R")

biketab <- fread("bikeshare.csv")
# tell R which are factors
biketab[, c("dteday", "mnth","season","weekday","hr","weathersit") := list(
  factor(dteday), factor(mnth), factor(season), 
  factor(weekday), factor(hr), factor(weathersit))]

####### Q1: outliers and FDR
## the next command calculates total cnt by day, 
# also keeping track of the corresponding yr and mnth id.
daytots <- biketab[, list(total=sum(cnt)), by=c("dteday","yr","mnth")]
row.names(daytots) <- daytots$dteday
# simple regression
daylm <- glm(total ~ yr*mnth, data=daytots)

#1.1
#What are the in-sample SSE and R2 for this regression?
summary(daylm)
daylm$null.deviance
daylm$deviance

sum(daylm$residuals^2)
1-daylm$deviance/daylm$null.deviance
#LOOK AT HW4 SOLUTIONS FOR R2 TRICKS! (pretty sure the above line is actually doing this)
#There's some magic here with in sample vs. out of sample

#1.2
#Write out the mathematical formula for daylm and describe it in words. Make sure to describe
#the probability model that is implied by the deviance we’ve minimized. Do you have any
#criticisms of this model?
summary(daylm)
names(daylm$coef)
coef_formula(daylm$coef)

#1.3
# A standardized residual for response y and prediction yˆ is ri = (yi − yˆi)/σˆ, where σˆ is the
# estimated standard deviation of residuals y−yˆ. Calculate the standardized residuals for daylm.
sd(daylm$residuals)
std_resids = daylm$residuals / sd(daylm$residuals)
sqrt(summary(daylm)$dispersion)
sd(daylm$residuals)
png(filename=paste(sep="", Project, path, "BD_midterm.1.3.png"))
hist(std_resids, col="grey", xlab="standardized residual", main="standardized residual distribution", 
     breaks=seq(-8,5,l=50))
dev.off()

# Now, we’ll call the outlier p-value 2 × p(Z < −|ri|) where Z ∼ N(0, 1). In R, this is
# 2*pnorm(-abs(std_resids)). Calculate these p-values. Describe what null hypothesis
# distribution they correspond to and why small values indicate a possible outlier day.
outlier_p = 2*pnorm(-abs(std_resids))
#null hypothesis - something along the lines of "this observation is caused by the variables we see in our model"
names(outlier_p[outlier_p <= .05])
sort(names(outlier_p[outlier_p <= .05]))

# 1.4
# What is the p-value rejection region associated with a 5% False Discovery Rate here?
# Which observations (days) are in this rejection region? Do you have any explanation for them?
#summary(daylm)$coef[-1,4]
pvals <- outlier_p 
q=0.05
pvals[order(pvals)]
png(filename=paste(sep="", Project, path, "BD_midterm.1.4.png"))
a = fdr_cut(pvals, q, TRUE)
dev.off()
a
names(pvals[pvals <= a])
#http://en.wikipedia.org/wiki/Hurricane_Sandy

# 1.5
# Plot the p-value distribution. What does it tell you about the assumptions of the probability
# model we used for our regression?
png(filename=paste(sep="", Project, path, "BD_midterm.1.5.png"))
hist(pvals, col="grey", xlab="p-value", main="outlier p-value distribution", 
     breaks=seq(0,1.0,l=25))
dev.off()

kurt(std_resids) 
hist(std_resids)
length(std_resids)
std_resids_prime = std_resids[std_resids > -4]
length(std_resids_prime)
kurt(std_resids_prime)
outlier_p_prime1 = 2*pnorm(-abs(std_resids_prime))
?pnorm
hist(outlier_p_prime1, col="grey", xlab="p-value", main="outlier p-value distribution", 
     breaks=seq(0,1.0,l=25))

#broken assumptions about uniform distribution of p!
#...check piazza to see if this assumption is necessary

#...need to think more about this question

# 2 Lasso Linear Regression and Model Selection
# For this question, consider the cv.gamlr object I’ve fit as fitlin.
#### Q2: lasso regression
mmbike <- sparse.model.matrix(
  cnt ~ . + yr*mnth + hr*notbizday, 
  data=naref(biketab))[,-1]

immbike <- sparse.model.matrix(
  cnt ~ . + temp*hum + yr*mnth + hr*notbizday, 
  data=naref(biketab))[,-1]

sparsedays <- sparse.model.matrix(
  cnt ~ dteday, 
  data=naref(biketab))[,-1]

sparsehours <- sparse.model.matrix(
  cnt ~ hr, 
  data=naref(biketab))[,-1]


y <- log(biketab$cnt)
## note, I need lambda.min.ratio=1e-4 because otherwise we don't get a path
## out to complex enough models (i.e. cv err is still decreasing at termination)
fitlin <- cv.gamlr( mmbike, y, lmr=1e-4, verb=TRUE )

# 2.1
# What is our response variable? Describe the columns of our model matrix. How has this model
# addressed the ‘outlier detection’ of question 1?
plot(y)
summary(biketab$cnt)
summary(y)
coef(fitlin)
colnames(mmbike)

#uses terms for each day! that's what the . is doign, including the day factor

# 2.2
# Describe the criteria used to choose models under select="1se" and select="min"
# rules. What are estimated out-of-sample R2
# for models fit using these λ?
summary(fitlin)
fitlin
summary(fitlin$gamlr)

png(filename=paste(sep="", Project, path, "BD_midterm.2.2.png"))
plot(fitlin, main="LASSO model min AICc")
abline(v=log(fitlin$lambda.min), col="orange", lty=3)
abline(v=log(fitlin$lambda.1se), col="green", lty=3)
legend("topleft", bty="n",
       fill=c("orange","green"),legend=c("min","1se"))
dev.off()

plot(fitlin$gamlr)
coef(fitlin, select="1se")
length(coef(fitlin, select="1se")[coef(fitlin, select="1se") !=0])
coef(fitlin, select="min")
length(coef(fitlin, select="min")[coef(fitlin, select="min") !=0])
length(coef(fitlin, select="1se")[coef(fitlin, select="1se") !=0])
#lambda
#log(fitlin$gamlr$lambda[which.min(AICc(fitlin$gamlr))])
log(fitlin$lambda.1se)
log(fitlin$lambda.min)
fitlin$seg.min
fitlin$seg.1se

#OOS R2
1-min(fitlin$cvm)/fitlin$cvm[1]
1-min(fitlin$cvm[fitlin$seg.min])/fitlin$cvm[1]
1-min(fitlin$cvm[fitlin$seg.1se])/fitlin$cvm[1]

# 2.3
# Compare AICc, AIC, and BIC selection to each other and to the CV rules.

#discuss differences between them, then show what happens when you run non-cv gamlr
refitlin <- gamlr( mmbike, y, lmr=1e-4, verb=TRUE, family="gaussian" )
AICcseg <- which.min(AICc(refitlin))
summary(refitlin)[AICcseg,]
Baicc <- coef(refitlin)
length(Baicc[Baicc != 0])
#Baicc[Baicc != 0]

AICseg <- which.min(AIC(refitlin))
summary(refitlin)[AICseg,]
Baic <- coef(refitlin)
length(Baic[Baic != 0])
#Baic[Baic != 0]

BICseg <- which.min(BIC(refitlin))
summary(refitlin)[BICseg,]
Bbic <- coef(refitlin)
length(Bbic[Bbic != 0])
#Bbic[Bbic != 0]

summary(fitlin)[fitlin$seg.min,]
summary(fitlin)[fitlin$seg.1se,]
ll <- log(refitlin$lambda) ## the sequence of lambdas
n <- nrow(mmbike)

length(ll)
length(AIC(refitlin)/n)
png(filename=paste(sep="", Project, path, "BD_midterm.2.3.png"))
plot(ll, AIC(refitlin)/n, 
     xlab="log lambda", ylab="IC/n", pch=21, bg="orange")
points(ll, BIC(refitlin)/n, pch=21, bg="green")
points(ll, AICc(refitlin)/n, pch=21, bg="black")

abline(v=ll[which.min(AICc(refitlin))], col="black", lty=2)
abline(v=ll[which.min(AIC(refitlin))], col="orange", lty=2)
abline(v=ll[which.min(BIC(refitlin))], col="green", lty=2)
abline(v=log(fitlin$lambda.min), col="blue", lty=2)
abline(v=log(fitlin$lambda.1se), col="purple", lty=2)
legend("topleft", bty="n", lwd=1, 
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV.min","CV.1se"))

dev.off()

png(filename=paste(sep="", Project, path, "BD_midterm.2.3.2.png"))
plot(refitlin, col="grey")
abline(v=ll[which.min(AICc(refitlin))], col="black", lty=2)
abline(v=ll[which.min(AIC(refitlin))], col="orange", lty=2)
abline(v=ll[which.min(BIC(refitlin))], col="green", lty=2)
abline(v=log(fitlin$lambda.min), col="blue", lty=2)
abline(v=log(fitlin$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, 
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV.min","CV.1se"))
dev.off()

# 2.4
# Print the top three dteday effects by absolute value under your preferred selection rule, and
# describe the implied effect on cnt. Can you explain any of these?

Bcv1se <- coef(fitlin)[colnames(sparsedays),]
sort(Bcv1se)
tail((Bcv1se[order(abs(Bcv1se))]))

names(tail(abs(Bcv1se[order(abs(Bcv1se))])))

# 2.5
# Bootstrap to get an estimate of the sampling distribution for AICc and BIC selected lambdas.
# Compare these distributions and describe why they look like they do.

#bootstrap means select random draws, redo our non-CV gamlr, and see what the min aicc lambda value is
n <- nrow(mmbike)
aiccLambdas <- c() 
bicLambdas <- c() 
for(b in 1:200){
  ## create a matrix of resampled indices
  ib <- sample(1:n, n, replace=TRUE)
  ## create the resampled data
  xb <- mmbike[ib,]
  yb <- y[ib]
  ## run the lasso regression
  
  fitb <- gamlr(xb,yb, lmr=1e-4, family="gaussian")
  BICseg <- which.min(BIC(fitb))
  AICcseg <- which.min(AICc(fitb))
  bicLambdas <- c(bicLambdas, log(summary(fitb)[BICseg,]$lambda))
  aiccLambdas <- c(aiccLambdas, log(summary(fitb)[AICcseg,]$lambda))
  
  print(b)
}

summary(aiccLambdas) 
png(filename=paste(sep="", Project, path, "BD_midterm.2.4.1.png"))
hist(aiccLambdas, xlab="log(lambda)", main="Bootstrapped AICc log lambda distribution") 
dev.off()
summary(bicLambdas) 
png(filename=paste(sep="", Project, path, "BD_midterm.2.4.2.png"))
hist(bicLambdas, xlab="log(lambda)", main="Bootstrapped BIC log lambda distribution")
dev.off()

png(filename=paste(sep="", Project, path, "BD_midterm.2.4.3.png"))
boxplot(aiccLambdas, bicLambdas, main="AICc vs. BIC lambda distribution", names=c("AICc", "BIC"))
dev.off()
#...not sure 'why they look like they do'... I guess normally distributed is expected, and so is BIC < AICc


##### Q3: logistic regression 
overload <- biketab$cnt > 500
set.seed(5807) 
n <- length(overload)
test <- sample(1:n, 3000)

# 3 Logistic Regression and Classification
# The managers of Capital Bikeshare have found that the system works smoothly until more than
# 500 bikes are rented in any one hour. At that point, it becomes necessary to insert extra bikes
# into the system and move them across stations to balance loads.
# 3.1
# Define the binary outcome variable overload that is one if cnt > 500, zero otherwise.
# Fit and plot the lasso path for regression of overload onto the same model matrix used in
# Question 2 (no need for cross validation).
ofitlin <- gamlr( mmbike, overload, lmr=1e-4, verb=TRUE, family="binomial")
png(filename=paste(sep="", Project, path, "BD_midterm.3.1.png"))
plot(ofitlin, main="Logistic regression on overload (cnt > 500)")
dev.off()
nrow(mmbike)
length(overload)
summary(ofitlin)[which.min(AICc(ofitlin)),]

# 3.2
# Summarize how hour-of-day effects the probability of an overload during business days. Consider
# a single hour with a strong effect and compare this to its effect in the regression of Q2.

summary(overload[(biketab$hr == "17" | (biketab$hr == "18") | (biketab$hr == "19"))  & !biketab$notbizday])
summary(overload)
biketab$hr
overload
coef(ofitlin)
Bhrs <- coef(ofitlin)[colnames(sparsehours),]
sort(Bhrs)
tail(abs(Bhrs[order(abs(Bhrs))]))
names(tail(abs(Bhrs[order(abs(Bhrs))])))

Bhrscv1se <- coef(fitlin)[colnames(sparsehours),]
sort(Bhrscv1se)
tail(abs(Bhrscv1se[order(abs(Bhrscv1se))]))
((Bhrscv1se[order(abs(Bhrscv1se))]))
names(tail(abs(Bhrscv1se[order(abs(Bhrscv1se))])))
exp(Bhrscv1se["hr17"])
mean(biketab$cnt)

#I think we can say the difference here is that in the original regression, we're predicting log(cnt)
#so a 1-unit increase in hr17 leads to ~2.7 increase in expectation of count over the baseline.  In the second regression,
#we're simplly predicting that log(odds) increases by 16x over the baseline

#This might make sense if the baseline number for these days is ~200-300.  Then a 2.7 increase is probably
#~ a 16x increase in odds of being over 500.

# 3.3
# Suppose that it costs you $200/hr in overtime pay if you have an overload (cnt > 500) with
# your usual number of staff. Staffing an extra driver to move the bikes costs only $100/hr and
# means you don’t have to pay any overtime. At what probability for overload > 0 will you
# want to staff an extra driver?
costs = matrix(c(100,200,100,0), nrow = 2, ncol = 2)
costs
#cost of predicting a:
#cost_p_a = p(a) * cost(a | p_a) + p(b) * cost(b | p_a) ...
#cost of predicting !a:
#cost_p_!a = p(a) * cost(a | p_!a) + p(b) * cost(b | p_!a) ...

# cost_p_a = p * 100 + (1-p) * 100 
# cost_p_!a == (1-p) * 200 + p * 0
# cost_p_a == cost_p_!a when:
#   p * 100 + 100 - p * 100 == 200 - 200 * p
#   100 == 200 * p
#   p == 0.5


rule = 0.5
#this only holds if all mispredictions of a have the same cost- in this case it holds, but the general
#case is more complex, and we need a more complex decision rule.  See appendix for Cost.R, which takes an array of 
#costs and an array of predictions and returns whether or not you should predict a == TRUE

# 3.4
# Plot and describe the ROC curve for your AICc-optimal regression from 3.1. What is the
# sensitivity and specificity of your rule from 3.3 if applied with this regression?
pred <- predict(ofitlin, mmbike, type="response")
png(filename=paste(sep="", Project, path, "BD_midterm.3.4.png"))
roc(p=pred, y=overload, bty="n")
points(x= 1-mean((pred<rule)[overload==0]), 
       y=mean((pred>rule)[overload==1]), 
       cex=1.5, pch=20, col='red') 
dev.off()

Q <- pred >= rule
specificity <- mean(!Q[!overload])
sensitivity <- mean(Q[overload])
specificity
sensitivity
Q2 <- c()
for(i in 1:length(pred)){
  Q2 <- c(Q2,predict_a(costs, c(pred[i], 1-pred[i])))
}
summary(Q2)
specificity2 <- mean(!Q2[!overload])
sensitivity2 <- mean(Q2[overload])
specificity2
sensitivity2


# 3.5
# Now, take the test sample and
# • fit the regression path excluding this sample (e.g., on mmbike[-test,]).
# • use the AICc-optimal model from this path to predict for the test set.
# • plot the ‘out-of-sample’ ROC curve for these predictions.
# Compare this curve to your ROC curve from 3.4 and describe what they imply about the quality
# of AICc selection for this regression.
# 

testfitlin <- gamlr( mmbike[-test,], overload[-test], lmr=1e-4, verb=TRUE, family="binomial")
plot(testfitlin)
predtest <- predict(testfitlin, mmbike[test,], type="response")
png(filename=paste(sep="", Project, path, "BD_midterm.3.5.png"), width = 960, height = 480, units = "px")
par(mfrow=c(1,2))
roc(p=predtest, y=overload[test], bty="n", main="Test OOS ROC curve")
roc(p=pred, y=overload, bty="n", main="Full data ROC curve")
dev.off()
par(mfrow=c(1,1))

Q3 <- predtest >= rule
specificity <- mean(!Q3[!overload[test]])
sensitivity <- mean(Q3[overload[test]])
specificity
sensitivity
#we are good at correctly predicting both true and false states
#BUT we are definitely worse at doing this when we have to predict out of sample.
# This doesn't imply AICc is bad (we may well have picked the model that optimally predicts OOS r2),
# just that we can never really do better with less data to train on.  Which we already knew.

##### Q4: treatment effects
coef(fitlin)["hum",]

# a new model matrix excluding humidity
x <- mmbike[, -grep("hum",colnames(mmbike))]
hum <- mmbike[, "hum"] # pull humidity out as a separate vector

# 4 Treatment Effects Estimation
# For this question, we’ll revisit the regression model of Q2 with the goal to infer the independent
# effect of humidity.
# 4.1
# Based on ‘naive’ fitlin estimate, what is the effect of an extra standard deviation of humidity
# (hum increasing by one unit) on the count of bikes rented?
coef(fitlin)["hum",]
exp(coef(fitlin)["hum",])
1- exp(coef(fitlin)["hum",])

# 4.2
# Predict humidity from a model matrix x that includes all our covariates except humidity. Describe
# how close the predicted values are to true humidity, and how this is relevant for our goal
# in this question.
treat <- gamlr(x, hum, lmr=1e-04, verb=TRUE, family="gaussian")
summary(treat)
summary(treat)[which.min(AICc(treat)),]
r2 = summary(treat)[which.min(AICc(treat)),]["r2"]
r2
plot(treat)
hhat = predict(treat, x, type="response")
summary(hhat)
png(filename=paste(sep="", Project, path, "BD_midterm.4.2.png"), width = 960, height = 480, units = "px")
par(mfrow=c(1,2))
plot(hhat)
plot(hhat,hum,bty="n",pch=21,bg=8) 
dev.off()
par(mfrow=c(1,1))

#we're quite good at predicting humidity from other things


# 4.3
# Obtain an estimate for the treatment effect of humidity on bike rentals, and describe this estimate
# in plain words. How does it compare to your estimate in 4.1?
causal <- gamlr(cBind(hum,hhat,x), y, lmr=1e-04, verb=TRUE, family="gaussian", free=2)
coef(causal)["hum",] # AICc says degree has a causal effect.
coef(causal)
#whoah... surprisingly still almost as impactful as before!


# 4.4
# Extend the fitlin model from 4.1 (and Q2) to allow for the effect of humidity to depend
# upon the temperature. Describe the resulting relationship between humidity and ride count.
ifitlin <- cv.gamlr( immbike, y, lmr=1e-4, verb=TRUE )
coef(ifitlin)[grep("hum",rownames(coef(ifitlin))),]
cor(biketab$hum, as.numeric(biketab$weather))

#if its hot, then humidity increases ride count.  If its cold, humidity decreased ride count.  
#That's kind of weird.

# 4.5
# Finally, extend your causal model from 4.3 to measure the temperature-dependent effect of
# humidity. Describe the results and compare to 4.4. Hint: you need to control for the interaction
# between temperature and the portion of humidity that is predictable from the controls.

# predict humidity from controls
# control for temp * hhat + hhat 
# free params should be hhat, temp:hhat

ix <- immbike[, -grep("hum",colnames(immbike))]
itemphum <- immbike[, "temp:hum"] # pull temp:humidity out as a separate vector
ihum <- immbike[, "hum"] # pull humidity out as a separate vector

itreath <- gamlr(ix, ihum, lmr=1e-04, verb=TRUE, family="gaussian")
# itreatht <- gamlr(ix, itemphum, lmr=1e-04, verb=TRUE, family="gaussian")

summary(itreath)
summary(itreath)[which.min(AICc(itreath)),]
r2 = summary(itreath)[which.min(AICc(itreath)),]["r2"]
r2
plot(itreath)
ihhat = predict(itreath, ix, type="response")
summary(ihhat)
plot(ihhat)
plot(ihhat,ihum,bty="n",pch=21,bg=8) 

# summary(itreatht)
# summary(itreatht)[which.min(AICc(itreatht)),]
# r2 = summary(itreatht)[which.min(AICc(itreatht)),]["r2"]
# r2
# plot(itreatht)
# ihthat = predict(itreatht, ix, type="response")
# summary(ihthat)
# plot(ihthat)
# plot(ihthat,ihum,bty="n",pch=21,bg=8) 
ihum
itemphum
hhat
temphumhat = hhat*ix[,"temp"]
colnames(hhat) <- "hhat"
colnames(temphumhat) <- "temphumhat"
icausx = cBind(ihum,itemphum, hhat, temphumhat, ix)
colnames(icausx)
icausal <- gamlr(icausx, y, lmr=1e-04, verb=TRUE, family="gaussian", free=3:4)
coef(icausal)[c("ihum","itemphum"),] 

# Bonus
# USE A MAXIMUM OF ONE PAGE, INCLUDING PLOTS, TO ANSWER.
# Provide additional analysis of the data. Bonus will handed out only for insightful use of data
# mining tools, not for scattershot application of techniques.
# Do not spend too much (or any!) effort here: it is worth little.

#try leaving those outlier dates out of the data set?
summary(daytots)
hist(daytots$total)
acf(biketab$cnt, lag.max = 48)
arima(biketab$cnt, order=c(0,0,1));
lasthr <- biketab$cnt
firsthr = lasthr[1]
firsthr
lasthr <- c(firsthr, lasthr)
lasthr <- lasthr[-length(lasthr)]
head(lasthr)

tsbike <- sparse.model.matrix(
  cnt ~ . * lasthr + yr*mnth + hr*notbizday, 
  data=naref(biketab))[,-1]
tsbike2 <- tsbike[, -grep("dteday",colnames(tsbike))]

tsfitlin <- cv.gamlr( tsbike, y, lmr=1e-4, verb=TRUE )
tsfitlin2 <- cv.gamlr( tsbike2, y, lmr=1e-4, verb=TRUE )

png(filename=paste(sep="", Project, path, "BD_midterm.bonus.png"), width = 960, height = 480, units = "px")
plot(tsfitlin, main="Hourly autoregressive OOS perfomance", pch=21, col="blue")
points(y=fitlin$cvm, x=log(summary(fitlin)$lambda), pch=21, col="red")

abline(v=log(tsfitlin$lambda.min), col="orange", lty=3)
abline(v=log(tsfitlin$lambda.1se), col="green", lty=3)
abline(v=log(fitlin$lambda.min), col="purple", lty=3)
abline(v=log(fitlin$lambda.1se), col="black", lty=3)

legend("topleft", bty="n",
       fill=c("orange","green", "blue", "purple", "black", "red"),legend=c("AR min","AR 1se", "AR MSE", "Original min", "Original 1se", "Original MSE"))

dev.off()

summary(fitlin)
summary(tsfitlin)
summary(tsfitlin2)

log(tsfitlin$lambda.1se)
log(tsfitlin2$lambda.1se)
log(fitlin$lambda.1se)
summary(tsfitlin$gamlr)[tsfitlin$seg.1se,]
summary(tsfitlin2$gamlr)[tsfitlin2$seg.1se,]
summary(fitlin$gamlr)[fitlin$seg.1se,]

tscoefs = coef(tsfitlin)[grep("lasthr",rownames(coef(tsfitlin))),]
length(tscoefs[tscoefs!=0])

tscoefs2 = coef(tsfitlin2)[grep("lasthr",rownames(coef(tsfitlin2))),]
length(tscoefs2[tscoefs2!=0])
tscoefs2


plot(tsfitlin2, main="Hourly autoregressive OOS perfomance", pch=21, col="blue")
points(y=fitlin$cvm, x=log(summary(fitlin)$lambda), pch=21, col="red")

abline(v=log(tsfitlin2$lambda.min), col="orange", lty=3)
abline(v=log(tsfitlin2$lambda.1se), col="green", lty=3)
abline(v=log(fitlin$lambda.min), col="purple", lty=3)
abline(v=log(fitlin$lambda.1se), col="black", lty=3)

legend("topleft", bty="n",
       fill=c("orange","green", "blue", "purple", "black", "red"),legend=c("AR min","AR 1se", "AR MSE", "Original min", "Original 1se", "Original MSE"))















#try leaving those outlier dates out of the data set?
lastdayhr <- biketab$cnt
firstdayhr = lastdayhr[1:24]
length(lastdayhr)
length(firstdayhr)
lastdayhr <- c(firstdayhr, lastdayhr)
lastdayhr <- lasthr[-((length(lastdayhr)-length(firstdayhr) + 1):length(lastdayhr))]
length(lastdayhr)
length(biketab$cnt)
tsdaybike <- sparse.model.matrix(
  cnt ~ . * lastdayhr + yr*mnth + hr*notbizday, 
  data=naref(biketab))[,-1]
tsdaybike2 <- tsdaybike[, -grep("dteday",colnames(tsdaybike))]

tsdayfitlin <- cv.gamlr( tsdaybike, y, lmr=1e-4, verb=TRUE )
tsdayfitlin2 <- cv.gamlr( tsdaybike2, y, lmr=1e-4, verb=TRUE )



summary(fitlin)
summary(tsdayfitlin)
summary(tsdayfitlin2)

log(tsdayfitlin$lambda.1se)
log(tsdayfitlin2$lambda.1se)
log(fitlin$lambda.1se)

summary(tsdayfitlin$gamlr)[tsdayfitlin$seg.1se,]
summary(tsdayfitlin2$gamlr)[tsdayfitlin2$seg.1se,]
summary(fitlin$gamlr)[fitlin$seg.1se,]

tsdaycoefs = coef(tsdayfitlin)[grep("lastdayhr",rownames(coef(tsdayfitlin))),]
length(tscoefs[tsdaycoefs!=0])

tsdaycoefs2 = coef(tsdayfitlin2)[grep("lastdayhr",rownames(coef(tsdayfitlin2))),]
length(tscoefs2[tsdaycoefs2!=0])
tsdaycoefs2[t]

coef(fitlin)

png(filename=paste(sep="", Project, path, "BD_midterm.bonus.2.png"), width = 960, height = 480, units = "px")
plot(tsdayfitlin2, main="Hourly autoregressive OOS perfomance", pch=21, col="blue")
points(y=fitlin$cvm, x=log(summary(fitlin)$lambda), pch=21, col="red")

abline(v=log(tsdayfitlin2$lambda.min), col="orange", lty=3)
abline(v=log(tsdayfitlin2$lambda.1se), col="green", lty=3)
abline(v=log(fitlin$lambda.min), col="purple", lty=3)
abline(v=log(fitlin$lambda.1se), col="black", lty=3)

legend("topleft", bty="n",
       fill=c("orange","green", "blue", "purple", "black", "red"),legend=c("AR min","AR 1se", "AR MSE", "Original min", "Original 1se", "Original MSE"))
dev.off()

1-(fitlin$cvm[fitlin$seg.1se])/fitlin$cvm[1]
1-(tsdayfitlin2$cvm[tsdayfitlin2$seg.1se])/tsdayfitlin2$cvm[1]
summary(tsdayfitlin2)

