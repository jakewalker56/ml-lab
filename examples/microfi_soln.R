## microfinance network 
source("microfi_start.R")

# create a sparse model matrix x and response y
# first, remove reference levels for each factor
source("naref.R")
hhnoref <- naref(hh)
## I looked all controls interacted.
## No big deal if you just did main effects.
x <- sparse.model.matrix(loan ~.^2, data=hhnoref)[,-1]
y <- hh$loan

## [1]
## look at degree
hist(degree)
## it looks like it might be normal on the log scale...
## this doesn't always indicate you need a log transform, but its a good hint.
##
## this is important because we need to predict it for control (dhat)
## for example, look what happens if you try a linear model for degree:
treat <- gamlr(x,degree) 
degreehat <- predict(treat, x)
plot(degree,degreehat)
## looks like nonconstant variance...
## Instead, we'll look at log(1+degree) as our treatment
d <- log(1+degree)
## this also makes sense if you think about the concept of `connectivity':
## moving from 0 to 1 friends is a bigger change than from 50 to 51.
##
## The big point here is that during the double lasso procedure
## we are predicting 'treatment'.  So the material of lecture 2 (on
## what scale do you expect linearity?) applies.
## 
## Alternatively, one could also argue for a `square root' transformation.  
## It gives results that look pretty much like what I get after log transform.
## The only disadvantage is that you lose the log-log % interpretation, but not a big deal.
dsqr <- sqrt(degree)
fitsqr <- gamlr(x,dsqr)
plot(predict(fitsqr,x),dsqr)

## [2]
## predicting treatment (degree).  
## I'll fit this with cv.gamlr for illustration, but you didn't need to.
cv.treat <- cv.gamlr(x,d) 
# I've used AICc for selecting dhat; its fine if you used something else.
dhat <- predict(cv.treat$gamlr, x)
dhat <- drop(dhat) # tell R to just treat this a simple vector (not sparse matrix).
# sometimes using sparse matrices in simple functions will cause errors.

# for illustration, plot the OOS experiment results
plot(cv.treat)
# OOS R^2 looks to be maximized under 10% on the plot 
1-min(cv.treat$cvm)/cv.treat$cvm[1] #it's actually ~7%
# so there's lots of independent variation upon which we can measure a treatment effect.

# We want to choose a model that gives high OOS R^2, (cv or aicc), but what actually
# dictates the amount of signal we have upon which to measure a treatment effect.
# To put it another way, so long as you have a the best available model for estimating dhat
# then you can test for effect of d if there is in-sample variation around this best fit.

# is the in-sample R^2: how much variation is there in our fitted residuals (the 'nu' from class)
# Here, IS R^2 is 11%; calculate from deviances
D <- sum( (d-dhat)^2 )
D0 <- sum( (d-mean(d))^2 )
1-D/D0
# Or, just use gamlr's summary function 
summary(cv.treat$gamlr)[which.min(AICc(cv.treat$gamlr)),]
# Regardless: Lots of in-sample variation.  
# This doesn't mean that our results will be necesarilly _causal_ if we didn't include
# or measure all of the controls that we should have (I doubt we have in this example), but
# it means that we have a strong signal to measure for effect of d after controlling for the given x.

# plot d v dhat: still some nonconstant variance, but looks better to me than for raw degree.
plot(d,dhat)

## [3]
causal <- gamlr(cBind(d,dhat,x),y,family="binomial",free=2)
plot(causal)
coef(causal)["d",] # around 0.13
## An effect!  so at least amongst these controls, we've got evidence
## that connectivity and propensity to borrow money are related.  
## open question: which way does the causation run?
## Interpretation:
## This is a log-log regression!  log odds on log degree (+1).
## So like with the OJ, example, we can interpret 0.13 
## as % increase in odds of taking a loan per 1% increase in degree.
## (Note that we don't care about R^2 here.)

## [4]
## naive lasso
naive <- gamlr(cBind(d,x),y,family="binomial")
coef(naive)["d",]
## (Note that we don't care about R^2 here either.)
## EXTRA: 'marginal' (no controls) regression too
marginal <- glm(y~d,family="binomial")
coef(marginal)["d"]
## these are pretty close here, because treatment is already
## mostly independent from controls 
## (d is not easy to predict, as we saw in [2], so this is like an experiment)
## Note also that the effect _increases_ in magnitude after adding controls.
#   
# Controlling for confounders doesn't always reduce causal effects.  
# This only happens if confounders have the same sign on their correlation with both
# treatment and response.  If these signs are different, then control
# can even give you a bigger effect than you would otherwise get.
#
# Think about a study where only the sickest people get some treatment.
# Then if you don't control for how sick they are, and compare them to
# [healthier] people who didn't get the treatment, you'll conclude the
# drug is less effective than it actually is.

## [5]
## bootstrap it;
n <- nrow(hh)
B <- 100 # you could have used bigger or smaller
bootgamma <- rep(0,B)
for(b in 1:B){
	ib <- sample(1:n, n,replace=TRUE)
	xb <- x[ib,]
	db <- d[ib]
	yb <- y[ib]
	treatb <- gamlr(xb,db)
	dhatb <- predict(treatb,xb)
	fitb <- gamlr(cBind(db,dhatb,xb),
				yb,free=2,family="binomial")
	bootgamma[b] <- coef(fitb)["db",]
	print(b)
}
## EXTRA: instead, do it in parallel!
## the stuff with cl and the parallel library is advanced R
## we'll be discussing parallelization more after midterm.
library(parallel) 
cl = makeCluster(detectCores())
resamp <- as.data.frame(
	matrix(sample(1:n, B*n, replace=TRUE),ncol=B))
## a fit function 
bootfit <- function(ib){ 
	require(gamlr)
	xb <- x[ib,]
	db <- d[ib]
	yb <- y[ib]
	treatb <- gamlr(xb,db)
	dhatb <- predict(treatb,xb)
	fitb <- gamlr(cBind(db,dhatb,xb),
				yb,free=2,family="binomial")
	return(coef(fitb)["db",])
}
clusterExport(cl, c("x","d","y"))
parbootgamma <- unlist(parLapply(cl,resamp,bootfit))
stopCluster(cl) 

## Finally, plots. Use either bootgamma or, if you got fancy with parallel, parbootgamma.
## These pictures will summarize the sampling distribution
## for 'gamma hat' estimated under our lasso treatment effect routine.
## It includes uncertainty from resampling both X and Y.
hist(bootgamma, col="grey70", xlab="log network degree effect", main="")
abline(v=coef(causal)["d",],lwd=2, col="gold")
## the picture shows that our full sample AIC estimate is near the low end of the sampling
# distribution; there is lots of uncertainty about a possibly larger effect.

## BONUS 
# kudos to anyone who described a coeherent experiment, especially if you
# talked about 'villages' as blocks.  They are like the DMAs -- your unit of experimentation.
# 
# ideally, I'd randomize the villages and then try to foster connectivity in 
# a treatment subset of villages.  You could go in and provide some venue for community members
# to meet and trade in ways the currently do not; I'm sure you all have more creative answers!

## related EXTRA content... 
## Another model that I like even better:
## Let all village `fixed effects' enter unpenalized
## just like we did with team-seasons in the hockey example
## (we have enough data, and this is closer to a blocked experiment)
colnames(x)[1:38]  # village effect is first 38 columns of my x
## say vc stands for 'village controls'
causal_vc <- gamlr(cBind(d,dhat,x),y,family="binomial",
	free=c(2,2+1:38)) # why 2+1:38?
coef(causal_vc)["d",] # result doesn't change much.

#########  WEEK 9 RANDOM FORESTS STUFF

## use RF to estimate dhat
library(randomForest) 
## RFs do a bad job with high dimensional factors (e.g. village)
## so we'll do as I do above in `EXTRA' and just include that unpenalized.
## now we need to get a model for dhat from all other controls
hh$loan <- factor(hh$loan) # RF likes you to do this first
rf.treat <- randomForest(d~.-village-loan, data=hh, ntree=250, nodesize=25,importance=TRUE)
varImpPlot(rf.treat, type=1)

rf.dhat <- predict(rf.treat, newdata=hh)
plot(d,rf.dhat)
cor(d,rf.dhat)^2 # lower than above, but we didn't include village

rf.causal <- gamlr(cBind(d,rf.dhat,x),y,family="binomial",
					free=c(2,2+1:38)) # village enters unpenalized

coef(rf.causal)["d",] # result drops to around 0.1; not a big diff.


