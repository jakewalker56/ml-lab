
SC <- read.csv("semiconductor.csv")

## full model
full <- glm(FAIL ~ ., data=SC, family=binomial)
1 - full$deviance/full$null.deviance

## grab p-values
pvals <- summary(full)$coef[-1,4] #-1 to drop the intercept
## plot them: it looks like we have some signal here
hist(pvals, xlab="p-value", main="", col="lightblue")

## At 10% FDR, we get 25 `signif'
source("fdr.R")
q <- 0.1
alpha <- fdr_cut(pvals, q) ## cutoff
signif <- which(pvals <= alpha) ## which are significant
length(signif)  ## the number significant

## Re-run a cut regression using only these 25
## [this is easy here because all of the inputs are numeric, 
## so the column names in SC are the same as the variable names.
## this sort of thing is harder with factors, etc.]
# get names of variables to include
cutvar <- c("FAIL", rownames(summary(full)$coef)[-1][signif]) 
# run regression on these alone
cut <- glm(FAIL ~ ., data=SC[,cutvar], family="binomial")
# new in-sample R2: drops to 
1 - cut$deviance/cut$null.deviance

## Out of sample prediction experiment
## for this we'll call our `deviance' function
source("deviance.R")
n <- nrow(SC) # the number of observations
nfold <- 10 # the number of OOS validation `folds'
# create a vector of fold memberships (random order)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
# create an empty dataframe of results
OOS <- data.frame(full=rep(NA,nfold), cut=rep(NA,nfold)) 
# use a for loop to run through the nfold trails
for(k in 1:nfold){ 
	train <- which(foldid!=k) # train on all but fold `k'
		
	## fit the two regressions
	rfull <- glm(FAIL~., data=SC, subset=train, family=binomial)
	rcut <- glm(FAIL~., data=SC[,cutvar], subset=train, family=binomial)

	## get predictions: type=response so we have probabilities
	predfull <- predict(rfull, newdata=SC[-train,], type="response")
	predcut <- predict(rcut, newdata=SC[-train,], type="response")

	## calculate and log R2
	OOS$full[k] <- R2(y=SC$FAIL[-train], pred=predfull, family="binomial")
	OOS$cut[k] <- R2(y=SC$FAIL[-train], pred=predcut, family="binomial")

	## print progress
	print(k)
}
## plot it in plum
boxplot(OOS, col="plum", ylab="R2", xlab="model")
## what are the average OOS R2?
colMeans(OOS) # WOW!  Full model really sucks.

## A forward stepwise procedure
# null model
null <- glm(FAIL~1, data=SC)
# forward stepwise: it takes a long time!
system.time(fwd <- step(null, scope=formula(full), dir="forward"))
length(coef(fwd)) # chooses around 70 coef

#### lasso (glmnet does L1-L2, gamlr does L0-L1)
library(gamlr) 
# for gamlr, and most other functions, you need to create your own numeric
# design matrix.  We'll do this as a sparse `simple triplet matrix' using 
# the sparse.model.matrix function.
scx <- sparse.model.matrix(FAIL ~ ., data=SC)[,-1] # do -1 to drop intercept!
# here, we could have also just done x <- as.matrix(SC[,-1]).
# but sparse.model.matrix is a good way of doing things if you have factors.
scy <- SC$FAIL # pull out `y' too just for convenience

# fit a single lasso
sclasso <- gamlr(scx, scy, family="binomial")
plot(sclasso) # the ubiquitous path plot

# AICc selected coef
scbeta <- coef(sclasso) 
log(sclasso$lambda[which.min(AICc(sclasso))])
sum(scbeta!=0) # chooses 30 (+intercept) @ log(lambda) = -4.5

# alt: BIC selected coef
BICseg <- which.min(BIC(sclasso))
scb.bic <- coef(sclasso, s=BICseg)
sum(scb.bic!=0) # sets all coef to zero: just the intercept!

## cross validated lasso (verb just prints progress)
sccvl <- cv.gamlr(scx, scy, family="binomial", verb=TRUE)
plot(sccvl, bty="n")

## CV min deviance selection
scb.min <- coef(sccvl, select="min")
log(sccvl$lambda.min)
sum(scb.min!=0) ## around 65-70 with log(lam) -4.8 (its random!)

## CV 1se selection (the default)
scb.1se <- coef(sccvl)
log(sccvl$lambda.1se)
sum(scb.1se!=0) ## usually selects all zeros (just the intercept)

## comparing AICc, BIC, and the CV error
plot(sccvl, bty="n")
lines(log(sclasso$lambda),AICc(sclasso)/n, col="green", lwd=2)
lines(log(sclasso$lambda),BIC(sclasso)/n, col="maroon", lwd=2)
legend("top", fill=c("blue","green","maroon"),
	legend=c("CV","AICc","BIC"), bty="n")

## BIC, AIC, AICc on full vs cut (they all prefer cut)
BIC(full)
BIC(cut)

AIC(full)
AIC(cut)

AICc(full)
AICc(cut)