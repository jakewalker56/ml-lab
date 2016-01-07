####### donohue and levitt 2001/2008: abortion and crime

## example reading non csv data: this is a dump from STATA
## skip says skip the first line of the file, sep="/t" says 'tab separated'
data <- read.table("abortion.dat", skip=1, sep="\t")
names(data) <- c("state","year","pop","y_viol","y_prop","y_murd",
	"a_murd","a_viol","a_prop",'prison','police',
	'ur','inc','pov','afdc','gun','beer')

## prison: log of lagged prisoners per capita
## police: the log of lagged police per capita
## ur: the unemployment rate
## inc: per-capita income
## pov: the poerty rate
## AFDC: generosity at year t-15
## gun: dummy for concealed weapons law
## beer: beer consumption per capita 

data <- data[!(data$state%in%c(2,9,12)),] # AK, DC, HA are strange places
data <- data[data$year>84 & data$year<98,] # incomplete data outside these years
data$pop <- log(data$pop)
t <- data$year-85
t2 <- t^2
s <- factor(data$state) ## the states are numbered alphabetically

controls <- data.frame(data[,c(3,10:17)])
## y is de-trended log crime rate, a is as described below
## note we also have violent and property crime versions
y <- data$y_murd
d <- data$a_murd

## The abortion 'a_' variables are weighted average of abortion rates where
## weights are determined by the fraction of the type of crime committed by
## various age groups. For example, if 60% of violent crime were committed by 18
## year olds and 40% were committed by 19 year olds in state i, the abortion rate
## for violent crime at time t in state i would be constructed as .6 times the
## abortion rate in state i at time t − 18 plus .4 times the abortion rate in
## state i at time t − 19. See Donohue and Levitt (2001) for further detail.

## we'll just look at murder
## note for convenience here I've made y,d,t, global: they are not in controls.
summary(orig <- glm(y ~ d+t+s+., data=controls) )$coef['d',]
## this is the levitt analysis: higher abortion leads to lower crime

## Now the same analysis, but for cellphones rather than abortion
cell <- read.csv("us_cellphone.csv")
# center on 1985 and scale by 1997-1985
cellrate <- (cell[,2]-cell[1,2])/(cell[13,2]-cell[1,2]) 
## what if we're just fitting a quadratic trend?
## there are many things that increased with similar shapes over time
## (cellphone usage, yoga revenues, home prices, ...)
plot(1985:1997, tapply(d, t, mean), xlab="year", ylab="adjusted rate", pch=21, bg=2)
points(1985:1997, cellrate, bg=4, pch=21)
legend("topleft", fill=c(2,4), legend=c("abortions","cellphones"), bty="n")
phone <- cellrate[t+1]
## clearly, cellphones fight crime.
summary(tech <- glm(y ~ phone+t+s+., data=controls))$coef['phone',]

## what is happening here is that murder has been increasing quadratically,
## and we have no other controls that do so.  To be correct, you need
## to allow quadratic trends that could be caused by other confounding variables (e.g. technology)

## each state should also have a different baseline linear and quadratic trend
## and, at very least, controls should interact with each other.
## we also allow the control effects to change in time (interact with t+t2)
# (no intercept, since we've removed the reference level from state)
summary(interact <- glm(y ~ d + (s + .^2)*(t+t2), data=controls))$coef['d',] 
## Abortion is no longer significant.
dim(model.matrix(y ~ d + (s + .^2)*(t+t2), data=controls))
## we have very few observations relative to number of parameters.

## so we need a way to select only important controls
## try using a lasso 
library(gamlr)
## refactor state to have NA reference level
s <- factor(s, levels=c(NA,levels(s)), exclude=NULL)
x = sparse.model.matrix(~ (s + .^2)*(t+t2), data=controls)[,-1]
dim(x)

## naive lasso regression
naive <- gamlr(cBind(d,x),y)
coef(naive)["d",] # effect is AICc selected <0

## now, what if we explicitly include dhat confounding:
treat <- gamlr(x,d,lambda.min.ratio=1e-4)
# we needed to drop lambda.min.ratio because AICc wants a complex model
# that indicates that abortion rates are highly correlated with controls.
plot(treat)

# Now, grab the predicted treatment
# type="response" is redundant here (gaussian), 
# but you'd want it if d was binary
dhat <- predict(treat, x, type="response") 
## not much signal in d not predicted by dhat
plot(dhat,d,bty="n",pch=21,bg=8) 
## that means we have little to resemble an experiment here...

## IS R^2?
cor(drop(dhat),d)^2
## Note: IS R2 is what governs how much independent signal
## you have for estimating 

# re-run lasso, with this (2nd column) included unpenalized
causal <- gamlr(cBind(d,dhat,x),y,free=2)
coef(causal)["d",] # AICc says abortion has no causal effect.

## BOOTSTRAP 
n <- nrow(x)

## Bootstrapping our lasso causal estimator is easy
gamb <- c() # empty gamma
for(b in 1:20){
	## create a matrix of resampled indices
	ib <- sample(1:n, n, replace=TRUE)
	## create the resampled data
	xb <- x[ib,]
	db <- d[ib]
	yb <- y[ib]
	## run the treatment regression
	treatb <- gamlr(xb,db,lambda.min.ratio=1e-3)
	dhatb <- predict(treatb, xb, type="response")

	fitb <- gamlr(cBind(db,dhatb,xb),yb,free=2)
	gamb <- c(gamb,coef(fitb)["db",])
	print(b)
}
## not very exciting though: all zeros
summary(gamb) 
## it's saying there's near zero chance AICc selects gamma!=0


#######################
## EXTRA
##
## FREQUENTIST OPTION: the Belloni, Chernozukov, Hansen version of this
yonx <- gamlr(x,y,lambda.min.ratio=0.001)
## get the union of controls that predict y or d
inthemodel <- unique(c(which(coef(yonx)[-1]!=0), # -1 drops intercept
						which(coef(treat)[-1]!=0))) # unique grabs union
selectdata <- cBind(d,x[,inthemodel]) 
selectdata <- as.data.frame(as.matrix(selectdata)) # make it a data.frame
dim(selectdata) ## p about half n

## run a glm
causal_glm <- glm(y~., data=selectdata)
## The BCH theory says that the standard SE calc for gamma is correct
summary(causal_glm)$coef["d",] # BCH agrees with AICc: not significant.
