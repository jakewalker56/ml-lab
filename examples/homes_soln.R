##### ******** Mortgage and Home Sales Data ******** #####

source("homes_start.R")

## Q1: do a bunch more plots.
## you could do anything; 
## here, e.g., I look at subjective survey responses

par(mfrow=c(1,2)) # make it rows of two
# HOWH
# Rating of unit as a place to live
plot(gt20dwn ~ HOWH, col=c(1,4), data=homes, xlab="unit is `good'")
plot(log(VALUE) ~ HOWH, col=c(8,3), data=homes, xlab="unit is `good'")
# HOWN
# Rating of neighborhood as place to live
plot(gt20dwn ~ HOWN, col=c(1,4), data=homes, xlab="hood is `good'")
plot(log(VALUE) ~ HOWN, col=c(8,3), data=homes, xlab="hood is `good'")
# ODORA
# Neighborhood has bad smells
plot(gt20dwn ~ ODORA, col=c(1,4), data=homes, xlab="hood smells")
plot(log(VALUE) ~ ODORA, col=c(3,8), data=homes, xlab="hood smells")
# STRNA
# Neighbrhd has heavy street noise/traffic
plot(gt20dwn ~ STRNA, col=c(1,4), data=homes, xlab="lots of traffic")
plot(log(VALUE) ~ STRNA, col=c(3,8), data=homes, xlab="lots of traffic")

## Q2: FDR and Deviance
summary(pricey)
1-10359/14920 #R2 of around 30% for full model

## pvalues
source("fdr.R")
alpha <- fdr_cut(pvals, q=.1) # turns out around .08
0.9*sum(pvals<=alpha) # gives us 34 signif, 3-4 of which are expected false discos.

names(pvals)[pvals>alpha]
pricecut <- glm(log(VALUE) ~ .-AMMORT-LPRICE-ECOM1-EGREEN-ELOW1-ETRANS-ODORA-PER-ZADULT,
				data=homes)

summary(pricecut)
1-10364/14920 #R2 still around 30% 

## Q3: logistic regression
loans <- glm(gt20dwn ~ .-AMMORT-LPRICE, data=homes, family="binomial")
summary(loans)
1-16969/18873 ## R2 is about 10%

b <- coef(loans)
exp(b["FRSTHOY"]) # .69 => first homers have ~30% lower odds of at least 20% down
exp(b["BATHS"]) # 1.27 => an extra bath room increases odds by ~30%

# add some interactions 
loans2 <- glm(gt20dwn ~ .-AMMORT-LPRICE+BATHS*FRSTHO, 
	data=homes, family="binomial")
summary(loans2)

## multiplicative effect of an extra bathroom on odds gt20 if its your first home
b <- coef(loans2)
exp(b["BATHS"] + b["BATHS:FRSTHOY"])  # now odds only increase by ~10%
## and effect of an extra bath if this is not your first home
exp(b["BATHS"]) # now odds increase by 34%
## so size of house (#baths) effect is more pronounced for non-first home buyers

# Q4 OOS prediction
gt100 <- which(homes$VALUE>1e5)
## fit the model to this subset only
loans_gt100 <- glm( gt20dwn ~ .-AMMORT-LPRICE+BATHS*FRSTHO, 
	data=homes, subset=gt100, family="binomial") 

summary(loans_gt100) ## summarize the regression fit
1-13617/15210 # still ~10% in-sample R2

## OOS deviance
source("deviance.R")

preds <- predict(loans_gt100, homes[-gt100,], type="response")
D <- deviance(y=homes$gt20dwn[-gt100], pred=preds, family="binomial")

# ybar and null deviance
ybar <- mean(homes$gt20dwn[-gt100]==TRUE)
D0 <- deviance(y=homes$gt20dwn[-gt100], pred=ybar, family="binomial")

1-D/D0 # down to only 3.5% R2.
## it looks like our model has done a poor job extrapolating
## to lower valued homes.
