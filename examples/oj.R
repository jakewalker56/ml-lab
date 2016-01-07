###################################################
############# Orange Juice Regression #############
###################################################

## read in the data
oj <- read.csv("oj.csv") 

## create some colors for the brands
levels(oj$brand)
brandcol <- c("green","red","gold")
par(mfrow=c(1,2))
plot(log(price) ~ brand, data=oj, col=brandcol)
plot(logmove ~ log(price), data=oj, col=brandcol[oj$brand])

## simple regression
reg = glm(logmove ~ log(price) + brand, data=oj)

## use the fitted model
summary(reg) ## coef, tests, fit
coef(reg) ## just coefficients

## create some data for prediction, using the data.frame function
## note the care in specifying brand factor (levels must match original data)
## we don't need all variables in oj; just those used as covariates in reg.
newdata=data.frame(price=rep(4,3), 
	brand=factor(c("tropicana","minute.maid","dominicks"),levels=levels(oj$brand)))
## predict
predict(reg, newdata=newdata)  ## predicted log units moved
exp(predict(reg, newdata=newdata)) ## predicted # of units moved

## under the hood: `design matrices' and model.matrix
x <- model.matrix( ~ log(price) + brand, data=oj)
x[1,] ## first obsv of design matrix
oj[1,]  ## original data for first obsv.

## same regression, but with minute.maid reference
oj_copy <- oj
oj_copy$brand <- relevel(oj_copy$brand, "minute.maid")
glm(logmove ~ log(price) + brand, data=oj_copy)

## Interactions
## note that `*' also adds the main effects automatically
reg_interact = glm(logmove ~ log(price)*brand, data=oj)
summary(reg_interact)
## compare brand-specific log(price) slopes to our earlier elasticity (-3.1)

##### investigating advertisement
reg_ads <- glm(logmove ~ log(price)*brand+feat, data=oj)
summary(reg_ads)
# log(price)                  -2.89261    0.03279 -88.227  < 2e-16 ***
# feat                         0.89728    0.01043  86.029  < 2e-16 ***
# log(price):brandminute.maid  0.33426    0.05123   6.525 6.92e-11 ***
# log(price):brandtropicana    0.82914    0.04780  17.346  < 2e-16 ***

## look at the advertisement effect on elasticity
reg_ads2 <- glm(logmove ~ log(price)*(brand+feat), data=oj)
summary(reg_ads2)
# log(price)                  -2.60187    0.03591 -72.461  < 2e-16 ***
# feat                         1.40000    0.02809  49.841  < 2e-16 ***
# log(price):brandminute.maid  0.18644    0.05148   3.622 0.000293 ***
# log(price):brandtropicana    0.54811    0.04969  11.031  < 2e-16 ***
# log(price):feat             -0.75214    0.03906 -19.256  < 2e-16 ***

## and finally, consider 3-way interactions
ojreg <- glm(logmove ~ log(price)*brand*feat, data=oj)
summary(ojreg)
# log(price)                       -2.77415    0.03883 -71.445  < 2e-16 ***
# feat                              1.09441    0.03810  28.721  < 2e-16 ***
# log(price):brandminute.maid       0.78293    0.06140  12.750  < 2e-16 ***
# log(price):brandtropicana         0.73579    0.05684  12.946  < 2e-16 ***
# log(price):feat                  -0.47055    0.07409  -6.351 2.17e-10 ***
# brandminute.maid:feat             1.17294    0.08196  14.312  < 2e-16 ***
# brandtropicana:feat               0.78525    0.09875   7.952 1.90e-15 ***
# log(price):brandminute.maid:feat -1.10922    0.12225  -9.074  < 2e-16 ***
# log(price):brandtropicana:feat   -0.98614    0.12411  -7.946 2.00e-15 ***

## for the elasticities table
b <- coef(ojreg)
b["log(price)"] 
b["log(price)"] + b["log(price):brandminute.maid"]
b["log(price)"] + b["log(price):brandtropicana"]
b["log(price)"] + b["log(price):feat"] 
b["log(price)"] + b["log(price):brandminute.maid"] + b["log(price):feat"] + b["log(price):brandminute.maid:feat"]
b["log(price)"] + b["log(price):brandtropicana"] + b["log(price):feat"] + b["log(price):brandtropicana:feat"]

## table explaining why ads confounded our brand elasticity estimates
salestable <- tapply(exp(oj$logmove), oj[,c("feat","brand")], sum)
mosaicplot(salestable,col=brandcol)

## fit plots and R^2 
## (the 'bty="n"' option removes boxes around your plot)
plot(ojreg$fitted ~ oj$logmove, col=brandcol[oj$brand], bty="n")
abline(a=0,b=1)#  add a line with slope 1, intercept 0
legend("topleft",legend=levels(oj$brand),fill=brandcol, bty="n")
cor(ojreg$fitted,oj$logmove)^2





