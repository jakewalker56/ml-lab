#### Purchases of Ben and Jerry's Ice Cream

source("benjerry_start.R")

#### analysis

## histograms
par(mfrow=c(1,2))
hist(pvals) ## looks like a ton of signal
hist(y)

## some high prices paid!  Who are these?
gt8 = priceper1 >8  ## create a logical variable
benjer[gt8,]  ## who pays more than $8 for ice cream?
levels(benjer$size1_descr) ## size is a "factor" with two levels

## plot price vs coupon value
plot(priceper1 ~ couponper1, data=xy)
## R has 8 basic colors; first two are black and red.
plot(priceper1 ~ couponper1, 
	col=c('Gray','Red')[xy$size1_descr],  data=xy, ## make it easier on the eye
  xlab="Coupon Value per unit", ylab="Price per Unit", main="Ben and Jerry's Purchases") ## and add labels!
legend("bottomright", legend=c("16oz","32oz"), fill=c('Gray','Red')) ## add a legend
## they're giving it away!! everything on the diagonal line was free.

## another variable of interest
paid <- priceper1 - xy$couponper1

## Boxplots
## a bit clunky, but super powerful.  'par' controls graphing parameters.
## 'mfrow' is 'multi-frame, by row'.  c(1,2) means 1-row, 2-columns.
par(mfrow = c(1,2))  
plot(paid ~ usecoup, data=xy, col=c(8,2), xlab="Coupon was Used?", ylab="Amount Paid per Unit") 
plot(paid ~ region, data=xy, col=c(3,5,6,7), xlab="Region", ylab="Amount Paid per Unit") # I like to fill the boxes with color

## Mosaics
par(mfrow = c(1,2))  
plot(region ~ usecoup, data=xy, col=c(3,5,6,7), xlab="Coupon was Used?", ylab="Region")
plot(married ~ usecoup, data=xy, xlab="Coupon was Used?", ylab="Married?")
## these mosaics are the graphical version of a contingency table

## for `improve the regression', I wanted to see if you could 
## add some variables and tell more of a story.  This should 
## happen before FDR: i.e., in the hypothesizing stage, rather ## than post control.  
## You could also have replaced log(priceper1+1) with log(paid)
summary(fit)

## FDR control
print(cutoff <- fdr_cut(pvals,.1)) 
signif <- coef(fit)[-1][pvals<=cutoff]
length(signif) # 46*0.9 = around 41 true discoveries.

signif[grep("region",names(signif))] ## everyone pays less than the NE
signif["usecoupTRUE"] ## coupon usage drops expected log price by -0.071
signif["couponper1"] ## but only given that price increases with SIZE of coupon...


## [+] we should be suspicious, because p-values in a regression 
## are all conditional on the other variables being in-the-model;
## thus we don't have independence between tests!

## extra: re-fitting only signif with model.matrix
modmat <- model.matrix( ~., data=xy)[,-1] # -1 drop the intercept
fit <- glm( modmat[,"y"] ~., data=as.data.frame(modmat)[, names(signif)])
