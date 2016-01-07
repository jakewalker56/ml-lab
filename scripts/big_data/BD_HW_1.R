#Mac/Unix
Project = "~/github"

path= "/projects/data/"
setwd(paste(Project, path, sep=""))


#### Purchases of Ben and Jerry's Ice Cream
benjer = read.csv("BenAndJerry.csv")

## explore a bit
names(benjer)
summary(benjer)

## create a new variable for price per unit
benjer[1,]
priceper1 = (benjer$price_paid_deal + benjer$price_paid_non_deal)/benjer$quantity
y <- log(1+priceper1)
plot(priceper1)
plot(y)

## grab some covariates of interest
## we'll create a properly formatted data.frame
x <- benjer[,c("flavor_descr","size1_descr",
               "household_income","household_size", "quantity")]

## relevel 'flavor' to have baseline of vanilla
x$flavor_descr <- relevel(x$flavor_descr,"VAN")
## coupon usage
x$usecoup = factor(benjer$coupon_value>0)
x$couponper1 <- benjer$coupon_value/benjer$quantity
## organize some demographics
x$region <- factor(benjer$region, 
                   levels=1:4, labels=c("East","Central","South","West"))
x$married <- factor(benjer$marital_status==1)
?factor
x$race <- factor(benjer$race,
                 levels=1:4,labels=c("white","black","asian","other"))
x$hispanic_origin <- benjer$hispanic_origin==1
x$microwave <- benjer$kitchen_appliances %in% c(1,4,5,7)
x$dishwasher <- benjer$kitchen_appliances %in% c(2,4,6,7)
x$sfh <- benjer$type_of_residence==1
x$internet <- benjer$household_internet_connection==1
x$tvcable <- benjer$tv_items>1


## combine x and y, just to follow my recommended `way to use glm'
## cbind is `column bind'.  It takes two dataframes and makes one.
xy <- cbind(x,y)

## fit the regression
fit <- glm(y~., data=xy) 

## grab the non-intercept p-values from a glm
## -1 to drop the intercept, 4 is 4th column
pvals <- summary(fit)$coef[-1,4] 
pvals
## source the fdr_cut function
source("../RScripts/fdr.R")
q=0.01
a = fdr_cut(pvals, q, TRUE)
coefs = pvals[pvals < a]
coefs[order(coefs)]
plot(xy$quantity, xy$y)
quantity_reg = lm(y~quantity, data=xy)
abline(quantity_reg, col="red")

summary(fit)
hist(pvals)
plot(xy$couponper1,xy$y)
plot((benjer$price_paid_deal + benjer$price_paid_non_deal)/benjer$quantity, xy$couponper1)
length(benjer[benjer$price_paid_deal == benjer$coupon_value,1])
plot(xy[xy$usecoup==TRUE,]$quantity)
summary(xy[xy$usecoup==TRUE & xy$y > 2,]$quantity)
summary(factor(xy[xy$usecoup==TRUE & xy$y > 1.75,]$quantity))
y_frame_ratio = c()
y_frame_x = c()
for (i in seq(1.5,2.0,.001))
{
  y_frame_x = c(y_frame_x,i)
  arr_2 = xy[xy$usecoup==TRUE & xy$y > i,]$quantity == 2
  arr_1 = xy[xy$usecoup==TRUE & xy$y > i,]$quantity == 1
  y_frame_ratio = c(y_frame_ratio, length(arr_2[arr_2==TRUE])/length(arr_1[arr_1==TRUE]))
}
xy$flavor_descr
y_frame_ratio
y_frame_x
plot(y_frame_x, y_frame_ratio)
length(xy[xy$usecoup==TRUE & xy$y > 2,]$quantity == 1)
2.718^1.7
price_reg = lm(xy$couponper1 ~ (benjer$price_paid_deal + benjer$price_paid_non_deal)/benjer$quantity)
summary(price_reg)
#how on earth does couponper1 have a positive coefficient?  
#Shouldn't that DEFINITELY be negative?
#seems like there's something sketchy here.  Some times the price is 8.98 AFTER an 8.98 coupon, other times the price for the same size is $6.00?  That seems unreasonable...
#best explination I can come up with is either bad data, or 'buy one get one free' is SUPER common (and they massively mark up the price before they do it?)

#flavors are probably correlated with cupon usage, quantity, size, etc.
#interact with coupon usage
coupon_fit <- glm(y ~., data=xy) 
coupon_pvals <- summary(coupon_fit)$coef[-1,4] 
q=0.01
a = fdr_cut(coupon_pvals, q, TRUE)
coupon_coefs = coupon_pvals[coupon_pvals < a]
length(coupon_pvals)
length(coupon_coefs)
coupon_coefs
#ok, that didn't help anything

#remove coupon usage
nocoupon_xy = xy[xy$usecoup == FALSE,]
nocoupon_xy = subset(nocoupon_xy, select=-c(usecoup))
nocoupon_fit <- glm(y ~., data=nocoupon_xy) 
nocoupon_pvals <- summary(nocoupon_fit)$coef[-1,4] 
q=0.01
a = fdr_cut(nocoupon_pvals, q, TRUE)
nocoupon_coefs = nocoupon_pvals[nocoupon_pvals < a]
length(nocoupon_pvals)
length(nocoupon_coefs)
nocoupon_coefs
summary(nocoupon_fit)

#commented out so I don't accidentally run it and fry my computer :p
interacted_fit <- glm(y~.*., data=xy) 
summary(interacted_fit)
interacted_pvals <- summary(interacted_fit)$coef[-1,4] 

q=0.01
a = fdr_cut(interacted_pvals, q, TRUE)
interacted_coefs = interacted_pvals[interacted_pvals < a]
length(interacted_pvals)
length(interacted_coefs)
interacted_coefs
interacted_coefs[order(interacted_coefs)]

summary(xy[xy$size=="32.0 MLOZ",]$flavor)
plot(xy[xy$size=="32.0 MLOZ",]$flavor)
summary(xy$size)

