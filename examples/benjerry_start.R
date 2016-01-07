#### Purchases of Ben and Jerry's Ice Cream
benjer = read.csv("BenAndJerry.csv")

## explore a bit
names(benjer)

## create a new variable for price per unit
priceper1 = (benjer$price_paid_deal + benjer$price_paid_non_deal)/benjer$quantity
y <- log(1+priceper1)

## grab some covariates of interest
## we'll create a properly formatted data.frame
x <- benjer[,c("flavor_descr","size1_descr",
	"household_income","household_size")]

## relevel 'flavor' to have baseline of vanilla
x$flavor_descr <- relevel(x$flavor_descr,"VAN")
## coupon usage
x$usecoup = factor(benjer$coupon_value>0)
x$couponper1 <- benjer$coupon_value/benjer$quantity
## organize some demographics
x$region <- factor(benjer$region, 
	levels=1:4, labels=c("East","Central","South","West"))
x$married <- factor(benjer$marital_status==1)
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

## source the fdr_cut function
source("fdr.R")



