#Mac/Unix
Project = "~/github"

path= "/ml-lab/data/"
setwd(paste(Project, path, sep=""))

library(gamlr)
library(glmnet)
source("../utilities/naref.R")

benjer = read.csv("final_health_data.csv")
rands = runif(length(benjer[,1]), 0, 1)
benjer <- benjer[rands > 0.99,]

#remove rows with unkown data
benjer <- benjer[benjer$mothers_age != 99,]
benjer <- benjer[benjer$maternal_bmi != 9,]
benjer <- benjer[!is.na(benjer$fathers_age),]

benjer$father_unknown <- benjer$fathers_age == 99
nrow(benjer)
colnames(benjer)

summary(benjer$father_unknown)

#factor that which ought be factored
benjer$mothers_race <- factor(benjer$mothers_race)
benjer$fathers_race <- factor(benjer$fathers_race)
benjer$mothers_status <- factor(benjer$mothers_status)
benjer$mothers_education <- factor(benjer$mothers_education)
benjer$fathers_education <- factor(benjer$fathers_education)
benjer$maternal_bmi <- factor(benjer$maternal_bmi)

#normalize ages to make coefs less sensitive
benjer$mothers_age <- benjer$mothers_age - 12
benjer$fathers_age <- benjer$fathers_age - 12

#add square vars
benjer$mothers_age_2 <- benjer$mothers_age^2
benjer$fathers_age_2 <- benjer$fathers_age^2


benjer <- naref(benjer)
benjer <- benjer[!is.na(benjer$prenatal_care_month),]

benjer[benjer$anencephaly != "Y",]$anencephaly <- "N"
benjer$anencephaly <- factor(benjer$anencephaly)
summary(benjer$anencephaly)

benjer[benjer$down_syndrome != "N",]$down_syndrome <- "C"
benjer$down_syndrome <- factor(benjer$down_syndrome)
summary(benjer$down_syndrome)

benjer[benjer$cg_heart_defect != "N",]$cg_heart_defect <- "Y"
benjer$cg_heart_defect <- factor(benjer$cg_heart_defect)
summary(benjer$cg_heart_defect)



#Exploratory
hist(benjer$apgar_5)
summary(benjer$apgar_10)

ages = seq(0,max(benjer$mothers_age))
plot(ages, unlist(lapply(ages, function(x) { 
  mean(benjer[benjer$mothers_age == x,]$apgar_5)})),
  ylab="mean apgar by age")

reg = lm(apgar_5 ~ (mothers_age + fathers_age + 
                      mothers_age_2 + fathers_age_2 + 
                      mothers_race + fathers_race + 
                      mothers_education + fathers_education + 
                      mothers_status + in_hospital + prenatal_care_month +
                      maternal_bmi + father_unkown) * 
           (mothers_age + fathers_age + 
              mothers_age_2 + fathers_age_2 + 
              mothers_race + fathers_race + 
              mothers_education + fathers_education + 
              mothers_status + in_hospital + prenatal_care_month +
              maternal_bmi + father_unkown), data=benjer)
#baseline r^2 = 0.028
summary(reg)

apgar = benjer$apgar_5
factored_apgar = factor(benjer$apgar_5)

x = sparse.model.matrix(~ (mothers_age + fathers_age + 
                             mothers_age_2 + fathers_age_2 + 
                             mothers_status + in_hospital + prenatal_care_month +
                             maternal_bmi + father_unkown) *
                          ( mothers_race + fathers_race + 
                             mothers_education + fathers_education + 
                             mothers_status + in_hospital + prenatal_care_month +
                             maternal_bmi + father_unkown), 
                        data=benjer)[,-1]


reg2 <- gamlr(x, apgar, lambda.min.ratio = 0.01,
              family="gaussian")

reg3 <- cv.glmnet(x, factored_apgar, lambda.min.ratio = 0.01,
                  family="multinomial")

summary(reg)

summary(reg2)
BICseg <- which.min(AICc(reg2))
BICseg
reg2
scb.bic <- coef(reg2, s=BICseg)
summary(reg2)[BICseg,]
Baicc <- coef(reg2)
scb.bic
reg2$lambda[which.min(AICc(reg2))]
dhat <- predict(reg2, x, type="response") 
cor(drop(dhat),apgar)^2
AICseg <- which.min(AICc(reg2))
summary(reg2)[AICseg,]

summary(reg3)
plot(reg3) # CV error; across top avg # nonzero across classes
## plot the 6 sets of coefficient paths for each response class
plot(reg3$glm, xvar="lambda") 
reg3
## extract coefficients
B  <- coef(reg3, select="min")
B # it's a list of coefficients, 1 matrix per glass type.
## combine into a matrix
B <- do.call(cBind, B)
## annoyingly, column names are dropped
colnames(B) <- levels(factored_apgar) # add them back
B
### fit plots: plot p_yi distribution for each true yi
# use predict to get in-sample probabilities
probfgl <- predict(reg3, x, type="response")
# for some reason glmnet gives back predictions as an nxKx1 array. 
# use drop() to make it an nxK matrix
probfgl <- drop(probfgl)
# get the probs for what actually happened
# note use of a matrix to index a matrix! 
# gives back the [i,j] entry of probfgl for each row of index matrix 
# so, here, that's the probability of true class for each observation
trueclassprobs <- probfgl[cbind(1:length(factored_apgar), factored_apgar)] 
## plot true probs, with varwidth to have the box widths proportional to response proportion.
plot(trueclassprobs ~ factored_apgar, col="lavender", varwidth=TRUE,
     xlab="apgar type", ylab="prob( true class )") 

## classification
## looking at Head vs all others (using 0.9 rule from slides)
headclass <- probfgl[,'1'] > .0066
sum(headclass)

## or, straightforward max prob classification
## apply which.max prob for every row
class <- levels(factored_apgar)[apply(probfgl,1,which.max)] 
cbind(class,as.character(factored_apgar)) 


hd <- benjer$cg_heart_defect
reg4 <- cv.glmnet(x, hd, lambda.min.ratio = 0.01,
                  family="multinomial")
B  <- coef(reg4, select="min")
B # it's a list of coefficients, 1 matrix per glass type.
## combine into a matrix
B <- do.call(cBind, B)
## annoyingly, column names are dropped
colnames(B) <- levels(hd) # add them back
B
### fit plots: plot p_yi distribution for each true yi
# use predict to get in-sample probabilities
probfgl <- predict(reg4, x, type="response")
# for some reason glmnet gives back predictions as an nxKx1 array. 
# use drop() to make it an nxK matrix
probfgl <- drop(probfgl)
# get the probs for what actually happened
# note use of a matrix to index a matrix! 
# gives back the [i,j] entry of probfgl for each row of index matrix 
# so, here, that's the probability of true class for each observation
trueclassprobs <- probfgl[cbind(1:length(hd), hd)] 
## plot true probs, with varwidth to have the box widths proportional to response proportion.
plot(trueclassprobs ~ hd, col="lavender", varwidth=TRUE,
     xlab="cg heart defect type", ylab="prob( true class )") 

## classification
## looking at Head vs all others (using 0.9 rule from slides)
summary(probfgl[,'N'])
summary(probfgl[,'Y'])

headclass <-probfgl[,'Y'] > .03
sum(headclass)
sum(headclass & hd == "Y")
sum(headclass & hd == "Y") / sum(headclass)

source("../utilities/roc.R")
roc(p=probfgl[,'Y'], y=hd=="Y", main="heart defect roc")
reg4
