library(gamlr)
library(xgboost)
library(plyr)
library(ggplot2)
library(scales)
library(formattable)
library(caret)

#my local data store
setwd("~/Desktop/civis_exam/ads_exam_data")

individual <- read.csv("individual_data.csv")
household <- read.csv("household_data.csv")
registration <- read.csv("registration_status.csv")

#some brief data exploration
summary(individual)
summary(household)
summary(registration)
#age distribution is very odd.
hist(individual$age, breaks=seq(from=0,to=50,length.out=51))


#clean data

#gender coded in multiple ways
individual[individual$gender=="FEMALE",]$gender <- "F"
individual[individual$gender=="Female",]$gender <- "F"
individual[individual$gender=="MALE",]$gender <- "M"
individual[individual$gender=="Male",]$gender <- "M"
#refactor to remove empty factors
individual$gender <- factor(individual$gender)


##Section 1

# Part 1
#1. Create a variable indicating whether each person is a child 
#   or adult (18 and older). Call it adult. What percent of the 
#   sample is adults?

individual$adult <- individual$age >= 18
#remove NA level from adult and change to factor
individual$adult <- factor(individual$adult)
percent_adults <- sum(individual$adult == TRUE)/nrow(individual)
print(percent_adults)
# => 76.46109%

#2. What is the average number of adults per household? What is 
#   the average number of children per household?
average_adults_per_household <- sum(individual$adult == TRUE) / nrow(household)
average_children_per_household <- sum(!individual$adult == TRUE) / nrow(household)
print(average_adults_per_household)
#=> 1.6092
print(average_children_per_household)
#=> 0.4954

#3. What percent of adults are college graduates? What percentage 
#   of people are college graduates? Do you find the coding of 
#   this variable (college graduate) reliable?

sum(individual[individual$adult == TRUE,]$is_college_graduate)/nrow(individual[individual$adult == TRUE,])
sum(individual[individual$adult != TRUE,]$is_college_graduate)/nrow(individual[individual$adult != TRUE,])
sum(individual$is_college_graduate)/nrow(individual)

#college graduation appears to be independent of age. Clearly bad data
hist(individual[individual$is_college_graduate == TRUE,]$age)
hist(individual[individual$is_college_graduate != TRUE,]$age)

#4. Create a variable that indicates the type of household: 
#   “single male head of household,” 
#    “single female head of household,” 
#    “married couple,” or “unmarried couple.” 
#    What percent of households are of each type?

# count number of adults in each household and verify it's never more than two or less than one
# the below code relies on this assumption to figure out family structure
household$total_adults <- plyr::count(individual[individual$adult == TRUE,], vars=c("hh_id"))[,"freq"]
summary(household$total_adults)
if(max(household$total_adults) > 2 || min(household$total_adults) < 1 ){
  print("Household structure does not match assumptions! Do not trust household type var")
}

#now figure out the family structure of the household
#note that we assume all adults in a household are parents
#this assumption would likely not work on real world data, but based
#on the exploration above seems reasonable with this data set
household$type <- apply(household, 1, function(h, individual){
  #here we dynamically apply a function to each household to figure out the family structure
  
  #I don't love the performance here.  I suspect it's because we're passing in the full
  #individual dataframe for each of the household rows.  If we had a sufficiently large
  #dataset that this became and issue I would start looking to optimize there
  parents = individual[individual$adult == TRUE &
                         individual$hh_id == as.numeric(h["hh_id"]),]
  if (nrow(parents) == 1) {
    if(parents[1,]$is_head_of_household == TRUE){
      if(parents[1,]$gender == "M") {
        return("single male head of household")
      } else {
        return("single female head of household")
      }
    } else {
      #the only adult is not the head of household... 
      #we don't know how to deal with that
      print("adult is not head of household:")
      print(parents[1,])
      return(NA)
    }
  } else if (nrow(parents) == 2) {
    if(nrow(parents[parents$married == "Married",]) == 2){
      #two parents who are married
      return("married couple")
    } else {
      #at least one of the parents is not married
      return("unmarried couple")
    }
  } else {
    #not 1 or 2 parents... we don't know what to do in this case
    #Note that we already ruled out this case in our above exploration, 
    #so we should never hit this code
    print("wrong number of parents:")
    print(parents)
    return(NA)
  }
}, individual)

#factorize
household$type <- factor(household$type)

#results look ok
summary(household$type)

sum(household$type == "married couple")/nrow(household)
sum(household$type == "unmarried couple")/nrow(household)
sum(household$type == "single male head of household")/nrow(household)
sum(household$type == "single female head of household")/nrow(household)

png(filename=paste(sep="", "percentage_household_type", ".png"), width=900, height=300)
  plot(household$type, ylim=c(0,nrow(household)/2), yaxt="n")
  axis(2, at=seq(from=0,to=1,length.out=5) * nrow(household), lab=paste(seq(from=0,to=100,length.out=5),"%"), las=TRUE)
dev.off()
  
## Part 2
#Use the categories of households you created in Part 1 
#(“single male,” “single female,” “married couple,” and 
#“unmarried couple”). 

#If you were unable to create these categories in question 4 above, 
#you can categorize households by whether they contain couples 
#(married or unmarried) (“couples”) and households that do not 
#contain couples (“singles”).

#1. Write a 1-2 paragraph comparing households’ socio-economic 
#   status across the categories.
summary(household)
png(filename=paste(sep="", "income by household type", ".png"), width=900, height=300)
  boxplot(household$hh_income ~ household$type)
dev.off()
plot(household$type, household$tercile_of_census_tract_income )

#this income distribution is way too consistent, and it's normal in linear
#space, which is not what real wage data looks like.  I'm pretty confident
#this is an entirely made up data set
hist(household[household$type == "married couple",]$hh_income)
hist(household[household$type == "unmarried couple",]$hh_income)
hist(household[household$type == "single male head of household",]$hh_income)
hist(household[household$type == "single female head of household",]$hh_income)
hist(household$hh_income)

# only married people own their home, and ALL married people own their home?  
# That's weird...
plot(household$type, factor(household$is_owner_of_home))
summary(household[household$is_owner_of_home == TRUE,])

#census tract tercile is the same for every group.  That's weird... perhaps I 
#don't quite understand what census tract income means
plot(household$type, household$tercile_of_census_tract_income)

#2. Make a table of summary statistics that illustrate your 
#   comparison. Include it below.
stat_table <- data.frame(Type=unique(household$type))
stat_table[,"Income mean"]<- rep(0,length(unique(household$type)))
stat_table[,"Income sd"]<- rep(0,length(unique(household$type)))
stat_table[,"Home ownership rate"]<- rep(0,length(unique(household$type)))
stat_table[,"Urban rate"]<- rep(0,length(unique(household$type)))
for(type in stat_table$Type){
  stat_table[stat_table$Type == type,"Income mean"] = mean(household[household$type == type,]$hh_income)
  stat_table[stat_table$Type == type,"Income sd"] = sd(household[household$type == type,]$hh_income)
  stat_table[stat_table$Type == type,"Home ownership rate"] = sum(household[household$type == type,]$is_owner_of_home)/nrow(household[household$type == type,])
  stat_table[stat_table$Type == type,"Urban rate"] = sum(household[household$type == type,]$is_urban)/nrow(household[household$type == type,])
}
stat_table

formattable(stat_table, list(
  "Income mean" = formatter("span", x ~ sprintf("$%.1fk", x/1000)),
  "Income sd" = formatter("span", x ~ sprintf("$%.1fk", x/1000)),
  "Home ownership rate" = formatter("span", x ~ sprintf("%.0f%%", x * 100)),
  "Urban rate" = formatter("span", x ~ sprintf("%.0f%%", x * 100))
))

#3. Create a client-ready graph to illustrate your comparison. 
#   Include it below.
png(filename=paste(sep="", "income by household type", ".png"), width=900, height=500)
qplot(x=hh_income, 
      data=household, 
      fill=type,
      bins = 50) + 
  geom_histogram(colour = 'black', bins=50) +
  scale_x_continuous(labels = dollar) +
  xlab("Income") +
  ylab("Number of Households") +
  theme(axis.line = element_blank(), #element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
dev.off()

## Part 3
#Please build a model that produces an individual level prediction of a 
#person’s probability to register as a Democrat. In Dataset C, approximately 
#the first 10,000 individuals have known registration status while for the 
#approximately other 10,000 individuals, registration is unknown. 

#Use Datasets A, B, and/or C to build individual level probability predictions 
#for the people for whom registration is unknown.

#You are free to generate any additional variables or to use any that you have 
#already generated for Part 1 and 2. You are free to use any modeling approach.
ir <- merge(individual, registration,by="person_id")
hir <- merge(household, ir, by="hh_id")

#don't want to train on hh id
hir <- hir[,-grep("hh_id",colnames(hir))]

#total_adults in household is redundant information with type
hir <- hir[,-grep("total_adults",colnames(hir))]

#as noted above, is_owner_of_home is redundant information with type
hir <- hir[,-grep("is_owner_of_home",colnames(hir))]

#we're going to exclude is_college_graduate, because we noted up above that that
#feild is improperly coded. Note that this variable may indeed have predictive power, 
#but since we don't know what it's actually representing it is very dangerous 
#to use for out-of-sample prediction. it also would make any model much harder 
#to interpret
hir <- hir[,-grep("is_college_graduate",colnames(hir))]

#save ids for output purposes later, but don't train on them
hir_ids <- hir[,grep("person_id",colnames(hir))]
hir <- hir[,-grep("person_id",colnames(hir))]

#separate train and test
train_hir <- hir[!is.na(hir$is_registered_democrat),]
test_hir <- hir[is.na(hir$is_registered_democrat),]

#separate out response data
train_hir_target <- train_hir[,grep("is_registered_democrat",colnames(train_hir))]
train_hir <- train_hir[,-grep("is_registered_democrat",colnames(train_hir))]
test_hir <- test_hir[,-grep("is_registered_democrat",colnames(test_hir))]

#let's check and make sure our train data looks ok
summary(train_hir)
colnames(train_hir)

#create train and validation sets for train/test comparison
in_sample = createDataPartition(train_hir_target, p=0.75, list=F)
train_hir_sparse <- sparse.model.matrix(
  train_hir_target[in_sample] ~ . , 
  data=train_hir[in_sample,])[,-1]
validate_hir_sparse <- sparse.model.matrix(
  train_hir_target[-in_sample] ~ . , 
  data=train_hir[-in_sample,])[,-1]

#train a cross-validated logistic regression model with l1 regularization

#lasso logistic regression models assume log-odds are a linear function of the inputs, with
#a regularization parameter for the absolute value of the coefficients.  However,
#when we look at our actual data, we can see that people under 18 are literally never
#registered democrats. This is a fact we expect to be consistent because of real world
#constraints, and therefore applicable out-of-sample.  That means the true coefficient 
#associated with a non-adult is -Inf, but regularization means we're going to 
#meaningfully mis-predict probabilities for children.  By allowing adultTRUE to be
#a free parameter, we are telling gamlr to allow it to fit whatever coefficient the 
#data merits, and ignore the regularization penalty
colnames(train_hir_sparse)
train_logistic_reg <- cv.gamlr(x=train_hir_sparse, y=train_hir_target[in_sample], 
                           free=c("adultTRUE"), lmr=1e-4,
                           family="binomial")
coef(train_logistic_reg, select="1se")
#In this world, it looks like hte only thing that matters is urban status,
#whether or not the individual is Black, and whether or not the person is rich compared
#to census tract peers.
#this is a really nice, simple, interpretable model

#we can also train a standardized model, which standardizes the inputs based on
#their standard deviation.  This makes low-occurence binary variable (things that
#are usually zero but occasionally 1) pop a lot more often, because the standard deviation
#is very low, so the 1's look really big after standardization (same goes for 0's if
#the column is mostly 1's, obviously)
train_logistic_reg_standardized <- cv.gamlr(x=train_hir_sparse, y=train_hir_target[in_sample],
                           free=c("adultTRUE"), lmr=1e-4, standardize=TRUE, 
                           family="binomial")
coef(train_logistic_reg_standardized, select="1se")

#try a gradient boosted tree
xgb_train <- xgb.DMatrix(train_hir_sparse, label=train_hir_target[in_sample])
xgb_validate <- xgb.DMatrix(validate_hir_sparse, label=train_hir_target[-in_sample])
#find best nrounds for given tuning params
bst <- xgb.cv(eval_metric="logloss", nrounds=1000,
                      data=xgb_train,
                      nfold=5, objective = "binary:logistic",
                      max_depth = 3, 
                      min_child_weight = 1, 
                      subsample=0.55, 
                      eta = 0.01, 
                      colsample_bytree = 0.55,
                      lambda=1,
                      alpha=0,
                      booster="gbtree",
                      gamma=0.1)

#now train the model based on the optimal nrounds from above. Looks to be around 700
train_xgb_reg <- xgb.train(eval_metric="logloss", nrounds=which.min(bst$test.logloss.mean + bst$test.logloss.std),
       data=xgb_train,
       nfold=5, objective = "binary:logistic",
       max_depth = 3, 
       min_child_weight = 1, 
       subsample=0.55, 
       eta = 0.01, 
       colsample_bytree = 0.55,
       lambda=1)

#use the multi los loss function from kaggle to establish a baseline comparison between
#our different methods (https://www.kaggle.com/wiki/LogarithmicLoss)
multi_log_loss <- function(act, pred)
{
  eps = 1e-15;
  nr <- nrow(pred)
  pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
  pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
  ll = sum(act*log(pred) + (1-act)*log(1-pred))
  ll = ll * -1/(nrow(act))      
  return(ll);
}
multi_log_loss(act=data.frame(train_hir_target[-in_sample]), pred=rbind(predict(train_logistic_reg, newdata=validate_hir_sparse, select="1se", type="response")[,1]))
multi_log_loss(act=data.frame(train_hir_target[-in_sample]), pred=rbind(predict(train_logistic_reg_standardized, newdata=validate_hir_sparse, select="1se", type="response")[,1]))
multi_log_loss(act=data.frame(train_hir_target[-in_sample]), pred=rbind(predict(train_xgb_reg, newdata=xgb_validate)))

#These all look pretty simillar, but non-standardized logistic regression performed 
#the best in my sample set.  We'll retrain our model from scratch
#on the full data to get a slightly better fit, then use that to predict on test
final_hir_sparse <- sparse.model.matrix(
  train_hir_target ~ . , 
  data=train_hir)[,-1]

final_logistic_reg <- cv.gamlr(x=final_hir_sparse, y=train_hir_target, free=c("adultTRUE"), 
                            lmr=1e-4, family="binomial")
coef(final_logistic_reg)
submit_hir_sparse <- sparse.model.matrix(
  ~ . , 
  data=hir[,-grep("is_registered_democrat",colnames(hir))])[,-1]


out = data.frame(person_id=hir_ids,score=predict(
  train_logistic_reg, newdata=submit_hir_sparse, select="1se", type="response")[,1])
summary(out)
write.csv(out, "../jakewalker_part3_scores.csv", row.names=FALSE)