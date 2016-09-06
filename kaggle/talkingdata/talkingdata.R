library(gamlr)
#library(xgboost)
library(plyr)
#library(ggplot2)
#library(scales)
#library(formattable)
#library(caret)
library(devtools)
install_github('jakewalker56/jml', force = TRUE)


#my local data store
setwd("~/github/ml-lab/kaggle/talkingdata")

max_rows <- 1000000
events <- merge(
  merge(
    merge(read.csv("events.csv", nrows=max_rows), 
          read.csv("phone_brand_device_model.csv", nrows=max_rows)),
    read.csv("app_events.csv", nrows=max_rows)),
  merge(read.csv("app_labels.csv", nrows=max_rows), 
        read.csv("label_categories.csv", nrows=max_rows)))

test <- merge(read.csv("gender_age_test.csv", nrow=max_rows), events)
train <- merge(read.csv("gender_age_train.csv", nrow=max_rows), events)
sample <- read.csv("sample_submission.csv")
events <- NULL

summary(train)
summary(test)
summary(sample)
nrow(train)

sort(unique(factor(train$group)))
#[1] F23-   F24-26 F27-28 F29-32 F33-42 F43+  
#[7] M22-   M23-26 M27-28 M29-31 M32-38 M39+  

#exploratory
hist(train$age)
mean(log(train$age))
hist(log(train$age)-3.4)
head(sort((train$age)),100)
head(sort((train$age), decreasing=TRUE),100)

#Remove any training row where age < 10
train <- train[train$age > 9,]
#Remove any training row where age > 89
train <- train[train$age < 90,]

summary(train)

brand_breakdown <- ddply(train,~phone_brand,summarise,mean=mean(gender == 'M'),count=length(gender))
brand_breakdown <- brand_breakdown[sort(brand_breakdown$count, index.return=TRUE)$ix,]

model_breakdown <- ddply(train,~device_model,summarise,mean=mean(gender == 'M'),count=length(gender))
model_breakdown <- model_breakdown[sort(model_breakdown$count, index.return=TRUE)$ix,]

agg_breakdown <- ddply(train,~device_model + phone_brand,summarise,mean=mean(gender == 'M'),count=length(gender))
agg_breakdown <- agg_breakdown[sort(agg_breakdown$count, index.return=TRUE)$ix,]

hist(model_breakdown$mean)

train_sparse <- sparse.model.matrix(
  gender ~ app_id * is_active + event_id + longitude + latitude + device_model, 
  data=train)[,-1]

model1 <- cv.gamlr(x=train_sparse, y=train$gender, 
                   lmr=1e-8, family="binomial")

jml::multi_log_loss(act=
                      data.frame(train$gender == 'M'), 
                    pred=
                      rbind(
                        predict(model1, newdata=train_sparse, select="1se", type="response")[,1]
                      )
)

#...what if we just predict the average?
jml::multi_log_loss(act=
                      data.frame(train$gender == 'M'), 
                    pred=
                      rbind(rep(mean(train$gender == 'M'), nrow(train)))
)

coef(model1, select="1se")

#...what if we predict the average of the device type?
jml::multi_log_loss(act=
                      data.frame(train$gender == 'M'), 
                    pred=
                      rbind(model_breakdown[match(train$device_model, model_breakdown$device_model),"mean"])
)
