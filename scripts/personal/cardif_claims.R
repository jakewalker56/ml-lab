library("devtools")
library(caret)
library(mlbench)
library(gamlr)
install_github('jakewalker56/jml', force = TRUE)
#install.packages("h2o")
#install.packages("xgboost")
library(xgboost)
library(h2o)
library(jml)

setwd("~/github/ml-lab/data")
train = read.csv("cardif_claims.csv")
test = read.csv("cardif_claims_test.csv")

traintest <- rbind(train, cbind(target=0,test))

clean_train_data <- function(data, 
                             include_numeric_values=TRUE,
                             include_missing_numeric_values = FALSE,
                             convert_factors_to_numeric=FALSE, 
                             factor_level_max=998,
                             pca_level_max=40,
                             factor_cols){
  #clean up data
  clean_data <- data[,-grep("ID",colnames(data))]
  clean_data <- clean_data[,-grep("target",colnames(clean_data))]

  clean_data_numeric <- clean_data[, -match(factor_cols, names(clean_data))]
  clean_data_missing <- data.frame()
  for(di in colnames(clean_data_numeric)) {
    d_missing = vector()
    d <- clean_data_numeric[,di]
    if(!is.numeric(d)){
      if(is.factor(d)) {
        #if the value is NA, add a missing column
        d_missing <- is.na(d)
        clean_data_numeric[is.na(d),di] <- -1
      }
    } else {
      #this is numeric, but it could still have NA's
      d_missing <- is.na(d)
      clean_data_numeric[is.na(d),di] <- -1
    }
    if(sum(d_missing, na.rm=TRUE) > 1) {
      #there are some missing values- record it!
      if(nrow(clean_data_missing) == 0){
        #colnames(d_missing) <- paste(di,"missing",sep="")
        clean_data_missing <- data.frame(d_missing)
        colnames(clean_data_missing) <- c(paste(di,"missing",sep=""))
      } else {
        clean_data_missing[,paste(di,"missing",sep="")] <- d_missing
      }
    }
  }
  
  #now that we've found the missing vals, convert it
  clean_data_numeric <- convert_to_numeric(clean_data_numeric)
  
  clean_data_factor <- naref(clean_data[, match(factor_cols, names(clean_data))])
  if(convert_factors_to_numeric){
    print("Replacing categorical variables with integers")
    for (f in factor_cols) {
      if (class(clean_data_factor[[f]])=="character") {
        levels <- unique(c(clean_data_factor[[f]]))
        clean_data_factor[[f]] <- as.integer(factor(clean_data_factor[[f]], levels=levels))
      }
    }
  } else {
    clean_data_factor <- (sapply(factor_cols,function(x, cleanme, limit){
      temp <- cleanme[,x]
      if (length(table(temp)) > limit){
        #more than n factors, trim to the n most common
        #this is not a great filter... we'd prefer to know exactly how impactful all of these are
        temp[!(as.numeric(temp) %in% order(table(temp), decreasing = TRUE)[1:limit])] <- NA
      }
      return(factor(temp))
    }, clean_data_factor, factor_level_max))
    clean_data_factor = naref(data.frame(clean_data_factor))
  }
  
  clean_data_final = data.frame(clean_data_factor)
  if(include_numeric_values){
    clean_data_final = cbind(clean_data_final, data.frame(clean_data_numeric))
  }
  if(include_missing_numeric_values){
    clean_data_final = cbind(clean_data_final, clean_data_missing)
  }
  
  if(pca_level_max > 0){
    #run PCA for dimensional reduction
    pca <- prcomp(clean_data_numeric, scale=TRUE)
    clean_data_pca <- predict(pca)[,1:pca_level_max]
    #build logistic regression
    clean_data_final = cbind(clean_data_final, clean_data_pca)
  }
  return(clean_data_final)
}

clean_test_data <- function( data, 
                             train_data,
                             pca,
                             include_numeric_values=TRUE,
                             convert_factors_to_numeric=FALSE, 
                             factor_level_max=1000,
                             pca_level_max=40,
                             factor_cols){
  #clean up data
  clean_data <- data[,-grep("ID",colnames(data))]
  clean_data_numeric <- convert_to_numeric(clean_data[, -match(factor_cols, names(clean_data))])
  clean_data_factor <- naref(clean_data[, match(factor_cols, names(clean_data))])
  if(convert_factors_to_numeric){
    print("Replacing categorical variables with integers")
    for (f in factor_cols) {
      if (class(clean_data_factor[[f]])=="character") {
        levels <- unique(c(clean_data_factor[[f]]))
        clean_data_factor[[f]] <- as.integer(factor(clean_data_factor[[f]], levels=levels))
      }
    }
  } else {
    clean_data_factor <- (sapply(factor_cols,function(x, cleanme, levels_from){
      temp <- factor(cleanme[,x])
      #get rid of levels that exist in test but not in train
      temp[!(temp %in% levels(levels_from[,x]))] <- NA
      #need to make sure levels are consistent between test and train
      levels(temp) <- levels(levels_from[,x])
      return(factor(temp))
    }, clean_data_factor, train_data[,factor_cols]))
    clean_data_factor = naref(data.frame(clean_data_factor))
    
  }
  
  clean_data_final = data.frame(clean_data_factor)
  if(include_numeric_values){
    clean_data_final = cbind(clean_data_final, data.frame(clean_data_numeric))
  }
  
  if(pca_level_max > 0){
    #run PCA for dimensional reduction
    clean_data_pca = predict(pca,newdata=data.frame(clean_data_numeric))[,1:pca_level_max]
    clean_data_final = cbind(clean_data_final, clean_data_pca)
  }
  return(clean_data_final)
}

train_logistic_regression <- function(train_target, train_clean, in_sample){
  gamlr_train_in_sample <- sparse.model.matrix(
    train_target[in_sample] ~ . , 
    data=train_clean[in_sample,])[,-1]
  
  gamlr_train_out_sample <- sparse.model.matrix(
    train_target[-in_sample] ~ . , 
    data=train_clean[-in_sample,])[,-1]
  
  print("training...")
  old = Sys.time()
  full <- cv.gamlr(x=gamlr_train_in_sample, y=train_target[in_sample], lmr=1e-4, family="binomial")
  print(Sys.time() - old)
  
  print("predicting...")
  
  print("in sample")
  PredictedProb = 1/(1+exp(-predict(full, newdata=gamlr_train_in_sample, select="1se", type="link")))
  print(multi_log_loss(act=data.frame(train_target[(in_sample)]), pred=rbind(PredictedProb)))
  
  if(length(in_sample) < length(train_target)) {
    print("out of sample")
    PredictedProb = 1/(1+exp(-predict(full, newdata=gamlr_train_out_sample, select="1se", type="link")))
    print(multi_log_loss(act=data.frame(train_target[(-in_sample)]), pred=rbind(PredictedProb)))
  }
  #visualize ROC curve for in-sample
  roc(1/(1+exp(-predict(full, newdata=gamlr_train_in_sample, select="min", type="link"))), train_target[in_sample])
  
  #predict test set here if applicable
  #some levels appeared in the train set but not in the test set- set those to zero!
  # for(missing in rownames(coef(full))[!(rownames(coef(full)) %in% colnames(gamlr_test))][-1])
  # {
  #   print(missing)
  #   a <- matrix(0, nrow=nrow(gamlr_test), ncol=1); 
  #   gamlr_test <- cbind2(gamlr_test, a)
  # }
  # PredictedProb = 1/(1+exp(-predict(full, newdata=gamlr_test, select="1se", type="link")))
  # out = data.frame(ID=test$ID,PredictedProb=PredictedProb[,1])
  # write.csv(out, "../output/cardif_claims.csv", row.names=FALSE)
  # 
  return(full)
}

train_caret_nn <- function(train_target, train_clean, in_sample,
                           my.grid, iterations, trace=TRUE){
  print("training...")
  old = Sys.time()
  fit <- train(factor(train_target[in_sample]) ~ ., data = train_clean[in_sample,],
               method = "nnet", maxit = iterations, tuneGrid = my.grid, trace = trace) #, linout = 1) 
  print(Sys.time() - old)
  
  print("predicting...")
  print("in sample")
  pred_in <- predict(fit, newdata = train_clean[in_sample,], type="prob")
  multi_log_loss(act=data.frame(train_target[in_sample]), pred=rbind(pred_in[,"1"]))
  
  if(length(in_sample) < length(train_target)) {
    print("out of sample")
    pred_out <- predict(fit, newdata = train_clean[-in_sample,], type="prob")
    multi_log_loss(act=data.frame(train_target[-in_sample]), pred=rbind(pred_out[,"1"]))
  }
  #predict test set here if applicable
  # PredictedProb =  predict(fit, newdata = test_final, type="prob")
  # out = data.frame(ID=test$ID,PredictedProb=PredictedProb[,"1"])
  # write.csv(out, "../output/cardif_claims.csv", row.names=FALSE)
  return(fit)
}

train_h2o_nn <- function(train_target, train_clean, in_sample,
                         activation="RectifierWithDropout",
                         input_dropout_ratio=0.1,
                         hidden_dropout_ratios = c(0.2,0.2),
                         balance_classes = TRUE,
                         hidden = c(100,100),
                         momentum_stable = 0.99,
                         l1=0.00001,
                         l2=0.0000,
                         epochs = 500
                         ){
  #this is WAY faster than caret

    train_h2o = as.h2o(cbind(as.factor(train_target[in_sample]), train_clean[in_sample,]))
  test_h2o = as.h2o(cbind(as.factor(train_target[-in_sample]), train_clean[-in_sample,]))
  
  print("training...")
  old = Sys.time()
  h2o_model =
    h2o.deeplearning(x = 2:ncol(train_h2o),  # column numbers for predictors
                     y = 1,   # column number for label
                     training_frame = train_h2o, # data in H2O format
                     activation = activation, # algorithm
                     input_dropout_ratio = input_dropout_ratio, # % of inputs dropout
                     hidden_dropout_ratios = hidden_dropout_ratios, # % for nodes dropout
                     balance_classes = balance_classes, 
                     hidden = hidden, # two layers of 100 nodes
                     momentum_stable = momentum_stable,
                     l1=l1,
                     l2=l2,
                     nesterov_accelerated_gradient = T, # use it for speed
                     epochs = epochs) # no. of epochs
  
  print(Sys.time() - old)
  
  print("predicting...")
  print("in sample")
  pred_in_h2o <- as.data.frame(h2o.predict(h2o_model, train_h2o, type="prob"))
  print(multi_log_loss(act=data.frame(train_target[in_sample]), pred=rbind(pred_in_h2o[,"p1"])))
  
  if(length(in_sample) < length(train_target)) {
    print("out_of_sample")
    pred_out_h2o <- as.data.frame(h2o.predict(h2o_model, test_h2o, type="prob"))
    print(multi_log_loss(act=data.frame(train_target[-in_sample]), pred=rbind(pred_out_h2o[,"p1"])))
  }
  return(h2o_model)
}

train_xgb <- function(train_target, train_clean, test_target = NULL, test_clean = NULL, bst, ...){
  xgb_train <- sparse.model.matrix(
    train_target ~ . , 
    data=train_clean)[,-1]
  
  dtrain <- xgb.DMatrix(xgb_train, label=train_target)
  if(!is.null(test_target) && !is.null(test_clean)){
     xgb_test <- sparse.model.matrix(
       test_target ~ . , 
       data=test_clean)[,-1]
     
    dtest <- xgb.DMatrix(xgb_test, label=test_target)
  }
  
  build_params = list("objective" = "binary:logistic"
                      , "eval_metric" = "logloss"
                      , 'eta' = bst$eta
                      , 'max.depth' = bst$max_depth
                      , 'min_child_weight' = bst$min_child_weight
                      , 'subsample' = bst$subsample
                      , 'colsample_bytree' = bst$colsample_bytree)
  nrounds = bst$nrounds
  
  old = Sys.time()
  print("training...")
  
  build_params <- append(build_params, list(...))
  
  if(!is.null(test_target) && !is.null(test_clean)){
    watchlist <- list(train = dtrain, test = dtest)
  } else {
    watchlist <- list(train = dtrain)
  }
  
  xgb_model <- xgb.train(param=build_params, data = dtrain, nrounds=nrounds, watchlist=watchlist, 
                         verbose = 2)
  
  # xgb_model <- xgboost(
  #     data=xgb_train,
  #     label = train_target,
  #     params = build_params,
  #     verbose=2,
  #     nrounds = nrounds,
  #     ...
  #   )

  print(Sys.time() - old)
  print("predicting")
  print("full sample")
  pred_in_xgb <- as.data.frame(predict(xgb_model, xgb_train))[,1]
  print(multi_log_loss(act=data.frame(train_target), pred=rbind(pred_in_xgb)))
  
  return(xgb_model)
}
######Begin Script

#constants for data cleaning
pca_level_max = 0
factor_level_max = 998
train_sample_factor = 1.1
include_numeric_values=TRUE
include_missing_numeric_values=FALSE
convert_factors_to_numeric=TRUE
factor_cols = c("v3", "v22", "v24", "v30", "v31", "v47", "v52", "v56", "v66", "v71", 
                "v74", "v75", "v79", "v91", "v107", "v110", "v112", "v113", "v125")
train_target <- train[,grep("target",colnames(train))]
in_sample = createDataPartition(train_target, p=1/train_sample_factor, list=F)
localH2O = h2o.init(max_mem_size = '6g', # use 6GB of RAM of *GB available
                    nthreads = -1) # use all CPUs
?data.frame

clean_train_data_result <- clean_train_data(data=traintest,
                                include_numeric_values=include_numeric_values,
                                include_missing_numeric_values=include_missing_numeric_values,
                                convert_factors_to_numeric=convert_factors_to_numeric, 
                                factor_level_max=factor_level_max,
                                pca_level_max=pca_level_max,
                                factor_cols=factor_cols)
train_clean <- clean_train_data_result[1:nrow(train),]
test_clean <- clean_train_data_result[(nrow(train)+1):nrow(clean_train_data_result),]
summary(train_clean)

colnames(train_clean)
summary(train_clean$v2missing)
#need to share the same pca across the two data sets
pca <- train_tuple[[2]]
# summary(pca)
# test_clean <- clean_test_data(data=test, 
#                               train_data=train_clean,
#                               pca=pca,
#                               include_numeric_values=include_numeric_values,
#                               convert_factors_to_numeric=convert_factors_to_numeric, 
#                               factor_level_max=factor_level_max,
#                               pca_level_max=pca_level_max,
#                               factor_cols=factor_cols)

logistic_model_1 = train_logistic_regression(train_target, train_clean, in_sample)
  
caret_nn_model_1 = train_caret_nn(train_target=train_target, train_clean=train_clean, 
                                  in_sample = in_sample, 
                                  my.grid = expand.grid(.decay = c(5.0), .size = c(5)),
                                  iterations = 2000)

#try h20
?h2o.deeplearning
h2o_nn_model_1 = train_h2o_nn(train_target=train_target, 
                              train_clean=train_clean, 
                              in_sample=in_sample,
                              activation="RectifierWithDropout",
                              input_dropout_ratio=0.1,
                              hidden_dropout_ratios = c(0.2,0.2),
                              balance_classes = TRUE,
                              hidden = c(100,100),
                              momentum_stable = 0.99,
                              l1=0.0001,
                              l2=0.00001,
                              epochs = 300)

h2o_nn_model_2 = train_h2o_nn(train_target=train_target, 
                              train_clean=train_clean, 
                              in_sample=in_sample,
                              activation="Rectifier",
                              input_dropout_ratio=0.1,
                              hidden_dropout_ratios = c(0.2,0.2,0.2),
                              balance_classes = FALSE,
                              hidden = c(50,50,50),
                              momentum_stable = 0.99,
                              l1=0.0001,
                              l2=0.00001,
                              epochs = 300
                              )

#try xgboost- gradient boosted trees
xgb_final <- sparse.model.matrix(
  train_target ~ . , 
  data=train_clean)[,-1]

xgb_submit <- sparse.model.matrix(
  ~ . , 
  data=test_clean)[,-1]

xgb_train <- sparse.model.matrix(
  train_target[in_sample] ~ . , 
  data=train_clean[in_sample,])[,-1]

xgb_test <- sparse.model.matrix(
  train_target[-in_sample] ~ . , 
  data=train_clean[-in_sample,])[,-1]

bst <- xg_fit(data=xgb_final, label = train_target, nrounds=1500,
         nfold=5, objective = "binary:logistic",
         max_depth = c(10), 
         min_child_weight = c(1), 
         subsample=c(0.55), 
         eta = c(0.01), 
         colsample_bytree = c(0.55),
         lambda=1,
         alpha=0,
         booster="gbtree",
         gamma=0.1)

bst

xgb_model_1 <- train_xgb(train_clean=train_clean, train_target=train_target,
                         #test_clean = test_clean, test_target = train_target,
                         bst=bst, lambda=1, alpha=0, booster="gbtree", gamma=0.1)

#xgb_model_2 <- train_xgb(train_clean=train_clean[in_sample,], train_target=train_target[in_sample],
#                         test_clean = train_clean[-in_sample,], test_target = train_target[-in_sample],
#                         bst=bst, lambda=1, alpha=0, booster="gbtree", gamma=0.1)

#xgb_model_3 <- train_xgb(train_clean=train_clean[in_sample,], train_target=train_target[in_sample],
#                         #test_clean = train_clean[-in_sample,], test_target = train_target[-in_sample],
#                        bst=bst, lambda=1, alpha=0, booster="gbtree", gamma=0.1)

pred_in_xgb <- as.data.frame(predict(xgb_model_1, xgb_train, ntreelimit = 2000))[,1]
print(multi_log_loss(act=data.frame(train_target[in_sample]), pred=rbind(pred_in_xgb)))

pred_submit_xgb <- as.data.frame(predict(xgb_model_1, xgb_submit))[,1]
out = data.frame(ID=test$ID,PredictedProb=pred_submit_xgb)
write.csv(out, "../output/cardif_claims.csv", row.names=FALSE)

#pred_in_xgb <- as.data.frame(predict(xgb_model_2, xgb_test,  ntreelimit = 2000))[,1]
#print(multi_log_loss(act=data.frame(train_target[-in_sample]), pred=rbind(pred_in_xgb)))

#pred_in_xgb <- as.data.frame(predict(xgb_model_3, xgb_test[,sample(ncol(xgb_test))],  ntreelimit = 2000))[,1]
#print(multi_log_loss(act=data.frame(train_target[-in_sample]), pred=rbind(pred_in_xgb)))

bst2 <- xg_fit(data=xgb_final, label = train_target, nrounds=100,
              nfold=5, objective = "binary:logistic",
              max_depth = c(10), 
              min_child_weight = c(1), 
              subsample=c(0.55), 
              eta = c(0.1), 
              colsample_bytree = c(0.55),
              lambda=1,
              alpha=0,
              booster="gbtree",
              gamma=0.1)


xgb_model_4 <- train_xgb(train_clean=train_clean, train_target=train_target,
                         #test_clean = test_clean, test_target = train_target,
                         bst=bst2, lambda=1, alpha=0, booster="gbtree", gamma=0.1)

head(predict(xgb_model_4, newdata=xgb_submit))
#predict using only the first 10 columns, missing values default to 0
head(predict(xgb_model_4, newdata=xgb_submit[,1:10]))
#predict using the wrong columns, because we ignore column names
head(predict(xgb_model_4, newdata=xgb_submit[,sample(ncol(xgb_submit))]))
     
pred_submit_xgb <- as.data.frame(predict(xgb_model_4, xgb_submit))[,1]
out = data.frame(ID=test$ID,PredictedProb=pred_submit_xgb)
write.csv(out, "../output/cardif_claims.csv", row.names=FALSE)

length(colnames(xgb_train)[!(colnames(xgb_train) %in% colnames(xgb_submit))])
test[test$v79 == "L",]

length(pred_submit_xgb)






bst3 <- xg_fit(data=xgb_train, label = train_target, nrounds=5000,
              nfold=5, objective = "binary:logistic",
              max_depth = c(10), 
              min_child_weight = c(1), 
              subsample=c(0.35), 
              eta = c(0.005), 
              colsample_bytree = c(0.35))
xgb_model_2 <- train_xgb(train_clean=train_clean, train_target=train_target,
                         in_sample=in_sample, params=bst3)
summary(train_clean)
bst4
?xgboost
nrow(train_clean)
nrow(train_target)

?xgb.cv
bst2 <- xg_fit(data=xgb_train, label = train_target, nrounds=200,
               nfold=5, objective = "binary:logistic",
               max_depth = c(5, 10), 
               min_child_weight = c(1), 
               subsample=c(0.55, 0.8), 
               eta = c(0.1, 0.01), 
               colsample_bytree = c(0.55, 0.8)
)

#Claims to see OOS = 0.45846 
bst3 <- xg_fit(data=xgb_train, label = train_target, nrounds=200,
               nfold=5, objective = "binary:logistic",
               max_depth = c(10), 
               min_child_weight = c(1), 
               subsample=c(0.8), 
               eta = c(0.01), 
               colsample_bytree = c(0.8)
)

params = data.frame(
  max_depth=c(10),
  min_child_weight=c(1),
  subsample=c(0.8),
  eta=c(0.01),
  colsample_bytree=c(0.8),
  nrounds=c(200))

xgb_model_2 = train_xgb(train_clean=train_clean, train_target=train_target,
                        params=bst)

#can we do better by merging?

logistic_model_1.expanded <- sparse.model.matrix(
  train_target ~ . , 
  data=train_clean)[,-1]

h2o_nn_model_1.expanded <- as.h2o(cbind(as.factor(train_target), train_clean))

pred1 = predict(logistic_model_1, newdata=logistic_model_1.expanded, type="response")
pred2 = as.vector(predict(h2o_nn_model_1, newdata=h2o_nn_model_1.expanded, type="prob")[,"p1"])
pred3 = as.vector(predict(xgb_model_1, newdata=xgb_train))

print(multi_log_loss(act=data.frame(train_target[-in_sample]), pred=rbind(pred_out_xgb)))

p = merge_predictors(train_target, 
                 pred1, 
                 pred2)
pred_mid_1 = pred1* p + pred2*(1-p)
p = merge_predictors(train_target, 
                     pred_mid_1, 
                     pred3)

pred_mid_2 = pred_mid1* p + pred3*(1-p)
multi_log_loss(act=data.frame(train_target), pred=rbind(pred_mid_2))

xgb_test <- sparse.model.matrix(
   ~ . , 
  data=test_clean)[,-1]
colnames(xgb_train)
colnames(xgb_test)

pred_test_xgb <- as.data.frame(predict(xgb_model_1, xgb_test))[,1]
out = data.frame(ID=test$ID,PredictedProb=pred_test_xgb)
write.csv(out, "../output/cardif_claims.csv", row.names=FALSE)

h2o.shutdown(prompt = F)

#40, 5, 1, min =   0.4853991
#100, 10, 1, min = 0.4832956
#100, 10, 1, 1se = 0.4849478
#40, 2, 1, 1se (pca .^2 only) = 0.4994
#40, 4, 2, 1se = 0.4847641, OOS 0.4894238
#4 minutes to train nn on 10 PCA, 1000 iterations, 1/10 train set
#22 minutes to train nn on 10 PCA + 18 factors (3 levels each) with 2x6 grid?
#36 min to train nn on 40, 4, 10, 1000, .decay = c(0.3, 0.5, 0.7), .size = c(5, 10) OOS = 0.5027 => decay 0.7 size 5 is selected
#32 min to train nn on 40 4 10 2000 decay=c(0.7, 1.0, 2.0), size=5, OOS = 
#36 min to train nn on 40 4 10 2000 decay=c(2, 5, 25), size=5, decay = 5, OOS = 0.4881175
#3.4 hr to train nn on 40 4 1 2000 decay=5, size=5, In Sample = 0.4681777, Test set = 0.47232

#TODO: build neural net?
#TODO: build autoencoder?
#TODO: build random forest?
#TODO: neural net + regression + random forest?  just do a logistic using outputs of others?
#TODO: Use full data set
#TODO: Use non-pca numerics
#TODO: Use more factor levels
#TODO: transform numerics (log, normalize for variance, normalize from 0-1, etc.)
#TODO: boosted nnet?
#TODO: xgboost
#TODO: try logitBoost- http://www.inside-r.org/packages/cran/caTools/docs/LogitBoost
#TODO: try random forest classifier
#TODO: try h2o_fit -> including 3, 4, 5 level networks
#TODO: do something else with NA's in numerics (instead of setting them to 0)
#TODO: blend multiple models
#TODO: convert factors to numerics
#TODO: try different activation functions
#TODO: try tensorflow

