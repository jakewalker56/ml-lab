location = "~/github"
path= "/ml-lab/data/"
setwd(paste(location, path, sep=""))
bat = read.csv("bat.csv")
oj = read.csv("OJ.csv")
adult = read.csv("adult.csv")

#BATmobiles

# NOTES:
#   Cumulative effect means we should consider qtr (possibly qtr*control?)
#   We really care about acc/fuel, not total acc (though 
#     if we caused fuel consumption to go down, that's interesting')
#   to tell if we had en affect, we really care if we changed the TREND 
#     from control to program
#   If we're confident in b1control - b1program !=0, then the answer is yes?
#   In class, he said acc/fuel was useful, but NOT the right thing to care about
#   Increasing variance as a function of X => try the log

summary(bat)
plot(bat$qtr, bat$acc, col=heat.colors(length(bat$fuel))[rank(bat$fuel)], pch=20, xlab="Quarter", ylab="# of Accidents", main="Accidents by quarter and fuel consumption")


library(ggplot2)
?aes
?geom_point
p <- ggplot(bat, aes(qtr,acc,colour=fuel)) +geom_point()
png(filename=paste(sep="", Dropbox, path, "1.1.png"))
p
dev.off()

png(filename=paste(sep="", Dropbox, path, "1.2.png"))
plot(bat$qtr, bat$acc/bat$fuel, type="b", xlab="Quarter", ylab="Acc/Fuel", main="Accidents per unit fuel consumption by quarter")
dev.off()

png(filename=paste(sep="", Dropbox, path, "1.3.png"))
acf(transformed_bat$acc, main="ACF of Accidents")
dev.off()


index = 1:30
control <- bat[index,]
program <- bat[-index,]

plot(control$fuel, control$acc)
plot(control$qtr, control$acc)
plot(control$qtr, control$fuel)

plot(program$fuel, program$acc)
plot(program$qtr, program$acc)
plot(program$qtr, program$fuel)
plot(program$qtr, program$acc/program$fuel)

QR <- 5:52
cos4 <- cos(QR*pi/2)
sin4 <- sin(QR*pi/2)


transformed_bat = data.frame(qtr=bat$qtr[QR])
transformed_bat$acc = bat$acc[QR]
transformed_bat$fuel = bat$fuel[QR]
#transformed_bat$rate = bat$acc[QR]/bat$fuel[QR]
transformed_bat$control = bat$qtr[QR] < 31
#transformed_bat$qtr_adj = bat$qtr[QR] - 31
transformed_bat$last4 <- bat$acc[QR-4]
transformed_bat$last1 <- bat$acc[QR-1]


transformed_model = lm(acc ~ control, data=transformed_bat)
summary(transformed_model)
anova_transformed_model = anova(transformed_model)

ssr = anova_transformed_model$Sum[1]
sse = anova_transformed_model$Sum[2]
sst = ssr + sse
#percent explained by the grouping with control
ssr
sst
ssr / sst
anova_transformed_model


bat_model = lm(acc ~ control + fuel, data=transformed_bat)
summary(bat_model)

bat_model_td = lm(acc ~ qtr * control + fuel + last1 + last4, data=transformed_bat)
summary(bat_model_td)

bat_model_sincos = lm(acc ~ qtr * control + fuel + sin4 + cos4 + last1 + last4, data=transformed_bat)
summary(bat_model_sincos)

#bat_model_man = lm(acc ~ qtr * control + sin4 + cos4 - qtr, data=transformed_bat)
#summary(bat_model_man)

null <- lm(acc~ 1, data=transformed_bat)
full <- lm(acc ~ . + .^2 + sin4 + cos4, data=transformed_bat)

fwd <- step(null, scope=formula(full), direction="forward", k=log(length(transformed_bat$acc)))
summary(fwd)

# fwd_man2 <- lm(acc ~ fuel * control + sin4 + last4 - control, data=transformed_bat)
# summary(fwd_man2)

fwd_man <- lm(acc ~ qtr * control + sin4 + last4 - qtr -control, data=transformed_bat)
summary(fwd_man)

bat_model.BIC <- c(control=extractAIC(transformed_model, k=log(length(bat$acc)))[2],
                   linear=extractAIC(bat_model, k=log(length(bat$acc)))[2],
                   time_dependant=extractAIC(bat_model_td, k=log(length(bat$acc)))[2],
                   sincos=extractAIC(bat_model_sincos, k=log(length(bat$acc)))[2],
                   #manual=extractAIC(bat_model_man, k=log(length(bat$acc)))[2],
                   stepwise_manual=extractAIC(fwd_man, k=log(length(bat$acc)))[2]
#                   forward_manual=extractAIC(fwd_man2, k=log(length(bat$acc)))[2]
                   )
bat_model.BIC

eBIC <- exp(-0.5*(bat_model.BIC-min(bat_model.BIC)))
round(probs <- eBIC/sum(eBIC), 5)


par(mfrow=c(1,2))
png(filename=paste(sep="", Dropbox, path, "1.4.png"))
acf(residuals(fwd_man), main="ACF of residuals")
plot(fwd_man$fitted, residuals(fwd_man), xlab="Fitted values", ylab="Residuals", main="Residuals vs. Fitted values")
dev.off()

r <- rstudent(fwd_man)
par(mfrow=c(1,3))
plot(fwd_man$fitted, r, xlab="Fitted values", main="Studentized Residuals")
hist(r, main="Histogram of Studentized Residuals")
qqnorm(r); 
abline(0,1)
par(mfrow=c(1,1))




#Orange Juice Competition

# NOTES:
#   Elasticity is %change in vol for a 1% change in price
#   Elasticity is also the slope of the log-log model => log(vol)/log(price)


summary(oj)
length(oj[,1])

par(mfrow=c(1,3))
plot(oj_transformed$minuteprice, oj_transformed$vol, xlab="log(minuteprice)", ylab="log(minutevol)", main="Minute Maid Sales Volume vs. Minute Maid Price")
plot(oj_transformed$dmnckprice, oj_transformed$vol, xlab="log(dmnckprice)", ylab="log(minutevol)", main="Minute Maid Sales Volume vs. Dominicks Price")
plot(oj_transformed$tropicprice, oj_transformed$vol, xlab="log(tropicprice)", ylab="log(minutevol)", main="Minute Maid Sales volume vs. Tropicana Price")

boxplot(vol~ minutead, data=oj_transformed, main="Minut Maid Sales Volume by Minute Maid Ad")
boxplot(vol~ dmnckad, data=oj_transformed, main="Minut Maid Sales Volume by Dominicks Ad")
boxplot(vol~ tropicad, data=oj_transformed, main="Minut Maid Sales Volume by Tropicana Ad")
par(mfrow=c(1,1))

oj_transformed = data.frame(vol=log(oj$minutevol))
oj_transformed$minuteprice = log(oj$minuteprice)
oj_transformed$tropicprice=log(oj$tropicprice)
oj_transformed$dmnckprice=log(oj$dmnckprice)
oj_transformed$tropicad=oj$tropicad
oj_transformed$minutead=oj$minutead
oj_transformed$dmnckad=oj$dmnckad



summary(oj_transformed)

plot(oj_transformed)

oreg1 = lm(vol~ minuteprice + tropicprice + dmnckprice, data=oj_transformed)
oreg2 = lm(vol~ minuteprice + tropicprice + dmnckprice + minutead, data=oj_transformed)
oreg3 = lm(vol~ tropicprice + tropicad + dmnckprice + dmnckad + minuteprice + minutead, data=oj_transformed)
oreg4 = lm(vol~ tropicprice * tropicad + dmnckprice * dmnckad + minuteprice*minutead, data=oj_transformed)
oreg5 = lm(vol~ tropicprice * tropicad + dmnckprice * dmnckad + minuteprice*minutead + minutead*dmnckad*tropicad, data=oj_transformed)


oreg6 = lm(vol~ tropicprice * tropicad * minutead + dmnckprice * dmnckad*minutead + minuteprice * minutead * dmnckad * tropicad, data=oj_transformed)

oreg7 = lm(vol~ tropicprice * tropicad * minutead * dmnckad + dmnckprice * dmnckad*minutead * tropicad + minuteprice * minutead * dmnckad * tropicad, data=oj_transformed)
oreg8 = lm(vol~ (tropicprice + dmnckprice + minuteprice) * tropicad * minutead * dmnckad + dmnckprice*tropicprice*minuteprice, data=oj_transformed)

summary(oreg1)
summary(oreg2)
summary(oreg3)
summary(oreg4)
summary(oreg5)
summary(oreg6)
summary(oreg7)
summary(oreg8)


fwd = step(oreg8, direction="backward", k=log(length(oj_transformed$vol)))
summary(fwd)

fwd2 = step(oreg1, scope=formula(oreg9), direction="forward", k=log(length(oj_transformed$vol)))
summary(fwd2)

model.BIC <- c(reg1=extractAIC(oreg1, k=log(length(oj_transformed$vol)))[2],
               reg2=extractAIC(oreg2, k=log(length(oj_transformed$vol)))[2],
               reg3=extractAIC(oreg3, k=log(length(oj_transformed$vol)))[2],
               reg4=extractAIC(oreg4, k=log(length(oj_transformed$vol)))[2],
               reg5=extractAIC(oreg5, k=log(length(oj_transformed$vol)))[2],
               reg6=extractAIC(oreg6, k=log(length(oj_transformed$vol)))[2],
               reg7=extractAIC(oreg7, k=log(length(oj_transformed$vol)))[2],
               reg8=extractAIC(oreg8, k=log(length(oj_transformed$vol)))[2]
#               fwd=extractAIC(fwd, k=log(length(oj_transformed$vol)))[2]
)             
model.BIC

eBIC <- exp(-0.5*(model.BIC-min(model.BIC)))
round(probs <- eBIC/sum(eBIC), 5)

r <- rstudent(oreg8)
par(mfrow=c(1,3))
plot(oreg8$fitted, r, xlab="Fitted values", main="Studentized Residuals")
hist(r, main="Histogram of Studentized Residuals")
qqnorm(r); 
abline(0,1)
par(mfrow=c(1,1))


#Predicting Income
# Notes:
#   Some vars (like age) don't appear to be good predictors, but might have interactions with other variables'
#   What is fnlwgt?
#   Consider taking logs of fnlwgt, capgain, caploss
#   Don't need to explain the model- just find a good one
#   Shouldn't use more than 6 or 7 variables

# par(mfrow=c(4,4))
# for(i in 1:14)
# {
#   if(colnames(adult)[i] == "pay") { next }
#   plot(adult[,i],adult$pay, xlab=paste(colnames(adult)[i]), ylab="pay") 
# }
# par(mfrow=c(4,4))
# for(i in 1:14)
# {
#   if(colnames(adult)[i] == "pay") { next }
#   boxplot(adult$pay, adult[,i])
#   #,  xlab="pay", ylab=paste(colnames(adult)[i])) 
# }
# par(mfrow=c(1,1))

sink("IncomePredictionLoop.txt", append=FALSE, split=TRUE)
misclassification = data.frame(
            reg1=double(),
            reg2=double(),
            reg3=double(),
            reg4=double(),
            reg5=double(),
            reg6=double(),
            reg7=double(),
            reg8=double())

Sys.time()

for(i in 1:10)
  {
train <- sample(1:length(adult$pay),length(adult$pay))
smalladult=adult

wc = smalladult$workclass =="Without-pay"
smalladult[wc,]$workclass <- "Private"
smalladult$workclass = factor(smalladult$workclass)

ed10 = smalladult$education =="10th"
ed11 = smalladult$education =="11th"
ed12 = smalladult$education =="12th"
ed1to4 = smalladult$education =="1st-4th"
ed5to6 = smalladult$education =="5th-6th"
ed7to8 = smalladult$education =="7th-8th"
ed9 = smalladult$education =="9th"
edpreschool = smalladult$education =="Preschool"
edassocvoc = smalladult$education =="Assoc-voc"
edassocacdm = smalladult$education =="Assoc-acdm"
edsomecol = smalladult$education =="Some-college"
smalladult$education<- factor(smalladult$education, levels = c(levels(smalladult$education), "Pre-HS", "Assoc"))
smalladult[ed10,]$education <- "Pre-HS"
smalladult[ed11,]$education <- "Pre-HS"
smalladult[ed12,]$education <- "Pre-HS"
smalladult[ed1to4,]$education <- "Pre-HS"
smalladult[ed5to6,]$education <- "Pre-HS"
smalladult[ed7to8,]$education <- "Pre-HS"
smalladult[ed9,]$education <- "Pre-HS"
smalladult[edpreschool,]$education <- "Pre-HS"
smalladult[edassocvoc,]$education <- "Assoc"
smalladult[edassocacdm,]$education <- "Assoc"
smalladult[edsomecol,]$education <- "HS-grad"
smalladult$education = factor(smalladult$education)

ocarmed = smalladult$occupation =="Armed-Forces"
ocpriv = smalladult$occupation =="Priv-house-serv"
ocprotective = smalladult$occupation =="Protective-serv"
smalladult$occupation<- factor(smalladult$occupation, levels = c(levels(smalladult$occupation), "Other"))
smalladult[ocarmed,]$occupation <- "Other"
smalladult[ocpriv,]$occupation <- "Other"
smalladult[ocprotective,]$occupation <- "Other"
smalladult$occupation = factor(smalladult$occupation)

smalladult$native<- factor(smalladult$native, levels = c(levels(smalladult$native), "Other"))

nvcambodia = smalladult$native =="Cambodia"
nvecuador = smalladult$native =="Ecuador"
nvfrance = smalladult$native =="France"
nvholand = smalladult$native =="Holand-Netherlands"
nvhonduras = smalladult$native =="Honduras"
nvhong = smalladult$native =="Hong"
nvhungary = smalladult$native =="Hungary"
nvireland = smalladult$native =="Ireland"
nvlaos = smalladult$native =="Laos"
nvoutlying = smalladult$native =="Outlying-US(Guam-USVI-etc)"
nvscotland = smalladult$native =="Scotland"
nvthailand = smalladult$native =="Thailand"
nvtrin = smalladult$native =="Trinadad&Tobago"
nvyug = smalladult$native =="Yugoslavia"
smalladult[nvcambodia,]$native <- "Other"
smalladult[nvcambodia,]$native <- "Other"
smalladult[nvecuador,]$native <- "Other"
smalladult[nvfrance,]$native <- "Other"
smalladult[nvholand,]$native <- "Other"
smalladult[nvhonduras,]$native <- "Other"
smalladult[nvhong,]$native <- "Other"
smalladult[nvhungary,]$native <- "Other"
smalladult[nvireland,]$native <- "Other"
smalladult[nvlaos,]$native <- "Other"
smalladult[nvoutlying,]$native <- "Other"
smalladult[nvscotland,]$native <- "Other"
smalladult[nvthailand,]$native <- "Other"
smalladult[nvtrin,]$native <- "Other"
smalladult[nvyug,]$native <- "Other"
smalladult$native = factor(smalladult$native)

native = smalladult$native
drops <- c("marital")
smalladult = smalladult[,!(names(smalladult) %in% drops)]

drops <- c("native")
smalladult_sansnative = smalladult[,!(names(smalladult) %in% drops)]

transformed_adult = data.frame(pay = smalladult$pay)
transformed_adult$age = smalladult$age
transformed_adult$fnlwgt = smalladult$fnlwgt
transformed_adult$capgain = smalladult$capgain
transformed_adult$caploss = smalladult$caploss
transformed_adult$hourspweek = smalladult$hourspweek
transformed_adult$workclass_federal = smalladult$workclass =="Federal-gov"
transformed_adult$workclass_local = smalladult$workclass =="Local-gov"
#transformed_adult$workclass_private = smalladult$workclass =="Private"
transformed_adult$workclass_self_emp_inc = smalladult$workclass =="Self-emp-inc"
transformed_adult$workclass_self_emp_not_inc = smalladult$workclass =="Self-emp-not-inc"
transformed_adult$workclass_state = smalladult$workclass =="State-gov"
transformed_adult$native_canada = smalladult$native =="Canada"
transformed_adult$native_china = smalladult$native =="China"
transformed_adult$native_columbia = smalladult$native =="Columbia"
transformed_adult$native_cuba = smalladult$native =="Cuba"
transformed_adult$native_dr = smalladult$native =="Dominican-Republic"
transformed_adult$native_es = smalladult$native =="El-Salvador"
transformed_adult$native_england = smalladult$native =="England"
transformed_adult$native_germany = smalladult$native =="Germany"
transformed_adult$native_greece = smalladult$native =="Greece"
transformed_adult$native_guatemala = smalladult$native =="Guatemala"
transformed_adult$native_haiti = smalladult$native =="Haiti"
transformed_adult$native_india = smalladult$native =="India"
transformed_adult$native_iran = smalladult$native =="Iran"
transformed_adult$native_italy = smalladult$native =="Italy"
transformed_adult$native_jamaica = smalladult$native =="Jamaica"
transformed_adult$native_japan = smalladult$native =="Japan"
transformed_adult$native_mexico = smalladult$native =="Mexico"
transformed_adult$native_nicaragua = smalladult$native =="Nicaragua"
transformed_adult$native_peru = smalladult$native =="Peru"
transformed_adult$native_philippines = smalladult$native =="Philippines"
transformed_adult$native_poland = smalladult$native =="Poland"
transformed_adult$native_portugal = smalladult$native =="Portugal"
transformed_adult$native_pr = smalladult$native =="Puerto-Rico"
transformed_adult$native_south = smalladult$native =="South"
transformed_adult$native_taiwan = smalladult$native =="Taiwan"
#transformed_adult$native_us = smalladult$native =="United-States"
transformed_adult$native_vietnam = smalladult$native =="Vietnam"
transformed_adult$native_other = smalladult$native =="Other"
#transformed_adult$education_bachelors = smalladult$education =="Bachelors"
transformed_adult$education_dr = smalladult$education =="Doctorate"
transformed_adult$education_hs = smalladult$education =="HS-grad"
transformed_adult$education_ms = smalladult$education =="Masters"
transformed_adult$education_prof = smalladult$education =="Prof-school"
transformed_adult$education_pre_hs = smalladult$education =="Pre-HS"
transformed_adult$education_assoc = smalladult$education =="Assoc"
transformed_adult$sex_m = smalladult$sex =="Male"
#transformed_adult$sex_f = smalladult$sex =="Female"
transformed_adult$occupation_cr = smalladult$occupation =="Craft-repair"
transformed_adult$occupation_ac = smalladult$occupation =="Adm-clerical"
transformed_adult$occupation_em = smalladult$occupation =="Exec-managerial"
transformed_adult$occupation_ff = smalladult$occupation =="Farming-fishing"
transformed_adult$occupation_hc = smalladult$occupation =="Handlers-cleaners"
transformed_adult$occupation_moi = smalladult$occupation =="Machine-op-inspct"
transformed_adult$occupation_os = smalladult$occupation =="Other-service"
transformed_adult$occupation_ps = smalladult$occupation =="Prof-specialty"
transformed_adult$occupation_s = smalladult$occupation =="Sales"
transformed_adult$occupation_ts = smalladult$occupation =="Tech-support"
transformed_adult$occupation_tm = smalladult$occupation =="Transport-moving"
#transformed_adult$occupation_o = smalladult$occupation =="Other"
#transformed_adult$relationship_husband = smalladult$relationship =="Husband"
transformed_adult$relationship_nif = smalladult$relationship =="Not-in-family"
transformed_adult$relationship_other = smalladult$relationship =="Other-relative"
transformed_adult$relationship_own = smalladult$relationship =="Own-child"
transformed_adult$relationship_unmarried = smalladult$relationship =="Unmarried"
transformed_adult$relationship_wife = smalladult$relationship =="Wife"
transformed_adult$race_native_amer = smalladult$race =="Amer-Indian-Eskimo"
transformed_adult$race_asian = smalladult$race =="Asian-Pac-Islander"
transformed_adult$race_black = smalladult$race =="Black"
transformed_adult$race_other = smalladult$race =="Other"
#transformed_adult$race_white = smalladult$race =="White"

null=glm(pay ~ 1, family="binomial", data=smalladult[train,])
null2=glm(pay ~ 1, family="binomial", data=transformed_adult[train,])
full=glm(pay ~ ., family="binomial", data=smalladult[train,])
full2=glm(pay ~ . + .^2, family="binomial", data=smalladult_sansnative[train,])
full4=glm(pay ~ ., family="binomial", data=transformed_adult[train,])

# fwd <- step(null, scope=formula(full), direction="forward", k=log(length(smalladult[train,]$pay)))
# fwd <- glm(pay ~ relationship + education + capgain + occupation + caploss + hourspweek + age + workclass,  family = "binomial", data=smalladult[train,])
# fwd2 <- step(null, scope=formula(full3), direction="forward", k=log(length(smalladult[train,]$pay)))
# fwd2 <- glm(pay ~ relationship + education + capgain + occupation + caploss + hourspweek + age + sex + workclass,  family = "binomial", data=smalladult[train,])

areg1 = glm(pay ~ age * workclass + fnlwgt + 
              education + occupation + relationship + race + 
              sex + capgain + caploss + 
              hourspweek + native,  family = "binomial", data=smalladult[train,])
areg2 = glm(pay ~ fnlwgt + education * age + occupation * hourspweek + age * relationship + relationship * capgain +
              relationship * hourspweek + race + age * sex + age * capgain + age * caploss + workclass * caploss +
              age * hourspweek + workclass*caploss + native + occupation * capgain + occupation * relationship + 
              workclass * occupation + education * relationship + education * sex + native,  family = "binomial", data=smalladult[train,])
areg3 = glm(pay ~  . * occupation + .*race + . * hourspweek - occupation*native - race * native,  family = "binomial", data=smalladult[train,])
areg4 = glm(pay ~ . * occupation + . * hourspweek - occupation*native,  family = "binomial", data=smalladult[train,])
areg5=glm(pay ~ (age + fnlwgt + capgain + caploss + hourspweek + sex_m) * (age + fnlwgt + capgain + caploss + hourspweek + sex_m) + 
            
            (age + fnlwgt + capgain + caploss + hourspweek + sex_m) * (workclass_federal + workclass_local + workclass_self_emp_inc + workclass_self_emp_not_inc + workclass_state) + 
            
            (age + fnlwgt + hourspweek + sex_m) * (native_canada + native_china + native_columbia + native_cuba + native_dr + native_es + native_england + 
            native_germany + native_greece + native_guatemala + native_haiti + native_india + native_iran + native_italy + 
            native_jamaica + native_japan + native_mexico + native_nicaragua + native_peru + native_philippines + 
            native_poland + native_portugal + native_pr + native_south + native_taiwan + native_vietnam + native_other) + 
            
            (age + fnlwgt + capgain + caploss + hourspweek + sex_m) * (education_dr + education_hs + education_ms + education_prof + education_pre_hs + education_assoc) + 
            
            (age + fnlwgt + hourspweek + race_asian + race_black + race_other + relationship_nif + relationship_other + relationship_own + relationship_unmarried + relationship_wife + race_native_amer)
            * (occupation_cr + occupation_ac + occupation_em + occupation_ff + occupation_hc + occupation_moi + 
            occupation_os + occupation_ps + occupation_s + occupation_ts + occupation_tm) + 
            
            (age + fnlwgt + capgain + caploss + hourspweek + sex_m) * (relationship_nif + relationship_other + relationship_own + relationship_unmarried + relationship_wife + race_native_amer) + 
            
            (race_asian + race_black + race_other)
            - capgain * caploss
          , family="binomial", data=transformed_adult[train,])

#fwd4 <- step(null2, scope=formula(areg5), direction="forward", k=log(length(transformed_adult[train,]$pay)))

areg6=glm(pay ~  relationship_nif + relationship_other + relationship_own + relationship_unmarried + relationship_wife +
                 education_dr + education_hs + education_prof + education_pre_hs + education_assoc +
                 occupation_em + occupation_ff + occupation_ps + occupation_os + occupation_hc + occupation_ts + occupation_moi + occupation_s +
                 fnlwgt + sex_m + 
                 workclass_self_emp_not_inc + workclass_federal +
                 native_columbia +
                 relationship_own * capgain + education_hs*caploss + age*workclass_self_emp_not_inc +
                 relationship_nif * hourspweek + relationship_nif * capgain + occupation_ps * hourspweek +
                 age * occupation_ts + hourspweek*age + relationship_own*age +hourspweek*workclass_self_emp_not_inc + 
                 occupation_s *relationship_wife
                 - capgain - caploss - age - hourspweek
          , family="binomial", data=transformed_adult[train,])

areg7=glm(pay ~  relationship_nif + relationship_other + relationship_own + relationship_unmarried + relationship_wife +
            education_dr + education_hs + education_prof + education_pre_hs + education_assoc +
            occupation_em + occupation_ff + occupation_ps + occupation_os + occupation_hc + occupation_ts + occupation_moi + occupation_s +
            fnlwgt + sex_m + age + hourspweek +
            workclass_self_emp_not_inc + workclass_federal +
            native_columbia + native_dr +
            relationship_own * capgain + education_hs*caploss + age*workclass_self_emp_not_inc +
            relationship_nif * hourspweek + relationship_nif * capgain + occupation_ps * hourspweek +
            age * occupation_ts + hourspweek*age + relationship_own*age +hourspweek*workclass_self_emp_not_inc + 
            occupation_s *relationship_wife + occupation_ff *relationship_wife
            - capgain - caploss
          , family="binomial", data=transformed_adult[train,])

fwd4 <- step(areg7, direction="backward", k=log(length(transformed_adult[train,]$pay)))
summary(fwd4)
summary(areg6)
summary(full)
summary(areg1)
length(areg6$coef)

#0.1464 . * occupation + . * race + . * hourspweek
#0.1475 . * occupation + . * hourspweek
#0.1468 - drop native, then .* + .^2
#0.14617 - drop native, then .* + .^2 + native

#age, capgain, caploss, relationship, fnlwgt, education, workclass, occupation, 
#race, sex
#* education is effective...
#* occupation is also effective, but takes awhile

# predregfwd4 <- predict(fwd4, newdata=transformed_adult[-train,], type="response") 
# errorregfwd4 <- as.numeric(smalladult[-train,]$pay) - 1 - (predregfwd4 >= .5) 
# mean(abs(errorregfwd4))

predareg1 <- predict(areg1, newdata=smalladult[train,], type="response") 
predareg2 <- predict(areg2, newdata=smalladult[train,], type="response") 
predareg3 <- predict(areg3, newdata=smalladult[train,], type="response") 
predareg4 <- predict(areg4, newdata=smalladult[train,], type="response") 
predareg5 <- predict(areg5, newdata=transformed_adult[train,], type="response") 
predareg6 <- predict(areg6, newdata=transformed_adult[train,], type="response") 
predareg7 <- predict(areg7, newdata=transformed_adult[train,], type="response") 
predregfull <- predict(full, newdata=smalladult[train,], type="response") 
predregfull2 <- predict(full2, newdata=smalladult_sansnative[train,], type="response") 
predregfull4 <- predict(full4, newdata=transformed_adult[train,], type="response") 
# predregfwd <- predict(fwd, newdata=smalladult[-train,], type="response") 
# predregfwd2 <- predict(fwd2, newdata=smalladult_sansnative[-train,], type="response") 
# predregfwd3 <- predict(fwd3, newdata=transformed_adult[-train,], type="response") 
# predregfwd4 <- predict(fwd4, newdata=transformed_adult[-train,], type="response") 


errorareg1 <- as.numeric(smalladult[train,]$pay) - 1 - (predareg1 >= .5) 
errorareg2 <- as.numeric(smalladult[train,]$pay) - 1 - (predareg2 >= .5) 
errorareg3 <- as.numeric(smalladult[train,]$pay) - 1 - (predareg3 >= .5) 
errorareg4 <- as.numeric(smalladult[train,]$pay) - 1 - (predareg4 >= .5) 
errorareg5 <- as.numeric(smalladult[train,]$pay) - 1 - (predareg5 >= .5) 
errorareg6 <- as.numeric(smalladult[train,]$pay) - 1 - (predareg6 >= .5) 
errorareg7 <- as.numeric(smalladult[train,]$pay) - 1 - (predareg7 >= .5) 
errorregfull <- as.numeric(smalladult[train,]$pay) - 1 - (predregfull >= .5) 
errorregfull2 <- as.numeric(smalladult[train,]$pay) - 1 - (predregfull2 >= .5) 
errorregfull4 <- as.numeric(smalladult[train,]$pay) - 1 - (predregfull4 >= .5) 
# errorregfwd <- as.numeric(smalladult[-train,]$pay) - 1 - (predregfwd >= .5) 
# errorregfwd2 <- as.numeric(smalladult[-train,]$pay) - 1 - (predregfwd2 >= .5) 
# errorregfwd3 <- as.numeric(smalladult[-train,]$pay) - 1 - (predregfwd3 >= .5) 
# errorregfwd4 <- as.numeric(smalladult[-train,]$pay) - 1 - (predregfwd4 >= .5) 
newrow = c(
  mean(abs(errorareg1)),
  mean(abs(errorareg2)),
  mean(abs(errorareg3)),
  mean(abs(errorareg4)),
  mean(abs(errorareg5)),
  mean(abs(errorareg6)),
  mean(abs(errorareg7))
#   mean(abs(errorregfull)),
#   mean(abs(errorregfull2)),
#   mean(abs(errorregfull4))
  )

newrow
misclassification = rbind(misclassification, newrow)

model.BIC <- c(reg1=extractAIC(areg1, k=log(length(smalladult[train,]$pay)))[2],
               reg2=extractAIC(areg2, k=log(length(smalladult[train,]$pay)))[2],
               reg3=extractAIC(areg3, k=log(length(smalladult[train,]$pay)))[2],
               reg4=extractAIC(areg4, k=log(length(smalladult[train,]$pay)))[2],
               reg5=extractAIC(areg5, k=log(length(smalladult[train,]$pay)))[2],
               reg6=extractAIC(areg6, k=log(length(smalladult[train,]$pay)))[2],
               reg7=extractAIC(areg7, k=log(length(smalladult[train,]$pay)))[2]
               )             
model.BIC

eBIC <- exp(-0.5*(model.BIC-min(model.BIC)))
round(probs <- eBIC/sum(eBIC), 5)
}
misclassification
mean(misclassification[,1]) 
mean(misclassification[,2]) 
mean(misclassification[,3]) 
mean(misclassification[,4]) 
mean(misclassification[,5]) 
mean(misclassification[,6]) 
mean(misclassification[,7]) 
mean(misclassification[,8]) 
mean(misclassification[,9]) 
mean(misclassification[,10]) 

summary(areg6)
predareg6 <- predict(areg6, newdata=transformed_adult[train,], type="response") 
errorareg6 <- as.numeric(smalladult[train,]$pay) - 1 - (predareg6 >= .5) 
mean(abs(errorareg6))

Sys.time()

c(3.14541E-06, 6.8546E-06, 3.28822E-07, 1.56018E-06, 7.12842E-09, 2.43413E-06, 1.22507E-06, 1.93626E-07
1.34599E-06
6.90097E-06
