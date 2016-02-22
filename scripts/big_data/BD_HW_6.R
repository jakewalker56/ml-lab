setwd("C:/Users/Andrew/Google Drive/Chicago Booth Class Materials/Spring 2015/Big Data/Homework/HW6")

#library(fBasics)
#library(quantmod)
#library(xtable)
#library(igraph)
library(gamlr)
source('naref.R')
source('kIC.R')
library(textir)
library(wordcloud)
library(maptpx)

?congress109

data(congress109)
# scale data and create main dataset
congress = scale(as.matrix(congress109Counts/rowSums(congress109Counts) ))

###################################
# Problem 1
###################################
# run k-means for 5, 10, 15, 20, 25


# do it all at once
kfit <- lapply(5*(1:5), function(k) kmeans(congress,k, nstart=25))
kfit
kaicc <- sapply(kfit,kIC)
kbic <- sapply(kfit,kIC,"B")
plot(kaicc, xlab="K", ylab="IC", 
     ylim=range(c(kaicc,kbic)), # get them on same page
     bty="n", type="l", lwd=2)
abline(v=which.min(kaicc))
lines(kbic, col=4, lwd=2)
abline(v=which.min(kbic),col=4)
title("BIC (blue) and AICc (black) for K-means with K = 5, 10, 15, 20, and 25")

help(topics)

# now in finer variatinos
kfit2 <- lapply(1:10, function(k) kmeans(congress,k, nstart=25))

kaicc2 <- sapply(kfit2,kIC)
kbic2 <- sapply(kfit2,kIC,"B")
plot(kaicc2, xlab="K", ylab="IC", 
     ylim=range(c(kaicc2,kbic2)), # get them on same page
     bty="n", type="l", lwd=2)
abline(v=which.min(kaicc2))
lines(kbic2, col=4, lwd=2)
abline(v=which.min(kbic2),col=4)
title("BIC (blue) and AICc (black) for K-means with K = 1-10")


# get r-squared
k=1
1 - sum(kfit[[k]]$tot.withinss)/kfit[[k]]$totss


###################################
# Problem 2
###################################
# Topic model

x <- as.simple_triplet_matrix(congress109Counts)
tpcs <- topics(x,K=2:30, tol=0.1, ord=TRUE)

help(topics)

summary(tpcs, n=3)

help(kmeans)

rownames(tpcs$omega)[order(tpcs$omega[,1], decreasing=TRUE)[1:12]]
rownames(tpcs$omega)[order(tpcs$omega[,1], decreasing=TRUE)[518:529]]

rownames(tpcs$theta)[order(tpcs$theta[,8], decreasing=TRUE)[1:30]]
rownames(tpcs$omega)[order(tpcs$omega[,8], decreasing=TRUE)[1:50]]
plot(kfit)

par(mfrow=c(2,2))
wordcloud(row.names(tpcs$theta), 
          freq=tpcs$theta[,1],  max.words=20, col="red", random.order=FALSE)
wordcloud(row.names(tpcs$theta), 
          freq=tpcs$theta[,2],  max.words=20, col="green", random.order=FALSE)
wordcloud(row.names(tpcs$theta), 
          freq=tpcs$theta[,3],  max.words=20, col="blue", random.order=FALSE)
wordcloud(row.names(tpcs$theta), 
          freq=tpcs$theta[,4],  max.words=20, col="black", random.order=FALSE)



###################################
# Problem 3
###################################
#create y vars for regressions

###################################
#Part 1
# extract party and repshare
party = congress109Ideology[,2]
repshare = congress109Ideology[,5]
# create just Republicans and everyone else
party_dummy = rep(0, 529)
party_dummy[party=="R"] = 1
party_dummy





###################################
#Part 2: Topic Regressions

###############
# party regressions

# create matrix for gamlrs
party_data = data.frame(cbind(party_dummy, tpcs$omega))
x_party = model.matrix(party_dummy ~ ., data=party_data)[,-1]

#cvgamlr
party_reg_cv <- cv.gamlr(x_party, party_dummy, min.lambda=0.000001,family="binomial")
summary(party_reg_cv)
coef(party_reg_cv, select="1se")
coef(party_reg_cv, select="min")

# regular gamlr
#party_reg <- gamlr(x_party, party_dummy, min.lambda=0.0001,family="binomial")
#summary(party_reg)
#coef(party_reg, select=which.min(BIC(party_reg)))
#coef(party_reg, select=which.min(AICc(party_reg)))


par(mfrow=c(1,1))
plot(party_reg_cv)


###############
# repshareregressions
repshare_data = data.frame(cbind(repshare, tpcs$omega))
x_repshare = model.matrix(repshare ~ ., data=repshare_data)[,-1]

repshare_cv.reg <- cv.gamlr(x_repshare, repshare, min.lambda=0.0001)
#repshare_reg <- gamlr(x_repshare, repshare, min.lambda=0.0001)
summary(repshare_cv.reg)
#summary(repshare_reg)

coef(repshare_cv.reg,  select="1se")
coef(repshare_cv.reg,  select="min")

#coef(repshare_reg, select=which.min(BIC(repshare_reg)))
#coef(repshare_reg, select=which.min(AICc(repshare_reg)))

plot(repshare_cv.reg)



###################################
#Part 2: Phrase Regressions
x<-100*congress109Counts/rowSums(congress109Counts)

# party regs
party_phrase_cvreg <- cv.gamlr(x, party_dummy, min.lambda=0.0001, family="binomial")
summary(party_phrase_cvreg)
coef(party_phrase_cvreg, select="1se")
coef(party_phrase_cvreg, select="Min")
plot(party_phrase_cvreg)

# repshare regs
repshare_phrase_cvreg <- cv.gamlr(x, repshare, min.lambda=0.01)
summary(repshare_phrase_cvreg)
coef(repshare_phrase_cvreg, select="1se")
coef(repshare_phrase_cvreg, select="Min")
plot(repshare_phrase_cvreg)

########################################3

party_Baicc <- drop(coef(party_phrase_cvreg))
repshare_Baicc <- drop(coef(repshare_phrase_cvreg))

party_Baicc_nonzero = party_Baicc[party_Baicc!=0]
repshare_Baicc_nonzero = repshare_Baicc[repshare_Baicc!=0]

party_Baicc_nonzero
repshare_Baicc_nonzero
