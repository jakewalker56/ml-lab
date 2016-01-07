# Homework: congressional text dataset 
# Much of this could have been copied straight from we8there.R or wine.R
# (that's why you had no starter script this week)

library(textir) # to get the data
library(maptpx) # for the topics function

data(congress109)


# [1] fit k-means for k in 5,10,15,20,25.  Use an IC to choose the
#   number of clusters and interpret some of the centers.

#  as we discussed in class, you can choose a variety of scales
#  upon which to fit k-means; here I just used standardized freq
fs <- scale(as.matrix( congress109Counts/rowSums(congress109Counts) ))

## follow wine code to fit for a set of k's
## notice the only difference here is that I've replaced 1:200 with 5*(1:5)
## the question asked for a smaller set of candidate models than we had for wine.
kfit <- lapply(5*(1:5), function(k) kmeans(fs,k))
source("kIC.R")
kaicc <- sapply(kfit,kIC)
kbic <- sapply(kfit,kIC,"B")
## plot 'em: they disagree!!
plot(5*(1:5), kaicc, xlab="K", ylab="IC", 
	ylim=range(c(kaicc,kbic)), 
	bty="n", type="l", lwd=2)
abline(v=which.min(kaicc)*5)
lines(5*(1:5), kbic, col=4, lwd=2)
abline(v=which.min(kbic)*5,col=4)

# there's no clear way to choose... we'll go with 20
# you could have gone with whatever makes sense to you: 
#  that's how I recommend working here!
kmfs <- kfit[[4]] # 2nd is 4*5=20
## interpretation: we can see the words with cluster centers
## highest above zero (these are in units of standard deviation of f)
print(apply(kmfs$centers,1,function(c) colnames(fs)[order(-c)[1:10]]))
## use what you know to interpret these.
## in my example, you get seemingly reasonable topics: 
## iraq, social security, courts, etc.

## [2] topic modelling.  
# first, convert to slam matrix
x <- as.simple_triplet_matrix(congress109Counts)

## Topic modelling: we'll choose the number of topics
## Recall: BF is like exp(-BIC), so you choose the bigggest BF
tpcs <- topics(x,K=2:25) # it chooses 13 topics  for me

## interpretation
# ordering by `topic over aggregate' lift:
summary(tpcs, n=10) 
# ordered by simple in-topic prob
print(rownames(tpcs$theta)[order(tpcs$theta[,1], decreasing=TRUE)[1:10]])
print(rownames(tpcs$theta)[order(tpcs$theta[,2], decreasing=TRUE)[1:10]])
# you can tell any story; I see big gop and dem topics 1 and 2, 
# then issue specific stuff
# if you ran a different configuration (e.g., K=5*(1:5)), then you might
# have ended up selecting or working with a totally different set of topics.
# Topic models are what we call 'unidentified' -- 
# in practice, this means that the fitting algorithms 
# give different answers depending upon where you start them. 
# The topics algorithm fits topics sequentially, 
# so that your fit at K=10 is used to derive a good starting location 
# for your fit at, say, K=15. It is not a hard rule, 
# but I think you tend to get better topics starting 
# from smaller K and taking smaller steps between K.

# look at party mean memberships
DemO <- colMeans(tpcs$omega[congress109Ideology$party=="D",])
RepO <- colMeans(tpcs$omega[congress109Ideology$party=="R",])
sort(DemO/RepO) 
# 1,3,7 are republican and 2,5,12 are strong dem
# I can say this because, e.g., 1 has a low Dem/Rep ratio 
# and 2 has a hight Dem/Rep ratio

## Wordles!  Again, in my fit looks like 1 is gop, 2 is dems
library(wordcloud)
par(mfrow=c(1,2))
wordcloud(row.names(tpcs$theta), 
	freq=tpcs$theta[,1], min.freq=0.004, col="maroon")
wordcloud(row.names(tpcs$theta), 
	freq=tpcs$theta[,2], min.freq=0.004, col="navy")

## [3] partisanship
# first, we can just table party by kmeans cluster 
# (like red v. white wine)
tapply(congress109Ideology$party,kmfs$cluster,table)
# looks like most clusters split along party lines.
# for me, cluster `14` was less partisan (50 Dem, 138 GOP)
# but I couldn't discern any clear subject
colnames(fs)[order(-kmfs$centers[14,])[1:10]]
# [1] "business.owner"        "private.property"      "death.tax"            
# [4] "urge.support"          "repeal.death.tax"      "sex.offender"         
# [7] "look.forward"          "strong.support"        "united.postal.service"

## now, fit a topic regression
library(gamlr)
## omega is the n x K matrix of document topic weights
## i.e., how much of each doc is from each topic

gop <- congress109Ideology[,"party"]=="R"
partyreg <- gamlr(tpcs$omega, gop, 
	family="binomial") # don't forget: its logistic regression!
# odd multipliers for a 10% rise in topic in doc
print(exp(coef(partyreg)*0.1))
# I see a big effect from topic 3 (more republican) and 5 (more democrat)

## same thing, but for `repshare'
# this is now linear regression
repreg <- gamlr(tpcs$omega, congress109Ideology[,"repshare"])
# increase in repshare per 10% rise in topic in doc
print(coef(repreg)*0.1)
## the effects have the same direction (+/- sign)

# Compare to straight regression.
regtopics.cv <- cv.gamlr(tpcs$omega, gop, family="binomial")
## give it the word %s as inputs
x <- 100*congress109Counts/rowSums(congress109Counts)
regwords.cv <- cv.gamlr(x, gop, family="binomial")

par(mfrow=c(1,2))
plot(regtopics.cv, main="topic regression")
plot(regwords.cv, main="phrase count regression")
# max OOS R^2s
max(1-regtopics.cv$cvm/regtopics.cv$cvm[1])
max(1-regwords.cv$cvm/regwords.cv$cvm[1])
## topics fit does better!
## we're not accounting for variability in the topic model here 
## (it was fit to the entire dataset)
## however, this is not actually unrealistic: it's typical that you
## are able to get much more un-labelled data for un-supervised clustering
## (here, text without knowing the speaker's party) than labelled data
## for regression.  So you can build a really strong unsupervised model,
## then use that in regression.  This only works, however, if the 
## dominant sources of variation in x (what you pick up in unsupervised 
## modelling) are related to y (like they were here, but unlike for wine).




