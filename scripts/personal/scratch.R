library("devtools")
library(roxygen2)
setwd("~/github/jml")
document()
install_github('jakewalker56/jml', force = TRUE)

#testing p values from covariance matrix
?runif
X <- NULL
X = data.frame(height=runif(10), weight=runif(10))
Y <- data.frame(response=runif(10))

reg = lm(Y$response~X$height + X$weight)
s2 = sum((predict(reg,X) - Y)^2)/(nrow(Y)-3)
X2 = data.frame(I=rep(1, nrow(X)), x=X)
co = s2 * solve(as.matrix(t(X2)) %*% as.matrix(X2))
solve(as.matrix(t(X2)) %*% as.matrix(X2)) %*% as.matrix(t(X2)) %*% as.matrix(Y)

#p values come from looking at the diagonals of the matrix generated by
#solving sigma^2 * (X'X)^-1

#in other words, the total variance of the residuals times the inverse of x times x transpose
#the way to think about this is that to solve the minimizing of residuals, we take a derivative and
#set it equal to zero, making the betas a function of X's and Y's.  We also know that Y is
#normally distributed, and since B depends only on X and a normally distributed variable with
#variance sigma^2, we can say what kind of distribution each of the B's must have.
reg$coef[1]/sqrt(co[1,1])
reg$coef[2]/sqrt(co[2,2])
reg$coef[3]/sqrt(co[3,3])
summary(reg)



#we can assign household type to individuals if we want to (I'm a bit unclear from )
#note we could also just use merge() on hh_id to do an inner join if we wanted to here
individual$household_type <- apply(individual, 1, function(ind, household){
  type = household[household$hh_id == as.numeric(ind["hh_id"]),][1,"type"]
  return(type)
}, household)

summary(individual)

hir <- hir[,-grep("household_type",colnames(hir))]





setwd("~/github/ml-lab/data")
train = read.csv("cardif_claims.csv")
test = read.csv("cardif_claims_test.csv")




