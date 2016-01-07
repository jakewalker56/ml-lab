## a few alternatives for calculating PCs on big sparse datasets

### testing
n <- 1000
p <- 200
N <- 10000

library(Matrix)

## big 'ol matrix of noise
Xraw <- sparseMatrix(i=sample.int(n,size=N,replace=TRUE),
						j=sample.int(p,size=N,replace=TRUE),
						x=rnorm(N))

## standardize
Xprec <- Diagonal(x=1/apply(Xraw,2,sd)) ## diagonal of 1/sd for each variable
X <- Xraw%*%Xprec
## double check scaling
apply(X,2,sd)
## note that X is off-center.  
## This is important, since we don't want
## to lose our nice sparsity!

## calculate the covariance X'X
xm <- colSums(X)/n
xx <- crossprod(X)
## this is what crossprod does:
all(xx==t(X)%*%X)
## calculate the covariance
xvar <- xx/(n-1) - xm%*%t(xm)*n/(n-1)
## double check again (not exactly the same, but close)
mean(abs(cov(as.matrix(X))-xvar))

## rotations are the eigen 'vectors'
e <- eigen(xvar, symmetric=TRUE)
## and pc score Z is (X-xm)'phi (i.e. centered X's)
## lets grab the first 10 directions
ze <- X%*%e$vec[,1:10]
## this is how much they are off since we didn't center X
shift <-  t(xm)%*%e$vec[,1:10]  
## we can use scale to center them (not a huge deal)
ze <- scale(ze, center=shift, scale=FALSE)

## compare to what we know
pca <- prcomp(X)
zpc <- predict(pca)[,1:10]
## the signs will be off in some directions, but rest is same.
plot(ze,zpc)

## now, for really big data you can use the irlba package
## given xvar calculated as above, it uses fast methods
## to get approximations to the first few eigenvectors
library(irlba) #install from CRAN
# nv is the number of PCs you want.
# this is called the 'singular value decomposition'
# it is a generalization of our eigen/spectral decomp
svd <- irlba(X,nv=10) 
## check it out; svd$v is an approx to eigen vectors..
## again, the signs are arbitrary
plot(e$vec[,1:10],svd$v) 
## calculate the implided PC directions
zsv <- X%*%svd$v
## as above, these need to be shifted since X is off-center
zsv <- scale(zsv, center=shift, scale=FALSE)
## check the results are similar but not exact
plot(zsv,ze)  





























