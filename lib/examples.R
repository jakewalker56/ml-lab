#Set current Directory
location = "~/github"
path= "/ml-lab/data/"
setwd(paste(location, path, sep=""))

#Trees
#required libs
library(tree)
#sample data
library(MASS)
attach(Boston)
#sample code
t1 = tree(medv ~ lstat, data=Boston, mindev=0.0001)
summary(t1)
plot(t1)
