k = 99
data = c(rep(1,1))
hist(data, breaks=seq(0,1,l=10))
for(i in 1:k)
{
  data <- c(data, rep(1 - 0.01*i, 1))
}
hist(data, breaks=seq(0,1,l=11))
data <- c(data, rep(0.01, 9))
hist(data, breaks=seq(0,1,l=11))
hist(data, breaks=seq(0,1,l=101))
