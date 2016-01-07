## multi-armed bandit illustration

## first, what is a beta distribution?
sample1 <- rbeta(1000, 1, 1)
sample2 <- rbeta(1000, 7, 3)
sample3 <- rbeta(1000, 70, 30)
par(mfrow=c(1,3))
hist(sample1, col="grey", main="a,b = 1,1",xlim=c(0,1))
hist(sample2, col="grey", main="a,b = 7,3",xlim=c(0,1))
hist(sample3, col="grey", main="a,b = 70,30",xlim=c(0,1))
## it is a distribution for values between zero and one,
## with mean a/b and variance that shrinks as a+b grows.

# now, our ad simulation
true_click_probs <- (0:9)/10
shows <- clicks <- rep(0,10) 
fullsample <- 20
for(i in 1:fullsample){
	q <- rbeta(10, clicks, shows-clicks) # sample click prob guesses
	j <- which.max(q) # show whichever is max
	shows[j] <- shows[j] + 1
	clicks[j] <- clicks[j] + rbinom(1, size=1, prob=true_click_probs[j])
	print(which.max(clicks/shows)) # which ad do you think is best at this time?
}
