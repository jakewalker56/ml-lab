data(airquality)
laq <- log(airquality[,1:4])
mle <- glm(Ozone ~ Solar.R+Wind+Temp, data=laq)

n <- nrow(airquality)
gamma <- c()

for(b in 1:100){
   ib <- sample(1:n,n,replace=TRUE)
   fb <- glm(Ozone ~ Solar.R+Wind+Temp, data=laq[ib,])
   gamma <- c(gamma,coef(fb)["Temp"]) }

hist(gamma)
abline(v=coef(mle)["Temp"], col=2)

## same thing, but add theoretical CI to plot
# plot it (lots going on here; use help and feel free to ask me about it)
hist(gamma, col="grey40", border="grey90", main="",xlab="gamma")
# mark the mean
abline(v=mean(gamma))
# and mark the 95% confidence interval + estimate for gamma from BCH
ghat <- coef(summary(mle))["Temp",1]
gse <- coef(summary(mle))["Temp",2]

polygon(x=c(rep(ghat-2*gse,2),rep(ghat+2*gse,2)),
		y=c(0,100,100,0), col=rgb(0,1,0,.2), border=NA)
abline(v=ghat,col="green")
legend("topright", fill=c(1,"green"), bty="n",
	legend=c("bootstrap (x rand)","95% CI (x given)") )
