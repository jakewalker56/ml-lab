#0.1.i
x <- -10
u <- -10
sigma <- 25
z <- (x-u)/sigma
prob <- 1- pnorm(abs(z))
prob

#0.1.ii
x <- -20
u <- -10
sigma <- 25
z <- (x-u)/sigma
prob <- pnorm(-abs(z))
prob

#0.1.iii
x1 <- -0.5
x2 <- 0.5
u <- -10
sigma <- 25
z1 <- (x1-u)/sigma
z2 <- (x2-u)/sigma
prob <- pnorm(abs(z2)) - pnorm(abs(z1))
prob

#0.1.iv
x1 <- -22
x2 <- -12
u <- -10
sigma <- 25
z1 <- (x1-u)/sigma
z2 <- (x2-u)/sigma
prob <- pnorm(z2) - pnorm(z1)
prob


#1.1.i
price <- c(1.5, 1.5, 1.75, 2.0, 2.0, 2.25, 2.5, 2.5)
sales <- c(420, 450, 420, 380, 440, 380, 360, 360)
mean(sales)
u = 390
sig2estimate = sum((sales- u)^2)/length(sales)
sig2estimate

#1.1.ii
price <- c(1.5, 1.5, 1.75, 2.0, 2.0, 2.25, 2.5, 2.5)
sales <- c(420, 450, 420, 380, 440, 380, 360, 360)
mean(sales)
m = -60
b = 500
sig2estimate = sum((sales- (m* price + b))^2)/length(sales)
sig2estimate

#1.1.iii
price <- c(1.5, 1.5, 1.75, 2.0, 2.0, 2.25, 2.5, 2.5)
sales <- c(420, 450, 420, 380, 440, 380, 360, 360)
cor = cor(price,sales)
cor
m = cor * sqrt(var(sales))/sqrt(var(price))
b = mean(sales) - mean(price) * m
plot(price, sales, pch=18)
abline(b, m, col="red")
legend("topleft", "y = -75.56*x + 552.36", fill=2:2)
cor

#1.1.iv
price <- c(1.5, 1.5, 1.75, 2.0, 2.0, 2.25, 2.5, 2.5)
sales <- c(420, 450, 420, 380, 440, 380, 360, 360)
cor = cor(price,sales)
cor
m = cor * sd(sales)/sd(price)
m
b = mean(sales) - mean(price) * m
b
residuals = sum(sales- (m* price + b))/length(sales)
residuals


#1.1.v
price <- c(1.5, 1.5, 1.75, 2.0, 2.0, 2.25, 2.5, 2.5)
sales <- c(420, 450, 420, 380, 440, 380, 360, 360)
cor = cor(price,sales)
cor
m = cor * sqrt(var(sales))/sqrt(var(price))
b = mean(sales) - mean(price) * m
sig2estimate = sum((sales- (m* price + b))^2)/length(sales)
sig2estimate

#1.2.i
teach <- read.csv("teach.csv")
par(mfrow=c(1,1))
summary(teach)
plot(teach$months, teach$salary, pch=18, col=teach$sex, xlab="months", ylab="salary")
legend("topleft", levels(teach$sex), fill=1:2)

#1.2.ii
teach <- read.csv("teach.csv")
summary(teach)
par(mfrow=c(1,6))
boxplot(salary ~ sex, data=teach, xlab="sex", ylab="salary")
boxplot(salary ~ marry, data=teach, xlab="marry", ylab="")
boxplot(salary ~ degree, data=teach, xlab="degree", ylab="")
boxplot(salary ~ type, data=teach, xlab="type", ylab="")
boxplot(salary ~ train, data=teach, xlab="train", ylab="")
boxplot(salary ~ brk, data=teach, xlab="break", ylab="")

#1.2.iii
#use teach$degree + 1 to avoid plotting white datapoints on a white background
par(mfrow=c(1,1))
plot(teach$months, teach$salary, col=(teach$degree + 1), pch=18, xlab="months", ylab="salary")
legend("topleft", paste("Degree ", sort(as.character(unique(teach$degree)))), fill=1:4)

#1.2.iv
model<-lm(teach$salary ~ teach$sex)
anovamodel_sex <- anova(model)
ssr = anovamodel_sex$Sum[1]
sse = anovamodel_sex$Sum[2]
sst = ssr + sse
#percent explained by the grouping with sex
ssr / sst
model
anova(model)
sse
ssr
sst

model<-lm(teach$salary ~ teach$degree)
anovamodel_degree <- anova(model)
ssr = anovamodel_degree$Sum[1]
sse = anovamodel_degree$Sum[2]
sst = ssr + sse
#percent explained by the grouping with degree
ssr / sst

#1.2.iv manual implementation
# SalaryMean <- function(arg1){
#   return(mean(arg1$salary))
# }
# GroupError <- function(arg1){
#   return(sum((SalaryMean(arg1) - arg1$salary)^2)) 
# }
# MultiDimSalaryLength <- function(arg1)
# {
#   return (length(arg1$salary))
# }
# maleteach <- teach[teach$sex=="M",]
# femaleteach <- teach[teach$sex=="F",]
# allteach <- list(maleteach, femaleteach)
# #SST = SSE + SSR
# # Sum of Squares Total (sq(total mean - value)
# sst = sum((mean(teach$salary) - teach$salary)^2)
# sst
# # Sum of Squares Error (sq(group mean - value))
# sse = sum(as.numeric(lapply(allteach,GroupError)))
# sse
# #  Sum of Squares Regression (sq(group mean - total mean)
# ssr = sum(as.numeric(lapply(allteach,MultiDimSalaryLength))  * (SalaryMean(teach) - as.numeric(lapply(allteach,SalaryMean)))^2)
# ssr
# sse
# sst

# 1.2.v
teach0 <- teach[teach$degree == 0,]
cor = cor(teach0$salary,teach0$months)
cor
m = cor * sqrt(var(teach0$salary))/sqrt(var(teach0$months))
b = mean(teach0$salary) - mean(teach0$months) * m
cor
m
b
sd(teach0$salary)
sd(teach0$months)
mean(teach0$months)
mean(teach0$salary)
#1.2.vi
plot(teach0$months, teach0$salary, pch=18,  ylim=c(900,2600), xlab="months", ylab="salary")
abline(b, m, col="red")
legend("topleft", paste("y = " , round(m, digits=2), "*x + ", round(b, digits=2)), fill=2:2)

residuals = teach0$salary - m*teach0$months - b
hist(residuals)
plot(teach0$month,residuals, pch=18, xlab="months", ylab="salary residuals")
model=lm(residuals ~ teach0$months)
abline(model, col=2)
model
residual_model = anova(model)
ssr = residual_model$Sum[1]
sse = residual_model$Sum[2]
sst = ssr + sse
#percent explained by the grouping with degree
ssr / sst
(sum(residuals^2)/(length(residuals)-1))^0.5
plot(teach0$months, teach0$salary, pch=18, xlab="months", ylab="salary", ylim=c(900,2600))
model=lm(teach0$salary ~ poly(teach0$months, 2, raw=TRUE))
summary(model)

points(teach0$months, predict(model), type="l", col="red", lwd=2)
legend("topleft", paste("y = ", 962.99," + ", 5.775, "x - ", 0.007433, "x^2"), fill=2:2)
model=lm(teach0$salary ~ poly(teach0$months, 1, raw=TRUE))
summary(model)
