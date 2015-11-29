#1.1.i
samplesize = 100
sd_x = 2^0.5
sd_e = 3^0.5
x = rnorm(samplesize, mean = 0, sd = sd_x)
e = rnorm(samplesize, mean = 0, sd = sd_e)
y = 2.5 - 1.0 * x + e
plot(x,y, ylim=c(-5, 10), pch=20)
model = lm(y~x)
abline(model, col="red")
abline(2.5, -1.0, col="green")
legend("topright", 
       c(paste("y = ", round(model$coefficients[2], digits = 2), 
             "x + ", round(model$coefficients[1], digits=2)), 
         paste("y = ", -1.0, "x + ", 2.5)), 
      col=c("red","green"), fill=c(2:3))

#1.1.ii
x1 = x[c(0:(samplesize * 0.25 - 1))]
y1 = y[c(0:(samplesize * 0.25 - 1))]
x2 = x[c((samplesize * 0.25):(samplesize-1))]
y2 = y[c((samplesize * 0.25):(samplesize-1))]
plot(x,y, ylim=c(-10, 20), pch=20)
model1 = lm(y1~x1)
model2 = lm(y2~x2)
abline(model1, col="red")
abline(model, col="green")
legend("topright", 
       c(paste("y(1,25) = ", round(model1$coefficients[2], digits = 2), 
               "x + ", round(model1$coefficients[1], digits=2)), 
         paste("y(26,100) = ", round(model2$coefficients[2], digits = 2), 
               "x + ", round(model2$coefficients[1], digits=2))), 
       col=c("red","green"), fill=2:3)

#1.1.iii
samplemarginalmean = mean(y)
truemarginalmean = 2.5
paste("sample marginal mean = ", round(samplemarginalmean, digits=2))
paste("true marginal mean = ", round(truemarginalmean, digits=2))
samplemarginalmean

#1.1.iv
qnorm95 = qnorm(0.95, mean = 0, sd = sd_e)
qnorm5 = qnorm(0.05, mean = 0, sd = sd_e)
qnorm95
qnorm5
abline(2.5 + qnorm95, - 1.0, col=4, lty=2)
abline(2.5 + qnorm5, - 1.0, col=4, lty=2)
legend("bottomleft", 
       c(paste("y_95th = -1.0x + ", round(2.5 + qnorm95, digits=2)),
         paste("y_5th = -1.0x + ", round(2.5 + qnorm5, digits=2))),
         col=4, fill=4:4)
yvalue = data.frame(value=y)
yvalue
yvalue$value
y95 = yvalue[yvalue$value > (2.5 + qnorm95 - x),]
y5 = yvalue[yvalue$value < (2.5 + qnorm5 - x),]
length(y95)/length(y) + length(y5)/length(y)

#1.2.i
tractors = read.csv("tractor.csv")
plot(tractors$age, tractors$cost, pch=20, ylab = "cost", xlab = "age")

#1.2.ii
cor=cor(tractors$cost, tractors$age)
sdcost = sd(tractors$cost)
sdage = sd(tractors$age)
ecost = mean(tractors$cost)
eage = mean(tractors$age)
cor
sdcost
sdage
ecost
eage
model=lm(tractors$cost ~ tractors$age)
m= cor*sdcost/sdage
b= ecost-m*eage
m
b
abline(model, col="red")
legend("topleft", 
       paste("y = ", round(model$coefficients[[2]], digits = 2), 
             "x + ", round(model$coefficients[[1]], digits=2)), 
       col="red", fill=2:2)

#1.2.iii
anova = anova(model)
anova
SSE=anova[2,2]
SSR=anova[1,2]
SST = SSR + SSE
SSR
SSE
SST
#R^2 = SSR/SST
SSR/SST

#1.2.iv
model$coefficients
y = model$coefficients[[2]] * 3 + model$coefficients[[1]]
y
y97=y + qnorm(0.975, sd=300)
y3=y + qnorm(0.025, sd=300)
y97
y3


#1.3.i
mkt <- read.csv("mktmodel.csv")
SP500 <- mkt$SP500
summary(SP500)
stocks <- mkt[,-1]
plot(SP500, col=0, ## Just get the plot up
     xlab = "Month", ylab = "Returns",
     main = "Monthly returns for 1992-1996",
     ylim=range(unlist(mkt)))
colors <- rainbow(30)  ## 30 different colors
## this is how you do 'loops' in R... this is useful!
for(i in 1:30){ lines(stocks[,i], col=colors[i], lty=2) }
lines(SP500, lwd=2)

#1.3.ii
cor=cor(SP500, stocks)
max=max(cor)
cor
max
cor[,cor==max]

#1.3.iii
stocks_models=lm(as.matrix(stocks) ~ SP500)
plot(stocks_models$coefficients[1,], stocks_models$coefficients[2,], xlab="alpha", ylab="beta", col=0)

#plot(CAPM$coeff[2,], CAPM$coeff[1,],ylab="alpha", xlab="beta", col=0)
text(x=stocks_models$coeff[1,], y=stocks_models$coeff[2,],labels=names(stocks), col=2)

stocks_models
#1.3.iv
#return = A1 + B1x - (A2 + B2x) = A1 - A2 if B1 and B2 are equal
SP500

#i.3.v/vi
Total_Return <- function(arg1){
  result = 1
  for(j in 1:length(arg1))
  {
    result = result * (1 + arg1[j])
  }
  return(prod(arg1 + 1) - 1)
    #result - 1)
}
buy = 27
sell = 23
stocks$UK
stocks$MRK
stocks_models

alpha_diff = stocks_models$coefficients[1,buy] - stocks_models$coefficients[1,sell]
beta_diff = stocks_models$coefficients[2,buy] - stocks_models$coefficients[2,sell]
beta_diff
alpha_diff
returns = SP500 * beta_diff + alpha_diff
returns
total_returns = Total_Return(returns)
total_returns
paste("Eyeball ideally, Buy ", names(stocks)[buy],", Sell", names(stocks)[sell], ", for a ", 
      round(total_returns * 100, digits=2), 
      "% total return, ", 
      round(((1 + total_returns)^(1/length(stocks[,buy]))-1) * 100, digits=2),
      "% average monthly return")
Total_Return(stocks_models$coefficients[2,buy]*SP500 + stocks_models$coefficients[1,buy])
Total_Return(stocks_models$coefficients[2,sell]*SP500 + stocks_models$coefficients[1,sell])

paste("Eyeball ideally no rebalancing, Buy ", names(stocks)[buy],", Sell", names(stocks)[sell], ", for a ", 
      round(total_returns * 100, digits=2), 
      "% total return, ", 
      round(((1 + total_returns)^(1/length(stocks[,buy]))-1) * 100, digits=2),
      "% average monthly return")


returns = stocks[,buy] - stocks[,sell]
total_returns = Total_Return(returns)
paste("Eyeball with rebalancing, Buy ", names(stocks)[buy],", Sell", 
      names(stocks)[sell], ", for a ",
      round(total_returns * 100, digits=2), 
      "% total return, ", 
      round(((1 + total_returns)^(1/length(stocks[,buy]))-1) * 100, digits=2),
      "% average monthly return")

Total_Return(stocks[,sell])
Total_Return(stocks[,buy])
Total_Return(SP500)
sum(stocks[,sell])
sum(stocks[,buy])/length(stocks[,buy])
sum(SP500)/length(SP500)
stocks[,buy]
stocks_models
plot(stocks[,buy])
plot(SP500)
var(stocks[,buy])
var(SP500)
plot(SP500, stocks[,buy])
model=lm(stocks[,buy], SP500)
abline(model)

total_returns = Total_Return(stocks[,buy]) - Total_Return(stocks[,sell])
paste("Eyeball without rebalancing, Buy ", names(stocks)[buy],", Sell", 
      names(stocks)[sell], ", for a ",
      round(total_returns * 100, digits=2), 
      "% total return, ", 
      round(((1 + total_returns)^(1/length(stocks[,buy]))-1) * 100, digits=2),
      "% average monthly return")

prod(stocks[,buy] + 1) - 1
prod(stocks[,sell] + 1) - 1

####Summarize Results#######
stocks_models
paste("Ideally, Buy ", names(stocks)[buy_ideal],", Sell", names(stocks)[sell_ideal], ", for a ", 
      round(max_ideal_return * 100, digits=2), 
      "% total return, ", 
      round(((1 + max_ideal_return)^(1/length(stocks[,buy_ideal]))-1) * 100, digits=2),
      "% average monthly return")

paste("With rebalancing, Buy ", names(stocks)[buy_rebalancing],", Sell", 
      names(stocks)[sell_rebalancing], ", for a ",
      round(max_rebalancing_return * 100, digits=2), 
      "% total return, ", 
      round(((1 + max_rebalancing_return)^(1/length(stocks[,buy_rebalancing]))-1) * 100, digits=2),
      "% average monthly return")

paste("Without rebalancing, Buy ", names(stocks)[buy_static],", Sell", names(stocks)[sell_static],
      round(max_static_return * 100, digits=2), 
      "% total return, ", 
      round(((1 + max_static_return)^(1/length(stocks[,buy_static]))-1) * 100, digits=2),
      "% average monthly return")

#i.3.v/vi optimization
Total_Return <- function(arg1){
  result = 1
  for(j in 1:length(arg1))
  {
    result = result * (1 + arg1[j])
  }
  return(result - 1)
}

total_ideal_return_array_vals=c(1:900)
total_ideal_return_array_strings_buy=as.character(c(1:900))
total_ideal_return_array_strings_sell=as.character(c(1:900))
total_ideal_return_array = data.frame(vals = total_ideal_return_array_vals, 
  buy=as.character(total_ideal_return_array_strings_buy), 
  sell=as.character(total_ideal_return_array_strings_sell))

for(i in 1:30){
  for(k in 1:30){
    #buy stock i, sell stock k
    #first, calculate assuming the values match our idealized a/b exactly
    alpha_diff = stocks_models$coefficients[1,i] - stocks_models$coefficients[1,k]
    beta_diff = stocks_models$coefficients[2,i] - stocks_models$coefficients[2,k]
    ideal_returns = SP500 * beta_diff + alpha_diff
    total_ideal_return = Total_Return(ideal_returns)
    total_ideal_return_array$vals[i + 30*(k-1)] = total_ideal_return
    total_ideal_return_array$buy[i + 30*(k-1)] = i
    total_ideal_return_array$sell[i + 30*(k-1)] = k
  }
}

max_ideal_return = max(total_ideal_return_array$vals)
max_ideal_trade=total_ideal_return_array[total_ideal_return_array$vals==max_ideal_return,]
buy_ideal = as.integer(paste(max_ideal_trade$buy[[1]]))
sell_ideal = as.integer(paste(max_ideal_trade[[3]]))
paste("Ideally, Buy ", names(stocks)[buy_ideal],", Sell", names(stocks)[sell_ideal], ", for a ", 
      round(max_ideal_return * 100, digits=2), 
      "% total return, ", 
      round(((1 + max_ideal_return)^(1/length(stocks[,buy_ideal]))-1) * 100, digits=2),
      "% average monthly return")


total_rebalancing_return_array_vals=c(1:900)
total_rebalancing_return_array_strings_buy=as.character(c(1:900))
total_rebalancing_return_array_strings_sell=as.character(c(1:900))
total_rebalancing_return_array = data.frame(vals = total_rebalancing_return_array_vals, 
  buy=as.character(total_rebalancing_return_array_strings_buy), 
  sell=as.character(total_rebalancing_return_array_strings_sell))

for(i in 1:30){
  for(k in 1:30){
    #with monthly rebalancing
    returns = stocks[,i] - stocks[,k]
    returns
    total_rebalancing_return = Total_Return(returns)
    total_rebalancing_return_array$vals[i + 30*(k-1)] = total_rebalancing_return[[1]]
    total_rebalancing_return_array$buy[i + 30*(k-1)] = i
    total_rebalancing_return_array$sell[i + 30*(k-1)] = k
  }
}
max_rebalancing_return = max(total_rebalancing_return_array$vals)
max_rebalancing_trade = 
  total_rebalancing_return_array[total_rebalancing_return_array$vals==max_rebalancing_return,]
buy_rebalancing = as.integer(paste(max_rebalancing_trade[[2]]))
sell_rebalancing = as.integer(paste(max_rebalancing_trade[[3]]))
paste("With rebalancing, Buy ", names(stocks)[buy_rebalancing],", Sell", 
      names(stocks)[sell_rebalancing], ", for a ",
      round(max_rebalancing_return * 100, digits=2), 
      "% total return, ", 
      round(((1 + max_rebalancing_return)^(1/length(stocks[,buy_rebalancing]))-1) * 100, digits=2),
      "% average monthly return")

total_static_return_array_vals=c(1:900)
total_static_return_array_strings_buy=as.character(c(1:900))
total_static_return_array_strings_sell=as.character(c(1:900))
total_static_return_array = data.frame(vals = total_static_return_array_vals, 
                                            buy=as.character(total_static_return_array_strings_buy), 
                                            sell=as.character(total_static_return_array_strings_sell))
for(i in 1:30){
  for(k in 1:30){
    total_static_return = Total_Return(stocks[,i]) - Total_Return(stocks[,k])
    total_static_return_array$vals[i + 30*(k-1)] = total_static_return[[1]]
    total_static_return_array$buy[i + 30*(k-1)] = i
    total_static_return_array$sell[i + 30*(k-1)] = k
  }
}
max_static_return = max(total_static_return_array$vals)
max_static_trade=total_static_return_array[total_static_return_array$vals==max_static_return,]
buy_static = as.integer(paste(max_static_trade[[2]]))
sell_static = as.integer(paste(max_static_trade[[3]]))

paste("Without rebalancing, Buy ", names(stocks)[buy_static],", Sell", names(stocks)[sell_static],
      round(max_static_return * 100, digits=2), 
      "% total return, ", 
      round(((1 + max_static_return)^(1/length(stocks[,buy_static]))-1) * 100, digits=2),
      "% average monthly return")


####Summarize Results#######
stocks_models
paste("Ideally, Buy ", names(stocks)[buy_ideal],", Sell", names(stocks)[sell_ideal], ", for a ", 
      round(max_ideal_return * 100, digits=2), 
      "% total return, ", 
      round(((1 + max_ideal_return)^(1/length(stocks[,buy_ideal]))-1) * 100, digits=2),
      "% average monthly return")

paste("With rebalancing, Buy ", names(stocks)[buy_rebalancing],", Sell", 
      names(stocks)[sell_rebalancing], ", for a ",
      round(max_rebalancing_return * 100, digits=2), 
      "% total return, ", 
      round(((1 + max_rebalancing_return)^(1/length(stocks[,buy_rebalancing]))-1) * 100, digits=2),
      "% average monthly return")

paste("Without rebalancing, Buy ", names(stocks)[buy_static],", Sell", names(stocks)[sell_static],
      round(max_static_return * 100, digits=2), 
      "% total return, ", 
      round(((1 + max_static_return)^(1/length(stocks[,buy_static]))-1) * 100, digits=2),
      "% average monthly return")

#don't forget to comment on how a high variation in alpha is actually really bad
#note that you have to be allowed to trade on margin of >= 2x to get this trade.  
#Otherwise, you could only use half your money, and your returns would be 1/2 of what they are
#...the problem is that historical alphas aren't necessaritly indicative of future alphas.  In fact, 
#expected alpha values ought to be priced into the market.