location = "~/github"
path= "/ml-lab/data/"
setwd(paste(location, path, sep=""))
library(XLConnect)
library(ggplot2)
library(nloptr)
source('../utilities/convert_to_numeric.R')

#Q1
df <- readWorksheetFromFile("AFC_hw_1.xls", 
                            sheet=1, 
                            startRow = 4,
                            endCol = 8)
df <- convert_to_numeric(df)
num_countries = ncol(df)-1
colors <- rainbow(num_countries)  
img="afc_hw_1.png"
png(filename=img, width = 800, height = 400, units = "px")
  #plot a blank canvas
  plot(df$Year,df[,2], col=colors[1], type="n",
       xlab = "Year", ylab = "Index Value",
       main = paste("Industrial Production by Country, ", min(df$Year)
,"-", max(df$Year)),
       ylim=range(unlist(df[,-1])))
  
  for(i in 2:(num_countries + 1)){ 
    lines(df$Year, df[,i], col=colors[i-1], lty=1) 
  }
  
  legend("topleft", bty="n", lwd=1, cex=0.75, pt.cex = 1,
         col=colors,
         legend=colnames(df[,-1]))
dev.off()

#Q2
df2 <- readWorksheetFromFile("AFC_hw_1.xls", 
                            sheet=2, 
                            startRow = 4,
                            endCol = 8)
df2 <- convert_to_numeric(df2)
num_countries2 = ncol(df2)-1
colors2 <- rainbow(num_countries2)  

img="afc_hw_2.png"
png(filename=img, width = 800, height = 400, units = "px")
#plot a blank canvas
plot(df2$Year,df2[,2], col=colors2[1], type="n",
     xlab = "Year", ylab = "Index Value",
     main = paste("Wholesale Prices by Country, ", min(df2$Year)
                  ,"-", max(df2$Year)),
     ylim=range(unlist(df2[,-1])))

for(i in 2:(num_countries2 + 1)){ 
  lines(df2$Year, df2[,i], col=colors2[i-1], lty=1) 
}

legend("topleft", bty="n", lwd=1, cex=0.75, pt.cex = 1,
       col=colors2,
       legend=colnames(df2[,-1]))

dev.off()

#Q3
df3 <- readWorksheetFromFile("AFC_hw_1.xls", 
                             sheet=3, 
                             startRow = 1,
                             endCol = 7)
df3 <- convert_to_numeric(df3)

consumption_utility <- function(value, index, beta) {
  if (value <= 0) {
    return(0)
  }
  else {
    return(beta^(index - 1) * log(value))
  }
}
opt_util_func <- function(values, betas) {
  #maximize this function
  #assume a utility...
  return(sum(mapply(consumption_utility, values, seq_along(values), betas)))
}
eval_f <- function(x) {
  return(opt_util_func(x,df3$beta))
}
neg_eval_f <- function(x) {
  return(-1*eval_f(x))
}
discount <- function(value, index, i) {
  return(value/i^(index - 1))
}
eval_g_ineq <- function(x){
  #a = prices
  #b = wages
  con = sum(mapply(discount, x*df3$p, seq_along(x), df3$I))
  wag = sum(mapply(discount, df3$p*df3$w, seq_along(x), df3$I))
  return(con-wag)
}
#find consumption such that I maximize the following:
# SUM((B)^t * U(ct))
# s.t. SUM(ct*pt / (1+i)^t) = SUM(wt*pt / (1+i)^t)
x0 <- df3$w
x0
res <- nloptr( x0=x0, 
               eval_f=eval_f,
               lb = rep(0,length(x0)), 
               ub = rep(Inf,length(x0)), 
               eval_g_ineq=eval_g_ineq,
               opts = list("algorithm"="NLOPT_LN_COBYLA","maxeval"="500"),
              )

res2 <- nloptr( x0=x0, 
               eval_f=neg_eval_f,
               lb = rep(0,length(x0)), 
               ub = rep(Inf,length(x0)), 
               eval_g_ineq=eval_g_ineq,
               opts = list("algorithm"="NLOPT_LN_COBYLA","maxeval"="500"),
              )

print( res )
print( res2 )
neg_eval_f(x0)
eval_g_ineq(c(81.48899, 98.06975, 105.1299, 114.7128, 116.8749, 120.7786, 
           0.00137276, 0.0003168575))
eval_g_ineq(x0)
df3$w
df3$p
#Q4
df4 <- readWorksheetFromFile("AFC_hw_1.xls", 
                             sheet=4, 
                             startRow = 1,
                             endCol = 5)
df4$NormalizedExports <- df4$Exports / df4$GDP
df4$TariffDelta <- 0
df4$ExportDelta <- 0 
df4$GDPDelta <- 0 
for(country in unique(df4$Country)) {
  df4[df4$Country == country,]$TariffDelta <- c(0,head(c(df4[df4$Country == country,][-1,]$Tariff, 0) - df4[df4$Country == country,]$Tariff, -1))
  df4[df4$Country == country,]$ExportDelta <- c(0,head(c(df4[df4$Country == country,][-1,]$NormalizedExports, 0) - df4[df4$Country == country,]$NormalizedExports, -1))
  df4[df4$Country == country,]$GDPDelta <- c(0,head((c(df4[df4$Country == country,][-1,]$GDP, 0) - df4[df4$Country == country,]$GDP)/df4[df4$Country == country,]$GDP, -1))
}

img="afc_hw_4_a.png"
png(filename=img, width = 800, height = 400, units = "px")
q4_subset = df4[df4$Year != 1928,]
x = q4_subset$TariffDelta
y = q4_subset$GDPDelta
plot(x, y, xlab = "YoY Change in Tariff Rate", ylab = "YoY Change in GDP",
     main = paste("YoY Change in GDP by Change in YoY Tariff Rate, ", min(q4_subset$Year)
                  ,"-", max(q4_subset$Year)),
     xlim=c(min(x)-.01,max(x)+.01),
     col=q4_subset$Year)

reg = lm(y ~ x)
summary(reg)
abline(reg$coef[1], reg$coef[2])

legend("topleft", bty="o", lty=0, cex=0.75, pt.cex = 1,
       pch=1,
       col=unique(q4_subset$Year),
       legend=unique(q4_subset$Year))

dev.off()

img="afc_hw_4_b.png"
png(filename=img, width = 800, height = 400, units = "px")
x = q4_subset$TariffDelta
y = q4_subset$ExportDelta
plot(x, y, xlab = "YoY Change in Tariff Rate", ylab = "YoY Change in Normalized Export Level",
     main = paste("YoY Change in Exports as a percentage of GDP by YoY Change in Tariff Rate, ", min(q4_subset$Year)
                  ,"-", max(q4_subset$Year)),
     xlim=c(min(x)-.01,max(x)+.01),
     col=q4_subset$Year)

reg = lm(y ~ x)
summary(reg)
abline(reg$coef[1], reg$coef[2])

legend("topleft", bty="o", lty=0, cex=0.75, pt.cex = 1,
       pch=1,
       col=unique(q4_subset$Year),
       legend=unique(q4_subset$Year))

dev.off()

#Q5
df5 <- readWorksheetFromFile("AFC_hw_1.xls", 
                             sheet=5, 
                             startRow = 1,
                             endCol = 4)
img="afc_hw_5.png"
png(filename=img, width = 800, height = 400, units = "px")
x = df5$wholesale_prices
y = df5$money_supply

plot(x, y, xlab = "YoY Change in Wholesale Prices", ylab = "YoY Change in Money Supply",
     main = paste("YoY Change in Wholesale Prices by YoY Change in Money Supply, ", min(df5$Year)
                  ,"-", max(df5$Year)),
     xlim=c(min(x)-.1,max(x)+.01),
     col=df5$Year)

reg = lm(y ~ x)
summary(reg)
abline(reg$coef[1], reg$coef[2])

legend("topleft", bty="o", lty=0, cex=0.75, pt.cex = 1,
       pch=1,
       col=unique(df5$Year),
       legend=unique(df5$Year))

dev.off()


#Q6
df6 <- readWorksheetFromFile("AFC_hw_1.xls", 
                             sheet=6, 
                             startRow = 1,
                             endCol = 7)
df6
img="afc_hw_6.png"
png(filename=img, width = 800, height = 400, units = "px")
x = df6$money_supply_change
y = df6$gold_change
plot(x, y, xlab = "Change in Money Supply", ylab = "Change in Gold Reserves",
     main = "Change in Money Supply by Change in Gold Reserves, 1929-1932",
     xlim=c(min(x)-.1,max(x)+.01),
     col=factor(df6$country))
reg = lm(y ~ x)
summary(reg)
abline(reg$coef[1], reg$coef[2])

legend("topleft", bty="o", lty=0, cex=0.75, pt.cex = 1,
       pch=1,
       col=factor(unique(df6$country)),
       legend=unique(df6$country))

dev.off()

#Q7
df7 <- readWorksheetFromFile("AFC_hw_1.xls", 
                             sheet=8, 
                             startRow = 1,
                             endCol = 3)
df7
img="afc_hw_7.png"
png(filename=img, width = 800, height = 400, units = "px")
x = df7$real_wages_change
y = df7$production_change
plot(x, y, xlab = "Change in Real Wages", ylab = "Change in Production",
     main = "Change in Production by Change in Real Wages, 1929-1933",
     xlim=c(min(x)-.1,max(x)+.01),
     col=factor(df7$country))
reg = lm(y ~ x)
summary(reg)
abline(reg$coef[1], reg$coef[2])

legend("topleft", bty="o", lty=0, cex=0.75, pt.cex = 1,
       pch=1,
       col=factor(unique(df7$country)),
       legend=unique(df7$country))

dev.off()

#Q7B
df7b <- readWorksheetFromFile("AFC_hw_1.xls", 
                             sheet=9, 
                             startRow = 1,
                             endCol = 4)
df7b
img="afc_hw_7b.png"
png(filename=img, width = 800, height = 400, units = "px")
x = df7b$bond_rate
y = df7b$price_change
plot(x, y, xlab = "Bond Rate", ylab = "Inflation",
     main = "Inflation by Bond Rate, 1929-1935",
     xlim=c(min(x)-.01,max(x)+.01),
     col=factor(df7b$country))
reg = lm(y ~ x)
summary(reg)
abline(reg$coef[1], reg$coef[2])

legend("topleft", bty="o", lty=0, cex=0.75, pt.cex = 1,
       pch=1,
       col=factor(unique(df7$country)),
       legend=unique(df7$country))

dev.off()

