#Windows
Dropbox = "C:\\users/Jake/Dropbox"
#Mac/Unix
Dropbox = "~/Dropbox"

path= "/Booth/Winter 2014/Financial Econometrics/Week 3/"
setwd(paste(Dropbox, path, sep=""))
#xy = read.csv("xyseris.csv")
GenerateAR1Series <- function (args)
{
  u = args[1]/(1-args[2])
  series = c(u)
  rand = rnorm(200, mean = 0, sd = args[3]^0.5)
  for(i in 2:200)
  {
    series[i] = args[1] + series[i-1] * args[2] + rand[i];
  }  
  return(series);
}

GenerateMA1Series <- function (args)
{
  u = args[1];
  theta = args[2];
  rand = rnorm(1000, mean = 0, sd = args[3]^0.5)
  series = rand[1];
  for(i in 2:1000)
  {
    series[i] = args[1] + rand[i-1] * args[2] + rand[i];
  }  
  return(series);
}
#1.1

#args = c(b0, b1, s2)
args1 = c(0,0,1)
args2 = c(0,0.9,1)
args3 = c(0,0.1,1)
args4 = c(0,0-.5,1)
args5 = c(1,0.9,1)

series_1 = GenerateAR1Series(args1)
series_2 = GenerateAR1Series(args2)
series_3 = GenerateAR1Series(args3)
series_4 = GenerateAR1Series(args4)
series_5 = GenerateAR1Series(args5)

#a
mean(series_1)
mean(series_2)
mean(series_3)
mean(series_4)
mean(series_5)

#b
png(filename=paste(sep="", Dropbox, path, "1.b.i.png"))
plot(series_1, pch = 20)
lines(series_1)
dev.off()
png(filename=paste(sep="", Dropbox, path, "1.b.ii.png"))
plot(series_2, pch = 20)
lines(series_2)
dev.off()
png(filename=paste(sep="", Dropbox, path, "1.b.iii.png"))
plot(series_3, pch = 20)
lines(series_3)
dev.off()
png(filename=paste(sep="", Dropbox, path, "1.b.iv.png"))
plot(series_4, pch = 20)
lines(series_4)
dev.off()
png(filename=paste(sep="", Dropbox, path, "1.b.v.png"))
plot(series_5, pch = 20)
lines(series_5)
dev.off()

# time seires with high b1 values (series_2 and series_5) appear to be much more correlated- 
# that is, they cross the mean line far less often, and they tend to stay in the same value area 
# for longer periods of time

#c

# Yes, all of these series are mean reverting, because |b1| < 0 for all of them

#2.a
args1 = c(1, 0.8, 1)
args2 = c(0, -0.8, 1)

series_1 = GenerateMA1Series(args1)
series_2 = GenerateMA1Series(args2)

png(filename=paste(sep="", Dropbox, path, "2.a.i.png"))
plot(series_1, pch = 20)
lines(series_1)
dev.off()
png(filename=paste(sep="", Dropbox, path, "2.a.i.2.png"))
acf(series_1, lag.max = 10)
dev.off()
png(filename=paste(sep="", Dropbox, path, "2.a.i.3.png"))
pacf(series_1, lag.max = 10)
dev.off()
png(filename=paste(sep="", Dropbox, path, "2.a.ii.png"))
plot(series_2, pch = 20)
lines(series_2)
dev.off()
png(filename=paste(sep="", Dropbox, path, "2.a.ii.2.png"))
acf(series_2, lag.max = 10)
dev.off()
png(filename=paste(sep="", Dropbox, path, "2.a.ii.3.png"))
pacf(series_2, lag.max = 10)
dev.off()

mean(series_1)
mean(series_2)

#2.b
MA1_series_1 = arima(series_1, order=c(0,0,1));
MA1_series_1
MA1_series_2 = arima(series_2, order=c(0,0,1));
MA1_series_2

AR10_series_1 = arima(series_1, order=c(10, 0, 0))
AR10_series_1

#turns out t-stats are coef/se, and se is calculated with the below formula
AR10_series_1_t_stat = AR10_series_1$coef/sqrt(diag(AR10_series_1$var.coef))
AR10_series_1_t_stat

#2.c
# We expect the slope coefficients to be continuously decreasing 
# with signs alternating between positive and negative 
# (indentical to the PCF values of the series).  This is exactly 
# what we see.