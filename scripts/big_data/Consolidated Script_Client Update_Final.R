########################################################################
#### ADVANTEK PRODUCT PORTFOLIO ANALYSIS ####
########################################################################

setwd("C:/Users/Maverick/Google Drive/Business school/Courses/Spring 2015/Big Data/Data")

library(fBasics)
library(quantmod)
library(xtable)
library(igraph)
library(gamlr)
library(maptpx)
library(wordcloud)
library(doBy)
library(reshape)
library(car)
library(data.table)
library(tree)
library(randomForest)
library(scales)
library(plyr)
library(xlsx)

source('naref.R')
source('kIC.R')
source('fdr.R')
source('reverse_naref.R')

data <- read.csv("C:/Users/Maverick/Google Drive/Business school/Courses/Spring 2015/Big Data/Big Data Group/Final Project/Data/SLS001-Sales and Profitability_Raw Data.csv")




########################################################################
#### Data Cleaning ####
########################################################################

trimnumerics<- function(d){
  for(di in colnames(d)) {
    if(!is.numeric(d[,di])){
      #drop the column    
      d[,di] <- NULL
    }
  }
  return(d)
}

trimdata <- function (d){
  #some values incorrectly assumed to be factor b/c of null values
  d[,"Sec_unit_factor"] <- as(levels(d[,"Sec_unit_factor"]),"numeric")[d[,"Sec_unit_factor"]]
  d[,"Sec_unit_qty_c"] <- as(levels(d[,"Sec_unit_qty_c"]),"numeric")[d[,"Sec_unit_qty_c"]]
  d[,"Size_ID"] <- as(levels(d[,"Size_ID"]),"numeric")[d[,"Size_ID"]]
  d[,"Tenure"] <- as(levels(d[,"Tenure"]),"numeric")[d[,"Tenure"]]
  
  
  
  #Factorize Inv_month, Inv_year, Inv_day, Inv_quarter
  d[,"Inv_month"] <- naref(factor(d[,"Inv_month"]))
  d[,"Inv_year"] <- naref(factor(d[,"Inv_year"]))
  d[,"Inv_date"] <- naref(factor(d[,"Inv_date"]))
  d[,"Inv_quarter"] <- naref(factor(d[,"Inv_quarter"]))
  
  #"USD extended price - USD total cost extended. divided by secondary qty 
  #would give you unit margin"
  d[,"USD_unit_price_correct"] <- d[,"USD_extended_price"] / d[,"Sec_unit_qty_c"]
  d[,"USD_std_unit_cost_correct"] <- d[,"USD_total_extended_cost"] / d[,"Sec_unit_qty_c"]
  d[,"USD_unit_profit"] <- (d[,"USD_extended_price"] - d[,"USD_total_extended_cost"])/d[,"Sec_unit_qty_c"]
  d[,"USD_unit_margin"] <- (d[,"USD_extended_price"] - d[,"USD_total_extended_cost"])/d[,"USD_extended_price"]
  
  
  for(di in colnames(d)) {
    if(is.numeric(d[,di])){
      #bad data!  BAD DATA!
      d[,di][is.na(d[,di])] <- 0
      d[,di][is.nan(d[,di])] <- 0
      d[,di][is.null(d[,di])] <- 0
      d[,di][d[,di]==Inf] <- 0
      d[,di][d[,di]==-Inf] <- 0
    }
    if(is.factor(d[,di])){
      #bad data!  BAD DATA!
      d[,di][is.null(d[,di])] <- NA
    }
  }
  
  # description has some unicode characters that cause problems.  Dropping the column for now.
  d[,"Description"] <- NULL
  
  return(d)
}


# NOTE: warnings about NAs introduced by coercion are ok- it's because the data
# is interpreting 'NULL' as a numeric, so it gets set to NaN but this is 
# resolved with the trimdata code used for data2

# data2 is used by Aggregate Data code - Other data sets available as needed
data2 <- trimdata(naref(data))                ## Core data set to use
data3 <- data2[data2$Product_group=="ECT",]   ## Data set that includes only ECT items
data4 <- data3[data3$Sec_unit_qty_c>0,]       ## Data set with only quantity >0
data5 <- data4[data4$USD_unit_margin>0,]      ## Data set with only margins >0 




########################################################################
#### Data Preparation #####
########################################################################

#### Aggregated data across all 25 months ####
data_agg_type  = summaryBy(Sec_unit_qty_c
                 + USD_extended_price + USD_total_extended_cost
                 ~ Product_group + Product_group_desc   # product group
                 + Process_type + Subprocess_type       # process types
                 + Product_type + Product_type_desc     # product type 
                 + Size_ID + Material_type,             # size and material
                 FUN=c(sum),
                 keep.names = TRUE,
                 data=data2)

# Remove offsetting entires by removing entires with quantity 0
data_agg_type = data_agg_type[data_agg_type$Sec_unit_qty_c !=0 , ]

# Create comprehensive identifying variable to be used when running regressions
data_agg_type$process_type_size_mat = 
                 paste(data_agg_type$Product_group,
                 data_agg_type$Subprocess_type,
                 data_agg_type$Product_type, 
                 data_agg_type$Size_ID,
                 data_agg_type$Material_type, sep = "_")

# Calculate unit price and cost
data_agg_type$USD_unit_price =  data_agg_type$USD_extended_price / data_agg_type$Sec_unit_qty_c
data_agg_type$USD_unit_cost  =  data_agg_type$USD_total_extended_cost / data_agg_type$Sec_unit_qty_c

# Calculate unit profit and margin
data_agg_type$USD_unit_profit = (data_agg_type$USD_extended_price - data_agg_type$USD_total_extended_cost) / data_agg_type$Sec_unit_qty_c
data_agg_type$USD_unit_margin_percent = data_agg_type$USD_unit_profit / data_agg_type$USD_unit_price

# Create ECT dataset
data_agg_type_ECT = data_agg_type[data_agg_type$Product_group =="ECT" ,]

########################################################################
#### Aggregated data on a yearly basis ####

# Aggregate data across comparable 12-month periods
data_annual_type  <- summaryBy(Sec_unit_qty_c
                    + USD_extended_price + USD_total_extended_cost
                    ~ YoY                                  # group by YoY value
                    + Product_group + Product_group_desc   # product group
                    + Process_type + Subprocess_type       # process type
                    + Product_type + Product_type_desc     # product type 
                    + Size_ID + Material_type,             # size and material
                    FUN=c(sum),
                    keep.names = TRUE,
                    data=data2)

# Remove offsetting entires by removing entires with quantity 0
data_annual_type <- data_annual_type[data_annual_type$Sec_unit_qty_c !=0 , ]
# Remove April 2013 from data set as it falls outside 24-month comparison
data_annual_type <- data_annual_type[data_annual_type$YoY !="X" , ]

# Create comprehensive identifying variable to be used when running regressions
data_annual_type$process_type_size_mat = 
            paste(data_annual_type$Subprocess_type,
                  data_annual_type$Product_type, 
                  data_annual_type$Size_ID,
                  data_annual_type$Material_type, sep = "_")

# Calculate unit price and cost
data_annual_type$USD_unit_price =  data_annual_type$USD_extended_price / data_annual_type$Sec_unit_qty_c
data_annual_type$USD_unit_cost  =  data_annual_type$USD_total_extended_cost / data_annual_type$Sec_unit_qty_c

# Calculate unit profit and margin
data_annual_type$USD_unit_profit = (data_annual_type$USD_extended_price - data_annual_type$USD_total_extended_cost) / data_annual_type$Sec_unit_qty_c
data_annual_type$USD_unit_margin_percent = data_annual_type$USD_unit_profit / data_annual_type$USD_unit_price

# create ECT dataset
data_annual_type_ECT = data_annual_type[data_annual_type$Product_group =="ECT" ,]

# Remove observations with negative unit prices and quantities
type_annual_basic <- data_annual_type_ECT[data_annual_type_ECT$Sec_unit_qty_c>0,]
type_annual_basic <- data_annual_type_ECT[data_annual_type_ECT$USD_unit_price>0,]

########################################################################
#### Aggregated data on a monthly basis ####

# Average monthly economic indicators
avg_monthly_economic_vars  = summaryBy(Polystyrene_foam_pricing 
          + S.P_Brent_Crude + S.P_500  + S.P_Industrials + Consumer_Electronics
          ~ Inv_year + Inv_month ,
          FUN=c(mean),
          keep.names = TRUE,
          data=data2)

# Aggregate monthly data by subprocess*type*size*material
data_monthly_type  = summaryBy(Sec_unit_qty_c
                     + USD_extended_price + USD_total_extended_cost
                     ~ Inv_year + Inv_month
                     + Product_group + Product_group_desc   # product group
                     + Process_type + Subprocess_type       # process type
                     + Product_type + Product_type_desc     # product type 
                     + Size_ID + Material_type,             # size and material
                     FUN=c(sum),
                     keep.names = TRUE,
                     data=data2)

# Remove offsetting entires by removing entires with quantity 0
data_monthly_type = data_monthly_type[data_monthly_type$Sec_unit_qty_c !=0 , ]

# Create comprehensive identifying variable to be used when running regressions
data_monthly_type$process_type_size_mat = 
                     paste(data_monthly_type$Subprocess_type,
                     data_monthly_type$Product_type, 
                     data_monthly_type$Size_ID,
                     data_monthly_type$Material_type, sep = "_")

# Calculate unit price and cost
data_monthly_type$USD_unit_price =  data_monthly_type$USD_extended_price / data_monthly_type$Sec_unit_qty_c
data_monthly_type$USD_unit_cost  =  data_monthly_type$USD_total_extended_cost / data_monthly_type$Sec_unit_qty_c

# Calculate unit profit and margin
data_monthly_type$USD_unit_profit = (data_monthly_type$USD_extended_price - data_monthly_type$USD_total_extended_cost) / data_monthly_type$Sec_unit_qty_c
data_monthly_type$USD_unit_margin_percent = data_monthly_type$USD_unit_profit / data_monthly_type$USD_unit_price

# create ECT dataset
data_monthly_type_ECT = data_monthly_type[data_monthly_type$Product_group =="ECT" ,]

# combine data for regressions
type_reg_basic = merge(data_monthly_type_ECT, avg_monthly_economic_vars,
                       by =c("Inv_year", "Inv_month"))

# Remove observations with negative unit prices and quantities
type_reg_basic <- type_reg_basic[type_reg_basic$Sec_unit_qty_c>0,]
type_reg_basic <- type_reg_basic[type_reg_basic$USD_unit_price>0,]

# Log key variables
type_reg_basic$Sec_unit_qty_c <- log(type_reg_basic$Sec_unit_qty_c)
type_reg_basic$USD_unit_price <- log(type_reg_basic$USD_unit_price)
type_reg_basic$S.P_500 <- log(type_reg_basic$S.P_500)
type_reg_basic$S.P_Industrials <- log(type_reg_basic$S.P_Industrials)
type_reg_basic$Consumer_Electronics <- log(type_reg_basic$Consumer_Electronics)

#########################################################################
#### Aggregated data on a weekly basis ####

days = as.Date(data2$Inv_date, "%m/%d/%y")
x <- as.POSIXlt(days)
data2$week = strftime(x,format="%W") 

# average weekly economic indicators
avg_weekly_economic_vars  = summaryBy(Polystyrene_foam_pricing + S.P_Brent_Crude 
                            + S.P_500  + S.P_Industrials + Consumer_Electronics
                            ~ Inv_year + week,
                            FUN=c(mean),
                            keep.names = TRUE,
                            data=data2)

# aggregate weekly data by subprocess*type*size*material
data_weekly_type  = summaryBy(Sec_unit_qty_c
                   + USD_extended_price + USD_total_extended_cost
                   ~ Inv_year + week
                   + Product_group + Product_group_desc   # product group
                   + Process_type + Subprocess_type       # process types        
                   + Product_type + Product_type_desc     # product type 
                   + Size_ID + Material_type,             # size and material
                   FUN=c(sum),
                   keep.names = TRUE,
                   data=data2)

# remove offsetting entries by removing entries with quantity 0
data_weekly_type = data_weekly_type[data_weekly_type$Sec_unit_qty_c !=0 , ]

# create comprehensive dentifying variable to be used when running regressions
data_weekly_type$process_type_size_mat = 
                   paste(data_weekly_type$Subprocess_type,       
                   data_weekly_type$Product_type,
                   data_weekly_type$Size_ID,
                   data_weekly_type$Material_type,
                   sep = "_")

# calculate unit price and cost
data_weekly_type$USD_unit_price =  data_weekly_type$USD_extended_price       /   data_weekly_type$Sec_unit_qty_c
data_weekly_type$USD_unit_cost  =  data_weekly_type$USD_total_extended_cost  /   data_weekly_type$Sec_unit_qty_c

# calculate unit profit and margin
data_weekly_type$USD_unit_profit = (data_weekly_type$USD_extended_price - data_weekly_type$USD_total_extended_cost) / data_weekly_type$Sec_unit_qty_c
data_weekly_type$USD_unit_margin_percent = data_weekly_type$USD_unit_profit / data_weekly_type$USD_unit_price

# create ECT dataset
data_weekly_type_ETC = data_weekly_type[data_weekly_type$Product_group =="ECT" ,]

# combine data for regressions
type_weekly_reg_basic = merge(data_weekly_type_ETC, avg_weekly_economic_vars,
         by =c("Inv_year", "week"))

# Remove observations with negative unit prices and quantities
type_weekly_reg_basic <- type_weekly_reg_basic[type_weekly_reg_basic$Sec_unit_qty_c>0,]
type_weekly_reg_basic <- type_weekly_reg_basic[type_weekly_reg_basic$USD_unit_price>0,]

# log key variables
type_weekly_reg_basic$Sec_unit_qty_c <- log(type_weekly_reg_basic$Sec_unit_qty_c)
type_weekly_reg_basic$USD_unit_price <- log(type_weekly_reg_basic$USD_unit_price)
type_weekly_reg_basic$S.P_500 <- log(type_weekly_reg_basic$S.P_500)
type_weekly_reg_basic$S.P_Industrials <- log(type_weekly_reg_basic$S.P_Industrials)
type_weekly_reg_basic$Consumer_Electronics <- log(type_weekly_reg_basic$Consumer_Electronics)




#########################################################################
#### Data Visualization ####
#########################################################################

## Identification of negative margin and volume products
data_agg_type_ECT$process_type_size_mat[data_agg_type_ECT$Sec_unit_qty_c<0] ## Negative quantity products
data_agg_type_ECT$process_type_size_mat[data_agg_type_ECT$USD_unit_margin_percent<0] ## Negative margin products

# Note that all visualization is done for gross volume & pricing

#### Volume x Margin ####
yname="Sec_unit_qty_c"
sl <- data5[,-grep(yname, colnames(data5))]
y <- data5[,grep(yname, colnames(data5))]
yframe <- data.frame(y)
colnames(yframe) <- yname

#Inspect the data
colnames(sl)
summary(y)

#plot 
par(mfrow=c(1,1))
mybreaks = seq(from=0, to=1.0, length=100)
hist_vals=c()
for(i in 2:length(mybreaks))
{
  hist_vals = c(hist_vals, sum(y[sl[,"USD_unit_margin"] 
  < mybreaks[i] & sl[,"USD_unit_margin"] >= mybreaks[i-1]])) 
}
png(filename=paste(sep="", "BD_Final_Visualization_", "VolxMar", ".png"), width = 960, height = 480, units = "px")
bp <- barplot(hist_vals,xlab="Unit Margin %",ylab="Volume Sold", 
        main="Volume Sold at Given Margin %")
axis(1, at=bp, labels=percent((1:99)/100))
dev.off()
order(hist_vals)
for(i in c(order(hist_vals)[91:100]))
{
  print(hist_vals[i]/sum(hist_vals))
}


#### Volume x Unit Cost ####
yname="Sec_unit_qty_c"
sl <- data5[,-grep(yname, colnames(data5))]
y <- data5[,grep(yname, colnames(data5))]
yframe <- data.frame(y)
colnames(yframe) <- yname

#Inspect the data
colnames(sl)
summary(y)

#plot 
par(mfrow=c(1,1))
mybreaks = seq(from=0, to=0.35, by=0.01) ## excludes sporadic values >$0.35
hist_vals=c()
for(i in 2:length(mybreaks))
{
  hist_vals = c(hist_vals, sum(y[sl[,"USD_std_unit_cost_correct"] 
   < mybreaks[i] & sl[,"USD_std_unit_cost_correct"] >= mybreaks[i-1]])) 
}
png(filename=paste(sep="", "BD_Final_Visualization_", "VolxCost", ".png"), width = 960, height = 480, units = "px")
bp <- barplot(hist_vals,xlab="Unit Cost (USD$)",ylab="Volume Sold", 
        main="Volume Sold at Given Unit Cost")
axis(1, at=bp, labels=(1:35/100), cex.axis=0.75)
dev.off()
order(hist_vals)[31:35]
for(i in c(order(hist_vals)[31:35]))
{
  print(hist_vals[i]/sum(hist_vals))
}


#### Volume x Material ####
yname="Sec_unit_qty_c"
sl <- data5[,-grep(yname, colnames(data5))]
mat<-levels(sl$Material_type)
mat
sl$Material_type <- as.numeric(sl$Material_type)
y <- data5[,grep(yname, colnames(data5))]
yframe <- data.frame(y)
colnames(yframe) <- yname

#Inspect the data
colnames(sl)
summary(y)

#plot 
par(mfrow=c(1,1))
mybreaks = seq(from=0, to=34, length=34) 
hist_vals=c()
for(i in 2:length(mybreaks))
{
  hist_vals = c(hist_vals, sum(y[sl[,"Material_type"] 
    < mybreaks[i] & sl[,"Material_type"] >= mybreaks[i-1]])) 
}
png(filename=paste(sep="", "BD_Final_Visualization_", "VolxMat", ".png"), width = 960, height = 480, units = "px")
bp <- barplot(hist_vals,xlab="Material Type",ylab="Volume Sold", 
        main="Volume sold by Material")
axis(1, at=bp, labels=c(mat), cex.axis=0.4)
dev.off()
hist_vals
hist_vals[10:12]
(hist_vals[10]+hist_vals[12])/sum(hist_vals)


#### Volume x Size ####
yname="Sec_unit_qty_c"
sl <- data5[,-grep(yname, colnames(data5))]
y <- data5[,grep(yname, colnames(data5))]
yframe <- data.frame(y)
colnames(yframe) <- yname

#Inspect the data
colnames(sl)
summary(y)

#plot 
par(mfrow=c(1,1))
mybreaks = seq(from=0, to=105, length=105)
hist_vals=c()
for(i in 2:length(mybreaks))
{
  hist_vals = c(hist_vals, sum(y[sl[,"Size_ID"] 
    < mybreaks[i] & sl[,"Size_ID"] >= mybreaks[i-1]])) 
}
png(filename=paste(sep="", "BD_Final_Visualization_", "VolxSize", ".png"), width = 960, height = 480, units = "px")
bp <- barplot(hist_vals,xlab="Size (mm)",ylab="Volume Sold", 
        main="Volume sold by Size")
axis(1, at=bp, labels=1:104, cex.axis=0.75)
dev.off()
hist_vals
hist_vals[12:24]
(hist_vals[12]+hist_vals[16]+hist_vals[24])/sum(hist_vals)


#### Volume x Work Center ####
yname="Sec_unit_qty_c"
sl <- data5[,-grep(yname, colnames(data5))]
wrkctr <- levels(sl$WRKCTRID)
wrkctr
sl$WRKCTRID <- as.numeric(sl$WRKCTRID)
y <- data5[,grep(yname, colnames(data5))]
yframe <- data.frame(y)
colnames(yframe) <- yname

#Inspect the data
colnames(sl)
summary(y)

#plot 
par(mfrow=c(1,1))
mybreaks = seq(from=1, to=3501, length=3502)
hist_vals=c()
for(i in 2:length(mybreaks))
{
  hist_vals = c(hist_vals, sum(y[sl[,"WRKCTRID"] 
    < mybreaks[i] & sl[,"WRKCTRID"] >= mybreaks[i-1]])) 
}
png(filename=paste(sep="", "BD_Final_Visualization_", "VolxWrkCtr", ".png"), width = 960, height = 480, units = "px")
bp <- barplot(hist_vals,xlab="Work Center",ylab="Volume Sold", 
              main="Volume sold by Work Center")
dev.off()
# match WRKCTRIDs with volume
sort(hist_vals)
wc <- cbind(wrkctr,hist_vals)
wc
wc[which(hist_vals>10000000)] ## Most significant work centers
hist_vals[which(hist_vals>10000000)] ## Volume sold per work center
sum(hist_vals[which(hist_vals>10000000)])/sum(hist_vals) # % of volume from top 10 work centers


#### Volume x Price ####
yname="Sec_unit_qty_c"
sl <- data5[,-grep(yname, colnames(data5))]
y <- data5[,grep(yname, colnames(data5))]
yframe <- data.frame(y)
colnames(yframe) <- yname

#Inspect the data
colnames(sl)
summary(y)

#plot 
par(mfrow=c(1,1))
mybreaks = seq(from=0.0, to=0.5, by=0.01) ## sporadic values after $0.50 excluded
hist_vals=c()
for(i in 2:length(mybreaks))
{
  hist_vals = c(hist_vals, sum(y[sl[,"USD_unit_price_correct"] 
    < mybreaks[i] & sl[,"USD_unit_price_correct"] >= mybreaks[i-1]])) 
}
png(filename=paste(sep="", "BD_Final_Visualization_", "VolxPrice", ".png"), width = 960, height = 480, units = "px")
bp <- barplot(hist_vals,xlab="Unit Price (USD$)",ylab="Volume Sold", 
              main="Volume sold by Unit Price")
axis(1, at=bp, labels=1:50/100, cex.axis=0.75)
dev.off()
order(hist_vals)[46:50]
for(i in c(order(hist_vals)[46:50]))
{
  print(hist_vals[i]/sum(hist_vals))
}


#### Margin x Unit Cost ####
yname="USD_unit_margin"
sl <- data5[,-grep(yname, colnames(data5))]
y <- data5[,grep(yname, colnames(data5))]
yframe <- data.frame(y)
colnames(yframe) <- yname

#Inspect the data
colnames(sl)
summary(y)

#plot 
par(mfrow=c(1,1))
mybreaks = seq(from=0, to=1.0, by=0.01) ## Limited unit costs >$1.00 excluded
hist_vals=c()
for(i in 2:length(mybreaks))
{
  hist_vals = c(hist_vals, mean(y[sl[,"USD_std_unit_cost_correct"] 
    < mybreaks[i] & sl[,"USD_std_unit_cost_correct"] >= mybreaks[i-1]])) 
}
png(filename=paste(sep="", "BD_Final_Visualization_", "MarxCost", ".png"), width = 960, height = 480, units = "px")
bp <- barplot(hist_vals,xlab="Unit Cost (USD$)",ylab="Average Unit Margin", 
        main="Average Margin by Unit Cost")
axis(1, at=bp, labels=(1:100/100), cex.axis=0.75)
dev.off()
order(hist_vals)[89:93]
hist_vals[3]
hist_vals[11]
hist_vals[100]
hist_vals[4]
hist_vals[10]


#### Margin x Material ####
yname="USD_unit_margin"
sl <- data5[,-grep(yname, colnames(data5))]
mat<-levels(sl$Material_type)
mat
sl$Material_type <- as.numeric(sl$Material_type)
y <- data5[,grep(yname, colnames(data5))]
yframe <- data.frame(y)
colnames(yframe) <- yname

#Inspect the data
colnames(sl)
summary(y)

#plot 
par(mfrow=c(1,1))
mybreaks = seq(from=0, to=34, length=34) 
hist_vals=c()
for(i in 2:length(mybreaks))
{
  hist_vals = c(hist_vals, mean(y[sl[,"Material_type"] 
   < mybreaks[i] & sl[,"Material_type"] >= mybreaks[i-1]])) 
}
png(filename=paste(sep="", "BD_Final_Visualization_", "MarxMat", ".png"), width = 960, height = 480, units = "px")
bp <- barplot(hist_vals,xlab="Material",ylab="Average Unit Margin", 
              main="Average Margin by Material")
axis(1, at=bp, labels=c(mat), cex.axis=0.4)
dev.off()
mats <- cbind(mat,hist_vals)
mats


#### Margin x Size ####
yname="USD_unit_margin"
sl <- data5[,-grep(yname, colnames(data5))]
y <- data5[,grep(yname, colnames(data5))]
yframe <- data.frame(y)
colnames(yframe) <- yname

#Inspect the data
colnames(sl)
summary(y)

#plot 
par(mfrow=c(1,1))
mybreaks = seq(from=1, to=105)
hist_vals=c()
for(i in 2:length(mybreaks))
{
  hist_vals = c(hist_vals, mean(y[sl[,"Size_ID"] 
  < mybreaks[i] & sl[,"Size_ID"] >= mybreaks[i-1]])) 
}
png(filename=paste(sep="", "BD_Final_Visualization_", "MarxSize", ".png"), width = 960, height = 480, units = "px")
bp <- barplot(hist_vals,xlab="Size",ylab="Average Unit Margin", 
      main="Average Margin by Size")
axis(1, at=bp, labels=1:104, cex.axis=0.75)
dev.off()
hist_vals


#### Margin x Work Center ####
yname="USD_unit_margin"
sl <- data5[,-grep(yname, colnames(data5))]
wrkctr <- levels(sl$WRKCTRID)
wrkctr
sl$WRKCTRID <- as.numeric(sl$WRKCTRID)
y <- data5[,grep(yname, colnames(data5))]
yframe <- data.frame(y)
colnames(yframe) <- yname

#Inspect the data
colnames(sl)
summary(y)

#plot 
par(mfrow=c(1,1))
mybreaks = seq(from=1, to=3501, length=3502)
hist_vals=c()
for(i in 2:length(mybreaks))
{
  hist_vals = c(hist_vals, mean(y[sl[,"WRKCTRID"] 
    < mybreaks[i] & sl[,"WRKCTRID"] >= mybreaks[i-1]])) 
}
png(filename=paste(sep="", "BD_Final_Visualization_", "MarxWrkCtr", ".png"), width = 960, height = 480, units = "px")
bp <- barplot(hist_vals,xlab="Work Center",ylab="Average Unit Margin", 
              main="Average Margin by Work Center")
dev.off()
# match WRKCTRIDs with margin
sort(hist_vals)
wc <- cbind(wrkctr,hist_vals)
wc[which(hist_vals>0.85)] ## Most profitable work centers
wc[which(hist_vals<0.05)] ## Least profitable work centers


#### Cost x Material ####
yname="USD_std_unit_cost_correct"
sl <- data5[,-grep(yname, colnames(data5))]
mat<-levels(sl$Material_type)
mat
sl$Material_type <- as.numeric(sl$Material_type)
y <- data5[,grep(yname, colnames(data5))]
yframe <- data.frame(y)
colnames(yframe) <- yname

#Inspect the data
colnames(sl)
summary(y)

#plot 
par(mfrow=c(1,1))
mybreaks = seq(from=0, to=34, length=34) 
hist_vals=c()
for(i in 2:length(mybreaks))
{
  hist_vals = c(hist_vals, mean(y[sl[,"Material_type"] 
   < mybreaks[i] & sl[,"Material_type"] >= mybreaks[i-1]])) 
}
png(filename=paste(sep="", "BD_Final_Visualization_", "CostxMat", ".png"), width = 960, height = 480, units = "px")
bp <- barplot(hist_vals,xlab="Material",ylab="Average Unit Cost", 
      main="Average Unit Cost by Material")
axis(1, at=bp, labels=c(mat), cex.axis=0.4)
dev.off()
cbind(mat,hist_vals)


#### Cost x Size ####
yname="USD_std_unit_cost_correct"
sl <- data5[,-grep(yname, colnames(data5))]
y <- data5[,grep(yname, colnames(data5))]
yframe <- data.frame(y)
colnames(yframe) <- yname

#Inspect the data
colnames(sl)
summary(y)

#plot 
par(mfrow=c(1,1))
mybreaks = seq(from=1, to=105)
hist_vals=c()
for(i in 2:length(mybreaks))
{
  hist_vals = c(hist_vals, mean(y[sl[,"Size_ID"] 
  < mybreaks[i] & sl[,"Size_ID"] >= mybreaks[i-1]])) 
}
png(filename=paste(sep="", "BD_Final_Visualization_", "CostxSize", ".png"), width = 960, height = 480, units = "px")
bp <- barplot(hist_vals,xlab="Size",ylab="Average Unit Cost", 
        main="Average Unit Cost by Size")
axis(1, at=bp, labels=1:104, cex.axis=0.75)
dev.off()
hist_vals


#### Cost x Work Center ####
yname="USD_std_unit_cost_correct"
sl <- data5[,-grep(yname, colnames(data5))]
wrkctr <- levels(sl$WRKCTRID)
wrkctr
sl$WRKCTRID <- as.numeric(sl$WRKCTRID)
y <- data5[,grep(yname, colnames(data5))]
yframe <- data.frame(y)
colnames(yframe) <- yname

#Inspect the data
colnames(sl)
summary(y)

#plot 
par(mfrow=c(1,1))
mybreaks = seq(from=1, to=3501, length=3502)
hist_vals=c()
for(i in 2:length(mybreaks))
{
  hist_vals = c(hist_vals, mean(y[sl[,"WRKCTRID"] 
    < mybreaks[i] & sl[,"WRKCTRID"] >= mybreaks[i-1]])) 
}
png(filename=paste(sep="", "BD_Final_Visualization_", "CostxWrkCtr", ".png"), width = 960, height = 480, units = "px")
barplot(hist_vals,xlab="Work Center",ylab="Unit Cost", 
        main="Average Unit Cost by Work Center")
dev.off()

# match WRKCTRIDs with unit costs
sort(hist_vals)
wc <- cbind(wrkctr,hist_vals)
wc
wc[which(hist_vals>0.60)] ## Most costly work centers
hist_vals[which(hist_vals>0.60)] ## High costs for work centers
wc[which(hist_vals<0.04)] ## Most cost effective work centers


########################################################################
#### 2x2 of Volume x Unit Margin % ####

# Analysis at 1st Quartile Cutoff
data_agg_type_ECT <- data_agg_type_ECT[data_agg_type_ECT$USD_unit_price!=0,]

png(filename=paste(sep="", "BD_Final_Visualization_", "2x2-1st Quartile", ".png"), width = 960, height = 480, units = "px")
plot(log(data_agg_type_ECT$Sec_unit_qty_c/25), 
     data_agg_type_ECT$USD_unit_margin_percent,  
     type="p", col="blue", pch=19,
     main="Log of Average Monthly Units Sold by Average Unit Margin %",
     ylab="Average Unit Margin %", xlab="Log of Average Monthly Units Sold")
abline(h = quantile(data_agg_type_ECT$USD_unit_margin_percent)[2],
       v = quantile(log(data_agg_type_ECT$Sec_unit_qty_c/25))[2],
       col="red")
legend("bottomleft", bty="n", legend="Low Margin and Volume Quadrant/n",
       text.col="red", cex=1)
dev.off()

# Identification of low margin low volume products at 1st Quartile cutoff
data_agg_type_ECT$process_type_size_mat[data_agg_type_ECT$USD_unit_margin_percent<
       quantile(data_agg_type_ECT$USD_unit_margin_percent)[2] & 
       log(data_agg_type_ECT$Sec_unit_qty_c/25)<
       quantile(log(data_agg_type_ECT$Sec_unit_qty_c/25))[2]] 

# 2x2 Analysis at Median Quartile Cutoff
png(filename=paste(sep="", "BD_Final_Visualization_", "2x2-Median", ".png"), width = 960, height = 480, units = "px")
plot(log(data_agg_type_ECT$Sec_unit_qty_c/25), 
       data_agg_type_ECT$USD_unit_margin_percent,  
       type="p", col="blue", pch=19,
       main="Log of Average Monthly Units Sold by Average Unit Margin %",
       ylab="Average Unit Margin %", xlab="Log of Average Monthly Units Sold")
abline(h = quantile(data_agg_type_ECT$USD_unit_margin_percent)[3],
       v = quantile(log(data_agg_type_ECT$Sec_unit_qty_c/25))[3],
       col="red")
legend("bottomleft", bty="n", legend="Low Margin and Volume Quadrant\n",
       text.col="red", cex=1)
dev.off()

# Identification of low margin low volume products at 1st Quartile cutoff
data_agg_type_ECT$process_type_size_mat[data_agg_type_ECT$USD_unit_margin_percent<
       quantile(data_agg_type_ECT$USD_unit_margin_percent)[3] & 
       log(data_agg_type_ECT$Sec_unit_qty_c/25)<
       quantile(log(data_agg_type_ECT$Sec_unit_qty_c/25))[3]] 


########################################################################
#### Pricing distributions by type - Volatile Top 20 ####
price_data <- data4
price_data$process_type_size_mat = 
        paste(price_data$Product_group,
              price_data$Subprocess_type,
              price_data$Product_type, 
              price_data$Size_ID,
              price_data$Material_type, sep = "_")

key_prod <- c("ECT_STV_STV_12_C", "ECT_STV_STV_16_C", "ECT_STV_STV_32_X",
              "ECT_STV_STV_24_X", "ECT_STV_STV_24_C", "ECT_STV_STV_44_X",
              "ECT_ADP_ADP_44_X", "ECT_STV_STV_44_C", "ECT_STV_STV_8_C", 
              "ECT_STV_STV_32_C")
data_price <- price_data[price_data$process_type_size_mat==c(key_prod),]

png(filename=paste(sep="", "BD_Final_Visualization_", "Volatile_Top_20", ".png"), width = 960, height = 480, units = "px")
boxplot(USD_unit_price_correct~process_type_size_mat,data=data_price, 
        main="Pricing Distribution of Volatile Top 20 Products", 
        ylab="Unit Price (USD$)", ylim=c(0,1.5),
        cex.axis=0.5, las=2)
dev.off()

#### Pricing distributions by type - Stable Top 20 ####
price_data2 <- data4
price_data2$process_type_size_mat = 
  paste(price_data2$Product_group,
        price_data2$Subprocess_type,
        price_data2$Product_type, 
        price_data2$Size_ID,
        price_data2$Material_type, sep = "_")

key_prod2 <- c("ECT_UPI_UPI_24_Q", "ECT_STV_STV_32_D", "ECT_STV_STV_24_D",
               "ECT_STV_STV_12_D", "ECT_STV_STV_44_D", "ECT_STV_STV_16_BB",
               "ECT_STV_STV_16_D", "ECT_STV_STV_16_BC", "ECT_STV_STV_12_BB",
               "ECT_STV_STV_12_BC")
data_price2 <- price_data2[price_data2$process_type_size_mat==c(key_prod2),]

png(filename=paste(sep="", "BD_Final_Visualization_", "Stable_Top_20", ".png"), width = 960, height = 480, units = "px")
boxplot(USD_unit_price_correct~process_type_size_mat,data=data_price2, 
        main="Pricing Distribution of Stable Top 20 Products", 
        ylab="Unit Price (USD$)", ylim=c(0,1.5),
        cex.axis=0.5, las=2)
dev.off()




#######################################################################
#### Linear Regressions ####
#######################################################################

#### Basic Linear Regression for Top 20 Products ####

#### STV_STV_12_C ####
type_weekly_reg_basic1 <- 
        naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_12_C",]
# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic1$week <- as.numeric(levels(type_weekly_reg_basic1$week))[type_weekly_reg_basic1$week]
type_weekly_reg_basic1$Inv_year <- as.numeric(levels(type_weekly_reg_basic1$Inv_year))[type_weekly_reg_basic1$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic1$week[1]
type_weekly_reg_basic1$week <- type_weekly_reg_basic1$week + (type_weekly_reg_basic1$Inv_year - 2013) * 52 - firstweek

# simple regression on a few variables.  
reg_glm <- glm(Sec_unit_qty_c ~ USD_unit_price + week
              + S.P_500 + S.P_Industrials + Consumer_Electronics,           
              data=type_weekly_reg_basic1)
# coefficients
summary(reg_glm)
# R2
1-summary(reg_glm)$deviance/summary(reg_glm)$null.deviance
# p-values
pvals <- summary(reg_glm)$coef[-1,4] #-1 to drop intercept
png(filename=paste(sep="", "BD_Final_GLM_Pvals_", "STV_STV_12_C", ".png"), 
    width = 960, height = 480, units = "px")
hist(pvals,xlab="P-value",main="Histogram of P-values",col="light blue",
     breaks=10)
dev.off()
png(filename=paste(sep="", "BD_Final_GLM_FDR_", "STV_STV_12_C", ".png"), 
    width = 960, height = 480, units = "px")
q = 0.05
alpha <- fdr_cut(pvals,q,plotit=TRUE)
dev.off()
signif <- which(pvals<=alpha) ##which are significant
length(signif)

# out-of-sample prediction
train_data = type_weekly_reg_basic1[type_weekly_reg_basic1$Inv_year =="2014" ,]
# train the model WITHOUT these observation
# week variable excluded because there are issues (i.e., perfectly predictive)
trainreg <- glm(Sec_unit_qty_c ~ USD_unit_price
                + S.P_500 + S.P_Industrials + Consumer_Electronics,           
                data=train_data)
summary(trainreg)

# predicted demand on actual 2015 data
predict_data = type_weekly_reg_basic1[type_weekly_reg_basic1$Inv_year =="2015" ,]
predict <- predict(trainreg, newdata=predict_data)
predict
# plot the OOS fit
png(filename=paste(sep="", "BD_Final_GLM_Fitted_", "STV_STV_12_C", ".png"), 
    width = 960, height = 480, units = "px")
plot(predict ~ predict_data$Sec_unit_qty_c, 
     xlab="Actual volume", 
     ylab="Predicted volume",
     main="Fitted vs. Actual log quantity for 2015 - STV_STV_12_C")
dev.off()


#### STV_STV_12_D ####
type_weekly_reg_basic2 <- 
  naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_12_D",]
# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic2$week <- as.numeric(levels(type_weekly_reg_basic2$week))[type_weekly_reg_basic2$week]
type_weekly_reg_basic2$Inv_year <- as.numeric(levels(type_weekly_reg_basic2$Inv_year))[type_weekly_reg_basic2$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic2$week[1]
type_weekly_reg_basic2$week <- type_weekly_reg_basic2$week + (type_weekly_reg_basic2$Inv_year - 2013) * 52 - firstweek

# simple regression on a few variables.  
reg_glm <- glm(Sec_unit_qty_c ~ USD_unit_price + week
               + S.P_500 + S.P_Industrials + Consumer_Electronics,           
               data=type_weekly_reg_basic2)
# coefficients
summary(reg_glm)
# R2
1-summary(reg_glm)$deviance/summary(reg_glm)$null.deviance
# p-values
pvals <- summary(reg_glm)$coef[-1,4] #-1 to drop intercept
png(filename=paste(sep="", "BD_Final_GLM_Pvals_", "STV_STV_12_D", ".png"), 
    width = 960, height = 480, units = "px")
hist(pvals,xlab="P-value",main="Histogram of P-values",col="light blue",
     breaks=10)
dev.off()
png(filename=paste(sep="", "BD_Final_GLM_FDR_", "STV_STV_12_D", ".png"), 
    width = 960, height = 480, units = "px")
q = 0.05
alpha <- fdr_cut(pvals,q,plotit=TRUE)
dev.off()
signif <- which(pvals<=alpha)##which are significant
length(signif)

# out-of-sample prediction
train_data = type_weekly_reg_basic2[type_weekly_reg_basic2$Inv_year =="2014" ,]
# train the model WITHOUT these observation
# week variable excluded because there are issues (i.e., perfectly predictive)
trainreg <- glm(Sec_unit_qty_c ~ USD_unit_price
                + S.P_500 + S.P_Industrials + Consumer_Electronics,           
                data=train_data)
summary(trainreg)

# predicted demand on actual 2015 data
predict_data = type_weekly_reg_basic2[type_weekly_reg_basic2$Inv_year =="2015" ,]
predict <- predict(trainreg, newdata=predict_data)
predict
# plot the OOS fit
png(filename=paste(sep="", "BD_Final_GLM_Fitted_", "STV_STV_12_D", ".png"), 
    width = 960, height = 480, units = "px")
plot(predict ~ predict_data$Sec_unit_qty_c, 
     xlab="Actual volume", 
     ylab="Predicted volume",
     main="Fitted vs. Actual log quantity for 2015 - STV_STV_12_D")
dev.off()


#### STV_STV_24_D ####
type_weekly_reg_basic3 <- 
  naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_24_D",]
# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic3$week <- as.numeric(levels(type_weekly_reg_basic3$week))[type_weekly_reg_basic3$week]
type_weekly_reg_basic3$Inv_year <- as.numeric(levels(type_weekly_reg_basic3$Inv_year))[type_weekly_reg_basic3$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic3$week[1]
type_weekly_reg_basic3$week <- type_weekly_reg_basic3$week + (type_weekly_reg_basic3$Inv_year - 2013) * 52 - firstweek

# simple regression on a few variables.  
reg_glm <- glm(Sec_unit_qty_c ~ USD_unit_price + week
               + S.P_500 + S.P_Industrials + Consumer_Electronics,           
               data=type_weekly_reg_basic3)
# coefficients
summary(reg_glm)
# R2
1-summary(reg_glm)$deviance/summary(reg_glm)$null.deviance
# p-values
pvals <- summary(reg_glm)$coef[-1,4] #-1 to drop intercept
png(filename=paste(sep="", "BD_Final_GLM_Pvals_", "STV_STV_24_D", ".png"), 
    width = 960, height = 480, units = "px")
hist(pvals,xlab="P-value",main="Histogram of P-values",col="light blue",
     breaks=10)
dev.off()
png(filename=paste(sep="", "BD_Final_GLM_FDR_", "STV_STV_24_D", ".png"), 
    width = 960, height = 480, units = "px")
q = 0.05
alpha <- fdr_cut(pvals,q,plotit=TRUE)
dev.off()
signif <- which(pvals<=alpha)##which are significant
length(signif)

# out-of-sample prediction
train_data = type_weekly_reg_basic3[type_weekly_reg_basic3$Inv_year =="2014" ,]
# train the model WITHOUT these observation
# week variable excluded because there are issues (i.e., perfectly predictive)
trainreg <- glm(Sec_unit_qty_c ~ USD_unit_price
                + S.P_500 + S.P_Industrials + Consumer_Electronics,           
                data=train_data)
summary(trainreg)

# predicted demand on actual 2015 data
predict_data = type_weekly_reg_basic3[type_weekly_reg_basic3$Inv_year =="2015" ,]
predict <- predict(trainreg, newdata=predict_data)
predict
# plot the OOS fit
png(filename=paste(sep="", "BD_Final_GLM_Fitted_", "STV_STV_24_D", ".png"), 
    width = 960, height = 480, units = "px")
plot(predict ~ predict_data$Sec_unit_qty_c, 
     xlab="Actual volume", 
     ylab="Predicted volume",
     main="Fitted vs. Actual log quantity for 2015 - STV_STV_24_D")
dev.off()


#### STV_STV_16_C ####
type_weekly_reg_basic4 <- 
  naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_16_C",]
# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic4$week <- as.numeric(levels(type_weekly_reg_basic4$week))[type_weekly_reg_basic4$week]
type_weekly_reg_basic4$Inv_year <- as.numeric(levels(type_weekly_reg_basic4$Inv_year))[type_weekly_reg_basic4$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic4$week[1]
type_weekly_reg_basic4$week <- type_weekly_reg_basic4$week + (type_weekly_reg_basic4$Inv_year - 2013) * 52 - firstweek

# simple regression on a few variables.  
reg_glm <- glm(Sec_unit_qty_c ~ USD_unit_price + week
               + S.P_500 + S.P_Industrials + Consumer_Electronics,           
               data=type_weekly_reg_basic4)
# coefficients
summary(reg_glm)
# R2
1-summary(reg_glm)$deviance/summary(reg_glm)$null.deviance
# p-values
pvals <- summary(reg_glm)$coef[-1,4] #-1 to drop intercept
png(filename=paste(sep="", "BD_Final_GLM_Pvals_", "STV_STV_16_C", ".png"), 
    width = 960, height = 480, units = "px")
hist(pvals,xlab="P-value",main="Histogram of P-values",col="light blue",
     breaks=10)
dev.off()
png(filename=paste(sep="", "BD_Final_GLM_FDR_", "STV_STV_16_C", ".png"), 
    width = 960, height = 480, units = "px")
q = 0.05
alpha <- fdr_cut(pvals,q,plotit=TRUE)
dev.off()
signif <- which(pvals<=alpha)##which are significant
length(signif)

# out-of-sample prediction
train_data = type_weekly_reg_basic4[type_weekly_reg_basic4$Inv_year =="2014" ,]
# train the model WITHOUT these observation
# week variable excluded because there are issues (i.e., perfectly predictive)
trainreg <- glm(Sec_unit_qty_c ~ USD_unit_price
                + S.P_500 + S.P_Industrials + Consumer_Electronics,           
                data=train_data)
summary(trainreg)

# predicted demand on actual 2015 data
predict_data = type_weekly_reg_basic4[type_weekly_reg_basic4$Inv_year =="2015" ,]
predict <- predict(trainreg, newdata=predict_data)
predict
# plot the OOS fit
png(filename=paste(sep="", "BD_Final_GLM_Fitted_", "STV_STV_16_C", ".png"), 
    width = 960, height = 480, units = "px")
plot(predict ~ predict_data$Sec_unit_qty_c, 
     xlab="Actual volume", 
     ylab="Predicted volume",
     main="Fitted vs. Actual log quantity for 2015 - STV_STV_16_C")
dev.off()


#### STV_STV_16_D ####
type_weekly_reg_basic5 <- 
  naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_16_D",]
# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic5$week <- as.numeric(levels(type_weekly_reg_basic5$week))[type_weekly_reg_basic5$week]
type_weekly_reg_basic5$Inv_year <- as.numeric(levels(type_weekly_reg_basic5$Inv_year))[type_weekly_reg_basic5$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic5$week[1]
type_weekly_reg_basic5$week <- type_weekly_reg_basic5$week + (type_weekly_reg_basic5$Inv_year - 2013) * 52 - firstweek

# simple regression on a few variables.  
reg_glm <- glm(Sec_unit_qty_c ~ USD_unit_price + week
               + S.P_500 + S.P_Industrials + Consumer_Electronics,           
               data=type_weekly_reg_basic5)
# coefficients
summary(reg_glm)
# R2
1-summary(reg_glm)$deviance/summary(reg_glm)$null.deviance
# p-values
pvals <- summary(reg_glm)$coef[-1,4] #-1 to drop intercept
png(filename=paste(sep="", "BD_Final_GLM_Pvals_", "STV_STV_16_D", ".png"), 
    width = 960, height = 480, units = "px")
hist(pvals,xlab="P-value",main="Histogram of P-values",col="light blue",
     breaks=10)
dev.off()
png(filename=paste(sep="", "BD_Final_GLM_FDR_", "STV_STV_16_D", ".png"), 
    width = 960, height = 480, units = "px")
q = 0.05
alpha <- fdr_cut(pvals,q,plotit=TRUE)
dev.off()
signif <- which(pvals<=alpha)##which are significant
length(signif)

# out-of-sample prediction
train_data = type_weekly_reg_basic5[type_weekly_reg_basic5$Inv_year =="2014" ,]
# train the model WITHOUT these observation
# week variable excluded because there are issues (i.e., perfectly predictive)
trainreg <- glm(Sec_unit_qty_c ~ USD_unit_price
                + S.P_500 + S.P_Industrials + Consumer_Electronics,           
                data=train_data)
summary(trainreg)

# predicted demand on actual 2015 data
predict_data = type_weekly_reg_basic5[type_weekly_reg_basic5$Inv_year =="2015" ,]
predict <- predict(trainreg, newdata=predict_data)
predict
# plot the OOS fit
png(filename=paste(sep="", "BD_Final_GLM_Fitted_", "STV_STV_16_D", ".png"), 
    width = 960, height = 480, units = "px")
plot(predict ~ predict_data$Sec_unit_qty_c, 
     xlab="Actual volume", 
     ylab="Predicted volume",
     main="Fitted vs. Actual log quantity for 2015 - STV_STV_16_D")
dev.off()


#### STV_STV_24_C ####
type_weekly_reg_basic6 <- 
  naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_24_C",]
# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic6$week <- as.numeric(levels(type_weekly_reg_basic6$week))[type_weekly_reg_basic6$week]
type_weekly_reg_basic6$Inv_year <- as.numeric(levels(type_weekly_reg_basic6$Inv_year))[type_weekly_reg_basic6$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic6$week[1]
type_weekly_reg_basic6$week <- type_weekly_reg_basic6$week + (type_weekly_reg_basic6$Inv_year - 2013) * 52 - firstweek

# simple regression on a few variables.  
reg_glm <- glm(Sec_unit_qty_c ~ USD_unit_price + week
               + S.P_500 + S.P_Industrials + Consumer_Electronics,           
               data=type_weekly_reg_basic6)
# coefficients
summary(reg_glm)
# R2
1-summary(reg_glm)$deviance/summary(reg_glm)$null.deviance
# p-values
pvals <- summary(reg_glm)$coef[-1,4] #-1 to drop intercept
png(filename=paste(sep="", "BD_Final_GLM_Pvals_", "STV_STV_24_C", ".png"), 
    width = 960, height = 480, units = "px")
hist(pvals,xlab="P-value",main="Histogram of P-values",col="light blue",
     breaks=10)
dev.off()
png(filename=paste(sep="", "BD_Final_GLM_FDR_", "STV_STV_24_C", ".png"), 
    width = 960, height = 480, units = "px")
q = 0.05
alpha <- fdr_cut(pvals,q,plotit=TRUE)
dev.off()
signif <- which(pvals<=alpha)##which are significant
length(signif)

# out-of-sample prediction
train_data = type_weekly_reg_basic6[type_weekly_reg_basic6$Inv_year =="2014" ,]
# train the model WITHOUT these observation
# week variable excluded because there are issues (i.e., perfectly predictive)
trainreg <- glm(Sec_unit_qty_c ~ USD_unit_price
                + S.P_500 + S.P_Industrials + Consumer_Electronics,           
                data=train_data)
summary(trainreg)

# predicted demand on actual 2015 data
predict_data = type_weekly_reg_basic6[type_weekly_reg_basic6$Inv_year =="2015" ,]
predict <- predict(trainreg, newdata=predict_data)
predict
# plot the OOS fit
png(filename=paste(sep="", "BD_Final_GLM_Fitted_", "STV_STV_24_C", ".png"), 
    width = 960, height = 480, units = "px")
plot(predict ~ predict_data$Sec_unit_qty_c, 
     xlab="Actual volume", 
     ylab="Predicted volume",
     main="Fitted vs. Actual log quantity for 2015 - STV_STV_24_C")
dev.off()


#### STV_STV_44_C ####
type_weekly_reg_basic7 <- 
  naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_44_C",]
# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic7$week <- as.numeric(levels(type_weekly_reg_basic7$week))[type_weekly_reg_basic7$week]
type_weekly_reg_basic7$Inv_year <- as.numeric(levels(type_weekly_reg_basic7$Inv_year))[type_weekly_reg_basic7$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic7$week[1]
type_weekly_reg_basic7$week <- type_weekly_reg_basic7$week + (type_weekly_reg_basic7$Inv_year - 2013) * 52 - firstweek

# simple regression on a few variables.  
reg_glm <- glm(Sec_unit_qty_c ~ USD_unit_price + week
               + S.P_500 + S.P_Industrials + Consumer_Electronics,           
               data=type_weekly_reg_basic7)
# coefficients
summary(reg_glm)
# R2
1-summary(reg_glm)$deviance/summary(reg_glm)$null.deviance
# p-values
pvals <- summary(reg_glm)$coef[-1,4] #-1 to drop intercept
png(filename=paste(sep="", "BD_Final_GLM_Pvals_", "STV_STV_44_C", ".png"), 
    width = 960, height = 480, units = "px")
hist(pvals,xlab="P-value",main="Histogram of P-values",col="light blue",
     breaks=10)
dev.off()
png(filename=paste(sep="", "BD_Final_GLM_FDR_", "STV_STV_44_C", ".png"), 
    width = 960, height = 480, units = "px")
q = 0.05
alpha <- fdr_cut(pvals,q,plotit=TRUE)
dev.off()
signif <- which(pvals<=alpha)##which are significant
length(signif)

# out-of-sample prediction
train_data = type_weekly_reg_basic7[type_weekly_reg_basic7$Inv_year =="2014" ,]
# train the model WITHOUT these observation
# week variable excluded because there are issues (i.e., perfectly predictive)
trainreg <- glm(Sec_unit_qty_c ~ USD_unit_price
                + S.P_500 + S.P_Industrials + Consumer_Electronics,           
                data=train_data)
summary(trainreg)

# predicted demand on actual 2015 data
predict_data = type_weekly_reg_basic7[type_weekly_reg_basic7$Inv_year =="2015" ,]
predict <- predict(trainreg, newdata=predict_data)
predict
# plot the OOS fit
png(filename=paste(sep="", "BD_Final_GLM_Fitted_", "STV_STV_44_C", ".png"), 
    width = 960, height = 480, units = "px")
plot(predict ~ predict_data$Sec_unit_qty_c, 
     xlab="Actual volume", 
     ylab="Predicted volume",
     main="Fitted vs. Actual log quantity for 2015 - STV_STV_44_C")
dev.off()


#### UPI_UPI_24_Q ####
type_weekly_reg_basic8 <- 
  naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="UPI_UPI_24_Q",]
# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic8$week <- as.numeric(levels(type_weekly_reg_basic8$week))[type_weekly_reg_basic8$week]
type_weekly_reg_basic8$Inv_year <- as.numeric(levels(type_weekly_reg_basic8$Inv_year))[type_weekly_reg_basic8$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic8$week[1]
type_weekly_reg_basic8$week <- type_weekly_reg_basic8$week + (type_weekly_reg_basic8$Inv_year - 2013) * 52 - firstweek

# simple regression on a few variables.  
reg_glm <- glm(Sec_unit_qty_c ~ USD_unit_price + week
               + S.P_500 + S.P_Industrials + Consumer_Electronics,           
               data=type_weekly_reg_basic8)
# coefficients
summary(reg_glm)
# R2
1-summary(reg_glm)$deviance/summary(reg_glm)$null.deviance
# p-values
pvals <- summary(reg_glm)$coef[-1,4] #-1 to drop intercept
png(filename=paste(sep="", "BD_Final_GLM_Pvals_", "UPI_UPI_24_Q", ".png"), 
    width = 960, height = 480, units = "px")
hist(pvals,xlab="P-value",main="Histogram of P-values",col="light blue",
     breaks=10)
dev.off()
png(filename=paste(sep="", "BD_Final_GLM_FDR_", "UPI_UPI_24_Q", ".png"), 
    width = 960, height = 480, units = "px")
q = 0.05
alpha <- fdr_cut(pvals,q,plotit=TRUE)
dev.off()
signif <- which(pvals<=alpha)##which are significant
length(signif)

# out-of-sample prediction
train_data = type_weekly_reg_basic8[type_weekly_reg_basic8$Inv_year =="2014" ,]
# train the model WITHOUT these observation
# week variable excluded because there are issues (i.e., perfectly predictive)
trainreg <- glm(Sec_unit_qty_c ~ USD_unit_price
                + S.P_500 + S.P_Industrials + Consumer_Electronics,           
                data=train_data)
summary(trainreg)

# predicted demand on actual 2015 data
predict_data = type_weekly_reg_basic8[type_weekly_reg_basic8$Inv_year =="2015" ,]
predict <- predict(trainreg, newdata=predict_data)
predict
# plot the OOS fit
png(filename=paste(sep="", "BD_Final_GLM_Fitted_", "UPI_UPI_24_Q", ".png"), 
    width = 960, height = 480, units = "px")
plot(predict ~ predict_data$Sec_unit_qty_c, 
     xlab="Actual volume", 
     ylab="Predicted volume",
     main="Fitted vs. Actual log quantity for 2015 - UPI_UPI_24_Q")
dev.off()


#### STV_STV_32_D ####
type_weekly_reg_basic9 <- 
  naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_32_D",]
# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic9$week <- as.numeric(levels(type_weekly_reg_basic9$week))[type_weekly_reg_basic9$week]
type_weekly_reg_basic9$Inv_year <- as.numeric(levels(type_weekly_reg_basic9$Inv_year))[type_weekly_reg_basic9$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic9$week[1]
type_weekly_reg_basic9$week <- type_weekly_reg_basic9$week + (type_weekly_reg_basic9$Inv_year - 2013) * 52 - firstweek

# simple regression on a few variables.  
reg_glm <- glm(Sec_unit_qty_c ~ USD_unit_price + week
               + S.P_500 + S.P_Industrials + Consumer_Electronics,           
               data=type_weekly_reg_basic9)
# coefficients
summary(reg_glm)
# R2
1-summary(reg_glm)$deviance/summary(reg_glm)$null.deviance
# p-values
pvals <- summary(reg_glm)$coef[-1,4] #-1 to drop intercept
png(filename=paste(sep="", "BD_Final_GLM_Pvals_", "STV_STV_32_D", ".png"), 
    width = 960, height = 480, units = "px")
hist(pvals,xlab="P-value",main="Histogram of P-values",col="light blue",
     breaks=10)
dev.off()
png(filename=paste(sep="", "BD_Final_GLM_FDR_", "STV_STV_32_D", ".png"), 
    width = 960, height = 480, units = "px")
q = 0.05
alpha <- fdr_cut(pvals,q,plotit=TRUE)
dev.off()
signif <- which(pvals<=alpha)##which are significant
length(signif)

# out-of-sample prediction
train_data = type_weekly_reg_basic9[type_weekly_reg_basic9$Inv_year =="2014" ,]
# train the model WITHOUT these observation
# week variable excluded because there are issues (i.e., perfectly predictive)
trainreg <- glm(Sec_unit_qty_c ~ USD_unit_price
                + S.P_500 + S.P_Industrials + Consumer_Electronics,           
                data=train_data)
summary(trainreg)

# predicted demand on actual 2015 data
predict_data = type_weekly_reg_basic9[type_weekly_reg_basic9$Inv_year =="2015" ,]
predict <- predict(trainreg, newdata=predict_data)
predict
# plot the OOS fit
png(filename=paste(sep="", "BD_Final_GLM_Fitted_", "STV_STV_32_D", ".png"), 
    width = 960, height = 480, units = "px")
plot(predict ~ predict_data$Sec_unit_qty_c, 
     xlab="Actual volume", 
     ylab="Predicted volume",
     main="Fitted vs. Actual log quantity for 2015 - STV_STV_32_D")
dev.off()


#### STV_STV_12_BC ####
type_weekly_reg_basic10 <- 
  naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_12_BC",]
# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic10$week <- as.numeric(levels(type_weekly_reg_basic10$week))[type_weekly_reg_basic10$week]
type_weekly_reg_basic10$Inv_year <- as.numeric(levels(type_weekly_reg_basic10$Inv_year))[type_weekly_reg_basic10$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic10$week[1]
type_weekly_reg_basic10$week <- type_weekly_reg_basic10$week + (type_weekly_reg_basic10$Inv_year - 2013) * 52 - firstweek

# simple regression on a few variables.  
reg_glm <- glm(Sec_unit_qty_c ~ USD_unit_price + week
               + S.P_500 + S.P_Industrials + Consumer_Electronics,           
               data=type_weekly_reg_basic10)
# coefficients
summary(reg_glm)
# R2
1-summary(reg_glm)$deviance/summary(reg_glm)$null.deviance
# p-values
pvals <- summary(reg_glm)$coef[-1,4] #-1 to drop intercept
png(filename=paste(sep="", "BD_Final_GLM_Pvals_", "STV_STV_12_BC", ".png"), 
    width = 960, height = 480, units = "px")
hist(pvals,xlab="P-value",main="Histogram of P-values",col="light blue",
     breaks=10)
dev.off()
png(filename=paste(sep="", "BD_Final_GLM_FDR_", "STV_STV_12_BC", ".png"), 
    width = 960, height = 480, units = "px")
q = 0.05
alpha <- fdr_cut(pvals,q,plotit=TRUE)
dev.off()
signif <- which(pvals<=alpha)##which are significant
length(signif)

# out-of-sample prediction
train_data = type_weekly_reg_basic10[type_weekly_reg_basic10$Inv_year =="2014" ,]
# train the model WITHOUT these observation
# week variable excluded because there are issues (i.e., perfectly predictive)
trainreg <- glm(Sec_unit_qty_c ~ USD_unit_price
                + S.P_500 + S.P_Industrials + Consumer_Electronics,           
                data=train_data)
summary(trainreg)

# predicted demand on actual 2015 data
predict_data = type_weekly_reg_basic10[type_weekly_reg_basic10$Inv_year =="2015" ,]
predict <- predict(trainreg, newdata=predict_data)
predict
# plot the OOS fit
png(filename=paste(sep="", "BD_Final_GLM_Fitted_", "STV_STV_12_BC", ".png"), 
    width = 960, height = 480, units = "px")
plot(predict ~ predict_data$Sec_unit_qty_c, 
     xlab="Actual volume", 
     ylab="Predicted volume",
     main="Fitted vs. Actual log quantity for 2015 - STV_STV_12_BC")
dev.off()


#### STV_STV_8_C ####
type_weekly_reg_basic11 <- 
  naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_8_C",]
# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic11$week <- as.numeric(levels(type_weekly_reg_basic11$week))[type_weekly_reg_basic11$week]
type_weekly_reg_basic11$Inv_year <- as.numeric(levels(type_weekly_reg_basic11$Inv_year))[type_weekly_reg_basic11$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic11$week[1]
type_weekly_reg_basic11$week <- type_weekly_reg_basic11$week + (type_weekly_reg_basic11$Inv_year - 2013) * 52 - firstweek

# simple regression on a few variables.  
reg_glm <- glm(Sec_unit_qty_c ~ USD_unit_price + week
               + S.P_500 + S.P_Industrials + Consumer_Electronics,           
               data=type_weekly_reg_basic11)
# coefficients
summary(reg_glm)
# R2
1-summary(reg_glm)$deviance/summary(reg_glm)$null.deviance
# p-values
pvals <- summary(reg_glm)$coef[-1,4] #-1 to drop intercept
png(filename=paste(sep="", "BD_Final_GLM_Pvals_", "STV_STV_8_C", ".png"), 
    width = 960, height = 480, units = "px")
hist(pvals,xlab="P-value",main="Histogram of P-values",col="light blue",
     breaks=10)
dev.off()
png(filename=paste(sep="", "BD_Final_GLM_FDR_", "STV_STV_8_C", ".png"), 
    width = 960, height = 480, units = "px")
q = 0.05
alpha <- fdr_cut(pvals,q,plotit=TRUE)
dev.off()
signif <- which(pvals<=alpha)##which are significant
length(signif)

# out-of-sample prediction
train_data = type_weekly_reg_basic11[type_weekly_reg_basic11$Inv_year =="2014" ,]
# train the model WITHOUT these observation
# week variable excluded because there are issues (i.e., perfectly predictive)
trainreg <- glm(Sec_unit_qty_c ~ USD_unit_price
                + S.P_500 + S.P_Industrials + Consumer_Electronics,           
                data=train_data)
summary(trainreg)

# predicted demand on actual 2015 data
predict_data = type_weekly_reg_basic11[type_weekly_reg_basic11$Inv_year =="2015" ,]
predict <- predict(trainreg, newdata=predict_data)
predict
# plot the OOS fit
png(filename=paste(sep="", "BD_Final_GLM_Fitted_", "STV_STV_8_C", ".png"), 
    width = 960, height = 480, units = "px")
plot(predict ~ predict_data$Sec_unit_qty_c, 
     xlab="Actual volume", 
     ylab="Predicted volume",
     main="Fitted vs. Actual log quantity for 2015 - STV_STV_8_C")
dev.off()


#### STV_STV_32_C ####
type_weekly_reg_basic12 <- 
  naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_32_C",]
# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic12$week <- as.numeric(levels(type_weekly_reg_basic12$week))[type_weekly_reg_basic12$week]
type_weekly_reg_basic12$Inv_year <- as.numeric(levels(type_weekly_reg_basic12$Inv_year))[type_weekly_reg_basic12$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic12$week[1]
type_weekly_reg_basic12$week <- type_weekly_reg_basic12$week + (type_weekly_reg_basic12$Inv_year - 2013) * 52 - firstweek

# simple regression on a few variables.  
reg_glm <- glm(Sec_unit_qty_c ~ USD_unit_price + week
               + S.P_500 + S.P_Industrials + Consumer_Electronics,           
               data=type_weekly_reg_basic12)
# coefficients
summary(reg_glm)
# R2
1-summary(reg_glm)$deviance/summary(reg_glm)$null.deviance
# p-values
pvals <- summary(reg_glm)$coef[-1,4] #-1 to drop intercept
png(filename=paste(sep="", "BD_Final_GLM_Pvals_", "STV_STV_32_C", ".png"), 
    width = 960, height = 480, units = "px")
hist(pvals,xlab="P-value",main="Histogram of P-values",col="light blue",
     breaks=10)
dev.off()
png(filename=paste(sep="", "BD_Final_GLM_FDR_", "STV_STV_32_C", ".png"), 
    width = 960, height = 480, units = "px")
q = 0.05
alpha <- fdr_cut(pvals,q,plotit=TRUE)
dev.off()
signif <- which(pvals<=alpha)##which are significant
length(signif)

# out-of-sample prediction
train_data = type_weekly_reg_basic12[type_weekly_reg_basic12$Inv_year =="2014" ,]
# train the model WITHOUT these observation
# week variable excluded because there are issues (i.e., perfectly predictive)
trainreg <- glm(Sec_unit_qty_c ~ USD_unit_price
                + S.P_500 + S.P_Industrials + Consumer_Electronics,           
                data=train_data)
summary(trainreg)

# predicted demand on actual 2015 data
predict_data = type_weekly_reg_basic12[type_weekly_reg_basic12$Inv_year =="2015" ,]
predict <- predict(trainreg, newdata=predict_data)
predict
# plot the OOS fit
png(filename=paste(sep="", "BD_Final_GLM_Fitted_", "STV_STV_32_C", ".png"), 
    width = 960, height = 480, units = "px")
plot(predict ~ predict_data$Sec_unit_qty_c, 
     xlab="Actual volume", 
     ylab="Predicted volume",
     main="Fitted vs. Actual log quantity for 2015 - STV_STV_32_C")
dev.off()


#### STV_STV_24_X ####
type_weekly_reg_basic13 <- 
  naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_24_X",]
# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic13$week <- as.numeric(levels(type_weekly_reg_basic13$week))[type_weekly_reg_basic13$week]
type_weekly_reg_basic13$Inv_year <- as.numeric(levels(type_weekly_reg_basic13$Inv_year))[type_weekly_reg_basic13$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic13$week[1]
type_weekly_reg_basic13$week <- type_weekly_reg_basic13$week + (type_weekly_reg_basic13$Inv_year - 2013) * 52 - firstweek

# simple regression on a few variables.  
reg_glm <- glm(Sec_unit_qty_c ~ USD_unit_price + week
               + S.P_500 + S.P_Industrials + Consumer_Electronics,           
               data=type_weekly_reg_basic13)
# coefficients
summary(reg_glm)
# R2
1-summary(reg_glm)$deviance/summary(reg_glm)$null.deviance
# p-values
pvals <- summary(reg_glm)$coef[-1,4] #-1 to drop intercept
png(filename=paste(sep="", "BD_Final_GLM_Pvals_", "STV_STV_24_X", ".png"), 
    width = 960, height = 480, units = "px")
hist(pvals,xlab="P-value",main="Histogram of P-values",col="light blue",
     breaks=10)
dev.off()
png(filename=paste(sep="", "BD_Final_GLM_FDR_", "STV_STV_24_X", ".png"), 
    width = 960, height = 480, units = "px")
q = 0.05
alpha <- fdr_cut(pvals,q,plotit=TRUE)
dev.off()
signif <- which(pvals<=alpha)##which are significant
length(signif)

# out-of-sample prediction
train_data = type_weekly_reg_basic13[type_weekly_reg_basic13$Inv_year =="2014" ,]
# train the model WITHOUT these observation
# week variable excluded because there are issues (i.e., perfectly predictive)
trainreg <- glm(Sec_unit_qty_c ~ USD_unit_price
                + S.P_500 + S.P_Industrials + Consumer_Electronics,           
                data=train_data)
summary(trainreg)

# predicted demand on actual 2015 data
predict_data = type_weekly_reg_basic13[type_weekly_reg_basic13$Inv_year =="2015" ,]
predict <- predict(trainreg, newdata=predict_data)
predict
# plot the OOS fit
png(filename=paste(sep="", "BD_Final_GLM_Fitted_", "STV_STV_24_X", ".png"), 
    width = 960, height = 480, units = "px")
plot(predict ~ predict_data$Sec_unit_qty_c, 
     xlab="Actual volume", 
     ylab="Predicted volume",
     main="Fitted vs. Actual log quantity for 2015 - STV_STV_24_X")
dev.off()


#### STV_STV_32_X ####
type_weekly_reg_basic14 <- 
  naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_32_X",]
# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic14$week <- as.numeric(levels(type_weekly_reg_basic14$week))[type_weekly_reg_basic14$week]
type_weekly_reg_basic14$Inv_year <- as.numeric(levels(type_weekly_reg_basic14$Inv_year))[type_weekly_reg_basic14$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic14$week[1]
type_weekly_reg_basic14$week <- type_weekly_reg_basic14$week + (type_weekly_reg_basic14$Inv_year - 2013) * 52 - firstweek

# simple regression on a few variables.  
reg_glm <- glm(Sec_unit_qty_c ~ USD_unit_price + week
               + S.P_500 + S.P_Industrials + Consumer_Electronics,           
               data=type_weekly_reg_basic14)
# coefficients
summary(reg_glm)
# R2
1-summary(reg_glm)$deviance/summary(reg_glm)$null.deviance
# p-values
pvals <- summary(reg_glm)$coef[-1,4] #-1 to drop intercept
png(filename=paste(sep="", "BD_Final_GLM_Pvals_", "STV_STV_32_X", ".png"), 
    width = 960, height = 480, units = "px")
hist(pvals,xlab="P-value",main="Histogram of P-values",col="light blue",
     breaks=10)
dev.off()
png(filename=paste(sep="", "BD_Final_GLM_FDR_", "STV_STV_32_X", ".png"), 
    width = 960, height = 480, units = "px")
q = 0.05
alpha <- fdr_cut(pvals,q,plotit=TRUE)
dev.off()
signif <- which(pvals<=alpha)##which are significant
length(signif)

# out-of-sample prediction
train_data = type_weekly_reg_basic14[type_weekly_reg_basic14$Inv_year =="2014" ,]
# train the model WITHOUT these observation
# week variable excluded because there are issues (i.e., perfectly predictive)
trainreg <- glm(Sec_unit_qty_c ~ USD_unit_price
                + S.P_500 + S.P_Industrials + Consumer_Electronics,           
                data=train_data)
summary(trainreg)

# predicted demand on actual 2015 data
predict_data = type_weekly_reg_basic14[type_weekly_reg_basic14$Inv_year =="2015" ,]
predict <- predict(trainreg, newdata=predict_data)
predict
# plot the OOS fit
png(filename=paste(sep="", "BD_Final_GLM_Fitted_", "STV_STV_32_X", ".png"), 
    width = 960, height = 480, units = "px")
plot(predict ~ predict_data$Sec_unit_qty_c, 
     xlab="Actual volume", 
     ylab="Predicted volume",
     main="Fitted vs. Actual log quantity for 2015 - STV_STV_32_X")
dev.off()


#### STV_STV_44_D ####
type_weekly_reg_basic15 <- 
  naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_44_D",]
# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic15$week <- as.numeric(levels(type_weekly_reg_basic15$week))[type_weekly_reg_basic15$week]
type_weekly_reg_basic15$Inv_year <- as.numeric(levels(type_weekly_reg_basic15$Inv_year))[type_weekly_reg_basic15$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic15$week[1]
type_weekly_reg_basic15$week <- type_weekly_reg_basic15$week + (type_weekly_reg_basic15$Inv_year - 2013) * 52 - firstweek

# simple regression on a few variables.  
reg_glm <- glm(Sec_unit_qty_c ~ USD_unit_price + week
               + S.P_500 + S.P_Industrials + Consumer_Electronics,           
               data=type_weekly_reg_basic15)
# coefficients
summary(reg_glm)
# R2
1-summary(reg_glm)$deviance/summary(reg_glm)$null.deviance
# p-values
pvals <- summary(reg_glm)$coef[-1,4] #-1 to drop intercept
png(filename=paste(sep="", "BD_Final_GLM_Pvals_", "STV_STV_44_D", ".png"), 
    width = 960, height = 480, units = "px")
hist(pvals,xlab="P-value",main="Histogram of P-values",col="light blue",
     breaks=10)
dev.off()
png(filename=paste(sep="", "BD_Final_GLM_FDR_", "STV_STV_44_D", ".png"), 
    width = 960, height = 480, units = "px")
q = 0.05
alpha <- fdr_cut(pvals,q,plotit=TRUE)
dev.off()
signif <- which(pvals<=alpha)##which are significant
length(signif)

# out-of-sample prediction
train_data = type_weekly_reg_basic15[type_weekly_reg_basic15$Inv_year =="2014" ,]
# train the model WITHOUT these observation
# week variable excluded because there are issues (i.e., perfectly predictive)
trainreg <- glm(Sec_unit_qty_c ~ USD_unit_price
                + S.P_500 + S.P_Industrials + Consumer_Electronics,           
                data=train_data)
summary(trainreg)

# predicted demand on actual 2015 data
predict_data = type_weekly_reg_basic15[type_weekly_reg_basic15$Inv_year =="2015" ,]
predict <- predict(trainreg, newdata=predict_data)
predict
# plot the OOS fit
png(filename=paste(sep="", "BD_Final_GLM_Fitted_", "STV_STV_44_D", ".png"), 
    width = 960, height = 480, units = "px")
plot(predict ~ predict_data$Sec_unit_qty_c, 
     xlab="Actual volume", 
     ylab="Predicted volume",
     main="Fitted vs. Actual log quantity for 2015 - STV_STV_44_D")
dev.off()


#### STV_STV_16_BB ####
type_weekly_reg_basic16 <- 
  naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_16_BB",]
# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic16$week <- as.numeric(levels(type_weekly_reg_basic16$week))[type_weekly_reg_basic16$week]
type_weekly_reg_basic16$Inv_year <- as.numeric(levels(type_weekly_reg_basic16$Inv_year))[type_weekly_reg_basic16$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic16$week[1]
type_weekly_reg_basic16$week <- type_weekly_reg_basic16$week + (type_weekly_reg_basic16$Inv_year - 2013) * 52 - firstweek

# simple regression on a few variables.  
reg_glm <- glm(Sec_unit_qty_c ~ USD_unit_price + week
               + S.P_500 + S.P_Industrials + Consumer_Electronics,           
               data=type_weekly_reg_basic16)
# coefficients
summary(reg_glm)
# R2
1-summary(reg_glm)$deviance/summary(reg_glm)$null.deviance
# p-values
pvals <- summary(reg_glm)$coef[-1,4] #-1 to drop intercept
png(filename=paste(sep="", "BD_Final_GLM_Pvals_", "STV_STV_16_BB", ".png"), 
    width = 960, height = 480, units = "px")
hist(pvals,xlab="P-value",main="Histogram of P-values",col="light blue",
     breaks=10)
dev.off()
png(filename=paste(sep="", "BD_Final_GLM_FDR_", "STV_STV_16_BB", ".png"), 
    width = 960, height = 480, units = "px")
q = 0.05
alpha <- fdr_cut(pvals,q,plotit=TRUE)
dev.off()
signif <- which(pvals<=alpha)##which are significant
length(signif)

# out-of-sample prediction
train_data = type_weekly_reg_basic16[type_weekly_reg_basic16$Inv_year =="2014" ,]
# train the model WITHOUT these observation
# week variable excluded because there are issues (i.e., perfectly predictive)
trainreg <- glm(Sec_unit_qty_c ~ USD_unit_price
                + S.P_500 + S.P_Industrials + Consumer_Electronics,           
                data=train_data)
summary(trainreg)

# predicted demand on actual 2015 data
predict_data = type_weekly_reg_basic16[type_weekly_reg_basic16$Inv_year =="2015" ,]
predict <- predict(trainreg, newdata=predict_data)
predict
# plot the OOS fit
png(filename=paste(sep="", "BD_Final_GLM_Fitted_", "STV_STV_16_BB", ".png"), 
    width = 960, height = 480, units = "px")
plot(predict ~ predict_data$Sec_unit_qty_c, 
     xlab="Actual volume", 
     ylab="Predicted volume",
     main="Fitted vs. Actual log quantity for 2015 - STV_STV_16_BB")
dev.off()


#### STV_STV_44_X ####
type_weekly_reg_basic17 <- 
  naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_44_X",]
# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic17$week <- as.numeric(levels(type_weekly_reg_basic17$week))[type_weekly_reg_basic17$week]
type_weekly_reg_basic17$Inv_year <- as.numeric(levels(type_weekly_reg_basic17$Inv_year))[type_weekly_reg_basic17$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic17$week[1]
type_weekly_reg_basic17$week <- type_weekly_reg_basic17$week + (type_weekly_reg_basic17$Inv_year - 2013) * 52 - firstweek

# simple regression on a few variables.  
reg_glm <- glm(Sec_unit_qty_c ~ USD_unit_price + week
               + S.P_500 + S.P_Industrials + Consumer_Electronics,           
               data=type_weekly_reg_basic17)
# coefficients
summary(reg_glm)
# R2
1-summary(reg_glm)$deviance/summary(reg_glm)$null.deviance
# p-values
pvals <- summary(reg_glm)$coef[-1,4] #-1 to drop intercept
png(filename=paste(sep="", "BD_Final_GLM_Pvals_", "STV_STV_44_X", ".png"), 
    width = 960, height = 480, units = "px")
hist(pvals,xlab="P-value",main="Histogram of P-values",col="light blue",
     breaks=10)
dev.off()
png(filename=paste(sep="", "BD_Final_GLM_FDR_", "STV_STV_44_X", ".png"), 
    width = 960, height = 480, units = "px")
q = 0.05
alpha <- fdr_cut(pvals,q,plotit=TRUE)
dev.off()
signif <- which(pvals<=alpha)##which are significant
length(signif)

# out-of-sample prediction
train_data = type_weekly_reg_basic17[type_weekly_reg_basic17$Inv_year =="2014" ,]
# train the model WITHOUT these observation
# week variable excluded because there are issues (i.e., perfectly predictive)
trainreg <- glm(Sec_unit_qty_c ~ USD_unit_price
                + S.P_500 + S.P_Industrials + Consumer_Electronics,           
                data=train_data)
summary(trainreg)

# predicted demand on actual 2015 data
predict_data = type_weekly_reg_basic17[type_weekly_reg_basic17$Inv_year =="2015" ,]
predict <- predict(trainreg, newdata=predict_data)
predict
# plot the OOS fit
png(filename=paste(sep="", "BD_Final_GLM_Fitted_", "STV_STV_44_X", ".png"), 
    width = 960, height = 480, units = "px")
plot(predict ~ predict_data$Sec_unit_qty_c, 
     xlab="Actual volume", 
     ylab="Predicted volume",
     main="Fitted vs. Actual log quantity for 2015 - STV_STV_44_X")
dev.off()


#### STV_STV_16_BC ####
type_weekly_reg_basic18 <- 
  naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_16_BC",]
# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic18$week <- as.numeric(levels(type_weekly_reg_basic18$week))[type_weekly_reg_basic18$week]
type_weekly_reg_basic18$Inv_year <- as.numeric(levels(type_weekly_reg_basic18$Inv_year))[type_weekly_reg_basic18$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic18$week[1]
type_weekly_reg_basic18$week <- type_weekly_reg_basic18$week + (type_weekly_reg_basic18$Inv_year - 2013) * 52 - firstweek

# simple regression on a few variables.  
reg_glm <- glm(Sec_unit_qty_c ~ USD_unit_price + week
               + S.P_500 + S.P_Industrials + Consumer_Electronics,           
               data=type_weekly_reg_basic18)
# coefficients
summary(reg_glm)
# R2
1-summary(reg_glm)$deviance/summary(reg_glm)$null.deviance
# p-values
pvals <- summary(reg_glm)$coef[-1,4] #-1 to drop intercept
png(filename=paste(sep="", "BD_Final_GLM_Pvals_", "STV_STV_16_BC", ".png"), 
    width = 960, height = 480, units = "px")
hist(pvals,xlab="P-value",main="Histogram of P-values",col="light blue",
     breaks=10)
dev.off()
png(filename=paste(sep="", "BD_Final_GLM_FDR_", "STV_STV_16_BC", ".png"), 
    width = 960, height = 480, units = "px")
q = 0.05
alpha <- fdr_cut(pvals,q,plotit=TRUE)
dev.off()
signif <- which(pvals<=alpha)##which are significant
length(signif)

# out-of-sample prediction
train_data = type_weekly_reg_basic18[type_weekly_reg_basic18$Inv_year =="2014" ,]
# train the model WITHOUT these observation
# week variable excluded because there are issues (i.e., perfectly predictive)
trainreg <- glm(Sec_unit_qty_c ~ USD_unit_price
                + S.P_500 + S.P_Industrials + Consumer_Electronics,           
                data=train_data)
summary(trainreg)

# predicted demand on actual 2015 data
predict_data = type_weekly_reg_basic18[type_weekly_reg_basic18$Inv_year =="2015" ,]
predict <- predict(trainreg, newdata=predict_data)
predict
# plot the OOS fit
png(filename=paste(sep="", "BD_Final_GLM_Fitted_", "STV_STV_16_BC", ".png"), 
    width = 960, height = 480, units = "px")
plot(predict ~ predict_data$Sec_unit_qty_c, 
     xlab="Actual volume", 
     ylab="Predicted volume",
     main="Fitted vs. Actual log quantity for 2015 - STV_STV_16_BC")
dev.off()


#### STV_STV_12_BB ####
type_weekly_reg_basic19 <- 
  naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_12_BB",]
# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic19$week <- as.numeric(levels(type_weekly_reg_basic19$week))[type_weekly_reg_basic19$week]
type_weekly_reg_basic19$Inv_year <- as.numeric(levels(type_weekly_reg_basic19$Inv_year))[type_weekly_reg_basic19$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic19$week[1]
type_weekly_reg_basic19$week <- type_weekly_reg_basic19$week + (type_weekly_reg_basic19$Inv_year - 2013) * 52 - firstweek

# simple regression on a few variables.  
reg_glm <- glm(Sec_unit_qty_c ~ USD_unit_price + week
               + S.P_500 + S.P_Industrials + Consumer_Electronics,           
               data=type_weekly_reg_basic19)
# coefficients
summary(reg_glm)
# R2
1-summary(reg_glm)$deviance/summary(reg_glm)$null.deviance
# p-values
pvals <- summary(reg_glm)$coef[-1,4] #-1 to drop intercept
png(filename=paste(sep="", "BD_Final_GLM_Pvals_", "STV_STV_12_BB", ".png"), 
    width = 960, height = 480, units = "px")
hist(pvals,xlab="P-value",main="Histogram of P-values",col="light blue",
     breaks=10)
dev.off()
png(filename=paste(sep="", "BD_Final_GLM_FDR_", "STV_STV_12_BB", ".png"), 
    width = 960, height = 480, units = "px")
q = 0.05
alpha <- fdr_cut(pvals,q,plotit=TRUE)
dev.off()
signif <- which(pvals<=alpha)##which are significant
length(signif)

# out-of-sample prediction
train_data = type_weekly_reg_basic19[type_weekly_reg_basic19$Inv_year =="2014" ,]
# train the model WITHOUT these observation
# week variable excluded because there are issues (i.e., perfectly predictive)
trainreg <- glm(Sec_unit_qty_c ~ USD_unit_price
                + S.P_500 + S.P_Industrials + Consumer_Electronics,           
                data=train_data)
summary(trainreg)

# predicted demand on actual 2015 data
predict_data = type_weekly_reg_basic19[type_weekly_reg_basic19$Inv_year =="2015" ,]
predict <- predict(trainreg, newdata=predict_data)
predict
# plot the OOS fit
png(filename=paste(sep="", "BD_Final_GLM_Fitted_", "STV_STV_12_BB", ".png"), 
    width = 960, height = 480, units = "px")
plot(predict ~ predict_data$Sec_unit_qty_c, 
     xlab="Actual volume", 
     ylab="Predicted volume",
     main="Fitted vs. Actual log quantity for 2015 - STV_STV_12_BB")
dev.off()


#### ADP_ADP_44_X ####
type_weekly_reg_basic20 <- 
  naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="ADP_ADP_44_X",]
# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic20$week <- as.numeric(levels(type_weekly_reg_basic20$week))[type_weekly_reg_basic20$week]
type_weekly_reg_basic20$Inv_year <- as.numeric(levels(type_weekly_reg_basic20$Inv_year))[type_weekly_reg_basic20$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic20$week[1]
type_weekly_reg_basic20$week <- type_weekly_reg_basic20$week + (type_weekly_reg_basic20$Inv_year - 2013) * 52 - firstweek

# simple regression on a few variables.  
reg_glm <- glm(Sec_unit_qty_c ~ USD_unit_price + week
               + S.P_500 + S.P_Industrials + Consumer_Electronics,           
               data=type_weekly_reg_basic20)
# coefficients
summary(reg_glm)
# R2
1-summary(reg_glm)$deviance/summary(reg_glm)$null.deviance
# p-values
pvals <- summary(reg_glm)$coef[-1,4] #-1 to drop intercept
png(filename=paste(sep="", "BD_Final_GLM_Pvals_", "ADP_ADP_44_X", ".png"), 
    width = 960, height = 480, units = "px")
hist(pvals,xlab="P-value",main="Histogram of P-values",col="light blue",
     breaks=10)
dev.off()
png(filename=paste(sep="", "BD_Final_GLM_FDR_", "ADP_ADP_44_X", ".png"), 
    width = 960, height = 480, units = "px")
q = 0.05
alpha <- fdr_cut(pvals,q,plotit=TRUE)
dev.off()
signif <- which(pvals<=alpha)##which are significant
length(signif)

# out-of-sample prediction
train_data = type_weekly_reg_basic20[type_weekly_reg_basic20$Inv_year =="2014" ,]
# train the model WITHOUT these observation
# week variable excluded because there are issues (i.e., perfectly predictive)
trainreg <- glm(Sec_unit_qty_c ~ USD_unit_price
                + S.P_500 + S.P_Industrials + Consumer_Electronics,           
                data=train_data)
summary(trainreg)

# predicted demand on actual 2015 data
predict_data = type_weekly_reg_basic20[type_weekly_reg_basic20$Inv_year =="2015" ,]
predict <- predict(trainreg, newdata=predict_data)
predict
# plot the OOS fit
png(filename=paste(sep="", "BD_Final_GLM_Fitted_", "ADP_ADP_44_X", ".png"), 
    width = 960, height = 480, units = "px")
plot(predict ~ predict_data$Sec_unit_qty_c, 
     xlab="Actual volume", 
     ylab="Predicted volume",
     main="Fitted vs. Actual log quantity for 2015 - ADP_ADP_44_X")
dev.off()




#######################################################################
#### Lasso Regressions and Cross Validation ####
#######################################################################

#### STV_STV_12_C ####
type_weekly_reg_basic21 = naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_12_C",]

# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic21$week <- as.numeric(levels(type_weekly_reg_basic21$week))[type_weekly_reg_basic21$week]
type_weekly_reg_basic21$Inv_year <- as.numeric(levels(type_weekly_reg_basic21$Inv_year))[type_weekly_reg_basic21$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic21$week[1]
type_weekly_reg_basic21$week <- type_weekly_reg_basic21$week + (type_weekly_reg_basic21$Inv_year - 2013) * 52 - firstweek

# create X-variable model matrix for gamlr
type_reg_x <- model.matrix(Sec_unit_qty_c ~ USD_unit_price + week 
                           + S.P_500 + S.P_Industrials + Consumer_Electronics,
                           data=type_weekly_reg_basic21)[,-1]
# Run regression
reg_gamlr <- gamlr(type_reg_x, type_weekly_reg_basic21$Sec_unit_qty_c, 
              verb=TRUE, lambda.min.ratio=0.0001)
summary(reg_gamlr)
coef(reg_gamlr)

# AICc, AIC and BIC
log(reg_gamlr$lambda[which.min(AICc(reg_gamlr))])
log(reg_gamlr$lambda[which.min(AIC(reg_gamlr))])
log(reg_gamlr$lambda[which.min(BIC(reg_gamlr))])

# cross-validation lasso regression - use this output to predict out-of-sample data
cv_reg <- cv.gamlr(type_reg_x, type_weekly_reg_basic21$Sec_unit_qty_c,
                    verb=TRUE, lambda.min.ratio=0.0001)
summary(cv_reg)
coef(cv_reg) ##1 standard error selection
coef(cv_reg, select="min")
log(cv_reg$lambda.min)
log(cv_reg$lambda.1se)

# compare CV and IC (gamlr) selections on lasso path plot
png(filename=paste(sep="", "BD_Final_Lasso-CV_Criteria_", "STV_STV_12_C", ".png"), 
    width = 960, height = 480, units = "px")
par(mfrow=c(1,1))
LL=log(reg_gamlr$lambda)
plot(reg_gamlr, col="grey", main="Path Plots with Seclection Criteria - STV_STV_12_C")
abline(v=LL[which.min(AICc(reg_gamlr))], col="black", lty=2)
abline(v=LL[which.min(AIC(reg_gamlr))], col="orange", lty=2)
abline(v=LL[which.min(BIC(reg_gamlr))], col="green", lty=2)
abline(v=log(cv_reg$lambda.min), col="blue", lty=2)
abline(v=log(cv_reg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, cex=0.75, pt.cex = 1,
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV min","CV 1se"))
dev.off()


#### STV_STV_12_D ####
type_weekly_reg_basic22 = naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_12_D",]

# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic22$week <- as.numeric(levels(type_weekly_reg_basic22$week))[type_weekly_reg_basic22$week]
type_weekly_reg_basic22$Inv_year <- as.numeric(levels(type_weekly_reg_basic22$Inv_year))[type_weekly_reg_basic22$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic22$week[1]
type_weekly_reg_basic22$week <- type_weekly_reg_basic22$week + (type_weekly_reg_basic22$Inv_year - 2013) * 52 - firstweek

# create X-variable model matrix for gamlr
type_reg_x <- model.matrix(Sec_unit_qty_c ~ USD_unit_price + week 
                           + S.P_500 + S.P_Industrials + Consumer_Electronics,
                           data=type_weekly_reg_basic22)[,-1]
# Run regression
reg_gamlr <- gamlr(type_reg_x, type_weekly_reg_basic22$Sec_unit_qty_c, 
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(reg_gamlr)
coef(reg_gamlr)

# AICc, AIC and BIC
log(reg_gamlr$lambda[which.min(AICc(reg_gamlr))])
log(reg_gamlr$lambda[which.min(AIC(reg_gamlr))])
log(reg_gamlr$lambda[which.min(BIC(reg_gamlr))])

# cross-validation lasso regression - use this output to predict out-of-sample data
cv_reg <- cv.gamlr(type_reg_x, type_weekly_reg_basic22$Sec_unit_qty_c,
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(cv_reg)
coef(cv_reg) ##1 standard error selection
coef(cv_reg, select="min")
log(cv_reg$lambda.min)
log(cv_reg$lambda.1se)

# compare CV and IC (gamlr) selections on lasso path plot
png(filename=paste(sep="", "BD_Final_Lasso-CV_Criteria_", "STV_STV_12_D", ".png"), 
    width = 960, height = 480, units = "px")
par(mfrow=c(1,1))
LL=log(reg_gamlr$lambda)
plot(reg_gamlr, col="grey", main="Path Plots with Selection Criteria - STV_STV_12_D")
abline(v=LL[which.min(AICc(reg_gamlr))], col="black", lty=2)
abline(v=LL[which.min(AIC(reg_gamlr))], col="orange", lty=2)
abline(v=LL[which.min(BIC(reg_gamlr))], col="green", lty=2)
abline(v=log(cv_reg$lambda.min), col="blue", lty=2)
abline(v=log(cv_reg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, cex=0.75, pt.cex = 1,
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV min","CV 1se"))
dev.off()


#### STV_STV_24_D ####
type_weekly_reg_basic23 = naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_24_D",]

# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic23$week <- as.numeric(levels(type_weekly_reg_basic23$week))[type_weekly_reg_basic23$week]
type_weekly_reg_basic23$Inv_year <- as.numeric(levels(type_weekly_reg_basic23$Inv_year))[type_weekly_reg_basic23$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic23$week[1]
type_weekly_reg_basic23$week <- type_weekly_reg_basic23$week + (type_weekly_reg_basic23$Inv_year - 2013) * 52 - firstweek

# create X-variable model matrix for gamlr
type_reg_x <- model.matrix(Sec_unit_qty_c ~ USD_unit_price + week 
                           + S.P_500 + S.P_Industrials + Consumer_Electronics,
                           data=type_weekly_reg_basic23)[,-1]
# Run regression
reg_gamlr <- gamlr(type_reg_x, type_weekly_reg_basic23$Sec_unit_qty_c, 
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(reg_gamlr)
coef(reg_gamlr)

# AICc, AIC and BIC
log(reg_gamlr$lambda[which.min(AICc(reg_gamlr))])
log(reg_gamlr$lambda[which.min(AIC(reg_gamlr))])
log(reg_gamlr$lambda[which.min(BIC(reg_gamlr))])

# cross-validation lasso regression - use this output to predict out-of-sample data
cv_reg <- cv.gamlr(type_reg_x, type_weekly_reg_basic23$Sec_unit_qty_c,
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(cv_reg)
coef(cv_reg) ##1 standard error selection
coef(cv_reg, select="min")
log(cv_reg$lambda.min)
log(cv_reg$lambda.1se)

# compare CV and IC (gamlr) selections on lasso path plot
png(filename=paste(sep="", "BD_Final_Lasso-CV_Criteria_", "STV_STV_24_D", ".png"), 
    width = 960, height = 480, units = "px")
par(mfrow=c(1,1))
LL=log(reg_gamlr$lambda)
plot(reg_gamlr, col="grey", main="Path Plots with Selection Criteria -  STV_STV_24_D")
abline(v=LL[which.min(AICc(reg_gamlr))], col="black", lty=2)
abline(v=LL[which.min(AIC(reg_gamlr))], col="orange", lty=2)
abline(v=LL[which.min(BIC(reg_gamlr))], col="green", lty=2)
abline(v=log(cv_reg$lambda.min), col="blue", lty=2)
abline(v=log(cv_reg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, cex=0.75, pt.cex = 1,
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV min","CV 1se"))
dev.off()


#### STV_STV_16_C ####
type_weekly_reg_basic24 = naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_16_C",]

# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic24$week <- as.numeric(levels(type_weekly_reg_basic24$week))[type_weekly_reg_basic24$week]
type_weekly_reg_basic24$Inv_year <- as.numeric(levels(type_weekly_reg_basic24$Inv_year))[type_weekly_reg_basic24$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic24$week[1]
type_weekly_reg_basic24$week <- type_weekly_reg_basic24$week + (type_weekly_reg_basic24$Inv_year - 2013) * 52 - firstweek

# create X-variable model matrix for gamlr
type_reg_x <- model.matrix(Sec_unit_qty_c ~ USD_unit_price + week 
                           + S.P_500 + S.P_Industrials + Consumer_Electronics,
                           data=type_weekly_reg_basic24)[,-1]
# Run regression
reg_gamlr <- gamlr(type_reg_x, type_weekly_reg_basic24$Sec_unit_qty_c, 
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(reg_gamlr)
coef(reg_gamlr)

# AICc, AIC and BIC
log(reg_gamlr$lambda[which.min(AICc(reg_gamlr))])
log(reg_gamlr$lambda[which.min(AIC(reg_gamlr))])
log(reg_gamlr$lambda[which.min(BIC(reg_gamlr))])

# cross-validation lasso regression - use this output to predict out-of-sample data
cv_reg <- cv.gamlr(type_reg_x, type_weekly_reg_basic24$Sec_unit_qty_c,
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(cv_reg)
coef(cv_reg) ##1 standard error selection
coef(cv_reg, select="min")
log(cv_reg$lambda.min)
log(cv_reg$lambda.1se)

# compare CV and IC (gamlr) selections on lasso path plot
png(filename=paste(sep="", "BD_Final_Lasso-CV_Criteria_", "STV_STV_16_C", ".png"), 
    width = 960, height = 480, units = "px")
par(mfrow=c(1,1))
LL=log(reg_gamlr$lambda)
plot(reg_gamlr, col="grey", main="Path Plots with Selection Criteria -  STV_STV_16_C")
abline(v=LL[which.min(AICc(reg_gamlr))], col="black", lty=2)
abline(v=LL[which.min(AIC(reg_gamlr))], col="orange", lty=2)
abline(v=LL[which.min(BIC(reg_gamlr))], col="green", lty=2)
abline(v=log(cv_reg$lambda.min), col="blue", lty=2)
abline(v=log(cv_reg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, cex=0.75, pt.cex = 1,
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV min","CV 1se"))
dev.off()


#### STV_STV_16_D ####
type_weekly_reg_basic25 = naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_16_D",]

# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic25$week <- as.numeric(levels(type_weekly_reg_basic25$week))[type_weekly_reg_basic25$week]
type_weekly_reg_basic25$Inv_year <- as.numeric(levels(type_weekly_reg_basic25$Inv_year))[type_weekly_reg_basic25$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic25$week[1]
type_weekly_reg_basic25$week <- type_weekly_reg_basic25$week + (type_weekly_reg_basic25$Inv_year - 2013) * 52 - firstweek

# create X-variable model matrix for gamlr
type_reg_x <- model.matrix(Sec_unit_qty_c ~ USD_unit_price + week 
                           + S.P_500 + S.P_Industrials + Consumer_Electronics,
                           data=type_weekly_reg_basic25)[,-1]
# Run regression
reg_gamlr <- gamlr(type_reg_x, type_weekly_reg_basic25$Sec_unit_qty_c, 
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(reg_gamlr)
coef(reg_gamlr)

# AICc, AIC and BIC
log(reg_gamlr$lambda[which.min(AICc(reg_gamlr))])
log(reg_gamlr$lambda[which.min(AIC(reg_gamlr))])
log(reg_gamlr$lambda[which.min(BIC(reg_gamlr))])

# cross-validation lasso regression - use this output to predict out-of-sample data
cv_reg <- cv.gamlr(type_reg_x, type_weekly_reg_basic25$Sec_unit_qty_c,
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(cv_reg)
coef(cv_reg) ##1 standard error selection
coef(cv_reg, select="min")
log(cv_reg$lambda.min)
log(cv_reg$lambda.1se)

# compare CV and IC (gamlr) selections on lasso path plot
png(filename=paste(sep="", "BD_Final_Lasso-CV_Criteria_", "STV_STV_16_D", ".png"), 
    width = 960, height = 480, units = "px")
par(mfrow=c(1,1))
LL=log(reg_gamlr$lambda)
plot(reg_gamlr, col="grey", main="Path Plots with Selection Criteria -  STV_STV_16_D")
abline(v=LL[which.min(AICc(reg_gamlr))], col="black", lty=2)
abline(v=LL[which.min(AIC(reg_gamlr))], col="orange", lty=2)
abline(v=LL[which.min(BIC(reg_gamlr))], col="green", lty=2)
abline(v=log(cv_reg$lambda.min), col="blue", lty=2)
abline(v=log(cv_reg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, cex=0.75, pt.cex = 1,
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV min","CV 1se"))
dev.off()


#### STV_STV_24_C ####
type_weekly_reg_basic26 = naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_24_C",]

# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic26$week <- as.numeric(levels(type_weekly_reg_basic26$week))[type_weekly_reg_basic26$week]
type_weekly_reg_basic26$Inv_year <- as.numeric(levels(type_weekly_reg_basic26$Inv_year))[type_weekly_reg_basic26$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic26$week[1]
type_weekly_reg_basic26$week <- type_weekly_reg_basic26$week + (type_weekly_reg_basic26$Inv_year - 2013) * 52 - firstweek

# create X-variable model matrix for gamlr
type_reg_x <- model.matrix(Sec_unit_qty_c ~ USD_unit_price + week 
                           + S.P_500 + S.P_Industrials + Consumer_Electronics,
                           data=type_weekly_reg_basic26)[,-1]
# Run regression
reg_gamlr <- gamlr(type_reg_x, type_weekly_reg_basic26$Sec_unit_qty_c, 
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(reg_gamlr)
coef(reg_gamlr)

# AICc, AIC and BIC
log(reg_gamlr$lambda[which.min(AICc(reg_gamlr))])
log(reg_gamlr$lambda[which.min(AIC(reg_gamlr))])
log(reg_gamlr$lambda[which.min(BIC(reg_gamlr))])

# cross-validation lasso regression - use this output to predict out-of-sample data
cv_reg <- cv.gamlr(type_reg_x, type_weekly_reg_basic26$Sec_unit_qty_c,
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(cv_reg)
coef(cv_reg) ##1 standard error selection
coef(cv_reg, select="min")
log(cv_reg$lambda.min)
log(cv_reg$lambda.1se)

# compare CV and IC (gamlr) selections on lasso path plot
png(filename=paste(sep="", "BD_Final_Lasso-CV_Criteria_", "STV_STV_24_C", ".png"), 
    width = 960, height = 480, units = "px")
par(mfrow=c(1,1))
LL=log(reg_gamlr$lambda)
plot(reg_gamlr, col="grey", main="Path Plots with Selection Criteria -  STV_STV_24_C")
abline(v=LL[which.min(AICc(reg_gamlr))], col="black", lty=2)
abline(v=LL[which.min(AIC(reg_gamlr))], col="orange", lty=2)
abline(v=LL[which.min(BIC(reg_gamlr))], col="green", lty=2)
abline(v=log(cv_reg$lambda.min), col="blue", lty=2)
abline(v=log(cv_reg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, cex=0.75, pt.cex = 1,
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV min","CV 1se"))
dev.off()


#### STV_STV_44_C ####
type_weekly_reg_basic27 = naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_44_C",]

# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic27$week <- as.numeric(levels(type_weekly_reg_basic27$week))[type_weekly_reg_basic27$week]
type_weekly_reg_basic27$Inv_year <- as.numeric(levels(type_weekly_reg_basic27$Inv_year))[type_weekly_reg_basic27$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic27$week[1]
type_weekly_reg_basic27$week <- type_weekly_reg_basic27$week + (type_weekly_reg_basic27$Inv_year - 2013) * 52 - firstweek

# create X-variable model matrix for gamlr
type_reg_x <- model.matrix(Sec_unit_qty_c ~ USD_unit_price + week 
                           + S.P_500 + S.P_Industrials + Consumer_Electronics,
                           data=type_weekly_reg_basic27)[,-1]
# Run regression
reg_gamlr <- gamlr(type_reg_x, type_weekly_reg_basic27$Sec_unit_qty_c, 
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(reg_gamlr)
coef(reg_gamlr)

# AICc, AIC and BIC
log(reg_gamlr$lambda[which.min(AICc(reg_gamlr))])
log(reg_gamlr$lambda[which.min(AIC(reg_gamlr))])
log(reg_gamlr$lambda[which.min(BIC(reg_gamlr))])

# cross-validation lasso regression - use this output to predict out-of-sample data
cv_reg <- cv.gamlr(type_reg_x, type_weekly_reg_basic27$Sec_unit_qty_c,
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(cv_reg)
coef(cv_reg) ##1 standard error selection
coef(cv_reg, select="min")
log(cv_reg$lambda.min)
log(cv_reg$lambda.1se)

# compare CV and IC (gamlr) selections on lasso path plot
png(filename=paste(sep="", "BD_Final_Lasso-CV_Criteria_", "STV_STV_44_C", ".png"), 
    width = 960, height = 480, units = "px")
par(mfrow=c(1,1))
LL=log(reg_gamlr$lambda)
plot(reg_gamlr, col="grey", main="Path Plots with Selection Criteria -  STV_STV_44_C")
abline(v=LL[which.min(AICc(reg_gamlr))], col="black", lty=2)
abline(v=LL[which.min(AIC(reg_gamlr))], col="orange", lty=2)
abline(v=LL[which.min(BIC(reg_gamlr))], col="green", lty=2)
abline(v=log(cv_reg$lambda.min), col="blue", lty=2)
abline(v=log(cv_reg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, cex=0.75, pt.cex = 1,
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV min","CV 1se"))
dev.off()


#### UPI_UPI_24_Q ####
type_weekly_reg_basic28 = naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="UPI_UPI_24_Q",]

# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic28$week <- as.numeric(levels(type_weekly_reg_basic28$week))[type_weekly_reg_basic28$week]
type_weekly_reg_basic28$Inv_year <- as.numeric(levels(type_weekly_reg_basic28$Inv_year))[type_weekly_reg_basic28$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic28$week[1]
type_weekly_reg_basic28$week <- type_weekly_reg_basic28$week + (type_weekly_reg_basic28$Inv_year - 2013) * 52 - firstweek

# create X-variable model matrix for gamlr
type_reg_x <- model.matrix(Sec_unit_qty_c ~ USD_unit_price + week 
                           + S.P_500 + S.P_Industrials + Consumer_Electronics,
                           data=type_weekly_reg_basic28)[,-1]
# Run regression
reg_gamlr <- gamlr(type_reg_x, type_weekly_reg_basic28$Sec_unit_qty_c, 
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(reg_gamlr)
coef(reg_gamlr)

# AICc, AIC and BIC
log(reg_gamlr$lambda[which.min(AICc(reg_gamlr))])
log(reg_gamlr$lambda[which.min(AIC(reg_gamlr))])
log(reg_gamlr$lambda[which.min(BIC(reg_gamlr))])

# cross-validation lasso regression - use this output to predict out-of-sample data
cv_reg <- cv.gamlr(type_reg_x, type_weekly_reg_basic28$Sec_unit_qty_c,
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(cv_reg)
coef(cv_reg) ##1 standard error selection
coef(cv_reg, select="min")
log(cv_reg$lambda.min)
log(cv_reg$lambda.1se)

# compare CV and IC (gamlr) selections on lasso path plot
png(filename=paste(sep="", "BD_Final_Lasso-CV_Criteria_", "UPI_UPI_24_Q", ".png"), 
    width = 960, height = 480, units = "px")
par(mfrow=c(1,1))
LL=log(reg_gamlr$lambda)
plot(reg_gamlr, col="grey", main="Path Plots with Selection Criteria -  UPI_UPI_24_Q")
abline(v=LL[which.min(AICc(reg_gamlr))], col="black", lty=2)
abline(v=LL[which.min(AIC(reg_gamlr))], col="orange", lty=2)
abline(v=LL[which.min(BIC(reg_gamlr))], col="green", lty=2)
abline(v=log(cv_reg$lambda.min), col="blue", lty=2)
abline(v=log(cv_reg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, cex=0.75, pt.cex = 1,
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV min","CV 1se"))
dev.off()


#### STV_STV_32_D ####
type_weekly_reg_basic29 = naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_32_D",]

# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic29$week <- as.numeric(levels(type_weekly_reg_basic29$week))[type_weekly_reg_basic29$week]
type_weekly_reg_basic29$Inv_year <- as.numeric(levels(type_weekly_reg_basic29$Inv_year))[type_weekly_reg_basic29$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic29$week[1]
type_weekly_reg_basic29$week <- type_weekly_reg_basic29$week + (type_weekly_reg_basic29$Inv_year - 2013) * 52 - firstweek

# create X-variable model matrix for gamlr
type_reg_x <- model.matrix(Sec_unit_qty_c ~ USD_unit_price + week 
                           + S.P_500 + S.P_Industrials + Consumer_Electronics,
                           data=type_weekly_reg_basic29)[,-1]
# Run regression
reg_gamlr <- gamlr(type_reg_x, type_weekly_reg_basic29$Sec_unit_qty_c, 
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(reg_gamlr)
coef(reg_gamlr)

# AICc, AIC and BIC
log(reg_gamlr$lambda[which.min(AICc(reg_gamlr))])
log(reg_gamlr$lambda[which.min(AIC(reg_gamlr))])
log(reg_gamlr$lambda[which.min(BIC(reg_gamlr))])

# cross-validation lasso regression - use this output to predict out-of-sample data
cv_reg <- cv.gamlr(type_reg_x, type_weekly_reg_basic29$Sec_unit_qty_c,
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(cv_reg)
coef(cv_reg) ##1 standard error selection
coef(cv_reg, select="min")
log(cv_reg$lambda.min)
log(cv_reg$lambda.1se)

# compare CV and IC (gamlr) selections on lasso path plot
png(filename=paste(sep="", "BD_Final_Lasso-CV_Criteria_", "STV_STV_32_D", ".png"), 
    width = 960, height = 480, units = "px")
par(mfrow=c(1,1))
LL=log(reg_gamlr$lambda)
plot(reg_gamlr, col="grey", main="Path Plots with Selection Criteria -  STV_STV_32_D")
abline(v=LL[which.min(AICc(reg_gamlr))], col="black", lty=2)
abline(v=LL[which.min(AIC(reg_gamlr))], col="orange", lty=2)
abline(v=LL[which.min(BIC(reg_gamlr))], col="green", lty=2)
abline(v=log(cv_reg$lambda.min), col="blue", lty=2)
abline(v=log(cv_reg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, cex=0.75, pt.cex = 1,
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV min","CV 1se"))
dev.off()


#### STV_STV_12_BC ####
type_weekly_reg_basic30 = naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_12_BC",]

# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic30$week <- as.numeric(levels(type_weekly_reg_basic30$week))[type_weekly_reg_basic30$week]
type_weekly_reg_basic30$Inv_year <- as.numeric(levels(type_weekly_reg_basic30$Inv_year))[type_weekly_reg_basic30$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic30$week[1]
type_weekly_reg_basic30$week <- type_weekly_reg_basic30$week + (type_weekly_reg_basic30$Inv_year - 2013) * 52 - firstweek

# create X-variable model matrix for gamlr
type_reg_x <- model.matrix(Sec_unit_qty_c ~ USD_unit_price + week 
                           + S.P_500 + S.P_Industrials + Consumer_Electronics,
                           data=type_weekly_reg_basic30)[,-1]
# Run regression
reg_gamlr <- gamlr(type_reg_x, type_weekly_reg_basic30$Sec_unit_qty_c, 
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(reg_gamlr)
coef(reg_gamlr)

# AICc, AIC and BIC
log(reg_gamlr$lambda[which.min(AICc(reg_gamlr))])
log(reg_gamlr$lambda[which.min(AIC(reg_gamlr))])
log(reg_gamlr$lambda[which.min(BIC(reg_gamlr))])

# cross-validation lasso regression - use this output to predict out-of-sample data
cv_reg <- cv.gamlr(type_reg_x, type_weekly_reg_basic30$Sec_unit_qty_c,
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(cv_reg)
coef(cv_reg) ##1 standard error selection
coef(cv_reg, select="min")
log(cv_reg$lambda.min)
log(cv_reg$lambda.1se)

# compare CV and IC (gamlr) selections on lasso path plot
png(filename=paste(sep="", "BD_Final_Lasso-CV_Criteria_", "STV_STV_12_BC", ".png"), 
    width = 960, height = 480, units = "px")
par(mfrow=c(1,1))
LL=log(reg_gamlr$lambda)
plot(reg_gamlr, col="grey", main="Path Plots with Selection Criteria -  STV_STV_12_BC")
abline(v=LL[which.min(AICc(reg_gamlr))], col="black", lty=2)
abline(v=LL[which.min(AIC(reg_gamlr))], col="orange", lty=2)
abline(v=LL[which.min(BIC(reg_gamlr))], col="green", lty=2)
abline(v=log(cv_reg$lambda.min), col="blue", lty=2)
abline(v=log(cv_reg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, cex=0.75, pt.cex = 1,
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV min","CV 1se"))
dev.off()


#### STV_STV_8_C ####
type_weekly_reg_basic31 = naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_8_C",]

# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic31$week <- as.numeric(levels(type_weekly_reg_basic31$week))[type_weekly_reg_basic31$week]
type_weekly_reg_basic31$Inv_year <- as.numeric(levels(type_weekly_reg_basic31$Inv_year))[type_weekly_reg_basic31$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic31$week[1]
type_weekly_reg_basic31$week <- type_weekly_reg_basic31$week + (type_weekly_reg_basic31$Inv_year - 2013) * 52 - firstweek

# create X-variable model matrix for gamlr
type_reg_x <- model.matrix(Sec_unit_qty_c ~ USD_unit_price + week 
                           + S.P_500 + S.P_Industrials + Consumer_Electronics,
                           data=type_weekly_reg_basic31)[,-1]
# Run regression
reg_gamlr <- gamlr(type_reg_x, type_weekly_reg_basic31$Sec_unit_qty_c, 
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(reg_gamlr)
coef(reg_gamlr)

# AICc, AIC and BIC
log(reg_gamlr$lambda[which.min(AICc(reg_gamlr))])
log(reg_gamlr$lambda[which.min(AIC(reg_gamlr))])
log(reg_gamlr$lambda[which.min(BIC(reg_gamlr))])

# cross-validation lasso regression - use this output to predict out-of-sample data
cv_reg <- cv.gamlr(type_reg_x, type_weekly_reg_basic31$Sec_unit_qty_c,
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(cv_reg)
coef(cv_reg) ##1 standard error selection
coef(cv_reg, select="min")
log(cv_reg$lambda.min)
log(cv_reg$lambda.1se)

# compare CV and IC (gamlr) selections on lasso path plot
png(filename=paste(sep="", "BD_Final_Lasso-CV_Criteria_", "STV_STV_8_C", ".png"), 
    width = 960, height = 480, units = "px")
par(mfrow=c(1,1))
LL=log(reg_gamlr$lambda)
plot(reg_gamlr, col="grey", main="Path Plots with Selection Criteria -  STV_STV_8_C")
abline(v=LL[which.min(AICc(reg_gamlr))], col="black", lty=2)
abline(v=LL[which.min(AIC(reg_gamlr))], col="orange", lty=2)
abline(v=LL[which.min(BIC(reg_gamlr))], col="green", lty=2)
abline(v=log(cv_reg$lambda.min), col="blue", lty=2)
abline(v=log(cv_reg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, cex=0.75, pt.cex = 1,
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV min","CV 1se"))
dev.off()


#### STV_STV_32_C ####
type_weekly_reg_basic32 = naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_32_C",]

# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic32$week <- as.numeric(levels(type_weekly_reg_basic32$week))[type_weekly_reg_basic32$week]
type_weekly_reg_basic32$Inv_year <- as.numeric(levels(type_weekly_reg_basic32$Inv_year))[type_weekly_reg_basic32$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic32$week[1]
type_weekly_reg_basic32$week <- type_weekly_reg_basic32$week + (type_weekly_reg_basic32$Inv_year - 2013) * 52 - firstweek

# create X-variable model matrix for gamlr
type_reg_x <- model.matrix(Sec_unit_qty_c ~ USD_unit_price + week 
                           + S.P_500 + S.P_Industrials + Consumer_Electronics,
                           data=type_weekly_reg_basic32)[,-1]
# Run regression
reg_gamlr <- gamlr(type_reg_x, type_weekly_reg_basic32$Sec_unit_qty_c, 
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(reg_gamlr)
coef(reg_gamlr)

# AICc, AIC and BIC
log(reg_gamlr$lambda[which.min(AICc(reg_gamlr))])
log(reg_gamlr$lambda[which.min(AIC(reg_gamlr))])
log(reg_gamlr$lambda[which.min(BIC(reg_gamlr))])

# cross-validation lasso regression - use this output to predict out-of-sample data
cv_reg <- cv.gamlr(type_reg_x, type_weekly_reg_basic32$Sec_unit_qty_c,
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(cv_reg)
coef(cv_reg) ##1 standard error selection
coef(cv_reg, select="min")
log(cv_reg$lambda.min)
log(cv_reg$lambda.1se)

# compare CV and IC (gamlr) selections on lasso path plot
png(filename=paste(sep="", "BD_Final_Lasso-CV_Criteria_", "STV_STV_32_C", ".png"), 
    width = 960, height = 480, units = "px")
par(mfrow=c(1,1))
LL=log(reg_gamlr$lambda)
plot(reg_gamlr, col="grey", main="Path Plots with Selection Criteria -  STV_STV_32_C")
abline(v=LL[which.min(AICc(reg_gamlr))], col="black", lty=2)
abline(v=LL[which.min(AIC(reg_gamlr))], col="orange", lty=2)
abline(v=LL[which.min(BIC(reg_gamlr))], col="green", lty=2)
abline(v=log(cv_reg$lambda.min), col="blue", lty=2)
abline(v=log(cv_reg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, cex=0.75, pt.cex = 1,
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV min","CV 1se"))
dev.off()


#### STV_STV_24_X ####
type_weekly_reg_basic33 = naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_24_X",]

# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic33$week <- as.numeric(levels(type_weekly_reg_basic33$week))[type_weekly_reg_basic33$week]
type_weekly_reg_basic33$Inv_year <- as.numeric(levels(type_weekly_reg_basic33$Inv_year))[type_weekly_reg_basic33$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic33$week[1]
type_weekly_reg_basic33$week <- type_weekly_reg_basic33$week + (type_weekly_reg_basic33$Inv_year - 2013) * 52 - firstweek

# create X-variable model matrix for gamlr
type_reg_x <- model.matrix(Sec_unit_qty_c ~ USD_unit_price + week 
                           + S.P_500 + S.P_Industrials + Consumer_Electronics,
                           data=type_weekly_reg_basic33)[,-1]
# Run regression
reg_gamlr <- gamlr(type_reg_x, type_weekly_reg_basic33$Sec_unit_qty_c, 
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(reg_gamlr)
coef(reg_gamlr)

# AICc, AIC and BIC
log(reg_gamlr$lambda[which.min(AICc(reg_gamlr))])
log(reg_gamlr$lambda[which.min(AIC(reg_gamlr))])
log(reg_gamlr$lambda[which.min(BIC(reg_gamlr))])

# cross-validation lasso regression - use this output to predict out-of-sample data
cv_reg <- cv.gamlr(type_reg_x, type_weekly_reg_basic33$Sec_unit_qty_c,
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(cv_reg)
coef(cv_reg) ##1 standard error selection
coef(cv_reg, select="min")
log(cv_reg$lambda.min)
log(cv_reg$lambda.1se)

# compare CV and IC (gamlr) selections on lasso path plot
png(filename=paste(sep="", "BD_Final_Lasso-CV_Criteria_", "STV_STV_24_X", ".png"), 
    width = 960, height = 480, units = "px")
par(mfrow=c(1,1))
LL=log(reg_gamlr$lambda)
plot(reg_gamlr, col="grey", main="Path Plots with Selection Criteria -  STV_STV_24_X")
abline(v=LL[which.min(AICc(reg_gamlr))], col="black", lty=2)
abline(v=LL[which.min(AIC(reg_gamlr))], col="orange", lty=2)
abline(v=LL[which.min(BIC(reg_gamlr))], col="green", lty=2)
abline(v=log(cv_reg$lambda.min), col="blue", lty=2)
abline(v=log(cv_reg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, cex=0.75, pt.cex = 1,
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV min","CV 1se"))
dev.off()


#### STV_STV_32_X ####
type_weekly_reg_basic34 = naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_32_X",]

# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic34$week <- as.numeric(levels(type_weekly_reg_basic34$week))[type_weekly_reg_basic34$week]
type_weekly_reg_basic34$Inv_year <- as.numeric(levels(type_weekly_reg_basic34$Inv_year))[type_weekly_reg_basic34$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic34$week[1]
type_weekly_reg_basic34$week <- type_weekly_reg_basic34$week + (type_weekly_reg_basic34$Inv_year - 2013) * 52 - firstweek

# create X-variable model matrix for gamlr
type_reg_x <- model.matrix(Sec_unit_qty_c ~ USD_unit_price + week 
                           + S.P_500 + S.P_Industrials + Consumer_Electronics,
                           data=type_weekly_reg_basic34)[,-1]
# Run regression
reg_gamlr <- gamlr(type_reg_x, type_weekly_reg_basic34$Sec_unit_qty_c, 
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(reg_gamlr)
coef(reg_gamlr)

# AICc, AIC and BIC
log(reg_gamlr$lambda[which.min(AICc(reg_gamlr))])
log(reg_gamlr$lambda[which.min(AIC(reg_gamlr))])
log(reg_gamlr$lambda[which.min(BIC(reg_gamlr))])

# cross-validation lasso regression - use this output to predict out-of-sample data
cv_reg <- cv.gamlr(type_reg_x, type_weekly_reg_basic34$Sec_unit_qty_c,
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(cv_reg)
coef(cv_reg) ##1 standard error selection
coef(cv_reg, select="min")
log(cv_reg$lambda.min)
log(cv_reg$lambda.1se)

# compare CV and IC (gamlr) selections on lasso path plot
png(filename=paste(sep="", "BD_Final_Lasso-CV_Criteria_", "STV_STV_32_X", ".png"), 
    width = 960, height = 480, units = "px")
par(mfrow=c(1,1))
LL=log(reg_gamlr$lambda)
plot(reg_gamlr, col="grey", main="Path Plots with Selection Criteria -  STV_STV_32_X")
abline(v=LL[which.min(AICc(reg_gamlr))], col="black", lty=2)
abline(v=LL[which.min(AIC(reg_gamlr))], col="orange", lty=2)
abline(v=LL[which.min(BIC(reg_gamlr))], col="green", lty=2)
abline(v=log(cv_reg$lambda.min), col="blue", lty=2)
abline(v=log(cv_reg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, cex=0.75, pt.cex = 1,
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV min","CV 1se"))
dev.off()


#### STV_STV_44_D ####
type_weekly_reg_basic35 = naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_44_D",]

# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic35$week <- as.numeric(levels(type_weekly_reg_basic35$week))[type_weekly_reg_basic35$week]
type_weekly_reg_basic35$Inv_year <- as.numeric(levels(type_weekly_reg_basic35$Inv_year))[type_weekly_reg_basic35$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic35$week[1]
type_weekly_reg_basic35$week <- type_weekly_reg_basic35$week + (type_weekly_reg_basic35$Inv_year - 2013) * 52 - firstweek

# create X-variable model matrix for gamlr
type_reg_x <- model.matrix(Sec_unit_qty_c ~ USD_unit_price + week 
                           + S.P_500 + S.P_Industrials + Consumer_Electronics,
                           data=type_weekly_reg_basic35)[,-1]
# Run regression
reg_gamlr <- gamlr(type_reg_x, type_weekly_reg_basic35$Sec_unit_qty_c, 
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(reg_gamlr)
coef(reg_gamlr)

# AICc, AIC and BIC
log(reg_gamlr$lambda[which.min(AICc(reg_gamlr))])
log(reg_gamlr$lambda[which.min(AIC(reg_gamlr))])
log(reg_gamlr$lambda[which.min(BIC(reg_gamlr))])

# cross-validation lasso regression - use this output to predict out-of-sample data
cv_reg <- cv.gamlr(type_reg_x, type_weekly_reg_basic35$Sec_unit_qty_c,
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(cv_reg)
coef(cv_reg) ##1 standard error selection
coef(cv_reg, select="min")
log(cv_reg$lambda.min)
log(cv_reg$lambda.1se)

# compare CV and IC (gamlr) selections on lasso path plot
png(filename=paste(sep="", "BD_Final_Lasso-CV_Criteria_", "STV_STV_44_D", ".png"), 
    width = 960, height = 480, units = "px")
par(mfrow=c(1,1))
LL=log(reg_gamlr$lambda)
plot(reg_gamlr, col="grey", main="Path Plots with Selection Criteria -  STV_STV_44_D")
abline(v=LL[which.min(AICc(reg_gamlr))], col="black", lty=2)
abline(v=LL[which.min(AIC(reg_gamlr))], col="orange", lty=2)
abline(v=LL[which.min(BIC(reg_gamlr))], col="green", lty=2)
abline(v=log(cv_reg$lambda.min), col="blue", lty=2)
abline(v=log(cv_reg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, cex=0.75, pt.cex = 1,
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV min","CV 1se"))
dev.off()


#### STV_STV_16_BB ####
type_weekly_reg_basic36 = naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_16_BB",]

# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic36$week <- as.numeric(levels(type_weekly_reg_basic36$week))[type_weekly_reg_basic36$week]
type_weekly_reg_basic36$Inv_year <- as.numeric(levels(type_weekly_reg_basic36$Inv_year))[type_weekly_reg_basic36$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic36$week[1]
type_weekly_reg_basic36$week <- type_weekly_reg_basic36$week + (type_weekly_reg_basic36$Inv_year - 2013) * 52 - firstweek

# create X-variable model matrix for gamlr
type_reg_x <- model.matrix(Sec_unit_qty_c ~ USD_unit_price + week 
                           + S.P_500 + S.P_Industrials + Consumer_Electronics,
                           data=type_weekly_reg_basic36)[,-1]
# Run regression
reg_gamlr <- gamlr(type_reg_x, type_weekly_reg_basic36$Sec_unit_qty_c, 
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(reg_gamlr)
coef(reg_gamlr)

# AICc, AIC and BIC
log(reg_gamlr$lambda[which.min(AICc(reg_gamlr))])
log(reg_gamlr$lambda[which.min(AIC(reg_gamlr))])
log(reg_gamlr$lambda[which.min(BIC(reg_gamlr))])

# cross-validation lasso regression - use this output to predict out-of-sample data
cv_reg <- cv.gamlr(type_reg_x, type_weekly_reg_basic36$Sec_unit_qty_c,
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(cv_reg)
coef(cv_reg) ##1 standard error selection
coef(cv_reg, select="min")
log(cv_reg$lambda.min)
log(cv_reg$lambda.1se)

# compare CV and IC (gamlr) selections on lasso path plot
png(filename=paste(sep="", "BD_Final_Lasso-CV_Criteria_", "STV_STV_16_BB", ".png"), 
    width = 960, height = 480, units = "px")
par(mfrow=c(1,1))
LL=log(reg_gamlr$lambda)
plot(reg_gamlr, col="grey", main="Path Plots with Selection Criteria - STV_STV_16_BB")
abline(v=LL[which.min(AICc(reg_gamlr))], col="black", lty=2)
abline(v=LL[which.min(AIC(reg_gamlr))], col="orange", lty=2)
abline(v=LL[which.min(BIC(reg_gamlr))], col="green", lty=2)
abline(v=log(cv_reg$lambda.min), col="blue", lty=2)
abline(v=log(cv_reg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, cex=0.75, pt.cex = 1,
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV min","CV 1se"))
dev.off()


#### STV_STV_44_X ####
type_weekly_reg_basic37 = naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_44_X",]

# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic37$week <- as.numeric(levels(type_weekly_reg_basic37$week))[type_weekly_reg_basic37$week]
type_weekly_reg_basic37$Inv_year <- as.numeric(levels(type_weekly_reg_basic37$Inv_year))[type_weekly_reg_basic37$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic37$week[1]
type_weekly_reg_basic37$week <- type_weekly_reg_basic37$week + (type_weekly_reg_basic37$Inv_year - 2013) * 52 - firstweek

# create X-variable model matrix for gamlr
type_reg_x <- model.matrix(Sec_unit_qty_c ~ USD_unit_price + week 
                           + S.P_500 + S.P_Industrials + Consumer_Electronics,
                           data=type_weekly_reg_basic37)[,-1]
# Run regression
reg_gamlr <- gamlr(type_reg_x, type_weekly_reg_basic37$Sec_unit_qty_c, 
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(reg_gamlr)
coef(reg_gamlr)

# AICc, AIC and BIC
log(reg_gamlr$lambda[which.min(AICc(reg_gamlr))])
log(reg_gamlr$lambda[which.min(AIC(reg_gamlr))])
log(reg_gamlr$lambda[which.min(BIC(reg_gamlr))])

# cross-validation lasso regression - use this output to predict out-of-sample data
cv_reg <- cv.gamlr(type_reg_x, type_weekly_reg_basic37$Sec_unit_qty_c,
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(cv_reg)
coef(cv_reg) ##1 standard error selection
coef(cv_reg, select="min")
log(cv_reg$lambda.min)
log(cv_reg$lambda.1se)

# compare CV and IC (gamlr) selections on lasso path plot
png(filename=paste(sep="", "BD_Final_Lasso-CV_Criteria_", "STV_STV_44_X", ".png"), 
    width = 960, height = 480, units = "px")
par(mfrow=c(1,1))
LL=log(reg_gamlr$lambda)
plot(reg_gamlr, col="grey", main="Path Plots with Selection Criteria -  STV_STV_44_X")
abline(v=LL[which.min(AICc(reg_gamlr))], col="black", lty=2)
abline(v=LL[which.min(AIC(reg_gamlr))], col="orange", lty=2)
abline(v=LL[which.min(BIC(reg_gamlr))], col="green", lty=2)
abline(v=log(cv_reg$lambda.min), col="blue", lty=2)
abline(v=log(cv_reg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, cex=0.75, pt.cex = 1,
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV min","CV 1se"))
dev.off()


#### STV_STV_16_BC ####
type_weekly_reg_basic38 = naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_16_BC",]

# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic38$week <- as.numeric(levels(type_weekly_reg_basic38$week))[type_weekly_reg_basic38$week]
type_weekly_reg_basic38$Inv_year <- as.numeric(levels(type_weekly_reg_basic38$Inv_year))[type_weekly_reg_basic38$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic38$week[1]
type_weekly_reg_basic38$week <- type_weekly_reg_basic38$week + (type_weekly_reg_basic38$Inv_year - 2013) * 52 - firstweek

# create X-variable model matrix for gamlr
type_reg_x <- model.matrix(Sec_unit_qty_c ~ USD_unit_price + week 
                           + S.P_500 + S.P_Industrials + Consumer_Electronics,
                           data=type_weekly_reg_basic38)[,-1]
# Run regression
reg_gamlr <- gamlr(type_reg_x, type_weekly_reg_basic38$Sec_unit_qty_c, 
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(reg_gamlr)
coef(reg_gamlr)

# AICc, AIC and BIC
log(reg_gamlr$lambda[which.min(AICc(reg_gamlr))])
log(reg_gamlr$lambda[which.min(AIC(reg_gamlr))])
log(reg_gamlr$lambda[which.min(BIC(reg_gamlr))])

# cross-validation lasso regression - use this output to predict out-of-sample data
cv_reg <- cv.gamlr(type_reg_x, type_weekly_reg_basic38$Sec_unit_qty_c,
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(cv_reg)
coef(cv_reg) ##1 standard error selection
coef(cv_reg, select="min")
log(cv_reg$lambda.min)
log(cv_reg$lambda.1se)

# compare CV and IC (gamlr) selections on lasso path plot
png(filename=paste(sep="", "BD_Final_Lasso-CV_Criteria_", "STV_STV_16_BC", ".png"), 
    width = 960, height = 480, units = "px")
par(mfrow=c(1,1))
LL=log(reg_gamlr$lambda)
plot(reg_gamlr, col="grey", main="Path Plots with Selection Criteria -  STV_STV_16_BC")
abline(v=LL[which.min(AICc(reg_gamlr))], col="black", lty=2)
abline(v=LL[which.min(AIC(reg_gamlr))], col="orange", lty=2)
abline(v=LL[which.min(BIC(reg_gamlr))], col="green", lty=2)
abline(v=log(cv_reg$lambda.min), col="blue", lty=2)
abline(v=log(cv_reg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, cex=0.75, pt.cex = 1,
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV min","CV 1se"))
dev.off()


#### STV_STV_12_BB ####
type_weekly_reg_basic39 = naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="STV_STV_12_BB",]

# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basic39$week <- as.numeric(levels(type_weekly_reg_basic39$week))[type_weekly_reg_basic39$week]
type_weekly_reg_basic39$Inv_year <- as.numeric(levels(type_weekly_reg_basic39$Inv_year))[type_weekly_reg_basic39$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basic39$week[1]
type_weekly_reg_basic39$week <- type_weekly_reg_basic39$week + (type_weekly_reg_basic39$Inv_year - 2013) * 52 - firstweek

# create X-variable model matrix for gamlr
type_reg_x <- model.matrix(Sec_unit_qty_c ~ USD_unit_price + week 
                           + S.P_500 + S.P_Industrials + Consumer_Electronics,
                           data=type_weekly_reg_basic39)[,-1]
# Run regression
reg_gamlr <- gamlr(type_reg_x, type_weekly_reg_basic39$Sec_unit_qty_c, 
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(reg_gamlr)
coef(reg_gamlr)

# AICc, AIC and BIC
log(reg_gamlr$lambda[which.min(AICc(reg_gamlr))])
log(reg_gamlr$lambda[which.min(AIC(reg_gamlr))])
log(reg_gamlr$lambda[which.min(BIC(reg_gamlr))])

# cross-validation lasso regression - use this output to predict out-of-sample data
cv_reg <- cv.gamlr(type_reg_x, type_weekly_reg_basic39$Sec_unit_qty_c,
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(cv_reg)
coef(cv_reg) ##1 standard error selection
coef(cv_reg, select="min")
log(cv_reg$lambda.min)
log(cv_reg$lambda.1se)

# compare CV and IC (gamlr) selections on lasso path plot
png(filename=paste(sep="", "BD_Final_Lasso-CV_Criteria_", "STV_STV_12_BB", ".png"), 
    width = 960, height = 480, units = "px")
par(mfrow=c(1,1))
LL=log(reg_gamlr$lambda)
plot(reg_gamlr, col="grey", main="Path Plots with Selection Criteria -  STV_STV_12_BB")
abline(v=LL[which.min(AICc(reg_gamlr))], col="black", lty=2)
abline(v=LL[which.min(AIC(reg_gamlr))], col="orange", lty=2)
abline(v=LL[which.min(BIC(reg_gamlr))], col="green", lty=2)
abline(v=log(cv_reg$lambda.min), col="blue", lty=2)
abline(v=log(cv_reg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, cex=0.75, pt.cex = 1,
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV min","CV 1se"))
dev.off()


#### ADP_ADP_44_X ####
type_weekly_reg_basci40 = naref(type_weekly_reg_basic)[type_weekly_reg_basic$process_type_size_mat=="ADP_ADP_44_X",]

# create weekly numeric index (e.g., week 53, week 54, etc)
# make week and year numeric variables
type_weekly_reg_basci40$week <- as.numeric(levels(type_weekly_reg_basci40$week))[type_weekly_reg_basci40$week]
type_weekly_reg_basci40$Inv_year <- as.numeric(levels(type_weekly_reg_basci40$Inv_year))[type_weekly_reg_basci40$Inv_year]
# set first week as week 0
firstweek <- type_weekly_reg_basci40$week[1]
type_weekly_reg_basci40$week <- type_weekly_reg_basci40$week + (type_weekly_reg_basci40$Inv_year - 2013) * 52 - firstweek

# create X-variable model matrix for gamlr
type_reg_x <- model.matrix(Sec_unit_qty_c ~ USD_unit_price + week 
                           + S.P_500 + S.P_Industrials + Consumer_Electronics,
                           data=type_weekly_reg_basci40)[,-1]
# Run regression
reg_gamlr <- gamlr(type_reg_x, type_weekly_reg_basci40$Sec_unit_qty_c, 
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(reg_gamlr)
coef(reg_gamlr)

# AICc, AIC and BIC
log(reg_gamlr$lambda[which.min(AICc(reg_gamlr))])
log(reg_gamlr$lambda[which.min(AIC(reg_gamlr))])
log(reg_gamlr$lambda[which.min(BIC(reg_gamlr))])

# cross-validation lasso regression - use this output to predict out-of-sample data
cv_reg <- cv.gamlr(type_reg_x, type_weekly_reg_basci40$Sec_unit_qty_c,
                   verb=TRUE, lambda.min.ratio=0.0001)
summary(cv_reg)
coef(cv_reg) ##1 standard error selection
coef(cv_reg, select="min")
log(cv_reg$lambda.min)
log(cv_reg$lambda.1se)

# compare CV and IC (gamlr) selections on lasso path plot
png(filename=paste(sep="", "BD_Final_Lasso-CV_Criteria_", "ADP_ADP_44_X", ".png"), 
    width = 960, height = 480, units = "px")
par(mfrow=c(1,1))
LL=log(reg_gamlr$lambda)
plot(reg_gamlr, col="grey", main="Path Plots with Selection Criteria -  ADP_ADP_44_X")
abline(v=LL[which.min(AICc(reg_gamlr))], col="black", lty=2)
abline(v=LL[which.min(AIC(reg_gamlr))], col="orange", lty=2)
abline(v=LL[which.min(BIC(reg_gamlr))], col="green", lty=2)
abline(v=log(cv_reg$lambda.min), col="blue", lty=2)
abline(v=log(cv_reg$lambda.1se), col="purple", lty=2)
legend("topright", bty="n", lwd=1, cex=0.75, pt.cex = 1,
       col=c("black","orange","green","blue","purple"),
       legend=c("AICc","AIC","BIC","CV min","CV 1se"))
dev.off()




#######################################################################
#### Treatments ####
#######################################################################

#set n to larger number to operate on only a subset of the data
n = 10

subset = sample(length(data2[,1]), length(data2[,1])/n)
treatdata = data2[subset,]
yname="Sec_unit_qty"

#drop a bunch of stuff with lots of factors, or else treatment stuff blows up
y <- treatdata[,grep(yname, colnames(treatdata))]
x_full <- treatdata[, -grep(yname, colnames(treatdata))]
x_full <- x_full[, -grep("Inv_number", colnames(x_full))]
x_full <- x_full[, -grep("Item", colnames(x_full))]
x_full <- x_full[, -grep("WRKCTRID", colnames(x_full))]
x_full <- x_full[, -grep("Inv_date", colnames(x_full))]
x_full <- x_full[, -grep("Inv_city", colnames(x_full))]
x_full <- x_full[, -grep("Inv_address_name", colnames(x_full))]
x_full <- x_full[, -grep("Sales_group", colnames(x_full))]

# run treatments for various variables (only works for numeric variables)
for(treatname in c("Exrate", "Tenure", "S.P_Industrials", "Consumer_Electronics")) {
  x <- sparse.model.matrix(y ~ ., data=x_full[, -grep(treatname, colnames(x_full))])
  treat <- treatdata[,treatname]
  if(is.factor(treat)) {
  } else {
    treatreg <- gamlr(x, treat, lmr=1e-06, verb=TRUE, family="gaussian")
    
    r2 = summary(treatreg)[which.min(AICc(treatreg)),]["r2"]
    that = predict(treatreg, x, type="response")
    png(filename=paste(sep="", "BD_Final_treatment_", treatname, ".png"), width = 960, height = 480, units = "px")
    plot(that,treat,bty="n",pch=21,bg=8) 
    dev.off()
    
    causal <- gamlr(cBind(treat,that,x), y, lmr=1e-04, verb=TRUE, family="gaussian", free=2)
    Baicc <- coef(treatreg)
    
    print(treatname)
    print(r2)
    print(coef(causal)["treat",])
    print("")
    
  }
}




#######################################################################
##### Tree and Random Forest Analysis #####
#######################################################################

#### Analysis to predict volume ####
# train tree on 2014 data
type_reg_basic$Size_ID = as.factor(type_reg_basic$Size_ID) ## Factor Size_ID
type_reg_basic1 <- type_reg_basic[type_reg_basic$Inv_year==2014,]

y2014 <- type_reg_basic1$Sec_unit_qty_c ## find values for demand
x2014 <- model.matrix(~ Product_type + Material_type + Size_ID
                   + Process_type + Subprocess_type, 
                   data = type_reg_basic1)[,-1] ## create dummy variables for Product & Material types

x_2014 = data.frame(Inv_year=type_reg_basic1$Inv_year, Inv_month=type_reg_basic1$Inv_month, x2014,
                    S.P_500=type_reg_basic1$S.P_500, S.P_Industrials=type_reg_basic1$S.P_Industrials,
                    Consumer_Electronics=type_reg_basic1$Consumer_Electronics, USD_extended_price=type_reg_basic1$USD_extended_price, 
                    USD_total_extended_cost=type_reg_basic1$USD_total_extended_cost, USD_unit_price=type_reg_basic1$USD_unit_price, 
                    USD_unit_cost=type_reg_basic1$USD_unit_cost,USD_unit_margin_percent=type_reg_basic1$USD_unit_margin_percent)
x_2014 <- reverse_naref(x_2014) ## replace NAs with "NA" so new data matches training set

vol_tree <- tree( y2014 ~., data= x_2014,  
                  mincut=100, mindev=0.0001)
print(vol_tree)
summary(vol_tree)
plot(vol_tree)
plot(vol_tree, type="uniform")
text(vol_tree, cex=.55)

# prune the tree
cv_vol_tree <- cv.tree(vol_tree, K=100)
cv_vol_tree$size
cv_vol_tree$dev
plot(cv_vol_tree, pch=21, bg=8, type="p", cex=1.5)
vol_tree_cut <- prune.tree(vol_tree, best = 8)
plot(vol_tree_cut, col=8, type="uniform")
text(vol_tree_cut, cex=0.5)

# create random forest
vol_tree_rf <- randomForest( y=y2014, x=x_2014,  
                             mincut=100, mindev=0.0001,
                             ntree=500, importance=TRUE)
varImpPlot(vol_tree_rf,  type=1, pch=21, bg="navy", main='RF variable importance', cex=0.5)

# create 2015 data set to test
type_reg_basic$Size_ID = as.factor(type_reg_basic$Size_ID) ## Factor Size_ID
type_reg_basic2 <- type_reg_basic[type_reg_basic$Inv_year==2015,]
y2015 <- type_reg_basic2$Sec_unit_qty_c ## find values for demand
x2015 <- model.matrix(~ Product_type + Material_type + Size_ID
                      + Process_type + Subprocess_type, 
                      data = type_reg_basic2)[,-1] ## create dummy variables for Product & Material types
x_2015 = data.frame(Inv_year=type_reg_basic2$Inv_year, Inv_month=type_reg_basic2$Inv_month, x2015,
                    S.P_500=type_reg_basic2$S.P_500, S.P_Industrials=type_reg_basic2$S.P_Industrials,
                    Consumer_Electronics=type_reg_basic2$Consumer_Electronics, USD_extended_price=type_reg_basic2$USD_extended_price, 
                    USD_total_extended_cost=type_reg_basic2$USD_total_extended_cost, USD_unit_price=type_reg_basic2$USD_unit_price, 
                    USD_unit_cost=type_reg_basic2$USD_unit_cost, USD_unit_margin_percent=type_reg_basic2$USD_unit_margin_percent)
x_2015 <- reverse_naref(x_2015) ## replace NAs with "NA" so new data matches training set

# predictions with the Tree
vol_tree_predict <- predict(vol_tree_cut, x_2015)
plot(vol_tree_predict, y2015, main="2015 Actual vs. Tree Predicted Volume",
     xlab="Tree Predicted Monthly Log Volume", ylab="Actual Monthly Log Volume")
sum((exp(vol_tree_predict) - exp(y2015))^2) 
sum(abs(exp(vol_tree_predict) - exp(y2015)))

# predictions with the random forest
vol_tree_class <- predict(vol_tree_rf, x_2015)
plot(vol_tree_class, y2015, main="2015 Actual vs. Random Forest Predicted Volume",
     xlab="Random Forest Predicted Monthly Log Volume", ylab="Actual Monthly Log Volume")
sum((exp(vol_tree_class) - exp(y2015))^2) 
sum(abs(exp(vol_tree_class) - exp(y2015)))






#### Analysis to predict size ####
# train tree on 2014 data
type_reg_basic$Size_ID = as.factor(type_reg_basic$Size_ID) ## Factor Size_ID
type_reg_basic3 <- type_reg_basic[type_reg_basic$Inv_year==2014,]

y2014_2 <- droplevels(type_reg_basic3$Size_ID) ## find values for size
x2014_2 <- model.matrix(~ Product_type + Material_type 
                      + Process_type + Subprocess_type, 
                      data = type_reg_basic3)[,-1] ## create dummy variables for Product & Material types

x_2014_2 = data.frame(Inv_year=type_reg_basic3$Inv_year, Inv_month=type_reg_basic3$Inv_month, x2014_2,
                      S.P_500=type_reg_basic3$S.P_500, S.P_Industrials=type_reg_basic3$S.P_Industrials,
                      Consumer_Electronics=type_reg_basic3$Consumer_Electronics, USD_extended_price=type_reg_basic3$USD_extended_price, 
                      USD_total_extended_cost=type_reg_basic3$USD_total_extended_cost, USD_unit_price=type_reg_basic3$USD_unit_price, 
                      USD_unit_cost=type_reg_basic3$USD_unit_cost,USD_unit_margin_percent=type_reg_basic3$USD_unit_margin_percent)
x_2014_2 <- reverse_naref(x_2014_2) ## replace NAs with "NA" so new data matches training set

size_tree <- tree( y2014_2 ~., data= x_2014_2,  
                  mincut=100, mindev=0.0001)
print(size_tree)
summary(size_tree)
plot(size_tree)
plot(size_tree, type="uniform")
text(size_tree, cex=.5)

# prune the tree
cv_size_tree <- cv.tree(size_tree, K=100)
cv_size_tree$size
cv_size_tree$dev
plot(cv_size_tree, pch=21, bg=8, type="p", cex=1.5)
size_tree_cut <- prune.tree(size_tree, best = 8)
plot(size_tree_cut, col=8, type="uniform")
text(size_tree_cut, cex=0.6)

# create random forest
size_tree_rf <- randomForest( y=y2014_2, x=x_2014_2, 
                             mincut=100, mindev=0.0001,
                             ntree=500, importance=TRUE)
varImpPlot(size_tree_rf,  type=1, pch=21, bg="navy", main='RF variable importance', cex=0.5)


# create 2015 data set to test
type_reg_basic$Size_ID = as.factor(type_reg_basic$Size_ID) ## Factor Size_ID
type_reg_basic4 <- type_reg_basic[type_reg_basic$Inv_year==2015,]
y2015_2 <- droplevels(type_reg_basic4$Size_ID) ## find values for size
x2015_2 <- model.matrix(~ Product_type + Material_type 
                      + Process_type + Subprocess_type, 
                      data = type_reg_basic4)[,-1] ## create dummy variables for Product & Material types
x_2015_2 = data.frame(Inv_year=type_reg_basic4$Inv_year, Inv_month=type_reg_basic4$Inv_month, x2015_2,
                      S.P_500=type_reg_basic4$S.P_500, S.P_Industrials=type_reg_basic4$S.P_Industrials,
                      Consumer_Electronics=type_reg_basic4$Consumer_Electronics, USD_extended_price=type_reg_basic4$USD_extended_price, 
                      USD_total_extended_cost=type_reg_basic4$USD_total_extended_cost, USD_unit_price=type_reg_basic4$USD_unit_price, 
                      USD_unit_cost=type_reg_basic4$USD_unit_cost, USD_unit_margin_percent=type_reg_basic4$USD_unit_margin_percent)
x_2015_2 <- reverse_naref(x_2015_2) ## replace NAs with "NA" so new data matches training set



# predictions with the Tree - not functioning due to different levels between y2014_2 and y2014_2
    #size_tree_predict <- predict(size_tree_cut, x_2015_2)
    #plot(size_tree_predict, y2015_2, main="2015 Actual vs. Tree Predicted Size",
    #     xlab="Tree Predicted Size", ylab="Actual Size")
    #sum((exp(size_tree_predict) - exp(y2015_2))^2) 
    #sum(abs(exp(size_tree_predict) - exp(y2015_2)))

# predictions with the random forest
size_tree_class <- predict(size_tree_rf, x_2015_2)
plot(size_tree_class, y2015_2, main="2015 Actual vs. Random Forest Predicted Size",
     xlab="Random Forest Predicted Size", ylab="Actual Size")
sum((exp(size_tree_class) - exp(y2015_2))^2) 
sum(abs(exp(size_tree_class) - exp(y2015_2)))