Project = "~/Downloads"
path= "/"
setwd(paste(Project, path, sep=""))

#library(XLConnect)
#library(xlsx)
library("openxlsx")
library(gamlr)

source("~/github/projects/RScripts/logging.R")
source("~/github/projects/RScripts/sudoanalyze.R")

data = read.xlsx("1_KeyMetrics.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
#data = read.xlsx("1_KeyMetrics.xlsx",1)
#data1 = readWorksheet(loadWorkbook("1_KeyMetrics.xlsx"),sheet=1)
#"4_DTAbookings.xlsx", "6_LOSbookings.xlsx", "7_LOSrevenue.xlsx"
for (name in c("2_VisitNumber.xlsx", "3_Brand.xlsx", "5_DTArevenue.xlsx")) {
  data = merge(data, read.xlsx(name, sheet = 1, startRow = 1, colNames = TRUE), by="Item")
}
data <- data[(data$Item != "Total"),]
data <- data[,-grep("Dimension", colnames(data))]
data <- data[,-grep("Item", colnames(data))]

colnames(data)
rownames(data)
summary(data)

data = naref(data)
for(di in colnames(data)) {
  if(is.numeric(data[,di])){
    #bad data!  BAD DATA!
    data[,di][is.na(data[,di])] <- 0
    data[,di][is.nan(data[,di])] <- 0
    data[,di][is.null(data[,di])] <- 0
    data[,di][data[,di]==Inf] <- 0
    data[,di][data[,di]==-Inf] <- 0
  }
}
yname="Revenue"
sl <- data[,-grep(yname, colnames(data))]
sl <- sl[,-grep("Booking", colnames(sl))]
sl <- sl[,-grep("Room", colnames(sl))]
sl <- sl[,-grep("Spent", colnames(sl))]
y <- data[,grep(yname, colnames(data))]
for(di in colnames(y)) {
    y[,di] <- log(y[,di] + 1)
}

colnames(y)
colnames(sl)
summary(sl)

yframe <- data.frame(y)

sudoanalyze(yframe, sl, debug=TRUE, console=TRUE)
colnames(summary(y))
y$"SPGlevel.P...Revenue"
basic_reg_y <- log(y$"SPGlevel.G...Revenue" + 1)
summary(basic_reg_y)
summary(sl)
sl2 <- sl[,grep("\\.G\\.", colnames(sl))]
colnames(sl2)
basic_reg_x <- model.matrix(basic_reg_y ~ ., data=sl2)[,-1]
basic_reg_x
reg1 <- gamlr(basic_reg_x, basic_reg_y, verb=TRUE, lambda.min.ratio=0.0001)
summary(reg1)
coef(reg1)


data2 = read.xlsx("NEW DATA COMPILED.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
data2 = data2[data2$Item != 42130.125,]
data2 = data2[data2$Item != 42131 + 23/24,]
data2 <- data2[,-grep("Item", colnames(data2))]
#data2 <- data2[,-grep("Single.Page.Visits", colnames(data2))]
#data2 <- data2[,-grep("Bookings.+.Bookings", colnames(data2))]
#data2 <- data2[,-grep("Bookings.+.Revenue", colnames(data2))]
colnames(data2)
summary(data2$"Total")
summary(data2$"No.Bookings")
summary(data2$"Bookings")

data2 <- data2[data2$"Bookings" == 0,]
data2 <- data2[data2$"No.Bookings" == 0,]
data2 <- data2[data2$"Total" == 0,]
data2 <- data2[,-grep("^Bookings$", colnames(data2))]
data2 <- data2[,-grep("No.Bookings", colnames(data2))]
data2 <- data2[,-grep("Total", colnames(data2))]
data2 <- data2[,-grep("Visits", colnames(data2))]
data2 <- data2[,-grep("Visitor", colnames(data2))]

summary(data2$"Bookings.1")

yname2="Total:Revenue"
sl3 <- data2[,-grep(yname2, colnames(data2))]
sl3 <- sl3[,-grep("Total:Bookings", colnames(sl3))]
sl3 <- sl3[,-grep("Revenue", colnames(sl3))]
y2 <- data2[,grep(yname2, colnames(data2))]
summary(y2)
hist(y2)

colnames(sl3)
yframe2 <- data.frame(y2)
basic_reg_x2 <- model.matrix(y2 ~ ., data=sl3)[,-1]
reg2 <- gamlr(basic_reg_x2, y2, verb=TRUE, lambda.min.ratio=0.0001)
summary(reg2)
coef(reg2)

yname3="Bookings.1"
sl4 <- data2[,-grep(yname3, colnames(data2))]
sl4 <- sl4[,-grep("Revenue", colnames(sl4))]
y3 <- data2[,grep(yname3, colnames(data2))]

summary(y3)
summary(sl4)
basic_reg_x3 <- model.matrix(y3 ~ .^2, data=sl4)[,-1]
reg3 <- cv.gamlr(basic_reg_x3, y3, verb=TRUE, lambda.min.ratio=0.0001)
summary(reg3)
coef(reg3)

reg4 <- gamlr(basic_reg_x3, y3, verb=TRUE, lambda.min.ratio=0.0001)
summary(reg4)
coef(reg4)
coef(reg4)[coef(reg4)!=0]

B_cv.reg3 <- drop(coef(reg3))
B_cv.reg3[1:200] 
B_cv.reg3_notzero = B_cv.reg3[B_cv.reg3!=0]
length(B_cv.reg3_notzero)
sort(B_cv.reg3_notzero)
sum(B_cv.reg3_notzero)

summary(sl4)
summary(y3)

yname4="Total:Revenue"
sl5 <- data2[,-grep(yname4, colnames(data2))]
sl5 <- sl5[,-grep("Total:", colnames(sl5))]
sl5 <- sl5[,-grep("DTA:2", colnames(sl5))]
sl5 <- sl5[,-grep("DTA:3", colnames(sl5))]
sl5 <- sl5[,-grep("DTA:4", colnames(sl5))]
sl5 <- sl5[,-grep("DTA:5", colnames(sl5))]
sl5 <- sl5[,-grep("DTA:6", colnames(sl5))]
sl5 <- sl5[,-grep("DTA:7", colnames(sl5))]
sl5 <- sl5[,-grep("DTA:8", colnames(sl5))]
sl5 <- sl5[,-grep("DTA:9", colnames(sl5))]
sl5 <- sl5[,-grep("DTA:10", colnames(sl5))]
sl5 <- sl5[,-grep("DTA:11", colnames(sl5))]
sl5 <- sl5[,-grep("DTA:12", colnames(sl5))]
sl5 <- sl5[,-grep("DTA:13", colnames(sl5))]
sl5 <- sl5[,-grep("DTA:14", colnames(sl5))]
sl5 <- sl5[,-grep("Revenue", colnames(sl5))]
colnames(sl5)
y4 <- data2[,grep(yname4, colnames(data2))]
summary(y4)
hist((y4))

basic_reg_x4 <- model.matrix(y4 ~ ., data=sl5)[,-1]
reg4 <- gamlr(basic_reg_x4, y4, verb=TRUE, lambda.min.ratio=0.0001)
summary(reg4)
coef(reg4)



yname5="Total:Bookings"
sl6 <- data2[,-grep(yname5, colnames(data2))]
sl6 <- sl6[,-grep("Total:", colnames(sl6))]
sl6 <- sl6[,-grep("DTA:2", colnames(sl6))]
sl6 <- sl6[,-grep("DTA:3", colnames(sl6))]
sl6 <- sl6[,-grep("DTA:4", colnames(sl6))]
sl6 <- sl6[,-grep("DTA:5", colnames(sl6))]
sl6 <- sl6[,-grep("DTA:6", colnames(sl6))]
sl6 <- sl6[,-grep("DTA:7", colnames(sl6))]
sl6 <- sl6[,-grep("DTA:8", colnames(sl6))]
sl6 <- sl6[,-grep("DTA:9", colnames(sl6))]
sl6 <- sl6[,-grep("DTA:10", colnames(sl6))]
sl6 <- sl6[,-grep("DTA:11", colnames(sl6))]
sl6 <- sl6[,-grep("DTA:12", colnames(sl6))]
sl6 <- sl6[,-grep("DTA:13", colnames(sl6))]
sl6 <- sl6[,-grep("DTA:14", colnames(sl6))]
sl6 <- sl6[,-grep("Booking", colnames(sl6))]
colnames(sl6)
y5 <- data2[,grep(yname5, colnames(data2))]
summary(y5)
hist((y5))

basic_reg_x5 <- model.matrix(y5 ~ ., data=sl6)[,-1]
reg5 <- gamlr(basic_reg_x5, y5, verb=TRUE, lambda.min.ratio=0.0001)
summary(reg5)
coef(reg5)


data2$"2b.Visits:Bookings.+.Bookings" == y4
summary(y4)
summary(data2$"2b.Visits:Bookings.+.Bookings")
sl5[sl5$"2b.Visits:Bookings.+.Bookings" != sl5$"Total:Bookings", ]
sl5
