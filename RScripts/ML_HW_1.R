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
max = max(data$"Revenue.+.LOS:94")
max
data[(data$"Revenue.+.LOS:94" == max),]

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
sl <- data[,-grep("Booking", colnames(data))]
sl <- data[,-grep("Room Nights", colnames(data))]
y <- data[,grep(yname, colnames(data))]
colnames(y)
colnames(sl)

yframe <- data.frame(y)

sudoanalyze(yframe, sl, debug=TRUE, console=TRUE)
colnames(summary(y))
y$"SPGlevel.P...Revenue"
basic_reg_y <- y$"SPGlevel.P...Revenue"
basic_reg_y
summary(sl)
basic_reg_x <- model.matrix(basic_reg_y ~ ., data=sl)[,-1]
basic_reg_x
reg1 <- gamlr(basic_reg_x, basic_reg_y, verb=TRUE, lambda.min.ratio=0.0001)
summary(reg1)
coef(reg1)