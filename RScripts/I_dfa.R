#Windows
Dropbox = "C:\\users/Jake/Dropbox"
#Mac/Unix
Dropbox = "~/Dropbox"
path= "/Booth/Winter 2014/Investments/DFA Case/"

setwd(paste(Dropbox, path, sep=""))

ffdata = read.csv("FFData.csv")
ffdata$n80s = (ffdata$Date > 198000 & ffdata$Date < 199000)
ffdata$n90s = (ffdata$Date > 199000 & ffdata$Date < 200000)
ffdata$post80s = (ffdata$Date > 198000)
ffdata$post90s = (ffdata$Date > 199000)
ffdata$index = 1:942
summary(ffdata)

reg1 = lm(SMB ~ index * (n80s + n90s), data=ffdata)
reg2 = lm(SMB ~ index * post80s, data=ffdata)
reg3 = lm(HML ~ index * (n80s + n90s), data=ffdata)
reg4 = lm(HML ~ index * post90s, data=ffdata)


mean(ffdata$SMB[600:900])
plot(ffdata$SMB[600:900], type="l")
#abline(mean(ffdata$SMB[600:900]),0, col="red")
lines(predict(reg1)[600:900], col = "red")

mean(ffdata$HML[600:900])
plot(ffdata$HML[600:900], type="l")
#abline(mean(ffdata$SMB[600:900]),0, col="red")
lines(predict(reg3)[600:900], col = "red")

summary(reg1)
summary(reg2)
summary(reg3)
summary(reg4)

