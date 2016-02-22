location = "~/github"
path= "/ml-lab/data/"
setwd(paste(location, path, sep=""))

#Q1
df <- readWorksheetFromFile("AFC_hw_2.xls", 
                            sheet=1, 
                            startRow = 4,
                            endCol = 8)
df <- convert_to_numeric(df)
