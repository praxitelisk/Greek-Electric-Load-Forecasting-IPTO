#############################################################
####Parsing the xlsx files from to get the loads#############
#############################################################

#library("xlsx")
#library("readxl")

# rm(list = ls())

startTime <- proc.time()[3]

xlsFiles <-
  list.files(
    path = "IPTO",
    pattern = "csv",
    all.files = FALSE,
    full.names = TRUE,
    recursive = FALSE,
    ignore.case = FALSE,
    include.dirs = FALSE,
    no.. = FALSE
  )


test <- vector()
for (i in 1:length(xlsFiles)) {
  
  #for the 2010 csv only, the csv seperator is ";"
  if (i == 1) {
    cat(i, xlsFiles[i], "; \n")
    df = read.csv(xlsFiles[i], sep=";")
    df = as.data.frame(df)
    df = df[,3:5]
    df$DATE = as.character(df$DATE)
    df$HOUR = as.character(df$HOUR)
    time = strsplit(df$DATE, "/")
    day = unlist(lapply(time, '[[', 1))
    month = unlist(lapply(time, '[[', 2))
    year = unlist(lapply(time, '[[', 3))
    date = paste(year, month, day, sep = "/")
    df$DATE = date
    temp = df$SETTLED.ENERGY.SUPPLIED.TO.CUSTOMERS.FOR.ALL.LOAD.REPRESENTATIVES..MWh.
    temp = gsub(pattern = ",", ".", temp)
    temp = as.numeric(temp)
    df$SETTLED.ENERGY.SUPPLIED.TO.CUSTOMERS.FOR.ALL.LOAD.REPRESENTATIVES..MWh. = temp
    
  }
  #for 2011 csv only the csv seperator is "," but csv date is in different order
  else if (i == 2) {
    cat(i, xlsFiles[i], ", \n")
    df = read.csv(xlsFiles[i], sep=",")
    df = as.data.frame(df)
    df = df[,3:5]
    df$DATE = as.character(df$DATE)
    df$HOUR = as.character(df$HOUR)
    time = strsplit(df$DATE, "/")
    day = unlist(lapply(time, '[[', 2))
    month = unlist(lapply(time, '[[', 1))
    year = unlist(lapply(time, '[[', 3))
    date = paste(year, month, day, sep = "/")
    df$DATE = date
    temp = df$SETTLED.ENERGY.SUPPLIED.TO.CUSTOMERS.FOR.ALL.LOAD.REPRESENTATIVES..MWh.
    temp = as.character(temp)
    temp = as.numeric(temp)
    df$SETTLED.ENERGY.SUPPLIED.TO.CUSTOMERS.FOR.ALL.LOAD.REPRESENTATIVES..MWh. = temp
  }
  #for 2012 - .... the rest csv the csv seperator is ","
  else {
    cat(i, xlsFiles[i], ", \n")
    df = read.csv(xlsFiles[i], sep=",")
    df = as.data.frame(df)
    df = df[,3:5]
    df$DATE = as.character(df$DATE)
    df$HOUR = as.character(df$HOUR)
    time = strsplit(df$DATE, "/")
    day = unlist(lapply(time, '[[', 1))
    month = unlist(lapply(time, '[[', 2))
    year = unlist(lapply(time, '[[', 3))
    date = paste(year, month, day, sep = "/")
    df$DATE = date
    temp = df$SETTLED.ENERGY.SUPPLIED.TO.CUSTOMERS.FOR.ALL.LOAD.REPRESENTATIVES..MWh.
    temp = as.character(temp)
    num1 = substr(gsub(",", "", temp), 1, 4)
    num2 = substr(gsub(",", "", temp), 5, nchar(temp))
    temp = as.numeric(paste(num1, num2, sep = "."))
    df$SETTLED.ENERGY.SUPPLIED.TO.CUSTOMERS.FOR.ALL.LOAD.REPRESENTATIVES..MWh. = temp
  }
  
  test <- rbind(test, df)
}


myLoads = test
##myLoads = test[!test$HOUR == 25, ]
##myLoads =  myLoads[complete.cases(myLoads),]

names(myLoads)[3] <- "Loads"
backUp.Loads = myLoads
rm("test", "df", i, temp, day, month, year, time, date, num1, num2)



cat("elapsed time in minutes: ", (proc.time()[3] - startTime) / 60)
rm(startTime)
#elapsed time in minutes:  0.025
