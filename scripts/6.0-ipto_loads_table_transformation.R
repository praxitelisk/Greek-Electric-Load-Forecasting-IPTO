###there is an issue after introducing March artificial hour on March's last Sunday
### chck it out

library("lubridate")

startTime <- proc.time()[3]

#removing the existing variables
rm(temp.row, temp, finalDataFrame, combinedWeather, combinedLoads, final.Data.Set)

#a small preprocess######
#merge the weather data and the Loads based on the common date entry
cat("#a small preprocess######\n")
darkSky.N.Loads.Combined = 
  merge(darkSky.WeatherData, myLoads, by="time", all.y = TRUE)


# #resolving some missing values that occured during the merge
# na.List = which(is.na(darkSky.N.Loads.Combined$weekday))
# 
# #filling the NA values
# for(i in 1:length(na.List)) {
#   
#   darkSky.N.Loads.Combined$time[na.List[i]] = 
#     darkSky.N.Loads.Combined$time[na.List[i]] + 23 * 60 * 60
#   
#   darkSky.N.Loads.Combined[na.List[i], 2:11] = 
#     darkSky.N.Loads.Combined[(na.List[i]+ 1), 2:11]
#   
#   darkSky.N.Loads.Combined[na.List[i], 12:13] = 
#     darkSky.N.Loads.Combined[(na.List[i] - 1), 12:13]
#   
#   
#   for(k in 14:24) {
#   
#     darkSky.N.Loads.Combined[na.List[i], k] = 
#     mean(c(darkSky.N.Loads.Combined[(na.List[i] + 1), k],
#            darkSky.N.Loads.Combined[(na.List[i] - 1), k]))
#      
#   }
#   
#   darkSky.N.Loads.Combined[na.List[i], 25:26] = 
#     darkSky.N.Loads.Combined[(na.List[i]+ 1), 25:26]
#   
#   
#   for(k in 27:37) {
#     
#     darkSky.N.Loads.Combined[na.List[i], k] = 
#       mean(c(darkSky.N.Loads.Combined[(na.List[i] + 1), k],
#              darkSky.N.Loads.Combined[(na.List[i] - 1), k]))
#     
#   }
#   
# }
# rm(k)
# 
# #resolving March's daylight saving (March's last Sunday) 23 hour day issue####
# cat("#resolving March's daylight saving (March's last Sunday) 23 hour day issue####\n")
# is.zero.list = which(darkSky.N.Loads.Combined$Loads == 0)
# 
# for (i in 1:length(is.zero.list)) {
#   
#   darkSky.N.Loads.Combined$Loads[is.zero.list[i]] = mean(c(
#     darkSky.N.Loads.Combined$Loads[is.zero.list[i] - 1], 
#     darkSky.N.Loads.Combined$Loads[is.zero.list[i] + 1]))
#   
# }

cat("#removing the extra daylight hour from October's last Sunday and create an extra artificial one for March's last Sunday####\n")
x <- seq(
  as.Date(as.POSIXct(min(darkSky.N.Loads.Combined$time), tz= "Europe/Athens"), tz= "Europe/Athens"), 
  as.Date(as.POSIXct(max(darkSky.N.Loads.Combined$time), tz= "Europe/Athens"), tz= "Europe/Athens"),
  by="1 day")


OctoberToBeRemoved = x[wday(x,label = TRUE) == "Sun" & day(x) >= 25 & month(x) == 10]
#MarchToBeAdded = x[wday(x,label = TRUE) == "Sun" & day(x) >= 25 & month(x) == 3]


for(i in 1:length(OctoberToBeRemoved)) {
  
  remove = which(darkSky.N.Loads.Combined$time == paste(OctoberToBeRemoved[i], "03:00:00"))[2]
  
  darkSky.N.Loads.Combined = darkSky.N.Loads.Combined[-remove,]
}


row.names(darkSky.N.Loads.Combined) <- 1:nrow(darkSky.N.Loads.Combined)
addAfterIndexList = c()
MarchToBeAdded = x[wday(x,label = TRUE) == "Sun" & day(x) >= 25 & month(x) == 3]


for(i in 1:length(MarchToBeAdded)) {
  
  
  temp = data.frame(matrix(NA, nrow=1, ncol=length(darkSky.N.Loads.Combined)))
  colnames(temp)= colnames(darkSky.N.Loads.Combined)
  temp$time = as.POSIXct(paste(MarchToBeAdded[i], "04:00:00"), tz="Europe/Athens")
  #temp$DATE[1] = MarchToBeAdded[i]
  #class(temp$DATE) = class(darkSky.N.Loads.Combined$DATE)
  
  addAfterIndex = which(as.character(darkSky.N.Loads.Combined$time) == paste(MarchToBeAdded[i], "02:00:00"))
  addAfterIndexList = c(addAfterIndexList, addAfterIndex)
  
  
  darkSky.N.Loads.Combined = rbind(
    darkSky.N.Loads.Combined[1:addAfterIndex, ], 
    temp, 
    darkSky.N.Loads.Combined[(addAfterIndex+1):nrow(darkSky.N.Loads.Combined), ])
  
  row.names(darkSky.N.Loads.Combined) <- 1:nrow(darkSky.N.Loads.Combined)
}



#row.names(darkSky.N.Loads.Combined) <- 1:nrow(darkSky.N.Loads.Combined)



#filling the NA values for March's last Sunday artificial new hour
addAfterIndexList = addAfterIndexList + 1
for(i in 1:length(addAfterIndexList)) {
  
  # darkSky.N.Loads.Combined$time[na.List[i]] =
  #   darkSky.N.Loads.Combined$time[na.List[i]] + 23 * 60 * 60
  
  darkSky.N.Loads.Combined[addAfterIndexList[i], 2:11] =
    darkSky.N.Loads.Combined[(addAfterIndexList[i]+ 1), 2:11]
  
  darkSky.N.Loads.Combined[addAfterIndexList[i], 12:13] =
    darkSky.N.Loads.Combined[(addAfterIndexList[i] - 1), 12:13]
  
  
  for(k in 14:24) {
    
    darkSky.N.Loads.Combined[addAfterIndexList[i], k] =
      mean(c(darkSky.N.Loads.Combined[(addAfterIndexList[i] + 1), k],
             darkSky.N.Loads.Combined[(addAfterIndexList[i] - 1), k]))
    
  }
  
  darkSky.N.Loads.Combined[addAfterIndexList[i], 25:26] =
    darkSky.N.Loads.Combined[(addAfterIndexList[i]+ 1), 25:26]
  
  
  for(k in 27:37) {
    
    darkSky.N.Loads.Combined[addAfterIndexList[i], k] =
      mean(c(darkSky.N.Loads.Combined[(addAfterIndexList[i] + 1), k],
             darkSky.N.Loads.Combined[(addAfterIndexList[i] - 1), k]))
    
  }
  
  darkSky.N.Loads.Combined[addAfterIndexList[i], ]$DATE = 
    darkSky.N.Loads.Combined[addAfterIndexList[i]-1, ]$DATE
  
  
  darkSky.N.Loads.Combined[addAfterIndexList[i], ]$HOUR = 3
  
  
  darkSky.N.Loads.Combined[addAfterIndexList[i], ]$Loads =
    mean(c(darkSky.N.Loads.Combined[(addAfterIndexList[i] + 1), ]$Loads,
           darkSky.N.Loads.Combined[(addAfterIndexList[i] - 1), ]$Loads))
  
}
rm(k, OctoberToBeRemoved, MarchToBeAdded, addAfterIndex, addAfterIndexList, x, remove, temp)


####add some NA values as a data-column to complete 24-hour days that are missing####
#e.g. the first and the last date from the weather data.frame
#the code below works only for 2010-10-01 00:00 row
temp = darkSky.N.Loads.Combined[1, ]
temp$time = temp$time - 3600
temp$Loads = 0


darkSky.N.Loads.Combined = rbind(temp, darkSky.N.Loads.Combined)
rm(temp)


####change some classes to factors due to a bug from previous lines of code####
darkSky.N.Loads.Combined$athens.summary = 
  factor(darkSky.N.Loads.Combined$athens.summary)

darkSky.N.Loads.Combined$thessaloniki.summary = 
  factor(darkSky.N.Loads.Combined$thessaloniki.summary)

darkSky.N.Loads.Combined$athens.icon = 
  factor(darkSky.N.Loads.Combined$athens.icon)

darkSky.N.Loads.Combined$thessaloniki.icon = 
  factor(darkSky.N.Loads.Combined$thessaloniki.icon)

# darkSky.N.Loads.Combined$isRushHour = 
#   as.factor(darkSky.N.Loads.Combined$isRushHour)
# 
# darkSky.N.Loads.Combined$isWeekend = 
#   as.factor(darkSky.N.Loads.Combined$isWeekend)
# 
# darkSky.N.Loads.Combined$isHoliday = 
#   as.factor(darkSky.N.Loads.Combined$isHoliday)
# 
# darkSky.N.Loads.Combined$day.of.week = 
#   as.factor(darkSky.N.Loads.Combined$day.of.week)


####remove the last element from data.frame in order to have full 24 hours for all days####
darkSky.N.Loads.Combined = 
  darkSky.N.Loads.Combined[1:(dim(darkSky.N.Loads.Combined)[1]-1),]


####converting from hourly to daily data.frame####
cat("convert hourly to daily data.frame####\n")
for (i in seq(1, dim(darkSky.N.Loads.Combined)[1], by = 24)) {
  
  for(j in 0:23) {
    
    temp = darkSky.N.Loads.Combined[i+j, ]
    colnames(temp) = paste(colnames(temp), (i+j-1)%%24, sep=".")
    
    if (!exists("temp.row")) {
      temp.row = temp
    }
    else {
      temp.row = cbind(temp.row, temp)
    }
    
  }
  
  
  if (!exists("finalDataFrame")) {
    finalDataFrame = temp.row
  }
  else {
    finalDataFrame = rbind(finalDataFrame, temp.row)
  }
  
  rm(temp.row, temp)
  
}


#exclude the loads and some other useless informations####
combinedWeather = finalDataFrame[-grep("Loads", colnames(finalDataFrame))]
combinedWeather = combinedWeather[-grep("DATE", colnames(combinedWeather))]
combinedWeather = combinedWeather[-grep("HOUR", colnames(combinedWeather))]

#fetch again the loads to put them in the end of the data.frame####
combinedLoads = finalDataFrame[grep("Loads", colnames(finalDataFrame))]

#create the final data.frame####
finalDataFrame = cbind(combinedWeather, combinedLoads)


####removing some redundant variables in finalDataFrame####
removeColumn = paste("time", 1:24, sep=".")
for(i in 1:length(removeColumn)) {
  finalDataFrame[, removeColumn[i]] = NULL
}

removeColumn = paste("weekday", 1:24, sep=".")
for(i in 1:length(removeColumn)) {
  finalDataFrame[, removeColumn[i]] = NULL
}

removeColumn = paste("day.of.week", 1:24, sep=".")
for(i in 1:length(removeColumn)) {
  finalDataFrame[, removeColumn[i]] = NULL
}

removeColumn = paste("cosine.day.of.week", 1:24, sep=".")
for(i in 1:length(removeColumn)) {
  finalDataFrame[, removeColumn[i]] = NULL
}

removeColumn = paste("sine.day.of.week", 1:24, sep=".")
for(i in 1:length(removeColumn)) {
  finalDataFrame[, removeColumn[i]] = NULL
}

removeColumn = paste("day.of.year", 1:24, sep=".")
for(i in 1:length(removeColumn)) {
  finalDataFrame[, removeColumn[i]] = NULL
}

removeColumn = paste("cosine.day.of.year", 1:24, sep=".")
for(i in 1:length(removeColumn)) {
  finalDataFrame[, removeColumn[i]] = NULL
}

removeColumn = paste("sine.day.of.year", 1:24, sep=".")
for(i in 1:length(removeColumn)) {
  finalDataFrame[, removeColumn[i]] = NULL
}

removeColumn = paste("isWeekend", 1:24, sep=".")
for(i in 1:length(removeColumn)) {
  finalDataFrame[, removeColumn[i]] = NULL
}

removeColumn = paste("isHoliday", 1:24, sep=".")
for(i in 1:length(removeColumn)) {
  finalDataFrame[, removeColumn[i]] = NULL
}


####keep a final.Data.Frame backUp####
backUp.final.Data.Frame = finalDataFrame


#adding the d-2, d-3 and the day before days#####
cat("#adding the d-2, d-3 and the day before days and construct the cases#####\n")
for (i in seq(5, dim(finalDataFrame)[1])) {
  
  ####2 preceding days loads
  two.preceding.days.loads = finalDataFrame[i-2, ]
  
  two.preceding.days.loads = 
    two.preceding.days.loads[grep("Loads", colnames(two.preceding.days.loads))]
  
  colnames(two.preceding.days.loads) <- paste("two.preceding.days.loads", colnames(two.preceding.days.loads), sep=".")
  
  
  ####3 preceding days loads
  three.preceding.days.loads = finalDataFrame[i-3, ]
  
  three.preceding.days.loads = 
    three.preceding.days.loads[grep("Loads", colnames(three.preceding.days.loads))]
  
  colnames(three.preceding.days.loads) <- paste("three.preceding.days.loads", colnames(three.preceding.days.loads), sep=".")
  
  
  ####yesterday meteorological values
  yesterday.weather.measures = finalDataFrame[i-1, ]
  
  yesterday.weather.measures = 
    yesterday.weather.measures[-grep("Loads", colnames(yesterday.weather.measures))]
  
  colnames(yesterday.weather.measures) <- paste("yesterday.weather.measures", colnames(yesterday.weather.measures), sep=".")
  
  
  ####present day data
  today = finalDataFrame[i, ]
  
  
  ###merging all together
  final.row = cbind(today, yesterday.weather.measures)
  final.row = cbind(final.row, two.preceding.days.loads)
  final.row = cbind(final.row, three.preceding.days.loads)
  
  if (!exists("final.Data.Set")) {
    final.Data.Set = final.row
  }
  else {
    final.Data.Set = rbind(final.Data.Set, final.row)
  }
  
  #remove some values
  rm(final.row, today, three.preceding.days.loads, yesterday.weather.measures, two.preceding.days.loads)
  
} #end of for


#renaming some of the first columns
names(final.Data.Set)[1] = "time"
names(final.Data.Set)[2] = "weekday"
names(final.Data.Set)[4] = "isWeekend"
names(final.Data.Set)[5] = "isHoliday"
names(final.Data.Set)[6] = "day.of.week"
names(final.Data.Set)[7] = "sine.day.of.week"
names(final.Data.Set)[8] = "cosine.day.of.week"
names(final.Data.Set)[9] = "day.of.year"
names(final.Data.Set)[10] = "sine.day.of.year"
names(final.Data.Set)[11] = "cosine.day.of.year"

colnames(final.Data.Set) = 
  gsub("yesterday.weather.measures.time.0", "yesterday.weather.measures.time", colnames(final.Data.Set))


colnames(final.Data.Set) = 
  gsub("yesterday.weather.measures.weekday.0", "yesterday.weather.measures.weekday", colnames(final.Data.Set))


colnames(final.Data.Set) = 
  gsub("yesterday.weather.measures.isHoliday.0", "yesterday.weather.measures.isHoliday", colnames(final.Data.Set))


colnames(final.Data.Set) = 
  gsub("yesterday.weather.measures.isWeekend.0", "yesterday.weather.measures.isWeekend", colnames(final.Data.Set))


colnames(final.Data.Set) = 
  gsub("yesterday.weather.measures.day.of.week.0", "yesterday.weather.measures.day.of.week", colnames(final.Data.Set))


colnames(final.Data.Set) = 
  gsub("yesterday.weather.measures.sine.day.of.week.0", "yesterday.weather.measures.sine.day.of.week", colnames(final.Data.Set))


colnames(final.Data.Set) = 
  gsub("yesterday.weather.measures.cosine.day.of.week.0", "yesterday.weather.measures.cosine.day.of.week", colnames(final.Data.Set))


colnames(final.Data.Set) = 
  gsub("yesterday.weather.measures.day.of.year.0", "yesterday.weather.measures.day.of.year", colnames(final.Data.Set))


colnames(final.Data.Set) = 
  gsub("yesterday.weather.measures.sine.day.of.year.0", "yesterday.weather.measures.sine.day.of.year", colnames(final.Data.Set))


colnames(final.Data.Set) = 
  gsub("yesterday.weather.measures.cosine.day.of.year.0", "yesterday.weather.measures.cosine.day.of.year", colnames(final.Data.Set))



#keep a final.Data.Set backup####
backUp.final.Data.Set = final.Data.Set


#remove some variables with no use####
rm(combinedWeather, combinedLoads, removeColumn, i, j, is.zero.list)
rm(na.List)


cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60)
rm(startTime)

#elapsed time in minutes:  8.6325