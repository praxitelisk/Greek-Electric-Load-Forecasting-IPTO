rm(combined, finalDataFrame, final.Date.Frame)

startTime <- proc.time()[3]

######a small preprocess######
#merge the weather data and the Loads based on the common date entry
darkSky.N.Loads.Combined = merge(darkSky.WeatherData, myLoads, by="time", all.y = TRUE)

#remove NA values from merge
darkSky.N.Loads.Combined =  subset(darkSky.N.Loads.Combined, !is.na(darkSky.N.Loads.Combined$weekday))

#add some NA values as a data-column to complete 24-hour days that are missing
#e.g. the first and the last date from the weather data.frame
# the code below works only for 2010-10-01 00:00 column
temp = data.frame(matrix(NA, nrow=1, ncol=length(darkSky.N.Loads.Combined)))
colnames(temp)= colnames(darkSky.N.Loads.Combined)
temp[,1] = as.POSIXct("2010-10-01 00:00")

darkSky.N.Loads.Combined = rbind(temp, darkSky.N.Loads.Combined)
rm(temp)

#the code below works only for for 2016-12-01 01:00
# temp = data.frame(matrix(NA, nrow=23, ncol=length(darkSky.N.Loads.Combined)))
# colnames(temp)= colnames(darkSky.N.Loads.Combined)
# temp[,1] <- as.POSIXct(temp[,1])
# temp[1,1] = as.POSIXct("2016-12-01 01:00")
# temp[2,1] = as.POSIXct("2016-12-01 02:00")
# temp[3,1] = as.POSIXct("2016-12-01 03:00")
# temp[4,1] = as.POSIXct("2016-12-01 04:00")
# temp[5,1] = as.POSIXct("2016-12-01 05:00")
# temp[6,1] = as.POSIXct("2016-12-01 06:00")
# temp[7,1] = as.POSIXct("2016-12-01 07:00")
# temp[8,1] = as.POSIXct("2016-12-01 08:00")
# temp[9,1] = as.POSIXct("2016-12-01 09:00")
# temp[10,1] = as.POSIXct("2016-12-01 10:00")
# temp[11,1] = as.POSIXct("2016-12-01 11:00")
# temp[12,1] = as.POSIXct("2016-12-01 12:00")
# temp[13,1] = as.POSIXct("2016-12-01 13:00")
# temp[14,1] = as.POSIXct("2016-12-01 14:00")
# temp[15,1] = as.POSIXct("2016-12-01 15:00")
# temp[16,1] = as.POSIXct("2016-12-01 16:00")
# temp[17,1] = as.POSIXct("2016-12-01 17:00")
# temp[18,1] = as.POSIXct("2016-12-01 18:00")
# temp[19,1] = as.POSIXct("2016-12-01 19:00")
# temp[20,1] = as.POSIXct("2016-12-01 20:00")
# temp[21,1] = as.POSIXct("2016-12-01 21:00")
# temp[22,1] = as.POSIXct("2016-12-01 22:00")
# temp[23,1] = as.POSIXct("2016-12-01 23:00")


#darkSky.N.Loads.Combined = rbind(darkSky.N.Loads.Combined, temp)
#rm(temp)


######Parsing the Weather data - Tranforming the data.frame######
#an iterator parser with a 24-hour step after preprocess is done
#to parse hour by hour the weather data and create a new data.frame
for (i in seq(1, dim(darkSky.N.Loads.Combined)[1], by = 24)) {
  combined = cbind(
    darkSky.N.Loads.Combined[i, ],
    darkSky.N.Loads.Combined[i+1, ],
    darkSky.N.Loads.Combined[i+2, ],
    darkSky.N.Loads.Combined[i+3, ],
    darkSky.N.Loads.Combined[i+4, ],
    darkSky.N.Loads.Combined[i+5, ],
    darkSky.N.Loads.Combined[i+6, ],
    darkSky.N.Loads.Combined[i+7, ],
    darkSky.N.Loads.Combined[i+8, ],
    darkSky.N.Loads.Combined[i+9, ],
    darkSky.N.Loads.Combined[i+10, ],
    darkSky.N.Loads.Combined[i+11, ],
    darkSky.N.Loads.Combined[i+12, ],
    darkSky.N.Loads.Combined[i+13, ],
    darkSky.N.Loads.Combined[i+14, ],
    darkSky.N.Loads.Combined[i+15, ],
    darkSky.N.Loads.Combined[i+16, ],
    darkSky.N.Loads.Combined[i+17, ],
    darkSky.N.Loads.Combined[i+18, ],
    darkSky.N.Loads.Combined[i+19, ],
    darkSky.N.Loads.Combined[i+20, ],
    darkSky.N.Loads.Combined[i+21, ],
    darkSky.N.Loads.Combined[i+22, ],
    darkSky.N.Loads.Combined[i+23, ]
  )
  
  #exclude the loads and some other useless informations
  combinedWeather = combined[-grep("Loads", colnames(combined))]
  combinedWeather = combinedWeather[-grep("DATE", colnames(combinedWeather))]
  combinedWeather = combinedWeather[-grep("HOUR", colnames(combinedWeather))]
  
  #fetch again the loads to put them in the end of the data.frame
  combinedLoads = combined[grep("Loads", colnames(combined))]
  
  #create a final data.frame
  combined = cbind(combinedWeather, combinedLoads)
  
  if (!exists("finalDataFrame")) {
    finalDataFrame = combined
  }
  else {
    #paste it
    finalDataFrame = rbind(finalDataFrame, combined)
    rm(combined, combinedLoads, combinedWeather)
  }
  
  
} #end of for - iterator

#####changing to date the time column#####
dates = as.Date(finalDataFrame$time)
dates = as.data.frame(dates)
finalDataFrame$time = dates
rm(dates)

#####adding the yesterday and the day before yesterday columns#####
for (i in seq(3, dim(finalDataFrame)[1])) {
 
  the.day.before.yesterday = finalDataFrame[i-2, ]
  colnames(the.day.before.yesterday) <- paste("the.day.before.yesterday", colnames(the.day.before.yesterday), sep=".")
  
  yesterday = finalDataFrame[i-1, ]
  colnames(yesterday) <- paste("yesterday", colnames(yesterday), sep=".")
  
  today = finalDataFrame[i, ]
  
  final.row = cbind(today, yesterday)
  final.row = cbind(final.row, the.day.before.yesterday)
  
  if (!exists("final.Data.Frame")) {
    final.Data.Frame = final.row
  }
  else {
    final.Data.Frame = rbind(final.Data.Frame, final.row)
    rm(final.row, the.day.before.yesterday, yesterday, today)
  }
  
} #end of for


#naming the columns
# colnames(finalDataFrame) = c(
#   paste (
#     "00:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "01:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "02:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "03:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "04:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "05:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "06:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "07:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "08:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "09:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "10:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "11:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "12:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "13:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "14:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "15:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "16:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "17:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "18:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "19:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "20:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "21:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "22:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-1)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "23:00",
#     colnames(darkSky.N.Loads.Combined)[1:(length(colnames(darkSky.N.Loads.Combined))-)],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "00:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "01:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "02:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "03:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "04:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "05:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "06:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "07:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "08:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "09:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "10:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "11:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "12:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "13:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "14:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "15:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "16:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "17:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "18:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "19:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "20:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "21:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "22:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   ),
#   paste (
#     "23:00",
#     colnames(darkSky.N.Loads.Combined)[(length(darkSky.HistoricalData)+1):length(darkSky.N.Loads.Combined) ],
#     sep = ".",
#     collapse = NULL
#   )
# )



#remove redundant information - columns here
rm(i)

cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60)
#elapsed time in minutes:  6.599