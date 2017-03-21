library("darksky")
library("tibble")

if (!exists("darkSky.HistoricalData")) {
  darkSky.HistoricalData <- list()
}

startTime <- proc.time()[3]

temp <- list()

temp.thess <- list()

temp.athens <- list()


days <- seq(from=as.Date('2016-10-07'), to=as.Date("2016-11-30"),by='days' )

for ( i in seq_along(days) ) {

  #historical past forecast for thessaloniki and athens
  temp.thess <- get_forecast_for(40.6403, 22.9353, paste0(days[i],"T12:00:00-0200"), add_headers=TRUE, units="si")
  
  temp.athens <- get_forecast_for(37.9841, 23.728, paste0(days[i],"T12:00:00-0200"), add_headers=TRUE, units="si")

  
  #convert tibbles to data.frame
  temp.athens <- as.data.frame(temp.athens$hourly)
  
  temp.thess <- as.data.frame(temp.thess$hourly)
  
  
  #merge a full 24-hour day data.frame to fill the missing columns
  #whether there are missing hour-rows from historical past predictions
  day =seq(
    from=as.POSIXct(paste(days[i]," 00:00", sep=" "), tz=Sys.timezone()),
    to=as.POSIXct(paste(days[i]," 23:00", sep=" "), tz=Sys.timezone()),
    by="hour"
  )
  day = as.data.frame(day)
  colnames(day) = "time"
  
  temp.athens = merge(day, temp.athens, by="time", all.x =TRUE)
  temp.thess = merge(day, temp.thess, by="time", all.x =TRUE)
  
  #change the name of columns for Athens' weather attributes
  colnames(temp.athens) <- paste0("athens.", colnames(temp.athens))
  colnames(temp.athens)[1] = "time"
  
  #change the name of columns for Thessaloniki's weather attributes
  temp.thess$time = NULL
  colnames(temp.thess) <- paste0("thessaloniki.", colnames(temp.thess))
  
  
  #save both cities' weather data in a common list
  temp <- cbind(temp.athens, temp.thess)
  
  #get names of all the weekdays
  test = cbind(weekdays(temp$time), temp)
  colnames(test)[1] = "weekday"
  
  #concat it in the main data frame table
  darkSky.HistoricalData <- rbind(darkSky.HistoricalData, test)
  

}

#get names of all the weekdays
#test = cbind(weekdays(darkSky.HistoricalData$time), darkSky.HistoricalData)
#colnames(test)[1] = "weekday"
#darkSky.HistoricalData = test


#remove all auxilary variables
rm(temp.athens, temp.thess, temp, test)

cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60)

#latest elapsed time in seconds:  280.89