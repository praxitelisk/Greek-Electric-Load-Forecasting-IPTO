###Adding an extra date column to loads in order to match them with
#darkSkyAPI dates and meteorogical data.

library(tibble) #for add_column command

myLoads = backUp.Loads


time = paste(myLoads$DATE," ",myLoads$HOUR,":00", sep="", collapse = NULL)
#time = strptime(time, format="%Y-%m-%d %H:%M")
time = as.POSIXct(time, format="%Y-%m-%d %H:%M", tz = Sys.timezone())


#an auxilliary variable taken from darkSkyAPI in order to check if
#previous time variable is made correctly.
#darkSkyTime = darkSky.WeatherData$time


#add time column to loads data frame.
myLoads = add_column(myLoads, time, .after = "HOUR")


#remove the other 25 hour values-------------
myLoads <- myLoads[!myLoads$HOUR == 25, ]


#remove March daylight saving day
datesToBeRemoved = myLoads$DATE[which(is.na(myLoads$time))]

for(i in 1:length(datesToBeRemoved)) {
  #print(datesToBeRemoved[i])
  myLoads = myLoads[!myLoads$DATE == datesToBeRemoved[i], ]
}


#re-indexing
row.names(myLoads) <- 1:nrow(myLoads)

rm(time, datesToBeRemoved)