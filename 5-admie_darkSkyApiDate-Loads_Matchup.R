#####
##Adding an extra date column to loads in order to match them with
##darkSkyAPI dates anda meteorogical data.
#####

library(tibble) #for add_column command


time = paste(myLoads$DATE," ",myLoads$HOUR,":00", sep="", collapse = NULL)
time = strptime(time, format="%Y-%m-%d %H:%M")
time = as.POSIXct(time)

#an auxilliary variable taken from darkSkyAPI in order to check if
#previous time variable is made correctly.
darkSkyTime = darkSky.WeatherData$time


#add time column to loads data frame.
myLoads = add_column(myLoads, time, .after = "HOUR")


#renaming the long load columns into a shorter one
names(myLoads)[4] <- "Loads"


#check it if works
#darkSkyTime[2] == time[1]

rm(time,darkSkyTime)