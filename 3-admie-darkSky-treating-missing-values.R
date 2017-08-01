#####Fill NA values with averages--------
startTime <- proc.time()[3]


darkSky.WeatherData = backUp.DarkSky

##thessaloniki temperature missing values####
cat("##thessaloniki temperature missing values####\n")
na.List = which(is.na(darkSky.WeatherData$thessaloniki.temperature))

  
for (i in 1:length(na.List)) {
    
    darkSky.WeatherData$thessaloniki.temperature[na.List[i]] = mean(c(
      darkSky.WeatherData$thessaloniki.temperature[na.List[i] - 1], 
      darkSky.WeatherData$thessaloniki.temperature[na.List[i] + 1]))
    
}
  
  na.List = which(is.na(darkSky.WeatherData$thessaloniki.temperature))


# about the 25801 25802 25803 25804 NA values who are consecutive values####
na.List = which(is.na(darkSky.WeatherData$thessaloniki.temperature))

for (i in 1:length(na.List)) {
  
  time1 = (darkSky.WeatherData$time[na.List[i]] - 60*60*24)
  time2 = (darkSky.WeatherData$time[na.List[i]] + 60*60*24)
  
  darkSky.WeatherData$thessaloniki.temperature[na.List[i]] = mean(c(
    darkSky.WeatherData[darkSky.WeatherData$time == time1, ]$thessaloniki.temperature,
    darkSky.WeatherData[darkSky.WeatherData$time == time2, ]$thessaloniki.temperature))
  
  
}


#athens temperature missing values####
cat("#athens temperature missing values####\n")
na.List = which(is.na(darkSky.WeatherData$athens.temperature))

for (i in 1:length(na.List)) {
  
  darkSky.WeatherData$athens.temperature[na.List[i]] = mean(c(
    darkSky.WeatherData$athens.temperature[na.List[i] - 1], 
    darkSky.WeatherData$athens.temperature[na.List[i] + 1]))
  
}

#thessaloniki apparent temperature missing values####
cat("#thessaloniki apparent temperature missing values####\n")
na.List = which(is.na(darkSky.WeatherData$thessaloniki.apparentTemperature))
for (i in 1:length(na.List)) {
  
  darkSky.WeatherData$thessaloniki.apparentTemperature[na.List[i]] = mean(c(
    darkSky.WeatherData$thessaloniki.apparentTemperature[na.List[i] - 1], 
    darkSky.WeatherData$thessaloniki.apparentTemperature[na.List[i] + 1]))
  
}

na.List = which(is.na(darkSky.WeatherData$thessaloniki.apparentTemperature))


# about the 25801 25802 25803 25804 NA values who are consecutive values####
na.List = which(is.na(darkSky.WeatherData$thessaloniki.apparentTemperature))

for (i in 1:length(na.List)) {
  
  time1 = (darkSky.WeatherData$time[na.List[i]] - 60*60*24)
  time2 = (darkSky.WeatherData$time[na.List[i]] + 60*60*24)
  
  darkSky.WeatherData$thessaloniki.apparentTemperature[na.List[i]] = mean(c(
    darkSky.WeatherData[darkSky.WeatherData$time == time1, ]$thessaloniki.apparentTemperature,
    darkSky.WeatherData[darkSky.WeatherData$time == time2, ]$thessaloniki.apparentTemperature))
  
}

#athens apparent temperature missing values####
cat("#athens temperature missing values####\n")
na.List = which(is.na(darkSky.WeatherData$athens.apparentTemperature))

for (i in 1:length(na.List)) {
  
  darkSky.WeatherData$athens.apparentTemperature[na.List[i]] = mean(c(
    darkSky.WeatherData$athens.apparentTemperature[na.List[i] - 1], 
    darkSky.WeatherData$athens.apparentTemperature[na.List[i] + 1]))
  
}
  
  
##thessaloniki cloudCover missing values####
cat("##thessaloniki Cloudcover missing values####\n")
na.List = which(is.na(darkSky.WeatherData$thessaloniki.cloudCover))
  
  
for (i in 1:length(na.List)) {
    
  darkSky.WeatherData$thessaloniki.cloudCover[na.List[i]] = mean(c(
    darkSky.WeatherData$thessaloniki.cloudCover[na.List[i] - 1], 
    darkSky.WeatherData$thessaloniki.cloudCover[na.List[i] + 1]))
  
}
  
na.List = which(is.na(darkSky.WeatherData$thessaloniki.cloudCover))
  
  
# about the NA values who are consecutive values####
na.List = which(is.na(darkSky.WeatherData$thessaloniki.cloudCover))
  
for (i in 1:length(na.List)) {
    
  time1 = (darkSky.WeatherData$time[na.List[i]] - 60*60*24)
  time2 = (darkSky.WeatherData$time[na.List[i]] + 60*60*24)
    
  darkSky.WeatherData$thessaloniki.cloudCover[na.List[i]] = mean(c(
    darkSky.WeatherData[darkSky.WeatherData$time == time1, ]$thessaloniki.cloudCover,
    darkSky.WeatherData[darkSky.WeatherData$time == time2, ]$thessaloniki.cloudCover))
    
    
}

na.List = which(is.na(darkSky.WeatherData$thessaloniki.cloudCover))
darkSky.WeatherData$thessaloniki.cloudCover[na.List] = 0


#athens cloudCover missing values####
cat("#athens Cloudcover missing values####\n")
na.List = which(is.na(darkSky.WeatherData$athens.cloudCover))
  
for (i in 1:length(na.List)) {
    
  darkSky.WeatherData$athens.cloudCover[na.List[i]] = mean(c(
    darkSky.WeatherData$athens.cloudCover[na.List[i] - 1], 
    darkSky.WeatherData$athens.cloudCover[na.List[i] + 1]))
    
}
  
# about the NA values who are consecutive values####
na.List = which(is.na(darkSky.WeatherData$athens.cloudCover))
  
for (i in 1:length(na.List)) {
  
  time1 = (darkSky.WeatherData$time[na.List[i]] - 60*60*24)
  time2 = (darkSky.WeatherData$time[na.List[i]] + 60*60*24)
    
  darkSky.WeatherData$athens.cloudCover[na.List[i]] = mean(c(
    darkSky.WeatherData[darkSky.WeatherData$time == time1, ]$athens.cloudCover,
    darkSky.WeatherData[darkSky.WeatherData$time == time2, ]$athens.cloudCover))
    
}
na.List = which(is.na(darkSky.WeatherData$athens.cloudCover))
darkSky.WeatherData$athens.cloudCover[na.List] = 0


##thessaloniki dewPoint missing values####
cat("##thessaloniki dewPoint missing values####\n")
na.List = which(is.na(darkSky.WeatherData$thessaloniki.dewPoint))


for (i in 1:length(na.List)) {
  
  darkSky.WeatherData$thessaloniki.dewPoint[na.List[i]] = mean(c(
    darkSky.WeatherData$thessaloniki.dewPoint[na.List[i] - 1], 
    darkSky.WeatherData$thessaloniki.dewPoint[na.List[i] + 1]))
  
}

na.List = which(is.na(darkSky.WeatherData$thessaloniki.dewPoint))


# about the NA values who are consecutive values####
na.List = which(is.na(darkSky.WeatherData$thessaloniki.dewPoint))

for (i in 1:length(na.List)) {
  
  time1 = (darkSky.WeatherData$time[na.List[i]] - 60*60*24)
  time2 = (darkSky.WeatherData$time[na.List[i]] + 60*60*24)
  
  darkSky.WeatherData$thessaloniki.dewPoint[na.List[i]] = mean(c(
    darkSky.WeatherData[darkSky.WeatherData$time == time1, ]$thessaloniki.dewPoint,
    darkSky.WeatherData[darkSky.WeatherData$time == time2, ]$thessaloniki.dewPoint))
  
}

#athens dewPoint missing values####
cat("#athens dewPoint missing values####\n")
na.List = which(is.na(darkSky.WeatherData$athens.dewPoint))

for (i in 1:length(na.List)) {
  
  darkSky.WeatherData$athens.dewPoint[na.List[i]] = mean(c(
    darkSky.WeatherData$athens.dewPoint[na.List[i] - 1], 
    darkSky.WeatherData$athens.dewPoint[na.List[i] + 1]))
  
}

##thessaloniki humidity missing values####
cat("##thessaloniki humidity missing values####\n")
na.List = which(is.na(darkSky.WeatherData$thessaloniki.humidity))


for (i in 1:length(na.List)) {
  
  darkSky.WeatherData$thessaloniki.humidity[na.List[i]] = mean(c(
    darkSky.WeatherData$thessaloniki.humidity[na.List[i] - 1], 
    darkSky.WeatherData$thessaloniki.humidity[na.List[i] + 1]))
  
}

na.List = which(is.na(darkSky.WeatherData$thessaloniki.humidity))


# about the NA values who are consecutive values####
na.List = which(is.na(darkSky.WeatherData$thessaloniki.humidity))

for (i in 1:length(na.List)) {
  
  time1 = (darkSky.WeatherData$time[na.List[i]] - 60*60*24)
  time2 = (darkSky.WeatherData$time[na.List[i]] + 60*60*24)
  
  darkSky.WeatherData$thessaloniki.humidity[na.List[i]] = mean(c(
    darkSky.WeatherData[darkSky.WeatherData$time == time1, ]$thessaloniki.humidity,
    darkSky.WeatherData[darkSky.WeatherData$time == time2, ]$thessaloniki.humidity))
  
}

na.List = which(is.na(darkSky.WeatherData$thessaloniki.humidity))
darkSky.WeatherData$thessaloniki.humidity[na.List] = 0


#athens humidity missing values####
cat("#athens humidity missing values####\n")
na.List = which(is.na(darkSky.WeatherData$athens.humidity))

for (i in 1:length(na.List)) {
  
  darkSky.WeatherData$athens.humidity[na.List[i]] = mean(c(
    darkSky.WeatherData$athens.humidity[na.List[i] - 1], 
    darkSky.WeatherData$athens.humidity[na.List[i] + 1]))
  
}


##thessaloniki windSpeed missing values####
cat("##thessaloniki windSpeed missing values####\n")
na.List = which(is.na(darkSky.WeatherData$thessaloniki.windSpeed))


for (i in 1:length(na.List)) {
    
  darkSky.WeatherData$thessaloniki.windSpeed[na.List[i]] = mean(c(
    darkSky.WeatherData$thessaloniki.windSpeed[na.List[i] - 1], 
    darkSky.WeatherData$thessaloniki.windSpeed[na.List[i] + 1]))
  
}

na.List = which(is.na(darkSky.WeatherData$thessaloniki.windSpeed))


##about the NA values who are consecutive values####
na.List = which(is.na(darkSky.WeatherData$thessaloniki.windSpeed))

for (i in 1:length(na.List)) {
  
  time1 = (darkSky.WeatherData$time[na.List[i]] - 60*60*24)
  time2 = (darkSky.WeatherData$time[na.List[i]] + 60*60*24)
  
  darkSky.WeatherData$thessaloniki.windSpeed[na.List[i]] = mean(c(
    darkSky.WeatherData[darkSky.WeatherData$time == time1, ]$thessaloniki.windSpeed,
    darkSky.WeatherData[darkSky.WeatherData$time == time2, ]$thessaloniki.windSpeed))
  
}


na.List = which(is.na(darkSky.WeatherData$thessaloniki.windSpeed))
darkSky.WeatherData$thessaloniki.windSpeed[na.List] = 0


##athens windSpeed missing values####
cat("##athens windSpeed missing values####\n")
na.List = which(is.na(darkSky.WeatherData$athens.windSpeed))


for (i in 1:length(na.List)) {
  
  darkSky.WeatherData$athens.windSpeed[na.List[i]] = mean(c(
    darkSky.WeatherData$athens.windSpeed[na.List[i] - 1], 
    darkSky.WeatherData$athens.windSpeed[na.List[i] + 1]))
  
}

na.List = which(is.na(darkSky.WeatherData$athens.windSpeed))


# about the NA values who are consecutive values####
na.List = which(is.na(darkSky.WeatherData$athens.windSpeed))

for (i in 1:length(na.List)) {
  
  time1 = (darkSky.WeatherData$time[na.List[i]] - 60*60*24)
  time2 = (darkSky.WeatherData$time[na.List[i]] + 60*60*24)
  
  darkSky.WeatherData$athens.windSpeed[na.List[i]] = mean(c(
    darkSky.WeatherData[darkSky.WeatherData$time == time1, ]$athens.windSpeed,
    darkSky.WeatherData[darkSky.WeatherData$time == time2, ]$athens.windSpeed))
  
}


#thessaloniki icon missing values#####
cat("#thessaloniki icon missing values#####\n")
na.List = which(is.na(darkSky.WeatherData$thessaloniki.icon))
for (i in 1:length(na.List)) {
  
  darkSky.WeatherData$thessaloniki.icon[na.List[i]] = 
    darkSky.WeatherData$thessaloniki.icon[na.List[i] - 1]
}


#athens icon missing values#####
cat("#athens icon missing values#####\n")
na.List = which(is.na(darkSky.WeatherData$athens.icon))
for (i in 1:length(na.List)) {
      
  darkSky.WeatherData$athens.icon[na.List[i]] = darkSky.WeatherData$athens.icon[na.List[i] - 1]
}


#thessaloniki summary missing values#####
cat("#thessaloniki icon missing values#####\n")
na.List = which(is.na(darkSky.WeatherData$thessaloniki.summary))
for (i in 1:length(na.List)) {
  
  darkSky.WeatherData$thessaloniki.summary[na.List[i]] = 
    darkSky.WeatherData$thessaloniki.summary[na.List[i] - 1]
}


#athens summary missing values#####
cat("#athens icon missing values#####\n")
na.List = which(is.na(darkSky.WeatherData$athens.summary))
for (i in 1:length(na.List)) {
  
  darkSky.WeatherData$athens.summary[na.List[i]] = darkSky.WeatherData$athens.summary[na.List[i] - 1]
}


#thessaloniki windbearing missing values#####
cat("##thessaloniki windBearing missing values####\n")
na.List = which(is.na(darkSky.WeatherData$thessaloniki.windBearing))


for (i in 1:length(na.List)) {
  
  darkSky.WeatherData$thessaloniki.windBearing[na.List[i]] = mean(c(
    darkSky.WeatherData$thessaloniki.windBearing[na.List[i] - 1], 
    darkSky.WeatherData$thessaloniki.windBearing[na.List[i] + 1]))
  
}

na.List = which(is.na(darkSky.WeatherData$thessaloniki.windBearing))


# about the consecutive values####
na.List = which(is.na(darkSky.WeatherData$thessaloniki.windBearing))

for (i in 1:length(na.List)) {
  
  time1 = (darkSky.WeatherData$time[na.List[i]] - 60*60*24)
  time2 = (darkSky.WeatherData$time[na.List[i]] + 60*60*24)
  
  darkSky.WeatherData$thessaloniki.windBearing[na.List[i]] = mean(c(
    darkSky.WeatherData[darkSky.WeatherData$time == time1, ]$thessaloniki.windBearing,
    darkSky.WeatherData[darkSky.WeatherData$time == time2, ]$thessaloniki.windBearing))
  
}


#about the rest of them turn them to zero####
na.List = which(is.na(darkSky.WeatherData$thessaloniki.windBearing))
darkSky.WeatherData$thessaloniki.windBearing[na.List] = 0

darkSky.WeatherData$sine.thessaloniki.windBearing = 
  sin(darkSky.WeatherData$thessaloniki.windBearing)

darkSky.WeatherData$cosine.thessaloniki.windBearing = 
  cos(darkSky.WeatherData$thessaloniki.windBearing)


#athens windbearing missing values#####
cat("##athens windBearing missing values####\n")
na.List = which(is.na(darkSky.WeatherData$athens.windBearing))


for (i in 1:length(na.List)) {
  
  darkSky.WeatherData$athens.windBearing[na.List[i]] = mean(c(
    darkSky.WeatherData$athens.windBearing[na.List[i] - 1], 
    darkSky.WeatherData$athens.windBearing[na.List[i] + 1]))
  
}

na.List = which(is.na(darkSky.WeatherData$athens.windBearing))

for (i in 1:length(na.List)) {
  
  time1 = (darkSky.WeatherData$time[na.List[i]] - 60*60*24)
  time2 = (darkSky.WeatherData$time[na.List[i]] + 60*60*24)
   
  darkSky.WeatherData$athens.windBearing[na.List[i]] = mean(c(
  darkSky.WeatherData[darkSky.WeatherData$time == time1, ]$athens.windBearing,
  darkSky.WeatherData[darkSky.WeatherData$time == time2, ]$athens.windBearing))
}

darkSky.WeatherData$sine.athens.windBearing = 
  sin(darkSky.WeatherData$athens.windBearing)

darkSky.WeatherData$cosine.athens.windBearing = 
  cos(darkSky.WeatherData$athens.windBearing)


##thessaloniki visibility missing values####
cat("##thessaloniki visibility missing values####\n")
na.List = which(is.na(darkSky.WeatherData$thessaloniki.visibility))

for (i in 1:length(na.List)) {
  
  darkSky.WeatherData$thessaloniki.visibility[na.List[i]] = mean(c(
    darkSky.WeatherData$thessaloniki.visibility[na.List[i] - 1], 
    darkSky.WeatherData$thessaloniki.visibility[na.List[i] + 1]))
  
}

na.List = which(is.na(darkSky.WeatherData$thessaloniki.visibility))

# about the NA values who are consecutive values####
na.List = which(is.na(darkSky.WeatherData$thessaloniki.visibility))

for (i in 1:length(na.List)) {
  
  time1 = (darkSky.WeatherData$time[na.List[i]] - 60*60*24)
  time2 = (darkSky.WeatherData$time[na.List[i]] + 60*60*24)
  
  darkSky.WeatherData$thessaloniki.visibility[na.List[i]] = mean(c(
    darkSky.WeatherData[darkSky.WeatherData$time == time1, ]$thessaloniki.visibility,
    darkSky.WeatherData[darkSky.WeatherData$time == time2, ]$thessaloniki.visibility))
  
}


#about the rest of them####
na.List = which(is.na(darkSky.WeatherData$thessaloniki.visibility))

for (i in 1:length(na.List)) {
  
  darkSky.WeatherData$thessaloniki.visibility[na.List[i]] = 
    darkSky.WeatherData$thessaloniki.visibility[na.List[i]-1]
  
}


##athens visibility missing values####
cat("##athens visibility missing values####\n")
na.List = which(is.na(darkSky.WeatherData$athens.visibility))

for (i in 1:length(na.List)) {
  
  darkSky.WeatherData$athens.visibility[na.List[i]] = mean(c(
    darkSky.WeatherData$athens.visibility[na.List[i] - 1], 
    darkSky.WeatherData$athens.visibility[na.List[i] + 1]))
  
}

na.List = which(is.na(darkSky.WeatherData$athens.visibility))

# about the NA values who are consecutive values####
na.List = which(is.na(darkSky.WeatherData$athens.visibility))

for (i in 1:length(na.List)) {
  
  time1 = (darkSky.WeatherData$time[na.List[i]] - 60*60*24)
  time2 = (darkSky.WeatherData$time[na.List[i]] + 60*60*24)
  
  darkSky.WeatherData$athens.visibility[na.List[i]] = mean(c(
    darkSky.WeatherData[darkSky.WeatherData$time == time1, ]$athens.visibility,
    darkSky.WeatherData[darkSky.WeatherData$time == time2, ]$athens.visibility))
  
}


#about the rest of them####
na.List = which(is.na(darkSky.WeatherData$athens.visibility))

for (i in 1:length(na.List)) {
  
  darkSky.WeatherData$athens.visibility[na.List[i]] = 
    darkSky.WeatherData$athens.visibility[na.List[i]-1]
  
}


##thessaloniki uvIndex missing values####
cat("##thessaloniki uvIndex missing values####\n")
na.List = which(is.na(darkSky.WeatherData$thessaloniki.uvIndex))
darkSky.WeatherData$thessaloniki.uvIndex[na.List] = 0


##athens uvIndex missing values####
cat("##athens uvIndex missing values####\n")
na.List = which(is.na(darkSky.WeatherData$athens.uvIndex))
darkSky.WeatherData$athens.uvIndex[na.List] = 0

#removing some variables####
rm(na.List, time1, time2, i)


cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60)
