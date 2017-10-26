#####Fill NA values with averages--------
startTime <- proc.time()[3]


darkSky.WeatherData = backUp.DarkSky

fill.NA.values = function(df.column) {
  
  while(length(which(is.na(darkSky.WeatherData[[df.column]]))) != 0 ) {
    
    na.List = which(is.na(darkSky.WeatherData[[df.column]]))
    cat(df.column, "NA percent: ", 100 * length(na.List) / nrow(darkSky.WeatherData),"%\n\n")
    
    if ( length(na.List) > 0 ) {
      for (i in 1:length(na.List)) {
        
        darkSky.WeatherData[[df.column]][na.List[i]] = mean(c(
          darkSky.WeatherData[[df.column]][na.List[i] - 1], 
          darkSky.WeatherData[[df.column]][na.List[i] + 1]))
        
      }
    }
    
    na.List = which(is.na(darkSky.WeatherData[[df.column]]))
    if ( length(na.List) > 0 ) {
      
      for (i in 1:length(na.List)) {
        
        time1 = (darkSky.WeatherData$time[na.List[i]] - 60*60*24)
        time2 = (darkSky.WeatherData$time[na.List[i]] + 60*60*24)
        
        darkSky.WeatherData[[df.column]][na.List[i]] = mean(c(
          darkSky.WeatherData[darkSky.WeatherData$time == time1, ][[df.column]],
          darkSky.WeatherData[darkSky.WeatherData$time == time2, ][[df.column]]))
        
        
      }
    }
    
    #about the rest NA values fill them using the previous one####
    na.List = which(is.na(darkSky.WeatherData[[df.column]]))
    if ( length(na.List) > 0 ) {
      
      darkSky.WeatherData[[df.column]][na.List] = 
        darkSky.WeatherData[[df.column]][na.List-1]
      
    }
    
    
  } #end of while
  
  return(darkSky.WeatherData)
  
}#end of function


##thessaloniki temperature missing values####
cat("##thessaloniki temperature missing values####\n")
darkSky.WeatherData = fill.NA.values("thessaloniki.temperature")


##athens temperature missing values####
cat("#athens temperature missing values####\n")
darkSky.WeatherData = fill.NA.values("athens.temperature")


##thessaloniki apparent temperature missing values####
cat("#thessaloniki apparent temperature missing values####\n")
darkSky.WeatherData = fill.NA.values("thessaloniki.apparentTemperature")


#athens apparent temperature missing values####
cat("#athens temperature missing values####\n")
darkSky.WeatherData = fill.NA.values("athens.apparentTemperature")


##thessaloniki cloudCover missing values####
cat("##thessaloniki Cloudcover missing values####\n")
darkSky.WeatherData = fill.NA.values("thessaloniki.cloudCover")


#athens cloudCover missing values####
cat("#athens Cloudcover missing values####\n")
darkSky.WeatherData = fill.NA.values("athens.cloudCover")


##thessaloniki dewPoint missing values####
cat("##thessaloniki dewPoint missing values####\n")
darkSky.WeatherData = fill.NA.values("thessaloniki.dewPoint")


#athens dewPoint missing values####
cat("#athens dewPoint missing values####\n")
darkSky.WeatherData = fill.NA.values("athens.dewPoint")


##thessaloniki humidity missing values####
cat("##thessaloniki humidity missing values####\n")
darkSky.WeatherData = fill.NA.values("thessaloniki.humidity")


#athens humidity missing values####
cat("#athens humidity missing values####\n")
darkSky.WeatherData = fill.NA.values("athens.humidity")


##thessaloniki windSpeed missing values####
cat("##thessaloniki windSpeed missing values####\n")
darkSky.WeatherData = fill.NA.values("thessaloniki.windSpeed")


##athens windSpeed missing values####
cat("##athens windSpeed missing values####\n")
darkSky.WeatherData = fill.NA.values("athens.windSpeed")


#thessaloniki icon missing values#####
cat("#thessaloniki icon missing values#####\n")
na.List = which(is.na(darkSky.WeatherData$thessaloniki.icon))
cat("icon NA percent: ", 100 * length(na.List) / nrow(darkSky.WeatherData),"\n\n")

for (i in 1:length(na.List)) {
  
  
  darkSky.WeatherData$thessaloniki.icon[na.List[i]] = 
    darkSky.WeatherData$thessaloniki.icon[na.List[i] - 1]
}


#athens icon missing values#####
cat("#athens icon missing values#####\n")
na.List = which(is.na(darkSky.WeatherData$athens.icon))
cat("icon NA percent: ", 100 * length(na.List) / nrow(darkSky.WeatherData),"\n\n")

for (i in 1:length(na.List)) {
  
  
  darkSky.WeatherData$athens.icon[na.List[i]] = 
    darkSky.WeatherData$athens.icon[na.List[i] - 1]
}


#thessaloniki summary missing values#####
cat("#thessaloniki summary missing values#####\n")
na.List = which(is.na(darkSky.WeatherData$thessaloniki.summary))
cat("summary NA percent: ", 100 * length(na.List) / nrow(darkSky.WeatherData),"\n\n")

for (i in 1:length(na.List)) {
  
  
  darkSky.WeatherData$thessaloniki.summary[na.List[i]] = 
    darkSky.WeatherData$thessaloniki.summary[na.List[i] - 1]
}


#athens summary missing values#####
cat("#athens summary missing values#####\n")
na.List = which(is.na(darkSky.WeatherData$athens.summary))
cat("summary NA percent: ", 100 * length(na.List) / nrow(darkSky.WeatherData),"\n\n")

for (i in 1:length(na.List)) {
  
  
  
  darkSky.WeatherData$athens.summary[na.List[i]] = 
    darkSky.WeatherData$athens.summary[na.List[i] - 1]
}


#thessaloniki windBearing missing values#####
cat("##thessaloniki windBearing missing values####\n")
darkSky.WeatherData = fill.NA.values("thessaloniki.windBearing")




#athens windbearing missing values#####
cat("##athens windBearing missing values####\n")
darkSky.WeatherData = fill.NA.values("athens.windBearing")




##thessaloniki visibility missing values####
cat("##thessaloniki visibility missing values####\n")
darkSky.WeatherData = fill.NA.values("thessaloniki.visibility")


##athens visibility missing values####
cat("##athens visibility missing values####\n")
darkSky.WeatherData = fill.NA.values("athens.visibility")


##thessaloniki uvIndex missing values####
cat("##thessaloniki uvIndex missing values####\n")
na.List = which(is.na(darkSky.WeatherData$thessaloniki.uvIndex))

while(length(na.List) != 0) {
  
  na.List = which(is.na(darkSky.WeatherData$thessaloniki.uvIndex))
  cat("uvIndex NA percent: ", 100 * length(na.List) / nrow(darkSky.WeatherData),"\n\n")
  
  for (i in 1:length(na.List)) {
    
    darkSky.WeatherData$thessaloniki.uvIndex[na.List[i]] =
      darkSky.WeatherData$thessaloniki.uvIndex[na.List[i] - 1]
    
  }
  
}


##athens uvIndex missing values####
cat("##athens uvIndex missing values####\n")
na.List = which(is.na(darkSky.WeatherData$athens.uvIndex))


while(length(na.List) != 0) {
  
  na.List = which(is.na(darkSky.WeatherData$athens.uvIndex))
  cat("uvIndex NA percent: ", 100 * length(na.List) / nrow(darkSky.WeatherData),"\n\n")
  
  for (i in 1:length(na.List)) {
    
    darkSky.WeatherData$athens.uvIndex[na.List[i]] =
      darkSky.WeatherData$athens.uvIndex[na.List[i] - 1]
    
  }
}

#removing some variables####
rm(na.List, i)


cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60)


#elapsed time in minutes:  4.498333