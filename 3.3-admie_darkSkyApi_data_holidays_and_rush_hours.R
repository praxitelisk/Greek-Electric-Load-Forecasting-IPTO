library(tibble)


time = darkSky.WeatherData$time
time = format(time, format='%H')
time = as.numeric(time)
isRushHour = time
isRushHour[(isRushHour>=0) & (isRushHour<=6)] = 0
isRushHour[(isRushHour>=7) & (isRushHour<=23)] = 1
darkSky.WeatherData =  add_column(darkSky.WeatherData, isRushHour, .after = "time")
darkSky.WeatherData$isRushHour = as.factor(darkSky.WeatherData$isRushHour)


isWeekend = as.character(darkSky.WeatherData$weekday)
isWeekend[(isWeekend == "Σάββατο") | (isWeekend == "Κυριακή")] = 1
isWeekend[(isWeekend != "Σαββατο") & (isWeekend != "Κυριακή") & (isWeekend != 1)] = 0
darkSky.WeatherData =  add_column(darkSky.WeatherData, isWeekend, .after = "isRushHour")
darkSky.WeatherData$isWeekend = as.factor(darkSky.WeatherData$isWeekend)


#new holidays feature
#Kudos:
#http://karamatskos.blogspot.gr/2012/04/blog-post_07.html
#http://www.eortologio.gr/arthra/pasxa.php

time = darkSky.WeatherData$time
time = format(time, format='%Y')
years = unique(time)
rm(time)
years = as.numeric(years)

isHoliday = rep.int(0, nrow(darkSky.WeatherData))
darkSky.WeatherData =  add_column(darkSky.WeatherData, isHoliday, .after = "isWeekend")

for (year in years) {
  a = year %% 19
  b = year %% 4
  c = year %% 7
  d = ((a * 19) + 15) %% 30
  e = (2*b + 4*c + 6*d +6) %% 7
  d = d + e + 4
  month = 4
  if (d> 30) {
    month = 5
    d = d %% 30
  }
  
  easter = as.Date(paste(year, month, d, sep="/"))
  print(easter)
  
  
  if (year != 2010) {
    
    #new year
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"01","01", sep = "-")), ]$isHoliday = 1
    
    #epifany
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"01","06", sep = "-")), ]$isHoliday = 1
    
    
    #lent day
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == (easter - 48), ]$isHoliday = 1
    
    
    #25th of March
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"03","25", sep = "-")), ]$isHoliday = 1
    
    
    #Great Friday
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == (easter - 2), ]$isHoliday = 1
    
    
    #Great Saturday
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == (easter - 1), ]$isHoliday = 1
    
    
    #Orthodox Easter
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == easter, ]$isHoliday = 1
    
    
    #Orthodox Easter 2nd day
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == (easter + 1), ]$isHoliday = 1
    
    
    if (year != 2017) {
      #work day
      darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"05","01", sep = "-")), ]$isHoliday = 1
      
      
      #Pentecost
      darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == (easter + 49), ]$isHoliday = 1
      
      
      
      #Holy spirit
      darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == (easter + 50), ]$isHoliday = 1
      
      
      
      #15 of August
      darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"08","15", sep = "-")), ]$isHoliday = 1
    }
  }
  
  if (year != 2017) {
    #28th of October
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"10","28", sep = "-")), ]$isHoliday = 1
    
    
    #Christmas
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"12","25", sep = "-")), ]$isHoliday = 1
    
    
    #Christmas 2nd day
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"12","26", sep = "-")), ]$isHoliday = 1
  }
  
  
  rm(a,b,c,d,e, month)
}


darkSky.WeatherData$isHoliday = as.factor(darkSky.WeatherData$isHoliday)


#keep a backup####
backUp.DarkSky = darkSky.WeatherData

rm(time, isRushHour, isWeekend, isHoliday, year, years, easter)