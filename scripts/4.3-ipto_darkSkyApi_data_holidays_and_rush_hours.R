library(tibble)

#RushHour new feature#####
cat("#RushHour new feature#####\n")
time = darkSky.WeatherData$time
time = format(time, format='%H')
time = as.numeric(time)
isRushHour = time
isRushHour[(isRushHour>=0) & (isRushHour<=7)] = 0
isRushHour[(isRushHour>=8) & (isRushHour<=14)] = 1
isRushHour[(isRushHour>=15) & (isRushHour<=17)] = 0
isRushHour[(isRushHour>=18) & (isRushHour<=22)] = 1
isRushHour[(isRushHour==23)] = 0
darkSky.WeatherData =  add_column(darkSky.WeatherData, isRushHour, .after = "time")
darkSky.WeatherData$isRushHour = as.factor(darkSky.WeatherData$isRushHour)



#isWeekend new feature####
cat("#isWeekend new feature####\n")
isWeekend = as.character(darkSky.WeatherData$weekday)
isWeekend[(isWeekend == "Saturday") | (isWeekend == "Sunday")] = 1
isWeekend[(isWeekend != "Saturday") & (isWeekend != "Sunday") & (isWeekend != 1)] = 0
darkSky.WeatherData =  add_column(darkSky.WeatherData, isWeekend, .after = "isRushHour")
darkSky.WeatherData$isWeekend = as.factor(darkSky.WeatherData$isWeekend)


#new holidays feature####
#Kudos:
#http://karamatskos.blogspot.gr/2012/04/blog-post_07.html
#http://www.eortologio.gr/arthra/pasxa.php

cat("#new holidays feature####\n")
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
  if (d > 30) {
    month = 5
    d = d %% 30
  }

  easter = as.Date(paste(year, month, d, sep="/"))
  #cat("greek easter date ", easter, "\n")


    #new year
  if(dim(darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"01","01", sep = "-")), ])[1] != 0 )
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"01","01", sep = "-")), ]$isHoliday = 1

    #epifany
  if(dim(darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"01","06", sep = "-")), ])[1] != 0)
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"01","06", sep = "-")), ]$isHoliday = 1


    #lent day
  if(dim(darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == (easter - 48), ])[1] != 0)
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == (easter - 48), ]$isHoliday = 1


    #25th of March
  if(dim(darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"03","25", sep = "-")), ])[1] != 0)
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"03","25", sep = "-")), ]$isHoliday = 1


    #Great Friday
  if(dim(darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == (easter - 2), ])[1] != 0)
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == (easter - 2), ]$isHoliday = 1


    #Great Saturday
  if(dim(darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == (easter - 2), ])[1] != 0)
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == (easter - 1), ]$isHoliday = 1


    #Orthodox Easter
  if(dim(darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == easter, ])[1] != 0)
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == easter, ]$isHoliday = 1


    #Orthodox Easter 2nd day
  if(dim(darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == (easter + 1), ])[1] != 0)
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == (easter + 1), ]$isHoliday = 1

    
    #work day
  if(dim(darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"05","01", sep = "-")), ])[1] != 0)
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"05","01", sep = "-")), ]$isHoliday = 1


    #Pentecost
  if(dim(darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == (easter + 49), ])[1] != 0)
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == (easter + 49), ]$isHoliday = 1



    #Holy spirit
  if(dim(darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == (easter + 50), ])[1] != 0 )
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == (easter + 50), ]$isHoliday = 1



    #15 of August
  if(dim(darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"08","15", sep = "-")), ])[1] != 0)
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"08","15", sep = "-")), ]$isHoliday = 1


    #28th of October
  if(dim(darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"10","28", sep = "-")), ])[1] != 0)
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"10","28", sep = "-")), ]$isHoliday = 1


    #Christmas
  if(dim(darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"12","25", sep = "-")), ])[1] != 0)
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"12","25", sep = "-")), ]$isHoliday = 1


    #Christmas 2nd day
  if(dim(darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"12","26", sep = "-")), ])[1] != 0)
    darkSky.WeatherData[as.Date(substr(as.character(darkSky.WeatherData$time), start = 1, stop = 10)) == as.Date(paste(year,"12","26", sep = "-")), ]$isHoliday = 1
}


rm(a,b,c,d,e, month)



darkSky.WeatherData$isHoliday = as.factor(darkSky.WeatherData$isHoliday)


#keep a backup####
#backUp.DarkSky = darkSky.WeatherData

rm(isRushHour, isWeekend, isHoliday, year, years, easter)