library(tibble) #for add_column command
library(lubridate) # for yday command

#day of week 
day.of.week = weekdays(darkSky.WeatherData$time)
weekday = day.of.week
day.of.week[day.of.week=="Κυριακή"]=1
day.of.week[day.of.week=="Δευτέρα"]=2
day.of.week[day.of.week=="Τρίτη"]=3
day.of.week[day.of.week=="Τετάρτη"]=4
day.of.week[day.of.week=="Πέμπτη"]=5
day.of.week[day.of.week=="Παρασκευή"]=6
day.of.week[day.of.week=="Σάββατο"]=7

day.of.week = as.numeric(day.of.week)
#day.of.week = 360/7 * day.of.week

#day of year
day.of.year = yday(darkSky.WeatherData$time)
#day.of.year = 360/365 * day.of.year

#cosine - sine day of week
cosine.day.of.year = cos(360/365 * day.of.year)
sine.day.of.year = sin(360/365 * day.of.year)

cosine.day.of.week = cos(360/7 * day.of.week)
sine.day.of.week = sin(360/7 * day.of.week)

#add new columns in main weather information data.frame
darkSky.WeatherData =  add_column(darkSky.WeatherData, day.of.week, .after = "time")
darkSky.WeatherData =  add_column(darkSky.WeatherData, sine.day.of.week, .after = "day.of.week")
darkSky.WeatherData =  add_column(darkSky.WeatherData, cosine.day.of.week, .after = "sine.day.of.week")
darkSky.WeatherData =  add_column(darkSky.WeatherData, day.of.year, .after = "cosine.day.of.week")
darkSky.WeatherData =  add_column(darkSky.WeatherData, sine.day.of.year, .after = "day.of.year")
darkSky.WeatherData =  add_column(darkSky.WeatherData, cosine.day.of.year, .after = "sine.day.of.year")

#remove auxiliary variables
rm(
  day.of.year,
  day.of.week,
  cosine.day.of.week,
  cosine.day.of.year,
  sine.day.of.week,
  sine.day.of.year,
  weekday
)







