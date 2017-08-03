#this code snippet adds the day of the week and the relative position of each
#day on the year and on the weekday using sine and cosine functions


library(tibble) #for add_column command
library(lubridate) # for yday command

#get the name of each day by name 
day.of.week = weekdays(darkSky.WeatherData$time)

day.of.week[day.of.week=="Κυριακή"] = 0
day.of.week[day.of.week=="Δευτέρα"] = 1
day.of.week[day.of.week=="Τρίτη"] = 2
day.of.week[day.of.week=="Τετάρτη"] = 3
day.of.week[day.of.week=="Πέμπτη"] = 4
day.of.week[day.of.week=="Παρασκευή"] = 5
day.of.week[day.of.week=="Σάββατο"] = 6

day.of.week = as.factor(day.of.week)
#day.of.week = 360/7 * day.of.week

#day of year
day.of.year = yday(darkSky.WeatherData$time)
#day.of.year = 360/365 * day.of.year

#cosine - sine day of week
cosine.day.of.year = cos(360/365 * day.of.year)
sine.day.of.year = sin(360/365 * day.of.year)

cosine.day.of.week = cos(360/7 * as.numeric(day.of.week))
sine.day.of.week = sin(360/7 * as.numeric(day.of.week))

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
  sine.day.of.year
)
