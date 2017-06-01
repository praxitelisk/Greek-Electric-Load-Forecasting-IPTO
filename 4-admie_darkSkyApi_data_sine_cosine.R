#this code snippet converts the wind bearing using the sine and cosine 
#functions, this helps to traslate the wind bearing onto the trigonometric circle


library(tibble) #for add_column command

#create cosine and sine columns for wind bearing
cosine.athens.windBearing = cos(darkSky.WeatherData$athens.windBearing)
sine.athens.windBearing = sin(darkSky.WeatherData$athens.windBearing)
cosine.thessaloniki.windBearing = cos(darkSky.WeatherData$thessaloniki.windBearing)
sine.thessaloniki.windBearing = sin(darkSky.WeatherData$thessaloniki.windBearing)


#add new sine cosine columns
darkSky.WeatherData =  add_column(darkSky.WeatherData, sine.athens.windBearing, .after = "athens.windBearing")
darkSky.WeatherData = add_column(darkSky.WeatherData, cosine.athens.windBearing, .after = "sine.athens.windBearing")


#add new sine cosine columns
darkSky.WeatherData = add_column(darkSky.WeatherData, sine.thessaloniki.windBearing, .after = "thessaloniki.windBearing")
darkSky.WeatherData = add_column(darkSky.WeatherData, cosine.thessaloniki.windBearing, .after = "sine.thessaloniki.windBearing")


#remove some auxilliary variables
rm(cosine.athens.windBearing, cosine.thessaloniki.windBearing, sine.thessaloniki.windBearing, sine.athens.windBearing)