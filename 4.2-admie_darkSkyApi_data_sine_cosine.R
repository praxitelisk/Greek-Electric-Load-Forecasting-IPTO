#this code snippet converts the wind bearing using the sine and cosine 
#functions, this helps to traslate the wind bearing onto the trigonometric circle


library(tibble) #for add_column command

###create cosine and sine columns for wind bearing####
athens.windBearing.cosine = cos(darkSky.WeatherData$athens.windBearing)
athens.windBearing.sine = sin(darkSky.WeatherData$athens.windBearing)
thessaloniki.windBearing.cosine = cos(darkSky.WeatherData$thessaloniki.windBearing)
thessaloniki.windBearing.sine = sin(darkSky.WeatherData$thessaloniki.windBearing)


###add new sine cosine columns####
darkSky.WeatherData =  add_column(darkSky.WeatherData, athens.windBearing.sine, .after = "athens.windBearing")
darkSky.WeatherData = add_column(darkSky.WeatherData, athens.windBearing.cosine, .after = "athens.windBearing.sine")


#add new sine cosine columns####
darkSky.WeatherData = add_column(darkSky.WeatherData, thessaloniki.windBearing.sine, .after = "thessaloniki.windBearing")
darkSky.WeatherData = add_column(darkSky.WeatherData, thessaloniki.windBearing.cosine, .after = "thessaloniki.windBearing.sine")


#keep a backup
#backUp.DarkSky = darkSky.WeatherData

#remove some auxilliary variables####
rm(athens.windBearing.cosine, thessaloniki.windBearing.cosine, thessaloniki.windBearing.sine, athens.windBearing.sine)