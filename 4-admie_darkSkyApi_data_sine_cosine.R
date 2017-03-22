library(tibble) #for add_column command

#create cosine and sine columns for wind bearing
cosine.athens.windBearing = cos(darkSky.HistoricalData$athens.windBearing)
sine.athens.windBearing = sin(darkSky.HistoricalData$athens.windBearing)
cosine.thessaloniki.windBearing = cos(darkSky.HistoricalData$thessaloniki.windBearing)
sine.thessaloniki.windBearing = sin(darkSky.HistoricalData$thessaloniki.windBearing)


#add new sine cosine columns
darkSky.HistoricalData =  add_column(darkSky.HistoricalData, sine.athens.windBearing, .after = "athens.windBearing")
darkSky.HistoricalData = add_column(darkSky.HistoricalData, cosine.athens.windBearing, .after = "sine.athens.windBearing")


#add new sine cosine columns
darkSky.HistoricalData = add_column(darkSky.HistoricalData, sine.thessaloniki.windBearing, .after = "thessaloniki.windBearing")
darkSky.HistoricalData = add_column(darkSky.HistoricalData, cosine.thessaloniki.windBearing, .after = "sine.thessaloniki.windBearing")


#remove some auxilliary variables
rm(cosine.athens.windBearing, cosine.thessaloniki.windBearing, sine.thessaloniki.windBearing, sine.athens.windBearing)