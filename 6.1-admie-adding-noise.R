
final.Data.Set = backUp.final.Data.Set

#get current day features####
current.day.list.of.features = 
  colnames(final.Data.Set)[-grep("^thessaloniki.icon|^athens.icon|^thessaloniki.summary|^athens.summary|^isHoliday|^isWeekend|^isRushHour|^cosine.day.of.year.0|^sine.day.of.year.0|^day.of.year.0|^cosine.day.of.week.0|^sine.day.of.week.0|^day.of.week.0|^weekday.0|^time.0|^yesterday|^three|^two|^Loads|^thessaloniki.uvIndex|^athens.uvIndex", colnames(final.Data.Set))]


#change the names for current loads adding the prefix forecast.####
colnames(final.Data.Set)[grep(paste("^", current.day.list.of.features, collapse = "|", sep = ""), colnames(final.Data.Set))] = 
  paste("forecast", colnames(final.Data.Set)[grep(paste("^", current.day.list.of.features, collapse = "|", sep = ""), colnames(final.Data.Set))], sep = ".")


#adding noise from uniform distribution####
final.Data.Set[grep("^forecast", colnames(final.Data.Set))] = 
  round(final.Data.Set[grep("^forecast", colnames(final.Data.Set))] +
          runif(dim(final.Data.Set[grep("^forecast", colnames(final.Data.Set))])[1], -0.1, 0.1), 3)


#recalculate the sine and cosine from windbearing####
for(i in 1:24) {
  final.Data.Set[paste("forecast.sine.thessaloniki.windBearing", i-1, sep=".")] = 
    sin(final.Data.Set[paste("forecast.thessaloniki.windBearing", i-1, sep=".")])
  
  final.Data.Set[paste("forecast.cosine.thessaloniki.windBearing", i-1, sep=".")] = 
    cos(final.Data.Set[paste("forecast.thessaloniki.windBearing", i-1, sep=".")])
  
  final.Data.Set[paste("forecast.sine.athens.windBearing", i-1, sep=".")] = 
    sin(final.Data.Set[paste("forecast.athens.windBearing", i-1, sep=".")])
  
  final.Data.Set[paste("forecast.cosine.athens.windBearing", i-1, sep=".")] = 
    cos(final.Data.Set[paste("forecast.athens.windBearing", i-1, sep=".")])
}

rm(current.day.list.of.features, i)