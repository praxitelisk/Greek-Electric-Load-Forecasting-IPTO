startTime <- proc.time()[3]

getForcastForDate="2010-10-08"
gerForcastUntilDate="2010-11-08"

YesterdayData = finalDataFrame[as.Date(as.POSIXct(finalDataFrame$time, tz=Sys.timezone(location = TRUE)), tz=Sys.timezone(location = TRUE))==(as.Date(getForcastForDate)-1),]
the.Day.Before.YesterdayData = finalDataFrame[as.Date(as.POSIXct(finalDataFrame$time, tz=Sys.timezone(location = TRUE)), tz=Sys.timezone(location = TRUE))==(as.Date(getForcastForDate)-2),]
todayData = finalDataFrame[as.Date(as.POSIXct(finalDataFrame$time, tz=Sys.timezone(location = TRUE)), tz=Sys.timezone(location = TRUE))==(as.Date(getForcastForDate)),]

todayLoads = (unlist(todayData[grep("Loads", colnames(todayData))]))
todayWeather = todayData[-grep("Loads", colnames(todayData))]

yesterdayLoads = (unlist(YesterdayData[grep("Loads", colnames(YesterdayData))]))
yesterdayWeather = YesterdayData[-grep("Loads", colnames(YesterdayData))]

the.Day.Before.YesterdayLoads = (unlist(the.Day.Before.YesterdayData[grep("Loads", colnames(the.Day.Before.YesterdayData))]))
the.Day.Before.YesterdayWeather = (the.Day.Before.YesterdayData[-grep("Loads", colnames(the.Day.Before.YesterdayData))])



fit = step(
  lm(
    todayLoads ~ 
      YesterdayLoads 
    + the.Day.Before.YesterdayLoads 
    + unlist(todayWeather[grep("athens.temperature", colnames(todayWeather))])
    + unlist(todayWeather[grep("thessaloniki.temperature", colnames(todayWeather))]) 
    + unlist(todayWeather[grep("athens.dewPoint", colnames(todayWeather))])
    + unlist(todayWeather[grep("thessaloniki.dewPoint", colnames(todayWeather))])
    + unlist(todayWeather[grep("athens.humidity", colnames(todayWeather))])
    + unlist(todayWeather[grep("thessaloniki.humidity", colnames(todayWeather))])
    + unlist(todayWeather[grep("athens.visibility", colnames(todayWeather))])
    + unlist(todayWeather[grep("athens.cloudCover", colnames(todayWeather))])
    + unlist(todayWeather[grep("thessaloniki.cloudCover", colnames(todayWeather))])
    + unlist(yesterdayWeather[grep("athens.temperature", colnames(yesterdayWeather))])
    + unlist(yesterdayWeather[grep("thessaloniki.temperature", colnames(yesterdayWeather))])
    + unlist(yesterdayWeather[grep("athens.dewPoint", colnames(yesterdayWeather))])
    + unlist(yesterdayWeather[grep("thessaloniki.dewPoint", colnames(yesterdayWeather))])
    + unlist(yesterdayWeather[grep("athens.humidity", colnames(yesterdayWeather))])
    + unlist(yesterdayWeather[grep("thessaloniki.humidity", colnames(yesterdayWeather))])
    + unlist(yesterdayWeather[grep("athens.visibility", colnames(yesterdayWeather))])
    + unlist(yesterdayWeather[grep("thessaloniki.visibility", colnames(yesterdayWeather))])
    + unlist(yesterdayWeather[grep("athens.cloudCover", colnames(yesterdayWeather))])
    + unlist(yesterdayWeather[grep("thessaloniki.cloudCover", colnames(yesterdayWeather))])
    + unlist(the.Day.Before.YesterdayWeather[grep("athens.temperature", colnames(the.Day.Before.YesterdayWeather))])
    
    
    )
)
  
  
print(summary(fit))

cat("elapsed time in seconds: ", (proc.time()[3]-startTime))


# YesterdayDataLoads00 = rep(YesterdayDataLoads["Loads.Mwh"], length(todayDataLoads))
# YesterdayDataLoads01 = rep(YesterdayDataLoads["Loads.Mwh.1"], length(todayDataLoads))
# YesterdayDataLoads02 = rep(YesterdayDataLoads["Loads.Mwh.2"], length(todayDataLoads))
# YesterdayDataLoads03 = rep(YesterdayDataLoads["Loads.Mwh.3"], length(todayDataLoads))
# YesterdayDataLoads04 = rep(YesterdayDataLoads["Loads.Mwh.4"], length(todayDataLoads))
# YesterdayDataLoads05 = rep(YesterdayDataLoads["Loads.Mwh.5"], length(todayDataLoads))
# YesterdayDataLoads06 = rep(YesterdayDataLoads["Loads.Mwh.6"], length(todayDataLoads))
# YesterdayDataLoads07 = rep(YesterdayDataLoads["Loads.Mwh.7"], length(todayDataLoads))
# YesterdayDataLoads08 = rep(YesterdayDataLoads["Loads.Mwh.8"], length(todayDataLoads))
# YesterdayDataLoads09 = rep(YesterdayDataLoads["Loads.Mwh.9"], length(todayDataLoads))
# YesterdayDataLoads10 = rep(YesterdayDataLoads["Loads.Mwh.10"], length(todayDataLoads))
# YesterdayDataLoads11 = rep(YesterdayDataLoads["Loads.Mwh.11"], length(todayDataLoads))
# YesterdayDataLoads12 = rep(YesterdayDataLoads["Loads.Mwh.12"], length(todayDataLoads))
# YesterdayDataLoads13 = rep(YesterdayDataLoads["Loads.Mwh.13"], length(todayDataLoads))
# YesterdayDataLoads14 = rep(YesterdayDataLoads["Loads.Mwh.14"], length(todayDataLoads))
# YesterdayDataLoads15 = rep(YesterdayDataLoads["Loads.Mwh.15"], length(todayDataLoads))
# YesterdayDataLoads16 = rep(YesterdayDataLoads["Loads.Mwh.16"], length(todayDataLoads))
# YesterdayDataLoads17 = rep(YesterdayDataLoads["Loads.Mwh.17"], length(todayDataLoads))
# YesterdayDataLoads18 = rep(YesterdayDataLoads["Loads.Mwh.18"], length(todayDataLoads))
# YesterdayDataLoads19 = rep(YesterdayDataLoads["Loads.Mwh.19"], length(todayDataLoads))
# YesterdayDataLoads20 = rep(YesterdayDataLoads["Loads.Mwh.20"], length(todayDataLoads))
# YesterdayDataLoads21 = rep(YesterdayDataLoads["Loads.Mwh.21"], length(todayDataLoads))
# YesterdayDataLoads22 = rep(YesterdayDataLoads["Loads.Mwh.22"], length(todayDataLoads))
# YesterdayDataLoads23 = rep(YesterdayDataLoads["Loads.Mwh.23"], length(todayDataLoads))
# 
# the.Day.Before.YesterdayDataLoads00 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads01 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.1"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads02 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.2"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads03 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.3"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads04 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.4"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads05 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.5"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads06 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.6"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads07 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.7"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads08 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.8"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads09 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.9"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads10 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.10"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads11 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.11"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads12 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.12"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads13 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.13"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads14 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.14"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads15 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.15"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads16 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.16"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads17 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.17"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads18 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.18"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads19 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.19"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads20 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.20"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads21 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.21"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads22 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.22"], length(todayDataLoads))
# the.Day.Before.YesterdayDataLoads23 = rep(the.Day.Before.YesterdayDataLoads["Loads.Mwh.23"], length(todayDataLoads))
