library("jsonlite")

startTime <- proc.time()[3]

list.of.api.keys = c("b8c4c289e035b3024a466e8d886b3fa9",
                     "06626db0344ec2b98259bf1efeda8cae",
                     "ec694749e1c8a8b9d716bf73e369bcce",
                     "18f9e95a36795766d7bd4d684c93df55",
                     "f3e286a8dfc0bd86b9cd92462da3cc3e",
                     "540ff06bad91d0accb73f6b259fabbe8",
                     "c958e254bbb4c87dbf76e9b006eae6df",
                     "4af9ccdc88e8fc385fd72bc2c71c704d",
                     "9a690d76a4f70ca8a58f92451333ce51",
                     "a18173bef1fb9cec7bb15bfde42d2028",
                     "a9d04a5e038b9198b8b38484562ee09e",
                     "482b5abd9d0976f99ead7d606443bff7",
                     "f5941de10a5dcfb2445c9b5a0699417a",
                     "97a5a043ef583d970e628e8649abd3bc",
                     "837efbc72aaaec75644a22c1488ecbcf",
                     "680e0da95389cfe69760a38af253617c")


darkSky.WeatherData.temp <- list()

api.key.calls = 1
api.key.index = 1

days <- seq(from=as.Date('2010-10-01'), to=as.Date("2017-05-01"),by='days' )

for ( i in seq_along(days) ) {
  
  
  url.athens  = paste(
    "https://api.darksky.net/forecast/",
    list.of.api.keys[api.key.index],
    "/37.9841,23.728,",
    paste0(days[i],"T12:00:00-0200"),
    "?units=si",
    sep = ""
  )
  
  url.thess = paste(
    "https://api.darksky.net/forecast/",
    list.of.api.keys[api.key.index],
    "/40.6403,22.9353,",
    paste0(days[i],"T12:00:00-0200"),
    "?units=si",
    sep = ""
  ) 
  
  
  json.df.athens = fromJSON(url.athens)
  json.df.athens = as.data.frame(json.df.athens$hourly)
  
  json.df.athens[,1] = NULL
  json.df.athens[,1] = NULL
  
  
  json.df.thess = fromJSON(url.thess)
  json.df.thess = as.data.frame(json.df.thess$hourly)
  
  json.df.thess[,1] = NULL
  json.df.thess[,1] = NULL
  
  colnames(json.df.athens) = gsub("data.", "", colnames(json.df.athens))
  colnames(json.df.thess) = gsub("data.", "", colnames(json.df.thess))
  
  
  json.df.athens$time = as.POSIXct(json.df.athens$time, origin="1970-01-01", tz = Sys.timezone())
  
  json.df.thess$time = as.POSIXct(json.df.thess$time, origin="1970-01-01", tz = Sys.timezone())
  
  
  aux_day = seq(
    from=as.POSIXct(paste(days[i]," 00:00", sep=" "), tz = Sys.timezone()),
    to=as.POSIXct(paste(days[i]," 23:00", sep=" "), tz = Sys.timezone()),
    by="hour"
  )

  ##resolving the October's 25 hour day (daylight saving)####
  if(length(aux_day) == 25)

    #remove the extra 3:00 o' clock
    aux_day = aux_day[-5]

  day = data.frame("time" = aux_day)
  colnames(day) = "time"


  json.df.athens = merge(day, json.df.athens, by="time", all.x =TRUE)

  json.df.thess = merge(day, json.df.thess, by="time", all.x =TRUE)
  
  
  
  if(nrow(json.df.athens) == 24 & nrow(json.df.thess) == 24) {
    
    #change the name of columns for Athens' weather attributes####
    colnames(json.df.athens) <- paste0("athens.", colnames(json.df.athens))
    colnames(json.df.athens)[1] = "time"
    
    #change the name of columns for Thessaloniki's weather attributes####
    json.df.thess$time = NULL
    colnames(json.df.thess) <- paste0("thessaloniki.", colnames(json.df.thess))
    
    
    #save both cities' weather data in a common list####
    temp <- cbind(json.df.athens, json.df.thess)
    
    #get names of all the weekdays####
    test = cbind(weekdays(temp$time), temp)
    colnames(test)[1] = "weekday"
    
    #concat it in the main data frame table####
    darkSky.WeatherData.temp <- rbind(darkSky.WeatherData.temp, test)
    
    #checking api's counters####
    api.key.calls = api.key.calls + 2
    
    if(api.key.calls > (2 * 365) ) {
      
      cat("change api key, api.key.calls: ", api.key.calls, "new api.key.index: ", api.key.index + 1,"\n")
      api.key.calls = 1
      api.key.index = api.key.index + 1
    }
  }
  
}

#remove some columns with no information####
darkSky.WeatherData.temp = 
  darkSky.WeatherData.temp[-grep("pressure",names(darkSky.WeatherData.temp))]

darkSky.WeatherData.temp = 
  darkSky.WeatherData.temp[-grep("precipType",names(darkSky.WeatherData.temp))]

#create a backup####
backUp.DarkSky = darkSky.WeatherData

cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60)


rm(json.df.athens, json.df.thess, test, temp, days, aux_day, day, url.athens, url.thess, api.key.calls, api.key.index, list.of.api.keys, i)