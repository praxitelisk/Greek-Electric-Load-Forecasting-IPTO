##########################################################
###Post Process final predictions------------------------
#########################################################


library("lubridate")


splitEvalSet = 365
splitTestSet = splitEvalSet + 365
len = dim(final.Data.Set)[1]


testSet = final.Data.Set[(len - splitEvalSet + 1):len, ]


time = 
  darkSky.N.Loads.Combined$time[which(darkSky.N.Loads.Combined$time == testSet$time[1]):nrow(darkSky.N.Loads.Combined)]

loads = darkSky.N.Loads.Combined$Loads[which(darkSky.N.Loads.Combined$time == testSet$time[1]):nrow(darkSky.N.Loads.Combined)]


mean.mape.ensembling.fs.ms =  100 * mean(abs((loads - prediction.ensembling.fs.ms)/loads))


select_mape = c(mean.mape.svm.fs.ms, 
                mean.mape.xgboost.fs.ms, 
                mean.mape.rule.fs.ms, 
                mean.mape.nn.fs.ms, 
                mean.mape.randomForest.fs.ms,
                mean.mape.knn.fs.ms,
                mean.mape.ensembling.fs.ms)


select_mape_str = c("svm", "xgboost", "rule", "nn", "randomForest", "knn", "ensembling")


#need to add more if-else when nn or ensembling is selected
if (select_mape_str[which.min(select_mape)] == "svm" | 
    select_mape_str[which.min(select_mape)] == "xgboost" | 
    select_mape_str[which.min(select_mape)] == "rule" | 
    select_mape_str[which.min(select_mape)] == "nn" | 
    select_mape_str[which.min(select_mape)] == "randomForest") {
  
  
  prediction = c(
    rbind(
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[1]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[2]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[3]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[4]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[5]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[6]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[7]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[8]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[9]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[10]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[11]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[12]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[13]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[14]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[15]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[16]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[17]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[18]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[19]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[20]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[21]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[22]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[23]],
      get(paste("prediction", select_mape_str[which.min(select_mape)], "fs.ms", sep = "."))[[24]]
    )
  )
  
  
}



df = data.frame("time" = time, "loads" = loads, "prediction" = prediction)
pred.ensembling.df = df



x <- seq(
  as.Date(as.POSIXct(min(pred.ensembling.df$time), tz= "Europe/Athens"), tz= "Europe/Athens"), 
  as.Date(as.POSIXct(max(pred.ensembling.df$time), tz= "Europe/Athens"), tz= "Europe/Athens"),
  by="1 day")



OctoberToBeAdded = x[wday(x,label = TRUE) == "Sun" & day(x) >= 25 & month(x) == 10]
MarchToBeRemoved = x[wday(x,label = TRUE) == "Sun" & day(x) >= 25 & month(x) == 3]


for(i in 1:length(OctoberToBeAdded)) {
  
  remove = which(pred.ensembling.df$time == paste(MarchToBeRemoved[i], "04:00:00"))[2]
  
  pred.ensembling.df = pred.ensembling.df[-remove,]
}


addAfterIndexList = c()
addExtraLoadList = c()
for(i in 1:length(OctoberToBeAdded)) {
  
  
  temp = data.frame(matrix(NA, nrow=1, ncol=length(pred.ensembling.df)))
  colnames(temp)= colnames(pred.ensembling.df)
  temp$time = as.POSIXct(paste(OctoberToBeAdded[i], "03:00:00"))
  #temp$DATE[1] = MarchToBeAdded[i]
  #class(temp$DATE) = class(darkSky.N.Loads.Combined$DATE)
  
  
  addAfterIndex = which(as.character(pred.ensembling.df$time) == paste(OctoberToBeAdded[i], "03:00:00"))
  addAfterIndexList = c(addAfterIndexList, addAfterIndex)
  addExtraLoadList = c(addExtraLoadList, which(as.character(myLoads$time) == paste(OctoberToBeAdded[i], "03:00:00"))[2])
  
  
  pred.ensembling.df = rbind(
    pred.ensembling.df[1:addAfterIndex, ], 
    temp, 
    pred.ensembling.df[(addAfterIndex+1):nrow(pred.ensembling.df), ])
}

addAfterIndexList = addAfterIndexList + 1
for(i in 1:length(addAfterIndexList)) {
  
  pred.ensembling.df[addAfterIndexList[i], 3] =
    mean(c(pred.ensembling.df[(addAfterIndexList[i] + 1), 3],
           pred.ensembling.df[(addAfterIndexList[i] - 1), 3]))
  
  
  pred.ensembling.df[addAfterIndexList[i], 2] = myLoads[addExtraLoadList[i], 4]
}

#calculate mape######################

pred.ensembling.df$ooem.predictions = ooem_predictions$ooem_predictions

rownames(pred.ensembling.df) = 1:dim(pred.ensembling.df)[1]

mape.postprocessed = 100 * mean(abs((pred.ensembling.df$loads - pred.ensembling.df$prediction)/pred.ensembling.df$loads))

mape.ooem = 100 * mean(abs((pred.ensembling.df$loads - pred.ensembling.df$ooem.predictions)/pred.ensembling.df$loads))


pred.ensembling.df["mape"]  = 100 * abs((pred.ensembling.df$loads - pred.ensembling.df$prediction)/pred.ensembling.df$loads)
pred.ensembling.df["mape.ooem"]  = 100 * abs((pred.ensembling.df$loads - pred.ensembling.df$ooem.predictions)/pred.ensembling.df$loads)

#print results#########

cat("mape postprocessed:", mape.postprocessed,"\n")
cat("mape mape.ooem:", mape.ooem,"\n")
cat("mape performance: ", round(100 * (mape.ooem - mape.postprocessed)/ mape.ooem, 3), "%", sep="")


#save data.frames as RDS files############

saveRDS(pred.ensembling.df, "pred.ensembling.df.RDS")
file.copy(from = "pred.ensembling.df.RDS", to = "gui/RDS.Files/pred.ensembling.df.RDS", overwrite = TRUE)
file.remove("pred.ensembling.df.RDS")


saveRDS(darkSky.N.Loads.Combined, "darkSky.N.Loads.Combined.RDS")
file.copy(from = "darkSky.N.Loads.Combined.RDS", to = "gui/RDS.Files/darkSky.N.Loads.Combined.RDS", overwrite = TRUE)
file.remove("darkSky.N.Loads.Combined.RDS")



rm(time, loads, df, x, OctoberToBeAdded, MarchToBeRemoved, temp, addAfterIndex, addAfterIndexList, addExtraLoadList, i, remove)