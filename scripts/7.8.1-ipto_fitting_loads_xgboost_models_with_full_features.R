##########################################################
#####xgboost with full features and default parameters####
##########################################################

#load the libraries####
library("xgboost")

#start measuring time#####
startTime <- proc.time()[3]

#creating the train and test set splits####
splitEvalSet = 365
splitTestSet = splitEvalSet + 365
len = dim(final.Data.Set)[1]

#trainPart = floor(split * dim(final.Data.Set)[1])
trainSet = final.Data.Set[1:(len - splitTestSet), ]
evaluationSet = final.Data.Set[(len-splitTestSet + 1):(len - splitEvalSet), ]
train.and.evalSet = final.Data.Set[1:(len - splitEvalSet), ]
testSet = final.Data.Set[(len - splitEvalSet + 1):len, ]



mape.xgboost.full.def = list()
mae.xgboost.full.def = list()
rmse.xgboost.full.def = list()
mse.xgboost.full.def = list()
prediction.xgboost.full.def = list()
fit.xgboost.full.def = list()


mean.xgboost.full.round = 0
for(i in 1:length(best.xgboost.parameters.full)) {
  mean.xgboost.full.round = mean.xgboost.full.round + best.xgboost.parameters.full[[i]][3]
}
mean.xgboost.full.round = round(mean.xgboost.full.round/length(best.xgboost.parameters.full))


#tuning xgboost parameters per 24hour model####
for(i in 1:24) {
  
  
  cat("\n\n xgboost training  model: Load.",i-1," with default parameters round = ", mean.xgboost.full.round ," and full features \n", sep = "")
  
  list.of.features = full.list.of.features
  
  
  #create the predictor variables from training
  FeaturesVariables = 
    train.and.evalSet[list.of.features]
  
  
  dtrain <- xgb.DMatrix(data = data.matrix(FeaturesVariables), label=train.and.evalSet[[paste("Loads", i-1, sep=".")]])
  
  
  #train a model####
  assign(paste("fit.xgboost", i-1, sep="."), 
         xgboost(data = dtrain, nrounds = mean.xgboost.full.round, nthread = 2, verbose = 0, booster= "gbtree", objective = "reg:linear"))
  
  
  
  #create the predictor.df data.frame for predictions####
  FeaturesVariables = 
    train.and.evalSet[list.of.features]
  
  predictor.df = data.frame()
  predictor.df = FeaturesVariables[0, ]
  predictor.df = rbind(predictor.df, testSet[names(testSet) %in% names(predictor.df)])
  
  
  #make the prediction
  assign(paste("prediction.xgboost", i-1, sep="."), predict(get(paste("fit.xgboost",i-1,sep=".")), data.matrix(predictor.df)))
  
  
  #calculate mape
  temp.mape = 100 * mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.xgboost", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")])))
  cat("mape = ", temp.mape,"\n\n")
  
  
  temp.mae =  mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.xgboost", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")])))
  
  
  temp.rmse = sqrt(mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.xgboost", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")]))^2))
  
  
  temp.mse = mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.xgboost", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")]))^2)
  
  
  assign(paste("mape.xgboost",i-1,sep="."), temp.mape)
  assign(paste("mae.xgboost",i-1,sep="."), temp.mae)
  assign(paste("rmse.xgboost",i-1,sep="."), temp.rmse)
  assign(paste("mse.xgboost",i-1,sep="."), temp.mse)
  
  
  
  fit.xgboost.full.def[[paste("fit.xgboost",i-1,sep=".")]] = get(paste("fit.xgboost",i-1, sep="."))
  
  prediction.xgboost.full.def[[paste("prediction.xgboost",i-1,sep=".")]] = get(paste("prediction.xgboost",i-1, sep="."))
  
  mape.xgboost.full.def[[paste("mape.xgboost",i-1,sep=".")]] = temp.mape
  mae.xgboost.full.def[[paste("mae.xgboost",i-1,sep=".")]] = temp.mae
  mse.xgboost.full.def[[paste("mse.xgboost",i-1,sep=".")]] = temp.mse
  rmse.xgboost.full.def[[paste("rmse.xgboost",i-1,sep=".")]] = temp.rmse
  
  
  
  cat("elapsed time in minutes: ", (proc.time()[3] - startTime)/60,"\n")
  
} ##end of models


#calculate the mean mape####
cat("calculate the mean mape\n")
mean.mape.xgboost.full.def = mean(unlist(mape.xgboost.full.def))

cat("calculate the mean mae\n")
mean.mae.xgboost.full.def = mean(unlist(mae.xgboost.full.def))

cat("calculate the mean mse\n")
mean.mse.xgboost.full.def = mean(unlist(mse.xgboost.full.def))

cat("calculate the mean rmse\n")
mean.rmse.xgboost.full.def = mean(unlist(rmse.xgboost.full.def))


cat("mean xgboost mape: ", round(mean.mape.xgboost.full.def, 3), "\n")
cat("mean xgboost mae: ", round(mean.mae.xgboost.full.def, 5), "\n")
cat("mean xgboost mse: ", round(mean.mse.xgboost.full.def, 5), "\n")
cat("mean xgboost rmse: ", round(mean.rmse.xgboost.full.def, 5), "\n")


cat("elapsed time in minutes: ", (proc.time()[3] - startTime)/60,"\n")


rm(list=ls(pattern="fit.xgboost.[0-9]"))
rm(list=ls(pattern="prediction.xgboost.[0-9]"))
rm(list=ls(pattern="mape.xgboost.[0-9]"))
rm(list=ls(pattern="mae.xgboost.[0-9]"))
rm(list=ls(pattern="mse.xgboost.[0-9]"))
rm(list=ls(pattern="rmse.xgboost.[0-9]"))
rm(list=ls(pattern="temp."))
rm(i)
rm(dtrain)