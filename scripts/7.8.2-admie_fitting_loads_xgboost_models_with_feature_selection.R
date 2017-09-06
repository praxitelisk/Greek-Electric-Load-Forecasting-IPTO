####################################################################
#####xgboost with feature selection and default parameters####
####################################################################

#load the libraries####
library("xgboost")
library("Boruta")

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



mape.xgboost.fs.def = list()
mae.xgboost.fs.def = list()
rmse.xgboost.fs.def = list()
mse.xgboost.fs.def = list()
prediction.xgboost.fs.def = list()
fit.xgboost.fs.def = list()


mean.xgboost.fs.round = 0
for(i in 1:length(best.xgboost.parameters.fs)) {
  mean.xgboost.fs.round = mean.xgboost.fs.round + best.xgboost.parameters.fs[[i]][3]
}
mean.xgboost.fs.round = round(mean.xgboost.fs.round/length(best.xgboost.parameters.fs))


#tuning xgboost parameters per 24hour model####
for(i in 1:24) {
  
  
  cat("\n\n xgboost training  model: Load.",i-1," with default parameters round = ", mean.xgboost.fs.round ," and feature selection \n", sep = "")
  
  list.of.features = getSelectedAttributes(final.boruta.list2[[i]], withTentative = F)
  
  
  #create the predictor variables from training
  FeaturesVariables = 
    train.and.evalSet[list.of.features]
  
  
  dtrain <- xgb.DMatrix(data = data.matrix(FeaturesVariables), label=train.and.evalSet[[paste("Loads", i-1, sep=".")]])
  
  
  #train a model####
  assign(paste("fit.xgboost", i-1, sep="."), 
         xgboost(data = dtrain, nrounds = mean.xgboost.fs.round, nthread = 2, verbose = 0, booster= "gbtree", objective = "reg:linear"))
  
  
  
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
  
  
  
  fit.xgboost.fs.def[[paste("fit.xgboost",i-1,sep=".")]] = get(paste("fit.xgboost",i-1, sep="."))
  
  prediction.xgboost.fs.def[[paste("prediction.xgboost",i-1,sep=".")]] = get(paste("prediction.xgboost",i-1, sep="."))
  
  mape.xgboost.fs.def[[paste("mape.xgboost",i-1,sep=".")]] = temp.mape
  mae.xgboost.fs.def[[paste("mae.xgboost",i-1,sep=".")]] = temp.mae
  mse.xgboost.fs.def[[paste("mse.xgboost",i-1,sep=".")]] = temp.mse
  rmse.xgboost.fs.def[[paste("rmse.xgboost",i-1,sep=".")]] = temp.rmse
  
  
  
  cat("elapsed time in minutes: ", (proc.time()[3] - startTime)/60,"\n")
  
} ##end of models


#calculate the mean mape####
cat("calculate the mean mape\n")
mean.mape.xgboost.fs.def = mean(unlist(mape.xgboost.fs.def))

cat("calculate the mean mae\n")
mean.mae.xgboost.fs.def = mean(unlist(mae.xgboost.fs.def))

cat("calculate the mean mse\n")
mean.mse.xgboost.fs.def = mean(unlist(mse.xgboost.fs.def))

cat("calculate the mean rmse\n")
mean.rmse.xgboost.fs.def = mean(unlist(rmse.xgboost.fs.def))


cat("mean xgboost mape: ", round(mean.mape.xgboost.fs.def, 3), "\n")
cat("mean xgboost mae: ", round(mean.mae.xgboost.fs.def, 5), "\n")
cat("mean xgboost mse: ", round(mean.mse.xgboost.fs.def, 5), "\n")
cat("mean xgboost rmse: ", round(mean.rmse.xgboost.fs.def, 5), "\n")


cat("elapsed time in minutes: ", (proc.time()[3] - startTime)/60,"\n")


rm(list=ls(pattern="fit.xgboost.[0-9]"))
rm(list=ls(pattern="prediction.xgboost.[0-9]"))
rm(list=ls(pattern="mape.xgboost.[0-9]"))
rm(list=ls(pattern="mae.xgboost.[0-9]"))
rm(list=ls(pattern="mse.xgboost.[0-9]"))
rm(list=ls(pattern="rmse.xgboost.[0-9]"))
rm(list=ls(pattern="temp."))
rm(i)