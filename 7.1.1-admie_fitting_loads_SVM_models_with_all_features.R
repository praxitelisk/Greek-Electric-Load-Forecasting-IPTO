##########################################################
#####SVM with full features and default parameters####
##########################################################

#load the libraries####
library("Boruta")
library("e1071")

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



mape.svm.full.def = list()
mae.svm.full.def = list()
rmse.svm.full.def = list()
mse.svm.full.def = list()
prediction.svm.full.def = list()
fit.svm.full.def = list()

#tuning svm parameters per 24hour model####
for(i in 1:24) {
  
  
  cat("\n\n training model: Load.",i-1, " \n", sep = "")
  
  list.of.features = full.list.of.features
  
  
  #create the predictor variables from training
  FeaturesVariables = 
    train.and.evalSet[list.of.features]
  
  
  #add the response variable in trainSet
  FeaturesVariables[paste("Loads", i-1, sep=".")] = 
    train.and.evalSet[paste("Loads", i-1, sep=".")]
  
  
  #train a model####
  assign(paste("fit.svm", i-1, sep="."), 
         svm(as.formula(paste("Loads.", i-1, "~.", sep="")), data = FeaturesVariables))
  
  
  FeaturesVariables[paste("Loads", i-1, sep=".")] = NULL
  
  
  
  #create the predictor.df data.frame for predictions####
  FeaturesVariables = 
    train.and.evalSet[list.of.features]
  
  predictor.df = data.frame()
  predictor.df = FeaturesVariables[0, ]
  predictor.df = rbind(predictor.df, testSet[names(testSet) %in% names(predictor.df)])
  
  
  #make the prediction
  assign(paste("prediction.svm", i-1, sep="."), predict(get(paste("fit.svm",i-1,sep=".")), predictor.df))
  
  
  #calculate mape
  temp.mape = 100 * mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.svm", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")])))
  cat("mape = ", temp.mape,"\n\n")
  
  
  temp.mae =  mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.svm", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")])))
  
  
  temp.rmse = sqrt(mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.svm", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")]))^2))
  
  
  temp.mse = mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.svm", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")]))^2)
  
  
  assign(paste("mape.svm",i-1,sep="."), temp.mape)
  assign(paste("mae.svm",i-1,sep="."), temp.mae)
  assign(paste("rmse.svm",i-1,sep="."), temp.rmse)
  assign(paste("mse.svm",i-1,sep="."), temp.mse)
  
  
  
  fit.svm.full.def[[paste("fit.svm",i-1,sep=".")]] = get(paste("fit.svm",i-1, sep="."))
  
  prediction.svm.full.def[[paste("prediction.svm",i-1,sep=".")]] = get(paste("prediction.svm",i-1, sep="."))
  
  mape.svm.full.def[[paste("mape.svm",i-1,sep=".")]] = temp.mape
  mae.svm.full.def[[paste("mae.svm",i-1,sep=".")]] = temp.mae
  mse.svm.full.def[[paste("mse.svm",i-1,sep=".")]] = temp.mse
  rmse.svm.full.def[[paste("rmse.svm",i-1,sep=".")]] = temp.rmse
  
  
  
  
  
  cat("elapsed time in minutes: ", (proc.time()[3] - startTime)/60,"\n")
  
} ##end of models


#calculate the mean mape####
cat("calculate the mean mape\n")
mean.mape.svm.full.def = mean(unlist(mape.svm.full.def))

cat("calculate the mean mae\n")
mean.mae.svm.full.def = mean(unlist(mae.svm.full.def))

cat("calculate the mean mse\n")
mean.mse.svm.full.def = mean(unlist(mse.svm.full.def))

cat("calculate the mean rmse\n")
mean.rmse.svm.full.def = mean(unlist(rmse.svm.full.def))


cat("mean svm mape: ", round(mean.mape.svm.full.def, 3), "\n")
cat("mean svm mae: ", round(mean.mae.svm.full.def, 5), "\n")
cat("mean svm mse: ", round(mean.mse.svm.full.def, 5), "\n")
cat("mean svm rmse: ", round(mean.rmse.svm.full.def, 5), "\n")


cat("elapsed time in minutes: ", (proc.time()[3] - startTime)/60,"\n")


rm(list=ls(pattern="fit.svm.[0-9]"))
rm(list=ls(pattern="prediction.svm.[0-9]"))
rm(list=ls(pattern="mape.svm.[0-9]"))
rm(list=ls(pattern="mae.svm.[0-9]"))
rm(list=ls(pattern="mse.svm.[0-9]"))
rm(list=ls(pattern="rmse.svm.[0-9]"))
rm(list=ls(pattern="temp."))
rm(i)