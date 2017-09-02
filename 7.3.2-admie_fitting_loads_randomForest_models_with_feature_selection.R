###################################################################
#####randomForest with feature selection and default parameters####
###################################################################


library("randomForest")
library("Boruta")


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


mape.randomForest.fs.def = list()
mae.randomForest.fs.def = list()
rmse.randomForest.fs.def = list()
mse.randomForest.fs.def = list()
prediction.randomForest.fs.def = list()
fit.randomForest.fs.def = list()


for(i in 1:24) {
  
  list.of.features =
    getSelectedAttributes(final.boruta.list2[[i]], withTentative = F)
  
  cat("\n\n training model: Load.",i-1," with feature selection and default parameters \n", sep="")
  
  #create the predictor variables from training
  FeaturesVariables =
    train.and.evalSet[list.of.features]
  
  
  FeaturesVariables[paste("Loads", i-1, sep=".")] = 
    train.and.evalSet[paste("Loads", i-1, sep=".")]
  
  
  set.seed(123)
  assign(paste("fit.randomForest", i-1, sep="."), 
         randomForest(as.formula(paste("Loads.", i-1, "~.", sep="")), data = FeaturesVariables))
  
  
  FeaturesVariables[paste("Loads", i-1, sep=".")] = NULL
  
  
  FeaturesVariables =
    train.and.evalSet[list.of.features]
  
  predictor.df = data.frame()
  predictor.df = FeaturesVariables[0, ]
  predictor.df = rbind(predictor.df, testSet[names(testSet) %in% names(predictor.df)])
  
  
  assign(paste("prediction.randomForest", i-1, sep="."), predict(get(paste("fit.randomForest",i-1,sep=".")), predictor.df))
  
  #calculate mape
  temp.mape = 100 * mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.randomForest", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")])))
  cat("mape.", i-1 ," = ", temp.mape,"\n\n", sep = "")
  
  
  temp.mae =  mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.randomForest", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")])))
  
  
  temp.rmse = sqrt(mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.randomForest", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")]))^2))
  
  
  temp.mse = mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.randomForest", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")]))^2)
  
  
  fit.randomForest.fs.def[[paste("fit.randomForest",i-1,sep=".")]] = get(paste("fit.randomForest",i-1, sep="."))
  
  prediction.randomForest.fs.def[[paste("prediction.randomForest",i-1,sep=".")]] = get(paste("prediction.randomForest",i-1, sep="."))
  
  mape.randomForest.fs.def[[paste("mape.randomForest",i-1,sep=".")]] = temp.mape
  mae.randomForest.fs.def[[paste("mae.randomForest",i-1,sep=".")]] = temp.mae
  mse.randomForest.fs.def[[paste("mse.randomForest",i-1,sep=".")]] = temp.mse
  rmse.randomForest.fs.def[[paste("rmse.randomForest",i-1,sep=".")]] = temp.rmse  
  
  
}


#calculate the mean mape####
cat("calculate the mean mape\n")
mean.mape.randomForest.fs.def = mean(unlist(mape.randomForest.fs.def))

cat("calculate the mean mae\n")
mean.mae.randomForest.fs.def = mean(unlist(mae.randomForest.fs.def))

cat("calculate the mean mse\n")
mean.mse.randomForest.fs.def = mean(unlist(mse.randomForest.fs.def))

cat("calculate the mean rmse\n")
mean.rmse.randomForest.fs.def = mean(unlist(rmse.randomForest.fs.def))


cat("mean randomForest mape: ", round(mean.mape.randomForest.fs.def,3), "\n")
cat("mean randomForest mae: ", round(mean.mae.randomForest.fs.def,5), "\n")
cat("mean randomForest mse: ", round(mean.mse.randomForest.fs.def,5), "\n")
cat("mean randomForest rmse: ", round(mean.rmse.randomForest.fs.def,5), "\n")


cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60,"\n")



rm(list=ls(pattern="fit.randomForest.[0-9]"))
rm(list=ls(pattern="prediction.randomForest.[0-9]"))
rm(list=ls(pattern="mape.randomForest.[0-9]"))
rm(list=ls(pattern="mae.randomForest.[0-9]"))
rm(list=ls(pattern="mse.randomForest.[0-9]"))
rm(list=ls(pattern="rmse.randomForest.[0-9]"))
rm(list=ls(pattern="min.mape."))
rm(list=ls(pattern="temp."))
rm(i)