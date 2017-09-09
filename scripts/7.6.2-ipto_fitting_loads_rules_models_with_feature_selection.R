####################################################################
#####xgboost with feature selection and default parameters####
####################################################################


library("Cubist")
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


#if (!exists("best.rule.parameters.fs")) {
best.rule.parameters.fs = list()
best.rule.fit.fs = list()
best.rule.prediction.fs = list()
#}

mape.rule.fs.def = list()
mae.rule.fs.def = list()
rmse.rule.fs.def = list()
mse.rule.fs.def = list()
prediction.rule.fs.def = list()
fit.rule.fs.def = list()


for(i in 1:24) {
  
  list.of.features =
    getSelectedAttributes(final.boruta.list2[[i]], withTentative = F)
  
  cat("\n\n training rules model: Load.",i-1, "\n", sep="")
  
  #create the predictor variables from training
  FeaturesVariables =
    train.and.evalSet[list.of.features]
  
  #add the response variable in trainSet
  FeaturesVariables[paste("Loads", i-1, sep=".")] = 
    train.and.evalSet[paste("Loads", i-1, sep=".")]
  
  
  set.seed(123)
  assign(paste("fit.rule", i-1, sep="."), 
         cubist(x = FeaturesVariables[-grep(paste("^Loads", i-1, sep="."), names(FeaturesVariables))], y = FeaturesVariables[[paste("Loads", i-1, sep=".")]]))
  
  
  FeaturesVariables[paste("Loads", i-1, sep=".")] = NULL
  
  
  FeaturesVariables =
    train.and.evalSet[list.of.features]
  
  
  predictor.df = data.frame()
  predictor.df = FeaturesVariables[0, ]
  predictor.df = rbind(predictor.df, testSet[names(testSet) %in% names(predictor.df)])
  
  
  assign(paste("prediction.rule", i-1, sep="."), predict(get(paste("fit.rule",i-1,sep=".")), predictor.df))
  
  
  #calculate mape
  temp.mape = 100 * mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.rule", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")])))
  cat("mape.", i-1 ," = ", temp.mape,"\n\n", sep = "")
  
  
  temp.mae =  mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.rule", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")])))
  
  
  temp.rmse = sqrt(mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.rule", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")]))^2))
  
  
  temp.mse = mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.rule", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")]))^2)
  
  
  fit.rule.fs.def[[paste("fit.rule",i-1,sep=".")]] = get(paste("fit.rule",i-1, sep="."))
  
  prediction.rule.fs.def[[paste("prediction.rule",i-1,sep=".")]] = get(paste("prediction.rule",i-1, sep="."))
  
  mape.rule.fs.def[[paste("mape.rule",i-1,sep=".")]] = temp.mape
  mae.rule.fs.def[[paste("mae.rule",i-1,sep=".")]] = temp.mae
  mse.rule.fs.def[[paste("mse.rule",i-1,sep=".")]] = temp.mse
  rmse.rule.fs.def[[paste("rmse.rule",i-1,sep=".")]] = temp.rmse
  
  
}


#calculate the mean mape####
cat("calculate the mean mape\n")
mean.mape.rule.fs.def = mean(unlist(mape.rule.fs.def))

cat("calculate the mean mae\n")
mean.mae.rule.fs.def = mean(unlist(mae.rule.fs.def))

cat("calculate the mean mse\n")
mean.mse.rule.fs.def = mean(unlist(mse.rule.fs.def))

cat("calculate the mean rmse\n")
mean.rmse.rule.fs.def = mean(unlist(rmse.rule.fs.def))


cat("mean rule mape: ", round(mean.mape.rule.fs.def,3), "\n")
cat("mean rule mae: ", round(mean.mae.rule.fs.def,5), "\n")
cat("mean rule mse: ", round(mean.mse.rule.fs.def,5), "\n")
cat("mean rule rmse: ", round(mean.rmse.rule.fs.def,5), "\n")


cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60,"\n")



rm(list=ls(pattern="fit.rule.[0-9]"))
rm(list=ls(pattern="prediction.rule.[0-9]"))
rm(list=ls(pattern="mape.rule.[0-9]"))
rm(list=ls(pattern="mae.rule.[0-9]"))
rm(list=ls(pattern="mse.rule.[0-9]"))
rm(list=ls(pattern="rmse.rule.[0-9]"))
rm(list=ls(pattern="min.mape."))
rm(list=ls(pattern="temp."))
rm(i)

