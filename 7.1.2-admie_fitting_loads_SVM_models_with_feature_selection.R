##########################################################
#####SVM with feature selection and default parameters####
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



mape.svm.fs.def = list()
mae.svm.fs.def = list()
rmse.svm.fs.def = list()
mse.svm.fs.def = list()
prediction.svm.fs.def = list()
fit.svm.fs.def = list()


# mean.svm.fs.gamma = 0
# for(i in 1:length(best.svm.parameters.fs)) {
#   mean.svm.fs.gamma = mean.svm.fs.gamma + best.svm.parameters.fs[[i]][1]
# }
# mean.svm.fs.gamma = mean.svm.fs.gamma/length(best.svm.parameters.fs)
# 
# 
# mean.svm.fs.cost = 0
# for(i in 1:length(best.svm.parameters.fs)) {
#   mean.svm.fs.cost = mean.svm.fs.cost + best.svm.parameters.fs[[i]][2]
# }
# mean.svm.fs.cost = round(mean.svm.fs.cost/length(best.svm.parameters.fs),3)


#tuning svm parameters per 24hour model####
for(i in 1:24) {
  

  cat("\n\n svm training  model: Load.",i-1," with default parameters gamma = ", mean.svm.fs.gamma ," cost = ", mean.svm.fs.cost ," and feature selection \n", sep = "")
  
  list.of.features = 
    getSelectedAttributes(final.boruta.list2[[i]], withTentative = F)
  
  
  #create the predictor variables from training
  FeaturesVariables = 
    train.and.evalSet[list.of.features]
  
  
  #add the response variable in trainSet
  FeaturesVariables[paste("Loads", i-1, sep=".")] = 
    train.and.evalSet[paste("Loads", i-1, sep=".")]
  
  
  #train a model####
  set.seed(123)
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
  
  

  fit.svm.fs.def[[paste("fit.svm",i-1,sep=".")]] = get(paste("fit.svm",i-1, sep="."))
  
  prediction.svm.fs.def[[paste("prediction.svm",i-1,sep=".")]] = get(paste("prediction.svm",i-1, sep="."))
  
  mape.svm.fs.def[[paste("mape.svm",i-1,sep=".")]] = temp.mape
  mae.svm.fs.def[[paste("mae.svm",i-1,sep=".")]] = temp.mae
  mse.svm.fs.def[[paste("mse.svm",i-1,sep=".")]] = temp.mse
  rmse.svm.fs.def[[paste("rmse.svm",i-1,sep=".")]] = temp.rmse

  
  cat("elapsed time in minutes: ", (proc.time()[3] - startTime)/60,"\n")
  
} ##end of models


#calculate the mean mape####
cat("calculate the mean mape\n")
mean.mape.svm.fs.def = mean(unlist(mape.svm.fs.def))

cat("calculate the mean mae\n")
mean.mae.svm.fs.def = mean(unlist(mae.svm.fs.def))

cat("calculate the mean mse\n")
mean.mse.svm.fs.def = mean(unlist(mse.svm.fs.def))

cat("calculate the mean rmse\n")
mean.rmse.svm.fs.def = mean(unlist(rmse.svm.fs.def))


cat("mean svm mape: ", round(mean.mape.svm.fs.def, 3), "\n")
cat("mean svm mae: ", round(mean.mae.svm.fs.def, 5), "\n")
cat("mean svm mse: ", round(mean.mse.svm.fs.def, 5), "\n")
cat("mean svm rmse: ", round(mean.rmse.svm.fs.def, 5), "\n")


cat("elapsed time in minutes: ", (proc.time()[3] - startTime)/60,"\n")


rm(list=ls(pattern="fit.svm.[0-9]"))
rm(list=ls(pattern="prediction.svm.[0-9]"))
rm(list=ls(pattern="mape.svm.[0-9]"))
rm(list=ls(pattern="mae.svm.[0-9]"))
rm(list=ls(pattern="mse.svm.[0-9]"))
rm(list=ls(pattern="rmse.svm.[0-9]"))
rm(list=ls(pattern="temp."))
rm(i)