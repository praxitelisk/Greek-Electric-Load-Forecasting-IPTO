##########################################################
#####NN with feature selection and default parameters####
##########################################################

#load the libraries####
library("nnet")
library("Boruta")
library("RSNNS")

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


fit.nn.fs.def = list()
prediction.nn.fs.def = list()
mape.nn.fs.def = list()
mae.nn.fs.def = list()
rmse.nn.fs.def = list()
mse.nn.fs.def = list()


mean.nn.fs.rate = 0
for(i in 1:length(best.nn.parameters.fs)) {
  mean.nn.fs.rate = mean.nn.fs.rate + best.nn.parameters.fs[[i]][1]
}
mean.nn.fs.rate = mean.nn.fs.rate/length(best.nn.parameters.fs)


mean.nn.fs.hiddenNeurons = 0
for(i in 1:length(best.nn.parameters.fs)) {
  mean.nn.fs.hiddenNeurons = mean.nn.fs.hiddenNeurons + best.nn.parameters.fs[[i]][2]
}
mean.nn.fs.hiddenNeurons = round(mean.nn.fs.hiddenNeurons/length(best.nn.parameters.fs))


mean.nn.fs.maxit = 0
for(i in 1:length(best.nn.parameters.fs)) {
  mean.nn.fs.maxit = mean.nn.fs.maxit + best.nn.parameters.fs[[i]][3]
}
mean.nn.fs.maxit = round(mean.nn.fs.maxit/length(best.nn.parameters.fs))

for(i in 1:24) {
  
  
  list.of.features = getSelectedAttributes(final.boruta.list2[[i]], withTentative = F)
  
  cat("\n\n nn training model: Load.",i-1," with default parameters hiddenLayerNeurons = ", mean.nn.full.hiddenNeurons, " maxit = ", mean.nn.full.maxit , " rate = ", mean.nn.full.rate ," and feature selection \n", sep = "")
  
  
  #create the predictor variables from training
  FeaturesVariables = 
    train.and.evalSet[list.of.features]
  
  
  #add the response variable in trainSet
  FeaturesVariables[paste("Loads", i-1, sep=".")] = 
    train.and.evalSet[paste("Loads", i-1, sep=".")]
  
  
  #scaling the train set####
  #scale the train and test set to[0,1] values
  
  #convert all factors to numeric
  FeaturesVariables.scale = data.matrix(FeaturesVariables)
  
  # normalize features into [0,1]
  FeaturesVariables.scale = normalizeData(FeaturesVariables.scale, type = "0_1")
  FeaturesVariables.scale = as.data.frame(FeaturesVariables.scale)
  colnames(FeaturesVariables.scale) = colnames(FeaturesVariables)
  
  
  #train a model####
  assign(paste("fit.nn", i-1, sep="."), 
         nnet(as.formula(paste("Loads.", i-1, "~.", sep="")), data = FeaturesVariables.scale, size = 5, rang = 0, trace = F, MaxNWts = 1000000, abstol = 1.0e-5))
  
  
  FeaturesVariables[paste("Loads", i-1, sep=".")] = NULL
  
  
  
  #make the prediction from train-eval set####
  
  FeaturesVariables = 
    train.and.evalSet[list.of.features]
  
  predictor.df = data.frame()
  predictor.df = FeaturesVariables[0, ]
  predictor.df = rbind(predictor.df, testSet[names(testSet) %in% names(predictor.df)])
  
  
  #scale the predictor.df data for prediction
  predictor.df.scale = data.matrix(predictor.df)
  predictor.df.scale = normalizeData(predictor.df.scale, type = "0_1")
  predictor.df.scale = as.data.frame(predictor.df.scale)
  colnames(predictor.df.scale) = colnames(predictor.df)
  
  
  #make the prediction####
  assign(paste("prediction.nn", i-1, sep="."), predict(get(paste("fit.nn",i-1,sep=".")), predictor.df.scale))
  
  
  #denormalize the predictions####
  load.scale = testSet[paste("Loads", i-1, sep=".")]
  load.scale = normalizeData(load.scale, type="0_1")
  #load.scale = scale(load.scale, center = min(load.scale), scale = max(load.scale) - min(load.scale))
  
  aux.prediction.nn = get(paste("prediction.nn", i-1, sep="."))
  #aux.prediction.nn = aux.prediction.nn * attr(load.scale, 'scaled:scale') + attr(load.scale, 'scaled:center')
  aux.prediction.nn = denormalizeData(aux.prediction.nn, getNormParameters(load.scale))
  
  assign(paste("prediction.nn", i-1, sep="."), aux.prediction.nn)
  
  
  #calculate mape
  temp.mape = 100 * mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.nn", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")])))
  cat("mape.", i-1 ," = ", temp.mape,"\n\n", sep = "")
  
  
  temp.mae =  mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.nn", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")])))
  
  
  temp.rmse = sqrt(mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.nn", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")]))^2))
  
  
  temp.mse = mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.nn", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")]))^2)
  
  
  
  fit.nn.fs.def[[paste("fit.nn",i-1,sep=".")]] = get(paste("fit.nn",i-1, sep="."))
  
  prediction.nn.fs.def[[paste("prediction.nn",i-1,sep=".")]] = get(paste("prediction.nn",i-1, sep="."))
  
  mape.nn.fs.def[[paste("mape.nn",i-1,sep=".")]] = temp.mape
  mae.nn.fs.def[[paste("mae.nn",i-1,sep=".")]] = temp.mae
  mse.nn.fs.def[[paste("mse.nn",i-1,sep=".")]] = temp.mse
  rmse.nn.fs.def[[paste("rmse.nn",i-1,sep=".")]] = temp.rmse
  
  
} #end of models


#calculate the mean mape####
cat("calculate the mean mape\n")
mean.mape.nn.fs.def = mean(unlist(mape.nn.fs.def))

cat("calculate the mean mae\n")
mean.mae.nn.fs.def = mean(unlist(mae.nn.fs.def))

cat("calculate the mean mse\n")
mean.mse.nn.fs.def = mean(unlist(mse.nn.fs.def))

cat("calculate the mean rmse\n")
mean.rmse.nn.fs.def = mean(unlist(rmse.nn.fs.def))


cat("mean nn mape: ", round(mean.mape.nn.fs.def,3), "\n")
cat("mean nn mae: ", round(mean.mae.nn.fs.def,5), "\n")
cat("mean nn mse: ", round(mean.mse.nn.fs.def,5), "\n")
cat("mean nn rmse: ", round(mean.rmse.nn.fs.def,5), "\n")


cat("elapsed time in minutes: ", (proc.time()[3] - startTime)/60,"\n")



rm(list=ls(pattern="min.mape."))
rm(list=ls(pattern="temp."))
rm(load.scale)
rm(predictor.df.scale)
rm(FeaturesVariables.scale)
rm(aux.prediction.nn)
rm(i)
rm(list=ls(pattern="fit.nn.[0-9]"))
rm(list=ls(pattern="prediction.nn.[0-9]"))
rm(list=ls(pattern="mape.nn.[0-9]"))
rm(list=ls(pattern="mae.nn.[0-9]"))
rm(list=ls(pattern="mse.nn.[0-9]"))
rm(list=ls(pattern="rmse.nn.[0-9]"))
