library(nnet)
library("Boruta")

#get the list of features from feature selection####
list.of.features = getSelectedAttributes(final.boruta.list$boruta.train.0, withTentative = F)


#creating the train and test set####
split = 2 * 365

#trainPart = floor(split * dim(final.Data.Set)[1])
trainSet = final.Data.Set[1:(dim(final.Data.Set)[1]-split), ]
testSet = final.Data.Set[(dim(final.Data.Set)[1] - split + 1):dim(final.Data.Set)[1], ]

startTime <- proc.time()[3]

cat("\n nn models with the following combination of features:\n\n",list.of.features,"\n\n")


#create the predictor variables from training
FeaturesVariables = 
  subset(trainSet, select = grep(paste(list.of.features, collapse = "|"), names(trainSet)))


#create the predictor.df data.frame for predictions####
predictor.df = data.frame()
predictor.df = FeaturesVariables[0, ]
predictor.df = rbind(predictor.df, testSet[names(testSet) %in% names(predictor.df)])


#scaling the train and test set####
#scale the train and test set to[0,1] values
FeaturesVariables.scale = FeaturesVariables

for(i in 1:ncol(FeaturesVariables)) {
 
  if(class(FeaturesVariables[, i]) == "factor") {
    FeaturesVariables.scale[, i] = as.numeric(FeaturesVariables[, i])

  } else {
    FeaturesVariables.scale[, i] = FeaturesVariables[, i]
  }
  
}


maxs <- apply(FeaturesVariables.scale, 2, max)
mins <- apply(FeaturesVariables.scale, 2, min)
for(i in 1:ncol(FeaturesVariables.scale)) {
  
  FeaturesVariables.scale[, i] = scale(FeaturesVariables.scale[, i], center = mins[i], scale = maxs[i] - mins[i])
  
}

predictor.df.scale = predictor.df

for(i in 1:ncol(predictor.df)) {
  
  if(class(predictor.df[, i]) == "factor") {
    predictor.df.scale[, i] = as.numeric(predictor.df[, i])
    
  } else {
    predictor.df.scale[, i] = predictor.df[, i]
  }
}


maxs <- apply(predictor.df.scale, 2, max)
mins <- apply(predictor.df.scale, 2, min)

for(i in 1:ncol(predictor.df.scale)) {
  
  predictor.df.scale[, i] = scale(predictor.df.scale[, i], center = mins[i], scale = maxs[i] - mins[i])
  
}

#run experiments for a different number of hiddenLayerNeurons####
for(hiddenLayerNeurons in seq(5,100,5)) {
  
  #create 24 models####
  for(i in 1:24) {
    
    cat("creating a model for Loads at",  i-1," o' clock\n")
    
    
    FeaturesVariables.scale[paste("Loads", i-1, sep=".")] = 
      trainSet[paste("Loads", i-1, sep=".")]
    
    
    maxs <- apply(FeaturesVariables.scale[paste("Loads", i-1, sep=".")], 2, max)
    mins <- apply(FeaturesVariables.scale[paste("Loads", i-1, sep=".")], 2, min)
    
    FeaturesVariables.scale[paste("Loads", i-1, sep=".")] = 
      scale(FeaturesVariables.scale[paste("Loads", i-1, sep=".")], center = mins, scale = maxs - mins)
    
    assign(paste("fit.nn", i-1, sep="."), 
           nnet(as.formula(paste("Loads.", i-1, "~.", sep="")), data = FeaturesVariables.scale, size = hiddenLayerNeurons, trace = TRUE, MaxNWts = 1000000, abstol = 1.0e-4, maxit = 300, linout=TRUE))
    
    FeaturesVariables.scale[paste("Loads", i-1, sep=".")] = NULL  
  }
  
  
  #collecting the fits in a list####
  cat("collecting the fits.nn in a list\n")
  fit.nn = list()
  for(i in 1:24) {
    fit.nn[[paste("fit.nn",i-1,sep=".")]] = get(paste("fit.nn",i-1, sep="."))
  }
  
  
  #making predictions####
  cat("making predictions\n")
  for(i in 1:24) {
    assign(paste("prediction.nn", i-1, sep="."), predict(fit.nn[[paste("fit.nn",i-1,sep=".")]], predictor.df.scale))
  }
  
  
  #rescale predictions####
  cat("rescale predictions\n")
  for(i in 1:24) {
    
    load.scale = testSet[paste("Loads", i-1, sep=".")]
    load.scale = scale(load.scale, center = min(load.scale), scale = max(load.scale) - min(load.scale))
    
    aux.prediction.nn = get(paste("prediction.nn", i-1, sep="."))
    aux.prediction.nn = aux.prediction.nn * attr(load.scale, 'scaled:scale') + attr(load.scale, 'scaled:center')  
    
    assign(paste("prediction.nn", i-1, sep="."), aux.prediction.nn)
    
  }
  
  
  
  #printing the models' summaries####
  for(i in 1:24) {
    print(summary(fit.nn[[i]]))
  }
  
  
  #and then collect the predictions in a list####
  cat("collecting the predictions in a list\n")
  prediction.nn = list()
  for(i in 1:24) {
    prediction.nn[[paste("prediction.nn",i-1,sep=".")]] = get(paste("prediction.nn",i-1, sep="."))
  }
  
  
  #calculate mape.nn per hour####
  cat("calculate mape.nn per hour\n")
  mape.nn = list()
  for(i in 1:24) {
    mape.nn[[paste("mape.nn",i-1,sep=".")]] = 100 * mean(unlist(abs(get("testSet")[paste("Loads", i-1, sep=".")] - prediction.nn[[paste("prediction.nn", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")]))
  }
  
  #calculate mae.svm per hour####
  cat("calculate mae.svm per hour\n")
  mae.nn = list()
  for(i in 1:24) {
    mae.nn[[paste("mae.nn",i-1,sep=".")]] = mean(unlist(abs(get("testSet")[paste("Loads", i-1, sep=".")] - prediction.nn[[paste("prediction.nn", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")]))
  }
  
  #calculate rmse.svm per hour####
  cat("calculate rmse.svm per hour\n")
  rmse.nn = list()
  for(i in 1:24) {
    rmse.nn[[paste("rmse.nn",i-1,sep=".")]] = sqrt(mean(unlist((get("testSet")[paste("Loads", i-1, sep=".")] - prediction.nn[[paste("prediction.nn", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")])^2))
  }
  
  #calculate mse.svm per hour####
  cat("calculate mse.svm per hour\n")
  mse.nn = list()
  for(i in 1:24) {
    mse.nn[[paste("mse.nn",i-1,sep=".")]] = mean(unlist((get("testSet")[paste("Loads", i-1, sep=".")] - prediction.nn[[paste("prediction.nn", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")])^2)
  }
  
  
  #calculate the mean mape####
  cat("calculate the mean mape\n")
  mean.mape.nn = mean(unlist(mape.nn))
  
  cat("calculate the mean mae\n")
  mean.mae.nn = mean(unlist(mae.nn))
  
  cat("calculate the mean mse\n")
  mean.mse.nn = mean(unlist(mse.nn))
  
  cat("calculate the mean rmse\n")
  mean.rmse.nn = mean(unlist(rmse.nn))
  

  cat("\n\n number of neurons in hidden layer = ", hiddenLayerNeurons,"\n\n")
  cat("mean nn mape: ", round(mean.mape.nn,3), "\n")
  cat("mean nn mae: ", round(mean.mae.nn,5), "\n")
  cat("mean nn mse: ", round(mean.mse.nn,5), "\n")
  cat("mean nn rmse: ", round(mean.rmse.nn,5), "\n")
  
  
  #saving the experiments####
  if (!exists("experiments_nn")) {
    experiments_nn = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA, "hiddenLayerNeurons" = NA) 
    
    experiments_nn$features = list(list.of.features)
    
    experiments_nn$mape = mean.mape.nn
    experiments_nn$mae = mean.mae.nn
    experiments_nn$mse = mean.mse.nn
    experiments_nn$rmse = mean.rmse.nn
    experiments_nn$hiddenLayerNeurons = hiddenLayerNeurons

  } else {
    temp = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA, "hiddenLayerNeurons" = NA)
    
    temp$features = list(list.of.features)
    
    temp$mape = mean.mape.nn
    temp$mae = mean.mae.nn
    temp$mse = mean.mse.nn
    temp$rmse = mean.rmse.nn
    temp$hiddenLayerNeurons = hiddenLayerNeurons

    
    experiments_nn = rbind(experiments_nn, temp)
    rm(temp)
  }
  
  
  #remove some auxiliary variables####
  rm(list=ls(pattern="fit.nn."))
  rm(list=ls(pattern="prediction.nn."))
  rm(list=ls(pattern="mape.nn."))
  rm(list=ls(pattern="mae.nn."))
  rm(list=ls(pattern="mse.nn."))
  rm(list=ls(pattern="rmse.nn."))
  rm(mins)
  rm(maxs)
  rm(aux.prediction.nn)
  rm(load.scale)
  rm(hiddenLayerNeurons)
  rm(i)
  
  cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60,"\n")

}