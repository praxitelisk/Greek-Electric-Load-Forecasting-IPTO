library(nnet)
library("Boruta")
library("RSNNS")

startTime <- proc.time()[3]

#creating the train and test set####
split = 2 * 365

#trainPart = floor(split * dim(final.Data.Set)[1])
trainSet = final.Data.Set[1:(dim(final.Data.Set)[1]-split), ]
testSet = final.Data.Set[(dim(final.Data.Set)[1] - split + 1):dim(final.Data.Set)[1], ]


best.nn.parameters.fs = list()
best.nn.fit.fs = list()
best.nn.prediction.fs = list()

#tuning NN parameters per 24hour model####
for(i in 1:24) {
  
  assign(paste("min.mape.", i-1, sep=""), 1000000)
  
  for(hiddenLayerNeurons in seq(1, 20, 1)) {
    for(maxitValue in seq(10, 250, 10)) {
      
      cat("\n\n tuning model: Load.",i-1, "with hiddenLayerNeurons = ", hiddenLayerNeurons," maxitValue = ", maxitValue," \n")
      
      list.of.features = 
        getSelectedAttributes(final.boruta.list2[[i]], withTentative = F)
      
      
      #create the predictor variables from training
      FeaturesVariables = 
        trainSet[list.of.features]
      
      
      #add the response variable in trainSet
      FeaturesVariables[paste("Loads", i-1, sep=".")] = 
        trainSet[paste("Loads", i-1, sep=".")]
      
      
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
             nnet(as.formula(paste("Loads.", i-1, "~.", sep="")), data = FeaturesVariables.scale, size = hiddenLayerNeurons, trace = FALSE, MaxNWts = 1000000, abstol = 1.0e-2, maxit = maxitValue, linout=TRUE, rang = 0))
      
      FeaturesVariables.scale[paste("Loads", i-1, sep=".")] = NULL
      
      
      
      #create the predictor.df data.frame for predictions####
      
      FeaturesVariables = 
        trainSet[list.of.features]
      
      predictor.df = data.frame()
      predictor.df = FeaturesVariables[0, ]
      predictor.df = rbind(predictor.df, testSet[names(testSet) %in% names(predictor.df)])
      
      #scale the predictor.df data for prediction
      predictor.df.scale = data.matrix(predictor.df)
      predictor.df.scale = normalizeData(predictor.df.scale, type = "0_1")
      predictor.df.scale = as.data.frame(predictor.df.scale)
      colnames(predictor.df.scale) = colnames(predictor.df)
      
      
      #make the prediction
      assign(paste("prediction.nn", i-1, sep="."), predict(get(paste("fit.nn",i-1,sep=".")), predictor.df.scale))
      
      
      #denormalize the predictor.df
      load.scale = testSet[paste("Loads", i-1, sep=".")]
      load.scale = normalizeData(load.scale, type="0_1")
      #load.scale = scale(load.scale, center = min(load.scale), scale = max(load.scale) - min(load.scale))
      
      aux.prediction.nn = get(paste("prediction.nn", i-1, sep="."))
      #aux.prediction.nn = aux.prediction.nn * attr(load.scale, 'scaled:scale') + attr(load.scale, 'scaled:center')
      aux.prediction.nn = denormalizeData(aux.prediction.nn, getNormParameters(load.scale))
      
      assign(paste("prediction.nn", i-1, sep="."), aux.prediction.nn)
      
      
      #calculate mape
      temp.mape = 100 * mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.nn", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")])))
      cat("mape = ", temp.mape,"\n\n")
      
      
      temp.mae =  mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.nn", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")])))
      
      
      temp.rmse = sqrt(mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.nn", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")]))^2))
      
      
      temp.mse = mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.nn", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")]))^2)
      
      
      assign(paste("mape.nn",i-1,sep="."), temp.mape)
      assign(paste("mae.nn",i-1,sep="."), temp.mae)
      assign(paste("rmse.nn",i-1,sep="."), temp.rmse)
      assign(paste("mse.nn",i-1,sep="."), temp.mse)
      
      
      if( get(paste("min.mape.", i-1, sep="")) > get(paste("mape.nn",i-1,sep=".")) ) {
        
        cat("\n\n ***New best paramenters for Load.", i-1, " model***\n")
        cat(get(paste("mape.nn",i-1,sep=".")),"\n")
        
        cat("new best maxit: ", maxitValue,"\n")
        cat("new best size: ",hiddenLayerNeurons,"\n")
        
        
        assign(paste("min.mape.", i-1, sep=""), get(paste("mape.nn",i-1,sep=".")))
        
        
        best.nn.parameters.fs[[paste("best.nn.param.", i-1, sep="")]] = c(hiddenLayerNeurons, maxitValue, get(paste("mape.nn",i-1,sep=".")), get(paste("mae.nn",i-1,sep=".")), get(paste("rmse.nn",i-1,sep=".")), get(paste("mse.nn",i-1,sep=".")))
        names(best.nn.parameters.fs[[i]]) = list("hiddenLayerNeurons", "maxitValue", paste("mape.nn",i-1,sep="."), paste("mae.nn",i-1,sep="."), paste("rmse.nn",i-1,sep="."), paste("mse.nn",i-1,sep="."))
        
        
        best.nn.fit.fs[[paste("fit.nn", i-1, sep=".")]] = get(paste("fit.nn",i-1, sep="."))
        
        best.nn.prediction.fs[[paste("prediction.nn",i-1,sep=".")]] = get(paste("prediction.nn",i-1, sep="."))
        
      }
      
    }
  }
  
  cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60,"\n")
  
  rm(list=ls(pattern="fit.nn."))
  rm(list=ls(pattern="prediction.nn."))
  rm(list=ls(pattern="mape.nn."))
  rm(list=ls(pattern="mae.nn."))
  rm(list=ls(pattern="mse.nn."))
  rm(list=ls(pattern="rmse.nn."))
  
} ##end of tuning


#calculate the mean mape after tuning
temp.mape = 0
temp.mae = 0
temp.mse = 0
temp.rmse = 0
for(i in 1:length(best.nn.parameters.fs)) {
  
  temp.mape  = temp.mape + best.nn.parameters.fs[[i]][[3]]
  temp.mae = temp.mae + best.nn.parameters.fs[[i]][[4]]
  temp.rmse = temp.rmse + best.nn.parameters.fs[[i]][[5]]
  temp.mse = temp.mse + best.nn.parameters.fs[[i]][[6]]

}
mean.mape.nn = temp.mape/24
mean.mae.nn = temp.mae/24
mean.rmse.nn = temp.rmse/24
mean.mse.nn = temp.mse/24

cat("\n****************\n")
cat("mean nn mape: ", round(mean.mape.nn,3), "\n")
cat("mean nn mae: ", round(mean.mae.nn,5), "\n")
cat("mean nn mse: ", round(mean.mse.nn,5), "\n")
cat("mean nn rmse: ", round(mean.rmse.nn,5), "\n")



rm(list=ls(pattern="min.mape."))
rm(list=ls(pattern="temp."))
rm(hiddenLayerNeurons)
rm(maxitValue)
rm(load.scale)
rm(predictor.df.scale)
rm(FeaturesVariables.scale)
rm(aux.prediction.nn)
rm(i)