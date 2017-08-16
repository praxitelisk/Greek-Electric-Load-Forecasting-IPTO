library("nnet")
library("Boruta")
library("RSNNS")

startTime <- proc.time()[3]

#creating the train and test set####
split = 2 * 365

#trainPart = floor(split * dim(final.Data.Set)[1])
trainSet = final.Data.Set[1:(dim(final.Data.Set)[1]-split), ]
testSet = final.Data.Set[(dim(final.Data.Set)[1] - split + 1):dim(final.Data.Set)[1], ]


if (!exists("best.nn.parameters.full")) {
  best.nn.parameters.full = list()
  best.nn.fit.full = list()
  best.nn.prediction.full = list()
}

#tuning NN parameters per 24hour model####
for(i in 1:24) {
  
  assign(paste("min.mape.", i-1, sep=""), 1000000)
  
  for(hiddenLayerNeurons in seq(2, 10, 1)) {
    for(maxitValue in seq(10, 100, 10)) {
      
      cat("\n\n tuning model: Load.",i-1,"with hiddenLayerNeurons = ", hiddenLayerNeurons," maxitValue = ", maxitValue," \n")
      
      list.of.features = 
        full.list.of.features
        #getSelectedAttributes(final.boruta.list2[[i]], withTentative = F)
      
      
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
             nnet(as.formula(paste("Loads.", i-1, "~.", sep="")), data = FeaturesVariables.scale, size = hiddenLayerNeurons, trace = FALSE, MaxNWts = 1000000, abstol = 1.0e-4, maxit = maxitValue, linout=TRUE, rang = 0))
      
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
      
      
      #check if this mape is less than a previous one.
      if( get(paste("min.mape.", i-1, sep="")) > get(paste("mape.nn",i-1,sep=".")) ) {
        
        cat("\n\n ***New best paramenters for Load.", i-1, " model***\n")
        cat(get(paste("mape.nn",i-1,sep=".")),"\n")
        
        cat("new best maxit: ", maxitValue,"\n")
        cat("new best size: ",hiddenLayerNeurons,"\n")
        
        
        assign(paste("min.mape.", i-1, sep=""), get(paste("mape.nn",i-1,sep=".")))
        
        
        best.nn.parameters.full[[paste("best.nn.param.", i-1, sep="")]] = c(hiddenLayerNeurons, maxitValue, get(paste("mape.nn",i-1,sep=".")), get(paste("mae.nn",i-1,sep=".")), get(paste("rmse.nn",i-1,sep=".")), get(paste("mse.nn",i-1,sep=".")))
        names(best.nn.parameters.full[[paste("best.nn.param.", i-1, sep="")]]) = list("hiddenLayerNeurons", "maxitValue", paste("mape.nn",i-1,sep="."), paste("mae.nn",i-1,sep="."), paste("rmse.nn",i-1,sep="."), paste("mse.nn",i-1,sep="."))
        
        
        best.nn.fit.full[[paste("fit.nn", i-1, sep=".")]] = get(paste("fit.nn",i-1, sep="."))
        
        best.nn.prediction.full[[paste("prediction.nn",i-1,sep=".")]] = get(paste("prediction.nn",i-1, sep="."))
        
      }
      
      
      #saving each tuning experiments####
      if (!exists("experiments.nn")) {
        
        experiments.nn = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA, "method" = NA, "hiddenLayerNeurons" = NA, "maxIter" = NA, "model" = NA) 
        
        experiments.nn$features = list(list.of.features)
        
        if(length(list.of.features) != length(full.list.of.features))
          experiments.nn$method = "feature selection"
        else
          experiments.nn$method = "full.list.of.features"
        
        experiments.nn$mape = temp.mape
        experiments.nn$mae = temp.mae
        experiments.nn$mse = temp.mse
        experiments.nn$rmse = temp.rmse
        experiments.nn$hiddenLayerNeurons = hiddenLayerNeurons
        experiments.nn$maxIter = maxitValue
        experiments.nn$model = paste("Loads.", i-1, sep="")
        
      } else {
        temp = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA, "method" = NA, "hiddenLayerNeurons" = NA, "maxIter" = NA, "model" = NA)
        
        temp$features = list(list.of.features)
        
        
        if(length(list.of.features) != length(full.list.of.features))
          temp$method = "feature selection"
        else
          temp$method = "full.list.of.features"
        
        
        temp$mape = temp.mape
        temp$mae = temp.mae
        temp$mse = temp.mse
        temp$rmse = temp.rmse
        temp$hiddenLayerNeurons = hiddenLayerNeurons
        temp$maxIter = maxitValue
        temp$model = paste("Loads.", i-1, sep="")
        
        experiments.nn = rbind(experiments.nn, temp)
        rm(temp)
      }
      
      cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60,"\n")
    }
  }
  
  
  
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
for(i in 1:length(best.nn.parameters.full)) {
  
  temp.mape  = temp.mape + best.nn.parameters.full[[i]][[3]]
  temp.mae = temp.mae + best.nn.parameters.full[[i]][[4]]
  temp.rmse = temp.rmse + best.nn.parameters.full[[i]][[5]]
  temp.mse = temp.mse + best.nn.parameters.full[[i]][[6]]
  
}

#change this variable to mean.mape.nn.full etc. for full.list.of.features
mean.mape.nn.full = temp.mape/length(best.nn.parameters.full)
mean.mae.nn.full = temp.mae/length(best.nn.parameters.full)
mean.rmse.nn.full = temp.rmse/length(best.nn.parameters.full)
mean.mse.nn.full = temp.mse/length(best.nn.parameters.full)

cat("\n****************\n")
cat("mean nn mape: ", round(mean.mape.nn.full,3), "\n")
cat("mean nn mae: ", round(mean.mae.nn.full,5), "\n")
cat("mean nn mse: ", round(mean.mse.nn.full,5), "\n")
cat("mean nn rmse: ", round(mean.rmse.nn.full,5), "\n")



rm(list=ls(pattern="min.mape."))
rm(list=ls(pattern="temp."))
rm(hiddenLayerNeurons)
rm(maxitValue)
rm(load.scale)
rm(predictor.df.scale)
rm(FeaturesVariables.scale)
rm(aux.prediction.nn)
rm(i)
rm(temp.mape)
rm(temp.mae)
rm(temp.mse)
rm(temp.rmse)

cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60,"\n")