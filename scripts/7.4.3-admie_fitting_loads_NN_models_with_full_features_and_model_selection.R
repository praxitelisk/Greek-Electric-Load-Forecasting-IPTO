##############################################################
#####NN tuning and model selection from all the features####
##############################################################

#load the libraries####
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


#create the lists which store the best parameters
#if (!exists("best.nn.parameters.full")) {
best.nn.parameters.full = list()
best.nn.fit.full = list()
best.nn.prediction.full = list()
#}



for(i in 1:24) {
  
  assign(paste("min.mape.", i-1, sep=""), 1000000)
  
  
  for(learningRateValue in seq(0.1, 0.5, 0.1)) {
    for(hiddenLayerNeurons in seq(2, 10, 1)) {
      for(maxitValue in seq(100, 300, 100)) {
      
        cat("\n\n tuning model: Load.",i-1,"with full features and learning rate ", learningRateValue," hiddenLayerNeurons = ", hiddenLayerNeurons," maxitValue = ", maxitValue," \n")
        
        list.of.features = full.list.of.features
        
        
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
              mlp(FeaturesVariables.scale[-grep(paste("^Loads", i-1, sep="."), names(FeaturesVariables.scale))], FeaturesVariables.scale[paste("Loads", i-1, sep=".")], hiddenLayerNeurons, 
                  maxit = maxitValue, initFuncParams = 0, learnFuncParams = learningRateValue, shufflePatterns = F, linOut = T, learnFunc = "Rprop"))
        
        
        FeaturesVariables[paste("Loads", i-1, sep=".")] = NULL
        
        
        
        #create the predictor.df data.frame for predictions####
        FeaturesVariables = 
          trainSet[list.of.features]
        
        predictor.df = data.frame()
        predictor.df = FeaturesVariables[0, ]
        predictor.df = rbind(predictor.df, evaluationSet[names(evaluationSet) %in% names(predictor.df)])
        
        
        #scale the predictor.df data for prediction
        predictor.df.scale = data.matrix(predictor.df)
        predictor.df.scale = normalizeData(predictor.df.scale, type = "0_1")
        predictor.df.scale = as.data.frame(predictor.df.scale)
        colnames(predictor.df.scale) = colnames(predictor.df)
        
        
        #make the prediction
        assign(paste("prediction.nn", i-1, sep="."), predict(get(paste("fit.nn",i-1,sep=".")), predictor.df.scale))
        
        
        #denormalize the predictions
        load.scale = evaluationSet[paste("Loads", i-1, sep=".")]
        load.scale = normalizeData(load.scale, type="0_1")
        #load.scale = scale(load.scale, center = min(load.scale), scale = max(load.scale) - min(load.scale))
        
        aux.prediction.nn = get(paste("prediction.nn", i-1, sep="."))
        #aux.prediction.nn = aux.prediction.nn * attr(load.scale, 'scaled:scale') + attr(load.scale, 'scaled:center')
        aux.prediction.nn = denormalizeData(aux.prediction.nn, getNormParameters(load.scale))
        
        assign(paste("prediction.nn", i-1, sep="."), aux.prediction.nn)
        
        
        #calculate mape
        temp.mape = 100 * mean(unlist(abs((get("evaluationSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.nn", i-1, sep=".")))/get("evaluationSet")[paste("Loads", i-1, sep=".")])))
        cat("mape = ", temp.mape,"\n\n")
        
        
        temp.mae =  mean(unlist(abs((get("evaluationSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.nn", i-1, sep=".")))/get("evaluationSet")[paste("Loads", i-1, sep=".")])))
        
        
        temp.rmse = sqrt(mean(unlist(abs((get("evaluationSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.nn", i-1, sep=".")))/get("evaluationSet")[paste("Loads", i-1, sep=".")]))^2))
        
        
        temp.mse = mean(unlist(abs((get("evaluationSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.nn", i-1, sep=".")))/get("evaluationSet")[paste("Loads", i-1, sep=".")]))^2)
        
        
        assign(paste("mape.nn",i-1,sep="."), temp.mape)
        assign(paste("mae.nn",i-1,sep="."), temp.mae)
        assign(paste("rmse.nn",i-1,sep="."), temp.rmse)
        assign(paste("mse.nn",i-1,sep="."), temp.mse)
        
        
        #check if this mape is less than a previous one.
        if( get(paste("min.mape.", i-1, sep="")) > get(paste("mape.nn",i-1,sep=".")) ) {
          
          cat("\n\n ***New best paramenters for Load.", i-1, " model***\n")
          cat(get(paste("mape.nn",i-1,sep=".")),"\n")
          
          cat("new best learningRate: ", learningRateValue,"\n")
          cat("new best hiddenLayerNeurons: ", hiddenLayerNeurons,"\n")
          cat("new best maxitValue: ",maxitValue,"\n")
          
          
          assign(paste("min.mape.", i-1, sep=""), get(paste("mape.nn",i-1,sep=".")))
          
          
          best.nn.parameters.full[[paste("best.nn.param.", i-1, sep="")]] = c(learningRateValue, hiddenLayerNeurons, maxitValue, get(paste("mape.nn",i-1,sep=".")), get(paste("mae.nn",i-1,sep=".")), get(paste("rmse.nn",i-1,sep=".")), get(paste("mse.nn",i-1,sep=".")))
          names(best.nn.parameters.full[[paste("best.nn.param.", i-1, sep="")]]) = list("learningRate", "hiddenLayerNeurons", "maxitValue", paste("mape.nn",i-1,sep="."), paste("mae.nn",i-1,sep="."), paste("rmse.nn",i-1,sep="."), paste("mse.nn",i-1,sep="."))
          
          
          best.nn.fit.full[[paste("fit.nn", i-1, sep=".")]] = get(paste("fit.nn",i-1, sep="."))
          
          best.nn.prediction.full[[paste("prediction.nn",i-1,sep=".")]] = get(paste("prediction.nn",i-1, sep="."))
          
        }
        
        cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60,"\n")
        
        
        
        #saving each tuning experiments####
        if (!exists("experiments.nn.ms")) {
          
          experiments.nn.ms = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA, "dataset" = NA, "learningRate" = NA, "hiddenLayerNeurons" = NA, "maxitValue" = NA, "algorithm" = NA, "model" = NA, "date" = NA) 
          
          experiments.nn.ms$features = list(list.of.features)
          
          if(length(list.of.features) != length(full.list.of.features))
            experiments.nn.ms$dataset = "feature selection"
          else
            experiments.nn.ms$dataset = "full.list.of.features"
          
          experiments.nn.ms$mape = temp.mape
          experiments.nn.ms$mae = temp.mae
          experiments.nn.ms$mse = temp.mse
          experiments.nn.ms$rmse = temp.rmse
          experiments.nn.ms$learningRate = learningRateValue
          experiments.nn.ms$hiddenLayerNeurons = hiddenLayerNeurons
          experiments.nn.ms$maxitValue = maxitValue
          experiments.nn.ms$algorithm = "mlp"
          experiments.nn.ms$model = paste("Loads.", i-1, sep="")
          experiments.nn.ms$date = format(Sys.time(), "%d-%m-%y %H:%M:%S")
          
        } else {
          temp = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA, "dataset" = NA, "hiddenLayerNeurons" = NA, "maxitValue" = NA, "algorithm" = NA, "model" = NA, "date" = NA) 
          
          temp$features = list(list.of.features)
          
          
          if(length(list.of.features) != length(full.list.of.features))
            temp$dataset = "feature selection"
          else
            temp$dataset = "full.list.of.features"
          
          
          temp$mape = temp.mape
          temp$mae = temp.mae
          temp$mse = temp.mse
          temp$rmse = temp.rmse
          temp$learningRate = learningRateValue
          temp$hiddenLayerNeurons = hiddenLayerNeurons
          temp$maxitValue = maxitValue
          temp$algorithm = "mlp"
          temp$model = paste("Loads.", i-1, sep="")
          temp$date = format(Sys.time(), "%d-%m-%y %H:%M:%S")
          
          experiments.nn.ms = rbind(experiments.nn.ms, temp)
          rm(temp)
        }
        
      }
    }
  }
  
} #end of tuning####



#create the new model after tuning and evaluation##################
mape.nn.full.ms = list()
mae.nn.full.ms = list()
rmse.nn.full.ms = list()
mse.nn.full.ms = list()
prediction.nn.full.ms = list()
fit.nn.full.ms = list()


for(i in 1:24) {
  
  
  list.of.features = full.list.of.features
  
  cat("\n\n training after evaluation model: Load.",i-1," with best hiddenLayerNeurons = ", best.nn.parameters.full[[paste("best.nn.param.", i-1, sep="")]][["hiddenLayerNeurons"]]," maxitValue = ", best.nn.parameters.full[[paste("best.nn.param.", i-1, sep="")]][["maxitValue"]]," best rate =  ", best.nn.parameters.full[[paste("best.nn.param.", i-1, sep="")]][["learningRate"]], "\n", sep="")
  
  
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
         mlp(FeaturesVariables.scale[-grep(paste("^Loads", i-1, sep="."), names(FeaturesVariables.scale))], FeaturesVariables.scale[paste("Loads", i-1, sep=".")], size=best.nn.parameters.full[[paste("best.nn.param.", i-1, sep="")]][["hiddenLayerNeurons"]], 
             maxit = best.nn.parameters.full[[paste("best.nn.param.", i-1, sep="")]][["maxitValue"]], initFuncParams = 0, learnFuncParams = best.nn.parameters.full[[paste("best.nn.param.", i-1, sep="")]][["learningRate"]], shufflePatterns = F, linOut = T, learnFunc = "Rprop"))
  
  
  FeaturesVariables[paste("Loads", i-1, sep=".")] = NULL
  
  
  
  #make the prediction from train-eval set####
  
  FeaturesVariables = 
    train.and.evalSet[list.of.features]
  
  predictor.df = data.frame()
  predictor.df = FeaturesVariables[0, ]
  predictor.df = rbind(predictor.df, testSet[names(evaluationSet) %in% names(predictor.df)])
  
  
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
  
  
  
  fit.nn.full.ms[[paste("fit.nn",i-1,sep=".")]] = get(paste("fit.nn",i-1, sep="."))
  
  prediction.nn.full.ms[[paste("prediction.nn",i-1,sep=".")]] = get(paste("prediction.nn",i-1, sep="."))
  
  mape.nn.full.ms[[paste("mape.nn",i-1,sep=".")]] = temp.mape
  mae.nn.full.ms[[paste("mae.nn",i-1,sep=".")]] = temp.mae
  mse.nn.full.ms[[paste("mse.nn",i-1,sep=".")]] = temp.mse
  rmse.nn.full.ms[[paste("rmse.nn",i-1,sep=".")]] = temp.rmse
  
  
} #end of models


#calculate the mean mape####
cat("calculate the mean mape\n")
mean.mape.nn.full.ms = mean(unlist(mape.nn.full.ms))

cat("calculate the mean mae\n")
mean.mae.nn.full.ms = mean(unlist(mae.nn.full.ms))

cat("calculate the mean mse\n")
mean.mse.nn.full.ms = mean(unlist(mse.nn.full.ms))

cat("calculate the mean rmse\n")
mean.rmse.nn.full.ms = mean(unlist(rmse.nn.full.ms))


cat("mean nn mape: ", round(mean.mape.nn.full.ms,3), "\n")
cat("mean nn mae: ", round(mean.mae.nn.full.ms,5), "\n")
cat("mean nn mse: ", round(mean.mse.nn.full.ms,5), "\n")
cat("mean nn rmse: ", round(mean.rmse.nn.full.ms,5), "\n")


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
