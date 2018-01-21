##############################################################
#####NN tuning and model selection from feature selection####
##############################################################

#load the libraries####
library("nnet")
library("Boruta")
library("RSNNS")


#start measuring time#####
startTime <- proc.time()[3]

#creating the train and test set splits#################
splitEvalSet = 365
splitTestSet = splitEvalSet + 365
len = dim(final.Data.Set)[1]

#creating the train and test set splits#################
splitEvalSet = 365
splitTestSet = splitEvalSet + 365
len = dim(final.Data.Set)[1]

#trainPart = floor(split * dim(final.Data.Set)[1])
trainSet = final.Data.Set[1:(len - splitTestSet), ]
evaluationSet = final.Data.Set[(len - splitTestSet + 1):(len - splitEvalSet), ]
train.and.evalSet = final.Data.Set[1:(len - splitEvalSet), ]
testSet = final.Data.Set[(len - splitEvalSet + 1):len, ]

####create the train, evaluation and test Set###################################

full.list.of.features = names(final.Data.Set)
full.list.of.features = full.list.of.features[-grep("^Loads|time|weekday|icon|^day.of.week$|^day.of.year$|yesterday.weather.measures.isQuietHour|yesterday.weather.measures.isHoliday|yesterday.weather.measures.isWeekend|yesterday.weather.measures.day.of.week|yesterday.weather.measures.sine.day.of.week|yesterday.weather.measures.cosine.day.of.week|yesterday.weather.measures.day.of.year|yesterday.weather.measures.cosine.day.of.year|yesterday.weather.measures.sine.day.of.year|temperature|windBearing.[0-9]+$", full.list.of.features)]


full.list.of.FeaturesVariables = final.Data.Set[full.list.of.features]


trainSet = trainSet[full.list.of.features]


evaluationSet = evaluationSet[full.list.of.features]


train.and.evalSet = train.and.evalSet[full.list.of.features]


testSet = testSet[full.list.of.features]



#create the lists which store the best parameters######################################

#if (!exists("best.nn.parameters.fs")) {

rm(experiments.nn.ms)

best.nn.parameters.fs = list()
best.nn.fit.fs = list()
best.nn.prediction.fs = list()
#}

learnFuncType = "Rprop"


#starting grid search - model selection################################################
for(i in 1:24) {
  
  assign(paste("min.mape.", i-1, sep=""), 1000000)
  
  
  for(learningRateValue in seq(0.01, 0.07, 0.03)) { ###needs more learning rates for better convergence
    for(hiddenLayerNeurons in seq(4, 10, 1)) {
      for(maxitValue in seq(100, 1500, 100)) { ###needs more iterations for better convergence
        
        cat("\n\n tuning model: Load.",i-1,"with feature selection and learning rate =", learningRateValue," hiddenLayerNeurons = ", hiddenLayerNeurons," maxitValue = ", maxitValue," \n")
        
        list.of.features = 
          getSelectedAttributes(final.boruta.list2[[i]], withTentative = F)
        
        
        #create the predictor variables from training
        FeaturesVariables = 
          trainSet[list.of.features]
        
        
        
        #add the response variable in trainSet
        FeaturesVariables[paste("Loads", i-1, sep=".")] = 
          final.Data.Set[1:dim(trainSet)[1], paste("Loads", i-1, sep=".")]
        
        
        #scaling the train set####
        #scale the train and test set to[0,1] values
        
        #convert all factors to numeric
        FeaturesVariables.scale = data.matrix(FeaturesVariables)
        
        # normalize features into [0,1]
        FeaturesVariables.scale = normalizeData(FeaturesVariables.scale, type = "0_1")
        FeaturesVariables.scale = as.data.frame(FeaturesVariables.scale)
        colnames(FeaturesVariables.scale) = colnames(FeaturesVariables)
        
        
        #train a model####
        set.seed(123)
        assign(paste("fit.nn", i-1, sep="."), 
               mlp(FeaturesVariables.scale[-grep(paste("^Loads", i-1, sep="."), names(FeaturesVariables.scale))], FeaturesVariables.scale[paste("Loads", i-1, sep=".")], hiddenLayerNeurons, 
                   maxit = maxitValue, learnFuncParams = learningRateValue, shufflePatterns = F, linOut = T, learnFunc = learnFuncType))
        
        
        #assign(paste("fit.nn", i-1, sep="."), 
        #       nnet(as.formula(paste("Loads.", i-1, "~.", sep="")), data = FeaturesVariables.scale, size = hiddenLayerNeurons, trace = FALSE, MaxNWts = 1000000, abstol = 1.0e-4, maxit = maxitValue, linout=TRUE, rang = 0, decay = 0.01))
        #
        
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
        evaluationSet[paste("Loads", i-1, sep=".")] = 
          final.Data.Set[(len - splitTestSet + 1):(len - splitEvalSet), paste("Loads", i-1, sep=".")]
        
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
          
          
          best.nn.parameters.fs[[paste("best.nn.param.", i-1, sep="")]] = c(learningRateValue, hiddenLayerNeurons, maxitValue, get(paste("mape.nn",i-1,sep=".")), get(paste("mae.nn",i-1,sep=".")), get(paste("rmse.nn",i-1,sep=".")), get(paste("mse.nn",i-1,sep=".")))
          names(best.nn.parameters.fs[[paste("best.nn.param.", i-1, sep="")]]) = list("learningRate", "hiddenLayerNeurons", "maxitValue", paste("mape.nn",i-1,sep="."), paste("mae.nn",i-1,sep="."), paste("rmse.nn",i-1,sep="."), paste("mse.nn",i-1,sep="."))
          
          
          best.nn.fit.fs[[paste("fit.nn", i-1, sep=".")]] = get(paste("fit.nn",i-1, sep="."))
          
          best.nn.prediction.fs[[paste("prediction.nn",i-1,sep=".")]] = get(paste("prediction.nn",i-1, sep="."))
          
        }
        
        cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60,"\n")
        
        evaluationSet[paste("Loads", i-1, sep=".")] = NULL
        
        
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



#create the new models after tuning and evaluation phase##################
mape.nn.fs.ms = list()
mae.nn.fs.ms = list()
rmse.nn.fs.ms = list()
mse.nn.fs.ms = list()
prediction.nn.fs.ms = list()
fit.nn.fs.ms = list()


for(i in 1:24) {


  list.of.features =
    getSelectedAttributes(final.boruta.list2[[i]], withTentative = F)

  cat("\n\n training after evaluation model: Load.",i-1," with best hiddenLayerNeurons = ", best.nn.parameters.fs[[paste("best.nn.param.", i-1, sep="")]][["hiddenLayerNeurons"]]," maxitValue = ", best.nn.parameters.fs[[paste("best.nn.param.", i-1, sep="")]][["maxitValue"]]," best rate =  ", best.nn.parameters.fs[[paste("best.nn.param.", i-1, sep="")]][["learningRate"]], "\n", sep="")
  
  #cat("\n\n training after evaluation model: Load.",i-1,"with best hiddenLayerNeurons = ", best.nn.parameters.fs[[i]][["hiddenLayerNeurons"]]," maxitValue = ", best.nn.parameters.fs[[i]][["maxitValue"]]," \n")


  #create the predictor variables from training
  FeaturesVariables =
    train.and.evalSet[list.of.features]


  #add the response variable in trainSet
  FeaturesVariables[paste("Loads", i-1, sep=".")] = 
    final.Data.Set[1:dim(train.and.evalSet)[1], paste("Loads", i-1, sep=".")]


  #scaling the train set####
  #scale the train and test set to[0,1] values

  #convert all factors to numeric
  FeaturesVariables.scale = data.matrix(FeaturesVariables)

  # normalize features into [0,1]
  FeaturesVariables.scale = normalizeData(FeaturesVariables.scale, type = "0_1")
  FeaturesVariables.scale = as.data.frame(FeaturesVariables.scale)
  colnames(FeaturesVariables.scale) = colnames(FeaturesVariables)


  #train a model####
  set.seed(123)
  assign(paste("fit.nn", i-1, sep="."), 
         mlp(FeaturesVariables.scale[-grep(paste("^Loads", i-1, sep="."), names(FeaturesVariables.scale))], FeaturesVariables.scale[paste("Loads", i-1, sep=".")], size=best.nn.parameters.fs[[paste("best.nn.param.", i-1, sep="")]][["hiddenLayerNeurons"]], 
             maxit = best.nn.parameters.fs[[paste("best.nn.param.", i-1, sep="")]][["maxitValue"]], learnFuncParams = best.nn.parameters.fs[[paste("best.nn.param.", i-1, sep="")]][["learningRate"]], shufflePatterns = F, linOut = T, learnFunc = learnFuncType))
  
  
  #assign(paste("fit.nn", i-1, sep="."),
  #       nnet(as.formula(paste("Loads.", i-1, "~.", sep="")), data = FeaturesVariables.scale, size = best.nn.parameters.fs[[i]][["hiddenLayerNeurons"]], trace = FALSE, MaxNWts = 1000000, abstol = 1.0e-4, maxit = best.nn.parameters.fs[[i]][["maxitValue"]], linout=TRUE, rang = 0))


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
  
  testSet[paste("Loads", i-1, sep=".")] = 
    final.Data.Set[(len - splitEvalSet + 1):len, paste("Loads", i-1, sep=".")]
  
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



  fit.nn.fs.ms[[paste("fit.nn",i-1,sep=".")]] = get(paste("fit.nn",i-1, sep="."))

  prediction.nn.fs.ms[[paste("prediction.nn",i-1,sep=".")]] = get(paste("prediction.nn",i-1, sep="."))

  mape.nn.fs.ms[[paste("mape.nn",i-1,sep=".")]] = temp.mape
  mae.nn.fs.ms[[paste("mae.nn",i-1,sep=".")]] = temp.mae
  mse.nn.fs.ms[[paste("mse.nn",i-1,sep=".")]] = temp.mse
  rmse.nn.fs.ms[[paste("rmse.nn",i-1,sep=".")]] = temp.rmse

  
  testSet[paste("Loads", i-1, sep=".")] = NULL

} #end of models


#calculate the mean mape####
cat("calculate the mean mape\n")
mean.mape.nn.fs.ms = mean(unlist(mape.nn.fs.ms))

cat("calculate the mean mae\n")
mean.mae.nn.fs.ms = mean(unlist(mae.nn.fs.ms))

cat("calculate the mean mse\n")
mean.mse.nn.fs.ms = mean(unlist(mse.nn.fs.ms))

cat("calculate the mean rmse\n")
mean.rmse.nn.fs.ms = mean(unlist(rmse.nn.fs.ms))


cat("mean nn mape: ", round(mean.mape.nn.fs.ms,3), "\n")
cat("mean nn mae: ", round(mean.mae.nn.fs.ms,5), "\n")
cat("mean nn mse: ", round(mean.mse.nn.fs.ms,5), "\n")
cat("mean nn rmse: ", round(mean.rmse.nn.fs.ms,5), "\n")


cat("elapsed time in minutes: ", (proc.time()[3] - startTime)/60,"\n")
rm(startTime)


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
rm(hiddenLayerNeurons)
rm(learningRateValue)
rm(maxitValue)
rm(learnFuncType)

