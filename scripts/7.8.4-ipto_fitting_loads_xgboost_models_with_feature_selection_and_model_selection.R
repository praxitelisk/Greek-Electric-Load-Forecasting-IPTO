##################################################################
#####xgboost tuning and model selection from feature selection####
##################################################################


library("xgboost")
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


#if (!exists("best.xgboost.parameters.fs")) {
  best.xgboost.parameters.fs = list()
  best.xgboost.fit.fs = list()
  best.xgboost.prediction.fs = list()
#}



for(i in 1:24) {
  
  assign(paste("min.mape.", i-1, sep=""), 1000000)
  
  for (etaValue in seq(0.1, 0.5, 0.1)) {
    for(depthValue in seq(2, 5, 1)) {
      for(roundValue in seq(100, 1200, 100)) {
        
        
        cat("\n\n tuning model: Load.", i-1, " with eta = ", etaValue, ", depth = ", depthValue,", round = ", roundValue ,"\n\n")
        
        
        list.of.features = getSelectedAttributes(final.boruta.list2[[i]], withTentative = F)
        
        
        #cat("\n xgboost model at",  i-1 ," o' clock, with the following combination of features:\n\n",list.of.features,"\n\n")
        
        
        #create the predictor variables from training
        FeaturesVariables = 
          trainSet[list.of.features]
        
        
        dtrain <- xgb.DMatrix(data = data.matrix(FeaturesVariables), label=trainSet[[paste("Loads", i-1, sep=".")]])
        
        
        set.seed(123)
        assign(paste("fit.xgboost", i-1, sep="."), 
               xgboost(data = dtrain, max_depth = depthValue, eta = etaValue, nrounds = roundValue, nthread = 3, verbose = 0, booster= "gbtree", objective = "reg:linear"))
        
  
        #create the predictor.df data.frame for predictions####
        
        FeaturesVariables = 
          trainSet[list.of.features]
        
        
        #create the predictor.df data.frame for predictions####
        predictor.df = data.frame()
        predictor.df = FeaturesVariables[0, ]
        predictor.df = rbind(predictor.df, evaluationSet[names(evaluationSet) %in% names(predictor.df)])
        
        
        assign(paste("prediction.xgboost", i-1, sep="."), predict(get(paste("fit.xgboost",i-1,sep=".")), data.matrix(predictor.df)))
        
        
        #calculate mape
        temp.mape = 100 * mean(unlist(abs((get("evaluationSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.xgboost", i-1, sep=".")))/get("evaluationSet")[paste("Loads", i-1, sep=".")])))
        cat("mape = ", temp.mape,"\n\n")
        
        
        temp.mae =  mean(unlist(abs((get("evaluationSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.xgboost", i-1, sep=".")))/get("evaluationSet")[paste("Loads", i-1, sep=".")])))
        
        
        temp.rmse = sqrt(mean(unlist(abs((get("evaluationSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.xgboost", i-1, sep=".")))/get("evaluationSet")[paste("Loads", i-1, sep=".")]))^2))
        
        
        temp.mse = mean(unlist(abs((get("evaluationSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.xgboost", i-1, sep=".")))/get("evaluationSet")[paste("Loads", i-1, sep=".")]))^2)
        
        
        assign(paste("mape.xgboost",i-1,sep="."), temp.mape)
        assign(paste("mae.xgboost",i-1,sep="."), temp.mae)
        assign(paste("rmse.xgboost",i-1,sep="."), temp.rmse)
        assign(paste("mse.xgboost",i-1,sep="."), temp.mse)
        
        
        if( get(paste("min.mape.", i-1, sep="")) > get(paste("mape.xgboost",i-1,sep=".")) ) {
          
          cat("\n\n ***New best paramenters for Load.", i-1, " model***\n")
          cat(get(paste("mape.xgboost",i-1,sep=".")),"\n")
          
          cat("new best eta: ", etaValue,"\n")
          cat("new best depth: ",depthValue,"\n")
          cat("new best round: ",roundValue,"\n")
          
          
          assign(paste("min.mape.", i-1, sep=""), get(paste("mape.xgboost",i-1,sep=".")))
          
          
          best.xgboost.parameters.fs[[paste("best.xgboost.param.", i-1, sep="")]] = c(etaValue, depthValue, roundValue, get(paste("mape.xgboost",i-1,sep=".")), get(paste("mae.xgboost",i-1,sep=".")), get(paste("rmse.xgboost",i-1,sep=".")), get(paste("mse.xgboost",i-1,sep=".")))
          names(best.xgboost.parameters.fs[[paste("best.xgboost.param.", i-1, sep="")]]) = list("eta", "depth", "round", paste("mape.xgboost",i-1,sep="."), paste("mae.xgboost",i-1,sep="."), paste("rmse.xgboost",i-1,sep="."), paste("mse.xgboost",i-1,sep="."))
          
          
          best.xgboost.fit.fs[[paste("fit.xgboost", i-1, sep=".")]] = get(paste("fit.xgboost",i-1, sep="."))
          
          best.xgboost.prediction.fs[[paste("prediction.xgboost",i-1,sep=".")]] = get(paste("prediction.xgboost",i-1, sep="."))
          
        }
        
        
        ###experiments####
        #saving each tuning experiments####
        if (!exists("experiments.xgboost.ms")) {
          
          experiments.xgboost.ms = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA, "dataset" = NA, "eta" = NA, "depth" = NA, "round" = NA, "algorithm" = NA, "model" = NA, "date" = NA) 
          
          experiments.xgboost.ms$features = list(list.of.features)
          
          if(length(list.of.features) != length(full.list.of.features))
            experiments.xgboost.ms$dataset = "feature selection"
          else
            experiments.xgboost.ms$dataset = "full.list.of.features"
          
          experiments.xgboost.ms$mape = temp.mape
          experiments.xgboost.ms$mae = temp.mae
          experiments.xgboost.ms$mse = temp.mse
          experiments.xgboost.ms$rmse = temp.rmse
          experiments.xgboost.ms$eta = etaValue
          experiments.xgboost.ms$depth = depthValue
          experiments.xgboost.ms$round = roundValue
          experiments.xgboost.ms$algorithm = "xgboost"
          experiments.xgboost.ms$model = paste("Loads.", i-1, sep="")
          experiments.xgboost.ms$date = format(Sys.time(), "%d-%m-%y %H:%M:%S")
          
        } else {
          temp = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA, "dataset" = NA, "eta" = NA, "depth" = NA, "round" = NA, "algorithm" = NA, "model" = NA, "date" = NA) 
          
          temp$features = list(list.of.features)
          
          
          if(length(list.of.features) != length(full.list.of.features))
            temp$dataset = "feature selection"
          else
            temp$dataset = "full.list.of.features"
          
          
          temp$mape = temp.mape
          temp$mae = temp.mae
          temp$mse = temp.mse
          temp$rmse = temp.rmse
          temp$eta = etaValue
          temp$depth = depthValue
          temp$round = roundValue
          temp$algorithm = "xgboost"
          temp$model = paste("Loads.", i-1, sep="")
          temp$date = format(Sys.time(), "%d-%m-%y %H:%M:%S")
          
          experiments.xgboost.ms = rbind(experiments.xgboost.ms, temp)
          rm(temp)
        }
        
        cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60,"\n")
        

        
      }
    }
  }
  
} #end of tuning####


#create the new models after tuning and evaluation phase##################
mape.xgboost.fs.ms = list()
mae.xgboost.fs.ms = list()
rmse.xgboost.fs.ms = list()
mse.xgboost.fs.ms = list()
prediction.xgboost.fs.ms = list()
fit.xgboost.fs.ms = list()


for(i in 1:24) {
  
  list.of.features =
    getSelectedAttributes(final.boruta.list2[[i]], withTentative = F)
  
  cat("\n\n training after evaluation model: Load.",i-1," with best depth = ", best.xgboost.parameters.fs[[paste("best.xgboost.param.", i-1, sep="")]][["depth"]]," eta = ", best.xgboost.parameters.fs[[paste("best.xgboost.param.", i-1, sep="")]][["eta"]]," best round =  ", best.xgboost.parameters.fs[[paste("best.xgboost.param.", i-1, sep="")]][["round"]], "\n", sep="")
  
  #create the predictor variables from training
  FeaturesVariables =
    train.and.evalSet[list.of.features]
  

  dtrain <- xgb.DMatrix(data = data.matrix(FeaturesVariables), label=train.and.evalSet[[paste("Loads", i-1, sep=".")]])
  
  
  set.seed(123)
  assign(paste("fit.xgboost", i-1, sep="."), 
         xgboost(data = dtrain, max_depth = best.xgboost.parameters.fs[[paste("best.xgboost.param.", i-1, sep="")]][["depth"]], eta = best.xgboost.parameters.fs[[paste("best.xgboost.param.", i-1, sep="")]][["eta"]], nrounds = best.xgboost.parameters.fs[[paste("best.xgboost.param.", i-1, sep="")]][["round"]], nthread = 3, verbose = 0, booster= "gbtree", objective = "reg:linear"))
  
  
  FeaturesVariables[paste("Loads", i-1, sep=".")] = NULL

  
  FeaturesVariables =
    train.and.evalSet[list.of.features]
  
  
  predictor.df = data.frame()
  predictor.df = FeaturesVariables[0, ]
  predictor.df = rbind(predictor.df, testSet[names(testSet) %in% names(predictor.df)])
  
  
  assign(paste("prediction.xgboost", i-1, sep="."), predict(get(paste("fit.xgboost",i-1,sep=".")), data.matrix(predictor.df)))
  
  
  #calculate mape
  temp.mape = 100 * mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.xgboost", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")])))
  cat("mape.", i-1 ," = ", temp.mape,"\n\n", sep = "")
  
  
  temp.mae =  mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.xgboost", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")])))
  
  
  temp.rmse = sqrt(mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.xgboost", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")]))^2))
  
  
  temp.mse = mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.xgboost", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")]))^2)
  

  fit.xgboost.fs.ms[[paste("fit.xgboost",i-1,sep=".")]] = get(paste("fit.xgboost",i-1, sep="."))
  
  prediction.xgboost.fs.ms[[paste("prediction.xgboost",i-1,sep=".")]] = get(paste("prediction.xgboost",i-1, sep="."))
  
  mape.xgboost.fs.ms[[paste("mape.xgboost",i-1,sep=".")]] = temp.mape
  mae.xgboost.fs.ms[[paste("mae.xgboost",i-1,sep=".")]] = temp.mae
  mse.xgboost.fs.ms[[paste("mse.xgboost",i-1,sep=".")]] = temp.mse
  rmse.xgboost.fs.ms[[paste("rmse.xgboost",i-1,sep=".")]] = temp.rmse  
  
  
}


#calculate the mean mape####
cat("calculate the mean mape\n")
mean.mape.xgboost.fs.ms = mean(unlist(mape.xgboost.fs.ms))

cat("calculate the mean mae\n")
mean.mae.xgboost.fs.ms = mean(unlist(mae.xgboost.fs.ms))

cat("calculate the mean mse\n")
mean.mse.xgboost.fs.ms = mean(unlist(mse.xgboost.fs.ms))

cat("calculate the mean rmse\n")
mean.rmse.xgboost.fs.ms = mean(unlist(rmse.xgboost.fs.ms))


cat("mean xgboost mape: ", round(mean.mape.xgboost.fs.ms,3), "\n")
cat("mean xgboost mae: ", round(mean.mae.xgboost.fs.ms,5), "\n")
cat("mean xgboost mse: ", round(mean.mse.xgboost.fs.ms,5), "\n")
cat("mean xgboost rmse: ", round(mean.rmse.xgboost.fs.ms,5), "\n")


cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60,"\n")



rm(list=ls(pattern="fit.xgboost.[0-9]"))
rm(list=ls(pattern="prediction.xgboost.[0-9]"))
rm(list=ls(pattern="mape.xgboost.[0-9]"))
rm(list=ls(pattern="mae.xgboost.[0-9]"))
rm(list=ls(pattern="mse.xgboost.[0-9]"))
rm(list=ls(pattern="rmse.xgboost.[0-9]"))
rm(list=ls(pattern="min.mape."))
rm(list=ls(pattern="temp."))
rm(dtrain)
rm(etaValue)
rm(roundValue)
rm(depthValue)
rm(i)