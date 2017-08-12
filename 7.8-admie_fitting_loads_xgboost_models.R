library("xgboost")
library("Boruta")

startTime <- proc.time()[3]

#creating the train and test set####
split = 2 * 365

#trainPart = floor(split * dim(final.Data.Set)[1])
trainSet = final.Data.Set[1:(dim(final.Data.Set)[1]-split), ]
testSet = final.Data.Set[(dim(final.Data.Set)[1] - split + 1):dim(final.Data.Set)[1], ]



for (etaValue in seq(0.1, 1, 0.2)) {
  for(depthValue in seq(2, 5, 1)) {
    for(roundValue in seq(100, 700, 100)) {
      
      cat("\n\n eta = ", etaValue, ", depth = ", depthValue,", round = ", roundValue ,"\n\n")
      
      
      for(i in 1:24) {
        
        list.of.features = #getSelectedAttributes(final.boruta.list2[[i]], withTentative = F)
          full.list.of.features
        
        cat("\n xgboost model at",  i-1 ," o' clock, with the following combination of features:\n\n",list.of.features,"\n\n")
        
        
        #create the predictor variables from training
        FeaturesVariables = 
          trainSet[list.of.features]
        
        
        dtrain <- xgb.DMatrix(data = data.matrix(FeaturesVariables), label=trainSet[[paste("Loads", i-1, sep=".")]])
        
        
        assign(paste("fit.xgboost", i-1, sep="."), 
               xgboost(data = dtrain, max_depth = depthValue, eta = etaValue, nrounds = roundValue, nthread = 1, verbose = 0, objective = "reg:linear"))
        
      }
      
      
      #collecting the fits in a list####
      cat("collecting the fits.svm in a list\n")
      fit.xgboost = list()
      for(i in 1:24) {
        fit.xgboost[[paste("fit.xgboost",i-1,sep=".")]] = get(paste("fit.xgboost",i-1, sep="."))
      }
      
      
      #making predictions####
      cat("making predictions\n")
      for(i in 1:24) {
        
        list.of.features = #getSelectedAttributes(final.boruta.list2[[i]], withTentative = F)
          full.list.of.features
        
        #create the predictor variables from training
        FeaturesVariables = 
          trainSet[list.of.features]
        
        
        #create the predictor.df data.frame for predictions####
        predictor.df = data.frame()
        predictor.df = FeaturesVariables[0, ]
        predictor.df = rbind(predictor.df, testSet[names(testSet) %in% names(predictor.df)])
        
        
        assign(paste("prediction.xgboost", i-1, sep="."), predict(fit.xgboost[[paste("fit.xgboost",i-1,sep=".")]], data.matrix(predictor.df)))
      }
      
      
      #and then collect the predictions in a list####
      cat("collecting the predictions in a list\n")
      prediction.xgboost = list()
      for(i in 1:24) {
        prediction.xgboost[[paste("prediction.xgboost",i-1,sep=".")]] = get(paste("prediction.xgboost",i-1, sep="."))
      }
      
      
      cat("calculate mape.xgboost per hour\n")
      mape.xgboost = list()
      for(i in 1:24) {
        mape.xgboost[[paste("mape.xgboost",i-1,sep=".")]] = 100 * mean(unlist(abs(get("testSet")[paste("Loads", i-1, sep=".")] - prediction.xgboost[[paste("prediction.xgboost", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")]))
      }
      
      #calculate mae.svm per hour####
      cat("calculate mae.xgboost per hour\n")
      mae.xgboost = list()
      for(i in 1:24) {
        mae.xgboost[[paste("mae.svm",i-1,sep=".")]] = mean(unlist(abs(get("testSet")[paste("Loads", i-1, sep=".")] - prediction.xgboost[[paste("prediction.xgboost", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")]))
      }
      
      #calculate rmse.xgboost per hour####
      cat("calculate rmse.xgboost per hour\n")
      rmse.xgboost = list()
      for(i in 1:24) {
        rmse.xgboost[[paste("rmse.xgboost",i-1,sep=".")]] = sqrt(mean(unlist((get("testSet")[paste("Loads", i-1, sep=".")] - prediction.xgboost[[paste("prediction.xgboost", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")])^2))
      }
      
      #calculate mse.svm per hour####
      cat("calculate mse.xgboost per hour\n")
      mse.xgboost = list()
      for(i in 1:24) {
        mse.xgboost[[paste("mse.xgboost",i-1,sep=".")]] = mean(unlist((get("testSet")[paste("Loads", i-1, sep=".")] - prediction.xgboost[[paste("prediction.xgboost", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")])^2)
      }
      
      
      cat("calculate the mean mape\n")
      mean.mape.xgboost = mean(unlist(mape.xgboost))
      
      cat("calculate the mean mae\n")
      mean.mae.xgboost = mean(unlist(mae.xgboost))
      
      cat("calculate the mean mse\n")
      mean.mse.xgboost = mean(unlist(mse.xgboost))
      
      cat("calculate the mean rmse\n")
      mean.rmse.xgboost = mean(unlist(rmse.xgboost))
      
      
      cat("\n\n eta = ", etaValue, ", depth = ", depthValue," round = ", roundValue,"\n")
      cat("mean xgboost mape: ", round(mean.mape.xgboost, 3), "\n")
      cat("mean xgboost mae: ", round(mean.mae.xgboost, 5), "\n")
      cat("mean xgboost mse: ", round(mean.mse.xgboost, 5), "\n")
      cat("mean xgboost rmse: ", round(mean.rmse.xgboost, 5), "\n")
      
      
      #saving the experiments####
      if (!exists("experiments_xgboost")) {
        experiments_xgboost = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA, "eta" = NA, "depth" = NA, "round" = NA) 
        
        experiments_xgboost$mape = mean.mape.xgboost
        experiments_xgboost$mae = mean.mae.xgboost
        experiments_xgboost$mse = mean.mse.xgboost
        experiments_xgboost$rmse = mean.rmse.xgboost
        
        if(length(list.of.features) != length(full.list.of.features))
          experiments_xgboost$features = "feature selection"
        else
          experiments_xgboost$features = "full.list.of.features"
        
        experiments_xgboost$eta = etaValue
        experiments_xgboost$depth = depthValue
        experiments_xgboost$round = roundValue
        
        
      } else {
        temp = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA, "eta" = NA, "depth" = NA, "round" = NA)
        
        
        temp$mape = mean.mape.xgboost
        temp$mae = mean.mae.xgboost
        temp$mse = mean.mse.xgboost
        temp$rmse = mean.rmse.xgboost
        
        if(length(list.of.features) != length(full.list.of.features))
          temp$features = "feature selection"
        else
          temp$features = "full.list.of.features"
        
        temp$eta = etaValue
        temp$depth = depthValue
        temp$round = roundValue
        
        experiments_xgboost = rbind(experiments_xgboost, temp)
        rm(temp)
      }
      
      cat("\n elapsed time in minutes: ", (proc.time()[3]-startTime)/60, "\n\n")
      
      
      rm(list=ls(pattern="fit.xgboost."))
      rm(list=ls(pattern="prediction.xgboost."))
      rm(list=ls(pattern="mape.xgboost."))
      rm(list=ls(pattern="mae.xgboost."))
      rm(list=ls(pattern="mse.xgboost."))
      rm(list=ls(pattern="rmse.xgboost."))
      
    }
  }
}


rm(dtrain)
rm(etaValue)
rm(roundValue)
rm(depthValue)
rm(i)