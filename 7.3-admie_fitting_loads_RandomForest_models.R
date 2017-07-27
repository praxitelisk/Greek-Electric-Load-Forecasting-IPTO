library("randomForest")
library("Boruta")


list.of.features = list.of.features = getSelectedAttributes(final.boruta.list$boruta.train.0, withTentative = F)

#creating the train and test set####
split = 2 * 365
#trainPart = floor(split * dim(final.Data.Set)[1])
trainSet = final.Data.Set[1:(dim(final.Data.Set)[1]-split), ]
testSet = final.Data.Set[(dim(final.Data.Set)[1]-split+1):dim(final.Data.Set)[1], ]

  
  

    startTime <- proc.time()[3]
    
   
    cat("\ncombination from a list of features:\n",list.of.features,"\n\n")
    
    
    #create the predictor variables from training
    predictorVariables = 
      subset(trainSet, select = grep(paste(list.of.features, collapse = "|"), names(trainSet)))
    
    
    #create the predictor.df data.frame for predictions####
    predictor.df = data.frame()
    predictor.df = predictorVariables[0, ]
    predictor.df = rbind(predictor.df, testSet[names(testSet) %in% names(predictor.df)])
    
    
    #list of arguments for experiments####
    for(t in seq(20, 20, 10)) {
      
      number.of.trees = t
      
      cat("\n\n number of trees in the forest: ", number.of.trees, "\n")
  
      
      #create 24 models
      for(i in 1:24) {
        cat("creating a model for Loads at",  i-1," o' clock\n")
        
        FeaturesVariables[paste("Loads", i-1, sep=".")] = 
          trainSet[paste("Loads", i-1, sep=".")]
        
        assign(paste("fit.randomForest", i-1, sep="."), 
               randomForest(as.formula(paste("Loads.", i-1, "~.", sep="")), data = FeaturesVariables))
        
        FeaturesVariables[paste("Loads", i-1, sep=".")] = NULL  
      }
      
      
      #collecting the fits in a list####
      cat("collecting the fits.randomForest in a list\n")
      fit.randomForest = list()
      for(i in 1:24) {
        fit.randomForest[[paste("fit.randomForest",i-1,sep=".")]] = get(paste("fit.randomForest",i-1, sep="."))
      }
      
      
      #making predictions####
      cat("making predictions\n")
      for(i in 1:24) {
        assign(paste("prediction.randomForest", i-1, sep="."), predict(fit.randomForest[[paste("fit.randomForest",i-1,sep=".")]], predictor.df))
      }
      
      
      # #printing the summaries####
      # for(i in 1:24) {
      #   print(summary(fit.randomForest[[i]]))
      #   cat("\n")
      # }
      
      
      #and then collect the predictions in a list####
      cat("collecting the predictions in a list\n")
      prediction.randomForest = list()
      for(i in 1:24) {
        prediction.randomForest[[paste("prediction.randomForest",i-1,sep=".")]] = get(paste("prediction.randomForest",i-1, sep="."))
      }
      
      
      #calculate mape per hour####
      cat("calculate mape.randomForest per hour\n")
      mape.randomForest = list()
      for(i in 1:24) {
      mape.randomForest[[paste("mape.randomForest",i-1,sep=".")]] = 100 * mean(unlist(abs(get("testSet")[paste("Loads", i-1, sep=".")] - prediction.randomForest[[paste("prediction.randomForest", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")]))
      }
      
      #calculate mae per hour####
      cat("calculate mae.randomForest per hour\n")
      mae.randomForest = list()
      for(i in 1:24) {
        mae.randomForest[[paste("mae.randomForest",i-1,sep=".")]] = mean(unlist(abs(get("testSet")[paste("Loads", i-1, sep=".")] - prediction.randomForest[[paste("prediction.randomForest", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")]))
      }
      
      #calculate rmse per hour####
      cat("calculate rmse.randomForest per hour\n")
      rmse.randomForest = list()
      for(i in 1:24) {
        rmse.randomForest[[paste("rmse.randomForest",i-1,sep=".")]] = sqrt(mean(unlist((get("testSet")[paste("Loads", i-1, sep=".")] - prediction.randomForest[[paste("prediction.randomForest", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")])^2))
      }
      
      #calculate mse per hour####
      cat("calculate mse.svm per hour\n")
      mse.randomForest = list()
      for(i in 1:24) {
        mse.randomForest[[paste("mse.randomForest",i-1,sep=".")]] = mean(unlist((get("testSet")[paste("Loads", i-1, sep=".")] - prediction.randomForest[[paste("prediction.randomForest", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")])^2)
      }
      
      
      #calculate the mean mape####
      cat("calculate the mean mape\n")
      mean.mape.randomForest = mean(unlist(mape.randomForest))
      
      cat("calculate the mean mae\n")
      mean.mae.randomForest = mean(unlist(mae.randomForest))
      
      cat("calculate the mean mse\n")
      mean.mse.randomForest = mean(unlist(mse.randomForest))
      
      cat("calculate the mean rmse\n")
      mean.rmse.randomForest = mean(unlist(rmse.randomForest))
      
      
      cat("mean randomForest mape: ", round(mean.mape.randomForest,3), "\n")
      cat("mean randomForest mae: ", round(mean.mae.randomForest,5), "\n")
      cat("mean randomForest mse: ", round(mean.mse.randomForest,5), "\n")
      cat("mean randomForest rmse: ", round(mean.rmse.randomForest,5), "\n")
      
      #saving the experiments####
      if (!exists("experiments_randomForest")) {
        experiments_randomForest = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA, "number.of.trees" = NA)
        experiments_randomForest$features = list(list.of.features)
        experiments_randomForest$number.of.trees = number.of.trees
        experiments_randomForest$mape = mean.mape.randomForest
        experiments_randomForest$mae = mean.mae.randomForest
        experiments_randomForest$mse = mean.mse.randomForest
        experiments_randomForest$rmse = mean.rmse.randomForest
      } else {
        temp = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA, "number.of.trees" = NA)
        temp$features = list(list.of.features)
        temp$number.of.trees = number.of.trees
        temp$mape = mean.mape.randomForest
        temp$mae = mean.mae.randomForest
        temp$mse = mean.mse.randomForest
        temp$rmse = mean.rmse.randomForest
        experiments_randomForest = rbind(experiments_randomForest, temp)
        rm(temp)
      }
      
      
      #remove some variables####
      rm(list=ls(pattern = "fit.randomForest."))
      rm(list=ls(pattern = "prediction.randomForest."))
      rm(list=ls(pattern = "mape.randomForest."))
      rm(list=ls(pattern = "mae.randomForest."))
      rm(list=ls(pattern = "mse.randomForest."))
      rm(list=ls(pattern = "rmse.randomForest."))
      
      cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60)
    }


rm(t)
rm(number.of.trees)
rm(split)


rm(startTime)