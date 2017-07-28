library("FNN")
library("Boruta")

startTime <- proc.time()[3]

#creating the train and test set####
split = 2 * 365

#trainPart = floor(split * dim(final.Data.Set)[1])
trainSet = final.Data.Set[1:(dim(final.Data.Set)[1]-split), ]
testSet = final.Data.Set[(dim(final.Data.Set)[1] - split + 1):dim(final.Data.Set)[1], ]

#making experiments####
algorithm.list = c("kd_tree", "cover_tree", "brute")

for(j in 1:length(algorithm.list)) {
  
  for(neighbors in seq(11, 20, 1)) {
    
    
    #making models####
    for(i in 1:24) {
      
      list.of.features = 
        getSelectedAttributes(final.boruta.list[[i]], withTentative = F)
      
      
      cat("\n KNN model at",  i-1 ," o' clock, with the following combination of features:\n\n",list.of.features,"\n")
      cat("\n algorithm = ", algorithm.list[j], ", neighbors = ", neighbors,"\n\n")
      
      #create the predictor variables from training
      FeaturesVariables = 
        subset(trainSet, select = grep(paste(list.of.features, collapse = "|"), names(trainSet)))
      
      
      #convert all factors to numeric because knn can't handle factor variables####
      for(k in 1:ncol(FeaturesVariables)) {
        
        if(class(FeaturesVariables[, k]) == "factor") {
          FeaturesVariables[, k] = as.numeric(FeaturesVariables[, k])
          
        } else {
          FeaturesVariables[, k] = FeaturesVariables[, k]
        }
        
      }
      
 
      assign(paste("fit.knn", i-1, sep="."), 
             knn.reg(FeaturesVariables, y = unlist(trainSet[paste("Loads", i-1, sep=".")]), k=neighbors, algorithm = algorithm.list[j]))
             
             
      FeaturesVariables[paste("Loads", i-1, sep=".")] = NULL  
      
    
    }
    
    
    #collecting the fits in a list####
    cat("collecting the fits.knn in a list\n")
    fit.knn = list()
    for(i in 1:24) {
      fit.knn[[paste("fit.knn",i-1,sep=".")]] = get(paste("fit.knn",i-1, sep="."))
    }
    
    
    #making predictions####
    cat("making predictions\n")
    for(i in 1:24) {
      
      list.of.features = 
        getSelectedAttributes(final.boruta.list[[i]], withTentative = F)
      
      
      #create the predictor variables from training
      FeaturesVariables = 
        subset(trainSet, select = grep(paste(list.of.features, collapse = "|"), names(trainSet)))
      
      
      #create the predictor.df data.frame for predictions####
      predictor.df = data.frame()
      predictor.df = FeaturesVariables[0, ]
      predictor.df = rbind(predictor.df, testSet[names(testSet) %in% names(predictor.df)])
      
      
      #convert all factors to numeric because knn can't handle factor variables####
      for(k in 1:ncol(FeaturesVariables)) {
        
        if(class(FeaturesVariables[, k]) == "factor") {
          FeaturesVariables[, k] = as.numeric(FeaturesVariables[, k])
          
        } else {
          FeaturesVariables[, k] = FeaturesVariables[, k]
        }
        
      }
      
      
      for(k in 1:ncol(predictor.df)) {
        
        if(class(predictor.df[, k]) == "factor") {
          predictor.df[, k] = as.numeric(predictor.df[, k])
          
        } else {
          predictor.df[, k] = predictor.df[, k]
        }
      }
      
      
      assign(paste("prediction.knn", i-1, sep="."), 
             knn.reg(FeaturesVariables, test = predictor.df, y=unlist(trainSet[paste("Loads", i-1, sep=".")]), k=neighbors, algorithm=algorithm.list[j]))
             
      
      #add the response variable in trainSet
      FeaturesVariables[paste("Loads", i-1, sep=".")] = NULL
    }
    
    
    #and then collect the predictions in a list####
    cat("collecting the predictions in a list\n")
    prediction.knn = list()
    for(i in 1:24) {
      prediction.knn[[paste("prediction.knn",i-1,sep=".")]] = get(paste("prediction.knn",i-1, sep="."))
    }
    
    
    #calculate mape.knn per hour####
    cat("calculate mape.knn per hour\n")
    mape.knn = list()
    for(i in 1:24) {
      mape.knn[[paste("mape.knn",i-1,sep=".")]] = 100 * mean(unlist(abs(get("testSet")[paste("Loads", i-1, sep=".")] - prediction.knn[[paste("prediction.knn", i-1, sep=".")]]$pred)/get("testSet")[paste("Loads", i-1, sep=".")]))
    }
    
    
    #calculate mae.knn per hour####
    cat("calculate mae.knn per hour\n")
    mae.knn = list()
    for(i in 1:24) {
      mae.knn[[paste("mae.knn",i-1,sep=".")]] = mean(unlist(abs(get("testSet")[paste("Loads", i-1, sep=".")] - prediction.knn[[paste("prediction.knn", i-1, sep=".")]]$pred)/get("testSet")[paste("Loads", i-1, sep=".")]))
    }
    
    #calculate rmse.knn per hour####
    cat("calculate rmse.knn per hour\n")
    rmse.knn = list()
    for(i in 1:24) {
      rmse.knn[[paste("rmse.knn",i-1,sep=".")]] = sqrt(mean(unlist((get("testSet")[paste("Loads", i-1, sep=".")] - prediction.knn[[paste("prediction.knn", i-1, sep=".")]]$pred)/get("testSet")[paste("Loads", i-1, sep=".")])^2))
    }
    
    #calculate mse.knn per hour####
    cat("calculate mse.knn per hour\n")
    mse.knn = list()
    for(i in 1:24) {
      mse.knn[[paste("mse.knn",i-1,sep=".")]] = mean(unlist((get("testSet")[paste("Loads", i-1, sep=".")] - prediction.knn[[paste("prediction.knn", i-1, sep=".")]]$pred)/get("testSet")[paste("Loads", i-1, sep=".")])^2)
    }
    
    
    #calculate the mean mape####
    cat("calculate the mean mape\n")
    mean.mape.knn = mean(unlist(mape.knn))
    
    cat("calculate the mean mae\n")
    mean.mae.knn = mean(unlist(mae.knn))
    
    cat("calculate the mean mse\n")
    mean.mse.knn = mean(unlist(mse.knn))
    
    cat("calculate the mean rmse\n")
    mean.rmse.knn = mean(unlist(rmse.knn))
    
    
    cat("mean knn mape: ", round(mean.mape.knn,3), "\n")
    cat("mean knn mae: ", round(mean.mae.knn,5), "\n")
    cat("mean knn mse: ", round(mean.mse.knn,5), "\n")
    cat("mean knn rmse: ", round(mean.rmse.knn,5), "\n")
    
    
    #saving the experiments####
    if (!exists("experiments_knn")) {
      experiments_knn = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "algorithm" = NA, "neighbors" = NA) 
      
      experiments_knn$mape = mean.mape.knn
      experiments_knn$mae = mean.mae.knn
      experiments_knn$mse = mean.mse.knn
      experiments_knn$rmse = mean.rmse.knn
      
      experiments_knn$algorithm = algorithm.list[j]
      experiments_knn$neighbors = neighbors
      
    } else {
      temp = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "algorithm" = NA, "neighbors" = NA) 
      
      temp$mape = mean.mape.knn
      temp$mae = mean.mae.knn
      temp$mse = mean.mse.knn
      temp$rmse = mean.rmse.knn
      
      temp$algorithm = algorithm.list[j]
      temp$neighbors = neighbors
      
      experiments_knn = rbind(experiments_knn, temp)
      rm(temp)
    }
    
    cat("\n elapsed time in minutes: ", (proc.time()[3]-startTime)/60, "\n\n")

  }
}


rm(list=ls(pattern="fit.knn."))
rm(list=ls(pattern="prediction.knn."))
rm(list=ls(pattern="mape.knn."))
rm(list=ls(pattern="mae.knn."))
rm(list=ls(pattern="mse.knn."))
rm(list=ls(pattern="rmse.knn."))
rm(neighbors)
rm(algorithm.list)
rm(j)
rm(k)
rm(i)