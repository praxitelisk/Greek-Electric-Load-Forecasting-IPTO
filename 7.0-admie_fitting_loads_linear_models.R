library("Boruta")

startTime <- proc.time()[3]


addlevelsToLm = function(fit.lm.x) {
  
  
  temp = fit.lm.x$xlevels
  
  if(length(names(temp)) > 0) {
    for(t in 1:length(names(temp))) {
      
      fit.lm.x$xlevels[[names(temp)[t]]] = 
        union(fit.lm.x$xlevels[[names(temp)[t]]], levels(predictor.df[, names(temp)[t]]))
      
    }
  }
  
  rm(temp, t)
  
  return(fit.lm.x)
}


#creating the train and test set####
split = 2 * 365

#trainPart = floor(split * dim(final.Data.Set)[1])
trainSet = final.Data.Set[1:(dim(final.Data.Set)[1]-split), ]
testSet = final.Data.Set[(dim(final.Data.Set)[1] - split + 1):dim(final.Data.Set)[1], ]

    
for(i in 1:24) {
 
  list.of.features = getSelectedAttributes(final.boruta.list2[[i]], withTentative = F)
    
  cat("creating a model for Loads at ", i ," o' clock\n")
  cat("\n lm models with the following combination of features:\n\n", list.of.features, "\n\n")
    
    
  #create the predictor variables from training
  FeaturesVariables = trainSet[list.of.features]
  
  
  #add the response variable in trainSet
  FeaturesVariables[paste("Loads", i-1, sep=".")] = 
    trainSet[paste("Loads", i-1, sep=".")]
  
    

  #list of arguments for experiments####
  step.direction.list = c("both", "backward", "forward")
  step.direction = step.direction.list[1]
  num.of.steps = 1000
    
    
  #creating the models####
  assign(paste("fit.lm", i-1, sep="."), lm(as.formula(paste("Loads.", i-1, "~.", sep="")), data = FeaturesVariables))
  
  
  #log - log
  #assign(paste("fit.lm", i-1, sep="."), lm(as.formula(paste("log(","Loads.", i-1, ")", "~", paste("log(", names(FeaturesVariables)[-grep(paste("^Loads.", i-1, sep=""), names(FeaturesVariables))], ")", sep="", collapse = "+"), sep="")), data = FeaturesVariables))
  
  
  FeaturesVariables[paste("Loads", i-1, sep=".")] = NULL
    
    

}
    
#collecting the fits in a list####
cat("collecting the fits.lm in a list\n")
fit.lm = list()
for(i in 1:24) {
  fit.lm[[paste("fit.lm",i-1,sep=".")]] = get(paste("fit.lm",i-1, sep="."))
}

#     
#     #printing the summaries####
#     # for(i in 1:24) {
#     #   print(summary(fit.lm[[i]]))
#     # }
#     
#     
     #making predictions####
     cat("making predictions\n")
     for(i in 1:24) {
       
       
       list.of.features = getSelectedAttributes(final.boruta.list2[[i]], withTentative = F)
       #full.list.of.features
       
       #create the predictor variables from training
       FeaturesVariables = 
         trainSet[list.of.features]
       
       
       #create the predictor.df data.frame for predictions####
       predictor.df = data.frame()
       predictor.df = FeaturesVariables[0, ]
       predictor.df = rbind(predictor.df, testSet[names(testSet) %in% names(predictor.df)])
       
       
       #add level to predictor.df in case of missing levels during prediction 
       #due to the fact that predictor.df is a subset of final.data.set and 
       #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
       
       prev.fit = get(paste("fit.lm",i-1, sep="."))
       returned.fit = addlevelsToLm(prev.fit)
       assign(paste("fit.lm",i-1, sep="."), returned.fit)


     assign(paste("prediction.lm", i-1, sep="."), predict(returned.fit, predictor.df))
    }
     

     #and then collect the predictions in a list
     cat("collecting the predictions in a list\n")
     prediction.lm = list()
     for(i in 1:24) {
       prediction.lm[[paste("prediction.lm",i-1,sep=".")]] = get(paste("prediction.lm",i-1, sep="."))
     }

    
    #calculate mape.lm per hour####
    cat("calculate mape.lm per hour\n")
    mape.lm = list()
    for(i in 1:24) {
      mape.lm[[paste("mape.lm",i-1,sep=".")]] = 100 * mean(unlist(abs(get("testSet")[paste("Loads", i-1, sep=".")] - prediction.lm[[paste("prediction.lm", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")]))
    }
   
    #calculate mae.svm per hour####
    cat("calculate mae.lm per hour\n")
    mae.lm = list()
    for(i in 1:24) {
      mae.lm[[paste("mae.lm",i-1,sep=".")]] = mean(unlist(abs(get("testSet")[paste("Loads", i-1, sep=".")] - prediction.lm[[paste("prediction.lm", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")]))
    }

    #calculate rmse.svm per hour####
    cat("calculate rmse.svm per hour\n")
    rmse.lm = list()
    for(i in 1:24) {
      rmse.lm[[paste("rmse.lm",i-1,sep=".")]] = sqrt(mean(unlist((get("testSet")[paste("Loads", i-1, sep=".")] - prediction.lm[[paste("prediction.lm", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")])^2))
    }

    #calculate mse.svm per hour####
    cat("calculate mse.svm per hour\n")
    mse.lm = list()
    for(i in 1:24) {
      mse.lm[[paste("mse.lm",i-1,sep=".")]] = mean(unlist((get("testSet")[paste("Loads", i-1, sep=".")] - prediction.lm[[paste("prediction.lm", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")])^2)
    }

    #calculate the mean mape####
    cat("calculate the mean mape\n")
    mean.mape.lm = mean(unlist(mape.lm))
     
    cat("calculate the mean mae\n")
    mean.mae.lm = mean(unlist(mae.lm))

    cat("calculate the mean mse\n")
    mean.mse.lm = mean(unlist(mse.lm))

    cat("calculate the mean rmse\n")
    mean.rmse.lm = mean(unlist(rmse.lm))
     
     
    cat("mean lm mape: ", round(mean.mape.lm,3), "\n")
    cat("mean lm mae: ", round(mean.mae.lm,5), "\n")
    cat("mean lm mse: ", round(mean.mse.lm,5), "\n")
    cat("mean lm rmse: ", round(mean.rmse.lm,5), "\n")

    #saving the experiments####
    if (!exists("experiments_lm")) {
      experiments_lm = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA)
      experiments_lm$features = list(list.of.features)
      experiments_lm$mape = mean.mape.lm
      experiments_lm$mae = mean.mae.lm
      experiments_lm$mse = mean.mse.lm
      experiments_lm$rmse = mean.rmse.lm
    } else {
      temp = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA)
      temp$features = list(list.of.features)
      temp$mape = mean.mape.lm
      temp$mae = mean.mae.lm
      temp$mse = mean.mse.lm
      temp$rmse = mean.rmse.lm
      experiments_lm = rbind(experiments_lm, temp)
      rm(temp)
    }
     
     
     #remove some variables####
     rm(list=ls(pattern="prediction.lm."))
     rm(list=ls(pattern="mape.lm."))
     rm(list=ls(pattern="mae.lm."))
     rm(list=ls(pattern="mse.lm."))
     rm(list=ls(pattern="rmse.lm."))
     rm(list=ls(pattern="fit.lm."))
     
    
     cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60)
     #elapsed time in minutes:  1.796333
 
 
rm(num.of.steps, step.direction, step.direction.list)
rm(i,j)
rm(combn.list.of.features, a.list.of.features)
rm(split)
rm(returned.fit, prev.fit) 