library("Cubist")
library("Boruta")

startTime <- proc.time()[3]

#creating the train and test set####
split = 2 * 365

#trainPart = floor(split * dim(final.Data.Set)[1])
trainSet = final.Data.Set[1:(dim(final.Data.Set)[1]-split), ]
testSet = final.Data.Set[(dim(final.Data.Set)[1] - split + 1):dim(final.Data.Set)[1], ]


for(committee in 31:33) {
  
  for(i in 1:24) {

    
    list.of.features = 
      getSelectedAttributes(final.boruta.list[[i]], withTentative = F)
    
    
    cat("\n Rule based model at",  i-1 ," o' clock, with the following combination of features:\n\n",list.of.features,"\n")
    cat("\n committee = ", committee, "\n\n")
    
    #create the predictor variables from training
    FeaturesVariables = 
      subset(trainSet, select = grep(paste(list.of.features, collapse = "|"), names(trainSet)))
    
    #add the response variable in trainSet
    FeaturesVariables[paste("Loads", i-1, sep=".")] = 
      trainSet[paste("Loads", i-1, sep=".")]
    
    assign(paste("fit.rule", i-1, sep="."), 
           cubist(x = FeaturesVariables[-grep(paste("Loads", i-1, sep="."), names(FeaturesVariables))], y = FeaturesVariables[[paste("Loads", i-1, sep=".")]], committees = committee))
    
    FeaturesVariables[paste("Loads", i-1, sep=".")] = NULL  
  }
  
  
  #collecting the fits in a list####
  cat("collecting the fit.rule in a list\n")
  fit.rule = list()
  for(i in 1:24) {
    fit.rule[[paste("fit.rule",i-1,sep=".")]] = get(paste("fit.rule",i-1, sep="."))
  }
  
  #printing the models' summaries####
  for(i in 1:24) {
    print(summary(fit.rule[[i]]))
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
    
    
    assign(paste("prediction.rule", i-1, sep="."), predict(fit.rule[[paste("fit.rule",i-1,sep=".")]], predictor.df))
  }
  
  
  #and then collect the predictions in a list####
  cat("collecting the predictions in a list\n")
  prediction.rule = list()
  for(i in 1:24) {
    prediction.rule[[paste("prediction.rule",i-1,sep=".")]] = get(paste("prediction.rule",i-1, sep="."))
  }
  
  #calculate mape.rule per hour####
  cat("calculate mape.rule per hour\n")
  mape.rule = list()
  for(i in 1:24) {
    mape.rule[[paste("mape.rule",i-1,sep=".")]] = 100 * mean(unlist(abs(get("testSet")[paste("Loads", i-1, sep=".")] - prediction.rule[[paste("prediction.rule", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")]))
  }
  
  #calculate mae.rule per hour####
  cat("calculate mae.rule per hour\n")
  mae.rule = list()
  for(i in 1:24) {
    mae.rule[[paste("mae.rule",i-1,sep=".")]] = mean(unlist(abs(get("testSet")[paste("Loads", i-1, sep=".")] - prediction.rule[[paste("prediction.rule", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")]))
  }
  
  #calculate rmse.rule per hour####
  cat("calculate rmse.rule per hour\n")
  rmse.rule = list()
  for(i in 1:24) {
    rmse.rule[[paste("rmse.rule",i-1,sep=".")]] = sqrt(mean(unlist((get("testSet")[paste("Loads", i-1, sep=".")] - prediction.rule[[paste("prediction.rule", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")])^2))
  }
  
  #calculate mse.rule per hour####
  cat("calculate mse.rule per hour\n")
  mse.rule = list()
  for(i in 1:24) {
    mse.rule[[paste("mse.rule",i-1,sep=".")]] = mean(unlist((get("testSet")[paste("Loads", i-1, sep=".")] - prediction.rule[[paste("prediction.rule", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")])^2)
  }
  
  
  #calculate the mean mape####
  cat("calculate the mean mape\n")
  mean.mape.rule = mean(unlist(mape.rule))
  
  cat("calculate the mean mae\n")
  mean.mae.rule = mean(unlist(mae.rule))
  
  cat("calculate the mean mse\n")
  mean.mse.rule = mean(unlist(mse.rule))
  
  cat("calculate the mean rmse\n")
  mean.rmse.rule = mean(unlist(rmse.rule))
  
  cat("mean rule mape: ", round(mean.mape.rule, 3), "\n")
  cat("mean rule mae: ", round(mean.mae.rule, 5), "\n")
  cat("mean rule mse: ", round(mean.mse.rule, 5), "\n")
  cat("mean rule rmse: ", round(mean.rmse.rule, 5), "\n")
  
  
  #saving the experiments####
  if (!exists("experiments_rules")) {
    experiments_rules = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "committee" = NA) 
    
    experiments_rules$mape = mean.mape.rule
    experiments_rules$mae = mean.mae.rule
    experiments_rules$mse = mean.mse.rule
    experiments_rules$rmse = mean.rmse.rule
    
    experiments_rules$committee = committee
    
  } else {
    temp = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "committee" = NA)
    
    temp$mape = mean.mape.rule
    temp$mae = mean.mae.rule
    temp$mse = mean.mse.rule
    temp$rmse = mean.rmse.rule
    
    temp$committee = committee
    
    experiments_rules = rbind(experiments_rules, temp)
    rm(temp)
  }
  
  cat("\n elapsed time in minutes: ", (proc.time()[3]-startTime)/60, "\n\n")
  
  #remove some variables####
  rm(list=ls(pattern="fit.rule."))
  rm(list=ls(pattern="prediction.rule."))
  rm(list=ls(pattern="mape.rule."))
  rm(list=ls(pattern="mae.rule."))
  rm(list=ls(pattern="mse.rule."))
  rm(list=ls(pattern="rmse.rule."))
  rm(i)
  
  
}

rm(committee)


#fit.Rule.0 <- cubist(x = FeaturesVariables[-grep("Loads.0", names(FeaturesVariables))], y = FeaturesVariables$Loads.0)


