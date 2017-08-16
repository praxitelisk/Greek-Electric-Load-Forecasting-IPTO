library("Boruta")
library("e1071")


startTime <- proc.time()[3]

#creating the train and test set####
split = 2 * 365

#trainPart = floor(split * dim(final.Data.Set)[1])
trainSet = final.Data.Set[1:(dim(final.Data.Set)[1]-split), ]
testSet = final.Data.Set[(dim(final.Data.Set)[1] - split + 1):dim(final.Data.Set)[1], ]


if (!exists("best.svm.parameters.fs")) {
  best.svm.parameters.fs = list()
  best.svm.fit.fs = list()
  best.svm.prediction.fs = list()
}


#tuning svm parameters per 24hour model####
for(i in 1:24) {
  
  assign(paste("min.mape.", i-1, sep=""), 1000000)
  
  gammaValues = 5 *  10 ^(-7:-1) #10^(-4) #
  costValues = 2 ^ (2:12) #(6)
  
  
  for(gammaValue in gammaValues) {
    for(costValue in costValues) {
      
      cat("\n\n tuning model: Load.",i-1,"with gammaValue = ", gammaValue," costValue = ", costValue," \n")
      
      list.of.features = 
        getSelectedAttributes(final.boruta.list2[[i]], withTentative = F)
      
      
      #create the predictor variables from training
      FeaturesVariables = 
        trainSet[list.of.features]
      
      
      #add the response variable in trainSet
      FeaturesVariables[paste("Loads", i-1, sep=".")] = 
        trainSet[paste("Loads", i-1, sep=".")]
      
      
      #train a model####
      assign(paste("fit.svm", i-1, sep="."), 
             svm(as.formula(paste("Loads.", i-1, "~.", sep="")), data = FeaturesVariables, cost = costValue, gamma = gammaValue))
      
      
      FeaturesVariables[paste("Loads", i-1, sep=".")] = NULL
      
      
      
      #create the predictor.df data.frame for predictions####
      FeaturesVariables = 
        trainSet[list.of.features]
      
      predictor.df = data.frame()
      predictor.df = FeaturesVariables[0, ]
      predictor.df = rbind(predictor.df, testSet[names(testSet) %in% names(predictor.df)])
      
      
      #make the prediction
      assign(paste("prediction.svm", i-1, sep="."), predict(get(paste("fit.svm",i-1,sep=".")), predictor.df))
      
      
      #calculate mape
      temp.mape = 100 * mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.svm", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")])))
      cat("mape = ", temp.mape,"\n\n")
      
      
      temp.mae =  mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.svm", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")])))
      
      
      temp.rmse = sqrt(mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.svm", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")]))^2))
      
      
      temp.mse = mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.svm", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")]))^2)
      
      
      assign(paste("mape.svm",i-1,sep="."), temp.mape)
      assign(paste("mae.svm",i-1,sep="."), temp.mae)
      assign(paste("rmse.svm",i-1,sep="."), temp.rmse)
      assign(paste("mse.svm",i-1,sep="."), temp.mse)
      
      
      #check if this mape is less than a previous one.
      if( get(paste("min.mape.", i-1, sep="")) > get(paste("mape.svm",i-1,sep=".")) ) {
        
        cat("\n\n ***New best paramenters for Load.", i-1, " model***\n")
        cat(get(paste("mape.svm",i-1,sep=".")),"\n")
        
        cat("new best gammaValue: ", gammaValue,"\n")
        cat("new best costValue: ",costValue,"\n")
        
        
        assign(paste("min.mape.", i-1, sep=""), get(paste("mape.svm",i-1,sep=".")))
        
        
        best.svm.parameters.fs[[paste("best.svm.param.", i-1, sep="")]] = c(gammaValue, costValue, get(paste("mape.svm",i-1,sep=".")), get(paste("mae.svm",i-1,sep=".")), get(paste("rmse.svm",i-1,sep=".")), get(paste("mse.svm",i-1,sep=".")))
        names(best.svm.parameters.fs[[paste("best.svm.param.", i-1, sep="")]]) = list("gamma", "cost", paste("mape.svm",i-1,sep="."), paste("mae.svm",i-1,sep="."), paste("rmse.svm",i-1,sep="."), paste("mse.svm",i-1,sep="."))
        
        
        best.svm.fit.fs[[paste("fit.svm", i-1, sep=".")]] = get(paste("fit.svm",i-1, sep="."))
        
        best.svm.prediction.fs[[paste("prediction.svm",i-1,sep=".")]] = get(paste("prediction.svm",i-1, sep="."))
        
      }
      
      
      #saving each tuning experiments####
      if (!exists("experiments.svm")) {
        
        experiments.svm = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA, "method" = NA, "gamma" = NA, "cost" = NA, "model" = NA) 
        
        experiments.svm$features = list(list.of.features)
        
        if(length(list.of.features) != length(full.list.of.features))
          experiments.svm$method = "feature selection"
        else
          experiments.svm$method = "full.list.of.features"
        
        experiments.svm$mape = temp.mape
        experiments.svm$mae = temp.mae
        experiments.svm$mse = temp.mse
        experiments.svm$rmse = temp.rmse
        experiments.svm$gamma = gammaValue
        experiments.svm$cost = costValue
        experiments.svm$model = paste("Loads.", i-1, sep="")
        
      } else {
        temp = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA, "method" = NA, "gamma" = NA, "cost" = NA, "model" = NA)
        
        temp$features = list(list.of.features)
        
        
        if(length(list.of.features) != length(full.list.of.features))
          temp$method = "feature selection"
        else
          temp$method = "full.list.of.features"
        
        
        temp$mape = temp.mape
        temp$mae = temp.mae
        temp$mse = temp.mse
        temp$rmse = temp.rmse
        temp$gamma = gammaValue
        temp$cost = costValue
        temp$model = paste("Loads.", i-1, sep="")
        
        experiments.svm = rbind(experiments.svm, temp)
        rm(temp)
      }
      
      
    }
  }
  
  cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60,"\n")
  
  rm(list=ls(pattern="fit.svm."))
  rm(list=ls(pattern="prediction.svm."))
  rm(list=ls(pattern="mape.svm."))
  rm(list=ls(pattern="mae.svm."))
  rm(list=ls(pattern="mse.svm."))
  rm(list=ls(pattern="rmse.svm."))
  
} ##end of tuning


#calculate the mean mape after tuning
temp.mape = 0
temp.mae = 0
temp.mse = 0
temp.rmse = 0
for(i in 1:length(best.svm.parameters.fs)) {
  
  temp.mape  = temp.mape + best.svm.parameters.fs[[i]][[3]]
  temp.mae = temp.mae + best.svm.parameters.fs[[i]][[4]]
  temp.rmse = temp.rmse + best.svm.parameters.fs[[i]][[5]]
  temp.mse = temp.mse + best.svm.parameters.fs[[i]][[6]]
  
}

#change this variable to mean.mape.nn.full etc. for full.list.of.features
mean.mape.svm.fs = temp.mape/length(best.svm.parameters.fs)
mean.mae.svm.fs = temp.mae/length(best.svm.parameters.fs)
mean.rmse.svm.fs = temp.rmse/length(best.svm.parameters.fs)
mean.mse.svm.fs = temp.mse/length(best.svm.parameters.fs)

cat("\n****************\n")
cat("mean nn mape: ", round(mean.mape.svm.fs,3), "\n")
cat("mean nn mae: ", round(mean.mae.svm.fs,5), "\n")
cat("mean nn mse: ", round(mean.mse.svm.fs,5), "\n")
cat("mean nn rmse: ", round(mean.rmse.svm.fs,5), "\n")



rm(list=ls(pattern="min.mape."))
rm(list=ls(pattern="temp."))
rm(gammaValue)
rm(gammaValues)
rm(costValue)
rm(costValues)
rm(i)