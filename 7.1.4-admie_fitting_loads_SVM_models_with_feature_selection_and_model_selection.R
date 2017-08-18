##############################################################
#####SVM tuning and model selection from feature selection####
##############################################################

#load the libraries####
library("Boruta")
library("e1071")

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
#if (!exists("best.svm.parameters.fs")) {
  best.svm.parameters.fs = list()
  best.svm.fit.fs = list()
  best.svm.prediction.fs = list()
#}



for(i in 1:24) {
  
  assign(paste("min.mape.", i-1, sep=""), 1000000)
  
  gammaValues = 5 *  10 ^(-5:-2) #10^(-4) #
  costValues = 2 ^ (2:9) #(6)
  
  
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
      predictor.df = rbind(predictor.df, evaluationSet[names(evaluationSet) %in% names(predictor.df)])
      
      
      #make the prediction
      assign(paste("prediction.svm", i-1, sep="."), predict(get(paste("fit.svm",i-1,sep=".")), predictor.df))
  
    
      #calculate mape
      temp.mape = 100 * mean(unlist(abs((get("evaluationSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.svm", i-1, sep=".")))/get("evaluationSet")[paste("Loads", i-1, sep=".")])))
      cat("mape = ", temp.mape,"\n\n")
      
      
      temp.mae =  mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.svm", i-1, sep=".")))/get("evaluationSet")[paste("Loads", i-1, sep=".")])))
      
      
      temp.rmse = sqrt(mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.svm", i-1, sep=".")))/get("evaluationSet")[paste("Loads", i-1, sep=".")]))^2))
      
      
      temp.mse = mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.svm", i-1, sep=".")))/get("evaluationSet")[paste("Loads", i-1, sep=".")]))^2)
      
      
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
      
      cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60,"\n")
      
      
      
      #saving each tuning experiments####
      if (!exists("experiments.svm.ms")) {
        
        experiments.svm.ms = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA, "method" = NA, "gamma" = NA, "cost" = NA, "algorithm" = NA, "model" = NA) 
        
        experiments.svm.ms$features = list(list.of.features)
        
        if(length(list.of.features) != length(full.list.of.features))
          experiments.svm.ms$method = "feature selection"
        else
          experiments.svm.ms$method = "full.list.of.features"
        
        experiments.svm.ms$mape = temp.mape
        experiments.svm.ms$mae = temp.mae
        experiments.svm.ms$mse = temp.mse
        experiments.svm.ms$rmse = temp.rmse
        experiments.svm.ms$gamma = gammaValue
        experiments.svm.ms$cost = costValue
        experiments.svm.ms$algorithm = "svm"
        experiments.svm.ms$model = paste("Loads.", i-1, sep="")
        
      } else {
        temp = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA, "method" = NA, "gamma" = NA, "cost" = NA, "algorithm" = NA, "model" = NA)
        
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
        temp$algorithm = "svm"
        temp$model = paste("Loads.", i-1, sep="")
        
        experiments.svm.ms = rbind(experiments.svm.ms, temp)
        rm(temp)
      }
      
    }
  }
  
  
} #end of tuning####



#create the new model after tuning and evaluation##########################################
mape.svm.fs.ms = list()
mae.svm.fs.ms = list()
rmse.svm.fs.ms = list()
mse.svm.fs.ms = list()
prediction.svm.fs.ms = list()
fit.svm.fs.ms = list()
  

for(i in 1:24) {
  
  
  list.of.features = 
    getSelectedAttributes(final.boruta.list2[[i]], withTentative = F)
  
  
  #create the predictor variables from training
  FeaturesVariables = 
    train.and.evalSet[list.of.features]
  
  
  
  #add the response variable in trainSet
  FeaturesVariables[paste("Loads", i-1, sep=".")] = 
    train.and.evalSet[paste("Loads", i-1, sep=".")]
  
  
  #train a model####
  assign(paste("fit.svm", i-1, sep="."), 
         svm(as.formula(paste("Loads.", i-1, "~.", sep="")), data = FeaturesVariables, cost = best.svm.parameters.fs[[i]][["cost"]], gamma = best.svm.parameters.fs[[i]][["gamma"]]))
  
  
  FeaturesVariables[paste("Loads", i-1, sep=".")] = NULL
  
  
  
  #make the prediction from train-eval set####
  FeaturesVariables = 
    train.and.evalSet[list.of.features]
  
  
  predictor.df = data.frame()
  predictor.df = FeaturesVariables[0, ]
  predictor.df = rbind(predictor.df, testSet[names(testSet) %in% names(predictor.df)])
  
  
  #make the prediction
  assign(paste("prediction.svm", i-1, sep="."), predict(get(paste("fit.svm",i-1,sep=".")), predictor.df))
  
  
  #calculate mape
  temp.mape = 100 * mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.svm", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")])))
  cat("mape.", i-1 ," = ", temp.mape,"\n\n", sep = "")
  
  
  temp.mae =  mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.svm", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")])))
  
  
  temp.rmse = sqrt(mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.svm", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")]))^2))
  
  
  temp.mse = mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.svm", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")]))^2)
  
  
  
  fit.svm.fs.ms[[paste("fit.svm",i-1,sep=".")]] = get(paste("fit.svm",i-1, sep="."))
  
  prediction.svm.fs.ms[[paste("prediction.svm",i-1,sep=".")]] = get(paste("prediction.svm",i-1, sep="."))
  
  mape.svm.fs.ms[[paste("mape.svm",i-1,sep=".")]] = temp.mape
  mae.svm.fs.ms[[paste("mae.svm",i-1,sep=".")]] = temp.mae
  mse.svm.fs.ms[[paste("mse.svm",i-1,sep=".")]] = temp.mse
  rmse.svm.fs.ms[[paste("rmse.svm",i-1,sep=".")]] = temp.rmse
  
  
} #end of models


#calculate the mean mape####
cat("calculate the mean mape\n")
mean.mape.svm.fs.ms = mean(unlist(mape.svm.fs.ms))

cat("calculate the mean mae\n")
mean.mae.svm.fs.ms = mean(unlist(mae.svm.fs.ms))

cat("calculate the mean mse\n")
mean.mse.svm.fs.ms = mean(unlist(mse.svm.fs.ms))

cat("calculate the mean rmse\n")
mean.rmse.svm.fs.ms = mean(unlist(rmse.svm.fs.ms))


cat("mean svm mape: ", round(mean.mape.svm.fs.ms,3), "\n")
cat("mean svm mae: ", round(mean.mae.svm.fs.ms,5), "\n")
cat("mean svm mse: ", round(mean.mse.svm.fs.ms,5), "\n")
cat("mean svm rmse: ", round(mean.rmse.svm.fs.ms,5), "\n")
  
  
cat("elapsed time in minutes: ", (proc.time()[3] - startTime)/60,"\n")



rm(list=ls(pattern="min.mape."))
rm(list=ls(pattern="temp."))
rm(gammaValue)
rm(gammaValues)
rm(costValue)
rm(costValues)
rm(i)
rm(list=ls(pattern="fit.svm.[0-9]"))
rm(list=ls(pattern="prediction.svm.[0-9]"))
rm(list=ls(pattern="mape.svm.[0-9]"))
rm(list=ls(pattern="mae.svm.[0-9]"))
rm(list=ls(pattern="mse.svm.[0-9]"))
rm(list=ls(pattern="rmse.svm.[0-9]"))
