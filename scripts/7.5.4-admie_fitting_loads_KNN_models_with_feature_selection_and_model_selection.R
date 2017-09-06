##################################################################
#####KNN tuning with model selection from feature selection#######
##################################################################


library("FNN")
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


#if (!exists("best.knn.parameters.fs")) {
best.knn.parameters.fs = list()
best.knn.fit.fs = list()
best.knn.prediction.fs = list()
#}

algorithm.list = c("kd_tree", "cover_tree", "brute")

for(i in 1:24) {
  
  assign(paste("min.mape.", i-1, sep=""), 1000000)
  
  for(j in 1:length(algorithm.list)) {
    for(neighbors in seq(1, 40, 1)) {
        
        
      cat("\n\n tuning knn model: Load.", i-1, " with neighbors = ", neighbors, ", algo = ", algorithm.list[j],"\n\n")
        
        
      list.of.features = getSelectedAttributes(final.boruta.list2[[i]], withTentative = F)
        
        
      #create the FeaturesVariables variables from training
      FeaturesVariables = 
        trainSet[list.of.features]
      
      
      #convert all factors to numeric because knn can't handle factor variables####
      for(k in 1:ncol(FeaturesVariables)) {
        
        if(class(FeaturesVariables[, k]) == "factor") {
          FeaturesVariables[, k] = as.numeric(FeaturesVariables[, k])
          
        } else {
          FeaturesVariables[, k] = FeaturesVariables[, k]
        }
        
      }
      
      #train the model and initialize random variables seed
      set.seed(123)
      assign(paste("fit.knn", i-1, sep="."), 
             knn.reg(FeaturesVariables, test = NULL, y = unlist(trainSet[paste("Loads", i-1, sep=".")]), k=neighbors, algorithm = algorithm.list[j]))
      
      
      FeaturesVariables[paste("Loads", i-1, sep=".")] = NULL
      
      
      #create the predictor.df data.frame for predictions####
      FeaturesVariables = 
        trainSet[list.of.features]
      
      
      #create the predictor.df data.frame for predictions####
      predictor.df = data.frame()
      predictor.df = FeaturesVariables[0, ]
      predictor.df = rbind(predictor.df, evaluationSet[names(evaluationSet) %in% names(predictor.df)])
      
      
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
      
      
      #calculate mape
      temp.mape = 100 * mean(unlist(abs((get("evaluationSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.knn", i-1, sep="."))$pred)/get("evaluationSet")[paste("Loads", i-1, sep=".")])))
      cat("mape = ", temp.mape,"\n\n")
      
      
      temp.mae =  mean(unlist(abs((get("evaluationSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.knn", i-1, sep="."))$pred)/get("evaluationSet")[paste("Loads", i-1, sep=".")])))
      
      
      temp.rmse = sqrt(mean(unlist(abs((get("evaluationSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.knn", i-1, sep="."))$pred)/get("evaluationSet")[paste("Loads", i-1, sep=".")]))^2))
      
      
      temp.mse = mean(unlist(abs((get("evaluationSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.knn", i-1, sep="."))$pred)/get("evaluationSet")[paste("Loads", i-1, sep=".")]))^2)
      
      
      assign(paste("mape.knn",i-1,sep="."), temp.mape)
      assign(paste("mae.knn",i-1,sep="."), temp.mae)
      assign(paste("rmse.knn",i-1,sep="."), temp.rmse)
      assign(paste("mse.knn",i-1,sep="."), temp.mse)
      
      
      if( get(paste("min.mape.", i-1, sep="")) > get(paste("mape.knn",i-1,sep=".")) ) {
        
        cat("\n\n ***New best paramenters for Load.", i-1, " model***\n")
        cat(get(paste("mape.knn",i-1,sep=".")),"\n")
        
        cat("new best neighbors: ", neighbors,"\n")
        cat("new best algorithm: ", algorithm.list[j],"\n")
        
        
        assign(paste("min.mape.", i-1, sep=""), get(paste("mape.knn",i-1,sep=".")))
        
        
        best.knn.parameters.fs[[paste("best.knn.param.", i-1, sep="")]] = c(neighbors, algorithm.list[j], get(paste("mape.knn",i-1,sep=".")), get(paste("mae.knn",i-1,sep=".")), get(paste("rmse.knn",i-1,sep=".")), get(paste("mse.knn",i-1,sep=".")))
        names(best.knn.parameters.fs[[paste("best.knn.param.", i-1, sep="")]]) = list("neighbors", "algo", paste("mape.knn",i-1,sep="."), paste("mae.knn",i-1,sep="."), paste("rmse.knn",i-1,sep="."), paste("mse.knn",i-1,sep="."))
        
        
        best.knn.fit.fs[[paste("fit.knn", i-1, sep=".")]] = get(paste("fit.knn",i-1, sep="."))
        
        best.knn.prediction.fs[[paste("prediction.knn",i-1,sep=".")]] = get(paste("prediction.knn",i-1, sep="."))
        
      }
      
      
      ###experiments####
      #saving each tuning experiments####
      if (!exists("experiments.knn.ms")) {
        
        experiments.knn.ms = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA, "dataset" = NA, "neighbors" = NA, "algo" = NA, "algorithm" = NA, "model" = NA, "date" = NA) 
        
        experiments.knn.ms$features = list(list.of.features)
        
        if(length(list.of.features) != length(full.list.of.features))
          experiments.knn.ms$dataset = "feature selection"
        else
          experiments.knn.ms$dataset = "full.list.of.features"
        
        experiments.knn.ms$mape = temp.mape
        experiments.knn.ms$mae = temp.mae
        experiments.knn.ms$mse = temp.mse
        experiments.knn.ms$rmse = temp.rmse
        experiments.knn.ms$neighbors = neighbors
        experiments.knn.ms$algo = algorithm.list[j]
        experiments.knn.ms$algorithm = "knn"
        experiments.knn.ms$model = paste("Loads.", i-1, sep="")
        experiments.knn.ms$date = format(Sys.time(), "%d-%m-%y %H:%M:%S")
        
      } else {
        temp = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA, "dataset" = NA, "neighbors" = NA, "algo" = NA, "algorithm" = NA, "model" = NA, "date" = NA) 
        
        temp$features = list(list.of.features)
        
        
        if(length(list.of.features) != length(full.list.of.features))
          temp$dataset = "feature selection"
        else
          temp$dataset = "full.list.of.features"
        
        
        temp$mape = temp.mape
        temp$mae = temp.mae
        temp$mse = temp.mse
        temp$rmse = temp.rmse
        temp$neighbors = neighbors
        temp$algo = algorithm.list[j]
        temp$algorithm = "knn"
        temp$model = paste("Loads.", i-1, sep="")
        temp$date = format(Sys.time(), "%d-%m-%y %H:%M:%S")
        
        experiments.knn.ms = rbind(experiments.knn.ms, temp)
        rm(temp)
      }
      
      cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60,"\n")
      
      
    }     
  }
  
} #end of tuning####

#create the new models after tuning and evaluation phase##################
mape.knn.fs.ms = list()
mae.knn.fs.ms = list()
rmse.knn.fs.ms = list()
mse.knn.fs.ms = list()
prediction.knn.fs.ms = list()
fit.knn.fs.ms = list()

for(i in 1:24) {
  
  list.of.features =
    getSelectedAttributes(final.boruta.list2[[i]], withTentative = F)
  
  
  cat("\n\n knn training after evaluation model: Load.",i-1," with best neighbors = ", best.knn.parameters.fs[[paste("best.knn.param.", i-1, sep="")]][["neighbors"]]," algo = ", best.knn.parameters.fs[[paste("best.knn.param.", i-1, sep="")]][["algo"]], "\n", sep="")
  
  
  #create the FeaturesVariables variables from training
  FeaturesVariables = 
    train.and.evalSet[list.of.features]
  
  
  #convert all factors to numeric because knn can't handle factor variables####
  for(k in 1:ncol(FeaturesVariables)) {
    
    if(class(FeaturesVariables[, k]) == "factor") {
      FeaturesVariables[, k] = as.numeric(FeaturesVariables[, k])
      
    } else {
      FeaturesVariables[, k] = FeaturesVariables[, k]
    }
    
  }
  
  #train the model and initialize random variables seed
  set.seed(123)
  assign(paste("fit.knn", i-1, sep="."), 
         knn.reg(FeaturesVariables, test = NULL, y = unlist(train.and.evalSet[paste("Loads", i-1, sep=".")]), k=as.numeric(best.knn.parameters.fs[[paste("best.knn.param.", i-1, sep="")]][["neighbors"]]), algorithm = best.knn.parameters.fs[[paste("best.knn.param.", i-1, sep="")]][["algo"]]))
  
  
  FeaturesVariables[paste("Loads", i-1, sep=".")] = NULL
  
  

  #preparing for predictions:
  FeaturesVariables =
    train.and.evalSet[list.of.features]
  
  
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
  rm(k)
  
  assign(paste("prediction.knn", i-1, sep="."), 
         knn.reg(FeaturesVariables, test = predictor.df, y=unlist(train.and.evalSet[paste("Loads", i-1, sep=".")]), k=as.numeric(best.knn.parameters.fs[[paste("best.knn.param.", i-1, sep="")]][["neighbors"]]), algorithm = best.knn.parameters.fs[[paste("best.knn.param.", i-1, sep="")]][["algo"]]))        
  
  
  #calculate mape
  temp.mape = 100 * mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.knn", i-1, sep="."))$pred)/get("testSet")[paste("Loads", i-1, sep=".")])))
  cat("mape.", i-1 ," = ", temp.mape,"\n\n", sep = "")
  
  
  temp.mae =  mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.knn", i-1, sep="."))$pred)/get("testSet")[paste("Loads", i-1, sep=".")])))
  
  
  temp.rmse = sqrt(mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.knn", i-1, sep="."))$pred)/get("testSet")[paste("Loads", i-1, sep=".")]))^2))
  
  
  temp.mse = mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.knn", i-1, sep="."))$pred)/get("testSet")[paste("Loads", i-1, sep=".")]))^2)
  
  
  fit.knn.fs.ms[[paste("fit.knn",i-1,sep=".")]] = get(paste("fit.knn",i-1, sep="."))
  
  prediction.knn.fs.ms[[paste("prediction.knn",i-1,sep=".")]] = get(paste("prediction.knn",i-1, sep="."))
  
  mape.knn.fs.ms[[paste("mape.knn",i-1,sep=".")]] = temp.mape
  mae.knn.fs.ms[[paste("mae.knn",i-1,sep=".")]] = temp.mae
  mse.knn.fs.ms[[paste("mse.knn",i-1,sep=".")]] = temp.mse
  rmse.knn.fs.ms[[paste("rmse.knn",i-1,sep=".")]] = temp.rmse  
  
  
}


#calculate the mean mape####
cat("calculate the mean mape\n")
mean.mape.knn.fs.ms = mean(unlist(mape.knn.fs.ms))

cat("calculate the mean mae\n")
mean.mae.knn.fs.ms = mean(unlist(mae.knn.fs.ms))

cat("calculate the mean mse\n")
mean.mse.knn.fs.ms = mean(unlist(mse.knn.fs.ms))

cat("calculate the mean rmse\n")
mean.rmse.knn.fs.ms = mean(unlist(rmse.knn.fs.ms))


cat("mean knn mape: ", round(mean.mape.knn.fs.ms,3), "\n")
cat("mean knn mae: ", round(mean.mae.knn.fs.ms,5), "\n")
cat("mean knn mse: ", round(mean.mse.knn.fs.ms,5), "\n")
cat("mean knn rmse: ", round(mean.rmse.knn.fs.ms,5), "\n")


cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60,"\n")



rm(list=ls(pattern="fit.knn.[0-9]"))
rm(list=ls(pattern="prediction.knn.[0-9]"))
rm(list=ls(pattern="mape.knn.[0-9]"))
rm(list=ls(pattern="mae.knn.[0-9]"))
rm(list=ls(pattern="mse.knn.[0-9]"))
rm(list=ls(pattern="rmse.knn.[0-9]"))
rm(list=ls(pattern="min.mape."))
rm(list=ls(pattern="temp."))
rm(neighbors)
rm(algorithm.list)
rm(j)
rm(i)