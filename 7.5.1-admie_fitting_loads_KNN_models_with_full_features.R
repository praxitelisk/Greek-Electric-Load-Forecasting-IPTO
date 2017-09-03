####################################################################
#####KNN with full features and default parameters##############
####################################################################


mape.knn.full.def = list()
mae.knn.full.def = list()
rmse.knn.full.def = list()
mse.knn.full.def = list()
prediction.knn.full.def = list()
fit.knn.full.def = list()

startTime <- proc.time()[3]

for(i in 1:24) {
  
  list.of.features = full.list.of.features
  
  
  cat("\n\n knn training with feature selection and default parameters \n", sep="")
  
  
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
         knn.reg(FeaturesVariables, test = NULL, y = unlist(train.and.evalSet[paste("Loads", i-1, sep=".")])))
  
  
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
         knn.reg(FeaturesVariables, test = predictor.df, y=unlist(train.and.evalSet[paste("Loads", i-1, sep=".")])))        
  
  
  #calculate mape
  temp.mape = 100 * mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.knn", i-1, sep="."))$pred)/get("testSet")[paste("Loads", i-1, sep=".")])))
  cat("mape.", i-1 ," = ", temp.mape,"\n\n", sep = "")
  
  
  temp.mae =  mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.knn", i-1, sep="."))$pred)/get("testSet")[paste("Loads", i-1, sep=".")])))
  
  
  temp.rmse = sqrt(mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.knn", i-1, sep="."))$pred)/get("testSet")[paste("Loads", i-1, sep=".")]))^2))
  
  
  temp.mse = mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.knn", i-1, sep="."))$pred)/get("testSet")[paste("Loads", i-1, sep=".")]))^2)
  
  
  fit.knn.full.def[[paste("fit.knn",i-1,sep=".")]] = get(paste("fit.knn",i-1, sep="."))
  
  prediction.knn.full.def[[paste("prediction.knn",i-1,sep=".")]] = get(paste("prediction.knn",i-1, sep="."))
  
  mape.knn.full.def[[paste("mape.knn",i-1,sep=".")]] = temp.mape
  mae.knn.full.def[[paste("mae.knn",i-1,sep=".")]] = temp.mae
  mse.knn.full.def[[paste("mse.knn",i-1,sep=".")]] = temp.mse
  rmse.knn.full.def[[paste("rmse.knn",i-1,sep=".")]] = temp.rmse  
  
  
}


#calculate the mean mape####
cat("calculate the mean mape\n")
mean.mape.knn.full.def = mean(unlist(mape.knn.full.def))

cat("calculate the mean mae\n")
mean.mae.knn.full.def = mean(unlist(mae.knn.full.def))

cat("calculate the mean mse\n")
mean.mse.knn.full.def = mean(unlist(mse.knn.full.def))

cat("calculate the mean rmse\n")
mean.rmse.knn.full.def = mean(unlist(rmse.knn.full.def))


cat("mean knn mape: ", round(mean.mape.knn.full.def,3), "\n")
cat("mean knn mae: ", round(mean.mae.knn.full.def,5), "\n")
cat("mean knn mse: ", round(mean.mse.knn.full.def,5), "\n")
cat("mean knn rmse: ", round(mean.rmse.knn.full.def,5), "\n")


cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60,"\n")



rm(list=ls(pattern="fit.knn.[0-9]"))
rm(list=ls(pattern="prediction.knn.[0-9]"))
rm(list=ls(pattern="mape.knn.[0-9]"))
rm(list=ls(pattern="mae.knn.[0-9]"))
rm(list=ls(pattern="mse.knn.[0-9]"))
rm(list=ls(pattern="rmse.knn.[0-9]"))
rm(list=ls(pattern="min.mape."))
rm(list=ls(pattern="temp."))
rm(i)