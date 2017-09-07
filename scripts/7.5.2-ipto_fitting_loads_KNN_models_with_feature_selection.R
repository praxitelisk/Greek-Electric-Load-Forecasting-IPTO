####################################################################
#####KNN with feature selection and default parameters##############
####################################################################


mape.knn.fs.def = list()
mae.knn.fs.def = list()
rmse.knn.fs.def = list()
mse.knn.fs.def = list()
prediction.knn.fs.def = list()
fit.knn.fs.def = list()

startTime <- proc.time()[3]

for(i in 1:24) {
  
  list.of.features =
    getSelectedAttributes(final.boruta.list2[[i]], withTentative = F)
  
  
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
  
  
  fit.knn.fs.def[[paste("fit.knn",i-1,sep=".")]] = get(paste("fit.knn",i-1, sep="."))
  
  prediction.knn.fs.def[[paste("prediction.knn",i-1,sep=".")]] = get(paste("prediction.knn",i-1, sep="."))
  
  mape.knn.fs.def[[paste("mape.knn",i-1,sep=".")]] = temp.mape
  mae.knn.fs.def[[paste("mae.knn",i-1,sep=".")]] = temp.mae
  mse.knn.fs.def[[paste("mse.knn",i-1,sep=".")]] = temp.mse
  rmse.knn.fs.def[[paste("rmse.knn",i-1,sep=".")]] = temp.rmse  
  
  
}


#calculate the mean mape####
cat("calculate the mean mape\n")
mean.mape.knn.fs.def = mean(unlist(mape.knn.fs.def))

cat("calculate the mean mae\n")
mean.mae.knn.fs.def = mean(unlist(mae.knn.fs.def))

cat("calculate the mean mse\n")
mean.mse.knn.fs.def = mean(unlist(mse.knn.fs.def))

cat("calculate the mean rmse\n")
mean.rmse.knn.fs.def = mean(unlist(rmse.knn.fs.def))


cat("mean knn mape: ", round(mean.mape.knn.fs.def,3), "\n")
cat("mean knn mae: ", round(mean.mae.knn.fs.def,5), "\n")
cat("mean knn mse: ", round(mean.mse.knn.fs.def,5), "\n")
cat("mean knn rmse: ", round(mean.rmse.knn.fs.def,5), "\n")


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