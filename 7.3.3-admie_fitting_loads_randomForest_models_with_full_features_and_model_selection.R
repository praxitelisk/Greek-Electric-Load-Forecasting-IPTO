library("randomForest")


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


#if (!exists("best.randomForest.parameters.full")) {
best.randomForest.parameters.full = list()
best.randomForest.fit.full = list()
best.randomForest.prediction.full = list()
#}


for(i in 1:24) {
  
  assign(paste("min.mape.", i-1, sep=""), 1000000)
  
  
  list.of.features = full.list.of.features
  
  
  #create the predictor variables from training
  FeaturesVariables = 
    trainSet[list.of.features]
  
  
  for (num.of.trees in seq(10, 70, 10)) {
    for(mtry.par in seq(1, max(floor(ncol(FeaturesVariables)/3), 1), 1)) {
      
      cat("\n\n tuning model: Load.", i-1, " with num.of.trees = ", num.of.trees, " mtry = ", mtry.par,"\n\n")
      
      
      FeaturesVariables[paste("Loads", i-1, sep=".")] = 
        trainSet[paste("Loads", i-1, sep=".")]
      
      
      set.seed(123)
      assign(paste("fit.randomForest", i-1, sep="."), 
             randomForest(as.formula(paste("Loads.", i-1, "~.", sep="")), data = FeaturesVariables, ntree = num.of.trees, mtry = mtry.par))
      
      
      #create the predictor.df data.frame for predictions####
      FeaturesVariables = 
        trainSet[list.of.features]
      
      
      #create the predictor.df data.frame for predictions####
      predictor.df = data.frame()
      predictor.df = FeaturesVariables[0, ]
      predictor.df = rbind(predictor.df, evaluationSet[names(evaluationSet) %in% names(predictor.df)])
      
      
      assign(paste("prediction.randomForest", i-1, sep="."), predict(get(paste("fit.randomForest",i-1,sep=".")), predictor.df))
      
      
      #calculate mape
      temp.mape = 100 * mean(unlist(abs((get("evaluationSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.randomForest", i-1, sep=".")))/get("evaluationSet")[paste("Loads", i-1, sep=".")])))
      cat("mape = ", temp.mape,"\n\n")
      
      
      temp.mae =  mean(unlist(abs((get("evaluationSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.randomForest", i-1, sep=".")))/get("evaluationSet")[paste("Loads", i-1, sep=".")])))
      
      
      temp.rmse = sqrt(mean(unlist(abs((get("evaluationSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.randomForest", i-1, sep=".")))/get("evaluationSet")[paste("Loads", i-1, sep=".")]))^2))
      
      
      temp.mse = mean(unlist(abs((get("evaluationSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.randomForest", i-1, sep=".")))/get("evaluationSet")[paste("Loads", i-1, sep=".")]))^2)
      
      
      assign(paste("mape.randomForest",i-1,sep="."), temp.mape)
      assign(paste("mae.randomForest",i-1,sep="."), temp.mae)
      assign(paste("rmse.randomForest",i-1,sep="."), temp.rmse)
      assign(paste("mse.randomForest",i-1,sep="."), temp.mse)
      
      
      if( get(paste("min.mape.", i-1, sep="")) > get(paste("mape.randomForest",i-1,sep=".")) ) {
        
        cat("\n\n ***New best paramenters for Load.", i-1, " model***\n")
        cat(get(paste("mape.randomForest",i-1,sep=".")),"\n")
        
        cat("new best num.of.trees: ", num.of.trees,"\n")
        cat("new best mtry: ", mtry.par,"\n")
        
        
        assign(paste("min.mape.", i-1, sep=""), get(paste("mape.randomForest",i-1,sep=".")))
        
        
        best.randomForest.parameters.full[[paste("best.randomForest.param.", i-1, sep="")]] = c(num.of.trees, mtry.par, get(paste("mape.randomForest",i-1,sep=".")), get(paste("mae.randomForest",i-1,sep=".")), get(paste("rmse.randomForest",i-1,sep=".")), get(paste("mse.randomForest",i-1,sep=".")))
        names(best.randomForest.parameters.full[[paste("best.randomForest.param.", i-1, sep="")]]) = list("num.of.trees", "mtry", paste("mape.randomForest",i-1,sep="."), paste("mae.randomForest",i-1,sep="."), paste("rmse.randomForest",i-1,sep="."), paste("mse.randomForest",i-1,sep="."))
        
        
        best.randomForest.fit.full[[paste("fit.randomForest", i-1, sep=".")]] = get(paste("fit.randomForest",i-1, sep="."))
        
        best.randomForest.prediction.full[[paste("prediction.randomForest",i-1,sep=".")]] = get(paste("prediction.randomForest",i-1, sep="."))
        
      }
      
      
      ###experiments####
      #saving each tuning experiments####
      if (!exists("experiments.randomForest.ms")) {
        
        experiments.randomForest.ms = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA, "dataset" = NA, "num.of.trees" = NA, "mtry" = NA, "algorithm" = NA, "model" = NA, "date" = NA) 
        
        experiments.randomForest.ms$features = list(list.of.features)
        
        if(length(list.of.features) != length(full.list.of.features))
          experiments.randomForest.ms$dataset = "feature selection"
        else
          experiments.randomForest.ms$dataset = "full.list.of.features"
        
        experiments.randomForest.ms$mape = temp.mape
        experiments.randomForest.ms$mae = temp.mae
        experiments.randomForest.ms$mse = temp.mse
        experiments.randomForest.ms$rmse = temp.rmse
        
        experiments.randomForest.ms$num.of.trees = num.of.trees
        experiments.randomForest.ms$mtry = mtry.par
        
        experiments.randomForest.ms$algorithm = "randomForest"
        experiments.randomForest.ms$model = paste("Loads.", i-1, sep="")
        
        experiments.randomForest.ms$date = format(Sys.time(), "%d-%m-%y %H:%M:%S")
        
      } else {
        temp = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA, "dataset" = NA, "num.of.trees" = NA, "mtry" = NA, "algorithm" = NA, "model" = NA, "date" = NA)
        
        temp$features = list(list.of.features)
        
        
        if(length(list.of.features) != length(full.list.of.features))
          temp$dataset = "feature selection"
        else
          temp$dataset = "full.list.of.features"
        
        
        temp$mape = temp.mape
        temp$mae = temp.mae
        temp$mse = temp.mse
        temp$rmse = temp.rmse
        
        temp$num.of.trees = num.of.trees
        temp$mtry = mtry.par
        
        temp$algorithm = "randomForest"
        temp$model = paste("Loads.", i-1, sep="")
        
        temp$date = format(Sys.time(), "%d-%m-%y %H:%M:%S")
        
        experiments.randomForest.ms = rbind(experiments.randomForest.ms, temp)
        rm(temp)
      }
      
      cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60,"\n")
      
      
    } 
  }
  
} #end of tuning####


#create the new models after tuning and evaluation phase##################
mape.randomForest.full.ms = list()
mae.randomForest.full.ms = list()
rmse.randomForest.full.ms = list()
mse.randomForest.full.ms = list()
prediction.randomForest.full.ms = list()
fit.randomForest.full.ms = list()


for(i in 1:24) {
  
  list.of.features = full.list.of.features
  
  cat("\n\n training after evaluation model: Load.",i-1," with best num.of.trees = ", best.randomForest.parameters.full[[paste("best.randomForest.param.", i-1, sep="")]][["num.of.trees"]]," mtry = ", best.randomForest.parameters.full[[paste("best.randomForest.param.", i-1, sep="")]][["mtry"]], "\n", sep="")
  
  #create the predictor variables from training
  FeaturesVariables =
    train.and.evalSet[list.of.features]
  
  FeaturesVariables[paste("Loads", i-1, sep=".")] = 
    train.and.evalSet[paste("Loads", i-1, sep=".")]
  
  
  set.seed(123)
  assign(paste("fit.randomForest", i-1, sep="."), 
         randomForest(as.formula(paste("Loads.", i-1, "~.", sep="")), data = FeaturesVariables, ntree = best.randomForest.parameters.full[[paste("best.randomForest.param.", i-1, sep="")]][["num.of.trees"]], mtry = best.randomForest.parameters.full[[paste("best.randomForest.param.", i-1, sep="")]][["mtry"]]))
  
  
  FeaturesVariables[paste("Loads", i-1, sep=".")] = NULL
  
  
  FeaturesVariables =
    train.and.evalSet[list.of.features]
  
  
  predictor.df = data.frame()
  predictor.df = FeaturesVariables[0, ]
  predictor.df = rbind(predictor.df, testSet[names(testSet) %in% names(predictor.df)])
  
  
  assign(paste("prediction.randomForest", i-1, sep="."), predict(get(paste("fit.randomForest",i-1,sep=".")), predictor.df))
  
  
  #calculate mape
  temp.mape = 100 * mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.randomForest", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")])))
  cat("mape.", i-1 ," = ", temp.mape,"\n\n", sep = "")
  
  
  temp.mae =  mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.randomForest", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")])))
  
  
  temp.rmse = sqrt(mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.randomForest", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")]))^2))
  
  
  temp.mse = mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.randomForest", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")]))^2)
  
  
  fit.randomForest.full.ms[[paste("fit.randomForest",i-1,sep=".")]] = get(paste("fit.randomForest",i-1, sep="."))
  
  prediction.randomForest.full.ms[[paste("prediction.randomForest",i-1,sep=".")]] = get(paste("prediction.randomForest",i-1, sep="."))
  
  mape.randomForest.full.ms[[paste("mape.randomForest",i-1,sep=".")]] = temp.mape
  mae.randomForest.full.ms[[paste("mae.randomForest",i-1,sep=".")]] = temp.mae
  mse.randomForest.full.ms[[paste("mse.randomForest",i-1,sep=".")]] = temp.mse
  rmse.randomForest.full.ms[[paste("rmse.randomForest",i-1,sep=".")]] = temp.rmse  
  
  
}


#calculate the mean mape####
cat("calculate the mean mape\n")
mean.mape.randomForest.full.ms = mean(unlist(mape.randomForest.full.ms))

cat("calculate the mean mae\n")
mean.mae.randomForest.full.ms = mean(unlist(mae.randomForest.full.ms))

cat("calculate the mean mse\n")
mean.mse.randomForest.full.ms = mean(unlist(mse.randomForest.full.ms))

cat("calculate the mean rmse\n")
mean.rmse.randomForest.full.ms = mean(unlist(rmse.randomForest.full.ms))


cat("mean randomForest mape: ", round(mean.mape.randomForest.full.ms,3), "\n")
cat("mean randomForest mae: ", round(mean.mae.randomForest.full.ms,5), "\n")
cat("mean randomForest mse: ", round(mean.mse.randomForest.full.ms,5), "\n")
cat("mean randomForest rmse: ", round(mean.rmse.randomForest.full.ms,5), "\n")


cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60,"\n")



rm(list=ls(pattern="fit.randomForest.[0-9]"))
rm(list=ls(pattern="prediction.randomForest.[0-9]"))
rm(list=ls(pattern="mape.randomForest.[0-9]"))
rm(list=ls(pattern="mae.randomForest.[0-9]"))
rm(list=ls(pattern="mse.randomForest.[0-9]"))
rm(list=ls(pattern="rmse.randomForest.[0-9]"))
rm(list=ls(pattern="min.mape."))
rm(list=ls(pattern="temp."))
rm(num.of.trees)
rm(mtry.par)
rm(i)
