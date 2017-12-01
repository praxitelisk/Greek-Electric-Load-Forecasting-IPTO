#####################################################################
#### Rule based tuning with model selection from feature selection ###
#####################################################################

library("Cubist")
library("Boruta")


startTime <- proc.time()[3]

#creating the train and test set splits#################
splitEvalSet = 365
splitTestSet = splitEvalSet + 365
len = dim(final.Data.Set)[1]

#trainPart = floor(split * dim(final.Data.Set)[1])
trainSet = final.Data.Set[1:(len - splitTestSet), ]
evaluationSet = final.Data.Set[(len-splitTestSet + 1):(len - splitEvalSet), ]
train.and.evalSet = final.Data.Set[1:(len - splitEvalSet), ]
testSet = final.Data.Set[(len - splitEvalSet + 1):len, ]


####create the train, evaluation and test Set###################################

full.list.of.features = names(final.Data.Set)
full.list.of.features = full.list.of.features[-grep("^Loads|time|weekday|icon|^day.of.week$|^day.of.year$|yesterday.weather.measures.day.of.week|yesterday.weather.measures.day.of.year|temperature|windBearing.[0-9]+$", full.list.of.features)]


trainSet =
  subset(trainSet, select = grep(paste(full.list.of.features, collapse = "|"), names(trainSet)))


evaluationSet =
  subset(evaluationSet, select = grep(paste(full.list.of.features, collapse = "|"), names(evaluationSet)))


train.and.evalSet =
  subset(train.and.evalSet, select = grep(paste(full.list.of.features, collapse = "|"), names(train.and.evalSet)))


testSet =
  subset(testSet, select = grep(paste(full.list.of.features, collapse = "|"), names(testSet)))



#create the lists which store the best parameters######################################

#if (!exists("best.rule.parameters.fs")) {

rm(experiments.rule.ms)

best.rule.parameters.fs = list()
best.rule.fit.fs = list()
best.rule.prediction.fs = list()
#}


#stating grid search - model selection################################################
for(i in 1:24) {
  
  assign(paste("min.mape.", i-1, sep=""), 1000000)
  for(committee in 1:10) {
    for(unbiasedFlag in 0:1) {
    
    
      cat("\n\n tuning rule model: Load.", i-1 ," with unbiasedFlag", unbiasedFlag ," with committee", committee ,"\n\n")
      
      list.of.features = getSelectedAttributes(final.boruta.list2[[i]], withTentative = F)
      
      FeaturesVariables = 
        trainSet[list.of.features]
      
      
      #add the response variable in trainSet
      FeaturesVariables[paste("Loads", i-1, sep=".")] = 
        final.Data.Set[1:dim(trainSet)[1], paste("Loads", i-1, sep=".")]
      
      
      set.seed(123)
      assign(paste("fit.rule", i-1, sep="."), 
             cubist(x = FeaturesVariables[-grep(paste("^Loads", i-1, sep="."), names(FeaturesVariables))], y = FeaturesVariables[[paste("Loads", i-1, sep=".")]], committees = committee, cubistControl(unbiased = unbiasedFlag)))
      
      
      FeaturesVariables[paste("Loads", i-1, sep=".")] = NULL
      
      
      #create the predictor.df data.frame for predictions####
      FeaturesVariables = 
        trainSet[list.of.features]
      
      
      predictor.df = data.frame()
      predictor.df = FeaturesVariables[0, ]
      predictor.df = rbind(predictor.df, evaluationSet[names(evaluationSet) %in% names(predictor.df)])
      
      
      evaluationSet[paste("Loads", i-1, sep=".")] = 
        final.Data.Set[(len - splitTestSet + 1):(len - splitEvalSet), paste("Loads", i-1, sep=".")]
      
      
      assign(paste("prediction.rule", i-1, sep="."), predict(get(paste("fit.rule",i-1,sep=".")), predictor.df))
      
      
      #calculate mape
      temp.mape = 100 * mean(unlist(abs((get("evaluationSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.rule", i-1, sep=".")))/get("evaluationSet")[paste("Loads", i-1, sep=".")])))
      cat("mape = ", temp.mape,"\n\n")
      
      temp.mae =  mean(unlist(abs((get("evaluationSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.rule", i-1, sep=".")))/get("evaluationSet")[paste("Loads", i-1, sep=".")])))
      
      
      temp.rmse = sqrt(mean(unlist(abs((get("evaluationSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.rule", i-1, sep=".")))/get("evaluationSet")[paste("Loads", i-1, sep=".")]))^2))
      
      
      temp.mse = mean(unlist(abs((get("evaluationSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.rule", i-1, sep=".")))/get("evaluationSet")[paste("Loads", i-1, sep=".")]))^2)
      
      
      assign(paste("mape.rule",i-1,sep="."), temp.mape)
      assign(paste("mae.rule",i-1,sep="."), temp.mae)
      assign(paste("rmse.rule",i-1,sep="."), temp.rmse)
      assign(paste("mse.rule",i-1,sep="."), temp.mse)
      
      
      if( get(paste("min.mape.", i-1, sep="")) > get(paste("mape.rule",i-1,sep=".")) ) {
        
        cat("\n\n ***New best paramenters for Load.", i-1, " model***\n")
        cat(get(paste("mape.rule",i-1,sep=".")),"\n")
        
        cat("new best committee: ", committee,"\n")
        cat("new best unbiasedFlag: ", unbiasedFlag,"\n")
        
        
        assign(paste("min.mape.", i-1, sep=""), get(paste("mape.rule",i-1,sep=".")))
        
        
        best.rule.parameters.fs[[paste("best.rule.param.", i-1, sep="")]] = c(unbiasedFlag, committee, get(paste("mape.rule",i-1,sep=".")), get(paste("mae.rule",i-1,sep=".")), get(paste("rmse.rule",i-1,sep=".")), get(paste("mse.rule",i-1,sep=".")))
        names(best.rule.parameters.fs[[paste("best.rule.param.", i-1, sep="")]]) = list("unbiased", "committee", paste("mape.rule",i-1,sep="."), paste("mae.rule",i-1,sep="."), paste("rmse.rule",i-1,sep="."), paste("mse.rule",i-1,sep="."))
        
        
        best.rule.fit.fs[[paste("fit.rule", i-1, sep=".")]] = get(paste("fit.rule",i-1, sep="."))
        
        best.rule.prediction.fs[[paste("prediction.rule",i-1,sep=".")]] = get(paste("prediction.rule",i-1, sep="."))
        
      }
      
      ###experiments####
      #saving each tuning experiments####
      if (!exists("experiments.rule.ms")) {
        
        experiments.rule.ms = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA, "dataset" = NA, "biased" = NA, "committee" = NA, "algorithm" = NA, "model" = NA, "date" = NA) 
        
        experiments.rule.ms$features = list(list.of.features)
        
        if(length(list.of.features) != length(full.list.of.features))
          experiments.rule.ms$dataset = "feature selection"
        else
          experiments.rule.ms$dataset = "full.list.of.features"
        
        experiments.rule.ms$mape = temp.mape
        experiments.rule.ms$mae = temp.mae
        experiments.rule.ms$mse = temp.mse
        experiments.rule.ms$rmse = temp.rmse
        experiments.rule.ms$committee = committee
        experiments.rule.ms$biased = unbiasedFlag
        experiments.rule.ms$algorithm = "rule"
        experiments.rule.ms$model = paste("Loads.", i-1, sep="")
        experiments.rule.ms$date = format(Sys.time(), "%d-%m-%y %H:%M:%S")
        
      } else {
        temp = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "features" = NA, "dataset" = NA, "biased" = NA, "committee" = committee, "algorithm" = NA, "model" = NA, "date" = NA) 
        
        temp$features = list(list.of.features)
        
        
        if(length(list.of.features) != length(full.list.of.features))
          temp$dataset = "feature selection"
        else
          temp$dataset = "full.list.of.features"
        
        
        temp$mape = temp.mape
        temp$mae = temp.mae
        temp$mse = temp.mse
        temp$rmse = temp.rmse
        temp$committee = committee
        temp$biased = unbiasedFlag
        temp$algorithm = "rule"
        temp$model = paste("Loads.", i-1, sep="")
        temp$date = format(Sys.time(), "%d-%m-%y %H:%M:%S")
        
        experiments.rule.ms = rbind(experiments.rule.ms, temp)
        rm(temp)
      }
      
      
      evaluationSet[paste("Loads", i-1, sep=".")] = NULL
      
      
      cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60,"\n")
    }
  }
  
  
} ###end of tuning####



#create the new models after tuning and evaluation phase##################
mape.rule.fs.ms = list()
mae.rule.fs.ms = list()
rmse.rule.fs.ms = list()
mse.rule.fs.ms = list()
prediction.rule.fs.ms = list()
fit.rule.fs.ms = list()


for(i in 1:24) {
  
  list.of.features =
    getSelectedAttributes(final.boruta.list2[[i]], withTentative = F)
  
  cat("\n\n training rules after evaluation model: Load.", i-1, ", with best unbiasedFlag = ", best.rule.parameters.fs[[paste("best.rule.param.", i-1, sep="")]][["unbiased"]], ", best committee = ", best.rule.parameters.fs[[paste("best.rule.param.", i-1, sep="")]][["committee"]] ,"\n", sep="")
  
  #create the predictor variables from training
  FeaturesVariables =
    train.and.evalSet[list.of.features]
  
  #add the response variable in trainSet
  FeaturesVariables[paste("Loads", i-1, sep=".")] = 
    final.Data.Set[1:dim(train.and.evalSet)[1], paste("Loads", i-1, sep=".")]
  
  
  set.seed(123)
  assign(paste("fit.rule", i-1, sep="."), 
         cubist(x = FeaturesVariables[-grep(paste("^Loads", i-1, sep="."), names(FeaturesVariables))], y = FeaturesVariables[[paste("Loads", i-1, sep=".")]], committees = best.rule.parameters.fs[[paste("best.rule.param.", i-1, sep="")]][["committee"]], cubistControl(unbiased = best.rule.parameters.fs[[paste("best.rule.param.", i-1, sep="")]][["unbiased"]])))
  
  
  FeaturesVariables[paste("Loads", i-1, sep=".")] = NULL
  
  
  #make the prediction from train-eval set###########################
  FeaturesVariables =
    train.and.evalSet[list.of.features]
  
  
  predictor.df = data.frame()
  predictor.df = FeaturesVariables[0, ]
  predictor.df = rbind(predictor.df, testSet[names(testSet) %in% names(predictor.df)])
  
  
  testSet[paste("Loads", i-1, sep=".")] = 
    final.Data.Set[(len - splitEvalSet + 1):len, paste("Loads", i-1, sep=".")]
  
  
  #make the prediction
  assign(paste("prediction.rule", i-1, sep="."), predict(get(paste("fit.rule",i-1,sep=".")), predictor.df))
  
  
  #calculate mape
  temp.mape = 100 * mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.rule", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")])))
  cat("mape.", i-1 ," = ", temp.mape,"\n\n", sep = "")
  
  
  temp.mae =  mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.rule", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")])))
  
  
  temp.rmse = sqrt(mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.rule", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")]))^2))
  
  
  temp.mse = mean(unlist(abs((get("testSet")[paste("Loads", i-1, sep=".")] - get(paste("prediction.rule", i-1, sep=".")))/get("testSet")[paste("Loads", i-1, sep=".")]))^2)
  
  
  fit.rule.fs.ms[[paste("fit.rule",i-1,sep=".")]] = get(paste("fit.rule",i-1, sep="."))
  
  prediction.rule.fs.ms[[paste("prediction.rule",i-1,sep=".")]] = get(paste("prediction.rule",i-1, sep="."))
  
  mape.rule.fs.ms[[paste("mape.rule",i-1,sep=".")]] = temp.mape
  mae.rule.fs.ms[[paste("mae.rule",i-1,sep=".")]] = temp.mae
  mse.rule.fs.ms[[paste("mse.rule",i-1,sep=".")]] = temp.mse
  rmse.rule.fs.ms[[paste("rmse.rule",i-1,sep=".")]] = temp.rmse
  
  
  testSet[paste("Loads", i-1, sep=".")] = NULL
}


#calculate the mean mape####
cat("calculate the mean mape\n")
mean.mape.rule.fs.ms = mean(unlist(mape.rule.fs.ms))

cat("calculate the mean mae\n")
mean.mae.rule.fs.ms = mean(unlist(mae.rule.fs.ms))

cat("calculate the mean mse\n")
mean.mse.rule.fs.ms = mean(unlist(mse.rule.fs.ms))

cat("calculate the mean rmse\n")
mean.rmse.rule.fs.ms = mean(unlist(rmse.rule.fs.ms))


cat("mean rule mape: ", round(mean.mape.rule.fs.ms,3), "\n")
cat("mean rule mae: ", round(mean.mae.rule.fs.ms,5), "\n")
cat("mean rule mse: ", round(mean.mse.rule.fs.ms,5), "\n")
cat("mean rule rmse: ", round(mean.rmse.rule.fs.ms,5), "\n")


cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60,"\n")



rm(list=ls(pattern="fit.rule.[0-9]"))
rm(list=ls(pattern="prediction.rule.[0-9]"))
rm(list=ls(pattern="mape.rule.[0-9]"))
rm(list=ls(pattern="mae.rule.[0-9]"))
rm(list=ls(pattern="mse.rule.[0-9]"))
rm(list=ls(pattern="rmse.rule.[0-9]"))
rm(list=ls(pattern="min.mape."))
rm(list=ls(pattern="temp."))
rm(unbiasedFlag)
rm(i)

