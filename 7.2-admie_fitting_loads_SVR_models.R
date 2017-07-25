#load SVM - SVR library
library("e1071")
library("Boruta")

startTime <- proc.time()[3]

#creating the train and test set####
split = 2 * 365

#trainPart = floor(split * dim(final.Data.Set)[1])
trainSet = final.Data.Set[1:(dim(final.Data.Set)[1]-split), ]
testSet = final.Data.Set[(dim(final.Data.Set)[1] - split + 1):dim(final.Data.Set)[1], ]


#list of arguments for experiments####
kernelType.List = c("radial", "sigmoid", "polynomial", "linear")
kernelType = kernelType.List[1]

reg.type.List = c("eps-regression", "nu-regression")
reg.type = reg.type.List[1]

degreeType = 1
gammaValues = 5*10^(-6:2)
costValues = 2^(-3:9) 
crossValue = 5
#, kernel = kernelType, degree = degreeType, gamma = gammaValue

for(costValue in costValues) {
  for(gammaValue in gammaValues) {

    cat("\n\ncost = ", costValue, ", gamma = ", gammaValue,"\n\n")
    
    #create 24 models####
    for(i in 1:24) {
      
      
      list.of.features = 
        getSelectedAttributes(final.boruta.list[[i]], withTentative = F)
      
      
      cat("\n svm model at",  i-1 ," o' clock, with the following combination of features:\n\n",list.of.features,"\n\n")
      
      
      #create the predictor variables from training
      FeaturesVariables = 
        subset(trainSet, select = grep(paste(list.of.features, collapse = "|"), names(trainSet)))
      
      #add the response variable in trainSet
      FeaturesVariables[paste("Loads", i-1, sep=".")] = 
        trainSet[paste("Loads", i-1, sep=".")]
      
      assign(paste("fit.svm", i-1, sep="."), 
             svm(as.formula(paste("Loads.", i-1, "~.", sep="")), data = FeaturesVariables, cost = costValue, gamma = gammaValue))
    
      FeaturesVariables[paste("Loads", i-1, sep=".")] = NULL  
    }
    
    
    #collecting the fits in a list####
    cat("collecting the fits.svm in a list\n")
    fit.svm = list()
    for(i in 1:24) {
      fit.svm[[paste("fit.svm",i-1,sep=".")]] = get(paste("fit.svm",i-1, sep="."))
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
      
      
      assign(paste("prediction.svm", i-1, sep="."), predict(fit.svm[[paste("fit.svm",i-1,sep=".")]], predictor.df))
    }
    
    
    #printing the models' summaries####
    for(i in 1:24) {
      print(summary(fit.svm[[i]]))
    }
    
    
    #and then collect the predictions in a list####
    cat("collecting the predictions in a list\n")
    prediction.svm = list()
    for(i in 1:24) {
      prediction.svm[[paste("prediction.svm",i-1,sep=".")]] = get(paste("prediction.svm",i-1, sep="."))
    }
    
    
    #calculate mape.svm per hour####
    cat("calculate mape.svm per hour\n")
    mape.svm = list()
    for(i in 1:24) {
      mape.svm[[paste("mape.svm",i-1,sep=".")]] = 100 * mean(unlist(abs(get("testSet")[paste("Loads", i-1, sep=".")] - prediction.svm[[paste("prediction.svm", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")]))
    }
    
    #calculate mae.svm per hour####
    cat("calculate mae.svm per hour\n")
    mae.svm = list()
    for(i in 1:24) {
      mae.svm[[paste("mae.svm",i-1,sep=".")]] = mean(unlist(abs(get("testSet")[paste("Loads", i-1, sep=".")] - prediction.svm[[paste("prediction.svm", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")]))
    }
    
    #calculate rmse.svm per hour####
    cat("calculate rmse.svm per hour\n")
    rmse.svm = list()
    for(i in 1:24) {
      rmse.svm[[paste("rmse.svm",i-1,sep=".")]] = sqrt(mean(unlist((get("testSet")[paste("Loads", i-1, sep=".")] - prediction.svm[[paste("prediction.svm", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")])^2))
    }
    
    #calculate mse.svm per hour####
    cat("calculate mse.svm per hour\n")
    mse.svm = list()
    for(i in 1:24) {
      mse.svm[[paste("mse.svm",i-1,sep=".")]] = mean(unlist((get("testSet")[paste("Loads", i-1, sep=".")] - prediction.svm[[paste("prediction.svm", i-1, sep=".")]])/get("testSet")[paste("Loads", i-1, sep=".")])^2)
    }
    
    
    #calculate the mean mape####
    cat("calculate the mean mape\n")
    mean.mape.svm = mean(unlist(mape.svm))
    
    cat("calculate the mean mae\n")
    mean.mae.svm = mean(unlist(mae.svm))
    
    cat("calculate the mean mse\n")
    mean.mse.svm = mean(unlist(mse.svm))
    
    cat("calculate the mean rmse\n")
    mean.rmse.svm = mean(unlist(rmse.svm))
    
    cat("\n\ncost = ", costValue, ", gamma = ", gammaValue,"\n\n")
    cat("mean svm mape: ", round(mean.mape.svm,3), "\n")
    cat("mean svm mae: ", round(mean.mae.svm,5), "\n")
    cat("mean svm mse: ", round(mean.mse.svm,5), "\n")
    cat("mean svm rmse: ", round(mean.rmse.svm,5), "\n")
    
    
    #saving the experiments####
    if (!exists("experiments_svm")) {
      experiments_svm = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "gamma" = NA, "cost" = NA) 
      
      experiments_svm$mape = mean.mape.svm
      experiments_svm$mae = mean.mae.svm
      experiments_svm$mse = mean.mse.svm
      experiments_svm$rmse = mean.rmse.svm
      
      experiments_svm$gamma = gammaValue
      experiments_svm$cost = costValue
      
    } else {
      temp = data.frame("mape" = NA, "mae" = NA, "mse" = NA, "rmse" = NA, "gamma" = NA, "cost" = NA)
      
      
      
      temp$mape = mean.mape.svm
      temp$mae = mean.mae.svm
      temp$mse = mean.mse.svm
      temp$rmse = mean.rmse.svm
      
      temp$gamma = gammaValue
      temp$cost = costValue
      
      experiments_svm = rbind(experiments_svm, temp)
      rm(temp)
    }
    
    cat("\n elapsed time in minutes: ", (proc.time()[3]-startTime)/60, "\n\n")
    
    #remove some variables####
    rm(list=ls(pattern="fit.svm."))
    rm(list=ls(pattern="prediction.svm."))
    rm(list=ls(pattern="mape.svm."))
    rm(list=ls(pattern="mae.svm."))
    rm(list=ls(pattern="mse.svm."))
    rm(list=ls(pattern="rmse.svm."))
    rm(kernelType.List)
    rm(kernelType)
    rm(degreeType)
    rm(reg.type.List)
    rm(reg.type)

  }
}


rm(gammaValue)
rm(gammaValues)
rm(costValue)
rm(costValues)
rm(crossValue)
rm(i)

