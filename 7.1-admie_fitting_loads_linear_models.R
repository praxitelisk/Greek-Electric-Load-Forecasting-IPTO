startTime <- proc.time()[3]

list.of.features = getSelectedAttributes(final.boruta, withTentative = F)


#creating the train and test set####
split = 2 * 365

#trainPart = floor(split * dim(final.Data.Set)[1])
trainSet = final.Data.Set[1:(dim(final.Data.Set)[1]-split), ]
testSet = final.Data.Set[(dim(final.Data.Set)[1] - split + 1):dim(final.Data.Set)[1], ]

    
    startTime <- proc.time()[3]
    
    cat("\n lm models with the following combination of features:\n\n", list.of.features, "\n\n")
    
    
    #create the predictor variables from training
    FeaturesVariables = 
      subset(trainSet, select = grep(paste(list.of.features, collapse = "|"), names(trainSet)))
    
    
    #create the predictor.df data.frame for predictions####
    predictor.df = data.frame()
    predictor.df = FeaturesVariables[0, ]
    predictor.df = rbind(predictor.df, testSet[names(testSet) %in% names(predictor.df)])


    #list of arguments for experiments####
    step.direction.list = c("both", "backward", "forward")
    step.direction = step.direction.list[1]
    num.of.steps = 1000
    
    
    #creating the models####
    cat("creating a model for Loads at 0 o' clock\n")
    FeaturesVariables$Loads.0 = trainSet$Loads.0
    fit.lm.0 = lm(Loads.0 ~ .*, data = FeaturesVariables)
    FeaturesVariables$Loads.0 = NULL
    
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.0$xlevels
    if(length(names(temp))>0) {
      for(t in 1:length(names(temp))) {
        
        fit.lm.0$xlevels[[names(temp)[t]]] = 
          union(fit.lm.0$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
      
      }
    }
    rm(temp, t)
    
    cat("creating a model for Loads at 1 o' clock\n")
    FeaturesVariables$Loads.1 = trainSet$Loads.1
    fit.lm.1 = lm(Loads.1 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.1 = NULL
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.1$xlevels
    if(length(names(temp))>0) {
      for(t in 1:length(names(temp))) {
        
        fit.lm.1$xlevels[[names(temp)[t]]] = 
          union(fit.lm.1$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
        
      }
    }
    rm(temp, t)
  
    
    cat("creating a model for Loads at 2 o' clock\n")
    FeaturesVariables$Loads.2 = trainSet$Loads.2
    fit.lm.2 = lm(Loads.2 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.2 = NULL
    
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.2$xlevels
    if(length(names(temp))>0) {
      for(t in 1:length(names(temp))) {
        
        fit.lm.2$xlevels[[names(temp)[t]]] = 
          union(fit.lm.2$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
        
      }
    }
    rm(temp, t)
    
    
    cat("creating a model for Loads at 3 o' clock\n")
    FeaturesVariables$Loads.3 = trainSet$Loads.3
    fit.lm.3 = lm(Loads.3 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.3 = NULL
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.3$xlevels
    if(length(names(temp))>0) {
      for(t in 1:length(names(temp))) {
        
        fit.lm.3$xlevels[[names(temp)[t]]] = 
          union(fit.lm.3$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
        
      }
    }
    rm(temp, t)
    
    
    cat("creating a model for Loads at 4 o' clock\n")
    FeaturesVariables$Loads.4 = trainSet$Loads.4
    fit.lm.4 = lm(Loads.4 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.4 = NULL
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.4$xlevels
    if(length(names(temp))>0) {
      for(t in 1:length(names(temp))) {
        
        fit.lm.4$xlevels[[names(temp)[t]]] = 
          union(fit.lm.4$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
        
      }
    }
    rm(temp, t)
    
    
    cat("creating a model for Loads at 5 o' clock\n")
    FeaturesVariables$Loads.5 = trainSet$Loads.5
    fit.lm.5 = lm(Loads.5 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.5 = NULL
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.5$xlevels
    if(length(names(temp))>0) {
      for(t in 1:length(names(temp))) {
        
        fit.lm.5$xlevels[[names(temp)[t]]] = 
          union(fit.lm.5$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
        
      }
    }
    rm(temp, t)
    
    
    cat("creating a model for Loads at 6 o' clock\n")
    FeaturesVariables$Loads.6 = trainSet$Loads.6
    fit.lm.6 = lm(Loads.6 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.6 = NULL
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.6$xlevels
    if(length(names(temp))>0) {
      for(t in 1:length(names(temp))) {
        
        fit.lm.6$xlevels[[names(temp)[t]]] = 
          union(fit.lm.6$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
        
      }
    }
    rm(temp, t)
    
    
    cat("creating a model for Loads at 7 o' clock\n")
    FeaturesVariables$Loads.7 = trainSet$Loads.7
    fit.lm.7 = lm(Loads.7 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.7 = NULL
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.7$xlevels
    if(length(names(temp))>0) {
      for(t in 1:length(names(temp))) {
        
        fit.lm.7$xlevels[[names(temp)[t]]] = 
          union(fit.lm.7$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
        
      }
    }
    rm(temp, t)
    
    
    cat("creating a model for Loads at 8 o' clock\n")
    FeaturesVariables$Loads.8 = trainSet$Loads.8
    fit.lm.8 = lm(Loads.8 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.8 = NULL
    
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.8$xlevels
    if(length(names(temp))>0) {
      for(t in 1:length(names(temp))) {
        
        fit.lm.8$xlevels[[names(temp)[t]]] = 
          union(fit.lm.8$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
        
      }
    }
    rm(temp, t)
    
    
    cat("creating a model for Loads at 9 o' clock\n")
    FeaturesVariables$Loads.9 = trainSet$Loads.9
    fit.lm.9 = lm(Loads.9 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.9 = NULL
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.9$xlevels
    if(length(names(temp))>0) {
      for(t in 1:length(names(temp))) {
        
        fit.lm.9$xlevels[[names(temp)[t]]] = 
          union(fit.lm.9$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
        
      }
    }
    rm(temp, t)
    
    
    cat("creating a model for Loads at 10 o' clock\n")
    FeaturesVariables$Loads.10 = trainSet$Loads.10
    fit.lm.10 = lm(Loads.10 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.10 = NULL
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.10$xlevels
    if(length(names(temp))>0) {
      for(t in 1:length(names(temp))) {
        
        fit.lm.10$xlevels[[names(temp)[t]]] = 
          union(fit.lm.10$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
        
      }
    }
    rm(temp, t)
    
    
    cat("creating a model for Loads at 11 o' clock\n")
    FeaturesVariables$Loads.11 = trainSet$Loads.11
    fit.lm.11 = lm(Loads.11 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.11 = NULL
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.11$xlevels
    if(length(names(temp))>0) {
      for(t in 1:length(names(temp))) {
        
        fit.lm.11$xlevels[[names(temp)[t]]] = 
          union(fit.lm.11$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
        
      }
    }
    rm(temp, t)
    
    
    cat("creating a model for Loads at 12 o' clock\n")
    FeaturesVariables$Loads.12 = trainSet$Loads.12
    fit.lm.12 = lm(Loads.12 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.12 = NULL
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.12$xlevels
    if(length(names(temp))>0) {
      for(t in 1:length(names(temp))) {
        
        fit.lm.12$xlevels[[names(temp)[t]]] = 
          union(fit.lm.12$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
        
      }
    }
    rm(temp, t)
    
    
    cat("creating a model for Loads at 13 o' clock\n")
    FeaturesVariables$Loads.13 = trainSet$Loads.13
    fit.lm.13 = lm(Loads.13 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.13 = NULL
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.13$xlevels
    if(length(names(temp))>0) {
      for(t in 1:length(names(temp))) {
        
        fit.lm.13$xlevels[[names(temp)[t]]] = 
          union(fit.lm.13$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
        
      }
    }
    rm(temp, t)
    
    
    cat("creating a model for Loads at 14 o' clock\n")
    FeaturesVariables$Loads.14 = trainSet$Loads.14
    fit.lm.14 = lm(Loads.14 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.14 = NULL
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.14$xlevels
    if(length(names(temp))>0) {
      for(t in 1:length(names(temp))) {
        
        fit.lm.14$xlevels[[names(temp)[t]]] = 
          union(fit.lm.14$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
        
      }
    }
    rm(temp, t)
    
    
    cat("creating a model for Loads at 15 o' clock\n")
    FeaturesVariables$Loads.15 = trainSet$Loads.15
    fit.lm.15 = lm(Loads.15 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.15 = NULL
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.15$xlevels
    if(length(names(temp))>0) {
      for(t in 1:length(names(temp))) {
        
        fit.lm.15$xlevels[[names(temp)[t]]] = 
          union(fit.lm.15$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
        
      }
    }
    rm(temp, t)
    
    
    cat("creating a model for Loads at 16 o' clock\n")
    FeaturesVariables$Loads.16 = trainSet$Loads.16
    fit.lm.16 = lm(Loads.16 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.16 = NULL
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.16$xlevels
    if(length(names(temp))>0) {
      for(t in 1:length(names(temp))) {
        
        fit.lm.16$xlevels[[names(temp)[t]]] = 
          union(fit.lm.16$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
        
      }
    }
    rm(temp, t)
    
    
    cat("creating a model for Loads at 17 o' clock\n")
    FeaturesVariables$Loads.17 = trainSet$Loads.17
    fit.lm.17 = lm(Loads.17 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.17 = NULL
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.17$xlevels
    if(length(names(temp))>0) {
      for(t in 1:length(names(temp))) {
        
        fit.lm.17$xlevels[[names(temp)[t]]] = 
          union(fit.lm.17$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
        
      }
    }
    rm(temp, t)
    
    
    cat("creating a model for Loads at 18 o' clock\n")
    FeaturesVariables$Loads.18 = trainSet$Loads.18
    fit.lm.18 = lm(Loads.18 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.18 = NULL
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.18$xlevels
    if(length(names(temp))>0) {
      for(t in 1:length(names(temp))) {
        
        fit.lm.18$xlevels[[names(temp)[t]]] = 
          union(fit.lm.18$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
        
      }
    }
    rm(temp, t)
    
    
    cat("creating a model for Loads at 19 o' clock\n")
    FeaturesVariables$Loads.19 = trainSet$Loads.19
    fit.lm.19 = lm(Loads.19 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.19 = NULL
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.19$xlevels
    if(length(names(temp))>0) {
      for(t in 1:length(names(temp))) {
        
        fit.lm.19$xlevels[[names(temp)[t]]] = 
          union(fit.lm.19$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
        
      }
    }
    rm(temp, t)
    
    
    cat("creating a model for Loads at 20 o' clock\n")
    FeaturesVariables$Loads.20 = trainSet$Loads.20
    fit.lm.20 = lm(Loads.20 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.20 = NULL
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.20$xlevels
    
    for(t in 1:length(names(temp))) {
      if(length(names(temp))>0) {  
        fit.lm.20$xlevels[[names(temp)[t]]] = 
          union(fit.lm.20$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
        
      }
    }
    rm(temp, t)
    
    
    cat("creating a model for Loads at 21 o' clock\n")
    FeaturesVariables$Loads.21 = trainSet$Loads.21
    fit.lm.21 = lm(Loads.21 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.21 = NULL
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.21$xlevels
    
    for(t in 1:length(names(temp))) {
      if(length(names(temp))>0) {
        fit.lm.21$xlevels[[names(temp)[t]]] = 
          union(fit.lm.21$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
        
      }
    }
    rm(temp, t)
    
    
    cat("creating a model for Loads at 22 o' clock\n")
    FeaturesVariables$Loads.22 = trainSet$Loads.22
    fit.lm.22 = lm(Loads.22 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.22 = NULL
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.22$xlevels
    
    for(t in 1:length(names(temp))) {
      if(length(names(temp))>0) {
        fit.lm.22$xlevels[[names(temp)[t]]] = 
          union(fit.lm.22$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
      }
    }
    rm(temp, t)
    
    
    cat("creating a model for Loads at 23 o' clock\n")
    FeaturesVariables$Loads.23 = trainSet$Loads.23
    fit.lm.23 = lm(Loads.23 ~ ., data = FeaturesVariables)
    FeaturesVariables$Loads.23 = NULL
    
    #add level to predictor.df in case of missing levels during prediction 
    #due to the fact that predictor.df is a subset of final.data.set and 
    #may has fewer levels in categorical data than the trainSet (FeaturesVariables)
    
    temp = fit.lm.23$xlevels
    
    for(t in 1:length(names(temp))) {
      if(length(names(temp))>0) {
      fit.lm.23$xlevels[[names(temp)[t]]] = 
        union(fit.lm.23$xlevels[[names(temp)[t]]][], levels(predictor.df[, names(temp)[t]]))
      }
    }
    rm(temp, t)
    
    
    #collecting the fits in a list####
    cat("collecting the fits.lm in a list\n")
    fit.lm = list()
    for(i in 1:24) {
      fit.lm[[paste("fit.lm",i-1,sep=".")]] = get(paste("fit.lm",i-1, sep="."))
    }
    
    
    #printing the summaries####
    # for(i in 1:24) {
    #   print(summary(fit.lm[[i]]))
    # }
    
    
    #making predictions####
    cat("making predictions\n")
    for(i in 1:24) {
      assign(paste("prediction.lm", i-1, sep="."), predict(fit.lm[[paste("fit.lm",i-1,sep=".")]], predictor.df))
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



cat("total elapsed time in minutes: ", (proc.time()[3]-startTime)/60)