
#creating the train and test set####
split = 2 * 365
#trainPart = floor(split * dim(final.Data.Set)[1])
trainSet = final.Data.Set[1:(dim(final.Data.Set)[1]-split), ]
testSet = final.Data.Set[(dim(final.Data.Set)[1] - split + 1):dim(final.Data.Set)[1], ]


full.list.of.features = names(trainSet)
full.list.of.features = full.list.of.features[-grep("pressure|precipType|^Loads|time.0|weekday.0", full.list.of.features)]


full.list.of.FeaturesVariables = 
  subset(trainSet, select = grep(paste(full.list.of.features, collapse = "|"), names(trainSet)))


#create the predictor.df data.frame for predictions####
predictor.df = data.frame()
predictor.df = full.list.of.FeaturesVariables[0, ]
predictor.df = rbind(predictor.df, testSet[names(testSet) %in% names(predictor.df)])