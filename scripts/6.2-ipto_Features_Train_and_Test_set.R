
#creating the train and test set splits####
splitEvalSet = 365
splitTestSet = splitEvalSet + 365
len = dim(final.Data.Set)[1]

#trainPart = floor(split * dim(final.Data.Set)[1])
trainSet = final.Data.Set[1:(len - splitTestSet), ]
evaluationSet = final.Data.Set[(len-splitTestSet + 1):(len - splitEvalSet), ]
testSet = final.Data.Set[(len - splitEvalSet + 1):len, ]


full.list.of.features = names(trainSet)
full.list.of.features = full.list.of.features[-grep("^Loads|time|weekday|icon|windBearing.[0-9]+$|apparentTemperature|day.of.week|sine.day.of.week|cosine.day.of.week|day.of.year|cosine.day.of.year|sine.day.of.year|cloudCover|uvIndex", full.list.of.features)]


full.list.of.FeaturesVariables = 
  subset(trainSet, select = grep(paste(full.list.of.features, collapse = "|"), names(trainSet)))


#create the predictor.df data.frame for predictions####
predictor.df = data.frame()
predictor.df = full.list.of.FeaturesVariables[0, ]
predictor.df = rbind(predictor.df, testSet[names(testSet) %in% names(predictor.df)])

