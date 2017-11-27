####################################################
####create the train, evaluation and test Set#######
####################################################


#creating the train and test set splits####
splitEvalSet = 365
splitTestSet = splitEvalSet + 365
len = dim(final.Data.Set)[1]

#trainPart = floor(split * dim(final.Data.Set)[1])
trainSet = final.Data.Set[1:(len - splitTestSet), ]
evaluationSet = final.Data.Set[(len - splitTestSet + 1):(len - splitEvalSet), ]
train.and.evalSet = final.Data.Set[1:(len - splitEvalSet), ]
testSet = final.Data.Set[(len - splitEvalSet + 1):len, ]


####remove some variables that do not offer any value for experiments####
full.list.of.features = names(final.Data.Set)

#full.list.of.features = full.list.of.features[-grep("^Loads|time|weekday|icon|windBearing.[0-9]+$|apparentTemperature|day.of.week|sine.day.of.week|cosine.day.of.week|day.of.year|cosine.day.of.year|sine.day.of.year|cloudCover|uvIndex", full.list.of.features)]
#full.list.of.features = full.list.of.features[-grep("^Loads|time|weekday|icon|windBearing.[0-9]+$|day.of.week|sine.day.of.week|cosine.day.of.week|day.of.year|cosine.day.of.year|sine.day.of.year|temperature", full.list.of.features)]
full.list.of.features = full.list.of.features[-grep("^Loads|time|weekday|icon|^day.of.week$|^day.of.year$|yesterday.weather.measures.day.of.week|yesterday.weather.measures.day.of.year|temperature|windBearing.[0-9]+$", full.list.of.features)]


full.list.of.FeaturesVariables = 
  subset(final.Data.Set, select = grep(paste(full.list.of.features, collapse = "|"), names(final.Data.Set)))


trainSet =
  subset(trainSet, select = grep(paste(full.list.of.features, collapse = "|"), names(trainSet)))


evaluationSet =
  subset(evaluationSet, select = grep(paste(full.list.of.features, collapse = "|"), names(evaluationSet)))


train.and.evalSet =
  subset(train.and.evalSet, select = grep(paste(full.list.of.features, collapse = "|"), names(train.and.evalSet)))


testSet =
  subset(testSet, select = grep(paste(full.list.of.features, collapse = "|"), names(testSet)))


#create the predictor.df data.frame for predictions####
# predictor.df = data.frame()
# predictor.df = full.list.of.FeaturesVariables[0, ]
# predictor.df = rbind(predictor.df, testSet[names(testSet) %in% names(predictor.df)])

