library(Boruta)


#creating the train and test set####
split = 2 * 365
#trainPart = floor(split * dim(final.Data.Set)[1])
trainSet = final.Data.Set[1:(dim(final.Data.Set)[1]-split), ]
testSet = final.Data.Set[(dim(final.Data.Set)[1] - split + 1):dim(final.Data.Set)[1], ]


#get the full list of features - weather variables before feature selection#### 
full.list.of.features = names(trainSet)
full.list.of.features = full.list.of.features[-grep("pressure|precipType|^Loads|time.0|weekday.0|icon", full.list.of.features)]


full.list.of.FeaturesVariables = 
  subset(trainSet, select = grep(paste(full.list.of.features, collapse = "|"), names(trainSet)))


#create the predictor.df data.frame for predictions from test Set####
predictor.df = data.frame()
predictor.df = full.list.of.FeaturesVariables[0, ]
predictor.df = rbind(predictor.df, testSet[names(testSet) %in% names(predictor.df)])



#preparing feature selection####
final.boruta.list = list()


for(i in 1:24) {
  
  cat("Feature selection for model: ", paste("Loads.", i-1, "~.", sep=""), "\n")
  
  full.list.of.FeaturesVariables[paste("Loads", i-1, sep=".")] = trainSet[paste("Loads", i-1, sep=".")]
  
  set.seed(100 + floor(runif(1, 1, 100)))
  
  assign(paste("boruta.train", i-1, sep="."), Boruta(as.formula(paste("Loads.", i-1, "~.", sep="")) , data = full.list.of.FeaturesVariables, doTrace = 2, maxRuns = 100))
  
  full.list.of.FeaturesVariables[paste("Loads", i-1, sep=".")] = NULL
  
  
  #get the most important features per Loads.i
  final.boruta.list[[paste("boruta.train",i-1,sep=".")]] =
    TentativeRoughFix(get(paste("boruta.train",i-1,sep=".")))
  
}


#remove some variables####
rm(list=ls(pattern="boruta.train."))
rm(i)