library(Boruta)


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