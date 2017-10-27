library(Boruta)

startTime <- proc.time()[3]

#preparing feature selection####
final.boruta.list2 = list()


for(i in 1:24) {
  
  cat("Feature selection for model: ", paste("Loads.", i-1, "~.", sep = ""), "\n")
  
  full.list.of.FeaturesVariables[paste("Loads", i-1, sep=".")] = final.Data.Set[paste("Loads", i-1, sep=".")]
  
  set.seed(123)
  
  assign(paste("boruta.train", i-1, sep="."), Boruta(as.formula(paste("Loads.", i-1, "~.", sep="")) , data = full.list.of.FeaturesVariables, doTrace = 2, maxRuns = 100))
  
  full.list.of.FeaturesVariables[paste("Loads", i-1, sep=".")] = NULL
  
  
  #get the most important features per Loads.i
  final.boruta.list2[[paste("boruta.train",i-1,sep=".")]] =
    TentativeRoughFix(get(paste("boruta.train", i-1, sep=".")))
  
  
  cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60,"\n\n")
  
  
}


#remove some variables####
rm(list=ls(pattern="boruta.train."))
rm(i)
rm(startTime)
