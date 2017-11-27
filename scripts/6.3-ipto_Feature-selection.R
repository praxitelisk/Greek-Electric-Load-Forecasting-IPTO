####################################################
####Performing Feature Selection####################
####################################################


library(Boruta)

startTime <- proc.time()[3]

#preparing feature selection####
final.boruta.list2 = list()


for(i in 1:24) {
  
  cat("Feature selection for model: ", paste("Loads.", i-1, "~.", sep = ""), "\n")
  
  
  #add the target variable to trainSet
  trainSet[paste("Loads", i-1, sep=".")] = final.Data.Set[1:dim(trainSet)[1], paste("Loads", i-1, sep=".")]
  
  
  set.seed(123)
  
  
  #perform feature selection per target variable
  assign(paste("boruta.train", i-1, sep="."), Boruta(as.formula(paste("Loads.", i-1, "~.", sep="")) , data = trainSet, doTrace = 2, maxRuns = 100, num.threads = 2))
  
  
  #remove the target variable
  trainSet[paste("Loads", i-1, sep=".")] = NULL
  
  
  #get the most important features per Loads.i
  final.boruta.list2[[paste("boruta.train",i-1,sep=".")]] =
    TentativeRoughFix(get(paste("boruta.train", i-1, sep=".")))
  
  
  cat("elapsed time in minutes: ", (proc.time()[3]-startTime)/60,"\n\n")
  
  
}


#remove some variables####
rm(list=ls(pattern="boruta.train."))
rm(i)
rm(startTime)
