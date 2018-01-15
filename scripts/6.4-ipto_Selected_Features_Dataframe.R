########################################################################
##Create a dataframe with all the selected Features per target variable#
########################################################################


library("Boruta")



selectedFeatures = c()

for (i in 1:24) {
  
  temp = attStats(final.boruta.list2[[i]])[c("maxImp", "decision")]
  temp = temp[temp$decision == "Confirmed",]
  temp = temp[order(-temp$maxImp),]
  temp$target = paste("Load", i-1, sep = ".")
  temp = t(temp)
  
  
  selectedFeatures = cbind(selectedFeatures, temp)
}


write.csv(t(selectedFeatures), "selectedFeatures.csv")

rm(temp, i)
