############################################################################
#####Ensemble Models with Feature Selection and Default model parameters####
############################################################################

temp = 0
str_pred_ensembling = c()
str_model_ensembling = c()

for(i in 1:24) {
  
  str.model.select = c("svm.fs.def", "nn.fs.def", "xgboost.fs.def", "randomForest.fs.def", "rule.fs.def", "knn.fs.def")
  str.pred.select = c("prediction.svm.fs.def", "prediction.nn.fs.def", "prediction.xgboost.fs.def", "prediction.randomForest.fs.def", "prediction.rule.fs.def", "knn.fs.def")
  
  model.select = c(mape.svm.fs.def[[i]], mape.nn.fs.def[[i]], mape.xgboost.fs.def[[i]], mape.randomForest.fs.def[[i]] ,mape.rule.fs.def[[i]], mape.knn.fs.def[[i]])
  pred.select = c(prediction.svm.fs.def[[i]], prediction.nn.fs.def[[i]], prediction.xgboost.fs.def[[i]], prediction.randomForest.fs.def[[i]], prediction.rule.fs.def[[i]], prediction.knn.fs.def[[i]])
  
  #cat("best mape for ", i-1 ,"th hour: ", min(model.select), ", calculated from model: ", str.model.select[which.min(model.select)] ,", fit.", str.model.select[which.min(model.select)], ".", i-1, " prediction: ", str.pred.select[which.min(model.select)], ".", i-1, "\n", sep = "")
  cat("best mape for ", i-1 ,"th hour: ", min(model.select), ", calculated from model: ", str.model.select[which.min(model.select)] ,", fit.", str.model.select[which.min(model.select)], ".", i-1, "\n", sep = "")
  
  str_model_ensembling = append(str_model_ensembling, str.model.select[which.min(model.select)])
  str_pred_ensembling = append(str_pred_ensembling, str.pred.select[which.min(model.select)])
  
  temp = temp + model.select[which.min(model.select)]
  
}

pred.ensembling.fs.def = c(
  rbind(
    get(str_pred_ensembling[1])[[1]],
    get(str_pred_ensembling[2])[[2]],
    get(str_pred_ensembling[3])[[3]],
    get(str_pred_ensembling[4])[[4]],
    get(str_pred_ensembling[5])[[5]],
    get(str_pred_ensembling[6])[[6]],
    get(str_pred_ensembling[7])[[7]],
    get(str_pred_ensembling[8])[[8]],
    get(str_pred_ensembling[9])[[9]],
    get(str_pred_ensembling[10])[[10]],
    get(str_pred_ensembling[11])[[11]],
    get(str_pred_ensembling[12])[[12]],
    get(str_pred_ensembling[13])[[13]],
    get(str_pred_ensembling[14])[[14]],
    get(str_pred_ensembling[15])[[15]],
    get(str_pred_ensembling[16])[[16]],
    get(str_pred_ensembling[17])[[17]],
    get(str_pred_ensembling[18])[[18]],
    get(str_pred_ensembling[19])[[19]],
    get(str_pred_ensembling[20])[[20]],
    get(str_pred_ensembling[21])[[21]],
    get(str_pred_ensembling[22])[[22]],
    get(str_pred_ensembling[23])[[23]],
    get(str_pred_ensembling[24])[[24]]
  )
)


cat("\n\n mean mape with feature selection:", temp/24)
cat("\n abbreviations: ms: model selection \n fs: feature selection\n")


rm(temp, str.model.select, str.pred.select, model.select, pred.select)
