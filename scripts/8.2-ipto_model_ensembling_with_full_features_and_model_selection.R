
temp = 0
str_pred_ensembling = c()
str_model_ensembling = c()

for(i in 1:24) {
  
  str.model.select = c("svm.full.ms", "nn.full.ms", "xgboost.full.ms", "randomForest.full.ms", "rule.full.ms", "knn.full.ms")
  str.pred.select = c("prediction.svm.full.ms", "prediction.nn.full.ms", "prediction.xgboost.full.ms", "prediction.randomForest.full.ms", "prediction.rule.full.ms", "knn.full.ms")
  
  model.select = c(mape.svm.full.ms[[i]], mape.nn.full.ms[[i]], mape.xgboost.full.ms[[i]], mape.randomForest.full.ms[[i]] ,mape.rule.full.ms[[i]], mape.knn.full.ms[[i]])
  pred.select = c(prediction.svm.full.ms[[i]], prediction.nn.full.ms[[i]], prediction.xgboost.full.ms[[i]], prediction.randomForest.full.ms[[i]], prediction.rule.full.ms[[i]], prediction.knn.full.ms[[i]])
  
  #cat("best mape for ", i-1 ,"th hour: ", min(model.select), ", calculated from model: ", str.model.select[which.min(model.select)] ,", fit.", str.model.select[which.min(model.select)], ".", i-1, " prediction: ", str.pred.select[which.min(model.select)], ".", i-1, "\n", sep = "")
  cat("best mape for ", i-1 ,"th hour: ", min(model.select), ", calculated from model: ", str.model.select[which.min(model.select)] ,", fit.", str.model.select[which.min(model.select)], ".", i-1, "\n", sep = "")
  
  str_model_ensembling = append(str_model_ensembling, str.model.select[which.min(model.select)])
  str_pred_ensembling = append(str_pred_ensembling, str.pred.select[which.min(model.select)])
  
  temp = temp + model.select[which.min(model.select)]
  
}

pred.ensembling.full.ms = c(
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
