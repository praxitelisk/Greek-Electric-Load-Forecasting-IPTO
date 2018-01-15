################################################################################
#####Ensemble Models with Feature selection and model selection#################
################################################################################


temp = 0
str_pred_ensembling = c()

for(i in 1:24) {
  
  
  str.model.select = c("nn", "svm", "xgboost", "rule", "randomForest", "knn")
  
  
  model.select = c(best.nn.parameters.fs[[i]][paste("mape.nn", i-1, sep = ".")],
                   best.svm.parameters.fs[[i]][paste("mape.svm", i-1, sep = ".")],
                   best.xgboost.parameters.fs[[i]][paste("mape.xgboost", i-1, sep = ".")],
                   best.rule.parameters.fs[[i]][paste("mape.rule", i-1, sep = ".")],
                   best.randomForest.parameters.fs[[i]][paste("mape.randomForest", i-1, sep = ".")],
                   best.knn.parameters.fs[[i]][paste("mape.knn", i-1, sep = ".")])
  
  
  str.pred.select = c("prediction.nn.fs.ms", "prediction.svm.fs.ms", "prediction.xgboost.fs.ms", "prediction.rule.fs.ms", "prediction.randomForest.fs.ms", "prediction.knn.fs.ms")
  
  cat("best mape for ", i-1 ,"th hour: ", min(model.select), ", calculated from model: ", str.model.select[which.min(model.select)], " prediction: ", str.pred.select[which.min(model.select)], ".", i-1, "\n", sep = "")
  
  
  
  str_pred_ensembling = append(str_pred_ensembling, str.pred.select[which.min(model.select)])
  
  
}

prediction.ensembling.fs.ms = c(
  rbind(
    get(str_pred_ensembling[1])[[1]],
    get(str_pred_ensembling[2])[[2]],
    get(str_pred_ensembling[3])[[3]],
    get(str_pred_ensembling[4])[[4]],
    get(str_pred_ensembling[5])[[5]][1:365, 1], #here it is a neural network, thus the special handling
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


#cat("\n\n mean mape with feature selection:", temp/24)
cat("\n abbreviations:\n ms: model selection \n fs: feature selection\n")



###############################################################
##########old code############################################
###############################################################
#str_model_ensembling = c()

#for(i in 1:24) {



#str.model.select = c("svm.fs.ms", "rule.fs.ms", "xgboost.fs.ms", "knn.fs.ms", "randomForest.fs.ms", "nn.fs.ms")
#str.pred.select = c("prediction.svm.fs.ms", "prediction.rule.fs.ms", "prediction.xgboost.fs.ms", "prediction.knn.fs.ms", "prediction.randomForest.fs.ms", "prediction.nn.fs.ms")

#model.select = c(mape.svm.fs.ms[[i]], mape.rule.fs.ms[[i]], mape.xgboost.fs.ms[[i]], mape.knn.fs.ms[[i]], mape.randomForest.fs.ms[[i]], mape.nn.fs.ms[[i]])
#pred.select = c(prediction.svm.fs.ms[[i]], prediction.rule.fs.ms[[i]], prediction.xgboost.fs.ms[[i]], prediction.knn.fs.ms[[i]], prediction.randomForest.fs.ms[[i]], prediction.nn.fs.ms[[i]])


#str.model.select = c("rule.fs.ms")
#str.pred.select = c("prediction.rule.fs.ms")

#model.select = c(mape.rule.fs.ms[[i]])
#pred.select = c(prediction.rule.fs.ms[[i]])


#cat("best mape for ", i-1 ,"th hour: ", min(model.select), ", calculated from model: ", str.model.select[which.min(model.select)] ,", fit.", str.model.select[which.min(model.select)], ".", i-1, " prediction: ", str.pred.select[which.min(model.select)], ".", i-1, "\n", sep = "")
#cat("best mape for ", i-1 ,"th hour: ", min(model.select), ", calculated from model: ", str.model.select[which.min(model.select)] ,", fit.", str.model.select[which.min(model.select)], ".", i-1, "\n", sep = "")

#str_model_ensembling = append(str_model_ensembling, str.model.select[which.min(model.select)])
#str_pred_ensembling = append(str_pred_ensembling, str.pred.select[which.min(model.select)])

#temp = temp + model.select[which.min(model.select)]

#}

# pred.ensembling.fs.ms = c(
#   rbind(
#     get(str_pred_ensembling[1])[[1]],
#     get(str_pred_ensembling[2])[[2]],
#     get(str_pred_ensembling[3])[[3]],
#     get(str_pred_ensembling[4])[[4]],
#     get(str_pred_ensembling[5])[[5]],
#     get(str_pred_ensembling[6])[[6]],
#     get(str_pred_ensembling[7])[[7]],
#     get(str_pred_ensembling[8])[[8]],
#     get(str_pred_ensembling[9])[[9]],
#     get(str_pred_ensembling[10])[[10]],
#     get(str_pred_ensembling[11])[[11]],
#     get(str_pred_ensembling[12])[[12]],
#     get(str_pred_ensembling[13])[[13]],
#     get(str_pred_ensembling[14])[[14]],
#     get(str_pred_ensembling[15])[[15]],
#     get(str_pred_ensembling[16])[[16]],
#     get(str_pred_ensembling[17])[[17]],
#     get(str_pred_ensembling[18])[[18]],
#     get(str_pred_ensembling[19])[[19]],
#     get(str_pred_ensembling[20])[[20]],
#     get(str_pred_ensembling[21])[[21]],
#     get(str_pred_ensembling[22])[[22]],
#     get(str_pred_ensembling[23])[[23]],
#     get(str_pred_ensembling[24])[[24]]
#   )
# )
# 
# 
# cat("\n\n mean mape with feature selection:", temp/24)
# cat("\n abbreviations: ms: model selection \n fs: feature selection\n")
# 
# 
# rm(temp, str.model.select, str.pred.select, model.select, pred.select, str_model_ensembling, str_pred_ensembling)

