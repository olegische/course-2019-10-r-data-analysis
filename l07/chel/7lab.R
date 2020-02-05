#----- 6 lab -------------------
setwd("D:/data")
#С‡РёС‚Р°РµРј С„Р°Р№Р»
trainData<- read.csv("train.csv", header = T, sep = "|")
testData<- read.csv("test.csv", header = T, sep = "|")
realData<- read.csv("realclass.csv", header = T, sep = "|")

#--РњРµС‚РѕРґ KNN
library(caret)

knn <- train(as.character(fraud)~trustLevel+totalScanTimeInSeconds+
               grandTotal+lineItemVoids+scansWithoutRegistration+
               quantityModifications+scannedLineItemsPerSecond+
               valuePerSecond+lineItemVoidsPerPosition, trainData, method = "knn")
res <- predict(knn, testData, type="raw")
res2 <- data.frame(fraud=res)

positive <- 0
negative <- 0

for(i in 1:length(res)){
  if (res2$fraud[i] == realData$fraud[i]){
    positive <- positive + 1
  }
  else {
    negative <- negative + 1
  }
}
res_knn <- (positive * 100)/length(realData$fraud)
print(paste("Р’РµСЂРЅРѕ РѕРїСЂРµРґРµР»РµРЅРѕ:"), as.character(res_knn), "%")

#---6СЏ РїСЂР°РєС‚РёРєР° KNN
knn <- train(as.character(fraud) ~ ., trainData, method="knn")
res <- predict(knn, testData)
res2 <- data.frame(fraud=res)

real<- factor(c(realData[,1]))
knn_precision <- precision(data=res, reference=real)
knn_recall <- recall(data=res, reference=real)

knn_F_means <- F_meas(data=res, reference=real)
library("MLmetrics")

res3 <- as.numeric(c(as.integer(res)-1))
res4 <- as.numeric(c(realData$fraud))
knnlogLoss<-LogLoss(res3, res4)
knn_accuracy <- Accuracy(res3, res4)

positive<-0
negative<-0
knn_effect<-0

for(i in 1:length(res)){
  if (res2$fraud[i] == realData$fraud[i]){
    if (realData$fraud[i] =="1") {
      knn_effect<- knn_effect+5
    }
    positive <- positive+1
  }
  else{
    if (realData$fraud[i]=="0"){
      knn_effect<-knn_effect-25
    }
    else{
      knn_effect<-knn_effect-25
    }
    negative<-negative+1
  }
}

knn.roc <- (positive*100)/length(realData$fraud)
print(paste("Р’РµСЂРЅРѕ РѕРїСЂРµРґРµР»РµРЅРѕ: "), as.character(res_knn), "%")

library(pROC)
knn.roc <- roc(res3, res4)
plot(knn.roc, print.thres="best")
    
    
    
#----------------------------------------------------------------------------
#РјРµС‚РѕРґ SVM Poly
svmPoly <- train(as.character(fraud)~trustLevel+totalScanTimeInSeconds+
                   grandTotal+lineItemVoids+scansWithoutRegistration+
                   quantityModifications+scannedLineItemsPerSecond+
                   valuePerSecond+lineItemVoidsPerPosition, trainData, method = "svmPoly")
res <- predict(svmPoly, testData, type="raw")
res2 <- data.frame(fraud=res)
positive <- 0
negative <- 0
for(i in 1:length(res)){
  if (res2$fraud[i] == realData$fraud[i]){
    positive <- positive + 1
  }
  else {
    negative <- negative + 1
  }
}
res_svmPoly <- (positive * 100)/length(realData$fraud)
print(paste("Р’РµСЂРЅРѕ РѕРїСЂРµРґРµР»РµРЅРѕ:", as.character(res_svmPoly), "%"))


#---6СЏ РїСЂР°РєС‚РёРєР° svmPoly
svmPoly <- train(as.character(fraud) ~ ., trainData, method="svmPoly")
res <- predict(svmPoly, testData)
res2 <- data.frame(fraud=res)

real<- factor(c(realData[,1]))
svm_poly_precision <- precision(data=res, reference=real)
svm_poly_recall <- recall(data=res, reference=real)

svm_poly_F_means <- F_meas(data=res, reference=real)
library("MLmetrics")

res3 <- as.numeric(c(as.integer(res)-1))
res4 <- as.numeric(c(realData$fraud))
svm_poly_logLoss<-LogLoss(res3, res4)
svm_poly_accuracy <- Accuracy(res3, res4)

positive<-0
negative<-0
svm_poly_effect<-0

for(i in 1:length(res)){
  if (res2$fraud[i] == realData$fraud[i]){
    if (realData$fraud[i] =="1") {
      svm_poly_effect<- svm_poly_effect+5
    }
    positive <- positive+1
  }
  else{
    if (realData$fraud[i]=="0"){
      svm_poly_effect<-svm_poly_effect-25
    }
    else{
      svm_poly_effect<-svm_poly_effect-25
    }
    negative<-negative+1
  }
}

svm_poly.roc <- (positive*100)/length(realData$fraud)
print(paste("Р’РµСЂРЅРѕ РѕРїСЂРµРґРµР»РµРЅРѕ: "), as.character(svm_poly.roc), "%")

library(pROC)
svm_poly.roc <- roc(res3, res4)
lines(svm_poly.roc, col="blue")




#---------------------------------------------------------------------------------------
#РјРµС‚РѕРґ SVM Linear
svmLine <- train(as.character(fraud)~trustLevel+totalScanTimeInSeconds+
                   grandTotal+lineItemVoids+scansWithoutRegistration+
                   quantityModifications+scannedLineItemsPerSecond+
                   valuePerSecond+lineItemVoidsPerPosition, trainData, method = "svmLinear3")
res <- predict(svmLine, testData, type="raw")
res2 <- data.frame(fraud=res)

positive <- 0
negative <- 0

for(i in 1:length(res)){
  if (res2$fraud[i] == realData$fraud[i]){
    positive <- positive + 1
  }
  else {
    negative <- negative + 1
  }
}

res_svmLine <- (positive * 100)/length(realData$fraud)
print(paste("Р’РµСЂРЅРѕ РѕРїСЂРµРґРµР»РµРЅРѕ:", as.character(res_svmLine), "%"))

#---6СЏ РїСЂР°РєС‚РёРєР° svmLine
svmLinear <- train(as.character(fraud) ~ ., trainData, method="svmLinear2")
res <- predict(svmLinear, testData, type="raw")
res2 <- data.frame(fraud=res)

real<- factor(c(realData[,1]))
svm_linear_precision <- precision(data=res, reference=real)
svm_linear_recall <- recall(data=res, reference=real)

svm_linear_F_means <- F_meas(data=res, reference=real)
library("MLmetrics")

res3 <- as.numeric(c(as.integer(res)-1))
res4 <- as.numeric(c(realData$fraud))
svm_linear_logLoss<-LogLoss(res3, res4)
svm_linear_accuracy <- Accuracy(res3, res4)

positive<-0
negative<-0
svm_linear_effect<-0

for(i in 1:length(res)){
  if (res2$fraud[i] == realData$fraud[i]){
    if (realData$fraud[i] =="1") {
      svm_linear_effect<- svm_linear_effect+5
    }
    positive <- positive+1
  }
  else{
    if (realData$fraud[i]=="0"){
      svm_linear_effect<-svm_linear_effect-25
    }
    else{
      svm_linear_effect<-svm_linear_effect-25
    }
    negative<-negative+1
  }
}

svm_linear.roc <- (positive*100)/length(realData$fraud)
print(paste("Р’РµСЂРЅРѕ РѕРїСЂРµРґРµР»РµРЅРѕ: "), as.character(svm_linear.roc), "%")

library(pROC)
svm_linear.roc <- roc(res3, res4)
lines(svm_linear.roc, col="green")








#-----------------------------------------------------------------------------------
#adaBoost Р°Р»РіРѕСЂРёС‚Рј
adaboost <- train(as.character(fraud)~trustLevel+totalScanTimeInSeconds+
                    grandTotal+lineItemVoids+scansWithoutRegistration+
                    quantityModifications+scannedLineItemsPerSecond+
                    valuePerSecond+lineItemVoidsPerPosition, trainData, method = "adaboost")
res <- predict(adaboost, testData, type="raw")
res2 <- data.frame(fraud=res)

positive <- 0
negative <- 0

for(i in 1:length(res)){
  if (res2$fraud[i] == realData$fraud[i]){
    positive <- positive + 1
  }
  else {
    negative <- negative + 1
  }
}

res_adaboost <- (positive * 100)/length(realData$fraud)
print(paste("Р’РµСЂРЅРѕ РѕРїСЂРµРґРµР»РµРЅРѕ:", as.character(res_adaboost), "%"))

#---6СЏ РїСЂР°РєС‚РёРєР° adaboost
adaboost <- train(as.character(fraud) ~ ., trainData, method="adaboost")
res <- predict(adaboost, testData)
res2 <- data.frame(fraud=res)

real<- factor(c(realData[,1]))
adaboost_precision <- precision(data=res, reference=real)
adaboost_recall <- recall(data=res, reference=real)

adaboost_F_means <- F_meas(data=res, reference=real)
library("MLmetrics")

res3 <- as.numeric(c(as.integer(res)-1))
res4 <- as.numeric(c(realData$fraud))
adaboost_logLoss<-LogLoss(res3, res4)
adaboost_accuracy <- Accuracy(res3, res4)

positive<-0
negative<-0
adaboost_effect<-0

for(i in 1:length(res)){
  if (res2$fraud[i] == realData$fraud[i]){
    if (realData$fraud[i] =="1") {
      adaboost_effect<- adaboost_effect+5
    }
    positive <- positive+1
  }
  else{
    if (realData$fraud[i]=="0"){
      adaboost_effect<-adaboost_effect-25
    }
    else{
      adaboost_effect<-adaboost_effect-25
    }
    negative<-negative+1
  }
}

adaboost.roc <- (positive*100)/length(realData$fraud)
print(paste("Р’РµСЂРЅРѕ РѕРїСЂРµРґРµР»РµРЅРѕ: "), as.character(adaboost.roc), "%")

library(pROC)
adaboost.roc <- roc(res3, res4)
lines(adaboost.roc, col="purple")




#---------------------------------------------------------------------------
#Р”РћР”Р•Р›РђРўР¬ РќР•Р™Р РћРќРќРЈР® РЎР•РўР¬!


#---6СЏ РїСЂР°РєС‚РёРєР° nnet
nnet <- train(as.character(fraud) ~ ., trainData, method="nnet")
res <- predict(nnet, testData)
res2 <- data.frame(fraud=res)

real<- factor(c(realData[,1]))
nnet_precision <- precision(data=res, reference=real)
nnet_recall <- recall(data=res, reference=real)

nnet_F_means <- F_meas(data=res, reference=real)
library("MLmetrics")

res3 <- as.numeric(c(as.integer(res)-1))
res4 <- as.numeric(c(realData$fraud))
nnet_logLoss<-LogLoss(res3, res4)
nnet_accuracy <- Accuracy(res3, res4)

positive<-0
negative<-0
nnet_effect<-0

for(i in 1:length(res)){
  if (res2$fraud[i] == realData$fraud[i]){
    if (realData$fraud[i] =="1") {
      nnet_effect<- nnet_effect+5
    }
    positive <- positive+1
  }
  else{
    if (realData$fraud[i]=="0"){
      nnet_effect<-nnet_effect-25
    }
    else{
      nnet_effect<-nnet_effect-25
    }
    negative<-negative+1
  }
}

nnet.roc <- (positive*100)/length(realData$fraud)
print(paste("Р’РµСЂРЅРѕ РѕРїСЂРµРґРµР»РµРЅРѕ: "), as.character(nnet.roc), "%")

library(pROC)
nnet.roc <- roc(res3, res4)
lines(nnet.roc, col="blue")





legend("topright", pch = list(10,10,10,10), col=c("black", "red", "green", "blue", "purple"), 
       legend=list("KNN", "SVM_Poly", "SVM_Linear", "nnet", "AdaBoost"), cex = 0.5,
       lty = c(1,1,1,1))


#-----------
#-- 7я лаба
#-----------

# Описание данных
#trustLevel - Индивидуальный уровень доверия клиента. 6: Высочайшая надежность
#totalScanTimeInSeconds - Общее время в секундах между первым и последним отсканированным продуктом
#grandTotal - Общая стоимость отсканированных продуктов
#lineItemVoids - Количество аннулированных сканирований
#scansWithoutRegistration - Количество попыток сканирования без какого-либо сканирования (неудачное сканирование)
#quantityModification - Число изменений количества товаров для одного из сканируемых продуктов
#scannedLineItemsPerSecond - Среднее количество отсканированных продуктов в секунду
#valuePerSecond - Средняя общая стоимость отсканированных продуктов в секунду
#lineItemVoidsPerPosition - Отношение числа аннулированных сканирований к общему числу аннулированных и не аннулированных сканирований
#fraud - Классификатор как мошенничество (1) или не мошенничество (0)

# Введенные вычислимые показатели

#ne_otm Количество неотменённых заказов = totalScanTimeInSeconds * scannedLineItemsPerSecond
#otm_i_ne_otm Отношение количества аннулированных сканирований к не аннулированным = lineItemVoids + ne_otm
#sec_na_1_udach_scan Отношение общего времени к количеству удачных сканирований = totalScanTimeInSeconds / otm_i_ne_otm
#udach_i_neudach_scan Количество удачных и неудачных сканирований = otm_i_ne_otm + scansWithoutRegistration
#dolya_neudach_scan Отношение количества удачных сканирований к неудачным = scansWithoutRegistration / udach_i_neudach_scan
#sec_na_1_scan Отношение общего времени в магазине к общему количеству сканирований = totalScanTimeInSeconds / udach_i_neudach_scan



#-- 1. ВВод новых параметров
#ne_otm Количество неотменённых заказов
trainData[,"ne_otm"]<-NA  #создание нового пустого столбца
trainData[,"otm_i_ne_otm"]<-NA
trainData[,"sec_na_1_udach_scan"]<-NA
trainData[,"udach_i_neudach_scan"]<-NA
trainData[,"dolya_neudach_scan"]<-NA
trainData[,"sec_na_1_scan"]<-NA

for(i in 1:length(trainData$fraud)){
    trainData$ne_otm[i] <- trainData$totalScanTimeInSeconds[i]*trainData$scannedLineItemsPerSecond[i]
    trainData$otm_i_ne_otm[i] <-  trainData$lineItemVoids[i] +  trainData$ne_otm[i]
    trainData$sec_na_1_udach_scan[i] <-  trainData$totalScanTimeInSeconds[i] /  trainData$otm_i_ne_otm[i]
    trainData$udach_i_neudach_scan[i] <-  trainData$otm_i_ne_otm[i] +  trainData$scansWithoutRegistration[i]
    trainData$dolya_neudach_scan[i] <-  trainData$scansWithoutRegistration[i] /  trainData$udach_i_neudach_scan[i]
    trainData$sec_na_1_scan[i] <-  trainData$totalScanTimeInSeconds[i] /  trainData$udach_i_neudach_scan[i]
}

#!!!!!!!!!после этого пробуем пересчитать верхние методы.


train2 <- trainData[,-10]         #убираем столбцы с низкой кореляцией
nImportant <- nearZeroVar(train2)
names(train2)[nImportant]
trainData[, -nImportant]

highCor = findCorrelation(cor(train2), cutoff = 0.5)
names(train2)[highCor]
trainData[, highCor]



#композиция
library(caretEnsemble)
control <- trainControl(method = "adaptive_boot", number=10, repeats =3, savePredictions = TRUE, classProbs = TRUE)
algorithmList <- c('nnet', 'svmLinear2', 'svmPoly')
trainData$fraud <- as.factor(trainData$fraud)
stack_models <- caretList(make.names(fraud) ~., data = trainData, methodList = algorithmList, trControl=control)
stackControl <- trainControl(sampling="rose", method="repeatedcv", number=10, repeats=3, savePredictions = TRUE, classProbs = TRUE)
stack.glm <- caretStack(stack_models, method="glm", trControl=stackControl)
print(stack.glm)

















