#----- 6 lab -------------------
setwd("D:/data")
#читаем файл
trainData<- read.csv("train.csv", header = T, sep = "|")
testData<- read.csv("test.csv", header = T, sep = "|")
realData<- read.csv("realclass.csv", header = T, sep = "|")

#--Метод KNN
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
print(paste("Верно определено:"), as.character(res_knn), "%")

#---6я практика KNN
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
print(paste("Верно определено: "), as.character(res_knn), "%")

library(pROC)
knn.roc <- roc(res3, res4)
plot(knn.roc, print.thres="best")
    
    
    
#----------------------------------------------------------------------------
#метод SVM Poly
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
print(paste("Верно определено:", as.character(res_svmPoly), "%"))


#---6я практика svmPoly
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
print(paste("Верно определено: "), as.character(svm_poly.roc), "%")

library(pROC)
svm_poly.roc <- roc(res3, res4)
lines(svm_poly.roc, col="blue")




#---------------------------------------------------------------------------------------
#метод SVM Linear
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
print(paste("Верно определено:", as.character(res_svmLine), "%"))

#---6я практика svmLine
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
print(paste("Верно определено: "), as.character(svm_linear.roc), "%")

library(pROC)
svm_linear.roc <- roc(res3, res4)
lines(svm_linear.roc, col="green")








#-----------------------------------------------------------------------------------
#adaBoost алгоритм
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
print(paste("Верно определено:", as.character(res_adaboost), "%"))

#---6я практика adaboost
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
print(paste("Верно определено: "), as.character(adaboost.roc), "%")

library(pROC)
adaboost.roc <- roc(res3, res4)
lines(adaboost.roc, col="purple")




#---------------------------------------------------------------------------
#ДОДЕЛАТЬ НЕЙРОННУЮ СЕТЬ!


#---6я практика nnet
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
print(paste("Верно определено: "), as.character(nnet.roc), "%")

library(pROC)
nnet.roc <- roc(res3, res4)
lines(nnet.roc, col="blue")





legend("topright", pch = list(10,10,10,10), col=c("black", "red", "green", "blue", "purple"), 
       legend=list("KNN", "SVM_Poly", "SVM_Linear", "nnet", "AdaBoost"), cex = 0.5,
       lty = c(1,1,1,1))


#-----------
#-- 7� ����
#-----------

# �������� ������
#trustLevel - �������������� ������� ������� �������. 6: ���������� ����������
#totalScanTimeInSeconds - ����� ����� � �������� ����� ������ � ��������� ��������������� ���������
#grandTotal - ����� ��������� ��������������� ���������
#lineItemVoids - ���������� �������������� ������������
#scansWithoutRegistration - ���������� ������� ������������ ��� ������-���� ������������ (��������� ������������)
#quantityModification - ����� ��������� ���������� ������� ��� ������ �� ����������� ���������
#scannedLineItemsPerSecond - ������� ���������� ��������������� ��������� � �������
#valuePerSecond - ������� ����� ��������� ��������������� ��������� � �������
#lineItemVoidsPerPosition - ��������� ����� �������������� ������������ � ������ ����� �������������� � �� �������������� ������������
#fraud - ������������� ��� ������������� (1) ��� �� ������������� (0)

# ��������� ���������� ����������

#ne_otm ���������� ������������ ������� = totalScanTimeInSeconds * scannedLineItemsPerSecond
#otm_i_ne_otm ��������� ���������� �������������� ������������ � �� �������������� = lineItemVoids + ne_otm
#sec_na_1_udach_scan ��������� ������ ������� � ���������� ������� ������������ = totalScanTimeInSeconds / otm_i_ne_otm
#udach_i_neudach_scan ���������� ������� � ��������� ������������ = otm_i_ne_otm + scansWithoutRegistration
#dolya_neudach_scan ��������� ���������� ������� ������������ � ��������� = scansWithoutRegistration / udach_i_neudach_scan
#sec_na_1_scan ��������� ������ ������� � �������� � ������ ���������� ������������ = totalScanTimeInSeconds / udach_i_neudach_scan



#-- 1. ���� ����� ����������
#ne_otm ���������� ������������ �������
trainData[,"ne_otm"]<-NA  #�������� ������ ������� �������
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

#!!!!!!!!!����� ����� ������� ����������� ������� ������.


train2 <- trainData[,-10]         #������� ������� � ������ ����������
nImportant <- nearZeroVar(train2)
names(train2)[nImportant]
trainData[, -nImportant]

highCor = findCorrelation(cor(train2), cutoff = 0.5)
names(train2)[highCor]
trainData[, highCor]



#����������
library(caretEnsemble)
control <- trainControl(method = "adaptive_boot", number=10, repeats =3, savePredictions = TRUE, classProbs = TRUE)
algorithmList <- c('nnet', 'svmLinear2', 'svmPoly')
trainData$fraud <- as.factor(trainData$fraud)
stack_models <- caretList(make.names(fraud) ~., data = trainData, methodList = algorithmList, trControl=control)
stackControl <- trainControl(sampling="rose", method="repeatedcv", number=10, repeats=3, savePredictions = TRUE, classProbs = TRUE)
stack.glm <- caretStack(stack_models, method="glm", trControl=stackControl)
print(stack.glm)

















