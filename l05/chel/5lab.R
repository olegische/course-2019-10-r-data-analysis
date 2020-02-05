#----- 5 lab -------------------
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


#метод SVM Linear
svmLine <- train(as.character(fraud)~trustLevel+totalScanTimeInSeconds+
                   grandTotal+lineItemVoids+scansWithoutRegistration+
                   quantityModifications+scannedLineItemsPerSecond+
                   valuePerSecond+lineItemVoidsPerPosition, trainData, method = "adaboost")
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


#adaBoost алгоритм
adaboost <- train(as.character(fraud)~trustLevel+totalScanTimeInSeconds+
                    grandTotal+lineItemVoids+scansWithoutRegistration+
                    quantityModifications+scannedLineItemsPerSecond+
                    valuePerSecond+lineItemVoidsPerPosition, trainData, method = "svmLinear3")
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


#ДОДЕЛАТЬ НЕЙРОННУЮ СЕТЬ!





