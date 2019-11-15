# Модель классификации клиентов магазина без кассиров - распознать мошенников.
# Читаем файлы
trainData <- read.csv("train.csv", header=T, sep="|")
testData <- read.csv("test.csv", header=T, sep="|")
realData <- read.csv("real.csv", header=T, sep="|")

summary(trainData)

library(caret)

# Часть 1. Бездумно запихиваем все переменные в модель.
## Метод KNN
knn <- train(as.character(fraud) ~ trustLevel+totalScanTimeInSeconds+grandTotal+lineItemVoids+
               scansWithoutRegistration+quantityModifications+scannedLineItemsPerSecond+
               valuePerSecond+lineItemVoidsPerPosition, trainData, method='knn')
res <- predict(knn, testData, type="raw")
res2 <- data.frame(fraud=res)

positive <- 0
negative <- 0

for (i in 1:length(res)){
  if (res2$fraud[i] == realData$fraud[i]) {
    positive <- positive + 1
  } else {
    negative <- negative + 1
  }
}

res_KNN <-  (positive*100)/length(realData$fraud) # 95.2366995167841
print(paste('Верно определено: ', as.character(res3), '%'))


## Метод SVM Poly
svmPoly <- train(as.character(fraud) ~ trustLevel+totalScanTimeInSeconds+grandTotal+lineItemVoids+
               scansWithoutRegistration+quantityModifications+scannedLineItemsPerSecond+
               valuePerSecond+lineItemVoidsPerPosition, trainData, method='svmPoly')
res <- predict(svmPoly, testData, type="raw")
res2 <- data.frame(fraud=res)

positive <- 0
negative <- 0

for (i in 1:length(res)){
  if (res2$fraud[i] == realData$fraud[i]) {
    positive <- positive + 1
  } else {
    negative <- negative + 1
  }
}

res_svmPoly <-  (positive*100)/length(realData$fraud) # 96.620901347263
print(paste('Верно определено: ', as.character(res_svmPoly), '%'))


## Метод SVM Linear
svmLine <- train(as.character(fraud) ~ trustLevel+totalScanTimeInSeconds+grandTotal+lineItemVoids+
                   scansWithoutRegistration+quantityModifications+scannedLineItemsPerSecond+
                   valuePerSecond+lineItemVoidsPerPosition, trainData, method='svmLinear3')
res <- predict(svmLine, testData, type="raw")
res2 <- data.frame(fraud=res)

positive <- 0
negative <- 0

for (i in 1:length(res)){
  if (res2$fraud[i] == realData$fraud[i]) {
    positive <- positive + 1
  } else {
    negative <- negative + 1
  }
}

res_svmLine <-  (positive*100)/length(realData$fraud) # 95.2366995167841
print(paste('Верно определено: ', as.character(res_svmLine), '%'))


## Нейронная сеть
nnet <- train(as.character(fraud) ~ trustLevel+totalScanTimeInSeconds+grandTotal+lineItemVoids+
                   scansWithoutRegistration+quantityModifications+scannedLineItemsPerSecond+
                   valuePerSecond+lineItemVoidsPerPosition, trainData, method='nnet')
res <- predict(nnet, testData, type="raw")
res2 <- data.frame(fraud=res)

positive <- 0
negative <- 0

for (i in 1:length(res)){
  if (res2$fraud[i] == realData$fraud[i]) {
    positive <- positive + 1
  } else {
    negative <- negative + 1
  }
}

res_nnet <-  (positive*100)/length(realData$fraud) # 98.0008873346034
print(paste('Верно определено: ', as.character(res_nnet), '%'))



## AdaBoost алгоритм
adaboost <- train(as.character(fraud) ~ trustLevel+totalScanTimeInSeconds+grandTotal+lineItemVoids+
                scansWithoutRegistration+quantityModifications+scannedLineItemsPerSecond+
                valuePerSecond+lineItemVoidsPerPosition, trainData, method='adaboost')
res <- predict(adaboost, testData, type="raw")
res2 <- data.frame(fraud=res)

positive <- 0
negative <- 0

for (i in 1:length(res)){
  if (res2$fraud[i] == realData$fraud[i]) {
    positive <- positive + 1
  } else {
    negative <- negative + 1
  }
}

res_adaboost <-  (positive*100)/length(realData$fraud) # 97.9292180012487
print(paste('Верно определено: ', as.character(res_adaboost), '%'))

