# Модель классификации клиентов магазина без кассиров - распознать мошенников.
# Читаем файлы
setwd("C:/Users/user/Documents/rom/l05")
trainData <- read.csv("train.csv", header=T, sep="|")
testData <- read.csv("test.csv", header=T, sep="|")
realData <- read.csv("DMC-2019-realclass.csv", header=T, sep="|")

summary(trainData)

library(caret)

# Часть 1. Бездумно запихиваем все переменные в модель.
## Метод KNN
# l04
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

effect_knn <- 0
for(i in 1:length(res)){
  if(res$fraud[i] == realData$fraud[i]){
    if(realData$fraud[i] =='1'){
      effect_knn <- effect_knn +5
    }
    positive <- positive + 1
  } else {
    if (realData$fraud[i] == '0'){
      effect_knn <-  effect_knn - 25
    } else {
      effect_knn <-  effect_knn - 25
    }
    negative <-  negative + 1
  }
}
# l05
library(caret)

knn <- train(as.character(fraud) ~ ., trainData, method = 'knn')
res <- predict(knn, testData)
res
res2 <- data.frame(fraud=res)

real <- factor(c(realData[,1]))
#knn_precision <- precision(data=factor(res), reference = real)
#knn_recall <- recall(data=factor(res), reference = real)
#knn_F_means <- F_meas(data=factor(res), reference = real)
knn_precision <- precision(data=res, reference = real)
knn_recall <- recall(data=res, reference = real)

knn_F_means <- F_meas(data=res, reference = real)

library("MLmetrics")

res3 <- as.numeric(c(as.integer(res)-1))
res4 <- as.numeric(c(realData$fraud))

knn_logLoss <- LogLoss(res3,res4)
knn_accuracy <- Accuracy(res3, res4)

positive <- 0
negative <- 0
knn_effect <- 0

for(i in 1:length(res2)){
  if(res2$fraud[i] == realData$fraud[i]){
    if(realData$fraud[i] =='1'){
      knn_effect <- knn_effect +5
    }
    positive <- positive + 1
  } else {
    if (realData$fraud[i] == '0'){
      knn_effect <-  knn_effect - 25
    } else {
      knn_effect <-  knn_effect - 25
    }
    negative <-  negative + 1
  }
}
knn_res <- (positive*100)/length(realData$fraud)
print(paste("Верно определено: ",as.character(knn_res),"%"))

library(pROC)

knn_roc <- roc(res3,res4)
plot(knn_roc, print.thres="best")


## Метод SVM Poly
svmPoly <- train(as.character(fraud) ~ trustLevel+totalScanTimeInSeconds+grandTotal+lineItemVoids+
               scansWithoutRegistration+quantityModifications+scannedLineItemsPerSecond+
               valuePerSecond+lineItemVoidsPerPosition, trainData, method='svmPoly')
res <- predict(svmPoly, testData, type="raw")
res
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

# l05
svmPoly <- train(as.character(fraud) ~ ., trainData, method = 'svmPoly')
res <- predict(svmPoly, testData)
res2 <- data.frame(fraud=res)

real <- factor(c(realData[,1]))
svmPoly_precision <- precision(data=res, reference = real)
svmPoly_recall <- recall(data=res, reference = real)

svmPoly_F_means <- F_meas(data=res, reference = real)

library("MLmetrics")

res3 <- as.numeric(c(as.integer(res)-1))
res4 <- as.numeric(c(realData$fraud))

svmPoly_logLoss <- LogLoss(res3,res4)
svmPoly_accuracy <- Accuracy(res3, res4)

positive <- 0
negative <- 0
svmPoly_effect <- 0

for(i in 1:length(res2)){
  if(res2$fraud[i] == realData$fraud[i]){
    if(realData$fraud[i] =='1'){
      svmPoly_effect <- svmPoly_effect +5
    }
    positive <- positive + 1
  } else {
    if (realData$fraud[i] == '0'){
      svmPoly_effect <-  svmPoly_effect - 25
    } else {
      svmPoly_effect <-  svmPoly_effect - 25
    }
    negative <-  negative + 1
  }
}
svmPoly_res <- (positive*100)/length(realData$fraud)
print(paste("Верно определено: ",as.character(svmPoly_res),"%"))

library(pROC)

svmPoly_roc <- roc(res3,res4)
plot(svmPoly_roc, print.thres="best")





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

