# Модель классификации клиентов магазина без кассиров - распознать мошенников.
# Читаем файлы
setwd("C:/Users/user/Documents/rom/l07")

trainData <- read.csv("train.csv", 
                      header=T, 
                      sep="|")

testData <- read.csv("test.csv",
                     header=T,
                     sep="|")

realData <- read.csv("real.csv",
                      header=T,
                      sep="|")

summary(trainData)

library(caret)

# Часть 1. Бездумно запихиваем все переменные в модель.
## Метод KNN
# l04
knn1 <- train(as.character(fraud) ~ trustLevel+
               totalScanTimeInSeconds+
               grandTotal+
               lineItemVoids+
               scansWithoutRegistration+
               quantityModifications+
               scannedLineItemsPerSecond+
               valuePerSecond+
               lineItemVoidsPerPosition, 
               trainData, 
               method='knn')

knn1_res1 <- predict(knn1, testData, type="raw")

knn1_res2 <- data.frame(fraud=knn1_res1)

positive <- 0
negative <- 0

for (i in 1:length(knn1_res1)){
  if (knn1_res2$fraud[i] == realData$fraud[i]) {
    positive <- positive + 1
  } else {
    negative <- negative + 1
  }
}

knn1_total <-  (positive*100)/length(realData$fraud) # 95.2366995167841
print(paste('Верно определено: ', as.character(knn1_total), '%'))

#effect_knn1 <- 0
#for(i in 1:length(knn1_res2)){
#  if(knn1_res1$fraud[i] == realData$fraud[i]){
#    if(realData$fraud[i] =='1'){
#      effect_knn1 <- effect_knn1 +5
#    }
#    positive <- positive + 1
#  } else {
#    if (realData$fraud[i] == '0'){
#      effect_knn1 <-  effect_knn1 - 25
#    } else {
#      effect_knn1 <-  effect_knn1 - 25
#    }
#    negative <-  negative + 1
#  }
#}


# l05
library(caret)

knn2 <- train(as.character(fraud) ~ .,
               trainData, 
               method = 'knn')

knn2_res1 <- predict(knn2, testData)

knn2_res2 <- data.frame(fraud=knn2_res1)

real <- factor(c(realData[,1]))

#knn2_precision <- precision(data=factor(knn2_res1), reference = real)
#knn2_recall <- recall(data=factor(knn2_res1), reference = real)
#knn2_F_means <- F_meas(data=factor(knn2_res1), reference = real)

knn2_precision <- precision(data=knn2_res1, reference = real)

knn2_recall <- recall(data=knn2_res1, reference = real)

knn2_F_means <- F_meas(data=knn2_res1, reference = real)


library("MLmetrics")

knn2_res3 <- as.numeric(c(as.integer(knn2_res1)-1))
knn2_res4 <- as.numeric(c(realData$fraud))

knn2_logLoss <- LogLoss(knn2_res3,knn2_res4)
knn2_accuracy <- Accuracy(knn2_res3, knn2_res4)

positive <- 0
negative <- 0
knn2_effect <- 0

for(i in 1:length(knn2_res1)){
  if(knn2_res2$fraud[i] == realData$fraud[i]){
    if(realData$fraud[i] =='1'){
      knn2_effect <- knn2_effect +5
    }
    positive <- positive + 1
  } else {
    if (realData$fraud[i] == '0'){
      knn2_effect <-  knn2_effect - 25
    } else {
      knn2_effect <-  knn2_effect - 25
    }
    negative <-  negative + 1
  }
}

knn2_total <- (positive*100)/length(realData$fraud)

print(paste("Верно определено: ", as.character(knn2_total), "%"))

library(pROC)

knn2_roc <- roc(knn2_res3,knn2_res4)

plot(knn2_roc, print.thres="best")


## Метод SVM Poly
svmPoly1 <- train(as.character(fraud) ~ trustLevel+
               totalScanTimeInSeconds+
               grandTotal+
               lineItemVoids+
               scansWithoutRegistration+
               quantityModifications+
               scannedLineItemsPerSecond+
               valuePerSecond+
               lineItemVoidsPerPosition, 
               trainData, 
               method='svmPoly')

svmPoly1_res1 <- predict(svmPoly1, testData, type="raw")

svmPoly1_res2 <- data.frame(fraud=svmPoly1_res1)

positive <- 0
negative <- 0

for (i in 1:length(svmPoly1_res1)){
  if (svmPoly1_res2$fraud[i] == realData$fraud[i]) {
    positive <- positive + 1
  } else {
    negative <- negative + 1
  }
}

svmPoly1_total <-  (positive*100)/length(realData$fraud) # 96.620901347263
print(paste('Верно определено: ', as.character(svmPoly1_total), '%'))

# l05
svmPoly2 <- train(as.character(fraud) ~ ., trainData, method = 'svmPoly')

svmPoly2_res1 <- predict(svmPoly2, testData, type = 'raw')

svmPoly2_res2 <- data.frame(fraud=svmPoly2_res1)

real <- factor(c(realData[,1]))

svmPoly2_precision <- precision(data=svmPoly2_res1, reference = real)
svmPoly2_recall <- recall(data=svmPoly2_res1, reference = real)
svmPoly2_F_means <- F_meas(data=svmPoly2_res1, reference = real)

library("MLmetrics")

svmPoly2_res3 <- as.numeric(c(as.integer(svmPoly2_res1)-1))

svmPoly2_res4 <- as.numeric(c(realData$fraud))

svmPoly2_logLoss <- LogLoss(svmPoly2_res3, svmPoly2_res4)
svmPoly2_accuracy <- Accuracy(svmPoly2_res3, svmPoly2_res4)

positive <- 0
negative <- 0
svmPoly2_effect <- 0

for(i in 1:length(svmPoly2_res1)){
  if(svmPoly2_res2$fraud[i] == realData$fraud[i]){
    if(realData$fraud[i] =='1'){
      svmPoly2_effect <- svmPoly2_effect +5
    }
    positive <- positive + 1
  } else {
    if (realData$fraud[i] == '0'){
      svmPoly2_effect <-  svmPoly2_effect - 25
    } else {
      svmPoly2_effect <-  svmPoly2_effect - 25
    }
    negative <-  negative + 1
  }
}

svmPoly2_total <- (positive*100)/length(realData$fraud)
print(paste("Верно определено: ",as.character(svmPoly2_total),"%"))

library(pROC)

svmPoly2_roc <- roc(svmPoly2_res3, svmPoly2_res4)
plot(svmPoly2_roc, print.thres="best")
#lines(svmPoly2, col = 'red')

## Метод SVM Linear
svmLine1 <- train(as.character(fraud) ~ trustLevel+
                    totalScanTimeInSeconds+
                    grandTotal+
                    lineItemVoids+
                    scansWithoutRegistration+
                    quantityModifications+
                    scannedLineItemsPerSecond+
                    valuePerSecond+
                    lineItemVoidsPerPosition, 
                    trainData, 
                    method='svmLinear3')
# можно использовать svmLinear2

svmLine1_res1 <- predict(svmLine1, testData, type="raw")

svmLine1_res2 <- data.frame(fraud=svmLine1_res1)

positive <- 0
negative <- 0

for (i in 1:length(svmLine1_res1)){
  if (svmLine1_res2$fraud[i] == realData$fraud[i]) {
    positive <- positive + 1
  } else {
    negative <- negative + 1
  }
}

svmLine1_total <-  (positive*100)/length(realData$fraud) # 95.2366995167841
print(paste('Верно определено: ', as.character(svmLine1_total), '%'))

# l05
svmLine2 <- train(as.character(fraud) ~ ., trainData, method = 'svmLinear2')

svmLine2_res1 <- predict(svmLine2, testData)

svmLine2_res2 <- data.frame(fraud=svmLine2_res1)

real <- factor(c(realData[,1]))

svmLine2_precision <- precision(data=svmLine2_res1, reference = real)
svmLine2_recall <- recall(data=svmLine2_res1, reference = real)
svmLine2_F_means <- F_meas(data=svmLine2_res1, reference = real)

library("MLmetrics")

svmLine2_res3 <- as.numeric(c(as.integer(svmLine2_res1)-1))

svmLine2_res4 <- as.numeric(c(realData$fraud))

svmLine2_logLoss <- LogLoss(svmLine2_res3,svmLine2_res4)
svmLine2_accuracy <- Accuracy(svmLine2_res3, svmLine2_res4)

positive <- 0
negative <- 0
svmLine2_effect <- 0

for(i in 1:length(svmLine2_res1)){
  if(svmLine2_res2$fraud[i] == realData$fraud[i]){
    if(realData$fraud[i] =='1'){
      svmLine2_effect <- svmLine2_effect +5
    }
    positive <- positive + 1
  } else {
    if (realData$fraud[i] == '0'){
      svmLine2_effect <-  svmLine2_effect - 25
    } else {
      svmLine2_effect <-  svmLine2_effect - 25
    }
    negative <-  negative + 1
  }
}

svmLine2_total <- (positive*100)/length(realData$fraud)
print(paste("Верно определено: ",as.character(svmLine2_total),"%"))

library(pROC)

svmLine2_roc <- roc(svmLine2_res3,svmLine2_res4)
#plot(svmLine2_roc, print.thres="best")
lines(svmLine2_roc, col = 'purple')

## Нейронная сеть
nnet1 <- train(as.character(fraud) ~ trustLevel+
                 totalScanTimeInSeconds+
                 grandTotal+
                 lineItemVoids+
                 scansWithoutRegistration+
                 quantityModifications+
                 scannedLineItemsPerSecond+
                 valuePerSecond+
                 lineItemVoidsPerPosition, 
                 trainData, 
                 method='nnet')

nnet1_res1 <- predict(nnet1, testData, type="raw")
nnet1_res2 <- data.frame(fraud=nnet1_res1)

positive <- 0
negative <- 0

for (i in 1:length(nnet1_res1)){
  if (nnet1_res2$fraud[i] == realData$fraud[i]) {
    positive <- positive + 1
  } else {
    negative <- negative + 1
  }
}

nnet1_total <-  (positive*100)/length(realData$fraud) # 98.0008873346034
print(paste('Верно определено: ', as.character(nnet1_total), '%'))

# l05
nnet2 <- train(as.character(fraud) ~ ., trainData, method = 'nnet')

nnet2_res1 <- predict(nnet2, testData)

nnet2_res2 <- data.frame(fraud=nnet2_res1)

real <- factor(c(realData[,1]))

nnet2_precision <- precision(data=nnet2_res1, reference = real)
nnet2_recall <- recall(data=nnet2_res1, reference = real)
nnet2_F_means <- F_meas(data=nnet2_res1, reference = real)

library("MLmetrics")

nnet2_res3 <- as.numeric(c(as.integer(nnet2_res1)-1))

nnet2_res4 <- as.numeric(c(realData$fraud))

nnet2_logLoss <- LogLoss(nnet2_res3,nnet2_res4)
nnet2_accuracy <- Accuracy(nnet2_res3, nnet2_res4)

positive <- 0
negative <- 0
nnet2_effect <- 0

for(i in 1:length(nnet2_res1)){
  if(nnet2_res2$fraud[i] == realData$fraud[i]){
    if(realData$fraud[i] =='1'){
      nnet2_effect <- nnet2_effect +5
    }
    positive <- positive + 1
  } else {
    if (realData$fraud[i] == '0'){
      nnet2_effect <-  nnet2_effect - 25
    } else {
      nnet2_effect <-  nnet2_effect - 25
    }
    negative <-  negative + 1
  }
}

nnet2_total <- (positive*100)/length(realData$fraud)
print(paste("Верно определено: ",as.character(nnet2_total),"%"))

library(pROC)

nnet2_roc <- roc(nnet2_res3,nnet2_res4)
#plot(nnet2_roc, print.thres="best")
lines(nnet2_roc, col = 'green')


## adaboost алгоритм
adaboost1 <- train(as.character(fraud) ~ trustLevel+
                     totalScanTimeInSeconds+
                     grandTotal+
                     lineItemVoids+
                     scansWithoutRegistration+
                     quantityModifications+
                     scannedLineItemsPerSecond+
                     valuePerSecond+
                     lineItemVoidsPerPosition, 
                     trainData, 
                     method='adaboost')

adaboost_res1 <- predict(adaboost1, testData, type="raw")

adaboost_res2 <- data.frame(fraud=adaboost_res1)

positive <- 0
negative <- 0

for (i in 1:length(adaboost_res1)){
  if (adaboost_res2$fraud[i] == realData$fraud[i]) {
    positive <- positive + 1
  } else {
    negative <- negative + 1
  }
}

adaboost1_total <-  (positive*100)/length(realData$fraud) # 97.9292180012487
print(paste('Верно определено: ', as.character(adaboost1_total), '%'))

# l05
adaboost2 <- train(as.character(fraud) ~ ., trainData, method = 'adaboost')

adaboost2_res1 <- predict(adaboost2, testData)

adaboost2_res2 <- data.frame(fraud=adaboost2_res1)

real <- factor(c(realData[,1]))

adaboost2_precision <- precision(data=adaboost2_res1, reference = real)
adaboost2_recall <- recall(data=adaboost2_res1, reference = real)
adaboost2_F_means <- F_meas(data=adaboost2_res1, reference = real)

library("MLmetrics")

adaboost2_res3 <- as.numeric(c(as.integer(adaboost2_res1)-1))
adaboost2_res4 <- as.numeric(c(realData$fraud))

adaboost2_logLoss <- LogLoss(adaboost2_res3,adaboost2_res4)
adaboost2_accuracy <- Accuracy(adaboost2_res3, adaboost2_res4)

positive <- 0
negative <- 0
adaboost2_effect <- 0

for(i in 1:length(adaboost2_res1)){
  if(adaboost2_res2$fraud[i] == realData$fraud[i]){
    if(realData$fraud[i] =='1'){
      adaboost2_effect <- adaboost2_effect +5
    }
    positive <- positive + 1
  } else {
    if (realData$fraud[i] == '0'){
      adaboost2_effect <-  adaboost2_effect - 25
    } else {
      adaboost2_effect <-  adaboost2_effect - 25
    }
    negative <-  negative + 1
  }
}
adaboost2_total <- (positive*100)/length(realData$fraud)
print(paste("Верно определено: ",as.character(adaboost2_total),"%"))

library(pROC)

adaboost2_roc <- roc(adaboost2_res3,adaboost2_res4)
#plot(adaboost2_roc, print.thres="best")
lines(adaboost2, col = 'orange')