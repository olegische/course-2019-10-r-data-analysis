#-- 8lab -----------
setwd("D:/data")
trainData<- read.csv("train.csv", header = T, sep = "|")
testData<- read.csv("test.csv", header = T, sep = "|")
realData<- read.csv("realclass.csv", header = T, sep = "|")

View(trainData)






#### считаем вероятности для дискретных величин ####

#trustLevel
trainData[,"trustLevel_nf"]<-NA
trainData[,"trustLevel_f"]<-NA

for (i in 0:max(trainData$trustLevel)) {
  trainData$trustLevel_nf[trainData$trustLevel == i]<-length(trainData$trustLevel[trainData$trustLevel == i & trainData$fraud==0])*100/length(trainData$trustLevel[trainData$fraud==0])/100
  trainData$trustLevel_f[trainData$trustLevel == i]<-length(trainData$trustLevel[trainData$trustLevel == i & trainData$fraud==1])*100/length(trainData$trustLevel[trainData$fraud==1])/100
}

#totalScanTimeInSeconds
trainData[,"totalScanTimeInSeconds_nf"]<-NA
trainData[,"totalScanTimeInSeconds_f"]<-NA

for (i in 0:max(trainData$totalScanTimeInSeconds)) {
  trainData$totalScanTimeInSeconds_nf[trainData$totalScanTimeInSeconds == i]<-length(trainData$totalScanTimeInSeconds[trainData$totalScanTimeInSeconds == i & trainData$fraud==0])*100/length(trainData$totalScanTimeInSeconds[trainData$fraud==0])/100
  trainData$totalScanTimeInSeconds_f[trainData$totalScanTimeInSeconds == i]<-length(trainData$totalScanTimeInSeconds[trainData$totalScanTimeInSeconds == i & trainData$fraud==1])*100/length(trainData$totalScanTimeInSeconds[trainData$fraud==1])/100
}

#lineItemVoids
trainData[,"lineItemVoids_nf"]<-NA
trainData[,"lineItemVoids_f"]<-NA

for (i in 0:max(trainData$lineItemVoids)) {
  trainData$lineItemVoids_nf[trainData$lineItemVoids == i]<-length(trainData$lineItemVoids[trainData$lineItemVoids == i & trainData$fraud==0])*100/length(trainData$lineItemVoids[trainData$fraud==0])/100
  trainData$lineItemVoids_f[trainData$lineItemVoids == i]<-length(trainData$lineItemVoids[trainData$lineItemVoids == i & trainData$fraud==1])*100/length(trainData$lineItemVoids[trainData$fraud==1])/100
}

#scansWithoutRegistration
trainData[,"scansWithoutRegistration_nf"]<-NA
trainData[,"scansWithoutRegistration_f"]<-NA

for (i in 0:max(trainData$scansWithoutRegistration)) {
  trainData$scansWithoutRegistration_nf[trainData$scansWithoutRegistration == i]<-length(trainData$scansWithoutRegistration[trainData$scansWithoutRegistration == i & trainData$fraud==0])*100/length(trainData$scansWithoutRegistration[trainData$fraud==0])/100
  trainData$scansWithoutRegistration_f[trainData$scansWithoutRegistration == i]<-length(trainData$scansWithoutRegistration[trainData$scansWithoutRegistration == i & trainData$fraud==1])*100/length(trainData$scansWithoutRegistration[trainData$fraud==1])/100
}

#quantityModification
trainData[,"quantityModifications_nf"]<-NA
trainData[,"quantityModifications_f"]<-NA

for (i in 0:max(trainData$quantityModifications)) {
  trainData$quantityModifications_nf[trainData$quantityModifications == i]<-length(trainData$quantityModifications[trainData$quantityModifications == i & trainData$fraud==0])*100/length(trainData$quantityModifications[trainData$fraud==0])/100
  trainData$quantityModifications_f[trainData$quantityModifications == i]<-length(trainData$quantityModifications[trainData$quantityModifications == i & trainData$fraud==1])*100/length(trainData$quantityModifications[trainData$fraud==1])/100
}

#ne_otm
trainData[,"ne_otm_nf"]<-NA
trainData[,"ne_otm_f"]<-NA

for (i in min(trainData$ne_otm):max(trainData$ne_otm)) {
  print(i)
  trainData$ne_otm_nf[trainData$ne_otm == i]<-length(trainData$ne_otm[trainData$ne_otm == i & trainData$fraud==0])*100/length(trainData$ne_otm[trainData$fraud==0])/100
  trainData$ne_otm_f[trainData$ne_otm == i]<-length(trainData$ne_otm[trainData$ne_otm == i & trainData$fraud==1])*100/length(trainData$ne_otm[trainData$fraud==1])/100
}

#### считаем вероятности для непрерывных величин ####

#grandTotal

trainData[,"grandTotals_nf"]<-0
trainData[,"grandTotal_f"]<-0



st<-0.1

trainData$grandTotal_nf<-0
trainData$grandTotal_f<-0

for (j in 1:length(trainData$grandTotal)) {
  
  for (i in seq(min(trainData$grandTotal), trainData$grandTotal[j], by = st)) {
    
    #min(trainData$grandTotal):trainData$grandTotal[j]) {
    trainData$grandTotal_nf[j]<-trainData$grandTotal_nf[j]+length(trainData$grandTotal[i<trainData$grandTotal& trainData$grandTotal<i+st & trainData$fraud==0])*100/length(trainData$grandTotal[trainData$fraud==0])/100
    trainData$grandTotal_f[j]<-trainData$grandTotal_f[j]+length(trainData$grandTotal[i<trainData$grandTotal& trainData$grandTotal<i+st & trainData$fraud==1])*100/length(trainData$grandTotal[trainData$fraud==1])/100
  }
  
}

#scannedLineItemsPerSecond

trainData[,"scannedLineItemsPerSecond_nf"]<-0
trainData[,"scannedLineItemsPerSecond_f"]<-0



st<-0.01

trainData$scannedLineItemsPerSecond_nf<-0
trainData$scannedLineItemsPerSecond_f<-0

for (j in 1:length(trainData$scannedLineItemsPerSecond)) {
  for (i in seq(min(trainData$scannedLineItemsPerSecond), trainData$scannedLineItemsPerSecond[j], by = st)) {
    trainData$scannedLineItemsPerSecond_nf[j]<-trainData$scannedLineItemsPerSecond_nf[j]+(length(trainData$scannedLineItemsPerSecond[i<trainData$scannedLineItemsPerSecond & trainData$scannedLineItemsPerSecond<i+st & trainData$fraud==0])*100/length(trainData$scannedLineItemsPerSecond[trainData$fraud==0]))/100
    trainData$scannedLineItemsPerSecond_f[j]<-trainData$scannedLineItemsPerSecond_f[j]+(length(trainData$scannedLineItemsPerSecond[i<trainData$scannedLineItemsPerSecond & trainData$scannedLineItemsPerSecond<i+st & trainData$fraud==1])*100/length(trainData$scannedLineItemsPerSecond[trainData$fraud==1]))/100
  }
}


#valuePerSecond

trainData[,"valuePerSecond_nf"]<-0
trainData[,"valuePerSecond_f"]<-0



st<-0.01

trainData$valuePerSecond_nf<-0
trainData$valuePerSecond_f<-0

for (j in 1:length(trainData$valuePerSecond)) {
  for (i in seq(min(trainData$valuePerSecond), trainData$valuePerSecond[j], by = st)) {
    trainData$valuePerSecond_nf[j]<-trainData$valuePerSecond_nf[j]+(length(trainData$valuePerSecond[i<trainData$valuePerSecond & trainData$valuePerSecond<i+st & trainData$fraud==0])*100/length(trainData$valuePerSecond[trainData$fraud==0]))/100
    trainData$valuePerSecond_f[j]<-trainData$valuePerSecond_f[j]+(length(trainData$valuePerSecond[i<trainData$valuePerSecond & trainData$valuePerSecond<i+st & trainData$fraud==1])*100/length(trainData$valuePerSecond[trainData$fraud==1]))/100
  }
}

#lineItemVoidsPerPosition

trainData[,"lineItemVoidsPerPosition_nf"]<-0
trainData[,"lineItemVoidsPerPosition_f"]<-0



st<-0.01

trainData$lineItemVoidsPerPosition_nf<-0
trainData$lineItemVoidsPerPosition_f<-0

for (j in 1:length(trainData$lineItemVoidsPerPosition)) {
  for (i in seq(min(trainData$lineItemVoidsPerPosition), trainData$lineItemVoidsPerPosition[j], by = st)) {
    trainData$lineItemVoidsPerPosition_nf[j]<-trainData$lineItemVoidsPerPosition_nf[j]+(length(trainData$lineItemVoidsPerPosition[i<trainData$lineItemVoidsPerPosition & trainData$lineItemVoidsPerPosition<i+st & trainData$fraud==0])*100/length(trainData$lineItemVoidsPerPosition[trainData$fraud==0]))/100
    trainData$lineItemVoidsPerPosition_f[j]<-trainData$lineItemVoidsPerPosition_f[j]+(length(trainData$lineItemVoidsPerPosition[i<trainData$lineItemVoidsPerPosition & trainData$lineItemVoidsPerPosition<i+st & trainData$fraud==1])*100/length(trainData$lineItemVoidsPerPosition[trainData$fraud==1]))/100
  }
}

#P
trainData[,"P_nf"]<-NA
trainData[,"P_f"]<-NA





trainData$P_nf<-(trainData$trustLevel_nf+trainData$totalScanTimeInSeconds_nf-trainData$trustLevel_nf*trainData$totalScanTimeInSeconds_nf)
trainData$P_nf<-(trainData$P_nf+trainData$lineItemVoids_nf-trainData$P_nf*trainData$lineItemVoids_nf)
trainData$P_nf<-(trainData$P_nf+trainData$scansWithoutRegistration_nf-trainData$P_nf*trainData$scansWithoutRegistration_nf)
trainData$P_nf<-(trainData$P_nf+trainData$quantityModifications_nf-trainData$P_nf*trainData$quantityModifications_nf)
trainData$P_nf<-(trainData$P_nf+trainData$grandTotal_nf-trainData$P_nf*trainData$grandTotal_nf)
trainData$P_nf<-(trainData$P_nf+trainData$scannedLineItemsPerSecond_nf-trainData$P_nf*trainData$scannedLineItemsPerSecond_nf)
trainData$P_nf<-(trainData$P_nf+trainData$valuePerSecond_nf-trainData$P_nf*trainData$valuePerSecond_nf)
trainData$P_nf<-(trainData$P_nf+trainData$lineItemVoidsPerPosition_nf-trainData$P_nf*trainData$lineItemVoidsPerPosition_nf)

trainData$P_f<-(trainData$trustLevel_f+trainData$totalScanTimeInSeconds_f-trainData$trustLevel_f*trainData$totalScanTimeInSeconds_f)
trainData$P_f<-(trainData$P_f+trainData$lineItemVoids_f-trainData$P_f*trainData$lineItemVoids_f)
trainData$P_f<-(trainData$P_f+trainData$scansWithoutRegistration_f-trainData$P_f*trainData$scansWithoutRegistration_f)
trainData$P_f<-(trainData$P_f+trainData$quantityModifications_f-trainData$P_f*trainData$quantityModifications_f)
trainData$P_f<-(trainData$P_f+trainData$scannedLineItemsPerSecond_f-trainData$P_f*trainData$scannedLineItemsPerSecond_f)
trainData$P_f<-(trainData$P_f+trainData$valuePerSecond_f-trainData$P_f*trainData$valuePerSecond_f)
trainData$P_f<-(trainData$P_f+trainData$lineItemVoidsPerPosition_f-trainData$P_f*trainData$lineItemVoidsPerPosition_f)


plot(trainData$P_f[trainData$fraud == 0],trainData$P_nf[trainData$fraud == 0],type = "p",col="blue", xlab = "P_f", ylab = "P_nf")
lines(trainData$P_f[trainData$fraud == 1],trainData$P_nf[trainData$fraud == 1],type = "p",col="red")


#-- EM-алгоритм
library(EMCluster, quiet=TRUE)
x<-data.frame(trainData$P_f, trainData$P_nf)
ret.em <- init.EM(x, nclass = 5, method="em.EM")
ret.Rnd <- init.EM(x, nclass = 5, method="Rnd.EM", EMC=.EMC.Rnd)
emobj <- simple.init(x, nclass = 5)
ret.init <- emcluster(x, emobj, assign.class = T)
#Вывод результатов при различных значениях начальных вероятностей для классов
plotem(ret.em, x, xlab='P_f', ylab='P_nf')
plotem(ret.Rnd, x, xlab='P_f', ylab='P_nf')
plotem(ret.init, x, xlab='P_f', ylab='P_nf')




#-- Сеть Кохонена
library(kohonen)
library(RColorBrewer)
#library(RCurl)

colnames(trainData)
dataMeasures <- c("trustLevel", "totalScanTimeInSeconds", "grandTotal")
dataMeasures2 <- c("trustLevel", "totalScanTimeInSeconds", "grandTotal", "lineItemVoids", "scansWithoutRegistration", "quantityModifications", "lineItemVoidsPerPosition")

names(trainData)
  
  #тесты
  #dataDraw <- som(scale(trainData[dataMeasures]), grid = somgrid(6, 4, "rectangular"))
  #plot(dataDraw)
  #plot(dataDraw, type="dist.neighbours", palette.name= terrain.colors)
  
  #dataDraw2 <- som(scale(trainData[dataMeasures2]), grid = somgrid(6, 4, "rectangular"))
  #plot(dataDraw2)
  #plot(dataDraw2, type="dist.neighbours", palette.name= terrain.colors)
  #----------
  
dataDraw2 <- xyf(scale(trainData[,dataMeasures2]), classvec2classmat(trainData[,"fraud"]),
                 grid=somgrid(20, 20, "hexagonal"))

par(mfrow = c(1, 2))
plot(dataDraw2, type="codes", main=c("Codes X", "Codes Y"))
dataDraw2.hc <- cutree(hclust(dist(data.frame(dataDraw2$codes)$X1)), 5)
add.cluster.boundaries(dataDraw2, dataDraw2.hc)



#-- Apriori алгоритм
library(arules)
dataMeasuresDiscret <- c("trustLevel", "totalScanTimeInSeconds", "lineItemVoids", "scansWithoutRegistration", "quantityModifications", "fraud")
factor(trainData)
trainData3 <- data.frame(trustLevel=factor(trainData[, "trustLevel"]),
                         totalScanTimeInSeconds=factor(trainData[, "totalScanTimeInSeconds"]),
                         lineItemVoids=factor(trainData[, "lineItemVoids"]),
                         scansWithoutRegistration=factor(trainData[, "scansWithoutRegistration"]),
                         quantityModifications=factor(trainData[, "quantityModifications"]),
                         fraud=factor(trainData[, "fraud"])
                         )
rules <- apriori(trainData3, parameter = list(supp=0.5, conf=0.9, target="rules"))
summary(rules)
inspect(rules)













