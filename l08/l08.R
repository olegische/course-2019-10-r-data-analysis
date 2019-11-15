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
trainData2[,"quantityModifications_nf"]<-NA
trainData2[,"quantityModifications_f"]<-NA

for (i in 0:max(trainData2$quantityModifications)) {
  trainData2$quantityModifications_nf[trainData2$quantityModifications == i]<-length(trainData2$quantityModifications[trainData2$quantityModifications == i & trainData2$fraud==0])*100/length(trainData2$quantityModifications[trainData$fraud==0])/100
  trainData2$quantityModifications_f[trainData2$quantityModifications == i]<-length(trainData2$quantityModifications[trainData2$quantityModifications == i & trainData2$fraud==1])*100/length(trainData$quantityModifications[trainData$fraud==1])/100
}

#ne_otm
trainData[,"ne_otm_nf"]<-NA
trainData[,"ne_otm_f"]<-NA

for (i in min(trainData2$ne_otm):max(trainData2$ne_otm)) {
  print(i)
  trainData2$ne_otm_nf[trainData2$ne_otm == i]<-length(trainData2$ne_otm[trainData2$ne_otm == i & trainData2$fraud==0])*100/length(trainData2$ne_otm[trainData2$fraud==0])/100
  trainData2$ne_otm_f[trainData2$ne_otm == i]<-length(trainData2$ne_otm[trainData2$ne_otm == i & trainData2$fraud==1])*100/length(trainData2$ne_otm[trainData2$fraud==1])/100
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