
#-- 9 практика

setwd("D:/")
myData<- read.csv("6_1_1_2.csv", header = T, sep = ";")
x=1:731
y <- myData$Td[1:731]
z <- myData$Tpr[1:731]
k <- myData$Tmes[1:731]

View(myData)
#выделяем набор данных, для которого будем строить регрессионную модель
e<- data.frame(y=myData$Td[1:500], x=x[1:500], z=myData$Tpr[1:500], k=myData$Tmes[1:500])

#----------------------------
#-- I часть -----------------
#----------------------------

#-- МЕТОД Lasso
library(caret)
mod<- train(z~x+y+k, data = e, method = "lasso")
test<-data.frame(y=y[2:730],x=x[2:730], z=z[2:730], k=k[2:730])
res<- predict(mod, test)
#рисуем график
plot(x[1:731], myData$Tpr[1:731], type = "l", xlab = "Номер измерения", ylab = "Значение параметра", ylim = c(50, 140))
lines(x[1:500], res[1:500], type = "l", col="red")
lines(x[501:731], res[501:731], type = "l", col="green")

#-- МЕТОД PLS
library(caret)
mod2<- train(z~x+y+k, data = e, method = "pls")
test2<-data.frame(y=y[2:730],x=x[2:730], z=z[2:730], k=k[2:730])
res2<- predict(mod2, test2)
#рисуем график
plot(x[1:731], myData$Tpr[1:731], type = "l", xlab = "Номер измерения", ylab = "Значение параметра", ylim = c(50, 140))
lines(x[1:500], res2[1:500], type = "l", col="red")
lines(x[501:731], res2[501:731], type = "l", col="green")


#-- Когнитивные карты

#сначала считаем корреляцию для подставления в матрицу
data1 <- myData$Td[2:500]
m11 <- cor(data1, myData$Td[2:500])
m12 <- cor(data1, myData$K0[2:500])
m13 <- cor(data1, myData$Tmes[2:500])
m14 <- cor(data1, myData$Tpr[2:500])
m15 <- cor(data1, myData$KN1[2:500])
m16 <- cor(data1, myData$EK1[2:500])
m17 <- cor(data1, myData$KN2[2:500])
m18 <- cor(data1, myData$EK2[2:500])
m19 <- cor(data1, myData$Tspr[2:500])

data1 <- myData$K0[2:500]
m21 <- cor(data1, myData$Td[2:500])
m22 <- cor(data1, myData$K0[2:500])
m23 <- cor(data1, myData$Tmes[2:500])
m24 <- cor(data1, myData$Tpr[2:500])
m25 <- cor(data1, myData$KN1[2:500])
m26 <- cor(data1, myData$EK1[2:500])
m27 <- cor(data1, myData$KN2[2:500])
m28 <- cor(data1, myData$EK2[2:500])
m29 <- cor(data1, myData$Tspr[2:500])

data1 <- myData$Tmes[2:500]
m31 <- cor(data1, myData$Td[2:500])
m32 <- cor(data1, myData$K0[2:500])
m33 <- cor(data1, myData$Tmes[2:500])
m34 <- cor(data1, myData$Tpr[2:500])
m35 <- cor(data1, myData$KN1[2:500])
m36 <- cor(data1, myData$EK1[2:500])
m37 <- cor(data1, myData$KN2[2:500])
m38 <- cor(data1, myData$EK2[2:500])
m39 <- cor(data1, myData$Tspr[2:500])

data1 <- myData$Tpr[2:500]
m41 <- cor(data1, myData$Td[2:500])
m42 <- cor(data1, myData$K0[2:500])
m43 <- cor(data1, myData$Tmes[2:500])
m44 <- cor(data1, myData$Tpr[2:500])
m45 <- cor(data1, myData$KN1[2:500])
m46 <- cor(data1, myData$EK1[2:500])
m47 <- cor(data1, myData$KN2[2:500])
m48 <- cor(data1, myData$EK2[2:500])
m49 <- cor(data1, myData$Tspr[2:500])

data1 <- myData$KN1[2:500]
m51 <- cor(data1, myData$Td[2:500])
m52 <- cor(data1, myData$K0[2:500])
m53 <- cor(data1, myData$Tmes[2:500])
m54 <- cor(data1, myData$Tpr[2:500])
m55 <- cor(data1, myData$KN1[2:500])
m56 <- cor(data1, myData$EK1[2:500])
m57 <- cor(data1, myData$KN2[2:500])
m58 <- cor(data1, myData$EK2[2:500])
m59 <- cor(data1, myData$Tspr[2:500])

data1 <- myData$EK1[2:500]
m61 <- cor(data1, myData$Td[2:500])
m62 <- cor(data1, myData$K0[2:500])
m63 <- cor(data1, myData$Tmes[2:500])
m64 <- cor(data1, myData$Tpr[2:500])
m65 <- cor(data1, myData$KN1[2:500])
m66 <- cor(data1, myData$EK1[2:500])
m67 <- cor(data1, myData$KN2[2:500])
m68 <- cor(data1, myData$EK2[2:500])
m69 <- cor(data1, myData$Tspr[2:500])

data1 <- myData$KN2[2:500]
m71 <- cor(data1, myData$Td[2:500])
m72 <- cor(data1, myData$K0[2:500])
m73 <- cor(data1, myData$Tmes[2:500])
m74 <- cor(data1, myData$Tpr[2:500])
m75 <- cor(data1, myData$KN1[2:500])
m76 <- cor(data1, myData$EK1[2:500])
m77 <- cor(data1, myData$KN2[2:500])
m78 <- cor(data1, myData$EK2[2:500])
m79 <- cor(data1, myData$Tspr[2:500])

data1 <- myData$EK2[2:500]
m81 <- cor(data1, myData$Td[2:500])
m82 <- cor(data1, myData$K0[2:500])
m83 <- cor(data1, myData$Tmes[2:500])
m84 <- cor(data1, myData$Tpr[2:500])
m85 <- cor(data1, myData$KN1[2:500])
m86 <- cor(data1, myData$EK1[2:500])
m87 <- cor(data1, myData$KN2[2:500])
m88 <- cor(data1, myData$EK2[2:500])
m89 <- cor(data1, myData$Tspr[2:500])

data1 <- myData$Tspr[2:500]
m91 <- cor(data1, myData$Td[2:500])
m92 <- cor(data1, myData$K0[2:500])
m93 <- cor(data1, myData$Tmes[2:500])
m94 <- cor(data1, myData$Tpr[2:500])
m95 <- cor(data1, myData$KN1[2:500])
m96 <- cor(data1, myData$EK1[2:500])
m97 <- cor(data1, myData$KN2[2:500])
m98 <- cor(data1, myData$EK2[2:500])
m99 <- cor(data1, myData$Tspr[2:500])



library(FCMapper)

matrix = matrix(nrow = 9, ncol = 9)
matrix[1,] = c(m11, m12, m13, m14, m15, m16, m17, m18, m19)
matrix[2,] = c(m21, m22, m23, m24, m25, m26, m27, m28, m29)
matrix[3,] = c(m31, m32, m33, m34, m35, m36, m37, m38, m39)
matrix[4,] = c(m41, m42, m43, m44, m45, m46, m47, m48, m49)
matrix[5,] = c(m51, m52, m53, m54, m55, m56, m57, m58, m59)
matrix[6,] = c(m61, m62, m63, m64, m65, m66, m67, m68, m69)
matrix[7,] = c(m71, m72, m73, m74, m75, m76, m77, m78, m79)
matrix[8,] = c(m81, m82, m83, m84, m85, m86, m87, m88, m89)
matrix[9,] = c(m91, m92, m93, m94, m95, m96, m97, m98, m99)

concept.names = c("Td", "K0", "Tmes", "Tpr", "KN1", "EK1", "KN2", "EK2", "Tspr")

results = nochanges.scenario(matrix, iter=10, concept.names)

graph.fcm(matrix, concept.sizes = results$Equilibrium_value, concept.names)

View(results)

#----------------------------
#-- II часть ----------------
#----------------------------

############################################################

#Sys.setlocale("LC_ALL", "Ru_Ru")
Sys.setlocale("LC_ALL", "en_US")

#Путь к файлам с данными
setwd("D:/")

myTarget<- read.csv("6_1_1_1.csv", header =T, sep = ";")
myData<- read.csv("6_1_1_2.csv", header =T, sep = ";")

plot(myTarget$Theat, myTarget$X.Tamb, type = "p", xlab = "Ттеплоносителя, °C",
     ylab = "Tокр.среды, °C") # зависимсоть температуры теплоносителя от температуры окр среды


plot(myData$K0,myData$Tspr,type = "p", xlab = "Объем тепловой энергии, Гкал", ylab = "Tокр.среды, °C") #количество вырабатываемой теплоты взависисимости от температуры на улице
plot(myData$K0,myData$Tmes,type = "p", xlab = "Объем тепловой энергии, Гкал", ylab = "Tокр.среды, °C") #количество вырабатываемой теплоты взависисимости от температуры на улице


modelData<-data.frame(Tamb=myData$Tmes,Theat=myData$Tspr,K0=myData$K0)
modelData <- modelData[!duplicated(modelData$Theat),]
modelData <- modelData[order(modelData$Theat),]

modelData = modelData[modelData$Tamb < 9,]
modelData = modelData[modelData$Tamb != -13,]
#modelData = modelData[modelData$Tamb != -12.8,]
#modelData = modelData[modelData$Tamb != -11.5,]
modelData = modelData[modelData$Tamb != -1.1,]
modelData = modelData[modelData$Tamb != -1.7,]
modelData = modelData[modelData$Tamb != -5.6,]
modelData = modelData[modelData$Tamb != -9.3,]
modelData = modelData[modelData$Tamb != -3.9,]

e<-data.frame(y=modelData$Theat-80,x=modelData$K0-141)

plot(e$x,e$y,type = "p", xlab = "Объем тепловой энергии, Гкал", ylab = "Tтеплоносителя, °C"#,
     
     # ylim = c(0,160),
     # xlim = c(0, 300),
) #количество вырабатываемой теплоты взависисимости от температуры на улице


#func<-nls(y ~ k0+k1*x+k2*x^2+k3*x^3, data=e, start=list(k0=0.1, k1=0.1, k2=0.1, k3=0.1))
#func<-nls(y ~ 160/(1+k1*exp(-k2*x)), data=e, start=list(k1=0.1, k2=0.1))
x<-0:110

res1<-nls(y~(60)/(1+b*exp(-c*(x))), data=e, start=list(b=2, c=0.001))
res2<-nls(y~(60)*exp(-b*exp(-c*(x))), data=e, start=list(b=2, c=0.001))


coef_func1 <- coef(res1)
yfunc1<-(60)/(1+coef_func1[1]*exp(-coef_func1[2]*(x)))

coef_func2 <- coef(res2)
yfunc2<-(60)*exp(-coef_func2[1]*exp(-coef_func2[2]*(x)))

yfunc3<-140*x/x

plot(modelData$K0,modelData$Theat,type = "p", xlab = "Объем тепловой энергии, Гкал", ylab = "Tтеплоносителя, °C",
     
     ylim = c(75,145),
     xlim = c(140, 250),
) #количество вырабатываемой теплоты взависисимости от температуры на улице

lines(x+141,yfunc3,col="black", lty=2)
lines(x+141,yfunc1+80,col="red")
lines(x+141,yfunc2+80,col="green")

legend("topleft", legend=c("Кривая Перла", "Кривая Гомперца","Статистические данные", "Макс. значение T°C пара"),
       
       # col=c("red", "green","white", "black"),
       cex=0.8,
       fill=c("red", "green","white", "black"),bg="white")

#проверка на адекватность

#Критерий Хи-квадрат
t1<-modelData$Theat
t2<-yfunc1
t3<-yfunc2
#test0 <- matrix(c(t1, t2), nrow = 2, byrow = TRUE)
#chisq.test(test0)

#Критерий Фишера
var.test(t1, t2)
var.test(t1, t3)

#Отклонения по целевым значениям температуры

plot(as.Date(myData$Data, format = "%d.%m.%Y"),myData$Tpr, typ="s",xlab = "Время", ylab = "Tтеплоносителя, °C")
target_Temp<-NA
for (i in 1:length(myData$Data)) {
  target_Temp[i]<-80
  if (myData$Td[i] <= 5) {target_Temp[i]<-82}
  if (myData$Td[i] <= 1) {target_Temp[i]<-83}
  if (myData$Td[i] <= 0) {target_Temp[i]<-85}
  if (myData$Td[i] <= -1) {target_Temp[i]<- 87}
  if (myData$Td[i] <= -3) {target_Temp[i]<-90}
  if (myData$Td[i] <= -5) {target_Temp[i]<-92}
  if (myData$Td[i] <= -7) {target_Temp[i]<-95}
  if (myData$Td[i] <= -10) {target_Temp[i]<-98}
  if (myData$Td[i] <= -12) {target_Temp[i]<-103}
  if (myData$Td[i] <= -14) {target_Temp[i]<-108}
  if (myData$Td[i] <= -16) {target_Temp[i]<-115}
  if (myData$Td[i] <= -18) {target_Temp[i]<-121}
  if (myData$Td[i] <= -20) {target_Temp[i]<-126}
  if (myData$Td[i] <= -22) {target_Temp[i]<-130}
  if (myData$Td[i] <= -25) {target_Temp[i]<-130}
  if (myData$Td[i] <= -28) {target_Temp[i]<-132}
  if (myData$Td[i] <= -30) {target_Temp[i]<-135}
  if (myData$Td[i] <= -32) {target_Temp[i]<-135}
  if (myData$Td[i] <= -35) {target_Temp[i]<-135}
  if (myData$Td[i] <= -38) {target_Temp[i]<-140}
  if (myData$Td[i] <= -40) {target_Temp[i]<-140}
}

lines(as.Date(myData$Data, format = "%d.%m.%Y"),target_Temp,typ="s",col="green")

legend("topleft", legend=c("Ретроспективные данные", "Целевые значения"),
       fill=c("black","green"),bg="white")

#Отклонения по количеству вырабатываемой тепловой энергии

plot(as.Date(myData$Data, format = "%d.%m.%Y"),myData$K0, typ="s",xlab = "Время", ylab = "Количество теплоты, Гкал")

model_Q<-NA
for (i in 1:length(myData$Data)) {
  y<-target_Temp[i]-80
  model_Q[i]<-log(((60/y)-1)/131.77623341)/(-0.06490475)+141
}

lines(as.Date(myData$Data, format = "%d.%m.%Y"),model_Q,typ="s",col="green")

legend("topleft", legend=c("Ретроспективные данные", "Модельные значения"),
       fill=c("black","green"),bg="white")

#Считаем количество записей для каждого месяца
month_count<-NA
c<-1
old_value<-myData$K0[1]
j<-1
for (i in 2:length(myData$Data)) {
  new_value<-myData$K0[i]
  if (old_value==new_value){
    j<-j+1
  } else {
    month_count[c]<-j
    c<-c+1
    j<-1
  }
  old_value<-new_value
  if (i==length(myData$Data)){
    month_count[c]<-j
  }
}

#Считаем и рисуем средние значения
j<-0
model_Q_medium<-NA
K0_medium<-NA
for (n in 1:length(month_count)) {
  for (m in 1:month_count[n]){
    j<-j+1
    if (m==1){
      model_Q_medium[n]<-model_Q[j]
      K0_medium[n]<-myData$K0[j]
    } else {
      model_Q_medium[n]<-model_Q_medium[n]+model_Q[j]
      K0_medium[n]<-K0_medium[n]+myData$K0[j]
    }
  }
  model_Q_medium[n]<-model_Q_medium[n]/month_count[n]
  K0_medium[n]<-K0_medium[n]/month_count[n]
}

plot(K0_medium,type = "s", xlab = "Месяц наблюдения", ylab = "Количество теплоты, Гкал")
lines(model_Q_medium,col="red",type = "s")

#Экономия в первый отопительный сезон (1200 руб. цена Гкал)
Sp1<-0
for (n in 1:3) {
  Sp1<-Sp1+K0_medium[n]-model_Q_medium[n]
}
print(Sp1)
print(Sp1*1200)

#Экономия во второй отопительный сезон
Sp2<-0
for (n in 11:15) {
  Sp2<-Sp2+K0_medium[n]-model_Q_medium[n]
}
print(Sp2)
print(Sp2*1200)

#Экономия в третий отопительный сезон
Sp3<-0
for (n in 23:25) {
  Sp3<-Sp3+K0_medium[n]-model_Q_medium[n]
}
print(Sp3)
print(Sp3*1200)

plot(as.Date(myData$Data, format = "%d.%m.%Y"),myData$K0, typ="s",xlab = "Время", ylim = c(-50,400), col="blue", ylab = "Количество теплоты, Гкал")
lines(as.Date(myData$Data, format = "%d.%m.%Y"),myData$KN1,typ="s",col="red")
lines(as.Date(myData$Data, format = "%d.%m.%Y"),myData$KN2,typ="s",col="green")
legend("topleft", legend=c("Подпитка (К2) ", "Тело на ТП (К1=К3)","Общее тепло (К0=К4)"),
       fill=c("green","red","blue"),bg="white")
#Зеленая линия - Подпитка (К2)
#Красная линия - Тепло технологического процесса (К1=К3)
#Синяя линия - Общее тепло (К0=К4) 
















