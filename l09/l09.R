# Lec 10
library(caret)

setwd("C:/Users/user/Documents/rom/l10")
MyData<-read.csv("6_1_1_2.csv",header=T, sep=";")

#Данные
x<-1:731
y <- MyData$Td[1:731]
summary(MyData)

#График
plot(x[1:731], MyData$Td[1:731], type="l", xlab="номер измерения", ylab="значение параметра", ylim = c(-30,145))
lines(x[1:731], MyData$Tpr[1:731], type="l", col='red')

#выбираем набор данных для которого будем строить регрессионную модель
# записываем в е столец Td как значения у, х задаем от 1 до 500
e1<-data.frame(Td=MyData$Td[1:500],x=x[1:500], Tpr=MyData$Tpr[1:500], K0=MyData$K0[1:500], Tmes=MyData$Tmes[1:500], 
               KN1=MyData$KN1[1:500], EK1=MyData$EK1[1:500], KN2=MyData$KN2[1:500], EK2=MyData$EK2[1:500], Tspr=MyData$Tspr[1:500])

## Метод lasso
library(caret) # библиотека для машинного обучения
mod_Tpr_lasso <- train(Tpr ~ ., data=e1, method='lasso')
test_Tpr_lasso <- data.frame(Td=MyData$Td[1:731],x=x[1:731], Tpr=MyData$Tpr[1:731], K0=MyData$K0[1:731], Tmes=MyData$Tmes[1:731],
                             KN1=MyData$KN1[1:731], EK1=MyData$EK1[1:731], KN2=MyData$KN2[1:731], EK2=MyData$EK2[1:731], Tspr=MyData$Tspr[1:731])
res2_Tpr_lasso <- predict(mod_Tpr_lasso, test_Tpr_lasso)

#Рисуем график kNN
plot(x[1:731], MyData$Tpr[1:731], type='l', xlab='Номер измерения', ylab='Значение параметра')
lines(x[1:500], res2_Tpr_lasso[1:500], type="l", col="red")
lines(x[501:731], res2_Tpr_lasso[501:731], type="l", col="green")

# проверка модели (насколько модель уловила основную закономерность в данных)
#Критерий Фишера
var.test(MyData$Tpr[501:731], res2_Tpr_lasso[501:731]) 
# p-value = 0.197 > 0.05 гипотеза об адекватности модели принимается/

## Метод pls
mod_Tpr_pls <- train(Tpr ~ ., data=e1, method='pls')
test_Tpr_pls <- data.frame(Td=MyData$Td[1:731],x=x[1:731], Tpr=MyData$Tpr[1:731], K0=MyData$K0[1:731], Tmes=MyData$Tmes[1:731],
                             KN1=MyData$KN1[1:731], EK1=MyData$EK1[1:731], KN2=MyData$KN2[1:731], EK2=MyData$EK2[1:731], Tspr=MyData$Tspr[1:731])
res2_Tpr_pls <- predict(mod_Tpr_pls, test_Tpr_pls)

#Рисуем график
plot(x[1:731], MyData$Tpr[1:731], type='l', xlab='Номер измерения', ylab='Значение параметра')
lines(x[1:500], res2_Tpr_pls[1:500], type="l", col="red")
lines(x[501:731], res2_Tpr_pls[501:731], type="l", col="green")

# проверка модели (насколько модель уловила основную закономерность в данных)
#Критерий Фишера
var.test(MyData$Tpr[501:731], res2_Tpr_pls[501:731]) 

# когнитивные карты
library(FCMapper)

# матрица трансцендентности
matr <- matrix(nrow = 7, ncol = 7)
matr[1,] <- c(0,-0.5,0,0,1,0,1)
matr[2,] <- c(1,0,1,0.2,0,0,0.6)
matr[3,] <- c(0,1,0,0,0,0,0)
matr[4,] <- c(0.6,0,0,1,0,0,0.1)
matr[5,] <- c(0,0.5,0,0,1,0,-0.6)
matr[6,] <- c(0,0,-1,0,0,0,0)
matr[7,] <- c(0,0,0,-0.5,0,0,1)
concept.names <- c("A","B","C","D","E","F","G")

results <- nochanges.scenario(matr,iter = 10,concept.names)

graph.fcm(matr,concept.sizes = results$Equilibrium_value,concept.names)

# когнитивные карты
# матрица трансцендентности
#matr <- matrix(nrow = 9, ncol = 9)
#matr[1,] <- c(0,cor(e1$Td,e1$K0),cor(e1$Td,e1$Tmes),cor(e1$Td,e1$Tpr),cor(e1$Td,e1$KN1),cor(e1$Td,e1$EK1),cor(e1$Td,e1$KN2),cor(e1$Td,e1$EK2),cor(e1$Td,e1$Tspr))
#matr[2,] <- c(cor(e1$K0,e1$K0),0,cor(e1$K0,e1$Tmes),cor(e1$K0,e1$Tpr),cor(e1$K0,e1$KN1),cor(e1$K0,e1$EK1),cor(e1$K0,e1$KN2),cor(e1$K0,e1$EK2),cor(e1$K0,e1$Tspr))
#matr[3,] <- c(cor(e1$Tmes,e1$Tmes),cor(e1$Tmes,e1$K0),0,cor(e1$Tmes,e1$Tpr),cor(e1$Tmes,e1$KN1),cor(e1$Tmes,e1$EK1),cor(e1$Tmes,e1$KN2),cor(e1$Tmes,e1$EK2),cor(e1$Tmes,e1$Tspr))
#matr[4,] <- c(cor(e1$Tpr,e1$Tpr),cor(e1$Tpr,e1$K0),cor(e1$Tpr,e1$Tmes),0,cor(e1$Tpr,e1$KN1),cor(e1$Tpr,e1$EK1),cor(e1$Tpr,e1$KN2),cor(e1$Tpr,e1$EK2),cor(e1$Tpr,e1$Tspr))
#matr[5,] <- c(cor(e1$KN1,e1$KN1),cor(e1$KN1,e1$K0),cor(e1$KN1,e1$Tmes),cor(e1$KN1,e1$Tpr),0,cor(e1$KN1,e1$EK1),cor(e1$KN1,e1$KN2),cor(e1$KN1,e1$EK2),cor(e1$KN1,e1$Tspr))
#matr[6,] <- c(cor(e1$EK1,e1$EK1),cor(e1$EK1,e1$K0),cor(e1$EK1,e1$Tmes),cor(e1$EK1,e1$Tpr),cor(e1$EK1,e1$KN1),0,cor(e1$EK1,e1$KN2),cor(e1$EK1,e1$EK2),cor(e1$EK1,e1$Tspr))
#matr[7,] <- c(cor(e1$KN2,e1$KN2),cor(e1$KN2,e1$K0),cor(e1$KN2,e1$Tmes),cor(e1$KN2,e1$Tpr),cor(e1$KN2,e1$KN1),cor(e1$KN2,e1$EK1),0,cor(e1$KN2,e1$EK2),cor(e1$KN2,e1$Tspr))
#matr[8,] <- c(cor(e1$EK2,e1$EK2),cor(e1$EK2,e1$K0),cor(e1$EK2,e1$Tmes),cor(e1$EK2,e1$Tpr),cor(e1$EK2,e1$KN1),cor(e1$EK2,e1$EK1),cor(e1$EK2,e1$KN2),0,cor(e1$EK2,e1$Tspr))
#matr[9,] <- c(cor(e1$Tspr,e1$Tspr),cor(e1$Tspr,e1$K0),cor(e1$Tspr,e1$Tmes),cor(e1$Tspr,e1$Tpr),cor(e1$Tspr,e1$KN1),cor(e1$Tspr,e1$EK1),cor(e1$Tspr,e1$KN2),cor(e1$Tspr,e1$EK2),0)

matr <- matrix(nrow = 8, ncol = 8)
matr[1,] <- c(0,cor(e1$Td,e1$K0),cor(e1$Td,e1$Tpr),cor(e1$Td,e1$KN1),cor(e1$Td,e1$EK1),cor(e1$Td,e1$KN2),cor(e1$Td,e1$EK2),cor(e1$Td,e1$Tspr))
matr[2,] <- c(cor(e1$K0,e1$K0),0,cor(e1$K0,e1$Tpr),cor(e1$K0,e1$KN1),cor(e1$K0,e1$EK1),cor(e1$K0,e1$KN2),cor(e1$K0,e1$EK2),cor(e1$K0,e1$Tspr))
matr[3,] <- c(cor(e1$Tpr,e1$Tpr),cor(e1$Tpr,e1$K0),0,cor(e1$Tpr,e1$KN1),cor(e1$Tpr,e1$EK1),cor(e1$Tpr,e1$KN2),cor(e1$Tpr,e1$EK2),cor(e1$Tpr,e1$Tspr))
matr[4,] <- c(cor(e1$KN1,e1$KN1),cor(e1$KN1,e1$K0),cor(e1$KN1,e1$Tpr),0,cor(e1$KN1,e1$EK1),cor(e1$KN1,e1$KN2),cor(e1$KN1,e1$EK2),cor(e1$KN1,e1$Tspr))
matr[5,] <- c(cor(e1$EK1,e1$EK1),cor(e1$EK1,e1$K0),cor(e1$EK1,e1$Tpr),cor(e1$EK1,e1$KN1),0,cor(e1$EK1,e1$KN2),cor(e1$EK1,e1$EK2),cor(e1$EK1,e1$Tspr))
matr[6,] <- c(cor(e1$KN2,e1$KN2),cor(e1$KN2,e1$K0),cor(e1$KN2,e1$Tpr),cor(e1$KN2,e1$KN1),cor(e1$KN2,e1$EK1),0,cor(e1$KN2,e1$EK2),cor(e1$KN2,e1$Tspr))
matr[7,] <- c(cor(e1$EK2,e1$EK2),cor(e1$EK2,e1$K0),cor(e1$EK2,e1$Tpr),cor(e1$EK2,e1$KN1),cor(e1$EK2,e1$EK1),cor(e1$EK2,e1$KN2),0,cor(e1$EK2,e1$Tspr))
matr[8,] <- c(cor(e1$Tspr,e1$Tspr),cor(e1$Tspr,e1$K0),cor(e1$Tspr,e1$Tpr),cor(e1$Tspr,e1$KN1),cor(e1$Tspr,e1$EK1),cor(e1$Tspr,e1$KN2),cor(e1$Tspr,e1$EK2),0)


concept.names <- c("Td","K0","Tpr","KN1","EK2","KN2","EK2","Tspr")

results <- nochanges.scenario(matr,iter = 10,concept.names)

graph.fcm(matr,concept.sizes = results$Equilibrium_value,concept.names)

############################################################

#Sys.setlocale("LC_ALL", "Ru_Ru")
Sys.setlocale("LC_ALL", "en_US")

#Путь к файлам с данными
setwd("")

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
