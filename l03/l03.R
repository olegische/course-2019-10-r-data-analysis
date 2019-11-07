#путь к файлам с данными
setwd("~/romcode/courses/r-data-analysis/l03")

MyData<-read.csv("6_1_1_2.csv", 
                 header=T, 
                 sep=";")

x<-1:731 

myDataDf<-data.frame(y=MyData$Td[1:500], x=x[1:500])

# Метод регрессии с использованием многочленов (МНК)
plot(x[1:731],
     MyData$Td[1:731], 
     type="l", 
     xlab="номер измерения", 
     ylab="значение параметра")

res10<-nls(y~a+b*x+c*x^2+d*x^3,
         data=myDataDf,
         start=list(a=0.1,b=0.1,c=0.1,d=0.1))

coef_func10<-coef(res10)

yfunc10<-coef_func10[1]+
  coef_func10[2]*x+
  coef_func10[3]*x^2+
  coef_func10[4]*x^3

lines(x[1:500],
      yfunc10[1:500],
      type="l",
      col="red")
lines(x[501:731],
      yfunc10[501:731],
      type="l",
      col="green")

# Использование Box-диаграмм для сопоставления тестовой выборки и гипотезы
boxplot(MyData$Td[501:731],
        yfunc10[501:731])

# Критерий Хи-квадрат
test10 <- matrix(c(MyData$Td[501:731]+100,
                yfunc10[501:731]+100),
                nrow = 2,
                byrow = T)

# смотрим на p-value (вероятность принятия гипотезы)
chisq.test(test10)

# Критерий Фишера
var.test(MyData$Td[501:731],
         yfunc10[501:731])

# Критерий Стьюдента
t.test(MyData$Td[501:731],
       yfunc10[501:731],
       paired = T)

## добавляем новый параметр z = f(x,y)
## Td - температура в котельной, Tpr - температура на улице
myDataDf11<-data.frame(y=MyData$Td[1:500],
               x=x[1:500],
               z=MyData$Tpr[1:500])

## Метод регрессии с использованием многочленов (МНК)
## График
plot(x[1:731],
     MyData$Td[1:731],
     type="l",
     xlab="номер измерения",
     ylab="значение параметра",
     ylim = c(-30,145))
lines(x[1:731],
      MyData$Tpr[1:731],
      type="l",
      col="black")

## Подбор коэффициентов регрессионной модели
res11 <- nls(z~a+b*x+c*x^2+f*y+g*y^2,
             data=myDataDf11,
             start=list(a=0.1,b=0.1,c=0.1,f=0.1,g=0.1))

## Получение значений коэффициентов
coef_func11<-coef(res11)

## Загрузка y параметра
y <- MyData$Td[1:731]

## Построение регрессионной функции
zfunc11 <- coef_func11[1]+
  coef_func11[2]*x+
  coef_func11[3]*x^2+
  coef_func11[4]*y+
  coef_func11[5]*y^2

## Вывод графика функции
lines(x[1:500],
      zfunc11[1:500],
      type="l",
      col="red")
lines(x[501:731],
      zfunc11[501:731],
      type="l",
      col="green")

## Использование Box-диаграмм для сопоставления тестовой выборки и гипотезы
boxplot(MyData$Tpr[501:731],
        zfunc11[501:731])

## Критерий Хи-квадрат
test11 <- matrix(c(MyData$Tpr[501:731]+100,
                 zfunc11[501:731]+100),
                 nrow = 2,
                 byrow = T)
## смотрим на p-value (вероятность принятия гипотезы)
chisq.test(test11)

## Критерий Фишера
var.test(MyData$Tpr[501:731],
         zfunc11[501:731])

## Критерий Стьюдента
t.test(MyData$Tpr[501:731],
       zfunc11[501:731],
       paired = T)

### Зависимость переменной от самой себя на предыдущем шаге z = f(x,y,z(i-1))
### Td - температура в котельной, Tpr - температура на улице
myDataDf11<-data.frame(y=MyData$Td[1:500],
               x=x[1:500],
               z=MyData$Tpr[1:500])

e2<-data.frame(y=MyData$Td[2:500],
               x=x[2:500],
               z=MyData$Tpr[2:500],
               z2=MyData$Tpr[1:499])

### Метод регрессии с использованием многочленов (МНК)
### График
plot(x[1:731],MyData$Td[1:731],
     type="l",
     xlab="номер измерения",
     ylab="значение параметра",
     ylim = c(-30,145))
lines(x[1:731],
      MyData$Tpr[1:731],
      type="l",
      col="black")

### Подбор коэффициентов регрессионной модели
res10 <- nls(z~a+b*x+c*x^2+f*y+g*y^2,
             data=myDataDf11,
             start=list(a=0.1,b=0.1,c=0.1,f=0.1,g=0.1))

### Получение значений коэффициентов
coef_func10<-coef(res10)

### Загрузка y параметра
y <- MyData$Td[1:731]

### Построение регрессионной функции
zfunc <- coef_func10[1]+
  coef_func10[2]*x+
  coef_func10[3]*x^2+
  coef_func10[4]*y+
  coef_func10[5]*y^2

### Вывод графика функции
lines(x[1:500],
      zfunc[1:500],
      type="l",
      col="red")
lines(x[501:731],
      zfunc[501:731],
      type="l",
      col="green")

### Использование Box-диаграмм для сопоставления тестовой выборки и гипотезы
boxplot(MyData$Tpr[501:731],
        zfunc[501:731])

### Критерий Хи-квадрат
test10 <- matrix(c(MyData$Tpr[501:731]+100,
                 zfunc[501:731]+100),
                 nrow = 2,
                 byrow = T)

### смотрим на p-value (вероятность принятия гипотезы)
chisq.test(test10)

### Критерий Фишера
var.test(MyData$Tpr[501:731],
         zfunc[501:731])

### Критерий Стьюдента
t.test(MyData$Tpr[501:731],
       zfunc[501:731],
       paired = T)

# Метод kNN
library(caret) # библиотека, позволяющая использовать методы машинного обучения

mod1 <- train(y~x,
              data=e,
              method="knn")

res1 <- predict(mod1,
                x[501:731])

plot(x[1:731], MyData$Td[1:731],
     type="l",
     xlab="номер измерения",
     ylab="значение параметра")
lines(x[1:500],
      res1[1:500],
      type="l",
      col="red")
lines(x[501:731],
      res1[501:731],
      type="l",
      col="green")

# Использование Box-диаграмм для сопоставления тестовой выборки и гипотезы
boxplot(MyData$Td[501:731],
        res1[501:731])

# Критерий Хи-квадрат
test1 <- matrix(c(MyData$Td[501:731]+100,
                res1[501:731]+100),
                nrow = 2,
                byrow = T)

# смотрим на p-value (вероятность принятия гипотезы)
chisq.test(test1)

# Критерий Фишера
var.test(MyData$Td[501:731],
         res1[501:731])

# Критерий Стьюдента
t.test(MyData$Td[501:731],
       res1[501:731],
       paired = T)

## добавляем новый параметр
mod10 <- train(z~x+y,
               data=e1,
               method="knn")

testDf10 <- data.frame(y=y[1:731],
                       x=x[1:731])

res10 <- predict(mod10,
                 testDf10)

plot(x[1:731], MyData$Tpr[1:731],
     type="l",
     xlab="номер измерения",
     ylab="значение параметра")
lines(x[1:500],
      res10[1:500],
      type="l",
      col="red")
lines(x[501:731],
      res10[501:731],
      type="l",
      col="green")

## Использование Box-диаграмм для сопоставления тестовой выборки и гипотезы
boxplot(MyData$Tpr[501:731],
        res10[501:731])

## Критерий Хи-квадрат
test10 <- matrix(c(MyData$Tpr[501:731]+100,
                 res10[501:731]+100),
                 nrow = 2,
                 byrow = T)

## смотрим на p-value (вероятность принятия гипотезы)
chisq.test(test10)

## Критерий Фишера
var.test(MyData$Tpr[501:731],
         res10[501:731])

## Критерий Стьюдента
t.test(MyData$Tpr[501:731],
       res10[501:731],
       paired = T)


# Метод SVM
#library(caret)
install.packages("kernlab")
#library(kernlab)

mod2 <- train(y~x,
              data=e,
              method="svmPoly") #"svmLinear")
res2 <- predict(mod2,
                x[501:731])

plot(x[1:731], MyData$Td[1:731],
     type="l",
     xlab="номер измерения",
     ylab="значение параметра")
lines(x[1:500],
      res2[1:500],
      type="l",
      col="red")
lines(x[501:731],
      res2[501:731],
      type="l",
      col="green")

# Использование Box-диаграмм для сопоставления тестовой выборки и гипотезы
boxplot(MyData$Td[501:731],
        res2[501:731])

# Критерий Хи-квадрат
test2 <- matrix(c(MyData$Td[501:731]+100,
                res2[501:731]+100),
                nrow = 2,
                byrow = T)

# смотрим на p-value (вероятность принятия гипотезы)
chisq.test(test2)

# Критерий Фишера
var.test(MyData$Td[501:731],
         res2[501:731])

# Критерий Стьюдента
t.test(MyData$Td[501:731],
       res2[501:731],
       paired = T)

## добавляем новый параметр
mod20 <- train(z~x+y,
               data=e1,
               method="svmPoly")
testDf20 <- data.frame(y=y[1:731],
                       x=x[1:731])

res20 <- predict(mod20, testDf20)

plot(x[1:731], MyData$Tpr[1:731],
     type="l",
     xlab="номер измерения",
     ylab="значение параметра")
lines(x[1:500],
      res20[1:500],
      type="l",
      col="red")
lines(x[501:731],
      res20[501:731],
      type="l",
      col="green")

## Использование Box-диаграмм для сопоставления тестовой выборки и гипотезы
boxplot(MyData$Tpr[501:731],
        res20[501:731])

## Критерий Хи-квадрат
test20 <- matrix(c(MyData$Tpr[501:731]+100,
                 res20[501:731]+100),
                 nrow = 2,
                 byrow = T)

# смотрим на p-value (вероятность принятия гипотезы)
chisq.test(test20)

## Критерий Фишера
var.test(MyData$Tpr[501:731],
         res20[501:731])

## Критерий Стьюдента
t.test(MyData$Tpr[501:731],
       res20[501:731],
       paired = T)


# Метод авторегрессии
library(forecast)

mod3 <- auto.arima(e$y)

res3 <- forecast(mod3, h=230)

plot(res3)
lines(x[501:731],
      MyData$Td[501:731],
      type="l",
      col="black")

# Использование Box-диаграмм для сопоставления тестовой выборки и гипотезы
boxplot(MyData$Td[501:731],
        res3$mean[1:230])

# Критерий Хи-квадрат
test3 <- matrix(c(MyData$Td[501:730]+100,
                res3$mean[1:230]+100),
                nrow = 2,
                byrow = T)

# смотрим на p-value (вероятность принятия гипотезы)
chisq.test(test3)

# Критерий Фишера
var.test(MyData$Td[501:730],
         res3$mean[1:230])

# Критерий Стьюдента
t.test(MyData$Td[501:730],
       res3$mean[1:230],
       paired = T)

# Метод вейвлет анализа
library(WaveletComp)
library(biwavelet)

mod4 <- analyze.wavelet(e,my.series = 1,
                        loess.span = 0,
                        dt = 1,
                        dj = 1/298,
                        lowerPeriod = 1/4,
                        upperPeriod = 380,
                        make.pval = F,
                        n.sim = 10)

wt.image(mod4,
         n.levels = 337,
         legend.params = list(lab="степень влияния"))

res4_1 <- reconstruct(mod4,
                      sel.period = 3,
                      show.legend = F)
res4_2 <- reconstruct(mod4,
                      sel.period = 6,
                      show.legend = F)
res4_3 <- reconstruct(mod4,
                      sel.period = 16,
                      show.legend = F)
res4_4 <- reconstruct(mod4,
                      sel.period = 64,
                      show.legend = F)
res4_5 <- reconstruct(mod4,
                      sel.period = 256,
                      show.legend = F)
res4_6 <- reconstruct(mod4,
                      sel.period = 350,
                      show.legend = F)
res4_7 <- reconstruct(mod4,
                      sel.period = 365,
                      show.legend = F)
res4_8 <- reconstruct(mod4,
                      sel.period = 380,
                      show.legend = F)

plot(x[1:731], MyData$Td[1:731],
     type="l",
     xlab="номер измерения",
     ylab="значение параметра")
lines(x[1:500],
      res4_5$series$y.r[1:500],
      col="red",
      type="l")
lines(x[1:500], res4_6$series$y.r[1:500],
      col="blue",
      type="l")
lines(x[1:500],
      res4_7$series$y.r[1:500],
      col="green",
      type="l")
lines(x[1:500],
      res4_8$series$y.r[1:500],
      col="gold",
      type="l")

# прогноз по каждой гармонике отдельно

mod4_rec4 <- auto.arima(res4_5$series$y.r)

res4_5_1 <- forecast(mod4_rec4,h=230)

lines(x[501:730],
      res4_5_1$mean[1:230],
      col="green",
      type="l")

# Использование Box-диаграмм для сопоставления тестовой выборки и гипотезы
boxplot(MyData$Td[501:731],
        res4_5_1$mean[1:230])

# Критерий Хи-квадрат
test4 <- matrix(c(MyData$Td[501:730]+100,
                res4_5_1$mean[1:230]+100),
                nrow = 2,
                byrow = T)

# смотрим на p-value (вероятность принятия гипотезы)
chisq.test(test4)

# Критерий Фишера
var.test(MyData$Td[501:730],
         res4_5_1$mean[1:230])

# Критерий Стьюдента
t.test(MyData$Td[501:730],
       res4_5_1$mean[1:230],
       paired = T)

## метод не параметрический

# Фрактальный метод
#library(quantmod)
library(fractaldim)

values <- 230
endingIndex <- 500
total_error <- 0
error_per_prediction <- c()
method <- "rodogram"
random_sample_count <- values

Sm1 <- as.data.frame(MyData$Td[501:731],
                     row.names = NULL) #test

Sm2 <- as.data.frame(e$y,
                     row.names = NULL) #train

for(i in 1:values){
  delta <- c()
  for(j in 2:500){
    delta <- rbind(delta, (e$y[j]-e$y[j-1]))
  }
  # calculate standard deviation of delta
  Std_delta <- apply(delta, 2, sd)
  # update fractal dimension used as reference
  V_reference <- fd.estimate(e$y,
                             methods = method,
                             trim = T)$fd
  # create N guesses drawing from the normal distribution
  # use the last value of Sm as mean and the standard deviation
  # of delte as the deviation
  Sm_guesses <- rnorm(random_sample_count,
                      mean = e$y[500],
                      sd = Std_delta)
  minDifference <- 1000000
  # check the fractal dimension of Sm plus each differenct guess and
  # choose the value with the least difference with the reference
  for(j in 1:length(Sm_guesses)){
    new_Sm <- rbind(Sm2, Sm_guesses[j])
    new_V_reference <- fd.estimate(new_Sm$e,
                                   methods = method,
                                   trim = T)$fd
    if(abs(new_V_reference - V_reference) < minDifference){
      Sm_prediction <- Sm_guesses[j]
      minDifference <- abs(new_V_reference - V_reference)
    }
  }
  Sm2 <- rbind(Sm2,Sm_prediction)
}
    
s <- Sm2[,1]
ss <- s[(endingIndex+1):(endingIndex+values)]

plot(x[1:731],MyData$Td[1:731], type="l",
     xlab="номер измерения",
     ylab="значение параметра")
lines(x[501:730],
      ss,
      col="green",
      type="l")

# Использование Box-диаграмм для сопоставления тестовой выборки и гипотезы
boxplot(MyData$Td[501:731],
        ss)

# Критерий Хи-квадрат
test5 <- matrix(c(MyData$Td[501:730]+100,
                ss[1:230]+100),
                nrow = 2,
                byrow = T)
 
# смотрим на p-value (вероятность принятия гипотезы)
chisq.test(test5)

# Критерий Фишера
var.test(MyData$Td[501:730], ss)

# Критерий Стьюдента
t.test(MyData$Td[501:730],
       ss,
       paired = T)

## метод не параметрический

##### Работа с собственными данными
sbOpenData<-read.csv("opendata.csv",
                     header=T,
                     sep=",")
 
xs<-1:73 #изменяем диапазон иксов
 
sbDf<-data.frame(ys=MyData$Td[1:500],
                 x=x[1:500])
