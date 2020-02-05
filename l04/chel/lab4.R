#путь
setwd("H:/R/R/Labs")
#читаем файл
myData<- read.csv("6_1_1_2.csv", header = T, sep = ";")

x=1:731

View(myData)

#--Корреляции
cor(myData$Tpr[2:500], myData$Td[2:500])
cor(myData$Tpr[2:500], myData$K0[2:500])
cor(myData$Tpr[2:500], myData$Tmes[2:500])
cor(myData$Tpr[2:500], myData$KN1[2:500])
cor(myData$Tpr[2:500], myData$EK1[2:500])
cor(myData$Tpr[2:500], myData$KN2[2:500])
cor(myData$Tpr[2:500], myData$EK2[2:500])
cor(myData$Tpr[2:500], myData$Tspr[2:500])

library(dtw)
dtw1 <- dtw(myData$Tpr[1:500], myData$Td[1:500], k=TRUE, step.pattern = symmetric1)
dtw1$distance
plot(dtw1, type="two", off=1, match.lty=2)

dtw2 <- dtw(myData$Tpr[1:500], myData$K0[1:500], k=TRUE, step.pattern = symmetric1)
dtw2$distance
plot(dtw2, type="two", off=1, match.lty=2)

dtw3 <- dtw(myData$Tpr[1:500], myData$Tmes[1:500], k=TRUE, step.pattern = symmetric1)
dtw3$distance
plot(dtw3, type="two", off=1, match.lty=2)

dtw4 <- dtw(myData$Tpr[1:500], myData$KN1[1:500], k=TRUE, step.pattern = symmetric1)
dtw4$distance
plot(dtw4, type="two", off=1, match.lty=2)

dtw5 <- dtw(myData$Tpr[1:500], myData$EK1[1:500], k=TRUE, step.pattern = symmetric1)
dtw5$distance
plot(dtw5, type="two", off=1, match.lty=2)

dtw6 <- dtw(myData$Tpr[1:500], myData$KN2[1:500], k=TRUE, step.pattern = symmetric1)
dtw6$distance
plot(dtw6, type="two", off=1, match.lty=2)

dtw7 <- dtw(myData$Tpr[1:500], myData$Tspr[1:500], k=TRUE, step.pattern = symmetric1)
dtw7$distance
plot(dtw7, type="two", off=1, match.lty=2)

#Зависимость от набора данных(количество измерений)
#Выделяем набор данных для которого будем строить регрессионную модель
e1<- data.frame(y=myData$Td[1:500], x=x[1:500], z=myData$Tpr[1:500])
e2<- data.frame(y=myData$Td[100:500], x=x[100:500], z=myData$Tpr[100:500])
e3<- data.frame(y=myData$Td[200:500], x=x[200:500], z=myData$Tpr[200:500])
e4<- data.frame(y=myData$Td[300:500], x=x[300:500], z=myData$Tpr[300:500])
e5<- data.frame(y=myData$Td[400:500], x=x[400:500], z=myData$Tpr[400:500])

plot(x[1:731], myData$Tpr[1:731], type = "l", xlab="Номер измерения", ylab = "Значение параметра")

#Модель на 500 точках
res <- nls(z~a+b*x+c*x^2+f*y+g*y^2, data=e1, start=list(a=0.1, b=0.1, c=0.1, f=0.1, g=0.1))
coef_func <- coef(res)
y <- myData$Td[1:731]
zfunc <- coef_func[1]+ coef_func[2]*x + coef_func[3]*x^2 + coef_func[4]*y + coef_func[5]*y^2
lines(x[1:731], zfunc[1:731], type = "l", col="red")
var.test(myData$Tpr[501:730], zfunc[501:730])
#Модель на 400 точках
res <- nls(z~a+b*x+c*x^2+f*y+g*y^2, data=e2, start=list(a=0.1, b=0.1, c=0.1, f=0.1, g=0.1))
coef_func <- coef(res)
y <- myData$Td[1:731]
zfunc <- coef_func[1]+ coef_func[2]*x + coef_func[3]*x^2 + coef_func[4]*y + coef_func[5]*y^2
lines(x[100:731], zfunc[100:731], type = "l", col="blue")
var.test(myData$Tpr[501:730], zfunc[501:730])
#Модель на 300 точках
res <- nls(z~a+b*x+c*x^2+f*y+g*y^2, data=e3, start=list(a=0.1, b=0.1, c=0.1, f=0.1, g=0.1))
coef_func <- coef(res)
y <- myData$Td[1:731]
zfunc <- coef_func[1]+ coef_func[2]*x + coef_func[3]*x^2 + coef_func[4]*y + coef_func[5]*y^2
lines(x[200:731], zfunc[200:731], type = "l", col="green")
var.test(myData$Tpr[501:730], zfunc[501:730])
#Модель на 200 точках
res <- nls(z~a+b*x+c*x^2+f*y+g*y^2, data=e4, start=list(a=0.1, b=0.1, c=0.1, f=0.1, g=0.1))
coef_func <- coef(res)
y <- myData$Td[1:731]
zfunc <- coef_func[1]+ coef_func[2]*x + coef_func[3]*x^2 + coef_func[4]*y + coef_func[5]*y^2
lines(x[300:731], zfunc[300:731], type = "l", col="yellow")
var.test(myData$Tpr[501:730], zfunc[501:730])
#Модель на 100 точках
res <- nls(z~a+b*x+c*x^2+f*y+g*y^2, data=e5, start=list(a=0.1, b=0.1, c=0.1, f=0.1, g=0.1))
coef_func <- coef(res)
y <- myData$Td[1:731]
zfunc <- coef_func[1]+ coef_func[2]*x + coef_func[3]*x^2 + coef_func[4]*y + coef_func[5]*y^2
lines(x[400:731], zfunc[400:731], type = "l", col="cyan")
var.test(myData$Tpr[501:730], zfunc[501:730])



#-- МЕТОД KNN
#Зависимость от набора данных(количество измерений)
library(caret)  #библиотека позволяет использовать методы машинного обучения

#Модель на 500 точках
mod1<- train(z~x+y, data = e1, method = "knn")
test<-data.frame(y=y[1:731], x=x[1:731])
res1<- predict(mod1, test)
#рисуем график
plot(x[1:731], myData$Tpr[1:731], type = "l", xlab = "Номер измерения", ylab = "Значение параметра")
lines(x[1:730], res1[1:730], type = "l", col="red")
#Критерий Фишера
var.test(myData$Tpr[501:730], res1[501:731])

#Модель на 400 точках
mod1<- train(z~x+y, data = e2, method = "knn")
test<-data.frame(y=y[1:731], x=x[1:731])
res1<- predict(mod1, test)
lines(x[100:730], res1[100:730], type = "l", col="blue")
#Критерий Фишера
var.test(myData$Tpr[501:730], res1[501:731])

#Модель на 300 точках
mod1<- train(z~x+y, data = e3, method = "knn")
test<-data.frame(y=y[1:731], x=x[1:731])
res1<- predict(mod1, test)
lines(x[200:730], res1[200:730], type = "l", col="green")
#Критерий Фишера
var.test(myData$Tpr[501:730], res1[501:731])

#Модель на 200 точках
mod1<- train(z~x+y, data = e4, method = "knn")
test<-data.frame(y=y[1:731], x=x[1:731])
res1<- predict(mod1, test)
lines(x[300:730], res1[300:730], type = "l", col="yellow")
#Критерий Фишера
var.test(myData$Tpr[501:730], res1[501:731])

#Модель на 100 точках
mod1<- train(z~x+y, data = e5, method = "cyan")
test<-data.frame(y=y[1:731], x=x[1:731])
res1<- predict(mod1, test)
lines(x[400:730], res1[400:730], type = "l", col="red")
#Критерий Фишера
var.test(myData$Tpr[501:730], res1[501:731])







#-- МЕТОД SVM
#Зависимость от набора данных(количество измерений)
#library(caret) #подключаем библиотеку, позволяющую использовать методы машинного обучения

plot(x[1:731], myData$Tpr[1:731], type = "l", xlab = "Номер измерения", ylab = "Значение параметра")

#Модель на 500 точках
mod2<- train(z~x+y, data=e1, method="svmPoly")  #можно использовать так же линейный - svmLinear; или svmPoly
test<-data.frame(y=y[1:731], x=x[1:731])
res2<-predict(mod2, test)
lines(x[1:731], res2[1:731], type = "l", col="red")
var.test(myData$Tpr[501:731], res2[501:731])
#Модель на 400 точках
mod2<- train(z~x+y, data=e2, method="svmPoly")  #можно использовать так же линейный - svmLinear; или svmPoly
test<-data.frame(y=y[1:731], x=x[1:731])
res2<-predict(mod2, test)
lines(x[100:731], res2[100:731], type = "l", col="blue")
var.test(myData$Tpr[501:731], res2[501:731])
#Модель на 300 точках
mod2<- train(z~x+y, data=e3, method="svmPoly")  #можно использовать так же линейный - svmLinear; или svmPoly
test<-data.frame(y=y[1:731], x=x[1:731])
res2<-predict(mod2, test)
lines(x[200:731], res2[200:731], type = "l", col="green")
var.test(myData$Tpr[501:731], res2[501:731])
#Модель на 200 точках
mod2<- train(z~x+y, data=e4, method="svmPoly")  #можно использовать так же линейный - svmLinear; или svmPoly
test<-data.frame(y=y[1:731], x=x[1:731])
res2<-predict(mod2, test)
lines(x[300:731], res2[300:731], type = "l", col="yellow")
var.test(myData$Tpr[501:731], res2[501:731])
#Модель на 100 точках
mod2<- train(z~x+y, data=e5, method="svmPoly")  #можно использовать так же линейный - svmLinear; или svmPoly
test<-data.frame(y=y[1:731], x=x[1:731])
res2<-predict(mod2, test)
lines(x[400:731], res2[400:731], type = "l", col="cyan")
var.test(myData$Tpr[501:731], res2[501:731])




#-- МЕТОД АВТОРЕГРЕССИИ
#Зависимость от набора данных(количество измерений)
library(forecast)
plot(x[1:731], myData$Tpr[1:731], type = "l", xlab = "Номер измерения", ylab = "Значение параметра")
#Модель на 500 точках
mod3<- auto.arima(e1$z)
res3<- forecast(mod3, h=231) #231 прогнозируемых значений
lines(x[501:731], res3$mean[1:231], type = "l", col="red")
var.test(myData$Tpr[501:731], res3$mean[1:231])
#Модель на 400 точках
mod3<- auto.arima(e2$z)
res3<- forecast(mod3, h=231) #231 прогнозируемых значений
lines(x[501:731], res3$mean[1:231], type = "l", col="blue")
var.test(myData$Tpr[501:731], res3$mean[1:231])
#Модель на 300 точках
mod3<- auto.arima(e3$z)
res3<- forecast(mod3, h=231) #231 прогнозируемых значений
lines(x[501:731], res3$mean[1:231], type = "l", col="green")
var.test(myData$Tpr[501:731], res3$mean[1:231])
#Модель на 200 точках
mod3<- auto.arima(e4$z)
res3<- forecast(mod3, h=231) #231 прогнозируемых значений
lines(x[501:731], res3$mean[1:231], type = "l", col="yellow")
var.test(myData$Tpr[501:731], res3$mean[1:231])
#Модель на 100 точках
mod3<- auto.arima(e5$z)
res3<- forecast(mod3, h=231) #231 прогнозируемых значений
lines(x[501:731], res3$mean[1:231], type = "l", col="cyan")
var.test(myData$Tpr[501:731], res3$mean[1:231])



#-- МЕТОД ВАЙЛЕТ АНАЛИЗА
library(WaveletComp)
library(biwavelet)
#Зависимость от набора данных(количество измерений)
prediction_wavelet <- function(test_data, train_data, test_time, train_time, values){
  endingIndex <- length(train_data)
  Sn1 <- as.data.frame(test_data, row.names = NULL)
  Sn2 <- as.data.frame(train_data, row.names = NULL)
  ny.w <- analyze.wavelet(Sn2, "train_data", loess.span = 0, dt=1, dj = 1/298, lowerPeriod = 1/4, upperPeriod = 256, make.pval = F, n.sim = 10)
  #гармоника с периодом 3
  ny.rec3 <- reconstruct(ny.w, sel.period = 3, show.legend = F)
  x.rec3 <- ny.rec3$series$train_data.r
  model3 <- auto.arima(x.rec3)
  future3 <- forecast(model3, h=values)
  #гармоника с периодом 8
  ny.rec8 <- reconstruct(ny.w, sel.period = 8, show.legend = F)
  x.rec8 <- ny.rec8$series$train_data.r
  model8 <- auto.arima(x.rec8)
  future8 <- forecast(model8, h=values)
  #гармоника с периодом 16
  ny.rec16 <- reconstruct(ny.w, sel.period = 16, show.legend = F)
  x.rec16 <- ny.rec16$series$train_data.r
  model16 <- auto.arima(x.rec16)
  future16 <- forecast(model16, h=values)
  #гармоника с периодом 172
  ny.rec172 <- reconstruct(ny.w, sel.period = 172, show.legend = F)
  x.rec172 <- ny.rec172$series$train_data.r
  model172 <- auto.arima(x.rec172)
  future172 <- forecast(model172, h=values)
  res <- NA
  s <- (future3$mean + future8$mean + future16$mean + future172$mean)/4
  return(s)
}
#Модель на 500 точках
res4_1 <- prediction_wavelet(myData$Tpr[501:731], e1$z, x[501:731], e1$x, 231)
#Модель на 400 точках
res4_2 <- prediction_wavelet(myData$Tpr[501:731], e2$z, x[501:731], e2$x, 231)
#Модель на 300 точках
res4_3 <- prediction_wavelet(myData$Tpr[501:731], e3$z, x[501:731], e3$x, 231)
#Модель на 200 точках
res4_4 <- prediction_wavelet(myData$Tpr[501:731], e4$z, x[501:731], e4$x, 231)
#Модель на 100 точках
res4_5 <- prediction_wavelet(myData$Tpr[501:731], e5$z, x[501:731], e5$x, 231)

plot(x[1:731], myData$Tpr[1:731], type="l", xlab = "Номер измерения", ylab = "Значение параметра")
lines(x[501:731], res4_1, type="l", col="red")
var.test(myData$Tpr[501:731], res4_1)

lines(x[501:731], res4_2, type="l", col="blue")
var.test(myData$Tpr[501:731], res4_2)

lines(x[501:731], res4_3, type="l", col="green")
var.test(myData$Tpr[501:731], res4_3)

lines(x[501:731], res4_4, type="l", col="yellow")
var.test(myData$Tpr[501:731], res4_4)

lines(x[501:731], res4_5, type="l", col="cyan")
var.test(myData$Tpr[501:731], res4_5)





#-- ФРАКТАЛЬНЫЙ МЕТОД
library(fractaldim)
#Зависимость от набора данных(количество измерений)
plot(x[1:731], myData$Tpr[1:731], type="l", xlab = "Номер измерения", ylab = "Значение параметра")
#основная функция расчёта
prediction_frac <- function(test_data, train_data, test_time, train_time, values)
{
  endingIndex <- length(train_data)
  total_error <- 0
  error_per_prediction <- c()
  method <- "rodogram"
  random_sample_count <- values
  
  Sn1 <- as.data.frame(test_data, row.names = NULL)  #test
  Sn2 <- as.data.frame(train_data, row.names = NULL)  #train
  
  for(i in 1:values){
    delta <- c()
    for(j in 2:length(train_data)){
      delta <-rbind(delta, (train_data[j]-train_data[j-1]))
    }
    Std_delta <- apply(delta, 2, sd)
    V_Reference <- fd.estimate(train_data, method=method, trim=TRUE)$fd
    Sn_guesses <- rnorm(random_sample_count, mean=train_data[length(train_data)], sd=Std_delta)
    
    minDifference = 1000000
    
    for(j in 1:length(Sn_guesses)){
      new_Sn <- rbind(Sn2, Sn_guesses[j])
      new_V_Reference <- fd.estimate(new_Sn$train_data, method=method, trim=TRUE)$fd
      
      if(abs(new_V_Reference - V_Reference) < minDifference){
        Sn_prediction <- Sn_guesses[j]
        minDifference = abs(new_V_Reference - V_Reference)
      }
    }
    Sn2 <- rbind(Sn2, Sn_prediction)
  }
  
  s<- Sn2[,1]
  ss<-s[(endingIndex+1):(endingIndex+values)]
  return(ss)
}

#Модель на 500 точках
res5<-prediction_frac(myData$Tpr[501:731], e1$z, x[501:731], e1$x, 231)
lines(x[501:731], res5, type="l", col="red")
var.test(myData$Tpr[501:731], res5)
#Модель на 400 точках
res5<-prediction_frac(myData$Tpr[501:731], e2$z, x[501:731], e2$x, 231)
lines(x[501:731], res5, type="l", col="green")
var.test(myData$Tpr[501:731], res5)
#Модель на 300 точках
res5<-prediction_frac(myData$Tpr[501:731], e3$z, x[501:731], e3$x, 231)
lines(x[501:731], res5, type="l", col="blue")
var.test(myData$Tpr[501:731], res5)
#Модель на 200 точках
res5<-prediction_frac(myData$Tpr[501:731], e4$z, x[501:731], e4$x, 231)
lines(x[501:731], res5, type="l", col="yellow")
var.test(myData$Tpr[501:731], res5)
#Модель на 100 точках
res5<-prediction_frac(myData$Tpr[501:731], e5$z, x[501:731], e5$x, 231)
lines(x[501:731], res5, type="l", col="cyan")
var.test(myData$Tpr[501:731], res5)












