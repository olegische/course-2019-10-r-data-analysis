#путь
setwd("D:/Study/R/Labs")
#читаем файл
myData<- read.csv("6_1_1_2.csv", header = T, sep = ";")
x=1:731
View(myData)
#выделяем набор данных, для которого будем строить регрессионную модель
e<-data.frame(y=myData$Td[1:500], x=x[1:500])

#-- МЕТОД РЕГРЕССИИ С ИСПОЛЬЗОВАНИЕМ МНОГОЧЛЕНОВ
plot(x[1:731], myData$Td[1:731], type = "l", xlab="Номер измерения", ylab = "Значение параметра", ylim=c(-40,70))

#подбираем коэффициенты регрессионной модели
res<- nls(y~a+b*x+c*x^2+d*x^3, data=e, start=list(a=0.1, b=0.1, c=0.1, d=0.1))
#получаем значения коэффициентов
coef_func <- coef(res)
#строим регрессивную функцию
yfunc<-coef_func[1]+ coef_func[2]*x + coef_func[3]*x^2 + coef_func[4]*x^3
#выводим найденную функцию
lines(x[1:500], yfunc[1:500], type="l", col="red")
lines(x[501:731], yfunc[501:731], type="l", col="green")


#-- МЕТОД KNN
library(caret)  #библиотека позволяет использовать методы машинного обучения
mod<- train(y~x, data = e, method = "knn")
res<- predict(mod, x[501:731])
#рисуем график
plot(x[1:731], myData$Td[1:731], type = "l", xlab = "Номер измерения", ylab = "Значение параметра")
lines(x[1:500], res[1:500], type = "l", col="red")
lines(x[501:731], res[501:731], type = "l", col="green")


#-- МЕТОД SVM
#library(caret) #подключаем библиотеку, позволяющую использовать методы машинного обучения
mod2<- train(y~x, data=e, method="svmPoly")  #можно использовать так же линейный - svmLinear; или svmPoly
res2<-predict(mod2, x[501:731])
#Рисуем график
plot(x[1:731], myData$Td[1:731], type = "l", xlab = "Номер измерения", ylab = "Значение параметра")
lines(x[1:500], res2[1:500], type = "l", col="red")
lines(x[501:731], res2[501:731], type = "l", col="green")


#-- МЕТОД АВТОРЕГРЕССИИ
library(forecast)
mod3<- auto.arima(e$y)
res3<- forecast(mod3, h=230) #230 прогнозируемых значений
plot(res3)
lines(x[501:730], myData$Td[501:730], type = "l", col="black")

#-- МЕТОД ВАЙЛЕТ АНАЛИЗА
library(WaveletComp)
library(biwavelet)
mod4<- analyze.wavelet(e, my.series = 1,
                       loess.span = 0,
                       dt = 1, dj = 1/298,
                       lowerPeriod = 1/4,
                       upperPeriod = 256,
                       make.pval = F,
                       n.sim = 10)
wt.image(mod4, n.levels = 337,
         legend.params = list(lab="Степень влияния"))
res4_1<- reconstruct(mod4, sel.period = 3, show.legend = F)
res4_2<- reconstruct(mod4, sel.period = 6, show.legend = F)
res4_3<- reconstruct(mod4, sel.period = 16, show.legend = F)
res4_4<- reconstruct(mod4, sel.period = 64, show.legend = F)
res4_5<- reconstruct(mod4, sel.period = 256, show.legend = F)
plot(x[1:731], myData$Td[1:731], type = "l", xlab = "Номер измерения", ylab = "Значение параметра")
lines(x[1:500], res4_5$series$y.r[1:500], type = "l", col="red")
#прогноз по каждой гармонике отдельно
mod4_rec4 <- auto.arima(res4_5$series$y.r)
res4_5_1 <- forecast(mod4_rec4, h=230)
lines(x[501:730], res4_5_1$mean[1:230], type = "l", col="green")


#-- ФРАКТАЛЬНЫЙ МЕТОД
library(fractaldim)
values <- 230
endingIndex <- 500
total_error <- 0
error_per_prediction <- c()
method <- "rodogram"
random_sample_count <- values
Sn1 <- as.data.frame(myData$Td[501:731], row.names = NULL)  #test
Sn2 <- as.data.frame(e$y, row.names = NULL)  #train
for(i in 1:values){
  delta <- c()
  for(j in 2:500){
    delta <-rbind(delta, (e$y[j]-e$y[j-1]))
  }
  Std_delta <- apply(delta, 2, sd)
  V_Reference <- fd.estimate(e$y, method=method, trim=TRUE)$fd
  Sn_guesses <- rnorm(random_sample_count, mean=e$y[500], sd=Std_delta)
  
  minDifference = 1000000
  
  for(j in 1:length(Sn_guesses)){
    new_Sn <- rbind(Sn2, Sn_guesses[j])
    new_V_Reference <- fd.estimate(new_Sn$e, method=method, trim=TRUE)$fd
    
    if(abs(new_V_Reference - V_Reference) < minDifference){
      Sn_prediction <- Sn_guesses[j]
      minDifference = abs(new_V_Reference - V_Reference)
    }
  }
  Sn2 <- rbind(Sn2, Sn_prediction)
}
s<- Sn2[,1]
ss<-s[(endingIndex+1):(endingIndex+values)]
plot(x[1:731], myData$Td[1:731], type="l", xlab = "Номер измерения", ylab = "Значение параметра")
lines(x[501:730], s[501:730], type = "l", col="green")

