#decompose(MyData$Td[1:731],type = "additive") 
#Decompose a time series into seasonal, 
#trend and irregular components using moving averages

#путь к файлам с данными
setwd("~/romcode/courses/r-data-analysis/l02")
MyData<-read.csv("6_1_1_2.csv", header=T, sep=";")
e<-data.frame(y=MyData$Td[1:500], x=x[1:500])

x<-1:731 #изменяем диапазон иксов

# Метод регрессии с использованием многочленов (МНК)
#рисуем график
plot(x[1:731],MyData$Td[1:731], 
     type="l", 
     xlab="номер измерения", 
     ylab="значение параметра")
#подбираем коэффициенты регрессионной модели
res<-nls(y~a+b*x+c*x^2+d*x^3, 
         data=e, 
         start=list(a=0.1,b=0.1,c=0.1,d=0.1))
#получаем значени¤ коэффициентов
coef_func<-coef(res)
#строим регрессионную функцию с найденными коэффициентами
yfunc<-coef_func[1]+
  coef_func[2]*x+
  coef_func[3]*x^2+
  coef_func[4]*x^3
# выводим найденную функцию
lines(x[1:500], 
      yfunc[1:500], 
      type="l",
      col="red")
lines(x[501:731], 
      yfunc[501:731], 
      type="l",
      col="green")

# Метод kNN
#train2<- data.frame(BoilerTP~Y_train)
# библиотека, позволяющая использовать методы машинного обучения
library(caret) 

mod1 <- train(y~x, 
              data=e, 
              method="knn")
res1 <- predict(mod1, 
                x[501:731])

plot(x[1:731], 
     MyData$Td[1:731], 
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

# Метод SVM
#library(caret)
install.packages("kernlab")
#library(kernlab)

mod2 <- train(y~x, 
              data=e, 
              method="svmPoly") #"svmLinear")
res2 <- predict(mod2, 
                x[501:731])

plot(x[1:731], 
     MyData$Td[1:731], 
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


# Метод авторегрессии
library(forecast)

mod3 <- auto.arima(e$y)
res3 <- forecast(mod3, h=230)
plot(res3)
lines(x[501:731], 
      MyData$Td[501:731], 
      type="l", 
      col="black")

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

plot(x[1:731], 
     MyData$Td[1:731], 
     type="l", 
     xlab="номер измерения", 
     ylab="значение параметра")
lines(x[1:500], 
      res4_5$series$y.r[1:500], 
      col="red",
      type="l")
lines(x[1:500], 
      res4_6$series$y.r[1:500], 
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
  V_reference <- fd.estimate(e$y,methods = method, 
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
    new_V_reference <- fd.estimate(new_Sm$e,methods = method, 
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

plot(x[1:731],
     MyData$Td[1:731], 
     type="l", 
     xlab="номер измерения", 
     ylab="значение параметра")
lines(x[501:730], 
      ss, 
      col="green", 
      type="l")
