myData <- read.csv("Task01.csv", header = T, sep = ";")
x = 1:731
e <- data.frame(y=myData$Td[1:500], x=x[1:500], z=myData$Tpr[1:500])
e2 <- data.frame(y=myData$Td[2:500], x=x[2:500], z=myData$Tpr[2:500], z2=myData$Tpr[1:499])
plot(x[1:731],myData$Td[1:731], type = "l", xlab = "Номер измерения", ylab = "Значение", ylim = c(-30, 145))
lines(x[1:731], myData$Tpr[1:731], type = "l", col = "red")


#МНК
plot(x[1:731],myData$Tpr[1:731], type = "l", xlab = "Номер измерения", ylab = "Значение", ylim = c(60, 145))
res <- nls(z~a+b*x+c*x^2+f*y+g*y^2+h*z2+i*z2^2, data = e2, start = list(a = 0.1, b = 0.1, c = 0.1, f = 0.1, g = 0.1, h=0.1, i=0.1))
coef_func <- coef(res)
y <- myData$Td[1:731]
x2 <- 2:731
y2 <- myData$Td[2:731]
z2 <- myData$Tpr[1:730]
zfunc <- coef_func[1] + coef_func[2]*x2 + coef_func[3]*x2^2 + coef_func[4]*y2 + coef_func[5]*y2^2 + coef_func[6]*z2 + coef_func[7]*z2^2
plot(x[2:731],myData$Tpr[2:731], type = "l", xlab = "Номер измерения", ylab = "Значение", ylim = c(65, 145))
lines(x[2:500], zfunc[2:500], type = "l", col = "red")
lines(x[501:730], zfunc[501:730], type = "l", col = "green")
#Использование Box-диаграммы для сопоставления текстовой выборки и прогноза
boxplot(myData$Tpr[501:730], zfunc[501:730])
#Критерий Хи-квадрат
test0 <- matrix(c(myData$Tpr[501:730], zfunc[501:730]), nrow = 2, byrow = TRUE)
chisq.test(test0)
#Критерий Фишера
var.test(myData$Tpr[501:731], zfunc[501:731])
#Критерий Стьюдента
t.test(myData$Tpr[501:731], zfunc[501:731], paired = TRUE)



#KNN
library(caret)
mod1<-train (z~x+y+z2, data=e2,method = "knn")
test<-data.frame(y=y2[2:730],x=x2[2:730], z2=z2[2:730])
res1<-predict(mod1, test)
plot (x[2:731], myData$Tpr[2:731], type="l", xlab = "Номер", ylab = "Значение")
lines(x[2:500], res1[2:500], type = "l", col = "red")
lines(x[501:731], res1[501:731], type = "l", col = "green")
#Использование Box-диаграммы для сопоставления текстовой выборки и прогноза
boxplot(myData$Tpr[501:731], res1[501:731])
#Критерий Хи-квадрат
test0 <- matrix(c(myData$Tpr[501:730], res1[501:730]), nrow = 2, byrow = TRUE)
chisq.test(test0)
#Критерий Фишера
var.test(myData$Tpr[501:730], res1[501:730])
#Критерий Стьюдента
t.test(myData$Tpr[501:730], res1[501:730], paired = TRUE)


#SVM
mod2<-train (z~x+y+z2, data=e2, method = "svmPoly") # = "svmLinear" - для линейной функции, Poly для полигональной
res2<-predict(mod2, test)
plot (x[2:731], myData$Tpr[2:731], type="l", xlab = "Номер", ylab = "Значение")
lines(x[2:500], res2[2:500], type = "l", col = "red")
lines(x[501:730], res2[501:730], type = "l", col = "green")
#Использование Box-диаграммы для сопоставления текстовой выборки и прогноза
boxplot(myData$Tpr[501:731], res2[501:731])
#Критерий Хи-квадрат
test0 <- matrix(c(myData$Tpr[501:731], res2[501:731]), nrow = 2, byrow = TRUE)
chisq.test(test0)
#Критерий Фишера
var.test(myData$Tpr[501:731], res2[501:731])
#Критерий Стьюдента
t.test(myData$Tpr[501:731], res2[501:731], paired = TRUE)

#(ЗДЕСЬ УЖЕ БЕЗ РЕГРЕССИИ)

#Метод авторегрессии
library(forecast)
mod3 <- auto.arima(e$y)
res3 <- forecast(mod3, h=230)
plot(res3)
lines(x[501:731], myData$Td[501:731], col = "black", type = "l")
#Использование Box-диаграммы для сопоставления текстовой выборки и прогноза
boxplot(myData$Td[501:731], res3$mean[1:230])
#Критерий Хи-квадрат
test0 <- matrix(c(myData$Td[501:731]+100, res3$mean[1:230]+100), nrow = 2, byrow = TRUE)
chisq.test(test0)
#Критерий Фишера
var.test(myData$Td[501:731], res3$mean[1:230])
#Критерий Стьюдента
t.test(myData$Td[501:731], res3$mean[1:230], paired = TRUE)




#Метод вейвлет анализа
library(WaveletComp)
library(biwavelet)
mod4 <- analyze.wavelet(e, my.series = 1,
                        loess.span = 0,
                        dt = 1, dj = 1/298,
                        lowerPeriod = 1/4, upperPeriod = 256,
                        make.pval = "F", n.sim = 10)
wt.image(mod4, n.levels = 337, legend.params = list(lab = "Степень влияния"))

res4_1 <- reconstruct(mod4, sel.period = 3, show.legend = F)
res4_2 <- reconstruct(mod4, sel.period = 6, show.legend = F)
res4_3 <- reconstruct(mod4, sel.period = 16, show.legend = F)
res4_4 <- reconstruct(mod4, sel.period = 64, show.legend = F)
res4_5 <- reconstruct(mod4, sel.period = 256, show.legend = F)
#В ДЗ НАДО ВЗЯТЬ СРЕДНЕЕ АРИФМЕТИЧЕСКОЕ ДВУХ ПОСЛЕДНИХ ГАРМОНИК

plot(x[1:731], myData$Td[1:731], type="l", xlab = "Номер", ylab = "Значение")
lines(x[1:500], res4_5$series$y.r[1:500], type = "l", col = "red")
mod4_rec4 = auto.arima(res4_5$series$y.r)
res4_5_1 <- forecast(mod4_rec4, h=230)
lines(x[501:730], res4_5_1$mean[1:230], type = "l", col = "green")

#Использование Box-диаграммы для сопоставления текстовой выборки и прогноза
boxplot(myData$Td[501:731], res4_5_1$mean[1:230])
#Критерий Хи-квадрат
test0 <- matrix(c(myData$Td[501:731]+100, res4_5_1$mean[1:230]+100), nrow = 2, byrow = TRUE)
chisq.test(test0)
#Критерий Фишера
var.test(myData$Td[501:731], res4_5_1$mean[1:230])
#Критерий Стьюдента
t.test(myData$Td[501:731], res4_5_1$mean[1:230], paired = TRUE)




#Фрактальный метод
library(quantmod)
library(fractaldim)
values <- 230
endingIndex <- 500
total_error <- 0
error_per_prediction <- c()
method <- "rodogram"
random_sample_count <- values

Sm1 <- as.data.frame(myData$Td[501:731], row.names = NULL) #test
Sm2 <- as.data.frame(e$y, row.names = NULL) #train

for (i in 1:values) {
  delta <-c()
  for (j in 2:500) {
    delta <- rbind(delta, (e$y[j]-e$y[j-1]))
  }
  Std_delta <- apply (delta, 2, sd)
  V_Reference <- fd.estimate(e$y, method = method, trim = TRUE)$fd
  Sm_guesses <- rnorm (random_sample_count, mean = e$y[500], sd = Std_delta)
  minDifference = 1000000
  for (j in 1:length(Sm_guesses)){
    new_Sm <- rbind(Sm2, Sm_guesses[j])
    new_V_Reference <- fd.estimate(new_Sm$e, method = method, trim = TRUE)$fd
    if (abs(new_V_Reference - V_Reference) < minDifference) {
      Sm_prediction <- Sm_guesses[j]
      minDifference = abs(new_V_Reference - V_Reference)
    }
  }
  Sm2 <- rbind(Sm2, Sm_prediction)
}
s <- Sm2 [,1]
ss <- s[(endingIndex+1):(endingIndex+values)]
plot (x[1:731], myData$Td[1:731], type = "l", xlab = "Номер", ylab = "Значение")
lines(x[501:730], ss, type = "l", col = "green")
#Использование Box-диаграммы для сопоставления текстовой выборки и прогноза
boxplot(myData$Td[501:731], ss)
#Критерий Хи-квадрат
test0 <- matrix(c(myData$Td[501:730]+100, ss[1:230]+100), nrow = 2, byrow = TRUE)
chisq.test(test0)
#Критерий Фишера
var.test(myData$Td[501:731], ss)
#Критерий Стьюдента
t.test(myData$Td[501:730], ss, paired = TRUE)
