myData <- read.csv("dataDtp.csv", header = T, sep = ";")
x = 1:365
e <- data.frame(x=x[1:255], y=myData$Temp[1:255], z=myData$Vict[1:255])
e2 <- data.frame(x=x[2:255], y=myData$Temp[2:255], z=myData$Vict[2:255], z2=myData$Vict[1:254])
plot(x, myData$Temp, type = "l", col = "black", xlab = "День", ylab = "Температура", ylim = c(-30, 50))
lines(x, myData$Vict/10, type="l", col="red", xlab = "День", ylab = "Количество")

#Метод наименьших квадратов
res1_1 <- nls(z~a+b*x+c*x^2+f*y+g*y^2, data = e, start = list(a = 0.1, b = 0.1, c = 0.1, f = 0.1, g = 0.1))
res1_2 <- nls(z~a+b*x+c*x^2+f*y+g*y^2+h*z2+i*z2^2, data = e2, start = list(a = 0.1, b = 0.1, c = 0.1, f = 0.1, g = 0.1, h=0.1, i=0.1))
coef_func_1 <- coef(res1_1)
coef_func_2 <- coef(res1_2)
y <- myData$Temp[1:365]
y2 <- myData$Temp[2:365]
x2 <- 2:365
z2 <- myData$Vict[1:364]
zfunc_1 <- coef_func_1[1] + coef_func_1[2]*x2 + coef_func_1[3]*x2^2 + coef_func_1[4]*y2 + coef_func_1[5]*y2^2
zfunc_2 <- coef_func_2[1] + coef_func_2[2]*x2 + coef_func_2[3]*x2^2 + coef_func_2[4]*y2 + coef_func_2[5]*y2^2 + coef_func_2[6]*z2 + coef_func_2[7]*z2^2
plot(x[1:365],myData$Vict[1:365], type = "l", xlab = "Номер измерения", ylab = "Количество")
lines(x[1:365], zfunc_1[1:365], type = "l", col = "red")
#регрессия
plot(x[2:365],myData$Vict[2:365], type = "l", xlab = "Номер измерения", ylab = "Количество")
lines(x[2:255], zfunc_2[2:255], type = "l", col = "red")
lines(x[256:365], zfunc_2[256:365], type = "l", col = "green")
boxplot(myData$Vict[256:365], zfunc_2[256:365]) #бокс-диаграмма
test0 <- matrix(c(myData$Vict[256:364], zfunc_2[256:364]), nrow = 2, byrow = TRUE) #критерий Пирсона
chisq.test(test0)
var.test(myData$Vict[256:364], zfunc_2[256:364])#Критерий Фишера
t.test(myData$Vict[256:364], zfunc_2[256:364], paired = TRUE) #Критерий Стьюдента

#Метод kNN
library(caret)
mod1<-train (z~x+y+z2, data=e2,method = "knn")
test<-data.frame(y=y2[2:364],x=x2[2:364], z2=z2[2:364])
res1<-predict(mod1, test)
plot (x[1:365], myData$Vict[1:365], type="l", xlab = "Номер", ylab = "Значение")
lines(x[2:255], res1[2:255], type = "l", col = "red")
lines(x[256:365], res1[256:365], type = "l", col = "green")
boxplot(myData$Vict[256:365], res1[256:365])#бокс-диаграмма
test0 <- matrix(c(myData$Vict[265:363], res1[265:363]), nrow = 2, byrow = TRUE) #критерий Пирсона
chisq.test(test0)
var.test(myData$Vict[265:364], res1[265:364])#Критерий Фишера
t.test(myData$Vict[265:364], res1[265:364], paired = TRUE)#Критерий Стьюдента

#Метод опорных векторов SVM
mod2<-train (z~x+y+z2, data=e2, method = "svmPoly")
res2<-predict(mod2, test)
plot (x[2:365], myData$Vict[2:365], type="l", xlab = "Номер", ylab = "Значение")
lines(x[2:255], res2[2:255], type = "l", col = "red")
lines(x[256:365], res2[256:365], type = "l", col = "green")
boxplot(myData$Vict[256:365], res2[256:365])#бокс-диаграмма
test0 <- matrix(c(myData$Vict[265:363], res2[265:363]), nrow = 2, byrow = TRUE) #критерий Пирсона
chisq.test(test0)
var.test(myData$Vict[265:364], res2[265:364])#Критерий Фишера
t.test(myData$Vict[265:364], res2[265:364], paired = TRUE)#Критерий Стьюдента

#Метод авторегрессии
library(forecast)
mod3 <- auto.arima(e$z)
res3 <- forecast(mod3, h=109)
plot(res3)
lines(x[256:364], myData$Vict[256:364], col = "black", type = "l")
boxplot(myData$Vict[256:365], res3$mean[1:109])#бокс-диаграмма
test0 <- matrix(c(myData$Vict[256:365], res3$mean[1:108]), nrow = 2, byrow = TRUE)#критерий Пирсона
chisq.test(test0)
var.test(myData$Vict[256:365], res3$mean[1:108])#Критерий Фишера
t.test(myData$Vict[256:363], res3$mean[1:108], paired = TRUE)#Критерий Стьюдента

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
res4_5 <- reconstruct(mod4, sel.period = 128, show.legend = F)
res4_6 <- reconstruct(mod4, sel.period = 256, show.legend = F)
### ЗДЕСЬ ЗАТЫК
plot(x[1:365], myData$Vict[1:365], type="l", xlab = "Номер", ylab = "Количество")
lines(x[1:255], res4_6$series$y.r[1:255], type = "l", col = "red")
mod4_rec4 = auto.arima(res4_5$series$y.r)
res4_5_1 <- forecast(mod4_rec4, h=109)
lines(x[256:365], res4_5_1$mean[1:109], type = "l", col = "green")
boxplot(myData$Vict[256:365], res4_5_1$mean[1:108]) #box-диаграмма
test0 <- matrix(c(myData$Td[501:731]+100, res4_5_1$mean[1:230]+100), nrow = 2, byrow = TRUE) #критерий Пирсона
chisq.test(test0)
var.test(myData$Td[501:731], res4_5_1$mean[1:230])#Критерий Фишера
t.test(myData$Td[501:731], res4_5_1$mean[1:230], paired = TRUE)#Критерий Стьюдента

#Фрактальный метод
library(quantmod)
library(fractaldim)
values <- 109
endingIndex <- 255
total_error <- 0
error_per_prediction <- c()
method <- "rodogram"
random_sample_count <- values

Sm1 <- as.data.frame(myData$Vict[256:365], row.names = NULL) #test
Sm2 <- as.data.frame(e$z, row.names = NULL) #train

for (i in 1:values) {
  delta <-c()
  for (j in 2:255) {
    delta <- rbind(delta, (e$z[j]-e$z[j-1]))
  }
  Std_delta <- apply (delta, 2, sd)
  V_Reference <- fd.estimate(e$z, method = method, trim = TRUE)$fd
  Sm_guesses <- rnorm (random_sample_count, mean = e$z[255], sd = Std_delta)
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
plot (x[1:365], myData$Vict[1:365], type = "l", xlab = "Номер", ylab = "Количество")
lines(x[256:364], ss, type = "l", col = "green")
boxplot(myData$Vict[256:364], ss) #box диаграмма
test0 <- matrix(c(myData$Vict[256:364], ss[1:109]), nrow = 2, byrow = TRUE) #критерий Пирсона
chisq.test(test0)
var.test(myData$Vict[256:364], ss)#Критерий Фишера
t.test(myData$Vict[256:364], ss, paired = TRUE)#Критерий Стьюдента
