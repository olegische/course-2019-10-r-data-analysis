#путь
setwd("D:/Study/R/Labs")
#читаем файл
myData<- read.csv("6_1_1_2.csv", header = T, sep = ";")
x=1:400
View(myData)
plot(myData$Td[1:400], type = "l", xlab="Номер измерения", ylab = "Значение параметра")
#выделяем набор данных, для которого будем строить регрессионную модель
e<-data.frame(y=myData$Td[1:400], x=1:400)
#подбираем коэффициенты регрессионной модели
res<- nls(y~a+b*x+c*x*x, data=e, start=list(a=0.1, b=0.1, c=0.1))
#получаем значения коэффициентов
coef_func <- coef(res)
#строим регрессивную функцию
yfunc<-coef_func[1]+ coef_func[2]*x + coef_func[3]*x*x
#выводим
lines(x, yfunc, type="l", col="red")