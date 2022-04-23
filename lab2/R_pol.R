x<-c(174,162,188,192,165,168,172) #данные о росте семи сотрудников небольшой компании
str(x) # Функция str() выводит данные об объектах
pol<-c("male","female","male","male","female","male","male") #Формируем вектор "пол" для сотрудников фирмы
is.character(pol)
is.factor(pol)
is.vector(pol)
str(pol)
table(pol)
pol.f<-factor(pol)
is.factor(pol.f)
pol.f
plot(pol.f)
#Вектор веса
w<-c(69,68,93,87,59,82,72)
#Построение графика
plot(x,w,pch=as.numeric(pol.f),col=as.numeric(pol.f)) 
legend("topleft",pch=1:2,col=1:2,legend=levels(pol.f))
plot(x,w,pch=(7:8), col=c("magenta","green"))
legend("topleft",pch=7:8, col=c("magenta","green"),legend=levels(pol.f))


h <- с(8, 10, NA, NA, 8, NA, 8)
mean(h)
