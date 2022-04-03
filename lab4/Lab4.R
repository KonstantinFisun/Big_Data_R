# Россия + Германия - Акробатика
# Места 1-8
germany_acro <- read.xlsx(file = "C:/Users/kosty/OneDrive/Документы/GitHub/Big_Data_R/lab4/1-8_Ger.xlsx", 
                   sheetIndex = 1)

rownames(germany_acro)<-germany_acro[,1] # Сделали первый столбец именами
olympics <- germany_acro[,1] # Олимпиады

germany_acro<-germany_acro[,-1] #удалили первый столбец

place_1_8 <- germany_acro[,2] # Места с первого по 8

# Значение для восстановления параметров
default <- par(no.readonly = TRUE) # создает копию текущих параметров
par(mar = c(5, 10, 4, 4)) # размеры полей низ лево вверх право

barplot(place_1_8, names.arg = olympics, horiz = TRUE, main="Диаграмма числа 1-8 мест сборной Германии по акробатики",
        xlab="Число мест", las=1)
par(default)

# Выбрали первые места из таблицы

first <- subset(germany_acro, Первые > 0, select = "Первые")

percentage = round(100 * first[,1] / sum(first[,1]), 1)
pieNames <- paste(rownames(first), " (", percentage, "%)", sep="")
par(mar = c(5, 4, 4, 4)) # размеры полей низ лево вверх право

pie(first[,1], pieNames, radius = 1, col=rainbow(6), 
    main="Распределение числа первых мест сборной Германии по акробатики")

# Призовые места мужчин и женщин за все вреия
men_women_acro <- read.xlsx(file = "C:/Users/kosty/OneDrive/Документы/GitHub/Big_Data_R/lab4/Men_Women.xlsx", 
                          sheetIndex = 1)

rownames(men_women_acro)<-men_women_acro[,1] # Сделали первый столбец именами
olympics <- men_women_acro[,1] # Олимпиады
men_women_acro<-men_women_acro[,-1] #удалили первый столбец

years <- substr(olympics,1,4)
plot(years, men_women_acro[,1], type='o', lty=1, ylim=c(0, 12), pch=17, col="blue",
     main="Тенденции изменения количества призовых мест",
     xlab="Год олимпиады", ylab="Число призовых мест")
lines(years, men_women_acro[,2], type='o', lty=1, pch=18, col='red')
legend('topright', c('Мужчины', 'Женщины'), pch=c(17, 18), col=c('blue', 'red'))


# Первые места последних 4 летние олимпиад
firstPlaces <- read.csv(file = "C:/Users/kosty/OneDrive/Документы/GitHub/Big_Data_R/lab4/Medal.csv", 
                        sep = ";", row.names = 1, header = TRUE)

years <- substr(rownames(firstPlaces),1,4)

# График изменения спортивных достижений по золотым медалям 
plot(years, firstPlaces[,1], type='o', lty=1, pch=20, col='brown',
     main='Тенденции изменения количества золотых медалей',
     xlab='Четыре последние летние олимпиады',
     ylab='Число медалей',
     ylim=c(7, 51))
lines(years, firstPlaces[,2], type='o', lty=1, pch=10, col='green')
lines(years, firstPlaces[,3], type='o', lty=1, pch=15, col='red')
lines(years, firstPlaces[,4], type='o', lty=1, pch=17, col='blue')
lines(years, firstPlaces[,5], type='o', lty=1, pch=18, col='black')
lines(years, firstPlaces[,6], type='o', lty=1, pch=12, col='yellow')
lines(years, firstPlaces[,7], type='o', lty=1, pch=13, col='purple')

legend('topright', colnames(firstPlaces),
       pch=c(20,10,15,17,18,12,13), lty=c(1,1,1,1,1,1,1),
       col=c('brown', 'green', 'red', 'blue', 'black', 'yellow', 'purple'),
       y.intersp = 1, text.width = 2)

# 
# Призовые места последние 4 летние олимпиады
prizePlaces <- read.csv(file = "C:/Users/kosty/OneDrive/Документы/GitHub/Big_Data_R/lab4/Prize Places.csv", 
                        sep = ";", row.names = 1, header = TRUE)

# График изменения спортивных достижений по 3-м медалям 
plot(years, prizePlaces[,1], type='o', lty=1, pch=20, col='brown',
     main='Тенденции изменения количества призовых мест',
     xlab='Четыре последние летние олимпиады',
     ylab='Число медалей',
     ylim=c(25, 121))
lines(years, prizePlaces[,2], type='o', lty=1, pch=10, col='green')
lines(years, prizePlaces[,3], type='o', lty=1, pch=15, col='red')
lines(years, prizePlaces[,4], type='o', lty=1, pch=17, col='blue')
lines(years, prizePlaces[,5], type='o', lty=1, pch=18, col='black')
lines(years, prizePlaces[,6], type='o', lty=1, pch=12, col='yellow')
lines(years, prizePlaces[,7], type='o', lty=1, pch=13, col='purple')

legend('topright', c('США', 'Великобритания', "Китай", "Россия", "Германия", "Япония", "Франция"),
       pch=c(20,10,15,17,18,12,13), lty=c(1,1,1,1,1,1,1),
       col=c('brown', 'green', 'red', 'blue', 'black', 'yellow', 'purple'),
       y.intersp = 1, text.width = 2)



# Призовые места мужчин и женщин последние 5 олимпиад по акробатике

с <-read.csv(file = "C:/Users/kosty/OneDrive/Документы/GitHub/Big_Data_R/lab4/summer.csv", 
             sep = ",", header = TRUE)
c <- c[c$year > 2000]
edit(c)
# График
plot(years, menWomen[,1], type='o', lty=1, pch=20, col='red',
     main='Тенденции изменения количества призовых мест по теннису среди мужчин и женщин',
     xlab='Пять последних летних олимпиад',
     ylab='Число медалей',
     ylim=c(1, 5))
lines(years, menWomen[,2], type='o', lty=1, pch=20, col='blue')

legend('topright', c("Мужчины", "Женщины"),
       lty=c(1,1),
       col=c('red', 'blue'),
       y.intersp = 0.2, text.width = 1)

# Столбчатая диаграмма
barplot(data.matrix(menWomen), beside=TRUE,
        col=topo.colors(5),
        main="Призовые места мужчин и женщин по теннису",
        ylab="Число медалей", ylim = c(0, 7))

legend('topright', rownames(menWomen), pch=15,
       col = topo.colors(5),
       y.intersp = 0.4, text.width = 1.5)

# Пирожная диаграмма
default <- par(no.readonly = TRUE)
par(mfrow=c(1,2), mar=c(5,1,2,1))
pie(menWomen[,1], rownames(menWomen), radius = 1.5, col=rainbow(6), 
    main="Распределение числа призовых мест по теннису среди мужчин")

pie(menWomen[,2], rownames(menWomen), radius = 1.5, col=rainbow(6), 
    main="Распределение числа призовых мест по теннису среди женщин")
par(default)
