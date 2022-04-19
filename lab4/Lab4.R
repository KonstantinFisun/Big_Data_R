library("xlsx")

# Германия - Акробатика
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

barplot(place_1_8, names.arg = olympics, horiz = TRUE, main="Диаграмма числа 1-8 мест сборной Германии по акробатики",cex.axis = 2,
        xlab="Число мест", las=1, col=rainbow(6))
par(default)

# Выбрали первые места из таблицы

first <- subset(germany_acro, Первые > 0, select = "Первые")

percentage = round(100 * first[,1] / sum(first[,1]), 1)
pieNames <- paste(rownames(first), " (", percentage, "%)", sep="")
par(mar = c(5, 4, 4, 4)) # размеры полей низ лево вверх право

pie(first[,1], pieNames, radius = 1, col=rainbow(6), cex = 1.4,
    main="Распределение числа первых мест сборной Германии по акробатики")

# Призовые места мужчин и женщин за все вреия
men_women_acro <- read.xlsx(file = "C:/Users/kosty/OneDrive/Документы/GitHub/Big_Data_R/lab4/Men_Women.xlsx", 
                          sheetIndex = 1)

rownames(men_women_acro)<-men_women_acro[,1] # Сделали первый столбец именами
olympics <- men_women_acro[,1] # Олимпиады
men_women_acro<-men_women_acro[,-1] #удалили первый столбец

years <- substr(olympics,1,4)
plot(years, men_women_acro[,1], type='o', lty=1, ylim=c(0, 12), pch=17, col="blue",cex = 3,
     main="Тенденции изменения количества призовых мест",
     xlab="Год олимпиады", ylab="Число призовых мест")
lines(years, men_women_acro[,2], type='o', lty=1, pch=18, col='red')
legend('topright', c('Мужчины', 'Женщины'), pch=c(17, 18), col=c('blue', 'red'))


# Первые места последних 4 летние олимпиад
firstPlaces <- read.csv(file = "C:/Users/kosty/OneDrive/Документы/GitHub/Big_Data_R/lab4/Medal.csv", 
                        sep = ";", row.names = 1, header = TRUE)

years <- substr(rownames(firstPlaces),1,4)

# График изменения спортивных достижений по золотым медалям 
plot(years, firstPlaces[,1], type='o', lty=1, pch=20, col='brown',cex = 3,
     main='Тенденции изменения количества золотых медалей',
     xlab='Четыре последние летние олимпиады',
     ylab='Число медалей',
     ylim=c(7, 51))
lines(years, firstPlaces[,2], type='o', lty=1, pch=10, col='green',cex = 3,)
lines(years, firstPlaces[,3], type='o', lty=1, pch=15, col='red',cex = 3,)
lines(years, firstPlaces[,4], type='o', lty=1, pch=17, col='blue',cex = 3,)
lines(years, firstPlaces[,5], type='o', lty=1, pch=18, col='black',cex = 3,)
lines(years, firstPlaces[,6], type='o', lty=1, pch=12, col='yellow',cex = 3,)
lines(years, firstPlaces[,7], type='o', lty=1, pch=13, col='purple',cex = 3,)

legend('topright', colnames(firstPlaces),
       pch=c(20,10,15,17,18,12,13), lty=c(1,1,1,1,1,1,1),
       col=c('brown', 'green', 'red', 'blue', 'black', 'yellow', 'purple'),
       y.intersp = 0.6, text.width = 2)

# 
# Призовые места последние 4 летние олимпиады
prizePlaces <- read.csv(file = "C:/Users/kosty/OneDrive/Документы/GitHub/Big_Data_R/lab4/Prize Places.csv", 
                        sep = ";", row.names = 1, header = TRUE)

# График изменения спортивных достижений по 3-м медалям 
plot(years, prizePlaces[,1], type='o', lty=1, pch=20, col='brown', cex = 3,
     main='Тенденции изменения количества призовых мест(1-3)',
     xlab='Четыре последние летние олимпиады',
     ylab='Число медалей',
     ylim=c(25, 121))
lines(years, prizePlaces[,2], type='o', lty=1, pch=10, col='green',cex = 3,)
lines(years, prizePlaces[,3], type='o', lty=1, pch=15, col='red',cex = 3)
lines(years, prizePlaces[,4], type='o', lty=1, pch=17, col='blue',cex = 3)
lines(years, prizePlaces[,5], type='o', lty=1, pch=18, col='black',cex = 3)
lines(years, prizePlaces[,6], type='o', lty=1, pch=12, col='yellow',cex = 3)
lines(years, prizePlaces[,7], type='o', lty=1, pch=13, col='purple',cex = 3)

legend('topright', c('США', 'Великобритания', "Китай", "Россия", "Германия", "Япония", "Франция"),
       pch=c(20,10,15,17,18,12,13), lty=c(1,1,1,1,1,1,1),
       col=c('brown', 'green', 'red', 'blue', 'black', 'yellow', 'purple'),
       y.intersp = 0.8, text.width = 2)



# Призовые места мужчин и женщин последние 5 олимпиад по акробатике

# Призовые места мужчин и женщин за все вреия
men_women_acro <- read.xlsx(file = "C:/Users/kosty/OneDrive/Документы/GitHub/Big_Data_R/lab4/Men_Women.xlsx", 
                            sheetIndex = 1)

rownames(men_women_acro)<-men_women_acro[,1] # Сделали первый столбец именами

men <- subset(men_women_acro, substr(men_women_acro[,1],1,4) > 1998 , select = "Мужчины") # Выбрали мужчин по последним 6 олимпиадам по акробатике

women <- subset(men_women_acro, substr(men_women_acro[,1],1,4) > 1998 , select = "Женщины") # Выбрали женщин по последним 6 олимпиадам по акробатике

men_women_acro<-men_women_acro[,-1] #удалили первый столбец

# Столбчатая диаграмма по мужчинам
barplot(men[,1], 
        col=rainbow(6),
        main="Призовые места мужчин за последнии 6 олимпиад по акробатике",
        ylab="Число медалей",
        xlab = "Олимпиады",
        names.arg = rownames(men), cex.axis = 2, cex.names = 0.8)

# Столбчатая диаграмма по женщинам
barplot(women[,1], 
        col=rainbow(6),
        main="Призовые места мужчин за последнии 6 олимпиад по акробатике",
        ylab="Число медалей",
        xlab = "Олимпиады",
        names.arg = rownames(women), cex.axis = 2, cex.names = 0.8)



