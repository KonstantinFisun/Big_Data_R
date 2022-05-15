library("xlsx")

airplan_crushes <- read.csv('C:/Users/kosty/OneDrive/Документы/GitHub/Big_Data_R/lab6/Airplane_Crashes.csv', sep = ",") # Считали базу данных

names_cols_airplan_crushes <- c("Дата", "Время", "Место крушения", "Компания",
               "Полет", "Рейс", "Самолет", "Бортовой номер",
               "Заводской серийный номер", "Находилось на борту", 
               "Cмертельные случаи на борту", "Убитые при приземлении",
               "Описание крушения") # Название столбцов

colnames(airplan_crushes) <- names_cols_airplan_crushes # Присвоили имена столбикам

airplan_crushes[airplan_crushes == ""] <- NA # Пустые значение, сделали NA


# Удалим строки, где есть пустые значения
airplan_crushes <- airplan_crushes[rowSums(is.na(airplan_crushes[,])) == 0,]

# Оставили 100 рейсов
airplan_crushes <- airplan_crushes[800:944,]

# Стандартизация переменных
airplane_N <- airplan_crushes[,c(10,11,12)]

maxs <- apply(airplane_N, 2, max) # Максимальное значение
mins <- apply(airplane_N, 2, min) # Минимальное значение

airplane_N <- scale(airplane_N, center = mins, scale = maxs - mins)

# Матрица попарных расстояний 
dist.airplan_crushes <- dist(airplane_N)

# Проводим кластерный анализ, результаты записываем в список clust.airplan_crushes
clust.airplan_crushes <- hclust(dist.airplan_crushes, "ward.D")

labels <- airplan_crushes[,7]

# Дендрограмма
plot(clust.airplan_crushes, labels, cex=0.5, main="Дендрограмма кластеров",
     xlab="Самолеты")

# целесообразно разделить ее на 3 кластера.
rect.hclust(clust.airplan_crushes, k=3, border="red")

# Разбиение дендрограммы на кластеры
groups <- cutree(clust.airplan_crushes, k = 3)

airplan_crushes[groups==1, 7]
airplan_crushes[groups==2, 7]
airplan_crushes[groups==3, 7]

par(mar = c(4, 5, 6, 4))
# Вычисляем среднее значение показателей в каждом кластере
#  в 1-ом кластере
g1<-colMeans(airplan_crushes[groups==1,10:12])
#  во 2-ом кластере
g2<-colMeans(airplan_crushes[groups==2,10:12])
#  в 3-ем кластере
g3<-colMeans(airplan_crushes[groups==3,10:12])

# Построение столбчатой диаграммы
df <- data.frame(g1,g2,g3)
rownames(df) <- names_cols_airplan_crushes[10:12]

barplot(data.matrix(df), main="Группы самолетов", col=rainbow(3), ylim = c(0,150), beside = TRUE)
legend("topright", names_cols_airplan_crushes[10:12], col=rainbow(3), lwd=5, bty = "n",  y.intersp = 0.8, text.width = 6)

# Каменная осыпь
plot(1:144, clust.airplan_crushes$height, type='b', main = "Диаграмма каменная осыпь") 

