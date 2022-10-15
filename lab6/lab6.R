library("xlsx")
library(stringr)

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

airplan_crushes <- airplan_crushes[800:944,]

# Оставили первые слова из названия самолетов
#
# airplane <-str_extract(airplan_crushes[,7],"^\\w+")
# air <- factor(airplane, labels = c(1:39))
# airplan_crushes <- cbind(airplan_crushes, as.numeric(air))

# Время
time <- gsub(":","",airplan_crushes[,2])
time <- as.numeric(gsub("(^|[^0-9])0+", "\\1", time, perl = TRUE))
airplan_crushes[,2] <- time

airplan_crushes[airplan_crushes == ""] <- NA # Пустые значение, сделали NA


# Удалим строки, где есть пустые значения
airplan_crushes <- airplan_crushes[rowSums(is.na(airplan_crushes[,])) == 0,]

# Нормализация переменных
airplane_N <- airplan_crushes[,c(2,10,11,12)]

maxs <- apply(airplane_N, 2, max) # Максимальное значение
mins <- apply(airplane_N, 2, min) # Минимальное значение

airplane_N[airplane_N == ""] <- NA # Пустые значение, сделали NA


# Удалим строки, где есть пустые значения
airplane_N <- airplane_N[rowSums(is.na(airplane_N[,])) == 0,]

airplane_N <- scale(airplane_N, center = mins, scale = maxs - mins)

airplane_N

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


#par(mar = c(4, 5, 6, 4))
# Вычисляем среднее значение показателей в каждом кластере
#  в 1-ом кластере
g1<-colMeans(airplan_crushes[groups==1,c(2,10,11,12)])
#  во 2-ом кластере
g2<-colMeans(airplan_crushes[groups==2,c(2,10,11,12)])
#  в 3-ем кластере
g3<-colMeans(airplan_crushes[groups==3,c(2,10,11,12)])


# Построение столбчатой диаграммы
df <- data.frame(g1,g2,g3)
rownames(df) <- c("Время", "Количество на борту", "Умерло на борту", "Умерло не на борту")
# colnames(df) <- c("Количество людей на борту больше 20", "Количество людей на борту до 20", "Большое количество жертв при приземление")

barplot(data.matrix(df), main="Группы самолетов", col=rainbow(4), ylim = c(0,2400), beside = TRUE)
legend("topright", c("Время", "Количество на борту", "Умерло на борту", "Умерло не на борту"),
       col=rainbow(4), lwd=5, bty = "n",  y.intersp = 0.8, text.width = 6)

# Каменная осыпь
plot(1:143, clust.airplan_crushes$height, type='b', main = "Диаграмма каменная осыпь") 

library(lattice)

# Двумерные диаграммы рассеяния
xyplot(airplan_crushes[,10] ~ airplan_crushes[,11], airplan_crushes, main='Зависимость количества людей на борту от летальных случаев на борту',
       xlab='Смертельные случаи на борту', ylab='Находились на борту',  auto.key = TRUE, groups = groups)

# Построим боксплот
boxplot(airplan_crushes[,2] ~ groups, data = iris, ylab = "Время крушения", frame = FALSE, col = "lightgray")

library("scatterplot3d")
colors <- c("#999999", "#E69F00", "#56B4E9")
colors <- colors[as.numeric(groups)]

scatterplot3d(airplan_crushes[,c(2,10,11)], pch = 16, color=colors)
legend("top", legend = c("Утреннее крушение ", "Пассажирские", "Вечернее крушение"),
       col =  c("#999999", "#E69F00", "#56B4E9"), 
       pch = c(16, 17, 18))

