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

# Будем определять пассажирские или военный самолет по числу на борте

# Будем определять самолеты, на которых больше шанс выжить при крушении

# Стандартизация переменных

airplane_N <- airplan_crushes[,c(10,11)]

maxs <- apply(airplane_N, 2, max) # Максимальное значение
mins <- apply(airplane_N, 2, min) # Минимальное значение

airplane_N <- scale(airplane_N, center = mins, scale = maxs - mins)

# Матрица попарных расстояний 
dist.airplan_crushes <- dist(airplane_N)

# Проводим кластерный анализ, результаты записываем в список clust.iris
clust.airplan_crushes <- hclust(dist.airplan_crushes, "ward.D")

# Построение дендрограммы
plot(clust.airplan_crushes)

labels <- airplan_crushes[,4]

# целесообразно разделить ее на 3 либо на 4 кластера. Попробуем разделить на 4 кластера.
plot(clust.airplan_crushes, labels, cex=0.5)
rect.hclust(clust.airplan_crushes, k=3, border="red")
