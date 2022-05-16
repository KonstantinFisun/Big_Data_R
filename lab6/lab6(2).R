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


airplan_crushes <- airplan_crushes[800:944,]

rownames(airplan_crushes) <- airplan_crushes[,8]

airplane_N <- airplan_crushes[,c(10,11,12)]

maxs <- apply(airplane_N, 2, max) # Максимальное значение
mins <- apply(airplane_N, 2, min) # Минимальное значение

airplane_N <- scale(airplane_N, center = mins, scale = maxs - mins)


# Матрица попарных расстояний 
dist.airplan_crushes <- dist(airplane_N)

# Проводим кластерный анализ, результаты записываем в список clust.airplan_crushes
clust.airplan_crushes <- hclust(dist.airplan_crushes, "ward.D")

# целесообразно разделить ее на 3 кластера.
rect.hclust(clust.airplan_crushes, k=3, border="red")

# Разбиение дендрограммы на кластеры
groups <- cutree(clust.airplan_crushes, k = 3)

# Преобразование в фактор
groups_f <- factor(groups)


airplane_N <- cbind(airplane_N, groups_f)
#-------------------------------------------------------------
# Классификация по формуле Байеса
library(klaR)
airplane_N <- data.frame(airplane_N[,-4])
# Вычисление вероятностей по всем признакам
naive_airplan_crushes <- NaiveBayes(airplane_N$groups_f ~ ., airplane_N)
naive_airplan_crushes$tables

# Ядерные функции плотности условной вероятности
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE)) 
plot(naive_airplan_crushes)


# Классификация по вероятностным данным
predict <- predict(naive_airplan_crushes, airplane_N[,-4])$class

# Соотношение фактического расстояния и прогноза
table(Группа = airplane_N$groups_f, Прогноз = predict)

# Вычисление точности классификации по формуле Байеса
accuracy_bayes <- mean(predict == airplane_N$groups_f)
accuracy_bayes

paste("Точность=", round(100*accuracy_bayes, 2), "%", sep = "")

#----------------------------------------------------------
