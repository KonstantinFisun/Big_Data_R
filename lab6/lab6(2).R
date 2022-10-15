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

# Оставили первые слова из названия самолетов
#
# airplane <-str_extract(airplan_crushes[,7],"^\\w+")
# air <- factor(airplane, labels = c(1:39))
# airplan_crushes <- cbind(airplan_crushes, as.numeric(air))

# Время
time <- gsub(":","",airplan_crushes[,2])
time <- as.numeric(gsub("(^|[^0-9])0+", "\\1", time, perl = TRUE))
airplan_crushes[,2] <- time

# Нормализация переменных
airplane_N <- airplan_crushes[,c(2,10,11,12)]

airplane_N[airplane_N == ""] <- NA # Пустые значение, сделали NA
# Удалим строки, где есть пустые значения
airplane_N <- airplane_N[rowSums(is.na(airplane_N[,])) == 0,]

airplan_crushes[airplan_crushes == ""] <- NA # Пустые значение, сделали NA
# Удалим строки, где есть пустые значения
airplan_crushes <- airplan_crushes[rowSums(is.na(airplan_crushes[,])) == 0,]

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


# Преобразование в фактор
groups_f <- factor(groups)

airplane_N <- data.frame(airplane_N)
airplane_N <- cbind(airplane_N, groups_f)

#-------------------------------------------------------------
# Классификация по формуле Байеса
library(klaR)

# Вычисление вероятностей по всем признакам
naive_airplan_crushes <- NaiveBayes(airplane_N$groups_f ~ ., data = airplane_N)
naive_airplan_crushes$tables

# Ядерные функции плотности условной вероятности
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE)) 
plot(naive_airplan_crushes,legendplot=FALSE)
legend("top", legend=c("g1", "g2", "g3"),col = c("red","green","blue"), lty=1:3, cex=1)

# Классификация по вероятностным данным
predict <- predict(naive_airplan_crushes, airplane_N[,-6])$class

# Соотношение фактического расстояния и прогноза
table(Группа = airplane_N$groups_f, Прогноз = predict)

# Вычисление точности классификации по формуле Байеса
accuracy_bayes <- mean(predict == airplane_N$groups_f)


paste("Точность=", round(100*accuracy_bayes, 2), "%", sep = "")

#----------------------------------------------------------

# Классификация с помощью дерева решений Decision Tree
set.seed(1234)
# Индексирование данных, для 1 вероятность 60%, для 2 - 40%
index <- sample(2, nrow(airplane_N), replace=TRUE, prob=c(0.7, 0.3))

# Разделение на обучающую и тестовую выборки
trainData <- airplane_N[index==1,]
testData <- airplane_N[index==2,]
nrow(trainData) 
nrow(testData)
nrow(airplane_N)

library(party)

# Построение модели
# Указываем зависимость групп от каждого параметра
formula <- groups_f ~ Находилось.на.борту+Cмертельные.случаи.на.борту+Убитые.при.приземлении+Время
airplane_ctree <- ctree(formula, trainData)

# Обучение модели
predict(airplane_ctree)
trainData$groups_f
table(predict(airplane_ctree), trainData$groups_f)
plot(airplane_ctree)

# Применение модели
test_predict <- predict(airplane_ctree, newdata=testData)
table(test_predict, testData$groups_f)
accuracy_tree <- mean(test_predict == testData$groups_f)
accuracy_tree

#------------------------------------------------------------------

# Алгоритм Random Forest
library(randomForest)

# Обучение модели
forest <- randomForest(groups_f ~ .,trainData, ntree=20, proximity=TRUE)
table(predict(forest), trainData$groups_f)


# Применение на тестовой выборке
test_forest <- randomForest(groups_f ~ .,testData, ntree=20, proximity=TRUE)
table(predict(test_forest), testData$groups_f)
accuracy_forest <- mean(predict(test_forest) == testData$groups_f)
accuracy_forest

