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

install.packages("party")
library(party)

# Построение модели
# Указываем зависимость групп от каждого параметра
formula <- groups_f ~ Находилось.на.борту+Cмертельные.случаи.на.борту+Убитые.при.приземлении
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
