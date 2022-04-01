install.packages("xlsx", dep = T)
library("xlsx")

food <- read.xlsx("C:/Users/kosty/OneDrive/Рабочий стол/3 курс 6 семест/Обработка больших данных/Practical R/lab2/food.xlsx", sheetIndex = 1)
rownames(food)<-food[,1]
name <- food[,1]

food<-food[,-1] #удаляем имена
food_name<- colnames(food)
colMax <- function(data) sapply(data, max, na.rm = TRUE) # Функция макс в каждом столбце
colMin <- function(data) sapply(data, min, na.rm = TRUE) # Функция мин в каждом столбце
colMean <- function(data) sapply(data, mean, na.rm = TRUE) # Функция среднего в каждом столбце

# Функция моды
getmode <- function(vector) {
  uniqv <- unique(vector) # Находим уникальные элементы вектора
  uniqv[which.max(tabulate(match(vector, uniqv)))] # Каждому элементу ставим соответствие его повторений
}


colMax(food) # Максимальная оценка
colMin(food) # Минимальная оценка
colMean(food) # Средняя оценка



count_score_high <- vector()

for(i in 1:ncol(food)){
  
  count_score_high[i] <- sum(food[,i] > 7)
}

count_score_high

rbind(food_name, count_score_high) # количество людей, отдавших предпочтение >7


count_score_low <- vector()

for(i in 1:ncol(food)){
  count_score_low[i] <- sum(food[,i] < 3)
}

count_score_low

rbind(food_name, count_score_low) # количество людей, отдавших предпочтение <3

sort(colMean(food),decreasing = TRUE) # рейтинг


# столбчатая диаграмма оценок 
barplot(height = colMean(food),col = "steelblue",
        xlab = "Еда",
        ylab = "Средняя оценка"
)

# Гистограмма средних оценок
hist(colMean(food))

boxplot(colMean(food),
        main = "Среднии оценки",
        ylab = "Средняя оценка",
        col=rainbow(10))


boxplot(food[,c(1:10)],
        main = "Оценки еды",
        xlab = "Блюда",
        ylab = "Оценка",
        col = rainbow(10))

# Гистограмма количество оценок
hist(data.matrix(na.omit(food)),
     breaks = 10,
     main = "Гистограмма",
     xlab = "Оценка",
     ylab = "Количество",
     col = rainbow(10))

# Импорт из csv
mdf <- read.table('food.csv', header=TRUE,  sep=";", row.names = "Name")

# Дескриптивный анализ
data <- sapply(mdf, summary, na.rm = TRUE)
sd <- sapply(mdf,sd,na.rm = TRUE) # Стандартное отклонение
var <- sapply(mdf,var,na.rm = TRUE) # Дисперсия
IQR <- sapply(mdf, IQR, na.rm = TRUE) # Межквартильный размах
mode <- sapply(mdf, getmode) # Мода
rbind(data, sd, var, IQR, mode)

# Сортировка по 3 столбцам
mdf[order(mdf$Cezar,mdf$Kotleta.s.pure,mdf$Makaroni),]

# Сформировать отдельные наборы данных по одинаковому признаку
# Оценка за цезарь больше 9
newdata1 <- subset(mdf, Cezar>=9)

dim(newdata1) # размерность

hist(data.matrix(na.omit(newdata1)),
     breaks = 10,
     main = "Гистограмма",
     xlab = "Оценка",
     ylab = "Количество",
     col = rainbow(10))

boxplot(newdata1[,c(1:10)],
        main = "Оценки еды",
        xlab = "Блюда",
        ylab = "Оценка",
        col = rainbow(10))


# Оценка за Котлюту и Оливье 
newdata2 <- subset(mdf, Kotleta.s.pure > 7 & Olive > 7)

dim(newdata2) # размерность

hist(data.matrix(na.omit(newdata2)),
     breaks = 10,
     main = "Гистограмма",
     xlab = "Оценка",
     ylab = "Количество",
     col = rainbow(10))

boxplot(newdata2[,c(1:10)],
        main = "Оценки еды",
        xlab = "Блюда",
        ylab = "Оценка",
        col = rainbow(10))

# Оценка за Бургер <= 6, Pizza >= 8, Makaroni < 8
newdata3 <- subset(mdf, Burger <= 6 & Pizza >= 8 & Makaroni < 8)

dim(newdata3) # размерность

hist(data.matrix(na.omit(newdata3)),
     breaks = 10,
     main = "Гистограмма",
     xlab = "Оценка",
     ylab = "Количество",
     col = rainbow(10))

boxplot(newdata3[,c(1:10)],
        main = "Оценки еды",
        xlab = "Блюда",
        ylab = "Оценка",
        col = rainbow(10))



  

