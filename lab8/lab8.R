input_table <- read.csv(file = "C:/Users/kosty/OneDrive/Документы/GitHub/Big_Data_R/lab8/Lab8_Data.csv", 
                        sep = ",", header = TRUE, dec = ',')
# Описание
definitions <- read.csv(file = "C:/Users/kosty/OneDrive/Документы/GitHub/Big_Data_R/lab8/Lab8_Definition and Source.csv", 
                        sep = ",", header = TRUE, dec = ',')

# Данные по Индии
india <- subset(input_table, Country.Code == "IND")
# Индия
# Приведение датасета к необходимому формату
years <- c(1989:2017)
ind <- c("GDP", "GDP growth", "Births", "Birth rate", "Adjusted income", "Unemployment advanced", 'Unemployment basic', "Imports of goods", "Industry", "Government health",
         "Life expectancy", "Population", "Government expenditure", "Goods imports", "Exports of goods",
         "Death rate", "Educational total", "Educational female", "High-technology exports", " Best industry", "Scientific articles")

indicators <- india[,3]
india <- india[-c(8,9),-c(1,2,3,4,34)]

# Преобразование в числовой тип
india <- sapply(india, as.numeric)
# Транспонирование
india <- t(india)

rownames(india) <- years
colnames(india) <- ind
india <- data.frame(india)

# График распределения ВВП по годам
plot(years, india[,1], main="Прирост ВВП", xlab='Годы', 
     ylab='ВВП (в долларах)', type='o', pch=20)

# Корреляционная матрица (учитываются все полные наблюдения для каждой пары переменных в отдельности)
cor(india, use="pairwise.complete.obs")



# Визуализация корреляционной матрицы
library(corrplot)

# Ellipses
corrplot(cor(india, use="everything"),mar = c(2, 1, 3, 1), method = "ellipse",tl.cex = 0.6,add = FALSE) 


# Корреляция роста ВВП и прироста населения
cor(india$GDP, india$Population)
plot(india$GDP, india$Population, main='Зависимость популяции от ВВП',
     xlab="Прирост ВВП", ylab="Популяция")


# Корреляция прироста населения на динамику безработицы
cor(india$Population, india$Unemployment.basic, use='pairwise.complete.obs')
plot(india$Population, india$Unemployment.basic, 
     main="Корреляция прироста населения на динамику безработицы",
     xlab="Популяция", ylab="Безработица")

# Корреляция расходов на медицину на увеличение продолжительности жизни
cor(india$Government.health, india$Life.expectancy, use="pairwise.complete.obs")
plot(india$Government.health, india$Life.expectancy, 
     main="Корреляция расходов на медицину на увеличение продолжительности жизни",
     xlab='Расходы на медицину', ylab="Продолжительность жизни")

# Корреляция расходов на медицину на смертность
cor(india$Government.health, india$Death.rate, use="pairwise.complete.obs")
plot(india$Government.health, india$Death.rate,
     main="Корреляция расходов на медицину на смертность",
     xlab="Расходы на медицину", ylab="Смертность")


library(car)
scatterplotMatrix(india[,c(1,4,7,11,12)], spread=FALSE, lty.smooth=4)

model <- lm(india$Life.expectancy ~ india$GDP + I(india$GDP^2), india)
model
summary(model)

plot(india$Life.expectancy ~ india$GDP)
abline(model)

lines(india$GDP, fitted(model))




