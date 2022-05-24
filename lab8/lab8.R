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
ind <- c("ВВП", "ВВП рост", "Рождаемость", "Динамика рож.", "Заемщики", "Безраб(выс)", 'Безработные(баз)', "Импорт", "Промышленность", "Расходы на мед.",
         "Продол. жизни", "Прирост популяции", "Расходы на образ", "Объем импорта", "Экспорт",
         "Смертностьь", "Образованных", "Образованных(ж)", "Экспорт High-tech", "Лучшая индустр", "Науч статей")

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
corrplot(cor(india, use="everything"),mar = c(0, 0, 0, 0), method = "ellipse",tl.cex = 0.6,add = FALSE) 


# Корреляция роста ВВП и прироста населения
cor(india$ВВП, india$Прирост.популяции)
plot(india$ВВП, india$Прирост.популяции, main='Зависимость популяции от ВВП',
     xlab="Прирост ВВП", ylab="Популяция")


# Корреляция прироста населения на динамику безработицы
cor(india$Прирост.популяции, india$Безработные.баз., use='pairwise.complete.obs')
plot(india$Прирост.популяции, india$Безработные.баз., 
     main="Корреляция прироста населения на динамику безработицы",
     xlab="Популяция", ylab="Безработица")

# Корреляция расходов на медицину на увеличение продолжительности жизни
cor(india$Расходы.на.мед., india$Продол..жизни, use="pairwise.complete.obs")
plot(india$Расходы.на.мед., india$Продол..жизни, 
     main="Корреляция расходов на медицину на увеличение продолжительности жизни",
     xlab='Расходы на медицину', ylab="Продолжительность жизни")

# Корреляция расходов на медицину на смертность
cor(india$Расходы.на.мед., india$Смертность, use="pairwise.complete.obs")
plot(india$Расходы.на.мед., india$Смертность,
     main="Корреляция расходов на медицину на смертность",
     xlab="Расходы на медицину", ylab="Смертность")


library(car)
scatterplotMatrix(india[,c(1,4,8,11,12)], spread=FALSE, lty.smooth=4)

# Полиномиальная регрессионная модель зависимости продолжительности жизни от ВВП
model <- lm(india$Продол..жизни ~ india$ВВП + I(india$ВВП^2), india)
model
summary(model)

plot(india$Продол..жизни ~ india$ВВП)

#График полиномиальной регрессии
lines(india$ВВП, fitted(model))




