library(dplyr)

# Считывание олимпийцев
olympics <- read.csv(file = "C:/Users/kosty/OneDrive/Документы/GitHub/Big_Data_R/lab7/athlete_events.csv", 
                     sep = ",", header = TRUE, dec = ',')

# Спортсмены по гимнастике
gymnastics_athlets <- subset(olympics, Sport == 'Gymnastics')


# Удалим строки, где есть пустые значения в весе
gymnastics_athlets[gymnastics_athlets == ""] <- NA # Пустые значение, сделали NA
gymnastics_athlets <- gymnastics_athlets[rowSums(is.na(gymnastics_athlets[,1:6])) == 0,]

# Убрали повторяющиеся строки со спортсменами
gymnastics_athlets <- gymnastics_athlets %>% group_by(ID) %>% filter (! duplicated(ID))

# Вес спортсменов
weight <- as.numeric(gymnastics_athlets$Weight)

# Гистограмма веса
hist(weight, main='Вес спорсменов',
     xlab='Вес', ylab='Частота',xlim = c(20,120))

# Проверка выборки на нормальность распределения с помощью Квантильно-квантильного графика
# (показывает распределение данных относительно ожидаемого нормального распределения)
qqnorm(weight)
qqline(weight, col='red')

install.packages('car')
library(car)
qqPlot(weight)

# Тест Стьюдента
t.test(weight, mu=65)

# Тест Уилкоксона
wilcox.test(weight, mu=65, conf.int = TRUE)

# Тест Шапиро-Уилкса для проверки на нормальность
shapiro.test(weight)
