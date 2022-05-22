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

# Выбрали мужчин
gymnastics_athlets_man <- gymnastics_athlets[gymnastics_athlets$Sex == 'M',]

# Вес спортсменов
weight <- as.numeric(gymnastics_athlets_man$Weight)

# Гистограмма веса
hist(weight, main='Вес спорсменов',
     xlab='Вес', ylab='Частота',xlim = c(40,90))

# Проверка выборки на нормальность распределения с помощью Квантильно-квантильного графика
# (показывает распределение данных относительно ожидаемого нормального распределения)
qqnorm(weight)
qqline(weight, col='blue', lwd = 5)

# Доверительные интервалы
library(car)
qqPlot(weight)

# Тест Стьюдента
t.test(weight, mu=65)

# Тест Уилкоксона
wilcox.test(weight, mu=65, conf.int = TRUE)

# Тест Шапиро-Уилкса для проверки на нормальность
shapiro.test(weight)

#-------------------------------------------------------------------------------

# Выборка гимнастов и атлетов
gymnastics_athletics_men <- subset(olympics, Sex == 'M' & (Sport == 'Gymnastics' | Sport == 'Athletics'))

# Удалим строки, где есть пустые значения в весе
gymnastics_athletics_men[gymnastics_athletics_men == ""] <- NA # Пустые значение, сделали NA
gymnastics_athletics_men <- gymnastics_athletics_men[rowSums(is.na(gymnastics_athletics_men[,1:6])) == 0,]

# Убрали повторяющиеся строки со спортсменами
gymnastics_athletics_men <- gymnastics_athletics_men %>% group_by(ID) %>% filter (! duplicated(ID))

weight_gymnastics_athletics_men <- as.numeric(gymnastics_athletics_men$Weight)

# Гистограмма веса
hist(weight_gymnastics_athletics_men, main='Гистограмма веса гимнастов и атлетов', xlab='Вес', xlim = c(40,150), ylim = c(0,5000))

# Проверка на нормальность
qqPlot(weight)

# Выбрали вес по отдельности
gymnastics_weight <- as.numeric(subset(gymnastics_athletics_men, Sport == 'Gymnastics')$Weight)
athletics_weight <- as.numeric(subset(gymnastics_athletics_men, Sport == 'Athletics')$Weight)

# Среднее
mean(gymnastics_weight, na.rm=TRUE)
mean(athletics_weight, na.rm = TRUE)

# Тест на равенство дисперсий
bartlett.test(as.numeric(gymnastics_athletics_men$Weight) ~ gymnastics_athletics_men$Sport, data=gymnastics_athletics_men)

# Проверка, различаются ли выбранные средние значения с помощью теста Уэлча
t.test(as.numeric(gymnastics_athletics_men$Weight) ~ gymnastics_athletics_men$Sport)

# Проверка, при условии, что дисперсии равны
t.test(as.numeric(gymnastics_athletics_men$Weight) ~ gymnastics_athletics_men$Sport, paired = FALSE, var.equal = TRUE)
