library(dplyr)

# Считывание олимпийцев
olympics <- read.csv(file = "C:/Users/kosty/OneDrive/Документы/GitHub/Big_Data_R/lab7/athlete_events.csv", 
                     sep = ",", header = TRUE, dec = ',')

# Спортсмены по гимнастике
gymnastics_man <- subset(olympics, Sport == 'Gymnastics')


# Удалим строки, где есть пустые значения в весе
gymnastics_man[gymnastics_man == ""] <- NA # Пустые значение, сделали NA
gymnastics_man <- gymnastics_man[rowSums(is.na(gymnastics_man[,1:6])) == 0,]

# Убрали повторяющиеся строки со спортсменами
gymnastics_man <- gymnastics_man %>% group_by(ID) %>% filter (! duplicated(ID))

# Выбрали мужчин
gymnastics_man <- gymnastics_man[gymnastics_man$Sex == 'M',]

# Вес спортсменов
weight_gymnastics_man <- as.numeric(gymnastics_man$Weight)

# Гистограмма веса
hist(weight_gymnastics_man, main='Вес спорсменов',
     xlab='Вес', ylab='Частота',xlim = c(40,90))

# Проверка выборки на нормальность распределения с помощью Квантильно-квантильного графика
# (показывает распределение данных относительно ожидаемого нормального распределения)
qqnorm(weight_gymnastics_man)
qqline(weight_gymnastics_man, col='blue', lwd = 5)

# Доверительные интервалы
library(car)
qqPlot(weight_gymnastics_man)

# Тест Стьюдента, т.к. распределение нормальное
t.test(weight_gymnastics_man, mu=63.2)

# Тест Уилкоксона
wilcox.test(weight_gymnastics_man, mu=63, conf.int = TRUE)

# Тест Шапиро-Уилкса для проверки на нормальность
shapiro.test(weight_gymnastics_man[1:4999])

#-------------------------------------------------------------------------------

# Выборка  атлетов
athletics_man <- subset(olympics, Sex == 'M' & Sport == 'Athletics')


# Удалим строки, где есть пустые значения в весе
athletics_man[athletics_man == ""] <- NA # Пустые значение, сделали NA
athletics_man <- athletics_man[rowSums(is.na(athletics_man[,1:6])) == 0,]

# Убрали повторяющиеся строки со спортсменами
athletics_man <- athletics_man %>% group_by(ID) %>% filter (! duplicated(ID))

weight_athletics_man <- as.numeric(athletics_man$Weight)

# Гистограмма веса
hist(weight_gymnastics_man, main='Гистограмма веса гимнастов', xlab='Вес', xlim = c(40,150), ylim = c(0,600))
hist(weight_athletics_man, main='Гистограмма веса атлетов', xlab='Вес', xlim = c(40,150), ylim = c(0,5000))

# Проверка на нормальность
qqPlot(weight_gymnastics_man)
qqPlot(weight_athletics_man)


# Среднее
mean(weight_athletics_man, na.rm=TRUE)
mean(weight_gymnastics_man, na.rm = TRUE)

# Объединяем
gymnastics_athletics_man <- rbind(gymnastics_man, athletics_man)

# Тест на равенство дисперсий
bartlett.test(as.numeric(gymnastics_athletics_man$Weight) ~ gymnastics_athletics_man$Sport, data=gymnastics_athletics_man)

# Проверка, различаются ли выбранные средние значения с помощью теста Уэлча
t.test(as.numeric(gymnastics_athletics_man$Weight) ~ gymnastics_athletics_man$Sport)

# Проверка, при условии, что дисперсии равны
t.test(as.numeric(gymnastics_athletics_man$Weight) ~ gymnastics_athletics_man$Sport, paired = FALSE, var.equal = TRUE)
