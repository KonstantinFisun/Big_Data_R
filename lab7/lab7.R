library(dplyr)

# Считывание олимпийцев
horses <- read.csv(file = "C:/Users/kosty/OneDrive/Документы/GitHub/Big_Data_R/lab7/HorsePrices.csv", 
                     sep = ",", header = TRUE)

horses[horses == ""] <- NA # Пустые значение, сделали NA
# Удалим строки, где есть пустые значения
horses <- horses[rowSums(is.na(horses[,])) == 0,]

mean <- apply(horses[,3:5], 2, mean) # Среднее значение

hor <- horses[horses$Price > mean[1] && horses$Age < mean[2] && horses$Height < mean[3],]

cor(horses$Price, horses$Height, method = "spearman")
cor.test(horses$Price, horses$Height, method = "spearman")

cor(horses$Price, horses$Height, use='pairwise.complete.obs')
plot(horses$Price, horses$Height, 
     main="Корреляция прироста цены на рост лошади",
     xlab="цена", ylab="рост")

cor(horses$Price, horses$Age, use='pairwise.complete.obs')
plot(horses$Price, horses$Age, 
     main="Корреляция прироста цены на возраст лошади",
     xlab="цена", ylab="возраст")

# Гистограмма возраста
hist(horses$Age, main='Гистограмма возраста лошадей',
     xlab='Возраст', ylab='Частота')

#Проверим на нормальность
# Проверка выборки на нормальность распределения с помощью Квантильно-квантильного графика
# (показывает распределение данных относительно ожидаемого нормального распределения)
qqnorm(horses$Age)
qqline(horses$Age, col='blue', lwd = 5)

# Распределение нормальное, проведем тест Стьюдента
# Тест Стьюдента, т.к. распределение нормальное
# Проверим, что средний вес равен 7
t.test(horses$Age, mu=7)
# Т.к p-value > 0.05 гипотезу принимаем

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
