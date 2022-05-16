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

# Стандартизация переменных
airplane_N <- airplan_crushes[,c(10,11,12)]

maxs <- apply(airplane_N, 2, max) # Максимальное значение
mins <- apply(airplane_N, 2, min) # Минимальное значение

airplane_N <- scale(airplane_N, center = mins, scale = maxs - mins)

# Готовые кластеры из RLab_6_1
groups

# Преобразование в фактор
groups_f <- factor(groups)


#-------------------------------------------------------------

