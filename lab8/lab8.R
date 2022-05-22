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
ind <- c("ВВП", "ВВП рост", "Рождаемость", "Коэффициетн рождаемости", "Скорректированный доход", "Безработные с вышкой", 'Безработные с базой', "Импорт товаров(%)", "Промышленность", "Государственное здравоохранение",
         "Продолжительность жизни", "Население", "Расходы государства", "Импорт товаров", "Экспорт товаров",
         "Смертность", "Образованных", "Образованных женщин", "Экспорт высоких технологий", "Лучшая отрасль", "Научные статьи")
indicators <- india[,3]
india <- india[-c(8,9),-c(1,2,3,4, 34)]

# Преобразование в числовой тип
india <- sapply(india, as.numeric)
# Транспонирование
india <- t(india)

rownames(india) <- years
colnames(india) <- ind
india <- data.frame(india)


