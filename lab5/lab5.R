library(rvest)

select_rat <- "td" # Таблица рейтинга

names_index <- c("Страна", "Качества жизни", "Покупательная способность", "Безопасность",
                "Здравоохранение", "Стоимости жизни", "Недвижимости к доходу",
                "Пробки", "Загрезнения", "Климат")

all_rating <- list() # Создание пустоq список
years <- c() # Создали пустой вектор хранящий года

# Идем по годам
for (i in 2014:2022){
  years <- c(years, toString(i)) # Добавили год
  iter <- read_html(paste0("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=", toString(i))) # Считывание со страницы
  
  country_rat <- html_nodes(iter, select_rat) %>% html_text() %>% as.array() # Считали рейтинги
  country_rat <- country_rat[-c(1,2,3)] # Убрали лишние из вектора
  contry_rat <- matrix(country_rat, ncol = 11, nrow = 60, byrow = TRUE) # Создание матрицы
  
  contry_rat <- contry_rat[,-c(11)] # Удаление пустого столбца
  
  rating <- data.frame(contry_rat) # Создание базы
  
  names(rating) <- names_index # Присвоили индекс
  
  all_rating[[length(all_rating)+1]] = rating # Добавление в список
}

names(all_rating) <- years # Присвоили года



# Канада, США, Турция, Греция, Дания - мои страны
my_country <- c("Canada", "United States", "Turkey", "Greece", "Denmark")


# Сравнение стран по Индексу жизни за последнии 5 лет

# Сбор данных

index_life <- list() # список индексов по каждой стране
for(country in my_country){
  iter <- c()
  for (year in 2014:2022){
    iter <- c(iter, all_rating[[toString(year)]][all_rating[[toString(year)]][,1] == country,2])# Выбрали индекс качества жизни определенной страны
  }
  index_life[[length(index_life)+1]] = iter # Добавление в список
  
}
names(index_life) <- my_country # Присвоили имена стран

# График изменения индекса качества жизни с 2014 года у каждой из 5 стран
plot(years,index_life[[1]], type='o', lty=1, pch=20, col='brown', cex = 3, cex.axis = 2, cex.lab = 1.5,
     main='Тенденции изменения качества жизни',
     xlab='Года',
     ylab='Индекс качества жизни', ylim = c(50,230))
lines(years, index_life[[2]], type='o', lty=1, pch=10, col='green',cex = 3,)
lines(years, index_life[[3]], type='o', lty=1, pch=15, col='red',cex = 3)
lines(years, index_life[[4]], type='o', lty=1, pch=17, col='blue',cex = 3)
lines(years, index_life[[5]], type='o', lty=1, pch=18, col='black',cex = 3)


legend('topright', my_country,
       pch=c(20,10,15,17,18,12,13), lty=c(1,1,1,1,1,1,1),
       col=c('brown', 'green', 'red', 'blue', 'black'),
       y.intersp = 0.8, text.width = 1)
#-------------------------------------------------------------------------------

# График изменения индексов у Канады с 2014 года

# Сбор данных
index_canada <- c()
for (year in 2014:2022){
  iter <- c()
  for (i in 2:10){
    iter <- c(iter, as.numeric(all_rating[[toString(year)]][all_rating[[toString(year)]][,1] == "Canada",i])) # Выбрали индекс качества жизни определенной страны
  }
  index_canada <- c(index_canada, iter)
}

index_canada <- data.frame(matrix(index_canada,ncol = 9, byrow = TRUE)) # Создание data.frame

# Добавляем имена столбикам и строкам
colnames(index_canada) <- names_index[-1]
rownames(index_canada) <- years


# График изменения индексов у Канады с 2014 года
barplot(data.matrix(index_canada),col = rainbow(9), beside = TRUE, ylim=c(0,200), cex.names = 0.85,
        main = "График изменения индексов у Канады с 2014 года", ylab = "Индекс") #beside = TRUE

legend('topright', rownames(index_canada), col = rainbow(9),
       y.intersp = 1, text.width = 10, pch=15)

#-------------------------------------------------------------------------------

# График за 2022 год, сравнение стран по всем индексам

index_country <- c() # список индексов по каждой стране
for(country in my_country){
  country_iter <- numeric()
  for (i in 2:10){
    country_iter <- c(country_iter, as.numeric(all_rating[['2022']][all_rating[['2022']][,1] == country,i])) # Выбрали индексы для страны
  }
  index_country <- c(index_country,country, country_iter) # Добавили страну и её индекс
  
}

index_country <- data.frame(matrix(index_country,ncol = 10, byrow = TRUE)) # Создание data.frame

colnames(index_country) <- names_index # Присвоили имена индексам
rownames(index_country) <- my_country # Присвоили страны

index_country <- index_country[-c(1)] # Удалили лишние

index_country <- sapply(index_country, as.numeric) # Сделали из character -> numeric


# График
barplot(data.matrix(index_country),col = rainbow(5), beside = TRUE, ylim=c(0,200), cex.names = 0.85,
        main = "График сравнения индексов стран за 2022 год", ylab = "Индекс") #beside = TRUE

legend('topright', my_country, col = rainbow(5),
       y.intersp = 1, text.width = 10, pch=15)

#-------------------------------------------------------------------------------

# Задание с музеем
# Считывание всех музеев со страницы
url_museum <- read_html("https://tonkosti.ru/Музеи_Санкт-Петербурга") # Считали ссылку

select_museum <- "h3.places-list__item-header" # Название класса хранящий имя музея
select_museum_address <- "div.places-list__address--rc"# Класс хранящий адреса музеев
select_museum_url <- ".places-list__item-img"

museums_names <- html_nodes(url_museum, select_museum) %>% html_text() %>% as.array() # Считали названия всех музеев
museums_adress <- html_nodes(url_museum, select_museum_address) %>% html_text() %>% as.array() # Считали адреса всех музеев
museums_urls <- html_nodes(url_museum, select_museum_url) %>% html_attr("href") %>% as.array()

museums_names
museums_adress
museums_urls <- unique(museums_urls) # Удалили повторения

full_museums_address <- paste0("https://tonkosti.ru", museums_urls) # Добавили полный адрес музея

museum <- data.frame("Музей" = museums_names, "Адрес" = museums_adress, "Веб страница" =  full_museums_address) # Создание базы музеев
