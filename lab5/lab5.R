library(rvest)

# url_country_rating <- read_html("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2021")

# select_country <- ".cityOrCountryInIndicesTable" # Название класса хранящий имена стран
# select_rat <- "td" # Таблица рейтинга

# country_names <- html_nodes(url_country_rating, select_country) %>% html_text() %>% as.array() # Считали имена стран с таблицы
# country_rat <- html_nodes(url_country_rating, select_rat) %>% html_text() %>% as.array() # Считали рейтинги
# country_rat <- country_rat[-c(1,2,3)] # Убрали лишние из вектора
# contry_rat <- matrix(country_rat, ncol = 11, nrow = 83, byrow = TRUE) # Создание матрицы

select_rat <- "td" # Таблица рейтинга

# Ссылки на определенные года
# url_country_rating_2022 <- read_html("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2022") # 22
# url_country_rating_2021 <- read_html("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2021") # 21
# url_country_rating_2020 <- read_html("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2020") # 20
# url_country_rating_2019 <- read_html("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2019") # 19
# url_country_rating_2018 <- read_html("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2018") # 18

names_index <- c("Страна", "Индекс качества жизни", "Индекс покупательной способности", "Индекс безопасности",
                "Индекс здравоохранения", "Индекс стоимости жизни", "Соотношение цены на недвижимости к доходу",
                "Индекс времени в пути в пробках", "Индекс загрезнения", "Климатический индекс")

all_rating <- list() # Создание пустоq список
years <- c() # Создали пустой вектор хранящий года
# Идем по годам
for (i in 2014:2022){
  years <- c(years, toString(i))
  iter <- read_html(paste0("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=", toString(i)))
  
  country_rat <- html_nodes(iter, select_rat) %>% html_text() %>% as.array() # Считали рейтинги
  country_rat <- country_rat[-c(1,2,3)] # Убрали лишние из вектора
  contry_rat <- matrix(country_rat, ncol = 11, nrow = 60, byrow = TRUE) # Создание матрицы
  
  contry_rat <- contry_rat[,-c(11)] # Удаление пустого столбца
  
  # Создание базы
  rating <- data.frame(contry_rat)
  
  names(rating) <- names_index
  
  all_rating[[length(all_rating)+1]] = rating
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
  index_life[[length(index_life)+1]] = iter
  
}
names(index_life) <- my_country # Присвоили имена стран

# График изменения индекса качества жизни с 2014 года у каждой из 5 стран
plot(years,index_life[[1]], type='o', lty=1, pch=20, col='brown', cex = 3,
     main='Тенденции изменения качества жизни',
     xlab='Года',
     ylab='Индекс качества жизни', ylim = c(80,230))
lines(years, index_life[[2]], type='o', lty=1, pch=10, col='green',cex = 3,)
lines(years, index_life[[3]], type='o', lty=1, pch=15, col='red',cex = 3)
lines(years, index_life[[4]], type='o', lty=1, pch=17, col='blue',cex = 3)
lines(years, index_life[[5]], type='o', lty=1, pch=18, col='black',cex = 3)


legend('topright', my_country,
       pch=c(20,10,15,17,18,12,13), lty=c(1,1,1,1,1,1,1),
       col=c('brown', 'green', 'red', 'blue', 'black'),
       y.intersp = 1, text.width = 2)


# График изменения индексов у Канады с 2014 года
index_canada <- c()
for (year in 2014:2022){
  iter <- c()
  for (i in 2:10){
    iter <- c(iter, as.numeric(all_rating[[toString(year)]][all_rating[[toString(year)]][,1] == "Canada",i]))# Выбрали индекс качества жизни определенной страны
  }
  index_canada <- c(index_canada, iter)
}

index_canada <- data.frame(matrix(index_canada,ncol = 9, byrow = TRUE))


colnames(index_canada) <- names_index[-1]
rownames(index_canada) <- years

# index_canada <- lapply(index_canada, function(x) as.numeric(gsub("^.*\\.", "", x)))


barplot(data.matrix(index_canada),col = rainbow(9), beside = TRUE, ylim=c(0,200), cex.names = 0.54,
        main = "График изменения индексов у Канады с 2014 года", ylab = "Индекс") #beside = TRUE

legend('topright', rownames(index_canada), col = rainbow(9),
       y.intersp = 1, text.width = 10, pch=15)

# Один год, все страны все индексы







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
