install.packages("rvest")
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
url_country_rating_2022 <- read_html("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2022") # 22
url_country_rating_2021 <- read_html("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2021") # 21
url_country_rating_2020 <- read_html("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2020") # 20
url_country_rating_2019 <- read_html("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2019") # 19
url_country_rating_2018 <- read_html("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=2018") # 18


all_rating <- c() # Создание пустого вектора
# Идем по годам
for (i in 2014:2022){
  iter <- read_html(paste0("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=", toString(i)))
  
  country_rat <- html_nodes(iter, select_rat) %>% html_text() %>% as.array() # Считали рейтинги
  country_rat <- country_rat[-c(1,2,3)] # Убрали лишние из вектора
  contry_rat <- matrix(country_rat, ncol = 11, nrow = 60, byrow = TRUE) # Создание матрицы
  
  contry_rat <- contry_rat[,-c(11)] # Удаление пустого столбца
  
  # Создание базы
  rating <- data.frame(contry_rat)
  
  all_rating <- c(all_rating,rating)
}

all_rating

country_rat_2022 <- html_nodes(url_country_rating_2022, select_rat) %>% html_text() %>% as.array() # Считали рейтинги
country_rat_2022 <- country_rat[-c(1,2,3)] # Убрали лишние из вектора
contry_rat_2022 <- matrix(country_rat, ncol = 11, nrow = 83, byrow = TRUE) # Создание матрицы



contry_rat_20 <- contry_rat[,-c(11)] # Удаление пустого столбца

# Создание базы
rating <- data.frame(contry_rat)
# Имена столбцов
names(rating) <- c("Страна", "Индекс качества жизни", "Индекс покупательной способности", "Индекс безопасности",
                              "Индекс здравоохранения", "Индекс стоимости жизни", "Соотношение цены на недвижимости к доходу",
                              "Индекс времени в пути в пробках", "Индекс загрезнения", "Климатический индекс")




# Канада, США, Турция, Греция, Дания - мои страны
# Составим таблицу из моих стран
rating_my <- rating[c(2,15,20,43,45),]


# Сравнение стран по Индексу жизни за последнии 5 лет



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
