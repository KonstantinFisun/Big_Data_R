install.packages("rvest")
library(rvest)

# Считывание всех музеев со страницы
url_museum <- read_html("https://tonkosti.ru/Музеи_Санкт-Петербурга") # Считали ссылку

select_museum <- "h3.places-list__item-header" # Название класса хранящий имя музея
select_museum_address <- "div.places-list__address--rc"# Класс хранящий адреса музеев
select_museum_url <- "a.places-list__item-img"

museums_names <- html_nodes(url_museum, select_museum) %>% html_text() %>% as.array() # Считали названия всех музеев
museums_adress <- html_nodes(url_museum, select_museum_address) %>% html_text() %>% as.array() # Считали адреса всех музеев
museums_urls <- html_nodes(url_museum, select_museum_url) %>% html_text("href") %>% as.array()

museums_names
museums_adress

