install.packages("rvest")
library(rvest)

# ���������� ���� ������ �� ��������
url_museum <- read_html("https://tonkosti.ru/�����_�����-����������") # ������� ������

select_museum <- "places-list__item-header" # �������� ������ �������� ��� �����
select_museum_address <- "div.places-list__address--rc"# ����� �������� ������ ������
select_museum_url <- ".places-list__item-img--rc"

museums_names <- html_nodes(url_museum, select_museum) %>% html_text() %>% as.array() # ������� �������� ���� ������
museums_adress <- html_nodes(url_museum, select_museum_address) %>% html_text() %>% as.array() # ������� ������ ���� ������
museums_urls <- html_nodes(url_museum, select_museum_url) %>% html_text("href") %>% as.array()

museums_names
museums_adress