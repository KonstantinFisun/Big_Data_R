library("igraph")
library("network")
library("sna")

# Создание графа
g1<-graph(c(1,2,1,3,1,4,2,3,2,4, 3,4, 4,5, 5,6, 2,6),n=6)

# Вывод вершин
V(g1)

# Вывод ребер
E(g1)
E(g1, P=NULL, path=NULL, directed=TRUE)

# Добавление новых вершин и ребер в граф: 
add.edges(g1, edges, ..., attr=list())
add.vertices(g1, vertices, ..., attr=list())

# Для создания полносвязного направленного/ненаправленного графа с циклами/без циклов 
# используйте команду graph.full с дополнительными параметрами
g<-graph.full(n=10, directed = FALSE, loops = FALSE)

# Граф звезда
g<-graph.star(n=10, mode="out")

g<-graph.star(n=10, mode="in")

#	Граф-кольцо
g<-graph.ring(n=10)

#	Граф с заданным списком ребер
edges <- c(1,2, 3,2, 2,4)
g<-graph(edges, n=max(edges), directed=TRUE)

# Отрисовка графа
plot(g1)
# Матрица смежности
g1[]

# Проверяем связность вершин
are_adjacent(g1,5,6) 
are_adjacent(g1,6,1) 

# Матрица достижимости
distMatrix <- distances(g1, v=V(g1), to=V(g1), weights=NA)
distMatrix

# Вы можете добавить узлы и ребра к уже существующему графу
# Команда 1: Создает граф с буквенными обозначениями для вершин с 1-й по 10-ю 
# букву латинского алфавита. Цвет вершин – красный.   
g<-graph.empty()+vertices(letters[1:10],color="red") 
plot(g)
# Команда 2: Добавляет к графу еще 10 синих вершин
g<-g+vertices(letters[11:20],color="blue") 
plot(g)
# Команда 3: Случайным образом формирует связи между вершинами (15 шт), окрашивая их в зеленый цвет.
g<-g+edges(sample(V(g),30,replace=TRUE),color="green")
plot(g)
# Команда 4:Рисует граф, задавая размеры вершин и стрелок
plot(g, edge.arrow.size=.4,vertex.size=20)

#===============================================================================
# Получение информации о структуре графа
edges <- c(1,2, 3,2, 2,4, 3,4)
g<-graph(edges, n=max(edges), directed=TRUE)

# Количество вершин
vcount(g)
# Количество ребер
ecount(g)

plot(g)

# число соседей для вершины
neighbors(g, V(g)[3], mode = 1)

# список смежных ребер для данной вершины
incident(g,V(g)[3], mode=c("all", "out", "in", "total"))

# Тип графа (направленный или ненаправленный)
is.directed(g)

# Связь между вершинами
are.connected(g, V(g)[1], V(g)[3])

# Список ребер в графеб.
get.edgelist(g)

#===============================================================================
# Импорт-экспорт графов

# Загрузить из текстового файла по списку вершин:
g <- read.graph("./graph.txt", format="edgelist")

# Создать граф из Базы Данных
advice_data_frame <- read.table('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-edgelist-Advice.txt')
g <- graph.data.frame(advice_data_frame)

# Экспорт графа
write.graph(g, file='my_graph.txt', format="edgelist")

# Визуализация Erdos-Renyi
er_graph<-erdos.renyi.game(100,2/100)
plot(er_graph,vertex.label=NA,vertex.size=3)

# Визуализация Watts Strogatz
ws_graph<-watts.strogatz.game(1,100,4,0.05)
plot(ws_graph,layout=layout.circle,vertex.label=NA,vertex.size=3)

# Визуализация Barabasi
ba_graph<-barabasi.game(100)
plot(ba_graph,vertex.label=NA,vertex.size=3)

#===============================================================================
# Функции размещения графов
g <- make_ring(10) + make_full_graph(5)

# Размещение в виде звезды
coords <- layout_(g, as_star())
plot(g, layout = coords)

# Размещение в виде круга
coords <- layout_(g, in_circle())
plot(g, layout = coords)

# Размещение в виде дерева
coords <- layout_(g, as_tree())
plot(g, layout = coords)

# Алгоритм силового размещения Камада-Каваи
g<-graph.lattice(length=100,dim=1,nei=5, circular = TRUE)
plot(g,vertex.size=2,vertex.label=NA,layout=layout.kamada.kawai)

#===============================================================================
# Измерение графов 
plot(g1)

# Диаметр
diameter(g1)

# Все пути
g1.all_simple<-all_simple_paths(g1, 2, 6) 
g1.all_simple

# Все короткие пути к другим вершинам
all_shortest_paths(g1, 1, to = V(g1), mode = c("out", "all", "in"),weights = NULL)

# Степеь каждой вершины
deg <- degree(g1, mode="all")

#===============================================================================
# Задание 1
# 1.	Создайте кольцевой граф  g со случайным числом вершин G_size  124
G_size <- sample(c(26:27),1)
g1<-graph.ring(n = G_size)
coords <- layout_(g1, as_star())
 # Количество вершин
vcount(g1)
# Количество ребер
ecount(g1)

# Матрица смежности
g1[]

plot(g1, main='Кольцевой граф', edge.arrow.size=.9,vertex.size=15)

# 2.	Создайте  граф g1 из  пустого графа с числом вершин G_size  желтого цвета. 
g1<-graph.empty()+vertices(1:G_size,color="yellow") 
g1[]
plot(g1, main = "Пустой граф")

# Добавьте ему 8 случайных ребер, сформированных из вектора вершин, окрасьте ребра красным цветом, 
g1 <- g1 + edges(sample(V(g1), 16, replace=TRUE), replace=TRUE, color="red")
g1[]
plot(g1, main = "Добавили красных ребер", layout = coords, edge.arrow.size=.2)

# Добавьте графу g1 еще  16 случайных ребер, сформированных из вектора вершин, окрасьте ребра синим цветом
g1 <- g1 + edges(sample(V(g1), 32, replace=TRUE), replace=TRUE, color="blue") 
g1[]
plot(g1, layout = coords, main="Добавили синих ребер", edge.arrow.size=.2)

# 3.	Добавьте ребра между вершиной 55 и 52, 54 и 31, 31 и 24, 32 и 33, 23 и 29

edges <- c(55,52, 54,31, 31,24, 32,33, 23,29)
i <- 1
while(i <= length(edges)){
  if((edges[i] %in% V(g1)) && (edges[i+1] %in% V(g1)))
    g1 <- g1 + edges(c(edges[i],edges[i+1]), replace=TRUE, color="black")
    #add.edges(g1, c(edges[i],edges[i+1]),  color="black")
  i <- i+1
}
plot(g1, layout = coords, main="Добавили черных ребер", edge.arrow.size=.2)

# Выведите соседей N - й вершины
neighbors(g1, V(g1)[16], mode = 1)
incident(g1, V(g1)[16], mode=("all"))
are_adjacent(g1, 26, 28)
g1[]

# 4. 4.	Добавьте еще одну вершину и подключите ее к той, которая имеет 
# наибольшее количество связанных с ней узлов
# Степень для каждой вершины
deg<-igraph::degree(g1)
deg
# Первая максимальная 
max_ver <- which(deg == max(deg))[[1]]
incident(g1,V(g1)[max_ver], mode=c("all", "out", "in", "total"))

# Добавляем новую вершину
g1 <- g1 + vertices("New", color="pink") + edges(c(max_ver,"New"), replace=TRUE, color="green")

plot(g1, layout=layout.circle, main="Добавили новую вершину", edge.arrow.size=.2)

# Алфавит
alf<-c(65:90,97:122,1040:1103)
alf<-intToUtf8(alf)
V(g1)$alf
plot(g1, main="Переименнованный граф", vertex.color="white", vertex.size=8, vertex.frame.color="yellow", vertex.label=alf[[1]][1:G_size])
g1[]

# Алгоритмы размещения
plot(g1, layout=layout_as_tree, edge.arrow.size=.4, main='Дерево')

plot(g1, layout=layout.kamada.kawai, edge.arrow.size=.4, main='Алгоритм Камада-Каваи')

plot(g1, layout=layout.fruchterman.reingold, edge.arrow.size=.4, main='Алгоритм Фрюхтермана-Рейнгольда')


# Измерение диаметра графа 
diameter(g1)

# Список самых коротких путей  для каждой вершины

all_simple_path_g1 <- list()
for (i in 1:length(V(g1))) {
  all_simple_path_g1 <- append(all_simple_path_g1, all_shortest_paths(g1, i, to = V(g1), mode = c("out", "all", "in"), weights = NULL))
}
all_simple_path_g1

