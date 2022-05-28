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


