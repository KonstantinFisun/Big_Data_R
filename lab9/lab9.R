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

