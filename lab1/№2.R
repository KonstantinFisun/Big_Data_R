#16.	Создать матрицу matr [m,n]. Значения m и n ввести с клавиатуры. 
n <- as.integer(readline(prompt = "Введите значение n: "))
m <- as.integer(readline(prompt = "Введите значение m: "))
matr <- matrix(data = rexp(200, rate = 1), nrow = n, ncol = m)
matr
#Присвоить имена строкам и столбцам. 
colnames(matr) <- c("A", "B", "C", "D","E")
rownames(matr) <- c("r1", "r2", "r3","r4","r5")
matr
#Сформировать субматирцу из четных строк. 
matr[seq(2,n,2),]
#Найти вектор максимальных элементов столбцов матрицы.
apply(matr, 2, function(x) max(x, na.rm = TRUE))

