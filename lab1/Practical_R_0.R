#####  Практика по R №0 #######
 
######    Ввод-вывод    #######

x<-2
cat(x)
print(x)
#-------------------------
hello <- "hello"
p<-print(hello)
#-------------------------
c<-cat(hello)
class(p)
#[1] "character"
class(c)
#[1] "NULL"
class(x)
#[1] "numeric"
p<-print(x)
c<-cat(x)

X <- scan()
# выполнение команды scan завершают введением пустой строки

length(X)

#-------------------------
y<-class(c(1, 2, TRUE))
#[1] "numeric"
y<-c(2, "boo")
#[1] "2"   "boo"
class(c(2, "boo"))
#[1] "character"

# Каков будет класс вектора?
#class(d<-c(1, 2, "tom", 2+8i, TRUE))
#typeof(d)



#------------------------- sprintf()
# NOT RUN {
## be careful with the format: most things in R are floats
## only integer-valued reals get coerced to integer.

sprintf("%s %f футов ростом\n", "Гусь", 7.1)      # OK
try(sprintf("%s %i футов ростом\n", "Гусь", 7.1)) # not OK
sprintf("%s %i футов ростом\n", "Гусь", 7  )  # OK

## use a literal % :

sprintf("%.0f%% сказали 'да'", 66.666)

## разные форматы pi :

sprintf("%f", pi)
sprintf("%.3f", pi)
sprintf("%1.0f", pi)
sprintf("%5.1f", pi)
sprintf("%05.1f", pi)
sprintf("%+f", pi)
sprintf("% f", pi)
sprintf("%-10f", pi) # left justified
sprintf("%e", pi)
sprintf("%E", pi)
sprintf("%g", pi)
sprintf("%g",   1e6 * pi) # -> exponential
sprintf("%.9g", 1e6 * pi) # -> "fixed"
sprintf("%G", 1e-6 * pi)

## no truncation:
sprintf("%1.f", 101)

## re-use one argument three times, show difference between %x and %X
xx <- sprintf("%1$d %1$x %1$X", 0:15)
xx <- matrix(xx, dimnames = list(rep("", 16), "%d%x%X"))
noquote(format(xx, justify = "right"))


# NOT RUN {
## Platform-dependent bad example from qdapTools 1.0.0:
## may pad with spaces or zeroes.
sprintf("%09s", month.name)
# }
# NOT RUN {
n <- 1:18
sprintf(paste0("e with %2d digits = %.", n, "g"), n, exp(1))

## Using arguments out of order
sprintf("second %2$1.0f, first %1$5.2f, third %3$1.0f", pi, 2, 3)

## Using asterisk for width or precision
sprintf("precision %.*f, width '%*.3f'", 3, pi, 8, pi)

## Asterisk and argument re-use, 'e' example reiterated:
sprintf("e with %1$2d digits = %2$.*1$g", n, exp(1))

## re-cycle arguments
sprintf("%s %d", "test", 1:3)

#__________________________Строки___________________

s = "В историю трудно войти, но легко вляпаться (М.Жванецкий)"
nchar(s)
length(s)
## [1] 56
substr(s, 3, 9) # извлекаем все символы с 3-го по 9-й
## [1] "историю"

# В отличие от чисел, конкатенация производится не оператором +, 
# а специальной функцией paste(). 
s1 = "В историю трудно войти,"
s2 = "но легко вляпаться"
s1
s2
s = paste(s1, s2); s
s<-s1+s2; s

strsplit(s,",")
# разбивает текстовый вектор х в соответствии с паттерном,
# заданным при помощи аргумента split

#луше для работы со строками использовать специальный пакет 
# с библиотекой "tidyverse"

#install.packages("stringi")
#library(tidyverse)
#library(stringi)

letters #  встроенный вектор
