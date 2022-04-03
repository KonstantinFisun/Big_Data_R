install.packages("xlsx", dep = T)
library("xlsx")

food <- read.xlsx("C:/Users/kosty/OneDrive/������� ����/3 ���� 6 ������/��������� ������� ������/Practical R/lab2/food.xlsx", sheetIndex = 1)
rownames(food)<-food[,1]
name <- food[,1]

food<-food[,-1] #������� �����
food_name<- colnames(food)
colMax <- function(data) sapply(data, max, na.rm = TRUE) # ������� ���� � ������ �������
colMin <- function(data) sapply(data, min, na.rm = TRUE) # ������� ��� � ������ �������
colMean <- function(data) sapply(data, mean, na.rm = TRUE) # ������� �������� � ������ �������

# ������� ����
getmode <- function(vector) {
  uniqv <- unique(vector) # ������� ���������� �������� �������
  uniqv[which.max(tabulate(match(vector, uniqv)))] # ������� �������� ������ ������������ ��� ����������
}


colMax(food) # ������������ ������
colMin(food) # ����������� ������
colMean(food) # ������� ������



count_score_high <- vector()

for(i in 1:ncol(food)){
  
  count_score_high[i] <- sum(food[,i] > 7)
}

count_score_high

rbind(food_name, count_score_high) # ���������� �����, �������� ������������ >7


count_score_low <- vector()

for(i in 1:ncol(food)){
  count_score_low[i] <- sum(food[,i] < 3)
}

count_score_low

rbind(food_name, count_score_low) # ���������� �����, �������� ������������ <3

sort(colMean(food),decreasing = TRUE) # �������


# ���������� ��������� ������ 
barplot(height = colMean(food),col = "steelblue",
        xlab = "���",
        ylab = "������� ������"
)

# ����������� ������� ������
hist(colMean(food))

boxplot(colMean(food),
        main = "������� ������",
        ylab = "������� ������",
        col=rainbow(10))


boxplot(food[,c(1:10)],
        main = "������ ���",
        xlab = "�����",
        ylab = "������",
        col = rainbow(10))

# ����������� ���������� ������
hist(data.matrix(na.omit(food)),
     breaks = 10,
     main = "�����������",
     xlab = "������",
     ylab = "����������",
     col = rainbow(10))

# ������ �� csv
mdf <- read.table('food.csv', header=TRUE,  sep=";", row.names = "Name")

# ������������� ������
data <- sapply(mdf, summary, na.rm = TRUE)
sd <- sapply(mdf,sd,na.rm = TRUE) # ����������� ����������
var <- sapply(mdf,var,na.rm = TRUE) # ���������
IQR <- sapply(mdf, IQR, na.rm = TRUE) # �������������� ������
mode <- sapply(mdf, getmode) # ����
rbind(data, sd, var, IQR, mode)

# ���������� �� 3 ��������
mdf[order(mdf$Cezar,mdf$Kotleta.s.pure,mdf$Makaroni),]

# ������������ ��������� ������ ������ �� ����������� ��������
# ������ �� ������ ������ 9
newdata1 <- subset(mdf, Cezar>=9)

dim(newdata1) # �����������

hist(data.matrix(na.omit(newdata1)),
     breaks = 10,
     main = "�����������",
     xlab = "������",
     ylab = "����������",
     col = rainbow(10))

boxplot(newdata1[,c(1:10)],
        main = "������ ���",
        xlab = "�����",
        ylab = "������",
        col = rainbow(10))


# ������ �� ������� � ������ 
newdata2 <- subset(mdf, Kotleta.s.pure > 7 & Olive > 7)

dim(newdata2) # �����������

hist(data.matrix(na.omit(newdata2)),
     breaks = 10,
     main = "�����������",
     xlab = "������",
     ylab = "����������",
     col = rainbow(10))

boxplot(newdata2[,c(1:10)],
        main = "������ ���",
        xlab = "�����",
        ylab = "������",
        col = rainbow(10))

# ������ �� ������ <= 6, Pizza >= 8, Makaroni < 8
newdata3 <- subset(mdf, Burger <= 6 & Pizza >= 8 & Makaroni < 8)

dim(newdata3) # �����������

hist(data.matrix(na.omit(newdata3)),
     breaks = 10,
     main = "�����������",
     xlab = "������",
     ylab = "����������",
     col = rainbow(10))

boxplot(newdata3[,c(1:10)],
        main = "������ ���",
        xlab = "�����",
        ylab = "������",
        col = rainbow(10))



  
