# 2.13
library(tidyverse)
dt <- tibble(
  x = c(110,130,125,120,115,120,125,130,150,140,100,110,115,120,135),
  y = c(75,90,80,80,70,75,90,95,90,85,60,65,75,75,90),
)

ggplot(dt, aes(x=x, y=y)) + geom_point()
cor(dt)

# Exercise Chapter 2
# 2.1
a <- rep("A", 18)
b <- rep("B", 4)
o <- rep("O", 16)
ab <- rep("AB", 2)
bld <- factor(c(a,b,o,ab))
y <- as.matrix(table(bld))
freq <- y[,1]
relative_freq <- freq/sum(y)
t(rbind(freq, relative_freq))

sort <- sort(table(bld), decreasing = TRUE)
barplot(sort, xlab="Blood Type", ylab="Count", main="Exam 2.19")
#ggplot(as.data.frame(bld), aes(bld)) + geom_bar()

# 2.2
a <- rep("a", 125)
b <- rep("b", 30)
c <- rep("c", 40)
d <- rep("d", 105)
e <- rep("e",10)
f <- rep("f",30)
g <- rep("g", 10)
h <- c(a,b,c,d,e,f,g)
mat <- as.matrix(table(h))
freq <- mat[,1]
relative_freq <- freq/sum(mat)
t(rbind(freq,relative_freq))

#lab <- names(freq)
lab <- c("나홀로차량","카풀","버스","지하철","자전거","도보","기타")
dat <- round(freq/sum(freq)*100, digits=1)
z <- paste(lab, "(", dat,"%",")")
par(family="AppleGothic")
pie(freq, labels=z, main="연습문제 2.2 파이차트")

# 2.3
a <- rep("0", 3)
b <- rep("1", 13)
c <- rep("2", 12)
d <- rep("3", 18)
e <- rep("4", 17)
f <- rep("5", 4)
g <- rep("6", 3)
h <- rep("7", 1)
i <- rep("8", 2)
j <- rep("9", 1)
k <- rep("10", 1)
l <- c(a,b,c,d,e,f,g,h,i,j,k)
m <- as.matrix(table(l))
sum(m)

freq <- m[,1]
relative_freq <- freq/sum(m)
t(rbind(freq,relative_freq))

x <- factor(l)
sort <- sort(table(x), decreasing = TRUE)
barplot(sort, xlab = "Seats left", ylab = "Count", main = "연습문제 2.3 바차트")

# 2.4
x <- c(1.5, 5.6,3.4,5.3,3.5,2.8,4.4,3.6,6.8,4.5,4.0,5.7,6.6,6.8,4.5,2.4,4.4,5.4,4.7,3.5,5.7,6.3,9.5,1.5,4.8,7.7,8.3,10.5,4.2,5.3,5.2,6.,2.7,3.3,3.9,4.7,15.1,4.7,3.1,4.8)
hist(x, breaks = 8)
z <- as.matrix(x)
plot(x, y = c(1:40))
dotchart(x)
total <- sum(z)
relative_freq <- z/total
bind <- cbind(z, relative_freq)
bind
plot(bind)

# 2.5
a <- rep("0", 3)
b <- rep("10", 5)
c <- rep("20", 4)
d <- rep("30", 13)
e <- rep("40", 35)
f <- rep("50", 55)
g <- rep("60", 78)
h <- rep("70", 40)
i <- rep("80", 27)
j <- rep("90", 10)
k <- c(a,b,c,d,e,f,g,h,i,j)
l <- as.matrix(table(k))
total <- sum(l)
relative_frea <- l/total
m <- cbind(l, relative_frea)
hist(l, breaks = 10)
