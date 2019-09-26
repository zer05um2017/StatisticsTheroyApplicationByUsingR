a <- rep("A", 1520)
b <- rep("B", 770)
c <- rep("C", 510)
x <- c(a,b,c)
y <- as.matrix(table(x))
freq <- y[,1]
relative_freq <- freq/sum(y)
t(rbind(freq,relative_freq))

lab <- names(freq)
dat <- round(freq/sum(freq)*100, digits=1)
z <- paste(lab, "(", dat,"%",")")
par(family="AppleGothic")
pie(freq, labels=z, main="예제 2.2 파이차트")

a <- rep("A", 35)
b <- rep("B", 9)
c <- rep("C", 45)
d <- rep("D", 6)
e <- rep("E", 5)
x <- factor(c(a,b,c,d,e))
sort <- sort(table(x), decreasing = TRUE)
barplot(sort, xlab = "요소", ylab = "count", main = "예제2.3 파레토그림")

x <- c(1,1,1,3,0,0,1,1,1,0,2,2,0,0,0,1,2,1,2,0,0,1,6,4,3,3,1,2,4,0)
y <- as.matrix(table(x))
freq <- y[,1]
total <- sum(freq)
relative_freq <- freq/total
cumsum <- cumsum(freq)
cum_relative_freq <- cumsum/total
t(rbind(freq, relative_freq, cumsum, cum_relative_freq))

par(mfrow=c(1,2))
barplot(table(x), xlab="Mistakes", ylab="Count", main="Exam 2.5 bar graph")
#ggplot(as.data.frame(x), aes(x)) + geom_bar()
hist(x, sub="Mistakes", labels=T, main="Exam 2.5 Histogram")
#ggplot(as.data.frame(x), aes(x)) + geom_histogram()

x <- c(1,3,4,6,6,7,8,8,9,10,15)
mean(x)
median(x)
y <- c(1,3,4,6,6,7,8,8,9,10,150)
mean(y)
median(y)

var(x)
sd(x)
quantile(x, c(0.25,0.75))

x <- c(2,3,4,5,6,7,8,9,4,6,3,7,2,5,8)
y <- c(4,7,6,8,10,9,13,11,5,7,4,10,3,7,11)
plot(x, y, pch=19, main="Exam 2.12 Scatter diagram")
k <- t(as.data.frame(rbind(x,y)))
j <- as_tibble(k)
ggplot(data=j, aes(x=x, y=y)) + geom_point(size=3, color="black")

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
