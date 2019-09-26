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


