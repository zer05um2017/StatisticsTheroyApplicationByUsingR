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
a <- sample(1:9,3,replace=T)
b <- sample(10:19,5,replace=T)
c <- sample(20:29,4,replace=T)
d <- sample(30:39,13,replace=T)
e <- sample(40:49,35,replace=T)
f <- sample(50:59,55,replace=T)
g <- sample(60:69,78,replace=T)
h <- sample(70:79,40,replace=T)
i <- sample(80:89,27,replace=T)
j <- sample(90:100,10,replace=T)
k <- c(a,b,c,d,e,f,g,h,i,j)
l <- as_tibble(k)
l <- l %>% mutate(value1 = k/sum(k))
ggplot(data = l) + geom_histogram(mapping = aes(x=value), binwidth = 10)


# 2.6
df <- tibble("data" = c(36,46,51,30,32,20,18,25,26,17,14,20,11,15,22,22,15,17,25,24,12,52,27,24,20,35,21,17,18,19,35,37,28,29,30,13,12,36,35,41,36,6,29,10,9,12,16,36,27,26))
ggplot(data = df, mapping = aes(y=data)) + geom_boxplot()

# 2.7
m <- c(54,38,35,60,51,48,47,43,45,50,45,47,46,49,53,44,45,45,48,58,41,40,46,45,48,53,55,47,47,43)
f <- c(54,55,61,68,58,59,67,78,45,43,56,56,57,55,62,64,68,65,54,50)
boxplot(m, f, horizontal = T)
ex_1 <- data.frame(m)
ex_2 <- data.frame(f)
summary(ex_1)
summary(ex_2)

# 2.9
df <- tribble(~type, ~manual, ~auto,
        #--|--|----
        "A", 10, 35,
        "B", 15, 25,
        "C", 18, 32,
        "D", 5, 15)
df <- df %>% mutate(rsum = manual + auto)

row <- tribble(~type,~manual, ~auto, ~rsum,
               #--|--|---
               "SUM", sum(df$manual), sum(df$auto), sum(df$rsum))

df <- df %>% bind_rows(row)
df
t(df[1:5,2:4] / 155)

# 2.11
df <- tribble(~type, ~top, ~median, ~bottom,
              #--|--|--|--
              "A", 43, 20, 17,
              "B", 50, 35, 15,
              "C", 45, 40, 15,
              "D", 25, 30, 35,
              "E", 5,  15, 10)

df <- df %>% mutate(rsum = top + median + bottom)

row <- tribble(~type, ~top, ~median, ~bottom, ~rsum,
              #--|--|--|--|--
              "SUM", sum(df$top), sum(df$median), sum(df$bottom), sum(df$rsum))
df <- bind_rows(df, row)
df[1:nrow(df),2:ncol(df)]/as.numeric(df[6,5])

# 2.12
df <- tribble(~X, ~Y,
              #--|--
              0,4,
              1,3,
              6,0,
              3,2,
              5,1)

ggplot(data = df) + geom_point(mapping = aes(x=X, y=Y))
cor(df)
#X          Y
#X  1.0000000 -0.9922779
#Y -0.9922779  1.0000000

V <- 2*df$X + 3
W <- df$Y + 1
cor(V,W)
#[1] -0.9922779 
