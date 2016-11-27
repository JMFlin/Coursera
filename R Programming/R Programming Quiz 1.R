x <- 4
class(4)

x <- c(4, "a", TRUE)
class(x)

x <- c(1,3, 5)
y <- c(3, 2, 10)

cbind(x,y)

x <- list(2, "a", "b", TRUE)
a <- x[[1]]
class(a)

x <- list(2, "a", "b", TRUE)
x[[2]]

x <- 1:4
y <- 2:3
x+y

x <- 1:4
y <- 2
x + y

x <- c(3, 5, 1, 10, 12, 6)
x[x <= 5] <- 0
x

a <- read.csv("hw1_data.csv", sep = ",")
head(a)
tail(a)
a[47,]
sum(is.na(a$Ozone))
mean(a$Ozone, na.rm = T)
b <- a[which(a$Ozone > 31 & a$Temp > 90), ]
mean(b$Solar.R)

c <- a[which(a$Month == 6), ]
mean(c$Temp, na.rm = T)

d <- a[which(a$Month == 5), ]
max(d$Ozone, na.rm = T)
