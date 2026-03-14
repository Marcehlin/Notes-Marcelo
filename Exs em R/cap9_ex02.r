n <- 10000
k <- 10 #grau de liberdade
y <- rchisq(n, k)

#x <- rnorm(n, 0, k/y)
z <- rnorm(n)
x <- z / sqrt (y/k)

hist(x, breaks = 20, probability = TRUE)
curve(dt(x, df = k), col = "red", lwd = 2, add = TRUE)

t <- rt(n, k)
hist(t, probability = TRUE)
