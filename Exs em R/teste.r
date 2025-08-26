B <- 1000
U <- runif(B)
X <- ifelse(U<0.2,0,1)
hist(X)

n <- 10000
u <- runif(n)
bins <- findInterval(u, c(0.3, 0.4))
bins
hist(bins)
help(findInterval)
