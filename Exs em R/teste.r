B <- 1000
U <- runif(B)
X <- ifelse(U<0.2,0,1)
hist(X)
