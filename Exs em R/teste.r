B <- 1000
U <- runif(B)
X <- ifelse(U<0.2,0,1)
hist(X)

n <- 10000
u <- runif(n)
bins <- findInterval(u, c(0.3, 0.4))
bins
hist(bins)

x <- rnorm(n, mean = 5, sd = 2)
print(sd(x))
# Histograma em densidade (freq=FALSE)
hist(x, probability = TRUE, col = "lightblue", border = "white")

# Adicionando curva da normal teórica
curve(dnorm(x, mean = 5, sd = 2), col = "red", lwd = 2, add = TRUE)

n <- 10000
lambda <- 5
exp_emp <- rexp(n, rate = lambda)
hist(exp_emp, probability = TRUE, breaks = 20)
curve(dexp(x, rate = lambda), from = 0, to = max(exp_emp), col = "red", lwd = 2, add = TRUE)

n <- 10000
U <- runif(n)
Y <- (-1/10) * log(1 - U)

# Histogram (scaled to density)
hist(Y, probability = TRUE, breaks = 50, col = "lightgray", border = "white")

# Overlay exponential density using curve()
curve(dexp(x, rate = 10), 
      from = 0, to = max(Y), 
      col = "blue", lwd = 2, add = TRUE)
