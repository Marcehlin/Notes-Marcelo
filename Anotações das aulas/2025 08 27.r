#Capítulo 6, Técnica de Inversão pra variáveis contínuas
X <- U^(1/5)
hist(X)

n <- 10000
U <- runif(n)
Y <- (-1/10) * (log(1 - U))
hist(Y,probability = TRUE)
grid <- seq(0, max(Y), length.out = 200)
grid
lines(grid, dexp(grid, rate = 10), col = 2, lwd = 3)

help(grid)
Y2 <- rexp(n)
hist(Y2)

help(qgamma)

#B <- 10000
#n <- 10 
#u <- matrix(runif(B))