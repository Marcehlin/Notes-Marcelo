lambda <- 5
n_amostras <- 1000

gerar_poisson <- function(lambda) {
  k <- 0
  S <- 0
  while (S <= 1) {
    X <- rexp(1, rate = lambda)
    S <- S + X
    k <- k + 1
  }
  return(k - 1)
}

N <- replicate(n_amostras, gerar_poisson(lambda))
hist(N, breaks = 15, probability = TRUE)
x_teorico <- 0:max(N)
y_teorico <- dpois(x_teorico, lambda = lambda)
points(x_teorico, y_teorico, type = "b", col = "red", lwd = 2, pch = 16)
