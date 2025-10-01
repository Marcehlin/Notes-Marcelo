inversa_cdf_geometrica <- function(p, u) {
  # k = falhas antes do 1º sucesso
  k <- ceiling(log1p(-u) / log1p(-p)) - 1L
  as.integer(k)
}

inversa_cdf_poisson_recursiva <- function(lam, u) {
  k <- 0
  p <- exp(-lam)  # P(X=0)
  f_acm <- p  # Iniciamos com a probabilidade P(X=0)

  # Continuamos somando até que F >= u
  while (u > f_acm) {
    k <- k + 1
    # Atualiza a probabilidade recursivamente para o próximo valor
    p <- p * lam / k
    f_acm <- f_acm + p
  }

  return (k)
}

gerar_mistura <- function(alpha, u) {
  u2 <- runif(1)
  if (u < alpha) {
    x_mistura <- inversa_cdf_poisson_recursiva(3, u2)
  } else {
    x_mistura <- inversa_cdf_geometrica(0.5, u2)
  }
  return (x_mistura)
}
alpha = 0.8

n <- 1000
uniformes <- runif(n)

lam_poisson <- 3

p_de_geometrica <- 0.5 # 0<p<1

mistura <- sapply(uniformes, gerar_mistura, alpha = alpha)
hist(mistura)
