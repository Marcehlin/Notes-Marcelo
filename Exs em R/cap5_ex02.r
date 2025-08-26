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

gerar_x_mistura_v1 <- function(alpha, u, x1, x2) { #0 < alpha < 1
  if (u < alpha) {
    x_mistura <- x1
  } else{
    x_mistura <- x2
  }
  return (x_mistura)
}
u <- runif(1)
x1_poisson <- inversa_cdf_poisson_recursiva(5, u)
x1_poisson

x2_geometrica <- inversa_cdf_geometrica(0.2, u)
x2_geometrica

gerar_mistura <- function(alpha,u){
  if (u < alpha) {
    x_mistura <- inversa_cdf_poisson_recursiva(3, u)
  } else{
    x_mistura <- inversa_cdf_geometrica(0.5, u)
  }
  return (x_mistura)
}
alpha = 0.8
x_de_mistura_gerado <- gerar_x_mistura_v1(alpha, u, x1_poisson, x2_geometrica)
x_de_mistura_gerado

help(sapply)
uniformes <- runif(1000)
uniformes

lam_poisson <- 3
poissons <- sapply(uniformes, inversa_cdf_poisson_recursiva, lam =lam_poisson)
poissons
hist(poissons)

p_de_geometrica <- 0.5 # 0<p<1
geometricas <- sapply(uniformes, inversa_cdf_geometrica, p = p_de_geometrica)
geometricas
hist(geometricas)

mistura <- sapply(uniformes,gerar_mistura,alpha = alpha)
mistura
hist(mistura)
