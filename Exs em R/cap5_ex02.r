inversa_cdf_geometrica <- function(p, u) {
  # k = falhas antes do 1º sucesso
  k <- ceiling(log1p(-u) / log1p(-p)) - 1L
  as.integer(k)
}

inversa_cdf_poisson_recursiva <- function(lam, u) {
  k <- 0
  p <- exp(-lam)  # P(X=0)
  F_acm <- p  # Iniciamos com a probabilidade P(X=0)
  
  # Continuamos somando até que F >= u
  while (u > F_acm) {
    k <- k + 1
    p <- p * lam / k  # Atualiza a probabilidade recursivamente para o próximo valor
    F_acm <- F_acm + p
  }
  
  return(k)
}

gerar_X_que_eh_va_mistura <- function(alpha,u,X1,X2){ #0 < alpha < 1
    if (u<alpha){
        X_mistura <- X1
    } else{
        X_mistura <- X2
    }
    return (X_mistura)
}
u <- runif(1)
X1_poisson <- inversa_cdf_poisson_recursiva(5,u)
X1_poisson

X2_geometrica <- inversa_cdf_geometrica(0.2,u)
X2_geometrica

gerar_mistura <- function(alpha,u){
    if (u<alpha){
        X_mistura <- inversa_cdf_poisson_recursiva(3,u)
    } else{
        X_mistura <- inversa_cdf_geometrica(0.5,u)
    }
    return (X_mistura)
}
alpha = 0.8
X_de_mistura_gerado <- gerar_X_que_eh_va_mistura(alpha,u,X1_poisson,X2_geometrica)
X_de_mistura_gerado

help(sapply)
uniformes <- runif(1000)
uniformes

lam_poisson <- 3
poissons <- sapply(uniformes,inversa_cdf_poisson_recursiva,lam =lam_poisson)
poissons
hist(poissons)

p_de_geometrica <- 0.5 # 0<p<1
geometricas <- sapply(uniformes, inversa_cdf_geometrica, p = p_de_geometrica)
geometricas
hist(geometricas)

mistura <- sapply(uniformes,gerar_mistura,alpha = alpha)
mistura
hist(mistura)
