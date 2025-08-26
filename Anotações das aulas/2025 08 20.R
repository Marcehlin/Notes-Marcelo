#Anotações da aula 2025 08 20
#Técnica de Inversão
#Exemplo1: X ~ Ber(0,2)
B <- 1000
U <- runif(B)

X <- ifelse(U<0.2,0,1)

hist(X)

as.numeric(U<0.2)

# Função para gerar a inversa da CDF para a distribuição geométrica
inversa_cdf_geometrica <- function(p, u) {
  # Usando a fórmula inversa da CDF geométrica: F⁻¹(u) = floor(log(1 - u) / log(1 - p))
  k <- ceiling(log(1 - u) / log(1 - p))
  return(as.integer(k))
}

# Parâmetro p da distribuição geométrica
p <- 0.5

# Gerando 1000 números uniformemente distribuídos
uniformes <- runif(1000)

# Gerando a variável aleatória geométrica correspondente para cada número uniforme
geometricas <- sapply(uniformes, inversa_cdf_geometrica, p = p)

# Função para gerar a inversa da CDF para a distribuição Poisson usando a técnica de inversão e a fórmula recursiva
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

# Parâmetro lambda da distribuição Poisson
lam <- 3

# Gerando 1000 números uniformemente distribuídos
uniformes <- runif(1000)

# Gerando a variável aleatória Poisson correspondente para cada número uniforme
poisson_vars <- sapply(uniformes, inversa_cdf_poisson_recursiva, lam = lam)

# Plotando o histograma das variáveis Poisson geradas
library(ggplot2)

df <- data.frame(poisson_vars = poisson_vars)
ggplot(df, aes(x = poisson_vars)) +
  geom_histogram(bins = max(poisson_vars) + 1, color = "black", fill = "skyblue", boundary = 0, closed = "left") +
  labs(title = "Histograma de Variáveis Aleatórias Poisson Usando a Fórmula Recursiva (λ = 3)",
       x = "Valor da Variável Aleatória Poisson", 
       y = "Frequência") +
  theme_minimal() +
  theme(panel.grid.major = element_blank())

poisson_vars <- rpois(10000,lambda = lam)