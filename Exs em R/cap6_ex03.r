library(ggplot2)

#parâmetros
alpha <- 10  # n = número de somas ou primeiro parâmetro da gamma
beta <- 10  # parâmetro(lambda) da distribuição exponencial ou segundo parâmetro da gamma

u <- matrix(runif(1000 * alpha, min = 0, max = 1), ncol = alpha) # uma uniforme gerada numa matriz com n valores numa linha, ou seja, n colunas

x <- -log(1 - u) / beta #transformar a matriz u em uma exponencial usando a técnica da inversa

y <- rowSums(x) #y agora é uma nova matriz de uma coluna só, cada valor de y é a soma de uma linha de x, assim y tem uma distribuição gamma

#histograma não usando pacote ggplot
hist(y, probability = TRUE, col = "lightblue", border = "white")
curve(dgamma(x, shape = alpha, rate = beta), col = "red", lwd = 2, add = TRUE)

#usando ggplot
# Criar um dataframe para o ggplot2
data <- data.frame(Y = y)
p <- ggplot(data, aes(x = Y)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightcoral", color = "black")+
  labs(title = paste("Histograma de variáveis geradas: Y ~ Gamma (", alpha, ",", beta, ")"), x = "valro de y", y = "frequencia") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12))

print(p)

#adicionando a curva teórica da função de densidade:
p + stat_function(fun = dgamma, args = list(shape = alpha, rate = beta), color = "blue", size = 1.5)
