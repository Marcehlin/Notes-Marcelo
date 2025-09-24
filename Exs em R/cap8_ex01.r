#pseudo_algoritmo:
#
#fazer uma função que simula um valor de X, usando a distribuição exponencial

# Função densidade alvo f(x), nesse caso é a f.d.p de gamma
library(ggplot2)
f <- function(x) {
  (1 / gamma(1.5)) * x ^ (0.5) * exp(-x)
}

# Função densidade proposta g(x) (distribuição exponencial)
g <- function(x) {
  lambda <- 3 / 2
  lambda * exp(-lambda * x)
}
razao <- function(x) {
  f(x) / g(x)
}

n_amostras <- 10000

# Constante c, o m
opt_result <- optimize(f = razao, interval = c(0, 10), maximum = TRUE)
c <- opt_result$maximum
# Amostragem por rejeição
amostragem_rejeicao <- function(f, c, n_amostras) {
  amostras <- numeric(0)
  while (length(amostras) < n_amostras) {
    # Gere Y ~ g(x) (uniforme entre 0 e 1)
    Y <- rexp(1, rate = 3 / 2)
    
    # Gere U ~ Uniforme(0, 1)
    U <- runif(1, 0, 1)
    
    # Verifica se aceitamos Y
    if (U <= f(Y) / (c * g(Y))) {
      amostras <- c(amostras, Y)
    }
  }
  return(amostras)
}
amostras <- amostragem_rejeicao(f, c, n_amostras)

x_vals <- seq(0, 10, length.out = 1000)
f_vals <- f(x_vals)
df <- data.frame(x = x_vals, y = f_vals)
# Gráfico com ggplot2
ggplot() +
  geom_line(data = df, aes(x = x, y = y), color = 'blue', size = 1, linetype = 'solid') +
  geom_histogram(aes(x = amostras, y = ..density..), bins = 30, fill = 'orange', alpha = 0.5, color = 'black') +
  ggtitle("Amostragem por Rejeição") +
  xlab("x") +
  ylab("Densidade") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_function(fun = dgamma, args = list(shape = 3 / 2, rate = 1), color = "red", size = 1)
