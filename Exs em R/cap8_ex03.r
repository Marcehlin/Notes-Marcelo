library(ggplot2)
# Função densidade alvo f(x)
f_estrela <- function(x) {
  exp(-x) * (sin(x) ^ 2)
}

# Função densidade proposta g(x) (distribuição exponencial com parametro = 1)
g <- function(x) {
  lambda <- 1
  lambda * exp(-lambda * x)
}

razao <- function(x) f_estrela(x) / g(x)

# Constante c (máximo da razão f(x)/g(x))
opt_result <- optimize(f = razao, interval = c(0, 10), maximum = TRUE)
c <- opt_result$maximum

n_amostras <- 5000

amostragem_rejeicao <- function(f, g, c, n_amostras) {
  amostras <- numeric(0)
  while (length(amostras) < n_amostras) {
    # Gere Y ~ g(x) (uniforme entre 0 e pi)
    Y <- rexp(1, rate = 1)
    
    # Gere U ~ Uniforme(0, 1)
    U <- runif(1, 0, 1)
    
    # Verifica se aceitamos Y
    if (U <= f(Y) / (c * g(Y))) {
      amostras <- c(amostras, Y)
    }
  }
  return(amostras)
}

amostras <- amostragem_rejeicao(f_estrela, g, c, n_amostras)

x_vals <- seq(0, 10, length.out = 5000)
f_estrela_vals <- f_estrela(x_vals)
df <- data.frame(x = x_vals, y = f_estrela_vals)
# Gráfico com ggplot2
ggplot() +
  geom_line(data = df, aes(x = x, y = y), color = '#dca717', size = 1, linetype = 'solid') +
  geom_histogram(aes(x = amostras, y = ..density..), bins = 30, fill = '#0d52e7', alpha = 0.5, color = 'black') +
  ggtitle("Amostragem por Rejeição") +
  xlab("x") +
  ylab("Densidade") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#o histograma de cima não está normalizada
#a integral de f_estrela dá 2 / 5, que não é um, precisa multiplicar por 5 / 2 que é 2.5
#versão normalizada:
f <- function(x) {
  2.5 * exp(-x) * (sin(x) ^ 2)
}

f_vals <- f(x_vals)
x_vals <- seq(0, 10, length.out = 5000)

f_vals <- f(x_vals)
df <- data.frame(x = x_vals, y = f_vals)
# Gráfico com ggplot2
ggplot() +
  geom_line(data = df, aes(x = x, y = y), color = '#dc0a0a', size = 1, linetype = 'solid') +
  geom_histogram(aes(x = amostras, y = ..density..), bins = 30, fill = '#1df0e2', alpha = 0.5, color = 'black') +
  ggtitle("Amostragem por Rejeição") +
  xlab("x") +
  ylab("Densidade") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))