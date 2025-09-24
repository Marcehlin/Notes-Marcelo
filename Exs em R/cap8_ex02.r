library(ggplot2)

# Função densidade alvo f(x)
f <- function(x) {
  1 / 2 * sin(x)
}

# Função densidade proposta g(x) (distribuição uniforme em 0 a pi)
g <- function(x) {
  rep(1 / pi, length(x))  # g(x) é uniforme no intervalo (0, 1)
}

# Constante c (máximo de f(x))
opt_result <- optimize(f = (function(x) -f(x) * pi), interval = c(0, pi))
c <- f(opt_result$minimum)
# ou simplesmente 
n_amostras <- 5000

amostragem_rejeicao <- function(f, c, n_amostras) {
  amostras <- numeric(0)
  while (length(amostras) < n_amostras) {
    # Gere Y ~ g(x) (uniforme entre 0 e pi)
    Y <- runif(1, 0, pi)
    
    # Gere U ~ Uniforme(0, 1)
    U <- runif(1, 0, 1)
    
    # Verifica se aceitamos Y
    if (U <= f(Y) / (c)) {
      amostras <- c(amostras, Y)
    }
  }
  return(amostras)
}

amostras <- amostragem_rejeicao(f, c, n_amostras)

x_vals <- seq(0, pi, length.out = 5000)
f_vals <- f(x_vals)
df <- data.frame(x = x_vals, y = f_vals)
# Gráfico com ggplot2
ggplot() +
  geom_line(data = df, aes(x = x, y = y), color = '#c800ff', size = 1, linetype = 'solid') +
  geom_histogram(aes(x = amostras, y = ..density..), bins = 30, fill = '#23bf45', alpha = 0.5, color = 'black') +
  ggtitle("Amostragem por Rejeição") +
  xlab("x") +
  ylab("Densidade") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))