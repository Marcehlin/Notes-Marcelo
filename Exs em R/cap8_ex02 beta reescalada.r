library(ggplot2)

# densidade alvo
f <- function(x) {
  0.5 * sin(x)
}

# densidade proposta: Beta(2,2) reescalada para [0, pi]
# é melhor usar uma beta onde parâmetros são iguais, dai melhora a eficiencia
g <- function(x) {
  (1 / pi) * dbeta(x / pi, shape1 = 2, shape2 = 2)
}

# razão f/g
razao <- function(x) {
  f(x) / g(x)
}

# constante c
opt_result <- optimize(f = razao, interval = c(0, pi), maximum = TRUE)
c <- opt_result$objective

# amostragem via rejeição
amostragem_rejeicao <- function(f, g, c, n_amostras) {
  amostras <- numeric(0)
  while (length(amostras) < n_amostras) {
    # Y ~ proposta Beta(2,2) reescalada
    Y <- pi * rbeta(1, 2, 2)
    U <- runif(1)
    
    if (U <= f(Y) / (c * g(Y))) {
      amostras <- c(amostras, Y)
    }
  }
  return(amostras)
}

# gerar amostras
n_amostras <- 5000
amostras <- amostragem_rejeicao(f, g, c, n_amostras)

# comparação visual
x_vals <- seq(0, pi, length.out = 1000)
df <- data.frame(x = x_vals, f = f(x_vals), g = c * g(x_vals))

ggplot() +
  geom_line(data = df, aes(x = x, y = f), color = "blue", size = 1) +
  geom_line(data = df, aes(x = x, y = g), color = "red", linetype = "dashed") +
  geom_histogram(aes(x = amostras, y = ..density..), bins = 40,
                 fill = "#23bf45", alpha = 0.5, color = "black") +
  labs(title = "Amostragem por Rejeição (f vs c*g)", x = "x", y = "densidade") +
  theme_minimal()
