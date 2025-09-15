n <- 10000
u <- runif(n)
lambda <- 5

gerar_exponenciais <- function(u,lambda){
  exp <- (-log(1 - u) / lambda)
  return (exp)
}
exp_emp <- gerar_exponenciais(u, lambda)

#não usando ggplot
hist(exp_emp, probability = TRUE)
curve(dexp(x, rate = lambda), col = "red", lwd = 2, add = TRUE)

#usando ggplot
data <- data.frame(X = exp_emp)
p <- ggplot(data, aes(x = X)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "#10d59d", color = "black") +
  labs(title = paste("Histograma de variáveis geradas: X ~ Exp (", lambda, ")"), x = "valro de X", y = "frequencia") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12))

print(p)
p + stat_function(fun = dexp, args = list(rate = lambda),
                  color = "blue", size = 1.2)
