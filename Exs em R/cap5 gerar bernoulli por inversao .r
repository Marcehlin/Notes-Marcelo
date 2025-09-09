# p é o  parâmetro da bernoulli
# n é o numero de simulacoes
p <- 0.3
n <- 100000

u <- runif(n)
x <- as.integer(u <= p) #X = 1 se u <= p, X = 0 caso contrario
help(as.integer)
help(prop.table)
help(table)
help(factor)

#a distribuição empírica, forçando "níveis" em 0 e 1
emp <- prop.table(table(factor(x, levels = c(0, 1))))
#a real, P(X = 0) = 1 - p, P(X = 1) = p
real <- c(`0` = 1 - p, `1` = p)

#dataframe de comparação: entre a empírica e real
comp <- data.frame(
  valor = c(0, 1),
  real = as.numeric(real),
  estimada = as.numeric(emp)
)
print(comp)

library(tidyr)
library(ggplot2)
help(pivot_longer)
# gráfico comparando real vs estimada
comp_long <- pivot_longer(comp, cols = c(real, estimada),
                          names_to = "tipo", values_to = "prob")

ggplot(comp_long, aes(x = factor(valor), y = prob, fill = tipo)) +
  geom_col(position = "dodge", color = "black") +
  scale_x_discrete(name = "Valor de X") +
  labs(title = sprintf("Bernoulli(p) com p = %.2f — n = %d", p, n),
       y = "Probabilidade") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), legend.title = element_blank())
