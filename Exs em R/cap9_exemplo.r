n <- 10000
u <- runif(n)
lambda <- 5
k <- 6

exp_emp <- (- log(1 - u) / (1 / (lambda ^ k)))

weibull_emp <- exp_emp ^ (1 / k)
X <- weibull_emp
#histograma não usando pacote ggplot
hist(weibull_emp, probability = TRUE)
curve(dweibull(x, shape = k, scale = lambda), col = "red", lwd = 2, add = TRUE)

#usando ggplot
# Criar um dataframe para o ggplot2
data <- data.frame(X = X)
p <- ggplot(data, aes(x = X)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightcoral", color = "black")+
  labs(title = paste("Histograma de variáveis geradas: X ~ Weibull (", lambda, ",", k, ")"), x = "valro de x", y = "frequencia") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12))

print(p)

#adicionando a curva teórica da função de densidade:
p + stat_function(fun = dweibull, args = list(shape = k, scale = lambda), color = "blue", size = 1.5)
