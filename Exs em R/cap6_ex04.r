#f(x) = (1/8)*x, 0<x<4
n <- 10000
u <- runif(n)

x <- 4 * sqrt(u)

#não usando ggplot
hist(x, probability = TRUE)
curve((1 / 8) * x, from = 0, to = 4, col = "red", lwd = 2, add = TRUE)

#usando ggplot
data <- data.frame(X = x)
p <- ggplot(data, aes(x = X)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "#9cb813", color = "black") +
  labs(title = paste("Histograma de variáveis geradas: X f(x) = (1/8)*x, 0<x<4"), x = "valro de X", y = "frequencia") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12))

print(p)
p +  stat_function(fun = function(x) (1 / 8) * x, color = "blue", size = 1.2, xlim = c(0, 4))
