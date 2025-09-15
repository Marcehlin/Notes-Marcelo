n <- 100000
u <- runif(n)

x <- u * 2 + 1
hist(x, probability = TRUE, col = "lightblue", border = "white")
curve(0 * x + 0.5, from = 1, to = 3, col = "red", lwd = 2, add = TRUE)

library(ggplot2)

data <- data.frame(X = x)
p <- ggplot(data, aes(x = X)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  labs(title = "histograma", x  = "valor de X", y = "Frequencia") +
  theme_minimal() 

print(p)
p +  stat_function(fun = function(x) 0.5 , color = "red", size = 1.2, xlim = c(1, 3))
