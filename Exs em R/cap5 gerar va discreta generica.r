#gerando a função de distribuição acumulada
#exemplo:
suporte_de_uma_va_discreta <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)

#a soma das probabilidades tem que dar 1
probabilidades_repectivas <- c(0.1, 0.1, 0.15, 0.15, 0.1, 0.2, 0, 0.1, 0.1)

#cdf = cumulative distribution function
cdf <- cumsum(probabilidades_repectivas)

#gerar um numero aleatorio entre 0 e 1 (uniforme)
u <- runif(1)

valor_gerado <- rep(NA, length(suporte_de_uma_va_discreta))

for (i in seq_along(suporte_de_uma_va_discreta)) {
  if (u <= cdf[i]) {
    valor_gerado <- suporte_de_uma_va_discreta[i]
    break
  }
}

library(ggplot2)

df <- data.frame(valores = suporte_de_uma_va_discreta, cdf = cdf)

ggplot(df, aes(x = valores, y = cdf)) +
  geom_step(direction = "hv", color = "blue", size = 1.5) +
  geom_hline(yintercept = u, color = "red", linetype = "dashed") +
  geom_vline(xintercept = valor_gerado, color = "green", linetype = "dashed") +
  labs(title = "Técnica da Inversão para Geração de Variável Aleatória Discreta",
       x = "Valores da Variável Aleatória", 
       y = "CDF") +
  annotate("text", x = max(suporte_de_uma_va_discreta), y = u, label = sprintf("u = %.2f", u), hjust = -0.1, vjust = -1) +
  annotate("text", x = valor_gerado, y = max(cdf), label = paste("Valor gerado =", valor_gerado), hjust = -0.1, vjust = -0.5) +
  theme_minimal() +
  theme(panel.grid = element_blank())
